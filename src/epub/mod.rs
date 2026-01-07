use crate::core::{CoverImageRef, Error, Metadata, MetadataIo, Result};
use serde::Deserialize;
use std::collections::HashSet;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

pub mod spec33;

#[derive(Deserialize)]
struct Container {
    rootfiles: RootFiles,
}

#[derive(Deserialize)]
struct RootFiles {
    #[serde(rename = "rootfile")]
    rootfile: Vec<RootFile>,
}

#[derive(Deserialize)]
struct RootFile {
    #[serde(rename = "@full-path")]
    full_path: String,
    #[serde(rename = "@media-type")]
    media_type: String,
}

/// Handles reading and writing metadata for EPUB files.
///
/// This struct implements the `MetadataIo` trait, allowing it to interact with
/// standard EPUB 3.3 files. It handles the details of the Open Container Format (OCF)
/// to locate the Package Document (OPF), and then parses/modifies the OPF file.
pub struct EpubMetadataManager;

impl MetadataIo for EpubMetadataManager {
    /// Checks if the file path has an .epub extension.
    fn can_handle(&self, path: &Path) -> bool {
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            matches!(ext.to_lowercase().as_str(), "epub")
        } else {
            false
        }
    }

    /// Reads metadata from an EPUB file.
    ///
    /// This method:
    /// 1. Opens the EPUB (ZIP) archive.
    /// 2. Reads `META-INF/container.xml` to find the rootfile (OPF).
    /// 3. Parses the OPF file using EPUB 3.3 strict validation.
    /// 4. Maps the standard EPUB metadata (DC terms) to the generic `Metadata` struct.
    ///
    /// # Errors
    ///
    /// Returns an `Error` if:
    /// * The file cannot be opened or is not a valid ZIP archive.
    /// * `META-INF/container.xml` is missing or invalid.
    /// * The rootfile (OPF) cannot be located or read.
    /// * The OPF file content is invalid or fails EPUB 3.3 validation.
    fn read(&self, path: &Path) -> Result<Metadata> {
        let file = File::open(path)?;
        let mut archive = zip::ZipArchive::new(file).map_err(|e| Error::Other(e.to_string()))?;

        // 1. Read META-INF/container.xml
        let mut container_file = archive
            .by_name("META-INF/container.xml")
            .map_err(|_| Error::Other("META-INF/container.xml not found".to_string()))?;
        let mut container_xml = String::new();
        container_file.read_to_string(&mut container_xml)?;
        drop(container_file);

        let container: Container = quick_xml::de::from_str(&container_xml)
            .map_err(|e| Error::Other(format!("Failed to parse container.xml: {e}")))?;

        // Find rootfile with media-type="application/oebps-package+xml"
        let rootfile = container
            .rootfiles
            .rootfile
            .iter()
            .find(|rf| rf.media_type == "application/oebps-package+xml")
            .ok_or(Error::Other(
                "No rootfile found with media-type application/oebps-package+xml".to_string(),
            ))?;

        let opf_path = rootfile.full_path.clone();

        // 2. Read OPF file
        let mut opf_file = archive
            .by_name(&opf_path)
            .map_err(|_| Error::Other(format!("OPF file {opf_path} not found in archive")))?;
        let mut opf_content = String::new();
        opf_file.read_to_string(&mut opf_content)?;
        drop(opf_file);

        // 3. Parse and Validate OPF
        let pkg = spec33::parse_opf(&opf_content).map_err(Error::Other)?;

        spec33::validate_package_document(&pkg).map_err(Error::Format)?;

        // 4. Convert to generic Metadata
        let mut metadata = Metadata::default();
        if let Some(title) = pkg.metadata.titles.first() {
            metadata.title = Some(title.clone());
        }
        if let Some(lang) = pkg.metadata.languages.first() {
            metadata.language = Some(lang.clone());
        }
        metadata.authors.clone_from(&pkg.metadata.creators);
        if let Some(desc) = pkg.metadata.descriptions.first() {
            metadata.description = Some(desc.clone());
        }
        if let Some(publ) = pkg.metadata.publishers.first() {
            metadata.publisher = Some(publ.clone());
        }
        if let Some(date) = pkg.metadata.dates.first() {
            metadata.published_date = Some(date.clone());
        }
        metadata.tags.clone_from(&pkg.metadata.subjects);

        // Extract cover image reference (manifest item with properties="cover-image")
        if let Some(cover_item) = pkg.manifest.items.iter().find(|item| {
            item.properties
                .as_deref()
                .is_some_and(|p| p.split_whitespace().any(|prop| prop == "cover-image"))
        }) {
            let cover_path = if let Some(parent) = Path::new(&opf_path).parent() {
                parent
                    .join(&cover_item.href)
                    .to_string_lossy()
                    .replace('\\', "/")
            } else {
                cover_item.href.clone()
            };

            metadata.cover_image_ref = Some(CoverImageRef {
                href: cover_path,
                media_type: cover_item.media_type.clone(),
            });
        }

        // Extract ISBN
        if let Some(isbn) = pkg
            .metadata
            .identifiers
            .iter()
            .find_map(|id| parse_isbn_identifier(&id.value))
        {
            metadata.isbn = Some(isbn);
        }

        // Fallback for published_date to modified if date is missing?
        // Or keep them separate. Metadata.published_date matches dc:date best.
        // modified is usually internal.

        Ok(metadata)
    }

    /// Writes metadata to an EPUB file.
    ///
    /// This process is atomic:
    /// 1. A temporary file (`.epub.tmp`) is created.
    /// 2. All content from the original EPUB is copied to the temporary file,
    ///    except for the OPF file which is regenerated with the new metadata.
    /// 3. The file compression settings are preserved where possible (defaulting to Deflate).
    /// 4. On success, the temporary file replaces the original.
    ///
    /// # Errors
    ///
    /// Returns an `Error` if:
    /// * The input file cannot be opened or parsed.
    /// * The temporary file cannot be created or written to.
    /// * `META-INF/container.xml` or the OPF file is missing.
    /// * Serialization of the updated OPF fails.
    /// * Renaming the temporary file to the original filename fails.
    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()> {
        let temp_path = path.with_file_name(format!(
            "{}.tmp",
            path.file_name()
                .unwrap_or(path.as_os_str())
                .to_string_lossy()
        ));

        // Perform write + rename with best-effort cleanup on any failure.
        let result = (|| -> Result<()> {
            // Scope to ensure files are closed before rename
            Self::perform_write(path, &temp_path, metadata)?;

            // 5. Replace original.
            std::fs::rename(&temp_path, path)?;

            Ok(())
        })();

        if result.is_err() {
            // Best-effort cleanup; ignore any error from removing the temp file.
            let _ = std::fs::remove_file(&temp_path);
        }

        result
    }
}

impl EpubMetadataManager {
    /// Helper to perform the read-modify-write cycle into a temporary file.
    ///
    /// This separates the file operations from the final atomic rename.
    ///
    /// # Errors
    ///
    /// Returns an `Error` if any step of reading the original EPUB, updating the metadata,
    /// or writing to the temporary file fails.
    fn perform_write(path: &Path, temp_path: &Path, metadata: &Metadata) -> Result<()> {
        let file = File::open(path)?;
        let mut archive = zip::ZipArchive::new(file).map_err(|e| Error::Other(e.to_string()))?;

        // 1. Read META-INF/container.xml
        let mut container_file = archive
            .by_name("META-INF/container.xml")
            .map_err(|_| Error::Other("META-INF/container.xml not found".to_string()))?;
        let mut container_xml = String::new();
        container_file.read_to_string(&mut container_xml)?;
        drop(container_file);

        let container: Container = quick_xml::de::from_str(&container_xml)
            .map_err(|e| Error::Other(format!("Failed to parse container.xml: {e}")))?;

        let rootfile = container
            .rootfiles
            .rootfile
            .iter()
            .find(|rf| rf.media_type == "application/oebps-package+xml")
            .ok_or(Error::Other(
                "No rootfile found with media-type application/oebps-package+xml".to_string(),
            ))?;

        let opf_path = rootfile.full_path.clone();

        // 2. Read and Parse OPF
        let mut opf_file = archive
            .by_name(&opf_path)
            .map_err(|_| Error::Other(format!("OPF file {opf_path} not found in archive")))?;
        let mut opf_content = String::new();
        opf_file.read_to_string(&mut opf_content)?;
        drop(opf_file);

        let mut pkg = spec33::parse_opf(&opf_content).map_err(Error::Other)?;

        // 3. Update OPF (may fail on invalid metadata such as bad ISBN)
        pkg.update_from_metadata(metadata).map_err(Error::Format)?;

        let mut cover_update_info = None;
        if let Some(cover) = &metadata.cover_image {
            let update = pkg.set_cover_image(&cover.media_type);
            cover_update_info = Some((update, &cover.content));
        }

        // Validate before writing
        spec33::validate_package_document(&pkg).map_err(Error::Format)?;

        let new_opf_content = quick_xml::se::to_string(&pkg)
            .map_err(|e| Error::Other(format!("Failed to serialize OPF: {e}")))?;
        let new_opf_content =
            format!("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n{new_opf_content}");
        let new_opf_content =
            spec33::insert_unknown_metadata(&new_opf_content, &pkg.metadata.unknown_children)
                .map_err(Error::Other)?;

        // 4. Create new ZIP and copy/replace
        let temp_file = File::create(temp_path)?;
        let mut writer = zip::ZipWriter::new(temp_file);

        // Calculate paths for cover image
        let opf_parent = Path::new(&opf_path).parent();
        let mut skip_paths = HashSet::new();
        let mut new_cover_path = None;

        if let Some((update, _)) = &cover_update_info {
            // Resolve paths relative to OPF
            let resolve = |href: &str| -> String {
                opf_parent.map_or_else(
                    || href.to_string(),
                    |parent| parent.join(href).to_string_lossy().replace('\\', "/"),
                )
            };

            let target_path = resolve(&update.href);
            new_cover_path = Some(target_path.clone());

            // We should skip the target path if it exists (overwrite)
            skip_paths.insert(target_path);

            // We should also skip the original path if it's different (delete/replace)
            if let Some(orig) = &update.original_href {
                let orig_path = resolve(orig);
                skip_paths.insert(orig_path);
            }
        }

        // EPUB 3.3 requires mimetype file to be first and uncompressed
        match archive.by_name("mimetype") {
            Ok(mut mimetype_file) => {
                let options = zip::write::SimpleFileOptions::default()
                    .compression_method(zip::CompressionMethod::Stored)
                    .unix_permissions(0o644);
                writer
                    .start_file("mimetype", options)
                    .map_err(|e| Error::Other(e.to_string()))?;

                let mut content = Vec::new();
                mimetype_file
                    .read_to_end(&mut content)
                    .map_err(|e| Error::Other(e.to_string()))?;
                writer.write_all(&content)?;

                skip_paths.insert("mimetype".to_string());
            }
            Err(err) => {
                return Err(Error::Other(format!(
                    "EPUB 3.3 validation failed: required 'mimetype' file is missing or unreadable: {err}"
                )));
            }
        }

        for i in 0..archive.len() {
            let file = archive
                .by_index(i)
                .map_err(|e| Error::Other(e.to_string()))?;
            let name = file.name().to_string();

            if name == opf_path {
                // Write new OPF
                let options = zip::write::SimpleFileOptions::default()
                    .compression_method(zip::CompressionMethod::Deflated)
                    .unix_permissions(0o644);
                writer
                    .start_file(&name, options)
                    .map_err(|e| Error::Other(e.to_string()))?;
                writer.write_all(new_opf_content.as_bytes())?;
            } else if !skip_paths.contains(&name) {
                // Copy other files
                writer
                    .raw_copy_file(file)
                    .map_err(|e| Error::Other(format!("Failed to copy file {name}: {e}")))?;
            }
        }

        // Write new cover image
        if let (Some(path), Some((_, content))) = (new_cover_path, cover_update_info) {
            let options = zip::write::SimpleFileOptions::default()
                .compression_method(zip::CompressionMethod::Deflated)
                .unix_permissions(0o644);
            writer
                .start_file(&path, options)
                .map_err(|e| Error::Other(e.to_string()))?;
            writer.write_all(content)?;
        }

        writer.finish().map_err(|e| Error::Other(e.to_string()))?;
        Ok(())
    }
}

fn parse_isbn_identifier(value: &str) -> Option<String> {
    let trimmed = value.trim();
    let lower = trimmed.to_ascii_lowercase();

    if !lower.starts_with("urn:isbn:") {
        return None;
    }

    let idx = lower.rfind(':')?;
    let isbn_start = idx.saturating_add(1);
    if isbn_start >= trimmed.len() {
        return None;
    }

    Some(trimmed[isbn_start..].to_string())
}

#[cfg(test)]
mod tests {
    use super::parse_isbn_identifier;

    #[test]
    fn parse_isbn_handles_mixed_case_and_whitespace() {
        let raw = " URN:IsBn:9781234567890 ";
        let parsed = parse_isbn_identifier(raw);
        assert_eq!(parsed.as_deref(), Some("9781234567890"));
    }

    #[test]
    fn parse_isbn_rejects_non_isbn() {
        assert!(parse_isbn_identifier("urn:uuid:abc").is_none());
    }
}
