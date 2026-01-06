use std::path::Path;
use std::fs::File;
use std::io::Read;
use crate::core::{Metadata, MetadataIo, Result, Error};
use serde::Deserialize;

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

pub struct EpubMetadataManager;

impl MetadataIo for EpubMetadataManager {
    fn can_handle(&self, path: &Path) -> bool {
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            matches!(ext.to_lowercase().as_str(), "epub")
        } else {
            false
        }
    }

    fn read(&self, path: &Path) -> Result<Metadata> {
        let file = File::open(path)?;
        let mut archive = zip::ZipArchive::new(file).map_err(|e| Error::Other(e.to_string()))?;

        // 1. Read META-INF/container.xml
        let mut container_file = archive.by_name("META-INF/container.xml")
            .map_err(|_| Error::Other("META-INF/container.xml not found".to_string()))?;
        let mut container_xml = String::new();
        container_file.read_to_string(&mut container_xml)?;
        drop(container_file);

        let container: Container = quick_xml::de::from_str(&container_xml)
            .map_err(|e| Error::Other(format!("Failed to parse container.xml: {}", e)))?;

        // Find rootfile with media-type="application/oebps-package+xml"
        let rootfile = container.rootfiles.rootfile.iter()
            .find(|rf| rf.media_type == "application/oebps-package+xml")
            .ok_or(Error::Other("No rootfile found with media-type application/oebps-package+xml".to_string()))?;
        
        let opf_path = rootfile.full_path.clone();

        // 2. Read OPF file
        let mut opf_file = archive.by_name(&opf_path)
            .map_err(|_| Error::Other(format!("OPF file {} not found in archive", opf_path)))?;
        let mut opf_content = String::new();
        opf_file.read_to_string(&mut opf_content)?;
        drop(opf_file);

        // 3. Parse and Validate OPF
        let pkg = spec33::parse_opf(&opf_content)
            .map_err(|e| Error::Other(e))?;
        
        spec33::validate_package_document(&pkg)
            .map_err(|e| Error::Format(e))?;

        // 4. Convert to generic Metadata
        let mut metadata = Metadata::default();
        if let Some(title) = pkg.metadata.titles.first() {
            metadata.title = Some(title.clone());
        }
        if let Some(lang) = pkg.metadata.languages.first() {
            metadata.language = Some(lang.clone());
        }
        metadata.authors = pkg.metadata.creators.clone();
        if let Some(modified) = pkg.metadata.modified.first() {
            metadata.published_date = Some(modified.clone());
        }

        Ok(metadata)
    }

    fn write(&self, _path: &Path, _metadata: &Metadata) -> Result<()> {
        // Todo: Implement epub metadata writing
        Err(Error::Other("Not implemented".to_string()))
    }
}
