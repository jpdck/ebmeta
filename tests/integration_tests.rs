use ebmeta::core::MetadataIo;
use ebmeta::epub::EpubMetadataManager;
use std::path::PathBuf;

#[test]
fn test_read_real_epub() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("CameronCooper-SolarWhisper.epub");

    let manager = EpubMetadataManager;
    assert!(manager.can_handle(&path));

    let metadata = manager.read(&path).expect("Failed to read metadata");

    // Check expected values based on the file name/content
    // Cameron Cooper - Solar Whisper
    assert!(metadata.title.is_some());
    // Note: We can add more specific assertions if we knew the content, but for now we ensure it parses successfully.
    if let Some(title) = &metadata.title {
        assert!(!title.is_empty());
    }
}

#[test]
#[allow(clippy::permissions_set_readonly_false)]
#[allow(clippy::disallowed_macros)]
fn test_write_epub_metadata() {
    let mut src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    src_path.push("tests");
    src_path.push("CameronCooper-SolarWhisper.epub");

    let mut dest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest_path.push("tests");
    dest_path.push("output_test.epub");

    // Copy file to avoid modifying the original test file
    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test epub");

    // Ensure writable
    let mut perms = std::fs::metadata(&dest_path)
        .expect("Failed to get metadata")
        .permissions();
    perms.set_readonly(false);
    std::fs::set_permissions(&dest_path, perms).expect("Failed to set permissions");

    let manager = EpubMetadataManager;

    // 1. Read
    let mut metadata = manager.read(&dest_path).expect("Failed to read metadata");

    // 2. Modify
    let new_title = "Modified Title".to_string();
    let new_author = "New Author".to_string();
    metadata.title = Some(new_title.clone());
    metadata.authors.push(new_author.clone());

    // 3. Write
    manager
        .write(&dest_path, &metadata)
        .expect("Failed to write metadata");

    // 4. Verify
    let new_metadata = manager
        .read(&dest_path)
        .expect("Failed to read new metadata");
    assert_eq!(new_metadata.title, Some(new_title));
    assert!(new_metadata.authors.contains(&new_author));

    // Cleanup
    if let Err(e) = std::fs::remove_file(&dest_path) {
        eprintln!("Warning: failed to remove test file {dest_path:?}: {e}");
    }
}

#[test]
#[allow(clippy::permissions_set_readonly_false)]
#[allow(clippy::disallowed_macros)]
fn test_write_cover_image() {
    use ebmeta::core::CoverImage;
    use std::io::Read;

    let mut src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    src_path.push("tests");
    src_path.push("CameronCooper-SolarWhisper.epub");

    let mut dest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest_path.push("tests");
    dest_path.push("output_cover_test.epub");

    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test epub");

    // Ensure writable
    let mut perms = std::fs::metadata(&dest_path)
        .expect("Failed to get metadata")
        .permissions();
    perms.set_readonly(false);
    std::fs::set_permissions(&dest_path, perms).expect("Failed to set permissions");

    let manager = EpubMetadataManager;

    // Create dummy cover image
    let dummy_content = vec![0x1, 0x2, 0x3, 0x4]; // Not real JPEG but sufficient for test
    let cover = CoverImage {
        content: dummy_content.clone(),
        media_type: "image/jpeg".to_string(),
    };

    let mut metadata = manager.read(&dest_path).expect("Read failed");
    metadata.cover_image = Some(cover);

    manager.write(&dest_path, &metadata).expect("Write failed");

    // Verify cover image via manual OPF lookup (since read() doesn't load cover content yet)
    let file = std::fs::File::open(&dest_path).expect("Failed to open zip");
    let mut archive = zip::ZipArchive::new(file).expect("Failed to open archive");

    // Read container.xml to find OPF
    let mut container = archive
        .by_name("META-INF/container.xml")
        .expect("Missing container.xml");
    let mut container_xml = String::new();
    container.read_to_string(&mut container_xml).unwrap();
    drop(container);

    // Naive parse of container.xml to find full-path
    let opf_path = container_xml
        .split("full-path=\"")
        .nth(1)
        .expect("No full-path in container.xml")
        .split('"')
        .next()
        .expect("Invalid full-path");

    // Read OPF
    let mut opf = archive.by_name(opf_path).expect("Missing OPF file");
    let mut opf_xml = String::new();
    opf.read_to_string(&mut opf_xml).unwrap();
    drop(opf);

    // Find manifest item with properties="cover-image"
    let mut cover_href = None;
    // Split by <item to get individual items (handles minified XML better)
    for part in opf_xml.split("<item") {
        let has_prop = part.contains("properties=\"cover-image\"")
            || part.contains("properties=\"nav cover-image\"");

        if has_prop {
            cover_href = part
                .split("href=\"")
                .nth(1)
                .and_then(|h| h.split('"').next())
                .map(ToString::to_string);

            if cover_href.is_some() {
                break;
            }
        }
    }
    let cover_href = cover_href.expect("No cover-image item found in OPF");

    // Read the actual cover file
    let cover_path = if let Some(parent) = std::path::Path::new(opf_path).parent() {
        parent.join(&cover_href).to_string_lossy().into_owned()
    } else {
        cover_href
    };

    let mut cover_file = archive
        .by_name(&cover_path)
        .expect("Cover file missing from ZIP");
    let mut content = Vec::new();
    cover_file.read_to_end(&mut content).unwrap();

    assert_eq!(content, dummy_content, "Cover content mismatch");

    if let Err(e) = std::fs::remove_file(&dest_path) {
        eprintln!("Warning: failed to remove test file {dest_path:?}: {e}");
    }
}
