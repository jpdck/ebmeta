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
fn test_write_epub_metadata() {
    let mut src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    src_path.push("tests");
    src_path.push("CameronCooper-SolarWhisper.epub");

    let mut dest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest_path.push("tests");
    dest_path.push("output_test.epub");

    // Copy file to avoid modifying the original test file
    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test epub");

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
    let _ = std::fs::remove_file(dest_path);
}
