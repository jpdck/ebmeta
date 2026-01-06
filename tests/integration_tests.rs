use std::path::PathBuf;
use ebmeta::core::MetadataIo;
use ebmeta::epub::EpubMetadataManager;

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
