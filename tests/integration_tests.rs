//! Integration tests for audio and EPUB metadata handlers.
//!
//! These tests define the behavioral contracts for the `MetadataIo` trait implementations:
//! - `AudioMetadataManager` (MP3, M4B, FLAC)
//! - `EpubMetadataManager` (EPUB files)
//!
//! Contract coverage:
//! - Input validation (`can_handle`)
//! - Successful metadata extraction (read)
//! - Round-trip write-read consistency
//! - Error handling for malformed/unsupported files

use ebmeta::audio::AudioMetadataManager;
use ebmeta::core::{Metadata, MetadataIo};
use ebmeta::epub::EpubMetadataManager;
use std::path::Path;

// ============================================================================
// AudioMetadataManager - MP3 Files (ID3v2 tags)
// ============================================================================

#[test]
fn audio_mp3_can_handle_recognizes_mp3_extension() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/MULT_MP3/Devolution-Part01.mp3");

    assert!(
        manager.can_handle(path),
        "AudioMetadataManager must handle .mp3 files"
    );
}

#[test]
fn audio_mp3_can_handle_rejects_non_audio_files() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/CameronCooper-SolarWhisper.epub");

    assert!(
        !manager.can_handle(path),
        "AudioMetadataManager must reject non-audio files"
    );
}

#[test]
fn audio_mp3_read_extracts_title_from_id3() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/MULT_MP3/Devolution-Part01.mp3");

    let metadata = manager.read(path).expect("Failed to read MP3 metadata");

    // Contract: title field must be populated if ID3 tag exists
    assert!(
        metadata.title.is_some(),
        "MP3 with ID3 tags must have title"
    );
}

#[test]
fn audio_mp3_read_extracts_authors() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/MULT_MP3/Devolution-Part01.mp3");

    let metadata = manager.read(path).expect("Failed to read MP3 metadata");

    // Contract: authors extracted from artist/album_artist
    assert!(
        !metadata.authors.is_empty(),
        "MP3 should extract authors from ID3"
    );
}

#[test]
fn audio_mp3_read_missing_file_returns_io_error() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/nonexistent.mp3");

    let result = manager.read(path);

    // Contract: missing file must return error (wrapped by ID3 library)
    assert!(result.is_err(), "Reading nonexistent file must fail");
}

#[test]
fn audio_mp3_read_corrupted_file_returns_error() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/corrupted/truncated.mp3");

    let result = manager.read(path);

    // Contract: corrupted files should not panic, must return error
    assert!(result.is_err(), "Corrupted MP3 must return error");
}

// ============================================================================
// AudioMetadataManager - M4B Files (MP4 metadata atoms)
// ============================================================================

#[test]
fn audio_m4b_can_handle_recognizes_m4b_extension() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");

    assert!(
        manager.can_handle(path),
        "AudioMetadataManager must handle .m4b files"
    );
}

#[test]
fn audio_m4b_read_extracts_title() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");

    let metadata = manager.read(path).expect("Failed to read M4B metadata");

    // Contract: title from ©nam atom
    assert!(
        metadata.title.is_some(),
        "M4B with metadata must have title"
    );
    assert_eq!(metadata.title.as_deref(), Some("The Science of Sci-Fi"));
}

#[test]
fn audio_m4b_read_extracts_authors() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");

    let metadata = manager.read(path).expect("Failed to read M4B metadata");

    // Contract: authors from ©ART atom
    assert!(!metadata.authors.is_empty(), "M4B should extract authors");
    // M4B file contains "Erin Macdonald" and "The Great Courses"
    assert!(metadata
        .authors
        .iter()
        .any(|a| a.contains("Erin Macdonald")));
}

#[test]
fn audio_m4b_read_extracts_narrators() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");

    let metadata = manager.read(path).expect("Failed to read M4B metadata");

    // Contract: narrators from ©nrt or similar fields
    assert!(
        !metadata.narrators.is_empty(),
        "M4B should extract narrators"
    );
    // M4B file contains "Erin Macdonald" as narrator
    assert!(metadata
        .narrators
        .iter()
        .any(|n| n.contains("Erin Macdonald")));
}

#[test]
fn audio_m4b_read_extracts_cover_image() {
    let manager = AudioMetadataManager::new();
    let path = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");

    let metadata = manager.read(path).expect("Failed to read M4B metadata");

    // Contract: cover image populated if embedded art exists
    assert!(
        metadata.cover_image.is_some(),
        "M4B with artwork must have cover_image"
    );
    let cover = metadata
        .cover_image
        .as_ref()
        .expect("cover_image was validated above");
    assert!(
        !cover.content.is_empty(),
        "Cover image content must not be empty"
    );
    assert!(
        !cover.media_type.is_empty(),
        "Cover image must have media type"
    );
}

// ============================================================================
// AudioMetadataManager - Write Operations
// ============================================================================

#[test]
fn audio_mp3_write_read_roundtrip_preserves_metadata() {
    let manager = AudioMetadataManager::new();
    let source = Path::new("tests/media/MULT_MP3/Devolution-Part01.mp3");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_roundtrip.mp3");

    // Copy original to temp location
    std::fs::copy(source, &test_file).expect("Failed to copy test file");

    // Read original metadata
    let mut metadata = manager.read(&test_file).expect("Failed to read original");

    // Modify fields that are supported by ID3
    metadata.title = Some("Test Title Override".to_string());
    metadata.authors = vec!["Test Author".to_string()];
    metadata.narrators = vec!["Test Narrator".to_string()];
    metadata.genre = Some("Test Genre".to_string());

    // Write modified metadata
    manager
        .write(&test_file, &metadata)
        .expect("Failed to write metadata");

    // Read back and verify
    let read_back = manager
        .read(&test_file)
        .expect("Failed to read after write");

    // Contract: ID3-supported fields must persist through write/read cycle
    assert_eq!(read_back.title.as_deref(), Some("Test Title Override"));
    assert_eq!(read_back.authors, vec!["Test Author".to_string()]);
    assert_eq!(read_back.narrators, vec!["Test Narrator".to_string()]);
    assert_eq!(read_back.genre.as_deref(), Some("Test Genre"));
}

#[test]
fn audio_m4b_write_read_roundtrip_preserves_metadata() {
    let manager = AudioMetadataManager::new();
    let source = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_roundtrip.m4b");

    std::fs::copy(source, &test_file).expect("Failed to copy test file");

    let mut metadata = manager.read(&test_file).expect("Failed to read original");

    // Modify core fields
    metadata.title = Some("Modified M4B Title".to_string());
    metadata.authors = vec!["Modified Author".to_string()];

    manager
        .write(&test_file, &metadata)
        .expect("Failed to write metadata");

    let read_back = manager
        .read(&test_file)
        .expect("Failed to read after write");

    assert_eq!(read_back.title.as_deref(), Some("Modified M4B Title"));
    assert_eq!(read_back.authors, vec!["Modified Author".to_string()]);
}

#[test]
fn audio_write_to_unsupported_format_returns_error() {
    let manager = AudioMetadataManager::new();
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let unsupported = temp_dir.path().join("test.wav");

    // Create empty file with unsupported extension
    std::fs::write(&unsupported, b"dummy").expect("Failed to create dummy file");

    let metadata = Metadata::default();
    let result = manager.write(&unsupported, &metadata);

    // Contract: unsupported format must return error
    assert!(result.is_err(), "Writing to unsupported format must fail");
}

// ============================================================================
// EpubMetadataManager - EPUB3.3 Compliance
// ============================================================================

#[test]
fn epub_can_handle_recognizes_epub_extension() {
    let manager = EpubMetadataManager;
    let path = Path::new("tests/media/CameronCooper-SolarWhisper.epub");

    assert!(
        manager.can_handle(path),
        "EpubMetadataManager must handle .epub files"
    );
}

#[test]
fn epub_can_handle_rejects_non_epub_files() {
    let manager = EpubMetadataManager;
    let path = Path::new("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");

    assert!(
        !manager.can_handle(path),
        "EpubMetadataManager must reject non-EPUB files"
    );
}

#[test]
fn epub_read_extracts_title_from_opf() {
    let manager = EpubMetadataManager;
    let path = Path::new("tests/media/CameronCooper-SolarWhisper.epub");

    let metadata = manager.read(path).expect("Failed to read EPUB metadata");

    // Contract: dc:title must be extracted
    assert!(
        metadata.title.is_some(),
        "EPUB must have title from dc:title"
    );
    assert_eq!(metadata.title.as_deref(), Some("Solar Whisper"));
}

#[test]
fn epub_read_extracts_authors_from_creators() {
    let manager = EpubMetadataManager;
    let path = Path::new("tests/media/CameronCooper-SolarWhisper.epub");

    let metadata = manager.read(path).expect("Failed to read EPUB metadata");

    // Contract: dc:creator elements map to authors (unless narrator role)
    assert!(
        !metadata.authors.is_empty(),
        "EPUB must extract authors from dc:creator"
    );
    assert!(metadata.authors.contains(&"Cameron Cooper".to_string()));
}

#[test]
fn epub_read_extracts_language() {
    let manager = EpubMetadataManager;
    let path = Path::new("tests/media/CameronCooper-SolarWhisper.epub");

    let metadata = manager.read(path).expect("Failed to read EPUB metadata");

    // Contract: dc:language is required in EPUB3.3
    assert!(metadata.language.is_some(), "EPUB must have language");
    assert_eq!(metadata.language.as_deref(), Some("en"));
}

#[test]
fn epub_read_missing_file_returns_io_error() {
    let manager = EpubMetadataManager;
    let path = Path::new("tests/media/nonexistent.epub");

    let result = manager.read(path);

    // Contract: missing file returns Io error (EPUB manager preserves IO errors)
    assert!(result.is_err(), "Reading nonexistent EPUB must fail");
    assert!(matches!(
        result.expect_err("validated as error above"),
        ebmeta::core::Error::Io(_)
    ));
}

#[test]
fn epub_read_invalid_zip_returns_error() {
    let manager = EpubMetadataManager;
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let invalid_epub = temp_dir.path().join("invalid.epub");

    // Create a file with .epub extension but invalid ZIP content
    std::fs::write(&invalid_epub, b"not a valid zip file").expect("Failed to create invalid file");

    let result = manager.read(&invalid_epub);

    // Contract: invalid ZIP structure must return error
    assert!(result.is_err(), "Invalid EPUB structure must return error");
}

// ============================================================================
// EpubMetadataManager - Write Operations
// ============================================================================

#[test]
fn epub_write_read_roundtrip_preserves_metadata() {
    let manager = EpubMetadataManager;
    let source = Path::new("tests/media/CameronCooper-SolarWhisper.epub");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_roundtrip.epub");

    std::fs::copy(source, &test_file).expect("Failed to copy test file");

    let mut metadata = manager.read(&test_file).expect("Failed to read original");

    // Modify metadata
    metadata.title = Some("Modified EPUB Title".to_string());
    metadata.authors = vec!["Modified Author".to_string()];
    metadata.publisher = Some("Test Publisher".to_string());
    metadata.description = Some("Test description for roundtrip validation".to_string());

    manager
        .write(&test_file, &metadata)
        .expect("Failed to write metadata");

    let read_back = manager
        .read(&test_file)
        .expect("Failed to read after write");

    // Contract: all modified fields must persist
    assert_eq!(read_back.title.as_deref(), Some("Modified EPUB Title"));
    assert_eq!(read_back.authors, vec!["Modified Author".to_string()]);
    assert_eq!(read_back.publisher.as_deref(), Some("Test Publisher"));
    assert_eq!(
        read_back.description.as_deref(),
        Some("Test description for roundtrip validation")
    );
}

#[test]
fn epub_write_preserves_unmodified_fields() {
    let manager = EpubMetadataManager;
    let source = Path::new("tests/media/CameronCooper-SolarWhisper.epub");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_preserve.epub");

    std::fs::copy(source, &test_file).expect("Failed to copy test file");

    let original = manager.read(&test_file).expect("Failed to read original");
    let mut modified = original.clone();

    // Only modify title
    modified.title = Some("Only Title Changed".to_string());

    manager
        .write(&test_file, &modified)
        .expect("Failed to write metadata");
    let read_back = manager
        .read(&test_file)
        .expect("Failed to read after write");

    // Contract: unmodified fields must remain unchanged
    assert_eq!(read_back.title.as_deref(), Some("Only Title Changed"));
    assert_eq!(
        read_back.language, original.language,
        "Language must be preserved"
    );
    assert_eq!(
        read_back.authors, original.authors,
        "Authors must be preserved"
    );
}

#[test]
fn epub_write_to_readonly_location_returns_error() {
    use std::fs::{self, File};
    use std::io::Write;

    let manager = EpubMetadataManager;

    // Create a temporary file and make it readonly
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let readonly_path = temp_dir.path().join("readonly_test.epub");

    // Create a minimal valid EPUB first
    {
        let mut file = File::create(&readonly_path).expect("Failed to create temp file");
        // Write minimal zip structure
        file.write_all(b"PK\x03\x04").expect("Failed to write");
    }

    // Make the file readonly
    let mut perms = fs::metadata(&readonly_path)
        .expect("Failed to get metadata")
        .permissions();
    perms.set_readonly(true);
    fs::set_permissions(&readonly_path, perms).expect("Failed to set readonly");

    let metadata = Metadata::default();
    let result = manager.write(&readonly_path, &metadata);

    // Contract: write to readonly location must return error
    assert!(result.is_err(), "Writing to readonly file must fail");
}

// ============================================================================
// Multi-file MP3 Audiobook Contract Tests
// ============================================================================

#[test]
fn audio_mp3_multifile_each_part_readable() {
    let manager = AudioMetadataManager::new();
    let parts = [
        "tests/media/MULT_MP3/Devolution-Part01.mp3",
        "tests/media/MULT_MP3/Devolution-Part02.mp3",
        "tests/media/MULT_MP3/Devolution-Part03.mp3",
    ];

    for part_path in &parts {
        let path = Path::new(part_path);
        let metadata = manager
            .read(path)
            .unwrap_or_else(|_| panic!("Failed to read {part_path}"));

        // Contract: each part must have title
        assert!(
            metadata.title.is_some(),
            "Each MP3 part must have title: {part_path}"
        );
    }
}

#[test]
fn audio_mp3_multifile_shared_album_metadata() {
    let manager = AudioMetadataManager::new();
    let part1 = Path::new("tests/media/MULT_MP3/Devolution-Part01.mp3");
    let part2 = Path::new("tests/media/MULT_MP3/Devolution-Part02.mp3");

    let meta1 = manager.read(part1).expect("Failed to read part 1");
    let meta2 = manager.read(part2).expect("Failed to read part 2");

    // Contract: multi-part audiobooks should share album/series metadata
    // (actual behavior depends on ID3 tags in test files)
    assert_eq!(
        meta1.authors, meta2.authors,
        "Authors should be consistent across parts"
    );
}
