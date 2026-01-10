use ebmeta::audio::AudioMetadataManager;
use ebmeta::core::{CoverImage, MetadataIo};
use ebmeta::epub::EpubMetadataManager;
use serde::Deserialize;
use std::path::PathBuf;

#[test]
fn test_read_real_epub() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/media");
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
    src_path.push("tests/media");
    src_path.push("CameronCooper-SolarWhisper.epub");

    let mut dest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest_path.push("tests/media");
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
    let mut src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    src_path.push("tests/media");
    src_path.push("CameronCooper-SolarWhisper.epub");

    let mut dest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dest_path.push("tests/media");
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

    // Read back via metadata reader (reference only; no bytes)
    let new_metadata = manager.read(&dest_path).expect("Failed to read metadata");
    let cover = new_metadata
        .cover_image_ref
        .expect("Cover image ref missing from metadata");
    assert_eq!(cover.media_type, "image/jpeg");

    if let Err(e) = std::fs::remove_file(&dest_path) {
        eprintln!("Warning: failed to remove test file {dest_path:?}: {e}");
    }
}

#[derive(Debug, Deserialize)]
struct FixtureChapter {
    start: f64,
    end: f64,
    title: String,
    #[allow(dead_code)]
    id: u32,
}

#[derive(Debug, Deserialize)]
struct FixtureMetadata {
    tags: Vec<String>,
    chapters: Vec<FixtureChapter>,
    title: String,
    authors: Vec<String>,
    narrators: Vec<String>,
    publisher: Option<String>,
    language: Option<String>,
}

fn fixture_path(rel: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(rel);
    path
}

fn read_fixture(path: &PathBuf) -> FixtureMetadata {
    let bytes = std::fs::read(path).expect("Failed to read fixture JSON");
    serde_json::from_slice(&bytes).expect("Failed to parse fixture JSON")
}

#[allow(clippy::permissions_set_readonly_false)]
fn set_writable(path: &PathBuf) {
    let mut perms = std::fs::metadata(path)
        .expect("Failed to get metadata")
        .permissions();
    perms.set_readonly(false);
    std::fs::set_permissions(path, perms).expect("Failed to set permissions");
}

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn seconds_to_ms(seconds: f64) -> u64 {
    (seconds * 1000.0).round() as u64
}

#[test]
fn test_read_real_m4b_matches_fixture_subset() {
    let audio_path = fixture_path("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");
    let json_path = fixture_path("tests/media/SINGLE_M4B/metadata.json");
    let fixture = read_fixture(&json_path);

    let manager = AudioMetadataManager::new();
    assert!(manager.can_handle(&audio_path));

    let metadata = manager
        .read(&audio_path)
        .expect("Failed to read m4b metadata");

    assert_eq!(metadata.title.as_deref(), Some(fixture.title.as_str()));
    for a in &fixture.authors {
        assert!(metadata.authors.contains(a));
    }
    for n in &fixture.narrators {
        assert!(metadata.narrators.contains(n));
    }

    // Publisher/language/tags are supported by the public metadata model but may not be present
    // in every source file, so we only assert them when the reader provides values.
    if let Some(publisher) = &metadata.publisher {
        assert!(fixture.publisher.as_ref().is_some_and(|p| p == publisher));
    }
    if let Some(language) = &metadata.language {
        assert!(fixture.language.as_ref().is_some_and(|l| l == language));
    }
    if !metadata.tags.is_empty() {
        for tag in &metadata.tags {
            assert!(fixture.tags.contains(tag));
        }
    }

    // Chapters are optional depending on container support.
    if !metadata.chapters.is_empty() {
        assert_eq!(metadata.chapters.len(), fixture.chapters.len());
        if let (Some(ch0), Some(f0)) = (metadata.chapters.first(), fixture.chapters.first()) {
            assert_eq!(ch0.title, f0.title);
            let expected_start = seconds_to_ms(f0.start);
            let expected_end = seconds_to_ms(f0.end);
            #[allow(clippy::cast_possible_wrap)]
            {
                assert!((ch0.start_time_ms as i64 - expected_start as i64).abs() <= 5);
                let end = ch0.end_time_ms.expect("Expected end_time_ms for chapter");
                assert!((end as i64 - expected_end as i64).abs() <= 5);
            }
        }
    }
}

#[test]
fn test_read_real_mp3_matches_fixture_subset() {
    let audio_path = fixture_path("tests/media/MULT_MP3/Devolution-Part01.mp3");
    let json_path = fixture_path("tests/media/MULT_MP3/metadata.json");
    let fixture = read_fixture(&json_path);

    let manager = AudioMetadataManager::new();
    assert!(manager.can_handle(&audio_path));

    let metadata = manager
        .read(&audio_path)
        .expect("Failed to read mp3 metadata");

    let title = metadata.title.expect("Expected title");
    assert!(title.contains(&fixture.title));

    for a in &fixture.authors {
        assert!(metadata.authors.contains(a));
    }

    if !metadata.narrators.is_empty() {
        for n in &metadata.narrators {
            assert!(fixture.narrators.contains(n));
        }
    }

    // Multi-file audiobooks may expose a total track count.
    #[allow(clippy::cast_possible_truncation)]
    let track_count = std::fs::read_dir(fixture_path("tests/media/MULT_MP3"))
        .expect("Failed to read MULT_MP3 dir")
        .filter_map(std::result::Result::ok)
        .filter(|e| {
            e.path()
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("mp3"))
        })
        .count() as u32;

    if let Some(total) = metadata.total_tracks {
        assert_eq!(total, track_count);
    }
}

#[test]
#[allow(clippy::permissions_set_readonly_false)]
#[allow(clippy::disallowed_macros)]
fn test_write_m4b_roundtrip_basic_fields() {
    let manager = AudioMetadataManager::new();

    let src_path = fixture_path("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");
    let cover_path = fixture_path("tests/media/SINGLE_M4B/cover.jpg");

    let dest_path = fixture_path("tests/media/output_audio_test.m4b");
    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test m4b");
    set_writable(&dest_path);

    let mut metadata = manager.read(&dest_path).expect("Read failed");

    let new_title = "Modified Title (m4b)".to_string();
    metadata.title = Some(new_title.clone());
    metadata.authors = vec!["Test Author".to_string()];
    metadata.narrators = vec!["Test Narrator".to_string()];
    metadata.total_tracks = Some(10);
    metadata.tags.push("TestTag".to_string());

    let cover_bytes = std::fs::read(&cover_path).expect("Failed to read cover.jpg");
    metadata.cover_image = Some(CoverImage {
        content: cover_bytes,
        media_type: "image/jpeg".to_string(),
    });

    manager.write(&dest_path, &metadata).expect("Write failed");

    let new_metadata = manager.read(&dest_path).expect("Re-read failed");
    assert_eq!(new_metadata.title, Some(new_title));
    assert!(new_metadata.authors.contains(&"Test Author".to_string()));
    assert!(new_metadata
        .narrators
        .contains(&"Test Narrator".to_string()));
    if let Some(total) = new_metadata.total_tracks {
        assert_eq!(total, 10);
    }
    if !new_metadata.tags.is_empty() {
        assert!(new_metadata.tags.contains(&"TestTag".to_string()));
    }

    if let Some(cover_ref) = new_metadata.cover_image_ref {
        assert_eq!(cover_ref.media_type, "image/jpeg");
    }

    if let Err(e) = std::fs::remove_file(&dest_path) {
        eprintln!("Warning: failed to remove test file {dest_path:?}: {e}");
    }
}

#[test]
#[allow(clippy::permissions_set_readonly_false)]
#[allow(clippy::disallowed_macros)]
fn test_write_mp3_roundtrip_basic_fields() {
    let manager = AudioMetadataManager::new();

    let src_path = fixture_path("tests/media/MULT_MP3/Devolution-Part01.mp3");
    let dest_path = fixture_path("tests/media/output_audio_test.mp3");

    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test mp3");
    set_writable(&dest_path);

    let mut metadata = manager.read(&dest_path).expect("Read failed");

    let new_title = "Modified Title (mp3)".to_string();
    metadata.title = Some(new_title.clone());
    metadata.authors = vec!["Test Author".to_string()];
    metadata.narrators = vec!["Test Narrator".to_string()];
    metadata.total_tracks = Some(9);
    metadata.tags.push("TestTag".to_string());

    manager.write(&dest_path, &metadata).expect("Write failed");

    let new_metadata = manager.read(&dest_path).expect("Re-read failed");
    assert_eq!(new_metadata.title, Some(new_title));
    assert!(new_metadata.authors.contains(&"Test Author".to_string()));
    assert!(new_metadata
        .narrators
        .contains(&"Test Narrator".to_string()));
    assert_eq!(new_metadata.total_tracks, Some(9));
    if !new_metadata.tags.is_empty() {
        assert!(new_metadata.tags.contains(&"TestTag".to_string()));
    }

    if let Err(e) = std::fs::remove_file(&dest_path) {
        eprintln!("Warning: failed to remove test file {dest_path:?}: {e}");
    }
}

#[test]
fn test_audio_manager_rejects_unsupported_paths() {
    let manager = AudioMetadataManager::new();

    let txt_path = fixture_path("tests/media/MULT_MP3/DEVOLUTION.txt");
    assert!(!manager.can_handle(&txt_path));

    assert!(manager.read(&txt_path).is_err());
}
