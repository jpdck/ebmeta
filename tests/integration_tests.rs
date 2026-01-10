use ebmeta::audio::AudioMetadataManager;
use ebmeta::core::{CoverImage, MetadataIo};
use ebmeta::epub::EpubMetadataManager;
use serde::Deserialize;
use std::path::PathBuf;

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

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn seconds_to_ms(seconds: f64) -> u64 {
    (seconds * 1000.0).round() as u64
}

#[test]
fn test_read_real_epub() {
    let path = fixture_path("tests/media/CameronCooper-SolarWhisper.epub");
    let manager = EpubMetadataManager;

    assert!(manager.can_handle(&path));
    let metadata = manager.read(&path).expect("Failed to read metadata");

    // Strengthened assertions
    assert_eq!(metadata.title.as_deref(), Some("Solar Whisper"));
    assert!(metadata.authors.contains(&"Cameron Cooper".to_string()));
    assert_eq!(metadata.language.as_deref(), Some("en"));
}

#[test]
fn test_write_epub_metadata() {
    let src_path = fixture_path("tests/media/CameronCooper-SolarWhisper.epub");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let dest_path = temp_dir.path().join("output_test.epub");

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
}

#[test]
fn test_write_cover_image() {
    let src_path = fixture_path("tests/media/CameronCooper-SolarWhisper.epub");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let dest_path = temp_dir.path().join("output_cover_test.epub");

    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test epub");

    let manager = EpubMetadataManager;

    // Create dummy cover image
    let dummy_content = vec![0x1, 0x2, 0x3, 0x4];
    let cover = CoverImage {
        content: dummy_content.clone(),
        media_type: "image/jpeg".to_string(),
    };

    let mut metadata = manager.read(&dest_path).expect("Read failed");
    metadata.cover_image = Some(cover);

    manager.write(&dest_path, &metadata).expect("Write failed");

    // Read back
    let new_metadata = manager.read(&dest_path).expect("Failed to read metadata");
    let cover_ref = new_metadata
        .cover_image_ref
        .expect("Cover image ref missing from metadata");
    assert_eq!(cover_ref.media_type, "image/jpeg");
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
fn test_write_m4b_roundtrip_basic_fields() {
    let manager = AudioMetadataManager::new();

    let src_path = fixture_path("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");
    let cover_path = fixture_path("tests/media/SINGLE_M4B/cover.jpg");

    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let dest_path = temp_dir.path().join("output_audio_test.m4b");

    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test m4b");

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
}

#[test]
fn test_write_mp3_roundtrip_basic_fields() {
    let manager = AudioMetadataManager::new();

    let src_path = fixture_path("tests/media/MULT_MP3/Devolution-Part01.mp3");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let dest_path = temp_dir.path().join("output_audio_test.mp3");

    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test mp3");

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
}

#[test]
fn test_audio_manager_rejects_unsupported_paths() {
    let manager = AudioMetadataManager::new();

    let txt_path = fixture_path("tests/media/MULT_MP3/DEVOLUTION.txt");
    assert!(!manager.can_handle(&txt_path));

    assert!(manager.read(&txt_path).is_err());
}

// --- New Tests ---

#[test]
fn epub_read_empty_file_returns_error() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let path = temp_dir.path().join("empty.epub");
    std::fs::write(&path, b"").unwrap();

    let result = EpubMetadataManager.read(&path);
    assert!(result.is_err());
    let msg = format!("{:?}", result.err());
    // Zip error usually
    assert!(
        msg.to_lowercase().contains("zip")
            || msg.to_lowercase().contains("invalid")
            || msg.to_lowercase().contains("error")
    );
}

#[test]
fn epub_read_truncated_zip_returns_error() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let path = temp_dir.path().join("truncated.epub");
    // Valid ZIP header, truncated before central directory
    std::fs::write(&path, [0x50, 0x4B, 0x03, 0x04, 0x00]).unwrap();

    let result = EpubMetadataManager.read(&path);
    assert!(result.is_err());
}

#[test]
fn epub_roundtrip_preserves_all_fields() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let src = fixture_path("tests/media/CameronCooper-SolarWhisper.epub");
    let dest = temp_dir.path().join("roundtrip.epub");
    std::fs::copy(&src, &dest).unwrap();

    let manager = EpubMetadataManager;
    let original = manager.read(&dest).unwrap();
    manager.write(&dest, &original).unwrap();
    let after = manager.read(&dest).unwrap();

    assert_eq!(original.title, after.title);
    assert_eq!(original.authors, after.authors);
    assert_eq!(original.series, after.series);
    assert_eq!(original.isbn, after.isbn);
    // Cover ref should survive
    assert_eq!(
        original.cover_image_ref.as_ref().map(|c| &c.media_type),
        after.cover_image_ref.as_ref().map(|c| &c.media_type)
    );
}

#[test]
fn epub_read_does_not_panic_on_bad_inputs() {
    let cases: &[(&str, &[u8])] = &[
        ("empty", &[]),
        ("random_bytes", &[0xDE, 0xAD, 0xBE, 0xEF]),
        ("zip_header_only", &[0x50, 0x4B, 0x03, 0x04]),
        ("pdf_magic", b"%PDF-1.4"),
        ("null_bytes", &[0u8; 1024]),
    ];

    for (name, bytes) in cases {
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let path = temp_dir.path().join(format!("{name}.epub"));
        std::fs::write(&path, bytes).unwrap();

        // Must not panic, error is acceptable
        let _ = EpubMetadataManager.read(&path);
    }
}

#[test]
fn mp3_read_no_id3_returns_usable_metadata_or_error() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let path = temp_dir.path().join("no_tag.mp3");
    // Minimal MP3 frame sync, no ID3
    std::fs::write(&path, [0xFF, 0xFB, 0x90, 0x00]).unwrap();

    let manager = AudioMetadataManager::new();
    let result = manager.read(&path);
    // Either returns empty metadata or clean error, not panic
    match result {
        Ok(meta) => assert!(meta.title.is_none()),
        Err(e) => assert!(!format!("{e:?}").contains("panic")),
    }
}

#[test]
fn m4b_write_clamps_large_track_numbers() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let src = fixture_path("tests/media/SINGLE_M4B/The_Science_of_Sci-Fi.m4b");
    let dest = temp_dir.path().join("overflow.m4b");
    std::fs::copy(&src, &dest).unwrap();

    let manager = AudioMetadataManager::new();
    let mut meta = manager.read(&dest).unwrap();

    // Values larger than u16::MAX (65535)
    meta.total_tracks = Some(100_000);
    meta.series_index = Some(100_000.0);

    manager
        .write(&dest, &meta)
        .expect("Write should succeed with clamping");

    let readback = manager.read(&dest).unwrap();
    // Should be clamped to 65535, NOT truncated to 34464
    assert_eq!(readback.total_tracks, Some(65535));
    assert_eq!(readback.series_index, Some(65535.0));
}

#[test]
fn mp3_handles_large_track_numbers() {
    let manager = AudioMetadataManager::new();

    let src_path = fixture_path("tests/media/MULT_MP3/Devolution-Part01.mp3");
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let dest_path = temp_dir.path().join("output_mp3_large_track.mp3");

    std::fs::copy(&src_path, &dest_path).expect("Failed to copy test mp3");

    let mut metadata = manager.read(&dest_path).expect("Read failed");

    // MP3 (ID3v2.4) frames can handle large numbers (text based), though TRCK is often numeric.
    // ID3 crate usually handles u32.
    metadata.series_index = Some(65535.0);
    metadata.total_tracks = Some(65535);

    manager.write(&dest_path, &metadata).expect("Write failed");

    let new_metadata = manager.read(&dest_path).expect("Re-read failed");

    if let Some(idx) = new_metadata.series_index {
        assert!((idx - 65535.0).abs() < 1.0, "MP3 should handle u16::MAX");
    }
}

#[test]
fn epub_error_messages_are_user_friendly() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let path = temp_dir.path().join("nonexistent.epub");

    let err = EpubMetadataManager.read(&path).unwrap_err();
    let msg = format!("{err}");

    assert!(!msg.contains("unwrap()"));
    assert!(!msg.contains("called `Option::unwrap()`"));
    assert!(
        msg.to_lowercase().contains("file")
            || msg.to_lowercase().contains("open")
            || msg.to_lowercase().contains("no such")
            || msg.to_lowercase().contains("found")
    );
}
