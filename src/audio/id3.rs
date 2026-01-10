use crate::core::{CoverImage, Error, Metadata, MetadataIo, Result};
use id3::{Tag, TagLike};
use std::path::Path;

pub struct Id3Handler;

impl MetadataIo for Id3Handler {
    fn can_handle(&self, path: &Path) -> bool {
        path.extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| e.eq_ignore_ascii_case("mp3"))
    }

    fn read(&self, path: &Path) -> Result<Metadata> {
        let tag =
            Tag::read_from_path(path).map_err(|e| Error::Other(format!("ID3 read error: {e}")))?;

        Ok(Metadata {
            title: tag.title().map(ToString::to_string),
            authors: extract_authors(&tag),
            narrators: extract_narrators(&tag),
            series: tag.album().map(ToString::to_string),
            series_index: extract_track_number(&tag),
            description: extract_comments(&tag),
            published_date: extract_year(&tag),
            genre: tag.genre().map(ToString::to_string),
            duration_seconds: tag.duration().map(u64::from),
            rating: extract_rating(&tag),
            format: Some("mp3".to_string()),
            total_tracks: tag.total_tracks(),
            copyright: extract_copyright(&tag),
            tags: extract_tags(&tag),
            cover_image: extract_cover(&tag),
            // Chapter support will be added later
            chapters: Vec::new(),
            contributors: Vec::new(),
            // Fields not commonly in ID3 tags
            language: None,
            isbn: None,
            volume: None,
            publisher: None,
            publisher_url: None,
            audiobook_type: None,
            bitrate_kbps: None,
            cover_image_ref: None,
        })
    }

    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()> {
        let mut tag = Tag::read_from_path(path).unwrap_or_else(|_| Tag::new());

        // Set title
        if let Some(title) = &metadata.title {
            tag.set_title(title);
        } else {
            tag.remove_title();
        }

        // Set authors (TPE1 - Artist)
        if metadata.authors.is_empty() {
            tag.remove_artist();
        } else {
            tag.set_artist(metadata.authors.join("; "));
        }

        // Set narrators (TPE2 - Album Artist)
        if metadata.narrators.is_empty() {
            tag.remove_album_artist();
        } else {
            tag.set_album_artist(metadata.narrators.join("; "));
        }

        // Set series (TALB - Album)
        if let Some(series) = &metadata.series {
            tag.set_album(series);
        } else {
            tag.remove_album();
        }

        // Set series index (TRCK - Track)
        if let Some(idx) = metadata.series_index {
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            tag.set_track(idx as u32);
        }

        // Set genre
        if let Some(genre) = &metadata.genre {
            tag.set_genre(genre);
        } else {
            tag.remove_genre();
        }

        // Set year
        if let Some(date) = &metadata.published_date {
            if let Some(year) = extract_year_number(date) {
                tag.set_year(year);
            } else {
                tag.remove_year();
            }
        } else {
            tag.remove_year();
        }

        // Set total tracks
        if let Some(total) = metadata.total_tracks {
            tag.set_total_tracks(total);
        } else {
            tag.remove_total_tracks();
        }

        // Set description (COMM - Comments)
        // First remove existing comment frames to prevent accumulation
        tag.remove("COMM");
        if let Some(desc) = &metadata.description {
            tag.add_frame(id3::frame::Comment {
                lang: "eng".to_string(),
                description: String::new(),
                text: desc.clone(),
            });
        }

        // Set copyright (TCOP)
        // First remove existing copyright frames to prevent accumulation
        tag.remove("TCOP");
        if let Some(copyright) = &metadata.copyright {
            use id3::frame::{Content, Frame};
            let frame = Frame::with_content("TCOP", Content::Text(copyright.clone()));
            tag.add_frame(frame);
        }

        // Set rating (POPM - Popularimeter)
        if let Some(rating) = metadata.rating {
            write_rating(&mut tag, rating);
        } else {
            // Remove existing rating frame when no rating is provided
            tag.remove("POPM");
        }

        // Set tags (TXXX - User defined text with description "KEYWORDS")
        // First remove all existing KEYWORDS frames
        tag.remove_extended_text(Some("KEYWORDS"), None);
        if !metadata.tags.is_empty() {
            use id3::frame::ExtendedText;
            tag.add_frame(ExtendedText {
                description: "KEYWORDS".to_string(),
                value: metadata.tags.join("; "),
            });
        }

        // Set cover image (APIC)
        if let Some(cover) = &metadata.cover_image {
            write_cover(&mut tag, cover);
        } else {
            // Remove existing cover images when no cover is provided
            tag.remove("APIC");
        }

        tag.write_to_path(path, id3::Version::Id3v24)
            .map_err(|e| Error::Other(format!("ID3 write error: {e}")))?;

        Ok(())
    }
}

// Helper functions for reading

fn extract_authors(tag: &Tag) -> Vec<String> {
    tag.artist()
        .map(|a| {
            a.split(';')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

fn extract_narrators(tag: &Tag) -> Vec<String> {
    tag.album_artist()
        .map(|n| {
            n.split(';')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

fn extract_track_number(tag: &Tag) -> Option<f32> {
    #[allow(clippy::cast_precision_loss)]
    tag.track().map(|t| t as f32)
}

fn extract_comments(tag: &Tag) -> Option<String> {
    tag.comments().next().map(|c| c.text.clone())
}

fn extract_year(tag: &Tag) -> Option<String> {
    tag.year().map(|y| y.to_string())
}

fn extract_copyright(tag: &Tag) -> Option<String> {
    tag.get("TCOP").and_then(|frame| {
        if let id3::Content::Text(text) = frame.content() {
            Some(text.clone())
        } else {
            None
        }
    })
}

fn extract_rating(tag: &Tag) -> Option<u8> {
    // POPM (Popularimeter) uses 0-255 scale, convert to 0-100
    tag.get("POPM").and_then(|frame| {
        if let id3::Content::Popularimeter(popm) = frame.content() {
            // Convert 0-255 to 0-100
            #[allow(clippy::cast_possible_truncation)]
            Some(((u16::from(popm.rating) * 100) / 255) as u8)
        } else {
            None
        }
    })
}

fn extract_tags(tag: &Tag) -> Vec<String> {
    tag.extended_texts()
        .filter(|ext| ext.description.eq_ignore_ascii_case("KEYWORDS"))
        .flat_map(|ext| {
            ext.value
                .split(';')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
        })
        .collect()
}

fn extract_cover(tag: &Tag) -> Option<CoverImage> {
    tag.pictures().next().map(|pic| CoverImage {
        content: pic.data.clone(),
        media_type: pic.mime_type.clone(),
    })
}

// Helper functions for writing

fn extract_year_number(date_str: &str) -> Option<i32> {
    // Try to extract a 4-digit year from various date formats
    if date_str.len() >= 4 {
        date_str[..4].parse::<i32>().ok()
    } else {
        None
    }
}

fn write_rating(tag: &mut Tag, rating: u8) {
    use id3::frame::{Content, Frame, Popularimeter};

    // Remove existing POPM frames to prevent accumulation
    tag.remove("POPM");

    // Convert 0-100 to 0-255
    #[allow(clippy::cast_possible_truncation)]
    let popm_rating = ((u16::from(rating) * 255) / 100) as u8;

    let popm = Popularimeter {
        user: String::new(),
        rating: popm_rating,
        counter: 0,
    };

    let frame = Frame::with_content("POPM", Content::Popularimeter(popm));
    tag.add_frame(frame);
}

fn write_cover(tag: &mut Tag, cover: &CoverImage) {
    use id3::frame::{Picture, PictureType};

    // Remove existing picture frames to prevent accumulation
    tag.remove("APIC");

    let picture = Picture {
        mime_type: cover.media_type.clone(),
        picture_type: PictureType::CoverFront,
        description: String::new(),
        data: cover.content.clone(),
    };

    tag.add_frame(picture);
}

#[cfg(test)]
mod tests {
    use super::*;
    use id3::frame::{Content, Frame, Popularimeter};

    #[test]
    fn test_rating_conversion_boundary_values() {
        // Test 0-255 → 0-100 conversion at boundary values

        let mut tag = Tag::new();

        // Test rating = 0
        let popm_0 = Popularimeter {
            user: String::new(),
            rating: 0,
            counter: 0,
        };
        tag.add_frame(Frame::with_content("POPM", Content::Popularimeter(popm_0)));
        assert_eq!(extract_rating(&tag), Some(0));

        // Test rating = 255
        let mut tag = Tag::new();
        let popm_255 = Popularimeter {
            user: String::new(),
            rating: 255,
            counter: 0,
        };
        tag.add_frame(Frame::with_content(
            "POPM",
            Content::Popularimeter(popm_255),
        ));
        assert_eq!(extract_rating(&tag), Some(100));

        // Test rating = 127 (middle value)
        let mut tag = Tag::new();
        let popm_127 = Popularimeter {
            user: String::new(),
            rating: 127,
            counter: 0,
        };
        tag.add_frame(Frame::with_content(
            "POPM",
            Content::Popularimeter(popm_127),
        ));
        let result = extract_rating(&tag).expect("Should have rating");
        assert!((49..=50).contains(&result)); // 127 * 100 / 255 ≈ 49.8
    }

    #[test]
    fn test_rating_write_boundary_values() {
        // Test 0-100 → 0-255 conversion at boundary values
        let mut tag = Tag::new();

        // Test rating = 0
        write_rating(&mut tag, 0);
        let frame = tag.get("POPM").expect("Should have POPM frame");
        if let Content::Popularimeter(popm) = frame.content() {
            assert_eq!(popm.rating, 0);
        } else {
            panic!("Expected Popularimeter content");
        }

        // Test rating = 100
        let mut tag = Tag::new();
        write_rating(&mut tag, 100);
        let frame = tag.get("POPM").expect("Should have POPM frame");
        if let Content::Popularimeter(popm) = frame.content() {
            assert_eq!(popm.rating, 255);
        } else {
            panic!("Expected Popularimeter content");
        }

        // Test rating = 50
        let mut tag = Tag::new();
        write_rating(&mut tag, 50);
        let frame = tag.get("POPM").expect("Should have POPM frame");
        if let Content::Popularimeter(popm) = frame.content() {
            assert!(popm.rating >= 127 && popm.rating <= 128); // 50 * 255 / 100 = 127.5
        } else {
            panic!("Expected Popularimeter content");
        }
    }

    #[test]
    fn test_rating_round_trip_approximation() {
        // Test that read(write(x)) ≈ x for key values
        // Due to rounding, we can't expect exact equality for all values

        for original in [0, 25, 50, 75, 100] {
            let mut tag = Tag::new();
            write_rating(&mut tag, original);
            let read_back = extract_rating(&tag).expect("Should read rating back");

            // Allow ±1 for rounding errors
            let diff = read_back.abs_diff(original);
            assert!(
                diff <= 1,
                "Rating {original} became {read_back} after round-trip (diff={diff})"
            );
        }
    }

    #[test]
    fn test_extract_authors_handles_empty_strings() {
        let mut tag = Tag::new();

        // Test empty string after split
        tag.set_artist("Author1;;Author2");
        let authors = extract_authors(&tag);
        assert_eq!(authors, vec!["Author1", "Author2"]);
        assert!(!authors.contains(&String::new()));

        // Test leading/trailing semicolons
        let mut tag = Tag::new();
        tag.set_artist(";Author1;Author2;");
        let authors = extract_authors(&tag);
        assert_eq!(authors, vec!["Author1", "Author2"]);

        // Test only semicolons
        let mut tag = Tag::new();
        tag.set_artist(";;;");
        let authors = extract_authors(&tag);
        assert!(authors.is_empty());

        // Test normal case
        let mut tag = Tag::new();
        tag.set_artist("Author1; Author2 ; Author3");
        let authors = extract_authors(&tag);
        assert_eq!(authors, vec!["Author1", "Author2", "Author3"]);
    }

    #[test]
    fn test_extract_narrators_handles_empty_strings() {
        let mut tag = Tag::new();

        // Test empty string after split
        tag.set_album_artist("Narrator1;;Narrator2");
        let narrators = extract_narrators(&tag);
        assert_eq!(narrators, vec!["Narrator1", "Narrator2"]);
        assert!(!narrators.contains(&String::new()));

        // Test leading/trailing semicolons
        let mut tag = Tag::new();
        tag.set_album_artist(";Narrator1;");
        let narrators = extract_narrators(&tag);
        assert_eq!(narrators, vec!["Narrator1"]);

        // Test only semicolons
        let mut tag = Tag::new();
        tag.set_album_artist(";");
        let narrators = extract_narrators(&tag);
        assert!(narrators.is_empty());
    }

    #[test]
    fn test_extract_year_number() {
        assert_eq!(extract_year_number("2023"), Some(2023));
        assert_eq!(extract_year_number("2023-01-15"), Some(2023));
        assert_eq!(extract_year_number("2023-12-31T23:59:59"), Some(2023));
        assert_eq!(extract_year_number("abc"), None);
        assert_eq!(extract_year_number(""), None);
        assert_eq!(extract_year_number("202"), None);
    }
}
