use crate::core::{CoverImage, Error, Metadata, MetadataIo, Result};
use metaflac::block::VorbisComment;
use metaflac::Tag as FlacTag;
use std::path::Path;

pub struct VorbisHandler;

impl MetadataIo for VorbisHandler {
    fn can_handle(&self, path: &Path) -> bool {
        matches!(
            path.extension().and_then(|e| e.to_str()),
            Some("flac" | "ogg" | "opus")
        )
    }

    fn read(&self, path: &Path) -> Result<Metadata> {
        let ext = path.extension().and_then(|e| e.to_str());

        match ext {
            Some("flac") => read_flac(path),
            Some("ogg" | "opus") => {
                // OGG support deferred - more complex parsing needed
                Err(Error::Other(
                    "OGG/Opus support not yet implemented".to_string(),
                ))
            }
            _ => Err(Error::Other("Unsupported format".to_string())),
        }
    }

    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()> {
        let ext = path.extension().and_then(|e| e.to_str());

        match ext {
            Some("flac") => write_flac(path, metadata),
            Some("ogg" | "opus") => {
                // OGG support deferred
                Err(Error::Other(
                    "OGG/Opus support not yet implemented".to_string(),
                ))
            }
            _ => Err(Error::Other("Unsupported format".to_string())),
        }
    }
}

fn read_flac(path: &Path) -> Result<Metadata> {
    let tag =
        FlacTag::read_from_path(path).map_err(|e| Error::Other(format!("FLAC read error: {e}")))?;

    let vorbis = tag
        .vorbis_comments()
        .ok_or_else(|| Error::Other("No Vorbis comments found".to_string()))?;

    Ok(Metadata {
        title: get_first_tag(vorbis, "TITLE"),
        authors: get_all_tags(vorbis, "ARTIST"),
        narrators: get_all_tags(vorbis, "PERFORMER"),
        series: get_first_tag(vorbis, "ALBUM"),
        series_index: get_first_tag(vorbis, "TRACKNUMBER").and_then(|s| s.parse::<f32>().ok()),
        description: get_first_tag(vorbis, "DESCRIPTION")
            .or_else(|| get_first_tag(vorbis, "COMMENT")),
        publisher: get_first_tag(vorbis, "ORGANIZATION"),
        published_date: get_first_tag(vorbis, "DATE"),
        genre: get_first_tag(vorbis, "GENRE"),
        language: get_first_tag(vorbis, "LANGUAGE"),
        copyright: get_first_tag(vorbis, "COPYRIGHT"),
        rating: parse_rating(get_first_tag(vorbis, "RATING")),
        format: Some("flac".to_string()),
        total_tracks: get_first_tag(vorbis, "TOTALTRACKS").and_then(|s| s.parse::<u32>().ok()),
        cover_image: extract_flac_picture(&tag),
        tags: get_all_tags(vorbis, "KEYWORDS"),
        // Chapter support will be added later
        chapters: Vec::new(),
        contributors: Vec::new(),
        // Fields not commonly in Vorbis comments
        isbn: None,
        volume: None,
        duration_seconds: None,
        publisher_url: None,
        audiobook_type: None,
        bitrate_kbps: None,
        cover_image_ref: None,
    })
}

fn write_flac(path: &Path, metadata: &Metadata) -> Result<()> {
    let mut tag = FlacTag::read_from_path(path).unwrap_or_else(|_| FlacTag::new());

    {
        let vorbis = tag.vorbis_comments_mut();

        // Set title
        set_tag(vorbis, "TITLE", metadata.title.as_deref());

        // Set authors
        set_tags(vorbis, "ARTIST", &metadata.authors);

        // Set narrators
        set_tags(vorbis, "PERFORMER", &metadata.narrators);

        // Set series
        set_tag(vorbis, "ALBUM", metadata.series.as_deref());

        // Set series index
        if let Some(idx) = metadata.series_index {
            set_tag(vorbis, "TRACKNUMBER", Some(&idx.to_string()));
        } else {
            vorbis.remove("TRACKNUMBER");
        }

        // Set genre
        set_tag(vorbis, "GENRE", metadata.genre.as_deref());

        // Set date
        set_tag(vorbis, "DATE", metadata.published_date.as_deref());

        // Set description
        set_tag(vorbis, "DESCRIPTION", metadata.description.as_deref());

        // Set publisher
        set_tag(vorbis, "ORGANIZATION", metadata.publisher.as_deref());

        // Set copyright
        set_tag(vorbis, "COPYRIGHT", metadata.copyright.as_deref());

        // Set language
        set_tag(vorbis, "LANGUAGE", metadata.language.as_deref());

        // Set rating
        if let Some(rating) = metadata.rating {
            set_tag(vorbis, "RATING", Some(&rating.to_string()));
        } else {
            vorbis.remove("RATING");
        }

        // Set total tracks
        if let Some(total) = metadata.total_tracks {
            set_tag(vorbis, "TOTALTRACKS", Some(&total.to_string()));
        } else {
            vorbis.remove("TOTALTRACKS");
        }

        // Set tags/keywords
        set_tags(vorbis, "KEYWORDS", &metadata.tags);
    }

    // Set cover image (PICTURE block)
    if let Some(cover) = &metadata.cover_image {
        write_flac_picture(&mut tag, cover);
    } else {
        // Remove all existing PICTURE blocks if no cover image is provided
        use metaflac::block::PictureType;
        tag.remove_picture_type(PictureType::CoverFront);
    }

    tag.write_to_path(path)
        .map_err(|e| Error::Other(format!("FLAC write error: {e}")))?;

    Ok(())
}

// Helper functions for reading

fn get_first_tag(vorbis: &VorbisComment, key: &str) -> Option<String> {
    vorbis
        .get(key)
        .and_then(|v| v.first())
        .map(std::string::ToString::to_string)
}

fn get_all_tags(vorbis: &VorbisComment, key: &str) -> Vec<String> {
    vorbis.get(key).cloned().unwrap_or_default()
}

fn parse_rating(rating_str: Option<String>) -> Option<u8> {
    rating_str.and_then(|s| {
        // Try direct parse first (e.g., "80")
        if let Ok(val) = s.parse::<u8>() {
            return Some(val.min(100));
        }

        // Try fraction format (e.g., "4/5")
        if s.contains('/') {
            let parts: Vec<&str> = s.split('/').collect();
            if parts.len() == 2 {
                let num = parts[0].trim().parse::<f32>().ok()?;
                let denom = parts[1].trim().parse::<f32>().ok()?;
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                return Some(((num / denom) * 100.0) as u8);
            }
        }

        None
    })
}

fn extract_flac_picture(tag: &FlacTag) -> Option<CoverImage> {
    tag.pictures().next().map(|pic| CoverImage {
        content: pic.data.clone(),
        media_type: pic.mime_type.clone(),
    })
}

// Helper functions for writing

fn set_tag(vorbis: &mut VorbisComment, key: &str, value: Option<&str>) {
    vorbis.remove(key);
    if let Some(v) = value {
        vorbis.set(key, vec![v]);
    }
}

fn set_tags(vorbis: &mut VorbisComment, key: &str, values: &[String]) {
    vorbis.remove(key);
    if !values.is_empty() {
        vorbis.set(key, values.iter().map(String::as_str).collect());
    }
}

fn write_flac_picture(tag: &mut FlacTag, cover: &CoverImage) {
    use metaflac::block::{Block, Picture, PictureType};

    // Remove existing pictures
    tag.remove_picture_type(PictureType::CoverFront);

    // Add new picture
    let picture = Picture {
        picture_type: PictureType::CoverFront,
        mime_type: cover.media_type.clone(),
        description: String::new(),
        width: 0,
        height: 0,
        depth: 0,
        num_colors: 0,
        data: cover.content.clone(),
    };

    tag.push_block(Block::Picture(picture));
}
