use crate::core::{CoverImage, Error, Metadata, MetadataIo, Result};
use mp4ameta::{Img, Tag};
use std::path::Path;

pub struct Mp4Handler;

impl MetadataIo for Mp4Handler {
    fn can_handle(&self, path: &Path) -> bool {
        matches!(
            path.extension().and_then(|e| e.to_str()),
            Some("m4b" | "m4a" | "mp4")
        )
    }

    fn read(&self, path: &Path) -> Result<Metadata> {
        let tag =
            Tag::read_from_path(path).map_err(|e| Error::Other(format!("MP4 read error: {e}")))?;

        let format = path
            .extension()
            .and_then(|e| e.to_str())
            .map(str::to_lowercase);

        Ok(Metadata {
            title: tag.title().map(ToString::to_string),
            authors: extract_authors(&tag),
            narrators: extract_narrators(&tag),
            series: tag.album().map(ToString::to_string),
            series_index: extract_track_number(&tag),
            description: tag.comment().map(ToString::to_string),
            published_date: tag.year().map(ToString::to_string),
            genre: tag.genre().map(ToString::to_string),
            rating: None, // MP4 doesn't have standard rating atom
            format,
            total_tracks: tag.total_tracks().map(u32::from),
            copyright: tag.copyright().map(ToString::to_string),
            cover_image: extract_cover(&tag),
            // Chapter support will be added later
            chapters: Vec::new(),
            contributors: Vec::new(),
            // Fields not commonly in MP4 atoms
            language: None,
            isbn: None,
            volume: None,
            duration_seconds: None,
            publisher: None,
            publisher_url: None,
            audiobook_type: None,
            bitrate_kbps: None,
            tags: Vec::new(),
            cover_image_ref: None,
        })
    }

    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()> {
        let mut tag = Tag::read_from_path(path).unwrap_or_default();

        // Set title (©nam)
        if let Some(title) = &metadata.title {
            tag.set_title(title);
        } else {
            tag.remove_title();
        }

        // Set authors (©ART - Artist)
        if metadata.authors.is_empty() {
            tag.remove_artists();
        } else {
            tag.set_artist(metadata.authors.join("; "));
        }

        // Set narrators (aART - Album Artist)
        if metadata.narrators.is_empty() {
            tag.remove_album_artists();
        } else {
            tag.set_album_artist(metadata.narrators.join("; "));
        }

        // Set series (©alb - Album)
        if let Some(series) = &metadata.series {
            tag.set_album(series);
        } else {
            tag.remove_album();
        }

        // Set series index (track number)
        if let Some(idx) = metadata.series_index {
            let total = metadata.total_tracks.unwrap_or(0);
            // Saturate f32 -> u16 (standard behavior)
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            let track_num = idx as u16;
            // Clamp u32 -> u16 to avoid wrapping
            #[allow(clippy::cast_possible_truncation)]
            let track_total = total.min(u32::from(u16::MAX)) as u16;
            tag.set_track(track_num, track_total);
        }

        // Set genre (©gen)
        if let Some(genre) = &metadata.genre {
            tag.set_genre(genre);
        } else {
            tag.remove_genres();
        }

        // Set year (©day)
        if let Some(date) = &metadata.published_date {
            tag.set_year(date);
        } else {
            tag.remove_year();
        }

        // Set description (©cmt - Comment)
        if let Some(desc) = &metadata.description {
            tag.set_comment(desc);
        } else {
            tag.remove_comments();
        }

        // Set copyright (cprt)
        if let Some(copyright) = &metadata.copyright {
            tag.set_copyright(copyright);
        } else {
            tag.remove_copyright();
        }

        // Set cover image (covr)
        if let Some(cover) = &metadata.cover_image {
            write_cover(&mut tag, cover);
        }

        tag.write_to_path(path)
            .map_err(|e| Error::Other(format!("MP4 write error: {e}")))?;

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
    tag.track_number().map(f32::from)
}

fn extract_cover(tag: &Tag) -> Option<CoverImage> {
    tag.artwork().map(|img| {
        let media_type = match img.fmt {
            mp4ameta::ImgFmt::Jpeg => "image/jpeg",
            mp4ameta::ImgFmt::Png => "image/png",
            mp4ameta::ImgFmt::Bmp => "image/bmp",
        };

        CoverImage {
            content: img.data.to_vec(),
            media_type: media_type.to_string(),
        }
    })
}

// Helper functions for writing

fn write_cover(tag: &mut Tag, cover: &CoverImage) {
    let fmt = match cover.media_type.as_str() {
        "image/png" => mp4ameta::ImgFmt::Png,
        "image/bmp" => mp4ameta::ImgFmt::Bmp,
        _ => mp4ameta::ImgFmt::Jpeg,
    };

    let img: Img<Vec<u8>> = Img {
        fmt,
        data: cover.content.clone(),
    };

    tag.set_artwork(img);
}
