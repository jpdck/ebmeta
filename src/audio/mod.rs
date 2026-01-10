mod id3;
mod mp4;
mod vorbis;

use crate::core::{Error, Metadata, MetadataIo, Result};
use id3::Id3Handler;
use mp4::Mp4Handler;
use std::path::Path;
use vorbis::VorbisHandler;

pub struct AudioMetadataManager {
    id3: Id3Handler,
    mp4: Mp4Handler,
    vorbis: VorbisHandler,
}

impl Default for AudioMetadataManager {
    fn default() -> Self {
        Self {
            id3: Id3Handler,
            mp4: Mp4Handler,
            vorbis: VorbisHandler,
        }
    }
}

impl AudioMetadataManager {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl MetadataIo for AudioMetadataManager {
    fn can_handle(&self, path: &Path) -> bool {
        self.id3.can_handle(path) || self.mp4.can_handle(path) || self.vorbis.can_handle(path)
    }

    fn read(&self, path: &Path) -> Result<Metadata> {
        if self.id3.can_handle(path) {
            self.id3.read(path)
        } else if self.mp4.can_handle(path) {
            self.mp4.read(path)
        } else if self.vorbis.can_handle(path) {
            self.vorbis.read(path)
        } else {
            Err(Error::Other("Unsupported audio format".to_string()))
        }
    }

    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()> {
        if self.id3.can_handle(path) {
            self.id3.write(path, metadata)
        } else if self.mp4.can_handle(path) {
            self.mp4.write(path, metadata)
        } else if self.vorbis.can_handle(path) {
            self.vorbis.write(path, metadata)
        } else {
            Err(Error::Other("Unsupported audio format".to_string()))
        }
    }
}
