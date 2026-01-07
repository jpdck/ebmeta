use crate::core::{Error, Metadata, MetadataIo, Result};
use std::path::Path;

pub struct AudioMetadataManager;

impl MetadataIo for AudioMetadataManager {
    fn can_handle(&self, path: &Path) -> bool {
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            matches!(ext.to_lowercase().as_str(), "mp3" | "m4b" | "flac" | "ogg")
        } else {
            false
        }
    }

    fn read(&self, _path: &Path) -> Result<Metadata> {
        // Todo: Implement audio metadata reading
        Err(Error::Other("Not implemented".to_string()))
    }

    fn write(&self, _path: &Path, _metadata: &Metadata) -> Result<()> {
        // Todo: Implement audio metadata writing
        Err(Error::Other("Not implemented".to_string()))
    }
}
