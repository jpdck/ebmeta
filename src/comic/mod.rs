use std::path::Path;
use crate::core::{Metadata, MetadataIo, Result, Error};

pub struct ComicMetadataManager;

impl MetadataIo for ComicMetadataManager {
    fn can_handle(&self, path: &Path) -> bool {
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            matches!(ext.to_lowercase().as_str(), "cbz" | "cbr")
        } else {
            false
        }
    }

    fn read(&self, _path: &Path) -> Result<Metadata> {
        // Todo: Implement comic metadata reading
        Err(Error::Other("Not implemented".to_string()))
    }

    fn write(&self, _path: &Path, _metadata: &Metadata) -> Result<()> {
        // Todo: Implement comic metadata writing
        Err(Error::Other("Not implemented".to_string()))
    }
}
