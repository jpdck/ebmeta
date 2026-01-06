use std::path::Path;
use std::result;

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Format(String),
    Other(String),
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::Io(err)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "IO error: {e}"),
            Error::Format(s) => write!(f, "Format error: {s}"),
            Error::Other(s) => write!(f, "Error: {s}"),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Default)]
pub struct Metadata {
    pub title: Option<String>,
    pub authors: Vec<String>,
    pub narrators: Vec<String>,
    pub series: Option<String>,
    pub series_index: Option<f32>,
    pub volume: Option<String>,
    pub description: Option<String>,
    pub publisher: Option<String>,
    pub language: Option<String>,
    pub published_date: Option<String>,
    pub duration_seconds: Option<u64>,
    pub tags: Vec<String>,
}

/// Interface for reading and writing metadata from/to files.
pub trait MetadataIo {
    /// Checks if the file at the given path is supported by this handler.
    fn can_handle(&self, path: &Path) -> bool;

    /// Reads metadata from the file.
    fn read(&self, path: &Path) -> Result<Metadata>;

    /// Writes metadata to the file.
    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()>;
}
