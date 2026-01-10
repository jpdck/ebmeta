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

/// Audiobook classification
#[derive(Debug, Clone)]
pub enum AudiobookType {
    Fiction,
    Nonfiction,
}

/// Contributor to an audiobook (editor, producer, additional narrator).
///
/// This represents contributors beyond the primary authors and narrators.
/// For example, an audiobook might have:
/// - Primary narrator(s) in `Metadata.narrators`
/// - Additional/guest narrators as `Contributor` with `ContributorRole::Narrator`
/// - Editors, producers, translators, etc. as `Contributor` with their respective roles
#[derive(Debug, Clone)]
pub struct Contributor {
    pub name: String,
    pub role: ContributorRole,
}

/// Role of a contributor.
///
/// **Note on Narrator role**: This is for ADDITIONAL or GUEST narrators beyond the
/// primary narrator(s) listed in `Metadata.narrators`. For example:
/// - `Metadata.narrators`: `["John Doe"]` (primary narrator)
/// - `Metadata.contributors`: `[Contributor { name: "Jane Smith", role: Narrator }]` (guest narrator for chapter 5)
#[derive(Debug, Clone)]
pub enum ContributorRole {
    /// Additional/guest narrator (not the primary narrator)
    Narrator,
    Editor,
    Producer,
    Translator,
    Other(String),
}

/// Chapter marker with timestamps
#[derive(Debug, Clone)]
pub struct Chapter {
    pub title: String,
    /// Start time in milliseconds from beginning
    pub start_time_ms: u64,
    /// Optional end time in milliseconds
    pub end_time_ms: Option<u64>,
    /// Track number if chapter corresponds to a file
    pub track_number: Option<u32>,
}

/// Metadata for ebooks and audiobooks.
///
/// ## Narrator Handling
///
/// Narrators are represented in two fields:
/// - `narrators`: Primary narrator(s) who narrate the main content
/// - `contributors` with `ContributorRole::Narrator`: Additional/guest narrators
///
/// This separation allows proper representation of audiobooks with multiple narrators,
/// such as dramatized audiobooks or books with guest narrators for specific chapters.
#[derive(Debug, Clone, Default)]
pub struct Metadata {
    pub title: Option<String>,
    pub authors: Vec<String>,
    /// Primary narrator(s). See also: `contributors` for additional/guest narrators.
    pub narrators: Vec<String>,
    pub series: Option<String>,
    pub series_index: Option<f32>,
    pub volume: Option<String>,
    pub description: Option<String>,
    pub publisher: Option<String>,
    pub language: Option<String>,
    pub published_date: Option<String>,
    pub isbn: Option<String>,
    pub duration_seconds: Option<u64>,
    pub tags: Vec<String>,
    /// Safe cover image reference extracted from the EPUB manifest.
    ///
    /// Design choice: we do not trust EPUB content and do not read cover bytes by default.
    /// Consumers can request or supply bytes explicitly via `cover_image`.
    pub cover_image_ref: Option<CoverImageRef>,
    pub cover_image: Option<CoverImage>,

    // Audiobook-specific fields
    /// Rating from 0-100 (percentage scale)
    pub rating: Option<u8>,
    /// Primary genre classification
    pub genre: Option<String>,
    /// Audio bitrate in kbps (deferred to future enhancement)
    pub bitrate_kbps: Option<u32>,
    /// File format ("mp3", "m4b", "flac", "ogg")
    pub format: Option<String>,
    /// Total number of tracks/chapters
    pub total_tracks: Option<u32>,
    /// Copyright notice
    pub copyright: Option<String>,
    /// Publisher website URL
    pub publisher_url: Option<String>,
    /// Fiction or Nonfiction classification
    pub audiobook_type: Option<AudiobookType>,
    /// Additional contributors beyond primary authors/narrators.
    ///
    /// Examples: editors, producers, translators, guest/additional narrators.
    /// See `ContributorRole` for available roles.
    pub contributors: Vec<Contributor>,
    /// Chapter markers (both embedded and per-file)
    pub chapters: Vec<Chapter>,
}

#[derive(Debug, Clone)]
/// Reference to a cover image embedded in the source file.
///
/// This points at the resource location and media type without loading bytes.
/// Use `CoverImage` when the actual image data is required.
pub struct CoverImageRef {
    pub href: String,
    pub media_type: String,
}

#[derive(Debug, Clone)]
pub struct CoverImage {
    pub content: Vec<u8>,
    pub media_type: String,
}

/// Interface for reading and writing metadata from/to files.
pub trait MetadataIo {
    /// Checks if the file at the given path is supported by this handler.
    fn can_handle(&self, path: &Path) -> bool;

    /// Reads metadata from the file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed.
    fn read(&self, path: &Path) -> Result<Metadata>;

    /// Writes metadata to the file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be written or updated.
    fn write(&self, path: &Path, metadata: &Metadata) -> Result<()>;
}
