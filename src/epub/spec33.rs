//! EPUB 3.3 Specification Enforcement
//! 
//! Ref: https://www.w3.org/TR/epub-33/

#[derive(Debug, Default)]
pub struct PackageDocument {
    pub version: String,
    pub unique_identifier: String,
    pub metadata: PackageMetadata,
    pub manifest: Manifest,
    pub spine: Spine,
}

#[derive(Debug, Default)]
pub struct PackageMetadata {
    pub titles: Vec<String>,
    pub languages: Vec<String>,
    pub identifiers: Vec<Identifier>,
    pub modified: Vec<String>,
}

#[derive(Debug, Default)]
pub struct Identifier {
    pub id: Option<String>,
    pub value: String,
}

#[derive(Debug, Default)]
pub struct Manifest {
    pub items: Vec<ManifestItem>,
}

#[derive(Debug, Default)]
pub struct ManifestItem {
    pub id: String,
    pub href: String,
    pub media_type: String,
    pub properties: Option<String>,
}

#[derive(Debug, Default)]
pub struct Spine {
    pub page_progression_direction: Option<String>,
    pub itemrefs: Vec<SpineItemRef>,
}

#[derive(Debug, Default)]
pub struct SpineItemRef {
    pub idref: String,
}

pub fn validate_package_document(pkg: &PackageDocument) -> Result<(), String> {
    validate_version(&pkg.version)?;
    validate_metadata(&pkg.metadata, &pkg.unique_identifier)?;
    validate_manifest(&pkg.manifest)?;
    validate_spine(&pkg.spine, &pkg.manifest)?;
    Ok(())
}

fn validate_version(_version: &str) -> Result<(), String> {
    // TODO: EPUB 3.3 defines version must be "3.0"
    todo!()
}

fn validate_metadata(_metadata: &PackageMetadata, _unique_identifier_id: &str) -> Result<(), String> {
    // TODO: Check required elements (title, language, identifier, modified)
    // TODO: Check that unique_identifier_id refers to an existing dc:identifier
    // TODO: Check dcterms:modified syntax (CCYY-MM-DDThh:mm:ssZ) and that exactly one exists
    // TODO: Check that titles and identifiers are non-empty strings
    // TODO: Check that languages are valid BCP 47 tags
    todo!()
}

fn validate_manifest(_manifest: &Manifest) -> Result<(), String> {
    // TODO: Check for unique IDs
    // TODO: Check for Navigation Document presence (properties="nav")
    // TODO: Check hrefs do not contain fragments
    todo!()
}

fn validate_spine(_spine: &Spine, _manifest: &Manifest) -> Result<(), String> {
    // TODO: Check all idrefs exist in manifest
    // TODO: Check page-progression-direction validity (ltr, rtl, default)
    todo!()
}

pub fn parse_opf(_content: &str) -> Result<PackageDocument, String> {
    // TODO: Parse XML content into PackageDocument struct
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_document_version_must_be_3_0() {
        let pkg = PackageDocument {
            version: "2.0".to_string(),
            ..Default::default()
        };
        let result = validate_package_document(&pkg);
        assert!(result.is_err(), "EPUB 3.3 requires version 3.0 or higher on package element");
        // Check specific error message if possible
    }

    #[test]
    fn test_unique_identifier_is_required() {
        let pkg = PackageDocument {
            version: "3.0".to_string(),
            unique_identifier: "".to_string(), // Missing or empty
            ..Default::default()
        };
        // Assuming we fill the rest with valid data
        let result = validate_package_document(&pkg);
        assert!(result.is_err());
    }

    #[test]
    fn test_required_metadata_elements() {
        // EPUB 3.3 Core 3.4.1: must include title, language, identifier
        let mut pkg = PackageDocument {
            version: "3.0".to_string(),
            unique_identifier: "uid".to_string(),
            metadata: PackageMetadata {
                identifiers: vec![Identifier { id: Some("uid".into()), value: "uuid".into() }],
                modified: vec!["2023-01-01T00:00:00Z".into()],
                ..Default::default()
            },
            ..Default::default()
        };
        
        // Missing title
        pkg.metadata.titles = vec![];
        pkg.metadata.languages = vec!["en".to_string()];
        pkg.metadata.modified = vec!["2023-01-01T00:00:00Z".to_string()];
        assert!(validate_package_document(&pkg).is_err(), "Missing title should fail");

        // Missing language
        pkg.metadata.titles = vec!["Title".to_string()];
        pkg.metadata.languages = vec![];
        assert!(validate_package_document(&pkg).is_err(), "Missing language should fail");

        // Missing identifier
        pkg.metadata.languages = vec!["en".to_string()];
        pkg.metadata.identifiers = vec![];
        assert!(validate_package_document(&pkg).is_err(), "Missing identifier should fail");

        // Missing modified property (required in EPUB 3)
        pkg.metadata.identifiers = vec![Identifier { id: Some("uid".into()), value: "uuid".into() }];
        pkg.metadata.modified = vec![];
        assert!(validate_package_document(&pkg).is_err(), "Missing dcterms:modified should fail");
    }

    #[test]
    fn test_unique_identifier_resolution() {
        // The package unique-identifier attribute must reference a dc:identifier element by its ID
        let pkg = PackageDocument {
            version: "3.0".to_string(),
            unique_identifier: "pub-id".to_string(),
            metadata: PackageMetadata {
                titles: vec!["T".into()],
                languages: vec!["en".into()],
                identifiers: vec![
                    Identifier { id: Some("pub-id".into()), value: "urn:uuid:12345".into() }
                ],
                modified: vec!["2023-01-01T00:00:00Z".into()],
            },
            ..Default::default()
        };
        
        let _result = validate_package_document(&pkg);
    }

    #[test]
    fn test_metadata_cardinality_and_content() {
        // Strict checks for non-empty strings and single modified date
        let mut pkg = PackageDocument {
            version: "3.0".to_string(),
            unique_identifier: "uid".to_string(),
            metadata: PackageMetadata {
                titles: vec!["T".into()],
                languages: vec!["en".into()],
                identifiers: vec![Identifier { id: Some("uid".into()), value: "uuid".into() }],
                modified: vec!["2023-01-01T00:00:00Z".into()],
            },
            ..Default::default()
        };

        // Multiple modified dates (forbidden)
        pkg.metadata.modified.push("2023-01-02T00:00:00Z".into());
        assert!(validate_package_document(&pkg).is_err(), "Multiple modified dates should fail");
        pkg.metadata.modified.pop();

        // Empty title string
        pkg.metadata.titles = vec!["".into()];
        assert!(validate_package_document(&pkg).is_err(), "Empty title string should fail");
        pkg.metadata.titles = vec!["T".into()];

        // Empty identifier string
        pkg.metadata.identifiers = vec![Identifier { id: Some("uid".into()), value: "   ".into() }]; // whitespace only or empty
        assert!(validate_package_document(&pkg).is_err(), "Empty/whitespace identifier should fail");
        pkg.metadata.identifiers = vec![Identifier { id: Some("uid".into()), value: "uuid".into() }];

        // Invalid BCP 47 language
        pkg.metadata.languages = vec!["not-a-lang-code".into()]; // simplistic check, real BCP 47 is complex
        assert!(validate_package_document(&pkg).is_err(), "Invalid BCP 47 language should fail");
    }

    #[test]
    fn test_manifest_nav_document_required() {
        // EPUB 3 requires a Navigation Document
        let mut manifest = Manifest::default();
        manifest.items.push(ManifestItem {
            id: "nav".into(),
            href: "nav.xhtml".into(),
            media_type: "application/xhtml+xml".into(),
            properties: None, // Missing "nav"
        });

        let result = validate_manifest(&manifest);
        assert!(result.is_err(), "Manifest without nav property should fail");

        manifest.items[0].properties = Some("nav".into());
        // Now it should pass (assuming stub allows it or we mock it)
        // Since validate_manifest is todo!(), this test will panic if run, which is expected for TDD.
    }

    #[test]
    fn test_spine_idref_validation() {
        let manifest = Manifest {
            items: vec![
                ManifestItem { id: "item1".into(), href: "1.html".into(), media_type: "application/xhtml+xml".into(), properties: None }
            ]
        };

        let spine = Spine {
            page_progression_direction: None,
            itemrefs: vec![
                SpineItemRef { idref: "item1".into() }, // Valid
                SpineItemRef { idref: "item2".into() }, // Invalid, not in manifest
            ]
        };

        let result = validate_spine(&spine, &manifest);
        assert!(result.is_err(), "Spine referencing missing manifest item should fail");
    }

    #[test]
    fn test_modified_date_syntax() {
        // EPUB 3.3 Core 3.4.1.4: must be CCYY-MM-DDThh:mm:ssZ
        let mut pkg = PackageDocument {
            version: "3.0".to_string(),
            unique_identifier: "uid".to_string(),
            metadata: PackageMetadata {
                titles: vec!["T".into()],
                languages: vec!["en".into()],
                identifiers: vec![Identifier { id: Some("uid".into()), value: "uuid".into() }],
                modified: vec!["2023-01-01".into()], // Invalid format (missing time/Z)
            },
            ..Default::default()
        };

        assert!(validate_package_document(&pkg).is_err(), "Invalid date format should fail");

        pkg.metadata.modified = vec!["2023-01-01T12:00:00Z".into()];
        // Should pass or at least fail differently if implemented
        // let _ = validate_package_document(&pkg); 
    }

    #[test]
    fn test_spine_page_progression_direction() {
        let manifest = Manifest {
             items: vec![ManifestItem { id: "i1".into(), href: "1.html".into(), media_type: "application/xhtml+xml".into(), properties: None }]
        };

        let spine = Spine {
            page_progression_direction: Some("invalid-dir".into()),
            itemrefs: vec![SpineItemRef { idref: "i1".into() }]
        };

        let mut pkg = PackageDocument::default();
        pkg.version = "3.0".to_string();
        pkg.manifest = manifest;
        // Mock other required fields to focus on spine failure
        pkg.metadata.titles = vec!["T".into()];
        pkg.metadata.languages = vec!["en".into()];
        pkg.metadata.identifiers = vec![Identifier { id: Some("uid".into()), value: "u".into() }];
        pkg.unique_identifier = "uid".into();
        pkg.metadata.modified = vec!["2023-01-01T00:00:00Z".into()];
        
        // Inject bad spine
        pkg.spine = spine;

        assert!(validate_package_document(&pkg).is_err(), "Invalid page-progression-direction should fail");
    }

    #[test]
    fn test_manifest_item_href_no_fragment() {
        // EPUB 3.3 Core 3.4.1.6: href MUST NOT include a fragment identifier
        let mut manifest = Manifest::default();
        manifest.items.push(ManifestItem {
            id: "i1".into(),
            href: "chapter1.html#fragment".into(), // Invalid
            media_type: "application/xhtml+xml".into(),
            properties: None,
        });

        let result = validate_manifest(&manifest);
        assert!(result.is_err(), "Manifest href with fragment should fail");
    }
}
