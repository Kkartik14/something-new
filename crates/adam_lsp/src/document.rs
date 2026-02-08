//! Document management — tracks open files and their analysis state.

use crate::analysis::{analyze, AnalysisResult};
use std::collections::HashMap;

/// An open document in the editor.
#[derive(Debug)]
pub struct Document {
    pub uri: String,
    pub version: i32,
    pub content: String,
    pub analysis: Option<AnalysisResult>,
}

impl Document {
    pub fn new(uri: String, version: i32, content: String) -> Self {
        Self {
            uri,
            version,
            content,
            analysis: None,
        }
    }

    /// Re-analyze the document content.
    pub fn analyze(&mut self) {
        self.analysis = Some(analyze(&self.content));
    }

    /// Apply a full content change.
    pub fn update(&mut self, version: i32, new_content: String) {
        self.version = version;
        self.content = new_content;
        self.analysis = None; // Invalidate.
    }

    /// Get or compute analysis.
    pub fn get_analysis(&mut self) -> &AnalysisResult {
        if self.analysis.is_none() {
            self.analyze();
        }
        self.analysis.as_ref().unwrap()
    }
}

/// Manages all open documents.
#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: HashMap<String, Document>,
}

impl DocumentStore {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
        }
    }

    /// Open a document.
    pub fn open(&mut self, uri: String, version: i32, content: String) {
        let mut doc = Document::new(uri.clone(), version, content);
        doc.analyze();
        self.documents.insert(uri, doc);
    }

    /// Update a document.
    pub fn update(&mut self, uri: &str, version: i32, content: String) {
        if let Some(doc) = self.documents.get_mut(uri) {
            doc.update(version, content);
            doc.analyze();
        }
    }

    /// Close a document.
    pub fn close(&mut self, uri: &str) {
        self.documents.remove(uri);
    }

    /// Get a document (immutable).
    pub fn get(&self, uri: &str) -> Option<&Document> {
        self.documents.get(uri)
    }

    /// Get a document (mutable).
    pub fn get_mut(&mut self, uri: &str) -> Option<&mut Document> {
        self.documents.get_mut(uri)
    }

    /// List all open document URIs.
    pub fn uris(&self) -> Vec<&str> {
        self.documents.keys().map(|s| s.as_str()).collect()
    }

    /// Number of open documents.
    pub fn len(&self) -> usize {
        self.documents.len()
    }

    pub fn is_empty(&self) -> bool {
        self.documents.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_document_new() {
        let doc = Document::new("file:///test.adam".into(), 1, "fn main() {}".into());
        assert_eq!(doc.uri, "file:///test.adam");
        assert_eq!(doc.version, 1);
        assert!(doc.analysis.is_none());
    }

    #[test]
    fn test_document_analyze() {
        let mut doc = Document::new("file:///test.adam".into(), 1, "fn main() {\n}\n".into());
        doc.analyze();
        assert!(doc.analysis.is_some());
    }

    #[test]
    fn test_document_update_invalidates() {
        let mut doc = Document::new("file:///test.adam".into(), 1, "fn main() {\n}\n".into());
        doc.analyze();
        assert!(doc.analysis.is_some());

        doc.update(2, "fn foo() {\n}\n".into());
        assert!(doc.analysis.is_none());
        assert_eq!(doc.version, 2);
    }

    #[test]
    fn test_document_get_analysis() {
        let mut doc = Document::new("file:///test.adam".into(), 1, "fn main() {\n}\n".into());
        let analysis = doc.get_analysis();
        assert!(analysis.ast.is_some());
    }

    #[test]
    fn test_store_open_close() {
        let mut store = DocumentStore::new();
        assert!(store.is_empty());

        store.open("file:///a.adam".into(), 1, "fn a() {}".into());
        assert_eq!(store.len(), 1);
        assert!(store.get("file:///a.adam").is_some());

        store.close("file:///a.adam");
        assert!(store.is_empty());
    }

    #[test]
    fn test_store_update() {
        let mut store = DocumentStore::new();
        store.open("file:///a.adam".into(), 1, "fn a() {}".into());

        store.update("file:///a.adam", 2, "fn b() {}".into());
        let doc = store.get("file:///a.adam").unwrap();
        assert_eq!(doc.version, 2);
        assert_eq!(doc.content, "fn b() {}");
    }

    #[test]
    fn test_store_uris() {
        let mut store = DocumentStore::new();
        store.open("file:///a.adam".into(), 1, "".into());
        store.open("file:///b.adam".into(), 1, "".into());

        let uris = store.uris();
        assert_eq!(uris.len(), 2);
    }

    #[test]
    fn test_store_multiple_documents() {
        let mut store = DocumentStore::new();
        store.open("file:///x.adam".into(), 1, "fn x() {}".into());
        store.open("file:///y.adam".into(), 1, "fn y() {}".into());

        assert_eq!(store.len(), 2);
        assert!(store.get("file:///x.adam").is_some());
        assert!(store.get("file:///y.adam").is_some());
        assert!(store.get("file:///z.adam").is_none());
    }
}
