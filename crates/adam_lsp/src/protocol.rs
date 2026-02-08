//! JSON-RPC 2.0 protocol handler for LSP communication over stdin/stdout.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::io::{self, BufRead, Write};

/// A JSON-RPC request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub jsonrpc: String,
    pub id: Option<Value>,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

/// A JSON-RPC response.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub jsonrpc: String,
    pub id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError>,
}

/// A JSON-RPC error.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResponseError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

/// A JSON-RPC notification (no id).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notification {
    pub jsonrpc: String,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

impl Response {
    pub fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn error(id: Value, code: i32, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            id,
            result: None,
            error: Some(ResponseError {
                code,
                message: message.into(),
                data: None,
            }),
        }
    }
}

impl Notification {
    pub fn new(method: impl Into<String>, params: Value) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            method: method.into(),
            params,
        }
    }
}

/// LSP message framing: read a message from stdin.
///
/// LSP uses the format:
/// ```text
/// Content-Length: <length>\r\n
/// \r\n
/// <JSON payload>
/// ```
pub fn read_message(reader: &mut impl BufRead) -> io::Result<Option<String>> {
    let mut content_length: Option<usize> = None;

    // Read headers.
    loop {
        let mut header = String::new();
        let bytes_read = reader.read_line(&mut header)?;
        if bytes_read == 0 {
            return Ok(None); // EOF.
        }

        let header = header.trim();
        if header.is_empty() {
            break; // End of headers.
        }

        if let Some(len_str) = header.strip_prefix("Content-Length: ") {
            content_length = len_str.parse().ok();
        }
        // Ignore other headers (Content-Type, etc.).
    }

    let length = content_length.ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidData, "missing Content-Length header")
    })?;

    // Read body.
    let mut body = vec![0u8; length];
    reader.read_exact(&mut body)?;

    String::from_utf8(body)
        .map(Some)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Write an LSP message to stdout.
pub fn write_message(writer: &mut impl Write, content: &str) -> io::Result<()> {
    write!(
        writer,
        "Content-Length: {}\r\n\r\n{}",
        content.len(),
        content
    )?;
    writer.flush()
}

/// Parse a JSON-RPC message.
pub fn parse_message(json: &str) -> Result<Request, String> {
    serde_json::from_str(json).map_err(|e| format!("failed to parse JSON-RPC message: {}", e))
}

/// Serialize a response to JSON.
pub fn serialize_response(response: &Response) -> String {
    serde_json::to_string(response).unwrap_or_else(|_| "{}".into())
}

/// Serialize a notification to JSON.
pub fn serialize_notification(notification: &Notification) -> String {
    serde_json::to_string(notification).unwrap_or_else(|_| "{}".into())
}

// ---------------------------------------------------------------------------
// Server capabilities
// ---------------------------------------------------------------------------

/// Build the InitializeResult capabilities.
pub fn server_capabilities() -> Value {
    serde_json::json!({
        "capabilities": {
            "textDocumentSync": {
                "openClose": true,
                "change": 1,
                "save": { "includeText": true }
            },
            "completionProvider": {
                "triggerCharacters": [".", ":"],
                "resolveProvider": false
            },
            "hoverProvider": true,
            "definitionProvider": true,
            "documentSymbolProvider": true,
            "diagnosticProvider": {
                "interFileDependencies": false,
                "workspaceDiagnostics": false
            }
        },
        "serverInfo": {
            "name": "adam-lsp",
            "version": "0.1.0"
        }
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_response_success() {
        let resp = Response::success(serde_json::json!(1), serde_json::json!({"result": true}));
        assert_eq!(resp.jsonrpc, "2.0");
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }

    #[test]
    fn test_response_error() {
        let resp = Response::error(serde_json::json!(1), -32600, "invalid request");
        assert!(resp.error.is_some());
        assert!(resp.result.is_none());
        assert_eq!(resp.error.unwrap().code, -32600);
    }

    #[test]
    fn test_notification_new() {
        let notif = Notification::new("textDocument/publishDiagnostics", serde_json::json!({}));
        assert_eq!(notif.method, "textDocument/publishDiagnostics");
    }

    #[test]
    fn test_read_message() {
        let input = "Content-Length: 13\r\n\r\n{\"test\":true}";
        let mut reader = io::BufReader::new(input.as_bytes());
        let msg = read_message(&mut reader).unwrap();
        assert_eq!(msg, Some("{\"test\":true}".into()));
    }

    #[test]
    fn test_read_message_eof() {
        let input = "";
        let mut reader = io::BufReader::new(input.as_bytes());
        let msg = read_message(&mut reader).unwrap();
        assert_eq!(msg, None);
    }

    #[test]
    fn test_write_message() {
        let mut output = Vec::new();
        write_message(&mut output, "{\"test\":true}").unwrap();
        let written = String::from_utf8(output).unwrap();
        assert!(written.starts_with("Content-Length: 13\r\n\r\n"));
        assert!(written.contains("{\"test\":true}"));
    }

    #[test]
    fn test_parse_message() {
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}"#;
        let req = parse_message(json).unwrap();
        assert_eq!(req.method, "initialize");
        assert_eq!(req.id, Some(serde_json::json!(1)));
    }

    #[test]
    fn test_parse_message_notification() {
        let json = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;
        let req = parse_message(json).unwrap();
        assert_eq!(req.method, "initialized");
        assert!(req.id.is_none());
    }

    #[test]
    fn test_serialize_response() {
        let resp = Response::success(serde_json::json!(1), serde_json::json!(null));
        let json = serialize_response(&resp);
        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"id\":1"));
    }

    #[test]
    fn test_server_capabilities() {
        let caps = server_capabilities();
        assert!(caps.get("capabilities").is_some());
        assert!(caps.get("serverInfo").is_some());
    }

    #[test]
    fn test_roundtrip_message() {
        let content = r#"{"jsonrpc":"2.0","id":42,"result":null}"#;
        let mut buf = Vec::new();
        write_message(&mut buf, content).unwrap();

        let mut reader = io::BufReader::new(buf.as_slice());
        let msg = read_message(&mut reader).unwrap().unwrap();
        assert_eq!(msg, content);
    }
}
