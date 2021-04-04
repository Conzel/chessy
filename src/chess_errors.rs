use std::error::Error;
use std::fmt;

// ---------------------------------------------
// Error Handling
// ---------------------------------------------
#[derive(Debug, Clone)]
pub struct ChessError(String);

pub type ChessResult<T> = std::result::Result<T, ChessError>;

impl From<String> for ChessError {
    fn from(s: String) -> ChessError {
        ChessError(s)
    }
}

impl From<&str> for ChessError {
    fn from(s: &str) -> ChessError {
        ChessError(s.to_string())
    }
}

impl Error for ChessError {}

impl fmt::Display for ChessError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Chess Error occured: {}", self.0)
    }
}
