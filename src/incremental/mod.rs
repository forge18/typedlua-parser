//! Incremental parsing infrastructure
//!
//! This module provides the foundation for incremental re-parsing by:
//! - Tracking which regions of the source text changed (`dirty.rs`)
//! - Caching parsed statements with hash validation (`cache.rs`)
//! - Adjusting span offsets after text edits (`offset.rs`)
//! - Token stream caching and merging (`tokens.rs`)
//! - Deep span adjustment for entire AST trees (`adjustment.rs`)
//! - State management during incremental parse (`state.rs`)
//!
//! Set `LUANEXT_DEBUG_INCREMENTAL=1` to see parse decisions on stderr.

pub mod adjustment;
pub mod cache;
pub mod dirty;
pub mod offset;
pub mod state;
pub mod tokens;

// Re-exports
pub use adjustment::is_statement_clean;
pub use cache::{get_statement_span, CachedStatement, IncrementalParseTree};
pub use dirty::{DirtyRegion, DirtyRegionSet, TextEdit};
pub use offset::adjust_span;
pub use state::IncrementalParseState;
pub use tokens::{
    adjust_token_offsets, expand_dirty_region, merge_token_streams, token_spans_boundary,
};

/// Check if debug logging is enabled for incremental parsing.
///
/// Uses `OnceLock` to cache the environment variable check so it's only
/// performed once per process lifetime.
#[inline]
pub fn debug_incremental_enabled() -> bool {
    use std::sync::OnceLock;
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("LUANEXT_DEBUG_INCREMENTAL").is_ok())
}

/// Log a debug message for incremental parsing decisions.
///
/// Only produces output when `LUANEXT_DEBUG_INCREMENTAL=1` is set.
/// Messages are written to stderr with an `[incremental]` prefix.
macro_rules! debug_incremental {
    ($($arg:tt)*) => {
        if $crate::incremental::debug_incremental_enabled() {
            eprintln!("[incremental] {}", format!($($arg)*));
        }
    };
}

pub(crate) use debug_incremental;
