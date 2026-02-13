//! Incremental parsing infrastructure
//!
//! This module provides the foundation for incremental re-parsing by:
//! - Tracking which regions of the source text changed (`dirty.rs`)
//! - Caching parsed statements with hash validation (`cache.rs`)
//! - Adjusting span offsets after text edits (`offset.rs`)
//! - Token stream caching and merging (`tokens.rs`)

pub mod cache;
pub mod dirty;
pub mod offset;
pub mod tokens;

// Re-exports
pub use cache::{CachedStatement, IncrementalParseTree};
pub use dirty::{DirtyRegion, DirtyRegionSet, TextEdit};
pub use offset::adjust_span;
pub use tokens::{adjust_token_offsets, expand_dirty_region, merge_token_streams, token_spans_boundary};
