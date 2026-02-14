# Incremental Parsing Design Document

## Overview

The LuaNext parser supports **statement-level incremental parsing**: when a document is edited, only the statements whose byte ranges overlap the edited regions are re-parsed. Unmodified statements are reused from the previous parse tree. This is gated behind the `incremental-parsing` feature flag (enabled by default).

## Design Decisions

### Why statement-level granularity?

LuaNext (like Lua) is a statement-oriented language — each top-level construct (variable declaration, function, class, if/while, etc.) is a self-contained unit. This makes statements the natural caching boundary:

- **Simple invalidation**: A statement is dirty if its byte range overlaps any edit. No need for complex tree-diffing.
- **Cheap validation**: One overlap check per statement per edit, using half-open interval arithmetic.
- **Good hit rate**: Typical edits (single line changes) affect 1-2 out of dozens of statements.

Finer granularity (expression-level) would add complexity without proportional benefit, since most edits change at least one full statement.

### Why FxHasher?

Source text hashing uses `FxHasher` from `rustc-hash` instead of `DefaultHasher`:

- **2-4x faster** for string hashing (no SipHash overhead)
- **Already a dependency** — used elsewhere in the parser
- **No security requirement** — we're hashing source text for cache validation, not for hash tables exposed to adversarial input

### Why `Rc<Bump>` instead of `Arc<Bump>`?

`bumpalo::Bump` is not `Sync`, so `Arc<Bump>` triggers clippy's `arc_with_non_send_sync` lint. Since incremental parsing is single-threaded (parser is `!Send`), `Rc<Bump>` is the correct choice — it avoids atomic overhead and correctly models the ownership semantics.

## Arena Handling

### Multi-arena strategy

Each incremental parse may create a new arena for re-parsed statements, while clean statements remain in their original arenas. This avoids copying clean statements on every edit.

```text
Version 1: [Arena 0] ← all statements from initial full parse
Version 2: [Arena 0, Arena 1] ← clean stmts in Arena 0, re-parsed in Arena 1
Version 3: [Arena 0, Arena 1, Arena 2] ← another re-parse adds Arena 2
Version 4: [Arena 3] ← consolidation: all cloned to fresh arena
```

### Consolidation triggers

Arenas are consolidated (all statements cloned to a single fresh arena) when:

1. **More than 3 arenas** — prevents unbounded growth from many small edits
2. **Every 10 versions** — periodic compaction even if arena count is low

After consolidation, unreferenced arenas are dropped, freeing their memory.

### Garbage collection

`collect_garbage()` runs after every incremental parse:

- If consolidation is not triggered, it drops arenas that no statement references (via generation tracking)
- If consolidation is triggered, it clones all statements to a new arena and drops all old arenas

### Unsafe lifetime transmute

Cached statements use `unsafe transmute` to cast arena lifetimes (`'static` <-> `'arena`). This is safe because:

1. Arenas are kept alive via `Rc<Bump>` in the `IncrementalParseTree`
2. Lifetimes are compile-time only — erased at runtime
3. `Statement` layout is identical regardless of lifetime parameter
4. This is the same pattern used in `module_phase.rs` for `ModuleRegistry`

## Performance Characteristics

### When incremental is faster

- **Single-line edits** in files with many statements: 2.7-3.3x speedup
- **No-edit cache hits** (e.g., focus change without modification): ~10x speedup
- **Append-only edits** at file end: only new statement is parsed

### When full reparse is better

- **All statements dirty** (e.g., formatter reformats entire file): incremental falls back to full parse automatically
- **Very small files** (< 5 statements): overhead of dirty detection exceeds savings
- **Edits spanning most of the file**: again triggers full-parse fallback

The `ParseStrategyAnalyzer` in the LSP applies heuristics before calling `parse_incremental()` to choose the best strategy.

## Troubleshooting

### Environment variables

| Variable | Effect |
|---|---|
| `LUANEXT_DEBUG_INCREMENTAL=1` | Print parse decisions to stderr |
| `LUANEXT_DISABLE_INCREMENTAL=1` | Force full reparse always (via heuristics config) |
| `LUANEXT_LSP_PARSE_STATS=1` | Print parse timing statistics in LSP |
| `LUANEXT_MAX_EDIT_SIZE=N` | Max edit size in bytes before forcing full parse |
| `LUANEXT_MAX_DIRTY_REGIONS=N` | Max dirty regions before forcing full parse |
| `LUANEXT_MAX_AFFECTED_RATIO=F` | Max affected ratio (0.0-1.0) before forcing full parse |

### Feature flag

To disable incremental parsing at compile time:

```toml
# In Cargo.toml
luanext-parser = { path = "...", default-features = false, features = ["typed"] }
```

### Common issues

**"All statements dirty" on every edit**: Check that edit ranges are in original source coordinates, not post-edit coordinates. The LSP adapter converts LSP positions to byte offsets before passing to `parse_incremental()`.

**Memory growth**: If arenas accumulate beyond 3, consolidation should trigger automatically. Check that `collect_garbage()` is being called (it runs at the end of `parse_incremental()`). Set `LUANEXT_DEBUG_INCREMENTAL=1` to verify.

## File Map

| File | Description |
|---|---|
| `src/incremental/mod.rs` | Module exports, `debug_incremental!` macro, `debug_incremental_enabled()` |
| `src/incremental/cache.rs` | `IncrementalParseTree`, `CachedStatement`, arena GC, `clone_statement_to_arena()` |
| `src/incremental/dirty.rs` | `TextEdit`, `DirtyRegion`, `DirtyRegionSet` with binary search |
| `src/incremental/adjustment.rs` | `is_statement_clean()` overlap detection |
| `src/incremental/offset.rs` | `adjust_span()` for relocating spans after edits |
| `src/incremental/state.rs` | `IncrementalParseState` helper for LSP integration |
| `src/parser/mod.rs` | `parse_incremental()` — main entry point (cfg-gated) |
| `tests/incremental_parsing_tests.rs` | Integration tests for incremental parse paths |
