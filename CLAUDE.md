# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**org-ref** is an Emacs package that provides citations, cross-references, indexes, glossaries, and bibtex utilities for org-mode. It makes academic writing in org-mode convenient by providing hyper-functional links that are clickable, exportable to LaTeX/PDF, and work with completion systems like ivy and helm.

The package is available on MELPA and has ~35 Emacs Lisp modules covering different aspects of functionality.

## Core Architecture

### Module Structure

org-ref follows a modular architecture with clear separation of concerns:

- **org-ref-core.el**: Central hub that requires and orchestrates all major modules. Entry point for most functionality.

- **Link Types** (each provides specific org-mode link functionality):
  - `org-ref-citation-links.el`: Citation links (cite, citeauthor, etc.)
  - `org-ref-ref-links.el`: Cross-reference links (ref, eqref, pageref, etc.)
  - `org-ref-label-link.el`: Label links for marking reference targets
  - `org-ref-bibliography-links.el`: Bibliography and bibliographystyle links
  - `org-ref-misc-links.el`: Miscellaneous utility links

- **Export System**:
  - `org-ref-export.el`: Handles export to various formats (LaTeX, HTML, etc.)
  - `org-ref-natbib-bbl-citeproc.el`: CSL/citeproc export backend

- **UI Backends** (user chooses one):
  - `org-ref-ivy.el`: Ivy/counsel completion interface
  - `org-ref-helm.el`: Helm completion interface
  - Default: vanilla completing-read

- **Bibtex Utilities**:
  - `org-ref-bibtex.el`: BibTeX manipulation and validation
  - Uses `bibtex-completion` library (external dependency) for bibliography management

- **External Integrations**:
  - `openalex.el`: OpenAlex API integration for bibliographic data
  - `org-ref-arxiv.el`: arXiv paper lookups
  - `org-ref-pubmed.el`: PubMed integration
  - `org-ref-isbn.el`: ISBN-based book metadata
  - `org-ref-wos.el`: Web of Science integration

### Key Design Patterns

1. **Customizable Functions**: org-ref uses function variables (`org-ref-insert-cite-function`, `org-ref-insert-ref-function`) that users can customize to use different backends (vanilla/ivy/helm).

2. **Link Activation**: Each link type registers itself with org-mode's link system and provides:
   - Click actions (follow behavior)
   - Export functions (how to render in different formats)
   - Font-lock/activation (visual appearance)
   - Help-echo (tooltips)

3. **Caching Strategy**:
   - Labels are cached per-buffer using `buffer-chars-modified-tick` for invalidation
   - Multi-file projects use global hash tables with timestamp-based change detection
   - Performance-critical: label scanning is used in font-lock

4. **Multi-File References** (Issue #1021, implemented):
   - Files included via `#+INCLUDE` directives share a global label cache
   - O(1) timestamp checking (`file-attributes`) detects file changes
   - Only changed files are re-scanned (incremental updates)
   - Feature enabled by default: `org-ref-enable-multi-file-references`

## Testing

### Running Tests

```bash
# Full test suite (requires Cask and ert-runner)
make test

# Direct test run (no Cask dependency)
make test-direct

# Run specific test file
emacs --batch -L . -l test/specific-test.el -f ert-run-tests-batch-and-exit

# Run tests matching pattern
emacs --batch -L . -l test/specific-test.el --eval "(ert-run-tests-batch-and-exit 'pattern)"

# All tests via mytest target (loads test setup)
make mytest
```

### Test Structure

- Test files follow pattern: `test/*-test.el`
- Test runner: `test/run-tests.el` (auto-discovers and loads all test files)
- Test initialization: `test/init.el` and `test/org-test-setup.el`
- Multi-file reference tests: `test/multi-file-refs-test.el` with test documents in `test/multi-file-refs/`

### CI/CD

GitHub Actions runs tests on push via `.github/workflows/test-org-ref.yml`:
- Tests on Emacs 28.2 and snapshot
- Uses `make github-actions` which installs Cask and runs ert-runner

## Development Commands

### Compilation

```bash
# Compile all .el files
make compile

# Byte-compile single file
emacs --batch --eval "(byte-compile-file \"filename.el\")"

# Clean compiled files
make clean
```

### Packaging

```bash
# Create MELPA-compatible package
make package
```

### Interactive Development

```bash
# Launch Emacs with org-ref loaded (ivy backend)
make ivy

# Launch with development settings
make devel

# Launch vanilla (no user config)
make vanilla
```

## Working with Cross-References (org-ref-ref-links.el)

This is one of the most complex modules due to performance requirements and multi-file support.

### Label Collection System

Labels are collected from multiple sources via regex patterns defined in `org-ref-ref-label-regexps`:
- `#+NAME:` keywords
- `:CUSTOM_ID:` properties
- `\label{}` LaTeX commands
- `<<target>>` org targets
- `label:` links

**Core scanning function**: `org-ref-scan-buffer-for-labels()`
- Returns list of `(label . context)` cons cells
- Extracts context based on org-element type (equation, table, paragraph, etc.)

### Single-file vs Multi-file

- `org-ref-get-labels-single-file()`: Original behavior, buffer-local cache
- `org-ref-get-labels-multi-file()`: Scans current file + all `#+INCLUDE`d files
- `org-ref-get-labels()`: Dispatcher based on `org-ref-enable-multi-file-references`

### Navigation

`org-ref-ref-jump-to()` handles clicking on ref links:
1. First searches current buffer
2. If multi-file enabled and not found, searches included files
3. Opens target file and positions cursor at label
4. Pushes to org-mark-ring (return with `C-c &`)

Helper functions:
- `org-ref-find-label-in-buffer()`: Search in current buffer
- `org-ref-find-label-in-file()`: Search in external file (opens with `find-file-noselect`)

## Working with Citations (org-ref-citation-links.el)

Citation links use `bibtex-completion` library for:
- Bibliography file parsing
- PDF management
- Notes management
- Completion candidates

The module defines multiple citation link types (cite, citeauthor, citeyear, etc.) that map to different LaTeX citation commands.

## Bibtex Configuration

org-ref v3 migrated from `org-ref-*` variables to `bibtex-completion-*` variables:
- `bibtex-completion-bibliography`: List of .bib files
- `bibtex-completion-library-path`: PDF storage location
- `bibtex-completion-notes-path`: Notes directory

This centralization means one configuration works for org-ref, helm-bibtex, and ivy-bibtex.

## OpenAlex Integration (openalex.el)

The OpenAlex integration provides bibliographic data lookup:

**Key functions**:
- `oa-author`: Get author information by OpenAlex ID
- `oa-work`: Get work/publication information
- Various `oa-` functions for searching authors, works, institutions

**API Key Handling**:
- Only sends `api_key` when `oa-api-key` is set and non-empty
- Uses `oa--params` helper to build parameter alists
- Autocomplete endpoints specifically exclude API key

**Template System**: Uses plists to extract data and populate org-mode buffers

## Common Pitfalls

### Performance Issues

- **Label scanning in font-lock**: `org-ref-get-labels()` is called frequently for syntax highlighting. Changes must be fast (< 50ms).
- **Multi-file overhead**: Always check for `#+INCLUDE` presence before expensive operations
- **Buffer-local vs global caching**: Know which cache you're using (buffer-local for single file, global hash tables for multi-file)

### Testing Labels and References

When testing cross-reference functionality:
1. Clear caches: `(clrhash org-ref-project-label-cache)`
2. Reset buffer-local tick: `(setq org-ref-buffer-chars-modified-tick nil)`
3. Use test documents in `test/multi-file-refs/` as examples

### Lexical Binding

All new .el files must have: `;;; filename.el --- description -*- lexical-binding: t; -*-`

Native compilation warnings are tracked in `test/native-compilation-warnings-test.el`

## File Naming Conventions

- Main modules: `org-ref-*.el`
- Test files: `test/*-test.el`
- Test setup: `test/init.el`, `test/org-test-setup.el`
- Implementation docs: `IMPLEMENTATION-SUMMARY-*.org`, `implementation-plan-*.org`

## Issue Tracking

When implementing features from GitHub issues:
- Create feature branches: `feature/descriptive-name`
- Reference issue in commit: `Fixes #NNNN`
- Add comprehensive tests before merging
- Update relevant IMPLEMENTATION-SUMMARY files if applicable

## Export Considerations

org-ref supports multiple export backends:
- **LaTeX**: Native support, primary target
- **HTML**: Via citeproc-el
- **Markdown**: Via pandoc and citeproc-el
- **ODT**: Via pandoc

Export functions are defined per-link-type in the link definition. Check `org-ref-export.el` for the orchestration logic.

## Local Skills

### TDD Skill (Test-Driven Development)

Invoke with: `tdd` skill

This skill provides a structured workflow for implementing GitHub issues using test-driven development:

1. **Analyzes the issue** and asks clarifying questions
2. **Creates an implementation plan** for approval
3. **Creates a feature branch** (feature/issue-N-description or fix/issue-N-description)
4. **Writes failing tests first** that demonstrate the problem
5. **Implements incrementally** and only commits when tests pass
6. **Waits for approval** before merging to master

The skill enforces strict TDD discipline: tests are always written before implementation, and commits only happen when at least one previously failing test passes.
