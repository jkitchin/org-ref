# Glossary and Acronym Test Examples

This directory contains example org-mode files for testing glossary and acronym functionality in org-ref.

## Files

### 1. Small Example (01-small-example.org)
Basic usage demonstrating:
- Glossary table with 3 terms
- Acronym table with 3 entries
- Various link types: gls, glspl, Gls
- Quick reference for understanding the feature

**Use for**: Understanding basic functionality, quick tooltip tests

### 2. Large Example (02-large-example.org)
Performance testing with:
- 50+ glossary terms
- 50+ acronyms
- 10 chapters with many references
- Lots of text content (~8000 lines)

**Use for**: Reproducing performance issues, stress testing tooltips

### 3. Multi-File Example (03-multifile-*.org)
Tests #+INCLUDE directive support:
- `03-multifile-main.org` - Main document with includes
- `03-multifile-glossary.org` - Glossary definitions
- `03-multifile-acronyms.org` - Acronym definitions
- `03-multifile-chapter1.org` - Content chapter 1
- `03-multifile-chapter2.org` - Content chapter 2

**Use for**: Testing multi-file references, include directive handling

## How to Test

### Basic Functionality

1. Open `01-small-example.org` in Emacs
2. Ensure org-ref is loaded
3. Hover mouse over any `gls:` or acronym link
4. Verify tooltip appears with correct content
5. Click on link to navigate to definition

### Performance Testing (Issue #1120)

1. Open `02-large-example.org` in Emacs
2. Try these operations that previously caused freezing:
   - Hover over multiple links quickly in succession
   - Scroll through document (mouse passes over many links)
   - Move mouse rapidly over the same link multiple times
   - Navigate with keyboard while mouse is over a link

**Expected behavior**: No freezing, tooltips appear quickly

**Bug reproduction** (before fix): Multi-second freezes when hovering

### Multi-File Testing

1. Open `03-multifile-main.org` in Emacs
2. Note that glossary/acronym definitions are in separate files
3. Hover over links in the main document
4. Open the chapter files individually
5. Hover over links in the chapter files

**Expected behavior**: Tooltips work in all files

## Current Issues (Issue #1120)

As of the last update, the tooltip performance issue was "fixed" but introduced
a new bug:

- ✓ No more freezing (performance fixed)
- ✗ Tooltips show "nil: ." instead of actual content

The fix should:
1. Restore correct tooltip content
2. Maintain the performance improvements
3. Add option to disable tooltips entirely

## Testing Checklist

When testing the fix:

- [ ] Small example tooltips show correct content
- [ ] Large example has no performance issues
- [ ] Large example tooltips are accurate
- [ ] Multi-file tooltips work in main document
- [ ] Multi-file tooltips work in included files
- [ ] Can disable tooltips via defcustom
- [ ] Navigation (clicking links) still works
- [ ] Font-lock/highlighting works correctly
- [ ] No console errors or warnings

## Notes

### Glossary Link Types

- `gls:label` - Basic glossary reference
- `glspl:label` - Plural form
- `Gls:label` - Capitalized form
- `Glspl:label` - Capitalized plural

### Acronym Link Types

Similar to glossary, using acronym keys defined in the acronyms table.

### LaTeX Export

These files include LaTeX headers for the glossaries package. To export to PDF:

```bash
# In Emacs
C-c C-e l p  # Export to LaTeX and compile to PDF
```

The `[[printglossaries:]]` link will generate the glossary/acronym list in the PDF output.
