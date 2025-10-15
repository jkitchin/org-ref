# org-ref Test Suite

This directory contains the test suite for org-ref.

## Test Files

All test files follow the naming convention `*-test.el`:

- **equation-image-tooltips-test.el** - Tests for equation image tooltip functionality
- **native-compilation-warnings-test.el** - Tests for native compilation warnings across all org-ref files
- **bibtex-completion-bibliography-normalization-test.el** - Tests for bibtex-completion-bibliography normalization (issue #1119)
- **all-org-test.el** - Tangled tests from org files
- **run-tests.el** - Master test runner that loads and executes all tests

## Running Tests

### Option 1: Using make (with Cask and ert-runner)

```bash
make test
```

This uses Cask and ert-runner to run all tests. Requires Cask to be installed.

### Option 2: Direct execution (no Cask required)

```bash
make test-direct
```

This runs tests directly using Emacs without requiring Cask or ert-runner. This is useful for:
- CI environments
- Local testing without Cask setup
- Quick test runs during development

### Option 3: Manual execution

```bash
emacs -batch -L . -L test -l test/run-tests.el
```

Or run individual test files:

```bash
emacs -batch -L . -L test \
  -l test/equation-image-tooltips-test.el \
  -f ert-run-tests-batch-and-exit
```

## Writing New Tests

1. Create a new file following the naming convention: `<feature>-test.el`
2. Use the standard ERT test framework
3. Include proper `require` statements at the top
4. Add `(provide '<feature>-test)` at the end
5. Tests will be automatically discovered and run by the test runner

Example structure:

```elisp
;;; my-feature-test.el --- Tests for my feature -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-ref)

(ert-deftest test-my-feature/basic ()
  "Test basic functionality."
  (should (equal 1 1)))

(provide 'my-feature-test)

;;; my-feature-test.el ends here
```

## Test Configuration

- **`.ert-runner`** - Configuration for ert-runner (loads `org-ref-test-init.el`)
- **`org-ref-test-init.el`** - Initialization code for tests (loads org-ref and dependencies)
- **`run-tests.el`** - Master test runner for direct execution

## Continuous Integration

The test suite is designed to work in CI environments without requiring Cask:

```bash
# In CI scripts
emacs -batch -L . -L test -l test/run-tests.el
```

Exit code will be 0 if all tests pass, non-zero otherwise.
