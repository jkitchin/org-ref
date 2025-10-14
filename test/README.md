# org-ref Test Suite

This directory contains the test suite for org-ref.

## Test Files

All test files follow the naming convention `test-*.el`:

- **test-equation-image-tooltips.el** - Tests for equation image tooltip functionality
- **test-native-compilation-warnings.el** - Tests for native compilation warnings across all org-ref files
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
  -l test/test-equation-image-tooltips.el \
  -f ert-run-tests-batch-and-exit
```

## Writing New Tests

1. Create a new file following the naming convention: `test-<feature>.el`
2. Use the standard ERT test framework
3. Include proper `require` statements at the top
4. Add `(provide 'test-<feature>)` at the end
5. Tests will be automatically discovered and run by the test runner

Example structure:

```elisp
;;; test-my-feature.el --- Tests for my feature -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-ref)

(ert-deftest test-my-feature/basic ()
  "Test basic functionality."
  (should (equal 1 1)))

(provide 'test-my-feature)

;;; test-my-feature.el ends here
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
