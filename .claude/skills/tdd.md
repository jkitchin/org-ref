# Test-Driven Development Workflow for GitHub Issues

You are helping implement a GitHub issue using strict test-driven development (TDD) methodology.

## Workflow Overview

1. **Issue Analysis** - Study the issue and ask clarifying questions
2. **Planning** - Create a detailed implementation plan
3. **Test First** - Write failing tests that demonstrate the problem
4. **Implementation** - Implement the solution incrementally
5. **Commit Strategy** - Only commit when tests pass
6. **Merge** - Wait for user approval before merging to master

## Phase 1: Issue Analysis & Planning

### Step 1: Fetch and Study the Issue

When the user provides an issue number:

1. Fetch the issue details using: `gh issue view <number>`
2. Read any linked files, documentation, or related code
3. Understand the problem thoroughly

### Step 2: Ask Clarifying Questions

Before creating a plan, ask the user questions about:
- **Scope**: What exactly should be included/excluded?
- **Behavior**: What is the expected behavior vs current behavior?
- **Edge cases**: What edge cases should be handled?
- **Compatibility**: Any backward compatibility concerns?
- **Performance**: Are there performance requirements?
- **API design**: If adding new functions, what should the interface look like?

Use the AskUserQuestion tool to present multiple-choice or specific questions.

### Step 3: Create Implementation Plan

After clarifications, present a detailed plan including:
- **Changes to be made**: Specific files and functions
- **Test strategy**: What tests will be written and what they'll verify
- **Implementation order**: Logical sequence of changes
- **Risk assessment**: Potential issues or breaking changes

Ask the user to approve the plan before proceeding.

## Phase 2: Create Feature Branch

Once the plan is approved:

1. Create a descriptive feature branch:
   ```bash
   git checkout -b feature/issue-<number>-<short-description>
   # OR
   git checkout -b fix/issue-<number>-<short-description>
   ```

2. Confirm branch creation with the user

## Phase 3: Write Failing Tests (Test First!)

**CRITICAL**: Write tests BEFORE implementation.

### Test File Creation

1. Determine appropriate test file:
   - New feature → create new test file `test/<feature>-test.el`
   - Existing feature → add to existing test file

2. Write tests that:
   - **Demonstrate the problem** (should fail with current code)
   - **Define success criteria** (will pass when implementation is correct)
   - **Cover edge cases** mentioned in the plan
   - **Are independent** (can run in any order)

### Test Naming

Follow ERT naming conventions:
```elisp
(ert-deftest test-<feature>-<specific-behavior> ()
  "Test that <feature> does <specific-behavior>."
  ...)
```

### Run Tests to Confirm Failure

After writing tests, run them to verify they fail:
```bash
emacs --batch -L . -l test/<test-file>.el -f ert-run-tests-batch-and-exit
```

**Show the user**:
- How many tests were written
- What each test verifies
- That tests currently fail (expected)

Ask user to confirm tests look correct before implementation.

## Phase 4: Incremental Implementation

Use the TodoWrite tool to track implementation tasks.

### Implementation Strategy

1. **Implement smallest unit first**: Start with the simplest test
2. **Run tests frequently**: After each logical change
3. **Commit when tests pass**: Each commit should have passing tests
4. **Keep commits atomic**: One logical change per commit

### Commit Rules

**ONLY commit when**:
- At least one test that was failing now passes
- No previously passing tests are broken
- Code compiles without errors

**Commit message format**:
```
<type>: <short description>

<detailed description of what changed and why>

- Test(s) that now pass: test-<name>, test-<name>
- Related to #<issue-number>
```

### Running Tests

Before each commit attempt:
```bash
# Run specific test file
emacs --batch -L . -l test/<test-file>.el -f ert-run-tests-batch-and-exit

# Run all tests (to ensure no regressions)
make test-direct
# OR
make mytest
```

### Progress Updates

After each commit:
- Update the TodoWrite list to mark tasks completed
- Show the user which tests now pass
- Explain what was implemented

## Phase 5: Completion Verification

When all tests pass:

1. **Run full test suite**:
   ```bash
   make test-direct
   ```

2. **Check for compilation warnings**:
   ```bash
   rm -f *.elc
   emacs --batch --eval "(byte-compile-file \"<modified-file>.el\")"
   ```

3. **Verify the issue is resolved**:
   - Review original issue requirements
   - Confirm all acceptance criteria are met
   - Test interactively if needed

4. **Create summary** for the user:
   - What was implemented
   - How many tests were added
   - All tests passing (X/X)
   - Files modified
   - Ready for review

## Phase 6: Merge Process

**IMPORTANT**: Do NOT merge to master automatically.

Wait for explicit user approval:
- User will review the changes
- User will test interactively if desired
- User will say "merge" or "merge and push"

Only then:
```bash
git checkout master
git merge feature/issue-<number>-<description> --no-edit
```

If user says "push":
```bash
git push origin master
```

## Best Practices for This Workflow

### Test Quality
- **Specific**: Each test should verify one specific behavior
- **Independent**: Tests should not depend on each other
- **Repeatable**: Same result every time
- **Clear failure messages**: Use descriptive assertions

### Implementation Quality
- **Minimal changes**: Only change what's needed to make tests pass
- **No premature optimization**: Get it working first
- **Follow existing patterns**: Match the codebase style
- **Document non-obvious code**: Add comments for complex logic

### Communication
- **Show your work**: Display test output, compilation results
- **Explain decisions**: Why you chose a particular approach
- **Ask when uncertain**: Better to ask than assume
- **Keep user informed**: Regular progress updates

## Example Workflow

```
User: "Work on issue #1234"
You: [Fetch issue] "I'll fetch issue #1234 and study it."
     [Run gh issue view 1234]
     [Read related code files]

You: [Ask questions using AskUserQuestion tool]
     "I have a few clarifying questions about the implementation..."

User: [Answers questions]

You: [Present plan]
     "Here's my implementation plan:
     1. Add function X to file Y
     2. Modify function Z to handle case A
     3. Tests will verify: ..."
     "Does this plan look good?"

User: "Yes, proceed"

You: [Create branch]
     git checkout -b feature/issue-1234-add-feature-x
     "Created feature branch. Now writing tests..."

You: [Write tests]
     "Created test/feature-x-test.el with 3 tests:
     - test-feature-x-basic-case
     - test-feature-x-edge-case
     - test-feature-x-error-handling"
     [Run tests - show they fail]
     "All 3 tests fail as expected (no implementation yet)."
     "Do these tests look correct?"

User: "Yes"

You: [Implement & commit cycle]
     TodoWrite: [Create implementation tasks]
     
     [Implement first part]
     [Run tests - 1 passes now]
     [Commit] "feat: Add basic feature X implementation
              - Test that now passes: test-feature-x-basic-case
              - Related to #1234"
     
     [Implement second part]
     [Run tests - 2 pass now]
     [Commit] "feat: Add edge case handling for feature X
              - Test that now passes: test-feature-x-edge-case
              - Related to #1234"
     
     [Implement final part]
     [Run all tests - all 3 pass]
     [Commit] "feat: Add error handling for feature X
              - Test that now passes: test-feature-x-error-handling
              - Related to #1234
              
              All tests now passing. Issue #1234 resolved."

You: [Final verification]
     "All tests passing (3/3)
      Full test suite passing (87/87)
      No compilation warnings
      Issue #1234 is resolved.
      Ready for merge when you approve."

User: "merge and push"

You: [Merge and push]
     git checkout master
     git merge feature/issue-1234-add-feature-x --no-edit
     git push origin master
     "Merged to master and pushed. Issue #1234 complete!"
```

## Troubleshooting

### Tests Keep Failing
- Review test logic carefully
- Check if the test expectations are correct
- Verify test setup/teardown
- Run tests individually to isolate issues

### Merge Conflicts
- Fetch latest from master before starting
- If conflicts occur, ask user how to resolve
- Never force-push or resolve conflicts without approval

### Performance Issues
- If tests are slow, discuss optimization with user
- Don't optimize prematurely - make it work first
- Add performance benchmarks if needed

## Remember

- **Tests first, code second** - Always!
- **Commit only when tests pass** - No exceptions
- **Keep user in the loop** - Frequent updates
- **Ask before merging** - User controls when to merge
- **Document your work** - Clear commit messages

When you're ready to start, I'll guide you through this TDD process step by step!
