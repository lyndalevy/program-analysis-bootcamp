# Analysis Report: Calculator Bugs

## Student Name: Analysis Completed
## Date: 2026-03-12

---

## Part 1: Static Analysis Findings (ESLint)

Run `npx eslint calculator.js` and record all findings below.

| # | Line | Rule | Description | Severity |
|---|------|------|-------------|----------|
| 1 | 15 | no-unused-vars | 'b' is defined but never used | warning |
| 2 | 16 | no-undef | 'reslt' is not defined | error |
| 3 | 22 | no-unreachable | Unreachable code | error |
| 4 | 31 | no-fallthrough | Expected a 'break' statement before 'case' | error |
| 5 | 62 | eqeqeq | Expected '===' and instead saw '==' (2 instances) | warning |
| 6 | 70 | no-unused-vars | 'temp' is assigned a value but never used | warning |
| 7 | 80 | no-constant-condition | Unexpected constant condition | error |

**Total static analysis issues found:** 8 (4 errors, 4 warnings)

---

## Part 2: Dynamic Analysis Findings (Test Suite)

Run `node test-calculator.js` and record all test failures below.

| # | Test Name | Error Message | Root Cause |
|---|-----------|---------------|------------|
| 1 | add(2, 3) should be 5 | reslt is not defined | Bug in add() function - typo 'reslt' instead of 'b' |
| 2 | add(-1, 1) should be 0 | reslt is not defined | Bug in add() function - typo 'reslt' instead of 'b' |
| 3 | divide(10, 0) should throw or return Infinity gracefully | Division by zero should be handled, got Infinity | No validation for divisor === 0 |
| 4 | calculate('add', 10, 5) should be 15 | reslt is not defined | Calls add() which has the 'reslt' bug |
| 5 | factorial(-1) should handle negative input | Infinite recursion detected | Missing base case for negative numbers |
| 6 | absolute(5) should be 5 | expected 5, got -5 | Constant condition 'if (true)' always negates |

**Total dynamic analysis issues found:** 6

---

## Part 3: Comparison

### Which bugs did ONLY static analysis catch?
<!-- List bugs found by ESLint but NOT by running tests -->

1. **Unreachable code** (line 22 in subtract): `console.log` after return statement
2. **Unused variable** (line 70 in power): `temp` is assigned but never used
3. **Type coercion warning** (line 62 in multiply): Using `==` instead of `===` (tests didn't fail because JavaScript coercion worked as "expected")

### Which bugs did ONLY dynamic analysis catch?
<!-- List bugs found by tests but NOT by ESLint -->

1. **Division by zero** (line 48 in divide): No validation for `b === 0`, returns `Infinity` at runtime
2. **Infinite recursion** (line 52-57 in factorial): Missing base case for negative numbers causes stack overflow

### Which bugs were found by BOTH approaches?
<!-- List bugs caught by both ESLint and test failures -->

1. **Undefined variable 'reslt'** (line 16 in add): ESLint flagged as `no-undef` error; tests failed with "reslt is not defined"
2. **Switch fallthrough** (line 31 in calculate): ESLint flagged as `no-fallthrough` error; would cause incorrect results in calculate('add')
3. **Constant condition** (line 80 in absolute): ESLint flagged as `no-constant-condition`; test showed absolute(5) returned -5 instead of 5

---

## Part 4: Reflection

### Why can't static analysis catch all bugs?
<!-- Your answer (2-3 sentences) -->

Static analysis examines code without executing it, so it cannot detect bugs that depend on runtime behavior or specific input values. For example, division by zero and infinite recursion with negative inputs require actual execution to manifest. Additionally, static analysis has limited ability to reason about complex runtime states, data flows from external sources, or the semantic correctness of algorithms.


### Why can't dynamic analysis catch all bugs?
<!-- Your answer (2-3 sentences) -->

Dynamic analysis only detects bugs in the code paths that are actually executed during testing, so it can miss bugs in unexercised code branches. Issues like unreachable code or unused variables don't cause test failures but represent code quality problems. Additionally, dynamic analysis requires comprehensive test coverage to be effective, and achieving complete coverage of all possible inputs and execution paths is often impractical or impossible.


### When would you prioritize one approach over the other?
<!-- Your answer (2-3 sentences) -->

Static analysis should be prioritized early in development and in continuous integration pipelines because it's fast, catches structural issues without execution, and can prevent entire classes of bugs before code review. Dynamic analysis should be prioritized when verifying correctness with real inputs, testing edge cases and error conditions, or when investigating performance issues and runtime behavior. In practice, both approaches are complementary and should be used together: static analysis for quick feedback and structural correctness, dynamic analysis for functional validation and real-world behavior.

