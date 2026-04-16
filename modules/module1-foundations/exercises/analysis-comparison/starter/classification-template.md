# Analysis Classification Exercise

## Instructions
For each code snippet in `code-samples.md`, fill in the table below.

**Objective categories:** Correctness, Security, Performance
**Detection method:** Static, Dynamic, Both

---

| Snippet | Issue Description | Objective | Detection Method | Explanation |
|---------|-------------------|-----------|-----------------|-------------|
| 1 | SQL injection via string concatenation | Security | Static | Concatenating `user_id` into SQL allows injection; detectable by analyzing string operations. |
| 2 | Unreachable code after `return` | Correctness | Static | `console.log` is never executed; a simple control‑flow issue. |
| 3 | Division by zero if list empty | Correctness | Both | Static tools can flag missing length check; runtime fails when `len(numbers)==0`. |
| 4 | No null terminator copied | Correctness | Static | Loop copies characters but not terminating `\0`, leading to buffer overflow. |
| 5 | Off‑by‑one indexing past end | Correctness | Static | Loop uses `<=` causing access to `items[items.length]`; index error. |
| 6 | Exponential recursion (slow) | Performance | Both | Obvious from recursion pattern; dynamic profiling would show blow‑up. |
| 7 | File handle never closed (resource leak) | Performance | Static | Lack of `close()` can lead to resource exhaustion; static analyzer spots. |
| 8 | Command injection via `os.system` | Security | Static | Concatenating `user_input` into shell command allows arbitrary execution. |
| 9 | Unbounded cache growth (memory leak) | Performance | Both | Logical error causing memory to grow; seen statically and by monitoring memory. |
| 10 | Dead code after return | Correctness | Static | `result.clear()` is never reached; harmless but incorrect flow. |
| 11 | Missing rollback on exception | Correctness | Both | If `withdraw` throws, transfer is inconsistent; runtime behavior matters. |
| 12 | Quadratic search (inefficient) | Performance | Static | Nested loops cause poor scaling; static analysis can infer O(n²). |
| 13 | Cross‑site scripting (direct DOM write) | Security | Static | Inserting `userInput` directly may allow XSS; static taint analysis catches it. |
| 14 | Potential divide‑by‑zero | Correctness | Both | `divisor` not checked; static check or dynamic crash. |
| 15 | Returning pointer to stack variable | Correctness | Static | Pointer refers to out‑of‑scope array; undefined behavior. |

---

## Summary Questions

### How many snippets had Correctness issues? 8
### How many had Security issues? 3
### How many had Performance issues? 4

### Which issues are best caught by static analysis? Why?
Several problems – unreachable code, off‑by‑one errors, missing null terminator, SQL/command injection, resource leaks, dead code, XSS and returning stack pointers – are structural and can be detected purely by examining source. Static analysis can reason about control flow, data flow, and common idioms to flag these before execution.

### Which issues require dynamic analysis? Why?
Bugs that depend on runtime values or performance profiles (empty list division, exponential recursion, unbounded cache growth, exception handling in transfers, divide‑by‑zero) need execution information. Dynamic tools exercise the code and observe actual inputs, resource usage, or exceptions to reveal these faults.

