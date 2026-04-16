# AST Structure Mapping Worksheet

## Instructions
For each snippet, first draw the AST by hand, then verify with the visualizer.

---

## Snippet 1: `result = (2 + 3) * 4`

### Hand-drawn AST
```
Assign("result")
  └─ BinOp(*)
      ├─ BinOp(+)
      │   ├─ IntLit(2)
      │   └─ IntLit(3)
      └─ IntLit(4)
```

The tree shows operator precedence: addition (2+3) is evaluated first (deeper in tree),
then multiplication (*4) is applied to the result.

### Visualizer output matches? (yes/no): yes

### Node types used:
- **Assign**: 1 occurrence - represents the assignment to variable "result"
- **BinOp**: 2 occurrences - one for addition (+), one for multiplication (*)
- **IntLit**: 3 occurrences - the literals 2, 3, and 4


---

## Snippet 2: If-Else
```python
if x > 0:
    y = x + 1
else:
    y = 0
```

### Hand-drawn AST
```
If
  ├─ condition: BinOp(>)
  │   ├─ Var("x")
  │   └─ IntLit(0)
  ├─ Then branch:
  │   └─ Assign("y")
  │       └─ BinOp(+)
  │           ├─ Var("x")
  │           └─ IntLit(1)
  └─ Else branch:
      └─ Assign("y")
          └─ IntLit(0)
```

### Key observation about the If node's children:
The If node has **three children**:
1. A condition expression (BinOp comparing x > 0)
2. A "then" branch containing a list of statements to execute if condition is true
3. An "else" branch containing a list of statements to execute if condition is false

Both branches can contain multiple statements, not just single statements.


---

## Snippet 3: Function Definition
```python
def greet(name):
    message = "Hello, " + name
    return message
```

### Hand-drawn AST
```
FunctionDef("greet")
  ├─ parameters: ["name"]
  └─ body:
      ├─ Assign("message")
      │   └─ BinOp(+)
      │       ├─ StrLit("Hello, ")
      │       └─ Var("name")
      └─ Return
          └─ Var("message")
```

Note: In our simplified AST, we focus on the function body (the statements inside).
The function definition metadata (name, parameters) is stored separately.

### How is the function parameter represented?
The function parameter "name" is represented in two places:
1. **In the function signature**: As a parameter name in the FunctionDef node's parameter list
2. **In the function body**: As a Var("name") node when the parameter is referenced in expressions

The parameter itself doesn't have a special node type when used - it's treated like any other variable (Var) in the body. The AST doesn't distinguish between parameters, local variables, or global variables at the syntax level - that's determined by semantic analysis (scope checking).


---

## Snippet 4: For Loop
```python
total = 0
for i in range(10):
    if i % 2 == 0:
        total = total + i
```

### Hand-drawn AST
```
Assign("total")
  └─ IntLit(0)
For
  ├─ target: "i"
  ├─ iter: Call("range")
  │   └─ IntLit(10)
  └─ body:
      └─ If
          ├─ condition: BinOp(==)
          │   ├─ BinOp(%)
          │   │   ├─ Var("i")
          │   │   └─ IntLit(2)
          │   └─ IntLit(0)
          ├─ then:
          │   └─ Assign("total")
          │       └─ BinOp(+)
          │           ├─ Var("total")
          │           └─ Var("i")
          └─ else: []
```

### What is the nesting depth of the innermost node?
The nesting depth is **6 levels**:
1. For loop
2. If statement (in for body)
3. BinOp(==) condition (in if condition)
4. BinOp(%) left operand (in ==)
5. Var("i") (in %)
6. The deepest nodes are also at depth 6: IntLit(2) in the modulo operation

Alternatively counting from the innermost assignment:
1. For loop → 2. If statement → 3. Assign("total") → 4. BinOp(+) → 5. Var("total") or Var("i")

The innermost **statement** is the Assign at depth 3, but the innermost **expression nodes** (the leaf Var and IntLit nodes) are at depth 5-6.


---

## Reflection Questions

### 1. How does operator precedence appear in the AST?

Operator precedence is represented by the **tree structure and depth**, not by explicit precedence numbers. Higher-precedence operators appear **deeper (lower)** in the tree and are evaluated first.

Example from Snippet 1: `result = (2 + 3) * 4`
- The addition operator (+) is deeper in the tree (child of *)
- The multiplication operator (*) is higher in the tree (child of Assign)
- This ensures addition is evaluated before multiplication, as required by precedence

The AST structure naturally enforces evaluation order: children are evaluated before parents, so deeper nodes are computed first.

### 2. What syntactic elements (from the source code) are NOT in the AST?

Several syntactic elements are **absent** from the AST:
- **Whitespace and indentation** - AST cares about structure, not formatting
- **Parentheses** - They affect parsing/precedence but aren't stored (precedence is in tree structure)
- **Semicolons and commas** - Delimiters used for parsing but not needed in tree form
- **Keywords** - Words like `if`, `while`, `def`, `return` become node types instead
- **Comments** - Typically discarded during parsing (unless specially preserved)
- **Syntactic sugar** - e.g., `x += 1` becomes `x = x + 1` in the AST

The AST is an **abstract** representation - it captures semantic meaning while discarding syntactic details that were only needed for parsing.

### 3. How would you use the AST to find all variable assignments in a program?

To find all variable assignments:

1. **Traverse the entire AST** recursively (depth-first or breadth-first)
2. **Pattern match** on each node to check if it's an `Assign` statement
3. **Collect information** from matching nodes:
   - Variable name being assigned (e.g., `Assign(var_name, expr)`)
   - The expression being assigned
   - Location in the tree (for source mapping)
4. **Recurse into compound nodes** (If, While, Block) to find nested assignments

Example implementation pattern:
```ocaml
let rec find_assignments stmt = match stmt with
  | Assign(var, expr) -> [(var, expr)]  (* Found one! *)
  | If(_, then_b, else_b) ->
      find_assignments_list then_b @ find_assignments_list else_b
  | While(_, body) -> find_assignments_list body
  | Block(stmts) -> find_assignments_list stmts
  | _ -> []
```

This is exactly the pattern used in `count_stmt` function in the visualizer!

