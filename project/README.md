# SpreadsheetLang

A small spreadsheet-oriented programming language implemented in Haskell.

The project provides a parser, dependency analysis, cycle detection, and evaluation engine for spreadsheet-like formulas. Cells can contain numeric values, arithmetic expressions, references to other cells, and range-based aggregation functions.

---

## Features

* Numeric cell values
* Arithmetic expressions (`+`, `-`, `*`, `/`)
* Cell references (`A1`, `B2`, ...)
* Dependency graph construction
* Cycle detection
* Range operations:

  * `SUM`
  * `AVG`
* Error propagation
* Evaluation independent of cell declaration order
* Command-line interface

---

## Example

Input file:

```text
sheet {
  A1 = 10;
  A2 = 20;
  A3 = A1 + A2;
  A4 = SUM(A1:A3);
}
```

Output:

```text
A1 = 10
A2 = 20
A3 = 30
A4 = 60
```

---

## Language Syntax

### Cell Definitions

```text
A1 = 10;
B1 = A1 + 5;
C1 = B1 * 2;
```

### Arithmetic Expressions

Supported operators:

```text
+
-
*
/
```

Example:

```text
A1 = 10;
A2 = 5;
A3 = (A1 + A2) * 2;
```

### Cell References

Cells may refer to previously defined or later defined cells:

```text
A1 = B1 + 5;
B1 = 10;
```

The evaluator automatically resolves dependencies.

### Range Functions

#### SUM

```text
A1 = 10;
A2 = 20;
A3 = 30;
A4 = SUM(A1:A3);
```

Result:

```text
A4 = 60
```

#### AVG

```text
A1 = 10;
A2 = 20;
A3 = 30;
A4 = AVG(A1:A3);
```

Result:

```text
A4 = 20
```

---

## Error Handling

The evaluator reports the following errors:

### Unknown Cell

```text
A1 = B1 + 5;
```

Error:

```text
Unknown cell: B1
```

### Division by Zero

```text
A1 = 10 / 0;
```

Error:

```text
Division by zero
```

### Type Error

Occurs when an operation is applied to incompatible values.

### Circular Dependency

```text
A1 = B1;
B1 = A1;
```

Error:

```text
Cycle detected
```

---

## Project Architecture

### Parser

The parser is implemented using Megaparsec and converts source files into an Abstract Syntax Tree (AST).

### Dependency Analysis

Before evaluation, a dependency graph is built where:

* vertices represent cells
* edges represent references between cells

Example:

```text
A3 = A1 + A2
```

creates dependencies:

```text
A3 → A1
A3 → A2
```

### Cycle Detection

The dependency graph is analyzed before evaluation.

If a cycle is found, evaluation is stopped and an error is reported.

### Evaluation

Cells are evaluated according to dependency order rather than declaration order.

This allows formulas such as:

```text
A1 = B1 + 1;
B1 = 10;
```

to be evaluated correctly.

---

## Project Structure

```text
src/
├── AST.hs
├── Parser.hs
├── Evaluator.hs
├── Dependency.hs
├── CycleDetection.hs

app/
└── Main.hs

examples/
└── basic.sheet
```

### Files

| File              | Description                         |
| ----------------- | ----------------------------------- |
| AST.hs            | Abstract syntax tree definitions    |
| Parser.hs         | Language parser                     |
| Evaluator.hs      | Expression evaluation               |
| Dependency.hs     | Dependency graph construction       |
| CycleDetection.hs | Cycle detection in dependency graph |
| Main.hs           | Command-line interface              |

---

## Building the Project

Requirements:

* GHC 9.x
* Cabal

Build:

```bash
cabal build
```

Run tests:

```bash
cabal test
```

Run the interpreter:

```bash
cabal run spreadsheet-lang -- examples/basic.sheet
```

---

## Technologies

* Haskell
* Megaparsec
* Data.Map
* Data.Graph
* Cabal

---

## Future Extensions

Possible improvements:

* String values
* Boolean values
* Conditional expressions (`IF`)
* Additional spreadsheet functions (`MIN`, `MAX`, `COUNT`)
* CSV import/export
* Interactive REPL
* Spreadsheet GUI

---
