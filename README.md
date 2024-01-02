# TP2 - Haskell Project
Group T08_G03

| Name                        | UP                                        | Contribuition |
| ------------                | ------------                              |------------   |
| Joaquim Afonso Marques da Cunha    | up202108779 |            |
| João Pedro Carvalho Correia   | up202005015 |            |


# Introduction 

This project consists in the creation of a low-level machine and a compiler for a small imperative programming language.

# Part 1

### Data Definitions

The data definitions for the stack machine include three main data types:
1. **Inst:** Represents the stack machine instructions. Instructions can be arithmetic operations (Push, Add, Mult, Sub), boolean operations (Tru, Fals, Equ, Le, And, Neg), memory operations (Fetch, Store), control flow operations (Noop, Branch, Loop).
2. **StackT:** Represents the stack items. The stack can contain either integers (`I Integer`) or booleans (`B Bool`).
3. **State:** Represents the state of the memory, which is a list of pairs associating variable names with their corresponding stack items.

### Functions

- **`run` Function:** This function takes the current code, stack, and state as input and executes the stack machine instructions. It handles the operations described above (arithmetic calculations, boolean operations, memory operations, and control flow).
  
- **Memory Operations (`Fetch` and `Store`):** The `Fetch` operation retrieves the value of a variable from the state, and the `Store` operation updates the state with a new variable-value pair.

- **Error Handling:** The `run` function includes error handling for certain conditions, such as run-time errors during arithmetic and boolean operations.

- **String Conversion Functions (`stack2Str` and `state2Str`):** These functions convert the stack and state to strings for testing purposes. They handle the display of boolean and integer values.

- **Utility Functions (`findInState` and `removeInState`):** These functions assist in searching for and removing entries in the state.

### Testing

The `testAssembler` function is provided to facilitate testing. It takes a code and returns a tuple containing the string representation of the stack and state after execution.

### Examples

**`stack2Str`**

```haskell
ghci> stack2Str [I 42, B True, I 10]
"42,True,10"
```

**`state2Str`**

```haskell
ghci> state2Str [("x", I 5), ("y", B False), ("z", I 20)]
"x=5,y=False,z=20"
```

**`Arithmetic case`**

```haskell
ghci> testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
True
```

**`Boolean case`**

```haskell
ghci> testAssembler [Push (-20),Push (-21), Le] == ("True","")
True
```

**`Loop case`**
```haskell
ghci> testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
True
```

**`Error case`**
```haskell
ghci> testAssembler [Push 1,Push 2,And]
("*** Exception: Run-time error
CallStack (from HasCallStack):
  error, called at quickstart.hs:71:40 in main:Main
```

# Part 2

## Data Structures

The data structures for this part of the project are `Exp`, `Aexp`, `Bexp`, and `Stm`. These structures represent expressions, arithmetic expressions, boolean expressions, and statements, respectively. 

### `Exp` (Expression)

The `Exp` is a container that holds either an arithmetic expression (`Ax`) or a boolean expression (`Bx`). It represents any expression in the language.

- **`Ax` (Arithmetic Expression):** Deals with numbers, variables, addition, subtraction, and multiplication.
- **`Bx` (Boolean Expression):** Deals with `true`, `false`, negation (not), conjunction (and), less than or equal to, and equality.

### `Aexp` (Arithmetic Expression)

Focuses specifically on arithmetic expressions. Includes variables, constants, addition, multiplication, and subtraction.

### `Bexp` (Boolean Expression)

Focuses specifically on boolean expressions. Includes `true`, `false`, negation (not), conjunction (and), less than or equal to, and equality.

### `Stm` (Statement)

Represents a command or instruction in the language. Can be an assignment, conditional statement (`if`), or while loop.

---

## Compiler Implementation Details

### 1. Expression Compilation

#### `compE` (Compile Expression)

The `compE` function is responsible for translating high-level expressions (`Exp`) into a sequence of low-level instructions (`Code`). This process involves handling both arithmetic (`Ax`) and boolean (`Bx`) expressions.

#### `compA` and `compB` Functions

- **`compA` (Compile Arithmetic Expression):**
  - Translates arithmetic expressions (`Aexp`) into a series of low-level instructions (`Code`).
  - Deals with variables, constants, addition, subtraction, and multiplication.

- **`compB` (Compile Boolean Expression):**
  - Translates boolean expressions (`Bexp`) into equivalent low-level instructions (`Code`).
  - Handles `true`, `false`, negation (not), conjunction (and), less than or equal to, and equality.

### 2. Statement Compilation

#### `compile` Function

The `compile` function transforms a sequence of high-level statements (`Stm`) into executable low-level code (`Code`). This process encompasses various language constructs, such as assignments, conditional statements, and while loops.

#### Statement Types

- **Assignment:**
  - Handles statements like `x := 5;`, translating them into code that updates the value of the variable `x`.

- **Conditional Statements:**
  - Translates conditional statements like `if (x > 0) then ... else ...` into code that evaluates the condition and executes the appropriate branch.

- **While Loops:**
  - Converts while loops such as `while (x > 0) do ...` into code that repeatedly executes the specified block as long as the condition holds.


