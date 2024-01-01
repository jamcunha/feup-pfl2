# TP2 - Haskell Project
Group T08_G03

| Name                        | UP                                        | Contribuition |
| ------------                | ------------                              |------------   |
| Joaquim Afonso Marques da Cunha    | up202108779 |            |
| João Pedro Carvalho Correia   | up202005015 |            |


# Introduction 

This project consists in a low-level machine ....

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




