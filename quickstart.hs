-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackT = I Integer | B Bool deriving Show
type Stack = [StackT]

type State = [(String, StackT)]

createEmptyStack :: Stack
createEmptyStack = [ ]

stack2Str :: Stack -> String
stack2Str [ ] = ""
stack2Str stack = init (stack2StrHelper stack) -- remove last comma from helper

stack2StrHelper :: Stack -> String
stack2StrHelper [ ] = ""
stack2StrHelper (B True:xs) = "True," ++ stack2StrHelper xs
stack2StrHelper (B False:xs) = "False," ++ stack2StrHelper xs
stack2StrHelper (I x:xs) = show x ++ "," ++ stack2StrHelper xs

createEmptyState :: State
createEmptyState = [ ]

state2Str :: State -> String
state2Str [ ] = ""
state2Str state = init (state2StrHelper state) -- remove last comma from helper

state2StrHelper :: State -> String
state2StrHelper [ ] = ""
state2StrHelper ((x, B True):xs) = x ++ "=True," ++ state2StrHelper xs
state2StrHelper ((x, B False):xs) = x ++ "=False," ++ state2StrHelper xs
state2StrHelper ((x, I y):xs) = x ++ "=" ++ show y ++ "," ++ state2StrHelper xs

-- TODO: Check the add, mult and sub run-time errors
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([ ], stack, state) = ([ ], stack, state)
run (Push x:code, stack, state) = run (code, I x:stack, state)
run (Add:code, I x:I y:stack, state) = run (code, I (x + y):stack, state)
run (Add:code, _ :B y:stack, state) = error "Run-time error"
run (Add:code, B x:_:stack, state) = error "Run-time error"
run (Mult:code, I x:I y:stack, state) = run (code, I (x * y):stack, state)
run (Mult:code, _ :B y:stack, state) = error "Run-time error"
run (Mult:code, B x:_:stack, state) = error "Run-time error"
run (Sub:code, I x:I y:stack, state) = run (code, I (x - y):stack, state)
run (Sub:code, _ :B y:stack, state) = error "Run-time error"
run (Sub:code, B x:_:stack, state) = error "Run-time error"
run (Tru:code, stack, state) = run (code, B True:stack, state)
run (Fals:code, stack, state) = run (code, B False:stack, state)
run (Equ:code, I x:I y:stack, state) = run (code, B (x == y):stack, state)
run (Equ:code, B x:B y:stack, state) = run (code, B (x == y):stack, state)
run (Le:code, I x:I y:stack, state) = run (code, B (x <=y ):stack, state)
run (Le:code, B x:B y:stack, state) = error "Run-time error"
run (And:code, B x:B y:stack, state) = run (code, B (x && y):stack, state)
run (And:code, I x:I y:stack, state) = error "Run-time error" -- TODO: check if this is correct
run (Neg:code, B x:stack, state) = run (code, B (not x):stack, state)
run (Neg:code, I x:stack, state) = error "Run-time error" -- TODO: check if this is correct
run (Fetch x:code, stack, state) = run (code, findInState x state:stack, state)
run (Store x:code, y:stack, state) = run (code, stack, (x, y):removeInState x state)
run (Noop:code, stack, state) = run (code, stack, state)
run (Branch code1 code2:code, B True:stack, state) = run (code1 ++ code, stack, state)
run (Branch code1 code2:code, B False:stack, state) = run (code2 ++ code, stack, state)
run (Loop code1 code2:code, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)

findInState :: String -> State -> StackT
findInState x [ ] = error "Run-time error"
findInState x ((y, z):xs)
    | x == y = z
    | otherwise = findInState x xs

removeInState :: String -> State -> State
removeInState x [ ] = [ ]
removeInState x ((y, z):xs)
    | x == y = xs
    | otherwise = (y, z):removeInState x xs

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, store2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
