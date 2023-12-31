-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.Char

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
run (Equ:code, _ :B y:stack, state) = error "Run-time error"
run (Equ:code, B x:_:stack, state) = error "Run-time error"
run (Le:code, I x:I y:stack, state) = run (code, B (x <= y):stack, state)
run (Le:code, B x:B y:stack, state) = error "Run-time error"
run (Le:code, _ :B y:stack, state) = error "Run-time error"
run (Le:code, B x:_:stack, state) = error "Run-time error"
run (And:code, B x:B y:stack, state) = run (code, B (x && y):stack, state)
run (And:code, I x:I y:stack, state) = error "Run-time error"
run (And:code, _ :I y:stack, state) = error "Run-time error"
run (And:code, I x:_:stack, state) = error "Run-time error"
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

data Exp = Ax Aexp | Bx Bexp deriving (Show, Eq)

data Aexp = Var String          -- Variable
        | Const Integer         -- Constant
        | AAdd Aexp Aexp        -- Addition
        | MMult Aexp Aexp       -- Multiplication
        | SSub Aexp Aexp        -- Subtraction
        deriving (Show, Eq)

data Bexp = TRUE                -- TRUE
        | FALSE                 -- FALSE
        | NNeg Bexp             -- Negation
        | AAnd Bexp Bexp        -- Conjunction
        | LLe Aexp Aexp         -- Less than or equal to
        | EEq Exp Exp           -- Equality
        deriving (Show, Eq)

data Stm = Assign String Aexp   -- Assignment
        | If Bexp [Stm] [Stm]   -- If-then-else
        | While Bexp [Stm]      -- While loop
        deriving (Show, Eq)

compE :: Exp -> Code
compE (Ax x) = compA x
compE (Bx x) = compB x

compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (Const x) = [Push x]
compA (AAdd x y) = compA y ++ compA x ++ [Add]
compA (MMult x y) = compA y ++ compA x ++ [Mult]
compA (SSub x y) = compA y ++ compA x ++ [Sub]

compB :: Bexp -> Code
compB TRUE = [Tru]
compB FALSE = [Fals]
compB (NNeg x) = compB x ++ [Neg]
compB (AAnd x y) = compB y ++ compB x ++ [And]
compB (LLe x y) = compA y ++ compA x ++ [Le]
compB (EEq x y) = compE y ++ compE x ++ [Equ]

compile :: [Stm] -> Code
compile [ ] = [ ]
compile (Assign x y:xs) = compA y ++ [Store x] ++ compile xs
compile (If x y z:xs) = compB x ++ [Branch (compile y) (compile z)] ++ compile xs
compile (While x y:xs) = [Loop (compB x) (compile y)] ++ compile xs

--------------  TEST  ----------------

data Token
    = TVar String
    | TConst Integer
    | TPlus
    | TTimes
    | TMinus
    | TTrue
    | TFalse
    | TNot
    | TAnd
    | TLe
    | TEqBool
    | TEqArith
    | TAssign
    | TIf
    | TThen
    | TElse
    | TWhile
    | TDo
    | TSemi
    | TOpenParen
    | TCloseParen
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [ ] = [ ]
lexer (' ':xs) = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('\t':xs) = lexer xs
lexer ('+':xs) = TPlus:lexer xs
lexer ('*':xs) = TTimes:lexer xs
lexer ('-':xs) = TMinus:lexer xs
lexer ('=':'=':xs) = TEqArith:lexer xs
lexer ('=':xs) = TEqBool:lexer xs
lexer ('<':'=':xs) = TLe:lexer xs
lexer ('(':xs) = TOpenParen:lexer xs
lexer (')':xs) = TCloseParen:lexer xs
lexer (';':xs) = TSemi:lexer xs
lexer ('a':'n':'d':xs) = TAnd:lexer xs
lexer ('n':'o':'t':xs) = TNot:lexer xs
lexer ('i':'f':xs) = TIf:lexer xs
lexer ('t':'h':'e':'n':xs) = TThen:lexer xs
lexer ('e':'l':'s':'e':xs) = TElse:lexer xs
lexer ('w':'h':'i':'l':'e':xs) = TWhile:lexer xs
lexer ('d':'o':xs) = TDo:lexer xs
lexer (':':'=':xs) = TAssign:lexer xs
lexer ('T':'r':'u':'e':xs) = TTrue:lexer xs
lexer ('F':'a':'l':'s':'e':xs) = TFalse:lexer xs
lexer (x:xs)
    | isDigit x = TConst (read (x:takeWhile isDigit xs)) : lexer (dropWhile isDigit xs)
    | isAlpha x = TVar (x:takeWhile isAlphaNum xs) : lexer (dropWhile isAlphaNum xs)
    | otherwise = error "Lexer error"

testLexerTemplate :: String -> [Token] -> IO ()
testLexerTemplate input expected
    = putStrLn ("Input: " ++ show input ++ "\n") >> putStr "Output: " >> print result >> putStrLn ""
    >> putStrLn ("Expected output: " ++ show expected ++ "\n")
    >> if result == expected
        then putStrLn "Test passed!"
        else putStrLn "Test failed!"
    where result = lexer input

testLexer :: Int -> IO ()
testLexer 1 = testLexerTemplate
    "x := 5; x := x - 1;"
    [TVar "x",TAssign,TConst 5,TSemi,TVar "x",TAssign,TVar "x",TMinus,TConst 1,TSemi]

testLexer 2 = testLexerTemplate
    "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
    [TIf,TOpenParen,TNot,TTrue,TAnd,TConst 2,TLe,TConst 5,TEqBool,TConst 3,
    TEqArith,TConst 4,TCloseParen,TThen,TVar "x",TAssign,TConst 1,TSemi,
    TElse,TVar "y",TAssign,TConst 2,TSemi]

testLexer 3 = testLexerTemplate
    "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
    [TVar "x",TAssign,TConst 42,TSemi,TIf,TVar "x",TLe,TConst 43,TThen,TVar "x",
    TAssign,TConst 1,TSemi,TElse,TOpenParen,TVar "x",TAssign,TConst 33,TSemi,
    TVar "x",TAssign,TVar "x",TPlus,TConst 1,TSemi,TCloseParen]

testLexer 4 = testLexerTemplate
    "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
    [TVar "x",TAssign,TConst 42,TSemi,TIf,TVar "x",TLe,TConst 43,TThen,TVar "x",
    TAssign,TConst 1,TSemi,TElse,TVar "x",TAssign,TConst 33,TSemi,TVar "x",
    TAssign,TVar "x",TPlus,TConst 1,TSemi]

testLexer 5 = testLexerTemplate
    "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
    [TVar "x",TAssign,TConst 42,TSemi,TIf,TVar "x",TLe,TConst 43,TThen,TVar "x",
    TAssign,TConst 1,TSemi,TElse,TVar "x",TAssign,TConst 33,TSemi,TVar "x",
    TAssign,TVar "x",TPlus,TConst 1,TSemi,TVar "z",TAssign,TVar "x",TPlus,
    TVar "x",TSemi]

testLexer 6 = testLexerTemplate
    "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
    [TVar "x",TAssign,TConst 2,TSemi,TVar "y",TAssign,TOpenParen,TVar "x",
    TMinus,TConst 3,TCloseParen,TTimes,TOpenParen,TConst 4,TPlus,TConst 2,
    TTimes,TConst 3,TCloseParen,TSemi,TVar "z",TAssign,TVar "x",TPlus,
    TVar "x",TTimes,TOpenParen,TConst 2,TCloseParen,TSemi]

testLexer 7 = testLexerTemplate
    "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
    [TVar "i",TAssign,TConst 10,TSemi,TVar "fact",TAssign,TConst 1,TSemi,TWhile,
    TOpenParen,TNot,TOpenParen,TVar "i",TEqArith,TConst 1,TCloseParen,TCloseParen,
    TDo,TOpenParen,TVar "fact",TAssign,TVar "fact",TTimes,TVar "i",TSemi,
    TVar "i",TAssign,TVar "i",TMinus,TConst 1,TSemi,TCloseParen,TSemi]

testLexer _ = error "testLexer: Invalid test number (1-7)"

parseAexpConstVar :: [Token] -> (Aexp, [Token])
parseAexpConstVar (TConst x:xs) = (Const x, xs)
parseAexpConstVar (TVar x:xs) = (Var x, xs)
parseAexpConstVar (TOpenParen:xs)
    = case parseAexp xs of
        (x, TCloseParen:xs') -> (x, xs')
        _ -> error "parseAexpConstVar: Parse error"
parseAexpConstVar _ = error "parseAexpConstVar: Parse error"

parseAexp :: [Token] -> (Aexp, [Token])
parseAexp tokens
    = case parseAexpConstVar tokens of
        (x, TPlus:xs) -> case parseAexp xs of
            (y, xs') -> (AAdd x y, xs')
        (x, TMinus:xs) -> case parseAexp xs of
            (y, xs') -> (SSub x y, xs')
        (x, TTimes:xs) -> case parseAexp xs of
            (y, xs') -> (MMult x y, xs')
        (x, xs) -> (x, xs)

parseBexpConst :: [Token] -> (Bexp, [Token])
parseBexpConst (TTrue:xs) = (TRUE, xs)
parseBexpConst (TFalse:xs) = (FALSE, xs)
parseBexpConst (TNot:xs)
    = case parseBexp xs of
        (x, xs') -> (NNeg x, xs')
parseBexpConst (TOpenParen:xs)
    = case parseBexp xs of
        (x, TCloseParen:xs') -> (x, xs')
        _ -> error "parseBexpConst: Parse error"

parseBexpConst _ = error "parseBexpConst: Parse error"

parseBexp :: [Token] -> (Bexp, [Token])
parseBexp tokens
    = case parseBexpConst tokens of
        (x, TAnd:xs) -> case parseBexp xs of
            (y, xs') -> (AAnd x y, xs')
        (x, TEqBool:xs) -> case parseBexp xs of
            (y, xs') -> (EEq (Bx x) (Bx y), xs')
        (x, xs) -> (x, xs)

parseBexpWithAexp :: [Token] -> (Bexp, [Token])
parseBexpWithAexp tokens
    = case parseAexp tokens of
        (x, TLe:xs) -> case parseAexp xs of
            (y, xs') -> (LLe x y, xs')
        (x, TEqArith:xs) -> case parseAexp xs of
            (y, xs') -> (EEq (Ax x) (Ax y), xs')
        (x, xs) -> error "parseBexpWithAexp: Parse error"

parseTokens :: [Token] -> [Stm]
parseTokens [ ] = [ ]
parseTokens (TSemi:xs) = parseTokens xs
parseTokens (TVar x:TAssign:TConst y:xs)
    | isUpper (head x) = error "parseTokens: Parse error - variable name starts with uppercase letter"
    | otherwise = Assign x n : parseTokens sn
    where (n, sn) = parseAexp (TConst y:xs)
parseTokens (TVar x:TAssign:TVar y:xs)
    | isUpper (head x) = error "parseTokens: Parse error - variable name starts with uppercase letter"
    | otherwise = Assign x n : parseTokens sn
    where (n, sn) = parseAexp (TVar y:xs)
parseTokens (TVar x:TAssign:TOpenParen:xs)
    | isUpper (head x) = error "parseTokens: Parse error - variable name starts with uppercase letter"
    | otherwise = Assign x n : parseTokens sn
    where (n, sn) = parseAexp (TOpenParen:xs)

-- parenthesis not working
parseTokens (TIf:TConst x:xs) = If n (parseTokens s1) (parseTokens s2) : parseTokens s3
    where (n, (TThen:rest)) = parseBexpWithAexp (TConst x:xs)
          s1 = takeWhile (/= TElse) rest
          ((TElse:s2), s3) = separateLists (dropWhile (/= TElse) rest)

parseTokens (TIf:TVar x:xs) = If n (parseTokens s1) (parseTokens s2) : parseTokens s3
    where (n, (TThen:rest)) = parseBexpWithAexp (TVar x:xs)
          s1 = takeWhile (/= TElse) rest
          ((TElse:s2), s3) = separateLists (dropWhile (/= TElse) rest)

parseTokens (TIf:TNot:xs) = If n (parseTokens s1) (parseTokens s2) : parseTokens s3
    where (n, (TThen:rest)) = parseBexp (TNot:xs)
          s1 = takeWhile (/= TElse) rest
          ((TElse:s2), s3) = separateLists (dropWhile (/= TElse) rest)

parseTokens (TIf:TTrue:xs) = If n (parseTokens s1) (parseTokens s2) : parseTokens s3
    where (n, (TThen:rest)) = parseBexp (TTrue:xs)
          s1 = takeWhile (/= TElse) rest
          ((TElse:s2), s3) = separateLists (dropWhile (/= TElse) rest)

parseTokens (TIf:TFalse:xs) = If n (parseTokens s1) (parseTokens s2) : parseTokens s3
    where (n, (TThen:rest)) = parseBexp (TFalse:xs)
          s1 = takeWhile (/= TElse) rest
          ((TElse:s2), s3) = separateLists (dropWhile (/= TElse) rest)

-- parenthesis not working
separateLists :: [Token] -> ([Token], [Token])
separateLists [] = ([], [])
separateLists (TElse : TOpenParen : rest) =
  case break (== TCloseParen) rest of
    (insideParen, afterCloseParen) -> ([TElse, TOpenParen] ++ insideParen ++ [TCloseParen], (drop 1 afterCloseParen))

separateLists (TElse : rest) =
  case break (\t -> t == TSemi || t == TOpenParen) rest of
    (beforeSemiOrParen, afterSemiOrParen) ->
      if null afterSemiOrParen
        then ([TElse] ++ beforeSemiOrParen ++ [TSemi], [])
        else ([TElse] ++ beforeSemiOrParen ++ [TSemi], (drop 1 afterSemiOrParen))

--------------  TEST  ----------------

parse :: String -> [Stm]
parse = parseTokens . lexer

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
