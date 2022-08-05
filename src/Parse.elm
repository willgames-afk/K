module Parse exposing (..)

import Dict exposing (Dict)
import List exposing (map, foldl)

--- PARSING BASICS ---

type MaybeParsed a 
    = Success a      String   --Whatever we parsed and the rest of the input
    | Error   (String, String) -- An error message, and the rest of the input- used for error locating

 -- Takes Function that returns true when the character is valid, and input string. Returns the parsed string and the remaining input
parseWhile: (Char -> Bool) -> String -> (String, String)
parseWhile func chars = parseWhile_ func (token chars) ""
parseWhile_ func chars out = 
    case String.uncons chars of 
        Just res ->
            if func (Tuple.first res) then
                parseWhile_ func (Tuple.second res) (String.cons (Tuple.first res) out)
            else 
                (String.reverse out, chars)
        Nothing -> (String.reverse out, chars)

lit: String -> String -> MaybeParsed String
lit literal rawchars =
    let chars = token rawchars in
        if String.startsWith literal chars then
            Success literal (String.dropLeft (String.length literal) chars)
        else 
            Error ("Expected `" ++ literal ++ "`", chars)

token: String -> String
token chars = String.trimLeft chars



--- BASIC ELEMENTS

number: String -> MaybeParsed Int
number chars = 
    let 
        res = parseWhile Char.isDigit chars
        maybeInt = String.toInt (Tuple.first res)
    in 
        case maybeInt of 
            Just value -> Success value (Tuple.second res)
            Nothing -> Error ("Expected Number!", Tuple.second res)

name: String -> MaybeParsed String
name chars =
    let 
        res = parseWhile Char.isAlphaNum chars
    in
        if String.isEmpty (Tuple.first res) then 
            Error ("Expected Name!", (Tuple.second res))
        else
            Success (Tuple.first res) (Tuple.second res)

bool: String -> MaybeParsed Bool
bool chars =
    case lit "true" chars of
        Success _ rem -> Success True rem
        Error _ ->
            case lit "false" chars of
                Success _ rem -> Success False rem
                Error err -> Error ("Expected Boolean!", Tuple.second err)

block: String -> MaybeParsed Block
block chars = 
    case lit "{" chars of 
        Error err -> Error ("Expected Block", Tuple.second err)
        Success _ rem -> case fetchBlock (String.toList rem) 1 "" of 
            Error err2 -> Error err2
            Success blocktxt rem2 -> case parseProgram blocktxt of
                Success prog _ -> Success prog rem2
                Error err3 -> Error err3


-- Scans thru a string looking for a matching parenthesis, then returns all the previos chars and the remaindier
fetchBlock: List Char -> Int -> String -> MaybeParsed String
fetchBlock chars counter buffer = case  chars of
    [] -> Error ("Missing `}` to close block","")
    char :: xs -> case char of
        '{' -> fetchBlock xs (counter + 1)  (buffer ++ "{")
        '}' -> if (counter == 1) then
                Success buffer (String.fromList xs)
            else
                fetchBlock xs (counter - 1) (buffer ++ "}")
        x -> fetchBlock xs counter (buffer ++ (String.fromChar x))


ifelse: String -> MaybeParsed (MathExpr, Block, Block)
ifelse chars = 
    case lit "if" chars of
        Error err -> Error ("Expected Ifelse", Tuple.second err)
        Success _ rem -> case lit "(" rem of 
            Error err1 -> Error ("Expected `(`", Tuple.second err1)

            Success _ rem2 -> case infixexpr rem2 of
                Error err2 -> Error ("Expected Expression", Tuple.second err2)
    
                Success mxrcond rem3 -> case lit ")" rem3 of
                    Error err3 -> Error ("Expected `)`", Tuple.second err3)

                    Success _ rem4 -> case block rem4 of
                        Error err4 -> Error err4
                       
                        Success mxrtrue rem7 -> case lit "else" rem7 of
                            Error err7 -> Error ("Expected Else Branch", Tuple.second err7)

                            Success _ rem8 -> case block rem8 of
                                Error err8 -> Error err8
                                Success mxrfalse rem9 -> Success (mxrcond, mxrtrue, mxrfalse) rem9
funcDef: String -> MaybeParsed (String, List String, Block)      
funcDef chars =
    case name chars of
        Error err -> Error err
        Success fname rem1 -> case lit "(" rem1 of
            Error err -> Error err
            Success _ rem2 -> case nameList rem2 [] of
                Error err -> Error err --Should be impossible, namelist doesn't fail
                Success args rem4 -> case lit "=" rem4 of
                    Error err -> Error err
                    Success _ rem5 -> case block rem5 of
                        Error err -> Error err
                        Success code rem6 -> Success (fname, args, code) rem6
 
nameList chars names = case name chars of
    Error _ -> case lit ")" chars of
        Error err -> Error err
        Success _ rem4 -> Success names rem4
    Success n1 rem1 -> case lit "," rem1 of
        Success _ rem2 -> nameList rem2 (names ++ [n1])
        Error _ -> case lit ")" rem1 of
            Error err2 -> Error err2
            Success _ rem3 -> Success names rem3

exprList chars exprs = case mathexpr chars of
    Error _ -> case lit ")" chars of
        Error err -> Error err
        Success _ rem4 -> Success exprs rem4
    Success n1 rem1 -> case lit "," rem1 of
        Success _ rem2 -> exprList rem2 (exprs ++ [n1])
        Error _ -> case lit ")" rem1 of
            Error err2 -> Error err2
            Success _ rem3 -> Success exprs rem3

funcCall: String -> MaybeParsed (String, List MathExpr)
funcCall chars = case name chars of
    Error err -> Error err
    Success fname rem1 -> case lit "(" rem1 of
        Error err2 -> Error err2
        Success _ rem2 -> case exprList rem2 [] of
            Error err3 -> Error err3
            Success args rem3 -> Success (fname, args) rem3 

 --- INFIX EXPRESSION PARSING ---

type Op 
    = Div | Mul | Mod
    | Add | Sub 
--  | Shl | Shr Disabled because you can't shift by more than 255
    | Llt | Lle | Lgt | Lge
    | Equ | Neq
    | Band
    | Bor
    | Land
    | Lor

type MType
    = MFunc (List MType) MType -- Arg types and return type
    | MInt
    | MBool
    | MA Int

--type alias VarList = Dict String MType
-- Moved to Main.elm and renamed to something more helpful

iInt = MInt  -- Leftover from a failed refactoring- I'm sick of changing that table
iBool = MBool


sigs: List (Op, ((MType, MType), MType))
sigs =  -- Op Signatures- 
    [ (Lor,  ((iBool, iBool), iBool))
    , (Land, ((iBool, iBool), iBool))
    , (Bor,  ((iInt,  iInt),  iInt))
    , (Band, ((iInt,  iInt),  iInt))
    , (Equ,  ((MA 1, MA 1), iBool))
    , (Neq,  ((MA 1, MA 1), iBool))
    , (Llt,  ((iInt,  iInt),  iBool))
    , (Lle,  ((iInt,  iInt),  iBool))
    , (Lgt,  ((iInt,  iInt),  iBool))
    , (Lge,  ((iInt,  iInt),  iBool))
--    , (Shl, ((MInt, MInt), MInt))
--    , (Shr, ((MInt, MInt), MInt))
    , (Add,  ((iInt,  iInt),  iInt))
    , (Sub,  ((iInt,  iInt),  iInt))
    , (Div,  ((iInt,  iInt),  iInt))
    , (Mul,  ((iInt,  iInt),  iInt))
    , (Mod,  ((iInt,  iInt),  iInt))
    ]

-- List of op tuples grouped by precedence (Later means more parethesisy)
-- op tuples contain one of the types in the Op Union and the string which represents that op.
math = 
    [ (False,
      [ (Lor, "or")
      ])
    , (False,
      [ (Land, "and")
      ])
    , (False,
      [ (Bor, "|")
      ])
    , (False,
      [ (Band, "&")
      ])
    , (False,
      [ (Equ, "==")
      , (Neq, "!=")
      ])
    , (False,
      [ (Llt, "<")
      , (Lle, "<=")
      , (Lgt, ">")
      , (Lge, ">=")
      ])
{-    , (False,
      [ (Shl, "<<")
      , (Shr, ">>")
      ]) -}
    , (False,
      [ (Add, "+")
      , (Sub, "-")
      ])
    , (False,
      [ (Div, "/")
      , (Mul, "*")
      , (Mod, "%")
      ])
    ]

-- Attempts to parse any operator in the current precedence level
infixop: List (Op, String) -> String -> MaybeParsed Op
infixop levels chars =
    case levels of 
        [] -> Error ("Expected Infix Operator", chars)
        val :: rest ->
            case lit ( Tuple.second val) chars of --Try to parse op
                Success _ rem -> Success (Tuple.first val) rem
                Error _ -> infixop rest chars -- Try to parse next op


-- Wrapper for infixexpr_
infixexpr: String -> MaybeParsed MathExpr
infixexpr chars = infixexpr_ math chars

-- Parses a term, and calls infixexpr_next
-- A term is either an operator from the next highest precedence, or a number
infixexpr_: List (Bool ,(List (Op, String))) -> String -> MaybeParsed MathExpr
infixexpr_ levels chars = 
    case levels of
        [] -> mathexpr chars -- If we're out of math levels, parse a mathexpr
        level :: rest -> 
            case infixexpr_ rest chars of -- Parse a Term
                Error err -> Error err      -- We need this term, let error fall thru
                Success term1 rem -> infixexpr_next term1 level rest rem

-- Parses as many op-term pairs as it can
infixexpr_next: MathExpr -> (Bool, List (Op,String)) -> List( (Bool, List (Op,String))) -> String -> MaybeParsed MathExpr
infixexpr_next term1 level tail chars =
    case infixop (Tuple.second level) chars of  -- Parse an Op
        Error _ -> Success term1 chars --If it fails, just return term1
        Success op rem ->
            case  infixexpr_ tail rem of  -- Parse a term
                Error err -> Error err
                Success term2 rem2 ->
                    case (Tuple.first level) of
                        False -> infixexpr_next (MathOp op term1 term2 ) level tail rem2  --Attempt to recursively parse another op-term pair
                        True -> let rterm2 = infixexpr_next term2 level tail rem2 in
                            case rterm2 of
                                Success val rem3 -> Success (MathOp op term1 val) rem3
                                Error err -> Error err



 --- FULL EXPRESSION PARSING ---

type MathExpr  --Todo: Rename this to inline expresssion or something like that
    = ValInt Int
    | ValBool Bool
    | Name String
    --| Ifelse MathExpr MathExpr MathExpr -- Inline If statement, disabled because it's not neccessary yet
    | MathOp Op MathExpr MathExpr 
    | FuncCall String (List MathExpr) --Fn name and args

type Line
    = Asnment String MathExpr -- Name and Value
    | Ifelse MathExpr Block Block -- Condition, code to run if true, and code to run if false- work still to be done
    | FunctionDef String (List String) Block -- Name, args, and code block

type alias Block = List Line

mathexpr: String -> MaybeParsed MathExpr
mathexpr chars = 
    case lit "(" chars of --Attempt to parse (
        Success _ rem -> case infixexpr rem of --Attempt to parse subexpr
            Success expr rem2 -> case lit ")" rem2 of -- Attempt to parse )
                Success _ rem3 -> Success expr rem3
                Error err -> Error err -- There always has to be a closing parenth 
            Error err -> Error err -- Parentheses must have content
        Error _ -> {-case ifelse chars of
            Success (cas, tru, fals) rem4 -> Success (Ifelse cas tru fals) rem4 --Disable inline ifelse for now
            Error err2 -> 
                if Tuple.first err2 == "Expected Ifelse!" then-}
                    case number chars of 
                        Success num rem4 -> Success (ValInt num) rem4
                        Error _ -> case bool chars of
                            Success val rem5 -> Success (ValBool val) rem5
                            Error _ -> case name chars of 
                                Success val rem6 -> Success (Name val) rem6
                                Error err -> Error ("Expected Mathexpr", Tuple.second err)
                {-else
                    Error err2-}

assign: String -> MaybeParsed (String, MathExpr)
assign chars = 
    let mname = name chars in
        case mname of 
            Error err -> Error err
            Success cname rem -> 
                case lit "=" rem of
                    Error _ -> Error ("Expected `=`",rem)
                    Success _ rem2 ->let mexpr = infixexpr rem2 in
                        case mexpr of
                            Error err -> Error err
                            Success cexpr rem3 -> Success (cname, cexpr) rem3

parseLine: String -> MaybeParsed Line
parseLine chars = case ifelse chars of
    Success (cond, tru, fals) rem -> Success (Ifelse cond tru fals) rem
    Error err -> 
        if (Tuple.first err) /= "Expected Ifelse" then 
            Error err 
        else 
            case assign chars of
                Success (vname, mxpr) rem2 -> Success (Asnment vname mxpr) rem2
                Error err2 -> Error err2

parseProgram: String -> MaybeParsed (List Line)
parseProgram chars = parseProgram_ chars []
parseProgram_ chars list =
    case String.isEmpty (String.trimRight chars) of
        True -> Success (List.reverse list) ""
        False -> case parseLine chars of
            Success ln rem -> parseProgram_ rem (ln :: list)
            Error err -> Error err


--- PRINTING/TOSTRING FUNCTIONS ---

printMathexpr: MathExpr -> String
printMathexpr mxpr = 
    case mxpr of 
        MathOp op expr1 expr2 -> "(" ++ (printMathexpr expr1) ++ (printOp op) ++ (printMathexpr expr2) ++ ")"
        ValInt val -> String.fromInt val
        ValBool val -> case val of
            True -> "True"
            False -> "False"
        Name nme -> nme
        FuncCall fname args -> fname ++ "(" ++ List.foldl (++) "" (List.intersperse ", " (List.map printMathexpr args)) ++ ")"
        {-Ifelse condition truval falseval ->
            "if (" ++ printMathexpr condition ++ ") {" ++ printMathexpr truval ++ "} else {" ++ printMathexpr falseval ++ "}"-} --Disabled Inline If


pasnnl asn stuff = stuff ++ printAssign asn ++ "\n"

printType: MType -> String
printType t =
    case t of
        MInt -> "Int"
        MBool -> "Bool"
        MA num -> "Any (" ++ String.fromInt num ++ ")"
        MFunc args return -> (foldl (++) "" (List.intersperse ", " (map printType args))) ++ " -> " ++ printType return

printOp: Op -> String
printOp t =
    case t of
        Mul  -> "*"
        Div  -> "/"
        Mod  -> "%"
        Add  -> "+"
        Sub  -> "-"
--        Shl  -> ">>"
--        Shr  -> "<<"
        Llt  -> "<"
        Lle  -> "<="
        Lgt  -> ">"
        Lge  -> ">="
        Equ  -> "==="
        Neq  -> "!=="
        Band -> "&"
        Bor  -> "|"
        Land -> "&&"
        Lor  -> "||"

printAssign assn = Tuple.first assn ++ " = " ++ printMathexpr (Tuple.second assn)