module Main exposing (..)

import Browser
import Html exposing (Html, button, div, textarea, text, br)
import Html.Attributes exposing (readonly)
import Html.Events exposing (onClick, onInput)

import List.Extra exposing (find)


--- Browser Stuff ---

main =
  Browser.sandbox { init = initialModel, update = update, view = view }

initialModel = 
    { input = ""
    , output = ""
    }

view model =
  div []
    [ textarea [ onInput UpdateInput] [ text initialModel.input ]
    , button   [ onClick Compile ]    [ text "Compile"]
    , div      [ ]                    (List.intersperse (br [] []) (List.map text (String.lines model.output)))
    ]

type Msg = Compile | UpdateInput String

update msg model =
  case msg of
    UpdateInput newInput ->  {model | input=newInput}
    Compile -> 
        let 
            maybeParsed = assign model.input
            result = case maybeParsed of
                Success val _ ->  case validateAssign val of
                    Ok t -> "Success, returns type " ++ printType t ++ "\n(Result: " ++ printAssign val ++ ")"
                    Err e -> "TypeError: " ++ e
                Error val  -> "Error: " ++ (Tuple.first val) ++ " Around `" ++ (Tuple.second val) ++ "`"
        in
            {model | output = result}




 --- Basic parsing stuff ---

type MaybeParsed a 
    = Success a      String   --Whatever we parsed and the rest of the input
    | Error   (String, String) -- An error message, and the rest of the input


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

 --- Parsing! --

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


type MathExpr
    = More MathExpr
    | ValInt Int
    | ValBool Bool
    | MathOp Op MathExpr MathExpr

type Op 
    = Div | Mul | Mod
    | Add | Sub 
    | Shl | Shr
    | Llt | Lle | Lgt | Lge
    | Equ | Neq
    | Band
    | Bor
    | Land
    | Lor

type MType
    = MBool
    | MInt
    | MA Int -- Type variable- Int denotes which one

printMathexpr: MathExpr -> String
printMathexpr mxpr = 
    case mxpr of 
        More val -> "(" ++ (printMathexpr val ) ++ ")"
        MathOp op expr1 expr2 -> "(" ++ (printMathexpr expr1) ++ (printOp op) ++ (printMathexpr expr2) ++ ")"
        ValInt val -> String.fromInt val
        ValBool val -> case val of
            True -> "True"
            False -> "False"

printType: MType -> String
printType t =
    case t of
        MBool -> "Bool"
        MInt -> "Int"
        MA num -> "Any (" ++ String.fromInt num ++ ")"

printOp: Op -> String
printOp t =
    case t of
        Mul  -> "Mul"
        Div  -> "Div"
        Mod  -> "Mod"
        Add  -> "Add"
        Sub  -> "Sub"
        Shl  -> "Shl"
        Shr  -> "Shr"
        Llt  -> "Llt"
        Lle  -> "Lle"
        Lgt  -> "Lgt"
        Lge  -> "Lge"
        Equ  -> "Equ"
        Neq  -> "Neq"
        Band -> "Band"
        Bor  -> "Bor"
        Land -> "Land"
        Lor  -> "Lor"
    

mathexpr: String -> MaybeParsed MathExpr
mathexpr chars = 
    case lit "(" chars of --Attempt to parse (
        Success _ rem -> case infixexpr rem of --Attempt to parse subexpr
            Success expr rem2 -> case lit ")" rem2 of -- Attempt to parse )
                Success _ rem3 -> Success (More expr) rem3
                Error err -> Error err -- There always has to be a closing parenth 
            Error err -> Error err -- Parentheses must have content
        Error _ -> case number chars of 
            Success num rem4 -> Success (ValInt num) rem4
            Error _ -> case bool chars of
                Success val rem5 -> Success (ValBool val) rem5
                Error err -> Error ("Expected Mathexpr", Tuple.second err)


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
    , (False,
      [ (Shl, "<<")
      , (Shr, ">>")
      ])
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

sigs = 
    [ (Lor, ((MBool, MBool), MBool))
    , (Land, ((MBool, MBool), MBool))
    , (Bor, ((MInt, MInt), MInt))
    , (Band, ((MInt, MInt), MInt))
    , (Equ, ((MA 1, MA 1), MBool))
    , (Neq, ((MA 1, MA 1), MBool))
    , (Llt, ((MInt, MInt), MBool))
    , (Lle, ((MInt, MInt), MBool))
    , (Lgt, ((MInt, MInt), MBool))
    , (Lge, ((MInt, MInt), MBool))
    , (Shl, ((MInt, MInt), MInt))
    , (Shr, ((MInt, MInt), MInt))
    , (Add, ((MInt, MInt), MInt))
    , (Sub, ((MInt, MInt), MInt))
    , (Div, ((MInt, MInt), MInt))
    , (Mul, ((MInt, MInt), MInt))
    , (Mod, ((MInt, MInt), MInt))
    ]

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


-- Attempts to parse any operator in the current precedence level
infixop: List (Op, String) -> String -> MaybeParsed Op
infixop levels chars =
    case levels of 
        [] -> Error ("Expected Infix Operator", chars)
        val :: rest ->
            case lit ( Tuple.second val) chars of --Try to parse op
                Success _ rem -> Success (Tuple.first val) rem
                Error _ -> infixop rest chars -- Try to parse next op

 ---- VALIDATORS ----
validate: MathExpr -> Result String MType
validate mxpr = 
    case mxpr of 
        More smxpr -> validate smxpr
        ValBool _ -> Ok MBool
        ValInt _ -> Ok MInt
        MathOp op t1 t2 -> 
            let 
                sig = find (\a -> (Tuple.first a) == op) sigs
            in case sig of
                Nothing -> Err "Invalid operator, or op signature is missing!"
                Just (_,((a,b),c)) ->
                    let 
                        vt1res = validate t1
                        vt2res = validate t2
                    in case vt1res of 
                     Err e -> Err e
                     Ok vt1 -> case vt2res of
                      Err e -> Err e
                      Ok vt2 ->
                        let 
                            a_ = case a of 
                                MA _ -> vt1 
                                _ -> a
                            b_ = case b of 
                                MA bnum -> case a of
                                    MA anum -> if anum == bnum then a_ else b
                                    _ -> b
                                _ -> b
                        in
                          if (a_ == vt1 && b_ == vt2) then
                              Ok c
                          else
                              Err ("Invalid params for Op `" ++ printOp op ++ "`- Expected " ++ printType a_ ++ " and " ++ printType b_ ++ " but got " ++ printType vt1 ++ " and " ++ printType vt2 ++ ".")


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

printAssign assn = Tuple.first assn ++ " = " ++ printMathexpr (Tuple.second assn)
validateAssign assn = validate (Tuple.second assn)