module Main exposing (main)

import List.Extra exposing (find)
import Dict exposing (Dict)

import Browser
import Html exposing (Html, button, div, textarea, text, br, a)
import Html.Attributes exposing (readonly, href, download)
import Html.Events exposing (onClick, onInput)

import Parse as P
import Compile as C

import Base64

--Fold 2 lists at once (to the left)

fold2l:(a -> b -> c -> c) -> c -> List a -> List b -> c
fold2l func acc inputA inputB = case inputA of
    [] -> acc
    a :: axs -> case inputB of
        [] -> acc
        b :: bxs ->
            fold2l func (func a b acc) axs bxs
        


--- Browser Stuff ---

main =
  Browser.sandbox { init = initialModel, update = update, view = view }

initialModel = 
    { input = """a = 10
b = a + 10
if (b > a) {
    a = b
} else {
    b = a
}

"""
    , output = ""
    , download = ""
    }

view model = 
    let 
        mostOfIt = 
            [ textarea [ onInput UpdateInput] [ text model.input ]
            , button   [ onClick Compile ]    [ text "Compile"]
            , div      [ ]                    (List.intersperse (br [] []) (List.map text (String.lines model.output)))
            ]
    in
        div [] (if (String.length model.download > 0) then
            List.append [(a [ href ("data:text/plain;base64," ++ (Base64.encode model.download)), download "prog.asm"] [text "Download Assembly"])] mostOfIt
        else
            mostOfIt)

type Msg = Compile | UpdateInput String

update msg model =
  case msg of
    UpdateInput newInput ->  {model | input=newInput}
    Compile -> 
        let 
            maybeParsed = P.parseProgram model.input
            result = case maybeParsed of
                P.Success val _ -> case validateProg val Dict.empty of 
                    Ok typelist -> ("Valid!\n" ++ List.foldl printVres "" typelist ++ "\nMacOS Assembly:" ++ C.comp_x86_64_macos val, C.comp_x86_64_macos val)
                    Err err -> (err, "")
                P.Error val  -> ("ParseError: " ++ (Tuple.first val) ++ " Around `" ++ String.slice 0 30 (Tuple.second val) ++ "`","")
        in
            {model | output = Tuple.first result, download = Tuple.second result}


type ValidateRes 
    = VAsnment String P.MType
    | VIfelse (List ValidateRes) (List ValidateRes)
    | VFunction String (List P.MType) P.MType

type alias SymbolMap = Dict String P.MType

type alias VContext = 
    { symbols: SymbolMap 
    , result: Result String (List ValidateRes)
    }

printVres: ValidateRes -> String -> String
printVres vres outstr= case vres of
    VAsnment name typ -> outstr ++ name ++ ": " ++ P.printType typ ++ "\n"
    VIfelse l1 l2 -> outstr ++ "If true {\n" ++ List.foldl printVres "" l1 ++ "\n}\nIf false {\n" ++ List.foldl printVres "" l2 ++ "\n}\n"

validateProg:  List P.Line -> SymbolMap -> Result String (List ValidateRes) --Bool is whether its valid or not, string is type or error
validateProg input symbols = (List.foldl validateProg_ {symbols=symbols, result= Ok []} input).result

validateProg_: P.Line -> VContext -> VContext
validateProg_ input context =  -- Result String or MType
    case validateLine input context.symbols of
        Err text -> {context | result = case context.result of 
                        Err oldErr -> Err (oldErr ++ "TypeError: " ++ text ++ "\n")
                        Ok _ -> Err ("TypeError: " ++ text ++ "\n")
                    } 
        Ok ln -> case ln of
            VAsnment name typ -> {context | symbols = (Dict.insert name typ context.symbols)
                                  , result = case context.result of
                                        Ok prev -> Ok (prev ++ [ln])
                                        Err err -> Err err
                                  }
            VIfelse _ _ -> {context | result = case context.result of
                                  Ok prev -> Ok (prev ++ [ln])
                                  Err err -> Err err
                              }
            VFunction name args return -> {context | symbols = (Dict.insert name (P.MFunc args return) context.symbols)
                                          , result = case context.result of 
                                                Ok prev -> Ok (prev ++ [ln])
                                                Err err -> Err err
                                          }


 ---- VALIDATORS ----

validateLine: P.Line -> SymbolMap -> Result String ValidateRes
validateLine line symbols = case line of
    P.Asnment name mxpr -> case validate mxpr symbols of
        Ok typ -> Ok (VAsnment name typ)
        Err err -> Err err
    P.Ifelse cond tru fals-> validateIfelse cond tru fals symbols
    P.FunctionDef name args block -> 
        let 
            dummyArgTypes = Dict.fromList (List.map2 (\n typnum->(n, P.MA typ)) args (List.range 0 ((List.length args) - 1)))
            funcs = Dict.filter (\_ val->case val of
                P.MFunc _ -> True
                _ -> False)

            vBlock = (List.foldl validateProg_ {symbols=Dict.union dummyArgTypes funcs, result=Ok []} block)
            exprs = raw.output
            bSymbols = raw.symbols
            argTypes = Dict.filter
        in
            VFunction name argTypes exprs


checkFuncArgTypes: SymbolMap -> String -> Result String (List P.MType)-> Result String (List P.MType)
checkFuncArgTypes symbols argname out = case out of
    Err _ -> out
    Ok l -> case Dict.get argname symbols of
        Nothing -> Err "Argument has dissapeared- Error with compiler" --Should be impossible as the args are added to this list
        Just a -> Ok (l ++ [a])
            

validateIfelse cond tru fals symbols = case validate cond symbols of
    Err err -> Err err
    Ok typ -> 
        if typ /= P.MBool then 
            Err "If condition must return a boolean"
        else
            let 
                vtru = validateProg tru symbols
                vfals = validateProg fals symbols
            in
                case vtru of 
                    Err err2 -> Err err2
                    Ok vvtru -> case vfals of
                        Err err3 -> Err err3
                        Ok vvfals -> Ok (VIfelse vvtru vvfals)

validate: P.MathExpr -> SymbolMap -> Result String P.MType --Validates Mathexprs.
validate mxpr symbols = 
    case mxpr of 
        P.ValBool _ -> Ok P.MBool
        P.ValInt _ -> Ok P.MInt
        P.MathOp op t1 t2 -> 
            let 
                sig = find (\a -> (Tuple.first a) == op) P.sigs --Fix this so it always succeds- 
            in case sig of
                Nothing -> Err "Invalid operator, or op signature is missing- Issue with the Compiler!!!"
                Just (_,((a,b),c)) ->
                    let 
                        vt1res = validate t1 symbols
                        vt2res = validate t2 symbols
                    in case vt1res of 
                     Err e -> Err e
                     Ok vt1 -> case vt2res of
                      Err e -> Err e
                      Ok vt2 ->
                        let 
                            vt1_ = case vt1 of
                                P.MA _ -> a
                                _ -> vt1
                            vt2_ = case vt2 of
                                P.MA _ -> b
                                _ -> vt2
                            a_ = case a of 
                                P.MA _ -> vt1 
                                _ -> a
                            b_ = case b of 
                                P.MA bnum -> case a of
                                    P.MA anum -> if anum == bnum then a_ else b
                                    _ -> b
                                _ -> b
                        in
                          if (a_ == vt1 && b_ == vt2) then
                              Ok c
                          else
                              Err ("Invalid params for Op `" ++ P.printOp op 
                                ++ "`- Expected " ++ P.printType a_ ++ " and " ++ P.printType b_ 
                                ++ " but got " ++ P.printType vt1 ++ " and " ++ P.printType vt2 ++ ".")
        P.Name name -> case Dict.get name symbols of 
            Nothing -> Err ("Variable `" ++ name ++ "` not found")
            Just vartype -> case vartype of 
                P.MFunc _ _ -> Err ("`" ++ name ++ "` is not a variable (type is "++ P.printType vartype ++ ")")
                _ -> Ok vartype
        P.FuncCall name args -> case Dict.get name symbols of
            Nothing -> Err ("Function `" ++ name ++ "` not found")
            Just functype -> case functype of
                P.MFunc argTs returnT -> 
                    if List.length args /= List.length argTs then
                        Err ("Incorrect number of function arguments- Expected " ++ String.fromInt (List.length argTs) ++ " but got " ++ String.fromInt (List.length args))
                    else
                        let results = List.map2 (\arg typ -> case validate arg symbols of
                                                    Err e -> Err e 
                                                    Ok argtype -> 
                                                        if typ == argtype then
                                                            Ok argtype
                                                        else
                                                            Err ("Expected argument of type `" ++ P.printType typ ++ "` but got `" ++ P.printType argtype ++ "` instead")
                                                ) args argTs in
                            case List.foldl (\arg acc-> case arg of
                                                Err a -> Err a
                                                _ -> acc
                                            ) (Ok 0) results of
                                Ok _ -> Ok returnT
                                Err err -> Err err

                _ -> Err ("`" ++ name ++ "` is not a function (type is " ++ P.printType functype ++ ")")
    

internErr = "Internal Error- Some condition that was supposed to be unreachable has been reached, contact the developer"