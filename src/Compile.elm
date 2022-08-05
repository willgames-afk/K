module Compile exposing (..)
import Dict exposing (Dict)
import Parse as P
--comp_x86_64_linux ast =


comp_x86_64_macos ast = 
    let 
        output = compLines ast "label" Dict.empty
    in """
    global start

    section .text
dump:
        mov     r9, -3689348814741910323
        sub     rsp, 40
        mov     BYTE [rsp+31], 10
        lea     rcx, [rsp+30]
.L2:
        mov     rax, rdi
        lea     r8, [rsp+32]
        mul     r9
        mov     rax, rdi
        sub     r8, rcx
        shr     rdx, 3
        lea     rsi, [rdx+rdx*4]
        add     rsi, rsi
        sub     rax, rsi
        add     eax, 48
        mov     BYTE [rcx], al
        mov     rax, rdi
        mov     rdi, rdx
        mov     rdx, rcx
        sub     rcx, 1
        cmp     rax, 9
        ja      .L2
        lea     rax, [rsp+32]
        mov     edi, 1
        sub     rdx, rax
        xor     eax, eax
        lea     rsi, [rsp+32+rdx]
        mov     rdx, r8
        mov     rax, 0x2000004
        syscall
        add     rsp, 40
        ret

start:
    push rbp
    mov rbp, rsp
""" ++ output ++ """

    mov rax, 0x2000004
    mov rdi, 1
    mov rsi, str
    mov rdx, str.len
    syscall

    pop rbp
    mov rax, 0x2000001 ;return
    xor rdi,rdi
    syscall
    ret ; Should be unreachable, but im putting this here anyway


    section .data
str: db "Hello world! test"
.len: equ $-str
    section .bss
"""

compLines: List P.Line -> String -> Dict String Int ->  String
compLines lines labelTracker vars = (List.foldl compLine {out="", vars=vars, labelTracker=labelTracker} lines).out

compLine: P.Line -> {out: String, vars:Dict String Int, labelTracker:String} -> {out: String, vars:Dict String Int, labelTracker:String}
compLine line c = case line of
    P.Asnment name mxpr -> 
        let 
            mxprS = compMathExpr mxpr c.vars c.labelTracker
            store = case Dict.get name c.vars of
                Nothing -> "    ;Initialized "++name++"\n" --If it hasn't been defined yet, leave it on the stack top
                Just addr -> """
    ;Store value to """++name++"""
    pop rax
    mov [rbp-""" 
                    ++String.fromInt ((addr+1)*8)++"], rax\n" --Otherwise we need to store it in correct location
        in
            {c | out = c.out ++ mxprS ++ "\n    ;Debug print\n    mov rdi, [rsp]\n    call dump\n" ++ store
            , vars = Dict.insert name (Dict.size c.vars) c.vars
            }
    P.Ifelse cond tru fals -> 
        let 
            condS = compMathExpr cond c.vars (c.labelTracker++"cond") 
            clf = compLines fals (c.labelTracker ++ "false") c.vars
            clt = compLines tru  (c.labelTracker ++ "true") c.vars
        in
            {c | out = c.out 
                ++ "\n    ;IF CONDITION"
                ++ condS
                ++ "\npop rcx\n"
                ++ "jrcxz if" ++ c.labelTracker ++ "else\n\n"
                ++ "    ;IF TRUE\n"
                ++ clt ++ "\n    jmp if" ++ c.labelTracker ++ "end\n"
                ++ "if" ++ c.labelTracker ++ "else:\n" ++ clf ++ "\n"
                ++ "if" ++ c.labelTracker ++ "end:\n"
            , labelTracker = c.labelTracker ++ "n"
            }
    P.FunctionDef name args content -> Debug.todo "Compile Functions"

-- Dict is for mapping var names to mem locs
compMathExpr: P.MathExpr -> Dict String Int -> String -> String
compMathExpr mxpr vars labelTracker = case mxpr of
    P.ValInt int ->   "\n    ; int literal\n    push " ++ (String.fromInt int) ++ "\n"
    P.ValBool bool -> "\n    ; bool literal\n    push " ++ (case bool of 
        True -> "1" 
        False -> "0") ++ "\n"
    P.MathOp op mathxpr mathxpr2 -> 
        let cdop = compOp op in
            compMathExpr mathxpr vars labelTracker++ compMathExpr mathxpr2 vars labelTracker++ cdop
    P.Name name -> let addr = Maybe.withDefault 0 (Dict.get name vars) in 
        """
    ; Fetch var """++name++"""
    push qword [rbp-"""++ String.fromInt ((addr + 1) * 8) ++"""]
"""
    P.FuncCall fname exprs -> (List.foldl (++) "" (List.map (\xpr -> compMathExpr xpr Dict.empty labelTracker) exprs)) ++ """

    call """++fname++"""
"""


    {-P.Ifelse cond truval falseval -> "\n    ;IF CONDITION" ++ compMathExpr cond vars labelTracker ++ """
    pop rcx
    jrcxz if"""++labelTracker++"else\n\n    ;IF TRUE\n" ++ compMathExpr truval vars (labelTracker ++ "sub1") ++ """
    jmp if"""++labelTracker++"""end

if"""++labelTracker++"else:\n" ++ compMathExpr falseval vars (labelTracker ++ "sub2") ++ """
if"""++labelTracker++"""end:  
"""-} -- Disable inline ifelse
    --P.Block asns -> compLine asns "" vars labelTracker
    

compOp: P.Op -> String
compOp op = case op of 
    P.Mul -> """
    ;Mul
    pop rbx
    pop rax
    imul rbx
    push rax
"""
    P.Div -> """
    ;Div (Integer)
    pop rbx
    pop rax
    idiv rbx
    push rax
"""    
    P.Mod -> """
    ;Modulo (Remandier)
    pop rbx
    pop rax
    idiv rbx
    push rdx
"""
    P.Add -> """
    ;ADD
    pop rbx
    pop rax
    add rax, rbx
    push rax
"""
    P.Sub -> """
    ;SUB
    pop rbx
    pop rax
    sub rax,rbx
    push rax
"""
{- Shift amount is limited to 8 bits- need to enforce that
    P.Shl -> """
    ;SHL
    pop rbx
    pop rax
    sal rax, rbx
    push rax
"""
    P.Shr -> """
    ;SHR
    pop rbx
    pop rax
    sar rax, rbx
    push rax
"""
-}
    P.Band -> """
    ;Bit And
    pop rbx
    pop rax
    and rax, rbx
    push rax
"""
    P.Bor -> """
    ;Bit Or
    pop rbx
    pop rax
    or rax, rbx
    push rax
"""
    P.Llt -> cmov "Llt" "l"
    P.Lle -> cmov "Lle" "le"
    P.Lgt -> cmov "Lgt" "g"
    P.Lge -> cmov "Lge" "ge"
    P.Equ -> cmov "Equ" "e"
    P.Neq -> cmov "Neq" "ne"
    P.Land -> """
    ; Logical And
    pop rbx
    pop rax
    and rax, rbx
    push rax
"""
    P.Lor -> """
    ; Logical Or
    pop rbx
    pop rax
    or rax, rbx
    push rax
"""


cmov n t = """
    ;"""++n++"""
    pop rbx
    pop rax
    cmp rax, rbx
    mov rax, 0
    mov rbx, 1
    cmov"""++t++""" rax, rbx
    push rax
"""