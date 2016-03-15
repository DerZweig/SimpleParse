# SimpleParse
The SimpleParse library is a lightweight parser generator written in F#. It contains 2 very simple builders for F# computation expressions,
which allows users to define a parser in a declarative style. 

The following code snippet shows a parser for C-Style expressions:

========================================
   Example: Expression Parser
========================================
```F#
open Tokenizer
open System
open System.Globalization
open Parser

type MyExpression                = 
    | PrimaryExpression         of MyPrimaryExpression
    | BinaryExpression          of MyExpression * MyBinaryOperator * MyExpression
    | UnaryExpression           of MyUnaryOperator * MyExpression
    | PostfixExpression         of MyExpression * MyPostfixOperator
and MyPrimaryExpression         =
    | Identifier                of string
    | IntegerLiteral            of string
    | FloatLiteral              of string
    | StringLiteral             of string
    | BoolLiteral               of string
    | Parenthesis               of MyExpression
    | Null
and MyBinaryOperator            =
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | And
    | Or
    | Equals
    | NEquals
    | Less
    | Greater
    | LEqual
    | GEqual
    | Left
    | Right
    | Add
    | Sub
    | Mul
    | Div
    | Mod
and MyUnaryOperator             = 
    | Increment
    | Decrement
    | Minus
    | Invert
    | Not
    | SizeOf
and MyPostfixOperator           = 
    | Decrement
    | Increment
    | MemberAccess              of string
    | ArrayAccess               of MyExpression
    | Call                      of MyExpression list
    
let isSpace = (['\n';'\r';'\t';' '] |> Set.ofList).Contains
let isDigit = (['0'..'9'] |> Set.ofList).Contains
let isHexDigit = ((['a'..'f'] @ ['A'..'F'] @ ['0'..'9']) |> Set.ofList).Contains
let isStrChar c = (c <> '\"') && (c <> '\\')
let isEscChar = (['\"';'\\';'/';'b';'f';'n';'r';'t'] |> Set.ofList).Contains
let space = (token("space") { return! isSpace }).Repeat
let letters = (token("letter") { return! Char.IsLetter }).Repeat
let digits = (token("digit") { return! isDigit }).Repeat
let underscores = (token("_") {return! (=) '_'}).Repeat
let hexDigits = (token("hexdigit") { return! isHexDigit }).Repeat


let pIdentifier = 
    let pa = token("identifier") {
        let! _ = space.OrNothing
        let! s1 = underscores
        let! s2 = (letters <|> digits).Repeat
        let! s3 = (letters <|> digits <|> underscores).Repeat.OrNothing
        let! _ = space.OrNothing
        return s1 + s2 + s3
    } 
    let pb = token("identifier") {
        let! _ = space.OrNothing
        let! s1 = letters
        let! s2 = (letters <|> digits <|> underscores).Repeat.OrNothing
        let! _ = space.OrNothing
        return s1 + s2
    }
    pa <|> pb

let pKeyword str = 
    let tmpToken =
        token(str) {
            let! id = pIdentifier
            return id
        } <|> token(str) {
            let! _ = space.OrNothing
            let! _ = str
            let! _ = space.OrNothing
            return str
        }
    token(str) {
        let! id = tmpToken
        if id = str then return str
    }

let pStringLiteral =
    let replaceEscChar = function 'b' -> '\b' | 'f' -> '\f' | 'n' -> '\n'
                                    | 'r' -> '\r'| 't' -> '\t' | other -> other
    let simple = token("char"){
        let! _ = (=) '\\'
        let! c = isEscChar
        return new string [|for x in c -> replaceEscChar x |]
    }
    let unicode = token("char") {
        let! _ = "\\u"
        let! d1 = isHexDigit
        let! d2 = isHexDigit
        let! d3 = isHexDigit
        let! d4 = isHexDigit
        let r =
            let s = d1 + d2 + d3 + d4
            Byte.Parse(s, Globalization.NumberStyles.HexNumber)
            |> char
        return r.ToString()
    }
    token("char sequence") {
        let! _ = pKeyword("\"")
        let! c = (unicode <|> simple <|> token("char") { return! isStrChar }).Repeat
        let! _ = pKeyword("\"")
        return c
    }

let pIntegerLiteral =
    token("integer") {
        let! i = digits
        let b,_ = UInt64.TryParse(i)
        if(b) then return i
    } <|> token("integer") {
        let! _ = pKeyword "0x" <|> pKeyword "0X"
        let! h = hexDigits
        let b,_ = UInt64.TryParse(h,NumberStyles.HexNumber,CultureInfo.InvariantCulture)
        if(b) then return ("0x"+ h)
    }

let pFloatLiteral =
    let frac = token("frac") {
        let! x = digits
        let! y = "."
        let! z = digits
        return x+y+z
    }
    let exp = token("exp") {
        let! e = pKeyword "e" <|> pKeyword "E"
        let! s = (pKeyword "+" <|> pKeyword "-").OrNothing
        let! d = digits
        return e+s+d
    }
    token("float") {
        let! f = frac
        let! e = exp.OrNothing
        let b,_ = Double.TryParse(f+e)
        if(b) then return (f+e)
    } <|> token("float") {
        let! d = digits
        let! e = exp
        let b,_ = Double.TryParse(d+e)
        if(b) then return (d+e)
    }
    
let private pMakeWithToken name (token:Tokenizer) ctor =
    parser(name) {
        let! _,v = token
        return ctor v
    }

let private pMakeWithKeyword name result =
    parser(name) {
        let! _ = pKeyword name
        return result
    }

let private pBinaryOperators = 
    pMakeWithKeyword "*" MyBinaryOperator.Mul           <|>
    pMakeWithKeyword "/" MyBinaryOperator.Div           <|>
    pMakeWithKeyword "%" MyBinaryOperator.Mod           <|>
    pMakeWithKeyword "+" MyBinaryOperator.Add           <|>
    pMakeWithKeyword "-" MyBinaryOperator.Sub           <|>
    pMakeWithKeyword "<<" MyBinaryOperator.Left         <|>
    pMakeWithKeyword ">>" MyBinaryOperator.Right        <|>
    pMakeWithKeyword ">=" MyBinaryOperator.GEqual       <|>
    pMakeWithKeyword "<=" MyBinaryOperator.LEqual       <|>
    pMakeWithKeyword "<"  MyBinaryOperator.Less         <|>
    pMakeWithKeyword ">"  MyBinaryOperator.Greater      <|>
    pMakeWithKeyword "==" MyBinaryOperator.Equals       <|>
    pMakeWithKeyword "!=" MyBinaryOperator.NEquals      <|>
    pMakeWithKeyword "&" MyBinaryOperator.BitwiseAnd    <|>
    pMakeWithKeyword "^" MyBinaryOperator.BitwiseXor    <|>
    pMakeWithKeyword "|" MyBinaryOperator.BitwiseOr     <|>
    pMakeWithKeyword "&&" MyBinaryOperator.And          <|>
    pMakeWithKeyword "||" MyBinaryOperator.Or 

let private pBinaryOperatorWeight = function
    | MyBinaryOperator.Mul          -> 1000
    | MyBinaryOperator.Div          -> 1000
    | MyBinaryOperator.Mod          -> 1000
    | MyBinaryOperator.Add          -> 2000
    | MyBinaryOperator.Sub          -> 2000
    | MyBinaryOperator.Left         -> 3000
    | MyBinaryOperator.Right        -> 3000
    | MyBinaryOperator.GEqual       -> 4000
    | MyBinaryOperator.LEqual       -> 4000
    | MyBinaryOperator.Less         -> 5000
    | MyBinaryOperator.Greater      -> 5000
    | MyBinaryOperator.Equals       -> 6000
    | MyBinaryOperator.NEquals      -> 6000
    | MyBinaryOperator.BitwiseAnd   -> 7000
    | MyBinaryOperator.BitwiseXor   -> 7500
    | MyBinaryOperator.BitwiseOr    -> 8000
    | MyBinaryOperator.And          -> 8000
    | MyBinaryOperator.Or           -> 8500

let rec private pBinaryOperatorShunt opa exa = function
    | MyExpression.BinaryExpression(exba,opb,exbb) as exb ->
        if((pBinaryOperatorWeight opa) < (pBinaryOperatorWeight opb)) then
            let exc = MyExpression.BinaryExpression(exa,opa,exba)
            pBinaryOperatorShunt opb exc exbb
        else
            MyExpression.BinaryExpression(exa,opa,exb)
    | exb -> MyExpression.BinaryExpression(exa,opa,exb)

#nowarn "40" //disable warning, recursive object construction
let rec parseExpression : Parser<MyExpression> = 
     parseUnaryExpression 
and parseBinaryExpression(prev) : Parser<MyExpression> =
    parser("binary expression") {
        let! op = pBinaryOperators
        let! exp = parseExpression
        return! parseBinaryExpression(pBinaryOperatorShunt op prev exp)
    } <|> parser("expression") { return prev }
and parsePostfixExpression(prev) : Parser<MyExpression> = 
    let pPostfix =
        pMakeWithKeyword "++" MyPostfixOperator.Increment <|>
        pMakeWithKeyword "--" MyPostfixOperator.Decrement <|>
        parser("member access") {
            let! _ = pKeyword "."
            let! _,id = pIdentifier
            return MyPostfixOperator.MemberAccess id
        } <|> parser("array access") {
            let! _ = pKeyword "["
            let! exp = parseExpression
            let! _ = pKeyword "]"
            return MyPostfixOperator.ArrayAccess exp
        } <|> parser("call") {
            let! _ = pKeyword "("
            let! exps = parseExpression.SeparatedBy(pKeyword ",")
            let! _ = pKeyword ")"
            return MyPostfixOperator.Call(exps)
        }
    parser("postfix expression") {
        let! op = pPostfix
        return MyExpression.PostfixExpression(prev,op)
    } <|> parser("expression") {  return prev }
and parseUnaryExpression = 
    let pUnary =
        pMakeWithKeyword "++" MyUnaryOperator.Increment     <|>
        pMakeWithKeyword "--" MyUnaryOperator.Decrement     <|>
        pMakeWithKeyword "-" MyUnaryOperator.Minus          <|>
        pMakeWithKeyword "~" MyUnaryOperator.Invert         <|>
        pMakeWithKeyword "!" MyUnaryOperator.Not            <|>
        pMakeWithKeyword "sizeof" MyUnaryOperator.SizeOf
    parser("unary expression") {
        let! op = pUnary
        let! exp = parsePrimaryExpression
        return! parseBinaryExpression(MyExpression.UnaryExpression (op,exp))
    } <|> parser("expression") {  
        let! exp = parsePrimaryExpression
        return! parseBinaryExpression exp
    }
and parsePrimaryExpression =
    let pPrimary = 
        pMakeWithKeyword "null" MyPrimaryExpression.Null                            <|>
        pMakeWithKeyword "true" (MyPrimaryExpression.BoolLiteral "true")            <|>
        pMakeWithKeyword "false" (MyPrimaryExpression.BoolLiteral "false")          <|>
        pMakeWithToken "string" pStringLiteral MyPrimaryExpression.StringLiteral    <|>
        pMakeWithToken "float" pFloatLiteral MyPrimaryExpression.FloatLiteral       <|>
        pMakeWithToken "int" pIntegerLiteral MyPrimaryExpression.IntegerLiteral     <|>
        pMakeWithToken "identifier" pIdentifier MyPrimaryExpression.Identifier      <|>
        parser("parenthesis") {
            let! _ = pKeyword "("
            let! ex = parseExpression
            let! _ = pKeyword ")"
            return MyPrimaryExpression.Parenthesis ex
        }
    parser("primary expression") {
        let! x = pPrimary
        return! parsePostfixExpression(MyExpression.PrimaryExpression x)
    }
    
```    
    
