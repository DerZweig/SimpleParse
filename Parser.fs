module Parser
open Tokenizer

type ParserResult<'t> = 
    | Result of 't
    | Error of int * string list
type ParserFunc<'t> = Text -> ParserResult<'t> * Text
type Parser<'t> = { Names : string list ; Eval : ParserFunc<'t> }
type ParserBuilder(name) =
    member self.Return(x) = {Names = [name]; Eval = fun cs -> Result(x),cs}
    member self.ReturnFrom(x) = {Names = [name]; Eval = x.Eval}
    member self.Bind(m : Tokenizer,f : (int*string) -> Parser<'a>) = {
        Names = [name]
        Eval = fun cs ->
            match m.RunWithText cs with
            | Some(r),rs -> 
                match (f (r.Line,new string[|for c in r.Value -> c|])).Eval rs with
                | Result(x),xs -> Result(x),xs
                | Error(el,es),_ -> Error(el,m.Names @ es),cs
            | _ -> Error(cs.Line,m.Names),cs
    }
    member self.Bind(p : Parser<'a>,f : 'a -> Parser<'b>) = {
        Names = [name]
        Eval = fun cs -> 
            match p.Eval cs with
            | Result(r),rs ->
                match (f r).Eval rs with
                | Result(x),xs -> Result(x),xs
                | Error(el,es),_ -> Error(el,p.Names @ es),cs
            | Error(el,es),_ -> Error(el,es),cs
    }
    member self.Zero() = {
        Names = [name]
        Eval = fun cs -> Error(cs.Line,[name]),cs
    }

let parser(name) = ParserBuilder(name)
type Parser with
    static member (<|>) (p: Parser<'t>,q : Parser<'t>) = {
        Names = (p.Names) @ (q.Names)
        Eval = fun cs -> match p.Eval cs with
                         | Result(r),rs -> Result(r),rs
                         | _,_ -> q.Eval cs
    }
    
    member self.Repeat = 
        let rec many1 (p:Parser<_>) : Parser<_> = (parser("") {
            let! r = p
            let! rs = many p
            return r::rs
        })
        and many p : Parser<_> = (many1 p) <|> ((parser("") { return [] }))
        {(many1 self) with Names = self.Names}
    member self.SeparatedBy (x:Tokenizer) = 
        let rec many1 (p:Parser<_>) : Parser<_> = (parser("") {
            let! r = p
            let! _ = x
            let! rs = many p
            return r::rs
        })
        and many p : Parser<_> = (many1 p) <|> ((parser("") { return [] }))
        let p = {(many self) with Names = self.Names}
        let p2 = parser("") {
            let! xs = p
            let! x = self
            return xs @ [x]
        }
        {p2 with Names = self.Names}
    member self.RunWithText s = self.Eval s
    member self.RunWithString s = self.RunWithText(Text.FromString s)
    