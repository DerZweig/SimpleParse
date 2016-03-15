module Tokenizer

type Text = { Line : int; Chars: char list }
type Token = { Name: string; Line : int; Value : char list }
type TokenFunc = Text -> Token option * Text
type Tokenizer = { Names : string list; Eval : TokenFunc }
type Text with 
    member self.IsEmpty = self.Chars.IsEmpty
    static member FromString s = {Line = 1; Chars = [for c in s -> c]}
type Token with
    static member JustValue s = {Name = ""; Line = 0; Value = s}
let private move1 (cs:Text) = function
    | '\n' -> {cs with Line = cs.Line + 1}
    | _ -> cs
type TokenBuilder(name) = 
    member self.ReturnFrom(x: TokenFunc) = {
        Names = [name]
        Eval = fun cs -> match x cs with
                         | Some(r),rs -> Some({r with Line = cs.Line; Name = name}),rs
                         | _ -> None,cs}
    member self.ReturnFrom(p : char -> bool) = 
        self.ReturnFrom(fun cs ->
            match cs.Chars with
            | r::rs when p r -> Some(Token.JustValue([r])),move1 {cs with Chars = rs} r
            | _ -> None,cs)
    member self.Bind(m : Tokenizer,f) = self.ReturnFrom(fun cs -> 
        match m.Eval cs with
        | Some(r),rs -> 
            let t2 = f(new string [|for c in r.Value -> c|])
            match t2.Eval rs with
            | Some(r2),r2s -> Some(Token.JustValue(r2.Value)),r2s
            | _ -> None,cs
        | _ -> None,cs)
    member self.Zero() = self.ReturnFrom(fun cs -> None,cs)
    member self.Return(c : char list) = self.ReturnFrom(fun cs -> Some(Token.JustValue(c)),cs)
    member self.Return(s : char seq) = self.Return([for c in s -> c])
    member self.Bind(m : char->bool,f) = self.Bind(self.ReturnFrom m,f)
    member self.ReturnFrom(txt: char seq) =
        let rec text = function
            | [] -> self.Return []
            | x::xs -> TokenBuilder(name) {
                let! _ = (=) x
                let! _ = text xs
                return x::xs}
        text [for c in txt -> c]
    member self.Bind(m : char seq,f) = self.Bind(self.ReturnFrom m,f)
let token(name) = TokenBuilder(name)
type Tokenizer with
    static member (<|>) (p: Tokenizer,q : Tokenizer) = {
        Names = (p.Names) @ (q.Names)
        Eval = fun cs -> match p.Eval cs with
                         | None,_ -> q.Eval cs
                         | r,rs -> r,rs }
    member self.OrNothing = self <|> ({token("") { return []} with Names = self.Names})
    member self.Repeat = 
        let rec many1 (p:Tokenizer) : Tokenizer = (token("") {
            let! r = p
            let! rs = many p
            return r+rs
        })
        and many p : Tokenizer = (many1 p) <|> ((token("") { return [] }))
        {(many1 self) with Names = self.Names}
    member self.RepeatUntil (x:Tokenizer) = 
        let rec until (p:Tokenizer) : Tokenizer = p <|>  (token("") {
            let! s = self
            let! r = until p
            return s + r
        })
        {(until x) with Names = self.Names}
        
    member self.RunWithText s = self.Eval s
    member self.RunWithString s = self.RunWithText (Text.FromString s)