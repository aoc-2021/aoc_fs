open System.IO

let file = File.ReadAllLines "input.txt"

type Num =
    | Const of int64
    | Pair of Num*Num 

type Token =
    | TConst of int64
    | TStart
    | TEnd
    | TComma  

let isNumber (c: char) = c >= '0' && c <= '9'
let digitValue (c:char) = (int64 c) - (int64 '0')

let rec reduceNums (num:List<Token>) : list<Token> =
    match num with
    | [] -> []
    | TConst d1 :: TConst d2 :: rest -> (TConst (d1*10L + d2)):: rest |> reduceNums
    | other :: rest -> other :: reduceNums rest 

let rec tokenizeChars (num:List<char>) : list<Token> =
    match num with
    | [] -> []
    | d1::rest when isNumber d1 -> (TConst (digitValue d1)) :: tokenizeChars rest
    | '['::rest -> TStart :: (tokenizeChars rest)
    | ']'::rest -> TEnd :: (tokenizeChars rest)
    | ','::rest -> TComma :: (tokenizeChars rest)

let parseString (num:string) =
    num.ToCharArray() |> Array.toList |> tokenizeChars |> reduceNums 

let tokens = file |> Array.map parseString |> Array.toList 
printfn $"tokens={tokens.Head}" 

let rec parseTokens (tokens: list<Token>) : Num*list<Token> =
    match tokens with
    | TConst n :: rest -> Const n,rest
    | TStart :: rest ->
        let first,rest = parseTokens rest
        if rest.Head <> TComma then
            failwith $"Syntax error, expected comma but was {rest}"
        let second,rest = parseTokens rest.Tail
        if rest.Head <> TEnd then
            failwith $"Syntax error, expected ] but was {rest}"
        Pair (first,second),rest.Tail 

let parsed = tokens |> List.map parseTokens

printfn $"{parsed}" 
    