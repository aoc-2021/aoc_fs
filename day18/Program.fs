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

let parseString (num:string) : Num =
    let tokens = num.ToCharArray() |> Array.toList |> tokenizeChars |> reduceNums
    let num,rest = parseTokens tokens
    num 

let fileNums = file |> Array.map parseString |> Array.toList 
printfn $"tokens={fileNums.Head}" 


printfn $"{fileNums}"

let explode (num:Num) : Num =
    printfn $"explode {num}"
    num

let addExploded (num:Num) (value:int64) : Num =
    printfn $"Adding {value} to {num}"
    match num with
    | Const n -> Const (n+value)
    | _ -> failwith $"Not implemented: adding to tree: {num} {value}"

let rec applyExplode (num:Num) (depth:int) : bool*Num =
    match num,depth with
    | Const n,_ -> false,Const n
    | Pair (Pair (n1,Const c2),right),3 ->
        true,(Pair(Const 0,addExploded right c2))
    | Pair (left,Pair (Const c1,n2)),3 ->
        true,Pair(addExploded left c1,Const 0)
    | Pair (n1,n2),depth ->
        let res1,n1 = applyExplode n1 (depth+1)
        let res2,n2 = applyExplode n2 (depth+1)
        res1||res2,Pair(n1,n2)
        

let rec reduce (num:Num) =
   printfn $"Reducing {num}"
   match applyExplode num 0 with
   | true,num -> reduce num
   | false,_ -> num 

let add (num1:Num) (num2:Num) =
    let num = Pair (num1,num2)
    reduce num 

let test1 = applyExplode (parseString "[[[[[9,8],1],2],3],4]") 0

printfn $"test1 = {test1}"

// let test1 = add (parseString "[[[[4,3],4],4],[7,[[8,4],9]]]") (parseString "[1,1]")
