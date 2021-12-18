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

let rec addExplodedLeft (num:Num) (value:int64) : Num*int64 =
//    printfn $"Adding {value} to {num}"
    match num with
    | Const v -> Const (v+value),0L
    | Pair (a,b) ->
        let a,value = addExplodedLeft a value
        let b,value = addExplodedLeft b value
        Pair (a,b),value 

let rec addExplodedRight (num:Num) (value:int64) : Num*int64 =
    printfn $"Adding {value} to {num}"
    match num with
    | Const v -> Const (v+value),0L
    | Pair (a,b) ->
        let b,value = addExplodedRight b value
        let a,value = addExplodedRight a value
        Pair (a,b),value 

let rec applyExplode (num:Num) (depth:int) : Option<int64*int64>*Num =
    match num,depth with
    | Const n,_ -> None,Const n
    | Pair (Const c1,Const c2),4 -> Some((c1,c2)),Const 0L
    | Pair (n1,n2),depth when depth < 4 ->
        match applyExplode n1 (depth+1) with
        | Some (v1,v2),n1 ->
            let n2,v2 = addExplodedLeft n2 v2
            Some (v1,v2),Pair(n1,n2) 
        | None,n1 ->
            match applyExplode n2 (depth+1) with
            | Some (v1,v2),n2 ->
                let n1,v1 = addExplodedRight n1 v1
                Some (v1,v2),Pair(n1,n2)
            | None,n2 ->
                None,Pair(n1,n2)
    | _ -> failwith $"Not handled: {num} depth={depth}" 

let rec reduce (num:Num) =
   printfn $"Reducing {num}"
   match applyExplode num 0 with
   | Some (_),num -> reduce num
   | None,_ -> num 

let add (num1:Num) (num2:Num) =
    let num = Pair (num1,num2)
    reduce num 

let test1 = applyExplode (parseString "[[[[[9,8],1],2],3],4]") 0
printfn $"test1 = {test1}"

let test2 = applyExplode (parseString "[7,[6,[5,[4,[3,2]]]]]") 0
printfn $"test2 = {test2}"

let test3 = applyExplode (parseString "[[6,[5,[4,[3,2]]]],1]") 0
printfn $"test3 = {test3}"

let test4 = applyExplode (parseString "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") 0
printfn $"test4 = {test4}"

let test5 = applyExplode (parseString "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") 0
printfn $"test5 = {test5}"

// let test1 = add (parseString "[[[[4,3],4],4],[7,[[8,4],9]]]") (parseString "[1,1]")

let splitNum (n:int64) =
    if n % 2L = 1L then
        ((n-1L)/2L,(n+1L)/2L)
    else
        (n/2L,n/2L)

printfn $"split 10 = {splitNum 10}"
printfn $"split 11 = {splitNum 11}"
