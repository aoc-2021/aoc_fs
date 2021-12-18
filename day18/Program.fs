open System.IO

let file = File.ReadAllLines "input.txt"

type Num =
    | Const of int64
    | Pair of Num * Num

type Token =
    | TConst of int64
    | TStart
    | TEnd
    | TComma

let isNumber (c: char) = c >= '0' && c <= '9'
let digitValue (c: char) = (int64 c) - (int64 '0')

let rec reduceNums (num: List<Token>) : list<Token> =
    match num with
    | [] -> []
    | TConst d1 :: TConst d2 :: rest -> (TConst(d1 * 10L + d2)) :: rest |> reduceNums
    | other :: rest -> other :: reduceNums rest

let rec tokenizeChars (num: List<char>) : list<Token> =
    match num with
    | [] -> []
    | d1 :: rest when isNumber d1 -> (TConst(digitValue d1)) :: tokenizeChars rest
    | '[' :: rest -> TStart :: (tokenizeChars rest)
    | ']' :: rest -> TEnd :: (tokenizeChars rest)
    | ',' :: rest -> TComma :: (tokenizeChars rest)

let rec parseTokens (tokens: list<Token>) : Num * list<Token> =
    match tokens with
    | TConst n :: rest -> Const n, rest
    | TStart :: rest ->
        let first, rest = parseTokens rest

        if rest.Head <> TComma then
            failwith $"Syntax error, expected comma but was {rest}"

        let second, rest = parseTokens rest.Tail

        if rest.Head <> TEnd then
            failwith $"Syntax error, expected ] but was {rest}"

        Pair(first, second), rest.Tail

let parseString (num: string) : Num =
    let tokens =
        num.ToCharArray()
        |> Array.toList
        |> tokenizeChars
        |> reduceNums

    let num, rest = parseTokens tokens
    num

let fileNums =
    file |> Array.map parseString |> Array.toList

printfn $"tokens={fileNums.Head}"


printfn $"{fileNums}"

let explode (num: Num) : Num =
//    printfn $"explode {num}"
    num

let rec addExplodedLeft (num: Num) (value: int64) : Num * int64 =
    //    printfn $"Adding {value} to {num}"
    match num with
    | Const v -> Const(v + value), 0L
    | Pair (a, b) ->
        let a, value = addExplodedLeft a value
        let b, value = addExplodedLeft b value
        Pair(a, b), value

let rec addExplodedRight (num: Num) (value: int64) : Num * int64 =
//    printfn $"Adding {value} to {num}"

    match num with
    | Const v -> Const(v + value), 0L
    | Pair (a, b) ->
        let b, value = addExplodedRight b value
        let a, value = addExplodedRight a value
        Pair(a, b), value

let rec applyExplode (num: Num) (depth: int) : Option<int64 * int64> * Num =
    match num, depth with
    | Const n, _ -> None, Const n
    | Pair (Const c1, Const c2), 4 -> Some((c1, c2)), Const 0L
    | Pair (n1, n2), depth when depth < 4 ->
        match applyExplode n1 (depth + 1) with
        | Some (v1, v2), n1 ->
            let n2, v2 = addExplodedLeft n2 v2
            Some(v1, v2), Pair(n1, n2)
        | None, n1 ->
            match applyExplode n2 (depth + 1) with
            | Some (v1, v2), n2 ->
                let n1, v1 = addExplodedRight n1 v1
                Some(v1, v2), Pair(n1, n2)
            | None, n2 -> None, Pair(n1, n2)
    | _ -> failwith $"Not handled: {num} depth={depth}"


let test1 =
    applyExplode (parseString "[[[[[9,8],1],2],3],4]") 0

printfn $"test1 = {test1}"

let test2 =
    applyExplode (parseString "[7,[6,[5,[4,[3,2]]]]]") 0

printfn $"test2 = {test2}"

let test3 =
    applyExplode (parseString "[[6,[5,[4,[3,2]]]],1]") 0

printfn $"test3 = {test3}"

let test4 =
    applyExplode (parseString "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") 0

printfn $"test4 = {test4}"

let test5 =
    applyExplode (parseString "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") 0

printfn $"test5 = {test5}"

// let test1 = add (parseString "[[[[4,3],4],4],[7,[[8,4],9]]]") (parseString "[1,1]")

let splitNum (n: int64) =
    if n % 2L = 1L then
        ((n - 1L) / 2L, (n + 1L) / 2L)
    else
        (n / 2L, n / 2L)

printfn $"split 10 = {splitNum 10}"
printfn $"split 11 = {splitNum 11}"

let rec singleSplit (num: Num) : bool * Num =
    match num with
    | Const c when c > 9 ->
//       printfn $"Splitting: {num}"
        let (c1, c2) = splitNum c
        true, Pair(Const c1, Const c2)
    | Const c -> false, Const c
    | Pair (n1, n2) ->
        match singleSplit n1 with
        | true, n1 -> true, Pair(n1, n2)
        | false, _ ->
            let res, n2 = singleSplit n2
            res, Pair(n1, n2)



let rec reduce (num: Num) : Num =
//    printfn $"Reducing {num}"

    match applyExplode num 0 with
    | Some (_), num -> reduce num
    | None, _ ->
        match singleSplit num with
        | true, num -> reduce num
        | false, num -> num

let add (num1: Num) (num2: Num) =
    let num = Pair(num1, num2)
    reduce num

let add1 =
    add (parseString "[[[[4,3],4],4],[7,[[8,4],9]]]") (parseString "[1,1]")

printfn $"add1 = {add1}"

let fileSeed = fileNums.Head
let fileRest = fileNums.Tail

let rec magnitude (num: Num) : int64 =
    match num with
    | Const n -> n
    | Pair (a, b) ->
        let magA = magnitude a
        let magB = magnitude b
        magA * 3L + magB * 2L

let m1 =
    magnitude (parseString "[[1,2],[[3,4],5]]")

printfn $"m1 = {m1}"

let m6 =
    magnitude (parseString "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")

printfn $"m6 = {m6}"

// let fileRes = fileRest |> List.fold add fileSeed
// printfn $"fileRes {fileRes}"

let example =
    [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]";
       "[[[5,[2,8]],4],[5,[[9,9],0]]]";
       "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]";
       "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]";
       "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]";
       "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]";
       "[[[[5,4],[7,7]],8],[[8,3],8]]";
       "[[9,3],[[9,9],[6,[4,9]]]]";
       "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]";
       "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" ]
    
let process (input:list<string>) =
    let input = input |> List.map parseString 
    let seed = input.Head
    let rest = input.Tail
    let result = rest |> List.fold add seed
    printf $"result = {result}"
    let m = magnitude result
    printf $"magnitude = {m}"

process example

process (file |> Array.toList)

let bestPair (input:list<string>):int64 =
    let input = input |> List.map parseString 
    Seq.allPairs (input |> List.toSeq) (input |> List.toSeq)
    |> Seq.toList
    |> List.filter (fun (a,b) -> a <> b)
    |> List.map (fun (a,b) -> add a b)
    |> List.map magnitude
    |> List.max 

printfn $"bestPair example= {bestPair example}" 
printfn $"bestPair input = {bestPair (file |> Array.toList)}" 
