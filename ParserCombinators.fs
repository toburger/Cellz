module internal Cellz.ParserCombinators

let ( ||| ) p1 p2 s =
    match p1 s with
    | Some _ as v -> v
    | None -> p2 s

let ( ++ ) p1 p2 s =
    match p1 s with
    | None -> None
    | Some(e1, s) ->
        match p2 s with
        | None -> None
        | Some(e2, s) -> Some((e1, e2), s)

let rec many' p s =
    match p s with
    | None -> [], s
    | Some(e, s) ->
        let es, s = many' p s
        e::es, s

let many p s = Some(many' p s)

let some p = function
    | h::t when p h -> Some(h, t)
    | _ -> None

let a x = some (( = ) x)

let ( >| ) p k i =
    match p i with
    | Some(e, s) -> Some(k e, s)
    | None -> None
