type syntax =
  | Char of char
  | String of string
  | Seq of syntax list
  | Alt of syntax * syntax
  | Star of syntax
  | Plus of syntax
  | Option of syntax
  | Group of syntax
[@@deriving show]


module SeqBuf = struct
  type t =
      { mutable next: syntax list;
        buf: Buffer.t; }

  let make init =
    { next = init;
      buf = Buffer.create 16; }

  let flush seq =
    let s = Buffer.contents seq.buf in
    Buffer.clear seq.buf;
    match String.length s with
    | 0 -> ()
    | 1 -> seq.next <- Char s.[0] :: seq.next
    | _ -> seq.next <- String s :: seq.next

  let add seq = function
    | Char c ->
        Buffer.add_char seq.buf c
    | _ as syn ->
        flush seq;
        seq.next <- syn :: seq.next

  let extract seq =
    flush seq;
    Seq (List.rev seq.next)
end


let parse s =
  let len = String.length s in

  let rec regexp i =
    let left, j = sequence i in
    if j < len && s.[j] = '|' then
      let right, k = regexp (j+1) in
      Alt(left, right), k
    else
      left, j
  and sequence i =
    sequence' (SeqBuf.make []) i
  and sequence' seq i  =
    if i >= len || s.[i] = '|' || s.[i] = ')' then
      SeqBuf.extract seq, i
    else
      let r, j = element i in
      SeqBuf.add seq r;
      sequence' seq j
  and element i =
    let r, j = atom i in
    if j >= len then r, j else
    match s.[j] with
    | '*' -> (Star r, j+1)
    | '+' -> (Plus r, j+1)
    | '?' -> (Option r, j+1)
    | _ -> r, j
  and atom i =
    match s.[i] with
    | '*' | '+' | '?' | '|' -> assert false
    | '(' ->
        let r, j = regexp (i+1) in
        if j < len && s.[j] = ')'
        then (Group r, j+1)
        else assert false
    | _ as c -> (Char c, i+1)
  in

  fst (regexp 0)
