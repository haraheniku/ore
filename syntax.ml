type syntax =
  | Any
  | Char of char
  | String of string
  | Seq of syntax list
  | Alt of syntax * syntax
  | Star of syntax
  | Plus of syntax
  | Option of syntax
  | Group of int * syntax
  | Ref of int
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
    | r ->
        flush seq;
        seq.next <- r :: seq.next

  let extract seq =
    flush seq;
    Seq (List.rev seq.next)
end

type program =
    { re: syntax;
      numgroups: int; }
[@@deriving show]

let parse s =
  let len = String.length s in
  let group_count = ref 0 in

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
    | '.' -> (Any, i+1)
    | '\\' -> backslash (i+1)
    | '(' ->
        let r, j = regexp (i+1) in
        if j < len && s.[j] = ')'
        then begin
          incr group_count;
          (Group (!group_count, r), j+1)
        end
        else assert false
    | _ as c -> (Char c, i+1)
  and backslash i =
    if i >= len then (Char '\\', i) else
    match s.[i] with
    | '1' .. '9' as c ->
        let k = (int_of_char c - 48) in
        if k > !group_count then
          failwith @@ Printf.sprintf "invalid group reference %d" k
        else
          (Ref k, i+1)
    | c -> (Char c, i)
  in

  let r, k = regexp 0 in
  if k != len then assert false
  else
    { re = r;
      numgroups = !group_count; }
