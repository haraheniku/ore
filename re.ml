open Syntax

type inst =
  | InstNop
  | InstMatch
  | InstFail
  | InstAny
  | InstAnyNotNL
  | InstChar of char
  | InstCharClass of char list * bool
  | InstString of string
  | InstJmp of int
  | InstSplit of int * int
  | InstBeginCap of int
  | InstEndCap of int
  | InstRef of int
  | InstBeginLine
  | InstEndLine
[@@deriving show]


type regex =
    { insts: inst array;
      numcaps: int; }
[@@deriving show]


let compile s =
  let { re = re; numgroups=numgroups; } = parse s in
  let prog = ref (Array.make 32 InstNop) in
  let pos = ref 0 in

  let emit_inst op =
    let len = Array.length !prog in
    if !pos >= len then begin
      let newlen = len * 2 in
      let newprog = (Array.make newlen InstNop) in
      Array.blit !prog 0 newprog 0 !pos;
      prog := newprog
    end;
    (!prog).(!pos) <- op;
    incr pos
  in
  let emit_hole () =
    let p = !pos in emit_inst InstNop; p
  in
  let patch_hole p op =
    (!prog).(p) <- op
  in
  let rec emit_code = function
    | Any ->
        emit_inst InstAny
    | AnyNotNL ->
        emit_inst InstAnyNotNL
    | Char c ->
        emit_inst (InstChar c)
    | CharClass (cs, compl) ->
        emit_inst (InstCharClass (cs, compl))
    | String s ->
        emit_inst (InstString s)
    | Seq rl ->
        List.iter emit_code rl
    | Alt (r1, r2) ->
        (*     SPLIT L1, L2
           L1: <r1>
               JMP L3
           L2: <r2>
           L3:
         *)
        let pos_split = emit_hole () in
        emit_code r1;
        let pos_jmp = emit_hole () in
        emit_code r2;
        patch_hole pos_split (InstSplit (pos_split+1, pos_jmp+1));
        patch_hole pos_jmp (InstJmp !pos)
    | Option r ->
        (*     SPLIT L1, L2
           L1: <r>
           L2:
         *)
        let p = emit_hole () in
        emit_code r;
        patch_hole p (InstSplit (p+1, !pos))
    | Star r ->
        (* L1: SPLIT L2, L3
           L2: <r>
               JMP L1
           L3:
         *)
        let pos_split = emit_hole () in
        emit_code r;
        let pos_jmp = emit_hole () in
        patch_hole pos_split (InstSplit (pos_split+1, !pos));
        patch_hole pos_jmp (InstJmp pos_split)
    | Plus r ->
        (* L1: <r>
               SPLIT L1, L2
           L2:
         *)
        let pos_start = !pos in
        emit_code r;
        let pos_split = emit_hole () in
        patch_hole pos_split (InstSplit (pos_start, !pos))
    | Group (i, r) ->
        emit_inst (InstBeginCap i);
        emit_code r;
        emit_inst (InstEndCap i)
    | Ref i ->
        emit_inst (InstRef i)
    | BeginLine ->
        emit_inst InstBeginLine
    | EndLine ->
        emit_inst InstEndLine
  in

  emit_code re;
  emit_inst InstMatch;
  { insts = Array.sub !prog 0 !pos;
    numcaps = numgroups; }


let substr_match s1 l1 start s2 l2 =
  let rec loop j =
    if j >= l2 then true
    else if start+j >= l1 then false
    else if s1.[start+j] = s2.[j] then loop (j+1) else false
  in
  loop 0


let exec reg s =
  let { insts = insts; numcaps = numcaps } = reg in
  let len = String.length s in
  let proglen = Array.length insts in
  let slots = Array.make numcaps (-1) in
  let caps = Array.make (numcaps+1) "" in

  let rec exec_code pc i =
    if pc >= proglen then assert false else
    let inst = insts.(pc) in
    match inst with
    | InstNop -> exec_code (pc+1) i
    | InstMatch -> Some i
    | InstFail -> None
    | InstAny ->
        if i >= len then None
        else exec_code (pc+1) (i+1)
    | InstAnyNotNL ->
        if i >= len then None else
          (match s.[i] with
          | '\r' | '\n' -> None
          | _ -> exec_code (pc+1) (i+1))
    | InstChar c ->
        if i >= len then None
        else if s.[i] = c then exec_code (pc+1) (i+1)
        else None
    | InstCharClass (cs, compl) ->
        if i >= len then None
        else if compl = List.mem s.[i] cs then None
        else exec_code (pc+1) (i+1)
    | InstString lit ->
        let litlen = String.length lit in
        let m = substr_match s len i lit litlen in
        if not m then None
        else exec_code (pc+1) (litlen + i)
    | InstJmp x -> exec_code x i
    | InstSplit (x, y) ->
        (match exec_code x i with
        | None -> exec_code y i
        | s -> s)
    | InstBeginCap j ->
        let old = slots.(j-1) in
        slots.(j-1) <- i;
        (match exec_code (pc+1) i with
        | None ->
            slots.(j-1) <- old;
            None
        | s -> s)
    | InstEndCap j ->
        let start = slots.(j-1) in
        let cap = String.sub s start (i-start) in
        let old = caps.(j) in
        caps.(j) <- cap;
        (match exec_code (pc+1) i with
        | None ->
            caps.(j) <- old;
            None
        | s -> s)
    | InstRef j ->
        let cap = caps.(j) in
        let caplen = String.length cap in
        let m = substr_match s len i cap caplen in
        if not m then None else exec_code (pc+1) (caplen + i)
    | InstBeginLine ->
        if i == 0 then exec_code (pc+1) i else
        begin
          match s.[i-1] with
          | '\r' | '\n' ->
              exec_code (pc+1) i
          | _ -> None
        end
    | InstEndLine ->
        if i >= len then exec_code (pc+1) i else
        begin
          match s.[i+1] with
          | '\r' | '\n' ->
              exec_code (pc+1) (i+1)
          | _ -> None
        end
  in

  let rec loop start =
    if start >= len then
      raise Not_found
    else begin
      match exec_code 0 start with
      | None -> loop (start+1)
      | Some stop ->
          String.sub s start (stop-start)
    end
  in
  let m = loop 0 in
  caps.(0) <- m;
  caps


let () =
  let prog = "^[][a-z]+$" in
  print_endline @@ show_program @@ parse prog;
  let reg = compile prog in
  print_endline @@ show_regex reg;
  let m = exec reg "[hage]" in
  Array.iteri (fun i s -> Printf.printf "%d:%s\n" i s) m
