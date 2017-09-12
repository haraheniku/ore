open Syntax

type inst =
  | InstNop
  | InstMatch
  | InstFail
  | InstChar of char
  | InstString of string
  | InstJmp of int
  | InstSplit of int * int
[@@deriving show]


type regex =
    { insts: inst array; }
[@@deriving show]


let compile s =
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
    | Char c ->
        emit_inst (InstChar c)
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
    | Group r ->
        emit_code r
  in

  emit_code (parse s);
  emit_inst InstMatch;
  { insts = Array.sub !prog 0 !pos; }


let substr_match s1 l1 start s2 l2 =
  let rec loop j =
    if j >= l2 then true
    else if start+j >= l1 then false
    else if s1.[start+j] = s2.[j] then loop (j+1) else false
  in
  loop 0


let exec reg s =
  let len = String.length s in
  let insts = reg.insts in
  let proglen = Array.length insts in

  let rec exec_code pc i =
    if pc >= proglen then assert false else
    let inst = insts.(pc) in
    match inst with
    | InstNop -> exec_code (pc+1) i
    | InstMatch -> Some i
    | InstFail -> None
    | InstChar c ->
        if i >= len then None
        else if s.[i] = c then exec_code (pc+1) (i+1)
        else None
    | InstString lit ->
        let litlen = String.length lit in
        let m = substr_match s len i lit litlen in
        if not m then None else exec_code (pc+1) (litlen + i)
    | InstJmp x -> exec_code x i
    | InstSplit (x, y) ->
        match exec_code x i with
        | None -> exec_code y i
        | _ as s -> s
  in

  let rec loop start =
    if start >= len then
      raise Not_found
    else
      match exec_code 0 start with
      | None -> loop (start+1)
      | Some stop ->
          String.sub s start (stop-start)
  in
  loop 0


let () =
  let prog = "(foo+)+" in
  print_endline @@ show_syntax @@ parse prog;
  let reg = compile prog in
  print_endline @@ show_regex reg;
  print_endline @@ exec reg "fooooofoofooooofoo"
