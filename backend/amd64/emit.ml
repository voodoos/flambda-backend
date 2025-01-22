(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Emission of Intel x86_64 assembly code *)

(* Correctness: carefully consider any use of [Config], [Clflags],
   [Flambda_backend_flags] and shared variables.
   For details, see [asmgen.mli]. *)

open Cmm
open Arch
open Proc
open Reg
open Simple_operation
open Linear
open Emitaux

open X86_ast
open X86_proc
open X86_dsl
module String = Misc.Stdlib.String
module Int = Numbers.Int

(* [Branch_relaxation] is not used in this file, but is required by
   emit.mlp files for certain other targets; the reference here ensures
   that when releases are being prepared the .depend files are correct
   for all targets. *)
[@@@ocaml.warning "-66"]
open! Branch_relaxation

let _label s = D.label ~typ:QWORD s

(* Override proc.ml *)

let int_reg_name =
  [| RAX; RBX; RDI; RSI; RDX; RCX; R8; R9;
     R12; R13; R10; R11; RBP; |]

let float_reg_name = Array.init 16 (fun i -> XMM i)

let register_name typ r =
  match (typ : machtype_component) with
  | Int | Val | Addr -> Reg64 (int_reg_name.(r))
  | Float | Float32 | Vec128 | Valx2 -> Regf (float_reg_name.(r - 100))

let phys_rax = phys_reg Int 0
let phys_rdx = phys_reg Int 4
let phys_rcx = phys_reg Int 5
let phys_xmm0v () = phys_reg Vec128 100

(* CFI directives *)

let cfi_startproc () =
  if Config.asm_cfi_supported then D.cfi_startproc ()

let cfi_endproc () =
  if Config.asm_cfi_supported then D.cfi_endproc ()

let cfi_adjust_cfa_offset n =
  if Config.asm_cfi_supported then D.cfi_adjust_cfa_offset n

let cfi_remember_state () =
  if Config.asm_cfi_supported then D.cfi_remember_state ()

let cfi_restore_state () =
  if Config.asm_cfi_supported then D.cfi_restore_state ()

let cfi_def_cfa_register reg =
  if Config.asm_cfi_supported then D.cfi_def_cfa_register reg

let emit_debug_info ?discriminator dbg =
  emit_debug_info_gen ?discriminator dbg D.file D.loc

let emit_debug_info_linear i =
  match i.fdo with
  | None -> emit_debug_info i.dbg
  | Some { discriminator; dbg } ->
    emit_debug_info ~discriminator dbg

let fp = Config.with_frame_pointers

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Layout of the stack frame *)
let stack_offset = ref 0

let num_stack_slots = Array.make Proc.num_stack_slot_classes 0

let prologue_required = ref false

let frame_required = ref false

let contains_calls = ref false

let frame_size () =
  Proc.frame_size
    ~stack_offset:!stack_offset
    ~num_stack_slots
    ~contains_calls:!contains_calls

let slot_offset loc stack_class =
  let offset =
    Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
      ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
  in
  match offset with
  | Bytes_relative_to_stack_pointer n -> n
  | Bytes_relative_to_domainstate_pointer _ ->
    Misc.fatal_errorf "Not a stack slot"

let emit_stack_offset n =
  if n < 0
  then I.add (int (-n)) rsp
  else if n > 0
  then I.sub (int n) rsp;
  if n <> 0
  then cfi_adjust_cfa_offset n;
  stack_offset := !stack_offset + n

let push r =
  I.push r;
  cfi_adjust_cfa_offset 8;
  stack_offset := !stack_offset + 8

let pop r =
  I.pop r;
  cfi_adjust_cfa_offset (-8);
  stack_offset := !stack_offset - 8

(* Symbols *)

let symbol_prefix = if system = S_macosx then "_" else ""

let emit_symbol s = string_of_symbol symbol_prefix s

(* Record symbols used and defined - at the end generate extern for those
   used but not defined *)

let symbols_defined = ref String.Set.empty
let symbols_used = ref String.Set.empty

let add_def_symbol s = symbols_defined := String.Set.add s !symbols_defined
let add_used_symbol s = symbols_used := String.Set.add s !symbols_used

let imp_table = Hashtbl.create 16

let reset_imp_table () = Hashtbl.clear imp_table

let get_imp_symbol s =
  match Hashtbl.find imp_table s with
  | exception Not_found ->
      let imps = "__caml_imp_" ^ s in
      Hashtbl.add imp_table s imps;
      imps
  | imps -> imps

let emit_imp_table () =
  let f s imps =
    _label (emit_symbol imps);
    D.qword (ConstLabel (emit_symbol s))
  in
  D.data();
  D.comment "relocation table start";
  D.align ~data:true 8;
  Hashtbl.iter f imp_table;
  D.comment "relocation table end"

let mem__imp s =
  let imp_s = get_imp_symbol s in
  mem64_rip QWORD (emit_symbol imp_s)

(* Output a label *)

let label_name lbl =
  match system with
  | S_macosx | S_win64 -> "L" ^ lbl
  | _ -> ".L" ^ lbl

let emit_label lbl =
  label_name (Label.to_string lbl)

let rel_plt (s : Cmm.symbol) =
  match s.sym_global with
  | Local -> sym (label_name (emit_symbol s.sym_name))
  | Global ->
    if windows && !Clflags.dlcode then mem__imp s.sym_name
    else
      let s = emit_symbol s.sym_name in
      sym (if use_plt then s ^ "@PLT" else s)

let emit_call s = I.call (rel_plt s)

let emit_jump s = I.jmp (rel_plt s)

let domain_field f =
  mem64 QWORD (Domainstate.idx_of_field f * 8) R14

let label s = sym (emit_label s)

let def_label ?typ s =
  D.label ?typ (emit_label s)

let emit_cmm_symbol (s : Cmm.symbol) =
  match s.sym_global with
  | Global -> emit_symbol s.sym_name
  | Local -> label_name (emit_symbol s.sym_name)

let load_symbol_addr s arg =
  match s.sym_global with
  | Local ->
    I.lea (mem64_rip NONE (label_name (emit_symbol s.sym_name))) arg
  | Global ->
    if !Clflags.dlcode then
      if windows then begin
        (* I.mov (mem__imp s) arg (\* mov __caml_imp_foo(%rip), ... *\) *)
        I.mov (sym (emit_symbol s.sym_name)) arg (* movabsq $foo, ... *)
      end else I.mov (mem64_rip QWORD (emit_symbol s.sym_name ^ "@GOTPCREL")) arg
    else if !Clflags.pic_code then
      I.lea (mem64_rip NONE (emit_symbol s.sym_name)) arg
    else
      I.mov (sym (emit_symbol s.sym_name)) arg

(* Output .text section directive, or named .text.caml.<name> if enabled and
   supported on the target system. *)

let emit_named_text_section ?(suffix = "") func_name =
  if !Clflags.function_sections ||
     !Flambda_backend_flags.basic_block_sections then
    begin match system with
    | S_macosx
    (* Names of section segments in macosx are restricted to 16 characters,
       but function names are often longer, especially anonymous functions. *)
    | S_win64 | S_mingw64 | S_cygwin
    (* Win systems provide named text sections, but configure on these
       systems does not support function sections. *)
      ->  assert false
    | _ -> D.section
             [ Printf.sprintf ".text.caml.%s%s" (emit_symbol func_name) suffix ]
             (Some "ax")
             ["@progbits"]
    end
  else D.text ()

(* Name of current function *)
let function_name = ref ""

(* Keep the name of the current block section to get back to it
   after emitting data. *)
let current_basic_block_section = ref ""

let emit_function_or_basic_block_section_name () =
  let suffix =
    if String.length !current_basic_block_section = 0 then
      ""
    else
      "."^(!current_basic_block_section)
  in
  emit_named_text_section !function_name ~suffix

let emit_Llabel fallthrough lbl section_name =
  if !Flambda_backend_flags.basic_block_sections then begin
    match section_name with
    | Some name ->
      if not (String.equal name !current_basic_block_section) then begin
        current_basic_block_section := name;
        cfi_endproc ();
        emit_function_or_basic_block_section_name ();
        cfi_startproc ();
      end
    | None ->
      ()
  end;
  if not fallthrough && !fastcode_flag then D.align ~data:false 4;
  def_label lbl

(* Output a pseudo-register *)

let x86_data_type_for_stack_slot : machtype_component -> data_type = function
  | Float -> REAL8
  | Vec128 -> VEC128
  | Valx2 -> VEC128
  | Int | Addr | Val -> QWORD
  | Float32 -> REAL4

let reg = function
  | { loc = Reg.Reg r; typ = ty } -> register_name ty r
  | { loc = Stack (Domainstate n); typ = ty } ->
      let ofs = n + Domainstate.(idx_of_field Domain_extra_params) * 8 in
      mem64 (x86_data_type_for_stack_slot ty) ofs R14
  | { loc = Stack s; typ = ty } as r ->
      let ofs = slot_offset s (stack_slot_class r.typ) in
      mem64 (x86_data_type_for_stack_slot ty) ofs RSP
  | { loc = Unknown } ->
      assert false

let reg64 = function
  | { loc = Reg.Reg r } -> int_reg_name.(r)
  | _ -> assert false


let res i n = reg i.res.(n)

let arg i n = reg i.arg.(n)

(* Output a reference to the lower 8, 16 or 32 bits of a register *)

let reg_low_8_name  = Array.map (fun r -> Reg8L r) int_reg_name
let reg_low_16_name = Array.map (fun r -> Reg16 r) int_reg_name
let reg_low_32_name = Array.map (fun r -> Reg32 r) int_reg_name

let emit_subreg tbl typ r =
  match r.loc with
  | Reg.Reg r when r < 13 -> tbl.(r)
  | Stack s -> mem64 typ (slot_offset s (stack_slot_class r.Reg.typ)) RSP
  | _ -> assert false

let arg8 i n = emit_subreg reg_low_8_name BYTE i.arg.(n)
let arg16 i n = emit_subreg reg_low_16_name WORD i.arg.(n)
let arg32 i n = emit_subreg reg_low_32_name DWORD i.arg.(n)
let arg64 i n = reg64 i.arg.(n)

let res8 i n = emit_subreg reg_low_8_name BYTE i.res.(n)
let res16 i n = emit_subreg reg_low_16_name WORD i.res.(n)
let res32 i n = emit_subreg reg_low_32_name DWORD i.res.(n)

(* Output an addressing mode *)

let addressing addr typ i n =
  match addr with
  | Ibased(sym_name, sym_global, ofs) ->
      add_used_symbol sym_name;
      let sym_global : Cmm.is_global =
        match sym_global with Global -> Global | Local -> Local in
      mem64_rip typ (emit_cmm_symbol { sym_name ; sym_global }) ~ofs
  | Iindexed d ->
      mem64 typ d (arg64 i n)
  | Iindexed2 d ->
      mem64 typ ~base:(arg64 i n) d (arg64 i (n+1))
  | Iscaled(2, d) ->
      mem64 typ ~base:(arg64 i n) d (arg64 i n)
  | Iscaled(scale, d) ->
      mem64 typ ~scale d (arg64 i n)
  | Iindexed2scaled(scale, d) ->
      mem64 typ ~scale ~base:(arg64 i n) d (arg64 i (n+1))

(* Record live pointers at call points -- see Emitaux *)

let record_frame_label live dbg =
  let lbl = new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | {typ = Val; loc = Reg r} as reg ->
          assert (Proc.gc_regs_offset reg = r);
          live_offset := ((r lsl 1) + 1) :: !live_offset
      | {typ = Val; loc = Stack s} as reg ->
          live_offset := slot_offset s (stack_slot_class reg.typ) :: !live_offset
      | {typ = Valx2; loc = Reg r} as reg ->
          let n = Proc.gc_regs_offset reg in
          let encode n = ((n lsl 1) + 1) in
          live_offset := encode n :: encode (n + 1) :: !live_offset
      | {typ = Valx2; loc = Stack s} as reg ->
          let n = slot_offset s (stack_slot_class reg.typ)  in
          live_offset := n :: n + Arch.size_addr :: !live_offset
      | {typ = Addr} as r ->
          Misc.fatal_error ("bad GC root " ^ Reg.name r)
      | { typ = (Val | Valx2); loc = Unknown ; } as r ->
        Misc.fatal_error ("Unknown location " ^ Reg.name r)
      | { typ = Int | Float | Float32 | Vec128; _ } -> ()
    )
    live;
  record_frame_descr ~label:lbl ~frame_size:(frame_size())
    ~live_offset:!live_offset dbg;
  lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in
  def_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: label;                      (* Entry label *)
    gc_return_lbl: label;               (* Where to branch after GC *)
    gc_frame: label;                    (* Label of frame descriptor *)
    gc_dbg : Debuginfo.t;               (* Location of the original instruction *)
  }

let call_gc_sites = ref ([] : gc_call list)

let call_gc_local_sym : Cmm.symbol =
  {sym_name = "caml_call_gc_"; sym_global=Local}

let emit_call_gc gc =
  def_label gc.gc_lbl;
  emit_debug_info gc.gc_dbg;
  emit_call call_gc_local_sym;
  def_label gc.gc_frame;
  I.jmp (label gc.gc_return_lbl)

(* Record calls to local stack reallocation *)

type local_realloc_call =
  { lr_lbl: label;
    lr_return_lbl: label;
    lr_dbg: Debuginfo.t;
  }

let local_realloc_sites = ref ([] : local_realloc_call list)

let emit_local_realloc lr =
  def_label lr.lr_lbl;
  emit_debug_info lr.lr_dbg;
  emit_call (Cmm.global_symbol "caml_call_local_realloc");
  I.jmp (label lr.lr_return_lbl)

(* Record calls to caml_ml_array_bound_error and caml_ml_array_align_error.
   In -g mode we maintain one call per bound check site.  Without -g, we can share
   a single call. *)

type safety_check = Bound_check | Align_check

type safety_check_failure = {
    sc_lbl: label;              (* Entry label *)
    sc_frame: label;            (* Label of frame descriptor *)
    sc_dbg: Debuginfo.t;        (* As for [gc_call]. *)
  }

type safety_check_sites = {
    mutable sc_sites: safety_check_failure list;
    mutable sc_call: label option;
  }

let bound_checks = { sc_sites = []; sc_call = None }
let align_checks = { sc_sites = []; sc_call = None }

let emit_call_safety_error kind sc =
  def_label sc.sc_lbl;
  emit_debug_info sc.sc_dbg;
  (match kind with
  | Bound_check -> emit_call (Cmm.global_symbol "caml_ml_array_bound_error")
  | Align_check -> emit_call (Cmm.global_symbol "caml_ml_array_align_error"));
  def_label sc.sc_frame

let clear_safety_checks () =
  bound_checks.sc_sites <- [];
  bound_checks.sc_call <- None;
  align_checks.sc_sites <- [];
  align_checks.sc_call <- None

let emit_call_safety_errors () =
  List.iter (emit_call_safety_error Bound_check) bound_checks.sc_sites;
  begin match bound_checks.sc_call with
  | None -> ()
  | Some sc_call ->
    def_label sc_call;
    emit_call (Cmm.global_symbol "caml_ml_array_bound_error")
  end;
  List.iter (emit_call_safety_error Align_check) align_checks.sc_sites;
  begin match align_checks.sc_call with
  | None -> ()
  | Some sc_call ->
    def_label sc_call;
    emit_call (Cmm.global_symbol "caml_ml_array_align_error")
  end

(* Stack reallocation *)
type stack_realloc = {
  sc_label : Label.t; (* Label of the reallocation code. *)
  sc_return : Label.t; (* Label to return to after reallocation. *)
  sc_size_in_bytes : int; (* Size for reallocation. *)
}

let stack_realloc = ref ([] : stack_realloc list)

let clear_stack_realloc () =
  stack_realloc := []

let emit_stack_realloc () =
  List.iter
    (fun { sc_label; sc_return; sc_size_in_bytes; } ->
       def_label sc_label;
       (* Pass the desired frame size on the stack, since all of the
          argument-passing registers may be in use.
          Also serves to align the stack properly before the call *)
       I.push (int (Config.stack_threshold + sc_size_in_bytes / 8));
       cfi_adjust_cfa_offset 8;
       (* measured in words *)
       emit_call (Cmm.global_symbol "caml_call_realloc_stack");
       I.add (int 8) rsp;
       cfi_adjust_cfa_offset (-8);
       I.jmp (label sc_return))
    !stack_realloc

let emit_stack_check ~size_in_bytes ~save_registers =
  let overflow = new_label () and ret = new_label () in
  let threshold_offset = Domainstate.stack_ctx_words * 8 + Stack_check.stack_threshold_size in
  if save_registers then I.push r10;
  I.lea (mem64 NONE (-(size_in_bytes + threshold_offset)) RSP) r10;
  I.cmp (domain_field Domainstate.Domain_current_stack) r10;
  if save_registers then I.pop r10;
  I.jb (label overflow);
  def_label ret;
  stack_realloc := {
    sc_label = overflow;
    sc_return = ret;
    sc_size_in_bytes = size_in_bytes;
  } :: !stack_realloc

(* Record jump tables *)
type jump_table =
  { table_lbl: string;
    elems: Linear.label array }

let jump_tables = ref ([] : jump_table list)

let emit_jump_table t =
  _label t.table_lbl;
  for i = 0 to Array.length t.elems - 1 do
    D.long (ConstSub (ConstLabel(emit_label t.elems.(i)),
                      ConstLabel t.table_lbl))
  done

let emit_jump_tables () =
  D.align ~data:true 4;
  List.iter emit_jump_table !jump_tables;
  jump_tables := []

let build_asm_directives () : (module Asm_targets.Asm_directives_intf.S) = (
    module Asm_targets.Asm_directives.Make(struct

      let emit_line str = X86_dsl.D.comment str

      let get_file_num file_name =
        Emitaux.get_file_num ~file_emitter:X86_dsl.D.file file_name

      let debugging_comments_in_asm_files =
        !Flambda_backend_flags.dasm_comments

      module D = struct
        open X86_ast

        include X86_dsl.D

        type data_type =
          | NONE | DWORD | QWORD | VEC128

        type nonrec constant = constant
        let const_int64 num = Const num
        let const_label str = ConstLabel str
        let const_add c1 c2 = ConstAdd (c1, c2)
        let const_sub c1 c2 = ConstSub (c1, c2)

        let label ?data_type str =
          let typ =
            Option.map
              (function
                | NONE -> X86_ast.NONE
                | DWORD -> X86_ast.DWORD
                | QWORD -> X86_ast.QWORD
                | VEC128 -> X86_ast.VEC128)
              data_type
          in
          label ?typ str
      end
    end)
  )


(* Names for instructions *)

let instr_for_intop = function
  | Iadd -> I.add
  | Isub -> I.sub
  | Imul -> (fun arg1 arg2 -> I.imul arg1 (Some arg2))
  | Iand -> I.and_
  | Ior -> I.or_
  | Ixor -> I.xor
  | Ilsl -> I.sal
  | Ilsr -> I.shr
  | Iasr -> I.sar
  | _ -> assert false

let instr_for_floatop width op =
  match width, op with
  | Float64, Iaddf -> I.addsd
  | Float64, Isubf -> I.subsd
  | Float64, Imulf -> I.mulsd
  | Float64, Idivf -> I.divsd
  | Float32, Iaddf -> I.addss
  | Float32, Isubf -> I.subss
  | Float32, Imulf -> I.mulss
  | Float32, Idivf -> I.divss
  | _ -> assert false

let instr_for_floatarithmem width op =
  match width, op with
  | Float64, Ifloatadd -> I.addsd
  | Float64, Ifloatsub -> I.subsd
  | Float64, Ifloatmul -> I.mulsd
  | Float64, Ifloatdiv -> I.divsd
  | Float32, Ifloatadd -> I.addss
  | Float32, Ifloatsub -> I.subss
  | Float32, Ifloatmul -> I.mulss
  | Float32, Ifloatdiv -> I.divss

let cond = function
  | Isigned Ceq   -> E   | Isigned Cne   -> NE
  | Isigned Cle   -> LE  | Isigned Cgt   -> G
  | Isigned Clt   -> L   | Isigned Cge   -> GE
  | Iunsigned Ceq -> E   | Iunsigned Cne -> NE
  | Iunsigned Cle -> BE  | Iunsigned Cgt -> A
  | Iunsigned Clt -> B   | Iunsigned Cge -> AE

(* Output an = 0 or <> 0 test. *)

let output_test_zero arg =
  match arg.loc with
  | Reg.Reg _ -> I.test (reg arg) (reg arg)
  | _  -> I.cmp (int 0) (reg arg)

(* Output a floating-point compare and branch *)

let emit_float_test (width : Cmm.float_width)
                    cmp i ~(taken:X86_ast.condition -> unit) =
  (* Effect of comisd on flags and conditional branches:
                     ZF PF CF  cond. branches taken
        unordered     1  1  1  je, jb, jbe, jp
        >             0  0  0  jne, jae, ja
        <             0  0  1  jne, jbe, jb
        =             1  0  0  je, jae, jbe.
     If FP traps are on (they are off by default),
     comisd traps on QNaN and SNaN but ucomisd traps on SNaN only.
  *)
  let ucomi, comi =
    match width with
    | Float64 -> I.ucomisd, I.comisd
    | Float32 -> I.ucomiss, I.comiss
  in
  match cmp with
  | CFeq when arg i 1 = arg i 0 ->
      ucomi (arg i 1) (arg i 0);
      taken NP
  | CFeq ->
      let next = new_label() in
      ucomi (arg i 1) (arg i 0);
      I.jp (label next);           (* skip if unordered *)
      taken E;                     (* branch taken if x=y *)
      def_label next
  | CFneq when arg i 1 = arg i 0 ->
      ucomi (arg i 1) (arg i 0);
      taken P
  | CFneq ->
      ucomi (arg i 1) (arg i 0);
      taken P;                     (* branch taken if unordered *)
      taken NE                     (* branch taken if x<y or x>y *)
  | CFlt ->
      comi (arg i 0) (arg i 1);
      taken A                      (* branch taken if y>x i.e. x<y *)
  | CFnlt ->
      comi (arg i 0) (arg i 1);
      taken BE                     (* taken if unordered or y<=x i.e. !(x<y) *)
  | CFle ->
      comi (arg i 0) (arg i 1);    (* swap compare *)
      taken AE                     (* branch taken if y>=x i.e. x<=y *)
  | CFnle ->
      comi (arg i 0) (arg i 1);    (* swap compare *)
      taken B                      (* taken if unordered or y<x i.e. !(x<=y) *)
  | CFgt ->
      comi (arg i 1) (arg i 0);
      taken A                      (* branch taken if x>y *)
  | CFngt ->
      comi (arg i 1) (arg i 0);
      taken BE                     (* taken if unordered or x<=y i.e. !(x>y) *)
  | CFge ->
      comi (arg i 1) (arg i 0);    (* swap compare *)
      taken AE                     (* branch taken if x>=y *)
  | CFnge ->
      comi (arg i 1) (arg i 0);    (* swap compare *)
      taken B                      (* taken if unordered or x<y i.e. !(x>=y) *)

let emit_test i ~(taken:X86_ast.condition -> unit) = function
  | Itruetest ->
    output_test_zero i.arg.(0);
    taken NE
  | Ifalsetest ->
    output_test_zero i.arg.(0);
    taken E
  | Iinttest cmp ->
    I.cmp (arg i 1) (arg i 0);
    taken (cond cmp)
  | Iinttest_imm((Isigned Ceq | Isigned Cne |
                  Iunsigned Ceq | Iunsigned Cne) as cmp, 0) ->
    output_test_zero i.arg.(0);
    taken (cond cmp)
  | Iinttest_imm(cmp, n) ->
    I.cmp (int n) (arg i 0);
    taken (cond cmp)
  | Ifloattest (width, cmp) ->
    emit_float_test width cmp i ~taken
  | Ioddtest ->
    I.test (int 1) (arg8 i 0);
    taken NE
  | Ieventest ->
    I.test (int 1) (arg8 i 0);
    taken E

(* Deallocate the stack frame before a return or tail call *)

let output_epilogue f =
  if !frame_required then begin
    let n = frame_size() - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      I.add (int n) rsp;
      cfi_adjust_cfa_offset (-n);
    end;
    if fp then I.pop rbp;
    f ();
    (* reset CFA back cause function body may continue *)
    if n <> 0
    then cfi_adjust_cfa_offset n
  end
  else
    f ()

(* Floating-point constants *)

let float_constants = ref ([] : (int64 * label) list)

let add_float_constant cst =
  try
    List.assoc cst !float_constants
  with Not_found ->
    let lbl = new_label() in
    float_constants := (cst, lbl) :: !float_constants;
    lbl

let emit_float_constant f lbl =
  _label (emit_label lbl);
  D.qword (Const f)

(* Vector constants *)

let vec128_constants = ref ([] : (Cmm.vec128_bits * label) list)

let add_vec128_constant bits =
  try
    List.assoc bits !vec128_constants
  with Not_found ->
    let lbl = new_label() in
    vec128_constants := (bits, lbl) :: !vec128_constants;
    lbl

let emit_vec128_constant {high; low} lbl =
  (* SIMD vectors respect little-endian byte order *)
  _label (emit_label lbl);
  D.qword (Const low);
  D.qword (Const high)

let global_maybe_protected sym =
  D.global sym;
  if !Flambda_backend_flags.symbol_visibility_protected then
    match system with
    | S_macosx
    | S_win32
    | S_win64
    | S_mingw64
    | S_cygwin
    | S_mingw
    | S_unknown -> ()
    | S_gnu
    | S_solaris
    | S_linux_elf
    | S_bsd_elf
    | S_beos
    | S_linux
    | S_freebsd
    | S_netbsd
    | S_openbsd ->
      (* Global symbols can be marked as being protected. Unlike in C we don't want
        them to be preempted as we're doing a lot of cross module inlining. *)
      D.protected sym

let emit_global_label_for_symbol lbl =
  add_def_symbol lbl;
  let lbl = emit_symbol lbl in
  global_maybe_protected lbl;
  _label lbl

let emit_global_label s =
  let lbl = Cmm_helpers.make_symbol s in
  emit_global_label_for_symbol lbl

let move (src : Reg.t) (dst : Reg.t) =
  let distinct = not (Reg.same_loc src dst) in
  begin match src.typ, src.loc, dst.typ, dst.loc with
  | Float, Reg _, Float, Reg _
  | Float32, Reg _, Float32, Reg _
  | (Vec128 | Valx2), _, (Vec128 | Valx2), _ (* Vec128 stack slots are always aligned. *) ->
    if distinct then I.movapd (reg src) (reg dst)
  | Float, _, Float, _ ->
    if distinct then I.movsd (reg src) (reg dst)
  | Float32, _, Float32, _ ->
    if distinct then I.movss (reg src) (reg dst)
  | (Int | Val | Addr), _, (Int | Val | Addr), _ ->
    if distinct then I.mov (reg src) (reg dst)
  | (Float | Float32 | Vec128 | Int | Val | Addr | Valx2), _, _, _ ->
    Misc.fatal_errorf
      "Illegal move between registers of differing types (%a to %a)\n"
      Printreg.reg src Printreg.reg dst
  end

let stack_to_stack_move (src : Reg.t) (dst : Reg.t) =
  assert (src.typ = dst.typ);
  if (src.loc <> dst.loc) then begin
    match src.typ with
    | Int | Val ->
      (* Not calling move because r15 is not in int_reg_name. *)
      I.mov (reg src) r15;
      I.mov r15 (reg dst)
    | Float | Addr | Vec128 | Valx2 | Float32 ->
      Misc.fatal_errorf
        "Unexpected register type for stack to stack move: from %s to %s\n"
        (Reg.name src) (Reg.name dst)
  end

let move_allowing_stack_to_stack src dst =
  match Reg.is_stack src, Reg.is_stack dst with
  | true, true -> stack_to_stack_move src dst
  | _ -> move src dst

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref None

(* Emit tracing probes *)

type probe =
  {
    stack_offset: int;
    num_stack_slots: int array;
    (* Record frame info held in the corresponding mutable variables. *)
    probe_label: label;
    (* Probe site, recorded in .note.stapsdt section
       for enabling and disabling the probes  *)
    probe_insn: Linear.instruction;
    (* Iprobe instruction, recorded at probe site and used for emitting
       the notes and the wrapper code at the end of the compilation unit. *)
  }

let probe_handler_wrapper_name probe_label =
  let w = Printf.sprintf "probe_wrapper_%s" (Label.to_string probe_label) in
  Cmm_helpers.make_symbol w
  |> emit_symbol

let probes = ref []

let probe_semaphores = ref String.Map.empty

let stapsdt_base_emitted = ref false

let reset_probes () =
  probes := [];
  probe_semaphores := String.Map.empty

let find_or_add_semaphore name enabled_at_init dbg =
  match String.Map.find_opt name !probe_semaphores with
  | Some (label, e) ->
    (match e, enabled_at_init with
     | None, None -> ()
     | None, Some _ ->
       let d = (label, enabled_at_init) in
       probe_semaphores :=
         String.Map.remove name !probe_semaphores |>
         String.Map.add name d
     | Some _, None ->
       (*  [find_or_add_semaphore] is called with None for Iprobe_is_enabled
           during code emission only. [find_or_add_semaphore] us called with
           Some to emit probe notes only after all code is emitted. *)
       assert false
     | Some b, Some b' ->
       if not (Bool.equal b b') then
         raise (Error (Inconsistent_probe_init (name,dbg))));
    label
  | None ->
    let sym = "caml_probes_semaphore_"^name in
    let d = (sym, enabled_at_init) in
    probe_semaphores := String.Map.add name d !probe_semaphores;
    sym

let emit_call_probe_handler_wrapper i ~enabled_at_init ~probe_label =
  assert !frame_required;
  let wrap_label = probe_handler_wrapper_name probe_label in
  (* We emit a cmp instruction that is effectively a nop:
     it only sets flags that are not read anywhere.
     To enable the probe, cmp is replaced with call
     by changing the first byte of the encoding from 3d to e8.
     The operand of the call is a displacement relative to
     the next instruction. Hence, the immediate operand
     of cmp is set up to have that value. *)
  if enabled_at_init then begin
      I.call (sym wrap_label)
  end else
  if !Clflags.pic_code then begin
    (* Manually emit encoding of cmp and an explicit relocation on it
       as needed for a call instruction, to ensure a correct
       result, instead of relying on an assembler that might choose a different
       encoding which produces an incorrect relocation and changes the meaning
       of the program. *)
    (* Emit the required encoding of "cmp $0, %eax" directly using .byte *)
    D.byte (Const 0x3dL);
    D.byte (Const 0L);
    D.byte (Const 0L);
    D.byte (Const 0L);
    D.byte (Const 0L);
    (* Emit the relocation for the call target *)
    (* [rel_size] is the number of bytes taken by the operand of cmp/call
       that needs to be relocated. It is used to form reloc's offset.
       [rel_offset_from_next] is the distance from the start of the relocated
       bytes to the start to the next instruction after the call.
       It used to form reloc's expr for call's operand (see above).
       [rel_size] is equal to [rel_offset_from_next] because
       the relocated operand is the last one and arch is little endian. *)
    let rel_size = 4L in
    let rel_offset_from_next = 4L in
    D.reloc ~offset:(ConstSub (ConstThis, Const rel_size))
      ~name:R_X86_64_PLT32
      ~expr:(ConstSub (ConstLabel wrap_label,
                       Const rel_offset_from_next));
  end else begin
    (* Emit absolute value, no relocation.
       The immediate operand of cmp is the offset of the wrapper
       from the current instruction's address "."
       minus the length of the current instruction, which is 5,
       and the wrapper is emitted at the end of the compilation unit. *)
    I.cmp (sym (Printf.sprintf "%s - . - 5" wrap_label)) eax
  end;
  (* Live registers are saved by the probe wrapper,
     so they are not recorded as gc roots at the probe site. *)
  let stack_live = Reg.Set.filter Reg.is_stack i.live in
  record_frame stack_live (Dbg_other i.dbg)

(* Emit trap handler notes *)

type traps = { mutable push_traps : label list;
               mutable pop_traps : label list;
               mutable enter_traps : Label.Set.t;
             }

let traps = { push_traps = []; pop_traps = []; enter_traps = Label.Set.empty; }

let reset_traps () =
  traps.push_traps <- [];
  traps.pop_traps <- [];
  traps.enter_traps <- Label.Set.empty

let emit_pop_trap_label () =
  let lbl = Cmm.new_label () in
  def_label lbl;
  traps.pop_traps <- lbl::traps.pop_traps

let emit_push_trap_label handler =
  let lbl = Cmm.new_label () in
  def_label lbl;
  traps.push_traps <- lbl::traps.push_traps;
  traps.enter_traps <- Label.Set.add handler traps.enter_traps

(* Emit Code *)

let emit_atomic instr op (size : Cmm.atomic_bitwidth) addr =
  let first_memory_arg_index =
    match op with
    | Compare_set -> 2
    | Fetch_and_add -> 1
    | Add | Sub | Land | Lor | Lxor -> 1
    | Exchange -> 1
    | Compare_exchange -> 2
  in
  let dst =
    addressing addr DWORD instr first_memory_arg_index
  in
  let src_index = first_memory_arg_index - 1 in
  let typ, src =
    match size with
    | Thirtytwo -> DWORD, arg32 instr src_index
    | (Sixtyfour|Word) -> QWORD, arg instr src_index
  in
  match op with
  | Fetch_and_add ->
    assert (Reg.same_loc instr.res.(0) instr.arg.(0));
    I.lock_xadd src dst
  | Add -> I.lock_add src dst
  | Sub -> I.lock_sub src dst
  | Land -> I.lock_and src dst
  | Lor -> I.lock_or src dst
  | Lxor -> I.lock_xor src dst
  | Compare_set ->
    (* compare_with is already in rax, set_to is src *)
    assert (Reg.is_reg instr.arg.(1));
    assert (Reg.same_loc instr.arg.(0) phys_rax);
    let res8, res = res8 instr 0, res instr 0 in
    I.lock_cmpxchg src dst;
    I.set E res8;
    I.movzx res8 res
  | Compare_exchange ->
    (* compare_with is already in rax, set_to is src, res in rax *)
    assert (Reg.is_reg instr.arg.(1));
    assert (Reg.same_loc instr.arg.(0) phys_rax);
    assert (Reg.same_loc instr.res.(0) phys_rax);
    I.lock_cmpxchg src dst
  | Exchange ->
    (* no need for a "lock" prefix for XCHG with a memory operand *)
    assert (Reg.is_reg instr.arg.(0));
    I.xchg src dst

let emit_reinterpret_cast (cast : Cmm.reinterpret_cast) i =
  let distinct = not (Reg.same_loc i.arg.(0) i.res.(0)) in
  match cast with
  | Int_of_value | Value_of_int ->
    if distinct then I.mov (arg i 0) (res i 0)
  | Float_of_float32 | Float32_of_float ->
    if distinct then I.movss (arg i 0) (res i 0)
  | V128_of_v128 ->
    if distinct then I.movapd (arg i 0) (res i 0)
  | Float_of_int64 | Int64_of_float -> I.movq (arg i 0) (res i 0)
  | Float32_of_int32 -> I.movd (arg32 i 0) (res i 0)
  | Int32_of_float32 -> I.movd (arg i 0) (res32 i 0)

let emit_static_cast (cast : Cmm.static_cast) i =
  let distinct = not (Reg.same_loc i.arg.(0) i.res.(0)) in
  match cast with
  | Float_of_int Float64 -> I.cvtsi2sd (arg i 0) (res i 0)
  | Int_of_float Float64 -> I.cvttsd2si (arg i 0) (res i 0)
  | Float_of_int Float32 -> I.cvtsi2ss (arg i 0) (res i 0)
  | Int_of_float Float32 -> I.cvttss2si (arg i 0) (res i 0)
  | Float_of_float32 -> I.cvtss2sd (arg i 0) (res i 0)
  | Float32_of_float -> I.cvtsd2ss (arg i 0) (res i 0)
  | V128_of_scalar Float64x2 | Scalar_of_v128 Float64x2 ->
    if distinct then I.movsd (arg i 0) (res i 0)
  | Scalar_of_v128 Int64x2 | V128_of_scalar Int64x2 ->
    I.movq (arg i 0) (res i 0)
  | Scalar_of_v128 Int32x4 -> I.movd (arg i 0) (res32 i 0)
  | V128_of_scalar Int32x4 -> I.movd (arg32 i 0) (res i 0)
  | V128_of_scalar Float32x4 | Scalar_of_v128 Float32x4 ->
    if distinct then I.movss (arg i 0) (res i 0)
  | Scalar_of_v128 Int16x8 ->
    (* [movw] and [movzx] cannot operate on vector registers.
       We must zero extend as the result is an untagged positive int.
       CR mslater: (SIMD) remove zx once we have unboxed int16 *)
    I.movd (arg i 0) (res32 i 0);
    I.movzx (res16 i 0) (res i 0)
  | Scalar_of_v128 Int8x16 ->
    (* [movb] and [movzx] cannot operate on vector registers.
       We must zero extend as the result is an untagged positive int.
       CR mslater: (SIMD) remove zx once we have unboxed int8 *)
    I.movd (arg i 0) (res32 i 0);
    I.movzx (res8 i 0) (res i 0)
  | V128_of_scalar Int16x8 | V128_of_scalar Int8x16 ->
    (* [movw] and [movb] cannot operate on vector registers.
       Moving 32 bits is OK because the argument is an untagged
       positive int and these operations leave the top bits of the vector unspecified.
       CR mslater: (SIMD) don't load 32 bits once we have unboxed int16/int8 *)
    I.movd (arg32 i 0) (res i 0)

let check_simd_instr (register_behavior : Simd_proc.register_behavior) i =
  (match register_behavior with
  | R_to_fst ->
    assert (Reg.same_loc i.arg.(0) i.res.(0));
    assert (Reg.is_reg i.arg.(0))
  | R_to_RM ->
    assert (Reg.is_reg i.arg.(0))
  | RM_to_R ->
    assert (Reg.is_reg i.res.(0))
  | R_to_R ->
    assert (Reg.is_reg i.arg.(0) && Reg.is_reg i.res.(0))
  | R_RM_to_fst ->
    assert (Reg.same_loc i.arg.(0) i.res.(0));
    assert (Reg.is_reg i.arg.(0))
  | R_RM_to_R ->
    assert (Reg.is_reg i.arg.(0));
    assert (Reg.is_reg i.res.(0))
  | R_RM_xmm0_to_fst ->
    assert (Reg.same_loc i.arg.(0) i.res.(0));
    assert (Reg.is_reg i.arg.(0));
    assert (Reg.same_loc i.arg.(2) (phys_xmm0v ()))
  | R_R_to_fst ->
    assert (Reg.same_loc i.arg.(0) i.res.(0));
    assert (Reg.is_reg i.arg.(0) && Reg.is_reg i.arg.(1))
  | R_RM_rax_rdx_to_rcx ->
    assert (Reg.is_reg i.arg.(0));
    assert (Reg.same_loc (i.arg.(2)) phys_rax);
    assert (Reg.same_loc (i.arg.(3)) phys_rdx);
    assert (Reg.same_loc i.res.(0) phys_rcx)
  | R_RM_to_rcx ->
    assert (Reg.is_reg i.arg.(0));
    assert (Reg.same_loc (i.res.(0)) phys_rcx)
  | R_RM_rax_rdx_to_xmm0 ->
    assert (Reg.is_reg i.arg.(0));
    assert (Reg.same_loc (i.arg.(2)) phys_rax);
    assert (Reg.same_loc (i.arg.(3)) phys_rdx);
    assert (Reg.same_loc (i.res.(0)) (phys_xmm0v ()))
  | R_RM_to_xmm0 ->
    assert (Reg.is_reg i.arg.(0));
    assert (Reg.same_loc i.res.(0) (phys_xmm0v ()))
  );
  ()

let emit_simd_instr_with_memory_arg op i addressing_mode =
  check_simd_instr (Simd_proc.Mem.register_behavior op) i;
  let addr = addressing addressing_mode VEC128 i 1 in
  match (op : Simd.Mem.operation) with
  | SSE2 Add_f64 -> I.addpd addr (res i 0)
  | SSE2 Sub_f64 -> I.subpd addr (res i 0)
  | SSE2 Mul_f64 -> I.mulpd addr (res i 0)
  | SSE2 Div_f64 -> I.divpd addr (res i 0)
  | SSE Add_f32 -> I.addps addr (res i 0)
  | SSE Sub_f32 -> I.subps addr (res i 0)
  | SSE Mul_f32 -> I.mulps addr (res i 0)
  | SSE Div_f32 -> I.divps addr (res i 0)

let emit_simd_instr op i =
  check_simd_instr (Simd_proc.register_behavior op) i;
  match (op : Simd.operation) with
  | CLMUL (Clmul_64 n) -> I.pclmulqdq (X86_dsl.int n) (arg i 1) (res i 0)
  | BMI2 Extract_64 -> I.pext (arg i 1) (arg i 0) (res i 0)
  | BMI2 Deposit_64 -> I.pdep (arg i 1) (arg i 0) (res i 0)
  | SSE Round_current_f32_i64 -> I.cvtss2si (arg i 0) (res i 0)
  | SSE Sqrt_scalar_f32 ->
    if arg i 0 <> res i 0 then
      I.xorpd (res i 0) (res i 0); (* avoid partial register stall *)
    I.sqrtss (arg i 0) (res i 0)
  | SSE Max_scalar_f32 -> I.maxss (arg i 1) (res i 0)
  | SSE Min_scalar_f32 -> I.minss (arg i 1) (res i 0)
  | SSE (Cmp_f32 n) -> I.cmpps n (arg i 1) (res i 0)
  | SSE Add_f32 -> I.addps (arg i 1) (res i 0)
  | SSE Sub_f32 -> I.subps (arg i 1) (res i 0)
  | SSE Mul_f32 -> I.mulps (arg i 1) (res i 0)
  | SSE Div_f32 -> I.divps (arg i 1) (res i 0)
  | SSE Max_f32 -> I.maxps (arg i 1) (res i 0)
  | SSE Min_f32 -> I.minps (arg i 1) (res i 0)
  | SSE Rcp_f32 -> I.rcpps (arg i 0) (res i 0)
  | SSE Sqrt_f32 -> I.sqrtps (arg i 0) (res i 0)
  | SSE Rsqrt_f32 -> I.rsqrtps (arg i 0) (res i 0)
  | SSE High_64_to_low_64 -> I.movhlps (arg i 1) (res i 0)
  | SSE Low_64_to_high_64 -> I.movlhps (arg i 1) (res i 0)
  | SSE Interleave_high_32 -> I.unpckhps (arg i 1) (res i 0)
  | SSE (Interleave_low_32 | Interleave_low_32_regs) ->
    I.unpcklps (arg i 1) (res i 0)
  | SSE Movemask_32 -> I.movmskps (arg i 0) (res i 0)
  | SSE (Shuffle_32 n) -> I.shufps (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE2 Max_scalar_f64 -> I.maxsd (arg i 1) (res i 0)
  | SSE2 Min_scalar_f64 -> I.minsd (arg i 1) (res i 0)
  | SSE2 Sqrt_scalar_f64 ->
    if arg i 0 <> res i 0 then
      I.xorpd (res i 0) (res i 0); (* avoid partial register stall *)
    I.sqrtsd (arg i 0) (res i 0)
  | SSE2 Sqrt_f64 -> I.sqrtpd (arg i 0) (res i 0)
  | SSE2 Add_i8 -> I.paddb (arg i 1) (res i 0)
  | SSE2 Add_i16 -> I.paddw (arg i 1) (res i 0)
  | SSE2 Add_i32 -> I.paddd (arg i 1) (res i 0)
  | SSE2 Add_i64 -> I.paddq (arg i 1) (res i 0)
  | SSE2 Add_f64 -> I.addpd (arg i 1) (res i 0)
  | SSE2 Add_saturating_i8 -> I.paddsb (arg i 1) (res i 0)
  | SSE2 Add_saturating_i16 -> I.paddsw (arg i 1) (res i 0)
  | SSE2 Add_saturating_unsigned_i8 -> I.paddusb (arg i 1) (res i 0)
  | SSE2 Add_saturating_unsigned_i16 -> I.paddusw (arg i 1) (res i 0)
  | SSE2 Sub_i8 -> I.psubb (arg i 1) (res i 0)
  | SSE2 Sub_i16 -> I.psubw (arg i 1) (res i 0)
  | SSE2 Sub_i32 -> I.psubd (arg i 1) (res i 0)
  | SSE2 Sub_i64 -> I.psubq (arg i 1) (res i 0)
  | SSE2 Sub_f64 -> I.subpd (arg i 1) (res i 0)
  | SSE2 Sub_saturating_i8 -> I.psubsb (arg i 1) (res i 0)
  | SSE2 Sub_saturating_i16 -> I.psubsw (arg i 1) (res i 0)
  | SSE2 Sub_saturating_unsigned_i8 -> I.psubusb (arg i 1) (res i 0)
  | SSE2 Sub_saturating_unsigned_i16 -> I.psubusw (arg i 1) (res i 0)
  | SSE2 Max_unsigned_i8 -> I.pmaxub (arg i 1) (res i 0)
  | SSE2 Max_i16 -> I.pmaxsw (arg i 1) (res i 0)
  | SSE2 Max_f64 -> I.maxpd (arg i 1) (res i 0)
  | SSE2 Min_unsigned_i8 -> I.pminub (arg i 1) (res i 0)
  | SSE2 Min_i16 -> I.pminsw (arg i 1) (res i 0)
  | SSE2 Min_f64 -> I.minpd (arg i 1) (res i 0)
  | SSE2 Mul_f64 -> I.mulpd (arg i 1) (res i 0)
  | SSE2 Div_f64 -> I.divpd (arg i 1) (res i 0)
  | SSE2 Avg_unsigned_i8 -> I.pavgb (arg i 1) (res i 0)
  | SSE2 Avg_unsigned_i16 -> I.pavgw (arg i 1) (res i 0)
  | SSE2 SAD_unsigned_i8 -> I.psadbw (arg i 1) (res i 0)
  | SSE2 Mulhi_i16 -> I.pmulhw (arg i 1) (res i 0)
  | SSE2 Mulhi_unsigned_i16 -> I.pmulhuw (arg i 1) (res i 0)
  | SSE2 Mullo_i16 -> I.pmullw (arg i 1) (res i 0)
  | SSE2 Mul_hadd_i16_to_i32 -> I.pmaddwd (arg i 1) (res i 0)
  | SSE2 And_bits -> I.pand (arg i 1) (res i 0)
  | SSE2 Andnot_bits -> I.pandnot (arg i 1) (res i 0)
  | SSE2 Or_bits -> I.por (arg i 1) (res i 0)
  | SSE2 Xor_bits -> I.pxor (arg i 1) (res i 0)
  | SSE2 Movemask_8 -> I.pmovmskb (arg i 0) (res i 0)
  | SSE2 Movemask_64 -> I.movmskpd (arg i 0) (res i 0)
  | SSE2 (Shift_left_bytes n) -> I.pslldq (X86_dsl.int n) (arg i 0)
  | SSE2 (Shift_right_bytes n) -> I.psrldq (X86_dsl.int n) (arg i 0)
  | SSE2 Cmpeq_i8 -> I.pcmpeqb (arg i 1) (res i 0)
  | SSE2 Cmpeq_i16 -> I.pcmpeqw (arg i 1) (res i 0)
  | SSE2 Cmpeq_i32 -> I.pcmpeqd (arg i 1) (res i 0)
  | SSE2 Cmpgt_i8 -> I.pcmpgtb (arg i 1) (res i 0)
  | SSE2 Cmpgt_i16 -> I.pcmpgtw (arg i 1) (res i 0)
  | SSE2 Cmpgt_i32 -> I.pcmpgtd (arg i 1) (res i 0)
  | SSE2 (Cmp_f64 n) -> I.cmppd n (arg i 1) (res i 0)
  | SSE2 I32_to_f64 -> I.cvtdq2pd (arg i 0) (res i 0)
  | SSE2 I32_to_f32 -> I.cvtdq2ps (arg i 0) (res i 0)
  | SSE2 F64_to_i32 -> I.cvtpd2dq (arg i 0) (res i 0)
  | SSE2 F64_to_f32 -> I.cvtpd2ps (arg i 0) (res i 0)
  | SSE2 F32_to_i32 -> I.cvtps2dq (arg i 0) (res i 0)
  | SSE2 F32_to_f64 -> I.cvtps2pd (arg i 0) (res i 0)
  | SSE2 I16_to_i8 -> I.packsswb (arg i 1) (res i 0)
  | SSE2 I32_to_i16 -> I.packssdw (arg i 1) (res i 0)
  | SSE2 I16_to_unsigned_i8 -> I.packuswb (arg i 1) (res i 0)
  | SSE2 I32_to_unsigned_i16 -> I.packusdw (arg i 1) (res i 0)
  | SSE2 Round_current_f64_i64 -> I.cvtsd2si (arg i 0) (res i 0)
  | SSE2 SLL_i16 -> I.psllw (arg i 1) (res i 0)
  | SSE2 SLL_i32 -> I.pslld (arg i 1) (res i 0)
  | SSE2 SLL_i64 -> I.psllq (arg i 1) (res i 0)
  | SSE2 SRL_i16 -> I.psrlw (arg i 1) (res i 0)
  | SSE2 SRL_i32 -> I.psrld (arg i 1) (res i 0)
  | SSE2 SRL_i64 -> I.psrlq (arg i 1) (res i 0)
  | SSE2 SRA_i16 -> I.psraw (arg i 1) (res i 0)
  | SSE2 SRA_i32 -> I.psrad (arg i 1) (res i 0)
  | SSE2 (SLLi_i16 n) -> I.psllwi (X86_dsl.int n) (res i 0)
  | SSE2 (SLLi_i32 n) -> I.pslldi (X86_dsl.int n) (res i 0)
  | SSE2 (SLLi_i64 n) -> I.psllqi (X86_dsl.int n) (res i 0)
  | SSE2 (SRLi_i16 n) -> I.psrlwi (X86_dsl.int n) (res i 0)
  | SSE2 (SRLi_i32 n) -> I.psrldi (X86_dsl.int n) (res i 0)
  | SSE2 (SRLi_i64 n) -> I.psrlqi (X86_dsl.int n) (res i 0)
  | SSE2 (SRAi_i16 n) -> I.psrawi (X86_dsl.int n) (res i 0)
  | SSE2 (SRAi_i32 n) -> I.psradi (X86_dsl.int n) (res i 0)
  | SSE2 (Shuffle_64 n) -> I.shufpd (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE2 (Shuffle_high_16 n) -> I.pshufhw (X86_dsl.int n) (arg i 0) (res i 0)
  | SSE2 (Shuffle_low_16 n) -> I.pshuflw (X86_dsl.int n) (arg i 0) (res i 0)
  | SSE2 Interleave_high_8 -> I.punpckhbw (arg i 1) (res i 0)
  | SSE2 Interleave_high_16 -> I.punpckhwd (arg i 1) (res i 0)
  | SSE2 Interleave_high_64 -> I.punpckhqdq (arg i 1) (res i 0)
  | SSE2 Interleave_low_8 -> I.punpcklbw (arg i 1) (res i 0)
  | SSE2 Interleave_low_16 -> I.punpcklwd (arg i 1) (res i 0)
  | SSE2 Interleave_low_64 -> I.punpcklqdq (arg i 1) (res i 0)
  | SSE3 Addsub_f32 -> I.addsubps (arg i 1) (res i 0)
  | SSE3 Addsub_f64 -> I.addsubpd (arg i 1) (res i 0)
  | SSE3 Hadd_f32 -> I.haddps (arg i 1) (res i 0)
  | SSE3 Hadd_f64 -> I.haddpd (arg i 1) (res i 0)
  | SSE3 Hsub_f32 -> I.hsubps (arg i 1) (res i 0)
  | SSE3 Hsub_f64 -> I.hsubpd (arg i 1) (res i 0)
  | SSE3 Dup_low_64 -> I.movddup (arg i 0) (res i 0)
  | SSE3 Dup_odd_32 -> I.movshdup (arg i 0) (res i 0)
  | SSE3 Dup_even_32 -> I.movsldup (arg i 0) (res i 0)
  | SSSE3 Abs_i8 -> I.pabsb (arg i 0) (res i 0)
  | SSSE3 Abs_i16 -> I.pabsw (arg i 0) (res i 0)
  | SSSE3 Abs_i32 -> I.pabsd (arg i 0) (res i 0)
  | SSSE3 Hadd_i16 -> I.phaddw (arg i 1) (res i 0)
  | SSSE3 Hadd_i32 -> I.phaddd (arg i 1) (res i 0)
  | SSSE3 Hadd_saturating_i16 -> I.phaddsw (arg i 1) (res i 0)
  | SSSE3 Hsub_i16 -> I.phsubw (arg i 1) (res i 0)
  | SSSE3 Hsub_i32 -> I.phsubd (arg i 1) (res i 0)
  | SSSE3 Hsub_saturating_i16 -> I.phsubsw (arg i 1) (res i 0)
  | SSSE3 Mulsign_i8 -> I.psignb (arg i 1) (res i 0)
  | SSSE3 Mulsign_i16 -> I.psignw (arg i 1) (res i 0)
  | SSSE3 Mulsign_i32 -> I.psignd (arg i 1) (res i 0)
  | SSSE3 Alignr_i8 n -> I.palignr (X86_dsl.int n) (arg i 1) (res i 0)
  | SSSE3 Shuffle_8 -> I.pshufb (arg i 1) (res i 0)
  | SSSE3 Mul_unsigned_hadd_saturating_i8_to_i16 ->
    I.pmaddubsw (arg i 1) (res i 0)
  | SSE41 (Blend_16 n) -> I.pblendw (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 (Blend_32 n) -> I.blendps (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 (Blend_64 n) -> I.blendpd (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 Blendv_8 -> I.pblendvb (arg i 1) (res i 0)
  | SSE41 Blendv_32 -> I.blendvps (arg i 1) (res i 0)
  | SSE41 Blendv_64 -> I.blendvpd (arg i 1) (res i 0)
  | SSE41 Cmpeq_i64 -> I.pcmpeqq (arg i 1) (res i 0)
  | SSE41 I8_sx_i16 -> I.pmovsxbw (arg i 0) (res i 0)
  | SSE41 I8_sx_i32 -> I.pmovsxbd (arg i 0) (res i 0)
  | SSE41 I8_sx_i64 -> I.pmovsxbq (arg i 0) (res i 0)
  | SSE41 I16_sx_i32 -> I.pmovsxwd (arg i 0) (res i 0)
  | SSE41 I16_sx_i64 -> I.pmovsxwq (arg i 0) (res i 0)
  | SSE41 I32_sx_i64 -> I.pmovsxdq (arg i 0) (res i 0)
  | SSE41 I8_zx_i16 -> I.pmovzxbw (arg i 0) (res i 0)
  | SSE41 I8_zx_i32 -> I.pmovzxbd (arg i 0) (res i 0)
  | SSE41 I8_zx_i64 -> I.pmovzxbq (arg i 0) (res i 0)
  | SSE41 I16_zx_i32 -> I.pmovzxwd (arg i 0) (res i 0)
  | SSE41 I16_zx_i64 -> I.pmovzxwq (arg i 0) (res i 0)
  | SSE41 I32_zx_i64 -> I.pmovzxdq (arg i 0) (res i 0)
  | SSE41 (Dp_f32 n) -> I.dpps (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 (Dp_f64 n) -> I.dppd (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 (Extract_i8 n) ->
    (* CR mslater: (SIMD) change once we have unboxed int8 *)
    I.pextrb (X86_dsl.int n) (arg i 0) (res i 0);
    I.movzx (res8 i 0) (res i 0)
  | SSE41 (Extract_i16 n) ->
    (* CR mslater: (SIMD) change once we have unboxed int16,
       the result will not need to zero-extend. *)
    I.pextrw (X86_dsl.int n) (arg i 0) (res i 0);
    I.movzx (res16 i 0) (res i 0)
  | SSE41 (Extract_i32 n) -> I.pextrd (X86_dsl.int n) (arg i 0) (res32 i 0)
  | SSE41 (Extract_i64 n) -> I.pextrq (X86_dsl.int n) (arg i 0) (res i 0)
  | SSE41 (Insert_i8 n) -> I.pinsrb (X86_dsl.int n) (arg32 i 1) (res i 0)
  | SSE41 (Insert_i16 n) -> I.pinsrw (X86_dsl.int n) (arg32 i 1) (res i 0)
  | SSE41 (Insert_i32 n) -> I.pinsrd (X86_dsl.int n) (arg32 i 1) (res i 0)
  | SSE41 (Insert_i64 n) -> I.pinsrq (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 Max_i8 -> I.pmaxsb (arg i 1) (res i 0)
  | SSE41 Max_i32 -> I.pmaxsd (arg i 1) (res i 0)
  | SSE41 Max_unsigned_i16 -> I.pmaxuw (arg i 1) (res i 0)
  | SSE41 Max_unsigned_i32 -> I.pmaxud (arg i 1) (res i 0)
  | SSE41 Min_i8 -> I.pminsb (arg i 1) (res i 0)
  | SSE41 Min_i32 -> I.pminsd (arg i 1) (res i 0)
  | SSE41 Min_unsigned_i16 -> I.pminuw (arg i 1) (res i 0)
  | SSE41 Min_unsigned_i32 -> I.pminud (arg i 1) (res i 0)
  | SSE41 (Round_scalar_f64 n) ->
    if arg i 0 <> res i 0 then
      I.xorpd (res i 0) (res i 0); (* avoid partial register stall *)
    I.roundsd n (arg i 0) (res i 0)
  | SSE41 (Round_scalar_f32 n) ->
    if arg i 0 <> res i 0 then
      I.xorpd (res i 0) (res i 0); (* avoid partial register stall *)
    I.roundss n (arg i 0) (res i 0)
  | SSE41 (Round_f64 n) -> I.roundpd n (arg i 0) (res i 0)
  | SSE41 (Round_f32 n) -> I.roundps n (arg i 0) (res i 0)
  | SSE41 (Multi_sad_unsigned_i8 n) -> I.mpsadbw (X86_dsl.int n) (arg i 1) (res i 0)
  | SSE41 Minpos_unsigned_i16 -> I.phminposuw (arg i 0) (res i 0)
  | SSE41 Mullo_i32 -> I.pmulld (arg i 1) (res i 0)
  | SSE42 Cmpgt_i64 -> I.pcmpgtq (arg i 1) (res i 0)
  | SSE42 Crc32_64 -> I.crc32 (arg i 1) (res i 0)
  | SSE42 (Cmpestrm n) -> I.pcmpestrm (X86_dsl.int n) (arg i 1) (arg i 0)
  | SSE42 (Cmpistrm n) -> I.pcmpistrm (X86_dsl.int n) (arg i 1) (arg i 0)
  | SSE42 (Cmpestri n) -> I.pcmpestri (X86_dsl.int n) (arg i 1) (arg i 0)
  | SSE42 (Cmpistri n) -> I.pcmpistri (X86_dsl.int n) (arg i 1) (arg i 0)
  | SSE42 (Cmpestra n) ->
    I.pcmpestri (X86_dsl.int n) (arg i 1) (arg i 0); I.set A (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpestrc n) ->
    I.pcmpestri (X86_dsl.int n) (arg i 1) (arg i 0); I.set B (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpestro n) ->
    I.pcmpestri (X86_dsl.int n) (arg i 1) (arg i 0); I.set O (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpestrs n) ->
    I.pcmpestri (X86_dsl.int n) (arg i 1) (arg i 0); I.set S (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpestrz n) ->
    I.pcmpestri (X86_dsl.int n) (arg i 1) (arg i 0); I.set E (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpistra n) ->
    I.pcmpistri (X86_dsl.int n) (arg i 1) (arg i 0); I.set A (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpistrc n) ->
    I.pcmpistri (X86_dsl.int n) (arg i 1) (arg i 0); I.set B (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpistro n) ->
    I.pcmpistri (X86_dsl.int n) (arg i 1) (arg i 0); I.set O (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpistrs n) ->
    I.pcmpistri (X86_dsl.int n) (arg i 1) (arg i 0); I.set S (res8 i 0); I.movzx (res8 i 0) (res i 0)
  | SSE42 (Cmpistrz n) ->
    I.pcmpistri (X86_dsl.int n) (arg i 1) (arg i 0); I.set E (res8 i 0); I.movzx (res8 i 0) (res i 0)

(* Emit an instruction *)
let emit_instr ~first ~fallthrough i =
  emit_debug_info_linear i;
  match i.desc with
  | Lend -> ()
  | Lprologue ->
    assert (!prologue_required);
    if fp then begin
      I.push rbp;
      cfi_adjust_cfa_offset 8;
      I.mov rsp rbp;
    end;
    if !frame_required then begin
      let n = frame_size() - 8 - (if fp then 8 else 0) in
      if n <> 0
      then begin
        I.sub (int n) rsp;
        cfi_adjust_cfa_offset n;
      end;
    end
  | Lop(Move | Spill | Reload) ->
      move i.arg.(0) i.res.(0)
  | Lop(Const_int n) ->
      if n = 0n then begin
        match i.res.(0).loc with
        | Reg _ ->
          (* Clearing the bottom half also clears the top half (except for
             64-bit-only registers where the behaviour is as if the operands
             were 64 bit). *)
          I.xor (res32 i 0) (res32 i 0)
        | _ ->
          I.mov (int 0) (res i 0)
      end else if n > 0n && n <= 0xFFFF_FFFFn then begin
        match i.res.(0).loc with
        | Reg _ ->
          (* Similarly, setting only the bottom half clears the top half. *)
          I.mov (nat n) (res32 i 0)
        | _ ->
          I.mov (nat n) (res i 0)
      end else
        I.mov (nat n) (res i 0)
  | Lop(Const_float32 f) ->
      begin match f with
      | 0x0000_0000l ->       (* +0.0 *)
          I.xorpd (res i 0) (res i 0)
      | _ ->
          (* float32 constants still take up 8 bytes; we load the lower half. *)
          let lbl = add_float_constant (Int64.of_int32 f) in
          I.movss (mem64_rip REAL4 (emit_label lbl)) (res i 0)
      end
  | Lop(Const_float f) ->
      begin match f with
      | 0x0000_0000_0000_0000L ->       (* +0.0 *)
          I.xorpd (res i 0) (res i 0)
      | _ ->
          let lbl = add_float_constant f in
          I.movsd (mem64_rip REAL8 (emit_label lbl)) (res i 0)
      end
  | Lop(Const_vec128 {high; low}) ->
      begin match (high, low) with
      | 0x0000_0000_0000_0000L, 0x0000_0000_0000_0000L ->
          I.xorpd (res i 0) (res i 0)
      | _ ->
          let lbl = add_vec128_constant {high; low} in
          I.movapd (mem64_rip VEC128 (emit_label lbl)) (res i 0)
      end
  | Lop(Const_symbol s) ->
      add_used_symbol s.sym_name;
      load_symbol_addr s (res i 0)
  | Lcall_op(Lcall_ind) ->
      I.call (arg i 0);
      record_frame i.live (Dbg_other i.dbg)
  | Lcall_op(Lcall_imm { func; }) ->
      add_used_symbol func.sym_name;
      emit_call func;
      record_frame i.live (Dbg_other i.dbg)
  | Lcall_op(Ltailcall_ind) ->
      output_epilogue (fun () -> I.jmp (arg i 0))
  | Lcall_op(Ltailcall_imm { func; }) ->
      begin
        if func.sym_name = !function_name then
          match !tailrec_entry_point with
          | None -> Misc.fatal_error "jump to missing tailrec entry point"
          | Some tailrec_entry_point -> I.jmp (label tailrec_entry_point)
        else begin
          output_epilogue begin fun () ->
            add_used_symbol func.sym_name;
            emit_jump func
          end
        end
      end
  | Lcall_op(Lextcall { func; alloc; stack_ofs }) ->
      add_used_symbol func;
      if Config.runtime5 && stack_ofs > 0 then begin
        I.mov rsp r13;
        I.lea (mem64 QWORD stack_ofs RSP) r12;
        load_symbol_addr (Cmm.global_symbol func) rax;
        emit_call (Cmm.global_symbol "caml_c_call_stack_args");
        record_frame i.live (Dbg_other i.dbg);
      end else if alloc then begin
        load_symbol_addr (Cmm.global_symbol func) rax;
        emit_call (Cmm.global_symbol "caml_c_call");
        record_frame i.live (Dbg_other i.dbg);
        if not Config.runtime5 && system <> S_win64 then begin

          (* In amd64.S, "caml_c_call" tail-calls the C function (in order to
             produce nicer backtraces), so we need to restore r15 manually after
             it returns (note that this increases code size).

             In amd64nt.asm (used for Win64), "caml_c_call" invokes the C
             function via a regular call, and restores r15 itself, thus avoiding
             the code size increase. *)

          I.mov (domain_field Domainstate.Domain_young_ptr) r15
        end
      end else begin
        if Config.runtime5 then begin
          I.mov rsp rbx;
          cfi_remember_state ();
          cfi_def_cfa_register "rbx";
          (* NB: gdb has asserts on contiguous stacks that mean it
            will not unwind through this unless we were to tag this
            calling frame with cfi_signal_frame in it's definition. *)
          I.mov (domain_field Domainstate.Domain_c_stack) rsp;
        end;
        emit_call (Cmm.global_symbol func);
        if Config.runtime5 then begin
          I.mov rbx rsp;
          cfi_restore_state ();
        end;
           end
  | Lop(Stackoffset n) ->
      emit_stack_offset n
  | Lop(Load { memory_chunk; addressing_mode; _ }) ->
      let dest = res i 0 in
      begin match memory_chunk with
      | Word_int | Word_val ->
          I.mov (addressing addressing_mode QWORD i 0) dest
      | Byte_unsigned ->
          I.movzx (addressing addressing_mode BYTE i 0) dest
      | Byte_signed ->
          I.movsx (addressing addressing_mode BYTE i 0) dest
      | Sixteen_unsigned ->
          I.movzx (addressing addressing_mode WORD i 0) dest
      | Sixteen_signed ->
          I.movsx (addressing addressing_mode WORD i 0) dest
      | Thirtytwo_unsigned ->
          I.mov (addressing addressing_mode DWORD i 0) (res32 i 0)
      | Thirtytwo_signed ->
          I.movsxd (addressing addressing_mode DWORD i 0) dest
      | Onetwentyeight_unaligned ->
          I.movupd (addressing addressing_mode VEC128 i 0) dest
      | Onetwentyeight_aligned ->
          I.movapd (addressing addressing_mode VEC128 i 0) dest
      | Single { reg = Float64 } ->
          I.cvtss2sd (addressing addressing_mode REAL4 i 0) dest
      | Single { reg = Float32 } ->
          I.movss (addressing addressing_mode REAL4 i 0) dest
      | Double ->
          I.movsd (addressing addressing_mode REAL8 i 0) dest
      end
  | Lop(Store(chunk, addr, _)) ->
      begin match chunk with
      | Word_int | Word_val ->
          I.mov (arg i 0) (addressing addr QWORD i 1)
      | Byte_unsigned | Byte_signed ->
          I.mov (arg8 i 0) (addressing addr BYTE i 1)
      | Sixteen_unsigned | Sixteen_signed ->
          I.mov (arg16 i 0) (addressing addr WORD i 1)
      | Thirtytwo_signed | Thirtytwo_unsigned ->
          I.mov (arg32 i 0) (addressing addr DWORD i 1)
      | Onetwentyeight_unaligned ->
          I.movupd (arg i 0) (addressing addr VEC128 i 1)
      | Onetwentyeight_aligned ->
          I.movapd (arg i 0) (addressing addr VEC128 i 1)
      | Single { reg = Float64 } ->
          I.cvtsd2ss (arg i 0) xmm15;
          I.movss xmm15 (addressing addr REAL4 i 1)
      | Single { reg = Float32 } ->
          I.movss (arg i 0) (addressing addr REAL4 i 1)
      | Double ->
          I.movsd (arg i 0) (addressing addr REAL8 i 1)
      end
  | Lop(Alloc { bytes = n; dbginfo; mode = Heap }) ->
      assert (n <= (Config.max_young_wosize + 1) * Arch.size_addr);
      if !fastcode_flag then begin
        I.sub (int n) r15;
        I.cmp (domain_field Domainstate.Domain_young_limit) r15;
        let lbl_call_gc = new_label() in
        let lbl_frame =
          record_frame_label i.live (Dbg_alloc dbginfo)
        in
        I.jb (label lbl_call_gc);
        let lbl_after_alloc = new_label() in
        def_label lbl_after_alloc;
        I.lea (mem64 NONE 8 R15) (res i 0);
        call_gc_sites :=
          { gc_lbl = lbl_call_gc;
            gc_return_lbl = lbl_after_alloc;
            gc_dbg = i.dbg;
            gc_frame = lbl_frame; } :: !call_gc_sites
      end else begin
        begin match n with
        | 16 -> emit_call (Cmm.global_symbol "caml_alloc1")
        | 24 -> emit_call (Cmm.global_symbol "caml_alloc2")
        | 32 -> emit_call (Cmm.global_symbol "caml_alloc3")
        | _  ->
            I.sub (int n) r15;
            emit_call (Cmm.global_symbol "caml_allocN")
        end;
        let label = record_frame_label i.live (Dbg_alloc dbginfo) in
        def_label label;
        I.lea (mem64 NONE 8 R15) (res i 0)
      end
  | Lop(Alloc { bytes = n; dbginfo=_; mode = Local }) ->
      let r = res i 0 in
      I.mov (domain_field Domainstate.Domain_local_sp) r;
      I.sub (int n) r;
      I.mov r (domain_field Domainstate.Domain_local_sp);
      I.cmp (domain_field Domainstate.Domain_local_limit) r;
      let lbl_call = new_label () in
      I.j L (label lbl_call);
      let lbl_after_alloc = new_label () in
      def_label lbl_after_alloc;
      I.add (domain_field Domainstate.Domain_local_top) r;
      I.add (int 8) r;
      local_realloc_sites :=
        { lr_lbl = lbl_call;
          lr_dbg = i.dbg;
          lr_return_lbl = lbl_after_alloc } :: !local_realloc_sites
  | Lop (Poll) ->
      I.cmp (domain_field Domainstate.Domain_young_limit) r15;
      let gc_call_label = new_label () in
      let lbl_after_poll = new_label() in
      let lbl_frame =
        record_frame_label i.live (Dbg_alloc [])
      in
      I.jbe (label gc_call_label);
      call_gc_sites :=
        { gc_lbl = gc_call_label;
          gc_return_lbl = lbl_after_poll;
          gc_dbg = i.dbg;
          gc_frame = lbl_frame; } :: !call_gc_sites;
      def_label lbl_after_poll
  | Lop(Intop(Icomp cmp)) ->
      I.cmp (arg i 1) (arg i 0);
      I.set (cond cmp) al;
      I.movzx al (res i 0)
  | Lop(Intop_imm(Icomp cmp, n)) ->
      I.cmp (int n) (arg i 0);
      I.set (cond cmp) al;
      I.movzx al (res i 0)
  | Lop(Intop_imm (Iand, n)) when n >= 0 && n <= 0xFFFF_FFFF && Reg.is_reg i.res.(0) ->
      I.and_ (int n) (res32 i 0)
  | Lop(Intop Ixor) when i.arg.(1).loc = i.res.(0).loc && Reg.is_reg i.res.(0) ->
      I.xor (res32 i 0) (res32 i 0)
  | Lop(Intop(Idiv | Imod)) ->
      I.cqo ();
      I.idiv (arg i 1)
  | Lop(Intop(Ilsl | Ilsr | Iasr as op)) ->
      (* We have i.arg.(0) = i.res.(0) and i.arg.(1) = %rcx *)
      instr_for_intop op cl (res i 0)
  | Lop(Intop (Imulh { signed = true })) ->
      I.imul (arg i 1) None
  | Lop(Intop (Imulh { signed = false })) ->
      I.mul (arg i 1)
  | Lop(Intop ((Iadd|Isub|Imul|Iand|Ior|Ixor) as op)) ->
      (* We have i.arg.(0) = i.res.(0) *)
      instr_for_intop op (arg i 1) (res i 0)
  | Lop(Intop_imm(Iadd, n)) when i.arg.(0).loc <> i.res.(0).loc ->
      I.lea (mem64 NONE n (arg64 i 0)) (res i 0)
  | Lop(Intop_imm(Iadd, 1) | Intop_imm(Isub, -1)) ->
      I.inc (res i 0)
  | Lop(Intop_imm(Iadd, -1) | Intop_imm(Isub, 1)) ->
      I.dec (res i 0)
  | Lop(Intop_imm(op, n)) ->
      (* We have i.arg.(0) = i.res.(0) *)
      instr_for_intop op (int n) (res i 0)
  | Lop(Intop_atomic{op; size; addr}) ->
      emit_atomic i op size addr
  | Lop(Floatop(Float64, Icompf cmp)) ->
      let cond, need_swap = float_cond_and_need_swap cmp in
      let a0, a1 = if need_swap then arg i 1, arg i 0 else arg i 0, arg i 1 in
      I.cmpsd cond a1 a0;
      I.movq a0 (res i 0);
      I.neg (res i 0)
  | Lop(Floatop(Float32, Icompf cmp)) ->
      let cond, need_swap = float_cond_and_need_swap cmp in
      let a0, a1 = if need_swap then arg i 1, arg i 0 else arg i 0, arg i 1 in
      I.cmpss cond a1 a0;
      I.movd a0 (res32 i 0);
      (* CMPSS only sets the bottom 32 bits of the result, so we sign-extend to
         copy the result to the top 32 bits. *)
      I.movsxd (res32 i 0) (res i 0);
      I.neg (res i 0)
  | Lop(Floatop(Float64, Inegf)) ->
      I.xorpd (mem64_rip VEC128 (emit_symbol "caml_negf_mask")) (res i 0)
  | Lop(Floatop(Float64, Iabsf)) ->
      I.andpd (mem64_rip VEC128 (emit_symbol "caml_absf_mask")) (res i 0)
  | Lop(Floatop(Float32, Inegf)) ->
      I.xorps (mem64_rip VEC128 (emit_symbol "caml_negf32_mask")) (res i 0)
  | Lop(Floatop(Float32, Iabsf)) ->
      I.andps (mem64_rip VEC128 (emit_symbol "caml_absf32_mask")) (res i 0)
  | Lop(Floatop(width, (Iaddf | Isubf | Imulf | Idivf as floatop))) ->
      instr_for_floatop width floatop (arg i 1) (res i 0)
  | Lop(Opaque) ->
      assert (i.arg.(0).loc = i.res.(0).loc)
  | Lop(Specific(Ilea addr)) ->
      I.lea (addressing addr NONE i 0) (res i 0)
  | Lop(Specific(Istore_int(n, addr, _))) ->
      I.mov (nat n) (addressing addr QWORD i 0)
  | Lop(Specific(Ioffset_loc(n, addr))) ->
      I.add (int n) (addressing addr QWORD i 0)
  | Lop(Specific(Ifloatarithmem(Float64, op, addr))) ->
      instr_for_floatarithmem Float64 op (addressing addr REAL8 i 1) (res i 0)
  | Lop(Specific(Ifloatarithmem(Float32, op, addr))) ->
      instr_for_floatarithmem Float32 op (addressing addr REAL4 i 1) (res i 0)
  | Lop(Specific(Ibswap { bitwidth = Sixteen })) ->
      I.xchg ah al;
      I.movzx (res16 i 0) (res i 0)
  | Lop(Specific(Ibswap { bitwidth = Thirtytwo })) ->
      I.bswap (res32 i 0);
  | Lop(Specific(Ibswap { bitwidth = Sixtyfour })) ->
      I.bswap (res i 0)
  | Lop(Specific(Isextend32)) ->
      I.movsxd (arg32 i 0) (res i 0)
  | Lop(Specific(Izextend32)) ->
      I.mov (arg32 i 0) (res32 i 0)
  | Lop(Intop(Iclz { arg_is_non_zero; })) ->
      (* CR-someday gyorsh: can we do it at selection?
         mshinwell: We need to address this and the similar CRs below.
         My feeling is that we should try to do this earlier, based on
         previous experience with similar things, but maybe the change
         should be left for later.
         mshinwell: The current situation is fine for now. *)
      if Arch.Extension.enabled LZCNT then begin
        I.lzcnt (arg i 0) (res i 0)
      end else if arg_is_non_zero then begin
        (* No need to handle that bsr is undefined on 0 input. *)
        I.bsr (arg i 0) (res i 0);
        (* We need (63 - result_of_bsr), which can be done with xor. *)
        I.xor (int 63) (res i 0)
      end else begin
        let lbl_z = new_label () in
        let lbl_nz = new_label () in
        I.bsr (arg i 0) (res i 0);
        I.je (label lbl_z);
        I.xor (int 63) (res i 0);
        I.jmp (label lbl_nz);
        def_label lbl_z;
        I.mov (int 64) (res i 0);
        def_label lbl_nz
      end
  | Lop(Intop(Ictz { arg_is_non_zero; })) ->
    (* CR-someday gyorsh: can we do it at selection? *)
    if Arch.Extension.enabled BMI then begin
      I.tzcnt (arg i 0) (res i 0)
    end else if arg_is_non_zero then begin
      (* No need to handle that bsf is undefined on 0 input. *)
      I.bsf (arg i 0) (res i 0)
    end else begin
      let lbl_nz = new_label () in
      I.bsf (arg i 0) (res i 0);
      I.jne (label lbl_nz);
      I.mov (int 64) (res i 0);
      def_label lbl_nz
    end
  | Lop(Intop Ipopcnt) ->
      assert (Arch.Extension.enabled POPCNT);
      I.popcnt (arg i 0) (res i 0)
  | Lop(Csel tst) ->
    let len = Array.length i.arg in
    let ifso = i.arg.(len - 2) in
    let ifnot = i.arg.(len - 1) in
    assert (Reg.same_loc ifnot i.res.(0));
    let taken c =
      I.cmov c (reg ifso) (res i 0)
    in
    emit_test i tst ~taken
  | Lop(Specific Irdtsc) ->
    I.rdtsc ();
    let rdx = Reg64 RDX in
    (* The instruction fills in the low 32 bits of the result registers. *)
    (* Combine edx and eax into a single 64-bit result. *)
    I.sal (int 32) rdx; (* shift edx to the high part of rdx *)
    (* On processors that support the Intel 64 architecture,
       the high-order 32 bits of each of RAX and RDX are cleared. *)
    (match reg64 i.res.(0) with
     | RAX -> I.or_ rdx (res i 0) (* combine high and low into rax *)
     | RDX -> I.or_ rax (res i 0) (* combine high and low into rdx *)
     | _ ->
       (* combine high and low into res *)
       I.mov rax (res i 0);
       I.or_ rdx (res i 0))
  | Lop(Specific Irdpmc) ->
    assert (arg64 i 0 = RCX);
    I.rdpmc ();
    let rdx = Reg64 RDX in
    (* The instruction fills in the low 32 bits of the result registers. *)
    (* Combine edx and eax into a single 64-bit result. *)
    I.sal (int 32) rdx; (* shift edx to the high part of rdx *)
    I.mov eax (res32 i 0); (* zero-extend eax *)
    I.or_ rdx (res i 0) (* combine high and low into rax *)
  | Lop (Specific Ilfence) ->
    I.lfence ()
  | Lop (Specific Isfence) ->
    I.sfence ()
  | Lop (Specific Imfence) ->
    I.mfence ()
  | Lop (Specific (Isimd op)) ->
    emit_simd_instr op i
  | Lop (Specific (Isimd_mem (op, addressing_mode))) ->
    emit_simd_instr_with_memory_arg op i addressing_mode
  | Lop (Static_cast cast) ->
    emit_static_cast cast i
  | Lop (Reinterpret_cast cast) ->
    emit_reinterpret_cast cast i
  | Lop (Specific Ipause) ->
    I.pause ()
  | Lop (Specific (Icldemote addr)) ->
    I.cldemote (addressing addr QWORD i 0)
  | Lop (Specific (Iprefetch { is_write; locality; addr; })) ->
    let locality =
      match locality with
      | Nonlocal -> Nta
      | Low -> T2
      | Moderate -> T1
      | High -> T0
    in
    I.prefetch is_write locality (addressing addr QWORD i 0)
  | Lop(Begin_region) ->
      I.mov (domain_field Domainstate.Domain_local_sp) (res i 0)
  | Lop(End_region) ->
      I.mov (arg i 0) (domain_field Domainstate.Domain_local_sp)
  | Lop (Name_for_debugger _) -> ()
  | Lcall_op (Lprobe { enabled_at_init; _ }) ->
    let probe_label = new_label () in
    let probe =
      { probe_label;
        probe_insn = i;
        stack_offset = !stack_offset;
        num_stack_slots = Array.copy num_stack_slots;
      }
    in
    probes := probe :: !probes;
    def_label probe_label;
    I.nop (); (* for uprobes and usdt probes as well *)
    (* A probe site does not directly call the probe handler.  There is an
       intervening wrapper that deals with getting the arguments to the probe
       in the correct place and managing the spilling/reloading of live
       registers.  See [emit_probe_handler_wrapper] below. *)
    emit_call_probe_handler_wrapper i ~enabled_at_init ~probe_label
  | Lop (Probe_is_enabled { name; }) ->
    let semaphore_sym = find_or_add_semaphore name None i.dbg in
    (* Load unsigned 2-byte integer value of the semaphore.
       According to the documentation [1],
       semaphores are of type unsigned short.
       [1] https://sourceware.org/systemtap/wiki/UserSpaceProbeImplementation
    *)
    (* OCaml has it's own semaphore, in addition to system tap,
       to control ocaml probe handlers independently from stap probe handlers.
       It is placed immediately after stap semaphore, and is the same
       size - hence offset 2. *)
    I.mov (addressing (Ibased(semaphore_sym, Global, 2)) WORD i 0) (res16 i 0);
    (* If the semaphore is 0, then the result is 0, otherwise 1. *)
    I.cmp (int 0) (res16 i 0);
    I.set (cond (Iunsigned Cne)) (res8 i 0);
    I.movzx (res8 i 0) (res i 0)
  | Lop (Dls_get) ->
    if Config.runtime5
    then I.mov (domain_field Domainstate.Domain_dls_root) (res i 0)
    else Misc.fatal_error "Dls is not supported in runtime4.";
  | Lreloadretaddr ->
      ()
  | Lreturn ->
      output_epilogue begin fun () ->
        I.ret ()
      end
  | Llabel { label = lbl; section_name } ->
      emit_Llabel fallthrough lbl section_name
  | Lbranch lbl ->
      I.jmp (label lbl)
  | Lcondbranch(tst, lbl) ->
      let lbl = label lbl in
      emit_test i tst ~taken:(fun c -> I.j c lbl)
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      I.cmp (int 1) (arg i 0);
      begin match lbl0 with
      | None -> ()
      | Some lbl -> I.jb (label lbl)
      end;
      begin match lbl1 with
      | None -> ()
      | Some lbl -> I.je (label lbl)
      end;
      begin match lbl2 with
      | None -> ()
      | Some lbl -> I.ja (label lbl)
      end
  | Lswitch jumptbl ->
      let lbl = emit_label (new_label()) in
      (* rax and rdx are clobbered by the Lswitch,
         meaning that no variable that is live across the Lswitch
         is assigned to rax or rdx.  However, the argument to Lswitch
         can still be assigned to one of these two registers, so
         we must be careful not to clobber it before use. *)
      let (tmp1, tmp2) =
        if i.arg.(0).loc = Reg 0 (* rax *)
        then (phys_rdx, phys_rax)
        else (phys_rax, phys_rdx) in

      I.lea (mem64_rip NONE lbl) (reg tmp1);
      I.movsxd (mem64 DWORD 0 (arg64 i 0) ~scale:4 ~base:(reg64 tmp1))
               (reg tmp2);
      I.add (reg tmp2) (reg tmp1);
      I.jmp (reg tmp1);

      let table =
        { table_lbl = lbl;
          elems = jumptbl }
      in
      jump_tables := table :: !jump_tables
  | Lentertrap ->
      if fp then begin
        let delta = frame_size () - 16 (* retaddr + rbp *) in
        I.lea (mem64 NONE delta RSP) rbp
      end
  | Ladjust_stack_offset { delta_bytes; } ->
      cfi_adjust_cfa_offset delta_bytes;
      stack_offset := !stack_offset + delta_bytes
  | Lpushtrap { lbl_handler; } ->
      emit_push_trap_label lbl_handler;
      let load_label_addr s arg =
        if !Clflags.pic_code then
          I.lea (mem64_rip NONE (emit_label s)) arg
        else
          I.mov (sym (emit_label s)) arg
      in
      load_label_addr lbl_handler r11;
      I.push r11;
      cfi_adjust_cfa_offset 8;
      I.push (domain_field Domainstate.Domain_exn_handler);
      cfi_adjust_cfa_offset 8;
      I.mov rsp (domain_field Domainstate.Domain_exn_handler);
      stack_offset := !stack_offset + 16;
  | Lpoptrap ->
      emit_pop_trap_label ();
      I.pop (domain_field Domainstate.Domain_exn_handler);
      cfi_adjust_cfa_offset (-8);
      I.add (int 8) rsp;
      cfi_adjust_cfa_offset (-8);
      stack_offset := !stack_offset - 16
  | Lraise k ->
      begin match k with
      | Lambda.Raise_regular ->
          I.mov (int 0) (domain_field Domainstate.Domain_backtrace_pos);
          emit_call (Cmm.global_symbol "caml_raise_exn");
          record_frame Reg.Set.empty (Dbg_raise i.dbg)
      | Lambda.Raise_reraise ->
          emit_call (Cmm.global_symbol
            (if Config.runtime5 then "caml_reraise_exn" else "caml_raise_exn"));
          record_frame Reg.Set.empty (Dbg_raise i.dbg)
      | Lambda.Raise_notrace ->
          I.mov (domain_field Domainstate.Domain_exn_handler) rsp;
          I.pop (domain_field Domainstate.Domain_exn_handler);
          I.pop r11;
          I.jmp r11
    end
  | Lstackcheck { max_frame_size_bytes; } ->
    emit_stack_check ~size_in_bytes:max_frame_size_bytes ~save_registers:(not first)

let emit_instr ~first ~fallthrough i =
  try emit_instr ~first ~fallthrough i
  with exn -> (
    Format.eprintf "Exception whilst emitting instruction:@ %a\n"
      Printlinear.instr i;
    raise exn
  )

let rec emit_all ~first ~fallthrough i =
  match i.desc with
  | Lend -> ()
  | _ ->
      (try emit_instr ~first ~fallthrough i
       with exn -> (
         Format.eprintf "Exception whilst emitting instruction:@ %a\n"
           Printlinear.instr i;
         raise exn));
      emit_all ~first:false ~fallthrough:(Linear.has_fallthrough i.desc) i.next

let all_functions = ref []

let emit_function_type_and_size fun_name =
  match system with
  | S_gnu | S_linux ->
    D.type_ (emit_symbol fun_name) "@function";
    if not !Flambda_backend_flags.basic_block_sections then
    D.size (emit_symbol fun_name)
      (ConstSub (
         ConstThis,
         ConstLabel (emit_symbol fun_name)))
  | _ -> ()

(* Emission of a function declaration *)

let fundecl fundecl =
  let fun_end_label, fundecl =
    match Emitaux.Dwarf_helpers.record_dwarf_for_fundecl fundecl with
    | None -> None, fundecl
    | Some { fun_end_label; fundecl } -> Some fun_end_label, fundecl
  in
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := fundecl.fun_tailrec_entry_point_label;
  contains_calls := fundecl.fun_contains_calls;
  stack_offset := 0;
  call_gc_sites := [];
  local_realloc_sites := [];
  clear_safety_checks ();
  clear_stack_realloc ();
  for i = 0 to Proc.num_stack_slot_classes - 1 do
    num_stack_slots.(i) <- fundecl.fun_num_stack_slots.(i);
  done;
  prologue_required := fundecl.fun_prologue_required;
  frame_required := fundecl.fun_frame_required;
  all_functions := fundecl :: !all_functions;
  current_basic_block_section :=
    Option.value fundecl.fun_section_name ~default:"";
  emit_function_or_basic_block_section_name ();
  D.align ~data:false 16;
  add_def_symbol fundecl.fun_name;
  if system = S_macosx
  && not !Clflags.output_c_object
  && is_generic_function fundecl.fun_name
  then (* PR#4690 *)
    D.private_extern (emit_symbol fundecl.fun_name)
  else
    global_maybe_protected (emit_symbol fundecl.fun_name);
  (* Even if the function name is Local, still emit an
     actual linker symbol for it. This provides symbols
     for perf, gdb, and similar tools *)
  D.label (emit_symbol fundecl.fun_name);
  D.label (label_name (emit_symbol fundecl.fun_name));
  emit_debug_info fundecl.fun_dbg;
  cfi_startproc ();
  if Config.runtime5 && (not Config.no_stack_checks) && !Clflags.runtime_variant = "d" then begin
    emit_call (Cmm.global_symbol "caml_assert_stack_invariants");
  end;
  emit_all ~first:true ~fallthrough:true fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  List.iter emit_local_realloc !local_realloc_sites;
  emit_call_safety_errors ();
  emit_stack_realloc ();
  if !frame_required then begin
    let n = frame_size() - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      cfi_adjust_cfa_offset (-n);
    end;
  end;
  Option.iter def_label fun_end_label;
  cfi_endproc ();
  emit_function_type_and_size fundecl.fun_name

(* Emission of data *)

let emit_item = function
  | Cdefine_symbol s ->
    begin match s.sym_global with
    | Local ->
      _label (label_name (emit_symbol s.sym_name))
    | Global ->
      global_maybe_protected (emit_symbol s.sym_name);
      add_def_symbol s.sym_name;
      _label (emit_symbol s.sym_name);
      _label (label_name (emit_symbol s.sym_name))
    end
  | Cint8 n -> D.byte (const n)
  | Cint16 n -> D.word (const n)
  | Cint32 n -> D.long (const_nat n)
  | Cint n -> D.qword (const_nat n)
  | Csingle f -> D.long  (Const (Int64.of_int32 (Int32.bits_of_float f)))
  | Cdouble f -> D.qword (Const (Int64.bits_of_float f))
  (* SIMD vectors respect little-endian byte order *)
  | Cvec128 {high; low} -> D.qword (Const low); D.qword (Const high)
  | Csymbol_address s ->
    add_used_symbol s.sym_name;
    D.qword (ConstLabel (emit_cmm_symbol s))
  | Csymbol_offset (s, o) ->
    add_used_symbol s.sym_name;
    D.qword (ConstLabelOffset (emit_cmm_symbol s, o))
  | Cstring s -> D.bytes s
  | Cskip n -> if n > 0 then D.space n
  | Calign n -> D.align ~data:true n

let data l =
  D.data ();
  D.align ~data:true 8;
  List.iter emit_item l

(* Beginning / end of an assembly file *)

let reset_all () =
  X86_proc.reset_asm_code ();
  Emitaux.reset ();
  reset_debug_info();                   (* PR#5603 *)
  reset_imp_table();
  reset_probes ();
  stapsdt_base_emitted := false;
  reset_traps ();
  float_constants := [];
  all_functions := []

let begin_assembly unix =
  reset_all ();

  if !Flambda_backend_flags.internal_assembler &&
     !Emitaux.binary_backend_available then
    X86_proc.register_internal_assembler (Internal_assembler.assemble unix);

  let code_begin = Cmm_helpers.make_symbol "code_begin" in
  let code_end = Cmm_helpers.make_symbol "code_end" in
  Emitaux.Dwarf_helpers.begin_dwarf ~build_asm_directives ~code_begin ~code_end
    ~file_emitter:D.file;

  if system = S_win64 then begin
    D.extrn "caml_call_gc" NEAR;
    D.extrn "caml_c_call" NEAR;
    D.extrn "caml_allocN" NEAR;
    D.extrn "caml_alloc1" NEAR;
    D.extrn "caml_alloc2" NEAR;
    D.extrn "caml_alloc3" NEAR;
    D.extrn "caml_ml_array_bound_error" NEAR;
    D.extrn "caml_raise_exn" NEAR;
  end;

  if !Clflags.dlcode || Arch.win64 then begin
    (* from amd64.S; could emit these constants on demand *)
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal16"] None ["16byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst16"] (Some "aM") ["@progbits";"16"]
    end;
    D.align ~data:true 16;
    _label (emit_symbol "caml_negf_mask");
    D.qword (Const 0x8000000000000000L);
    D.qword (Const 0L);
    D.align ~data:true 16;
    _label (emit_symbol "caml_absf_mask");
    D.qword (Const 0x7FFFFFFFFFFFFFFFL);
    D.qword (Const 0xFFFFFFFFFFFFFFFFL);
    _label (emit_symbol "caml_negf32_mask");
    D.qword (Const 0x80000000L);
    D.qword (Const 0L);
    D.align ~data:true 16;
    _label (emit_symbol "caml_absf32_mask");
    D.qword (Const 0xFFFFFFFF7FFFFFFFL);
    D.qword (Const 0xFFFFFFFFFFFFFFFFL);
  end;

  D.data ();
  emit_global_label "data_begin";

  emit_named_text_section code_begin;
  emit_global_label_for_symbol code_begin;
  if system = S_macosx then I.nop (); (* PR#4690 *)

  D.label (emit_cmm_symbol call_gc_local_sym);
  cfi_startproc ();
  I.jmp (rel_plt (Cmm.global_symbol "caml_call_gc"));
  cfi_endproc ();

  ()

let make_stack_loc ~offset n (r : Reg.t) =
  (* Use "Outgoing" stack locations, instead of "Local",
     because [slot_offset] emits (Outgoing n) directly
     as offset [n] from the stack pointer,
     rather than a calculation relative to the stack frame,
     which is incorrect for naked floats (arising from live variables, not
     probe arguments) in the wrapper's frame. *)
  let loc = Stack (Outgoing (offset + n)) in
  (* Manufacture stack entry with this register's type *)
  Reg.at_location r.typ loc

(* CR mshinwell: Not now, but after code review, it would be better to
   move this code so it's contiguous with the other probe code above. *)

(* Calls to probe handler wrappers do not follow the standard calling
   convention for OCaml.  Instead, all registers are callee saved: the wrapper
   saves and restores all registers that are live across its (unique) call site.

   There is one wrapper per probe site in the code.  Probe wrappers can never
   be called from anywhere except their unique probe site, since they make
   assumptions about the layout of their caller's stack frame.  Probe wrappers
   create their own stack frames; these will be elided in OCaml backtraces
   but may be displayed in platform debuggers such as gdb.

   The current strategy for argument passing to a probe handler is to spill
   all arguments of the probe, plus all live registers, to stack and then
   reload the arguments into locations expected by the handler.
   This is gratuitously inefficient but easier to reason about correctness.
   This problem is a variation of the "sequentialization of a parallel
   assignment" problem, which admits an optimal and efficient solution.

   The stack layout of the wrapper, before the call to the handler, is as
   follows:
   |     ...    |
   |------------|<-- %rsp at probe site, 16-byte aligned by caller
   | ret        |
   |------------|
   | fp         | only present when Config.with_frame_pointers is true
   |------------|
   | live       | saved live registers
   |------------|
   | r15        | spill to use as a temporary for stack to stack move [2]
   |------------|
   | tmp        | spilled reg and stack arguments of the wrapper
   |------------|<-- 16-byte aligned [1]
   | stack args | arguments passed to handler on the stack (size in loc_offset)
   |------------|<-- %rsp at the call from wrapper to handler, 16-byte aligned

   [1] To ensure that the stack before the call from the wrapper to the handler
   is 16-byte aligned, we align this explicitly. It is a little wasteful,
   but reuses the calculation made by [Proc.loc_arguments].

   [2] Stack to stack moves are only used for probe arguments, which must be
   valid OCaml values and not naked floats, because probes are treated like
   calls.
   Therefore, for now, we do not need a temporary register of type float.
   This assumption might no longer hold in the presence of unboxed types.
*)

let size_of_regs regs =
  Array.fold_right
    (fun r acc ->
      match r.Reg.typ with
      | Int | Addr | Val -> acc + size_int
      | Float | Float32 ->
        (* Float32 slots still take up a full word *)
        acc + size_float
      | Vec128 | Valx2 -> acc + size_vec128)
    regs 0

let stack_locations ~offset regs =
  let _, locs = Array.fold_right (fun r (n, offsets) ->
    let next = n + match r.Reg.typ with
      | Int | Val | Addr -> size_int
      | Float | Float32 ->
        (* Float32 slots still take up a full word *)
        size_float
      | Vec128 | Valx2 -> size_vec128 in
    next, (make_stack_loc n r ~offset :: offsets)) regs (0, []) in
  locs |> Array.of_list

let emit_probe_handler_wrapper p =
  let wrap_label = probe_handler_wrapper_name p.probe_label in
  let probe_name, handler_code_sym =
    match p.probe_insn.desc with
    | Lcall_op (Lprobe { name; handler_code_sym; enabled_at_init = _; }) ->
       name, handler_code_sym
    | _ -> assert false
  in
  (* Restore stack frame info as it was at the probe site, so we can easily
     refer to slots in the corresponding frame.  (As per the comment above,
     recall that the wrapper does however have its own frame.) *)
  frame_required := true;
  stack_offset := p.stack_offset;
  for i = 0 to Proc.num_stack_slot_classes - 1 do
    num_stack_slots.(i) <- p.num_stack_slots.(i);
  done;
  (* Account for the return address that is now pushed on the stack. *)
  stack_offset := !stack_offset + 8;
  (* Emit function entry code *)
  D.comment (Printf.sprintf "probe %s %s" probe_name handler_code_sym);
  emit_named_text_section wrap_label;
  D.align ~data:false 16;
  _label wrap_label;
  cfi_startproc ();
  if fp then begin
    push rbp;
    I.mov rsp rbp;
  end;
  (* Prepare to call the handler: calculate and allocate stack space.
     Compute the size of stack slots for all live hard registers. *)
  let live =
    Reg.Set.elements p.probe_insn.live
    |> List.filter Reg.is_reg
    |> Array.of_list
  in
  let live_offset = size_of_regs live in
  (* Compute the size of stack slots for spilling all arguments of the probe. *)
  let aux_offset = 8 (* for saving r15 *) in
  let tmp_offset = size_of_regs p.probe_insn.arg in
  let loc_args, loc_offset = Proc.loc_arguments (Reg.typv p.probe_insn.arg) in
  (* Ensure the stack is aligned.
     Assuming that the stack at the probe site is 16-byte aligned,
     [loc_arguments] ensures that [loc_offset] is 16-byte aligned.
     All temporaries are 8 bytes long, the return address is already pushed,
     and optionally the frame pointer is already pushed on the stack. *)
  let wrapper_frame_size k = 8 + (if fp then 8 else 0) + k in
  let k = live_offset + aux_offset + tmp_offset + loc_offset in
  assert (k mod 8 = 0);
  let padding = if ((wrapper_frame_size k) mod 16) = 0 then 0 else 8 in
  let n = k + padding in
  (* Allocate stack space *)
  if Config.runtime5 && (not Config.no_stack_checks) && (n >= Stack_check.stack_threshold_size) then
    emit_stack_check ~size_in_bytes:n ~save_registers:true;
  emit_stack_offset n;
  (* Save all live hard registers *)
  let offset = aux_offset + tmp_offset + loc_offset in
  let saved_live = stack_locations ~offset live in
  Array.iteri (fun i reg -> move reg saved_live.(i)) live;
  (* Spill r15 to free it to be used as a temporary register for stack to
     stack copying. *)
  let offset = tmp_offset + loc_offset in
  let saved_r15 = make_stack_loc 0 (Reg.dummy) ~offset in
  I.mov r15 (reg saved_r15);
  (* Spill all arguments of the probe.  Some of these may already be on the
     stack, in which case a temporary is used for the move. *)
  let saved_args = stack_locations ~offset:loc_offset p.probe_insn.arg in
  Array.iteri (fun i reg -> move_allowing_stack_to_stack reg (saved_args.(i)))
    p.probe_insn.arg;
  (* Load probe arguments to correct locations for the handler *)
  Array.iteri (fun i reg -> move_allowing_stack_to_stack saved_args.(i) reg) loc_args;
  (* Reload spilled registers used as temporaries *)
  I.mov (reg saved_r15) r15;
  (* Emit call to handler *)
  add_used_symbol handler_code_sym;
  emit_call (Cmm.global_symbol handler_code_sym);
  (* Record a frame description for the wrapper *)
  let label = new_label () in
  let live_offset =
    Array.fold_right (fun (r:Reg.t) acc ->
      match r.loc with
      | Stack (Outgoing k) ->
        (match r.typ with
        | Val -> k::acc
        | Int | Float | Vec128 | Float32 -> acc
        | Valx2 -> k::k+Arch.size_addr::acc
        | Addr -> Misc.fatal_error ("bad GC root " ^ Reg.name r))
      | _ -> assert false)
    saved_live
    []
  in
  record_frame_descr ~label ~frame_size:(wrapper_frame_size n) ~live_offset
    (Dbg_other Debuginfo.none);
  def_label label;
  (* After the probe handler has finished executing, restore all live registers
     and free stack space. *)
  Array.iteri (fun i reg -> move saved_live.(i) reg) live;
  emit_stack_offset (-n);
  if fp then begin
    pop rbp
  end;
  I.ret ();
  cfi_endproc ();
  emit_function_type_and_size wrap_label

let emit_stapsdt_base_section () =
  if not !stapsdt_base_emitted then begin
    stapsdt_base_emitted := true;
    D.section [".stapsdt.base"] (Some "aG")
      ["\"progbits\""; ".stapsdt.base"; "comdat"];
    D.weak "_.stapsdt.base";
    D.hidden "_.stapsdt.base";
    D.label "_.stapsdt.base";
    D.space 1;
    D.size "_.stapsdt.base" (const 1)
  end

let emit_elf_note ~owner ~typ ~emit_desc =
  D.align ~data:true 4;
  let a = new_label() in
  let b = new_label() in
  let c = new_label() in
  let d = new_label() in
  D.long (ConstSub (ConstLabel (emit_label b),
                    ConstLabel(emit_label a)));
  D.long (ConstSub (ConstLabel (emit_label d),
                    ConstLabel (emit_label c)));
  D.long (const_32 typ);
  def_label a;
  D.bytes (owner^"\000");
  def_label b;
  D.align ~data:true 4;
  def_label c;
  emit_desc ();
  def_label d;
  D.align ~data:true 4

let emit_probe_notes0 () =
  let is_macosx_system =
    match system with
    | S_macosx -> (* CR-someday gyorsh: emit dtrace format on mac *) true
    | S_gnu | S_solaris | S_linux_elf | S_bsd_elf
    | S_beos | S_linux  -> false
    | S_cygwin | S_mingw | S_mingw64 | S_win64 | S_win32 | S_unknown
    | S_freebsd| S_netbsd | S_openbsd ->
      Misc.fatal_error "emit_probe_notes: unexpected system"
  in
  (match is_macosx_system with
   | false -> D.section [".note.stapsdt"] (Some "?") [ "\"note\"" ]
   | true -> D.section ["__DATA";"__note_stapsdt"] None ["regular"]);
  let stap_arg arg =
    let arg_name =
      match arg.loc with
      | Stack s ->
        Printf.sprintf "%d(%%rsp)" (slot_offset s (stack_slot_class arg.Reg.typ))
      | Reg reg -> Proc.register_name arg.Reg.typ reg
      | Unknown ->
        Misc.fatal_errorf "Cannot create probe: illegal argument: %a"
          Printreg.reg arg
    in
    Printf.sprintf "%d@%s" (Select_utils.size_component arg.Reg.typ) arg_name
  in
  let describe_one_probe p =
    let probe_name, enabled_at_init =
      match p.probe_insn.desc with
      | Lcall_op (Lprobe { name; enabled_at_init; handler_code_sym=_; }) ->
         name, enabled_at_init
      | _ -> assert false
    in
    let args =
      Array.fold_right (fun arg acc -> (stap_arg arg)::acc)
        p.probe_insn.arg
        []
      |> String.concat " "
    in
    let semsym =
      find_or_add_semaphore probe_name (Some enabled_at_init) p.probe_insn.dbg
    in
    let semaphore_label = emit_symbol semsym in
    let emit_desc () =
      D.qword (ConstLabel (emit_label p.probe_label));
      (match is_macosx_system with
       | false -> D.qword (ConstLabel "_.stapsdt.base")
       | true -> D.qword (const 0));
      D.qword (ConstLabel semaphore_label);
      D.bytes "ocaml_1\000";
      D.bytes (probe_name ^ "\000");
      D.bytes (args ^ "\000")
    in
    emit_elf_note ~owner:"stapsdt" ~typ:3l ~emit_desc;
  in
  List.iter describe_one_probe !probes;
  (match is_macosx_system with
   | false ->
     emit_stapsdt_base_section ();
     D.section [".probes"] (Some "wa") ["\"progbits\""]
   | true ->
     D.section ["__TEXT";"__probes"] None ["regular"]);
  D.align ~data:true 2;
  String.Map.iter (fun _ (label, enabled_at_init) ->
      (* Unresolved weak symbols have a zero value regardless
         of the following initialization. *)
      let enabled_at_init = Option.value enabled_at_init ~default:false in
      D.weak label;
      D.hidden label;
      _label label;
      D.word (const 0); (* for systemtap probes *)
      D.word (const (Bool.to_int enabled_at_init)); (* for ocaml probes *)
      add_def_symbol label)
    !probe_semaphores

let emit_probe_notes () =
  match !probes with
  | [] -> ()
  | _ -> emit_probe_notes0 ()

let emit_trap_notes () =
  (* Don't emit trap notes on windows and macos systems *)
  let is_system_supported =
    match system with
    | S_macosx -> false  (* can be supported with a symbol *)
    | S_gnu | S_solaris | S_linux_elf | S_bsd_elf
    | S_beos | S_linux -> true
    | S_cygwin | S_mingw | S_mingw64 | S_win64 | S_win32 | S_unknown ->
      false
    | S_freebsd| S_netbsd | S_openbsd ->
      (* Probably works, as these are ELF-based, but untested. *)
      false
  in
  let emit_labels list =
    List.iter (fun l -> D.qword (ConstLabel (emit_label l))) list;
    D.qword (const 0)
  in
  let emit_desc () =
    D.qword (ConstLabel "_.stapsdt.base");
    emit_labels (Label.Set.elements traps.enter_traps);
    emit_labels traps.push_traps;
    emit_labels traps.pop_traps
  in
  if is_system_supported && !Arch.trap_notes &&
     not (Label.Set.is_empty traps.enter_traps) then begin
    D.section [".note.ocaml_eh"] (Some "?") [ "\"note\"" ];
    emit_elf_note ~owner:"OCaml" ~typ:1l ~emit_desc;
    (* Reuse stapsdt base section for calcluating addresses after pre-link *)
    emit_stapsdt_base_section ();
    (* Switch back to Data section *)
    D.data ()
  end

let end_assembly () =
  if !float_constants <> [] then begin
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal8"] None ["8byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst8"] (Some "aM") ["@progbits";"8"]
    end;
    D.align ~data:true 8;
    List.iter (fun (cst,lbl) -> emit_float_constant cst lbl) !float_constants;
  end;
  if !vec128_constants <> [] then begin
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal16"] None ["16byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst16"] (Some "aM") ["@progbits";"16"]
    end;
    D.align ~data:true 16;
    List.iter (fun (cst,lbl) -> emit_vec128_constant cst lbl) !vec128_constants;
  end;

  (* Emit probe handler wrappers *)
  List.iter emit_probe_handler_wrapper !probes;

  emit_named_text_section (Cmm_helpers.make_symbol "jump_tables");
  emit_jump_tables ();

  let code_end = Cmm_helpers.make_symbol "code_end" in
  emit_named_text_section code_end;
  if system = S_macosx then I.nop ();
  (* suppress "ld warning: atom sorting error" *)

  emit_global_label_for_symbol code_end;

  emit_imp_table();

  D.data ();
  D.qword (const 0);  (* PR#6329 *)
  emit_global_label "data_end";
  D.qword (const 0);

  D.text ();
  D.align ~data:true 8;                            (* PR#7591 *)
  emit_global_label "frametable";

  let setcnt = ref 0 in
  emit_frames
    { efa_code_label = (fun l -> D.qword (ConstLabel (emit_label l)));
      efa_data_label = (fun l -> D.qword (ConstLabel (emit_label l)));
      efa_8 = (fun n -> D.byte (const n));
      efa_16 = (fun n -> D.word (const n));
      efa_32 = (fun n -> D.long (const_32 n));
      efa_word = (fun n -> D.qword (const n));
      efa_align = D.align ~data:true;
      efa_label_rel =
        (fun lbl ofs ->
           let c =
             ConstAdd (
               ConstSub(ConstLabel(emit_label lbl), ConstThis),
               const_32 ofs
             ) in
           if system = S_macosx then begin
             incr setcnt;
             let s = Printf.sprintf "L$set$%d" !setcnt in
             D.setvar (s, c);
             D.long (ConstLabel s)
           end else
             D.long c
        );
      efa_def_label = (fun l -> _label (emit_label l));
      efa_string = (fun s -> D.bytes (s ^ "\000"))
    };

  if system = S_linux || system = S_freebsd || system = S_netbsd || system = S_openbsd then begin
    let frametable = emit_symbol (Cmm_helpers.make_symbol "frametable") in
    D.size frametable (ConstSub (ConstThis, ConstLabel frametable))
  end;

  D.data ();
  emit_probe_notes ();
  emit_trap_notes ();

  if system = S_linux then
    (* Mark stack as non-executable, PR#4564 *)
    D.section [".note.GNU-stack"] (Some "") [ "%progbits" ];

  if system = S_win64 then begin
    D.comment "External functions";
    String.Set.iter
      (fun s ->
         if not (String.Set.mem s !symbols_defined) then
           D.extrn (emit_symbol s) NEAR)
      !symbols_used;
    symbols_used := String.Set.empty;
    symbols_defined := String.Set.empty;
  end;

  let asm =
    if !X86_proc.create_asm_file then
      Some
        (
         (if X86_proc.masm then X86_masm.generate_asm
          else X86_gas.generate_asm) !Emitaux.output_channel
        )
    else
      None
  in
  if not !Flambda_backend_flags.internal_assembler then
    Emitaux.Dwarf_helpers.emit_dwarf ();
  X86_proc.generate_code asm;
  (* The internal assembler does not work if reset_all is called here *)
  if not !Flambda_backend_flags.internal_assembler then
    reset_all ()

