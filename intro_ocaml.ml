let add x y = x + y (* or let add = ( + ) *)

let simple_main () =
  let x = read_int () in
  let y = read_int () in
  print_int (add x y);
  print_newline ()

let add1 = add 1 ;;

add1 2

(** [foldlf v l] is f ... (f (f v a0) a1) ... an)
 ** assuming [l] is [a0; a1; ...; an].
 ** [foldl] is sometimes called reduce ,*)
let rec foldl f acc = function
  | [] -> acc
  | x :: xs -> foldl f (f acc x) xs ;;

foldl (+) 0 [1;2;3;4]  ;;

(** [( |-> ) n m] is an infix operator computing
 ** the list of elements from [n] included to [m] included. *)
  let ( |-> ) lo hi  =
    let rec loop acc n =
      if n > hi then List.rev acc
      else loop (n :: acc) (n + 1) in
    loop [] lo
  ;;

1 |-> 10 ;;

(* This second declaration hides the first.*)
let ( |-> ) lo hi =
  assert (hi >= lo);
  lo |-> hi

(** [max cmp l] computes the maximun element of a list [l] provided a [cmp]
 ** function conforming to the following specification:
 ** - cmp x y = 0 if x is equivalent to y
 ** - cmp x y > 0 if x is bigger than y
 ** - cmp x y < 0 if x if smaller than y **)
let max cmp l =
  let rec loop vmax = function
    | [] -> vmax
    | x :: xs ->
       let vmax' =
         match vmax with
         | None -> Some x
         | Some y -> if cmp x y > 0 then Some x else vmax in
       loop vmax' xs
  in loop None l
;;

max Stdlib.compare [1; 2; 3;] ;;

(* We just hid [Stdlib.max] behind another definition !*)
Stdlib.max ;;

let double x = print_int x; 2 * x ;;

(* We forgot to use x ... *)
let dadd _x y =
  let x' = double y
  and y' = double y in
  (* Infix operators are prefixed ones that are treated specially
     by the parser. Have fun and create your owns. *)
  ( + ) x' y' ;;

dadd (double 1) (double 2) ;;

add (print_string "foo!"; 1) (print_string "bar!"; 2) ;;

( || ) (print_string "foo!"; false) (print_string "bar!"; true) ;;

let a = 1, 2 in
let x, y = a in
x + y
;;

let a = (1, 2) in
let (x, y) = a in
x + y
;;

let create x y z = x, y, z

(* FP arithmetic operations have a dedicated syntax *)
let square a = a *. a

let dist (x1, y1, z1) p =
  let x2, y2, z2 = p in
  let xdiff = x2 -. x1
  and ydiff = y2 -. y1
  and zdiff = z2 -. z1 in
  square xdiff +. square ydiff +. square zdiff |> sqrt

let dist p1 p2 =
  match p1, p2 with
  (* The | can also be used as a separator instead of as a starting
     annotation. *)
  | (x1, y1, z1), (x2, y2, z2) ->
     let xdiff = x2 -. x1
     and ydiff = y2 -. y1
     and zdiff = z2 -. z1 in
     sqrt @@ square xdiff +. square ydiff +. square zdiff

type point_2d = { x : float; y: float; }  ;;

(* C-like . notations for field access *)
let dist p1 p2 =
  let xdiff = p1.x -. p2.x
  and ydiff = p1.y -. p2.y in
  sqrt (xdiff *. xdiff +. ydiff *. ydiff)

(* Using pattern-matching *)
let dist p1 p2 =
  match p1, p2 with
  | { x; y; }, { x = x'; y = y';} ->
     let xdiff = x -. x'
     and ydiff = y -. y' in
     sqrt (xdiff *. xdiff +. ydiff *. ydiff)

(* Record can be built/destructed using a shortcut notation.
   [let create x y = { x; y; }] is a shortcut for
   [let create x y = { x = x; y = y; }].

   Choose your field names wisely and unleash your inner procrastinator !
 *)
let create x y = { x; y; }

let of_int myx myy = { x = float myx; y = float myy; }

type prop = (* inductively defined types do not need a rec keyword *)
  | Pcst of bool
  | Pvar of string
  | Pand of prop * prop
  | Por of prop * prop
  | Pnot of prop

let free_variables =
  (* The pattern matching in [loop] is well-typed but not exhaustive *)
  let rec loop vars = function
    | Pvar s -> if List.mem s vars then vars else s :: vars
    | Pand (p1, p2) ->
       let vars' = loop vars p1 in
       loop vars' p2
  in loop []

let free_variables =
  (* Now it is exhaustive, but ... fragile *)
  let rec loop vars = function
    | Pvar s -> if List.mem s vars then vars else s :: vars
    | Pand (p1, p2) ->
       let vars' = loop vars p1 in loop vars' p2
    | Por (p1, p2) ->
       let vars' = loop vars p1 in loop vars' p2
    | Pnot p -> loop vars p
    (* fragile pattern-matching below.
     * if a constructor is added, it is matched *)
    | _ -> vars
  in loop []

let free_variables =
  let rec loop vars = function
    | Pvar s -> if List.mem s vars then vars else s :: vars
    | Pand (p1, p2)
    | Por (p1, p2) -> (* 'or' pattern *)
       let vars' = loop vars p1 in loop vars' p2
    | Pnot p -> loop vars p
    | Pcst _ -> vars (* non-fragile pattern-matching *)
    (* When later adding [Bxor] constructor, the
     * compiler will show me where pattern-matching
     * is not exhaustive. *)
  in loop []

type 'a interval = { lo : 'a; hi : 'a } ;;
let create ~lo ~hi = { lo; hi; } ;;

(* Which version would you rather write? *)
let lo = 1 and hi = 2 in create ~lo ~hi  ;;

let lbd = 12 and ubd = 15 in create ~lo:lbd ~hi:ubd ;;

(* Reusing type interval *)
let create ?(lo=0) hi = { lo; hi; } ;;

create 2 ;;

let create ?(lo=0) ~hi () = { lo; hi; } ;;

let ival = create ~hi:2 ();;

(* The use of partial arguments complicate partial applications.*)
let pp_ival ?(pre="(") ?(post=")") ?(sep=",") ppf { lo; hi; } =
  Format.fprintf ppf "@[<h>%s%d%s%d%s@]" pre lo sep hi post ;;

(* You need to create another function *)
Format.printf "%a@." (fun ppf ival -> pp_ival ppf ival) ival ;;

(* The following does work though *)
let pp_ival2 ppf = pp_ival ppf ;;
Format.printf "%a@." pp_ival2 ival ;;

type ('a, 'b) return = {
    value : 'a;
    explanation : 'b option;
  }

(* Optional arguments of a type ['a] are really ['a option] types and ce be
 * used that way in the body of the function *)
let create_return_value ?explanation value =
  { value; explanation; }

(* Now if you have a default value [v], [Some v] needs to be used. *)
let create_defaulted_return_value ?(explanation="message") value =
  { value; explanation = Some explanation; }

type fi_pair = { x : float; y : int; }

(* Shadowing x and y *)
type if_pair = { x : int; y : float; }

let addall (v1:fi_pair) v2 =
  let xsum = truncate v1.x + v2.x in
  let ysum = v1.y + truncate v2.y in
  xsum + ysum

let fact n =
  let res = ref 1 in
  for j = 2 to n do
    res := !res * j; (* this assignment has type unit *)
  done; (* The for loop too ! *)
  !res

let x = ref 1

let y : int Stdlib.ref = { contents = 12 }

type 'a ref = { mutable contents : 'a }

let _ = x := 13; y := 14 ;;

(!x, !y) ;;

type 'a set = 'a list 

let cardinal (l:'a set) =
  let h = Hashtbl.create 7 in
  let rec loop = function
    | x :: xs ->
       Hashtbl.add h x (); (* Hashtbl.replace may be better here *)
       loop xs
    | [] ->
       Hashtbl.length h
  in loop l

(* Concatenating the elements of a string list.
 * Clearly not thread safe. *)
let concat =
  (* An OCaml [Buffer.t] is similar to a Java [StringBuffer].
   * It is a self-growing array of bytes. *)
  let b = Buffer.create 1024 in
  fun ~sep l ->
  Buffer.reset b;    (* cleanup any previously written contents *)
  List.iter (fun s -> Buffer.add_string b s;
                      Buffer.add_string b sep;) l;
  Buffer.contents b

let h = Hashtbl.create 7 ;;
Hashtbl.add h 1 2 ;;
Hashtbl.add h 1 3 ;;
Hashtbl.iter (fun k v -> Format.printf "%d -> %d@." k v) h ;;

(* This is correct *)
let test_and_print =
  let count_success = ref 0 in
  fun secret ->
  if secret = "you will never guess that" then begin (* or ( *)
      incr count_success; Format.printf "Success"
    end (* or ) *)
  else Format.printf "Failure"

exception Empty_list 

let nth i l =
  assert (i >= 0);
  let rec aux j = function
    | [] ->
       raise Empty_list
    | x :: xs ->
       if j = 0 then x
       else aux (j - 1) xs
  in aux i l

(** [find p a] returns 
 **  - [None] if no element of [a] verifies predicate [p]
 **  - [Some e] otherwise where [e] is the first element of [a] s.t.
 **    [p e = true ]
 **)
let find (type a) (p:a -> bool) (arr:a array) =
  let exception Found of a in
  match Array.iter (fun e -> if p e then raise (Found e)) arr with
  | () -> None
  | exception (Found elt) -> Some elt

module Interval_concrete = struct
  type t =
  | Ival of { lo : int; hi : int; } (* here's an inline record *)
  | Top

  let top = Top 

  let ival ~lo ~hi =
    assert (lo <= hi);
    if lo = min_int && hi = max_int then top
    else Ival {lo; hi;}
end

open Interval_concrete

(* Pattern-matching is ok *)
let size = function
  | Ival {lo; hi; } -> float @@ hi - lo + 1
  | Top -> infinity

(* This is authorized. *)
let interval = Top

(* This is too
 * the [top] function is part of the module signature *)
let top2 = top

module type IPRIVATE = sig
  type t = private
          | Ival of { lo : int; hi: int;}
          | Top

  val ival : lo:int -> hi:int -> t
  val top : t
end

module Interval_private : IPRIVATE = Interval_concrete

open Interval_private

(* Pattern-matching is ok on private types *)
let size = function
  | Ival {lo; hi; } -> float @@ hi - lo + 1
  | Top -> infinity

(* This is ok * the [top] function is part of
 * the [IPRIVATE] module signature *)
let top2 = top

module type IABSTRACT = sig
  type t (* opaque to the outside world *)
  val ival : lo:int -> hi:int -> t ;;
  val top : t

  (** Accessors needs to be in the interface now *)
  val is_top : t -> bool

  (** Fails if value is [!top] *)
  val lo : t -> int
  val hi : t -> int
end

module Interval_abstract : IABSTRACT = struct
  include Interval_concrete 

  let is_top = function Top -> true
  | Ival _ -> false

   let lo = function
   | Ival i -> i.lo
   | Top -> assert false

   let hi = function
   | Ival i -> i.hi
   | Top -> assert false
end

open Interval_abstract

(* Pattern-matching does not work anymore *)
let size ival =
  if is_top ival then infinity
  else float @@ hi ival - lo ival + 1

(* This is ok for the [top] function is part
   of the module signature *)
let top2 = top

let rec length = function 
  | [] -> 0
  | _ :: l' -> 1 + length l'
;;

module type PRINTABLE = sig
  type t
  val pp: Format.formatter -> t -> unit
end

module List_printer(X:PRINTABLE) = struct
  let pp_list
        ?(pre=(fun ppf () -> Format.pp_print_string ppf "["))
        ?(post=(fun ppf () -> Format.pp_print_string ppf "]"))
        ?(sep=(fun ppf () -> Format.fprintf ppf ";@ "))
        ppf l =
    let open Format in
    let rec loop = function
      | [] -> post ppf ()
      | e :: es ->
         begin
           X.pp ppf e;
           sep ppf ();
           loop es
         end
    in pre ppf (); loop l
end

module Int_list_pp =
  List_printer(struct type t = int let pp = Format.pp_print_int end)

let pp_ilist ppf l = Int_list_pp.pp_list ppf l ;;
pp_ilist Format.std_formatter [1;2;3]

module String_list_pp =
  List_printer(
      struct 
        type t = string 
        let pp = Format.pp_print_string 
      end)

let pp_slist = fun ppf l -> String_list_pp.pp_list ppf l;;
Format.printf "@[<h>%a@]" pp_slist  ["foo"; "bar"; "bar";] ;;

module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

let lmax (type a) (module M:COMPARABLE with type t = a) (l:a list) =
  let rec aux vmax l =
    match l with
    | [] -> vmax
    | x :: xs ->
       let vmax' =
         match vmax with
         | None -> Some x
         | Some v -> if M.compare x v > 0 then Some x else vmax in
       aux vmax' xs
  in aux None l

module Int = struct type t = int let compare = Stdlib.compare end ;;

lmax (module Int) [1;2;3;] ;;

(* Module [String] is part of the standard library *)
lmax (module String) ["foo"; "bar"; "baz";] ;;

type ('var,'cst,'bop,'uop) expr =
  | Var of 'var
  | Cst of 'cst
  | Bop of 'bop *
             ('var,'cst,'bop,'uop) expr *
               ('var,'cst,'bop,'uop) expr
  | Uop of 'uop * ('var,'cst,'bop,'uop) expr

module type EXPR = sig
  type var
  type uop
  type cst
  type bop
end

module Bool = struct
  type bop =
    | Band
    | Bor
    | Bxor

  type uop = Bnot

  type var = string

  type cst = bool
end

let free_variables (type a b c d)
      (module M:EXPR with type var = a and type cst = b and
                          type bop = c and type uop = d)
      (e:(a,b,c,d) expr) : a list =
  let module S =
    Set.Make(struct type t = M.var let compare = Stdlib.compare end) in
  let rec loop (set:S.t) = function
    | Var v -> S.add v set
    | Cst _ -> set
    | Bop (_, e1, e2) -> S.union (loop set e1) (loop S.empty e2)
    | Uop (_, e) -> loop set e
  in
  let set = loop S.empty e in
  S.fold List.cons set []
;;

free_variables (module Bool) (Var "foo") ;;

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let (>>=) = Option.bind 

let hd = function
  | [] -> None
  | x :: _ -> Some x

let sum_heads l1 l2 =
  hd l1 >>=
    fun v1 -> hd l2 >>=
    fun v2 -> v1 + v2 |> Option.some

type _ bop =
  | Add : int bop
  | Mul : int bop
  | Div : int bop
  | Bor : bool bop
  | Band : bool bop

type _ uop =
  | UMin : int uop
  | Bnot : bool uop

type comparison = Eq | Gt

type _ typ =
  | Int  : int -> int typ
  | Bool : bool -> bool typ
  | Ite  : bool typ * 'a typ * 'a typ -> 'a typ
  | Bin  : 'a bop * 'a typ * 'a typ -> 'a typ
  | Un   : 'a uop * 'a typ -> 'a typ
  | Cmp  : comparison * 'a typ * 'a typ -> bool typ

let term = Ite (Cmp (Eq, Int 3, Int 4), Int 12, Int 11)

let term2 = 
  Ite (Cmp (Eq, Int 3, Un (UMin, Int 2)), 
       Bool true, Un (Bnot, Bool true)) ;;

let eval_bop: type a. a bop -> a -> a -> a = function
  | Add -> ( + ) 
  | Mul -> ( * ) 
  | Div -> ( / )
  | Bor -> ( || ) 
  | Band -> ( && )

let eval_cmp = function Eq -> ( = ) | Gt -> ( > ) ;;

let rec eval: type a. a typ -> a  = function
  | Int n -> n
  | Bool b -> b
  | Ite (b, csq, alt) -> if eval b then eval csq else eval alt
  | Bin (op, e1, e2) -> eval_bop op (eval e1) (eval e2)
  | Un (UMin, e) -> - (eval e)
  | Un (Bnot, e) -> not (eval e)
  | Cmp (op, e1, e2) -> (eval_cmp op) (eval e1) (eval e2)
  ;;

(* Ite (Cmp (Eq, Int 3, Int 4), Int 12, Int 11) *)
eval term  ;;

(* let term2 = 
 *   Ite (Cmp (Eq, Int 3, Un (UMin, Int 2)), 
 *        Bool true, Un (Bnot, Bool true)) ;; *)

eval term2 ;;

module E = struct
  type t =
    | Int of int
    | Add of t * t

  let rec pp_expr ppf = function
    | Int n -> Format.fprintf ppf "%02d" n
    | Add (e1, e2) ->
       Format.fprintf ppf "%a +@ %a" pp_expr e1 pp_expr e2
end

let () =
  let open E in
  List.fold_left (fun e n -> Add (Int n, e)) (Int 0) (1 |-> 20)
  |>  Format.printf "@[<hov>%a@]@." pp_expr

type const = [ `True | `False ]

(* See e.g., https://en.wikipedia.org/wiki/NAND_logic *)
let rec nandify = function
  | #const as b -> b
  | `Bnot b ->
     let b' = nandify b in `Bnand (b', b')
  | `Band (b1, b2) ->
     let b1 = nandify b1 and b2 = nandify b2 in
     `Bnand (`Bnand (b1, b2), `Bnand (b1, b2))
  | `Bnand (b1, b2) ->
     `Bnand(nandify b1, nandify b2)
  | `Bor (b1, b2) ->
     let b1 = nandify b1 and b2 = nandify b2 in
     `Bnand (`Bnand (b1, b1), `Bnand (b2, b2))
