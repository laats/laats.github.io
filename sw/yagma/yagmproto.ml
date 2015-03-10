(* -*-OCaml-*-
********************************************************************************
*
* File:         yagmproto.ml
* RCS:          $Header: $
* Description:  An Ocaml implementation of matching a list of trees in a graph
*               using color coding. By graphs we mean simple attributed graphs.
*               Input is provided in three files, the first containing the trees, 
*               each tree is given on a single line in a recursive
*               "(vertex index, [subtrees])"  format.
*               The next two files contain real valued matrices 
*               A and W in comma separated row major layout. The A matrix
*               is a matrix where entry i,j contains the similarity of 
*               vertex i in a graph G1 with vertex j in Graph G2. Graph G1
*               must be a superset of all the trees given. Graph G2 is the
*               target graph in which we seek to match the trees.  
*               The matrix W is the edge similarity matrix  
*               of G1 and G2. Edges for each graph are enumerated
*               as in the row major linear storage of the adjacency 
*               matrix of the graph. In any case, self loops, i.e., 
*               the diagonals are not counted. In the undirected case,
*               when the adjacancy matrix is symmetric, 
*               the upper triangular entries are also not counted.
*               The program within this file can be compiled as follows:
*   ocamlopt -thread -package batteries,batteries.pa_comprehension.syntax \
*            -syntax camlp4o -linkpkg yagmproto.ml -o yagmproto
*
* Author:       Staal Vinterbo
* Created:      Thu Aug  4 10:12:08 2011
* Modified:     Sun Oct 30 21:19:14 2011 (Staal Vinterbo) staal@mats
* Language:     caml
* Package:      N/A
* Status:       Experimental
*
* yagmproto.ml is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* yagmproto.ml is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with yagmproto.ml; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*
* (c) Copyright 2011, Staal Vinterbo, all rights reserved.
*
********************************************************************************
*)

open Batteries;;
open Genlex;;


(* ------------------ Utility definitions --------------------- *)

(* List.cartesian_product *)
let product xs ys =
  List.fold_left (fun acc x ->
    List.fold_left (fun acc y -> (x,y) :: acc) acc ys) [] xs;;

(* cumulative "sum" of elements in a list, returned as an array *)
let cumsum op zero list =
  Array.of_list (List.rev (snd (List.fold_left
				  (fun (x,l) y ->
				    let res = op x y in
				    (res, res::l)) (zero,[]) list)));;

let uncurry f (a,b) = f a b;;

(* debug print function *)
let debug s = prerr_string s;flush stderr;;

(* exponentiations in ocaml is only for floats *)
let intexp a b = int_of_float ((float_of_int a) ** (float_of_int b));;

(* List.combine *)
let zip a b = List.map2 (fun x y -> (x,y)) a b;;

(* IO ----------- *)

(* read an enumeration of lines from a file, '-' as filename means stdin *)
let readlines ?p:(p=(fun s -> (String.length s) > 0 && s.[0] != '#')) file =
  let input = match file with
    "-" -> stdin
  | _ -> open_in file in
  Enum.filter p (IO.lines_of input);;

(* read a comma separated matrix of floats from file with name 'file' *)
let readmatrix file =
  let s2a s = [? Array : (String.to_float (String.trim ss)) |
          ss <- List : (String.nsplit s ",") ?] in
  try
    [? Array : (s2a s) | s <- (readlines file) ?]
  with _ -> raise (Failure file);;


(* print a comma separated matrix *)
let fprint fmt stream x = IO.printf stream fmt x;;
let printmatrix ?stream:(stream=IO.stdout) fmt matrix =
  Array.iter (fun a ->
    Array.print ~first:"" ~sep:"," ~last:"\n" (fprint fmt) stream a) matrix;;



(* --------- Tree-related code ------------ *)

type tree = Node of (int * tree list);;

(* tree fold *)
let rec treefold f g z tree =
  let rec treefolds f g z subtrees =
    match subtrees with
      [] -> z
    | (x::xs) -> g (treefold f g z x) (treefolds f g z xs) in
  match tree with Node(label, subtrees) -> f label (treefolds f g z subtrees);;

let preorder = treefold List.cons (@) [];;

let edgef label ll =
  let f (i, pl) res = (label, i) :: (pl @ res) in
  (label, List.fold_right f ll []);;

let preedges tree = let (_, edges) = treefold edgef List.cons [] tree in edges;;

(* extract root of tree *)
let root tree = match tree with Node(r, _) -> r;;

(* # vertices in tree *)
let order = treefold (fun _ x -> x + 1) (+) 0;;

(* parse representation of tree (using Genlex and camplp4) *)
let lexer = make_lexer ["("; ")"; ","; "["; "]"];;
let rec parse_tree = parser
  | [< 'Kwd "("; 'Int label; 'Kwd ","; 'Kwd "["; nodes=parse_nodelist; 'Kwd "]" ;'Kwd ")" >] ->
      Node(label, nodes)
and parse_nodelist = parser
  | [< node = parse_tree ; rest = parse_tail >] -> node::rest
  | [< >] -> []
and parse_tail = parser
  | [< 'Kwd "," ; node = parse_tree; rest = parse_tail >] -> node::rest
  | [< >] -> [];;
let tree_from_string = parse_tree -| lexer -| Stream.of_string;;

(** color coding stack operations: lets us access next edge parent in constant time *)

type operations = PUSH | POP | POPPUSH | C | TOP;;

(* implementations of operations *)
let opfun x =
  match x with
    PUSH -> (fun l current -> (current::l, current))
  | POPPUSH -> (fun (x::xs) current -> (current::xs, current))
  | POP -> (fun (x::y::ys) _ -> (y::ys, y))
  | C -> (fun l current -> (l, current))
  | TOP -> (fun (x::xs) current -> (x::xs, x));;

(* fix rightmost subtree: repair (using splitwhile). *) 
(*   Translates first PUSH and TOP into POPPUSH and POP *)
let rec splitwhile p l =
  match l with
    [] -> ([], [])
  | (x::xs) ->
      if p x then let (left, right) = splitwhile p xs in (x::left, right)
      else ([], l);;
let repair oplist =
  match splitwhile (fun x -> x == C) oplist with
    (left, right) -> match right with
      [] -> oplist
    | (x::xs) -> match x with
	PUSH -> left @ (POPPUSH::xs)
      | TOP -> left @ (POP::xs)
      | _ -> oplist;;

(* treefold f function for generating stack operations *)
let opsf label ilist =
  match ilist with
    [] -> [TOP]
  | (l::[]) -> C::l
  | _ -> let (x::xs) = List.rev ilist in PUSH::(List.concat (List.rev ((repair x) :: xs)));;
let treeopsl = treefold opsf List.cons [];;
let treeops tree = List.map opfun (treeopsl tree);;

(* initial stack *)
let initstack v = [v;-1];;

(* preorder (operation,edge) list from tree *)
let ielist tree = let (x::xs) = treeops tree in zip xs (preedges tree);;


(* abstracted store access ********** *)
(* provide update, items, values and refresh operations on some map data store.
   here Hashtbl *)

let update h ((s, u),(q, l, ss)) =
  match Hashtbl.find_option h (s, u) with
    None -> begin Hashtbl.add h (s, u) (q, l, ss); h end
  | Some (q',l', s') -> begin if (q > q') then Hashtbl.replace h (s,u) (q,l,ss); h end;;

let items = Hashtbl.enum;; (* all key,value pairs *)
let values = Enum.map snd -| items;; (* all the values, without keys *)
let refresh = Hashtbl.create;; (* create a new store *)

(*  Color coding as a fold over tree edges in preorder ************* *)
(*  :  foldl oplus table edges *)

(* state threaded through calls (monads anyone?)  *)
type state = {
    times : int;
    gvertices : int list;
    root : int;
    n : int;
    v : (int -> int -> float);
    w : ((int*int) -> (int*int) -> float);
    new_colors : (unit -> (int array));
    mutable color : int array;
    keep : (int -> float -> float -> bool);
    make_assignment : ((int list) -> (int * int) list);
    tadj : int array array;
  } ;;


(* a candidate tree consists of ((endpoint, colorset), (score, vertices in reverse, endpoint stack) *)

(* List comprehension version, kept for understandability
let extend nextlen state bound edge ptree =
  let (op, (a, b)), ((u, colorset), (score, vlist, stack)) = edge, ptree in
  let l = [? (colorset lor state.color.(v), score +. (state.v b v) +. (state.w (a,b) (u,v)),
	      v::vlist, op stack v) | v <- Array: state.tadj.(u);
           (colorset land state.color.(v)) == 0 ?] in
  [? ((nextu, cs), (q, vl, nextstack)) | (cs, q, vl, (nextstack, nextu)) <- l;
   state.keep nextlen bound q ?];;
*)

(* slightly faster version, computing only what is needed *)
let extend nextlen state bound edge ptree =
  let (op, (a, b)), ((u, colorset), (score, vlist, stack)) = edge, ptree in
  let extrip v =
    if (colorset land state.color.(v)) == 0 then
      let q = score +. (state.v b v) +. (state.w (a,b) (u,v)) in
      if state.keep nextlen bound q then
	let (ns, nu) = op stack v in
	Some ((nu, colorset lor state.color.(v)),(q, v::vlist, ns))
      else None
    else None in
  Enum.filter_map extrip (Array.enum state.tadj.(u));;

(* fold f for single trial which is a fold over a list of preorder edges with stack instructions *)
let oplus bound (nextlen, state, table) edge =
  let op table ptree = Enum.fold update table (extend nextlen state bound edge ptree) in
  (nextlen + 1, state, Enum.fold op (refresh (Hashtbl.length table)) (items table));;

(* pick the max score (score, vertex list, stack) triplet *)
let emax e =
  Enum.fold (fun (q,l,s) (qq, ll, ss) -> if qq > q then (qq,ll,ss) else (q,l,s)) (0.,[], []) e;;

(* single trial: fold oplus over the (instruction,edge) list, return best match *)
let singletrial state bound ilist =
  let init = [? ((u, state.color.(u)), (state.v state.root u, [u], initstack u)) |
                u <- List: state.gvertices ?] in
  let table = Enum.fold (fun tab (key, vlue) ->
    Hashtbl.add tab key vlue; tab) (refresh state.n) init in
  let _,_,ftable = List.fold_left (oplus bound) (2, state, table) ilist in
  emax (values ftable);;

(* treematch: perform predetermined number of trials, returning the highest score match *)
let treematch ?bound:(bound=0.0) state tree =
  let trial ilist (boundt,ll) _ =
    let () = state.color <- state.new_colors () in
    let (q, l, _) = singletrial state boundt ilist in
    if (boundt > q) then (boundt, ll) else (q, l) in
  let ilist = ielist tree in
  let (score, l) = Enum.fold (trial ilist) (bound, []) (0 --^ state.times) in
  (score, [state.make_assignment (List.rev l)]);;



(* ------------ Messy computing of 'state' -------- *)

(*  The inputs are two matrices amatrix and wmatrix.
    wmatrix is an edge similarity matrix, i.e, 
    wmatrix.(i).(j) contains the similarity between edge i
    in graph 1 and edge j in graph 2. Edges are enumerated
    as in the row major linear storage of the adjacency 
    matrix of the graph. In any case, self loops, i.e., 
    the diagonals are not counted. In the undirected case,
    when the adjacancy matrix is symmetric, 
    the upper triangular entries are also not counted.
    Whether graphs are directed or not is determined by 
    the dimensions of wmatrix which correspond to the number of 
    counted edges (in the complete simple graphs).
    The parameters m and n correspond to the number of vertices
    in graph 1 and graph 2, respectively.
*)

(* figure out whether the graphs are directed or not *)
let directedp m n wmatrix =
  let nrow,ncol = Array.length wmatrix, Array.length wmatrix.(0) in
  (nrow == (intexp m 2) - m,  ncol == (intexp n 2) - n);;

(* map edge (i,j) to index in wmatrix *)
let eidx m n wmatrix =
  let (d1, d2) = directedp m n wmatrix and
      f1d = (fun i j -> let c = i*m + j in c - (c/(m + 1) + 1)) and
      f2d = (fun i j -> let c = i*n + j in c - (c/(n + 1) + 1)) and
      fu = (fun i j ->
	let (x,y) = (if i > j then (i,j) else (j,i)) in (x*(x - 1))/2 + y) in
  ((if d1 then f1d else fu), (if d2 then f2d else fu));;

let make_v amatrix = (fun a b -> amatrix.(a).(b));;

let make_w m n wmatrix =
  let edgei1, edgei2 = eidx m n wmatrix in
  (fun (a1, a2) (b1, b2) ->
    wmatrix.(edgei1 a1 a2).(edgei2 b1 b2));;
  
(* -- Create initial colorsets --*)
let make_colors n k =
  [? Array : 1 lsl x | x <- Enum.map (fun _ -> Random.choice (0 --^ k)) (0 --^ n) ?];;
    

(* --- Pruning of solutions --- *) 
(* -- compute array that estimates the max additional value an assignment can
      get when extended by i tuples --*)
let boundsarray amatrix wmatrix tree =
  let m, n = Array.length amatrix, Array.length amatrix.(0) and
      tnodes = preorder tree and
      tpairs = preedges tree in
  let ei1, ei2 = eidx m n wmatrix and
      k = List.length tnodes in
  let teidxs = List.map (uncurry ei1) tpairs in
  let widxs = product teidxs [? List : x | x <- 0 --^ (Array.length wmatrix.(0)) ?] and
      aidxs = product tnodes [? List : x | x <- 0 --^ n ?] in
  let wvals = List.take k (List.sort ~cmp:(fun x y -> compare y x) (List.map (fun (i,j) -> wmatrix.(i).(j)) widxs)) and
      avals = List.take k (List.sort ~cmp:(fun x y -> compare y x) (List.map (fun (i,j) -> amatrix.(i).(j)) aidxs)) in
  let tmp = List.map2 (+.) wvals avals in
  Array.rev (cumsum (+.) 0. (0.::tmp));;


(* -- function to create function for filtering sub-matches  -- *)
let keep_p barray nextlen bound q =
  bound < q +. barray.(nextlen);;

(* - list of edges in an undirected complete graph of order n *)
let kedgesudir n =
  let l = List.of_enum (0 --^ n) in
  List.rev [? List : (x,y) | (x,y) <- List : product l l; x > y ?];;

(* - list of edges in a directed complete graph of order n *)
let kedgesdir n =
  let l = List.of_enum (0 --^ n) in
  List.rev [? List : (x,y) | (x,y) <- List : product l l; x != y ?];;

(* - indices of elements in array a for which p holds *)
let which p a = filter (fun i -> p a.(i)) (0 --^ (Array.length a));;

(* extract an adjacency matrix (array of arrays representation) from watrix *)
let adjlist sel m n wmatrix =
  let dt, dg = directedp m n wmatrix in
  let eit, eig = eidx m n wmatrix in
  let row m i = m.(i) and
      col m i = Array.map (fun r -> r.(i)) m in
  let k,ei,d,acc = sel ((m,eit,dt,row), (n,eig,dg,col)) in
  let edges = (if d then kedgesdir else kedgesdir) k and
      good a = (Array.fold_left (+.) 0. a) > 0.0 and
      am = Array.make_matrix k k 0 in
  begin
    List.iter (fun (a,b) ->
      let i = ei a b in
	am.(a).(b) <- (if (good (acc wmatrix i)) then 1 else 0)) edges;
    (*printmatrix "%d" am;*)
    Array.map (fun row -> Array.of_enum (which (fun x -> x > 0) row)) am;
  end;;

let oadjlist = adjlist fst;;  (* adjlist for origin *)
let tadjlist = adjlist snd;;  (* adjlist for target *)

(* compute the 'state' for a given program input and tree -- *)
let create_state eps wmatrix amatrix tree =
  let m = Array.length amatrix and
      n = Array.length amatrix.(0) and
      barray = boundsarray amatrix wmatrix tree and
      k = order tree in
  let tvertices = preorder tree in {
    times = int_of_float ((log (1.0 /. eps)) *. (exp (float_of_int k)));
    gvertices = [? List : x | x <- 0 --^ n ?];
    new_colors = (fun () -> make_colors n k);
    n = n;
    root = root tree;
    color = make_colors n k;
    v = make_v amatrix;
    w =  make_w m n wmatrix;
    keep  = keep_p barray; 
    make_assignment = (fun l -> zip tvertices l);
    tadj = tadjlist m n wmatrix;
  };;

(* -------- GA Stuff ------- *)
open Ga;;
let ascore v w tedges tnodes ind =
  try 
    let l = [? List : (a,b) | (a,b) <- List : (List.map2 (fun x y -> (x,y)) tnodes (Array.to_list ind)) ?] in
    let lmap = Map.IntMap.of_enum [? (a,b) | (a,b) <- List: l ?] in
    let fl i = Map.IntMap.find i lmap in
    let ascore = List.fold_left (+.) 0.0 (List.map (uncurry v) l) and
	wscore = List.fold_left (+.) 0.0 (List.map (fun (a,b) -> w (a, fl a) (b, fl b)) tedges) in
    ascore +. wscore
  with Not_found -> 0.0;;
let flipw w (a1,b1) (a2,b2) = w (a1, a2) (b1, b2);;
let ga_bound verbose n state tree =
      let tedges = preedges tree and
	  tnodes = preorder tree in
      let gafit = ascore state.v (flipw state.w) tedges tnodes in
      let () = if verbose then debug " (running GA..." and
	  gabound, _ = runaga 300 128 (makeri gafit (order tree) n) gafit in
      let  () = if verbose then debug ("bound: " ^ (Printf.sprintf "%f" gabound) ^ ") ") in
      gabound;;


(* ----- Create weighted bi-partite graph of tree match assignments ------ *)
  
let graphmatch ?verbose:(verbose=false) ?gbound:(gbound=false) eps amatrix wmatrix trees =
  let m, n = Array.length amatrix, Array.length amatrix.(0) in
  let bigraph = Array.make_matrix m n 0.0 in
  let dotree i tree =
    let state = create_state eps wmatrix amatrix tree in
    let start_bound = if gbound then
          (ga_bound verbose n state tree) -. 1.0e-10
    else
      0.0 in
    let (score, matches) = treematch ~bound:start_bound state tree in
    begin
      List.iter (fun (a,b) -> bigraph.(a).(b) <- bigraph.(a).(b) +. score) (List.concat matches);
      if verbose then begin
	debug ("tree # " ^ (Int.to_string i) ^ "  score: " ^ (Float.to_string score) ^ "\n");
      end;
    end;
  in
  Enum.iteri dotree trees;
  bigraph;;


(* -------------------------- Program entry point ------------------------ *)
					      
let version = "1.3";;
let main() = 
  flush stderr;
  let eps = ref 0.2 and
      verbose = ref false and
      seed = ref (int_of_float (Unix.time ())) and
      gbound = ref false and
      file1 = ref "" and
      file2 = ref "" and
      file3 = ref "" and
      files = ref [] and
      usage = (Sys.argv.(0) ^ " version " ^ version ^
                 ". (c) 2011 Staal A. Vinterbo.\n" ^
	       "usage: " ^ Sys.argv.(0) ^
               " [-e FLOAT] [-s INT] [-v] [-g] TFILE AFILE WFILE") in
  let speclist = [ ("-e", Arg.Float  (fun ein -> eps := ein),
                    ": set eps to FLOAT.");
                   ("-g", Arg.Unit    (fun () -> gbound := true;()),
                    ": compute initial bounds using GA.");
		   ("-s", Arg.Int    (fun sin -> seed := sin),
                    ": seed random number generator with INT.");
		   ("-v", Arg.Unit    (fun () -> verbose := true;()),
                    ": print progress notes.");
		   ("-stdin", Arg.Unit (fun () -> files := ("-"::!files);()),
		    ": specify stdin as input for this positional file argument.") 
		 ] in
  let () = begin
    Arg.parse speclist (fun x -> files := (x::!files)) usage;
    if List.length !files != 3 then begin
      Printf.eprintf "Need three filenames. Try -help for usage information.\n";
      raise (Arg.Bad "No filenames given.");
    end;
    Random.init !seed;
    file1 := List.last !files;
    file2 := List.nth !files 1;
    file3 := List.first !files;
    if !verbose then
      debug ("Reading data from " ^ !file1 ^ " " ^ !file2 ^ " " ^ !file3 ^
	     ".\nParameters: eps = " ^ (Float.to_string !eps) ^ ".\n");
  end in
  let trees = Enum.map tree_from_string (readlines !file1) and
      amatrix = readmatrix !file2 and
      wmatrix = readmatrix !file3 in
  begin
    debug("Starting computations, please wait..\n");
    let result = graphmatch ~verbose:!verbose ~gbound:!gbound !eps amatrix wmatrix trees in
    printmatrix "%f" result;
  end;;


(* --- call the entry point --- *)
main();;


