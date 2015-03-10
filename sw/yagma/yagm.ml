(* -*-OCaml-*-
********************************************************************************
*
* File:         yagm.ml
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
*               Alternatively the matrices A and W contain the graphs
*               G1 and G2 respectively. The first row contains the weights
*               associated with vertices, the subsequent rows are the weighted
*               adjacency matrix of the graph.
*
*               The program within this file can be compiled as follows:
*   ocamlopt -thread -package batteries,batteries.pa_comprehension.syntax \
*            -syntax camlp4o -linkpkg yagm.ml -o yagm
*
* Author:       Staal Vinterbo
* Created:      Thu Aug  4 10:12:08 2011
* Modified:     Tue Nov  1 19:57:00 2011 (Staal Vinterbo) staal@dink
* Language:     caml
* Package:      N/A
* Status:       Experimental
*
* yagm.ml is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* yagm.ml is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with yagm.ml; if not, write to the Free Software
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

(* numerical tolerance for comparisons *)
let tolerance = (max epsilon_float min_float);;

(* cumulative "sum" of elements in a list *)
let cumsum op zero list =
  Array.of_list (List.rev (snd (List.fold_left (fun (x,l) y ->
    (op x y, (op x y)::l)) (zero,[]) list)));;

let unpack f (a,b) = f a b;;

(* debug print function *)
let debug s = prerr_string s;flush stderr;;

(* exponentiations in ocaml is only for floats *)
let intexp a b = int_of_float ((float_of_int a) ** (float_of_int b));;

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

(* Array.take is missing from the standard library/Batteries *)
let atake k a = Array.sub a 0 k;;

(* sample n elements from enum b without replacement *)
let sample n b = atake n (Random.shuffle (Array.enum b));;  

(* List.combine *)
let zip a b = List.map2 (fun x y -> (x,y)) a b;;

let lmax (x::xs) = List.fold_left max x xs;;

let lkeepmax ?cmp:(cmp=compare) l = match l with
    [] -> []
  | _ ->
    let max_ a b = if cmp a b < 0 then b else a in
    let maxelm = List.reduce max_ l in
    List.filter (fun a -> cmp a maxelm == 0) l;;

    
(* --------- Tree-related code ------------ *)

type tree = Node of (int * tree list);;

(* "left" preorder fold  *)
let preorder_left ?up:(up=(fun acc -> acc)) f g init tree =
  let rec helper acc tree =
    match tree with 
      | Node (nval, []) -> up (g acc (f nval))
      | Node (nval, subtrees) ->
        let init = g acc (f nval) in
        up (List.fold_left helper init subtrees) in
  helper init tree;;

(* preorder listing of vertices in a tree *)
let treenodes tree = (* linear time, using left preorder fold *)
  let f = (fun x -> x) and
      g = (fun l x -> x::l) in
  let l = preorder_left f g [] tree in
  List.rev l;;

(* preorder listing of edges in a tree *)
let treeedges tree = 
  let f = fun a -> a and
      g = (fun (stack,l) b ->
        match stack with
            [] -> (b::stack, l)
          | (a::rest) -> (b::stack, (a,b)::l)) and
      up = (fun (stack, l) ->
        match stack with
            [] -> (stack, l)
          | (top::rest) -> (rest, l)) in
  List.rev (snd (preorder_left ~up:up f g ([],[]) tree));;

(* the number of vertices in the tree *)
let order tree =
  preorder_left (fun x -> 1) (+) 0 tree;;

let prelist tree = 
  let up (c, s, l) = match s with [] -> (c, s, l) | (h::t) -> (c, t, l) and
      f _ = 1 and
      g (c, s, l) _ = match s with [] -> (c + 1, c::s, c::l) | (h::t) -> (c + 1, c::s, h::l) in
  let _,_,ll = preorder_left ~up:up f g (0,[], []) tree in Array.of_list (List.rev ll);;

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


(* ------- Color colding using a left preorder fold --------- *)



type state = {
    times : int;
    gvertices : int list;
    v : (int -> int -> float);
    w : ((int*int) -> (int*int) -> float);
    new_colors : (unit -> (int array));
    mutable color : int array;
    parents : int array;
    keep : (float -> float -> int -> bool);
    make_assignment : ((int list) -> (int * int) list);
    tadj : int array array;
  } ;;


let extend_triplet state (s, q, l) (t, t') index =
   let u = List.nth l index in
   [? List : (s lor state.color.(u'), q +. (state.v t' u') +. (state.w (t, t') (u, u')), l @ [u']) |
      u' <- Array : state.tadj.(u) ; s land state.color.(u') == 0 ?];;

let tblupdate index h (s, q, l) =
   let u = List.nth l index in
   match Hashtbl.find_option h (s, u) with
     None -> begin Hashtbl.add h (s, u) (q, l); h end
   | Some (q',l') -> begin if (q > q') then Hashtbl.replace h (s,u) (q,l); h end;;

let foldfun state index pindex t t' h (s, q, l) =
  let extensions = extend_triplet state (s, q, l) (t, t') index in
  List.fold_left (tblupdate pindex) h extensions;;

let newl state bigl t t' index pindex =
  let h = Hashtbl.create (List.length bigl) in
  let h2 = List.fold_left (foldfun state index pindex t t') h bigl in
  [? List : (s,q,l) | ((s,u), (q, l)) <- Hashtbl.enum h2 ?];;

let init state vy =
  [? List : (state.color.(u), state.v vy u, [u]) |
      u <- List : state.gvertices ?];;
       
let oplus state bound (counter, stack, s) t' =
  let l = match stack with
      [] -> init state t'
    | ((index, t)::rest) -> newl state s t t' index state.parents.(counter) in 
  let filterp (s, q, l) = state.keep bound q (counter + 1) in
  (counter+ 1, (counter, t')::stack, List.filter filterp l);;

let up (c, s, l) = match s with [] -> (c, s, l) | (a::rest) -> (c, rest, l);;

let check_ass state tree ass =
  let tv = treenodes tree and
      te = treeedges tree in
  let lmap = Map.IntMap.of_enum [? (a,b) | (a,b) <- List: ass ?] in
  let fl i = Map.IntMap.find i lmap and
      fsum l = List.fold_left (+.) 0. l in
  ((fsum (List.map (fun t -> state.v t (fl t)) tv))
     +. (fsum (List.map (fun (t,t') -> state.w (t,t') ((fl t), (fl t'))) te)));;


let treematch ?bound:(bound=0.0) state tree =
  let trd (a,b,c) = c in
  let trial (bound,l) _ =
    let () = state.color <- state.new_colors () in
    let	comp = (fun (_,q1,_) (_,q2,_) -> compare q1 q2) in
    let oplus_ = (oplus state bound) and
	pi x = x in
    let _,_, matches = preorder_left ~up:up pi oplus_ (0, [], []) tree in
    let result = lkeepmax ~cmp:comp matches in
    match result with
        [] ->  (bound, l)
      | ((news, newq, newl)::tail) -> 
        (*debug("trial colorset: " ^ (Int.to_string news) ^ " score:" ^ (Float.to_string newq) ^ "\n");*)
        match compare newq bound with
            -1 -> (bound, l)
          |  0 -> (bound, l @ (List.map trd result))
          |  _ -> (newq, List.map trd result) in
  let (score, l) = Enum.fold trial (bound, []) (0 --^ state.times) in
  let check_assignment ass = begin
    let cscore = check_ass state tree ass in begin
      debug((Printf.sprintf "[%f, %f]" score cscore));
      assert ((Float.abs (score -. cscore)) < tolerance);
    end;
    ass
  end in 
  let asses = List.map state.make_assignment l in
  if true then (score, asses) else (score, List.map check_assignment asses);;


(* ------------ Messy details of computing 'state' -------- *)



(*  SIMILARITY INPUT
    The inputs are two matrices amatrix and wmatrix.
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
      tnodes = treenodes tree and
      tpairs = treeedges tree in
  let ei1, ei2 = eidx m n wmatrix and
      k = List.length tnodes in
  let teidxs = List.map (unpack ei1) tpairs in
  let widxs = product teidxs [? List : x | x <- 0 --^ (Array.length wmatrix.(0)) ?] and
      aidxs = product tnodes [? List : x | x <- 0 --^ n ?] in
  let wvals = List.take k (List.sort ~cmp:(fun x y -> compare y x) (List.map (fun (i,j) -> wmatrix.(i).(j)) widxs)) and
      avals = List.take k (List.sort ~cmp:(fun x y -> compare y x) (List.map (fun (i,j) -> amatrix.(i).(j)) aidxs)) in
  let tmp = List.map2 (+.) wvals avals in
  cumsum (+.) 0. (0.::tmp);;


(* -- function to create function for filtering sub-matches  -- *)
let keep_p k barray bound v i =
  let rem = k - i in
  bound < v +. barray.(rem);;

let kedgesudir n =
  let l = List.of_enum (0 --^ n) in
  List.rev [? List : (x,y) | (x,y) <- List : product l l; x > y ?];;

let kedgesdir n =
  let l = List.of_enum (0 --^ n) in
  List.rev [? List : (x,y) | (x,y) <- List : product l l; x != y ?];;

let which p a = filter (fun i -> p a.(i)) (0 --^ (Array.length a));;

(* extract an adjacency list (array of arrays representation) from watrix *)
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


module EdgeSet =
  Set.Make(struct let compare = Pervasives.compare; type t = int*int end);;

let target_edges m n wmatrix =
  let am = tadjlist m n wmatrix in
  let edges = [? List : (x,y) | x <- 0 --^ n; y <- Array: am.(x) ?] in
  List.fold_left (fun set elm -> EdgeSet.add elm set) EdgeSet.empty edges;;

(* compute the 'state' for a given program input and tree -- *)
let create_state_sim eps wmatrix amatrix tree =
  let m = Array.length amatrix and
      n = Array.length amatrix.(0) and
      barray = boundsarray amatrix wmatrix tree and
      k = order tree in
  let tvertices = treenodes tree in {
    times = int_of_float ((log (1.0 /. eps)) *. (exp (float_of_int k)));
    gvertices = [? List : x | x <- 0 --^ n ?];
    new_colors = (fun () -> make_colors n k);
    color = make_colors n k;
    parents = prelist tree;
    v = make_v amatrix;
    w =  make_w m n wmatrix;
    keep = keep_p k barray;
    make_assignment = (fun l -> zip tvertices l);
    tadj = tadjlist m n wmatrix;
  };;

(* ADJACENCY INPUT
   matrices A and W are representations of two graphs G' and G 
   respectively. Graph G' must contain the union of all input
   trees, and the matrix A consists of a first row of vertex 
   weights, and the subsequent rows are the weighted adjacency 
   matrix of G'. W is the corresponding matrix for the target 
   graph G.
*)


let sim a b = 1. -. (Float.abs (a -. b)) (*
  if a == b then 1.
  else 1. -. (Float.abs (a -. b))  /. (max (Float.abs(a)) (Float.abs(b))) *);;

let make_v2 amatrix wmatrix =
  (fun t u -> sim amatrix.(0).(t) wmatrix.(0).(u));;

let make_w2 amatrix wmatrix =
  (fun (t,t') (u, u') -> sim amatrix.(t + 1).(t') wmatrix.(u + 1).(u'));;

let flip f x y = f y x;;
let boundsarray2 gedges tedges gnodes tnodes v w =
  let k = List.length tnodes in
  let taketop l = List.take k (List.sort ~cmp:(flip compare) l) in 
  let avals =
    taketop [? List: v t u | t <- List: tnodes; u <- List: gnodes ?] and
      wvals =
    taketop [? List: w a b | a <- List: tedges; b <- List: gedges ?] in
  let tmp = List.map2 (+.) wvals avals in
  let ba = cumsum (+.) 0. (0.::tmp) in
  begin
    (*debug ("barray " ^
             (Enum.fold (fun s x -> s ^ " " ^ (Float.to_string x)) ""
                (Array.enum ba)) ^ "\n"); *)
    ba
  end;;

let adjlist2 gmat =
  let n = Array.length gmat.(0) in
  let nbors arr =
      Array.of_enum (Enum.filter_map (fun i ->
        match arr.(i) with 0. -> None | _ -> Some i) (0--^n)) in
  Array.of_enum (Enum.map (fun i -> nbors gmat.(i)) (1--n));;

let graphedges admat =
  let edgs i a = [? List : (i, x) | x <- Array: a ?] in
  Enum.foldi (fun i a l -> (edgs i a) @ l) [] (Array.enum admat);;

let create_state_adj eps wmatrix amatrix tree =
  let n = Array.length wmatrix.(0) and
      v = make_v2 amatrix wmatrix and
      w = make_w2 amatrix wmatrix and
      tadj = adjlist2 wmatrix and
      tedges = treeedges tree and
      tnodes = treenodes tree in
  let gnodes = [? List : x | x <- 0 --^ n ?] and
      gedges = List.filter (fun (a,b) -> a != b) (graphedges tadj) in
  let barray = boundsarray2 gedges tedges gnodes tnodes v w and
      k = order tree in {
    times = int_of_float ((log (1.0 /. eps)) *. (exp (float_of_int k)));
    gvertices = gnodes;
    new_colors = (fun () -> make_colors n k);
    color = make_colors n k;
    parents = prelist tree;
    v = v;
    w = w;
    keep = keep_p k barray;
    make_assignment = (fun l -> zip tnodes l);
    tadj = tadj;
  };;


  


(* ----------------- Ugly Genetic algorithm code --------------- *)

type indt = float * int array;;
type popt = indt list;;
type fitt = int array -> float;;

open Std;;

(* generate random permutation on n symbols *)
let randperm n =
  Random.shuffle (0--^n)


(* generates a function that generates a random individual.
   Note that n must be smaller than 2^30. *)
let makeri (fitness:fitt) (m:int) (n:int) =
  (fun () -> let v = atake m (randperm n) in (fitness v, v));;


(* inverse of permutation *)
let invperm p = 
  let a = Array.make (Array.length p) (-1) in
  Array.iteri (fun i x -> a.(x) <- i) p;
  a;;

(* apply permutation *)
let permute what by = Array.map (fun where -> what.(where)) by;;

(* compute the preimage of an injection a as a Hashtbl.
   a represents an injection a(x) = a.(x). *)
let backedges a =
  let h = Hashtbl.create (Array.length a)
  in Array.iteri (fun i x -> Hashtbl.add h x i) a; h;;

(*  modified and extended cycle crossover for injections.

    we want to randomly mix two injections, creating an injection c
    such that
       c(x) in {a(x), b(x)}.

    Inputs are two lists of length m representing two injections
    a,b:{0,1,...,m} -> {0,1,...,n} for some n >= m (a sends i to a[i]).

    Algorithm sketch:
    The algorithm is an extension of cycle crossover with random selection
    of cycles (not altenating). This is extended to the injection case
    as explained below.

    If n = m we can view each of a and b as a perfect bipartite matching
    between Domain and Codomain.
    Let B be the bipartite graph constructed by the union of a and b.
    Each vertex in B has degree at most two, and B can be decomposed into
    disconnected components in which either all vertices have degree 1 or 2.
    All components where vertices have degree 1 are single edges, and
    represent the set of shared assigments. These are kept as they are.
    The remaining components are of vertice degree two, and are cycles.
    Of these cycles, we are only allowed to include every other edge for the
    result to be a perfect matching. Starting at a Domain vertex we can choose
    between the two directions we want to go, one direction includes
    edges from a, the other includes edges from b. We do this until we have
    traversed all cycles.

    If n > m, we can "ignore" cycle parts that have vertices outside
    of Domain and the union of Ranges. We simply stop processing an alternating
    path when we venture into the "ignored" part. The idea is to sample among
    all possible extensions of a and b into bijections and then processing
    as in the case where n = m.
*)
let mix a' b' =
  let perm = randperm (Array.length a') in
  let a = permute a' perm and
      b = permute b' perm in 
  let alen = Array.length a and
      used = Array.make (Array.length a) false and
      mixed = Array.make (Array.length a) (-1) and
      aback = backedges a and
      bback = backedges b and 
      start = ref 0 in
  while !start <> -1 & not used.(!start) do
    let (there,back) = if (Random.float 1.0) > 0.5 
      then (a, bback) else (b, aback) and
        x = ref !start
    in
    while !x <> -1 & not used.(!x) do
      mixed.(!x) <- there.(!x);
      used.(!x) <- true;
      x := if Hashtbl.mem back there.(!x) then
          Hashtbl.find back there.(!x) else -1
    done;
    while !start < alen & used.(!start) do start := !start + 1 done;
    if !start = alen then start := -1
  done;
  permute mixed (invperm perm);;

(* works only for limited length arrays 2^10 *)
let asample k a = atake k (Random.shuffle (Array.enum a));;


(* stochastic universal sampling *)
let sus (randval:float) (n:int) (pop:popt) =
  let sw = List.fold_left (+.)  0.0 (List.map fst pop) in
  let inc = sw/.(float n) in
  let pairs = List.rev (List.sort ~cmp:Pervasives.compare pop) in
  let rec next p t i = match p with
      [] -> failwith "sus panic"
    | ((xw,x)::xs) ->
      if t -. xw < 0.0 then (p, t +. inc, i, (xw,x))
      else next xs (t -. xw) (i + 1)
  in
  let rec iter p t i n acc = if n < 1 then acc else
      let (p', t', i', x') = next p t i in iter p' t' i' (n - 1) (x'::acc) 
  in List.rev (iter pairs (randval/.(float n) *. sw) 0 n []);;



(* generates a function that performs xover *)
let makexo (fitness:fitt) =
  (fun (af,a) (bf,b) ->
    List.map (fun v -> (fitness v, v)) [(mix a b); (mix a b)]);;

(* generates a function that mutatates *)
let makemut (fitness:fitt) (ri:unit -> indt) =
  (fun (vf,v) -> let w = mix v (snd (ri ())) in (fitness w, w));;

(* quick and blah shuffle of lists... *)
let blahshuffle l =
  let cmp _ _ = (Random.int 2) * 2 - 1 in List.sort ~cmp:cmp l;;

(* sample a quarter of the population to be parents and mate them,
   return list of offspring *)
let crossover (xo:indt->indt->indt list) (pop:popt) =
  let plen = List.length pop in
  let parents = sus (Random.float 1.) (plen/4) pop in
  let (moms, dads) = List.split_at (plen/8) (blahshuffle parents) in
  List.concat (List.map2 xo moms dads);;

(* sample psize/div individuals and mutate,
   returning results in a list *)
let mutate (mut:indt -> indt) (div:int) (pop:popt) =
  let plen = List.length pop in
  List.map mut (sus (Random.float 1.) (plen/div) pop);;

(* loop a function n times  returning results in a list *)
let gloop g init n =
  let rec helper g n y acc =
    if n > 0 then
      let x = g y in helper g (n - 1) x (x::acc)
    else acc in
  List.rev (helper g n init []);;

(* remove duplicates and replenish with random elements *)
let diversify ri (pop:popt) =
  let n = List.length pop and
      newpop = List.sort_unique Pervasives.compare pop in
  if List.length newpop = n then newpop else
    newpop @ (gloop (fun _ -> ri()) (0.,[|1|]) (n - (List.length newpop)));;

(* create an initial random population, using ri to generate each
   individual. Return a list of n individuals. *)
let initpop ri n =
  let il = List.of_enum (1--n) in List.map (fun _ -> ri ()) il;;


(* do one generation: generational and elitist *)
let generation (xo:indt->indt->indt list) (mut:indt->indt) (diversify:popt->popt) (div:int) (pop:popt) =
  let ox = crossover xo pop and
      om = mutate mut div pop in
  let cand = List.rev (List.sort (diversify (List.concat [om ; ox ; pop]))) in
  (List.hd cand) :: (sus (Random.float 1.) ((List.length pop) - 1) cand) ;;

(* run the algorithm, using a function gen : pop -> newpop.
   Waits wait generations without improvement before halting. *)
let run ?l:((log, strm) = (false, stderr)) wait gen pop = 
  let rec iter delay pop (bestw,bestx) =
    if delay < 1 then (bestw,bestx) else
      let ((xw,x)::rest) = gen pop in
      if log then
        IO.write_line strm (String.of_float xw);
      if xw > bestw then iter wait ((xw,x)::rest) (xw,x)
      else iter (delay - 1) ((xw,x)::rest) (bestw,bestx)
  in
  iter wait pop (List.max pop);;

(* the entry point to the algorithm for the assignment problem *)
let runaga ?l:((log, strm) = (false, stderr)) wait psize ri fit =
  let xo = makexo fit and
      mut = makemut fit ri in
  (* let gen = generation xo mut (diversify ri) 10 in *)
  let gen = generation xo mut (fun x -> x) 10 in
  run ~l:(log,strm) wait gen (initpop ri psize);;

(* -------- GA Fitness ------- *)

let ascore v w tedges tnodes ind =
  try 
    let l = [? List : (a,b) | (a,b) <- List : (List.map2 (fun x y -> (x,y)) tnodes (Array.to_list ind)) ?] in
    let lmap = Map.IntMap.of_enum [? (a,b) | (a,b) <- List: l ?] in
    let fl i = Map.IntMap.find i lmap in
    let ascore = List.fold_left (+.) 0.0 (List.map (unpack v) l) and
	wscore = List.fold_left (+.) 0.0 (List.map (fun (a,b) -> w (a, fl a) (b, fl b)) tedges) in
    ascore +. wscore
  with Not_found -> 0.0;;
let flipw w (a1,b1) (a2,b2) = w (a1, a2) (b1, b2);;
let ga_bound verbose n state tree =
      let tedges = treeedges tree and
	  tnodes = treenodes tree in
      let gafit = ascore state.v (flipw state.w) tedges tnodes in
      let () = if verbose then debug " (running GA..." and
	  gabound, _ = runaga 300 128 (makeri gafit (order tree) n) gafit in
      let  () = if verbose then debug ("bound: " ^ (Printf.sprintf "%f" gabound) ^ ") ") in
      gabound;;

(* ------------ End Ugly Genetic algorithm --------------- *)


(* ---------------------Implementation of overall Algorithm  ------------ *)
type inputmode = Similarity | Adjacency;;

(* ----- Matching graphs by sub-tree matching ------ *)

let graphmatch ?verbose:(verbose=false) ?gbound:(gbound=false) ?mode:(mode=Similarity) eps amatrix wmatrix trees =
  let m, n =
    (if mode == Similarity then Array.length amatrix, Array.length amatrix.(0)
     else Array.length amatrix.(0), Array.length wmatrix.(0)) in
  let bigraph = Array.make_matrix m n 0.0 in
  let create_state = (if mode == Similarity then
        create_state_sim else create_state_adj) in
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
	let mlen = List.length matches in
	debug ("tree # " ^ (Int.to_string i) ^ "  score: " ^ (Float.to_string score) ^
	       ", # matches: " ^ (Int.to_string mlen) ^ "\n");
      end;
    end;
  in
  Enum.iteri dotree trees;
  bigraph;;

(* -------------------------- Program entry point ------------------------ *)
					      
let version = "1.31";;
let main() = 
  flush stderr;
  let eps = ref 0.2 and
      verbose = ref false and
      gbound = ref false and
      mode = ref Similarity and
      seed = ref (int_of_float (Unix.time ())) and
      file1 = ref "" and
      file2 = ref "" and
      file3 = ref "" and
      files = ref [] and
      usage = (Sys.argv.(0) ^ " version " ^ version ^
                 ". (c) 2011 Staal A. Vinterbo.\n" ^
	       "usage: " ^ Sys.argv.(0) ^
               " [-e FLOAT] [-k INT] [-c INT] [-s INT] [-v] [-g] [-C] TFILE AFILE WFILE") in
  let speclist = [ ("-e", Arg.Float  (fun ein -> eps := ein),
                    ": set eps to FLOAT.");
                   ("-g", Arg.Unit    (fun () -> gbound := true;()),
                    ": compute initial bounds using GA.");
                   ("-m", Arg.Unit    (fun () -> mode := Adjacency;()),
                    ": files contain graph matrices (first row is vertex weights,\n"^
                      "       while following rows contain the weighted adjacency matrix).");
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
	     ".\nParameters: eps = " ^ (Float.to_string !eps) ^ 
             ", seed = " ^ (Int.to_string !seed) ^
             ", input mode = " ^ (if !mode == Similarity
               then "Similarity" else "Adjacency") ^ ".\n");
  end in
  let trees = Enum.map tree_from_string (readlines !file1) and
      amatrix = readmatrix !file2 and
      wmatrix = readmatrix !file3 in
  begin
    let result = graphmatch ~verbose:!verbose ~gbound:!gbound ~mode:!mode !eps amatrix wmatrix trees in
    printmatrix "%f" result;
  end;;


(* --- call the entry point --- *)
main();;


