(* Note: This incantation allows us to tell the compiler to temporarily stop
   notifying us when we have unused functions and values. Feel free to delete
   after completing exercise 6. *)
[@@@disable_unused_warnings]

open Core

module Node_id : sig
  (** A [Node_id.t] uniquely identifies a node in a graph. We will using it later for
      looking up and setting the state of nodes in the course of our path search. *)
  type t [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t

  val create : int -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  (* Remember that this is the syntax for include modules such as [Map] and
     [Set] that are provided by [Comparable.Make] to our module. In our case,
     we use [Node_id.Map.t] in the [Nodes.t]. *)
  include T
  include Comparable.Make (T)

  let create id = id
end

module Edge = struct
  (** This type represents an edge between two nodes [a] and [b]. Note that since we are
      working with undirected graphs, the order of [a] and [b] do not matter. That is, an
      [Edge.t] { a = 1; b = 2; ... } is equivalent to { a = 2; b = 1; ... } *)

  module T = struct
    type t =
      { a : Node_id.t
      ; b : Node_id.t
      ; distance : int
      }
    [@@deriving compare, equal, sexp]

    let get_neighbor node_id edge =
      match Node_id.equal edge.a node_id || Node_id.equal edge.b node_id with
      | true ->
        (match Node_id.equal edge.a node_id with
         | true -> Some edge.b
         | false -> Some edge.a)
      | false -> None
    ;;
  end

  include T
  include Comparable.Make (T)
end

module Edges = struct
  type t = Edge.t list [@@deriving sexp]

  (* Exercise 1: Given a [t] (list of edges) and a [Node_id.t], implement a
     function that returns a list of neighboring nodes with their
     corresponding distances. *)
  let neighbors t node_id : (Node_id.t * int) list =
    List.filter_map t ~f:(fun edge ->
      match Edge.get_neighbor node_id edge with
      | None -> None
      | Some neighbor -> Some (neighbor, edge.distance))
  ;;

  (* We've left all of the tets in this file disabled. As you complete the
     exercises, please make sure to remove `[@tags "disabled"]` and run `dune
     runtest` to ensure that your implementation passes the test. *)
  let%expect_test "neighbors" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      Edge.
        [ { a = n0; b = n1; distance = 1 }
        ; { a = n1; b = n2; distance = 3 }
        ; { a = n2; b = n3; distance = 2 }
        ; { a = n2; b = n4; distance = 1 }
        ; { a = n3; b = n5; distance = 5 }
        ; { a = n4; b = n5; distance = 1 }
        ]
    in
    let neighbors = neighbors t n2 in
    print_s [%message (neighbors : (Node_id.t * int) list)];
    [%expect {| (neighbors ((1 3) (3 2) (4 1))) |}]
  ;;
end

module Node = struct
  module State = struct
    type t =
      | Origin (** Used to mark the node where our search starts *)
      | Unseen (** Used to mark unexplored Nodes *)
      | Todo of
          { distance : int
          ; via : Node_id.t
          }
      (** Used to mark nodes that have been encountered but have not been processed yet *)
      | Done of { via : Node_id.t }
      (** Used to mark nodes that we are finished processing *)
    [@@deriving sexp]
  end

  type t = { mutable state : State.t } [@@deriving fields ~getters, sexp]
  let init () = { state = Unseen }
  let set_state t state = t.state <- state
end

module Nodes = struct
  (** This type represents a stateful collection of nodes in our graph. These [Node.t]s
      will be updated in the course of our graph search to keep track of progress. *)
  type t = Node.t Node_id.Map.t [@@deriving sexp]

  (* Exercise 2: Given a list of edges, create a [t] that contains all nodes
     found in the edge list. Note that you can construct [Node.t]s with the
     [Node.init] function. *)
  let of_edges (edges : Edge.t list) : t =
    let half_map = List.fold edges ~init:Node_id.Map.empty ~f:(fun map edge -> Map.add_exn map ~key:edge.a ~data:(Node.init ())) in
    let full_map = List.fold edges ~init:half_map ~f:(fun map edge -> Map.add_exn map ~key:edge.a ~data:(Node.init ())) in
    full_map
  let find = Map.find_exn
  let state t node_id = find t node_id |> Node.state

  let set_state t id state =
    let node = Map.find_exn t id in
    Node.set_state node state
  ;;

  (* Exercise 3: Given a [t], find the next node to process by selecting the
     node with the smallest distance along with its via route. *)
  let next_node (t : t) : (Node_id.t * (int * Node_id.t)) option =
    let todo_nodes = Map.filter t ~f:(fun value -> match value.state with | Todo _ -> true | _ -> false) in
    let todo_nodes_list = Map.to_alist todo_nodes in
    let min_function ((node_id1, node1) : (Node_id.t * Node.t)) ((node_id2, node2) : (Node_id.t * Node.t)) =
      match (node1.state, node2.state) with
      | (Todo state1, Todo state2) ->
        (match state1.distance > state2.distance with
        | true -> 1
        | false -> -1)
      | _ -> 0 in
    let min_todo_node = List.min_elt todo_nodes_list ~compare:min_function in
    match min_todo_node with
    | None -> None
    | Some (node_id, node) -> match node.state with | Todo {distance; via} -> Some (node_id, (distance, via)) | _ -> None

  let%expect_test ("next_node") =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let next_node = next_node t in
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}]
  ;;

  (* Exercise 4: Given a [t] that has already been processed from some origin
     -- that is the origin has been marked as [Origin] and nodes on the
     shortest path have been marked as [Done] -- return the path from the
     origin to the given [distination]. *)
  let find_node_id (t : t) (node : Node.t) =
    let keys = Map.keys t in
    let wrapped_id = List.filter_map keys ~f:(fun id ->
      match (Map.find t id) with
      | Some n ->
        (match n.state with
        | Done {via} -> (match node.state with | Done obj -> (match Node_id.equal via obj.via with | true -> Some id | false -> None) | _ -> None)
        | _ -> None )
      | _ -> None) in
    match List.hd wrapped_id with
    | None -> None
    | id -> id
  let rec helper (t : t) (node : Node.t) : Node_id.t list =
    match node.state with
    | Done {via} -> let id = find_node_id t node in
      (match id with | None -> [] | Some id -> List.append (helper t (Map.find_exn t via)) [id])
    | _ -> []
  let path (t : t) (destination : Node.t) : Node_id.t list = helper t destination

  (* Excercise 5: Write an expect test for the [path] function above. *)
  let%expect_test "path" = ()
    (* let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let path = path t n5 in
    List.iter path ~f:(fun node -> node.)
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}] *)
end

(* Exercise 6: Using the functions and types above, implement Dijkstras graph
   search algorithm! Remember to reenable unused warnings by deleting the
   first line of this file. *)
let check_stop_condition (graph : Nodes.t) = Map.for_all graph ~f:(fun node -> match node.state with | Done _ -> true | Origin -> true | _ -> false)

let find_neighbors (graph : Nodes.t) (node : Node.t) (edges : Edges.t) = 
  Map.filter graph ~f:(fun value -> )

let helper (graph : Nodes.t) ~(node : Node.t) =
  (* check if everything has been visited yet; if not: *)
  match check_stop_condition graph with
  | true -> ()
  | false -> 
  (* find all neighboring nodes and update their values to either the min of their value or the value of the current node plus its distance to that neighbor *)
  let nbrs = find_neighbors graph node in

  (* assign the min valued unfinished node as the current node *)
let shortest_path ~edges ~origin ~destination : Node_id.t list =
  let graph = Nodes.of_edges edges in
  let graph = Map.mapi graph ~f:(fun ~key ~data -> match (Node_id.equal key origin) with | true -> Node.set_state data Origin; data | false -> Node.set_state data Unseen; data) in
  helper graph ~node:origin;
  Nodes.path graph destination

let%expect_test ("shortest_path" [@tags "disabled"]) =
  let n = Node_id.create in
  let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
  let edges =
    Edge.
      [ { a = n0; b = n1; distance = 1 }
      ; { a = n1; b = n2; distance = 1 }
      ; { a = n2; b = n3; distance = 1 }
      ; { a = n2; b = n4; distance = 1 }
      ; { a = n3; b = n5; distance = 2 }
      ; { a = n4; b = n5; distance = 1 }
      ]
  in
  let origin = n0 in
  let destination = n5 in
  let path = shortest_path ~edges ~origin ~destination in
  print_s ([%sexp_of: Node_id.t list] path);
  [%expect {| (0 1 2 4 5) |}]
;;

(* Exercise 7: Add some more test cases, exploring any corner cases you can
   think of. *)
