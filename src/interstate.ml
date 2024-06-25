open! Core
module City = String
module Highway = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = Highway.t * City.t * City.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)
  end

  let rec parse_line tokens ~hwy : (string * string * string) list =
    let str_without_periods str = String.substr_replace_all str ~pattern:"." ~with_:"" in
    match List.length tokens with
    | 0 -> []
    | _ ->
      let first, last = List.hd_exn tokens, List.tl_exn tokens in
      let current_edges =
        List.map last ~f:(fun el ->
          ( str_without_periods first, str_without_periods el, hwy ))
      in
      let rest_edges = parse_line last ~hwy in
      List.append current_edges rest_edges
  ;;

  let get_edges_of_line line =
    let line = String.split line ~on:',' in
    let hwy, cities = List.hd_exn line, List.tl_exn line in
    parse_line cities ~hwy
  ;;

  let of_file file =
    In_channel.read_lines (File_path.to_string file)
    |> List.concat_map ~f:get_edges_of_line
    |> Connection.Set.of_list
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let _n = Network.of_file input_file in
        ()]
;;

module G = Graph.Imperative.Graph.Concrete (City)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, city2, _) ->
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
