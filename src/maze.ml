open! Core

module Position = struct
  type t =
    { row : int
    ; col : int
    }
  [@@deriving compare, sexp, hash, equal]

  (* type position_type = WALL | SPACE | START | END *)
  let create_position ~row_num ~col_num = { row = row_num; col = col_num }

  let add_position pos1 pos2 =
    { row = pos1.row + pos2.row; col = pos1.col + pos2.col }
  ;;

  let is_pos_out_of_bounds board ~pos =
    pos.row >= List.length board
    || pos.row < 0
    || pos.col >= List.length (List.hd_exn board)
    || pos.col < 0
  ;;
  (* let get_position_type character = match character with | '.' -> SPACE |
     '#' -> WALL | 'S' -> START | _ -> END *)
end

let load_file_into_2d_board input_file =
  In_channel.read_lines (File_path.to_string input_file)
  |> List.map ~f:(fun line -> String.to_list line)
;;

let is_pos_type_passable ~(pos : Position.t) board =
  (* TODO: use nth *)
  let f i row =
    List.filter_mapi row ~f:(fun j value ->
      match Int.( = ) i pos.row && Int.( = ) j pos.col with
      | true -> Some value
      | false -> None)
  in
  let results = List.mapi board ~f in
  let wrapped_char =
    List.find_exn results ~f:(fun ll -> List.length ll > 0)
  in
  let char = List.hd_exn wrapped_char in
  Char.equal char '.' || Char.equal char 'E'
;;

let find_neighbors board ~pos =
  let dirs =
    [ Position.create_position ~row_num:(-1) ~col_num:0
    ; Position.create_position ~row_num:1 ~col_num:0
    ; Position.create_position ~row_num:0 ~col_num:(-1)
    ; Position.create_position ~row_num:0 ~col_num:1
    ]
  in
  let possible_neighbors =
    List.map dirs ~f:(fun diff -> Position.add_position pos diff)
  in
  let is_neighbor_viable nbr =
    (not (Position.is_pos_out_of_bounds board ~pos:nbr))
    && is_pos_type_passable ~pos:nbr board
  in
  let viable_neighbors =
    List.filter possible_neighbors ~f:is_neighbor_viable
  in
  viable_neighbors
;;

let check_win_condition ~pos ~end_pos = Position.equal pos end_pos

let get_pos_of ~opt board =
  let f i row =
    List.filter_mapi row ~f:(fun j value ->
      match Char.equal value opt with
      | true -> Some (Position.create_position ~row_num:i ~col_num:j)
      | false -> None)
  in
  let results = List.mapi board ~f in
  let wrapped_pos =
    List.find_exn results ~f:(fun ll -> List.length ll > 0)
  in
  List.hd_exn wrapped_pos
;;

let rec helper board ~pos ~end_pos ~visited =
  let is_maze_solved = check_win_condition ~end_pos ~pos in
  match is_maze_solved with
  | true -> [ pos ]
  | false ->
    let neighbors = find_neighbors board ~pos in
    let unseen_neighbors =
      List.filter neighbors ~f:(fun neighbor ->
        not (Hash_set.mem visited neighbor))
    in
    List.iter unseen_neighbors ~f:(fun neighbor ->
      Hash_set.add visited neighbor);
    let f neighbor = helper board ~pos:neighbor ~end_pos ~visited in
    let results = List.map unseen_neighbors ~f in
    (* TODO: use find_map *)
    let soln =
      List.find results ~f:(fun ll ->
        match ll with [] -> false | _ -> true)
    in
    (match soln with None -> [] | Some ll -> List.append [ pos ] ll)
;;

let solver (board : char list list) =
  let visited = Hash_set.create (module Position) in
  let start_pos = get_pos_of ~opt:'S' board in
  let end_pos = get_pos_of ~opt:'E' board in
  let soln = helper board ~pos:start_pos ~end_pos ~visited in
  print_endline "SOLUTION :D";
  List.iter soln ~f:(fun pos -> printf "row:%i col:%i\n" pos.row pos.col);
  ()
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let board = load_file_into_2d_board input_file in
        solver board]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
