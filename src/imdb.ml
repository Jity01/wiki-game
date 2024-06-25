open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and
   return a list of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  let credits = parse contents $$ "a[class]" in
  let credits = to_list credits in
  let get_class a = R.attribute "class" a in
  let is_link_credit a =
    String.equal (get_class a) "ipc-primary-image-list-card__title"
  in
  let credits = List.filter credits ~f:is_link_credit in
  List.map credits ~f:(fun strng ->
    texts strng |> String.concat ~sep:"" |> String.strip)
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
