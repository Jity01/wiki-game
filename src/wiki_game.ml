open! Core

module Title = String
module Url = String

module Article = struct
  type t = { title: Title.t ; url: Url.t } [@@deriving compare, sexp, hash, equal]
end

module Network = struct
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp, hash, equal]
    end
    include Comparable.Make (T)
  end
end

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    (* let vertex_attributes _ = [] *)
    (* let vertex_name (v : Article.t) = String.append (String.append "title: " v.title) (String.append "url:" v.url) *)
    let vertex_name v = sprintf {|"%s"|} v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)


(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  let all_links = parse contents $$ "a[href]" in
  let all_links = to_list all_links in
  let get_href a = (R.attribute "href" a) in
  let is_wiki_link a = String.is_prefix (get_href a) ~prefix:"/wiki" in
  let wiki_links = List.filter all_links ~f:(fun a -> is_wiki_link a) in
  let get_name_space a = Wikipedia_namespace.namespace (get_href a) in
  let non_namespace_wiki_links = List.filter wiki_links ~f:(fun a -> match (get_name_space a) with | None -> true | _ -> false ) in
  let target_links = List.map non_namespace_wiki_links ~f:get_href in
  List.dedup_and_sort target_links ~compare:String.compare
;;

let%expect_test "get_linked_articles" =
  let contents = File_fetcher.fetch_exn (Local (File_path.of_string "../resources/wiki")) ~resource:"Carnivore" in
  let credit_links = get_linked_articles contents in
  List.iter credit_links ~f: (fun str -> print_endline str);
  [%expect {|
    /wiki/Animal
    /wiki/Caniformia
    /wiki/Feliformia |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let get_text_of_node node =
  let open Soup in
  texts node |> String.concat ~sep:"" |> String.strip
let get_contents ~how_to_fetch ?(is_remote = false) resource =
  print_endline resource;
  match is_remote && not (String.is_prefix resource ~prefix:"https") with
  | false -> File_fetcher.fetch_exn how_to_fetch ~resource
  | true -> File_fetcher.fetch_exn how_to_fetch ~resource:(String.append "https://en.wikipedia.org" resource)
let rec get_edges ~how_to_fetch visited ~url ~max_depth =
  match max_depth with
  | 0 -> []
  | _ ->
    let children_urls = get_linked_articles (get_contents ~how_to_fetch url) in
    let unseen_children_urls = List.filter children_urls ~f:(fun child_url -> not (Hash_set.mem visited child_url)) in
    match unseen_children_urls with
    | [] -> []
    | _ ->
      List.iter unseen_children_urls ~f:(fun child_url -> Hash_set.add visited child_url);
      let edges = List.map unseen_children_urls ~f:(fun child_url -> url, child_url) in
      let f child_url = get_edges visited ~how_to_fetch ~url:child_url ~max_depth:(max_depth - 1) in
      let rest_of_edges = List.concat_map unseen_children_urls ~f in
      List.append edges rest_of_edges
;;

let get_title url ~how_to_fetch=
  let open Soup in
  parse (get_contents ~how_to_fetch url) $ "title" |> get_text_of_node

let create_article_from_url url ~how_to_fetch : Article.t = {title=(get_title url ~how_to_fetch); url=url}

let get_graph ~how_to_fetch ~origin ~max_depth = 
  let visited = Hash_set.create (module Url) in
  Hash_set.add visited origin;
  let graph_of_urls = get_edges ~how_to_fetch ~url:origin visited ~max_depth in
  let graph_of_articles = List.map graph_of_urls ~f:(fun (parent_url, child_url) -> (create_article_from_url parent_url ~how_to_fetch, create_article_from_url child_url ~how_to_fetch)) in
  Network.Connection.Set.of_list graph_of_articles

let write_to_file network ~output_file = 
  let graph = G.create () in
  let str_without_spaces str = String.substr_replace_all str ~pattern:" " ~with_:" " in
  Set.iter network ~f:(fun ((parent_article, child_article) : Article.t * Article.t) -> G.add_edge graph (str_without_spaces parent_article.title) (str_without_spaces child_article.title));
  Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let network = get_graph ~how_to_fetch ~origin ~max_depth in
  write_to_file network ~output_file;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let check_win_condition url ~dest = String.equal url dest

let rec helper url ~dest ~visited ~how_to_fetch ~depth ~is_remote =
  let is_dest_found = check_win_condition url ~dest in
  match is_dest_found || Int.(=) depth 0 with
  | true -> Some [url]
  | false ->
    let inner_links = get_linked_articles (get_contents url ~how_to_fetch ~is_remote) in
    let unseen_inner_links = List.filter inner_links ~f:(fun lnk -> not (Hash_set.mem visited lnk)) in
    List.iter unseen_inner_links ~f:(fun lnk -> Hash_set.add visited lnk);
    let f lnk = helper lnk ~dest ~how_to_fetch ~depth:(depth - 1) ~visited ~is_remote in
    let results = List.map unseen_inner_links ~f in
    let find_list_with_urls (ll : string list option) = match ll with | None -> false | _ -> true in
    let soln = List.find results ~f:find_list_with_urls in 
    match soln with
    | None -> None
    | Some ll -> match ll with | None -> None | Some lst -> Some (List.append [url] lst)

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let is_remote = String.is_prefix origin ~prefix:"https" in
  let visited = Hash_set.create (module Url) in
  let soln = helper origin ~dest:destination ~visited ~how_to_fetch ~depth:max_depth ~is_remote in
  match soln with
  | None -> None
  | Some path ->
      let path_titles = List.map path ~f:(fun lnk -> get_title lnk ~how_to_fetch) in
      Some path_titles
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      (* fun () -> let contents = get_contents ~how_to_fetch "/wiki/Acoustic_Kitty" ~is_remote:true in print_endline contents;] *)
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
