open! Core

let command =
  Command.basic
    ~summary:
      "Demo showing usage of [File_fetcher] to fetch a resource, either \
       locally or remotely, and print it to stdout"
    [%map_open.Command
      let how_to_fetch, resource = File_fetcher.param in
      fun () -> print_endline (File_fetcher.fetch_exn how_to_fetch ~resource)]
;;
