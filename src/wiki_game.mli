open! Core

(** Commands for interacting with Wikipedia articles.

    A small dataset for local testing can be found in the ../resources/wiki directory. *)

val command : Command.t
val get_linked_articles : string -> string list
