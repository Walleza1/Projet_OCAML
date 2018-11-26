open Gfile
open Graph

type ford_graph 
type ford_path
type ford_ecart_graph


(**************  CONSTRUCTORS  **************)
(** Create a ford_graph from an int graph **)
val init: (int * int) graph -> ford_graph 

val create_ecart_graph: ford_graph -> ford_ecart_graph
(************** FORD-FULKERSON RUN **************)

(** Runs Ford-Fulkerson algorithm with a ford graph **)
val run_ecart : ford_graph -> id -> id -> int

(** Exports functions **)
val toString: ford_graph -> string graph

val find_path_ecart: ford_ecart_graph -> id -> id -> ford_path option

val export: path -> ford_graph -> id ->id ->unit

val export_path : path -> ford_graph -> id -> id -> ford_path -> unit

val run_and_print : path-> ford_graph -> id -> id -> int
