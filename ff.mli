open Gfile
open Graph

type ford_graph 
type ford_path

(**************  CONSTRUCTORS  **************)
(** Create a ford_graph from an int graph **)
val init: int graph -> ford_graph 

(************** FORD-FULKERSON RUN **************)

(** Runs Ford-Fulkerson algorithm with a ford graph **)
val run : ford_graph -> id -> id -> int

(** Exports functions **)
val toString: ford_graph -> string graph

val find_path: ford_graph -> id -> id -> ford_path option

val export: path -> ford_graph -> id ->id ->unit

val export_path : path -> ford_graph -> id -> id -> ford_path -> unit

val run_and_print : path-> ford_graph -> id -> id -> int
