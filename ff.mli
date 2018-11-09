open Graph

type ford_graph = (int * int) graph

(**************  CONSTRUCTORS  **************)
(* Create a ford_graph from a string graph *)
val init: int graph -> ford_graph 

val run : ford_graph -> int

val toString: ford_graph -> string graph
