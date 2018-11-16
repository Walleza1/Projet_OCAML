open Graph

type ford_graph 

(**************  CONSTRUCTORS  **************)
(* Create a ford_graph from an int graph *)
val init: int graph -> ford_graph 

(************** FORD-FULKERSON RUN **************)

(* Runs Ford-Fulkerson algorithm with a ford graph *)
(*val run : ford_graph -> int *) 

val toString: ford_graph -> string graph
