open Graph
open Ff

(** Graph1 : non realistic graph, only testing validity of basic functions*)
let graph1= Ff.from_file_ford_graph "Template_Graphs/graph1" 
(** Graph2 : realistic example with a few students*)
let graph2= Ff.from_file_ford_graph "Template_Graphs/graph2" 
(** Graph3 : realistic example with a realistic number of students (approx 40)*)
let graph3= Ff.from_file_ford_graph "Template_Graphs/graph3" 
(** Graph4 : performance based graph, huge number of students*)
let graph4= Ff.from_file_ford_graph "Template_Graphs/graph4" 

let test_run (gr:ford_graph) gr_number origin dest expected=
	let res=Ff.run_ecart gr origin dest
	in
	if res=expected then Printf.printf "Test OK Graph%d ori %s dest %s ! \n%!" gr_number origin dest else Printf.printf "Test failed graph %d : ori %s - dest %s - res %d - expected %d\n %!"  gr_number origin dest res expected

let main ()=

	test_run graph1 1 "Didier" "tennis" 1;
	test_run graph1 1 "Didier" "golf" 0;
	test_run graph1 1 "Didier" "basket" 0;

	test_run graph1 1 "Jérôme" "tennis" 0;
	test_run graph1 1 "Jérôme" "golf" 1;
	test_run graph1 1 "Jérôme" "basket" 0;

	test_run graph1 1 "Vincent" "tennis" 1;
	test_run graph1 1 "Vincent" "golf" 0;
	test_run graph1 1 "Vincent" "basket" 0

let ()= main ()
