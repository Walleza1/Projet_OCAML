open Graph
open Ff

(** Graph1 : non realistic graph, only testing validity of basic functions*)
let graph1= Ff.from_file_ford_graph "Test_Graphs/graph1" 
(** Graph2 : realistic example with a few students*)
let graph2= Ff.from_file_ford_graph "Test_Graphs/graph2" 
(** Graph3 : realistic example with a realistic number of students (approx 40)*)
let graph3= Ff.from_file_ford_graph "Test_Graphs/graph3" 

let test_run (gr:ford_graph) gr_number outfile expected=
	let res = Ff.run_max_flow_min_cost outfile gr
	in
	if res=expected then Printf.printf "Graph %d : Test OK affectation rate - %d/%d \n%!" gr_number res expected
	else Printf.printf "Graph %d : Test failed affectation rate - %d/%d - expected %d %d \n%!"  gr_number res expected expected expected

let main ()=
	test_run graph1 1 "Test_Graphs/graph1.dot" 3;
	let ret1 = Sys.command("dot -Tpng Test_Graphs/graph1.dot > Test_Graphs/graph1.png") in
	if ret1 <> 0 then Printf.printf("error converting to png\n");

	test_run graph2 2 "Test_Graphs/graph2.dot" 4;
	let ret2 = Sys.command("dot -Tpng Test_Graphs/graph2.dot > Test_Graphs/graph2.png") in
	if ret2 <> 0 then Printf.printf("error converting to png\n");

	test_run graph3 3 "Test_Graphs/graph3.dot" 40;
	let ret3 = Sys.command("dot -Tpng Test_Graphs/graph3.dot > Test_Graphs/graph3.png") in
	if ret3 <> 0 then Printf.printf("error converting to png\n")

let ()= main ()