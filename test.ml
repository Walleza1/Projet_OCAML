open Graph
open Ff

let graph1= Ff.from_file_ford_graph "Template_Graphs/graph1" 
let graph2= Ff.from_file_ford_graph "Template_Graphs/graph2" 
let graph3= Ff.from_file_ford_graph "Template_Graphs/graph3" 
let graph4= Ff.from_file_ford_graph "Template_Graphs/graph4" 

let test_run (gr:ford_graph) gr_number origin dest expected=
	let res=Ff.run_ecart gr origin dest
	in
	if res=expected then Printf.printf "Test OK Graph%d ori %s dest %s ! \n%!" gr_number origin dest else Printf.printf "Test failed graph %d : ori %s - dest %s - res %d - expected %d\n %!"  gr_number origin dest res expected

let main ()=
	test_run graph1 1 "1" "1" 0;
	test_run graph1 1 "1" "2" 0

let ()= main ()
