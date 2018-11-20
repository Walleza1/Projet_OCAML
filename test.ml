open Graph
open Ff

let graph1=init (Graph.map (Gfile.from_file "Template_Graphs/graph1") int_of_string)
let graph2=init (Graph.map (Gfile.from_file "Template_Graphs/graph2") int_of_string)
let graph3=init (Graph.map (Gfile.from_file "Template_Graphs/graph3") int_of_string)
let graph4=init (Graph.map (Gfile.from_file "Template_Graphs/graph4") int_of_string)

let test_run (gr:ford_graph) gr_number origin dest expected=
	let res=Ff.run_ecart gr origin dest
	in
	if res=expected then Printf.printf "Test OK Graph%d ori %s dest %s ! \n%!" gr_number origin dest else Printf.printf "Test failed graph %d : ori %s - dest %s - res %d - expected %d\n %!"  gr_number origin dest res expected

let main ()=
	test_run graph1 1 "0" "0" 0;
	test_run graph1 1 "0" "1" 2;
	test_run graph1 1 "0" "2" 6;
	test_run graph1 1 "0" "3" 6;
	test_run graph1 1 "1" "0" 0;
	test_run graph1 1 "1" "1" 0;
	test_run graph1 1 "1" "2" 3;
	test_run graph1 1 "1" "3" 4;
	test_run graph1 1 "2" "0" 0;
	test_run graph1 1 "2" "1" 0;
	test_run graph1 1 "2" "2" 0;
	test_run graph1 1 "2" "3" 5;
	test_run graph1 1 "3" "0" 0;
	test_run graph1 1 "3" "1" 0;
	test_run graph1 1 "3" "2" 0;
	test_run graph1 1 "3" "3" 0;
	test_run graph2 2 "0" "5" 7;
	test_run graph3 3 "0" "3" 25;
	test_run graph4 4 "0" "5" 23

let ()= main ()
