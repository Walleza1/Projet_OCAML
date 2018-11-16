open Graph
open Ff

let graph1=init (Graph.map (Gfile.from_file "Template_Graphs/graph1") int_of_string)
let graph2=init (Graph.map (Gfile.from_file "Template_Graphs/graph2") int_of_string)
let graph3=init (Graph.map (Gfile.from_file "Template_Graphs/graph3") int_of_string)
let graph4=init (Graph.map (Gfile.from_file "Template_Graphs/graph4") int_of_string)

let ()=
	let res1=Ff.run graph1 "0" "3" 
	in
	let res2=Ff.run graph2 "0" "5" 
	in
	let res3=Ff.run graph3 "0" "3" 
	in
	let res4=Ff.run graph4 "0" "5" 
	in
	let ()=
		Printf.printf "Graph 1\n Résultat attentu 6 | obtenu :  %d %!\n" res1;
		Printf.printf "Graph 2\n Résultat attentu 7 | obtenu :  %d %!\n" res2;
		Printf.printf "Graph 3\n Résultat attentu 25 | obtenu :  %d %!\n" res3;
		Printf.printf "Graph 4\n Résultat attentu 23 | obtenu :  %d %!\n" res4;
	in
	()
