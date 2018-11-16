open Graph
open Ff

let graph1=init (Graph.map (Gfile.from_file "Template_Graphs/graph1") int_of_string)
let graph2=init (Graph.map (Gfile.from_file "Template_Graphs/graph2") int_of_string)
let graph3=init (Graph.map (Gfile.from_file "Template_Graphs/graph3") int_of_string)
let graph4=init (Graph.map (Gfile.from_file "Template_Graphs/graph4") int_of_string)


let ()=
	Gfile.export "Template_Graphs/graph1.dot" (Ff.toString graph1) "";
	Gfile.export "Template_Graphs/graph2.dot" (Ff.toString graph2) "";
	Gfile.export "Template_Graphs/graph3.dot" (Ff.toString graph3) "";
	Gfile.export "Template_Graphs/graph4.dot" (Ff.toString graph4) "";
	let ret1=Sys.command("dot -Tpng Template_Graphs/graph1.dot > Template_Graphs/graph1.png && rm Template_Graphs/graph1.dot") in
	let ret2 =Sys.command("dot -Tpng Template_Graphs/graph2.dot > Template_Graphs/graph2.png && rm Template_Graphs/graph2.dot") in 
	let ret3=Sys.command("dot -Tpng Template_Graphs/graph3.dot > Template_Graphs/graph3.png && rm Template_Graphs/graph3.dot") in 
	let ret4=Sys.command("dot -Tpng Template_Graphs/graph4.dot > Template_Graphs/graph4.png && rm Template_Graphs/graph4.dot")
	in
		Printf.printf "Retour 1 : %d \nRetour 2: %d \nRetour 3: %d \nRetour 4 : %d\n" ret1 ret2 ret3 ret4;
()