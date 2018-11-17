open Graph
open Gfile

type ford_graph = (int*int) graph

type ford_path = id list

(** Function init **)
let init graph = map graph (fun x -> (0,x))
;;


(** Boolean to test if this arc is available *)
let is_arc_available (id,(actu,cap)) =
	actu < cap
;;


(** Returns actual flow value of an arc *)
let value_of_arc (id,(actu,cap)) = actu
;;


(**Take a ford_graph and finda path *)
let find_path (graph:ford_graph) origin destination=
	let rec loop start acc visited =
		if start = destination then
			Some (List.rev acc)
		else 
			let arcs=List.filter is_arc_available (out_arcs graph start)
			in
			let rec try_path list_arc=match list_arc with
				| [] -> None
				|(id,(actu,cap))::rest-> 
					if List.mem id visited then
						None
					else
						let result=loop id (id::acc) (id::visited)
						in 
						begin
							match result with
								| None -> try_path rest
								| _ -> result
						end
			in
			try_path arcs
	in
		loop origin [origin] []	
;;


(**Get the maximum flow value available (total capacity - actual) in a path *)
let get_path_max_value (graph:ford_graph) (path:ford_path)=
	let rec loop pathi= match pathi with
		| a::b::rest -> 
			let arc=find_arc graph a b
			in
				begin
				match arc with
					|Some(actu,cap)-> (cap-actu)::(loop (b::rest))
					|None -> failwith "Error, path is not valid in graph"
				end
		| _ -> []
	in 
	let path_values=loop path
	in
		try 
			List.fold_left (fun x y -> Pervasives.min x y) (List.hd path_values) path_values
		with 
			| hd->0
;;


(**Get the maximum flow value available (total capacity - actual) in a path *)
let update_ford_graph (graph:ford_graph) (path:ford_path)=
	(* value >=0 *)
	let value=get_path_max_value graph path
	in
	let rec loop pathi acc = match pathi with
		|a::b::rest -> 
			let arc_actu=find_arc graph a b
			in
			begin
			match arc_actu with
				|Some(actu,cap)->
					let nvalue=actu+value 
					in loop (b::rest) (add_arc acc a b (nvalue,cap))
				|None -> failwith "Error, path is not valid"
			end
		| _ -> acc
	in
	loop path graph
;;

(**Sums the flow value of all outgoing arcs of node id *)
let sum_outarcs_value graph id = 
	let arcs = out_arcs graph id 
	in 
		List.fold_left (+) 0 (List.map value_of_arc arcs)
;;

(**Takes a ford graph, an origin and destination node id, and runs ford-fulkerson on it, returning the max flow value *)
let run (graph:ford_graph) origin dest =
	let rec loop gr = 
	(* Tu avais utilisé graph du coup il partait toujours du graph initial donc toujours un chemin donc stack overflow *)
	let path=find_path gr origin dest in
	match (path) with
		|Some(chemin) -> 
			begin
			(* Ajout d'un match case quand le chemin n'est composé que d'un élément ou est vide. Si on demande origin = dest il renvoit le chemine [origin] *)
				match chemin with
					|a::b::rest ->loop (update_ford_graph gr chemin)
					| _ -> sum_outarcs_value gr origin
			end
		|None -> sum_outarcs_value gr origin
	in
	loop graph
;;



(* Create a string graph *)
let toString (graph:ford_graph) = map graph (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))	

let export path (graph:ford_graph) origin destination=
 let (o,d)=((node_exists graph origin),(node_exists graph destination))
	in match (o,d) with
	|(true,true)-> 
		let s=origin^"[fillcolor=green,style=filled];\n"^destination^"[fillcolor=orange,style=filled]\n"
			in
		Gfile.export path (toString graph) s
	|(_,_) -> raise (Graph_error("Nodes aren't present in graph"))
	 

let export_path file_path (graph:ford_graph) origin destination (path:ford_path)=
	let (o,d)=((node_exists graph origin),(node_exists graph destination))
	in match (o,d) with
		|(true,true)-> 
			let rec loop pathi acc= match pathi with
				|a::b::rest -> loop (b::rest) (acc^a^"->"^b^" [color=red,fontcolor=red];\n")
				| _ -> acc
			in
			let edge=loop path ""
			in
			let s=origin^"[fillcolor=green,style=filled];\n"^destination^"[fillcolor=orange,style=filled]\n"
			in
			Gfile.export file_path (toString graph) (s^edge)
		|(_,_) -> raise (Graph_error("Nodes aren't present in graph"))

