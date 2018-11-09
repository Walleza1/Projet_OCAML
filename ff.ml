open Graph

type ford_graph = (int*int) graph

type path = id list

(**************  CONSTRUCTORS  **************)
(** Function init **)
let init graph = map graph (fun x -> (0,x))

(** Boolean to test if this arc is available *)
let is_arc_available (id,(actu,cap)) =
	if actu < cap then true else false


(**Take a ford_graph and find a path *)
let find_path (graph:ford_graph) origin destination=
	let rec loop start acc visited=
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
		loop origin [] []	

let run (graph:ford_graph)=
	let chemin=find_path graph "4" "1"
	in
	match chemin with
		| Some(a) -> List.iter (fun x -> Printf.printf "%s - " x;Printf.printf "%!") a;4
		| None -> 2

(* Create a string graph *)
let toString graph = map graph (fun (a,b) -> (string_of_int a)^"/"^(string_of_int b))	

