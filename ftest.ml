open Graph
open Ff

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in
	let fg=Ff.init (Graph.map graph int_of_string)
	in
	let path_i=Ff.find_path fg "4" "1"
	in
	match path_i with
		|Some(c) -> 
			(* Rewrite the graph that has been read. *)
			let () = Ff.export_path outfile fg "4" "1" c in
			()
		|None -> let ()=Printf.printf "Failed %!" in ()
 
