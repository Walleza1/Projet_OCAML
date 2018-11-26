open Graph
open Ff

let () =

  if Array.length Sys.argv <> 4 then
    begin
      Printf.printf "\nUsage: infile origin dest \n\n%!" ;
      exit 0
    end ;

  let infile = Sys.argv.(1) and
  origin = Sys.argv.(2) and
  dest = Sys.argv.(3)
  in
  (* Open file *)
  let graph = Gfile.from_file infile in
  let double_int = (fun (x,y) -> (int_of_string x, int_of_string y)) in
	let fg=Ff.init (Graph.map graph double_int)
	in
	let result = run fg origin dest
	in
	Printf.printf "%s \n %!" (string_of_int result)
 
