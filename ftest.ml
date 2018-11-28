open Graph
open Ff
let () =

  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: infile outfile \n\n%!" ;
      exit 0
    end ;

  let infile = Sys.argv.(1) and
  outfile = Sys.argv.(2) 
  in
  (* Open file *)
  let gr =Ff.from_file_ford_graph infile
  in
  let res = Ff.run_max_flow_min_cost outfile gr in  
  ()
 
