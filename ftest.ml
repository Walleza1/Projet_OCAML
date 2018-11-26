open Graph
open Ff
let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: infile origin dest outfile \n\n%!" ;
      exit 0
    end ;

  let infile = Sys.argv.(1) and
  origin = Sys.argv.(2) and
  dest = Sys.argv.(3) and 
  outfile =Sys.argv.(4)
  in
  (* Open file *)
  let res=Ff.from_file_ford_graph infile
  in
    Ff.export_ford outfile res "s" "p";
  ()
 
