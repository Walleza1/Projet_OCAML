open Graph

(** Actu, Cap, Cost*)
type ford_graph = (int*int*int) graph

type ford_ecart_graph = (int*bool*int) graph

type ford_path = id list


(** Reads a line with a node. 
if start with s then it's a student
if start if c then if a class -> c "tennis" 12 means 12 seats in tennis
*)

let read_node_ford (graph:ford_graph) line is_source =
  try if is_source then
    Scanf.sscanf line "s %s" (fun id -> let gr=add_node graph id in add_arc gr "s" id (0,1,1))
  else 
    Scanf.sscanf line "c %s %d" (fun id cap-> let gr=add_node graph id in add_arc gr id "p" (0,cap,1))
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ; failwith "Error node from_file"


(* Reads a line with an arc.
if it's starting with p then it's preference *)
let read_arc_ford (graph:ford_graph) line =
  try Scanf.sscanf line "p %s %d %s" (fun id1 cost id2 -> add_arc graph id1 id2 (0,1,cost))
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ; failwith "Error arc from_file"


let from_file_ford_graph path =
  let infile = open_in path in
  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
      (* Ignore empty lines *)
        if line = "" then graph
      (* The first character of a line determines its content : 
      s for student, c for class, p for preference
       * Lines not starting with v or e are ignored. *)
        else match line.[0] with
          | 's' -> read_node_ford graph line true
          | 'c' -> read_node_ford graph line false
          | 'p' -> read_arc_ford graph line
          | _ -> graph
      in                 
        loop graph2        
    with End_of_file -> graph
      in
      let gr= add_node (add_node (empty_graph) "s") "p"
      in
      let final_graph = loop gr in
       close_in infile ;
       final_graph



(** Function init **)
let init graph = map graph (fun (x,y) -> (0,x,y))
;;

(** Returns actual flow value of an arc *)
let value_of_arc (id,(actu,cap,_)) = actu;;

let to_fold ecart_g id l=
  let graph=
    try
      add_node ecart_g id
    with
      |Graph_error(e)->ecart_g
  in
  let rec loop acu li=match li with
    |(nid,(cap,max_cap,_))::rest-> 
      let gr=try
        add_node acu nid
      with
        |Graph_error(e)->acu
      in
      let available=max_cap-cap
      in
        begin
          match (cap,available) with
            |(0,0)-> loop gr rest
            |(cap,0)->
              let graph_res=
                try
                  add_arc gr nid id (cap,true,1)
                with
                  |Graph_error(e)->gr
              in
                loop graph_res rest
            |(0,available)->
              let graph_res=
                try
                  add_arc gr id nid (available,false,1)
                with
                  |Graph_error(e)->gr
              in
                loop graph_res rest
            |(cap,available)->
              let graph_arc_right=
                try
                  add_arc gr id nid (available,false,1)
                with
                  | Graph_error(e)->gr
              in
              let graph_res=
                try
                  add_arc graph_arc_right nid id (cap,true,1)
                with
                  |Graph_error(e)->graph_arc_right
              in
                loop graph_res rest
          end
    | [] -> acu
  in
  loop graph l
;;

let create_ecart_graph (graph:ford_graph)=Graph.v_fold (graph) to_fold (Graph.empty_graph)

(**Take a ford_ecart_graph and finda path *)
let find_path_ecart (graph:ford_ecart_graph) origin destination=
  let rec loop start (acc:id list) visited =
    if start = destination then
      Some (List.rev acc)
    else
      let arcs=out_arcs graph start
      in
      let rec try_path list_arc=match list_arc with
        | [] -> None
        |(id,(actu,cap,_))::rest-> 
          if List.mem id visited then
            try_path rest
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

(**Get the maximum flow value available (total capacity - actual) in a path *)
let get_path_max_value_ecart (graph:ford_ecart_graph) path=
  let rec loop pathi= match pathi with
    | a::b::rest -> 
      let arc=find_arc graph a b
      in
        begin
          match arc with
            |Some(value,bol,_)-> (value)::(loop (b::rest))
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
let update_ford_graph_ecart (graph:ford_graph) path=
  (* value >=0 *)
  let ecart_graph=(create_ecart_graph graph)
  in
  let value=get_path_max_value_ecart ecart_graph path
  in
  let rec loop pathi acc = match pathi with
    |a::b::rest -> 
      let arc_ecart=find_arc ecart_graph a b
      in
        begin
          match arc_ecart with
            |Some(value_arc,is_reverse,_)->
              if is_reverse then
                let arc_actu=find_arc graph b a
                in
                  begin
                    match arc_actu with
                      |Some(actu,cap,cost)->let nvalue=actu-value in loop (b::rest) (add_arc acc b a (nvalue,cap,cost))
                      |None -> Printf.printf "arc %s - %s " a b ;failwith "Error, arc reverse is not valid"
                  end
              else
                let arc_actu=find_arc graph a b
                in
                  begin
                    match arc_actu with
                      |Some(actu,cap,cost)->let nvalue=actu+value in loop (b::rest) (add_arc acc a b (nvalue,cap,cost))
                      |None-> failwith "Error arc is not valid"
                  end
            |None->failwith "Error arc not found in ford_ecart_graph"
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
let run_ecart (graph:ford_graph) origin dest =
  let rec loop gr = 
    let gr_ecart=create_ecart_graph gr
    in
    let path=find_path_ecart gr_ecart origin dest in
  match (path) with
  |Some(chemin) -> 
      begin
      (* Ajout d'un match case quand le chemin n'est composé que d'un élément ou est vide. Si on demande origin = dest il renvoit le chemine [origin] *)
        match chemin with
          |a::b::rest ->loop (update_ford_graph_ecart gr chemin)
          | _ -> sum_outarcs_value gr origin
      end
    |None -> sum_outarcs_value gr origin
  in
  loop graph
;;




(* Create a string graph *)
let toString (graph:ford_graph) = map graph (fun (a,b,c) -> (string_of_int a)^"/"^(string_of_int b)^" : "^(string_of_int c))  


  
let export_ford file_path (graph) origin destination=
  let s=origin^"[fillcolor=green,style=filled];\n"^destination^"[fillcolor=orange,style=filled]\n"
  in
  Gfile.export file_path (toString graph) s

 
