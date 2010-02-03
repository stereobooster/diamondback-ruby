type
  ('n,'e) vertex = {
    mutable vertex_label : 'n;
    mutable succ_edges : (('n,'e) edge) list;
    (*mutable pred_edges : (('n,'e) edge) list*)
  }
  and ('n,'e) edge = {
    mutable edge_label : 'e;
    mutable dest : ('n,'e) vertex;
    (*mutable source : ('n,'e) vertex*)
  }

let create_e v1 label v2 =
  let new_edge = { edge_label = label; (*source = v1;*) dest = v2} in
  v1.succ_edges <- new_edge::(v1.succ_edges);
  (*v2.pred_edges <- new_edge::(v2.pred_edges);*)
  new_edge

(*let remove_edge_e edge =
  let v1 = edge.source in
  let v2 = edge.dest in
  let f e = e!=edge in
  v1.succ_edges <- List.filter f v1.succ_edges;
  v2.pred_edges <- List.filter f v2.pred_edges*)

let create_v label =
  { vertex_label = label; succ_edges = []; (*pred_edges = []*) }

