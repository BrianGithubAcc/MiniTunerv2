(* I think mutablity introduces garbage collection?*)
type expr_data = {
  mutable delta : string option;
  mutable higher : string option;
  mutable epsilon : string option;
  mutable max_delta : string option;
  mutable max_higher : string option;
  mutable max_epsilon : string option;
}
