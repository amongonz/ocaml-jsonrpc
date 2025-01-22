type structured = Jsont.json

let structured_jsont =
  let enc = function
    | Jsont.Object _ | Array _ -> Jsont.json
    | _ -> invalid_arg "expected object or array"
  in
  Jsont.any ~kind:"object or array" ~dec_array:Jsont.json ~dec_object:Jsont.json
    ~enc ()

type id = Jsont.json

let compare_id = Jsont.Json.compare

let id_jsont =
  let enc = function
    | Jsont.Null _ | String _ | Number _ -> Jsont.json
    | _ -> invalid_arg "not a valid id value"
  in
  Jsont.any ~kind:"JSON-RPC request ID" ~dec_null:Jsont.json_null
    ~dec_string:Jsont.json_string ~dec_number:Jsont.json_number ~enc ()

type error = {
  code : int64;
  message : string;
  data : Jsont.json option;
}

let parse_error = { code = -32700L; message = "Parse error"; data = None }

let invalid_request =
  { code = -32600L; message = "Invalid Request"; data = None }

let method_not_found =
  { code = -32601L; message = "Method not found"; data = None }

let invalid_params = { code = -32602L; message = "Invalid params"; data = None }
let internal_error = { code = -32603L; message = "Internal error"; data = None }

let error_jsont =
  let open Jsont.Object in
  map (fun code message data -> { code; message; data })
  |> mem "code" Jsont.int64 ~enc:(fun e -> e.code)
  |> mem "message" Jsont.string ~enc:(fun e -> e.message)
  |> opt_mem "data" Jsont.json ~enc:(fun e -> e.data)
  |> finish

type _ message' =
  | Request : {
      method' : string;
      params : structured option;
      id : id option;
    }
      -> [> `Request ] message'
  | Response : {
      value : (Jsont.json, error) result;
      id : id;
    }
      -> [> `Response ] message'

type message = [ `Request | `Response ] message'
type request = [ `Request ] message'
type response = [ `Response ] message'

let message_jsont =
  let open Jsont.Object in
  map ~kind:"JSON-RPC message"
    (fun `V2 method' params result error id : message ->
      match (method', result, error, id) with
      | Some method', None, None, _ -> Request { method'; params; id }
      | None, Some result, None, Some id -> Response { value = Ok result; id }
      | None, None, Some error, Some id -> Response { value = Error error; id }
      | _ ->
          Jsont.Error.msg Jsont.Meta.none "Ambiguous JSON-RPC message fields.")
  |> mem "jsonrpc" (Jsont.enum [ ("2.0", `V2) ]) ~enc:(Fun.const `V2)
  |> opt_mem "method" Jsont.string ~enc:(function
       | Request r -> Some r.method'
       | Response _ -> None)
  |> opt_mem "params" structured_jsont ~enc:(function
       | Request r -> r.params
       | Response _ -> None)
  |> opt_mem "result" Jsont.json ~enc:(function
       | Response { value = Ok value; _ } -> Some value
       | Request _ | Response { value = Error _; _ } -> None)
  |> opt_mem "error" error_jsont ~enc:(function
       | Response { value = Error error; _ } -> Some error
       | Request _ | Response { value = Ok _; _ } -> None)
  |> opt_mem "id" id_jsont ~enc:(function
       | Request r -> r.id
       | Response r -> Some r.id)
  |> finish

type batch = message list

let batch_jsont = Jsont.list ~kind:"JSON-RPC batch" message_jsont

let message_or_batch_jsont =
  let single = Jsont.map message_jsont ~dec:(fun msg -> [ msg ]) ~enc:List.hd in

  Jsont.any () ~dec_array:batch_jsont ~dec_object:single ~enc:(function
    | [ _ ] -> single
    | _ -> batch_jsont)

type handler = request -> (unit -> Jsont.json, error) result

let method' name ?no_params ?by_pos ?by_name result_jsont handler =
  (match (no_params, by_pos, by_name) with
  | None, None, None ->
      invalid_arg "Expected at least one of ?no_params, ?by_pos or ?by_name."
  | _ -> ());

  let no_params_result = Option.to_result no_params ~none:invalid_params in
  let params_jsont = Jsont.any ?dec_array:by_pos ?dec_object:by_name () in

  fun (Request r : request) ->
    if not (String.equal name r.method') then Error method_not_found
    else
      let ( let* ) = Result.bind in

      let* params =
        match r.params with
        | None -> no_params_result
        | Some json -> (
            match Jsont.Json.decode params_jsont json with
            | Ok _ as result -> result
            | Error msg ->
                Error { invalid_params with message = "Invalid params: " ^ msg }
            )
      in

      let* result = handler params in

      let json_thunk () =
        match Jsont.Json.encode result_jsont result with
        | Ok result -> result
        | Error msg -> invalid_arg msg
      in

      Ok json_thunk

let rec choose handlers (req : request) =
  match handlers with
  | [] -> Error method_not_found
  | handler :: handlers -> (
      match handler req with
      | Error { code = -32601L; _ } ->
          (* Method not found *)
          choose handlers req
      | value -> value)
