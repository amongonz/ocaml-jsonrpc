(** JSON-RPC codecs and handlers

    This module implements Jsont codecs for the JSON-RPC 2.0 specification,
    extended with peer-to-peer support, as well as a request handler
    abstraction. *)

(** {1 Basic types} *)

type structured_json = private Jsont.json
(** Represents a JSON object or array. *)

val structured_json : Jsont.json -> structured_json
(** [structure json] is [json] ensuring it's an structured value. *)

val structured_json_jsont : structured_json Jsont.t

type id_json = private Jsont.json
(** Represents a request identifier. This ID should be considered opaque and
    forwarded from request to response. *)

val id_json : Jsont.json -> id_json
(** [id_json json] is [json] ensuring it's a valid request identifier. *)

val compare_id_json : id_json -> id_json -> int
(** [compare_id_json id1 id2] is a total order on request IDs. *)

val id_json_jsont : id_json Jsont.t

(** {1 Errors} *)

type error = {
  code : int64;
  message : string;
  data : Jsont.json option;
}
(** Represents a response error.

    - [code] must be an integer between -2^53 and 2^53. The full range of valid
      error codes is formally unspecified, so it is assumed to be an integer
      representable under IEEE 754 binary64. *)

(** {2 Predefined errors}

    According to the specification:

    - When the server receives invalid JSON, a {i Parse error} response should
      be sent back to the client.
    - When a message doesn't match the request schema, an {i Invalid Request}
      response should be sent back to the client.

    These error responses are not mandatory, but if sent, the response ID must
    be set to null when the request ID is unknown. In practice, it will always
    be set to null because decoding the ID out of malformed messages is
    unworkable.

    Before deciding how to send or handle these two errors, consider the
    following caveats:

    - It can be hard for the server to distinguish whether a Jsont decode error
      represents {i Parse error} or {i Invalid Request}.
    - Error responses with null IDs can't be attributed to any particular
      request, and servers may not send these error responses at all. Thus,
      clients must consider all outstanding requests as possibly hanging at all
      times, and specially after such an error response.
    - Additionally, in peer-to-peer contexts, the receiver might not distingish
      whether a malformed message was intented to encode a request or a
      response, so these errors may get sent as counter-responses.
    - These interactions get even fuzzier when batches are involved.

    Thus, consider recovering from a malformed message or a
    {i Parse error}/{i Invalid Request} response by simply resetting the
    underlying session, rather than expecting both endpoints to handle the edge
    cases correctly. *)

val parse_error : error
(** [parser_error] is a response error with code [-32700] and message
    ["Parse error"]. *)

val invalid_request : error
(** [invalid_request] is a response error with code [-32600] and message
    ["Invalid Request"]. *)

val method_not_found : error
(** [method_not_found] is a response error with code [-32601] and message
    ["Method not found"]. *)

val invalid_params : error
(** [invalid_params] is a response error with code [-32602] and message
    ["Invalid params"]. *)

val internal_error : error
(** [internal_error] is a response error with code [-32603] and message
    ["Internal error"]. *)

val error_jsont : error Jsont.t

(** {1 Messages} *)

type _ message' =
  | Request : {
      method' : string;
      params : structured_json option;
      id : id_json option;
    }
      -> [> `Request ] message'
  | Response : {
      value : (Jsont.json, error) result;
      id : id_json;
    }
      -> [> `Response ] message'

type message = [ `Request | `Response ] message'
(** Represents a message, either a request or a response. *)

type request = [ `Request ] message'
(** Represents a request message. *)

type response = [ `Response ] message'
(** Represents a response message. *)

val message_jsont : message Jsont.t

(** {1 Batches} *)

type batch = message list
(** Represents a batch of messages. *)

val batch_jsont : batch Jsont.t

(** {1 JSON-RPC codec} *)

val message_or_batch_jsont : batch Jsont.t
(** [message_or_batch_jsont] is the main JSON-RPC codec, which encodes and
    decodes both bare messages and message batches. Bare messages are
    represented as single-message batches. *)

(** {1 Request handlers} *)

type handler = request -> (unit -> Jsont.json, error) result
(** Represents a request handler, which takes a parameter and returns either a
    JSON thunk or an error. *)

val method' :
  string ->
  ?no_params:'params ->
  ?by_pos:'params Jsont.t ->
  ?by_name:'params Jsont.t ->
  'result Jsont.t ->
  ('params -> ('result, error) result) ->
  handler
(** [method' name ?no_params ?by_pos ?by_name result_jsont handler] is a handler
    for the method [name], which decodes the parameters from the request, calls
    [handler] and encodes the result with [result_jsont].

    - [?no_params] is the value to use when the request parameters are missing.
    - [?by_pos] is the JSON array decoder to use when the request binds
      parameters by position.
    - [?by_name] is the JSON object decoder to use when the request binds
      parameters by name.

    Raises [Invalid_argument] if none of [?no_params], [?by_pos] or [?by_name]
    are set.

    The handler returns [Error method_not_found] if the request's method name is
    not equal to [name]. *)

val choose : handler list -> handler
(** [choose handlers] is a routing handler. It attempts each inner handler in
    sequence until one returns anything other than a {i Method not found} error,
    identified by the code [-32601].

    The handler returns [Error method_not_found] if every inner handler returns
    a {i Method not found} error. *)
