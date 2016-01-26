module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

type creds = {
  consumer_key: string;
  consumer_secret: string;
  access_token: string;
  access_token_secret: string;
} [@@deriving yojson]

let nonce len =
  let `Hex s =
    Nocrypto.Rng.generate len |>
    Hex.of_cstruct
  in s

let pct_encode = Uri.pct_encode ~component:`Authority

(* https://dev.twitter.com/docs/auth/authorizing-request *)
(* https://dev.twitter.com/docs/auth/creating-signature *)
let signed_call ?(headers=C.Header.init ()) ?(body=[]) ?chunked ~creds meth uri =
  let base_uri = Uri.to_string (Uri.with_query uri []) in
  let query_params = Uri.query uri in
  let fixed_params =
    ["oauth_consumer_key", [creds.consumer_key];
     "oauth_nonce", [nonce 16];
     "oauth_signature_method", ["HMAC-SHA1"];
     "oauth_timestamp", [string_of_int (int_of_float (Unix.time ()))];
     "oauth_token", [creds.access_token];
     "oauth_version", ["1.0"]
    ] in
  let all_params = body @ query_params @ fixed_params in
  let all_params = List.map (fun (k, vs) -> pct_encode k, List.map (pct_encode) vs) all_params in
  let all_params = List.fast_sort compare all_params in
  let all_params = List.map (fun (k, vs) -> k, String.concat "," vs) all_params in
  let all_params = List.map (fun (k, v) -> k ^ "=" ^ v) all_params in
  let params_string = String.concat "&" all_params in
  let base_string = [C.Code.string_of_method meth; pct_encode base_uri; pct_encode params_string] in
  let base_string = String.concat "&" base_string |> Cstruct.of_string in
  let signing_key = pct_encode creds.consumer_secret ^ "&" ^ pct_encode creds.access_token_secret |> Cstruct.of_string in
  let signature = Nocrypto.(Hash.SHA1.hmac ~key:signing_key base_string |> Base64.encode) |> Cstruct.to_string in
  let authorization_string = ("oauth_signature", [signature])::fixed_params in
  let authorization_string = List.map (fun (k, vs) ->
      pct_encode k,
      "\"" ^ pct_encode (List.hd vs) ^ "\"") authorization_string in
  let authorization_string = List.fast_sort compare (List.map (fun (k, v) -> k ^ "=" ^ v) authorization_string) in
  let authorization_string = ("OAuth " ^ String.concat ", " authorization_string) in
  let body_string = List.map (fun (k, vs) -> k, String.concat "," vs) body in
  let body_string = List.map (fun (k, v) -> k ^ "=" ^ v) body_string in
  let body_string = String.concat "&" body_string in
  let body' = CB.of_string body_string in
  let headers = List.fold_left (fun a (k,v) -> C.Header.add a k v) headers
      ([ "User-Agent", "athanor/0.1";
         "Authorization", authorization_string;
       ] @
       (if meth = `POST then [
           "Content-Length", string_of_int (String.length body_string);
           "Content-Type", "application/x-www-form-urlencoded";
         ] else []
       )
      )
  in
  match meth with
  | `GET ->
      let uri = Uri.with_query uri body in
      CU.Client.get ~headers uri
  | `POST ->
      CU.Client.call ~headers ~body:body' ?chunked `POST uri
  | #C.Code.meth ->
      assert false

let sanitize_date date_string =
  let int_of_month = function
    | "Jan" -> 0 | "Feb" -> 1 | "Mar" -> 2 | "Apr" -> 3 | "May" -> 4
    | "Jun" -> 5 | "Jul" -> 6 | "Aug" -> 7 | "Sep" -> 8 | "Oct" -> 9
    | "Nov" -> 10 | "Dec" -> 11
    | s -> raise (Invalid_argument s) in
  let int_of_day = function
    | "Sun" -> 0 | "Mon" -> 1 | "Tue" -> 2 | "Wed" -> 3 | "Thu" -> 4
    | "Fri" -> 5 | "Sat" -> 6
    | s -> raise (Invalid_argument s) in
  let open Re_str in
  let rex = regexp " +" in
  let rex_colon = regexp ":" in
  match split rex date_string with
  | [day; month; mday; hour; zone; year] ->
    (match split rex_colon hour with
     | [hh; mm; ss] ->
       let open Unix in
       mktime { tm_sec = int_of_string ss; tm_min = int_of_string mm;
                tm_hour = int_of_string hh; tm_mday = int_of_string mday;
                tm_mon = int_of_month month; tm_year = (int_of_string year) - 1900;
                tm_wday = int_of_day day; tm_yday = 0; tm_isdst = false }
     | _ -> raise (Invalid_argument "not a twitter date")
    )
  | _ -> raise (Invalid_argument "not a twitter date")
