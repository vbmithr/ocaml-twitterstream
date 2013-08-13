open Cohttp
module C  = Cohttp_lwt_unix.Client
module CB = Cohttp_lwt_body

let (>>=) = Lwt.bind

type creds = {
  consumer_key: string;
  consumer_secret: string;
  access_token: string;
  access_token_secret: string;
}

let creds_of_json json =
  let extract_field name assoc = match List.assoc name assoc with
    | `String s -> s
    | _ -> raise (Invalid_argument "corrupted input") in
  match json with
  | `Assoc assoc -> {
      consumer_key = extract_field "consumer_key" assoc;
      consumer_secret = extract_field "consumer_secret" assoc;
      access_token = extract_field "access_token" assoc;
      access_token_secret = extract_field "access_token_secret" assoc
    }
  | _ -> raise (Invalid_argument "input is not a JSON object")

let nonce ?(len=16) rng =
  let open Cryptokit in
  transform_string (Hexa.encode ()) (Random.string rng len)

let pct_encode = Uri.pct_encode ~component:`Authority

(* https://dev.twitter.com/docs/auth/authorizing-request *)
(* https://dev.twitter.com/docs/auth/creating-signature *)
let signed_call ?(headers=Header.init ()) ?(body=[]) ?chunked ~rng ~creds meth uri =
  let open Cryptokit in
  let base_uri = Uri.to_string (Uri.with_query uri []) in
  let query_params = Uri.query uri in
  let fixed_params =
    ["oauth_consumer_key", [creds.consumer_key];
     "oauth_nonce", [nonce rng];
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
  let base_string = [Code.string_of_method meth; pct_encode base_uri; pct_encode params_string] in
  let base_string = String.concat "&" base_string in
  let signing_key = pct_encode creds.consumer_secret ^ "&" ^ pct_encode creds.access_token_secret in
  let signature = hash_string (MAC.hmac_sha1 signing_key) base_string in
  let signature = transform_string (Base64.encode_compact_pad ()) signature in
  let authorization_string = ("oauth_signature", [signature])::fixed_params in
  let authorization_string = List.map (fun (k, vs) ->
      pct_encode k,
      "\"" ^ pct_encode (List.hd vs) ^ "\"") authorization_string in
  let authorization_string = List.fast_sort compare (List.map (fun (k, v) -> k ^ "=" ^ v) authorization_string) in
  let authorization_string = ("OAuth " ^ String.concat ", " authorization_string) in
  let body_string = List.map (fun (k, vs) -> k, String.concat "," vs) body in
  let body_string = List.map (fun (k, v) -> k ^ "=" ^ v) body_string in
  let body_string = String.concat "&" body_string in
  let body = CB.body_of_string body_string in
  let headers = List.fold_left (fun a (k,v) -> Header.add a k v) headers
      ([ "User-Agent", "athanor/0.1";
        "Authorization", authorization_string
      ] @ if meth = `POST then [
      "Content-Length", string_of_int (String.length body_string);
      "Content-Type", "application/x-www-form-urlencoded"
    ] else []) in
  C.call ~headers ?body ?chunked meth uri

let _ =
  let open Cryptokit in
  let ic = open_in ".twitterstream" in
  let oc = open_out "twitter.stdout" in
  let creds = creds_of_json (Yojson.Basic.from_channel ic) in
  let rng = Random.pseudo_rng (Random.string Random.secure_rng 20) in
  let cmdargs = List.tl (Array.to_list Sys.argv) in
  Lwt_main.run
    begin
      signed_call ~body:["track", cmdargs] ~rng ~creds `POST (Uri.of_string "https://stream.twitter.com/1.1/statuses/filter.json")
      >>= function
      | None -> Printf.printf "No response, exiting... :(\n%!"; exit 0
      | Some (resp, body) ->
        let body_stream = CB.stream_of_body body in
        let rec print_body_forever stream =
          Lwt.catch
            (fun () -> Lwt_stream.next stream >>= fun frag ->
              Printf.printf "%s" frag;
              Printf.fprintf oc "%s" frag;
              print_body_forever stream)
            (fun _ -> Lwt.return ())
        in
        print_body_forever body_stream
    end

