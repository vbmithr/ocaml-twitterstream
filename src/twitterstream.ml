open Cohttp
module C  = Cohttp_lwt_unix.Client
module CB = Cohttp_lwt_body
module YB = Yojson.Basic

let (>>=) = Lwt.bind
let (|>) x f = f x

let daemonize = ref false

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

(* Filter out everything that is not a tweet object *)
let is_tweet = function
  | `Assoc _ as a -> (match YB.Util.member "created_at" a with `Null -> false | _ -> true)
  | _ -> false

let sanitize_json_object f = function
  | `Assoc assocs -> `Assoc (YB.Util.filter_map f assocs)
  | _ -> raise (Invalid_argument "not a JSON object")

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

let sanitize_user kv = match fst kv with
  | "created_at" -> snd kv |> YB.Util.to_string |>
    sanitize_date |> fst |> int_of_float |> string_of_int
    |> fun t -> Some ("created_at", `String t)
  | "followers_count" -> Some kv
  | "friends_count" -> Some kv
  | "id_str" -> Some ("_id", snd kv)
  | "lang" -> Some kv
  | "statuses_count" -> Some kv
  | "utc_offset" -> Some kv
  | _ -> None

let sanitize_tweet kv = match fst kv with
  | "created_at" -> snd kv |> YB.Util.to_string |>
    sanitize_date |> fst |> int_of_float |> string_of_int
    |> fun t -> Some ("created_at", `String t)
  | "favorite_count" -> Some kv
  | "id_str" -> Some ("_id", snd kv)
  | "in_reply_to_status_id" -> Some kv
  | "in_reply_to_user_id" -> Some kv
  | "lang" -> Some kv
  | "text" -> Some kv
  | "retweet_count" -> Some kv
  | "retweeted" -> Some kv
  | "user" -> Some ("user", sanitize_json_object sanitize_user (snd kv))
  | _ -> None

let main ?db_uri ~db_name ~creds ~rng ~tracks =
  if tracks = []
  then
    (prerr_endline "You have to specify at least one track.";
     exit 1)
  else
    Couchdb.handle ?uri:db_uri () >>= fun h ->
    Couchdb.DB.create h db_name >>= fun _ ->
    let main_exn () =
      signed_call ~body:["track", tracks] ~rng ~creds `POST
        (Uri.of_string "https://stream.twitter.com/1.1/statuses/filter.json") >>= function
      | None -> Lwt.fail (Failure "signed_call returned None")
      | Some (resp, body) ->
        Lwt_stream.(
          CB.stream_of_body body
          |> map (fun s -> try YB.from_string s with _ -> `Null)
          |> filter is_tweet
          |> map (sanitize_json_object sanitize_tweet)
          |> iter_s (fun json ->
              let id = json |> YB.Util.member "_id" |> YB.Util.to_string in
              (* Capture exceptions from CouchDB. We don't want to
                 reconnect to twitter because of a CouchDB error. *)
              Lwt.try_bind
                (fun () -> Couchdb.Doc.add h db_name json)
                (function
                  | `Success (st, _) -> Lwt_io.printf "%s %s\n" id (Couchdb.string_of_status st)
                  | `Failure (st, err) -> Lwt_io.printf "%s %s: %s\n" id (Couchdb.string_of_status st) err)
                (fun exn -> Lwt_io.printf "%s: CouchDB raised %s\n" id (Printexc.to_string exn))
            ))
    in
    let rec main () =
      Lwt.catch main_exn
        (fun exn ->
           Lwt_io.printf "Caught unhandled exception %s\n"
             (Printexc.to_string exn)) >>= fun () ->
      main ()
    in main ()

let _ =
  let open Arg in
  let db_uri = ref "http://127.0.0.1:5984" in
  let db_name = ref "twitter" in
  let conf_file = ref ".twitterstream" in
  let tracks = ref [] in
  let speclist = align [
      "--conf", Set_string conf_file, "<string> Path of the configuration file (default: .twitterstream)";
      "--db-name", Set_string db_name, "<string> Name of the CouchDB database";
      "--db-uri", Set_string db_uri, "<string> URI of the CouchDB server in use (default: http://localhost:5984)";
      "--daemon", Set daemonize, " Start the program as a daemon"
    ] in
  let anon_fun s = tracks := s::!tracks in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> track [tracks...]\nOptions are:" in
  parse speclist anon_fun usage_msg;
  let ic = open_in !conf_file in
  let creds = creds_of_json (YB.from_channel ic) in
  close_in ic;
  if !daemonize then Lwt_daemon.daemonize ();
  let open Cryptokit in
  let rng = Random.device_rng "/dev/urandom" in
  Lwt_main.run (main ~db_uri:!db_uri ~db_name:!db_name ~creds ~rng ~tracks:!tracks)
