open Cohttp
open Lwt.Infix

module C  = Cohttp_lwt_unix.Client
module CB = Cohttp_lwt_body
module YB = Yojson.Basic
module YS = Yojson.Safe

open Twitterutils

let daemonize = ref false

(* Sets the default logger to be stdout with a custom template *)
let () = Lwt_log.default := Lwt_log.channel
            ~template:"$(date).$(milliseconds) [$(level)]: $(message)"
            ~close_mode:`Keep
            ~channel:Lwt_io.stdout ()

(* Filter out everything that is not a tweet object *)
let is_tweet = function
  | `Assoc assocs -> begin
      match CCList.Assoc.get assocs "created_at" with
      | Some v when v <> `Null -> true
      | _ -> false
    end
  | #YB.json -> false

let sanitize_json_object f = function
  | `Assoc assocs -> `Assoc (YB.Util.filter_map f assocs)
  | #YB.json -> invalid_arg "not a JSON object"

let sanitize_user ((k, v) as kv) = match k with
  | "created_at" -> v |> YB.Util.to_string |> sanitize_date |> fun (t, _) -> Some ("created_at", `Int (int_of_float t))
  | "followers_count" -> Some kv
  | "friends_count" -> Some kv
  | "id_str" -> Some ("_id", v)
  | "lang" -> Some kv
  | "statuses_count" -> Some kv
  | "utc_offset" -> Some kv
  | _ -> None

let sanitize_tweet ((k, v) as kv) = match k with
  | "created_at" -> v |> YB.Util.to_string |> sanitize_date |> fun (t, _) -> Some ("created_at", `Int (int_of_float t))
  | "favorite_count" -> Some kv
  | "id_str" -> Some ("_id", v)
  | "in_reply_to_status_id" -> Some kv
  | "in_reply_to_user_id" -> Some kv
  | "lang" -> Some kv
  | "text" -> Some kv
  | "retweet_count" -> Some kv
  | "retweeted" -> Some kv
  | "user" -> Some ("user", sanitize_json_object sanitize_user v)
  | _ -> None

let stream ~creds ~tracks write_fun =
  let creds = List.hd creds in
  match%lwt
  signed_call ~body:["track", tracks] ~creds `POST
    (Uri.of_string "https://stream.twitter.com/1.1/statuses/filter.json")
  with
  | exception exn -> Lwt.fail exn
  | resp, body ->
      Lwt_log.ign_debug "Connected!";
      let open Lwt_stream in
      CB.to_stream body
      |> map (fun s -> try YB.from_string s with _ -> `Null)
      |> filter is_tweet
      |> map (sanitize_json_object sanitize_tweet)
      |> iter_s (fun json -> write_fun json)

let followers ~screen_name ~creds write_fun =
  let creds = List.hd creds in
  let followers = ref [] in
  let cursor = ref @@ -1 in
  let rec inner () =
    match%lwt
      signed_call ~creds `GET
        ~body:["screen_name", [screen_name];
               "cursor", [string_of_int !cursor];
               "count", ["5000"]
              ]
        Uri.(of_string "https://api.twitter.com/1.1/followers/ids.json")
    with
    | exception exn -> Lwt.fail exn
    | resp, body ->
        C.Response.pp_hum Format.err_formatter resp;
        Lwt_log.ign_debug "Connected!";
        let open Lwt_stream in
        CB.to_stream body
        |> map (fun s -> try YB.from_string s with _ -> `Null)
        (* |> filter is_tweet *)
        (* |> map (sanitize_json_object sanitize_tweet) *)
        |> iter_s (fun json -> write_fun json)
  in
  inner ()

let users ~screen_name ~creds write_fun =
  let creds = List.hd creds in
  match%lwt
    signed_call ~body:["screen_name", [screen_name]] ~creds `POST
      Uri.(of_string "https://api.twitter.com/1.1/users/lookup.json")
  with
  | exception exn -> Lwt.fail exn
  | resp, body ->
      Lwt_log.ign_debug "Connected!";
      let open Lwt_stream in
      CB.to_stream body
      |> map (fun s -> try YB.from_string s with _ -> `Null)
      (* |> filter is_tweet (\* also works for users *\) *)
      (* |> map (sanitize_json_object sanitize_tweet) *)
      |> iter_s (fun json -> write_fun json)

let _ =
  let conf_file = ref "config.json" in
  let tracks = ref [] in
  let mode = ref `Stream in
  let speclist = Arg.(align [
      "-user", String (fun s -> mode := `User s), " Download users";
      "-conf", Set_string conf_file, "<string> Path of the configuration file (default: .twitterstream)";
      "-daemon", Set daemonize, " Start the program as a daemon";
      "-v", Unit (fun () -> Lwt_log.(add_rule "*" Info)), " Be verbose";
      "-vv", Unit (fun () -> Lwt_log.(add_rule "*" Debug)), " Be more verbose"
    ]) in
  let anon_fun s = tracks := s::!tracks in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> track [tracks...]\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
    let creds = match (YS.from_file !conf_file) with
      | `List creds ->
          CCList.filter_map
            (fun c -> match creds_of_yojson c with `Ok c -> Some c | `Error _ -> None)
            creds
      | #YS.json -> invalid_arg "config file"
    in
    if !daemonize then Lwt_daemon.daemonize ();
    let write_fun json = Lwt_io.printf "%s\n" (YS.prettify @@ YB.to_string json) in
    match !mode, !tracks with
    | `Stream, [] ->
        prerr_endline "You have to specify at least one track.";
        exit 1
    | `Stream, tracks ->
        let run () =
          Nocrypto_entropy_lwt.initialize () >>
          stream ~creds ~tracks write_fun
        in
        Lwt_main.run @@ run ()
    | `User screen_name, _ ->
        let run () =
          Nocrypto_entropy_lwt.initialize () >>
          followers ~screen_name ~creds write_fun
        in
        Lwt_main.run @@ run ()
