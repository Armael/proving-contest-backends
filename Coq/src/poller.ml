open Rresult

type config = {
  token : string;
  baseurl : string;
} [@@deriving yojson { strict = false }]

let read_config (filename : string) =
  config_of_yojson (Yojson.Safe.from_file filename)


type response = {
  sID : int
} [@@deriving yojson { strict = false }]

type submission_files = {
  defs : string; [@key "Defs"]
  check : string; [@key "Check"]
  submission : string; [@key "Submission"]
} [@@deriving yojson { strict = false }]

type submission = {
  sID : int;
  aID : int;
  allow_sorry : bool;
  checkfile : string;
  files : submission_files;
} [@@deriving yojson { strict = false }]

let json_of_string s =
  try Ok (Yojson.Safe.from_string s) with
    Yojson.Json_error msg -> Error ("Invalid json: " ^ msg)

let poll_submission_raw ~(cfg : config) : (string, string) result =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let url = Uri.of_string (cfg.baseurl ^ "/pollsubmission/?ITP=COQ") in
  let headers = Header.of_list ["Authorization", "Token " ^ cfg.token] in
  Lwt_main.run @@ begin
    Client.get ~headers url >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    Cohttp_lwt.Body.to_string body >|= fun body ->
    if Code.is_success code then
      Ok body
    else
      Error (Printf.sprintf "Response code %d: %s" code body)
  end

let poll_submission ~(cfg : config) : (submission option, string) result =
  let open R in
  poll_submission_raw ~cfg >>= fun body ->
  json_of_string body >>= fun j ->
  response_of_yojson j >>= function
  | { sID = -1 } -> Ok None
  | _ -> submission_of_yojson j >>| (fun x -> Some x)

let main () =
  read_config Sys.argv.(1) >>= fun cfg ->
  Printf.eprintf "token: %s; baseurl: %s\n%!" cfg.token cfg.baseurl;
  poll_submission ~cfg >>| function
  | None ->
    Printf.printf "Success: No submission\n"
  | Some submission ->
    Printf.printf "Success: %s\n"
      (Yojson.Safe.to_string (submission_to_yojson submission))

let () =
  match main () with
  | Ok () -> ()
  | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
