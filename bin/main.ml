open Lwt
open Cohttp_lwt_unix
open Soup

(* return the body of the scraped webpage *)
let getBody url =
  (* t >>= f *)
  (* bind t f *)
  (* if Client.get fails don't run the function on the right *)
  Client.get (Uri.of_string url) >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string

(* match https://www.gutenberg.org/ebooks/bookshel *)
let process s q =
  if String.starts_with ~prefix:"/ebooks/bookshel" s then
    Queue.push ("https://www.gutenberg.org" ^ s) q

let () =
  (* get webpage from url *)
  let body = Lwt_main.run (getBody "https://www.gutenberg.org/ebooks/bookshelf/") in
  let data = Hashtbl.create 1000000007 in

  (* lambdasoup basic parse *)
  let links = Queue.create () in
  let genres = Queue.create () in
  let soup = body |> parse in
  soup $$ "a[title]" |> iter (fun a -> Queue.push (R.attribute "title" a) genres);
  soup $$ "a[href]" |> iter (fun a -> process (R.attribute "href" a) links);
  Queue.iter (print_endline) genres;
  
