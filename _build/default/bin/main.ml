open Lwt
open Cohttp_lwt_unix
open Soup

(* return the body of the scraped webpage *)
let getBody url =
  (* t >>= f *)
  (* bind t f *)
  (* if Client.get fails don't run the function on the right *)
  Client.get (Uri.of_string url) >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string

(* process each page of titles *)
let process s =
  let res  = Queue.create () in
  let soup = Lwt_main.run (getBody s) |> parse in
  (* select all titles from the page and push to queue *)
  soup $$ ".title" |> iter (fun t -> Queue.push (R.leaf_text t) res);
  res

let () =
  (* get webpage from url *)
  let body = Lwt_main.run (getBody "https://www.gutenberg.org/ebooks/bookshelf/") in
  let data = Hashtbl.create 1000000007 in

  (* lambdasoup basic parse *)
  let links = Queue.create () in
  let soup = body |> parse in

  (* get list of all next links to crawl *)
  soup $$ "a[href]" |> iter (fun a -> 
    if String.starts_with ~prefix:"/ebooks/bookshel" (R.attribute "href" a) then
      Queue.push ("https://www.gutenberg.org" ^ (R.attribute "href" a)) links);

  (* for each title crawl that link for all titles belonging to that genre *)
  soup $$ "a[title]" |> iter (fun a ->
    (* title is genre -> list of book titles *)
    Hashtbl.add data (R.attribute "title" a) (process (Queue.take links)));

  (* output the resultant data table *)
  Hashtbl.iter (fun k v -> print_endline k; print_endline (Queue.take v)) data
;;
