(* Let's write a function (or a pool of functions) that given a quite large text 
(over than 2000 words)
counts how frequent each word occurs in the text.
The text is read from a file (look at the pervasive module in the manual)
and it is a real text with punctuation (i.e., commas, semicolons, ...) that should be counted.
Note that words with different case should be considered the same.*)
let file = "file 4.txt";;
let channel = open_in file;;
let input_line_opt channel = 
  try Some (input_line channel)
  with End_of_file -> None
;;

let rec print_file channel =
  let line = input_line_opt channel in
  match line with
      Some line -> (print_endline line; print_file channel)
      | None -> ()
;;

(* funzione che data una stringa di parole e una parola ne conta le ricorrenze *)

let rec contaparole (line : string) (parola : string) : int =
  match line with
  
;;