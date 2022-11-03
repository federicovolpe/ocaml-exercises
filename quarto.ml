(* Let's write a function (or a pool of functions) that given a quite large text 
(over than 2000 words)
counts how frequent each word occurs in the text.
The text is read from a file (look at the pervasive module in the manual)
and it is a real text with punctuation (i.e., commas, semicolons, ...) that should be counted.
Note that words with different case should be considered the same.*)
let file = "file 4.txt";;
let () =
let ic = open_in file 
in
try
  let line = input_line ic in
  print_endline line;
  flush stdout;
  close_in ic
with e ->df
  close_in_noerr ic;
  raise e
;;