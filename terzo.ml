module Matrix =
struct
type matrix = int list list

(*1) A function zeroes to construct a matrix of size n×m filled with zeros.*)
let zeroes (n : int) (m : int) =
  Array.make_matrix n m

(*2) A function identity to construct the identity matrix 
(the one with all 0s but the 1s on the diagonal) of given size.*)
let identity size =
  Array.init size (fun x -> Array.init size
         (fun y -> if x = y then 1 else 0))

(*3) A function init to construct a square matrix of a given size n 
filled with the first n×n integers.*)
let init n m =
  List.init n (fun x -> List.init m 
  (fun y -> x+y))

(*4) A function transpose that transposes 
a generic matrix independently of its size and content.*)

let transpose matrix1 : matrix =
  List.init (List.length matrix1)(fun x -> List.init (List.length matrix1) (fun y -> List.nth (List.nth matrix1 y) x) )

(*5) The basics operators + and * 
that adds and multiplies two matrices non necessarily squared.*)
let (+) a b =
List.map2 (fun riga1 riga2 -> List.map2 (fun elem1 elem2 -> elem1 + elem2) riga1 riga2) a b

let ( * ) (a : matrix) (b : matrix) : int list list =
List.map2 (fun riga1 colonna2 -> List.map2 (fun r1 c1 -> r1 * c1) riga1 colonna2) a b

end ;;

Matrix.(+)
[[0; 1; 2; 3; 4]; 
[5; 6; 7; 8; 9]; 
[10; 11; 12; 13; 14]; 
[15; 16; 17; 18; 19];
[20; 21; 22; 23; 24] ]

[[0; 5; 10; 15; 20]; 
[1; 6; 11; 16; 21]; 
[2; 7; 12; 17; 22];
 [3; 8; 13; 18; 23]; 
 [4; 9; 14; 19; 24]]