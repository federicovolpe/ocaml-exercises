let alkaline_earth_metals = ("beryllium",4)::("magnesium",12)::("calcium",20)::("strontium",38)::("barium",56)::("radium",88)::[];;

(*funzione che da una lista di interi ne ricava quello con il valore maggiore*)
let heaviest lst = 
  List.fold_left maggiore (List.hd lst) (List.tl lst)
;;

let maggiore x y = 
  let z = (snd x) - (snd y) in
  if z > 0 then x else y
;;

let (>:) a b = (snd a) - (snd b);;
let sort x = List.sort (>:) x ;;
sort alkaline_earth_metals ;;

let nobile_gasses = ("helium",2)::("neon",10)::("argon",18)::("krypton",36)::("xenon",54)::("radon",86)::[];;
(*funzione che unisce le due liste e stampa come risultato una lista che deve essere ordinata*)
List.merge (>:) alkaline_earth_metals nobile_gasses ;;
