

(*mi creo una variante con i tipi*)
type scala = |Celsius|Farenheit|Kelvin|Rankine|Delisle|Newton|Reaumur|Romer ;;
type temperatura = {valore : float; unita : scala} ;;

(*1 Write a function that given a pure number returns 
a conversion table for it among any of the 8 scales.*)

let all2c t = 
  match t.unita with
  |Celsius   -> t
  |Farenheit -> {valore = (t.valore -. 32.) *. 5. /. 9.;    unita = Celsius}
  |Kelvin    -> {valore = t.valore -. 273.15;               unita = Celsius}
  |Rankine   -> {valore = (t.valore -. 491.67) *. 5. /. 9.; unita = Celsius}
  |Delisle   -> {valore = 100. -. (t.valore *. 2. /. 3. );  unita = Celsius}
  |Newton    -> {valore = t.valore *. 100. /. 33.;          unita = Celsius}
  |Reaumur   -> {valore = t.valore *. 5. /. 4.;             unita = Celsius}
  |Romer     -> {valore = (t.valore -. 7.5) *. 40. /. 21. ; unita = Celsius}
;;

(*2 Write a function that given a temperature in a specified scale returns a list of all 
the corresponding temperatures in the other scales, note that the scale must be specified.*)

let c2all t wish =
  match wish with
  |Celsius -> t
  |Farenheit -> {valore = t.valore *. 9. /. 5. +. 32.;     unita = Farenheit}
  |Kelvin ->    {valore = t.valore +. 273.15;              unita = Kelvin}
  |Rankine ->   {valore = (t.valore +. 273.15)*. 9. /. 5.; unita = Rankine}
  |Delisle ->   {valore = (100. -. t.valore) *. 3. /. 2.;  unita = Delisle}
  |Newton ->    {valore = t.valore *. 33. /. 100. ;        unita = Newton}
  |Reaumur ->   {valore = t.valore *. 4. /. 5.;            unita = Reaumur}
  |Romer ->     {valore = t.valore *. 21. /. 40. +. 7.5 ;  unita = Romer}
;;
