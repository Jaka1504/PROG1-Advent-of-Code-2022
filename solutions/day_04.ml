let preberi vsebina_datoteke = 
  let seznam_nizov = String.split_on_char '\n' vsebina_datoteke in
  let uredi niz = niz
  |> String.split_on_char ','
  |> List.map (String.split_on_char '-')
  |> List.map (List.map int_of_string)
  in
  List.map uredi seznam_nizov

let preveri1 = function
  | (a :: b :: []) :: (c :: d :: []) :: [] -> (a >= c && b <= d) || (a <= c && b >= d)
  | _ -> failwith "Ni prave dolzine seznam"

let preveri2 = function
  | (a :: b :: []) :: (c :: d :: []) :: [] -> c <= b && a <= d
  | _ -> failwith "Ni prave dolzine seznam"

let prestej predikat seznam =
  let rec prestej_aux predikat acc = function
  | [] -> acc
  | x :: xs -> if predikat x then prestej_aux predikat (acc + 1) xs else prestej_aux predikat acc xs
  in prestej_aux predikat 0 seznam

let naloga1 vsebina_datoteke = vsebina_datoteke
  |> preberi
  |> prestej preveri1
  |> string_of_int

let naloga2 vsebina_datoteke = vsebina_datoteke
  |> preberi
  |> prestej preveri2
  |> string_of_int

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "input/day_04.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_04_1.out" odgovor1;
  izpisi_datoteko "output/day_04_2.out" odgovor2