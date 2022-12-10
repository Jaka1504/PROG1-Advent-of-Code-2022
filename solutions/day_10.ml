let preberi_po_vrsticah vsebina_datoteke = 
  String.split_on_char '\n' vsebina_datoteke


let seznam_vrednosti_registra seznam_navodil =
  let rec seznam_vrednosti_registra_aux acc = function
  | [] -> List.rev acc
  | navodilo :: ostalo ->
    let trenutni = List.nth acc 0 in
    let navodilo_po_kosih = String.split_on_char ' ' navodilo in
    match List.length navodilo_po_kosih with
    | 1 -> seznam_vrednosti_registra_aux (trenutni :: acc) ostalo
    | 2 -> let dodatek = int_of_string (List.nth navodilo_po_kosih 1) in
      seznam_vrednosti_registra_aux ((dodatek + trenutni) :: trenutni :: acc) ostalo
    | _ -> failwith ("Čudno navodilo: " ^ navodilo)
  in
  seznam_vrednosti_registra_aux [1] seznam_navodil


let sestej_zanimive zgodovina_registra =
  let zanimivi = List.filteri (fun i _ -> (i mod 40 = 19) && (i < 220)) zgodovina_registra in
  List.fold_left (+) 0 (List.mapi (fun i x -> x * (20 + 40 * i)) zanimivi) 


let string_od_seznama_crk seznam_crk = 
  let rec string_od_seznama_crk_aux acc do_konca_vrste = function
  | [] -> acc
  | crka :: preostanek -> match do_konca_vrste with
    | 0 -> string_od_seznama_crk_aux (acc ^ crka ^ "\n") 39 preostanek
    | n -> string_od_seznama_crk_aux (acc ^ crka) (n - 1) preostanek
  in
  string_od_seznama_crk_aux "" 39 seznam_crk


let slika seznam_vrednosti_registra =
  List.mapi (
    fun i x ->
      if (Int.abs ((i mod 40) - x)) <= 1 then
        "█"
      else
        " "
  ) seznam_vrednosti_registra
  |> string_od_seznama_crk
  
  
let naloga1 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> seznam_vrednosti_registra
  |> sestej_zanimive
  |> string_of_int


let naloga2 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> seznam_vrednosti_registra
  |> slika


let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan ((in_channel_length chan)) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "input/day_10.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_10_1.out" odgovor1;
  izpisi_datoteko "output/day_10_2.out" odgovor2