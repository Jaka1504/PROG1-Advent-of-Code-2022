type opica = {
  stvari: int list;
  funkcija_koliko: int -> int;
  funkcija_kam: int -> int;
  business: int
}


let preberi_po_vrsticah vsebina_datoteke = 
  String.split_on_char '\n' vsebina_datoteke


let spakiraj_navodila_po_opicah seznam_vrstic =
  let rec spakiraj_navodila_po_opicah_aux acc sub_acc = function
  | [] -> List.rev acc
  | "" :: ostalo -> spakiraj_navodila_po_opicah_aux ((List.rev sub_acc) :: acc) [] ostalo
  | vrstica :: ostalo -> spakiraj_navodila_po_opicah_aux acc (vrstica :: sub_acc) ostalo
  in
  spakiraj_navodila_po_opicah_aux [] [] seznam_vrstic



let stvari_iz_vrstice vrstica = 
  let rec stvari_iz_vrsice_aux acc sub_acc = function
  | "" -> List. rev ((int_of_string sub_acc) :: acc)
  | neprazna -> 
    let prvi = neprazna.[0] and nadaljevanje = String.sub neprazna 1 ((String.length neprazna) - 1) in
    if prvi = ' ' then
      stvari_iz_vrsice_aux acc "" nadaljevanje
    else
      if prvi = ',' then
        stvari_iz_vrsice_aux ((int_of_string sub_acc) :: acc) "" nadaljevanje
      else
        stvari_iz_vrsice_aux acc (sub_acc ^ (String.make 1 prvi)) nadaljevanje
  in
  stvari_iz_vrsice_aux [] "" vrstica


let zadnja_beseda_v_vrstici vrstica = vrstica
  |> String.split_on_char ' '
  |> (fun sez -> List.nth sez ((List.length sez) -1))


let sestavi_opico navodila =
  let trenutne_stvari = stvari_iz_vrstice (List.nth navodila 1) in
  let modul = zadnja_beseda_v_vrstici (List.nth navodila 3) |> int_of_string
  and true_vrednost = zadnja_beseda_v_vrstici (List.nth navodila 4) |> int_of_string
  and false_vrednost = zadnja_beseda_v_vrstici (List.nth navodila 5) |> int_of_string
  and vrstica_z_operacijo = List.nth navodila 2 in
  let dodatek = zadnja_beseda_v_vrstici vrstica_z_operacijo
  and operacija = if String.contains vrstica_z_operacijo '*' then '*' else '+'
  in
  let koliko worry_level =
    if dodatek = "old" then
      ((worry_level * worry_level) / 3)
    else
      let dodatek = dodatek |> int_of_string in
      if operacija = '+' then
        ((worry_level + dodatek) / 3)
      else
        ((worry_level * dodatek) / 3)
  in
  let kam worry_level = if (koliko worry_level) mod modul = 0 then true_vrednost else false_vrednost in
  {stvari = trenutne_stvari; funkcija_koliko = koliko; funkcija_kam = kam; business = 0}


let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina
  

let input = preberi_datoteko "input/day_11.in"
  

let opice_na_zacetku = input
  |> preberi_po_vrsticah
  |> spakiraj_navodila_po_opicah
  |> List.map sestavi_opico


let runda opice =
  let rec runda_aux na_vrsti opice =
    if na_vrsti >= List.length opice then opice else
    let opica_na_vrsti = List.nth opice na_vrsti in
    match opica_na_vrsti.stvari with
    | [] -> runda_aux (na_vrsti + 1) opice
    | item :: ostali ->
      let koliko = opica_na_vrsti.funkcija_koliko item
      and kam = opica_na_vrsti.funkcija_kam item in
      let nove_opice = opice
      |> List.mapi (
        fun index ta_opica ->
          if index = na_vrsti then
            {ta_opica with stvari = ostali; business = ta_opica.business + 1} 
          else if index = kam then
            {ta_opica with stvari = ta_opica.stvari @ [koliko]}
          else ta_opica
      )
      in 
      runda_aux na_vrsti nove_opice
  in runda_aux 0 opice


let rec uporabi_veckrat n funkcija argument = 
  if n < 0 then failwith "Negativno število aplikacij funkcije" else
  match n with
  | 0 -> argument
  | n -> uporabi_veckrat (n - 1) funkcija (funkcija argument)


let produkt_najvecjih_dveh seznam =
  let rec produkt_najvecjih_dveh_aux acc1 acc2 = function
    | [] -> acc1 * acc2
    | x :: xs -> 
      if x > acc1 then
        produkt_najvecjih_dveh_aux x acc1 xs
      else if x > acc2 then 
        produkt_najvecjih_dveh_aux acc1 x xs
      else 
        produkt_najvecjih_dveh_aux acc1 acc2 xs
  in produkt_najvecjih_dveh_aux 0 0 seznam

  
(* Druga podnaloga *)
(* Ideja: ker se mi ne da razmisljat o boljsem nacinu,
  deluje da vzamemo vedno modulo lcm podanih modulov,
  da ne bo previsoka stevilka *)

let lcm = 11 * 19 * 7 * 17 * 3 * 5 * 13 * 2

(* žal mi je da sem grešil in kopiral kodo, trdno sklenem, da se bom poboljšal, sam zdle se mi res ne da :( *)
let sestavi_opico2 navodila =
  let trenutne_stvari = stvari_iz_vrstice (List.nth navodila 1) in
  let modul = zadnja_beseda_v_vrstici (List.nth navodila 3) |> int_of_string
  and true_vrednost = zadnja_beseda_v_vrstici (List.nth navodila 4) |> int_of_string
  and false_vrednost = zadnja_beseda_v_vrstici (List.nth navodila 5) |> int_of_string
  and vrstica_z_operacijo = List.nth navodila 2 in
  let dodatek = zadnja_beseda_v_vrstici vrstica_z_operacijo
  and operacija = if String.contains vrstica_z_operacijo '*' then '*' else '+'
  in
  let koliko worry_level =
    if dodatek = "old" then
      ((worry_level * worry_level) mod lcm)
    else
      let dodatek = dodatek |> int_of_string in
      if operacija = '+' then
        ((worry_level + dodatek) mod lcm)
      else
        ((worry_level * dodatek) mod lcm)
  in
  let kam worry_level = if (koliko worry_level) mod modul = 0 then true_vrednost else false_vrednost in
  {stvari = trenutne_stvari; funkcija_koliko = koliko; funkcija_kam = kam; business = 0}


let naloga1 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> spakiraj_navodila_po_opicah
  |> List.map sestavi_opico
  |> uporabi_veckrat 20 runda
  |> List.map (fun opica -> opica.business)
  |> produkt_najvecjih_dveh
  |> string_of_int
  


let naloga2 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> spakiraj_navodila_po_opicah
  |> List.map sestavi_opico2
  |> uporabi_veckrat 10000 runda
  |> List.map (fun opica -> opica.business)
  |> produkt_najvecjih_dveh
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
  let vsebina_datoteke = preberi_datoteko "input/day_11.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_11_1.out" odgovor1;
  izpisi_datoteko "output/day_11_2.out" odgovor2