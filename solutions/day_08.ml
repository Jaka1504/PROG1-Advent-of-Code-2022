let preberi_po_vrsticah vsebina =
  String.split_on_char '\n' vsebina


let seznam_znakov niz =
  let rec seznam_znakov_aux acc = function
  | "" -> acc
  | niz -> seznam_znakov_aux (niz.[0] :: acc) (String.sub niz 1 ((String.length niz) - 1))
  in
  seznam_znakov_aux [] niz
  |> List.rev


let matrika seznam_vrstic = seznam_vrstic
  |> List.map seznam_znakov
  |> List.map (List.filter (function '\r' -> false | _ -> true))
  |> List.map (List.map int_of_char)
  |> List.map (List.map (fun x -> x - 48))


let vidna_drevesa_v_vrstici seznam_dreves =
  let rec vidna_drevesa_aux acc max = function
  | [] -> acc
  | drevo :: ostalo ->
    if drevo > max then
      vidna_drevesa_aux (true :: acc) drevo ostalo
    else
      vidna_drevesa_aux (false :: acc) max ostalo
  in 
  (vidna_drevesa_aux [] min_int seznam_dreves) |> List.rev

(* Zarotira matriko za 90° v pozitivni smeri *)
let zarotiraj_matriko matrika =
  List.mapi (fun i _ -> List.map (fun x -> List.nth x i) matrika) (List.nth matrika 0)
  |> List.rev


let rec uporabi_veckrat n funkcija argument = 
  if n < 0 then failwith "Negativno število aplikacij funkcije" else
  match n with
  | 0 -> argument
  | n -> uporabi_veckrat (n - 1) funkcija (funkcija argument)


(* Smer 0 je z leve, 1 od zgoraj, 2 z desne in 3 od spodaj. *)
let vidna_drevesa_iz_smeri smer matrika = matrika
  |> uporabi_veckrat smer zarotiraj_matriko
  |> List.map vidna_drevesa_v_vrstici
  |> uporabi_veckrat (4 - smer) zarotiraj_matriko


let ali_po_komponentah_na_matrikah matrika1 matrika2 = matrika1
  |> List.mapi (fun i vrstica -> List.mapi (fun j stolpec -> (List.nth (List.nth matrika2 i) j) || stolpec) vrstica)


let vidna_drevesa_skupaj matrika = [1; 2; 3]
  |> List.map (fun smer -> (vidna_drevesa_iz_smeri smer matrika))
  |> List.fold_left ali_po_komponentah_na_matrikah (vidna_drevesa_iz_smeri 0 matrika)


let prestej_po_matriki matrika =
  let prestej_vrstico vrstica = List.fold_left (fun stevec p -> if p then stevec + 1 else stevec) 0 vrstica in
  List.fold_left (fun stevec vrstica -> stevec + (prestej_vrstico vrstica)) 0 matrika

(* Druga naloga: *)


let st_vidnih_dreves_v_vrstici visina_hisice seznam_dreves =
  let rec st_vidnih_dreves_v_vrstici_aux acc = function
  | [] -> acc
  | drevo :: ostalo ->
    if drevo >= visina_hisice then
      acc + 1
      (* naprej nas tu ne zanima *)
    else
      st_vidnih_dreves_v_vrstici_aux (acc + 1) ostalo
  in 
  st_vidnih_dreves_v_vrstici_aux 0 seznam_dreves


let st_vidnih_dreves_iz_smeri smer matrika = 
  let po_vrstici vrstica st_drevesa visina = st_vidnih_dreves_v_vrstici (List.nth vrstica st_drevesa) (List.filteri (fun i _ -> i > st_drevesa) vrstica) in
  matrika
  |> uporabi_veckrat smer zarotiraj_matriko
  |> List.map (fun vrstica -> List.mapi (po_vrstici vrstica) vrstica) 
  |> uporabi_veckrat (4 - smer) zarotiraj_matriko


let produkt_po_komponentah_na_matrikah matrika1 matrika2 = matrika1
  |> List.mapi (fun i vrstica -> List.mapi (fun j stolpec -> (List.nth (List.nth matrika2 i) j) * stolpec) vrstica)


let scenic_score_matrika matrika = [1; 2; 3]
  |> List.map (fun smer -> (st_vidnih_dreves_iz_smeri smer matrika))
  |> List.fold_left produkt_po_komponentah_na_matrikah (st_vidnih_dreves_iz_smeri 0 matrika)


let max_po_matriki matrika =
  let max_vrstice vrstica = List.fold_left (fun max trenutni -> if trenutni > max then trenutni else max) min_int vrstica in
  List.fold_left (fun max vrstica -> if (max_vrstice vrstica) > max then (max_vrstice vrstica) else max) min_int matrika


(* Testni podatki *)
let testni_gozd =
"1436245
3672812
3474837
6937448
1564594"

let testna_matrika = testni_gozd
  |> preberi_po_vrsticah
  |> matrika


let naloga1 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> matrika
  |> vidna_drevesa_skupaj
  |> prestej_po_matriki
  |> string_of_int

let naloga2 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> matrika
  |> scenic_score_matrika
  |> max_po_matriki
  |> string_of_int

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
  let vsebina_datoteke = preberi_datoteko "input/day_08.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_08_1.out" odgovor1;
  izpisi_datoteko "output/day_08_2.out" odgovor2