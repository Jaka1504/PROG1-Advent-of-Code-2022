let prioriteta znak =
  let lestvica = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  (String.index lestvica znak) + 1


let razdeli_na_compartmente rucksack = 
  let len = String.length rucksack / 2 in
  let prvi = String.sub rucksack 0 len in
  let drugi = String.sub rucksack len len in
  (prvi, drugi)


let rec poisci_skupni_element = function
| [] -> failwith "Ni danih nizov"
| seq2 :: list_of_seq -> 
  match seq2 with
  | "" -> failwith "Napaka"
  | seq2 ->
    if List.length (List.filter_map (fun seq1 -> String.index_opt seq1 seq2.[0]) list_of_seq) = List.length list_of_seq
    then 
      seq2.[0]
    else
      poisci_skupni_element ((String.sub seq2 1 (String.length seq2 - 1)) :: list_of_seq)


let poisci_skupni_element_v_compartmentih rucksack = 
  let (prvi, drugi) = razdeli_na_compartmente rucksack in
  poisci_skupni_element [prvi; drugi]


let pristej_prioriteto niz stevilo =
  let dodatek = niz
  |> poisci_skupni_element_v_compartmentih
  |> prioriteta
  in
  stevilo + dodatek

let preberi vsebina_datoteke = 
  String.split_on_char '\n' vsebina_datoteke


let naloga1 vsebina_datoteke =
  let seznam_vrstic = preberi vsebina_datoteke in
  string_of_int (List.fold_right pristej_prioriteto seznam_vrstic 0)

  
let naloga2 vsebina_datoteke =
  let seznam_vrstic = preberi vsebina_datoteke in
  let rec naloga2_aux acc = function
  | [] -> acc
  | a :: b :: c :: xs -> 
    let skupni = [a; b; c] |> poisci_skupni_element in
    naloga2_aux (acc + prioriteta skupni) xs
  | _ -> failwith "Dolžina seznama ni deljiva s tri."
  in
  string_of_int (naloga2_aux 0 seznam_vrstic)


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
  let vsebina_datoteke = preberi_datoteko "input/day_03.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_03_1.out" odgovor1;
  izpisi_datoteko "output/day_03_2.out" odgovor2