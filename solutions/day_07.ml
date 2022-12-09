type dir = {
  files_size: int;
  dirs: dir list;
}

let preberi_po_vrsticah vsebina_datoteke = 
  String.split_on_char '\n' vsebina_datoteke

let sestej seznam_vrstic =
  let rec sestej_aux skupna_vsota delne_vsote = function
    | [] -> skupna_vsota
    | "$ ls" :: xs -> sestej_aux skupna_vsota (0 :: delne_vsote) xs
    | "$ cd .." :: xs ->
      let delna_vsota :: ostale_vsote = delne_vsote in
      if delna_vsota <= 100000 then
        sestej_aux (skupna_vsota + delna_vsota) ostale_vsote xs
      else
        sestej_aux skupna_vsota ostale_vsote xs
    | vrstica :: xs ->
      if (String.starts_with ~prefix:"$ cd" vrstica) || (String.starts_with ~prefix:"dir" vrstica) then
        sestej_aux skupna_vsota delne_vsote xs
      else
        let file_size = vrstica |> (String.split_on_char ' ') |> (fun sez -> List.nth sez 0) |> int_of_string in
        sestej_aux skupna_vsota (List.map (fun x -> file_size + x) delne_vsote) xs
  in
  sestej_aux 0 [] seznam_vrstic


let velikost_vseh_datotek seznam_vrstic = 
  let rec velikost_vseh_datotek_aux acc = function
  | [] -> acc
  | vrstica :: xs ->
    if (String.starts_with ~prefix:"$" vrstica) || (String.starts_with ~prefix:"dir" vrstica) then
      velikost_vseh_datotek_aux acc xs
    else
      let file_size = vrstica |> (String.split_on_char ' ') |> (fun sez -> List.nth sez 0) |> int_of_string in
      velikost_vseh_datotek_aux (acc + file_size) xs
  in
  velikost_vseh_datotek_aux 0 seznam_vrstic


let poisci_min seznam_vrstic =
  let potreben_prostor = (velikost_vseh_datotek seznam_vrstic) - 40000000 in
  let rec poisci_min_aux min delne_vsote = function
    | [] -> min
    | "$ ls" :: xs -> poisci_min_aux min (0 :: delne_vsote) xs
    | "$ cd .." :: xs -> (
      match delne_vsote with
      | delna_vsota :: ostale_vsote ->
        if (delna_vsota < min) && (delna_vsota >= potreben_prostor) then
          poisci_min_aux delna_vsota ostale_vsote xs
        else
          poisci_min_aux min ostale_vsote xs
      | [] -> failwith "prevec cd .."
      )
    | vrstica :: xs ->
      if (String.starts_with ~prefix:"$ cd" vrstica) || (String.starts_with ~prefix:"dir" vrstica) then
        poisci_min_aux min delne_vsote xs
      else
        let file_size = vrstica |> (String.split_on_char ' ') |> (fun sez -> List.nth sez 0) |> int_of_string in
        poisci_min_aux min (List.map (fun x -> file_size + x) delne_vsote) xs
  in
  poisci_min_aux max_int [] seznam_vrstic
  

let naloga1 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> sestej
  |> string_of_int

let naloga2 vsebina_datoteke = vsebina_datoteke
  |> preberi_po_vrsticah
  |> poisci_min
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
  let vsebina_datoteke = preberi_datoteko "input/day_07.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_07_1.out" odgovor1;
  izpisi_datoteko "output/day_07_2.out" odgovor2