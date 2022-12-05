let preberi vsebina_datoteke = vsebina_datoteke
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ' ')

let ovrednoti1 = function
  | "A" :: t :: [] -> (
    match t with
    | "X" -> 4
    | "Y" -> 8
    | "Z" -> 3
    | _ -> failwith "Nepričakovan vnos"
    )
  | "B" :: t :: [] -> (
    match t with
    | "X" -> 1
    | "Y" -> 5
    | "Z" -> 9
    | _ -> failwith "Nepričakovan vnos"
    )
  | "C" :: t :: [] -> (
    match t with
    | "X" -> 7
    | "Y" -> 2
    | "Z" -> 6
    | _ -> failwith "Nepričakovan vnos"
    )
  | _ -> failwith "Nepričakovan vnos"

  let ovrednoti2 = function
  | "A" :: t :: [] -> (
    match t with
    | "X" -> 3
    | "Y" -> 4
    | "Z" -> 8
    | _ -> failwith "Nepričakovan vnos"
    )
  | "B" :: t :: [] -> (
    match t with
    | "X" -> 1
    | "Y" -> 5
    | "Z" -> 9
    | _ -> failwith "Nepričakovan vnos"
    )
  | "C" :: t :: [] -> (
    match t with
    | "X" -> 2
    | "Y" -> 6
    | "Z" -> 7
    | _ -> failwith "Nepričakovan vnos"
    )
  | _ -> failwith "Nepričakovan vnos"


let naloga1 vsebina_datoteke = vsebina_datoteke
  |> preberi (* Da seznam seznamov po dveh črk *)
  |> List.map ovrednoti1 (* iz parov da točke *)
  |> List.fold_left (+) 0 (* sešteje po seznamu *)
  |> string_of_int

let naloga2 vsebina_datoteke = vsebina_datoteke
  |> preberi (* Da seznam seznamov po dveh črk *)
  |> List.map ovrednoti2 (* iz parov da točke *)
  |> List.fold_left (+) 0 (* sešteje po seznamu *)
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
  let vsebina_datoteke = preberi_datoteko "input/day_02.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_02_1.out" odgovor1;
  izpisi_datoteko "output/day_02_2.out" odgovor2