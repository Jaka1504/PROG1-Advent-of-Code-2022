let prva_ponovitev1 vsebina =
  let rec prva_ponovitev_aux acc niz =
    if (String.length niz) < 4 then
      failwith "Niz prekratek"
    else
      String.sub niz 0 4
      |> String.mapi (
        fun i ch -> ((String.contains (String.sub niz (i + 1) (3 - i)) ch) |> (function true -> '1' | false -> '0'))
      )
      |> function "0000" -> acc | _ -> prva_ponovitev_aux (acc + 1) (String.sub niz 1 ((String.length niz) - 1))
  in
  prva_ponovitev_aux 4 vsebina

let prva_ponovitev2 vsebina =
  let rec prva_ponovitev_aux acc niz =
    if (String.length niz) < 14 then
      failwith "Niz prekratek"
    else
      String.sub niz 0 14
      |> String.mapi (
        fun i ch -> ((String.contains (String.sub niz (i + 1) (13 - i)) ch) |> (function true -> '1' | false -> '0'))
      )
      |> function "00000000000000" -> acc | _ -> prva_ponovitev_aux (acc + 1) (String.sub niz 1 ((String.length niz) - 1))
  in
  prva_ponovitev_aux 14 vsebina


let naloga1 vsebina_datoteke = vsebina_datoteke
  |> prva_ponovitev1
  |> string_of_int

let naloga2 vsebina_datoteke = vsebina_datoteke
  |> prva_ponovitev2
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
  let vsebina_datoteke = preberi_datoteko "input/day_06.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_06_1.out" odgovor1;
  izpisi_datoteko "output/day_06_2.out" odgovor2