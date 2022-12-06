let prva_ponovitev_splosna n vsebina =
  let rec prva_ponovitev_aux acc niz =
    if (String.length niz) < n then
      failwith "Niz prekratek"
    else
      let same_nule = String.make n '0' in
      let dejanski_niz =
      String.sub niz 0 n (* Pogleda prvih n characterjev*)
      |> String.mapi (
        fun i ch -> ((String.contains (String.sub niz (i + 1) (n - 1 - i)) ch) |> (function true -> '1' | false -> '0'))
      ) (* za vsakega preveri ali se še kdaj pojavi kasneje *)
      in if dejanski_niz = same_nule then
        acc (* če se ne, smo konec *)
      else
        prva_ponovitev_aux (acc + 1) (String.sub niz 1 ((String.length niz) - 1)) (* če se, pogledamo naslednji niz dolžine n *)
  in prva_ponovitev_aux n vsebina

let naloga1 vsebina_datoteke = vsebina_datoteke
  |> prva_ponovitev_splosna 4
  |> string_of_int

let naloga2 vsebina_datoteke = vsebina_datoteke
  |> prva_ponovitev_splosna 14
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