let test = "[T]     [D]         [L]            
[R]     [S] [G]     [P]         [H]
[G]     [H] [W]     [R] [L]     [P]
[W]     [G] [F] [H] [S] [M]     [L]
[Q]     [V] [B] [J] [H] [N] [R] [N]
[M] [R] [R] [P] [M] [T] [H] [Q] [C]
[F] [F] [Z] [H] [S] [Z] [T] [D] [S]
[P] [H] [P] [Q] [P] [M] [P] [F] [D]";;

let pridobi_tekst_za_skladisce vsebina_datoteke =
  String.sub vsebina_datoteke 0 287

let pridobi_seznam_navodil vsebina_datoteke =
  String.sub vsebina_datoteke 325 ((String.length vsebina_datoteke) - 325)
  |> String.split_on_char '\n'

let sestavi_skladisce tekst =
  let rec preoblikuj_vrstico = function
    | "" -> []
    | niz -> niz.[1] :: preoblikuj_vrstico (
      if String.length niz > 4 then
        (String.sub niz 4 ((String.length niz) - 4))
      else
        ""
        )
  in
  let dodaj_vrstico skladisce vrstica =
    List.mapi (fun index stolpec -> (List.nth vrstica index) :: stolpec) skladisce
  in
  tekst
  |> String.split_on_char '\n'
  |> List.map preoblikuj_vrstico
  |> List.fold_left dodaj_vrstico [[]; []; []; []; []; []; []; []; []]
  |> List.map (List.filter (fun ch -> ch <> ' '))
  |> List.map List.rev
  
let skladisce_test = sestavi_skladisce test;;
  
let parametri_iz_navodil navodilo =
  let seznam = String.split_on_char ' ' navodilo in
  [1; 3; 5]
  |> List.map (List.nth seznam)
  |> List.mapi (fun i niz -> if i = 0 then int_of_string niz else (int_of_string niz) - 1)

let rec prestavi1 skladisce = function
  | 0 :: _ -> skladisce 
  | koliko :: od_kod :: kam :: [] -> 
    prestavi1 (skladisce
      |> List.mapi (
        fun i kup ->
          if i = kam then
            (List.nth (List.nth skladisce od_kod) 0) :: kup
          else
            kup
      )
      |> List.mapi (
        fun i kup ->
          if i = od_kod then
            match kup with
            | [] -> failwith "Prazen kup"
            | _ :: ostali -> ostali
          else
            kup
      ) 
    ) ((koliko - 1) :: od_kod :: kam :: [])
  | _ -> failwith "Napaka pri razvozlavanju navodila"


let prvih_n seznam n =
  let rec prvih_n_aux acc seznam = function
  | 0 -> acc
  | n -> match seznam with
    | [] -> failwith "Prekratek seznam"
    | x :: xs -> prvih_n_aux (x :: acc) xs (n - 1)
  in
  List.rev (prvih_n_aux [] seznam n)


let prestavi2 skladisce = function
  | koliko :: od_kod :: kam :: [] -> skladisce
    |> List.mapi (
      fun i kup ->
        if i = kam then
          (prvih_n (List.nth skladisce od_kod) koliko) @ kup
        else
          kup
    )
    |> List.mapi (
      fun i kup ->
        if i = od_kod then
          List.filteri (fun i _ -> i >= koliko) kup
        else
          kup
    ) 
  | _ -> failwith "Napaka pri razvozlavanju navodila"


let naloga1 vsebina_datoteke =
  pridobi_seznam_navodil vsebina_datoteke
  |> List.map parametri_iz_navodil
  |> List.fold_left prestavi1 (vsebina_datoteke |> pridobi_tekst_za_skladisce |> sestavi_skladisce)
  |> List.map (fun x -> List.nth x 0)
  |> List.fold_left (fun acc ch -> acc ^ (String.make 1 ch)) ""


let naloga2 vsebina_datoteke =
  pridobi_seznam_navodil vsebina_datoteke
  |> List.map parametri_iz_navodil
  |> List.fold_left prestavi2 (vsebina_datoteke |> pridobi_tekst_za_skladisce |> sestavi_skladisce)
  |> List.map (fun x -> List.nth x 0)
  |> List.fold_left (fun acc ch -> acc ^ (String.make 1 ch)) ""


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
  let vsebina_datoteke = preberi_datoteko "input/day_05.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_05_1.out" odgovor1;
  izpisi_datoteko "output/day_05_2.out" odgovor2