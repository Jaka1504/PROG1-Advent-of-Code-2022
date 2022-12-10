type polje = {
  x: int;
  y: int
}


type dve_polji = {
  p1: polje;
  p2: polje
}


type stanje = {
  obiskani: bool list list;
  polji: dve_polji
  }

  
(* vektorsko sestevanje *)
let v_plus v1 v2 =
  {x = v1.x + v2.x; y = v1.y + v2.y}


let preberi_po_vrsticah vsebina_datoteke = 
  String.split_on_char '\n' vsebina_datoteke


let d_neskoncno_norma polje = 
  Int.max (Int.abs polje.x) (Int.abs polje.y)


let predznak stevilo = if stevilo = 0 then 0 else (if stevilo > 0 then 1 else -1)


(* razmik je za koliko je glava bolj desno dol od repa *)
let premik_repa razmik =
  if (d_neskoncno_norma razmik) < 2 then
    {x = 0; y = 0}
  else
    {x = predznak razmik.x; y = predznak razmik.y}


(* Koordinate kot v matrikah, pozitivna smer desno in dol *)
let premik_iz_navodil vrstica =
  let [smer; st_korakov] = String.split_on_char ' ' vrstica in
  let st_korakov = int_of_string st_korakov in
  match smer with
  | "R" -> {x = st_korakov; y = 0}
  | "D" -> {x = 0; y = st_korakov}
  | "L" -> {x = -st_korakov; y = 0}
  | "U" -> {x = 0; y = -st_korakov}
  | _ -> failwith "Neustrezna navodila"


(* Določi najmanjši okvir, ki ga H in T ne zapustita. p1 je levo zgoraj, p2 pa desno spodaj *)
let okvir seznam_navodil =
  let rec okvir_aux acc polje = function
  | [] -> acc
  | navodilo :: ostale ->
    let novo_polje = v_plus polje navodilo in
    okvir_aux {
      p1 = {
        x = Int.min novo_polje.x acc.p1.x;
        y = Int.min novo_polje.y acc.p1.y
      };
      p2 = {
        x = Int.max novo_polje.x acc.p2.x;
        y = Int.max novo_polje.y acc.p2.y
      }
    } novo_polje ostale
  in
  okvir_aux {p1 = {x = 0; y = 0}; p2 = {x = 0; y = 0}} {x = 0; y = 0} seznam_navodil


let nastavi_element_matrike matrika polje vrednost =
  let i = polje.x and j = polje.y in
  matrika
  |> List.mapi (
    fun st_vrstice vrstica ->
      if st_vrstice = i then
        List.mapi (
          fun st_stolpca stolpec ->
            if st_stolpca = j then
              vrednost
            else
              stolpec
        ) vrstica
      else
        vrstica
    )


let rec izvrsi_navodilo stanje = 
  let matrika = stanje.obiskani and polji_HT = stanje.polji in
  function
  | {x = 0; y = 0} -> stanje
  | {x = dx; y = dy} -> 
    let novi_polji = {
      p1 = v_plus polji_HT.p1 {x = predznak dx; y = predznak dy};
      p2 = v_plus polji_HT.p2 (
        premik_repa {
          x = polji_HT.p1.x + predznak dx - polji_HT.p2.x;
          y = polji_HT.p1.y + predznak dy - polji_HT.p2.y
          }
        )
      }
    in
      izvrsi_navodilo
      {
        obiskani = nastavi_element_matrike matrika novi_polji.p2 true;
        polji = novi_polji
      }
      {
        x = dx - predznak dx;
        y = dy - predznak dy
      }
  
let rec matrika_enakih_vrednosti st_vrstic st_stolpcev vrednost =
  let rec vrstica_enakih_vrednosti vrednost = function
  | 0 -> []
  | n -> vrednost :: (vrstica_enakih_vrednosti vrednost (n - 1))
  in
  match st_vrstic with
  | 0 -> []
  | n -> (vrstica_enakih_vrednosti vrednost st_stolpcev) :: (matrika_enakih_vrednosti (n - 1) st_stolpcev vrednost)


let zacetno_stanje okvir =
  let st_vrstic = okvir.p2.x - okvir.p1.x + 1 and st_stolpcev = okvir.p2.y - okvir.p1.y + 1 in
  {
    obiskani = matrika_enakih_vrednosti st_vrstic st_stolpcev false;
    polji = {
      p1 = {
        x = - okvir.p1.x;
        y = - okvir.p1.y
      };
      p2 = {
        x = - okvir.p1.x;
        y = - okvir.p1.y
      }
    }
  }


let prestej_po_matriki matrika =
  let prestej_vrstico vrstica = List.fold_left (fun stevec p -> if p then stevec + 1 else stevec) 0 vrstica in
  List.fold_left (fun stevec vrstica -> stevec + (prestej_vrstico vrstica)) 0 matrika


(* Druga podnaloga *)

let posodobi_vrv nova_glava vrv =
  let rec posodobi_vrv_aux acc nova_glava = function
    | [] -> List.rev acc
    | vozel :: preostanek -> 
      let nov_vozel = v_plus vozel (premik_repa {x = nova_glava.x - vozel.x; y = nova_glava.y - vozel.y}) in
      posodobi_vrv_aux (nov_vozel :: acc) nov_vozel preostanek
  in
  posodobi_vrv_aux [] nova_glava vrv


type stanje2 = {
  obiskani: bool list list;
  vrv: polje list
}


let rec izvrsi_navodilo2 stanje2 = 
  let matrika = stanje2.obiskani and glava :: vozli = stanje2.vrv in
  function
  | {x = 0; y = 0} -> stanje2
  | {x = dx; y = dy} -> 
    let nova_glava = v_plus glava {x = predznak dx; y = predznak dy} in
    let nova_vrv = nova_glava :: posodobi_vrv nova_glava vozli in
    izvrsi_navodilo2
    {
      obiskani = nastavi_element_matrike matrika (List.nth nova_vrv 9) true;
      vrv = nova_vrv
    }
    {
      x = dx - predznak dx;
      y = dy - predznak dy
    }


let zacetno_stanje2 okvir =
  let st_vrstic = okvir.p2.x - okvir.p1.x + 1 and st_stolpcev = okvir.p2.y - okvir.p1.y + 1 in
  {
    obiskani = matrika_enakih_vrednosti st_vrstic st_stolpcev false;
    vrv = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
    |> List.map (
      fun _ -> 
        {
          x = - okvir.p1.x;
          y = - okvir.p1.y
        };
      )
  }


let naloga1 vsebina_datoteke = 
  let seznam_navodil = List.map premik_iz_navodil (preberi_po_vrsticah vsebina_datoteke) in
  let zacetek = seznam_navodil |> okvir |> zacetno_stanje in
  let koncna_matrika = (List.fold_left izvrsi_navodilo zacetek seznam_navodil).obiskani in
  prestej_po_matriki koncna_matrika
  |> string_of_int

  
let naloga2 vsebina_datoteke = 
  let seznam_navodil = List.map premik_iz_navodil (preberi_po_vrsticah vsebina_datoteke) in
  let zacetek = seznam_navodil |> okvir |> zacetno_stanje2 in
  let koncna_matrika = (List.fold_left izvrsi_navodilo2 zacetek seznam_navodil).obiskani in
  prestej_po_matriki koncna_matrika
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
  let vsebina_datoteke = preberi_datoteko "input/day_09.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_09_1.out" odgovor1;
  izpisi_datoteko "output/day_09_2.out" odgovor2