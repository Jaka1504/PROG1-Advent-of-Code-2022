let razpakiraj seznam =
    let rec razpakiraj_aux acc nov_seznam = function
    | [] -> nov_seznam
    | "" :: xs -> razpakiraj_aux 0 (acc :: nov_seznam) xs
    | x :: xs -> razpakiraj_aux (acc + int_of_string x) nov_seznam xs
    in
    razpakiraj_aux 0 [] seznam
    
let najvecji seznam =
    let rec najvecji_aux acc = function
    | [] -> acc
    | x :: xs -> najvecji_aux (if x > acc then x else acc) xs
    in
    najvecji_aux min_int seznam

let preberi vsebina_datoteke = 
    String.split_on_char '\n' vsebina_datoteke
    
let top_k k seznam =
    let rec top_k_aux k candidates = function
    | [] -> candidates
    | x :: xs -> top_k_aux k (
        let mali :: ostala = candidates in
        if mali > x then candidates else List.sort (-) (x :: ostala)
    )
    xs
    in
    top_k_aux k [0; 0; 0] seznam

let rec sum = function
    | [] -> 0
    | x :: xs -> x + sum xs

let naloga1 vsebina_datoteke =
    string_of_int (najvecji (razpakiraj (preberi vsebina_datoteke)))

let naloga2 vsebina_datoteke =
    string_of_int (sum (top_k 3 (razpakiraj (preberi vsebina_datoteke))))

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan ((in_channel_length chan) - 2234) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "input/day_01.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "output/day_01_1.out" odgovor1;
    izpisi_datoteko "output/day_01_2.out" odgovor2