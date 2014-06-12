let u0 = 1

let (%) f g x = f (g x)

let question n f l =
  Printf.printf "Question %d\n" n;
  List.iteri (fun i -> Printf.printf "%c) %s\n" (Char.chr (i+97)) % f) l

let u = Array.make 1000001 0
let () = begin
  u.(0) <- u0;
  for i = 1 to 1000000 do
    u.(i) <- 15731 * u.(i-1) mod 32003
  done
end

let () = question 1 (fun i -> string_of_int u.(i)) [1000; 10000; 30000]

let v i j = u.(1+i+j*(j-1)/2)

let g m n =
  let g = Array.make n [] in
  for i = 0 to n-2 do
    for j = i+1 to n-1 do
      if v i j mod m = 0 then begin
        g.(i) <- j :: g.(i);
        g.(j) <- i :: g.(j);
      end
    done
  done;
  g

let g_5_10 = g 5 10
let g_6_16 = g 6 16
let g_7_20 = g 7 20
let g_10_50 = g 10 50
let g_50_50 = g 50 50
let g_125_100 = g 125 100
let g_50_250 = g 50 250
let g_360_250 = g 360 250

let aretes g = Array.fold_right ((+) % List.length) g 0 / 2
let degre g = Array.fold_right (max % List.length) g 0

let () = question 2 (fun g -> Printf.sprintf "%d, %d" (aretes g) (degre g)) [g_5_10; g_10_50; g_50_250]

let plus_petit g i j =
  let di = List.length g.(i) in
  let dj = List.length g.(j) in
  dj < di || di = dj && i <= j

let cmp g i j =
  if i = j then 0 else if plus_petit g i j then -1 else 1

let ordre g =
  let o = Array.init (Array.length g) (fun i -> i) in
  Array.sort (cmp g) o;
  o

let o_5_10 = ordre g_5_10
let o_6_16 = ordre g_6_16
let o_7_20 = ordre g_7_20
let o_10_50 = ordre g_10_50
let o_50_50 = ordre g_50_50
let o_125_100 = ordre g_125_100
let o_50_250 = ordre g_50_250
let o_360_250 = ordre g_360_250

let () =
  question 3 (fun o -> Printf.sprintf "%d, %d" o.(0) o.(Array.length o - 1))
    [o_5_10; o_10_50; o_50_250]

let rec couvre g c i =
  i < 0 || (c.(i) || List.for_all (fun j -> c.(j)) g.(i)) && (couvre g c (i-1))
let couvre g c = couvre g c (Array.length g - 1)

let suivante p =
  let n = Array.length p in
  let c = ref 0 in
  while p.(n - 1 - !c) do
    p.(n - 1 - !c) <- false;
    incr c
  done;
  let i = ref (n - 1 - !c) in
  while !i >= 0 && not p.(!i) do
    decr i
  done;
  if !i < 0 then
    for i = 0 to !c do
      p.(i) <- true
    done
  else begin
    p.(!i) <- false;
    for j = 1 to !c + 1 do
      p.(!i + j) <- true
    done
  end

let couverture g =
  let t = Array.make (Array.length g) false in
  while not (couvre g t) do suivante t done;
  t

let string_of_t t =
  let b = Buffer.create 1 in
  for i = 0 to Array.length t - 1 do
    if t.(i) then begin
      Buffer.add_string b (string_of_int i);
      Buffer.add_string b ", "
    end
  done;
  Buffer.sub b 0 (Buffer.length b - 2)

let () = question 4 (string_of_t % couverture) [g_5_10; g_6_16; g_7_20]

let cc g =
  let n = Array.length g in
  let classe = Array.init n (fun i -> i) in
  let rec propage c i =
    if classe.(i) = i then begin
      classe.(i) <- c;
      List.iter (propage c) g.(i)
    end
  in
  for i = 0 to n-1 do propage i i done;
  classe

let nb_cc classe =
  Array.fold_right (+) (Array.mapi (fun i j -> if i = j then 1 else 0) classe) 0

let taille classe =
  let n = Array.length classe in
  let t = Array.make n 0 in
  Array.iter (fun i -> t.(i) <- t.(i) + 1) classe;
  t

let () =
  question 5
    (fun g -> let c = cc g in Printf.sprintf "%d, %d" (nb_cc c) (Array.fold_right (max) (taille c) 0))
    [g_5_10; g_6_16; g_7_20; g_10_50; g_50_50; g_125_100; g_50_250; g_360_250]

let card c = Array.fold_right (+) (Array.map (fun b -> if b then 1 else 0) c) 0

let card_couv_cc g =
  let n = Array.length g in
  let c = cc g in
  let classes = Array.make n [] in
  Array.iteri (fun i j -> classes.(j) <- i :: classes.(j)) c;
  let total = ref 0 in
  for i = 0 to n - 1 do
    let c = Array.of_list classes.(i) in
    let tc = Array.make n 0 in
    Array.iteri (fun i j -> tc.(j) <- i) c;
    let sg = Array.init (Array.length c) (fun i -> List.map (fun j -> tc.(j)) g.(c.(i))) in
    total := !total + card (couverture sg)
  done;
  !total

let () = question 6 (Printf.sprintf "%d" % card_couv_cc) [g_50_50; g_125_100; g_360_250]

let approch g =
  let n = Array.length g in
  let r = Array.make n false in
  for i = 0 to n-1 do
    if List.exists (fun j -> not r.(j)) g.(i) then r.(i) <- true
  done;
  r

let () =
  question 7
    (fun g -> Printf.sprintf "%d" (card (approch g )))
    [g_5_10; g_6_16; g_7_20; g_10_50; g_50_50; g_125_100; g_50_250; g_360_250]

let approch_ordre g =
  let n = Array.length g in
  let r = Array.make n false in
  let b = Array.to_list (Array.init n (fun i -> i)) in
  let cmp i j =
    if i = j then 0 else
    let di = List.length (List.filter (fun j -> not r.(j)) g.(i)) in
    let dj = List.length (List.filter (fun j -> not r.(j)) g.(j)) in
    if dj < di || di = dj && i <= j then -1 else 1
  in
  let tri l =
    List.sort cmp l
  in
  let rec boucle = function
  | [] -> ()
  | h :: t ->
    if List.exists (fun j -> not r.(j)) g.(h) then begin
      r.(h) <- true;
      boucle (tri t);
    end else boucle t
  in
  boucle (tri b);
  r

let () =
  question 8 (fun g -> Printf.sprintf "%d" (card (approch_ordre g)))
    [g_5_10; g_6_16; g_7_20; g_10_50; g_50_50; g_125_100; g_50_250; g_360_250]

let approch_opt g =
  let n = Array.length g in
  let r = Array.make n false in
  let rl = ref [] in
  let b = Array.to_list (Array.init n (fun i -> i)) in
  let cmp i j =
    if i = j then 0 else
    let di = List.length (List.filter (fun j -> not r.(j)) g.(i)) in
    let dj = List.length (List.filter (fun j -> not r.(j)) g.(j)) in
    if dj < di || di = dj && i <= j then -1 else 1
  in
  let tri l =
    List.sort cmp l
  in
  let rec boucle = function
  | [] -> ()
  | h :: t ->
    if List.exists (fun j -> not r.(j)) g.(h) then begin
      r.(h) <- true;
      rl := h :: !rl;
      boucle (tri t);
    end else boucle t
  in
  boucle (tri b);
  let r = Array.make n false in
  List.iter (fun i -> if List.exists (fun j -> not r.(j)) g.(i) then r.(i) <- true) !rl;
  r

let () =
  question 9 (fun g -> Printf.sprintf "%d" (card (approch_opt g)))
    [g_5_10; g_6_16; g_7_20; g_10_50; g_50_50; g_125_100; g_50_250; g_360_250]
