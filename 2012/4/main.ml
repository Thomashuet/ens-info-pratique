let u0 = 7

let rec u = function
| 0 -> u0
| k -> 15091 * u (k-1) mod 64007

let (%) f g x = f (g x)

let question n f a b c =
  Printf.printf "Question %d\na) %s\nb) %s\nc) %s\n" n (f a) (f b) (f c)

let () = question 1 (string_of_int % u) 10 100 1000

type 'a arbre = F of 'a | N of 'a arbre * 'a * 'a arbre

let adr = function
| F a -> a
| N(_, a, _) -> a

let rec a i = function
| 0 -> F(i, i)
| n ->
  let g = a (i+1) (u (2*n) mod n) in
  let d = a (snd (adr g) + 1) (u (2*n+1) mod n) in
  N(g, (i, snd (adr d)), d)

let a10 = a 0 10
let a100 = a 0 100
let a500 = a 0 500
let a1000 = a 0 1000
let a3000 = a 0 3000

let rec noeuds a =
  let l, m = adr a in
  m - l + 1

let rec feuilles a =
  (noeuds a + 1) / 2

let () = question 2 (fun a -> Printf.sprintf "%d, %d" (noeuds a) (feuilles a)) a10 a100 a1000

let rec goto acc l = function
| F(ll, _) as a -> if l = ll then (a :: acc), a else invalid_arg "goto"
| N(a, (ll, _), b) as c ->
  if l = ll then (c :: acc), c else
  let (_, m) = adr a in
  if l <= m then goto (c :: acc) l a
  else goto (c :: acc) l b
let goto = goto []

let rec m_of_l l = function
| F(ll, m) -> if l = ll then m else invalid_arg "m_of_l"
| N(a, (ll, m), b) ->
  if l = ll then m else
  let (_, m) = adr a in
  if l <= m then m_of_l l a
  else m_of_l l b

let m_of_l a l = snd % adr % snd % goto l @@ a

let () = question 3 (fun (a, l) -> string_of_int (m_of_l a l)) (a10, 3) (a100, 9) (a1000, 30)

let rec dist_list (=) l1 l2 = match l1, l2 with
| h1 :: t1, h2 :: t2 when h1 = h2 -> dist_list (=) t1 t2
| _ -> List.length l1 + List.length l2

let dist a i j =
  let l1, _ = goto i a in
  let l2, _ = goto j a in
  dist_list (==) (List.rev l1) (List.rev l2)

let () = question 4 (fun (a, i, j) -> string_of_int (dist a i j - 1)) (a10, 3, 9) (a100, 5, 30) (a1000, 30, 90)
