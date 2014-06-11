let u0 = 26

let size_max = 64007

let v = Array.make size_max 0

let () =
  let rec fill ui i =
    if i < size_max then begin
      v.(i) <- ui / 320 - 100;
      fill (15091 * ui mod 64007) (i + 1)
    end
  in
  fill u0 0

let x i =
  min v.(2 * i) v.(2 * i + 1), max v.(2 * i) v.(2 * i + 1)

(* Question 1 *)

let print_segment (a, b) = begin
  print_char '[';
  print_int a;
  print_string ", ";
  print_int b;
  print_char ']';
end

let () = begin
  print_string "Question 1\na) ";
  print_segment (x 1);
  print_string "\nb) ";
  print_segment (x 100);
  print_string "\nc) ";
  print_segment (x 10000);
  print_newline ()
end

(* Question 2 *)

let center2 (a, b) = a + b

let rec arg_max f i =
  if i <= 0 then 0
  else
    let j = arg_max f (i - 1) in
    if f.(i) >= f.(j) then i else j

let () = begin
  let count = Array.make 401 0 in
  print_string "Question 2\na) ";
  for i = 1 to 100 do
    count.(center2 (x i) + 200) <- count.(center2 (x i) + 200) + 1
  done;
  let a = arg_max count 400 in
  print_float (float (a - 200) /. 2.);
  print_string " (x";
  print_int count.(a);
  print_string ")\nb) ";
  for i = 101 to 1000 do
    count.(center2 (x i) + 200) <- count.(center2 (x i) + 200) + 1
  done;
  let b = arg_max count 400 in
  print_float (float (b - 200) /. 2.);
  print_string " (x";
  print_int count.(b);
  print_string ")\nc) ";
  for i = 1001 to 10000 do
    count.(center2 (x i) + 200) <- count.(center2 (x i) + 200) + 1
  done;
  let c = arg_max count 400 in
  print_float (float (c - 200) /. 2.);
  print_string " (x";
  print_int count.(c);
  print_string ")";
  print_newline ()
end

(* Question 3 *)

let square x = x *. x

let ($) f g x = f (g x)

let avg x =
  Array.fold_left (+.) 0. x /. float (Array.length x)

let var x = avg (Array.map square x) -. square (avg x)

let e n =
  let x = Array.init n (fun i -> x (i+1)) in
  avg (Array.map (float $ fst) x), avg (Array.map (float $ snd) x)

let print_segment_float (a, b) = begin
  print_char '[';
  print_float a;
  print_string ", ";
  print_float b;
  print_char ']';
end

let () = begin
  print_string "Question 3\na) ";
  print_segment_float (e 10);
  print_string "\nb) ";
  print_segment_float (e 100);
  print_string "\nc) ";
  print_segment_float (e 10000);
  print_newline ()
end

(* Question 8 *)

let add (a, b) (c, d) = a + c, b + d

let mul (a, b) (c, d) =
  min (min (a * c) (a * d)) (min (b * c) (b * d)),
  max (max (a * c) (a * d)) (max (b * c) (b * d))

let sq (a, b) = min (abs (max 0 (a * b))) (min (a * a) (b * b)), max (a * a) (b * b)

let () = begin
  print_string "Question 8\na) ";
  print_segment (mul (x 12) (x 23));
  print_string "\nb) ";
  print_segment (mul (x 33) (x 33));
  print_string "\nc) ";
  print_segment (sq (x 33));
  print_string "\nd) ";
  print_segment (sq (x 44));
  print_newline ()
end
