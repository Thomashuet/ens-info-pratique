let u0 = 3

(* Question 1 *)

let next x = 15091 * x mod 64007

let b l =
  let arn = Array.make l 0 in
  let rec b k u =
    if k < l then begin
      arn.(k) <- u / 16002;
      b (k+1) (next u)
    end
  in
  b 0 (next u0);
  arn

let b20 = b 20
let b400 = b 400
let b10000 = b 10000

let () = begin
  print_string "Question 1\na) ";
  print_char [|'A'; 'C'; 'G'; 'U'|].(b20.(17));
  print_char [|'A'; 'C'; 'G'; 'U'|].(b20.(18));
  print_char [|'A'; 'C'; 'G'; 'U'|].(b20.(19));
  print_newline ();
  print_string "b) ";
  print_char [|'A'; 'C'; 'G'; 'U'|].(b400.(397));
  print_char [|'A'; 'C'; 'G'; 'U'|].(b400.(398));
  print_char [|'A'; 'C'; 'G'; 'U'|].(b400.(399));
  print_newline ();
  print_string "c) ";
  print_char [|'A'; 'C'; 'G'; 'U'|].(b10000.(9997));
  print_char [|'A'; 'C'; 'G'; 'U'|].(b10000.(9998));
  print_char [|'A'; 'C'; 'G'; 'U'|].(b10000.(9999));
  print_newline ()
end

(* Question 2 *)

let max_app arn =
  let count = Array.make 4 0 in
  for i = 0 to Array.length arn - 1 do
    count.(arn.(i)) <- count.(arn.(i)) + 1
  done;
  min count.(0) count.(3) + min count.(1) count.(2)

let () = begin
  print_string "Question 2\na) ";
  print_int (max_app b20);
  print_newline ();
  print_string "b) ";
  print_int (max_app b400);
  print_newline ();
  print_string "c) ";
  print_int (max_app b10000);
  print_newline ()
end

(* Question 3 *)

let max_emp arn =
  let l = Array.length arn in
  let maxi = ref 0
  and cur = ref 0
  in
  for i = 0 to l-1 do
    cur := 0;
    for j = 0 to min (l-1-i) i do
      if arn.(i+j) + arn.(i-j) = 3 then incr cur
      else cur := 0;
      maxi := max (!maxi) (!cur)
    done;
    cur := 0;
    for j = 0 to min (l-1-i) (i-1) do
      if arn.(i+j) + arn.(i-1-j) = 3 then incr cur
      else cur := 0;
      maxi := max (!maxi) (!cur)
    done
  done;
  !maxi

let () = begin
  print_string "Question 3\na) ";
  print_int (max_emp b20);
  print_newline ();
  print_string "b) ";
  print_int (max_emp b400);
  print_newline ();
  print_string "c) ";
  print_int (max_emp b10000);
  print_newline ()
end

(* Question 4 *)

let max_lex_fold arn =
  let l = Array.length arn in
  let r = Array.make l 0 in
  let rec fill_between a b =
    let i = ref a
    and j = ref b
    and k = ref b
    in
    while !i < !k do
      j := !k;
      while !i < !j do
	if arn.(!i) + arn.(!j) = 3 then begin
	  r.(!i) <- !j - !i;
          fill_between (!j+1) (!k);
	  incr i;
	  k := !j - 1;
	end;
	decr j;
      done;
      incr i
    done
  in
  fill_between 0 (l-1);
  r

let count_app r =
  let l = Array.length r in
  let ans = ref 0 in
  for i = 0 to l-1 do
    if r.(i) > 0 then incr ans
  done;
  !ans

let r20 = max_lex_fold b20
let r400 = max_lex_fold b400
let r10000 = max_lex_fold b10000

let () = begin
  print_string "Question 4\na) ";
  print_int (count_app r20);
  print_newline ();
  print_string "b) ";
  print_int (count_app r400);
  print_newline ();
  print_string "c) ";
  print_int (count_app r10000);
  print_newline ()
end

(* Question 5 *)

let count_loop r =
  let l = Array.length r in
  let ans = ref 0
  and cur = ref 0
  and incr = ref 0
  in
  for i = 0 to l-1 do
    if r.(i) = 0 then decr cur
    else begin
      cur := r.(i);
      incr := r.(i) - 1
    end;
    if !cur = 0 then ans := !ans + !incr
  done;
  !ans

let () = begin
  print_string "Question 5\na) ";
  print_int (count_loop r20);
  print_newline ();
  print_string "b) ";
  print_int (count_loop r400);
  print_newline ();
  print_string "c) ";
  print_int (count_loop r10000);
  print_newline ()
end

(* Question 6 *)

let max_lex_fold_NJ arn =
  let l = Array.length arn in
  let r = Array.make l 0 in
  let rec fill_between a b =
    let i = ref a
    and j = ref b
    and k = ref b
    in
    while !i < !k do
      j := !k;
      while !i < !j do
	if arn.(!i) + arn.(!j) = 3 || arn.(!i) + arn.(!j) = 5 then begin
	  r.(!i) <- !j - !i;
          fill_between (!j+1) (!k);
	  incr i;
	  k := !j - 1;
	end;
	decr j;
      done;
      incr i
    done
  in
  fill_between 0 (l-1);
  r

let count_app_NJ r b =
  let l = Array.length r in
  let ans = ref 0 in
  for i = 0 to l-1 do
    if r.(i) > 0 then begin
      if b.(i) = 0 || b.(i+r.(i)) = 0 then ans := !ans - 2
      else if b.(i) = 1 || b.(i+r.(i)) = 1 then ans := !ans - 3
      else ans := !ans - 1
    end
  done;
  !ans

let rNJ20 = max_lex_fold_NJ b20
let rNJ400 = max_lex_fold_NJ b400
let rNJ10000 = max_lex_fold_NJ b10000

let () = begin
  print_string "Question 6\na) ";
  print_int (count_app_NJ rNJ20 b20);
  print_newline ();
  print_string "b) ";
  print_int (count_app_NJ rNJ400 b400);
  print_newline ();
  print_string "c) ";
  print_int (count_app_NJ rNJ10000 b10000);
  print_newline ()
end

(* Question 7 *)

let b150 = b 150
let b1000 = b 1000

let app_max_NJ arn =
  let l = Array.length arn in
  let dyn = Array.make_matrix l l 0 in
  for i = 1 to l-1 do
    for j = 0 to l-i-1 do
      let mini = ref (if i <= 1 then 0 else dyn.(j+1).(j+i-1)) in
      if arn.(j) + arn.(j+i) = 5 then mini := !mini - 1
      else if arn.(j) = 0 && arn.(j+i) = 3 || arn.(j+i) = 0 && arn.(j) = 3 then mini := !mini - 2
      else if arn.(j) = 1 && arn.(j+i) = 2 || arn.(j+i) = 1 && arn.(j) = 2 then mini := !mini - 3;
      for k = 0 to i-1 do
        mini := min (!mini) (dyn.(j).(j+k) + dyn.(j+k+1).(j+i))
      done;
      dyn.(j).(j+i) <- !mini
    done
  done;
  dyn.(0).(l-1)

let () = begin
  print_string "Question 7\na) ";
  print_int (app_max_NJ b20);
  print_newline ();
  print_string "b) ";
  print_int (app_max_NJ b150);
  print_newline ();
  print_string "c) ";
  print_int (app_max_NJ b1000);
  print_newline ()
end

