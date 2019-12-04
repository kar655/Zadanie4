(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(* Kwadrat liczby *)
let square x = x *. x

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
w pozostałych przypadkach 0 razy *)
let prostokat ((x1, y1): point) ((x2, y2): point) =
  (fun (x3, y3) ->
    if x3 >= x1 && x3 <= x2 && y3 >= y1 && y3 <= y2 then 1
    else 0
  : kartka)

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie [p] i promieniu [r] *)
let kolko ((x1, y1): point) r =
  (fun (x2, y2) ->
    if sqrt (square (x2 -. x1) +. square (y2 -. y1)) <= r then 1
    else 0
  : kartka)


(* odbija punkt wzgledem prostej *)
let gen a b c x y =
  ((square b -. square a) *. x -. 2.0 *. a *. b *. y -. 2.0 *. a *. c) /. (square a +. square b)

(* wartosc funckji w punkcie *)
let value x y a b c =
  a *. x +. b *. y +. c

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0.
Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej -
tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
który nałożył się na punkt przebicia. *)
let zloz ((x1, y1): point) ((x2, y2): point) (kar: kartka) =
  let a = y1 -. y2 in
  let b = x2 -. x1 in
  let c = x1 *. y2 -. y1 *. x2 in

  (fun (x3, y3) ->
    let v = value x3 y3 a b c in
    if v > 0.0 then kar (x3, y3) + kar (gen a b c x3 y3, gen b a c y3 x3)
    else if v = 0.0 then kar (x3, y3)
    else 0
  : kartka)

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy *)
let skladaj (lista: (point * point) list) (kar: kartka) =
  List.fold_left (fun a (p1, p2) -> zloz p1 p2 a ) kar lista




(* Przykladowe testy *)
(*
let op=[((0.0,1.0),(9.0,7.0));((10.0,10.0),(9.0,3.0));((9.0,10.0),(10.0,7.0));((1.0,2.0),(1.0,7.0));((10.0,3.0),(2.0,9.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test0=skladaj op kartka;;
assert (test0 (6.0,0.0)=0);;
let op=[((7.0,6.0),(7.0,10.0));((10.0,10.0),(5.0,3.0));((2.0,7.0),(10.0,10.0));((7.0,4.0),(6.0,2.0));((10.0,3.0),(5.0,5.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test1=skladaj op kartka;;
assert (test1 (9.0,8.0)=0);;
let op=[((9.0,6.0),(3.0,1.0));((5.0,7.0),(9.0,2.0));((2.0,5.0),(8.0,3.0));((2.0,10.0),(4.0,5.0));((0.0,4.0),(6.0,9.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test2=skladaj op kartka;;
assert (test2 (2.0,6.0)=0);;
let op=[((7.0,4.0),(6.0,6.0));((8.0,4.0),(2.0,7.0));((4.0,0.0),(4.0,8.0));((3.0,0.0),(0.0,9.0));((3.0,1.0),(0.0,6.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test3=skladaj op kartka;;
assert (test3 (1.0,8.0)=0);;
let op=[((0.0,2.0),(2.0,10.0));((7.0,8.0),(3.0,8.0));((1.0,3.0),(10.0,7.0));((8.0,7.0),(10.0,8.0));((1.0,6.0),(4.0,3.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test4=skladaj op kartka;;
assert (test4 (6.0,9.0)=0);;
let op=[((9.0,5.0),(10.0,10.0));((10.0,8.0),(0.0,2.0));((2.0,10.0),(0.0,6.0));((5.0,0.0),(0.0,9.0));((10.0,10.0),(7.0,3.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test5=skladaj op kartka;;
assert (test5 (8.0,0.0)=0);;
let op=[((0.0,8.0),(3.0,0.0));((2.0,3.0),(9.0,5.0));((0.0,7.0),(5.0,1.0));((6.0,8.0),(8.0,3.0));((0.0,9.0),(2.0,5.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test6=skladaj op kartka;;
assert (test6 (0.0,2.0)=0);;
let op=[((4.0,7.0),(7.0,7.0));((6.0,10.0),(3.0,9.0));((2.0,2.0),(3.0,5.0));((1.0,3.0),(2.0,8.0));((7.0,0.0),(10.0,2.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test7=skladaj op kartka;;
assert (test7 (10.0,8.0)=0);;
*)
