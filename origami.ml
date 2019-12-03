(** Punkt na płaszczyźnie *)
type point = float * float

type kartka = point -> int
(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)

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
    if sqrt((x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)) <= r then 1
    else 0
  : kartka)

let square x = x *. x

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
  let value = fun x y -> y *. b +. x *. a +. c in

  let gen a b c x y =
    ((square b -. square a) *. x -. 2.0 *. a *. b *. y -. 2.0 *. a *. c) /. (square a +. square b)
  in

  (fun (x3, y3) ->
    let v = value x3 y3 in
    if v > 0.0 then kar (x3, y3) + kar (gen a b c x3 y3, gen b a c y3 x3)
    else if v = 0.0 then kar (x3, y3)
    else 0
  : kartka)



(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy *)
let skladaj (lista: (point * point) list) (kar: kartka) =
  let rec helper lis k =
    match lista with
    | (p1, p2)::t -> helper t (zloz p1 p2 k)
    | [] -> k
  in helper lista kar
