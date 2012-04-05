open Graphics
open Unix

type ('a, 'b) tree = Leaf of 'b
                   | Node of 'a * ('a, 'b) tree * ('a, 'b) tree

type split_direction = Vertical | Horizontal

type child_layout = {ratio: float; split: split_direction}

let random_item lst =
  List.nth lst (Random.int (List.length lst))

let bound x low high =
  min (max x low) high

let random_split vertprob =
  if Random.int 100 <= vertprob then Vertical else Horizontal

let adjust_vertprob vertprob split =
  bound (match split with | Vertical -> vertprob / 2 | Horizontal -> vertprob * 2) 1 100

let partition xs =
  List.fold_left (fun (ys, zs) x -> (zs, x :: ys)) ([], []) xs

let ratios = [0.5; 0.333; 0.666; 0.618; 0.382]

let rec make_gridtree vertprob colors depth =
  match (depth, colors) with
    | (1, color :: _) -> Leaf (color)
    | _ ->
        let randsplit                   = random_split vertprob in
        let vertprob                    = adjust_vertprob vertprob randsplit in
        let (left_colors, right_colors) = partition colors in
          Node ({ratio = random_item ratios; split = randsplit},
                make_gridtree vertprob left_colors (depth - 1),
                make_gridtree vertprob right_colors (depth - 1))

let scale ratio x =
  let fx = float_of_int x in
    (int_of_float (floor (ratio *. fx)), int_of_float (ceil ((1.0 -. ratio) *. fx)))

let rec draw_gridtree x y width height border_width = function
  | Leaf (color) ->
      set_color color;
      fill_rect x y width height
  | Node ({ratio = ratio; split = split}, l, r) ->
      match split with
        | Vertical ->
            let (left_width, right_width) = scale ratio (width - border_width) in
              draw_gridtree x y left_width height border_width l;
              draw_gridtree (x + border_width + left_width) y right_width height border_width r
        | Horizontal ->
            let (top_height, bottom_height) = scale ratio (height - border_width) in
              draw_gridtree x y width bottom_height border_width l;
              draw_gridtree x (y + border_width + bottom_height) width top_height border_width r

let rec range n d =
  if n <= d then n :: range (n + 1) d else []

let factors n =
  List.map (fun x -> (x, n / x)) (List.filter (fun x -> n mod x = 0) (range 1 n))

let rec ntimes n x =
  if n = 0 then [] else x :: ntimes (n - 1) x

let make_factors_colors color1 color2 n =
  if n != 0 then
    let (p, q) = random_item (factors n) in
      ntimes p color1 @ ntimes q color2
  else
    []

let make_time_colors t =
  let minute_colors = make_factors_colors blue yellow (t.tm_min / 10) in
  let hour_colors   = make_factors_colors red black (t.tm_hour mod 6) in
    minute_colors @ hour_colors

let depth = 5

let leaves = 1 lsl (depth - 1)

let permute lst =
  List.sort (fun _ _ -> Random.int 3 - 1) lst

let make_time_tree () =
  let time_colors  = make_time_colors (Unix.localtime (Unix.time ())) in
  let colors       = permute (ntimes (leaves - List.length time_colors) white @ time_colors) in
    make_gridtree 50 colors depth

let rec draw time_tree =
  let (width, height) = (size_x (), size_y ()) in
    set_color black;
    fill_rect 0 0 width height;
    let border_width = width / 100 in
      draw_gridtree 0 0 width height border_width time_tree

let rec update time_tree ttl =
  let (time_tree, ttl) = if ttl > 0 then (time_tree, ttl) else (make_time_tree (), 60) in
    draw time_tree;
    let startt = time () in
    let _      = sleep ttl in
    let endt   = time () in
    let ttl    = ttl - int_of_float (endt -. startt) in
    let key    = if key_pressed () then Some (read_key ()) else None in
      match key with
        | Some 'q' -> ()
        | Some 'r' -> update time_tree 0
        | _        -> update time_tree ttl

let _ =
  Random.self_init ();
  open_graph "";
  clear_graph ();
  update (make_time_tree ()) 60
