(*******************************************************************************
 * Copyright (c) 2010 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum
 *******************************************************************************)

(* Visualizing parse trees *)

(*********************** CALCULATING LAYOUTS ***********************)

type layout = {mutable x:int; mutable y:int; mutable h:int; mutable w:int}
(* trees; leaves are trees with no children *)
type tree = {label:string; layout:layout; mutable children:tree list}

let list_max = List.fold_left max 0
let list_sum = List.fold_left (fun m n -> m + n) 0

let rec heights t =
  (* height of a leaf is 1 *)
  t.layout.h <- 1 + list_max(List.map heights t.children);
  t.layout.h

let rec widths t =
  let w = list_sum(List.map widths t.children) in
  (* assign every leaf a width of 1 *)
  let w = if w=0 then 1 else w in
  t.layout.w <- w;
  t.layout.w

let rec xs t = (* if t.layout.x is set, then set the xs of t's children; must be run after widths *)
  List.fold_left (fun x t2 -> t2.layout.x <- x; ignore(xs t2); x+t2.layout.w) t.layout.x t.children

let rec ys t = (* if t.layout.y is set, then set the ys of t's children; must be run after heights *)
  let y = t.layout.y in
  let h = t.layout.h in
  List.iter (fun t2 -> t2.layout.y <- y + (h - t2.layout.h); ys t2) t.children

let do_layout t =
  t.layout.x <- 0;
  t.layout.y <- 0;
  ignore (heights t);
  ignore (widths t);
  ignore (xs t);
  ys t

let l0() = {x=0;y=0;h=0;w=0}
let t0 l children = {label=l; layout=l0(); children=children}

type extent =
    Start of int
  | End of string * int (* string is nonterminal name *)
type parse_forest = (extent, History.label) History.history
module Memo = History.Make(struct type t = extent let compare = compare
                                                  let hash = Hashtbl.hash
                                                  let memoize = true
                           end)
(* To use an extent history:

   Memo.memoize := true           (* If desired.  Default is false, see history.ml *)
   let h = Viz.Memo.new_history() (* History to pass to parser *)

   On a call, the argument should be h#empty.
   Every callee should immediately do a h#push(Start pos_) to mark the left end.
   Every nonterminal n should do a h#push(End(n,pos_)) to mark the right end.
   On return do a h#merge(Start 0), the value doesn't matter, as our history
   traversal throws it away.

   Here's one that works:

    #!build/yakker exec
    @begin{
    module Viz=Yak.Viz
    let h0 = Viz.Memo.new_history()
    type vhist = Viz.parse_forest
    let m0 = Viz.Start 0 (* throw-away extent used as merge label,
                            discarded by history traversal *)
    }
    S = @{h0#push pos_ (Viz.Start pos_)}@h1
        S1
        !OCTET
        @{h1#merge pos_ m0 h}@h1
        @{h1#push pos_ (Viz.End("S",pos_))}@h1
        @delay(pos_{int})$pos
        @delay(h1{vhist})$parse_forest
          { let input = (Yak.YkBuf.Snapshot.sub ykinput 0 pos) in
            Viz.HTML.pr stdout input parse_forest
          }
    .
    S1>@(;h:vhist) = (@{h0#push pos_ (Viz.Start pos_)}>@h1)
                     A (@{h1#merge pos_ m0 h}>@h1)
                     B (@{h1#merge pos_ m0 h}>@h1)
                     (@{h1#push pos_ (Viz.End("S1",pos_))}>@h)
    .
    A>@(;h:vhist)  = (@{h0#push pos_ (Viz.Start pos_)}>@h1)
                     "a"
                     (@{h1#push pos_ (Viz.End("A",pos_))}>@h)
    ;                            ["a" [C] A]
    .
    B>@(;h:vhist)  = (@{h0#push pos_ (Viz.Start pos_)}>@h1)
                     "b"
                     (@{h1}>@h) ; for "" case of option below
                     [
                      B (@{h1#merge pos_ m0 h}>@h1)
                     ]
                     (@{h1#push pos_ (Viz.End("B",pos_))}>@h)
    .

 *)

let mk_t input (pf:parse_forest) =
  (* utilities for building leaves and sequences of leaves *)
  let epsilon() = t0 "" [] in
  let leaf i =
    t0 (Printf.sprintf "%c" (String.get input i)) [] in
  let span left right =
    if left>=right then [] else
    let leaves = ref [] in
    for i = right - 1 downto left do
      leaves := (leaf i)::!leaves
    done;
    !leaves in
  let traverse = pf#left_to_right in    (* Postfix traversal, gives sequence of extents *)
  let rec symbols left children =          (* Convert traversal to layout tree *)
    try
      match traverse#next() with
      | Start left' ->
          symbols left ((symbols left' [])::children)
      | End(n,right) ->
          ({label = n;
            layout = { x=left; w=right-left; y=0; h=0 };
            children = List.rev children})
    with Not_found -> (* at end of traversal *)
      (match children with [t] -> t
      | _ -> failwith "Impossible Viz.mk_t")
  in
  (* given a list of symbol trees and an extent, fill out the list of trees with additional leaves of input not covered *)
  let rec add_leaves left right =
    function [] ->
      span left right
      | hd::tl ->
          if hd.children=[] && hd.layout.w=0 then hd.children <- [epsilon()] else
          hd.children <- add_leaves hd.layout.x (hd.layout.x+hd.layout.w) hd.children;
          (span left hd.layout.x)@(hd::(add_leaves (hd.layout.x+hd.layout.w) right tl)) in
  let t = symbols 0 [] in
  let left = t.layout.x in
  let right = t.layout.w+left in
  add_leaves left right [t]

(***************************** PRINTING *****************************)

(* colors *)
let hsvtorgb3 h s v =
    (* h(ue) in [0,360) *)
    (* s(aturation) in [0,1.0]*)
    (* v(alue) in [0,1.0] *)
    let h = h mod 360 in
    let h = (float_of_int h) /. 60.0 in (* h in [0,6) *)
    let i = floor h in
    let f = h -. i in (* f in [0,1) *)
    let p = v *. (1.0 -. s) in
    let q = v *. (1.0 -. (s *. f)) in
    let t = v *. (1.0 -. (s *. (1.0 -. f))) in
    let r,g,b =
      match (int_of_float i) with
      | 0 -> (v, t, p)
      | 1 -> (q, v, p)
      | 2 -> (p, v, t)
      | 3 -> (p, q, v)
      | 4 -> (t, p, v)
      | _ -> (v, p, q) in
    let r = int_of_float(floor(r *. 255.0)) in
    let g = int_of_float(floor(g *. 255.0)) in
    let b = int_of_float(floor(b *. 255.0)) in
    (r,g,b)

let hsvtorgb h s v =
  let (r,g,b) = hsvtorgb3 h s v in
  ((r lsl 16) lor (g lsl 8) lor b)

let random() =
    hsvtorgb (Random.int 360) 1.0 1.0

let random3() =
    hsvtorgb3 (Random.int 360) 1.0 1.0

let multiplier = 30.0

(* text *)
let printable c = (' '<= c && c <= '~')

module Svg = struct
let escape_char = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | c ->
        if printable c then Printf.sprintf "%c" c
        else Printf.sprintf "%.2x" (Char.code c)

let escape s =
  let b = Buffer.create 11 in
  for i = 0 to String.length s - 1 do
    Buffer.add_string b (escape_char (String.get s i))
  done;
  Buffer.contents b

let pr_text f t =
  if t.label="" && t.layout.w=1 then
    Printf.fprintf f "<text x='%g' y='%g' style='fill:#888888'>&#949;</text>\n"
      ((float(t.layout.x) +. 0.25) *. multiplier)
      (((float t.layout.y) +. 0.75) *. multiplier)
  else
    let n = min (String.length t.label) t.layout.w in
    for i = 0 to n - 1 do
      let c = String.get t.label i in
      if printable c then
        Printf.fprintf f "<text x='%g' y='%g'>%s</text>\n"
          ((float(t.layout.x + i) +. 0.25) *. multiplier)
          (((float t.layout.y) +. 0.75) *. multiplier)
          (escape_char c)
      else
        Printf.fprintf f "<text x='%g' y='%g' style='font-size:%gpx'>%s</text>\n"
          ((float(t.layout.x + i) +. 0.15) *. multiplier)
          (((float t.layout.y) +. 0.7) *. multiplier)
          (0.5 *. multiplier)
          (escape_char c)
    done

let pr_layout f t =
  do_layout t;
  Printf.fprintf f "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n";
  Printf.fprintf f "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    ((float t.layout.w) *. multiplier) ((float t.layout.h) *. multiplier);
  Printf.fprintf f
"<defs><style type=\"text/css\"><![CDATA[
  text { font-size:%gpx; font-family:Courier }
]]></style>
</defs>
" (0.8 *. multiplier);
  let rec loop t =
    let color = if t.layout.h=1 then 0xeeeeee else random() in
    Printf.fprintf f "<rect x='%g' y='%g' width='%g' height='%g' style='fill:#%06x'/>\n"
      ((float t.layout.x) *. multiplier)
      ((float t.layout.y) *. multiplier)
      ((float t.layout.w) *. multiplier)
      ((float t.layout.h) *. multiplier)
      color;
    pr_text f t;
    List.iter loop t.children in
  loop t;
  Printf.fprintf f "</svg>\n";
  ()

let pr outch input pf =
  let t = match mk_t input pf with [t] -> t
  | ts ->
      Util.warn Util.Sys_warn "internal error: parse tree without a root in viz.ml";
      t0 "<root>" ts in
  pr_layout outch t
end

module Clutter = struct
let escape_char = function
    | '\"' -> "\\\""
    | '\'' -> "\\\'"
    | '\\' -> "\\\\"
    | c ->
        if printable c then Printf.sprintf "%c" c
        else Printf.sprintf "\\x%.2x" (Char.code c)
let escape s =
  let b = Buffer.create 11 in
  for i = 0 to String.length s - 1 do
    Buffer.add_string b (escape_char (String.get s i))
  done;
  Buffer.contents b
let bigfont = Printf.sprintf "Courier %g" (0.8 *. multiplier)
let smallfont = Printf.sprintf "Courier %g" (0.5 *. multiplier)
let tinyfont = Printf.sprintf "Courier %g" (0.4 *. multiplier)
let pr_text f t =
  if t.label="" && t.layout.w=1 then begin
    Printf.fprintf f
"text = new Clutter.Text({text:'eps', font_name:'%s', color:black, x:%g, y:%g});
text.set_anchor_point_from_gravity(Clutter.Gravity.NORTH_WEST);
"
      tinyfont
      ((float(t.layout.x) +. 0.05) *. multiplier)
      (((float t.layout.y) -. 0.15) *. multiplier);
    Printf.fprintf f "stage.add_actor(text);\n"
  end
  else
    let n = min (String.length t.label) t.layout.w in
    for i = 0 to n - 1 do
      let c0 = String.get t.label i in
      let c = escape_char c0 in
      if not(printable c0) then begin
        Printf.fprintf f
"text = new Clutter.Text({text:'%s', font_name:'%s', color:black, x:%g, y:%g});
text.set_anchor_point_from_gravity(Clutter.Gravity.NORTH_WEST);
"
          c
          smallfont
          ((float(t.layout.x + i) +. 0.15) *. multiplier)
          (((float t.layout.y) +. 0.15) *. multiplier);
      end
      else
        Printf.fprintf f
"text = new Clutter.Text({text:'%s', font_name:'%s', color:black, x:%g, y:%g});
text.set_anchor_point_from_gravity(Clutter.Gravity.NORTH_WEST);
"
          c
          bigfont
          ((float(t.layout.x + i) +. 0.15) *. multiplier)
          (((float t.layout.y) -. 0.05) *. multiplier);
      Printf.fprintf f "stage.add_actor(text);\n"
    done

let pr_util_functions f =
  Printf.fprintf f "var multiplier = %g;" multiplier;
  Printf.fprintf f "%s" "
function hsvtorgb(h,s,v) {
  var r, g, b;
  h %= 360;
  h /= 60;
  var i = Math.floor(h) % 6;
  var f = h-i;
  var p = v*(1-s);
  var q = v*(1-(s*f));
  var t = v*(1-(s*(1-f)));
  switch(i) {
  case 0: r=v; g=t; b=p;
  case 1: r=q; g=v; b=p;
  case 2: r=p; g=v; b=t;
  case 3: r=p; g=q; b=v;
  case 4: r=t; g=p; b=v;
  case 5: r=v; g=p; b=q;
  }
  var R = Math.round(r*255);
  var G = Math.round(g*255);
  var B = Math.round(b*255);
  var rgb = {r:R, g:G, b:B};
  return rgb;
}
function random() {
  return hsvtorgb(Math.random()*360,1.0,1.0);
}
function printable(c) { return (' ' <= c && c <= '~'); }
";
  ()

let pr_tree f t =
  Printf.fprintf f "var t0=";
  let rec loop t =
    Printf.fprintf f "{label:'%s',layout:{x:%d,y:%d,w:%d,h:%d},\nchildren:["
      (escape t.label)
      t.layout.x
      t.layout.y
      t.layout.w
      t.layout.h;
    (match t.children with [] -> ()
    | [hd] -> loop hd
    | hd::tl -> loop hd; List.iter (fun t2 -> Printf.fprintf f ","; loop t2) tl);
    Printf.fprintf f "]}"
  in
  loop t;
  Printf.fprintf f ";\n";
  ()

let pr_layout f t =
  do_layout t;
  Printf.fprintf f "%s" "#!/usr/bin/env seed
Clutter = imports.gi.Clutter;
Clutter.init(Seed.argv);
";
  pr_util_functions f;
  Printf.fprintf f "%s" "
var bigfont = 'Courier ' + (0.8*multiplier);
var smallfont = 'Courier ' + (0.5*multiplier);
var tinyfont = 'Courier ' + (0.4*multiplier);
function render_text(t) {
  if (t.label=='' && t.layout.w==1) {
     var text =
       new Clutter.Text({text:'eps', font_name:tinyfont, color:black,
                         x:(t.layout.x + 0.05) * multiplier,
                         y:(t.layout.y + 0.15) * multiplier});
     text.set_anchor_point_from_gravity(Clutter.Gravity.NORTH_WEST);
     stage.add_actor(text);
  }
  else {
     var n = Math.min(t.label.length,t.layout.w);
     for (var i = 0; i<n; i++) {
       var c0 = t.label[i];
       if (printable(c0)) {
         var text =
           new Clutter.Text({text:c0, font_name:bigfont, color:black,
                             x:(t.layout.x + i + 0.15) * multiplier,
                             y:(t.layout.y - 0.05) * multiplier});
         text.set_anchor_point_from_gravity(Clutter.Gravity.NORTH_WEST);
         stage.add_actor(text);
       }
       else {
         var text =
           new Clutter.Text({text:'xx', font_name:smallfont, color:black,
                             x:(t.layout.x + i + 0.15) * multiplier,
                             y:(t.layout.y + 0.15) * multiplier});
         text.set_anchor_point_from_gravity(Clutter.Gravity.NORTH_WEST);
         stage.add_actor(text);
       }
     }
  }
}
function render(t) {
  var clr;
  if (1 == t.layout.h) {
    clr = {r:0xee,g:0xee,b:0xee};
  }
  else {
    clr = random();
  }
  var rect = new Clutter.Rectangle({color:new Clutter.Color({red:clr.r,green:clr.g,blue:clr.b,alpha:255}),
                                    x:t.layout.x*multiplier,y:t.layout.y*multiplier,width:t.layout.w*multiplier,height:t.layout.h*multiplier});
  stage.add_actor(rect);
  render_text(t);
  for (var i = 0; i < t.children.length; i++) {
    render(t.children[i]);
  }
}
var stage = Clutter.Stage.get_default();
stage.signal.hide.connect(Clutter.main_quit);
var black = new Clutter.Color({alpha:255});
stage.color = black;
";
  Printf.fprintf f "stage.width=%d*multiplier; stage.height=%d*multiplier; stage.show_all();\n"
    t.layout.w t.layout.h;
  pr_tree f t;
  Printf.fprintf f "render(t0);\nClutter.main();\n";
  ()

let pr outch input pf =
  let t = match mk_t input pf with [t] -> t
  | ts ->
      Util.warn Util.Sys_warn "internal error: parse tree without a root in viz.ml";
      t0 "<root>" ts in
  pr_layout outch t
end

module Canvas = struct
let escape_char = function
    | '\"' -> "\\\""
    | '\'' -> "\\\'"
    | c ->
        if printable c then Printf.sprintf "%c" c
        else Printf.sprintf "%.2x" (Char.code c)
let escape s =
  let b = Buffer.create 11 in
  for i = 0 to String.length s - 1 do
    Buffer.add_string b (escape_char (String.get s i))
  done;
  Buffer.contents b
let bigfont = Printf.sprintf "%gpx Courier" (0.8 *. multiplier)
let smallfont = Printf.sprintf "%gpx Courier" (0.5 *. multiplier)
let tinyfont = Printf.sprintf "%gpx Courier" (0.4 *. multiplier)
let pr_text f t =
  Printf.fprintf f "c.fillStyle='black';\n";
  if t.label="" && t.layout.w=1 then begin
    Printf.fprintf f "c.font = '%s';\n" tinyfont;
    Printf.fprintf f "c.fillText('eps',%g,%g);\n"
      ((float(t.layout.x) +. 0.15) *. multiplier)
      (((float t.layout.y) +. 0.6) *. multiplier);
    Printf.fprintf f "c.font = '%s';\n" bigfont
  end
  else
    let n = min (String.length t.label) t.layout.w in
    for i = 0 to n - 1 do
      let c0 = String.get t.label i in
      let c = escape_char c0 in
      if not(printable c0) then begin
        Printf.fprintf f "c.font = '%s';\n" smallfont;
        Printf.fprintf f "c.fillText('%s',%g,%g);\n"
          c
          ((float(t.layout.x + i) +. 0.15) *. multiplier)
          (((float t.layout.y) +. 0.65) *. multiplier);
        Printf.fprintf f "c.font = '%s';\n" bigfont;
      end
      else
        Printf.fprintf f "c.fillText('%s',%g,%g);\n"
          c
          ((float(t.layout.x + i) +. 0.25) *. multiplier)
          (((float t.layout.y) +. 0.75) *. multiplier);
    done

let pr_layout f t =
  do_layout t;
  Printf.fprintf f "<html>\n<body>\n";
  Printf.fprintf f "<canvas id=\"a\" width=\"%g\" height=\"%g\"></canvas>\n"
    ((float t.layout.w) *. multiplier) ((float t.layout.h) *. multiplier);
  Printf.fprintf f "
<script>
var c = document.getElementById(\"a\").getContext(\"2d\");
c.font = '%s';
" bigfont;
  Clutter.pr_util_functions f;
  Printf.fprintf f "%s" "
var bigfont =   (0.8*multiplier) + 'px Courier';
var smallfont = (0.5*multiplier) + 'px Courier';
var tinyfont =  (0.4*multiplier) + 'px Courier';
function render_text(t) {
  c.fillStyle='black';
  if (t.label=='' && t.layout.w==1) {
     c.font = tinyfont;
     c.fillText('eps',(t.layout.x + 0.15) * multiplier,(t.layout.y + 0.6) * multiplier);
     c.font = bigfont;
  }
  else {
     var n = Math.min(t.label.length,t.layout.w);
     for (var i = 0; i<n; i++) {
       var c0 = t.label[i];
       if (printable(c0)) {
         c.fillText(c0,(t.layout.x + i + 0.25) * multiplier,(t.layout.y + 0.75) * multiplier);
       }
       else {
         c.font = smallfont;
         c.fillText('xx',(t.layout.x + i + 0.15) * multiplier,(t.layout.y + 0.65) * multiplier);
         c.font = bigfont;
       }
     }
  }
}
function render(t) {
  var clr;
  if (1 == t.layout.h) {
    clr = '#eeeeee';
  }
  else {
    var rgb = random();
    clr = 'rgb('+rgb.r+','+rgb.g+','+rgb.b+')'
  }
  c.fillStyle=clr;
  c.fillRect(t.layout.x*multiplier,
             t.layout.y*multiplier,
             t.layout.w*multiplier,
             t.layout.h*multiplier);
  render_text(t);
  for (var i = 0; i < t.children.length; i++) {
    render(t.children[i]);
  }
}
";
  Clutter.pr_tree f t;
  Printf.fprintf f "render(t0);\n";
  Printf.fprintf f "</script>\n</body>\n</html>\n";
  ()

let pr outch input pf =
  let t = match mk_t input pf with [t] -> t
  | ts ->
      Util.warn Util.Sys_warn "internal error: parse tree without a root in viz.ml";
      t0 "<root>" ts in
  pr_layout outch t
end

(* Javascript InfoVis Toolkit http://thejit.org/ *)
module JIT = struct
let pr_tree f t =
  Printf.fprintf f "var t0=";
  let id = ref 0 in
  let rec loop t =
    Printf.fprintf f "{id:'%d',name:'%s',data:{},\nchildren:["
      (Util.postincr id)
      (Clutter.escape t.label);
    (match t.children with [] -> ()
    | [hd] -> loop hd
    | hd::tl -> loop hd; List.iter (fun t2 -> Printf.fprintf f ","; loop t2) tl);
    Printf.fprintf f "]}"
  in
  loop t;
  Printf.fprintf f ";\n";
  ()
let pr_layout f t =
  pr_tree f t
let pr outch input pf =
  let t = match mk_t input pf with [t] -> t
  | ts ->
      Util.warn Util.Sys_warn "internal error: parse tree without a root in viz.ml";
      t0 "<root>" ts in
  pr_layout outch t
end

module HTML = struct
let escape_char = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | c ->
        if printable c then Printf.sprintf "%c" c
        else Printf.sprintf "<u><small>%.2x</small></u>" (Char.code c)
let escape_maybe_line_end s i =
  let cr_ends_line = false in (* usually a good choice *)
  let c = String.get s i in
  match c with
(* This case in practice is not useful because s is usually a substring that contains just
   a carraige return, and some other substring includes the linefeed.
  | '\r' when (i+1 < String.length s) && ('\n' = String.get s (i+1)) ->
      Printf.sprintf "%.2x" (Char.code c)
*)
  | '\r' when cr_ends_line -> Printf.sprintf "%s<br/>" (escape_char c)
  | '\n' -> Printf.sprintf "%s<br/>" (escape_char c)
  | _ -> escape_char c
let escape s =
  let b = Buffer.create 11 in
  for i = 0 to String.length s - 1 do
    Buffer.add_string b (escape_maybe_line_end s i)
  done;
  Buffer.contents b
let pr_tree f t =
  Printf.fprintf f "<html><body>\n";
  Printf.fprintf f "<p>Navigation: h=left j=down k=up l=right</p>\n";
  Printf.fprintf f "<div id='rule'>&gt;</div><div id='tree'>";
  let rec loop t =
    Printf.fprintf f "";
    (match t.children with
    | [] when t.label = "" ->
        Printf.fprintf f "<span><u><small>&epsilon;</u></small></span>"
    | [] ->
        Printf.fprintf f "<span>%s</span>" (escape t.label)
    | l ->
        Printf.fprintf f "<span class='%s'>" (escape t.label);
        List.iter loop l;
        Printf.fprintf f "</span>") in
  loop t;
  Printf.fprintf f "</div>
<script>
var colr = '#88e';
var rule = document.getElementById('rule');
var top = document.getElementById('tree').firstElementChild;
var elem = top;
elem.style.backgroundColor = colr;
msg(elem.className);
function msg(s) {
  rule.innerHTML = '&gt; ' + s;
}
function down() {
  var e = elem.firstElementChild;
  if (!focus(e)) msg('Cannot go down');
  return false;
}
function up() {
  var e = null;
  if (elem != top) e = elem.parentNode;
  if (!focus(e)) msg('Cannot go up');
  return false;
}
function left() {
  if (elem == top) return false;
  var e = elem.previousElementSibling;
  if (!e) e = (elem == top ? top : elem.parentNode).lastElementChild;
  if (!focus(e)) msg('Cannot go left');
  return false;
}
function right() {
  if (elem == top) return false;
  var e = elem.nextElementSibling;
  if (!e) e = (elem == top ? top : elem.parentNode).firstElementChild;
  if (!focus(e)) msg('Cannot go right');
  return false;
}
function focus(e) {
  if (e) {
    elem.style.backgroundColor = null;
    elem = e;
    msg(elem.className);
    elem.style.backgroundColor = colr;
//  elem.scrollIntoView();
    return true;
  }
  else return false;
}
document.onkeydown = function (evt) {
  var key = evt.keyCode;
  if (key == 72) left();       // h
  else if (key == 75) up();    // k
  else if (key == 76) right(); // l
  else if (key == 74) down();  // j
};
</script>
</body></html>
";
  ()
let pr_layout f t =
  pr_tree f t
let pr outch input pf =
  let t = match mk_t input pf with [t] -> t
  | ts ->
      Util.warn Util.Sys_warn "internal error: parse tree without a root in viz.ml";
      t0 "<root>" ts in
  pr_layout outch t
end
