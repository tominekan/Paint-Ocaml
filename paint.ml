(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; p1: point; p2: point; thickness: bool}
  | Points of {color: color; points: point list; thickness: bool}
  | Ellipse of 
    {color: color; midpoint: point; rx: int; ry: int; thickness: bool}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode of point list
  | EllipseMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *) 
  mutable preview: shape option;

  mutable is_thick: bool;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 *)
  preview = None;
  is_thick = false
}



(** This function creates a graphics context with the appropriate
    pen color. *)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (thickness: bool): gctx =
  let g = with_color g c in
  let g_thicc = with_thickness g thickness in
  g_thicc


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points (with_params g p.color p.thickness) p.points
      | Ellipse e -> 
        draw_ellipse (with_params g e.color e.thickness) e.midpoint e.rx e.ry
    end in

    (*draws a line preview*)
  let draw_preview (s: shape option) : unit = 
    begin match s with
    | Some shape -> 
      begin match shape with 
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points (with_params g p.color p.thickness) p.points
      | Ellipse e -> 
        draw_ellipse (with_params g e.color e.thickness) e.midpoint e.rx e.ry
      end
    | None -> ()
    end in
  (
  Deque.iterate draw_shape paint.shapes;
  draw_preview paint.preview;
  )

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint

(*Get the dimensions of the ellipse from two points*)
type ellipse_data = {midpoint: point; rx: int; ry: int}
let get_ellipse_data (p1: point) (p2: point): ellipse_data =
  let (x1, y1) = p1 in 
  let (x2, y2) = p2 in
  let dx = (x2 - x1)/2 in 
  let dy = (y2 - y1)/2 in
  {
    midpoint = (x1+dx, y1+dy);
    rx = abs dx;
    ry = abs dy;
  }

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
               so change it to LineEndMode, recording the starting point of
               the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
               so create the line and add it to the deque of shapes. Go back
               to waiting for the first click. *)
            Deque.insert_tail
              (Line {
              color=paint.color; 
              p1=p1; 
              p2=p; 
              thickness=(paint.is_thick)}) paint.shapes;
            paint.mode <- LineStartMode
          | PointMode _ -> paint.mode <- PointMode ([p])
          (*
          If the mode is in EllipseMode, set the anchor point as the 
          point where the mouse was held down. 
          *)
          | EllipseMode _-> paint.mode <- EllipseMode p
       end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. Initially there is nothing to do, but you'll
         need to update this part for Task 2, 3, 4 and maybe 6. *)
      begin match paint.mode with 
      | LineEndMode p1 -> paint.preview <- 
        Some (Line {
        color=paint.color;
        p1=p1;
        p2=p;
        thickness=paint.is_thick
        })

      (*If we are dragging in PointMode, we not only update the preview, but 
      we also update paint.mode to add the current point at which
      we are currently dragging the point.
      *)
      | PointMode p_list -> paint.preview <- 
        Some (Points {
        color=paint.color; 
        points=p_list @ [p];
        thickness=paint.is_thick;
        });
        paint.mode <-
           PointMode (p_list @ [p])

      (*If dragging in PointMode, then keep drawing ellipses using our 
      original anchor point as the anchor point *)
      | EllipseMode anchor -> paint.preview <- (
        let e = (get_ellipse_data anchor p) in
        (Some (Ellipse {
        color=paint.color;
        midpoint=e.midpoint; 
        rx=e.rx;
        ry=e.ry;
        thickness=paint.is_thick;
        })) )
      | _ -> ()
      end
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2,
         3, 4, and possibly 6 need to do something different here. *)
      (
      (*Here we add the drawing (line) to the canvas*)
      begin match paint.mode with 
      | LineEndMode p1 -> (Deque.insert_tail
              (Line {
              color=paint.color;
              p1=p1;
              p2=p;
              thickness=paint.is_thick;
              }) paint.shapes;
            paint.mode <- LineStartMode;
            paint.preview <- None;
            )
      (*We are adding all previous points in p_list and adding the current
      point. We also clear the value of paint.preview  *)
      | PointMode p_list -> ( Deque.insert_tail 
        (Points {
        color=paint.color;
        points=p_list @ [p];
        thickness=paint.is_thick;
        }) paint.shapes; 

        paint.mode <- PointMode [];

        paint.preview <- None
      )
      (*Add the ellipse with points anchor and current mouse to canvas*)
      | EllipseMode anchor ->  (Deque.insert_tail 
        ( let e = (get_ellipse_data anchor p) in
        Ellipse {
        color=paint.color;
        midpoint=e.midpoint;
        rx=e.rx;
        ry=e.ry;
        thickness=paint.is_thick;
        } )
         paint.shapes;

         paint.preview <- None;
      )
      | _ -> ()
      end
      )
    
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 asks
   you to make improvements to make the layout more appealing. You may
   choose to arrange the buttons and other GUI elements of the paint
   program however you like, so long as it is easily apparent how to
   use the interface; the sample screenshot in the homework
   description shows one possible design. Also, feel free to improve
   the visual components of the GUI; for example, our solution puts
   borders around the buttons and uses a custom "color button" that
   changes its appearance based on whether or not the color is
   currently selected. *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(*Create the Line Button*)
let (w_line, lc_line, nc_line) = button "Line"

(*Create the Points Button*)
let (w_points, lc_points, nc_points) = button "Points"

let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

(*Create the Thickness checkbox*)
let (w_thbox, vc_thbox) = checkbox false "Thick"

(*Create a the color sliders*)
let (w_red, vc_red) =  slider 0 (0, 255) red
let (w_green, vc_green) = slider 0 (0, 255) green
let (w_blue, vc_blue) = slider 0 (0, 255) blue

let color_pickers = vlist [w_red; 
  space (0, 10);
  w_green; 
  space (0, 10); 
  w_blue;
  ]

(* TODO: You need to modify this in Task 3 and 4, and potentially in
   Task 2 (depending on your implementation). *)
let set_mode (new_mode: mode) : unit -> unit = 
  fun () -> (paint.mode <- new_mode)

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

(*Updates the thickness of lines drawn*)
let set_thickness (is_thick: bool): unit = 
  paint.is_thick <- is_thick;


;; nc_undo.add_event_listener (mouseclick_listener undo)
;; nc_line.add_event_listener (mouseclick_listener (set_mode LineStartMode))
;; nc_points.add_event_listener (mouseclick_listener (set_mode (PointMode []) ))
;; nc_ellipse.add_event_listener 
  (mouseclick_listener (set_mode (EllipseMode (0, 0))))
;; vc_thbox.add_change_listener (set_thickness)
;; vc_red.add_change_listener (fun (red: int) -> paint.color <- 
  {paint.color with r = red})
;; vc_green.add_change_listener (fun (green: int) -> paint.color <- 
  {paint.color with g = green})
;; vc_blue.add_change_listener (fun (blue: int) -> paint.color <- 
  {paint.color with b = blue})

(** A spacer widget *)
let spacer : widget = space (10,10)


(** The mode toolbar, initially containing just the Undo button.
    TODO: you will need to modify this widget to add more buttons
    to the toolbar in Task 1, 3, 4, 5, and possibly 6. *)
let mode_toolbar : widget = 
  hlist [border w_undo; spacer; 
    border w_line; spacer; 
    border w_points; spacer;
    border w_ellipse; spacer;
    border w_thbox]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with; they are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)
  let color_widgets: widget list = [
    color_indicator;
    space (15, 0);
    color_button black; 
    color_button white;
    color_button red;
    color_button green;
    color_button blue;
    color_button yellow;
    color_button cyan;
    space (15, 0);
    color_pickers;
  ]
   let color_toolbar : widget = hlist color_widgets

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
  let all_mainwidgets = [paint_canvas; 
  space (0, 10);
  mode_toolbar; spacer; color_toolbar;] in
  vlist all_mainwidgets


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
