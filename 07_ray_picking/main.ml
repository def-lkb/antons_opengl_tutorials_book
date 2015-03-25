open Tgl4
open Tsdl

open Gl_utils
open Math_funcs

let rec list_filtermapi f i = function
  | [] -> []
  | x :: xs ->
    match f i x with
    | None -> list_filtermapi f (succ i) xs
    | Some x' -> x' :: list_filtermapi f (succ i) xs

let list_filtermapi f l = list_filtermapi f 0 l

let rec list_filtermap f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> list_filtermap f xs
    | Some x' -> x' :: list_filtermap f xs

let sphere_pos_wor = [
  {V3. x = -2.0; y = 0.0; z = 0.0};
  {V3. x = 2.0; y = 0.0; z = 0.0};
  {V3. x = -2.0; y = 0.0; z = -2.0};
  {V3. x = 1.5; y = 1.0; z = -1.0};
]
let sphere_radius = 1.0

let get_ray_from_mouse proj_mat view_mat width height mx my =
  let ray_clip =
    {V4. x = (2.0 *. mx) /. width -. 1.0;
     y = 1.0 -. (2.0 *. my) /. height;
     z = -1.0;
     w = 1.0;
    } in
	let ray_eye = M4.act (M4.inverse proj_mat) ray_clip in
  let ray_eye = {ray_eye with V4. z = -1.0; w = 0.0} in
	(* world space *)
  let ray_wor = M4.act (M4.inverse view_mat) ray_eye in
  V3.normalize (v3_of_v4 ray_wor)

(* check if a ray and a sphere intersect. if not hit, returns false. it rejects
   intersections behind the ray caster's origin, and sets intersection_distance
   to the closest intersection *)
let ray_sphere origin direction sphere_centre sphere_radius =
  (* work out components of quadratic *)
  let dist_to_sphere = V3.(origin -% sphere_centre) in
  let b = V3.dot direction dist_to_sphere in
  let c = V3.dot dist_to_sphere dist_to_sphere -. sphere_radius *. sphere_radius in
  let b2_c = b *. b -. c in
  (* check for "imaginary" answer. == ray completely misses sphere *)
  if b2_c < 0.0 then
    None
  else if b2_c > 0.0 then
    (* check for ray hitting twice (in and out of the sphere) *)
    let b2_c = sqrt b2_c in
    let t_a = -.b +. b2_c in
    let t_b = -.b -. b2_c in
    match t_a < 0.0, t_b < 0.0 with
    | true, true -> None
    | false, true -> Some t_a
    | _, false -> Some t_b (* t_b always < t_a *)
  else if b2_c = 0.0 then
    let t = -.b +. sqrt b2_c in
    if t >= 0.0 then
      Some t
    else None
  else None

let create_shader filename kind =
  let s = Gl.create_shader kind in
  Gl.shader_source s (file_contents filename);
  Gl.compile_shader s;
  Gl.get_shaderiv s Gl.compile_status small_buf;
  if Int32.to_int small_buf.{0} <> Gl.true_ then
    (Printf.eprintf "ERROR: GL shader index %d did not compile\n%s\n" s
       (shader_info_log s));
  s

let create_program vertex fragment =
  let vs = create_shader vertex Gl.vertex_shader
  and fs = create_shader fragment Gl.fragment_shader in
  let ps = Gl.create_program () in
  Gl.attach_shader ps vs;
  Gl.attach_shader ps fs;
  Gl.bind_attrib_location ps 0 "vertex_position";
  Gl.link_program ps;
  Gl.get_programiv ps Gl.link_status small_buf;
  if Int32.to_int small_buf.{0} <> Gl.true_ then
    (Printf.eprintf "ERROR: GL could not link shader program GL index %d\n%s\n%s\n"
       ps (program_info_log ps) (program_all_info ps));
  assert (validate_program ps);
  Gl.delete_shader vs;
  Gl.delete_shader fs;
  ps

let main () =
  let window, context = init_scene () in
  let width = ref 800 and height = ref 800 in

	(* get version info *)
  Printf.printf "Renderer: %s\n%!"
    (option_get @@ Gl.get_string Gl.renderer);
  Printf.printf "OpenGL version supported %s\n%!"
    (option_get @@ Gl.get_string Gl.version);
  log_gl_params ();

  (*----------------------------CREATE GEOMETRY-----------------------------*)
  let point_count, vp, vt, vn = Obj_parser.load_obj_file "sphere.fobj" in
  let vao = gen_vertex_array () in
  Gl.bind_vertex_array vao;

  let points_vbo = gen_buffer () in
  Gl.bind_buffer Gl.array_buffer points_vbo;
  Gl.buffer_data Gl.array_buffer (3 * point_count * 4) (Some vp) Gl.static_draw;

  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
  Gl.enable_vertex_attrib_array 0;

  let shader_program = create_program "test_vs.glsl" "test_fs.glsl" in
  let view_mat_location = Gl.get_uniform_location shader_program "view" in
  let proj_mat_location = Gl.get_uniform_location shader_program "proj" in
  let model_mat_location = Gl.get_uniform_location shader_program "model" in
  let blue_location = Gl.get_uniform_location shader_program "blue" in

  (*----------------------------create camera-----------------------------*)
  let near = 0.1 and far = 100.0 and fovy = 67.0 in
  let aspect = float_of_int !width /. float_of_int !height in
  let proj_mat = M4.perspective fovy aspect near far in

  let cam_speed = 3.0 in
  let cam_heading_speed = 50.0 in
  let cam_heading = 0.0 in
  let cam_pos = ref {V3.zero with V3.z = 5.0} in

  let fwd0 = {V4.zero with V4.z = -1.} in
  let rgt0 = {V4.zero with V4.x = 1.} in
  let up0  = {V4.zero with V4.y = 1.} in

  let quaternion = ref (Q.from_axis_deg (-. cam_heading) (v3_of_v4 up0)) in
  let view_mat () =
    M4.(inverse (of_quat !quaternion) *% inverse (translate identity (!cam_pos)))
  in

  let fwd = ref fwd0 in
  let rgt = ref rgt0 in
  let up  = ref up0  in

  (* Set rendering defaults *)
  Gl.use_program shader_program;
  Gl.uniform_matrix4fv view_mat_location 1 false (M4.m (view_mat ()));
  Gl.uniform_matrix4fv proj_mat_location 1 false (M4.m proj_mat);

  let model_mats = List.map M4.(translate identity) sphere_pos_wor in

  Gl.enable Gl.depth_test; (* enable depth testing *)
  Gl.depth_func Gl.less; (* depth-testing interprets a smaller value as "closer" *)
  Gl.enable Gl.cull_face_enum; (* cull face *)
  Gl.cull_face Gl.back; (* cull back face *)
  Gl.front_face Gl.ccw; (* ccw vertex order to meant the front *)
  Gl.clear_color 0.2 0.2 0.2 1.0; (* Dark grey background to help spot mistakes *)
  Gl.viewport 0 0 !width !height;

  (* Rendering loop *)
  let event = Sdl.Event.create () in
  let update_fps_counter = fps_counter () in
  let frame_timer = frame_timer 60 in

  let previous_seconds = ref (Unix.gettimeofday ()) in
  let selected = ref None in

  let on_click x y =
    let ray_wor = get_ray_from_mouse proj_mat (view_mat ())
        (float_of_int !width) (float_of_int !height)
        (float_of_int x) (float_of_int y) in
    match list_filtermapi (fun i sphere_pos ->
        match ray_sphere !cam_pos ray_wor sphere_pos sphere_radius with
        | None -> None
        | Some vec -> Some (vec, i))
        sphere_pos_wor
    with
    | [] -> selected := None
    | x :: xs ->
      let dist, index = List.fold_left min x xs in
      selected := Some index
  in

  (* The main event loop *)
  let rec loop () =
    let is_done =
      if Sdl.wait_event_timeout (Some event) (frame_timer ()) then
        match Sdl.Event.(enum (get event typ)) with
        | `Quit -> true
        | `Window_event
          when Sdl.Event.(get event window_event_id = window_event_resized) ->
          let w = Sdl.Event.(get event window_data1) in
          let h = Sdl.Event.(get event window_data2) in
          width := Int32.to_int w;
          height := Int32.to_int h;
          false
        | `Mouse_button_down ->
          let x, y =
            Sdl.Event.(get event mouse_motion_x, get event mouse_motion_y)
          in
          on_click x y;
          false
        | _ -> false
      else
        false
    in
    if is_done then ()
    else
      begin
        update_fps_counter window;
        Gl.clear (Gl.depth_buffer_bit lor Gl.color_buffer_bit);
        Gl.viewport 0 0 !width !height;

        Gl.use_program shader_program;
        Gl.bind_vertex_array vao;

        List.iteri (fun i mat ->
            Gl.uniform1f blue_location (if !selected = Some i then 1.0 else 0.0);
            Gl.uniform_matrix4fv model_mat_location 1 false (M4.m mat);
            Gl.draw_arrays Gl.triangles 0 point_count
          ) model_mats;

        let current_seconds = Unix.gettimeofday () in
        let elapsed_seconds = current_seconds -. !previous_seconds in
        previous_seconds := current_seconds;

        (* Handle input *)
        let check_key =
          let keys = Sdl.get_keyboard_state () in
          fun k -> keys.{k} <> 0
        in
        let mx = ref 0.0 and my = ref 0.0 and mz = ref 0.0 in
        let cam_yaw = ref 0.0 and cam_pitch = ref 0.0 and cam_roll = ref 0.0 in
        let cam_moved = ref false in
        let check_cam_key k m d =
          if check_key k then
            (cam_moved := true; m := !m +. d *. elapsed_seconds)
        in
        check_cam_key Sdl.Scancode.a mx (-. cam_speed);
        check_cam_key Sdl.Scancode.d mx cam_speed;
        check_cam_key Sdl.Scancode.q my cam_speed;
        check_cam_key Sdl.Scancode.e my (-. cam_speed);
        check_cam_key Sdl.Scancode.w mz (-. cam_speed);
        check_cam_key Sdl.Scancode.s mz cam_speed;

        if check_key Sdl.Scancode.left then
          begin
            cam_yaw := !cam_yaw +. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_yaw (v3_of_v4 !up)) !quaternion;
          end;

        if check_key Sdl.Scancode.right then
          begin
            cam_yaw := !cam_yaw -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_yaw (v3_of_v4 !up)) !quaternion;
          end;


        if check_key Sdl.Scancode.up then
          begin
            cam_pitch := !cam_pitch +. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_pitch (v3_of_v4 !rgt)) !quaternion;
          end;

        if check_key Sdl.Scancode.down then
          begin
            cam_pitch := !cam_pitch -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_pitch (v3_of_v4 !rgt)) !quaternion;
          end;

        if check_key Sdl.Scancode.z then
          begin
            cam_roll := !cam_roll -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_roll (v3_of_v4 !fwd)) !quaternion;
          end;

        if check_key Sdl.Scancode.c then
          begin
            cam_roll := !cam_roll +. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_roll (v3_of_v4 !fwd)) !quaternion;
          end;

        if !cam_moved then
          begin
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;

            cam_pos := V3.(!cam_pos
                           +% v3_of_v4 !fwd *% -. !mz
                           +% v3_of_v4 !up  *% !my
                           +% v3_of_v4 !rgt *% !mx);
            Gl.uniform_matrix4fv view_mat_location 1 false (M4.m (view_mat ()))
          end;

        (* put the stuff we've been drawing onto the display *)
        Sdl.gl_swap_window window;
        loop ()
      end
  in
  loop ()

let () = main ()
