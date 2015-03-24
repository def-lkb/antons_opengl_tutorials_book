open Tgl4
open Tsdl

open Gl_utils
open Math_funcs

let sphere_pos_mat = [|
  {V3. x = -2.0; y = 0.0; z = 0.0};
  {V3. x = 2.0; y = 0.0; z = 0.0};
  {V3. x = -2.0; y = 0.0; z = -2.0};
  {V3. x = 1.5; y = 1.0; z = -1.0};
|]

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

  (*----------------------------create camera-----------------------------*)
  let near = 0.1 and far = 100.0 and fovy = 67.0 in
  let aspect = float_of_int !width /. float_of_int !height in
  let proj_mat = M4.perspective fovy aspect near far in

  let cam_speed = 5.0 in
  let cam_heading_speed = 100.0 in
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

  let model_mats = Array.map M4.(translate identity) sphere_pos_mat in

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
        Array.iter (fun mat ->
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
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;
          end;

        if check_key Sdl.Scancode.right then
          begin
            cam_yaw := !cam_yaw -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_yaw (v3_of_v4 !up)) !quaternion;
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;
          end;


        if check_key Sdl.Scancode.up then
          begin
            cam_pitch := !cam_pitch +. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_pitch (v3_of_v4 !rgt)) !quaternion;
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;
          end;

        if check_key Sdl.Scancode.down then
          begin
            cam_pitch := !cam_pitch -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_pitch (v3_of_v4 !rgt)) !quaternion;
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;
          end;

        if check_key Sdl.Scancode.z then
          begin
            cam_roll := !cam_roll -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_roll (v3_of_v4 !fwd)) !quaternion;
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;
          end;

        if check_key Sdl.Scancode.c then
          begin
            cam_roll := !cam_roll -. cam_heading_speed *. elapsed_seconds;
            cam_moved := true;
            (* create a quaternion representing change in heading (the yaw) *)
            quaternion := Q.(%*%) (Q.from_axis_deg !cam_roll (v3_of_v4 !fwd)) !quaternion;
            let r = M4.of_quat !quaternion in
            fwd := M4.act r fwd0;
            rgt := M4.act r rgt0;
            up  := M4.act r up0;
          end;

        if !cam_moved then
          begin
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
