open Tgl4
open Tsdl

open Gl_utils
open Math_funcs

let main () =
  let window, context = init_scene () in
  let width = ref 640 and height = ref 480 in

	(* get version info *)
  Printf.printf "Renderer: %s\n%!"
    (option_get @@ Gl.get_string Gl.renderer);
  Printf.printf "OpenGL version supported %s\n%!"
    (option_get @@ Gl.get_string Gl.version);
  log_gl_params ();

  (* tell GL to only draw onto a pixel if the shape is closer to the viewer *)
  Gl.enable Gl.depth_test;
  Gl.depth_func Gl.less;

  (*----------------------------create geometry-----------------------------*)
  let points = float_buffer [|
       0.0;  0.5; 0.0;
       0.5; -0.5; 0.0;
      -0.5; -0.5; 0.0
    |] in

  let colours = float_buffer [|
      1.0; 0.0; 0.0;
      0.0; 1.0; 0.0;
      0.0; 0.0; 1.0;
    |] in

  (* a vertex buffer object (VBO) is created here.
     this stores an array of data on the graphics adapter's memory.
     in our case - the vertex points
  *)
  let points_vbo = gen_buffer () in
  Gl.bind_buffer Gl.array_buffer points_vbo;
  Gl.buffer_data Gl.array_buffer (9 * 4) (Some points) Gl.static_draw;

  (* create a second VBO, containing the array of colours.
     note that we could also put them both into a single vertex buffer. in this
     case we would use the pointer and stride parameters of glVertexAttribPointer()
     to describe the different data layouts *)
  let colours_vbo = gen_buffer () in
  Gl.bind_buffer Gl.array_buffer colours_vbo;
  Gl.buffer_data Gl.array_buffer (9 * 4) (Some colours) Gl.static_draw;

  (* create the VAO.
     we bind each VBO in turn, and call glVertexAttribPointer() to indicate where
     the memory should be fetched for vertex shader input variables 0, and 1,
     respectively. we also have to explicitly enable both 'attribute' variables.
     'attribute' is the older name for vertex shader 'in' variables. *)
  let vao = gen_vertex_array () in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer points_vbo;
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
  Gl.bind_buffer Gl.array_buffer colours_vbo;
  Gl.vertex_attrib_pointer 1 3 Gl.float false 0 (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.enable_vertex_attrib_array 1;

  (*------------------------------create shaders--------------------------------*)
  let vs = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source vs (file_contents "test_vs.glsl");
  Gl.compile_shader vs;
  Gl.get_shaderiv vs Gl.compile_status small_buf;
  if Int32.to_int small_buf.{0} <> Gl.true_ then
    (Printf.eprintf "ERROR: GL shader index %d did not compile\n%s\n" vs
       (shader_info_log vs));

  let fs = Gl.create_shader Gl.fragment_shader in
  Gl.shader_source fs (file_contents "test_fs.glsl");
  Gl.compile_shader fs;
  Gl.get_shaderiv fs Gl.compile_status small_buf;
  if Int32.to_int small_buf.{0} <> Gl.true_ then
    (Printf.eprintf "ERROR: GL shader index %d did not compile\n%s\n" fs
       (shader_info_log fs));

  let shader_program = Gl.create_program () in
  Gl.attach_shader shader_program fs;
  Gl.attach_shader shader_program vs;
  Gl.bind_attrib_location shader_program 0 "vertex_position";
  Gl.bind_attrib_location shader_program 1 "vertex_color";
  Gl.link_program shader_program;
  Gl.get_programiv shader_program Gl.link_status small_buf;
  if Int32.to_int small_buf.{0} <> Gl.true_ then
    (Printf.eprintf "ERROR: GL could not link shader program GL index %d\n%s\n"
       shader_program (program_info_log shader_program));

  prerr_endline (program_all_info shader_program);

  (*--------------------------create camera matrices----------------------------*)
  (* create projection matrix *)
  let near = 0.1 in
  let far  = 100.0 in
  let fov  = 67.0 *. one_deg_in_rad in
  let aspect = float_of_int !width /. float_of_int !height in
  let range = tan (fov *. 0.5) *. near in
  let sx = near /. (range *. aspect) in
  let sy = near /. range in
  let sz = -. (far +. near) /. (far -. near) in
  let pz = -. (2.0 *. far *. near) /. (far -. near) in

  let proj_mat = M4.make
       sx 0.0 0.0   0.0 (* first column  *)
      0.0  sy 0.0   0.0 (* second column *)
      0.0 0.0  sz (-1.0) (* third column  *)
      0.0 0.0  pz   0.0 (* fourth column *)
  in

  (* create view matrix *)
  let cam_speed = 1.0 in
  let cam_yaw_speed = 10.0 in
  let cam_pos = ref {V3. x = 0.0; y = 0.0; z = 2.0} in
  let cam_yaw = ref 0.0 in

  let view_mat () =
    let m = M4.(rotate_y_deg identity (-. !cam_yaw) *%
                translate identity V3.(!cam_pos *% -1.)) in
    prerr_endline @@ M4.to_string m;
    m
  in

  let view_mat_location =
    Gl.get_uniform_location shader_program "view" in
  let proj_mat_location =
    Gl.get_uniform_location shader_program "proj" in
  Gl.use_program shader_program;
  Gl.uniform_matrix4fv view_mat_location 1 false (M4.m (view_mat ()));
  Gl.uniform_matrix4fv proj_mat_location 1 false (M4.m proj_mat);

  Gl.enable Gl.cull_face_enum;
  Gl.cull_face Gl.back;
  Gl.front_face Gl.cw;

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
        (* Note: this call is not necessary, but I like to do it anyway before any
           time that I call glDrawArrays() so I never use the wrong shader programme *)
        Gl.use_program shader_program;

        let current_seconds = Unix.gettimeofday () in
        let elapsed_seconds = current_seconds -. !previous_seconds in
        previous_seconds := current_seconds;

        (* Handle input *)
        let check_key =
          let keys = Sdl.get_keyboard_state () in
          fun k -> keys.{k} <> 0
        in
        let cam_moved = ref false in
        let check_cam_key k =
          let result = check_key k in
          if result then cam_moved := true;
          result
        in
        if check_cam_key Sdl.Scancode.a then
          cam_pos := {!cam_pos with V3.x = !cam_pos.V3.x -. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.d then
          cam_pos := {!cam_pos with V3.x = !cam_pos.V3.x +. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.pageup then
          cam_pos := {!cam_pos with V3.y = !cam_pos.V3.y +. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.pagedown then
          cam_pos := {!cam_pos with V3.y = !cam_pos.V3.y -. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.w then
          cam_pos := {!cam_pos with V3.z = !cam_pos.V3.z -. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.s then
          cam_pos := {!cam_pos with V3.z = !cam_pos.V3.z +. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.left then
          cam_yaw := !cam_yaw +. cam_yaw_speed *. elapsed_seconds;
        if check_cam_key Sdl.Scancode.right then
          cam_yaw := !cam_yaw -. cam_yaw_speed *. elapsed_seconds;

        if !cam_moved then
          Gl.uniform_matrix4fv view_mat_location 1 false (M4.m (view_mat ()));

        (* Note: this call is not necessary, but I like to do it anyway before any
           time that I call glDrawArrays() so I never use the wrong vertex data *)
        Gl.bind_vertex_array vao;
        (* draw points 0-3 from the currently bound VAO with current in-use shader *)
        Gl.draw_arrays Gl.triangles 0 3;
        (* put the stuff we've been drawing onto the display *)
        Sdl.gl_swap_window window;
        loop ()
      end
  in
  loop ()

let () = main ()
