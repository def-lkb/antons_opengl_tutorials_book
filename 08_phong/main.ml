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

  let normals = float_buffer [|
      0.0; 0.0; 1.0;
      0.0; 0.0; 1.0;
      0.0; 0.0; 1.0;
    |] in

  let points_vbo = gen_buffer () in
  Gl.bind_buffer Gl.array_buffer points_vbo;
  Gl.buffer_data Gl.array_buffer (9 * 4) (Some points) Gl.static_draw;

  let normals_vbo = gen_buffer () in
  Gl.bind_buffer Gl.array_buffer normals_vbo;
  Gl.buffer_data Gl.array_buffer (9 * 4) (Some normals) Gl.static_draw;

  let vao = gen_vertex_array () in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer points_vbo;
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
  Gl.bind_buffer Gl.array_buffer normals_vbo;
  Gl.vertex_attrib_pointer 1 3 Gl.float false 0 (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.enable_vertex_attrib_array 1;

  let shader_program = create_program "test_vs.glsl" "test_fs.glsl" in

  (* input variables *)
  let near = 0.1 in (* clipping plane *)
  let far  = 100.0 in (* clipping plane *)
  let fov  = 67.0 *. one_deg_in_rad in
  let aspect = float_of_int !width /. float_of_int !height in (* aspect ratio *)
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
  let cam_pos = ref {V3. x = 0.0; y = 0.0; z = 2.0} in
  let cam_yaw = ref 0.0 in

  let view_mat () =
    let m = M4.(rotate_y_deg identity (-. !cam_yaw) *%
                translate identity (V3.neg!cam_pos)) in
    prerr_endline @@ M4.to_string m;
    m
  in

  let model_rot = ref 0.0 in
  let model_mat () = {M4.identity with M4.m = !model_rot} in

  let view_mat_location = Gl.get_uniform_location shader_program "view_mat" in
  let proj_mat_location = Gl.get_uniform_location shader_program "proj_mat" in
  let model_mat_location = Gl.get_uniform_location shader_program "model_mat" in

  Gl.use_program shader_program;
  Gl.uniform_matrix4fv view_mat_location 1 false (M4.m (view_mat ()));
  Gl.uniform_matrix4fv proj_mat_location 1 false (M4.m proj_mat);
  Gl.uniform_matrix4fv model_mat_location 1 false (M4.m (model_mat ()));

  Gl.enable Gl.cull_face_enum;
  Gl.cull_face Gl.back;
  Gl.front_face Gl.cw;

  let event = Sdl.Event.create () in
  let update_fps_counter = fps_counter () in
  let frame_timer = frame_timer 60 in

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

        model_rot := sin (Unix.gettimeofday ());
        Gl.uniform_matrix4fv model_mat_location 1 false (M4.m (model_mat ()));

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
