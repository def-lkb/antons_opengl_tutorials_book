open Tgl4 open Tsdl

open Gl_utils
open Math_funcs

(* load a mesh using the assimp library *)
let load_mesh filename =
  let open Assimp in
  match import_file filename process_triangulate with
  | `Error msg -> failwith (sprintf "Error loading %S: %s" filename msg)
  | `Ok raw_scene ->
    let scene = view_scene raw_scene in
    release_scene raw_scene;
    Printf.printf "  %i animations\n" (Array.length scene.scene_animations);
    Printf.printf "  %i cameras\n" (Array.length scene.scene_cameras);
    Printf.printf "  %i lights\n" (Array.length scene.scene_lights);
    Printf.printf "  %i materials\n" (Array.length scene.scene_materials);
    Printf.printf "  %i meshes\n" (Array.length scene.scene_meshes);
    Printf.printf "  %i textures\n" (Array.length scene.scene_textures);

    (* get first mesh in file only *)
    let mesh = scene.scene_meshes.(0) in
    let point_count = Array.length mesh.mesh_vertices in
    Printf.printf "    %i vertices in mesh[0]\n" point_count;

    let vao = gen_vertex_array () in
    Gl.bind_vertex_array vao;

    (* we really need to copy out all the data from AssImp's funny little data
       structures into pure contiguous arrays before we copy it into data buffers
       because assimp's texture coordinates are not really contiguous in memory.
       i allocate some dynamic memory to do this. *)
    let points = Bigarray.(Array1.create float32 c_layout (point_count * 3)) in
    for i = 0 to point_count - 1 do
      points.{i * 3 + 0} <- mesh.mesh_vertices.(i).(0);
      points.{i * 3 + 1} <- mesh.mesh_vertices.(i).(1);
      points.{i * 3 + 2} <- mesh.mesh_vertices.(i).(2);
    done;

    let normals = Bigarray.(Array1.create float32 c_layout (point_count * 3)) in
    for i = 0 to point_count - 1 do
      normals.{i * 3 + 0} <- mesh.mesh_normals.(i).(0);
      normals.{i * 3 + 1} <- mesh.mesh_normals.(i).(1);
      normals.{i * 3 + 2} <- mesh.mesh_normals.(i).(2);
    done;

    let texcoords = Bigarray.(Array1.create float32 c_layout (point_count * 2)) in
    for i = 0 to point_count - 1 do
      texcoords.{i * 2 + 0} <- mesh.mesh_texture_coords.(0).(i).(0);
      texcoords.{i * 2 + 1} <- mesh.mesh_texture_coords.(0).(i).(1);
    done;

    let points_vbo = gen_buffer () in
    Gl.bind_buffer Gl.array_buffer points_vbo;
    Gl.buffer_data Gl.array_buffer (3 * point_count * 4) (Some points) Gl.static_draw;
    Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
    Gl.enable_vertex_attrib_array 0;

    let normals_vbo = gen_buffer () in
    Gl.bind_buffer Gl.array_buffer normals_vbo;
    Gl.buffer_data Gl.array_buffer (3 * point_count * 4) (Some normals) Gl.static_draw;
    Gl.vertex_attrib_pointer 1 3 Gl.float false 0 (`Offset 0);
    Gl.enable_vertex_attrib_array 1;

    let texcoords_vbo = gen_buffer () in
    Gl.bind_buffer Gl.array_buffer texcoords_vbo;
    Gl.buffer_data Gl.array_buffer (2 * point_count * 4) (Some texcoords) Gl.static_draw;
    Gl.vertex_attrib_pointer 2 2 Gl.float false 0 (`Offset 0);
    Gl.enable_vertex_attrib_array 2;

    point_count, vao

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
  Gl.enable Gl.cull_face_enum;
  Gl.front_face Gl.ccw;
  Gl.clear_color 0.2 0.2 0.2 1.0;
  Gl.viewport 0 0 !width !height;

  (*----------------------------create geometry-----------------------------*)
  let point_count, vao = load_mesh "monkey2.fobj" in

  (*----------------------------create shaders-----------------------------*)
  let shader_program = create_program "test_vs.glsl" "test_fs.glsl" in

  let view_mat_location = Gl.get_uniform_location shader_program "view" in
  let proj_mat_location = Gl.get_uniform_location shader_program "proj" in

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
  let cam_speed = 1.0 in
  let cam_yaw_speed = 10.0 in
  let cam_pos = ref {V3. x = 0.0; y = 0.0; z = 5.0} in
  let cam_yaw = ref 0.0 in

  let view_mat () =
    M4.(rotate_y_deg identity (-. !cam_yaw) *%
        translate identity (V3.neg!cam_pos))
  in

  Gl.use_program shader_program;
  Gl.uniform_matrix4fv view_mat_location 1 false (M4.m (view_mat ()));
  Gl.uniform_matrix4fv proj_mat_location 1 false (M4.m proj_mat);

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
        let current_seconds = Unix.gettimeofday () in
        let elapsed_seconds = current_seconds -. !previous_seconds in
        previous_seconds := current_seconds;

        update_fps_counter window;
        Gl.clear (Gl.depth_buffer_bit lor Gl.color_buffer_bit);
        Gl.viewport 0 0 !width !height;

        Gl.use_program shader_program;

        Gl.bind_vertex_array vao;
        (* draw points 0-3 from the currently bound VAO with current in-use shader *)
        Gl.draw_arrays Gl.triangles 0 point_count;

        let cam_moved = ref false in
        let check_key =
          let keys = Sdl.get_keyboard_state () in
          fun k -> keys.{k} <> 0
        in
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

        (* put the stuff we've been drawing onto the display *)
        Sdl.gl_swap_window window;
        loop ()
      end
  in
  loop ()

let () = main ()
