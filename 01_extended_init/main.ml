open Tgl4
open Tsdl

module Helpers = struct
  let check = function
    | `Ok x -> x
    | `Error msg -> failwith msg

  let option_get = function
    | None -> failwith "option_get"
    | Some x -> x

  let sprintf = Printf.sprintf

  let gl_set_attribute x y = check (Sdl.gl_set_attribute x y)

  let join l = String.concat "\n" l

  let alloc_buffer n =
    Bigarray.Array1.create
      Bigarray.int32 Bigarray.c_layout
      n

  let gen_buffers n =
    let vbo = alloc_buffer n in
    Gl.gen_buffers n vbo;
    let arr = Array.make n 0 in
    for i = 0 to n - 1 do
      arr.(i) <- Int32.to_int vbo.{i};
    done;
    arr

  let gen_vertex_arrays n =
    let vao = alloc_buffer n in
    Gl.gen_vertex_arrays n vao;
    let arr = Array.make n 0 in
    for i = 0 to n - 1 do
      arr.(i) <- Int32.to_int vao.{i};
    done;
    arr

  let small_buf = alloc_buffer 8

  let gen_buffer () =
    Gl.gen_buffers 1 small_buf;
    Int32.to_int small_buf.{0}

  let gen_vertex_array () =
    Gl.gen_vertex_arrays 1 small_buf;
    Int32.to_int small_buf.{0}

  let float_buffer arr =
    Bigarray.Array1.of_array
      Bigarray.float32 Bigarray.c_layout
      arr

  let log_file = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o644
      "gl.log"

  let gl_log f = Printf.fprintf log_file f
  let gl_log_err f =
    Printf.ksprintf
      (fun s -> prerr_string s; output_string log_file s)
      f
end
open Helpers

let init_scene () =
  check @@ Sdl.init Sdl.Init.(timer + video);
  at_exit Sdl.quit;

  let width = 640 and height = 480 in
  gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;
  gl_set_attribute Sdl.Gl.context_major_version 3;
  gl_set_attribute Sdl.Gl.context_minor_version 0;

  (* Do double buffering in GL *)
  gl_set_attribute Sdl.Gl.doublebuffer 1;
  gl_set_attribute Sdl.Gl.depth_size 24;

  let window = check @@
    Sdl.create_window "Shader example" ~w:width ~h:height
      Sdl.Window.(opengl + shown + resizable)
  in

  let context = check @@ Sdl.gl_create_context window in

  window, context

let log_gl_params () =
  let log_gl_int_param (param,name) =
    Gl.get_integerv param small_buf;
    gl_log "%s %ld\n" name small_buf.{0}
  in
  gl_log "GL Context Params:\n";
  (* integers - only works if the order is 0-10 integer return types *)
  List.iter log_gl_int_param [
    Gl.max_combined_texture_image_units, "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS";
    Gl.max_cube_map_texture_size       , "GL_MAX_CUBE_MAP_TEXTURE_SIZE";
    Gl.max_draw_buffers                , "GL_MAX_DRAW_BUFFERS";
    Gl.max_fragment_uniform_components , "GL_MAX_FRAGMENT_UNIFORM_COMPONENTS";
    Gl.max_texture_image_units         , "GL_MAX_TEXTURE_IMAGE_UNITS";
    Gl.max_texture_size                , "GL_MAX_TEXTURE_SIZE";
    Gl.max_varying_floats              , "GL_MAX_VARYING_FLOATS";
    Gl.max_vertex_attribs              , "GL_MAX_VERTEX_ATTRIBS";
    Gl.max_vertex_texture_image_units  , "GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS";
    Gl.max_vertex_uniform_components   , "GL_MAX_VERTEX_UNIFORM_COMPONENTS";
  ];
  Gl.get_integerv Gl.max_viewport_dims small_buf;
  gl_log "GL_MAX_VIEWPORT_DIMS %ld %ld\n" small_buf.{0} small_buf.{1};
  let tmp = Bigarray.(Array1.create int8_unsigned c_layout 1) in
  Gl.get_booleanv Gl.stereo tmp;
  gl_log "GL_STEREO %b\n" (tmp.{0} <> 0)

let fps_counter () =
  let frame_count = ref 0 in
  let previous_seconds = ref (Unix.gettimeofday ()) in
  fun window ->
    let current_seconds = Unix.gettimeofday () in
    let elapsed_seconds = current_seconds -. !previous_seconds in
    if (elapsed_seconds > 0.25) then
      begin
        previous_seconds := current_seconds;
        let fps = float_of_int !frame_count /. elapsed_seconds in
        Sdl.set_window_title window (sprintf "opengl @ fps: %.2f" fps);
        frame_count := 0
      end
    else
      incr frame_count

let frame_timer fps =
  let tick = ref (Unix.gettimeofday ()) in
  let delta = 1. /. float_of_int fps in
  fun () ->
    let now = Unix.gettimeofday () in
    let tick' = !tick +. delta in
    let delay =
      if now > tick' then
        (tick := now; 0.)
      else if now > !tick then
        (tick := tick'; tick' -. now)
      else
        (!tick -. now)
    in
    int_of_float (delay *. 1000.)

let main () =
  let points = float_buffer [|
       0.0;  0.5; 0.0;
       0.5; -0.5; 0.0;
      -0.5; -0.5; 0.0
    |] in

  let vertex_shader = join
      [ "#version 130"
      ; "in vec3 vp;"
      ; "void main () {"
      ; "	gl_Position = vec4 (vp, 1.0);"
      ; "}"
      ]
  in

  let fragment_shader = join
      [ "#version 130"
      ; "out vec4 frag_colour;"
      ; "void main () {"
      ; "	frag_colour = vec4 (0.5, 0.0, 0.5, 1.0);"
      ; "}"
      ]
  in

  let window, context = init_scene () in

	(* get version info *)
  Printf.printf "Renderer: %s\n%!"
    (option_get @@ Gl.get_string Gl.renderer);
  Printf.printf "OpenGL version supported %s\n%!"
    (option_get @@ Gl.get_string Gl.version);
  log_gl_params ();

  (* tell GL to only draw onto a pixel if the shape is closer to the viewer *)
  Gl.enable Gl.depth_test;
  Gl.depth_func Gl.less;

  (* a vertex buffer object (VBO) is created here.
     this stores an array of data on the graphics adapter's memory.
     in our case - the vertex points
  *)
  let vbo = gen_buffer () in
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.buffer_data Gl.array_buffer (9 * 4) (Some points) Gl.static_draw;

  (* the vertex array object (VAO) is a little descriptor that defines which
     data from vertex buffer objects should be used as input variables to
     vertex shaders.
     in our case - use our only VBO, and say 'every three floats is a variable'
  *)
  let vao = gen_vertex_array () in
  Gl.bind_vertex_array vao;
  Gl.enable_vertex_attrib_array 0;
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);

  (* here we copy the shader strings into GL shaders, and compile them. we then
     create an executable shader 'program' and attach both of the compiled
     shaders.  we link this, which matches the outputs of the vertex shader to
     the inputs of the fragment shader, etc. and it is then ready to use *)
  let vs = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source vs vertex_shader;
  Gl.compile_shader vs;

  let fs = Gl.create_shader Gl.fragment_shader in
  Gl.shader_source fs fragment_shader;
  Gl.compile_shader fs;

  let shader_program = Gl.create_program () in
  Gl.attach_shader shader_program fs;
  Gl.attach_shader shader_program vs;
  Gl.link_program shader_program;

  (* this loop clears the drawing surface, then draws the geometry described by
     the VAO onto the drawing surface. we 'poll events' to see if the window
     was closed, etc. finally, we 'swap the buffers' which displays our drawing
     surface onto the view area. we use a double-buffering system which means
     that we have a 'currently displayed' surface, and 'currently being drawn'
     surface. hence the 'swap' idea. in a single-buffering system we would see
     stuff being drawn one-after-the-other *)

  let event = Sdl.Event.create () in
  let width = ref 640 and height = ref 480 in
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
        Gl.use_program shader_program;
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
