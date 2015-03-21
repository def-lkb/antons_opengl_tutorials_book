open Tgl4
open Tsdl

let check = function
  | `Ok x -> x
  | `Error msg -> failwith msg

let option_get = function
  | None -> failwith "option_get"
  | Some x -> x

let gl_set_attribute x y = check (Sdl.gl_set_attribute x y)

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
      Sdl.Window.(opengl + shown)
  in

  let context = check @@ Sdl.gl_create_context window in

  Gl.enable Gl.depth_test;
  Gl.depth_func Gl.less;

  window, context

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


let float_buffer arr =
  Bigarray.Array1.of_array
    Bigarray.float32 Bigarray.c_layout
    arr

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

  (* a vertex buffer object (VBO) is created here.
     this stores an array of data on the graphics adapter's memory.
     in our case - the vertex points
  *)
  let vbo = gen_buffers 1 in
  Gl.bind_buffer Gl.array_buffer vbo.(0);
  Gl.buffer_data Gl.array_buffer (9 * 4) (Some points) Gl.static_draw;

  (* the vertex array object (VAO) is a little descriptor that defines which
     data from vertex buffer objects should be used as input variables to
     vertex shaders.
     in our case - use our only VBO, and say 'every three floats is a variable'
  *)
  let vao = gen_vertex_arrays 1 in
  Gl.bind_vertex_array vao.(0);
  Gl.enable_vertex_attrib_array 0;
  Gl.bind_buffer Gl.array_buffer vbo.(0);
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
  let rec loop () =
    let is_done =
      if Sdl.wait_event_timeout (Some event) (1000 / 60) then
        match Sdl.Event.(enum (get event typ)) with
        | `Quit -> true
        | _ -> false
      else
        false
    in
    if is_done then ()
    else
      begin
        Gl.clear (Gl.depth_buffer_bit lor Gl.color_buffer_bit);
        Gl.use_program shader_program;
        Gl.bind_vertex_array vao.(0);
        Gl.draw_arrays Gl.triangles 0 3;
        Sdl.gl_swap_window window;
        loop ()
      end
  in
  loop ()

let () = main ()
