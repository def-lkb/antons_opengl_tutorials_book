let count_obj_data ic =
  let unsorted_vp_count = ref 0 in
  let unsorted_vt_count = ref 0 in
  let unsorted_vn_count = ref 0 in
  let face_count = ref 0 in
  begin try
      while true do
        let line = input_line ic in
        if String.length line < 2 then ()
        else if line.[0] = 'v' then
          begin
            if line.[1] = ' ' then
              incr unsorted_vp_count
            else if line.[1] = 't' then
              incr unsorted_vt_count
            else if line.[1] = 'n' then
              incr unsorted_vn_count
          end
        else if line.[0] = 'f' then
          incr face_count
      done
    with End_of_file -> ()
  end;
  Printf.eprintf "found %d vp, %d vt, %d vn, %d faces unique in obj\n"
    !unsorted_vp_count !unsorted_vt_count !unsorted_vn_count !face_count;
  !unsorted_vp_count,!unsorted_vt_count,!unsorted_vn_count,!face_count

let load_obj_data ic (unsorted_vp_count,unsorted_vt_count,unsorted_vn_count,face_count) =
  let unsorted_vp_array =
    Bigarray.(Array1.create float32 c_layout (unsorted_vp_count * 3))
  and unsorted_vt_array =
    Bigarray.(Array1.create float32 c_layout (unsorted_vt_count * 2))
  and unsorted_vn_array =
    Bigarray.(Array1.create float32 c_layout (unsorted_vn_count * 3))
  in
  let points =
    Bigarray.(Array1.create float32 c_layout (3 * face_count * 3))
  and tex_coords =
    Bigarray.(Array1.create float32 c_layout (3 * face_count * 2))
  and normals =
    Bigarray.(Array1.create float32 c_layout (3 * face_count * 3))
  in
  let current_unsorted_vp = ref 0 in
  let current_unsorted_vt = ref 0 in
  let current_unsorted_vn = ref 0 in
  let point_count = ref 0 in
  begin try
      while true do
        let line = input_line ic in
        if String.length line < 2 then ()
        else if line.[0] = 'v' then
          begin
            if line.[1] = ' ' then
              (Scanf.sscanf line
                "v %f %f %f" (fun x y z ->
                    unsorted_vp_array.{!current_unsorted_vp * 3 + 0} <- x;
                    unsorted_vp_array.{!current_unsorted_vp * 3 + 1} <- y;
                    unsorted_vp_array.{!current_unsorted_vp * 3 + 2} <- z
                   );
               incr current_unsorted_vp)
            else if line.[1] = 't' then
              (Scanf.sscanf line
                 "vt %f %f" (fun x y ->
                     unsorted_vt_array.{!current_unsorted_vt * 2 + 0} <- x;
                     unsorted_vt_array.{!current_unsorted_vt * 2 + 1} <- y;
                   );
               incr current_unsorted_vt)
            else if line.[1] = 'n' then
              (Scanf.sscanf line
                 "vn %f %f %f" (fun x y z ->
                     unsorted_vn_array.{!current_unsorted_vn * 3 + 0} <- x;
                     unsorted_vn_array.{!current_unsorted_vn * 3 + 1} <- y;
                     unsorted_vn_array.{!current_unsorted_vn * 3 + 2} <- z
                   );
               incr current_unsorted_vn)
          end
        else if line.[0] = 'f' then
          begin
            (* work out if using quads instead of triangles and print a warning *)
            let slash_count = ref 0 in
            String.iter (fun c -> if c = '/' then incr slash_count) line;
            if !slash_count <> 6 then
              begin
                prerr_endline
                  "ERROR: file contains quads or does not match v vp/vt/vn layout - \
                   make sure exported mesh is triangulated and contains vertex points, \
                   texture coordinates, and normals\n";
                invalid_arg "load_obj_data"
              end;
            let bound x a b = x >= a && x < b in
            Scanf.sscanf line "f %d/%d/%d %d/%d/%d %d/%d/%d"
              (fun vpx vtx vnx vpy vty vny vpz vtz vnz ->
                 List.iter (fun (vp,vt,vn) ->
                     let vp = vp - 1 and vt = vt - 1 and vn = vn - 1 in
                     if not (bound vp 0 unsorted_vp_count) then
                       begin
                         prerr_endline "ERROR: invalid vertex position index in face";
                         invalid_arg "load_obj_data"
                       end;
                     if not (bound vt 0 unsorted_vt_count) then
                       begin
                         prerr_endline "ERROR: invalid vertex position index in face";
                         invalid_arg "load_obj_data"
                       end;
                     if not (bound vn 0 unsorted_vn_count) then
                       begin
                         prerr_endline "ERROR: invalid vertex position index in face";
                         invalid_arg "load_obj_data"
                       end;
                     points.{!point_count * 3 + 0} <-
                       unsorted_vp_array.{vp * 3 + 0};
                     points.{!point_count * 3 + 1} <-
                       unsorted_vp_array.{vp * 3 + 1};
                     points.{!point_count * 3 + 2} <-
                       unsorted_vp_array.{vp * 3 + 2};
                     tex_coords.{!point_count * 2 + 0} <-
                       unsorted_vt_array.{vt * 2 + 0};
                     tex_coords.{!point_count * 2 + 1} <-
                       unsorted_vt_array.{vt * 2 + 1};
                     normals.{!point_count * 3 + 0} <-
                       unsorted_vn_array.{vn * 3 + 0};
                     normals.{!point_count * 3 + 1} <-
                       unsorted_vn_array.{vn * 3 + 1};
                     normals.{!point_count * 3 + 2} <-
                       unsorted_vn_array.{vn * 3 + 2};
                     incr point_count)
                   [(vpx,vtx,vnx); (vpy,vty,vny); (vpz,vtz,vnz)]
              )
          end
      done
    with End_of_file -> ()
  end;
  Printf.eprintf "allocated %d points" !point_count;
  !point_count, points, tex_coords, normals

let load_obj_file fname =
  let ic = open_in fname in
  try
    let stats = count_obj_data ic in
    seek_in ic 0;
    let result = load_obj_data ic stats in
    close_in_noerr ic;
    result
  with exn ->
    close_in_noerr ic;
    raise exn
