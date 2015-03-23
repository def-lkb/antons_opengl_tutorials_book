let pi = 4.0 *. atan 1.0
let one_deg_in_rad = (2.0 *. pi) /. 360.0
let one_rad_in_deg = 360.0 /. (2.0 *. pi)

module V2 = struct
  type t = {x: float; y: float}
  let zero = {x = 0.; y = 0.}

  let ( +% ) {x = x1; y = y1} {x = x2; y = y2} =
    {x = x1 +. x2; y = y1 +. y2}

  let ( -% ) {x = x1; y = y1} {x = x2; y = y2} =
    {x = x1 -. x2; y = y1 -. y2}

  let ( *% ) {x; y} s =
    {x = x *. s; y = y *. s}

  let ( /% ) {x; y} s =
    {x = x /. s; y = y /. s}

  let norm {x; y} = sqrt (x *. x +. y *. y)
  let norm2 {x; y} = (x *. x +. y *. y)
  let normalize v = v /% norm v

  let to_string {x; y} =
    Printf.sprintf "{x = %f; y = %f}" x y
end

module V3 = struct
  type t = {x: float; y: float; z: float}
  let zero = {x = 0.; y = 0.; z = 0.}

  let v2 {V2. x; y} z = {x; y; z}

  let ( +% ) {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    {x = x1 +. x2; y = y1 +. y2; z = z1 +. z2}

  let ( -% ) {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    {x = x1 -. x2; y = y1 -. y2; z = z1 -. z2}

  let ( *% ) {x; y; z} s =
    {x = x *. s; y = y *. s; z = z *. s}

  let ( /% ) {x; y; z} s =
    {x = x /. s; y = y /. s; z = z /. s}

  let norm {x; y; z} = sqrt (x *. x +. y *. y +. z *. z)
  let norm2 {x; y; z} = (x *. x +. y *. y +. z *. z)
  let normalize v = v /% norm v

  let dot {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let cross {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    { x = y1 *. z2 -. z1 *. y2;
      y = z1 *. x2 -. x1 *. z2;
      z = x1 *. y2 -. y1 *. x2 }

  let to_string {x; y; z} =
    Printf.sprintf "{x = %f; y = %f; z = %f}" x y z

  let dist2 v1 v2 = norm2 (v1 -% v2)

  let direction_to_heading d =
    atan2 (-. d.x) (-. d.z) *. one_rad_in_deg

  let heading_to_direction d =
    let rad = d *. one_deg_in_rad in
    { x = -. sin rad; y = 0.0; z = -. cos rad }
end

module V4 = struct
  type t = {x: float; y: float; z: float; w: float}
  let zero = {x = 0.; y = 0.; z = 0.; w = 0.}

  let v2 {V2. x; y} z w = {x; y; z; w}
  let v3 {V3. x; y; z} w = {x; y; z; w}

  let ( +% ) {x = x1; y = y1; z = z1; w = w1} {x = x2; y = y2; z = z2; w = w2} =
    {x = x1 +. x2; y = y1 +. y2; z = z1 +. z2; w = w1 +. w2}

  let ( -% ) {x = x1; y = y1; z = z1; w = w1} {x = x2; y = y2; z = z2; w = w2} =
    {x = x1 -. x2; y = y1 -. y2; z = z1 -. z2; w = w1 -. w2}

  let ( *% ) {x; y; z; w} s =
    {x = x *. s; y = y *. s; z = z *. s; w = w *. s}

  let ( /% ) {x; y; z; w} s =
    {x = x /. s; y = y /. s; z = z /. s; w = w /. s}

  let to_string {x; y; z; w} =
    Printf.sprintf "{x = %f; y = %f; z = %f; w = %f}" x y z w
end

let v3_of_v4 {V4. x; y; z; w = _} = {V3. x; y; z}

module Q = struct
  type t = {r: float; i: float; j: float; k: float}

  let ( /% ) {r; i; j; k} s =
    {r = r /. s; i = i /. s; j = j /. s; k = k /. s}

  let ( *% ) {r; i; j; k} s =
    {r = r *. s; i = i *. s; j = j *. s; k = k *. s}

  let normalize q =
    let sum = q.r *. q.r +. q.i *. q.i +. q.j *. q.j +. q.k *. q.k in
    if abs_float (1.0 -. sum) < 0.0001 then
      q
    else
      q /% sqrt sum

  let ( +% )
      {r = r1; i = i1; j = j1; k = k1}
      {r = r2; i = i2; j = j2; k = k2} =
    normalize {
      r = r1 +. r2;
      i = i1 +. i2;
      j = j1 +. j2;
      k = k1 +. k2;
    }
  let ( %*% )
      {r = r1; i = i1; j = j1; k = k1}
      {r = r2; i = i2; j = j2; k = k2} =
    normalize {
      r = r2 *. r1 -. i2 *. i1 -. j2 *. j1 -. k2 *. k1;
      i = r2 *. i1 +. i2 *. r1 -. j2 *. k1 +. k2 *. j1;
      j = r2 *. j1 +. i2 *. k1 -. j2 *. r1 +. k2 *. i1;
      k = r2 *. k1 +. i2 *. j1 -. j2 *. i1 +. k2 *. r1;
    }

  let dot
      {r = r1; i = i1; j = j1; k = k1}
      {r = r2; i = i2; j = j2; k = k2} =
    r1 *. r2 +. i1 *. i2 +. j1 *. j2 +. k1 *. k2

  let from_axis_rad rad v =
    let open V3 in
    let rad = rad /. 2. in
    let c = cos rad and s = sin rad in
    {r = c; i = s *. v.x; j = s *. v.y; k = s *. v.z}

  let from_axis_deg deg v =
    from_axis_rad (one_deg_in_rad *. deg) v

  let slerp q r t =
    (* angle between q0-q1 *)
    let cos_half_theta = dot q r in
    (* as found here http://stackoverflow.com/questions/2886606/flipping-issue-when-interpolating-rotations-using-quaternions
       if dot product is negative then one quaternion should be negated, to make
       it take the short way around, rather than the long way
       yeah! and furthermore Susan, I had to recalculate the d.p. after this *)
    let q, cos_half_theta =
      if cos_half_theta < 0.0 then
        let q = q *% (-1.) in
        q, dot q r
      else
        q, cos_half_theta
    in
    (* if qa=qb or qa=-qb then theta = 0 and we can return qa *)
    if abs_float cos_half_theta >= 1.0 then
      q
    else
      (* Calculate temporary values *)
      let sin_half_theta = sqrt (1.0 -. cos_half_theta *. cos_half_theta) in
      (* if theta = 180 degrees then result is not fully defined
         we could rotate around any axis normal to qa or qb *)
      if abs_float sin_half_theta < 0.001 then
        q *% (1.0 -. t) +% r *% t
      else
        let half_theta = acos cos_half_theta in
        let a = sin ((1.0 -. t) *. half_theta) /. sin_half_theta in
        let b = sin (t *. half_theta) /. sin_half_theta in
        q *% a +% r *% b

  let to_string {r; i; j; k} =
    Printf.sprintf "{r = %f; i = %f; j = %f; k = %f}" r i j k

end

(* TODO: mat3 / mat4 *)
