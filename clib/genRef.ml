type 'a t = (unit -> 'a) * ('a -> unit)

let make g s = (g, s)

let const x = ((fun () -> x), (fun _ -> ()))

let ref x =
  let r = ref x in
  ((fun () -> !r), (fun y -> r := y))

let get (g, s) = g ()

let set (g, s) x = s x

let combine f (g1, s1) (g2, s2) = ((fun () -> f (g1 ()) (g2 ())), (fun x -> s1 x; s2 x))

let observe f (g, s) = (g, (fun x -> f x; s x))
