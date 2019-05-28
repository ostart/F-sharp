type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y =
    (x.hours > y.hours) && (x.minutes > y.minutes) && (x.f = y.f || x.f = PM)