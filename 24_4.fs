type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    (x.hours > y.hours) && (x.minutes > y.minutes) && (x.f = y.f || x.f = "PM")