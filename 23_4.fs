// 23.4.1
let (.+.) x y = 
    let (gold1,silver1,cupper1) = x
    let (gold2,silver2,cupper2) = y
    let cupper = (cupper1 + cupper2) % 12
    let silver = (((cupper1 + cupper2) / 12) + silver1 + silver2) % 20
    let gold = ((((cupper1 + cupper2) / 12) + silver1 + silver2) / 20) + gold1 + gold2
    (gold,silver,cupper)
let (.-.) x y =
    let (gold2,silver2,cupper2) = y
    let z = (-gold2,-silver2,-cupper2)
    x .+. z

// // 23.4.2
let (.+) x y =
    let (x1,y1) = x
    let (x2,y2) = y
    (x1+x2,y1+y2)
let (.-) x y =
    let (x2,y2) = y
    let z = (-x2,-y2)
    x .+ z
let (.*) x y =
    let (x1,y1) = x
    let (x2,y2) = y
    (x1*x2 - y1*y2, y1*x2 + x1*y2)
let (./) x y =
    let (x1,y1) = x
    let (x2,y2) = y
    let quadro = x2*x2+y2*y2
    let z = (x2/quadro,-y2/quadro)
    x .* z
