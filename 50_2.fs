let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)

// 50.2.1
let fac_seq = seq {
    for i in 0..System.Int32.MaxValue do
      yield (factorial i)
       }

// 50.2.2
let seq_seq = seq {
    yield 0
    for i in 1..System.Int32.MaxValue do
      yield! seq [-i;i]
       }

// // 50.2.1
// let fac_seq = seq {
//     let mutable i = 0
//     while true do
//       yield (factorial i)
//       i <- i + 1
//        }

// // 50.2.2
// let seq_seq = seq {
//     yield 0
//     let mutable i = 1
//     while true do
//       yield! seq [-i;i]
//       i <- i + 1
//        }

// Решение учителя:
// let rec fac n res = seq {
//   yield res
//   yield! fac (n+1) (res*n)
// }

// let fac_seq = seq {
//   yield 1
//   yield! fac 2 1
// }