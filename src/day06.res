let exampleInputStr = "3,4,3,1,2"

let inputStr = "1,3,4,1,5,2,1,1,1,1,5,1,5,1,1,1,1,3,1,1,1,1,1,1,1,2,1,5,1,1,1,1,1,4,4,1,1,4,1,1,2,3,1,5,1,4,1,2,4,1,1,1,1,1,1,1,1,2,5,3,3,5,1,1,1,1,4,1,1,3,1,1,1,2,3,4,1,1,5,1,1,1,1,1,2,1,3,1,3,1,2,5,1,1,1,1,5,1,5,5,1,1,1,1,3,4,4,4,1,5,1,1,4,4,1,1,1,1,3,1,1,1,1,1,1,3,2,1,4,1,1,4,1,5,5,1,2,2,1,5,4,2,1,1,5,1,5,1,3,1,1,1,1,1,4,1,2,1,1,5,1,1,4,1,4,5,3,5,5,1,2,1,1,1,1,1,3,5,1,2,1,2,1,3,1,1,1,1,1,4,5,4,1,3,3,1,1,1,1,1,1,1,1,1,5,1,1,1,5,1,1,4,1,5,2,4,1,1,1,2,1,1,4,4,1,2,1,1,1,1,5,3,1,1,1,1,4,1,4,1,1,1,1,1,1,3,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,5,1,2,1,1,1,1,1,1,1,1,1"

let parseInput = input => {
  input
  |> String.split_on_char(',')
  |> List.map(int_of_string)
  |> List.fold_left((fishCounts, next) => {
    Belt.Map.Int.update(fishCounts, next, v => {
      switch v {
      | Some(count) => Some(Int64.add(count, Int64.of_int(1)))
      | None => Some(Int64.of_int(1))
      }
    })
  }, Belt.Map.Int.empty)
}

type fishCounts = Belt.Map.Int.t<int64>

let nextDay = (fishCounts: fishCounts) => {
  Belt.Map.Int.reduce(fishCounts, Belt.Map.Int.empty, (counts, timerValue, count) => {
    let inc = (val: option<int64>) =>
      switch val {
      | Some(v) => Some(Int64.add(v, count))
      | None => Some(count)
      }
    if timerValue > 0 {
      Belt.Map.Int.update(counts, timerValue - 1, inc)
    } else {
      Belt.Map.Int.update(counts, 6, inc)->Belt.Map.Int.update(8, inc)
    }
  })
}

let rec nextNDays = (days: int, fishCounts: fishCounts) => {
  if days == 0 {
    fishCounts
  } else {
    nextNDays(days - 1, nextDay(fishCounts))
  }
}

let part1 = () => {
  let input = parseInput(exampleInputStr)
  let out = nextNDays(80, input)

  Belt.Map.Int.toArray(out) |> Array.fold_left((sum, (_, count)) => {
    Int64.add(sum, count)
  }, Int64.of_int(0)) |> Int64.to_float
}

Js.log2("Part 1", part1())

let part2 = () => {
  let input = parseInput(inputStr)
  let out = nextNDays(256, input)
  Belt.Map.Int.toArray(out) |> Array.fold_left((sum, (_, count)) => {
    Int64.add(sum, count)
  }, Int64.of_int(0)) |> Int64.to_float
}

Js.log2("Part 2", part2())
