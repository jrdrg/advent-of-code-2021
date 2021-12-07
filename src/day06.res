let exampleInputStr = "3,4,3,1,2"

let inputStr = "1,3,4,1,5,2,1,1,1,1,5,1,5,1,1,1,1,3,1,1,1,1,1,1,1,2,1,5,1,1,1,1,1,4,4,1,1,4,1,1,2,3,1,5,1,4,1,2,4,1,1,1,1,1,1,1,1,2,5,3,3,5,1,1,1,1,4,1,1,3,1,1,1,2,3,4,1,1,5,1,1,1,1,1,2,1,3,1,3,1,2,5,1,1,1,1,5,1,5,5,1,1,1,1,3,4,4,4,1,5,1,1,4,4,1,1,1,1,3,1,1,1,1,1,1,3,2,1,4,1,1,4,1,5,5,1,2,2,1,5,4,2,1,1,5,1,5,1,3,1,1,1,1,1,4,1,2,1,1,5,1,1,4,1,4,5,3,5,5,1,2,1,1,1,1,1,3,5,1,2,1,2,1,3,1,1,1,1,1,4,5,4,1,3,3,1,1,1,1,1,1,1,1,1,5,1,1,1,5,1,1,4,1,5,2,4,1,1,1,2,1,1,4,4,1,2,1,1,1,1,5,3,1,1,1,1,4,1,4,1,1,1,1,1,1,3,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,5,1,2,1,1,1,1,1,1,1,1,1"

let parseInput = input => {
  input |> String.split_on_char(',') |> List.map(int_of_string)
}

let updateFish = (fishTimer: int) => {
  if fishTimer == 0 {
    (6, Some(8))
  } else {
    (fishTimer - 1, None)
  }
}

let passDay = (fishes: list<int>) => {
  let (original, added) = fishes |> List.fold_left(((origFishes, newFishes), next) => {
    let (updated, newFish) = updateFish(next)
    switch newFish {
    | Some(fish) => (list{updated, ...origFishes}, list{fish, ...newFishes})
    | None => (list{updated, ...origFishes}, newFishes)
    }
  }, (list{}, list{}))

  //   Js.log(original |> Array.of_list)
  //   Js.log(added |> Array.of_list)

  List.append(List.rev(original), List.rev(added))
}

let rec passDays = (day: int, fishes: list<int>) => {
  if day > 0 {
    let updated = passDay(fishes)
    passDays(day - 1, updated)
  } else {
    fishes
  }
}

let part1 = () => {
  let input = parseInput(inputStr)

  let out = passDays(50, input) |> Array.of_list

  Js.log2("OUT", out)

  Array.length(out)
}

Js.log2("Part 1", part1())
