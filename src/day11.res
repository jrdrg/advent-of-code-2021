let exampleInputStr = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let inputStr = "2682551651
3223134263
5848471412
7438334862
8731321573
6415233574
5564726843
6683456445
8582346112
4617588236"

module Grid = {
  type t = array<array<int>>

  let coordsToString = (r, c) => j`${string_of_int(r)}:${string_of_int(c)}`

  let stringToCoords = key => {
    let coords = key |> String.split_on_char(':') |> List.map(int_of_string) |> Array.of_list
    (coords[0], coords[1])
  }

  let getAdjacentNeighborCoords = (row, col) => {
    list{(row, col - 1), (row, col + 1), (row - 1, col), (row + 1, col)}
  }

  let getNeighborCoords = (row, col) => {
    list{
      (row - 1, col - 1),
      (row - 1, col),
      (row - 1, col + 1),
      (row, col - 1),
      (row, col + 1),
      (row + 1, col - 1),
      (row + 1, col),
      (row + 1, col + 1),
    }
  }

  let validPoints = (points: list<(int, int)>, grid: t) => {
    points |> List.filter(((r, c)) => {
      r >= 0 && c >= 0 && r < Array.length(grid) && c < Array.length(grid[r])
    })
  }

  let getNeighbors = (row, col, grid: t) => {
    let possible = getNeighborCoords(row, col)
    validPoints(possible, grid)
  }

  let valueAt = (r, c, grid: t) => {
    grid[r][c]
  }

  let setValueAt = (r, c, v, grid: t) => {
    grid[r][c] = v
    grid
  }

  let toPointsList = (grid: t) => {
    grid
    |> Array.mapi((iR, row) => row |> Array.mapi((iC, _) => (iR, iC)) |> Array.to_list)
    |> Array.to_list
    |> List.flatten
  }

  let fromString = (input: string): t => {
    input
    |> String.split_on_char('\n')
    |> List.map(Js.String.split(""))
    |> List.map(Array.map(int_of_string))
    |> Array.of_list
  }
}

let increaseInput = (grid: Grid.t): Grid.t => {
  grid |> Array.map(Array.map(c => c + 1))
}

let addToSet = (r, c, set: Belt.Set.String.t) => {
  Belt.Set.String.add(set, Grid.coordsToString(r, c))
}

let setHas = (r, c, set: Belt.Set.String.t) => {
  Belt.Set.String.has(set, Grid.coordsToString(r, c))
}

let flash = (r, c, flashed: Belt.Set.String.t, grid: Grid.t) => {
  let neighbors = Grid.getNeighbors(r, c, grid)
  neighbors |> List.fold_left(((pGrid, pFlashed, willFlash), (nR, nC)) => {
    let newValue = Grid.valueAt(nR, nC, pGrid) + 1
    let g = pGrid |> Grid.setValueAt(nR, nC, newValue)
    if newValue > 9 {
      (g, pFlashed, list{(nR, nC), ...willFlash})
    } else {
      (g, pFlashed, willFlash)
    }
  }, (grid, Belt.Set.String.add(flashed, Grid.coordsToString(r, c)), list{}))
}

let rec flashOctopuses = (
  remaining: list<(int, int)>,
  visited: Belt.Set.String.t,
  flashed: Belt.Set.String.t,
  grid: Grid.t,
) => {
  switch remaining {
  | list{(r, c), ...tail} =>
    if Grid.valueAt(r, c, grid) > 9 && !setHas(r, c, flashed) {
      let (nGrid, nFlashed, willFlash) = flash(r, c, flashed, grid)
      let points = List.append(willFlash, tail)

      flashOctopuses(points, visited, nFlashed, nGrid)
    } else {
      flashOctopuses(tail, visited, flashed, grid)
    }
  | _ => (grid, flashed)
  }
}

let rec resetFlashedOctopuses = (remaining: list<(int, int)>, grid: Grid.t) => {
  switch remaining {
  | list{(r, c), ...tail} =>
    let nGrid = Grid.setValueAt(r, c, 0, grid)
    resetFlashedOctopuses(tail, nGrid)
  | list{} => grid
  }
}

let part1 = () => {
  let input = Grid.fromString(inputStr)
  let pointsList = Grid.toPointsList(input)

  let rec doStep = (grid: Grid.t, step: int, totalFlashes: int) => {
    if step <= 0 {
      totalFlashes
    } else {
      let (flashedGrid, flashed) =
        grid
        |> increaseInput
        |> flashOctopuses(pointsList, Belt.Set.String.empty, Belt.Set.String.empty)

      let resetGrid = resetFlashedOctopuses(
        Belt.Set.String.toList(flashed) |> List.map(Grid.stringToCoords),
        flashedGrid,
      )

      let flashCount = Belt.Set.String.size(flashed)

      doStep(resetGrid, step - 1, totalFlashes + flashCount)
    }
  }

  doStep(input, 100, 0)
}

Js.log2("Part 1", part1())

let part2 = () => {
  let input = Grid.fromString(inputStr)
  let pointsList = Grid.toPointsList(input)
  let pointsCount = List.length(pointsList)

  let rec doStep = (grid: Grid.t, step: int, totalFlashes: int) => {
    if step >= 10000 {
      None
    } else {
      let (flashedGrid, flashed) =
        grid
        |> increaseInput
        |> flashOctopuses(pointsList, Belt.Set.String.empty, Belt.Set.String.empty)

      let resetGrid = resetFlashedOctopuses(
        Belt.Set.String.toList(flashed) |> List.map(Grid.stringToCoords),
        flashedGrid,
      )

      let flashCount = Belt.Set.String.size(flashed)
      if flashCount == pointsCount {
        Some(step)
      } else {
        doStep(resetGrid, step + 1, totalFlashes + flashCount)
      }
    }
  }

  doStep(input, 1, 0)
}

Js.log2("Part 2", part2())
