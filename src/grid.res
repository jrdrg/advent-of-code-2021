type t<'v> = Belt.Map.String.t<'v>

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

let validPoints = (points: list<(int, int)>, grid: t<'v>) => {
  points |> List.filter(((r, c)) => {
    r >= 0 && c >= 0 && Belt.Map.String.has(grid, coordsToString(r, c))
  })
}

let getNeighbors = (row, col, grid: t<'v>) => {
  let possible = getNeighborCoords(row, col)
  validPoints(possible, grid)
}

let valueAt = (r, c, grid: t<'v>) => {
  Belt.Map.String.get(grid, coordsToString(r, c))
}

let setValueAt = (r, c, v, grid: t<'v>) => {
  Belt.Map.String.set(grid, coordsToString(r, c), v)
}

let toPointsList = (grid: t<'v>) => {
  grid |> Belt.Map.String.keysToArray
}

let fromString = (_input: string): t<'v> => {
  Belt.Map.String.empty
}

let size = (grid: t<'v>): (int, int) => {
  let (mR, mC) =
    Belt.Map.String.keysToArray(grid) |> Array.to_list |> List.fold_left(((curH, curW), point) => {
      let (r, c) = stringToCoords(point)

      (max(curH, r), max(curW, c))
    }, (Int32.min_int |> Int32.to_int, Int32.min_int |> Int32.to_int))

  (mR + 1, mC + 1)
}

let pointsCount = (grid: t<'v>) => {
  Belt.Map.String.size(grid)
}

let fromPointsList = (initialValue: 'v, points: list<(int, int)>): t<'v> => {
  points |> List.fold_left((grid, (r, c)) => {
    Belt.Map.String.set(grid, coordsToString(r, c), initialValue)
  }, Belt.Map.String.empty)
}

let print = (fn: option<'v> => string, grid: t<'v>): string => {
  let (h, w) = size(grid)

  let rec printRow = (r: int, c: int, str: string) => {
    if c < w {
      let val = valueAt(r, c, grid)
      printRow(r, c + 1, str ++ fn(val))
    } else {
      str
    }
  }
  let rec printRows = (r: int, str: string) => {
    if r < h {
      let row = printRow(r, 0, "")
      printRows(r + 1, str ++ "\n" ++ row)
    } else {
      str
    }
  }

  printRows(0, "")
}
