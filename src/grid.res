type t<'v> = Belt.Map.String.t<'v>

let coordsToString = (r, c) => j`${string_of_int(r)}:${string_of_int(c)}`

let stringToCoords = key => {
  let coords = Js.String.split(":", key) |> Array.map(int_of_string)
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

let getNonDiagonalNeighbors = (row, col, grid: t<'v>) => {
  let possible = getAdjacentNeighborCoords(row, col)
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

let fromString = (converter: string => 'v, input: string): t<'v> => {
  let rows = input |> String.split_on_char('\n')
  rows |> List.mapi((i, row) => (i, row)) |> List.fold_left((map, (rIndex, row)) => {
    row
    |> Js.String.split("")
    |> Array.to_list
    |> List.mapi((i, row) => (i, row))
    |> List.fold_left((map2, (cIndex, col)) => {
      Belt.Map.String.set(map2, coordsToString(rIndex, cIndex), converter(col))
    }, map)
  }, Belt.Map.String.empty)
}

let boundaries = (grid: t<'v>) => {
  let (minR, maxR, minC, maxC) =
    Belt.Map.String.keysToArray(grid)
    |> Array.to_list
    |> List.fold_left(((curMinR, curMaxR, curMinC, curMaxC), point) => {
      let (r, c) = stringToCoords(point)

      (min(curMinR, r), max(curMaxR, r), min(curMinC, c), max(curMaxC, c))
    }, (
      Int32.max_int |> Int32.to_int,
      Int32.min_int |> Int32.to_int,
      Int32.max_int |> Int32.to_int,
      Int32.min_int |> Int32.to_int,
    ))

  ((minR, minC), (maxR, maxC))
}

let size = (grid: t<'v>): (int, int) => {
  let ((minR, minC), (maxR, maxC)) = boundaries(grid)
  (maxR - minR, maxC - minC)
}

let pointsCount = (grid: t<'v>) => {
  Belt.Map.String.size(grid)
}

let fromPointsList = (initialValue: 'v, points: list<(int, int)>): t<'v> => {
  points |> List.fold_left((grid, (r, c)) => {
    Belt.Map.String.set(grid, coordsToString(r, c), initialValue)
  }, Belt.Map.String.empty)
}

let fromPointsAndValueList = (points: list<(int, int, 'v)>): t<'v> => {
  points |> List.fold_left((grid, (r, c, v)) => {
    Belt.Map.String.set(grid, coordsToString(r, c), v)
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

let isWithinBoundaries = ((r, c): (int, int), grid: t<'v>) => {
  let ((minR, minC), (maxR, maxC)) = boundaries(grid)

  r >= minR && r <= maxR && c >= minC && c <= maxC
}

let distance = ((r, c): (int, int), grid: t<'v>) => {
  let ((minR, minC), (maxR, maxC)) = boundaries(grid)
  let rDist = if r < minR {
    r - minR
  } else if r >= minR && r < maxR {
    0
  } else {
    r - maxR
  }
  let cDist = if c < minC {
    c - minC
  } else if c >= minC && c < maxC {
    0
  } else {
    c - maxC
  }

  (rDist, cDist)
}
