let exampleInputStr = "target area: x=20..30, y=-10..-5"
let inputStr = "target area: x=32..65, y=-225..-177"

let parseInput = input => {
  exception InvalidInput(string)
  let ((x1, y1), (x2, y2)) = switch Js.String.match_(
    %re("/target area: x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)/"),
    input,
  ) {
  | Some([_, x1, x2, y1, y2]) =>
    let c = [x1, x2, y1, y2] |> Array.map(int_of_string)
    ((c[0], c[2]), (c[1], c[3]))
  | _ => raise(InvalidInput(input))
  }

  Grid.fromPointsList(true, list{(y1, x1), (y2, x2)})
}

let hasPassedTargetArea = (pos: (int, int), velocity: (int, int), targetArea: Grid.t<'v>) => {
  let (vy, _) = velocity
  let (py, px) = pos
  let ((minR, _), (_, maxC)) = Grid.boundaries(targetArea)

  if vy < 0 && py < minR {
    true
  } else if px > maxC {
    true
  } else {
    false
  }
}

let canNeverHitTargetArea = ((_, c): (int, int), (_, vx): (int, int), targetArea: Grid.t<'v>) => {
  if vx == 0 {
    let ((_, minC), (_, maxC)) = Grid.boundaries(targetArea)
    c < minC || c > maxC
  } else {
    false
  }
}

let processStep = (pos: (int, int), velocity: (int, int)) => {
  let (py, px) = pos
  let (vy, vx) = velocity
  let nextPoint = (py + vy, px + vx)
  let vx' = if vx > 0 {
    vx - 1
  } else if vx < 0 {
    vx + 1
  } else {
    vx
  }
  let vy' = vy - 1

  (nextPoint, (vy', vx'))
}

type targetHitResult =
  | Hit((int, int), array<(int, int)>)
  | CanNeverHit(int, int)
  | PassedTargetArea(int, int)

let checkIfHitTargetArea = ((vx, vy): (int, int), targetArea: Grid.t<'v>) => {
  let rec determineVelocity = (
    pos: (int, int),
    velocity: (int, int),
    targetArea: Grid.t<'v>,
    points: list<(int, int)>,
  ) => {
    let (nextPoint, nextVelocity) = processStep(pos, velocity)

    if Grid.isWithinBoundaries(nextPoint, targetArea) {
      Hit(velocity, list{nextPoint, ...points} |> List.rev |> Array.of_list)
    } else if canNeverHitTargetArea(nextPoint, nextVelocity, targetArea) {
      let (rDist, cDist) = Grid.distance(nextPoint, targetArea)
      CanNeverHit(rDist, cDist)
    } else if hasPassedTargetArea(nextPoint, nextVelocity, targetArea) {
      let (rDist, cDist) = Grid.distance(nextPoint, targetArea)
      PassedTargetArea(rDist, cDist)
    } else {
      determineVelocity(nextPoint, nextVelocity, targetArea, list{nextPoint, ...points})
    }
  }

  determineVelocity((0, 0), (vy, vx), targetArea, list{})
}

let opposite = (num: int) => {
  if num < 0 {
    1
  } else if num > 0 {
    -1
  } else {
    0
  }
}

let rec findHighestYPosition = (
  velocities: list<(int, int)>,
  highest: option<(int, (int, int))>,
  targetArea: Grid.t<'v>,
) => {
  switch velocities {
  | list{velocity, ...rest} =>
    switch checkIfHitTargetArea(velocity, targetArea) {
    | Hit(_, points) =>
      let maxY = points |> Array.fold_left((mY, (y, _)) => {
        max(mY, y)
      }, 0)
      let newHighest = switch highest {
      | Some((highestY, _)) =>
        if maxY > highestY {
          Some((maxY, velocity))
        } else {
          highest
        }
      | None => Some((maxY, velocity))
      }
      findHighestYPosition(rest, newHighest, targetArea)

    | CanNeverHit(_, _) => findHighestYPosition(rest, highest, targetArea)

    | PassedTargetArea(_, _) => findHighestYPosition(rest, highest, targetArea)
    // highest
    }
  | list{} => highest
  }
}

let part1 = () => {
  let input = parseInput(inputStr)

  let (h, w) = Grid.size(input)
  let m = max(h, w) * 10

  let possibleVelocities =
    Array.init(m, r => Array.init(m, c => (r, c)) |> Array.to_list) |> Array.to_list |> List.flatten

  let highest = findHighestYPosition(possibleVelocities, None, input)

  highest
}

Js.log2("Part 1", part1())

let rec getHitList = (
  velocities: list<(int, int)>,
  hits: list<(int, int)>,
  targetArea: Grid.t<'v>,
) => {
  switch velocities {
  | list{velocity, ...rest} =>
    switch checkIfHitTargetArea(velocity, targetArea) {
    | Hit(_, _) => getHitList(rest, list{velocity, ...hits}, targetArea)
    | _ => getHitList(rest, hits, targetArea)
    }
  | list{} => hits
  }
}

let part2 = () => {
  let input = parseInput(inputStr)

  let (h, w) = Grid.size(input)
  let m = max(h, w) * 10

  let possibleVelocities =
    Array.init(m, r => Array.init(m, c => (r, c)) |> Array.to_list) |> Array.to_list |> List.flatten

  let negativeVelocities = possibleVelocities |> Array.of_list |> Array.map(((r, c)) => (r, -1 * c))

  let possibleVelocities =
    Array.append(Array.of_list(possibleVelocities), negativeVelocities) |> Array.to_list

  let hits = getHitList(possibleVelocities, list{}, input)

  let hitKeys =
    hits
    |> List.map(((r, c)) => j`${string_of_int(r)},${string_of_int(c)}`)
    |> Array.of_list
    |> Belt.Set.String.fromArray

  Belt.Set.String.size(hitKeys)
}

Js.log2("Part 2", part2())
