let exampleInputStr = "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

let exampleInputStr2 = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

let inputStr = "EO-jc
end-tm
jy-FI
ek-EO
mg-ek
jc-jy
FI-start
jy-mg
mg-FI
jc-tm
end-EO
ds-EO
jy-start
tm-EO
mg-jc
ek-jc
tm-ek
FI-jc
jy-EO
ek-jy
ek-LT
start-mg"

module CaveMap = {
  type t = Belt.Map.String.t<Belt.Set.String.t>

  let fromString = input => {
    input
    |> String.split_on_char('\n')
    |> List.map(String.split_on_char('-'))
    |> List.map(Array.of_list)
    |> List.fold_left((arrs, next) => {
      let s = next[0]
      let e = next[1]
      list{[s, e], [e, s], ...arrs}
    }, list{})
    |> List.fold_left((map, coords) => {
      let start = coords[0]
      let end = coords[1]
      Belt.Map.String.update(map, start, v => {
        switch v {
        | Some(set) => Some(Belt.Set.String.add(set, end))
        | None => Some(Belt.Set.String.add(Belt.Set.String.empty, end))
        }
      })
    }, Belt.Map.String.empty)
  }

  let connectionsFrom = (cave: string, map: t) => {
    Belt.Map.String.getExn(map, cave)
  }
}

let possibleCaves = (cave: string, visited: Belt.Set.String.t, map: CaveMap.t) => {
  CaveMap.connectionsFrom(cave, map)->Belt.Set.String.keep(v => !Belt.Set.String.has(visited, v))
    |> Belt.Set.String.toList
}

let isLowercase = str => {
  String.lowercase_ascii(str) == str
}

let rec findPathsFrom = (paths: list<list<string>>, found: list<list<string>>, map: CaveMap.t) => {
  switch paths {
  | list{path, ...remainingPaths} =>
    let currentPathVisited = Belt.Set.String.fromArray(
      path |> List.filter(isLowercase) |> Array.of_list,
    )
    let possible = possibleCaves(List.hd(path), currentPathVisited, map)
    if List.length(possible) == 0 {
      findPathsFrom(remainingPaths, found, map)
    } else {
      let (updatedPaths, found) = possible |> List.fold_left(((paths, found), cave) => {
        let newPath = list{cave, ...path}
        if cave == "end" {
          (paths, list{newPath, ...found})
        } else {
          (list{newPath, ...paths}, found)
        }
      }, (list{}, found))

      let allPaths = List.append(updatedPaths, remainingPaths)

      findPathsFrom(allPaths, found, map)
    }

  | list{} => found
  }
}

let part1 = () => {
  let input = CaveMap.fromString(inputStr)
  let paths = input |> findPathsFrom(list{list{"start"}}, list{})

  List.length(paths)
}

Js.log2("Part 1", part1())

let possibleCaves2 = (cave: string, visited: Belt.Map.String.t<int>, map: CaveMap.t) => {
  let alreadyVisitedSmallCaveTwice = Belt.Map.String.findFirstBy(visited, (_, v) => v > 1)

  CaveMap.connectionsFrom(cave, map)->Belt.Set.String.keep(v =>
    switch (v, alreadyVisitedSmallCaveTwice) {
    | ("end" | "start", _) => Belt.Map.String.getWithDefault(visited, v, 0) == 0
    | (_, Some(_)) => Belt.Map.String.getWithDefault(visited, v, 0) < 1
    | (_, None) => Belt.Map.String.getWithDefault(visited, v, 0) < 2
    }
  ) |> Belt.Set.String.toList
}

let rec findPathsFrom2 = (paths: list<list<string>>, found: list<list<string>>, map: CaveMap.t) => {
  switch paths {
  | list{path, ...remainingPaths} =>
    let visitCounts = path |> List.filter(isLowercase) |> List.fold_left((counts, cave) => {
        Belt.Map.String.update(counts, cave, v =>
          switch v {
          | Some(count) => Some(count + 1)
          | None => Some(1)
          }
        )
      }, Belt.Map.String.empty)

    let possible = possibleCaves2(List.hd(path), visitCounts, map)

    if List.length(possible) == 0 {
      findPathsFrom2(remainingPaths, found, map)
    } else {
      let (updatedPaths, found) = possible |> List.fold_left(((paths, found), cave) => {
        let newPath = list{cave, ...path}
        if cave == "end" {
          (paths, list{newPath, ...found})
        } else {
          (list{newPath, ...paths}, found)
        }
      }, (list{}, found))

      let allPaths = List.append(updatedPaths, remainingPaths)

      findPathsFrom2(allPaths, found, map)
    }

  | list{} => found
  }
}

let part2 = () => {
  let input = CaveMap.fromString(inputStr)
  let paths = input |> findPathsFrom2(list{list{"start"}}, list{})

  List.length(paths)
}

Js.log2("Part 2", part2())
