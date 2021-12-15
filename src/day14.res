let exampleInputStr = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let inputStr = "FPNFCVSNNFSFHHOCNBOB

ON -> S
SO -> B
OH -> C
SN -> F
BP -> O
SK -> F
OO -> K
CF -> O
PP -> F
KS -> K
KN -> B
BN -> H
HN -> H
NP -> P
BB -> N
SB -> F
BH -> V
NV -> S
PO -> S
CN -> N
VP -> B
HH -> B
NB -> V
NF -> O
BV -> B
CV -> B
SS -> H
CB -> C
VN -> S
FH -> K
BF -> H
NH -> P
PV -> K
OP -> F
HO -> N
SH -> C
VH -> P
VK -> B
OF -> F
KK -> B
SC -> H
CO -> S
BK -> V
PF -> B
OK -> K
FO -> V
CH -> O
KO -> B
CS -> V
OC -> P
SP -> V
KF -> C
HV -> S
KH -> B
VS -> K
KB -> F
FF -> P
VF -> H
NC -> S
HB -> V
NN -> C
FV -> B
PH -> V
KV -> C
PB -> C
OS -> O
PS -> H
FS -> N
FP -> O
VV -> O
FN -> V
NO -> K
NK -> V
OB -> F
PC -> O
OV -> H
FK -> C
HS -> F
SF -> N
VC -> C
BS -> N
PK -> O
FB -> S
CK -> B
KP -> N
KC -> F
BC -> F
HK -> H
VO -> O
NS -> B
VB -> K
FC -> K
SV -> O
HF -> H
HC -> C
CP -> O
CC -> P
PN -> P
HP -> C
BO -> F"

exception InvalidInput(string)

type pairs = Belt.Map.String.t<string>

let parseInput = input => {
  switch input |> String.split_on_char('\n') |> List.filter(i => String.length(i) != 0) {
  | list{template, ...rest} =>
    let pairs =
      rest
      |> List.map(pair =>
        switch pair |> Js.String.split(" -> ") {
        | [first, second] => (first, second)
        | _ => raise(InvalidInput(pair))
        }
      )
      |> List.fold_left((map, (first, second)) => {
        Belt.Map.String.set(map, first, second)
      }, Belt.Map.String.empty)

    (template, pairs)
  | _ => raise(InvalidInput(input))
  }
}

let processStep = (pairs: pairs, template: string) => {
  let templateLetters = template |> Js.String.split("")

  templateLetters
  |> Array.mapi((i, letter) => (i, letter))
  |> Array.fold_left((output, (index, letter)) => {
    if index == Array.length(templateLetters) - 1 {
      list{letter, ...output}
    } else {
      let first = templateLetters[index]
      let second = templateLetters[index + 1]

      let pair = first ++ second
      switch Belt.Map.String.get(pairs, pair) {
      | Some(letterToInsert) => list{letterToInsert, first, ...output}
      | None => list{letter, ...output}
      }
    }
  }, list{})
  |> List.rev
  |> Array.of_list
  |> Js.Array.joinWith("")
}

let rec processSteps = (pairs: pairs, template: string, step: int) => {
  if step <= 0 {
    template
  } else {
    let processed = processStep(pairs, template)
    processSteps(pairs, processed, step - 1)
  }
}

let getLetterCounts = (str: string) => {
  str |> Js.String.split("") |> Array.fold_left((counts, letter) => {
    Belt.Map.String.update(counts, letter, v =>
      switch v {
      | Some(count) => Some(count + 1)
      | None => Some(1)
      }
    )
  }, Belt.Map.String.empty)
}

let part1 = () => {
  let (template, pairs) = parseInput(inputStr)

  let output = processSteps(pairs, template, 10)

  let counts =
    output
    |> getLetterCounts
    |> Belt.Map.String.toList
    |> List.sort(((_, a), (_, b)) => a - b)
    |> Array.of_list

  let (_, least) = counts[0]
  let (_, most) = counts[Array.length(counts) - 1]

  most - least
}

Js.log2("Part 1", part1())

type template = {
  pairs: Belt.Map.String.t<float>,
  letters: Belt.Map.String.t<float>,
}

let pairLetters = (pair: string): (string, string) => {
  let arr = pair |> Js.String.split("")
  (arr[0], arr[1])
}

let increment = (amount, v) =>
  switch v {
  | Some(count) => Some(count +. amount)
  | None => Some(amount)
  }

let decrement = (amount, v) =>
  switch v {
  | Some(count) if count > amount => Some(count -. amount)
  | _ => None
  }

let processStep2 = (pairMappings: pairs, template: template) => {
  let (pairs, letters) =
    Belt.Map.String.toList(template.pairs) |> List.fold_left(((p, l), (pair, _)) => {
      // for each pair, if it exists then insert 2 items in the new map
      // AB=>C, AB = [AC +1, CB +1]
      // and decrement the original pair [AB -1]
      // letters C +1

      switch (Belt.Map.String.get(template.pairs, pair), Belt.Map.String.get(pairMappings, pair)) {
      | (Some(count), Some(toInsert)) =>
        // AB exists
        let (first, second) = pairLetters(pair)
        let newPair1 = first ++ toInsert
        let newPair2 = toInsert ++ second

        let pairs =
          Belt.Map.String.update(p, newPair1, increment(count))
          ->Belt.Map.String.update(newPair2, increment(count))
          ->Belt.Map.String.update(pair, decrement(count))

        let letters = Belt.Map.String.update(l, toInsert, increment(count))

        (pairs, letters)
      | _ => (p, l)
      }
    }, (template.pairs, template.letters))

  {pairs: pairs, letters: letters}
}

let stringToTemplate = (str: string): template => {
  let templateLetters = str |> Js.String.split("")

  let increment = v =>
    switch v {
    | Some(count) => Some(count +. 1.)
    | None => Some(1.)
    }

  let (pairs, letters) =
    templateLetters
    |> Array.mapi((i, letter) => (i, letter))
    |> Array.fold_left(((p, l), (index, letter)) => {
      if index == Array.length(templateLetters) - 1 {
        let letters = Belt.Map.String.update(l, letter, increment)
        (p, letters)
      } else {
        let first = templateLetters[index]
        let second = templateLetters[index + 1]

        let pair = first ++ second
        let pairs = Belt.Map.String.update(p, pair, increment)
        let letters = Belt.Map.String.update(l, first, increment)

        (pairs, letters)
      }
    }, (Belt.Map.String.empty, Belt.Map.String.empty))

  {pairs: pairs, letters: letters}
}

let rec processSteps2 = (pairs: pairs, template: template, step: int) => {
  if step <= 0 {
    template
  } else {
    let processed = processStep2(pairs, template)
    processSteps2(pairs, processed, step - 1)
  }
}

let part2 = () => {
  let (templateStr, pairMappings) = parseInput(inputStr)
  let template = stringToTemplate(templateStr)

  let p = processSteps2(pairMappings, template, 40)

  let counts =
    Belt.Map.String.toList(p.letters)
    |> List.sort(((_, a), (_, b)) =>
      switch a -. b {
      | x if x > 0. => 1
      | x if x < 0. => -1
      | _ => 0
      }
    )
    |> Array.of_list

  let (_, least) = counts[0]
  let (_, most) = counts[Array.length(counts) - 1]

  most -. least
}

Js.log2("Part 2", part2())
