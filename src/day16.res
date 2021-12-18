let shortExampleStr = "D2FE28"
let operatorExampleStr = "38006F45291200"
let operatorCountExampleStr = "EE00D40C823060"

module Binary = {
  type t = array<int>

  exception InvalidHexDigit(string)

  let fromHex = (hex: string) => {
    switch hex {
    | "0" => "0000"
    | "1" => "0001"
    | "2" => "0010"
    | "3" => "0011"
    | "4" => "0100"
    | "5" => "0101"
    | "6" => "0110"
    | "7" => "0111"
    | "8" => "1000"
    | "9" => "1001"
    | "A" => "1010"
    | "B" => "1011"
    | "C" => "1100"
    | "D" => "1101"
    | "E" => "1110"
    | "F" => "1111"
    | _ => raise(InvalidHexDigit(hex))
    }
  }

  let stringToNumber = (binaryString): t => {
    binaryString |> Js.String.split("") |> Array.map(int_of_string)
  }

  let toDecimal = (binary: t) => {
    let l = Array.length(binary)
    binary |> Array.mapi((i, digit) => (l - i - 1, digit)) |> Array.fold_left((dec, (i, digit)) => {
      let value = 2. ** float_of_int(i) *. float_of_int(digit)
      dec +. value
    }, 0.)
  }
}

type rec packet = {
  version: float,
  packetType: packetType,
}
and packetType = Literal(float) | Operator(int, list<packet>)

let pVersion = packet => {
  Js.Float.toString(packet.version)
}

let rec packetTypeToString = packetType => {
  switch packetType {
  | Literal(v) => j`Literal ${Js.Float.toString(v)}`
  | Operator(typeId, packets) =>
    let subpackets = packets |> List.map(packetToString) |> Array.of_list |> Js.Array.joinWith(",")
    j`Operator ${string_of_int(typeId)} [${subpackets}]`
  }
}
and packetToString = (packet: packet) => {
  j`<v(${pVersion(packet)}):${packetTypeToString(packet.packetType)}>`
}

let parseInput = input => {
  input
  |> Js.String.split("")
  |> Array.map(Binary.fromHex)
  |> Js.Array.joinWith("")
  |> Binary.stringToNumber
}

let a2s = arr => arr |> Js.Array.joinWith("")

let rec parseLiteralValue = (binary: list<int>, value: string, length: int): (float, int) => {
  exception InvalidLiteralValue(string)

  switch binary {
  | list{1, a, b, c, d, ...rest} =>
    let val = value ++ ([a, b, c, d] |> Array.map(string_of_int) |> Js.Array.joinWith(""))
    parseLiteralValue(rest, val, length + 5)

  | list{0, a, b, c, d, ..._rest} =>
    let val = value ++ ([a, b, c, d] |> Array.map(string_of_int) |> Js.Array.joinWith(""))
    (Binary.stringToNumber(val) |> Binary.toDecimal, length + 5)

  | _ => raise(InvalidLiteralValue(binary |> Array.of_list |> Js.Array.joinWith("")))
  }
}

type lengthTypeId = TotalLength(int) | SubpacketsCount(int)

let lengthTypeIdToString = typeId => {
  switch typeId {
  | TotalLength(l) => j`TotalLength(${string_of_int(l)})`
  | SubpacketsCount(c) => j`SubpacketsCount(${string_of_int(c)})`
  }
}

let determineOperatorType = (binary: Binary.t): (lengthTypeId, int) => {
  exception InvalidLengthTypeId(string)
  switch binary[0] {
  | 0 =>
    let totalLength = Array.sub(binary, 1, 15) |> Binary.toDecimal
    (TotalLength(int_of_float(totalLength)), 16)

  | 1 =>
    let subPacketCount = Array.sub(binary, 1, 11) |> Binary.toDecimal
    (SubpacketsCount(int_of_float(subPacketCount)), 12)

  | _ => raise(InvalidLengthTypeId(binary |> Js.Array.joinWith("")))
  }
}

let splitPacket = (binary: Binary.t) => {
  let version = Array.sub(binary, 0, 3) |> Binary.toDecimal
  let typeId = Array.sub(binary, 3, 3) |> Binary.toDecimal
  let value = Array.sub(binary, 6, Array.length(binary) - 6)

  (version, typeId, value)
}

let packetsToString = packets => {
  packets |> List.map(packetToString) |> Array.of_list
}

let rec decodeSubpacketsByLength = (
  binary: Binary.t,
  subpackets: list<packet>,
  currentLength: int,
  totalLength: int,
) => {
  Js.log2("decoding subpackets by length", (currentLength, totalLength, Array.length(binary)))
  if currentLength >= totalLength {
    Js.log2(" -- COMPLETED READING length", packetsToString(subpackets))
    Js.log2(" ?? sanity check", (Array.length(binary), currentLength))
    subpackets
  } else {
    let subArray = Array.sub(binary, currentLength, Array.length(binary) - currentLength)
    Js.log2(" -- length", Array.length(subArray))

    let (version, typeId, value) = splitPacket(subArray)
    let (subpacket, length) = decodePacket(subArray)
    Js.log2(" -- subpacket split", (version, typeId, a2s(value), length))
    Js.log2(" -- subpacket", packetToString(subpacket))

    decodeSubpacketsByLength(
      subArray,
      list{subpacket, ...subpackets},
      currentLength + length,
      totalLength,
    )
  }
}
and decodeSubpacketsByCount = (
  binary: Binary.t,
  subpackets: list<packet>,
  currentCount: int,
  totalCount: int,
  arrayPos: int,
) => {
  Js.log2("decoding subpackets by count", (currentCount, totalCount, Array.length(binary)))
  if currentCount >= totalCount {
    Js.log2(" -- COMPLETED READING count", (packetsToString(subpackets), arrayPos))
    (subpackets, arrayPos)
  } else {
    let (subpacket, length) = decodePacket(binary)
    Js.log2(" > decoded by count", (packetToString(subpacket), length))

    let newLength = Array.length(binary) - length
    let subArray = Array.sub(binary, length, newLength)

    Js.log2(" >> new length", (length, newLength))
    Js.log2(" >> new length", (length, newLength, Array.length(binary), Array.length(subArray)))
    decodeSubpacketsByCount(
      subArray,
      list{subpacket, ...subpackets},
      currentCount + 1,
      totalCount,
      length,
    )
  }
}
and decodePacket = (binary: Binary.t): (packet, int) => {
  let (version, typeId, value) = splitPacket(binary)
  let headerLength = 6

  Js.log2("\n\n -- Decoding packet, length", Array.length(binary))
  Js.log2(" -- contents", a2s(binary))

  switch typeId {
  | 4. =>
    let (literalValue, length) = parseLiteralValue(Array.to_list(value), "", 0)
    Js.log2(" +++ Literal", (literalValue, length))
    (
      {
        version: version,
        packetType: Literal(literalValue),
      },
      length + headerLength,
    )

  | t =>
    let (operatorType, packetStart) = determineOperatorType(value)
    let packetValues = Array.sub(value, packetStart, Array.length(value) - packetStart)

    Js.log2("operator", (version, typeId, lengthTypeIdToString(operatorType)))
    Js.log2("pv", (a2s(packetValues), Array.length(packetValues)))

    let (subpackets, length) = switch operatorType {
    | TotalLength(length) =>
      Js.log2(" +++ Operator, length", length)
      let sub = Array.sub(packetValues, 0, length)
      let s = decodeSubpacketsByLength(sub, list{}, 0, length)
      let diff = Array.length(packetValues) - length
      Js.log2(" === diff", (diff, Array.length(packetValues), length))

      (s, length + headerLength + packetStart)

    | SubpacketsCount(count) =>
      Js.log2(" +++ Operator, count", count)
      let (s, subpacketsLength) = decodeSubpacketsByCount(packetValues, list{}, 0, count, 0)
      let diff = Array.length(value) - subpacketsLength
      Js.log2(" === l", (subpacketsLength, diff, subpacketsLength + headerLength + packetStart))
      // (s, diff + headerLength + packetStart)
      (s, subpacketsLength + headerLength + packetStart)
    }

    ({version: version, packetType: Operator(int_of_float(t), List.rev(subpackets))}, length)
  }
}

let rec versionSum = (packets: list<packet>, sum: float) => {
  switch packets {
  | list{} => sum
  | list{{version, packetType: Literal(_)}, ...rest} => versionSum(rest, sum +. version)
  | list{{version, packetType: Operator(_, subpackets)}, ...rest} =>
    let remainingPackets = List.append(subpackets, rest)
    versionSum(remainingPackets, sum +. version)
  }
}

let part1 = () => {
  // let input = parseInput(shortExampleStr)
  // let input = parseInput(operatorExampleStr)
  let input = parseInput(operatorCountExampleStr)
  // let input = parseInput("EE00D40C823060")
  // let input = parseInput("8A004A801A8002F478")
  // let input = parseInput("620080001611562C8802118E34")
  let input = parseInput("C0015000016115A2E0802F182340")
  // let input = parseInput("A0016C880162017C3686B18A3D4780")

  let _input =
    // "00000000000000000101100001000101010110001011001000100000000010000100011000111000110100"
    "1100000000000001010100000000000000000001011000010001010110100010111000001000000000101111000110000010001101000000"
    |> Js.String.split("")
    |> Array.map(int_of_string)

  let (decoded, length) = decodePacket(input)

  Js.log2("input", input |> a2s)
  Js.log2("decoded", decoded)

  Js.log2("Decoded", packetToString(decoded))
  Js.log2("Length", length)
  Js.log2("Sum", versionSum(list{decoded}, 0.))

  decoded
}

Js.log2("Part 1", part1())

/*

104
01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100
011000 = v4, type 0 (operator)

98
10000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100
length type 1

97
0000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100

length 11
00000000010 = 2

86
00000000000000000101100001000101010110001011001000100000000010000100011000111000110100
000000 = v0, type 0 (operator)

80
00000000000101100001000101010110001011001000100000000010000100011000111000110100
length type 0

79
0000000000101100001000101010110001011001000100000000010000100011000111000110100

length 15
000000000010110 = 22

64
0001000101010110001011001000100000000010000100011000111000110100

22 bits
0001000101010110001011

000100 = v0, type 4 (literal)

0101010110001011
01010 = 0, 1010 = 10

10110001011
101100 = v5, type 4 (literal)

01011 = 0, 1011 = 11

remaining (42)
001000100000000010000100011000111000110100

*/

/*

112
1100000000000001010100000000000000000001011000010001010110100010111000001000000000101111000110000010001101000000



*/
