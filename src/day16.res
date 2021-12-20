let shortExampleStr = "D2FE28"
let operatorExampleStr = "38006F45291200"
let operatorCountExampleStr = "EE00D40C823060"

let inputStr = "E0529D18025800ABCA6996534CB22E4C00FB48E233BAEC947A8AA010CE1249DB51A02CC7DB67EF33D4002AE6ACDC40101CF0449AE4D9E4C071802D400F84BD21CAF3C8F2C35295EF3E0A600848F77893360066C200F476841040401C88908A19B001FD35CCF0B40012992AC81E3B980553659366736653A931018027C87332011E2771FFC3CEEC0630A80126007B0152E2005280186004101060C03C0200DA66006B8018200538012C01F3300660401433801A6007380132DD993100A4DC01AB0803B1FE2343500042E24C338B33F5852C3E002749803B0422EC782004221A41A8CE600EC2F8F11FD0037196CF19A67AA926892D2C643675A0C013C00CC0401F82F1BA168803510E3942E969C389C40193CFD27C32E005F271CE4B95906C151003A7BD229300362D1802727056C00556769101921F200AC74015960E97EC3F2D03C2430046C0119A3E9A3F95FD3AFE40132CEC52F4017995D9993A90060729EFCA52D3168021223F2236600ECC874E10CC1F9802F3A71C00964EC46E6580402291FE59E0FCF2B4EC31C9C7A6860094B2C4D2E880592F1AD7782992D204A82C954EA5A52E8030064D02A6C1E4EA852FE83D49CB4AE4020CD80272D3B4AA552D3B4AA5B356F77BF1630056C0119FF16C5192901CEDFB77A200E9E65EAC01693C0BCA76FEBE73487CC64DEC804659274A00CDC401F8B51CE3F8803B05217C2E40041A72E2516A663F119AC72250A00F44A98893C453005E57415A00BCD5F1DD66F3448D2600AC66F005246500C9194039C01986B317CDB10890C94BF68E6DF950C0802B09496E8A3600BCB15CA44425279539B089EB7774DDA33642012DA6B1E15B005C0010C8C917A2B880391160944D30074401D845172180803D1AA3045F00042630C5B866200CC2A9A5091C43BBD964D7F5D8914B46F040"

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
  length: int,
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
  j`<v(${pVersion(packet)}):${packetTypeToString(packet.packetType)}__${string_of_int(
      packet.length,
    )}>`
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

let parseLiteral = (binary: Binary.t, index: int) => {
  exception InvalidLiteralValue(int, string)

  let rec parseLiteralRec = (idx: int, value: string) => {
    switch Array.sub(binary, idx, 5) {
    | [1, a, b, c, d] =>
      let val = value ++ ([a, b, c, d] |> Array.map(string_of_int) |> Js.Array.joinWith(""))
      parseLiteralRec(idx + 5, val)

    | [0, a, b, c, d] =>
      let val = value ++ ([a, b, c, d] |> Array.map(string_of_int) |> Js.Array.joinWith(""))
      (Binary.stringToNumber(val) |> Binary.toDecimal, idx + 5)

    | _ => raise(InvalidLiteralValue(index, ""))
    }
  }

  parseLiteralRec(index, "")
}

type lengthTypeId = TotalLength(int) | SubpacketsCount(int)

let lengthTypeIdToString = typeId => {
  switch typeId {
  | TotalLength(l) => j`TotalLength(${string_of_int(l)})`
  | SubpacketsCount(c) => j`SubpacketsCount(${string_of_int(c)})`
  }
}

let determineOperatorType = (index: int, binary: Binary.t): (lengthTypeId, int) => {
  exception InvalidLengthTypeId(string)

  switch binary[index] {
  | 0 =>
    let totalLength = Array.sub(binary, index + 1, 15) |> Binary.toDecimal
    (TotalLength(int_of_float(totalLength)), index + 16)

  | 1 =>
    let subPacketCount = Array.sub(binary, index + 1, 11) |> Binary.toDecimal
    (SubpacketsCount(int_of_float(subPacketCount)), index + 12)

  | _ => raise(InvalidLengthTypeId(binary |> Js.Array.joinWith("")))
  }
}

let hasRemainingPackets = (index: int, binary: Binary.t) => {
  index < Array.length(binary) - (6 + 1)
}

let splitPacket = (index: int, binary: Binary.t) => {
  let version = Array.sub(binary, index, 3) |> Binary.toDecimal
  let typeId = Array.sub(binary, index + 3, 3) |> Binary.toDecimal
  let value = Array.sub(binary, index + 6, Array.length(binary) - (index + 6))

  ((version, typeId, value), index + 6)
}

let packetsToString = packets => {
  packets |> List.map(packetToString) |> Array.of_list
}

type currentOperation = {
  op: lengthTypeId,
  remaining: int,
  packet: packet,
}

let updatePendingPacket = (stackPacket: packet, currentPacket: packet) => {
  exception InvalidSubpacket(packet)
  switch stackPacket {
  | {packetType: Operator(t, opPackets), length} =>
    let subpackets = List.append(opPackets, list{currentPacket})
    let length = length + currentPacket.length
    {...stackPacket, packetType: Operator(t, subpackets), length: length}

  | {packetType: Literal(_)} => raise(InvalidSubpacket(stackPacket))
  }
}

let rec decodePackets = (
  binary: Binary.t,
  index: int,
  pendingPacketStack: list<currentOperation>,
  packets: list<packet>,
) => {
  let isComplete = !hasRemainingPackets(index, binary)

  switch (isComplete, pendingPacketStack, packets) {
  | (
      _,
      list{{op: TotalLength(length), remaining, packet}, ...restPending},
      list{firstPacket, ...restPackets},
    ) =>
    // pop off the stack and add to packets
    let updatedOpPacket = updatePendingPacket(packet, firstPacket)
    let remaining = remaining - firstPacket.length

    if remaining == 0 {
      let packetList = list{updatedOpPacket, ...restPackets}
      let stack = restPending

      decodePackets(binary, index, stack, packetList)
    } else {
      let packetList = restPackets
      let stack = list{
        {op: TotalLength(length), remaining: remaining, packet: updatedOpPacket},
        ...restPending,
      }
      decodePackets(binary, index, stack, packetList)
    }

  | (
      _,
      list{{op: SubpacketsCount(count), remaining, packet}, ...restPending},
      list{firstPacket, ...restPackets},
    ) =>
    // pop off the stack and add to packets
    let updatedOpPacket = updatePendingPacket(packet, firstPacket)

    let remaining = remaining - 1

    if remaining == 0 {
      let packetList = list{updatedOpPacket, ...restPackets}
      let stack = restPending

      decodePackets(binary, index, stack, packetList)
    } else {
      let packetList = restPackets
      let stack = list{
        {op: SubpacketsCount(count), remaining: remaining, packet: updatedOpPacket},
        ...restPending,
      }
      decodePackets(binary, index, stack, packetList)
    }

  | (true, _, _) => packets

  // | (list{}, list{packet, ...restPackets}) => // continue, nothing to update
  // | (list{pending, ...restPending}, list{}) => // continue, can't pop
  // | (list{}, list{}) => // continue

  | _ =>
    let ((version, typeId, _value), nextIndex) = splitPacket(index, binary)

    switch typeId {
    | 4. =>
      let (literalValue, idx) = parseLiteral(binary, nextIndex)
      let length = idx - index

      let packet = {
        version: version,
        packetType: Literal(literalValue),
        length: length,
      }

      decodePackets(binary, idx, pendingPacketStack, list{packet, ...packets})

    | t =>
      let (operatorType, packetStart) = determineOperatorType(nextIndex, binary)

      let opLength = packetStart - index

      switch operatorType {
      | TotalLength(length) =>
        let op = {
          op: TotalLength(length),
          remaining: length,
          packet: {
            version: version,
            packetType: Operator(int_of_float(t), list{}),
            length: opLength,
          },
        }
        decodePackets(binary, packetStart, list{op, ...pendingPacketStack}, packets)

      | SubpacketsCount(count) =>
        let op = {
          op: SubpacketsCount(count),
          remaining: count,
          packet: {
            version: version,
            packetType: Operator(int_of_float(t), list{}),
            length: opLength,
          },
        }
        decodePackets(binary, packetStart, list{op, ...pendingPacketStack}, packets)
      }
    }
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
  let input = parseInput(inputStr)
  let packets = decodePackets(input, 0, list{}, list{})

  versionSum(packets, 0.)
}

Js.log2("Part 1", part1())

let rec determinePacketValue = (packets: list<packet>, value: float) => {
  exception InvalidPacketTypeId(int)

  switch packets {
  | list{} => value
  | list{{packetType: Literal(lvalue)}, ...rest} => determinePacketValue(rest, value +. lvalue)
  | list{{packetType: Operator(opType, subpackets)}, ...rest} =>
    switch opType {
    | 0 =>
      let sum = subpackets |> List.fold_left((psum, packet) => {
        psum +. determinePacketValue(list{packet}, 0.)
      }, 0.)
      determinePacketValue(rest, value +. sum)
    | 1 =>
      let product = subpackets |> List.fold_left((prod, packet) => {
        prod *. determinePacketValue(list{packet}, 0.)
      }, 1.)
      determinePacketValue(rest, value +. product)
    | 2 =>
      let minimum = subpackets |> List.fold_left((m, packet) => {
        let val = determinePacketValue(list{packet}, 0.)
        min(m, val)
      }, infinity)
      determinePacketValue(rest, value +. minimum)
    | 3 =>
      let maximum = subpackets |> List.fold_left((m, packet) => {
        let val = determinePacketValue(list{packet}, 0.)
        max(m, val)
      }, infinity *. -1.)
      determinePacketValue(rest, value +. maximum)
    | 5 =>
      let gt = switch subpackets {
      | list{first, second} =>
        if determinePacketValue(list{first}, 0.) > determinePacketValue(list{second}, 0.) {
          1.
        } else {
          0.
        }
      | _ => raise(InvalidPacketTypeId(5))
      }
      determinePacketValue(rest, value +. gt)
    | 6 =>
      let lt = switch subpackets {
      | list{first, second} =>
        if determinePacketValue(list{first}, 0.) < determinePacketValue(list{second}, 0.) {
          1.
        } else {
          0.
        }
      | _ => raise(InvalidPacketTypeId(5))
      }
      determinePacketValue(rest, value +. lt)
    | 7 =>
      let eq = switch subpackets {
      | list{first, second} =>
        if determinePacketValue(list{first}, 0.) == determinePacketValue(list{second}, 0.) {
          1.
        } else {
          0.
        }
      | _ => raise(InvalidPacketTypeId(5))
      }
      determinePacketValue(rest, value +. eq)
    | t => raise(InvalidPacketTypeId(t))
    }
  }
}

let part2 = () => {
  let input = parseInput(inputStr)
  let packets = decodePackets(input, 0, list{}, list{})

  determinePacketValue(packets, 0.)
}

Js.log2("Part 2", part2())
