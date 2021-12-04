// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("rescript/lib/js/list.js");
var $$Array = require("rescript/lib/js/array.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_format = require("rescript/lib/js/caml_format.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

var inputStr = "4,75,74,31,76,79,27,19,69,46,98,59,83,23,90,52,87,6,11,92,80,51,43,5,94,17,15,67,25,30,48,47,62,71,85,58,60,1,72,99,3,35,42,10,96,49,37,36,8,44,70,40,45,39,0,63,2,78,68,53,50,77,20,55,38,86,54,93,26,88,12,91,95,34,9,14,33,66,41,13,28,57,29,73,56,22,89,21,64,61,32,65,97,84,18,82,81,7,16,24\n\n30 46 94 20  2\n53 67 69 75 65\n27 24 85 28 60\n57 58 42 36 78\n35 98 87 91 93\n\n72 71 91 73 19\n 2 13 14  8 74\n42 34 31 56  9\n82 59 44 67 79\n49  6 98 10 30\n\n95 24 25 11 34\n57 65 41 92  8\n91 26  1 62 38\n47 93  4 37  0\n15 44 33 20 97\n\n24 69 55  7 25\n45 64 56 71 18\n94 10 62 19 36\n53 74 49 61 80\n50 68 60 76 84\n\n86 78 29  1 71\n 2  9 24 34 96\n47 75 61 13 26\n10 66 28 83 14\n91 63 45 76 50\n\n61 60 22 11 95\n25 81 13 15 53\n59 89 65 18 39\n58 50  1 47 52\n48 16 29 75 56\n\n62  0 93 41 53\n69 47 29 50 46\n81  8 20 38 23\n 4 64  5 37 27\n32 75 48 33 15\n\n97 75 15 55 36\n98 77 76  3 69\n11 39 88 18 93\n94 99 59 50 63\n33 26 35 58 14\n\n58 91  7 36 81\n44 90 46 57 93\n16 35 28 61 34\n60  3 96 65 14\n24 49 94 11 77\n\n 5 91 53 85 36\n 6 64 41  7 50\n87 94 96 15 49\n18 78 37 52 75\n28 34 16 71 48\n\n75 14  2 52 49\n79 37 13 53 12\n91 73 94 72 36\n48 54  3 93  5\n40 85 42  9 50\n\n26 53 24 58 95\n15 54 65 80 30\n90 72 27 40 47\n81 22 57  1 17\n82 46 20 94 49\n\n60 25 86 18 92\n 2 85 89  5 55\n12 71 74 46 68\n33 52 82 84 29\n76 43 40 11 31\n\n21 23 93 46 60\n99 20 75 55  4\n73  9 74 92 16\n25 35  0 70 90\n27 86 42 94 15\n\n69 73 42 46 53\n 5 71 50  6 74\n14 44 99 62 87\n54 84 86 94 21\n29 51 38 67  8\n\n43 28 24 46 22\n61 15  4 52 17\n62 77 18 56 85\n93 60 33 71 41\n63  2  6 68 92\n\n60 92 52 36 38\n66 34 26 19 25\n24 65 90 39 74\n17 97 96  7 48\n50 55 57 73 64\n\n19 77 60 66 16\n41 54  5 49  6\n69 61 94 86 98\n67 37 87 71 72\n44 96 90 40 74\n\n90 49 68 74 32\n31 85 42 65 53\n76 43 41 36 20\n16 75 46 47 86\n54 44 95 13 23\n\n56  0 88 99 76\n10 42 96 30 14\n67 73 16 21 35\n80 41 64 40 78\n13 19  4 24 20\n\n79 98 28 58 41\n24 97 85 22 89\n12 81 68 50 47\n 2 34 16  6 95\n64 51 11 43 26\n\n 6 39 79 95  3\n82  9 61 80 33\n94 87 13 70 11\n 0  8 37 35 19\n62 75 84 55 93\n\n44 51 54 27 94\n77 32 81 71 62\n98 91 68 41 89\n 6 39 40 56 53\n73 88  5 49 80\n\n97 29 15 61 83\n46 69 51 71 17\n40 94 49 14 66\n52 20 57 62 80\n19 72 75 84 36\n\n27 26 95 78 92\n98 18 31 51 45\n39 43 94 33 13\n50 16 71 30 22\n70 81 36 38 64\n\n90  7 71 11 63\n25 39 61 17 46\n51 86 56 81 84\n14 33 37 23 60\n52 64  8 65 29\n\n41 92 40 71 33\n90  2 24 37 25\n 0 94 74 53 69\n81 61  1 70 88\n44 34 99 29 75\n\n63 39 44  3 82\n68 95 67 28 49\n22 53 76 81 47\n15 75  0 54  6\n86 37 65 52 77\n\n11 64 39 47 72\n97 59 83 19 58\n12 65 92 89 28\n 9 78 40 79 99\n17 50 71 18 68\n\n31 78 27 32 18\n97 20 60 68 88\n12  5 99 49 82\n35  6 87  2 61\n70 53 63 36 93\n\n89  4 50 54 80\n85 36 17  5 71\n44 95 57 73 60\n46 92 25  8 59\n98 82 21 93 99\n\n27 12 82 95 47\n 8 21 69 83 64\n11  7 88 26 30\n70 96 18 75 53\n28 22 56 52 29\n\n56  1 30 13 53\n37 86 98 19  9\n 3 67 16 71 85\n83 79 48 54 14\n47 62 44 95 65\n\n51 18 87 35 55\n52 85 79 56 82\n83 26 24 29 43\n80 76  4 45 13\n11 12 99 94 47\n\n14  1 52 95 63\n54 27 67 92 98\n34 61 26 32 33\n76 77 49 83  2\n97 59 12 71 80\n\n78 16 59 44  5\n73 21 53 37 50\n25 86 88 61 74\n80 30 69 56 57\n98 39 26 58 51\n\n71 48 28 14 81\n69 67  6 77 47\n94 83  8 40 20\n30 58  9 99 76\n51 24 91 21 52\n\n84 76 33 14 72\n37 36 25 12 34\n39 54 89 81 30\n 2 15 46 10 22\n41 75 27 66 69\n\n 8 20 53 16 86\n38 99  4 11 60\n55 14 47  1 48\n51 50 69 52 37\n 3 56 32 79 68\n\n69 40 17 70 98\n12 86 41 35 50\n60 44  8 20 81\n14 82 25 55  4\n87 67 85  3  5\n\n72 90 14 78 94\n 2 85 91 97 42\n84  9 27 70 95\n55 56 74 73  1\n11 59 13 67 18\n\n 5 84 21 73 13\n11 46 35 79 99\n57 25 48 52  2\n51 70 56 54 94\n37 62 47 43 41\n\n99 30 74 11 51\n48 90  1 27 76\n71 63 28 86 10\n52  5 83 16 69\n70 93 92 73 43\n\n52 70 58 95 82\n74 18 90 99 39\n12 51 71 48 47\n92 11 91 16 61\n41 62 97 68  0\n\n20 32 76 50 55\n 4 70 14 36 82\n74 10 97 26 87\n61 83 56 98 71\n64 38  8 65 92\n\n63 68 84 36 41\n71 44 12 77 50\n18 92 54 58 23\n89 98 72 69 25\n62 38 42  5 52\n\n59 65 60 84 49\n51 69 12 15 38\n70  1 79 22 35\n66 88 85 83 32\n 3 33 78 48 16\n\n79 91 35 90 77\n22 59 58 96 97\n99 84 34  2 74\n10 92  5  4 45\n53 21 42 71 56\n\n43 23 45 81 34\n 1 52  7 24 51\n42 22 17 20 77\n31 21 29 19 79\n58 87 30 60 49\n\n81 64 86 76 70\n44 14 43 90  2\n96 16 42 22  7\n 5 57 19 84 21\n95 74 80 28 72\n\n 3 57 12 95 35\n61 72 98 39 17\n62 87 30 66  4\n26 58 16 20 47\n37 46 13 42 85\n\n55 24 36 49 85\n19 39 88 73 61\n 1 60 45 72 29\n47 12 53 76 44\n28 98 70 54  0\n\n77 29 17 36 96\n35 64 93 37 83\n12 10 57 82  7\n90 69  0 86 32\n74 66 72 63 97\n\n53 18 82 30  4\n 6 47 28 80 71\n39 36 22 20 51\n 7 57 26 34 79\n72 10 56 89  1\n\n92 20 76 27 51\n72 82 39 95 38\n19 33 70 62 26\n79 99 40 30  8\n94 80 10 91  4\n\n56 21 15 54 60\n69 64 55  0 59\n39 95 98 34 99\n24 76  3  6 30\n65 45 96 82 26\n\n59 55 44 79 12\n87 73 37 76 91\n68 92 51 49 36\n99 54  3 71 64\n25 60 94 45 81\n\n23 67 96 86 98\n14 47 45 66 62\n73 76 74 54 50\n64 60 35 10 58\n99 81 34  9 13\n\n71 44 19 13  2\n18 80 24 11 85\n36  1 99 26 52\n48 76 84 88 63\n61 30 49 86 35\n\n20 85 55 47 99\n18 49 38 65 61\n37 48 32  6 15\n80 94 66 89 91\n 1 44 36 92 21\n\n72 65  4 76 16\n80 97 15 56 33\n14 40 50 11 57\n34 37 68 88 44\n 6 38 21 49  7\n\n39 80 87 32 21\n41 97 66 15 83\n68 69 28 88 62\n18  2 48 58 77\n63 64 17 13 95\n\n44  3 41 55 85\n83 75 13  0 81\n95  9 23  8 26\n71 94 37 70 45\n77 82 62 87 19\n\n65 16 30 91 52\n78 67 24 58 11\n75 47 90  0  8\n83 88 73 60  2\n46 59 77 32 19\n\n82 80  0 24 85\n92 99 50 94 38\n19 98 10 51 32\n36 73 67 43 57\n46 21 13 69 37\n\n89 94 78  1  9\n16 34 18 15 38\n69 82 35 92 27\n66 64 68 63 26\n62 21 65 36 71\n\n15  4 25 50 41\n69 98 12 74 21\n 2 13 66 55 83\n93 90 23 27 33\n82 52 68 61 60\n\n57 21 28 29  5\n67 35 19 62 68\n91 83  3 33 99\n20 30 79 50 85\n60 89  4  7 36\n\n43  4 81 19 77\n89 92 46 52 35\n 1 21  2 75 88\n 8 97 26 62 71\n 9 93 30 50 66\n\n42 46 38 85 82\n18 80 91  1 40\n72 81 89 51 31\n37 20 24 67 92\n32 43 95 70 84\n\n90 48 63 15 45\n67 52  2 26 31\n30 13 36 77 49\n60  8 86 70 99\n94 27 85 78 34\n\n76 65 22 60 55\n81 88 54  4 26\n72 39 86 12  8\n68 46 98 28 99\n45 69 21  7 35\n\n47 22 34 19 95\n30 15 39 51 10\n11 37 48 44 71\n 2 89 92 78 35\n21 73 33 20 69\n\n 6 70 84 25  3\n21 12 55 78 49\n80 60 98 58 83\n17 96 69  9 66\n76 59 39 86 51\n\n97 60 93 22 99\n 2  4 25 45 78\n43 53 63 41  6\n64 74 16 56 28\n77 12 20 35 49\n\n82 10 91 16 77\n17 85 48 24  1\n61 96 38 68 99\n41 42 25 66 56\n97 18 63 93 29\n\n95 37 83 61 17\n11 15 43  6 24\n 0 28 51 87  9\n76 52  2 64 32\n85 41 99 29  7\n\n11 86  3 39 80\n35 78 26 34 65\n46 79 44 64 66\n29 74 63 20  0\n92 28 41 69 50\n\n99 58 15 51 28\n 1 36 45 38 34\n46 94 35 44 88\n39 20  8 59 61\n 3  4 37 14 63\n\n31 91 85 61 29\n66 54  9 49  2\n81 62 70 98 38\n68  1 16 95 78\n59 52 53 21 36\n\n69 59 50 48 56\n17 61 16 92 47\n63 60 62  5  3\n37 97 38 83 58\n73 18 71 19 94\n\n55  9 34 57 85\n31 37 30 16 64\n44 91 94  6  7\n90 87 77 59 50\n12 79 43 17 89\n\n90 53 57 28 58\n56 49 29  8 12\n77 27 62 30 82\n71 98 63 37 83\n 9 15 84 36 74\n\n80 56 52 44 71\n40  5 26 28 46\n11 70 57 95 93\n85 29 21 84 35\n20 15 81 54 91\n\n60 86 80 79 11\n90 82 84 48 43\n92 81 39 57 47\n64 36  4 71  9\n78 62 53 51 66\n\n84 51 19 73 55\n42 18 75 96  9\n47 46 12 98 93\n62 57 24  6 74\n50 53 30 70 80\n\n57 60  1 49 20\n93  0 39  6 74\n86  9 56 41 25\n53 99 83 38 80\n37 79 18 23 45\n\n33 95 37 86 45\n62 65 16  3 77\n 4 14 82 61 13\n18 71 11  8 23\n50 67 35 75 76\n\n43 30 48 38 86\n62 46 72 21 97\n 0 18 91 17 42\n 6 99 56 22 64\n15 25 79 13 55\n\n54 34 98 43 86\n39 47 56 52 95\n62 92  6 70 29\n65 78 57 99 35\n72 55 20 88 77\n\n87 97 67 99 20\n58 50 30 78 31\n 4  6 96 85 70\n80 59 77 88 93\n 9  0 90 86  3\n\n18 17 81 50  8\n12 62 73 32 72\n41 90 42 11 79\n 1  7 94 13  0\n77 33 23 83 74\n\n71 84 22 14 54\n98 34 56 81 33\n58 39  6 46 96\n15  7 11 13 37\n70  5  2  9 68\n\n28 58 11 63 26\n 6 14 44 70 93\n32 52 60 96  3\n76  0 75 66 71\n50 54 34 30 98\n\n91 26  2 53 92\n45 67 68 32 50\n80 30 15 78 73\n10 14 28 27  0\n21 38 88 22  5\n\n42 11 23 88 41\n54 58  8 74 40\n 6 13 80 89 82\n81  3  5 53 76\n47 39  9 25 46\n\n82 14 52 43 95\n15 37 12 58 80\n64 97 45 61 49\n71 65 29 25  9\n21 11 51  1 87\n\n20 80 50 27 90\n21 35  9 40 81\n89 16 26 74 84\n29 97 88 19 32\n85 63 10 46 52\n\n16 66  0 53 40\n94 42 80 86 25\n11 15 68 35  5\n60 89 41 92 79\n51 77 88 36 67\n\n51 65 33 97 81\n78 96 86 64 22\n10 28 93  2 14\n71 29 92  6 62\n98 38 35  0 70";

function printBoard(board) {
  var printNumber = function (v) {
    if (v.TAG === /* Marked */0) {
      return "Marked " + String(v._0);
    } else {
      return "Unmarked " + String(v._0);
    }
  };
  for(var y = 0 ,y_finish = board.spaces.length; y < y_finish; ++y){
    console.log($$Array.map(printNumber, Caml_array.get(board.spaces, y)));
  }
  
}

function parseInput(input) {
  var lines = $$String.split_on_char(/* '\n' */10, input);
  var match = lines ? [
      lines.hd,
      lines.tl
    ] : [
      "",
      /* [] */0
    ];
  var parseBoards = function (_boardStr, _boards) {
    while(true) {
      var boards = _boards;
      var boardStr = _boardStr;
      if (!boardStr) {
        return boards;
      }
      var row = boardStr.hd;
      if (row === "") {
        _boards = {
          hd: {
            spaces: []
          },
          tl: boards
        };
        _boardStr = boardStr.tl;
        continue ;
      }
      if (!boards) {
        return boards;
      }
      var rowValues = $$Array.of_list(List.map((function (v) {
                  return {
                          TAG: /* Unmarked */1,
                          _0: v
                        };
                }), List.map(Caml_format.caml_int_of_string, List.filter(function (i) {
                          return i !== "";
                        })(List.map((function (i) {
                              if (i !== undefined) {
                                return $$String.trim(i);
                              } else {
                                return "";
                              }
                            }), $$Array.to_list(row.split(/\s+/)))))));
      var spaces = $$Array.append(boards.hd.spaces, [rowValues]);
      _boards = {
        hd: {
          spaces: spaces
        },
        tl: boards.tl
      };
      _boardStr = boardStr.tl;
      continue ;
    };
  };
  var numbers = List.map(Caml_format.caml_int_of_string, $$String.split_on_char(/* ',' */44, match[0]));
  var boards = parseBoards(match[1], /* [] */0);
  return {
          numbers: numbers,
          boards: List.rev(boards)
        };
}

function applyNumberToBoard(number, board) {
  for(var y = 0 ,y_finish = board.spaces.length; y < y_finish; ++y){
    for(var x = 0 ,x_finish = Caml_array.get(board.spaces, y).length; x < x_finish; ++x){
      var val = Caml_array.get(Caml_array.get(board.spaces, y), x);
      if (val.TAG !== /* Marked */0) {
        var val$1 = val._0;
        if (val$1 === number) {
          Caml_array.set(Caml_array.get(board.spaces, y), x, {
                TAG: /* Marked */0,
                _0: val$1
              });
        }
        
      }
      
    }
  }
  return board;
}

function isCompleteColumn(board, columnIndex) {
  var _rowIndex = 0;
  while(true) {
    var rowIndex = _rowIndex;
    if (rowIndex >= board.spaces.length) {
      return true;
    }
    var match = Caml_array.get(Caml_array.get(board.spaces, columnIndex), rowIndex);
    if (match.TAG !== /* Marked */0) {
      return false;
    }
    _rowIndex = rowIndex + 1 | 0;
    continue ;
  };
}

function isCompleteRow(board, rowIndex) {
  var _columnIndex = 0;
  while(true) {
    var columnIndex = _columnIndex;
    if (columnIndex >= Caml_array.get(board.spaces, rowIndex).length) {
      return true;
    }
    var match = Caml_array.get(Caml_array.get(board.spaces, columnIndex), rowIndex);
    if (match.TAG !== /* Marked */0) {
      return false;
    }
    _columnIndex = columnIndex + 1 | 0;
    continue ;
  };
}

function isBoardWinner(board, _row, _col) {
  while(true) {
    var col = _col;
    var row = _row;
    if (row >= board.spaces.length && col >= Caml_array.get(board.spaces, 0).length) {
      return false;
    }
    if (row < board.spaces.length) {
      if (isCompleteRow(board, row)) {
        return true;
      }
      _row = row + 1 | 0;
      continue ;
    }
    if (col >= Caml_array.get(board.spaces, 0).length) {
      return false;
    }
    if (isCompleteColumn(board, col)) {
      return true;
    }
    _col = col + 1 | 0;
    continue ;
  };
}

function findWinningBoard(_boards) {
  while(true) {
    var boards = _boards;
    if (!boards) {
      return ;
    }
    var head = boards.hd;
    if (isBoardWinner(head, 0, 0)) {
      return head;
    }
    _boards = boards.tl;
    continue ;
  };
}

function applyInput(_numbers, boards) {
  while(true) {
    var numbers = _numbers;
    if (!numbers) {
      return ;
    }
    var head = numbers.hd;
    var updated = List.map((function(head){
        return function (param) {
          return applyNumberToBoard(head, param);
        }
        }(head)), boards);
    var winningBoard = findWinningBoard(updated);
    if (winningBoard !== undefined) {
      return [
              winningBoard,
              head
            ];
    }
    _numbers = numbers.tl;
    continue ;
  };
}

function getUnmarkedNumberSum(board) {
  return $$Array.fold_left((function (sum, next) {
                return sum + $$Array.fold_left((function (rowSum, val) {
                              if (val.TAG === /* Marked */0) {
                                return rowSum;
                              } else {
                                return rowSum + val._0 | 0;
                              }
                            }), 0, next) | 0;
              }), 0, board.spaces);
}

var NoWinningBoard = /* @__PURE__ */Caml_exceptions.create("Day04.NoWinningBoard");

function part1(param) {
  var input = parseInput(inputStr);
  var match = applyInput(input.numbers, input.boards);
  if (match !== undefined) {
    var lastNum = match[1];
    var unmarkedSum = getUnmarkedNumberSum(match[0]);
    console.log("Unmarked", unmarkedSum);
    console.log("Last", lastNum);
    return Math.imul(unmarkedSum, lastNum);
  }
  throw {
        RE_EXN_ID: NoWinningBoard,
        Error: new Error()
      };
}

console.log("Part 1", part1(undefined));

function findLastWinner(numbers, _boards) {
  while(true) {
    var boards = _boards;
    if (boards) {
      if (boards.tl) {
        var match = applyInput(numbers, boards);
        if (match !== undefined) {
          var board = match[0];
          var remainingBoards = List.filter((function(board){
                return function (b) {
                  return b !== board;
                }
                }(board)))(boards);
          _boards = remainingBoards;
          continue ;
        }
        throw {
              RE_EXN_ID: NoWinningBoard,
              Error: new Error()
            };
      }
      var match$1 = applyInput(numbers, {
            hd: boards.hd,
            tl: /* [] */0
          });
      if (match$1 !== undefined) {
        return Math.imul(getUnmarkedNumberSum(match$1[0]), match$1[1]);
      }
      throw {
            RE_EXN_ID: NoWinningBoard,
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: NoWinningBoard,
          Error: new Error()
        };
  };
}

function part2(param) {
  var input = parseInput(inputStr);
  return findLastWinner(input.numbers, input.boards);
}

console.log("Part 2", part2(undefined));

var exampleInputStr = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7";

exports.exampleInputStr = exampleInputStr;
exports.inputStr = inputStr;
exports.printBoard = printBoard;
exports.parseInput = parseInput;
exports.applyNumberToBoard = applyNumberToBoard;
exports.isCompleteColumn = isCompleteColumn;
exports.isCompleteRow = isCompleteRow;
exports.isBoardWinner = isBoardWinner;
exports.findWinningBoard = findWinningBoard;
exports.applyInput = applyInput;
exports.getUnmarkedNumberSum = getUnmarkedNumberSum;
exports.NoWinningBoard = NoWinningBoard;
exports.part1 = part1;
exports.findLastWinner = findLastWinner;
exports.part2 = part2;
/*  Not a pure module */