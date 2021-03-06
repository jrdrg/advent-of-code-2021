// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("rescript/lib/js/list.js");
var $$Array = require("rescript/lib/js/array.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_int64 = require("rescript/lib/js/caml_int64.js");
var Belt_MapInt = require("rescript/lib/js/belt_MapInt.js");
var Caml_format = require("rescript/lib/js/caml_format.js");

var exampleInputStr = "3,4,3,1,2";

var inputStr = "1,3,4,1,5,2,1,1,1,1,5,1,5,1,1,1,1,3,1,1,1,1,1,1,1,2,1,5,1,1,1,1,1,4,4,1,1,4,1,1,2,3,1,5,1,4,1,2,4,1,1,1,1,1,1,1,1,2,5,3,3,5,1,1,1,1,4,1,1,3,1,1,1,2,3,4,1,1,5,1,1,1,1,1,2,1,3,1,3,1,2,5,1,1,1,1,5,1,5,5,1,1,1,1,3,4,4,4,1,5,1,1,4,4,1,1,1,1,3,1,1,1,1,1,1,3,2,1,4,1,1,4,1,5,5,1,2,2,1,5,4,2,1,1,5,1,5,1,3,1,1,1,1,1,4,1,2,1,1,5,1,1,4,1,4,5,3,5,5,1,2,1,1,1,1,1,3,5,1,2,1,2,1,3,1,1,1,1,1,4,5,4,1,3,3,1,1,1,1,1,1,1,1,1,5,1,1,1,5,1,1,4,1,5,2,4,1,1,1,2,1,1,4,4,1,2,1,1,1,1,5,3,1,1,1,1,4,1,4,1,1,1,1,1,1,3,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,5,1,2,1,1,1,1,1,1,1,1,1";

function parseInput(input) {
  return List.fold_left((function (fishCounts, next) {
                return Belt_MapInt.update(fishCounts, next, (function (v) {
                              if (v !== undefined) {
                                return Caml_int64.add(v, Caml_int64.one);
                              } else {
                                return Caml_int64.one;
                              }
                            }));
              }), undefined, List.map(Caml_format.caml_int_of_string, $$String.split_on_char(/* ',' */44, input)));
}

function nextDay(fishCounts) {
  return Belt_MapInt.reduce(fishCounts, undefined, (function (counts, timerValue, count) {
                var inc = function (val) {
                  if (val !== undefined) {
                    return Caml_int64.add(val, count);
                  } else {
                    return count;
                  }
                };
                if (timerValue > 0) {
                  return Belt_MapInt.update(counts, timerValue - 1 | 0, inc);
                } else {
                  return Belt_MapInt.update(Belt_MapInt.update(counts, 6, inc), 8, inc);
                }
              }));
}

function nextNDays(_days, _fishCounts) {
  while(true) {
    var fishCounts = _fishCounts;
    var days = _days;
    if (days === 0) {
      return fishCounts;
    }
    _fishCounts = nextDay(fishCounts);
    _days = days - 1 | 0;
    continue ;
  };
}

function part1(param) {
  var input = parseInput(exampleInputStr);
  var out = nextNDays(80, input);
  return Caml_int64.to_float($$Array.fold_left((function (sum, param) {
                    return Caml_int64.add(sum, param[1]);
                  }), Caml_int64.zero, Belt_MapInt.toArray(out)));
}

console.log("Part 1", part1(undefined));

function part2(param) {
  var input = parseInput(inputStr);
  var out = nextNDays(256, input);
  return Caml_int64.to_float($$Array.fold_left((function (sum, param) {
                    return Caml_int64.add(sum, param[1]);
                  }), Caml_int64.zero, Belt_MapInt.toArray(out)));
}

console.log("Part 2", part2(undefined));

exports.exampleInputStr = exampleInputStr;
exports.inputStr = inputStr;
exports.parseInput = parseInput;
exports.nextDay = nextDay;
exports.nextNDays = nextNDays;
exports.part1 = part1;
exports.part2 = part2;
/*  Not a pure module */
