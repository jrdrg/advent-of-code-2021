// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml = require("rescript/lib/js/caml.js");
var List = require("rescript/lib/js/list.js");
var $$Array = require("rescript/lib/js/array.js");
var Curry = require("rescript/lib/js/curry.js");
var Int32 = require("rescript/lib/js/int32.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_format = require("rescript/lib/js/caml_format.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");

function coordsToString(r, c) {
  return String(r) + ":" + String(c);
}

function stringToCoords(key) {
  var coords = $$Array.of_list(List.map(Caml_format.caml_int_of_string, $$String.split_on_char(/* ':' */58, key)));
  return [
          Caml_array.get(coords, 0),
          Caml_array.get(coords, 1)
        ];
}

function getAdjacentNeighborCoords(row, col) {
  return {
          hd: [
            row,
            col - 1 | 0
          ],
          tl: {
            hd: [
              row,
              col + 1 | 0
            ],
            tl: {
              hd: [
                row - 1 | 0,
                col
              ],
              tl: {
                hd: [
                  row + 1 | 0,
                  col
                ],
                tl: /* [] */0
              }
            }
          }
        };
}

function getNeighborCoords(row, col) {
  return {
          hd: [
            row - 1 | 0,
            col - 1 | 0
          ],
          tl: {
            hd: [
              row - 1 | 0,
              col
            ],
            tl: {
              hd: [
                row - 1 | 0,
                col + 1 | 0
              ],
              tl: {
                hd: [
                  row,
                  col - 1 | 0
                ],
                tl: {
                  hd: [
                    row,
                    col + 1 | 0
                  ],
                  tl: {
                    hd: [
                      row + 1 | 0,
                      col - 1 | 0
                    ],
                    tl: {
                      hd: [
                        row + 1 | 0,
                        col
                      ],
                      tl: {
                        hd: [
                          row + 1 | 0,
                          col + 1 | 0
                        ],
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            }
          }
        };
}

function validPoints(points, grid) {
  return List.filter(function (param) {
                var c = param[1];
                var r = param[0];
                if (r >= 0 && c >= 0) {
                  return Belt_MapString.has(grid, coordsToString(r, c));
                } else {
                  return false;
                }
              })(points);
}

function getNeighbors(row, col, grid) {
  var possible = getNeighborCoords(row, col);
  return validPoints(possible, grid);
}

function valueAt(r, c, grid) {
  return Belt_MapString.get(grid, coordsToString(r, c));
}

function setValueAt(r, c, v, grid) {
  return Belt_MapString.set(grid, coordsToString(r, c), v);
}

var toPointsList = Belt_MapString.keysToArray;

function fromString(_input) {
  
}

function size(grid) {
  var match = List.fold_left((function (param, point) {
          var match = stringToCoords(point);
          return [
                  Caml.caml_int_max(param[0], match[0]),
                  Caml.caml_int_max(param[1], match[1])
                ];
        }), [
        Int32.min_int,
        Int32.min_int
      ], $$Array.to_list(Belt_MapString.keysToArray(grid)));
  return [
          match[0] + 1 | 0,
          match[1] + 1 | 0
        ];
}

var pointsCount = Belt_MapString.size;

function fromPointsList(initialValue, points) {
  return List.fold_left((function (grid, param) {
                return Belt_MapString.set(grid, coordsToString(param[0], param[1]), initialValue);
              }), undefined, points);
}

function print(fn, grid) {
  var match = size(grid);
  var w = match[1];
  var h = match[0];
  var printRow = function (r, _c, _str) {
    while(true) {
      var str = _str;
      var c = _c;
      if (c >= w) {
        return str;
      }
      var val = valueAt(r, c, grid);
      _str = str + Curry._1(fn, val);
      _c = c + 1 | 0;
      continue ;
    };
  };
  var _r = 0;
  var _str = "";
  while(true) {
    var str = _str;
    var r = _r;
    if (r >= h) {
      return str;
    }
    var row = printRow(r, 0, "");
    _str = str + "\n" + row;
    _r = r + 1 | 0;
    continue ;
  };
}

exports.coordsToString = coordsToString;
exports.stringToCoords = stringToCoords;
exports.getAdjacentNeighborCoords = getAdjacentNeighborCoords;
exports.getNeighborCoords = getNeighborCoords;
exports.validPoints = validPoints;
exports.getNeighbors = getNeighbors;
exports.valueAt = valueAt;
exports.setValueAt = setValueAt;
exports.toPointsList = toPointsList;
exports.fromString = fromString;
exports.size = size;
exports.pointsCount = pointsCount;
exports.fromPointsList = fromPointsList;
exports.print = print;
/* No side effect */