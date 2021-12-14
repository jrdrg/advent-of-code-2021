// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("rescript/lib/js/list.js");
var $$Array = require("rescript/lib/js/array.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");
var Belt_SetString = require("rescript/lib/js/belt_SetString.js");

var inputStr = "EO-jc\nend-tm\njy-FI\nek-EO\nmg-ek\njc-jy\nFI-start\njy-mg\nmg-FI\njc-tm\nend-EO\nds-EO\njy-start\ntm-EO\nmg-jc\nek-jc\ntm-ek\nFI-jc\njy-EO\nek-jy\nek-LT\nstart-mg";

function fromString(input) {
  return List.fold_left((function (map, coords) {
                var start = Caml_array.get(coords, 0);
                var end = Caml_array.get(coords, 1);
                return Belt_MapString.update(map, start, (function (v) {
                              if (v !== undefined) {
                                return Caml_option.some(Belt_SetString.add(Caml_option.valFromOption(v), end));
                              } else {
                                return Caml_option.some(Belt_SetString.add(undefined, end));
                              }
                            }));
              }), undefined, List.fold_left((function (arrs, next) {
                    var s = Caml_array.get(next, 0);
                    var e = Caml_array.get(next, 1);
                    return {
                            hd: [
                              s,
                              e
                            ],
                            tl: {
                              hd: [
                                e,
                                s
                              ],
                              tl: arrs
                            }
                          };
                  }), /* [] */0, List.map($$Array.of_list, List.map((function (param) {
                            return $$String.split_on_char(/* '-' */45, param);
                          }), $$String.split_on_char(/* '\n' */10, input)))));
}

function connectionsFrom(cave, map) {
  return Belt_MapString.getExn(map, cave);
}

var CaveMap = {
  fromString: fromString,
  connectionsFrom: connectionsFrom
};

function possibleCaves(cave, visited, map) {
  return Belt_SetString.toList(Belt_SetString.keep(Belt_MapString.getExn(map, cave), (function (v) {
                    return !Belt_SetString.has(visited, v);
                  })));
}

function isLowercase(str) {
  return $$String.lowercase_ascii(str) === str;
}

function findPathsFrom(_paths, _found, map) {
  while(true) {
    var found = _found;
    var paths = _paths;
    if (!paths) {
      return found;
    }
    var remainingPaths = paths.tl;
    var path = paths.hd;
    var currentPathVisited = Belt_SetString.fromArray($$Array.of_list(List.filter(isLowercase)(path)));
    var possible = possibleCaves(List.hd(path), currentPathVisited, map);
    if (List.length(possible) === 0) {
      _paths = remainingPaths;
      continue ;
    }
    var match = List.fold_left((function(path){
        return function (param, cave) {
          var found = param[1];
          var paths = param[0];
          var newPath = {
            hd: cave,
            tl: path
          };
          if (cave === "end") {
            return [
                    paths,
                    {
                      hd: newPath,
                      tl: found
                    }
                  ];
          } else {
            return [
                    {
                      hd: newPath,
                      tl: paths
                    },
                    found
                  ];
          }
        }
        }(path)), [
          /* [] */0,
          found
        ], possible);
    var allPaths = List.append(match[0], remainingPaths);
    _found = match[1];
    _paths = allPaths;
    continue ;
  };
}

function part1(param) {
  var input = fromString(inputStr);
  return List.length(findPathsFrom({
                  hd: {
                    hd: "start",
                    tl: /* [] */0
                  },
                  tl: /* [] */0
                }, /* [] */0, input));
}

console.log("Part 1", part1(undefined));

function possibleCaves2(cave, visited, map) {
  var alreadyVisitedSmallCaveTwice = Belt_MapString.findFirstBy(visited, (function (param, v) {
          return v > 1;
        }));
  return Belt_SetString.toList(Belt_SetString.keep(Belt_MapString.getExn(map, cave), (function (v) {
                    switch (v) {
                      case "end" :
                      case "start" :
                          return Belt_MapString.getWithDefault(visited, v, 0) === 0;
                      default:
                        if (alreadyVisitedSmallCaveTwice !== undefined) {
                          return Belt_MapString.getWithDefault(visited, v, 0) < 1;
                        } else {
                          return Belt_MapString.getWithDefault(visited, v, 0) < 2;
                        }
                    }
                  })));
}

function findPathsFrom2(_paths, _found, map) {
  while(true) {
    var found = _found;
    var paths = _paths;
    if (!paths) {
      return found;
    }
    var remainingPaths = paths.tl;
    var path = paths.hd;
    var visitCounts = List.fold_left((function (counts, cave) {
            return Belt_MapString.update(counts, cave, (function (v) {
                          if (v !== undefined) {
                            return v + 1 | 0;
                          } else {
                            return 1;
                          }
                        }));
          }), undefined, List.filter(isLowercase)(path));
    var possible = possibleCaves2(List.hd(path), visitCounts, map);
    if (List.length(possible) === 0) {
      _paths = remainingPaths;
      continue ;
    }
    var match = List.fold_left((function(path){
        return function (param, cave) {
          var found = param[1];
          var paths = param[0];
          var newPath = {
            hd: cave,
            tl: path
          };
          if (cave === "end") {
            return [
                    paths,
                    {
                      hd: newPath,
                      tl: found
                    }
                  ];
          } else {
            return [
                    {
                      hd: newPath,
                      tl: paths
                    },
                    found
                  ];
          }
        }
        }(path)), [
          /* [] */0,
          found
        ], possible);
    var allPaths = List.append(match[0], remainingPaths);
    _found = match[1];
    _paths = allPaths;
    continue ;
  };
}

function part2(param) {
  var input = fromString(inputStr);
  return List.length(findPathsFrom2({
                  hd: {
                    hd: "start",
                    tl: /* [] */0
                  },
                  tl: /* [] */0
                }, /* [] */0, input));
}

console.log("Part 2", part2(undefined));

var exampleInputStr = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end";

var exampleInputStr2 = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc";

exports.exampleInputStr = exampleInputStr;
exports.exampleInputStr2 = exampleInputStr2;
exports.inputStr = inputStr;
exports.CaveMap = CaveMap;
exports.possibleCaves = possibleCaves;
exports.isLowercase = isLowercase;
exports.findPathsFrom = findPathsFrom;
exports.part1 = part1;
exports.possibleCaves2 = possibleCaves2;
exports.findPathsFrom2 = findPathsFrom2;
exports.part2 = part2;
/*  Not a pure module */