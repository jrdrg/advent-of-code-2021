// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("rescript/lib/js/list.js");
var $$Array = require("rescript/lib/js/array.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

var inputStr = "FPNFCVSNNFSFHHOCNBOB\n\nON -> S\nSO -> B\nOH -> C\nSN -> F\nBP -> O\nSK -> F\nOO -> K\nCF -> O\nPP -> F\nKS -> K\nKN -> B\nBN -> H\nHN -> H\nNP -> P\nBB -> N\nSB -> F\nBH -> V\nNV -> S\nPO -> S\nCN -> N\nVP -> B\nHH -> B\nNB -> V\nNF -> O\nBV -> B\nCV -> B\nSS -> H\nCB -> C\nVN -> S\nFH -> K\nBF -> H\nNH -> P\nPV -> K\nOP -> F\nHO -> N\nSH -> C\nVH -> P\nVK -> B\nOF -> F\nKK -> B\nSC -> H\nCO -> S\nBK -> V\nPF -> B\nOK -> K\nFO -> V\nCH -> O\nKO -> B\nCS -> V\nOC -> P\nSP -> V\nKF -> C\nHV -> S\nKH -> B\nVS -> K\nKB -> F\nFF -> P\nVF -> H\nNC -> S\nHB -> V\nNN -> C\nFV -> B\nPH -> V\nKV -> C\nPB -> C\nOS -> O\nPS -> H\nFS -> N\nFP -> O\nVV -> O\nFN -> V\nNO -> K\nNK -> V\nOB -> F\nPC -> O\nOV -> H\nFK -> C\nHS -> F\nSF -> N\nVC -> C\nBS -> N\nPK -> O\nFB -> S\nCK -> B\nKP -> N\nKC -> F\nBC -> F\nHK -> H\nVO -> O\nNS -> B\nVB -> K\nFC -> K\nSV -> O\nHF -> H\nHC -> C\nCP -> O\nCC -> P\nPN -> P\nHP -> C\nBO -> F";

var InvalidInput = /* @__PURE__ */Caml_exceptions.create("Day14.InvalidInput");

function parseInput(input) {
  var match = List.filter(function (i) {
          return i.length !== 0;
        })($$String.split_on_char(/* '\n' */10, input));
  if (match) {
    var pairs = List.fold_left((function (map, param) {
            return Belt_MapString.set(map, param[0], param[1]);
          }), undefined, List.map((function (pair) {
                var match = pair.split(" -> ");
                if (match.length !== 2) {
                  throw {
                        RE_EXN_ID: InvalidInput,
                        _1: pair,
                        Error: new Error()
                      };
                }
                var first = match[0];
                var second = match[1];
                return [
                        first,
                        second
                      ];
              }), match.tl));
    return [
            match.hd,
            pairs
          ];
  }
  throw {
        RE_EXN_ID: InvalidInput,
        _1: input,
        Error: new Error()
      };
}

function processStep(pairs, template) {
  var templateLetters = template.split("");
  return $$Array.of_list(List.rev($$Array.fold_left((function (output, param) {
                          var letter = param[1];
                          var index = param[0];
                          if (index === (templateLetters.length - 1 | 0)) {
                            return {
                                    hd: letter,
                                    tl: output
                                  };
                          }
                          var first = Caml_array.get(templateLetters, index);
                          var second = Caml_array.get(templateLetters, index + 1 | 0);
                          var pair = first + second;
                          var letterToInsert = Belt_MapString.get(pairs, pair);
                          if (letterToInsert !== undefined) {
                            return {
                                    hd: letterToInsert,
                                    tl: {
                                      hd: first,
                                      tl: output
                                    }
                                  };
                          } else {
                            return {
                                    hd: letter,
                                    tl: output
                                  };
                          }
                        }), /* [] */0, $$Array.mapi((function (i, letter) {
                              return [
                                      i,
                                      letter
                                    ];
                            }), templateLetters)))).join("");
}

function processSteps(pairs, _template, _step) {
  while(true) {
    var step = _step;
    var template = _template;
    if (step <= 0) {
      return template;
    }
    var processed = processStep(pairs, template);
    _step = step - 1 | 0;
    _template = processed;
    continue ;
  };
}

function getLetterCounts(str) {
  return $$Array.fold_left((function (counts, letter) {
                return Belt_MapString.update(counts, letter, (function (v) {
                              if (v !== undefined) {
                                return v + 1 | 0;
                              } else {
                                return 1;
                              }
                            }));
              }), undefined, str.split(""));
}

function part1(param) {
  var match = parseInput(inputStr);
  var output = processSteps(match[1], match[0], 10);
  var counts = $$Array.of_list(List.sort((function (param, param$1) {
              return param[1] - param$1[1] | 0;
            }), Belt_MapString.toList(getLetterCounts(output))));
  var match$1 = Caml_array.get(counts, 0);
  var match$2 = Caml_array.get(counts, counts.length - 1 | 0);
  return match$2[1] - match$1[1] | 0;
}

console.log("Part 1", part1(undefined));

function pairLetters(pair) {
  var arr = pair.split("");
  return [
          Caml_array.get(arr, 0),
          Caml_array.get(arr, 1)
        ];
}

function increment(amount, v) {
  if (v !== undefined) {
    return v + amount;
  } else {
    return amount;
  }
}

function decrement(amount, v) {
  if (v !== undefined && v > amount) {
    return v - amount;
  }
  
}

function processStep2(pairMappings, template) {
  var match = List.fold_left((function (param, param$1) {
          var pair = param$1[0];
          var l = param[1];
          var p = param[0];
          var match = Belt_MapString.get(template.pairs, pair);
          var match$1 = Belt_MapString.get(pairMappings, pair);
          if (match === undefined) {
            return [
                    p,
                    l
                  ];
          }
          if (match$1 === undefined) {
            return [
                    p,
                    l
                  ];
          }
          var match$2 = pairLetters(pair);
          var newPair1 = match$2[0] + match$1;
          var newPair2 = match$1 + match$2[1];
          var pairs = Belt_MapString.update(Belt_MapString.update(Belt_MapString.update(p, newPair1, (function (param) {
                          return increment(match, param);
                        })), newPair2, (function (param) {
                      return increment(match, param);
                    })), pair, (function (param) {
                  return decrement(match, param);
                }));
          var letters = Belt_MapString.update(l, match$1, (function (param) {
                  return increment(match, param);
                }));
          return [
                  pairs,
                  letters
                ];
        }), [
        template.pairs,
        template.letters
      ], Belt_MapString.toList(template.pairs));
  return {
          pairs: match[0],
          letters: match[1]
        };
}

function stringToTemplate(str) {
  var templateLetters = str.split("");
  var increment = function (v) {
    if (v !== undefined) {
      return v + 1;
    } else {
      return 1;
    }
  };
  var match = $$Array.fold_left((function (param, param$1) {
          var index = param$1[0];
          var l = param[1];
          var p = param[0];
          if (index === (templateLetters.length - 1 | 0)) {
            var letters = Belt_MapString.update(l, param$1[1], increment);
            return [
                    p,
                    letters
                  ];
          }
          var first = Caml_array.get(templateLetters, index);
          var second = Caml_array.get(templateLetters, index + 1 | 0);
          var pair = first + second;
          var pairs = Belt_MapString.update(p, pair, increment);
          var letters$1 = Belt_MapString.update(l, first, increment);
          return [
                  pairs,
                  letters$1
                ];
        }), [
        undefined,
        undefined
      ], $$Array.mapi((function (i, letter) {
              return [
                      i,
                      letter
                    ];
            }), templateLetters));
  return {
          pairs: match[0],
          letters: match[1]
        };
}

function processSteps2(pairs, _template, _step) {
  while(true) {
    var step = _step;
    var template = _template;
    if (step <= 0) {
      return template;
    }
    var processed = processStep2(pairs, template);
    _step = step - 1 | 0;
    _template = processed;
    continue ;
  };
}

function part2(param) {
  var match = parseInput(inputStr);
  var template = stringToTemplate(match[0]);
  var p = processSteps2(match[1], template, 40);
  var counts = $$Array.of_list(List.sort((function (param, param$1) {
              var x = param[1] - param$1[1];
              if (x > 0) {
                return 1;
              } else if (x < 0) {
                return -1;
              } else {
                return 0;
              }
            }), Belt_MapString.toList(p.letters)));
  var match$1 = Caml_array.get(counts, 0);
  var match$2 = Caml_array.get(counts, counts.length - 1 | 0);
  return match$2[1] - match$1[1];
}

console.log("Part 2", part2(undefined));

var exampleInputStr = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C";

exports.exampleInputStr = exampleInputStr;
exports.inputStr = inputStr;
exports.InvalidInput = InvalidInput;
exports.parseInput = parseInput;
exports.processStep = processStep;
exports.processSteps = processSteps;
exports.getLetterCounts = getLetterCounts;
exports.part1 = part1;
exports.pairLetters = pairLetters;
exports.increment = increment;
exports.decrement = decrement;
exports.processStep2 = processStep2;
exports.stringToTemplate = stringToTemplate;
exports.processSteps2 = processSteps2;
exports.part2 = part2;
/*  Not a pure module */
