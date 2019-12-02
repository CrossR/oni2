/*
 * Filter.re
 *
 * Module to filter & rank items using various strategies.
 */
open ReasonFuzz;

open Oni_Core.CamomileBundled.Camomile;

module Utility = Oni_Core.Utility;
module Zed_utf8 = Oni_Core.ZedBundled;

module Option = {
  let map = f =>
    fun
    | Some(x) => Some(f(x))
    | None => None;
};

type result('a) = {
  item: 'a,
  highlight: list((int, int)),
};

let makeResult = (matchTuple) => {
  let (match, item) = matchTuple;
  {item, highlight: Utility.ranges(match.Fzy.Result.positions)};
};

let rank = (query, format, items) => {
  let shouldLower = query == String.lowercase_ascii(query);
  let format = item => format(item, ~shouldLower);

  let processItem = (item) => {
    let formattedItemString = format(item);

    (formattedItemString, item);
  };

  let search = (query, searchList) => {
    let (strings, items) = List.split(searchList);
    Console.log("Starting search...");
    let matches = Fzy.fzySearchList(strings, query);
    Console.log(matches);
    Console.log("Finished search...");

    List.combine(matches, items)
  }

  switch(List.length(items), String.length(query)) {
  | (0, 0) | (0, _) | (_, 0) => []
  | (_, _) => items |> List.map(processItem) |> search(query) |> List.map(makeResult);
  }
};

// Check whether the query matches...
// Benchmarking showed that this was slightly faster than the recursive version
let fuzzyMatches = (query: list(UChar.t), str) => {
  let toMatch = Zed_utf8.explode(str);

  let q = ref(query);
  let m = ref(toMatch);

  let atEnd = ref(false);
  let result = ref(false);

  while (! atEnd^) {
    switch (q^, m^) {
    | ([], _) =>
      result := true;
      atEnd := true;
    | (_, []) =>
      result := false;
      atEnd := true;
    | ([qh, ...qtail], [mh, ...mtail]) when UChar.eq(qh, mh) =>
      q := qtail;
      m := mtail;
    | (_, [_, ...mtail]) => m := mtail
    };
  };

  result^;
};
