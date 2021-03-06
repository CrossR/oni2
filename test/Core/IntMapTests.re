/* open Oni_Core; */
open TestFramework;

module IntMap = Oni_Core.IntMap;

let simpleList = [(0, "a"), (1, "b"), (2, "c"), (3, "d"), (4, "e")];

let map = simpleList |> List.to_seq |> IntMap.of_seq;

describe("IntMap", ({describe, _}) => {
  describe("shift", ({test, _}) => {
    test("no shift if no add / deletions", ({expect, _}) => {
      let newMap = IntMap.shift(~startPos=1, ~endPos=1, ~delta=0, map);

      let firstVal = IntMap.find(0, newMap);
      let secondVal = IntMap.find(1, newMap);
      let thirdVal = IntMap.find(2, newMap);

      expect.string(firstVal).toEqual("a");
      expect.string(secondVal).toEqual("b");
      expect.string(thirdVal).toEqual("c");
    });

    test("shift sparse list", ({expect, _}) => {
      let map = [(1, "a")] |> List.to_seq |> IntMap.of_seq;

      let map' =
        IntMap.shift(
          ~default=_ => None,
          ~startPos=0,
          ~endPos=0,
          ~delta=1,
          map,
        );

      let val0 = IntMap.find_opt(0, map');
      let val1 = IntMap.find_opt(1, map');
      let val2 = IntMap.find_opt(2, map');

      expect.equal(val0, None);
      expect.equal(val1, None);
      expect.equal(val2, Some("a"));
    });

    test("add lines between 1 and 2", ({expect, _}) => {
      let newMap =
        IntMap.shift(
          ~default=_ => Some("f"),
          ~startPos=1,
          ~endPos=1,
          ~delta=2,
          map,
        );

      let val0 = IntMap.find(0, newMap);
      let val1 = IntMap.find(1, newMap);
      let val2 = IntMap.find(2, newMap);
      let val3 = IntMap.find(3, newMap);
      let val4 = IntMap.find(4, newMap);

      expect.string(val0).toEqual("a");
      expect.string(val1).toEqual("f");
      expect.string(val2).toEqual("f");
      expect.string(val3).toEqual("b");
      expect.string(val4).toEqual("c");
    });

    test("remove line", ({expect, _}) => {
      let newMap =
        IntMap.shift(
          ~default=_ => Some("f"),
          ~startPos=1,
          ~endPos=1,
          ~delta=[@reason.preserve_braces] -1,
          map,
        );

      let val0 = IntMap.find(0, newMap);
      let val1 = IntMap.find(1, newMap);

      expect.string(val0).toEqual("a");
      expect.string(val1).toEqual("c");
    });

    test("remove line in sparse list", ({expect, _}) => {
      let map = [(1, "b")] |> List.to_seq |> IntMap.of_seq;
      let newMap =
        map
        |> IntMap.shift(~default=_ => None, ~startPos=1, ~endPos=1, ~delta=-1);
      let val0 = IntMap.find_opt(0, newMap);
      let val1 = IntMap.find_opt(1, newMap);
      let val2 = IntMap.find_opt(2, newMap);

      expect.equal(val0, None);
      expect.equal(val1, None);
      expect.equal(val2, None);
    });
  })
});
