#load "Data.fsx"
#load "Utils.fsx"
#r "nuget:Expecto"

open System
open Expecto

let print p = Utils.printResult 6 p
let testdata =
    [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
      "bvwbjplbgvbhsrlpgdmjqwftvncz"
      "nppdvjthqldpwncqszvftbrmjlhg"
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" ]

let isUnique (xs: seq<'a>) =
    xs |> Seq.distinct |> Seq.length |> (=) (Seq.length xs)

let pair a b = a,b

let findUniqueAfter n = Seq.windowed n >> Seq.mapi pair >> Seq.find (snd >> isUnique) >> fst >> (+) n
let findFirstMarker = findUniqueAfter 4

let testResult = testdata |> List.map findFirstMarker
let expectedResult = [7;5;6;10;11]
Expect.equal testResult expectedResult "Test result is incorrect"

Data.markerData () |> findFirstMarker |> print 1

let findMessage = findUniqueAfter 14

let testResult2 = testdata |> List.map findMessage
let expectedResult2 = [19;23;23;29;26]
Expect.equal testResult2 expectedResult2 "Test result is incorrect"

Data.markerData () |> findMessage |> print 2