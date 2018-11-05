#light

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']

//
let explode s =
  [for c in s -> c]

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string.
//
let implode L =
  let sb = System.Text.StringBuilder()
  List.iter (fun c -> ignore (sb.Append (c:char))) L
  sb.ToString()


//
// FileInput:
//
// Inputs text from a file line by line, returning this as a list
// of strings.  Each line is converted to lower-case.
//
let FileInput filename = 
  [ for line in System.IO.File.ReadLines(filename) -> line.ToLower() ]


//
// UserInput:
//
// This function reads from the keyboard, line by line, until 
// # appears on a line by itself.  The lines are returned as
// a list of strings; each line is converted to lower-case.
//
// NOTE: if the first line of input is blank (i.e. the user 
// presses ENTER), then input is read from the file 'input.txt'.
// Helpful when testing.
//
let rec _UserInput input =
  let line = System.Console.ReadLine()
  match line with
  | "#" -> List.rev input
  |  _  -> _UserInput (line.ToLower() :: input)

let UserInput() =
  let firstLine = System.Console.ReadLine()
  match firstLine with
  | "" -> FileInput @"./input.txt"
  | _  -> _UserInput [firstLine.ToLower()]
  



//
// combineLines: for a list that contains the lines of a training file,
//               explode each line (element of list) and add it to the accumulator
//
//                _combineLines: helper function to make it tail recursive
let rec _combineLines L accumulator = 
  match L with
  |[] -> List.rev(accumulator)
  |e::rest -> _combineLines rest (explode e @ accumulator)

let combineLines L =
  _combineLines L []
  
  



//
// mainList: create list of all languages and their letters tail recursivly
//
let rec _mainList file accumulator =
  match file with
  |[] -> List.rev(accumulator)
  |e::rest -> let f1 = FileInput e //parses e and turns it into List of the strings (each string is a line)
              let fixedf1 = List.tail f1 //ignore the first line because it contains the language name.
              let h1 = combineLines fixedf1 //creates a list of all letters
              _mainList rest (h1::accumulator)

let mainList file = 
  _mainList file []
  
 
 
 
//
// getLangs: gets first line in every file aka the Language name 
//

let rec _getLangs file accumulator =
  match file with
  |[] -> List.rev(accumulator)
  |e::rest -> let strings = FileInput e
              let langName = List.head(strings)
              _getLangs rest (langName::accumulator)


let getLangs file =
  _getLangs file []
 
 
 
 
 
//
// mainListTuples: creates tuples of (languageName,[list of letters])
//

let rec _mainListTuples L file accumulator =
  match L,file with
  |[],[] -> List.rev(accumulator)
  |[],_  -> List.rev(accumulator) //should never happen
  |_,[]  -> List.rev(accumulator) //should never happen
  |e::rest,lang::restFile -> _mainListTuples rest restFile ((lang,e)::accumulator)


let mainListTuples L file =
  _mainListTuples L file []
  

//
// getCounts: gets counts of letters for a lanuage list and returns a list of tuples
//
let rec _getCounts abcList langLetters accumulator =
  match abcList with
  |[] -> List.rev(accumulator)
  |letter::rest -> let name,L = langLetters
                   let mapped = List.map(fun e -> if(string(e) = letter) then 1 else 0) L
                   let count = List.sum mapped
                   let letterCountTuple = (letter, count)
                   _getCounts rest langLetters (letterCountTuple::accumulator)


let getCounts abcList langLetters =
  _getCounts abcList langLetters []
  
  
//
// getCountsForAllLangs: calls getCounts on every lang and returns a list of those tuples.
//

let rec _getCountsForAllLangs abcList L accumulator =
  match L with
  |[] -> List.rev(accumulator)
  |e::rest -> _getCountsForAllLangs abcList rest ((getCounts abcList e)::accumulator)
  
let getCountsForAllLangs abcList L =
  _getCountsForAllLangs abcList L []
  
  
//
// addName: creates tuple with language and the complete list of count tuples
//
let rec _addName countList nameList accumulator =
  match countList, nameList with
  |[],[] -> List.rev(accumulator)
  |[],_  -> List.rev(accumulator) //should not happen
  |_, [] -> List.rev(accumulator) //should not happen
  |e1::rest1, e2::rest2 -> _addName rest1 rest2 ((e2,e1)::accumulator)
  
  
let addName countList nameList = 
  _addName countList nameList []
  
  
//
// createListofCounts: extract the list counts from a language's letters
//      
  
let rec _createListofCounts L accumulator =
  match L with
  |[] -> List.rev(accumulator)
  |(letter,count)::rest -> _createListofCounts rest (count::accumulator)
  
let createListofCounts L =
  _createListofCounts L []
  


//
//printTupleFreq
//
let rec printTupleFreq L =
  match L with
  |[] -> ()
  |(letter,freq)::rest -> printf "%A " freq
                          printTupleFreq rest

//
//printTupleLetter
//
let rec printTupleLetter L =
  match L with
  |[] -> ()
  |(letter,freq)::rest -> printf "%s" letter
                          printTupleLetter rest
//
//returnInorder
//
let rec _returnInorder L accumulator =
  match L with
  |[] -> List.rev(accumulator)
  |(letter,freq)::rest -> _returnInorder rest (letter::accumulator)

let returnInorder L =
  _returnInorder L []


//
//print
//

let rec print L =
  match L with
  |[] -> ()
  |(name,countList)::rest -> printf "%A: " name
                             printTupleFreq countList
                             printfn ""
                             print rest
//
//printInOrder
//

let rec printInOrder L =
  match L with
  |[] -> ()
  |(name,countList)::rest -> printf "%A: " name
                             let sorted = List.rev(List.sortBy(fun (x,y) -> y) countList)
                             printTupleLetter sorted
                             printfn ""
                             printInOrder rest


// EXTRACT LISTS
let rec _extractLists L accumulator =
  match L with
  |[] -> List.rev(accumulator)
  |(lang,countList)::rest -> _extractLists rest (countList::accumulator)

let extractLists L =
  _extractLists L []
  
  
  
//Given a list of tuples, sort them
let rec _sortTuples L accumulator=
  match L with
  |[] -> List.rev(accumulator)
  |e::rest -> let sorted = List.rev(List.sortBy(fun (x,y) -> y) e)
              _sortTuples rest (sorted::accumulator)

let sortTuples L =
  _sortTuples L []
  
  
//for each tuple in a list, get the letter and build a list from it
let rec _getLetter L accumulator =
  match L with
  |[] -> List.rev(accumulator)
  |(letter,count)::rest -> _getLetter rest (letter::accumulator)

let getLetter L =
  _getLetter L []
  
  
//do the above function for a list of lists and create a new list of lists from it
let rec _createLetterList L accumulator =
  match L with
  |[] -> List.rev(accumulator)
  |e::rest -> _createLetterList rest ((getLetter e)::accumulator)

let createLetterList L =
  _createLetterList L []
  
  
  
// addIndex: creates tuples of (letter,indexInList)
let rec _addIndex L numList accumulator =
  match L ,numList  with
  |[],[] -> List.rev(accumulator)
  |[],_ -> List.rev(accumulator) //wont happen
  |_, []-> List.rev(accumulator) //wont happen
  |e1::rest , num::otherNum -> _addIndex rest otherNum ((num,e1)::accumulator)

let addIndex numList L =
  _addIndex L numList []
  
//addIndexForALL: does above for a List of Lists
let rec _addIndexForALL LL numlist accumulator =
  match LL with
  |[] -> List.rev(accumulator)
  |e::rest -> _addIndexForALL rest numlist ((addIndex e numlist)::accumulator)

let addIndexForALL LL numlist =
  _addIndexForALL LL numlist []


//whereIsAndDifference: find the index pos of the given letter and compute the difference

let rec whereIsAndDifference L tuple threshold =
  match L with
  |[] -> 0 //shoudl not happen, all letters are in every list
  |(letter,index)::rest -> let difference = (abs (index - (snd tuple)))
                           if ((letter = (fst tuple)) && (difference > threshold)) then difference
                           else whereIsAndDifference rest tuple threshold


//differenceALL : do above for a List of Tuples (above is for a single tuple) and build a sum that is 
//                contained of numbers above the threshold

let rec _differenceALL tupleList L threshold sum =
  match tupleList with
  |[] -> sum
  |e::rest -> _differenceALL rest L threshold ( (whereIsAndDifference L e threshold) + sum)


let differenceALL tupleList L threshold =
  _differenceALL tupleList L threshold 0
  
  
//
//differenceALLALL: above for a list of lists
//

let rec _differenceALLALL L1 L2 threshold accumulator =
  match L1 with
  |[] -> List.rev(accumulator)
  |e::rest -> _differenceALLALL rest L2 threshold ((differenceALL e L2 threshold)::accumulator)


let differenceALLALL L1 L2 threshold =
  _differenceALLALL L1 L2 threshold []
  

//addLangs: adds the language to the list from above

let rec _addLangs L langList accumulator =
  match L,langList with
  |[],[] -> List.rev(accumulator)
  |[],_  -> List.rev(accumulator) // wont happen
  |_, [] -> List.rev(accumulator) //wont happen
  |e1::rest1,e2::rest2 -> _addLangs rest1 rest2 ((e2,e1)::accumulator)

let addLangs L langList = 
  _addLangs L langList []







// *********************************************************************** //
//
// Main:
//
[<EntryPoint>]
let main argv =
  printfn ""
  printfn "** Training... **"
  printfn ""
  
  //---------------------------------------- PART 1 -----------------------------------------//
  
  // List of all leters
  let AllAlphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]
  let ListOfNumbers = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26]
  //printfn "AllAlphabet: %A" AllAlphabet
  //
  let files = [ for filename in System.IO.Directory.GetFiles(@"./training") -> filename]
  //printfn "FILES: %A"files
  // create a list of all language names
  let langNames = getLangs files
  //printfn "Lang Names: %A" langNames
  //
  let MainList = mainList files
  //printfn "MAINLIST: %A" MainList
  // creat list of tuples [(lang name, [exploded letters])...]
  let tupleList = mainListTuples MainList langNames
  //printfn "tupleList: %A" tupleList
  let total = getCountsForAllLangs AllAlphabet tupleList
  //printfn "total: %A" total
  let f = addName total langNames
  //printfn "f: %A" f

  printfn "** Letter Frequency Counts (A->Z) **"
  print f
  printfn ""
//---------------------------------------------------------------------------------------------//  
  
  


//------------------------------------------------- PART 2-------------------------------------//
  //
  printfn "** Letter Frequency Order (High->Low) **"
  printInOrder f
  printfn ""
  //
//----------------------------------------------------------------------------------------------//  
  
  
  
  
  
  
//-------------------------------------------- PART 3 --------------------------------------------------//  
  // Here we get text from the user, analyze, and guess the language:
  //
  printfn "Please enter text, followed by # (default = 'input.txt')> "
  let text = UserInput()
  let userCombined = combineLines text
  let userTuple = ("input",userCombined)
  //printfn "%A" userTuple
  let inputCounts = getCounts AllAlphabet userTuple
  
  //Print out numbers of letters
  printfn ""
  printf "\"input\": "
  printTupleFreq inputCounts
  printfn ""
  
  //Print out letters sorted by freq
  printf "\"input\": "
  //let IC = List.rev(List.sortBy(fun (letter,count) -> count) inputCounts)
  let IC = List.sortByDescending(fun (letter,count) -> count) inputCounts
  printTupleLetter IC
  printfn ""
  //
  
//----------------------------------------------------------------------------------------------//  
  
  
  
  
  
  
  
 //------------------------------------------- Part 4 ---------------------------------------------------//   
  
  printfn ""
  printf "Enter difference threshold (default = 4)> "
  let s = System.Console.ReadLine()
  let threshold = if s = "" then 4 else int(s)
  

  let counts = extractLists f //get only the counts for every language
  let sortedCounts = sortTuples counts //sort the counts in descending order
  let LetterList = createLetterList sortedCounts // removes the counts and keeps only the letter
  let InputList = getLetter IC
 
  let inputLetterIndex = addIndex InputList ListOfNumbers
  //printfn "inputLetterIndex: %A"inputLetterIndex

  
  let langLetterIndex = addIndexForALL LetterList ListOfNumbers
  //printfn "langLetterIndex: %A" langLetterIndex.[0]
  printfn ""
  
  let testTuple = ("a",1)
  let testNum = whereIsAndDifference inputLetterIndex testTuple threshold
  //printfn "testNum: %A" testNum
  
  let differenceTest = differenceALL inputLetterIndex langLetterIndex.[0] threshold
  //printfn "differenceTest: %A" differenceTest
  
  
  let full = differenceALLALL langLetterIndex inputLetterIndex threshold
 // printfn "Full: %A" full
  
  let Full = addLangs full langNames
  let diffs = List.sortBy(fun (x,y) -> y) Full
  printfn "diffs: %A" diffs
  
  
  
  
  printfn ""
  //
  let prediction = List.head diffs
  let answer = fst prediction
  printfn "** Input language: %A" answer 
  printfn ""
  //
  0
