(*
   Name: Micah Arndt
   File Name: main.ml
   *)

(*
   Initialization of the board, including size and holes
   *)

let size = read_int ();;
(*Initializes Holes*)
let getHoles () =
  let rec getHoles list = match read_int() with
      (-1) -> list
    | value -> getHoles (list @ [value])
  in getHoles []
;;

let holeList = getHoles();;
let generateCoords list =
  let rec genC list retList acc = match list with
      [] -> retList
    | x::xs -> if acc = (-1) then genC xs retList x
      else genC xs (retList @ [(acc, x)]) (-1)
  in genC list [] (-1)
;;

let generatePos list size =
  let rec genP list size retList = match list with
      [] -> retList
    | (x,y)::xs -> genP xs size (retList @ [x * size + y])
  in genP list size []
;;
let holeList = generatePos (generateCoords holeList) size;;

let init_board size =
  let rec initialize_board s list =
    if s > 0 then initialize_board (s - 1) ([" "] @ list)
    else list
  in initialize_board (size * size) []
;;

let init_Hole list pos size =
  let rec placeHole board n newBoard = match board with
      [] -> newBoard
    | x::xs -> if n = 0 && x = " " then placeHole xs (-1) (newBoard @ ["@"])
      else placeHole xs (n - 1) (newBoard @ [x])
  in placeHole list pos []
;;

let finishBoard holePosList board size =
  let rec fBoard hps retBoard = match hps with
      [] -> retBoard
    | x::xs -> fBoard xs (init_Hole board x size)
  in fBoard holePosList [];;

let init_inBetween size =
  let rec init_inBetween s string =
    if s > 1 then init_inBetween (s - 1) ("-+" ^ string)
    else string ^ "-"
  in init_inBetween size ""
;;



let board = finishBoard holeList (init_board size) size;;
let inBetween = init_inBetween size;;

(*
   Gets a string for the printing of the board
*)
let string_board list board_size =
let rec string_board list s board_size board_string =
  match list with
    [] -> board_string
  | x::xs -> 
               (if (s mod board_size > 0)
                then string_board xs (s - 1) board_size (
                    if (s - 1) mod board_size = 0
                    then  board_string ^ x ^ "\n" 
                    else  board_string ^ x ^ "|")
                else
                  string_board xs (s - 1) board_size (board_string ^
                                                      (if s = (board_size * (board_size)) then "" else
                                                         inBetween) ^ "\n" ^ x ^ "|")
               )
    in string_board list (size * size) size ""    
;;


let print_board board =
  let _ = print_string board in ()
;;

(*
Utility Function to check how many available places there are to play
   *)
let available_spaces list =
  let rec avSp list acc = match list with
      [] -> acc
    | x::xs -> avSp xs (if x = " " then acc + 1 else acc)
  in avSp list 0
;;

(*
   This returns a board for the Player Input
   *)

let playerTurn list (row, col) size =
  let rec place board n newBoard = match board with
      [] -> newBoard
    | x::xs -> if n = 0 && x = " " then place xs (-1) (newBoard @ ["X"])
      else place xs (n - 1) (newBoard @ [x])
  in place list (row * size + (col)) []
;;

(*
   Prompts the player for an input
   *)
let getValue () = read_int();;

let playerInputRow () = let _ = print_string "Row: " in getValue();;
let playerInputCol () = let _ = print_string "Col: " in getValue();;
let flip (x, y) = (y, x);;

(*Ensures the Space is Unoccupied*)
let getCharOnB board space = 
  let rec getChar board space acc = match board with
      [] -> acc
    | x::xs -> if space > 0 then getChar xs (space - 1) true
        else if x = " " then true else false
  in getChar board space true;;

let playerInput () = flip(playerInputCol (), playerInputRow());;
let valid_input (row, col) size =
  if (row >= size || row < 0)
  then let _ = print_string "Invalid Row!\n" in playerInput()
  else if (col >= size || col < 0) then let _ = print_string "Invalid Col!\n" in playerInput()
      else if not(getCharOnB board (row * size + col)) then let _ = "Space is Occupied!\n" in playerInput()
  else
    (row, col)
;;


(*
   The next few functions are used to check the Win Conditions.
   They will be used as functions on every turn
   *)

(*
   This is the Horizontal Win Condition, Generalized for both Computer and Player
   *)
let hWinCon list start checkFor size =
  let rec winH list sPos checkFor acc nChecked = match list with
      [] -> acc
    | x::xs ->
      (
        if sPos = 0 then
          if x = checkFor then
            if nChecked < size
            then winH xs sPos checkFor (acc && true) (nChecked + 1)
            else winH xs sPos checkFor (acc && true) (nChecked)
          else
          if nChecked != size then
            winH xs sPos checkFor (acc && false) (nChecked)
          else winH xs sPos checkFor (acc && true) (nChecked)
        else
          winH xs(sPos - 1) checkFor (true) (nChecked)
      )
  in winH list start checkFor true 0
;;

(*
All Horizontal Win Checks for the Player
   *)

let playerCheckH board size =
  let rec rHC board size acc current = 
      if current = (size) then  acc
      else
        rHC board size (acc || (hWinCon board (size * current) "X" size)) (current + 1)
  in rHC board size false 0
;;

(*
Checks the Vertical Win condition.
Generalized for both the player and computer
*)

let vWinCon list start checkFor size =
  let rec winV list sPos checkFor acc nChecked = match list with
      [] -> acc
    | x::xs ->
      (
        if sPos = 0 then
          (
            if x = checkFor then
              (
                if nChecked < size
                then winV xs (sPos + size - 1) checkFor (acc && true) (nChecked + 1)
                else winV xs sPos checkFor (acc && true) (nChecked)
              )
            else
            if nChecked != size then
              (
                winV xs sPos checkFor (acc && false) (nChecked)
              )
            else winV xs sPos checkFor (acc && true) (nChecked)
          )
        else
          winV xs (sPos - 1) checkFor (acc && true) (nChecked)
      )
  in winV list start checkFor true 0
;;


let playerCheckV board size =
  let rec rVC board size acc current = 
      if current = (size) then  acc
      else
        rVC board size (acc || (vWinCon board current "X" size)) (current + 1)
  in rVC board size false 0
;;



(*

   This checks the Diagonals
   *)
let dWinCon list start checkFor size =
  let rec winD list sPos checkFor acc nChecked = match list with
      [] -> acc
    | x::xs ->
      (
        if sPos = 0 then
          if x = checkFor then
            if nChecked < size
            then winD xs (sPos + size) checkFor (acc && true) (nChecked + 1)
            else winD xs sPos checkFor (acc && true) (nChecked)
          else
          if nChecked != size then
            winD xs sPos checkFor (acc && false) (nChecked)
          else winD xs sPos checkFor (acc && true) (nChecked)
        else
          winD xs(sPos - 1) checkFor (acc && true) (nChecked)
      )
  in winD list start checkFor true 0
;;


let playerCheckD board size =
  let rec rDC board size acc current = 
      if current > (size) then  acc
      else
        rDC board size (acc || (dWinCon board current "X" size)) (current + 2)
  in rDC board size false 0
;;


(*Putting it all together*)
let checkConP board size =
  (available_spaces board = 0) || (playerCheckH board size)
  || (playerCheckV board size) || (playerCheckD board size)
;;

(*
   These functions are for controling the AI opponent

   The AI makes a move based on this priority:
   -Win Move
       +Checks the three horizontal rows first
       +Then checks the vertical rows
       +Finally checks the diagonals
   -Block Move
   -First Slot Available
   *)

let getInHRow board start size checkFor ignoreFor =
  let rec cInHR board sPos size nChecked checkFor playerPiece = match board with
      [] -> nChecked
    | x::xs ->
      (
        if sPos = 0 then
          if x = checkFor then
            if nChecked < size - 1
            then cInHR xs sPos size (nChecked + 1) checkFor playerPiece
            else cInHR xs sPos size (nChecked) checkFor playerPiece
          else
          if nChecked != size then if x = playerPiece || x = "@"
            then cInHR xs sPos size (size + 1) checkFor playerPiece
            else cInHR xs sPos size (nChecked) checkFor playerPiece
          else cInHR xs sPos size (nChecked) checkFor playerPiece
        else
          cInHR xs(sPos - 1) size (nChecked) checkFor playerPiece
      )
  in cInHR board start size  0 checkFor ignoreFor
;;

let getInVRow board start size checkFor ignoreFor=
  let rec cInVR board sPos size nChecked checkFor playerPiece = match board with
      [] -> nChecked
    | x::xs ->
      (
        if sPos = 0 then
          if x = checkFor then
            if nChecked < size - 1
            then cInVR xs (sPos + size - 1)
                size (nChecked + 1) checkFor playerPiece
            else cInVR xs sPos size (nChecked) checkFor playerPiece
          else
          if nChecked != size then if x = playerPiece || x = "@"
            then cInVR xs sPos size (size + 1) checkFor playerPiece
            else cInVR xs sPos size (nChecked) checkFor playerPiece
          else cInVR xs sPos size (nChecked) checkFor playerPiece
        else
          cInVR xs(sPos - 1) size (nChecked) checkFor playerPiece
      )
  in cInVR board start size  0 checkFor ignoreFor
;;

let getVIndex board start size =
  let rec getVin board start size acc = match board with
      [] -> acc
    | x::xs ->
      if start = 0 then
        if x = " " then
          getVin xs (-1) size acc
        else getVin xs (size - 1) size (acc + size)
      else
          getVin xs(start - 1) size acc
     
in getVin board start size  start
;;


let getInDRRow board start size checkFor ignoreFor=
  let rec cInDR board sPos size nChecked checkFor playerPiece = match board with
      [] -> nChecked
    | x::xs ->
      (
        if sPos = 0 then
          if x = checkFor then
            if nChecked < size - 1
            then cInDR xs (sPos + size)
                size (nChecked + 1) checkFor playerPiece
            else cInDR xs sPos size (nChecked) checkFor playerPiece
          else
          if nChecked != size then if x = playerPiece || x = "@"
            then cInDR xs sPos size (size + 1) checkFor playerPiece
            else cInDR xs sPos size (nChecked) checkFor playerPiece
          else cInDR xs sPos size (nChecked) checkFor playerPiece
        else
          cInDR xs(sPos - 1) size (nChecked) checkFor playerPiece
      )
  in cInDR board start size  0 checkFor ignoreFor
;;

let getDRIndex board start size =
  let rec getDRin board start size acc = match board with
      [] -> acc
    | x::xs ->
      if start = 0 then
        if x = " " then
          getDRin xs (-1) size acc
        else getDRin xs (size - 1) size (acc + size + 1)
      else
          getDRin xs (start - 1) size acc
     
in getDRin board start size  start
;;

let getInDLRow board start size checkFor ignoreFor=
  let rec cInDLR board sPos size nChecked checkFor playerPiece = match board with
      [] -> nChecked
    | x::xs ->
      (
        if sPos = 0 then
          if x = checkFor then
            if nChecked < size - 1
            then cInDLR xs (sPos + size - 2)
                size (nChecked + 1) checkFor playerPiece
            else cInDLR xs sPos size (nChecked) checkFor playerPiece
          else
          if nChecked != size then if x = playerPiece || x = "@"
            then cInDLR xs sPos size (size + 1) checkFor playerPiece
            else cInDLR xs sPos size (nChecked) checkFor playerPiece
          else cInDLR xs sPos size (nChecked) checkFor playerPiece
        else
          cInDLR xs(sPos - 1) size (nChecked) checkFor playerPiece
      )
  in cInDLR board start size  0 checkFor ignoreFor
;;

let getDLIndex board start size =
  let rec getDLin board start size acc = match board with
      [] -> acc
    | x::xs ->
      if start = 0 then
        if x = " " then
          getDLin xs (-1) size acc
        else getDLin xs (size - 2) size (acc + size - 1)
      else
          getDLin xs (start - 1) size acc
     
in getDLin board start size  start
;;

let getFirstOpen board =
  let rec fO board index = match board with
      [] -> index
    | x::xs -> if x = " " then fO xs index
      else fO xs (index + 1)
  in fO board 0;;

(*For placeing the pieces for the computer*)
let computerTurn list index size =
  let rec placeC board n newBoard = match board with
      [] -> newBoard
    | x::xs -> if n = 0 && x = " " then placeC xs (-1) (newBoard @ ["O"])
      else  if n > 0 then placeC xs (n - 1) (newBoard @ [x])
          else placeC xs (n) (newBoard @ [x])
  in placeC list index []
;;

(*Horizontal Checks first*)
let computerMove board size =
  (*This First Segement is for checking the Computer Win Cons*)
  if getInHRow board 0 size "O" "X" = 2 then computerTurn board 0 size
  else
  if getInHRow board 3 size "O" "X" = 2 then computerTurn board 3 size
  else
  if getInHRow board 6 size "O" "X" = 2 then computerTurn board 6 size
  else
  if getInVRow board 0 size "O" "X" = 2
  then computerTurn board (getVIndex board 0 size)  size
  else
  if getInVRow board 1 size "O" "X" = 2
  then computerTurn board (getVIndex board 1 size)  size
  else
  if getInVRow board 2 size "O" "X" = 2
  then computerTurn board (getVIndex board 2 size)  size
  else
  if getInDRRow board 0 size "O" "X" = 2
  then computerTurn board (getDRIndex board 0 size)  size
  else
  if getInDLRow board 2 size "O" "X" = 2
  then computerTurn board (getDLIndex board 2 size)  size
  else
    (*Now We Want to scan to see possible player wins next*)
  if getInHRow board 0 size "X" "O" = 2 then computerTurn board 0 size
  else
  if getInHRow board 3 size "X" "O" = 2 then computerTurn board 3 size
  else
  if getInHRow board 6 size "X" "O" = 2 then computerTurn board 6 size
  else
  if getInVRow board 0 size "X" "O" = 2
  then computerTurn board (getVIndex board 0 size)  size
  else
  if getInVRow board 1 size "X" "O" = 2
  then computerTurn board (getVIndex board 1 size)  size
  else
  if getInVRow board 2 size "X" "O" = 2
  then computerTurn board (getVIndex board 2 size)  size
  else
  if getInDRRow board 0 size "X" "O" = 2
  then computerTurn board (getDRIndex board 0 size)  size
  else
  if getInDLRow board 2 size "X" "O" = 2
  then computerTurn board (getDLIndex board 2 size)  size
  else
    (*Now We Find the First Open*)
    computerTurn board (getFirstOpen board) size
;;

let compCheckH board size =
  let rec cHC board size acc current = 
      if current = (size) then  acc
      else
        cHC board size (acc || (hWinCon board (size * current) "O" size)) (current + 1)
  in cHC board size false 0
;;

let compCheckV board size =
  let rec cVC board size acc current = 
      if current = (size) then  acc
      else
        cVC board size (acc || (vWinCon board current "O" size)) (current + 1)
  in cVC board size false 0
;;



let compCheckD board size =
  let rec cDC board size acc current = 
      if current > (size) then  acc
      else
        cDC board size (acc || (dWinCon board current "O" size)) (current + 2)
  in cDC board size false 0
;;



(*Checks for AI*)
let checkConComp board size =
  (available_spaces board = 0) || (compCheckH board size)
  || (compCheckV board size) || (compCheckD board size)
;;

let checkDraw board =
  available_spaces board = 0
;;
 (*
   This is the main function.
   It is used to play the game
*)
let main board size=
  let rec  main board size turn =
    let _ = 
       print_board(string_board board size)
    in
    if checkConP board size
    then print_string "You Win!!\n"
    else
    if checkConComp board size then print_string "I Win!!\n"
    else if checkDraw board
    then print_string "Draw!!\n"
    else if turn then main (playerTurn board (valid_input(playerInput()) size) size) size false
        else
          main (computerMove board  size) size true
  in main board size true
;;

main board size;;

