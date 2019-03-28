// A silly enumeration of noughts and crosses, or tic-tac-toe.
// Basically, the value tictactoe builds the entire possible game play for a 3x3 board,
// starting with a blank board.

module TicTacToe

// There are eight possible winning positions.  If 0 indicates a square is not
// filled, but 1 and 2 represent O or X, then these are the logical conditions.
let wingame1 (x : int array) = x.[0] > 0 && x.[0] = x.[1] && x.[1] = x.[2]
let wingame2 (x : int array) = x.[3] > 0 && x.[3] = x.[4] && x.[4] = x.[5]
let wingame3 (x : int array) = x.[6] > 0 && x.[6] = x.[7] && x.[7] = x.[8]
let wingame4 (x : int array) = x.[0] > 0 && x.[0] = x.[3] && x.[3] = x.[6]
let wingame5 (x : int array) = x.[1] > 0 && x.[1] = x.[4] && x.[4] = x.[7]
let wingame6 (x : int array) = x.[2] > 0 && x.[2] = x.[5] && x.[5] = x.[8]
let wingame7 (x : int array) = x.[0] > 0 && x.[0] = x.[4] && x.[4] = x.[8]
let wingame8 (x : int array) = (x.[2] > 0) && (x.[2] = x.[4]) && (x.[4] = x.[6])

// Check the eight winning positions on a board.
let winning_game x = wingame1 x || wingame2 x || wingame3 x || wingame4 x || wingame5 x || wingame6 x || wingame7 x || wingame8 x

// A gametree is a tree of future moves, or a winning board or a drawed board (not a win, but no possible future moves.)
type GameTree = Node of (int array) * int * (GameTree option array) | Win of (int * int array) | Draw of (int array)

// Lets populate a board, given the player whose turn it is and the current state of the board.
let rec populate (board : int array) (player : int) =
    
    let nomoves nb = nb |> Array.map (fun x -> (x > 0)) |> Array.fold (&&) true

    // Given a position on a board and its current occupied value, either build a new board, or
    // we there's no turn for that square.  Hence the option type.  This is the future boards.
    let newposition (pos, opt) = 
        if (opt = 0) 
            then (Option.Some [| for i in 0 .. board.Length-1 -> if (i = pos) then player else board.[i] |]) 
            else None

    let switchplayer p = if (p = 1) then 2 else 1

    // Given a future board, either there was no possible move for the chosen position, or
    // the board is a winner, or a draw.  If none of these, we populate the next step for this position.
    let recurse newboard =
        match newboard with
        | None -> None
        | Some theboard -> if (winning_game theboard) 
                           then Some (Win (player,theboard))
                           else if (nomoves theboard) 
                                then Some (Draw theboard)
                                else Some (Node (theboard, player, (populate theboard (switchplayer player))))

    // Now the finale - calculate the future moves for the given board.
    Array.zip [| for i in 0 .. board.Length-1 -> i|] board |>
    Array.map newposition |>
    Array.map recurse

// Create all the moves for a 3x3 board.
let tictactoe = 
    let blankboard = [| for i in 0 .. 8 -> 0 |]

    Node (blankboard, 1, populate blankboard 1)

// Not sure what this is doing.
let findgame (barr : GameTree option array) =
       let rec g (n:int) = 
           if (n = 0)
           then barr.[0]
           else match barr.[n] with | None -> (g (n-1)) | Some b -> Some b

       g (barr.Length - 1)

// traverse the whole game to count the number of wins.
let rec countwins board = 
    match board with
    | Win b -> 1
    | Draw b -> 0
    | Node (a,b,rest) ->
        let f x = match x with
                    | None -> 0
                    | Some p -> countwins p
        rest |> Array.map f |> Array.sum

// traverse the whole game to count the number of draws.
let rec countdraws board = 
    match board with
    | Win b -> 0
    | Draw b -> 1
    | Node (a,b,rest) ->
        let f x = match x with
                    | None -> 0
                    | Some p -> countdraws p
        rest |> Array.map f |> Array.sum

// We could build an interactive game here - player goes first - we find the option with the best
// win to draw ratio and move that way - gives us the best possible chance of winning.  Of course,
// win doesn't, at the moment, announce WHO won.  That probably needs rectifying.
