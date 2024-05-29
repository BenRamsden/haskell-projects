Noughts and Crosses
===================

The 3x3 board is represented as a list of list of player values:

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a space on the board that is not yet occupied:

> import Data.List (maximumBy, transpose)
> import Data.Ord (comparing)
> import Data.List.Split (splitOn)
>
> type Board                    = [[Player]]
> data Player                   =  Nought | Blank | Cross
>                                  deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard                     :: Board -> IO ()
> showBoard                     =  putStrLn . unlines . concat . interleave bar . map showRow
>                                  where
>                                     bar = [replicate 18 '-']
>
> showRow                       :: [Player] -> [String]
> showRow                       =  beside . interleave bar . map showPlayer
>                                  where
>                                     beside = foldr1 (zipWith (++))
>                                     bar    = replicate 5 "|"
>
> showPlayer                    :: Player -> [String]
> showPlayer Nought             =  ["     ", " +-+ ", " | | ", " +-+ ", "     "]
> showPlayer Blank              =  ["     ", "     ", "     ", "     ", "     "]
> showPlayer Cross              =  ["     ", " \\ / ", "  X  ", " / \\ ", "     "]
>
> interleave                    :: a -> [a] -> [a]
> interleave x []               =  []
> interleave x [y]              =  [y]
> interleave x (y:ys)           =  y : x : interleave x ys

----------------------------------------------------------------------

> type Line						= [Player]
> type LineData					= (Bool, Int)
>
> emptyBoard					= [[Blank,  Blank, Blank], [Blank, Blank,  Blank], [Blank, Blank,  Blank]]
>
> --Gets players Nought or Cross preference, then begins game loop
> main                          :: IO ()
> main                          = do
>                              putStrLn "Welecome To Noughts and Crosses! Nought (N) or Cross (C)?"
>                              playerIn <- getLine
>                              let player = stringToPlayer playerIn
>                              if player==Blank then return() else humanLoop player emptyBoard
> 
> --Converts "N" and "n" to Nought, "C" and "c" to Cross, and anything else to Blank (invalid)
> stringToPlayer               :: String -> Player
> stringToPlayer str           = if (str == "N" || str == "n") then Nought
>                                else if (str == "C" || str == "c") then Cross
>                                else Blank
>
> --Asks for human players next move coordinates, checks input then makes the move and passes game to AI
> humanLoop                    :: Player -> Board -> IO()
> humanLoop player board       = do
>                              showBoard (board)
>                              putStrLn (show(player) ++ ": Pick your next move y x")  --y read first
>                              playerIn <- getLine
>                              if notElem ' ' playerIn then invalidInput player board
>                                 else do
>                                     let ins = (splitOn " " playerIn)
>                                     if(length ins /= 2) then invalidInput player board
>                                       else do
>                                           let (x,y) = (read (ins!!1) :: Int, read (ins!!0) :: Int)
>                                           if invalidBoardIndex (x,y) board then invalidInput player board
>                                              else do
>                                                  let newBoard = putPlayer player (x,y) board
>                                                  if elem newBoard (boardToChoices player board) then changeState player newBoard False
>                                                     else invalidInput player board
>
> --Runs if the human player enters invalid input
> invalidInput                  :: Player -> Board -> IO()
> invalidInput player board     = do
>                               putStrLn "Invalid Input" 
>                               humanLoop player board
>
> --Checks if coordinates given exist on the board
> invalidBoardIndex             :: (Int,Int) -> Board -> Bool
> invalidBoardIndex (x,y) brd   = (x < 0) || (y < 0) || (y > length brd - 1) || (x > length (brd !! y) - 1)
>
> --Makes the best possible choice using miniMax algorithm then passes back to Human player
> aiLoop                        :: Player -> Board -> IO()
> aiLoop aiPlayer board         = do
>                               let bestBoard = getBestBoard aiPlayer (boardToChoices aiPlayer board)
>                               changeState aiPlayer bestBoard True
>
> --Checks if player has won, then board is not full (draw), then passes to next player
> changeState                   :: Player -> Board -> Bool -> IO()
> changeState player board ai   = do
>                               if hasWon player board then endGame player board False --player wins
>                                 else if notElem Blank $ concat board then endGame player board True --fullboard
>                                      else if ai then humanLoop (nextPlayer player) board --ai->human transition
>                                           else aiLoop (nextPlayer player) board --human->ai transition
>
> --Called if board is full or player wins
> endGame                       :: Player -> Board -> Bool -> IO()
> endGame player board draw     = do
>                               showBoard board
>                               if(draw) then putStrLn ("Draw!") else putStrLn (show(player) ++ "'s won!")
>
> --Forces AI to make final winning move, if none gets the board from the highest rated (Board,Int) pair
> getBestBoard                  :: Player -> [Board] -> Board
> getBestBoard player boards    = if length winners > 0 then winners!!0 else fst $ maximumBy (comparing snd) winPairs
>                                  where winPairs = boardsToWinPairs player boards
>                                        winners = filter (hasWon player) (map fst winPairs)                      
> --Returns opposite player to that entered
> nextPlayer                    :: Player -> Player
> nextPlayer Nought		        = Cross
> nextPlayer Cross              = Nought
>
> --Returns a list of rows each with just one more blank set to the player
> rowToChoices                  :: Player -> Line -> [Line]
> rowToChoices _ []             = []
> rowToChoices player (c:cs)    = (if (c==Blank) then [player : cs] else []) ++ (map (c:) (rowToChoices player cs))
>
> --Returns a list of boards each with just one more blank set to the player (maps rowToChoices)
> boardToChoices                :: Player -> Board -> [Board]
> boardToChoices _ []			= []
> boardToChoices player (l:ls)  = [x:ls | x <- rowToChoices player l] ++ (map (l:) (boardToChoices player ls))
>
> --Returns all possible final states of the board with the blanks filled in cartesian product style, filtered for valid combinations (similar nought and cross count)
> allPossibleBoards            :: Board -> [Board]
> allPossibleBoards b          = filter (\b -> abs(getPNum Nought b - getPNum Cross b) <= 1 ) cartesianBoards --prunes for Cross/Nought count difference of <= 1
>                                   where getPNum = (\p b -> length $ filter (==p) $ concat b)
>                                         cartesianBoards = sequence $ map sequence $ (map . map) (\p -> if p==Blank then [Nought,Cross] else [p]) b
>
> --Substitutes newX at the nth position in the list xs 
> replaceAt                     :: Int -> a -> [a] -> [a]
> replaceAt n newX xs         = a ++ (newX:b)
>                                   where (a, (_:b)) = splitAt n xs
>
> --Subs the player at position x in row y, then puts that new row back at row y in the board
> putPlayer                     :: Player -> (Int,Int) -> Board -> Board
> putPlayer player (x,y) board  = replaceAt y (replaceAt x player (board !! y)) board
>
> --Returns row LineData's for board [Nought,Nought,Nought] (True,3)
> rowWin						:: Player -> Board -> [LineData]
> rowWin player                 = map (\l -> let cs = map (==player) l in (and cs, sum $ map fromEnum cs))
>
> --Turns boards columns into rows, then passes to rowWin
> colWin						:: Player -> Board -> [LineData]
> colWin player                 = rowWin player . transpose
>
> --Row checks line from top left to bottom right of board (x==y)
> diagWinBWSlash				:: Player -> Board -> LineData
> diagWinBWSlash player board   = (bwsAllPlayer, fromEnum(bwsAllPlayer))
>                                   where bwsAllPlayer = all (==player) $ zipWith (!!) board [0..]
>
> --As above but reverses each row, checks line from bottom left to top right of board
> diagWinFWSlash                :: Player -> Board -> LineData
> diagWinFWSlash player         = diagWinBWSlash player . map reverse
>
> --Gathers all win data from row, column, diagBW, diagSW functions on board into one list
> playerWins					:: Player -> Board -> [LineData]	--0=rowWins 1=colWins 2=diagWins
> playerWins player b		    = (rowWin player b) ++ (colWin player b) ++ [diagWinBWSlash player b, diagWinFWSlash player b]
>
> --Checks if any Win functions fst == True which means the line was all that player (win)
> hasWon                        :: Player -> Board -> Bool
> hasWon player                 = or . map fst . playerWins player
>
> --Combines count of player on each row to the power 3, giving rough idea of player dominance on board
> lineDatasToInt				:: [LineData] -> Int
> lineDatasToInt       	     	= sum . map (\ld -> (snd ld) ^ 3) 
>
> --Pairs up the board with its MiniMax score, for sorting by the getBestBoard
> boardsToWinPairs              :: Player -> [Board] -> [(Board,Int)]
> boardsToWinPairs player bs    = map (\b -> (b, miniMaxScore player (allPossibleBoards b))) bs                                      
>
> --Subtracts opponent board score from players board score (minimax step)
> miniMaxScore                  :: Player -> [Board] -> Int
> miniMaxScore player allBs     = boardsToScore player allBs - boardsToScore (nextPlayer player) allBs
>
> --Sums score of all boards given to it into a single Int
> boardsToScore                 :: Player -> [Board] -> Int
> boardsToScore player allBs    = sum $ map lineDatasToInt $ map (playerWins player) allBs