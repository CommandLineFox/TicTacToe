{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.State
import Data.List (transpose)

-- 1. Struktura podataka Rose tree
data Rose a = Node a [Rose a] deriving (Show)

-- Metoda koja vraca broj cvorova u stablu
size :: Rose a -> Int
size (Node _ children) = 1 + sum (map size children)

-- Metoda koja vraca visinu stabla
height :: Rose a -> Int
height (Node _ []) = 0
height (Node _ children) = 1 + maximum (map height children)

-- Metoda koja vraca broj listova
leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ children) = sum (map leavesCount children)

-- Metoda koja vraca listove
leaves :: Rose a -> [a]
leaves (Node value []) = [value]
leaves (Node _ children) = concatMap leaves children

-- Metoda koja vraca sve elemente na odredjenoj dubini
elemsOnDepth :: Rose a -> Int -> [a]
elemsOnDepth (Node value _) 0 = [value]
elemsOnDepth (Node _ children) depth = concatMap (`elemsOnDepth` (depth - 1)) children

-- Funktor klase Rose
instance Functor Rose where
  fmap :: (a -> b) -> Rose a -> Rose b
  fmap f (Node value children) = Node (f value) (map (fmap f) children)

-- Levi fold nad stablom Rose
foldRose :: (a -> b -> b) -> b -> Rose a -> b
foldRose f acc (Node value children) = foldl (foldRose f) (f value acc) children

-- 2. Predstavljanje stanja u igri
-- Tip igraca
data Player = Player1 | Player2 deriving (Show, Eq)

-- Stanje igre sa opstim tipom board
data GameState board = GameState {currentPlayer :: Player, board :: board} deriving (Show)

-- Potez u igri
data Move board = Move {player :: Player, position :: (Int, Int)} deriving (Show, Eq)

-- State monada za stanje u igri
newtype GameStateOp board a = GameStateOp {runOp :: GameState board -> (a, GameState board)}

instance Functor (GameStateOp board) where
  fmap f (GameStateOp op) = GameStateOp (\state -> let (x, newState) = op state in (f x, newState))

instance Applicative (GameStateOp board) where
  pure x = GameStateOp (\state -> (x, state))
  (GameStateOp f) <*> (GameStateOp x) =
    GameStateOp
      ( \state ->
          let (f', newState) = f state
           in let (x', finalState) = x newState
               in (f' x', finalState)
      )

instance Monad (GameStateOp board) where
  return = pure
  (GameStateOp op) >>= f =
    GameStateOp
      ( \state ->
          let (x, newState) = op state
           in runOp (f x) newState
      )

-- State monada za stara stanja igre
newtype GameStateOpHistory board a = GameStateOpHistory {runOpHistory :: [GameState board] -> (a, [GameState board])}

instance Functor (GameStateOpHistory board) where
  fmap f (GameStateOpHistory op) = GameStateOpHistory (\states -> let (x, newStates) = op states in (f x, newStates))

instance Applicative (GameStateOpHistory board) where
  pure x = GameStateOpHistory (\states -> (x, states))
  (GameStateOpHistory f) <*> (GameStateOpHistory x) =
    GameStateOpHistory
      ( \states ->
          let (f', newStates) = f states
           in let (x', finalStates) = x newStates
               in (f' x', finalStates)
      )

instance Monad (GameStateOpHistory board) where
  return = pure
  (GameStateOpHistory op) >>= f =
    GameStateOpHistory
      ( \states ->
          let (x, newStates) = op states
           in runOpHistory (f x) newStates
      )

-- 3. Primena kreiranih tipova na primeru igre Iks-Oks
-- Vrednost polja
data Symbol = X | O | Empty deriving (Show, Eq)

-- Sama tabla za iks-oks
type Board = [[Symbol]]

-- Metod koji radi ispis table u zadatom formatu
--  |X|O| |
--  | |X| |
--  |O|X| |
printBoard :: Board -> IO ()
printBoard board = putStrLn $ unlines $ map formatRow board
  where
    formatRow :: [Symbol] -> String
    formatRow symbols = "|" ++ concatMap showSymbol symbols

    showSymbol :: Symbol -> String
    showSymbol X = "X|"
    showSymbol O = "O|"
    showSymbol Empty = " |"

-- Metod koji vraca sve validne poteze
validMovesForBoard :: GameState Board -> [Move Board]
validMovesForBoard gameState = [Move (currentPlayer gameState) (i, j) | i <- [0 .. 2], j <- [0 .. 2], isValidMove gameState (i, j)]

-- Metod koji proverava da li je polje prazno
isValidMove :: GameState Board -> (Int, Int) -> Bool
isValidMove gameState (i, j) = (board gameState !! i !! j) == Empty

-- Metod koji vraca novo stanje table primenom jednog poteza
makeMove :: Move Board -> GameState Board -> GameState Board
makeMove (Move player (i, j)) (GameState currentPlayer board) =
  let symbol = case player of
        Player1 -> X
        Player2 -> O
   in GameState (switchPlayer currentPlayer) (updateBoard board i j symbol)

-- Metod koji azurira tablu
updateBoard :: Board -> Int -> Int -> Symbol -> Board
updateBoard board i j symbol =
  take i board ++ [take j (board !! i) ++ [symbol] ++ drop (j + 1) (board !! i)] ++ drop (i + 1) board

-- Metod za promenu igraca
switchPlayer :: Player -> Player
switchPlayer Player1 = Player2
switchPlayer Player2 = Player1

-- Metod koji proverava da li se igra zavrsila
isGameOver :: Board -> Bool
isGameOver board = isWinningState board || isBoardFull board
  where
    isWinningState :: Board -> Bool
    isWinningState board = any (allEqual . map (\(row, col) -> board !! row !! col)) winningCombinations

    allEqual :: [Symbol] -> Bool
    allEqual [] = True
    allEqual (x : xs) = all (\y -> y == x && (y == X || y == O)) xs

    winningCombinations :: [[(Int, Int)]]
    winningCombinations =
      [ [(0, 0), (0, 1), (0, 2)],
        [(1, 0), (1, 1), (1, 2)],
        [(2, 0), (2, 1), (2, 2)],
        [(0, 0), (1, 0), (2, 0)],
        [(0, 1), (1, 1), (2, 1)],
        [(0, 2), (1, 2), (2, 2)],
        [(0, 0), (1, 1), (2, 2)],
        [(0, 2), (1, 1), (2, 0)]
      ]

    isBoardFull :: Board -> Bool
    isBoardFull = all (notElem Empty)

-- Metod koji pravi novo stablo
createGameTree :: GameState Board -> Rose (GameState Board)
createGameTree gameState@(GameState _ board)
  | isGameOver board = Node gameState []
  | otherwise = Node gameState children
  where
    validMoves = validMovesForBoard gameState
    children = map (\move -> createGameTree (makeMove move gameState)) validMoves

-- Startna tabla
emptyBoard :: Board
emptyBoard = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

main :: IO ()
main = do
  let initialState = GameState Player1 emptyBoard
      gameTree = createGameTree initialState
  putStrLn $ "Velicina stabla: " ++ show (size gameTree)
  putStrLn $ "Visina stabla: " ++ show (height gameTree)
  putStrLn $ "Broj listova u stablu: " ++ show (leavesCount gameTree)
  putStrLn $ "Listovi u stablu: " ++ show (leaves gameTree)