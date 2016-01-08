module Main where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Int (toNumber)
import Data.Tuple
import Data.Foldable (traverse_, for_, all, foldlDefault)
import Data.Array ((!!), length)
import Data.List (List(), (..), filter, toList, index, updateAt, zip)
import qualified Data.List.Lazy as L

import Graphics.Canvas (Canvas(), getCanvasElementById, getContext2D)
import Graphics.Canvas.Free

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Signal hiding (filter)
import Signal.Time
import Signal.DOM

width = 400.0
height = 600.0

type Position = { row :: Int, col :: Int }
type OwnBlock = { block :: Block, pos :: Position }

data Cell = Cell String | EmptyCell

type GameState = { playing :: Boolean
                 , own :: OwnBlock
                 , cells :: List (List Cell)
                 }

data Block = Stick      Int
           | Square     Int
           | Tturn      Int
           | RightSnake Int
           | LeftSnake  Int
           | LeftGun    Int
           | RightGun   Int

data Action = NewGame
            | Tick
            | LeftPress
            | UpPress
            | RightPress
            | DownPress
            | SpacePress
            | None

get :: forall a. Int -> Int -> List (List a) -> Maybe a
get i j l =
  case index l i of
    (Just l') -> index l' j
    Nothing   -> Nothing

set :: forall a. Int -> Int -> a -> List (List a) -> List (List a)
set i j v l = let row = fromJust $ updateAt j v (fromJust (index l i))
              in fromJust $ updateAt i row l

swapRow :: forall a. Int -> Int -> List (List a) -> List (List a)
swapRow i j l = let r1 = fromJust (index l i)
                    r2 = fromJust (index l j)
                    l1 = fromJust (updateAt i r2 l)
                    l2 = fromJust (updateAt j r1 l1)
                 in l2

move :: (Int -> Int) -> (Int -> Int) -> Position -> Position
move frow fcol p = { row: frow p.row, col: fcol p.col }

minus :: Int -> Int -> Int
minus = flip sub

left :: Position -> Position
left = move id (minus 1)

right :: Position -> Position
right = move id (+1)

up :: Position -> Position
up = move (minus 1) id

down :: Position -> Position
down = move (+1) id

sticks :: Array (Array (Position -> Position))
sticks = [ [ id, up, up >>> up, up >>> up >>> up ]
         , [ id, right, right >>> right, right >>> right >>> right ]
         ]

squares :: Array (Array (Position -> Position))
squares = [ [ id, up, right >>> up, right] ]

tturns :: Array (Array (Position -> Position))
tturns = [ [ id, up, right, left]
         , [ id, up, right, down]
         , [ id, right, down, left]
         , [ id, up, down, left]
         ]

rsnakes :: Array (Array (Position -> Position))
rsnakes = [ [id, right, down, down >>> left]
          , [id, up, right, right >>> down]
          ]

lsnakes :: Array (Array (Position -> Position))
lsnakes = [ [id, right >>> down, down, left]
          , [id, up, left >>> down, left]
          ]

lguns :: Array (Array (Position -> Position))
lguns = [ [id, right, right >>> down, left]
        , [id, up, up >>> right, down]
        , [id, right, left, left >>> up]
        , [id, up, down, left >>> down]
        ]

rguns :: Array (Array (Position -> Position))
rguns = [ [id, right, left >>> down, left]
        , [id, up, right >>> down, down]
        , [id, right >>> up, right, left]
        , [id, up, down, right >>> up]
        ]

points :: OwnBlock -> Array Position
points { block: (Stick      i), pos: p } = map ($ p) (fromJust $ sticks !! i)
points { block: (Square     i), pos: p } = map ($ p) (fromJust $ squares !! i)
points { block: (Tturn      i), pos: p } = map ($ p) (fromJust $ tturns !! i)
points { block: (RightSnake i), pos: p } = map ($ p) (fromJust $ rsnakes !! i)
points { block: (LeftSnake  i), pos: p } = map ($ p) (fromJust $ lsnakes !! i)
points { block: (LeftGun    i), pos: p } = map ($ p) (fromJust $ lguns !! i)
points { block: (RightGun   i), pos: p } = map ($ p) (fromJust $ rguns !! i)

colorOf :: Block -> String
colorOf (Stick      _) = "cyan"
colorOf (Square     _) = "yellow"
colorOf (Tturn      _) = "purple"
colorOf (RightSnake _) = "green"
colorOf (LeftSnake  _) = "red"
colorOf (LeftGun    _) = "blue"
colorOf (RightGun   _) = "orange"

nextShape :: Block -> Block
nextShape (Stick      i) = Stick      $ (i+1) `mod` (length sticks)
nextShape (Square     i) = Square     $ (i+1) `mod` (length squares)
nextShape (Tturn      i) = Tturn      $ (i+1) `mod` (length tturns)
nextShape (RightSnake i) = RightSnake $ (i+1) `mod` (length rsnakes)
nextShape (LeftSnake  i) = LeftSnake  $ (i+1) `mod` (length lsnakes)
nextShape (LeftGun    i) = LeftGun    $ (i+1) `mod` (length lguns)
nextShape (RightGun   i) = RightGun   $ (i+1) `mod` (length rguns)

withContext action = do
  mcanvas <- getCanvasElementById "c"
  case mcanvas of
    (Just canvas) -> do
      context <- getContext2D canvas
      runGraphics context action
    Nothing -> return unit

drawBackground = do
  setFillStyle "#444444"
  rect { x: 0.0, y: 0.0, w: width, h: height }
  fill

drawGrid = do
  setStrokeStyle "black"
  traverse_ drawVertical   (L.take 20 $ L.iterate (+ 20.0) 0.0)
  traverse_ drawHorizontal (L.take 30 $ L.iterate (+ 20.0) 0.0)
    where
      drawVertical x = do
        beginPath
        moveTo x 0.0
        lineTo x height
        closePath
        stroke

      drawHorizontal y = do
        beginPath
        moveTo 0.0 y
        lineTo width y
        closePath
        stroke

drawOwnBlock own =
  for_ (points own) \p -> do
    drawCell (toNumber p.row) (toNumber p.col) (colorOf own.block)

drawCell row col color = do
  setFillStyle color
  fillRect { x: 1.0 + col * 20.0, y: 1.0 + row * 20.0, w: 18.0, h: 18.0 }

drawCells rows =
  for_ (zip (0..29) rows) \(Tuple i row) -> do
    for_ (zip (0..19) row) \(Tuple j cell) -> do
      case cell of
        (Cell color) -> drawCell (toNumber i) (toNumber j) color
        EmptyCell -> return unit

newGameState :: GameState
newGameState = { playing: true
               , own: { block: Tturn 1
                      , pos: { row : 0, col: 9 }
                      }
               , cells: rows
               }
  where
    rows :: List (List Cell)
    rows = L.fromList $ L.take 30 (L.repeat row)

    row :: List (Cell)
    row = L.fromList $ L.take 20 (L.repeat EmptyCell)

moveBlock :: (Position -> Position) -> GameState -> GameState
moveBlock f game = game { own = game.own { pos = f game.own.pos } }

rotateBlock :: GameState -> GameState
rotateBlock game =
  let next = nextShape game.own.block
  in game { own = game.own { block = next } }

inBound :: Position -> Boolean
inBound p = p.row < 30 && 0 <= p.col && p.col < 20

strictInBound :: Position -> Boolean
strictInBound p = 0 <= p.row && p.row < 30 && 0 <= p.col && p.col < 30

isAvailPos :: List (List Cell) -> Position -> Boolean
isAvailPos matrix point = 
  case get point.row point.col matrix of
    (Just (Cell _)) -> false
    (Just EmptyCell) -> true
    Nothing -> inBound point

canMove :: (Position -> Position) -> GameState -> Boolean
canMove f game =
  let points1 = points game.own
      points2 = map f points1
  in all id (map (isAvailPos game.cells) points2)

abandonOwn :: GameState -> GameState
abandonOwn game =
  let p = filter strictInBound (toList $ points game.own)
      c = foldlDefault coloring game.cells p
  in game { cells = c }
  where
    color :: String
    color = colorOf game.own.block

    coloring :: List (List Cell) -> Position -> List (List Cell)
    coloring cells point = set point.row point.col (Cell color) cells

nextOwn :: OwnBlock -> OwnBlock
nextOwn own = let r = unsafePerformEff (randomInt 0 6)
                  b = fromJust $ [Stick 0, Square 0, Tturn 0, RightSnake 0, LeftSnake 0, LeftGun 0, RightGun 0] !! r
              in own { block = b, pos = { row: 0, col: 9 } }

filledRows :: List (List Cell) -> List Int
filledRows matrix = let p1 = map isRowFilled matrix
                        p2 = filter snd (zip (0..29) p1)
                        p3 = map fst p2
                    in p3
  where
    isEmpty :: Cell -> Boolean
    isEmpty EmptyCell = true
    isEmpty _ = false

    isRowFilled :: List Cell -> Boolean
    isRowFilled row = all (not <<< isEmpty) row

breakLines :: GameState -> GameState
breakLines game =
  let ridxs = filledRows game.cells
      cells1 = foldlDefault break game.cells ridxs
      cells2 = foldlDefault clear cells1 ridxs
  in game { cells = cells2 }
  where
    break :: List (List Cell) -> Int -> List (List Cell)
    break matrix ridx =
      let emptyRow = toList $ map (\_ -> EmptyCell) [0..19]
          matrix' = fromJust $ updateAt ridx emptyRow matrix
      in matrix'

    clear :: List (List Cell) -> Int -> List (List Cell)
    clear matrix ridx = go ridx matrix where
      go ridx matrix = if ridx > 0
                      then let matrix' = swapRow ridx (ridx-1) matrix in go (ridx-1) matrix'
                      else matrix

finish :: GameState -> GameState
finish game = game { playing = false }

step :: Action -> GameState -> GameState
step Tick game = step DownPress game
step LeftPress game =
  if game.playing
     then if canMove left game
             then moveBlock left game
             else game
     else game

step RightPress game =
  if game.playing
     then if canMove right game
             then moveBlock right game
             else game
     else game

step UpPress game =
  if game.playing
     then
       let
         next  = nextShape game.own.block
         game' = game { own = game.own { block = next } }
       in
         if canMove id game'
             then rotateBlock game
             else game
     else game

step DownPress game =
  if game.playing
     then if canMove down game
             then moveBlock down game
             else
               let g1 = abandonOwn game
                   g2 = breakLines g1
                   g3 = g2 { own = nextOwn game.own }
               in if canMove id g3
                     then g3
                     else finish g3
     else game

step SpacePress game =
  if game.playing
     then go game
     else game
  where
    go game = if canMove down game
                 then let game' = moveBlock down game in go game'
                 else step DownPress game

step None game = game

render :: forall e. GameState -> Eff (canvas :: Canvas | e) Unit
render game = withContext $ do
  drawBackground
  drawGrid
  drawOwnBlock game.own
  drawCells game.cells

main = do
  lkey <- keyPressed 37
  ukey <- keyPressed 38
  rkey <- keyPressed 39
  dkey <- keyPressed 40
  skey <- keyPressed 32

  let actions = fromJust $
                  mergeMany [ every 500.0 ~> (\_ -> Tick)
                            , lkey ~> (\t -> if t then LeftPress  else None)
                            , ukey ~> (\t -> if t then UpPress    else None)
                            , rkey ~> (\t -> if t then RightPress else None)
                            , dkey ~> (\t -> if t then DownPress  else None)
                            , skey ~> (\t -> if t then SpacePress else None)
                            ]

  let gameLogic = foldp step newGameState actions
  runSignal $ gameLogic ~> render

