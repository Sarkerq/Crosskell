module Geometry  
(
    genCrossword
) where
data Crossword = Crossword { fields :: [Field],
                             xSize :: Int,
                             ySize :: Int
                           } deriving (Show)  
data SparseCrossword = SparseCrossword  { sparseFields :: [Field],
                                          crosses :: Int
                                        } deriving (Show) 
data Field = Field { letter :: Char,
                     xCoord :: Int, 
                     yCoord :: Int,
                     disabled :: Bool,
                     clue :: Maybe String
                     } deriving (Show) 

data Direction = ToDown | ToRight deriving(Show,Eq)
type Key = String
type Value = String
type KeyValue = (Key, Value)

genCrossword :: [KeyValue] -> String
genCrossword [] = ""
genCrossword ((x, y):rest) = toString (toCrossword  (backtrackCrossword (initSparse (x,y)) rest ) )

backtrackCrossword :: SparseCrossword -> [KeyValue] -> SparseCrossword
backtrackCrossword cword [] = cword
backtrackCrossword cword  candidates =  bestFit (potentialSolutions cword candidates)

bestFit :: [SparseCrossword] -> SparseCrossword
bestFit [] = SparseCrossword [] 0
bestFit (cword:cwords) = if (crosses cword) > (crosses (bestFit cwords))
    then cword
    else bestFit cwords

potentialSolutions :: SparseCrossword -> [KeyValue] -> [SparseCrossword]
potentialSolutions cword candidates =  concat (map (availableInsertions cword) candidates)

availableInsertions :: SparseCrossword -> KeyValue -> [SparseCrossword]
availableInsertions cword candidate = map (insertKeyValue cword candidate) (availableSpots cword (fst candidate) (fst candidate) 0)

insertKeyValue :: SparseCrossword -> KeyValue -> (Int, Int,Direction) -> SparseCrossword
insertKeyValue cword keyvalue coords@(x,y,_) = SparseCrossword (addOrEditFieldsToList (sparseFields cword) ([Field  (head (fst keyvalue)) x y False (Just (snd keyvalue))] ++ (tailSparse  (tail (fst keyvalue)) coords) )) (numCrosses cword coords (length (tail (fst keyvalue))))

tailSparse :: String -> (Int,Int,Direction) -> [Field]
tailSparse [] _ = []
tailSparse (a:as) (x, y, dir) 
 | dir == ToDown = [Field a x (y+1) False Nothing] ++ (tailSparse as (x,y+1,dir))
 | dir == ToRight = [Field a (x+1) y False Nothing] ++ (tailSparse as (x+1,y,dir))

availableSpots :: SparseCrossword -> Key -> Key -> Int -> [(Int,Int,Direction)]
availableSpots cword [] _ _ = []
availableSpots cword (a:as) wholeKey x =filter (\tuple -> tryInsert cword wholeKey tuple) (concat (map (\field ->  [(xCoord field,yCoord field - x ,ToDown), (xCoord field - x, yCoord field, ToRight)]  )  (filter (\field -> letter field == a && disabled field == False)  (sparseFields cword)))) ++ (availableSpots (cword) as wholeKey (x+1))

tryInsert :: SparseCrossword -> Key -> (Int,Int,Direction) -> Bool
tryInsert _ [] _ = True 
tryInsert cword (a:as) position@(x,y,dir) = (tryInsertSingle cword a position) && (if(dir == ToDown) then tryInsert cword as (x,y+1, dir) else tryInsert cword as (x+1,y,dir))

tryInsertSingle :: SparseCrossword -> Char -> (Int,Int,Direction) -> Bool
tryInsertSingle cword a position@(x,y,dir) = length (filter (\field -> xCoord field == x && yCoord field == y && disabled field == False)  (sparseFields cword)) == 1

addOrEditFieldsToList :: [Field] -> [Field] -> [Field]
addOrEditFieldsToList olds [] = olds
addOrEditFieldsToList olds (new:news) = addOrEditFieldsToList (addOrEditSingleFieldToList olds new) news

addOrEditSingleFieldToList :: [Field] -> Field -> [Field]
addOrEditSingleFieldToList [] new = [new]
addOrEditSingleFieldToList (old:olds) new = if (xCoord old == xCoord new) && (yCoord old == yCoord new)
    then  [Field (letter old) (xCoord old) (yCoord old) True (if (clue old) == Nothing then clue new else clue old )]      ++ olds
    else [old] ++ (addOrEditSingleFieldToList olds new)

numCrosses :: SparseCrossword -> (Int, Int,Direction) -> Int -> Int
numCrosses cword _ _  = length (filter (\x -> disabled x) (sparseFields cword))

initSparse :: KeyValue -> SparseCrossword
initSparse (x,y) = SparseCrossword ([Field (head x) 0 0 False (Just y) ] ++ initTailSparse (tail x) 1  ) 0

initTailSparse :: String -> Int -> [Field]
initTailSparse [] _ = []
initTailSparse (a:as) x = [Field a x 0 False Nothing ]  ++ initTailSparse as (x+1)

toString :: Crossword -> String
toString cword = mySplit (map letter (fields cword)) (xSize cword)


mySplit :: String -> Int -> String
mySplit [] _ = []
mySplit xs interval = 
    if length xs <= interval
    then xs
    else take interval xs ++ ['\n'] ++ mySplit (drop interval xs) interval

toCrossword :: SparseCrossword -> Crossword
toCrossword x = toCrosswordWithBounds (minValX x) (maxValX x) (minValY x) (maxValY x) x

minValX :: SparseCrossword -> Int
minValX cword = if null (sparseFields cword)
    then 0
    else  foldl min 0 (map (xCoord) (sparseFields cword))
minValY :: SparseCrossword -> Int
minValY cword = if null (sparseFields cword)
    then 0
    else  foldl min 0 (map (yCoord) (sparseFields cword))
maxValX :: SparseCrossword -> Int
maxValX cword = if null (sparseFields cword)
    then 0
    else  foldl max 0 (map (xCoord) (sparseFields cword))
maxValY :: SparseCrossword -> Int
maxValY cword = if null (sparseFields cword)
    then 0
    else  foldl max 0 (map (yCoord) (sparseFields cword))

toCrosswordWithBounds :: Int -> Int -> Int -> Int -> SparseCrossword -> Crossword
toCrosswordWithBounds minX maxX minY maxY cword = insertSparse (generateEmptyCrossword (maxX - minX + 1) (maxY - minY + 1)) cword minX minY

generateEmptyCrossword :: Int -> Int -> Crossword
generateEmptyCrossword x y = Crossword (map (\n -> Field '-' (n `mod` x) (n `div` x) False Nothing) [0 .. (x*y) - 1]  ) x y

getField :: Crossword -> Int -> Int -> Maybe Field
getField cword x y  
    | x <0 || y<0 || xSize cword <= x || ySize cword <=y = Nothing
    | otherwise = Just ((fields cword) !! (y* (xSize cword) + x))

insertSparse :: Crossword -> SparseCrossword -> Int -> Int -> Crossword
insertSparse cword sparseWord x y 
        |  null (sparseFields sparseWord) = cword
        | otherwise = insertSparse  (insertField cword (head (sparseFields sparseWord)) x y) (SparseCrossword (tail (sparseFields sparseWord)) 0) x y

insertField :: Crossword -> Field -> Int -> Int -> Crossword
insertField cword field x y = 
    let (xs,ys) = splitAt ((xSize cword)* (yCoord field - y) + (xCoord field -x)) (fields cword) in
        Crossword (xs ++ (Field (letter field) (xCoord field -x) (yCoord field - y) (disabled field) (clue field) ) : (if null ys then [] else tail ys)) (xSize cword) (ySize cword)