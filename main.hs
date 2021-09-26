import System.IO
import Data.List
import Data.Maybe
import System.Random

setup_brd brd = map (\x -> [-1] ++ x ++ [-1]) m where
    amount_r = length brd
    amount_c = maximum $ map (\x -> length x) brd
    max = maximum [amount_r, amount_c]
    m = [take max [-1,-1..]] ++ brd ++ [take max [-1,-1..]]

iter_brd :: Int -> [[Int]] -> [((Int, Int), Int)] 
iter_brd i (x:[]) = iter_list i 0 x
iter_brd i (x:xs) = iter_list i 0 x ++ iter_brd (i+1) xs

iter_list :: Int -> Int -> [Int] -> [((Int, Int), Int)]
iter_list i j (x:[]) = [((i, j), x)] 
iter_list i j (x:xs) = [((i, j), x)] ++ iter_list i (j+1) xs

get_givens :: [[Int]] -> [Int]
get_givens brd = givens ++ [last givens] where
    givens = sort $ map (\((_,_),x) -> x) $ filter ((> 0) . snd) $ iter_brd 0 brd

find_val :: [[Int]] -> Int -> (Int, Int)
find_val brd val = fst $ head $ filter ((== val) . snd) $ iter_brd 0 brd

get_adj brd r c = filter ((>=0) . snd) $ map (\(ri, ci) -> ((r+ri, c+ci), brd !! (r+ri) !! (c+ci))) alldirs where
    alldirs = [(-1, 0), (-1, -1), (-1, 1), (0, 0), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

insert_val :: [[Int]] -> Int -> Int -> Int -> [[Int]]
insert_val (x:xs) 0 c val = [insertAt c x val] ++ xs
insert_val (x:xs) r c val = [x] ++ insert_val xs (r-1) c val

insertAt :: Int -> [Int] -> Int -> [Int] 
insertAt _ []     val = []
insertAt 0 (x:xs) val = [val] ++ xs
insertAt i (x:xs) val = [x] ++ insertAt (i-1) xs val 

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

listNullCells brd = filter ((== 0) .snd) $ iter_brd 0 brd

getNulleableCells brd last = filter ((<last) .snd) $ filter ((>1) .snd) $ iter_brd 0 brd

insertListValues brd [] = brd 
insertListValues brd (((i,j),v):xs) = insertListValues cbrd xs where
    cbrd = insert_val brd i j v

printBrd brd = putStrLn $ pB brd 0 0 nRows nCols max_space where
    nRows     = length brd
    nCols     = maximum $ map (\x -> length x) brd
    max_space = length $ show $ maximum $ map maximum brd
    pB brd cRow cCol mRow mCol maxSpace
        | cRow == mRow = ""
        | cCol == mCol = "\n" ++ pB brd (cRow+1) 0 mRow mCol maxSpace
        | otherwise    = nsp ++ elem ++ pB brd cRow (cCol+1) mRow mCol maxSpace where
            elem = if brd !! cRow !! cCol == -1 then " " else show $ brd !! cRow !! cCol
            nsp  = concat [" " | _ <- [0..(maxSpace - length elem)]]

solve brd (r, c) n given
    | n >  last given = [brd]
    | n == head given = if null nInAdj then [] 
        else solve brd (fst $ head nInAdj) (n+1) (tail given)
    | otherwise = concatMap (\((i,j),_) -> solve (insert_val brd i j n) (i,j) (n+1) given) emptySlots where
        adjN = get_adj brd r c
        nInAdj = filter ((== n) . snd) adjN 
        emptySlots = filter ((== 0) . snd) adjN

sample1 :: [[Int]]
sample1 = [[6,  0,  9],
           [0,  2,  8],
           [1,  0,  0]]

sample2 :: [[Int]]
sample2 =  [[8,  0,  2,  0],
            [0,  7,  4,  1],
            [0,  0,  0,  5],
            [0, 13,  0, 16]]

sample3 :: [[Int]]
sample3 =    [[-1, -1, -1,  0, 22,  0],
              [ 8,  0, -1, 20,  0,  0],
              [ 6,  0, 10, -1, 16, 14],
              [ 0,  1,  0, 11,  0,  0],
              [-1,  4,  0,  0, -1, -1]]

sample4 :: [[Int]]
sample4 =    [[0,  33,  35, 0,   0, -1, -1, -1],
              [0,   0,  24, 22,  0, -1, -1, -1],
              [0,   0,   0, 21,  0,  0, -1, -1],
              [0,  26,   0, 13, 40, 11, -1, -1],
              [27,  0,   0,  0,  9,  0,  1, -1],
              [-1, -1,   0,  0, 18,  0,  0, -1],
              [-1, -1,  -1, -1,  0,  7,  0,  0],
              [-1, -1,  -1, -1, -1, -1,  5,  0]]

shapes = [
        
    ((3,3), [[0,  0,  0],
             [0,  0,  0],
             [0,  0,  0]]),

    ((4,4), [[0,  0,  0, 0],
             [0,  0,  0, 0],
             [0,  0,  0, 0],
             [0,  0,  0, 0]]),
            
    ((5,6), [[-1, -1, -1,  0,  0,  0],
             [ 0,  0, -1,  0,  0,  0],
             [ 0,  0,  0, -1,  0,  0],
             [ 0,  0,  0,  0,  0,  0],
             [-1,  0,  0,  0, -1, -1]]),

    ((8,8), [[0,   0,   0,  0,  0, -1, -1, -1],
             [0,   0,   0,  0,  0, -1, -1, -1],
             [0,   0,   0,  0,  0,  0, -1, -1],
             [0,   0,   0,  0,  0,  0, -1, -1],
             [0,   0,   0,  0,  0,  0,  0, -1],
             [-1, -1,   0,  0,  0,  0,  0, -1],
             [-1, -1,  -1, -1,  0,  0,  0,  0],
             [-1, -1,  -1, -1, -1, -1,  0,  0]])

    ]

find_solution _brd = do
    let brd   = setup_brd _brd
    let given = get_givens brd
    let start = find_val brd 1
    let rbrd  = solve brd start 1 given
    putStrLn "> Sample:"
    printBrd brd
    putStrLn "> Solution:"
    printBrd $ rbrd !! 0


gen_board = do
    seed1 <- newStdGen
    seed2 <- newStdGen
    seed3 <- newStdGen

    let sample = shapes !! head ( map (\x -> x `mod` (length shapes) ) ( randomlist 1 seed1 ) )
    
    let init_brd = setup_brd (snd sample)
    
    let brd_cells = listNullCells init_brd
    let num_cells = length brd_cells
    let ifl_rpos  = map (\x -> x `mod` (num_cells)) ( randomlist 2 seed2 )
    let ffl_rpos  = if (ifl_rpos !! 0) == (ifl_rpos !! 1) then  [ifl_rpos !! 0, ((ifl_rpos !! 0) + 1) `mod` (num_cells)] else ifl_rpos 
    let positions = [(fst $ brd_cells !! (ffl_rpos !! 0), 1), (fst $ brd_cells !! (ffl_rpos !! 1), (num_cells))]
    let brd       = insertListValues init_brd positions 
    
    let given = get_givens brd
    let start = find_val brd 1

    putStrLn $ "> Generating board of (" ++ (show $ fst $ fst sample) ++ "," ++ (show $ snd $ fst sample) ++ "): \n"

    let rbrd = solve brd start 1 given
    
    let nonv = round $ (fromIntegral (2 * num_cells)) / (fromIntegral 3 )
    let fnp  = getNulleableCells (rbrd !! 0) (last given)
    let rpos = map (\x -> x `mod` ( num_cells - 2 )) ( randomlist nonv seed3 )
    let snp  = concatMap (\x -> [(fst $ fnp !! x, 0)]) rpos
    let quiz = insertListValues (rbrd !! 0) snp
        
    putStrLn $ "> Generated Board:"
    printBrd quiz    

    putStrLn $ "> Solved Board:"
    printBrd $ rbrd !! 0


    

    