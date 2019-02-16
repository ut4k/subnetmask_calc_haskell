-- ------------------------------------------------
-- 前提
--
-- (IPv4において)IPアドレスは32ビットの数値で表される
-- 8ビットずつ4組 |8bit|8bit|8bit|8bit|
--
-- 192.168.0.1/21 スラッシュの後ろにくっついてるのは32桁のうち"左端から何桁"が1で埋められるかの桁数
-- 21なら左から21個全部1で埋める
-- 11111111.11111111.11111000.00000000 -- 10進数に変換すると 255.255.248.0
--
-- サブネットマスクの2進数表記で0の部分がホスト部アドレスとして使用可能。
--
--
-- 参考:
-- https://www.pluralsight.com/blog/it-ops/simplify-routing-how-to-organize-your-network-into-smaller-subnets
--
-- ------------------------------------------------
--
-- こんな結果を得られるように作る
--
-- インプット:
--   * クラスCのIPだけとする! それ以外も考えると難しくてわからない!
--   192.168.150.77/21 (255.255.248.0)
--
-- アウトプット:
--   ネットワークアドレス 192.168.144.0
--   ホストアドレス       192.168.144.1 ～ 192.168.151.254
--
-- ------------------------------------------------

import Data.List
import Data.Char

-- decimal to binary
decToBin :: Int -> Int
decToBin 0 = 0
decToBin n = (n `mod` 2) + (10 * (decToBin $ n `div` 2))

-- stole from stackoverflow
binToDec :: Integral i => i -> i
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + (mod i 10)

--左からn番目まで1で埋める。残りは0で埋める
fillOneUntil :: Int -> [Int]
fillOneUntil n
    | n > 32    = take 32 (repeat 1)
    | otherwise = take n (repeat 1) ++ take (32 - n) (repeat 0)

-- stole from stackoverflow
-- slice 2 [1,2,3,4,5,6] ---> [[1,2],[3,4],[5,6]]
slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs = hs : slice n ts
  where (hs, ts) = splitAt n xs

digitsToInt :: [Int] -> Int
digitsToInt xs = foldl1 (\acc num -> 10 * acc + num) xs

-- acc アキュムレーター ... 前のステップで畳み込んだ結果
-- num 現在要素の値
--
-- digitsToInt [1,2,3] のとき何がおきてるか
--
-- fold1はアキュムレーターの初期値にリストの先頭をつかう ので 1
-- [注意] 要素1の畳み込みは行われず要素2から畳み込まれていく
--
-- Step1 : 10 * (アキュムレーター初期値1)        + リスト要素2 ---> 10  + 2 ----> 12
-- Step2 : 10 * (アキュムレーターに溜まった値12) + リスト要素3 ---> 120 + 3 ----> 123
--
-- 10×前の値 + 今の値...を繰り返すと数値のリストは数値をくっつけた数になる規則があるらしい。。。！？！

toDigits :: String -> [Int]
toDigits a = map digitToInt $ a

-- マスク番号から10進数のIPをリストで得る
maskNumToIp :: Int -> [Int]
maskNumToIp n = map binToDec (map digitsToInt $ slice 8 $ fillOneUntil n)

ipToBin :: [Int] -> [[Int]]
ipToBin xs = map zfill (map toDigits (map show (map decToBin xs)))

-- マスク番号から２進数のアドレスのリストを得る
maskNumToBin :: Int -> [[Int]]
maskNumToBin n = slice 8 $ fillOneUntil n

-- リストが8要素になるまで左をゼロで満たす
zfill :: [Int] -> [Int]
zfill ls
    | length ls < 8 = zfill $ 0:ls
    | otherwise = ls

-- 2進数のリストを足し算
sumBinList :: [Int] -> [Int] -> [Int]
sumBinList a b = zipWith sumBin a b

--数値aもbも1なら1
--それ以外は0となる足し算
sumBin :: Int -> Int -> Int
sumBin a b
    | a == 1 && b == 1 = 1
    | otherwise        = 0 

--
--上のも全部型シノニムでやっとけばよかった。。。
type IPv4    = [Int]
type MaskNum = Int

mask :: IPv4 -> MaskNum -> [Int]
mask a b = map binToDec (map digitsToInt (zipWith sumBinList (ipToBin a) (maskNumToBin b)))

-- mask [192,168,10,44] 29 <--- 192.168.10.44/29の意味
--
-- A 元IPのバイナリー化 : ipToBinが[[Int]]をかえす         [[0,0,0,...],[0,0,0,...],[0,0,0,...],[0,0,0,...]]
-- B マスクIPのバイナリー化 : maskNumToBinも[[Int]]をかえす [[0,0,0,...],[0,0,0,...],[0,0,0,...],[0,0,0,...]]
-- AとBを１桁ずつ上下で足し算する
-- 1+1の縦列だけ1になって新しいリストになる
-- リストを数値に変換
-- 数値を10進数に変換
-- 結果[192,168,10,40]のような[Int]ができる

reverseBits :: MaskNum -> [[Int]]
reverseBits n = map (map rev) (maskNumToBin n)

--lambdaでかければよかった
rev :: Int -> Int
rev 0 = 1
rev 1 = 0

--マスクのビットを反転->[[Int]]を[Int]に->文字列として連結->Intにもう一回パース->10進数に変換
--wtf??? 汚すぎる
--最終的にビットのあまり部分?を10進数で得られる
rangeEnd :: MaskNum -> Int
rangeEnd a = binToDec $ pInt $ concat $ map show $ (map digitsToInt (reverseBits a))

-- ネットワークアドレスはmask関数で入手そこがIP下限
-- 下限のIP[Int]はwhereでstartとして持っておく
-- 下限のIPのの一番最後の番号に範囲上限までの数値を足し算したIPを[Int]でかえす
endIp :: IPv4 -> MaskNum -> [Int]
endIp a b = [start !! 0, start !! 1, start !! 2, start !! 3 + rangeEnd b]
    where start = mask a b

--parse Int むりやり
pInt :: [Char] -> Int
pInt c = read c :: Int

--192.168.1.1/21 ===> ([192,168,1,1], 21)
cidrParse :: String -> ([Int], Int)
cidrParse s = (ip, mask)
    where splitted = split '/' s
          ip       = map pInt $ split '.' $ head $ splitted
          mask     = pInt $ splitted !! 1

-- split :: Eq a => a -> [a] -> [[a]]
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

ipShow :: [Int] -> String
ipShow ls = init $ concatMap (++".") (map (\x -> show x) ls)

----------------------------------------------------
main = do
    putStrLn "Enter IP to mask"
    input <- getLine
    let ip      = fst $ cidrParse input --[Int]
    let maskNum = snd $ cidrParse input --Int
    let start = mask  ip maskNum
    let end   = endIp ip maskNum
    putStrLn $ "network address:" ++  (ipShow $ start)
    putStrLn $ "range:" ++ (ipShow $ start) ++ " TO " ++ (ipShow $ end)

