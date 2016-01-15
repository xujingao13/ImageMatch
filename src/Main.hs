module Main where

import Data.List
import Data.Array
import System.Environment (getArgs)
import Control.Parallel
import qualified Data.ByteString.Lazy as L1
import qualified Data.ByteString.Lazy as L2

main :: IO()
main = do
    --get args
    args <- getArgs

    --get original bmp
    originBmp <- L1.readFile $ head args
    let data_origin = map fromEnum (L1.unpack (L1.drop 54 originBmp))
    let info_origin = listArray (0, 53) (map fromEnum (L1.unpack (L1.take 54 originBmp))) :: Array Int Int
    let origin = getBmpInfo data_origin info_origin

    --get search bmp
    searchBmp <- L2.readFile $ head(tail args)
    let data_search = map fromEnum (L2.unpack (L2.drop 54 searchBmp))
    let info_search = listArray (0,53) (map fromEnum (L2.unpack (L2.take 54 searchBmp))) :: Array Int Int
    let search = getBmpInfo data_search info_search

    case last args of
      "0" -> putStrLn $ getAnswer (img_match0 (fst origin) (snd origin) (fst search) (snd search) (0,0))
      "1" -> putStrLn $ getAnswer (handlemode1 (fst origin) (snd origin) (fst search) (snd search))
      "2" -> print 2
      "3" -> putStrLn $ getAnswer (handlemode3 (fst origin) (snd origin) (fst search) (snd search))
      "4" -> putStrLn $ getAnswer (img_match4 (fst origin) (snd origin) (fst search) (snd search) (0,0))

-- 元组打印
getAnswer :: (Int, Int) -> String
getAnswer (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- 读入内容数据 读入图片信息 -> （原始图片二维数组，（图片宽度，图片高度））
getBmpInfo :: [Int] -> Array Int Int -> (Array Int (Array Int Int), (Int, Int))
getBmpInfo datas info  = (bitMap, widthheight)
                          where widthheight = getWidthHeight info
                                bytesPerLine = getBytesPerLine (fst widthheight)
                                bitMap = getBmpMap (snd widthheight) (getLineBytes bytesPerLine (snd widthheight) datas [])

getPointRGB :: Array Int (Array Int Int) -> Int -> Int -> (Int, Int, Int)
getPointRGB bitmap x y = ((bitmap ! y) ! (x * 3 + 2), (bitmap ! y) ! (x * 3 + 1), (bitmap ! y) ! (x * 3))

getPointGrey :: Array Int (Array Int Int) -> Int -> Int -> Int
getPointGrey bitmap x y = (((bitmap ! y) ! (x * 3 + 2))*299 + ((bitmap ! y) ! (x * 3 + 1))*587 + ((bitmap ! y) ! (x * 3))*114 + 500) `div` 1000

getWidthHeight :: Array Int Int -> (Int, Int)
getWidthHeight x = ((x!18) + (x!19)*256 + (x!20)*256*256 + (x!21)*256*256*256, (x!22) + (x!23)*256 + (x!24)*256*256 + (x!25)*256*256*256)

-- 宽度 高度 原始读入数据 中间结果 -> 最终结果
getLineBytes :: Int -> Int -> [Int] -> [Array Int Int] -> [Array Int Int]
getLineBytes bytesPerLine biHeight datas result = if biHeight == 0
                                                  then result
                                                  else getLineBytes bytesPerLine (biHeight - 1) (drop bytesPerLine datas) (line : result)
                                                        where line = listArray (0, bytesPerLine - 1) (take bytesPerLine datas) :: Array Int Int

getBmpMap :: Int -> [Array Int Int] -> Array Int (Array Int Int)
getBmpMap biHeight datas = listArray (0, biHeight - 1) datas :: Array Int (Array Int Int)

getMedian :: [Int] -> Int -> Int
getMedian ns n
          | n `mod` 2 == 0 = ((last (take (n `div` 2 + 1) (sort ns))) + (last (init (take (n `div` 2 + 1) (sort ns))))) `div` 2
          | otherwise = last $ take ((n+1) `div` 2) (sort ns)
-- 原始灰度数组 x y -> 中值灰度
getMiddlePointGrey :: Array Int (Array Int Int) -> Int -> Int -> Int -> Int -> Int
getMiddlePointGrey bitmap width height x y
                      | x == 0 && y == 0 = getMedian ((getPointGrey bitmap 0 0):(getPointGrey bitmap 0 1):(getPointGrey bitmap 1 1):(getPointGrey bitmap 1 0):[]) 4
                      | x == width-1 && y == 0 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap (x-1) y):(getPointGrey bitmap (x-1) (y+1)):(getPointGrey bitmap x (y+1)):[]) 4
                      | x == 0 && y == height-1 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap x (y-1)):(getPointGrey bitmap (x+1) (y-1)):(getPointGrey bitmap (x+1) y):[]) 4
                      | x == width-1 && y == height-1 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap x (y-1)):(getPointGrey bitmap (x-1) y):(getPointGrey bitmap (x-1) (y-1)):[]) 4
                      | y == 0 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap (x-1) y):(getPointGrey bitmap (x+1) y):(getPointGrey bitmap x (y+1)):(getPointGrey bitmap (x-1) (y+1)):(getPointGrey bitmap (x+1) (y+1)):[]) 6
                      | y == height-1 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap (x-1) y):(getPointGrey bitmap (x+1) y):(getPointGrey bitmap x (y-1)):(getPointGrey bitmap (x-1) (y-1)):(getPointGrey bitmap (x+1) (y-1)):[]) 6
                      | x == 0 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap x (y+1)):(getPointGrey bitmap x (y-1)):(getPointGrey bitmap (x+1) (y+1)):(getPointGrey bitmap (x+1) (y-1)):(getPointGrey bitmap (x+1) y):[]) 6
                      | x == width-1 = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap x (y+1)):(getPointGrey bitmap x (y-1)):(getPointGrey bitmap (x-1) (y+1)):(getPointGrey bitmap (x-1) (y-1)):(getPointGrey bitmap (x-1) y):[]) 6
                      | otherwise = getMedian ((getPointGrey bitmap x y):(getPointGrey bitmap (x-1) y):(getPointGrey bitmap (x+1) y):(getPointGrey bitmap x (y+1)):(getPointGrey bitmap (x-1) (y+1)):(getPointGrey bitmap (x+1) (y+1)):(getPointGrey bitmap x (y-1)):(getPointGrey bitmap (x-1) (y-1)):(getPointGrey bitmap (x+1) (y-1)):[]) 9

getAveragePointGrey :: Array Int (Array Int Int) -> Int -> Int -> Int -> Int -> Int
getAveragePointGrey bitmap width height x y
                      | x == 0 && y == 0 = ((getPointGrey bitmap 0 0)+(getPointGrey bitmap 0 1)+(getPointGrey bitmap 1 1)+(getPointGrey bitmap 1 0)) `div` 4
                      | x == width-1 && y == 0 = ((getPointGrey bitmap x y)+(getPointGrey bitmap (x-1) y)+(getPointGrey bitmap (x-1) (y+1))+(getPointGrey bitmap x (y+1))) `div` 4
                      | x == 0 && y == height-1 = ((getPointGrey bitmap x y)+(getPointGrey bitmap x (y-1))+(getPointGrey bitmap (x+1) (y-1))+(getPointGrey bitmap (x+1) y)) `div` 4
                      | x == width-1 && y == height-1 = ((getPointGrey bitmap x y)+(getPointGrey bitmap x (y-1))+(getPointGrey bitmap (x-1) y)+(getPointGrey bitmap (x-1) (y-1))) `div` 4
                      | y == 0 = ((getPointGrey bitmap x y)+(getPointGrey bitmap (x-1) y)+(getPointGrey bitmap (x+1) y)+(getPointGrey bitmap x (y+1))+(getPointGrey bitmap (x-1) (y+1))+(getPointGrey bitmap (x+1) (y+1))) `div` 6
                      | y == height-1 = ((getPointGrey bitmap x y)+(getPointGrey bitmap (x-1) y)+(getPointGrey bitmap (x+1) y)+(getPointGrey bitmap x (y-1))+(getPointGrey bitmap (x-1) (y-1))+(getPointGrey bitmap (x+1) (y-1))) `div` 6
                      | x == 0 = ((getPointGrey bitmap x y)+(getPointGrey bitmap x (y+1))+(getPointGrey bitmap x (y-1))+(getPointGrey bitmap (x+1) (y+1))+(getPointGrey bitmap (x+1) (y-1))+(getPointGrey bitmap (x+1) y)) `div` 6
                      | x == width-1 = ((getPointGrey bitmap x y)+(getPointGrey bitmap x (y+1))+(getPointGrey bitmap x (y-1))+(getPointGrey bitmap (x-1) (y+1))+(getPointGrey bitmap (x-1) (y-1))+(getPointGrey bitmap (x-1) y)) `div` 6
                      | otherwise = ((getPointGrey bitmap x y)+(getPointGrey bitmap (x-1) y)+(getPointGrey bitmap (x+1) y)+(getPointGrey bitmap x (y+1))+(getPointGrey bitmap (x-1) (y+1))+(getPointGrey bitmap (x+1) (y+1))+(getPointGrey bitmap x (y-1))+(getPointGrey bitmap (x-1) (y-1))+(getPointGrey bitmap (x+1) (y-1))) `div` 9

-- 原始灰度数组 宽 高 x y 中间列表 中间数组 -> 中值灰度数组
getMiddleGreyBmpMap :: Array Int (Array Int Int) -> Int -> Int -> Int -> Int -> [Int] -> [Array Int Int] -> Array Int (Array Int Int)
getMiddleGreyBmpMap bitmap width height x y ns tempArray
                      | x == 0 && y == height = listArray (0, height-1) tempArray :: Array Int (Array Int Int)
                      | x == width = getMiddleGreyBmpMap bitmap width height 0 (y+1) [] (tempArray ++ [listArray (0, width - 1) ns :: Array Int Int])
                      | otherwise = getMiddleGreyBmpMap bitmap width height (x+1) y (ns ++ [getMiddlePointGrey bitmap width height x y]) tempArray

-- 原始灰度数组 宽 高 x y 中间列表 中间数组 -> 均值灰度数组
getAverageGreyBmpMap :: Array Int (Array Int Int) -> Int -> Int -> Int -> Int -> [Int] -> [Array Int Int] -> Array Int (Array Int Int)
getAverageGreyBmpMap bitmap width height x y ns tempArray
                      | x == 0 && y == height = listArray (0, height-1) tempArray :: Array Int (Array Int Int)
                      | x == width = getAverageGreyBmpMap bitmap width height 0 (y+1) [] (tempArray ++ [listArray (0, width - 1) ns :: Array Int Int])
                      | otherwise = getAverageGreyBmpMap bitmap width height (x+1) y (ns ++ [getAveragePointGrey bitmap width height x y]) tempArray

-- 原始数组 宽 高 x y 中间列表 中间数组 -> 灰度数组
getGreyBmpMap :: Array Int (Array Int Int) -> Int -> Int -> Int -> Int -> [Int] -> [Array Int Int] -> Array Int (Array Int Int)
getGreyBmpMap bitmap width height x y ns tempArray
                      | x == 0 && y == height = listArray (0, height-1) tempArray :: Array Int (Array Int Int)
                      | x == width = getGreyBmpMap bitmap width height 0 (y+1) [] (tempArray ++ [listArray (0, width - 1) ns :: Array Int Int])
                      | otherwise = getGreyBmpMap bitmap width height (x+1) y (ns ++ [getPointGrey bitmap x y]) tempArray

getBytesPerLine :: Int -> Int
getBytesPerLine width = if width `mod` 4 == 0
                      then width * 3
                      else width * 3 + 4 - (width * 3) `mod` 4

mode0 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Bool
mode0 bitMap widthheight serbitMap serwidthheight (x, y) (ox, oy) count = if (fst widthheight) - (ox+1) < (fst serwidthheight) || (snd widthheight) - (oy+1) < (snd serwidthheight)
                                                               then False
                                                               else if count == minimum [fst serwidthheight, snd serwidthheight]
                                                                    then True
                                                                    else if (getPointRGB bitMap (ox+count) (oy+count)) == (getPointRGB serbitMap x y)
                                                                         then mode0 bitMap widthheight serbitMap serwidthheight (x + 1, y + 1) (ox, oy) (count + 1)
                                                                         else False

img_match0 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
img_match0 bitMap widthheight serbitMap serwidthheight (x, y) = if mode0 bitMap widthheight serbitMap serwidthheight (0, 0) (x, y) 0
                                                                then (x, y)
                                                                else if x == (fst widthheight) - (fst serwidthheight)
                                                                     then if y == (snd widthheight) - (snd serwidthheight)
                                                                          then (10000,10000)
                                                                          else img_match0 bitMap widthheight serbitMap serwidthheight (0, y + 1)
                                                                     else img_match0 bitMap widthheight serbitMap serwidthheight (x + 1, y)

mode1 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> Bool
mode1 origin_greymap origin_widthheight search_bitmap search_widthheight (sx, sy) (ox, oy) count miss len= if (fst origin_widthheight) - (ox+1) < (fst search_widthheight) || (snd origin_widthheight) - (oy+1) < (snd search_widthheight) || miss > len `div` 20
                                                                then False
                                                                else if count == len
                                                                     then True
                                                                     else if abs ((origin_greymap ! (oy+count) ! (ox+count)) - (getPointGrey search_bitmap sx sy)) < 15
                                                                          then mode1 origin_greymap origin_widthheight search_bitmap search_widthheight (sx + 1, sy + 1) (ox, oy) (count + 1) miss len
                                                                          else mode1 origin_greymap origin_widthheight search_bitmap search_widthheight (sx + 1, sy + 1) (ox, oy) (count + 1) (miss+1) len

img_match1 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
img_match1 origin_greymap origin_widthheight search_bitmap search_widthheight (x,y) = if mode1 origin_greymap origin_widthheight search_bitmap search_widthheight (0,0) (x,y) 0 0 (min (fst search_widthheight) (snd search_widthheight) `div` 2)
                                                                                       then (x,y)
                                                                                       else if x == (fst origin_widthheight) - (fst search_widthheight)
                                                                                            then if y == (snd origin_widthheight) - (snd search_widthheight)
                                                                                                 then (10000,10000)
                                                                                                 else img_match1 origin_greymap origin_widthheight search_bitmap search_widthheight (0,y+1)
                                                                                            else img_match1 origin_greymap origin_widthheight search_bitmap search_widthheight (x+1,y)

handlemode1 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int)
handlemode1 origin_bitmap origin_widthheight search_bitmap search_widthheight = img_match1 origin_greymap origin_widthheight search_bitmap search_widthheight (0,0)
                                                                                  where origin_greymap = getAverageGreyBmpMap origin_bitmap (fst origin_widthheight) (snd origin_widthheight) 0 0 [] []

mode3 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> Bool
mode3 origin_greymap origin_widthheight search_greymap search_widthheight (sx,sy) (ox,oy) count miss len = if (((fst origin_widthheight) - (ox+1) < (fst search_widthheight)) || ((snd origin_widthheight) - (oy+1) < (snd search_widthheight))) || miss > len `div` 20
                                                                                          then False
                                                                                          else if count == len
                                                                                               then True
                                                                                               else if abs ((origin_greymap ! (oy+count) ! (ox+count)) - (search_greymap ! sy ! sx)) < 20
                                                                                                    then mode3 origin_greymap origin_widthheight search_greymap search_widthheight (sx+1,sy+1) (ox,oy) (count+1) miss len
                                                                                                    else mode3 origin_greymap origin_widthheight search_greymap search_widthheight (sx+1,sy+1) (ox,oy) (count+1) (miss+1) len


-- 原图中值灰度数组 原图宽高 搜索图灰度中值数组 搜索图宽高 原图之中开始搜索点坐标 -> 所在位置左上角坐标
img_match3 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
img_match3 origin_greymap origin_widthheight search_greymap search_widthheight (x,y) = if mode3 origin_greymap origin_widthheight search_greymap search_widthheight (0,0) (x,y) 0 0 (min (fst search_widthheight) (snd search_widthheight) `div` 2)
                                                                                       then (x,y)
                                                                                       else if x == (fst origin_widthheight) - (fst search_widthheight)
                                                                                            then if y == (snd origin_widthheight) - (snd search_widthheight)
                                                                                                 then (10000,10000)
                                                                                                 else img_match3 origin_greymap origin_widthheight search_greymap search_widthheight (0,y+1)
                                                                                            else img_match3 origin_greymap origin_widthheight search_greymap search_widthheight (x+1,y)

handlemode3 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int)
handlemode3 origin_bitmap origin_widthheight search_bitmap search_widthheight = img_match3 origin_greymap origin_widthheight search_greymap search_widthheight (0,0)
                                                                                  where origin_greymap = getMiddleGreyBmpMap origin_bitmap (fst origin_widthheight) (snd origin_widthheight) 0 0 [] []
                                                                                        search_greymap = getMiddleGreyBmpMap search_bitmap (fst search_widthheight) (snd search_widthheight) 0 0 [] []

-- 原始数组 原始长宽 搜索数组 搜索长宽 原图搜索点 搜索图搜索点 计数 错误个数 搜索图长宽的最小值 -> 是否匹配上
mode4 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> Bool
mode4 bitMap widthheight serbitMap serwidthheight (x, y) (ox, oy) count miss len= if (fst widthheight) - (ox+1) < (fst serwidthheight) || (snd widthheight) - (oy+1) < (snd serwidthheight) || miss > (len * 20 `div` 100)
                                                               then False
                                                               else if count == len
                                                                    then True
                                                                    else if (getPointRGB bitMap (ox+count) (oy+count)) == (getPointRGB serbitMap x y)
                                                                         then mode4 bitMap widthheight serbitMap serwidthheight (x + 1, y + 1) (ox, oy) (count + 1) miss len
                                                                         else mode4 bitMap widthheight serbitMap serwidthheight (x + 1, y + 1) (ox, oy) (count + 1) (miss + 1) len

img_match4 :: Array Int (Array Int Int) -> (Int, Int) -> Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
img_match4 origin_bitmap origin_widthheight search_bitmap search_widthheight (x,y) = if mode4 origin_bitmap origin_widthheight search_bitmap search_widthheight (0,0) (x,y) 0 0 (min (fst search_widthheight) (snd search_widthheight))
                                                                                     then (x,y)
                                                                                     else if x == (fst origin_widthheight) - (fst search_widthheight)
                                                                                          then if y == (snd origin_widthheight) - (snd search_widthheight)
                                                                                               then (10000,10000)
                                                                                               else img_match4 origin_bitmap origin_widthheight search_bitmap search_widthheight (0,y+1)
                                                                                          else img_match4 origin_bitmap origin_widthheight search_bitmap search_widthheight (x+1,y)
