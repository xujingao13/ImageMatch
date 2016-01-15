module Main where
import qualified System.IO
import qualified Data.ByteString.Char8 as L8
import qualified Data.ByteString as L
import qualified Data.Array.IArray as Arr
import Control.Parallel.Strategies as CP
import Data.Matrix as Ma
import Data.List as Li
import Data.Word
import Data.Char
import Data.Complex


data Bitmap = Bitmap {
      pic_type :: String
    , pic_size :: Int
    , width :: Int
    , height :: Int
    , greyData :: RGB_bit
    } deriving (Eq, Show)


data RGB_bit = RGB_bit {
    r :: Matrix Integer
    , g :: Matrix Integer
    , b :: Matrix Integer
    , grey :: [Double]
}deriving (Eq, Show)


type Value_type = Integer

main = do
          putStrLn "Hello World"

read_bmp :: String -> String -> IO()
read_bmp in_path out_path= do
                            big_file <- L.readFile in_path
                            L.writeFile out_path big_file
                            putStrLn "OK"


readWord :: L.ByteString -> Maybe (Int, L.ByteString)
readWord xs | (L8.length xs) >= 4 = Just ((ord $ L8.head xs) + (ord $ L8.head (L8.tail xs)) * 256 + (ord $ L8.head (L8.tail $ L8.tail xs)) * 256 * 256 + (ord $ L8.head (L8.tail (L8.tail $ L8.tail xs))) * 256 * 256 * 256, L8.tail (L8.tail (L8.tail $ L8.tail xs)))
            | otherwise = Nothing

readByte :: L.ByteString -> Maybe (Int, L.ByteString)
readByte xs | (L8.length xs) >= 1 = Just (ord $ L8.head xs, L8.tail xs)
            | otherwise = Nothing

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = clear_head str 2
    | otherwise
        = Nothing


getFstInt :: L.ByteString -> Maybe (Int, L.ByteString)
getFstInt s = case readWord s of
             Nothing -> Nothing
             Just (num, rest)
                 | num < 0    -> Nothing
                 | otherwise -> Just (num, rest)


clear_head :: L.ByteString -> Int -> Maybe L.ByteString
clear_head xs 0 = Just xs
clear_head xs n = case readByte xs of
                  Nothing -> Nothing
                  Just (num, rest)
                      | num < 0  -> Nothing
                      | otherwise -> clear_head (L8.dropWhile isSpace rest) (n-1)

get_dib :: L.ByteString -> Int -> (L.ByteString, L.ByteString)
get_dib xs n = (L.take n xs, L.drop n xs)


mapm :: (Num b,Enum b, Num a) => (b -> a) -> [Integer] -> [a]
mapm fun [] = []
mapm fun (x:xs) = (fun (fromInteger x)):(mapm fun xs)

get_rgb_matrix1 :: [Word8] -> Int -> Int -> Int -> (([Integer], [Integer]), [Integer])
get_rgb_matrix1 xs n align_com line_num = get_rgb_matrix_aux1 [] [] [] n xs align_com line_num

get_rgb_matrix :: [Word8] -> Int -> Int -> Int -> (([Integer], [Integer]), [Integer])
get_rgb_matrix xs n align_com line_num = get_rgb_matrix_aux [] [] [] n xs align_com line_num

toRGBMatrix :: L.ByteString -> Int -> (Int, Int) -> RGB_bit
toRGBMatrix xs n (width, height) = RGB_bit (fromList height width (fst (fst rgb))) (fromList height width (snd (fst rgb))) (fromList height width (snd rgb)) (zipWith (+) (mapm ((*) 0.299) (fst (fst rgb))) (zipWith (+) (mapm ((*) 0.587) (snd (fst rgb))) (mapm ((*) 0.114) (snd rgb)) ))
    where rgb = get_rgb_matrix (L.unpack xs) 0 (if (4 - ((width * 3) `mod` 4)) == 4 then 0 else (4 - ((width * 3) `mod` 4))) (width * 3)
          rgb1 = get_rgb_matrix1 (L.unpack xs) 0 (if (4 - ((width * 3) `mod` 4)) == 4 then 0 else (4 - ((width * 3) `mod` 4))) (width * 3)


parseP5 :: L.ByteString -> Maybe Bitmap
parseP5 s =
    case matchHeader (L8.pack "BM") s of
        Nothing -> Nothing
        Just s1 ->
            case getFstInt s1 of
                Nothing -> Nothing
                Just (pic_size, s2) ->
                    case clear_head (L8.dropWhile isSpace s2) 4 of
                        Nothing -> Nothing
                        Just s3 ->
                            case getFstInt (L8.dropWhile isSpace s3) of
                                Nothing -> Nothing
                                Just (offset, s4) ->
                                    case (get_dib s4 (offset - 14)) of
                                        (dib_header, s5) ->
                                            case clear_head (L8.dropWhile isSpace dib_header) 4 of
                                                Nothing -> Nothing
                                                Just s6 ->
                                                    case getFstInt s6 of
                                                        Nothing -> Nothing
                                                        Just (width, s7) ->
                                                            case getFstInt s7 of
                                                                Nothing -> Nothing
                                                                Just (height, s7) ->Just (Bitmap "BM" (pic_size - offset) width height (toRGBMatrix s5 (pic_size - offset) (width, height)))