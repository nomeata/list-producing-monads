import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.List
import Text.Pandoc.Definition
import Text.Pandoc
import Text.Printf
import Data.Functor

main = do
    input <- BS.getContents
    let Right rows = decode HasHeader input
    let rows' = [ (n,s,v)
            | (x,v,(),(),(),(),()) <- V.toList rows
            , let (s,'/':n) = span (/= '/') x
            ] :: [(String, String, Double)]

    let (columns, rows, entries) = listToTable rows' 0
    let trim = take (1 + length columns)

    let table =  Table []
                (trim $ AlignLeft : repeat AlignRight)
                (trim $ repeat 0)
                (map str2TableCell ("Variant" : columns))
                (zipWith (\r e -> map str2TableCell (r : map sTime e)) rows entries)

    Pandoc meta blocks <- readMarkdown def <$> readFile "README.in"
    putStr $
        writeMarkdown (def { writerExtensions = githubMarkdownExtensions }) $
        Pandoc meta (blocks ++ [table])

sTime :: Double -> String
sTime n | n < 10**(-5) = printf "%.0fns" (n * 1000000000)
        | n < 10**(-2) = printf "%.0fµs" (n * 1000000)
        | n < 10**1 = printf "%.0fms" (n * 1000)
        | otherwise = printf "%.0fs" n

str2TableCell s = [Plain [Str s]]

-- returns column header, row headers, rows
listToTable :: (Eq a, Eq b) => [(a,b,c)] -> c -> ([b],[a],[[c]])
listToTable entries def
    = (columns, rows, body)
  where
    columns = nub [ x | (_,x,_) <- entries ]
    rows    = nub [ x | (x,_,_) <- entries ]
    body =
        [ [ case find (\(a,b,_) -> a == r && b == c) entries  of
             Just (_,_,v) -> v
             Nothing      -> def
          | c <- columns
          ]
        | r <- rows ]
