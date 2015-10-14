import Data.Csv hiding ((.=))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.List
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main = do
    input <- BS.getContents
    let Right rows = decode HasHeader input
    let rows' = [ (n,s,v)
            | (x,v,(),(),(),(),()) <- V.toList rows
            , let (s,'/':n) = span (/= '/') x
            , not $ "accum" `isPrefixOf` n || n == "listFix"
            ] :: [(String, String, Double)]

    let (columns, rows, entries) = listToTable rows'
    let trim = take (1 + length columns)

    toFile (def & fo_size .~ (690, 500) & fo_format .~ SVG) "graphs.svg" $ do
        layout_title .= "Constructing a list in a monad"
        layout_y_axis . laxis_title .= "µs/entry"
        layout_x_axis . laxis_title .= "input list length"
        layout_x_axis . laxis_generate .= autoIndexAxis columns
        sequence_ [
            plot $ line r [zipWith (\i v -> (PlotIndex i, v*10**(9 - fromIntegral i))) [0..] e]
            | (r,e) <- zip rows entries
            ]

-- returns column header, row headers, rows
listToTable :: (Eq a, Eq b) => [(a,b,c)] -> ([b],[a],[[c]])
listToTable entries
    = (columns, rows, body)
  where
    columns = nub [ x | (_,x,_) <- entries ]
    rows    = nub [ x | (x,_,_) <- entries ]
    body =
        [ [ v
          | c <- columns
          , let Just (_,_,v) = find (\(a,b,_) -> a == r && b == c) entries
          ]
        | r <- rows ]
