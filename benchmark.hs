import Criterion
import Criterion.Main

import MonadList

main = do
    defaultMain
        [ bgroup ("10^" ++ show n) [ bench name $ nfIO (f nn) | (name,f) <- variants]
        | n <- [0..6]
        , let nn = 10^n
        ]
