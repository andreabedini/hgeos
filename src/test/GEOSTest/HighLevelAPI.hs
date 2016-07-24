module GEOSTest.HighLevelAPI (demo) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Geolocation.GEOS
import Data.Maybe

-- Demonstrates use of high-level API
-- Lifetimes of GEOS objects are automatically managed by the context objects
-- which guarantees that they are released when the context goes out of scope
demo :: IO ()
demo = do
    result <- withGEOS $ \ctx -> runMaybeT $ do
        reader <- MaybeT $ mkReader ctx
        g0 <- MaybeT $ readGeometry reader "POLYGON (( 10 10, 10 20, 20 20, 20 10, 10 10 ))"
        g1 <- MaybeT $ readGeometry reader "POLYGON (( 11 11, 11 12, 12 12, 12 11, 11 11 ))"
        g2 <- MaybeT $ intersection g0 g1
        writer <- MaybeT $ mkWriter ctx
        str <- MaybeT $ writeGeometry writer g2
        lift $ putStrLn str
    putStrLn $ "HighLevelAPI.demo: " ++ (if isJust result then "succeeded" else "failed")
