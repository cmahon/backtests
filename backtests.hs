{-# LANGUAGE OverloadedStrings,FlexibleContexts,TypeFamilies #-}

import           Control.Applicative
import           Control.Arrow                 ((&&&))
import           Control.Lens                  hiding ((|>))
import           Control.Monad                 (liftM,void)
import           Control.Monad.Primitive.Class
import           Data.Csv                      (HasHeader(..))
import           Data.List                     (mapAccumL)
import           Data.Ratio     
import           Data.Time     
import qualified Data.Vector                   as V (Vector)
import qualified Data.Vector.Generic           as V hiding (Vector)
import           Graphics.EasyPlot
import           Numeric.LinearAlgebra  
import           Numeric.LinearAlgebra.Util
import           Pipes
import qualified Pipes.ByteString              as PB
import           Pipes.Csv                     (decode)
import           Pipes.Safe                    (SafeT, runSafeT)
import qualified Pipes.Safe.Prelude            as PS
import qualified Pipes.Vector                  as PV
import           System.IO                     (IOMode (ReadMode))

-----------------------------------------------------------------------------

main :: IO ()
main = do
  runLRStrategy "Cumulative sum of " absReturn cmlAbsReturn 0
  runLRStrategy "Cumulative product of " pctReturn cmlPctReturn 0
  runKFStrategy "Cumulative sum of " absReturn cmlAbsReturn 0

-----------------------------------------------------------------------------

-- Linear regression strategy inspired by http://businessforecastblog.com/predicting-the-sp-500-or-the-spy-exchange-traded-fund/

runLRStrategy :: String -> Return -> Return -> Matrix Double -> IO ()
runLRStrategy titleprefix freturn fcmlreturn minret = do
  (mx,my,vdtd) <- prepareData 30 freturn
  let
    t = 2484 -- number of training data points
    mm = takeRows t mx <\> takeRows t my
    my' = mx <> mm
    mr = step (my' - minret) * my
    vy = head $ toColumns my
    vr = head $ toColumns mr  
    vcy = fcmlreturn vy
    vcr = fcmlreturn vr  
    lcy = V.toList vcy
    lcr = V.toList vcr
    ldt = map (fromRational . dayToYear . readDay) $ V.toList vdtd 
  void $ plot' [] X11 [
    Data2D [Title $ titleprefix ++ "in sample strategy returns", Style Lines] [] $ take t $ zip ldt lcr, 
    Data2D [Title $ titleprefix ++ "out of sample strategy returns", Style Lines] [] $ drop t $ zip ldt lcr,
    Data2D [Title $ titleprefix ++ "base returns", Style Lines] [] $ zip ldt lcy    
    ]

-----------------------------------------------------------------------------

-- Kalman filter strategy

runKFStrategy :: String -> Return -> Return -> Matrix Double -> IO ()
runKFStrategy titleprefix freturn fcmlreturn minret = do
  (mx,my,vdtd) <- prepareData 30 freturn
  let
    f = diag $ 61 |> repeat 1 :: Matrix Double
    q = diag $ 61 |> repeat 0.2 :: Matrix Double
    r = (1><1) [0.1] :: Matrix Double
    x0 = 61 |> repeat 0
    p0 = q
    s0 = State x0 p0    
    (s',y') = mapAccumL (\st (xt,yt) -> (kalman (System f (fromRows [xt]) q r) st yt,(xt * (sX st)) @> 1)) s0 $ zip (toRows mx) (toRows my)
    my' = fromColumns [fromList y'] 
    mr = (step (my' - minret)) * my
    vy = head $ toColumns my
    vr = head $ toColumns mr  
    vcy = fcmlreturn vy
    vcr = fcmlreturn vr  
    lcy = V.toList vcy
    lcr = V.toList vcr
    ldt = map (fromRational . dayToYear . readDay) $ V.toList vdtd 
  print $ sX s'
  void $ plot' [] X11 [
    Data2D [Title $ titleprefix ++ "strategy returns", Style Lines] [] $ zip ldt lcr, 
    Data2D [Title $ titleprefix ++ "base returns", Style Lines] [] $ zip ldt lcy    
    ]

-----------------------------------------------------------------------------

-- Kalman code from hmatrix examples

data System = System {kF, kH, kQ, kR :: Matrix Double}
data State = State {sX :: Vector Double , sP :: Matrix Double} deriving Show
type Measurement = Vector Double

kalman :: System -> State -> Measurement -> State
kalman (System f h q r) (State x p) z = State x' p' where
    px = f <> x                            -- prediction
    pq = f <> p <> trans f + q             -- its covariance
    y  = z - h <> px                       -- residue
    cy = h <> pq <> trans h + r            -- its covariance
    k  = pq <> trans h <> inv cy           -- kalman gain
    x' = px + k <> y                       -- new state
    p' = (ident (dim x) - k <> h) <> pq    -- its covariance

-----------------------------------------------------------------------------

--TODO: streamline inefficient conversion between lists and vectors

prepareData :: Int -> Return -> IO (Matrix Double,Matrix Double,V.Vector String)
prepareData g freturn = do
  vsp' <- V.map ((^._1) &&& (^._7)) `liftM` readCSV "gspc.csv" 
  vvx' <- V.map ((^._1) &&& (^._7)) `liftM` readCSV "vix.csv"
  let
    vm' = V.fromList $ mergeEqual (V.toList vsp') (V.toList vvx') :: V.Vector (String,Double,Double)  
    vdtd = V.map (^._1) (V.drop g vm') :: V.Vector String
    [vsp'',vvx''] = [V.map] <*> [(^._2),(^._3)] <*> [vm']
    [vsp,vvx] = map V.convert [vsp'',vvx''] :: [Vector Double]
    [vspd,vvxd] = map freturn [vsp,vvx] 
    l = V.length vdtd - 1
    [mspds,mvxds] = map (\v -> fromColumns [subVector (g-n) l v | n <- [0..g]]) [vspd,vvxd]
    mx = (konst 1 (l,1)) ! (dropColumns 1 mspds) ! (dropColumns 1 mvxds)
    my = takeColumns 1 mspds 
  return (mx,my,vdtd)

-----------------------------------------------------------------------------

type PriceData = (String,Double,Double,Double,Double,Double,Double)

type ParsedPriceData = Either String PriceData 

readCSV :: FilePath -> IO (V.Vector PriceData)
readCSV f = runSafeT $ runEffect $ PV.runToVectorP $
    (hoist lift (decode HasHeader (PS.withFile f ReadMode (PB.fromHandle)))) 
    >-> rightP
    >-> PV.toVector 

-----------------------------------------------------------------------------

mergeEqual :: Ord a => [(a,b)] -> [(a,b)] -> [(a,b,b)]
mergeEqual x [] = []
mergeEqual [] y = []
mergeEqual x@((xi,xv):xs) y@((yi,yv):ys) 
  | xi == yi = (xi,xv,yv) : mergeEqual xs ys
  | xi < yi = mergeEqual xs y
  | xi > yi = mergeEqual x ys

-----------------------------------------------------------------------------

type Return = Vector Double -> Vector Double

absReturn :: Return
absReturn v = V.zipWith (-) (V.tail v) (V.init v)

pctReturn :: Return
pctReturn v = (V.zipWith (-) (V.tail v) (V.init v))/(V.init v)

cmlAbsReturn :: Return
cmlAbsReturn = V.scanl (+) 0

cmlPctReturn :: Return
cmlPctReturn = V.scanl (\x y -> x * (1+y)) 1

-----------------------------------------------------------------------------

readDay :: String -> Day
readDay = read

dayToYear :: Day -> Ratio Integer
dayToYear day = 
  let
    (y,_,_) = toGregorian day
    ystart = fromGregorian y 1 1
    yend = fromGregorian y 12 31
    dd = diffDays day ystart
    dy = diffDays yend ystart
  in 
    y % 1 + dd % dy

-----------------------------------------------------------------------------

rightP :: (Monad m) => Pipe (Either a b) b m r
rightP = for cat $ either (const $ return ()) yield

-----------------------------------------------------------------------------

instance MonadPrim m => MonadPrim (SafeT m) where
  type BasePrimMonad (SafeT m) = BasePrimMonad m
  liftPrim = lift . liftPrim



