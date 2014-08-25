-- file: Life.hs
-- based on https://speakerdeck.com/dmoverton/comonads-in-haskell

import Control.Applicative ((<$>), (<*>))
import Control.Comonad     (Comonad, extract, duplicate, extend)
import Data.Foldable       (Foldable, foldMap)
import Data.List           (unfoldr, intercalate)
import Data.Maybe          (catMaybes)
import Data.Monoid         ((<>))
import Data.Traversable    (Traversable, traverse, sequenceA)
import System.Environment  (getArgs)
import System.Random       (RandomGen, getStdGen, randoms)

import Codec.Picture       (Image, Pixel, GifLooping(LoopingNever))
import Codec.Picture       (generateFoldImage, writeGifImages)
import Codec.Picture.Gif   (greyPalette)

-- one-dimensional zipper
data Z a = Z [a] a [a]
-- two-dimensional zipper
newtype Z2 a = Z2 (Z (Z a))
type Board = Z2 Bool

-- shift one-dimensional focus back/forth
back, forth :: Z a -> Maybe (Z a)
back  (Z (l:ls) c rs) = Just $ Z ls l (c:rs)
back  _               = Nothing
forth (Z ls c (r:rs)) = Just $ Z (c:ls) r rs
forth _               = Nothing

-- shift two-dimensiona focus left/right/up/down
left, right, up, down :: Z2 a -> Maybe (Z2 a)
left  (Z2 z) = Z2 <$> back  z
right (Z2 z) = Z2 <$> forth z
up    (Z2 z) = (Z2 <$>) . sequenceA $ fmap back  z
down  (Z2 z) = (Z2 <$>) . sequenceA $ fmap forth z

toZ :: [a] -> Z a
toZ (c:rs) = Z [] c rs
toZ _      = undefined

fromZ :: Z a -> [a]
fromZ (Z ls c rs) = (reverse ls) ++ c : rs

instance Functor Z where
    fmap f (Z ls c rs) = Z (map f ls) (f c) (map f rs)

instance Comonad Z where
    extract (Z _ c _) = c
    duplicate z = Z (unfoldr ((split <$>) . back ) z)
                    z
                    (unfoldr ((split <$>) . forth) z)

split :: a -> (a,a)
split a = (a,a)

instance Foldable Z where
    foldMap f (Z ls c rs) = foldMap f (reverse ls) <> f c <> foldMap f rs

instance Traversable Z where
    traverse f (Z ls c rs) = Z <$> traverse f ls <*> f c <*> traverse f rs

instance Functor Z2 where
    fmap f (Z2 z) = Z2 $ fmap (fmap f) z

instance Comonad Z2 where
    extract (Z2 z) = extract $ extract z
    duplicate (Z2 z) = fmap Z2 $ Z2 $ roll $ roll z where
        roll a = Z (unfoldr ((split <$>) . traverse back) a)
                   a
                   (unfoldr ((split <$>) . traverse forth) a)

instance Foldable Z2 where
    foldMap f (Z2 z) = foldMap (foldMap f) z

instance Traversable Z2 where
    traverse f (Z2 z) = Z2 <$> traverse (traverse f) z

main :: IO ()
main = do
    (x:y:gens:file:[]) <- getArgs
    board <- mkBoard (read x, read y) <$> getStdGen
    let generations = take (read gens) $ iterate (extend live) board
    let images = map (boardToImage (read x,read y)) generations
    let triples = zip3 (repeat greyPalette) (repeat 10) images
    let result = writeGifImages file LoopingNever triples
    case result of
         Right r -> r >> putStrLn "Success"
         Left  s -> putStrLn s

mkBoard :: RandomGen g => (Int,Int) -> g -> Board
mkBoard (x,y) = Z2 . toZ . take x
              . unfoldr ((\(a,b) -> return (toZ a,b)) . splitAt y)
              . randoms

showBoard :: Board -> String
showBoard (Z2 z) = intercalate "\n"
                 $ map (intercalate " " . map (show . btoi) . fromZ)
                 $ fromZ z

boardToImage :: (Bounded p, Pixel p) => (Int,Int) -> Board -> Image p
boardToImage (x,y) (Z2 z) = snd
                          $ generateFoldImage (\l _ _ -> (tail l, btop (head l)))
                                              (concat listBoard)
                                              x
                                              y
    where listBoard = map fromZ $ fromZ z

btop :: (Bounded p, Pixel p) => Bool -> p
btop True  = maxBound -- white
btop False = minBound -- black

btoi :: Bool -> Int
btoi False = 0
btoi True  = 1

live :: Board -> Bool
live b = case (extract b, sn < 2, sn < 4, sn == 3) of
              (True,  True,  _,     _   ) -> False
              (True,  False, True,  _   ) -> True
              (False, _,     _,     True) -> True
              _                           -> False
  where sn = sum $ catMaybes $ map get [ right b
                                       , left  b
                                       , up    b
                                       , down  b
                                       , right b >>= up
                                       , right b >>= down
                                       , left  b >>= up
                                       , left  b >>= down
                                       ]
        get = (btoi . extract <$>)
