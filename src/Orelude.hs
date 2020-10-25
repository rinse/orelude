{- HLINT ignore "Use maximum" -}
{- HLINT ignore "Use minimum" -}
module Orelude
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Category
    , module Control.Exception.Safe
    , module Control.Monad
    , module Control.Monad.Cont
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans
    , module Control.Monad.Writer
    , module Data.Either
    , module Data.Functor
    , module Data.Function
    , module Data.Foldable
    , module Data.Maybe
    , module Data.Traversable
    , module Prelude
    -- Safe variants
    , Orelude.foldr1
    , Orelude.foldl1
    , Orelude.head
    , Orelude.last
    , Orelude.tail
    , Orelude.init
    , (Orelude.!!)
    , Orelude.maximum
    , Orelude.maximumBy
    , Orelude.minimum
    , Orelude.minimumBy
    , Orelude.concat
    , Orelude.mconcat
    , Orelude.concatMap
    , Orelude.read
    -- Lifted variants
    , (Orelude.++)
    , Orelude.map
    , Orelude.putChar
    , Orelude.putStr
    , Orelude.putStrLn
    , Orelude.print
    , Orelude.getChar
    , Orelude.getLine
    , Orelude.getContents
    , Orelude.interact
    , Orelude.readFile
    , Orelude.writeFile
    , Orelude.appendFile
    , Orelude.readIO
    , Orelude.mapM_
    , Orelude.forM_
    , Orelude.sequence_
    , Orelude.msum
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Exception.Safe
import           Control.Monad          hiding (forM_, mapM_, msum, sequence_)
import           Control.Monad.Cont     hiding (forM_, mapM_, msum, sequence_)
import           Control.Monad.IO.Class
import           Control.Monad.Reader   hiding (forM_, mapM_, msum, sequence_)
import           Control.Monad.State    hiding (forM_, mapM_, msum, sequence_)
import           Control.Monad.Trans
import           Control.Monad.Writer   hiding (forM_, mapM_, mconcat, msum,
                                         sequence_)
import           Data.Either
import           Data.Foldable          hiding (concat, concatMap, foldl1,
                                         foldr1, forM_, mapM_, maximum,
                                         maximumBy, minimum, minimumBy,
                                         msum, sequence_)
import           Data.Function          hiding (id, (.))
import           Data.Functor
import           Data.List              hiding (foldl1, foldl1', foldr1, head,
                                         init, last, length, map, maximum,
                                         maximumBy, minimum, minimumBy, tail,
                                         (!!), (++))
import           Data.Maybe
import           Data.Traversable
import           Prelude                hiding (appendFile, concat, concatMap,
                                         foldl1, foldr1, getChar, getContents,
                                         getLine, head, id, init, interact,
                                         last, length, map, mapM_, maximum,
                                         mconcat, minimum, print, putChar,
                                         putStr, putStrLn, read, readFile,
                                         readIO, readLn, sequence_, tail,
                                         writeFile, (!!), (++), (.))
import qualified Prelude                as P
import           Text.Read              (readMaybe)


{- $setup
>>> import Debug.Trace
-}

-- |A variant of foldl that has no starting value argument.
foldr1 :: (Foldable t, MonadThrow m) => (a -> a -> a) -> t a -> m a
foldr1 f = foldr g Nothing >>> liftMaybe "foldr1: empty structure"
    where
    g a Nothing  = Just a
    g a (Just b) = Just $ f a b

-- |A variant of foldl that has no starting value argument.
foldl1 :: (Foldable t, MonadThrow m) => (a -> a -> a) -> t a -> m a
foldl1 f = foldl g Nothing >>> liftMaybe "foldl1: empty structure"
    where
    g Nothing a  = Just a
    g (Just b) a = Just $ f a b

{- |Returns the first element of a list.
prop> head (x:xs) == Just x
prop> head [] == Nothing
-}
head :: (MonadThrow m, Foldable t) => t a -> m a
head = foldr1 const >>> liftMaybe "head: empty structure"

{- |Returns the last element of a list.
prop> last (xs <> [x]) == Just x
prop> last [] == Nothing
-}
last :: (MonadThrow m, Foldable t) => t a -> m a
last = foldr1 (flip const) >>> liftMaybe "last: empty structure"

{- |Removes the first element of a list and returns the rest of the list.
prop> tail (x:xs) == Just xs
prop> tail [] == Nothing
-}
tail :: MonadThrow m => [a] -> m [a]
tail [] = throwEmptyListException "tail"
tail xs = return $ P.tail xs

{- |Removes the last element of a list and returns the rest of the list.
prop> init (xs <> [x]) == Just xs
prop> init [] == Nothing
-}
init :: MonadThrow m => [a] -> m [a]
init [] = throwEmptyListException "init"
init xs = return $ P.init xs

throwEmptyListException :: MonadThrow m => String -> m a
throwEmptyListException funName = throwString $ modulePrefix <> funName <> ": empty list"

{- |Returns the nth element of a list.
prop> (x:list) !! 0 == Just x
prop> list !! length list == Nothing
prop> [x,y,z] !! (-1) == Nothing
-}
(!!) :: MonadThrow m => [a] -> Int -> m a
_ !! n | n < 0 = throwString $ modulePrefix <> "!!: negative index"
(x:_)  !! 0 = return x
(_:xs) !! n = xs !! (n - 1)
[]     !! _ = throwString $ modulePrefix <> "!!: index too large"
infixl 9 !!

{- |Equivalent to (<>).
prop> (() ++ x) == (() <> x)
-}
(++) :: Semigroup a => a -> a -> a
(++) = (<>)
infixr 5 ++

{- |Equivalent to fmap.
prop> fmap (+ 1) (Just x) == map (+ 1) (Just x)
-}
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

{- |Maximum value of a foldable structure.
>>> (maximum [0,1,2,3] :: Maybe Int)
Just 3
>>> (maximum [] :: Maybe Int)
Nothing
-}
maximum :: (Ord a, Foldable t, MonadThrow m) => t a -> m a
maximum = maximumBy compare >>> liftMaybe "maximum: empty structure"

-- |A variant of maximum which takes a comparator.
maximumBy :: (Foldable t, MonadThrow m) => (a -> a -> Ordering) -> t a -> m a
maximumBy f = foldr1 g >>> liftMaybe "maximumBy: empty structure"
    where g a b = case f a b of GT -> a
                                _  -> b

{- |Minimum value of a foldable structure.
>>> (minimum [0,1,2,3] :: Maybe Int)
Just 0
>>> (minimum [] :: Maybe Int)
Nothing
-}
minimum :: (Ord a, Foldable t, MonadThrow m) => t a -> m a
minimum = minimumBy compare >>> liftMaybe "minimum: empty structure"

-- |A variant of minimum which takes a comparator.
minimumBy :: (Foldable t, MonadThrow m) => (a -> a -> Ordering) -> t a -> m a
minimumBy f = foldr1 g >>> liftMaybe "minimumBy: empty structure"
    where g a b = case f a b of GT -> b
                                _  -> a

-- |Equivalent to fold.
concat :: (Foldable t, Monoid m) => t m -> m
concat = fold

-- |Equivalent to fold.
mconcat :: (Foldable t, Monoid m) => t m -> m
mconcat = fold

-- |Equivalent to foldMap.
concatMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
concatMap = foldMap

-- |A lifted variant of putChar.
putChar :: MonadIO m => Char -> m ()
putChar = liftIO . P.putChar

-- |A lifted variant of putStr.
putStr :: MonadIO m => String -> m ()
putStr = liftIO . P.putStr

-- |A lifted variant of putStrLn.
putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . P.putStrLn

-- |A lifted variant of print.
print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . P.print

-- |A lifted variant of getChar.
getChar :: MonadIO m => m Char
getChar = liftIO P.getChar

-- |A lifted variant of getLine.
getLine :: MonadIO m => m String
getLine = liftIO P.getLine

-- |A lifted variant of getContents.
getContents :: MonadIO m => m String
getContents = liftIO P.getContents

-- |A lifted variant of interact.
interact :: MonadIO m => (String -> String) -> m ()
interact = liftIO . P.interact

-- |A lifted variant of readFile.
readFile :: MonadIO m => FilePath -> m String
readFile = liftIO . P.readFile

-- |A lifted variant of writeFile.
writeFile :: MonadIO m => FilePath -> String -> m ()
writeFile = fmap liftIO . P.writeFile

-- |A lifted variant of appendFile.
appendFile :: MonadIO m => FilePath -> String -> m ()
appendFile = fmap liftIO . P.appendFile

-- |A lifted variant of readIO.
readIO :: (MonadIO m, Read a) => String -> m a
readIO = liftIO . P.readIO

-- |A lifted variant of read.
read :: (MonadThrow m, Read a) => String -> m a
read = readMaybe >>> throwString (modulePrefix <> "read: no parse") `maybe` return

-- |Equivalent to traverse_.
mapM_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
mapM_ = traverse_

-- |Equivalent to for_.
forM_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
forM_ = for_

-- |Equivalent to sequenceA_.
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = sequenceA_

-- |Equivalent to asum.
msum :: (Foldable t, Alternative f) => t (f a) -> f a
msum = asum

modulePrefix :: String
modulePrefix = "Orelude."

-- |Lifts maybe to MonadThrow
liftMaybe :: MonadThrow m => String -> Maybe a -> m a
liftMaybe msg = throwString (modulePrefix <> msg) `maybe` return
