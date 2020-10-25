{- HLINT ignore "Use sum" -}
module OreludeSpec where

import           Orelude
import           Test.Hspec
import           Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "foldr1" $ do
        prop "behaves as foldr for a non-empty structure." $ \(x :: Int, list) ->
            foldr1 (+) (x:list) == Just (foldr (+) 0 (x:list))
        it "throws a monadic exception for an empty structure." $ do
            str <- extractMessage $ foldr1 (+) ([] :: [Int])
            str `shouldBe` "Orelude.foldr1: empty structure"

    describe "foldl1" $ do
        prop "behaves as foldl for a non-empty structure with an identity element." $ \(x :: Int, list) ->
            foldl1 (+) (x:list) == Just (foldl (+) 0 (x:list))
        it "throws a monadic exception for an empty structure." $ do
            str <- extractMessage $ foldl1 (+) ([] :: [Int])
            str `shouldBe` "Orelude.foldl1: empty structure"

    describe "head" $ do
        prop "returns the first element of a non-empty structure." $ \(x :: Int, list) ->
            head (x:list) == Just x
        it "throws a monadic exception for an empty structure." $ do
            str <- extractMessage $ head []
            str `shouldBe` "Orelude.head: empty structure"

    describe "last" $ do
        prop "returns the last element of a non-empty structure." $ \(list, x :: Int) ->
            last (list <> [x]) == Just x
        it "throws a monadic exception for an empty structure." $ do
            str <- extractMessage $ last []
            str `shouldBe` "Orelude.last: empty structure"

    describe "tail" $ do
        prop "Removes the first element of a list and returns the rest of the list." $ \(x :: Int, list) ->
            tail (x:list) == Just list
        it "throws a monadic exception for an empty list." $ do
            str <- extractMessage $ init []
            str `shouldBe` "Orelude.init: empty list"

    describe "init" $ do
        prop "Removes the first element of a list and returns the rest of the list." $ \(list, x :: Int) ->
            init (list <> [x]) == Just list
        it "throws a monadic exception for an empty list." $ do
            str <- extractMessage $ init []
            str `shouldBe` "Orelude.init: empty list"

    describe "!!" $ do
        prop "takes nth element of a list" $ \(x, y, z :: Int) ->
            [x,y,z] !! 1 `shouldBe` Just y
        context "throws a monadic exception for invalid accesses." $ do
            it "too large index" $ do
                str <- extractMessage $ ([1,2,3] :: [Int]) !! 4
                str `shouldBe` "Orelude.!!: index too large"
            it "negative index" $ do
                str <- extractMessage $ ([1,2,3] :: [Int]) !! (- 1)
                str `shouldBe` "Orelude.!!: negative index"

    describe "++" $
        context "behaves just as <>" $ do
            prop "still available for lists" $ \(list1, list2 :: [Int]) ->
                list1 ++ list2 == list1 <> list2
            prop "now available for any monoids" $ \(monoid1, monoid2 :: ()) ->
                monoid1 ++ monoid2 == monoid1 <> monoid2

    describe "map" $
        context "behaves just as fmap" $ do
            prop "still available for lists" $ \(list :: [Int]) ->
                map (+ 1) list == fmap (+ 1) list
            prop "now available for any functors" $ \(a :: Maybe Int) ->
                map (+ 1) a == fmap (+ 1) a

    describe "maximum" $ do
        prop "the maximum value of the non-empty structure" $ \(x :: Int, list) ->
            maximum (x:list) == Just (foldr max minBound (x:list))
        it "throws a monadic exception for an empty list." $ do
            str <- extractMessage $ maximum ([] :: [Int])
            str `shouldBe` "Orelude.maximum: empty structure"

    describe "minimum" $ do
        prop "the minimum value of the non-empty structure" $ \(x :: Int, list) ->
            minimum (x:list) == Just (foldr min maxBound (x:list))
        it "throws a monadic exception for an empty list." $ do
            str <- extractMessage $ minimum ([] :: [Int])
            str `shouldBe` "Orelude.minimum: empty structure"

    describe "read" $ do
        it "parses a string" $
            read "123" == Just (123 :: Int)
        it "throws a monadic exception when it failed to parse a string." $ do
            str <- extractMessage (read "a" :: Either SomeException Int)
            str `shouldBe` "Orelude.read: no parse"

extractMessage :: (MonadThrow m) => Either SomeException a -> m String
extractMessage e =
    case e of
        Right _ -> throwString "extract failed"
        Left someException ->
            case fromException someException of
                Nothing -> throwString "extract failed"
                Just (StringException str _) -> return str

