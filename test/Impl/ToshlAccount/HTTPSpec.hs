{-# LANGUAGE QuasiQuotes #-}
module Impl.ToshlAccount.HTTPSpec where

import Test.Hspec
import Impl.ToshlAccount.HTTP
import Text.RawString.QQ

spec :: Spec
spec =
    describe "parseToshlPaging" $ do
        it "should parse last page" $ do
            let str = [r|</tags?page=0>; rel="first", </tags?page=2>; rel="last", </tags?page=1>; rel="previous"|]
            parseToshlPaging str `shouldBe` Right (ToshlPaging { pageFirst = Just "/tags?page=0"
                                                               , pageLast = Just "/tags?page=2"
                                                               , pageNext = Nothing
                                                               , pagePrevious = Just "/tags?page=1"
                                                               })
