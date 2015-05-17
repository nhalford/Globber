module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do
    describe "empty pattern" $ do
        it "matches empty string" $
            matchGlob "" "" `shouldBe` True
        it "shouldn't match non-empty string" $
            matchGlob "" "string" `shouldBe` False
    describe "question mark cases" $ do
        it "matches any single character" $
            matchGlob "?" "*" `shouldBe` True
        it "matches any character at the beginning of a longer string" $
            matchGlob "?s" "xs" `shouldBe` True
        it "matches any character within a longer string" $
            matchGlob "a?s" "axs" `shouldBe` True
    describe "star cases" $ do
        it "matches the empty string" $
            matchGlob "*" "" `shouldBe` True
        it "matches longer strings" $
            matchGlob "*" "a longer string" `shouldBe` True
        it "matches at least one character when followed by a question mark" $
            matchGlob "*?" "abc" `shouldBe` True
        it "does not match the empty string when followed by a question mark" $
            matchGlob "*?" "" `shouldBe` False
        it "should match up to escaped characters" $
            matchGlob "*\\?abc" "xyz?abc" `shouldBe` True
        it "should match up to unescaped literals" $
            matchGlob "*abc" "xyzabc" `shouldBe` True
        it "should match with nothing within a string" $
            matchGlob "xyz*abc" "xyzabc" `shouldBe` True
        it "should match an arbitrary string within a string" $
            matchGlob "xyz*abc" "xyz glob abc" `shouldBe` True
    describe "escaped characters" $ do
        it "matches literals" $
            matchGlob "\\a\\b\\c" "abc" `shouldBe` True
        it "matches special characters" $
            matchGlob "a\\*b" "a*b" `shouldBe` True
        it "does not reduce to other glob matching" $
            matchGlob "a\\?c" "abc" `shouldBe` False
