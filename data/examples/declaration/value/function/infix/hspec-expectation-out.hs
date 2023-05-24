import Test.Hspec

my_tests = do
  context "my context" $
    it "does something" $
      property $ \pos xs0 xs1 ->
        FancyError pos xs0 <> FancyError pos xs1
          `shouldBe` (FancyError pos (E.union xs0 xs1) :: PE)
