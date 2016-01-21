import           Test.DocTest
main :: IO ()
main = doctest ["-isrc", "src/HerfTime.hs","src/HerfTime/ZonedTime.hs"]


