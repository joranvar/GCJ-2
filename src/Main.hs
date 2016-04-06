import GCJ (R(..), Runner(..))
import Solution (solve')

main :: IO ()
main = interact (solve (R solve'))
