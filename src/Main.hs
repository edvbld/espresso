import qualified Espresso.Parser as Parser

main :: IO()
main = print $ Parser.parse "foo"
