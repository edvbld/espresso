import qualified Espresso.Lexer as Lexer

main :: IO()
main = print $ Lexer.lex "123 aab 23"
