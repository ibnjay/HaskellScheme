module Parser ( parseScheme, parseSchemeList ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import Datatypes

symbol :: Parser Char
symbol = initialSymbol <|> oneOf "+-.@"
 
-- '+', '-', and '.' aren't allowed to be the first character of an atom
initialSymbol :: Parser Char
initialSymbol = oneOf "!#$%&*/:<=>?^_~"
 
spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = char '\\' >> fmap escape anyChar
    where escape 'n' = '\n'
          escape 'r' = '\r'
          escape 't' = '\t'
          escape x   = x
 
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapeChar <|> noneOf "\"")
    char '"'
    return $ String x
 
parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> initialSymbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of 
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \s ->
    case reads s of
        [(n, _)] -> return (Number n)
        _ -> fail ""

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseAnyList

parseList :: Parser LispVal
parseList = List `fmap` sepBy parseExpr space

parseAnyList :: Parser LispVal
parseAnyList = do
    char '('
    many space
    x <- sepEndBy parseExpr spaces
    c <- char '.' <|> char ')'
    if c == '.'
        then do 
          y <- spaces >> parseExpr
          many space
          char ')'
          return $ DottedList x y
        else return $ List x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseComment :: Parser LispVal
parseComment = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseGeneric :: Parser a -> String -> ThrowsError a
parseGeneric parser input = case parse parser "scheme" input of
                              Left err -> throwError $ Parser err
                              Right val -> return val

parseScheme = parseGeneric parseExpr
parseSchemeList = parseGeneric $ sepEndBy (try parseExpr) spaces
