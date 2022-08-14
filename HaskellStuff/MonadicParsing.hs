{-# LANGUAGE InstanceSigs, FlexibleInstances, LambdaCase #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Function
import Numeric


newtype ParserError = ParserError 
    { unwrapError :: String }

instance Alternative (Either ParserError) where
    empty :: Either ParserError a
    empty = Left (ParserError "Empty parser")

    (<|>) :: Either ParserError a -> Either ParserError a -> Either ParserError a
    Left _ <|> b = b
    a <|> _ = a

instance MonadPlus (Either ParserError)


newtype Parser a = Parser
    { unwrapParser :: StateT String (Either ParserError) a }

runParser = runStateT . unwrapParser

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f a = Parser $ f <$> unwrapParser a

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ pure a

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    f <*> a = Parser $ unwrapParser f <*> unwrapParser a

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= f = Parser $ unwrapParser a >>= (unwrapParser . f)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser . StateT . const $ empty 

    (<|>) :: Parser a -> Parser a -> Parser a
    a <|> b = Parser $ unwrapParser a <|> unwrapParser b

instance MonadPlus Parser


anyChar :: Parser Char
anyChar = Parser . StateT $ \case 
    [] -> empty 
    (c:cs) -> pure (c, cs)

parseIf :: (Char -> Bool) -> Parser Char
parseIf cond = anyChar >>= \c -> 
    if cond c then pure c
    else Parser . StateT . const $ Left (ParserError "Unmet condition")

parseChar :: Char -> Parser Char
parseChar = parseIf . (==)

parseSpace :: Parser String
parseSpace = many (parseIf isSpace)

parseString :: String -> Parser String
parseString = foldr (\c -> (<*>) ((:) <$> parseChar c)) (pure [])

parseStringLiteral :: Parser String
parseStringLiteral = parseChar '"' *> many (parseIf ((&&) <$> (/= '"') <*> (/= '\\')) <|> processEscapes) <* parseChar '"'
    where
        processEscapes :: Parser Char
        processEscapes = ('"' <$ parseString "\\\"") <|>
                         ('\\' <$ parseString "\\\\") <|>
                         ('/' <$ parseString "\\/") <|>
                         ('\b' <$ parseString "\\b") <|>
                         ('\f' <$ parseString "\\f") <|>
                         ('\n' <$ parseString "\\n") <|>
                         ('\r' <$ parseString "\\r") <|>
                         ('\t' <$ parseString "\\t") <|>
                         (parseString "\\u" *> unicodeEscape)
            where
                unicodeEscape :: Parser Char
                unicodeEscape = chr . fst . head . readHex <$> replicateM 4 (parseIf isHexDigit)

parseDoubleLiteral :: Parser Double
parseDoubleLiteral =
    doubleFromParts
        <$> (minus <|> pure 1)
        <*> (read <$> digits)
        <*> ((read <$> (('0':) <$> ((:) <$> parseChar '.' <*> digits))) <|> pure 0)
        <*> (((parseChar 'e' <|> parseChar 'E') *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0)
    where
        digits = some (parseIf isDigit)
        minus = (-1) <$ parseChar '-'
        plus = 1 <$ parseChar '+'

        doubleFromParts :: Integer  -- sign
                        -> Integer  -- integral part
                        -> Double   -- decimal part
                        -> Integer  -- exponent
                        -> Double
        doubleFromParts sign int dec expo = fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)

sepBy :: Parser a   -- Parser for the separators
      -> Parser b   -- Parser for elements
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []


main :: IO ()
main = undefined
