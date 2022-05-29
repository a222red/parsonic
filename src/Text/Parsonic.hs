{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

module Text.Parsonic (
    Parser (..),
    ParseError (..),
    eof,
    unexpected,
    satisfy,
    notFollowedBy,
    manyUntil,
    char,
    string,
    count,
    between,
    option,
    choice,
    sepBy,
    sepBy1,
    chainl,
    chainl1
) where

import Control.Applicative (Alternative (..))
import safe Data.Functor ()
import Data.Semigroup (Semigroup (..))

data ParseError i e = Empty
    | Unexpected i
    | ExpectedEof i
    | CustomError e
    | EndOfInput deriving (Show)

newtype Parser i e t = Parser {
    runParser :: [i] -> Either (ParseError i e) (t, [i])
}

instance Functor (Parser i e) where
    fmap f (Parser p) = Parser $ \input -> do
        (output, rest) <- p input
        pure (f output, rest)

instance Applicative (Parser i e) where
    pure a = Parser $ \input -> Right (a, input)

    Parser f <*> Parser p = Parser $ \input -> do
        (f', rest) <- f input
        (output, rest') <- p rest
        pure (f' output, rest')

instance Monad (Parser i e) where
    return = pure

    Parser p >>= k = Parser $ \input -> do
        (output, rest) <- p input
        runParser (k output) rest

instance Alternative (Parser i e) where
    empty = Parser $ \_ -> Left Empty

    Parser l <|> Parser r = Parser $ \input -> case l input of
        Left err -> case r input of
            Left err' -> Left err
            Right (output, rest) -> Right (output, rest)
        Right (output, rest) -> Right (output, rest)

instance (Semigroup t) => Semigroup (Parser i e t) where
    Parser l <> Parser r = Parser $ \input -> do
        (output, rest) <- l input
        (output', rest') <- r rest
        return (output <> output', rest')

eof :: Parser i e ()
eof = Parser $ \case
    [] -> Right ((), [])
    hd:rest -> Left (ExpectedEof hd)

unexpected :: Parser i e t
unexpected = Parser $ \case
    [] -> Left EndOfInput
    hd:rest -> Left (Unexpected hd)

customError :: e -> Parser i e t
customError msg = Parser $ const (Left (CustomError msg))

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \case
    [] -> Left EndOfInput
    hd:rest
        | predicate hd -> Right (hd, rest)
        | otherwise -> Left (Unexpected hd)

notFollowedBy :: Parser i e t0 -> Parser i e t1 -> Parser i e t1
notFollowedBy (Parser e) (Parser p) = Parser $ \input -> case p input of
    Left err -> Left err
    Right (output, rest) -> case e rest of
        Left _ -> Right (output, rest)
        Right (_, rest') -> Left (Unexpected (head rest))

manyUntil :: Parser i e t0 -> Parser i e t1 -> Parser i e [t1]
manyUntil (Parser e) (Parser p) = Parser $ \input -> case e input of
    Left _ -> case p input of
        Left err -> Left err
        Right (output, rest) ->
            case runParser (manyUntil (Parser e) (Parser p)) rest of
                Left err -> Left err
                Right (output', rest') -> Right (output:output', rest')
    Right (_, _) -> Right ([], input)

char :: Eq i => i -> Parser i e i
char c = satisfy (==c)

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

count :: Int -> Parser i e t -> Parser i e [t]
count 0 _ = return []
count n p = (:) <$> p <*> count (n - 1) p

between :: Parser i e t0 -> Parser i e t1 -> Parser i e t2 -> Parser i e t2
between left right middle = left *> middle <* right

option :: t -> Parser i e t -> Parser i e t
option x p = p <|> return x

choice :: [Parser i e t] -> Parser i e t
choice = foldr (<|>) unexpected

sepBy :: Parser i e t0 -> Parser i e t1 -> Parser i e [t1]
sepBy sep p = sepBy1 sep p <|> return []

sepBy1 :: Parser i e t0 -> Parser i e t1 -> Parser i e [t1]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)

chainl :: Parser i e t -> Parser i e (t -> t -> t) -> t -> Parser i e t
chainl p op x = chainl1 p op <|> return x

chainl1 :: Parser i e t -> Parser i e (t -> t -> t) -> Parser i e t
chainl1 p op = p >>= rest
    where rest x = ((op <*> return x <*> p) >>= rest) <|> return x
