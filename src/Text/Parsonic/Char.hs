{-# LANGUAGE Safe #-}

module Text.Parsonic.Char (
    alpha, digit, hexDigit, alphaNum, space, spaced, maybeSpaced
) where

import Text.Parsonic (Parser, satisfy, many, some, interspersedWith)
import safe Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace)

alpha :: Parser Char e Char
alpha = satisfy isAlpha

digit :: Parser Char e Char
digit = satisfy isDigit

hexDigit :: Parser Char e Char
hexDigit = satisfy isHexDigit

alphaNum :: Parser Char e Char
alphaNum = satisfy isAlphaNum

space :: Parser Char e Char
space = satisfy isSpace

spaced :: [Parser Char e Char] -> Parser Char e String
spaced = interspersedWith (some space)

maybeSpaced :: [Parser Char e Char] -> Parser Char e String
maybeSpaced = interspersedWith (many space)
