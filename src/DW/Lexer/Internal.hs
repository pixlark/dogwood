{-# LANGUAGE BangPatterns #-}

module DW.Lexer.Internal where

import DW.Common
import DW.Util

import Data.Char
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Internal (Text (..))
import Data.Text.Unsafe (Iter (..), iter, lengthWord8)

data TokenKind = Eof | Symbol !Text | Keyword !Text | Glyph !Text | IntLiteral !Int
  deriving (Eq, Show)

data Token = Token {kind :: !TokenKind, span :: !Span}
  deriving (Eq, Show)

data Lexer = Lexer {cursor :: !Int, byteOffset :: !Int, source :: !Text, srcLen :: !Int}
  deriving (Show)

{-# INLINEABLE unsafeCodepointAtByte #-}
unsafeCodepointAtByte :: Text -> Int -> (Char, Int)
unsafeCodepointAtByte text byteOffset = (c, len)
  where
    Iter c len = iter text byteOffset

{-# INLINEABLE advance #-}
advance :: (State Lexer :> es) => Eff es ()
advance = modify advance'
  where
    advance' lexer =
      if lexer.byteOffset >= lexer.srcLen
        then lexer
        else
          let (_, len) = unsafeCodepointAtByte lexer.source lexer.byteOffset
           in lexer {byteOffset = lexer.byteOffset + len, cursor = lexer.cursor + 1}

{-# INLINEABLE advanceBy #-}
advanceBy :: (State Lexer :> es) => Int -> Eff es ()
advanceBy n = forM_ [1 .. n] $ \_ -> do advance

{-# INLINEABLE current #-}
current :: (State Lexer :> es) => Eff es (Maybe Char)
current = gets current'
  where
    current' :: Lexer -> Maybe Char
    current' lexer =
      if lexer.byteOffset >= lexer.srcLen
        then Nothing
        else
          let (c, _) = unsafeCodepointAtByte lexer.source lexer.byteOffset
           in Just c

makeLexer :: T.Text -> Lexer
makeLexer source = Lexer {cursor = 0, byteOffset = 0, source, srcLen = lengthWord8 source}

keywords :: [T.Text]
keywords =
  [ "void",
    "arr",
    "bool",
    "int",
    "new",
    "cast",
    "if",
    "else",
    "switch",
    "let",
    "return",
    "loop",
    "fn",
    "where",
    "struct",
    "union",
    "enum",
    "typeclass",
    "instance",
    "true",
    "false",
    "break",
    "undefined",
    "builtin",
    "any"
  ]

validGlyphs :: [T.Text]
validGlyphs =
  [ "[",
    "]",
    "{",
    "}",
    "(",
    ")",
    "+",
    "-",
    "*",
    "/",
    "=",
    "==",
    "!=",
    ">",
    "<",
    ">=",
    "<=",
    "&",
    "&&",
    "||",
    "!",
    "->",
    "=>",
    ",",
    ",",
    ",",
    ":",
    "::",
    ";",
    "%"
  ]

validGlyphStarts :: [Char]
validGlyphStarts = List.nub $ map T.head validGlyphs

tryMakeSingleGlyph :: (State Lexer :> es) => Char -> Eff es (Maybe Token)
tryMakeSingleGlyph c =
  if s `elem` validGlyphs
    then do
      token <- makeToken 1 (Glyph s)
      return $ Just token
    else return Nothing
  where
    s = T.pack [c]

tryMakeDoubleGlyph :: (State Lexer :> es) => Char -> Char -> Eff es (Maybe Token)
tryMakeDoubleGlyph c1 c2 =
  if s `elem` validGlyphs
    then do
      advance
      token <- makeToken 2 (Glyph s)
      return $ Just token
    else return Nothing
  where
    s = T.pack [c1, c2]

makeToken :: (State Lexer :> es) => Int -> TokenKind -> Eff es Token
makeToken length kind = do
  cursor <- gets cursor
  return $ Token kind $ Span (cursor - length) length

{- HLINT ignore "Redundant <$>" -}
nextToken :: (State Lexer :> es, Errors Err :> es) => Eff es Token
nextToken = do
  skipWhitespace
  c <- current
  cur <- gets cursor
  case c of
    Nothing -> makeToken 0 Eof
    Just c ->
      let unrecognizedCharacter = Err (UnrecognizedCharacter c) (Span cur 1)
       in if
            | isDigit c -> do
                (src, cur) <- gets ((,) <$> source <*> cursor)
                let numberString = T.takeWhile isDigit $ T.drop cur src
                let length = T.length numberString
                advanceBy length
                makeToken length $ IntLiteral $ read $ T.unpack numberString
            | isAlpha c || c == '_' -> do
                (src, cur) <- gets ((,) <$> source <*> cursor)
                let symbol = T.takeWhile (\c -> isAlpha c || isDigit c || c == '_') $ T.drop cur src
                let length = T.length symbol
                advanceBy length
                if symbol `elem` keywords
                  then makeToken length $ Keyword symbol
                  else makeToken length $ Symbol symbol
            | c `elem` validGlyphStarts -> do
                advance
                c' <- current
                case c' of
                  Nothing -> eitherFromMaybe unrecognizedCharacter <$> tryMakeSingleGlyph c >>= except
                  Just c' -> eitherFromMaybe unrecognizedCharacter <$> tryMakeDoubleOrSingleGlyph c c' >>= except
            | otherwise -> throwErr unrecognizedCharacter
  where
    skipWhitespace = do
      c <- current
      when (c == Just '#') skipRestOfLine
      c <- current
      when (c == Just ' ' || c == Just '\n') (do advance; skipWhitespace)
    skipRestOfLine = do
      c <- current
      advance
      when (c /= Just '\n') skipRestOfLine
    tryMakeDoubleOrSingleGlyph c c' = do
      doubleGlyph <- tryMakeDoubleGlyph c c'
      case doubleGlyph of
        Nothing -> tryMakeSingleGlyph c
        j@(Just _) -> return j
    except (Right value) = return value
    except (Left e) = throwErr e
