module TwitterSearch
( Data.Text
, something
) where

import qualified Data.ByteString as B

type Year = Integer
type Month = Integer
type Day = Integer

data Query = Token B.ByteString
           | And Query Query
           | Phrase B.ByteString
           | Or Query Query
           | Not Query
           | HashTag B.ByteString
           | From B.ByteString
           | To B.ByteString
           | At B.ByteString
           | Since Date
           | Until Date
           | Positive
           | Negative
           | Question
           | Links
           | Source B.ByteString
