HTML2XML
--------

> Convert your invalid XML to valid XML with the power of TagSoup

Example:

```haskell
repair :: StringLike s => s -> s
repair "<head><p></head>" -- Outputs "<head><p></p></head>"
```

Example with conduit:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.XML
import Text.XML.Cursor
import Network.HTTP.Conduit
import Text.HTML2XML
import Data.ByteString.Lazy.Char8 as LB

main = do
  str <- fmap repair $ simpleHttp url

  LB.putStrLn str

  let doc = fromDocument $ parseLBS_ def str
      dss = doc $// localNameIs "a" &// content

  print dss

localNameIs ln = checkName $ \n -> nameLocalName n == ln

url = "http://www.yesodweb.com/book/xml"

-- Currently failing with errorMessage = "Failed reading: takeWhile1"
```
