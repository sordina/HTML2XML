module Text.HTML2XML (repair, StringLike(..)) where

import Control.Monad
import Control.Monad.Identity
import Text.HTML.TagSoup
import Text.StringLike
import qualified Control.Monad.Trans.State.Lazy as ST

data State a = State {_incomming :: a, _open :: a, result :: a}

repair :: StringLike s => s -> s
repair = runIdentity . (transform . parseTags >=> return . renderTags . reverse . result)

transform :: StringLike s => [Tag s] -> Identity (State [Tag s])
transform = ST.execStateT tagCloser . initialState

initialState :: [a] -> State [a]
initialState a = State a [] []

tagCloser :: StringLike s => ST.StateT (State [Tag s]) Identity ()
tagCloser = do
  State i o r <- ST.get -- Return when there is nothing left to process (match failure)
  if null i then cleanup o r
            else ST.modify move >> tagCloser

move :: StringLike a => State [Tag a] -> State [Tag a]
move (State []     _ _) = error "unreachable code"
move (State (x:xs) o r) = shuffle x xs o r

cleanup :: (Monad m, StringLike str) => [Tag str] -> t -> ST.StateT (State [Tag str]) m ()
cleanup o _ = mapM_ (ST.modify . close) o
  where close (TagOpen s _) (State _ (_:os) r) = State [] os (TagClose s : r)
        close _             _                  = error $ "Invalid tags on open-tag stack [" ++ toString (renderTags o) ++ "]"

shuffle :: StringLike a => Tag a -> [Tag a] -> [Tag a] -> [Tag a] -> State [Tag a]
shuffle t@(TagOpen s _) ts o r | s == fromString "!DOCTYPE" = State ts     (o)   (t:r)            -- Non Closing tags
shuffle t@(TagOpen _ _) ts o                  r             = State ts     (t:o) (t:r)            -- Push new open tags onto the stack
shuffle   (TagClose  _) ts []                 r             = State ts     []    r                -- Ignore close tags if there are no open tags on the stack
shuffle t@(TagClose  n) ts (TagOpen s _ : os) r | n == s    = State ts     os    (t:r)            -- If open and close tags match, close the tag, pop the stack
                                                | otherwise = State (t:ts) os    (TagClose s : r) --    Otherwise, close the top open tag
shuffle t               ts os                 r             = State ts     os    (t:r)            -- All other tags are added to the result, ignoring the stack
