module HTML2XML where

import Control.Monad
import Control.Monad.Identity
import Text.HTML.TagSoup
import Text.StringLike
import qualified Control.Monad.Trans.State.Lazy as State

data CloseState a = CloseState {incomming :: a, open :: a, result :: a}

repair :: StringLike s => s -> s
repair = runIdentity . (transform . parseTags >=> return . renderTags . reverse . result)

transform :: StringLike s => [Tag s] -> Identity (CloseState [Tag s])
transform = State.execStateT tagCloser . initialState

initialState :: [a] -> CloseState [a]
initialState a = CloseState a [] []

tagCloser :: StringLike s => State.StateT (CloseState [Tag s]) Identity ()
tagCloser = do
  CloseState i o r <- State.get -- Return when there is nothing left to process (match failure)
  if null i then cleanup o r
            else State.modify move >> tagCloser

move (CloseState []     _ _) = error "unreachable code"
move (CloseState (x:xs) o r) = shuffle x xs o r

cleanup o _ = mapM_ (State.modify . close) o
  where close (TagOpen s _) (CloseState _ (_:os) r) = CloseState [] os (TagClose s : r)
        close _             _                       = error $ "Invalid tags on open-tag stack [" ++ toString (renderTags o) ++ "]"

shuffle t@(TagOpen _ _) ts o                  r             = CloseState ts     (t:o) (t:r)            -- Push new open tags onto the stack
shuffle   (TagClose  _) ts []                 r             = CloseState ts     []    r                -- Ignore close tags if there are no open tags on the stack
shuffle t@(TagClose  n) ts (TagOpen s _ : os) r | n == s    = CloseState ts     os    (t:r)            -- If open and close tags match, close the tag, pop the stack
                                                | otherwise = CloseState (t:ts) os    (TagClose s : r) --    Otherwise, close the top open tag
shuffle t               ts os                 r             = CloseState ts     os    (t:r)            -- All other tags are added to the result, ignoring the stack
