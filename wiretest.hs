{-# LANGUAGE Arrows, TypeOperators, OverloadedStrings #-}

import qualified Data.ByteString as BStr

import Control.Arrow ((>>>), (<<<), arr, Arrow, first)
import Control.Category (Category, id, (.))
import Control.Wire
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Serialize (Result(..), runGetPartial, Get)
import Data.ByteString (ByteString)
import Prelude hiding (id, (.))

import LibTIB (TibEvent, get_event)

parse_ev :: (Monoid e, MonadIO m) => IO ByteString -> Get b -> Wire s e m a b
parse_ev more getit = wait_more mempty f where
    wait_more bytes parse = mkGenN $ \_ -> case bytes of
        "" -> liftIO more >>= \b -> return (cont $ parse b)
        rest -> return . cont $ parse rest

    cont (Partial f') = (Left mempty, wait_more "" f')
    cont (Fail err rest) = (Left mempty, wait_more rest f)
    cont (Done ev rest) = (Right ev, wait_more rest f)

    f = runGetPartial getit

test_parse = do
    let wire = parse_ev (return test_dat) get_event :: Wire () () IO () TibEvent
    (rv, _) <- stepWire wire () $ Right ()
    print rv
    where
    test_dat = BStr.pack [0, 19, 132, 16, 153, 177, 248, 143, 231, 195, 237, 182, 134, 61, 47, 183, 151, 78, 230, 66, 86]
