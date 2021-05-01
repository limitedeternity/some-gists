{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.TypeLits
import Data.Typeable
import System.Environment
import Control.Monad

-- | An existential box to place anything that is a Handler and KnownSymbol.
data SomeHandler
   = forall h. (KnownSymbol h, Handler h) => SomeHandler (Proxy h)

class Handler h where
    -- We need to pass the proxy in here because a class has to work on a 
    -- value that mentions at least one type variable.
    handleIt :: Proxy h -> IO ()

-- | The type just goes inline as a literal.
instance Handler "dance" where
    handleIt _ = putStrLn "*   *\n \\o/\n _|\n   \\"

-- | The user will need to place the commands in here to be iterated over to
-- check for a match.
handlers :: [SomeHandler]
handlers = [SomeHandler (Proxy :: Proxy "dance")]

same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = case sameSymbol a b of
             Just _ -> True
             Nothing -> False

handleCommand :: [String] -> IO ()
handleCommand []      = pure ()
handleCommand (str:_) =
    -- A case statement is needed to extract the existential here, or GHC's
    -- brain explodes.
    case someSymbolVal str of
        SomeSymbol user_proxy ->
            forM_ handlers $ \(SomeHandler proxy) ->
                when (same user_proxy proxy) (handleIt proxy)

main :: IO ()
main = getArgs >>= handleCommand
