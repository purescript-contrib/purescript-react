module Control.Monad.Eff.Console (CONSOLE(), print, error, log) where
import Prelude ()
import Control.Monad.Eff.Console ()
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
--  | The `CONSOLE` effect represents those computations which write to the console.
--  | Write a message to the console.
--  | Write an error to the console.
--  | Write a value to the console, using its `Show` instance to produce a `String`.
foreign import data CONSOLE :: !
foreign import print :: forall a eff. (Prelude.Show a) => a -> Control.Monad.Eff.Eff (console :: Control.Monad.Eff.Console.CONSOLE | eff) Prelude.Unit
foreign import error :: forall eff. Prim.String -> Control.Monad.Eff.Eff (console :: Control.Monad.Eff.Console.CONSOLE | eff) Prelude.Unit
foreign import log :: forall eff. Prim.String -> Control.Monad.Eff.Eff (console :: Control.Monad.Eff.Console.CONSOLE | eff) Prelude.Unit