module Test.Main (main, counter, incrementCounter, hello, helloInConsole, interval) where
import Prelude ()
import React ()
import Control.Monad.Eff.Console ()
import React.DOM ()
import Test.Main ()
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
import Control.Monad.Eff.Console ()
import React ()
import React.DOM ()
foreign import main :: forall t322. Control.Monad.Eff.Eff (dom :: React.DOM | t322) React.UI
foreign import counter :: forall t204. t204 -> React.UI
foreign import incrementCounter :: forall t11 t18 t25. t11 -> Control.Monad.Eff.Eff (state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed | t25) Prim.Int | t18) Prim.Int
foreign import hello :: forall t166. { name :: Prim.String | t166 } -> React.UI
foreign import helloInConsole :: forall t30 t45 t46. t30 -> Control.Monad.Eff.Eff (console :: Control.Monad.Eff.Console.CONSOLE, props :: React.ReactProps { name :: Prim.String | t45 } | t46) Prelude.Unit
foreign import interval :: forall eff a. Prim.Int -> Control.Monad.Eff.Eff (console :: Control.Monad.Eff.Console.CONSOLE | eff) a -> Control.Monad.Eff.Eff (console :: Control.Monad.Eff.Console.CONSOLE | eff) Prelude.Unit