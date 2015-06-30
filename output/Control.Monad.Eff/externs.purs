module Control.Monad.Eff (Pure(), Eff(), foreachE, forE, whileE, untilE, runPure) where
import Prelude ()
import Control.Monad.Eff ()
import Prim ()
import Prelude ()
--  | The `Eff` type constructor is used to represent _native_ effects.
--  |
--  | See [Handling Native Effects with the Eff Monad](https://github.com/purescript/purescript/wiki/Handling-Native-Effects-with-the-Eff-Monad) for more details.
--  |
--  | The first type parameter is a row of effects which represents the contexts in which a computation can be run, and the second type parameter is the return type.
--  | The `Pure` type synonym represents _pure_ computations, i.e. ones in which all effects have been handled.
--  |
--  | The `runPure` function can be used to run pure computations and obtain their result.
--  | Run a pure computation and return its result.
--  |
--  | Note: since this function has a rank-2 type, it may cause problems to apply this function using the `$` operator. The recommended approach
--  | is to use parentheses instead.
--  | Loop until a condition becomes `true`.
--  |
--  | `untilE b` is an effectful computation which repeatedly runs the effectful computation `b`,
--  | until its return value is `true`.
--  | Loop while a condition is `true`.
--  |
--  | `whileE b m` is effectful computation which runs the effectful computation `b`. If its result is
--  | `true`, it runs the effectful computation `m` and loops. If not, the computation ends.
--  | Loop over a consecutive collection of numbers.
--  |
--  | `forE lo hi f` runs the computation returned by the function `f` for each of the inputs
--  | between `lo` (inclusive) and `hi` (exclusive).
--  | Loop over an array of values.
--  |
--  | `foreach xs f` runs the computation returned by the function `f` for each of the inputs `xs`.
type Pure (a :: *) = forall e. Control.Monad.Eff.Eff e a
foreign import data Eff :: # ! -> * -> *
foreign import foreachE :: forall e a. Prim.Array a -> (a -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import forE :: forall e. Prim.Number -> Prim.Number -> (Prim.Number -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import whileE :: forall e a. Control.Monad.Eff.Eff e Prim.Boolean -> Control.Monad.Eff.Eff e a -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import untilE :: forall e. Control.Monad.Eff.Eff e Prim.Boolean -> Control.Monad.Eff.Eff e Prelude.Unit
foreign import runPure :: forall a. Control.Monad.Eff.Pure a -> a
foreign import instance functorEff :: Prelude.Functor (Control.Monad.Eff.Eff e)
foreign import instance applyEff :: Prelude.Apply (Control.Monad.Eff.Eff e)
foreign import instance applicativeEff :: Prelude.Applicative (Control.Monad.Eff.Eff e)
foreign import instance bindEff :: Prelude.Bind (Control.Monad.Eff.Eff e)
foreign import instance monadEff :: Prelude.Monad (Control.Monad.Eff.Eff e)