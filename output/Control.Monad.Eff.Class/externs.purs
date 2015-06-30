module Control.Monad.Eff.Class (MonadEff, liftEff) where
import Prelude ()
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
--  | The `MonadEff` class captures those monads which support native effects.
--  |
--  | Instances are provided for `Eff` itself, and the standard monad transformers.
--  |
--  | `liftEff` can be used in any appropriate monad transformer stack to lift an action
--  | of type `Eff eff a` into the monad.
--  |
--  | Note that `MonadEff` is parameterized by the row of effects, so type inference can be
--  | tricky. It is generally recommended to either work with a polymorphic row of effects,
--  | or a concrete, closed row of effects such as `(trace :: Trace)`.
--  | The `MonadEff` class captures those monads which support native effects.
--  |
--  | Instances are provided for `Eff` itself, and the standard monad transformers.
--  |
--  | `liftEff` can be used in any appropriate monad transformer stack to lift an action
--  | of type `Eff eff a` into the monad.
--  |
--  | Note that `MonadEff` is parameterized by the row of effects, so type inference can be
--  | tricky. It is generally recommended to either work with a polymorphic row of effects,
--  | or a concrete, closed row of effects such as `(trace :: Trace)`.
--  | The `MonadEff` class captures those monads which support native effects.
--  |
--  | Instances are provided for `Eff` itself, and the standard monad transformers.
--  |
--  | `liftEff` can be used in any appropriate monad transformer stack to lift an action
--  | of type `Eff eff a` into the monad.
--  |
--  | Note that `MonadEff` is parameterized by the row of effects, so type inference can be
--  | tricky. It is generally recommended to either work with a polymorphic row of effects,
--  | or a concrete, closed row of effects such as `(trace :: Trace)`.
class (Prelude.Monad m) <= MonadEff eff m where
  liftEff :: forall a. Control.Monad.Eff.Eff eff a -> m a
foreign import instance monadEffEff :: Control.Monad.Eff.Class.MonadEff eff (Control.Monad.Eff.Eff eff)