module React.Types where

  import Control.Monad.Eff (Eff())

  import Data.Function (Fn2(), Fn3())

  import DOM (DOM())

  -- These should be moved to DOM.
  foreign import data DOMEvent :: *
  foreign import data DOMEventTarget :: *
  foreign import data Element :: *
  foreign import data Event :: !

  -- Use for methods we don't want users to access.
  foreign import data BlackList :: *
  -- A Component is an instantiated Component, i.e. it has props and children.
  foreign import data Component :: *
  foreign import data React :: !

  type ReactSyntheticEvent fields =
    { bubbles          :: Boolean
    , cancelable       :: Boolean
    , currentTarget    :: DOMEventTarget
    , defaultPrevented :: Boolean
    , eventPhase       :: Number
    , isTrusted        :: Boolean
    , nativeEvent      :: DOMEvent
    , preventDefault   :: forall eff. Eff (event :: Event | eff) Unit
    , stopPropagation  :: forall eff. Eff (event :: Event | eff) Unit
    , target           :: DOMEventTarget
    , timeStamp        :: Number -- We need an actual Date type here.
    , "type"           :: String
    | fields
    }
  type ReactFormEvent = ReactSyntheticEvent (unit :: Unit) -- Should be empty row.

  -- A ComponentClass is a builder for a Component.
  type ComponentClass props state = props -> [Component] -> Component

  type Spec fields props state s eff mixins statics =
    { render                    :: Render                    fields {children :: [Component] | props} { | state} eff
    , componentDidMount         :: ComponentDidMount         fields {                        | props} { | state} eff
    , componentDidUpdate        :: ComponentDidUpdate        fields {                        | props} { | state} eff
    , componentWillMount        :: ComponentWillMount        fields {                        | props} { | state} eff
    , componentWillReceiveProps :: ComponentWillReceiveProps fields {                        | props} { | state} eff
    , componentWillUnmount      :: ComponentWillUnmount      fields {                        | props} { | state} eff
    , componentWillUpdate       :: ComponentWillUpdate       fields {                        | props} { | state} eff
    , getInitialState           :: GetInitialState           fields                                   { | state} eff
    , shouldComponentUpdate     :: ShouldComponentUpdate     fields {                        | props} { | state} eff
    , displayName               :: String
    , mixins                    :: [{ | mixins}]
    , statics                   :: { | statics}
    | s
    }

  type This fields =
    { isMounted :: forall eff. Eff (react :: React | eff) Boolean
    | fields
    }

  type ReactThis fields props state = This
    ( state :: state
    , props :: props
    , refs :: forall r. { | r}
    , setState :: state -> Unit
    , replaceState :: state -> Unit
    | fields
    )

  type Render fields props state eff
    =  This ( props :: props
            , state :: state
            , forceUpdate :: BlackList
            , getDOMNode :: BlackList
            , replaceState :: BlackList
            , setState :: BlackList
            | fields
            )
    -> Eff (react :: React, dom :: DOM | eff) Component

  type GetInitialState fields state eff
    =  This ( props :: BlackList
            , state :: BlackList
            , forceUpdate :: BlackList
            , getDOMNode :: BlackList
            , replaceState :: BlackList
            , setState :: BlackList
            | fields
            )
    -> Eff (react :: React, dom :: DOM | eff) state

  type ComponentDidMount fields props state eff
    =  ReactThis ( getDOMNode :: Eff (react :: React, dom :: DOM | eff) Element
                 | fields
                 )
                 props
                 state
    -> Eff (react :: React, dom :: DOM | eff) Unit

  type ComponentWillMount fields props state eff
    =  ReactThis (getDOMNode :: BlackList | fields) props state
    -> Eff (react :: React, dom :: DOM | eff) Unit

  type ComponentWillUnmount fields props state eff
    =  ReactThis ( getDOMNode :: Eff (react :: React, dom :: DOM | eff) Element
                 | fields
                 )
                 props
                 state
    -> Eff (react :: React, dom :: DOM | eff) Unit

  type ComponentWillReceiveProps fields props state eff =
    Fn2 (ReactThis fields props state)
        props
        (Eff (react :: React, dom :: DOM | eff) Boolean)

  type ShouldComponentUpdate fields props state eff =
    Fn3 (ReactThis fields props state)
        props
        state
        (Eff (react :: React, dom :: DOM | eff) Boolean)

  type ComponentDidUpdate fields props state eff =
    Fn3 (ReactThis fields props state)
        props
        state
        (Eff (react :: React, dom :: DOM | eff) Boolean)

  type ComponentWillUpdate fields props state eff =
    Fn3 (This ( props :: props
              , state :: props
              , forceUpdate :: Eff (react :: React, dom :: DOM | eff) Unit
              , getDOMNode :: Eff (react :: React, dom :: DOM | eff) Element
              , replaceState :: BlackList
              , setState :: BlackList
              | fields
              )
        )
        props
        state
        (Eff (react :: React, dom :: DOM | eff) Boolean)
