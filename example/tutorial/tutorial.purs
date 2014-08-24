module Tutorial where

  -- This is a mostly PureScript implementation of the tutorial found here:
  -- http://facebook.github.io/react/docs/tutorial.html

  import Control.Monad.Eff (Eff())

  import Data.Array () -- Just want the instances.
  import Data.Function (mkFn3)
  import Data.String (trim)

  import Debug.Trace (trace, Trace())

  import DOM (DOM())

  import Network.HTTP (Verb(..))
  import Network.Oboe
    ( done
    , fail
    , oboe
    , oboeGet
    , oboeOptions
    , Oboe()
    , OboeEff()
    )

  import React
    ( createClass
    , eventHandler
    , renderComponentById
    , spec
    )
  import React.Types
    ( Component()
    , ComponentClass()
    , Event()
    , React()
    , ReactFormEvent()
    , ReactThis()
    )

  import Showdown (makeHtml)

  import qualified React.DOM as D

  foreign import setInterval
    "function setInterval(f) {\
    \  return function(time) {\
    \    return function() {\
    \      window.setInterval(f, time);\
    \    }\
    \  }\
    \}" :: forall eff a. Eff eff a -> Number -> Eff eff Unit
  foreign import unsafeCorece
    "function unsafeCorece(x) {\
    \  return x;\
    \}" :: forall a b. a -> b
  foreign import stringify
    "function stringify(x) {\
    \  return JSON.stringify(x, null, 4);\
    \}" :: forall a. a -> String

  newtype Comment = Comment {author :: String, text :: String}
  type CommentProps = {author :: String}
  type CommentState = {}
  type CommentBoxProps = {url :: String, pollInterval :: Number}
  type CommentPropsState = {comments :: [Comment]}
  type CommentFormProps =
    {onCommentSubmit :: forall eff
                     .  Comment
                     -> Eff (oboe :: OboeEff, trace :: Trace | eff) Oboe
    }
  type CommentFormState = {}

  instance eqComment :: Eq Comment where
    (==) (Comment c) (Comment c') = c.author == c'.author && c.text == c'.text
    (/=) c c' = not (c == c')

  comment :: ComponentClass CommentProps CommentState
  comment = createClass spec
    { displayName = "Comment"
    , render = \this -> do
      let rawMarkup = case this.props.children of
            []    -> ""
            (x:_) -> makeHtml $ unsafeCorece x
      pure $ D.div {className: "comment"}
        [ D.h2 {className: "commentAuthor"}
            [D.rawText this.props.author]
        , D.span {dangerouslySetInnerHTML: {__html: rawMarkup}} []
        ]
    }

  commentBox :: ComponentClass CommentBoxProps CommentPropsState
  commentBox = createClass spec
    { displayName = "CommentBox"
    , getInitialState = \_ -> pure {comments: []}
    , componentDidMount = \this -> do
      loadCommentsFromServer this
      setInterval (loadCommentsFromServer this) this.props.pollInterval
    , shouldComponentUpdate = mkFn3 \this _ state -> do
      pure $ state.comments /= this.state.comments
    , render = \this -> pure $
      D.div {className: "commentBox"}
        [ D.h1 {} [D.rawText "Comments"]
        , commentList {comments: this.state.comments} []
        , commentForm {onCommentSubmit: handleCommentSubmit $ unsafeCorece this} []
        ]
    }

  commentList :: ComponentClass {comments :: [Comment]} {}
  commentList = createClass spec
    { displayName = "CommentList"
    , render = \this -> pure $ D.div {className: "commentList"} $
      (\(Comment c) -> comment {author: c.author} [D.rawText c.text]) <$> this.props.comments
    }

  commentForm :: ComponentClass CommentFormProps CommentFormState
  commentForm = createClass spec
    { displayName = "CommentForm"
    , render = \this -> pure $ D.form
      { className: "commentForm"
      , onSubmit: eventHandler this handleSubmit
      }
      [ D.input {"type": "text", placeholder: "Your name", ref: "author"} []
      , D.input {"type": "text", placeholder: "Say something...", ref: "text"} []
      , D.input {"type": "submit", value: "Post"} []
      ]
    }

  loadCommentsFromServer :: forall eff fields
                         .  ReactThis fields CommentBoxProps CommentPropsState
                         -> Eff (oboe :: OboeEff, trace :: Trace | eff) Oboe
  loadCommentsFromServer this = do
    o <- oboeGet this.props.url
    done o (\json -> pure $ this.setState {comments: unsafeCorece json})
    fail o (\obj  -> trace obj.body)

  handleCommentSubmit :: forall eff fields
                      .  ReactThis fields CommentBoxProps CommentPropsState
                      -> Comment
                      -> Eff (oboe :: OboeEff, trace :: Trace | eff) Oboe
  handleCommentSubmit this comment = do
    o <- oboe oboeOptions
      { url = this.props.url
      , method = POST
      , body = unsafeCorece comment
      }
    done o (\json -> pure $ this.setState {comments: unsafeCorece json})
    fail o (\obj  -> trace obj.body)

  handleSubmit :: forall eff fields
               .  ReactThis fields CommentFormProps CommentFormState
               -> ReactFormEvent
               -> Eff (event :: Event, oboe :: OboeEff, trace :: Trace | eff) Boolean
  handleSubmit this event = do
    author <- (\n -> n.value # trim) <$> this.refs.author.getDOMNode
    text   <- (\n -> n.value # trim) <$> this.refs.text.getDOMNode
    this.props.onCommentSubmit $ Comment {author: author, text: text}
    pure false

  main :: Eff (react :: React, dom :: DOM) Component
  main = renderComponentById
    (commentBox {url: "comments.json", pollInterval: 2000} [])
    "content"
