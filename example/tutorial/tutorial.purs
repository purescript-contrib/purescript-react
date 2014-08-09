module Tutorial where

  -- This is a mostly PureScript implementation of the tutorial found here:
  -- http://facebook.github.io/react/docs/tutorial.html

  import Control.Monad.Eff (Eff())

  import Data.Array ()

  import DOM (DOM())

  import React
    ( createClass
    , renderToId
    , spec
    , Component()
    , ComponentClass()
    , React()
    )

  import Showdown (makeHtml)

  import qualified React.DOM as D

  foreign import undefined :: forall a. a

  type CommentProps = {url :: String, pollInterval :: Number}
  type CommentState = {comments :: [Comment]}
  type Comment = {author :: String, text :: String}

  commentBox :: ComponentClass CommentProps CommentState
  commentBox = createClass spec
    { render = \this -> pure $ D.div
        {className: "commentBox"}
        [ D.h1 {} [D.rawText "Comments"]
        , commentList {comments: this.state.comments} []
        , commentForm {} []
        ]
    , getInitialState = \_ -> pure {comments: []}
    }

  commentForm :: ComponentClass {} {}
  commentForm = createClass spec
    { render = \_ -> pure $ D.form
        {className: "commentForm"}
        [ D.input
            { "type": "text"
            , placeholder: "Your name"
            , ref: "author"
            } []
        , D.input
            { "type": "text"
            , placeholder: "Say something..."
            , ref: "text"
            } []
        , D.input
            { "type": "submit"
            , value: "Post"
            } []
        ]
    }

  commentList :: ComponentClass {comments :: [Comment]} {}
  commentList = createClass spec
    { render = \this -> pure $ D.div
        {className: "commentList"}
        (commentNode <$> this.props.comments)
    }

  commentNode :: Comment -> Component
  commentNode = flip comment []

  comment :: ComponentClass Comment Component
  comment = createClass spec
    { render = \this -> pure $ D.div
        {className: "comment"}
        [ D.h2 {className: "commentAuthor"} [D.rawText this.props.author]
        , D.span {dangerouslySetInnerHTML: makeHtml this.props.text} []
        ]
    }

  -- cBoxRender = do
  --   state <- readState
  --   pure $ div [ className "commentBox" ]
  --                  [ h1' [ text "Comments" ]
  --                  , commentList { data': state }
  --                  , commentForm { onCommentSubmit: commentSubmit }
  --                  ]

  -- commentBox = mkUI spec {
  --     getInitialState = return [],
  --     componentWillMount = componentWillMount
  --   } cBoxRender

  -- foreign import componentWillMount
  --   "function componentWillMount() {\
  --   \  var load = loadCommentsFromServer.bind(this);\
  --   \  load();\
  --   \  setInterval(function() { load(); }, this.props.pollInterval);\
  --   \}" :: forall eff props state. ReadState eff props state {}

  -- commentForm = mkUI spec do
  --   props <- getProps
  --   pure $ form [ className "commentForm"
  --                   , onSubmit submit
  --                   ]
  --                   [ input [ typeProp "text"
  --                           , placeholder "Your name"
  --                           , ref "author"
  --                           ] []
  --                   , input [ typeProp "text"
  --                           , placeholder "Say something..."
  --                           , ref "text"
  --                           ] []
  --                   , input [ typeProp "submit"
  --                           , value "Post"
  --                           ] []
  --                   ]


  -- foreign import commentSubmit
  --   "function commentSubmit(comment) {\
  --   \  var comments = this.state.state;\
  --   \  var newComments = comments.concat([comment]);\
  --   \  this.setState({state: newComments});\
  --   \  $.ajax({\
  --   \    url: this.props.url,\
  --   \    dataType: 'json',\
  --   \    type: 'POST',\
  --   \    data: comment,\
  --   \    success: function(data) {\
  --   \      this.setState({state: data});\
  --   \    }.bind(this)\
  --   \  });\
  --   \}" :: forall eff. Eff eff {}

  -- foreign import submit
  --   "function submit(e) {\
  --   \  e.preventDefault();\
  --   \  return function() { \
  --   \    var author = this.refs.author.getDOMNode().value.trim();\
  --   \    var text = this.refs.text.getDOMNode().value.trim();\
  --   \    this.props.onCommentSubmit.call(this, {author: author, text: text});\
  --   \    this.refs.author.getDOMNode().value = '';\
  --   \    this.refs.text.getDOMNode().value = '';\
  --   \    return false;\
  --   \  } \
  --   \}" :: Event -> forall eff. Eff eff Boolean

  -- foreign import loadCommentsFromServer
  --   "function loadCommentsFromServer() {\
  --   \  $.ajax({\
  --   \    url: this.props.url,\
  --   \    dataType: 'json',\
  --   \    success: function(data) {\
  --   \      this.replaceState({state: data});\
  --   \    }.bind(this)\
  --   \  });\
  --   \}" :: forall a r. {props :: {url :: String}, replaceState :: {state :: a} -> {} | r} -> {}

  main :: Eff (react :: React, dom :: DOM) Component
  main = let component = commentBox { url: "comments.json", pollInterval: 2000} [] in
    renderToId "content" component
