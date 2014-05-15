module Tutorial where

  -- This is a mostly PureScript implementation of the tutorial found here:
  -- http://facebook.github.io/react/docs/tutorial.html

  import Control.Monad.Eff
  import Data.Array
  import React
  import React.DOM
  import Showdown
  import Debug.Trace

  cBoxRender = do
    state <- readState
    pure $ div [ className "commentBox" ]
                   [ h1' [ text "Comments" ]
                   , commentList { data': state }
                   , commentForm { onCommentSubmit: commentSubmit }
                   ]

  commentBox = mkUI spec {
      getInitialState = return [],
      componentWillMount = componentWillMount
    } cBoxRender

  foreign import componentWillMount
    "function componentWillMount() {\
    \  var load = loadCommentsFromServer.bind(this);\
    \  load();\
    \  setInterval(function() { load(); }, this.props.pollInterval);\
    \}" :: forall eff props state. ReadState eff props state {}


  commentList = mkUI spec do
    props <- getProps
    pure $ div [ className "commentList" ]
                   (commentNodes <$> props.data')

  commentForm = mkUI spec do
    props <- getProps
    pure $ form [ className "commentForm"
                    , onSubmit submit
                    ]
                    [ input [ typeProp "text"
                            , placeholder "Your name"
                            , ref "author"
                            ] []
                    , input [ typeProp "text"
                            , placeholder "Say something..."
                            , ref "text"
                            ] []
                    , input [ typeProp "submit"
                            , value "Post"
                            ] []
                    ]

  comment = mkUI spec do
    props <- getProps
    pure $ div [ className "comment" ]
                   [ h2 [ className "commentAuthor" ]
                            [ text props.author ]
                   , span [ dangerouslySetInnerHTML $ makeHtml props.children ] []
                   ]

  commentNodes c = comment { author: c.author, children: c.text }

  foreign import commentSubmit
    "function commentSubmit(comment) {\
    \  var comments = this.state.state;\
    \  var newComments = comments.concat([comment]);\
    \  this.setState({state: newComments});\
    \  $.ajax({\
    \    url: this.props.url,\
    \    dataType: 'json',\
    \    type: 'POST',\
    \    data: comment,\
    \    success: function(data) {\
    \      this.setState({state: data});\
    \    }.bind(this)\
    \  });\
    \}" :: forall eff. Eff eff {}

  foreign import submit
    "function submit(e) {\
    \  e.preventDefault();\
    \  return function() { \
    \    var author = this.refs.author.getDOMNode().value.trim();\
    \    var text = this.refs.text.getDOMNode().value.trim();\
    \    this.props.onCommentSubmit.call(this, {author: author, text: text});\
    \    this.refs.author.getDOMNode().value = '';\
    \    this.refs.text.getDOMNode().value = '';\
    \    return false;\
    \  } \
    \}" :: Event -> forall eff. Eff eff Boolean

  foreign import loadCommentsFromServer
    "function loadCommentsFromServer() {\
    \  $.ajax({\
    \    url: this.props.url,\
    \    dataType: 'json',\
    \    success: function(data) {\
    \      this.replaceState({state: data});\
    \    }.bind(this)\
    \  });\
    \}" :: forall a r. {props :: {url :: String}, replaceState :: {state :: a} -> {} | r} -> {}

  main = renderToElementById "content" $ commentBox { url: "comments.json"
                                                    , pollInterval: 2000
                                                    }
