module Tutorial where

  -- This is a mostly PureScript implementation of the tutorial found here:
  -- http://facebook.github.io/react/docs/tutorial.html

  import Control.Monad.Eff
  import Data.Array
  import React
  import Showdown
  import Debug.Trace

  import qualified React.DOM as DOM

  cBoxRender = do
    state <- readState
    pure $ DOM.div { className: "commentBox" }
                   [ DOM.h1 {} [ DOM.text "Comments" ]
                   , commentList { data': state }
                   , commentForm { onCommentSubmit: handle commentSubmit }
                   ]

  commentBox = mkStatefulUIFromSpec [] cBoxRender $
    defaultStatefulSpec {
      componentWillMount = componentWillMount
    }

  foreign import componentWillMount
    "function componentWillMount() {\
    \  var load = loadCommentsFromServer.bind(this);\
    \  load();\
    \  setInterval(function() { load(); }, this.props.pollInterval);\
    \}" :: forall eff props state. ReadState eff props state {}


  commentList = mkUI do
    props <- getProps
    pure $ DOM.div { className: "commentList" }
                   (commentNodes <$> props.data')

  commentForm = mkUI do
    props <- getProps
    pure $ DOM.form { className: "commentForm"
                    , onSubmit: handle submit
                    }
                    [ DOM.input { attrType: "text"
                                , placeholder: "Your name"
                                , ref: "author"
                                }
                                []
                    , DOM.input { attrType: "text"
                                , placeholder: "Say something..."
                                , ref: "text"
                                }
                                []
                    , DOM.input { attrType: "submit"
                                , value: "Post"
                                }
                                []
                    ]

  comment = mkUI do
    props <- getProps
    pure $ DOM.div { className: "comment" }
                   [ DOM.h2 { className: "commentAuthor" }
                            [ DOM.text props.author ]
                   , DOM.span { dangerouslySetInnerHTML: { __html: makeHtml props.children } }
                              []
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
    "function submit() {\
    \  var author = this.refs.author.getDOMNode().value.trim();\
    \  var text = this.refs.text.getDOMNode().value.trim();\
    \  this.props.onCommentSubmit.call(this, {author: author, text: text});\
    \  this.refs.author.getDOMNode().value = '';\
    \  this.refs.text.getDOMNode().value = '';\
    \  return false;\
    \}" :: forall eff. Eff eff Boolean

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
