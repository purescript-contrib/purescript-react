module ProductTable where

  import Control.Monad.Eff

  import Data.Array
  import Data.Function

  import React

  import qualified Data.String as S
  import qualified React.DOM as DOM

  type ProductType = { category :: String
                     , price :: String
                     , stocked :: Boolean
                     , name :: String
                     }

  filterableProductTable = mkUI filterSpec do
    props <- getProps
    state <- readState
    pure $ DOM.div { className: "filterable-product-table" }
                   [ searchBar { filterText: state.filterText
                               , inStockOnly: state.inStockOnly
                               , onUserInput: handle userInput
                               }
                   , productTable { products: props.products
                                  , filterText: state.filterText
                                  , inStockOnly: state.inStockOnly
                                  }
                   ]

  filterSpec = spec { getInitialState = initialState }

  -- The initial state wont type if we just return this record in ps.
  foreign import initialState
    "function initialState() {\
    \  return { filterText: '', inStockOnly: false };\
    \}" :: forall a eff. Eff eff a

  -- This is a dirty hack because we can't really work with callbacks properly.
  -- We lose the `this` context when it is called from anywhere else.
  foreign import userInput
    "function userInput(state) {\
    \  React.writeState(state);\
    \}" :: forall a. a

  searchBar = mkUI spec do
    props <- getProps
    pure $ DOM.form {}
                    [ DOM.input { attrType: "input"
                                , onChange: handle userInputChange
                                , placeholder: "Search..."
                                , ref: "filterTextInput"
                                , value: props.filterText
                                }
                                []
                    , DOM.p {} [ DOM.input { attrType: "checkbox"
                                           , ref: "inStockOnlyInput"
                                           , value: props.inStockOnly
                                           , onChange: handle userInputChange
                                           }
                                           []
                               , DOM.text "Only show products in stock"
                               ]
                    ]

    where
      userInputChange = do
        props <- getProps
        refs <- getRefs
        let ft = (getDOMNode refs.filterTextInput).value
        let iso = (getDOMNode refs.inStockOnlyInput).checked
        pure $ props.onUserInput { filterText: ft
                                 , inStockOnly: iso
                                 }

  foreign import getDOMNode
    "function getDOMNode(x) {\
    \  return x.getDOMNode();\
    \}" :: forall a b. a -> b

  productTable = mkUI spec do
    props <- getProps
    let pred p = (not props.inStockOnly || p.stocked) && p.name `contains` props.filterText
    let filtered = filter pred props.products
    pure $ DOM.div { className: "product-table" }
                   [ productTable' filtered ]
      where
        contains str sub = sub `S.indexOf` str /= -1

  productTable' :: forall r. [ProductType] -> UI
  productTable' rows =
    DOM.table {}
      [ DOM.thead {}
          [ DOM.tr {}
              [ DOM.th {} [ DOM.text "Name" ]
              , DOM.th {} [ DOM.text "Price" ]
              ]
          ]
      , DOM.tbody {} categorized
      ]
      where
        categorized = concat $ grouped2Categorized <$> (groupBy' _category rows)

  productCategoryRow product =
    DOM.tr {}
      [ DOM.th { colSpan: "2" }
          [ DOM.text product.category ]
      ]

  productRow product =
    DOM.tr {}
      [ DOM.td {}
          [ DOM.span { className: (if product.stocked then "" else "red") }
                     [ DOM.text product.name ]
          ]
      , DOM.td {} [ DOM.text product.price ]
      ]

  -- Helper functions.

  grouped2Categorized :: [ProductType] -> [UI]
  grouped2Categorized []       = []
  grouped2Categorized xs@(x:_) = (productCategoryRow x) : (productRow <$> xs)

  _category :: forall a r. { category :: a | r} -> a
  _category {category = c} = c
  _price :: forall a r. { price :: a | r} -> a
  _price {price = p} = p
  _stocked :: forall a r. { stocked :: a | r} -> a
  _stocked {stocked = s} = s
  _name :: forall a r. { name :: a | r} -> a
  _name {name = n} = n

  groupBy' :: forall a b. (Ord b) => (a -> b) -> [a] -> [[a]]
  groupBy' p = groupBy ((==) `on` p) <<< sortBy (compare `on` p)

  products =
    [ { category: "Sporting Goods"
      , price: "$49.99"
      , stocked: true
      , name: "Football"
      }
    , { category: "Electronics"
      , price: "$399.99"
      , stocked: false
      , name: "iPhone 5"
      }
    , { category: "Sporting Goods"
      , price: "$9.99"
      , stocked: true
      , name: "Baseball"
      }
    , { category: "Sporting Goods"
      , price: "$29.99"
      , stocked: false
      , name: "Basketball"
      }
    , { category: "Electronics"
      , price: "$99.99"
      , stocked: true
      , name: "iPod Touch"
      }
    , { category: "Electronics"
      , price: "$199.99"
      , stocked: true
      , name: "Nexus 7"
      }
    ]

  main =
    renderToElementById "content" (filterableProductTable {products: products})
