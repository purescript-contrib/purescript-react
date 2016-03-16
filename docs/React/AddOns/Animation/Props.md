## Module React.Addons.Animation.Props

See the [Animation section](https://facebook.github.io/react/docs/animation.html) of the React documentation for details concerning usage/API.
This API is byte-for-byte compatible.
Note:  You must `npm install react-addons-css-transition-group` to use this module.

#### `transitionName`

``` purescript
transitionName :: String -> Props
```

#### `TransitionNames`

``` purescript
type TransitionNames = { enter :: String, enterActive :: String, leave :: String, leaveActive :: String, appearActive :: String }
```

#### `transitionName'`

``` purescript
transitionName' :: TransitionNames -> Props
```

#### `transitionAppear`

``` purescript
transitionAppear :: Boolean -> Props
```

#### `transitionEnter`

``` purescript
transitionEnter :: Boolean -> Props
```

#### `transitionLeave`

``` purescript
transitionLeave :: Boolean -> Props
```

#### `transitionEnterTimeout`

``` purescript
transitionEnterTimeout :: Int -> Props
```

#### `transitionLeaveTimeout`

``` purescript
transitionLeaveTimeout :: Int -> Props
```

#### `component`

``` purescript
component :: String -> Props
```


