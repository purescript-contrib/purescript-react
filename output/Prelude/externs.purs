module Prelude (Ordering(LT, GT, EQ), Unit(), Show, BooleanAlgebra, BoundedOrd, Bounded, Ord, Eq, DivisionRing, Num, Ring, ModuloSemiring, Semiring, Semigroup, Monad, Bind, Applicative, Apply, Functor, Category, Semigroupoid, show, (||), (&&), not, disj, conj, bottom, top, (>=), (<=), (>), (<), compare, (/=), (==), eq, (-), negate, sub, (/), mod, div, (*), (+), one, mul, zero, add, (++), (<>), append, ap, liftM1, return, (>>=), bind, liftA1, pure, (<*>), apply, void, (<#>), (<$>), map, id, (>>>), (<<<), compose, otherwise, asTypeOf, const, flip, (#), ($), unit) where
import Prelude ()
import Prim ()
--  | The `Unit` type has a single inhabitant, called `unit`. It represents
--  | values with no computational content.
--  |
--  | `Unit` is often used, wrapped in a monadic type constructor, as the
--  | return type of a computation where only
--  | the _effects_ are important.
--  | The `Show` type class represents those types which can be converted into
--  | a human-readable `String` representation.
--  |
--  | While not required, it is recommended that for any expression `x`, the
--  | string `show x` be executable PureScript code which evaluates to the same
--  | value as the expression `x`.
--  | The `Semiring` class is for types that support an addition and
--  | multiplication operation.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Commutative monoid under addition:
--  |   - Associativity: `(a + b) + c = a + (b + c)`
--  |   - Identity: `zero + a = a + zero = a`
--  |   - Commutative: `a + b = b + a`
--  | - Monoid under multiplication:
--  |   - Associativity: `(a * b) * c = a * (b * c)`
--  |   - Identity: `one * a = a * one = a`
--  | - Multiplication distributes over addition:
--  |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
--  |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
--  | - Annihiliation: `zero * a = a * zero = zero`
--  | A `Semigroupoid` is similar to a [`Category`](#category) but does not
--  | require an identity element `id`, just composable morphisms.
--  |
--  | `Semigroupoid`s must satisfy the following law:
--  |
--  | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
--  |
--  | One example of a `Semigroupoid` is the function type constructor `(->)`,
--  | with `(<<<)` defined as function composition.
--  | The `Semigroup` type class identifies an associative operation on a type.
--  |
--  | Instances are required to satisfy the following law:
--  |
--  | - Associativity: `(x <> y) <> z = x <> (y <> z)`
--  |
--  | One example of a `Semigroup` is `String`, with `(<>)` defined as string
--  | concatenation.
--  | The `Ring` class is for types that support addition, multiplication,
--  | and subtraction operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Additive inverse: `a - a = (zero - a) + a = zero`
--  | The `Ordering` data type represents the three possible outcomes of
--  | comparing two values:
--  |
--  | `LT` - The first value is _less than_ the second.
--  | `GT` - The first value is _greater than_ the second.
--  | `EQ` - The first value is _equal to_ the second.
--  | The `ModuloSemiring` class is for types that support addition,
--  | multiplication, division, and modulo (division remainder) operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Remainder: `a / b * b + (a `mod` b) = a`
--  | A `Functor` is a type constructor which supports a mapping operation
--  | `(<$>)`.
--  |
--  | `(<$>)` can be used to turn functions `a -> b` into functions
--  | `f a -> f b` whose argument and return types use the type constructor `f`
--  | to represent some computational context.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Identity: `(<$>) id = id`
--  | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
--  | The `Eq` type class represents types which support decidable equality.
--  |
--  | `Eq` instances should satisfy the following laws:
--  |
--  | - Reflexivity: `x == x = true`
--  | - Symmetry: `x == y = y == x`
--  | - Transitivity: if `x == y` and `y == z` then `x == z`
--  | The `Ord` type class represents types which support comparisons with a
--  | _total order_.
--  |
--  | `Ord` instances should satisfy the laws of total orderings:
--  |
--  | - Reflexivity: `a <= a`
--  | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
--  | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
--  | A `Ring` where every nonzero element has a multiplicative inverse.
--  |
--  | Instances must satisfy the following law in addition to the `Ring` and
--  | `ModuloSemiring` laws:
--  |
--  | - Multiplicative inverse: `(one / x) * x = one`
--  |
--  | As a consequence of this ```a `mod` b = zero``` as no divide operation
--  | will have a remainder.
--  | The `Num` class is for types that are commutative fields.
--  |
--  | Instances must satisfy the following law in addition to the
--  | `DivisionRing` laws:
--  |
--  | - Commutative multiplication: `a * b = b * a`
--  | `Category`s consist of objects and composable morphisms between them, and
--  | as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
--  | must have an identity element.
--  |
--  | Instances must satisfy the following law in addition to the
--  | `Semigroupoid` law:
--  |
--  | - Identity: `id <<< p = p <<< id = p`
--  | The `Bounded` type class represents types that are finite.
--  |
--  | Although there are no "internal" laws for `Bounded`, every value of `a`
--  | should be considered less than or equal to `top` by some means, and greater
--  | than or equal to `bottom`.
--  |
--  | The lack of explicit `Ord` constraint allows flexibility in the use of
--  | `Bounded` so it can apply to total and partially ordered sets, boolean
--  | algebras, etc.
--  | The `BoundedOrd` type class represents totally ordered finite data types.
--  |
--  | Instances should satisfy the following law in addition to the `Ord` laws:
--  |
--  | - Ordering: `bottom <= a <= top`
--  | The `BooleanAlgebra` type class represents types that behave like boolean
--  | values.
--  |
--  | Instances should satisfy the following laws in addition to the `Bounded`
--  | laws:
--  |
--  | - Associativity:
--  |   - `a || (b || c) = (a || b) || c`
--  |   - `a && (b && c) = (a && b) && c`
--  | - Commutativity:
--  |   - `a || b = b || a`
--  |   - `a && b = b && a`
--  | - Distributivity:
--  |   - `a && (b || c) = (a && b) || (a && c)`
--  |   - `a || (b && c) = (a || b) && (a || c)`
--  | - Identity:
--  |   - `a || bottom = a`
--  |   - `a && top = a`
--  | - Idempotent:
--  |   - `a || a = a`
--  |   - `a && a = a`
--  | - Absorption:
--  |   - `a || (a && b) = a`
--  |   - `a && (a || b) = a`
--  | - Annhiliation:
--  |   - `a || top = top`
--  | - Complementation:
--  |   - `a && not a = bottom`
--  |   - `a || not a = top`
--  | The `Apply` class provides the `(<*>)` which is used to apply a function
--  | to an argument under a type constructor.
--  |
--  | `Apply` can be used to lift functions of two or more arguments to work on
--  | values wrapped with the type constructor `f`. It might also be understood
--  | in terms of the `lift2` function:
--  |
--  | ```purescript
--  | lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
--  | lift2 f a b = f <$> a <*> b
--  | ```
--  |
--  | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
--  | the function application operator `($)` to arguments wrapped with the
--  | type constructor `f`.
--  |
--  | Instances must satisfy the following law in addition to the `Functor`
--  | laws:
--  |
--  | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
--  |
--  | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
--  | The `Bind` type class extends the [`Apply`](#apply) type class with a
--  | "bind" operation `(>>=)` which composes computations in sequence, using
--  | the return value of one computation to determine the next computation.
--  |
--  | The `>>=` operator can also be expressed using `do` notation, as follows:
--  |
--  | ```purescript
--  | x >>= f = do y <- x
--  |              f y
--  | ```
--  |
--  | where the function argument of `f` is given the name `y`.
--  |
--  | Instances must satisfy the following law in addition to the `Apply`
--  | laws:
--  |
--  | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
--  |
--  | Associativity tells us that we can regroup operations which use `do`
--  | notation so that we can unambiguously write, for example:
--  |
--  | ```purescript
--  | do x <- m1
--  |    y <- m2 x
--  |    m3 x y
--  | ```
--  | The `Applicative` type class extends the [`Apply`](#apply) type class
--  | with a `pure` function, which can be used to create values of type `f a`
--  | from values of type `a`.
--  |
--  | Where [`Apply`](#apply) provides the ability to lift functions of two or
--  | more arguments to functions whose arguments are wrapped using `f`, and
--  | [`Functor`](#functor) provides the ability to lift functions of one
--  | argument, `pure` can be seen as the function which lifts functions of
--  | _zero_ arguments. That is, `Applicative` functors support a lifting
--  | operation for any number of function arguments.
--  |
--  | Instances must satisfy the following laws in addition to the `Apply`
--  | laws:
--  |
--  | - Identity: `(pure id) <*> v = v`
--  | - Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
--  | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
--  | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
--  | The `Monad` type class combines the operations of the `Bind` and
--  | `Applicative` type classes. Therefore, `Monad` instances represent type
--  | constructors which support sequential composition, and also lifting of
--  | functions of arbitrary arity.
--  |
--  | Instances must satisfy the following laws in addition to the
--  | `Applicative` and `Bind` laws:
--  |
--  | - Left Identity: `pure x >>= f = f x`
--  | - Right Identity: `x >>= pure = x`
--  | A `Semigroupoid` is similar to a [`Category`](#category) but does not
--  | require an identity element `id`, just composable morphisms.
--  |
--  | `Semigroupoid`s must satisfy the following law:
--  |
--  | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
--  |
--  | One example of a `Semigroupoid` is the function type constructor `(->)`,
--  | with `(<<<)` defined as function composition.
--  | `Category`s consist of objects and composable morphisms between them, and
--  | as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
--  | must have an identity element.
--  |
--  | Instances must satisfy the following law in addition to the
--  | `Semigroupoid` law:
--  |
--  | - Identity: `id <<< p = p <<< id = p`
--  | A `Functor` is a type constructor which supports a mapping operation
--  | `(<$>)`.
--  |
--  | `(<$>)` can be used to turn functions `a -> b` into functions
--  | `f a -> f b` whose argument and return types use the type constructor `f`
--  | to represent some computational context.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Identity: `(<$>) id = id`
--  | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
--  | The `Apply` class provides the `(<*>)` which is used to apply a function
--  | to an argument under a type constructor.
--  |
--  | `Apply` can be used to lift functions of two or more arguments to work on
--  | values wrapped with the type constructor `f`. It might also be understood
--  | in terms of the `lift2` function:
--  |
--  | ```purescript
--  | lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
--  | lift2 f a b = f <$> a <*> b
--  | ```
--  |
--  | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
--  | the function application operator `($)` to arguments wrapped with the
--  | type constructor `f`.
--  |
--  | Instances must satisfy the following law in addition to the `Functor`
--  | laws:
--  |
--  | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
--  |
--  | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
--  | The `Applicative` type class extends the [`Apply`](#apply) type class
--  | with a `pure` function, which can be used to create values of type `f a`
--  | from values of type `a`.
--  |
--  | Where [`Apply`](#apply) provides the ability to lift functions of two or
--  | more arguments to functions whose arguments are wrapped using `f`, and
--  | [`Functor`](#functor) provides the ability to lift functions of one
--  | argument, `pure` can be seen as the function which lifts functions of
--  | _zero_ arguments. That is, `Applicative` functors support a lifting
--  | operation for any number of function arguments.
--  |
--  | Instances must satisfy the following laws in addition to the `Apply`
--  | laws:
--  |
--  | - Identity: `(pure id) <*> v = v`
--  | - Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
--  | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
--  | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
--  | The `Bind` type class extends the [`Apply`](#apply) type class with a
--  | "bind" operation `(>>=)` which composes computations in sequence, using
--  | the return value of one computation to determine the next computation.
--  |
--  | The `>>=` operator can also be expressed using `do` notation, as follows:
--  |
--  | ```purescript
--  | x >>= f = do y <- x
--  |              f y
--  | ```
--  |
--  | where the function argument of `f` is given the name `y`.
--  |
--  | Instances must satisfy the following law in addition to the `Apply`
--  | laws:
--  |
--  | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
--  |
--  | Associativity tells us that we can regroup operations which use `do`
--  | notation so that we can unambiguously write, for example:
--  |
--  | ```purescript
--  | do x <- m1
--  |    y <- m2 x
--  |    m3 x y
--  | ```
--  | The `Monad` type class combines the operations of the `Bind` and
--  | `Applicative` type classes. Therefore, `Monad` instances represent type
--  | constructors which support sequential composition, and also lifting of
--  | functions of arbitrary arity.
--  |
--  | Instances must satisfy the following laws in addition to the
--  | `Applicative` and `Bind` laws:
--  |
--  | - Left Identity: `pure x >>= f = f x`
--  | - Right Identity: `x >>= pure = x`
--  | The `Semigroup` type class identifies an associative operation on a type.
--  |
--  | Instances are required to satisfy the following law:
--  |
--  | - Associativity: `(x <> y) <> z = x <> (y <> z)`
--  |
--  | One example of a `Semigroup` is `String`, with `(<>)` defined as string
--  | concatenation.
--  | The `Semiring` class is for types that support an addition and
--  | multiplication operation.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Commutative monoid under addition:
--  |   - Associativity: `(a + b) + c = a + (b + c)`
--  |   - Identity: `zero + a = a + zero = a`
--  |   - Commutative: `a + b = b + a`
--  | - Monoid under multiplication:
--  |   - Associativity: `(a * b) * c = a * (b * c)`
--  |   - Identity: `one * a = a * one = a`
--  | - Multiplication distributes over addition:
--  |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
--  |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
--  | - Annihiliation: `zero * a = a * zero = zero`
--  | The `Ring` class is for types that support addition, multiplication,
--  | and subtraction operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Additive inverse: `a - a = (zero - a) + a = zero`
--  | The `ModuloSemiring` class is for types that support addition,
--  | multiplication, division, and modulo (division remainder) operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Remainder: `a / b * b + (a `mod` b) = a`
--  | A `Ring` where every nonzero element has a multiplicative inverse.
--  |
--  | Instances must satisfy the following law in addition to the `Ring` and
--  | `ModuloSemiring` laws:
--  |
--  | - Multiplicative inverse: `(one / x) * x = one`
--  |
--  | As a consequence of this ```a `mod` b = zero``` as no divide operation
--  | will have a remainder.
--  | The `Num` class is for types that are commutative fields.
--  |
--  | Instances must satisfy the following law in addition to the
--  | `DivisionRing` laws:
--  |
--  | - Commutative multiplication: `a * b = b * a`
--  | The `Eq` type class represents types which support decidable equality.
--  |
--  | `Eq` instances should satisfy the following laws:
--  |
--  | - Reflexivity: `x == x = true`
--  | - Symmetry: `x == y = y == x`
--  | - Transitivity: if `x == y` and `y == z` then `x == z`
--  | The `Ord` type class represents types which support comparisons with a
--  | _total order_.
--  |
--  | `Ord` instances should satisfy the laws of total orderings:
--  |
--  | - Reflexivity: `a <= a`
--  | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
--  | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
--  | The `Bounded` type class represents types that are finite.
--  |
--  | Although there are no "internal" laws for `Bounded`, every value of `a`
--  | should be considered less than or equal to `top` by some means, and greater
--  | than or equal to `bottom`.
--  |
--  | The lack of explicit `Ord` constraint allows flexibility in the use of
--  | `Bounded` so it can apply to total and partially ordered sets, boolean
--  | algebras, etc.
--  | The `BoundedOrd` type class represents totally ordered finite data types.
--  |
--  | Instances should satisfy the following law in addition to the `Ord` laws:
--  |
--  | - Ordering: `bottom <= a <= top`
--  | The `BooleanAlgebra` type class represents types that behave like boolean
--  | values.
--  |
--  | Instances should satisfy the following laws in addition to the `Bounded`
--  | laws:
--  |
--  | - Associativity:
--  |   - `a || (b || c) = (a || b) || c`
--  |   - `a && (b && c) = (a && b) && c`
--  | - Commutativity:
--  |   - `a || b = b || a`
--  |   - `a && b = b && a`
--  | - Distributivity:
--  |   - `a && (b || c) = (a && b) || (a && c)`
--  |   - `a || (b && c) = (a || b) && (a || c)`
--  | - Identity:
--  |   - `a || bottom = a`
--  |   - `a && top = a`
--  | - Idempotent:
--  |   - `a || a = a`
--  |   - `a && a = a`
--  | - Absorption:
--  |   - `a || (a && b) = a`
--  |   - `a && (a || b) = a`
--  | - Annhiliation:
--  |   - `a || top = top`
--  | - Complementation:
--  |   - `a && not a = bottom`
--  |   - `a || not a = top`
--  | The `Show` type class represents those types which can be converted into
--  | a human-readable `String` representation.
--  |
--  | While not required, it is recommended that for any expression `x`, the
--  | string `show x` be executable PureScript code which evaluates to the same
--  | value as the expression `x`.
infixr 0 $
infixl 1 #
infixr 9 >>>
infixr 9 <<<
infixl 4 <$>
infixl 1 <#>
infixl 4 <*>
infixl 1 >>=
infixr 5 <>
infixr 5 ++
infixl 6 +
infixl 7 *
infixl 6 -
infixl 7 /
infix 4 ==
infix 4 /=
infixl 4 <
infixl 4 >
infixl 4 <=
infixl 4 >=
infixr 3 &&
infixr 2 ||
--  | Applies a function to its argument.
--  |
--  | ```purescript
--  | length $ groupBy productCategory $ filter isInStock $ products
--  | ```
--  |
--  | is equivalent to:
--  |
--  | ```purescript
--  | length (groupBy productCategory (filter isInStock products))
--  | ```
--  |
--  | `($)` is different from [`(#)`](#-2) because it is right-infix instead of
--  | left: `a $ b $ c $ d x = a $ (b $ (c $ (d $ x))) = a (b (c (d x)))`
--  | Applies an argument to a function.
--  |
--  | ```purescript
--  | products # filter isInStock # groupBy productCategory # length
--  | ```
--  |
--  | is equivalent to:
--  |
--  | ```purescript
--  | length (groupBy productCategory (filter isInStock products))
--  | ```
--  |
--  | `(#)` is different from [`($)`](#-1) because it is left-infix instead of
--  | right: `x # a # b # c # d = (((x # a) # b) # c) # d = d (c (b (a x)))`
--  | The `Semiring` class is for types that support an addition and
--  | multiplication operation.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Commutative monoid under addition:
--  |   - Associativity: `(a + b) + c = a + (b + c)`
--  |   - Identity: `zero + a = a + zero = a`
--  |   - Commutative: `a + b = b + a`
--  | - Monoid under multiplication:
--  |   - Associativity: `(a * b) * c = a * (b * c)`
--  |   - Identity: `one * a = a * one = a`
--  | - Multiplication distributes over addition:
--  |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
--  |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
--  | - Annihiliation: `zero * a = a * zero = zero`
--  | `unit` is the sole inhabitant of the `Unit` type.
--  | The `Bounded` type class represents types that are finite.
--  |
--  | Although there are no "internal" laws for `Bounded`, every value of `a`
--  | should be considered less than or equal to `top` by some means, and greater
--  | than or equal to `bottom`.
--  |
--  | The lack of explicit `Ord` constraint allows flexibility in the use of
--  | `Bounded` so it can apply to total and partially ordered sets, boolean
--  | algebras, etc.
--  | The `Ring` class is for types that support addition, multiplication,
--  | and subtraction operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Additive inverse: `a - a = (zero - a) + a = zero`
--  | `(-)` is an alias for `sub`.
--  | The `Show` type class represents those types which can be converted into
--  | a human-readable `String` representation.
--  |
--  | While not required, it is recommended that for any expression `x`, the
--  | string `show x` be executable PureScript code which evaluates to the same
--  | value as the expression `x`.
--  | The `Applicative` type class extends the [`Apply`](#apply) type class
--  | with a `pure` function, which can be used to create values of type `f a`
--  | from values of type `a`.
--  |
--  | Where [`Apply`](#apply) provides the ability to lift functions of two or
--  | more arguments to functions whose arguments are wrapped using `f`, and
--  | [`Functor`](#functor) provides the ability to lift functions of one
--  | argument, `pure` can be seen as the function which lifts functions of
--  | _zero_ arguments. That is, `Applicative` functors support a lifting
--  | operation for any number of function arguments.
--  |
--  | Instances must satisfy the following laws in addition to the `Apply`
--  | laws:
--  |
--  | - Identity: `(pure id) <*> v = v`
--  | - Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
--  | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
--  | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
--  | `return` is an alias for `pure`.
--  | An alias for `true`, which can be useful in guard clauses:
--  |
--  | ```purescript
--  | max x y | x >= y    = x
--  |         | otherwise = y
--  | ```
--  | The `Semiring` class is for types that support an addition and
--  | multiplication operation.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Commutative monoid under addition:
--  |   - Associativity: `(a + b) + c = a + (b + c)`
--  |   - Identity: `zero + a = a + zero = a`
--  |   - Commutative: `a + b = b + a`
--  | - Monoid under multiplication:
--  |   - Associativity: `(a * b) * c = a * (b * c)`
--  |   - Identity: `one * a = a * one = a`
--  | - Multiplication distributes over addition:
--  |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
--  |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
--  | - Annihiliation: `zero * a = a * zero = zero`
--  | The `BooleanAlgebra` type class represents types that behave like boolean
--  | values.
--  |
--  | Instances should satisfy the following laws in addition to the `Bounded`
--  | laws:
--  |
--  | - Associativity:
--  |   - `a || (b || c) = (a || b) || c`
--  |   - `a && (b && c) = (a && b) && c`
--  | - Commutativity:
--  |   - `a || b = b || a`
--  |   - `a && b = b && a`
--  | - Distributivity:
--  |   - `a && (b || c) = (a && b) || (a && c)`
--  |   - `a || (b && c) = (a || b) && (a || c)`
--  | - Identity:
--  |   - `a || bottom = a`
--  |   - `a && top = a`
--  | - Idempotent:
--  |   - `a || a = a`
--  |   - `a && a = a`
--  | - Absorption:
--  |   - `a || (a && b) = a`
--  |   - `a && (a || b) = a`
--  | - Annhiliation:
--  |   - `a || top = top`
--  | - Complementation:
--  |   - `a && not a = bottom`
--  |   - `a || not a = top`
--  | `negate x` can be used as a shorthand for `zero - x`.
--  | The `Semiring` class is for types that support an addition and
--  | multiplication operation.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Commutative monoid under addition:
--  |   - Associativity: `(a + b) + c = a + (b + c)`
--  |   - Identity: `zero + a = a + zero = a`
--  |   - Commutative: `a + b = b + a`
--  | - Monoid under multiplication:
--  |   - Associativity: `(a * b) * c = a * (b * c)`
--  |   - Identity: `one * a = a * one = a`
--  | - Multiplication distributes over addition:
--  |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
--  |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
--  | - Annihiliation: `zero * a = a * zero = zero`
--  | `(*)` is an alias for `mul`.
--  | The `ModuloSemiring` class is for types that support addition,
--  | multiplication, division, and modulo (division remainder) operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Remainder: `a / b * b + (a `mod` b) = a`
--  | A `Functor` is a type constructor which supports a mapping operation
--  | `(<$>)`.
--  |
--  | `(<$>)` can be used to turn functions `a -> b` into functions
--  | `f a -> f b` whose argument and return types use the type constructor `f`
--  | to represent some computational context.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Identity: `(<$>) id = id`
--  | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
--  | `(<$>)` is an alias for `map`
--  | `(<#>)` is `(<$>)` with its arguments reversed. For example:
--  |
--  | ```purescript
--  | [1, 2, 3] <#> \n -> n * n
--  | ```
--  | `Category`s consist of objects and composable morphisms between them, and
--  | as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
--  | must have an identity element.
--  |
--  | Instances must satisfy the following law in addition to the
--  | `Semigroupoid` law:
--  |
--  | - Identity: `id <<< p = p <<< id = p`
--  | Flips the order of the arguments to a function of two arguments.
--  |
--  | ```purescript
--  | flip const 1 2 = const 2 1 = 2
--  | ```
--  | The `Eq` type class represents types which support decidable equality.
--  |
--  | `Eq` instances should satisfy the following laws:
--  |
--  | - Reflexivity: `x == x = true`
--  | - Symmetry: `x == y = y == x`
--  | - Transitivity: if `x == y` and `y == z` then `x == z`
--  | `(==)` is an alias for `eq`. Tests whether one value is equal to another.
--  | The `ModuloSemiring` class is for types that support addition,
--  | multiplication, division, and modulo (division remainder) operations.
--  |
--  | Instances must satisfy the following law in addition to the `Semiring`
--  | laws:
--  |
--  | - Remainder: `a / b * b + (a `mod` b) = a`
--  | `(/)` is an alias for `div`.
--  | The `BooleanAlgebra` type class represents types that behave like boolean
--  | values.
--  |
--  | Instances should satisfy the following laws in addition to the `Bounded`
--  | laws:
--  |
--  | - Associativity:
--  |   - `a || (b || c) = (a || b) || c`
--  |   - `a && (b && c) = (a && b) && c`
--  | - Commutativity:
--  |   - `a || b = b || a`
--  |   - `a && b = b && a`
--  | - Distributivity:
--  |   - `a && (b || c) = (a && b) || (a && c)`
--  |   - `a || (b && c) = (a || b) && (a || c)`
--  | - Identity:
--  |   - `a || bottom = a`
--  |   - `a && top = a`
--  | - Idempotent:
--  |   - `a || a = a`
--  |   - `a && a = a`
--  | - Absorption:
--  |   - `a || (a && b) = a`
--  |   - `a && (a || b) = a`
--  | - Annhiliation:
--  |   - `a || top = top`
--  | - Complementation:
--  |   - `a && not a = bottom`
--  |   - `a || not a = top`
--  | `(||)` is an alias for `disj`.
--  | Returns its first argument and ignores its second.
--  |
--  | ```purescript
--  | const 1 "hello" = 1
--  | ```
--  | The `void` function is used to ignore the type wrapped by a
--  | [`Functor`](#functor), replacing it with `Unit` and keeping only the type
--  | information provided by the type constructor itself.
--  |
--  | `void` is often useful when using `do` notation to change the return type
--  | of a monadic computation:
--  |
--  | ```purescript
--  | main = forE 1 10 \n -> void do
--  |   print n
--  |   print (n * n)
--  | ```
--  | The `BooleanAlgebra` type class represents types that behave like boolean
--  | values.
--  |
--  | Instances should satisfy the following laws in addition to the `Bounded`
--  | laws:
--  |
--  | - Associativity:
--  |   - `a || (b || c) = (a || b) || c`
--  |   - `a && (b && c) = (a && b) && c`
--  | - Commutativity:
--  |   - `a || b = b || a`
--  |   - `a && b = b && a`
--  | - Distributivity:
--  |   - `a && (b || c) = (a && b) || (a && c)`
--  |   - `a || (b && c) = (a || b) && (a || c)`
--  | - Identity:
--  |   - `a || bottom = a`
--  |   - `a && top = a`
--  | - Idempotent:
--  |   - `a || a = a`
--  |   - `a && a = a`
--  | - Absorption:
--  |   - `a || (a && b) = a`
--  |   - `a && (a || b) = a`
--  | - Annhiliation:
--  |   - `a || top = top`
--  | - Complementation:
--  |   - `a && not a = bottom`
--  |   - `a || not a = top`
--  | `(&&)` is an alias for `conj`.
--  | A `Semigroupoid` is similar to a [`Category`](#category) but does not
--  | require an identity element `id`, just composable morphisms.
--  |
--  | `Semigroupoid`s must satisfy the following law:
--  |
--  | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
--  |
--  | One example of a `Semigroupoid` is the function type constructor `(->)`,
--  | with `(<<<)` defined as function composition.
--  | `(<<<)` is an alias for `compose`.
--  | Forwards composition, or `(<<<)` with its arguments reversed.
--  | The `Ord` type class represents types which support comparisons with a
--  | _total order_.
--  |
--  | `Ord` instances should satisfy the laws of total orderings:
--  |
--  | - Reflexivity: `a <= a`
--  | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
--  | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
--  | Test whether one value is _strictly less than_ another.
--  | Test whether one value is _non-strictly less than_ another.
--  | Test whether one value is _strictly greater than_ another.
--  | Test whether one value is _non-strictly greater than_ another.
--  | The `Bounded` type class represents types that are finite.
--  |
--  | Although there are no "internal" laws for `Bounded`, every value of `a`
--  | should be considered less than or equal to `top` by some means, and greater
--  | than or equal to `bottom`.
--  |
--  | The lack of explicit `Ord` constraint allows flexibility in the use of
--  | `Bounded` so it can apply to total and partially ordered sets, boolean
--  | algebras, etc.
--  | `(/=)` tests whether one value is _not equal_ to another. Shorthand for
--  | `not (x == y)`.
--  | The `Bind` type class extends the [`Apply`](#apply) type class with a
--  | "bind" operation `(>>=)` which composes computations in sequence, using
--  | the return value of one computation to determine the next computation.
--  |
--  | The `>>=` operator can also be expressed using `do` notation, as follows:
--  |
--  | ```purescript
--  | x >>= f = do y <- x
--  |              f y
--  | ```
--  |
--  | where the function argument of `f` is given the name `y`.
--  |
--  | Instances must satisfy the following law in addition to the `Apply`
--  | laws:
--  |
--  | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
--  |
--  | Associativity tells us that we can regroup operations which use `do`
--  | notation so that we can unambiguously write, for example:
--  |
--  | ```purescript
--  | do x <- m1
--  |    y <- m2 x
--  |    m3 x y
--  | ```
--  | `liftM1` provides a default implementation of `(<$>)` for any
--  | [`Monad`](#monad), without using `(<$>)` as provided by the
--  | [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.
--  |
--  | `liftM1` can therefore be used to write [`Functor`](#functor) instances
--  | as follows:
--  |
--  | ```purescript
--  | instance functorF :: Functor F where
--  |   map = liftM1
--  | ```
--  | `(>>=)` is an alias for `bind`.
--  | This function returns its first argument, and can be used to assert type
--  | equalities. This can be useful when types are otherwise ambiguous.
--  |
--  | ```purescript
--  | main = print $ [] `asTypeOf` [0]
--  | ```
--  |
--  | If instead, we had written `main = print []`, the type of the argument
--  | `[]` would have been ambiguous, resulting in a compile-time error.
--  | The `Apply` class provides the `(<*>)` which is used to apply a function
--  | to an argument under a type constructor.
--  |
--  | `Apply` can be used to lift functions of two or more arguments to work on
--  | values wrapped with the type constructor `f`. It might also be understood
--  | in terms of the `lift2` function:
--  |
--  | ```purescript
--  | lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
--  | lift2 f a b = f <$> a <*> b
--  | ```
--  |
--  | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
--  | the function application operator `($)` to arguments wrapped with the
--  | type constructor `f`.
--  |
--  | Instances must satisfy the following law in addition to the `Functor`
--  | laws:
--  |
--  | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
--  |
--  | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
--  | `(<*>)` is an alias for `apply`.
--  | `liftA1` provides a default implementation of `(<$>)` for any
--  | [`Applicative`](#applicative) functor, without using `(<$>)` as provided
--  | by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
--  | relationship.
--  |
--  | `liftA1` can therefore be used to write [`Functor`](#functor) instances
--  | as follows:
--  |
--  | ```purescript
--  | instance functorF :: Functor F where
--  |   map = liftA1
--  | ```
--  | The `Semigroup` type class identifies an associative operation on a type.
--  |
--  | Instances are required to satisfy the following law:
--  |
--  | - Associativity: `(x <> y) <> z = x <> (y <> z)`
--  |
--  | One example of a `Semigroup` is `String`, with `(<>)` defined as string
--  | concatenation.
--  | `(++)` is an alternative alias for `append`.
--  | `(<>)` is an alias for `append`.
--  | `ap` provides a default implementation of `(<*>)` for any
--  | [`Monad`](#monad), without using `(<*>)` as provided by the
--  | [`Apply`](#apply)-[`Monad`](#monad) superclass relationship.
--  |
--  | `ap` can therefore be used to write [`Apply`](#apply) instances as
--  | follows:
--  |
--  | ```purescript
--  | instance applyF :: Apply F where
--  |   apply = ap
--  | ```
--  | The `Semiring` class is for types that support an addition and
--  | multiplication operation.
--  |
--  | Instances must satisfy the following laws:
--  |
--  | - Commutative monoid under addition:
--  |   - Associativity: `(a + b) + c = a + (b + c)`
--  |   - Identity: `zero + a = a + zero = a`
--  |   - Commutative: `a + b = b + a`
--  | - Monoid under multiplication:
--  |   - Associativity: `(a * b) * c = a * (b * c)`
--  |   - Identity: `one * a = a * one = a`
--  | - Multiplication distributes over addition:
--  |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
--  |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
--  | - Annihiliation: `zero * a = a * zero = zero`
--  | `(+)` is an alias for `add`.
data Ordering = LT  | GT  | EQ 
data Unit
class Show a where
  show :: a -> Prim.String
class (Prelude.Bounded a) <= BooleanAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a
class (Prelude.Bounded a, Prelude.Ord a) <= BoundedOrd a where
class Bounded a where
  top :: a
  bottom :: a
class (Prelude.Eq a) <= Ord a where
  compare :: a -> a -> Prelude.Ordering
class Eq a where
  eq :: a -> a -> Prim.Boolean
class (Prelude.Ring a, Prelude.ModuloSemiring a) <= DivisionRing a where
class (Prelude.DivisionRing a) <= Num a where
class (Prelude.Semiring a) <= Ring a where
  sub :: a -> a -> a
class (Prelude.Semiring a) <= ModuloSemiring a where
  div :: a -> a -> a
  mod :: a -> a -> a
class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a
class Semigroup a where
  append :: a -> a -> a
class (Prelude.Applicative m, Prelude.Bind m) <= Monad m where
class (Prelude.Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b
class (Prelude.Apply f) <= Applicative f where
  pure :: forall a. a -> f a
class (Prelude.Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
class (Prelude.Semigroupoid a) <= Category a where
  id :: forall t. a t t
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d
foreign import (||) :: forall a. (Prelude.BooleanAlgebra a) => a -> a -> a
foreign import (&&) :: forall a. (Prelude.BooleanAlgebra a) => a -> a -> a
foreign import (>=) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (<=) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (>) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (<) :: forall a. (Prelude.Ord a) => a -> a -> Prim.Boolean
foreign import (/=) :: forall a. (Prelude.Eq a) => a -> a -> Prim.Boolean
foreign import (==) :: forall a. (Prelude.Eq a) => a -> a -> Prim.Boolean
foreign import (-) :: forall a. (Prelude.Ring a) => a -> a -> a
foreign import negate :: forall a. (Prelude.Ring a) => a -> a
foreign import (/) :: forall a. (Prelude.ModuloSemiring a) => a -> a -> a
foreign import (*) :: forall a. (Prelude.Semiring a) => a -> a -> a
foreign import (+) :: forall a. (Prelude.Semiring a) => a -> a -> a
foreign import (++) :: forall s. (Prelude.Semigroup s) => s -> s -> s
foreign import (<>) :: forall s. (Prelude.Semigroup s) => s -> s -> s
foreign import ap :: forall m a b. (Prelude.Monad m) => m (a -> b) -> m a -> m b
foreign import liftM1 :: forall m a b. (Prelude.Monad m) => (a -> b) -> m a -> m b
foreign import return :: forall m a. (Prelude.Applicative m) => a -> m a
foreign import (>>=) :: forall m a b. (Prelude.Bind m) => m a -> (a -> m b) -> m b
foreign import liftA1 :: forall f a b. (Prelude.Applicative f) => (a -> b) -> f a -> f b
foreign import (<*>) :: forall f a b. (Prelude.Apply f) => f (a -> b) -> f a -> f b
foreign import void :: forall f a. (Prelude.Functor f) => f a -> f Prelude.Unit
foreign import (<#>) :: forall f a b. (Prelude.Functor f) => f a -> (a -> b) -> f b
foreign import (<$>) :: forall f a b. (Prelude.Functor f) => (a -> b) -> f a -> f b
foreign import (>>>) :: forall a b c d. (Prelude.Semigroupoid a) => a b c -> a c d -> a b d
foreign import (<<<) :: forall a b c d. (Prelude.Semigroupoid a) => a c d -> a b c -> a b d
foreign import otherwise :: Prim.Boolean
foreign import asTypeOf :: forall a. a -> a -> a
foreign import const :: forall a b. a -> b -> a
foreign import flip :: forall a b c. (a -> b -> c) -> b -> a -> c
foreign import (#) :: forall a b. a -> (a -> b) -> b
foreign import ($) :: forall a b. (a -> b) -> a -> b
foreign import unit :: Prelude.Unit
foreign import instance semigroupoidFn :: Prelude.Semigroupoid Prim.Function
foreign import instance categoryFn :: Prelude.Category Prim.Function
foreign import instance functorFn :: Prelude.Functor (Prim.Function r)
foreign import instance functorArray :: Prelude.Functor Prim.Array
foreign import instance applyFn :: Prelude.Apply (Prim.Function r)
foreign import instance applyArray :: Prelude.Apply Prim.Array
foreign import instance applicativeFn :: Prelude.Applicative (Prim.Function r)
foreign import instance applicativeArray :: Prelude.Applicative Prim.Array
foreign import instance bindFn :: Prelude.Bind (Prim.Function r)
foreign import instance bindArray :: Prelude.Bind Prim.Array
foreign import instance monadFn :: Prelude.Monad (Prim.Function r)
foreign import instance monadArray :: Prelude.Monad Prim.Array
foreign import instance semigroupString :: Prelude.Semigroup Prim.String
foreign import instance semigroupUnit :: Prelude.Semigroup Prelude.Unit
foreign import instance semigroupFn :: (Prelude.Semigroup s') => Prelude.Semigroup (s -> s')
foreign import instance semigroupOrdering :: Prelude.Semigroup Prelude.Ordering
foreign import instance semigroupArray :: Prelude.Semigroup (Prim.Array a)
foreign import instance semiringInt :: Prelude.Semiring Prim.Int
foreign import instance semiringNumber :: Prelude.Semiring Prim.Number
foreign import instance semiringUnit :: Prelude.Semiring Prelude.Unit
foreign import instance ringInt :: Prelude.Ring Prim.Int
foreign import instance ringNumber :: Prelude.Ring Prim.Number
foreign import instance ringUnit :: Prelude.Ring Prelude.Unit
foreign import instance moduloSemiringInt :: Prelude.ModuloSemiring Prim.Int
foreign import instance moduloSemiringNumber :: Prelude.ModuloSemiring Prim.Number
foreign import instance moduloSemiringUnit :: Prelude.ModuloSemiring Prelude.Unit
foreign import instance divisionRingNumber :: Prelude.DivisionRing Prim.Number
foreign import instance divisionRingUnit :: Prelude.DivisionRing Prelude.Unit
foreign import instance numNumber :: Prelude.Num Prim.Number
foreign import instance numUnit :: Prelude.Num Prelude.Unit
foreign import instance eqBoolean :: Prelude.Eq Prim.Boolean
foreign import instance eqInt :: Prelude.Eq Prim.Int
foreign import instance eqNumber :: Prelude.Eq Prim.Number
foreign import instance eqChar :: Prelude.Eq Prim.Char
foreign import instance eqString :: Prelude.Eq Prim.String
foreign import instance eqUnit :: Prelude.Eq Prelude.Unit
foreign import instance eqArray :: (Prelude.Eq a) => Prelude.Eq (Prim.Array a)
foreign import instance eqOrdering :: Prelude.Eq Prelude.Ordering
foreign import instance ordBoolean :: Prelude.Ord Prim.Boolean
foreign import instance ordInt :: Prelude.Ord Prim.Int
foreign import instance ordNumber :: Prelude.Ord Prim.Number
foreign import instance ordString :: Prelude.Ord Prim.String
foreign import instance ordChar :: Prelude.Ord Prim.Char
foreign import instance ordUnit :: Prelude.Ord Prelude.Unit
foreign import instance ordArray :: (Prelude.Ord a) => Prelude.Ord (Prim.Array a)
foreign import instance ordOrdering :: Prelude.Ord Prelude.Ordering
foreign import instance boundedBoolean :: Prelude.Bounded Prim.Boolean
foreign import instance boundedUnit :: Prelude.Bounded Prelude.Unit
foreign import instance boundedOrdering :: Prelude.Bounded Prelude.Ordering
foreign import instance boundedInt :: Prelude.Bounded Prim.Int
foreign import instance boundedFn :: (Prelude.Bounded b) => Prelude.Bounded (a -> b)
foreign import instance boundedOrdBoolean :: Prelude.BoundedOrd Prim.Boolean
foreign import instance boundedOrdUnit :: Prelude.BoundedOrd Prelude.Unit
foreign import instance boundedOrdOrdering :: Prelude.BoundedOrd Prelude.Ordering
foreign import instance boundedOrdInt :: Prelude.BoundedOrd Prim.Int
foreign import instance booleanAlgebraBoolean :: Prelude.BooleanAlgebra Prim.Boolean
foreign import instance booleanAlgebraUnit :: Prelude.BooleanAlgebra Prelude.Unit
foreign import instance booleanAlgebraFn :: (Prelude.BooleanAlgebra b) => Prelude.BooleanAlgebra (a -> b)
foreign import instance showBoolean :: Prelude.Show Prim.Boolean
foreign import instance showInt :: Prelude.Show Prim.Int
foreign import instance showNumber :: Prelude.Show Prim.Number
foreign import instance showChar :: Prelude.Show Prim.Char
foreign import instance showString :: Prelude.Show Prim.String
foreign import instance showUnit :: Prelude.Show Prelude.Unit
foreign import instance showArray :: (Prelude.Show a) => Prelude.Show (Prim.Array a)
foreign import instance showOrdering :: Prelude.Show Prelude.Ordering