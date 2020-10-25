# Orelude

An alternative prelude which is aimed at:

- Exporting monad transformers from mtl.
    - Cont, Reader, Writer
- Safe
    - It doesn't contain any partial functions.
        - It exports safe alternatives instead.
    - It uses the safe-exceptions package for error handling.
- Flexible
    - It replaces MonadIO functions with IO functions.
    - It uses MonadThrow for error handling instead of Maybe or Either.
- LightWeight
    - It has a small amount of dependencies.

## List of the re-exported modules.

```Haskell
Control.Applicative
Control.Arrow
Control.Category
Control.Exception.Safe
Control.Monad
Control.Monad.Cont
Control.Monad.IO.Class
Control.Monad.Reader
Control.Monad.State
Control.Monad.Trans
Control.Monad.Writer
Data.Either
Data.Functor
Data.Function
Data.Foldable
Data.Maybe
Data.Traversable
```
