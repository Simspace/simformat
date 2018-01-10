# SimFormat

Formats Haskell import lists.

It produces import lists that look like this:

```haskell
import Range.Shared.Types
  ( ExternalRef(..), Imports(Imports), SomeExternalRef(SomeExternalRef), SpecEntity(..)
  , SpecRef(SpecRefExternal, SpecRefLocal), ExternalEntities, IsSpecEntity, ResolvedSpec
  , SpecEntities, VmRepetitionGroup, VmRgTag, importsConfigModuleDefs, importsOperatingSystems
  , importsVmTemplates, specEntitiesConfigModuleDefs, specEntitiesOperatingSystems
  , specEntitiesVmTemplates, specEntity, specificationEntities
  )
```

## Usage

SimFormat reads from stdin and writes to stdout. It should always
succeed: if it cannot find any import blocks, it will simply pass the
text on unchanged.

```
$ stack install
```

From within Emacs, assuming you have the `haskell-mode` and
`stylish-haskell` packages installed and working, `(setq
haskell-mode-stylish-haskell-path "simformat")` and `(setq
haskell-stylish-on-save t)` should automatically reformat your import
list on every save. This is completely impossible in Vim, and @asivitz
should by no means take this as a personal challenge.

From within Vim, e.g., visual select your code (or the whole file) and
then run `:!simformat`
