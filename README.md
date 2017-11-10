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

SimFormat reads from stdin and writes to stdout.

```
$ stack install
```

From within Vim, e.g., visual select your code and then run `:!simformat`
