# SimFormat

Formats Haskell import lists.

It produces import lists that look like this:

```haskell
import Range.Shared.Types
  ( ExternalEntities, ExternalRef(..), Imports(Imports), IsSpecEntity, ResolvedSpec
  , SomeExternalRef(SomeExternalRef), SpecEntities, SpecEntity(..)
  , SpecRef(SpecRefExternal, SpecRefLocal), VmRepetitionGroup, VmRgTag, importsConfigModuleDefs
  , importsOperatingSystems, importsVmTemplates, specEntitiesConfigModuleDefs
  , specEntitiesOperatingSystems, specEntitiesVmTemplates, specEntity, specificationEntities
  )
```

## Usage

SimFormat reads from stdin and writes to stdout.

```
$ stack install
```

From within Vim, e.g., visual select your code and then run `:!simformat`
