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

### Emacs

From within Emacs, assuming you have the `haskell-mode` and
`stylish-haskell` packages installed and working, `(setq
haskell-mode-stylish-haskell-path "simformat")` and `(setq
haskell-stylish-on-save t)` should automatically reformat your import
list on every save. This is completely impossible in Vim, and @asivitz
should by no means take this as a personal challenge.

If you prefer not to run code on save, you can use
`shell-command-on-region` with a region active and the prefix argument
set, i.e. `C-u M-| simformat`

### Vim

From within Vim, e.g., visual select your code (or the whole file) and
then run `:!simformat`

### Visual Studio Code

After installing the `simformat` executable using `stack`, link the `simformat-file` wrapper to somewhere on your path.

For example:

```
ln -s ~/dev/simspace/simformat/simformat-file ~/.local/bin/simformat-file
```

(Make sure to use absolute paths when using `ln`.)

Then in your project directory, create a `.vscode/tasks.json` file and add the contents of the `vscode-tasks.json` file checked in this repo.

Finally, with a Haskell file open in VS Code, hit `Shift+Cmd+B`, and select `simformat` from the build task list. It will format the opened file in-place.
