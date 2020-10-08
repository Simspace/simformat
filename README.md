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

By default, `simformat` formats an entire git repository using a `.simformatrc` config file in the repo root to identify
files to include. To bypass this behavior you can call it with `-i <file>` which will format the file regardless of
whether it's in the configuration. It should always succeed: if it cannot find any import blocks, it will simply pass
the text on unchanged. You can always run `simformat --help` for more command-line options.

```
$ stack install
```

From within Emacs, assuming you have the `haskell-mode` and `stylish-haskell` packages installed and working, `(setq
haskell-mode-stylish-haskell-path "simformat")` and `(setq haskell-stylish-on-save t)` should automatically reformat
your import list on every save.

One possible vim solution is to add this to your `.vimrc` file:
```
function! Write()
  if &filetype == "haskell"
    let l:pos=getpos(".")
    exe "%!simformat"
    call setpos(".", l:pos)
    write
  else
    write
  endif
endfunc
map :w<cr> :call Write()<cr>
```

If you prefer not to run code on save, you can use `shell-command-on-region` with a region active and the prefix
argument set, i.e. `C-u M-| simformat`

From within Vim, run `:!simformat`

## .simformatrc example

The config file, `.simformatrc`, is expected to live in the directory where the `simformat` executable is being run.
It's a YAML file containing two keys: `files` and `whitelist`, each with a list of filepaths. If `.simformatrc` is not
present, it will interpret `files` to be all files, and `whitelist` to be empty.

```yaml
files:
  - foo
whitelist:
  - foo/bar
```
