# SimFormat

[![Build Status](https://travis-ci.com/simspace/simformat.svg?branch=master)](https://travis-ci.com/simspace/simformat)

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

### Emacs

From within Emacs, assuming you have the `haskell-mode` and `stylish-haskell` packages installed and working, `(setq
haskell-mode-stylish-haskell-path "simformat -e")` and `(setq haskell-stylish-on-save t)` should automatically reformat
your import list on every save.

### Vim

One possible vim solution is to add this to your `.vimrc` file:

```
function! s:RunSimformat()
  let cmd = 'simformat -e'
  let stdin = join(getline(1, '$'), "\n")
  let output = system(cmd, stdin)
  if v:shell_error != 0
    echom output
  else
    call s:OverwriteBuffer(output)
  endif
endfunction

function! s:OverwriteBuffer(output)
  if &modifiable
    let l:curw=winsaveview()
    try | silent undojoin | catch | endtry
    let splitted = split(a:output, '\n')
    if line('$') > len(splitted)
      execute len(splitted) .',$delete'
    endif
    call setline(1, splitted)
    call winrestview(l:curw)
  else
    echom "Cannot write to non-modifiable buffer"
  endif
endfunction

augroup Simformat
  autocmd!
  autocmd BufWritePre * call <SID>RunSimformat()
augroup END
```

If you prefer not to run code on save, you can use `shell-command-on-region` with a region active and the prefix
argument set, i.e. `C-u M-| simformat`

From within Vim, run `:!simformat`

### VSCode

Add this to `~/Library/Application Support/Code/User/settings.json` once you have installed the [Run on Save](https://github.com/pucelle/vscode-run-on-save) extension.

```json
    "runOnSave.statusMessageTimeout": 3000,
    "runOnSave.commands": [
        {
            "match": ".*\\.hs$",
            "notMatch": "[\\\\\\/]_[^\\\\\\/]*\\.hs$",
            "command": "simformat -i ${file}",
            "runIn": "backend",
            "runningStatusMessage": "Formatting ${fileBasename}",
            "finishStatusMessage": "${fileBasename} formatted"
        }
    ]
```

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
