.PHONY: testwatch

# a bit unfortunate that we have to recapitulate doctests.hs here,
# but the target seems to need to be :simformat, otherwise it doesn't
# update on changes (which is correct behaviour, really: :simformat-doctests
# does not directly depend on main.hs. If you find a better way of doing
# this, please replace this hack!
testwatch:
	ghcid -T ':m +Test.DocTest' -T 'doctest ["-XRecordWildCards", "main.hs"]' -c "stack ghci :simformat --package doctest"


