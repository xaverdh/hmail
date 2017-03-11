
## Building

```sh
git clone https://gitlab.com/xaverdh/hmail
cd hmail
git clone https://gitlab.com/xaverdh/dtypes
git clone https://gitlab.com/xaverdh/dtypes-extra
cabal sandbox init
cabal sandbox add-source dtypes dtypes-extra
cabal install --ghc-option=-threaded
```

