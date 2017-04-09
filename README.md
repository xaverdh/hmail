
## Building

```sh
git clone https://gitlab.com/xaverdh/hmail
cd hmail
git clone https://gitlab.com/xaverdh/dtypes
git clone https://gitlab.com/xaverdh/dtypes-extra
git clone -b faster-normalizeCRLF https://gitlab.com/xaverdh/mime
git clone -b patchwork https://gitlab.com/xaverdh/HaskellNet
cabal sandbox init
cabal sandbox add-source dtypes dtypes-extra mime HaskellNet
cabal install --ghc-option=-threaded
```

