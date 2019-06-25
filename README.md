
## Building

```sh
git clone https://gitlab.com/xaverdh/hmail
cd hmail
git clone https://gitlab.com/xaverdh/dtypes
git clone https://gitlab.com/xaverdh/dtypes-extra
git clone -b patchwork https://gitlab.com/xaverdh/HaskellNet
cabal sandbox init
cabal sandbox add-source dtypes dtypes-extra HaskellNet
cabal install --ghc-option=-threaded
```

There is a [build script][hmail-build-script] automating this.

# Building with nix

Run `nix-build` in the top level directory of this repository.
To build a specific commit / ref: `nix-build --argstr ref <commit-hash>`


[hmail-build-script]: https://gitlab.com/xaverdh/hmail/blob/master/build
