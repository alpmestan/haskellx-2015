Slides and code for the *servant* talk at the Haskell eXchange in London, 2015 edition.

The slides are available in markdown and HTML under `slides/`.

### Building the code

``` bash
$ cabal sandbox init
$ cabal install --dep
$ cabal build
# binaries available under dist/build/ after a while

# You'll need to run this if you want to launch Soundskell
bash createdb.sh
```

A `stack.yml` file is included as well.