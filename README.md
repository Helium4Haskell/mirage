# Mirage

This is the git repository of Mirage, an attribute grammar visualizer for UUAGC
attribute grammars.

To install simply clone this repository and run:

```
$ cabal install
```

This might require you to install the development package of gtk3 first, you can
find more information about that in [the the installation instructions of haskell-gi](https://github.com/haskell-gi/haskell-gi#installation).
It has instructions for Fedora, Debian / Ubuntu, Arch Linux, Mac OSX and Windows. 

In addition, the `cairo` library also needs to be installed, which can be done with `apt install libcairo2-dev` on Debian / Ubuntu and `brew install cairo` on Mac. 

This cabal package currently needs to be built with [this custom mirage branch of UUAGC](https://github.com/noughtmare/uuagc/tree/mirage).
But retrieving that should be handled automatically by cabal.

To obtain `.mirage` files to visualize, however, you need to install the UUAGC
version in that custom branch using the following procedure:

```
$ cd /tmp
$ git clone https://github.com/noughtmare/uuagc --branch mirage --depth 1
$ cd uuagc/uuagc/trunk
$ cabal install --overwrite-policy=always
```

Then you can generate mirage files of `.ag` files by adding the `--genmirage`
flag to the invocation of uuagc. E.g.

```
$ uuagc Test.ag --genmirage
```

You can also add the `genmirage` option in the `uuagc_options` file, e.g.

```
file: "src/Test.ag"
options: genmirage
```

Either of these options will cause a `Test.hs.mirage` file to be generated (you
may have to look in dist/ or dist-newstyle/). These files can be opened with
mirage by starting mirage and then selecting 'File -> Open' in the top left of
the window. After selecting your .mirage file you should be able to select
nonterminals and production rules in the sidebar.

For more information I invite you to look at the internship report in `report/report.pdf`.
It should give an overview of the functionality and notes about the implementation.

If you want to develop this further you can look at the future work section of
that report and I have also made a brief list of todos in `TODO.md`.

Have fun!
