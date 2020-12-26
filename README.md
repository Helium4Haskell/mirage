# Mirage

This is the git repository of Mirage, an attribute grammar visualizer for UUAGC
attribute grammars.

To install simply clone this repository and run:

```
$ cabal install
```

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

For more information I invite you to look at the internship report in `report/report.pdf`.
It should give an overview of the functionality and notes about the implementation.

If you want to develop this further you can look at the future work section of
that report and I have also made a brief list of todos in `TODO.md`.

Have fun!
