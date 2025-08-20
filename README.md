# fpm-deps

*Generate dependency graphs of fpm packages*

To start using `fpm-deps` run the commands:

```
git clone https://github.com/ivan-pi/fpm-deps && cd fpm-deps   # clone project
fpm install --prefix $HOME/.local    # install in a directory on the user PATH
fpm-deps   # run fpm-deps in the root directory of an fpm project
```

The commands above will install the `fpm-deps` executable into the `$HOME/.local/bin` folder, assuming it is included in your shell's `PATH`.

## Graphviz

The result can be piped to the Graphviz [`dot`](https://graphviz.org/doc/info/command.html) command, e.g.:

```
fpm-deps | dot -Tsvg -ofpmdeps.svg
```
resulting in

![fpmdeps package dependency graph](./fpmdeps.svg)

## Mermaid

To generate a [Mermaid](https://mermaid.js.org/) dependency graph you can include in your Markdown documents (either Github- or Gitlab-flavoured markdown) use
```
fpm-deps --mermaid -o depgraph.mmd
```

Alternatively, you can output a standalone HTML page:
```
fpm-deps --mermaid html -o depgraph.html
```

The rendered output can be seen [here](https://ivan-pi.github.io/fpm-deps/depgraph.html).

## See also

For visualizing Fortran *module dependency graphs*, check out [**fpm-modules**](https://github.com/davidpfister/fpm-modules).

Similar projects for other programming languages:

- [cargo-depgraph](https://crates.io/crates/cargo-depgraph/)
- [cargo-deps](https://crates.io/crates/cargo-deps)
- [PkgGraph.jl](https://github.com/tfiers/PkgGraph.jl)
- [pipdeptree](https://pypi.org/project/pipdeptree/)
- [conda-tree](https://github.com/conda-incubator/conda-tree)


## Acknowledgments

Thanks to [@vmagnin](https://github.com/vmagnin) for early testing via the fpm plugin mechanism and for pointing me toward the existing cargo crates.

## ToDo list

* browse option (see [`browse`](https://ninja-build.org/manual.html#_extra_tools) in ninja)
* use style/colors to distinguish dependencies (local, git, registry)
* customize nodes with other package information
* offline mode (use only cached data)
* library dependencies (link-time)
* subgraph selection based on category/authors/...
