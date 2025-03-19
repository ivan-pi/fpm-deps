# fpmdeps

Generate dependency graphs of fpm projects

To start using `fpmdeps` run the commands:

```
> git clone ... && cd fpmdeps   # clone project
> fpm install --prefix ~/.local    # install in directory on the user PATH
> fpmdeps  # run fpmdeps in the root directory of an fpm project
```

## Acknowledgments

Thanks to @vmagnin for early testing via the fpm plugin mechanism and for pointing me toward the cargo dependency graph crates.

## See also

- [cargo-depgraph](https://crates.io/crates/cargo-depgraph/)
- [cargo-deps](https://crates.io/crates/cargo-deps)
- [PkgGraph.jl](https://github.com/tfiers/PkgGraph.jl)
- [pipdeptree](https://pypi.org/project/pipdeptree/)
- [conda-tree](https://github.com/conda-incubator/conda-tree)
