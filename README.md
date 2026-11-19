# SmallTheories
Exploring small theory (presentations) over different vocabularies


## Dependencies

Make sure Ocaml is installed by following the instructions listed here:
[ocaml](https://ocaml.org/)

Make sure to install `dune`, which is the build system we are using.
We rely on the `Base` library ``

## Running the code
Go inside `generators` and run `dune build`, this creates a `_build`
folder.

To run type
```dune exec ./main.exe -- 5``` 
to execute with size `5` as input, in general the format is
```dune exec ./main.exe -- <size | default 9>``` 
Where 9 is the default size of printed theories

To run on `utop` load `utop` directly inside the directory, it is configured to add the necessary imports and run the examples interactively
see `.ocamlinit` for what's loaded when `utop` runs.
