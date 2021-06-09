Here are the sources of the [website](https://math-comp.github.io/mcb/) of the book

## Updating index.html
To update index.html one needs to update index.htpl and then type 
`make index.html`.

After updating snippets in folder ../coq/ one needs type `make iclean` to clean
up index-related files, then `make sclean` to clean up the `snippets/`
directory, and then `make index.html` to generate new index.html. Then one 
needs to commit the changes including `index.htpl`, `index.html`, and
the `snippets/` directory.
