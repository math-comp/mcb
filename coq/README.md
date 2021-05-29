# This is the "Mathematical Components" book's code listings for jsCoq.

Code snippets are intended to be viewed using jsCoq, but you also can just open
selected `.v` file in your favorite Galina code editor. To view pages in your 
browser locally you need install `coq`, `mathcomp`, then do `npm install` and
`node app.js` run node applicaion (app.js) and then open link 
http://127.0.0.1:8010/ in your browser. A bit more detailed instructions are 
provided below.

## Installation & Run

### Prerequisites
* [Coq](https://github.com/coq/coq)
* [The Mathematical Components library](https://github.com/math-comp/math-comp)
* [Node.js](https://nodejs.org/en)

### Installation
* Select a directory for files, in example /home/uname/mcb/coq.
* Put files from https://github.com/math-comp/mcb/tree/master/coq to selected
directory.
* Open terminal and navigate to the selected directory.
* Type `npm install` to install dependences.
* Type `make` to create .html files.

### Run
* Type `node app.js` to ask Node.js to serve files.
* Navigate your browser to http://127.0.0.1:8010/ , you should see the list of
snippets by chapters.

## Contributing

### Updating the code snippests
* Edit chapter snippet file (in example, ch7.1.v).
* Type `rm ch7.1.html && make ch7.1.html`.
* Or alternatively type `make clean && make` to recreate all html snippet files
in the directory (and also index.html).
* Refresh page in your browser (if you need it).

### Updating the list of snippets (index.html)
* Add/Remove snippet file (in example, ch7.1.v).
* Open Makefile. You will the list of chapter files at the top of the Makefile.
* Add/Remove file name from the list.
* Type `rm index.html && make index.html` to generate new index.html.
* Or alternatively type `make clean && make` to recreate all html snippet files
in the directory and also index.html.

## Homepages

* Link to the [homepage of the book](https://math-comp.github.io/mcb)
* Link to the [homepage of jsCoq](https://coq.vercel.app)
