# badspreadsheet

**Warning** This is a *prototype*, so please set your expectations accordingly.

A Clojure Spreadsheet for building design tools.

Badspreadsheet is an experimental tool that I've built to help make little CAD tools and design explorations on a 2D canvas.

I'm making this prototype open to let others try it out and to explore some questions, figure out how to fix obvious design and usage issues, and to see what ideas other people might have.

### Features
Check out these videos, which show things off nicely:

https://github.com/adam-james-v/badspreadsheet/assets/21064735/78a3b36f-68b1-48f5-9f73-dd8d7612d710


https://github.com/adam-james-v/badspreadsheet/assets/21064735/b66c29af-17dd-4a67-8804-20b4a7208a8e

- **Code Cells**: Write Clojure code directly in spreadsheet cells and execute them on-the-fly.
- **Cell Linking**: Easily link cells together, creating complex dependencies and calculations.
- **Rendering**: Renders Strings as Markdown, Gives numerical values usable inputs, and Renders Hiccup!

## Getting Started
If you're interested in just quickly playing around with the prototype, I'd recommend downloading the jar and running it that way. It's also not too hard to clone the repository and run the code yourself if you're used to using Clojure's CLI tool. Finally, if you're a Clojure dev, it could be fun to use badspreadsheet as a library and really poke around.

### Run the Jar
Otherwise, you can
 - [download the jar](https://github.com/adam-james-v/badspreadsheet/releases/tag/prototype-01).
 - `cd` into that directory and
 - run the build with `java -jar badspreadsheet-prototype.jar`. This will start the server and print the port it's using.
 - Head to `localhost:THE_PORT_SPECIFIED` and get creative!

### Build from Source
If you're comfortable with Clojure, you can do the following:
 - `git clone https://github.com/adam-james-v/badspreadsheet.git`
 - `cd badspreadsheet`
 - `clojure -T:build uber` which should build the uberjar `target/badspreadsheet-prototype.jar`
 - run SDFx with `java -jar target/badspreadsheet-prototype.jar`. This command will start the server and print the port it's using.
 - Head to `localhost:THE_PORT_SPECIFIED` and have fun!

### As a Library
To use badspreadsheet as a library, you can add this to your `deps.edn`:

```clojure
{badspreadsheet/badspreadsheet {:git/url "https://github.com/adam-james-v/badspreadsheet"
                                :git/sha "GET LATEST"}}
```

## Open Questions and Known Problems

 - 'State smearing' -> I have state held in the cells and also in the spreadsheet namespace.
 - The canvas is not pannable and thus isn't actually infinite yet
 - moving and sizing cells is cumbersome and only possible with the keyboard
 - Cell and Entity dependencies have no protection from cycles
 - Slow cells can hang the whole app
 - It's hard to tell which cells are linked. There's no visual for this.
 - if you 'scoop' multiple cells inside the cursor at once it's hard/hacky to separate them again
 - errors should be communicated to the user in the UI. Cleanly
 - the UI should probably never care about the current pending status of any slow calculations (it should stay responsive)
 - the codemirror editor needs to be better. Eg. Paredit and paren match highlighting.
 - Linking cells could have some more UI affordances (eg, clicking another cell inserts its ref)
 - Location cell refs don't work properly yet
 - What's the right way to store 'entities'? I have a map of entities with integer keys, but I often want to do 'spatial queries' where I get a cell at a location or all cells in a window. What makes sense in terms of data structure here?
 - Things get kinda weird when you do `def` or `defn` in a cell and use that fn in other cells. It sort of works but if you update the definition it doesn't push changes to the cells (the cells don't know that they're actually dependent on other cells).
 - fast UI changes can 'flood' and maybe cause out-of-order weirdness, which I think relates to:
 - state computations and UI updates are coupled, it feels like the UI should update consistently at 60+ FPS, no matter what the state. That is, even if a long computation is happening, the UI should still render fine and never care about any state other than the most recent state.
 - ergonomics of requiring libraries in the Spreadsheet's namespace is not great
 - In fact, I haven't been careful to set up a proper namespace or execution context for the spreadsheet: I think it's actually executing in `clojure.core`
 - the js used to make the frontend stuff work is... messy. I've considered figuring out Squint as a way to write js in a lisp, in the backend, but it's half (quarter) baked at best so far. It might make sense to just use javascript directly and just go ask people about the best practices there. Not *everything* needs to be a Clojure-like 🤔.
 - the UI affordances around 'flipping' a cell are not visually clear.
 - the keyboard shortcuts aren't intuitive enough I think. I often flip cards when I mean to move them, for example.
 - cell visibility isn't great.
 - cell id 'chips' get in the way when you're editing small cells

## Usage

After starting `badspreadsheet`, you can begin by creating code cells. Here's a quick guide:

1. **Create a Cell**: Click on a cell and start typing Clojure code.
2. **Link Cells**: Reference other cells in your code for dynamic calculations.
3. **Run Code**: Execute the code and see the results in real-time.

For detailed usage, refer to the [Documentation](link-to-documentation).

## Contributing

`badspreadsheet` is in its prototype phase, and contributions are welcome! If you're interested in contributing, open an issue!

## License

This project is licensed under the [MIT License](link-to-license) - see the LICENSE file for details.

## Acknowledgments

- Clojure
