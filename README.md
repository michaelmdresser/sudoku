# sudoku
sudoku solver

Based on [Peter Norvig's post](https://norvig.com/sudoku.html) on the subject. I had to mangle it a fair bit for Clojure, but I stole all of the core concepts from him.

### But why?
It seemed like an interesting problem and I wanted to learn some Clojure.

Is it fast? No. Is my Clojure good? Not really. Does it work? Yes.

### Important Notes
Puzzles in the format
```
......52..8.4......3...9...5.1...6..2..7........3.....6...1..........7.4.......3.
```
are allowed, `.` can also be replaced with `0`.

I just run this in `lein repl` and then load the file and run `(display (search (parseGrid GRIDSTRING)))`. Puzzles that can be solved with only constraint propagation and no exploration only need `parseGrid` but calling `search` on top of that has essentially no overhead.

### Things that could be better
* Efficiency
		* I think I waste time not exiting early in certain situations.
		* I am really unfamiliar with the cost of the sheer number of function calls I am making.
* Testing
* Code organization
* Variable naming in a few places
* Comments that are more explicit about types
* I wanted to write this in as functional of a style as I possibly could (I think I got reasonably close) but I'm sure it could be more idiomatic if I knew Clojure better.
* My Clojure is extremely poor
* Related to the above 2, I rely far too heavily on exceptions to short-circuit bad paths.
* Search could become nondeterministic if I had it pick from the possible value for an unsolved square randomly, which may have better average case behavior.

### Takeaways
* I realize that I like forcing types on arguments to functions. I was bit several types by thinking a set contained values of a certain type that were actually of a different type.
* As ever, writing functional requires a much different mindset than the imperative style I usually work in.
* Sets as a data type are great (looking at you, Golang)
