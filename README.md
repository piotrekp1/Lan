# Lan

Imperative functionally oriented language

### Quickstart 

enter the main directory with Makefile and build the project <br/>
`make`<br/>
then you can run programs with <br/>
`./Lan <program_name>`<br/>
or you can launch a repl with<br/>
`./Lan`<br/>
for help enter <br/>
 `./Lan -h` <br/>

### Color schemes
there are color schemes made for emacs, they are in the color_schemes folder, to launch them open the file 
`lan.clrs` in emacs <br/>
`M - x` and then type `eval-buffer`<br/>
after that in your buffer where you plan to write lan code launch <br/>
`M - x` and type `lan-mode` <br/>


### Language Features
- 3 basic types: Int, Bool and Char, and every possible combination of arrays and functions,e.g. [(Int -> Int) -> [Bool]] is as good type as Bool
- it has loops, assignments, '+=' and '++'s , and prints.
- static binding of ids and static typing - type control ends before starting the program
- explicitly handed runtime errors (for now division by 0 and index out of the array range)
- functions can be both passed to functions and returned from functions
- lambdas, local definitions in functions can be functions themselves.
- arbitrarly nested definitions of a functions with static binding
