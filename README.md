## TP-Funcional

### Overview
This project consists of two modules that allow generating documents with a predefined format, in accordance with the [documentation](Consignas.pdf). The documentation is written in Spanish.

### Challenges and insights
This was a team project for the course "Programming Paradigms", where we concluded our study of the functional paradigm using Haskell.
I started programming in Haskell a year ago, and it has been a very interesting and eye-opening experience for me.
In this project, we used built-in language functions that allowed us to write cleaner code and explore key features of the paradigm, such as different forms of recursion and partial function application.

### How to run
- The [test file](Main.hs) is located within the project. You can use it to add your own tests and try the program.
- To execute the project, it is necessary to have the Haskell compiler (GHC) installed on your device.
- The tests were implemented using the HUnit unit testing framework for Haskell. Once it is installed, you can run the test file or invoke a specific function from the console.
- To run the test file:
1. If you are using an IDE, it may be necessary to open the folder with all the files (and not just the test file).
2. Open a terminal and move to the folder src.
3. Type:
   - `ghci`
   - `:l Main.hs`
   - `main`
- To invoke a specific function:
1. Open the terminal and navigate to the directory src.
2. Then type:
   - `ghci`
   - `:l Documento.hs` -if you want to test the functions from the Documento module.
   - `:l PPON.hs` -if you want to test the functions from the PPON module.
   - Then, type the function name, followed by a space, input value 1, input value 2..., ensuring a space between each input value.
