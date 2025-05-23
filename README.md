# Firestone

An educational project for DD2489 Large scale development.

## Important note

This code base is **strictly for educational use**. It is **not allowed to publish any content of the course outside of KTH github**.

## REPL dependency

### Starting the REPL and the code is broken

Not that the REPL is a live environment, where you can define symbols to have a meaning. If you load a function into the REPL and then rename it and load it again into the REPL, both function will be loaded into the REPL. This means that test might work, but if you restart the REPL your code will be broken.

Another common mistake is that the order in a namespace does matter. If you rearrange functions you need to restart the REPL to make sure you didn't break anything.

It is a good habit to restart the REPL and then run all tests before you commit your code.

 

