# Implementing a Functional Language: Notes

Ideally, each part should be a literal Haskell file - that way, the whole thing is
executable and I can be sure that all the code compiles.

Need to consider at what point to define the framework for visualisation. At minimum we
want to be able to produce graphs for each stage of the reduction process. Maybe this
should be a standalone part?

We'll use megaparsec to define a nice concise parser for the language, but we also need to
write a concise pretty printer - worth surveying the current Haskell libraries for pretty
printing as there are a lot of them.


