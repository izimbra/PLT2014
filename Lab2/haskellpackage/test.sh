bnfc --haskell CPP.cf

happy ParCPP.y

alex LexCPP.x

ghc --make -o lab2 lab2.hs


