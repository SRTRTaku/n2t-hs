# n2t-hs
[![Haskell CI](https://github.com/SRTRTaku/n2t-hs/actions/workflows/haskell.yml/badge.svg)](https://github.com/SRTRTaku/n2t-hs/actions/workflows/haskell.yml)

The Elements of Computing Systems(nand2tetris) in Haskell
* [The Elements of Computing Systems](http://mitpress.mit.edu/9780262140874/)
* (Japanese)[コンピュータシステムの理論と実装](https://www.oreilly.co.jp/books/9784873117126/)

## How to use
```sh
$ cabal run n2t-hs [OPTION] <file name / dir name>
OPTION
  asm (with <assemble file name>)
    run assembler
  vm (with <virtual machine code directory>)
    run VM translator
```
