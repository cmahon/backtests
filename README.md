# Backtest examples

Examples of vectorized backtests using hmatrix. Equity curves in GnuPlot via easyplot.

## Setup

Tested on OS X with ghc 7.6.3 and cabal 1.20.

    git clone https://github.com/cmahon/backtests.git
    cd backtests
    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build
    cabal run 
