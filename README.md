# tweet v1.0

Echo statistics about recent tweets from twitter's test stream.

## To Install

Stack will install ghc for you, fetch all dependencies and compile the project.

* Install [Stack](https://docs.haskellstack.org/en/stable/README/)
* Run `stack setup`
* In this directory, type `stack build`
* `mv config.ini.example config.ini`
* Set all of your twitter oauth settings in config.ini
* Run `.stack-work/install/<your arch>-linux/lts-9.0/8.0.2/bin/tweet +RTS -N`
