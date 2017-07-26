#!/bin/bash

# install system dependencies:
#
# - aspcud for opam (on OS X; installed via apt on Linux)
# - syck

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case "$TRAVIS_OS_NAME" in
  osx)
    printf "travis_fold:start:brew_install\nInstalling brew\n"
    brew update
    brew install aspcud syck
    printf "travis_fold:end:brew_install\n"
    ;;
  linux)
    printf "travis_fold:start:syck\nsyck install\n"
    git clone https://github.com/indeyets/syck --depth 1
    cd syck
    ./bootstrap
    ./configure
    make
    sudo make install
    printf "travis_fold:end:syck\n"
    ;;
  *)
    ;;
esac
