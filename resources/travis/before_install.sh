#!/bin/bash

# install system dependencies:
#
# - aspcud for opam (on OS X; installed via apt on Linux)
# - awscli (to talk to AWS)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case "$TRAVIS_OS_NAME" in
  osx)
    printf "travis_fold:start:brew_install\nInstalling brew\n"
    brew update
    brew install aspcud awscli syck
    printf "travis_fold:end:brew_install\n"
    ;;
  *)
    ;;
esac
