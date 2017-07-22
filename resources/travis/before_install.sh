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
    printf "travis_fold:start:sudo_apt-get_update\nsudo apt-get update\n"
    gem install syck
    printf "travis_fold:end:sudo_apt-get_update\n"
    ;;
  *)
    ;;
esac
