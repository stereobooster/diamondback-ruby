#!/bin/bash -e

if [[ "$TRAVIS_TAG" = "" ]]; then exit 0; fi

PLATFORM=$([ "$TRAVIS_OS_NAME" == "linux" ] && echo "linux64" || echo "$TRAVIS_OS_NAME")

mkdir -p "$HOME/release"
rm -rf "$HOME/release/diamondback-ruby"
cp -R bin "$HOME/release/diamondback-ruby"
pushd "$HOME/release" > /dev/null
zip -r \
  "$TRAVIS_BUILD_DIR/diamondback-ruby-$PLATFORM-$TRAVIS_TAG.zip" \
  diamondback-ruby \
  -x \*.tar.gz # exclude diamondback-rubylib.tar.gz
popd > /dev/null

# We don't deploy the whole website on tags, but we do upload diamondback-ruby.js and the
# libs. This sets up the directory we'll upload. See the deploy section of
# .travis.yml.
if [ "$TRAVIS_OS_NAME" == "linux" ]; then
  mkdir -p "$HOME/static/$TRAVIS_TAG"
  cp -r "lib" "$HOME/static/${TRAVIS_TAG}/diamondback-rubylib"
fi
