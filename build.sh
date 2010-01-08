#!/bin/sh

pushd lib/erlsom
./configure || exit 1
make || exit 1
popd

make
