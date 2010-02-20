#!/bin/sh

pushd lib/erlsom
make || exit 1
popd

make compile test docs
