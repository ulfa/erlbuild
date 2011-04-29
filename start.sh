#!/bin/sh

erl +A 5 +K true -pa $PWD/ebin $PWD/test $PWD/deps/*/ebin -boot start_sasl -s erlbuild
