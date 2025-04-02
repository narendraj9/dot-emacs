#!/usr/bin/env bash

JAVA_HOME=$(coursier java-home --jvm 17) metals-emacs "$@"
