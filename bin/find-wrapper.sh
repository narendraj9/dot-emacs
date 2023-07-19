#!/usr/bin/env bash

find "$@" 2> /tmp/find-wrapper-error.log;
exit 0;
