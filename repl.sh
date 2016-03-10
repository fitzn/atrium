#!/bin/bash

Version="0.0.1"

if [ "$1" == "-d" -o "$1" == "-debug" -o "$1" == "--debug" ]; then
  Debug="-Dcom.root81.atrium.debug=true"
fi

scala $Debug -Djava.awt.headless=true -cp target/atrium-cli-$Version.jar -i repl-load.scala
