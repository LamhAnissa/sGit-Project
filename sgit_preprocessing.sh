#!/bin/bash

#create project's jar with all the dependencies
sbt assembly

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

#So "sgit" command can be runned in any directories
alias sgit=$DIR'/sgit_processing.sh'
