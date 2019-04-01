#!/usr/bin/env bash
./prolog $1 | swipl -s typer.pl -g main_stdin -g halt
echo $?
