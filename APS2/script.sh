#!/usr/bin/env bash
make
for exemple in $1/*; do
  echo $exemple
  #cat $exemple
  echo
  echo
  echo "Typechecking..."
  ./prolog $exemple | swipl -s typer.pl -g main_stdin > /tmp/zz.txt
  if [ $? == 0 ]
  then
	echo "Typechecking OK, starting evaluation..."
	./eval $exemple
  else
	echo "Typechecking KO..."
  fi

done
