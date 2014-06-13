#!/bin/bash
set -eu

do_single_test ()
{
  ALGORITHM="$1"
  CHARS_COUNT="$2"
  FILE_SIZE="$3"
  shift 3

  GC_CALL="./grammar_compression --algorithm $ALGORITHM"

  echo '--------------------'
  echo "Testing file size $FILE_SIZE with $CHARS_COUNT distinct characters"
  ./mk_test_file $CHARS_COUNT $FILE_SIZE > mk_test.sh.tmp
  $GC_CALL    mk_test.sh.tmp         mk_test.sh.tmp.grammar
  $GC_CALL -d mk_test.sh.tmp.grammar mk_test.sh.tmp.2
  diff -u mk_test.sh.tmp mk_test.sh.tmp.2
}

do_test_alg ()
{
  ALGORITHM="$1"
  shift 1

  do_single_test "$ALGORITHM" 1 0

  for (( FILE_SIZE = 1 ; FILE_SIZE <= 256 ; FILE_SIZE ++ )); do
    for (( CHARS_COUNT = 1 ; CHARS_COUNT <= FILE_SIZE ; CHARS_COUNT ++ )); do
      do_single_test "$ALGORITHM" "$CHARS_COUNT" "$FILE_SIZE"
    done
  done

  for (( FILE_SIZE = 257 ; FILE_SIZE < 1000 ; FILE_SIZE += 10 )); do
    for (( CHARS_COUNT = 1 ; CHARS_COUNT <= 256 ; CHARS_COUNT += 10 )); do
      do_single_test "$ALGORITHM" "$CHARS_COUNT" "$FILE_SIZE"
    done
  done

  do_single_test "$ALGORITHM" 256 10000
  do_single_test "$ALGORITHM" 256 100000
  do_single_test "$ALGORITHM" 256 1000000
}

# do_test_alg none
# do_test_alg sequitur
do_test_alg sequential
# do_test_alg rytter

rm -f mk_test.sh.tmp mk_test.sh.tmp.grammar mk_test.sh.tmp.2
