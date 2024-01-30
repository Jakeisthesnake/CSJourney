#!/bin/bash

source init_test


# Test for beargit_init
beargit init
if [ ! -d ".beargit" ] || [ ! -f ".beargit/.index" ] || [ ! -f ".beargit/.prev" ]; then
  echo "Test for beargit_init failed"
  exit 1
fi

# Test for beargit_add
echo "test" > testfile
beargit add testfile
if ! grep -q "testfile" ".beargit/.index"; then
  echo "Test for beargit_add failed"
  exit 1
fi

# Test for beargit_add with already added file
output=$(beargit add testfile 2>&1)
#echo "Test for beargit_add with already added file: $output"
if [[ $output != "ERROR: File testfile already added" ]]; then
  echo "Test for beargit_add with already added file failed"
  exit 1
fi

# Test for beargit_rm
beargit rm testfile
if grep -q "testfile" ".beargit/.index"; then
  echo "Test for beargit_rm failed"
  exit 1
fi

# Test for beargit_rm with untracked file
echo "untracked" > untrackedfile
output=$(beargit rm untrackedfile 2>&1)
#echo "Test for beargit_rm with untracked file: $output"
if [[ $output != "ERROR: File untrackedfile not tracked" ]]; then
  echo "Test for beargit_rm with untracked file failed"
  exit 1
fi

# Test for beargit_commit with invalid message
output=$(beargit commit -m "Invalid message" 2>&1)
#echo "Test for beargit_commit with invalid message: $output"
if [[ $output != "ERROR: Message must contain \"GO BEARS!\"" ]]; then
  echo "Test for beargit_commit with invalid message failed"
  exit 1
fi

# Test for beargit_commit with valid message
beargit commit -m "GO BEARS!"
if [ ! -d ".beargit/cccccccccccccccccccccccccccccccccccccccc" ]; then
  echo "Test for beargit_commit with valid message failed"
  exit 1
fi

# Test for beargit_commit with no files added
beargit commit -m "GO BEARS!"
if [ ! -d ".beargit/ccccccccccccccccccccccccccccccccccccccc1" ]; then
  echo "Test for beargit_commit with no files added failed"
  exit 1
fi

# Test for beargit_add with a non-existent file
output=$(beargit add nonexistentfile 2>&1)
#echo "Test for beargit_add with a non-existent file: $output"
if [[ $output != "ERROR: No or invalid filname given" ]]; then
  echo "Test for beargit_add with non-existent file failed"
  exit 1
fi

# Test for beargit_rm with a file that is not in the index
echo "untracked" > untrackedfile
output=$(beargit rm untrackedfile 2>&1)
if [[ $output != "ERROR: File untrackedfile not tracked" ]]; then
  echo "Test for beargit_rm with a file not in the index failed"
  exit 1
fi

# Test for beargit_status with no files added
output=$(beargit status)
if [[ $output != *"0 files total"* ]]; then
  echo "Test for beargit_status with no files added failed"
  exit 1
fi

echo "All tests passed"