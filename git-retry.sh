#!/bin/bash

GITBIN=/usr/bin/git
RETRIES=280
DELAY=600
COUNT=1
while [ $COUNT -lt $RETRIES ]; do
  $GITBIN $*
  if [ $? -eq 0 ]; then
    RETRIES=0
    break
  fi
  let COUNT=$COUNT+1
  sleep $DELAY
done

