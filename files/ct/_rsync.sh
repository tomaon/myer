#!/bin/bash --norc

 REMOTE=wheezy@wheezy

 rsync -n -avzc --delete "$REMOTE:node/" "mysqld"
#rsync    -avzc --delete "$REMOTE:node/" "mysqld"

#rsync -n -avzc --delete "mysqld/" "$REMOTE:node"
#rsync    -avzc --delete "mysqld/" "$REMOTE:node"
