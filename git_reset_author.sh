#!/bin/sh

# Credits: http://stackoverflow.com/a/750191

git filter-branch -f --env-filter "
    GIT_AUTHOR_NAME='Michał Darda'
    GIT_AUTHOR_EMAIL='mdarda@pm.me'
    GIT_COMMITTER_NAME='Michał Darda'
    GIT_COMMITTER_EMAIL='mdarda@pm.me'
  " HEAD
