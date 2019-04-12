# script to automatically tie `i` to the command to log ideas
alias idea="vim ~/blog/README.md && (pushd ~/blog; git add README.md; git commit; git push origin master; popd)"
alias i=idea
