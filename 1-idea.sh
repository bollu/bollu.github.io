# script to automatically tie `i` to the command to log ideas
alias idea="vim ~/1-cool-stuff.md && (pushd ~/blog; git add README.md; git commit; git push origin master; popd)"
alias i=idea
