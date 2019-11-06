
#!/bin/sh

# This script renders all changed posts and commits the changes to git

# has one parameter that is the commit message, has to be passed in the form
# If no commit message is passed, the changes are only staged and not committed

# Usage
# ./render.sh
# ./render.sh { "this is my commit message" }

R -e 'source("renderAll.r")'

cd ..
git add .
cd -

if [ $# -eq 0 ]
  then
    echo "Changes have been staged, but not committed"
else 
  git commit -m $1
  echo "Changes have been committed"
fi
