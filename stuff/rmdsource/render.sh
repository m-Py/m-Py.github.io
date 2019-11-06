
#!/bin/sh

# This script renders all changed posts and commits the changes to git

# If no post was rendered, this script will still stage and commit --
# there is no test if any file was actually changed. This may be 
# convenient anyway

# Has one parameter that is the commit message, has to be passed as shown in the example below
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
  echo "Changes have been committed" # does not test if they actually were ;)
fi
