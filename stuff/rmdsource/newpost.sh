
#!/bin/sh

# test if changes should be committed
if [ $# -eq 0 ]
  then
    echo "give a title for the post"
else 
  git commit -m "$1"

Changes have been staged, but not committed

sed 's/Posts/Posts\n\n\nNew Inserted Line/' index.Rmd
