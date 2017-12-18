### I use this file to render all Rmd files to html

setwd("./rmdsource")
rmdFiles <- list.files(pattern=".Rmd")
setwd("../")

for (i in rmdFiles) {
    rmarkdown::render(paste0("./rmdsource/", i), output_dir="./")
}
