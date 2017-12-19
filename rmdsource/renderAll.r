### I use this file to render all Rmd files to html

rmdFiles <- list.files(pattern=".Rmd")

for (i in rmdFiles) {
    rmarkdown::render(i, output_dir="../")
}
