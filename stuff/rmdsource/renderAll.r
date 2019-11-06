### I use this file to render all Rmd files to html

rmdFiles <- setdiff(list.files(pattern=".Rmd"), "06_BFSize.Rmd")

for (i in rmdFiles) {
    rmarkdown::render(i, output_dir="../")
}
