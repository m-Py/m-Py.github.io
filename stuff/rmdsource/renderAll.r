# I use this file to render all Rmd files to html. It assumes that
# files are changed/staged, but not committed

# 
staged_files  <- system("git diff --name-only --cached", intern = TRUE)
changed_files <- system("git diff --name-only", intern = TRUE)
new_files <- system("git ls-files --others --exclude-standard", intern = TRUE)

files <- unique(c(staged_files, changed_files, new_files))


# only select Rmd files
rmds <- files[grepl(pattern =".Rmd", x = files)]
rmds <- sapply(strsplit(rmds, split = "/"), function(x) { x[grepl(x, pattern = ".Rmd")] })

message("\nRendering ", c("no file", "file", "files")[(length(rmds) > 1) + 1], " ", toString(rmds))

for (i in rmds) {
    rmarkdown::render(i, output_dir="../")
}
