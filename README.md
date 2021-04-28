This repository contains the code and data to replicate my paper on the relationship between gentrification and the racial composition of neighborhood employment. To replicate the paper on your local machine, change the root directory in all Rmd files. I do this using the following find and replace command in the shell:

find . -type f -name "*.Rmd" | xargs gsed -i 's|Users/liampurkey/Desktop/Research/Gentrification|your/path|g'

