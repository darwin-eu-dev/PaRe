PaRe 0.1.13
==========
1. Fixed issue where pak 0.7.1 broke `getDefaultPermittedPackages()` and `getGraphData()` and updated internal code.
2. Tests should run anywhere except when there is no internet connection.
3. Regardless if dependencies are being fetched when using `makeReport()`, it should continue.

PaRe 0.1.12
==========
1. Changed `magick::image_read()` call to `magick::image_read_svg()` to address
an error encountered with using libmagick 6.9.12-93.
2. Changed the way base packages are fetched.
3. Resolved addPareArticle issue with injecting path into Rmd.

PaRe 0.1.11
==========
1. Only create the ./vignettes/articles directory, if it does not exist already.
2. skip one unit test for checkDependencies on CRAN.
3. Fixed issue where rds-files would be picked up in ./R/ folder.

PaRe 0.1.10
==========
1. Fixed issue with pattern matching of files in .gitignore.
2. Fixed issue with retrieving the correct amount of arguments from a function.
3. Changed colour highlights for report.
4. Added addPareArticle function to add a PaRe report as an article.
5. Added setup for unittesting.
6. Added unittest for:
    - getDefaultPermittedPackages
    - addPareArticle
    - getApplyCall
    - getApplyFromLines
    - getDlplyCall
    - getDlplyCallFromLines
    - getDoCall
    - getDoCallFromLines
    - getFunCall
    - getMultiLineFun
    - checkDependencies
7. Fixed issue detecting functions called by plyr::dlply

PaRe 0.1.9
==========
1. Added CRAN badge to README
2. Turned off evaluation of code block that keeps crashing on:
    - r-devel-linux-x86_64-debian-clang
    - r-devel-linux-x86_64-debian-gcc
    - r-patched-linux-x86_64
    - r-release-linux-x86_64
3. The message: "Could not connect to the internet, online hosted whitelists will be ignored." has been replaced with the actual warning or error it throws as a message.
4. Fetching file paths now accounts for .gitignore

PaRe 0.1.8
==========
1. Set pak to version >= 0.2.0.
2. Removed version requirement for utils.
3. Attempt at solving issue with pak (0.1.6: 4).

PaRe 0.1.7
==========
1. Fixed issue when creating a File object for .cpp, .o, .h, .java and .sql files.
2. Imported dependencies have a set package requirement.
3. getDefaultPermittedPackages example now has tag \donttest.
4. ~~Solved issue where dependency pak did not include glue.~~

PaRe 0.1.6
==========

1. Started tracking changes in NEWS.md
2. Added significant changes to documentation.
3. Optimizations using R6 representations.
4. Added functionality to subset package diagram
5. Added git blame functionality

PaRe 0.1.5
==========

1. Added R6 code representations.

PaRe 0.1.0
==========

1. Initial transfer from DependencyReviewer to Package Reviewer.

