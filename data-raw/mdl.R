## code to prepare `mdl` dataset goes here

mdl_object <- "

(1-K)*F + (1-K)*(1-F)*g # first line
K # some comment
(1-K)*(1-F)* (1-g)
 # separation
(1-K)*(1-F)*g # first line
K + (1-K)*F
(1-K)*(1-F)* (1-g)

"

usethis::use_data(mdl_object, overwrite = TRUE, internal = TRUE)

writeLines(mdl_object, "inst/extdata/mdl")
