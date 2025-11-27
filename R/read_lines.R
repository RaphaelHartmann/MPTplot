
read_lines <- function(mdl) {

  # read the lines
  if (grepl("\\+", mdl) & grepl("\\*", mdl)) {
    lines <- strsplit(mdl, "\n")[[1]]
  } else {
    lines <- readLines(mdl)
  }

  # remove comments
  lines <- as.character(sapply(lines, FUN = function(x) {sub("#.*", "", x)}, simplify = TRUE))

  # remove empty characters
  lines <- as.character(sapply(lines, FUN = function(x) {gsub("\\s+", "", x)}))

  # remove empty line(s) at end
  if (lines[length(lines)] == "") {
    while(lines[length(lines)] == "") {
      lines <- lines[-length(lines)]
    }
  }
  # remove empty line(s) at start
  if (lines[1] == "") {
    while(lines[1] == "") {
      lines <- lines[-1]
    }
  }

  return(lines)

}
