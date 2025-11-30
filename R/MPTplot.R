
MPTplot <- function(mdl, output = NULL, mode = "combine") {

  lines <- read_lines(mdl)

  trees <- tree_info(lines)

  n.trees <- length(trees$categories)
  n.params <- sapply(trees$params, length)
  n.branches <- sapply(trees$ordered_branches, length)

  # minimal test for tree structure
  if (mode == "combine" & length(unique(n.params)) != 1) {
    stop("the trees do not even share the same number of parameters")
  }
  if (mode == "combine" & length(unique(n.branches)) != 1) {
    stop("the trees do not even share the same number of branches")
  }


  # stimulus names
  stim_names <- c("TRUE", "FALSE")

  # category names
  cat_names <- c("true", "false")

  # process text
  prc_text <- list(c("text 1 a", "text 1 b"),
                   c("text 2 a", "text 2 b"),
                   c("text 3 a", "text 3 b"))



  # lines for process outcomes:
  line_width <- 2

  # lines for last process to response:
  arrow_width <- 1.5

  # text boxes with round corners for each process:
  prc_box_width <- 2.5
  prc_box_height <- 1

  # response boxes:
  resp_box_width <- 2
  resp_box_height <- 1
  resp_box_dist_x <- 0.5
  resp_box_dist_y <- 0.5

  # stimulus box
  stim_box_width <- 2
  stim_box_height <- 1

  # stimulus text above response boxes
  stim_txt_dist_y <- 0.5

  # body width (includes only processes and lines/arrows)
  body_width <- n.params[1]*(line_width+prc_box_width) + arrow_width

  # body height (includes everything except stimulus title)
  body_height <- n.branches[1]*(resp_box_height+resp_box_dist_y) - resp_box_dist_y

  # total width
  if (mode == "combine") {
    total_width <-  body_width + length(trees$categories)*(resp_box_width+resp_box_dist_x)-resp_box_dist_x
    total_height <- body_height + stim_txt_dist_y
  } else {
    total_width <- stim_box_width + body_width + resp_box_width
    total_height <- body_height
  }

  # start LaTeX document
  tex_lines <- c("\\documentclass[tikz,border=2pt]{standalone}",
                 "",
                 "\\usepackage{tikz}",
                 "",
                 "\\begin{document}",
                 "",
                 "\\begin{tikzpicture}",
                 "",
                 "")

  if (mode == "combine") {

    tex_lines <- c(tex_lines, "% stimulus name for each tree (from n.trees to 1) and corresponding responses:")
    stim_texts <- rev(stim_names)
    for (tr in 1:n.trees) {
      pos_x <- total_width - tr*(resp_box_width+resp_box_dist_x) + resp_box_dist_x
      tex_lines <- c(tex_lines, paste0("\\node[anchor=west] at (", pos_x, ", ", total_height, ") {\\large ", stim_texts[tr], "};"), "")
      for (br in 1:n.branches[tr]) {
        pos_y <- body_height - (br-1)*(resp_box_height+resp_box_dist_y)
        tex_lines <- c(tex_lines, paste0("\\draw [thick] (", pos_x, ", ", pos_y-resp_box_height, ") rectangle (", pos_x + resp_box_width, ", ", pos_y, ") node[pos=.5] {\\begin{tabular}{cc} ", cat_names[trees$cat_pos[[tr]]][br], "\\end{tabular}};"))
      }
      tex_lines <- c(tex_lines, "")
    }

    tex_lines <- c(tex_lines, "", "% make arrows:")
    for (br in 1:n.branches[1]) {
      branch <- trees$ordered_branches[[1]][br]
      n.procs <- length(strsplit(branch, "*", fixed = TRUE)[[1]])
      pos_y <- body_height - round(0.5*resp_box_height, 6) - (br-1)*(resp_box_height+resp_box_dist_y)
      pos_x <- n.procs*(line_width+prc_box_width)
      tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x, ", ", pos_y, ") -- (", body_width, ", ", pos_y, ");"))
    }

    tex_lines <- c(tex_lines, "", "% process outcome lines:")
    strip_branches <- sapply(trees$ordered_branches[[1]], function(x) {strsplit(x, split = "\\*")})
    strip_branches_x <- lapply(strip_branches, function(x) {(1:length(x))*(line_width+prc_box_width)})
    strip_branches_y <- lapply(strip_branches, function(x) {rep(NA, length(x))})
    for (br in 1:n.branches[1]) {
      strip_branches_y[[br]][length(strip_branches_y[[br]])] <- body_height - 0.5*resp_box_height - (br-1)*(resp_box_height+resp_box_dist_y)
    }
    tree_len <- length(strip_branches)
    len <- as.numeric(sapply(strip_branches, length))
    max <- max(len)
    for (curr_max in max:1) {

      ind_max <- which(len == curr_max)
      max_list <- split(ind_max, rep(1:(length(ind_max)/2), each = 2))
      for (m in 1:length(max_list)) {
        ind_fill1 <- max_list[[m]][1]
        ind_fill2 <- max_list[[m]][2]

        pos_x <- strip_branches_x[[ind_fill1]][curr_max] - prc_box_width
        pos1_y <- strip_branches_y[[ind_fill1]][curr_max] - 0.5*prc_box_height
        pos2_y <- strip_branches_y[[ind_fill2]][curr_max] - 0.5*prc_box_height
        curr_prc <- strip_branches[[ind_fill1]][curr_max]
        curr_prc_text <- prc_text[[which(trees$params[[1]]==curr_prc)]]
        tex_lines <- c(tex_lines, paste0("% draw text box and lines for process ", curr_prc))
        tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos1_y, ") rectangle (", pos_x+prc_box_width, ", ", pos1_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc}", curr_prc_text[1], "\\end{tabular}};"))
        tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos2_y, ") rectangle (", pos_x+prc_box_width, ", ", pos2_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc}", curr_prc_text[2], "\\end{tabular}};"))

        new_pos_y <- mean(c(strip_branches_y[[ind_fill1]][curr_max], strip_branches_y[[ind_fill2]][curr_max]))
        strip_branches_y[[ind_fill1]][curr_max-1] <- new_pos_y
        strip_branches_y[[ind_fill2]][curr_max-1] <- new_pos_y
        tex_lines <- c(tex_lines, "%% You might want to change the text position of these lines:")
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[above=3, align=center] {\\Large $", curr_prc, "$} (", pos_x, ", ", strip_branches_y[[ind_fill1]][curr_max], ");"))
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[below=3, xshift=-13] {\\Large $(1-", curr_prc, ")$} (", pos_x, ", ", strip_branches_y[[ind_fill2]][curr_max], ");"))
        tex_lines <- c(tex_lines, "")

        len[max_list[[m]][1]] <- len[max_list[[m]][1]] - 1
        len[max_list[[m]][2]] <- 0
      }

    }
  }





  # end LaTeX document
  tex_lines <- c(tex_lines,
                 "",
                 "\\end{tikzpicture}",
                 "",
                 "\\end{document}",
                 "")

  return(tex_lines)

}
