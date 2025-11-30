
MPTplot <- function(mdl, mode = "combine", stimuli = NULL, responses = NULL, states = NULL, control = NULL) {

  lines <- read_lines(mdl)

  trees <- tree_info(lines)

  n.trees <- length(trees$categories)
  n.params <- sapply(trees$params, length)
  n.branches <- sapply(trees$ordered_branches, length)
  n.categories <- sapply(trees$categories, length)

  # minimal test for tree structure
  check_structure(mode, n.params, n.branches, n.categories)

  # stimulus names
  if (is.null(stimuli)) stimuli <- paste0("STIM. ", 1:n.trees)
  check_stimuli(stimuli, n.trees)
  stim_names <- stimuli

  # category names
  if (is.null(responses)) responses <- lapply(n.categories, function(x) {paste0("resp ", 1:x)})
  if (is.atomic(responses)) responses <- lapply(1:n.trees, function(x) responses)
  check_responses(responses, n.trees, n.categories, mode)
  cat_names <- responses

  # process text
  if (is.null(states)) states <- lapply(trees$params, function(x) {c(paste0("State of ", x), paste0("State of\\\\(1-", x,")"))})
  if (is.atomic(states)) states <- lapply(1:n.trees, function(x) states)
  prc_text <- states

  # control variables
  if (is.null(control)) control <- list()
  control_default <- get_default()
  control <- modifyList(control_default, control)
  list2env(control, envir = environment())

  # body width (includes only processes and lines/arrows)
  body_width <- n.params[1]*(line_width+prc_box_width) + min_arrow_width

  # body height (includes everything except stimulus title)
  body_height <- n.branches[1]*(resp_box_height+resp_box_dist_y) - resp_box_dist_y

  # total width
  if (mode == "combine") {
    # body width (includes only processes and lines/arrows)
    body_width <- n.params[1]*(line_width+prc_box_width) + min_arrow_width

    # body height (includes everything except stimulus title)
    body_height <- n.branches[1]*(resp_box_height+resp_box_dist_y) - resp_box_dist_y

    total_width <-  body_width + length(trees$categories)*(resp_box_width+resp_box_dist_x)-resp_box_dist_x
    total_height <- body_height + stim_txt_dist_y
  } else {
    # body width (includes only processes and lines/arrows)
    body_width <- n.params*(line_width+prc_box_width) + min_arrow_width

    # body height (includes everything except stimulus title)
    body_height <- n.branches*(resp_box_height+resp_box_dist_y) - resp_box_dist_y

    total_width <- stim_box_width + body_width + resp_box_width
    total_height <- body_height
  }


  # combine mode
  if (mode == "combine") {

    # start LaTeX document
    tex_lines <- begin_lines()

    # MAKE RESPONSE BOXES
    tex_lines <- c(tex_lines, "% stimulus name for each tree (from n.trees to 1) and corresponding responses:")
    stim_texts <- rev(stim_names)
    for (tr in 1:n.trees) {
      pos_x <- total_width - tr*(resp_box_width+resp_box_dist_x) + resp_box_dist_x
      tex_lines <- c(tex_lines, paste0("\\node[anchor=west] at (", pos_x, ", ", total_height, ") {\\", stim_txt_size, " ", stim_texts[tr], "};"), "")
      for (br in 1:n.branches[tr]) {
        pos_y <- body_height - (br-1)*(resp_box_height+resp_box_dist_y)
        tex_lines <- c(tex_lines, paste0("\\draw [thick] (", pos_x, ", ", pos_y-resp_box_height, ") rectangle (", pos_x + resp_box_width, ", ", pos_y, ") node[pos=.5] {\\begin{tabular}{cc} ", cat_names[[tr]][trees$cat_pos[[tr]]][br], " \\end{tabular}};"))
      }
      tex_lines <- c(tex_lines, "")
    }

    # MAKE ARROWS
    tex_lines <- c(tex_lines, "", "% make arrows:")
    for (br in 1:n.branches[1]) {
      branch <- trees$ordered_branches[[1]][br]
      n.procs <- length(strsplit(branch, "*", fixed = TRUE)[[1]])
      pos_y <- body_height - round(0.5*resp_box_height, 6) - (br-1)*(resp_box_height+resp_box_dist_y)
      pos_x <- n.procs*(line_width+prc_box_width)
      tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x, ", ", pos_y, ") -- (", body_width, ", ", pos_y, ");"))
    }

    # MAKE PROCESS OUTCOME LINES AND STATES
    tex_lines <- c(tex_lines, "", "% process outcome lines and states:")
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
        ind_curr_prc <- which(trees$params[[1]]==curr_prc)
        curr_prc_text <- prc_text[[1]][c(ind_curr_prc, ind_curr_prc+n.params[1])]
        tex_lines <- c(tex_lines, paste0("% draw text box and lines for process ", curr_prc))
        tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos1_y, ") rectangle (", pos_x+prc_box_width, ", ", pos1_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc}", curr_prc_text[1], " \\end{tabular}};"))
        tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos2_y, ") rectangle (", pos_x+prc_box_width, ", ", pos2_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc}", curr_prc_text[2], " \\end{tabular}};"))

        new_pos_y <- mean(c(strip_branches_y[[ind_fill1]][curr_max], strip_branches_y[[ind_fill2]][curr_max]))
        strip_branches_y[[ind_fill1]][curr_max-1] <- new_pos_y
        strip_branches_y[[ind_fill2]][curr_max-1] <- new_pos_y
        tex_lines <- c(tex_lines, "%% You might want to change the text position of these lines:")
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[above=3, align=center] {\\", probs_size," $", curr_prc, "$} (", pos_x, ", ", strip_branches_y[[ind_fill1]][curr_max], ");"))
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[below=3, xshift=-13] {\\", probs_size, " $(1-", curr_prc, ")$} (", pos_x, ", ", strip_branches_y[[ind_fill2]][curr_max], ");"))
        tex_lines <- c(tex_lines, "")

        len[max_list[[m]][1]] <- len[max_list[[m]][1]] - 1
        len[max_list[[m]][2]] <- 0
      }

    }

    # end LaTeX document
    tex_lines <- c(tex_lines, end_lines())

    # return tikz LaTeX code
    return(tex_lines)

  }


  if (mode == "separate") {

    tex_list <- vector("list", length = n.trees)

    for (tr in 1:n.trees) {

      # start LaTeX document
      tex_lines <- begin_lines()

      # MAKE RESPONSE BOXES
      tex_lines <- c(tex_lines, "% responses:")
      pos_x <- total_width[tr] - resp_box_width
      for (br in 1:n.branches[tr]) {
        pos_y <- body_height[tr] - (br-1)*(resp_box_height+resp_box_dist_y)
        tex_lines <- c(tex_lines, paste0("\\draw [thick] (", pos_x, ", ", pos_y-resp_box_height, ") rectangle (", pos_x + resp_box_width, ", ", pos_y, ") node[pos=.5] {\\begin{tabular}{cc} ", cat_names[[tr]][trees$cat_pos[[tr]]][br], " \\end{tabular}};"))
      }
      tex_lines <- c(tex_lines, "")

      # MAKE ARROWS
      tex_lines <- c(tex_lines, "", "% make arrows:")
      for (br in 1:n.branches[tr]) {
        branch <- trees$ordered_branches[[tr]][br]
        n.procs <- length(strsplit(branch, "*", fixed = TRUE)[[1]])
        pos_y <- body_height[tr] - round(0.5*resp_box_height, 6) - (br-1)*(resp_box_height+resp_box_dist_y)
        pos_x <- n.procs*(line_width+prc_box_width) + stim_box_width
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x, ", ", pos_y, ") -- (", total_width[tr] - resp_box_width, ", ", pos_y, ");"))
      }

      # MAKE PROCESS OUTCOME LINES AND STATES
      tex_lines <- c(tex_lines, "", "% process outcome lines and states:")
      strip_branches <- sapply(trees$ordered_branches[[tr]], function(x) {strsplit(x, split = "\\*")})
      strip_branches_x <- lapply(strip_branches, function(x) {(1:length(x))*(line_width+prc_box_width)})
      strip_branches_y <- lapply(strip_branches, function(x) {rep(NA, length(x))})
      for (br in 1:n.branches[tr]) {
        strip_branches_y[[br]][length(strip_branches_y[[br]])] <- body_height[tr] - 0.5*resp_box_height - (br-1)*(resp_box_height+resp_box_dist_y)
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

          pos_x <- strip_branches_x[[ind_fill1]][curr_max] - prc_box_width + stim_box_width
          pos1_y <- strip_branches_y[[ind_fill1]][curr_max] - 0.5*prc_box_height
          pos2_y <- strip_branches_y[[ind_fill2]][curr_max] - 0.5*prc_box_height
          curr_prc <- strip_branches[[ind_fill1]][curr_max]
          ind_curr_prc <- which(trees$params[[tr]]==curr_prc)
          curr_prc_text <- prc_text[[tr]][c(ind_curr_prc, ind_curr_prc+n.params[1])]
          tex_lines <- c(tex_lines, paste0("% draw text box and lines for process ", curr_prc))
          tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos1_y, ") rectangle (", pos_x+prc_box_width, ", ", pos1_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc}", curr_prc_text[1], " \\end{tabular}};"))
          tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos2_y, ") rectangle (", pos_x+prc_box_width, ", ", pos2_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc}", curr_prc_text[2], " \\end{tabular}};"))

          new_pos_y <- mean(c(strip_branches_y[[ind_fill1]][curr_max], strip_branches_y[[ind_fill2]][curr_max]))
          strip_branches_y[[ind_fill1]][curr_max-1] <- new_pos_y
          strip_branches_y[[ind_fill2]][curr_max-1] <- new_pos_y
          tex_lines <- c(tex_lines, "%% You might want to change the text position of these lines:")
          tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[above=3, align=center] {\\", probs_size," $", curr_prc, "$} (", pos_x, ", ", strip_branches_y[[ind_fill1]][curr_max], ");"))
          tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[below=3, xshift=-13] {\\", probs_size, " $(1-", curr_prc, ")$} (", pos_x, ", ", strip_branches_y[[ind_fill2]][curr_max], ");"))
          tex_lines <- c(tex_lines, "")

          len[max_list[[m]][1]] <- len[max_list[[m]][1]] - 1
          len[max_list[[m]][2]] <- 0

          # stimulus box on left side
          if (curr_max == 1) {
            tex_lines <- c(tex_lines, paste0("\\draw [thick] (", 0, ", ", new_pos_y-0.5*stim_box_height, ") rectangle (", stim_box_width, ", ", new_pos_y+0.5*stim_box_height, ") node[pos=.5] {\\begin{tabular}{cc} ", stim_names[tr], " \\end{tabular}};"))
            tex_lines <- c(tex_lines, "")
          }

        }

      }

      # end LaTeX document
      tex_lines <- c(tex_lines, end_lines())

      # fill list with tex lines per tree
      tex_list[[tr]] <- tex_lines

    }

    # return tikz LaTeX code
    return(tex_list)

  }

}
