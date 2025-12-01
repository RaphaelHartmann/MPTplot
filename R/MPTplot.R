
#' Generate LaTeX code for plotting multinomial processing tree models
#'
#' Generate LaTeX code for multiple binary multinomial processing trees (MPTs).
#'   Either combine multiple MPTs with the same structure in one file or make
#'   separate files for each MPT.
#'
#' @param mdl MDL model syntax file or character string defining the MPT model.
#'   Trees must be separated with one or more empty lines. A line with only a
#'   comment (using the hash symbol) will also count as an empty line.
#' @param mode either of two options. "combine" lets you combine multiple trees
#'   into one figure with stimuli condition and responses on right-hand side.
#'   The mode "separate" will produce a figure per tree. The second option must
#'   be used if the trees do not have the same structure of processes.
#' @param stimuli vector of characters with stimulus conditions defining each tree.
#' @param responses vector or list of characters giving the expected response for
#'   each category in each tree. The MDL syntax defines the order of the possible
#'   response categories; First line means first response category (e.g., "true"
#'   or "correct"), the second line means second possible response category (e.g.,
#'   "false" or "incorrect"), and so on.
#'
#'   * If \code{mode = "separate"} this must be a list of character vectors. The
#'     number of list elements must equal the number of trees in the model. The
#'     length of the character vectors must equal the number of possible response
#'     categories per tree.
#'
#'   * If \code{mode = "combine"} this can, but must not be a list. A vector is
#'     sufficient, since only the first list element would be taken anyway. Make
#'     sure the order of response categories are the same in each tree in your
#'     MDL syntax file or character string.
#'
#' @param states vector or list of characters giving the latent states that are
#'   reached after each process. The order of the processes is alphabetical.
#'   Therefore, the text for each state must be given in that order within a
#'   vector, first for the process outcomes in the positive direction (e.g., "D")
#'   and then for the process outcomes in the negative direction (e.g., "(1-D)").
#'
#'   * If \code{mode = "separate"} this must be a list of character vectors. The
#'     number of list elements must equal the number of trees in the model. The
#'     length of the character vectors must equal two times the number of
#'     processes.
#'
#'   * If \code{mode = "combine"} this can, but must not be a list. A vector is
#'     sufficient, since only the first list element would be taken anyway.
#'
#' @param control a named list of variables that control the shape of the figure.
#'   Allowed list elements are:
#'
#'   * \code{line_width} length of process-outcome lines in x-dimension. Default
#'     is 2.
#'
#'   * \code{min_arrow_width} length of minimal distance from last box of latent
#'     states to the box of responses. Default is 1.5.
#'
#'   * \code{prc_box_width, prc_box_height} width and height of box for latent
#'     states after each process outcome. Default is 2.5 and 1, respectively.
#'
#'   * \code{resp_box_width, resp_box_height} width and height of box for
#'     responses. Default is 2 and 1, respectively.
#'
#'   * \code{resp_box_dist_x, resp_box_dist_y} distance in x- and y-dimension
#'     between the boxes for the responses. This is only relevant for
#'     \code{mode = "combine"} where the responses of all trees are placed on
#'     the right-hand sinde of the figure. Default is 0.5 for both.
#'
#'   * \code{stim_box_width, stim_box_heigth} width and heigth of box for stimuli
#'     in each tree in \code{mode = "separate"}. This will be placed at the
#'     left-hand side of each tree at the starting node. Default is 2 and 1,
#'     respectively.
#'
#'   * \code{stim_txt_dist_y} distance in y-dimension between response-boxes
#'     and the stimulus name. This is only relevant for \code{mode = "combine"}.
#'     Default is 0.5.
#'
#'   * \code{stim_font_size} font size in LaTeX for the stimulus name. This can be
#'     any LaTeX font size from "tiny" to "Huge". Default is "large".
#'
#'   * \code{probs_font_size} font size in LaTeX for the probability parameters (e.g.,
#'     "D" and "(1-D)"). This can be any LaTeX font size from "tiny" to "Huge".
#'     Default is "Large".
#'
#'   * \code{state_font_size} font size in LaTeX for the latent process states This
#'     can be any LaTeX font size from "tiny" to "Huge". Default is "normalsize".
#'
#'   * \code{resp_font_size} font size in LaTeX for the possible responses. This
#'     can be any LaTeX font size from "tiny" to "Huge". Default is "normalsize".
#'
#' @returns either a character vector (for \code{mode = "combine"}) or a list of
#'   the same length as the number of trees (for \code{mode = "separate"}). This
#'   character can be used with \code{writeLines()} to generate a LaTeX file.
#'
#' @examples
#' ############ example for "separate" mode: ############
#'
#' mdl <- "
#'   D_o + (1-D_o)*g
#'   (1-D_o)*(1-g)
#'
#'   (1-D_n)*g
#'   D_n + (1-D_n)*(1-g)
#' "
#'
#' tex <- MPTplot(mdl, mode = "separate", stimuli = c("TARGET", "LURE"),
#'                responses = list(c("old", "new"), c("old", "new")),
#'                states = list(c("detect item\\\\as old", "guess old",
#'                                "no detection", "guess new"),
#'                              c("detect item\\\\as new",
#'                                "guess old", "no detection", "guess new")))
#'
#' # show directly in the viewer pane using the texPreview package:
#' if(interactive()){
#' texPreview::tex_preview(tex_lines = tex[[1]])
#' texPreview::tex_preview(tex_lines = tex[[2]])
#' }
#'
#' # or store them using the writeLines() command to use them in your LaTeX UI
#' writeLines(tex[[1]], paste0(tempdir(), "/MPT1.tex"))
#' writeLines(tex[[2]], paste0(tempdir(), "/MPT2.tex"))
#'
#'
#' ############ example for "combine" mode: ############
#'
#' mdl <- "
#'   D + (1-D)*g
#'   (1-D)*(1-g)
#'
#'   (1-D)*g
#'   D + (1-D)*(1-g)
#' "
#'
#' tex <- MPTplot(mdl, mode = "combine", stimuli = c("TARGET", "LURE"),
#'                responses = c("old", "new"),
#'                states = c("detect item", "guess old", "no detection", "guess new"))
#'
#'
#' # show directly in the viewer pane using the texPreview package:
#' if(interactive()){
#' texPreview::tex_preview(tex_lines = tex)
#' }
#'
#' # or store it using the writeLines() command to use it in your LaTeX UI
#' writeLines(tex, paste0(tempdir(), "/MPT.tex"))
#'
#'
#' @importFrom utils modifyList
#' @export
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
  line_width <- min_arrow_width <- prc_box_width <- prc_box_height <- resp_box_width <-
    resp_box_height <- resp_box_dist_x <- resp_box_dist_y <- stim_box_width <-
    stim_box_height <- stim_txt_dist_y <- stim_font_size <- probs_font_size <-
    resp_font_size <- state_font_size <- NULL
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
    tex_lines <- begin_lines(stim_font_size, probs_font_size, resp_font_size, state_font_size)

    # MAKE RESPONSE BOXES
    tex_lines <- c(tex_lines, "% stimulus name for each tree (from n.trees to 1) and corresponding responses:")
    stim_texts <- rev(stim_names)
    for (tr in 1:n.trees) {
      pos_x <- total_width - tr*(resp_box_width+resp_box_dist_x) + resp_box_dist_x
      tex_lines <- c(tex_lines, paste0("\\node[anchor=west] at (", pos_x, ", ", total_height, ") {\\stimFontSize ", stim_texts[tr], "};"), "")
      for (br in 1:n.branches[tr]) {
        pos_y <- body_height - (br-1)*(resp_box_height+resp_box_dist_y)
        tex_lines <- c(tex_lines, paste0("\\draw [thick] (", pos_x, ", ", pos_y-resp_box_height, ") rectangle (", pos_x + resp_box_width, ", ", pos_y, ") node[pos=.5] {\\begin{tabular}{cc} \\respFontSize ", cat_names[[tr]][trees$ordered_cat_pos[[n.trees+1-tr]]][br], " \\end{tabular}};"))
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
        tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos1_y, ") rectangle (", pos_x+prc_box_width, ", ", pos1_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc} \\stateFontSize ", curr_prc_text[1], " \\end{tabular}};"))
        tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos2_y, ") rectangle (", pos_x+prc_box_width, ", ", pos2_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc} \\stateFontSize ", curr_prc_text[2], " \\end{tabular}};"))

        new_pos_y <- mean(c(strip_branches_y[[ind_fill1]][curr_max], strip_branches_y[[ind_fill2]][curr_max]))
        strip_branches_y[[ind_fill1]][curr_max-1] <- new_pos_y
        strip_branches_y[[ind_fill2]][curr_max-1] <- new_pos_y
        tex_lines <- c(tex_lines, "%% You might want to change the text position of these lines:")
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[above=3, align=center] {\\probsFontSize $", curr_prc, "$} (", pos_x, ", ", strip_branches_y[[ind_fill1]][curr_max], ");"))
        tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[below=3, xshift=-13] {\\probsFontSize $(1-", curr_prc, ")$} (", pos_x, ", ", strip_branches_y[[ind_fill2]][curr_max], ");"))
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
      tex_lines <- begin_lines(stim_font_size, probs_font_size, resp_font_size, state_font_size)

      # MAKE RESPONSE BOXES
      tex_lines <- c(tex_lines, "% responses:")
      pos_x <- total_width[tr] - resp_box_width
      for (br in 1:n.branches[tr]) {
        pos_y <- body_height[tr] - (br-1)*(resp_box_height+resp_box_dist_y)
        tex_lines <- c(tex_lines, paste0("\\draw [thick] (", pos_x, ", ", pos_y-resp_box_height, ") rectangle (", pos_x + resp_box_width, ", ", pos_y, ") node[pos=.5] {\\begin{tabular}{cc} \\respFontSize ", cat_names[[tr]][trees$ordered_cat_pos[[tr]]][br], " \\end{tabular}};"))
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
          tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos1_y, ") rectangle (", pos_x+prc_box_width, ", ", pos1_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc} \\stateFontSize ", curr_prc_text[1], " \\end{tabular}};"))
          tex_lines <- c(tex_lines, paste0("\\draw [thick, rounded corners] (", pos_x, ", ", pos2_y, ") rectangle (", pos_x+prc_box_width, ", ", pos2_y+prc_box_height, ") node[pos=.5] {\\begin{tabular}{cc} \\stateFontSize ", curr_prc_text[2], " \\end{tabular}};"))

          new_pos_y <- mean(c(strip_branches_y[[ind_fill1]][curr_max], strip_branches_y[[ind_fill2]][curr_max]))
          strip_branches_y[[ind_fill1]][curr_max-1] <- new_pos_y
          strip_branches_y[[ind_fill2]][curr_max-1] <- new_pos_y
          tex_lines <- c(tex_lines, "%% You might want to change the text position of these lines:")
          tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[above=3, align=center] {\\probsFontSize $", curr_prc, "$} (", pos_x, ", ", strip_branches_y[[ind_fill1]][curr_max], ");"))
          tex_lines <- c(tex_lines, paste0("\\draw [-latex, thick] (", pos_x-line_width, ", ", new_pos_y, ") -- node[below=3, xshift=-13] {\\probsFontSize $(1-", curr_prc, ")$} (", pos_x, ", ", strip_branches_y[[ind_fill2]][curr_max], ");"))
          tex_lines <- c(tex_lines, "")

          len[max_list[[m]][1]] <- len[max_list[[m]][1]] - 1
          len[max_list[[m]][2]] <- 0

          # stimulus box on left side
          if (curr_max == 1) {
            tex_lines <- c(tex_lines, paste0("\\draw [thick] (", 0, ", ", new_pos_y-0.5*stim_box_height, ") rectangle (", stim_box_width, ", ", new_pos_y+0.5*stim_box_height, ") node[pos=.5] {\\begin{tabular}{cc} \\stimFontSize ", stim_names[tr], " \\end{tabular}};"))
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
