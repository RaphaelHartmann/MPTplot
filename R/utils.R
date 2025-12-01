
######### CONSTANTS #########

begin_lines <- function(stim_font_size, probs_font_size, resp_font_size, state_font_size) {
  c("\\documentclass[tikz,border=2pt]{standalone}",
    "",
    "\\usepackage{tikz}",
    "",
    paste0("\\newcommand{\\stimFontSize}{\\", stim_font_size, "}"),
    paste0("\\newcommand{\\probsFontSize}{\\", probs_font_size, "}"),
    paste0("\\newcommand{\\respFontSize}{\\", resp_font_size, "}"),
    paste0("\\newcommand{\\stateFontSize}{\\", state_font_size, "}"),
    "",
    "\\begin{document}",
    "",
    "\\begin{tikzpicture}",
    "",
    "")
}

end_lines <- function() {c(
  "",
  "\\end{tikzpicture}",
  "",
  "\\end{document}",
  "")
}

######### DEFAULTS #########

get_default <- function() {
  list(
    # lines for process outcomes:
    line_width = 2,

    # lines for last process to response:
    min_arrow_width = 1.5,

    # text boxes with round corners for each process:
    prc_box_width = 2.5,
    prc_box_height = 1,

    # response boxes:
    resp_box_width = 2,
    resp_box_height = 1,
    resp_box_dist_x = 0.5,
    resp_box_dist_y = 0.5,

    # stimulus box
    stim_box_width = 2,
    stim_box_height = 1,

    # stimulus text above response boxes
    stim_txt_dist_y = 0.5,

    # stimulus text size
    stim_font_size = "large",

    # font size probability parameters
    probs_font_size = "Large",

    # font size latent states
    state_font_size = "normalsize",

    # font size probability parameters
    resp_font_size = "normalsize"
  )
}

######### CHECKS #########

check_structure <- function(mode, n.params, n.branches, n.categories) {
  if (mode == "combine") {
    if (length(unique(n.params)) != 1) {
      stop("the trees do not even share the same number of parameters")
    }
    if (length(unique(n.branches)) != 1) {
      stop("the trees do not even share the same number of branches")
    }
    if (length(unique(n.categories)) != 1) {
      stop("the trees do not even share the same number of response categories")
    }
  }
}

check_stimuli <- function(stimuli, n.trees) {
  if (!is.character(stimuli)) stop("stimuli must be a character (vector)")
  if (length(stimuli) != n.trees) stop("stimuli must be of the same length as number of trees")
}

check_responses <- function(responses, n.trees, n.categories, mode) {
  if (!all(sapply(responses, is.character))) stop("responses must be a vector or list of characters")
  if (length(responses) != n.trees) stop("responses list is not same length as number of trees")
  if (mode == "combine") {
    if (!all(sapply(responses, length) == n.categories[1])) stop("for mode \"combine\" all trees must have the same number of response categories")
  }
}

check_states <- function(states, n.params, n.trees, mode) {
  if (!all(sapply(states, is.character))) stop("states must be a vector or list of characters")
  if (length(states) != n.trees) stop("states list is not same length as number of trees")
  if (mode == "combine") {
    if (!all(sapply(states, length) == n.params[1])) stop("for mode \"combine\" all trees must have the same number of parameters")
  }
}

