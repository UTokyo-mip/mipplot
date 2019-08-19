#' Complementation of color scheme
#'
#' fill colors automatically
#'
#' @param rule_table_without_colors Incomplete color specification rule table. It dosen't contain "Color_code" column.
#' @return Complete color specification rule table. It is containing "Color_code" column.
#' However, if color complementation can not be performed
#' automatically, the return value is an incomplete color specification.
#'
#' @export
#'

mipplot_autofill_color <- function(rule_table_without_colors) {

  random_colors_for_the_not_matched <-
    c("#f643eb", "#109dc2", "#343520", "#b148d4", "#887900",
      "#515284", "#1f8ea2", "#48ab93", "#ecc22e", "#2cbe5f",
      "#0f5b90", "#6828b3", "#331643", "#17ca46", "#b30fb1",
      "#45aa57", "#125f23", "#ff3eea", "#8422be", "#ee9f8c",
      "#6bc3bc", "#517386", "#8a65ee", "#41845d", "#23dd15",
      "#fa42e5", "#3e424a", "#86da6e", "#2143c2", "#00207f",
      "#9a65f9", "#2eb822", "#24a78c", "#086ec1", "#61ddea",
      "#86bb03", "#2356ae", "#821e4b", "#f38460", "#6f80c5",
      "#9d3a31", "#86596c", "#805aae")

  rule_table_with_colors <- rule_table_without_colors

  ith_LHS <- NA

  n_rule <- nrow(rule_table_with_colors)

  for (i_rule in 1:n_rule) {

    ith_rule <- rule_table_with_colors[i_rule, ]

    # print(paste("i_rule:", i_rule))

    # if new LHS is found, update LHS
    if (has_LHS_in(ith_rule)) {
      ith_LHS <- extract_LHS_from_rule(ith_rule)
    }

    # if RHS is empty, skip the row
    if (nchar(ith_rule$Right_side) == 0) next

    # if color code is already filled
    if (nchar(ith_rule$Color_code) > 0) next

    ith_rule$Left_side <- ith_LHS

    V <- extract_specific_category_from_rule(ith_rule)

    # print(paste('V:', V))

    standard_color_scheme_table <- mipplot::mipplot_default_color_palette[[1]]

    n_standard_color_scheme <- length(standard_color_scheme_table)

    distance_list <- numeric(n_standard_color_scheme)

    for (i_standard_color_scheme in 1:n_standard_color_scheme) {

      V_prime <- names(standard_color_scheme_table)[i_standard_color_scheme]

      # print(paste('V_prime:', V_prime))

      distance <- levenshtein_distance(tolower(V), tolower(V_prime))

      distance_list[i_standard_color_scheme] <- distance

    }

    minimum_distance <- min(distance_list)

    # if minimum Levenshtein distance is under the threshold,
    # best mached V_prime color specification will be copied to
    # V color specification.
    DISTANCE_THRESHOLD <- as.integer(max(nchar(V), nchar(V_prime)) * 0.8)

    if (minimum_distance < DISTANCE_THRESHOLD) {

      i_minimum_distance <- which.min(distance_list)
      rule_table_with_colors[i_rule, INDEX_COL_COLOR_CODE] <-
        standard_color_scheme_table[i_minimum_distance]

      print(paste(
        '[message] ',
        "'", V, "'", ' matched to ',
        "'", names(standard_color_scheme_table)[i_minimum_distance], "'",
        sep = ''))

    }else{

      random_color_code <- random_colors_for_the_not_matched[1]
      random_colors_for_the_not_matched <-
        random_colors_for_the_not_matched[
          2:length(random_colors_for_the_not_matched)]
      if (length(random_colors_for_the_not_matched) == 0) {

        # for running out
        random_colors_for_the_not_matched <- c("#000000")
      }
      rule_table_with_colors[i_rule, INDEX_COL_COLOR_CODE] <-
        random_color_code

      print(paste(
        '[message] ',
        'Similar name of variable to ',
        "'", V, "'",' is not found. ',
        'random color code ', random_color_code, ' is inserted.',
        sep = ''))
    }
  }
  return(rule_table_with_colors)

}

INDEX_COL_LHS <- 2
INDEX_COL_RHS <- 3
INDEX_COL_COLOR_CODE <- 4

extract_LHS_from_rule <- function(rule) {
  return(rule[1, INDEX_COL_LHS])
}

has_LHS_in <- function(rule) {

  # if the LHS column is empty
  if (nchar(rule[1, INDEX_COL_LHS]) == 0) {

        return(FALSE)

  }else{

        return(TRUE)
  }
}

extract_specific_category_from_rule <- function(rule) {

  LHS <- rule[1, INDEX_COL_LHS]
  RHS <- rule[1, INDEX_COL_RHS]

  category <- LHS

  specific_category <- gsub(paste(category, "|", sep=""), "", RHS, fixed = TRUE)

  return(specific_category)

}


levenshtein_distance <- function(s, t) {

  m <- nchar(s)
  n <- nchar(t)

  # Attention:
  # d[i, j] (in wikipedia) := d_[i+1, j+1]
  d <- matrix(0, nrow = m + 1, ncol = n + 1)

  for (i in 1:m) {
    d[i+1, 0+1] <- i
  }

  for (j in 1:n) {
    d[0+1, j+1] <- j
  }

  for (j in 1:n) {
    for (i in 1:m) {
      if (substr(s, i, i) == substr(t, j, j)) {
        substitution_cost <- 0
      } else {
        substitution_cost <- 1
      }
      d[i+1, j+1] <- min(
        d[i-1+1, j+1]+1,  # deletion
        d[i+1, j-1+1]+1,  # insertion
        d[i-1+1, j-1+1] + substitution_cost) # substitution
    }
  }

  return(d[m+1, n+1])
}

