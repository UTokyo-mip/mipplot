#' Manual coloring
#'
#' Reads manual color table file
#'
#' @param filepath color table file, which columns include "Right_side" and "Color_code".
#' @param category_separator regular expression for separating
#' right-hand-side variable name into categories.
#' For example: separator should be "\\|" for "Secondary Energy|Electricity|Coal"
#' @return list. which elements are mapping from variable name to color
#'
#' @export mipplot_read_color_table

mipplot_read_color_table <-
  function(filepath, category_separator = "\\|")
  {
    # Read raw color table from file.
    raw_table <- read.csv(filepath, stringsAsFactors = FALSE)

    ####################################################################
    # Add column of variable names that include only most detailed part.
    ####################################################################

    # Variable names that include non-detailed part.
    non_detailed_variable_names <- raw_table$Right_side

    print(str(non_detailed_variable_names))

    # Number of elements of variable names
    n_names <- length(non_detailed_variable_names)

    # Initialize detailed variable names
    detailed_variable_names <- rep("", n_names)

    for (i in 1:n_names) {

      # Target non-detailed variable name
      non_detailed_variable_name <- non_detailed_variable_names[i]

      # Skip if non-detailed variable name is empty
      if (non_detailed_variable_name == "") next

      # Split variable name into categories
      category_names <- strsplit(non_detailed_variable_name, split = category_separator)[[1]]

      # Extract the name of most detailed category
      name_of_deepest_category <- tail(category_names, n = 1)

      # Store the category name as detailed variable name
      detailed_variable_names[i] <- name_of_deepest_category
    }

    # Add detailed variable name to dataframe as a new column.
    raw_table$detailed_variable_name <- detailed_variable_names

    ####################################################################
    # Create ggplot2 scale object (mapping from variable name to color)
    ####################################################################

    # Empty mapping object (list with names attributes)
    mapper_from_variable_name_to_color = c()

    for (i in 1:n_names) {

      # Skip if detailed variable name is empty
      if (raw_table$detailed_variable_name[i] == "") next

      # Upsert (update or insert) a mapping from variable name to
      mapper_from_variable_name_to_color[raw_table$detailed_variable_name[i]] <-
        raw_table$Color_code[i]
    }

    return(mapper_from_variable_name_to_color)

  }
