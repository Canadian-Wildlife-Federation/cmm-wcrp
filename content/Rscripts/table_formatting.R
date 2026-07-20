library("flextable")
library("magrittr")
library("officer")
library("dplyr")

std_border <- fp_border(color = "grey")

# Table formatting for csv's
format_flextable <- function(ft) {
  ft <- ft %>%
    colformat_num(big.mark = "", decimal.mark = ".", digits = 2) %>%
    bg(bg = "#008270", part = "header") %>%
    color(color = "white", part = "header") %>%
    set_caption() %>%
    align_text_col(align = "left", header = TRUE) %>%
    align_nottext_col(align = "left", header = TRUE) %>%
    vline(part = "all", border = std_border) %>%
    hline(part = "all", border = std_border) %>%
    autofit()
  return(ft)
}


# Table sorting for priority tables
create_sorted_flextable <- function(df) {
  # Step 1: Sort the data frame
  df <- df %>%
    mutate(Comparison_Column = pmax(`Total Habitat Gain in Set - AS (km)`, `Total Habitat Gain in Set - AE (km)`, na.rm = TRUE))

  # Step 3: Sort the data frame using the comparison column
  sorted_df <- df %>%
    arrange(
      dplyr::desc(is.na(Comparison_Column)),
      desc(Comparison_Column)
    ) %>%
    select(-Comparison_Column)

  # Step 4: Create a flextable
  ft <- flextable(sorted_df)

  # Return the flextable
  return(ft)
}