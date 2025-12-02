
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(tictoc)

tic("Input prep")

input = readr::read_file("day02_input.txt")

input_df <- input |>
  str_split(",") |>
  unlist() |>
  as_tibble() %>%
  separate(value, into = c("n1", "n2"), sep = "-") |>
  mutate(n1 = as.numeric(n1),
         n2 = as.numeric(n2))

int_length <- function(x) {
  floor(log10(x)) + 1
}

repeated_number <- function(x, repeats) {
  n = int_length(x)
  
  out = 0
  for (r in 1:repeats) {
    out = out + x*10^((r-1)*n)
  }
  return(out)
}

max_input_number <- input |>
  str_split("[,-]") %>%
  unlist()  %>%
  map_vec(~as.numeric(.x)) %>%
  max()

max_n = ceiling(int_length(max_input_number)/2)

toc()
tic("Part 1")

invalid_ids_df <- repeated_number(seq(1,10^max_n),2) |>
  as_tibble()



invalid_ids_df |>
  inner_join(input_df,
             by = join_by(between(value, n1, n2))) |>
  pull(value) |>
  sum() |>
  (\(x) print(paste0("Part 1: ", x)))()

toc()
tic("Part 2")

invalid_ids_p2 = c(repeated_number(seq(1,10^max_n),2),
                   repeated_number(seq(1,10^max_n),3),
                   repeated_number(seq(1,10^max_n),4),
                   repeated_number(seq(1,10^max_n),5),
                   repeated_number(seq(1,9),6),
                   repeated_number(seq(1,9),7),
                   repeated_number(seq(1,9),8),
                   repeated_number(seq(1,9),9),
                   repeated_number(seq(1,9),10)) %>%
  unique() |>
  sort()

invalid_ids_p2_df <- as_tibble(invalid_ids_p2) %>%
  filter(value < max_input_number)

invalid_ids_p2_df |>
  inner_join(input_df,
             by = join_by(between(value, n1, n2))) |>
  pull(value) |>
  sum() |>
  (\(x) print(paste0("Part 2: ", x)))()

toc()
    