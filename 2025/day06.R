
library(stringr)
library(purrr)
library(tictoc)

input_file <- "day06_input.txt"

tic("Part 1")

input_p1 = readr::read_file(input_file) |>
  str_split_1("\n") |> # Split input into rows
  map(~str_split_1(.x, " ")) |> # Split rows into lists
  map(~discard(.x, ~(.x==""))) |> # Remove blank elements of lists
  list_transpose() |>
  map(~list(numbers = as.numeric(.x[1:(length(.x)-1)]),
            op = .x[[length(.x)]],
            func = ifelse(.x[[length(.x)]] == "*",
                          \(x) prod(x),
                          \(x) sum(x))))

p1_ans <- input_p1 %>%
  map_vec(~.x$func(.x$numbers)) %>%
  sum()

print("Part 1:")
print(sprintf("%1.0f", p1_ans))

toc()

tic("Part 2")
  
input_p2 = readr::read_file(input_file) |>
  str_replace_all("[+*]", " ") |> 
  str_split_1("\n") |>
  map(~str_split_1(.x, "")) |>
  list_transpose() |>
  unlist() |>
  str_c(collapse = "") %>%
  str_split_1("[ ]{5}") %>% # this 5 needs to be the number of rows in the input file (or one more is fine)
  map(~str_split_1(.x, " ")) |>
  map(~discard(.x, ~(.x==""))) |> # Remove blank elements of lists
  discard(~is_empty(.x)) |> # Discard empty elements
  map(~as.numeric(.x))

# Reuse the operators from part 1
p2_ans <- map2_vec(input_p2, input_p1, ~.y$func(.x)) %>%
  sum()

print("Part 2:")
print(sprintf("%1.0f", p2_ans))

toc()
