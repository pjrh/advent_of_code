
library(stringr)
library(collections)
library(tictoc)

tic("Process inputs and prep")

input = readr::read_file("day07_input.txt")

nrows = input |> str_count("\n") + 1
ncols = input |> str_count("[.S^]")/nrows

# Matrix where splitters are 1 and space is 0
M_in <- input |>
  str_remove_all("\n") |>
  str_replace_all("[.S]", "0") |>
  str_replace_all("[\\^]", "1") |>
  str_split_1("") |>
  purrr::map_vec(~as.numeric(.x)) |>
  matrix(nrow = nrows, ncol = ncols, byrow = TRUE)

# Matrix where beam end is 1
B_in <- input |>
  str_split_1("\n") |>
  dplyr::first() |>
  str_locate("S") |>
  as.vector() |>
  unique()

# Shift matrix upper
S1 <- matrix(0, nrows, nrows)
for (r in 1:(nrows-1)) {
  S1[r, r+1] <- 1
}

# Shift matrix lower
S2 = t(S1)

beam_array_split <- function(beam_array, split_loc) {
  if (split_loc %in% beam_array) {
    beam_end_logical <- beam_array == split_loc
    beam_array[beam_end_logical] <- (split_loc - 1)
    #beam_array <- c(beam_array, split_loc+1) |> unique()
    beam_array <- c(beam_array, replicate(sum(beam_end_logical), split_loc+1))
  }
  return(beam_array)
}

beam_dict_split <- function(beam_dict, split_loc) {
  if (split_loc %in% beam_dict$keys()) {
    beam_dict$set(split_loc-1L, 
                  beam_dict$get(split_loc-1L,0) + beam_dict$get(split_loc))
    
    beam_dict$set(split_loc+1L, 
                  beam_dict$get(split_loc+1L,0) + beam_dict$get(split_loc))
    
    beam_dict$remove(split_loc)
  }
  return(beam_dict)
}

toc()

tic("Part 1")

split_counter <- 0
M <- M_in
B <- B_in
B_dict <- dict()
B_in %>%
  purrr::walk(~B_dict$set(.x[[1]], 1))

for (r in 1:nrows) {
  M <- S1 %*% M
  splitters <- which(as.logical(M[1,]))
  
  for (s in splitters) {
    if (s %in% unlist(B_dict$keys())) {
      split_counter <- split_counter + 1
      #B <- beam_array_split(B,s)
      B_dict <- beam_dict_split(B_dict,s)
    }
  }
}

print("Part 1:")
print(split_counter)

print("Part 2:")
p2_ans <- sum(unlist(B_dict$values()))
print(sprintf("%1.0f", p2_ans))


toc()