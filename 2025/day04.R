
library(stringr)
library(tictoc)

tic("Process inputs and prep")

input = readr::read_file("day04_input.txt")

nrows = input |> str_count("\n") + 1
ncols = input |> str_count("[.@]")/nrows

# Matrix where paper rolls are 1 and space is 0
M <- input |>
  str_remove_all("\n") |>
  str_replace_all("[.]", "0") |>
  str_replace_all("@", "1") |>
  str_split_1("") |>
  purrr::map_vec(~as.numeric(.x)) |>
  matrix(nrow = nrows, ncol = ncols, byrow = TRUE)

# Shift matrix upper
S_shift1 <- matrix(0, nrows, nrows)
for (r in 1:(nrows-1)) {
  S_shift1[r, r+1] <- 1
}

# Shift matrix lower
S_shift2 = t(S_shift1)


# Function takes the sum of the input matrix shifted in all eight directions
# which is the sum of the number of adjacent rolls
# and then returns a matrix of the locations of where there was an input roll
# and there were fewer than four adjacent rolls
find_accessible_rolls <- function(M) {
  M_adjacents <- 
    S_shift1 %*% M + #N
    S_shift2 %*% M + #S
    M %*% S_shift1 + #E
    M %*% S_shift2 + #W
    S_shift1 %*% M %*% S_shift1 + #NE
    S_shift1 %*% M %*% S_shift2 + #NW
    S_shift2 %*% M %*% S_shift1 + #SE
    S_shift2 %*% M %*% S_shift2   #SW
  
  return(M*(M_adjacents < 4))
}

toc()

tic("Part 1")

print(paste0("Part 1: ", sum(find_accessible_rolls(M))))

toc()

tic("Part 2")

M_in <- M
roll_total <- 0

# Repeats the roll finding until there are no
# new rolls.
repeat{
  M_out <- find_accessible_rolls(M_in)
  new_rolls <- sum(M_out)
  if (new_rolls == 0) break
  roll_total <- roll_total + new_rolls
  M_in <- M_in - M_out # remove the found rolls from input to next loop
}

print(paste0("Part 2: ", sum(roll_total)))

toc()
