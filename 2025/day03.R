
library(purrr)
library(stringr)
library(tictoc)

tic("Input prep")

input <- readr::read_file("day03_test.txt")

toc()

tic("Part 1")

max_joltage <- function(x) {
  ind1 <- which(x[1:(length(x)-1)] == max(x[1:(length(x)-1)]))[1]
  ind2 <- ind1 + which(x[(ind1+1):length(x)] == max(x[(ind1+1):length(x)]))[1]
  
  x[[ind1]]*10+x[[ind2]]
}

input |>
  str_split_1("\n") |>
  map(~as.numeric(str_split_1(.x,""))) |>
  map_vec(~max_joltage(.x)) |>
  sum() |>
  (\(x) print(paste0("Part 1: ", x)))()

toc()

tic("Part 2")

max_joltage_p2 <- function(x) {
  ind1 <- which(x[1:(length(x)-11)] == max(x[1:(length(x)-11)]))[1]
  ind2 <- ind1 + which(x[(ind1+1):(length(x)-10)] == max(x[(ind1+1):(length(x)-10)]))[1]
  ind3 <- ind2 + which(x[(ind2+1):(length(x)-9)] == max(x[(ind2+1):(length(x)-9)]))[1]
  ind4 <- ind3 + which(x[(ind3+1):(length(x)-8)] == max(x[(ind3+1):(length(x)-8)]))[1]
  ind5 <- ind4 + which(x[(ind4+1):(length(x)-7)] == max(x[(ind4+1):(length(x)-7)]))[1]
  ind6 <- ind5 + which(x[(ind5+1):(length(x)-6)] == max(x[(ind5+1):(length(x)-6)]))[1]
  ind7 <- ind6 + which(x[(ind6+1):(length(x)-5)] == max(x[(ind6+1):(length(x)-5)]))[1]
  ind8 <- ind7 + which(x[(ind7+1):(length(x)-4)] == max(x[(ind7+1):(length(x)-4)]))[1]
  ind9 <- ind8 + which(x[(ind8+1):(length(x)-3)] == max(x[(ind8+1):(length(x)-3)]))[1]
  ind10 <- ind9 + which(x[(ind9+1):(length(x)-2)] == max(x[(ind9+1):(length(x)-2)]))[1]
  ind11 <- ind10 + which(x[(ind10+1):(length(x)-1)] == max(x[(ind10+1):(length(x)-1)]))[1]
  ind12 <- ind11 + which(x[(ind11+1):(length(x)-0)] == max(x[(ind11+1):(length(x)-0)]))[1]
  
  x[[ind1]]*10^11+x[[ind2]]*10^10+x[[ind3]]*10^9+x[[ind4]]*10^8+x[[ind5]]*10^7+x[[ind6]]*10^6+x[[ind7]]*10^5+x[[ind8]]*10^4+x[[ind9]]*10^3+x[[ind10]]*10^2+x[[ind11]]*10^1+x[[ind12]]*10^0
}

input |>
  str_split_1("\n") |>
  map(~as.numeric(str_split_1(.x,""))) |>
  map_vec(~max_joltage_p2(.x)) |>
  sum() |>
  (\(x) print(paste0("Part 2: ", x)))()

toc()

tic("Part 2 recursive")

# x is a vector of numbers
# n is the number of digits (batteries) required in the answer
max_joltage_p2_r <- function(x,n) {
  if (n==0) return(0) # Stop if no digits remaining to find
  
  # Ignore the trailing digits so there are space for the remaining 10s places
  x_head <- x[1:(length(x)-(n-1))]
  # Find the index of the largest digit in the relevant portion of the batteries
  ind <- which(x_head == max(x_head))[1]
  
  # Returns the value of the nth digit in correct 10s place
  # plus the value of the n-1th digit in the correct 10s place
  return(x[[ind]]*10^(n-1) + max_joltage_p2_r(x[(ind+1):length(x)], n-1))
}

input |>
  str_split_1("\n") |> # Split input into rows
  map(~as.numeric(str_split_1(.x,""))) |> # Make each row a vector of numbers
  map_vec(~max_joltage_p2_r(.x, 12)) |> # Run the function on each row
  sum() |> # Sum the result
  (\(x) print(paste0("Part 2 (recursive): ", x)))() # Enjoy the result

toc()
