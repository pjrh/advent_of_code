
library(stringr)
library(purrr)
library(tictoc)

tic("Process inputs and prep")

input = readr::read_file("day05_input.txt")

fresh_ranges <- input |>
  str_split_1("\n\n") |>
  pluck(1) |>
  str_split_1("\n") |>
  map(~as.numeric(str_split_1(.x, "-")))

ingredient_IDs <- input |>
  str_split_1("\n\n") |>
  pluck(2) |>
  str_split_1("\n") |>
  map_vec(~as.numeric(.x))

toc()

tic("Part 1")

between <- function(x, r) {
  return(x >= r[1] && x <= r[2])
}

spoiled_IDs <- ingredient_IDs
for (r in fresh_ranges) {
  spoiled_IDs <- discard(spoiled_IDs, ~between(.x, r))
}

p1_ans <- length(ingredient_IDs) - length(spoiled_IDs)
print(paste0("Part 1: ", sprintf("%1.0f", p1_ans)))

toc()

tic("Part 2")

# check if a range overlaps
range_overlap <- function(r1, r2) {
  
  if ((r1[[1]] <= r2[[1]]) & (r1[[2]] >= r2[[1]])) {
    return(TRUE)
  } else if ((r1[[1]] >= r2[[1]]) & (r2[[2]] >= r1[[1]])) {
    return(TRUE)
  } else if ((r1[[1]] >= r2[[1]]) & (r1[[2]] <= r2[[2]])) {
    return(TRUE)
  } else if ((r2[[1]] >= r1[[1]]) & (r2[[2]] <= r1[[2]])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

# Increases the range of the first range in a list of 
# ranges which overlaps with a new range to span 
# the union of the ranges - remaining ranges untouched.
# If no ranges overlapped then appends new range to end
# of range list.
append_range <- function(ranges_in, new_range) {
  if (is_empty(ranges_in)) return(list(new_range))
  
  for (i in 1:length(ranges_in)) {
    r1 <- ranges_in[[i]]
    r2 <- new_range
    overlapped <- range_overlap(r1, r2)
    
    if (overlapped) {
      ranges_in[i] <- list(c(min(r1[[1]], r2[[1]]),
                             max(r1[[2]], r2[[2]])))
      break
    }
  }
  
  if (!overlapped) {
    ranges_in <- append(ranges_in, list(new_range))
  }
  
  return(ranges_in)
}

fresh_ranges_in <- fresh_ranges

# Takes each range and appends it to the range list
# one-by-one as above. Repeats this process until
# all ranges are merged and there is no further change
# to the range list.
repeat {
  fresh_ranges_out <- fresh_ranges_in |>
    reduce(append_range, .init = list()) 
  
  if (identical(fresh_ranges_in, fresh_ranges_out)) break
  fresh_ranges_in <- fresh_ranges_out
}

# Sums the spans of ranges (adds one because the ranges are inclusive)
p2_ans <- fresh_ranges_out |>
  unique() |>
  map_vec(~(1+.x[[2]]-.x[[1]])) |>
  sum()

print(paste0("Part 2: ", sprintf("%1.0f", p2_ans)))

toc()

