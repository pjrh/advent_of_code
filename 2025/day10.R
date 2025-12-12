
library(purrr)
library(stringr)
library(dplyr)
library(tictoc)

input <- readr::read_lines("day10_input.txt") |>
  map_vec(\(x) {
    bits <- str_split_1(x, "[\\[\\]{}]") |>
      discard(\(x) {x == ""})
    
    bits[1] <- str_replace_all(bits[1], "[.]", "0") |>
      str_replace_all("[#]", "1") |>
      str_split("") |>
      map(~as.numeric(.x))
    
    bits[2] <- str_remove_all(bits[2], " ") |>
      str_remove("^\\(") |>
      str_remove("\\)$") |>
      str_split("\\)\\(") |>
      map(~str_split(.x, ",")) |>
      map_depth(2, ~as.numeric(.x))
    
    bits[3] <- bits[3] |>
      map(~as.numeric(str_split_1(.x, ",")))
    
    
    return(list(bits))
  })


find_presses <- function(buttons,target) {
  
  M <- 0:(length(target)-1) |>
    map(\(x) {buttons |>
        map_vec(~(x %in% .x))}) |>
    unlist() |>
    as.numeric() |>
    matrix(nrow = length(target), byrow = TRUE)
  
  presses <- 0
  
  repeat{
    
    onoff_perms <- RcppAlgos::permuteGeneral(0:1, m=dim(M)[2], repetition = TRUE,
                                             constraintFun = "sum",
                                             comparisonFun = "==",
                                             limitConstraints = presses)
    
    for (i in 1:dim(onoff_perms)[1]) {
      found <- all(M%*%onoff_perms[i,] %% 2 == target)
      if (found) break
    }
    if (found) break
    presses <- presses + 1
  }
  
  return(presses)
}



p1_ans <- input %>%
  map_vec(\(x) find_presses(x[[2]], x[[1]])) |>
  sum()
print("Part 1:")
print(p1_ans)


buttons_matrix <- function(buttons) {
  0:max(unlist(buttons)) |>
    map(\(x) {buttons |>
        map_vec(~(x %in% .x))}) |>
    unlist() |>
    as.numeric() |>
    matrix(nrow = length(target), byrow = TRUE)
}


# target <- input[[1]][[3]]
# buttons <- input[[1]][[2]]
# 
# M <- buttons_matrix(buttons)
# matlib::echelon(cbind(M, target))

# [1,] 1 0 0 0 0 0 0 0 0 0 -1.0  0  1.0  103.0
# [2,] 0 1 0 0 0 0 0 0 0 0  1.0  0  0.0   16.0
# [3,] 0 0 1 0 0 0 0 0 0 0  3.0  0  0.0   48.0
# [4,] 0 0 0 1 0 0 0 0 0 0 -3.0  0  0.0  -30.0
# [5,] 0 0 0 0 1 0 0 0 0 0  0.0  1  0.0    3.0
# [6,] 0 0 0 0 0 1 0 0 0 0  4.0 -1  0.0   51.0
# [7,] 0 0 0 0 0 0 1 0 0 0 -2.5  1  0.5  -18.5
# [8,] 0 0 0 0 0 0 0 1 0 0 -0.5  0 -0.5   11.5
# [9,] 0 0 0 0 0 0 0 0 1 0 -0.5  0  0.5   17.5
# [10,] 0 0 0 0 0 0 0 0 0 1 -2.0  0  0.0  -21.0
# 
# c <- b |> mutate(`1` = 103 + 1*`11` - `13`,
#                  `2` = 16 - `11`,
#                  `3` = 48 - 3*`11`,
#                  `4` = -30 -3*`11`,
#                  `5` = 3 - `12`,
#                  `6` = 51  - 4*`11` +`12`,
#                  `7` = -18.5 + 2.5*`11` -`12`-0.5*`13`,
#                  `8` = 11.5 +0.5*`11` + 0.5*`12`,
#                  `9` = 17.5 + 0.5*`11` + 0.5*`13`,
#                  `10` = -21 +2*`11`)
# 
# c <- c %>%
#   select(order(as.numeric(colnames(c))))

find_presses2 <- function(buttons,target) {
  
  M <- buttons_matrix(buttons)
  
  rs <- rowSums(M)
  row_ordering <- order(rs)
  M_sort <- M[row_ordering,]
  target_sort <- target[row_ordering]
  
  
  b <- RcppAlgos::permuteGeneral(0:target_sort[1], m=sum(M_sort[1,]), repetition = TRUE,
                                 constraintFun = "sum",
                                 comparisonFun = "==",
                                 limitConstraints = target_sort[1])
  
  b_buttons <- which(as.logical(M_sort[1,]))
  
  colnames(b) <- as.character(b_buttons)
  b <- tibble::as_tibble(b)
  
  for (row_ind in 1:length(rs)) {
    
    b2_buttons <- which(as.logical(M_sort[row_ind,]))
    new_cols <- setdiff(as.character(b2_buttons), colnames(b))
    overlapping_cols <- intersect(as.character(b2_buttons), colnames(b))
    
    if (is_empty(new_cols)) {
      b <- b |>
        mutate(s = rowSums(select(b,all_of(overlapping_cols)))) |>
        filter(s == target_sort[row_ind]) |>
        select(-s)
    } else if (length(overlapping_cols) > 0) {
      
      rows_summed <- b |> 
        dplyr::select(all_of(overlapping_cols)) |>
        rowSums() |>
        unique()
      
      b3 <- tibble::tibble(.rows = 0)
      for (rr in rows_summed) {
        mm <- sum(M_sort[row_ind,])-length(overlapping_cols)
        b2 <- RcppAlgos::permuteGeneral(0:(target_sort[row_ind]-rr), m=mm, repetition = TRUE,
                                        constraintFun = "sum",
                                        comparisonFun = "==",
                                        limitConstraints = target_sort[row_ind]-rr)
        
        colnames(b2) <- new_cols
        b2 <- tibble::as_tibble(b2)
        
        c <- b |>
          mutate(s = rowSums(select(b,all_of(overlapping_cols)))) |>
          filter(s == rr) |>
          select(-s)
        b3 <- rbind(b3, dplyr::cross_join(c, b2))
        
      }
      b <- b3
    }  else {
      b2 <- RcppAlgos::permuteGeneral(0:(target_sort[row_ind]), m=sum(M_sort[row_ind,]), repetition = TRUE,
                                      constraintFun = "sum",
                                      comparisonFun = "==",
                                      limitConstraints = target_sort[row_ind])
      
      colnames(b2) <- b2_buttons
      b2 <- tibble::as_tibble(b2)
      
      b <- dplyr::cross_join(b,
                             b2)
    }
  }
  
  
  
  Mb <- b %>%
    dplyr::select(order(as.numeric(colnames(b)))) |>
    as.matrix()
  
  
  Mbs <- rowSums(Mb)
  MBrow_ordering <- order(Mbs)
  Mb <- Mb[MBrow_ordering,, drop=FALSE]
  
  
  
  for (r_ind in 1:dim(Mb)[1]) {
    
    found <- all(M_sort%*%Mb[r_ind,] == target_sort)
    if (found) {
      #print("Got one!")
      return(sum(Mb[r_ind,]))
    }
  }
  
  
  return(NULL)
  
}

# target <- input[[5]][[3]]
# buttons <- input[[5]][[2]]
# find_presses2(buttons, target)

tic("Part 2")
p2_ans_arr <- array(dim = length(input))
for (ans_i in 1:length(input)) {
  target <- input[[ans_i]][[3]]
  buttons <- input[[ans_i]][[2]]
  tic()
  try(p2_ans_arr[ans_i] <- find_presses2(buttons, target))
  print(ans_i)
  toc()
}
print(p2_ans_arr)
toc()
# can't do 11

