
library(purrr)
library(stringr)
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


tic("Part 2")
p2_ans_arr <- array(dim = length(input))
for (ans_i in 1:length(input)) {
  #ans_i <- 111
  #print(ans_i)
  target <- input[[ans_i]][[3]]
  buttons <- input[[ans_i]][[2]]
  M <- buttons_matrix(buttons)
  colnames(M) <- as.character(1:length(buttons))
  eschelon <- matlib::echelon(cbind(M, target))
  
  pivot_cols <- list()
  pivot_rows <- list()
  ii <- 1
  for (i in 1:dim(M)[1]) {
    row <- eschelon[i,1:dim(eschelon)[2]-1]
    first_1 <- which(row==1)[1]
    check_zeros <- !is.na(first_1) && ifelse(first_1 == 1, TRUE, all(row[1:(first_1-1)]==0))
    if (check_zeros) {
      pivot_cols[ii] <- colnames(M)[first_1]
      pivot_rows[ii] <- i
      ii <- ii+1
    }
  }
  names(pivot_rows) <- pivot_cols
  
  free_vars <- setdiff(as.character(1:length(buttons)), pivot_cols)
  
  if (length(free_vars) == 0) {
    p2_ans_arr[ans_i] <- sum(eschelon[,"target"])
  } else {
    
    permutes <- RcppAlgos::permuteGeneral(0:max(target), m=length(free_vars), repetition = TRUE)
    colnames(permutes) <- free_vars
    
    permutes2 <- cbind(matrix(0,nrow = dim(permutes)[1], ncol = length(pivot_cols)), permutes)
    colnames(permutes2)[1:length(pivot_cols)] <- pivot_cols
    permutes2 <- permutes2[,order(as.numeric(colnames(permutes2)))]
    
    for (i in rev(pivot_cols)) {
      permutes2[,i] = eschelon[pivot_rows[[i]],1+dim(M)[2]] - rowSums(sweep(permutes2, MARGIN = 2, eschelon[pivot_rows[[i]],1:dim(M)[2]], `*`))
    }
    
    permutes2 <- permutes2[order(rowSums(permutes2)),]
    permutes2 <- round(permutes2)
    
    for (row_ind in 1: dim(permutes2)[1]) {
      check_perm <- permutes2[row_ind,,drop = FALSE]
      
      if (all(check_perm >=0) && all(M%*%t(check_perm)==target)) {
        p2_ans_arr[ans_i] <- sum(check_perm)
        break
      }
    }
  }
}
print("Part 2:")
print(sum(p2_ans_arr))
toc()

stop()
# Everything below here was a waste of time because it was slow and 
# couldn't do four of the inputs :(

compile_constraints <- function(M,target) {
  constraints <- list()
  for (i in 1:dim(M)[1]) {
    constraints[[i]] <- list("buttons" = as.character(which(as.logical(M[i,]))),
                             "sum" = target[i])
    
  }
  return(constraints)
}

find_constraint_order <- function(constraints, init_ind = NULL, verbose = TRUE) {
  
  ordering_cost <- c()
  set_order <- c()
  buttons_gen = c()
  
  constraints_out <- list()
  
  constraints_left <- constraints %>% 
    imap(~list("buttons" = .x$buttons, "sum" = .x$sum, "index" = .y))
  for (ordering in 1:length(constraints)) {
    
    constraints_choices <- constraints_left |>
      map(\(x) {
        new_cols <- setdiff(x$buttons, buttons_gen)
        overlapping_cols <- intersect(buttons_gen, x$buttons)
        if (is_empty(new_cols)) {
          rough_perm_rows <- 1
        } else {
          rough_perm_rows <- RcppAlgos::permuteIter(0:x$sum,
                                                    m = length(x$buttons)-length(overlapping_cols),
                                                    constraintFun = "sum",
                                                    comparisonFun = "==",
                                                    limitConstraints = x$sum)$summary()$totalResults
        }
        
        return(list("buttons" = x$buttons,
                    "sum" = x$sum,
                    "new_cols" = new_cols,
                    "overlapping_cols" = overlapping_cols,
                    "rough_perm_rows" = rough_perm_rows,
                    "index" = x$index))
      })
    
    new_col_opts <- constraints_choices |>
      map_vec(~length(.x$new_cols))
    
    perm_cost_opts <- constraints_choices |>
      map_vec(~.x$rough_perm_rows)
    
    if (ordering == 1) {
      next_action <- "first gen"
      if (is.null(init_ind)) {
        next_constraint <- min(which(perm_cost_opts == min(perm_cost_opts)))
      } else {
        next_constraint <- init_ind
      }
      next_cost <- constraints_choices[[next_constraint]]$rough_perm_rows
    } else if (any(new_col_opts == 0)) {
      next_constraint <- min(which(new_col_opts == 0))
      next_cost <- 1
      next_action <- "filter"
    } else if (any(new_col_opts == 1)) {
      next_constraint <- min(which(new_col_opts == 1))
      next_cost <- 1
      next_action <- "constrained add"
      
    } else {
      next_constraint <- min(which(perm_cost_opts == min(perm_cost_opts)))
      next_cost <- constraints_choices[[next_constraint]]$rough_perm_rows
      
      if (length(constraints_choices[[next_constraint]]$overlapping_cols)>0) {
        next_action <- "constrained permute"
      } else {
        next_action <- "unconstrained permute"
      }
    }
    
    constraints_out[ordering] <- constraints_choices[next_constraint]
    constraints_out[[ordering]]$cost <- next_cost
    constraints_out[[ordering]]$action <- next_action
    constraints_left[next_constraint] <- NULL
    buttons_gen <- union(buttons_gen, constraints_choices[[next_constraint]]$buttons)
  }
  
  total_cost <- constraints_out |>
    map_vec(~.x$cost) |>
    prod()
  if (verbose) print(paste("Rough max rows generated:", prettyNum(total_cost, big.mark = ",")))
  return(constraints_out)
}


find_presses2 <- function(cons) {
  
  b <- RcppAlgos::permuteGeneral(0:cons[[1]]$sum, m=length(cons[[1]]$new_cols), repetition = TRUE,
                                 constraintFun = "sum",
                                 comparisonFun = "==",
                                 limitConstraints = cons[[1]]$sum)
  
  colnames(b) <- cons[[1]]$new_cols
  
  for (ind in 2:length(cons)) {
    
    if (cons[[ind]]$action == "filter") {
      b <- b[rowSums(b[,cons[[ind]]$buttons, drop = FALSE]) == cons[[ind]]$sum,, drop = FALSE]
    } else if (cons[[ind]]$action == "constrained add") {
      new_col <- matrix(cons[[ind]]$sum - rowSums(b[,cons[[ind]]$overlapping_cols, drop = FALSE]),ncol = 1)
      colnames(new_col) <- cons[[ind]]$new_cols
      b <- cbind(b, new_col)
    } else if (cons[[ind]]$action == "constrained permute") {
      
      rows_summed <- b[,cons[[ind]]$overlapping_cols, drop = FALSE] |> 
        rowSums() |>
        unique()
      
      b3 <- matrix(nrow = 0, ncol = length(cons[[ind]]$buttons))
      colnames(b3) <- cons[[ind]]$buttons
      for (rr in rows_summed) {
        b2 <- RcppAlgos::permuteGeneral(0:(cons[[ind]]$sum-rr), m=length(cons[[ind]]$new_cols), repetition = TRUE,
                                        constraintFun = "sum",
                                        comparisonFun = "==",
                                        limitConstraints = cons[[ind]]$sum-rr)
        
        colnames(b2) <- cons[[ind]]$new_cols
        b3 <- rbind(b3, merge(b[rowSums(b[,cons[[ind]]$overlapping_cols, drop = FALSE]) == rr,, drop = FALSE],
                              b2,
                              by = NULL))
        
        
      }
      b <- b3
    }  else if (cons[[ind]]$action == "unconstrained permute") {
      b2 <- RcppAlgos::permuteGeneral(0:cons[[ind]]$sum, m=length(cons[[ind]]$new_cols), repetition = TRUE,
                                      constraintFun = "sum",
                                      comparisonFun = "==",
                                      limitConstraints = cons[[ind]]$sum)
      
      colnames(b2) <- cons[[ind]]$buttons
      
      b <- merge(b,b2, by = NULL)
    }
    else {
      stop()
    }
  }
  
  b <- b[order(rowSums(b)),,drop = FALSE]
  return(b[,order(as.numeric(colnames(b))),drop = FALSE])
}

checker <- function(M,Mb,target) {
  
  for (r_ind in 1:dim(Mb)[1]) {
    
    found <- all(M%*%t(Mb[r_ind,,drop = FALSE]) == target)
    if (found) {
      print(paste("Got one! On check", r_ind))
      return(sum(Mb[r_ind,]))
    }
  }
  
  
  return(NULL)
  
}


tic("Part 2")
p2_ans_arr <- array(dim = length(input))
for (ans_i in 1:length(input)) {
  #ans_i <- 4
  print(ans_i)
  target <- input[[ans_i]][[3]]
  buttons <- input[[ans_i]][[2]]
  tic()
  M <- buttons_matrix(buttons)
  constraints <- compile_constraints(M,target)
  costs_start <- 1:length(constraints) |>
    map_vec(\(x) {
      find_constraint_order(constraints,x, verbose = FALSE) |>
        map_vec(~.x$cost) |>
        prod()
    })
  constraints_optim <- find_constraint_order(constraints,min(which(costs_start==min(costs_start))))
  #cons <- constraints_optim
  try({
    Mb <- find_presses2(constraints_optim)
    # print(sum(Mb[1,]))
    p2_ans_arr[ans_i] <- checker(M,Mb,target)
  })
  gc()
  toc()
}
print(p2_ans_arr)
print(sum(p2_ans_arr))
toc()
