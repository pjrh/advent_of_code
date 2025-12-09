
library(stringr)
library(collections)
library(purrr)
library(dplyr)
library(tictoc)

tic("Whole run")
tic("Process inputs and prep")

input <- readr::read_lines("day09_input.txt") |>
  map_vec(\(x) {
    coords <- as.numeric(str_split_1(x, ","))
    return(complex(real = coords[1], imaginary = coords[2]))
  })

area <- function(c1, c2) {
  cd <- c2-c1
  (abs(Re(cd))+1) * (abs(Im(cd))+1)
}

toc()

tic("Part 1")

p1_areas <- RcppAlgos::comboGeneral(input, m=2,
                                    FUN = \(x) area(x[1],x[2])) |>
  unlist() 

p1_ans <- p1_areas |>
  max()

print("Part 1:")
print(p1_ans)

toc()

tic("Part 2")

input2 <- rank(Re(input),ties.method = "min") + rank(Im(input),ties.method = "min")*1i

boundary_segment <- function(p1, p2) {
  x1 <- Re(p1)
  y1 <- Im(p1)
  x2 <- Re(p2)
  y2 <- Im(p2)
  
  if (x1==x2) {
    out <- x1+(y1:y2)*1i
  } else if (y1==y2) {
    out <- x1:x2+y1*1i
  } else {
    stop()
  }
  return(out)
}

generate_boundary <- function(points) {
  out <- boundary_segment(points[length(points)], points[1])
  for (i in 1:(length(points)-1)) {
    out <- union(out, boundary_segment(points[i], points[i+1]))
  }
  return(out)
}

boundary <- generate_boundary(input2)

# flood_fill <- function(p_init, xmin, xmax, ymin, ymax, boundary) {
#   
#   flooded_area <- c()
#   q <- queue(items = p_init)
#   while (q$size() > 0) {
#     new_p <- q$pop()
#     
#     if (!(new_p %in% boundary) & !(new_p %in% flooded_area)) {
#       flooded_area <- union(flooded_area, new_p)
#       if (Re(new_p + 1) <= xmax) q$push(new_p + 1)
#       if (Re(new_p - 1) >= xmin) q$push(new_p - 1)
#       if (Im(new_p + 1i) <= ymax) q$push(new_p + 1i)
#       if (Im(new_p - 1i) >= ymin) q$push(new_p - 1i)
#     }
#   }
#   return(flooded_area)
# }

# flood_fill(input2[1]-1i, 0, 500, 0, 500 ,boundary)

outside_boundary <- function(p_init, boundary) {
  
  surrounding_boundary <- 
    c(boundary +1,
      boundary -1,
      boundary +1i,
      boundary -1i,
      boundary +1+1i,
      boundary -1+1i,
      boundary +1-1i,
      boundary -1-1i) |>
    unique()
  
  surrounding_boundary <- setdiff(surrounding_boundary, boundary)
  
  if (!(p_init %in% surrounding_boundary)) stop("p_init must be next to boundary")
  
  iters <- 0
  b <- c()
  q <- queue(items = p_init)
  while (q$size() > 0) {
    new_p <- q$pop()
    b <- union(b, new_p)
    for (l in c(1,-1, 1i, -1i)) {
      next_p <- new_p + l
      if((next_p %in% surrounding_boundary) & !(next_p %in% b)) q$push(next_p)
    }
    
    iters <- iters+1
    # if ((iters %% 1000) == 0) print(iters)
  }
  return(b)
}

ob1 <- outside_boundary(input2[1] +1+1i, boundary)
ob2 <- outside_boundary(input2[1] -1-1i, boundary)

if (length(ob1) > length(ob2)) ob <- ob1
if (length(ob2) > length(ob1)) ob <- ob2

# outside_points <- flood_fill(1+1i, 
#                              min(Re(input))-1,
#                              max(Re(input))+1,
#                              max(Im(input))+1,
#                              min(Im(input))-1,
#                              boundary)

box_boundary <- function(c1,c2) {
  points <- c(c1, Re(c1) + Im(c2)*1i, c2, Re(c2) + Im(c1)*1i)
  generate_boundary(points)
}

valid_area <- function(p1, p2) {
  return(length(intersect(box_boundary(p1,p2), ob))==0)
}

tic("Part 2 combos")

# p2_valid_inds <- RcppAlgos::comboGeneral(input2, m=2,
#                                          FUN = \(x) valid_area(x[1],x[2])) |>
#   unlist()
# 
# p2_ans <- max(p1_areas[p2_valid_inds])

p2_combos <- RcppAlgos::comboGeneral(input2, m=2)
p2_combos_sorted <- p2_combos[order(p1_areas, decreasing = TRUE),]

tic()
i = 1
repeat {
  if (valid_area(p2_combos_sorted[i,1], p2_combos_sorted[i,2])) break
  i <- i+1
}
p2_ind <- which((p2_combos[,1] == p2_combos_sorted[i,1]) & (p2_combos[,2] == p2_combos_sorted[i,2]))
p2_ans <- p1_areas[p2_ind]
toc()

toc()

print("Part 2:")
print(p2_ans)
# too low: 140783340

toc()