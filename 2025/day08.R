
library(stringr)
library(collections)
library(purrr)
library(dplyr)
library(tictoc)

tic("Whole run")
tic("Process inputs and prep")

input <- readr::read_lines("day08_input.txt") %>%
  map(~as.numeric(str_split_1(.x, ",")))

# Going to refer to the boxes by number which means
# each box will be numbered up from 1 and then a
# dictionary will store the coordinates
boxes <- dict()
input %>%
  iwalk(~boxes$set(.y,.x))

# Euclidean distance
dist_func <- function(r1, r2) {
  sqrt(sum((r2-r1)^2))
}

# Find the combinations of pairs of box numbers and the 
# distances between each pair
link_combinations <- RcppAlgos::comboGeneral(1:boxes$size(), m=2,
                             FUN = function(x) c(x[[1]], x[[2]])) 
link_distances <- link_combinations |>
  map_vec(\(x) dist_func(boxes$get(x[1]), boxes$get(x[2])))

# Create a priority queue so we can consider each box pair at a time
link_queue <- priority_queue(items = link_combinations,
                              priorities = -link_distances)

# We create two dictionaries to store the mapping of box numbers to
# which circuit they are in, and the box numbers in each circuit.
# To start with each box is its own circuit.
# Ciruits are numbered by which box is the lowest number box in them.
box_to_circuit <- dict(items = 1:boxes$size(), keys = 1:boxes$size())
circuit_to_box <- dict(items = 1:boxes$size(), keys = 1:boxes$size())

# This function takes a new connection (i.e. pair of box names)
# and then updates the dictionaries above mirror the changes this
# makes to the circuits.
update_dicts <- function(box_to_circuit, circuit_to_box, new_connection) {
  # get the existing circuit the boxs being connected are currently in
  circuit1 <- box_to_circuit$get(new_connection[[1]])
  circuit2 <- box_to_circuit$get(new_connection[[2]])
  
  # get the boxes currently connected in each circuit
  # and add the new box in the pair in to each
  existing_circuit1 <- circuit_to_box$get(circuit1)
  circuit1_out <- c(existing_circuit1, new_connection[[2]])
  existing_circuit2 <- circuit_to_box$get(circuit2)
  circuit2_out <- c(existing_circuit2, new_connection[[1]])
  
  # Find the totality of the new circuit
  circuit_out <- union(circuit1_out, circuit2_out)
  # and the name of the new circuit
  circuit_out_key <- min(circuit_out)
  
  # update the circuit
  circuit_to_box$set(key = circuit_out_key, value = circuit_out)
  # and remove the references in the dictionary to circuits which
  # no longer exist (i.e. where two circuits are joined then we
  # need to take out the reference to the larger numbered circuit now)
  for (k in intersect(setdiff(circuit_out, circuit_out_key), circuit_to_box$keys())) {
    circuit_to_box$remove(k)
  }
  
  # update the circuits which the new box pairs belongs to
  if (circuit1 != circuit_out_key) box_to_circuit$set(key = new_connection[[1]], value = circuit_out_key)
  if (circuit2 != circuit_out_key) box_to_circuit$set(key = new_connection[[2]], value = circuit_out_key)
  
  # if the new name of the circuit was not one of the boxes in the original
  # circuit then we have to update all those boxes with the new circuit name
  if (!(circuit_out_key %in% existing_circuit1)) {
    for (k in existing_circuit1) {
      box_to_circuit$set(key = k, value = circuit_out_key)
    }
  }
  
  # same for the second initial circuit
  if (!(circuit_out_key %in% existing_circuit2)) {
    for (k in existing_circuit2) {
      box_to_circuit$set(key = k, value = circuit_out_key)
    }
  }
}

toc()


tic("Part 1")

for (i in 1:1000) {
  update_dicts(box_to_circuit, circuit_to_box, link_queue$pop())
}

circuits_out <- circuit_to_box$values()

len <- sapply(circuits_out, length)
circuits_out <- circuits_out[order(len, decreasing = TRUE)]

print("Part 1:")
p1_ans <- 
  length(circuits_out[[1]])*
  length(circuits_out[[2]])*
  length(circuits_out[[3]])
print(p1_ans)


toc()

tic("Part 2")

while (circuit_to_box$size() > 1) {
  new_connection <- link_queue$pop()
  update_dicts(box_to_circuit, circuit_to_box, new_connection)
}

print("Part 2:")
p2_ans <- boxes$get(new_connection[[1]])[[1]]*boxes$get(new_connection[[2]])[[1]]
print(p2_ans)

toc()
toc()