

library(purrr)
library(stringr)
library(tictoc)

input <- readr::read_file("day12_input.txt")

shapes <- input |>
  str_remove_all("[0-9]:\\n") |>
  str_split_1("\n\n") |>
  head(6) |>
  map(\(x) {
    list("str" = x,
         "area" = x |> str_count("#"))
  })

regions <- input |>
  str_split_1("\n\n") |>
  tail(-6) |>
  str_split_1("\n") |>
  map(\(x) {x2 <- x|> str_split_1(": ")
  list("grid_size" = as.numeric(str_split_1(x2[1],"x")),
       "shape_req" = as.numeric(str_split_1(x2[2]," ")))
  })

for (i in 1:length(regions)) {
  region_area <- regions[[i]]$grid_size[1] * regions[[i]]$grid_size[2]
  adjacent_area_req <- sum(regions[[i]]$shape_req*9)
  packed_req <- map2_vec(regions[[i]]$shape_req,
                         shapes,
                         \(x,y) {x*y$area}) |>
    sum()
  definitely_fail[i] <- packed_req > region_area
  definitely_pass[i] <- adjacent_area_req <= region_area
  unsure[i] <- !definitely_fail[i] & !definitely_pass[i]
}

unsure <- length(regions) - (sum(definitely_fail) + sum(definitely_pass))

print("Part 1:")
print(sum(definitely_pass))
