
import Combinatorics

input = read("2024/day8.txt", String)

coord_list = [([col_num, row_num], char) for (row_num, row) in enumerate(split(input, "\n")) for (col_num, char) in enumerate(row)]
antennae_list = filter(x -> string(x[2]) != ".", coord_list)

antennae_dict = Dict()

for antenna in antennae_list
    antennae_dict[antenna[2]]=vcat(get(antennae_dict, antenna[2] , []), [antenna[1]])
end

antinodes = Set()
for (antenna, locations) in antennae_dict
    for (ant1, ant2) in Combinatorics.combinations(locations, 2)
        diff = ant2-ant1

        union!(antinodes, [ant1 - diff])
        union!(antinodes, [ant2 + diff])
    end
end

intersect!(antinodes, map(x -> x[1], coord_list))

println("Puzzle 1: ", length(antinodes))


n_cols = maximum(x->x[1][1], coord_list)
n_rows = maximum(x->x[1][2], coord_list)
max_side_length = max(n_cols, n_rows)

antinodes2 = Set()
for (antenna, locations) in antennae_dict
    for (ant1, ant2) in Combinatorics.combinations(locations, 2)
        diff = ant2-ant1

        diff = map(x -> Int(x), diff/gcd(diff[1], diff[2]))

        for n in -max_side_length:max_side_length
            union!(antinodes2, [ant1 + n*diff])
        end
    end
end

intersect!(antinodes2, map(x -> x[1], coord_list))

println("Puzzle 2: ", length(antinodes2))