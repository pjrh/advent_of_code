
import Graphs

input = read("2024/day10.txt", String)

row_length = length(split(input, "\n"))
coord_list = [([col_num, row_num], (col_num + row_length*(row_num-1), parse(Int, char))) for (row_num, row) in enumerate(split(input, "\n")) for (col_num, char) in enumerate(row)]

coord_dict = Dict(coord_list)
get(coord_dict, [4,2], 0)

next_directions = [[1,0], [0,1], [-1,0], [0,-1]]
edge_list = []
for (point, (vertex, height)) in coord_dict
    for d in next_directions
        (next_vertex, next_height) = get(coord_dict, point + d, (0,0))
        if (next_height - height) == 1
            push!(edge_list, (vertex, next_vertex))
        end

    end
end

g = Graphs.SimpleDiGraph(Graphs.Edge.(edge_list))

trailhead_list = filter(x -> x[2][2] == 0, coord_list)
trailhead_verts = [x[2][1] for x in trailhead_list]

tops_list = filter(x -> x[2][2] == 9, coord_list)
tops_verts = [x[2][1] for x in tops_list]

function count_trails(g, trailhead_verts, tops_verts)
    trails = 0
    for headv in trailhead_verts
        for topv in tops_verts
            #println(headv, " ", topv)
            #println(Graphs.has_path(g,headv,topv))
            trails += Graphs.has_path(g,headv,topv)
        end
    end
    return(trails)
    end

println("Puzzle 1: ", count_trails(g, trailhead_verts, tops_verts))

function sum_ratings(g, trailhead_verts, tops_verts, maxK)
    ratings_sum = 0
    for headv in trailhead_verts
        for topv in tops_verts
            #println(headv, " ", topv)
            #println(Graphs.has_path(g,headv,topv))
            paths = Graphs.yen_k_shortest_paths(g,headv,topv, Graphs.weights(g), maxK)

            ratings_sum += length(paths.paths)
        end
    end
return(ratings_sum)
end

@time println("Puzzle 2: ", sum_ratings(g, trailhead_verts, tops_verts, length(coord_list)))