
using Memoize

input = read("2024/day16.txt", String)

struct Path
    tiles::Int
    corners::Int
    found_end::Int
    cost::Int
    #route::Set{Vector{Int}}
end

Path(t::Int, c::Int) = Path(t,c,0,t+1000*c)
Path(t::Int, c::Int, f::Int) = Path(t,c,f,t+1000*c)

function Base.:+(x::Path, y::Path)
    Path(x.tiles+y.tiles,
           x.corners+y.corners,
           x.found_end+y.found_end,
           x.cost+y.cost
           )
end

row_length = length(split(input, "\n"))
coord_list = [([col_num, row_num], string(char)) for (row_num, row) in enumerate(split(input, "\n")) for (col_num, char) in enumerate(row)]
coord_dict = Dict(coord_list)


function get_path_options(coord, dir)
    
    # use rotation matrix to find new options
    [
    coord + dir, # straight on
    coord + [[0,-1] [1,0]]*dir, # turn right
    coord + [[0,1] [-1,0]]*dir # turn left
    ]

end


function get_paths(start_coord, start_dir, max_cost, coord_dict, path = Path(0,0), travelled = Vector{Vector{Int}}())


    push!(travelled, start_coord)
    #println(travelled)
    local check_next

    while true

    if coord_dict[start_coord] == "E"
        println("Found E! Cost: ", path.cost)
        return(path + Path(0,0,1))
    end

    if path.cost > max_cost
        #println("Too far")
        return(Path(0,0))
    end

    check_next = get_path_options(start_coord, start_dir)
    check_next = [x for x in check_next if get(coord_dict, x, "NA") in [".", "E"] && !(x in travelled)]

    if isempty(check_next)
        #println("Nowhere to go")
        return(Path(0,0))
    elseif length(check_next) > 0

        break
    end

    new_coord = check_next[1]
    new_dir = new_coord-start_coord

            if start_dir == new_dir
                path = path + Path(1,0,0)
            else
                path = path + Path(1,1,0)
            end

        push!(travelled, start_coord)
        start_coord = new_coord
        start_dir = new_dir

    end

    out = Vector{Path}()
    for new_coord in check_next
            new_dir = new_coord-start_coord
            if start_dir == new_dir
                path_new = path + Path(1,0,0)
            else
                path_new = path + Path(1,1,0)
            end
            next_steps = get_paths(new_coord, new_dir, max_cost, coord_dict, path_new, travelled)
            pop!(travelled)
            out = vcat(out, next_steps)

            #println(next_steps)
        #if next_steps.found_end == 1
        #    max_cost = next_steps.cost
        #end
    end

    if length(out) > 1; out = argmin(x -> if x.found_end == 1; x.cost else Inf end, out) end
    return(out)

 

end

start_coord = first(filter(x -> x[2] == "S" , coord_dict))[1]
@time path = get_paths(start_coord, [1,0], 34000, coord_dict)
#@time all_paths = get_paths(start_coord, [1,0], 10000, coord_dict)
println(path.cost)

 