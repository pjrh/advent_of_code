
import Base: ==, hash

input = read("2024/day7.txt", String)

# Define a structure for storing the input rows
struct Calibration
    answer::Int
    elements::Vector{Int}
end

# Function to initialise the structure from input strings
Calibration(s::String) = Calibration(
    parse(Int, split(s,":")[1]),
    [parse(Int, x) for x in split(split(s,":")[2], " ") if x != ""]
)

# Need the following two definitions to test equality between input rows
# because they get duplicated later, so this means we can call the
# unique() function to deduplicate
function ==(t1::Calibration, t2::Calibration)
    return((t1.answer == t2.answer) & (t1.elements == t2.elements))
end
# (equality in Julia also implies equal hashes so need this)
hash(t::Calibration) = hash(t.answer, hash(t.elements))

# Define a structure for storing intermediate and final results
# of looking for a solution for each calibration. This contains
# the calibration itself and the bits needed to keep track of
# progress in solving (or showing unsolvable) the problem
mutable struct Solution
    calibration::Calibration
    answer_left::Int
    elements_left::Vector{Int}
    operators::String
    worth_continuing::Bool
    solved::Bool
end

# Define an initialisation function
Solution(calibration) = Solution(
    calibration, 
    calibration.answer, 
    deepcopy(calibration.elements), 
    "", 
    true, 
    false)

function Base.show(io::IO, s::Solution)
    if s.solved == true
        print(
            io,
            "Solved\n   ",
            string(s.calibration.answer),
            "=", 
            reduce(*,[string(i[1])*i[2] for i in zip(s.calibration.elements,s.operators * " ")])
            )
    elseif s.worth_continuing == false
        print(
            io,
            "Did not work\n   ",
            string(s.calibration.answer),
            "!=", 
            reduce(*,[string(i[1])*i[2] for i in zip(s.calibration.elements,s.operators * " ")])
        )
    elseif s.calibration.elements == s.elements_left
        print(
            io,
            "Not started yet\n   ",
            string(s.calibration.answer),
            ": ",
            reduce((x,y) -> string(x)*" "*string(y), s.calibration.elements)
        )
    else
        print(
            io,
            "Still working\n   ",
            s.calibration,
            " ",
            s.answer_left,
            ": ",
            reduce((x,y) -> string(x)*" "*string(y), s.elements_left),
            "; ",
            s.operators)
    end
end

function integer_digits(i)
    Int(floor(log(10,i))+1)
end

function integer_concatenation(i1, i2)

    Int(i1*10^integer_digits(i2) + i2)

end

function integer_check_end(i1, i2)

    mod(i2 - i1, 10^integer_digits(i2)) == 0

end

function integer_deconcat(i1, i2)

    Int(floor(i1*10^-float(integer_digits(i2))))

end

# This is the stopping condition for is the solution is
# already solved
function check_sol_stop!(sol)

    if length(sol.elements_left) == 1
        sol.worth_continuing = false
        if sol.answer_left == sol.elements_left[1]
            sol.solved = true
        end
    end

    return(sol)
end


# Define the function which takes the next step in solving a
# partial solution
function step_calibration(sol_in, with_concat = false)

    sol_out_add = deepcopy(sol_in)
    num = pop!(sol_out_add.elements_left)
    sol_out_add.operators = "+" * sol_out_add.operators
    sol_out_add.answer_left = sol_out_add.answer_left - num
    check_sol_stop!(sol_out_add)

    if mod(sol_in.answer_left, num) == 0
        sol_out_mult = deepcopy(sol_in)
        num = pop!(sol_out_mult.elements_left)
        sol_out_mult.operators = "*" * sol_out_mult.operators
        sol_out_mult.answer_left = Int(sol_out_mult.answer_left/num)
        check_sol_stop!(sol_out_mult)

        sol_out_list = [sol_out_add, sol_out_mult]
    else
        sol_out_list =[sol_out_add]
    end

    if with_concat

        sol_out_concat = deepcopy(sol_in)
        num = pop!(sol_out_concat.elements_left)

        if integer_check_end(sol_out_concat.answer_left, num)

            sub_calib_sol = Solution(Calibration(integer_deconcat(sol_out_concat.answer_left,num), sol_out_concat.elements_left))
            check_sol_stop!(sub_calib_sol)

            sub_solve = solve_calibration([sub_calib_sol], with_concat = true)

            sub_solve_solved = filter(x -> x.solved, sub_solve)

            if length(sub_solve_solved) > 0

                sol_out_concat.operators = sub_solve_solved[1].operators * "|" * sol_out_concat.operators
                sol_out_concat.answer_left = sub_solve_solved[1].answer_left
                sol_out_concat.solved = sub_solve_solved[1].solved
                sol_out_concat.worth_continuing = sub_solve_solved[1].worth_continuing

                sol_out_list = vcat(sol_out_list, sol_out_concat)
            end

        end

    end

    return(sol_out_list)
end

# Function which recursively calls the stepper function above on the solutions
# until there are no solutions which haven't finished
function solve_calibration(sol_in_list; with_concat = false)

    stepped_calibs = [step_calibration(sol_in, with_concat) for sol_in in sol_in_list if sol_in.worth_continuing == true]
    non_stepped_calibs = [sol_in for sol_in in sol_in_list if sol_in.worth_continuing == false]

    if length(stepped_calibs) == 0
        return(non_stepped_calibs)
    else
        stepped_calibs = reduce(vcat, stepped_calibs)
        return(solve_calibration(vcat(stepped_calibs, non_stepped_calibs), with_concat = with_concat))
    end
end

calibration_list = [Calibration(String(x)) for x in split(input, "\n")]

sol_list_in = [Solution(x) for x in calibration_list]

sol_list_out = solve_calibration(sol_list_in)
solved_list = filter(x -> x.solved, sol_list_out)

valid_calibrations = unique([(x.calibration) for x in solved_list])
invalid_calibrations = unique([(x.calibration) for x in sol_list_out if x.solved == false])

println("Puzzle 1: ", sum([x.answer for x in valid_calibrations]))

sol_list_out2 = solve_calibration(sol_list_in, with_concat = true)
solved_list2 = filter(x -> x.solved, sol_list_out2)

valid_calibrations2 = unique([(x.calibration) for x in solved_list2])

println("Puzzle 2: ", sum([x.answer for x in valid_calibrations2]))