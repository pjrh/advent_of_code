
input = read("2024/day3.txt", String)
# input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

mul_matches = eachmatch(r"mul\(([0-9]+),([0-9]+)\)", input)

vals = [[parse(Int, mul_match.captures[1]), parse(Int, mul_match.captures[2])] for mul_match in mul_matches]
prods = [x[1] * x[2] for x in vals]

println("Puzzle 1: ", sum(prods))

input2 = "do()" * input * "don't()"
input2 = replace(input2, "\n" => "NEWLINE")
input2 = replace(input2, r"don't\(\).*?do\(\)" => "CUT")

mul_matches2 = eachmatch(r"mul\(([0-9]+),([0-9]+)\)", input2)

vals2 = [[parse(Int, mul_match.captures[1]), parse(Int, mul_match.captures[2])] for mul_match in mul_matches2]
prods2 = [x[1] * x[2] for x in vals2]

println("Puzzle 2: ", sum(prods2))