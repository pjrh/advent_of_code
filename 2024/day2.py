import polars as pl

input = pl.read_csv("2024/day2.txt", has_header=False)

input = input.select(
        pl.int_range(start = 1, end = pl.len() + 1, dtype=pl.UInt32).alias("report"),
        pl.col("column_1").str.split(" ").alias("data")
    ).\
    explode("data").\
    cast({"data": pl.UInt8})

output = input.group_by("report").agg(
    pl.col("data").diff().min().alias("diff_min"),
    pl.col("data").diff().max().alias("diff_max")
).with_columns(((pl.col("diff_min").is_in([1,2,3]) & pl.col("diff_max").is_in([1,2,3])) |
                       (pl.col("diff_min").is_in([-1,-2,-3]) & pl.col("diff_max").is_in([-1,-2,-3]))).
                       alias("safe"))


safe_reports = output.filter(pl.col("safe") == True).get_column("report").to_list()
print("Puzzle 1: ", len(safe_reports))


input2 = input.\
    with_columns(
        pl.int_range(start = 1, end = pl.len() + 1, dtype=pl.UInt32).over("report").alias("obs")
    )
safe_reports2 = set(safe_reports)
for miss_item in range(1, input2.max().item(row=0,column="obs") + 1):
    loopdf = input2.filter(
                           (pl.col("obs") != miss_item)).\
        group_by("report").agg(
            pl.col("data").diff().min().alias("diff_min"),
            pl.col("data").diff().max().alias("diff_max")
        ).with_columns(((pl.col("diff_min").is_in([1,2,3]) & pl.col("diff_max").is_in([1,2,3])) |
                       (pl.col("diff_min").is_in([-1,-2,-3]) & pl.col("diff_max").is_in([-1,-2,-3]))).
                       alias("safe"))


    safe_reports2.update(loopdf.filter(pl.col("safe") == True).get_column("report").to_list())

print("Puzzle 2: ", len(safe_reports2))