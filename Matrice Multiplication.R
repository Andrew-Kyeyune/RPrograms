A <- matrix(1:4,2,2)
B <- matrix(5:8,2,2)
A %*% B

c(4,9,16) |> sqrt() |> sum()

sales <- c(120, 340, 95)
for (i in seq_along(sales)) {
  cat("Day", i, ":", sales[i], "\n")
}