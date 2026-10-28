install.packages("renv")
renv::init()      # initialises renv for your project
renv::snapshot()  # saves current package versions to renv.lock
renv::restore()   # reinstalls exact versions from renv.lock


# Define population: exam scores of all 80 students
set.seed(42)   # ensures reproducibility
population <- round(rnorm(80, mean = 65, sd = 10))  # simulate marks

# View basic population statistics
cat("Population Size (N):", length(population), "\n")
cat("Population Mean (mu):", mean(population), "\n")
cat("Population Variance:", var(population), "\n")
cat("Min Score:", min(population), "  Max Score:", max(population), "\n")

# Visualise the population
hist(population,
     main = "Distribution of Exam Scores – ISBAT (Population)",
     xlab = "Score", col = "steelblue", border = "white")

# SCENARIO: 100 students at ISBAT, select 15 for a survey
set.seed(7)
population  <- 1:100              # student IDs 1 to 100
sample_size <- 15

# Without replacement (default) — no student selected twice
my_sample <- sample(population, size = sample_size, replace = FALSE)
cat("Sample (no replacement):", my_sample, "\n")

# With replacement — a student can appear more than once
my_sample_wr <- sample(population, size = sample_size, replace = TRUE)
cat("Sample (with replacement):", my_sample_wr, "\n")

# Sample summary statistics
cat("Sample Mean:", round(mean(my_sample), 2), "\n")
cat("Population Mean:", mean(population), "\n")
cat("Difference:", round(mean(population) - mean(my_sample_wr), 2), "\n")

print(sample_size)
print(population)

# SCENARIO: ISBAT has 3 departments; we want a fair survey sample
dept_A <- paste0("A", 1:60)  # 60 IT students
dept_B <- paste0("B", 1:30)  # 30 CS students
dept_C <- paste0("C", 1:10)  # 10 Business IT students

# Proportional stratified sample (10% from each group)
set.seed(99)
strata_sample <- c(
  sample(dept_A, size = round(0.10 * length(dept_A))),
  sample(dept_B, size = round(0.10 * length(dept_B))),
  sample(dept_C, size = round(0.10 * length(dept_C)))
)
cat("Total sample size:", length(strata_sample), "\n")
# Output: Total sample size: 10
print(strata_sample)

