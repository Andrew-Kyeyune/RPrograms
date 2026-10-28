# Definition — no parameters inside the parentheses
show_welcome <- function() {
  cat("================================\n")
  cat("  Welcome to R Programming!    \n")
  cat("  Instructor: Andrew Kyeyune  \n")
  cat("================================\n")
}

# Calling the function — still need the () even with no arguments
show_welcome()

# OUTPUT:
# ================================
#   Welcome to R Programming!
#   Instructor: Andrew Kyeyune
# ================================


display_sum <- function(a, b) {
  total <- a + b
  cat("The sum of", a, "and", b, "is:", total, "\n")
}

display_sum(15, 25)
display_sum(100, 200)

# Calculate a student's letter grade
get_grade <- function(score) {
  if (score >= 90) return("A")
  else if (score >= 80) return("B")
  else if (score >= 70) return("C")
  else if (score >= 60) return("D")
  else return("F")
}

# Using the returned value in different ways
letter <- get_grade(85)              # Stored in variable
cat("Grade:", letter, "\n")          # B
cat("Result:", get_grade(72), "\n")  # Directly printed: C

# Compose with other functions
all_grades <- sapply(c(91, 78, 65, 55, 83), get_grade)
print(all_grades)  # "A" "C" "D" "F" "B"
