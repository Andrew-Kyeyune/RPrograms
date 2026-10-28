get_grade <- function(score) {
  if (score >= 90) return("A")
  else if (score >= 80) return("B")
  else if (score >= 70) return("C")
  else if (score >= 60) return("D")
  else return("F")
}

letter <- get_grade(85)
cat("Grade:", letter, "\n")
cat("Result:", get_grade(72), "\n")

all_grades <- sapply(c(91, 78, 65, 55, 83), get_grade)
print(all_grades)  # "A" "C" "D" "F" "B"
