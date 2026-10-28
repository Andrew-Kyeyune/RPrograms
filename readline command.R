# Interactive grade calculator
calculate_grade <- function(score) {
  if      (score >= 90) return(list(letter="A", remark="Excellent!"))
  else if (score >= 80) return(list(letter="B", remark="Good"))
  else if (score >= 70) return(list(letter="C", remark="Average"))
  else if (score >= 60) return(list(letter="D", remark="Below average"))
  else                  return(list(letter="F", remark="Fail"))
}

# Interactive use
student <- readline("Enter student name: ")
mark <- as.numeric(readline("Enter score (0-100): "))

result <- calculate_grade(mark)
cat("\nStudent:", student, "\n")
cat("Score:", mark, "\n")
cat("Grade:", result$letter, "-", result$remark, "\n")
