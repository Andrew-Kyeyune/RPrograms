greet_student <- function(name, course) {
  message <- paste("Hello,", name, "! Welcome to", course)
  return(message)
}

# Calling the function:
result <- greet_student("Alice", "R Programming")
print(result)

districts <- c("Kampala", "Gulu", "Mbale")

for (d in districts) {
  print(paste("Report for", d))
}
