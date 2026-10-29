titanic = read.csv("./titanic.csv")
head(titanic)

cls <- sapply(titanic, class)
cls <- as.data.frame(cls)

head(titanic)
tail(titanic)

sapply(titanic, class)
as.data.frame(cls)


titanic$Survived = as.factor(titanic$Survived)
titanic$Sex = as.factor(titanic$Sex)

sapply(titanic, class)
as.data.frame(cls)

titanic$Survived = as.factor(titanic$Survived)
titanic$Sex = as.factor(titanic$Sex)
sapply(titanic, class)
as.data.frame(cls)


titanic$Survived = as.factor(titanic$Survived)
titanic$Sex = as.factor(titanic$Sex)

titanic$Survived

cls <- sapply(titanic, class)
print(cls)

summary(titanic)

sum(is.na(titanic))

dropnull_titanic = titanic[rowSums(is.na(titanic)) <= 0, ]

print(dropnull_titanic)

survivedlist = dropnull_titanic[dropnull_titanic$Survived == 1, ]
notsurvivedlist = dropnull_titanic[dropnull_titanic$Survived == 0, ]

print(dropnull_titanic)

survivedlist    <- dropnull_titanic[dropnull_titanic$Survived == 1, ]
notsurvivedlist <- dropnull_titanic[dropnull_titanic$Survived == 0, ]

mytable <- table(titanic$Survived)
lbls    <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls,
    main = "Survival Rate (with sample sizes)")

head(survivedlist)
head(notsurvivedlist)

mytable

hist(survivedlist$Age, xlab = "Age", ylab = "Frequency", main = "Age Distribution of Survivors")

barplot(table(notsurvivedlist$Sex), xlab = "Gender", ylab = "Frequency", main = "Gender Distribution of Non-Survivors")

install.packages("ggplot2")
library(ggplot2)

ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Pclass vs Survived", x = "Pclass (1 = First, 2 = Second, 3 = Third)", y = "Count") +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Survived", "Survived")) +
  theme_minimal() +
  theme(legend.title = element_blank())

View(titanic)
