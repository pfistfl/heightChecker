# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer

library(dplyr)


age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")



checkHeight3 = function(students.input = students){

  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")

  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))

  height.diff = apply(students.input, 1, function(person, avg_male, avg_female) {
    getDiff(avg_male, avg_female, data.frame(t(person)))
  }, "avg_male" = male.mean$mean, "avg_female" = female.mean$mean)
  
  result.frame$name = students.input$name
  result.frame$difference = height.diff
  return(result.frame)
  
}

getDiff = function(avg_male, avg_female, person) {
  mean_height = ifelse(person$sex == "F", avg_female, avg_male)
  diff = (as.numeric(as.character(person$height)) - mean_height)*100
  return(diff)
}


checkHeight3(students.input = students)
