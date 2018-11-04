# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer

# FIXME Comment Function
# FIXME Comment Inputs
# FIXME Comment Function Outputs
library(dplyr)

#Initialize the data frame
age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")
students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))
students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

# Input: 
# Argument1, class of argument1 object

#' calculate sex specific height difference of persons in a data.frame to 
#' the average height
#' @param students.input ('data.frame') \cr
#'   data.frame with columns height, sex and name 
#' @return ('data.frame') \cr
#'   returns data.frame with names and difference to average gender height
checkHeight3 <- function(students.input = students){
  ## average height by gender
  women_mean_height = mean(with(students.input, height[sex=="F"]))
  men_mean_height = mean(with(students.input, height[sex=="M"]))

  ## initialize vector to store height difference for every person in df
  height_vector = c() 

   ## apply function to calculate difference and store the value in height_vector
  height_vector = apply(students.input, MARGIN = 1, 
    FUN = function(student){
    #substract the gender specific means from the individuals to get height differnces
    (if (student["sex"] == "M") men_mean_height - as.numeric(student["height"]) 
    else women_mean_height - as.numeric(student["height"]) ) 
    } ) 
  
  ## create return data.frame 
  result.frame = data.frame("name" = students.input$name, "sexspec_height_diff" = height_vector*100)
  
  ## return resulting data.frame
  return(result.frame)
}

print(checkHeight3(students.input = students))
