
# This is the revised version of the PDF file. There are some major differences
# between them. I had to change the core of the script some I saved it as 
# a separate file.

library(pdftools)
library(tidyverse)
library(readxl)

first_page <- strsplit(
  pdf_text(
    'Quiz _ Homework II.pdf') ,
  split = '\r\n')[[1]]


second_page <- strsplit(
  pdf_text(
    'Quiz _ Homework II.pdf') ,
  split = '\r\n')[[2]]




odd_rows <- seq(1 , length(first_page) , 2)
even_rows <- seq(2 , length(first_page) , 2)

part_1 <- trimws( first_page[odd_rows] )
part_2 <- trimws( first_page[even_rows] )

first_page <- data.frame( part_2 , part_1)



names(first_page) <- first_page[1 , ] 
first_page <- first_page[ -1 , ]


first_page <- first_page %>%
  separate(Name , into = c('Last_Name' , 'First_Name') , sep = ',') 






grade_vector_flatten_down <- unlist(strsplit(first_page$`QUIZ GRADE HW` ,
                                             split = " "))
grade_vector_flatten_down_filtered <- grade_vector_flatten_down != ""
grades <- grade_vector_flatten_down[grade_vector_flatten_down_filtered]
odd_rows_grades <- seq(1 , length(grades) , 2)
even_rows_grades <- seq(2 , length(grades) , 2)
Quiz <- grades[odd_rows_grades]
Homework <- grades[even_rows_grades]

first_page <- cbind(first_page , Quiz , Homework)
first_page <- first_page[ , -3]

# Now let's deal with the second page.

second_page <- trimws(second_page)
odd_rows <- seq(1 , length(second_page) , 2)
even_rows <- seq(2 , length(second_page) , 2)
part1 <- second_page[odd_rows]
part2 <- second_page[even_rows]
second_page <- data.frame(part1 , part2)




second_page_last_row <- second_page[nrow(second_page) , ]
second_page_last_row_splitted <- strsplit(second_page_last_row$part1 ,
                                          split =  " ")[[1]]
non_empties <- second_page_last_row_splitted != ""
second_page_last_row_splitted <- second_page_last_row_splitted[non_empties]
take_except <- (length(second_page_last_row_splitted)-1):length(second_page_last_row_splitted)
pasted_name1 <- paste(second_page_last_row_splitted[-take_except] ,collapse = " " )
pasted_name2 <- paste(pasted_name1 , second_page_last_row$part2 , collapse = " ")  
grade_section <- second_page_last_row_splitted[(length(second_page_last_row_splitted)-1):length(second_page_last_row_splitted)]
grade_section <- paste(grade_section , collapse = " " )
second_page <- rbind(second_page , c(grade_section , pasted_name2) )
second_page <-  second_page[-29 , ]

second_page <- second_page %>%
  separate(part2 , into = c('Last_Name' , 'First_Name') , sep = ',')

grades <- unlist(strsplit(second_page$part1 , split = " "))[unlist(strsplit(second_page$part1 , split = " ")) != ""]

odd_rows_grades <- seq(1 , length(grades) , 2)
even_rows_grades <- seq(2 , length(grades) , 2)
Quiz <- grades[odd_rows_grades]
Homework <- grades[even_rows_grades]
second_page <- cbind(second_page , Quiz , Homework)
second_page <- second_page[ , -1]




grades_of_the_class <- rbind(first_page , second_page)
grades_of_the_class[grades_of_the_class$Quiz == '*' , ]$Quiz <- 0
grades_of_the_class[grades_of_the_class$Homework == '*' , ]$Homework <- 0
grades_of_the_class$First_Name <- trimws(grades_of_the_class$First_Name)

grades_of_the_class$Quiz <- as.integer(grades_of_the_class$Quiz)
grades_of_the_class$Homework <- as.integer(grades_of_the_class$Homework)

# The data is ready.

quantile(grades_of_the_class$Homework)
quantile(grades_of_the_class$Quiz)


grades_of_the_class <- grades_of_the_class %>%
  mutate(passing_grade = Quiz+Homework) %>%
  arrange(desc(passing_grade)) %>%
  mutate(percentage = percent_rank(passing_grade))



grades_of_the_class %>%
  ggplot() +
  geom_point(mapping = aes(x = 1:nrow(grades_of_the_class) , y = passing_grade))+ 
  xlab("Every Person in the Class") + ylab("Passing Grade")


grades_of_the_class$graduate_students <- ifelse(grades_of_the_class$First_Name %in% c('UĞURCAN' , 'NİYAZİ') , T , F)


grades_of_the_class %>%
  ggplot() +
  geom_point(mapping = aes(x = 1:nrow(grades_of_the_class) ,
                           y = percentage ,
                           colour = graduate_students)  , show.legend = F) + 
  xlab("Every Person in the Class") + ylab("Percentage of the Class")



grades_of_the_class %>%
  ggplot() +
  geom_point(mapping = aes(x = 1:nrow(grades_of_the_class) ,
                           y = Quiz ,
                           colour = graduate_students)  , show.legend = F) +
  xlab("Every Person in the Class")


cor(grades_of_the_class$Quiz , grades_of_the_class$Homework)

# Let's write this data out as a CSV file.

write_excel_csv(x = grades_of_the_class , file = "grades.csv")

