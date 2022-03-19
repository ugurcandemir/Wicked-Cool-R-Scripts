
# In this project I undertake a very tedious task. I import a dataset by
# parsing a PDF file.

library(pdftools)
library(tidyverse)

# There are more than one function to import a text from a PDF file. But 
# pdftools::pdf_text() seemed the most convenient one to me. Here I use it.
# It returns the content of every page as the elements of a list. Since the 
# document is two pages a take each of those as a character vector.

first_page <- strsplit(
  pdf_text(
    'Quiz _ Homework.pdf') ,
  split = '\r\n')[[1]]


second_page <- strsplit(
  pdf_text(
    'Quiz _ Homework.pdf') ,
  split = '\r\n')[[2]]

# Each page has its own flaws. I will use the most appropriate methods for each
# of them. I start with manipulating the first page. Names and grades are placed
# in the vector in a detectable pattern. I will clear the white space and put
# them in a data.frame.

odd_rows <- seq(1 , length(first_page) , 2)
even_rows <- seq(2 , length(first_page) , 2)

part_1 <- trimws( first_page[odd_rows] )
part_2 <- trimws( first_page[even_rows] )

first_page <- data.frame( part_2 , part_1)

# Take care of the column names.

names(first_page) <- first_page[1 , ] 
first_page <- first_page[ -1 , ]

# Take care of the names.
first_page <- first_page %>%
  separate(Name , into = c('Last_Name' , 'First_Name') , sep = ',') 

# Here quiz grades and homework grades are taken as a one value and there is a 
# white space between them. Here I clear the white space between them and place
# them in the dataframe as two separate values.

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

# At this point the lst row seems a little bir problematic. We need to take it 
# into a particular shape.

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

# The second page is also done. Now let's bind the two dataframes each other
# and give the last settings before analysis.

grades_of_the_class <- rbind(first_page , second_page)
grades_of_the_class[grades_of_the_class$Quiz == '*' , ]$Quiz <- 0
grades_of_the_class$First_Name <- trimws(grades_of_the_class$First_Name)

grades_of_the_class$Quiz <- as.integer(grades_of_the_class$Quiz)
grades_of_the_class$Homework <- as.integer(grades_of_the_class$Homework)

# The data is ready. It has been a good example. It includes only PDF import and
# string manipulation , nothing else. If we ignore help comments and empty lines
# it took approximately 75-80 lines of code.

# Now we can run any analysis we want.

quantile(grades_of_the_class$Homework)
quantile(grades_of_the_class$Quiz)


grades_of_the_class <- grades_of_the_class %>%
  mutate(passing_grade = Quiz+Homework) %>%
  arrange(desc(passing_grade)) %>%
  mutate(percentage = percent_rank(passing_grade))

grades_of_the_class %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = Quiz)  )

grades_of_the_class %>%
  ggplot() +
  geom_point(mapping = aes(x = Homework , y = Quiz)  )

cor(grades_of_the_class$Quiz , grades_of_the_class$Homework)

