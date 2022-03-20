
# This simple script find the number of and visualizes PDF books if it is given
# the tight path. Again , it is simple , but amazingly fun.


# Let's start by setting the path and importing the tidyverse.

library(tidyverse )
library(readxl)

setwd("C:/Users/ugurc/Desktop/Science/Statistics/Probability,Mathematical Statistics and Statistical Inference")

the_content_of_the_current_wd <- dir()



# Here we find the names of the directories.

names_of_the_directories <- c()

for (each in the_content_of_the_current_wd) {
  if( !(grepl('pdf' , each)) ){
    names_of_the_directories <- append(names_of_the_directories , each)
  }
}


# We create the paths that we are going to use and we store them in a vector.

the_pathways_to_use <- c()

for (each in names_of_the_directories) {
  the_pathways_to_use <-  append(the_pathways_to_use , paste(getwd() 
                                                    , each , sep = '/'))
}


dirs_and_the_number_of_content <- list()
dirs_and_the_number_of_content[['dir_names']] <- names_of_the_directories


# As the name suggests , we find the number of content of each directory.

the_number_of_content <- c()

for (each in the_pathways_to_use) {
  the_number_of_content <- append(the_number_of_content , length(dir(each)))
}



dirs_and_the_number_of_content[['the_number_of_content']] <- the_number_of_content



# Here we create the final data.frame object.

dirs_and_the_number_of_content_as_a_df <- data.frame(
                                          dirs_and_the_number_of_content)



dirs_and_the_number_of_content_as_a_df$dir_names <- factor(
  dirs_and_the_number_of_content_as_a_df$dir_names ,
  levels = dirs_and_the_number_of_content_as_a_df$dir_names 
)


# Let's visualize what we have found.

ggplot(dirs_and_the_number_of_content_as_a_df, aes(x=dir_names, y=the_number_of_content)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Statistics Books", 
       subtitle="The Statistics Books I Have by Topic") + 
  theme(axis.text.x = element_text(angle=85, vjust=0.6))


# And we close the script with outputting the data.frame object as as CSV file.

write_excel_csv(x = dirs_and_the_number_of_content_as_a_df ,
                file = "Stat_books.csv" )
