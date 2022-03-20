# In this project we illustrate how to parse a large character and turn it into
# a dataset.

# As usual we start by importing the libraries.

library(tidyverse)
library(readxl)

# This college transcript was on a PDF. I could opt to parse the file from R or 
# to filter the characters in to a .txt file and import it as a text file from
# R. But here I copy-pasted all the text and now I start to wrangle it. 

transcript <- 
'IBF 1001 HUKUKUN TEMEL KAVRAMLARI 3 4 3 CC
IBF 1003 SOSYAL PSIKOLOJI 3 3 3 CC
IBF 1005 MATEMATIK I 3 4 3 BB
IBF 1007 ISTATISTIK I 3 5 3 CB
IKT 1001 IKTISADA GIRIS I 3 4 3 CB
ISL 1001 ISLETME BILIMI I 3 3 3 AA
ISL 1005 FINANSAL MUHASEBE I 3 4 3 CB
TDL 1001 Turk Dili I 2 2 2 CB
GSR 1001 Resim I 1 1 1 B
EMT 1004 TEMEL BILGI TEKNOLOJILERI 3 3 3 AA
EMT 1006 DOGRUSAL CEBIR 3 4 3 BB
IBF 1006 MATEMATIK II 3 4 3 BB
IBF 1008 ISTATISTIK II 3 5 3 BB
IKT 1002 IKTISADA GIRIS II 3 4 3 BA
ISL 1002 ISLETME BILIMI II 3 3 3 CB
ISL 1004 FINANSAL MUHASEBE II 3 4 3 CB
TDL 1002 Turk Dili II 2 2 2 BA
GSR 1002 Resim II 1 1 1 B
EMT 2005 BILGISAYAR PROGRAMLAMA 3 4 3 BA
EMT 2007 ILERI MATEMATIK I 3 4 3 CB
EMT 2011 MATEMATIKSEL ISTATISTIK I 3 4 3 CB
IKT 2013 MIKRO IKTISAT 3 4 3 CB
ISL 3031 ISLETME FINANSI 3 5 3 BB
IBF 2001 YABANCI DILDE OKUMA VE KONUSMA 3 4 3 BA
ATA 1001 Ataturk Ilkeleri ve InkIlap Tarihi I 2 2 2 BA
YDI 1009 YabancI Dil I (Ingilizce) 3 3 3 BB
EMT 2004 ILERI MATEMATIK II 3 4 3 CB
EMT 2006 VERI TABANI YONETIM SISTEMLERI 3 3 3 CB
EMT 2008 MATEMATIKSEL ISTATISTIK II 3 4 3 BB
IKT 2008 MAKRO IKTISAT 3 4 3 AA
ISL 3009 FINANSAL TABLOLAR ANALIZI 3 5 3 AA
EMT 2066 MESLEKI YABANCI DIL I (EKONOMETRI) 3 5 3 AA
ATA 1002 Ataturk Ilkeleri ve InkIlap Tarihi II 2 2 2 BA
YDI 1008 YabancI Dil II (Ingilizce) 3 3 3 AA
EMT 3005 EKONOMETRIYE GIRIS I 3 5 3 CC
EMT 3009 PARAMETRIK OLMAYAN ISTATISTIK 3 5 3 CC
EMT 3011 YONEYLEM ARASTIRMASI I 3 5 3 CB
EMT 3013 SISTEM ANALIZI VE MODELLEME 3 5 3 CC
EMT 3017 ISTATISTIKSEL ANALIZ VE ARASTIRMA YONTEMLERI 3 5 3 AA
EMT 3067 MESLEKI YABANCI DIL II (EKONOMETRI) 3 5 3 BA
EMT 3004 EKONOMETRIYE GIRIS II 3 5 3 CB
EMT 3006 YONEYLEM ARASTIRMASI II 3 5 3 CC
EMT 3008 ORNEKLEME YINTEMLERI 3 5 3 BB
EMT 3016 REGRESYON ANALIZI 3 5 3 AA
EMT 3020 BILGISAYAR DESTEKLI EKONOMETRIK UYGULAMALAR 3 5 3 BA
EMT 3022 UYGULAMALI OPTIMIZASYON 3 5 3 AA
EMT 4003 EKONOMETRIK MODELLER 3 5 3 BA
EMT 4005 COK DEGISKENLI ISTATISTIKSEL ANALIZ 3 5 3 BA
EMT 4009 BENZETIM TEORISI 3 5 3 CB
IKT 3024 MATEMATIKSEL IKTISAT 3 5 3 BA
EMT 4011 VERI MADENCILIGI 3 5 3 BA
IKT 4025 PARA VE SERMAYE PIYASALARI 3 5 3 BA
EMT 4004 UYGULAMALI EKONOMETRI 3 5 3 AA
EMT 4008 ISTATISTIKSEL KALITE KONTROLU 3 5 3 BA
EMT 4010 STOKASTIK SURECLER 3 5 3 BA
EMT 4018 BILGISAYAR DESTEKLI VERI ANALIZI 3 5 3 BA
EMT 4012 EKONOMETRI SEMINERI 3 5 3 AA
EMT 4024 IS HAYATI ICIN TAHMIN TEKNIKLERI 3 5 3 AA'

# Now this is just a long character. I should parse this into a data.frame by 
# using string manipulation methods.

each_class <- strsplit(transcript , split = '\n')[[1]]
each_class_splitted_by_tab <- strsplit(each_class , split = ' ')

codes <- vector()
code_number <- vector()
tk <- vector()
akts <- vector()
tk2 <- vector()
grade <- vector()

for (each in each_class_splitted_by_tab) {
  codes <- append(codes , each[1])
  code_number <- append(code_number , each[2])
  tk <- append(tk , each[length(each)-3])
  akts <- append(akts , each[length(each)-2])
  tk2 <- append(tk2 , each[length(each)-1])
  grade <- append(grade , each[length(each)])
  
}

course_name <- vector()

for (each in each_class_splitted_by_tab) {
  
  current_class_name <- each[3:(length(each)-4)]
  empty_string <- character()
  for (i in 1:(length(current_class_name))) {
    empty_string <- paste(empty_string , current_class_name[i])
  }
  
  course_name <- append(course_name , empty_string)
                            
}


all_of_the_courses_as_list <- list( 'department_codes' = codes ,
                                    'class_number' = code_number ,
                                    'course_name' = course_name ,
                                    'tk' = tk ,
                                    'AKTS/ECTS' = akts ,
                                    'tk2' = tk2 ,
                                    'grade' = grade 
                                    )

all_of_the_courses_as_list

courses_df <- data.frame(all_of_the_courses_as_list)

# Now this undergraduate transcript is a data.frame object.It is still not ready
# to be analyzed so I will work on the details a little bit more.

semester <- vector()
number_of_semesters <- c(1:8)
number_of_courses_per_semester <- c(9,9,8,8,6,6,6,6)
for (i in seq_along(number_of_semesters)) {
  semester <- c(semester , rep(number_of_semesters[i] , number_of_courses_per_semester[i]))
}
courses_df$semester <- semester


courses_df$tk <- as.integer(courses_df$tk)
courses_df$tk2 <- as.integer(courses_df$tk2)
courses_df$AKTS.ECTS <- as.integer(courses_df$AKTS.ECTS)


course_grades <- c(sort(unique(grade))[-2] , sort(unique(grade))[2])
course_grades <- rev(course_grades)
courses_df$grade <- factor(x = courses_df$grade ,
                              levels = course_grades ,
                              ordered = TRUE)


number_grade <- c(NA , seq(2 , 4 , 0.5))
letter_grade <- course_grades
credit_grade <- c()
for (i in seq_along(courses_df$grade)) {
  the_number_grade <- number_grade[courses_df$grade[i] == letter_grade]
  credit_grade <- append(credit_grade , the_number_grade)
}
courses_df$credit_grade <- credit_grade


courses_df <-courses_df %>%
  mutate(course_credit = (tk*credit_grade))

# cumsum() suprisingly does not have a na.rm argument

courses_df$course_credit[is.na(courses_df$course_credit)] <- 0
courses_df$cumulative_credit <- cumsum(courses_df$course_credit)
courses_df$course_credit[courses_df$course_credit == 0] <- NA



# It is ready! Now we add semester GPA.

courses_df <- courses_df %>%
  filter(!is.na(credit_grade)) %>%
  group_by(semester) %>%
  summarize(semester_gpa = sum(course_credit)/sum(tk)) %>%
  full_join(courses_df) %>%
  as.data.frame() %>%
  select(!contains('semester_gpa') , 'semester_gpa')

# Some visualizations.

courses_df %>%
  group_by(semester) %>%
  summarize(semester_gpa = mean(semester_gpa)) %>%
  ggplot() + 
  geom_col(mapping = aes(x = semester , y = semester_gpa)) + 
  geom_abline(mapping = aes( intercept = 3 , slope = 0 , color = 'red')) + 
  geom_abline(mapping = aes( intercept = 3.5 , slope = 0 , color = 'red')) + 
  geom_abline(mapping = aes( intercept = 2.5 , slope = 0 , color = 'red') )  +
  theme(legend.position = "none") + 
  labs(title = "GPA by Semester") + 
  xlab(label = "Semesters") + 
  ylab(label = "GPA")
  

# Finally , after having converted some text into a usable data by parsing it ,
# we service it as a CSV file.

write_excel_csv(x = courses_df , file = "transcript.csv")






