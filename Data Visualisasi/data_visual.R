
# LOAD CSV 

Participant = read.csv('Participant.csv', fileEncoding = 'UTF-8-BOM')

Result = read.csv('Result.csv', fileEncoding = 'UTF-8-BOM')

Question = read.csv('Question.csv', fileEncoding = 'UTF-8-BOM')

Covid_world  = read.csv('Covid Data - world.csv', fileEncoding = 'UTF-8-BOM')


# DATA QUESTION 

question_1 <- Question$Question[Question$Question.Number==1]

question_2 <- Question$Question[Question$Question.Number==2]

question_3 <- Question$Question[Question$Question.Number==3]

question_4 <- Question$Question[Question$Question.Number==4]

question_5 <- Question$Question[Question$Question.Number==5]

question_6 <- Question$Question[Question$Question.Number==6]

# Dikarenakan pada question 1 terdapat data NA sehingga kita harus filter data tersebut

Result <- Result[!is.na(Result$Question.1),]

View(Result)


# NO 1 PIE CHART 

# Data for Pie Chart-1:

data_1 <- table(Result$Question.2)

data_label_1 <- round(data_1 / sum(data_1) *100,1)

data_legend_1 <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")


# PIE CHART 1

pie(data_1, labels = data_label_1, col = rainbow(length(data_1)),
    main = strwrap(paste("Result Percentage of Question 2:", question_2),40),
    cex.main = 0.8
)

legend("topright", legend=data_legend_1, fill=rainbow(length(data_1),
),
cex = 0.8
)



# NO 1 PIE CHART-2:

# Data untuk Pie Chart-2:

merged_data <- merge(Participant,Result, by = "Participant.Number")

data_2 <- merged_data$Question.6[merged_data$Gender == "Female"]

data_2 <- table(data_2)

data_label_2 <- round(data_2/ sum(data_2) * 100,1)


# PIE CHART 2

pie(data_2, labels = data_label_2, col = rainbow(length(data_2)), main = strwrap(paste("Result percentage of Question 6 for Female Participant:",question_6),55),cex.main = 0.8)

legend("topright", legend = rownames(data_2), fill = rainbow(length(data_2)),
       cex = 0.8)



# NO 2 GROUP BAR CHART:

# Data untuk Group Bar Chart;

data_2 <- table(merged_data$Question.1,merged_data$Gender)

data_2_labels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

data_2 <- t(data_2)


# GROUP BAR CHART 

barplot(data_2, names.arg = data_2_labels, col = c("red", "orange"), main = strwrap(paste("Result percentage of Question 1:",question_1),50),cex.main = 0.8)

legend("topleft", legend=rownames(data_2), fill = c("red", "orange"), cex = 0.6)



# NO 3 BOX PLOT 

boxplot(Question.1 ~ Gender,data=merged_data, ylab = "Question 1 Answer", col = c("red","orange"), main = strwrap(paste("Relationship of Gender and Result of Question 1:",question_1),50),cex.main = 0.8)

summary(merged_data$Question.1[merged_data$Gender=="Male"])

# NO 4 HISTOGRAM

# Data untuk Histogram:

data_4 <- (Result$Question.4)


# HISTOGRAM

hist(data_4, breaks = 1:5, col = rainbow(11), main = strwrap(paste("Result of Question 4: ", question_4),55),cex.main = 0.8, xlab = "Question 4 Answer", ylab = "Frequency")

# Sebelum lanjut ke step selanjutnya kita harus count seluruh question 1 hinga 5 dan menggabungkan data question 1 hingga 5

# Load Libraries:

library(dplyr)
library(ggplot2)
detach("package:lubridate")                       

# Pertama menghilangkan nilai N/A

result.omitted_data = na.omit(Result)

# Cara mendapatkan Count dari Question 1 hingga Question 5 :

result_omitt_data_1 = result.omitted_data %>% select(Question.1) %>% count(Question.1) %>% rename(Answer = Question.1) %>% mutate(Question = "Question.1")

result_omitt_data_2 = result.omitted_data %>% select(Question.2) %>% count(Question.2) %>% rename(Answer = Question.2) %>% mutate(Question = "Question.2")

result_omitt_data_3 = result.omitted_data %>% select(Question.3) %>% count(Question.3) %>% rename(Answer = Question.3) %>% mutate(Question = "Question.3")

result_omitt_data_4 = result.omitted_data %>% select(Question.4) %>% count(Question.4) %>% rename(Answer = Question.4) %>% mutate(Question = "Question.4")

result_omitt_data_5 = result.omitted_data %>% select(Question.5) %>% count(Question.5) %>% rename(Answer = Question.5) %>% mutate(Question = "Question.5")

# Kedua kita akan menggabungkan semua data dari q1 hingga q5 

result.merged_data_1_to_5 = rbind(
  result_omitt_data_1,
  result_omitt_data_2, 
  result_omitt_data_3, 
  result_omitt_data_4, 
  result_omitt_data_5
)

# NO 5 STACKED BAR CHART 

ggplot(
  result.merged_data_1_to_5, aes(x = Question, y = n, fill = Answer)
) + geom_bar(
  position = position_stack(),
  stat = 'identity'
)

# NO 6 DOT CHART & LINE CHART

result_survey_data = result.omitted_data %>% count(Date)

# DOT CHART
ggplot(
  result_survey_data, aes(x = Date, y = n)
) + geom_dotplot(
  binaxis = 'y', stackdir = 'center'
)


# LINE CHART
ggplot(
  result_survey_data, aes(x = Date, y = n)
) + geom_line(
  aes(group = 1)
)

# NO 7 DOTTED LINE CHART 

# 1 Open png file

png(filename = 'result.png')

# 2 Dotted Line Chart

ggplot(
  result_survey_data, aes(x = Date, y = n)
) + geom_dotplot(
  binaxis = 'y',
  stackdir = 'center'
) + geom_line(
  aes(group = 1)
) 

# 3 Flush output file

dev.off()


