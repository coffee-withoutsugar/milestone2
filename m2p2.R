library(ggplot2)
library(dplyr)
dataset_url <- "https://uwmadison.box.com/shared/static/g6uvupaaf8q4rn6mrfpowenh9o7f0baz.csv"
student_data <- read.csv(dataset_url)
ggplot(student_data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "score vs study hours", x = "study hours", y = "score")

ggplot(student_data, aes(x = Sleep_Hours, y = Exam_Score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "score vs sleep hours", x = "sleep hours", y = "score")

ggplot(student_data, aes(x = Sleep_Hours, y = Exam_Score, fill = Sleep_Hours )) +
  #scale_fill_gradient(low = "lightblue", high = "darkblue") +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.5, size = 3, color = "black") +
  labs(title = "score vs sleep hours", x = "sleep hours", y = "score")
avg_scores <- student_data %>%
  group_by(Peer_Influence) %>%
  summarise(Avg_Exam_Score = mean(Exam_Score, na.rm = TRUE) - 60)

ggplot(avg_scores, aes(x = Peer_Influence, y = Avg_Exam_Score, fill = Peer_Influence)) +
  geom_bar(stat = "identity") +
  labs(title = "score vs Peer Influence", x = "Peer Influence", y = "Average Exam Score - 60") +
  theme_minimal()