
#### Outline ####
#   Audience
#     Head of Academics
#
#   Question
#     What strategies should we take to
#     ensure student success?
#
#   Methodology
#     How do we measure success?
#       Class:
#         Low-Level: interval includes values from 0 to 69,
#         Middle-Level: interval includes values from 70 to 89,
#         High-Level: interval includes values from 90-100.
#
#     Possible approaches - advantages/disadvantages
#
#     Descriptive: 
#       Are there differences in the frequency of L, M, H grades by variable?
#     Prescriptive: 
#       Can we predict these classes?
#         Decision tree?
#
#   Results
#     Ex. A: Students in less are less likely to succeed
#
#   Recommendations
#     Ex. A: Encourage supplemental instruction
#
#   Next steps
#     Implement changes described
#     Monitoring dashboard
#     Experimental testing
#     Track students by ID for longitudinal study


# Load Packages -----------------------------------------------------------


library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(rpart)
library(rpart.plot)
library(gridExtra)


# Load Data ---------------------------------------------------------------


edu_data <- read_csv("./xAPI-Edu-Data.csv") %>% 
  rename_all(str_to_lower) %>% 
  mutate(class = fct_relevel(class, c("L", "M", "H")))


# Begin some EDA ----------------------------------------------------------


# View data summary
skimr::skim(edu_data)


# Grades by course
edu_data %>% 
  group_by(topic, class) %>% 
  summarise(N = n()) %>% 
  mutate(pct = N / sum(N),
         h_pct = mean(if_else(class == "H", pct, NA_real_), na.rm = TRUE),
         total_n = sum(N)) %>% 
  ggplot(aes(x = fct_reorder(topic, h_pct), y = pct, fill = class)) +
  geom_col() +
  geom_label(inherit.aes = FALSE, 
             data = edu_data %>% 
               group_by(topic) %>% 
               count(), 
             aes(x = topic, y = 1.1, 
                 label = paste0("N = ", n)),
             fill = "#F0F0F0",
             color = "#535353") +
  scale_fill_brewer(palette = "RdYlGn",
                    name = "Class (i.e. Success level)",
                    labels = c("Low", "Middle", "High")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  theme_fivethirtyeight(base_size = 16) +
  labs(title = "How Does Student Success Vary by Course?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by course")

engagement <- function(var) {
  edu_data %>% 
    ggplot(aes(y = !!enquo(var), x = class,  fill = class)) +
    geom_violin() +
    geom_boxplot(width = 0.05) +
    scale_fill_brewer(palette = "RdYlGn", position = NULL) +
    scale_x_discrete(labels = c("Low", "Middle", "High")) +
    theme_fivethirtyeight(base_size = 10) +
    theme(legend.position="none")
}


p1 <- engagement(raisedhands) + 
  labs(title = "Are Students that Participate in Class More Successful?",
       subtitle = "Number of Times Student Raised Hand in Class")
p2 <- engagement(discussion) + 
  labs(title = "Are Students that Participate in Class More Successful?",
       subtitle = "Frequency of Times Student Participated in Group Discussion")
p3 <- engagement(visitedresources) + 
  labs(title = "Are Students that Engage with Coursework More Successful?",
       subtitle = "Frequency of Times Student Visited Course Content")
p4 <- engagement(announcementsview) + 
  labs(title = "Are Students that View Announcements More Successful?",
       subtitle = "Frequency of Times Student Checked New Announcements")

grid.arrange(p1, p2, p3, p4)

# Grades by days of absence
edu_data %>% 
  group_by(studentabsencedays, class) %>% 
  summarise(N = n()) %>% 
  mutate(pct = N / sum(N),
         h_pct = mean(if_else(class == "H", pct, NA_real_), na.rm = TRUE)) %>% 
  ggplot(aes(x = studentabsencedays, y = pct, fill = class)) +
  geom_col() +
  geom_label(inherit.aes = FALSE, 
             data = edu_data %>% 
               group_by(studentabsencedays) %>% 
               count(), 
             aes(x = studentabsencedays, y = 1.1, 
                 label = paste0("N = ", n)),
             fill = "#F0F0F0",
             color = "#535353") +
  scale_fill_brewer(palette = "RdYlGn",
                    name = "Class (i.e. Success level)",
                    labels = c("Low", "Middle", "High")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = seq(0, 1, 0.2), 
                     labels = scales::percent) +
  theme_fivethirtyeight(base_size = 14) + 
  labs(title = "Do Students that Attend Class Succeed at Higher Rates?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by days absent")


# IT appears to have weakest performance, let's see why
itPlot <- function(metric, course = "IT") {
  edu_data %>%
    mutate(tmp_fct = if_else(topic == course, course, "All Others"),
           r = cut_width(!!enquo(metric), boundary = 0, width = 10)) %>%
    group_by(tmp_fct, r) %>% 
    summarize(n = n()) %>% 
    mutate(pct = n/sum(n)) %>% 
    ggplot(aes(x = r, y = pct, fill = tmp_fct)) +
    geom_bar(stat = "identity", 
             position = position_dodge2(preserve = "single"), 
             alpha = 0.8, 
             color = "transparent") +
    theme_fivethirtyeight(base_size = 14) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_discrete(name = "Course") +
    guides(fill = guide_legend(reverse = TRUE)) 
}

it1 <- itPlot(raisedhands) +
  labs(title = "1a. In-Class Participation",
       subtitle = "Frequency of Times Student Raised Hand in Class")

it2 <- itPlot(discussion) +
  labs(title = "1b. In-Class Participation",
       subtitle = "Frequency of Times Student Participated in Group Discussions")  

it3 <- itPlot(visitedresources) +
  labs(title = "2a. Out-of-Class Participation",
       subtitle = "Frequency of Times Student Visited Course Content")

it4 <- itPlot(announcementsview) +
  labs(title = "2b. Out-of-Class Participation",
       subtitle = "Frequency of Times Student Checked New Announcements")  

grid.arrange(it1, it2, it3, it4)

# Biology
bio1 <- itPlot(raisedhands, "Biology") +
  labs(title = "1a. In-Class Participation",
       subtitle = "Frequency of Times Student Raised Hand in Class")

bio2 <- itPlot(discussion, "Biology") +
  labs(title = "1b. In-Class Participation",
       subtitle = "Frequency of Times Student Participated in Group Discussions")  

bio3 <- itPlot(visitedresources, "Biology") +
  labs(title = "2a. Out-of-Class Participation",
       subtitle = "Frequency of Times Student Visited Course Content")

bio4 <- itPlot(announcementsview, "Biology") +
  labs(title = "2b. Out-of-Class Participation",
       subtitle = "Frequency of Times Student Checked New Announcements")  

grid.arrange(bio1, bio2, bio3, bio4)



# Grades by gender
edu_data %>% 
  mutate(gender = fct_recode(gender, Male = "M", Female = "F")) %>% 
  group_by(gender, class) %>% 
  summarise(N = n()) %>% 
  mutate(pct = N / sum(N),
         h_pct = mean(if_else(class == "H", pct, NA_real_), na.rm = TRUE)) %>% 
  ggplot(aes(x = gender, y = pct, fill = class)) +
  geom_col() +
  geom_label(inherit.aes = FALSE, 
             data = edu_data %>% 
               mutate(gender = fct_recode(gender, Male = "M", Female = "F")) %>% 
               group_by(gender) %>% 
               count(), 
             aes(x = gender, y = 1.15, 
                 label = paste0("N = ", n)),
             fill = "#F0F0F0",
             color = "#535353") +
  scale_fill_brewer(palette = "RdYlGn",
                    name = "Class (i.e. Success level)",
                    labels = c("Low", "Middle", "High")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.2), 
                     breaks = seq(0, 1, 0.2), 
                     labels = scales::percent) +
  theme_fivethirtyeight(base_size = 10) +
  labs(title = "Do Males or Females Perform Better?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by gender")

edu_data %>% 
  mutate(tmp_fct = if_else(topic == "Biology", "Biology", "All Others")) %>% 
  ggplot(aes(x = class, y = visitedresources, fill = class)) +
  geom_violin() +
  geom_boxplot(width = 0.05) +
  scale_fill_brewer(palette = "RdYlGn", position = NULL) +
  scale_x_discrete(labels = c("Low", "Middle", "High")) +
  theme_fivethirtyeight(base_size = 10) +
  theme(legend.position="none") +
  facet_wrap(tmp_fct ~ .)


# Grades by parental engagement
par1 <- edu_data %>% 
  group_by(parentansweringsurvey, class) %>% 
  summarise(N = n()) %>% 
  mutate(pct = N/sum(N)) %>% 
  ggplot(aes(x = parentansweringsurvey, y = pct, fill = class)) +
  geom_col() +
  geom_label(inherit.aes = FALSE, 
             data = edu_data %>% 
               group_by(parentansweringsurvey) %>% 
               count(), 
             aes(x = parentansweringsurvey, y = 1.15, 
                 label = paste0("N = ", n)),
             fill = "#F0F0F0",
             color = "#535353") +
  scale_fill_brewer(palette = "RdYlGn",
                    name = "Class (i.e. Success level)",
                    labels = c("Low", "Middle", "High")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.2), 
                     breaks = seq(0, 1, 0.2), 
                     labels = scales::percent) +
  theme_fivethirtyeight(base_size = 10) +
  theme(legend.position="none") +
  labs(title = "Did the Students' Parent Respond to School Surveys?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by parental engagement")

par2 <- edu_data %>% 
  group_by(parentschoolsatisfaction, class) %>% 
  summarise(N = n()) %>% 
  mutate(pct = N/sum(N)) %>% 
  ggplot(aes(x = parentschoolsatisfaction, y = pct, fill = class)) +
  geom_col() +
  geom_label(inherit.aes = FALSE, 
             data = edu_data %>% 
               group_by(parentschoolsatisfaction) %>% 
               count(), 
             aes(x = parentschoolsatisfaction, y = 1.15, 
                 label = paste0("N = ", n)),
             fill = "#F0F0F0",
             color = "#535353") +
  scale_fill_brewer(palette = "RdYlGn",
                    name = "Class (i.e. Success level)",
                    labels = c("Low", "Middle", "High")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.2), 
                     breaks = seq(0, 1, 0.2), 
                     labels = scales::percent) +
  theme_fivethirtyeight(base_size = 10) +
  labs(title = "How does the Students' Parent Feel about the School?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by parental satisfaction")

grid.arrange(par1, par2)


# Did students improve from one semester to the next?
edu_data %>% 
  mutate(semester = fct_recode(semester, First = "F", Second = "S")) %>% 
  group_by(semester, class) %>% 
  summarize(N = n()) %>% 
  mutate(pct = N/sum(N)) %>% 
  ggplot(aes(x = semester, y = pct, fill = class)) +
  geom_col() +
  scale_fill_brewer(palette = "RdYlGn",
                    name = "Class (i.e. Success level)",
                    labels = c("Low", "Middle", "High")) +
  theme_fivethirtyeight(base_size = 10) +
  labs(title = "Did Students Improve from First to Second Semester?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by semester")

# What about by grade level?
edu_data %>% 
  mutate(semester = fct_recode(semester, First = "F", Second = "S")) %>% 
  group_by(semester, gradeid, class) %>% 
  summarize(N = n()) %>%
  mutate(pct = N/sum(N)) %>% 
  ggplot(aes(x = semester, y = pct, fill = fct_rev(class))) +
  geom_col() +
  geom_label(inherit.aes = FALSE, 
             data = edu_data %>% 
               mutate(semester = fct_recode(semester, First = "F", Second = "S")) %>% 
               group_by(semester, gradeid) %>% 
               count(), 
             aes(x = semester, y = -0.1, 
                 label = paste0("N = ", n)),
             fill = "#F0F0F0",
             color = "#535353") +
  scale_y_continuous(limits = c(-0.15, 1), 
                     breaks = seq(0, 1, 0.2), 
                     labels = scales::percent) +
  facet_wrap(. ~ gradeid, ncol = 5) + 
  scale_fill_brewer(palette = "RdYlGn", direction = -1,
                    name = "Class (i.e. Success level)",
                    labels = c("High", "Middle", "Low")) +
  theme_fivethirtyeight(base_size = 14) +
  labs(title = "Did Students Improve from First to Second Semester?",
       subtitle = "Percent of students achieving High-, Middle-, and Low-level grades by grade, by semester")


# Decision Tree -----------------------------------------------------------


# Set random seed for reproducibility
set.seed(21)

# 10-fold cross validation
xval_tree <- rpart(fct_rev(fct_recode(class, High = "H", Middle = "M", Low = "L")) ~ ., 
                   data = edu_data,
                   method = "class", 
                   control = rpart.control(minbucket = 10, xval = 10))

# Plot the cross validated error to select complexity parameter
# Notice the elbow near cp = 0.033
plotcp(xval_tree)

# Select the final model
final_tree <- prune(xval_tree, cp = 0.0333)

# Plot the final tree
rpart.plot(final_tree, box.palette = '-RdYlGn')

# View the accuracy of the model
mean(predict(final_tree, edu_data, type = "class") == fct_rev(fct_recode(edu_data$class, High = "H", Middle = "M", Low = "L")))
