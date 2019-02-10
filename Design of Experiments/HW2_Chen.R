
library(dplyr)
library(ggplot2)

survey_data = read.csv("/Users/chenhuizhang/Desktop/Spring 1/DOE/HW/orange team 5.csv")

loc_1 = c(-78.878130,35.89314)
loc_2 = c(-78.875880,35.74628)
loc_3 = c(-78.676540,35.7724)
loc_4 = c(-79.054280,35.90535)
loc_5 = c(-78.575981,35.86696)

#### calculating the distance to location 
survey_data = survey_data %>%
  mutate(target_dist = if_else(
    Location==1,sqrt((-78.878130-LONG)^2+(35.89314-LAT)^2),
    if_else(Location==2,sqrt((-78.875880-LONG)^2+(35.74628-LAT)^2),
            if_else(Location==3,sqrt((-78.676540-LONG)^2+(35.7724-LAT)^2),
              if_else(Location==4,sqrt((-79.054280-LONG)^2+(35.90535-LAT)^2),
                      sqrt((-78.575981-LONG)^2+(35.86696-LAT)^2)
              )
            ))
  ))

#### some descriptive statistics
# 240(5*3*4*4) treatment groups with an average size of 19 or 20 people
table(survey_data$Location,survey_data$Experience,survey_data$Price,survey_data$Other)
table(survey_data$Experience)
table(survey_data$Price)
table(survey_data$Other)

# location 2 has the highest response rate regardless the other factors
loc_table = table(survey_data$will_attend,survey_data$Location)
prop.table(loc_table,2)

# experience 2 has the highest response rate regardless the other factors
exp_table = table(survey_data$will_attend,survey_data$Experience)
prop.table(exp_table,2)

# however loc 2 + exp 3 combined has the highest response rate 7.7%
prop.table(table(survey_data$will_attend,survey_data$Location,survey_data$Experience),2)

# price 2 highest
price_table = table(survey_data$will_attend,survey_data$Price)
prop.table(price_table,2)

# people like to have both arcade and putt-putt
other_table = table(survey_data$will_attend,survey_data$Other)
prop.table(other_table,2)

#### other non-decision variables
# male is slightly more likely to go
sex_table = table(survey_data$will_attend,survey_data$sex)
prop.table(sex_table,2)

# age group around 36-40 is the largest attendee group
ggplot(survey_data,aes(x=ages)) + 
  geom_histogram(data=subset(survey_data,will_attend == 0),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(survey_data,will_attend == 1),fill = "blue", alpha = 0.2) 


# rich people with income >150k are least likely to go, middle class from 25k-150k like it more
inc_table = table(survey_data$Location,survey_data$income)
prop.table(inc_table,2)


#### build a logistic regression
glm.fit <- glm(will_attend ~ target_dist + factor(Location) + factor(Experience) + factor(Other) + factor(Price) + sex + ages + income, data = survey_data, family = binomial)
summary(glm.fit)

# model findings:
# distance to location is significant -> the far from the park the less likely people are to to go
# only location 2 has a positive coefficient
# experience 2 and 3 are almost equally strongly positive
# other 4 stands out as the best options
# price 1 and 2 has no significant difference, but 3 and 4 are negative

# the older people are the less likely they are attending
# income groups 25-50k and 74-100k are most certain to go
# gender makes no differences
