library(tidyverse)

tips <- read.csv("restaurant_tips/tip.csv")

str(tips)
table(tips$day)
# its only for 4 diferent days data so,
tips_v2 <- tips %>% 
  mutate(day = recode(day, 
                       "Fri" = "Friday",
                       "Sat" = "Saturday",
                       "Sun" = "Sunday",
                       "Thur" = "Thursday"))

# creating a vector to line up the days
days <- c("Thursday","Friday","Saturday","Sunday")
tips_v2$day <- factor(tips_v2$day, levels = days)

# there is a sex column and also size colum that is the number how many ---
# people dining together
# so if we want to analyze by gender, we need a new df that people solo dining
table(tips_v2$size)
# but there is only 4 people that dines solo so any analysis out of this data ---
# might be biased so we do not include gender spesific analysis

summary(tips_v2$total_bill)
summary(tips_v2$tip)

# this to compare analyze total bills
mean_total_bill <- tips_v2 %>%
  group_by(size) %>%
  summarise(
    mean_total_bill = mean(total_bill),
    median_total_bill = median(total_bill)
  )


ggplot(mean_total_bill, aes(x=size, y=mean_total_bill)) +
  geom_bar(stat = "identity",fill = "#86CD82", alpha = 0.7) +
  geom_point(aes(y=median_total_bill), size = 2, color = "red") +
  geom_text(aes(y=median_total_bill, label = round(median_total_bill,1)),
            vjust = -0.9, color = "red") +
  labs(title = "Average and Median Total Bill by Group Size",
       x = "Group Size",
       y = "Total Bill") +
  scale_x_continuous(breaks = 1:6) + 
  theme_minimal()

mean_tips <- tips_v2 %>% 
  group_by(size) %>% 
  summarise(
    mean_tip=mean(tip),
    median_tip=median(tip)
  )

ggplot(mean_tips, aes(x= size, y= mean_tip)) +
  geom_bar(stat = "identity",fill="#86CD82", alpha = 0.7) +
  geom_point(aes(y=median_tip), size = 2, color ="red") +
  geom_text(aes(y=median_tip, label = round(median_tip,1)),
            vjust = -0.9, color = "red") +
  labs(title = "Average and Median Tips by Group Size",
       x = "Group Size",
       y= "Tip Amount",
       fill = "Group Size") +
  scale_x_continuous(breaks = 1:6) +
  theme_minimal()

# ----------------------------------------------------------------------------
# question 1
# does the tip rate changes according group size 

tips_v2 <- tips_v2 %>% 
  mutate(tip_rate = tip / total_bill)

tip_rate <- tips_v2 %>% 
  group_by(size) %>% 
  summarise(
    mean_tip_rate = mean(tip_rate),
    median_tip_rate = median(tip_rate)
  )


ggplot(tip_rate, aes(x= size, y = mean_tip_rate)) +
  geom_bar(stat = "identity",fill = "#86CD82", alpha = 0.7) +
  geom_point(aes(y = median_tip_rate), size = 2, color = "red") +   # sadece noktayı çiz
  geom_text(aes(y = median_tip_rate, label = round(median_tip_rate, 3)),  # metni ekle
            vjust = -1.6, color = "red") +
  labs(title = "Average and Median Tip Rate by Group Size",
       x = "Group Size",
       y= "Tip Rate",
       fill = "Group Size") +
  scale_x_continuous(breaks = 1:6) +
  theme_minimal()
# ----------------------------------------------------------------------------

# question 2
# is there any change of bill per person according group size ?

tips_v2 <- tips_v2 %>% 
  mutate(per_person_bill = total_bill/size)

mean_person_bill <- tips_v2 %>% 
  group_by(size) %>% 
  summarise(mean_per_person = mean(per_person_bill))

ggplot(mean_person_bill) +
  geom_line(aes(x=size, y=mean_per_person), size = 0.7) +
  labs(title = "Per Person Bill / Group Size",
       x = "Group Size",
       y= "Average Per Person Bill") +
  scale_x_continuous(breaks = 1:6) +
  ylim(0,NA) +
  theme_minimal()

ggplot(tips_v2, aes(x= size, y= per_person_bill)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = 1:6) +
  ylim(0,NA) +
  labs(title = "Per Person Bill vs Group Size",
       x = "Group Size",
       y = "Per Person Bill")+
  theme_minimal()



# ----------------------------------------------------------------------------
# question 3
# what is the relationship between size, total_bill, tip and tip rate

cor(tips_v2$size, tips_v2$per_person_bill)
cor(tips_v2$size, tips_v2$tip)
cor(tips_v2$size, tips_v2$tip_rate)


# ----------------------------------------------------------------------------
# question 4
# days compared

mean_tips_day <- tips_v2 %>% 
  group_by(day) %>% 
  summarise(
    mean_tip=mean(tip),
    median_tip=median(tip)
  )

ggplot(mean_tips_day, aes(x= day, y= mean_tip, fill=day)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_point(aes(y=median_tip), size = 2, color ="red") +
  geom_text(aes(y=median_tip, label = round(median_tip,1)),
            vjust = -0.9, color = "red") +
  labs(title = "Average and Median Tips by Day",
       x = "Day of Week",
       y= "Tip Amount",
       fill = "Days") +
  theme_minimal()

# ----------------------------------------------------------------------------
# question 5
# group sizes compared by days

ggplot(tips_v2, aes(x = size, fill = day)) +
  geom_bar() +
  facet_wrap(~day,  strip.position = "bottom") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    axis.text.x = element_text(size = 10),
    strip.background = element_blank()
  ) +
  labs(
    title = "Group Sizes Compared by Day",
    x = "Group Size",
    y = "Count"
  )


# we see that 2-3 people groups tend to go dining at weekends




tip_rate_smoker <- tips_v2 %>% 
  group_by(smoker) %>% 
  summarise(
    mean_tip_rate = mean(tip_rate),
    median_tip_rate = median(tip_rate)
  )


ggplot(tip_rate_smoker, aes(x=smoker , y = mean_tip_rate)) +
  geom_bar(stat = "identity",fill = "#86CD82", alpha = 0.7) +
  geom_point(aes(y = median_tip_rate), size = 2, color = "red") +
  geom_text(aes(y = median_tip_rate, label = round(median_tip_rate, 3)),
            vjust = -0.6, color = "red") +
  labs(title = "Average and Median Tip Rate by Smokers",
       x = "Smoker",
       y= "Tip Rate",
       fill = "Smoker") +
  ylim(0,0.2)+
  theme_minimal()

ggplot(tips_v2, aes(x = smoker, fill = day)) +
  geom_bar() +
  facet_wrap(~day,  strip.position = "bottom") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    axis.text.x = element_text(size = 10),
    strip.background = element_blank()
  ) +
  labs(
    title = "Smokers Compared by Day",
    x = "Smokers",
    y = "Count"
  )

