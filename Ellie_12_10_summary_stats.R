# ES summary statistics

library(tidyverse)

#KF - I switched this to the March 2024 archive dataset
nyc_2024 <- read.csv("nyc_mar2024.csv") 

# these are all the variables that seemed at all related to variables in the kaggle dataset
nyc_2024 <- nyc_2024 %>%
  select(id, name, host_id, host_name, neighborhood_overview, neighbourhood_cleansed, neighbourhood_group_cleansed,latitude, longitude, room_type, price, last_scraped, last_review, minimum_nights, number_of_reviews, last_review, reviews_per_month, reviews_per_month, calculated_host_listings_count, availability_365, number_of_reviews_ltm, license, host_location, host_since, host_neighbourhood, host_listings_count, host_total_listings_count,  number_of_reviews_l30d )
head(nyc_2024)

#39,319 total listings
dim(nyc_2024)

#has reviews in the last 6 months
filtered_nyc_2024 <- nyc_2024 %>%
  mutate(last_review_date = as.Date(last_review) ) %>%
  filter(last_review_date >= max(last_review_date, na.rm = T) %m-% months(6) )
#Confirm that this dataset covers listings with reviews between 9/7/2023 and 3/7/2024
#This is six months after the law came into effect and can cover the same time period as the 2023 Kaggle data
filtered_nyc_2024 %>% select(last_review_date) %>% summary()

dim(filtered_nyc_2024)

## ES 12/7 cutting things without a price
post_law <- filtered_nyc_2024 %>%
  filter(price != "") %>%
  mutate(price = as.numeric(str_remove(price, "\\$")) )

# 10704 total. so top 1% is the top 107 listings, so 
post_law_ordered <- post_law %>% 
  arrange(desc(price)) 


#cut off top prices
post_law <- post_law_ordered[108:10704,]
hist(post_law$price)  


dim(post_law)

nyc_2023 <- read.csv("nyc_2023.csv")
dim(nyc_2023)

## "OLD" data ##
#entries with reviews in the last 6 months. 18,993 listings. so from 2022-09-06 to 2023-03-06
filtered_nyc_2023 <- nyc_2023 %>%
  mutate(last_review_date = as.Date(last_review) ) %>%
  filter(last_review_date >= max(last_review_date, na.rm = T) %m-% months(6) )

filtered_nyc_2023 %>% select(last_review_date) %>% summary()

dim(filtered_nyc_2023)

# ES update 12/7
pre_law_ordered <- filtered_nyc_2023 %>%
  filter(price != "") %>%
  mutate(price = as.numeric(str_remove(price, "\\$")) ) %>%
  arrange(desc(price))

dim(pre_law)

# 18993 total. so top 1% is 190 the top  listings, so 

#cut off top prices
pre_law <- pre_law_ordered[190:18993,]
hist(pre_law$price)  

#cut out the top 1% to get rid of outliers
pre_law

dim(pre_law)

#ideas: figure out which are the same listings. either through ID or hostID?
## KF - I would suggest using a full join to do this 
joined_prices <- filtered_nyc_2023 %>% select(id, price_2023 = price) %>%
  mutate(in_2023 = 1) %>%
  full_join(filtered_nyc_2024 %>% select(id,neighbourhood_group_cleansed, price_2024 = price) %>%
              mutate(in_2024 = 1))





##ES - updating KF's join code to use "pre_law" and "post_law" (also with non empty prices)
joined_prices1 <- pre_law %>% select(id, pre_price = price) %>%
  mutate(in_pre = 1) %>%
  full_join(post_law %>% select(id, neighbourhood_group_cleansed, post_price = price) %>%
              mutate(in_post = 1)) 


#Using the "id" variable to join the two datasets, there are
#     7,047 listings in both 2023 and 2024
#     11,946 listings that are only in 2023
#     4,731 listings that are only in 2024
#(Note that I am still referring to the datasets by year, but they should probably be called pre/post-law?)
joined_prices %>% count(in_2023, in_2024)


#now, 
# 6347 in both datasets
# 12457 only in the pre dataset
# 4250 only in the post dataset

(joined_prices1 %>% count(in_pre, in_post))



### summary statistics #### 

## 
pre_box <- joined_prices1 %>%
  filter(in_pre == 1) %>%
  mutate(in_post = ifelse(is.na(in_post), "Not in Post", "In Post"))

post_box <- joined_prices1 %>%
  filter(in_post == 1 & !is.na(post_price)) %>%
  mutate(in_pre = ifelse(is.na(in_pre), "Not in Pre", "In Pre"))

par(mfrow = c(1, 2), title = "Pre and Post Law Prices")
boxplot(pre_box$pre_price ~ pre_box$in_post, names = c("Not in Post", "In Post"), col = c(4, 5), ylab = "Price", xlab = "Pre Law Listings")

boxplot(post_box$post_price ~ post_box$in_pre, names = c("Not in Pre", "In Pre"), col = c(4, 5), ylab = "Price", xlab = "Post Law Listings")

count_pre_box <- pre_box %>%
  group_by(in_post) %>%
  summarise(count = n())

p <- ggplot(pre_box, aes(x = in_post, y = pre_price))
p <- p + geom_boxplot(varwidth = TRUE, fill = "#F8766D", outlier.alpha = 0.1) +
  labs(y = "Price", x = "Pre LL18 Listings") +
   
  geom_text(data = count_pre_box, aes(x = in_post, y = mean(pre_box$pre_price), label = paste("n =", count)), 
            inherit.aes = FALSE, size = 4)

count_post_box <- post_box %>%
  group_by(in_pre) %>%
  summarise(count = n())

p1 <- ggplot(post_box, aes(x = in_pre, y = post_price))
p1 <- p1 + geom_boxplot(varwidth = TRUE, fill = "#F8766D", outlier.alpha = 0.1) +
  labs(y = "Price", x = "Post LL18 Listings") +
  
  geom_text(data = count_post_box, aes(x = in_pre, y = mean(post_box$post_price), label = paste("n =", count)), 
            inherit.aes = FALSE, size = 4)

library(gridExtra)

grid.arrange(p, p1, ncol=2
)



#overall pre_price average. Should cut out the large outliers as well??
(joined_prices1 %>%
   filter(in_pre == 1) %>%
   summarise(average = mean(pre_price), stdv = sd(pre_price), med = median(pre_price), intqr = IQR(pre_price)))

#pre_price average for listings in both
(pre_both_summary <- joined_prices1 %>%
    filter(in_pre == 1 & in_post == 1) %>%
    summarise(average = mean(pre_price), stdv = sd(pre_price), med = median(pre_price), intqr = IQR(pre_price)) )

#pre_price average for listings ONLY in pre
(just_pre_summary <- joined_prices1 %>%
    filter(in_pre == 1 & is.na(in_post)) %>%
    summarise(average = mean(pre_price), stdv = sd(pre_price), med = median(pre_price), intqr = IQR(pre_price)))


#overall post_price average. Should cut out the large outliers as well??
(post_total_summary <- joined_prices1 %>%
    filter(in_post == 1) %>%
    summarise(average = mean(post_price, na.rm = T), stdv = sd(post_price, na.rm = T), med = median(post_price, na.rm = T), intqr = IQR(post_price, na.rm = T)))


## whoops. apparently I didn't remove 65 NAs??? but I thought I did earlier?
(joined_prices1 %>%
    filter(in_post == 1 & is.na(post_price)) )

#post_price average for listings in both. just removing the weird 65 listings that are mysteriously NA.
(post_both_summary <- joined_prices1 %>%
    filter(in_pre == 1 & in_post == 1) %>%
    summarise(average = mean(post_price, na.rm = T,), stdv = sd(post_price, na.rm = T), med = median(post_price, na.rm = T), intqr = IQR(post_price, na.rm = T)))

#post_price average for listings ONLY in post
(just_post_summary <- joined_prices1 %>%
    filter(in_post == 1, is.na(in_pre)) %>%
    summarise(average = mean(post_price, na.rm = T), stdv = sd(post_price, na.rm = T), med = median(post_price, na.rm = T), intqr = IQR(post_price, na.rm = T)))


############ min length ##################

joined_min <- pre_law %>% select(id, pre_min = minimum_nights) %>%
  mutate(in_pre = 1) %>%
  full_join(post_law %>% select(id, neighbourhood_group_cleansed, post_min = minimum_nights) %>%
              mutate(in_post = 1)) %>%
  mutate(in_pre = ifelse(is.na(in_pre), "Not in Pre", "In Pre")) %>%
  mutate(in_post = ifelse(is.na(in_post), "Not in Post", "In Post"))


pre_box_min <- joined_min %>%
  filter(in_pre == "In Pre") %>%
  filter(pre_min <= 90)

post_box_min <- joined_min %>%
  filter(in_post == "In Post") %>%
  filter(post_min <= 90) 

par(mfrow = c(1, 2))


boxplot(pre_box_min$pre_min ~ pre_box_min$in_post, names = c("Not in Post", "In Post"), col = c(4, 5), ylab = "Min Number of Nights", xlab = "Pre Law Listings")

boxplot(post_box_min$post_min ~ post_box_min$in_pre, names = c("Not in Pre", "In Pre"), col = c(4, 5), ylab = "Price", xlab = "Post Law Listings")
length(post_box_min$post_min)

count_pre_box_min <- pre_box_min %>%
  group_by(in_post) %>%
  summarise(count = n())

q1 <- ggplot(pre_box_min, aes(x = in_post, y = pre_min))
 q1 <- q1 + geom_boxplot(varwidth = TRUE, fill = "#F8766D", outlier.alpha = 0.1) +
  labs(y = "Minimum Number of Nights", x = "Pre LL18 Listings") +
  
  geom_text(data = count_pre_box, aes(x = in_post, y = 95, label = paste("n =", count)), 
            inherit.aes = FALSE, size = 4)

count_post_box_min <- post_box_min %>%
  group_by(in_pre) %>%
  summarise(count = n())

q2 <- ggplot(post_box_min, aes(x = in_pre, y = post_min))
q2 <- q2 + geom_boxplot(varwidth = TRUE, fill = "#F8766D", outlier.alpha = 0.1) +
  labs(y = "Minimum Number of Nights", x = "Post LL18 Listings") +
  
  geom_text(data = count_post_box, aes(x = in_pre, y = 95, label = paste("n =", count)), 
            inherit.aes = FALSE, size = 4)

library(gridExtra)

grid.arrange(q1, q2, ncol=2
)



#overall pre_price average. Should cut out the large outliers as well??
(joined_min %>%
    filter(in_pre == 1) %>%
    summarise(average = mean(pre_min), stdv = sd(pre_min), med = median(pre_min), intqr = IQR(pre_min)))

#pre_price average for listings in both
( joined_min %>%
    filter(in_pre == 1 & in_post == 1) %>%
    summarise(average = mean(pre_min), stdv = sd(pre_min), med = median(pre_min), intqr = IQR(pre_min)) )

#pre_price average for listings ONLY in pre
( joined_min %>%
    filter(in_pre == 1 & in_post == 0 ) %>%
    summarise(average = mean(pre_min), stdv = sd(pre_min), med = median(pre_min), intqr = IQR(pre_min)))


#overall post_min average. Should cut out the large outliers as well??
( joined_min %>%
    filter(in_post == 1) %>%
    summarise(average = mean(post_min, na.rm = T), stdv = sd(post_min, na.rm = T), med = median(post_min, na.rm = T), intqr = IQR(post_min, na.rm = T)))


#post_min average for listings in both. just removing the weird 65 listings that are mysteriously NA.
(joined_min %>%
    filter(in_pre == 1 & in_post == 1) %>%
    summarise(average = mean(post_min, na.rm = T,), stdv = sd(post_min, na.rm = T), med = median(post_min, na.rm = T), intqr = IQR(post_min, na.rm = T)))

#post_min average for listings ONLY in post
(joined_min %>%
    filter(in_post == 1 & in_pre == 0) %>%
    summarise(average = mean(post_min, na.rm = T), stdv = sd(post_min, na.rm = T), med = median(post_min, na.rm = T), intqr = IQR(post_min, na.rm = T)))


####################### type of listing ####################


joined_type <- pre_law %>% select(id, room_type, neighbourhood_group) %>%
  mutate(in_pre = 1) %>%
  full_join(post_law %>% select(id, neighbourhood_group_cleansed, room_type) %>%
              mutate(in_post = 1)) %>%
  mutate(in_pre = ifelse(is.na(in_pre), 0, in_pre)) %>%
  mutate(in_post = ifelse(is.na(in_post), 0, in_post)) 

## I want a stacked bar chart 
par(mfrow = c(1, 2))




# library
library(ggplot2)

count_borough <- joined_type %>%
  filter(!is.na(neighbourhood_group)) %>%
  group_by(neighbourhood_group) %>% 
  summarise(count = n())

count_borough_post <- joined_type %>%
  filter(!is.na(neighbourhood_group_cleansed)) %>%
  group_by(neighbourhood_group_cleansed) %>% 
  summarise(count = n())

plot1 <- ggplot(joined_type %>%
                  filter(!is.na(neighbourhood_group)), aes(x = neighbourhood_group, fill = room_type, )) + 
  geom_bar(stat = "count", position = "fill") + 
  theme(legend.position = "none", ) + 
  labs(y = "Pre-LL18 Listings", x = element_blank()) +
  geom_text(data = count_borough, aes(x = neighbourhood_group, y = 1.05, label = paste("n =", count)), 
            size = 3, inherit.aes = FALSE)


plot2 <- ggplot(joined_type %>%
                  filter(!is.na(neighbourhood_group_cleansed)), aes(x = neighbourhood_group_cleansed, fill = room_type )) + 
  geom_bar(stat = "count", position = "fill") + 
  labs(y = "Post-LL18 Listings", x = element_blank() , fill = "Room Type") +
  geom_text(data = count_borough_post, aes(x = neighbourhood_group_cleansed, y = 1.05, label = paste("n =", count)), 
            size = 3, inherit.aes = FALSE)


grid.arrange(plot1, plot2, ncol=2, widths = c(1,1.5)
    )


