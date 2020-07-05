library('jsonlite')
library('tibble')
library('stringr')
library('RJSONIO')
library('dplyr')
library('tidyverse')
library('reader')
library('expss')
library('ggplot2')
library('plotly')
library('gridExtra')
library('reshape2')


### Setting the system to UTF-8 encoding so that characters like é be displayed correctly. ###
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           Data wrangling for business data from 'business.json'                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Reading the business json file ####
business <- stream_in(file('yelp_academic_dataset_business.json'))

business_df <- as_data_frame(flatten(business))

restaurant_df <- business_df %>% filter(str_detect(categories, "Restaurants"))

colSums(is.na(restaurant_df))

### Dropping of columns where the ratio of NA values is more than 80% of the total data for that column ###
rest_df <- restaurant_df[, which(colMeans(!is.na(restaurant_df)) > 0.80)]
colSums(is.na(rest_df))

### Further dropping uncessary columns ###
rest_df <- rest_df[-c(11,13,14,17,18)]

#colnames(rest_df)

### Renaming of the columns for better readbility ###
rest_df <- rest_df %>% rename(Price_range = attributes.RestaurantsPriceRange2)
rest_df <- rest_df %>% rename(Restaurant_takeout = attributes.RestaurantsTakeOut)
rest_df <- rest_df %>% rename(Restaurant_delivery = attributes.RestaurantsDelivery)
rest_df <- rest_df %>% rename(Outdoor_seating = attributes.OutdoorSeating)


### Change the values for better readibility at the output ####
### i.e. True -> 'Yes', False -> 'No', NA or None -> 'Not Available' ###

rest_df$Price_range[is.na(rest_df$Price_range)] <- 'Not Available'
rest_df$Price_range <- str_replace_all(rest_df$Price_range, "None", "Not Available")

rest_df$Restaurant_takeout[is.na(rest_df$Restaurant_takeout)] <- 0
rest_df$Restaurant_delivery[is.na(rest_df$Restaurant_delivery)] <- 0
rest_df$Outdoor_seating[is.na(rest_df$Outdoor_seating)] <- 0


rest_df$Restaurant_takeout <- ifelse(grepl('True', rest_df$Restaurant_takeout, ignore.case = F), 'Yes',
                                     ifelse(grepl('False',rest_df$Restaurant_takeout, ignore.case = F), 'No',
                                            ifelse(grepl('None',rest_df$Restaurant_takeout, ignore.case = F), 'No',
                                                   'Not Available')))

rest_df$Restaurant_delivery <- ifelse(grepl('True', rest_df$Restaurant_delivery, ignore.case = F), 'Yes',
                                      ifelse(grepl('False',rest_df$Restaurant_delivery, ignore.case = F), 'No',
                                             ifelse(grepl('None',rest_df$Restaurant_delivery, ignore.case = F), 'No',
                                                    'Not Available')))

rest_df$Outdoor_seating <- ifelse(grepl('True', rest_df$Outdoor_seating, ignore.case = F), 'Yes',
                                  ifelse(grepl('False',rest_df$Outdoor_seating, ignore.case = F), 'No',
                                         ifelse(grepl('None',rest_df$Outdoor_seating, ignore.case = F), 'No',
                                                'Not Available')))


### Categories list ###
categories <- restaurant_df %>% mutate(categories = as.character(categories)) %>% select(categories)


### Assigning the respective sub-categories for the Restaurants ####
categories$`Sub_category` <- ifelse(grepl("american",categories$categories, ignore.case = T),"American",
                                    ifelse(grepl("burgers",categories$categories, ignore.case = T),"American",
                                           ifelse(grepl("food",categories$categories, ignore.case = T),"American",
                                                  ifelse(grepl("Cheesesteaks",categories$categories, ignore.case = T),"American",
                                                         ifelse(grepl("American (Traditional)",categories$categories, ignore.case = T),"American",
                                                                ifelse(grepl("American (New)",categories$categories, ignore.case = T),"American",
                                                                       ifelse(grepl("Steakhouses",categories$categories, ignore.case = T),"American",
                                                                              ifelse(grepl("Breakfast & Brunch",categories$categories, ignore.case = T),"American",
                                                                                     ifelse(grepl("Sandwiches",categories$categories, ignore.case = T),"American",
                                                                                            ifelse(grepl("african",categories$categories, ignore.case = T),"African",
                                                                                                   ifelse(grepl("asian",categories$categories, ignore.case = T),"Asian",
                                                                                                          ifelse(grepl("asian fusion",categories$categories, ignore.case = T),"Asian",
                                                                                                                 ifelse(grepl("argentine",categories$categories, ignore.case = T),"Argentine",
                                                                                                                        ifelse(grepl("Middle East" ,categories$categories, ignore.case = T),"Arabic",
                                                                                                                               ifelse(grepl("bakeries",categories$categories, ignore.case = T),"Bakery",
                                                                                                                                      ifelse(grepl("delis",categories$categories, ignore.case = T),"Bakery",
                                                                                                                                             ifelse(grepl("Coffee & Tea",categories$categories, ignore.case = T),"Bakery",
                                                                                                                                                    ifelse(grepl("bars",categories$categories, ignore.case = T),"Pub",
                                                                                                                                                           ifelse(grepl("Pub",categories$categories, ignore.case = T),"Pub",
                                                                                                                                                                  ifelse(grepl("sport bar",categories$categories, ignore.case = T),"Pub",
                                                                                                                                                                         ifelse(grepl("nightlife",categories$categories, ignore.case = T),"Pub",
                                                                                                                                                                                ifelse(grepl("clubs",categories$categories, ignore.case = T),"Pub",
                                                                                                                                                                                       ifelse(grepl("buffets",categories$categories, ignore.case = T),"Buffet",
                                                                                                                                                                                              ifelse(grepl("Chinese",categories$categories, ignore.case = T),"Chinese",
                                                                                                                                                                                                     ifelse(grepl("sushi bars",categories$categories, ignore.case = T),"Chinese",
                                                                                                                                                                                                            ifelse(grepl("dumplings",categories$categories, ignore.case = T),"Chinese",
                                                                                                                                                                                                                   ifelse(grepl("cafes",categories$categories, ignore.case = T),"Cafes",
                                                                                                                                                                                                                          ifelse(grepl("Cajun",categories$categories, ignore.case = T),"Cajun",
                                                                                                                                                                                                                                 ifelse(grepl("Donuts",categories$categories, ignore.case = T),"Donuts",
                                                                                                                                                                                                                                        ifelse(grepl("french",categories$categories, ignore.case = T),"French",
                                                                                                                                                                                                                                               ifelse(grepl("filipino",categories$categories, ignore.case = T),"Filipino",
                                                                                                                                                                                                                                                      ifelse(grepl("greek",categories$categories, ignore.case = T),"Greek",
                                                                                                                                                                                                                                                             ifelse(grepl("lebanese",categories$categories, ignore.case = T),"Lebanese",
                                                                                                                                                                                                                                                                    ifelse(grepl("Desserts",categories$categories, ignore.case = T),"Ice-cream Parlor",
                                                                                                                                                                                                                                                                           ifelse(grepl("indian",categories$categories, ignore.case = T),"Indian",
                                                                                                                                                                                                                                                                                  ifelse(grepl("italian",categories$categories, ignore.case = T),"Italian",
                                                                                                                                                                                                                                                                                         ifelse(grepl("juice bars & smoothies",categories$categories, ignore.case = T),"Juice-Bar",
                                                                                                                                                                                                                                                                                                ifelse(grepl("korean",categories$categories, ignore.case = T),"Korean",
                                                                                                                                                                                                                                                                                                       ifelse(grepl("mexican",categories$categories, ignore.case = T),"Mexican",
                                                                                                                                                                                                                                                                                                              ifelse(grepl("polish",categories$categories, ignore.case = T),"Polish",
                                                                                                                                                                                                                                                                                                                     ifelse(grepl("Peruvian",categories$categories, ignore.case = T),"Peruvian",
                                                                                                                                                                                                                                                                                                                            ifelse(grepl("Afghan",categories$categories, ignore.case = T),"Afghan",
                                                                                                                                                                                                                                                                                                                                   ifelse(grepl("pizza",categories$categories, ignore.case = T),"Pizza",
                                                                                                                                                                                                                                                                                                                                          ifelse(grepl("russian",categories$categories, ignore.case = T),"Russian",
                                                                                                                                                                                                                                                                                                                                                 ifelse(grepl("japanese",categories$categories, ignore.case = T),"Japanese",
                                                                                                                                                                                                                                                                                                                                                        ifelse(grepl("seafood",categories$categories, ignore.case = T),"Seafood",
                                                                                                                                                                                                                                                                                                                                                               ifelse(grepl("caribbean",categories$categories, ignore.case = T),"Seafood",
                                                                                                                                                                                                                                                                                                                                                                      ifelse(grepl("thai",categories$categories, ignore.case = T),"Thai",
                                                                                                                                                                                                                                                                                                                                                                             ifelse(grepl("vietnamese",categories$categories, ignore.case = T),"Vietnamese",
                                                                                                                                                                                                                                                                                                                                                                                    "American")))))))))))))))))))))))))))))))))))))))))))))))))


### Combining the sub-category to the original data frame ###
rest_df <- cbind(rest_df,categories)

### Dropping the unnecessary columns from the data frame ###
final_df <- rest_df[-c(11,16)]

### Segregate the restaurants of the state AZ and ON ###
az_df <- final_df[final_df[,'state'] == 'AZ',]

on_df <- final_df[final_df[,'state'] == 'ON',]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                     End of Data wrangling for business data from 'business.json'                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           Data Clean up for Arizona & Ontario states                             #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Cleanup the city names for state AZ ###
az_df$city <- tolower(az_df$city)

az_df$city = str_replace(az_df$city,"pheonix","phoenix")
az_df$city = str_replace(az_df$city,"phoenix az","phoenix")
az_df$city = str_replace(az_df$city,"phoenix valley","phoenix")
az_df$city = str_replace(az_df$city,"phx","phoenix")

az_df$city = str_replace(az_df$city,"central","phoenix")
az_df$city = str_replace(az_df$city,"phoenix city","phoenix")
az_df$city = str_replace(az_df$city,"phoenix village","phoenix")

az_df$city = str_replace(az_df$city,"mes","mesa")
az_df$city = str_replace(az_df$city,"mesaa","mesa")

az_df$city = str_replace(az_df$city,"gelndale","glendale")
az_df$city = str_replace(az_df$city,"glendale az","glendale")
az_df$city = str_replace(az_df$city,"laveen village","laveen")

az_df$city = str_replace(az_df$city,"schottsdale","scottsdale")
az_df$city = str_replace(az_df$city,"scottdale","scottsdale")

az_df$city <- str_to_title(az_df$city)


### Cleanup the city names for State ON ###
on_df$city <- tolower(on_df$city)

on_df$city = str_replace(on_df$city,"caledon east","caledon")
on_df$city = str_replace(on_df$city,"caledon village","caledon")

on_df$city = str_replace(on_df$city,"east gwillimburry","east gwillimbury")

on_df$city = str_replace(on_df$city,"etibicoke","etobicoke")
on_df$city = str_replace(on_df$city,"etobiicoke","etobicoke")

on_df$city = str_replace(on_df$city,"missisauga","mississauga")
on_df$city = str_replace(on_df$city,"mississuaga","mississauga")

on_df$city = str_replace(on_df$city,"oakridges","oak ridges")

on_df$city = str_replace(on_df$city,"oakridges","oak ridges")

on_df$city = str_replace(on_df$city,"north york","north york")

on_df$city = str_replace(on_df$city,"richmond hil","richmond hill")
on_df$city = str_replace(on_df$city,"richmond hilll","richmond hill")

on_df$city = str_replace(on_df$city,"scarobrough","scarborough")

on_df$city = str_replace(on_df$city,"thornhil","thornhill")
on_df$city = str_replace(on_df$city,"thornhilll","thornhill")

on_df$city = str_replace(on_df$city,"tornto","toronto")

on_df$city = str_replace(on_df$city,"whiitby","whitby")
on_df$city = str_replace(on_df$city,"whtiby","whitby")

on_df$city <- str_to_title(on_df$city)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                        End of Data Clean up for Arizona & Ontario states                         #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


### Remove redundant variable to make available of more RAM ###
rm(business_df,business,restaurant_df,categories,rest_df)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#               Bubble Plot - Restaurant price per person distribution across states               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
temp_final <- final_df[c(5,11)]

temp_final$Price_range <- ifelse(grepl('1', temp_final$Price_range, ignore.case = F), '1',
                                                 ifelse(grepl('2',temp_final$Price_range, ignore.case = F), '2',
                                                        ifelse(grepl('3',temp_final$Price_range, ignore.case = F), '2',
                                                               ifelse(grepl('4',temp_final$Price_range, ignore.case = F), '3',
                                                                      ifelse(grepl('Not Available',temp_final$Price_range, ignore.case = F), '0',
                                                                      '')))))

data_group_state_price <- temp_final %>%
  filter(state != '') %>% mutate(tsum = n()) %>%
  group_by(state, Price_range)

data_group_state_price$Price_range <- as.numeric(data_group_state_price$Price_range)

data_weighted_group_state_price <- data_group_state_price %>%
  summarise(total_res_price_range = n()) %>% arrange(desc(Price_range)) %>%
  mutate(total_res = sum(total_res_price_range)) %>% mutate(percent = round((total_res_price_range / total_res)*100, 1))

data_weighted_group_state_price$Price_range <- as.integer(data_weighted_group_state_price$Price_range)

ggplot(data_weighted_group_state_price, aes(x = reorder(state,total_res), y = Price_range, label = paste0(percent))) +
  geom_point(aes(size = percent, color = Price_range), alpha= 1) +
  geom_text(hjust = 0.4, size = 4) + 
  scale_size(range = c(1, 12), guide= "none") +
  labs(title = "Restaurants Price per person by State ", subtitle = "(All numbers are in percentage)",
       x = "States (ascending order in terms of number of restaurants)", 
       y = "Price per Person") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_gradient(name = "Price per Person", breaks= c(0,1,2,3),
                       labels = c("Not Available","Under $10", "$11-60", "Above $60"),
                       guide = "colorbar", low= "yellow", high = "#FF2000") + 
  scale_y_continuous(breaks = c(0,1,2,3), labels= c("Not Available","Under $10", "$11-60", "Above $60")) +
  scale_x_discrete(breaks=c("ON","AZ","NV","QC","OH","NC","PA","AB","WI","IL","AR","AL","BC","CA",
                            "CO","FL","HI","HPL","MB","NE","NY","OR","SC","TX","VA","VT","WA","XWY"),
                   labels= c("Ontario", "Arizona", "Nevada","Quebec","Ohio","North Carolina",
                             "Pennslyvania","Alberta","Wisconsin","Illinois","Arkansas","Alabama","British Columbia",
                             "California","Colorado","Florida","Hawaii","Hartlepool","Manitoba",
                             "Nebraska","New York","Oregon","South Carolina","Texas","Virginia",
                             "Vermont","Washington","Leeds"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#           End of Bubble Plot - Restaurant price per person distribution across states            #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                            States and number of Restaurants                                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ggplot(data = as.data.frame(table(final_df$state)) ,
       aes(x = reorder(Var1, -Freq), y = Freq, label = paste0(Freq))) +
  geom_segment(aes(x = reorder(Var1, -Freq) ,xend=Var1, y=0, yend=Freq), color = 'black') +
  geom_point(size=13, color= 'maroon') +
  geom_text(color = "white", size = 4) +
  labs(x ="States", y= "No. of Restaurants", title = "States and No. of Restaurants") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("ON","AZ","NV","QC","OH","NC","PA","AB","WI","IL","AR","AL","BC","CA",
                            "CO","FL","HI","HPL","MB","NE","NY","OR","SC","TX","VA","VT","WA","XWY"),
                   labels= c("Ontario", "Arizona", "Nevada","Quebec","Ohio","North Carolina",
                             "Pennslyvania","Alberta","Wisconsin","Illinois","Arkansas","Alabama","British Columbia",
                             "California","Colorado","Florida","Hawaii","Hartlepool","Manitoba",
                             "Nebraska","New York","Oregon","South Carolina","Texas","Virginia",
                             "Vermont","Washington","Leeds"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         End of States and number of Restaurants                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#             Arizona & Ontario State - Restaurant distribution based on the Star ratings          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
temp_az_star <- as.data.frame(table(az_df$stars))
temp_on_star <- as.data.frame(table(on_df$stars))

final_temp <- temp_az_star

final_temp['Freq1'] <- vlookup_df(final_temp$Var1, temp_on_star, result_column= 'Freq', lookup_column= 'Var1')

final_temp <- final_temp %>% rename(Arizona = Freq)

final_temp <- final_temp %>% rename(Ontario = Freq1)

final_temp <- melt(final_temp, id = 'Var1')

ggplot() + 
  geom_bar(data = final_temp, aes(x = Var1, y = value, fill = variable), position = "dodge", stat = "identity") +
  labs(x ="Restaurant Star ratings", y= "No. of Restaurants", title = "States- Arizona & Ontario", subtitle = "Restaurants star rating distribution" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_discrete(name= "States")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#           End of Arizona & Ontario State - Restaurant distribution based on the Star ratings     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                 Arizona State- Top 5 Restaurant Types                                            #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Arizona State - Top 5 Restaurant Types ###
az_df$Sub_category <- as.factor(az_df$Sub_category)

temp_az_res_type <- as.data.frame(table(az_df$Sub_category))

temp_az_res_type <- temp_az_res_type[order(-temp_az_res_type$Freq),] 

temp_az_res_type$Var1 <- as.factor(temp_az_res_type$Var1)

ggplot(data = top_n(temp_az_res_type,5) , aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = 'identity', fill="steelblue") + 
  labs(x ="Restaurant Type/Cuisine", y= "Count", title = "Arizona State", subtitle = "Top 5 Restaurant Types" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                End of Arizona State- Top 5 Restaurant Types                                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                 Arizona State- Top 5 Cities w.r.t. number of Restaurants                         #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

num_rest_az <- as.data.frame(table(az_df$city))

num_rest_az <- num_rest_az[order(-num_rest_az$Freq),]

ggplot(data = top_n(num_rest_az,5) , aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = 'identity', fill="tomato3") + 
  labs(x ="City Name", y= "No. of Restaurants", title = "Arizona State", subtitle = "Top 5 Cities w.r.t. number of Restaurants" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                 End of Arizona State- Top 5 Cities w.r.t. number of Restaurants                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                             Ontario State- Top 5 Restaurant Types                                #        #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
on_df$Sub_category <- as.factor(on_df$Sub_category)

temp_on_res_type <- as.data.frame(table(on_df$Sub_category))

temp_on_res_type <- temp_on_res_type[order(-temp_on_res_type$Freq),] 

temp_on_res_type$Var1 <- as.factor(temp_on_res_type$Var1)

ggplot(data = top_n(temp_on_res_type,5) , aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = 'identity', fill="steelblue") + 
  labs(x ="Restaurant Type/Cuisine", y= "Count", title = "Ontario State", subtitle = "Top 5 Restaurant Types" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                 End of Ontario State- Top 5 Cities w.r.t. number of Restaurants                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                   Ontario State- Top 5 Cities w.r.t. number of Restaurants                       #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Ontario State - Top 5 Cities w.r.t number of Restaurants ###
num_rest_on <- as.data.frame(table(on_df$city))

num_rest_on <- num_rest_on[order(-num_rest_on$Freq),]

ggplot(data = top_n(num_rest_on,5) , aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = 'identity', fill="tomato3") + 
  labs(x ="City Name", y= "No. of Restaurants", title = "Ontario State", subtitle = "Top 5 Cities w.r.t. number of Restaurants" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                 End of Ontario State- Top 5 Cities w.r.t. number of Restaurants                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                   Pie plot- Phoenix & Toronto City restaurants Price/Person                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
most_exp_ph <- az_df[az_df[,'city'] == 'Phoenix',]

temp_ph <- as.data.frame(table(most_exp_ph$Price_range))

temp_ph$Var1 <- ifelse(grepl('1', temp_ph$Var1, ignore.case = F), '1',
                       ifelse(grepl('2',temp_ph$Var1, ignore.case = F), '2',
                              ifelse(grepl('3',temp_ph$Var1, ignore.case = F), '2',
                                     ifelse(grepl('4',temp_ph$Var1, ignore.case = F), '4',
                                            'Not Available'))))

temp_ph <- temp_ph %>% group_by(Var1) %>% summarise(sum_price = sum(Freq))

temp_ph$Var1 <- ifelse(grepl('1', temp_ph$Var1, ignore.case = F), 'Under $10',
                       ifelse(grepl('2',temp_ph$Var1, ignore.case = F), '$11-60',
                              ifelse(grepl('4',temp_ph$Var1, ignore.case = F), 'Above $60',
                                     'Not Available')))

most_exp_tn <- on_df[on_df[,'city'] == 'Toronto',]

most_exp_tn$Price_range <- as.factor(most_exp_tn$Price_range)

temp_tn <- as.data.frame(table(most_exp_tn$Price_range))

temp_tn$Var1 <- ifelse(grepl('1', temp_tn$Var1, ignore.case = F), '1',
                       ifelse(grepl('2',temp_tn$Var1, ignore.case = F), '2',
                              ifelse(grepl('3',temp_tn$Var1, ignore.case = F), '2',
                                     ifelse(grepl('4',temp_tn$Var1, ignore.case = F), '4',
                                            'Not Available'))))

temp_tn <- temp_tn %>% group_by(Var1) %>% summarise(sum_price = sum(Freq))

temp_tn$Var1 <- ifelse(grepl('1', temp_tn$Var1, ignore.case = F), 'Under $10',
                       ifelse(grepl('2',temp_tn$Var1, ignore.case = F), '$11-60',
                              ifelse(grepl('4',temp_tn$Var1, ignore.case = F), 'Above $60',
                                     'Not Available')))

### Plotting the pie charts side-by-side ###
fig_pie <- plot_ly()

fig_pie <- fig_pie %>% add_pie(data = temp_ph, labels = ~Var1, values = ~sum_price, name = 'Phoenix \nRestaurant',
                         domain = list(row = 1, column = 0))

fig_pie <- fig_pie %>% add_pie(data = temp_tn, labels = ~Var1, values = ~sum_price, name = 'Toronto \nRestaurant',
                         domain = list(row = 1, column = 1))

fig_pie <- fig_pie %>% layout(title = "Phoenix & Toronto City - Restaurant Price/Person Distribution",
                              showlegend = T,
                              legend=list(title=list(text='<b> Price per Person </b>')),
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      annotations = list(
                        list(x = 0.2 , y = 0.85, text = "<b>Phoenix</b>", showarrow = F, xref='paper', yref='paper'),
                        list(x = 0.8 , y = 0.85, text = "<b>Toronto</b>", showarrow = F, xref='paper', yref='paper')
                      ))
fig_pie
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                 End of Pie plot- Phoenix & Toronto City restaurants Price/Person                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                              Box plot- AZ & ON with star ratings                                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
sum_az_on <- rbind(az_df,on_df)

sum_az_on <- sum_az_on[c(4,5,9)]

az_on_cities_stars <- sum_az_on %>% group_by(state,city) %>% summarise(avg_star = mean(stars))

bx_plt <- ggplot(az_on_cities_stars, aes(x = state, y = avg_star, fill= state)) +
  geom_boxplot(alpha = 0.7) + 
  labs(title = "Average Star Rating of Arizona & Ontario", x= "States", y= "Star Ratings")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11)) +
  scale_x_discrete(breaks=c("AZ","ON"), labels= c("Arizona", "Ontario")) +
  scale_fill_discrete(name= "State", breaks=c("AZ","ON"), labels= c("Arizona", "Ontario"))
  
bx_plt
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         End of Box plot- AZ & ON with star ratings                               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#             Circular Bar plots - Phonix & Toronto Restaurant Type/Cuisine Distrbution            #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Phoenix City - Restaurant Type/Cuisine Distribution ###
label_data_ph <- as.data.frame(table(most_exp_ph$Sub_category))

label_data_ph[label_data_ph==0] <- NA

label_data_ph <- na.omit(label_data_ph)

label_data_ph <- label_data_ph[with(label_data_ph,order(-Freq)),]

label_data_ph['id'] <- 1:nrow(label_data_ph)

number_of_bar <- nrow(label_data_ph)
angle <-  90 - 360 * (label_data_ph$id - 0.5) /number_of_bar

label_data_ph['hjust'] <- ifelse( angle < -90, 1, 0)

label_data_ph['angle'] <- ifelse(angle < -90, angle+180, angle)

plt_ph <- ggplot(data= label_data_ph , aes(x = reorder(Var1,-Freq) ,y = Freq)) +
  geom_bar(stat = 'identity', aes(fill= Freq)) +
  ylim(-500,3100) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar(start = 0) +
  scale_fill_gradientn(name = "Count", colors = rainbow(10), breaks= c(1,1000,2000,3000),limits=c(1,3100))+
  geom_text(data=label_data_ph, aes(x=id, y=Freq+20, label=Var1, hjust= hjust), 
            color="black", fontface="bold",alpha=1, size=3,
            angle= label_data_ph$angle, inherit.aes = FALSE )

grid.arrange(plt_ph, top="Phoenix City - Restaurant Type/Cuisine Distribution" , padding = unit(2,'line'))

### Toronto city Restaurant Type/Cuisine Distribution ###
label_data_tn <- as.data.frame(table(most_exp_tn$Sub_category))

label_data_tn <- label_data_tn[with(label_data_tn,order(-Freq)),]

label_data_tn['id'] <- 1:nrow(label_data_tn)

number_of_bar <- nrow(label_data_tn)
angle <-  90 - 360 * (label_data_tn$id - 0.5) /number_of_bar

label_data_tn['hjust'] <- ifelse( angle < -90, 1, 0)

label_data_tn['angle'] <- ifelse(angle < -90, angle+180, angle)

plt_tn <- ggplot(data= label_data_tn , aes(x = reorder(Var1,-Freq) ,y = Freq)) +
  geom_bar(stat = 'identity', aes(fill = Freq)) +
  ylim(-500,5000) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar(start = 0) +
  scale_fill_gradientn(name = "Count", colors = rainbow(10), breaks= c(4,1000,2000,3000,4000,5000) ,limits= c(4,5000))+
  geom_text(data=label_data_tn, aes(x=id, y=Freq+20, label=Var1, hjust= hjust), 
            color="black", fontface="bold", alpha=1, size=3,
            angle= label_data_tn$angle, inherit.aes = FALSE )

grid.arrange(plt_tn, top="Toronto City - Restaurant Type/Cuisine Distribution" , padding = unit(2,'line'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#         End of Circular Bar plot - Phonix & Toronto Restaurant Type/Cuisine Distrbution          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           Data wrangling for reviews from 'review.json'                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Reading the Reviews ###
review_newlines <- n.readLines("yelp_academic_dataset_review.json", n = 1000000,skip = 1000000, header = FALSE)

review_2_df <- stream_in(textConnection(review_newlines))

### Keep reviews only of restaurants ###
review_2_rest <- review_2_df[review_2_df$business_id %in% final_df$business_id,]


### Separate the reviews of restaurants of state AZ and ON, to avoid data leak ###
### This data will only be used for prediction on the reviews by the Shiny App ###

az_review <- review_2_rest[review_2_rest$business_id %in% az_df$business_id,]

on_review <- review_2_rest[review_2_rest$business_id %in% on_df$business_id,]



### final review data set to be used for selecting the reviews sub sample #####
final_review <- anti_join(review_2_rest, az_review, by= "review_id")
final_review <- anti_join(final_review, on_review, by = "review_id")


### Sampling for star rating 1,2,4,5 for training set ###
### Number of records for each star rating is set as 25000 and is randomly picked ###
sampled_reviews_1 <- final_review[final_review[,'stars'] == 1,] %>% group_by(stars) %>% sample_n(size = 25000)

sampled_reviews_2 <- final_review[final_review[,'stars'] == 2,] %>% group_by(stars) %>% sample_n(size = 25000)

sampled_reviews_4 <- final_review[final_review[,'stars'] == 4,] %>% group_by(stars) %>% sample_n(size = 25000)

sampled_reviews_5 <- final_review[final_review[,'stars'] == 5,] %>% group_by(stars) %>% sample_n(size = 25000)


sample_training_set <- rbind.data.frame(sampled_reviews_1,sampled_reviews_2,sampled_reviews_4,sampled_reviews_5)

### Concatenate the reviews of AZ & ON for pre-processing for the NLP process ###
sum_az_on_review <- rbind.data.frame(az_review, on_review)

### Concatenate the training set and reviews of AZ,ON for pre-processing ###
final_training_set <- rbind.data.frame(sample_training_set, sum_az_on_review )


### Reading the output of the model pre-processing for AZ and ON reviews ###
read_train_app <- read.csv("train_app_V1.2.csv")

read_train_app <- read_train_app[-c(1)]

### Remove reviews of star rating 3 as the model is not trained on it ###
az_review <- az_review[az_review[,'stars'] != 3,]

on_review <- on_review[on_review[,'stars'] != 3,]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          End of Data wrangling for reviews from 'review.json'                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           Data preparation to be used for the Shiny App                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Prepare the State Arizona review with additional details to be used by the Shiny App ###
az_fil_rev <- read_train_app[1:161913,]

az_fil_rev$business_id <- az_review$business_id

az_fil_rev["Name"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'name', lookup_column= 'business_id')

az_fil_rev$State <- 'Arizona'

az_fil_rev["City"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'city', lookup_column= 'business_id')

az_fil_rev["Address"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'address', lookup_column= 'business_id')

az_fil_rev["Latitude"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'latitude', lookup_column= 'business_id')

az_fil_rev["Longitude"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'longitude', lookup_column= 'business_id')

az_fil_rev["Business_Stars"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'stars', lookup_column= 'business_id')

az_fil_rev["Sub_category"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'Sub_category', lookup_column= 'business_id')

az_fil_rev["Price_range"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'Price_range', lookup_column= 'business_id')

az_fil_rev["Restaurant_takeout"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'Restaurant_takeout', lookup_column= 'business_id')

az_fil_rev["Restaurant_delivery"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'Restaurant_delivery', lookup_column= 'business_id')

az_fil_rev["Outdoor_seating"] <- vlookup_df(az_fil_rev$business_id, az_df, result_column= 'Outdoor_seating', lookup_column= 'business_id')

az_fil_rev$Review_rating <- az_review$stars

az_fil_rev$Review_text <- az_review$text


### Prepare the State Ontario review with additional details to be used by the Shiny App ###
on_fil_rev <- read_train_app[161914:231722,]

on_fil_rev$business_id <- on_review$business_id

on_fil_rev["Name"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'name', lookup_column= 'business_id')

on_fil_rev$State <- 'Ontario'

on_fil_rev["City"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'city', lookup_column= 'business_id')

on_fil_rev["Address"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'address', lookup_column= 'business_id')

on_fil_rev["Latitude"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'latitude', lookup_column= 'business_id')

on_fil_rev["Longitude"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'longitude', lookup_column= 'business_id')

on_fil_rev["Business_Stars"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'stars', lookup_column= 'business_id')

on_fil_rev["Sub_category"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'Sub_category', lookup_column= 'business_id')

on_fil_rev["Price_range"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'Price_range', lookup_column= 'business_id')

on_fil_rev["Restaurant_takeout"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'Restaurant_takeout', lookup_column= 'business_id')

on_fil_rev["Restaurant_delivery"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'Restaurant_delivery', lookup_column= 'business_id')

on_fil_rev["Outdoor_seating"] <- vlookup_df(on_fil_rev$business_id, on_df, result_column= 'Outdoor_seating', lookup_column= 'business_id')

on_fil_rev$Review_rating <- on_review$stars

on_fil_rev$Review_text <- on_review$text


### Final dataframe containing the details of AZ & ON states. ###
### This will used for the R-shiny application.  ###
final_train_app <- rbind.data.frame(az_fil_rev, on_fil_rev)

### Re-ordering the column for better interpretability. ###
final_train_app <- final_train_app[, c(27:41,1:26)]

### Writing the results to csv, to be read by the Shiny app ###
### This would help to better RAM management by removing unwanted variables ###
### and yet keeping the results intact ###
write.csv(final_train_app,"final_train_app_v7.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         End of Data preparation to be used for the Shiny App                     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#