library('jsonlite')
library('tibble')
library('stringr')
library('RJSONIO')
library('dplyr')
library('tidyverse')
library('reader')
library('expss')


### Setting the system to UTF-8 encoding so that characters like é be displayed correctly.
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") 

### Reading the business json file ####
business <- stream_in(file('yelp_academic_dataset_business.json'))

#business_flat <- flatten(business)
business_df <- as_data_frame(flatten(business))

restaurant_df <- business_df %>% filter(str_detect(categories, "Restaurants"))

colSums(is.na(restaurant_df))

### Dropping of columns where the ratio of NA values is more than 80% of the total data for that column####
rest_df <- restaurant_df[, which(colMeans(!is.na(restaurant_df)) > 0.80)]
colSums(is.na(rest_df))

### Further dropping uncessary columns ###
rest_df <- rest_df[-c(11,13,14,17,18)]

colnames(rest_df)

### Renaming of the columns for better readbility ###
rest_df <- rest_df %>% rename(Price_range = attributes.RestaurantsPriceRange2)
rest_df <- rest_df %>% rename(Restaurant_takeout = attributes.RestaurantsTakeOut)
rest_df <- rest_df %>% rename(Restaurant_delivery = attributes.RestaurantsDelivery)
rest_df <- rest_df %>% rename(Outdoor_seating = attributes.OutdoorSeating)


### Change the values for better readibility at the output ####
# i.e. True -> 'Yes', False -> 'No', NA or None -> 'Not Available' #

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



#nrow(rest_df)
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


### Lets read the reviews ###
review_newlines <- n.readLines("yelp_academic_dataset_review.json", n = 1000000,skip = 1000000, header = FALSE)

review_2_df <- stream_in(textConnection(review_newlines))

### Keep reviews only of restaurants ###
review_2_rest <- review_2_df[review_2_df$business_id %in% final_df$business_id,]


### Segregate the restaurants of the state AZ and ON ###

az_df <- final_df[final_df[,'state'] == 'AZ',]

on_df <- final_df[final_df[,'state'] == 'ON',]


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




### Cleanup the city names  for state AZ ###
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


az_city_list <- as.data.frame(table(az_df$city))

az_df$city <- str_to_title(az_df$city)



### Cleanup the city names  for state ON ###

on_df$city <- tolower(on_df$city)
on_city_list <- as.data.frame(table(on_df$city))

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


### Reading the output of the model pre-processing of the AZ and ON reviews ###
read_train_app <- read.csv("train_app_V1.2.csv")

read_train_app <- read_train_app[-c(1)]

### Remove reviews of star rating 3 as the model is not trained on it ###
az_review <- az_review[az_review[,'stars'] != 3,]

on_review <- on_review[on_review[,'stars'] != 3,]


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

### Re-ordering the column for better inter-pretability. ###
final_train_app <- final_train_app[, c(27:41,1:26)]

write.csv(final_train_app,"final_train_app_v7.csv", row.names = FALSE)
