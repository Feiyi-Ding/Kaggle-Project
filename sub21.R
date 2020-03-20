# Read and clean the data
data0 = read.csv('analysisData.csv')
data = data0[!(data0$price == 0),]

library(Hmisc)
data$beds = impute(data$beds, -1)
data$security_deposit = impute(data$security_deposit, -1)
data$cleaning_fee = impute(data$cleaning_fee, -1)
data$host_listings_count = impute(data$host_listings_count, -1)
data$zipcode = as.numeric(data$zipcode)

library(randomForest)
set.seed(617)
forest = randomForest(price~accommodates+bathrooms+bedrooms+beds+security_deposit+ cleaning_fee+guests_included+
                            extra_people+review_scores_location+calculated_host_listings_count_private_rooms+
                            availability_365+review_scores_rating+review_scores_cleanliness+calculated_host_listings_count_entire_homes+
                            availability_30+availability_60+availability_90+host_listings_count+
                            zipcode+room_type+bed_type+cancellation_policy+property_type+neighbourhood_group_cleansed+
                            host_is_superhost+is_location_exact+instant_bookable,
                           data=data,ntree=1000)

# Predictions
scoringData = read.csv('scoringData.csv')
scoringData$beds = impute(scoringData$beds, -1)
scoringData$security_deposit = impute(scoringData$security_deposit, -1)
scoringData$cleaning_fee = impute(scoringData$cleaning_fee, -1)
scoringData$host_listings_count = impute(scoringData$host_listings_count, -1)
scoringData$zipcode = as.numeric(scoringData$zipcode)
scoringData$property_type[scoringData$property_type == "Casa particular (Cuba)"] <- "Other"
scoringData$property_type[scoringData$property_type == "Castle"] <- "Other"
scoringData$property_type[scoringData$property_type == "Farm stay"] <- "Other"
scoringData$property_type = droplevels(scoringData$property_type)

dataForPred = data[,-47]
scoringData <- rbind(dataForPred[1, ] , scoringData)
scoringData <- scoringData[-1,]

pred = predict(forest,newdata=scoringData)

# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'submission21.csv',row.names = F)
