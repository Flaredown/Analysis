# Objective: Find baseline correlative relationship between logged instances of pred uptake and abdominal pain
# Date: 12/27/17

# Get just the users who log abdominal pain and, of those, who uses pred
abdominalPainUsers <- fd[grepl(x = trackable_name, pattern = "abdominal pain",ignore.case = T),user_id]
abdominal <- fd[user_id%in%abdominalPainUsers]
abdominal[, uniqueN(user_id)]
predUsers <- abdominal[grepl(x=trackable_name, pattern = "prednisone", ignore.case = T), user_id]
pred <- abdominal[user_id%in%predUsers]

# Get all the days where there was pred usage
predDays <- abdominal[grepl(x=trackable_name, pattern = "prednisone", ignore.case = T), checkin_date]
corr <- abdominal[(user_id%in%predUsers)]
corr[, usedPred := 0]
corr[(checkin_date%in%predDays & user_id%in%predUsers), usedPred := 1]

# Get a simple correlation/model
corr <- corr[trackable_type %in% c("Symptom", "Condition")]
corr$trackable_value <- as.numeric(corr$trackable_value)
simpleCor <- cor(x = corr$usedPred, y= corr$trackable_value)
model.lm <- lm(trackable_value ~ usedPred, data = corr)
summary(model.lm)

# Reshape data to get days since Pred
corr$checkin_date <- as.Date(corr$checkin_date)
corr <- corr[,][order(user_id, checkin_date)] 
corr <- unique(corr[, .(checkin_date, user_id, usedPred)])
corr[usedPred == 1, date := checkin_date]
corr[!is.na(date), diff := date - shift(date), by = user_id]


setkey(corr, user_id, checkin_date)
abdominal$checkin_date <- as.Date(abdominal$checkin_date)
setkey(abdominal, user_id, checkin_date)
full <- corr[abdominal]
full$diff <- as.integer(full$diff)
fullNoNA <- full[!is.na(diff) & trackable_type %in% c("Symtpom", "Condition")]
fullNoNA$trackable_value <- as.integer(fullNoNA$trackable_value)
fullNoNA <- fullNoNA[user_id %in% predUsers]
cor(x = fullNoNA$diff, fullNoNA$trackable_value)
model.lm <- lm(trackable_value ~ diff, data = fullNoNA)
summary(model.lm)

# curious to bring weather back on 
# weather <- fd[, .(user_id, checkin_date, trackable_type = trackable_type[trackable_type == "Weather"])]
# 
# setkey(fd, user_id, checkin_date, trackable_type)
# setkey(weather, user_id, checkin_date, trackable_type)
# x <- weather[fd]

# look at users who are not loggina t all or if they really are not taking pred for that long
