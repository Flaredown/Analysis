# Objective: Find baseline correlative relationship between logged instances of pred uptake and abdominal pain
# Date: 12/27/17

# Inital loading
fd <- read.csv("fd-export 3.csv", stringsAsFactors = F)
fd <- data.table(fd)


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

# Get a simple correlation
corr <- corr[trackable_type %in% c("Symptom", "Condition")]
corr$trackable_value <- as.numeric(corr$trackable_value)
simpleCor <- cor(x = corr$usedPred, y= corr$trackable_value)
model.lm <- lm(trackable_value ~ usedPred, data = corr)
summary(model.lm)

# Unnecessary and unhelpful plot
ggplot(corr, aes(x=usedPred, y=trackable_value)) + 
  geom_point()

# Scatter Plot Matrix
scatterplot_matrix_full <- pairs(~trackable_value + usedPred + age ,
                                 data=corr, 
                                 main="Scatterplot Matrix of FD Data (Full Set)")

bound <- floor((nrow(corr)/4)*3)         
corr <- corr[sample(nrow(corr)), ]          
train <- corr[1:bound, ]             
test <- corr[(bound+1):nrow(corr), ]   

# Run linear regression on training set
full_reg <-lm(trackable_value~usedPred,data=train)
summary(full_reg)
