# Objective: Look into periodocity of flareups per user and then the distribution of periodocity 
# # Questions: when do you hit your highest point, for how long, and how frequently
# Date: 1/3/17

# Inital loading
fd <- read.csv("fd-export 3.csv", stringsAsFactors = F)
fd <- data.table(fd)

# find max pain by user  
fd$trackable_value <- as.numeric(fd$trackable_value)
fd$checkin_date <- as.Date(fd$checkin_date)
fd[, maxPain := NA_real_]
fd <- fd[trackable_type %in% c("symptom", "Condition"), maxPain := maxMissing(trackable_value), by = user_id]
fd[, .N, by = user_id][order(N)]
fdSC <- fd[user_id=="QEVuQwEAlNMIH8RXhjZvx6HzoW8iXQ==" & trackable_name == "Ulcerative colitis"]

ggplot(fdSC, aes(checkin_date, trackable_value)) + 
  geom_line() + xlab("") + ylab("Trackable Value")

sapply(fdSC, class)

# add in missing dates and facet wrap by condition and maybe by month
x <- zoo(fdSC$checkin_date)
x <- as.ts(x)
x <- na.interp(x)
print(x)
