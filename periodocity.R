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
fd <- fd[trackable_type %in% c("symptom", "Condition"),
         maxPain := maxMissing(trackable_value), by = user_id]

fd[, .N, by = user_id][order(N)]
fdSC <- fd[user_id=="QEVuQwEAlNMIH8RXhjZvx6HzoW8iXQ==" & trackable_name == "Ulcerative colitis"]

ggplot(fdSC, aes(checkin_date, trackable_value)) + 
  geom_line() + xlab("") + ylab("Trackable Value") + geom_point()

sapply(fdSC, class)

# add in missing dates and facet wrap by condition and maybe by month
fd <- fd[trackable_type %in% c("Symptom", "Condition")]
fd[, min:= as.POSIXct.Date(min(checkin_date, na.rm = T)), by = user_id]
fd[, max:= as.POSIXct.Date(max(checkin_date, na.rm = T)), by = user_id]
fd[, median := median(trackable_value, na.rm = T), by= user_id]
fd[, q1 := quantile(trackable_value, 0.25, na.rm = T), by = user_id]
fd[, q13:= quantile(trackable_value, 0.75, na.rm = T), by = user_id]

#distribution by user-- I want to figure out what constitutes as a flare per user


# make a data table of full dates
min <- min(fd$checkin_date, na.rm = T)
max <- max(fd$checkin_date, na.rm = T)
x <- seq.POSIXt(from = as.POSIXct(min), to= as.POSIXct(max),by = "1 day")
x<-data.table(x)

x <- fd[, .(dates = seq.POSIXt(from = min, to= max ,by = "1 day")), by = user_id]

setkey(dt, "id", "wday")
vals <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
idx <- expand.grid(vals, unique(dt$id))[, 2:1]
dt[J(idx), allow.cartesian=TRUE]

fd$checkin_date <- as.POSIXct.Date(fd$checkin_date)

fdDates <- fd[, .(user_id = user_id)]
fd[, date := seq.POSIXt(from=min_time, to=max_time,by = "1 day"), by = user_id]

fd[, date := seq.POSIXt(min(fd$checkin_date),max(fd$checkin_date)), by = user_id]
dates <- as.Date(as.POSIXct(dates), origin = as.Date(min_time))

# find min date and max date by user
function(x){
  fd[user_id==x, 
     date := seq.POSIXt(min(fd$checkin_date),max(fd$checkin_date))]
}

dfDates <- data.frame(date=seq.POSIXt(min(fd$checkin_date),max(fd$checkin_date),by="day"))
