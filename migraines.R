# Objective: Look into periodocity of flareups per user and then the distribution of periodocity 
# # Questions: when do you hit your highest point, for how long, and how frequently
# Date: 1/3/17

# periodicity
# weather
# what condiitons and symptoms is it tracked alongside
# demographics (male vs female)
# how long do they track it for?
# what causes/helps



# filter for migraines
migraines <- fd[grepl(x=trackable_name, pattern="migraine") & trackable_type %in% c("Symptom", "Condition")]

# find max pain by user  
migraines$trackable_value <- as.numeric(migraines$trackable_value)
migraines$checkin_date <- as.Date(migraines$checkin_date)

# Get the daily max by symptoms so we can collapse on date/user
migraines[, .N, by = user_id][order(N)]
migraines <- migraines[, .(dailyMax = max(trackable_value, na.rm = T)),
                       by = .(user_id, checkin_date, trackable_type)]

# get max pain of all time logged
migraines[, maxPain := maxMissing(trackable_value), 
          by = user_id]

# get total days tracked by user 
migraines[,daysLogged := uniqueN(checkin_date), 
          by = user_id]

# Test
migrainesTest <- migraines[user_id=="QEVuQwEAPl0naFuNbqRIIMQWovkObQ=="]

# Plots

ggplot(data = migrainesTest, mapping =  aes(as.Date(checkin_date[]),dailyMax)) + xlab("") + ylab("Trackable Value") + geom_point()
ggplot(migrainesTest,
       aes(x= checkin_date[month(checkin_date)==6],
           y=as.integer(dailyMax),
           color=user_id))+
  geom_point() +
  scale_x_date(labels = format("%Y-%m-%d"),
               breaks = "1 week")


# migraines with treatment ------------------------------------------------
## I only want the days where they took a treatment 
migrainesTreatment <- fd[trackable_type %in% c("Symptom", "Condition", "Treatment") & grepl(x=trackable_name, pattern="migraine")]
migrainesTreatmentDates <- migrainesTreatment[trackable_type == "Treatment", .(user_id, checkin_date)]
migrainesTreatment$trackable_value <- as.integer(migrainesTreatment$trackable_value)
migrainesTreatment[, averageValue := mean(trackable_value, na.rm = T), 
                   by = .(user_id, checkin_date)]

migrainesTreatment <- migrainesTreatment[grepl(x=trackable_name, pattern="migraine") & trackable_type %in% c("Symptom", "Condition"),
                         user_id,
                         by = .(checkin_date, averageValue)]
migrainesTreatment <- migrainesTreatment[checkin_date %in% migrainesTreatmentDates$checkin_date]
nt[migrainesTreatmentDates]




## get the list of days where they took a treatment
### it would be helpful to know/tag treatments with what condition/symptom they're targetting 
### (users may not know tho)
listOfTreatmeents <- migrainesTreatment[trackable_type %in% c("Treatment"), 
                                        unique(checkin_date),
                                        by = user_id]
migrainesTreatment[trackable_type %in% c("Treatment"), 
                   tookTreatment := 1,
                   by = .(user_id, checkin_date)]

# Futzing around ----------------------------------------------------------
# add in missing dates and facet wrap by condition and maybe by month
migraines <- migraines[trackable_type %in% c("Symptom", "Condition")]
migraines[, min:= as.POSIXct.Date(min(checkin_date, na.rm = T)), by = user_id]
migraines[, max:= as.POSIXct.Date(max(checkin_date, na.rm = T)), by = user_id]
migraines[, median := median(trackable_value, na.rm = T), by= user_id]
migraines[, q1 := quantile(trackable_value, 0.25, na.rm = T), by = user_id]
migraines[, q13:= quantile(trackable_value, 0.75, na.rm = T), by = user_id]

#distribution by user-- I want to figure out what constitutes as a flare per user


# make a data table of full dates
min <- min(migraines$checkin_date, na.rm = T)
max <- max(migraines$checkin_date, na.rm = T)
x <- seq.POSIXt(from = as.POSIXct(min), to= as.POSIXct(max),by = "1 day")
x<-data.table(x)

x <- migraines[, .(dates = seq.POSIXt(from = min, to= max ,by = "1 day")), by = user_id]

setkey(dt, "id", "wday")
vals <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
idx <- expand.grid(vals, unique(dt$id))[, 2:1]
dt[J(idx), allow.cartesian=TRUE]

migraines$checkin_date <- as.POSIXct.Date(migraines$checkin_date)

migrainesDates <- migraines[, .(user_id = user_id)]
migraines[, date := seq.POSIXt(from=min_time, to=max_time,by = "1 day"), by = user_id]

migraines[, date := seq.POSIXt(min(migraines$checkin_date),max(migraines$checkin_date)), by = user_id]
dates <- as.Date(as.POSIXct(dates), origin = as.Date(min_time))

# find min date and max date by user
function(x){
  migraines[user_id==x, 
            date := seq.POSIXt(min(migraines$checkin_date),max(migraines$checkin_date))]
}

dmigrainesates <- data.frame(date=seq.POSIXt(min(migraines$checkin_date),max(migraines$checkin_date),by="day"))
