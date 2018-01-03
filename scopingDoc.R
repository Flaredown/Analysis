# Objective: Scope out initial analysis 
# Date: 12/27/2017

# HIGH LEVEL --------------------------------------------------------------
# Find correlations between a treatment and a symptom in an automated way thay we can show back to people (ie pred has a 50% impact on your abdominal pain-- there are a lot of considerations). Let's not look at an individual user but across all users. 

# METHODOLOGY -------------------------------------------------------------
# - look at all the days that an inidvidual was on a treatment and day by day the effects and compare to same period of time that they were not on that treatment (this includes "the off days"-- just because someone doesn't take a treatment everyday doesn't mean they are not on a treatment during that time esp if the treamtment is weekly/monthly)
# - check for correlation between symptom and treatment via period vs the correlation between dates

# ACTIONABLE --------------------------------------------------------------
# - Hot topics: effects of stress, sleep, marijuana, weather, day of the week vs symptoms 
# - Just looking at severity of symptoms (trackable_value (1:4 for symptoms/conditions))
# - Jumping off point: correlation between predisone and abdominal pain

# QUESTIONS ---------------------------------------------------------------
# - Lag time of effect: if you take a treatment on a certain day, when are you seeing an effect. 
# - Compare two individuals who who have similar conditions/symptoms and one who is on the treatment and one is not
# - Does a treatment have an immediate effect or not?
  

# GOING FORWARD -----------------------------------------------------------
# - input the CDAI (metric of disease severity)
# Periodicity: looking at intervals of flareups per and user and the distribution of periodocity amongst users with a condition


