library(rtweet)

tweets <- get_timeline(c( "Tesla", "VolvoTrucksUK", "elonmusk", "DaimlerTruckBus", "BMWi","BMW",
                          "nikolamotor", "AudiOfficial", "NissanUSA", "NissanElectric", "VolvoGroup", "MobilityDaimler",
                          "Toyota", "ToyotaMotorCorp", "MercedesBenz"), n = 2000)

b_tweets <- get_timeline("BMWi", n = 3250)

table(b_tweets$screen_name)

#jegyzet----
#szótövesítésel foglalkozni még (lemmatization)
#bigram --> tf-idf mutató
#hashtageknél kiszûrni a cégeket???


#hashtag alapján tweetek kellenek----
#battery
battery_tweets <- search_tweets(q = "#battery", n=2000 )#)
table(battery_tweets$is_retweet) 

#zeroemissions
zeroemissions_tweets <- search_tweets(q = "#zeroemissions", n=2000 )#)
table(zeroemissions_tweets$is_retweet)

#ev
ev_tweets <- search_tweets(q = "#ev", n=2000 )#)
table(ev_tweets$is_retweet)

#autonomous
autonom_tweets <- search_tweets(q = "#autonomous", n=2000)
table(autonom_tweets$is_retweet)


#hashtag alapján tweetek megnezni----

#5g
fiveg_tweets <- search_tweets(q = "#5g", n=2000)
table(fiveg_tweets$is_retweet)

#lithium
lithium_tweets <- search_tweets(q = "#lithium", n=2000 )#)
table(lithium_tweets$is_retweet)

#machinelearning
machinelearning_tweets <- search_tweets(q = "#machinelearning", n=2000 )#)
table(machinelearning_tweets$is_retweet)

#climatechange
climatechange_tweets <- search_tweets(q = "#climatechange", n=2000 )#)
table(climatechange_tweets$is_retweet)

#charging
charging_tweets <- search_tweets(q = "#charging", n=2000 )#)
table(charging_tweets$is_retweet)


#electricvehicles
electricvehicles_tweets <- search_tweets(q = "#electricvehicles", n=2000 )#)
table(electricvehicles_tweets$is_retweet)

#autonomousvehicles
autonomousvehicles_tweets <- search_tweets(q = "autonomousvehicles", n=2000 )#)
table(autonomousvehicles_tweets$is_retweet)

#100daysofcode
ai_tweets <- search_tweets(q = "#ai", n=2000 )#)
table(hundreddaysofcode_tweets$is_retweet)

#hashtag alapján tweetek nem kell----


