###BUILDING UNITS
getdata = function (){
#ALLZONES is the original AirSage Data
#METRO is the original WMATA data
#NEARZONES is the data set produced from ArcMaps ERcs application software.
  #NEARZONES containts the nearest 200 zones to each of the 86 metro stations in metropolean area within a range of 100 miles.
ALLZONES <<- read.csv("long_0514cusWDDP.csv")
METRO <<- read.csv("METRODATA.csv")
NEARZONES <<- read.csv ("Nearest 200 Zones_100 _86 Stations.csv")
}
writemodifieddata = function (){
write.csv(ALLZONES,"ALLZONES.csv")
write.csv(METRO,"METRO.csv")
write.csv(NEARZONES,"NEARZONES.csv")}
getmodifiedata = function (){
  ALLZONES = read.csv("ALLZONES.csv")
  METRO = read.csv("METRO.csv")
  NEARZONES = read.csv( "NEARZONES,csv")
}
modifydata = function (){
  ##---ALLZONES Units
  Keep_residentsonly_ALLZONES = function ()   {
    condition = ALLZONES$Subscriber_Class == "Resident"
    ALLZONES <<- ALLZONES [condition==TRUE,] 
  }
  remove_RedundantVariables_ALLZONES = function () {
    ALLZONES$Start_Date  <<- NULL
    ALLZONES$End_Date    <<- NULL
    ALLZONES$Aggregation <<- NULL
    ALLZONES$Subscriber_Class <<-NULL
  }
  make_categorial_variables_asnumeric_ALLZONES = function () {
    ALLZONES$Time_of_Day <<- as.numeric(factor(ALLZONES$Time_of_Day))
    ALLZONES$Purpose     <<- as.numeric(factor(ALLZONES$Purpose)) 
  }
  removenonactivezones_ALLZONES = function (){
    Condition1 = ALLZONES$Origin_Zone %in% NEARZONES$ZONE.ID.TAZ 
    Condition2 = ALLZONES$Destination_Zone %in% NEARZONES$ZONE.ID.TAZ
    Condition3 = Condition1 & Condition2
    ALLZONES <<- ALLZONES [Condition3,]
  }
  makeallzonesnamessameasmetro_ALLZONES = function (StationName) {
     #condition = grepl( METRO [StationNumber, "Ent.Station"],NEARZONES$MetroStnFullPt.NAME)
     condition = grepl( substr(StationName, start=2, stop=7) ,NEARZONES$MetroStnFullPt.NAME)
     TargetedRecords =  NEARZONES [condition,]
     TargetedRecords$MetroStnFullPt.NAME =  sapply (TargetedRecords$MetroStnFullPt.NAME , as.character)
     #TargetedRecords$MetroStnFullPt.NAME  = paste(  METRO [StationNumber, "Ent.Station"])
     #TargetedRecords$MetroStnFullPt.NAME  = paste(StationName)
     NEARZONES [which (grepl( substr(StationName, start=2, stop=7)  ,NEARZONES$MetroStnFullPt.NAME),arr.ind = T),3]  <<- paste(StationName)
     print (NEARZONES [which (grepl( substr(StationName, start=2, stop=7)  ,NEARZONES$MetroStnFullPt.NAME),arr.ind = T),3])
     return (TargetedRecords$MetroStnFullPt.NAME)
  }
  
  
  ##---METRO Units
  remove_RedundantVariables_METRO = function() {
    #Remove the year and month because I know the analysis runs in may and in 2014.
    METRO$Year.Month <<- NULL 
    METRO$Ent.Date.Holiday <<-NULL #Because the column next it already has this information
  }
  make_categorial_variables_asnumeric_METRO = function () {
    METRO$Ent.Date.Service.Type <<- as.numeric(factor(METRO$Ent.Date.Service.Type))
    METRO$Ent.Time.Period <<- as.numeric(factor(METRO$Ent.Time.Period))
  }
  
  ##---NEARZONES
  remove_RedundantVariables_NEARZONES = function (){
    NEARZONES$METRO.STATION.ID <<-NULL
    NEARZONES$NEAR.ZONES.ID    <<-NULL
    NEARZONES$STATION.LATITUDE <<-NULL
    NEARZONES$STATION.LOGITUDE <<-NULL
    NEARZONES$ZONE.LATITUDE    <<-NULL
    NEARZONES$ZONE.LONGITUDE   <<-NULL
    NEARZONES$MetroStnFullPt.GIS_ID <<-NULL
    NEARZONES$MetroStnFullPt.WEB_URL <<-NULL
    NEARZONES$MetroStnFullPt.ADDRESS <<-NULL
  }
  make_categorial_variables_asnumeric_NEARZONES = function () {
    NEARZONES$MetroStnFullPt.LINE <<- as.numeric(factor(NEARZONES$MetroStnFullPt.LINE))
  }
  calculate_distanceinmiles_NEARZONES = function (){
    #The current distance is in meters. For better comprehension , it was transformed in to miles.
    #1 meter = 0.000621371 miles
    NEARZONES$DistanceMiles <<- (NEARZONES$DISTANCE.TO.ZONE..DECIMALS.) * 0.000621371
    NEARZONES$DISTANCE.TO.ZONE..DECIMALS. <<- NULL
  }
    
  
  ##---Execution
  Keep_residentsonly_ALLZONES ()
  remove_RedundantVariables_ALLZONES()
  make_categorial_variables_asnumeric_ALLZONES()
  
  
  remove_RedundantVariables_METRO()
  make_categorial_variables_asnumeric_METRO()
  
  remove_RedundantVariables_NEARZONES ()
  make_categorial_variables_asnumeric_NEARZONES ()
  calculate_distanceinmiles_NEARZONES()
  
  removenonactivezones_ALLZONES()
  #makeallzonesnamessameasmetro_ALLZONES()
  # vector1 = METRO$Ent.Station
  # vector1 = sapply (vector1,as.character)
  # vector1 = unique(vector1)
  # vector1  = vector1 [-c(56,52,75)]
  # lapply (vector1 , function (x)makeallzonesnamessameasmetro_ALLZONES(x) )
  # 
}
construct_Predictors = function (){
  #First 2 columns are origin and destination stations
  Predictors <<- data.frame (METRO$Ent.Station,METRO$Ext.Station)
  
  ##-------CONSTRUCTING UNITS
  add200predictors_closestzones_everystation = function (){
    for (i in 1:200)
      {
      Predictors[1,paste ("Closesness Rank" ,i)] <<-  NA
      }
  }
  #METRO - NEARZONES : Get the nearest 200 zones from NEARZONES for the origin station in METRO
  get_closest200Zones_1st_record_OriginStation = function (){
    closest200originstations = c()
    closest200originstations = NEARZONES [ grepl( METRO [1, "Ent.Station"],NEARZONES$MetroStnFullPt.NAME), 2]
    return (closest200originstations)
  } 
  get_closest200Zones_1stDestinationStation = function (){
    closest200Destinationstations = c()
    closest200Destinationstations = NEARZONES [ grepl( METRO [2, "Ext.Station"],NEARZONES$MetroStnFullPt.NAME), 2]
    return (closest200Destinationstations)
  }
  
  merging_METROwithNEARZONES = function () {
    
  }
  
  
  get_count = function (x){
    origin = O_D[x,1]
    Destination = O_D [x,2]
    conditoin1 = ALLZONES$Origin_Zone == O_D$Origin[x] 
    conditoin2 = ALLZONES$Destination_Zone == O_D$Destination [x]
    condition3 = conditoin1 & conditoin2
    zonescounts = c()
    zonecounts = ALLZONES$Count [condition3]
    output = cbind (origin,Destination,zonecounts)
    return (output)
  }
  ##--------EXECUTION
  add200predictors_closestzones_everystation()
  O_D <<- data.frame (get_closest200Zones_1st_record_OriginStation(),get_closest200Zones_1stDestinationStation()) #1st_origin_Destination_dataframe
  colnames (O_D) <<- c ("Origin","Destination")
 # for (i in c(1:200)) {print (get_count(i)); print (i)}
    
}




####EXECUTION
getdata()
modifydata ()
writemodifieddata()
getmodifiedata ()
construct_Predictors ()


###TESTING
> NEARZONES$ZONE.ID.TAZ [METRO$Ent.Station [1]]
[1] 1750


> head (NEARZONES [NEARZONES$MetroStnFullPt.NAME== "Van Dorn Street",])
CLOSEST.ZONES_RANK ZONE.ID.TAZ MetroStnFullPt.NAME MetroStnFullPt.LINE
1                  1        1750     Van Dorn Street                   1
2                  2        1480     Van Dorn Street                   1
3                  3        1748     Van Dorn Street                   1
4                  4        1488     Van Dorn Street                   1
5                  5        1751     Van Dorn Street                   1
6                  6        1749     Van Dorn Street                   1
DistanceMiles
1     0.3743194
2     0.5177304
3     0.5956201
4     0.7118358
5     0.8018161
6     0.9321130



> head (NEARZONES [NEARZONES$MetroStnFullPt.NAME== "Van Dorn Street", 2])
[1] 1750 1480 1748 1488 1751 1749



> grepl( "Addison Road","Addison Road Seat Pleasant")
[1] TRUE

grepl( METRO [1, "Ent.Station"],NEARZONES$MetroStnFullPt.NAME)


substr("Archives-Navy Memorial", start=1, stop=10) 

output = data.frame()
output = data.frame (lapply (myvector , function (x)makeallzonesnamessameasmetro_ALLZONES(x) ))
lapply (myvector , function (x)makeallzonesnamessameasmetro_ALLZONES(x) )
lapply (vector1 , function (x)makeallzonesnamessameasmetro_ALLZONES(x) )

        which (grepl( "Archives-N"  ,NEARZONES$MetroStnFullPt.NAME),arr.ind = T)
        
        substr("Arlington Cemetery" , start=1, stop=10)     
        
        myvector = myvector[-56] $"New York Ave"      
        myvector = myvector[-75] #"U Street-Cardozo" 
        myvector = myvector[-52]#"Mt. Vernon Square-UDC" 
        vector1  = vector1 [-c(56,52,75)]
table (ALLZONES$MetroStnFullPt.NAME)

