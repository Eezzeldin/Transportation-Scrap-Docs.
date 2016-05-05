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
    condition1 = ALLZONES$Origin_Zone %in% NEARZONES$ZONE.ID.TAZ
    condition2 = ALLZONES$Destination_Zone %in% NEARZONES$ZONE.ID.TAZ 
    ALLZONES <<- ALLZONES [condition1 * condition2 ,]
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
  removenonactivezones_ALLZONES()
  
  remove_RedundantVariables_METRO()
  make_categorial_variables_asnumeric_METRO()
  
  remove_RedundantVariables_NEARZONES ()
  make_categorial_variables_asnumeric_NEARZONES ()
  calculate_distanceinmiles_NEARZONES()
  
}
construsctdata = function (){
  
}




####EXECUTION
getdata()
modifydata ()



###TESTING
getdata() : Works
modifydata ()
  -  get_residentsonly() : 
        + Works? Yes
        + Why? Original Dataset was bigger
  -  remove_start_end_dates()
        +works ? yes
        +Evidence? AllZONES reduced to 7 instead of 9 variables