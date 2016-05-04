###BUILDING UNITS
getdata = function (){
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
  
  ##---Execution
  Keep_residentsonly_ALLZONES ()
  remove_RedundantVariables_ALLZONES()
  make_categorial_variables_asnumeric_ALLZONES()
  
  remove_RedundantVariables_METRO()
  make_categorial_variables_asnumeric_METRO()
  
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