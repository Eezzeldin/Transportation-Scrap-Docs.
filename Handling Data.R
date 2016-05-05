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
  ALLZONES <<- read.csv("ALLZONES.csv")
  METRO <<- read.csv("METRO.csv")
  NEARZONES <<- read.csv( "NEARZONES.csv")
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
  ALLZONES <<- removecategorial_ALLZONES  (2,1) 
  METRO    <<- removecategorial_METRO (1,1)
}
modifydata_1 = function (){
  ##---BUILDING UNITS
  #ALLZONES
  removecategorial_ALLZONES = function (Purpose,Timeofday) {
    indexes = c()
    indexes = which (ALLZONES$Purpose==Purpose & ALLZONES$Time_of_Day ==Timeofday,arr.ind =TRUE)
    ALLZONES <-ALLZONES[indexes  ,]
    return (ALLZONES)
  }
  
  #METRO
  removecategorial_METRO = function (DATE,TimePeriod) {
    METRO <- METRO[ which (METRO$Ent.Date.Service.Type==DATE & METRO$Ent.Time.Period ==TimePeriod ,arr.ind =TRUE),]
    return (METRO)
  }
  
  #NEARZONES
  
  
  ##---EXECUTION
  ALLZONES <<- removecategorial_ALLZONES(1,1)#(Purpose: 1:NHB , 2:HBW ,3:HBO,TimeofDay:1:H16:H20 2:H6:H10)
  METRO    <<-  removecategorial_METRO(1,1) #(Week:1:Weekday , 2:Sunday , 3:Saterday  , Time: 1:AMPeak , 2:Evening , 3:Midday , 4: PM Peak)
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

  get_count = function (x){
    origin = O_D[x,1]
    Destination = O_D [x,2]
    conditoin1 = ALLZONES$Origin_Zone == O_D$Origin[x] 
    conditoin2 = ALLZONES$Destination_Zone == O_D$Destination [x]
    condition3 = conditoin1 & conditoin2
    zonescounts = c()
    zonecounts = ALLZONES$Count [condition3]
    Purpose = ALLZONES$Purpose
    Time = ALLZONES$Time_of_Day
    output = cbind (origin,Destination,zonecounts,Purpose,Time)
    return (output)
  }
  
  
  ##--------EXECUTION
  add200predictors_closestzones_everystation()
  O_D <<- data.frame (get_closest200Zones_1st_record_OriginStation(),get_closest200Zones_1stDestinationStation()) #1st_origin_Destination_dataframe
  colnames (O_D) <<- c ("Origin","Destination")
 # for (i in c(1:200)) {print (get_count(i)); print (i)}
    
}




####EXECUTION
#getdata()
#modifydata ()
#writemodifieddata()
#
remove (getdata,writemodifieddata, modifydata)
#
getmodifiedata ()
modifydata_1()
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

> sapply (c(1:200),function(x) get_count(x))
{
[[1]]
origin Destination
[1,]    755        1908

[[2]]
origin Destination
[1,]    748        1912

[[3]]
origin Destination
[1,]    746        1911

[[4]]
origin Destination zonecounts
[1,]    713        1909       1.62
[2,]    713        1909       1.08

[[5]]
origin Destination
[1,]    718        1913

[[6]]
origin Destination zonecounts
[1,]    751        1905       2.80
[2,]    751        1905       0.77

[[7]]
origin Destination zonecounts
[1,]    747        1848       3.64

[[8]]
origin Destination zonecounts
[1,]    749        1930       1.69

[[9]]
origin Destination zonecounts
[1,]    719        1914       2.57

[[10]]
origin Destination
[1,]    803        1907

[[11]]
origin Destination zonecounts
[1,]    404        1915       2.12

[[12]]
origin Destination zonecounts
[1,]    750        1910       0.89
[2,]    750        1910       1.30

[[13]]
origin Destination zonecounts
[1,]    714        1906       0.47

[[14]]
origin Destination
[1,]    715        1847

[[15]]
origin Destination zonecounts
[1,]    661         469       1.68

[[16]]
origin Destination zonecounts
[1,]   1885        1964       2.19

[[17]]
origin Destination zonecounts
[1,]    753        1904       0.76

[[18]]
origin Destination zonecounts
[1,]    763         473       0.82

[[19]]
origin Destination zonecounts
[1,]    866         467       1.40
[2,]    866         467       6.42

[[20]]
origin Destination
[1,]    401        1953

[[21]]
origin Destination
[1,]    397        1931

[[22]]
origin Destination zonecounts
[1,]   1901         472       0.76
[2,]   1901         472       1.62
[3,]   1901         472       1.09

[[23]]
origin Destination zonecounts
[1,]    548        1939      30.76
[2,]    548        1939       1.34

[[24]]
origin Destination
[1,]   1884         474

[[25]]
origin Destination
[1,]   1900        1929

[[26]]
origin Destination zonecounts
[1,]    422        1902       1.33

[[27]]
origin Destination
[1,]    553        1960

[[28]]
origin Destination zonecounts
[1,]    423        1957       3.28

[[29]]
origin Destination
[1,]    802        1954

[[30]]
origin Destination
[1,]   1886        1926

[[31]]
origin Destination zonecounts
[1,]    754         470       2.38

[[32]]
origin Destination
[1,]    549        1927

[[33]]
origin Destination
[1,]    628        1923

[[34]]
origin Destination
[1,]   1894         471

[[35]]
origin Destination zonecounts
[1,]    406        1844      67.40
[2,]    406        1844       0.73
[3,]    406        1844       5.51
[4,]    406        1844       0.33
[5,]    406        1844       6.37

[[36]]
origin Destination
[1,]    552        1965

[[37]]
origin Destination zonecounts
[1,]    396        1928       1.20
[2,]    396        1928       0.74

[[38]]
origin Destination zonecounts
[1,]    828        1846       2.44
[2,]    828        1846       1.07

[[39]]
origin Destination zonecounts
[1,]   1883        1887       1.34
[2,]   1883        1887       6.84

[[40]]
origin Destination zonecounts
[1,]    629         468       6.17
[2,]    629         468       8.26
[3,]    629         468       1.85

[[41]]
origin Destination
[1,]    421        1952

[[42]]
origin Destination zonecounts
[1,]   1899        1918        0.7

[[43]]
origin Destination zonecounts
[1,]    830        1903       0.73
[2,]    830        1903       1.33
[3,]    830        1903       2.52
[4,]    830        1903       1.26
[5,]    830        1903       2.29

[[44]]
origin Destination
[1,]    764        1963

[[45]]
origin Destination zonecounts
[1,]    706        1919       2.43

[[46]]
origin Destination zonecounts
[1,]    752        1898       2.70
[2,]    752        1898       4.42

[[47]]
origin Destination zonecounts
[1,]    845         420       1.88

[[48]]
origin Destination
[1,]    569        1961

[[49]]
origin Destination zonecounts
[1,]    632        1955       1.04

[[50]]
origin Destination zonecounts
[1,]    514        1799       2.29
[2,]    514        1799       6.63
[3,]    514        1799      61.87

[[51]]
origin Destination zonecounts
[1,]    756        1857       1.53

[[52]]
origin Destination zonecounts
[1,]    831        1845       4.89
[2,]    831        1845       1.28
[3,]    831        1845       0.39

[[53]]
origin Destination
[1,]   1958         511

[[54]]
origin Destination zonecounts
[1,]   1903        1858       2.25

[[55]]
origin Destination zonecounts
[1,]    425         409       2.68

[[56]]
origin Destination zonecounts
[1,]   1959        1962       1.12

[[57]]
origin Destination
[1,]    407        1800

[[58]]
origin Destination zonecounts
[1,]    829        1920       1.03

[[59]]
origin Destination zonecounts
[1,]    662         632       3.24
[2,]    662         632       1.36

[[60]]
origin Destination
[1,]   1898         705

[[61]]
origin Destination
[1,]    550        1947

[[62]]
origin Destination zonecounts
[1,]    712         421       0.83

[[63]]
origin Destination zonecounts
[1,]    405        1916       1.83

[[64]]
origin Destination
[1,]    798         512

[[65]]
origin Destination
[1,]    761        1888

[[66]]
origin Destination zonecounts
[1,]    470        1956       0.55
[2,]    470        1956       2.87

[[67]]
origin Destination zonecounts
[1,]    516        1843       1.21

[[68]]
origin Destination zonecounts
[1,]   1902        1959       2.79
[2,]   1902        1959       0.77

[[69]]
origin Destination
[1,]    551         510

[[70]]
origin Destination zonecounts
[1,]   1957        1801       0.41
[2,]   1957        1801       7.82
[3,]   1957        1801       2.15

[[71]]
origin Destination zonecounts
[1,]    515        1889       0.42
[2,]    515        1889       2.13

[[72]]
origin Destination zonecounts
[1,]    833        1946       2.95
[2,]    833        1946       2.50
[3,]    833        1946       0.82
[4,]    833        1946       2.28

[[73]]
origin Destination
[1,]    860        1883

[[74]]
origin Destination zonecounts
[1,]   1887         761       1.11
[2,]   1887         761       3.05

[[75]]
origin Destination zonecounts
[1,]    832         408       0.76

[[76]]
origin Destination
[1,]    827        1802

[[77]]
origin Destination
[1,]    826        1851

[[78]]
origin Destination zonecounts
[1,]    400         706       4.11
[2,]    400         706       9.12
[3,]    400         706      12.02

[[79]]
origin Destination
[1,]    420        1942

[[80]]
origin Destination
[1,]   1962        1899

[[81]]
origin Destination
[1,]    716        1966

[[82]]
origin Destination
[1,]    547         476

[[83]]
origin Destination zonecounts
[1,]   1888        1886       4.47

[[84]]
origin Destination zonecounts
[1,]    660        1890       1.51
}

final_output =data.frame ( rbind(lapply (c(1:200),function(x) get_count(x))))
{}
which (ALLZONES$Purpose==1 & ALLZONES$Time_of_Day ==2 ,arr.ind =TRUE),]