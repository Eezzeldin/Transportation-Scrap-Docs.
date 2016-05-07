{#Purpose : HBW TIME OF DAY :Morning
#(Week:1:Weekday , 2:Sunday , 3:Saterday  , Time: 1:AMPeak , 2:Evening , 3:Midday , 4: PM Peak)
#(Purpose: 1:HBW , 2:HBO ,3:NHB,TimeofDay:1:H16:H20 2:H6:H10)
#936 Zones
}

###BUILDING UNITS
makeitfaster = function (){
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
foreach(i=1:3) %dopar% sqrt(i)
}
getmodifiedata = function (){
  ALLZONES <<- read.csv("ALLZONES.csv")
  METRO    <<- read.csv("METRO.csv")
  NEARZONES <<- read.csv( "NEARZONES_NAMESCHANGED.csv")
}
modifydata_1 = function (){
  ##------BUILDING UNITS
  #---ALLZONES
  removecategorial_ALLZONES = function (Purpose,Timeofday) {
    indexes = c()
    indexes = which (ALLZONES$Purpose==Purpose & ALLZONES$Time_of_Day ==Timeofday,arr.ind =TRUE)
    ALLZONES <<-ALLZONES[indexes  ,]
  }
  checkforduplicated_ALLZONES = function (){
    print ('Number of duplicate O-D record in ALLZONES dataset')
    return (which (duplicated (data.frame(ALLZONES$Origin_Zone, ALLZONES$Destination_Zone)) ,arr.ind = TRUE))
  }
  
  #---METRO
  removecategorial_METRO = function (DATE,TimePeriod) {
    METRO <- METRO[ which (METRO$Ent.Date.Service.Type==DATE & METRO$Ent.Time.Period ==TimePeriod ,arr.ind =TRUE),]
    return (METRO)
  }
  formattingVariables_METRO = function (){
    METRO$X <<- NULL
    METRO$Ent.Station <<- sapply (METRO$Ent.Station , as.character)
    METRO$Ext.Station <<- sapply (METRO$Ext.Station , as.character)
    METRO$Riders..Typical.Day..May.2014 <<- sapply (METRO$Riders..Typical.Day..May.2014 , as.numeric)
  }
  #---NEARZONES
  namesmatchNEARZONE_METRO =function(){ #check to see if METRO NAMES are the same as NEARZONE Snames
  #unique (METRO$Ent.Station)
  #unique (METRO$Ent.Station) %in% NEARZONES$MetroStnFullPt.NAME
  # which (unique (METRO$Ent.Station) %in% NEARZONES$MetroStnFullPt.NAME)  
  print (' No. of Stations that are in METRO dataset but not NEARZONES')  
  which (unique (METRO$Ent.Station) %in% NEARZONES$MetroStnFullPt.NAME == FALSE) 
  }
  formattingVariables_NEARZONES = function() {
    NEARZONES$MetroStnFullPt.NAME <<- sapply (NEARZONES$MetroStnFullPt.NAME , as.character) 
  }
  ##------EXECUTION
  formattingVariables_METRO()
  formattingVariables_NEARZONES()
  removecategorial_ALLZONES(3,1)#(Purpose: 1:NHB , 2:HBW ,3:HBO,TimeofDay:1:H16:H20 2:H6:H10)
  namesmatchNEARZONE_METRO()
  print ("Only Van dorn station has 199 near zones")
  table (NEARZONES$MetroStnFullPt.NAME)
  ALLZONES$X <<- NULL
}
construct_Predictors = function (){
  #First 2 columns are origin and destination stations
  ##-------CONSTRUCTING UNITS
   Predictors <<-  data.frame (METRO)
    Predictors$X <<- NULL
    for  (i in 1:200) { Predictors[1,paste ("Closesness Rank" ,i)] <<-  0}
    apply(Predictors[,6:ncol(Predictors)], 1, function(x) 0)
  
  
    #METRO - NEARZONES : Get the nearest 200 zones from NEARZONES for the origin station in METRO
    get_count = function (x){#x: Row number in metro , y:zone Rank 
    ###PREDICTOR ANALYSIS
    get_closest200Zones_x_OriginStation = function (x){
      closest200originstations = c()
      closest200originstations = NEARZONES [METRO [x, "Ent.Station"], 2]
      return (closest200originstations)
    } #x here is the row number in the dataframe METRO
    get_closest200Zones_x_DestinationStation = function (x){
      closest200Destinationstations = c()
      closest200Destinationstations = NEARZONES [ grepl( METRO [x, "Ext.Station"],NEARZONES$MetroStnFullPt.NAME), 2]
      return (closest200Destinationstations)
    }
    
    
    ###ZONE ANALYSIS
    O_D <<- data.frame (get_closest200Zones_x_OriginStation(x),get_closest200Zones_x_DestinationStation(x)) #1st_origin_Destination_dataframe
    colnames (O_D) <<- c ("Origin","Destination") #Origin and desitnation zones for the yth station in METRO 
    
    getting_one_zonecombinationcounts = function (y){ #y here is the zone closeness rank
      origin = O_D[y,1] #Origin Zone of rank y
      Destination = O_D [y,2] #Destination Zone of rank y
      #get_count_from_origin_Destination 
      conditoin1 = ALLZONES$Origin_Zone == origin 
      conditoin2 = ALLZONES$Destination_Zone == Destination
      condition3 = conditoin1 & conditoin2
      zonescombination_index_ALLZONES = c()
      zonescombination_index_ALLZONES = which (condition3 , arr.ind = TRUE)
      zonescombination_count = ALLZONES$Count[zonescombination_index_ALLZONES]
      return (zonescombination_count)
    }
    return(lapply (row (O_D), function(x) getting_one_zonecombinationcounts(x)))
  }
   
  
  ##--------EXECUTION
    addCloseRanks()
    #get_count(1) #1: 1st record in METRO dataset
}
add_ALLZONESPredictors = function (){
  Closezones <<- data.frame(matrix(0, nrow = nrow(METRO), ncol = 200))
  colnames(Closezones) <<- c(1:200)
}

####EXECUTION
getmodifiedata ()
modifydata_1()
makeitfaster()
construct_Predictors ()


###TESTING
