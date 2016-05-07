# -*- coding: utf-8 -*-
"""
Created on Thu May 05 10:54:11 2016

@author: Ahmed
"""

import csv 

Stations_List = []
Stations_Dict = {} #Key1 : Enter Key2: Exit  [Weekend or weekday , Time of the day , Ridership]
myFile = open('METRO_P.csv','rt')
myReader = csv.reader(myFile)  
for row in myReader:
    if row [1]!= "Ent.Date.Service.Type":
       Stations_List.append ([row[1],row[2],row[3],row[4],row[5]]) 
       Stations_Dict[row[2] , row[3]] = [int(row[1]),int(row[4]), int (row[5])] 
myFile.close()


AllZONES = {} #Key1: Origin Zone Key2:Destination Zone ,Values [purpose,timeofday, count]
myFile = open('ALLZONES.csv','rt')
myReader = csv.reader(myFile)  
for row in myReader:
      if row [1]!= "Origin_Zone":          
          AllZONES[int(row[1]) ,int(row[2])] = [int(row[3]),int(row[4]),float(row[5])]  
myFile.close()

NEARZONs ={} #Key1 : Station Key2:ZoneRank , Values [TAR,Miles]
myFile = open('NEARZONES_NAMESCHANGED.csv','rt')
myReader = csv.reader(myFile)  
for row in myReader:
    if row [2] != "ZONE.ID.TAZ":
        NEARZONs[row[3] , int(row[1])] = [int(row[2]),float(row[5])] 
myFile.close()

REDUCEDZONEs = {}
def DataReduction ():
    for key in AllZONES:
        if AllZONES [key] [0] == 1 and   AllZONES [key] [1]==1:
            REDUCEDZONEs [key] =  AllZONES [key] [2]
            

# For the Fist Rank
#1- ENTRANCE METRO EXIT METRO TAR ENTRANCE TAR EXIT COUNT
#[Entrance - Exit ] : [Count,Rank]
#get Entrance Station
def getTAR (Station,Rank):
    return NEARZONs [Station ,Rank] [0]
def getCount (OriginTAR,DestinationTAR):
    if (OriginTAR,DestinationTAR) in REDUCEDZONEs:
        return REDUCEDZONEs [OriginTAR,DestinationTAR] 
    else:
        return 0    
    
EntrExt = []
def RankCounts (Rank): # [Entrance , Exit , Counts]
    for EntExt in Stations_List:
        EnterStation = EntExt[1]
        EntranceTAR = getTAR (EnterStation,Rank)
        ExitStation = EntExt[2]
        ExitTAR     = getTAR (ExitStation,Rank)
        Count       = getCount (EntranceTAR,ExitTAR)
        EntrExt.append( [EnterStation,ExitStation,Rank,Count])
    return  EntrExt 
    
#RankCounts (1)    
    
RanksList = [] # Ent.S Ext.S Rank Count
Records = []
def makefile (rang):
    for Rank in range (1,rang):
        Records = RankCounts (Rank)    
        for line in Records:
                counts = line [2]
                RanksList.append([line[0],line[1],line[2], counts])
        
makefile (20)   




    