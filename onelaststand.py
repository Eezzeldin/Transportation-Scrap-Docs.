# -*- coding: utf-8 -*-
"""
Created on Thu May 05 10:54:11 2016

@author: Ahmed
"""
def matchingstationstrings(NearZones,Metro):
    return Metro in NearZones
def replacestationname (Nearzone,Metro):
    Metro1 = Metro
    for Metro in Metro1:
        if matchingstationstrings(Nearzone,Metro):
            return Metro


import csv 
Station_List = []
Stations_Dict = {} #Key1 : Enter Key2: Exit  [Weekend or weekday , Time of the day , Ridership]
myFile = open('METRO.csv','rt')
myReader = csv.reader(myFile)  
for row in myReader:
    if row [1]!= "Ent.Date.Service.Type":
       Stations_Dict[row[2] , row[3]] = [int(row[1]),int(row[4]), float (row[5])] 
       Station_List.append(row[2])
myFile.close()

AllZONES = {} #Key1: Origin Zone Key2:Destination Zone ,Values [purpose,timeofday, count]
myFile = open('ALLZONES.csv','rt')
myReader = csv.reader(myFile)  
for row in myReader:
      if row [1]!= "Origin_Zone":
          AllZONES[int(row[1]) ,int(row[2])] = [int(row[3]),int(row[4]),float(row[5])]  
myFile.close()

NEARZONs_1 ={}
NEARZONs ={} #Key1 : Station Key2:ZoneRank , Values [TAR,Miles]
myFile = open('NEARZONES.csv','rt')
myReader = csv.reader(myFile)  
for row in myReader:
    if row [2] != "ZONE.ID.TAZ":
        NEARZONs[row[3] , int(row[1])] = [int(row[2]),float(row[5])] 
        NEARZONs_1[replacestationname (row[3],Station_List) , int(row[1])] = [int(row[2]),float(row[5])]  
myFile.close()


def get_TAR(Station,Rank):
    TAR = NEARZONES [Station,Rank] [0]
    return TAR
def get_zonecount (TAR1,TAR2):
    zonecount = AllZONES [TAR1,TAR2]
    return zonecount     
#Def a function that returns a dictoinary . This dictionary has the TAR of both the entrance and exit staions.
#Stations_Zones ={} #Key1 : [Enter Station,ExitStations,Rank] Value: [TAR of Rank of Enter, TAR of Rank of Exit]
#
#for EntExtStation in Stations_Dict:
#    Stations_Zones [Enter Station,ExitStations,Rank]