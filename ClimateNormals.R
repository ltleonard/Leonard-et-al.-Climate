#Data was retrieved from the NWCC Report Generator for Butte 380: wcc.sc.egov.usda.gov/

################________________________DRY SOIL____________________################
	#Let's begin with Maximum Soil Percents, which was available from 2006-2016.
	#Edit the daily data column, and change the dates to only the year in format: YYYY
Max =read.table("YourFilePath.csv",sep=',',header=TRUE)
	#Let's identify the data associated with each year.
Max24 <- Max[(Max$DATE== "2006"),]
Max25 <- Max[(Max$DATE== "2007"),]
Max26 <- Max[(Max$DATE== "2008"),]
Max27 <- Max[(Max$DATE== "2009"),]
Max28 <- Max[(Max$DATE== "2010"),]
Max29 <- Max[(Max$DATE== "2011"),]
Max30 <- Max[(Max$DATE== "2012"),]
Max31 <- Max[(Max$DATE== "2013"),]
Max32 <- Max[(Max$DATE== "2014"),]
Max33 <- Max[(Max$DATE== "2015"),]
Max34 <- Max[(Max$DATE== "2016"),]
	#Omit any missing data.
Max24NA <- na.omit(Max24)
Max25NA <- na.omit(Max25)
Max26NA <- na.omit(Max26)
Max27NA <- na.omit(Max27)
Max28NA <- na.omit(Max28)
Max29NA <- na.omit(Max29)
Max30NA <- na.omit(Max30)
Max31NA <- na.omit(Max31)
Max32NA <- na.omit(Max32)
Max33NA <- na.omit(Max33)
Max34NA <- na.omit(Max34)
  #We want to determine the total days per each year that the soil moisture (Labeled B380 in my file) is less than 10. 
Y206<-sum(Max24NA$B380 <10)
Y207<-sum(Max25NA$B380 <10)
Y208<-sum(Max26NA$B380 <10)
Y209<-sum(Max27NA$B380 <10)
Y210<-sum(Max28NA$B380 <10)
Y211<-sum(Max29NA$B380 <10)
Y212<-sum(Max30NA$B380 <10)
Y213<-sum(Max31NA$B380 <10)
Y214<-sum(Max32NA$B380 <10)
Y215<-sum(Max33NA$B380 <10)
Y216<-sum(Max34NA$B380 <10)
#Write the results to a .csv
write.csv(list(Y206,Y207,Y208,Y209,Y210,Y211,Y212,Y213,Y214,Y215,Y216), "YourFile.csv") 

################________________________HOT DAYS____________________################
#Next up is calculating the total hot days. This is using the Maximum Air Temperature data 
Max =read.table("YourFilePath.csv",sep=',',header=TRUE)
	#Let's identify the data associated with each year.
Max8 <- Max[(Max$DATE== "1990"),]
Max9 <- Max[(Max$DATE== "1991"),]
Max10 <- Max[(Max$DATE== "1992"),]
Max11 <- Max[(Max$DATE== "1993"),]
Max12 <- Max[(Max$DATE== "1994"),]
Max13 <- Max[(Max$DATE== "1995"),]
Max14 <- Max[(Max$DATE== "1996"),]
Max15 <- Max[(Max$DATE== "1997"),]
Max16 <- Max[(Max$DATE== "1998"),]
Max17 <- Max[(Max$DATE== "1999"),]
Max18 <- Max[(Max$DATE== "2000"),]
Max19 <- Max[(Max$DATE== "2001"),]
Max20 <- Max[(Max$DATE== "2002"),]
Max21 <- Max[(Max$DATE== "2003"),]
Max22 <- Max[(Max$DATE== "2004"),]
Max23 <- Max[(Max$DATE== "2005"),]
Max24 <- Max[(Max$DATE== "2006"),]
Max25 <- Max[(Max$DATE== "2007"),]
Max26 <- Max[(Max$DATE== "2008"),]
Max27 <- Max[(Max$DATE== "2009"),]
Max28 <- Max[(Max$DATE== "2010"),]
	#Omit any missing data.
Max8NA <- na.omit(Max8)
Max9NA <- na.omit(Max9)
Max10NA <- na.omit(Max10)
Max11NA <- na.omit(Max11)
Max12NA <- na.omit(Max12)
Max13NA <- na.omit(Max13)
Max14NA <- na.omit(Max14)
Max15NA <- na.omit(Max15)
Max16NA <- na.omit(Max16)
Max17NA <- na.omit(Max17)
Max18NA <- na.omit(Max18)
Max19NA <- na.omit(Max19)
Max20NA <- na.omit(Max20)
Max21NA <- na.omit(Max21)
Max22NA <- na.omit(Max22)
Max23NA <- na.omit(Max23)
Max24NA <- na.omit(Max24)
Max25NA <- na.omit(Max25)
Max26NA <- na.omit(Max26)
Max27NA <- na.omit(Max27)
Max28NA <- na.omit(Max28)
  #We want to determine the total days per each year that the max aire temp (Labeled B380 in my file) is greater than 20 degrees Celsius. 
Y91<-sum(Max9NA$B380 >20)
Y92<-sum(Max10NA$B380 >20)
Y93<-sum(Max11NA$B380 >20)
Y94<-sum(Max12NA$B380 >20)
Y95<-sum(Max13NA$B380 >20)
Y96<-sum(Max14NA$B380 >20)
Y97<-sum(Max15NA$B380 >20)
Y98<-sum(Max16NA$B380 >20)
Y99<-sum(Max17NA$B380 >20)
Y200<-sum(Max18NA$B380 >20)
Y201<-sum(Max19NA$B380 >20)
Y202<-sum(Max20NA$B380 >20)
Y203<-sum(Max21NA$B380 >20)
Y204<-sum(Max22NA$B380 >20)
Y205<-sum(Max23NA$B380 >20)
Y206<-sum(Max24NA$B380 >20)
Y207<-sum(Max25NA$B380 >20)
Y208<-sum(Max26NA$B380 >20)
Y209<-sum(Max27NA$B380 >20)
Y210<-sum(Max28NA$B380 >20)
#Write the results to a .csv
write.csv(list(Y90,Y91,Y92,Y93,Y94,Y95,Y96,Y97,Y98,Y99,Y200,Y201,Y202,Y203,Y204,Y205,Y206,Y207,Y208,Y209,Y210), "YourFile.csv") 

################________________________Total Precipitaiton____________________################
#Next up is calculating the total precipitation SWE. This is using the Precipitation increment- snow -adj data
Max =read.table("YourFilePath.csv",sep=',',header=TRUE)
	#Let's identify the data associated with each year.
Max8 <- Max[(Max$DATE== "1990"),]
Max9 <- Max[(Max$DATE== "1991"),]
Max10 <- Max[(Max$DATE== "1992"),]
Max11 <- Max[(Max$DATE== "1993"),]
Max12 <- Max[(Max$DATE== "1994"),]
Max13 <- Max[(Max$DATE== "1995"),]
Max14 <- Max[(Max$DATE== "1996"),]
Max15 <- Max[(Max$DATE== "1997"),]
Max16 <- Max[(Max$DATE== "1998"),]
Max17 <- Max[(Max$DATE== "1999"),]
Max18 <- Max[(Max$DATE== "2000"),]
Max19 <- Max[(Max$DATE== "2001"),]
Max20 <- Max[(Max$DATE== "2002"),]
Max21 <- Max[(Max$DATE== "2003"),]
Max22 <- Max[(Max$DATE== "2004"),]
Max23 <- Max[(Max$DATE== "2005"),]
Max24 <- Max[(Max$DATE== "2006"),]
Max25 <- Max[(Max$DATE== "2007"),]
Max26 <- Max[(Max$DATE== "2008"),]
Max27 <- Max[(Max$DATE== "2009"),]
Max28 <- Max[(Max$DATE== "2010"),]
	#Omit any missing data.
Max8NA <- na.omit(Max8)
Max9NA <- na.omit(Max9)
Max10NA <- na.omit(Max10)
Max11NA <- na.omit(Max11)
Max12NA <- na.omit(Max12)
Max13NA <- na.omit(Max13)
Max14NA <- na.omit(Max14)
Max15NA <- na.omit(Max15)
Max16NA <- na.omit(Max16)
Max17NA <- na.omit(Max17)
Max18NA <- na.omit(Max18)
Max19NA <- na.omit(Max19)
Max20NA <- na.omit(Max20)
Max21NA <- na.omit(Max21)
Max22NA <- na.omit(Max22)
Max23NA <- na.omit(Max23)
Max24NA <- na.omit(Max24)
Max25NA <- na.omit(Max25)
Max26NA <- na.omit(Max26)
Max27NA <- na.omit(Max27)
Max28NA <- na.omit(Max28)
  #We want to determine the sum of SWE(Labeled B380 in my file). 
Y90<-sum(Max8NA$B380)
Y91<-sum(Max9NA$B380)
Y92<-sum(Max10NA$B380)
Y93<-sum(Max11NA$B380)
Y94<-sum(Max12NA$B380)
Y95<-sum(Max13NA$B380)
Y96<-sum(Max14NA$B380)
Y97<-sum(Max15NA$B380)
Y98<-sum(Max16NA$B380)
Y99<-sum(Max17NA$B380)
Y200<-sum(Max18NA$B380)
Y201<-sum(Max19NA$B380)
Y202<-sum(Max20NA$B380)
Y203<-sum(Max21NA$B380)
Y204<-sum(Max22NA$B380)
Y205<-sum(Max23NA$B380)
Y206<-sum(Max24NA$B380)
Y207<-sum(Max25NA$B380)
Y208<-sum(Max26NA$B380)
Y209<-sum(Max27NA$B380)
Y210<-sum(Max28NA$B380)
#Write the results to a .csv
write.csv(list(Y90,Y91,Y92,Y93,Y94,Y95,Y96,Y97,Y98,Y99,Y200,Y201,Y202,Y203,Y204,Y205,Y206,Y207,Y208,Y209,Y210), "YourFile.csv") 
################________________________Total Snow____________________################
#Next up is calculating the total precipitation SWE. This is using the Snow Water Equivilant data.
Max =read.table("YourFilePath.csv",sep=',',header=TRUE)
	#Let's identify the data associated with each year.
Max8 <- Max[(Max$DATE== "1990"),]
Max9 <- Max[(Max$DATE== "1991"),]
Max10 <- Max[(Max$DATE== "1992"),]
Max11 <- Max[(Max$DATE== "1993"),]
Max12 <- Max[(Max$DATE== "1994"),]
Max13 <- Max[(Max$DATE== "1995"),]
Max14 <- Max[(Max$DATE== "1996"),]
Max15 <- Max[(Max$DATE== "1997"),]
Max16 <- Max[(Max$DATE== "1998"),]
Max17 <- Max[(Max$DATE== "1999"),]
Max18 <- Max[(Max$DATE== "2000"),]
Max19 <- Max[(Max$DATE== "2001"),]
Max20 <- Max[(Max$DATE== "2002"),]
Max21 <- Max[(Max$DATE== "2003"),]
Max22 <- Max[(Max$DATE== "2004"),]
Max23 <- Max[(Max$DATE== "2005"),]
Max24 <- Max[(Max$DATE== "2006"),]
Max25 <- Max[(Max$DATE== "2007"),]
Max26 <- Max[(Max$DATE== "2008"),]
Max27 <- Max[(Max$DATE== "2009"),]
Max28 <- Max[(Max$DATE== "2010"),]
	#Omit any missing data.
Max8NA <- na.omit(Max8)
Max9NA <- na.omit(Max9)
Max10NA <- na.omit(Max10)
Max11NA <- na.omit(Max11)
Max12NA <- na.omit(Max12)
Max13NA <- na.omit(Max13)
Max14NA <- na.omit(Max14)
Max15NA <- na.omit(Max15)
Max16NA <- na.omit(Max16)
Max17NA <- na.omit(Max17)
Max18NA <- na.omit(Max18)
Max19NA <- na.omit(Max19)
Max20NA <- na.omit(Max20)
Max21NA <- na.omit(Max21)
Max22NA <- na.omit(Max22)
Max23NA <- na.omit(Max23)
Max24NA <- na.omit(Max24)
Max25NA <- na.omit(Max25)
Max26NA <- na.omit(Max26)
Max27NA <- na.omit(Max27)
Max28NA <- na.omit(Max28)
  #We want to determine the sum of SWE(Labeled B380 in my file). 
Y90<-sum(Max8NA$B380)
Y91<-sum(Max9NA$B380)
Y92<-sum(Max10NA$B380)
Y93<-sum(Max11NA$B380)
Y94<-sum(Max12NA$B380)
Y95<-sum(Max13NA$B380)
Y96<-sum(Max14NA$B380)
Y97<-sum(Max15NA$B380)
Y98<-sum(Max16NA$B380)
Y99<-sum(Max17NA$B380)
Y200<-sum(Max18NA$B380)
Y201<-sum(Max19NA$B380)
Y202<-sum(Max20NA$B380)
Y203<-sum(Max21NA$B380)
Y204<-sum(Max22NA$B380)
Y205<-sum(Max23NA$B380)
Y206<-sum(Max24NA$B380)
Y207<-sum(Max25NA$B380)
Y208<-sum(Max26NA$B380)
Y209<-sum(Max27NA$B380)
Y210<-sum(Max28NA$B380)
#Write the results to a .csv
write.csv(list(Y90,Y91,Y92,Y93,Y94,Y95,Y96,Y97,Y98,Y99,Y200,Y201,Y202,Y203,Y204,Y205,Y206,Y207,Y208,Y209,Y210), "YourFile.csv") 
