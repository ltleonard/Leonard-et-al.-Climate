#Let's read in the file first. 

Max =read.table("/Users/lleonard/Desktop/max.csv",sep=',',header=TRUE)
Max =read.table("/Users/lleonard/Desktop/TRY2.csv",sep=',',header=TRUE)
Max1 <- Max[(Max$DATE== "1983"),]
Max2 <- Max[(Max$DATE== "1984"),]
Max3 <- Max[(Max$DATE== "1985"),]
Max4 <- Max[(Max$DATE== "1986"),]
Max5 <- Max[(Max$DATE== "1987"),]
Max6 <- Max[(Max$DATE== "1988"),]
Max7 <- Max[(Max$DATE== "1989"),]
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
Max29 <- Max[(Max$DATE== "2011"),]
Max30 <- Max[(Max$DATE== "2012"),]
Max31 <- Max[(Max$DATE== "2013"),]
Max32 <- Max[(Max$DATE== "2014"),]
Max33 <- Max[(Max$DATE== "2015"),]
Max34 <- Max[(Max$DATE== "2016"),]
Max35 <- Max[(Max$DATE== "2017"),]
Max36 <- Max[(Max$DATE== "2018"),]
Max37 <- Max[(Max$DATE== "2019"),]
Max38 <- Max[(Max$DATE== "2020"),]

Max1NA <- na.omit(Max1)
Max2NA <- na.omit(Max2)
Max3NA <- na.omit(Max3)
Max4NA <- na.omit(Max4)
Max5NA <- na.omit(Max5)
Max6NA <- na.omit(Max6)
Max7NA <- na.omit(Max7)
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
Max29NA <- na.omit(Max29)
Max30NA <- na.omit(Max30)
Max31NA <- na.omit(Max31)
Max32NA <- na.omit(Max32)
Max33NA <- na.omit(Max33)
Max34NA <- na.omit(Max34)
Max35NA <- na.omit(Max35)
Max36NA <- na.omit(Max36)
Max37NA <- na.omit(Max37)
Max38NA <- na.omit(Max38)

max(Max1NA$P842PIncSnoAdj)
max(Max2NA$P842PIncSnoAdj)
max(Max3NA$P842PIncSnoAdj)
max(Max4NA$P842PIncSnoAdj)
max(Max5NA$P842PIncSnoAdj)
max(Max6NA$P842PIncSnoAdj)
max(Max7NA$P842PIncSnoAdj)
max(Max8NA$P842PIncSnoAdj)
max(Max9NA$P842PIncSnoAdj)
max(Max10NA$P842PIncSnoAdj)
max(Max11NA$P842PIncSnoAdj)
max(Max12NA$P842PIncSnoAdj)
max(Max13NA$P842PIncSnoAdj)
max(Max14NA$P842PIncSnoAdj)
max(Max15NA$P842PIncSnoAdj)
max(Max16NA$P842PIncSnoAdj)
max(Max17NA$P842PIncSnoAdj)
max(Max18NA$P842PIncSnoAdj)
max(Max19NA$P842PIncSnoAdj)
max(Max20NA$P842PIncSnoAdj)
max(Max21NA$P842PIncSnoAdj)
max(Max22NA$P842PIncSnoAdj)
max(Max23NA$P842PIncSnoAdj)
max(Max24NA$P842PIncSnoAdj)
max(Max25NA$P842PIncSnoAdj)
max(Max26NA$P842PIncSnoAdj)
max(Max27NA$P842PIncSnoAdj)
max(Max28NA$P842PIncSnoAdj)
max(Max29NA$P842PIncSnoAdj)
max(Max30NA$P842PIncSnoAdj)
max(Max31NA$P842PIncSnoAdj)
max(Max32NA$P842PIncSnoAdj)
max(Max33NA$P842PIncSnoAdj)
max(Max34NA$P842PIncSnoAdj)
max(Max35NA$P842PIncSnoAdj)
max(Max36NA$P842PIncSnoAdj)
max(Max37NA$P842PIncSnoAdj)
max(Max38NA$P842PIncSnoAdj)


Y83<- sum(Max1NA$B380 >20)
Y84<-sum(Max2NA$B380 >20)
Y85<-sum(Max3NA$B380 >20)
Y86<-sum(Max4NA$B380 >20)
Y87<-sum(Max5NA$B380 >20)
Y88<-sum(Max6NA$B380 >20)
Y89<-sum(Max7NA$B380 >20)
Y90<-sum(Max8NA$B380 >20)
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
Y211<-sum(Max29NA$B380 >20)
Y212<-sum(Max30NA$B380 >20)
Y213<-sum(Max31NA$B380 >20)
Y214<-sum(Max32NA$B380 >20)
Y215<-sum(Max33NA$B380 >20)
Y216<-sum(Max34NA$B380 >20)
Y217<-sum(Max35NA$B380 >20)
Y218<-sum(Max36NA$B380 >20)
Y219<-sum(Max37NA$B380 >20)
Y220<-sum(Max38NA$B380 >20)

sum(Max1NA$P842PIncSnoAdj)
sum(Max2NA$P842PIncSnoAdj)
sum(Max3NA$P842PIncSnoAdj)
sum(Max4NA$P842PIncSnoAdj)
sum(Max5NA$P842PIncSnoAdj)
sum(Max6NA$P842PIncSnoAdj)
sum(Max7NA$P842PIncSnoAdj)
sum(Max8NA$P842PIncSnoAdj)
sum(Max9NA$P842PIncSnoAdj)
sum(Max10NA$P842PIncSnoAdj)
sum(Max11NA$P842PIncSnoAdj)
sum(Max12NA$P842PIncSnoAdj)
sum(Max13NA$P842PIncSnoAdj)
sum(Max14NA$P842PIncSnoAdj)
sum(Max15NA$P842PIncSnoAdj)
sum(Max16NA$P842PIncSnoAdj)
sum(Max17NA$P842PIncSnoAdj)
sum(Max18NA$P842PIncSnoAdj)
sum(Max19NA$P842PIncSnoAdj)
sum(Max20NA$P842PIncSnoAdj)
sum(Max21NA$P842PIncSnoAdj)
sum(Max22NA$P842PIncSnoAdj)
sum(Max23NA$P842PIncSnoAdj)
sum(Max24NA$P842PIncSnoAdj)
sum(Max25NA$P842PIncSnoAdj)
sum(Max26NA$P842PIncSnoAdj)
sum(Max27NA$P842PIncSnoAdj)
sum(Max28NA$P842PIncSnoAdj)
sum(Max29NA$P842PIncSnoAdj)
sum(Max30NA$P842PIncSnoAdj)
sum(Max31NA$P842PIncSnoAdj)
sum(Max32NA$P842PIncSnoAdj)
sum(Max33NA$P842PIncSnoAdj)
sum(Max34NA$P842PIncSnoAdj)
sum(Max35NA$P842PIncSnoAdj)
sum(Max36NA$P842PIncSnoAdj)
sum(Max37NA$P842PIncSnoAdj)
sum(Max38NA$P842PIncSnoAdj)

write.csv(list(Y83,Y84,Y85,Y86,Y87,Y88,Y89,Y90,Y91,Y92,Y93,Y94,Y95,Y96,Y97,Y98,Y99,Y200,Y201,Y202,Y203,Y204,Y205,Y206,Y207,Y208,Y209,Y210,Y211,Y212,Y213,Y214,Y215,Y216,Y217,Y218,Y219,Y220), "/Users/lleonard/Documents/R/list.csv") 


