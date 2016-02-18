#Homework 5
#Carmen Rollins

#Vectors
#1 
#a
vector1<-c(1:20) #creating vector from 1-20

#b
vector2<-c(20:1) #creating vector from 20-1

#c
vector3<-c(vector1,vector2[-1]) #merges the vectors to have 1-20 and 19-1 in a vector. The [-1] after vector2 will eliminate the first position (i.e. the number 20) so that 20 is not repeated in this vector

#d
tmp<-c(4,6,3) 

? rep #this explains the function rep, which is short for 'replicate'. It will replicate values in x

#e
vector4<-rep(tmp,10) #vector tmp=4,6,3 sequence we need. Therefore we want to repreat the vector tmp 10 times

#f
vector5<-c(rep(tmp,10),rep.int(4,1)) #again we want tmp repeated 10 times and then have a 4 at the end of the sequence for 11 occurences of 4 
#alternatively we could do:  vector5<-c(rep(tmp,10),tmp[1])

#g
vector6<-c(rep.int(tmp[1],10),rep.int(tmp[2],20),rep.int(tmp[3],30))
#here i use tmp[1] (position 1, which is "4") to compute 10 occurences of 4 ;
# tmp[2](position 2, which is "6") to compute 20 occurences of 6 ;
# and tmp[3](position 3, which is "3") to compute 30 occurences of 3;

#2
x=seq.int(3,6,.1) #entering x values: values are from 3-6 with .1 length 
vector_value<-exp(x)*cos(x) #creating vector with x values

#3 #a
a<-c(0.1^seq.int(3,36,3)*0.2^seq.int(1,34,3))
#creates vector with .01 raised to the power of a sequence from 3-36 jumping by 3 (i.e. .1^3,.1^6..etc)
#multiplied by .2 raised to the power of a sequence from 1-34 jumping by 3 (i.e. .2^1, .2^4, etc)

#b
b<-2^seq(1,25) #setting up first part of equation: 2^1,2^2...2^25
c<-c(b/seq(1,25)) #using the first part (b) with the second part, b/1,b/2...b/25
#the result will be (2,(2^2)/2,...(2^25)/25)

#4 #a
i=seq(10,100) #defining i=10-100
sum((i^3)+(4*i^2)) #sum of all the i's as indicated in the initial equation

#b
i=seq(1,25) #defining i=1-25
sum(((2^i)/i)+((3^i)/(i^2))) #sum of all the i's as indicated in the initial equation

#5 
#see help for info...
? paste

#a
paste(c("label"),1:30) #pasting the vector 'label' to the numbers 1-30

#b
paste(c("fn"),1:30,sep = "")

#6
#creating two vectors with length 250, as instructed:
set.seed(50) 
xVec <- sample(0:999, 250, replace=T) 
yVec <- sample(0:999, 250, replace=T) 

#a
Vector1<-c(yVec[2:250]-xVec[-250])
#starting at y2 (second position in the y values) to the end of 250 values
#starting at x1 (first position) to n-1 x values (250-1=249 values) 
    #i.e. minus the 250th position, the last position)

#b
Vector2<-c(sin(yVec[-250])/cos(xVec[2:250]))
#starting at sin(y1) (first position of y) to n-1 y values (250-1=249 values)
#then starting atcos(x2) (second position of x) to the end of 250 values 

#c
Vector3<-c(xVec[1:248]+2*xVec[2:249]-xVec[3:250])
#we know n=250
#starting at X1 through Xn-2 we want vector from 1 to 248 (n-2=248)
#From 2*X2 we want X2 through Xn-1 which is 2 to 249 (n-1=249)
#From X3 we want X3 through Xn which is 3 to 250 

#d
sum(exp(-xVec[2:250])/(xVec[1:249]+10))
#we know n=250, therefore n-1=249. When we calculate Xi+1 we know the values range
#from 2-250 (since i is from 1-(250-1)) so we plug this into the xVec in the numerator
#in the demoninator, we know that i=1-249, therefore we want xVec from 1-249.

#7 #a
a<- yVec[yVec > 600] #will show all values greater than 600 in yVector

#b
yindex<-sort(a,index.return=1:250)
yindex$ix
#sorts yVec values >600 (i.e. a from 7a) in order (lowest-highest) and indexes
#$x are the values and $ix are the index positions of the values of a

#c
b<- xVec[xVec > 600] #will show all values greater than 600 in xVector
xindex<-sort(a,index.return=1:250)
#sorts xVec values >600 (i.e. b) in order (lowest-highest) and indexes
#$x are the values and $ix are the index positions of the values of b

(yindex$ix)
(xindex$ix) #by eyeballing it I can see all x and y index positions correpond
#to confirm i'll take the difference of them:
difference<-(yindex$ix)-(xindex$ix)
difference #all equal zero therefore we know values are identical.
#all values correspond

sort.list(b)
#d
abs(xVec[1:250]-mean(xVec))^(1/2)
#or alternatively 1/2= sqrt so to use the sqrt function as instructed in question:
sqrt(abs(xVec[1:250]-mean(xVec)))

#e

e<-sort(yVec) #using sort, we see the max value is 997
#altenatively we could have found the max value using max()
max(yVec) 
997-200 # Taking the max value and subtrating it by 200, we get 797
e1<-length(e[e >= 797]) #length will count all the values of e that are >= 797
e1 #57 values

#f

f<-length(xVec[xVec %% 2==0]) 
#length counts all values in xVec that are divisible by 2
#That is, %% 2==0 will return all values that have a remainder of 0 when divided by 2


#g

xVec[order(yVec)]#All xVec numbers are sorted in order of increasing values of yVec.


#h
h<-yVec[seq(1,250,3)]
#this will take positions 1-250 and skip by 3 (i.e. 1,4,7,10, etc.) and return values
#at those positions for yVec


#8
1+sum((cumprod(seq(2,38,2))/cumprod(seq(3,39,2))))
#numerator: will calculate the cumulative product of from sequence 2-38, jumping by 2 this (i.e. all even numbers)
#denomanator: will calculate the cumulative product of from sequence 2-39, jumping by 2 this (i.e. all odd numbers)
