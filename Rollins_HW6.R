#Homework 6
#Carmen Rollins

#1 Using the diamonds data set, the objective is to create the following four plots 
#that show the relationship of diamond weight (in carats) to price:
require(ggplot2)
require(grid)
data(diamonds)
str(diamonds)
#carat is 1 column, price is 7th
str(diamonds[1])
str(diamonds[7])

#creating chart similar to image:
#
weight_price<- ggplot(diamonds) + geom_point(aes(x=carat,y=price, colour=factor(color))) + 
  labs(title="Diamonds - Weight to Price by Colour") 
#creating the ggplot using diamonds data adding geom_point (to create scatterplot) by 
#mapping the aesthetic points to carat and price data 
#adding the colour=factor(color) to sort the diamonds by color 
#(diamonds data has colours D-J in the dataset that we sort by)
#adding label for chart title

print(weight_price) #computing the scatterplot


#Similarily, I get the same graphic by using a method similar to Prof G's (from our first R class):
 #creating the ggplot using diamonds data, with aesthetic mappings  to carat and price
#now i must create the layers by adding labels and 
d_price <- ggplot(diamonds, aes(carat,price)) + labs(title="Diamonds - Weight to Price by Colour") +
  theme(plot.title = element_text(size = rel(2), colour = "blue")) +
  labs(x="Weight", y="Price") + 
  geom_point(aes(color = factor(color)))
#creating the 
print(d_price)

#2: Remove the non-linearity and replot. Think about transforming both price and weight 
#(e.g. take the natural log of both variables)
plot2<- ggplot(diamonds) + geom_point(aes(x=log(carat), y=log(price), 
              colour=factor(color))) + labs(title="Diamonds - Weight to Price by Colour") + labs(x="Weight", y="Price")
print(plot2)  
#exact same thing as #1 except adding log to x and y variables to take the natural log 
#and adding labels for Price and Carat (x,y axis)
#i.e: creating the ggplot using diamonds data adding geom_point (to create scatterplot)
#by mapping the aesthetic points to the natural
#log of carat and price data, then adding the colour=factor(color) to sort the diamonds 
#by color (diamonds data has colours D-J in the dataset that we sort by) adding label 
#for chart title and for Price and Carat (x,y axis)

#3 Remove the linear trend (create a linear model and use the residuals on y axis) and 
#create a scatter plot. Now use the grid package to create a custom version of the 
#scatterhist. The histogram on the left is a density histogram of the price and the 
#histogram on the bottom is a density of carat.

rmv_trnd<-lm(log(price)~log(carat), data=diamonds) #running a linear regression of the 
#natural logs of price on carat
resids<-resid(rmv_trnd)  #taking the residuals of the above regression
  
plot3<- ggplot(diamonds) + geom_point(aes(x=log(carat), resids, 
               colour=factor(color))) + labs(title="Diamonds - Weight to Price by Colour") + labs(x="Weight", y="Price Residuals")
print(plot3) #print first chart

#to create the density carat chart:
bin50 <- (max(diamonds[[1]])-min(diamonds[[1]]))/50 #range of carat[1] (carat).. take the max
#subtract the min and divide by 50 for width of bin for histogram..
dens_crt<- ggplot(diamonds, aes(x=diamonds[[1]], fill=color)) + aes(y=..density..) + 
  geom_histogram(binwidth=bin50) 
print(dens_crt)


#to create the density price chart:
bin50_2 <- (max(diamonds[[7]])-min(diamonds[[7]]))/50 #range of carat[1] (carat).. take the max
#subtract the min and divide by 50 for width of bin for histogram..
dens_price<- ggplot(diamonds, aes(x=diamonds[[7]], fill=color)) + aes(y=..density..) +
  geom_histogram(binwidth=bin50_2) 
print(dens_price)


#to use grid to get them all on the same page:

grid.newpage #create new page to use grid
#using viewport vp, change the space (x and y positions and width and length of each chart)
#to get the dimensions requested in question 3 (I played around with these for awhile)
print(plot3,vp=viewport(x=0.63, y=0.6, w=0.75, h=0.6)) 
print(dens_crt,vp=viewport(x=0.6, y=0.15, w=0.75, h=0.25))   
print(dens_price,vp=viewport(angle=90, x=0.13, y=0.6, w=0.5, h=0.25))

#4 Using grid package again, use viewports to create overlays
#plot the main chart
print(plot3)
#then add the next two charts (overlay them on top of other chart)
#play around with positios agian
print(dens_price,vp=viewport(x=0.26, y=0.16, w=0.4, h=0.2))
print(dens_crt,vp=viewport(x=0.8, y=0.7, w=0.4, h=0.2))





#####This is just rough draft code that I was playing with in attempt to solve the above.. 
#just keeping this scratchwork for my own record 

## Prof G: Script failed here because the layout is invalid.
viewport(layout = 3,
         layout.pos.row = NULL, layout.pos.col = NULL,
         name = NULL)

grid.draw(dens_price)

#flip the price chart
dens_price_flip<-dens_price+ coord_flip()
dfinal<-grid.arrange(dens_price_flip, plot3,dens_crt,widths=c(1, 2), heights=c(2, 1))
grid.arrange(dens_price_flip, plot3,dens_crt, ncol=2, nrow=2, widths=c(1, 3), heights=c(1, 2))

blank <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(dens_price_flip, plot3, dens_crt, ncol=2, nrow=2, widths=c(2, 2), heights=c(2, 2))