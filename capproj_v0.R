library(jsonlite)

dataBUS <- stream_in(file("yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"))

dataCHK <- stream_in(file("yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"))

dataREV <- stream_in(file("yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"))

dataTIP <- stream_in(file("yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tip.json"))

dataUSR <- stream_in(file("yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"))

#Q11
xx <- as.data.frame(dataUSR$comp)[,3]
vecfunny <- tapply(xx, as.data.frame(dataUSR$name), sum, na.rm=T)
yy <- as.data.frame(dataUSR$fans)
vecfans <- tapply(dataUSR$fans, dataUSR$name, sum, na.rm=T)

v11 <- 0
v12 <- 0
v21 <- 0
v22 <- 0
for (i in 1:nrow(vecfunny)) {
        if(vecfunny[i] <= 1 & vecfans[i] <= 1) {v11 <- v11 + 1}
        if(vecfunny[i] <= 1 & vecfans[i] >  1) {v12 <- v12 + 1}
        if(vecfunny[i] >  1 & vecfans[i] <= 1) {v21 <- v21 + 1}
        if(vecfunny[i] >  1 & vecfans[i] >  1) {v22 <- v22 + 1}  
}

mtx <- matrix(nrow=2, ncol=2)
mtx[1,1] <- v11
mtx[1,2] <- v12
mtx[2,1] <- v21
mtx[2,2] <- v22



#########################################################
# Making a Table with the different CATEGORIES os business:
#myfunc <- function(x) {
        x <- dataBUS
        flg <- numeric()
        for (i in 1:nrow(x)) {
                for (j in 1:nrow(as.data.frame(x$cat[[i]]))) {
                        #Case of no entries in Characteristics
                        print(c("i=",i))
                        print(c("j=",j))
                        print(" ")
                        if(dim(as.data.frame(x$cat[[i]]))[1] == 0) {
                                #flg[i] <- "FALSE"
                                break
                        } else {
                                #if(x$cat[[i]][[j]] == "Coffee & Tea") {        
                                if(x$cat[[i]][[j]] == "Restaurants") {        
                                                #flg[i] <- "TRUE"
                                  flg[i] <- i
                                  break
                                 } else {
                                         if(j == nrow(as.data.frame(x$cat[[i]]))) { 
                                                 #flg[i] <- "FALSE"
                                                 break
                                           }
                                 }
                        }
                 }               
         }
 
#Plotting
jpeg('plot_2.jpg', quality = 100, bg = "white", res = 200, 
     width = 7, height = 7, units = "in")
par(mfrow=c(2,2), mar=c(2,2,2,0), oma=c(2,2,0,1), xpd=NA)

#Panel 1
plot(as.factor(x$attr$Smoking[flg]), x$star[flg], type="p", 
     col="blue", pch=20, xlab=" ", ylab="avg. Stars", main="Allowed to Smoke?")

#Panel 2
plot(as.factor(x$attr$Parking$street[flg]), x$star[flg], type="p", 
     col="red", pch=20, xlab=" ", ylab=" ", main="Parking in street?")

#Panel 3
plot(as.factor(x$attr$Price[flg]), x$star[flg], type="p", 
     col="green", pch=20, xlab=" ", ylab="Avg. Stars", main="Price range (low to high)")

#Panel 4
plot(as.factor(x$attr$Atti[flg]), x$star[flg], type="p", 
     col="orange", pch=20, xlab=" ", ylab=" ", main="Dressing code")

dev.off()

# I want to create an app that tells the user if his/her (new) business will be sucessful or not. The user will have to provide some basic information like category of business (e.g. restaurant, car services, plumber, etc.), location and specific features/attributes of it. In return, the user will receive a percentage (%) of sucess.
# To quantify business sucess, I will use the Yelp database (available for academic purposes). For each business I will obtain (1) average number of stars, (2) number of reviews and (3) number of check-in customers as estimators of how good a given business is doing.
# I will characterize each business features based on two main criteria: (A) attributes the customers find valuable in a business (e.g., having music, parking, or free Wi-Fi), and (B) attributes related to business location.
# For criterium A, I will use again the Yelp database to retrieve information about 38 different attributes a business may have.
# For criterium B, I will use 2 different information sources: (1) the Zillow API from which I will get the mean property value for the location of a given business, and (2) Local Crime index information (only for the city of Chicago and for Washington D.C.)
# With all these sources of data, I will attempt different approches in order to characterize a sucessfull business, including clustering, trees, and principal component analysis (PCA).
# The code will be implemented in R language.

##PLOT: the linked plot shows some results of a preliminary data exploration (Yelp). Certain business attributes clearly correlate with customer appreciation (number of stars, proxy of business success). 



##############################
# Nov-06-15
##############################

# Making matrix with Bus. vs Category Info

x <- dataBUS
strg_categ <- character()
flg_nocat <- numeric()
for (i in 1:nrow(x)) {
        for (j in 1:nrow(as.data.frame(x$cat[[i]]))) {
                #Case of no entries in Characteristics
                print(c("i=",i))
                print(c("j=",j))
                print(" ")
                if(dim(as.data.frame(x$cat[[i]]))[1] == 0) {
                        flg_nocat[i] <- i
                        break
                } else {
                        if(i==1 & j==1) {strg_categ <- x$cat[[i]][[j]]}
                        else {strg_categ <- rbind(strg_categ, x$cat[[i]][[j]])}
                        }
        }
} 

######################

# Getting total Avg. Check-in per week
y <- dataRESTO
num_checkin <- numeric()
flg_withcheckin <- numeric()
count <- 1
for (i in 1:nrow(y)) {
        #flg <- dataCHK$bus == y$bus[[i]] #T/F vector y-size
        flgnum <- which(dataCHK$bus == y$bus[[i]], arr.ind=T) # Index single number
        print(c(">> Case ",i, " of ",nrow(y)))
        print(c("Flg=", flgnum))
        
        #Case with no entry for i-esim business
        if(dim(as.data.frame(flgnum))[1] == 0) {
                print("No data for this entry...")
        } else {
                flg_withcheckin[count] <- i
                num_checkin[count] <- sum(dataCHK$checkin[flgnum,],na.rm = TRUE)
                print(c("Num. Check-ins=", num_checkin[count]))
                count <- count + 1
                
        }
        print(" ")
}

dataFIN <- cbind(dataRESTO[flg_withcheckin,], num_checkin) 



#########
# Figure 1: Plotting Success tracers: stars, num. reviews, num.check-in
par(mfrow=c(1,3))#, mar=c(0,0,0,0), oma=c(4,4,4,1), xpd=NA)

#Panel 1: Num. Reviews vs Stars
plot(log10(dataFIN$rev), dataFIN$star, col="green", type="p",
     xlab="log10(Num. of Reviews)", ylab="Mean Stars")

vec_meanrev <- tapply(dataFIN$rev, dataFIN$star, mean)
lg10vec_meanrev <- log10(vec_meanrev)
vec_star <- levels(factor(dataFIN$star))

points(lg10vec_meanrev, vec_star, col="red", pch=16, type="o")



#Panel 2: Stars vs Check-in
plot(log10(dataFIN$num), dataFIN$star, col="green", type="p",
     xlab="log10(Check-Ins per week)", ylab="Mean Stars")

vec_meancheckin <- tapply(dataFIN$num, dataFIN$star, mean)
lg10vec_meancheckin <- log10(vec_meancheckin)
vec_star <- levels(factor(dataFIN$star))

points(lg10vec_meancheckin, vec_star, col="red", pch=16, type="o")


#Panel 3: Num. reviews vs Check-in
plot(log10(dataFIN$num), log10(dataFIN$rev), col="green", type="p",
     xlab="log10(Check-Ins per week)", ylab="log10(Num. of Reviews)")

mymod <- lm(log10(dataFIN$rev) ~ log10(dataFIN$num) + 
                    I(log10(dataFIN$num)^2))
curve(mymod$coef[1] + mymod$coef[2]*x + 
              mymod$coef[3]*I(x^2), col="red", add=T, lwd=3)




###########
# Figure 2: Histograms of star and check-in
# Panel TOP: Stars
par(mfrow=c(2,1), mar=c(2,0,2,0), oma=c(4,4,0,0), xpd=NA)
bins <- seq(0.5,5, by=0.5)
hstar <- hist(dataFIN$star, breaks=bins, xlab="Avg.Stars", 
              ylab="Number", main="", xaxt="n")
hstar
axis(side=1, at=hstar$mids, labels=seq(1,5,by=0.5))

#Worst/Median/Best subsamples
#dataBstar <- dataFIN$star[dataFIN$star >= 4.5]
dataBstar <- dataFIN[dataFIN$star >= 4.5,]
dataWstar <- dataFIN[dataFIN$star < 3,]
dataMstar <- dataFIN[dataFIN$star >= 3 & dataFIN$star < 4,]
hist(dataBstar$star, breaks=bins, add=T, col="blue")# BEST stars
hist(dataWstar$star, breaks=bins, add=T, col="red")# WORST stars

lines(c(3.25,3.25), c(0,5200), col="green", lty=3, lw=3)
text(2.5, 5000, labels="Median", col="green")
text(1.5, 2100, labels="Worst", col="red")
text(4.4, 2400, labels="Best", col="blue")


# Panel BOTTOM: Check-in
hist(log10(dataFIN$num), xlab="log10(Avg.Check-in)", ylab="Number", main="")

#Best/Median/Worst Checkin... just number of check-ins
dataBcheck <- dataFIN$num[log10(dataFIN$num) >= 2.199]
dataWcheck <- dataFIN$num[log10(dataFIN$num) < 1.2040]
dataMcheck <- dataFIN$num[log10(dataFIN$num) >= 1.2040 & 
                                  log10(dataFIN$num) < 2.199]
bins <- seq(2.199,4.199, by=0.2)
hist(log10(dataBcheck), add=T, breaks=bins, col="blue")# BEST check-in
bins <- seq(0,1.2040, by=0.2)
hist(log10(dataWcheck), add=T, breaks=bins, col="red")# WORST check-in

#Compar with stars selection
#bins <- seq(0, max(log10(dataFIN$num)), by=0.2)
hist(log10(dataFIN$num[dataFIN$star >= 4.5]), add=T, density=10,
     border="black", angle=45, col="cyan", lw=5)# BEST star overplot

hist(log10(dataFIN$num[dataFIN$star < 3]), add=T, density=10,
     border="black", angle=315, col="orange", lw=5)# WORST star overplot

#Lineas and leyends
lines(c(1.69,1.69), c(0,2000), col="green", lty=3, lw=3)
text(1.2, 2100, labels="Median", col="green")
text(0.75, 1700, labels="Worst", col="red")
text(3.1, 1000, labels="Best", col="blue")


##########
#Main features of best restaurants (STARS)

for(i in 1:38) {mystr <- class(dataBstar$att[,i])
                print(c(i, " ", mystr))}

#dataBstar$att[11,12][1]
#romantic
#256    FALSE
#> dataBstar$att[11,12][9]
#casual
#256   TRUE


###################################
# Function to get data
####################################


fn_getdata <- function(dataXXstar) {

        attrXXstar <- numeric()
        perc_noNAXXstar <- numeric()
        norm_validsXXstar <- numeric()
        nameattrstar <- character()
        
        count <- 1
        for(i in 1:38) {
        
                #Excluding atttr. with no info
                  #Age allowed, Payment Types, Hair Styles, Accepts Insurance
                 if(i != 36 && i != 37 && i != 34 && i != 35) {
                
                 #Case 1: Logical T/F inputs in database 
                 #(+Accepts Credit card case)
                 if(class(dataXXstar$att[,i]) == "logical" || i == 3) {
                          print(c("Simple T/F Info in Column ",i))
                          #Number of TRUE
                          attrXXstar[count] <- 
                           dim(as.data.frame(which(dataXXstar$att[,i] == "TRUE",
                            arr.ind=T)))[1]

                          #Normalization: num of valid entries (!=NA)
                          norm_validsXXstar[count] <- dim(as.data.frame(which(!is.na(dataXXstar$att[,i]))))[1]
                          
                          #Note: for some reason I cannot get the size (to normalize %)
                          #  for the Median and Worst sud-samples. That is why I
                          # made this long + between NA+T+F to get number(???).
                          perc_noNAXXstar[count] <- round(100 * 
                                  dim(as.data.frame(which(!is.na(dataXXstar$att[,i]))))[1] / 
                                  (dim(as.data.frame(which(is.na(dataXXstar$att[,i]))))[1] + 
                                   dim(as.data.frame(which(dataXXstar$att[,i]=="TRUE")))[1] + 
                                   dim(as.data.frame(which(dataXXstar$att[,i]=="FALSE")))[1]), digits=2)       
          
                          nameattrstar[count] <- colnames(dataXXstar$att[i])
                         count <- count+1
                 }  else {
                  #Case 2: cases with multiple options (num or char)
                    #Price Range, Alcohol, Noise Level, Attire, Smoking, BYOB/Corkage
                  #Wi-Fi,
                          if(i !=35 && (i==6 || i==8 || i==9 || i==11 || i==16 || i==20 || i==29)) {
                                  print(" ")
                                  print(c("Special case in Column ",i))
                                 for(j in 1:dim(table(dataXXstar$attr[i]))) {
                                   attrXXstar[count] <- dim(
                                      as.data.frame(
                                          which(dataXXstar$attr[i] == names(table(dataXXstar$attr[i])[j]),
                                         arr.ind=T)))[1] 
                                   
                                   perc_noNAXXstar[count] <- round(100 * 
                                           dim(as.data.frame(which(!is.na(dataXXstar$att[i]))))[1] / 
                                           nrow(as.data.frame(dataXXstar$att[,i])), digits=2)
                                   
                                   #Normalization: num of valid entries (!=NA)
                                   norm_validsXXstar[count] <- dim(as.data.frame(which(!is.na(dataXXstar$att[i]))))[1]
                                   
                                   nameattrstar[count] <- paste(colnames(dataXXstar$attr[i]),
                                                      names(table(dataXXstar$attr[i])[j]))
                                   count <- count + 1
                                   }
                         } else { # Case 3: All other cases with sub-attr.
                                     #Extractong T/F matrix with restaurant vs sub-attr.
                                     print(" ")
                                     print(c("Complex T/F Info in Column ",i))
                                     mtx <- data.frame(row.names=seq(1:as.numeric(nrow(dataXXstar))))
                                     mtxOLD <- data.frame(row.names=seq(1:as.numeric(nrow(dataXXstar))))
                                     print("Making matrix...")
                                     for(j in 1:as.numeric(nrow(dataXXstar))) {
                                          row_j <- dataXXstar$att[j,i]
                                           #print(row_j)
                                          mtx <- rbind(mtxOLD, row_j)
                                          mtxOLD <- mtx
                                         }
                              
                                     # Getting info from each matrix column (sub-attr)
                                     print("Getting sub-attr info...")
                                     for(k in 1:ncol(mtx)) {
                                            #print(c("K is ", k))
                                            #Best star
                                            attrXXstar[count] <- 
                                                dim(as.data.frame(which(mtx[,k] == "TRUE",
                                                                      arr.ind=T)))[1]
                                            
                                            perc_noNAXXstar[count] <- round(100 * 
                                                    dim(as.data.frame(which(!is.na(mtx[k]))))[1] / 
                                                    nrow(as.data.frame(dataXXstar$att[,i])), digits=2)

                                            #Normalization: num of valid entries (!=NA)
                                            norm_validsXXstar[count] <- dim(as.data.frame(which(!is.na(mtx[k]))))[1]
                                                                                       
                                            #print("Pass 1")
                                            nameattrstar[count] <- paste(colnames(dataXXstar$att[i]),
                                                                    colnames(mtx[k]))
                                           #print("Pass 2")
                                           count <- count + 1
                                         }                            
                                  }   
                         }
                }
        }
        allXXstar <- cbind(attrXXstar, perc_noNAXXstar, norm_validsXXstar, nameattrstar)
        return(allXXstar)
}


# Calling function for the 3 subsamples
allBstar <- fn_getdata(dataBstar)
allMstar <- fn_getdata(dataMstar)
allWstar <- fn_getdata(dataWstar)


# Combining Best, Median, Worst sample data
allStar <- cbind(allBstar[,4], allBstar[,1], allMstar[,1], allWstar[,1], 
                 allBstar[,2], allMstar[,2], allWstar[,2], 
                 allBstar[,3], allMstar[,3], allWstar[,3])
colnames(allStar) <- c("Attrib.", "Best Star", "Median Star", "Worst Star",
                        "Best %noNA", "MEDIAN %noNA","Worst %noNA",
                        "Normal. valid Best", "Normal. valid Median", "Normal. valid Worst")

# In % of each subsample size
percattrStar <-cbind(allStar[,1],
                     # % Normalized wr total number of entries
                     #round(as.numeric(allStar[,2]) * 100 / 
                     #              nrow(dataBstar), digits=1),
                     #round(as.numeric(allStar[,3]) * 100 / 
                     #              nrow(dataMstar), digits=1),
                     #round(as.numeric(allStar[,4]) * 100 / 
                     #              nrow(dataWstar), digits=1),
                     
                     # % Normalized wr total number of entries
                     round(as.numeric(allStar[,2]) * 100 / 
                                   allStar[,8], digits=1),
                     round(as.numeric(allStar[,3]) * 100 / 
                                   allStar[,9], digits=1),
                     round(as.numeric(allStar[,4]) * 100 / 
                                   allStar[,10], digits=1),
                     allStar[,5:7])

percattrStar




##############################################################
#Studing sub-samples based on number of Check-in
################################################################

#Best/Median/Worst Checkin... just number of check-ins
dataBcheck <- dataFIN[dataFIN$num[log10(dataFIN$num) >= 2.199],]
dataWcheck <- dataFIN[dataFIN$num[log10(dataFIN$num) < 1.2040],]
dataMcheck <- dataFIN[dataFIN$num[log10(dataFIN$num) >= 1.2040 & 
                                  log10(dataFIN$num) < 2.199],]


allBcheck <- fn_getdata(dataBcheck)
allMcheck <- fn_getdata(dataMcheck)
allWcheck <- fn_getdata(dataWcheck)


# In % of each subsample size
#allXXstar <- cbind(attrXXstar, perc_noNAXXstar, norm_validsXXstar, nameattrstar)

# Best
percattrCheckB <-cbind(allBcheck[,4],
                       
                       # % Normalized wr total number of entries
                       round(as.numeric(allBcheck[,1]) * 100 / 
                                     as.numeric(allBcheck[,3]), digits=1),
                       allBcheck[,2])

percattrCheckB


# Median
percattrCheckM <-cbind(allMcheck[,4],
                       
                       # % Normalized wr total number of entries
                       round(as.numeric(allMcheck[,1]) * 100 / 
                                     as.numeric(allMcheck[,3]), digits=1),
                       allMcheck[,2])

percattrCheckM


# Worst
percattrCheckW <-cbind(allWcheck[,4],
                       
                       # % Normalized wr total number of entries
                       round(as.numeric(allWcheck[,1]) * 100 / 
                                     as.numeric(allWcheck[,3]), digits=1),
                       allWcheck[,2])

percattrCheckW


# making final table (matrix) with % in Star and Check-in 
#  selection for data fractions >=80%

#indStar <- c(2, 3, 4, 5, 6, 7, 8, 9,10,18,19,20,21,37,38,39,43,44,45,46,47,48,49,50,51,52,53,54,62,63,64,65,69,70,71,72)
indStar <- c(3, 4, 5, 6, 7, 8, 9,10,18,19,20,21,37,38,39,43,44,45,46,47,48,49,50,51,52,53,54)
indCheckM <- c(3, 4, 5, 6, 7, 8, 9,10,18,19,20,36,37,38,41,42,43,44,45,46,47,48,49,50,51,52)
indCheckW <- c(3,4,5,6,7,8,15,16,30,31,32,35,36,37,38,39,40,41,42,43,44,45,46)


fintable <- data.frame(cbind(percattrStar[indStar, 1:4], 
                             percattrCheckB[indStar, 2],
                             c(percattrCheckM[indCheckM[1:11], 2], 0,percattrCheckM[indCheckM[12:26], 2])),
                             c(percattrCheckW[indCheckW[1:5], 2], 0, 0, percattrCheckW[indCheckW[6:8], 2], 0, 0, percattrCheckW[indCheckW[9:23], 2]))

colnames(fintable) <- c("Attributes","Best Star [%]","Avg. Star [%]","Worst Star [%]","Best Check-in [%]","Avg. Check-in [%]","Worst Check-in [%]")


print(fintable)

###### Making nice table with results
```{r, results='asis', warning=FALSE, message=FALSE}
library(ReporteRs)
#data(iris)
fintableFT = FlexTable( fintable )

vars <- colnames(fintable)
for (i in vars) {
        if(i == 1) fintableFT[, i] = cellProperties( background.color = "white" )        
        if(i == 2) fintableFT[, i] = cellProperties( background.color = "white" )        
        if(i == 3) fintableFT[, i] = cellProperties( background.color = "white" )        
        if(i == 4) fintableFT[, i] = cellProperties( background.color = "white" )        
        if(i == 5) fintableFT[, i] = cellProperties( background.color = "white" )        
        if(i == 6) fintableFT[, i] = cellProperties( background.color = "white" )        
        if(i == 7) fintableFT[, i] = cellProperties( background.color = "white" )        
        #irisFT[iris[, i] >= 3 & iris[, i] < 3.5, i] = cellProperties( background.color = "yellow" )
        #irisFT[iris[, i] > 4, i] = cellProperties( background.color = "#81DAF5" )
}

cat(as.html(fintableFT))
```
###RAW Final table
| Attributes Best Star [%] Avg. Star [%] Worst Star [%] Best Check-in [%] Avg. Check-in [%] Worst Check-in [%]| 
_______________________________________________________________________________________________________________
| 1  Accepts Credit Cards          89.7          95.6             93              96.8              86.9               89.2| 
| 2       Good For Groups          80.5          87.7           74.5              88.6              83.4               81.7| 
| 3       Outdoor Seating          38.9            42           26.3              37.4              30.7               29.3| 
| 4         Price Range 1          49.3          41.1           51.1              50.6              47.8               56.4| 
| 5         Price Range 2          35.9          49.8           37.1              43.2              44.7               43.6| 
| 6         Price Range 3           7.4           4.8            2.6               4.9               5.9                  0| 
| 7         Price Range 4           2.8           0.8            0.7               1.2               1.6                  0| 
| 8         Good for Kids          77.5            80           76.3              82.1              79.8               83.8| 
| 9                Has TV          35.2          48.8           33.7              54.9              51.3               61.4| 
| 10        Attire casual          87.8            93           87.3              94.8              96.9                100| 
| 11        Attire dressy           5.8           2.6            1.2               4.4               3.1                  0| 
| 12        Attire formal           0.3           0.1            0.5               0.7                 0                  0| 
| 13             Take-out          82.7          88.1           84.9              95.3              93.6                 84| 
| 14   Takes Reservations          32.9          34.1           16.8              30.2                33               19.9| 
| 15       Waiter Service            49          59.2           37.2                65              68.4               84.7| 
| 16               Caters          36.8          31.8           14.4              50.7              52.9                 29| 
| 17     Good For dessert           2.7           1.3            0.7                 1                 2                  0| 
| 18   Good For latenight             2           6.1            4.3               6.3               8.9               29.2| 
| 19       Good For lunch          31.9          40.3             21              41.1              33.2                6.3| 
| 20      Good For dinner          19.8          30.4           13.1              36.7              39.1               18.2| 
| 21   Good For breakfast           6.8             8            7.7               9.1               8.7                 19| 
| 22      Good For brunch           6.4           4.8            2.4               3.2               6.7               17.6| 
| 23       Parking garage           2.4           7.3            5.4                 5                 2                  0| 
| 24       Parking street          19.4          13.2            4.9              23.7              41.4               22.4| 
| 25    Parking validated           0.5           0.5            0.2               0.8               0.7                  0| 
| 26          Parking lot          37.8          47.3           25.2              48.8              38.6               34.8| 
| 27        Parking valet           2.1           3.3            1.4                 3                 2                  0| 













