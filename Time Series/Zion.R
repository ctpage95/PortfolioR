#Title,Bookmark
#Zion NP,Bookmark this report: https://irma.nps.gov/Stats/SSRSReports/Park%20Specific%20Reports/Annual%20Park%20Recreation%20Visitation%20(1904%20-%20Last%20Calendar%20Year)

zion <- read.csv(header = TRUE, stringsAsFactors =  FALSE, text = '
Year,RecreationVisitors,TotalRecreationVisitors
1919,"1,814","115,815,890"
1920,"3,692","115,815,890"
1921,"2,937","115,815,890"
1922,"4,109","115,815,890"
1923,"6,408","115,815,890"
1924,"8,400","115,815,890"
1925,"16,817","115,815,890"
1926,"21,964","115,815,890"
1927,"24,303","115,815,890"
1928,"30,016","115,815,890"
1929,"33,383","115,815,890"
1930,"55,297","115,815,890"
1931,"59,186","115,815,890"
1932,"51,650","115,815,890"
1933,"48,763","115,815,890"
1934,"68,801","115,815,890"
1935,"97,280","115,815,890"
1936,"124,393","115,815,890"
1937,"137,404","115,815,890"
1938,"149,075","115,815,890"
1939,"158,063","115,815,890"
1940,"165,029","115,815,890"
1941,"192,805","115,815,890"
1942,"68,797","115,815,890"
1943,"44,089","115,815,890"
1944,"42,243","115,815,890"
1945,"78,280","115,815,890"
1946,"212,280","115,815,890"
1947,"273,953","115,815,890"
1948,"297,571","115,815,890"
1949,"307,881","115,815,890"
1950,"323,402","115,815,890"
1951,"331,079","115,815,890"
1952,"352,921","115,815,890"
1953,"389,445","115,815,890"
1954,"416,800","115,815,890"
1955,"406,800","115,815,890"
1956,"421,200","115,815,890"
1957,"525,100","115,815,890"
1958,"590,700","115,815,890"
1959,"585,000","115,815,890"
1960,"575,800","115,815,890"
1961,"604,700","115,815,890"
1962,"622,100","115,815,890"
1963,"681,100","115,815,890"
1964,"705,200","115,815,890"
1965,"763,600","115,815,890"
1966,"815,200","115,815,890"
1967,"788,400","115,815,890"
1968,"877,100","115,815,890"
1969,"904,300","115,815,890"
1970,"903,600","115,815,890"
1971,"897,000","115,815,890"
1972,"889,417","115,815,890"
1973,"993,800","115,815,890"
1974,"859,300","115,815,890"
1975,"1,055,200","115,815,890"
1976,"1,090,000","115,815,890"
1977,"1,105,900","115,815,890"
1978,"1,193,212","115,815,890"
1979,"1,040,528","115,815,890"
1980,"1,123,846","115,815,890"
1981,"1,288,808","115,815,890"
1982,"1,246,290","115,815,890"
1983,"1,273,030","115,815,890"
1984,"1,377,254","115,815,890"
1985,"1,503,272","115,815,890"
1986,"1,670,503","115,815,890"
1987,"1,777,619","115,815,890"
1988,"1,948,332","115,815,890"
1989,"1,998,856","115,815,890"
1990,"2,102,400","115,815,890"
1991,"2,236,997","115,815,890"
1992,"2,390,626","115,815,890"
1993,"2,392,580","115,815,890"
1994,"2,270,871","115,815,890"
1995,"2,430,162","115,815,890"
1996,"2,498,001","115,815,890"
1997,"2,445,534","115,815,890"
1998,"2,370,048","115,815,890"
1999,"2,449,664","115,815,890"
2000,"2,432,348","115,815,890"
2001,"2,217,779","115,815,890"
2002,"2,592,545","115,815,890"
2003,"2,458,792","115,815,890"
2004,"2,677,342","115,815,890"
2005,"2,586,665","115,815,890"
2006,"2,567,350","115,815,890"
2007,"2,657,281","115,815,890"
2008,"2,690,154","115,815,890"
2009,"2,735,402","115,815,890"
2010,"2,665,972","115,815,890"
2011,"2,825,505","115,815,890"
2012,"2,973,607","115,815,890"
2013,"2,807,387","115,815,890"
2014,"3,189,696","115,815,890"
2015,"3,648,846","115,815,890"
2016,"4,295,127","115,815,890"
2017,"4,504,812","115,815,890"
')

#Remove the comas from the data
zion$RecreationVisitors <- as.numeric(gsub(',','', zion$RecreationVisitors))

#Convert to millions of visitors
zion$RecreationVisitors <- zion$RecreationVisitors / 10^6

#Check the tail of the data
tail(zion)

#EDA 

#Plot the time series
ts.plot(zion$RecreationVisitors, type = "b", ylab = "Visitors (in millions)", xlab = "Years")


#Analysis
#Is the data additive or multiplicative?
#The curvature in the plot indicates that the data is multiplicative
# Since it is multiplicative, we will transofrm the data using the log function
#   to create an additive effect

zion$lnVisitors = log(zion$RecreationVisitors)


#Verify the effectiveness of the log transformation
ts.plot(zion$lnVisitors, type = "b", ylab = "Visitors (in millions)", xlab = "Years")

#If you look at the recent data, the data is linear

#Does the data show a constant mean change?
#Focus on the most recent chunk of data that represents recent circumstances

#In this example, around the 1950's is when cars became popular which is still the 
#   case today

zion1950 = subset(zion,Year > 1950)

#Verify effectiveness of the filter
ts.plot(zion1950$lnVisitors, type = "b", ylab = "Visitors (in millions)", xlab = "Years")

#Model : ARIMA (1,1,1)
#features: trend, long and short memory

# add the astsa library to use the correct ARIMA model
#install the library (download)

#install.packages("devtools")     # only need to do this once
#devtools::install_github("nickpoison/astsa")

library(astsa)

zion1950.out = sarima(zion1950$lnVisitors,1,1,1)
#Remember to confirm convergence of computation
zion1950.out$ttable

#It is interesting to see the statistically significant positive trend, 
# but it is information that you do not share. We calculates it for oursleves

#Predict the next 5 years
#predict the log visitors
zion1950_Future <- sarima.for(zion1950$lnVisitors, n.ahead = 5, 1, 1, 1)

#untransform
# Compute the 95% PREDICTION intervals
zion1950_Future_Lower = exp(zion1950_Future$pred - (qnorm(0.975) * zion1950_Future$se))
zion1950_Future_Upper = exp(zion1950_Future$pred + (qnorm(0.975) * zion1950_Future$se))

#Create the table including the prediction and the upper and lower prediction
# intervals
cbind(exp(zion1950_Future$pred),zion1950_Future_Lower,zion1950_Future_Upper)

#Create a graphic representing the predicted visitors (in millions) for the next 5 years.
# This plot includes the prediction intervals

#PLot the data that we already have, limiting the years from 1990 to 2020

plot(RecreationVisitors~Year, data = zion, type = "b",
     ylab = "Zion National Park Visitors (in millions)",
     xlim = c(1990, 2022), ylim = c(1, 6.7))

# These lines show the predictions with the prediction intervals

lines(2018:2022, exp(zion1950_Future$pred), col = "darkorange2", type = "b", pch = 19)
lines(2018:2022, zion1950_Future_Lower, col = "darkorange2", type = "b", lty = 2)
lines(2018:2022, zion1950_Future_Upper, col = "darkorange2", type = "b", lty = 2)


