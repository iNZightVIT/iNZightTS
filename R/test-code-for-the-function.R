## test code

######### 1.yearly  ###########
vardata = 1991:1996

get.ts.structure(vardata)
### 1.1starting with Y ###
vardata = paste("Y", 1992:1996, sep ="")
get.ts.structure(vardata)


######### 2. monthly ############

x = "M"
v1 = paste("1991",x, 7:12, sep="")
v2 = paste("1992",x, 1:12, sep ="" )
v3 = paste("1993",x, 1:12, sep ="" )
v4 = paste("1994",x, 1:4, sep = "")
vardata = c(v1,v2,v3,v4)

get.ts.structure(vardata)

#### 2.1  one year only ###
vardata = paste("1991","m", 7:9, sep="")
get.ts.structure(vardata)



######### 3.Quaterly ########


x = "Q"
v1 = paste("1991",x, 2:4, sep="")
v2 = paste("1992",x, 1:4, sep ="" )
v3 = paste("1993",x, 1:4, sep ="" )
v4 = paste("1994",x, 1:2, sep = "")
vardata = c(v1,v2,v3,v4)

get.ts.structure(vardata)


#### 3.1  one year only ###
vardata = paste("1991","Q", 2:4, sep="")
get.ts.structure(vardata)





####### 4.daily with yearly seasonality#######
## "1993D03
x = "D"
v1 = paste("1991",x, 100:365, sep="")
v2 = paste("1992",x, 1:366, sep ="" )
v3 = paste("1993",x, 1:365, sep ="" )
v4 = paste("1994",x, 1:200, sep = "")
vardata = c(v1,v2,v3,v4)

get.ts.structure(vardata)



#### 4.1 one year only###
vardata = paste("1991","D", 100:265, sep="")

get.ts.structure(vardata)



######## 5. daily with weekly seasonality ##########
x = "W"
y = "D"
v1 = paste(x,1,y, 2:7 , sep="")
v2 = paste(x,2,y, 1:7, sep ="" )
v3 = paste(x,3,y, 1:5, sep ="" )
vardata = c(v1,v2,v3)

get.ts.structure(vardata)


##### 5.1 in one week only

vardata = paste(x, 1, y , 3:7, sep="")

get.ts.structure(vardata)



########## 6. hourly with daily seasonality##########


x = "D"
y = "H"
v1 = paste(x,1,y, 18:24 , sep="")
v2 = paste(x,2,y, 1:24, sep ="" )
v3 = paste(x,3,y, 1:5, sep ="" )
vardata = c(v1,v2,v3)

get.ts.structure(vardata)


####6.1 one day only###
vardata = paste(x, 1, y , 3:17, sep="")
get.ts.structure(vardata)






























