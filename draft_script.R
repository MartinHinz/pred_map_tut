library(c14bazAAR)
library(magrittr)
library(mapview)
library(sp)
library(raster)

neolith <- get_RADON()

neolith %<>%
  standardize_country_name() %>%
  determine_country_by_coordinate()

neolith %<>%
  dplyr::filter(country_coord == "Switzerland")

neolith %<>%
  dplyr::filter(sitetype == "settlement")

neolith %<>% dplyr::rename(x = lon, y = lat) 

coordinates(neolith) <- ~x+y

proj4string(neolith)<- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 

mapview(neolith)


bronze <- get_RADONB()

bronze %<>%
  standardize_country_name() %>%
  determine_country_by_coordinate()

bronze %<>%
  dplyr::filter(country_coord == "Switzerland")

bronze %<>%
  dplyr::filter(sitetype == "settlement")

bronze %<>% dplyr::rename(x = lon, y = lat) 

coordinates(bronze) <- ~x+y

proj4string(bronze)<- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 

mapview(bronze)

switzerland <- getData('alt', country='CHE', level=0)

mapview(switzerland)

x <- terrain(switzerland, opt = c("slope", "aspect", "tpi"), unit = 'degrees')
mapview(x$aspect)

# transform to directions
x$aspect <- ceiling((x$aspect + 360/8/2) / (360/8))
x$aspect[x$aspect>8]<-1

values(x$aspect) <- factor(values(x$aspect))

summary(x$aspect)
mapview(x$aspect)

write.csv(unique(as.data.frame(neolith)[,c("site", "y", "x")]), file="data/sites_neolithic.csv", row.names = F)

neolith_sp <- raster::extract(x, neolith, sp=T)
mapview(neolith_sp)

random <- as.data.frame(sampleRandom(x, 1000, xy=T))

coordinates(random)<-~x+y

proj4string(random) <- proj4string(switzerland)

mapview(random)

# GLM
mydata <- rbind(data.frame(neolith_sp[,c("tpi", "slope", "aspect")], origin="neolith"),
      data.frame(random[,c("tpi", "slope", "aspect")], origin="random"))

mydata <- na.omit(mydata)

fit <- glm(origin~tpi+slope+aspect, data=mydata,family=binomial())

summary(fit)
confint(fit)

pcdata <- predict(fit, type="response")

comp <- data.frame(pred = as.logical(pcdata<0.5),
                   orig = as.logical(mydata$origin=="neolith"))

table(comp)

pdata <- 1- predict(fit, newdata = as.data.frame(x), type = "response")

boxplot(pdata)

x_pred <- x
x_pred$pred <- pdata
mapview(x_pred$pred) + neolith_sp

# mfp

library(mfp)

mylogit<- mfp(origin~fp(tpi)+fp(slope)+as.factor(aspect), data=mydata, family=binomial ,na.action=na.pass, verbose=T)

pdata<-predict(mylogit,newdata=mydata,type="response")


comp <- data.frame(pred = as.logical(pdata<0.5),
                   orig = as.logical(mydata$origin=="neolith"))

table(comp)

pdata <- 1- predict(fit, newdata = as.data.frame(x), type = "response")

boxplot(pdata)

x_pred <- x
x_pred$pred <- pdata
mapview(x_pred$pred) + neolith_sp

# quantile
test_data<-mydata
sites<-mydata[mydata$origin=="neolith",]
nonsites<-mydata[mydata$origin=="random",]
sites$P<-1-predict(mylogit,newdata=sites,type="response")
nonsites$P<-1-predict(mylogit,newdata=nonsites,type="response")

squan <- as.vector(quantile(sites$P))
nquan <- as.vector(quantile(nonsites$P))

boxplot(P~origin,data=rbind(sites,nonsites))

# cutpoint

library(cutpointr)

cutpoint <- cutpointr(rbind(sites,nonsites), P, origin, 
          method = maximize_metric, metric = sum_sens_spec)

x_pred <- x
x_pred$pred <- pdata>cutpoint$optimal_cutpoint
mapview(x_pred$pred) + neolith_sp


# gain

#GAIN-Berechnung

sites_predicted <- raster::extract(x_pred, neolith, sp=T)

n_sites_correct_predicted <- sum(sites_predicted$pred, na.rm=T)

n_sites <- sum(!is.na(sites_predicted$pred))

richtigesiedlungen <- n_sites_correct_predicted/n_sites * 100

gain <- na.omit(pdata)
siedl <- length(gain[gain >= cutpoint$optimal_cutpoint]) #Fläche die über dem Grenzwert liegt
fl <- length(pdata) # Gesamtfläche
proz <- siedl / fl *100 # klassifizierte Fläche in Prozent
g <- 1-(proz/richtigesiedlungen) #GAIN-Wert
g

# bayesian

library(e1071)
model_bayes <- naiveBayes(site~tpi+slope+aspect, data=evidence)

model_bayes
pcdata <- predict(model_bayes, newdata = evidence)

comp <- data.frame(pcdata,evidence$site)

table(comp)

env_data.df <- as.data.frame(env_data)

env_data.df$aspect <- factor(env_data.df$aspect)
str(env_data.df)

pdata_b <- predict(model_bayes, newdata = env_data.df, type = "raw")

summary(pdata_b)

x_pred_b <- env_data
x_pred_b$pred <- pdata_b[,"TRUE"]
mapview(x_pred_b$pred)

# optimistic cutoff

x_pred_b <- env_data
x_pred_b$pred <- F
x_pred_b$pred <- as.numeric(pdata_b[,"TRUE"]>=0.25)
mapview(x_pred_b$pred) + neolith_sp

plot(x_pred_b$pred)

sum(raster::extract(x_pred_b$pred, sites))/length(sites)
sum(raster::extract(x_pred_b$pred, nonsites))/length(nonsites)


# pessimistic cutoff

x_pred_b <- x
x_pred_b$pred <- F
x_pred_b$pred <- as.numeric(pdata_b[,1]<=0.05)
mapview(x_pred_b$pred) + neolith_sp


writeRaster(switzerland, "data/dem_switzerland")


plot(dem_utm)

plot(sites_utm, pch=19, add=TRUE)
