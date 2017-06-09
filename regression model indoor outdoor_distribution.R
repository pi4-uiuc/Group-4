## outdoor height regression model 
## regression model for plant_height
co=c()
for(culti in unique(plant_height1$cultivar_id)){
  a <- lm(mean~date,data=subset(plant_height1,cultivar_id ==culti))
  co=c(co,a$coefficients)
}

coeff_plant_height = c()
for(i in 1:length(co)){
  coeff_plant_height <-c(co[i*2],coeff_plant_height)
}

library(ggplot2)
par(mfrow=c(2,1))
qplot(coeff_plant_height)
qplot(coeff_plant_height,geom="density")


##regression model for height
co_height=c()
for(cultii in unique(height1$cultivar_id)){
  model_height <- lm(mean~date,data=subset(height1, cultivar_id==cultii))
  co_height =c(co_height,model_height$coefficients)
}

coeff_height = c()
for(i in 1:173){
  coeff_height <-c(co_height[i*2],coeff_height)
}

##plot of outdoor growth rate
library(ggplot2)
par(mfrow=c(2,1))
qplot(coeff_height)
qplot(coeff_height,geom="density")

#model <- lm(mean~date, data=subset(height_nona,cultivar_id=unique()))




