#Import Library
library(readr)

#Import Data
well_data = read_csv("~/sloping_slab/data/data_sloping_slab.csv")

#define watershed as factor
well_data$Watershed = as.factor(well_data$Watershed)

#Compute model
model_null = glm(Saturation ~ 1, data = well_data, family = binomial(link = "logit"))

model_full = glm(Saturation ~ TWI + Watershed, 
                 data = well_data, family = binomial(link = "logit"))

model_final = step(model_null, scope=list(lower=model_null, upper=model_full), dir="both", critereon = "AIC")

summary(model_final)

# Compute model lines with dummy data
# NFEC First
prediction_NFEC = data.frame(TWI = seq(min(well_data$TWI), max(well_data$TWI), 0.01), 
                             Watershed = "NFEC")

prediction_NFEC$Saturation = predict(model_final, newdata = prediction_NFEC ,type = "response")

#CW second
prediction_CW = data.frame(TWI = seq(min(well_data$TWI), max(well_data$TWI), 0.01), 
                             Watershed = "CW")

prediction_CW$Saturation = predict(model_final, newdata = prediction_CW ,type = "response")


plot(well_data$TWI, well_data$Saturation, pch = 19, col = well_data$Watershed)
lines(prediction_NFEC$TWI, prediction_NFEC$Saturation, col = "red")
lines(prediction_CW$TWI, prediction_CW$Saturation, col = "black")


#Put data into exporting data.frames
model_exports = data.frame(TWI = prediction_CW$TWI,
                           CW_Model = prediction_CW$Saturation,
                           NFEC_Model = prediction_NFEC$Saturation)

obs_data = data.frame(TWI = well_data$TWI,
                      Saturation_ratio = well_data$Saturation,
                      Watershed = well_data$Watershed)

write.csv(model_exports, "~/sloping_slab/output/model_predictions.csv")
write.csv(obs_data, "~/sloping_slab/output/obs_data.csv")
