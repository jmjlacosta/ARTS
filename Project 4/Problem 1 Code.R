df <- p1data
names(df) <- c("height_at_18", "length_at_birth", 
               "mother_ht_at_18", "father_ht_at_18", 
               "mater_grandma_ht_at_18",
               "mater_grandpa_ht_at_18", 
               "pater_grandma_ht_at_18", 
               "pater_grandpa_ht_at_18")
model <- lm(height_at_18 ~ length_at_birth + mother_ht_at_18 + 
              father_ht_at_18 + mater_grandma_ht_at_18 +
              mater_grandpa_ht_at_18 + pater_grandma_ht_at_18 + 
              pater_grandpa_ht_at_18, data=df)
model
summary(model)
influence.measures(model)
cat("Mean of hat/leverage values: ",mean(hatvalues(model)))
plot(height_at_18 ~ length_at_birth +
       mother_ht_at_18 +
       father_ht_at_18 +
       mater_grandma_ht_at_18 +
       mater_grandpa_ht_at_18 +
       pater_grandma_ht_at_18 +
       pater_grandpa_ht_at_18,
     data=df,cex=sqrt(cooks.distance(model)))
plot(fitted(model),resid(model))
plot(fitted(model),rstudent(model))
ggpairs(df)
