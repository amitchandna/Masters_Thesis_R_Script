library(knitr)
library(agricolae)
library(knitLatex)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggpubr)


#Import all the data into environment
data_NPC <- read.csv(file.choose())
data_height <- read.csv(file.choose())
#Convert each variable for wheat trials into dataframes for analysis
df_N <- data.frame(data_NPC$Total_Nitrogen, data_NPC$Treatment)
df_N <- df_N %>% drop_na()
df_P <- data.frame(data_NPC$Total_Phosphorus, data_NPC$Treatment)
df_P <- df_P %>% drop_na()
df_C <- data.frame(data_NPC$Weight.of.Seeds, data_NPC$Treatment)
df_C <- df_C %>% drop_na()
#Convert variable for maize height into a dataframe
df_height <- data.frame(data_Height$Height, data_Height$Treatment)
df_height <- df_height %>% drop_na()
#Create a linear model from each dataframe
df_N_lm <- lm(data_NPC$Total_Nitrogen ~ data_NPC$Treatment, data = data_NPC)
df_C_lm <- lm(data_NPC$Weight.of.Seeds ~ data_NPC$Treatment, data = data_NPC)
df_P_lm <- lm(data_NPC$Total_Phosphorus ~ data_NPC$Treatment, data= data_NPC)
df_height_lm <- lm(data_Height$Height ~data_Height$Treatment, data=data_Height)
#ANOVA and Duncan test for Nitrogen in Wheat
summary(df_N_lm)
Nitrogen_ANOVA <- anova(df_N_lm)
Nitrogen_ANOVA
model_N <- aov(data_NPC.Total_Nitrogen~data_NPC.Treatment, data = df_N)
duncan_results_nitrogen <- duncan.test(model_N, alpha = 0.5,"group", main = "Plot of treatment against Nitrogen Success")
duncan_results_nitrogen
#ANOVA and Duncan test for Carbon in Wheat
summary(df_C_lm)
Carbon_ANOVA <- anova(df_C_lm)
Carbon_ANOVA
model_C <- aov(data_NPC.Weight.of.Seeds~data_NPC.Treatment, data = df_C)
duncan_results_carbon <- duncan.test(model_C,"group", main="Plot of treatment against Mass Accumulation")
duncan_results_carbon
#ANOVA and Duncan test for Phosphorus in Wheat
summary(df_P_lm)
Phosphorus_ANOVA <- anova(df_P_lm)
Phosphorus_ANOVA
model_P <- aov(data_NPC.Total_Phosphorus~data_NPC.Treatment, data = df_P)
duncan_results_phosphorus <- duncan.test(model_P, "group", main="Plot of treatment against Phosphorus Success")
duncan_results_phosphorus
#ANOVA and Duncan test for Height in Maize
summary(df_height_lm)
Height_ANOVA <- anova(df_height_lm)
Height_ANOVA
model_height <- aov(data_Height.Height~data_Height.Treatment, data = df_height)
duncan_results_height <- duncan.test(model_height, alpha = 0.1,main="PLot of treatment against height suceess in maize")
duncan_results_height
#plotting
Nitrogen_in_Wheat <- ggplot(df_N, aes(x=data_NPC.Treatment, y=data_NPC.Total_Nitrogen, fill=data_NPC.Treatment)) + geom_boxplot()+ggtitle("Nitrogen Accumulation in Wheat by Treatment") +
  xlab("Treatment") + ylab("Nitrogen (grams/square meter)")
Nitrogen_in_Wheat
Carbon_in_Wheat <- ggplot(df_C, aes(x=data_NPC.Treatment, y=data_NPC.Weight.of.Seeds, fill=data_NPC.Treatment)) + geom_boxplot()+ggtitle("Wheat of seeds (Wheat) by Treatment") +  
  xlab("Treatment") + ylab("Average Weight of Seeds(g)")
Carbon_in_Wheat
Phosphorus_in_Wheat <- ggplot(df_P, aes(x=data_NPC.Treatment, y=data_NPC.Total_Phosphorus, fill=data_NPC.Treatment)) + geom_boxplot()+ggtitle("Phosphorus Accumulation in Wheat by Treatment") +
  xlab("Treatment") + ylab("Phosphorus (grams/square meter)")
Phosphorus_in_Wheat
Maize_Height <- ggplot(df_height, aes(x=data_Height.Treatment, y=data_Height.Height, fill =data_Height.Treatment)) + geom_boxplot()+ggtitle("Height accumulation in Maize by Treatment") +
  xlab("Treatment") + ylab("Height (metres)")
Maize_Height

  
