# Project Goal

# Predict the expected MLB ERA+ of NPB pitchers based on their NPB performance and age and identify skill profiles most likely to succeed.

# Libraries & Load Data

library(dplyr)
library(forecast)
library(corrgram)
library(car)
library(lmtest)

baseball <- read.csv("NPB_MLB_Prediction.csv", header = TRUE)
head(baseball, 10)

# Filter Data & Select Variable

baseball_filter <- baseball[, -c(1:2,4)]
baseball_filter$MLB.ERA.PLUS <- as.numeric(baseball_filter$MLB.ERA.PLUS)

# TV Split

set.seed(666)

train_index <- sample(1:nrow(baseball_filter), 0.6 * nrow(baseball_filter))
valid_index <- setdiff(1:nrow(baseball_filter), train_index)

train_df <- baseball_filter[train_index, ]
valid_df <- baseball_filter[valid_index, ]

nrow(train_df)
nrow(valid_df)

corrgram(train_df)

# Model

model <- lm(MLB.ERA.PLUS ~ MLB.Debut.Age + IP + K. + BB. + HR., data = train_df)
summary(model)

vif(model)

shapiro.test(sample(train_df$MLB.ERA.PLUS))

bptest(model)

# Prediction

PL_2025_Pitcher <- read.csv("NPB_PL_2025.csv", header = TRUE)

PL_2025_Pitcher$K.  <- PL_2025_Pitcher$SO / PL_2025_Pitcher$BF
PL_2025_Pitcher$BB. <- PL_2025_Pitcher$BB / PL_2025_Pitcher$BF
PL_2025_Pitcher$HR. <- PL_2025_Pitcher$HR / PL_2025_Pitcher$BF
PL_2025_Pitcher <- PL_2025_Pitcher %>%
  rename(MLB.Debut.Age = Age)
PL_2025_Pitcher <- PL_2025_Pitcher %>%
  rename(IP = IP.)
PL_2025_Pitcher$MLB.Debut.Age <- as.numeric(PL_2025_Pitcher$MLB.Debut.Age)

PL_2025_Pitcher$Predicted_MLB_ERA_PLUS <- predict(model, newdata = PL_2025_Pitcher)

PL_2025_Pitcher$MLB_Ready <- ifelse(
  PL_2025_Pitcher$Predicted_MLB_ERA_PLUS > 100 & PL_2025_Pitcher$IP > 100, 1, 0
)

CL_2025_Pitcher <- read.csv("NPB_CL_2025.csv", header = TRUE)

CL_2025_Pitcher$K.  <- CL_2025_Pitcher$SO / CL_2025_Pitcher$BF
CL_2025_Pitcher$BB. <- CL_2025_Pitcher$BB / CL_2025_Pitcher$BF
CL_2025_Pitcher$HR. <- CL_2025_Pitcher$HR / CL_2025_Pitcher$BF
CL_2025_Pitcher <- CL_2025_Pitcher %>%
  rename(MLB.Debut.Age = Age)
CL_2025_Pitcher <- CL_2025_Pitcher %>%
  rename(IP = IP.)
CL_2025_Pitcher$MLB.Debut.Age <- as.numeric(CL_2025_Pitcher$MLB.Debut.Age)

CL_2025_Pitcher$Predicted_MLB_ERA_PLUS <- predict(model, newdata = CL_2025_Pitcher)

CL_2025_Pitcher$MLB_Ready <- ifelse(
  CL_2025_Pitcher$Predicted_MLB_ERA_PLUS > 100 & CL_2025_Pitcher$IP > 100, 1, 0
)

MLB_Ready_All <- rbind(PL_2025_Pitcher, CL_2025_Pitcher)
MLB_Ready <- subset(MLB_Ready_All, MLB_Ready == 1)

MLB_Ready <- MLB_Ready[, -c(1,4:17,19:35)]

MLB <- MLB_Ready[, -c(2:6)]
nrow(MLB)


MLB_Ready %>%
  summarise(
    Avg_Predicted_ERA_PLUS = mean(Predicted_MLB_ERA_PLUS),
    Max_Predicted_ERA_PLUS = max(Predicted_MLB_ERA_PLUS),
    Min_Predicted_ERA_PLUS = min(Predicted_MLB_ERA_PLUS),
    Avg_IP = mean(IP)
  )
library(ggplot2) 

ggplot(MLB, aes(x=reorder(Name, -Predicted_MLB_ERA_PLUS), y=Predicted_MLB_ERA_PLUS)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(title="Predicted MLB ERA+ for 2025 NPB MLB-Ready Pitchers",
       x="Player", y="Predicted MLB ERA+") +
  theme_minimal()

