install.packages('ggplot2')
install.packages("lme4")
#install.packages("afex")

library(ggplot2)
library(lme4)
#library(afex)

# read excel data in r

library(readxl)
df<-read_excel("/input/Input - linear mixed model in R/Human Conciousness - Raw data.xlsx")

head(df)

#  only use id, sex, age, year, faculty , emotion to create new data frame

data <- df[,c("Sex","Age","Year","Faculty","Imagination","Emotion")]
head(data)

summary(data)

# convert faculty, sex , year in factor

data$Faculty <- factor(data$Faculty)
data$Sex <- factor(data$Sex)
data$Year <- factor(data$Year)

#  recode faculty level with  eadh number such as 1 become Faculty1

data$Faculty <- as.factor(data$Faculty)
levels(data$Faculty) <- paste0("Faculty", levels(data$Faculty))

table(data$Faculty)

str(data)

# run a simple regression wherer emotion is dependent and sex age year faculty imagination is independent

model0 <- lm(Emotion ~ Sex + Age + Year + Faculty + Imagination, data = data)
summary(model0)

plot(model0,3)


# emotions by imagination across faculties
options(repr.plot.width=15, repr.plot.height=8)
ggplot(data = data, aes(x = Imagination, y = Emotion, color = Faculty)) +
  geom_point() +
  facet_wrap(~ Faculty)+ theme(plot.title = element_text(size = 26, hjust = 0.5))+
  labs(title = "Emotion by Imagination across Faculties",
       x = "Imagination",
       y = "Emotion")


#  intercept only model for emotion as dependent and faculty cluster variable

model1 <- lmer(Emotion ~ 1 + (1 | Faculty), data = data)
summary(model1)

plot(model1)

# create a random intercept model

model2 <- lmer(Emotion ~ 1 + Imagination + (1 | Faculty), data = data)
summary(model2)

plot(model2)

#  compare model 1 and model2

anova(model2,model1, refit = FALSE)

# create random slope model

model3 <- lmer(Emotion ~ 1 + Imagination + (1 + Imagination | Faculty), data = data)
summary(model3)

plot(model3)

# compare model2 and model3

anova(model2,model3, refit = FALSE)

#
model4 <- lmer(Emotion ~ 1 + Sex +  Imagination + (1 + Sex +  Imagination | Faculty) , data = data)
summary(model4)

model4 <- lmer(Emotion ~ 1 + Sex +  Imagination + (1 + Sex +  Imagination | Faculty) ,control = lmerControl(optimizer = "bobyqa"), data = data)
summary(model4)

plot(model4)

# compare model 4 and model 3

anova(model4,model3, refit = FALSE)
coef(model3)

