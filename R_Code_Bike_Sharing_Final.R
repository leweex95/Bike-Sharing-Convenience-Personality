rm(list=ls())

# ------------------------------------------------------------------------------------------------------------------
# Levi's header, please do not delete, just comment out :)

dir               <- "C:/Users/csibi/Google Drive/Synch/RSM EUR MiM/0102_Research Methods/Big project/"
setwd(dir)
dirData           <- paste0(dir, "RAW data only variables 3.csv")
dirOutputs        <- paste0(dir,"Outputs/")
dirHistograms     <- paste0(dirOutputs,"Histograms/")
dirScatterPlots   <- paste0(dirOutputs,"ScatterPlots/")
dirTables         <- paste0(dirOutputs,"Tables/")
dirTablesCronbach <- paste0(dirTables,"Cronbach/")
dirTablesFtests   <- paste0(dirTables,"Ftests/")
dirTablesRegClass <- paste0(dirTables,"Regressions and Classifications/")
dirTrees          <- paste0(dirOutputs,"Trees/")
dirROCs           <- paste0(dirOutputs,"ROCs/")
dsBikeSharing     <- read.csv2(dirData, sep = ",", stringsAsFactors = FALSE)

# -------------------------------------------------------------------------------------------------------------------

# Turn characters to numeric
iii <- 1
for (iii in 1:ncol(dsBikeSharing)) {
  dsBikeSharing[, iii] <- as.numeric(dsBikeSharing[, iii])
}
rm(iii)

# -------------------------------------------------------------------------------------------------------------------
# DESCRIPTIVE ANALYSIS
# -------------------------------------------------------------------------------------------------------------------

# install.packages("psych", dependencies = TRUE)
library(psych)
# install.packages("stargazer", dependencies = TRUE)
library(stargazer)

getmode <- function(v) {
  uniqv <- unique(v)
  as.numeric(uniqv[which.max(tabulate(match(v, uniqv)))])
}

iii <- 1
mode   <- integer(ncol(dsBikeSharing))

for (iii in 1:ncol(dsBikeSharing)) {
  mode[iii]   <- as.numeric(getmode(dsBikeSharing[,iii]))
}
rm(iii)
rm(getmode)

# Mean, standard dev, median, min, max and mode
# Note: Mode is not added by stargazer output
# This output below adds the mode
descriptive.statistics <- cbind(describe(dsBikeSharing)[c(3,4,5,8,9)], mode)
capture.output(descriptive.statistics, file = paste0(dirTables, "Descriptive Summary - Bike Sharing Preference with mode.txt"))

# stargazer(dsBikeSharing,
#          title = "Descriptive Summary Bike Sharing Preference",
#          type = "text",
#          out = paste(dirTables, "Descriptive Summary.tex"))

stargazer(dsBikeSharing, 
          title = "Descriptive Summary Bike Sharing Preference", 
          out = paste(dirTables, "Descriptive Summary - Bike Sharing Preference.doc"),
          type = "html")


# -------------------------------------------------------------------------------------------------------------------
# VISUALISATION - HISTOGRAMS
# -------------------------------------------------------------------------------------------------------------------

# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
library(psych)

# A general template for plotting histograms
# ggplot(dsBikeSharing, aes(x = WorkHours)) +
#   geom_histogram(binwidth = 10,
#                  fill = "red",
#                  col = "black") +
#   xlab("blablabla") +
#   scale_x_continuous(breaks = seq(0,80, by = 10)) +
#   ylab("blablabla") +
#   scale_y_continuous() +
#   ggtitle("Title")
# ggsave(paste0(dirHistograms, "Variable histogram general.pdf"))
# dev.off()

# Gender
ggplot(dsBikeSharing, aes(x = Gender)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous(breaks = seq(1,2, by = 1), labels = c("Male", "Female")) +
  ylab("Count") +
  ggtitle("Histogram of respondents' gender")
ggsave(paste0(dirHistograms, "Histogram - Gender.pdf"))
dev.off()

# Age
ggplot(dsBikeSharing, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  xlab("Age") + scale_x_continuous(breaks = seq(18, 68, by = 5)) +
  ylab("Count") +
  ggtitle("Histogram of respondents' age")
ggsave(paste0(dirHistograms, "Histogram - Age.pdf"))
dev.off()

# Current country
ggplot(dsBikeSharing, aes(x = Country)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Current country", breaks = seq(1,13, by=1),
                     labels = c("Netherlands", "UK", "France", "Italy", "Hungary", "Germany", "Switzerland", "Sweden", "Turkey", "Greece", "Canada", "Lithuania", "Spain")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous("Count", breaks = seq(0,60, by = 5)) +
  ggtitle("Current country histogram")
ggsave(paste0(dirHistograms, "Histogram - Current country.pdf"))
dev.off()

# Country of origin
ggplot(dsBikeSharing, aes(x = Origin)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Country of origin", breaks = seq(1,17, by=1),
                     labels = c("Netherlands", "UK", "France", "Italy", "Hungary", "China", "Germany", "Greece", "Israel", "India", "Portugal", "Poland", "Zimbabwe", "Peru", "Turkey", "Lithuania", "Spain")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous("Count", breaks = seq(0,60, by = 5)) +
  ggtitle("Country of origin histogram")
ggsave(paste0(dirHistograms, "Histogram - Country of origin.pdf"))
dev.off()

# Job
ggplot(dsBikeSharing, aes(x = Job)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Do you have a job?", breaks = seq(1,2, by=1), labels = c("No", "Yes")) +
  scale_y_continuous("Count") +
  ggtitle("Employment status histogram")
ggsave(paste0(dirHistograms, "Histogram - Job.pdf"))
dev.off()

# Job type
ggplot(dsBikeSharing, aes(x = Jobtype)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Current form of occupation", breaks = seq(1,7,by=1),
                     labels = c("Student", "Working Student", "Intern", "Fulltime employed", "Parttime employed", "Unemployed", "Part employed part self employed")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous("Count") +
  ggtitle("Form of occupation histogram")
ggsave(paste0(dirHistograms, "Histogram - Job type.pdf"))
dev.off()

# Work hours
ggplot(dsBikeSharing, aes(x = WorkHours)) +
  geom_histogram(binwidth = 10, fill = "red", col = "black") +
  scale_x_continuous("Weekly working hours", breaks = seq(0,80,by=10)) +
  scale_y_continuous("Count") +
  ggtitle("Weekly working hours histogram")
ggsave(paste0(dirHistograms, "Histogram - Work hours.pdf"))
dev.off()

# Transport
ggplot(dsBikeSharing, aes(x = Transport)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Type of transport used to go to work/school", breaks = seq(1,4,by=1),
                     labels = c("Bike", "Car", "Public","Walking")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous("Count") +
  ggtitle("Type of transport histogram")
ggsave(paste0(dirHistograms, "Histogram - Transport type.pdf"))
dev.off()

# Commute distance
ggplot(dsBikeSharing, aes(x = CommuteDist)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Commute distance", breaks = seq(1,4,by=1),
                     labels = c("Less than 1 km", "1-5 km", "5-10 km", "More than 10 km")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous("Count") +
  ggtitle("Commute distance histogram")
ggsave(paste0(dirHistograms, "Histogram - Commute distance.pdf"))
dev.off()

# Bike ownership
ggplot(dsBikeSharing, aes(x = Bikeown)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Bike ownership", breaks = seq(1,2,by=1),
                     labels = c("No", "Yes")) +
  scale_y_continuous("Count") +
  ggtitle("Bike ownership histogram")
ggsave(paste0(dirHistograms, "Histogram - Bike ownership.pdf"))
dev.off()

# Car ownership
ggplot(dsBikeSharing, aes(x = Car)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Bike ownership", breaks = seq(1,2,by=1),
                     labels = c("No", "Yes")) +
  scale_y_continuous("Count") +
  ggtitle("Car ownership histogram")
ggsave(paste0(dirHistograms, "Histogram - Car ownership.pdf"))
dev.off()

# Bike lease usage
ggplot(dsBikeSharing, aes(x = Bikelease)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Bike lease usage", breaks = seq(1,2,by=1),
                     labels = c("No", "Yes")) +
  scale_y_continuous("Count") +
  ggtitle("Bike lease usage histogram")
ggsave(paste0(dirHistograms, "Histogram - Bike lease.pdf"))
dev.off()

# Bike sharing usage
ggplot(dsBikeSharing, aes(x = Bikeshare)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Bike lease usage", breaks = seq(1,2,by=1),
                     labels = c("No", "Yes")) +
  scale_y_continuous("Count") +
  ggtitle("Bike share usage histogram")
ggsave(paste0(dirHistograms, "Histogram - Bike share.pdf"))
dev.off()

# Sharing location preference
ggplot(dsBikeSharing, aes(x = SharingLocPref)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Sharing location preference", breaks = seq(1,3,by=1),
                     labels = c("On the street", "At the depot", "Both")) +
  scale_y_continuous("Count") +
  ggtitle("Sharing location preference")
ggsave(paste0(dirHistograms, "Histogram - Sharing location preference.pdf"))
dev.off()

# Depot distance
ggplot(dsBikeSharing, aes(x = DepotDist)) +
  geom_histogram(binwidth = 10, fill = "red", col = "black") +
  scale_x_continuous("Depot distance", breaks = seq(0,100,by=10)) +
  scale_y_continuous("Count") +
  ggtitle("Distance of nearest depot to house")
ggsave(paste0(dirHistograms, "Histogram - Depot distance.pdf"))
dev.off()

# Depot in station
ggplot(dsBikeSharing, aes(x = DepotInStation)) +
  geom_histogram(binwidth = 1, fill = "red", col = "black") +
  scale_x_continuous("Depot in public transport station", breaks = seq(1,2,by=1), labels = c("No", "Yes")) +
  scale_y_continuous("Count") +
  ggtitle("Depot in station histogram")
ggsave(paste0(dirHistograms, "Histogram - Depot distance.pdf"))
dev.off()

# -------------------------------------------------------------------------------------------------------------------
#  ALPHAS AND HISTOGRAMS
# -------------------------------------------------------------------------------------------------------------------
library(psych)
# Openness calc with original 4 items
RawOpenness <- psych::alpha(dsBikeSharing[c("Openness01", "Openness02", "Openness03", "Openness04")], keys = c(1,1,1,-1))
capture.output(RawOpenness$total[1], RawOpenness$alpha.drop, file = paste0(dirTablesCronbach, "Raw Openness01-04 Output.txt"))

# New Openness calc after removal of Openness04
RawOpenness <- psych::alpha(dsBikeSharing[c("Openness01", "Openness02", "Openness03")])
capture.output(RawOpenness$total[1], RawOpenness$alpha.drop, file = paste0(dirTablesCronbach, "Raw Openness01-03 Output.txt"))

# Adding Openness column to dataset (cumulative = FALSE --> average)
dsBikeSharing$Openness <-
  psych::alpha(dsBikeSharing[c("Openness01", "Openness02", "Openness03")], cumulative = FALSE)$scores
ggplot(dsBikeSharing, aes(x = Openness)) +
  geom_histogram(binwidth = 1,
                 fill = "red",
                 col = "black") +
  xlab("Openness score") +
  ylab("Respondents") +
  ggtitle("Openness")
ggsave(paste0(dirHistograms, "Histogram - Openness.pdf"))
dev.off()


# Neuroticism calc with original 3 items
RawNeuroticism <- psych::alpha(dsBikeSharing[c("Neuroticism01", "Neuroticism02", "Neuroticism03")], keys = c(-1,1,1))
capture.output(RawNeuroticism$total[1], RawNeuroticism$alpha.drop, file = paste0(dirTablesCronbach, "Raw Neuroticism 01-03 Output.txt"))

# Adding Neuroticism column to dataset (cumulative = FALSE --> average)
dsBikeSharing$Neuroticism <-
  psych::alpha(dsBikeSharing[c("Neuroticism01", "Neuroticism02", "Neuroticism03")], keys = c(-1,1,1), cumulative = FALSE)$scores
ggplot(dsBikeSharing, aes(x = Neuroticism)) +
  geom_histogram(binwidth = 1,
                 fill = "red",
                 col = "black") +
  xlab("Neuroticism score") +
  ylab("Respondents") +
  ggtitle("Neuroticism")
ggsave(paste0(dirHistograms, "Histogram - Neuroticism.pdf"))
dev.off()

# Neighbourhood calc with original 2 items
RawNeighbourhood <- psych::alpha(dsBikeSharing[c("Neighbourhood01", "Neighbourhood02")])
capture.output(RawNeighbourhood$total[1], RawNeighbourhood$alpha.drop, file = paste0(dirTablesCronbach, "Raw Neighbourhood 01-02 Output.txt"))
# Cronbach's alpha is very low (0.42), therefore there is no point in keeping the double
# Neighbourhood02 is removed

# Neuroticism calc after removal of Neuroticism02
ggplot(dsBikeSharing, aes(x = Neighbourhood02)) +
  geom_histogram(binwidth = 1,
                 fill = "red",
                 col = "black") +
  xlab("Neighbourhood affluence score") +
  ylab("Respondents") +
  ggtitle("Neighbourhood affluence")
ggsave(paste0(dirHistograms, "Histogram - Neighbourhood affluence.pdf"))
dev.off()


# Preference for bike sharing calc with original 6 items
RawBikeSharePref <- psych::alpha(dsBikeSharing[c("Sharing01", "Sharing02", "Sharing03", "Sharing04", "Sharing05", "Sharing06")])
capture.output(RawBikeSharePref$total[1], RawBikeSharePref$alpha.drop, file = paste0(dirTablesCronbach, "Raw Bike Sharing Preference All Items.txt"))

# Preference for bike sharing calc after removal of Sharing05
RawBikeSharePref <- psych::alpha(dsBikeSharing[c("Sharing01", "Sharing02", "Sharing03", "Sharing04", "Sharing06")])
capture.output(RawBikeSharePref$total[1], RawBikeSharePref$alpha.drop, file = paste0(dirTablesCronbach, "Raw Bike Sharing Preference Without 05.txt"))

# Preference for bike sharing calc after removal of Sharing06
RawBikeSharePref <- psych::alpha(dsBikeSharing[c("Sharing01", "Sharing02", "Sharing03", "Sharing04")])
capture.output(RawBikeSharePref$total[1], RawBikeSharePref$alpha.drop, file = paste0(dirTablesCronbach, "Raw Bike Sharing Preference Without 05,06.txt"))

# Preference for bike sharing calc after removal of Sharing01
RawBikeSharePref <- psych::alpha(dsBikeSharing[c("Sharing02", "Sharing03", "Sharing04")])
capture.output(RawBikeSharePref$total[1], RawBikeSharePref$alpha.drop, file = paste0(dirTablesCronbach, "Raw Bike Sharing Preference Without 01,05,06.txt"))

# Adding Preference for bike sharing column to dataset (cumulative = FALSE --> average)
dsBikeSharing$Sharing <- psych::alpha(dsBikeSharing[c("Sharing02", "Sharing03", "Sharing04")], cumulative = FALSE)$scores
ggplot(dsBikeSharing, aes(x = Sharing)) +
  geom_histogram(binwidth = 1,
                 fill = "red",
                 col = "black") +
  scale_x_continuous("Preference score for bike sharing",
                     breaks = seq(1,7, by = 1), labels = c("Strongly disagree", "Disagree", "Mildly disagree",
                                                           "Neither agree nor disagree", "Mildly agree", "Agree", "Strongly agree")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Respondents") +
  ggtitle("Bike sharing willingness")
ggsave(paste0(dirHistograms, "Histogram - Preference for bike sharing 03-04.pdf"))
dev.off()

# -------------------------------------------------------------------------------------------------------------------
# VISUALISATION - SCATTER PLOTS
# -------------------------------------------------------------------------------------------------------------------

ggplot(dsBikeSharing, aes(x = Openness, y = Sharing)) +
  geom_point(col = "red") +
  xlab("Openness") +
  ylab("Preference for bike sharing") +
  ggtitle("Effect of openness on bike sharing preference") +
  geom_smooth(alpha=0.10, color="black", fill="black")
ggsave(paste0(dirScatterPlots, "Scatter plot Openness - Sharing.pdf"))
dev.off()

ggplot(dsBikeSharing, aes(x = Neuroticism, y = Sharing)) +
  geom_point(col = "red") +
  xlab("Neuroticism") +
  ylab("Preference for bike sharing") +
  ggtitle("Effect of neuroticism on bike sharing preference") +
  geom_smooth(alpha=0.10, color="black", fill="black")
ggsave(paste0(dirScatterPlots, "Scatter plot Neuroticism - Sharing.pdf"))
dev.off()

ggplot(dsBikeSharing, aes(x = Neighbourhood01, y = Sharing)) +
  geom_point(col = "red") +
  xlab("Neighbourhood") +
  ylab("Preference for bike sharing") +
  ggtitle("Effect of neighbourhood on bike sharing preference") +
  geom_smooth(alpha=0.10, color="black", fill="black")
ggsave(paste0(dirScatterPlots, "Scatter plot Affluence - Sharing.pdf"))
dev.off()

# -------------------------------------------------------------------------------------------------------------------
# SUMMARY TABLES
# -------------------------------------------------------------------------------------------------------------------

stargazer(dsBikeSharing, 
          title = "Descriptive Summary Bike Sharing Preference", 
          out = paste(dirTables, "Descriptive Summary - Bike Sharing Preference FULL.doc"),
          type = "html")

options(max.print=1000000)
# dsBikeSharing[-c(15,16,17,18,19,20,24,25,26,27,28,29,30,32)]
capture.output(cor(dsBikeSharing[-c(15,16,17,18,19,20,24,25,26,27,28,29,30,32)], method = c("pearson")),
               file = paste0(dirTables, "Correlation Table.txt"))

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson"), removeTriangle=c("upper"), result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type= "pearson")
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

capture.output(corstars(as.matrix(dsBikeSharing[-c(15,16,17,18,19,20,24,25,26,27,28,29,30,32)])),
               file = paste0(dirTables, "Correlation Table With Significances.txt"))





# -------------------------------------------------------------------------------------------------------------------
# FACTORISATION OF VARIABLES
# -------------------------------------------------------------------------------------------------------------------

dsBikeSharing$f.Gender        <- factor(dsBikeSharing$Gender,
                                        labels = c("Male", "Female"))
dsBikeSharing$f.Country       <- factor(dsBikeSharing$Country,
                                        labels = c("Netherlands", "UK", "Italy", "Hungary", "Germany", "Switzerland", "Sweden", "Turkey", "Greece", "Canada", "Lithuania", "Spain"))
dsBikeSharing$f.Origin        <- factor(dsBikeSharing$Origin,
                                        labels = c("Netherlands", "UK", "France", "Italy", "Hungary", "China", "Germany", "Greece", "Israel", "India", "Portugal", "Poland", "Zimbabwe", "Peru", "Turkey", "Lithuania", "Spain"))
dsBikeSharing$f.Job           <- factor(dsBikeSharing$Job,
                                        labels = c("Yes", "No"))
dsBikeSharing$f.Jobtype       <- factor(dsBikeSharing$Jobtype, 
                                        labels = c("Student", "Working Student", "Intern", "Full-time", "Part-time", "Unemployed", "Self-employed and Part-time"))
dsBikeSharing$f.Transport     <- factor(dsBikeSharing$Transport,
                                        labels = c("Bike", "Car", "Public","Walking"))
dsBikeSharing$f.Commutedist   <- factor(dsBikeSharing$Commute,
                                        labels = c("Less than 1 km", "1-5 km", "5-10 km", "More than 10 km"))
dsBikeSharing$f.Bikeown       <- factor(dsBikeSharing$Bikeown,
                                        labels = c("No", "Yes"))
dsBikeSharing$f.Car           <- factor(dsBikeSharing$Car,
                                        labels = c("No", "Yes"))
dsBikeSharing$f.Bikelease     <- factor(dsBikeSharing$Bikelease,
                                        labels = c("No", "Yes"))
dsBikeSharing$f.SharingLocPref<- factor(dsBikeSharing$SharingLocPref,
                                        labels = c("On the street", "At the depot","Both"))
dsBikeSharing$f.DepotInStation<- factor(dsBikeSharing$DepotInStation,
                                        labels = c("No", "Yes"))

# -------------------------------------------------------------------------------------------------------------------
# CREATING BINARY TARGET VARIABLE: ABOVE- OR BELOW-MEDIAN PREFERENCE FOR SHARING
# -------------------------------------------------------------------------------------------------------------------

medianSharingPref <- median(dsBikeSharing$Sharing)
dsBikeSharing$binSharing <- ifelse(dsBikeSharing$Sharing>=medianSharingPref,1,0)

# -------------------------------------------------------------------------------------------------------------------
# SPLIT DATA INTO TRAINING AND HOLDOUT SET
# -------------------------------------------------------------------------------------------------------------------

set.seed(123)
smpl <- sample(1:nrow(dsBikeSharing),ceiling(0.7*nrow(dsBikeSharing)))
smpl <- sort(smpl)

# Split the sample into training and test sets
dsBikeSharing.Train <- dsBikeSharing[smpl,]
dsBikeSharing.Test  <- dsBikeSharing[-smpl,]


# -------------------------------------------------------------------------------------------------------------------
# LINEAR REGRESSION MODELS
# -------------------------------------------------------------------------------------------------------------------

# First model with only variables of the hypothesis
Mdl.R <- Sharing ~ f.Bikeown + Income + Neighbourhood01 + DepotDist + Neuroticism + Openness + Income:f.Bikeown
Rslt.LinReg.R <- lm(Mdl.R, data = dsBikeSharing)

# First 1.2 model with reversed interaction direction
Mdl.R.RevInteract <- Sharing ~ f.Bikeown + Income + Neighbourhood01 + DepotDist + Neuroticism + Openness + f.Bikeown:Income
Rslt.LinReg.R.RevInteract <- lm(Mdl.R.RevInteract, data = dsBikeSharing)

# stargazer(Rslt.LinReg.R, Rslt.LinReg.R.RevInteract,
#           align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
#           type = "html", 
#           title = "Comparison of Original and Reversed Interaction Direction in Linear Regression of Restricted Model", 
#           out=paste(dirTablesRegClass, "Linear Regression - Restricted, Interaction Reversed.doc"))

# Second model with only "control variables" (i.e. variables that are not in first but are in third)
Mdl.C <- Sharing ~  + f.Gender + Age + f.Job + f.Jobtype + WorkHours + f.Transport + f.Commutedist + f.Car + f.Bikelease + f.SharingLocPref + f.DepotInStation
Rslt.LinReg.C <- lm(Mdl.C, data = dsBikeSharing)

# Third model with all variables except for country-related ones (origin and birth)
Mdl.U <- Sharing ~ f.Bikeown + Income + Neighbourhood01 + DepotDist + Neuroticism + Openness + Income:f.Bikeown + f.Gender + Age + f.Job + f.Jobtype + WorkHours + f.Transport + f.Commutedist + f.Car + f.Bikelease + f.SharingLocPref + f.DepotInStation
Rslt.LinReg.U <- lm(Mdl.U, data = dsBikeSharing)

stargazer(Rslt.LinReg.R, Rslt.LinReg.C, Rslt.LinReg.U,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type = "html", 
          title = "Linear regression results of the restricted, control and unrestricted models", 
          out=paste(dirTablesRegClass, "Linear Regression Results Restricted, Control and Unrestricted Models.doc"))

# Evidence for multicollinearity
library(car)
rbind(Restricted = vif(Rslt.LinReg.R),
      Control = vif(Rslt.LinReg.C)[,1],
      Unrestricted = vif(Rslt.LinReg.U)[,1])

stargazer(vif(Rslt.LinReg.R), vif(Rslt.LinReg.C)[,1], vif(Rslt.LinReg.U)[,1],
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type = "html", 
          title = c("Multicollinearity Effects in the Restricted Model", 
                    "Multicollinearity Effects among the Control Variables", 
                    "Multicollinearity Effects in the Unrestricted Model"), 
          out=paste(dirTablesRegClass, "Multicollinearity Effects in the Restricted, Control and Unrestricted Models.doc"))

# -------------------------------------------------------------------------------------------------------------------
# LINEAR REGRESSION, LOGISTIC (LOGIT) REGRESSION AND CLASSIFICATION TREE FOR BINARY TARGET VARIABLE BINSHARING
# CALCULATED ON TRAINING SET AND LATER EVALUATED ON TEST (HOLDOUT) SET
# -------------------------------------------------------------------------------------------------------------------

library(rpart)
library(rpart.plot)

# LINEAR REGRESSION FOR BINARY TARGET BINSHARING (without interaction term)
Mdl.Binary.R   <- binSharing ~ f.Bikeown + Income + Neighbourhood01 + DepotDist + Neuroticism + Openness
Mdl.Binary.U   <- binSharing ~ f.Bikeown + Income + Neighbourhood01 + DepotDist + Neuroticism + Openness + f.Gender + Age + f.Job + f.Jobtype + WorkHours + f.Transport + f.Commutedist + f.Car + f.Bikelease + f.SharingLocPref + f.DepotInStation

Rslt.LinReg.Binary.R <- lm(Mdl.Binary.R, data = dsBikeSharing.Train)
Rslt.LinReg.Binary.U <- lm(Mdl.Binary.U, data = dsBikeSharing.Train)

stargazer(Rslt.LinReg.Binary.R, Rslt.LinReg.Binary.U,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type = "html", 
          title = "Linear regression results of restricted and unrestricted models for binSharing target", 
          out=paste(dirTablesRegClass, "Linear Regression Binary Target Restricted and Unrestricted Model.doc"))


# CLASSIFICATAION TREE BINARY TARGET BINSHARING

Rslt.Tree.Binary.R <- rpart(Mdl.Binary.R, data = dsBikeSharing.Train, method = "class", parms = list(split = "information"))
Rslt.Tree.Binary.U <- rpart(Mdl.Binary.U, data = dsBikeSharing.Train, method = "class", parms = list(split = "information"))

rpart.plot(Rslt.Tree.Binary.R, extra = 104, digits = 3)
rpart.plot(Rslt.Tree.Binary.U, extra = 104, digits = 3)

pdf(paste0(dirTrees, "Classification Tree - Binary Target, Restricted Model.pdf"))
rpart.plot(Rslt.Tree.Binary.R, extra = 104, digits = 3, title = "Classification Tree - Binary Target, Restricted Model")
dev.off()

pdf(paste0(dirTrees, "Classification Tree - Binary Target, Unrestricted Model.pdf"))
rpart.plot(Rslt.Tree.Binary.U, extra = 104, digits = 3, title("Classification Tree - Binary Target, Unrestricted Model"))
dev.off()


# LOGISTIC (LOGIT) REGRESSION

Rslt.Logit.Binary.R <- glm(Mdl.Binary.R, data = dsBikeSharing.Train, binomial(link = "logit"))
Rslt.Logit.Binary.U <- glm(Mdl.Binary.U, data = dsBikeSharing.Train, binomial(link = "logit"))

stargazer(Rslt.Logit.Binary.R, Rslt.Logit.Binary.U,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type = "html", 
          title = "Logistic regression results of restricted and unrestricted models for binSharing target", 
          out=paste(dirTablesRegClass, "Logistic Regression Binary Target Restricted and Unrestricted Model.doc"))

stargazer(Rslt.LinReg.Binary.R, Rslt.Logit.Binary.R, Rslt.LinReg.Binary.U, Rslt.Logit.Binary.U,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE,
          type = "html", 
          title = "Linear and logit regression results of restricted and unrestricted models for binSharing target", 
          out=paste(dirTablesRegClass, "Linear+Logit Regression Binary Target Restricted and Unrestricted Model.doc"))


# CLASSIFICATION PERFORMANCE OF THE SIX MODELS

ClassPerf <- function(y, p, tau=0.5) {
  p <- as.numeric(p > tau)
  y <- factor(y, levels = c(0,1))
  p <- factor(p,levels = c(0,1))
  tbl <- table(Predicted=p, Observed=y)
  print(tbl)
  TP<-tbl[2,2]
  FN<-tbl[1,2]
  TN<-tbl[1,1]
  FP<-tbl[2,1]
  perf<-c(
    Accuracy = (TP+TN)/sum(tbl),
    Sensitivity = TP/(TP+FN),
    Specificity = TN/(FP+TN),
    Precision = TP/(FP+TP))
  return(perf)
}

Pred.LinReg.R <- predict(Rslt.LinReg.Binary.R, dsBikeSharing.Test)
Pred.Tree.R   <- predict(Rslt.Tree.Binary.R, dsBikeSharing.Test, type = "prob")[ ,2]
Pred.Logit.R  <- predict(Rslt.Logit.Binary.R, dsBikeSharing.Test, type = c("response"))

Pred.LinReg.U <- predict(Rslt.LinReg.Binary.U, dsBikeSharing.Test)
Pred.Tree.U   <- predict(Rslt.Tree.Binary.U, dsBikeSharing.Test, type = "prob")[ ,2]
Pred.Logit.U  <- predict(Rslt.Logit.Binary.U, dsBikeSharing.Test, type = c("response"))

perfRestricted   <- data.frame(LinReg = ClassPerf(dsBikeSharing.Test$binSharing, Pred.LinReg.R),
                               Tree = ClassPerf(dsBikeSharing.Test$binSharing, Pred.Tree.R),
                               Logit = ClassPerf(dsBikeSharing.Test$binSharing, Pred.Logit.R))

perfUnrestricted <- data.frame(LinReg = ClassPerf(dsBikeSharing.Test$binSharing, Pred.LinReg.U),
                               Tree = ClassPerf(dsBikeSharing.Test$binSharing, Pred.Tree.U),
                               Logit = ClassPerf(dsBikeSharing.Test$binSharing, Pred.Logit.U))

stargazer(perfRestricted, summary = FALSE,
          align = TRUE, no.space = TRUE, type = "html",
          title = "Classification Performance Restricted Model",
          out = paste0(dirTablesRegClass, "Classification Performance Restricted Model.doc"))

stargazer(perfUnrestricted, summary = FALSE,
          align = TRUE, no.space = TRUE, type = "html",
          title = "Classification Performance Unrestricted Model",
          out = paste0(dirTablesRegClass, "Classification Performance Unrestricted Model.doc"))

stargazer(perfRestricted, perfUnrestricted, summary = FALSE,
          align = TRUE, no.space = FALSE, type = "html", 
          title = "Classification Performance Restricted and Unrestricted Models",
          out = paste0(dirTablesRegClass, "Classification Performance Both Models.doc"))

# RECEIVER OPERATING CHARACTERISTIC (ROC) CURVE

# install.packages("ROCR", dependencies = TRUE)
library("ROCR")

pred.LinReg.Test.R  <- prediction(Pred.LinReg.R, dsBikeSharing.Test$binSharing)
pred.Tree.Test.R    <- prediction(Pred.Tree.R, dsBikeSharing.Test$binSharing)
pred.Logit.Test.R   <- prediction(Pred.Logit.R, dsBikeSharing.Test$binSharing)

pred.LinReg.Test.U  <- prediction(Pred.LinReg.U, dsBikeSharing.Test$binSharing)
pred.Tree.Test.U    <- prediction(Pred.Tree.U, dsBikeSharing.Test$binSharing)
pred.Logit.Test.U   <- prediction(Pred.Logit.U, dsBikeSharing.Test$binSharing)


perf.LinReg.Test.R  <- performance(pred.LinReg.Test.R, measure = "tpr", x.measure = "fpr")
perf.Tree.Test.R    <- performance(pred.Tree.Test.R, measure = "tpr", x.measure = "fpr")
perf.Logit.Test.R   <- performance(pred.Logit.Test.R, measure = "tpr", x.measure = "fpr")
perf.LinReg.Test.U  <- performance(pred.LinReg.Test.U, measure = "tpr", x.measure = "fpr")
perf.Tree.Test.U    <- performance(pred.Tree.Test.U, measure = "tpr", x.measure = "fpr")
perf.Logit.Test.U   <- performance(pred.Logit.Test.U, measure = "tpr", x.measure = "fpr")

pdf(paste0(dirROCs, "Three methods comparison ROC - Restricted Model.pdf"))
plot(perf.LinReg.Test.R, lty = "solid", lwd = 2.0, col = "red",
     main = "Linear regression, classification tree and logistic regression, \nBike Sharing Data, Restricted Model")
plot(perf.Tree.Test.R, lty = "solid", lwd = 2.0, col = "blue", add = TRUE)
plot(perf.Logit.Test.R, lty = "dashed", lwd = 2.0, col = "green", add = TRUE)
abline(a = 0, b = 1, lty = 3, lwd = 1.5)
mtext("Holdout sample performance (70% / 30%)", side = 3)
legend(0.62, 0.15, c("Logistic Regression", "Linear Regression", "Classification Tree"), col = c("red", "blue", "green"), lwd = 3)
dev.off()

pdf(paste0(dirROCs, "Three methods comparison ROC - Unrestricted Model.pdf"))
plot(perf.LinReg.Test.U, lty = "solid", lwd = 2.0, col = "red",
     main = "Linear regression, classification tree and logistic regression, \nBike Sharing Data, Unrestricted Model")
plot(perf.Tree.Test.U, lty = "solid", lwd = 2.0, col = "blue", add = TRUE)
plot(perf.Logit.Test.U, lty = "solid", lwd = 2.0, col = "green", add = TRUE)
abline(a = 0, b = 1, lty = 3, lwd = 1.5)
mtext("Holdout sample performance (70% / 30%)", side = 3)
legend(0.62, 0.15, c("Logistic Regression", "Linear Regression", "Classification Tree"), col = c("red", "blue", "green"), lwd = 3)
dev.off()


# FIVE FOLD CROSS VALIDATION

nFolds <- 5
iFolds <- sample(1:nFolds, nrow(dsBikeSharing), replace = TRUE)
table(iFolds)
rslt.R <- list()
rslt.U <- list()

for(fold in 1:nFolds) {
  dsBikeSharing.Train <- dsBikeSharing[iFolds != fold ,]
  dsBikeSharing.Test <- dsBikeSharing[iFolds == fold ,]
  
  # Calculation with restricted model
  regRslt <- lm(Mdl.Binary.R, data = dsBikeSharing.Train)
  treeRslt <- rpart(Mdl.Binary.R, data = dsBikeSharing.Train, method = "class", parms = list(split = "information"))
  logRslt <- glm(Mdl.Binary.R, data = dsBikeSharing.Train, binomial(link = "logit"))

  predReg.Test  <- predict(regRslt, dsBikeSharing.Test)
  predTree.Test <- predict( treeRslt, dsBikeSharing.Test, type = "prob")[ ,2]
  predLog.Test  <- predict(logRslt , dsBikeSharing.Test, type = c("response"))
  
  rslt.R$observed[[fold]]       <- dsBikeSharing.Test$binSharing
  rslt.R$predicted$Reg[[fold]]  <- unname(predReg.Test)
  rslt.R$predicted$Tree[[fold]] <- unname(predTree.Test)
  rslt.R$predicted$Log[[fold]]  <- unname(predLog.Test)
  
  # Calculation with unrestricted model
  regRslt <- lm(Mdl.Binary.U, data = dsBikeSharing.Train)
  treeRslt <- rpart(Mdl.Binary.U, data = dsBikeSharing.Train, method = "class", parms = list(split = "information"))
  logRslt <- glm(Mdl.Binary.U, data = dsBikeSharing.Train, binomial(link = "logit"))
  
  predReg.Test  <- predict(regRslt, dsBikeSharing.Test)
  predTree.Test <- predict( treeRslt, dsBikeSharing.Test, type = "prob")[ ,2]
  predLog.Test  <- predict(logRslt , dsBikeSharing.Test, type = c("response"))
  
  rslt.U$observed[[fold]]       <- dsBikeSharing.Test$binSharing
  rslt.U$predicted$Reg[[fold]]  <- unname(predReg.Test)
  rslt.U$predicted$Tree[[fold]] <- unname(predTree.Test)
  rslt.U$predicted$Log[[fold]]  <- unname(predLog.Test)
}

# Restricted Model
yvalueObs.R <- rslt.R$observed

predReg.R   <- rslt.R$predicted$Reg
predTree.R  <- rslt.R$predicted$Tree
predLog.R   <- rslt.R$predicted$Log

pred.Reg.R  <- prediction(predReg.R, yvalueObs.R)
pred.Tree.R <- prediction(predTree.R, yvalueObs.R)
pred.Log.R  <- prediction(predLog.R, yvalueObs.R)

perf.Reg.R  <- performance(pred.Reg.R, "tpr", "fpr")
perf.Tree.R <- performance(pred.Tree.R,"tpr", "fpr")
perf.Log.R  <- performance(pred.Log.R, "tpr", "fpr")

pdf(paste0(dirROCs, "Three methods comparison Five-fold ROC - Restricted Model.pdf"))
plot(perf.Log.R, lty = 2, lwd = 1.0, col = "red",
     main = "Linear regression, classification tree and logistic regression \nBike Sharing Data, Restricted Model")
plot(perf.Reg.R, lty = 2, lwd = 1.0, col = "blue", add = TRUE)
plot(perf.Tree.R,lty = 2, lwd = 1.0, col = "green",add = TRUE)
plot(perf.Log.R, avg= "threshold", lty = 1, lwd = 2.0, col = "red", add = TRUE)
plot(perf.Reg.R, avg= "threshold", lty = 1, lwd = 2.0, col = "blue",add = TRUE)
plot(perf.Tree.R,avg= "threshold", lty = 1, lwd = 2.0, col = "green",add = TRUE)
mtext(paste0(nFolds,"-folds cross validation"),side = 3)
abline (a = 0, b = 1, lty = 3, lwd = 1.5)
legend(0.62, 0.15, c("Logistic Regression", "Linear Regression", "Classification Tree"), col = c("red", "blue", "green"), lwd = 3)
dev.off()


# Unrestricted Model
yvalueObs.U <- rslt.U$observed

predReg.U   <- rslt.U$predicted$Reg
predTree.U  <- rslt.U$predicted$Tree
predLog.U   <- rslt.U$predicted$Log

pred.Reg.U  <- prediction(predReg.U, yvalueObs.U)
pred.Tree.U <- prediction(predTree.U, yvalueObs.U)
pred.Log.U  <- prediction(predLog.U, yvalueObs.U)

perf.Reg.U  <- performance(pred.Reg.U, "tpr", "fpr")
perf.Tree.U <- performance(pred.Tree.U,"tpr", "fpr")
perf.Log.U  <- performance(pred.Log.U, "tpr", "fpr")

pdf(paste0(dirROCs, "Three methods comparison Five-fold ROC - Unrestricted Model.pdf"))
plot(perf.Log.U, lty = 2, lwd = 1.0, col = "red",
     main = "Linear regression, classification tree and logistic regression \nBike Sharing Data, Unrestricted Model")
plot(perf.Reg.U, lty = 2, lwd = 1.0, col = "blue", add = TRUE)
plot(perf.Tree.U,lty = 2, lwd = 1.0, col = "green",add = TRUE)
plot(perf.Log.U, avg= "threshold", lty = 1, lwd = 2.0, col = "red", add = TRUE)
plot(perf.Reg.U, avg= "threshold", lty = 1, lwd = 2.0, col = "blue",add = TRUE)
plot(perf.Tree.U,avg= "threshold", lty = 1, lwd = 2.0, col = "green",add = TRUE)
mtext(paste0(nFolds,"-folds cross validation"),side = 3)
abline (a = 0, b = 1, lty = 3, lwd = 1.5)
legend(0.62, 0.15, c("Logistic Regression", "Linear Regression", "Classification Tree"), col = c("red", "blue", "green"), lwd = 3)
dev.off()

# -------------------------------------------------------------------------------------------------------------------
# PARTIAL F TESTS
# -------------------------------------------------------------------------------------------------------------------

# Partial F test on linear regression results for non-binary target (restricted and unrestricted)
anova(Rslt.LinReg.R, Rslt.LinReg.U)
capture.output(anova(Rslt.LinReg.R, Rslt.LinReg.U), file = paste0(dirTablesFtests, "Partial F - Linear Regression Non-binary.txt"))

# Partial F test on linear regression results for binary target (restricted and unrestricted)
anova(Rslt.LinReg.Binary.R, Rslt.LinReg.Binary.U)
capture.output(anova(Rslt.LinReg.Binary.R, Rslt.LinReg.Binary.U), file = paste0(dirTablesFtests, "Partial F - Linear Regression Binary.txt"))

# -------------------------------------------------------------------------------------------------------------------

