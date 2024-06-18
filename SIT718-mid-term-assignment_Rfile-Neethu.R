###Assignment tasks
##T1. Understand the data
##(i) Download the txt file (ENB_2023.txt) from CloudDeakin and save it to your R working directory.

source_path <- "C:\\Users\\Dell\\OneDrive\\Documents\\Deakin\\ENB_2023.txt"

destination_path <-getwd()

# Use file.copy to move or copy the file to the destination
file.copy(source_path, destination_path, overwrite = TRUE)
# Check if the file has been moved/copied successfully
if (file.exists(file.path(destination_path, "ENB_2023.txt"))) {
  cat("File moved/copied successfully to the working directory.\n")
} else {
  cat("Error moving/copying the file.\n")
}

##(ii) Assign the data to a matrix

my_data <- as.matrix(read.table("ENB_2023.txt"))

print(my_data)

##(iii) The variable of interest is Y (Appliances). To investigate Y, generate a subset of 340 with numerical data

set.seed(123)  # for reproducibility
my.data <- my_data[sample(1:nrow(my_data), 340), c(1:6)]
print(head(my.data))

##Load the Libraries
library(ggplot2)
library(RColorBrewer)
library(moments)
library(grid)

#(iv) Use scatter plots and histograms to understand the relationship between each of the variables X1, X2, X3, X4, 
#X5, and your variable of interest Y. (You should build 5 scatter plots and 6 histograms).

#Scatter Plots

# Scatter Plots
par(mfrow = c(2, 3))  # Set up a 2x3 layout for the plots

# Scatter Plot for X1 vs. Y
plot(my.data[, 1], my.data[, 6],main = "V1 vs Y", xlab = "V1", ylab = "Y")
# Scatter Plot for X2 vs. Y
plot(my.data[, 2], my.data[, 6], main = "V2 vs Y", xlab = "V2", ylab = "Y")

# Scatter Plot for X3 vs. Y
plot(my.data[, 3], my.data[, 6], main = "V3 vs Y", xlab = "V3", ylab = "Y")

# Scatter Plot for X4 vs. Y
plot(my.data[, 4], my.data[, 6], main = "V4 vs Y", xlab = "V4", ylab = "Y")

# Scatter Plot for X5 vs. Y
plot(my.data[, 5], my.data[, 6], main = "V5 vs Y", xlab = "V5", ylab = "Y")

##HISTOGRAMS

# Histograms
par(mfrow = c(2, 3))  # Reset the layout for histograms

# Histogram for X1
hist(my.data[, 1], main = "Histogram for X1", xlab = "V1", col = "lightblue", border = "black")

# Histogram for X2
hist(my.data[, 2], main = "Histogram for X2", xlab = "V2", col = "lightgreen", border = "black")

# Histogram for X3
hist(my.data[, 3], main = "Histogram for X3", xlab = "V3", col = "blue", border = "black")

# Histogram for X4
hist(my.data[, 4], main = "Histogram for X4", xlab = "V4", col = "pink", border = "black")

# Histogram for X5
hist(my.data[, 5], main = "Histogram for X5", xlab = "V5", col = "purple", border = "black")

# Histogram for Y
hist(my.data[, 6], main = "Histogram for Y", xlab = "Y", col = "green", border = "black")

# Correlation

library(corrplot)
library(RColorBrewer)
M <-cor(my.data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

##Boxplot

# Boxplot for a variable
boxplot(my.data, main = "Boxplot")

#T2. Transform the data
##Choose any FOUR variables from the five variables X1, X2, X3, X4, X5.

##Make appropriate transformations so that the values can be aggregated in order to predictthe variable of interest Y (Appliances).
##Assign your transformed data along with your transformed variable of interest to an array (it should be 340 rows and 5 columns). Save it to a txt file titled "name-transformed.txt".
#write.table(your.data,"name-transformed.txt")
#The following tasks are based on the saved transformed data.


# Choose four variables
selected_vars <- c("V1", "V2", "V4", "V5")

# Transform the data
transformed_data <- log(my.data[, selected_vars])
transformed_data <-scale(transformed_data)

# Dimension of the data

dim(transformed_data)

# Combine transformed variables with the variable of interest (Y)
transformed_data <- cbind(transformed_data, my.data[, 5])

# Add column names
colnames(transformed_data) <-c(paste0("Transformed_",selected_vars),"V6")

# Save transformed data to a txt file
write.table(transformed_data, "name-transformed.txt", row.names = FALSE, col.names = TRUE)

# Boxplot for a variable
boxplot(transformed_data, main = "Boxplot")

# Defining simple data transformation functions:

# 1. Min-Max transformation
min_max_transform <- function(x, input_data=x){
  min = min(input_data)
  max = max(input_data)
  mm_x = 1 * ((x - min)/(max - min))
}

# 2. Negative Min-Max Transformation
# The below negation function is taking care of scaling as well
negative_min_max_transform <- function(x, input_data=x){
  min = min(input_data)
  max = max(input_data)
  t_x = 1 - (x - min)/(max - min)
}

# Applying data transformation function to the outlier-treated columns:
# Col #1: RM, Col #2 : LSTAT, Col #3: PTRATIO, Col #4: MEDV

transformed_data[,1]=min_max_transform(transformed_data[,1]) 
transformed_data[,2]=negative_min_max_transform(transformed_data[,2]) 
transformed_data[,3]=negative_min_max_transform(transformed_data[,3]) 
transformed_data[,4]=min_max_transform(transformed_data[,4]) 
transformed_data[,5]=min_max_transform(transformed_data[,5]) 

transformed_data <- na.omit(transformed_data)

##T3. Build models and investigate the importance of each variable.
#(i) Download the AggWaFit.R file (from CloudDeakin) to your working directory and load into the R workspace using,source("AggWaFit718.R")


# Reading the AggWaFit718.R file to use its functions
source_path <- "C:\\Users\\Dell\\OneDrive\\Documents\\Deakin\\R-Demo\\AggWaFit718.R"

destination_path <-getwd()

# Use file.copy to move or copy the file to the destination
file.copy(source_path, destination_path, overwrite = TRUE)
# Check if the file has been moved/copied successfully
if (file.exists(file.path(destination_path, "AggWaFit718.R"))) {
  cat("File moved/copied successfully to the working directory.\n")
} else {
  cat("Error moving/copying the file.\n")
}


# Reading the AggWaFit718.R file to use its functions
source("AggWaFit718.R")

#(ii) Use the fitting functions to learn the parameters for
#a. A weighted arithmetic mean (WAM)

# find the weights for a weighted arithmetic mean 
fit.QAM(transformed_data, output.1="AM-output.txt",stats.1="AM-stats.txt", g=AM, g.inv=invAM) # by default, it uses AM
#output1<- readLines("AM-output.txt")
#print(output1)

output2<-readLines("AM-stats.txt")
print(output2)

##b. Weighted power means (WPM) with p = 0.5

fit.QAM(transformed_data,output.1="PM05output1.txt",stats.1="PM05stats1.txt", g=PM05,g.inv = invPM05)
p = 0.5

#output3<- readLines("PM05output1.txt")
#print(output3)

output4<-readLines("PM05stats1.txt")
print(output4)

### c. Weighted power means with p=2, the outputs files are QMoutput1.txt and QMstats1.txt

fit.QAM(transformed_data,output.1="QMoutput1.txt",stats.1="QMstats1.txt",g=QM,g.inv = invQM) 
p = 2


#output5<- readLines("QMoutput1.txt")
#print(output5)

output6<-readLines("QMstats1.txt")
print(output6)

#d. An ordered weighted averaging function (OWA).

owa_model <-fit.OWA(transformed_data,output.1 = "OWAoutput1.txt",stats.1 = "OWAstats1.txt")

# OWA
#output9<- readLines("OWAoutput1.txt")
#print(output9)

output10<-readLines("OWAstats1.txt")
print(output10)

##e. The Choquet integral

choquet_model <- fit.choquet(transformed_data,output.1 = "CHQoutput1.txt",stats.1 = "CHQstats1.txt")

# choquet
#output11<- readLines("CHQoutput1.txt")
#print(output11)

output11<-readLines("CHQstats1.txt")
print(output11)

##BEST MODEL:
models <- list(
  WAM = print(output2),
  WPM_05 = print(output4),
  WPM_2 = print(output6),
  OWA = print(output10),
  Choquet = print(output11)
)

print(models)

# Best fit model:
#After comparing with the different models, we could find that the best model is Choquet where the lowest RMSE, Absolute Error and correlation

#$Choquet
#[1] "RMSE 0.0872904151479106"               
#[2] "Av. abs error 0.0725554035831504"      
#[3] "Pearson Correlation 0.892230868949794" 
#[4] "Spearman Correlation 0.857269695162676"
#[5] "Orness 0.313743651884586"              
#[6] "i Shapley i"                           
#[7] "1 0.0970975391129206"                  
#[8] "2 0.0865125656323513"                  
#[9] "3 0.0970975391129206"                  
#[10] "4 0.719292356141808"                   
#[11] "binary number fm.weights"              
#[12] "1 0"                                   
#[13] "2 0"                                   
#[14] "3 0"                                   
#[15] "4 0"                                   
#[16] "5 0"                                   
#[17] "6 0"                                   
#[18] "7 0"                                   
#[19] "8 0.622194817028887"                   
#[20] "9 0.622194817028887"                   
#[21] "10 0.622194817028887"                  
#[22] "11 0.622194817028887"                  
#[23] "12 0.622194817028887"                  
#[24] "13 0.653949737470595"                  
#[25] "14 0.622194817028887"                  
#[26] "15 1" 

##T4. Use your model for prediction.
##Using your best fitting model from T3, predict Y (the area) for the following input
##X1=22; X2=38; X3=4; X4=88.2, X5=34.
##You should use the same preprocessing as in Task 2.
#Compare your prediction with the measured value of Y, Y=100.

# New Data
new_data_2 <- data.frame(V1 = 22, V2 = 38,V3=4, V4 = 88.2, V5 = 34)

# Select 4 variables
selected_vars <- c("V1", "V2", "V4", "V5")

new_transformed_data10 <- log(new_data_2[, selected_vars])

dim(new_transformed_data10)

## Combine transformed variables with the variable of interest (Y)
new_transformed_data10 <- cbind(new_transformed_data10, new_data_2[, 5])

# Save transformed data to a txt file
write.table(new_transformed_data10, "new1-transformed.txt", row.names = FALSE, col.names = TRUE)

Choquet <- function(x,v) { # 1. pre-defining the inputs
  n <- length(x) # 2. store the length of x
  w <- array(0,n) # 3. create an empty weight vector
  for(i in 1:(n-1)) { # 4. define weights based on order
    v1 <- v[sum(2^(order(x)[i:n]-1))] #
    # 4i. v1 is f-measure of set of all
    # elements greater or equal to
    # i-th smallest input.
    v2 <- v[sum(2^(order(x)[(i+1):n]-1))] #
    # 4ii. v2 is same as v1 except
    # without i-th smallest
    w[i] <- v1 - v2 # 4iii. subtract to obtain w[i]
  } #
  w[n] <- 1- sum(w) # 4iv. final weight leftover
  x <- sort(x) # 5. sort our vector
  sum(w*x) # 6. calculate as we would WAM
}

# Weights from Choquet model

Choquetweights<-c(0.622194817028887,0.622194817028887,0.622194817028887,0.622194817028887,0.622194817028887, 0.653949737470595, 0.622194817028887)

## Scaled new dataset

new_data_scaled <- c(0.01768237,0.04492434,0.01408388)

# transformation of the dataset

new_data_scaled_1 <- matrix(c(
  negative_min_max_transform(new_transformed_data10),nrow = 1))

print(new_data_scaled_1)

new_data_scaled_1 <- c(0.9823176,0.9550757,0.9859161)

##Prediction:

output_15 <- choquet(new_data_scaled_1 ,Choquetweights)
print(output_15)



