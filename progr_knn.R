library(class)
library(caret)

normalize <- function(x){
  num <- x-min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}


glass = read.csv("glass.data", header = FALSE, sep = ",")
names(glass) <- c("Id number","RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type_of_glass")
glass = glass[-1] ##usuwam pierwszą kolumnę ID
glass$`Type_of_glass` <- factor(glass$`Type_of_glass`)
glass.scaled <- scale(glass[, -10 ])
glass.norm <- as.data.frame(lapply(glass[, -10 ], normalize))

diabetes = read.table("pima_indians_diabetes.txt", header = FALSE, sep = ",")
names(diabetes) <- c("No_pregnant", "Plasma_glucose", "Blood_pres", "Skin_thick", "Serum_insu", "BMI", "Diabetes_func", "Age", "Class")
diabetes$`Class` <- factor(diabetes$`Class`)
diabetes.scaled <- scale(diabetes[, -9 ]) 
diabetes.norm <- as.data.frame(lapply(diabetes[, -9 ], normalize))

wine = read.table("wine.data", header = FALSE, sep = ",")
names(wine) <- c("Class","Alcohol","Malic Acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
wine$`Class` <- factor(wine$`Class`)
wine.scaled <- scale(wine[, -1 ])
wine.norm <- as.data.frame(lapply(wine[, -1 ], normalize))

seeds = read.table("seeds_dataset.txt",header = FALSE, sep = "\t")
seeds$V8 <-factor(seeds$V8)
seeds.scaled <- scale(seeds[, -8])
seeds.norm <- as.data.frame(lapply(seeds[, -8 ], normalize))

grid <- expand.grid(kmax=c(2,3,4,5,6,7,8,9,10,11,12,13),distance=c(1,2), kernel="optimal")

#distance 1= manhattan 2 euclidean
model <-train(wine.scaled[,-1 ],wine[,1], method='kknn', metric = 'Mean_F1', tuneLength = 8,trControl = trainControl(method = "cv" , number=10, summaryFunction = multiClassSummary),tuneGrid=grid )
model <-train(glass.scaled[,-10 ],glass[,10], method='kknn',metric = 'Mean_F1', tuneLength = 8, trControl = trainControl(method = "cv" , number=10, summaryFunction = multiClassSummary),tuneGrid=grid )
model <-train(diabetes.scaled[,-9 ],diabetes[,9], method='kknn',metric = 'Mean_F1', tuneLength = 8, trControl = trainControl(method = "cv" , number=10, summaryFunction = multiClassSummary),tuneGrid=grid )
model <-train(seeds.scaled[,1:7],seeds[,8], method='kknn',metric = 'Mean_F1', tuneLength = 8, trControl = trainControl(method = "cv" , number=3, summaryFunction = multiClassSummary),tuneGrid=grid )
print(model)
plot(model)
