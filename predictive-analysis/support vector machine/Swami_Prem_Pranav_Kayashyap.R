# SVM Model for handwritten digit recognition.  
#
# Scope : To develop a model using Support Vector Machine which should correctly classify
#         the handwritten digits based on the pixel values given as features.
#
# USes  : This model will be used to identify the image of a digit(between 0-9) submitted by a user via
#         a scanner, a tablet, or other digital devices. 
# Input datasets ->
#
# 1. mnist_train.csv   : It is basically a large database of handwritten digits where we have pixel
#                        values of each digit along with its label(between 0-9).It will be used to train the model.
# 2. mnist_test.csv    : It is basically a large database of handwritten digits where we have pixel
#                        values of each digit along with its label(between 0-9).It will be used to evaluate the model.
#
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 24th Jun, 2018

#Import library
library(ggplot2)   # For plotting
library(gridExtra) # For multiple plotting in one grid
library(kernlab)   
library(caret)     

# Loading data
mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = F, header = F)

## Data Understanding and Preparation started
#Number of rows and columns of data
dim(mnist_train)
# Number of Instances Found  : 60,000
# Number of Attributes Found : 785
dim(mnist_test)
# Number of Instances Found  : 10,000
# Number of Attributes Found : 785 

# Structure of the dataset
str(mnist_train)
str(mnist_test)

#printing first few rows
head(mnist_train)
head(mnist_test)

#Exploring the data
summary(mnist_train)
summary(mnist_test)

#checking missing value
sapply(mnist_train, function(x) sum(is.na(x)))

#Naming the missing column names in mnist_train.csv and mnist_test.csv

#out of 785 attributes 1 attribute is label(between 0-9) and other 784 attributes is pixel value of the image
colnames(mnist_train) <- c("label","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14",
                                   "X15","X16","X17","X18","X19","X20","X21","X22","X23","X24","X25","X26","X27","X28",
                                   "X29","X30","X31","X32","X33","X34","X35","X36","X37","X38","X39","X40","X41","X42",
                                   "X43","X44","X45","X46","X47","X48","X49","X50","X51","X52","X53","X54","X55","X56",
                                   "X57","X58","X59","X60","X61","X62","X63","X64","X65","X66","X67","X68","X69","X70",
                                   "X71","X72","X73","X74","X75","X76","X77","X78","X79","X80","X81","X82","X83","X84",
                                   "X85","X86","X87","X88","X89","X90","X91","X92","X93","X94","X95","X96","X97","X98",
                                   "X99","X100","X101","X102","X103","X104","X105","X106","X107","X108","X109","X110",
                                   "X111","X112","X113","X114","X115","X116","X117","X118","X119","X120","X121","X122",
                                   "X123","X124","X125","X126","X127","X128","X129","X130","X131","X132","X133","X134",
                                   "X135","X136","X137","X138","X139","X140","X141","X142","X143","X144","X145","X146",
                                   "X147","X148","X149","X150","X151","X152","X153","X154","X155","X156","X157","X158",
                                   "X159","X160","X161","X162","X163","X164","X165","X166","X167","X168","X169","X170",
                                   "X171","X172","X173","X174","X175","X176","X177","X178","X179","X180","X181","X182",
                                   "X183","X184","X185","X186","X187","X188","X189","X190","X191","X192","X193","X194",
                                   "X195","X196","X197","X198","X199","X200","X201","X202","X203","X204","X205","X206",
                                   "X207","X208","X209","X210","X211","X212","X213","X214","X215","X216","X217","X218",
                                   "X219","X220","X221","X222","X223","X224","X225","X226","X227","X228","X229","X230",
                                   "X231","X232","X233","X234","X235","X236","X237","X238","X239","X240","X241","X242",
                                   "X243","X244","X245","X246","X247","X248","X249","X250","X251","X252","X253","X254",
                                   "X255","X256","X257","X258","X259","X260","X261","X262","X263","X264","X265","X266",
                                   "X267","X268","X269","X270","X271","X272","X273","X274","X275","X276","X277","X278",
                                   "X279","X280","X281","X282","X283","X284","X285","X286","X287","X288","X289","X290",
                                   "X291","X292","X293","X294","X295","X296","X297","X298","X299","X300","X301","X302",
                                   "X303","X304","X305","X306","X307","X308","X309","X310","X311","X312","X313","X314",
                                   "X315","X316","X317","X318","X319","X320","X321","X322","X323","X324","X325","X326",
                                   "X327","X328","X329","X330","X331","X332","X333","X334","X335","X336","X337","X338",
                                   "X339","X340","X341","X342","X343","X344","X345","X346","X347","X348","X349","X350",
                                   "X351","X352","X353","X354","X355","X356","X357","X358","X359","X360","X361","X362",
                                   "X363","X364","X365","X366","X367","X368","X369","X370","X371","X372","X373","X374",
                                   "X375","X376","X377","X378","X379","X380","X381","X382","X383","X384","X385","X386",
                                   "X387","X388","X389","X390","X391","X392","X393","X394","X395","X396","X397","X398",
                                   "X399","X400","X401","X402","X403","X404","X405","X406","X407","X408","X409","X410",
                                   "X411","X412","X413","X414","X415","X416","X417","X418","X419","X420","X421","X422",
                                   "X423","X424","X425","X426","X427","X428","X429","X430","X431","X432","X433","X434",
                                   "X435","X436","X437","X438","X439","X440","X441","X442","X443","X444","X445","X446",
                                   "X447","X448","X449","X450","X451","X452","X453","X454","X455","X456","X457","X458",
                                   "X459","X460","X461","X462","X463","X464","X465","X466","X467","X468","X469","X470",
                                   "X471","X472","X473","X474","X475","X476","X477","X478","X479","X480","X481","X482",
                                   "X483","X484","X485","X486","X487","X488","X489","X490","X491","X492","X493","X494",
                                   "X495","X496","X497","X498","X499","X500","X501","X502","X503","X504","X505","X506",
                                   "X507","X508","X509","X510","X511","X512","X513","X514","X515","X516","X517","X518",
                                   "X519","X520","X521","X522","X523","X524","X525","X526","X527","X528","X529","X530",
                                   "X531","X532","X533","X534","X535","X536","X537","X538","X539","X540","X541","X542",
                                   "X543","X544","X545","X546","X547","X548","X549","X550","X551","X552","X553","X554",
                                   "X555","X556","X557","X558","X559","X560","X561","X562","X563","X564","X565","X566",
                                   "X567","X568","X569","X570","X571","X572","X573","X574","X575","X576","X577","X578",
                                   "X579","X580","X581","X582","X583","X584","X585","X586","X587","X588","X589","X590",
                                   "X591","X592","X593","X594","X595","X596","X597","X598","X599","X600","X601","X602",
                                   "X603","X604","X605","X606","X607","X608","X609","X610","X611","X612","X613","X614",
                                   "X615","X616","X617","X618","X619","X620","X621","X622","X623","X624","X625","X626",
                                   "X627","X628","X629","X630","X631","X632","X633","X634","X635","X636","X637","X638",
                                   "X639","X640","X641","X642","X643","X644","X645","X646","X647","X648","X649","X650",
                                   "X651","X652","X653","X654","X655","X656","X657","X658","X659","X660","X661","X662",
                                   "X663","X664","X665","X666","X667","X668","X669","X670","X671","X672","X673","X674",
                                   "X675","X676","X677","X678","X679","X680","X681","X682","X683","X684","X685","X686",
                                   "X687","X688","X689","X690","X691","X692","X693","X694","X695","X696","X697","X698",
                                   "X699","X700","X701","X702","X703","X704","X705","X706","X707","X708","X709","X710",
                                   "X711","X712","X713","X714","X715","X716","X717","X718","X719","X720","X721","X722",
                                   "X723","X724","X725","X726","X727","X728","X729","X730","X731","X732","X733","X734",
                                   "X735","X736","X737","X738","X739","X740","X741","X742","X743","X744","X745","X746",
                                   "X747","X748","X749","X750","X751","X752","X753","X754","X755","X756","X757","X758",
                                   "X759","X760","X761","X762","X763","X764","X765","X766","X767","X768","X769","X770",
                                   "X771","X772","X773","X774","X775","X776","X777","X778","X779","X780","X781","X782",
                                   "X783","X784")

colnames(mnist_test) <- c("label","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14",
                               "X15","X16","X17","X18","X19","X20","X21","X22","X23","X24","X25","X26","X27","X28",
                               "X29","X30","X31","X32","X33","X34","X35","X36","X37","X38","X39","X40","X41","X42",
                               "X43","X44","X45","X46","X47","X48","X49","X50","X51","X52","X53","X54","X55","X56",
                               "X57","X58","X59","X60","X61","X62","X63","X64","X65","X66","X67","X68","X69","X70",
                               "X71","X72","X73","X74","X75","X76","X77","X78","X79","X80","X81","X82","X83","X84",
                               "X85","X86","X87","X88","X89","X90","X91","X92","X93","X94","X95","X96","X97","X98",
                               "X99","X100","X101","X102","X103","X104","X105","X106","X107","X108","X109","X110",
                               "X111","X112","X113","X114","X115","X116","X117","X118","X119","X120","X121","X122",
                               "X123","X124","X125","X126","X127","X128","X129","X130","X131","X132","X133","X134",
                               "X135","X136","X137","X138","X139","X140","X141","X142","X143","X144","X145","X146",
                               "X147","X148","X149","X150","X151","X152","X153","X154","X155","X156","X157","X158",
                               "X159","X160","X161","X162","X163","X164","X165","X166","X167","X168","X169","X170",
                               "X171","X172","X173","X174","X175","X176","X177","X178","X179","X180","X181","X182",
                               "X183","X184","X185","X186","X187","X188","X189","X190","X191","X192","X193","X194",
                               "X195","X196","X197","X198","X199","X200","X201","X202","X203","X204","X205","X206",
                               "X207","X208","X209","X210","X211","X212","X213","X214","X215","X216","X217","X218",
                               "X219","X220","X221","X222","X223","X224","X225","X226","X227","X228","X229","X230",
                               "X231","X232","X233","X234","X235","X236","X237","X238","X239","X240","X241","X242",
                               "X243","X244","X245","X246","X247","X248","X249","X250","X251","X252","X253","X254",
                               "X255","X256","X257","X258","X259","X260","X261","X262","X263","X264","X265","X266",
                               "X267","X268","X269","X270","X271","X272","X273","X274","X275","X276","X277","X278",
                               "X279","X280","X281","X282","X283","X284","X285","X286","X287","X288","X289","X290",
                               "X291","X292","X293","X294","X295","X296","X297","X298","X299","X300","X301","X302",
                               "X303","X304","X305","X306","X307","X308","X309","X310","X311","X312","X313","X314",
                               "X315","X316","X317","X318","X319","X320","X321","X322","X323","X324","X325","X326",
                               "X327","X328","X329","X330","X331","X332","X333","X334","X335","X336","X337","X338",
                               "X339","X340","X341","X342","X343","X344","X345","X346","X347","X348","X349","X350",
                               "X351","X352","X353","X354","X355","X356","X357","X358","X359","X360","X361","X362",
                               "X363","X364","X365","X366","X367","X368","X369","X370","X371","X372","X373","X374",
                               "X375","X376","X377","X378","X379","X380","X381","X382","X383","X384","X385","X386",
                               "X387","X388","X389","X390","X391","X392","X393","X394","X395","X396","X397","X398",
                               "X399","X400","X401","X402","X403","X404","X405","X406","X407","X408","X409","X410",
                               "X411","X412","X413","X414","X415","X416","X417","X418","X419","X420","X421","X422",
                               "X423","X424","X425","X426","X427","X428","X429","X430","X431","X432","X433","X434",
                               "X435","X436","X437","X438","X439","X440","X441","X442","X443","X444","X445","X446",
                               "X447","X448","X449","X450","X451","X452","X453","X454","X455","X456","X457","X458",
                               "X459","X460","X461","X462","X463","X464","X465","X466","X467","X468","X469","X470",
                               "X471","X472","X473","X474","X475","X476","X477","X478","X479","X480","X481","X482",
                               "X483","X484","X485","X486","X487","X488","X489","X490","X491","X492","X493","X494",
                               "X495","X496","X497","X498","X499","X500","X501","X502","X503","X504","X505","X506",
                               "X507","X508","X509","X510","X511","X512","X513","X514","X515","X516","X517","X518",
                               "X519","X520","X521","X522","X523","X524","X525","X526","X527","X528","X529","X530",
                               "X531","X532","X533","X534","X535","X536","X537","X538","X539","X540","X541","X542",
                               "X543","X544","X545","X546","X547","X548","X549","X550","X551","X552","X553","X554",
                               "X555","X556","X557","X558","X559","X560","X561","X562","X563","X564","X565","X566",
                               "X567","X568","X569","X570","X571","X572","X573","X574","X575","X576","X577","X578",
                               "X579","X580","X581","X582","X583","X584","X585","X586","X587","X588","X589","X590",
                               "X591","X592","X593","X594","X595","X596","X597","X598","X599","X600","X601","X602",
                               "X603","X604","X605","X606","X607","X608","X609","X610","X611","X612","X613","X614",
                               "X615","X616","X617","X618","X619","X620","X621","X622","X623","X624","X625","X626",
                               "X627","X628","X629","X630","X631","X632","X633","X634","X635","X636","X637","X638",
                               "X639","X640","X641","X642","X643","X644","X645","X646","X647","X648","X649","X650",
                               "X651","X652","X653","X654","X655","X656","X657","X658","X659","X660","X661","X662",
                               "X663","X664","X665","X666","X667","X668","X669","X670","X671","X672","X673","X674",
                               "X675","X676","X677","X678","X679","X680","X681","X682","X683","X684","X685","X686",
                               "X687","X688","X689","X690","X691","X692","X693","X694","X695","X696","X697","X698",
                               "X699","X700","X701","X702","X703","X704","X705","X706","X707","X708","X709","X710",
                               "X711","X712","X713","X714","X715","X716","X717","X718","X719","X720","X721","X722",
                               "X723","X724","X725","X726","X727","X728","X729","X730","X731","X732","X733","X734",
                               "X735","X736","X737","X738","X739","X740","X741","X742","X743","X744","X745","X746",
                               "X747","X748","X749","X750","X751","X752","X753","X754","X755","X756","X757","X758",
                               "X759","X760","X761","X762","X763","X764","X765","X766","X767","X768","X769","X770",
                               "X771","X772","X773","X774","X775","X776","X777","X778","X779","X780","X781","X782",
                               "X783","X784")

#Converting label to factor type
mnist_train$label <- as.factor(mnist_train$label)
mnist_test$label <-  as.factor(mnist_test$label)

#Extracting subset of 5,000 training or testing observations for model building and evaluation
set.seed(100)
#nrow(mnist_train)
training_indices <- sample(1: nrow(mnist_train), 5000) 
training_data    <- mnist_train[training_indices, ]
#nrow(mnist_test)
testing_indices <- sample(1: nrow(mnist_test), 5000) 
testing_data    <- mnist_train[testing_indices, ]

# Scaling all input feature
training_data[ , 2:ncol(training_data)] <- lapply(training_data[ , 2:ncol(training_data)],function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
testing_data[ , 2:ncol(testing_data)]   <- lapply(testing_data[ , 2:ncol(testing_data)],function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))

##Exploratory data analysis started

## Distribution of digits across all data sets
plot1 <- ggplot(mnist_train, aes(x = label, y = (..count..)/sum(..count..), fill = label)) +
  geom_bar(stat= "count") + theme_light() +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1, position = position_dodge(0.2)) +
  xlab("Digits") +
  ylab("Frequency") +
  ggtitle("mnist_train dataset")

plot2 <- ggplot(training_data, aes(x = label, y = (..count..)/sum(..count..), fill = label)) +
  geom_bar(stat= "count") + theme_light() +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1, position = position_dodge(0.2)) +
  xlab("Digits") +
  ylab("Frequency") +
  ggtitle("Training dataset")

plot3 <- ggplot(testing_data, aes(x = label, y = (..count..)/sum(..count..), fill = label)) +
  geom_bar(stat= "count") + theme_light() +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1, position = position_dodge(0.2)) +
  xlab("Digits") +
  ylab("Frequency") +
  ggtitle("Testing dataset")

grid.arrange(plot1, plot2, plot3, nrow = 3)

# We can observe that frequencies of the pixels has been retained for all digit after random sampling training data.
# And Similar frequency is observed in test dataset as well.

#Total Number of Digits (Training Set)
ggplot(training_data, aes(x = label, y = (..count..), fill = label)) +
  geom_bar(stat= "count") + theme_light() +
  geom_text(stat = "count", aes(label= (..count..)), vjust=-0.1, position = position_dodge(0.2)) +
  xlab("Digits") +
  ylab("Count") +
  ggtitle("Total Number of Digits (Training Set)")

#Total Number of Digits (Testing Set)
ggplot(testing_data, aes(x = label, y = (..count..), fill = label)) +
  geom_bar(stat= "count") + theme_light() +
  geom_text(stat = "count", aes(label= (..count..)), vjust=-0.1, position = position_dodge(0.2)) +
  xlab("Digits") +
  ylab("Count") +
  ggtitle("Total Number of Digits (Testing Set)")

###Model Building & Evaluation

##Using Linear kernel

# Linear kernel using default parameters
SVM_model_linear_C1 <- ksvm(label ~ ., data = training_data, scaled = FALSE, kernel = "vanilladot", C = 1)
predicted_data_linear_C1 <- predict(SVM_model_linear_C1, newdata = testing_data, type = "response")
confusionMatrix(predicted_data_linear_C1, testing_data$label) 

# Observations:
# Accuracy - 0.9184

## Linear kernel using C = 10
SVM_model_linear_C10 <- ksvm(label ~ ., data = training_data, scaled = FALSE, kernel = "vanilladot", C = 10)
predicted_data_linear_C10 <- predict(SVM_model_linear_C10, newdata = testing_data, type = "response")
confusionMatrix(predicted_data_linear_C10, testing_data$label) 

# Observations:
# Accuracy - 0.9184

# Hyperparameter tuning and Cross Validation  - Linear - SVM

# Performing 5-fold cross validation
trainControl_linear <- trainControl(method="cv", number=5)

#Our Evaluation metric is Accuracy.
metric_linear <- "Accuracy"

# making a grid of 5 different values of C.
set.seed(100)
grid_linear <- expand.grid(C=seq(1, 5, by=1))

# We will use the train function from caret package to perform crossvalidation
fit.svm_linear <- train(label ~ ., data=training_data, method="svmLinear", metric=metric_linear, tuneGrid=grid_linear, trControl=trainControl_linear)

# Printing cross validation result
print(fit.svm_linear)
# Plotting "fit.svm_linear" results
plot(fit.svm_linear)

# Best tune at C = 2, 
# Accuracy - 0.9039

##Using Radial kernel
# Radial kernel using default parameters
SVM_model_radial_C1 <- ksvm(label ~ ., data = training_data, scaled = FALSE, kernel = "rbfdot", C = 1, kpar = "automatic")
predicted_data_radial_C1 <- predict(SVM_model_radial_C1, newdata = testing_data, type = "response")
confusionMatrix(predicted_data_radial_C1, testing_data$label) 

# Observations:
# Accuracy - 0.9282

# Hyperparameter tuning and Cross Validation  -  Non-Linear - SVM

#Cross Validation with 5 folds.
trainControl_radial <- trainControl(method="cv", number=5)

#Our Evaluation metric is Accuracy.
metric_radial <- "Accuracy"

#set multiple hyperparameters, Sigma and cost that we shall pass to our model.
set.seed(7)
grid_radial <- expand.grid(.sigma = c(0.01,0.02,0.025,0.05,0.1), .C = seq(1, 5, by=1) )

# We will use the train function from caret package to perform Cross Validation. 
fit.svm_radial <- train(label~., data=training_data, method="svmRadial", metric=metric_radial, tuneGrid=grid_radial, trControl = trainControl_radial)

# Printing cross validation result
print(fit.svm_radial)

# Plotting "fit.svm_radial" results
plot(fit.svm_radial)

# Observations:
# Accuracy is highest at C = 1 and sigma = 0.01
# Accuracy of 93%

# our final model is Support Vector Machines with Radial Basis Function Kernel with sigma = 0.01 and C = 3 
# Radial kernel using default parameters
SVM_model_final <- ksvm(label ~ ., data = training_data, scaled = FALSE, kernel = "rbfdot", C = 1, kpar = list(sigma = 0.01) )

predicted_data_final <- predict(SVM_model_final, newdata = testing_data, type = "response")

confusionMatrix(predicted_data_final, testing_data$label) 

# Observations:
# Accuracy is highest at C = 3 and sigma = 0.01
# Accuracy of 95%