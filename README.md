# IncomeClassification

OBJECTIVE:

1.The goal of the project is  to estimate if an individual belongs to higher (>= 50K) income group or not. 

2.To create a best fit model using Logistic Regression, Decision Tree and Random Forest.

3.To calculate the Accuracy, Precision and Recall for the models to decide which classification technique gives the best result.

DATA CLEANING:

1.income_ev[] = lapply(income_ev, gsub, pattern='-', replacement=‘_’)

2.Recoding done for Workclass and Country:
Workclass: 
 GOVT
 Self Employed
 Private
 Others
Education
Schooling
Graduate
Masters
Phd
Marital
Single
Married
Separated

Country: 
 US
 Europe
 Asia
 UK
Relationship
Husband
Wife
Parent
Others
Occupation
Admin/Clerical
Technical
Managerial
Others

3.Converted Variables Capital gain and Capital Loss into one variable 
     Capital Change= Capital gain-Capital Loss

4.Imputed the missing values with the mode.

EXPLORING DATA:

Some of the visualization are as follows:

 <img src="https://user-images.githubusercontent.com/99994988/154979744-47b6c2bc-f8b2-448b-ba8a-f0414634a979.png" width="400" height="400" align="right">
 
 <img src="https://user-images.githubusercontent.com/99994988/154981402-87aba7ba-dfe7-40de-a17c-839d29fcf0cf.png" width="400" height="400">

<img src="https://user-images.githubusercontent.com/99994988/154984089-3f046b11-509e-4b0c-9465-8f903d0f58d1.png" width="500" height="400">

Logistic Regression Model:

Model 1: Logistic Regression done with all the variables  :AUC 74.6%

Model 2: Logistic Regression done without country,workclass, education, race :AUC 75.3%

Decision Tree: AUC 85.4%

<img src="https://user-images.githubusercontent.com/99994988/154985156-c9316129-2fc2-4b01-9298-69ebc15ea179.png" width="700" height="400">

Random Forest: AUC 90.3%

<img src="https://user-images.githubusercontent.com/99994988/154989864-074b2c40-3a3c-480f-86f4-64b5ec2b0f3b.png" width="700" height="400">


CONCLUSION:

Random Forest Model is giving better result as compared to Logistic Regression on the basis of the comparison table below . Therefore, this model is best fit in predicting the income level for this dataset.




<img src="https://user-images.githubusercontent.com/99994988/154990132-8165737a-6410-4be8-a6ed-6816a795502e.png" width="700" height="400">


