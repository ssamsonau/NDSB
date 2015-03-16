This code was written to participate in Kaggle [National Data Science Bowl](https://www.kaggle.com/c/datasciencebowl)  
Features are extracted from images. Random Forest is used to make predictions 

**Feature extraction:**

* EBimageFeatureExtraction.R - extract various characteristics of the image. Usage of EBImage package. Distribution of mass by radius and angles.

* imToData.R - script to perform feature extraction and to form csv file.

* imToData_test.R - the same for test data

* join_with_Vadim_Data.R - join features with those obtained by a teammate

* balance_classes.R - file contains a function that can be used to under or over sample unbalanced classes

**Make model**

* make_model.R

* mcLogLoss.R - calculates multiclass log loss if needed

* mcLogLoss_metrics.R - use mcLogLoss.R with caret package

**Predict**

* predict.R - make prediction

* dominoRunScript.R - run prediction file on domino lab 

* correct_file_created_by_domino.R - corrects a submission file created by domino  