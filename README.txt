The dataset I used for my MRP was provided by St. Michael's Hospital and is not avavilable for public.
I was instructed by my MRP superviser, Dr. Bener, to use this dataset for my MRP.
This means that my files in GitHub won't have the data. 

Also, the tft-master folder is a toolkit that I used for my MRP, given to me by Can Kavaklioglu.
The codes I wrote are in the experiment folder.

In the experiment folder:
model divergence n=30 is a Matlab figure that shows how the model converges
mymodel is the main code that builds the model
scrapcode mainly contains scrape code I saved for testing purposes. It also has the code for the SVD
training_model is similar to mymodel except it uses the training set instead of the full set
test_error tests the tensor built in training_model by comparing a tensor that has not been decomposed by the model and the tensor from the training_model
MakeModel is for the 5-fold CV to build a model from the training sets
TestError tests the erro from the training sets from MakeModel 

my own test.R is the R code used for exploratory data analysis, building the dataset, and linear regression
Appendix.txt is the values of the latex dimensions from the training model