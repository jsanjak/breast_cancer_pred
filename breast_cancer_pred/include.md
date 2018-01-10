# Welcome to a demonstration of some basic data exploration and prediction methods 

We will use the Wisconsin Breast Cancer Diagnostic dataset as our example. Here are some characteristics of this data set:

* 569 breast cancer tumor samples (fine needle aspirate)
* 10 real-valued features are taken for each sample. Each feature is measured multiple times at different position on the tumoe and the mean, standard error and (min/max) worst value are recorded.
* The features include:
    * Radius in mm
    * Texture, measured by variance of grayscale image pixels
    * Perimeter in mm
    * Area
    * Smoothness, a moving measure of variation in radii
    * Compactness, (perimeter^2 /area - 1)
    * Symmetry
    * Fractal dimension ("coastline approximation")
* An additional two discrete feature are provided with each record
    * The tumor ID number
    * The clinical diagnosis
        * Either benign (B) or malignant (M)
        * Taken to be gold standard truth.

## Data exploration

In the Explore section you can peruse the dataset to understand its structure. Specifically we look at:
* Histograms of the raw data, colored by diagnosis
* Single variable logistic regressions, to see how well individual variables separate the diagnosis classes
* Correlations between variables to understand higher level strucutre of the data
* Dimensionality reduction approaches to see how well the data can be separated in reduced space

## Prediction

In the Predict section we examine and tune the performance of a few standard machine learning methods including:
* Linear support vector machines
* Support vector machines with radial basis functions
* Random forests classifiers

For each method we vary key model parameters and perform 10-Fold Cross Validation. The cross validation error, the average prediction error across the 10 data folds, is displayed as function of the model parameters. Additionally, we display the predictions of each model using its best parameter set. 
