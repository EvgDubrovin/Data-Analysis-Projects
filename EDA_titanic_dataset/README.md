# Exploratory data analysis

Here is my EDA practice on a famous titanic dataset. 

***EDA_titanic_dataset_practice.ipynb*** - EDA itself.

Main goals of EDA:
1. To maximize understanding of a given dataset (how good the data is).
2. To define the most important variables for the analysis.
3. To detect outliers and anomalies.

Main instruments:
* charting (boxplots, histograms, qq-plot, dependencies) - to reveal patterns in our data.
* estimating the parameters of a distribution.

What to do witn NA's:
1. Check if data was collected right.
2. Drop missing values.
3. Remove features with too many NA's.
4. Replace for mean, median (for quantitative data) or mode (for categorical data).
5. Multiple imputation.

I also implemented pandas_profiling library that generates a quick (but quite deep) report. Pandas profiling helps to simplify and accelerate EDA process.