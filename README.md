# Melbourne_Datathon_2016
SEEK data

# Kaggle
This is the home of the predictive modelling component of the 2016 Melbourne Datathon.

The objective is to predict if a job is in the 'Hotel and Tourism' category.

In the 'jobs' table there is a column 'HAT' which stands for 'Hotel and Tourism'. The values in this column are 1 or 0 representing 'Yes' and 'No' meaning it is or is not in the Hotel and Tourism category. This binary flag is a look up from the column 'Subclasses'.

Some of the rows have a value of -1 for HAT. These are the rows you need to predict.

The prediction can be a  1/0 or a continuous number representing a probability of a job being in the HAT category.

Example code in R and SQL to generate the Barista benchmark will be on the data provided, and Python code will also be made available.

## findings
- important info also lies in raw_job_type: some adds put role desc in raw_job_type

## todo
- try 2 grams and filter out some
- location (done)
- company names
- duplicated rows (done, no use)
- merge title and abstract together (done, no use)
- nchar of title and abstract
- collaborative filtering via user_id from search and impression
- count of users who searched for hat in train set
- count of top grams for hat == 1 (done, no use)
- feature hashing, FA, PCA
- unsupervised (kmeans, tnse)
- try different models (knn, vw, lr, nn, rf, et, etc.)
- try gblinear

## features
- 1-gram of title, abstract, and raw_job_type
- log_salary_min, log_salary_max, log_salary_mean, log_salary_diff -> all scaled
- salary_type -> dummied
- location (city, state, lat, lng, fcl, fcode, rot_45/30/50_x/y, radial_r)
- count of most likely words (performance not good)
- is.dup (performance not good)
- combined title and abstract (performance not good)

## models
- xgb (objective = "binary:logistic", max_depth = 6, subsample = .8, colsample_bylevel = .4, eta = .2)
