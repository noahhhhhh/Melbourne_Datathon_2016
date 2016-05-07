# Melbourne_Datathon_2016 (We Won, again!)
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
- try 2 grams and filter out some (done, all 2 grams are good with 1 gram)
- modify stemming for 1 and 2 grams!!!!!!!!!!!!!!! (done good)
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
- try different models (knn, sparsenn*, vw*, glmnet*, nn, rf, et, etc.)
- try gblinear
- try 3 grams
- try predict class and treat the output as input to predict hat

## features
- 1-gram of title, abstract, and raw_job_type
- log_salary_min, log_salary_max, log_salary_mean, log_salary_diff -> all scaled
- salary_type -> dummied
- location (city, state, lat, lng, fcl, fcode, rot_45/30/50_x/y, radial_r)
- 2-gram of title, abstract, and raw_job_type

## models
- xgb on full train (objective = "binary:logistic", min_child_weight = 10, max_depth = 20, subsample = .9, colsample_bylevel = .4, eta = .2)
