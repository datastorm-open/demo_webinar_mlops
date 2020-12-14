# DataStorm's Webinar 2 
*Un outil pour anticiper les dérives du Machine Learning*

R Package and app R Shiny used during DataStorm's webinar about MLOps.

## Install and Run 
<div style='display:inline;'>
  <img src="https://raw.githubusercontent.com/rstudio/shiny/master/man/figures/logo.png" height="70px">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/Docker_%28container_engine%29_logo.svg/915px-Docker_%28container_engine%29_logo.svg.png" height="70px">
</div>

```bash
# Step 1 : Download package
git clone https://github.com/datastorm-open/webinar_mlops.git

# Step 2 : Build package with build.sh
bash build.sh

# Step 3 : Create Docker instance
docker build . -t datastorm_wbnr_mlops

# Step 4 : Run Docker instance
docker run datastorm_wbnr_mlops
```

## Resources

### Dataset

The dataset used by the pacakge can be downloaded at :
- https://www.kaggle.com/carrie1/ecommerce-data/home
- https://archive.ics.uci.edu/ml/datasets/Online+Retail 

```bash
wget https://archive.ics.uci.edu/ml/machine-learning-databases/00502/online_retail_II.xlsx
```

### Packages used

- data.table
- rAmCharts
- caret
- shiny / shinydashboard
- ...

