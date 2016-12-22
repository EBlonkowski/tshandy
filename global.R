# config file for visualization app

################################################################################
# Load/import stage

max_mb_upload <- 9

#some input datasets
gefcom_sample <- '../data/gefcom17-sample.tsev'
sensor_sample <- '../data/sensor.tsev'
shss_path <- '../data/shss-mod.tsev'
test_path <- 'g17-test.tsev'

# default input dataset
default_tsev <- test_path
default_out_tsev <- test_path

################################################################################
# Edit stage

# default split parameters
default_split_ratio <- 70

################################################################################
# Correct/Select stage

# default stack decomposition parameters
default_trend <- TRUE
default_seasonal <- TRUE
default_harmonics <- 5
default_daily <- TRUE
default_clusters <- 14
default_ar <- TRUE

################################################################################
# Visualization stage