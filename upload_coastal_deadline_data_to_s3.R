#upload all coastal deadline data to coastaldeadline s3 bucket
source("~/coastal_deadline/scripts/configuration.R")
library(aws.s3)

# Define a function to upload a single file to S3
upload_to_s3 <- function(local_file, s3_path, bucket) {
  put_object(file = local_file,
             object = s3_path,
             bucket = bucket)
}

#TODO LOIZA, UHN - CONVERT TO GPKG to upload

#critical infrastructure
local_files <- list.files(path = paste(output.dir, "coastal_infrastructure",sep="/"), pattern="*.gpkg$", recursive = TRUE, full.names = TRUE)
print("Uploading critical infrastructure data to S3")
s3_paths <- paste("s3://", bucket_name,"/critical_infrastructure/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#census geographies
local_files    <- list.files(path = census.output.dir, pattern="*.gpkg$", recursive = TRUE, full.names = TRUE)
s3_paths <- paste("s3://", bucket_name,"/Census/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#inundation summaries
local_files <- list.files(path = inundation.summaries.dir, pattern = "*.gpkg$", full.names = TRUE )
s3_paths <- paste("s3://", bucket_name,"/inundation_summaries/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#named infrastructure inundation summaries
local_files <- list.files(path = paste(output.dir,"/inundated_named_infrastructure",sep=""), pattern = "*.gpkg$", full.names = TRUE )
print("Uploading census geographies to S3")
s3_paths <- paste("s3://", bucket_name,"/inundated_named_infrastructure/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#NPL inundation summaries
local_files <- list.files(path = paste(output.dir,"/npl_inundated",sep=""), pattern = "*.gpkg$", full.names = TRUE )
s3_paths <- paste("s3://", bucket_name,"/npl_inundated/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#county_subdivision_summaries_merged
local_files <- list.files(path = paste(output.dir,"/county_subdivision_summaries_merged",sep=""), pattern = "*.xlsx$", full.names = TRUE )
s3_paths <- paste("s3://", bucket_name,"/county_subdivision_summaries_merged/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#state inundation frequency charts
local_files    <- list.files(path = paste(output.dir, "charts",sep="/"), pattern="*.png$", recursive = TRUE, full.names = TRUE)
s3_paths <- paste("s3://", bucket_name,"/state_level_inundation_summary_graphs/", basename(dirname(local_files)), "/", basename(local_files), sep="")
lapply(1:length(local_files), function(i) {
  upload_to_s3(local_file = local_files[i],
               s3_path = s3_paths[i],
               bucket = bucket_name)
})

#
# #PR-VI inundation layer crops
# files <- list.files(path = cropped.output.dir, pattern = "*PRVI*", full.names = TRUE, recursive = TRUE )
# print("Uploading census geographies to S3")
# folders <- paste("inundation_layer_crops/", basename(dirname(files)), sep="")
# lapply(files, FUN=upload_to_s3, folder=folders)


#TRI
# files <- list.files(path = paste(data.dir, "TRI",sep="/"), pattern="*", recursive = TRUE, full.names = TRUE)
# print("Uploading WWTP data to S3")
# lapply(files, FUN=upload_to_s3, folder= "critical_infrastructure")
#
# #power plants
# files <- list.files(path = paste(data.dir, "energy_infrastructure",sep="/"), pattern="power_plants.gpkg", recursive = TRUE, full.names = TRUE)
# print("Uploading WWTP data to S3")
# lapply(files, FUN=upload_to_s3, folder= "other")
#
# #WWTP
# files <- list.files(path = paste(data.dir, "wwtp",sep="/"), pattern="*", recursive = TRUE, full.names = TRUE)
# print("Uploading WWTP data to S3")
# lapply(files, FUN=upload_to_s3, folder= "other")
#
# #VA hospitals
# files <- list.files(path = paste(data.dir, "VA_hospitals",sep="/"), pattern="VA_hospitals.gpkg", recursive = TRUE, full.names = TRUE)
# print("Uploading VA hospital data to S3")
# lapply(files, FUN=upload_to_s3, folder="other")
#
# #other
# files <- list.files(path = paste(data.dir, "other",sep="/"), pattern="*", recursive = TRUE, full.names = TRUE)
# print("Uploading Other data to S3")
# lapply(files, FUN=upload_to_s3, folder="other")

#accuracy checks
# files <- list.files(path = paste(output.dir,"accuracy_checks",sep="/"), pattern = "*", full.names = TRUE )
# print("Uploading coastal structures projected to S3")
# lapply(files, FUN=upload_to_s3, folder="accuracy_checks")






