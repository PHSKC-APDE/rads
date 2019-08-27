# RADS requirements https://kc1.sharepoint.com/:x:/r/teams/APDEdata/_layouts/15/Doc.aspx?sourcedoc=%7B8F9C4404-DF4F-4BB1-8522-75809D4F3362%7D&file=Requirements%20for%20RADS_2019-05-28.xlsx


get_data <- function(dataset, #my_data ... hys, brfss, birth, etc, 
cols, # default is all, specifies returned cols 
year, 
...) # additional subsetting based on dataset (e.g., geographies for vital stats, weight options, etc.)

get_codebook <- function(
dataset, 
cols, 
year)

get_design <-fuction(
dataset)  # @return of psu, weights, etc.


#some psuedo code
get_data <- function(dataset, cols, year, ...){
	
	##confirm arguments are legit
	# load a reference table
	# does the dataset exist
	# do the selected columns make sense
	# are the year(s) valid
	# check dot arguments. Warn when they don't make sense for the selected dataset

	## dispatch to dataset specific function 


	#option 1
	{
		funny = match.fun(reference$function[1])
		return(funny(args_go_here))
	}

	#option 2
	{
		if(dataset == 'hys'){
			return(get_data_hys(dataset = dataset, cols = cols, year = year, ...))
		}
	}
}

get_data_hys(dataset, cols, year, weight_variable,...){
	# fetch data given subsets

	# fetch survey settings

	# turn data into survey object

	# return
}

get_data_birth(blah.....){
	
}