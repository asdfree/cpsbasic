if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available CPSBASIC microdata files
cpsbasic_cat <-
	get_catalog( "cpsbasic" ,
		output_dir = file.path( getwd() ) )

# march 2016 only
cpsbasic_cat <- subset( cpsbasic_cat , year == 2016 & month == 3 )
# download the microdata to your local computer
stopifnot( nrow( cpsbasic_cat ) > 0 )

library(survey)

cpsbasic_df <- 
	readRDS( file.path( getwd() , "2016 03 cps basic.rds" ) )

cpsbasic_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = cpsbasic_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
cpsbasic_design <- 
	update( 
		cpsbasic_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( cpsbasic_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , cpsbasic_design , unwtd.count )
svytotal( ~ one , cpsbasic_design )

svyby( ~ one , ~ ever_smoked_marijuana , cpsbasic_design , svytotal )
svymean( ~ bmipct , cpsbasic_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , cpsbasic_design , svymean , na.rm = TRUE )
svymean( ~ q2 , cpsbasic_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , cpsbasic_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , cpsbasic_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , cpsbasic_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , cpsbasic_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , cpsbasic_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , cpsbasic_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	cpsbasic_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	cpsbasic_design ,
	na.rm = TRUE
)
sub_cpsbasic_design <- subset( cpsbasic_design , qn41 == 1 )
svymean( ~ bmipct , sub_cpsbasic_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , cpsbasic_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		cpsbasic_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( cpsbasic_design )
svyvar( ~ bmipct , cpsbasic_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , cpsbasic_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , cpsbasic_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , cpsbasic_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , cpsbasic_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	cpsbasic_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		cpsbasic_design 
	)

summary( glm_result )
library(srvyr)
cpsbasic_srvyr_design <- as_survey( cpsbasic_design )
cpsbasic_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

cpsbasic_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

