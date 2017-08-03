if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available CPSBASIC microdata files
cpsbasic_cat <-
	get_catalog( "cpsbasic" ,
		output_dir = file.path( getwd() ) )

# march 2017 only
cpsbasic_cat <- subset( cpsbasic_cat , year == 2017 & month == 3 )
# download the microdata to your local computer
stopifnot( nrow( cpsbasic_cat ) > 0 )

library(survey)

cpsbasic_df <- 
	readRDS( file.path( getwd() , "2017 03 cps basic.rds" ) )

# construct a fake survey design
warning( "this survey design produces correct point estimates
but incorrect standard errors." )
cpsbasic_design <- 
	svydesign( 
		~ 1 , 
		data = cpsbasic_df , 
		weights = ~ pwsswgt
	)
cpsbasic_design <- 
	update( 
		cpsbasic_design , 
		
		pesex = factor( pesex , levels = 1:2 , labels = c( 'male' , 'female' ) ) ,
		
		prempnot = factor( prempnot , levels = 1:4 ,
			labels = c( "employed" , "unemployed" , "nilf-discouraged" , "nilf-other" ) ) ,
		
		weekly_earnings = ifelse( prernwa == -.01 , NA , prernwa ) ,
		
		# exclude anyone whose hours vary
		weekly_hours = ifelse( pehrusl1 < 0 , NA , pehrusl1 ) ,
		
		self_employed = ifelse( peio1cow < 0 , NA , as.numeric( peio1cow %in% c( 6 , 7 ) ) )
	)
sum( weights( cpsbasic_design , "sampling" ) != 0 )

svyby( ~ one , ~ pesex , cpsbasic_design , unwtd.count )
svytotal( ~ one , cpsbasic_design )

svyby( ~ one , ~ pesex , cpsbasic_design , svytotal )
svymean( ~ weekly_earnings , cpsbasic_design , na.rm = TRUE )

svyby( ~ weekly_earnings , ~ pesex , cpsbasic_design , svymean , na.rm = TRUE )
svymean( ~ prempnot , cpsbasic_design , na.rm = TRUE )

svyby( ~ prempnot , ~ pesex , cpsbasic_design , svymean , na.rm = TRUE )
svytotal( ~ weekly_earnings , cpsbasic_design , na.rm = TRUE )

svyby( ~ weekly_earnings , ~ pesex , cpsbasic_design , svytotal , na.rm = TRUE )
svytotal( ~ prempnot , cpsbasic_design , na.rm = TRUE )

svyby( ~ prempnot , ~ pesex , cpsbasic_design , svytotal , na.rm = TRUE )
svyquantile( ~ weekly_earnings , cpsbasic_design , 0.5 , na.rm = TRUE )

svyby( 
	~ weekly_earnings , 
	~ pesex , 
	cpsbasic_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ weekly_earnings , 
	denominator = ~ weekly_hours , 
	cpsbasic_design ,
	na.rm = TRUE
)
sub_cpsbasic_design <- subset( cpsbasic_design , gestfips == 6 )
svymean( ~ weekly_earnings , sub_cpsbasic_design , na.rm = TRUE )
this_result <- svymean( ~ weekly_earnings , cpsbasic_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ weekly_earnings , 
		~ pesex , 
		cpsbasic_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( cpsbasic_design )
svyvar( ~ weekly_earnings , cpsbasic_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ weekly_earnings , cpsbasic_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ weekly_earnings , cpsbasic_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ self_employed , cpsbasic_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( weekly_earnings ~ self_employed , cpsbasic_design )
svychisq( 
	~ self_employed + prempnot , 
	cpsbasic_design 
)
glm_result <- 
	svyglm( 
		weekly_earnings ~ self_employed + prempnot , 
		cpsbasic_design 
	)

summary( glm_result )
library(srvyr)
cpsbasic_srvyr_design <- as_survey( cpsbasic_design )
cpsbasic_srvyr_design %>%
	summarize( mean = survey_mean( weekly_earnings , na.rm = TRUE ) )

cpsbasic_srvyr_design %>%
	group_by( pesex ) %>%
	summarize( mean = survey_mean( weekly_earnings , na.rm = TRUE ) )

