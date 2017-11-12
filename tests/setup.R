if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

cpsbasic_cat <-
	get_catalog( "cpsbasic" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( cpsbasic_cat ) ) / ceiling( nrow( cpsbasic_cat ) / 20 ) )

cpsbasic_cat <- unique( rbind( cpsbasic_cat[ record_categories == this_sample_break , ] , cpsbasic_cat[ cpsbasic_cat$year == 2017 & cpsbasic_cat$month == 3 , ] ) )

lodown( "cpsbasic" , cpsbasic_cat )
