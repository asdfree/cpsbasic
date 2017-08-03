if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

cpsbasic_cat <-
	get_catalog( "cpsbasic" ,
		output_dir = file.path( getwd() ) )

# sample 25% of the records
which_records <- sample( seq( nrow( cpsbasic_cat ) ) , round( nrow( cpsbasic_cat ) * 0.25 ) )

# always sample year == 2017 & month == 3
cpsbasic_cat <- unique( rbind( cpsbasic_cat[ which_records , ] , subset( cpsbasic_cat , year == 2017 & month == 3 ) ) )

lodown( "cpsbasic" , cpsbasic_cat )
