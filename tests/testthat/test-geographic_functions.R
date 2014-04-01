# test for the geographic functions


test_that("tests on pbdb_map_effort", {

	##missing coordinates
	data<-  pbdb_occurrences (limit="100", vocab="pbdb", base_name="canis")
	
###	pbdb_map_effort (data)
	expect_error(pbdb_map_effort (data))

	data<-  pbdb_occurrences (limit="100", vocab="pbdb", base_name="canis",show='coords')
	expect_error(pbdb_map_effort (data))  

	# todo: test class to be RasterLayer (with params ok)
	# r<-pbdb_map_effort (data)
	# class(r)
})
