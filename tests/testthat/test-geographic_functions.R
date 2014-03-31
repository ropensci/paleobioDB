# test for the geographic functions


data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                          base_name="canis")##missing coordinates
pbdb_map_effort (data)
expect_error(pbdb_map_effort (data))
x11()
pbdb_map_effort (data)
expect_error(pbdb_map_effort (data))

data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                          base_name="canis",show='coords')
expect_error(pbdb_map_effort (data))  

r<-pbdb_map_effort (data)
class(r)
