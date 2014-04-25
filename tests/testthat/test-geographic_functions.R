# test for the geographic functions


context("pbdb_map_occur")
test_that("tests on pbdb_map_occur", {

	##missing coordinates
  
	data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                            base_name="Canis")
	expect_error(pbdb_map_occur (data))
	data<-  pbdb_occurrences (limit="100", vocab="pbdb", 
	                          base_name="canis",show='coords')
	mp<- pbdb_map_occur (data, res=4, 
	                     do.plot=F) 
	expect_true(class (mp) == 'RasterLayer')
	expect_true(sum(mp@data@values,na.rm=T)==nrow(data))
	  d1<-data.frame(lng=c(0,0),lat=c(0,0))
	r1<-pbdb_map_occur(d1, do.plot=F)
	expect_true(sum(r1@data@values,na.rm=T)==nrow(d1))

	mp2<- pbdb_map_occur (data, res=10, do.plot=F) 
	expect_true(class (mp2) == 'RasterLayer')
	expect_true(sum(mp2@data@values,na.rm=T)==nrow(data2))

})

context("pbdb_map_richness")
test_that("tests on pbdb_map_richness", {
    
    ##missing coordinates
    data<-  pbdb_occurrences (limit="100", vocab="pbdb", base_name="canis")
    expect_error(pbdb_map_richness (data))
    data2<-  pbdb_occurrences (limit="100", vocab="pbdb", base_name="canis",show=c("phylo","coords"))
    expect_error(pbdb_map_richness (data2))
    data3<-  pbdb_occurrences (limit="100", vocab="pbdb", base_name="canis",show=c("phylo","coords","ident"))
    expect_true(class (pbdb_map_richness (data3,do.plot=F)) == 'RasterLayer')
    expect_error(pbdb_map_richness (data3,rank='Specie'))
    sp<-letters[sample(1:26,1000,r=T)]
    n=25
    data.teste<-data.frame(lat=rep(0,n),
                          lng=rep(0,n),
                          taxon_rank=rep('species',n),
                          taxon_no=sample(sp,n,r=T),
                          genus_name=sample(sp,n,r=T),
                          family=sample(sp,n,r=T),
                          order=sample(sp,n,r=T),
                          class=sample(sp,n,r=T),
                          phylum=sample(sp,n,r=T))
    t1<-pbdb_map_richness (data.teste,res=20,rank='species',do.plot=F)
    expect_true(sum(t1@data@values,na.rm=T)==length(unique(data.teste$taxon_no)))
    t2<-pbdb_map_richness (data.teste,res=20,rank='genus',do.plot=F)
    expect_true(sum(t2@data@values,na.rm=T)==length(unique(data.teste$genus_name)))
    t3<-pbdb_map_richness (data.teste,res=20,rank='family',do.plot=F)
    expect_true(sum(t3@data@values,na.rm=T)==length(unique(data.teste$family)))
    t4<-pbdb_map_richness (data=data.teste,res=20,rank='order',do.plot=F)
    expect_true(sum(t4@data@values,na.rm=T)==length(unique(data.teste$order)))
    t5<-pbdb_map_richness (data=data.teste,res=20,rank='class',do.plot=F)
    expect_true(sum(t5@data@values,na.rm=T)==length(unique(data.teste$class)))
    t6<-pbdb_map_richness (data=data.teste,res=20,rank='phylum',do.plot=F)
    expect_true(sum(t6@data@values,na.rm=T)==length(unique(data.teste$phylum)))
})
