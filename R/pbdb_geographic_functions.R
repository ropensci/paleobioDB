
.extract.LatLong <- function (query){
    latlong <- data.frame(lng = query$lng, lat = query$lat)
    counts<- ddply(latlong,.(lng,lat),nrow)
    colnames (counts)<- c("lng", "lat", "Occur")
    counts
}


.add.ColOcean <-function(col.ocean,col.int,...){
    par(mar=c(0,0,0,0),xpd=TRUE,...)
    map(t='n',...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr") [4], col = col.ocean)
    map(col=col.int,fill=T,add=T,...)
}

.add.Points <-function(data,col.point,pch,...){
    Pal <- colorRampPalette(col.point)
    data$n<-as.numeric(cut(data$Occur,breaks = 5))
    data$Col <- Pal(5)[data$n]
    points(data[,1:2], col=alpha(data$Col,0.8),pch=pch,...)
    data
}

.add.Legend <- function(dat,col.int,pch,...){
    n=length(unique(dat$Col))
    Col=unique(dat$Col)[order(unique(dat$n))]
    Legend=seq(min(dat$Occur),max(dat$Occur),length.out=n)
    legend("bottom",col=Col, inset=c(0,-0.11), legend=Legend,ncol=n, title="Occurrences",bg=col.int,pch=pch,...)
}  

#' pbdb_map
#' 
#' shows the map of occurrences of fossil records
#' 
#' @usage pbdb_map (query, col.int='white' ,pch=19, col.ocean='black', main=NULL, col.point=c('light blue','blue'),...)
#' 
#' @param query Input dataframe. This dataframe is the output of  \code{\link{pbdb_occurrences}} function
#' @param col.int This will be the color of mainland
#' @param pch See: \code{\link{par}}
#' @param col.ocean This will be the color of ocean
#' @param main The title of map 
#' @param col.point Two or more colors. This generates a color gradient and, is used to show the number of samples at the same point
#' @param ... Others parameters. See \code{\link{par}} 
#' @return A map of occurences
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
#' X11( width=20, height=10) ## or x11()
#' pbdb_map(data)
#' pbdb_map(data,pch=1)
#' pbdb_map(data,pch=19,col.point=c('pink','red'), col.ocean='light blue',main='canis')
#'}
#'


pbdb_map <- function(query, col.int='white' ,pch=19, col.ocean='black',
                     main=NULL, col.point=c('light blue','blue'),...){
    if(names(dev.cur())!='X11cairo'){
        stop("Can only view from 'X11(type=\"*cairo\")'. See \"pdbd_map\" help page")}
    .add.ColOcean(col.ocean,col.int,...)
    data <- .extract.LatLong(query)
    dat<-.add.Points(data,col.point,pch=pch,...)
    title(main=main,line=1,...)
    .add.Legend(dat,col.int,pch=pch,...)
}

###pronto
.add.ColOcean2 <-function(col.ocean,col.int,...){
    par(mar=c(0,0,0,0),xpd=TRUE,...)
    map(t='n',add=T,...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr") [4], col = col.ocean)
    map(col=col.int,fill=T,add=T,...)
}

.plot.Raster<-function(data,res,col.int,col.ocean,...){
    par(oma=c(4,0,2,2),...)
    e<-map()
    ext<-extent(e$range)
    r<-raster(ext)
    res(r)<-c(res,res)
    values(r)<-NA
    plot(r,xaxt='n',yaxt='n')
    .add.ColOcean2 (col.ocean,col.int,...)
    map(col=col.int,fill=T,add=T,...)
    r<-rasterize(data[,1:2],r,data[,3],fun=sum)
}
##r<-.plot.Raster(data,res=5,col.int='black',col.ocean='white')

.add.pattern<-function(r,col.rich,col.int.line,...){
    Pal <- colorRampPalette(col.rich)
    plot(r,col=alpha(Pal(5),0.8),add=T,...)
    #map(,add=T,col=col.int.line,...)
}

# x11()
#' pbdb_map
#' 
#' shows the map of occurrences of fossil records
#' 
#' @usage pbdb_map (query, col.int='white' ,pch=19, col.ocean='black', main=NULL, col.point=c('light blue','blue'),...)
#' 
#' @param query Input dataframe. This dataframe is the output of  \code{\link{pbdb_occurrences}} function
#' @param col.int This will be the color of mainland
#' @param pch See: \code{\link{par}}
#' @param col.ocean This will be the color of ocean
#' @param main The title of map 
#' @param col.point Two or more colors. This generates a color gradient and, is used to show the number of samples at the same point
#' @param ... Others parameters. See \code{\link{par}} 
#' @return A map of occurences
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
#' X11( width=20, height=10) ## or x11()
#' pbdb_map(data)
#' pbdb_map(data,pch=1)
#' pbdb_map(data,pch=19,col.point=c('pink','red'), col.ocean='light blue',main='canis')
#'}
#'

pbdb_map_effort <- function(query,res=1,col.int='white', col.int.line='black', col.ocean='black',
                            main=NULL, col.rich=c('light blue','blue'),...){
    if(names(dev.cur())!='X11cairo'){
        stop("Can only view from 'X11(type=\"*cairo\")'. See \"pdbd_map_effort\" help page")}
    data <- .extract.LatLong(query)
    r<-.plot.Raster(data,res,col.int,col.ocean,...)
    .add.pattern(r,col.rich,col.int.line,...)
    title(main=main,...)
    mtext('Number of records',4,line=-1,cex=2)
    r
}
# x11()
#  pbdb_map_effort (canis,res=2,main='Canis')
# title('A',line=1)
# box()
#savePlot('pbdb_map_effort.tiff','tiff')

###extrai o esforço amostral 
.extract.rank.pbdb<-function(query,rank){
    if (length (query$taxon_rank)!=0){ 
        species<- query [query$taxon_rank==rank, ]
        s<-split(species,paste(species$lng,species$lat))
        X<-do.call(rbind,lapply(s,function(x)c(x$lng[1],x$lat[1],length(unique(x$taxon_name)))))
    }
    
    if (length (query$rnk)!=0){
        rnnk<- data.frame (c("species", "genera", "families", "orders", "classes"), 
                           c(3,5,9,13,15))
        rnkk<- rnnk [match (rank, rnnk[,1]), 2]
        species<- query [query$rnk==rnkk, ]
        s<-split(species,paste(species$lng,species$lat))
        x<-s[[1]]
        X<-do.call(rbind,lapply(s,function(x)c(x$lng[1],x$lat[1],length(unique(x$tna)))))   
    }
    rownames(X)<-NULL
    colnames(X)<-c('lng','lat','rich')
    X
}



pbdb_map_richness <- function(query, rank='species', res=1,col.int='white', col.int.line='black', col.ocean='black',
                              main=NULL, col.rich=c('light blue','blue'),...){
    if(!any(rank==c("species", "genera", "families", "orders", "classes"))){
        stop("Invalid rank name. Use: \"species\" or \"genera\" or \"families\" or \"orders\" or \"classes\"" )}
    if(names(dev.cur())!='X11cairo'){
        stop("Can only view from 'X11(type=\"*cairo\")'. See \"pbdb_map_richness\" help page")}
    
    data <- .extract.rank.pbdb(query,rank)
    r<-.plot.Raster(data,res,col.int,col.ocean,...)
    .add.pattern(r,col.rich,col.int.line,...)
    title(main=main,...)
    mtext(paste('Richness of', rank),4,line=-1,cex=2)
    r
}
# pbdb_map_richness (query,rank='genera',res=1,main='canis')
# x11()

# query<-canis2
# sort(unique(paste(canis$idt,canis$ids)))
# length(unique(canis$tid))
# canis <- pbdb_occurrences (base_name="canis",vocab= "pbdb", limit=200, show=c('phylo','coords','ident'))
# head(canis)
# 
# canis2 <- pbdb_occurrences (base_name="mammalia", limit=2000, vocab= "pbdb", show=c('phylo','coords','ident'))
# head(canis2)
# canis3 <- pbdb_occurrences (base_name="canis", limit='all', show=c('phylo','coords','ident'))
# dim(canis3)
# canis4 <- pbdb_occurrences (base_name="canis", limit='all',vocab= "pbdb", show=c('coords'))
# dim(canis4)
# query<-canis4
.extract.rank.specie<-function(query,res=5){
    e<-map(plot=F)
    ext<-extent(e$range)
    r<-raster(ext)
    res(r)<-c(res,res)
    values(r)<-0
    if (length (query$taxon_rank)!=0){ 
        species<- query [query$taxon_rank=='species', ]
        S<-split(species,species$taxon_no)
    }
    
    if (length (query$rnk)!=0){
        species<- query [query$rnk==3, ]
        S<-split(species,species$tid)
    }
    R<-lapply(S,function(y){
        s<-split(y,paste(y$lng,y$lat))
        X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$lng[1],x$lat[1],1))))
        X<-rbind(X[1,],X)
        r2<-rasterize(X[,1:2],r,X[,3])
    }
    )
    all<-calc(stack(R), function(x) sum(x[!is.na(x)]))
    values(all)[values(all)==0]<-NA
    #     plot(all)
    #     map(add=T)
    all
}

# c3<-.extract.rank.specie(canis3,res=5)
# x11()
# c4<-.extract.rank.specie(canis4,res=5)
# query<-canis3


.extract.rank.all<-function(query,res=5,rank='genus'){
    e<-map(plot=F)
    ext<-extent(e$range)
    r<-raster(ext)
    res(r)<-c(res,res)
    values(r)<-0
    ranks<-data.frame(rank=c("genus","family","order","class","phylum"),
                      taxon_rank=c("genus_name","family","order","class","phylum"), 
                      rnk=c("idt","fmn","odn","cll","phl") )
    if (length (query$taxon_rank)!=0){ 
        f<-paste(query[,paste(ranks$taxon_rank[ranks$rank==rank])])
        S<-split(query,f)
    }
    
    if (length (query$rnk)!=0){
        f<-paste(query[,paste(ranks$rnk[ranks$rank==rank])])
        S<-split(query,f)
    }
    R<-lapply(S,function(y){
        s<-split(y,paste(y$lng,y$lat))
        X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$lng[1],x$lat[1],1))))
        X<-rbind(X[1,],X)
        r2<-rasterize(X[,1:2],r,X[,3])
    }
    )
    all<-calc(stack(R), function(x) sum(x[!is.na(x)]))
    values(all)[values(all)==0]<-NA
    #     plot(all)
    #     map(add=T)
    all
}


# c3<-.extract.rank.all(canis3,res=5)
# x11()
# c4<-.extract.rank.all(canis4,res=5)
# query<-canis3
# ranks<-data.frame(rank=c("genus","family","order","class","phylum"),
#                   taxon_rank=c("genus_name","family","order","class","phylum"), 
#                   rnk=c("idt","fmn","odn","cll","phl") )

.plot.Raster.rich<-function(r,col.int,col.ocean,...){
    par(oma=c(4,0,2,2),...)
    e<-map()
    plot(r,xaxt='n',yaxt='n')
    .add.ColOcean2 (col.ocean,col.int,...)
    map(col=col.int,fill=T,add=T,...)
}

pbdb_raster_richness <- function(query, rank='species', do.plot=F, res=1,col.int='white', col.int.line='black', col.ocean='black',
                                 main=NULL, col.rich=c('light blue','blue'),...){
    if(!any(rank==c("species", "genus","family","order","class","phylum"))){
        stop("Invalid rank name. Use: \"species\" or \"genus\" or \"family\" or \"order\" or \"class\" or \"phylum\".
             See \"pbdb_map_richness\" help page" )}
    
    if (!any(colnames(query) %in% c("genus_name","family","order","class","phylum","idt","fmn","odn","cll","phl"))){
        stop("Invalid data input. Use in \"pbdb_occurrences\" function the argument: show=c(\"phylo\",\"coords\",\"ident\").
             e.g. pbdb_occurrences(..., show=c(\"phylo\",\"coords\",\"ident\")). 
             See \"pbdb_map_richness\" help page" )}
    
    if(rank=='species'){
        r<-.extract.rank.specie(query,res)
    }
    else
    {
        r<-.extract.rank.all(query,res,rank)
    }
    
    if(do.plot==TRUE){
        if(names(dev.cur())!='X11cairo'){
            stop("Can only view from 'X11(type=\"*cairo\")'. See \"pbdb_map_richness\" help page")}
        .plot.Raster.rich(r,col.int,col.ocean,...)
                                    .add.pattern(r,col.rich,col.int.line,...)
                                    mtext(paste('Richness of', rank),4,line=-1,cex=2)
                                    
                                    
    }
}
