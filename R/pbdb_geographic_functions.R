.extract.LatLong <- function (data){
    latlong <- data.frame(lng = data$lng, lat = data$lat)
    counts<- ddply(latlong,.(lng,lat),nrow)
    colnames (counts)<- c("lng", "lat", "Occur")
    counts
}

.add.ColOcean <-function(col.ocean,col.int,...){
    par(mar=c(0,0,0,0),xpd=TRUE,...)
    map(type="n",...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr") [4], col = col.ocean)
    map(col=col.int,fill=T,add=T,...)
}

.add.Points <-function(Y,col.point,pch,...){
    Pal <- colorRampPalette(col.point)
    Y$n<-as.numeric(cut(Y$Occur,breaks = 5))
    Y$Col <- Pal(5)[Y$n]
    points(Y[,1:2], col=alpha(Y$Col,0.8),pch=pch,...)
    Y
}

.add.Legend <- function(Y1,col.int,pch,...){
    n=length(unique(Y1$Col))
    Col=unique(Y1$Col)[order(unique(Y1$n))]
    Legend=seq(min(Y1$Occur),max(Y1$Occur),length.out=n)
    legend("bottom",col=Col, inset=c(0,-0.14), legend=Legend,ncol=n, title="Occurrences",bg=col.int,pch=pch,...)
}  

#' pbdb_map
#' 
#' Maps the fossil records
#' 
#' The function opens a new window for the map
#' 
#' @usage pbdb_map (data, col.int="white" ,pch=19, col.ocean="black", main=NULL,
#'                  col.point=c("light blue","blue"), ...)
#' 
#' @param data Input dataframe. This dataframe is the output of \code{\link{pbdb_occurrences}} function using the argument: \code{\link{show = "coords"}}. See too: \strong{Details} and \strong{Examples}
#' @param col.int The colour of the mainland. 
#' @param pch See: \code{\link{par}}
#' @param col.ocean The colour of the ocean.
#' @param main To set the title of the map. See: \code{\link{par}}
#' @param col.point Two or more colours. To generate the colour gradient used to show the number of occurrences per cell in map
#' @param ... Others parameters. See \code{\link{par}} and \code{\link{map()}} 
#' @details \strong{CAUTION!} The argument \code{\link{show =  "coords"}} in \code{\link{pbdb_occurrences}} function is required. See \strong{Examples}
#' @return A map showing the distribution of the fossil records, with the points with a color gradient, according to the number of occurrences per cell.
#' @seealso See \code{\link{pbdb_occurrences}}, \code{\link{map}}, \code{\link{par}} and \code{\link{colors}} help pages
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (limit="all", vocab= "pbdb", 
#'                          base_name="Canis", show="coords")
#' pbdb_map(data)
#' pbdb_map(data,pch=1)
#' pbdb_map(data,pch=19,col.point=c("pink","red"), col.ocean="light blue",
#'          main="canis")
#' l_ply(dev.list(),dev.off)## to close all windows graphics
#' }
#' 



pbdb_map <- function(data, col.int='white' ,pch=19, col.ocean='black',
                     main=NULL, col.point=c('light blue','blue'), ...){
    if (sum((colnames(data) %in% c("lat","lng")))!=2){
        stop("Invalid data input. Use in \"pbdb_occurrences\" function the argument: show=\"coords\". e.g. pbdb_occurrences(..., show=\"coords\"). 
             See \"pbdb_map\" help page" )}
    X11(width=12, height=8)    
    .add.ColOcean(col.ocean,col.int,...)
    Y <- .extract.LatLong(data)
    Y1<-.add.Points(Y,col.point,pch,...)
    title(main=main,line=1,...)
    .add.Legend(Y1,col.int,pch,...)
}

#-------------------------------------------------

.add.ColOcean2 <-function(col.ocean,col.int,...){
    par(mar=c(0,0,0,0),...)
    map(type='n',add=T,...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr") [4], col = col.ocean)
    map(col=col.int,fill=T,add=T,...)
}

.Raster<-function(Y,res,col.int,col.ocean,...){
    e<-map(plot=F,...)
    ext<-extent(e$range)
    r<-raster(ext)
    res(r)<-c(res,res)
    values(r)<-NA
    r<-rasterize(Y[,1:2],r,Y[,3],fun=sum)
    r
}

.add.pattern<-function(r,col.eff,...){
    Pal <- colorRampPalette(col.eff)
    plot(r,col=alpha(Pal(5),0.8),add=T,...)
}

.plot.Raster.rich<-function(r,col.eff,col.ocean,col.int,res,...){
    par(oma=c(4,0,2,2),...)
    e<-map(type="n",...)
    ext<-extent(e$range)
    r2<-raster(ext)
    res(r2)<-c(res,res)
    values(r2)<-NA
    plot(r2,xaxt="n",yaxt="n")
    .add.ColOcean2 (col.ocean,col.int,...)
    map(col=col.int,fill=T,add=T,...)
    .add.pattern(r,col.eff,...)
}
#' pbdb_map_effort
#' 
#' Creates a RasterLayer object and a plot of the sampling effort (number of fossil records per cell). 
#' 
#' @usage pbdb_map_effort (data, res=1, col.int="white", col.ocean="black", 
#'                      col.eff=c("light blue","blue"), do.plot=TRUE, ...)
#' 
#' @param data Input dataframe. This dataframe is the output of  \code{\link{pbdb_occurrences}} function using the argument: \code{\link{show="coords"}}. See too: \strong{Details} and \strong{Examples}
#' @param res the resolution of the RasterLayer object (in decimal degrees). See: \code{\link{raster}} ()
#' @param col.int The colour of the mainland
#' @param col.ocean The colour of the ocean
#' @param col.eff Two or more colours. To generate the colour gradient used to show the number of occurrences per cell in map
#' @param do.plot Logical; \code{\link{TRUE}} the function returns a RasterLayer and a plot.
#' @param ... Others parameters. See \code{\link{par}} and \code{\link{map()}} 
#' @details \strong{CAUTION!} The argument \code{\link{show =  "coords"}} in \code{\link{pbdb_occurrences}} function is
#'  required. See \strong{Examples}
#' @return A RasterLayer object and a plot with the sampling effort (number of fossil records per cell). This RasterLayer object have the resolution controlled by the argument \code{\link{res}}. The deflaut is  \code{\link{res=1}}.
#' @seealso See \code{\link{pbdb_occurrences}}, \code{\link{map}}, \code{\link{par}} and \code{\link{colors}} help pages
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", 
#'                          show="coords")
#' pbdb_map_effort (data,res=2)
#' pbdb_map_effort (data,res=2,do.plot=F) 
#' l_ply(dev.list(),dev.off)## to close all windows graphics
#'}
#'

pbdb_map_effort <- function(data,res=1,col.int="white", col.ocean="black",
                            col.eff=c("light blue","blue"), do.plot=TRUE, ...){
    if (sum((colnames(data) %in% c("lat","lng")))!=2){
        stop("Invalid data input. Use in \"pbdb_occurrences\" function the argument: show=\"coords\". e.g. pbdb_occurrences(..., show=\"coords\"). 
             See \"pbdb_map_effort\" help page" )}
    Y <- .extract.LatLong(data)
    r<-.Raster(Y,res,col.int,col.ocean,...)
    if(do.plot==T){
        X11(width=13, height=7.8)
        .plot.Raster.rich(r,col.eff,col.ocean,col.int,res,...)
            mtext("Number of records",4,line=-1,cex=2)}
    r
}

#-------------------------------------------------

.extract.rank.specie<-function(data,res=5){
    e<-map(plot=F)
    ext<-extent(e$range)
    r<-raster(ext)
    res(r)<-c(res,res)
    values(r)<-0
    if (length (data$taxon_rank)!=0){ 
        species<- data [data$taxon_rank=="species", ]
        S<-split(species,species$taxon_no)
    }
    
    if (length (data$rnk)!=0){
        species<- data [data$rnk==3, ]
        S<-split(species,species$tid)
    }
    R<-lapply(S,function(y){
        s<-split(y,paste(y$lng,y$lat))
        X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$lng[1],x$lat[1],1))))
        X<-rbind(X[1,],X)
        r2<-rasterize(X[,1:2],r,X[,3])
    }
    )
    names(R)==NULL
    all<-calc(stack(R), function(x) sum(x,na.rm=T))
    values(all)[values(all)==0]<-NA
    all
}

.extract.rank.all<-function(data,res=5,rank="genus"){
    e<-map(plot=F)
    ext<-extent(e$range)
    r<-raster(ext)
    res(r)<-c(res,res)
    values(r)<-0
    ranks<-data.frame(rank=c("genus","family","order","class","phylum"),
                      taxon_rank=c("genus_name","family","order","class","phylum"), 
                      rnk=c("idt","fmn","odl","cll","phl") )
    if (length (data$taxon_rank)!=0){ 
        f<-paste(data[,paste(ranks$taxon_rank[ranks$rank==rank])])
        S<-split(data,f)
    }
    
    if (length (data$rnk)!=0){
        f<-paste(data[,paste(ranks$rnk[ranks$rank==rank])])
        S<-split(data,f)
    }
    R<-lapply(S,function(y){
        s<-split(y,paste(y$lng,y$lat))
        X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$lng[1],x$lat[1],1))))
        X<-rbind(X[1,],X)
        r2<-rasterize(X[,1:2],r,X[,3])
    }
    )
    names(R)=NULL
    all<-calc(stack(R), function(x) sum(x,na.rm=T))
    values(all)[values(all)==0]<-NA
    all
}

#' pbdb_map_richness
#' 
#' Creates a RasterLayer object and a plot with richness of species, genera, families, etc. per cell.
#' 
#' @usage pbdb_map_richness (data, rank="species", do.plot=TRUE, res=1, 
#'                          col.int="white", col.ocean="black", 
#'                          col.rich=c("light blue","blue"),...)
#' 
#' @param data Input dataframe. This dataframe is the output of  \code{\link{pbdb_occurrences}} function using the argument: \code{\link{show = c("phylo", "coords", "ident")}}. See too: \strong{Details} and \strong{Examples}
#' @param rank To set which taxon rank you are interested for calculate richness. The options are: "species", "genus", "family", "order", "class" or "phylum")
#' @param do.plot Logical; \code{\link{TRUE}} the function returns a RasterLayer and a plot.
#' @param res The resolution of the RasterLayer object (in decimal degrees). See: \code{\link{raster}} ()
#' @param col.int The colour of the mainland
#' @param col.ocean The colour of the ocean
#' @param col.rich Two or more colours. To generate the colour gradient used to show the richness per cell in map
#' @param ... Others parameters. See \code{\link{par}} and \code{\link{map()}} 
#' @details \strong{CAUTION!} The argument \code{\link{show =  "coords"}} in \code{\link{pbdb_occurrences}} function is required. See \strong{Examples}
#' @return A RasterLayer object and a plot with richness of species, genera, families, etc. per cell. This RasterLayer object have the resolution controlled by 
#' the argument \code{\link{res}}. The default is  \code{\link{res=1}}. 
#' @seealso See \code{\link{pbdb_occurrences}}, \code{\link{map}}, \code{\link{par}} and \code{\link{colors}} help pages
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (limit=1000, vocab= "pbdb", base_name="mammalia", 
#'                          show=c("phylo","coords","ident"))
#' pbdb_map_richness (data,res=3,rank="genus")
#' pbdb_map_richness (data,res=8,rank="family")
#' pbdb_map_richness (data,res=3,rank="family",do.plot=F)
#' l_ply(dev.list(),dev.off)## to close all windows graphics
#' }
#'

pbdb_map_richness <- function(data, rank="species", do.plot=TRUE, res=1,col.int="white", col.ocean="black",
                              col.rich=c("light blue","blue"),...){
    if(!any(rank==c("species", "genus","family","order","class","phylum"))){
        stop("Invalid rank name. Use: \"species\", \"genus\", \"family\", \"order\", \"class\" or \"phylum\".
             See \"pbdb_map_richness\" help page" )}
    if (sum(colnames(data) %in% c("lat","lng","genus_name","family","order","class","phylum","idt","fmn","odl","cll","phl"))!=7){
        stop("Invalid data input. Use in \"pbdb_occurrences\" function the argument: show=c(\"phylo\",\"coords\",\"ident\"). e.g. pbdb_occurrences(..., show=c(\"phylo\",\"coords\",\"ident\")). 
             See \"pbdb_map_richness\" help page" )}
    
    if(rank=="species"){
        r<-.extract.rank.specie(data,res)
    }
    else
    {
        r<-.extract.rank.all(data,res,rank)
    }
    
    if(do.plot==TRUE){
        X11(width=13, height=7.8)
        .plot.Raster.rich(r,col.rich,col.ocean,col.int,res,...)
        mtext(paste("Richness of", rank),4,line=-1,cex=2)
        
        
    }
    r
}
