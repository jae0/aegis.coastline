coastline_layout = function( p, xy.scalebar=c(-2e5, 1.5e5), depths=c( 100, 200, 300, 400, 500, 600, 700 ), plotmap=FALSE, redo=FALSE ) {

  fn = file.path( project.datadirectory("aegis", "polygons", "coastline" ), paste( "coastline_layout", p$spatial.domain, "rdata", sep="." ) )
  out = NULL

  if (!redo) {
    if (file.exists(fn)) load(fn)
    if( !is.null(out) ) return(out)
  }

  bounding_domain = Polygon( matrix( c(
    min(p$boundingbox[["xlim"]]), min(p$boundingbox[["ylim"]]),
    min(p$boundingbox[["xlim"]]), max(p$boundingbox[["ylim"]]),
    max(p$boundingbox[["xlim"]]), max(p$boundingbox[["ylim"]]),
    max(p$boundingbox[["xlim"]]), min(p$boundingbox[["ylim"]]),
    min(p$boundingbox[["xlim"]]), min(p$boundingbox[["ylim"]])
  ), ncol = 2, byrow = TRUE ) )

  bounding_domain = SpatialPolygons(
    list(Polygons(list(bounding_domain), ID = "bb")),
    proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # expect lon/lat
  )
  bounding_domain = spTransform( bounding_domain, sp::CRS(p$internal.crs) )

  # coastline for mapping via spplot

  # coast = aegis_coastline::coastline.db( p=p, DS=" gshhg coastline highres", no.clip=TRUE )
  coast = coastline.db( p=p, DS="eastcoast_gadm" )
  coast = spTransform( coast, sp::CRS(p$internal.crs) )
  coast = gSimplify(coast, tol = 0.01) # simplify the polgons a bit (km)
  coast = rgeos::gIntersection( bounding_domain, coast, drop_lower_td=TRUE, byid=TRUE, checkValidity=TRUE ) # crop
  # sum(gIsValid(coast, byid=TRUE)==FALSE) # check if any bad polys?
  # coast = gBuffer(coast, byid=TRUE, width=0)


  polyid = gsub( "^bb[[:space:]]", "", names(coast) )
  oo = which(duplicated(polyid) )
  if (length(oo)>0) {
    for ( i in 1:length(oo) ) {
      j = which( polyid == polyid[oo[i]] )
      for ( k in 2:length(j)) polyid[j[k]] = paste( polyid[j[k]], k, sep="_")
    }
  }

  coast = sp::spChFIDs( coast,  polyid ) #fix id's
  #
  # depth contours
  isobs = aegis.bathymetry::isobath.db( p=p, depths=depths  )
  isobs = spTransform( isobs, sp::CRS(p$internal.crs) )
  isobs = rgeos::gIntersection( bounding_domain, isobs, drop_lower_td=TRUE, byid=TRUE ) # crop
  # sum(gIsValid(isobs, byid=TRUE)==FALSE) # check if any bad polys?
  # isobs = gBuffer(isobs, byid=TRUE, width=0)
  # plot(isobs)

  isoid = gsub( "^bb[[:space:]]", "", names(isobs) )
  oo = which(duplicated(isoid) )
  if (length(oo)>0) {
    for ( i in 1:length(oo) ) {
      j = which( isoid == isoid[oo[i]] )
      for ( k in 2:length(j)) isoid[j[k]] = paste( isoid[j[k]], k, sep="_")
    }
  }

  isobs = sp::spChFIDs( isobs,  isoid ) #fix id's

  if (plotmap) {
    plot(coast, col="grey95")
    plot(isobs, add=TRUE, col="lightblue")
    sppoly = aegis.polygons::maritimes_groundfish_strata( timeperiod="pre2014", returntype="polygons" )
    sppoly = spTransform( sppoly, sp::CRS(p$internal.crs) )
    plot(sppoly, col="transparent", add=TRUE)
  }

  out = list(
    coastLayout = list(
      list("sp.polygons", coast, fill=FALSE, col="gray", first=FALSE ), # outline of NS for plotting with spplot
      list("sp.lines", isobs, col="lightgray" ), # outline of NS for plotting with spplot
      # list("SpatialPolygonsRescale", layout.north.arrow(), offset=xy.arrow, scale = 100000),
      list("SpatialPolygonsRescale", layout.scale.bar(), offset =xy.scalebar, scale = 100000, fill=c("transparent","gray") ),
      list("sp.text", xy.scalebar+c(0,-10000), "0"),
      list("sp.text", xy.scalebar+c(100000 ,-10000), "100 km")
    ),
    bounding_domain=bounding_domain
  )

  save(out, file=fn, compress=TRUE)

  return(out)

}
