
coastline.db = function( DS="eastcoast_gadm", crs="+init=epsg:4326", p=NULL, level=4, xlim=NULL, ylim=NULL, redo=FALSE,
  spatial.domain="canada.east.highres", coastline.dir=project.datadirectory( "aegis", "polygons", "coastline" ), ... ) {

  #\\various methods to obtain coastline data

  # ---------------------
  # default (= only supported resolution of 0.5 km discretization)  .. do NOT change
  if (!file.exists( coastline.dir) ) dir.create( coastline.dir, showWarnings=FALSE , recursive=TRUE )

  if (is.null(p)) p=list()
  if (!exists("spatial.domain", p) ) p$spatial.domain = spatial.domain

  p = spatial_parameters( p=p, ...)

  ylim = range(p$corners$lat)
  xlim = range(p$corners$lon)

  # due to projections, a larger buffer area is required to ensure that polygons or coordinates are sufficient,e sp for mapping
  # determine the buffers from the projection of interest
  lon_buffer = c(-1, +1) * diff(range(xlim)) / 10 # add 10% of the range on each side
  lat_buffer = c(-1, +1) * diff(range(ylim)) / 10
  xlim = xlim + lon_buffer
  ylim = ylim + lat_buffer


  if (grepl( "gshhg", DS, ignore.case=TRUE ) ) {
    #\\ simple wrapper to read in the requested data
    fn.gshhs = "http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-bin-2.3.4.zip"
    fn.local = file.path( coastline.dir, basename( fn.gshhs ) )

    if (DS=="gshhg.download") {
      #\\ GSHHG is probably the best world-wide standard for coastline, etc
      #\\ 'A Global Self-consistent, Hierarchical, High-resolution Geography' Database
      #\\ http://www.soest.hawaii.edu/pwessel/gshhg/
      print( "If this fails, you might need to update filename manually in coastline.db( DS='gshhg.download' )" )
      download.file( url=fn.gshhs, destfile=fn.local )
      unzip( fn.local, exdir=file.path( coastline.dir, "gshhg" )  )
      return( "Download finished")
    }

    if (grepl( "coastline", DS, ignore.case=TRUE )) fn.root = "gshhs"
    if (grepl( "rivers", DS, ignore.case=TRUE  )) fn.root = "wdb_rivers"
    if (grepl( "borders", DS, ignore.case=TRUE  )) fn.root = "wdb_borders"

    if (grepl("full", DS, ignore.case=TRUE ) ) fn.suffix = "f.b"
    if (grepl( "highres", DS, ignore.case=TRUE )) fn.suffix = "h.b"
    if (grepl( "intermediate", DS, ignore.case=TRUE  )) fn.suffix = "i.b"
    if (grepl( "low", DS, ignore.case=TRUE  )) fn.suffix = "l.b"
    if (grepl( "crude", DS, ignore.case=TRUE ) ) fn.suffix = "c.b"

    # construct the gshhg data filename appropriate to above choices:
    fn = file.path( coastline.dir, "gshhg", paste( fn.root, fn.suffix, sep="_" ) )
    # local saves to speed things up a little

    fn.loc = paste( fn, p$spatial.domain, "rdata", sep="." )
    out = NULL
    if ( !grepl("redo", DS) ){
      if ( file.exists( fn.loc) ) {
        load( fn.loc )
        if ( ! proj4string( out ) == crs ) out = spTransform( out, sp::CRS(crs) )
        return (out)
    }}
    # if here then none found or we are redoing .. create a new one

    if (!file.exists(fn)) {
      print( "Global Self-consistent, Hierarchical, High-resolution Geography' Database")
      print( "not found ... Downloading to bio.data/polygons/coastline/..." )
      coastline.db( DS="gshhg.download")
    }
    print ("Don't panic about  the following 'error'.. Rgshhs is just being fussy:")
    out = maptools::getRgshhsMap( fn, xlim=xlim, ylim=ylim, level=level, verbose=FALSE, ... )
    print ("")
    print ("")
    print( "The above is not a fatal error .. check your data: " )
    print (out)
    if ( length(out) > 0 ) save (out, file=fn.loc, compress=TRUE )
    if ( ! proj4string( out ) ==  crs ) out = spTransform( out, sp::CRS(crs) )
    return(out)
  }

  # -----------------------------

  if (DS=="eastcoast_gadm") {

    fn = file.path( coastline.dir, paste( "eastcoast_gadm", p$spatial.domain, "rdata", sep="." ) )
    if ( !redo ) {
      if ( file.exists(fn) )  {
        load( fn )
        if ( ! proj4string( out ) ==  crs ) out = spTransform( out, sp::CRS(crs) )
        return (out)
      }
    }

    require(GADMTools)

    message( "Downloading .. " )
    dir.local = file.path( coastline.dir, "polygons", "gadm" )
    dir.create( dir.local, recursive=TRUE, showWarnings=FALSE )

    gadmsp = GADMTools::gadm_sp.loadCountries( fileNames="CAN", level=1, basefile=dir.local )
    maritimes = GADMTools::subset(gadmsp, level=1, regions=c("Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec","New Brunswick"  )  )

    gadmsp = GADMTools::gadm_sp.loadCountries( fileNames="USA", level=1, basefile=dir.local )
    if (0) {
      listNames(gadmsp, level=1)
    }
    useast = GADMTools::subset(gadmsp, level=1, regions=c("Connecticut", "Delaware", "Florida",  "Georgia",
      "Maine",  "Maryland", "Massachusetts","New Hampshire", "New Jersey", "New York" ,"North Carolina",
      "Pennsylvania", "Rhode Island", "South Carolina",  "Virginia"  )  )

    out = rbind( maritimes$spdf, useast$spdf )

    if ( ! proj4string( out ) ==  crs ) out = spTransform( out, sp::CRS(crs) )

    bb = NULL
    if ((!is.null(xlim) && !is.null(ylim)) ) {
      bb = list( xlim =xlim , ylim=ylim) # bounding box for plots using spplot
    }

    if (is.null(bb)) {
      if (!is.null(p$bb) ) bb = p$bb
    }

    if (!is.null(bb)) {
      bd = Polygon( matrix( c(
          min(bb[["xlim"]]), min(bb[["ylim"]]),
          min(bb[["xlim"]]), max(bb[["ylim"]]),
          max(bb[["xlim"]]), max(bb[["ylim"]]),
          max(bb[["xlim"]]), min(bb[["ylim"]]),
          min(bb[["xlim"]]), min(bb[["ylim"]])
        ), ncol = 2, byrow = TRUE )
      )
      bd = SpatialPolygons(
        list(Polygons(list(bd), ID = "bb")),
        proj4string=sp::CRS(crs)
      )
      bd = spTransform( bd, sp::CRS(crs) )

      # trim
      out = rgeos::gIntersection( bd, out, drop_lower_td=TRUE, byid=TRUE ) # crop
      # sum(gIsValid(out, byid=TRUE)==FALSE) # check if any bad polys?
      # out = gBuffer(out, byid=TRUE, width=0)
      # plot(out)

      polyid = gsub( "^bb[[:space:]]", "", names(out) )
      oo = which(duplicated(polyid) )
      if (length(oo)>0) {
        for ( i in 1:length(oo) ) {
          j = which( polyid == polyid[oo[i]] )
          for ( k in 2:length(j)) polyid[j[k]] = paste( polyid[j[k]], k, sep="_")
        }
      }
      out = sp::spChFIDs( out,  polyid ) #fix id's
    }

    # out = gIntersection( out, bboxout, byid = TRUE )
    # sum(gIsValid(out, byid=TRUE)==FALSE) # check if any bad polys?
    # out = gBuffer(out, byid=TRUE, width=0)
    # plot(out)
    # out = sp::spChFIDs( out, "NovaScotia" ) #fix id's

    save(out, file=fn, compress=TRUE)
    return(out)
  }

  # -----------------------------

  if (DS %in% c( "mapdata.coastLine", "mapdata.coastLine.redo")) {
    fn.coastline = file.path( coastline.dir, "mapdata.coastline.rdata" )
    if ( !redo | DS != "mapdata.coastLine.redo" ) {
      if ( file.exists( fn.coastline) ) {
        load( fn.coastline)
        if ( ! proj4string( coastSp ) ==  crs ) coastSp = spTransform( coastSp, sp::CRS(crs) )
        if (DS=="mapdata.coastLine") return( coastSp )
      }
    }
    coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                ylim=ylim, xlim=xlim, resolution=0, plot=FALSE)
    coastSp = map2SpatialLines( coast, IDs=sapply(coast$names, function(x) "0"),  # force all to be "0" elevation
                proj4string= sp::CRS("+init=epsg:4326"))
    save( coastSp, file=fn.coastline ) ## save spherical
    if ( ! proj4string( coastSp ) == crs ) coastSp = spTransform( coastSp, sp::CRS(crs) )
    return( coastSp )
  }

  # ------------------------

  if (DS %in% c("mapdata.coastPolygon", "mapdata.coastPolygon.redo") ) {
    fn.coastpolygon = file.path( coastline.dir, "mapdata.coastpolygon.rdata" )
    if (  !redo |  DS != "mapdata.coastPolygon.redo") {
      if ( file.exists( fn.coastpolygon)) {
        load( fn.coastpolygon)
        if ( ! proj4string( coastSp ) == crs ) coastSp = spTransform( coastSp, sp::CRS(crs) )
        if (DS=="mapdata.coastPolygon") return( coastSp )
      }
    }
    RLibrary( "maps", "mapdata", "maptools", "rgdal" )
    coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                ylim=ylim, xlim=xlim, resolution=0, plot=FALSE)

    coastSp = map2SpatialPolygons( coast, IDs=sapply(coast$names, function(x) x[1]),
                proj4string= sp::CRS("+init=epsg:4326") )

#      coastSp = map2SpatialPolygons( coast, IDs=sapply(coast$names, function(x) x[1]),
#                  proj4string= raster::crs("+init=epsg:4326"))
    save( coastSp, file=fn.coastpolygon )
    if ( ! proj4string( coastSp) == crs ) coastSp = spTransform( coastSp, sp::CRS(crs) )
    return( coastSp )
  }


}
