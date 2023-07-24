

coastline_db = function( DS="eastcoast_gadm", project_to=projection_proj4string("lonlat_wgs84"), p=NULL, level=4, xlim=NULL, ylim=NULL, redo=FALSE,
  spatial_domain="canada.east.highres", coastline.dir=project.datadirectory( "aegis", "polygons", "coastline" ), ... ) {

  #\\various methods to obtain coastline data

  # ---------------------
  # default (= only supported resolution of 0.5 km discretization)  .. do NOT change
  if (!file.exists( coastline.dir) ) dir.create( coastline.dir, showWarnings=FALSE , recursive=TRUE )

  if (is.null(p)) p=list()
  if (!exists("spatial_domain", p) ) p$spatial_domain = spatial_domain

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
      print( "If this fails, you might need to update filename manually in coastline_db( DS='gshhg.download' )" )
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

    fn.loc = paste( fn, p$spatial_domain, "rdata", sep="." )
    out = NULL
    if ( !grepl("redo", DS) ){
      if ( file.exists( fn.loc) ) {
        load( fn.loc )
        if ( ! st_crs( out ) == st_crs(project_to) ) out = st_transform( out, st_crs(project_to) )
        return (out)
    }}
    # if here then none found or we are redoing .. create a new one

    if (!file.exists(fn)) {
      print( "Global Self-consistent, Hierarchical, High-resolution Geography' Database")
      print( "not found ... Downloading to bio.data/polygons/coastline/..." )
      coastline_db( DS="gshhg.download")
    }
    print ("Don't panic about  the following 'error'.. Rgshhs is just being fussy:")

    message( "FIXE ME :: maptools is deprecated" )

    out = maptools::getRgshhsMap( fn, xlim=xlim, ylim=ylim, level=level, verbose=FALSE, ... )
    print ("")
    print ("")
    print( "The above is not a fatal error .. check your data: " )
    print (out)
    out = as( out, "sf" )
    if ( length(out) > 0 ) save (out, file=fn.loc, compress=TRUE )
    if ( ! st_crs( out ) == st_crs(project_to) ) out = st_transform( out, st_crs(project_to) )
    return(out)
  }

  # -----------------------------

  if (DS=="eastcoast_gadm") {

    fn = file.path( coastline.dir, paste( "eastcoast_gadm", p$spatial_domain, "rdata", sep="." ) )
    if ( !redo ) {
      if ( file.exists(fn) )  {
        load( fn )
        if ( ! st_crs( out ) == st_crs(project_to) ) out = st_transform( out, st_crs(project_to) )
        return (out)
      }
    }

    require(GADMTools)

    message( "Downloading .. \n" )
    message( "Warnings about 'old-style crs object detected ...' ' can be ignored .. the maintainer needs to update their files" )
    dir.local = file.path( coastline.dir, "polygons", "gadm" )
    dir.create( dir.local, recursive=TRUE, showWarnings=FALSE )

    maritimes = GADMTools::gadm_subset(GADMTools::gadm_sf.loadCountries( fileNames="CAN", level=1, basefile=dir.local   ),
      level=1, regions=c("Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec","New Brunswick"  )  )$sf
    

    useast = GADMTools::gadm_subset( GADMTools::gadm_sf.loadCountries( fileNames="USA", level=1, basefile=dir.local  ),
      level=1, regions=c("Connecticut", "Delaware", "Florida",  "Georgia",
        "Maine",  "Maryland", "Massachusetts","New Hampshire", "New Jersey", "New York" ,"North Carolina",
        "Pennsylvania", "Rhode Island", "South Carolina",  "Vermont", "Virginia"  )  )$sf

    utm = sf::st_crs("+proj=utm +zone=20 +ellps=GRS80 +datum=NAD83 +units=km") # units m!

    st_crs(maritimes) = st_crs(projection_proj4string("lonlat_wgs84") )
    maritimes = sf::st_transform(maritimes, crs=utm)

    st_crs(useast) = st_crs(projection_proj4string("lonlat_wgs84") )
    useast = sf::st_transform(useast, crs=utm) 
    
    
    bb = NULL
    if ((!is.null(xlim) && !is.null(ylim)) ) {
      bb = list( xlim =xlim , ylim=ylim) # bounding box for plots using spplot
    }


    if (is.null(bb)) {
      if (!is.null(p$bb) ) bb = p$bb
    }

      bd =
        matrix( c(
          min(bb[["xlim"]]), min(bb[["ylim"]]),
          min(bb[["xlim"]]), max(bb[["ylim"]]),
          max(bb[["xlim"]]), max(bb[["ylim"]]),
          max(bb[["xlim"]]), min(bb[["ylim"]]),
          min(bb[["xlim"]]), min(bb[["ylim"]])
        ), ncol = 2, byrow = TRUE )

      bd= (
        st_multipoint(bd)
        %>% st_sfc()
        %>% st_cast("POLYGON" )
        %>% st_make_valid()
      )
    st_crs(bd) =st_crs( projection_proj4string("lonlat_wgs84") )
    bd = sf::st_transform(bd, crs=utm) 

    useast = (
      st_intersection( useast, bd )
      %>% st_buffer(0.5)
      %>% st_union()
      %>% st_cast("MULTIPOLYGON" )
      %>% st_union()
      %>% st_make_valid()
    )

    maritimes = (
      st_intersection( maritimes, bd )
      %>% st_buffer(0.5)
      %>% st_union()
      %>% st_cast("MULTIPOLYGON" )
      %>% st_union()
      %>% st_make_valid()
    )

    out = (
      st_union( maritimes, useast)
      %>% st_simplify()
      %>% st_buffer(0.5)
      %>% st_cast("MULTIPOLYGON" )
      %>% st_union()
      %>% st_make_valid()
    )

    bd = sf::st_transform(bd, crs=st_crs(projection_proj4string("lonlat_wgs84") )) 

    save(out, file=fn, compress=TRUE)

    if ( ! st_crs( out ) == st_crs(project_to) ) out = st_transform( out, st_crs(project_to) )

    return(out)
  }

  # -----------------------------

  if (DS %in% c( "mapdata.coastLine", "mapdata.coastLine.redo")) {
    
    message( "FIXE ME :: maptools is deprecated" )

    RLibrary( "maps", "mapdata", "maptools", "rgdal", "sf" )
    fn.coastline = file.path( coastline.dir, "mapdata.coastline.rdata" )
    if ( !redo | DS != "mapdata.coastLine.redo" ) {
      if ( file.exists( fn.coastline) ) {
        load( fn.coastline)
        if ( ! st_crs( coastSp ) == st_crs( project_to) ) coastSp = spTransform( coastSp, sp::CRS(project_to) )
        if (DS=="mapdata.coastLine") return( coastSp )
      }
    }
    coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                ylim=ylim, xlim=xlim, resolution=0, plot=FALSE)
    coastSp = map2SpatialLines( coast, IDs=sapply(coast$names, function(x) "0"),  # force all to be "0" elevation
                proj4string= sp::CRS(projection_proj4string("lonlat_wgs84")))
    coastSp = as(coastSp, "sf")
    save( coastSp, file=fn.coastline ) ## save spherical
    if ( ! st_crs( coastSp ) == st_crs(project_to) ) coastSp = st_transform( coastSp, st_crs(project_to) )
    return( coastSp )
  }

  # ------------------------

  if (DS %in% c("mapdata.coastPolygon", "mapdata.coastPolygon.redo") ) {
  
    message( "FIXE ME :: maptools is deprecated" )

    # RLibrary( "maps", "mapdata", "maptools", "rgdal", "sf" )
    fn.coastpolygon = file.path( coastline.dir, "mapdata.coastpolygon.rdata" )
    if (  !redo |  DS != "mapdata.coastPolygon.redo") {
      if ( file.exists( fn.coastpolygon)) {
        load( fn.coastpolygon)
        if ( ! st_crs( coastSp ) == st_crs(project_to) ) coastSp = spTransform( coastSp, sp::CRS(project_to) )
        if (DS=="mapdata.coastPolygon") return( coastSp )
      }
    }
    coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                ylim=ylim, xlim=xlim, resolution=0, plot=FALSE)

    coastSp = map2SpatialPolygons( coast, IDs=sapply(coast$names, function(x) x[1]),
                proj4string= sp::CRS(projection_proj4string("lonlat_wgs84")) )

#      coastSp = map2SpatialPolygons( coast, IDs=sapply(coast$names, function(x) x[1]),
#                  proj4string= raster::crs(projection_proj4string("lonlat_wgs84")))
    coastSp = as(coastSp, "sf")
    save( coastSp, file=fn.coastpolygon )
    if ( ! st_crs( coastSp ) == st_crs(project_to) ) coastSp = st_transform( coastSp, st_crs(project_to) )
    return( coastSp )
  }


}
