coastline_layout = function( p, xy.scalebar=c(-2e5, 1.5e5), depths=c( 100, 200, 300, 400, 500, 600, 700 ), plotmap=FALSE, redo=FALSE ) {

  fn = file.path( project.datadirectory("aegis", "polygons", "coastline" ), paste( "coastline_layout", p$spatial_domain, "rdata", sep="." ) )
  out = NULL

  if (!redo) {
    if (file.exists(fn)) load(fn)
    if( !is.null(out) ) return(out)
  }

  bounding_domain = matrix( c(
    min(p$boundingbox[["xlim"]]), min(p$boundingbox[["ylim"]]),
    min(p$boundingbox[["xlim"]]), max(p$boundingbox[["ylim"]]),
    max(p$boundingbox[["xlim"]]), max(p$boundingbox[["ylim"]]),
    max(p$boundingbox[["xlim"]]), min(p$boundingbox[["ylim"]]),
    min(p$boundingbox[["xlim"]]), min(p$boundingbox[["ylim"]])
  ), ncol = 2, byrow = TRUE )

  bounding_domain = (
    st_multipoint(bounding_domain)
    %>% st_sfc()
    %>% st_cast("POLYGON" )
    %>% st_make_valid()
  )
  st_crs(bounding_domain) =st_crs( projection_proj4string("lonlat_wgs84") )
  bounding_domain = st_transform( bounding_domain, st_crs(p$aegis_proj4string_planar_km) )

  # coastline for mapping via spplot
  coast = coastline_db( p=p, DS="eastcoast_gadm" )
  coast = st_transform( coast, st_crs(p$aegis_proj4string_planar_km) )
  coast = st_intersection( coast, bounding_domain )

  # depth contours
  isobs = aegis.bathymetry::isobath_db( p=p, depths=depths  )
  isobs = st_transform( isobs, st_crs(p$aegis_proj4string_planar_km) )
  isobs = st_intersection( isobs, bounding_domain )

  if (plotmap) {
    plot(coast, col="grey95")
    plot(isobs, add=TRUE, col="lightblue")
    sppoly = aegis.polygons::maritimes_groundfish_strata( areal_units_timeperiod="pre2014", returntype="polygons" )
    sppoly = spTransform( sppoly, sp::CRS(p$aegis_proj4string_planar_km) )
    plot(sppoly, col="transparent", add=TRUE)
  }

  out = list(
    coastLayout = list(
      list("sp.polygons", as(coast, "Spatial"), fill=FALSE, col="gray", first=FALSE ), # outline of NS for plotting with spplot
      list("sp.lines", as(isobs, "Spatial"), col="lightgray" ), # outline of NS for plotting with spplot
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
