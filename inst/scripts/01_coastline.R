
  require( aegis.coastline)
  require(sf)

  for (r in c( "SSE", "snowcrab", "canada.east.superhighres", "canada.east.highres", "canada.east" ) ){
    coast = coastline_db( spatial_domain=r, DS="eastcoast_gadm", redo=TRUE ) # default method is GADM based
  }

  # remaining are examples

  if (0) {
    # no longer used

    # worldHires coastlines from mapdata (still pretty crude ..)
    coast = coastline_db( spatial_domain="canada.east", DS="mapdata.coastLine.redo" ) # flatten into one
    coast = coastline_db( spatial_domain="canada.east", DS="mapdata.coastPolygon.redo" )
  }

  if (0) {
    # no longer used

    # GSHHG coastline data (NOAA) -- best quality data
    # download the current version of the GSHHG coastline data
    #  coastline_db( "gshhg.download" )  << ---- RUN THIS if you want to refresh the local copy from NOAA

    # these are generated on the fly the first pass .. p must contain corner$lon/lat
    # and ensure they are the largest domain for your use (here "canada.east.highres" )
    # actual data saved will have a buffer around them to try and account for spherical -> rectangular warping
    # ignore error messages ("no shore line in selection...")
    #  .. conversion to spatial data frames is verbose and fussy

    # pre-construct the main grids and resoultions to save some time, any others will be made on the fly
    coastline_db ( spatial_domain="canada.east", DS="gshhg.download" )

    grids =c( "canada.east.superhighres", "canada.east.highres", "canada.east", "SSE", "SSE.mpa" , "snowcrab")
    for ( gr in grids ){
      u = coastline_db ( spatial_domain=gr, DS="gshhg coastline full redo", no.clip=TRUE  ) # full is all data
      u = coastline_db ( spatial_domain=gr, DS="gshhg coastline highres redo ", no.clip=TRUE  ) # highres is almost all data
      u = coastline_db ( spatial_domain=gr, DS="gshhg coastline intermediate redo", no.clip=TRUE  ) # medium res
      u = coastline_db ( spatial_domain=gr, DS="gshhg coastline low redo ", no.clip=TRUE  ) # low res
      u = coastline_db ( spatial_domain=gr, DS="gshhg coastline crude redo ", no.clip=TRUE  ) # crude is very rough

      # note you can also get rivers and political boundaries .. these are spatial Lines
      u = coastline_db ( spatial_domain=gr, DS="gshhg rivers full redo " ) # crude is very rough
      u = coastline_db ( spatial_domain=gr, DS="gshhg rivers highres redo " ) # crude is very rough
      u = coastline_db ( spatial_domain=gr, DS="gshhg rivers intermediate redo " ) # crude is very rough

      u = coastline_db ( spatial_domain=gr, DS="gshhg borders full redo " ) # crude is very rough
      u = coastline_db ( spatial_domain=gr, DS="gshhg borders highres redo " ) # crude is very rough
      u = coastline_db ( spatial_domain=gr, DS="gshhg borders intermediate redo " ) # crude is very rough

      # to call without p$corners:
      u = coastline_db ( spatial_domain=gr, DS="gshhg coastline full redo", xlim=p$corners$lon, ylim=p$corners$lat ) # full is all data
    }
  }


  #example plot
  depths = c( 100, 200, 250, 300, 400, 500)
  plygn = isobath_db( p=p, DS="isobath", depths=depths  )  # this can only be run after bathymetry.r

  coast = coastline_db( spatial_domain="canada.east", DS=" gshhg coastline highres", no.clip=TRUE )
  plot( coast, col="transparent", border="steelblue2" ,
       xlim=c(-68,-52), ylim=c(41,50),  xaxs="i", yaxs="i", axes=TRUE )  # ie. coastline
  lines( plygn[ as.character(depths)], col="gray90" ) # for multiple polygons

  sp::compassRose( -55, 43, cex= 0.75 )
  maps::map.scale( -57, 42, ratio =FALSE )



  # -----------------------
  # make an sp layout ... used with spplot based maps
  require(sp)
  require(rgeos)
  require(aegis.bathymetry)
  require(stmv)

  p = spatial_parameters( spatial_domain="snowcrab" )
  p = spatial_parameters( spatial_domain="SSE" )

  p$boundingbox = list( xlim=p$corners$lon, ylim=p$corners$lat) # bounding box for plots using spplot

  oo = aegis.coastline::coastline_layout( p=p, redo=TRUE )
  names(oo)

