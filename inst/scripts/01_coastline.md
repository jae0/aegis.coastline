
# Example usage of aegis.coastline

The R library [aegis.coastline](https://github.com/jae0/aegis.coastline) helps to create basic polygons of coastlines. Here, we loop through a few geographic areas of interest and build polygons. Modify them as required.

```r
  require( sf )
  require( aegis.coastline )

  
  for (r in c( "SSE", "snowcrab", "canada.east.superhighres", "canada.east.highres", "canada.east" ) ){
    coast = coastline_db( spatial_domain=r, DS="eastcoast_gadm", redo=TRUE ) 
  }

```


---

The remaining codes are methods for use of other sources of data, namely: 

 - mapdata, 
 
 - GSHHG/NOAA
 
The methods are deprecated, but here in case you require these other data sources.

```r
# Example using mapdata (install the R-library)

    # worldHires coastlines from mapdata (still pretty crude ..)

    coast = coastline_db( spatial_domain="canada.east", DS="mapdata.coastLine.redo" ) # flatten into one
    coast = coastline_db( spatial_domain="canada.east", DS="mapdata.coastPolygon.redo" )

```


```r

# Example using GSHHG coastline data (NOAA) -- best quality data

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

  # example plot

  coast = coastline_db( spatial_domain="canada.east", DS=" gshhg coastline highres", no.clip=TRUE )
  plot( coast, col="transparent", border="steelblue2" ,
       xlim=c(-68,-52), ylim=c(41,50),  xaxs="i", yaxs="i", axes=TRUE )  # ie. coastline
  lines( plygn[ as.character(depths)], col="gray90" ) # for multiple polygons

  sp::compassRose( -55, 43, cex= 0.75 )
  maps::map.scale( -57, 42, ratio =FALSE )


```

 

## Installation


To install, run the following:

```r
  remotes::install_github( "jae0/aegis")  # helper functions
  remotes::install_github( "jae0/aegis.coastline")
``` 

You probably will want to have an Rprofile set up properly such as:

```r
homedir = path.expand("~")
code_root = file.path( homedir, "bio" )   ### replace with correct path to the parent directory of your git-projects
data_root = file.path( homedir, "bio.data" )   ### replace with correct path to your data

require( aegis )
require( aegis.coastline )

```
 
