2025-02-20

<!--
+ ongoing...
+ Depends on R (>= 4.1.0) instead of R (>= 3.0.0) due to using native pipes `|>` in code
+ Depends on R (>= 4.1.0) instead of R (>= 3.0.0) due to changed behaviour in `*apply()` functions with argument `simplify`
-->

### Version 3.11.4

-   Fixing internal handling of raster ignore values.

### Version 3.11.3

-   Graphical output (\*.png or other graphical formats) after
    `display()`, `glance()` and `compose_close()` is wrapped by HTML and
    is opened in default internet browser.
    `options(ursaOutputBrowser=NULL)` opens graphical output in default
    internet browser (for Windows OS only).
    `options(ursaOutputBrowser=FALSE)` opens graphical output in default
    app by file type.

-   Fixed vertical flipping of spatial visualization, which was occurred
    if package **`png`** is missed.

-   Coastline is updated to version 2025-02-14T11:51.

### Version 3.11.2

-   In `colorize()` added new value “zero” for choice in argument `fun`
    to accentuate importance of low values.

-   In `colorize()` default value of argument `ramp` is changed to
    `FALSE`.

-   In `allocate()` added new value “sd” for choice in argument `fun`
    for outputting variance in cell aggregation.

-   Minor fixes in internal functions.

-   Coastline is updated to version 2025-01-13T13:47.

### Version 3.11.1

-   In function `colorize()` default value of argument `lazyload` is
    changed from `FALSE` to `TRUE` to speed-up rendering large rasters.

-   Plotting doesn’t affect sessional grid.

-   Layout matrix can be specified directly (argument `layout` in
    `compose_design()` and superior functions).

-   Package start-up initializes `options(ursaProj4Legacy=TRUE)` to keep
    using PROJ4, primarely, for printing. This initialization is skipped
    if `ursaProj4Legacy` option was assigned before package loading.

-   Package **`png`** is added to ‘Suggests’ (from ‘Imports’).

-   Adding support for compressed ESRI Shapefiles `*.shz`.

<!--
+ Package **`proj4`** is removed from 'Suggests'.
-->

### Version 3.11 (transient)

-   MAJOR: WKT as well as PROJ4 is used for coordinate reference
    systems.

### Version 3.10.4

-   New function `whiteboxing()` for manipulation with
    [WhiteboxTools](https://github.com/jblindsay/whitebox-tools/releases)
    using `ursaRaster` objects.

-   Fixed issues `update_coastline()` for polygons with 180 meridian
    cutting.

-   Improved tile URL parsing for external leaflet providers.

-   Coastline is updated to version 2024-01-09T04:48.

### Version 3.10.3

-   Default Mapnik service is replaced to CartoDB service for tiled
    basemaps.

-   `get_earhthdata()` returns “black” image in the case of missed tiles
    instead of error.

-   Coastline is updated to version 2023-12-16T04:43.

### Version 3.10.2

-   Minor fixes in internal functions.

### Version 3.10.1

-   Packages **`rgdal`** and **`jpeg`** are removed from ‘Imports’.

-   Package **`jpeg`** is added to ‘Suggests’ (from ‘Imports’).

-   Package **`sf`** is removed from ‘Suggests’.

-   Package **`sf`** is added to ‘Imports’ (from ‘Suggests’).

-   Package **`gdalraster`** is added to ‘Suggests’ for experimental
    purposes with partial raster reading.

-   Coastline is updated to version 2023-10-06T03:39.

### Version 3.10 (transient)

-   MAJOR: packages **`rgdal`** and **`rgeos`** are rejected.

### Version 3.9.11

-   Repaired parsing of Nominatim responses.

-   Package **`RSQLite`** is added as suggested for using map tiles from
    specified directory of SAS Planet cache

-   In `trackline()` 1) improved response of `by` argument, 2) order of
    `type` argument is reversed to `c("united","conseqwent")`

-   Coastline is updated to version 2023-08-24T03:31.

### Version 3.9.10

-   Foreign functions are symbolic instead of character after [R-devel
    BUG FIXES
    2023-03-20](https://developer.r-project.org/blosxom.cgi/R-devel/NEWS/2023/03/20#n2023-03-20).

-   Coastline is updated to version 2023-03-23T05:08.

### Version 3.9.9

-   Added `quantile_global()`, `quantile_band()` and `quantile_local`
    functions based on `stats::quantile()`.

-   Speeding up elapsed time for `panel_raster()` example.

-   Packages **`terra`** and **`stars`** added as suggested for
    development purpose. New function `as_stars()` creates object of
    class `stars` without **`stars`** package.

### Version 3.9.8

-   Minor changes in `ursa.c` file for removing warnings during
    `r-devel-linux` compilation (`-Wstrict-prototypes`).

-   New function `sort()` for `ursaRaster` objects for ordering based on
    band names.

-   ‘session\_grid()’ prioritizes raster rather vector files, with the
    same names of files.

-   Coastline is updated to version 2022-11-16T04:45.

### Version 3.9.7

-   New function `palettize()` as a wrapper
    `ursa_colortable(colorize(...))`.

-   New argunent `expand` in function `ursa_crop()` for spatial
    expansion using relative value.

-   New argument `by` in function `trackline()` to split segments by
    specified field name.

-   New function `spatial_levelsplit()` trasforms nested polygons (e.g.,
    polygonized isolines) to intervaled non-overlapped polygons.

### Version 3.9.6

-   Package **`magick`** is added as suggested and used for color depth
    decreasing and in SVG whitespace clipping.

<!--
+ Temporal output gpaphic files are created in SVG format, excepting knitting.
-->

-   Available value `"geojsonsf"` for argument `engine` in function
    `spatial_read()` allows using package **`geojsonsf`** for (fast)
    reading GeoJSON files.

-   `update_coastline()` is now forced not to use s2 spherical geometry
    package. Fixed polygons merging in `update_coastline()`.

-   Coastline is updated to version 2022-03-07T04:39.

### Version 3.9.5

-   Test building for development R 4.2.0.

-   Avoiding in code and examples comparions more than two objects using
    `all.equal()`.

-   Corrected index in `value_cr`, when number of columns and number in
    argunents.

-   New argument `zoom` in function `regrid()` simultaneously replaces
    `expand=zoom` and `mul=1/zoom`.

-   New argument `fixed` in function `compose_desing()` forms output
    layout, which is close to session grid.

-   New argument `verbose` in function `consistent_grid()` for verbose
    output.

-   New coersion (yet simplified) from `SpatRaster` class (package
    **terra**).

### Version 3.9.4

-   Fixed example of `trackline()` for non-Windows systems.

### Version 3.9.3

-   Test building for R 4.1.0.

-   New function `trackline()` to connect sequence of spatial points to
    line segments.

-   New return value `ursaLegend` in function `panel_plot()` for spatial
    objects (simple features, spatial abstrac class) instead of
    `ursaColorTable`.

-   New arguments `...` in function `session_grid()` for preliminary
    passing to `regrid()`.

-   New argument `connect` in function `trackline()` for creating either
    solid multi-segment or multiple consequent segments.

-   Patterned argument `vertical` in function `panel_annnotation()` can
    be numeric (degrees on label inclination).

-   `panel_graticule()` puts marginal longitide/latitude captions at the
    plot edge in the case when panel has no external border for
    graticule labels and panel grids are not the same.

-   Coastline is updated to version 2021-05-20T05:37.

### Version 3.9.2

-   Argument `expand` in `regrid()` now supports length 1 or 2 (or,
    coerced to length 2). Grid expansion is proportional to the side of
    square, which area is equal to area inside of boundary box.

-   Package **`ragg`** is added as suggested instead of package
    **`Cairo`**, which is removed from suggested due to poor support
    font families.

-   Improved handling for systems with unsupported “cairo” graphic
    system.

### Version 3.9.1

-   Consistence with imported package **sp** (&gt;=1.4-4) and suggested
    package **`sf`** (&gt;=0.9-6) is in progress with focus to
    “proj4string” declaration of CRS.

-   Additional value `shape` for `area` argument and additional value
    `pickpoint` for `geocode` argument in function `.glance()`.

-   Package **`geojsonsf`** (&gt;=2.0.0) is added as suggested for
    faster `sf`/GeoJSON import/export.

-   Package **`leaflet.providers`** is added as suggested.

-   `update_coastline()`: merging of splitted by meridian 180 polygons
    is changed.

-   Coastline is updated to version 2020-11-08T06:14

### Version 3.9 (transient)

-   MAJOR: Field `$proj4` in `ursaGrid` objects is renamed to `$crs` due
    to activity with PROJ library, but **`ursa`** internally still
    continues operating with PROJ4 strings. Projection (CRS) values can
    be extracted or replaced by `ursa_crs()` (or `ursa_proj4()`, synonym
    to `ursa_crs()`) functions.

### Version 3.8.20

-   Consistence with imported package **sp** (&gt;=1.4-0) and suggested
    package **`sf`** (&gt;=0.9-3) is in progress.

-   New argument `engine` in function `read_gdal()` for optional
    importind data via `sf::gdal_read()` function.

-   New argument `geometry` in `spatial_intersecion()` for desired
    output geometry of spatial features intersection (`enfine="sf"`).

-   New function `consistent_grid()` for regrid keeping dimension ratio.

-   Zero tail of GTiff palettes are cropped for plotting.

-   In `ursaProgressBar()` added argument `tail` with default value
    `FALSE`; use `TRUE` (previous behaviour) if indication is appeared
    after progress step.

-   In `compose_open` added value `CairoPNG` for agrument
    `device`/`type` for handled by **Cairo** package, which is added as
    suggested.

-   Packagae **`ggmap`** is removed from suggested list. Package
    **`rmarkdown`** is added as suggested. Package **`widgetframe`** is
    added as suggested for development purpose.

-   In `allocate()` added argument `resetGrid` with default value
    `FALSE` for resetting session grid before raster formation.

### Version 3.8.19

-   Minor adjustments for timing of examples.

### Version 3.8.18

-   `spatial_write()`: fixed deprecated (**`sf`** &gt;= 0.9-0) argument
    `update`-&gt;`append` in suggested `sf::sf_write()`.

-   Re-check “don’t test” examples.

### Version 3.8.17

-   Fixed for suggested package **`proj4`** (&gt;=1.0.10).

-   Tested on R-devel (r77878, r77936) and Rtools 4.0 (2020-02-05); no
    errors. For R-devel (r77936) suggested package **`raster`** should
    be reinstalled.

-   Added argument `title` in function `print()` for objects of class
    `ursaRaster` for optional header printing.

### Version 3.8.16

-   C-level: `memcpy` is replaced by `memmove` for overlapped memory
    areas.

-   C-level: `fseek` is replaced by `fseeko/fseeko64` for positions in
    large files.

-   Added argument `retina` in functions for plotting. Not supported for
    tiles.

-   Package **`RColorBrewer`** is added as suggested for cases when
    `pal` attribute in `colorize()` is equal to any palette name or
    category from **`RColorBrewer`**.

-   Fixed replace function `spatial_geometry<-` for Spatial (**`sp`**)
    objects.

-   The environental variable `PROJ_PATH` is temporal specified for GDAL
    utils for their execution, if GDAL is found (optional).

-   Coastline is updated to version 2020-01-09T05:30

### Version 3.8.15

-   Fixed behaviour for using in rmarkdown for self-contained documents

-   Path to R is taken from `R.home("bin")`

### Version 3.8.14

-   Fixed appeared “length &gt; 1 in coercion to logical” in examples
    during CRAN check.

-   Packages **`leaflet`**, **`leafem`**, **`leafpop`** are added as
    suggested for polar web mapping.

-   ‘http’ protocol is changed to ‘https’ one for `style=polarmap` in
    `glance()`

-   packages **`htmlwidgets`**, **`htmltools`** are added as suggested
    for development purpose without any next steps for involvement.

-   Added new argument `ref` to functions `band_blank()` and
    `ursa_blank()` for more flexible blank detection.

-   Changed interface to ‘mapnik’ (https instead of http) and
    ‘mapsurfer’ (Tokenized Openroute service) tile services.

-   Improved formatting of numeric labels in `legend_colorbar()`

-   Coastline is updated to version 2019-09-21T05:39.

### Version 3.8.13

-   R function are used for reading of large binary files, if their size
    overflows ‘long’ capacity.

-   Duplicated band names are allowed now for `print()` of `ursa`
    objects.

-   Cancelled replacement “ESRI WKT” by “OGC WKT” format of projection
    file when writing “ESRI Shapefile”.

-   Connections (writing and simplified reading) are made public:
    `spatial_write` and `spatial_read` functions.

-   File extensions `.webp` (if **webp** is installed), `.jpg`, `.jpeg`
    can be specified for attribute `fileout` for saving image file,
    produced by `display`, `glance`, `compose_open`. Package **webp** is
    added as suggested.

-   Coastline is updated to version 2018-12-13 09:34.

### Version 3.8.12

-   Default style for web basemaps is OSM Mapnik tiles insead of OSM
    static map.
    <!-- due to [limitations](http://staticmap.openstreetmap.de/staticmap.php). -->

-   Parsing of metadata from object of package **stars** (&gt;=0.2).

-   Domain changed from “arcticconnect.org” to “arcticconnect.ca” for
    `style=polarmap` in `glance()`

-   Coastline is updated to version 2018-11-17 09:50.

### Version 3.8.11

-   Package **tools** is added as *Suggested* for MD5 manipulations.

-   Selective improvement of compressed raster matrices in `c`,
    `display`. Still far for full support.

-   `obj[2] <- NULL` removes second band of raster object `obj`.

-   Package **rgeos** is added as *Suggested* for geometry operations
    with `sp` objects.

-   Added `spatial_intersection`, `spatial_symdifference`, and other
    functions for operations with geometries of spatial objects.

-   Added **Author@R** field in `DESCRIPTION`.

-   Changed behaviour for opening of visualization for `interactive()`
    session. Now, figures are opened outsize of GUI, by default. Use
    `session_pngviewer(FALSE)` for restoring of previous behaviour.

### Version 3.8.10

-   Forced UTF-8 encoding for non-shapefiles for “sp” engine

-   PNG is removed in embedded Rscript by `file.remove()` after
    `Sys.which()`

-   Accepting `sf::st_set_geometry(x,NULL)` for extracting
    `spatial_data()` of `sf` objects

### Version 3.8.9

-   Version 3.8.8 is removed from CRAN

-   Fixed opening ENVI .hdr Labelled Rasters for Read-only file systems

-   Fixed ignoring of `stdout` in `system2` for `interactive()`
    (appeared on Gui for Windows)

-   Added recipe to use visualization in **shiny**
    (`imageOutput`/`renderImage`)

### Version 3.8.8

-   Initial submission to CRAN

-   Coastline is updated to version 2018-06-03 09:34

-   Package `IRdisplay` is added as ‘Suggested’.

-   In adopting to CRAN policy and reviews of CRAN members:

    -   Reduced time of examples, fixed some features.

    -   Changed cache management (after Uwe Ligges code review).

    -   In \*.Rd files ‘\\dontrun}’ is replaced to ‘\\donttest}’ (after
        Swetlana Herbrandt review). Packages `caTools`, `ggmap` are
        added as ‘Suggested’ for running of examples.

### Version 3.8.7

-   Support importing of 3-dimensional
    ‘[stars](https://github.com/r-spatial/stars)’ arrays

-   Improvement of character encoding for attributes of spatial objects

### Version 3.8.6

-   Coastline is updated to version 2018-03-27 09:31

-   Corrected graticules and scalebar for ‘+proj=merc’ projection class
    for non zero latitude of true scale (e.g., ‘+lat\_ts=80.8’)

-   Package `fasterize` is suggested. Version of suggested package `sf`
    should be `>=0.6-1`.

### Version 3.8.5

-   Imporing results of `sf::gdal_read`.

-   Increased contrasting in cubehelix for low-colored palettes.

-   Fixed cubehelix for colorizing with `breakvalue=0`

-   Corrected band names for opening rasters using GDAL

-   Performance improvement for Group Summary (comparisson) of
    `ursaRaster` class.

-   Coastline is updated 2018-03-07

### Version 3.8.4

-   Added argument `coords` for function `allocate`.

-   New wrappers and checkers for spatial (vector GIS) objects.

-   Some issues for web services are fixed.

-   in DESCRIPTION file ‘exportPattern’ is replaced by multiple ‘export’
    for ability to use non-public funstions which names don’t start with
    “.”

-   In `panel_coastline` improved detection of polygons’ coodrinates
    spreading in result of reprojection

### Version 3.8-3

-   Visualization output is included to R-markdown document and
    R-Jupyter code without additional controls. Currently, some outputs
    are not supported (e.g., bookdown::gitbook)

### Version 3.8-2

-   Changed registration of native routines.

-   Package `knitr` is added as suggested.

-   In `spatial_coordinates()`: fixed return value for ‘POINT’ geometry
    for ‘sf’ engine.

-   In `glance()`: if all attributes are `NA`, then plot only geometry.

-   In `band_blank()` 1) fixed inaccuracy for values not in memory 2)
    added argument `verbose`.

-   In `cubehelix()` canceled auto brightness changing if `dark` and
    `light` are specified

-   In `read_envi` and `open_envi` added argument `cache` to use cache
    for compressed files.

### Version 3.8-1

-   Internal land polygons (coastline) data are replaced from GSHHG to
    OSM. Function `update_coastline` is added to update coastline data
    personally.

-   Caching is introduced for downloaded files.

-   Changed structure of `inst` directory by adding subdirectories
    `requisite` with neccessary files and `optional` with secondary
    files and directories.

-   Added family of functions `spatial_xxxxxxx` to retrieve properties
    from non-raster spatial objects: simple features (package **sf**)
    and Spatial abstract class (package **sp**).

### Version 3.7-19

-   Argument ‘attr’ is replaced to ‘field’ in internal functions
    `.spatialize` and `.glance`.

-   Argument ‘r’ is replaced to ‘rotate’ in public function `cubehelix`.

-   In coersion from “stack” to “brick”: if nodata values are the same,
    then nodata is assigned.

-   in `polygonize` added choice of “engine” by means applying functions
    from either **sp** or **sf** packages.

### Version 3.7-18

-   Improved consistence beetween ‘dim’ interity in non-public
    `.regrid()`

-   Supporting categories in exporting to `data.frame`

-   Back to patch of failure with ‘rgdal’ of Unix build machine at
    r-forge

### Version 3.7-17

-   Correction for bounding around 180 degree longitude

### Version 3.7-16

-   minor improvement to spatial allocation of vector objects with
    crossing of 180 degree longitude

-   allocate(): slightly improvement for regular grid detection

-   background for future functionality

### Version 3.7-15

-   added possibility of image annotation; argument ‘label’ in
    ‘panel\_annotation’ can be object of class ‘array’

-   fixed divergent coloring for (only) two values in ‘cubehelix()’

-   ‘ggmap’, ‘foreign’ are removed from the list of suggested packages;
    ‘ncdf4’ is added to the list of suggested packages.

### Version 3.7-14

-   fixed export to Raster(Layer|Brick|Stack) with NA nodata

### Version 3.7-13

-   gentle requirements to “chessboard” grid in ‘panel\_new()’

-   fix layout in ‘compose\_design()’ for images like strips

-   in suggestion, sf (&gt;= 0.5-0)

### Version 3.7-12

-   Minor changes for geocoded glance()

### Version 3.7-11

-   ‘nominatim’ geocoding for 180-longitute-objects is more correct, but
    traffic is higher

-   alternate geocoding service in the case of base one failure

### Version 3.7-10

-   Adaptation glance() for condition if argument “dsn” is “point”
    ‘c(lon,lat)’ and “boundary” ‘c(minx,miny,maxx,maxy)’

### Version 3.7-9

-   Vectors without data table - fixed

### Version 3.7-8

-   Bypass for ‘rgdal’ usage diring examples on r-forge UNIX building
    machine. E.g.: Error in dyn.load(file, DLLpath = DLLpath, …) :
    unable to load shared object
    ‘/home/rforge/lib/R/3.4/rgdal/libs/rgdal.so’: libgdal.so.1: cannot
    open shared object file: No such file or directory

### Version 3.7-7

-   Better matching for floating-point coordinates

### Version 3.7-6

-   Minor fixes for categories after resample

-   ‘glance()’ is recoded

### Version 3.7-5

-   Adaptation for R-exts’ “5.4 Registering native routines” for
    R-3.4.0.

### Version 3.7-4

-   Non-ascii for geocoding in ‘glance’

-   New function ‘get\_earthdata’ for retreiving MODIS mosaics.

-   Added package ‘jpeg’ in the category ‘Imported’.

### Version 3.7-3

-   Non-ascii for geocoding in ‘glance’

### Version 3.7-2

-   Introduce geocode to ‘glance’. There is no relation between data and
    geocoded place.

-   Introduce tiles to ‘glance’. Now static maps and tiles for basemap
    in “+proj=merc”

-   Dismiss from dQuote() and sQuote(), which put non-symmetrical quotes
    in Rgui; GDAL does’t understand it.

-   ‘inst/glance’ contains mock-up to create vector/raster file
    associantion with glance()

-   ‘glance’ can work without package ‘sf’; however “package:methods”
    should be in “search()”

-   Rename ‘panel\_gridline’ to ‘panel\_graticule’.

### Version 3.7-1

-   Public wrapper ‘glance()’ for non-public ‘.glance()’: quick-look of
    GIS raster and vector data

### Version 3.6-3

-   Documentation for ‘ursaProgressBar’

### Version 3.6-2

-   Added argument “…” to function ‘read\_gdal’. Now, if ‘as.ursa(uri)’
    or ‘display(uri)’, then additional arguments can be passed to
    ‘download.file’. For example, if you need ‘mode=“wb”’ or ignore
    certificate for HTTPS

### Version 3.6-1

-   Added ‘session\_pngviewer()’ and ‘session\_tempdir()’ to follow CRAN
    policy. If “Rscript”, then external software is used to open PNG;
    current directory is used to write files If ‘interactive()’ or “R
    CMD BATCH”, no external software for PNG; ‘tempdir()’ is used to
    write files

### Version 3.5-2

-   Initial submission to R-Forge

<!--
<style>
   code,
   kbd,
   pre {
     font-family: Cousine, Consolas, Menlo, "Liberation Mono", Courier, monospace;
     font-weight: regular;
     line-height: 115%;
     font-size: 90%;
     background-color: #E6F1FF;
   }
</style>
-->
