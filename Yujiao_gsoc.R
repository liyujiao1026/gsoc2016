rm(list = ls())
 
library("spacetime")
library("sp")
library("RPostgreSQL")
library("rgdal")
library("rpostgis")
library("rgeos")
library("wkb")


#======================   Step 1. Construct SpatialPointDataFrame   ======================================#
data(fires)
fires$X <- fires$X * 100000
fires$Y <- fires$Y * 100000
fires$Time <- as.POSIXct(as.Date("1960-01-01") + (fires$Time - 1))
coordinates(fires) <- c("X", "Y")
proj4string(fires) <- CRS("+init=epsg:2229 +ellps=GRS80")

str(fires)





#=======================   Step 2. Connect to the PostgreSQL   =============================================#

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "mypostgis", host = "localhost",
                 port = 5432, user = "postgres", password = 1)

# Check whether the extension PostGis has been created well
# If not, create the extension now
extension <- try(dbSendQuery(con, "SELECT postgis_full_version();"), silent = TRUE)

if ('try-error' %in% class(extension)) {
        dbSendQuery(con, "CREATE EXTENSION POSTGIS;")
}

# "spatial_ref_sys" should be shown hereby because extension "PostGis" is created

# Create a new schema named by myself:)

dbSendQuery(con, "CREATE SCHEMA yujiao;")
dbSendQuery(con, "COMMENT ON SCHEMA yujiao IS 'Test result by YujiaoLi';")




#==========================   Step 3. Export to the PostgreSQL   =============================================#


#--- (3.1) Method 1: Use function "rgdal::writeOGR" to export (Work bad)-------------------|

# -----------------------------------------------------------------------------------------|
# *Pro: convenient and fairly straightforward syntax. 							    	                 |
#											                                                                     |
# *Con: It can only support several drivers. ( ogrDrivers() to check available drivers)    |
#       														                                                       |
#       Sometimes,Windows-OS cannot work well.                                             |
#       It depends on the individual user's installation of GDAL/OGR.                      |
#       "Rgdal" package installed from the CRAN repos includes its own basic gdal,         |
#       but it doesn't have the necessary driver.                                          |
# If availabe, use "writeOGR" write into postgis directly. Orelse, consider (3.2) solution |
#------------------------------------------------------------------------------------------|


if ("PostgreSQL" %in% (ogrDrivers()$name)) {
        writeOGR(
                fires, driver = "PostgreSQL", "PG:dbname=mypostgis host=localhost", layer = "data"
        )
} else {
        stop(   "Dear friend, perhaps we could give up 'rgdal' and use other solutions.
                \n this depends on the GDAL installation on your OS. :)"
                )
}

# For us reference
file.show(system.file("README", package = "rgdal"))
file.show(system.file("README.windows", package = "rgdal"))

# Surely we can use "writeOGR" to convert as shapefile. And then export into PostGis,
# But it produce the unecessary table and wasting time.






#---  (3.2) Method 2: Convert spatial object to WKT string, and then export into PostGIS -----------------#

# --------------------------------------------------------------------------------------------------------|
#  *Pro: Can be generalized to other geometry object, not only limited in spatial-point data	    	      |
#	   WKT format is rather readable by human than WKB								    				                          |
#	   Exported data type(non-sp part) is kept consistent with raw data.No need to set datatype again       |
#            e.g. Variable "Time" can be recognized and saved as "timestamp with time zone" automatically |
#        When Converted from WKT to geometry, SRID can be either set well or not. No need to check and set|
#        Circular Strings and Curves are also supported	                                                  |
#        "EWKT" can be extended further to allow 3 dimensional data                                       |
#							                                                                                            |
#  *Con: "WKT" is not as faster as "WKB" to process data                                                  |
#        Not well-wrapped function in the package, this can be done later.                                |
#  *Note: row.names column was automatically generated.It might be useful for setting primary key. 	      |
#---------------------------------------------------------------------------------------------------------|

wkt <- writeWKT(fires,byid = TRUE)
class(wkt)  # convert spatial object (coordinates here) to WellKnownText string

## Note: byid=TRUE allow to have results individually, it is easier to export data one by one.
## Or else it turns to be "GEOMETRYCOLLECTION" which is one geometry

wktdata <- data.frame(fires@data,wkt)
class(wktdata$wkt)  # factor
wktdata$wkt <- as.character(wktdata$wkt)
class(wktdata$wkt)  # Converting factor to character aims for later SQL statement
head(wktdata)

dbWriteTable(con, c("yujiao" , "wktdata") , wktdata)
dbSendQuery(con,  "ALTER TABLE yujiao.wktdata ADD geom geometry;")


query <- paste0("UPDATE yujiao.wktdata SET geom= ST_GeomFromText(\"wkt\");")
dbSendQuery(con,query)




#---  (3.3) Method 3: Convert spatial object to WKB string,  and then export into PostGIS  ---------------#

# --------------------------------------------------------------------------------------------------------|
#  *Pro: faster than WKT when data is large, but I need make experiments more times to test               |									 				                                                                                               	|
#  *my thought: Compared with WKT, WKB is the binary format, I did not see too much difference in SQL     |
#---------------------------------------------------------------------------------------------------------|


wkb <- writeWKB(fires)
wkbdata <- data.frame(fires@data, wkb)
wkbdata$geom <- as.character(wkbdata$wkb)
head(wkbdata)

dbWriteTable(con, c("yujiao","wkbdata"),wkbdata)

que <- "ALTER TABLE yujiao.wkbdata ADD geom2 geometry;"
dbSendQuery(con,que)

query <- paste0("UPDATE yujiao.wkbdata SET geom2= ST_GeomFromWKB(\"geom2\");")
dbSendQuery(con,query)






#---  (3.4) Method 4: Use ST_MakePoint (Manually & rpostgis::pgMakePts)-------------------------------------#

# ----------------------------------------------------------------------------------------------------------|
#  *Pro: 																	                                                                  |
#       The function of "pgMakePts" develop the smooth prodecure                                            |
#          (1)add geometry column:   "ALTER TABLE yujiao.wkbdata ADD COLUMN geom4 geometry(POINT, 32632);"  |
#          (2)add index on geometry: "CREATE INDEX wkbdata_geom4_idx ON yujiao.wkbdata USING GIST (geom4);" |
#          (3)update geomtry:        "UPDATE yujiao.wkbdata SET geom4=                                      |
#                                     ST_SetSRID(ST_MakePoint(X, X), 32632);"                               |
#											                                         					                                    |
#  *Con: can be only used in "POINT" or "LINESTRING" geometry cases.                                        |
#        cannot generalize well to other geometry obj: spatial lines, multilines, multipolygons etc.        |
#																											                                                      |
#																									                                                       		|
#  *Note: Bug in package "rpostgis"                                                   											|
#              Function "pgMakePts()" did not follow the argument of "x" and "y" in its document.           |
#		   Document states: x and y is the NAME of coordinates, 											                          |
#                               but it can only accept the VALUE instead of NAME in this function.          |
#		                                                                          				                			|
#-----------------------------------------------------------------------------------------------------------|

## transform to data.frame and then add geometry column

dbWriteTable(con, c("yujiao","pkgdata") ,data.frame(fires))

que <- "ALTER TABLE yujiao.pkgdata ADD newgeom geometry;"
dbSendQuery(con,que)

# (3.4.1) and (3.4.2) are basically the same function.

# (3.4.1) use ST_MakePoint
que1 <- "UPDATE yujiao.pkgdata SET newgeom= ST_SetSRID(ST_MakePoint(\"X\",\"Y\"),32632);"
dbSendQuery(con,que1)
dbListTables(con)


# (3.4.2) use rpostgis::pgMakePts

#----------------------------------------------------------------------------------------------------------#
# NOTE: There is one error in pgMakePts() function Document								                                 #
# which one is right for argument of x and y?    VALUE of coordinates   v.s.  NAME of coordinates(document)#
# Official document said it should be NAME, but my test result is VALUE works well. See my following test  #
#----------------------------------------------------------------------------------------------------------#

# Work well    (VALUE of coordinates)
xc <- coordinates(fires)[,1]
yc <- coordinates(fires)[,2]

pgMakePts(
        con, c("yujiao","pkgdata"), colname = "geom_pkg33", x = xc, y = yc, srid = 32632,
        index = TRUE, display = TRUE, exec = TRUE
)

# Not Work well (NAME of coordinates)
pgMakePts(
        con, c("yujiao","pkgdata"), colname = "geom_pkg", x = "X", y = "Y", srid = 32632,
        index = TRUE, display = TRUE, exec = TRUE
)




#=======================   Step 4. Build spatial and temporal indexes =====================================#

#--- Method 1: Use sql-------------------------------------------------------------------------------------#

query1 <- ' CREATE INDEX time_index ON yujiao.wkbdata ("Time");'
dbSendQuery(con, query1)

query2 <- ' CREATE INDEX sp_index ON yujiao.wkbdata USING GIST(geom2);'
dbSendQuery(con, query2)




#--- Method 2: Use rpostgis package (BUG existed and my suggestions of updating source code)--#

# When we follow the package document to run following code,
# it will unreasonably return :"ERROR:  column "time" does not exist"

pgIndex(
        con, name = c("yujiao","wktdata"), colname = "Time", unique = FALSE,
        method = "btree", display = TRUE,  exec = TRUE
)     ## error due to the quotation in source file of package


# --------------     Source file of "rpostgis::pgIndex()" updated suggestions --------------#
#																						                                                |
# My suggestion is to modify the source file "pgIndex.r"							                      |
# https://github.com/basille/rpostgis/blob/master/R/pgIndex.r							                  |
#																						                                                |
# The error is in line 53 																                                  |
# Old code:          " (", colname, ");")												                            |
# Updated code:      " (\"", colname, "\" );")						            		                  |
#															                                                              |
# If we use "rpostgis" package, the generated sql statement is automatically: 			        |
#            str = "CREATE INDEX wktdata_Time_idx ON yujiao.wktdata (Time);			            |
#            Apparently, the last part should be quoted as ("Time") instead of (Time)	      |
#															                                                              |
#-------------------------------------------------------------------------------------------#







#=======================   Step 5. Build SQL function to retrieve  =========================#
## retrieve
##
## Retrieve the data given the condition about spatial coordianates and time
##'
##' @connet Converts to timestamp
##' @tableName is the table for retrieve
##' @geomName is the "WKB" or "WKT" type of geometry column for spatial data.
##' @xmin,xmin,xmax,ymin,ymax,tmin,tmax is the boundary of retrived condition.



retrieve <-
        function(connect, tableName, geomName, xmin,xmax, ymin,ymax, tmin,tmax) {
                if (!(length(tableName) %in% 1:2)   |
                    !(tableName[2] %in% dbListTables(con))) {
                        stop("data is not available")
                }else{
                        str0 <- sprintf(
                                "SELECT * FROM %s
                                WHERE ST_X(%s)> %d  AND   ST_X(%s)< %d AND
                                ST_Y(%s)> %d  AND   ST_Y(%s)< %d AND
                                \"Time\"> '%s' AND \"Time\" < '%s';"  ,
                                
                                tbname,
                                geomName,xmin,geomName,xmax,
                                geomName,ymin,geomName,ymax,
                                tmin,tmax
                                )
                        
                        Result <- dbGetQuery(con,  str0)
                }
                return(Result)
        }


## ---------------------------------------------------------------------------------#
## Single quotation and double qutation are rather different in query statement!!   #
## I cost too much time to debug this point.                                        #
##----------------------------------------------------------------------------------#


# Test the function "retrieve"

# assign the parameter in function
connect <- con
tableName <- c("yujiao","pkgdata")
geomName <- "newgeom"
xmin     <- 6400000
xmax     <- 6500000
ymin     <- 1950000
ymax     <- 2050000
tmin     <- "1990-01-01"
tmax     <- "2000-01-01"


# run the function
retrieve(connect,  tableName,  geomName,
         xmin, xmax,  ymin, ymax,  tmin, tmax)


dbDisconnect(con)

# Thanks for watching! :)
