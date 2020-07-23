#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 26 02:40:19 2020

@author: wcurrier - modificado por fmanquehual
"""
##


## modificar  'get_dataset.py' para ver el tema de la descarga de variables (problemas con la direccion de descarga en CEDA)


##def download(model, var_name, run="r1i1p1", institute=None,
##            domain="atmos", interval="6hr",
##            start_time="20900101", end_time="21000101",
##            scenario="rcp85"):
##    """Download a requested variable from the CEDA ftp site"""




import numpy as np
import pandas as pd
import xarray as xr
import dask
import glob
from cdo import *
import os
import sys
from datetime import datetime
import cftime

sys.path.append('/home/msomos/Documentos/proyecto_DownscaleR/cmip_ingest/scripts')
from download import get_dataset

sys.path.append('/home/msomos/Documentos/proyecto_DownscaleR/cmip_work')
import cmipFunctions

cdo = Cdo()

# Parameters
# Slice/Subset data down further based on latitutde (-90 -- +90), longitude (0 -- 360)

#####################################################
############## CHANGE THIS INFORMATION ##############
#####################################################

ins         = 'MIROC'        # BCC, NCAR
model       = 'MIROC6'  # bcc-csm1-1, ACCESS1-0
scen        = 'ssp585'  # historical, rcp45, rcp85
ensemble    = 'r1i1p1f1'      # r6i1p1, r1i1p1
version     = 'v20191114'   # v1, v20121128
workingDir  = '/home/msomos/Documentos/proyecto_DownscaleR/ej_descarga'
lat_bnds, lon_bnds = [-48, -41], [284, 289]

## IMPORTANTE: solo se puede descargar info anual (no mensual ni diario, ver: http://data.ceda.ac.uk/badc/cmip5/data/cmip5/output1/CSIRO-QCCCE/CSIRO-Mk3-6-0/historical/6hr/atmos/6hrLev/r1i1p1/v20120607/ta)
start_date = "20010101" # Used for downloading data 
end_date   = "20020101" # Used for downloading data
 # Dates used for subsampling
# startDates = pd.to_datetime(['1950-01-01 12:00:00','1960-01-01 06:00:00','1970-01-01 06:00:00','1980-01-01 06:00:00','1990-01-01 06:00:00','2000-01-01 06:00:00'],format='%Y-%m-%d %H:%M:%S') # original
# endDates   = pd.to_datetime(['1960-01-01 00:00:00','1970-01-01 00:00:00','1980-01-01 00:00:00','1990-01-01 00:00:00','2000-01-01 00:00:00','2006-01-01 00:00:00'],format='%Y-%m-%d %H:%M:%S') # original

startDates = pd.to_datetime(['2001-01-01 06:00:00'],format='%Y-%m-%d %H:%M:%S')
endDates   = pd.to_datetime(['2002-01-01 00:00:00'],format='%Y-%m-%d %H:%M:%S')
#####################################################
############## NO NEED TO CHANGE BELOW ##############
#####################################################

def process(mip="cmip6",ins='NCAR',model="CCSM4",ensemble="r6i1p1",scen="historical"):

    lat_bnds, lon_bnds = [-48, -41], [284, 289] # nuevo
    
    # Need to write these in:
    if scen=='historical':
        start_date = "19500101" # Used for downloading data
        end_date   = "20060101" # Used for downloading data
        startDates = pd.to_datetime(['1950-01-01 12:00:00','1960-01-01 06:00:00','1970-01-01 06:00:00','1980-01-01 06:00:00','1990-01-01 06:00:00','2000-01-01 06:00:00'],format='%Y-%m-%d %H:%M:%S')
        endDates   = pd.to_datetime(['1960-01-01 00:00:00','1970-01-01 00:00:00','1980-01-01 00:00:00','1990-01-01 00:00:00','2000-01-01 00:00:00','2006-01-01 00:00:00'],format='%Y-%m-%d %H:%M:%S')
    else :
        start_date = "20060101" # Used for downloading data
        end_date   = "21000101" # Used for downloading data
        startDates = pd.to_datetime(['2006-01-01 00:00:00','2010-01-01 06:00:00','2020-01-01 06:00:00','2030-01-01 06:00:00','2040-01-01 06:00:00','2050-01-01 06:00:00','2060-01-01 06:00:00','2070-01-01 06:00:00','2080-01-01 06:00:00','2090-01-01 06:00:00'],format='%Y-%m-%d %H:%M:%S')
        endDates   = pd.to_datetime(['2010-01-01 00:00:00','2020-01-01 00:00:00','2030-01-01 00:00:00','2040-01-01 00:00:00','2050-01-01 00:00:00','2060-01-01 00:00:00','2070-01-01 00:00:00','2080-01-01 00:00:00','2090-01-01 00:00:00','2100-01-01 00:00:00'],format='%Y-%m-%d %H:%M:%S')

    # Check if we've already created a directory for subsetted/processed data
    outDir = workingDir+'/'+model+'/'+scen+'/' # must have back slash # nuevo
    
    if os.path.isdir(outDir) == False : # Make output directory if it doesn't exist
        os.makedirs(outDir)
        print("created out directory: "+outDir)
    else:
        print("Out directory already exists: "+outDir)

    
    ####### TEMPERATURE
    
    # Load in Temperature Data First
    taFiles=glob.glob(outDir+'ta*.nc') # Get list of files already subsetted
    
    # el error '' se corrige anhadiendo el modelo e institucion en 'models.py' de la carpeta 'cmip_ingest'
       if len(taFiles) == 0:
        os.chdir(outDir)
        get_dataset.download(institute=ins, model=model,var_name="ta",domain="atmos",interval='6hr', run=ensemble, scenario=scen, start_time=start_date, end_time=end_date)
    
    cmipFunctions.loadNonStaggeredVars(outDir,outDir,'ta',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)

    taFilesSub=glob.glob(outDir+'ta*subset.nc') # Get list of files already subsetted
    if len(taFilesSub) == 0:              # If list of files haven't been subsetted, subset them
        cmipFunctions.loadNonStaggeredVars(outDir,outDir,'ta',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)
    else:
        print("Subsetted atmospheric temperature data already existed")

    ####### SEA SURFACE TEMEPRATURE

    # daily, historical, rcp45, rcp85, sea surface temperature data doesn't exist on cheyenne
    # download first - except for NCAR/CCSM4 and NSF-DOE-NCAR-CESM1-WACCM

    tosFiles=glob.glob(outDir+'tos*.nc') # Get list of files already subsetted
    tosChyDir='/glade/collections/cmip/cmip5/output1/'+ins+'/'+model+'/'+scen+'/day/ocean/day/'+ensemble+'/latest/tos/' # original
    tosChyFiles=sorted(glob.glob(tosChyDir+'*.nc'))
    if len(tosFiles) == 0 :              # If list of files haven't been subsetted/downlaoded - check to see if they're on Cheyenne
        if len(tosChyFiles) == 0:        # If files don't exist on Cheyenne download them
            os.chdir(outDir)
            get_dataset.download(model=model,var_name="tos",domain="ocean",interval="day", run=ensemble,\
                                 scenario=scen, start_time=start_date, end_time=end_date)

    # Convert the TOS data from rotated pole to temperature data
    tosFilesRegrd=glob.glob(outDir+'tos_day*_regrd.nc') # ORIGINAL
    if len(tosFilesRegrd) == 0: 
        if len(tosChyFiles) > 0: # if TOS files are on cheyenne - inDir = tosChyDir
            os.chdir(outDir)
            cmipFunctions.convertTos(outDir,tosChyDir)
        else: # we downloaded the data: inDir = outDir
            os.chdir(outDir)
            cmipFunctions.convertTos(outDir,outDir)
    # Convert the
    tosFilesSub=glob.glob(outDir+'tos_6hr*_regrd_subset.nc') # original # NO CORRE !!!!!!
    #tosFilesSub=glob.glob(outDir+'tos_day*_regrd_subset.nc') # nuevo
    if len(tosFilesSub) == 0:
        cmipFunctions.processTOS(outDir,outDir,'tos',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates)
    else:
        print("Subsetted sea surface temperature data already existed")

    ###### Humidity Data
    husFiles=glob.glob(outDir+'hus*.nc')
    #if chyColl == False and len(husFiles)==0: # deshabilitado
        os.chdir(outDir)
        get_dataset.download(model=model,var_name="hus",domain="atmos",interval="6hr", run=ensemble,\
                             scenario=scen, start_time=start_date, end_time=end_date)
        cmipFunctions.loadNonStaggeredVars(outDir,outDir,'hus',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)

    husFilesSub=glob.glob(outDir+'hus*subset.nc')
    if len(husFilesSub) == 0:
        cmipFunctions.loadNonStaggeredVars(outDir,outDir,'hus',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)
    else:
        print("Subsetted specific humidity data already existed")

    ###### U Data
    uFiles=glob.glob(outDir+'ua*.nc')
     #if chyColl == False and len(uFiles)==0: # deshabilitado
        os.chdir(outDir)
        get_dataset.download(model=model,var_name="ua",domain="atmos",interval="6hr", run=ensemble,\
                             scenario=scen, start_time=start_date, end_time=end_date)
        cmipFunctions.loadStaggeredVars(outDir,outDir,'ua',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)

    uFilesSub=glob.glob(outDir+'ua*subset.nc')
    if len(uFilesSub) == 0:
        cmipFunctions.loadStaggeredVars(outDir,outDir,'ua',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)
    else:
        print("Subsetted U wind speed data already existed")

    ###### V Data
    vFiles=glob.glob(outDir+'va*.nc')
    # if chyColl == False and len (vFiles)==0: # deshabilitado
        os.chdir(outDir)
        get_dataset.download(model=model,var_name="va",domain="atmos",interval="6hr", run=ensemble,\
                             scenario=scen, start_time=start_date, end_time=end_date)
        cmipFunctions.loadStaggeredVars(outDir,outDir,'va',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)

    vFilesSub=glob.glob(outDir+'va*subset.nc')
    if len(vFiles) > 0 and len(vFilesSub) == 0: # if files were already downloaded - use them
        cmipFunctions.loadStaggeredVars(outDir,outDir,'va',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)
    elif len(vFilesSub) == 0:
        cmipFunctions.loadStaggeredVars(outDir,outDir,'va',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)
    else:
        print("Subsetted V wind speed data already existed")

    ###### Surface Pressure
    psFiles=glob.glob(outDir+'ps*.nc')
    # if chyColl == False and len(psFiles)==0: # deshabilitado
        os.chdir(outDir)
        get_dataset.download(model=model,var_name="ps",domain="atmos",interval="6hr", run=ensemble,\
                             scenario=scen, start_time=start_date, end_time=end_date)
        cmipFunctions.loadNonStaggeredVars(outDir,outDir,'ps',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)

    psFilesSub=glob.glob(outDir+'ps*subset.nc')
    if len(psFilesSub) == 0:
        cmipFunctions.loadNonStaggeredVars(outDir,outDir,'ps',model,scen,ensemble,lat_bnds,lon_bnds,startDates,endDates,False)
    else:
        print("Subsetted surface pressure data already existed")

    ###### Precipitation # ESTABA DESHABILITADO
    # daily, historical, rcp45, rcp85, sea surface temperature data doesn't exist on cheyenne - download first
     prcFiles=glob.glob(outDir+'prc*.nc')
     if len(prcFiles) == 0 :
         os.chdir(outDir)
         #get_dataset.download(model=model,var_name="prc",domain="atmos",interval="3hr", run=ensemble, scenario=scen, start_time=start_date, end_time=end_date) # original
         get_dataset.download(model=model,var_name="prc",domain="atmos",interval="6hr", run=ensemble, scenario=scen, start_time=start_date, end_time=end_date) # nuevo
         # NO HUBIERON DATOS PARA INTERVALOS DE 6 HRS!

    start = ['1960-01-01 06:00:00','1970-01-01 06:00:00','1980-01-01 06:00:00','1990-01-01 06:00:00']
    end   = ['1970-01-01 00:00:00','1980-01-01 00:00:00','1990-01-01 00:00:00','2000-01-01 00:00:00']
    startDatesPrc = pd.to_datetime(start, format='%Y-%m-%d %H:%M:%S')
    endDatesPrc   = pd.to_datetime(end,   format='%Y-%m-%d %H:%M:%S')
    # Precipitation is 3D, only time,lat,lon - stored in longer time files in 3 hr data
    prcFiles=glob.glob(outDir+'prc*subset.nc')
    if len(prcFiles) == 0:
        cmipFunctions.loadNonStaggeredVarsResample(outDir,outDir,'prc',model,scen,ensemble,lat_bnds,lon_bnds,startDatesPrc,endDatesPrc,False,'6hr','sum')
        ###### Precipitation # ESTABA DESHABILITADO HACIA ARRIBA
        
        # ORIGINAL
#    dsT    = xr.open_mfdataset(outDir+'ta*subset.nc',combine='by_coords')
#    dsTOS  = xr.open_mfdataset(outDir+'tos*subset.nc',combine='by_coords')
#    dsHus  = xr.open_mfdataset(outDir+'hus*subset.nc',combine='by_coords')
#    dsU    = xr.open_mfdataset(outDir+'ua*subset.nc',combine='by_coords')
#    dsV    = xr.open_mfdataset(outDir+'va*subset.nc',combine='by_coords')
#    dsPrc  = xr.open_mfdataset(outDir+'prc*subset.nc',combine='by_coords') # estaba deshabilitado
#    dsPrcO = xr.open_mfdataset(outDir+'prc_3hr*.nc',combine='by_coords') # estaba deshabilitado
#    dsPs   = xr.open_mfdataset(outDir+'ps*subset.nc',combine='by_coords')
        

    dsT    = xr.open_mfdataset(outDir+'ta*subset.nc',combine='by_coords')
    dsTOS  = xr.open_mfdataset(outDir+'tos*subset.nc',combine='by_coords')
    dsHus  = xr.open_mfdataset(outDir+'hus*subset.nc',combine='by_coords')
    dsU    = xr.open_mfdataset(outDir+'ua*subset.nc',combine='by_coords')
    dsV    = xr.open_mfdataset(outDir+'va*subset.nc',combine='by_coords')
#    dsPrc  = xr.open_mfdataset(outDir+'prc*subset.nc',combine='by_coords') # estaba deshabilitado
#    dsPrcO = xr.open_mfdataset(outDir+'prc_3hr*.nc',combine='by_coords') # estaba deshabilitado
    dsPs   = xr.open_mfdataset(outDir+'ps*subset.nc',combine='by_coords')

    # Calculate the model level heights [m] or Pressure Levels
    try:
        z_t=dsT.lev+(dsT.b*dsT.orog)
        print("\nCalculating model Level Heights")
    except:
        P = dsT['a']*dsT['p0'] + dsT['b']*dsT['ps']
        print("Computed pressure from surface pressure")
        pressure  = True
        elevation = False
    else:
        elevation = True
        pressure  = False

    # Calculate water vapor mixing ratio (w/Qv) [kg/kg] from specific humdiity
    print("\n Converting from Specific Humidity to Water Vapor Mixing Ratio, Qv = hus/1-hus")
    dsHus['Qv'] = dsHus['hus']/(1-dsHus['hus'])

    # Creating New Dataset
    print("\nCreating new dataset\n")

    # Create New Datasets that's just the datasets with dimensions and coordinates

    ######## Terrain Height
    dsOrog = xr.Dataset({"HGT":(("lat","lon"),orogDsSub.orog)},coords={"lat":orogDsSub.lat,"lon":orogDsSub.lon})
    dsOrog['HGT'].attrs['standard_name'] = orogDsSub['orog'].standard_name
    dsOrog['HGT'].attrs['long_name']     = orogDsSub['orog'].long_name
    dsOrog['HGT'].attrs['units']         = orogDsSub['orog'].units
    dsOrog['HGT'].attrs['Processing Note'] = 'Provided orogoraphy file - orogoraphy variable'
    print('Loaded the orography data')
    if elevation:
        ######## 3D Model Level Heights [m]
        ds_Z    = xr.Dataset({"Z":(("lev","lat","lon"),z_t)},
                     coords={"lev":tads_sub.lev,"lat":tads_sub.lat,"lon":tads_sub.lon})
        ds_Z['Z'].attrs['standard_name'] = '3D model level heights'
        ds_Z['Z'].attrs['long_name'] = '3D Model Level Heights'
        ds_Z['Z'].attrs['units'] = 'm'
        ds_Z['Z'].attrs['Processing Note'] = 'Calculated using temperature lev coordinates, b values, and orogoraphy varaibles. Z = lev + b * orog'
        print('Loaded the model level elevations')
    if pressure:
        ds_P = xr.Dataset({"P":(("time","lev","lat","lon"),P)},
                     coords={"time":dsT.time,"lev":P.lev,"lat":P.lat,"lon":P.lon})
        ds_P['P'].attrs['standard_name'] = 'pressure'
        ds_P['P'].attrs['long_name'] = 'pressure'
        ds_P['P'].attrs['comment'] = 'calculated from subsetted 6 hr temperature data file: a*p0+b*Ps'
        ds_P['P'].attrs['units'] = 'Pa'
        print('Loaded the 4D pressure data')


    ######## Surface Air Pressure [Pa]
    ds_Ps   = xr.Dataset({"Ps":(("time","lat","lon"),dsPs.ps)},
                     coords={"time":dsPs.time,"lat":dsPs.lat,"lon":dsPs.lon})
    ds_Ps['Ps'].attrs = dsPs.ps.attrs
    print('Loaded the surface air pressure data')

    ######## Sea Surface Temeperature [K]
    ds_SST  = xr.Dataset({"SST":(("time","lat","lon"),dsTOS.tos)},
                     coords={"time":dsTOS.time,"lat":dsT.lat,"lon":dsT.lon})
    ds_SST['SST'].attrs = dsTOS['tos'].attrs
    ds_SST['SST'].attrs['Processing Note'] = 'Daily data forward filled to 6H data'
    print('Loaded the sea surface temperature data')

    ######## Precipitation [kg m-2 s-1]
    # ds_prec = xr.Dataset({"prec":(("time","lat","lon"),dsPrc.prc)},
    #                  coords={"time":dsPrc.time,"lat":dsPrc.lat,"lon":dsPrc.lon})
    # ds_prec['prec'].attrs['standard_name'] =dsPrcO['prc'].attrs['standard_name']
    # ds_prec['prec'].attrs['long_name'] = dsPrcO['prc'].attrs['long_name']
    # ds_prec['prec'].attrs['comment'] = 'at surface. This is a 6-hour mean convective preciptiation'
    # ds_prec['prec'].attrs['units'] = dsPrcO['prc'].attrs['units']
    # ds_prec['prec'].attrs['cell_methods'] = dsPrcO['prc'].attrs['cell_methods']
    # ds_prec['prec'].attrs['cell_measures'] = dsPrcO['prc'].attrs['cell_measures']
    # ds_prec['prec'].attrs['associated_files'] = dsPrcO['prc'].attrs['associated_files']
    # ds_prec['prec'].attrs['Processing Note'] = 'Resample from 3H data to 6H data using the sum between time-steps'

    ######## Air Temperature [K]
    ds_T = xr.Dataset({"T":(("time","lev","lat","lon"),dsT.ta)},
                      coords={"time":dsT.time,"lev":dsT.lev,"lat":dsT.lat,"lon":dsT.lon})
    ds_T['T'].attrs = dsT.ta.attrs
    print('Loaded the air temperature data')

    ######## Water Vapor Mixing Ratio [kg/kg]
    ds_Qv = xr.Dataset({"Qv":(("time","lev","lat","lon"),dsHus.Qv)},
                       coords={"time":dsHus.time,"lev":dsHus.lev,"lat":dsHus.lat,"lon":dsHus.lon})
    ds_Qv['Qv'].attrs['standard_name'] = 'Water Vapor Mixing Ratio'
    ds_Qv['Qv'].attrs['long_name'] = 'Water Vapor Mixing Ratio'
    ds_Qv['Qv'].attrs['units'] = 'kg/kg'
    ds_Qv['Qv'].attrs['cell measures'] = dsHus['hus'].attrs['cell_measures']
    ds_Qv['Qv'].attrs['associated_files'] = dsHus['hus'].attrs['associated_files']
    ds_Qv['Qv'].attrs['Processing Note'] = 'Calculated from specific humidity (q) data Qv = q/1-q'
    print('Loaded the water vapor mixing ratio data')

    ######## North South Wind Speeds [m s-1]
    ds_v = xr.Dataset({"V":(("time","lev","lat","lon"),dsV.va)},
                      coords={"time":dsV.time,"lev":dsT.lev,"lat":dsV.lat,"lon":dsV.lon})
    ds_v['V'].attrs = dsV.va.attrs
    ds_v['V'].attrs['Processing Note'] = 'Interpolated/Regrided from staggered north-south grid - one additional row offset by 1/2 a grid cell in latitude to the temperature grid'
    print('Loaded the V wind speed data')

    ######## East Wind Wind Speeds [m s-1]
    ds_u = xr.Dataset({"U":(("time","lev","lat","lon"),dsU.ua)},
                      coords={"time":dsU.time,"lev":dsT.lev,"lat":dsU.lat,"lon":dsU.lon})
    ds_u['U'].attrs = dsU.ua.attrs
    ds_u['U'].attrs['Processing Note'] = 'Interpolated/Regrided from staggered east-west grid - one additional column offset by 1/2 a grid cell in longitude to the temperature grid'
    print('Loaded the U wind speed data')

    print('Merging the dataset')
    # Make a new dataset
    if elevation:
        ds = xr.merge([dsOrog, ds_Z, ds_Ps, ds_SST, ds_T, ds_Qv, ds_v, ds_u])
    elif pressure:
        ds = xr.merge([dsOrog, ds_P, ds_Ps, ds_SST, ds_T, ds_Qv, ds_v, ds_u])
    print('Merged dataset - fixing longitudes: 0-359 to -180-179')

    # Convert longitude coordinates from 0-359 to -180-179
    ds['lon']=(((ds['lon'] + 180) % 360) - 180)
    ds['lon'].attrs['units']         = 'degrees_east'
    ds['lon'].attrs['comment']       = 'LONGITUDE, WEST IS NEGATIVE'
    ds['lon'].attrs['axis']          = 'X'
    ds['lon'].attrs['long_name']     = 'longitude'
    ds['lon'].attrs['standard_name'] = 'longitude'

    ds['SST']=ds['SST'].ffill('time')
    print('Filling in SST if any NaNs -vforward fill')

    now = datetime.now()
    ds.attrs['Condensed/Merged File Created'] = now.strftime("%m/%d/%Y, %H:%M:%S")
    ds.attrs = dsT.attrs
    print("Created new dataset")

    # Write out files - write out a new directory
    if os.path.isdir(outDir+'forcing/') == False:
        print("Writing new directory: "+outDir+'forcing/')
        os.mkdir(outDir+'forcing/')

    allFiles=glob.glob(outDir+'forcing/'+model+'_6hrLev_'+ scen +'_'+ ensemble +'*' +'_subset.nc')
    if len(allFiles) == 0:
        print("Writing out merged files to 10 year periods")
        for startDateLs, endDateLs in zip(startDates, endDates):
            ds_sub_Time  = ds.sel(time=slice(startDateLs.strftime('%Y-%m-%dT%H:%M:%S'),endDateLs.strftime('%Y-%m-%dT%H:%M:%S')))  # Time slice
            ds_sub_Time.to_netcdf(outDir+'forcing/'+model+'_6hrLev_'+ scen +'_'+ ensemble +'_' + startDateLs.strftime('%Y%m%d')+ '-'+endDateLs.strftime('%Y%m%d') +'_subset.nc')
    else:
        print("Files already existed in forcing directory - did not overwrite")
