# Piezometric Well Data Analysis Tool


## Overview

This repository contains a web-based analytical tool developed in R/Shiny for rapid evaluation of piezometric well data. Originally designed for water depth analysis in Khorasan Razavi Province (Iran), the application provides:

- Interactive visualization of groundwater level trends
- Statistical analysis of well data
- Spatial mapping of aquifer systems
- Automated reporting capabilities

The tool has been deployed operationally by water resource management companies to streamline data review processes, reducing analysis time from days to minutes.

## Key Features

✔ **Excel Integration** - Direct processing of standard Excel data formats  
✔ **Interactive Dashboard** - User-friendly interface with real-time updates  
✔ **Automated QC** - Built-in outlier detection and data validation  
✔ **Multi-scale Analysis** - Individual well and aquifer-scale perspectives  
✔ **GIS Integration** - Spatial visualization with shapefile support  
✔ **Reporting Tools** - One-click export of charts and data  

## Technical Specifications

```r
# Core Functionality
- Data Cleaning: IQR-based outlier removal, NA handling
- Visualization: Dynamic scatter plots, histograms, and maps
- Spatial Analysis: SF/leaflet integration for aquifer mapping
- Output Generation: Excel reports and PNG figures
