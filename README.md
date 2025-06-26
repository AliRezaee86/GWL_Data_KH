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
```
### Quick Start

## 1. Clone the repository
```bash
git clone https://github.com/yourusername/well-analysis-tool.git
cd well-analysis-tool
```
## 2. Install Required R Packages
Run the following command in R or RStudio console:
```r
install.packages(c("shiny", "ggplot2", "leaflet", "sf", "openxlsx", "dplyr", "tidyr", "readxl"))
```
## 3. Data Preparation

1. Create a `data` folder in your project directory
2. Add required files:
   - `Well_Data.xlsx`: Water depth measurements (columns: Year, Month, Well1, Well2,...)
   - `Well_Locations.xlsx`: Well metadata (columns: Name_Well, Lat, Lon, AQ)
3. Optional file:
   - `Aquifer_Boundaries.shp`: Shapefile for spatial visualization (with .shx, .dbf, .prj companion files)
### Workflow Diagram

```mermaid
graph TD
    A[📌 Select Aquifer] -->|Filters available wells| B[⚙️ Choose Wells]
    B -->|Loads time-series| C[📊 View Statistics]
    C -->|Processes data| D[📈 Generate Plots]
    D -->|Formats output| E[💾 Export Results]
