# global coastline age raster and shelf sea rasters 

Repository with functions and scripts to generate global coastline age raster (AGE) and shelf sea rasters (SLW140). The generated raster datasets were the input for analysis of the manuscript entitled _Global raster dataset on historic coastline positions and shelf sea extents since the Last Glacial Maximum_. Generated datasets are available in Figshare at https://figshare.com/s/aa0316ce41225918bc8d. To generate the datasets we required (1) a bathymetric model and (2) a spatio-temporal sea level curve. 

# Bathymetry 

For the bathymetry we used the General Bathymetric Chart of the Oceans ([GEBCO 2019](https://www.gebco.net/data_and_products/historical_data_sets/#gebco_2019) / [GEBCO 2021](https://www.gebco.net/data_and_products/gridded_bathymetry_data/)).  

**Table:** Download links for different GEBCO versions

| version         |  description          |              comments                    |
|:---------------:|:---------------------:|:----------------------------------------:|
| [GEBCO 2019](https://www.bodc.ac.uk/data/open_download/gebco/GEBCO_15SEC/zip/) | ice surface elevation | SLW and AGE available in Figshare repository; <br /> Figures in manuscript based on GEBCO 2019 |
| [GEBCO 2020](https://www.bodc.ac.uk/data/open_download/gebco/gebco_2020/zip/) | ice surface elevation | Not used in manuscript |
| [GEBCO 2021](https://www.bodc.ac.uk/data/open_download/gebco/gebco_2021/zip/) | ice surface elevation | SLW and AGE available in Figshare repository |
| [GEBCO 2021](https://www.bodc.ac.uk/data/open_download/gebco/gebco_2021_sub_ice_topo/zip/) | sub-ice topo/bathy | Not used in manuscript |

# Spatio-temporal sea level curve (RSL)

The spatio-temporal relative sea level curve (RSL) was developed using the software [SELEN4](https://zenodo.org/record/3520451), [ICE-5G (VM2)](https://pmip2.lsce.ipsl.fr/design/ice5g/) and [DEMSRE3a](https://zenodo.org/record/1637816) derived from the public domain. The output model is available in [Figshare](https://uvaauas.figshare.com/account/articles/20029991). To obtain access please contact Johannes De Groeve (maintainer of repository).

# INSTRUCTIONS

Clone or [download](https://github.com/jedgroev/piac_global_age_slw/archive/refs/heads/main.zip) the repository. The script [create_AGE_SLW.R](https://github.com/jedgroev/piac_global_age_slw/blob/main/code/create_AGE_SLW.R) loads the required functions and generates the AGE and SLW dataset using a GEBCO model. Make sure to specify the settings including: 

**Table:** Settings 

| setting         |  description          |
|:---------------:|:---------------------:|
| **to_path** | path where to save the AGE and SLW rasters |
| **gebco_path** | path of the gebco raster |
| **curve_path** | path of the spatiotemporal sea level curve (RSL) |

