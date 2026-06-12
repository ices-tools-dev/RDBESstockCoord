# WGRDBES-StockCoord
WGRDBES-StockCoord develops functions and exchange formats for stock coordinator (SC) work within the Regional DataBase and Estimation System (RDBES).

<img width="1920" height="1080" alt="RDBES infographic" src="https://github.com/user-attachments/assets/58187417-48fb-4bc8-bd8f-e3eedbe5e228" />

## SC functionality
All functions can be found in the [R](https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/R) folder and guidance in the [vignettes](https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/vignettes) folder. 
### Discard raising and allocation of biological parameters
For now, there are two existing approaches: Jean-Baptiste Lecomte’s and Yves Reecht’s.

Jean-Baptiste Lecomte’s approach is described in the [rasing_discards_sole8ab_vignette](https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/rasing_discards_sole8ab_vignette.html)

Yves Reecht’s approach is described in [Example_saithe_raising_and_allocation](https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/Example_saithe_raising_and_allocation.R)

### InterCatch to CEF conversion
It is possible to convert both InterCatch upload files and InterCatch download files (MeanWeightAtAgeLength, NumbersAtAgeLength, StockOverview) to CEF, see [vignette_conversion_functions](https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/vignette_conversion_functions.pdf)

### Overview of data
Examples of overviews of data can be found in [1.diagnosis_discards](https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/R/1.diagnosis_discards.R) and [rasing_discards_sole8ab_vignette](https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/rasing_discards_sole8ab_vignette.html)

### Issues
Issues regarding the functions should be raised as an [Issue](https://github.com/ices-tools-dev/RDBESstockCoord/issues)

## Exchange formats 
### Catch Estimates Format (CEF)
The format for exchanging national (or regional) estimates between national (or regional) estimator and stock coordinator

The CEF, version 17, is in production

**Format specification**: https://github.com/ices-tools-dev/RDBES/tree/master/CEF%20and%20SCF%20formats

**Format examples**: https://github.com/ices-tools-dev/RDBES/tree/master/CEF%20and%20SCF%20formats

**Format description / presentations**: A presentation from April 2026 can be found at https://github.com/ices-tools-dev/RDBES/tree/master/CEF%20and%20SCF%20formats/presentations/260414_CEF_presentation and a recording of it can be found at [WGRDBES-StockCoord - Recordings and notes - All Documents](https://icesit.sharepoint.com/sites/ExpertGroup/WGRDBES-StockCoord/Shared%20Documents/Forms/AllItems.aspx)

**Format issues**: Issues regarding format constraints should be raised in [Issues](https://github.com/ices-tools-dev/RDBESstockCoord/issues)

**Upload and download**: https://github.com/ices-tools-dev/RDBES/tree/master/API%20information

### Stock Coordination Format (SCF)
The format for exchanging stock estimates between the stock coordinator and stock assessor

The format is under development

**Format specification**: https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/WGRDBESstockCoord/format/SCF

**Format examples**: https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/WGRDBESstockCoord/format/SCF
