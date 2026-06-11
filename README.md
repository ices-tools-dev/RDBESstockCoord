# RDBESstockCord
RDBESstockCord develops functions and exchange formats for stock coordinator (SC) work within the Regional DataBase and Estimation System (RDBES).

<img width="1920" height="1080" alt="RDBES infographic" src="https://github.com/user-attachments/assets/58187417-48fb-4bc8-bd8f-e3eedbe5e228" />

## SC functionality
See [vignettes](https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/vignettes) and _(link to presentations)_
### Discard rasing and allocation of biological paramters
For now, two approaches exist, Jean Baptiste Lecomte’s and Yves Reecht’s.

Jean Baptiste Lecomte’s approache is described in https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/rasing_discards_sole8ab_vignette.html

Yves Reecht’s approache is described in https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/Example_saithe_raising_and_allocation.R and _Link to presentation_

### InterCatch to CEF conversion
It is possible to convert both InterCatch upload file and InterCatch download format (MeanWeightAtAgeLength, NumbersAtAgeLength, StockOverview) to CEF, see https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/vignette_conversion_functions.pdf

### Overview of data
Examples of overviews of data can be found in https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/R/1.diagnosis_discards.R and https://github.com/ices-tools-dev/RDBESstockCoord/blob/main/vignettes/rasing_discards_sole8ab_vignette.html

### Issues
Issues regarding the functions should be raised at https://github.com/ices-tools-dev/RDBESstockCoord/issues

## Exchange formats 
### Catch Estimates Format (CEF)
The format for exchanging national (or regional) estimates between national (or regional) estimator and stock coordinator

The CEF, version 17, is in production

**Format specification**: https://github.com/ices-tools-dev/RDBES/tree/master/CEF%20and%20SCF%20formats

**Format examples**: https://github.com/ices-tools-dev/RDBES/tree/master/CEF%20and%20SCF%20formats

**Format description / presentations**: A presentation from April 2026 can be found at https://github.com/ices-tools-dev/RDBES/tree/master/CEF%20and%20SCF%20formats/presentations/260414_CEF_presentation and a recording of it can be found at [WGRDBES-StockCoord - Recordings and notes - All Documents](https://icesit.sharepoint.com/sites/ExpertGroup/WGRDBES-StockCoord/Shared%20Documents/Forms/AllItems.aspx)

**Format issues**: Issues regarding format constraints should be raised at https://github.com/ices-tools-dev/RDBESstockCoord/issues

**Upload and download**: https://github.com/ices-tools-dev/RDBES/tree/master/API%20information

### Stock coordination format (SCF)
The format for exchanging stock estimates between to stock coordinator and stock assessor

The format is under development

**Format specification**: https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/WGRDBESstockCoord/format/SCF

**Format examples**: https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/WGRDBESstockCoord/format/SCF
