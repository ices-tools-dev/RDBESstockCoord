# 2023_pok.27.3a46_RDBES_combined

Saithe (Pollachius virens) in Subareas 4, 6 and Division 3.a (North Sea, Rockall and West of Scotland, Skagerrak and Kattegat) --
Intercatch raising procedure reproduction based on 2022 data.

Test repository for WKRDBES-Raise&TAF2 -- Stock coordinator role.

## How to run

Install the icesTAF package, version >=2.2 from CRAN.

Then open R in the `2023_pok.27.3a46_RDBES_combined` directory and run:

```{r}
library(icesTAF)
## icesTAF::clean()  # Remove working directories (force re-install everything).
taf.bootstrap(clean = TRUE)
sourceAll()
```
