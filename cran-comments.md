## Resubmission
This is resubmission where notes have been addressed and updated.

## Test environments
* local OS X install, R 4.2.3
* This is a new release.

## R CMD check results
There were no notes, errors or warnings.

Window tests and Mac release tests were successful.

The R-hub runs failed because it couldn't find JAGS but I think this is a missing dependency on R-hub rather than a problem with the package (or a missing option that I need to add in somehow).

I get a warning about legacy package depending on fields and geosphere which further depend on sp and depend on maptools, rgdal and regeos. This seems to be related to other packages and not something I can fix. 
  
I am getting a note about 'lastMiKTeXException' which seems to be a bug/crash in MiKTeX and I cannot solve it.

I am getting a note regarding ''NULL'' which is a R-hub issue which is out of my control.
    

