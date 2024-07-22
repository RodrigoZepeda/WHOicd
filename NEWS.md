# WHOicd 0.5.1

* Stopped exporting functions to `code_place` and `code_maternal`.
* Reduced the size of the examples' output

# WHOicd 0.5.0

* Changed `icd_search()` method to POST. 
* Removed the code validation step from ICD-10 as it might introduce bugs down the line. 
* Completed the first batch of `icd_11` functions

# WHOicd 0.4.1

* Fixed minor errors on the website.

# WHOicd 0.4.0

* `doris` now allows for multiple languages beyond English
* Changed `README` and article disposition. 
* Added `ICD-11` functionality: `icd11_autocode`, `icd11_search`. 

# WHOicd 0.3.0

* Added access to the DORIS system with the  `doris` function. 
* Now requires `httr2 >= 1.0.0`. 

# WHOicd 0.2.0

* Reduced the amount of functions for ICD-10 and improve speed for vectorized search. 
