# hys_data_prep
Scripts/Package for prepping a HYS dataset

General Principles
1) Use package based design
2) Implement unit testing of functions
3) Use `drake` to control workflow
4) Include sainity and QA checks

General Steps:
1) Acquire enrollment counts by School X Grade X Year X Sex X Race from OSPI
2) Acquire newest iteration of HYS
3) Format enrollment counts into a single long dataset
4) Standardize/clean HYS dataset
5) Calculate psuedo-pweights for the microdata
6) Generate standard QA statistics

Principles for contributing
1) Functions must follow: What comes out is only a function of what comes in (e.g. no scoping/environment jumping) UNLESS specified-- and then the default setting is F
2) Each function/script must be properly documented, contain few hardcoded paths (with big highlights on where they are), and feature unit testing as appropriate
