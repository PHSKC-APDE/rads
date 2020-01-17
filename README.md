# apdeRecodes
Scripts/Package for prepping a HYS dataset

General Principles
1) Use package based design
2) Implement unit testing of functions
3) Include sanity and QA checks

Principles for contributing
1) Functions must follow: What comes out is only a function of what comes in (e.g. no scoping/environment jumping) UNLESS specified-- and then the default setting is F
2) Each function/script must be properly documented, contain few hardcoded paths (with big highlights on where they are), and feature unit testing as appropriate
