*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.01.2013 at 12:47:10 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6HR039_ARR_REG.................................*
DATA:  BEGIN OF STATUS_Z6HR039_ARR_REG               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HR039_ARR_REG               .
CONTROLS: TCTRL_Z6HR039_ARR_REG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6HR039_ARR_REG               .
TABLES: Z6HR039_ARR_REG                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
