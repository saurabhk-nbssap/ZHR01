*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.09.2010 at 10:51:42 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6HRA_WAGE_GRP..................................*
DATA:  BEGIN OF STATUS_Z6HRA_WAGE_GRP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HRA_WAGE_GRP                .
CONTROLS: TCTRL_Z6HRA_WAGE_GRP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6HRA_WAGE_GRP                .
TABLES: Z6HRA_WAGE_GRP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
