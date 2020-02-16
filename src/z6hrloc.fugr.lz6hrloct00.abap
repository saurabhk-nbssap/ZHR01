*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 19.08.2010 at 15:15:26 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6HRA_LOCATION..................................*
DATA:  BEGIN OF STATUS_Z6HRA_LOCATION                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HRA_LOCATION                .
CONTROLS: TCTRL_Z6HRA_LOCATION
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6HRA_LOCATION                .
TABLES: Z6HRA_LOCATION                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
