*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.04.2011 at 10:38:53 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZF16_WAGETYPES..................................*
DATA:  BEGIN OF STATUS_ZF16_WAGETYPES                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZF16_WAGETYPES                .
CONTROLS: TCTRL_ZF16_WAGETYPES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZF16_WAGETYPES                .
TABLES: ZF16_WAGETYPES                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
