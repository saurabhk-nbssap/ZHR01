*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.11.2010 at 12:57:42 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVI_T7IND3......................................*
TABLES: ZVI_T7IND3, *ZVI_T7IND3. "view work areas
CONTROLS: TCTRL_ZVI_T7IND3
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVI_T7IND3. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVI_T7IND3.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVI_T7IND3_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVI_T7IND3.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_T7IND3_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVI_T7IND3_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVI_T7IND3.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_T7IND3_TOTAL.

*.........table declarations:.................................*
TABLES: T7IND1                         .
TABLES: T7IND3                         .
TABLES: T7IND9                         .
