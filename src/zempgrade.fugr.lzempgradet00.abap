*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.10.2010 at 15:11:15 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6HR_EMP_GRADE..................................*
DATA:  BEGIN OF STATUS_Z6HR_EMP_GRADE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HR_EMP_GRADE                .
CONTROLS: TCTRL_Z6HR_EMP_GRADE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6HR_EMP_GRADE                .
TABLES: Z6HR_EMP_GRADE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
