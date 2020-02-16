*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.02.2013 at 10:12:27 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6HR_PPL_2_SAP..................................*
DATA:  BEGIN OF STATUS_Z6HR_PPL_2_SAP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HR_PPL_2_SAP                .
CONTROLS: TCTRL_Z6HR_PPL_2_SAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: Z6HR_PS_SAP_ACT.................................*
DATA:  BEGIN OF STATUS_Z6HR_PS_SAP_ACT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HR_PS_SAP_ACT               .
CONTROLS: TCTRL_Z6HR_PS_SAP_ACT
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: Z6HR_PS_SAP_POS.................................*
DATA:  BEGIN OF STATUS_Z6HR_PS_SAP_POS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6HR_PS_SAP_POS               .
CONTROLS: TCTRL_Z6HR_PS_SAP_POS
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *Z6HR_PPL_2_SAP                .
TABLES: *Z6HR_PS_SAP_ACT               .
TABLES: *Z6HR_PS_SAP_POS               .
TABLES: Z6HR_PPL_2_SAP                 .
TABLES: Z6HR_PS_SAP_ACT                .
TABLES: Z6HR_PS_SAP_POS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
