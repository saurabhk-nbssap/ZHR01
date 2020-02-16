report Z6HR015C_INFOTYPE_014_UPL1
       no standard page heading line-size 255.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: 014 INFOTYPE UPLOAD
* OBJECT TYPE       :  BDC               FUNC. CONSULTANT  :MANOHAR
*          DEVELOPER:SUPRIYA BHATNAGAR
*      CREATION DATE:   21.07.2010
*        DEV REQUEST:  IRDK900522
*       TCODE       :  ZPY_IT0014
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
tables :SSCRFIELDS.
TYPE-POOLS : TRUXS.

DATA: FILENAME_S TYPE RLGRAP-FILENAME,
       w_lines type i.

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

DATA : BEGIN OF ITAB_WA,
       PERNR TYPE P0014-PERNR,
       BEGDA TYPE P0014-BEGDA,
       ENDDA TYPE P0014-ENDDA,
       LGART TYPE P0014-LGART,
       BETRG TYPE P0014-BETRG,
       ZUORD TYPE P0014-ZUORD,
       END OF ITAB_WA.

DATA : BEGIN OF IDOWNLOAD_WA ,
   T1(30) TYPE C,
   T2(30) TYPE C,
   T3(30) TYPE C,
   T4(25) TYPE C,
   T5(30) TYPE C,
   T6(30) TYPE C,
   END OF IDOWNLOAD_WA.

DATA : IDOWNLOAD LIKE STANDARD TABLE OF IDOWNLOAD_WA,
       ITAB LIKE STANDARD TABLE OF  ITAB_WA.


DATA : G_ANSWER              TYPE C,
G_LINES_TAB           TYPE POPUPTEXT OCCURS 0
               WITH HEADER LINE.



DATA : WA_BEGDA(10) TYPE C,
WA_ENDDA(10)  TYPE C,
wa_amount(13) type c.

  DATA: LD_FILENAME TYPE STRING,
        LD_PATH TYPE STRING,
        LD_FULLPATH TYPE STRING,
        LD_RESULT TYPE I.

SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE TEXT-002 .
PARAMETERS: FILENAME TYPE IBIPPARMS-PATH.
SELECTION-SCREEN END OF BLOCK S2 .


AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILENAME.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'DATASET'
    IMPORTING
      FILE_NAME     = FILENAME.


start-of-selection.
*------------------------------
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
  EXPORTING
*  *I_FIELD_SEPERATOR =
    I_LINE_HEADER = 'X'
    I_TAB_RAW_DATA = IT_RAW
    I_FILENAME = FILENAME
  TABLES
     I_TAB_CONVERTED_DATA = ITAB
  EXCEPTIONS
    CONVERSION_FAILED = 1
    OTHERS = 2.

  IF SY-SUBRC = 0.
*MESSAGE 'data is saved' TYPE 'I'.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  describe table  ITAB lines w_lines.

  INCLUDE ZBDCRECX2.
*include zbdcrecx1.
  INITIALIZATION.
*----------------------------------------------------------------------*
*  MOVE 'DOCUMENT UPLOAD MODE' TO  SSCRFIELDS-FUNCTXT_01.
  MOVE '@0S@' TO HELP.
  MOVE '@49@ DOWNLOAD' TO DOWN.
  PERFORM F_FILL_INFOTEXT.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


  IF SSCRFIELDS-UCOMM = 'INFO'.
    CALL FUNCTION 'DD_POPUP_WITH_INFOTEXT'
      EXPORTING
        TITEL        = 'EXCEL FILE FORMAT '(020)
        START_COLUMN = 10
        START_ROW    = 10
        END_COLUMN   = 85
        END_ROW      = 27
        INFOFLAG     = ' '
      IMPORTING
        ANSWER       = G_ANSWER
      TABLES
        LINES        = G_LINES_TAB.
  ELSEIF SSCRFIELDS-UCOMM = 'DOWN'.

    PERFORM FILL_DOWNLOAD_DATA.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
   EXPORTING
*      WINDOW_TITLE      = ' '
     DEFAULT_EXTENSION = 'XLS'
     DEFAULT_FILE_NAME = 'FILE_INFO14'
     INITIAL_DIRECTORY = 'C:\TEMP\'
   CHANGING
     FILENAME          = LD_FILENAME
     PATH              = LD_PATH
     FULLPATH          = LD_FULLPATH
     USER_ACTION       = LD_RESULT.

* CHECK USER DID NOT CANCEL REQUEST
    CHECK LD_RESULT EQ '0'.

    CALL FUNCTION 'GUI_DOWNLOAD'
     EXPORTING
          FILENAME         = LD_FULLPATH
          FILETYPE         = 'DAT'
*       APPEND           = 'X'
*        WRITE_FIELD_SEPARATOR = 'X'
*       CONFIRM_OVERWRITE = 'X'
     TABLES
          DATA_TAB         = IDOWNLOAD[]
     EXCEPTIONS
          FILE_OPEN_ERROR  = 1
          FILE_WRITE_ERROR = 2
          OTHERS           = 3.


  ENDIF.


start-of-selection.

  perform open_group.
  clear itab_wa.
  loop at itab into itab_wa.

    CONCATENATE ITAB_WA-BEGDA+6(2) ITAB_WA-BEGDA+4(2) ITAB_WA-BEGDA+0(4)
   INTO WA_BEGDA SEPARATED BY '.'.

    CONCATENATE ITAB_WA-ENDDA+6(2) ITAB_WA-ENDDA+4(2) ITAB_WA-ENDDA+0(4)
     INTO WA_ENDDA SEPARATED BY '.'.
    wa_amount = ITAB_WA-BETRG.
    condense wa_amount.

    PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RP50G-PERNR'
*                                '22'.
                                   ITAB_WA-PERNR.
    PERFORM BDC_FIELD       USING 'RP50G-TIMR6'
                                  'X'.
    PERFORM BDC_FIELD       USING 'RP50G-BEGDA'
*                                '18.07.2010'.
                                  WA_BEGDA.
    PERFORM BDC_FIELD       USING 'RP50G-ENDDA'
*                                '31.12.9999'.
                                 WA_ENDDA.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RP50G-SUBTY'.
    PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                  '14'.
    PERFORM BDC_FIELD       USING 'RP50G-SUBTY'
*                                '1103'.
                                  ITAB_WA-LGART.
    PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RP50G-PERNR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=INS'.
    PERFORM BDC_FIELD       USING 'RP50G-PERNR'
*                                '22'.
                                   ITAB_WA-PERNR.
    PERFORM BDC_FIELD       USING 'RP50G-TIMR6'
                                  'X'.
    PERFORM BDC_FIELD       USING 'RP50G-BEGDA'
*                                '18.07.2010'.
                                   WA_BEGDA.
    PERFORM BDC_FIELD       USING 'RP50G-ENDDA'
*                                '31.12.9999'.
                                  WA_ENDDA.
    PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                  'RECURRING PAYMENTS/DEDUCTIONS'.
    PERFORM BDC_FIELD       USING 'RP50G-SUBTY'
*                                '1103'.
                                   ITAB_WA-LGART.
    PERFORM BDC_DYNPRO      USING 'MP001400' '2000'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'P0014-ZUORD'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  'UPD'.
    PERFORM BDC_FIELD       USING 'P0014-BEGDA'
*                                '18.07.2010'.
                                   WA_BEGDA.
    PERFORM BDC_FIELD       USING 'P0014-ENDDA'
*                                '31.12.2010'.
                                   WA_ENDDA.
    PERFORM BDC_FIELD       USING 'P0014-LGART'
*                                '1103'.
                                  ITAB_WA-LGART.
    PERFORM BDC_FIELD       USING 'Q0014-BETRG'
*                                '           100.00'.
*                                ITAB_WA-BETRG.
                                   wa_amount.
*  PERFORM BDC_FIELD       USING 'P0014-WAERS'
*                                'INR'.
    PERFORM BDC_FIELD       USING 'P0014-ZUORD'
*                                'XYZ'.
                                   ITAB_WA-ZUORD.

    perform bdc_transaction using 'PA30'.
  endloop.
  perform close_group.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_INFOTEXT .
  MOVE: 'X' TO G_LINES_TAB-HELL,
        'X' TO G_LINES_TAB-TOPOFPAGE,
        'FORMAT FOR UPLOAD DATA'(005)  TO G_LINES_TAB-TEXT.
  APPEND G_LINES_TAB.

  MOVE:  'X' TO G_LINES_TAB-HELL,
          ' ' TO G_LINES_TAB-TOPOFPAGE,
          'FIELD        TYPE      WIDTH  DEC  REMARKS'(006)
          TO G_LINES_TAB-TEXT.
  APPEND G_LINES_TAB.
  MOVE ' ' TO G_LINES_TAB-TEXT.
  APPEND G_LINES_TAB.
  PERFORM APPEND_FIELDS USING :
  'PERNR'	'NUMC' '8' '' 'PERSONNEL NUMBER',
  'BEGDA' 'DATS' '8' '' 'START DATE',
  'ENDDA' 'DATS' '8' '' 'END DATE',
  'LGART'	'CHAR' '4' ''	'WAGE TYPE',
  'BETRG'	'CURR' '13' '2'	'WAGE TYPE AMOUNT FOR PAYMENTS',
  'ZUORD'	'CHAR' '20'	'' 'ASSIGNMENT NUMBER'.

ENDFORM.                    " F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*&      FORM  APPEND_FIELDS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_0443   TEXT
*      -->P_0444   TEXT
*      -->P_0445   TEXT
*      -->P_0446   TEXT
*      -->P_0447   TEXT
*----------------------------------------------------------------------*
FORM APPEND_FIELDS  USING  FIELD TYP WIDTH DECM REM.

  DATA : TEXT(140).
  TEXT = FIELD.
  TEXT+13(10)  = TYP.
  TEXT+24(6)  =  WIDTH.
  TEXT+30(5)  =  DECM.
  TEXT+35(80)  = REM.

  MOVE:  ' ' TO G_LINES_TAB-HELL,
         ' ' TO G_LINES_TAB-TOPOFPAGE,
         TEXT  TO G_LINES_TAB-TEXT.
  APPEND G_LINES_TAB.


ENDFORM.                    "APPEND_FIELDS
*&---------------------------------------------------------------------*
*&      Form  FILL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_DOWNLOAD_DATA .
  CLEAR :IDOWNLOAD_WA,IDOWNLOAD.

*  IDOWNLOAD_WA-T1  = 'PERNR'.
*  IDOWNLOAD_WA-T2  = 'ENDDA'.
*  IDOWNLOAD_WA-T3  = 'BEGDA'.
*  IDOWNLOAD_WA-T4  = 'LGART'.
*  IDOWNLOAD_WA-T5  = 'BETRG'.
*  IDOWNLOAD_WA-T6  = 'ZUORD'.
*
*  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
*  CLEAR IDOWNLOAD_WA.

  IDOWNLOAD_WA-T1  = 'PERSONNEL NUMBER'.
  IDOWNLOAD_WA-T2  = 'START DATE'.
  IDOWNLOAD_WA-T3  = 'END DATE'.
  IDOWNLOAD_WA-T4  = 'WAGE TYPE'.
  IDOWNLOAD_WA-T5  = 'WAGE TYPE AMOUNT FOR PAYMENTS'.
  IDOWNLOAD_WA-T6  = 'ASSIGNMENT NUMBER'.

  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
  CLEAR IDOWNLOAD_WA.
ENDFORM.                    " FILL_DOWNLOAD_DATA
