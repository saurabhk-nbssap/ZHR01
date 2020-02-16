*&---------------------------------------------------------------------*
*& Report  Z6HR013_INFOTYPE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR025_INFT0013_UPL.

* ----------------------------------------------------------------------*
*     PRE DEFINED DATA TYPES
*----------------------------------------------------------------------*

TYPE-POOLS : TRUXS.

*----------------------------------------------------------------------*
*     TABLES
**----------------------------------------------------------------------*

TABLES : RP50G     ,  " INPUT FIELDS FOR HR MASTER DATA TRANSACTIONS
         Q0015     ,  " SCREEN FIELDS INFO TYPE 0015(ADDITIONALPAYMENTS)
         P0015     ," HR MASTER RECORD INFO TYPE 0015
         SSCRFIELDS.  " FIELDS ON SELECTION SCREEN

 "DECLARATION OF internal table for Error records

*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA:   Pmesstab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF i_error_messtab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: text(300).
DATA: END OF i_error_messtab.
*       error session opened (' ' or 'X')
*       message texts
TABLES: t100.

*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001 .
PARAMETERS: P_FORE   RADIOBUTTON GROUP RAD1 DEFAULT 'X' ,
            P_BACK   RADIOBUTTON GROUP RAD1 .
SELECTION-SCREEN END   OF BLOCK S1 .
SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE TEXT-002 .
PARAMETERS: P_FILE  TYPE IBIPPARMS-PATH  .

SELECTION-SCREEN END OF BLOCK S2 .

SELECTION-SCREEN : BEGIN OF BLOCK S3  WITH FRAME TITLE TEXT-S03.
SELECTION-SCREEN PUSHBUTTON  /1(7) HELP USER-COMMAND INFO.
SELECTION-SCREEN PUSHBUTTON  15(17) DOWN USER-COMMAND DOWN.
SELECTION-SCREEN : END OF BLOCK S3.

INCLUDE Z6XX003I_BDC_NOTE.

INITIALIZATION.

  MOVE '@0S@' TO HELP.
  MOVE '@49@ DOWNLOAD' TO DOWN.
  PERFORM F_FILL_INFOTEXT.
* ----------------------------------------------------------------------*
*     GLOBLE DATA
*----------------------------------------------------------------------*
  DATA  V_MODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
  DATA : V_FILENAME TYPE IBIPPARMS-PATH.
  DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

  DATA : G_ANSWER              TYPE C,
       G_LINES_TAB           TYPE POPUPTEXT OCCURS 0
                             WITH HEADER LINE.

  DATA : BEGIN OF IDOWNLOAD_WA ,
     T1(30) TYPE C,
     T2(30) TYPE C,
     T3(30) TYPE C,
     T4(25) TYPE C,
     T5(30) TYPE C,
     T6(30) TYPE C,
     T7(30) TYPE C,
     T8(30) TYPE C,
     T9(30) TYPE C,
     T10(30) TYPE C,

     END OF IDOWNLOAD_WA.

   DATA : IDOWNLOAD LIKE STANDARD TABLE OF IDOWNLOAD_WA.


  TYPES : BEGIN OF TY_TABLES,
        PERNR LIKE RP50G-PERNR, "PERSONAL NUMBER
        TIMR6 , "TIME PERIOD INDICATOR-PERIOD
        BEGDA(10), "FROM DATE
        ENDDA(10), "TO DATE
        CHOIC(35), "INFOTYPE SELECTION FOR HR MASTER
        SUBTY(4), "SUBTYPE
        BETRG(13), "WAGE TYPE AMOUNT FOR PAYMENTS
        WAERS LIKE P0015-WAERS, "CURRENCY KEY
        BEGDA1(10),"START DATE
        ZUORD(20), "ASSIGNMENT NUMBER
        END OF TY_TABLES.
 "INTERNAL TABLE CREATION

 DATA: WA_TABLES TYPE TY_TABLES,

       ITAB1 TYPE TABLE OF TY_TABLES.

 "DECLARATION OF BDCDATA STRUCTURE
 DATA : WA_BDCDATA TYPE BDCDATA,
       IT_BDCDATA TYPE TABLE OF BDCDATA.

DATA : BEGIN OF err_data OCCURS 0 ,
           PERNR  LIKE RP50G-PERNR ,
           SUBTY  LIKE RP50G-SUBTY ,
           BETRG(15) ,
           ZUORD  LIKE P0015-ZUORD ,
           err(250) ,
         END   OF err_data .



  DATA : WA_BEGDA(10) TYPE C,
         WA_ENDDA(10)  TYPE C.

  DATA: LD_FILENAME TYPE STRING,
        LD_PATH TYPE STRING,
        LD_FULLPATH TYPE STRING,
        LD_RESULT TYPE I.

*  ----
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
     DEFAULT_FILE_NAME = 'FILE_INFO0015'
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE '
    IMPORTING
      FILE_NAME     = P_FILE.

  IF NOT P_FILE IS INITIAL.
    V_FILENAME = P_FILE.
  ENDIF.

*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*

*START-OF-SELECTION.
*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION .
  IF P_FORE EQ 'X' .
    V_MODE = 'A' .
  ELSE .
    V_MODE = 'N' .
  ENDIF .
  PERFORM  FILE_UPLOAD  .
  PERFORM RUN_BDC.
  perform disp_ERR.
*
*&---------------------------------------------------------------------*
*&      FORM  F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
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

 'PERNR' 'NUMC' '8' '' 'PERSONNNEL NUMBER',
'TIMR6' 'CHAR' '1' '' 'TIME PERIOD INDICATOR:PERIOD',
'BEGDA' 'CHAR' '10' '' 'FROM',
'ENDDA' 'CHAR' '10' '' 'VALID TO DATE',
'CHOIC' 'CHAR' '35' '' 'INFOTYPE SELECTION FOR HR MASTER',
'SUBTY' 'CHAR' '4' '' 'SUB TYPE',
'BETRG' 'CURR' '13' '' 'WAGE TYPE AMOUNT FOR PAYMENTS',
'WAERS' 'CUKY' '5' '' 'CURRENCY KEY',
'ZUORD' 'CHAR' '20' ' ' 'ASSIGNMENT NUMBER'.

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


ENDFORM.                    " APPEND_FIELDS
*&---------------------------------------------------------------------*
*&      FORM  FILL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FILL_DOWNLOAD_DATA .
  CLEAR :IDOWNLOAD_WA,IDOWNLOAD.

  IDOWNLOAD_WA-T1  = 'PERNR'.
  IDOWNLOAD_WA-T2  = 'TIMR6'.
  IDOWNLOAD_WA-T3  = 'BEGDA'.
  IDOWNLOAD_WA-T4  = 'ENDDA'.
  IDOWNLOAD_WA-T5  = 'CHOIC'.
  IDOWNLOAD_WA-T6  = 'SUBTY'.
  IDOWNLOAD_WA-T7  = 'BETRG'.
  IDOWNLOAD_WA-T8  = 'WAERS'.
  IDOWNLOAD_WA-T9  = 'BEGDA1'.
  IDOWNLOAD_WA-T10  = 'ZUORD'.

  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
  CLEAR IDOWNLOAD_WA.

  IDOWNLOAD_WA-T1  = 'PERSONNEL NUMBER'.
  IDOWNLOAD_WA-T2  = 'TIME PERIOD'.
  IDOWNLOAD_WA-T3  = 'FROM DATE'.
  IDOWNLOAD_WA-T4  = 'TO DATE'.
  IDOWNLOAD_WA-T5  = 'INFO TYPE SELECTION'.
  IDOWNLOAD_WA-T6  = 'SUB TYPE'.
  IDOWNLOAD_WA-T7  = 'AMOUNT'.
  IDOWNLOAD_WA-T8  = 'CURRENCY KEY'.
  IDOWNLOAD_WA-T9  = 'START DATE'.
  IDOWNLOAD_WA-T10  = 'ASSIGNMENT NUMBER'.


  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
  CLEAR IDOWNLOAD_WA.
ENDFORM.                    " FILL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      FORM  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FILE_UPLOAD .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
   EXPORTING
*  *I_FIELD_SEPERATOR =
     I_LINE_HEADER = 'X'
     I_TAB_RAW_DATA = IT_RAW
     I_FILENAME = V_FILENAME
   TABLES
      I_TAB_CONVERTED_DATA = ITAB1
   EXCEPTIONS
     CONVERSION_FAILED = 1
     OTHERS = 2.

  IF SY-SUBRC = 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      FORM  RUN_BDC
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM RUN_BDC .

LOOP AT ITAB1 INTO WA_TABLES.

   REFRESH IT_BDCDATA.

    PERFORM SCREEN_DETAILS USING 'SAPMP50A' '1000' 'X'.

    PERFORM FIELD_DETAILS USING 'BDC_CURSOR' WA_TABLES-PERNR.
    PERFORM FIELD_DETAILS USING 'BDC_OKCODE' '/00'.
    PERFORM FIELD_DETAILS USING 'RP50G-PERNR' WA_TABLES-PERNR.
    PERFORM FIELD_DETAILS USING 'RP50G-TIMR6' WA_TABLES-TIMR6.
    PERFORM FIELD_DETAILS USING 'RP50G-BEGDA' WA_TABLES-BEGDA.
    PERFORM FIELD_DETAILS USING 'RP50G-ENDDA' WA_TABLES-ENDDA.
    PERFORM FIELD_DETAILS USING 'BDC_CURSOR' WA_TABLES-SUBTY.
    PERFORM FIELD_DETAILS USING 'RP50G-CHOIC' WA_TABLES-CHOIC.
    PERFORM FIELD_DETAILS USING 'RP50G-SUBTY' WA_TABLES-SUBTY.



    PERFORM SCREEN_DETAILS USING 'SAPMP50A' '1000' 'X'.

    PERFORM FIELD_DETAILS USING 'BDC_CURSOR' WA_TABLES-PERNR.
    PERFORM FIELD_DETAILS USING 'BDC_OKCODE' '=INS'.
    PERFORM FIELD_DETAILS USING 'RP50G-PERNR' WA_TABLES-PERNR.
    PERFORM FIELD_DETAILS USING 'RP50G-TIMR6' WA_TABLES-TIMR6.
    PERFORM FIELD_DETAILS USING 'RP50G-BEGDA' WA_TABLES-BEGDA.
    PERFORM FIELD_DETAILS USING 'RP50G-ENDDA' WA_TABLES-ENDDA.
    PERFORM FIELD_DETAILS USING 'RP50G-CHOIC' WA_TABLES-CHOIC.
    PERFORM FIELD_DETAILS USING 'RP50G-SUBTY' WA_TABLES-SUBTY.


    PERFORM SCREEN_DETAILS USING 'MP001500' '2040' 'X'.

    PERFORM FIELD_DETAILS USING 'BDC_CURSOR' WA_TABLES-ZUORD.
    PERFORM FIELD_DETAILS USING 'BDC_OKCODE' '=UPD'.
    PERFORM FIELD_DETAILS USING 'Q0015-BETRG' WA_TABLES-BETRG.
    PERFORM FIELD_DETAILS USING 'P0015-WAERS' WA_TABLES-WAERS.
    PERFORM FIELD_DETAILS USING 'P0015-BEGDA' WA_TABLES-BEGDA1.
    PERFORM FIELD_DETAILS USING 'P0015-ZUORD' WA_TABLES-ZUORD.

    REFRESH : MESSTAB.
    CLEAR   : MESSTAB.

    CALL TRANSACTION 'PA30' USING IT_BDCDATA MODE V_MODE MESSAGES INTO MESSTAB.

    LOOP AT MESSTAB where msgtyp eq 'S' OR msgtyp eq 'E'.
      SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
                                AND   ARBGB = MESSTAB-MSGID
                                AND   MSGNR = MESSTAB-MSGNR.
      IF SY-SUBRC = 0.
        L_MSTRING = T100-TEXT.
        IF L_MSTRING CS '&1'.
          REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
          REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
          REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
          REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
        ELSE.
          REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
        ENDIF.
        CONDENSE L_MSTRING.

      ENDIF.
      ERR_DATA-ERR = L_MSTRING.
      MOVE-CORRESPONDING WA_TABLES TO ERR_DATA.
      APPEND ERR_DATA.
      CLEAR  ERR_DATA.
    ENDLOOP.

  ENDLOOP.
ENDFORM.                    " RUN_BDC

                   " DISPLAY_MESSAGE

*
FORM disp_err.
  LOOP AT err_data .
    AT FIRST .
      WRITE : / sy-uline(120) .
      WRITE : / '|' NO-GAP , (8)  'Personnel no.'    NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (10)  'sub type.'  NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (15) 'wagetype amount.' NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (20)  'Assign number.'  NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (60) 'ERROR'   NO-GAP COLOR COL_NEGATIVE ,
                '|' NO-GAP .

      WRITE : / sy-uline(120) .
    ENDAT .


    WRITE : / '|' NO-GAP , (8) err_data-PERNR NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (10) err_data-subty  NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (15) err_data-betrg NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (20) err_data-zuord NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (60) err_data-err(60)   NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP .

    AT LAST .
      WRITE : / sy-uline(120) .
    ENDAT .
  ENDLOOP .
ENDFORM.

FORM SCREEN_DETAILS USING PROGRAM SCREEN BEGIN.
    CLEAR WA_BDCDATA.

    WA_BDCDATA-PROGRAM = PROGRAM.
    WA_BDCDATA-DYNPRO = SCREEN.
    WA_BDCDATA-DYNBEGIN = BEGIN.

    APPEND WA_BDCDATA TO IT_BDCDATA.
    ENDFORM.

   FORM FIELD_DETAILS USING FIELD VALUE.
   CLEAR WA_BDCDATA.

   WA_BDCDATA-FNAM = FIELD.
   WA_BDCDATA-FVAL = VALUE.

   APPEND WA_BDCDATA TO IT_BDCDATA.
   ENDFORM.
