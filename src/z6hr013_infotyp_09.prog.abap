*&---------------------------------------------------------------------*
*& Report  Z6HR013_INFOTYP_09
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR013_INFOTYP_09.
* ----------------------------------------------------------------------*
*     PRE DEFINED DATA TYPES
*----------------------------------------------------------------------*

TYPE-POOLS : TRUXS.

*----------------------------------------------------------------------*
*     TABLES
**----------------------------------------------------------------------*

TABLES : RP50G     ,  " INPUT FIELDS FOR HR MASTER DATA TRANSACTIONS
         P0009     ,  " HR MASTER RECORD: INFOTYPE 0009(BANK DETAILS)
         SSCRFIELDS.  " FIELDS ON SELECTION SCREEN


*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
**----------------------------------------------------------------------*

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
           BEGDA(10),           "FROM DATE
           BANKN(18),           "BANK ACCOUNT NUMBER
           ZLSCH   ,            "PAYMENT METHOD
           ZZPAYEE_NAME(40),    "PAYEE NAME
           ZZBACC_NO     LIKE  P0009-ZZBACC_NO,"BANK ACCOUNT NUMBER
           ZZBANK_NAME(40),        "  BANK NAME
           ZZBANK_BRANCH(40),      "BANK BRANCH
           ZZRTGS_NEFT_IFSC(15),
           ZZTRAN_TYPE ,          " TRANSACTION TYPE
           END OF TY_TABLES.

    "INTERNAL TABLE CREATION

 DATA: WA_TABLES TYPE TY_TABLES,

       ITAB1 TYPE TABLE OF TY_TABLES.

 "DECLARATION OF BDCDATA STRUCTURE

 DATA : WA_BDCDATA TYPE BDCDATA,
       IT_BDCDATA TYPE TABLE OF BDCDATA.

 DATA : BEGIN OF err_data OCCURS 0 ,
           PERNR  LIKE RP50G-PERNR ,
           BANKN(10),
           ZLSCH,
           ZZPAYEE_NAME(40),
           ZZBACC_NO LIKE P0009-ZZBACC_NO,
           ZZBANK_NAME(40),
           err(250) ,
         END   OF err_data .


*
*  DATA : WA_BEGDA(10) TYPE C,
*         WA_ENDDA(10)  TYPE C.

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
  perform disp_err.
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
          'FIELD        TYPE      WIDTH  DEC  REMARKS'(006) TO G_LINES_TAB-TEXT.

  APPEND G_LINES_TAB.
  MOVE ' ' TO G_LINES_TAB-TEXT.
  APPEND G_LINES_TAB.
  PERFORM APPEND_FIELDS USING :

 'PERNR'      'NUMC' '8' '' 'PERSONNNEL NUMBER',
 'BEGDA'       'CHAR' '10' '' 'FROM',
 'BANKN'       'CHAR'  '18'    ''     'BANK ACCOUNT NUMBER',
 'ZLSCH'       'CHAR'   '1'    ''    'PAMENT METHOD',
 'ZZPAYEE_NAME' 'CHAR' '40'  ''   'PAYEE NAME',
 'ZZBACC_NO '   'CHAR' '40'  ''   'BANK ACCOUNT NUMBER',
 'ZZBANK_NAME'  'CHAR' '40'   ''      'BANK NAME',
 'ZZBANK_BRANCH'  'CHAR' '40' ''    'BANK BRANCH',
 'ZZRTGS_NEFT_IFSC' 'CHAR' '15' ''   '',
 'ZZTRAN_TYPE'  'CHAR'  '1' '' 'TRANSACTION TYPE'.


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
  IDOWNLOAD_WA-T2  = 'BEGDA'.
  IDOWNLOAD_WA-T3  = 'BANKN'.
  IDOWNLOAD_WA-T4  = 'ZLSCH'.
  IDOWNLOAD_WA-T5  = 'ZZPAYEE_NAME'.
  IDOWNLOAD_WA-T6  = 'ZZBACC_NO '.
  IDOWNLOAD_WA-T7  = 'ZZBANK_NAME'.
  IDOWNLOAD_WA-T8  = 'ZZBANK_BRANCH'.
  IDOWNLOAD_WA-T9  = 'ZZRTGS_NEFT_IFSC'.
  IDOWNLOAD_WA-T10  = 'ZZTRAN_TYPE'.

  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
  CLEAR IDOWNLOAD_WA.

  IDOWNLOAD_WA-T1  = 'PERSONNEL NUMBER'.
  IDOWNLOAD_WA-T2  = 'FROM DATE'.
  IDOWNLOAD_WA-T3  = 'BANK ACCOUNT NUMBER'.
  IDOWNLOAD_WA-T4  = 'PAYMENT METHOD'.
  IDOWNLOAD_WA-T5  = 'PAYEE NAME'.
  IDOWNLOAD_WA-T6  = 'BANK ACCOUNT NUMBER'.
  IDOWNLOAD_WA-T7  = 'BANK NAME'.
  IDOWNLOAD_WA-T8  = 'BANK BRANCH'.
  IDOWNLOAD_WA-T9  = 'RTGS/NFET/IFSC'.
  IDOWNLOAD_WA-T10  = 'TRANSACTION TYPE'.


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

   PERFORM FIELD_DETAILS USING 'BDC_OKCODE' '=INS'.

   PERFORM FIELD_DETAILS USING 'RP50G-PERNR' WA_TABLES-PERNR.

   PERFORM FIELD_DETAILS USING 'RP50G-BEGDA' WA_TABLES-BEGDA.

   PERFORM FIELD_DETAILS  USING 'BDC_CURSOR' 'RP50G-SUBTY'.

   PERFORM FIELD_DETAILS USING 'RP50G-CHOIC' '9'.

   PERFORM FIELD_DETAILS USING 'RP50G-SUBTY' '0'.



   PERFORM SCREEN_DETAILS USING 'MP000900' '2000' 'X'.



   PERFORM FIELD_DETAILS USING 'BDC_OKCODE' 'UPD'.

   PERFORM FIELD_DETAILS USING 'P0009-BEGDA'  WA_TABLES-BEGDA.

   PERFORM FIELD_DETAILS USING 'P0009-ENDDA' '31.12.9999'.

   PERFORM FIELD_DETAILS USING 'P0009-BANKL' '0001'.

   PERFORM FIELD_DETAILS USING 'P0009-BANKN' WA_TABLES-BANKN.

   PERFORM FIELD_DETAILS USING 'P0009-ZLSCH' WA_TABLES-ZLSCH.

   PERFORM FIELD_DETAILS USING 'BDC_CURSOR' 'P0009-ZZTRAN_TYPE'.

   PERFORM FIELD_DETAILS   USING     'P0009-ZZPAYEE_NAME' WA_TABLES-ZZPAYEE_NAME .

   PERFORM FIELD_DETAILS   USING      'P0009-ZZBACC_NO'   WA_TABLES-ZZBACC_NO .

   PERFORM FIELD_DETAILS   USING      'P0009-ZZBANK_NAME'  WA_TABLES-ZZBANK_NAME.

   PERFORM FIELD_DETAILS   USING     'P0009-ZZBANK_BRANCH'  WA_TABLES-ZZBANK_BRANCH.

   PERFORM FIELD_DETAILS   USING     'P0009-ZZRTGS_NEFT_IFSC' WA_TABLES-ZZRTGS_NEFT_IFSC.

   PERFORM FIELD_DETAILS   USING 'P0009-ZZTRAN_TYPE'      WA_TABLES-ZZTRAN_TYPE.


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
      WRITE : / sy-uline(126) .

      WRITE : / '|' NO-GAP , (8)  'Personnel no.'    NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (10)  'bank account no'  NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (10) 'payment method' NO-GAP COLOR
COL_HEADING ,
                '|' NO-GAP , (20)  'payee name.'  NO-GAP COLOR
COL_HEADING ,

                '|' NO-GAP , (20)  'bank account no.'  NO-GAP COLOR
 COL_HEADING ,
                 '|' NO-GAP , (10)  'bank name'  NO-GAP COLOR
 COL_HEADING ,

                 '|' NO-GAP , (40) 'ERROR'   NO-GAP COLOR COL_NEGATIVE ,

                  '|' NO-GAP .

      WRITE : / sy-uline(126) .
    ENDAT .

    WRITE : / '|' NO-GAP , (8) err_data-PERNR NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (10) err_data-BANKN  NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (10) err_data-ZLSCH NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (20) err_data-ZZPAYEE_NAME NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (20) err_data-ZZBACC_NO NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (10) err_data-ZZBANK_NAME NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (40) err_data-err   NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP .

    AT LAST .
      WRITE : / sy-uline(126) .
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
