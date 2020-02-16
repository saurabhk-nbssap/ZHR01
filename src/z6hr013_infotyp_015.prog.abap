*&---------------------------------------------------------------------*
*& Report  Z6HR013_INFOTYP_015
*& OBJECT DESCRIPTION: 015 INFOTYPE UPLOAD
*&   OBJECT TYPE       :  BDC               FUNC. CONSULTANT :MANOHAR
*&        TEAM LEAD : RAMAKRISHNA
*&        Developer : venugopal
*&     CREATION DATE:   04.08.2010
*&     Request No : IRDK901510
*&      TCODE       :  ZPY_IT0015
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR013_INFOTYP_015 NO STANDARD PAGE HEADING.

* ----------------------------------------------------------------------*
*     PRE DEFINED DATA TYPES
*----------------------------------------------------------------------*

TYPE-POOLS : TRUXS.

*----------------------------------------------------------------------*
*     TABLES
*----------------------------------------------------------------------*
TABLES:   SSCRFIELDS ,
               RP50G,
               P0015,
               Q0015.
*               T100.

*----------------------------------------------------------------------*
INCLUDE  Z6BDCRECXX .

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
     END OF IDOWNLOAD_WA.

  DATA : BEGIN OF ITAB_WA,
         PERNR TYPE RP50G-PERNR,
         LGART TYPE P0015-LGART,
         BETRG(15),
         BEGDA(10),
         ZUORD TYPE P0015-ZUORD,
         END OF ITAB_WA.

  "DECLARATION OF internal table for Error records
* DATA : IT_BDCMSGCOLL type table of BDCMSGCOLL.
*
  DATA : BEGIN OF err_data OCCURS 0 ,
            PERNR  LIKE RP50G-PERNR ,
            LGART LIKE P0015-LGART,
            BEGDA(10),
            BETRG(15) ,
            ZUORD  LIKE P0015-ZUORD ,
            err(250) ,
          END   OF err_data .

  DATA : IDOWNLOAD LIKE STANDARD TABLE OF IDOWNLOAD_WA,
         ITAB1 LIKE STANDARD TABLE OF  ITAB_WA.

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
  'PERNR'	'NUMC' '8' '' 'PERSONNEL NUMBER',
  'LGART'	'CHAR' '4' ''	'WAGE TYPE',
  'BETRG'	'CURR' '13' '2'	'AMOUNT',
  'BEGDA' 'DATS' '8' '' 'START DATE',
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
  IDOWNLOAD_WA-T2  = 'LGART'.
  IDOWNLOAD_WA-T3  = 'BETRG'.
  IDOWNLOAD_WA-T4  = 'BEGDA'.
  IDOWNLOAD_WA-T5  = 'ZUORD'.


  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
  CLEAR IDOWNLOAD_WA.

  IDOWNLOAD_WA-T1  = 'PERSONNEL NUMBER'.
  IDOWNLOAD_WA-T2  = 'WAGE TYPE'.
  IDOWNLOAD_WA-T3  = 'AMOUNT'.
  IDOWNLOAD_WA-T4  = 'START DATE'.
  IDOWNLOAD_WA-T5  = 'ASSIGNMENT NUMBER'.

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
  LOOP AT ITAB1  INTO ITAB_WA.

    CONCATENATE ITAB_WA-BEGDA+6(2) ITAB_WA-BEGDA+4(2) ITAB_WA-BEGDA+0(4)
    INTO WA_BEGDA SEPARATED BY '.'.

* CONCATENATE ITAB_WA-ENDDA+6(2) ITAB_WA-ENDDA+4(2) ITAB_WA-ENDDA+0(4)
*  INTO WA_ENDDA SEPARATED BY '.'.
*    wa_amount = ITAB_WA-BETRG.
*      condense wa_amount.


    PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=INS'.
    PERFORM BDC_FIELD       USING 'RP50G-PERNR'
*                                '22'.
                                   ITAB_WA-PERNR.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RP50G-SUBTY'.
    PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                  '15'.
    PERFORM BDC_FIELD       USING 'RP50G-SUBTY'
*                                '1103'.
                                  ITAB_WA-LGART.


    PERFORM BDC_DYNPRO      USING 'MP001500' '2040'.


    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'P0015-ZUORD'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=UPD'.


    PERFORM BDC_FIELD       USING 'Q0015-BETRG'
*                                '100.00'.
                                  ITAB_WA-BETRG.
    PERFORM BDC_FIELD      USING  'P0015-WAERS'
                                   'INR'.

    PERFORM BDC_FIELD       USING 'P0015-BEGDA'
                                   ITAB_WA-BEGDA.
    PERFORM BDC_FIELD       USING 'P0015-ZUORD'
*                                  'XYZ'.
                                   ITAB_WA-ZUORD.




*    PERFORM BDC_DYNPRO      USING 'MP001500' '2040'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                  'P0015-ZUORD'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                  '/00'.
**   PERFORM BDC_FIELD       USING 'P0015-LGART'
***                               '1103'.
**                              ITAB_WA-LGART.
*
*    PERFORM BDC_FIELD       USING 'Q0015-BETRG'
**                                '100.00'.
*                                  ITAB_WA-BETRG.
*
*    PERFORM BDC_FIELD       USING 'P0015-BEGDA'
*                                   ITAB_WA-BEGDA.
*    PERFORM BDC_FIELD       USING 'P0015-ZUORD'
**                                  'XYZ'.
*                                   ITAB_WA-ZUORD.
*
*    PERFORM BDC_DYNPRO      USING 'MP001500' '2040'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                  'P0015-ZUORD'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                  '=UPD'.
**
    REFRESH : MESSTAB.
    CLEAR   : MESSTAB.

    CALL TRANSACTION 'PA30' USING BDCDATA MODE V_MODE MESSAGES INTO MESSTAB.

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
      MOVE-CORRESPONDING ITAB_WA TO ERR_DATA.
      APPEND ERR_DATA.
      CLEAR  ERR_DATA.
    ENDLOOP.

    CLEAR BDCDATA.
    REFRESH BDCDATA.
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
                '|' NO-GAP , (10)  'wage type.'  NO-GAP COLOR
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
              '|' NO-GAP , (10) err_data-lgart  NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , (15) err_data-betrg NO-GAP COLOR COL_NORMAL ,
*{   REPLACE        SBXK900030                                        1
*\              '|' NO-GAP , (20)err_data-zuord NO-GAP COLOR COL_NORMAL ,
*&---------------------------------------------------------------------*
*---<< S/4HANA >>---*
*&---------------------------------------------------------------------*
* Changed On - Wednesday, October 17, 2018 11:55:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - There must be a space or equivalent character
* Solution   - Add Space
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*&---------------------------------------------------------------------*
              '|' NO-GAP , (20) err_data-zuord NO-GAP COLOR COL_NORMAL ,
*}   REPLACE
    '|' NO-GAP , (60) err_data-err(60)   NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP .

    AT LAST .
      WRITE : / sy-uline(120) .
    ENDAT .
  ENDLOOP .
ENDFORM.
