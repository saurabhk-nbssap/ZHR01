*&---------------------------------------------------------------------*
*& REPORT  Z6HR014C_INFOTYPE_008_UPL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR014C_INFOTYPE_008_UPL.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: 008 INFOTYPE UPLOAD
* OBJECT TYPE       : BDC                FUNC. CONSULTANT  :MANOHAR
*          DEVELOPER: SUPRIYA BHATNAGAR
*      CREATION DATE:   19.07.2010
*        DEV REQUEST:  IRDK900462
*  TCODE            :  ZPY_IT0008
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
TYPE-POOLS : TRUXS.
*----------------------------------------------------------------------*
*     TABLES
*----------------------------------------------------------------------*
TABLES:PA0001,
       PA0007,
       PA0003,
       T001P,
       T503,
       SSCRFIELDS .
*----------------------------------------------------------------------*
*INCLUDE  Z6BDCRECXX .

*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001 .
*PARAMETERS: P_FORE   RADIOBUTTON GROUP RAD1 DEFAULT 'X' ,
*            P_BACK   RADIOBUTTON GROUP RAD1 .
*SELECTION-SCREEN END   OF BLOCK S1 .
SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE TEXT-002 .
PARAMETERS: P_FILE  TYPE IBIPPARMS-PATH  .

SELECTION-SCREEN END OF BLOCK S2 .

SELECTION-SCREEN : BEGIN OF BLOCK S3  WITH FRAME TITLE TEXT-S03.
SELECTION-SCREEN PUSHBUTTON  /1(7) HELP USER-COMMAND INFO.
SELECTION-SCREEN PUSHBUTTON  15(17) DOWN USER-COMMAND DOWN.
SELECTION-SCREEN : END OF BLOCK S3.

INCLUDE Z6XX003I_BDC_NOTE.

INITIALIZATION.
*----------------------------------------------------------------------*
*  MOVE 'DOCUMENT UPLOAD MODE' TO  SSCRFIELDS-FUNCTXT_01.
  MOVE '@0S@' TO HELP.
  MOVE '@49@ DOWNLOAD' TO DOWN.
  PERFORM F_FILL_INFOTEXT.
* ----------------------------------------------------------------------*
*     GLOBLE DATA
*----------------------------------------------------------------------*
  DATA  V_MODE.
*  DATA : V_FILENAME TYPE STRING.
  data : V_FILENAME TYPE IBIPPARMS-PATH.
  DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

*----------------------------------------------------------------------*
*     INTERNAL TABLES
*----------------------------------------------------------------------*
  DATA : BEGIN OF IDATA_WA,
        PERNR	  TYPE PA0008-PERNR,
        BEGDA	  TYPE PA0008-BEGDA,
        ENDDA	  TYPE PA0008-ENDDA,
*        BEGDA    TYPE PA0008-BEGDA,
        PREAS   TYPE PA0008-PREAS,
        TRFAR   TYPE PA0008-TRFAR,
        TRFGB	  TYPE PA0008-TRFGB,
        TRFGR   TYPE PA0008-TRFGR,
        TRFST   TYPE PA0008-TRFST,
*        BSGRD   TYPE PA0008-BSGRD,
*        DIVGV   TYPE PA0008-DIVGV,
        LGA01   TYPE PA0008-LGA01,
        BET01   TYPE PA0008-BET01,
        LGA02   TYPE PA0008-LGA02,
        BET02   TYPE PA0008-BET02,
        LGA03	 	TYPE PA0008-LGA03,
        BET03   TYPE PA0008-BET03,
        LGA04   TYPE PA0008-LGA04,
        BET04   TYPE PA0008-BET04,
        LGA05   TYPE PA0008-LGA05,
        BET05   TYPE PA0008-BET05,
        LGA06   TYPE PA0008-LGA06,
        BET06   TYPE PA0008-BET06,
        LGA07   TYPE PA0008-LGA07,
        BET07   TYPE PA0008-BET07,
        LGA08   TYPE PA0008-LGA08,
        BET08	  TYPE PA0008-BET08,
        LGA09   TYPE PA0008-LGA09,
        BET09   TYPE PA0008-BET09,
        LGA10   TYPE PA0008-LGA10,
        BET10   TYPE PA0008-BET10,
        LGA11   TYPE PA0008-LGA11,
        BET11   TYPE PA0008-BET11,
        LGA12	 	TYPE PA0008-LGA12,
        BET12   TYPE PA0008-BET12,
        LGA13	  TYPE PA0008-LGA13,
        BET13   TYPE PA0008-BET13,
        LGA14   TYPE PA0008-LGA14,
        BET14   TYPE PA0008-BET14,
        LGA15   TYPE PA0008-LGA15,
        BET15   TYPE PA0008-BET15,
       	LGA16	  TYPE PA0008-LGA16,
        BET16   TYPE PA0008-BET16,
        LGA17   TYPE PA0008-LGA17,
        BET17   TYPE PA0008-BET17,
        LGA18   TYPE PA0008-LGA18,
        BET18   TYPE PA0008-BET18,
        LGA19   TYPE PA0008-LGA19,
        BET19   TYPE PA0008-BET19,
        LGA20   TYPE PA0008-LGA20,
        BET20   TYPE PA0008-BET20,
         END OF IDATA_WA.

  DATA : IFINAL_WA TYPE PA0008.

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
         T11(30) TYPE C,
         T12(30) TYPE C,
         T13(30) TYPE C,
         T14(30) TYPE C,
         T15(30) TYPE C,
         T16(30) TYPE C,
         T17(30) TYPE C,
         T18(30) TYPE C,
         T19(30) TYPE C,
         T20(30) TYPE C,
         T21(30) TYPE C,
         T22(30) TYPE C,
         T23(30) TYPE C,
         T24(30) TYPE C,
         T25(30) TYPE C,
         T26(30) TYPE C,
         T27(30) TYPE C,
         T28(30) TYPE C,
         T29(30) TYPE C,
         T30(30) TYPE C,
         T31(30) TYPE C,
         T32(30) TYPE C,
         T33(30) TYPE C,
         T34(30) TYPE C,
         T35(30) TYPE C,
         T36(30) TYPE C,
         T37(30) TYPE C,
         T38(30) TYPE C,
         T39(30) TYPE C,
         T40(30) TYPE C,
         T41(30) TYPE C,
         T42(30) TYPE C,
         T43(30) TYPE C,
         T44(30) TYPE C,
         T45(30) TYPE C,
         T46(30) TYPE C,
         T47(30) TYPE C,
         T48(30) TYPE C,
*       T49(30) TYPE C,
*       T50(5) TYPE C,
*       T30(5) TYPE C,
         END OF IDOWNLOAD_WA.

  DATA : BEGIN OF IPA0007_WA,
         PERNR TYPE PA0007-PERNR,
*         BEGDA TYPE PA0007-BEGDA,
         ENDDA TYPE PA0007-ENDDA,
         BEGDA TYPE PA0007-BEGDA,
         EMPCT TYPE PA0007-EMPCT,
         MOSTD TYPE PA0007-MOSTD,
         END OF IPA0007_WA.

  DATA : BEGIN OF IPA0001_WA,
         PERNR TYPE PA0001-PERNR,
         ENDDA TYPE PA0001-ENDDA,
         BEGDA TYPE PA0001-BEGDA,
         WERKS TYPE PA0001-WERKS,
         PERSG TYPE PA0001-PERSG,
         PERSK TYPE PA0001-PERSK,
         BTRTL TYPE PA0001-BTRTL,
          END OF IPA0001_WA.

  DATA : IDATA     LIKE STANDARD TABLE OF IDATA_WA,
         IDOWNLOAD LIKE STANDARD TABLE OF IDOWNLOAD_WA,
         IPA0007   LIKE STANDARD TABLE OF IPA0007_WA,
         IPA0001   LIKE STANDARD TABLE OF IPA0001_WA.

  DATA :  WA_EMPCT TYPE PA0007-EMPCT,
          WA_MOSTD TYPE PA0007-MOSTD,
          WA_WERKS TYPE PA0001-WERKS,
          WA_PERSG TYPE PA0001-PERSG,
          WA_PERSK TYPE PA0001-PERSK,
          WA_BTRTL TYPE PA0001-BTRTL.

  DATA : G_ANSWER              TYPE C,
         G_LINES_TAB           TYPE POPUPTEXT OCCURS 0
                               WITH HEADER LINE.

  DATA: TMP_ZEINH LIKE T549R-ZEINH.

  DATA: LD_FILENAME TYPE STRING,
        LD_PATH TYPE STRING,
        LD_FULLPATH TYPE STRING,
        LD_RESULT TYPE I.

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
     DEFAULT_FILE_NAME = 'FILE_INFO08'
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
START-OF-SELECTION .
*  IF P_FORE EQ 'X' .
*    V_MODE = 'A' .
*  ELSE .
*    V_MODE = 'E' .
*  ENDIF .
  PERFORM  FILE_UPLOAD  .
  PERFORM  FETCH_DATA.
  PERFORM  UPDATE_INFOTYPE.
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
  'BEGDA' 'DATS' '8' '' 'START DATE',
  'ENDDA' 'DATS' '8' '' 'END DATE',
*  'BEGDA' 'DATS' '8' '' 'START DATE',
  'PREAS'	'CHAR' '2' ''	'REASON FOR CHANGING MASTER DATA',
  'TRFAR' 'CHAR' '2' '' 'PAY SCALE TYPE',
  'TRFGB' 'CHAR' '2' '' 'PAY SCALE AREA',
  'TRFGR' 'CHAR' '8' '' 'PAY SCALE GROUP',
  'TRFST' 'CHAR' '2' '' 'PAY SCALE LEVEL',

*  'BSGRD'  'DEC' '5' '2' 'CAPACITY UTILIZATION LEVEL',
*  'DIVGV'  'DEC' '5' '2' 'WORKING HOURS PER PAYROLL PERIOD',

  'LGA01'	'CHAR' '4' '' 'WAGE TYPE',
  'BET01' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA02'	'CHAR' '4' '' 'WAGE TYPE',
  'BET02' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA03'	'CHAR' '4' '' 'WAGE TYPE',
  'BET03' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA04'	'CHAR' '4' '' 'WAGE TYPE',
  'BET04' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA05'	'CHAR' '4' '' 'WAGE TYPE',
  'BET05' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA06'	'CHAR' '4' '' 'WAGE TYPE',
  'BET06' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA07'	'CHAR' '4' '' 'WAGE TYPE',
  'BET07' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA08'	'CHAR' '4' '' 'WAGE TYPE',
  'BET08' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA09'	'CHAR' '4' '' 'WAGE TYPE',
  'BET09' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA10'	'CHAR' '4' '' 'WAGE TYPE',
  'BET10' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA11'	'CHAR' '4' '' 'WAGE TYPE',
  'BET11' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA12'	'CHAR' '4' '' 'WAGE TYPE',
  'BET12' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA13'	'CHAR' '4' '' 'WAGE TYPE',
  'BET13' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA14'	'CHAR' '4' '' 'WAGE TYPE',
  'BET14' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA15'	'CHAR' '4' '' 'WAGE TYPE',
  'BET15' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA16'	'CHAR' '4' '' 'WAGE TYPE',
  'BET16' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA17'	'CHAR' '4' '' 'WAGE TYPE',
  'BET17' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA18'	'CHAR' '4' '' 'WAGE TYPE',
  'BET18' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA19'	'CHAR' '4' '' 'WAGE TYPE',
  'BET19' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',

  'LGA20'	'CHAR' '4' '' 'WAGE TYPE',
  'BET20' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS'.

ENDFORM.                    " F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*&      FORM  APPEND_FIELDS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM APPEND_FIELDS  USING    FIELD TYP WIDTH DECM REM.

  DATA : TEXT(140).
  TEXT = FIELD.
  TEXT+13(10)  = TYP.
  TEXT+24(6)  = WIDTH.
  TEXT+30(5)  = DECM.
  TEXT+35(80)  = REM.

  MOVE:  ' ' TO G_LINES_TAB-HELL,
         ' ' TO G_LINES_TAB-TOPOFPAGE,
         TEXT  TO G_LINES_TAB-TEXT.
  APPEND G_LINES_TAB.


ENDFORM.                    " APPEND_FIELDS
*&---------------------------------------------------------------------*
*&      FORM  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FILE_UPLOAD .

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      FILENAME                      = V_FILENAME
*     FILETYPE                      = 'DAT'
*
**   HEADER                        =
*    TABLES
*      DATA_TAB                      = IDATA
** EXCEPTIONS
**   FILE_OPEN_ERROR               = 1
**   FILE_READ_ERROR               = 2
*
*            .
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
   EXPORTING
*  *I_FIELD_SEPERATOR =
     I_LINE_HEADER = 'X'
     I_TAB_RAW_DATA = IT_RAW
     I_FILENAME = v_FILENAME
   TABLES
      I_TAB_CONVERTED_DATA = IDATA
   EXCEPTIONS
     CONVERSION_FAILED = 1
     OTHERS = 2.

  IF SY-SUBRC = 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      FORM  UPDATE_INFOTYPE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM UPDATE_INFOTYPE .

  DATA : RET TYPE HRPBSINBAPIRET,
         WA_RET TYPE LINE OF HRPBSINBAPIRET.

  DATA : W_COUNT(18) TYPE C,
 v_length  type i.

  CLEAR : IDATA_WA,IFINAL_WA.
  LOOP AT IDATA  INTO IDATA_WA.

* ---if pernr not maintained in infotype 0003.
    select single * from pa0003 where pernr = IDATA_WA-pernr.
*    ----
  if sy-subrc = 0.
    v_length = strlen( IDATA_WA-PREAS ).
    W_COUNT = 2 - v_length.
    DO W_COUNT TIMES.
      CONCATENATE '0' IDATA_WA-PREAS INTO IDATA_WA-PREAS.
    ENDDO.
    CLEAR : v_length,W_COUNT.

     v_length = strlen( IDATA_WA-TRFAR ).
    W_COUNT = 2 - v_length.
    DO W_COUNT TIMES.
      CONCATENATE '0' IDATA_WA-TRFAR INTO IDATA_WA-TRFAR.
    ENDDO.
    CLEAR : v_length,W_COUNT.

     v_length = strlen( IDATA_WA-TRFGB ).
    W_COUNT = 2 - v_length.
    DO W_COUNT TIMES.
      CONCATENATE '0' IDATA_WA-TRFGB INTO IDATA_WA-TRFGB.
    ENDDO.
    CLEAR : v_length,W_COUNT.

*    ----------------------------------------
    MOVE-CORRESPONDING IDATA_WA TO IFINAL_WA.
    ifinal_wa-SUBTY = '0'.
*----FETCH DATA FOR BSGRD AND DIVGV.
    PERFORM GET_DATA.


    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        NUMBER = IFINAL_WA-PERNR.

*    CALL FUNCTION 'HR_IN_SALARY_INFTY_UPDATE'
*      EXPORTING
*        WA0008_INS = IFINAL_WA
*        WA0902_MOD = WA_PA0902
*        WA0902_INS = WA_PA0902.
*    IF SY-SUBRC = 0.
*      WRITE : 'INFOTYPE SUCESSFULLY UPDATED FOR' ,IFINAL_WA-PERNR.
*    ELSE.
*      WRITE : 'INFOTYPE NOT UPDATED FOR' ,IFINAL_WA-PERNR.
*    ENDIF.

    CALL FUNCTION 'HRXSS_IN_INSERT_P0008'
      EXPORTING
        WA0008       = IFINAL_WA
        COMMIT       = 'X'
        CLEAR_BUFFER = 'X'
      IMPORTING
        ERR_MESSAGES = RET.

    IF NOT RET[] IS INITIAL.
      LOOP AT RET INTO WA_RET.        .
        IF WA_RET-TYPE = 'E'.
          WRITE : /'ERROR',WA_RET-MESSAGE,IFINAL_WA-PERNR.
        ELSE.
          WRITE : /'SUCESS',WA_RET-MESSAGE,IFINAL_WA-PERNR.
        ENDIF.
      ENDLOOP.
    ELSE.
      WRITE : /'INFOTYPE SUCESSFULLY UPDATED FOR',IFINAL_WA-PERNR.
    ENDIF.


    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        NUMBER = IFINAL_WA-PERNR.

    CLEAR IFINAL_WA.

    else.
      write :/'ERROR',idata_WA-PERNR,'does not exists in infotype 0003'.
     endif.
  ENDLOOP.
ENDFORM.                    " UPDATE_INFOTYPE
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

*  IDOWNLOAD_WA-T1  = 'PERNR'.
*  IDOWNLOAD_WA-T2  = 'BEGDA'.
*  IDOWNLOAD_WA-T3  = 'ENDDA'.
**  IDOWNLOAD_WA-T3  = 'BEGDA'.
*  IDOWNLOAD_WA-T4  = 'PREAS'.
*  IDOWNLOAD_WA-T5  = 'TRFAR'.
*  IDOWNLOAD_WA-T6  = 'TRFGB'.
*  IDOWNLOAD_WA-T7  = 'TRFGR'.
*  IDOWNLOAD_WA-T8  = 'TRFST'.
**IDOWNLOAD_WA-T9  = 'BSGRD'.
**IDOWNLOAD_WA-T10 = 'DIVGV'.
*  IDOWNLOAD_WA-T9 = 'LGA01'.
*  IDOWNLOAD_WA-T10 = 'BET01'.
*
*  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
*  CLEAR IDOWNLOAD_WA.

  IDOWNLOAD_WA-T1  = 'PERSONNEL NUMBER'.
  IDOWNLOAD_WA-T2  = 'START DATE'.
  IDOWNLOAD_WA-T3  = 'END DATE'.
  IDOWNLOAD_WA-T4  = 'REASON FOR CHANGING MASTER DATA'.
  IDOWNLOAD_WA-T5  = 'PAY SCALE TYPE'.
  IDOWNLOAD_WA-T6  = 'PAY SCALE AREA'.
  IDOWNLOAD_WA-T7  = 'PAY SCALE GROUP'.
  IDOWNLOAD_WA-T8  = 'PAY SCALE LEVEL'.
*IDOWNLOAD_WA-T9  = 'CAPACITY UTILIZATION LEVEL'.
*IDOWNLOAD_WA-T10  = 'WORKING HOURS PER PAYROLL PERIOD'.
  IDOWNLOAD_WA-T9 = 'WAGE TYPE 1'.
  IDOWNLOAD_WA-T10 = 'AMOUNT 1'.
   IDOWNLOAD_WA-T11 = 'WAGE TYPE 2'.
  IDOWNLOAD_WA-T12 = 'AMOUNT 2'.
  IDOWNLOAD_WA-T13 = 'WAGE TYPE 3'.
  IDOWNLOAD_WA-T14 = 'AMOUNT 3'.
  IDOWNLOAD_WA-T15 = 'WAGE TYPE 4'.
  IDOWNLOAD_WA-T16 = 'AMOUNT 4'.
  IDOWNLOAD_WA-T17 = 'WAGE TYPE 5'.
  IDOWNLOAD_WA-T18 = 'AMOUNT 5'.
  IDOWNLOAD_WA-T19 = 'WAGE TYPE 6'.
  IDOWNLOAD_WA-T20 = 'AMOUNT 6'.
  IDOWNLOAD_WA-T21 = 'WAGE TYPE 7'.
  IDOWNLOAD_WA-T22 = 'AMOUNT 7'.
  IDOWNLOAD_WA-T23 = 'WAGE TYPE 8'.
  IDOWNLOAD_WA-T24 = 'AMOUNT 8'.
  IDOWNLOAD_WA-T25 = 'WAGE TYPE 9'.
  IDOWNLOAD_WA-T26 = 'AMOUNT 9'.
  IDOWNLOAD_WA-T27 = 'WAGE TYPE 10'.
  IDOWNLOAD_WA-T28 = 'AMOUNT 10'.
  IDOWNLOAD_WA-T20 = 'WAGE TYPE 11'.
  IDOWNLOAD_WA-T30 = 'AMOUNT 11'.
  IDOWNLOAD_WA-T31 = 'WAGE TYPE 12'.
  IDOWNLOAD_WA-T32 = 'AMOUNT 12'.
  IDOWNLOAD_WA-T33 = 'WAGE TYPE 13'.
  IDOWNLOAD_WA-T34 = 'AMOUNT 13'.
  IDOWNLOAD_WA-T35 = 'WAGE TYPE 14'.
  IDOWNLOAD_WA-T36 = 'AMOUNT 14'.
  IDOWNLOAD_WA-T37 = 'WAGE TYPE 15'.
  IDOWNLOAD_WA-T38 = 'AMOUNT 15'.
  IDOWNLOAD_WA-T39 = 'WAGE TYPE 16'.
  IDOWNLOAD_WA-T40 = 'AMOUNT 16'.
  IDOWNLOAD_WA-T41 = 'WAGE TYPE 17'.
  IDOWNLOAD_WA-T42 = 'AMOUNT 17'.
  IDOWNLOAD_WA-T43 = 'WAGE TYPE 18'.
  IDOWNLOAD_WA-T44 = 'AMOUNT 18'.
  IDOWNLOAD_WA-T45 = 'WAGE TYPE 19'.
  IDOWNLOAD_WA-T46 = 'AMOUNT 19'.
  IDOWNLOAD_WA-T47 = 'WAGE TYPE 20'.
  IDOWNLOAD_WA-T48 = 'AMOUNT 20'.



  APPEND IDOWNLOAD_WA TO IDOWNLOAD.
  CLEAR IDOWNLOAD_WA.
ENDFORM.                    " FILL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      FORM  FETCH_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FETCH_DATA .
  IF NOT IDATA[] IS INITIAL.
    SELECT PERNR
           ENDDA
           BEGDA
           EMPCT
           MOSTD FROM PA0007
           INTO TABLE IPA0007
           FOR ALL ENTRIES IN IDATA
           WHERE PERNR = IDATA-PERNR.
    IF SY-SUBRC = 0.
      SORT IPA0007 BY PERNR
                      ENDDA
                      BEGDA.
    ENDIF.

    SELECT PERNR
           ENDDA
           BEGDA
           WERKS
           PERSG
           PERSK
           BTRTL FROM PA0001
           INTO TABLE IPA0001
           FOR ALL ENTRIES IN IDATA
           WHERE PERNR = IDATA-PERNR.
    IF SY-SUBRC = 0.
      SORT IPA0001 BY PERNR
                      ENDDA
                      BEGDA.
    ENDIF.


  ENDIF.

ENDFORM.                    " FETCH_DATA
*&---------------------------------------------------------------------*
*&      FORM  GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_DATA .
*  --PA0007
  READ TABLE IPA0007 INTO IPA0007_WA
           WITH KEY PERNR = IFINAL_WA-PERNR
           ENDDA = IFINAL_WA-ENDDA
           BEGDA = IFINAL_WA-BEGDA
           BINARY SEARCH.
  IF SY-SUBRC = 0.
    WA_EMPCT  = IPA0007_WA-EMPCT.
    WA_MOSTD  = IPA0007_WA-MOSTD .

  ELSE.
    READ TABLE IPA0007 INTO IPA0007_WA
                WITH KEY PERNR = IFINAL_WA-PERNR
                BINARY SEARCH.

    IF SY-SUBRC = 0.
      WA_EMPCT  = IPA0007_WA-EMPCT.
      WA_MOSTD  = IPA0007_WA-MOSTD .

    ENDIF.
  ENDIF.

*--PA0001
  READ TABLE IPA0001 INTO IPA0001_WA
           WITH KEY PERNR = IFINAL_WA-PERNR
           ENDDA = IFINAL_WA-ENDDA
           BEGDA = IFINAL_WA-BEGDA
           BINARY SEARCH.
  IF SY-SUBRC = 0.
    WA_WERKS  = IPA0001_WA-WERKS.
    WA_PERSG  = IPA0001_WA-PERSG .
    WA_PERSK  = IPA0001_WA-PERSK.
    WA_BTRTL  = IPA0001_WA-BTRTL.

  ELSE.
    READ TABLE IPA0001 INTO IPA0001_WA
                WITH KEY PERNR = IFINAL_WA-PERNR
                BINARY SEARCH.

    IF SY-SUBRC = 0.
      WA_WERKS  = IPA0001_WA-WERKS.
      WA_PERSG  = IPA0001_WA-PERSG .
      WA_PERSK  = IPA0001_WA-PERSK.
      WA_BTRTL  = IPA0001_WA-BTRTL.
    ENDIF.
  ENDIF.
*-----
  SELECT SINGLE * FROM T001P WHERE WERKS =  WA_WERKS
                             AND   BTRTL = WA_BTRTL.
  SELECT SINGLE * FROM T503 WHERE PERSG = WA_PERSG
                            AND   PERSK = WA_PERSK.
  if not WA_EMPCT is initial.
    ifinal_wa-BSGRD = WA_EMPCT.
  endif.

  if not t001p is initial .
    if not t503 is initial.
      CALL FUNCTION 'RP_ZEINH_GET'
        EXPORTING
          P_MOLGA        = T001P-MOLGA
          P_TRFGB        = ifinal_wa-TRFGB
          P_TRFAR        = ifinal_wa-TRFAR
          P_TRFKZ        = T503-TRFKZ
          P_DATE         = ifinal_wa-BEGDA
        IMPORTING
          P_ZEINH        = TMP_ZEINH
        EXCEPTIONS
          NO_ENTRY_T549R = 1
          ILLEGAL_ZEINH  = 2
          OTHERS         = 3.


      CASE TMP_ZEINH. "pay frequency
        WHEN '01'.      "monthly
          ifinal_wa-DIVGV = WA_MOSTD.
        WHEN '02'.      "semimonthly
          ifinal_wa-DIVGV = WA_MOSTD / 2.
        WHEN '03'.      "weekly
          ifinal_wa-DIVGV = WA_MOSTD.
        WHEN '04'.      "biweekly
          ifinal_wa-DIVGV = WA_MOSTD * 2.
        WHEN '05'.                                          "4-weekly
          ifinal_wa-DIVGV = WA_MOSTD * 4.
*      WHEN '06'.      "annual
        WHEN '07'.      "quarterly
          ifinal_wa-DIVGV = WA_MOSTD * 3.
        WHEN OTHERS.
          ifinal_wa-DIVGV = WA_MOSTD.
      ENDCASE.
    endif.
  endif.
ENDFORM.                    " GET_DATA
