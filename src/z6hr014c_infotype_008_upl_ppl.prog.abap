*&---------------------------------------------------------------------*
*& REPORT  Z6HR014C_INFOTYPE_008_UPL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6hr014c_infotype_008_upl_ppl.
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
TYPE-POOLS : truxs.
*----------------------------------------------------------------------*
*     TABLES
*----------------------------------------------------------------------*
TABLES:pa0001,
       pa0007,
       pa0003,
       t001p,
       t503,
       sscrfields .
*----------------------------------------------------------------------*
*INCLUDE  Z6BDCRECXX .

*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001 .
*PARAMETERS: P_FORE   RADIOBUTTON GROUP RAD1 DEFAULT 'X' ,
*            P_BACK   RADIOBUTTON GROUP RAD1 .
*SELECTION-SCREEN END   OF BLOCK S1 .
SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-002 .
*PARAMETERS: p_file  TYPE ibipparms-path  .
parameters: p_pernr type pa0008-pernr.

SELECTION-SCREEN END OF BLOCK s2 .

*SELECTION-SCREEN : BEGIN OF BLOCK s3  WITH FRAME TITLE text-s03.
*SELECTION-SCREEN PUSHBUTTON  /1(7) help USER-COMMAND info.
*SELECTION-SCREEN PUSHBUTTON  15(17) down USER-COMMAND down.
*SELECTION-SCREEN : END OF BLOCK s3.

INCLUDE z6xx003i_bdc_note.

INITIALIZATION.
*----------------------------------------------------------------------*
*  MOVE 'DOCUMENT UPLOAD MODE' TO  SSCRFIELDS-FUNCTXT_01.
*  MOVE '@0S@' TO help.
*  MOVE '@49@ DOWNLOAD' TO down.
*  PERFORM f_fill_infotext.
* ----------------------------------------------------------------------*
*     GLOBLE DATA
*----------------------------------------------------------------------*
  DATA  v_mode.
*  DATA : V_FILENAME TYPE STRING.
  DATA : v_filename TYPE ibipparms-path.
  DATA: it_raw TYPE truxs_t_text_data.

*----------------------------------------------------------------------*
*     INTERNAL TABLES
*----------------------------------------------------------------------*
  DATA : BEGIN OF idata_wa,
        pernr	  TYPE pa0008-pernr,
        begda(10),"	  TYPE pa0008-begda,
        endda(10),"	  TYPE pa0008-endda,
*        BEGDA    TYPE PA0008-BEGDA,
        preas   TYPE pa0008-preas, "Reason for Changing Master Data
        trfar   TYPE pa0008-trfar, "Pay scale type
        trfgb	  TYPE pa0008-trfgb, "Pay Scale Area
        trfgr   TYPE pa0008-trfgr, "Pay Scale Group
        trfst   TYPE pa0008-trfst, "Pay Scale Level
*        BSGRD   TYPE PA0008-BSGRD,
*        DIVGV   TYPE PA0008-DIVGV,
        lga01(08),"   TYPE pa0008-lga01,
        bet01   TYPE pa0008-bet01,
        lga02(08),"   TYPE pa0008-lga02,
        bet02   TYPE pa0008-bet02,
        lga03(08),"	 	TYPE pa0008-lga03,
        bet03   TYPE pa0008-bet03,
        lga04(08),"   TYPE pa0008-lga04,
        bet04   TYPE pa0008-bet04,
        lga05(08),"   TYPE pa0008-lga05,
        bet05   TYPE pa0008-bet05,
        lga06(08),"   TYPE pa0008-lga06,
        bet06   TYPE pa0008-bet06,
        lga07(08),"   TYPE pa0008-lga07,
        bet07   TYPE pa0008-bet07,
        lga08(08),"   TYPE pa0008-lga08,
        bet08	  TYPE pa0008-bet08,
        lga09(08),"   TYPE pa0008-lga09,
        bet09   TYPE pa0008-bet09,
        lga10(08),"  TYPE pa0008-lga10,
        bet10   TYPE pa0008-bet10,
        lga11(08),"   TYPE pa0008-lga11,
        bet11   TYPE pa0008-bet11,
        lga12(08),"	 	TYPE pa0008-lga12,
        bet12   TYPE pa0008-bet12,
        lga13(08),"	  TYPE pa0008-lga13,
        bet13   TYPE pa0008-bet13,
        lga14(08),"  TYPE pa0008-lga14,
        bet14   TYPE pa0008-bet14,
        lga15(08),"   TYPE pa0008-lga15,
        bet15   TYPE pa0008-bet15,
       	lga16(08),"	  TYPE pa0008-lga16,
        bet16   TYPE pa0008-bet16,
        lga17(08),"   TYPE pa0008-lga17,
        bet17   TYPE pa0008-bet17,
        lga18(08),"   TYPE pa0008-lga18,
        bet18   TYPE pa0008-bet18,
        lga19   TYPE pa0008-lga19,
        bet19   TYPE pa0008-bet19,
        lga20   TYPE pa0008-lga20,
        bet20   TYPE pa0008-bet20,
         END OF idata_wa.

  DATA : ifinal_wa TYPE pa0008.


DATA : IT_PA0008 like standard table of  idata_wa.

  DATA : BEGIN OF idownload_wa ,
         t1(30) TYPE c,
         t2(30) TYPE c,
         t3(30) TYPE c,
         t4(25) TYPE c,
         t5(30) TYPE c,
         t6(30) TYPE c,
         t7(30) TYPE c,
         t8(30) TYPE c,
         t9(30) TYPE c,
         t10(30) TYPE c,
         t11(30) TYPE c,
         t12(30) TYPE c,
         t13(30) TYPE c,
         t14(30) TYPE c,
         t15(30) TYPE c,
         t16(30) TYPE c,
         t17(30) TYPE c,
         t18(30) TYPE c,
         t19(30) TYPE c,
         t20(30) TYPE c,
         t21(30) TYPE c,
         t22(30) TYPE c,
         t23(30) TYPE c,
         t24(30) TYPE c,
         t25(30) TYPE c,
         t26(30) TYPE c,
         t27(30) TYPE c,
         t28(30) TYPE c,
         t29(30) TYPE c,
         t30(30) TYPE c,
         t31(30) TYPE c,
         t32(30) TYPE c,
         t33(30) TYPE c,
         t34(30) TYPE c,
         t35(30) TYPE c,
         t36(30) TYPE c,
         t37(30) TYPE c,
         t38(30) TYPE c,
         t39(30) TYPE c,
         t40(30) TYPE c,
         t41(30) TYPE c,
         t42(30) TYPE c,
         t43(30) TYPE c,
         t44(30) TYPE c,
         t45(30) TYPE c,
         t46(30) TYPE c,
         t47(30) TYPE c,
         t48(30) TYPE c,
*       T49(30) TYPE C,
*       T50(5) TYPE C,
*       T30(5) TYPE C,
         END OF idownload_wa.

  DATA : BEGIN OF ipa0007_wa,
         pernr TYPE pa0007-pernr,
*         BEGDA TYPE PA0007-BEGDA,
         endda TYPE pa0007-endda,
         begda TYPE pa0007-begda,
         empct TYPE pa0007-empct,
         mostd TYPE pa0007-mostd,
         END OF ipa0007_wa.

  DATA : BEGIN OF ipa0001_wa,
         pernr TYPE pa0001-pernr,
         endda TYPE pa0001-endda,
         begda TYPE pa0001-begda,
         werks TYPE pa0001-werks,
         persg TYPE pa0001-persg,
         persk TYPE pa0001-persk,
         btrtl TYPE pa0001-btrtl,
          END OF ipa0001_wa.

  DATA : idata     LIKE STANDARD TABLE OF idata_wa,
         idownload LIKE STANDARD TABLE OF idownload_wa,
         ipa0007   LIKE STANDARD TABLE OF ipa0007_wa,
         ipa0001   LIKE STANDARD TABLE OF ipa0001_wa.

  DATA :  wa_empct TYPE pa0007-empct,
          wa_mostd TYPE pa0007-mostd,
          wa_werks TYPE pa0001-werks,
          wa_persg TYPE pa0001-persg,
          wa_persk TYPE pa0001-persk,
          wa_btrtl TYPE pa0001-btrtl.

  DATA : g_answer              TYPE c,
         g_lines_tab           TYPE popuptext OCCURS 0
                               WITH HEADER LINE.

  DATA: tmp_zeinh LIKE t549r-zeinh.

  DATA: ld_filename TYPE string,
        ld_path TYPE string,
        ld_fullpath TYPE string,
        ld_result TYPE i.

AT SELECTION-SCREEN.
*  IF sscrfields-ucomm = 'INFO'.
*    CALL FUNCTION 'DD_POPUP_WITH_INFOTEXT'
*      EXPORTING
*        titel        = 'EXCEL FILE FORMAT '(020)
*        start_column = 10
*        start_row    = 10
*        end_column   = 85
*        end_row      = 27
*        infoflag     = ' '
*      IMPORTING
*        answer       = g_answer
*      TABLES
*        lines        = g_lines_tab.
*  ELSEIF sscrfields-ucomm = 'DOWN'.
*
*    PERFORM fill_download_data.
*    CALL METHOD cl_gui_frontend_services=>file_save_dialog
*   EXPORTING
**      WINDOW_TITLE      = ' '
*     default_extension = 'XLS'
*     default_file_name = 'FILE_INFO08'
*     initial_directory = 'C:\TEMP\'
*   CHANGING
*     filename          = ld_filename
*     path              = ld_path
*     fullpath          = ld_fullpath
*     user_action       = ld_result.
*
** CHECK USER DID NOT CANCEL REQUEST
*    CHECK ld_result EQ '0'.
*
*    CALL FUNCTION 'GUI_DOWNLOAD'
*     EXPORTING
*          filename         = ld_fullpath
*          filetype         = 'DAT'
**       APPEND           = 'X'
**        WRITE_FIELD_SEPARATOR = 'X'
**       CONFIRM_OVERWRITE = 'X'
*     TABLES
*          data_tab         = idownload[]
*     EXCEPTIONS
*          file_open_error  = 1
*          file_write_error = 2
*          OTHERS           = 3.
*
*
*  ENDIF.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*
*  CALL FUNCTION 'F4_FILENAME'
*    EXPORTING
*      program_name  = syst-cprog
*      dynpro_number = syst-dynnr
*      field_name    = 'P_FILE '
*    IMPORTING
*      file_name     = p_file.
*
*  IF NOT p_file IS INITIAL.
*    v_filename = p_file.
*  ENDIF.

*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION .
*  IF P_FORE EQ 'X' .
*    V_MODE = 'A' .
*  ELSE .
*    V_MODE = 'E' .
*  ENDIF .
  PERFORM  file_upload  .
  PERFORM  data_convert.
  PERFORM  fetch_data.
  PERFORM  update_infotype.
*&---------------------------------------------------------------------*
*&      FORM  F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
*FORM f_fill_infotext .
*  MOVE: 'X' TO g_lines_tab-hell,
*         'X' TO g_lines_tab-topofpage,
*         'FORMAT FOR UPLOAD DATA'(005)  TO g_lines_tab-text.
*  APPEND g_lines_tab.
*
*  MOVE:  'X' TO g_lines_tab-hell,
*          ' ' TO g_lines_tab-topofpage,
*          'FIELD        TYPE      WIDTH  DEC  REMARKS'(006)
*          TO g_lines_tab-text.
*  APPEND g_lines_tab.
*  MOVE ' ' TO g_lines_tab-text.
*  APPEND g_lines_tab.
*  PERFORM append_fields USING :
*  'PERNR'  'NUMC' '8' '' 'PERSONNEL NUMBER',
*  'BEGDA' 'DATS' '8' '' 'START DATE',
*  'ENDDA' 'DATS' '8' '' 'END DATE',
**  'BEGDA' 'DATS' '8' '' 'START DATE',
*  'PREAS'  'CHAR' '2' '' 'REASON FOR CHANGING MASTER DATA',
*  'TRFAR' 'CHAR' '2' '' 'PAY SCALE TYPE',
*  'TRFGB' 'CHAR' '2' '' 'PAY SCALE AREA',
*  'TRFGR' 'CHAR' '8' '' 'PAY SCALE GROUP',
*  'TRFST' 'CHAR' '2' '' 'PAY SCALE LEVEL',
*
**  'BSGRD'  'DEC' '5' '2' 'CAPACITY UTILIZATION LEVEL',
**  'DIVGV'  'DEC' '5' '2' 'WORKING HOURS PER PAYROLL PERIOD',
*
*  'LGA01'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET01' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA02'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET02' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA03'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET03' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA04'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET04' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA05'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET05' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA06'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET06' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA07'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET07' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA08'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET08' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA09'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET09' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA10'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET10' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA11'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET11' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA12'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET12' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA13'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET13' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA14'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET14' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA15'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET15' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA16'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET16' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA17'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET17' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA18'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET18' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA19'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET19' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS',
*
*  'LGA20'  'CHAR' '4' '' 'WAGE TYPE',
*  'BET20' 'CURR' '13' '2' 'WAGE TYPE AMOUNT FOR PAYMENTS'.
*
*ENDFORM.                    " F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*&      FORM  APPEND_FIELDS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM append_fields  USING    field typ width decm rem.

  DATA : text(140).
  text = field.
  text+13(10)  = typ.
  text+24(6)  = width.
  text+30(5)  = decm.
  text+35(80)  = rem.

  MOVE:  ' ' TO g_lines_tab-hell,
         ' ' TO g_lines_tab-topofpage,
         text  TO g_lines_tab-text.
  APPEND g_lines_tab.


ENDFORM.                    " APPEND_FIELDS
*&---------------------------------------------------------------------*
*&      FORM  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM file_upload .

IMPORT IT_PA0008 = IT_PA0008 FROM MEMORY ID 'ZHRBDC'.

idata = IT_PA0008.



**  CALL FUNCTION 'GUI_UPLOAD'
**    EXPORTING
**      FILENAME                      = V_FILENAME
**     FILETYPE                      = 'DAT'
**
***   HEADER                        =
**    TABLES
**      DATA_TAB                      = IDATA
*** EXCEPTIONS
***   FILE_OPEN_ERROR               = 1
***   FILE_READ_ERROR               = 2
**
**            .
**  IF SY-SUBRC <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**  ENDIF.

*  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*   EXPORTING
**  *I_FIELD_SEPERATOR =
*     i_line_header = 'X'
*     i_tab_raw_data = it_raw
*     i_filename = v_filename
*   TABLES
*      i_tab_converted_data = idata
*   EXCEPTIONS
*     conversion_failed = 1
*     OTHERS = 2.
*
*  IF sy-subrc = 0.
**  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      FORM  UPDATE_INFOTYPE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM update_infotype .

  DATA : ret TYPE hrpbsinbapiret,
         wa_ret TYPE LINE OF hrpbsinbapiret.

  DATA : w_count(18) TYPE c,
 v_length  TYPE i.

  CLEAR : idata_wa,ifinal_wa.
  LOOP AT idata  INTO idata_wa.

* ---if pernr not maintained in infotype 0003.
    SELECT SINGLE * FROM pa0003 WHERE pernr = idata_wa-pernr.
*    ----
    IF sy-subrc = 0.
      v_length = STRLEN( idata_wa-preas ).
      w_count = 2 - v_length.
      DO w_count TIMES.
        CONCATENATE '0' idata_wa-preas INTO idata_wa-preas.
      ENDDO.
      CLEAR : v_length,w_count.

      v_length = STRLEN( idata_wa-trfar ).
      w_count = 2 - v_length.
      DO w_count TIMES.
        CONCATENATE '0' idata_wa-trfar INTO idata_wa-trfar.
      ENDDO.
      CLEAR : v_length,w_count.

      v_length = STRLEN( idata_wa-trfgb ).
      w_count = 2 - v_length.
      DO w_count TIMES.
        CONCATENATE '0' idata_wa-trfgb INTO idata_wa-trfgb.
      ENDDO.
      CLEAR : v_length,w_count.

      concatenate idata_wa-begda+06(04) idata_wa-begda+03(02) idata_wa-begda(02) into idata_wa-begda.
      concatenate idata_wa-endda+06(04) idata_wa-endda+03(02) idata_wa-endda(02) into idata_wa-endda.

*    ----------------------------------------
      MOVE-CORRESPONDING idata_wa TO ifinal_wa.
      ifinal_wa-subty = '0'.
*----FETCH DATA FOR BSGRD AND DIVGV.
      PERFORM get_data.


      CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = ifinal_wa-pernr.

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
          wa0008       = ifinal_wa
          commit       = 'X'
          clear_buffer = 'X'
        IMPORTING
          err_messages = ret.

      IF NOT ret[] IS INITIAL.
        LOOP AT ret INTO wa_ret.        .
*          IF wa_ret-type = 'E'.
*            WRITE : /'ERROR',wa_ret-message,ifinal_wa-pernr.
*          ELSE.
*            WRITE : /'SUCESS',wa_ret-message,ifinal_wa-pernr.
*          ENDIF.
        ENDLOOP.
      ELSE.
*        WRITE : /'INFOTYPE SUCESSFULLY UPDATED FOR',ifinal_wa-pernr.
      ENDIF.


      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = ifinal_wa-pernr.

      CLEAR ifinal_wa.

    ELSE.
*      WRITE :/'ERROR',idata_wa-pernr,'does not exists in infotype 0003'.
    ENDIF.
  ENDLOOP.

  export ret to MEMORY ID 'ZHR_MSG'.
ENDFORM.                    " UPDATE_INFOTYPE
*&---------------------------------------------------------------------*
*&      FORM  FILL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM fill_download_data .
  CLEAR :idownload_wa,idownload.

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

  idownload_wa-t1  = 'PERSONNEL NUMBER'.
  idownload_wa-t2  = 'START DATE'.
  idownload_wa-t3  = 'END DATE'.
  idownload_wa-t4  = 'REASON FOR CHANGING MASTER DATA'.
  idownload_wa-t5  = 'PAY SCALE TYPE'.
  idownload_wa-t6  = 'PAY SCALE AREA'.
  idownload_wa-t7  = 'PAY SCALE GROUP'.
  idownload_wa-t8  = 'PAY SCALE LEVEL'.
*IDOWNLOAD_WA-T9  = 'CAPACITY UTILIZATION LEVEL'.
*IDOWNLOAD_WA-T10  = 'WORKING HOURS PER PAYROLL PERIOD'.
  idownload_wa-t9 = 'WAGE TYPE 1'.
  idownload_wa-t10 = 'AMOUNT 1'.
  idownload_wa-t11 = 'WAGE TYPE 2'.
  idownload_wa-t12 = 'AMOUNT 2'.
  idownload_wa-t13 = 'WAGE TYPE 3'.
  idownload_wa-t14 = 'AMOUNT 3'.
  idownload_wa-t15 = 'WAGE TYPE 4'.
  idownload_wa-t16 = 'AMOUNT 4'.
  idownload_wa-t17 = 'WAGE TYPE 5'.
  idownload_wa-t18 = 'AMOUNT 5'.
  idownload_wa-t19 = 'WAGE TYPE 6'.
  idownload_wa-t20 = 'AMOUNT 6'.
  idownload_wa-t21 = 'WAGE TYPE 7'.
  idownload_wa-t22 = 'AMOUNT 7'.
  idownload_wa-t23 = 'WAGE TYPE 8'.
  idownload_wa-t24 = 'AMOUNT 8'.
  idownload_wa-t25 = 'WAGE TYPE 9'.
  idownload_wa-t26 = 'AMOUNT 9'.
  idownload_wa-t27 = 'WAGE TYPE 10'.
  idownload_wa-t28 = 'AMOUNT 10'.
  idownload_wa-t20 = 'WAGE TYPE 11'.
  idownload_wa-t30 = 'AMOUNT 11'.
  idownload_wa-t31 = 'WAGE TYPE 12'.
  idownload_wa-t32 = 'AMOUNT 12'.
  idownload_wa-t33 = 'WAGE TYPE 13'.
  idownload_wa-t34 = 'AMOUNT 13'.
  idownload_wa-t35 = 'WAGE TYPE 14'.
  idownload_wa-t36 = 'AMOUNT 14'.
  idownload_wa-t37 = 'WAGE TYPE 15'.
  idownload_wa-t38 = 'AMOUNT 15'.
  idownload_wa-t39 = 'WAGE TYPE 16'.
  idownload_wa-t40 = 'AMOUNT 16'.
  idownload_wa-t41 = 'WAGE TYPE 17'.
  idownload_wa-t42 = 'AMOUNT 17'.
  idownload_wa-t43 = 'WAGE TYPE 18'.
  idownload_wa-t44 = 'AMOUNT 18'.
  idownload_wa-t45 = 'WAGE TYPE 19'.
  idownload_wa-t46 = 'AMOUNT 19'.
  idownload_wa-t47 = 'WAGE TYPE 20'.
  idownload_wa-t48 = 'AMOUNT 20'.



  APPEND idownload_wa TO idownload.
  CLEAR idownload_wa.
ENDFORM.                    " FILL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      FORM  FETCH_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM fetch_data .
  IF NOT idata[] IS INITIAL.
    SELECT pernr
           endda
           begda
           empct
           mostd FROM pa0007
           INTO TABLE ipa0007
           FOR ALL ENTRIES IN idata
           WHERE pernr = idata-pernr.
    IF sy-subrc = 0.
      SORT ipa0007 BY pernr
                      endda
                      begda.
    ENDIF.

    SELECT pernr
           endda
           begda
           werks
           persg
           persk
           btrtl FROM pa0001
           INTO TABLE ipa0001
           FOR ALL ENTRIES IN idata
           WHERE pernr = idata-pernr.
    IF sy-subrc = 0.
      SORT ipa0001 BY pernr
                      endda
                      begda.
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
FORM get_data .
*  --PA0007
  READ TABLE ipa0007 INTO ipa0007_wa
           WITH KEY pernr = ifinal_wa-pernr
           endda = ifinal_wa-endda
           begda = ifinal_wa-begda
           BINARY SEARCH.
  IF sy-subrc = 0.
    wa_empct  = ipa0007_wa-empct.
    wa_mostd  = ipa0007_wa-mostd .

  ELSE.
    READ TABLE ipa0007 INTO ipa0007_wa
                WITH KEY pernr = ifinal_wa-pernr
                BINARY SEARCH.

    IF sy-subrc = 0.
      wa_empct  = ipa0007_wa-empct.
      wa_mostd  = ipa0007_wa-mostd .

    ENDIF.
  ENDIF.

*--PA0001
  READ TABLE ipa0001 INTO ipa0001_wa
           WITH KEY pernr = ifinal_wa-pernr
           endda = ifinal_wa-endda
           begda = ifinal_wa-begda
           BINARY SEARCH.
  IF sy-subrc = 0.
    wa_werks  = ipa0001_wa-werks.
    wa_persg  = ipa0001_wa-persg .
    wa_persk  = ipa0001_wa-persk.
    wa_btrtl  = ipa0001_wa-btrtl.

  ELSE.
    READ TABLE ipa0001 INTO ipa0001_wa
                WITH KEY pernr = ifinal_wa-pernr
                BINARY SEARCH.

    IF sy-subrc = 0.
      wa_werks  = ipa0001_wa-werks.
      wa_persg  = ipa0001_wa-persg .
      wa_persk  = ipa0001_wa-persk.
      wa_btrtl  = ipa0001_wa-btrtl.
    ENDIF.
  ENDIF.
*-----
  SELECT SINGLE * FROM t001p WHERE werks =  wa_werks
                             AND   btrtl = wa_btrtl.
  SELECT SINGLE * FROM t503 WHERE persg = wa_persg
                            AND   persk = wa_persk.
  IF NOT wa_empct IS INITIAL.
    ifinal_wa-bsgrd = wa_empct.
  ENDIF.

  IF NOT t001p IS INITIAL .
    IF NOT t503 IS INITIAL.
      CALL FUNCTION 'RP_ZEINH_GET'
        EXPORTING
          p_molga        = t001p-molga
          p_trfgb        = ifinal_wa-trfgb
          p_trfar        = ifinal_wa-trfar
          p_trfkz        = t503-trfkz
          p_date         = ifinal_wa-begda
        IMPORTING
          p_zeinh        = tmp_zeinh
        EXCEPTIONS
          no_entry_t549r = 1
          illegal_zeinh  = 2
          OTHERS         = 3.


      CASE tmp_zeinh. "pay frequency
        WHEN '01'.      "monthly
          ifinal_wa-divgv = wa_mostd.
        WHEN '02'.      "semimonthly
          ifinal_wa-divgv = wa_mostd / 2.
        WHEN '03'.      "weekly
          ifinal_wa-divgv = wa_mostd.
        WHEN '04'.      "biweekly
          ifinal_wa-divgv = wa_mostd * 2.
        WHEN '05'.                                          "4-weekly
          ifinal_wa-divgv = wa_mostd * 4.
*      WHEN '06'.      "annual
        WHEN '07'.      "quarterly
          ifinal_wa-divgv = wa_mostd * 3.
        WHEN OTHERS.
          ifinal_wa-divgv = wa_mostd.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_convert .
  DATA: it_temp TYPE STANDARD TABLE OF z6hr_ppl_2_sap,
        wa_temp TYPE z6hr_ppl_2_sap.

  SELECT *
    FROM z6hr_ppl_2_sap
    INTO TABLE it_temp
  WHERE tabname = 'PA0008'.
  IF sy-subrc = 0.
    SORT it_temp BY field
                    ppl_sft.

    LOOP AT idata  INTO idata_wa.

*    PAY SCALE TYPE
      CLEAR: wa_temp.
*      READ TABLE it_temp INTO wa_temp
*                         WITH KEY field = 'TRFAR'
*                                  ppl_sft = idata_wa-trfar.
*      IF sy-subrc = 0.
*        idata_wa-trfar = wa_temp-sap.
*      ENDIF.

**    PAY SCALE AREA
*      CLEAR: wa_temp.
*      READ TABLE it_temp INTO wa_temp
*                         WITH KEY field = 'TRFGB'
*                                  ppl_sft = idata_wa-trfgb.
*      IF sy-subrc = 0.
*        idata_wa-trfgb = wa_temp-sap.
*      ENDIF.
*
**    PAY SCALE GROUP
*      CLEAR: wa_temp.
*      READ TABLE it_temp INTO wa_temp
*                         WITH KEY field = 'TRFGR'
*                                  ppl_sft = idata_wa-trfgr.
*      IF sy-subrc = 0.
*        idata_wa-trfgr = wa_temp-sap.
*      ENDIF.
*
**    PAY SCALE LEVEL
*      CLEAR: wa_temp.
*      READ TABLE it_temp INTO wa_temp
*                         WITH KEY field = 'TRFST'
*                                  ppl_sft = idata_wa-trfst.
*      IF sy-subrc = 0.
*        idata_wa-trfst = wa_temp-sap.
*      ENDIF.
*
**   Wage type
      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga01.
      IF sy-subrc = 0.
        idata_wa-lga01 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga02.
      IF sy-subrc = 0.
        idata_wa-lga02 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga03.
      IF sy-subrc = 0.
        idata_wa-lga03 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga04.
      IF sy-subrc = 0.
        idata_wa-lga04 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga05.
      IF sy-subrc = 0.
        idata_wa-lga05 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga06.
      IF sy-subrc = 0.
        idata_wa-lga06 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga07.
      IF sy-subrc = 0.
        idata_wa-lga07 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga08.
      IF sy-subrc = 0.
        idata_wa-lga08 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga09.
      IF sy-subrc = 0.
        idata_wa-lga09 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga10.
      IF sy-subrc = 0.
        idata_wa-lga10 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga11.
      IF sy-subrc = 0.
        idata_wa-lga11 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga12.
      IF sy-subrc = 0.
        idata_wa-lga12 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga13.
      IF sy-subrc = 0.
        idata_wa-lga13 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga14.
      IF sy-subrc = 0.
        idata_wa-lga14 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga15.
      IF sy-subrc = 0.
        idata_wa-lga15 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga16.
      IF sy-subrc = 0.
        idata_wa-lga16 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga17.
      IF sy-subrc = 0.
        idata_wa-lga17 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga18.
      IF sy-subrc = 0.
        idata_wa-lga18 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga19.
      IF sy-subrc = 0.
        idata_wa-lga19 = wa_temp-sap.
      ENDIF.

      CLEAR: wa_temp.
      READ TABLE it_temp INTO wa_temp
                         WITH KEY field = 'LGA01'
                                  ppl_sft = idata_wa-lga20.
      IF sy-subrc = 0.
        idata_wa-lga20 = wa_temp-sap.
      ENDIF.

      MODIFY idata FROM idata_wa
*                 TRANSPORTING trfar
*                              trfgb
*                              trfgr
*                              trfst
                                .
      CLEAR: idata_wa.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DATA_CONVERT
