*&---------------------------------------------------------------------*
*& Report  Z6HR016R_CORRES_LETTERS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z6HR019R_CORRES_LETTERS.

TABLES : pa0001,
         PA0000,
         PA0002,
         PA0008,
         PA0589,
         pa0006,
         T510,
         PA0041,
         T528T,
         hrp1000.
*&---------------------------------------------------------------------*&
* Data Declaration
*&---------------------------------------------------------------------*&

DATA : gt_pa0001 TYPE pA0001.
data : tt_pa0001 type TABLE OF pa0001.
data : v_lines type i.
data : wa6_pa0001 type pa0001.
DATA : gt_pb0001 TYPE pb0001.

DATA : IT_PA0002 TYPE P0002_TAB.
DATA : WA_PA0002 TYPE P0002.

DATA : IT_PA0008 TYPE P0008_TAB.
DATA : IT_T510 TYPE TABLE OF T510.
DATA : WA_T510 TYPE T510.
DATA : WA_PA0008 TYPE P0008.
DATA : w_ctr(2) TYPE N,
       w_wtf(14),
       w_hsl(14).
FIELD-SYMBOLS: <hsl> , <wtf>.
DATA : IT_PA0589 TYPE TABLE OF P0589.
DATA : WA_PA0589 TYPE P0589.

DATA : IT_PA0041 TYPE P0041_TAB.
DATA : WA_PA0041 TYPE PA0041.

DATA : WA_PA0000 TYPE PA0000.
DATA : WA1_PA0000 TYPE PA0000.
DATA : WA2_PA0000 TYPE PA0000.
DATA : WA3_PA0000 TYPE PA0000.
DATA : WA4_PA0000 TYPE PA0000.
DATA : WA5_PA0000 TYPE PA0000.

DATA : IT_PA0000 TYPE TABLE OF PA0000.
DATA : TT_PA0000 TYPE TABLE OF PA0000.


DATA : WA_PA0019 TYPE PA0019.
DATA : WA1_PA0019 TYPE PA0019.
DATA : WA2_PA0019 TYPE PA0019.
DATA: IT_PA0019 TYPE TABLE OF PA0019.

*{   REPLACE        SBXK900030                                        1
*\DATA : LV_WEEKDAY TYPE DTRESR-WEEKDAY.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 03, 2018 20:06:00
* Changed By - ABAP01 - BhushanM
* Purpose    - Simplification list - 2220005 - S/4 HANA: Data Model Changes in Pricing and Condition Technic
* Solution   - Used alternate Data Declaration
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
DATA : lv_weekday TYPE hrvsched-daytxt.
*}   REPLACE
DATA : LV_DATE TYPE SY-DATUM,
       lv_date1(25) type c,
       LV_LTX TYPE T247-LTX.
DATA : lf_formname  TYPE tdsfname,
       lf_fm_name   TYPE rs38l_fnam.
DATA : lf_formname1  TYPE tdsfname,
       lf_fm_name1   TYPE rs38l_fnam.
DATA : homephone    LIKE rhldapp-homephone,
       streetaddress LIKE rhldapp-streetaddress,
       SCNADDRESSLINE TYPE BAPIP0006-LOCAT,
       locality     LIKE rhldapp-locality,
       district      LIKE rhldapp-district,
       postalcode     LIKE rhldapp-postalcode,
       state          LIKE BAPIP0006-NAMEOFSTATE,
       country       LIKE BAPIP0006-NAMEOFLAND1.
DATA : WA_EMP_INF TYPE ZZLT_EMP_INF.
DATA : IT_SAL TYPE ZZTT_SALDATA,
       ITM_SAL TYPE ZZTT_SALDATA,
       V_ADRNR TYPE ADRC-ADDRNUMBER,
       WA_T526 TYPE T526,
       WA_SAL TYPE ZZLT_SALDATA.
DATA : LV_WORD TYPE SPELL.
DATA: addressempkey LIKE  bapipakey OCCURS 0 WITH HEADER LINE.
DATA: WA_HRCM_EMPINFO TYPE HRCM_EMPINFO.
**&---------------------------------------------------------------------*&
**  Include for selection screen
**&---------------------------------------------------------------------*&
include z6hri_selscreen1.
*&---------------------------------------------------------------------*&
* INCLUDE for AT SELECTION-SCREEN
*&---------------------------------------------------------------------*&
AT SELECTION-SCREEN OUTPUT.
  include z6hri_selscreen_modifications1.

*AT SELECTION-SCREEN ON pb_pernr.
*  IF pb_pernr IS NOT INITIAL.
*    SELECT SINGLE * FROM pb0001 INTO gt_pb0001
*    WHERE pernr = pb_pernr.
*    IF sy-subrc <> 0 .
*      MESSAGE 'Please enter correct Applicant Number' TYPE 'E' .
*    ENDIF.
*  ENDIF .

AT SELECTION-SCREEN ON pa_pernr.
  IF pa_pernr IS NOT INITIAL.
    SELECT SINGLE * FROM pa0001 INTO gt_pa0001
    WHERE pernr = pa_pernr.
    IF sy-subrc <> 0 .
      MESSAGE 'Please enter correct Personal Number' TYPE 'E' .
    ENDIF.
  ENDIF .

*&---------------------------------------------------------------------*&
*& Start of selection
*&---------------------------------------------------------------------*&

START-OF-SELECTION.

  PERFORM fetch_data.
  IF gt_pb0001 IS NOT INITIAL.
    PERFORM call_app_smartforms.
  ENDIF.
  IF gt_pa0001 IS NOT INITIAL.
    PERFORM call_emp_smartforms.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  fetch_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM fetch_data.
*
*  IF pb_pernr IS NOT INITIAL.
*    SELECT SINGLE * FROM pb0001 INTO gt_pb0001
*    WHERE pernr = pb_pernr.
*  ELSE
  IF pa_pernr IS NOT INITIAL.
    SELECT SINGLE * FROM pa0001 INTO gt_pa0001
    WHERE pernr = pa_pernr
      AND BEGDA LE SY-DATUM
      AND ENDDA GE SY-DATUM.

    SELECT  * FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE tt_pa0001
  WHERE pernr = pa_pernr.


    SELECT SINGLE ENAME FROM PA0001 INTO gt_pa0001-ENAME
    WHERE PERNR = gt_pa0001-MSTBR.


    SELECT SINGLE * FROM pa0002 INTO pa0002
     WHERE pernr = pa_pernr
       AND BEGDA LE SY-DATUM
       AND ENDDA GE SY-DATUM.

    SELECT single * FROM pa0041 INTO  wa_pa0041
     WHERE pernr = pa_pernr
       AND BEGDA LE SY-DATUM
       AND ENDDA GE SY-DATUM.


    SELECT  * FROM PA0000 INTO CORRESPONDING FIELDS OF TABLE IT_PA0000 WHERE PERNR = PA_PERNR.
    TT_PA0000 = IT_PA0000.
    sort it_pa0000 by endda DESCENDING.

    read table it_pa0000 into wa_pa0000 index 1.
    if sy-subrc eq 0.
      clear it_pa0000.
      append wa_pa0000 to it_pa0000.
      clear wa_pa0000.
    endif.


    SELECT * FROM PA0019 INTO CORRESPONDING FIELDS OF TABLE IT_PA0019 WHERE PERNR = PA_PERNR.





    if wa_pa0041-dar01 eq 'I1'.
      LV_DATE = WA_PA0041-DAT01.



      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX LV_DATE+6(2) ',' LV_DATE+0(4) INTO WA_EMP_INF-JOIDAT SEPARATED BY SPACE.
      ENDIF.

    ENDIF.

    if wa_pa0041-dar02 eq 'IO'.
      LV_DATE = WA_PA0041-DAT02.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2) ',' LV_DATE+0(4) INTO WA_EMP_INF-OFLDAT SEPARATED BY space.
      ENDIF.

    ENDIF.



    IF  WA_PA0041-dar03 eq 'IP'.

      LV_DATE = WA_PA0041-DAT03.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)','   LV_DATE+0(4)  INTO WA_EMP_INF-APPOINTMENTDATE SEPARATED BY SPACE .
      ENDIF.

    ENDIF.


    READ TABLE IT_PA0000 INTO WA1_PA0000 WITH KEY MASSN = 'I3' .

    IF WA1_PA0000-MASSN = 'I3'.

      LV_DATE = WA1_PA0000-BEGDA.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-CONFIRMATIONDATE SEPARATED BY SPACE.
      ENDIF.

    ENDIF.


    READ TABLE IT_PA0000 INTO WA2_PA0000 WITH KEY MASSN = 'I8'.

    IF WA2_PA0000-MASSN = 'I8'.

      LV_DATE = WA2_PA0000-BEGDA.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-ABSORPTIONDATE SEPARATED BY SPACE.
      ENDIF.
    ENDIF.





    READ TABLE IT_PA0000 INTO WA_PA0000 WITH KEY MASSN = 'I2' MASSG = '02'.

    if wa_pa0000-MASSN eq 'I2'
          AND  WA_PA0000-MASSG = '02'.

      LV_DATE = WA_PA0000-BEGDA.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-EXTENTIONDATE SEPARATED BY SPACE.
      ENDIF.
    ENDIF.


    READ TABLE TT_PA0000 INTO WA3_PA0000 WITH KEY MASSN = 'IB' .

    IF WA3_PA0000-MASSN = 'IB'.

      LV_DATE = WA3_PA0000-BEGDA.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-RESIGNDATE SEPARATED BY SPACE.
      ENDIF.

    ENDIF.


    READ TABLE IT_PA0000 INTO WA4_PA0000 WITH KEY MASSN = 'I7'.

    IF WA4_PA0000-MASSN = 'I7'.

      LV_DATE = WA4_PA0000-BEGDA.

      LV_DATE = LV_DATE - 1.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-CLOSINGDATE SEPARATED BY SPACE.
      ENDIF.
    ENDIF.
    WA_EMP_INF-DDAYS = 30 - ( WA4_PA0000-BEGDA - WA3_PA0000-BEGDA ).
    READ TABLE TT_PA0000 INTO WA5_PA0000 WITH KEY MASSN = 'I4' .

    IF WA5_PA0000-MASSN = 'I4'.

      LV_DATE = WA5_PA0000-BEGDA.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-RELOCATION SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

     READ TABLE TT_PA0000 INTO WA5_PA0000 WITH KEY MASSN = 'I5' .

    IF WA5_PA0000-MASSN = 'I5'.

      LV_DATE = WA5_PA0000-BEGDA.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-RELOCATION SEPARATED BY SPACE.
      ENDIF.
    ENDIF.




    READ TABLE IT_PA0019 INTO WA_PA0019 WITH KEY TMART = 'IA' .

    IF WA_PA0019-TMART = 'IA'.

      LV_DATE = WA_PA0019-TERMN.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-EXT_DATE2 SEPARATED BY SPACE.
      ENDIF.
    ENDIF.





    READ TABLE IT_PA0019 INTO WA1_PA0019 WITH KEY TMART = 'I8'.

    IF WA1_PA0019-TMART = 'I8'.

      LV_DATE = WA1_PA0019-TERMN.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
        CONCATENATE LV_LTX  LV_DATE+6(2)',' LV_DATE+0(4)  INTO WA_EMP_INF-PROBATIONEXTDATE SEPARATED BY SPACE.
      ENDIF.
    ENDIF.



    READ TABLE IT_PA0019 INTO WA2_PA0019 WITH KEY TMART = 'I4' .

    IF WA2_PA0019-TMART = 'I4'.

      LV_DATE = WA2_PA0019-TERMN.

      CALL FUNCTION 'ISP_GET_MONTH_NAME'
        EXPORTING
          DATE               =  LV_DATE
          LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
        IMPORTING
*        LANGU_BACK         =
          LONGTEXT           = LV_LTX
*        SHORTTEXT          =
       EXCEPTIONS
         CALENDAR_ID        = 1
         DATE_ERROR         = 2
         NOT_FOUND          = 3
         WRONG_INPUT        = 4
         OTHERS             = 5
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        TRANSLATE LV_LTX TO UPPER CASE.
*{   REPLACE        SBXK900030                                        1
*\        CALL FUNCTION 'DATE_TO_DAY'
*\          EXPORTING
*\            DATE    = LV_DATE
*\          IMPORTING
*\            WEEKDAY = LV_WEEKDAY.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 03, 2018 20:07:00
* Changed By - ABAP01 - BhushanM
* Purpose    - Simplification list - 2220005 - S/4 HANA: Data Model Changes in Pricing and Condition Technic
* Solution   - Used alternate FM
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
        CALL FUNCTION 'RH_GET_DATE_DAYNAME'
        EXPORTING
          langu                     = sy-langu
          DATE                      = lv_date
        IMPORTING
          daytxt                    = lv_weekday
        EXCEPTIONS
          no_langu                  = 1
          no_date                   = 2
          no_daytxt_for_langu       = 3
          invalid_date              = 4
          OTHERS                    = 5.
        IF SY-SUBRC <> 0.
*   Implement suitable error handling here
        ENDIF.
*}   REPLACE

        CONCATENATE LV_LTX  LV_DATE+6(2) ',' LV_DATE+0(4)  INTO WA_EMP_INF-RETIREMENTDATE SEPARATED BY SPACE.
        CONCATENATE LV_WEEKDAY  LV_LTX  LV_DATE+6(2)   ',' LV_DATE+0(4)  INTO WA_EMP_INF-RETIREDATE SEPARATED BY SPACE.
      ENDIF.
    ENDIF.





    CALL FUNCTION 'ISP_GET_MONTH_NAME'
       EXPORTING
         DATE               =  sy-datum
         LANGUAGE           = SY-LANGU
*        MONTH_NUMBER       = '00'
       IMPORTING
*        LANGU_BACK         =
         LONGTEXT           = LV_LTX
*        SHORTTEXT          =
      EXCEPTIONS
        CALENDAR_ID        = 1
        DATE_ERROR         = 2
        NOT_FOUND          = 3
        WRONG_INPUT        = 4
        OTHERS             = 5
               .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      TRANSLATE LV_LTX TO UPPER CASE.
      CONCATENATE LV_LTX sy-datum+6(2) ',' sy-datum+0(4) INTO wa_emp_inf-RUNDAT SEPARATED BY SPACE.
    ENDIF.
    SELECT  * FROM pa0008 INTO  CORRESPONDING FIELDS OF TABLE it_pa0008
    WHERE pernr = pa_pernr
      AND BEGDA LE SY-DATUM
      AND ENDDA GE SY-DATUM.

    sort it_pa0008 by begda DESCENDING.
    read table it_pa0008 into wa_pa0008 index 1.

*    MOVE-CORRESPONDING PA0008 TO WA_PA0008.

    SELECT  * FROM T510   INTO CORRESPONDING FIELDS OF TABLE IT_T510
    WHERE  TRFAR = WA_PA0008-TRFAR
      AND  TRFGB = WA_PA0008-TRFGB
      AND  TRFGR = WA_PA0008-TRFGR
      AND  TRFST = WA_PA0008-TRFST
      AND  LGART IN ('1018','1019','1020').

    SELECT SINGLE * FROM pa0589 INTO  pa0589
    wHERE pernr = pa_pernr
     AND BEGDA LE SY-DATUM
     AND ENDDA GE SY-DATUM.

    MOVE-CORRESPONDING PA0589 TO WA_PA0589.



    if not wa_pa0008 IS INITIAL.
      w_ctr = 1.
      WHILE w_ctr <= 20.
        CONCATENATE 'LGA' w_ctr INTO w_hsl.
        CONCATENATE 'BET' w_ctr INTO w_wtf.
        ASSIGN COMPONENT w_hsl of structure wa_pa0008 TO <hsl>.
        ASSIGN COMPONENT w_wtf of structure wa_pa0008 TO <WTF>.
        IF NOT <hsl> IS INITIAL.
          CASE <HSL>.
            WHEN  '1001' OR '1004' OR '1005' OR '1006' OR '1007' OR '1008'.
              WA_SAL-PERNR = PA_PERNR.
              WA_SAL-LGART = <HSL>.
              WA_SAL-BET01 = <WTF>.
              WA_SAL-AMONT = <WTF>.
              CONDENSE WA_SAL-AMONT NO-GAPS.
              SELECT SINGLE LGTXT FROM T512T INTO WA_SAL-LGTXT
                                             WHERE SPRSL EQ SY-LANGU
                                               AND MOLGA EQ '40'
                                               AND LGART EQ WA_SAL-LGART.

              CALL FUNCTION 'Z6XX_AMOUNT_IN_WORDS_INR'
                EXPORTING
                  I_AMOUNT         = WA_SAL-AMONT
                  I_CURRENCY       = wa_pa0008-WAERS
*                 I_FILLER         =
*                 I_LANGUAGE       =
                IMPORTING
                  E_IN_WORDS       = LV_WORD
                        .
              WA_SAL-WORDS = LV_WORD-WORD.

              APPEND WA_SAL TO IT_SAL.
              CLEAR WA_SAL.
          ENDCASE.
        ENDIF.


        w_ctr = w_ctr + 1.
      ENDWHILE.
    endif.
    if not wa_pa0589 IS INITIAL.
      w_ctr = 1.
      WHILE w_ctr <= 20.
        CONCATENATE 'LGA' w_ctr INTO w_hsl.
        CONCATENATE 'BET' w_ctr INTO w_wtf.

        ASSIGN COMPONENT w_hsl of structure wa_pa0589 TO <hsl>.
        ASSIGN COMPONENT w_wtf of structure wa_pa0589 TO <WTF>.
        IF NOT <hsl> IS INITIAL.
          CASE <HSL>.
            WHEN  '1101'  .
              WA_SAL-PERNR = PA_PERNR.
              WA_SAL-LGART = <HSL>.
              WA_SAL-BET01 = <WTF>.
              WA_SAL-AMONT = <WTF>.
              CONDENSE WA_SAL-AMONT NO-GAPS.
              SELECT SINGLE LGTXT FROM T512T INTO WA_SAL-LGTXT
                                             WHERE SPRSL EQ SY-Langu
                                               AND MOLGA EQ '40'
                                               AND LGART EQ WA_Sal-lgart.

              CALL FUNCTION 'Z6XX_AMOUNT_IN_WORDS_INR'
                EXPORTING
                  I_AMOUNT   = WA_SAL-AMONT
                  I_CURRENCY = wa_pa0008-WAERS
*                  I_FILLER   = I_LANGUAGE

                IMPORTING
                  E_IN_WORDS = LV_WORD.

              WA_SAL-WORDS = LV_WORD-WORD.

              APPEND WA_SAL TO IT_SAL.
              CLEAR WA_SAL.
            WHEN  '1102' OR '1108'  .
              WA_SAL-PERNR = PA_PERNR.
              WA_SAL-LGART = <HSL>.
              WA_SAL-BET01 = <WTF>.
              WA_SAL-AMONT = <WTF>.
              CONDENSE WA_SAL-AMONT NO-GAPS.
              SELECT SINGLE LGTXT FROM T512T INTO WA_SAL-LGTXT
                                             WHERE SPRSL EQ SY-LANGU
                                               AND MOLGA EQ '40'
                                               AND LGART EQ WA_SAL-LGART.
              CALL FUNCTION 'Z6XX_AMOUNT_IN_WORDS_INR'
                EXPORTING
                  I_AMOUNT         = WA_SAL-AMONT
                  I_CURRENCY       = wa_pa0008-WAERS
*                 I_FILLER         =
*                 I_LANGUAGE       =
                IMPORTING
                  E_IN_WORDS       = LV_WORD
                        .
              WA_SAL-WORDS = LV_WORD-WORD.

              APPEND WA_SAL TO ITM_SAL.
              CLEAR WA_SAL.
          endcase.
        endif.
        w_ctr = w_ctr + 1.
      ENDWHILE.
    endif.

    if not IT_T510 IS INITIAL.
      LOOP AT IT_T510 INTO WA_T510.
        WA_SAL-PERNR = PA_PERNR.
        WA_SAL-LGART = WA_T510-LGART.
        WA_SAL-BET01 = WA_T510-BETRG.
        WA_SAL-AMONT =  WA_T510-BETRG.
        CONDENSE WA_SAL-AMONT NO-GAPS.
        SELECT SINGLE LGTXT FROM T512T INTO WA_SAL-LGTXT
                                       WHERE SPRSL EQ SY-LANGU
                                         AND MOLGA EQ '40'
                                         AND LGART EQ WA_SAL-LGART.
        CALL FUNCTION 'Z6XX_AMOUNT_IN_WORDS_INR'
          EXPORTING
            I_AMOUNT         = WA_SAL-AMONT
            I_CURRENCY       = wa_pa0008-WAERS
*                 I_FILLER         =
*                 I_LANGUAGE       =
          IMPORTING
            E_IN_WORDS       = LV_WORD
                  .
        WA_SAL-WORDS = LV_WORD-WORD.


        if WA_SAL-LGART eq '1018'.
          wa_emp_inf-HFGEXP = WA_SAL-AMONT.
          WA_EMP_INF-HFGEXP_WORDS = LV_WORD-WORD.
          CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
            EXPORTING
              INPUT_STRING  = WA_EMP_INF-HFGEXP_WORDS
              SEPARATORS    = ' -.,;:'
            IMPORTING
              OUTPUT_STRING = WA_EMP_INF-HFGEXP_WORDS.


        endif.
        if WA_SAL-LGART eq '1019'.
          wa_emp_inf-TELEXP = WA_SAL-AMONT.
          WA_EMP_INF-TELEXP_WORDS = LV_WORD-WORD.
          CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
            EXPORTING
              INPUT_STRING  = WA_EMP_INF-TELEXP_WORDS
              SEPARATORS    = ' -.,;:'
            IMPORTING
              OUTPUT_STRING = WA_EMP_INF-TELEXP_WORDS.
        endif.
        if WA_SAL-LGART eq '1020'.
          wa_emp_inf-ENTEXP = WA_SAL-AMONT.
          WA_EMP_INF-ENTEXP_WORDS = LV_WORD-WORD.
          CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
            EXPORTING
              INPUT_STRING  = WA_EMP_INF-ENTEXP_WORDS
              SEPARATORS    = ' -.,;:'
            IMPORTING
              OUTPUT_STRING = WA_EMP_INF-ENTEXP_WORDS.
        endif.
        APPEND WA_SAL TO ITM_SAL.
        CLEAR WA_SAL.

      ENDLOOP.
    ENDIF.




    LOOP AT IT_SAL INTO WA_SAL.
      CASE SY-TABIX.
        WHEN '1'.
          WA_SAL-SEQNO = 'a)'.
        WHEN '2'.
          WA_SAL-SEQNO = 'b)'.
        WHEN '3'.
          WA_SAL-SEQNO = 'c)'.

        WHEN '4'.
          WA_SAL-SEQNO = 'd)'.
        WHEN '5'.
          WA_SAL-SEQNO = 'e)'.
        WHEN '6'.
          WA_SAL-SEQNO = 'f)'.

*        WHEN '7'.
*          WA_SAL-SEQNO = 'g)'.

      ENDCASE.

      CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
        EXPORTING
          INPUT_STRING  = wa_sal-words
          SEPARATORS    = ' -.,;:'
        IMPORTING
          OUTPUT_STRING = wa_sal-words.



      Concatenate
        wa_sal-lgtxt ' of ' into wa_sal-text1.
      condense wa_sal-text1 .

      CONCATENATE ' Rs.' wa_sal-AMONT '/-' '(Rupees'  wa_sal-words ')' INTO WA_SAL-WORDS.

      if wa_Sal-lgart eq '1001'.
        wa_sal-text3 = ' per month in Management level '.

        CONCATENATE GT_PA0001-PERSK '.'  INTO WA_SAL-PERSK.
*         CONCATENATE WA_SAL-TEXT3  GT_PA0001-PERSK '.'  INTO WA_SAL-TEXT3.
      elseif wa_sal-lgart eq '1101'.
        wa_sal-text3 = ' per Annum for self and family to be claimed once in a year'.
        wa_sal-text4 = 'while proceeding on privilege leave subject to company policy.'.
      elseif wa_sal-lgart eq '1102'.
        CONCATENATE ' Rs.' '15000 /-' '(Rupees Fifteen Thousand Only )' INTO WA_SAL-WORDS.
        wa_sal-text4 = ' per Annum subject to company policy.'.
        wa_sal-text3 = 'Company'.
      else.
        wa_sal-text3 = ' per month.'.
      endif.
*      CONCATENATE WA_SAL-TEXT1 WA_SAL-TEXT2 WA_SAL-TEXT3 WA_SAL-TEXT4 INTO WA_SAL-TEXT1.


      modify it_sal from wa_sal.
      clear wa_sal.

    ENDLOOP.

    LOOP AT ITM_SAL INTO WA_SAL.
      CASE SY-TABIX.
        WHEN '1'.
          WA_SAL-SEQNO = 'g)'.

          wa_sal-words = ' Fifteen Thousand Only'.
          wa_sal-amont =  '15000'.

      ENDCASE.


      CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
        EXPORTING
          INPUT_STRING  = wa_sal-words
          SEPARATORS    = ' -.,;:'
        IMPORTING
          OUTPUT_STRING = wa_sal-words.



      Concatenate
        wa_sal-lgtxt ' of ' into wa_sal-text1.
      condense wa_sal-text1 .

      CONCATENATE ' Rs.' wa_sal-AMONT '/-' '(Rupees'  wa_sal-words ')' INTO WA_SAL-WORDS.

      if wa_Sal-lgart eq '1001'.

        wa_sal-text3 = ' per month in Management level '.

        CONCATENATE GT_PA0001-PERSK '.'  INTO WA_SAL-PERSK.
*         CONCATENATE WA_SAL-TEXT3  GT_PA0001-PERSK '.'  INTO WA_SAL-TEXT3.
      elseif wa_sal-lgart eq '1101'.
        wa_sal-text3 = ' per Annum for self and family to be claimed once in a year'.
        wa_sal-text4 = 'while proceeding on privilege leave subject to company policy.'.
      elseif wa_sal-lgart eq '1102'.
        concatenate ' Company '
        ' ' wa_sal-lgtxt ' of ' into wa_sal-text1.
        condense wa_sal-text1 .
*        concatenate ' Rs.' '15000 /-' '(Rupees Fifteen Thousand Only )' into wa_Sal-text2.
        wa_sal-text3 = ' per Annum subject to company policy.'.

      else.
        wa_sal-text3 = ' per month.'.
      endif.
*      CONCATENATE WA_SAL-TEXT1 WA_SAL-TEXT2 WA_SAL-TEXT3 WA_SAL-TEXT4 INTO WA_SAL-TEXT1.


      modify itm_sal from wa_sal.
      clear wa_sal.

    ENDLOOP.
    wa_sal-pernr = pa_pernr.
    wa_sal-seqno = 'h)'.
    wa_sal-text1 = 'Bonus,Provident Fund, Gratuity,'.
    wa_sal-text2 = 'Personal accident Insurance - as per Company Policy'..
    append wa_Sal to itM_sal.
    clear  wa_Sal.
    wa_sal-pernr = pa_pernr.
    wa_sal-seqno = 'i)'.
    wa_sal-text1 = 'Medical Insurance for self and family as per Company Policy, under '.
    wa_sal-text2 = 'Group Health Insurance Scheme. Please complete and return the '.
    wa_sal-text3 = 'enclosed Form immediately.'.
    append wa_Sal to itM_sal.
    clear  wa_Sal.



    CALL FUNCTION 'HRCM_EMPLOYEE_INFO_GET'
      EXPORTING
        EMP_PERNR             = PA_PERNR
*   EMP_ELOCK             = ' '
        PLVAR                 = ' '
        BEGDA                 = SY-DATUM
*   ENDDA                 =
*   CAREA                 = ' '
*   BASIC_DATA_ONLY       = ' '
*   REACTION              = ' '
*  IMPORTING
*   RCODE                 =
* TABLES
*   ERROR_TABLE           =
      CHANGING
        EMP_INFO              = WA_HRCM_EMPINFO
              .


    CALL FUNCTION 'BAPI_ADDRESSEMP_GETLIST'
    EXPORTING
      employeenumber         = PA_PERNR
      SUBTYPE                = '01'
      timeintervallow        = SY-DATUM
      timeintervalhigh       = SY-DATUM
*   IMPORTING
*     RETURN                 =
    TABLES
      addressempkey          = addressempkey.

    READ TABLE addressempkey INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_ADDRESSEMP_GETDETAIL'
        EXPORTING
          employeenumber          = addressempkey-employeeno
          subtype                 = addressempkey-subtype
          objectid                = addressempkey-objectid
          lockindicator           = addressempkey-lockindic
          validitybegin           = addressempkey-validbegin
          validityend             = addressempkey-validend
          recordnumber            = addressempkey-recordnr
        IMPORTING
*       RETURN                  =
*       ADDRESSTYPE             =
*       CONAME                  =
          streetandhouseno        = streetaddress
        SCNDADDRESSLINE         =  SCNADDRESSLINE
          city                    = locality
*          district                = district
          postalcodecity          = postalcode
*         state                   = state
*          country                 = country
          telephonenumber         = homephone
*       NAMEOFADDRESSTYPE       =
        NAMEOFSTATE             = STATE
        NAMEOFCOUNTRY           = COUNTRY
                .
      MOVE-CORRESPONDING WA_HRCM_EMPINFO TO WA_EMP_INF.
      move : streetaddress to wa_emp_inf-streetaddress,

             LOCALITY     to wa_emp_inf-LOCALITY,
             postalcode    to wa_emp_inf-postalcode,
             state         to wa_emp_inf-state,
             SCNADDRESSLINE TO WA_EMP_INF-SCNADDRESSLINE,
             GT_PA0001-PERSK         TO WA_EMP_INF-PERSK,
             country       to wa_emp_inf-country.
      move : P_CHRONO to wa_emp_inf-CHRONO.
      move : pa0002-nachn to wa_emp_inf-nachn.
      move : pa0002-ANRED to wa_Emp_inf-anred,
             GT_PA0001-ZZLOC_TEXT TO WA_EMP_INF-ZZLOC_TEXT,
             GT_PA0001-ENAME TO WA_EMP_INF-ENAME1.


      TRANSLATE WA_EMP_INF-ZZLOC_TEXT TO UPPER CASE.


      DESCRIBE TABLE tt_pa0001 lines v_lines.
*      sort tt_pa0001 by begda DESCENDING.
      v_lines = v_lines - 1.
      if v_lines gt 0.
        read table tt_pa0001 into wa6_pa0001 index v_lines.
        move : wa6_pa0001-zzloc_text to wa_emp_inf-zzloc_textp.
        TRANSLATE WA_EMP_INF-ZZLOC_TEXTp TO UPPER CASE.
      endif.
      select single atext from t522t into wa_emp_inf-atext
                               where sprsl eq sy-langu
                                 and anred eq wa_emp_inf-anred.

      select single adrnr from t001 into v_adrnr where bukrs = gt_pa0001-bukrs.
      if not v_adrnr is INITIAL.
        select single name1 from adrc into wa_emp_inf-cname
                      where addrnumber eq v_adrnr.
      endif.

      select single * from t526 into wa_t526 where werks = wa_emp_inf-werks
                                               and sachx  = 'COL'.

      CALL FUNCTION 'HR_GETEMPLOYEEDATA_FROMUSER'
        EXPORTING
          USERNAME                        = wa_t526-usrid
          VALIDBEGIN                      = SY-DATUM
        IMPORTING
          EMPLOYEENUMBER                  = WA_EMP_INF-EMPLOYEENUMBER
*          COUNTRYGROUPING                 =
          NAME                            = WA_EMP_INF-HNAME
*          BUSINESSAREA                    =
*          NAMEOFBUSAREA                   =
*          PERSONNELAREA                   =
*          NAMEOFPERSAREA                  =
*          PERSSUBAREA                     =
*          NAMEOFPERSSUBAREA               =
*          EMPLOYEEGROUP                   =
*          NAMEOFEMPGROUP                  =
*          EMPLOYEESUBGROUP                =
*          NAMEOFEMPSUBGROUP               =
*          ORGUNIT                         =
*          NAMEOFORGUNIT                   =
                  POSITION                        = wa_emp_inf-authpos
*         NAMEOFPOSITION                  = WA_EMP_INF-NAMEOFPOSITION
*          COSTCENTER                      =
*          NAMEOFCOSTCENTER                =
*          ADMINGROUP                      =
*          PAYROLLADMIN                    =
*          PERSONNELLADMIN                 =
*          TIMEADMIN                       =
*          CONTROLLINGAREA                 =
*          EMPLOYMENTSTATUS                =
*          PAYROLLAREA                     =
*          NAMEOFPAYROLLAREA               =
*          PAYROLLSTATUS                   =
*          COMPANYCODE                     =
*          NAMEOFCOMPANYCODE               =
*          USERLOGONLANGUAGE               =
*          USERDATEFORMAT                  =
*          USERDECIMALFORMAT               =
*          CURRENCY                        =
*          CURRENCYDECIMALS                =
*          SAPRELEASE                      =
*          ACCOUNTEDTO                     =
*          FIRSTNAME                       =
*          LASTNAME                        =
*          FIRSTNAMEROMAJI                 =
*          LASTNAMEROMAJI                  =
*          BIRTHDATE                       =
*          RETURN                          =
       EXCEPTIONS
         USER_NOT_FOUND                  = 1
         COUNTRYGROUPING_NOT_FOUND       = 2
         INFTY_NOT_FOUND                 = 3
         OTHERS                          = 4
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      SELECT SINGLE stext FROM HRP1000
             INTO WA_EMP_INF-NAMEOFPOSITION WHERE PLVAR EQ '01'
                                            AND OTYPE EQ 'S'
                                            AND OBJID EQ wa_emp_inf-authpos
                                            AND ISTAT EQ '1'
                                            AND BEGDA LE SY-DATUM
                                            AND ENDDA GE SY-DATUM.
      TRANSLATE WA_EMP_INF-HNAME TO UPPER CASE.
      TRANSLATE WA_EMP_INF-NAMEOFPOSITION TO UPPER CASE.
      TRANSLATE WA_EMP_INF-ENAME TO UPPER CASE.




    ENDIF.

  ELSE.
    MESSAGE 'Please enter correct number'  TYPE 'E'.
  ENDIF.

ENDFORM.                    "fetch_data

*&---------------------------------------------------------------------*
*&      Form  call_smartforms
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_app_smartforms.
*  IF al = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APPLICATION_LETTER'.
*    lf_formname1 = lf_formname.
*    IF p_mail1 = 'X'.
*      PERFORM print_form_mail USING lf_formname1..
*    ENDIF.
*
*  ELSEIF rt = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_REJECTION_LETTER'.
*    lf_formname1 = lf_formname.
*    IF p_mail2 = 'X'.
*      PERFORM print_form_mail USING lf_formname1..
*    ENDIF.
*
*  ELSEIF oj = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_OFFER_LET_FOR_JMS_OFF'.
*
*  ELSEIF  oj1 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_OFFER_LET_FOR_JMS_MNG'.
*
*  ELSEIF  jda = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_JMS_DIRECT_APP_KALWE'.
*
*  ELSEIF  amg1 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_MG1'.
*
*
*  ELSEIF  amg2 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_MG2'.
*
*
*  ELSEIF  amg3 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_MG3'.
*
*
*  ELSEIF  amg4 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_MG4'.
*
*  ELSEIF  amg5 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_MG5_MG6'.
*
*  ELSEIF  amg6 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_MG6'.
*
*  ELSEIF  ajmo = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_JMS_APP_LET_MUMBAI_OUTSTN'.
*
*  ELSEIF  ace = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_CONTRACT_EMPLOYEE'.
*
*  ELSEIF  add = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_APPR_DEG_DIPLOMA'.
*
*  ELSEIF  add1 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_APPR_DEG_NORMAL'.
*
*  ELSEIF  aiti = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_APPR_ITI'.
*
*  ELSEIF  ast = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_APP_LET_SUMMER_TRAINEE'.
*
*  ELSEIF  p_r16 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_TR_ENG_DEGREE_MUM'.
*
*  ELSEIF  p_r17 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_TR_ENG_DIPLOMA_MUM'.
*
*  ELSEIF  p_r18 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_TR_ENG_DEGREE_OUT_STN'.
*
*  ELSEIF  p_r19 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_TR_ENG_DIPLOMA_OUT_STN'.
*
*  ELSEIF  p_r20 = 'X' .
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_OFFER_LETTER_MNGMNT_TR'.
*  ENDIF.

* PERFORM printing_appl_layout.

ENDFORM.                    "call_smartforms

*&---------------------------------------------------------------------*
*&      Form  CALL_EMP_SMARTFORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM call_emp_smartforms .

  IF  l3 = 'X' .
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_APPOINT_LETTER_MGC'.

  ELSEIF l1 = 'X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_WELCOME_LETTER_EXP'.

  ELSEIF l2 = 'X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_WELCOME_LETTER'.

  ELSEIF l5 = 'X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_EXTENSION_OF_PROBATIO'.

  ELSEIF l6 = 'X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_CONFIRMATION_LETTER'.

  ELSEIF l7 = 'X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_TRAINEE_EXTENSION'.

  ELSEIF l8 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_ABSORPTION_LETTER'.

  ELSEIF l9 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_RESIGNATION_LETTER'.

  ELSEIF l10 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_RESIGNATION_REGRET'.

  ELSEIF l11 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_RETIREMENT_UNION'.

  ELSEIF l12 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_RETIREMENT_MANAGEMENT'.

  ELSEIF l13 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_RETIREMENT_EXTENTION'.

  ELSEIF l14 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_TRANSFER_RELOCATION'.

  ELSEIF l15 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_DEPT_TRANSFER'.

  ELSEIF l16 ='X'.
    CLEAR : lf_formname.
    lf_formname = 'Z6HR019S_RESIGNATION'.

*  ELSEIF p_pa3 = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_CONFIRMATION_LETTER'.
*  ELSEIF p_pa4 = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_CONFIRMATION_LETTER_JMS'.
*  ELSEIF p_pa5 = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_JOINING_FORM_MGR'.
*  ELSEIF p_pa6 = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_JOINING_FORM_JMS'.
* ELSEIF p_pa20 = 'X'.
*    CLEAR : lf_formname.
*    lf_formname = 'ZHRM_JOINING_FORM_MG5_ABOVE'.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION    lf_fm_name               "'/1BCDWB/SF00000015'
    EXPORTING

      I_EMP_INFO                  = WA_EMP_INF
      I_REPID                = SY-REPID
   TABLES
      IT_SAL             = IT_SAL
      itm_sal            = itm_sal
    EXCEPTIONS
   formatting_error           = 1
   internal_error             = 2
   send_error                 = 3
   user_canceled              = 4
   OTHERS                     = 5
            .
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " CALL_EMP_SMARTFORMS
*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM_MAIL
*&---------------------------------------------------------------------*
*       text : Sending Email to User
*----------------------------------------------------------------------*
*      -->P_LF_FORMNAME1  text
*----------------------------------------------------------------------*
FORM print_form_mail  USING    p_lf_formname1.

* Internal Table declarations
  DATA: i_otf TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead TYPE soli_tab,
        w_ctrlop TYPE ssfctrlop,
        w_compop TYPE ssfcompop,
        w_return TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data TYPE sodocchgi1,
        wa_buffer TYPE string,
* ” to convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in LIKE sood-objlen,
        v_len_out LIKE sood-objlen,
        v_len_outn TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING  formname           = p_lf_formname1
*                 variant            = ' '
*                 direct_call        = ' '
       IMPORTING  fm_name            = lf_fm_name
       EXCEPTIONS no_form            = 1
                  no_function_module = 2
                  OTHERS             = 3.
  IF sy-subrc <> 0.
  ENDIF.

  w_ctrlop-getotf    = 'X'.
  w_ctrlop-no_dialog = 'X'.
  w_compop-tdnoprev  = 'X'.

  CALL FUNCTION    lf_fm_name               "'/1BCDWB/SF00000015'
    EXPORTING
      control_parameters         = w_ctrlop
      output_options             = w_compop
      user_settings              = 'X'
      gt_pb0001                  = gt_pb0001
    IMPORTING
      job_output_info            = w_return
  EXCEPTIONS
      formatting_error           = 1
      internal_error             = 2
      send_error                 = 3
      user_canceled              = 4
      OTHERS                     = 5
                  .
  IF sy-subrc <> 0.
  ENDIF.

  i_otf[] = w_return-otfdata[].

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = v_len_in
    TABLES
      otf                   = i_otf
      lines                 = i_tline
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT i_tline.
    TRANSLATE i_tline USING '~'.
    CONCATENATE wa_buffer i_tline INTO wa_buffer.
  ENDLOOP.

  TRANSLATE wa_buffer USING '~'.
  DO.
    i_record = wa_buffer.
    APPEND i_record.
    SHIFT wa_buffer LEFT BY 255 PLACES.
    IF wa_buffer IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
* Attachment
  REFRESH: i_reclist,
           i_objtxt,
           i_objbin,
           i_objpack.
  CLEAR :  wa_objhead.
  i_objbin[] = i_record[].
* create message body  title and description

  i_objtxt = 'Letter'.
  APPEND i_objtxt.
  i_objtxt = ' '.
  APPEND i_objtxt.
  i_objtxt = 'With Regards,'.
  APPEND i_objtxt.
  i_objtxt = ' '.
  APPEND i_objtxt.
  i_objtxt = 'Bharat Bijlee Limited'.
  APPEND i_objtxt.
  i_objtxt = 'Human Resources Division'.
  APPEND i_objtxt.
  i_objtxt = 'Thane - Belapur Road'.
  APPEND i_objtxt.
  i_objtxt = 'Opp Airoli Railway Station'.
  APPEND i_objtxt.
  i_objtxt = 'Airoli, Navi Mumbai - 400 607'.
  APPEND i_objtxt.

  DESCRIBE TABLE i_objtxt LINES v_lines_txt.
  READ TABLE i_objtxt INDEX v_lines_txt.

  wa_doc_chng-obj_name   = 'Your candidature at Bharat Bijlee Limited'.
  wa_doc_chng-expiry_dat = sy-datum + 10.
  wa_doc_chng-obj_descr  = 'smartform'.
  wa_doc_chng-sensitivty = 'F'.
  wa_doc_chng-doc_size   = v_lines_txt * 255.
*# main text
* wa_doc_chng-doc_size = ( v_lines_txt - 1 ) * 255 + strlen( i_objtxt )
*.
  CLEAR i_objpack-transf_bin.
  i_objpack-head_start = 1.
  i_objpack-head_num   = 0.
  i_objpack-body_start = 1.
  i_objpack-body_num   = v_lines_txt.
  i_objpack-doc_type   = 'RAW'.
  APPEND i_objpack.

*# attachment
* (pdf-Attachment)
  i_objpack-transf_bin = 'X'.
  i_objpack-head_start = 1.
  i_objpack-head_num   = 0.
  i_objpack-body_start = 1.
* Länge des Attachment ermitteln
  DESCRIBE TABLE i_objbin LINES v_lines_bin.
  READ TABLE i_objbin INDEX v_lines_bin.
  i_objpack-doc_size  = v_lines_bin * 255 .
  i_objpack-body_num  = v_lines_bin.
  i_objpack-doc_type  = 'PDF'.
  i_objpack-obj_name  = 'Smart'.
  i_objpack-obj_descr = 'Letter'.
  APPEND i_objpack.
  CLEAR i_reclist.

  DATA : l_userid TYPE p0105-usrid_long.
  SELECT SINGLE usrid_long INTO l_userid
                           FROM pb0105
                          WHERE pernr = pa_pernr.
  i_reclist-receiver = l_userid.
  i_reclist-rec_type = 'U'.
  i_reclist-com_type = 'INT'.
  APPEND  i_reclist.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wa_doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = i_objpack
      object_header              = wa_objhead
      contents_bin               = i_objbin
      contents_txt               = i_objtxt
      receivers                  = i_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc EQ 0.
    MESSAGE 'Mail sent sent Sucessfully' TYPE 'S'.
  ELSE.
    MESSAGE 'Error occured in Sending the Mail' TYPE 'E'.
  ENDIF.
ENDFORM.                    " PRINT_FORM_MAIL
*&---------------------------------------------------------------------*
*&      Form  PRINTING_APPL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printing_appl_layout .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
  ENDIF.

  CALL FUNCTION    lf_fm_name               "'/1BCDWB/SF00000015'
    EXPORTING
      gt_pb0001                  = gt_pb0001
  EXCEPTIONS
      formatting_error           = 1
      internal_error             = 2
      send_error                 = 3
      user_canceled              = 4
      OTHERS                     = 5
                  .
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.                    " PRINTING_APPL_LAYOUT

*Text elements
*----------------------------------------------------------
* 001 Applicant Number
* 002 Personnel Number
* 003 Application Receipt Letter-Before Interview
* 004 Rejection Letter-Before Interview
* 005 Offer Letter-JMS
* 006 Appointment Letter-J&O-JMS - Direct Apt
* 007 Appointment Letter-J&O- MG I
* 008 Appointment Letter-J&O- MG II
* 009 Appointment Letter-J&O- MG III
* 010 Appointment Letter-J&O- MG IV
* 011 Appointment Letter-J&O- MG V
* 012 Appointment Letter-J&O-JMS - Mumbai & Outstation
* 013 Appointment Letter-J&O-Contract Employee
* 014 Appointment Letter- Apprentice Offering in College
* 015 Appointment Letter-J&O-ITI Apprentice
* 016 Stipend Increase Memo-Apprentice & Trainee Engineer-J&O
* 017 Appointment Letter-J&O- Summer Trainee
* 018 Offer Letter-Manager
* 019 Appointment Letter-J&O- MG VI
* 020 Appointment Letter-J&O-Apprentice (Degree & Diploma)
* 022 e-Mail
* 023 Appointment Letter-J&O-Trainee Engineer -Degree (Mumbai)
* 024 Appointment Letter-J&O-Trainee Engineer -Diploma (Mumbai)
* 025 Appointment Letter-J&O-Trainee Engineer Degree (Outstation)
* 026 Appointment Letter-J&O-Trainee Engineer Diploma (Outstation)
* 027 Relievning Memo
* 028 Relievning Memo - Apprentices
* 029 Confirmation Memo - Trainee Engineer
* 030 Confirmation Memo - JMS
* 031 Joining Form for Managers
* 032 Joining Form for JMS
* 033 Offer Letter for Management Trainee
* 034 Joining form for Manager V & above


*Selection texts
*----------------------------------------------------------
* P_MAIL2         Mail


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   Please enter correct Applicant Number
