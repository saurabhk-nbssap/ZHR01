*&---------------------------------------------------------------------*
*& Report  ZHR_APAY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZHR_APAY.
TABLES:PA0001,PA0501.
TYPE-POOLS:SLIS.
DATA : FM_NAME TYPE RS38L_FNAM.

TYPES: BEGIN OF ST_FINAL,
         PERNR      TYPE P_PERNR,
         GJAHR      TYPE GJAHR,
         MONAT      TYPE CHAR6,"monat,
         ENAME      TYPE EMNAM,
         MONTH      TYPE CHAR8,
         BASIC      TYPE PAD_AMT7S,
         DA         TYPE PAD_AMT7S,
         HRA        TYPE PAD_AMT7S,
         TRANS      TYPE PAD_AMT7S,
         EDUCT      TYPE PAD_AMT7S,
         LV_ENC     TYPE PAD_AMT7S,
         MEDIC      TYPE PAD_AMT7S,
         OVERTIME   TYPE PAD_AMT7S,
         SHIFT      TYPE PAD_AMT7S,
         LTA        TYPE PAD_AMT7S,
         INCENT     TYPE PAD_AMT7S,
         BONUS      TYPE PAD_AMT7S,
         OTHERS     TYPE PAD_AMT7S,
         TOTAL      TYPE PAD_AMT7S,
         PF_ARR     TYPE PAD_AMT7S,
         VPF_ARR    TYPE PAD_AMT7S,
         TAX        TYPE PAD_AMT7S,
         UNION      TYPE PAD_AMT7S,
         INSUR      TYPE PAD_AMT7S,
         OTH_DEDCT  TYPE PAD_AMT7S,
         DEDCT      TYPE PAD_AMT7S,
         NET_TOT    TYPE PAD_AMT7S,
         GROSS TYPE PAD_AMT7S,
       END OF ST_FINAL,
          BEGIN OF ST_PA0001,
         PERNR      TYPE PERSNO,
         ENAME      TYPE EMNAM,
         PLANS TYPE PLANS,
         PERSK TYPE PERSK,
       END OF ST_PA0001.
TYPES: BEGIN OF TY_FINAL2,
         PERNR      TYPE P_PERNR,
         GJAHR      TYPE GJAHR,
         MONAT      TYPE CHAR6,"monat,
         ENAME      TYPE EMNAM,
         MONTH      TYPE CHAR8,
         BASIC      TYPE PAD_AMT7S,
         DA         TYPE PAD_AMT7S,
         HRA        TYPE PAD_AMT7S,
         TRANS      TYPE PAD_AMT7S,
         EDUCT      TYPE PAD_AMT7S,
         OVERTIME   TYPE PAD_AMT7S,
         BONUS      TYPE PAD_AMT7S,
         LTA        TYPE PAD_AMT7S,
       END OF TY_FINAL2,

       BEGIN OF TY_FINAL3,
         PERNR      TYPE P_PERNR,
         GJAHR      TYPE GJAHR,
         MONAT      TYPE CHAR6,"monat,
         ENAME      TYPE EMNAM,
         MEDIC      TYPE PAD_AMT7S,
         LV_ENC     TYPE PAD_AMT7S,
         OTHERS     TYPE PAD_AMT7S,
         TOTAL      TYPE PAD_AMT7S,
         PF_ARR     TYPE PAD_AMT7S,
         VPF_ARR    TYPE PAD_AMT7S,
         TAX        TYPE PAD_AMT7S,
         OTH_DEDCT  TYPE PAD_AMT7S,
         DEDCT      TYPE PAD_AMT7S,
         NET_TOT    TYPE PAD_AMT7S,
         GROSS TYPE PAD_AMT7S,
       END OF TY_FINAL3,

       BEGIN OF TY_FINAL4,
         PERNR      TYPE P_PERNR,
         GJAHR      TYPE GJAHR,
         MONAT      TYPE CHAR6,"monat,
         ENAME      TYPE EMNAM,
         MONTH      TYPE CHAR8,
         BASIC      TYPE PAD_AMT7S,
         DA         TYPE PAD_AMT7S,
         HRA        TYPE PAD_AMT7S,
         TRANS      TYPE PAD_AMT7S,
         EDUCT      TYPE PAD_AMT7S,
         OVERTIME   TYPE PAD_AMT7S,
         BONUS      TYPE PAD_AMT7S,
         LTA        TYPE PAD_AMT7S,

         MEDIC      TYPE PAD_AMT7S,
         LV_ENC     TYPE PAD_AMT7S,
         OTHERS     TYPE PAD_AMT7S,
         TOTAL      TYPE PAD_AMT7S,
         PF_ARR     TYPE PAD_AMT7S,
         VPF_ARR    TYPE PAD_AMT7S,
         TAX        TYPE PAD_AMT7S,
         OTH_DEDCT  TYPE PAD_AMT7S,
         DEDCT      TYPE PAD_AMT7S,
         NET_TOT    TYPE PAD_AMT7S,
         GROSS TYPE PAD_AMT7S,
       END OF TY_FINAL4.

DATA: IT_FINAL      TYPE STANDARD TABLE OF ZSTR_UNION_ARR, WA_FINAL TYPE ZSTR_UNION_ARR.
DATA: IT_FINAL1     TYPE STANDARD TABLE OF ZSTR_UNION_ARR, WA_FINAL1 TYPE ZSTR_UNION_ARR.
DATA: IT_FINAL2     TYPE STANDARD TABLE OF ZSTR_UNION_ARR, WA_FINAL2 TYPE ZSTR_UNION_ARR.
DATA: IT_FINAL3     TYPE STANDARD TABLE OF ZSTR_UNION_ARR, WA_FINAL3 TYPE ZSTR_UNION_ARR.
DATA: IT_FINAL4     TYPE TABLE OF TY_FINAL4, WA_FINAL4 TYPE TY_FINAL4.
DATA: IT_FINAL5     TYPE TABLE OF TY_FINAL4, WA_FINAL5 TYPE TY_FINAL4.

DATA: IT_MONTH_NAMES TYPE TABLE OF T247,
      WA_MONTH_NAMES LIKE LINE OF IT_MONTH_NAMES.

CONSTANTS: FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
DATA: FIELDTAB TYPE SLIS_T_FIELDCAT_ALV,
      P_HEADING  TYPE SLIS_T_LISTHEADER,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      EVENTS   TYPE SLIS_T_EVENT,
      REPNAME  LIKE SY-REPID,
      F2CODE   LIKE SY-UCOMM VALUE  '&ETA',
      G_SAVE(1) TYPE C,
      G_EXIT(1) TYPE C,
      G_VARIANT LIKE DISVARIANT,
      GX_VARIANT LIKE DISVARIANT,
      P_VAIRAINT LIKE DISVARIANT,
      FLAG.
DATA: ALV_PRINT        TYPE SLIS_PRINT_ALV.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: C1 TYPE C RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND US1,
            C2 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_PERNR FOR PA0001-PERNR ,
                S_FAPER FOR PA0501-FPPER OBLIGATORY NO INTERVALS no-EXTENSION.
*PARAMETERS:  P_PERSA TYPE PERSA ,
*             P_BTRTL TYPE BTRTL.
SELECT-OPTIONS : P_PERSA FOR pa0001-werks no-EXTENSION no INTERVALS,
                 P_BTRTL FOR pa0001-btrtl no-EXTENSION no INTERVALS.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT. " ALV Variant
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.
  REPNAME = SY-REPID.
  PERFORM INITIALIZE_VARIANT.
  PERFORM BUILD_EVENTTAB USING EVENTS[].


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_FOR_VARIANT.

START-OF-SELECTION.
  PERFORM BUILD_COMMENT USING P_HEADING[].
  PERFORM GET_ATIVE_EMP.
  IF IT_FINAL IS NOT INITIAL.
    IF C1 IS NOT INITIAL.
      PERFORM PRINT_ARR.
    ENDIF.
  ENDIF.

  PERFORM INITIALIZE_FIELDCAT USING FIELDTAB[].
  IF C2 IS NOT INITIAL.
    IF IT_FINAL[] IS NOT INITIAL.
      PERFORM DISPLAY_ALV_GOD.
    ELSE.
      MESSAGE 'No data found. Kindly check the selection' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_ATIVE_EMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ATIVE_EMP .
  DATA: IT_PA0001     TYPE STANDARD TABLE OF ST_PA0001 , WA_PA0001 LIKE LINE OF IT_PA0001 , MONTH TYPE MONTH , YEAR TYPE GJAHR.
  DATA: LGART TYPE TABLE OF SEL_LGART,
        RETRO_MONTH_DIFF LIKE TABLE OF  HR99LRET_S_MONTH_DIFF,
        WA_RETRO LIKE LINE OF RETRO_MONTH_DIFF.

DATA: Z14value TYPE pa0014-betrg.

  SELECT PERNR
             ENAME PLANS PERSK
        FROM PA0001
        INTO TABLE IT_PA0001
        WHERE PERNR IN S_PERNR AND ENDDA = '99991231'.
*        AND WERKS in P_PERSA
*        AND btrtl in p_BTRTL.

  CALL FUNCTION 'MONTH_NAMES_GET'
   EXPORTING
     LANGUAGE                    = SY-LANGU
* IMPORTING
*   RETURN_CODE                 =
    TABLES
      MONTH_NAMES                 = IT_MONTH_NAMES
   EXCEPTIONS
     MONTH_NAMES_NOT_FOUND       = 1
     OTHERS                      = 2.

  LOOP AT IT_PA0001 INTO WA_PA0001.
    CLEAR: RETRO_MONTH_DIFF , LGART.
*LOOP AT .
*
*ENDLOOP.
    MONTH = S_FAPER-LOW(02).
    YEAR =  S_FAPER-LOW+02(04).

    CALL FUNCTION 'HR99L00_GET_RETRO_MONTH_DIFF'
      EXPORTING
        PERNR                      = WA_PA0001-PERNR
        MONTH                      = MONTH
        YEAR                       = YEAR
*   SEL_DATE                   = 'B'
      TABLES
        LGART                      = LGART
        RETRO_MONTH_DIFF           = RETRO_MONTH_DIFF
 EXCEPTIONS
   T500L_NOT_MAINTAINED       = 1
   NO_READ_AUTHORITY          = 2
   IMPORT_ERROR               = 3
   OTHERS                     = 4  .



    IF RETRO_MONTH_DIFF IS NOT INITIAL.

*      DELETE RETRO_MONTH_DIFF WHERE RET_MONTH >= MONTH AND RET_YEAR >= YEAR.

*      WA_RETRO-RET_MONTH
*WA_RETRO-RET_YEAR
*MONTH
*YEAR
*
      LOOP AT RETRO_MONTH_DIFF INTO WA_RETRO.

        CASE WA_RETRO-LGART.
          WHEN '1001'."'5000'.
            WA_FINAL-BASIC = WA_RETRO-BETRG.
          WHEN '1002' or '1003'."'5001' OR '5002'.
            WA_FINAL-DA = WA_FINAL-DA + WA_RETRO-BETRG.
          WHEN '1004'."'5003'.
            WA_FINAL-HRA = WA_RETRO-BETRG.
          WHEN '1005'."'5004'.
            WA_FINAL-EDUCT = WA_RETRO-BETRG.
          WHEN '1006'."'5005'.
            WA_FINAL-TRANS = WA_RETRO-BETRG.
          WHEN '1301' OR '1302' OR '1303'."'5100' OR '5101' OR '5102'.
            WA_FINAL-OVERTIME = WA_FINAL-OVERTIME + WA_RETRO-BETRG.
*          WHEN '5103' OR '5104' OR '5105' OR '5106'.
*            WA_FINAL-SHIFT = WA_FINAL-SHIFT + WA_RETRO-BETRG. " not required remarks by kashid

          WHEN '1011'.
            WA_FINAL-INCENT = WA_RETRO-BETRG.
          WHEN '/3F1'.
*            IF WA_RETRO-LGART = '/3F1'.
*              WA_RETRO-BETRG = WA_RETRO-BETRG * -1.
*            ENDIF.

            WA_FINAL-PF_ARR = WA_FINAL-PF_ARR + WA_RETRO-BETRG.
***  added by NK - 31.08.2016 - start
*          WHEN '5201' OR '5202'. " Bonus Diff. is only from April-15 to March-16
          WHEN '/3F2'.
*            IF WA_RETRO-LGART = '/3F2'.
*              WA_RETRO-BETRG = WA_RETRO-BETRG * -1.
*            ENDIF.
            WA_FINAL-VPF_ARR = WA_RETRO-BETRG.
          WHEN '1110'.
            wa_final-bonus = wa_retro-betrg.
          WHEN '1109'."OTHER EARNINGS chng by kashid "'1319'. " Gratuity
            WA_FINAL-OTHERS = WA_RETRO-BETRG.
          WHEN '1101'. " LTA Leave Travel Allowance
            WA_FINAL-LTA = WA_RETRO-BETRG.
          WHEN '1102'. " Medical
            WA_FINAL-MEDIC = WA_RETRO-BETRG.
          WHEN '1312' or '1313' or '1315'. " Leave EN
            WA_FINAL-LV_ENC = WA_FINAL-LV_ENC + WA_RETRO-BETRG.
          WHEN '1MVT'. " TDS
            WA_FINAL-TAX = WA_RETRO-BETRG.
          WHEN '2009'. " Other DED
            WA_FINAL-OTH_DEDCT = WA_RETRO-BETRG.
            if  WA_RETRO-BETRG > 0.
            CLEAR: Z14value.
            SELECT SINGLE betrg FROM pa0014 INTO Z14value WHERE pernr = WA_PA0001-PERNR AND endda = '99991231' AND subty = '2009'.
            IF  sy-subrc = 0.
              WA_FINAL-OTH_DEDCT = WA_FINAL-OTH_DEDCT - Z14value.
            ENDIF.
            ENDIF.
*  when '5201' or '5202'. " Total DED
*    wa_final-dedct = wa_retro-betrg.
*  when '5201' or '5202'. " Net Total
*    wa_final-net_tot = wa_retro-betrg.
***  added by NK - 31.08.2016 - end
          WHEN OTHERS.
        ENDCASE.
* Total DED

IF WA_RETRO-RET_MONTH >= MONTH AND WA_RETRO-RET_YEAR >= YEAR.
           CLEAR: WA_FINAL-BASIC,
                  WA_FINAL-DA,
                  WA_FINAL-HRA,
                  WA_FINAL-TRANS,
                  WA_FINAL-EDUCT,
*                  WA_FINAL-LV_ENC,
*                  WA_FINAL-MEDIC,
                  WA_FINAL-OVERTIME,
                  WA_FINAL-SHIFT,
*                  WA_FINAL-LTA,
                  WA_FINAL-INCENT,
*                  WA_FINAL-BONUS,
*                  WA_FINAL-OTHERS,
                  WA_FINAL-TOTAL,
                  WA_FINAL-PF_ARR,
                  WA_FINAL-VPF_ARR,
*                  WA_FINAL-TAX, "tds
                  WA_FINAL-UNION,
                  WA_FINAL-INSUR.
*                  WA_FINAL-OTH_DEDCT,
*                  WA_FINAL-DEDCT,
*                  WA_FINAL-NET_TOT,
*                  WA_FINAL-GROSS.

        ENDIF.



        WA_FINAL-DEDCT =  WA_FINAL-PF_ARR +  WA_FINAL-VPF_ARR +  WA_FINAL-TAX + WA_FINAL-OTH_DEDCT.

*        WA_FINAL-BONUS = ( WA_FINAL-BASIC + WA_FINAL-DA ) * 2 / 10 .

        wa_final-gross = WA_FINAL-basic + WA_FINAL-da + WA_FINAL-hra + WA_FINAL-educt + WA_FINAL-trans  + WA_FINAL-OVERTIME + WA_FINAL-LTA
                         + WA_FINAL-LV_ENC +  WA_FINAL-MEDIC + WA_FINAL-BONUS +  WA_FINAL-OTHERS.

*  Net Payable
        WA_FINAL-NET_TOT = WA_FINAL-gross - WA_FINAL-DEDCT.

*        IF WA_FINAL-PF_ARR < 0.
*           WA_FINAL-PF_ARR = WA_FINAL-PF_ARR * -1 .
*        ENDIF.
*
*        IF WA_FINAL-VPF_ARR < 0.
*           WA_FINAL-VPF_ARR = WA_FINAL-VPF_ARR * -1 .
*        ENDIF.
*
*        IF WA_FINAL-VPF_ARR < 0.
*           WA_FINAL-VPF_ARR = WA_FINAL-VPF_ARR * -1 .
*        ENDIF.


*I.	Basic – Wage type – 5000
*II.  DA – 5001 & 5002
*III.	HRA - 5003
*IV.  Education – 5004
*V.	Transport – 5005
*VI.  Over Time – 5100 + 5101 + 5102
*VII.	Shift Allowance – 5103 + 5104 + 5105 + 5106
*VIII.  Prod. Incentives – 5010
*IX.  Gross = Sum of all above components
*X.	PF. VPF. Arrears – 5201 + 5202
*Xi          Bonus Arrears = (Basic + DA) * 0.2. This bonus Arrears should be calculated only for the for-periods which doesn’t fall in current fiscal year.


        WA_FINAL-PERNR = WA_PA0001-PERNR.
        WA_FINAL-GJAHR = WA_RETRO-RET_YEAR.
        WA_FINAL-MONAT = WA_RETRO-RET_MONTH.
*        SELECT SINGLE ENAME FROM PA0001
*          INTO WA_FINAL-ENAME WHERE PERNR = WA_PA0001-PERNR.
        WA_FINAL-ENAME = WA_PA0001-ename.


        SELECT SINGLE PLSTX FROM T528T INTO WA_FINAL-PLSTX WHERE PLANS = WA_PA0001-PLANS AND SPRSL = 'EN' AND OTYPE = 'S' AND ENDDA = '99991231'.
        WA_FINAL-PERSK = WA_PA0001-PERSK.


*wa_final-ename      TYPE emnam,
        CLEAR: WA_MONTH_NAMES.
        READ TABLE IT_MONTH_NAMES INTO WA_MONTH_NAMES WITH KEY MNR = WA_FINAL-MONAT.
        WA_MONTH_NAMES-LTX = WA_MONTH_NAMES-LTX+0(3).

        CONCATENATE WA_MONTH_NAMES-LTX '-' WA_RETRO-RET_YEAR INTO WA_FINAL-MONTH.

*        IF WA_RETRO-RET_MONTH >= MONTH AND WA_RETRO-RET_YEAR >= YEAR.
*           CLEAR: WA_FINAL-BASIC,
*                  WA_FINAL-DA,
*                  WA_FINAL-HRA,
*                  WA_FINAL-TRANS,
*                  WA_FINAL-EDUCT,
**                  WA_FINAL-LV_ENC,
**                  WA_FINAL-MEDIC,
*                  WA_FINAL-OVERTIME,
*                  WA_FINAL-SHIFT,
**                  WA_FINAL-LTA,
*                  WA_FINAL-INCENT,
**                  WA_FINAL-BONUS,
**                  WA_FINAL-OTHERS,
*                  WA_FINAL-TOTAL,
*                  WA_FINAL-PF_ARR,
*                  WA_FINAL-VPF_ARR,
**                  WA_FINAL-TAX, "tds
*                  WA_FINAL-UNION,
*                  WA_FINAL-INSUR.
**                  WA_FINAL-OTH_DEDCT,
**                  WA_FINAL-DEDCT,
**                  WA_FINAL-NET_TOT,
**                  WA_FINAL-GROSS.
*
*        ENDIF.

        COLLECT WA_FINAL INTO IT_FINAL.

        MOVE-CORRESPONDING WA_FINAL TO WA_FINAL5.
        CLEAR:WA_FINAL5-MONAT,  WA_FINAL5-GJAHR , WA_FINAL5-month.
        COLLECT WA_FINAL5 INTO IT_FINAL5.

        CLEAR: WA_FINAL.

      ENDLOOP.
*wa_final-pernr = wa_pa0001-pernr.
*wa_final-gjahr = wa_retro-ret_year.
*wa_final-monat = wa_retro-ret_month.
**wa_final-ename      TYPE emnam,
* collect wa_final into it_final.
* clear: wa_final.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_FINAL INTO WA_FINAL.
    WA_FINAL1-ENAME = WA_FINAL-ENAME.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_FINAL-PERNR
      IMPORTING
        OUTPUT = WA_FINAL-PERNR.


    WA_FINAL1-PERNR = WA_FINAL-PERNR.
    WA_FINAL1-PERSK = WA_FINAL-PERSK.
    WA_FINAL1-PLSTX = WA_FINAL-PLSTX.


    COLLECT WA_FINAL1 INTO IT_FINAL1.
    CLEAR: WA_FINAL1.
  ENDLOOP.


*  CALL FUNCTION 'MONTH_NAMES_GET'
*   EXPORTING
*     language                    = sy-langu
** IMPORTING
**   RETURN_CODE                 =
*    TABLES
*      month_names                 = it_month_names
*   EXCEPTIONS
*     month_names_not_found       = 1
*     OTHERS                      = 2.


  LOOP AT IT_FINAL INTO WA_FINAL.
    WA_FINAL2-PERNR = WA_FINAL-PERNR.
    WA_FINAL2-GJAHR = WA_FINAL-GJAHR.
    WA_FINAL2-MONAT = WA_FINAL-MONAT.
    WA_FINAL2-ENAME = WA_FINAL-ENAME.

*    READ TABLE it_month_names INTO wa_month_names WITH KEY mnr = wa_final-monat.
*    wa_month_names-ltx = wa_month_names-ltx+0(3).
*
*    CONCATENATE wa_month_names-ltx '-' wa_final-monat INTO wa_final2-monat.

    WA_FINAL2-MONTH = WA_FINAL-MONTH.
    WA_FINAL2-BASIC = WA_FINAL-BASIC.
    WA_FINAL2-DA = WA_FINAL-DA.
    WA_FINAL2-HRA = WA_FINAL-HRA.
    WA_FINAL2-TRANS = WA_FINAL-TRANS.
    WA_FINAL2-EDUCT = WA_FINAL-EDUCT.
    WA_FINAL2-OVERTIME = WA_FINAL-OVERTIME.
    WA_FINAL2-BONUS = WA_FINAL-BONUS.
    WA_FINAL2-LTA = WA_FINAL-LTA.
    APPEND WA_FINAL2 TO IT_FINAL2.
    CLEAR: WA_FINAL2.

    WA_FINAL3-PERNR = WA_FINAL-PERNR.
    WA_FINAL3-GJAHR = WA_FINAL-GJAHR.
    WA_FINAL3-MONAT = WA_FINAL-MONAT.
    WA_FINAL3-ENAME = WA_FINAL-ENAME.

*
*    READ TABLE it_month_names INTO wa_month_names WITH KEY mnr = wa_final-monat.
*    wa_month_names-ltx = wa_month_names-ltx+0(3).
*
*    CONCATENATE wa_month_names-ltx '-' wa_final-monat INTO wa_final3-monat.

    WA_FINAL3-MONTH = WA_FINAL-MONTH.
    WA_FINAL3-MEDIC = WA_FINAL-MEDIC.   " medical
    WA_FINAL3-LV_ENC = WA_FINAL-LV_ENC. " Leave En
    WA_FINAL3-OTHERS = WA_FINAL-OTHERS. " Gratuity
    WA_FINAL3-TOTAL = WA_FINAL-TOTAL.   " Gross Tot
    WA_FINAL3-PF_ARR = WA_FINAL-PF_ARR. " Pf
    WA_FINAL3-VPF_ARR = WA_FINAL-VPF_ARR." VPF
    WA_FINAL3-TAX = WA_FINAL-TAX.       "TDS
    WA_FINAL3-OTH_DEDCT = WA_FINAL-OTH_DEDCT." Other Ded
    WA_FINAL3-DEDCT = WA_FINAL-DEDCT.    " Total Ded
    WA_FINAL3-NET_TOT = WA_FINAL-NET_TOT." Net Payable
    WA_FINAL3-gross = WA_FINAL-gross." GROSS
    APPEND WA_FINAL3 TO IT_FINAL3.
    CLEAR: WA_FINAL3.


  ENDLOOP.

*  CLEAR: wa_month_names.
*  LOOP AT it_final INTO wa_final.
*    wa_final3-pernr = wa_final-pernr.
*    wa_final3-gjahr = wa_final-gjahr.
*    wa_final3-monat = wa_final-monat.
*    wa_final3-ename = wa_final-ename.
*
*
*    READ TABLE it_month_names INTO wa_month_names WITH KEY mnr = wa_final-monat.
*    wa_month_names-ltx = wa_month_names-ltx+0(3).
*
*    CONCATENATE wa_month_names-ltx '-' wa_final-monat INTO wa_final3-monat.
*
*
*    wa_final3-medic = wa_final-medic.   " medical
*    wa_final3-lv_enc = wa_final-lv_enc. " Leave En
*    wa_final3-others = wa_final-others. " Gratuity
*    wa_final3-total = wa_final-total.   " Gross Tot
*    wa_final3-pf_arr = wa_final-pf_arr. " Pf
*    wa_final3-vpf_arr = wa_final-vpf_arr." VPF
*    wa_final3-tax = wa_final-tax.       "TDS
*    wa_final3-oth_dedct = wa_final-oth_dedct." Other Ded
*    wa_final3-dedct = wa_final-dedct.    " Total Ded
*    wa_final3-net_tot = wa_final-net_tot." Net Payable
*    APPEND wa_final3 TO it_final3.
*    CLEAR: wa_final3.
*  ENDLOOP.

  DATA LV_MONAT TYPE CHAR6.

*  LOOP AT it_final INTO wa_final.
*
*    READ TABLE it_month_names INTO wa_month_names WITH KEY mnr = wa_final-monat." spras = sy-langu.
*    wa_month_names-ltx = wa_month_names-ltx+0(3).
*    CONCATENATE wa_month_names-ltx '-' wa_final-monat INTO lv_monat."wa_final-monat.
*
*    READ TABLE it_final2 INTO wa_final2 WITH KEY pernr = wa_final-pernr gjahr = wa_final-gjahr monat = lv_monat.
*    IF sy-subrc EQ 0.
**      MOVE-CORRESPONDING wa_final2 TO wa_final4.
*      MOVE:
*      wa_final2-pernr TO wa_final4-pernr,
*      wa_final2-gjahr TO wa_final4-gjahr,
*      wa_final2-monat TO wa_final4-monat,
*      wa_final2-ename TO wa_final4-ename,
*      wa_final2-basic TO wa_final4-basic,
*      wa_final2-da    TO wa_final4-da,
*      wa_final2-hra   TO wa_final4-hra,
*      wa_final2-trans TO wa_final4-trans,
*      wa_final2-educt TO wa_final4-educt,
*      wa_final2-overtime TO wa_final4-overtime,
*      wa_final2-bonus    TO wa_final4-bonus,
*      wa_final2-lta      TO wa_final4-lta.
*    ENDIF.
*    READ TABLE it_final3 INTO wa_final3 WITH KEY pernr = wa_final-pernr gjahr = wa_final-gjahr monat = lv_monat.
*    IF sy-subrc EQ 0.
**      MOVE-CORRESPONDING wa_final3 TO wa_final4.
*      MOVE:
*       wa_final3-medic     TO wa_final4-medic,
*       wa_final3-lv_enc    TO wa_final4-lv_enc,
*       wa_final3-others    TO wa_final4-others,
*       wa_final3-total     TO wa_final4-total,
*       wa_final3-pf_arr    TO wa_final4-pf_arr,
*       wa_final3-vpf_arr   TO wa_final4-vpf_arr,
*       wa_final3-tax       TO wa_final4-tax,
*       wa_final3-oth_dedct TO wa_final4-oth_dedct,
*       wa_final3-dedct     TO wa_final4-dedct,
*       wa_final3-net_tot   TO wa_final4-net_tot.
*    ENDIF.
*    APPEND wa_final4 TO it_final4.
*    CLEAR wa_final4.
*  ENDLOOP.

*  LOOP AT it_final4 INTO wa_final4.
*
*    wa_final5-ename = wa_final4-ename.
*
*    wa_final5-basic   = wa_final4-basic.
*    wa_final5-da      = wa_final4-da.
*    wa_final5-hra     = wa_final4-hra.
*    wa_final5-trans   = wa_final4-trans.
*    wa_final5-educt   = wa_final4-educt.
*    wa_final5-overtime = wa_final4-overtime.
*    wa_final5-bonus =    wa_final4-bonus.
*    wa_final5-lta =      wa_final4-lta.
*    wa_final5-medic   =     wa_final4-medic.
*    wa_final5-lv_enc  =     wa_final4-lv_enc.
*    wa_final5-others  =     wa_final4-others.
*    wa_final5-total   =     wa_final4-total.
*    wa_final5-pf_arr  =     wa_final4-pf_arr.
*    wa_final5-vpf_arr =     wa_final4-vpf_arr.
*    wa_final5-tax     =     wa_final4-tax.
*    wa_final5-oth_dedct =   wa_final4-oth_dedct.
*    wa_final5-dedct     =   wa_final4-dedct.
*    wa_final5-net_tot   =   wa_final4-net_tot.
*
*    wa_final5-pernr = wa_final4-pernr.
*
*    COLLECT wa_final5 INTO it_final5.
*    CLEAR wa_final5.
*  ENDLOOP.

  SORT IT_FINAL.
  DELETE ADJACENT DUPLICATES FROM IT_FINAL COMPARING ALL FIELDS.

  SORT IT_FINAL2.
  DELETE ADJACENT DUPLICATES FROM IT_FINAL2 COMPARING ALL FIELDS.

  SORT IT_FINAL3.
  DELETE ADJACENT DUPLICATES FROM IT_FINAL3 COMPARING ALL FIELDS.
ENDFORM.                    " GET_ATIVE_EMP
*&---------------------------------------------------------------------*
*&      Form  PRINT_ARR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_ARR .
  DATA : CONTROL TYPE SSFCTRLOP.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
   EXPORTING
     FORMNAME                 = 'Z6HR_UNION_BONUS_ARREAR_N'
*   VARIANT                  = ' '
*   DIRECT_CALL              = ' '
   IMPORTING
    FM_NAME                  = FM_NAME
  EXCEPTIONS
   NO_FORM                  = 1
   NO_FUNCTION_MODULE       = 2
   OTHERS                   = 3   .

*LOOP AT IT_FINAL1 INTO wa_final1 .


CALL FUNCTION FM_NAME" '/1BCDWB/SF00000369'
  EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
   CONTROL_PARAMETERS         = CONTROL
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
    S_FAPER                    = S_FAPER-LOW
*    ZPERNR                     = wa_final1-pernr
*    zename                     = wa_final1-ename
*    ZPLSTX                     = wa_final1-plstx
*    ZPERSK                     = wa_final1-persk
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
  TABLES
    IT_FINAL                   = IT_FINAL
    IT_FINAL1                  = IT_FINAL1
    IT_FINAL2                  = IT_FINAL2
    IT_FINAL3                  = IT_FINAL3
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
*
*ENDLOOP.














*  CALL FUNCTION FM_NAME"'/1BCDWB/SF00000367'
*    EXPORTING
**   ARCHIVE_INDEX              =
**   ARCHIVE_INDEX_TAB          =
**   ARCHIVE_PARAMETERS         =
*     CONTROL_PARAMETERS         = CONTROL
**   MAIL_APPL_OBJ              =
**   MAIL_RECIPIENT             =
**   MAIL_SENDER                =
**   OUTPUT_OPTIONS             =
*     USER_SETTINGS              = 'X'
*      IT_FINAL                   = IT_FINAL
*      IT_FINAL1                  = IT_FINAL1
*      IT_FINAL2                  = IT_FINAL2
*      IT_FINAL3                  = IT_FINAL3
*      S_FAPER                    = S_FAPER-LOW
** IMPORTING
**   DOCUMENT_OUTPUT_INFO       =
**   JOB_OUTPUT_INFO            =
**   JOB_OUTPUT_OPTIONS         =
*   EXCEPTIONS
*     FORMATTING_ERROR           = 1
*     INTERNAL_ERROR             = 2
*     SEND_ERROR                 = 3
*     USER_CANCELED              = 4
*     OTHERS                     = 5 .


*  CALL FUNCTION fm_name
*   EXPORTING
**   ARCHIVE_INDEX              =
**   ARCHIVE_INDEX_TAB          =
**   ARCHIVE_PARAMETERS         =
*    control_parameters         = control
**   MAIL_APPL_OBJ              =
**   MAIL_RECIPIENT             =
**   MAIL_SENDER                =
**   OUTPUT_OPTIONS             =
*    user_settings              = 'X'
*     it_final                   = it_final
*     it_final1                  = it_final1
*     it_final2                  = it_final2
*     it_final3                  = it_final3
** IMPORTING
**   DOCUMENT_OUTPUT_INFO       =
**   JOB_OUTPUT_INFO            =
**   JOB_OUTPUT_OPTIONS         =
* EXCEPTIONS
*   formatting_error           = 1
*   internal_error             = 2
*   send_error                 = 3
*   user_canceled              = 4
*   OTHERS                     = 5 .

ENDFORM.                    " PRINT_ARR
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE_VARIANT .
  G_SAVE = 'A'.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = REPNAME.
  G_VARIANT-VARIANT = P_VARI.
  GX_VARIANT = G_VARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = G_SAVE
    CHANGING
      CS_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    P_VARI = GX_VARIANT-VARIANT.
    G_VARIANT = GX_VARIANT.

  ENDIF.
  LAYOUT-GET_SELINFOS = 'X'.
  LAYOUT-GROUP_CHANGE_EDIT = 'X'.

  ALV_PRINT-NO_PRINT_SELINFOS  = 'X'.
  ALV_PRINT-NO_COVERPAGE       = 'X'.
  ALV_PRINT-NO_PRINT_LISTINFOS = 'X'.

ENDFORM.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EVENTS[]  text
*----------------------------------------------------------------------*
FORM BUILD_EVENTTAB  USING   P_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = P_EVENTS.
  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.
ENDFORM.                    " BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FOR_VARIANT .
  G_SAVE = 'A'.
  G_VARIANT-REPORT = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = G_VARIANT
      I_SAVE     = G_SAVE
    IMPORTING
      E_EXIT     = G_EXIT
      ES_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF G_EXIT = SPACE.
      P_VARI = GX_VARIANT-VARIANT.
    ENDIF.
    FLAG = 1.
  ENDIF.
ENDFORM.                    " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_HEADING[]  text
*----------------------------------------------------------------------*
FORM BUILD_COMMENT  USING    P_P_HEADING TYPE SLIS_T_LISTHEADER.
  DATA: S_VARIANT LIKE P_VAIRAINT.
  DATA: HLINE TYPE SLIS_LISTHEADER,
        TEXT(60) TYPE C,
        SEP(20) TYPE C,
        LV_1(6),
        LV_2(6),
        LV_3(6).

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = SY-LANGU
    TABLES
      MONTH_NAMES           = IT_MONTH_NAMES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.

  LV_1 = 'Apr-15'.
  LV_2 = S_FAPER-LOW+0(2). "072016
  LV_3 = S_FAPER-LOW+4(2).
  CLEAR WA_MONTH_NAMES.
  READ TABLE IT_MONTH_NAMES INTO WA_MONTH_NAMES WITH KEY MNR = LV_2.
  WA_MONTH_NAMES-LTX = WA_MONTH_NAMES-LTX+0(3).

  CONCATENATE WA_MONTH_NAMES-LTX '-' LV_3 INTO LV_2.

  CLEAR HLINE.
  HLINE-TYP  = 'H'.
  HLINE-INFO = 'Indofil Industries Ltd. '.
  APPEND HLINE TO P_HEADING.
  CLEAR TEXT.

  CONCATENATE 'Arrears Register for the Period' LV_1 'To' LV_2 INTO TEXT SEPARATED BY SPACE.
  HLINE-TYP  = 'S'.
  HLINE-INFO = TEXT.
  APPEND HLINE TO P_HEADING.
  CLEAR TEXT.
ENDFORM.                    " BUILD_COMMENT

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DATA : RS_VARIANT LIKE DISVARIANT,
         PLINE TYPE SLIS_LISTHEADER,
         V_LINES TYPE I.
  CLEAR RS_VARIANT.
  IMPORT RS_VARIANT FROM MEMORY ID 'VARIANT'.

  IF NOT RS_VARIANT-TEXT IS INITIAL.
    PLINE-TYP = 'S'.
    PLINE-INFO = RS_VARIANT-TEXT.
    APPEND PLINE TO P_HEADING.

  ENDIF.

  CALL FUNCTION 'Z6XX_REUSE_ALV_COMMENTARY_WR'
    EXPORTING
      IT_LIST_COMMENTARY = P_HEADING.

  IF NOT RS_VARIANT-TEXT IS INITIAL.
    DESCRIBE TABLE P_HEADING LINES V_LINES.
    DELETE P_HEADING INDEX V_LINES.

  ENDIF.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDTAB[]  text
*----------------------------------------------------------------------*
FORM INITIALIZE_FIELDCAT  USING P_FIELDTAB TYPE SLIS_T_FIELDCAT_ALV.
  DATA: FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        LV_POS TYPE I.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'PERNR'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Employee No'.
  FIELDCAT-OUTPUTLEN  = 12.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'ENAME'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Employee Name'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'BASIC'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Basic ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'DA'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'DA ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'HRA'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'HRA ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'TRANS'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Transport ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'EDUCT'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'EDU ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'OVERTIME'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Over Time ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'BONUS'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Bonus ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'LTA'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'LTA ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'MEDIC'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Medical ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'LV_ENC'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Leave EN ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'OTHERS'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'OTHER EARNINGS'."'Gratuity'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'TOTAL'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Gross TOT'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'PF_ARR'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'PF ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'VPF_ARR'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'VPF ARR'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'TAX'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'TDS'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'OTH_DEDCT'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Other DED'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'DEDCT'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Total DED'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.

  LV_POS = LV_POS + 1.
*  FIELDCAT-TABNAME    = 'IT_FINAL4'.
  FIELDCAT-FIELDNAME  = 'NET_TOT'.
  FIELDCAT-COL_POS    = LV_POS.
  FIELDCAT-SELTEXT_M  = 'Net Payable'.
  FIELDCAT-OUTPUTLEN  = 18.
  APPEND FIELDCAT TO P_FIELDTAB.
  CLEAR FIELDCAT.
ENDFORM.                    " INITIALIZE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_GOD .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = REPNAME
      IS_LAYOUT          = LAYOUT
      I_BUFFER_ACTIVE    = ' '
      IT_FIELDCAT        = FIELDTAB[]
      I_SAVE             = G_SAVE
      IS_VARIANT         = G_VARIANT
      IT_EVENTS          = EVENTS[]
      IS_PRINT           = ALV_PRINT
*       IT_SORT           = IT_SORT
    TABLES
      T_OUTTAB           = IT_FINAL5
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

ENDFORM.                    " DISPLAY_ALV_GOD
