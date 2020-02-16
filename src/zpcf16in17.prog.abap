*&-------------------------------------------------------------------*
*&  Include           PCF16IN17
*&-------------------------------------------------------------------*
*&-------------------------------------------------------------------*
*&      Form  CHK_SWITCH
*&-------------------------------------------------------------------*
*       Check if switch to TAN based reporting is enabled
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM CHK_SWITCH USING $PNPBEGDA $PNPENDDA CHANGING $CO_CD $TANR.

  CLEAR WA_T7INSW.

  SELECT SINGLE * INTO WA_T7INSW FROM T7INSW CLIENT SPECIFIED
                                WHERE MANDT = SY-MANDT
                                  AND MOLGA = '40'
                                  AND SWGRP = '40TX'
                                  AND SWNAM = 'TANK'
                                  AND ENDDA GE $PNPBEGDA
                                  AND BEGDA LE $PNPENDDA.

*  IF  WA_T7INSW IS INITIAL.
*    CLEAR TAN_NO.
*    $co_cd = 'X'.
*  ELSEIF ( ( wa_t7insw-begda > $pnpbegda
*              AND NOT ( $pnpbegda IS INITIAL ) )
*                              OR ( wa_t7insw-reval IS INITIAL ) ).
*    CLEAR TAN_NO.
*    $co_cd = 'X'.
*    MESSAGE S313(HRPADIN01).
*  ENDIF.
  IF PNPESSCF = 'X'.
    IF  WA_T7INSW-REVAL IS INITIAL.
      CLEAR TAN_NO.
      $TANR = ' '.
      $CO_CD = 'X'.
    ELSEIF WA_T7INSW-REVAL = 'X' .
*    CLEAR COMP_CD.
      SELECT SINGLE TANNO INTO TAN_NO FROM T7INTN WHERE SEQNR = COMP_CD.
      CLEAR COMP_CD.
      $TANR = 'X'.
      $CO_CD = ' '.

    ENDIF.
  ELSE.
    IF  WA_T7INSW-REVAL IS INITIAL.
      CLEAR TAN_NO.
      $TANR = ' '.
      $CO_CD = 'X'.
    ELSEIF WA_T7INSW-REVAL = 'X' .
      CLEAR COMP_CD.
      $TANR = 'X'.
      $CO_CD = ' '.
    ENDIF.
  ENDIF.
  IF $TANR = 'X' AND $CO_CD = 'X'.
    CLEAR $CO_CD.
  ENDIF.

  PERFORM MODIFY_SCREEN USING $TANR $CO_CD.

ENDFORM.                    " CHK_SWITCH

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TANR     text
*      -->P_CO_CD    text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN USING P_TANR P_CO_CD.
  IF P_TANR = 'X'.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LO1'.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LO2'.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LO3'.
    PERFORM CHANGE_SCREEN_LOT USING OFF OFF 'LO4'.
*      clear co_cd.
    CLEAR COMP_CD.
    IF PNPESSCF = ' '.
      MESSAGE S318(HRPADIN01).
    ENDIF.
  ELSEIF P_CO_CD = 'X'.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LO1'.
    PERFORM CHANGE_SCREEN_LOT USING OFF OFF 'LO2'.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LO3'.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LO4'.
    IF PNPESSCF = ' '.
      MESSAGE S313(HRPADIN01).
    ENDIF.
  ENDIF.
ENDFORM.                    "MODIFY_SCREEN

*&-------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN_LOT
*&-------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------
*      -->P_OFF  text
*      -->P_ON  text
*      -->P_0097   text
*--------------------------------------------------------------------*
FORM CHANGE_SCREEN_LOT  USING    $OFF
                                 $ON
                                 $PAR.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ $PAR.
      SCREEN-INVISIBLE = $OFF.
      SCREEN-INPUT = $ON.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHANGE_SCREEN_LOT
*&-------------------------------------------------------------------*
*&      Form  GET_FISCAL
*&-------------------------------------------------------------------*
*       Get start and end date of Fiscal Year
*--------------------------------------------------------------------*
*      -->P_YEAR  text
*      <--P_PBEGDA  text
*      <--P_PENDDA  text
*--------------------------------------------------------------------
FORM GET_FISCAL  USING    $YEAR
                          P_AFY_SWITCH
                 CHANGING $PBEGDA
                          $PENDDA.


  IF P_AFY_SWITCH IS INITIAL.
    $PBEGDA+0(4) = $YEAR.
    $PBEGDA+4(4) = '0401'.
    $PENDDA+0(4) = $YEAR + 1.
    $PENDDA+4(4) = '0331'.
  ELSE.
    $PBEGDA+0(4) = $YEAR.
    $PBEGDA+4(4) = '0301'.
    $PENDDA      = $PBEGDA.
    $PENDDA      = $PENDDA - 1.
    $PENDDA+0(4) = $PENDDA+0(4) + 1.
  ENDIF.

ENDFORM.                    " GET_FISCAL
*&-------------------------------------------------------------------*
*&      Form  CUST_LAYOUT
*&-------------------------------------------------------------------*
*       Modify selection screen for customer or standard layout
*--------------------------------------------------------------------*
*      -->P_DISP_FLG_LOT  tex
*--------------------------------------------------------------------*
FORM CUST_LAYOUT  USING    P_DISP_FLG_LOT.

  IF P_DISP_FLG_LOT GE 1.
    MOVE 'Customer Layout'(005) TO LOUT.
    PERFORM CHANGE_SCREEN_LOT USING ON OFF 'LOT'.
  ELSE.
    MOVE 'Standard Layout'(006) TO LOUT.
    PERFORM CHANGE_SCREEN_LOT USING OFF ON 'LOT'.
  ENDIF.

ENDFORM.                    " CUST_LAYOUT

*&-------------------------------------------------------------------*
*&      Form  CHK_AFY
*&-------------------------------------------------------------------*
*       Check if Alternate Financial Year solution is implemented
*--------------------------------------------------------------------*
*      -->P_PBEGDA  text
*      -->P_PENDDA  text
*      <--P_AFY_SWITCH  text
*--------------------------------------------------------------------*
FORM CHK_AFY  USING    P_PBEGDA
                            P_PENDDA
                   CHANGING P_AFY_SWITCH.

  SELECT SINGLE * FROM T54C0 INTO G_T54C0 WHERE MOLGA = '40'.
  CLEAR AFY_SWITCH.
  CALL METHOD CL_HRPAYIN_SWITCH_CHECK_5=>HRLOCIN_SFWS_SC_05
    RECEIVING
      RV_ACTIVE = HTEXT_CSWITCH.
  IF HTEXT_CSWITCH = 'X'.
*   Check if Alternate Financial Year solution is implemented
    PERFORM IS_AFY_IMPLEMENTED IN PROGRAM PCUTSIN0
                               IF FOUND
                               USING PBEGDA PENDDA
                               CHANGING P_AFY_SWITCH.
  ENDIF.

ENDFORM.                    " CHK_AFY_CRUN
*&-------------------------------------------------------------------*
*&      Form  CHK_MUL_F16
*&-------------------------------------------------------------------*
*       Check if Multiple Form 16 is Enabled.
*--------------------------------------------------------------------*
*      -->P_PBEGDA  text
*      <--P_MULTIPLE_F16  text
*--------------------------------------------------------------------*
FORM CHK_MUL_F16  USING    P_PBEGDA
                  CHANGING P_MULTIPLE_F16.

  CLEAR: WA_T7INSW,
         P_MULTIPLE_F16.
  CALL FUNCTION 'HR_IN_CHK_MULTI_F16'
   EXPORTING
     RUNDATE             = P_PBEGDA
   IMPORTING
     MULTIPLE_F16        = P_MULTIPLE_F16
*     MF16_START_DT       =
            .

ENDFORM.                    " CHK_MUL_F16

*&-------------------------------------------------------------------*
*&      Form  GET_TMP_COCD
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_TMP_COCD  text
*      -->P_COCD  text
*      -->P_PERNR_PERNR  text
*      -->P_MULTIPLE_F16  text
*      -->P_PBEGDA  text
*      -->P_PENDDA  text
*--------------------------------------------------------------------*
FORM GET_TMP_COCD  TABLES   P_TMP_COCD STRUCTURE TMP_COCD
                            P_P0000  STRUCTURE P0000
                            P_P0001  STRUCTURE P0001
                            P_P0185  STRUCTURE P0185
                            P_P0580 STRUCTURE P0580
                     USING  P_TAN_NO
                            P_PERNR-PERNR
                            P_MULTIPLE_F16
                            P_PBEGDA
                            P_PENDDA
                            P_COMP_CD
                            P_TANR
                   CHANGING P_COCD_LINES
                            P_F16_BEGDA.
*                            P_F16_ENDDA.

  DATA: TMP_T7INTN TYPE TABLE OF T7INTN WITH HEADER LINE,
        WA_T7INTN LIKE T7INTN.
  DATA: WA_TMP_COCD LIKE LINE OF TMP_COCD.

  CLEAR: P_TMP_COCD,OLD_COCD,INDX2.
  SORT P_TMP_COCD BY ENDDA.
  LOOP AT P_TMP_COCD.
    IF P_TMP_COCD-BUKRS  NE OLD_COCD-BUKRS OR
       P_TMP_COCD-CNTR2  NE OLD_COCD-CNTR2.
      OLD_COCD = P_TMP_COCD.
      CONTINUE.
    ENDIF.
    P_TMP_COCD-BEGDA = OLD_COCD-BEGDA.
    P_TMP_COCD-CNTR2 = OLD_COCD-CNTR2.
    P_TMP_COCD-STAT2 = OLD_COCD-STAT2.
    INDX2 = SY-TABIX - 1.
    MODIFY P_TMP_COCD.
    DELETE P_TMP_COCD INDEX INDX2.
  ENDLOOP.

  SORT P_TMP_COCD BY BEGDA ASCENDING.
  DESCRIBE TABLE P_TMP_COCD LINES P_COCD_LINES.

  LOOP AT P_TMP_COCD INTO WA_TMP_COCD.
    IF SY-TABIX = 1.
      P_F16_BEGDA = WA_TMP_COCD-BEGDA.
*    ELSEIF SY-TABIX = P_COCD_LINES.
*      P_F16_ENDDA = WA_TMP_COCD-ENDDA.
    ENDIF.
  ENDLOOP.

  IF P_MULTIPLE_F16 = 'X' AND P_TANR = ' '.
    DELETE P_TMP_COCD WHERE BUKRS <> P_COMP_CD.
  ELSEIF P_MULTIPLE_F16 = 'X' AND P_TANR = 'X'.
    SELECT SINGLE * FROM T7INTN INTO CORRESPONDING FIELDS OF WA_T7INTN
       WHERE TANNO EQ P_TAN_NO.
    DELETE P_TMP_COCD WHERE BUKRS <> WA_T7INTN-SEQNR.
  ENDIF.
  RP-PROVIDE-FROM-LAST P_P0001 SPACE P_PBEGDA P_PENDDA.
  IF P_MULTIPLE_F16 = ' ' AND P_TANR = ' '.
    IF P_P0001-BUKRS <> P_COMP_CD.
      REJECT.
    ENDIF.
  ELSEIF P_TANR = 'X'.
    IF NOT ( P_TAN_NO IS INITIAL ).
      IF P_MULTIPLE_F16 = ' '.
        LOOP AT P_P0185 WHERE BEGDA <= P_PENDDA
                          AND ENDDA >= P_PBEGDA.
        ENDLOOP.
        IF NOT ( P_P0185-ICNUM EQ P_TAN_NO ).
          REJECT.
        ENDIF.
      ELSE.
        READ TABLE P_P0185 WITH KEY ICNUM = P_TAN_NO.
        IF SY-SUBRC NE 0.
          REJECT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    " GET_TMP_COCD

*&---------------------------------------------------------------------*
*&      Form  GET_F16_ENDDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P0000  text
*      -->P_P0001  text
*      -->P_TMP_COCD  text
*      <--P_F16_ENDDA  text
*----------------------------------------------------------------------*
FORM GET_F16_ENDDA  TABLES   P_P0000 STRUCTURE P0000
                             P_P0001 STRUCTURE P0001
                       USING P_WA_TMP_COCD-BEGDA
                             P_WA_TMP_COCD-ENDDA
                             P_PBEGDA
                             P_PENDDA
                    CHANGING P_F16_ENDDA.

  DATA: PPHIFI_TAB LIKE TABLE OF PHIFI WITH HEADER LINE.

  CALL FUNCTION 'RP_HIRE_FIRE'
   EXPORTING
     BEG             = P_PBEGDA
     END             = P_PENDDA
*   IMPORTING
*     FIRE_DATE       =
*     HIRE_DATE       =
    TABLES
      PPHIFI          = PPHIFI_TAB
      PP0000          = P_P0000
      PP0001          = P_P0001
            .

  P_F16_ENDDA = P_WA_TMP_COCD-ENDDA.
  CLEAR PPHIFI_TAB.
  LOOP AT PPHIFI_TAB WHERE ENDDA BETWEEN P_WA_TMP_COCD-BEGDA
                                     AND P_WA_TMP_COCD-ENDDA
                       AND STAT2 = '3'.
*                           AND FIRES = 'X'.
    IF  PPHIFI_TAB-ENDDA IS NOT INITIAL.
*          AND P_WA_TMP_COCD_CNTR2 NE P_COCD_LINES.
      P_F16_ENDDA = PPHIFI_TAB-ENDDA.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_F16_ENDDA
*&-------------------------------------------------------------------*
*&      Form  READ_INFOTYPES
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM READ_INFOTYPES .

  RP-PROVIDE-FROM-LAST P0001 SPACE PNPBEGDA PNPENDDA.

  RP-PROVIDE-FROM-LAST P0002 SPACE PBEGDA PENDDA.
  IF PNP-SW-FOUND EQ 0 AND PNPESSCF = ' '.
    MESSAGE S089(HRPADIN01) WITH '0002' PERNR-PERNR PBEGDA PENDDA.
*   There is no infotype & for personnel no & from period & to &
*     PERFORM BUILD_ERROR TABLES HR_ERROR
*                        USING SPACE SY-MSGID SY-MSGNO
*                        SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
    REJECT.
  ENDIF.

ENDFORM.                    " READ_INFOTYPES
*&-------------------------------------------------------------------*
*&      Form  GET_RGDIR
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_1532   text
*      -->P_RGDIR  text
*--------------------------------------------------------------------*
FORM GET_RGDIR  TABLES   P_RGDIR STRUCTURE PC261
                USING    P_PERNR-PERNR.
  "Insert correct name for <...>.

  CD-KEY = P_PERNR-PERNR.
  RP-IMP-C2-CU.

ENDFORM.                    " GET_RGDIR

*&-------------------------------------------------------------------*
*&      Form  GET_PAYRESULT
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_RT           text
*      -->P_RGDIR        text
*      -->P_PERNR-PERNR  text
*--------------------------------------------------------------------*
FORM GET_PAYRESULT TABLES P_RT_TAB    TYPE PC207_TAB
                          P_S80       TYPE HRPAYIN_S80
                          P_S88       TYPE HRPAYIN_S88
                   USING  P_VERSC
                          P_SEQNR
                          P_PERNR-PERNR.

"$$
"$$

  RX-KEY-SEQNO = P_SEQNR.
  RX-KEY-PERNR = P_PERNR-PERNR.

  RP-IMP-C2-IN.
  P_RT_TAB[] = RT[].
  P_VERSC = VERSC.
  P_S80[] = S80[].
  P_S88[] = S88[].

*  APPEND WA_PAY_RESULT TO P_PAY_RESULT.
*  CLEAR WA_PAY_RESULT.

ENDFORM.                    "GET_PAYRESULT
*&-------------------------------------------------------------------*
*&      Form  POP_MON_TAX
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_MON_TAX  text
*      -->P_RGDIR  text
*      -->P_WA_TMP_COCD  text
*--------------------------------------------------------------------*
FORM POP_MON_TAX  TABLES   P_MON_TAX LIKE MON_TAX_COMP
                           P_RGDIR
                           P_PAY_RESULT LIKE PAY_RESULT

                  USING    P_WA_TMP_COCD STRUCTURE TMP_COCD
                           P_PERNR TYPE P_PERNR
                CHANGING   P_LAST_RESULT LIKE LAST_RESULT.


  DATA: TMP_RGDIR TYPE TABLE OF PC261,
        WA_RGDIR TYPE PC261,
        MON_TAX_COMP TYPE TABLE OF MON_TAX,
        WA_TAX_COMP TYPE MON_TAX,
        RESULT TYPE TABLE OF PC207,
        WA_RESULT TYPE PC207.

  DATA: WA_T549Q LIKE T549Q,
        PERMO    LIKE T549Q-PERMO.
  DATA: TMP_PAY_RESULT LIKE P_PAY_RESULT OCCURS 0 WITH HEADER LINE.
  DATA: TMP_PBEGDA TYPE SY-DATUM,
        TMP_PENDDA TYPE SY-DATUM.

  DATA: TMP_PAYRESULT LIKE P_PAY_RESULT OCCURS 0,
        WA_TMP_PAYRESULT  LIKE LINE OF P_PAY_RESULT.

  DATA: TMP_MSG1 TYPE STRING,
        TMP_MSG2 TYPE STRING.
  DATA: PANCHK TYPE C.
  CLEAR: P_LAST_RESULT, WA_PAY_RESULT.

  SORT P_PAY_RESULT ASCENDING BY VERSC-FPBEG VERSC-FPEND VERSC-IPEND.
  LOOP AT P_PAY_RESULT INTO WA_PAY_RESULT WHERE VERSC-FPBEG <= P_WA_TMP_COCD-ENDDA
                                           AND  VERSC-FPEND >= P_WA_TMP_COCD-BEGDA.

    IF WA_PAY_RESULT-VERSC-PAYTY = ' '.
      P_LAST_RESULT = WA_PAY_RESULT.
    ENDIF.
    MOVE-CORRESPONDING WA_PAY_RESULT TO WA_TMP_PAYRESULT.
    APPEND WA_TMP_PAYRESULT TO TMP_PAY_RESULT.
  ENDLOOP.

  CLEAR: WA_PAY_RESULT.
  SORT TMP_PAY_RESULT DESCENDING BY VERSC-FPBEG VERSC-FPEND
                                                VERSC-PAYTY VERSC-PAYID.
  DELETE ADJACENT DUPLICATES FROM TMP_PAY_RESULT
                  COMPARING VERSC-FPBEG VERSC-FPEND VERSC-PAYTY VERSC-PAYID.
  SORT TMP_PAY_RESULT BY VERSC-FPBEG VERSC-FPEND VERSC-PAYTY VERSC-PAYID.

  LOOP AT TMP_PAY_RESULT.

    IF TMP_PAY_RESULT-VERSC-PAYTY = 'A'
      AND TMP_PAY_RESULT-VERSC-PAYDT > P_LAST_RESULT-VERSC-PAYDT.
      CLEAR WA_TAX_COMP.

      CONCATENATE TEXT-600
                   P_WA_TMP_COCD-BEGDA
                   INTO TMP_MSG1 SEPARATED BY SPACE.
      CONCATENATE P_WA_TMP_COCD-ENDDA
                 TEXT-601
                  P_PERNR INTO TMP_MSG2 SEPARATED BY SPACE.

      MESSAGE S201(HRPADIN01) WITH TMP_MSG1 TMP_MSG2.

      PERFORM BUILD_ERROR TABLES HR_ERROR
                      USING SPACE SY-MSGID SY-MSGNO
                      SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
      CONTINUE.
    ENDIF.

    WA_TAX_COMP-CNTR2 = P_WA_TMP_COCD-CNTR2.
    WA_TAX_COMP-PERNR = P_PERNR.
    WA_TAX_COMP-PAYTY = TMP_PAY_RESULT-VERSC-PAYTY.
    WA_TAX_COMP-PAYID = TMP_PAY_RESULT-VERSC-PAYID.
    WA_TAX_COMP-PAYDT = TMP_PAY_RESULT-VERSC-PAYDT.
    WA_TAX_COMP-FPPER = TMP_PAY_RESULT-VERSC-FPPER.
    WA_TAX_COMP-FPBEG = TMP_PAY_RESULT-VERSC-FPBEG.
    WA_TAX_COMP-FPEND = TMP_PAY_RESULT-VERSC-FPEND.
    WA_TAX_COMP-INPER = TMP_PAY_RESULT-VERSC-INPER.
    WA_TAX_COMP-IPEND = TMP_PAY_RESULT-VERSC-IPEND.

    PERFORM GET_MONTH_BEGDA_ENDDA USING WA_TAX_COMP-FPBEG
                                        TMP_PAY_RESULT-VERSC-ABKRS
                               CHANGING WA_TAX_COMP-PBEGDA
                                        WA_TAX_COMP-PENDDA.

    READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                               WITH KEY LGART = '/460'.
    IF SY-SUBRC = 0 .
      WA_TAX_COMP-TDEDT = WA_RESULT-BETRG.
    ELSE.
      WA_TAX_COMP-TDEDT = 0.
    ENDIF.

    READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                                  WITH KEY LGART = '/462'.
    IF SY-SUBRC = 0 .
      WA_TAX_COMP-TDEDT = WA_TAX_COMP-TDEDT + WA_RESULT-BETRG.
    ELSE.
      WA_TAX_COMP-TDEDT = WA_TAX_COMP-TDEDT.
    ENDIF.
    IF NOT WA_TAX_COMP-TDEDT IS INITIAL.
      CLEAR WA_RESULT.
      READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                                      WITH KEY LGART = '/4ME'.
      IF SY-SUBRC = 0.
        WA_TAX_COMP-MECSS = WA_RESULT-BETRG.
      ELSE.
        WA_TAX_COMP-MECSS = 0.
      ENDIF.

      CLEAR WA_RESULT.
      READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                                      WITH KEY LGART = '/4MH'.
      IF SY-SUBRC = 0.
        WA_TAX_COMP-MHECSS = WA_RESULT-BETRG.
      ELSE.
        WA_TAX_COMP-MHECSS = 0.
      ENDIF.

      CLEAR WA_RESULT.
      READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                                      WITH KEY LGART = '/4MT'.
      IF SY-SUBRC = 0.
        WA_TAX_COMP-MTPAY = WA_RESULT-BETRG.
      ELSE.
        WA_TAX_COMP-MTPAY = 0.
      ENDIF.

      CLEAR WA_RESULT.
      READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                                      WITH KEY LGART = '/4MI'.
      IF SY-SUBRC = 0.
        WA_TAX_COMP-MTINC = WA_RESULT-BETRG.
      ELSE.
        WA_TAX_COMP-MTINC = 0.
      ENDIF.

      CLEAR WA_RESULT.
      READ TABLE TMP_PAY_RESULT-RT_TAB INTO WA_RESULT
                                      WITH KEY LGART = '/4MS'.
      IF SY-SUBRC = 0.
        WA_TAX_COMP-MTSUR = WA_RESULT-BETRG.
      ELSE.
        WA_TAX_COMP-MTSUR = 0.
      ENDIF.

    ENDIF.

    DATA: BD_TAX TYPE REF TO HR_IN_F24Q_TAX_CHECK,
 RESULT_FLAG(1),
 FLAG.
    CLEAR FLAG.
    TRY.
        GET BADI BD_TAX
          FILTERS
            FLT_VAL = '40'.

        CALL BADI BD_TAX->CHK_F24Q
          EXPORTING
            EMPNO         = P_PERNR
            BEGDA         = TMP_PAY_RESULT-VERSC-FPBEG
            ENDDA         = TMP_PAY_RESULT-VERSC-FPEND
            RGDIR_TABLE   = P_RGDIR[]
            RESULTS_TABLE = TMP_PAY_RESULT-RT_TAB[]
            F16_TABLE     = F16[]
            F16_CNTR2     = P_WA_TMP_COCD-CNTR2
            FLT_VAL       = '40'
            PANCHK_OLD    = PANCHK
          IMPORTING
            RESULT        = RESULT_FLAG.
        FLAG = 'X'.

      CATCH CX_BADI_NOT_IMPLEMENTED.
    ENDTRY.
    IF RESULT_FLAG <> ' ' AND FLAG = 'X' AND NOT WA_TAX_COMP IS INITIAL.
      APPEND WA_TAX_COMP TO P_MON_TAX.
    ELSEIF FLAG NE 'X' AND NOT WA_TAX_COMP IS INITIAL.
      APPEND WA_TAX_COMP TO P_MON_TAX.
    ENDIF.
    CLEAR WA_TAX_COMP.

  ENDLOOP.

ENDFORM.                   " POP_MON_TAX

*&--------------------------------------------------------------------
*&      Form  FILL_MAIN_TABLE
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_RESULT_TAB  text
*      -->P_MAIN_TAB  text
*--------------------------------------------------------------------*
FORM FILL_MAIN_TABLE  TABLES   P_MAIN_TAB STRUCTURE MAIN_TAB
                       USING   P_LAST_RESULT TYPE T_PAY_RESULT-RT_TAB.

  DATA: WA_RT_TAB TYPE PC207.

  CLEAR MAIN_TAB.
  CLEAR EMPLOYER_TAX.

  LOOP AT P_LAST_RESULT INTO WA_RT_TAB.
    CASE WA_RT_TAB-LGART.
      WHEN GROSS_SAL.
        P_MAIN_TAB-GROSS_SAL = P_MAIN_TAB-GROSS_SAL +
                               WA_RT_TAB-BETRG.
      WHEN SEC10_ALL.
        P_MAIN_TAB-SEC10_ALL = P_MAIN_TAB-SEC10_ALL +
                               WA_RT_TAB-BETRG.
      WHEN BALANCE.
        P_MAIN_TAB-BALANCE = P_MAIN_TAB-BALANCE +
                             WA_RT_TAB-BETRG.
      WHEN STD_DED.
        P_MAIN_TAB-STD_DED = P_MAIN_TAB-STD_DED +
                             WA_RT_TAB-BETRG.
      WHEN PTAX.
        P_MAIN_TAB-PTAX = P_MAIN_TAB-PTAX +
                          WA_RT_TAB-BETRG.
      WHEN AGGR_DED.
        P_MAIN_TAB-AGGR_DED = P_MAIN_TAB-AGGR_DED +
                              WA_RT_TAB-BETRG.
      WHEN SALARIES.
        P_MAIN_TAB-SALARIES = P_MAIN_TAB-SALARIES + WA_RT_TAB-BETRG.
      WHEN OTH_INCOME.
        P_MAIN_TAB-OTH_INCOME = P_MAIN_TAB-OTH_INCOME +
                                WA_RT_TAB-BETRG.
      WHEN BUS_PROF OR GAIN_LONGN OR GAIN_LONGS OR GAIN_SHORT OR
            DVDND OR INTST OR OTH_IN.
        P_MAIN_TAB-BUS_PROF = P_MAIN_TAB-BUS_PROF + WA_RT_TAB-BETRG.

      WHEN DEDN_INTEREST_S24 OR DEDN_REPAIR_S24 OR DEDN_OTHERS_S24
           OR TOT_LETTABLE_VAL.
        P_MAIN_TAB-DEDN_S24 =  P_MAIN_TAB-DEDN_S24 + WA_RT_TAB-BETRG.
      WHEN GROSS_TOT_INCOME.
        P_MAIN_TAB-GROSS_TOT_INCOME = P_MAIN_TAB-GROSS_TOT_INCOME
                                           + WA_RT_TAB-BETRG.
      WHEN SEC80_DED.
        P_MAIN_TAB-SEC80_DED = P_MAIN_TAB-SEC80_DED +
                               WA_RT_TAB-BETRG.
      WHEN TOT_INCOME.
        P_MAIN_TAB-TOT_INCOME = P_MAIN_TAB-TOT_INCOME +
                                WA_RT_TAB-BETRG.
      WHEN TAX_TOT_INCOME.
        P_MAIN_TAB-TAX_TOT_INCOME = P_MAIN_TAB-TAX_TOT_INCOME +
                                           WA_RT_TAB-BETRG.
      WHEN SEC88_DED.
        P_MAIN_TAB-SEC88_DED = P_MAIN_TAB-SEC88_DED +
                               WA_RT_TAB-BETRG.
      WHEN SEC88B_DED.
        P_MAIN_TAB-SEC88B_DED = P_MAIN_TAB-SEC88B_DED +
                                WA_RT_TAB-BETRG.
      WHEN SEC88C_DED.
        P_MAIN_TAB-SEC88C_DED = P_MAIN_TAB-SEC88C_DED +
                                WA_RT_TAB-BETRG.
      WHEN SEC88D_DED.
        P_MAIN_TAB-SEC88D_DED = P_MAIN_TAB-SEC88D_DED +
                                WA_RT_TAB-BETRG.
      WHEN EPF_TOT.
        P_MAIN_TAB-EPF_TOT = P_MAIN_TAB-EPF_TOT + WA_RT_TAB-BETRG.
      WHEN PYR_EPF.
        IF WA_RT_TAB-BETRG > 0.
          P_MAIN_TAB-EPF_TOT = P_MAIN_TAB-EPF_TOT + WA_RT_TAB-BETRG.
        ENDIF.
      WHEN CHAPVI_DED..
        P_MAIN_TAB-CHAPVI_DED = P_MAIN_TAB-CHAPVI_DED +
                                WA_RT_TAB-BETRG.
*       WHEN TAX_PAYABLE.
*         P_main_tab-TAX_PAYABLE = P_main_tab-TAX_PAYABLE +
*                                         P_RESULT_TAB-BETRG.
      WHEN TAX_PAYABLE.
        P_MAIN_TAB-TAX_PAYABLE_BEFORE_RELIEF
                     = P_MAIN_TAB-TAX_PAYABLE_BEFORE_RELIEF
                       + WA_RT_TAB-BETRG.
      WHEN SURCHG_AMT.
        P_MAIN_TAB-SURCHG = P_MAIN_TAB-SURCHG + WA_RT_TAB-BETRG.
      WHEN SEC_CESS.
        P_MAIN_TAB-EDU_CESS =  P_MAIN_TAB-EDU_CESS +
                               WA_RT_TAB-BETRG.
      WHEN EDU_CESS.
        P_MAIN_TAB-EDU_CESS =  P_MAIN_TAB-EDU_CESS +
                               WA_RT_TAB-BETRG.
      WHEN SEC89_RELIEF.
        P_MAIN_TAB-SEC89_RELIEF = P_MAIN_TAB-SEC89_RELIEF +
                                WA_RT_TAB-BETRG.
      WHEN TAX_DED_SO_FAR.
        P_MAIN_TAB-TAX_DED_SO_FAR = P_MAIN_TAB-TAX_DED_SO_FAR +
                                          WA_RT_TAB-BETRG.
      WHEN TAX_THIS_MONTH.
        P_MAIN_TAB-TAX_THIS_MONTH = P_MAIN_TAB-TAX_THIS_MONTH +
                                           WA_RT_TAB-BETRG.
      WHEN TDS_PETD.
        P_MAIN_TAB-TDS_PETD = P_MAIN_TAB-TDS_PETD + WA_RT_TAB-BETRG.
      WHEN TDS_PETD_SAP.
        P_MAIN_TAB-TDS_PETD = P_MAIN_TAB-TDS_PETD + WA_RT_TAB-BETRG.
      WHEN TDS_IFOS.
        P_MAIN_TAB-TDS_IFOS = P_MAIN_TAB-TDS_IFOS + WA_RT_TAB-BETRG.
      WHEN PET_S172.
        P_MAIN_TAB-PETD_S172 = P_MAIN_TAB-PETD_S172 +
                               WA_RT_TAB-BETRG.
      WHEN PET_S173.
        P_MAIN_TAB-PETD_S173 = P_MAIN_TAB-PETD_S173 +
                               WA_RT_TAB-BETRG.
    ENDCASE.
  ENDLOOP.

*   PERFORM READ_CRT USING VOL_TAX 'Y'
*                    CHANGING P_main_tab-VOL_TAX.
  READ TABLE F16 WITH KEY CNTR2 = F16_CNTR2
               LGART = VOL_TAX
               CUMTY = 'Y'.
  IF SY-SUBRC = 0.
    P_MAIN_TAB-VOL_TAX = F16-BETRG.
  ENDIF.


* Calculate total tax deducted = tax deducted so far(/456) +
*                                tax this month(/460) +
*                                voluntary tax(/462)
*   P_main_tab-CHAPVI_DED = P_main_tab-CHAPVI_DED
* P_main_tab-SEC89_RELIEF.

  P_MAIN_TAB-TAX_PAYABLE = P_MAIN_TAB-TAX_PAYABLE_BEFORE_RELIEF
                         - P_MAIN_TAB-SEC89_RELIEF.


  CALL FUNCTION 'HR_IN_ROUND_AMT'
    EXPORTING
      AMOUNT = P_MAIN_TAB-TAX_PAYABLE
      RNDOFF = 100
      RNDLMT = 'N'
    IMPORTING
      RETAMT = P_MAIN_TAB-TAX_PAYABLE.
*RSKNT615316

* BADI to return the tax paid by the employer on behalf of
* the employee.

  DATA: CUST_EXIT TYPE REF TO IF_EX_HR_IN_TAX_EMPLOYER.

  CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
    EXPORTING
      EXIT_NAME              = 'HR_IN_TAX_EMPLOYER'
      NULL_INSTANCE_ACCEPTED = ' '
    CHANGING
      INSTANCE               = CUST_EXIT.

  CALL METHOD CUST_EXIT->GET_EMPLOYER_TAX
    EXPORTING
      EMPNO         = PERNR-PERNR
      RESULTS_TABLE = RT[]
      F16_TABLE     = F16[]
      F16_CNTR2     = F16_CNTR2
      FLT_VAL       = '40'
    IMPORTING
      EMPLOYER_TAX  = EMPLOYER_TAX.


  P_MAIN_TAB-TOT_TAX_DEDUCTED = P_MAIN_TAB-TAX_DED_SO_FAR +
                              P_MAIN_TAB-TAX_THIS_MONTH +
                              P_MAIN_TAB-VOL_TAX.

  P_MAIN_TAB-TAX_DEDUCTED   =   P_MAIN_TAB-TAX_DED_SO_FAR +
                              P_MAIN_TAB-TAX_THIS_MONTH +
                              P_MAIN_TAB-VOL_TAX - EMPLOYER_TAX.

  P_MAIN_TAB-TAX_PAID_EMPLOYER = EMPLOYER_TAX.

  P_MAIN_TAB-NET_TAX_PAYABLE = P_MAIN_TAB-TAX_PAYABLE -
                             P_MAIN_TAB-TOT_TAX_DEDUCTED.

  MOVE F16_CNTR2 TO P_MAIN_TAB-CNTR1.
  APPEND P_MAIN_TAB.

ENDFORM.                    " FILL_P_main_tabLE

*&-------------------------------------------------------------------*
*&      Form  FILL_FINAL_TA
*&-------------------------------------------------------------------*
FORM FILL_FINAL_TAB.
  CLEAR FINAL_TAB.
*   MOVE-CORRESPONDING HD_TAB TO FINAL_TAB.
  MOVE-CORRESPONDING MAIN_TAB TO FINAL_TAB.
  APPEND FINAL_TAB.
ENDFORM.                              " FILL_FINAL_TAB
*&-------------------------------------------------------------------*
*&      Form  FILL_FINAL_TABLE
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_MAIN_TAB  text
*      -->P_FINAL_TAB  text
*--------------------------------------------------------------------*
FORM FILL_FINAL_TABLE  TABLES   P_HD_TAB   STRUCTURE HD_TAB
                                P_MAIN_TAB STRUCTURE MAIN_TAB
                                  "Insert correct name for <...>
                                P_FINAL_TAB STRUCTURE FINAL_TAB
                       USING    P_PERNR_PERNR.
  CLEAR FINAL_TAB.
  MOVE-CORRESPONDING HD_TAB TO FINAL_TAB.
*  FINAL_TAB-PERNR = P_PERNR_PERNR.
  MOVE-CORRESPONDING MAIN_TAB TO FINAL_TAB.
  APPEND FINAL_TAB.
ENDFORM.                    " FILL_FINAL_TABLE
*&-------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->P_INAL_TAB  text
*--------------------------------------------------------------------*
FORM DISPLAY_ALV  TABLES   P_FINAL_TAB STRUCTURE FINAL_TAB.
  "Insert correct name for <...>.

  TYPE-POOLS: SLIS.
  DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
        BEGIN OF G_ITAB_FCODE1 OCCURS 10,
         FCODE LIKE RSMPE-FUNC,
        END   OF G_ITAB_FCODE1.

  LOOP AT FINAL_TAB.
    MOVE-CORRESPONDING FINAL_TAB TO DISP_TAB.
    APPEND DISP_TAB.
    CLEAR FINAL_TAB.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = 'HINCF160'
      I_INTERNAL_TABNAME     = 'DISP_TAB'
      I_INCLNAME             = 'PCF16IN12'
    CHANGING
      CT_FIELDCAT            = FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'F16_BEGDA'.
  FIELDCAT-FIELDNAME = 'F16_BEGDA'.
  FIELDCAT-SELTEXT_M = 'From Date'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.


  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'F16_ENDDA'.
  FIELDCAT-FIELDNAME = 'F16_ENDDA'.
  FIELDCAT-SELTEXT_M = 'End Date'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'PERNR'.
  FIELDCAT-FIELDNAME = 'PERNR'.
  FIELDCAT-SELTEXT_M = 'Employee No.'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 13.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'ENAME'.
  FIELDCAT-FIELDNAME = 'ENAME'.
  FIELDCAT-SELTEXT_M = 'Employee Name'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 14.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'POSITION'.
  FIELDCAT-FIELDNAME = 'POSITION'.
  FIELDCAT-SELTEXT_M = 'Employee Designation'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 20.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'ICNUM'.
  FIELDCAT-FIELDNAME = 'ICNUM'.
  FIELDCAT-SELTEXT_M = 'PAN Number'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'PANNO'.
  FIELDCAT-FIELDNAME = 'PANNO'.
  FIELDCAT-SELTEXT_M = 'Employer PAN Number'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TANNO'.
  FIELDCAT-FIELDNAME = 'TANNO'.
  FIELDCAT-SELTEXT_M = 'TAN Number'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'GIRNO'.
  FIELDCAT-FIELDNAME = 'GIRNO'.
  FIELDCAT-SELTEXT_M = 'GIR Number'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'GROSS_SAL'.
  FIELDCAT-FIELDNAME = 'GROSS_SAL'.
  FIELDCAT-SELTEXT_M = 'Gross Salary'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'SEC10_ALL'.
  FIELDCAT-FIELDNAME = 'SEC10_ALL'.
  FIELDCAT-SELTEXT_M = 'Section 10 Allowance'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'BALANCE'.
  FIELDCAT-FIELDNAME = 'BALANCE'.
  FIELDCAT-SELTEXT_M = 'Balance'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'SALARIES'.
  FIELDCAT-FIELDNAME = 'SALARIES'.
  FIELDCAT-SELTEXT_M = 'Head Salaries'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'OTH_INCOME'.
  FIELDCAT-FIELDNAME = 'OTH_INCOME'.
  FIELDCAT-SELTEXT_M = 'Other Income'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'GROSS_TOT_INCOME'.
  FIELDCAT-FIELDNAME = 'GROSS_TOT_INCOME'.
  FIELDCAT-SELTEXT_M = 'Gross Total Income'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TOT_INCOME'.
  FIELDCAT-FIELDNAME = 'TOT_INCOME'.
  FIELDCAT-SELTEXT_M = 'Total Income'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TAX_TOT_INCOME'.
  FIELDCAT-FIELDNAME = 'TAX_TOT_INCOME'.
  FIELDCAT-SELTEXT_M = 'Tax on Total Income'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'CHAPVI_DED'.
  FIELDCAT-FIELDNAME = 'CHAPVI_DED'.
  FIELDCAT-SELTEXT_M = 'Chapter VI Deductions'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'SURCHG'.
  FIELDCAT-FIELDNAME = 'SURCHG'.
  FIELDCAT-SELTEXT_M = 'Surcharge'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'EDU_CESS'.
  FIELDCAT-FIELDNAME = 'EDU_CESS'.
  FIELDCAT-SELTEXT_M = 'Education Cess'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TAX_PAYABLE_BEFORE_RELIEF'.
  FIELDCAT-FIELDNAME = 'TAX_PAYABLE_BEFORE_RELIEF'.
  FIELDCAT-SELTEXT_M = 'Tax Payable Before Relief'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TAX_PAYABLE'.
  FIELDCAT-FIELDNAME = 'TAX_PAYABLE'.
  FIELDCAT-SELTEXT_M = 'Tax Payable'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'SEC89_RELIEF'.
  FIELDCAT-FIELDNAME = 'SEC89_RELIEF'.
  FIELDCAT-SELTEXT_M = 'Section 89 Relief'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TDS_PETD'.
  FIELDCAT-FIELDNAME = 'TDS_PETD'.
  FIELDCAT-SELTEXT_M = 'TDS PETD'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TDS_IFOS'.
  FIELDCAT-FIELDNAME = 'TDS_IFOS'.
  FIELDCAT-SELTEXT_M = 'TDS IFOS'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'VOL_TAX'.
  FIELDCAT-FIELDNAME = 'VOL_TAX'.
  FIELDCAT-SELTEXT_M = 'Voluntary Tax'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TOT_TAX_DEDUCTED'.
  FIELDCAT-FIELDNAME = 'TOT_TAX_DEDUCTED'.
  FIELDCAT-SELTEXT_M = 'Total Tax Deducted'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'TAX_PAID_EMPLOYER'.
  FIELDCAT-FIELDNAME = 'TAX_PAID_EMPLOYER'.
  FIELDCAT-SELTEXT_M = 'Tax Paid by the Employer'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.

  READ TABLE FIELDCAT WITH KEY FIELDNAME = 'NET_TAX_PAYABLE'.
  FIELDCAT-FIELDNAME = 'NET_TAX_PAYABLE'.
  FIELDCAT-SELTEXT_M = 'Net Tax Payable'.
  FIELDCAT-DDICTXT = 'M'.
  FIELDCAT-OUTPUTLEN = 10.
  MODIFY FIELDCAT INDEX SY-TABIX.
  CLEAR FIELDCAT.


  REFRESH G_ITAB_FCODE1.
  MOVE 'AMBC' TO G_ITAB_FCODE1-FCODE.
  APPEND G_ITAB_FCODE1.
  MOVE 'CORC' TO G_ITAB_FCODE1-FCODE.
  APPEND G_ITAB_FCODE1.
  IF PNPESSCF = ' '.
    PERFORM DISPLAY_ALV_GRID IN PROGRAM HINCALV0
                         TABLES DISP_TAB FIELDCAT G_ITAB_FCODE1
                         USING SY-REPID TEXT-015 TEXT-062.
  ELSE.
    PERFORM PRINT_MODULE.
  ENDIF.
ENDFORM.                    " DISPLAY_ALV

*&-------------------------------------------------------------------*
*&      Form  GET_EMPR_ADDRS
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
FORM GET_EMPR_ADDRS.

ENDFORM.                    "GET_EMPR_ADDRS
*&-------------------------------------------------------------------*
*&      Form  GET_EMPR_DETAILS
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM GET_EMPR_DETAILS TABLES P_P0001 STRUCTURE P0001
                             P_P0185 STRUCTURE P0185
                             P_P0002 STRUCTURE P0002
                             P_P0006 STRUCTURE P0006
                             P_P0021 STRUCTURE P0021
                             P_HD_TAB STRUCTURE HD_TAB
                      USING  P_WA_TMP_COCD-BEGDA
                             P_WA_TMP_COCD-CNTR2
                             P_WA_TMP_COCD-ENDDA
                             P_WA_TMP_COCD-BUKRS
                             P_MULTIPLE_F16
                             P_PERNR-PERNR
                             P_TANR
                             P_COCD_LINES
                             P_F16_BEGDA
                             P_F16_ENDDA.

  DATA: BEGIN OF ECC_TAB OCCURS 10,
         BACK(40) TYPE C,
        END OF ECC_TAB.
  DATA: WA_P0001 TYPE P0001,
        WA_P0002 TYPE P0002,
        WA_P0006 TYPE P0006,
        WA_P0021 TYPE P0021,
        LEN      TYPE I.
  CLEAR P_HD_TAB.
  IF P_TANR = ' '.  "IF TAN switch is inactive

*    CLEAR: TMP_P0001[], WA_P0001, TMP_P0002[], WA_P0002.
    IF P_MULTIPLE_F16 = ' '.
      RP-PROVIDE-FROM-LAST P_P0001 SPACE P_WA_TMP_COCD-BEGDA
                                         P_WA_TMP_COCD-ENDDA.
      MOVE-CORRESPONDING P_P0001 TO PME01.
    ELSEIF P_MULTIPLE_F16 = 'X'.
      LOOP AT P_P0001 INTO WA_P0001 WHERE BEGDA LE P_WA_TMP_COCD-ENDDA
                                    AND ENDDA GE P_WA_TMP_COCD-BEGDA
                                    AND BUKRS EQ P_WA_TMP_COCD-BUKRS.
        MOVE-CORRESPONDING WA_P0001 TO PME01.
        EXIT.
      ENDLOOP.
    ENDIF.


    CLEAR ECC_TAB.
    REFRESH ECC_TAB.
    CALL FUNCTION 'HR_FEATURE_BACKTABLE'
        EXPORTING
             FEATURE                     = '40ECC'
             STRUC_CONTENT               = PME01
             KIND_OF_ERROR               = '3'
        TABLES
             BACK                        = ECC_TAB
*      CHANGING
*           STATUS                      =
       EXCEPTIONS
*           DUMMY                       = 1
*           ERROR_OPERATION             = 2
*           NO_BACKVALUE                = 3
*           FEATURE_NOT_GENERATED       = 4
*           INVALID_SIGN_IN_FUNID       = 5
*           TAB_IN_REPORT_FIELD_IN_PE03 = 6
            OTHERS                      = 0
             .
    IF SY-SUBRC = 0.                                        "#EC *
      CLEAR ECC_TAB.
      LOOP AT ECC_TAB.

        CASE SY-TABIX.
          WHEN 1.
          WHEN 2.
            P_HD_TAB-PANNO = ECC_TAB-BACK+0(15).
          WHEN 3.
            P_HD_TAB-TANNO = ECC_TAB-BACK+0(15).
          WHEN 4.
            LEN = STRLEN( ECC_TAB-BACK ).
            IF  LEN > 0.
              P_HD_TAB-GIRNO = ECC_TAB-BACK+0(20).
              LEN = STRLEN( HD_TAB-PANNO ).
              IF LEN > 0.
                MOVE '/' TO P_HD_TAB-PANNO+LEN(1).
              ENDIF.
            ENDIF.
        ENDCASE.

      ENDLOOP.
    ENDIF.

  ELSEIF P_TANR = 'X'.  " IF TAN switch is active
    IF P_MULTIPLE_F16 = ' '.
      RP-PROVIDE-FROM-LAST P_P0185 05 P_WA_TMP_COCD-BEGDA
                                         P_WA_TMP_COCD-ENDDA.
      IF PNP-SW-FOUND EQ 0 AND PNPESSCF = ' '.
        MESSAGE S089(HRPADIN01) WITH '0185 Subtype 05' PERNR-PERNR
                                                       P_WA_TMP_COCD-BEGDA
                                                       P_WA_TMP_COCD-ENDDA.
*   There is no infotype & for personnel no & from period & to &
        PERFORM BUILD_ERROR TABLES HR_ERROR
                           USING SPACE SY-MSGID SY-MSGNO
                           SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
        REJECT.
      ENDIF.
      MOVE P_P0185-ICNUM TO P_HD_TAB-TANNO.
      MOVE P_P0185-ICOLD TO P_HD_TAB-PANNO.
      MOVE P_P0185-AUTH1 TO P_HD_TAB-GIRNO.
    ELSEIF P_MULTIPLE_F16 = 'X'.
      LOOP AT P_P0185 WHERE SUBTY = '05'
                      AND BEGDA LE P_WA_TMP_COCD-ENDDA
                      AND ENDDA GE P_WA_TMP_COCD-BEGDA
                      AND DOCN1 =  P_WA_TMP_COCD-BUKRS.
        MOVE P_P0185-ICNUM TO P_HD_TAB-TANNO.
        MOVE P_P0185-ICOLD TO P_HD_TAB-PANNO.
        MOVE P_P0185-AUTH1 TO P_HD_TAB-GIRNO.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDIF.

  P_HD_TAB-CNTR2     = P_WA_TMP_COCD-CNTR2.
  P_HD_TAB-PERNR     = P_PERNR-PERNR.

  IF P_COCD_LINES = P_WA_TMP_COCD-CNTR2.
    P_HD_TAB-F16_BEGDA = P_WA_TMP_COCD-BEGDA.
    P_HD_TAB-F16_ENDDA = P_F16_ENDDA.
  ELSE.
    P_HD_TAB-F16_BEGDA = P_WA_TMP_COCD-BEGDA.
    P_HD_TAB-F16_ENDDA = P_WA_TMP_COCD-ENDDA.
  ENDIF.

*  PERFORM GET_EMPLOYEE_DETAILS TABLES P_HD_TAB USING P_HD_TAB-PERNR.

*  TMP_P0002[] = P_P0002[].
*  SORT TMP_P0002 BY BEGDA DESCENDING.
*  READ TABLE TMP_P0002 INTO WA_P0002 INDEX 1.
  RP-PROVIDE-FROM-LAST P_P0002 SPACE P_WA_TMP_COCD-BEGDA
                                   P_WA_TMP_COCD-ENDDA.
  MOVE-CORRESPONDING P_P0002 TO WA_P0002.

*  TMP_P0006[] = P_P0006[].
*  SORT TMP_P0006 BY BEGDA DESCENDING.
  SELECT SINGLE KWERT FROM T511K INTO KWERT
    WHERE MOLGA = CALCMOLGA
      AND KONST = 'ADDSS'
      AND BEGDA <= P_WA_TMP_COCD-BEGDA
      AND ENDDA >= P_WA_TMP_COCD-BEGDA.
*  RP-PROVIDE-FROM-LAST P0006 KWERT PBEGDA PENDDA.
  RP-PROVIDE-FROM-LAST P_P0006 KWERT P_WA_TMP_COCD-BEGDA
                                     P_WA_TMP_COCD-ENDDA.
  MOVE-CORRESPONDING P_P0006 TO WA_P0006.

*  TMP_P0021[] = P_P0021[].
*  SORT TMP_P0021 BY BEGDA DESCENDING.
  RP-PROVIDE-FROM-LAST P_P0021 ' ' P_WA_TMP_COCD-BEGDA
                                 P_WA_TMP_COCD-ENDDA.
*    IF PNP-SW-FOUND EQ 0.
*     MESSAGE S089(HRPADIN01) WITH '0021' PERNR-PERNR PBEGDA PENDDA.
**   There is no infotype & for personnel no & from period & to &
*     PERFORM BUILD_ERROR TABLES HR_ERROR
*                        USING SPACE SY-MSGID SY-MSGNO
*                        SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
**     REJECT.
*   ENDIF.
  MOVE-CORRESPONDING P_P0021 TO WA_P0021.

  CALL FUNCTION 'RP_EDIT_NAME'
         EXPORTING
             FORMAT    = '01'
              LANGU     = SY-LANGU
              MOLGA     = '40'
            PP0002     =  WA_P0002
          IMPORTING
              EDIT_NAME = P_HD_TAB-ENAME
*         RETCODE   =
         EXCEPTIONS
              OTHERS    = 1.

  RP-PROVIDE-FROM-LAST P_P0185 '02' P_WA_TMP_COCD-BEGDA
                                    P_WA_TMP_COCD-ENDDA.
  IF PNP-SW-FOUND EQ 0 AND PNPESSCF = ' '.
    MESSAGE S089(HRPADIN01) WITH '0185 Subtype 02' PERNR-PERNR
                                                   P_WA_TMP_COCD-BEGDA
                                                   P_WA_TMP_COCD-ENDDA.
*   There is no infotype & for personnel no & from period & to &
    PERFORM BUILD_ERROR TABLES HR_ERROR
                       USING SPACE SY-MSGID SY-MSGNO
                       SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
*     REJECT.
  ENDIF.
  IF SY-SUBRC = 0.
    MOVE P_P0185-ICNUM TO P_HD_TAB-ICNUM.
  ENDIF.
  MOVE WA_P0002-GESCH TO P_HD_TAB-GENDER.
  MOVE WA_P0021-FAVOR TO P_HD_TAB-FFNAME.
  MOVE WA_P0021-FANAM TO P_HD_TAB-FLNAME.
  MOVE WA_P0006-STRAS TO P_HD_TAB-HNUMB.
  MOVE WA_P0006-LOCAT TO P_HD_TAB-LOCALITY.
  MOVE WA_P0006-PSTLZ TO P_HD_TAB-PIN.
  MOVE WA_P0006-ORT01 TO P_HD_TAB-CITY.
  MOVE WA_P0006-LAND1 TO P_HD_TAB-COUNTRY.
  MOVE WA_P0006-TELNR TO P_HD_TAB-TELN.
  IF P_HD_TAB-GENDER = 1.
    P_HD_TAB-GENDER = 'M'.
  ELSE.
    P_HD_TAB-GENDER = 'F'.
  ENDIF.
  MOVE WA_P0002-GBDAT TO P_HD_TAB-DOB.

  CLEAR WA_P0001.
  RP-PROVIDE-FROM-LAST P_P0001 SPACE P_HD_TAB-F16_BEGDA
P_HD_TAB-F16_ENDDA.
  MOVE-CORRESPONDING P_P0001 TO WA_P0001.
  PERFORM RE_T7IN0P USING WA_P0001-WERKS WA_P0001-BTRTL.
  PERFORM RE_T7INT5 USING T7IN0P-TXGRP.
  P_HD_TAB-ADDRS = T7INT5-ADDRS.
  CLEAR T528T.

  CHECK T528T-PLANS NE P_P0001-PLANS
    OR T528T-OTYPE NE P_P0001-OTYPE
    OR T528T-ENDDA LT P_P0001-ENDDA.

  SELECT SINGLE * FROM T528T CLIENT SPECIFIED
         WHERE  MANDT =  SY-MANDT
         AND    SPRSL =  SY-LANGU
         AND    OTYPE =  WA_P0001-OTYPE
         AND    PLANS =  WA_P0001-PLANS
         AND    ENDDA GE WA_P0001-ENDDA.

  IF SY-SUBRC EQ 0.
    P_HD_TAB-POSITION = T528T-PLSTX.
  ENDIF.

  DATA: BD_DESIG TYPE REF TO HR_IN_EE_DESIGN,
        FLG(1),
        FLG1(1),
        POSIT TYPE CHAR40,
        DIRSTAT TYPE C.

  TRY.
      GET BADI BD_DESIG
        FILTERS
          FLT_VAL = '40'.

      CALL BADI BD_DESIG->EMPLOYEE_DESIGNATION
        EXPORTING
          PERNR       = WA_P0001-PERNR
          P0001       = WA_P0001
          BEGDA       = P_WA_TMP_COCD-BEGDA
          ENDDA       = P_WA_TMP_COCD-ENDDA
          FLT_VAL     = '40'
          DESIGNATION = P_HD_TAB-POSITION
        IMPORTING
          DESIGN      = POSIT.
      FLG = 'X'.
    CATCH CX_BADI_NOT_IMPLEMENTED.
"$$
  ENDTRY.
  TRY.
      GET BADI BD_DESIG
        FILTERS
          FLT_VAL = '40'.

      CALL BADI BD_DESIG->RETURN_DIRSTAT
        EXPORTING
          PERNR   = WA_P0001-PERNR
          P0001   = WA_P0001
          BEGDA   = P_WA_TMP_COCD-BEGDA
          ENDDA   = P_WA_TMP_COCD-ENDDA
          FLT_VAL = '40'
        IMPORTING
          DIRSTAT = DIRSTAT.
      FLG1 = 'X'.
    CATCH CX_BADI_NOT_IMPLEMENTED.
"$$
  ENDTRY.
  IF FLG = 'X' AND POSIT NE SPACE.
    P_HD_TAB-POSITION = POSIT.
  ENDIF.

  IF FLG1 = 'X' AND DIRSTAT NE SPACE.
    P_HD_TAB-DIRST = DIRSTAT.
  ENDIF.

  APPEND P_HD_TAB.



ENDFORM.                    " GET_EMPR_DETAILS


*&---------------------------------------------------------------------*
*&      Form  FILL_INT_S80
*&---------------------------------------------------------------------*
*       tex
*----------------------------------------------------------------------*
*      -->P_INT_S80            text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR        text
*----------------------------------------------------------------------*
FORM FILL_INT_S80  TABLES P_INT_S80 STRUCTURE INT_S80

                   USING  P_P_S80 TYPE T_PAY_RESULT-S80
                          P_WA_TMP_COCD_CNTR2
                          P_PERNR_PERNR PBEGDA PENDDA.

  DATA : GRS LIKE PC207-BETRG,
          LMT LIKE PC207-BETRG.
  DATA: P_S80 TYPE T_PAY_RESULT-S80 WITH HEADER LINE.

  CLEAR: P_INT_S80, WA_S80, P_S80.
  REFRESH: P_S80.

  P_S80[] = P_P_S80[].
  I = 0.
  SORT P_INT_S80[] BY SBSEC SBDIV.
  SELECT SINGLE * FROM T7INI7 WHERE SBSEC = '15' AND
                                        BEGDA LE PENDDA  AND
                                        ENDDA GE PBEGDA.
  LMT = T7INI7-SSCLT.
  LOOP AT P_S80.
    IF P_S80-SBSEC = 01.
      IF P_S80-SDVLT > LMT.
        P_S80-SDVLT = LMT.
      ENDIF.
* CONMT and COAMT refer to the same value (contribution/investment
* amount. COAMT is added to increase the field length so that it
* can capture amount > 9999999.99. This field will be filled up
* after customer has applied relevant Note/HR SP. For payroll
* results created earlier, CONMT should be used for reporting.
      IF P_S80-COAMT IS INITIAL.

        IF P_S80-CONMT GT P_S80-SDVLT.
          GRS = P_S80-SDVLT.
          LMT = LMT - GRS.
        ELSE.
          GRS = P_S80-CONMT.
          LMT = P_S80-SDVLT - GRS.
        ENDIF.
      ELSE.
        IF P_S80-COAMT GT P_S80-SDVLT.
          GRS = P_S80-SDVLT.
          LMT = LMT - GRS.
        ELSE.
          GRS = P_S80-COAMT.
          LMT = P_S80-SDVLT - GRS.
        ENDIF.
      ENDIF.
    ENDIF.

    IF P_S80-SBSEC = 15.
      READ TABLE RT WITH KEY LGART = '/6I2'.
      IF SY-SUBRC EQ 0 AND RT-BETRG > 0.
        P_S80-DEDMT = P_S80-DEDMT + RT-BETRG.
        P_S80-QLAMT = P_S80-QLAMT + RT-BETRG.
        P_S80-CONMT = P_S80-CONMT + RT-BETRG.
        P_S80-COAMT = P_S80-COAMT + RT-BETRG.
      ENDIF.
      READ TABLE P_S80 WITH KEY SBSEC = '15' SBDIV = '01' INTO WA_S80.
      IF WA_S80-COAMT IS INITIAL.
        IF P_S80-CONMT GT LMT.
          P_S80-QLAMT = LMT.
          P_S80-DEDMT = LMT.
          GRS = LMT.
        ELSE.
          GRS = P_S80-CONMT.
        ENDIF.
      ELSE.
        IF P_S80-COAMT GT LMT.
          P_S80-QLAMT = LMT.
          P_S80-DEDMT = LMT.
          GRS = LMT.
        ELSE.
          GRS = P_S80-COAMT.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE P_WA_TMP_COCD_CNTR2 TO P_INT_S80-CNTR2.
    TEMP = ALPHA+I(1).
    I = I + 1.
    TEMP1 = '('.
    MOVE TEMP TO TEMP1+1(1).
    MOVE ')' TO TEMP1+2(1).
    CONDENSE TEMP1 NO-GAPS.
    MOVE-CORRESPONDING P_S80 TO P_INT_S80.
    MOVE P_PERNR_PERNR TO P_INT_S80-PERNR.
    SELECT SINGLE * FROM T7INI5 WHERE SBSEC = P_S80-SBSEC.
    MOVE-CORRESPONDING T7INI5 TO P_INT_S80.
    IF P_S80-SBSEC = '15' OR P_S80-SBSEC = '1'.
      READ TABLE P_S80 WITH KEY SBSEC = '15' SBDIV = '01' INTO WA_S80.
      IF WA_S80-COAMT IS INITIAL.
        MOVE P_S80-CONMT TO P_INT_S80-COAMT.
      ELSE.
        MOVE P_S80-COAMT TO P_INT_S80-COAMT.
      ENDIF.
      MOVE GRS TO P_INT_S80-QLAMT.
      MOVE GRS TO P_INT_S80-DEDMT.
    ELSE.
      IF P_S80-COAMT IS INITIAL.
        MOVE P_S80-CONMT TO P_INT_S80-COAMT.
      ELSE.
        MOVE P_S80-COAMT TO P_INT_S80-COAMT.
      ENDIF.
    ENDIF.
    MOVE T7INI5-SBTDS TO TEMP1+4.
    MOVE TEMP1 TO P_INT_S80-SBTDS.
    APPEND P_INT_S80.
  ENDLOOP.
  SORT P_INT_S80[] BY SBSEC SBDIV.
*  DELETE P_INT_S80 WHERE DEDMT IS INITIAL.

  INT_S80[] = P_INT_S80[].

ENDFORM.                    " FILL_INT_S80

*&---------------------------------------------------------------------*
*&      Form  FILL_INT_S88
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INT_S88  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR  text
*----------------------------------------------------------------------*
FORM FILL_INT_S88  TABLES   P_INT_S88 STRUCTURE INT_S88
                   USING    P_P_S88 TYPE T_PAY_RESULT-S88
                            P_WA_TMP_COCD_CNTR2
                            P_PERNR_PERNR
                   CHANGING P_S88_TOTAL.

  DATA : RESULT(10) TYPE C.
  DATA : TEMP2(10) TYPE C.
  DATA: P_S88 TYPE T_PAY_RESULT-S88 WITH HEADER LINE.
  CLEAR RESULT.
  CLEAR: P_INT_S88, P_S88.
  REFRESH P_S88.
  DATA: TMP_T7INI3 TYPE STANDARD TABLE OF T7INI3 WITH HEADER LINE.
  I = 1.

  P_S88[] = P_P_S88[].
  SORT P_S88 BY ICODE.
  LOOP AT P_S88.
    PERFORM GET_ROMAN_NUMBER_1 USING I RESULT.
    TEMP = RESULT.
    I = I + 1.
    TEMP1 = '('.
    MOVE TEMP TO TEMP1.
    TEMP2 = STRLEN( TEMP1 ).
    MOVE ')' TO TEMP1+TEMP2(1).
    CONDENSE TEMP1 NO-GAPS.
    MOVE P_WA_TMP_COCD_CNTR2 TO P_INT_S88-CNTR2.
    MOVE-CORRESPONDING P_S88 TO P_INT_S88.
    MOVE P_PERNR_PERNR TO P_INT_S88-PERNR.
    SELECT SINGLE * INTO TMP_T7INI3 FROM T7INI3
                    WHERE ICODE = P_S88-ICODE.        "ECCI_GENBUFF
    MOVE TMP_T7INI3-ITEXT(50) TO TEMP1+5.
    MOVE-CORRESPONDING TMP_T7INI3 TO P_INT_S88.
    MOVE TEMP1 TO P_INT_S88-ITEXT.
    APPEND P_INT_S88.
    IF P_S88-INAMT IS INITIAL.
      P_S88_TOTAL = P_S88_TOTAL + P_S88-INVMT.
    ELSE.
      P_S88_TOTAL = P_S88_TOTAL + P_S88-INAMT.
    ENDIF.
  ENDLOOP.
  SORT P_INT_S88 BY ICODE.
  INT_S88[] = P_INT_S88[].

ENDFORM.                    " FILL_INT_S88
*&---------------------------------------------------------------------*
*&      Form  GET_ROMAN_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I  text
*----------------------------------------------------------------------*
FORM GET_ROMAN_NUMBER_1  USING    P_I
                                P_RESULT.
  DATA : COUNTER TYPE I,
          ALPHA(10) TYPE C,
          IQ(10) TYPE C,
          LEN1 TYPE I,
          LOOPCOUNTER TYPE I VALUE 0,
          FINALCOUNTER TYPE I VALUE 0.
  DATA : NUMB(10) TYPE C VALUE '1'.
  DATA : ROMA(150) TYPE C.
  ROMA = TEXT-RMN.
  LEN1 = STRLEN( ROMA ).

  COUNTER = SY-FDPOS + 1.

  NUMB = P_I + 1.
  NUMB = NUMB - 1.

  WHILE LOOPCOUNTER LT NUMB.
    LOOPCOUNTER = LOOPCOUNTER + 1.
    LEN1 = ROMA+0(1).
    LEN1 = LEN1 + 1.
    SHIFT ROMA BY LEN1 PLACES LEFT.
  ENDWHILE.

  LEN1 = ROMA+0(1).

  SHIFT ROMA BY 1 PLACES LEFT.

  WHILE FINALCOUNTER LT LEN1.
    P_RESULT+FINALCOUNTER = ROMA+FINALCOUNTER(1).
    FINALCOUNTER = FINALCOUNTER + 1.
  ENDWHILE.
ENDFORM.                    " GET_ROMAN_NUMBER

*&---------------------------------------------------------------------*
*&      Form  GET_WTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I512W  text
*----------------------------------------------------------------------*
FORM GET_WTS_1  TABLES   P_I512W STRUCTURE I512W
              USING    P_PNPENDDA.

  SELECT * FROM T512W WHERE MOLGA = '40' AND BEGDA <= P_PNPENDDA

                                         AND ENDDA >=
P_PNPENDDA.

    IF  ( T512W-AKLAS+10(2) IS INITIAL )   AND
        ( T512W-AKLAS+12(2) IS INITIAL ) AND
        ( T512W-AKLAS+14(2) IS INITIAL ) AND
        ( T512W-AKLAS+16(2) IS INITIAL ).
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING T512W TO P_I512W.
    APPEND P_I512W.
  ENDSELECT.
  SORT P_I512W[] BY LGART.

ENDFORM.                    " GET_WTS
*&---------------------------------------------------------------------*
*&      Form  FILL_GROSS_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GROSS_TAB  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR  text
*      -->P_ENDLOOP  text
*----------------------------------------------------------------------*
FORM FILL_GROSS_TAB  TABLES   P_GROSS_TAB STRUCTURE GROSS_TAB
                     USING    P_LAST_RESULT TYPE T_PAY_RESULT-RT_TAB
                              P_WA_TMP_COCD_CNTR2
                              P_PERNR_PERNR.

  SORT RT BY LGART.
  LOOP AT I512W WHERE NOT AKLAS+10(2) IS INITIAL.
    CLEAR P_GROSS_TAB.
    P_GROSS_TAB-PERNR = P_PERNR_PERNR.
    MOVE P_WA_TMP_COCD_CNTR2 TO P_GROSS_TAB-CNTR2.
    READ TABLE F16 WITH KEY CNTR2 = P_WA_TMP_COCD_CNTR2
                LGART = I512W-LGART
                CUMTY = 'Y'.

    IF SY-SUBRC = 0.
      P_GROSS_TAB-AMOUNT = F16-BETRG.
      P_GROSS_TAB-SIGN = F16-ANZHL.
    ELSE.
      PERFORM READ_RT USING I512W-LGART
                            P_LAST_RESULT
                      CHANGING P_GROSS_TAB-AMOUNT
                               P_GROSS_TAB-SIGN.
    ENDIF.
    IF P_GROSS_TAB-AMOUNT <> 0.
      P_GROSS_TAB-EVCLS_SPEC = I512W-AKLAS+10(2).
      PERFORM READ_SPEC_TXT USING '06' I512W-AKLAS+10(2)
                            CHANGING P_GROSS_TAB-SPEC_TXT.
      COLLECT P_GROSS_TAB.
    ENDIF.
  ENDLOOP.
  SORT P_GROSS_TAB BY PERNR EVCLS_SPEC.

ENDFORM.                    " FILL_GROSS_TAB

*&---------------------------------------------------------------------*
*&      Form  READ_RT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I512W_LGART  text
*      <--P_P_GROSS_TAB_AMOUNT  text
*----------------------------------------------------------------------*
FORM READ_RT  USING    P_I512W_LGART
                       P_LAST_RESULT TYPE T_PAY_RESULT-RT_TAB
              CHANGING P_SAL
                       P_SIGN.

  DATA: RT_TMP TYPE T_PAY_RESULT-RT_TAB WITH HEADER LINE.
  RT_TMP[] = P_LAST_RESULT[].

  LOOP AT RT_TMP WHERE LGART = P_I512W_LGART.
    P_SAL = P_SAL + RT_TMP-BETRG.
    P_SIGN = RT_TMP-ANZHL.
  ENDLOOP.

ENDFORM.                    " READ_RT

*&---------------------------------------------------------------------*
*&      Form  READ_SPEC_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2277   text
*      -->P_I512W_AKLAS+10(2)  text
*      <--P_P_GROSS_TAB_SPEC_TXT  text
*----------------------------------------------------------------------*
*FORM READ_SPEC_TXT  USING    VALUE(P_EVCLS)
*                             P_SPEC
*                    CHANGING P_SPEC_TXT.
*  SELECT SINGLE EVCVT INTO P_SPEC_TXT FROM T52DB
*                      WHERE SPRSL = SY-LANGU AND
*                            MOLGA = '40' AND
*                            EVCLS = P_EVCLS AND
*                            EVCLV = P_SPEC.
*ENDFORM.                    " READ_SPEC_TXT


*&---------------------------------------------------------------------*
*&      Form  FILL_IFOS_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IFOS_TAB  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR  text
*----------------------------------------------------------------------*
FORM FILL_IFOS_TAB  TABLES   P_IFOS_TAB STRUCTURE IFOS_TAB
                    USING    P_LAST_RESULT TYPE T_PAY_RESULT-RT_TAB
                             P_WA_TMP_COCD_CNTR2
                             P_PERNR_PERNR.

  SORT RT BY LGART.
  LOOP AT I512W WHERE NOT AKLAS+14(2) IS INITIAL.
    CLEAR P_IFOS_TAB.
    P_IFOS_TAB-PERNR = P_PERNR_PERNR.
    MOVE P_WA_TMP_COCD_CNTR2 TO P_IFOS_TAB-CNTR2.
*     READ TABLE CRT WITH KEY LGART = I512W-LGART CUMTY = 'Y'.
    READ TABLE F16 WITH KEY CNTR2 = P_WA_TMP_COCD_CNTR2
                LGART = I512W-LGART
                CUMTY = 'Y'.
    IF SY-SUBRC = 0.
      P_IFOS_TAB-AMOUNT = F16-BETRG.
      P_IFOS_TAB-SIGN = F16-ANZHL.
    ELSE.
      PERFORM READ_RT USING I512W-LGART
                            P_LAST_RESULT
                      CHANGING P_IFOS_TAB-AMOUNT
                               P_IFOS_TAB-SIGN.
    ENDIF.
    IF P_IFOS_TAB-AMOUNT <> 0.
      P_IFOS_TAB-EVCLS_SPEC = I512W-AKLAS+14(2).
      PERFORM READ_SPEC_TXT USING '08' I512W-AKLAS+14(2)
                            CHANGING P_IFOS_TAB-SPEC_TXT.
      COLLECT P_IFOS_TAB.
    ENDIF.

  ENDLOOP.
  SORT P_IFOS_TAB[] BY PERNR EVCLS_SPEC.

ENDFORM.                    " FILL_IFOS_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_PERK_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERK_TAB  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR  text
*----------------------------------------------------------------------*
FORM FILL_PERK_TAB  TABLES   P_PERK_TAB STRUCTURE PERK_TAB
                    USING    P_LAST_RESULT TYPE T_PAY_RESULT-RT_TAB
                             P_WA_TMP_COCD_CNTR2
                             P_PERNR_PERNR.

  SORT RT BY LGART.
  LOOP AT I512W WHERE NOT AKLAS+16(2) IS INITIAL.
    CLEAR P_PERK_TAB.
    IF I512W-AKLAS+16(2) BETWEEN 31 AND 60.
      CONTINUE.
    ENDIF.
    P_PERK_TAB-PERNR = P_PERNR_PERNR.
    MOVE P_WA_TMP_COCD_CNTR2 TO P_PERK_TAB-CNTR2.
    READ TABLE F16 WITH KEY CNTR2 = P_WA_TMP_COCD_CNTR2
                LGART = I512W-LGART
                CUMTY = 'Y'.
    IF SY-SUBRC = 0.
      P_PERK_TAB-AMOUNT = F16-BETRG.
      P_PERK_TAB-SIGN = F16-ANZHL.
    ELSE.
      PERFORM READ_RT USING I512W-LGART
                            P_LAST_RESULT
                      CHANGING P_PERK_TAB-AMOUNT
                               P_PERK_TAB-SIGN.
    ENDIF.
    IF P_PERK_TAB-AMOUNT <> 0.
      P_PERK_TAB-EVCLS_SPEC = I512W-AKLAS+16(2).
      PERFORM READ_SPEC_TXT USING '09' I512W-AKLAS+16(2)
                            CHANGING P_PERK_TAB-SPEC_TXT.
      COLLECT P_PERK_TAB.
    ENDIF.

  ENDLOOP.
  SORT P_PERK_TAB[] BY PERNR EVCLS_SPEC.

ENDFORM.                    " FILL_PERK_TAB
*&---------------------------------------------------------------------*
*&      Form  FILL_SEC10_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SEC10_TAB  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR  text
*----------------------------------------------------------------------*
FORM FILL_SEC10_TAB  TABLES   P_SEC10_TAB STRUCTURE SEC10_TAB
                     USING    P_LAST_RESULT TYPE T_PAY_RESULT-RT_TAB
                              P_WA_TMP_COCD_CNTR2
                              P_PERNR_PERNR.

  SORT RT BY LGART.
  LOOP AT I512W WHERE NOT AKLAS+12(2) IS INITIAL.
    CLEAR P_SEC10_TAB.
    P_SEC10_TAB-PERNR = P_PERNR_PERNR.
    MOVE P_WA_TMP_COCD_CNTR2 TO SEC10_TAB-CNTR2.
    READ TABLE F16 WITH KEY CNTR2 = P_WA_TMP_COCD_CNTR2
                LGART = I512W-LGART
                CUMTY = 'Y'.
    IF SY-SUBRC = 0.
      P_SEC10_TAB-AMOUNT = F16-BETRG.
      P_SEC10_TAB-SIGN = F16-ANZHL.
    ELSE.
      PERFORM READ_RT USING I512W-LGART
                            P_LAST_RESULT
                      CHANGING P_SEC10_TAB-AMOUNT
                               P_SEC10_TAB-SIGN.
    ENDIF.
    IF P_SEC10_TAB-AMOUNT <> 0.
      P_SEC10_TAB-EVCLS_SPEC = I512W-AKLAS+12(2).
      PERFORM READ_SPEC_TXT USING '07' I512W-AKLAS+12(2)
                            CHANGING P_SEC10_TAB-SPEC_TXT.
      COLLECT P_SEC10_TAB.
    ENDIF.

  ENDLOOP.
  SORT P_SEC10_TAB BY PERNR EVCLS_SPEC.

ENDFORM.                    " FILL_SEC10_TAB

*&---------------------------------------------------------------------*
*&      Form  FILL_SUM_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SUM_TAB  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_PERNR_PERNR  text
*----------------------------------------------------------------------*
FORM FILL_SUM_TAB  TABLES   P_SUM_TAB STRUCTURE SUM_TAB
                            P_MON_TAX_COMP LIKE MON_TAX_COMP
                            P_CH_DET LIKE CH_DET
                            P_P0001 STRUCTURE P0001
                            P_P0185 STRUCTURE P0185
                   USING    P_WA_TMP_COCD_CNTR2
                            P_WA_TMP_COCD_BUKRS
                            P_PERNR_PERNR
                            P_HD_TAB_TANNO
                            P_YEAR
                            P_TANR
                            P_MULTIPLE_F16
                            P_AFY_SWITCH TYPE C.

  DATA: WA_SUM_TAB LIKE LINE OF SUM_TAB,
        WA_MON_TAX_COMP LIKE LINE OF MON_TAX_COMP.

  DATA: PINAC_STRUCT LIKE PINAC,
        WA_CH_DET TYPE TY_CH_DET.

  DATA: BEGIN OF ECC_TAB OCCURS 10,
         BACK(40) TYPE C,
        END OF ECC_TAB.

  DATA: TANNO TYPE PIN_TANNO,
        TAX_DEPOSITED TYPE PC207-BETRG.

  DATA: COUNTER1, COUNTER2, COUNTER3, COUNTER4.

  CLEAR: WA_SUM_TAB, WA_MON_TAX_COMP, PINAC_STRUCT.

  WA_SUM_TAB-CNTR2 = P_WA_TMP_COCD_CNTR2.
  WA_SUM_TAB-PERNR = P_PERNR_PERNR.

  LOOP AT P_MON_TAX_COMP INTO WA_MON_TAX_COMP
       WHERE CNTR2 = P_WA_TMP_COCD_CNTR2
        AND PERNR = P_PERNR_PERNR.

    CLEAR: TAX_DEPOSITED,WA_CH_DET.
*    REFRESH P4DEDT.
    PERFORM GET_TAN TABLES P_P0001 P_P0185
                    USING  WA_MON_TAX_COMP-FPBEG
                           WA_MON_TAX_COMP-FPEND
                           WA_MON_TAX_COMP-PAYDT
                           P_TANR
                           P_MULTIPLE_F16
                           P_WA_TMP_COCD_BUKRS
                    CHANGING TANNO.

    PINAC_STRUCT-TANNO = TANNO.
    PINAC_STRUCT-FYEAR = P_YEAR.

    CLEAR ECC_TAB.
    REFRESH ECC_TAB.

    CALL FUNCTION 'HR_FEATURE_BACKTABLE'
      EXPORTING
        FEATURE                           = '40ACK'
        STRUC_CONTENT                     = PINAC_STRUCT
        KIND_OF_ERROR                     = '3'
      TABLES
        BACK                              = ECC_TAB
*      CHANGING
*        STATUS                            =
     EXCEPTIONS
          DUMMY                             = 1
          ERROR_OPERATION                   = 2
          NO_BACKVALUE                      = 3
          FEATURE_NOT_GENERATED             = 4
          INVALID_SIGN_IN_FUNID             = 5
          TAB_IN_REPORT_FIELD_IN_PE03       = 6
       OTHERS                            = 0
              .

    LOOP AT P_CH_DET INTO WA_CH_DET
                    WHERE  PERNR = P_PERNR_PERNR
                      AND  FPBEG = WA_MON_TAX_COMP-FPBEG
                      AND  FPEND = WA_MON_TAX_COMP-FPEND
                      AND  CNTR2 = WA_MON_TAX_COMP-CNTR2.
      "TANNO = TANNO.
      IF WA_CH_DET-BETRG IS INITIAL.
        TAX_DEPOSITED = TAX_DEPOSITED + WA_CH_DET-AMONT.
      ELSE.
        TAX_DEPOSITED = TAX_DEPOSITED + WA_CH_DET-BETRG.
      ENDIF.
    ENDLOOP.

    IF P_AFY_SWITCH = ' '.
      IF WA_MON_TAX_COMP-FPBEG+4(2) = '01'
        OR WA_MON_TAX_COMP-FPBEG+4(2) = '02'
        OR WA_MON_TAX_COMP-FPBEG+4(2) = '03'.
        WA_SUM_TAB-TAX_DED_Q4 = WA_SUM_TAB-TAX_DED_Q4
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q4 = WA_SUM_TAB-TAX_DEP_Q4
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '4'.
              COUNTER4 = COUNTER4 + 1.
              CASE COUNTER4.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q4_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q4_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q4_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q4_4 = ECC_TAB-BACK+2.
              ENDCASE.
          ENDCASE.
        ENDLOOP.

      ELSEIF WA_MON_TAX_COMP-FPBEG+4(2) = '04'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '05'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '06'.
        WA_SUM_TAB-TAX_DED_Q1 = WA_SUM_TAB-TAX_DED_Q1
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q1 = WA_SUM_TAB-TAX_DEP_Q1
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '1'.
              COUNTER1 = COUNTER1 + 1.
              CASE COUNTER1.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q1_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q1_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q1_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q1_4 = ECC_TAB-BACK+2.

              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ELSEIF WA_MON_TAX_COMP-FPBEG+4(2) = '07'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '08'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '09'.
        WA_SUM_TAB-TAX_DED_Q2 = WA_SUM_TAB-TAX_DED_Q2
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q2 = WA_SUM_TAB-TAX_DEP_Q2
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '2'.
              COUNTER2 = COUNTER2 + 1.
              CASE COUNTER2.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q2_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q2_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q2_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q2_4 = ECC_TAB-BACK+2.
              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ELSEIF WA_MON_TAX_COMP-FPBEG+4(2) = '10'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '11'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '12'.
        WA_SUM_TAB-TAX_DED_Q3 = WA_SUM_TAB-TAX_DED_Q3
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q3 = WA_SUM_TAB-TAX_DEP_Q3
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '3'.
              COUNTER3 = COUNTER3 + 1.
              CASE COUNTER3.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q3_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q3_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q3_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q3_4 = ECC_TAB-BACK+2.
              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ELSE.
      IF WA_MON_TAX_COMP-FPBEG+4(2) = '12'
        OR WA_MON_TAX_COMP-FPBEG+4(2) = '01'
        OR WA_MON_TAX_COMP-FPBEG+4(2) = '02'.
        WA_SUM_TAB-TAX_DED_Q4 = WA_SUM_TAB-TAX_DED_Q4
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q4 = WA_SUM_TAB-TAX_DEP_Q4
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '4'.
              COUNTER4 = COUNTER4 + 1.
              CASE COUNTER4.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q4_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q4_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q4_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q4_4 = ECC_TAB-BACK+2.
              ENDCASE.
          ENDCASE.
        ENDLOOP.

      ELSEIF WA_MON_TAX_COMP-FPBEG+4(2) = '03'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '04'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '05'.
        WA_SUM_TAB-TAX_DED_Q1 = WA_SUM_TAB-TAX_DED_Q1
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q1 = WA_SUM_TAB-TAX_DEP_Q1
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '1'.
              COUNTER1 = COUNTER1 + 1.
              CASE COUNTER1.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q1_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q1_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q1_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q1_4 = ECC_TAB-BACK+2.

              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ELSEIF WA_MON_TAX_COMP-FPBEG+4(2) = '06'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '07'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '08'.
        WA_SUM_TAB-TAX_DED_Q2 = WA_SUM_TAB-TAX_DED_Q2
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q2 = WA_SUM_TAB-TAX_DEP_Q2
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '2'.
              COUNTER2 = COUNTER2 + 1.
              CASE COUNTER2.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q2_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q2_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q2_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q2_4 = ECC_TAB-BACK+2.
              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ELSEIF WA_MON_TAX_COMP-FPBEG+4(2) = '09'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '10'
          OR WA_MON_TAX_COMP-FPBEG+4(2) = '11'.
        WA_SUM_TAB-TAX_DED_Q3 = WA_SUM_TAB-TAX_DED_Q3
                                   + WA_MON_TAX_COMP-TDEDT.
        WA_SUM_TAB-TAX_DEP_Q3 = WA_SUM_TAB-TAX_DEP_Q3
                                   + TAX_DEPOSITED.
        LOOP AT ECC_TAB. " WHERE BACK+0(1) = 4.
          CASE ECC_TAB-BACK+0(1).
            WHEN '3'.
              COUNTER3 = COUNTER3 + 1.
              CASE COUNTER3.
                WHEN '1'.
                  WA_SUM_TAB-ACKNO_Q3_1 = ECC_TAB-BACK+2.
                WHEN '2'.
                  WA_SUM_TAB-ACKNO_Q3_2 = ECC_TAB-BACK+2.
                WHEN '3'.
                  WA_SUM_TAB-ACKNO_Q3_3 = ECC_TAB-BACK+2.
                WHEN '4'.
                  WA_SUM_TAB-ACKNO_Q3_4 = ECC_TAB-BACK+2.
              ENDCASE.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDIF.
    CLEAR: COUNTER1, COUNTER2, COUNTER3, COUNTER4.
    CLEAR WA_MON_TAX_COMP.
  ENDLOOP.


  IF SY-SUBRC = 0.
    APPEND WA_SUM_TAB TO P_SUM_TAB.
  ENDIF.
ENDFORM.                    " FILL_SUM_TAB
*&---------------------------------------------------------------------*
*&      Form  GET_TAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P0001  text
*      -->P_P0185  text
*      -->P_WA_MON_TAX_COMP_FPBEG  text
*      -->P_WA_MON_TAX_COMP_FPEND  text
*      -->P_P_TANR  text
*----------------------------------------------------------------------*
FORM GET_TAN  TABLES   P_P0001 STRUCTURE P0001
                       P_P0185 STRUCTURE P0185
              USING    P_WA_MON_TAX_COMP_FPBEG
                       P_WA_MON_TAX_COMP_FPEND
                       P_WA_MON_TAX_COMP_PAYDT
                       P_P_TANR
                       P_P_MULTIPLE_F16
                       P_P_WA_TMP_COCD_BUKRS
              CHANGING P_TANNO.

  DATA: BEGIN OF ECC_TAB OCCURS 10,
         BACK(40) TYPE C,
        END OF ECC_TAB.
  DATA: WA_P0001 TYPE P0001,
        LEN      TYPE I.
  DATA: TMP_P0001 TYPE TABLE OF P0001 ,
        TMP_P0185 TYPE TABLE OF P0185 WITH HEADER LINE.

  IF P_P_TANR = ' '.

    CLEAR: TMP_P0001[], WA_P0001.
    LOOP AT P_P0001 INTO WA_P0001 WHERE BEGDA LE P_WA_MON_TAX_COMP_PAYDT
                                  AND ENDDA GE P_WA_MON_TAX_COMP_PAYDT
                                  .
      MOVE-CORRESPONDING WA_P0001 TO PME01.
      EXIT.
    ENDLOOP.
*    ENDIF.


    CLEAR ECC_TAB.
    REFRESH ECC_TAB.
    CALL FUNCTION 'HR_FEATURE_BACKTABLE'
        EXPORTING
             FEATURE                     = '40ECC'
             STRUC_CONTENT               = PME01
             KIND_OF_ERROR               = '3'
        TABLES
             BACK                        = ECC_TAB
*      CHANGING
*           STATUS                      =
       EXCEPTIONS
*           DUMMY                       = 1
*           ERROR_OPERATION             = 2
*           NO_BACKVALUE                = 3
*           FEATURE_NOT_GENERATED       = 4
*           INVALID_SIGN_IN_FUNID       = 5
*           TAB_IN_REPORT_FIELD_IN_PE03 = 6
            OTHERS                      = 0
             .
    IF SY-SUBRC = 0.                                        "#EC *
      CLEAR ECC_TAB.
      LOOP AT ECC_TAB.

        CASE SY-TABIX.
          WHEN 3.
            P_TANNO = ECC_TAB-BACK+0(15).
        ENDCASE.

      ENDLOOP.
    ENDIF.

  ELSEIF P_P_TANR = 'X'.  " IF TAN switch is active

    LOOP AT P_P0185 WHERE SUBTY = '05'
                   AND BEGDA LE P_WA_MON_TAX_COMP_PAYDT
                   AND ENDDA GE P_WA_MON_TAX_COMP_PAYDT.
      MOVE P_P0185-ICNUM TO P_TANNO.
      EXIT.
    ENDLOOP.
  ENDIF.



ENDFORM.                    " GET_TAN
*&---------------------------------------------------------------------*
*&      Form  READ_CHALLAN_CLUSTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TANNO  text
*      -->P_P_PERNR_PERNR  text
*      -->P_WA_MON_TAX_COMP_FPBEG  text
*      -->P_WA_MON_TAX_COMP_FPEND  text
*----------------------------------------------------------------------*
FORM READ_CHALLAN_CLUSTER TABLES P_P_P4DEDT STRUCTURE P4DEDT
                                 P_P_P4CHLN STRUCTURE P4CHLN
                           USING    P_TANNO
                                    P_P_PERNR_PERNR
                                    P_WA_MON_TAX_COMP_FPBEG
                                    P_WA_MON_TAX_COMP_FPEND
                                    P_WA_MON_TAX_COMP_PENDDA.
*                           CHANGING p_tax_deposited.
  DATA: I2_KEY TYPE TS_I2_KEY.
  DATA: I1_KEY TYPE TS_I1_KEY.
  DATA: WA_P4CHLN TYPE PINCHL.

  CLEAR P4DEDT.
  REFRESH P4DEDT.
  I2_KEY-TANNO = P_TANNO.
  I2_KEY-PERNR = P_P_PERNR_PERNR.
  I2_KEY-FPBEG = P_WA_MON_TAX_COMP_FPBEG.
  I2_KEY-FPEND = P_WA_MON_TAX_COMP_FPEND.
  I2_KEY-IDTY  = ' '.
  RP-INIT-BUFFER.
  RP-REF-C4-I2.
  RP-REF-C4-I2-O.
  RP-IMP-C4-I2.
  IF RP_IMP_I2_SUBRC = 0.

    LOOP AT P4DEDT.
      MOVE-CORRESPONDING P4DEDT TO P4DEDT_TAB.
      APPEND P4DEDT_TAB.
    ENDLOOP.

  ENDIF.


  DO.
    I2_KEY-TANNO = P_TANNO.
    I2_KEY-PERNR = P_P_PERNR_PERNR.
    I2_KEY-FPBEG = P_WA_MON_TAX_COMP_FPBEG.
    I2_KEY-FPEND = P_WA_MON_TAX_COMP_FPEND.
    I2_KEY-IDTY  = I2_KEY-IDTY + 1 .
    RP-INIT-BUFFER.
    RP-REF-C4-I2.
    RP-REF-C4-I2-O.
    RP-IMP-C4-I2.
    IF RP_IMP_I2_SUBRC = 0.
      LOOP AT P4DEDT.
        MOVE-CORRESPONDING P4DEDT TO P4DEDT_TAB.
        APPEND P4DEDT_TAB.
      ENDLOOP.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  I1_KEY-TANNO =  P_TANNO.
  I1_KEY-FPEND =  P_WA_MON_TAX_COMP_PENDDA.

  RP-INIT-BUFFER.
  RP-REF-C4-I1.
  RP-REF-C4-I1-O.
  RP-IMP-C4-I1.
  IF RP_IMP_I1_SUBRC = 0.
    LOOP AT P4CHLN INTO WA_P4CHLN.
      APPEND WA_P4CHLN TO P_P_P4CHLN.
    ENDLOOP.
  ENDIF.

  LOOP AT P4CHLN.
    MOVE-CORRESPONDING P4CHLN TO P4CHLN_TAB.
    APPEND P4CHLN_TAB.
  ENDLOOP.

ENDFORM.                    " READ_CHALLAN_CLUSTER
*&---------------------------------------------------------------------*
*&      Form  READ_CHALLAN_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MON_TAX_COMP  text
*      -->P_CHALLAN_DET  text
*      -->P_WA_TMP_COCD_CNTR2  text
*      -->P_WA_TMP_COCD_BUKRS  text
*      -->P_PERNR_PERNR  text
*      -->P_HD_TAB_TANNO  text
*      -->P_YEAR  text
*      -->P_TANR  text
*      -->P_MULTIPLE_F16  text
*----------------------------------------------------------------------*
FORM READ_CHALLAN_DETAILS  TABLES   P_MON_TAX_COMP LIKE MON_TAX_COMP
                                    P_CHALLAN_DET LIKE CH_DET
                                    P_P4DEDT_TAB
                                    P_P4CHLN_TAB
                                    P_P0001 STRUCTURE P0001
                                    P_P0185 STRUCTURE P0185
                           USING    P_WA_TMP_COCD_CNTR2
                                    P_WA_TMP_COCD_BUKRS
                                    P_PERNR_PERNR
                                    P_YEAR
                                    P_TANR
                                    P_MULTIPLE_F16.
  DATA: WA_MON_TAX_COMP TYPE MON_TAX,
        TMP_MON_TAX TYPE TABLE OF MON_TAX,
        TANNO TYPE PIN_TANNO,
*        P4DEDT TYPE PINDD,
        WA_P4DEDT LIKE PINDD,
*        P4CHLN TYPE PINCHL,
        WA_P4CHLN LIKE PINCHL,
        TMP_P4DEDT_TAB TYPE TABLE OF PINDD,
        TMP_P4CHLN_TAB TYPE TABLE OF PINCHL,
        TAX_DEPOSITED TYPE PC207-BETRG,
        RATIO TYPE PC207-BETRG.
  DATA: WA_CHALLAN_DET LIKE LINE OF CH_DET.
  CLEAR: WA_MON_TAX_COMP,TMP_MON_TAX.
  REFRESH: TMP_MON_TAX[].
  TMP_MON_TAX[] = P_MON_TAX_COMP[].
  TMP_P4DEDT_TAB[] = P_P4DEDT_TAB[].
  TMP_P4CHLN_TAB[] = P_P4CHLN_TAB[].
  DATA: WA_P0001 TYPE P0001.  "Anees

  SORT TMP_MON_TAX BY FPPER FPBEG FPEND PAYTY PAYID ASCENDING.
  LOOP AT TMP_MON_TAX INTO WA_MON_TAX_COMP
       WHERE CNTR2 = P_WA_TMP_COCD_CNTR2
         AND PERNR = P_PERNR_PERNR.

    CLEAR: WA_P4CHLN , WA_P4DEDT.
*    REFRESH: P_P4CHLN[] , P_P4DEDT[] .
    PERFORM GET_TAN TABLES P_P0001 P_P0185
                    USING  WA_MON_TAX_COMP-FPBEG
                           WA_MON_TAX_COMP-FPEND
                           WA_MON_TAX_COMP-PAYDT
                           P_TANR
                           P_MULTIPLE_F16
                           P_WA_TMP_COCD_BUKRS
                    CHANGING TANNO.

    LOOP AT TMP_P4DEDT_TAB INTO WA_P4DEDT
                        WHERE TANNO = TANNO
                          AND PERNR = P_PERNR_PERNR
                          AND PAYTY = WA_MON_TAX_COMP-PAYTY
                          AND PAYID = WA_MON_TAX_COMP-PAYID
                          AND FPBEG = WA_MON_TAX_COMP-FPBEG
                          AND FPEND = WA_MON_TAX_COMP-FPEND.

      WA_CHALLAN_DET-CNTR2 = P_WA_TMP_COCD_CNTR2.
      WA_CHALLAN_DET-PERNR = P_PERNR_PERNR.
      WA_CHALLAN_DET-FPPER = WA_MON_TAX_COMP-FPPER.
      WA_CHALLAN_DET-INPER = WA_MON_TAX_COMP-INPER.
      WA_CHALLAN_DET-FPBEG = WA_MON_TAX_COMP-FPBEG.
      WA_CHALLAN_DET-FPEND = WA_MON_TAX_COMP-FPEND.
      WA_CHALLAN_DET-IPEND = WA_MON_TAX_COMP-IPEND.
      WA_CHALLAN_DET-PAYTY = WA_P4DEDT-PAYTY.
      WA_CHALLAN_DET-PAYID = WA_P4DEDT-PAYID.
      WA_CHALLAN_DET-CHLNO = WA_P4DEDT-CHLNO.
      WA_CHALLAN_DET-VOUNO = WA_P4DEDT-VOUNO.
      WA_CHALLAN_DET-PANNO = WA_P4DEDT-PANNO.
      WA_CHALLAN_DET-CHDAT = WA_P4DEDT-CHDAT.
*      WA_CHALLAN_DET-BETRG = WA_P4DEDT-BETRG.
*      WA_CHALLAN_DET-AMONT = WA_P4DEDT-AMONT.
      IF NOT ( WA_P4DEDT-BETRG IS INITIAL ).
        WA_CHALLAN_DET-AMONT = WA_P4DEDT-BETRG.
      ELSE.
        WA_CHALLAN_DET-AMONT = WA_P4DEDT-AMONT.
      ENDIF.
      IF NOT WA_MON_TAX_COMP-TDEDT IS INITIAL.
        RATIO = ( WA_CHALLAN_DET-AMONT / WA_MON_TAX_COMP-TDEDT ) * 100000.
      ENDIF.

      WA_CHALLAN_DET-MNTAX = ( WA_MON_TAX_COMP-MTPAY * RATIO ) / 100000.
      WA_CHALLAN_DET-MNEDU = ( WA_MON_TAX_COMP-MECSS * RATIO ) /
100000 + ( WA_MON_TAX_COMP-MHECSS * RATIO ) / 100000.
      WA_CHALLAN_DET-MNINC = ( WA_MON_TAX_COMP-MTINC * RATIO ) / 100000.
      WA_CHALLAN_DET-MNSUR = ( WA_MON_TAX_COMP-MTSUR * RATIO ) / 100000.

      READ TABLE TMP_P4CHLN_TAB INTO WA_P4CHLN
                      WITH KEY TANNO =  TANNO
                               CHLNO =  WA_P4DEDT-CHLNO.
      IF SY-SUBRC = 0.
        WA_CHALLAN_DET-CHKNO = WA_P4CHLN-CHKNO.
        WA_CHALLAN_DET-BRCOD = WA_P4CHLN-BRCOD.
      ELSE.
        "Anees
        IF P_WA_TMP_COCD_BUKRS = '1000'.
          CLEAR: WA_P0001.
          LOOP AT P_P0001 INTO WA_P0001 WHERE PERNR = WA_P4DEDT-PERNR
                                          AND BEGDA <= WA_MON_TAX_COMP-FPBEG
                                          AND ENDDA >= WA_MON_TAX_COMP-FPEND.
            CASE WA_P0001-BTRTL.
              WHEN 'IN01'.
                WA_CHALLAN_DET-BRCOD = 0510308.
              WHEN 'TH01'.
                WA_CHALLAN_DET-BRCOD = 0510308.
              WHEN 'TU01'.
                WA_CHALLAN_DET-BRCOD = 0510308.
            ENDCASE.
            EXIT.
          ENDLOOP.
        ELSEIF P_WA_TMP_COCD_BUKRS = '2000'.
          WA_CHALLAN_DET-BRCOD = 0004329.
        ENDIF.
        "End
      ENDIF.

      APPEND WA_CHALLAN_DET TO P_CHALLAN_DET.
      CLEAR WA_CHALLAN_DET.

    ENDLOOP.
    IF SY-SUBRC NE 0.
      WA_CHALLAN_DET-CNTR2 = P_WA_TMP_COCD_CNTR2.
      WA_CHALLAN_DET-PERNR = P_PERNR_PERNR.
      WA_CHALLAN_DET-FPPER = WA_MON_TAX_COMP-FPPER.
      WA_CHALLAN_DET-INPER = WA_MON_TAX_COMP-INPER.
      WA_CHALLAN_DET-FPBEG = WA_MON_TAX_COMP-FPBEG.
      WA_CHALLAN_DET-FPEND = WA_MON_TAX_COMP-FPEND.
      WA_CHALLAN_DET-IPEND = WA_MON_TAX_COMP-IPEND.
      WA_CHALLAN_DET-PAYTY = WA_MON_TAX_COMP-PAYTY.
      WA_CHALLAN_DET-PAYID = WA_MON_TAX_COMP-PAYID.
      APPEND WA_CHALLAN_DET TO P_CHALLAN_DET.
      CLEAR WA_CHALLAN_DET.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " READ_CHALLAN_DETAILS
*&---------------------------------------------------------------------*
*&      Form  UPLD_CH_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UPD_FILE  text
*      -->P_PATHNAME  text
*      -->P_FTYPE  text
*      -->P_FILE_LENGTH  text
*----------------------------------------------------------------------*
FORM UPLD_CH_DETAILS  TABLES   P_UPD_FILE
                      USING    P_PATHNAME
                               P_FTYPE
                               P_FILE_LENGTH.

  DATA: FILENAME TYPE STRING.

  P_FTYPE = 'ASC'.
  IF P_PATHNAME NE ' '.
    FILENAME = P_PATHNAME.

    CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            FILENAME                     = FILENAME
           FILETYPE                      = P_FTYPE
           HAS_FIELD_SEPARATOR           = 'X'
*        DAT_MODE                      = 'X'
          IMPORTING
            FILELENGTH                    = P_FILE_LENGTH
         TABLES
            DATA_TAB                      = EXCEL_TAB
         EXCEPTIONS
           FILE_OPEN_ERROR               = 1
           FILE_READ_ERROR               = 2
           NO_BATCH                      = 3
           GUI_REFUSE_FILETRANSFER       = 4
           INVALID_TYPE                  = 5
           NO_AUTHORITY                  = 6
           UNKNOWN_ERROR                 = 7
           BAD_DATA_FORMAT               = 8
           HEADER_NOT_ALLOWED            = 9
           SEPARATOR_NOT_ALLOWED         = 10
           HEADER_TOO_LONG               = 11
           UNKNOWN_DP_ERROR              = 12
           ACCESS_DENIED                 = 13
           DP_OUT_OF_MEMORY              = 14
           DISK_FULL                     = 15
           DP_TIMEOUT                    = 16
           OTHERS                        = 17.
    IF SY-SUBRC EQ 0.
*      QUAT = 4.
    ELSE.
      MESSAGE W281(HRPADIN01) WITH P_PATHNAME.
*      QUAT = 1.
    ENDIF.


  ENDIF.


ENDFORM.                    " UPLD_CH_DETAILS


*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_MODULE .

  DATA: LANG LIKE SY-LANGU VALUE 'E',
        PAGE_NO TYPE I,
        TAX_PERK(10),
        EERECVR(9),
        VAL_PERK(10),
        VAL_F12BA LIKE PC207-BETRG,
        PROF_F12BA LIKE PC207-BETRG,
        SAL_SEC17 LIKE PC207-BETRG,
        TOT_VAL_PERK LIKE PC207-BETRG,
        TOT_EERECVR LIKE PC207-BETRG,
        TOT_TAX_PERK LIKE PC207-BETRG,
        SALARIES_WO_PERK LIKE PC207-BETRG,
        F12BA_BALANCE LIKE PC207-BETRG,
        F12BA_STD_DED LIKE PC207-BETRG,
        SEC10_TAB_AMOUNT(15) TYPE C,
        ENT_ALL LIKE PC207-BETRG,
        OTH_INCOME_S(15) TYPE C,
        IFOS_TAB_AMOUNT(15) TYPE C,
        PR_EMP_TOTAL LIKE PC207-BETRG, " Total of tax paid by the
                                       " present employer to the tax
                                       " office.
        OF_TAX_AMT LIKE PC207-BETRG,

        NET_TAX_PAYABLE_S(15) TYPE C,
        GROSS_TAB_AMOUNT(15) TYPE C,
        PERK_TAB_AMOUNT(15) TYPE C,
        VAL_PERK_S(15) TYPE C,
        EERECVR_S(15) TYPE C,
        TAX_PERK_S(15) TYPE C,
        TOT_VAL_PERK_S(15) TYPE C,
        TOT_EERECVR_S(15) TYPE C,
        TOT_TAX_PERK_S(15) TYPE C.


  DATA: TAX_DED_SUM LIKE PC207-BETRG,
        TAX_DEP_SUM LIKE PC207-BETRG,
        AMT_IN_WORDS(130).

  DATA: BEGIN OF TEMP_PAYMENTS OCCURS 10.
          INCLUDE STRUCTURE PINBK.
  DATA: END OF TEMP_PAYMENTS.
  DATA: OUT TYPE I.
  DATA: TEMP_PSCRIPT TYPE C.
  DATA : CNTR TYPE I.
  DATA : J TYPE I.
  DATA : TOTAL_TAX LIKE PC207-BETRG.
  DATA : BEGIN OF TEMP_S88 OCCURS 0.
          INCLUDE STRUCTURE INT_S88.
  DATA : END OF TEMP_S88.
  DATA : CONT_AMT(20),
         DED_AMT(20),
         SEQ TYPE I,
         SLNO(3) TYPE N VALUE 1.

  DATA : FPMON  LIKE T549Q-PABRP,
         CONMT_EPF(20),
         DEDMT_EPF(20).

  DATA: SNAME LIKE T596F-SNAME,
        ENDDATE LIKE SY-DATUM.

  DATA: FORMNAME(30) TYPE C,
        FORMNAME1(30) TYPE C,
        FORMNAME2(30) TYPE C,
        DOC_TYPE(12),
        FORM_DATE LIKE SY-DATUM,
*        DISP_FLG_LOT TYPE I VALUE 1,
        CNTR_FTAB TYPE I,
        STATE LIKE T005U-BEZEI,
        FIN_START(4) TYPE C,
        FIN_END(4) TYPE C,
        BANKNAME(60) TYPE C.

  DATA:  FORM_16(1) TYPE C,
        HTEXT_SWITCH TYPE C,
        SY_TABIX TYPE SY-TABIX,
        FLEXTXT TYPE CHAR20,
        ASSM_END(4) TYPE C.

  DATA: BEGIN OF IT_FINAL_TAB OCCURS 10.
          INCLUDE STRUCTURE HD_TAB.
          INCLUDE STRUCTURE MAIN_TAB.
  DATA: END OF IT_FINAL_TAB.

  IF ORG = 'X'.
    DOC_TYPE = 'ORIGINAL'.
  ELSE.
    DOC_TYPE = 'NON-ORIGINAL'.
  ENDIF.

  FIN_START = PBEGDA(4).
  FIN_END   = FIN_START + 1.

  ENDDATE = SY-DATUM.
  FORM_DATE = '99991231'.
  FORM_DATE+0(4) = YEAR.

  SNAME = '40TAX016'.
  PERFORM GET_LAYOUT_SET USING SNAME FORM_DATE
                         CHANGING FORMNAME.
  PERFORM GET_OUTPUT_TYPE USING    FORM_DATE
                          CHANGING FORMNAME
                                   TEMP_PSCRIPT.
  IF DISP_FLG_LOT GE 1.
    P_SCRIPT = TEMP_PSCRIPT.
    IF P_SCRIPT EQ 'X'.
      CLEAR P_PDF.
    ELSE.
      P_PDF = 'X'.
    ENDIF.
  ENDIF.

  CLEAR SNAME.
  CLEAR FORMNAME.
  IT_FINAL_TAB[] = FINAL_TAB[].
  DESCRIBE TABLE FINAL_TAB LINES CNTR_FTAB.
  IF P_SCRIPT = GC_X.
********END OF PDF CODING**********

    IF SY-BATCH = 'X' AND ( NOT LAYOUT IS INITIAL OR
                            NOT LAYOUT1 IS INITIAL OR
                            NOT LAYOUT2 IS INITIAL OR
                            NOT LAYOUT3 IS INITIAL ).
      DISP_FLG_LOT = -1.
    ENDIF.



    LOOP AT FINAL_TAB.

      CLEAR: TAX_DEDUCTED_WRDS,
      TAX_DED_SUM,
      TAX_DEP_SUM.

* CLEARING VARIABLES OF FORM12BA
      CLEAR VAL_F12BA.CLEAR PROF_F12BA.CLEAR SAL_SEC17.CLEAR TOT_VAL_PERK.
      CLEAR TOT_EERECVR.CLEAR TOT_TAX_PERK.

      SNAME = '40TAX16A'.
      CLEAR FORM_16.                        "Print Form 16AA
      SY_TABIX = SY-TABIX.
      IF INFOS IS INITIAL.
        IF FINAL_TAB-BALANCE < 150000 AND FINAL_TAB-BUS_PROF <= 0 AND
           FINAL_TAB-TDS_IFOS <= 0.
          IF DISP_FLG_LOT < 1 AND LAYOUT3 NE ' '.
            FORMNAME = LAYOUT3.
          ELSE.
            PERFORM GET_LAYOUT_SET USING SNAME FORM_DATE
                                   CHANGING FORMNAME.
          ENDIF.
        ELSE.
          FORM_16 = 'X'.                  "Print Form 16
          IF DISP_FLG_LOT < 1 AND LAYOUT NE ' '.
            FORMNAME = LAYOUT.
          ELSE.
            SNAME = '40TAX016'.
            PERFORM GET_LAYOUT_SET USING SNAME FORM_DATE
                                   CHANGING FORMNAME.
          ENDIF.
        ENDIF.
      ELSE.
        FORM_16 = 'X'.                     "Print Form 16
        IF DISP_FLG_LOT < 1 AND LAYOUT NE ' '.
          FORMNAME = LAYOUT.
        ELSE.
          SNAME = '40TAX016'.
          PERFORM GET_LAYOUT_SET USING SNAME FORM_DATE
                                 CHANGING FORMNAME.
        ENDIF.
      ENDIF.


      PERFORM OPEN_FORM USING LANG FORMNAME.

*      PERFORM START_FORM USING FORMNAME LANG 'MAIN'.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'doc_type' DOC_TYPE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'fybegda' FINAL_TAB-F16_BEGDA.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'fyendda' FINAL_TAB-F16_ENDDA.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'assm_start' PENDDA(4).

      ASSM_END = PENDDA(4) + 1.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'assm_end' ASSM_END.

* Get employer address
      CLEAR SADR.
      CLEAR STATE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'emp_no' FLEXTXT .
      PERFORM ADDRESS USING COMP_CD ADDR1_VAL.
*      PERFORM ER_ADDRESS USING FINAL_TAB-TANNO.
      PERFORM ER_ADDRESS USING FINAL_TAB-TANNO
                           CHANGING SADR
                                    SADR_CIT
                                    ADDR1_VAL
                                    ADDR1_VAL_CIT  .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ename1'(010) ADDR1_VAL-NAME1 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ename2'(016) SADR-NAME2 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Estreet'(024) SADR-STRAS .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Epobox'(025) SADR-PFACH .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Epocode'(026) SADR-PSTLZ+0(6) .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ecity'(027) SADR-ORT01 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Edistrict'(028) SADR-ORT02 .
      IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
        PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                       CHANGING STATE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'Estate'(035) STATE.
      ENDIF.
*      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITname1'(010) ADDR1_VAL_CIT-NAME1 .
*      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITname2'(016) SADR_CIT-NAME2 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITstreet' SADR_CIT-STRAS .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITpobox' SADR_CIT-PFACH .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITpocode' SADR_CIT-PSTLZ+0(6) .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITcity' SADR_CIT-ORT01 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'CITdistrict' SADR_CIT-ORT02 .
      IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
        PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                       CHANGING STATE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'Estate'(035) STATE.
      ENDIF.
      PERFORM START_FORM USING FORMNAME LANG 'MAIN'.

* Get TDS Circle address
      CLEAR SADR.
      CLEAR STATE.
      PERFORM GET_ADDRESS USING FINAL_TAB-ADDRS.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cname1'(036) SADR-NAME1 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cname2'(063) SADR-NAME2 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cstreet'(064) SADR-STRAS .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cpobox'(065) SADR-PFACH .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cpocode'(066) SADR-PSTLZ+0(6) .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ccity'(067) SADR-ORT01 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Cdistrict'(068) SADR-ORT02 .
      IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
        PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                         CHANGING STATE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'Cstate'(069) STATE.
      ENDIF.

* Employer info
      PERFORM CONVERT_TO_SCRIPTVAR USING 'panno' FINAL_TAB-PANNO.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tanno' FINAL_TAB-TANNO.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'girno' FINAL_TAB-GIRNO.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tdscr' FINAL_TAB-TDSCR.

* Employee info
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ename' FINAL_TAB-ENAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'design' FINAL_TAB-POSITION.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'empno' FINAL_TAB-PERNR.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'icnum' FINAL_TAB-ICNUM.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'gender' FINAL_TAB-GENDER.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'dob' FINAL_TAB-DOB.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ffname' FINAL_TAB-FFNAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'flname' FINAL_TAB-FLNAME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'hnumb' FINAL_TAB-HNUMB.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'locality' FINAL_TAB-LOCALITY.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'pin' FINAL_TAB-PIN.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'city' FINAL_TAB-CITY.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'country' FINAL_TAB-COUNTRY.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'teln' FINAL_TAB-TELN.

      CLEAR SUM_TAB.
      READ TABLE SUM_TAB WITH KEY CNTR2 = FINAL_TAB-CNTR2
                                  PERNR = FINAL_TAB-PERNR.
      IF SY-SUBRC = 0.

        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_1' SUM_TAB-ACKNO_Q1_1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_1' SUM_TAB-ACKNO_Q2_1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_1' SUM_TAB-ACKNO_Q3_1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_1' SUM_TAB-ACKNO_Q4_1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_2' SUM_TAB-ACKNO_Q1_2.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_2' SUM_TAB-ACKNO_Q2_2.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_2' SUM_TAB-ACKNO_Q3_2.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_2' SUM_TAB-ACKNO_Q4_2.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_3' SUM_TAB-ACKNO_Q1_3.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_3' SUM_TAB-ACKNO_Q2_3.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_3' SUM_TAB-ACKNO_Q3_3.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_3' SUM_TAB-ACKNO_Q4_3.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_4' SUM_TAB-ACKNO_Q1_4.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_4' SUM_TAB-ACKNO_Q2_4.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_4' SUM_TAB-ACKNO_Q3_4.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_4' SUM_TAB-ACKNO_Q4_4.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q1' SUM_TAB-TAX_DED_Q1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q2' SUM_TAB-TAX_DED_Q2.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q3' SUM_TAB-TAX_DED_Q3.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q4' SUM_TAB-TAX_DED_Q4.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q1' SUM_TAB-TAX_DEP_Q1.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q2' SUM_TAB-TAX_DEP_Q2.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q3' SUM_TAB-TAX_DEP_Q3.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q4' SUM_TAB-TAX_DEP_Q4.

        TAX_DED_SUM = SUM_TAB-TAX_DED_Q1 + SUM_TAB-TAX_DED_Q2 +
                      SUM_TAB-TAX_DED_Q3 + SUM_TAB-TAX_DED_Q4.

        TAX_DEP_SUM = SUM_TAB-TAX_DEP_Q1 + SUM_TAB-TAX_DEP_Q2 +
                      SUM_TAB-TAX_DEP_Q3 + SUM_TAB-TAX_DEP_Q4.
      ELSE.

        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_1' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_1' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_1' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_1' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_2' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_2' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_2' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_2' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_3' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_3' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_3' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_3' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq1_4' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq2_4' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq3_4' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'acknoq4_4' ''.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q1' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q2' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q3' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_q4' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q1' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q2' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q3' 0.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_q4' 0.

        TAX_DED_SUM = 0.

        TAX_DEP_SUM = 0.

      ENDIF.

      PERFORM CONVERT_INR_TO_WORDS USING TAX_DEP_SUM
                                   CHANGING TAX_DEDUCTED_WRDS.
      REPLACE 'Rupees' WITH SPACE INTO TAX_DEDUCTED_WRDS.
      CONDENSE TAX_DEDUCTED_WRDS.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_ded_sum' TAX_DED_SUM.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_dep_sum' TAX_DEP_SUM.

      PERFORM WRITE_FORM USING 'EMP' 'APPEND' 'BODY' 'EMPNAME'.

      PERFORM WRITE_FORM USING 'DETAILS1' 'APPEND' 'BODY' 'MAIN'.

* Compute for sl. no. 1(b) of form 16
      LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                             AND CNTR2 = FINAL_TAB-CNTR2
                             AND EVCLS_SPEC NE 17 .
        VAL_F12BA = VAL_F12BA + FORM12BA_TAB-TAX_PERK .
      ENDLOOP.

*   Value of perq u/s 17(2) from Prev Emp added.
      VAL_F12BA = VAL_F12BA + FINAL_TAB-PETD_S172.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'VAL_F12BA' VAL_F12BA.

* Compute for sl. no. 1(c) of form 16
      LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR
                             AND CNTR2 = FINAL_TAB-CNTR2
                             AND EVCLS_SPEC = 17 .
        PROF_F12BA = PROF_F12BA + FORM12BA_TAB-TAX_PERK.
      ENDLOOP.

*   Value of Profits u/s 17(3) from Prev Emp added.
      PROF_F12BA = PROF_F12BA + FINAL_TAB-PETD_S173.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'PROF_F12BA' PROF_F12BA.

* Compute for sl. no. 1(a) of form 16
      SAL_SEC17 = FINAL_TAB-GROSS_SAL - ( PROF_F12BA + VAL_F12BA ).
      PERFORM CONVERT_TO_SCRIPTVAR USING 'SAL_SEC17' SAL_SEC17.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'gross_sal' FINAL_TAB-GROSS_SAL.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec10_all' FINAL_TAB-SEC10_ALL.


      PERFORM WRITE_FORM USING 'DETAILS2' 'APPEND' 'BODY' 'MAIN'.

      LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2.
        IF SEC10_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
        CLEAR SEC10_TAB_AMOUNT.
        PERFORM SIGN_ALGMNT USING SEC10_TAB-AMOUNT
                             CHANGING SEC10_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' SEC10_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'SEC10_COMPONENTS' 'APPEND' 'BODY' 'MAIN'
   .
      ENDLOOP.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'balance' FINAL_TAB-BALANCE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'std_ded' FINAL_TAB-STD_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'etall' ENT_ALL.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'ptax' FINAL_TAB-PTAX.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'aggr_ded' FINAL_TAB-AGGR_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'salaries' FINAL_TAB-SALARIES.

      CLEAR OTH_INCOME_S.
      PERFORM SIGN_ALGMNT USING FINAL_TAB-OTH_INCOME
                              CHANGING OTH_INCOME_S.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'oth_income'
                                        OTH_INCOME_S.
      PERFORM CONVERT_TO_SCRIPTVAR USING  'DEDN_S24' FINAL_TAB-DEDN_S24.
      PERFORM CONVERT_TO_SCRIPTVAR USING  'BUS_PROF' FINAL_TAB-BUS_PROF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'bus_prof' FINAL_TAB-BUS_PROF.
      PERFORM WRITE_FORM USING 'DETAILS13' 'APPEND' 'BODY' 'MAIN'.

      LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2.
        IF IFOS_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
        CLEAR IFOS_TAB_AMOUNT.
        PERFORM SIGN_ALGMNT USING IFOS_TAB-AMOUNT
                             CHANGING IFOS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' IFOS_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'IFOS_COMPONENTS' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'gross_tot_income'
                                FINAL_TAB-GROSS_TOT_INCOME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec80_ded' FINAL_TAB-SEC80_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tot_income'
                                FINAL_TAB-TOT_INCOME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_tot_income'
                           FINAL_TAB-TAX_TOT_INCOME.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'epf' FINAL_TAB-EPF_TOT.
      READ TABLE INT_S88 WITH KEY PERNR = FINAL_TAB-PERNR
                           CNTR2 = FINAL_TAB-CNTR2.
      IF SY-SUBRC = 0.
        CLEAR : CONMT_EPF, DEDMT_EPF.
      ELSE.
*        conmt_epf = FINAL_TAB-EPF_TOT.
        READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                    SBSEC = '15' SBDIV = '01' CNTR2 = FINAL_TAB-CNTR2.
        IF SY-SUBRC EQ 0.
          CONMT_EPF = INT_S80-COAMT.
          DEDMT_EPF = INT_S80-DEDMT.
        ENDIF.
      ENDIF.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt_epf' CONMT_EPF.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt_epf' DEDMT_EPF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88_ded' FINAL_TAB-SEC88_DED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88b_ded' FINAL_TAB-SEC88B_DED
            .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88c_ded' FINAL_TAB-SEC88C_DED
            .

      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec88d_ded' FINAL_TAB-SEC88D_DED
            .

      IF FORM_16 IS INITIAL.
        FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF = FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF
                                               - FINAL_TAB-EDU_CESS.
      ENDIF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_bef_relief' FINAL_TAB-TAX_PAYABLE_BEFORE_RELIEF.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'chapVI_ded'
                               FINAL_TAB-CHAPVI_DED.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'surch_amt'
                            FINAL_TAB-SURCHG.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'educ_cess'
                              FINAL_TAB-EDU_CESS.


      PERFORM CONVERT_TO_SCRIPTVAR USING 'sec89_relief'
                                FINAL_TAB-SEC89_RELIEF.

      PERFORM WRITE_FORM USING 'DETAILS14' 'APPEND' 'BODY' 'MAIN'.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_payable'
                              FINAL_TAB-TAX_PAYABLE.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_deducted'
                             FINAL_TAB-TAX_DEDUCTED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'employer_tax'
                             FINAL_TAB-TAX_PAID_EMPLOYER.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tot_tax_deducted'
                             FINAL_TAB-TOT_TAX_DEDUCTED.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tax_employer'
                             FINAL_TAB-TAX_PAID_EMPLOYER.

      CLEAR NET_TAX_PAYABLE_S.                              "PKT1148133
      PERFORM SIGN_ALGMNT USING FINAL_TAB-NET_TAX_PAYABLE
                              CHANGING NET_TAX_PAYABLE_S.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'net_tax_payable'
                                             NET_TAX_PAYABLE_S.


      TOTAL_TAX = FINAL_TAB-TAX_DEDUCTED + FINAL_TAB-TAX_PAID_EMPLOYER."MDSNT927906

      PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_TAX' TOTAL_TAX.

      CLEAR I.
      CLEAR S80_TOTAL.
      CLEAR CONT_AMT.
      CLEAR DED_AMT.
      CLEAR CNTR.
      CLEAR SEQ.
      CNTR = 0.
      LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR AND
                           CNTR2 = FINAL_TAB-CNTR2.
        SEQ = SEQ + 1.
      ENDLOOP.
      I = 1.
      CNTR = 1.
      LOOP AT INT_S88 WHERE PERNR = FINAL_TAB-PERNR AND
                            CNTR2 = FINAL_TAB-CNTR2.
        I = I + 1.
        IF INT_S88-INAMT IS INITIAL.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'invmt' INT_S88-INVMT.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'invmt' INT_S88-INAMT.
        ENDIF.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'itlmt' INT_S88-ITLMT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'qlamt' INT_S88-QLAMT.
        IF CNTR NE SEQ.
          CONT_AMT = ''.
          DED_AMT  = ''.
        ELSE.
          READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                        SBSEC = '15' SBDIV = '01' CNTR2 = INT_S88-CNTR2.
          IF SY-SUBRC EQ 0.
            CONT_AMT  = INT_S80-COAMT.
            CONDENSE CONT_AMT NO-GAPS.
            DED_AMT = INT_S80-DEDMT.
            CONDENSE DED_AMT NO-GAPS.
          ENDIF.
        ENDIF.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' CONT_AMT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' DED_AMT.
        PERFORM WRITE_FORM USING 'SEC88_DED' 'APPEND' 'BODY' 'MAIN'.
        CNTR = CNTR + 1.
      ENDLOOP.

      CLEAR INT_S80.
      READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                  CNTR2 = FINAL_TAB-CNTR2
                                  SBSEC = '01' SBDIV = '01'.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' INT_S80-COAMT.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' INT_S80-DEDMT.
      S80_TOTAL = S80_TOTAL + INT_S80-COAMT.
      PERFORM WRITE_FORM USING 'DETAILS10' 'APPEND' 'BODY' 'MAIN'.

      CLEAR INT_S80.
      READ TABLE INT_S80 WITH KEY PERNR = FINAL_TAB-PERNR
                                  CNTR2 = FINAL_TAB-CNTR2
                                  SBSEC = '17' SBDIV = '01'.
      PERFORM CONVERT_TO_SCRIPTVAR USING '80ccda' INT_S80-COAMT.
      PERFORM CONVERT_TO_SCRIPTVAR USING '80ccdd' INT_S80-DEDMT.
      S80_TOTAL = S80_TOTAL + INT_S80-COAMT.
*   s80ccd_amt = s88_total + s80_total.
*      PERFORM CONVERT_TO_SCRIPTVAR USING 's80ccd' S80CCD_AMT.
      PERFORM WRITE_FORM USING 'DETAILS11' 'APPEND' 'BODY' 'MAIN'.

      J = 0.
*
      IF SNAME = '40TAX016'.
        LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR AND
                                      CNTR2 = FINAL_TAB-CNTR2 AND
                                      SBSEC <> '01' AND SBSEC <> '15' AND SBSEC <> '17'.
          CLEAR TEMP.
          CLEAR TEMP1.
          TEMP = ALPHA+J(1).
          J = J + 1.
          TEMP1 = '('.
          MOVE TEMP TO TEMP1+1(1).
          MOVE ')' TO TEMP1+2(1).
          CONDENSE TEMP1 NO-GAPS.
          MOVE TEMP1 TO INT_S80-SBTDS+0(3).
          PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' INT_S80-COAMT.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'qlamt' INT_S80-QLAMT.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' INT_S80-DEDMT.
          PERFORM WRITE_FORM USING 'SEC80_DED' 'APPEND' 'BODY' 'MAIN'.
        ENDLOOP.
      ELSE.
        IF SNAME = '40TAX16A'.
          LOOP AT INT_S80 WHERE PERNR = FINAL_TAB-PERNR AND
                                        CNTR2 = FINAL_TAB-CNTR2 AND
                                        SBSEC <> '01' AND SBSEC <> '15' AND SBSEC <> '17'.
            CLEAR TEMP.
            CLEAR TEMP1.
            TEMP = ALPHA+J(1).
            J = J + 1.
            TEMP1 = '('.
            MOVE TEMP TO TEMP1+1(1).
            MOVE ')' TO TEMP1+2(1).
            CONDENSE TEMP1 NO-GAPS.
            MOVE TEMP1 TO INT_S80-SBTDS+0(3).
            PERFORM CONVERT_TO_SCRIPTVAR USING 'conmt' INT_S80-COAMT.
            PERFORM CONVERT_TO_SCRIPTVAR USING 'qlamt' INT_S80-QLAMT.
            PERFORM CONVERT_TO_SCRIPTVAR USING 'dedmt' INT_S80-DEDMT.
            PERFORM WRITE_FORM USING 'SEC80_DED' 'APPEND' 'BODY' 'MAIN'.
          ENDLOOP.
        ENDIF.
      ENDIF.

      PERFORM WRITE_FORM USING 'DETAILS3' 'APPEND' 'BODY' 'MAIN'.

      CLEAR TEMP.
      CLEAR TEMP1.
      I = 0.
      TEMP = ALPHA+I(1).
      TEMP1 = '('.
      MOVE TEMP TO TEMP1+1(1).
      MOVE ')' TO TEMP1+2(1).
      CONDENSE TEMP1 NO-GAPS.
      MOVE 'TOTAL' TO TEMP1+4.
      TEMP = ALPHA+0(1).
      MOVE '[(' TO TEMP1+10.
      MOVE TEMP TO TEMP1+12.
      MOVE ') to (' TO TEMP1+13.
      IF I = 0.
        I = 1.
      ENDIF.
      I = I - 1.
      TEMP = ALPHA+I(1).
      MOVE TEMP TO TEMP1+20.
      MOVE ')]' TO TEMP1+21.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'tot_sec88' TEMP1.

      I = I + 1.
      MOVE '[I(' TO TEMP1.
      TEMP = ALPHA+I(1).
      MOVE TEMP TO TEMP1+3.
      CONDENSE TEMP1 NO-GAPS.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'p14_text' TEMP1.

      PERFORM WRITE_FORM USING 'DETAILS4' 'APPEND' 'BODY' 'MAIN'.
*
      PERFORM WRITE_FORM USING 'DETAILS5' 'APPEND' 'BODY' 'MAIN'.

      IF GOVT = 'X'.
        PERFORM WRITE_FORM USING 'DETAILSA' 'APPEND' 'BODY' 'MAIN'.
      ELSE.
        PERFORM WRITE_FORM USING 'DETAILSB' 'APPEND' 'BODY' 'MAIN'.
      ENDIF.

      PERFORM WRITE_FORM USING 'DETAILS7' 'APPEND' 'BODY' 'MAIN'.

      SLNO = 1.

      IF FORM_16 = 'X'.
        COUNTER = 5.
        IF YEAR GE '2010'.
          COUNTER = 9.
        ENDIF.
      ELSE.
        COUNTER = 9.
      ENDIF.

      SORT CH_DET BY CHDAT.
      LOOP AT CH_DET  INTO WA_CH_DET
                     WHERE CNTR2 = FINAL_TAB-CNTR2
                       AND PERNR = FINAL_TAB-PERNR.

        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-208
                               SLNO.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'TDS'
                               WA_CH_DET-MNTAX.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-207
                               WA_CH_DET-MNSUR.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-211
                               WA_CH_DET-MNEDU.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'TotalTax'
                               WA_CH_DET-AMONT.
        PERFORM CONVERT_TO_SCRIPTVAR USING TEXT-210
                               WA_CH_DET-CHKNO.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'BSR'
                               WA_CH_DET-BRCOD.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'c_date'
                               WA_CH_DET-CHDAT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'voucher'
                               WA_CH_DET-CHLNO.
        PERFORM WRITE_FORM USING 'DETAILS8' 'APPEND' 'BODY' 'MAIN'.
        IF GOVT = 'X'.
          PERFORM WRITE_FORM USING 'DETAILSAC' 'APPEND' 'BODY' 'MAIN'.
        ELSE.
          PERFORM WRITE_FORM USING 'DETAILSBC' 'APPEND' 'BODY' 'MAIN'.
        ENDIF.
        SLNO = SLNO + 1.

      ENDLOOP.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'total' TAX_DEP_SUM.
      PERFORM CONVERT_INR_TO_WORDS USING TAX_DEP_SUM
                                   CHANGING AMT_IN_WORDS.
      REPLACE 'Rupees' WITH SPACE INTO AMT_IN_WORDS.
      CONDENSE TAX_DEDUCTED_WRDS.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'amt_in_words' AMT_IN_WORDS.

      PERFORM WRITE_FORM USING 'DETAILS9' 'APPEND' 'BODY' 'MAIN'.

      PERFORM WRITE_FORM USING 'DETAILST' 'APPEND' 'BODY' 'MAIN'.

      PERFORM WRITE_FORM USING 'DETAILSV' 'APPEND' 'BODY' 'VERIF'.

      PERFORM END_FORM.

* Annexure
      IF DISP_FLG_LOT LT 1 AND LAYOUT1 NE ' '.
        FORMNAME1 = LAYOUT1.
      ELSE.
        FORMNAME1 = 'HR_IN_TAXF16NX_Y'.
      ENDIF.
      LANG = 'EN'.

      PERFORM START_FORM USING FORMNAME1 LANG 'PAGE1'.

      PERFORM WRITE_FORM USING 'HEADER' 'APPEND' 'BODY' 'MAIN'.

* Gross components
      PERFORM WRITE_FORM USING 'GROSS_HEADER' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT GROSS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2    .
        IF GROSS_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
        CLEAR GROSS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'emp_no' FLEXTXT .
        PERFORM SIGN_ALGMNT USING GROSS_TAB-AMOUNT
                             CHANGING GROSS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' GROSS_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'GROSS_COMPONENTS' 'APPEND' 'BODY' 'MAIN'
   .
      ENDLOOP.

* Perk components
      PERFORM WRITE_FORM USING 'PERK_HEADER' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT PERK_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                             CNTR2 = FINAL_TAB-CNTR2.
        IF PERK_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
        CLEAR PERK_TAB_AMOUNT.
        PERFORM SIGN_ALGMNT USING PERK_TAB-AMOUNT
                             CHANGING PERK_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' PERK_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'PERK_COMPONENTS' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.

      PERFORM WRITE_FORM USING 'GROSS_TOTAL' 'APPEND' 'BODY' 'MAIN'.

*  IFOS components
      PERFORM WRITE_FORM USING 'IFOS_HEADER' 'APPEND' 'BODY' 'MAIN'.
      LOOP AT IFOS_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                             CNTR2 = FINAL_TAB-CNTR2.
        IF IFOS_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
        CLEAR IFOS_TAB_AMOUNT.
        PERFORM SIGN_ALGMNT USING IFOS_TAB-AMOUNT
                             CHANGING IFOS_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' IFOS_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'IFOS_COMPONENTS' 'APPEND' 'BODY' 'MAIN'.
      ENDLOOP.
      PERFORM WRITE_FORM USING 'IFOS_TOTAL' 'APPEND' 'BODY' 'MAIN'.


* Section 10 components
      PERFORM WRITE_FORM USING 'SEC10_HEADER' 'APPEND' 'BODY' 'MAIN'.

      LOOP AT SEC10_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                              CNTR2 = FINAL_TAB-CNTR2.
        IF SEC10_TAB-SIGN = -1.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' '-'.
        ELSE.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'sign' SPACE.
        ENDIF.
        CLEAR SEC10_TAB_AMOUNT.
        PERFORM SIGN_ALGMNT USING SEC10_TAB-AMOUNT
                             CHANGING SEC10_TAB_AMOUNT.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'amount' SEC10_TAB_AMOUNT.
        PERFORM WRITE_FORM USING 'SEC10_COMPONENTS' 'APPEND' 'BODY' 'MAIN'
   .
      ENDLOOP.
      PERFORM WRITE_FORM USING 'SEC10_TOTAL' 'APPEND' 'BODY' 'MAIN'.

      PERFORM WRITE_FORM USING 'FOOTER' 'APPEND' 'BOTTOM' 'MAIN'.

      PERFORM END_FORM.

* End of Annexure

* End of Second Page

* Second Annexure For Form 12BA

* FORM 12BA IS TO BE PRINTED ONLY IF SALARY PAID OR SALARY PAYABLE IS
* GREATER THAN THE AMOUNT IN THE CONSTANT F12BA IN T511P

      IF DISP_FLG_LOT LT 1 AND LAYOUT2 NE ' '.
        FORMNAME2 = LAYOUT2.
      ELSE.
        FORMNAME2 = 'HR_IN_TAXF16NX_P'.
      ENDIF.
      LANG = 'EN'.

      PERFORM START_FORM USING FORMNAME2 LANG 'PAGE1'.

      PERFORM ER_ADDRESS USING FINAL_TAB-TANNO
                           CHANGING SADR
                                    SADR_CIT
                                    ADDR1_VAL
                                    ADDR1_VAL_CIT  .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ename1'(010) ADDR1_VAL-NAME1 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ename2'(016) SADR-NAME2 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Estreet'(024) SADR-STRAS .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Epobox'(025) SADR-PFACH .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Epocode'(026) SADR-PSTLZ+0(6) .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Ecity'(027) SADR-ORT01 .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'Edistrict'(028) SADR-ORT02 .
      IF ( NOT SADR-LAND1 IS INITIAL ) AND ( NOT SADR-REGIO IS INITIAL ).
        PERFORM RE_T005U USING SADR-LAND1 SADR-REGIO
                       CHANGING STATE.
        PERFORM CONVERT_TO_SCRIPTVAR USING 'Estate'(035) STATE.
      ENDIF.

      PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'HDR'.
      PERFORM WRITE_FORM USING '' 'APPEND' 'BODY' 'FLEFT'.

      PERFORM CONVERT_TO_SCRIPTVAR USING 'emp_no' FLEXTXT .
      PERFORM CONVERT_TO_SCRIPTVAR USING 'fin_start' FIN_START.
      PERFORM CONVERT_TO_SCRIPTVAR USING 'fin_end' FIN_END.

*--- Compute for sl. no.6 of form 12BA------------------*
      F12BA_BALANCE = SAL_SEC17 + PROF_F12BA - FINAL_TAB-SEC10_ALL.
      SNAME = '40INDED'.
      ENDDATE = PENDDA.
      PERFORM RE596F USING SNAME ENDDATE.
      PERFORM (T596F-MODNA) IN PROGRAM (T596F-PGMNA)
                           USING F12BA_BALANCE
                                 ENDDATE
                           CHANGING F12BA_STD_DED.
      SALARIES_WO_PERK = F12BA_BALANCE - ( F12BA_STD_DED + ENT_ALL +
                                           FINAL_TAB-PTAX ).

      PERFORM CONVERT_TO_SCRIPTVAR USING 'salaries_wo_perk'
   SALARIES_WO_PERK.

*---------------------------------------------------------*

*  FILLING OF THE MAIN WINDOW.*****************

      LOOP AT FORM12BA_TAB WHERE PERNR = FINAL_TAB-PERNR AND
                                 CNTR2 = FINAL_TAB-CNTR2.

***************************************************
* FORM THE VARIABLE.
        VAL_PERK = 'VAL_PERK'.
        CONCATENATE VAL_PERK FORM12BA_TAB-EVCLS_SPEC INTO VAL_PERK.

        CLEAR : VAL_PERK_S, EERECVR_S, TAX_PERK_S.

        PERFORM SIGN_ALGMNT USING FORM12BA_TAB-VAL_PERK
                               CHANGING VAL_PERK_S.
        PERFORM CONVERT_TO_SCRIPTVAR USING VAL_PERK
                                  VAL_PERK_S.

***************************************************
* FORM THE VARIABLE.

        EERECVR = 'EERECVR'.
        CONCATENATE EERECVR FORM12BA_TAB-EVCLS_SPEC INTO EERECVR.

        PERFORM SIGN_ALGMNT USING FORM12BA_TAB-EERECVR
                               CHANGING EERECVR_S.
        PERFORM CONVERT_TO_SCRIPTVAR USING EERECVR
                                  EERECVR_S.

***************************************************
* FORM THE VARIABLE.

        TAX_PERK = 'TAX_PERK'.
        CONCATENATE TAX_PERK FORM12BA_TAB-EVCLS_SPEC INTO TAX_PERK.

        PERFORM SIGN_ALGMNT USING FORM12BA_TAB-TAX_PERK
                               CHANGING TAX_PERK_S.
        PERFORM CONVERT_TO_SCRIPTVAR USING TAX_PERK
                                      TAX_PERK_S.

***************************************************

        IF FORM12BA_TAB-EVCLS_SPEC NE 17.

          TOT_VAL_PERK = TOT_VAL_PERK + FORM12BA_TAB-VAL_PERK.
          TOT_EERECVR = TOT_EERECVR + FORM12BA_TAB-EERECVR.
          TOT_TAX_PERK = TOT_TAX_PERK + FORM12BA_TAB-TAX_PERK.

          CLEAR : TOT_VAL_PERK_S, TOT_EERECVR_S, TOT_TAX_PERK_S.

          PERFORM SIGN_ALGMNT USING TOT_VAL_PERK
                              CHANGING TOT_VAL_PERK_S.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_VAL_PERK'
                                         TOT_VAL_PERK_S.

          PERFORM SIGN_ALGMNT USING TOT_EERECVR
                              CHANGING TOT_EERECVR_S.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_EERECVR'
                                         TOT_EERECVR_S.

          PERFORM SIGN_ALGMNT USING TOT_TAX_PERK
                              CHANGING TOT_TAX_PERK_S.
          PERFORM CONVERT_TO_SCRIPTVAR USING 'TOT_TAX_PERK'
                                         TOT_TAX_PERK_S.
        ENDIF.

        PERFORM WRITE_FORM USING 'FORM12BA_COMPONENTS' 'APPEND' 'BODY'
                                                   'DETAILS'.

      ENDLOOP.

      PERFORM END_FORM.

    ENDLOOP.   " Final_tab

    PERFORM CLOSE_FORM.
  ELSE.

    PERFORM PRINT_PDF_MODULE.

  ENDIF.



ENDFORM.                    " PRINT_FORM
*&---------------------------------------------------------------------*
*&      Form  GET_LAYOUT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SNAME  text
*      -->P_ENDDATE  text
*----------------------------------------------------------------------*
FORM GET_LAYOUT_SET  USING    P_SNAME
                              P_ENDDATE
                     CHANGING P_FORMNAME.

  PERFORM RE596F USING P_SNAME P_ENDDATE.

  PERFORM (T596F-MODNA) IN PROGRAM (T596F-PGMNA)
                        CHANGING P_FORMNAME.

ENDFORM.                    " GET_LAYOUT_SET
*&---------------------------------------------------------------------*
*&      Form  RE596F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SNAME  text
*      -->P_P_ENDDA  text
*----------------------------------------------------------------------*
FORM RE596F  USING  $SNAME $DATUM.

  CHECK T596F-SNAME NE $SNAME OR
        T596F-BEGDA GT $DATUM OR T596F-ENDDA LT $DATUM.
  SELECT * FROM T596F WHERE SNAME EQ $SNAME
                        AND BEGDA LE $DATUM
                        AND ENDDA GE $DATUM.
  ENDSELECT.
  IF SY-SUBRC NE 0.
*    MESSAGE E059(HRPADIN01) WITH 'T596F' $SNAME.
**   No entry in table & for key &
*    PERFORM BUILD_ERROR TABLES HR_ERROR
*                       USING SPACE SY-MSGID SY-MSGNO
*                       SY-MSGV1  SY-MSGV2  SY-MSGV3  SY-MSGV4.
  ENDIF.

ENDFORM.                                                    "RE596F
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENDDATE  text
*      <--P_FORMNAME  text
*      <--P_TEMP_PSCRIPT  text
*----------------------------------------------------------------------*
FORM GET_OUTPUT_TYPE  USING    END_DATE
                      CHANGING FORMNAME
                               P_SCRIPT.

  DATA: OUTPUT_TYPE TYPE T5F99OSFT-FOTYPE.

  CALL FUNCTION 'HR_99S_GET_LFORM_PROPERTIES'
    EXPORTING
      I_FOLNAME                      = FORMNAME
*   I_FOVARIANT                    =
      I_DATE                         = END_DATE
*   I_FOTYPE                       = OUTPUT_TYPE
    IMPORTING
      E_FOPNAME                      = FORMNAME
      E_FOTYPE                       = OUTPUT_TYPE
    EXCEPTIONS
      TYPE_NAME_NOT_MAINTAINED       = 1
      FORM_NAME_NOT_FOUND            = 2
      OTHERS                         = 3
            .
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E262(HRPADIN01).
      WHEN 2.
        MESSAGE E263(HRPADIN01).
      WHEN 3.
        MESSAGE E264(HRPADIN01).
    ENDCASE.
  ENDIF.

*  OUTPUT_TYPE = 'SSC'.

  IF OUTPUT_TYPE = 'SSC'.
    P_SCRIPT = 'X'.
  ELSE.
    CLEAR P_SCRIPT.
  ENDIF.

ENDFORM.                    " GET_OUTPUT_TYPE
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_FORM .

  DATA: OTF_TABLE  LIKE ITCOO OCCURS 0 WITH HEADER LINE.
* Data for ESS scenario

  CALL FUNCTION 'CLOSE_FORM'
*    IMPORTING
*         RESULT                   =
*         RDI_RESULT               =
     TABLES
          OTFDATA                  = OTF_TABLE
      EXCEPTIONS
           UNOPENED                 = 1
           BAD_PAGEFORMAT_FOR_PRINT = 2
           OTHERS                   = 3
            .
  IF SY-SUBRC <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
* BEGIN OF ESS CHANGES
  IF PNPESSCF = 'X'.
*   perform report_data_convert(saplhress00_rep) tables otf_table[].
    PERFORM REPORT_DATA_CONVERT TABLES OTF_TABLE[] .

  ENDIF.

ENDFORM.                    " CLOSE_FORM
*&---------------------------------------------------------------------*
*&      Form  CONVERT_INR_TO_WORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PR_EMP_TOTAL  text
*      <--P_TAX_DEDUCTED_WRDS  text
*----------------------------------------------------------------------*
FORM CONVERT_INR_TO_WORDS  USING    P_AMT_IN_NUM
                           CHANGING P_AMT_IN_WORDS.

  CALL FUNCTION 'HR_IN_CHG_INR_WRDS'
    EXPORTING
      AMT_IN_NUM         = P_AMT_IN_NUM
    IMPORTING
      AMT_IN_WORDS       = P_AMT_IN_WORDS
    EXCEPTIONS
      DATA_TYPE_MISMATCH = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " CONVERT_INR_TO_WORDS
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_SCRIPTVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4530   text
*      -->P_PENDDA(4)  text
*----------------------------------------------------------------------*
FORM CONVERT_TO_SCRIPTVAR  USING    VALUE($NAME)
                                    VALUE($WERT).

  DATA: FNAME(22) TYPE C.

  FNAME+00(1) = '&'.
  FNAME+01(20) = $NAME.
  FNAME+21(1) = '&'.
  CONDENSE FNAME NO-GAPS.
  CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
    EXPORTING
      NAME         = FNAME
      VALUE        = $WERT
      VALUE_LENGTH = 0.

ENDFORM.                    " CONVERT_TO_SCRIPTVAR
*&---------------------------------------------------------------------*
*&      Form  END_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM END_FORM .

  CALL FUNCTION 'END_FORM'
*    IMPORTING
*         RESULT                   =
      EXCEPTIONS
           UNOPENED                 = 1
           BAD_PAGEFORMAT_FOR_PRINT = 2
           OTHERS                   = 3
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " END_FORM

*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOM_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_FLEXTXT  text
*      <--P_FINAL_TAB_PERNR  text
*----------------------------------------------------------------------*
FORM GET_CUSTOM_TEXT  CHANGING P_FLEXTXT
                               P_PERNR.

  DATA: CUST_TEXT TYPE REF TO HR_IN_F16_HTEXT1,
          CHECK_TMP(1) TYPE C.
  CLEAR : CHECK_TMP.

  TRY.
      GET BADI CUST_TEXT
        FILTERS
          FLT_VAL = '40'.

      CALL BADI CUST_TEXT->CUSTOM_TEXT_F16
        EXPORTING
          PERNR     = PERNR-PERNR
          P0001     = P0001
          F16BEG    = F16_BEGDA
          F16END    = F16_ENDDA
          FLT_VAL   = '40'
        IMPORTING
          CUST_TEXT = P_FLEXTXT.

      CHECK_TMP = 'T'.

    CATCH CX_BADI_NOT_IMPLEMENTED.
  ENDTRY.
  IF SY-SUBRC NE 0.
    P_FLEXTXT = P_PERNR.
  ENDIF.

ENDFORM.                    " GET_CUSTOM_TEXT
*&---------------------------------------------------------------------*
*&      Form  OPEN_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LANG  text
*----------------------------------------------------------------------*
FORM OPEN_FORM  USING    $LANG
                         P_FORMNAME.

  TABLES: ITCPO.

  DATA: OPTIONS LIKE ITCPO.
  DATA: DIALOG(1).
  DATA: PRI_PARAMS LIKE PRI_PARAMS.

  IF PNPESSCF = 'X'.
    OPTIONS-TDNOPRINT = 'X'.
    OPTIONS-TDPREVIEW = 'X'.
    OPTIONS-TDTITLE = SPACE.
    OPTIONS-TDGETOTF = 'X'.
    OPTIONS-TDPRINTER = 'POSTSCPT'.

    DIALOG = SPACE.
  ELSE.
    DIALOG = 'X'.
  ENDIF.
* END OF ESS CHANGES

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
         EXPORTING  NO_DIALOG             = 'X'
                    MODE                  = 'CURRENT'
*                  NEW_LIST_ID           = $PRNEW
         IMPORTING  OUT_PARAMETERS       =  PRI_PARAMS.

  OPTIONS-TDCOPIES = PRI_PARAMS-PRCOP.
  OPTIONS-TDDEST = PRI_PARAMS-PDEST.
  OPTIONS-TDNEWID = PRI_PARAMS-PRNEW.
  OPTIONS-TDIMMED = PRI_PARAMS-PRIMM.
  OPTIONS-TDDELETE = PRI_PARAMS-PRREL.
  OPTIONS-TDLIFETIME = PRI_PARAMS-PEXPI.
  OPTIONS-TDTITLE = PRI_PARAMS-PRTXT.
  OPTIONS-TDCOVER = PRI_PARAMS-PRSAP.
  OPTIONS-TDCOVTITLE = PRI_PARAMS-PRTXT.
  OPTIONS-TDRECEIVER = PRI_PARAMS-PRREC.
  OPTIONS-TDDIVISION = PRI_PARAMS-PRABT.
  OPTIONS-TDAUTORITY = PRI_PARAMS-PRBER.

  CALL FUNCTION 'OPEN_FORM'
      EXPORTING
           APPLICATION                 = 'TX'
*         ARCHIVE_INDEX               =
*         ARCHIVE_PARAMS              =
           DEVICE                      = 'PRINTER'
           DIALOG                      = DIALOG
*         FORM                        = P_FORMNAME
           LANGUAGE                    = $LANG
          OPTIONS                     = OPTIONS
*         MAIL_SENDER                 =
*         MAIL_RECIPIENT              =
*         MAIL_APPL_OBJECT            =
*         RAW_DATA_INTERFACE          = '*'
*    IMPORTING
*         LANGUAGE                    =
*         NEW_ARCHIVE_PARAMS          =
*         RESULT                      =
      EXCEPTIONS
           CANCELED                    = 1
           DEVICE                      = 2
           FORM                        = 3
           OPTIONS                     = 4
           UNCLOSED                    = 5
           MAIL_OPTIONS                = 6
           ARCHIVE_ERROR               = 7
           INVALID_FAX_NUMBER          = 8
           MORE_PARAMS_NEEDED_IN_BATCH = 9
           OTHERS                      = 10
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " OPEN_FORM
*&---------------------------------------------------------------------*
*&      Form  RE_T005U
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SADR_LAND1  text
*      -->P_SADR_REGIO  text
*      <--P_STATE  text
*----------------------------------------------------------------------*
*FORM RE_T005U  USING    P_COUNTRY
*                        P_REGION
*               CHANGING P_STATE.
*  SELECT SINGLE BEZEI INTO P_STATE FROM T005U
*                      WHERE SPRAS = SY-LANGU AND
*                            LAND1 = P_COUNTRY AND
*                            BLAND = P_REGION.
*
*ENDFORM.                                                    " RE_T005U
*&---------------------------------------------------------------------*
*&      Form  SIGN_ALGMNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SEC10_TAB_AMOUNT  text
*      <--P_SEC10_TAB_AMOUNT  text
*----------------------------------------------------------------------*
FORM SIGN_ALGMNT  USING    P_FINAL_TAB_OTH_INCOME
                  CHANGING P_OTH_INCOME_S.

  DATA: TEXT1(1) TYPE C.
  DATA: VALUE(20) TYPE C.
  VALUE = P_FINAL_TAB_OTH_INCOME.
  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.
  P_OTH_INCOME_S = VALUE.

ENDFORM.                    " SIGN_ALGMNT
*&---------------------------------------------------------------------*
*&      Form  START_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FORMNAME  text
*      -->P_LANG  text
*      -->P_4617   text
*----------------------------------------------------------------------*
FORM START_FORM  USING   $FORMNAME $LANG $PAGE.

  CALL FUNCTION 'START_FORM'
       EXPORTING
*           ARCHIVE_INDEX    = ' '
            FORM             = $FORMNAME
            LANGUAGE         = $LANG
            STARTPAGE        = $PAGE
*           PROGRAM          = ' '
*           MAIL_APPL_OBJECT =
*      IMPORTING
*           LANGUAGE      =
       EXCEPTIONS
            FORM          = 1
            FORMAT        = 2
            UNENDED       = 3
            UNOPENED      = 4
            UNUSED        = 5
            OTHERS        = 6.

  CASE SY-SUBRC.
    WHEN 1.
*     Layout set $formname in language $lang does not exist
      MESSAGE  E108(HRPADIN01) WITH $FORMNAME $LANG.
  ENDCASE.

ENDFORM.                    " START_FORM
*&---------------------------------------------------------------------*
*&      Form  WRITE_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4821   text
*      -->P_4822   text
*      -->P_4823   text
*      -->P_4824   text
*----------------------------------------------------------------------*
FORM WRITE_FORM  USING   $ELE $FUN $TYP $WIN.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT               = $ELE
            FUNCTION              = $FUN
            TYPE                  = $TYP
            WINDOW                = $WIN
*    IMPORTING
*          PENDING_LINES           =
       EXCEPTIONS
         ELEMENT                  = 1
         FUNCTION                 = 2
         TYPE                     = 3
         UNOPENED                 = 4
         UNSTARTED                = 5
         WINDOW                   = 6
         BAD_PAGEFORMAT_FOR_PRINT = 7
         OTHERS                   = 8
          .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " WRITE_FORM
*&---------------------------------------------------------------------*
*&      Form  FILL_FORM12BA_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I512W  text
*      -->P_FORM12BA_TAB  text
*      -->P_PERNR_PERNR  text
*      -->P_WA_TMP_COCD_CNTR2  text
*----------------------------------------------------------------------*
FORM FILL_FORM12BA_TAB  TABLES   P_I512W STRUCTURE I512W
                                 P_FORM12BA_TAB STRUCTURE FORM12BA_TAB
                        USING    P_PERNR_PERNR
                                 P_WA_TMP_COCD_CNTR2.

  DATA: DED_EVAL_SPEC TYPE I,
         EVL_SPEC(2) TYPE N.

  DATA: BEGIN OF EVAL_TAB OCCURS 10,
         LGART LIKE T512W-LGART,
         EVCLS_SPEC LIKE T52D4-EVCLV,
        END OF EVAL_TAB.

  CLEAR EVAL_TAB.
  REFRESH EVAL_TAB.
  CLEAR P_FORM12BA_TAB.
  LOOP AT P_I512W WHERE NOT AKLAS+16(2) IS INITIAL.
    MOVE-CORRESPONDING P_I512W TO EVAL_TAB.
    EVAL_TAB-EVCLS_SPEC = P_I512W-AKLAS+16(2).
    APPEND EVAL_TAB.
  ENDLOOP.

  SORT EVAL_TAB BY EVCLS_SPEC LGART.
  SORT RT BY LGART.
  LOOP AT EVAL_TAB.

    IF EVAL_TAB-EVCLS_SPEC GE 31.
      EXIT.
    ELSE.
      CLEAR P_FORM12BA_TAB.

      P_FORM12BA_TAB-PERNR = P_PERNR_PERNR.
      P_FORM12BA_TAB-EVCLS_SPEC = EVAL_TAB-EVCLS_SPEC.
      MOVE P_WA_TMP_COCD_CNTR2 TO P_FORM12BA_TAB-CNTR2.
*    READ TABLE CRT WITH KEY LGART = EVAL_TAB-LGART CUMTY = 'Y'.
      READ TABLE F16 WITH KEY CNTR2 = P_WA_TMP_COCD_CNTR2
              LGART = EVAL_TAB-LGART
              CUMTY = 'Y'.

      IF SY-SUBRC = 0.
        P_FORM12BA_TAB-TAX_PERK = F16-BETRG.
      ELSE.
*         PERFORM READ_RT USING EVAL_TAB-LGART
*                         CHANGING FORM12BA_TAB-TAX_PERK.
        PERFORM READ_F16 USING EVAL_TAB-LGART
                               ''
                               TMP_COCD-CNTR2
                        CHANGING P_FORM12BA_TAB-TAX_PERK.
      ENDIF.

      COLLECT P_FORM12BA_TAB.
    ENDIF.
  ENDLOOP.
* To display the fields with blank as zero on Form12BA.
  CLEAR: P_FORM12BA_TAB.
  EVL_SPEC = '01'.
  DO 18 TIMES.
    READ TABLE P_FORM12BA_TAB WITH KEY EVCLS_SPEC = EVL_SPEC PERNR =
P_PERNR_PERNR .
    IF SY-SUBRC NE 0.
      P_FORM12BA_TAB-CNTR2 = '01'.
      P_FORM12BA_TAB-PERNR = P_PERNR_PERNR.
      P_FORM12BA_TAB-EVCLS_SPEC = EVL_SPEC.
      P_FORM12BA_TAB-TAX_PERK = '0.00'.
      P_FORM12BA_TAB-EERECVR = '0.00'.
      P_FORM12BA_TAB-VAL_PERK = '0.00'.
      APPEND P_FORM12BA_TAB.
    ENDIF.
    EVL_SPEC = EVL_SPEC + '01'.
  ENDDO.
* End of changes to display the fields with blank as zero on Form12BA.

  LOOP AT P_FORM12BA_TAB WHERE PERNR = P_PERNR_PERNR AND
                             CNTR2 = P_WA_TMP_COCD_CNTR2.

    CLEAR DED_EVAL_SPEC.

    DED_EVAL_SPEC = P_FORM12BA_TAB-EVCLS_SPEC + 30.

    LOOP AT EVAL_TAB WHERE EVCLS_SPEC = DED_EVAL_SPEC.
      READ TABLE F16 WITH KEY CNTR2 = P_WA_TMP_COCD_CNTR2
              LGART = EVAL_TAB-LGART
              CUMTY = 'Y'.
      IF SY-SUBRC = 0.
        P_FORM12BA_TAB-EERECVR = P_FORM12BA_TAB-EERECVR + ABS( F16-BETRG
 ).
      ENDIF.
    ENDLOOP.

    P_FORM12BA_TAB-VAL_PERK = P_FORM12BA_TAB-EERECVR +
                            P_FORM12BA_TAB-TAX_PERK .
    MODIFY P_FORM12BA_TAB.
  ENDLOOP.

ENDFORM.                    " FILL_FORM12BA_TAB
*&---------------------------------------------------------------------*
*&      Form  READ_F16
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EVAL_TAB_LGART  text
*      -->P_7198   text
*      -->P_TMP_COCD_CNTR2  text
*      <--P_FORM12BA_TAB_TAX_PERK  text
*----------------------------------------------------------------------*
FORM READ_F16  USING     P_WTYPE LIKE T512W-LGART
                         P_CUMTY LIKE T54C3-CUMTY
                         P_CNTR2 LIKE PC207-CNTR2
               CHANGING  AMUNT   LIKE PC207-BETRG.

  CLEAR F16.
  LOOP AT F16 WHERE CNTR2 = P_CNTR2 AND
                    LGART = P_WTYPE AND
                    CUMTY = P_CUMTY.
    AMUNT = AMUNT + F16-BETRG.
  ENDLOOP.

ENDFORM.                                                    " READ_F16

*&---------------------------------------------------------------------*
*&      Form  DRAW_TDSBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM DRAW_TDSBOX TABLES IN_TAB STRUCTURE ITCSY
                   OUT_TAB STRUCTURE ITCSY.
  DATA: CH(2) TYPE C.
  COUNTER = COUNTER + 1.
  MOVE COUNTER TO CH.
  IN_TAB-NAME = 'YP'.
  IN_TAB-VALUE = CH.
  APPEND IN_TAB.
  OUT_TAB-NAME = 'YP'.
  OUT_TAB-VALUE = CH.
  APPEND OUT_TAB.

ENDFORM.                    "DRAW_TDSBOX
*&---------------------------------------------------------------------*
*&      Form  MERGE_UPL_CHL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXCEL_TAB  text
*      -->P_CH_DET  text
*----------------------------------------------------------------------*
FORM MERGE_UPL_CHL  TABLES   P_EXCEL_TAB STRUCTURE EXCEL_TAB
                             P_CH_DET LIKE CH_DET
                    USING    P_TMP_COCD LIKE LINE OF TMP_COCD.

  DATA: WA_EXCEL_TAB LIKE LINE OF EXCEL_TAB,
        WA_CH_DET LIKE LINE OF CH_DET.
  DATA: TEMP_CH_DET LIKE CH_DET,
        TEMP_CH_DET_1 LIKE CH_DET.
*  LOOP AT EMP_TAB.
  READ TABLE EXCEL_TAB WITH KEY EMP_NO = PERNR-PERNR.
  IF SY-SUBRC = 0.
    TEMP_CH_DET[] = TEMP_CH_DET_1[] = P_CH_DET[].
*  REFRESH P_CH_DET[].
    LOOP AT P_CH_DET  WHERE PERNR = PERNR-PERNR
                        AND CNTR2 EQ P_TMP_COCD-CNTR2.
      DELETE P_CH_DET INDEX SY-TABIX.
    ENDLOOP.
    LOOP AT TEMP_CH_DET INTO WA_CH_DET WHERE CNTR2 EQ P_TMP_COCD-CNTR2
                                         AND PERNR = PERNR-PERNR."
*AND BEGDA >= P_BEGDA AND ENDDA <= P_ENDDA.
      DELETE TEMP_CH_DET_1 WHERE CNTR2 = WA_CH_DET-CNTR2
                      AND PERNR = WA_CH_DET-PERNR
                      AND FPPER = WA_CH_DET-FPPER
                      AND FPBEG = WA_CH_DET-FPBEG
                      AND FPEND = WA_CH_DET-FPEND
                      AND PAYTY = WA_CH_DET-PAYTY
                      AND PAYID = WA_CH_DET-PAYID.
      IF SY-SUBRC = 0.
        LOOP AT EXCEL_TAB INTO WA_EXCEL_TAB WHERE EMP_NO = WA_CH_DET-PERNR
                                             AND PAYPER = WA_CH_DET-FPPER
                                             AND FPBEG = WA_CH_DET-FPBEG
                                             AND FPEND = WA_CH_DET-FPEND
                                             AND PAYTY = WA_CH_DET-PAYTY
                                             AND PAYID = WA_CH_DET-PAYID.

          MOVE WA_EXCEL_TAB-EMP_NO TO WA_CH_DET-PERNR.
          MOVE WA_EXCEL_TAB-PAYTY TO WA_CH_DET-PAYTY.
          MOVE WA_EXCEL_TAB-PAYID TO WA_CH_DET-PAYID.
          MOVE WA_EXCEL_TAB-PAYPER TO WA_CH_DET-FPPER.
          MOVE WA_EXCEL_TAB-FPBEG TO WA_CH_DET-FPBEG.
          MOVE WA_EXCEL_TAB-FPEND TO WA_CH_DET-FPEND.
*            MOVE WA_EXCEL_TAB-BEGDA  TO WA_CH_DET-CBEGD.
*            MOVE WA_EXCEL_TAB-ENDDA TO WA_CH_DET-CENDD.
          MOVE WA_EXCEL_TAB-BANK_CHALLAN TO WA_CH_DET-CHLNO.
          MOVE WA_EXCEL_TAB-DT_CHALLAN TO WA_CH_DET-CHDAT.
          MOVE WA_EXCEL_TAB-TOTTAX TO WA_CH_DET-AMONT.
          MOVE WA_EXCEL_TAB-INCOMETAX TO WA_CH_DET-MNTAX.
          MOVE WA_EXCEL_TAB-EDUCESS TO WA_CH_DET-MNEDU.
          MOVE WA_EXCEL_TAB-SURCHARGE TO WA_CH_DET-MNSUR.
          MOVE WA_EXCEL_TAB-CHKNO TO WA_CH_DET-CHKNO.
          MOVE WA_EXCEL_TAB-BR_CODE TO WA_CH_DET-BRCOD.
*            MODIFY P_CH_DET FROM WA_CH_DET INDEX sy-tabix.
          APPEND WA_CH_DET TO P_CH_DET.

        ENDLOOP.
        IF SY-SUBRC NE 0.
          APPEND WA_CH_DET TO P_CH_DET.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*  P_CH_DET[] = TEMP_CH_DET[].
ENDFORM.                    " MERGE_UPL_CHL

*&---------------------------------------------------------------------*
*&      Form  report_data_convert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OTF_TABLE  text
*----------------------------------------------------------------------*
FORM REPORT_DATA_CONVERT TABLES OTF_TABLE STRUCTURE ITCOO.

  DATA: OTF_TAB LIKE ITCOO OCCURS 0 WITH HEADER LINE.
  STATICS: CP LIKE TCP00-CPCODEPAGE.
  DATA: PDF_TABLE TYPE  RCL_BAG_TLINE,
        PDF_STRING_X TYPE XSTRING,
        PDF_FSIZE TYPE  I.

  OTF_TAB[] = OTF_TABLE[].

  CALL FUNCTION 'SYSTEM_CODEPAGE'
    IMPORTING
      CURRENT_DYNAMIC_CODEPAGE = CP.

* If Unicode system
  IF CP(1) = '4'.
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
      IMPORTING
        BIN_FILESIZE          = PDF_FSIZE
        BIN_FILE              = PDF_STRING_X
      TABLES
        OTF                   = OTF_TABLE
        LINES                 = PDF_TABLE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        ERR_BAD_OTF           = 4
        OTHERS                = 5.
    IF PNPESSCF = 'X'.
      IMPORT PDF_STRING_X FROM MEMORY ID 'F16_PDF'.
    ENDIF.
    EXPORT PDF_STRING_X TO MEMORY ID 'PDFT'.
    EXPORT PDF_FSIZE TO MEMORY ID 'PDSZ'.
    EXPORT PDF_STRING_X TO MEMORY ID 'PDFF'.
    EXPORT PDF_STRING_X TO MEMORY ID 'PDFF_IN'.
    EXPORT PDF_TABLE TO MEMORY ID 'PDFT_IN'.
    EXPORT PDF_FSIZE TO MEMORY ID 'PDSZ_IN'.

  ELSE.
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
      IMPORTING
        BIN_FILESIZE          = PDF_FSIZE
*        bin_file              = pdf_string_x
      TABLES
        OTF                   = OTF_TAB
        LINES                 = PDF_TABLE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        OTHERS                = 4.

    EXPORT PDF_TABLE TO MEMORY ID 'PDFT'.
    EXPORT PDF_FSIZE TO MEMORY ID 'PDSZ'.
*    export pdf_string_x to memory id 'PDFF'.
*    export pdf_string_x to memory id 'PDFF_IN'.
    EXPORT PDF_TABLE TO MEMORY ID 'PDFT_IN'.
    EXPORT PDF_FSIZE TO MEMORY ID 'PDSZ_IN'.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
      IMPORTING
        BIN_FILESIZE          = PDF_FSIZE
        BIN_FILE              = PDF_STRING_X
      TABLES
        OTF                   = OTF_TAB
        LINES                 = PDF_TABLE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        OTHERS                = 4.
    IF PNPESSCF = 'X'.
      IMPORT PDF_STRING_X FROM MEMORY ID 'F16_PDF'.
    ENDIF.
*    export pdf_table to memory id 'PDFT'.
*    export pdf_fsize to memory id 'PDSZ'.
    EXPORT PDF_STRING_X TO MEMORY ID 'PDFF'.
    EXPORT PDF_STRING_X TO MEMORY ID 'PDFF_IN'.
*    export pdf_table to memory id 'PDFT_IN'.
*    export pdf_fsize to memory id 'PDSZ_IN'.


  ENDIF.
ENDFORM.                    "report_data_convert

*&---------------------------------------------------------------------*
*&      Form  CORRECT_CASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CORRECT_CASE.

  DATA: FILENAME TYPE STRING.

  CONCATENATE 'C:\' 'form16_' YEAR INTO FILENAME.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                    =
      FILENAME                        = FILENAME
*     FILETYPE                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS            = ABAP_TRUE
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      DATA_TAB                        = FINAL_TAB
*     FIELDNAMES                      =
*   EXCEPTIONS
*     FILE_WRITE_ERROR                = 1
*     NO_BATCH                        = 2
*     GUI_REFUSE_FILETRANSFER         = 3
*     INVALID_TYPE                    = 4
*     NO_AUTHORITY                    = 5
*     UNKNOWN_ERROR                   = 6
*     HEADER_NOT_ALLOWED              = 7
*     SEPARATOR_NOT_ALLOWED           = 8
*     FILESIZE_NOT_ALLOWED            = 9
*     HEADER_TOO_LONG                 = 10
*     DP_ERROR_CREATE                 = 11
*     DP_ERROR_SEND                   = 12
*     DP_ERROR_WRITE                  = 13
*     UNKNOWN_DP_ERROR                = 14
*     ACCESS_DENIED                   = 15
*     DP_OUT_OF_MEMORY                = 16
*     DISK_FULL                       = 17
*     DP_TIMEOUT                      = 18
*     FILE_NOT_FOUND                  = 19
*     DATAPROVIDER_EXCEPTION          = 20
*     CONTROL_FLUSH_ERROR             = 21
*     OTHERS                          = 22
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    "CORRECT_CASE

*&---------------------------------------------------------------------*
*&      Form  ER_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FINAL_TAB_TANNO  text
*----------------------------------------------------------------------*
FORM ER_ADDRESS  USING P_FINAL_TAB_TANNO
                 CHANGING  P_SADR LIKE SADR
                           P_SADR_CIT LIKE SADR
                           P_ADDR1_VAL LIKE ADDR1_VAL
                           P_ADDR1_VAL_CIT LIKE ADDR1_VAL.
  DATA: ADD_NO TYPE T500P-ADRNR.
  DATA:  AA(1), BB(1), CC(1), DD(1), EE(1),
         FF(1), GG(1), HH(1), II(1).

  DATA: ADRNR LIKE HRCA_COMPANY-ADDRESS.
  DATA: SADR1 TYPE SADR,
        ADDR1_VAL1 TYPE ADDR1_VAL .

  DATA:  SELECTION LIKE ADDR1_SEL.
*          ADDRESS_VALUE LIKE ADDR1_VAL.

*   RP-PROVIDE-FROM-LAST P0001 SPACE F16_BEGDA F16_ENDDA.
*   MOVE P0001-WERKS TO PERS_AREA.

*  READ TABLE HD_TAB INDEX 1.
  SELECT SINGLE ADRNR INTO ADD_NO FROM T500P
                                  WHERE PERSA = PERS_AREA.

  SELECTION-ADDRNUMBER = ADD_NO.

  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      ADDRESS_SELECTION = SELECTION
      ADDRESS_GROUP     = 'CA01'  "to read SADR for unchanged
    IMPORTING  "data
      ADDRESS_VALUE     = P_ADDR1_VAL  "both structures filled
      SADR              = P_SADR  "choose one of them
    EXCEPTIONS
      ADDRESS_NOT_EXIST = 1
      OTHERS            = 2.                                "SADR40A

  IF P_SADR-LAND1 IS INITIAL.
*       READ TABLE HD_TAB INDEX 1.
    PERFORM ADDRESS USING COMP_CD ADDR1_VAL.
  ENDIF.

* BADI to return the address of the Employer

  DATA  : CUST_EXIT TYPE REF TO IF_EX_HR_IN_ER_ADDRESS,
          CHECK_IMPL(1)    TYPE C.
  DATA: AD_TAX TYPE REF TO HR_IN_ER_ADDRESS,
        RESULT(1),
        FLAG.
  CLEAR FLAG.
  EXPORT YEAR TO MEMORY ID 'FINYEAR'.
  EXPORT FINAL_TAB-F16_BEGDA TO MEMORY ID 'BEGINDAT'.
  EXPORT FINAL_TAB-F16_ENDDA TO MEMORY ID 'ENDDAT'.
  TRY.
      GET BADI AD_TAX
        FILTERS
          FLT_VAL = '40'.

      CALL BADI AD_TAX->GET_ER_ADDRESS
        EXPORTING
          P0001     = P0001[]
          TANNO     = P_FINAL_TAB_TANNO
          FLT_VAL   = '40'
        CHANGING
          SADR      = SADR
          ADDR1_VAL = ADDR1_VAL.
      FLAG = 'X'.

      IF ( FLAG = 'X' ) AND NOT ( SADR IS INITIAL ).
        MOVE-CORRESPONDING SADR TO P_SADR.
        MOVE-CORRESPONDING ADDR1_VAL TO P_ADDR1_VAL.
      ENDIF.

      CALL BADI AD_TAX->GET_CIT_ADDRESS
        EXPORTING
          P0001     = P0001[]
          FLT_VAL   = '40'
          TANNO     = P_FINAL_TAB_TANNO
        CHANGING
          SADR      = P_SADR_CIT
          ADDR1_VAL = P_ADDR1_VAL_CIT.

    CATCH CX_BADI_NOT_IMPLEMENTED.
  ENDTRY.
  FREE MEMORY ID 'FINYEAR'.
  FREE MEMORY ID 'BEGINDAT'.
  FREE MEMORY ID 'ENDDAT'.
  IF ADDR1_VAL-NAME1 IS INITIAL. AA = 'N'. ENDIF.
  IF P_SADR-NAME2 IS INITIAL. BB = 'N'. ENDIF.
  IF P_SADR-NAME3 IS INITIAL. CC = 'N'. ENDIF.
  IF P_SADR-NAME4 IS INITIAL. DD = 'N'. ENDIF.
  IF P_SADR-STRAS IS INITIAL. EE = 'N'. ENDIF.
  IF P_SADR-PFACH IS INITIAL. FF = 'N'. ENDIF.
  IF P_SADR-PSTLZ IS INITIAL. GG = 'N'. ENDIF.
  IF P_SADR-ORT01 IS INITIAL. HH = 'N'. ENDIF.
  IF P_SADR-ORT02 IS INITIAL. II = 'N'. ENDIF.

  PERFORM CONVERT_TO_SCRIPTVAR USING 'A' AA.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'B' BB.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'C' CC.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'D' DD.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'E' EE.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'F' FF.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'G' GG.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'H' HH.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'I' II.
ENDFORM.                    " ER_ADDRESS_PDF

*&---------------------------------------------------------------------*
*&      Form  GET_CHALLAN_CLUSTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MON_TAX_COMP  text
*      -->P_P0001  text
*      -->P_P0185  text
*      -->P_P4DEDT_TAB  text
*      -->P_P4CHLN_TAB  text
*----------------------------------------------------------------------*
FORM GET_CHALLAN_CLUSTER  TABLES   P_MON_TAX_COMP LIKE MON_TAX_COMP
                                     "Insert correct name for <...>
                                   P_P0001 STRUCTURE P0001
                                   P_P0185 STRUCTURE P0185
                                   P_P4DEDT_TAB STRUCTURE P4DEDT_TAB
                                   P_P4CHLN_TAB STRUCTURE P4CHLN_TAB
                           USING   P_TANR
                                   P_MULTIPLE_F16
                                   P_PERNR_PERNR
                                   P_WA_TMP_COCD_BUKRS
                                   P_WA_TMP_COCD_CNTR2.

  DATA: WA_MON_TAX_COMP LIKE LINE OF MON_TAX_COMP,
        TANNO TYPE PIN_TANNO.

  LOOP AT P_MON_TAX_COMP INTO WA_MON_TAX_COMP
       WHERE CNTR2 = P_WA_TMP_COCD_CNTR2
       AND PERNR = PERNR-PERNR.

    PERFORM GET_TAN TABLES P0001 P0185
                      USING  WA_MON_TAX_COMP-FPBEG
                             WA_MON_TAX_COMP-FPEND
                             WA_MON_TAX_COMP-PAYDT
                             P_TANR
                             P_MULTIPLE_F16
                             P_WA_TMP_COCD_BUKRS
                      CHANGING TANNO.

    PERFORM READ_CHALLAN_CLUSTER TABLES P_P4DEDT_TAB
                                          P_P4CHLN_TAB
                                   USING TANNO
                                         P_PERNR_PERNR
                                         WA_MON_TAX_COMP-FPBEG
                                         WA_MON_TAX_COMP-FPEND
                                         WA_MON_TAX_COMP-PENDDA.
*                                   CHANGING tax_deposited.
  ENDLOOP.




ENDFORM.                    " GET_CHALLAN_CLUSTER

*&---------------------------------------------------------------------*
*&      Form  BUILD_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->HR_ERROR   text
*      -->$PERNR     text
*      -->$MSGID     text
*      -->$MSGNO     text
*      -->$MSGV1     text
*      -->$MSGV2     text
*      -->$MSGV3     text
*      -->$MSGV4     text
*----------------------------------------------------------------------*
FORM BUILD_ERROR TABLES   HR_ERROR STRUCTURE HR_ERROR
                  USING    $PERNR
                           $MSGID
                           $MSGNO
                           $MSGV1
                           $MSGV2
                           $MSGV3
                           $MSGV4.

  HR_ERROR-PERNR = $PERNR.
  HR_ERROR-ARBGB = $MSGID.
  HR_ERROR-MSGTY = 'E'.
  HR_ERROR-MSGNO = $MSGNO.
  HR_ERROR-MSGV1 = $MSGV1.
  HR_ERROR-MSGV2 = $MSGV2.
  HR_ERROR-MSGV3 = $MSGV3.
  HR_ERROR-MSGV4 = $MSGV4.
  APPEND HR_ERROR.

ENDFORM.                              " BUILD_ERROR

*&---------------------------------------------------------------------*
*&      Form  ERROR_CASES
*&---------------------------------------------------------------------*
FORM ERROR_CASES.

  DATA: NUM TYPE I.

  DESCRIBE TABLE HR_ERROR LINES NUM.
  IF NUM > 0.
    CALL FUNCTION 'HR_DISPLAY_ERROR_LIST'
   EXPORTING
         NO_POPUP         = ' '
         NO_PRINT         = ' '
*         NO_IMG           = ' '
*         LINESIZE         = SY-LINSZ
     TABLES
          ERROR            = HR_ERROR
         EXCEPTIONS
              INVALID_LINESIZE = 1
              OTHERS           = 2.
  ELSE.
    MESSAGE S361(HRPADIN01).
*   There are no errors
    IF SY-BATCH = 'X'.
      WRITE: TEXT-NER.
    ENDIF.
  ENDIF.



ENDFORM.                               " ERROR_CASES

*&---------------------------------------------------------------------*
*&      Form  writeverif
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM WRITEVERIF TABLES IN_PAR STRUCTURE ITCSY
                      OUT_PAR STRUCTURE ITCSY.

  LOOP AT IN_PAR WHERE NAME = 'ENDPAGE2'.
    PRINT_VERIF = IN_PAR-VALUE.
  ENDLOOP.
ENDFORM.                    "writeverif

*&---------------------------------------------------------------------*
*&      Form  CLEARVERIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM CLEARVERIF TABLES IN_PAR STRUCTURE ITCSY
                       OUT_PAR STRUCTURE ITCSY.
  CLEAR PRINT_VERIF.
ENDFORM.                    "CLEARVERIF
*&---------------------------------------------------------------------*
*&      Form  GET_MONTH_BEGDA_ENDDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TAX_COMP_FPBEG  text
*      <--P_WA_TAX_COMP_PBEGDA  text
*      <--P_WA_TAX_COMP_PENDDA  text
*----------------------------------------------------------------------*
FORM GET_MONTH_BEGDA_ENDDA  USING    P_WA_TAX_COMP_FPBEG
                                     P_ABKRS
                            CHANGING P_WA_TAX_COMP_PBEGDA
                                     P_WA_TAX_COMP_PENDDA.

  DATA: WA_T549Q LIKE T549Q,
        PERMO    LIKE T549Q-PERMO.

  SELECT SINGLE PERMO FROM T549A INTO PERMO
         WHERE ABKRS = P_ABKRS.

  SELECT * FROM T549Q INTO WA_T549Q
         WHERE  PERMO       = PERMO
         AND    BEGDA      <= P_WA_TAX_COMP_FPBEG
         AND    ENDDA      >= P_WA_TAX_COMP_FPBEG.
    EXIT.
  ENDSELECT.

  P_WA_TAX_COMP_PBEGDA = WA_T549Q-BEGDA.
  P_WA_TAX_COMP_PENDDA = WA_T549Q-ENDDA.

ENDFORM.                    " GET_MONTH_BEGDA_ENDDA
*&---------------------------------------------------------------------*
*&      Form  READ_COCD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TMP_COCD      text
*      -->P_P0000         text
*      -->P_P0001         text
*      -->P_P0580         text
*      -->P_TAN_NO        text
*      -->P_PERNR-PERNR   text
*      -->P_MULTIPLE_F16  text
*      -->P_PBEGDA        text
*      -->P_PENDDA        text
*----------------------------------------------------------------------*
FORM READ_COCD TABLES P_TMP_COCD STRUCTURE TMP_COCD
                      P_P0000  STRUCTURE P0000
                      P_P0001  STRUCTURE P0001
                      P_P0580 STRUCTURE P0580
                USING P_TAN_NO
                      P_PERNR-PERNR
                      P_MULTIPLE_F16
                      P_PBEGDA
                      P_PENDDA.

  CALL FUNCTION 'HR_IN_CALC_F16'
    EXPORTING
      EMPNO             = P_PERNR-PERNR
      MULTIPLE_F16      = P_MULTIPLE_F16
      FYBEGDA           = P_PBEGDA
      FYENDDA           = P_PENDDA
    TABLES
      P0000             = P_P0000
      P0001             = P_P0001
      P0580             = P_P0580
      COCD              = P_TMP_COCD
      F16               = F16
    EXCEPTIONS
      NO_RGDIR          = 1
      NO_CLUSTER_RESULT = 2
      OTHERS            = 3.
  IF SY-SUBRC <> 0.

  ENDIF.

ENDFORM.                    "READ_COCD


*&---------------------------------------------------------------------*
*&      Form  GET_ADDRESS
*&---------------------------------------------------------------------*
*       Get the address using the address number
*----------------------------------------------------------------------*
*  -->  $adrnum   Address number
*----------------------------------------------------------------------*
FORM GET_ADDRESS USING $ADRNUM.
  DATA:  AA(1), BB(1), CC(1), DD(1), EE(1),
         FF(1), GG(1), HH(1), II(1).

  DATA:  ADRNR LIKE HRCA_COMPANY-ADDRESS.
  DATA: SADR1 LIKE SADR OCCURS 10 WITH HEADER LINE.

  DATA:  SELECTION LIKE ADDR1_SEL,
         ADDRESS_VALUE LIKE ADDR1_VAL.

* To get the address for the number specified.

  SELECTION-ADDRNUMBER = $ADRNUM.        "SADR40A (check TVKO-ADRNR)

  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      ADDRESS_SELECTION = SELECTION
      ADDRESS_GROUP     = 'CA01' "to read SADR for unchanged
    IMPORTING                       "data
      ADDRESS_VALUE     = ADDRESS_VALUE  "both structures filled
      SADR              = SADR  "choose one of them
    EXCEPTIONS
      ADDRESS_NOT_EXIST = 1
      OTHERS            = 2.                                "SADR40A

  IF SADR-NAME1 IS INITIAL. AA = 'N'. ENDIF.
  IF SADR-NAME2 IS INITIAL. BB = 'N'. ENDIF.
  IF SADR-NAME3 IS INITIAL. CC = 'N'. ENDIF.
  IF SADR-NAME4 IS INITIAL. DD = 'N'. ENDIF.
  IF SADR-STRAS IS INITIAL. EE = 'N'. ENDIF.
  IF SADR-PFACH IS INITIAL. FF = 'N'. ENDIF.
  IF SADR-PSTLZ IS INITIAL. GG = 'N'. ENDIF.
  IF SADR-ORT01 IS INITIAL. HH = 'N'. ENDIF.
  IF SADR-ORT02 IS INITIAL. II = 'N'. ENDIF.

  PERFORM CONVERT_TO_SCRIPTVAR USING 'A' AA.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'B' BB.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'C' CC.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'D' DD.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'E' EE.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'F' FF.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'G' GG.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'H' HH.
  PERFORM CONVERT_TO_SCRIPTVAR USING 'I' II.
ENDFORM.                               " ADDRESS
