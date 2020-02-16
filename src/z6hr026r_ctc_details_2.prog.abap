*&---------------------------------------------------------------------*
*& Report  Z6HR026R_CTC_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT   Z6HR026R_CTC_DETAILS
         MESSAGE-ID ZH
         LINE-SIZE 132
         LINE-COUNT 60(3)
      NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: CTC Report
* OBJECT TYPE       : Report                FUNC. CONSULTANT  : Himalee
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 01.10.2010
*        DEV REQUEST: IRDK901101
*  TCODE            : ZCTC
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:   PUNAM                     DATE:   DD.MM.YYYY
*        DESCRIPTION:  CHANGES AS PER PRADNYA JOSHI
*----------------------------------------------------------------------*
TYPE-POOLS : SLIS.

INCLUDE PC2RXIN0.
INCLUDE RPC2RX09.

INFOTYPES : 0000,0001 , 0002, 0008, 0014, 0015, 0589, 0581. ",0185.

TABLES : PERNR,
         T5W7A,
         T503T,
         T510,
         T512T,
         PA0185,
         Z6HR_EMP_GRADE.

FIELD-SYMBOLS: <HSL> , <WTF>.

DATA : W_HSL(14) , W_CTR(2) TYPE N, CHOSEN TYPE I, ZTYPE, ZTYPEDESC(25),
      DESC(40), W_WTF(14), MPERIOD(6), LGART LIKE PA0008-LGA01,
      ENAME LIKE P0002-CNAME.
DATA : W_AWDYEAR1(4),
       W_AWDYEAR2(4).


DATA : CURR_YR(4) TYPE N,
      W_BEGDA LIKE SY-DATUM,
      W_ENDDA LIKE SY-DATUM.

DATA : BEGIN OF ZT5W7A OCCURS 0.
        INCLUDE STRUCTURE T5W7A.
DATA : END OF ZT5W7A.

DATA : BEGIN OF I_T512T OCCURS 0,
       LGART LIKE T512T-LGART,
       LGTXT LIKE T512T-LGTXT,
       END OF I_T512T.

DATA I_P0001 TYPE P0001 OCCURS 0 WITH HEADER LINE.
DATA I_P0002 TYPE P0002 OCCURS 0 WITH HEADER LINE.
DATA I_P0008 TYPE P0008 OCCURS 0 WITH HEADER LINE.
DATA I_P0014 TYPE P0014 OCCURS 0 WITH HEADER LINE.
DATA I_P0015 TYPE P0015 OCCURS 0 WITH HEADER LINE.
DATA I_P0015_1 TYPE P0015 OCCURS 0 WITH HEADER LINE.
DATA I_P0589 TYPE P0589 OCCURS 0 WITH HEADER LINE.
DATA I_P0581 TYPE P0581 OCCURS 0 WITH HEADER LINE.
DATA I_P0000 TYPE P0000 OCCURS 0 WITH HEADER LINE.
DATA I_P00001 TYPE PA0000 OCCURS 0 WITH HEADER LINE.

DATA : SRCHLGART LIKE PA0008-LGA01.

DATA: BEGIN OF FINTAB OCCURS 0,
      PERNR        LIKE PA0008-PERNR,
      LGART        LIKE PA0008-LGA01,
      CHOSEN       LIKE PA0008-BET01,
      CHOSEN_MON   LIKE PA0008-BET01,
     END OF FINTAB.

DATA: BEGIN OF PRNTAB OCCURS 0,
      ZTYPE(2),
      ZTYPDESC(25),
      PERNR    LIKE PA0008-PERNR,
      IT(2),
      LGART    LIKE PA0008-LGA01,
      DESC(40),
      STEXT    LIKE T5W7AT-STEXT,
      CHOSEN   LIKE PA0008-BET01,
      END OF PRNTAB.

DATA: BEGIN OF ZPRNTAB OCCURS 0,
      ZTYPE(1),
      ZTYPDESC(25),
      PERNR    LIKE PA0008-PERNR,
      LGART    LIKE PA0008-LGA01,
      DESC(40),
      CHOSEN(15),
     END OF ZPRNTAB.

DATA : BEGIN OF EMP_DOJ  OCCURS 0,
       PERNR TYPE P0001-PERNR,
       DOJ   TYPE P0000-BEGDA,
       END OF EMP_DOJ.

DATA :ZMBETRG LIKE PA0008-BET01,
       ZZMBETRG LIKE PA0008-BET01,
       ZZMBETRG_MON LIKE PA0008-BET01,
       ZMMONTHS(2) TYPE N.

DATA : WA_BONUS   LIKE ZPRNTAB-CHOSEN,
       WA_PF      LIKE ZPRNTAB-CHOSEN,
       WA_GRT     LIKE ZPRNTAB-CHOSEN,
       WA_SANN    LIKE ZPRNTAB-CHOSEN.

DATA : IT_T510 TYPE STANDARD TABLE OF T510.
DATA: WA_T510 LIKE LINE OF IT_T510.

DATA WUNAME LIKE SY-UNAME.
DATA WPERNR LIKE PA0001-PERNR.
DATA WBUKRS LIKE PA0001-BUKRS.
DATA WABKRS LIKE PA0001-ABKRS.
DATA WA_START_DT LIKE PA0185-BEGDA.
DATA NO_YEAR TYPE I.

FIELD-SYMBOLS : <TABLE>    TYPE  TABLE ,     " Main Internal Table
                <STRUC> ,
                <TABLE1>   TYPE  TABLE,
                <STRUC1>,
                <TABLE2>   TYPE  TABLE,
                <STRUC2>,

                <TABLE3>   TYPE  TABLE,
                <STRUC3>,
                <WSTRUC>,                    " Header Struct for <table>
                <FIELD> ,
                <COMPONENT> .



DATA      :   ALV_FIELDCAT TYPE              SLIS_T_FIELDCAT_ALV ,
              LT_ALV_CAT   TYPE  TABLE OF    LVC_S_FCAT ,
              LT_ALV_CAT1   TYPE  TABLE OF    LVC_S_FCAT ,
              LT_ALV_CAT2   TYPE  TABLE OF    LVC_S_FCAT ,
              LT_ALV_CAT3   TYPE  TABLE OF    LVC_S_FCAT ,
              IT_FIELDCAT  LIKE  LINE  OF    LT_ALV_CAT .

DATA      :   I_TABLE      TYPE  REF   TO    DATA ,
              I_STRUCT     TYPE  REF   TO    DATA ,
              TABIX        TYPE SY-TABIX.


DATA : BEGIN OF IT_FIELD_DYNAMIC OCCURS 0,
       FIELDNAME LIKE DD03L-FIELDNAME,
       REFTAB  LIKE DD03L-TABNAME,
       REFFIELD  LIKE DD03L-REFFIELD,
       DESC(35),
      END OF IT_FIELD_DYNAMIC.


DATA : G_SAVE(1)            TYPE  C                      ,
       G_EXIT(1)            TYPE  C                      ,
       G_VARIANT            LIKE  DISVARIANT             ,
       GX_VARIANT           LIKE  DISVARIANT             ,
       F2CODE               LIKE  SY-UCOMM VALUE  '&ETA' ,
       LAYOUT               TYPE  SLIS_LAYOUT_ALV        ,
       PRG                  LIKE  SY-REPID               ,
       KEYINFO  TYPE SLIS_KEYINFO_ALV,
       GT_LIST_TOP_OF_PAGE  TYPE  SLIS_T_LISTHEADER      ,
       GT_EVENTS            TYPE  SLIS_T_EVENT           ,
       FIELDCAT             TYPE  SLIS_T_FIELDCAT_ALV    .

*      --------------------------------------------------------*
*      INITIALIZATION.
*      --------------------------------------------------------*

INITIALIZATION.
  IF SY-TCODE = 'ZHR_CTC'. "'ZCTC'.
    WUNAME = SY-UNAME.
    CLEAR: WPERNR, WABKRS, WBUKRS.
    CALL FUNCTION 'HR_GETEMPLOYEEDATA_FROMUSER'
      EXPORTING
        USERNAME                  = WUNAME
        VALIDBEGIN                = SY-DATUM
      IMPORTING
        EMPLOYEENUMBER            = WPERNR
        PAYROLLAREA               = WABKRS
        COMPANYCODE               = WBUKRS
      EXCEPTIONS
        USER_NOT_FOUND            = 1
        COUNTRYGROUPING_NOT_FOUND = 2
        INFTY_NOT_FOUND           = 3
        OTHERS                    = 4.
    IF SY-SUBRC = 0.
      PNPPERNR-LOW = WPERNR. PNPPERNR-OPTION = 'EQ'. PNPPERNR-SIGN = 'I'.
      APPEND PNPPERNR.
      PNPBUKRS-LOW = WBUKRS. PNPBUKRS-OPTION = 'EQ'. PNPBUKRS-SIGN = 'I'.
      APPEND PNPBUKRS.
      PNPABKRS-LOW = WABKRS. PNPABKRS-OPTION = 'EQ'. PNPABKRS-SIGN = 'I'.
      APPEND PNPABKRS.
    ENDIF.

  ENDIF.

START-OF-SELECTION.

GET PERNR.
  PERFORM DISPLAY_LIST .
  PERFORM GETDATA.

END-OF-SELECTION.

  PERFORM CUMMDATA.
  PERFORM F_FILL_FIELDCAT_STRUCT_DYN.
  PERFORM F_FIELD_CAT_FOR_DYN_TABLE.
  PERFORM F_CREATE_DYNAMIC_TABLE.
  PERFORM GET_DISPLAY_DATA.
  PERFORM F_PRINT_DATA.
*&---------------------------------------------------------------------*
*&      Form  GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETDATA.
  RP_PROVIDE_FROM_LAST P0001 SPACE SY-DATUM SY-DATUM.
  IF PNP-SW-FOUND = 1.
    MOVE-CORRESPONDING P0001 TO I_P0001.
    APPEND I_P0001.
  ENDIF.

  RP_PROVIDE_FROM_LAST P0002 SPACE SY-DATUM SY-DATUM.
  IF PNP-SW-FOUND = 1.
    MOVE-CORRESPONDING P0002 TO I_P0002.
    APPEND I_P0002.
  ENDIF.

  RP_PROVIDE_FROM_LAST P0008 SPACE SY-DATUM SY-DATUM.
  IF PNP-SW-FOUND = 1.
    MOVE-CORRESPONDING P0008 TO I_P0008.
    APPEND I_P0008.
  ENDIF.

  LOOP AT P0014 WHERE BEGDA <= SY-DATUM AND ENDDA >= SY-DATUM.
    MOVE-CORRESPONDING P0014 TO I_P0014.
    APPEND I_P0014.
  ENDLOOP.
  IF SY-DATUM+4(2) > 3.
    CURR_YR = SY-DATUM+0(4).
    CONCATENATE CURR_YR '0401' INTO W_BEGDA.
    CURR_YR = CURR_YR + 1.
    CONCATENATE CURR_YR '0331' INTO W_ENDDA.
  ELSE.
    CURR_YR = SY-DATUM+0(4).
    CURR_YR = CURR_YR - 1.
    CONCATENATE CURR_YR '0401' INTO W_BEGDA.
    CURR_YR = SY-DATUM+0(4).
    CONCATENATE CURR_YR '0331' INTO W_ENDDA.
  ENDIF.
  W_AWDYEAR1 = W_BEGDA+0(4).
  W_AWDYEAR1 = W_AWDYEAR1 - 1.
  W_AWDYEAR2 = W_AWDYEAR1 + 1.

  LOOP AT P0015   .
    MOVE-CORRESPONDING P0015 TO I_P0015.
    APPEND I_P0015.
    CLEAR P0015.
  ENDLOOP.

  RP_PROVIDE_FROM_LAST P0589 SPACE SY-DATUM SY-DATUM.
  IF PNP-SW-FOUND = 1.
    MOVE-CORRESPONDING P0589 TO I_P0589.
    APPEND I_P0589.
  ENDIF.

  RP_PROVIDE_FROM_LAST P0581 SPACE SY-DATUM SY-DATUM.
  IF PNP-SW-FOUND = 1.
    MOVE-CORRESPONDING P0581 TO I_P0581.
    APPEND I_P0581.
  ENDIF.

  RP_PROVIDE_FROM_LAST P0000 SPACE SY-DATUM SY-DATUM.
  IF PNP-SW-FOUND = 1.
    MOVE-CORRESPONDING P0000 TO I_P0000.
    APPEND I_P0000.
  ENDIF.
ENDFORM.                    " GETDATA

*&---------------------------------------------------------------------*
*&      Form  CUMMDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CUMMDATA.

  SELECT * FROM T5W7A INTO TABLE ZT5W7A.

  LOOP AT I_P0001.

    SELECT LGART LGTXT FROM T512T
    INTO CORRESPONDING FIELDS OF TABLE I_T512T
    WHERE SPRSL = 'E'
    AND MOLGA = '40'.


    IF NOT I_T512T[] IS INITIAL.
      SORT I_T512T BY LGART.
    ENDIF.


    PERFORM FINTAB.
***IT 8
    LOOP AT I_P0008 WHERE PERNR = I_P0001-PERNR.
      W_CTR = 1.
      WHILE W_CTR <= 20.
        CONCATENATE 'i_P0008-LGA' W_CTR INTO W_HSL.
        CONCATENATE 'i_P0008-BET' W_CTR INTO W_WTF.
        ASSIGN (W_HSL) TO <HSL>.
        ASSIGN (W_WTF) TO <WTF>.
        IF NOT <HSL> IS INITIAL.


****additional patch for including 9FME CODE BEGIN

          READ TABLE PRNTAB WITH KEY IT = 8  LGART = <HSL>.
          IF SY-SUBRC = 0.
            IF NOT  <WTF> = 0.
              CLEAR :  ZMBETRG , ZZMBETRG.
              MOVE <WTF> TO ZMBETRG.
              IF ZMBETRG > 0.
                ZZMBETRG =  ZMBETRG * 12.
                ZZMBETRG_MON =  ZMBETRG.
              ENDIF.
              MOVE <HSL> TO SRCHLGART.
              READ TABLE FINTAB WITH KEY PERNR = I_P0001-PERNR
                    LGART = SRCHLGART BINARY SEARCH.
              IF SY-SUBRC NE 0.
                CLEAR FINTAB.
                MOVE I_P0001-PERNR TO FINTAB-PERNR.
                MOVE <HSL>         TO FINTAB-LGART.
                MOVE ZZMBETRG      TO FINTAB-CHOSEN.
                MOVE ZZMBETRG_MON  TO FINTAB-CHOSEN_MON.
                INSERT  FINTAB INDEX SY-TABIX.
              ELSE.
                FINTAB-CHOSEN = FINTAB-CHOSEN + ZZMBETRG.
                FINTAB-CHOSEN_MON = FINTAB-CHOSEN_MON + ZZMBETRG_MON.
                MODIFY FINTAB INDEX SY-TABIX.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        W_CTR = W_CTR + 1.
      ENDWHILE.
    ENDLOOP.

    SELECT SINGLE BETRG
      FROM PA0014 INTO FINTAB-CHOSEN_MON
      WHERE PERNR = I_P0001-PERNR
      AND SUBTY = '1116'
      AND ENDDA = '99991231'.

    IF SY-SUBRC = 0.
      FINTAB-PERNR = I_P0001-PERNR.
      FINTAB-LGART = '1116'.
      FINTAB-CHOSEN = FINTAB-CHOSEN_MON * 12.
      APPEND FINTAB.
      CLEAR FINTAB.
    ENDIF.

*    BEGIN OF FINTAB OCCURS 0,
*      PERNR        LIKE PA0008-PERNR,
*      LGART        LIKE PA0008-LGA01,
*      CHOSEN       LIKE PA0008-BET01,
*      CHOSEN_MON   LIKE PA0008-BET01,
*     END OF FINTAB.

****IT 14
*    LOOP AT i_p0014 WHERE pernr = i_p0001-pernr.
*      READ TABLE prntab WITH KEY lgart = i_p0014-lgart
*      it = '14'.
*      IF sy-subrc NE 0.
*        DELETE i_p0014 WHERE lgart = i_p0014-lgart.
*      ENDIF.
*    ENDLOOP.
*    CLEAR i_p0014.
*
*    SORT i_p0014 BY pernr begda endda.
*    LOOP AT i_p0014 WHERE pernr = i_p0001-pernr.
*      CLEAR :  zmbetrg , zzmbetrg.
*      MOVE i_p0014-betrg TO zmbetrg.
*      zzmbetrg =  zmbetrg * 12.
*      READ TABLE fintab WITH KEY pernr = i_p0014-pernr
*            lgart =  i_p0014-lgart BINARY SEARCH.
*      IF sy-subrc NE 0.
*        CLEAR fintab.
*        MOVE i_p0001-pernr TO fintab-pernr.
*        MOVE i_p0014-lgart TO fintab-lgart.
*        MOVE zzmbetrg      TO fintab-chosen.
*        INSERT  fintab INDEX sy-tabix.
*      ELSE.
*        fintab-chosen = fintab-chosen + zzmbetrg.
*        MODIFY fintab INDEX sy-tabix.
*      ENDIF.
*    ENDLOOP.
*********IT 15

*    CLEAR : i_p0581.", year.", zbegda, zendda.
*    SORT i_p0581 BY pernr begda endda.
*    LOOP AT i_p0581 WHERE pernr = i_p0001-pernr.
*      CLEAR :  zmbetrg , zzmbetrg.
*      MOVE i_p0581-RTAMT TO zmbetrg.
*      zzmbetrg =  zmbetrg.
*      READ TABLE fintab WITH KEY pernr = i_p0001-pernr
*      BINARY SEARCH.
**            lgart =  i_p0015-lgart BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        CLEAR fintab.
*        MOVE i_p0001-pernr TO fintab-pernr.
**        MOVE i_p0015-lgart TO fintab-lgart.
*        fintab-lgart = 'LE12'.
*        MOVE zzmbetrg      TO fintab-chosen.
*        INSERT  fintab INDEX sy-tabix.
*      ENDIF.
*    ENDLOOP.

********IT 15
    CLEAR : I_P0015.", year.", zbegda, zendda.
    SORT I_P0015 BY PERNR BEGDA ENDDA .

    CLEAR: I_P0015_1,I_P0015_1[].
    LOOP AT I_P0015 WHERE PERNR = I_P0001-PERNR.
      MOVE-CORRESPONDING I_P0015 TO I_P0015_1.
      APPEND I_P0015_1.
      CLEAR I_P0015_1.
    ENDLOOP.


    DELETE I_P0015_1 WHERE LGART NE '1106'.
    SORT I_P0015_1 BY BEGDA  DESCENDING .
*    LOOP AT i_p0015 WHERE pernr = i_p0001-pernr.

    READ TABLE I_P0015_1 INDEX 1.
    IF SY-SUBRC = 0.
      CLEAR :  ZMBETRG , ZZMBETRG.
      MOVE I_P0015_1-BETRG TO ZMBETRG.
      ZZMBETRG =  ZMBETRG.
      READ TABLE FINTAB WITH KEY PERNR = I_P0001-PERNR
             LGART =  '1106' BINARY SEARCH.
*            lgart =  i_p0015-lgart BINARY SEARCH.
      IF SY-SUBRC NE 0.
        CLEAR FINTAB.
        MOVE I_P0001-PERNR TO FINTAB-PERNR.
        MOVE I_P0015_1-LGART TO FINTAB-LGART.
        MOVE ZZMBETRG      TO FINTAB-CHOSEN.
        INSERT  FINTAB INDEX SY-TABIX.
*      ELSE.
*        fintab-chosen = fintab-chosen + zzmbetrg.
*        MODIFY fintab INDEX sy-tabix.
      ENDIF.
    ENDIF.
*    ENDLOOP.
*****Infotype 589
    LOOP AT I_P0589 WHERE PERNR = I_P0001-PERNR.
      MOVE 12 TO ZMMONTHS.
      W_CTR = 1.
      WHILE W_CTR <= 15.
        CONCATENATE 'i_P0589-LGA' W_CTR INTO W_HSL.
        CONCATENATE 'i_P0589-BET' W_CTR INTO W_WTF.
        ASSIGN (W_HSL) TO <HSL>.
        ASSIGN (W_WTF) TO <WTF>.
        IF  <HSL> = '1101'.
          IF NOT  <WTF> = 0.
            CLEAR :  ZMBETRG , ZZMBETRG.
            MOVE <WTF> TO ZMBETRG.
            IF ZMBETRG > 0.
              ZZMBETRG =  ZMBETRG.
            ENDIF.
            MOVE <HSL> TO SRCHLGART.
            READ TABLE FINTAB WITH KEY PERNR = I_P0001-PERNR
                  LGART = SRCHLGART BINARY SEARCH.
            IF SY-SUBRC NE 0.
              CLEAR FINTAB.
              MOVE I_P0001-PERNR TO FINTAB-PERNR.
              MOVE <HSL>         TO FINTAB-LGART.
              MOVE ZZMBETRG      TO FINTAB-CHOSEN.
              INSERT  FINTAB INDEX SY-TABIX.
            ELSE.
              MOVE ZZMBETRG      TO FINTAB-CHOSEN.
              MODIFY FINTAB INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ENDIF.
******************NEW CODE FOR INTRODUCING IT 589  CODE 3730
        IF  <HSL> = '1102'.
          IF NOT  <WTF> = 0.
            CLEAR :  ZMBETRG , ZZMBETRG.
            MOVE <WTF> TO ZMBETRG.
            IF ZMBETRG > 0.
              ZZMBETRG =  ZMBETRG.
            ENDIF.
*            MOVE '3610' TO srchlgart.
            MOVE '1102' TO SRCHLGART.
            READ TABLE FINTAB WITH KEY PERNR = I_P0001-PERNR
                  LGART =  SRCHLGART BINARY SEARCH.
            IF SY-SUBRC NE 0.
              CLEAR FINTAB.
              MOVE I_P0001-PERNR TO FINTAB-PERNR.
              MOVE '1102'        TO FINTAB-LGART.
              MOVE ZZMBETRG      TO FINTAB-CHOSEN.
              INSERT  FINTAB INDEX SY-TABIX.
            ELSE.
              FINTAB-CHOSEN = FINTAB-CHOSEN + ZZMBETRG.
*              MOVE zzmbetrg      TO fintab-chosen.
              MODIFY FINTAB INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ENDIF.
******************NEW CODE FOR INTRODUCING IT 589  CODE 3730
        W_CTR = W_CTR + 1.
      ENDWHILE.
    ENDLOOP.

*  ---for bonus ,pf ,gratuity,superannuation


*CHANGES MADE BY PUNAM to remove bonus ,pf ,gratuity,superannuation for trainee employees.
*    IF I_P0001-PLANS <> '50000693'.

    READ TABLE FINTAB WITH KEY LGART = '1001'
                               PERNR = I_P0001-PERNR.
    IF SY-SUBRC = 0.
*
      IF I_P0001-PERSG = 'S' OR I_P0001-PERSG = 'W'.
        WA_BONUS = FINTAB-CHOSEN * '0.20'.
      ELSE.
        IF FINTAB-CHOSEN > '120000'.
          WA_BONUS = 0.
        ELSEIF FINTAB-CHOSEN <= '120000'.
          WA_BONUS = FINTAB-CHOSEN * '0.20'.
        ENDIF.
      ENDIF.

      WA_PF    = FINTAB-CHOSEN * '0.12' .
      WA_GRT   = FINTAB-CHOSEN * '0.0481'.
*              wa_sann
      CLEAR FINTAB.
      MOVE I_P0001-PERNR TO FINTAB-PERNR.
      MOVE 'C3G1'        TO FINTAB-LGART.
      FINTAB-CHOSEN = WA_BONUS .
      INSERT  FINTAB INDEX SY-TABIX.

      CLEAR FINTAB.
      MOVE I_P0001-PERNR TO FINTAB-PERNR.
      MOVE 'C3G2'        TO FINTAB-LGART.
      FINTAB-CHOSEN = WA_PF .
      INSERT  FINTAB INDEX SY-TABIX.

      IF I_P0001-PERSG <> 'T'.
        CLEAR FINTAB.
        MOVE I_P0001-PERNR TO FINTAB-PERNR.
        MOVE 'C3G3'        TO FINTAB-LGART.
        FINTAB-CHOSEN = WA_GRT .
        INSERT  FINTAB INDEX SY-TABIX.
      ENDIF.
    ENDIF.

*    ENDIF.
* -----
*    ------super annuation

    PERFORM GET_SUPERANN_DATA.

    PERFORM GET_T510_DATA.
  ENDLOOP.
ENDFORM.                    " CUMMDATAk



*&---------------------------------------------------------------------*
*&      Form  FINTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FINTAB.
  CLEAR PRNTAB.
  REFRESH PRNTAB.

  MOVE 'A' TO PRNTAB-ZTYPE.
  MOVE 'BASIC : ' TO PRNTAB-ZTYPDESC.
  MOVE '1001' TO PRNTAB-LGART.
  MOVE 'Basic Pay' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'A1' TO PRNTAB-ZTYPE.
  MOVE 'BASIC M : ' TO PRNTAB-ZTYPDESC.
  MOVE 'M001' TO PRNTAB-LGART.
  MOVE 'Monthly Basic' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'B' TO PRNTAB-ZTYPE.
  MOVE 'HRA : ' TO PRNTAB-ZTYPDESC.
  MOVE '1004' TO PRNTAB-LGART.
  MOVE 'House Rent Allowance .' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'B1' TO PRNTAB-ZTYPE.
  MOVE 'HRA M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M004' TO PRNTAB-LGART.
  MOVE 'Monthly HRA' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'C' TO PRNTAB-ZTYPE.
  MOVE 'SPA : ' TO PRNTAB-ZTYPDESC.
  MOVE '1007' TO PRNTAB-LGART.
  MOVE 'Special Allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'C1' TO PRNTAB-ZTYPE.
  MOVE 'SPA M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M007' TO PRNTAB-LGART.
  MOVE 'Monthly SPL.allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

*((((((((((((((((((((((((((((((((((((((
  MOVE 'D' TO PRNTAB-ZTYPE.
  MOVE 'OA : ' TO PRNTAB-ZTYPDESC.
  MOVE '1013' TO PRNTAB-LGART.
  MOVE 'Other Allowance .' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'D1' TO PRNTAB-ZTYPE.
  MOVE 'MOA : ' TO PRNTAB-ZTYPDESC.
  MOVE 'M013' TO PRNTAB-LGART.
  MOVE 'Monthly Other Allowance .' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.
*)))))))))))))))))))))))))))))))))))


  MOVE 'E' TO PRNTAB-ZTYPE. "D
  MOVE 'EDA : ' TO PRNTAB-ZTYPDESC.
  MOVE '1005' TO PRNTAB-LGART.
  MOVE 'Educational Allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'E1' TO PRNTAB-ZTYPE."D1
  MOVE 'EDA M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M005' TO PRNTAB-LGART.
  MOVE 'Monthly Edu. Allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'F' TO PRNTAB-ZTYPE."E
  MOVE 'CNA : ' TO PRNTAB-ZTYPDESC.
  MOVE '1006' TO PRNTAB-LGART.
  MOVE 'Conveyance Allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'F1' TO PRNTAB-ZTYPE."E1
  MOVE 'CNA M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M006' TO PRNTAB-LGART.
  MOVE 'Monthly Conveyance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'G' TO PRNTAB-ZTYPE."F
  MOVE 'TEL M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M019' TO PRNTAB-LGART.
  MOVE 'Monthly.Telephone Reimb.' TO PRNTAB-DESC.
  MOVE '510'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'G1' TO PRNTAB-ZTYPE."F1
  MOVE 'TEL : ' TO PRNTAB-ZTYPDESC.
  MOVE '1019' TO PRNTAB-LGART.
  MOVE ' Telephone Reimbursement.' TO PRNTAB-DESC.
  MOVE '510'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'H' TO PRNTAB-ZTYPE."G
  MOVE 'ENT M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M020' TO PRNTAB-LGART.
  MOVE 'Monthly.Ent. Allowance. ' TO PRNTAB-DESC.
  MOVE '589'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'H1' TO PRNTAB-ZTYPE."G1
  MOVE 'ENT' TO PRNTAB-ZTYPDESC.
  MOVE '1020' TO PRNTAB-LGART.
  MOVE 'Entertainment. Allowance. ' TO PRNTAB-DESC.
  MOVE '589'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'I' TO PRNTAB-ZTYPE."H
  MOVE 'CAR M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M008' TO PRNTAB-LGART.
  MOVE 'Monthly Car Allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'I1' TO PRNTAB-ZTYPE."H
  MOVE 'CAR : ' TO PRNTAB-ZTYPDESC.
  MOVE '1008' TO PRNTAB-LGART.
  MOVE 'Car Allowance' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

*  ---MONTHLY P.A.
  MOVE 'J' TO PRNTAB-ZTYPE."I
  MOVE 'MON : ' TO PRNTAB-ZTYPDESC.
  MOVE 'MON1' TO PRNTAB-LGART.
  MOVE 'MONTHLY' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'K' TO PRNTAB-ZTYPE."J
  MOVE 'P.A. : ' TO PRNTAB-ZTYPDESC.
  MOVE 'ANNM' TO PRNTAB-LGART.
  MOVE 'P.A.' TO PRNTAB-DESC.
  APPEND PRNTAB.

*----
  MOVE 'L' TO PRNTAB-ZTYPE."K
  MOVE 'BONUS : ' TO PRNTAB-ZTYPDESC.
  MOVE 'C3G1' TO PRNTAB-LGART.
  MOVE 'Bonus' TO PRNTAB-DESC.
  APPEND PRNTAB.


  MOVE 'M' TO PRNTAB-ZTYPE."L
  MOVE 'MED : ' TO PRNTAB-ZTYPDESC.
  MOVE '1102' TO PRNTAB-LGART.
  MOVE 'Medical Reimbursement .' TO PRNTAB-DESC.
  MOVE '589'  TO PRNTAB-IT.
  APPEND PRNTAB.

*  MOVE 'I' TO prntab-ztype.
*  MOVE 'Lease : ' TO prntab-ztypdesc.
*  MOVE 'LE12' TO prntab-lgart.
*  MOVE 'Lease Value' TO prntab-desc.
*  MOVE '581'  TO prntab-it.
*  APPEND prntab.

  MOVE 'N' TO PRNTAB-ZTYPE."M
  MOVE 'Lease : ' TO PRNTAB-ZTYPDESC.
  MOVE '1015' TO PRNTAB-LGART.
  MOVE 'Lease Value' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'N' TO PRNTAB-ZTYPE."M
  MOVE 'Lease : ' TO PRNTAB-ZTYPDESC.
  MOVE '1016' TO PRNTAB-LGART.
  MOVE 'Comp Acc.' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

*  --mONTHLY
  MOVE 'N1' TO PRNTAB-ZTYPE."M1
  MOVE 'Lease M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M015' TO PRNTAB-LGART.
  MOVE 'Lease Value Monthlty' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'N1' TO PRNTAB-ZTYPE."M1
  MOVE 'Lease M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M016' TO PRNTAB-LGART.
  MOVE 'Comp Acc.Monthlty' TO PRNTAB-DESC.
  MOVE '8'  TO PRNTAB-IT.
  APPEND PRNTAB.

*--
  MOVE 'O' TO PRNTAB-ZTYPE."N
  MOVE 'LTA : ' TO PRNTAB-ZTYPDESC.
  MOVE '1101' TO PRNTAB-LGART.
  MOVE 'Leave Travel Allowance' TO PRNTAB-DESC.
  MOVE '589'  TO PRNTAB-IT.
  APPEND PRNTAB.



  MOVE 'P' TO PRNTAB-ZTYPE."O
  MOVE 'HFS : ' TO PRNTAB-ZTYPDESC.
  MOVE '1018' TO PRNTAB-LGART.
  MOVE 'Hard Furnishing' TO PRNTAB-DESC.
  MOVE '589'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'Q' TO PRNTAB-ZTYPE."P
  MOVE 'TOT : ' TO PRNTAB-ZTYPDESC.
  MOVE 'TOT1' TO PRNTAB-LGART.
  MOVE 'TOTAL 1' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'R' TO PRNTAB-ZTYPE."Q
  MOVE 'PF : ' TO PRNTAB-ZTYPDESC.
  MOVE 'C3G2' TO PRNTAB-LGART.
  MOVE 'PF' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'R' TO PRNTAB-ZTYPE."Q
  MOVE 'SUO : ' TO PRNTAB-ZTYPDESC.
  MOVE 'C3G4' TO PRNTAB-LGART.
  MOVE 'Super Annuation' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'S' TO PRNTAB-ZTYPE."R
  MOVE 'GRATUITY : ' TO PRNTAB-ZTYPDESC.
  MOVE 'C3G3' TO PRNTAB-LGART.
  MOVE 'GRATUITY' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'T' TO PRNTAB-ZTYPE."S
  MOVE 'TOTAL : ' TO PRNTAB-ZTYPDESC.
  MOVE 'TOT2' TO PRNTAB-LGART.
  MOVE 'TOTAL 2' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'U' TO PRNTAB-ZTYPE."T
  MOVE 'TFP : ' TO PRNTAB-ZTYPDESC.
  MOVE 'CTC1' TO PRNTAB-LGART.
  MOVE 'Total Fixed Pay' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'U1' TO PRNTAB-ZTYPE."T1
  MOVE 'PER : ' TO PRNTAB-ZTYPDESC.
  MOVE '1106' TO PRNTAB-LGART.
  MOVE 'Performance Incentive' TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'U2' TO PRNTAB-ZTYPE."T2
  MOVE 'TCTC : ' TO PRNTAB-ZTYPDESC.
  MOVE 'TCTC'    TO PRNTAB-LGART.
  MOVE 'CTC'     TO PRNTAB-DESC.
  APPEND PRNTAB.

  MOVE 'V' TO PRNTAB-ZTYPE."F
  MOVE 'Per.TEL M: ' TO PRNTAB-ZTYPDESC.
  MOVE 'M116' TO PRNTAB-LGART.
  MOVE 'IT 14 Month.Tel.Reimb.' TO PRNTAB-DESC.
  MOVE '14'  TO PRNTAB-IT.
  APPEND PRNTAB.

  MOVE 'V1' TO PRNTAB-ZTYPE."F1
  MOVE 'Per.TEL: ' TO PRNTAB-ZTYPDESC.
  MOVE '1116' TO PRNTAB-LGART.
  MOVE 'IT 14 Tel.Reimb.' TO PRNTAB-DESC.
  MOVE '14'  TO PRNTAB-IT.
  APPEND PRNTAB.


  SORT PRNTAB BY ZTYPE.

ENDFORM.                    " FINTAB

*&---------------------------------------------------------------------*
*&      Form  GET_COMPGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WCOMP_GROUP  text
*----------------------------------------------------------------------*
FORM GET_COMPGRP CHANGING P_WCOMP_GROUP.

  CALL FUNCTION 'HRCM_COMPENSATION_GROUP_GET'
        EXPORTING
             PERNR           = ZPRNTAB-PERNR
             BEGDA           = SY-DATUM
             ENDDA           = SY-DATUM
*           REACTION        = ' '
       IMPORTING
            CMPGR           = P_WCOMP_GROUP
*      TABLES
*           ERROR_TABLE     =
       EXCEPTIONS
            CMPGR_NOT_FOUND = 1
            OTHERS          = 2
             .
  IF SY-SUBRC <> 0.
*    message e004(zh) with Com
  ENDIF.

ENDFORM.                    " GET_COMPGRP
*&---------------------------------------------------------------------*
*&      Form  F_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PRINT_DATA .

  DATA : IT_SORT TYPE SLIS_SORTINFO_ALV OCCURS 1 WITH HEADER LINE.

  DATA FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA : V_MCNT TYPE I.
  IF NOT <TABLE> IS INITIAL.
    LOOP AT LT_ALV_CAT INTO IT_FIELDCAT.
      MOVE-CORRESPONDING IT_FIELDCAT TO FIELDCAT.
      IF SY-TABIX LE 2.
        FIELDCAT-KEY = 1.
      ENDIF.
      FIELDCAT-DDICTXT = 'M'.
      FIELDCAT-TABNAME  = '<TABLE>' .
      FIELDCAT-SELTEXT_L = IT_FIELDCAT-SELTEXT.
      FIELDCAT-SELTEXT_M = IT_FIELDCAT-SELTEXT.
      FIELDCAT-SELTEXT_L = IT_FIELDCAT-SELTEXT.
      APPEND FIELDCAT TO ALV_FIELDCAT .
      CLEAR FIELDCAT.
    ENDLOOP.


    DATA: LD_COLUMN      TYPE LVC_FNAME,
          LD_HIDE          TYPE C ,
          W_LINES TYPE I,
          W_NO TYPE I,
          WA_NO(4) TYPE C.
    FIELD-SYMBOLS:
       <LS_ENTRY>     TYPE ANY,
       <LD_FLD>         TYPE ANY.

    DESCRIBE TABLE ALV_FIELDCAT LINES W_LINES.

    LOOP AT IT_FIELD_DYNAMIC WHERE REFFIELD = 'BET01'.
      CLEAR LD_COLUMN.
      LD_COLUMN = IT_FIELD_DYNAMIC-FIELDNAME.
      CONDENSE LD_COLUMN.
      LOOP AT <TABLE> ASSIGNING <LS_ENTRY>.
        CLEAR LD_HIDE.
        ASSIGN COMPONENT LD_COLUMN OF STRUCTURE <LS_ENTRY> TO <LD_FLD>.

*        IF ( <ld_fld>   > '0.00' ) .
        IF ( <LD_FLD>   <> '0.00' ) .
          LD_HIDE = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      READ TABLE ALV_FIELDCAT INTO FIELDCAT
                           WITH KEY FIELDNAME = LD_COLUMN.
      IF ( SYST-SUBRC = 0 ).
        IF LD_HIDE = 'X'.
          FIELDCAT-NO_OUT = ''.
        ELSE.
          FIELDCAT-NO_OUT = 'X'.
        ENDIF.
        MODIFY ALV_FIELDCAT FROM FIELDCAT INDEX SYST-TABIX.
      ENDIF.
    ENDLOOP.
*    ----

    IT_SORT-SPOS = 1.
    IT_SORT-FIELDNAME = 'PERNR'.
    IT_SORT-UP = 'X'.
    IT_SORT-SUBTOT = ''.
    APPEND IT_SORT.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM     = SY-REPID
        I_BYPASSING_BUFFER     = 'X'
        I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
        IT_FIELDCAT            = ALV_FIELDCAT
        IT_SORT                = IT_SORT[]
        I_DEFAULT              = 'X'
        I_SAVE                 = 'A'
        IT_EVENTS              = GT_EVENTS
      TABLES
        T_OUTTAB               = <TABLE>.
  ENDIF.

*EXPORT LT_ALV_CAT = LT_ALV_CAT TO MEMORY ID 'ZFCAT'.
  EXPORT <TABLE> = <TABLE> TO MEMORY ID 'ZCOUNT'.
*IMPORT LT_ALV_CAT = LT_ALV_CAT FROM MEMORY ID 'ZFIELDCAT'.

*IMPORT it_TABLE = <TABLE> FROM MEMORY ID 'ZCOUNT'.

ENDFORM.                    " F_PRINT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FIELD_CAT_FOR_DYN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FIELD_CAT_FOR_DYN_TABLE .


  LOOP AT IT_FIELD_DYNAMIC.
    IF IT_FIELD_DYNAMIC-FIELDNAME EQ 'PERNR'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ENAME'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'STEXT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'PERSK'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ZZLOC_TEXT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'GBDAT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'BEGDA'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'KOSTL'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'KTEXT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ORGEH'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ORGTX'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'WERKS'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'NAME1'.
      IT_FIELDCAT-FIELDNAME = IT_FIELD_DYNAMIC-FIELDNAME.
      IT_FIELDCAT-COL_POS    = SY-TABIX.
      IT_FIELDCAT-REF_FIELD = IT_FIELD_DYNAMIC-FIELDNAME.
      IT_FIELDCAT-REF_TABLE = IT_FIELD_DYNAMIC-REFTAB.
      IT_FIELDCAT-SELTEXT   = IT_FIELD_DYNAMIC-DESC.
      COLLECT IT_FIELDCAT INTO LT_ALV_CAT .
      CLEAR IT_FIELDCAT .
    ELSE.
      IT_FIELDCAT-FIELDNAME = IT_FIELD_DYNAMIC-FIELDNAME.
      IT_FIELDCAT-COL_POS    = SY-TABIX.
      IT_FIELDCAT-REF_FIELD = 'BET01'.

*      it_fieldcat-DO_SUM = 'X'.
      IT_FIELDCAT-REF_TABLE = IT_FIELD_DYNAMIC-REFTAB.
      IT_FIELDCAT-SELTEXT   = IT_FIELD_DYNAMIC-DESC.
      COLLECT IT_FIELDCAT INTO LT_ALV_CAT .
      CLEAR IT_FIELDCAT .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_FIELD_CAT_FOR_DYN_TABLE
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CREATE_DYNAMIC_TABLE .

  IF NOT LT_ALV_CAT[] IS INITIAL .
* Create Dynamic Table -> i_table
    CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
      EXPORTING
        IT_FIELDCATALOG = LT_ALV_CAT
      IMPORTING
        EP_TABLE        = I_TABLE.
    ASSIGN I_TABLE->* TO <TABLE> .
* Create dynamic work area and assign to FS

    CREATE DATA I_STRUCT LIKE LINE OF <TABLE>.
    ASSIGN I_STRUCT->* TO <STRUC>.


  ENDIF.

ENDFORM.                    " F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FIELDCAT_STRUCT_DYN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_FIELDCAT_STRUCT_DYN .

  IT_FIELD_DYNAMIC-FIELDNAME = 'PERNR'.
  IT_FIELD_DYNAMIC-DESC      = 'Employee No'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0008'.
  IT_FIELD_DYNAMIC-REFFIELD = 'PERNR'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'ENAME'.
  IT_FIELD_DYNAMIC-DESC      = 'Employee Name'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0001'.
  IT_FIELD_DYNAMIC-REFFIELD = 'ENAME'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'STEXT'.
  IT_FIELD_DYNAMIC-DESC      = 'Designation'.
  IT_FIELD_DYNAMIC-REFTAB   = 'HRP1000'.
  IT_FIELD_DYNAMIC-REFFIELD = 'STEXT'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'PERSK'.
  IT_FIELD_DYNAMIC-DESC      = 'Grade'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0001'.
  IT_FIELD_DYNAMIC-REFFIELD = 'PERSK'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'ORGEH'.
  IT_FIELD_DYNAMIC-DESC      = 'Org.Unit'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0001'.
  IT_FIELD_DYNAMIC-REFFIELD = 'ORGEH'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'ORGTX'.
  IT_FIELD_DYNAMIC-DESC      = 'Org.Unit Txt'.
  IT_FIELD_DYNAMIC-REFTAB   = 'T527X'.
  IT_FIELD_DYNAMIC-REFFIELD = 'ORGTX'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'WERKS'.
  IT_FIELD_DYNAMIC-DESC      = 'Pers.area'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0001'.
  IT_FIELD_DYNAMIC-REFFIELD = 'WERKS'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'NAME1'.
  IT_FIELD_DYNAMIC-DESC      = 'Pers.area Txt'.
  IT_FIELD_DYNAMIC-REFTAB   = 'T500P'.
  IT_FIELD_DYNAMIC-REFFIELD = 'NAME1'.
  COLLECT IT_FIELD_DYNAMIC.



  IT_FIELD_DYNAMIC-FIELDNAME = 'KOSTL'.
  IT_FIELD_DYNAMIC-DESC      = 'Cost Cntr.CODE'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0001'.
  IT_FIELD_DYNAMIC-REFFIELD = 'KOSTL'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'KTEXT'.
  IT_FIELD_DYNAMIC-DESC      = 'Cost Cntr.'.
  IT_FIELD_DYNAMIC-REFTAB   = 'CSKT'.
  IT_FIELD_DYNAMIC-REFFIELD = 'KTEXT'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'ZZLOC_TEXT'.
  IT_FIELD_DYNAMIC-DESC      = 'Location'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0001'.
  IT_FIELD_DYNAMIC-REFFIELD = 'ZZLOC_TEXT'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'GBDAT'.
  IT_FIELD_DYNAMIC-DESC      = 'DOB'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0002'.
  IT_FIELD_DYNAMIC-REFFIELD = 'GBDAT'.
  COLLECT IT_FIELD_DYNAMIC.

  IT_FIELD_DYNAMIC-FIELDNAME = 'BEGDA'.
  IT_FIELD_DYNAMIC-DESC      = 'DOJ'.
  IT_FIELD_DYNAMIC-REFTAB   = 'PA0000'.
  IT_FIELD_DYNAMIC-REFFIELD = 'BEGDA'.
  COLLECT IT_FIELD_DYNAMIC.

  LOOP AT PRNTAB.

    IT_FIELD_DYNAMIC-FIELDNAME = PRNTAB-LGART.
    IT_FIELD_DYNAMIC-DESC      = PRNTAB-DESC.
    IT_FIELD_DYNAMIC-REFTAB   = 'PA0008'.
    IT_FIELD_DYNAMIC-REFFIELD = 'BET01'.
    COLLECT IT_FIELD_DYNAMIC.

  ENDLOOP.


ENDFORM.                    " F_FILL_FIELDCAT_STRUCT_DYN
*&---------------------------------------------------------------------*
*&      Form  GET_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DISPLAY_DATA .

  DATA : V_TEXT(30),
         ENAME LIKE PA0001-ENAME,
         FL_FLAG.

  DATA : BEGIN OF STRU,
       PERNR LIKE PA0015-PERNR,
       ENAME LIKE PA0001-ENAME,
       STEXT LIKE HRP1000-STEXT," DESIGNATION
       PERSK LIKE P0001-PERSK,"GRADE
       ZZLOC_TEXT  LIKE P0001-ZZLOC_TEXT,"LOCATION
       GBDAT   LIKE P0002-GBDAT,
       BEGDA   LIKE P0000-BEGDA,
       KOSTL LIKE PA0001-KOSTL,
       KTEXT LIKE CSKT-KTEXT,
       ORGEH LIKE PA0001-ORGEH,
       ORGTX LIKE T527X-ORGTX,
       WERKS LIKE PA0001-WERKS,
       NAME1 LIKE T500P-NAME1,
       END OF STRU.
  FIELD-SYMBOLS : <G>,<H>,
                  <G1>,<H1>,
                  <G2>,<H2>,
                  <G3>,<H3>,
                  <G4>,<H4>,
                  <G5>,<H5>,
                  <G6>,<H6>,
                  <G7>,<H7>,
                  <G8>,<H8>,
                  <G9>,<H9>,
                  <G10>,<H10>,
                  <G11>,<H11>,
                  <G12>,<H12>,
                  <G13>,<H13>,
                  <G14>,<H14>,
                  <G15>,<H15>,
                  <G16>,<H16>,
                  <G17>,<H17>,
                  <G18>,<H18>,
                  <G19>,<H19>,
                  <G20>,<H20>,
                  <T1>,<TOT1>,
                  <T2>,<TOT2>,
                  <P>,<PA>,
                  <M>,<MON>,
                  <M1>,
                  <M2>,
                  <M3>,
                  <M4>,
                  <M5>,
                  <M6>,
                  <M7>,
                  <M19>,
                  <M8>,
                  <M20>,
                  <C>,<CTC>.

  SORT FINTAB BY PERNR.

  CLEAR I_P0001.
  LOOP AT I_P0001.

*    ASSIGN COMPONENT 'PERNR'  OF STRUCTURE <STRUC>
*                                              TO <G>.
*    CONCATENATE '<struc>' '-' 'PERNR' INTO V_TEXT.
*    assign (v_text) to <h>.
*    <h> = i_p0001-PERNR.
*    CLEAR V_TEXT.

    CLEAR FINTAB.
    LOOP AT FINTAB WHERE PERNR = I_P0001-PERNR.

      CASE FINTAB-LGART.
        WHEN '1001'.
          ASSIGN COMPONENT '1001'  OF STRUCTURE <STRUC>
                                            TO <G1>.
          CONCATENATE '<struc>' '-' '1001' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H1>.
          <H1> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M001' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M1>.
          <M1> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1004'.
          ASSIGN COMPONENT '1004'  OF STRUCTURE <STRUC>
                                            TO <G2>.
          CONCATENATE '<struc>' '-' '1004' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H2>.
          <H2> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M004' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M2>.
          <M2> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1007'.
          ASSIGN COMPONENT '1007'  OF STRUCTURE <STRUC>
                                            TO <G3>.
          CONCATENATE '<struc>' '-' '1007' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H3>.
          <H3> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M007' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M3>.
          <M3> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1005'.
          ASSIGN COMPONENT '1005'  OF STRUCTURE <STRUC>
                                            TO <G4>.
          CONCATENATE '<struc>' '-' '1005' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H4>.
          <H4> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M005' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M4>.
          <M4> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1006'.
          ASSIGN COMPONENT '1006'  OF STRUCTURE <STRUC>
                                            TO <G5>.
          CONCATENATE '<struc>' '-' '1006' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H5>.
          <H5> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M006' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M5>.
          <M5> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1019'.
          ASSIGN COMPONENT '1019'  OF STRUCTURE <STRUC>
                                            TO <G6>.
          CONCATENATE '<struc>' '-' '1019' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H6>.
*          IF I_P0001-PERNR = '00010116'." Mr. Shekar Shetty
*            <H6> = 0.
*          ELSEIF I_P0001-PERNR = '00001129'." Mr. Debabrata Ray
*            <H6> = 0.
*          ELSEIF I_P0001-PERNR = '00001170'." Mr. Sugur Karnam Thirumala Rao
*            <H6> = 4500.
*          ELSEIF I_P0001-PERNR = '00001271'."   Mr. Chiradeep Mukherjee
*            <H6> = 0.
*          ELSEIF I_P0001-PERNR = '00001283'."   Mr. Parveen Singh Chambial
*          ELSEIF I_P0001-PERNR = '00001833'."   Mr. Sanjeev Jha
*          ELSEIF I_P0001-PERNR = '00001840'."   Mrs. Shweta Tirpude
*          ELSEIF I_P0001-PERNR = '00007078'."   Mr. Vishal Niranjanbhai Parekh
*          ELSEIF I_P0001-PERNR = '00010182'."   Mr. Manish Anand
*          ELSEIF I_P0001-PERNR = '00010200'."   Mr. Rabindra Kumar Singh
*          ELSEIF I_P0001-PERNR = '00010233'."   Mr. Ashok Satya Bhushan Kumar
*
*
*          ELSE.
          <H6> = FINTAB-CHOSEN.
*          ENDIF.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M019' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M5>.
*          IF I_P0001-PERNR = '00010116'. "Mr. Shekar Shetty
*            <M5> = 0.
*          ELSE.
          <M5> = FINTAB-CHOSEN_MON.
*          ENDIF.

          CLEAR V_TEXT.

        WHEN '1116'.

          ASSIGN COMPONENT '1116'  OF STRUCTURE <STRUC>
                                            TO <G20>.
          CONCATENATE '<struc>' '-' '1116' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H20>.

          <H20> = FINTAB-CHOSEN.

          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M116' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M20>.
          <M20> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN 'C3G1'.
          ASSIGN COMPONENT 'C3G1'  OF STRUCTURE <STRUC>     "'C3G2'
                                            TO <G7>.
          CONCATENATE '<struc>' '-' 'C3G1' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H7>.


          <H7> = FINTAB-CHOSEN.

          CLEAR V_TEXT.

        WHEN '1102'." Medical
          ASSIGN COMPONENT '1102'  OF STRUCTURE <STRUC>
                                            TO <G8>.
          CONCATENATE '<struc>' '-' '1102' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H8>.
*          <h8> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          IF I_P0001-PERSK = '1A' OR I_P0001-PERSK = '1B' OR I_P0001-PERSK = '1C'
           OR I_P0001-PERSK = '2A' OR I_P0001-PERSK = '2B'
           OR I_P0001-PERSK = '3A' OR I_P0001-PERSK = '3B' OR I_P0001-PERSK = '3C'
           OR I_P0001-PERSK = '4A' OR I_P0001-PERSK = '4B'
           OR I_P0001-PERSK = '5A' OR I_P0001-PERSK = '5B' OR I_P0001-PERSK = '5C'.

            IF I_P0001-PERNR =  '00010116'." Mr. Shekar Shetty
              <H8>  = 0.
            ELSEIF I_P0001-PERNR =  '00010068' OR I_P0001-PERNR =  '00010071' ." Bajrang Lal Saini – (10068),   P.V. Mahajan (10071
              <H8>  = 4100.
            ELSE.
              <H8>  = 15000.
            ENDIF.
          ELSE.

            <H8> = FINTAB-CHOSEN.
          ENDIF.



*        WHEN 'LE12'.
*          ASSIGN COMPONENT 'LE12'  OF STRUCTURE <STRUC>
*                                            TO <G9>.
*          CONCATENATE '<struc>' '-' 'LE12' INTO V_TEXT.
*          assign (v_text) to <h9>.
*          <h9> = FINTAB-CHOSEN.
*          CLEAR V_TEXT.

        WHEN '1015'.
          ASSIGN COMPONENT '1015'  OF STRUCTURE <STRUC>
                                            TO <G9>.
          CONCATENATE '<struc>' '-' '1015' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H9>.
          <H9> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M015' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M6>.
          <M6> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1016'.
          ASSIGN COMPONENT '1016'  OF STRUCTURE <STRUC>
                                            TO <G>.
          CONCATENATE '<struc>' '-' '1016' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H>.
          <H> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M016' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M7>.
          <M7> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1101'. " LTA
          ASSIGN COMPONENT '1101'  OF STRUCTURE <STRUC>
                                            TO <G10>.
          CONCATENATE '<struc>' '-' '1101' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H10>.
*          <h10> = FINTAB-CHOSEN.
          CLEAR V_TEXT.
*          I_P0001-PERSK  " Code by Punam
*          NEW GRADE 1A 1B 1C 2A 2B 3A 3B 3C 4A 4B 5A 5B 5C

* This Coding is done as per requirement of HR dept to fix LTA acco to Grades
* also for some Employees  , HR want to fix LTA .00000138

          IF I_P0001-PERSK = '1A' OR I_P0001-PERSK = '1B' OR I_P0001-PERSK = '1C'.
            IF I_P0001-PERNR = '00004005' "Dr. Harshvardhan Singh Chauhan
            OR I_P0001-PERNR = '00004025' "Mr. Rabi Mishra
            OR I_P0001-PERNR = '00004031' "Mr. Sunil Hemchand Patel
            OR I_P0001-PERNR = '00004029'."Dr. Girish Khandekar
              <H10> = 54000.

            ELSEIF I_P0001-PERNR = '00004038'. "Dr. Avinash V. Deolekar
              <H10> = 55000.

            ELSEIF I_P0001-PERNR = '00004034' "Dr. Girish Shriram Nagarkar
                OR I_P0001-PERNR = '00004036' "Mr. Alok Kumar
                OR I_P0001-PERNR = '00004012'."Dr. Anil Ganpat Powar
              <H10> = 48000.

            ELSEIF I_P0001-PERNR = '00010116'." Mr. Shekar Shetty
              <H10> = 100000.

            ELSE.
              <H10> = 35000.
            ENDIF.

          ELSEIF I_P0001-PERSK = '2A' OR I_P0001-PERSK = '2B'.
            IF  I_P0001-PERNR = '00004030'. "Mr. Pradeep Venkatesh Kamat
              <H10> = 48000.
            ELSE.
              <H10> = 35000.
            ENDIF.
          ELSEIF I_P0001-PERSK = '3A' OR I_P0001-PERSK = '3B' OR I_P0001-PERSK = '3C'.
            IF  I_P0001-PERNR = '00001805'. "Mr. Sandeep Tulshyan
              <H10> = 35000.
            ELSE.
              <H10> = 28000.
            ENDIF.
*            <H10> = 28000.
          ELSEIF I_P0001-PERSK = '4A' OR I_P0001-PERSK = '4B' .
            <H10> = 28000.
          ELSEIF I_P0001-PERSK = '5A' OR I_P0001-PERSK = '5B' OR I_P0001-PERSK = '5C'.
            IF  I_P0001-PERNR = '00010068' OR I_P0001-PERNR = '00010071' . "Bajrang Lal Saini – (10068),   P.V. Mahajan (10071
              <H10> = 5000.
            ELSE.
              <H10> = 20000.
            ENDIF.
          ELSEIF I_P0001-PERSK = 'MT'.
            <H10> = 28000.
          ELSEIF I_P0001-PERSK = 'M0'.
            <H10> = 40000.
          ELSE.
            <H10> = FINTAB-CHOSEN.
          ENDIF.


        WHEN '1008'.
          ASSIGN COMPONENT '1008'  OF STRUCTURE <STRUC>
                                            TO <G11>.
          CONCATENATE '<struc>' '-' '1008' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H11>.
          <H11> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M008' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M8>.
          <M8> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.


        WHEN '1020'.
          ASSIGN COMPONENT '1020'  OF STRUCTURE <STRUC>
                                            TO <G12>.
          CONCATENATE '<struc>' '-' '1020' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H12>.
          IF I_P0001-PERNR = '00010116'." Mr. Shekar Shetty
            <H12> = 0.
          ELSE.
            <H12> = FINTAB-CHOSEN .
          ENDIF.

          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M020' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M5>.
          IF I_P0001-PERNR = '00010116'." Mr. Shekar Shetty
            <M5> = 0.
*          ELSEIF I_P0001-PERNR = '00001805'." Mr. Sandeep Tulshyan
*            <M5> = 9600.
          ELSE.
            <M5> = FINTAB-CHOSEN_MON.
          ENDIF.
          CLEAR V_TEXT.

        WHEN '1018'.
          ASSIGN COMPONENT '1018'  OF STRUCTURE <STRUC>
                                            TO <G13>.
          CONCATENATE '<struc>' '-' '1018' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H13>.

          IF I_P0001-PERNR = '00010116'." Mr. Shekar Shetty
            <H13> = 0.
*          ELSEIF I_P0001-PERNR = '00001805'." Mr. Sandeep Tulshyan
*            <H13> = 25000.
          ELSE.
            <H13> = FINTAB-CHOSEN.
          ENDIF.
          CLEAR V_TEXT.

        WHEN 'C3G2'.
          ASSIGN COMPONENT 'C3G2'  OF STRUCTURE <STRUC>
                                            TO <G14>.
          CONCATENATE '<struc>' '-' 'C3G2' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H14>.

          IF I_P0001-PERNR = '00010116'. "Mr. Shekar Shetty
            <H14> = 0.
          ELSE.
            <H14> = FINTAB-CHOSEN.
          ENDIF.

          CLEAR V_TEXT.

        WHEN 'C3G3'.
          ASSIGN COMPONENT 'C3G3'  OF STRUCTURE <STRUC>
                                            TO <G15>.
          CONCATENATE '<struc>' '-' 'C3G3' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H15>.
          IF I_P0001-PERNR = '00010116'." Mr. Shekar Shetty
            <H15> = 0.
          ELSE.
            <H15> = FINTAB-CHOSEN.
          ENDIF.
          CLEAR V_TEXT.

        WHEN 'C3G4'.
          ASSIGN COMPONENT 'C3G4'  OF STRUCTURE <STRUC>
                                            TO <G16>.
          CONCATENATE '<struc>' '-' 'C3G4' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H16>.
          <H16> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN '1106'.
          ASSIGN COMPONENT '1106'  OF STRUCTURE <STRUC>
                                            TO <G17>.
          CONCATENATE '<struc>' '-' '1106' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H17>.
          <H17> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN '1013'.
          ASSIGN COMPONENT '1013'  OF STRUCTURE <STRUC>
                                            TO <G19>.

          CONCATENATE '<struc>' '-' '1013' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <H19>.
          <H19> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M013' INTO V_TEXT.
          ASSIGN (V_TEXT) TO <M19>.
          <M19> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

*          CONCATENATE '<struc>' '-' '1007' INTO V_TEXT.
*          assign (v_text) to <h3>.
*          <h3> = FINTAB-CHOSEN.
*          CLEAR V_TEXT.
*
*          CONCATENATE '<struc>' '-' 'M007' INTO V_TEXT.
*          assign (v_text) to <M3>.
*          <M3> = FINTAB-CHOSEN_MON.
*          CLEAR V_TEXT.

      ENDCASE.

*----TOTAL
      ASSIGN COMPONENT 'TOT1'  OF STRUCTURE <STRUC>    TO <T1>.
      CONCATENATE '<struc>' '-' 'TOT1' INTO V_TEXT.
      ASSIGN (V_TEXT) TO <TOT1>.
*      <TOT1> = <H> + <H1> + <H2> + <H3> + <H4> + <H5>+ <H6> + <H11> + <H12> +
      <TOT1> =  <H7> + <H8> + <H9> + <H10> +  <H13> .
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'TOT2'  OF STRUCTURE <STRUC>    TO <T2>.
      CONCATENATE '<struc>' '-' 'TOT2' INTO V_TEXT.
      ASSIGN (V_TEXT) TO <TOT2>.
      <TOT2> = <H14> + <H15> + <H16> .
      CLEAR V_TEXT.


      ASSIGN COMPONENT 'ANNM'  OF STRUCTURE <STRUC>    TO <P>.
      CONCATENATE '<struc>' '-' 'ANNM' INTO V_TEXT.
      ASSIGN (V_TEXT) TO <PA>.
      <PA> = <H1> + <H2> + <H3> + <H4> + <H5>  + <H20> + <H11> + <H12> + <H19> .
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'MON1'  OF STRUCTURE <STRUC>    TO <M>.
      CONCATENATE '<struc>' '-' 'MON1' INTO V_TEXT.
      ASSIGN (V_TEXT) TO <MON>.
      <MON> =  <PA> / 12.
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'CTC1'  OF STRUCTURE <STRUC>    TO <C>.
      CONCATENATE '<struc>' '-' 'CTC1' INTO V_TEXT.
      ASSIGN (V_TEXT) TO <CTC>.
      <CTC> = <PA> + <TOT1> + <TOT2>.
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'TCTC'  OF STRUCTURE <STRUC>    TO <G18>.
      CONCATENATE '<struc>' '-' 'TCTC' INTO V_TEXT.
      ASSIGN (V_TEXT) TO <H18>.
      <H18> = <CTC> + <H17>.
      CLEAR V_TEXT.




*----

    ENDLOOP.

    AT END OF PERNR.
      FL_FLAG = 1.
    ENDAT.
    IF FL_FLAG = 1.
      CLEAR :ENAME,FL_FLAG,STRU,I_P0002,EMP_DOJ.
      STRU-PERNR =   I_P0001-PERNR.
      STRU-ENAME =   I_P0001-ENAME.
      STRU-PERSK =   I_P0001-PERSK .
      STRU-ZZLOC_TEXT =   I_P0001-ZZLOC_TEXT .
      STRU-KOSTL =  I_P0001-KOSTL.
      STRU-ORGEH = I_P0001-ORGEH.

      SELECT SINGLE ORGTX FROM T527X
        INTO STRU-ORGTX WHERE SPRSL = SY-LANGU
        AND ORGEH = I_P0001-ORGEH.

*      STRU-ORGTX = T527X-ORGTX.
      STRU-WERKS = I_P0001-WERKS.

      SELECT SINGLE NAME1 FROM T500P
        INTO STRU-NAME1 WHERE PERSA = I_P0001-WERKS.


*      STRU-NAME1 = i_p0001-NAME1.
* SELECT SINGLE STEXT INTO STRU-STEXT FROM HRP1000
*        WHERE OBJID = i_P0001-plans AND LANGU = 'EN'.

*Change by Punam
      SELECT SINGLE KTEXT
        INTO STRU-KTEXT
        FROM CSKT
        WHERE KOSTL = STRU-KOSTL
        AND SPRAS = 'EN'
        AND KOKRS = I_P0001-BUKRS.
* POONAM: CHANGES MADE IN BELOW QUERY FOR CORRUNT DESIGNATION
*SELECTION PREVIOUSLY IT WAS MAX(BEGDA) INSTED OF MAX(AEDTM)*

      SELECT SINGLE STEXT INTO STRU-STEXT FROM HRP1000
        WHERE OBJID = I_P0001-PLANS AND LANGU = 'EN'
        AND OTYPE = 'S'
        AND BEGDA IN
        ( SELECT MAX( BEGDA ) FROM HRP1000 WHERE OBJID = I_P0001-PLANS AND LANGU = 'EN'  AND OTYPE = 'S' ).

*      SELECT SINGLE STEXT INTO STRU-STEXT FROM HRP1000
*        WHERE OBJID = i_P0001-plans AND LANGU = 'EN'
*        AND BEGDA = i_P0001-BEGDA.
**        AND aedtm in ( SELECT MAX( aedtm )
**        FROM HRP1000 WHERE OBJID = i_P0001-plans
**        AND LANGU = 'EN' ).
********************
      READ TABLE I_P0002 WITH KEY PERNR = I_P0001-PERNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        STRU-GBDAT = I_P0002-GBDAT.
      ENDIF.

      READ TABLE EMP_DOJ WITH KEY PERNR = I_P0001-PERNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        STRU-BEGDA = EMP_DOJ-DOJ.
      ENDIF.

      MOVE-CORRESPONDING STRU TO <STRUC>.
    ENDIF.
    APPEND <STRUC> TO <TABLE>.
    CLEAR <STRUC>.
  ENDLOOP.
ENDFORM.                    " GET_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_SUPERANN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SUPERANN_DATA .

  CLEAR I_P0000.
  SORT I_P0000 BY PERNR BEGDA ASCENDING.

  CLEAR :  I_P00001,I_P00001[], I_P0000.
*  loop at i_p0000 where pernr = i_p0001-pernr.
*    move-corresponding i_p0000 to  i_p00001.
*    append i_p00001.
*  endloop.

  SELECT * FROM PA0000 INTO TABLE I_P00001
    WHERE PERNR = I_P0001-PERNR.

*  ENDLOOP.
  SORT I_P00001 BY BEGDA ASCENDING.

  CLEAR WA_START_DT.
  READ TABLE I_P00001 INDEX 1.
  IF SY-SUBRC = 0.
    WA_START_DT =  I_P00001-BEGDA.
  ENDIF.


  IF NOT WA_START_DT IS INITIAL AND WA_START_DT LE '20100401'.
    CALL FUNCTION 'HR_COMPUTE_YEARS_BETWEEN_DATES'
      EXPORTING
        FIRST_DATE                        = WA_START_DT
*       MODIFY_INTERVAL                   = ' '
        SECOND_DATE                       = SY-DATUM
     IMPORTING
       YEARS_BETWEEN_DATES               = NO_YEAR
     EXCEPTIONS
       SEQUENCE_OF_DATES_NOT_VALID       = 1
       OTHERS                            = 2
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*    if not no_year is initial.
    CLEAR :FINTAB,WA_SANN.
    READ TABLE FINTAB WITH KEY PERNR = I_P0001-PERNR
                            LGART = '1001'.
    IF SY-SUBRC = 0.
      SELECT SINGLE * FROM PA0185 WHERE
        PERNR = I_P0001-PERNR AND SUBTY = '01'
        AND ENDDA GT '20100331'.
      IF SY-SUBRC = 0.
        IF NO_YEAR LE '5'.
          WA_SANN   = FINTAB-CHOSEN * '0.10'.
        ELSEIF NO_YEAR GT '5' AND NO_YEAR LE '10'.
          WA_SANN   = FINTAB-CHOSEN * '0.125'.
        ELSEIF NO_YEAR GT '10'.
          WA_SANN   = FINTAB-CHOSEN * '0.15'.
        ENDIF.
        IF I_P0001-PERSG <> 'T'.
          CLEAR FINTAB.
          MOVE I_P0001-PERNR TO FINTAB-PERNR.
          MOVE 'C3G4'        TO FINTAB-LGART.
          FINTAB-CHOSEN = WA_SANN .
*        fintab-doj = wa_start_dt.
          INSERT  FINTAB INDEX SY-TABIX.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  EMP_DOJ-PERNR = I_P0001-PERNR.
  EMP_DOJ-DOJ = WA_START_DT.
  APPEND EMP_DOJ.
  CLEAR EMP_DOJ.
ENDFORM.                    " GET_SUPERANN_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_T510_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_T510_DATA .
  CLEAR I_P0008.
  READ TABLE I_P0008 WITH KEY  PERNR = I_P0001-PERNR.
  IF SY-SUBRC = 0.
    CLEAR : IT_T510,T510.
    SELECT SINGLE * FROM Z6HR_EMP_GRADE WHERE PERNR = I_P0001-PERNR.
    IF SY-SUBRC NE 0.
      SELECT * FROM T510   INTO CORRESPONDING FIELDS OF TABLE IT_T510
      WHERE  TRFAR =  I_P0008-TRFAR
       AND   TRFGB =  I_P0008-TRFGB
       AND   TRFGR =  I_P0008-TRFGR
       AND   TRFST =  I_P0008-TRFST
       AND   LGART IN ('1018','1019','1020').
      IF SY-SUBRC = 0.
        IF I_P0008-PERNR = '00001805'.
          READ TABLE IT_T510 INTO T510 WITH KEY LGART = '1018'.
          IF SY-SUBRC <> 0.
            WA_T510-MANDT = SY-MANDT.
            WA_T510-MOLGA = 40.
            WA_T510-TRFAR = I_P0008-TRFAR.
            WA_T510-TRFGB = I_P0008-TRFGB.
            WA_T510-TRFKZ = T510-TRFKZ.
            WA_T510-TRFGR = I_P0008-TRFGR.
            WA_T510-TRFST = I_P0008-TRFST.
            WA_T510-LGART = '1018'.
            WA_T510-ENDDA = I_P0008-ENDDA.
            WA_T510-BEGDA = I_P0008-BEGDA.
            WA_T510-BETRG = 25000.
            APPEND WA_T510 TO IT_T510.
            CLEAR WA_T510.
            WA_T510-MANDT = SY-MANDT.
            WA_T510-MOLGA = 40.
            WA_T510-TRFAR = I_P0008-TRFAR.
            WA_T510-TRFGB = I_P0008-TRFGB.
            WA_T510-TRFKZ = T510-TRFKZ.
            WA_T510-TRFGR = I_P0008-TRFGR.
            WA_T510-TRFST = I_P0008-TRFST.
            WA_T510-LGART = '1020'.
            WA_T510-ENDDA = I_P0008-ENDDA.
            WA_T510-BEGDA = I_P0008-BEGDA.
            WA_T510-BETRG = 800.
            APPEND WA_T510 TO IT_T510.
            CLEAR WA_T510.
          ENDIF.
        ENDIF.
      ENDIF.


    ELSE.

      SELECT * FROM T510   INTO CORRESPONDING FIELDS OF TABLE IT_T510
      WHERE  TRFAR =  Z6HR_EMP_GRADE-TRFAR
       AND   TRFGB =  Z6HR_EMP_GRADE-TRFGB
       AND   TRFGR =  Z6HR_EMP_GRADE-TRFGR
       AND   TRFST =  Z6HR_EMP_GRADE-TRFST
       AND   LGART IN ('1018','1019','1020').
    ENDIF.

    IF NOT IT_T510 IS INITIAL.
      CLEAR T510.
      READ TABLE IT_T510 INTO T510 WITH KEY LGART = '1018'.
      IF SY-SUBRC = 0.
        CLEAR FINTAB.
        MOVE I_P0001-PERNR TO FINTAB-PERNR.
        FINTAB-LGART  = T510-LGART.
        FINTAB-CHOSEN = T510-BETRG .
        INSERT  FINTAB INDEX SY-TABIX.
      ENDIF.

      CLEAR T510.
      READ TABLE IT_T510 INTO T510 WITH KEY LGART = '1019'.
      IF SY-SUBRC = 0.
        CLEAR FINTAB.
        MOVE I_P0001-PERNR TO FINTAB-PERNR.
        FINTAB-LGART  = T510-LGART.
        FINTAB-CHOSEN_MON = T510-BETRG .
        FINTAB-CHOSEN = T510-BETRG * 12.
        INSERT  FINTAB INDEX SY-TABIX.
      ENDIF.

      CLEAR T510.
      READ TABLE IT_T510 INTO T510 WITH KEY LGART = '1020'.
      IF SY-SUBRC = 0.
        CLEAR FINTAB.
        MOVE I_P0001-PERNR TO FINTAB-PERNR.
        FINTAB-LGART  = T510-LGART.
        FINTAB-CHOSEN_MON = T510-BETRG .
        FINTAB-CHOSEN = T510-BETRG * 12.
        INSERT  FINTAB INDEX SY-TABIX.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " GET_T510_DATA

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE .
  DATA : V_TCOUNT TYPE I.
  .
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LIST .

  PERFORM  EVENTTAB_BUILD USING GT_EVENTS .
  PERFORM  BUILD_LAYOUT USING LAYOUT .
  PERFORM BUILD_COMMENT USING GT_LIST_TOP_OF_PAGE[].

ENDFORM.                    " DISPLAY_LIST
*---------------------------------------------------------------------*
*       FORM eventtab_build                                           *
*---------------------------------------------------------------------*
FORM EVENTTAB_BUILD USING LT_EVENTS TYPE SLIS_T_EVENT.
  CONSTANTS:
  GC_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
*  gc_formname_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = LT_EVENTS.

  READ TABLE LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE GC_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO LT_EVENTS.
  ENDIF.

ENDFORM.                    "eventtab_build

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT USING P_LAYOUT TYPE SLIS_LAYOUT_ALV.
  P_LAYOUT-F2CODE       = '&IC1'.
  P_LAYOUT-ZEBRA        = 'X'.
*  p_layout-detail_popup = ' '.
ENDFORM.                    "build_layout
*&--------------------------------------------------------------------*
*&      Form  BUILD_comment
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_HEADING  text
*---------------------------------------------------------------------*
FORM BUILD_COMMENT USING P_HEADING TYPE SLIS_T_LISTHEADER.
  DATA: LINE        TYPE  SLIS_LISTHEADER ,
        RS_VARIANT  TYPE  DISVARIANT      ,
        C_DATE(10)                        .

  CLEAR : GT_LIST_TOP_OF_PAGE[], GT_LIST_TOP_OF_PAGE .

  LINE-INFO = 'INDOFIL CHEMICALS COMPANY' .
  LINE-TYP = 'S' .
  APPEND LINE TO GT_LIST_TOP_OF_PAGE .

  LINE-INFO = 'CTC REPORT ' .
  LINE-TYP = 'S' .
  APPEND LINE TO GT_LIST_TOP_OF_PAGE .

  IMPORT RS_VARIANT FROM MEMORY ID 'VARIANT'.
  IF NOT RS_VARIANT IS INITIAL .
    CONCATENATE 'Layout : ' RS_VARIANT-TEXT INTO LINE-INFO
                                             SEPARATED BY SPACE .
    LINE-TYP  = 'S' .
    APPEND LINE TO GT_LIST_TOP_OF_PAGE .
    CLEAR LINE .
  ENDIF .

  LINE-TYP = 'S' .
  WRITE SY-DATUM TO C_DATE .
  CONCATENATE 'Date : ' C_DATE INTO LINE-INFO SEPARATED BY SPACE .
  APPEND LINE TO GT_LIST_TOP_OF_PAGE .
  CLEAR LINE .

ENDFORM .                    "build_comment
