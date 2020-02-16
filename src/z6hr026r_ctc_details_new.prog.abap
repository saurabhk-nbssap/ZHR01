*&---------------------------------------------------------------------*
*& Report  Z6HR026R_CTC_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT   Z6HR026R_CTC_DETAILS
         MESSAGE-ID zh
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
type-pools : slis.

INCLUDE pc2rxin0.
INCLUDE rpc2rx09.

INFOTYPES : 0000,0001 , 0002, 0008, 0014, 0015, 0589, 0581. ",0185.

TABLES : pernr,
         t5w7a,
         t503t,
         t510,
         t512t,
         PA0185,
         Z6HR_EMP_GRADE.

FIELD-SYMBOLS: <hsl> , <wtf>.

DATA : w_hsl(14) , w_ctr(2) TYPE n, chosen TYPE i, ztype, ztypedesc(25),
      desc(40), w_wtf(14), mperiod(6), lgart LIKE pa0008-lga01,
      ename LIKE p0002-cname.
DATA : w_awdyear1(4),
       w_awdyear2(4).


data : curr_yr(4) TYPE n,
      w_begda LIKE sy-datum,
      w_endda LIKE sy-datum.

DATA : BEGIN OF zt5w7a OCCURS 0.
        INCLUDE STRUCTURE t5w7a.
DATA : END OF zt5w7a.


DATA : BEGIN OF i_t512t OCCURS 0,
       lgart LIKE t512t-lgart,
       lgtxt LIKE t512t-lgtxt,
       END OF i_t512t.

DATA i_p0001 TYPE p0001 OCCURS 0 WITH HEADER LINE.
DATA i_p0002 TYPE p0002 OCCURS 0 WITH HEADER LINE.
DATA i_p0008 TYPE p0008 OCCURS 0 WITH HEADER LINE.
DATA i_p0014 TYPE p0014 OCCURS 0 WITH HEADER LINE.
DATA i_p0015 TYPE p0015 OCCURS 0 WITH HEADER LINE.
DATA i_p0015_1 TYPE p0015 OCCURS 0 WITH HEADER LINE.
DATA i_p0015_2 TYPE p0015 OCCURS 0 WITH HEADER LINE.
DATA i_p0589 TYPE p0589 OCCURS 0 WITH HEADER LINE.
DATA i_p0581 TYPE p0581 OCCURS 0 WITH HEADER LINE.
DATA i_p0000 TYPE p0000 OCCURS 0 WITH HEADER LINE.
DATA i_p00001 TYPE PA0000 OCCURS 0 WITH HEADER LINE.
DATA i_T549Q TYPE T549Q OCCURS 0 WITH HEADER LINE.


DATA : srchlgart LIKE pa0008-lga01,
       i_p0015_BEGDA TYPE SYDATUM,
       i_p0015_ENDDA TYPE SYDATUM.

DATA: BEGIN OF fintab OCCURS 0,
      pernr        LIKE pa0008-pernr,
      lgart        LIKE pa0008-lga01,
      chosen       LIKE pa0008-bet01,
      chosen_mon   like pa0008-bet01,
     END OF fintab.

DATA: BEGIN OF prntab OCCURS 0,
      ztype(2),
      ztypdesc(25),
      pernr    LIKE pa0008-pernr,
      it(2),
      lgart    LIKE pa0008-lga01,
      desc(40),
      stext    LIKE t5w7at-stext,
      chosen   LIKE pa0008-bet01,
      END OF prntab.

DATA: BEGIN OF zprntab OCCURS 0,
      ztype(1),
      ztypdesc(25),
      pernr    LIKE pa0008-pernr,
      lgart    LIKE pa0008-lga01,
      desc(40),
      chosen(15),
     END OF zprntab.

data : begin of emp_doj  OCCURS 0,
       pernr type p0001-pernr,
       doj   type P0000-begda,
       end of emp_doj.

data :zmbetrg LIKE pa0008-bet01,
       zzmbetrg LIKE pa0008-bet01,
       zzmbetrg_mon LIKE pa0008-bet01,
       zmmonths(2) TYPE n.

data : WA_BONUS   LIKE zprntab-chosen,
       wa_pf      like zprntab-chosen,
       wa_grt     LIKE zprntab-chosen,
       wa_sann    LIKE zprntab-chosen.

DATA : IT_T510 TYPE standard TABLE OF T510.

DATA wuname LIKE sy-uname.
DATA wpernr LIKE pa0001-pernr.
DATA wbukrs LIKE pa0001-bukrs.
DATA wabkrs LIKE pa0001-abkrs.
data wa_start_dt like PA0185-begda.
data no_year type I.

FIELD-SYMBOLS : <table>    TYPE  table ,     " Main Internal Table
                <struc> ,
                <table1>   TYPE  table,
                <struc1>,
                <table2>   TYPE  table,
                <struc2>,

                <table3>   TYPE  table,
                <struc3>,
                <wstruc>,                    " Header Struct for <table>
                <field> ,
                <component> .

DATA      :   alv_fieldcat TYPE              slis_t_fieldcat_alv ,
              lt_alv_cat   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat1   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat2   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat3   TYPE  TABLE OF    lvc_s_fcat ,
              it_fieldcat  LIKE  LINE  OF    lt_alv_cat .

DATA      :   i_table      TYPE  REF   TO    data ,
              i_struct     TYPE  REF   TO    data ,
              tabix        TYPE sy-tabix.

DATA : BEGIN OF it_field_dynamic OCCURS 0,
       fieldname LIKE dd03l-fieldname,
       reftab  LIKE dd03l-tabname,
       reffield  LIKE dd03l-reffield,
       desc(35),
      END OF it_field_dynamic.


DATA : g_save(1)            TYPE  c                      ,
       g_exit(1)            TYPE  c                      ,
       g_variant            LIKE  disvariant             ,
       gx_variant           LIKE  disvariant             ,
       f2code               LIKE  sy-ucomm VALUE  '&ETA' ,
       layout               TYPE  slis_layout_alv        ,
       prg                  LIKE  sy-repid               ,
       keyinfo  TYPE slis_keyinfo_alv,
       gt_list_top_of_page  TYPE  slis_t_listheader      ,
       gt_events            TYPE  slis_t_event           ,
       fieldcat             TYPE  slis_t_fieldcat_alv    .

*      --------------------------------------------------------*
*      INITIALIZATION.
*      --------------------------------------------------------*

INITIALIZATION.
  IF sy-tcode = 'ZHR_CTC'. "'ZCTC'.
    wuname = sy-uname.
    CLEAR: wpernr, wabkrs, wbukrs.
    CALL FUNCTION 'HR_GETEMPLOYEEDATA_FROMUSER'
      EXPORTING
        username                  = wuname
        validbegin                = sy-datum
      IMPORTING
        employeenumber            = wpernr
        payrollarea               = wabkrs
        companycode               = wbukrs
      EXCEPTIONS
        user_not_found            = 1
        countrygrouping_not_found = 2
        infty_not_found           = 3
        OTHERS                    = 4.
    IF sy-subrc = 0.
      pnppernr-low = wpernr. pnppernr-option = 'EQ'. pnppernr-sign = 'I'.
      APPEND pnppernr.
      pnpbukrs-low = wbukrs. pnpbukrs-option = 'EQ'. pnpbukrs-sign = 'I'.
      APPEND pnpbukrs.
      pnpabkrs-low = wabkrs. pnpabkrs-option = 'EQ'. pnpabkrs-sign = 'I'.
      APPEND pnpabkrs.
    ENDIF.

  ENDIF.

START-OF-SELECTION.

GET pernr.
  PERFORM display_list .
  PERFORM getdata.

END-OF-SELECTION.

  PERFORM cummdata.
  PERFORM f_fill_fieldcat_struct_dyn.
  PERFORM F_field_cat_for_dyn_table.
  PERFORM F_CREATE_DYNAMIC_TABLE.
  perform get_display_data.
  PERFORM F_PRINT_dATA.
*&---------------------------------------------------------------------*
*&      Form  GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM getdata.
  rp_provide_from_last p0001 space sy-datum sy-datum.
  IF pnp-sw-found = 1.
    MOVE-CORRESPONDING p0001 TO i_p0001.
    APPEND i_p0001.
  ENDIF.

  rp_provide_from_last p0002 space sy-datum sy-datum.
  IF pnp-sw-found = 1.
    MOVE-CORRESPONDING p0002 TO i_p0002.
    APPEND i_p0002.
  ENDIF.

  rp_provide_from_last p0008 space sy-datum sy-datum.
  IF pnp-sw-found = 1.
    MOVE-CORRESPONDING p0008 TO i_p0008.
    APPEND i_p0008.
  ENDIF.

  LOOP AT p0014 WHERE begda <= sy-datum AND endda >= sy-datum.
    MOVE-CORRESPONDING p0014 TO i_p0014.
    APPEND i_p0014.
  ENDLOOP.
  IF sy-datum+4(2) > 3.
    curr_yr = sy-datum+0(4).
    CONCATENATE curr_yr '0401' INTO w_begda.
    curr_yr = curr_yr + 1.
    CONCATENATE curr_yr '0331' INTO w_endda.
  ELSE.
    curr_yr = sy-datum+0(4).
    curr_yr = curr_yr - 1.
    CONCATENATE curr_yr '0401' INTO w_begda.
    curr_yr = sy-datum+0(4).
    CONCATENATE curr_yr '0331' INTO w_endda.
  ENDIF.
  w_awdyear1 = w_begda+0(4).
  w_awdyear1 = w_awdyear1 - 1.
  w_awdyear2 = w_awdyear1 + 1.

  LOOP AT p0015   .
    MOVE-CORRESPONDING p0015 TO i_p0015.
    APPEND i_p0015.
    CLEAR P0015.
  ENDLOOP.

  rp_provide_from_last p0589 space sy-datum sy-datum.
  IF pnp-sw-found = 1.
    MOVE-CORRESPONDING p0589 TO i_p0589.
    APPEND i_p0589.
  ENDIF.

  rp_provide_from_last p0581 space sy-datum sy-datum.
  IF pnp-sw-found = 1.
    MOVE-CORRESPONDING p0581 TO i_p0581.
    APPEND i_p0581.
  ENDIF.

  rp_provide_from_last p0000 space sy-datum sy-datum.
  IF pnp-sw-found = 1.
    MOVE-CORRESPONDING p0000 TO i_p0000.
    APPEND i_p0000.
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
FORM cummdata.

  SELECT * FROM t5w7a INTO TABLE zt5w7a.

  if pn-permo ne space and pn-pabrj ne space.

  SELECT * FROM T549Q INTO TABLE I_T549Q WHERE PERMO = PN-PERMO AND PABRJ = PN-PABRJ.
  IF SY-SUBRC = 0.
    READ TABLE I_T549Q WITH KEY PABRP = '01'.
    IF SY-SUBRC = 0.
    i_p0015_BEGDA = I_T549Q-BEGDA.
    ENDIF.
    READ TABLE I_T549Q WITH KEY PABRP = '12'.
    IF SY-SUBRC = 0.
    i_p0015_ENDDA = I_T549Q-ENDDA.
    ENDIF.
  ENDIF.
  else.
   select SINGLE * from t549Q into I_T549Q where PERMO = 01 and begda le pn-begda and endda ge pn-begda.

   SELECT * FROM T549Q INTO TABLE I_T549Q WHERE PERMO = I_T549Q-permo AND PABRJ = I_T549Q-pabrj.
   IF SY-SUBRC = 0.
    clear I_T549Q.
    READ TABLE I_T549Q WITH KEY PABRP = '01'.
    IF SY-SUBRC = 0.
    i_p0015_BEGDA = I_T549Q-BEGDA.
    ENDIF.
    READ TABLE I_T549Q WITH KEY PABRP = '12'.
    IF SY-SUBRC = 0.
    i_p0015_ENDDA = I_T549Q-ENDDA.
    ENDIF.
  ENDIF.

  endif.
  LOOP AT i_p0001.

    SELECT lgart lgtxt FROM t512t
    INTO CORRESPONDING FIELDS OF TABLE i_t512t
    WHERE sprsl = 'E'
    AND molga = '40'.


    IF NOT i_t512t[] IS INITIAL.
      SORT i_t512t BY lgart.
    ENDIF.


    PERFORM fintab.
***IT 8
    LOOP AT i_p0008 WHERE pernr = i_p0001-pernr.
      w_ctr = 1.
      WHILE w_ctr <= 20.
        CONCATENATE 'i_P0008-LGA' w_ctr INTO w_hsl.
        CONCATENATE 'i_P0008-BET' w_ctr INTO w_wtf.
        ASSIGN (w_hsl) TO <hsl>.
        ASSIGN (w_wtf) TO <wtf>.
        IF NOT <hsl> IS INITIAL.


****additional patch for including 9FME CODE BEGIN

          READ TABLE prntab WITH KEY it = 8  lgart = <hsl>.
          IF sy-subrc = 0.
            IF NOT  <wtf> = 0.
              CLEAR :  zmbetrg , zzmbetrg.
              MOVE <wtf> TO zmbetrg.
              IF zmbetrg > 0.
                zzmbetrg =  zmbetrg * 12.
                zzmbetrg_mon =  zmbetrg.
              ENDIF.
              MOVE <hsl> TO srchlgart.
              READ TABLE fintab WITH KEY pernr = i_p0001-pernr
                    lgart = srchlgart BINARY SEARCH.
              IF sy-subrc NE 0.
                CLEAR fintab.
                MOVE i_p0001-pernr TO fintab-pernr.
                MOVE <hsl>         TO fintab-lgart.
                MOVE zzmbetrg      TO fintab-chosen.
                MOVE zzmbetrg_mon  TO fintab-chosen_mon.
                INSERT  fintab INDEX sy-tabix.
              ELSE.
                fintab-chosen = fintab-chosen + zzmbetrg.
                fintab-chosen_mon = fintab-chosen_mon + zzmbetrg_mon.
                MODIFY fintab INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        w_ctr = w_ctr + 1.
      ENDWHILE.
    ENDLOOP.
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
    CLEAR : i_p0015.", year.", zbegda, zendda.
    SORT i_p0015 BY pernr begda endda.

    clear: i_p0015_1,i_p0015_1[],i_p0015_2,i_p0015_2[].

    LOOP AT i_p0015 WHERE pernr = i_p0001-pernr.
      move-corresponding i_p0015 to i_p0015_1.
      move-corresponding i_p0015 to i_p0015_2.
      append i_p0015_1. append i_p0015_2.
      clear i_p0015_1. clear i_p0015_2.
    endloop.

    DELETE i_p0015_1 WHERE LGART NE '1106'.
    DELETE i_p0015_1 WHERE BEGDA LT i_p0015_BEGDA or BEGDA GT i_p0015_ENDDA.

*    SORT i_p0015_1 BY begda  DESCENDING .
*    READ TABLE i_p0015_1 INDEX 1.
*    IF SY-SUBRC = 0.

      CLEAR :  zmbetrg , zzmbetrg.
      LOOP AT i_p0015_1.
       zmbetrg = i_p0015_1-betrg + zmbetrg.
      ENDLOOP.

*      MOVE i_p0015_1-betrg TO zmbetrg.
      zzmbetrg =  zmbetrg.
      READ TABLE fintab WITH KEY pernr = i_p0001-pernr
             lgart =  '1106' BINARY SEARCH.
*            lgart =  i_p0015-lgart BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR fintab.
        MOVE i_p0001-pernr TO fintab-pernr.
        MOVE i_p0015_1-lgart TO fintab-lgart.
        MOVE zzmbetrg      TO fintab-chosen.
        INSERT  fintab INDEX sy-tabix.
*      ELSE.
*        fintab-chosen = fintab-chosen + zzmbetrg.
*        MODIFY fintab INDEX sy-tabix.
      ENDIF.
*    ENDIF.

    DELETE i_p0015_2 WHERE LGART NE '1121'.
    DELETE i_p0015_2 WHERE BEGDA LT i_p0015_BEGDA or BEGDA GT i_p0015_ENDDA.


      CLEAR :  zmbetrg , zzmbetrg.
      LOOP AT i_p0015_2.
       zmbetrg = i_p0015_2-betrg + zmbetrg.
      ENDLOOP.

      zzmbetrg =  zmbetrg.

      READ TABLE fintab WITH KEY pernr = i_p0001-pernr
             lgart =  '1121' BINARY SEARCH.
*            lgart =  i_p0015-lgart BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR fintab.
        MOVE i_p0001-pernr TO fintab-pernr.
        MOVE i_p0015_2-lgart TO fintab-lgart.
        MOVE zzmbetrg      TO fintab-chosen.
        INSERT  fintab INDEX sy-tabix.
*      ELSE.
*        fintab-chosen = fintab-chosen + zzmbetrg.
*        MODIFY fintab INDEX sy-tabix.
      ENDIF.


*    ENDLOOP.
*****Infotype 589
    LOOP AT i_p0589 WHERE pernr = i_p0001-pernr.
      MOVE 12 TO zmmonths.
      w_ctr = 1.
      WHILE w_ctr <= 15.
        CONCATENATE 'i_P0589-LGA' w_ctr INTO w_hsl.
        CONCATENATE 'i_P0589-BET' w_ctr INTO w_wtf.
        ASSIGN (w_hsl) TO <hsl>.
        ASSIGN (w_wtf) TO <wtf>.
        IF  <hsl> = '1101'.
          IF NOT  <wtf> = 0.
            CLEAR :  zmbetrg , zzmbetrg.
            MOVE <wtf> TO zmbetrg.
            IF zmbetrg > 0.
              zzmbetrg =  zmbetrg.
            ENDIF.
            MOVE <hsl> TO srchlgart.
            READ TABLE fintab WITH KEY pernr = i_p0001-pernr
                  lgart = srchlgart BINARY SEARCH.
            IF sy-subrc NE 0.
              CLEAR fintab.
              MOVE i_p0001-pernr TO fintab-pernr.
              MOVE <hsl>         TO fintab-lgart.
              MOVE zzmbetrg      TO fintab-chosen.
              INSERT  fintab INDEX sy-tabix.
            ELSE.
              MOVE zzmbetrg      TO fintab-chosen.
              MODIFY fintab INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ENDIF.
******************NEW CODE FOR INTRODUCING IT 589  CODE 3730
        IF  <hsl> = '1102'.
          IF NOT  <wtf> = 0.
            CLEAR :  zmbetrg , zzmbetrg.
            MOVE <wtf> TO zmbetrg.
            IF zmbetrg > 0.
              zzmbetrg =  zmbetrg.
            ENDIF.
*            MOVE '3610' TO srchlgart.
            MOVE '1102' TO srchlgart.
            READ TABLE fintab WITH KEY pernr = i_p0001-pernr
                  lgart =  srchlgart BINARY SEARCH.
            IF sy-subrc NE 0.
              CLEAR fintab.
              MOVE i_p0001-pernr TO fintab-pernr.
              MOVE '1102'        TO fintab-lgart.
              MOVE zzmbetrg      TO fintab-chosen.
              INSERT  fintab INDEX sy-tabix.
            ELSE.
              fintab-chosen = fintab-chosen + zzmbetrg.
*              MOVE zzmbetrg      TO fintab-chosen.
              MODIFY fintab INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ENDIF.
******************NEW CODE FOR INTRODUCING IT 589  CODE 3730
        w_ctr = w_ctr + 1.
      ENDWHILE.
    ENDLOOP.

*  ---for bonus ,pf ,gratuity,superannuation


*CHANGES MADE BY PUNAM to remove bonus ,pf ,gratuity,superannuation for trainee employees.
*    IF I_P0001-PLANS <> '50000693'.

    read table fintab with key lgart = '1001'
                               pernr = i_p0001-pernr.
    if sy-subrc = 0.
      WA_BONUS = fintab-CHOSEN * '0.20'.
      wa_pf    = fintab-CHOSEN * '0.12' .
      wa_grt   = fintab-CHOSEN * '0.0481'.
*              wa_sann
      CLEAR fintab.
      MOVE i_p0001-pernr TO fintab-pernr.
      MOVE 'C3G1'        TO fintab-lgart.
      fintab-chosen = WA_BONUS .
      INSERT  fintab INDEX sy-tabix.

      CLEAR fintab.
      MOVE i_p0001-pernr TO fintab-pernr.
      MOVE 'C3G2'        TO fintab-lgart.
      fintab-chosen = WA_pf .
      INSERT  fintab INDEX sy-tabix.

      IF I_P0001-PERSG <> 'T'.
      CLEAR fintab.
      MOVE i_p0001-pernr TO fintab-pernr.
      MOVE 'C3G3'        TO fintab-lgart.
      fintab-chosen = WA_grt .
      INSERT  fintab INDEX sy-tabix.
      ENDIF.
    endif.

*    ENDIF.
* -----
*    ------super annuation

     perform get_superann_data.

    perform get_t510_data.
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
FORM fintab.
  CLEAR prntab.
  REFRESH prntab.

  MOVE 'A' TO prntab-ztype.
  MOVE 'BASIC : ' TO prntab-ztypdesc.
  MOVE '1001' TO prntab-lgart.
  MOVE 'Basic Pay' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'A1' TO prntab-ztype.
  MOVE 'BASIC M : ' TO prntab-ztypdesc.
  MOVE 'M001' TO prntab-lgart.
  MOVE 'Monthly Basic' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'B' TO prntab-ztype.
  MOVE 'HRA : ' TO prntab-ztypdesc.
  MOVE '1004' TO prntab-lgart.
  MOVE 'House Rent Allowance .' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'B1' TO prntab-ztype.
  MOVE 'HRA M: ' TO prntab-ztypdesc.
  MOVE 'M004' TO prntab-lgart.
  MOVE 'Monthly HRA' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'C' TO prntab-ztype.
  MOVE 'SPA : ' TO prntab-ztypdesc.
  MOVE '1007' TO prntab-lgart.
  MOVE 'Special Allowance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'C1' TO prntab-ztype.
  MOVE 'SPA M: ' TO prntab-ztypdesc.
  MOVE 'M007' TO prntab-lgart.
  MOVE 'Monthly SPL.allowance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

*((((((((((((((((((((((((((((((((((((((
  MOVE 'D' TO prntab-ztype.
  MOVE 'OA : ' TO prntab-ztypdesc.
  MOVE '1013' TO prntab-lgart.
  MOVE 'Other Allowance .' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'D1' TO prntab-ztype.
  MOVE 'MOA : ' TO prntab-ztypdesc.
  MOVE 'M013' TO prntab-lgart.
  MOVE 'Monthly Other Allowance .' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.
*)))))))))))))))))))))))))))))))))))


  MOVE 'E' TO prntab-ztype. "D
  MOVE 'EDA : ' TO prntab-ztypdesc.
  MOVE '1005' TO prntab-lgart.
  MOVE 'Educational Allowance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'E1' TO prntab-ztype."D1
  MOVE 'EDA M: ' TO prntab-ztypdesc.
  MOVE 'M005' TO prntab-lgart.
  MOVE 'Monthly Edu. Allowance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'F' TO prntab-ztype."E
  MOVE 'CNA : ' TO prntab-ztypdesc.
  MOVE '1006' TO prntab-lgart.
  MOVE 'Conveyance Allowance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'F1' TO prntab-ztype."E1
  MOVE 'CNA M: ' TO prntab-ztypdesc.
  MOVE 'M006' TO prntab-lgart.
  MOVE 'Monthly Conveyance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'G' TO prntab-ztype."F
  MOVE 'TEL M: ' TO prntab-ztypdesc.
  MOVE 'M019' TO prntab-lgart.
  MOVE 'Monthly.Telephone Reimb.' TO prntab-desc.
  MOVE '510'  TO prntab-it.
  APPEND prntab.

  MOVE 'G1' TO prntab-ztype."F1
  MOVE 'TEL : ' TO prntab-ztypdesc.
  MOVE '1019' TO prntab-lgart.
  MOVE ' Telephone Reimbursement.' TO prntab-desc.
  MOVE '510'  TO prntab-it.
  APPEND prntab.

  MOVE 'H' TO prntab-ztype."G
  MOVE 'ENT M: ' TO prntab-ztypdesc.
  MOVE 'M020' TO prntab-lgart.
  MOVE 'Monthly.Ent. Allowance. ' TO prntab-desc.
  MOVE '589'  TO prntab-it.
  APPEND prntab.

  MOVE 'H1' TO prntab-ztype."G1
  MOVE 'ENT' TO prntab-ztypdesc.
  MOVE '1020' TO prntab-lgart.
  MOVE 'Entertainment. Allowance. ' TO prntab-desc.
  MOVE '589'  TO prntab-it.
  APPEND prntab.

  MOVE 'I' TO prntab-ztype."H
  MOVE 'CAR : ' TO prntab-ztypdesc.
  MOVE '1008' TO prntab-lgart.
  MOVE 'Car Allowance' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'I2' TO prntab-ztype."T1
  MOVE 'Hard : ' TO prntab-ztypdesc.
  MOVE '1121' TO prntab-lgart.
  MOVE 'Hard Furnishing All.' TO prntab-desc.
  APPEND prntab.

*  ---MONTHLY P.A.
  MOVE 'J' TO prntab-ztype."I
  MOVE 'MON : ' TO prntab-ztypdesc.
  MOVE 'MON1' TO prntab-lgart.
  MOVE 'MONTHLY' TO prntab-desc.
  APPEND prntab.

  MOVE 'K' TO prntab-ztype."J
  MOVE 'P.A. : ' TO prntab-ztypdesc.
  MOVE 'ANNM' TO prntab-lgart.
  MOVE 'P.A.' TO prntab-desc.
  APPEND prntab.

*----
  MOVE 'L' TO prntab-ztype."K
  MOVE 'BONUS : ' TO prntab-ztypdesc.
  MOVE 'C3G1' TO prntab-lgart.
  MOVE 'Bonus' TO prntab-desc.
  APPEND prntab.


  MOVE 'M' TO prntab-ztype."L
  MOVE 'MED : ' TO prntab-ztypdesc.
  MOVE '1102' TO prntab-lgart.
  MOVE 'Medical Reimbursement .' TO prntab-desc.
  MOVE '589'  TO prntab-it.
  APPEND prntab.

*  MOVE 'I' TO prntab-ztype.
*  MOVE 'Lease : ' TO prntab-ztypdesc.
*  MOVE 'LE12' TO prntab-lgart.
*  MOVE 'Lease Value' TO prntab-desc.
*  MOVE '581'  TO prntab-it.
*  APPEND prntab.

  MOVE 'N' TO prntab-ztype."M
  MOVE 'Lease : ' TO prntab-ztypdesc.
  MOVE '1015' TO prntab-lgart.
  MOVE 'Lease Value' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'N' TO prntab-ztype."M
  MOVE 'Lease : ' TO prntab-ztypdesc.
  MOVE '1016' TO prntab-lgart.
  MOVE 'Comp Acc.' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

*  --mONTHLY
  MOVE 'N1' TO prntab-ztype."M1
  MOVE 'Lease M: ' TO prntab-ztypdesc.
  MOVE 'M015' TO prntab-lgart.
  MOVE 'Lease Value Monthlty' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

  MOVE 'N1' TO prntab-ztype."M1
  MOVE 'Lease M: ' TO prntab-ztypdesc.
  MOVE 'M016' TO prntab-lgart.
  MOVE 'Comp Acc.Monthlty' TO prntab-desc.
  MOVE '8'  TO prntab-it.
  APPEND prntab.

*--
  MOVE 'O' TO prntab-ztype."N
  MOVE 'LTA : ' TO prntab-ztypdesc.
  MOVE '1101' TO prntab-lgart.
  MOVE 'Leave Travel Allowance' TO prntab-desc.
  MOVE '589'  TO prntab-it.
  APPEND prntab.



  MOVE 'P' TO prntab-ztype."O
  MOVE 'HFS : ' TO prntab-ztypdesc.
  MOVE '1018' TO prntab-lgart.
  MOVE 'Hard Furnishing' TO prntab-desc.
  MOVE '589'  TO prntab-it.
  APPEND prntab.

  MOVE 'Q' TO prntab-ztype."P
  MOVE 'TOT : ' TO prntab-ztypdesc.
  MOVE 'TOT1' TO prntab-lgart.
  MOVE 'TOTAL 1' TO prntab-desc.
  APPEND prntab.

  MOVE 'R' TO prntab-ztype."Q
  MOVE 'PF : ' TO prntab-ztypdesc.
  MOVE 'C3G2' TO prntab-lgart.
  MOVE 'PF' TO prntab-desc.
  APPEND prntab.

  MOVE 'R' TO prntab-ztype."Q
  MOVE 'SUO : ' TO prntab-ztypdesc.
  MOVE 'C3G4' TO prntab-lgart.
  MOVE 'Super Annuation' TO prntab-desc.
  APPEND prntab.

  MOVE 'S' TO prntab-ztype."R
  MOVE 'GRATUITY : ' TO prntab-ztypdesc.
  MOVE 'C3G3' TO prntab-lgart.
  MOVE 'GRATUITY' TO prntab-desc.
  APPEND prntab.

  MOVE 'T' TO prntab-ztype."S
  MOVE 'TOTAL : ' TO prntab-ztypdesc.
  MOVE 'TOT2' TO prntab-lgart.
  MOVE 'TOTAL 2' TO prntab-desc.
  APPEND prntab.

  MOVE 'U' TO prntab-ztype."T
  MOVE 'TFP : ' TO prntab-ztypdesc.
  MOVE 'CTC1' TO prntab-lgart.
  MOVE 'Total Fixed Pay' TO prntab-desc.
  APPEND prntab.

  MOVE 'U1' TO prntab-ztype."T1
  MOVE 'PER : ' TO prntab-ztypdesc.
  MOVE '1106' TO prntab-lgart.
  MOVE 'Performance Incentive' TO prntab-desc.
  APPEND prntab.

*   MOVE 'U2' TO prntab-ztype."T1
*  MOVE 'PER : ' TO prntab-ztypdesc.
*  MOVE '1121' TO prntab-lgart.
*  MOVE 'Hard Furnishing All.' TO prntab-desc.
*  APPEND prntab.

  MOVE 'U3' TO prntab-ztype."T2
  MOVE 'TCTC : ' TO prntab-ztypdesc.
  MOVE 'TCTC'    TO prntab-lgart.
  MOVE 'CTC'     TO prntab-desc.
  APPEND prntab.


  SORT PRNTAB BY ztype.

ENDFORM.                    " FINTAB

*&---------------------------------------------------------------------*
*&      Form  GET_COMPGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WCOMP_GROUP  text
*----------------------------------------------------------------------*
FORM get_compgrp CHANGING p_wcomp_group.

  CALL FUNCTION 'HRCM_COMPENSATION_GROUP_GET'
        EXPORTING
             pernr           = zprntab-pernr
             begda           = sy-datum
             endda           = sy-datum
*           REACTION        = ' '
       IMPORTING
            cmpgr           = p_wcomp_group
*      TABLES
*           ERROR_TABLE     =
       EXCEPTIONS
            cmpgr_not_found = 1
            OTHERS          = 2
             .
  IF sy-subrc <> 0.
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

  DATA : it_sort type slis_sortinfo_alv occurs 1 with header line.

  DATA fieldcat TYPE slis_fieldcat_alv.
  DATA : v_mcnt TYPE i.
  IF NOT <table> IS INITIAL.
    LOOP AT lt_alv_cat INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.


    DATA: ld_column      TYPE lvc_fname,
          ld_hide          TYPE C ,
          w_lines type i,
          w_no type i,
          wA_no(4) type c.
    FIELD-SYMBOLS:
       <ls_entry>     TYPE any,
       <ld_fld>         TYPE any.

    describe table alv_fieldcat lines w_lines.

    loop at it_field_dynamic where REFFIELD = 'BET01'.
      CLEAR ld_column.
      ld_column = it_field_dynamic-FIELDNAME.
      condense ld_column.
      LOOP AT <table> ASSIGNING <ls_entry>.
        clear ld_hide.
        ASSIGN COMPONENT ld_column OF STRUCTURE <ls_entry> TO <ld_fld>.

*        IF ( <ld_fld>   > '0.00' ) .
        IF ( <ld_fld>   <> '0.00' ) .
          ld_hide = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      READ TABLE alv_fieldcat INTO fieldcat
                           WITH KEY fieldname = ld_column.
      IF ( syst-subrc = 0 ).
        if ld_hide = 'X'.
          fieldcat-no_out = ''.
        else.
          fieldcat-no_out = 'X'.
        endif.
        MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
      ENDIF.
    ENDLOOP.
*    ----

    it_sort-spos = 1.
    it_sort-fieldname = 'PERNR'.
    it_sort-up = 'X'.
    it_sort-subtot = ''.
    APPEND it_sorT.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_bypassing_buffer     = 'X'
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = alv_fieldcat
        IT_SORT                = it_sort[]
        I_DEFAULT              = 'X'
        I_SAVE                 = 'A'
        it_events              = gt_events
      TABLES
        t_outtab               = <table>.
  ENDIF.

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


  LOOP AT it_field_dynamic.
    IF IT_FIELD_DYNAMIC-FIELDNAME EQ 'PERNR'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ENAME'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'STEXT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'PERSK'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'ZZLOC_TEXT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'GBDAT'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'BEGDA'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'KOSTL'
      OR IT_FIELD_DYNAMIC-FIELDNAME EQ 'KTEXT'.
      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = it_field_dynamic-fieldname.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    else.
      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = 'BET01'.

*      it_fieldcat-DO_SUM = 'X'.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
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

  IF NOT lt_alv_cat[] IS INITIAL .
* Create Dynamic Table -> i_table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_alv_cat
      IMPORTING
        ep_table        = i_table.
    ASSIGN i_table->* TO <table> .
* Create dynamic work area and assign to FS

    create data i_struct like line of <table>.
    assign i_struct->* to <struc>.


  endif.

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

  it_field_dynamic-fieldname = 'PERNR'.
  it_field_dynamic-desc      = 'Employee No'.
  it_field_dynamic-reftab   = 'PA0008'.
  it_field_dynamic-reffield = 'PERNR'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'ENAME'.
  it_field_dynamic-desc      = 'Employee Name'.
  it_field_dynamic-reftab   = 'PA0001'.
  it_field_dynamic-reffield = 'ENAME'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'STEXT'.
  it_field_dynamic-desc      = 'Designation'.
  it_field_dynamic-reftab   = 'HRP1000'.
  it_field_dynamic-reffield = 'STEXT'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'PERSK'.
  it_field_dynamic-desc      = 'Grade'.
  it_field_dynamic-reftab   = 'PA0001'.
  it_field_dynamic-reffield = 'PERSK'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'KOSTL'.
  it_field_dynamic-desc      = 'Cost Cntr.CODE'.
  it_field_dynamic-reftab   = 'PA0001'.
  it_field_dynamic-reffield = 'KOSTL'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'KTEXT'.
  it_field_dynamic-desc      = 'Cost Cntr.'.
  it_field_dynamic-reftab   = 'CSKT'.
  it_field_dynamic-reffield = 'KTEXT'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'ZZLOC_TEXT'.
  it_field_dynamic-desc      = 'Location'.
  it_field_dynamic-reftab   = 'PA0001'.
  it_field_dynamic-reffield = 'ZZLOC_TEXT'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'GBDAT'.
  it_field_dynamic-desc      = 'DOB'.
  it_field_dynamic-reftab   = 'PA0002'.
  it_field_dynamic-reffield = 'GBDAT'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'BEGDA'.
  it_field_dynamic-desc      = 'DOJ'.
  it_field_dynamic-reftab   = 'PA0000'.
  it_field_dynamic-reffield = 'BEGDA'.
  COLLECT it_field_dynamic.

  loop at prntab.

    it_field_dynamic-fieldname = prntab-lgart.
    it_field_dynamic-desc      = prntab-desc.
    it_field_dynamic-reftab   = 'PA0008'.
    it_field_dynamic-reffield = 'BET01'.
    COLLECT it_field_dynamic.

  endloop.


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

  DATA : v_text(30),
         ename like pa0001-ename,
         fl_flag.

  data : begin of stru,
       pernr like pa0015-pernr,
       ename like pa0001-ename,
       STEXT LIKE HRP1000-STEXT," DESIGNATION
       persk LIKE P0001-persk,"GRADE
       ZZLOC_TEXT  LIKE P0001-ZZLOC_TEXT,"LOCATION
       GBDAT   LIKE P0002-GBDAT,
       begda   LIKE P0000-begda,
       kostl like pa0001-kostl,
       ktext LIKE cskt-ktext,
       END OF STRU.
  FIELD-SYMBOLS : <g>,<h>,
                  <g1>,<h1>,
                  <g2>,<h2>,
                  <g3>,<h3>,
                  <g4>,<h4>,
                  <g5>,<h5>,
                  <g6>,<h6>,
                  <g7>,<h7>,
                  <g8>,<h8>,
                  <g9>,<h9>,
                  <g10>,<h10>,
                  <g11>,<h11>,
                  <g12>,<h12>,
                  <g13>,<h13>,
                  <g14>,<h14>,
                  <g15>,<h15>,
                  <g16>,<h16>,
                  <g17>,<h17>,
                  <g18>,<h18>,
                  <g19>,<h19>,
                  <g20>,<h20>,
                  <t1>,<tot1>,
                  <t2>,<tot2>,
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
                  <C>,<CTC>.

  sort fintab by pernr.

  CLEAR i_p0001.
  LOOP AT i_p0001.

*    ASSIGN COMPONENT 'PERNR'  OF STRUCTURE <STRUC>
*                                              TO <G>.
*    CONCATENATE '<struc>' '-' 'PERNR' INTO V_TEXT.
*    assign (v_text) to <h>.
*    <h> = i_p0001-PERNR.
*    CLEAR V_TEXT.

    CLEAR FINTAB.
    LOOP AT fintab WHERE pernr = i_p0001-pernr.

      CASE FINTAB-LGART.
        WHEN '1001'.
          ASSIGN COMPONENT '1001'  OF STRUCTURE <STRUC>
                                            TO <G1>.
          CONCATENATE '<struc>' '-' '1001' INTO V_TEXT.
          assign (v_text) to <h1>.
          <h1> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M001' INTO V_TEXT.
          assign (v_text) to <M1>.
          <M1> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.



        WHEN '1004'.
          ASSIGN COMPONENT '1004'  OF STRUCTURE <STRUC>
                                            TO <G2>.
          CONCATENATE '<struc>' '-' '1004' INTO V_TEXT.
          assign (v_text) to <h2>.
          <h2> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M004' INTO V_TEXT.
          assign (v_text) to <M2>.
          <M2> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1007'.
          ASSIGN COMPONENT '1007'  OF STRUCTURE <STRUC>
                                            TO <G3>.
          CONCATENATE '<struc>' '-' '1007' INTO V_TEXT.
          assign (v_text) to <h3>.
          <h3> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M007' INTO V_TEXT.
          assign (v_text) to <M3>.
          <M3> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1005'.
          ASSIGN COMPONENT '1005'  OF STRUCTURE <STRUC>
                                            TO <G4>.
          CONCATENATE '<struc>' '-' '1005' INTO V_TEXT.
          assign (v_text) to <h4>.
          <h4> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M005' INTO V_TEXT.
          assign (v_text) to <M4>.
          <M4> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1006'.
          ASSIGN COMPONENT '1006'  OF STRUCTURE <STRUC>
                                            TO <G5>.
          CONCATENATE '<struc>' '-' '1006' INTO V_TEXT.
          assign (v_text) to <h5>.
          <h5> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M006' INTO V_TEXT.
          assign (v_text) to <M5>.
          <M5> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1019'.
          ASSIGN COMPONENT '1019'  OF STRUCTURE <STRUC>
                                            TO <G6>.
          CONCATENATE '<struc>' '-' '1019' INTO V_TEXT.
          assign (v_text) to <h6>.
          <h6> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M019' INTO V_TEXT.
          assign (v_text) to <M5>.
          <M5> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN 'C3G1'.
          ASSIGN COMPONENT 'C3G2'  OF STRUCTURE <STRUC>
                                            TO <G7>.
          CONCATENATE '<struc>' '-' 'C3G1' INTO V_TEXT.
          assign (v_text) to <h7>.
          <h7> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN '1102'." Medical
          ASSIGN COMPONENT '1102'  OF STRUCTURE <STRUC>
                                            TO <G8>.
          CONCATENATE '<struc>' '-' '1102' INTO V_TEXT.
          assign (v_text) to <h8>.
*          <h8> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

           IF I_P0001-PERSK = '1A' OR I_P0001-PERSK = '1B' OR I_P0001-PERSK = '1C'
           OR I_P0001-PERSK = '2A' OR I_P0001-PERSK = '2B'
           OR I_P0001-PERSK = '3A' OR I_P0001-PERSK = '3B' OR I_P0001-PERSK = '3C'
           OR I_P0001-PERSK = '4A' OR I_P0001-PERSK = '4B'
           OR I_P0001-PERSK = '5A' OR I_P0001-PERSK = '5B' OR I_P0001-PERSK = '5C'.

            <h8>  = 15000.

          ELSE.
            <h8> = FINTAB-CHOSEN.
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
          assign (v_text) to <h9>.
          <h9> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M015' INTO V_TEXT.
          assign (v_text) to <M6>.
          <M6> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1016'.
          ASSIGN COMPONENT '1016'  OF STRUCTURE <STRUC>
                                            TO <G>.
          CONCATENATE '<struc>' '-' '1016' INTO V_TEXT.
          assign (v_text) to <h>.
          <h> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M016' INTO V_TEXT.
          assign (v_text) to <M7>.
          <M7> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1101'. " LTA
          ASSIGN COMPONENT '1101'  OF STRUCTURE <STRUC>
                                            TO <G10>.
          CONCATENATE '<struc>' '-' '1101' INTO V_TEXT.
          assign (v_text) to <h10>.
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
            <h10> = 54000.

            ELSEIF I_P0001-PERNR = '00004038'. "Dr. Avinash V. Deolekar
            <h10> = 55000.

            ELSEIF I_P0001-PERNR = '00004034' "Dr. Girish Shriram Nagarkar
                OR I_P0001-PERNR = '00004036' "Mr. Alok Kumar
                OR I_P0001-PERNR = '00004012'."Dr. Anil Ganpat Powar
            <h10> = 48000.

            ELSE.
            <h10> = 35000.
            ENDIF.

          ELSEIF I_P0001-PERSK = '2A' OR I_P0001-PERSK = '2B'.
            IF  I_P0001-PERNR = '00004030'. "Mr. Pradeep Venkatesh Kamat
            <h10> = 48000.
            ELSE.
            <h10> = 35000.
            ENDIF.
          ELSEIF I_P0001-PERSK = '3A' OR I_P0001-PERSK = '3B' OR I_P0001-PERSK = '3C'.
            <h10> = 28000.
          ELSEIF I_P0001-PERSK = '4A' OR I_P0001-PERSK = '4B' .
            <h10> = 28000.
          ELSEIF I_P0001-PERSK = '5A' OR I_P0001-PERSK = '5B' OR I_P0001-PERSK = '5C'.
            <h10> = 20000.
          ELSE.
            <h10> = FINTAB-CHOSEN.
          ENDIF.


        WHEN '1008'.
          ASSIGN COMPONENT '1008'  OF STRUCTURE <STRUC>
                                            TO <G11>.
          CONCATENATE '<struc>' '-' '1008' INTO V_TEXT.
          assign (v_text) to <h11>.
          <h11> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN '1020'.
          ASSIGN COMPONENT '1020'  OF STRUCTURE <STRUC>
                                            TO <G12>.
          CONCATENATE '<struc>' '-' '1020' INTO V_TEXT.
          assign (v_text) to <h12>.
          <h12> = FINTAB-CHOSEN .
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M020' INTO V_TEXT.
          assign (v_text) to <M5>.
          <M5> = FINTAB-CHOSEN_MON.
          CLEAR V_TEXT.

        WHEN '1018'.
          ASSIGN COMPONENT '1018'  OF STRUCTURE <STRUC>
                                            TO <G13>.
          CONCATENATE '<struc>' '-' '1018' INTO V_TEXT.
          assign (v_text) to <h13>.
          <h13> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN 'C3G2'.
          ASSIGN COMPONENT 'C3G2'  OF STRUCTURE <STRUC>
                                            TO <G14>.
          CONCATENATE '<struc>' '-' 'C3G2' INTO V_TEXT.
          assign (v_text) to <h14>.
          <h14> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN 'C3G3'.
          ASSIGN COMPONENT 'C3G3'  OF STRUCTURE <STRUC>
                                            TO <G15>.
          CONCATENATE '<struc>' '-' 'C3G3' INTO V_TEXT.
          assign (v_text) to <h15>.
          <h15> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN 'C3G4'.
          ASSIGN COMPONENT 'C3G4'  OF STRUCTURE <STRUC>
                                            TO <G16>.
          CONCATENATE '<struc>' '-' 'C3G4' INTO V_TEXT.
          assign (v_text) to <h16>.
          <h16> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN '1106'.
          ASSIGN COMPONENT '1106'  OF STRUCTURE <STRUC>
                                            TO <G17>.
          CONCATENATE '<struc>' '-' '1106' INTO V_TEXT.
          assign (v_text) to <h17>.
          <h17> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

        WHEN '1121'.
          ASSIGN COMPONENT '1121'  OF STRUCTURE <STRUC>
                                            TO <G20>.
          CONCATENATE '<struc>' '-' '1121' INTO V_TEXT.
          assign (v_text) to <h20>.
          <h20> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

       WHEN '1013'.
          ASSIGN COMPONENT '1013'  OF STRUCTURE <STRUC>
                                            TO <G19>.

          CONCATENATE '<struc>' '-' '1013' INTO V_TEXT.
          assign (v_text) to <h19>.
          <h19> = FINTAB-CHOSEN.
          CLEAR V_TEXT.

          CONCATENATE '<struc>' '-' 'M013' INTO V_TEXT.
          assign (v_text) to <M19>.
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
      assign (v_text) to <TOT1>.
*      <TOT1> = <H> + <H1> + <H2> + <H3> + <H4> + <H5>+ <H6> + <H11> + <H12> +
      <TOT1> =  <H7> + <H8> + <H9> + <H10> +  <H13> .
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'TOT2'  OF STRUCTURE <STRUC>    TO <T2>.
      CONCATENATE '<struc>' '-' 'TOT2' INTO V_TEXT.
      assign (v_text) to <TOT2>.
      <TOT2> = <H14> + <H15> + <H16> .
      CLEAR V_TEXT.


      ASSIGN COMPONENT 'ANNM'  OF STRUCTURE <STRUC>    TO <P>.
      CONCATENATE '<struc>' '-' 'ANNM' INTO V_TEXT.
      assign (v_text) to <PA>.
      <PA> = <H1> + <H2> + <H3> + <H4> + <H5>  + <H6> + <H11> + <H12> + <h19> + <h20>.
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'MON1'  OF STRUCTURE <STRUC>    TO <M>.
      CONCATENATE '<struc>' '-' 'MON1' INTO V_TEXT.
      assign (v_text) to <MON>.
      <MON> =  <PA> / 12.
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'CTC1'  OF STRUCTURE <STRUC>    TO <C>.
      CONCATENATE '<struc>' '-' 'CTC1' INTO V_TEXT.
      assign (v_text) to <CTC>.
      <CTC> = <PA> + <TOT1> + <TOT2>.
      CLEAR V_TEXT.

      ASSIGN COMPONENT 'TCTC'  OF STRUCTURE <STRUC>    TO <g18>.
      CONCATENATE '<struc>' '-' 'TCTC' INTO V_TEXT.
      assign (v_text) to <h18>.
      <h18> = <CTC> + <h17>.
      CLEAR V_TEXT.




*----

    endloop.

    at end of pernr.
      FL_flag = 1.
    endat.
    if fl_flag = 1.
      clear :ename,fl_flag,STRU,i_p0002,emp_doj.
      stru-pernr =   i_p0001-pernr.
      stru-ename =   i_p0001-ename.
      stru-persk =   i_p0001-persk .
      stru-ZZLOC_TEXT =   i_p0001-ZZLOC_TEXT .
      stru-kostl =  i_p0001-kostl.
* SELECT SINGLE STEXT INTO STRU-STEXT FROM HRP1000
*        WHERE OBJID = i_P0001-plans AND LANGU = 'EN'.

*Change by Punam
     SELECT SINGLE ktext
       INTO stru-ktext
       FROM cskt
       WHERE kostl = stru-kostl
       AND SPRAS = 'EN'
       AND KOKRS = i_p0001-BUKRS.
* POONAM: CHANGES MADE IN BELOW QUERY FOR CORRUNT DESIGNATION
*SELECTION PREVIOUSLY IT WAS MAX(BEGDA) INSTED OF MAX(AEDTM)*

      SELECT SINGLE STEXT INTO STRU-STEXT FROM HRP1000
        WHERE OBJID = i_P0001-plans AND LANGU = 'EN'
        and OTYPE = 'S'
        AND begda in
        ( SELECT MAX( begda ) FROM HRP1000 WHERE OBJID = i_P0001-plans AND LANGU = 'EN'  and OTYPE = 'S' ).

*      SELECT SINGLE STEXT INTO STRU-STEXT FROM HRP1000
*        WHERE OBJID = i_P0001-plans AND LANGU = 'EN'
*        AND BEGDA = i_P0001-BEGDA.
**        AND aedtm in ( SELECT MAX( aedtm )
**        FROM HRP1000 WHERE OBJID = i_P0001-plans
**        AND LANGU = 'EN' ).
********************
      read table i_p0002 with key pernr = i_p0001-pernr binary search.
      if sy-subrc = 0.
        stru-GBDAT = i_P0002-GBDAT.
      endif.

      read table emp_doj with key pernr = i_p0001-pernr binary search.
      if sy-subrc = 0.
        stru-begda = emp_doj-doj.
      endif.

      move-corresponding STRU to <struc>.
    ENDIF.
    append <struc> to <table>.
    clear <struc>.
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

  clear i_p0000.
  sort i_p0000 by pernr begda ascending.

  clear :  i_p00001,i_p00001[], i_p0000.
*  loop at i_p0000 where pernr = i_p0001-pernr.
*    move-corresponding i_p0000 to  i_p00001.
*    append i_p00001.
*  endloop.

  select * from PA0000 into table i_p00001
    where pernr = i_p0001-pernr.

*  ENDLOOP.
  sort i_p00001 by begda ascending.

  clear wa_start_dt.
  read table i_p00001 index 1.
  if sy-subrc = 0.
    wa_start_dt =  i_p00001-begda.
  endif.


  if not wa_start_dt is initial and wa_start_dt le '20100401'.
    CALL FUNCTION 'HR_COMPUTE_YEARS_BETWEEN_DATES'
      EXPORTING
        FIRST_DATE                        = wa_start_dt
*       MODIFY_INTERVAL                   = ' '
        SECOND_DATE                       = sy-datum
     IMPORTING
       YEARS_BETWEEN_DATES               = no_year
     EXCEPTIONS
       SEQUENCE_OF_DATES_NOT_VALID       = 1
       OTHERS                            = 2
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*    if not no_year is initial.
    clear :fintab,wa_sann.
    read table fintab with key pernr = i_p0001-pernr
                            lgart = '1001'.
    if sy-subrc = 0.
      select single * from PA0185 where
        PERNR = i_p0001-pernr and SUBTY = '01'
        and ENDDA gt '20100331'.
      if sy-subrc = 0.
        if no_year le '5'.
          wa_sann   = fintab-CHOSEN * '0.10'.
        elseif no_year gt '5' and no_year le '10'.
          wa_sann   = fintab-CHOSEN * '0.125'.
        elseif no_year gt '10'.
          wa_sann   = fintab-CHOSEN * '0.15'.
        endif.
        IF I_P0001-PERSG <> 'T'.
        CLEAR fintab.
        MOVE i_p0001-pernr TO fintab-pernr.
        MOVE 'C3G4'        TO fintab-lgart.
        fintab-chosen = WA_sann .
*        fintab-doj = wa_start_dt.
        INSERT  fintab INDEX sy-tabix.
        endif.
      endif.
    endif.
  endif.

  emp_doj-pernr = i_p0001-pernr.
  emp_doj-doj = wa_start_dt.
  append emp_doj.
  clear emp_doj.
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
  clear i_p0008.
  read table i_p0008 with key  pernr = i_p0001-pernr.
  if sy-subrc = 0.
    clear : IT_T510,T510.
    select single * from Z6HR_EMP_GRADE where pernr = i_p0001-pernr.
    if sy-subrc ne 0.
      SELECT * FROM T510   INTO CORRESPONDING FIELDS OF TABLE IT_T510
      WHERE  TRFAR =  i_p0008-TRFAR
       AND   TRFGB =  i_p0008-TRFGB
       AND   TRFGR =  i_p0008-TRFGR
       AND   TRFST =  i_p0008-TRFST
       AND   LGART IN ('1018','1019','1020').

    else.

      SELECT * FROM T510   INTO CORRESPONDING FIELDS OF TABLE IT_T510
      WHERE  TRFAR =  Z6HR_EMP_GRADE-TRFAR
       AND   TRFGB =  Z6HR_EMP_GRADE-TRFGB
       AND   TRFGR =  Z6HR_EMP_GRADE-TRFGR
       AND   TRFST =  Z6HR_EMP_GRADE-TRFST
       AND   LGART IN ('1018','1019','1020').
    endif.

    if not IT_T510 is initial.
      clear t510.
      read table IT_T510 into t510 with key lgart = '1018'.
      if sy-subrc = 0.
        CLEAR fintab.
        MOVE i_p0001-pernr TO fintab-pernr.
        fintab-lgart  = t510-lgart.
        fintab-chosen = t510-betrg .
        INSERT  fintab INDEX sy-tabix.
      endif.

      clear t510.
      read table IT_T510 into t510 with key lgart = '1019'.
      if sy-subrc = 0.
        CLEAR fintab.
        MOVE i_p0001-pernr TO fintab-pernr.
        fintab-lgart  = t510-lgart.
        fintab-chosen_mon = t510-betrg .
        fintab-chosen = t510-betrg * 12.
        INSERT  fintab INDEX sy-tabix.
      endif.

      clear t510.
      read table IT_T510 into t510 with key lgart = '1020'.
      if sy-subrc = 0.
        CLEAR fintab.
        MOVE i_p0001-pernr TO fintab-pernr.
        fintab-lgart  = t510-lgart.
        fintab-chosen_mon = t510-betrg .
        fintab-chosen = t510-betrg * 12.
        INSERT  fintab INDEX sy-tabix.
      endif.

    endif.
  endif.
ENDFORM.                    " GET_T510_DATA

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE
*---------------------------------------------------------------------*
FORM top_of_page .
  DATA : v_tcount TYPE i.
  .
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.

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

  PERFORM  eventtab_build USING gt_events .
  PERFORM  build_layout USING layout .
  PERFORM build_comment USING gt_list_top_of_page[].

ENDFORM.                    " DISPLAY_LIST
*---------------------------------------------------------------------*
*       FORM eventtab_build                                           *
*---------------------------------------------------------------------*
FORM eventtab_build USING lt_events TYPE slis_t_event.
  CONSTANTS:
  gc_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*  gc_formname_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = lt_events.

  READ TABLE lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO lt_events.
  ENDIF.

ENDFORM.                    "eventtab_build

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout USING p_layout TYPE slis_layout_alv.
  p_layout-f2code       = '&IC1'.
  p_layout-zebra        = 'X'.
*  p_layout-detail_popup = ' '.
ENDFORM.                    "build_layout
*&--------------------------------------------------------------------*
*&      Form  BUILD_comment
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_HEADING  text
*---------------------------------------------------------------------*
FORM build_comment USING p_heading TYPE slis_t_listheader.
  DATA: line        TYPE  slis_listheader ,
        rs_variant  TYPE  disvariant      ,
        c_date(10)                        .

  CLEAR : gt_list_top_of_page[], gt_list_top_of_page .

  line-info = 'INDOFIL CHEMICALS COMPANY' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  line-info = 'CTC REPORT ' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  IMPORT rs_variant FROM MEMORY ID 'VARIANT'.
  IF NOT rs_variant IS INITIAL .
    CONCATENATE 'Layout : ' rs_variant-text INTO line-info
                                             SEPARATED BY space .
    line-typ  = 'S' .
    APPEND line TO gt_list_top_of_page .
    CLEAR line .
  ENDIF .

  line-typ = 'S' .
  WRITE sy-datum TO c_date .
  CONCATENATE 'Date : ' c_date INTO line-info SEPARATED BY space .
  APPEND line TO gt_list_top_of_page .
  CLEAR line .

ENDFORM .                    "build_comment
