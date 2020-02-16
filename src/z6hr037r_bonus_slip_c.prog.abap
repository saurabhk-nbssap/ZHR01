*&---------------------------------------------------------------------*
*& Report  Z6HR037R_BONUS_SLIP_C
*&
*&---------------------------------------------------------------------*
*& Copy of prog Z6HR007R_WAGE_REGISTER
*& Called prog for Z6HR037R_BONUS_SLIP
*& Developer : Anees Ahamed
*& Date      : Aug 17, 2011
*&---------------------------------------------------------------------*

REPORT  z6hr037r_bonus_slip_c.


*&---------------------------------------------------------------------*
*&                      TABLES
*&---------------------------------------------------------------------*

TABLES : hrpy_rgdir, pa0001, t512t.


*&---------------------------------------------------------------------*
*&                      TYPE POOLS
*&---------------------------------------------------------------------*

TYPE-POOLS : slis.

*&---------------------------------------------------------------------*
*&                      Selection Screen
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
*{   REPLACE        SBXK900030                                        1
*\PARAMETER : p_vari  LIKE  disvariant-variant .
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Friday, October 12, 2018 13:34:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Use PARAMETERS, not PARAMETER
* Solution   - Replace PARAMETERS with PARAMETER
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
  PARAMETERS: p_vari LIKE disvariant-variant.
*}   REPLACE
  SELECTION-SCREEN END   OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-015.
SELECT-OPTIONS : s_pernr FOR pa0001-pernr
                                MATCHCODE OBJECT prem.

SELECTION-SCREEN END OF BLOCK a .

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-016.

SELECT-OPTIONS : s_abkrs FOR hrpy_rgdir-abkrs
                   MATCHCODE OBJECT h_t549t OBLIGATORY.


PARAMETERS: p_gjahr LIKE t549q-pabrj OBLIGATORY,
            p_bonus(3) TYPE n OBLIGATORY.


SELECTION-SCREEN END OF BLOCK b01.

DATA : p_fpper LIKE hrpy_rgdir-fpper.
TYPES : BEGIN OF i_t549q.
INCLUDE TYPE t549q.
TYPES : fpper(9) TYPE c.
TYPES : END OF i_t549q.
DATA : wa_t549q TYPE i_t549q.

DATA : it_t549q TYPE TABLE OF i_t549q .
*&---------------------------------------------------------------------*
*&                      FIELD SYMBOLS
*&---------------------------------------------------------------------*

FIELD-SYMBOLS : <table>    TYPE  table ,     " Main Internal Table
               <table_basic>    TYPE  table ,     " Main Internal Table
               <table_da>    TYPE  table ,     " Main Internal Table
               <struc_basic> ,
               <struc_da> ,
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
*
DATA : v_recnt TYPE i.
DATA : BEGIN OF it_field_dynamic OCCURS 0,
      fieldname LIKE dd03l-fieldname,
      reftab  LIKE dd03l-tabname,
      reffield  LIKE dd03l-reffield,
      desc(35),
      END OF it_field_dynamic.

*&---------------------------------------------------------------------*
*&                      ALV Declarations
*&---------------------------------------------------------------------*


**-- FOR TREE ALV
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

*&---------------------------------------------------------------------*
*&                      DATA DECLARATIONS
*&---------------------------------------------------------------------*

DATA : BEGIN OF itab1 OCCURS 0 ,
           pernr LIKE hrpy_rgdir-pernr,

           paydt LIKE hrpy_rgdir-paydt,
           seqnr LIKE hrpy_rgdir-seqnr,
           fpper LIKE hrpy_rgdir-fpper,
           inper LIKE hrpy_rgdir-inper,
           persg LIKE pa0001-persg,
       END OF itab1.
DATA : BEGIN OF itab_basic OCCURS 0 ,
           pernr LIKE hrpy_rgdir-pernr,

           paydt LIKE hrpy_rgdir-paydt,
           seqnr LIKE hrpy_rgdir-seqnr,
           fpper LIKE hrpy_rgdir-fpper,
           inper LIKE hrpy_rgdir-inper,
           persg LIKE pa0001-persg,
       END OF itab_basic.
DATA : BEGIN OF itab_da OCCURS 0 ,
           pernr LIKE hrpy_rgdir-pernr,

           paydt LIKE hrpy_rgdir-paydt,
           seqnr LIKE hrpy_rgdir-seqnr,
           fpper LIKE hrpy_rgdir-fpper,
           inper LIKE hrpy_rgdir-inper,
           persg LIKE pa0001-persg,
       END OF itab_da.
DATA : v_fsdate TYPE sy-datum,
        v_fedate TYPE sy-datum.
DATA : BEGIN OF rt1 OCCURS 0.
        INCLUDE STRUCTURE pc207 .
DATA : END OF rt1.

DATA : BEGIN OF rt2 OCCURS 0,
        fpper  LIKE hrpy_rgdir-inper,
        inper  LIKE hrpy_rgdir-inper,
*        ZWAGE_GRP like Z6HRA_WAGE_GRP-ZWAGE_GRP,
*        ABART  like pc207-abart,
*        LGART  like pc207-lgart,
**        APZNR  like pc207-apznr,
**        CNTR1  like pc207-cntr1,
**        CNTR2  like pc207-cntr2,
**        CNTR3  like pc207-cntr3,
*        ALZNR  like pc207-alznr,
*        C1ZNR  like pc207-c1znr,
*        BTZNR  like pc207-btznr,
*        ABZNR  like pc207-abznr,
*        V0TYP  like pc207-v0typ,
*        V0ZNR  like pc207-v0znr,
*        ZEINH  like pc207-zeinh,
*        BETPE  like pc207-betpe,
*        ANZHL  like pc207-anzhl,
        betrg  LIKE pc207-betrg,

*        RTE_CURR like pc207-RTE_CURR,
*        AMT_CURR like pc207-AMT_CURR,
*        ZZINDICATOR type Z6HRA_WAGE_GRP-ZZINDICATOR,
       END OF rt2.
DATA : rt3 LIKE rt2 OCCURS 0 WITH HEADER LINE.
DATA : rt2_basic LIKE rt2 OCCURS 0 WITH HEADER LINE.
DATA : rt2_da LIKE rt2 OCCURS 0 WITH HEADER LINE.

DATA: payresult TYPE pay99_result.
DATA : employee LIKE pa0021-betrg,
       employer LIKE pa0021-betrg,
       total    LIKE pa0021-betrg,
       tot_wage LIKE pa0021-betrg,
       amt(21),
       amt_wrd LIKE spell-word,
       v_tabix TYPE i,
       count    TYPE i.
DATA: date LIKE sy-datum.

DATA : BEGIN OF it_lgart OCCURS 0,
       lgart LIKE p0015-lgart,
       zwage_grp LIKE z6hra_wage_grp-zwage_grp,
       seq       TYPE z6hra_wage_grp-seq,
       END OF it_lgart.

DATA : BEGIN OF wa_z6hra_wage_grp,
       lgart       TYPE z6hra_wage_grp-lgart,
       zwage_grp   TYPE z6hra_wage_grp-zwage_grp,
       zwage_txt   TYPE z6hra_wage_grp-zwage_txt,
       seq         TYPE z6hra_wage_grp-seq,
       zzindicator TYPE z6hra_wage_grp-zzindicator,
       END OF wa_z6hra_wage_grp.

DATA : it_z6hra_wage_grp LIKE STANDARD TABLE OF wa_z6hra_wage_grp.

FIELD-SYMBOLS : <f1>,<g1>,<h1>,<g2>,<h2>,<g3>,
                 <gs1>,<hs1>,<gs2>,<hs2>,<gs3>,<hs3>.
  DATA : v_text(30),fl_flag,v_text1(30).
  DATA : v_txt(30).
  DATA : v_fpper TYPE hrpy_rgdir-fpper,
         v_flper TYPE hrpy_rgdir-inper.
  DATA : v_lines TYPE i.
  DATA : BEGIN OF stru,
         pernr LIKE pa0015-pernr,
         ename LIKE pa0001-ename,
         paydt LIKE sy-datum,
         bet01 LIKE pa0008-bet01,
         bet02 LIKE pa0008-bet01,
         kostl TYPE kostl,          "Anees
         ktext TYPE ktext,          "Anees
         begda TYPE dats,         "Anees
         END OF stru.
  DATA : v_total LIKE pa0008-bet01,
         esic    TYPE p DECIMALS 2,
         esic1   LIKE pa0008-bet01,
         v_dedc  LIKE pa0008-bet01,
         v_etot LIKE pa0008-bet01,
         v_dtot LIKE pa0008-bet01,
         v_ntot LIKE pa0008-bet01..
DATA: roundoff TYPE i.

*&---------------------------------------------------------------------*
*&     AT SELECTION SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

AT SELECTION-SCREEN.
  PERFORM pai_of_selection_screen.

*&---------------------------------------------------------------------*
*&     INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .
  prg = sy-repid .

*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM wage_grp_get.
  PERFORM f_fill_fieldcat_struct_dyn.
  PERFORM f_field_cat_for_dyn_table.
  PERFORM f_create_dynamic_table.
  PERFORM get_data. " This form includes data fetching in cumulated form
  PERFORM basic_calculation.
  PERFORM da_calculation.
  EXPORT itab = <table> TO MEMORY ID 'BONUS'.
  EXPORT itab_basic = <table_basic> TO MEMORY ID 'BONUS_BASIC'.
  EXPORT itab_da = <table_da> TO MEMORY ID 'BONUS_DA'.

*&---------------------------------------------------------------------*
*&                      END-OF-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
FIELD-SYMBOLS : <f1>,<g1>,<h1>,<g2>,<h2>,<g3>,
                 <gs1>,<hs1>,<gs2>,<hs2>,<gs3>,<hs3>.
  DATA : v_text(30),fl_flag,v_text1(30).
  DATA : v_txt(30).
  DATA : v_fpper TYPE hrpy_rgdir-fpper,
         v_flper TYPE hrpy_rgdir-inper.
  DATA : v_lines TYPE i.
  DATA : BEGIN OF stru,
         pernr LIKE pa0015-pernr,
         ename LIKE pa0001-ename,
         paydt LIKE sy-datum,
         bet01 LIKE pa0008-bet01,
         bet02 LIKE pa0008-bet01,
         kostl TYPE kostl,          "Anees
         ktext TYPE ktext,          "Anees
         begda TYPE dats,         "Anees
         END OF stru.
  DATA : v_total LIKE pa0008-bet01,
         esic    TYPE p DECIMALS 2,
         esic1   LIKE pa0008-bet01,
         v_dedc  LIKE pa0008-bet01,
         v_etot LIKE pa0008-bet01,
         v_dtot LIKE pa0008-bet01,
         v_ntot LIKE pa0008-bet01..
DATA: roundoff TYPE i.



  READ TABLE it_t549q INTO wa_t549q INDEX 1.
  IF sy-subrc EQ 0.
    v_fpper = wa_t549q-fpper.
  ENDIF.
  DESCRIBE TABLE it_t549q LINES v_lines.
  READ TABLE it_t549q INTO wa_t549q INDEX v_lines.
  IF sy-subrc EQ 0.
    v_flper = wa_t549q-fpper.
  ENDIF.
  SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab1
                     WHERE pernr IN s_pernr
*{   REPLACE        PREK900435                                        1
*\                     AND   paydt EQ s_paydt.
                     AND abkrs IN s_abkrs
*                     AND   paydt EQ s_paydt
                     AND fpper BETWEEN v_fpper
                     AND v_flper
                     AND inper BETWEEN v_fpper
                     AND v_flper
                     AND   occat EQ space.
*}   REPLACE


  LOOP AT itab1 .
     SELECT SINGLE * FROM pa0001 WHERE pernr EQ itab1-pernr
                                   AND begda LE sy-datum
                                   AND endda GE sy-datum
                                   AND persg EQ 'D'.
     IF sy-subrc EQ 0.
        DELETE itab1 INDEX sy-tabix.
        CLEAR itab1.
     ENDIF.
  ENDLOOP.

  SORT itab1 BY pernr paydt seqnr.
  LOOP AT itab1.
    v_tabix  = sy-tabix.
    esic = '1.75'.
    CLEAR : v_dedc,  esic1,v_etot,v_dtot,v_ntot.
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
*             clusterid               = t500l_header-relid
                 employeenumber          =  itab1-pernr
                 sequencenumber          =  itab1-seqnr
                 read_only_international =  'X'
            CHANGING
                 payroll_result          = payresult
            EXCEPTIONS
                 error_generating_import = 2
                 import_mismatch_error   = 3
                 subpool_dir_full        = 4
                 no_read_authority       = 5
                 no_record_found         = 6
                 versions_do_not_match   = 7
                 OTHERS                  = 8.
    IF sy-subrc <> 0.
      DELETE itab1 .
      CONTINUE.
    ELSE.
*   count = count + 1.
      CLEAR : rt2, rt2[].
      SORT rt1 BY lgart.
      LOOP AT payresult-inter-rt INTO rt1.
*        IF RT1-LGART+0(1) EQ '/'.
*          MOVE RT1-LGART+1(3) TO RT1-LGART.
*        ENDIF.
        IF rt1-lgart EQ '1001' OR rt1-lgart EQ '1002' OR rt1-lgart EQ '1003'
                OR rt1-lgart EQ '5000' OR rt1-lgart EQ '5001' OR rt1-lgart EQ '5002'.

          MOVE-CORRESPONDING rt1 TO rt2.
          MOVE : itab1-fpper TO rt2-fpper.
          MOVE : itab1-inper TO rt2-inper.

          COLLECT rt2.
          CLEAR : rt2.
        ENDIF.
      ENDLOOP.

*      ----add wage grp


      SORT rt2 BY  fpper inper   .
      LOOP AT  rt2.
        AT NEW  fpper.
          fl_flag = 1.
        ENDAT.
        IF fl_flag = 1.
          CLEAR fl_flag.
          LOOP AT it_t549q INTO wa_t549q  .
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE wa_t549q
                                                   TO <f1>.
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.

              CASE sy-index.
                WHEN '9'.

                  IF rt2-fpper EQ <f1> AND rt2-inper EQ <f1>.


*                    ASSIGN COMPONENT RT2-ZWAGE_GRP OF STRUCTURE <STRUC>
*                                            TO <G1>.
                    MOVE rt2-fpper TO v_txt.
                    CONCATENATE '<struc>' '-' v_txt INTO v_text.
*                    CONCATENATE '<struc>' '-' rt2-ZWAGE_GRP INTO V_TEXT.
                    ASSIGN (v_text) TO <h1>.
                    <h1> = rt2-betrg.
                    v_total = v_total + <h1>.


*                      condense <h1> no-gaps.
*---for total earnings and deductions

                  ENDIF.
              ENDCASE.
            ENDDO.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.


*      at end of paydt.
    AT END OF pernr.
      fl_flag = 1.

    ENDAT.
    IF fl_flag = 1.
      CLEAR fl_flag.
      ASSIGN COMPONENT 'TEARN' OF STRUCTURE <struc> TO <gs1>.
                          CONCATENATE '<struc>' '-' 'TEARN' INTO v_text1.
                          ASSIGN (v_text1) TO <hs1>.

                          <hs1> = v_total. .
                          CLEAR v_text1.
       ASSIGN COMPONENT 'TDEDUC' OF STRUCTURE <struc> TO <gs1>.
                          CONCATENATE '<struc>' '-' 'TDEDUC' INTO v_text1.
                          ASSIGN (v_text1) TO <hs1>.

                          <hs1> = ( v_total * p_bonus ) / 100 .
                          CLEAR roundoff.
                          roundoff = <hs1>. "Anees
                          <hs1> = roundoff. "Anees
                          CLEAR v_text1.
      MOVE-CORRESPONDING itab1 TO stru.
      CLEAR v_total.

*      select single ename from pa0001 client specified
*                              into stru-ename
*                               where mandt = sy-mandt
*                                 and pernr = itab1-pernr
*                                 and begda le sy-datum
*                                 and endda ge sy-datum.
CLEAR: stru-kostl, stru-ktext, stru-begda.
      SELECT SINGLE ename kostl FROM pa0001 CLIENT SPECIFIED
                                INTO (stru-ename, stru-kostl)
                                WHERE mandt = sy-mandt
                                 AND pernr = itab1-pernr
                                 AND begda LE sy-datum
                                 AND endda GE sy-datum.
      IF stru-kostl IS NOT INITIAL.
          SELECT SINGLE ktext FROM cskt INTO stru-ktext
            WHERE kostl = stru-kostl.
      ENDIF.
      SELECT SINGLE begda FROM pa0302 CLIENT SPECIFIED
                          INTO stru-begda
                          WHERE mandt = sy-mandt
                            AND pernr = itab1-pernr
                            AND massn = 'I7'.
      stru-begda = stru-begda - 1.
      MOVE-CORRESPONDING stru TO <struc>.
      APPEND <struc> TO <table>.
      CLEAR <struc>.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  f4_for_variant
*&---------------------------------------------------------------------*
FORM f4_for_variant.
  g_save = 'A'.
  g_variant-report = prg.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    " f4_for_variant

*&---------------------------------------------------------------------*
*&      Form  pai_of_selection_screen
*&---------------------------------------------------------------------*
FORM pai_of_selection_screen.
  IF NOT p_vari IS INITIAL.
    g_save = 'A'.
    MOVE g_variant TO gx_variant.
    MOVE p_vari TO gx_variant-variant.
    gx_variant-report = sy-repid .
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
    g_variant = gx_variant.
  ELSE .
    g_save = 'A'.
    CLEAR g_variant.
    g_variant-report = sy-repid .
    gx_variant = g_variant.
  ENDIF.
ENDFORM.                    " pai_of_selection_screen

*&---------------------------------------------------------------------*
*&      Form  f_fill_fieldcat_struct_dyn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_fieldcat_struct_dyn .
  DATA : v_text(10).
  DATA : i_month_n TYPE t247 OCCURS 0 WITH HEADER LINE.
  it_field_dynamic-fieldname = 'PERNR'.
  it_field_dynamic-desc      = 'Employee'.
  it_field_dynamic-reftab   = 'PA0015'.

  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'ENAME'.
  it_field_dynamic-desc      = 'Emp Name'.
  it_field_dynamic-reftab   = 'PA0001'.
  COLLECT it_field_dynamic.

*  it_field_dynamic-fieldname = 'PAYDT'.
*  it_field_dynamic-desc      = 'Pay Date'.
*  it_field_dynamic-reftab   = 'HRPY_RGDIR'.
*  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'TEARN'.
  it_field_dynamic-desc      = 'Total Basic+Da'.
  it_field_dynamic-reftab   = 'PA0008'.
  it_field_dynamic-reffield = 'BET01'.
  COLLECT it_field_dynamic.
*
  CONCATENATE 'Bonus@' p_bonus INTO v_text.
  it_field_dynamic-fieldname = 'TDEDUC'.
  it_field_dynamic-desc      = v_text.
  it_field_dynamic-reftab   = 'PA0008'.
  it_field_dynamic-reffield = 'BET01'.
  COLLECT it_field_dynamic.

"Anees

  it_field_dynamic-fieldname = 'KOSTL'.
  it_field_dynamic-desc      = 'Cost Center'.
  it_field_dynamic-reftab   = 'PA0001'.
  it_field_dynamic-reffield = 'KOSTL'.
  COLLECT it_field_dynamic.


  it_field_dynamic-fieldname = 'KTEXT'.
  it_field_dynamic-desc      = 'Cost Center'.
  it_field_dynamic-reftab   = 'CSKT'.
  it_field_dynamic-reffield = 'KTEXT'.
  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'BEGDA'.
  it_field_dynamic-desc      = 'Date Of Leaving'.
  it_field_dynamic-reftab   = 'PA0302'.
  it_field_dynamic-reffield = 'BEGDA'.
  COLLECT it_field_dynamic.
"End
CLEAR wa_t549q.
CLEAR   it_field_dynamic.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language                    = sy-langu
*   IMPORTING
*     RETURN_CODE                 =
    TABLES
      month_names                 =  i_month_n
*   EXCEPTIONS
*     MONTH_NAMES_NOT_FOUND       = 1
*     OTHERS                      = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT it_t549q INTO wa_t549q.
*    SELECT SINGLE * FROM t512t CLIENT SPECIFIED
*                        WHERE mandt = sy-mandt
*                          and sprsl = sy-langu
*                          and molga = '40'
*                          AND lgart = it_lgart-lgart.


*    if it_lgart-LGART+0(1) eq '/'.
*      IT_LGART-LGART = IT_LGART-LGART+1(3).
*      MODIFY IT_LGART TRANSPORTING LGART.
*
*    ENDIF.

    CONCATENATE  wa_t549q-pabrj    wa_t549q-pabrp INTO v_text.
    IF sy-subrc = 0.
      it_field_dynamic-fieldname = v_text.

     READ TABLE i_month_n WITH KEY mnr = wa_t549q-begda+4(2).
     IF sy-subrc EQ 0.
        v_text = i_month_n-ltx.
     ENDIF.

      it_field_dynamic-desc      = v_text.
      it_field_dynamic-reftab   = 'PA0008'.
      it_field_dynamic-reffield = 'BET01'.
      COLLECT it_field_dynamic.
      CLEAR   it_field_dynamic.
    ENDIF.

*    it_field_dynamic-fieldname = it_lgart-LGART.
*    it_field_dynamic-desc      = t512t-lgtxt.
*    it_field_dynamic-reftab   = 'T512T'.
*    COLLECT it_field_dynamic.

  ENDLOOP.

ENDFORM.                    " f_fill_fieldcat_struct_dyn
*&---------------------------------------------------------------------*
*&      Form  F_field_cat_for_dyn_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_field_cat_for_dyn_table .
  LOOP AT it_field_dynamic.
    IF it_field_dynamic-fieldname EQ 'PERNR'
           OR it_field_dynamic-fieldname EQ 'ENAME'
           OR it_field_dynamic-fieldname EQ 'PAYDT'
           OR it_field_dynamic-fieldname EQ 'BET01'
           OR it_field_dynamic-fieldname EQ 'BET02'
           OR it_field_dynamic-fieldname EQ 'KOSTL'
           OR it_field_dynamic-fieldname EQ 'KTEXT'
           OR it_field_dynamic-fieldname EQ 'BEGDA'.

      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = it_field_dynamic-fieldname.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    ELSE.
      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = 'BET01'.
*      it_fieldcat-ref_field = 'ZWAGE_TXT'.
      it_fieldcat-do_sum = 'X'.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_field_cat_for_dyn_table
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_dynamic_table .

  IF NOT lt_alv_cat[] IS INITIAL .
* Create Dynamic structure -> i_struct
*    CALL METHOD zcl_alv_struct_create=>create_dynammic_stru
*      EXPORTING
*        it_fieldcatalog = lt_alv_cat
*      IMPORTING
*        ep_table        = i_struct.
*    ASSIGN i_struct->* TO <struc> .       " Header Structure for <table>


* Create Dynamic Table -> i_table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_alv_cat
      IMPORTING
        ep_table        = i_table.
    ASSIGN i_table->* TO <table> .
* Create dynamic work area and assign to FS

    CREATE DATA i_struct LIKE LINE OF <table>.
    ASSIGN i_struct->* TO <struc>.


  ENDIF.

    IF NOT lt_alv_cat[] IS INITIAL .
* Create Dynamic structure -> i_struct
*    CALL METHOD zcl_alv_struct_create=>create_dynammic_stru
*      EXPORTING
*        it_fieldcatalog = lt_alv_cat
*      IMPORTING
*        ep_table        = i_struct.
*    ASSIGN i_struct->* TO <struc> .       " Header Structure for <table>


* Create Dynamic Table -> i_table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_alv_cat
      IMPORTING
        ep_table        = i_table.
    ASSIGN i_table->* TO <table_basic> .
* Create dynamic work area and assign to FS

    CREATE DATA i_struct LIKE LINE OF <table_basic>.
    ASSIGN i_struct->* TO <struc>.


  ENDIF.

    IF NOT lt_alv_cat[] IS INITIAL .
* Create Dynamic structure -> i_struct
*    CALL METHOD zcl_alv_struct_create=>create_dynammic_stru
*      EXPORTING
*        it_fieldcatalog = lt_alv_cat
*      IMPORTING
*        ep_table        = i_struct.
*    ASSIGN i_struct->* TO <struc> .       " Header Structure for <table>


* Create Dynamic Table -> i_table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_alv_cat
      IMPORTING
        ep_table        = i_table.
    ASSIGN i_table->* TO <table_da> .
* Create dynamic work area and assign to FS

    CREATE DATA i_struct LIKE LINE OF <table_da>.
    ASSIGN i_struct->* TO <struc>.


  ENDIF.

ENDFORM.                    " F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  WAGE_GRP_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM wage_grp_get .
  DATA : lv_tabix TYPE sy-tabix.
  DATA : t_t549q TYPE t549q OCCURS 0 WITH HEADER LINE.



  CALL FUNCTION 'HR_IN_GET_FISCAL_PERIOD'
   EXPORTING
     parea1                      = 'IN'
      pyear1                      = p_gjahr
    IMPORTING
      pbegda                      = v_fsdate
      pendda                      = v_fedate
*   FIYEAR                      =
* EXCEPTIONS
*   NO_ENTRY_FOUND_T549A        = 1
*   NO_ENTRY_FOUND_T549Q        = 2
*   NO_BEGIN_PERIOD_FOUND       = 3
*   NO_END_PERIOD_FOUND         = 4
*   NO_CNTRL_RECORD_FOUND       = 5
*   OTHERS                      = 6
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
      EXPORTING
        get_begda             = v_fsdate
        get_endda             = v_fedate
*        GET_PERMO             = RPTIME_PERIOD
*   IMPORTING
*     GET_PABRJ             =
*     GET_PABRP             =
      TABLES
        get_periods           = t_t549q
*   EXCEPTIONS
*     NO_PERIOD_FOUND       = 1
*     NO_VALID_PERMO        = 2
*     OTHERS                = 3
              .
    IF sy-subrc EQ 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      LOOP AT t_t549q.
        MOVE-CORRESPONDING t_t549q TO wa_t549q.
        CONCATENATE  t_t549q-pabrj t_t549q-pabrp  INTO wa_t549q-fpper.
        APPEND wa_t549q TO it_t549q.
        CLEAR  wa_t549q.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.                    " WAGE_GRP_GET
                   " FOR_DA
*&---------------------------------------------------------------------*
*&      Form  BASIC_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM basic_calculation.

  DATA : v_text_b(30),fl_flag,v_text1_b(30).
  DATA : v_txt_b(30).

  FIELD-SYMBOLS : <h2_basic>.
 READ TABLE it_t549q INTO wa_t549q INDEX 1.
  IF sy-subrc EQ 0.
    v_fpper = wa_t549q-fpper.
  ENDIF.
  DESCRIBE TABLE it_t549q LINES v_lines.
  READ TABLE it_t549q INTO wa_t549q INDEX v_lines.
  IF sy-subrc EQ 0.
    v_flper = wa_t549q-fpper.
  ENDIF.
  SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab_basic
                     WHERE pernr IN s_pernr
*{   REPLACE        PREK900435                                        1
*\                     AND   paydt EQ s_paydt.
                     AND abkrs IN s_abkrs
*                     AND   paydt EQ s_paydt
                     AND fpper BETWEEN v_fpper
                     AND v_flper
                     AND inper BETWEEN v_fpper
                     AND v_flper
                     AND   occat EQ space.
*}   REPLACE


  LOOP AT itab_basic .
     SELECT SINGLE * FROM pa0001 WHERE pernr EQ itab_basic-pernr
                                   AND begda LE sy-datum
                                   AND endda GE sy-datum
                                   AND persg EQ 'D'.
     IF sy-subrc EQ 0.
        DELETE itab_basic INDEX sy-tabix.
        CLEAR itab_basic.
     ENDIF.
  ENDLOOP.

  SORT itab_basic BY pernr paydt seqnr.
  LOOP AT itab_basic.
    v_tabix  = sy-tabix.
    esic = '1.75'.
    CLEAR : v_dedc,  esic1,v_etot,v_dtot,v_ntot.
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
*             clusterid               = t500l_header-relid
                 employeenumber          =  itab_basic-pernr
                 sequencenumber          =  itab_basic-seqnr
                 read_only_international =  'X'
            CHANGING
                 payroll_result          = payresult
            EXCEPTIONS
                 error_generating_import = 2
                 import_mismatch_error   = 3
                 subpool_dir_full        = 4
                 no_read_authority       = 5
                 no_record_found         = 6
                 versions_do_not_match   = 7
                 OTHERS                  = 8.
    IF sy-subrc <> 0.
      DELETE itab_basic .
      CONTINUE.
    ELSE.
*   count = count + 1.
      CLEAR : rt2, rt2_basic[].
      SORT rt1 BY lgart.
      LOOP AT payresult-inter-rt INTO rt1.

*        IF rt1-lgart EQ '1001' OR rt1-lgart EQ '1002' OR rt1-lgart EQ '1003'
*                OR rt1-lgart EQ '5000' OR rt1-lgart EQ '5001' OR rt1-lgart EQ '5002'.

       IF rt1-lgart EQ '1001' OR rt1-lgart EQ '5000'.
                  MOVE-CORRESPONDING rt1 TO rt2_basic.
                  MOVE : itab_basic-fpper TO rt2_basic-fpper.
                  MOVE : itab_basic-inper TO rt2_basic-inper.

          COLLECT rt2_basic.
          CLEAR : rt2_basic.
           ENDIF.

*       IF rt1-lgart EQ '1002' OR rt1-lgart EQ '1003'
*              OR rt1-lgart EQ '5001' OR rt1-lgart EQ '5002'.
*
*                  MOVE-CORRESPONDING rt1 TO rt3.
*                  MOVE : itab1-fpper TO rt3-fpper.
*                  MOVE : itab1-inper TO rt3-inper.
*
*          COLLECT rt3.
*          CLEAR : rt3.
*        ENDIF.

      ENDLOOP.

*      ----add wage grp


      SORT rt2_basic BY  fpper inper   .
      LOOP AT  rt2_basic.
        AT NEW  fpper.
          fl_flag = 1.
        ENDAT.
        IF fl_flag = 1.
          CLEAR fl_flag.
          LOOP AT it_t549q INTO wa_t549q  .
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE wa_t549q
                                                   TO <f1>.
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.

              CASE sy-index.
                WHEN '9'.

                  IF rt2_basic-fpper EQ <f1> AND rt2_basic-inper EQ <f1>.


*                    ASSIGN COMPONENT rt2_basic-ZWAGE_GRP OF STRUCTURE <STRUC>
*                                            TO <G1>.
                    MOVE rt2_basic-fpper TO v_txt_b.
                    CONCATENATE '<struc>' '-' v_txt_b INTO v_text_b.
*                    CONCATENATE '<struc>' '-' rt2_basic-ZWAGE_GRP INTO V_TEXT.
                    ASSIGN (v_text_b) TO <h2_basic>.
                    <h2_basic> = rt2_basic-betrg.
                    v_total = v_total + <h2_basic>.


*                      condense <h1> no-gaps.
*---for total earnings and deductions

                  ENDIF.

              ENDCASE.
            ENDDO.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.


*      at end of paydt.
    AT END OF pernr.
      fl_flag = 1.

    ENDAT.
    IF fl_flag = 1.
      CLEAR fl_flag.
      ASSIGN COMPONENT 'TEARN' OF STRUCTURE <struc> TO <gs1>.
                          CONCATENATE '<struc>' '-' 'TEARN' INTO v_text1_b.
                          ASSIGN (v_text1_b) TO <hs1>.

                          <hs1> = v_total. .
                          CLEAR v_text1_b.
       ASSIGN COMPONENT 'TDEDUC' OF STRUCTURE <struc> TO <gs1>.
                          CONCATENATE '<struc>' '-' 'TDEDUC' INTO v_text1_b.
                          ASSIGN (v_text1_b) TO <hs1>.

                          <hs1> = ( v_total * p_bonus ) / 100 .
                          CLEAR roundoff.
                          roundoff = <hs1>. "Anees
                          <hs1> = roundoff. "Anees
                          CLEAR v_text1_b.
      MOVE-CORRESPONDING itab_basic TO stru.
      CLEAR v_total.

*      select single ename from pa0001 client specified
*                              into stru-ename
*                               where mandt = sy-mandt
*                                 and pernr = itab1-pernr
*                                 and begda le sy-datum
*                                 and endda ge sy-datum.
CLEAR: stru-kostl, stru-ktext, stru-begda.
      SELECT SINGLE ename kostl FROM pa0001 CLIENT SPECIFIED
                                INTO (stru-ename, stru-kostl)
                                WHERE mandt = sy-mandt
                                 AND pernr = itab_basic-pernr
                                 AND begda LE sy-datum
                                 AND endda GE sy-datum.
      IF stru-kostl IS NOT INITIAL.
          SELECT SINGLE ktext FROM cskt INTO stru-ktext
            WHERE kostl = stru-kostl.
      ENDIF.
      SELECT SINGLE begda FROM pa0302 CLIENT SPECIFIED
                          INTO stru-begda
                          WHERE mandt = sy-mandt
                            AND pernr = itab_basic-pernr
                            AND massn = 'I7'.
      stru-begda = stru-begda - 1.
      MOVE-CORRESPONDING stru TO <struc>.

      APPEND <struc> TO <table_basic>.
      CLEAR <struc>.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " BASIC_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  DA_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM da_calculation .

 READ TABLE it_t549q INTO wa_t549q INDEX 1.
  IF sy-subrc EQ 0.
    v_fpper = wa_t549q-fpper.
  ENDIF.
  DESCRIBE TABLE it_t549q LINES v_lines.
  READ TABLE it_t549q INTO wa_t549q INDEX v_lines.
  IF sy-subrc EQ 0.
    v_flper = wa_t549q-fpper.
  ENDIF.
  SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab_da
                     WHERE pernr IN s_pernr
*{   REPLACE        PREK900435                                        1
*\                     AND   paydt EQ s_paydt.
                     AND abkrs IN s_abkrs
*                     AND   paydt EQ s_paydt
                     AND fpper BETWEEN v_fpper
                     AND v_flper
                     AND inper BETWEEN v_fpper
                     AND v_flper
                     AND   occat EQ space.
*}   REPLACE


  LOOP AT itab_da .
     SELECT SINGLE * FROM pa0001 WHERE pernr EQ itab1-pernr
                                   AND begda LE sy-datum
                                   AND endda GE sy-datum
                                   AND persg EQ 'D'.
     IF sy-subrc EQ 0.
        DELETE itab_da INDEX sy-tabix.
        CLEAR itab_da.
     ENDIF.
  ENDLOOP.

  SORT itab_da BY pernr paydt seqnr.
  LOOP AT itab_da.
    v_tabix  = sy-tabix.
    esic = '1.75'.
    CLEAR : v_dedc,  esic1,v_etot,v_dtot,v_ntot.
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
*             clusterid               = t500l_header-relid
                 employeenumber          =  itab_da-pernr
                 sequencenumber          =  itab_da-seqnr
                 read_only_international =  'X'
            CHANGING
                 payroll_result          = payresult
            EXCEPTIONS
                 error_generating_import = 2
                 import_mismatch_error   = 3
                 subpool_dir_full        = 4
                 no_read_authority       = 5
                 no_record_found         = 6
                 versions_do_not_match   = 7
                 OTHERS                  = 8.
    IF sy-subrc <> 0.
      DELETE itab_da .
      CONTINUE.
    ELSE.
*   count = count + 1.
      CLEAR : rt2, rt2_da[].
      SORT rt1 BY lgart.
      LOOP AT payresult-inter-rt INTO rt1.

*        IF rt1-lgart EQ '1001' OR rt1-lgart EQ '1002' OR rt1-lgart EQ '1003'
*                OR rt1-lgart EQ '5000' OR rt1-lgart EQ '5001' OR rt1-lgart EQ '5002'.

*       IF rt1-lgart EQ '1001' OR rt1-lgart EQ '5000'.
*                  MOVE-CORRESPONDING rt1 TO rt2.
*                  MOVE : itab_da-fpper TO rt2-fpper.
*                  MOVE : itab_da-inper TO rt2-inper.
*
*          COLLECT rt2.
*          CLEAR : rt2.
*           ENDIF.

       IF rt1-lgart EQ '1002' OR rt1-lgart EQ '1003'
              OR rt1-lgart EQ '5001' OR rt1-lgart EQ '5002'.

                  MOVE-CORRESPONDING rt1 TO rt2_da.
                  MOVE : itab_da-fpper TO rt2_da-fpper.
                  MOVE : itab_da-inper TO rt2_da-inper.

          COLLECT rt2_da .
          CLEAR : rt2_da .
        ENDIF.

      ENDLOOP.

*      ----add wage grp


      SORT rt2_da  BY  fpper inper   .
      LOOP AT  rt2_da .
        AT NEW  fpper.
          fl_flag = 1.
        ENDAT.
        IF fl_flag = 1.
          CLEAR fl_flag.
          LOOP AT it_t549q INTO wa_t549q  .
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE wa_t549q
                                                   TO <f1>.
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.

              CASE sy-index.
                WHEN '9'.

                  IF rt2_da-fpper EQ <f1> AND rt2_da-inper EQ <f1>.


*                    ASSIGN COMPONENT rt2_da -ZWAGE_GRP OF STRUCTURE <STRUC>
*                                            TO <G1>.
                    MOVE rt2_da-fpper TO v_txt.
                    CONCATENATE '<struc>' '-' v_txt INTO v_text.
*                    CONCATENATE '<struc>' '-' rt2_da -ZWAGE_GRP INTO V_TEXT.
                    ASSIGN (v_text) TO <h1>.
                    <h1> = rt2_da-betrg.
                    v_total = v_total + <h1>.


*                      condense <h1> no-gaps.
*---for total earnings and deductions

                  ENDIF.

              ENDCASE.
            ENDDO.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.


*      at end of paydt.
    AT END OF pernr.
      fl_flag = 1.

    ENDAT.
    IF fl_flag = 1.
      CLEAR fl_flag.
      ASSIGN COMPONENT 'TEARN' OF STRUCTURE <struc> TO <gs1>.
                          CONCATENATE '<struc>' '-' 'TEARN' INTO v_text1.
                          ASSIGN (v_text1) TO <hs1>.

                          <hs1> = v_total. .
                          CLEAR v_text1.
       ASSIGN COMPONENT 'TDEDUC' OF STRUCTURE <struc> TO <gs1>.
                          CONCATENATE '<struc>' '-' 'TDEDUC' INTO v_text1.
                          ASSIGN (v_text1) TO <hs1>.

                          <hs1> = ( v_total * p_bonus ) / 100 .
                          CLEAR roundoff.
                          roundoff = <hs1>. "Anees
                          <hs1> = roundoff. "Anees
                          CLEAR v_text1.
      MOVE-CORRESPONDING itab_da TO stru.
      CLEAR v_total.

*      select single ename from pa0001 client specified
*                              into stru-ename
*                               where mandt = sy-mandt
*                                 and pernr = itab1-pernr
*                                 and begda le sy-datum
*                                 and endda ge sy-datum.
CLEAR: stru-kostl, stru-ktext, stru-begda.
      SELECT SINGLE ename kostl FROM pa0001 CLIENT SPECIFIED
                                INTO (stru-ename, stru-kostl)
                                WHERE mandt = sy-mandt
                                 AND pernr = itab_da-pernr
                                 AND begda LE sy-datum
                                 AND endda GE sy-datum.
      IF stru-kostl IS NOT INITIAL.
          SELECT SINGLE ktext FROM cskt INTO stru-ktext
            WHERE kostl = stru-kostl.
      ENDIF.
      SELECT SINGLE begda FROM pa0302 CLIENT SPECIFIED
                          INTO stru-begda
                          WHERE mandt = sy-mandt
                            AND pernr = itab_da-pernr
                            AND massn = 'I7'.
      stru-begda = stru-begda - 1.
      MOVE-CORRESPONDING stru TO <struc>.

      APPEND <struc> TO <table_da>.
      CLEAR <struc>.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " DA_CALCULATION
