*&---------------------------------------------------------------------*
*& Report  Z6HR007R_WAGE_REGISTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6hr008r_bonus_register.


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
* Changed On - Friday, October 12, 2018 13:15:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Use PARAMETERS, not PARAMETER
* Solution   - Replace PARAMETERS with PARAMETER
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
  PARAMETERS : p_vari  LIKE  disvariant-variant.
*}   REPLACE
  SELECTION-SCREEN END   OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-015.
SELECT-OPTIONS : s_pernr FOR pa0001-pernr
                                MATCHCODE OBJECT prem,
                 s_werks FOR pa0001-werks,
                 s_btrtl FOR pa0001-btrtl,
                 s_persg FOR pa0001-persg, "EMPLOYEE GROUP
                 s_persk FOR pa0001-persk. "EMPLOYEE SUB GROUP

SELECTION-SCREEN END OF BLOCK a .

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-016.

SELECT-OPTIONS : s_bukrs FOR pa0001-bukrs,
                 s_abkrs FOR hrpy_rgdir-abkrs
                   MATCHCODE OBJECT h_t549t OBLIGATORY.
PARAMETERS     : p_gjahr LIKE t549q-pabrj OBLIGATORY.
SELECT-OPTIONS : s_paydt FOR hrpy_rgdir-paydt.
PARAMETERS     : p_bonus(3) TYPE n OBLIGATORY.


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
DATA : it_pa0001 LIKE STANDARD TABLE OF pa0001,
      wa_pa0001 LIKE pa0001.
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

  PERFORM display_list .
*  PERFORM F_GET_WAGE_TYPES.
  PERFORM wage_grp_get.
  PERFORM f_fill_fieldcat_struct_dyn.
  PERFORM f_field_cat_for_dyn_table.
  PERFORM f_create_dynamic_table.
  PERFORM get_data.
  PERFORM display_data.

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
  SELECT * FROM pa0001 INTO TABLE it_pa0001
                WHERE pernr IN s_pernr
                  AND bukrs IN s_bukrs
                  AND werks IN s_werks
                  AND btrtl IN s_btrtl
                  AND persg IN s_persg
                  AND persk IN s_persk
                  AND persg <> 'D'.
  IF sy-subrc = 0.
    SORT it_pa0001 BY pernr.
  ENDIF.

  SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab1
                   FOR ALL ENTRIES IN it_pa0001
                   WHERE pernr = it_pa0001-pernr
                   AND abkrs IN s_abkrs
                   AND fpper BETWEEN v_fpper AND v_flper
                   AND inper BETWEEN v_fpper AND v_flper
                   AND occat EQ space
                   AND paydt IN s_paydt.

*  SELECT * FROM hrpy_rgdir INTO CORRESPONDING FIELDS OF TABLE itab1
*                     WHERE pernr IN s_pernr
**{   REPLACE        PREK900435                                        1
**\                     AND   paydt EQ s_paydt.
*                     AND abkrs IN s_abkrs
**                     AND   paydt EQ s_paydt
*                     AND fpper BETWEEN v_fpper
*                     AND v_flper
*                     AND inper BETWEEN v_fpper
*                     AND v_flper
*                     AND occat EQ space
*                     AND paydt IN s_paydt.
**}   REPLACE


*  LOOP AT itab1 .
*    SELECT SINGLE * FROM pa0001 WHERE pernr EQ itab1-pernr
*                                  AND begda LE sy-datum
*                                  AND endda GE sy-datum
*                                  AND persg EQ 'D'.
*    IF sy-subrc EQ 0.
*      DELETE itab1 INDEX sy-tabix.
*      CLEAR itab1.
*    ENDIF.
*  ENDLOOP.

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
      "Anees
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
      "End
      MOVE-CORRESPONDING stru TO <struc>.
      APPEND <struc> TO <table>.
      CLEAR <struc>.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list .
  PERFORM  eventtab_build USING gt_events .
  PERFORM  build_layout USING layout .
  PERFORM build_comment USING gt_list_top_of_page[].

ENDFORM.                    " display_list

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
*&      Form  initialize_variant
*&---------------------------------------------------------------------*
FORM initialize_variant.
  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = prg.
  gx_variant = g_variant .
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

ENDFORM.                    " initialize_variant

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout USING p_layout TYPE slis_layout_alv.
  p_layout-f2code       = '&IC1'.
  p_layout-zebra        = 'X'.
*  p_layout-detail_popup = ' '.
ENDFORM.                    " build_layout

*&---------------------------------------------------------------------*
*&      Form  disp_alv
*&---------------------------------------------------------------------*
*FORM disp_alv.
**  layout-coltab_fieldname = 'COLOR'.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program = prg
*      it_fieldcat        = fieldcat
*      i_save             = g_save
*      is_variant         = g_variant
*      is_layout          = layout
*      it_events          = gt_events[]
*    TABLES
*      t_outtab           = disptab.
**ENDFORM.                    " disp_alv

FORM display_output .
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'.


  DATA fieldcat TYPE slis_fieldcat_alv.
  DATA : v_mcnt TYPE i.
  IF NOT <table1> IS INITIAL.
    LOOP AT lt_alv_cat1 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE1>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.



    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE1>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table1>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  IF NOT lt_alv_cat2[] IS INITIAL.
    LOOP AT lt_alv_cat2 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE2>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.


    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE2>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table2>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  IF NOT lt_alv_cat3[] IS INITIAL.
    LOOP AT lt_alv_cat3 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE3>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE3>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table3>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
    EXPORTING
      i_interface_check             = ' '
*     IS_PRINT                      =
*     I_SCREEN_START_COLUMN         = 0
*     I_SCREEN_START_LINE           = 0
*     I_SCREEN_END_COLUMN           = 0
*     I_SCREEN_END_LINE             = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER       =
*     ES_EXIT_CAUSED_BY_USER        =
    EXCEPTIONS
      program_error                 = 0
      OTHERS                        = 0.


*      fieldcat-fieldname = 'MATNR'.
*      fieldcat-col_pos   = '37'.
*      fieldcat-ref_fieldname = 'MATNR'.
*      fieldcat-ref_tabname = 'MARA'.
*      fieldcat-tabname = 'I_HEADER'.
*      fieldcat-seltext_m   = 'Material'.
**      it_fieldcat-outputlen = 22 .
*      APPEND fieldcat TO alv_fieldcat .
*      CLEAR fieldcat .


*  CALL FUNCTION 'ZCFM_HIDE_INITIALFIELD_ALV'
*    EXPORTING
*      struc      = <struc>
*      fieldcat   = alv_fieldcat
*      hide       = 'X'
*    IMPORTING
*      fieldcat_e = alv_fieldcat
*    TABLES
*      it_tab     = <table>.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*     EXPORTING
*          i_callback_program = sy-repid
*          i_bypassing_buffer = 'X'
**            i_callback_pf_status_set = 'SET_PF_STAT_L'
*          i_callback_user_command  = 'USER_COMMAND'
*          i_callback_top_of_page  = 'TOP_OF_PAGE'
*          it_fieldcat        = alv_fieldcat
*          it_events          = gt_events
**          is_Layout          = layout
**            I_STRUCTURE_NAME   = <struc>
*          TABLES
*          t_outtab           = <table>.

*  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
*            EXPORTING
**         I_INTERFACE_CHECK        = ' '
*                 i_callback_program       = SY-REPID
**         I_CALLBACK_PF_STATUS_SET =
*                i_callback_user_command  = 'USER_COMMAND'
**                 is_layout                =  layout
*                 it_fieldcat             =  alv_fieldcat
**                 is_print                =  alv_print
**         IT_EXCLUDING             =
**         IT_SPECIAL_GROUPS        =
**         IT_SORT                  =
**         IT_FILTER                =
**         IS_SEL_HIDE              =
**         I_SCREEN_START_COLUMN    = 0
**        i_screen_start_line      = 70
**         I_SCREEN_END_COLUMN      = 0
**         I_SCREEN_END_LINE        = 0
**                i_default                = 'A'
**                i_save                   = g_save
**                is_variant               = g_variant
*                it_events                = GT_events
*
**         IT_EVENT_EXIT            =
*
*                 i_tabname_header         = 'I_HEADER'
*
*                 i_tabname_item           = '<TABLE>'
**         I_STRUCTURE_NAME_HEADER  =
**         I_STRUCTURE_NAME_ITEM    =
*                 is_keyinfo               = keyinfo
**         IS_PRINT                 =
**    IMPORTING
**         E_EXIT_CAUSED_BY_CALLER  =
**         ES_EXIT_CAUSED_BY_USER   =
*            TABLES
*                 t_outtab_header          = i_header
*                 t_outtab_item            = <table>
*            EXCEPTIONS
*                 program_error            = 1
*                 OTHERS                   = 2.



ENDFORM.                    " DIsplay_output

*
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

*  READ TABLE lt_events WITH KEY name =  slis_ev_user_command
*                           INTO ls_event.
*  IF sy-subrc = 0.
*    MOVE gc_formname_user_command TO ls_event-form.
*    APPEND ls_event TO lt_events.
*  ENDIF.
ENDFORM.                    "eventtab_build

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE
*---------------------------------------------------------------------*
FORM top_of_page .
  DATA : v_tcount TYPE i.
*  V_TCOUNT = V_TCOUNT + 1.
*  IF V_TCOUNT EQ 1.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.
*  ENDIF.
ENDFORM.                    "top_of_page

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

  line-info = 'INDOFIL INDUSTRIES LTD' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  line-info = 'REPORT : Bonus Register' .
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

ENDFORM .                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  F_GET_WAGE_TYPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_wage_types .
*  PERFORM F_ADD_WAGE_TYPE USING '5000'.
*  PERFORM F_ADD_WAGE_TYPE USING '5001'.
*  PERFORM F_ADD_WAGE_TYPE USING '5020'.
*  PERFORM F_ADD_WAGE_TYPE USING '5021'.
*  PERFORM F_ADD_WAGE_TYPE USING '5022'.
*  PERFORM F_ADD_WAGE_TYPE USING '5030'.
*  PERFORM F_ADD_WAGE_TYPE USING '5040'.
*  PERFORM F_ADD_WAGE_TYPE USING '5050'.
*  PERFORM F_ADD_WAGE_TYPE USING '5060'.
*  PERFORM F_ADD_WAGE_TYPE USING '5070'.
*  PERFORM F_ADD_WAGE_TYPE USING '5080'.
*  PERFORM F_ADD_WAGE_TYPE USING '5090'.
*  PERFORM F_ADD_WAGE_TYPE USING '5210'.
*  PERFORM F_ADD_WAGE_TYPE USING '5220'.
*  PERFORM F_ADD_WAGE_TYPE USING '5230'.
*  PERFORM F_ADD_WAGE_TYPE USING '5240'.
*  PERFORM F_ADD_WAGE_TYPE USING '5510'.
*  PERFORM F_ADD_WAGE_TYPE USING '5520'.
*  PERFORM F_ADD_WAGE_TYPE USING '5530'.
*  PERFORM F_ADD_WAGE_TYPE USING '5540'.
*  PERFORM F_ADD_WAGE_TYPE USING '5920'.
*
*  PERFORM F_ADD_WAGE_TYPE USING '6610'.
*  PERFORM F_ADD_WAGE_TYPE USING '6710'.
*  PERFORM F_ADD_WAGE_TYPE USING '6720'.
*  PERFORM F_ADD_WAGE_TYPE USING '6730'.
*  PERFORM F_ADD_WAGE_TYPE USING '6740'.
*  PERFORM F_ADD_WAGE_TYPE USING '6750'.
*  PERFORM F_ADD_WAGE_TYPE USING '7470'.
*  PERFORM F_ADD_WAGE_TYPE USING '7480'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3F1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3P1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3E1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/3W1'.
*  PERFORM F_ADD_WAGE_TYPE USING '/460'.
*  PERFORM F_ADD_WAGE_TYPE USING '/560'.

  PERFORM f_add_wage_type USING '1001'.
  PERFORM f_add_wage_type USING '1002'.
  PERFORM f_add_wage_type USING '1003'.
  PERFORM f_add_wage_type USING '1004'.
  PERFORM f_add_wage_type USING '1005'.
  PERFORM f_add_wage_type USING '1006'.
  PERFORM f_add_wage_type USING '1007'.
  PERFORM f_add_wage_type USING '1008'.
  PERFORM f_add_wage_type USING '1010'.
  PERFORM f_add_wage_type USING '1011'.
  PERFORM f_add_wage_type USING '1012'.
  PERFORM f_add_wage_type USING '1013'.
  PERFORM f_add_wage_type USING '1014'.

  PERFORM f_add_wage_type USING '1101'.
  PERFORM f_add_wage_type USING '1102'.
  PERFORM f_add_wage_type USING '1103'.
  PERFORM f_add_wage_type USING '1104'.
  PERFORM f_add_wage_type USING '1105'.
  PERFORM f_add_wage_type USING '1106'.
  PERFORM f_add_wage_type USING '1107'.
  PERFORM f_add_wage_type USING '1108'.
  PERFORM f_add_wage_type USING '1109'.
  PERFORM f_add_wage_type USING '1110'.

  PERFORM f_add_wage_type USING '1301'.
  PERFORM f_add_wage_type USING '1302'.
  PERFORM f_add_wage_type USING '1303'.
  PERFORM f_add_wage_type USING '1304'.
  PERFORM f_add_wage_type USING '1305'.
  PERFORM f_add_wage_type USING '1306'.
  PERFORM f_add_wage_type USING '1307'.

  PERFORM f_add_wage_type USING '1312'.
  PERFORM f_add_wage_type USING '1313'.
  PERFORM f_add_wage_type USING '1314'.
  PERFORM f_add_wage_type USING '1315'.
  PERFORM f_add_wage_type USING '1316'.
  PERFORM f_add_wage_type USING '1317'.

  PERFORM f_add_wage_type USING '5000'.
  PERFORM f_add_wage_type USING '5001'.
  PERFORM f_add_wage_type USING '5002'.
  PERFORM f_add_wage_type USING '5003'.
  PERFORM f_add_wage_type USING '5004'.
  PERFORM f_add_wage_type USING '5005'.
  PERFORM f_add_wage_type USING '5006'.
  PERFORM f_add_wage_type USING '5007'.
  PERFORM f_add_wage_type USING '5008'.
  PERFORM f_add_wage_type USING '5009'.
  PERFORM f_add_wage_type USING '5010'.
  PERFORM f_add_wage_type USING '5011'.
  PERFORM f_add_wage_type USING '5012'.
  PERFORM f_add_wage_type USING '5013'.

  PERFORM f_add_wage_type USING '5100'.
  PERFORM f_add_wage_type USING '5101'.
  PERFORM f_add_wage_type USING '5102'.
  PERFORM f_add_wage_type USING '5103'.
  PERFORM f_add_wage_type USING '5104'.
  PERFORM f_add_wage_type USING '5105'.
  PERFORM f_add_wage_type USING '5106'.
  PERFORM f_add_wage_type USING '5107'.
  PERFORM f_add_wage_type USING '5108'.
  PERFORM f_add_wage_type USING '5109'.
  PERFORM f_add_wage_type USING '5110'.
  PERFORM f_add_wage_type USING '5111'.

  PERFORM f_add_wage_type USING '5201'.
  PERFORM f_add_wage_type USING '5202'.

  PERFORM f_add_wage_type USING '/ZP1'.

  PERFORM f_add_wage_type USING '2001'.
  PERFORM f_add_wage_type USING '2002'.
  PERFORM f_add_wage_type USING '2003'.
  PERFORM f_add_wage_type USING '2004'.
  PERFORM f_add_wage_type USING '2005'.
  PERFORM f_add_wage_type USING '2006'.
  PERFORM f_add_wage_type USING '2007'.
  PERFORM f_add_wage_type USING '2008'.
  PERFORM f_add_wage_type USING '2009'.
  PERFORM f_add_wage_type USING '2010'.
  PERFORM f_add_wage_type USING '2011'.

  PERFORM f_add_wage_type USING '3001'.
  PERFORM f_add_wage_type USING '3002'.
  PERFORM f_add_wage_type USING '3021'.
  PERFORM f_add_wage_type USING '3004'.
  PERFORM f_add_wage_type USING '3005'.
  PERFORM f_add_wage_type USING '3006'.
  PERFORM f_add_wage_type USING '3010'.
  PERFORM f_add_wage_type USING '3011'.
  PERFORM f_add_wage_type USING '3015'.
  PERFORM f_add_wage_type USING '3016'.
  PERFORM f_add_wage_type USING '3024'.

  PERFORM f_add_wage_type USING '/3E1'.
  PERFORM f_add_wage_type USING '/3F1'.
  PERFORM f_add_wage_type USING '/3F2'.
  PERFORM f_add_wage_type USING '/3P1'.
  PERFORM f_add_wage_type USING '/3W1'.
  PERFORM f_add_wage_type USING '/460'.
  PERFORM f_add_wage_type USING '/462'.

  PERFORM f_add_wage_type USING '/ROR'.



ENDFORM.                    " F_GET_WAGE_TYPES
*&---------------------------------------------------------------------*
*&      Form  F_ADD_WAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0923   text
*----------------------------------------------------------------------*
FORM f_add_wage_type  USING    value(p_0923).
  MOVE : p_0923 TO it_lgart-lgart.
  APPEND it_lgart.
  CLEAR  it_lgart.
ENDFORM.                    " F_ADD_WAGE_TYPE
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
  CLEAR wa_t549q.

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

ENDFORM.                    " F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .

  DATA : it_sort TYPE slis_sortinfo_alv OCCURS 1 WITH HEADER LINE.

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

*    ----hide column
    DATA:
  ld_column      TYPE lvc_fname,
  ld_hide          TYPE c ,
  w_lines TYPE i,
  w_no TYPE i,
  wa_no(4) TYPE c.
    FIELD-SYMBOLS:
      <ls_entry>     TYPE ANY,
      <ld_fld>         TYPE ANY.

    DESCRIBE TABLE alv_fieldcat LINES w_lines.

*    w_no = w_lines - 2.
*    wA_no = w_no + 1.
*    do w_no times.
*      wA_no = wa_no - 1.
*      condense wA_no.
*      ld_column = wA_no ."ld_column + '01'.
*      if ld_column ge 1 and ld_column lt 10.
*        concatenate '0' ld_column into ld_column.
*      endif.
    LOOP AT it_field_dynamic WHERE reffield = 'BET01'.
      CLEAR ld_column.
      ld_column = it_field_dynamic-fieldname.
      CONDENSE ld_column.
      LOOP AT <table> ASSIGNING <ls_entry>.
        CLEAR ld_hide.
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
        IF ld_hide = 'X'.
          fieldcat-no_out = ''.
        ELSE.
          fieldcat-no_out = 'X'.
        ENDIF.

        MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
      ENDIF.
    ENDLOOP.

*    READ TABLE alv_fieldcat INTO fieldcat
*                        WITH KEY fieldname = 'TNET'.
*    IF SY-SUBRC = 0.
*      fieldcat-no_out = 'X'.
*    ENDIF.
*    MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
*    ----

    it_sort-spos = 1.
    it_sort-fieldname = 'PERNR'.
    it_sort-up = 'X'.
    it_sort-subtot = ''.
    APPEND it_sort.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = sy-repid
            i_bypassing_buffer = 'X'
*            i_callback_pf_status_set = 'SET_PF_STAT_L'
*            i_callback_user_command  = 'USER_COMMAND'
            i_callback_top_of_page  = 'TOP_OF_PAGE'
            it_fieldcat        = alv_fieldcat
            it_sort                = it_sort[]
            i_default                         = 'X'
             i_save                            = 'A'
            it_events          = gt_events
*          is_Layout          = layout
*            I_STRUCTURE_NAME   = <struc>
            TABLES
            t_outtab           = <table>.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
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
