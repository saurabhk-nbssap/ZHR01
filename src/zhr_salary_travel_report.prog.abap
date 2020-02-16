*&---------------------------------------------------------------------*
*& Report  ZHR_SALARY_TRAVEL_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   23.10.2015
*        DESCRIPTION: HR: New Authorization added
*----------------------------------------------------------------------*
REPORT  zhr_salary_travel_report.

TYPE-POOLS:slis.
TABLES:pa0001, ptrv_doc_hd, qppnp.
***********************************************************************
*              Types Declaration                                      *
***********************************************************************
TYPES: BEGIN OF ty_pa0001,
         pernr TYPE pa0001-pernr,
         kostl TYPE pa0001-kostl,
         begda TYPE pa0001-begda,
         orgeh TYPE pa0001-orgeh,
         ename TYPE pa0001-ename,
         bukrs TYPE pa0001-bukrs,
         werks TYPE pa0001-werks,
         abkrs TYPE pa0001-abkrs,
         END OF ty_pa0001.

TYPES: BEGIN OF ty_output,
        sno TYPE i,
        orgeh TYPE pa0001-orgeh,
        orgtx TYPE t527x-orgtx,
        pernr TYPE pa0001-pernr,
        ename TYPE pa0001-ename,
        lgart TYPE pa0008-lga01,
        betrg TYPE netwr,
       END OF ty_output.

TYPES: BEGIN OF ty_trav,
        filler TYPE string,
        pernr TYPE string,
        trip  TYPE xblnr,
        expty TYPE string,
        expn  TYPE string,
        amt   TYPE string,
        cur   TYPE string,
        pc    TYPE string,
        date  TYPE dats,    "string,
        desc  TYPE string,
        filler1 TYPE string,
      END OF ty_trav.

TYPES: BEGIN OF ty_trav1,
        filler TYPE string,
        pernr TYPE string,
        trip  TYPE xblnr,
        expty TYPE string,
        expn  TYPE string,
        amt   TYPE netwr,
        cur   TYPE string,
        pc    TYPE string,
        date  TYPE string,
*        desc  TYPE string,
*        filler1 TYPE string,
      END OF ty_trav1.

TYPES: BEGIN OF ty_out,
        pernr TYPE pa0001-pernr,
        ename TYPE pa0001-ename,
        kostl TYPE pa0001-kostl,
        kostxt  TYPE kltxt,
        bukrs TYPE pa0001-bukrs,
        butxt TYPE butxt,
        werks TYPE pa0001-werks,
        name1 TYPE pbtxt,
        abkrs TYPE pa0001-abkrs,
        atext TYPE atext,
        from_month TYPE pnppabrp,
        to_month TYPE pnppabrp,
        gjahr TYPE gjahr,
        s101  TYPE netwr,
        s3610  TYPE netwr,
        s3611  TYPE netwr,
        s3616  TYPE netwr,
        s3618  TYPE netwr,
        s110 TYPE netwr,
        s560 TYPE netwr,
        s460 TYPE netwr,
        s3f1 TYPE netwr,
        s3p1 TYPE netwr,
        s3e1 TYPE netwr,
        s3ea TYPE netwr,
        s3l1 TYPE netwr,
        sub_sal TYPE netwr,
        eepfcont TYPE pc207-betrg,
        bonus TYPE netwr,
        leave TYPE netwr,
        pl TYPE netwr,

        aird TYPE netwr,
        book TYPE netwr,
        comp TYPE netwr,
        conf TYPE netwr,
        ent  TYPE netwr,
        htl  TYPE netwr,
        htle TYPE netwr,
        intr TYPE netwr,
        lc   TYPE netwr,
        loca TYPE netwr,
        misr TYPE netwr,
        mobi TYPE netwr,
        post TYPE netwr,
        prnt TYPE netwr,
        taoa TYPE netwr,
        tele TYPE netwr,
        tran TYPE netwr,
        treq TYPE netwr,
        vcar TYPE netwr,
        vehp TYPE netwr,
        vehr TYPE netwr,
        veis TYPE netwr,
        visa TYPE netwr,
        airp TYPE netwr,
        sub_trav TYPE netwr,
        stwf TYPE netwr,

        c_book TYPE netwr,
        c_canc TYPE netwr,
        c_ent  TYPE netwr,
        c_lc   TYPE netwr,
        c_loca TYPE netwr,
        c_misr TYPE netwr,
        c_post TYPE netwr,
        c_prnt TYPE netwr,
        c_pujc TYPE netwr,
        c_stwf TYPE netwr,
        c_stwn TYPE netwr,
        c_tele TYPE netwr,
        c_vcar TYPE netwr,
        c_vehc TYPE netwr,
        c_vehr TYPE netwr,
        c_visa TYPE netwr,
        sub_cash TYPE netwr,
        tot TYPE netwr,
      END OF ty_out.

TYPES : BEGIN OF ty_sal,
       pernr TYPE pa0001-pernr,
       bukrs TYPE pa0001-bukrs,
       werks TYPE pa0001-werks,
       abkrs TYPE pa0001-abkrs.
        INCLUDE STRUCTURE pc207.
TYPES : END OF ty_sal.

TYPES : BEGIN OF ty_period,
       month TYPE pnppabrp,
       year TYPE gjahr,
        END OF ty_period.
***********************************************************************
*         Internal Tables Declaration                                 *
***********************************************************************
DATA i_pa0001 TYPE STANDARD TABLE OF ty_pa0001.
DATA wa_pa0001 TYPE ty_pa0001.
DATA: it_rgdir   TYPE STANDARD TABLE OF pc261.
DATA: i_rgdir    TYPE STANDARD TABLE OF pc261.
DATA i_output TYPE STANDARD TABLE OF ty_output.
DATA : it_t549t TYPE TABLE OF t549t, "Payroll areas
       wa_t549t TYPE t549t,
       it_t001 TYPE TABLE OF t001,
       wa_t001 TYPE t001,
       it_t500p TYPE TABLE OF t500p,
       wa_t500p TYPE t500p,
       it_sal TYPE TABLE OF ty_sal,
       wa_sal TYPE ty_sal,
       it_trav TYPE TABLE OF ty_trav,
       it_trav1 TYPE TABLE OF ty_trav1,
       wa_trav TYPE ty_trav,
       wa_trav1 TYPE ty_trav1,
       it_out TYPE TABLE OF ty_out,
       wa_out TYPE ty_out,
       it_hd TYPE TABLE OF ptrv_doc_hd,
       wa_hd TYPE ptrv_doc_hd.

DATA:  it_cash TYPE TABLE OF ptrv_doc_hd,
       wa_cash TYPE ptrv_doc_hd,
       it_ptrv_srec TYPE STANDARD TABLE OF ptrv_srec,
       wa_ptrv_srec TYPE ptrv_srec,
       it_cskt TYPE STANDARD TABLE OF cskt,
       wa_cskt TYPE cskt,
       it_out1 TYPE TABLE OF ty_out,
       wa_out1 TYPE ty_out,
       it_period TYPE STANDARD TABLE OF ty_period,
       wa_period TYPE ty_period.

DATA: BEGIN OF wa_cash1 OCCURS 0.
        INCLUDE STRUCTURE ptrv_doc_hd.
DATA:   reinr TYPE reinr.
DATA: END OF wa_cash1,
      it_cash1 LIKE STANDARD TABLE OF wa_cash1.

DATA  BEGIN OF itab_list OCCURS 0.
        INCLUDE STRUCTURE abaplist.
DATA  END OF itab_list.

DATA: BEGIN OF vlist OCCURS 0,
          line(1024) TYPE c,
      END OF vlist.
DATA: seltab TYPE TABLE OF rsparams,
      seltab_wa LIKE LINE OF seltab.

RANGES : r_xblnr FOR ptrv_doc_hd-xblnr.
DATA : wa_xblnr LIKE LINE OF r_xblnr.

*& Header structure declaration
DATA fs_pa0001 TYPE ty_pa0001.
DATA: fs_rgdir    TYPE pc261.
DATA :wa_fieldcat TYPE slis_fieldcat_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv.

***********************************************************************
*              Variable Declaration                                   *
***********************************************************************
DATA:
  v_begda      TYPE begda,
  v_endda      TYPE endda,
  v_atext      TYPE t549t-atext,
  v_error      TYPE c,
  v_netwr(13)     TYPE p DECIMALS 2.
DATA: v_mname(15).
DATA: v_mname2(20).
DATA : v_trip(10) TYPE n,
       p_month1(2) TYPE n,
       date TYPE char10.
************************Start********************************     " added by NK on 23.10.2015
DATA : GV_AUTH_ABKRS_FLG.
DATA : PRG LIKE SY-REPID,
       REPID TYPE SY-REPID,
       G_SAVE,
       G_EXIT,
       LAYOUT TYPE SLIS_LAYOUT_ALV,
       G_VARIANT LIKE DISVARIANT,
       GX_VARIANT LIKE DISVARIANT,
       P_VARIANT LIKE DISVARIANT.
*************************End*********************************
***********************************************************************
*              Constants Declaration                                  *
***********************************************************************
CONSTANTS:
 c_x TYPE c VALUE 'X'.
***********************************************************************
*              SELECTION SCREEN                                       *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS      : p_bukrs TYPE pa0001-bukrs OBLIGATORY.
*                  p_abkrs TYPE pa0001-abkrs OBLIGATORY.
SELECT-OPTIONS  : s_abkrs FOR pa0001-abkrs OBLIGATORY,
                  s_month FOR qppnp-pabrp OBLIGATORY.
*PARAMETER       : p_month1(2) TYPE n OBLIGATORY,
*{   REPLACE        SBXK900030                                        1
*\PARAMETER       : p_fyear1(4) TYPE n OBLIGATORY .
PARAMETERS      : p_fyear1(4) TYPE n OBLIGATORY .
*}   REPLACE
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 01(20) text-t02 FOR FIELD p_month1.
*SELECTION-SCREEN POSITION 33.
*PARAMETER       : p_month1(2) TYPE n OBLIGATORY,
*                : p_fyear1(4) TYPE n OBLIGATORY .
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t03.
SELECT-OPTIONS: s_pernr FOR pa0001-pernr,
                s_kostl FOR pa0001-kostl.
SELECTION-SCREEN END OF BLOCK b2.
************************Start********************************     " added by NK on 23.10.2015
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
  PARAMETERS P_VARI TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B3.

AT SELECTION-SCREEN.
  PERFORM PAI_OF_SELECTION_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_FOR_VARIANT.

*************************End*********************************
**********************************************************************
*               AT SELECTION-SCREEN ON FIELD                          *
***********************************************************************
*AT SELECTION-SCREEN ON p_month1.
*  IF p_month1 GT '12'.
*    MESSAGE e531(0u) WITH 'Enter a Valid Period'.
*  ENDIF.

AT SELECTION-SCREEN ON s_abkrs.
* Validating the Payroll Area.
  PERFORM validate_payroll_area.
***********************************************************************
*               START-OF-SELECTION EVENT                              *
***********************************************************************
************************Start********************************     " added by NK on 23.10.2015
INITIALIZATION.
PRG   = SY-REPID.
REPID = SY-REPID.
PERFORM INITIALIZE_VARIANT.
*************************End*********************************
START-OF-SELECTION.
  PERFORM CHK_AUTH_OBJ.                         " added by Naren Karra on 23.10.2015
  PERFORM init.

  LOOP AT it_period INTO wa_period.

    PERFORM init_tab.
    PERFORM get_period.
    PERFORM get_pernrs.
    PERFORM process_data.
    PERFORM collect_data.

  ENDLOOP.

  PERFORM build_fieldcat.
  PERFORM display_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_pernrs
*&---------------------------------------------------------------------*
* This subroutine is used to get the amount to pay.
*----------------------------------------------------------------------*
* There are no interface parameters are there to pass
*----------------------------------------------------------------------*
FORM get_pernrs .
  FIELD-SYMBOLS:
      <payresult>     TYPE ANY,
      <lv_payresult>  TYPE h99_clst_s_payresult,
      <lv_versc>      TYPE pc202,
      <li_rt>         TYPE hrpay99_rt,
      <li_ddntk>      TYPE hrpay99_ddntk.
  DATA: lfs_rt         TYPE pc207.
  DATA: lfs_ddntk      TYPE pc23e.  DATA: lv_relid      TYPE t500l-relid.  DATA: lv_type       TYPE t52relid-typename.
  DATA: lv_typename   TYPE hrpclx_type.
  DATA: ref_payresult TYPE REF TO data.
  DATA: lv_molga      TYPE molga.
  DATA: lv_type_1     TYPE tadir-obj_name.
  DATA: lv_tadir      TYPE tadir-obj_name.
  DATA: lv_unpaid     TYPE ktsol.
  DATA: lv_paid       TYPE ktsol.
*& Reading all the Personnel Numbers
  SELECT pernr
          kostl
          begda
          orgeh
          ename
          bukrs
          werks
          abkrs
    FROM pa0001
    INTO TABLE i_pa0001
    WHERE bukrs = p_bukrs
      AND pernr IN s_pernr
      AND kostl IN s_kostl
      AND endda GE v_begda
      AND begda LE v_endda
      AND abkrs IN s_abkrs.
*      AND abkrs EQ p_abkrs.
  IF sy-subrc NE 0.
*    MESSAGE s531(0u) WITH 'No active Employees found for the selection'.                                 " commented by Naren Karra on 23.10.2015
    MESSAGE 'No active Employees found for the selection/Missing Authorization' type 'S' DISPLAY LIKE 'W'." added by Naren Karra on 23.10.2015
    v_error = c_x.
    STOP.
  ENDIF.                               " IF sy-subrc NE 0.
  SORT i_pa0001 BY pernr begda DESCENDING.
  DELETE ADJACENT DUPLICATES FROM i_pa0001 COMPARING pernr.
  SORT i_pa0001 BY orgeh pernr.
  LOOP AT i_pa0001 INTO fs_pa0001.
*& Reading of the Payresult for all the Periods
    REFRESH:
      it_rgdir,
      i_rgdir.
    CLEAR:fs_rgdir.
    CALL FUNCTION 'CU_READ_RGDIR'
      EXPORTING
        persnr          = fs_pa0001-pernr
      TABLES
        in_rgdir        = it_rgdir
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.
    IF sy-subrc = 0.
      CALL FUNCTION 'PYXX_GET_RELID_FROM_PERNR'
        EXPORTING
          employee                    = fs_pa0001-pernr
        IMPORTING
          relid                       = lv_relid
          molga                       = lv_molga
        EXCEPTIONS
          error_reading_infotype_0001 = 1
          error_reading_molga         = 2
          error_reading_relid         = 3
          OTHERS                      = 4.
      IF sy-subrc NE 0.
      ENDIF.
      SELECT SINGLE typename
               FROM t52relid
               INTO lv_type
              WHERE relid EQ lv_relid
                AND tabname = 'PCL2'.
      IF sy-subrc NE 0.
        lv_relid = 'IN'.
        lv_type = 'PAYIN_RESULT'.
      ENDIF.
      lv_typename = lv_type.
      CREATE DATA ref_payresult TYPE (lv_typename).
      ASSIGN ref_payresult->* TO <payresult>.
*      SRTZA--Status Of records.
*      SRTZA = 'A' --Current Result.
      DELETE it_rgdir WHERE srtza NE 'A'.
* PAYTY --> Payment Type ( Regular or Bonus)
* PAYTY =  SPACE --'Regular Payroll'.
* PAYTY = 'B'   --> 'Bonus'.
      LOOP AT it_rgdir INTO fs_rgdir WHERE payty = ''
                                     AND fpbeg GE v_begda
                                     AND fpend LE v_endda.
        APPEND fs_rgdir TO i_rgdir.
      ENDLOOP.                       " LOOP AT it_rgdir
      SORT i_rgdir BY seqnr DESCENDING.
      CLEAR: lv_unpaid,lv_paid.
      LOOP AT i_rgdir INTO fs_rgdir.
        CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
          EXPORTING
            clusterid                    = lv_relid
            employeenumber               = fs_pa0001-pernr
            sequencenumber               = fs_rgdir-seqnr
          CHANGING
            payroll_result               = <payresult>
          EXCEPTIONS
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            error_reading_archive        = 8
            error_reading_relid          = 9
            OTHERS                       = 10.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'INTER-RT' OF STRUCTURE <payresult> TO <li_rt>.

*       Fill employee data for calcutating trip details
          seltab_wa-selname = 'LDBPERNR'.
          seltab_wa-kind = 'P'.
          seltab_wa-sign    = 'I'.
          seltab_wa-option  = 'EQ'.
          seltab_wa-low    = fs_pa0001-pernr.
          APPEND seltab_wa TO seltab.

*          WRITE /: fs_pa0001-pernr.
*          SKIP 2.
          LOOP AT <li_rt> INTO lfs_rt.
            wa_sal-pernr = fs_pa0001-pernr.
            wa_sal-bukrs = fs_pa0001-bukrs.
            wa_sal-werks = fs_pa0001-werks.
            wa_sal-abkrs = fs_pa0001-abkrs.
            MOVE-CORRESPONDING lfs_rt TO wa_sal.
            APPEND wa_sal TO it_sal.
            CLEAR wa_sal.
*                        WRITE:/ lfs_rt-lgart,30 lfs_rt-betrg.
          ENDLOOP.
        ENDIF.
*        SUBMIT rpr_trip_receipt_data2 VIA SELECTION-SCREEN
*    WITH pernr = fs_pa0001-pernr EXPORTING LIST TO MEMORY.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF i_pa0001[] IS NOT INITIAL.
    SORT i_pa0001 BY abkrs.
    SELECT * FROM t549t INTO TABLE it_t549t FOR ALL ENTRIES IN i_pa0001
      WHERE sprsl = sy-langu
      AND  abkrs = i_pa0001-abkrs.

    SORT i_pa0001 BY bukrs werks.
    SELECT * FROM t500p INTO TABLE it_t500p FOR ALL ENTRIES IN i_pa0001
      WHERE persa = i_pa0001-werks
      AND   molga = '40'
      AND   bukrs = i_pa0001-bukrs.

    SELECT * FROM t001 INTO TABLE it_t001 FOR ALL ENTRIES IN i_pa0001
      WHERE bukrs = i_pa0001-bukrs.

    SORT i_pa0001 BY kostl.
    SELECT * FROM cskt INTO TABLE it_cskt FOR ALL ENTRIES IN i_pa0001
      WHERE kostl = i_pa0001-kostl
        AND spras = sy-langu.

    SORT i_pa0001 BY orgeh pernr.
  ENDIF.

**  seltab_wa-selname = 'LDBFROM'.
**  seltab_wa-kind = 'S'.
**  seltab_wa-sign    = 'I'.
**  seltab_wa-option  = 'BT'.
**  seltab_wa-low    = '00000000'.
**  seltab_wa-high   = v_endda.
**  APPEND seltab_wa TO seltab.
**
**
**  seltab_wa-selname = 'LDBTO'.
**  seltab_wa-kind = 'S'.
**  seltab_wa-sign    = 'I'.
**  seltab_wa-option  = 'BT'.
**  seltab_wa-low    = '00000000'.
**  seltab_wa-high   = v_endda.
**  APPEND seltab_wa TO seltab.
*
*  seltab_wa-selname = 'LDBRCDT'.
*  seltab_wa-kind = 'S'.
*  seltab_wa-sign    = 'I'.
*  seltab_wa-option  = 'BT'.
*  seltab_wa-low    = v_begda.
*  seltab_wa-high   = v_endda.
*  APPEND seltab_wa TO seltab.
*
*  SUBMIT rpr_trip_receipt_data2
*  WITH SELECTION-TABLE seltab
*  EXPORTING LIST TO MEMORY
*  AND RETURN.
*
*  IF sy-subrc = 0.
*    CALL FUNCTION 'LIST_FROM_MEMORY'
*      TABLES
*        listobject = itab_list
*      EXCEPTIONS
*        not_found  = 4
*        OTHERS     = 8.
*    IF itab_list[] IS NOT INITIAL.
*      CALL FUNCTION 'LIST_TO_ASCI'
*        EXPORTING
*          list_index         = -1
*        TABLES
*          listasci           = vlist
*          listobject         = itab_list
*        EXCEPTIONS
*          empty_list         = 1
*          list_index_invalid = 2
*          OTHERS             = 3.
*
*      IF sy-subrc NE '0'.
*        WRITE:/ 'LIST_TO_ASCI error !! ', sy-subrc.
*      ELSE.
*        LOOP AT vlist WHERE NOT LINE CS '-----' .
*          SPLIT vlist-line AT '|' INTO wa_trav-filler
*                                  wa_trav-pernr
*                                  wa_trav-trip
*                                  wa_trav-expty
*                                  wa_trav-expn
*                                  wa_trav-amt
*                                  wa_trav-cur
*                                  wa_trav-pc
*                                  date        "wa_trav-date     "04/05/2012
*                                  wa_trav-desc
*                                  wa_trav-filler1.
*
*          CLEAR v_trip.
*          v_trip = wa_trav-trip.
*          wa_trav-trip = v_trip.
*          "Start 04/05/2012
*          REPLACE ALL OCCURRENCES OF '.' IN date WITH ''.
*          CONCATENATE date+4(4) date+2(2) INTO wa_trav-date.
*          CONCATENATE wa_trav-date date+0(2) INTO wa_trav-date.
*          "End 04/05/2012
*          REPLACE '*' INTO  wa_trav-pernr WITH space.
*          REPLACE ALL OCCURRENCES OF ',' IN wa_trav-amt WITH ''.
*
*          CONDENSE : wa_trav-filler,
*                    wa_trav-pernr,
*                    wa_trav-trip ,
*                    wa_trav-expty,
*                    wa_trav-expn ,
*                    wa_trav-amt,
*                    wa_trav-cur  ,
*                    wa_trav-pc   ,
*                    wa_trav-date ,
*                    wa_trav-desc,
*                    wa_trav-filler1.
*
*          IF wa_trav-amt CO '1234567890. '.
*            MOVE wa_trav-amt TO v_netwr.
*            wa_trav-amt = v_netwr.
*          ENDIF.
*          APPEND wa_trav TO it_trav.
*          CLEAR wa_trav.
*        ENDLOOP.
*        DELETE it_trav INDEX 1.
*        DATA : w_lines TYPE i.
*        DESCRIBE TABLE it_trav LINES w_lines.
*        DELETE it_trav INDEX w_lines.
**      DELETE it_trav WHERE trip NE ' '.
*        DELETE it_trav WHERE trip EQ '0000000000'.
*
*        "Start 04/05/2012
*        DELETE it_trav WHERE date NOT BETWEEN v_begda AND v_endda.
*        LOOP AT  it_trav INTO wa_trav.
*          CLEAR : wa_trav-date, wa_trav-trip.
*          MOVE-CORRESPONDING wa_trav TO wa_trav1.
*          COLLECT wa_trav1 INTO it_trav1.
*        ENDLOOP.
*
**        IF it_trav[] IS NOT INITIAL.
**          SELECT * FROM ptrv_doc_hd INTO TABLE it_hd
**            FOR ALL ENTRIES IN it_trav
**            WHERE xblnr = it_trav-trip
**            AND  blart = 'AB'
**            AND  budat BETWEEN v_begda AND v_endda.
**        ENDIF.
**
**        LOOP AT it_hd INTO wa_hd.
**          CLEAR wa_trav.
**          LOOP AT  it_trav INTO wa_trav WHERE trip = wa_hd-xblnr.
**            CLEAR : wa_trav-date, wa_trav-trip.
**            MOVE-CORRESPONDING wa_trav TO wa_trav1.
**            COLLECT wa_trav1 INTO it_trav1.
**          ENDLOOP.
**        ENDLOOP.
*        "End 04/05/2012
*
*        CALL FUNCTION 'LIST_FREE_MEMORY'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
**  Cash part of ESS
*  SELECT * FROM ptrv_doc_hd INTO TABLE it_cash
*    WHERE int_glvor = 'TRV1'
*      AND commi = 'X'
*      AND bukrs = p_bukrs
*      AND budat  BETWEEN v_begda AND v_endda.
*  IF it_cash IS NOT INITIAL.
*    LOOP AT it_cash INTO wa_cash.
*      CONDENSE wa_cash-xblnr.
*      wa_cash1-reinr = wa_cash-xblnr.
*      APPEND wa_cash1 TO it_cash1.
*      CLEAR wa_cash1.
*    ENDLOOP.
*    SORT it_cash1 BY reinr.
*    SELECT * FROM ptrv_srec INTO TABLE it_ptrv_srec
*      FOR ALL ENTRIES IN it_cash1
*      WHERE reinr = it_cash1-reinr
*        AND pernr IN s_pernr.
*    IF sy-subrc = 0.
*      SORT it_ptrv_srec BY pernr exp_type.
*    ENDIF.
*  ENDIF.
*  Travel part of ESS
  SELECT * FROM ptrv_srec INTO TABLE it_ptrv_srec
     WHERE pernr IN s_pernr
       AND rec_date BETWEEN v_begda AND v_endda
       AND payot = 'TC'.
  IF sy-subrc = 0.
    SORT it_ptrv_srec BY pernr exp_type.
    LOOP AT it_ptrv_srec INTO wa_ptrv_srec.
      wa_trav1-pernr = wa_ptrv_srec-pernr.
      wa_trav1-expty = wa_ptrv_srec-exp_type.
      wa_trav1-amt   = wa_ptrv_srec-loc_amount.
      COLLECT wa_trav1 INTO it_trav1.
    ENDLOOP.
  ENDIF.
  REFRESH: it_ptrv_srec.
*  Cash part of ESS
  SELECT * FROM ptrv_srec INTO TABLE it_ptrv_srec
     WHERE pernr IN s_pernr
       AND rec_date BETWEEN v_begda AND v_endda
       AND payot = 'CC'.
  IF sy-subrc = 0.
    SORT it_ptrv_srec BY pernr exp_type.
  ENDIF.
ENDFORM.                    "get_pernrs
"get_pernrs*&---------------------------------------------------------------------*
*&      Form  get_period
*&---------------------------------------------------------------------*
* This subroutine is used to calculate the period range
*----------------------------------------------------------------------*
* There are no interface parameters are there to pass
*----------------------------------------------------------------------*
FORM get_period .
  p_fyear1 = wa_period-year.
  p_month1 = wa_period-month.

  SELECT SINGLE begda
                endda
           INTO (v_begda,
                 v_endda)
           FROM t549q
          WHERE permo = '01'
            AND pabrj = p_fyear1
            AND pabrp = p_month1.
  IF sy-subrc NE 0.
    MESSAGE s531(0u) WITH 'Error in Period calculation- table T549Q'.
    v_error = 'X'.
    STOP.
  ENDIF.
ENDFORM.                               " Get_period
*&---------------------------------------------------------------------*
*&      Form  validate_payroll_area
*&---------------------------------------------------------------------*
* This Subroutine is used to validate the payroll area
*----------------------------------------------------------------------*
* There are no Parameters to pass this subroutine
*----------------------------------------------------------------------*
FORM validate_payroll_area .
  DATA:    lv_abkrs TYPE t549a-abkrs.
  SELECT SINGLE abkrs                  " Payroll Area
           FROM t549a
           INTO lv_abkrs
           WHERE abkrs IN s_abkrs.
*          WHERE abkrs EQ p_abkrs.
  IF sy-subrc NE 0.
    MESSAGE e531(0u) WITH 'Enter a valid Payroll Area'.
  ENDIF.                               " IF sy-subrc NE 0.
ENDFORM.                               " Validate_payroll_area
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  SORT it_sal BY pernr lgart.
  SORT it_trav1 BY pernr expty.

  CLEAR wa_out.
  LOOP AT it_sal INTO wa_sal.
    wa_out-pernr = wa_sal-pernr.
    wa_out-from_month = s_month-low.
    wa_out-to_month = s_month-high.
    wa_out-gjahr = wa_period-year.
    READ TABLE i_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_out-pernr.
    wa_out-ename = wa_pa0001-ename.
    wa_out-bukrs = wa_sal-bukrs.
    wa_out-kostl = wa_pa0001-kostl.
    CLEAR wa_t001.
    READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_out-kostl.
    wa_out-kostxt = wa_cskt-ltext.
    CLEAR wa_cskt.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_sal-bukrs.
    wa_out-butxt = wa_t001-butxt.
    wa_out-werks = wa_sal-werks.
    CLEAR wa_t500p.
    READ TABLE it_t500p INTO wa_t500p WITH KEY persa = wa_sal-werks
                                               molga = '40'
                                               bukrs = wa_sal-bukrs.
    wa_out-name1 = wa_t500p-name1.


    wa_out-abkrs = wa_sal-abkrs.
    CLEAR wa_t549t.
    READ TABLE it_t549t INTO wa_t549t WITH KEY abkrs = wa_sal-abkrs.
    wa_out-atext = wa_t549t-atext.
    CASE wa_sal-lgart.
      WHEN '/101'.
        wa_out-s101 = wa_sal-betrg.
      WHEN '/110'.
        wa_out-s110 = wa_sal-betrg.
      WHEN '/560'.
        wa_out-s560 = wa_sal-betrg.
      WHEN '/460'.
        wa_out-s460 = wa_sal-betrg.
      WHEN '/3F1'.
        wa_out-s3f1 = wa_sal-betrg.
      WHEN '/3P1'.
        wa_out-s3p1 = wa_sal-betrg.
      WHEN '/3E1'.
        wa_out-s3e1 = wa_sal-betrg.
      WHEN '/3EA'.
        wa_out-s3ea = wa_sal-betrg.
      WHEN '/3L1'.
        wa_out-s3l1 = wa_sal-betrg.
      WHEN '3610'.
        wa_out-s3610 = wa_sal-betrg.
      WHEN '3611'.
        wa_out-s3611 = wa_sal-betrg.
      WHEN '3616'.
        wa_out-s3616 = wa_sal-betrg.
      WHEN '3618'.
        wa_out-s3618 = wa_sal-betrg.
    ENDCASE.
    AT END OF pernr.
      LOOP AT it_trav1 INTO wa_trav1 WHERE pernr = wa_out-pernr.
        CASE wa_trav1-expty.
          WHEN 'AIRD'.
            wa_out-aird = wa_trav1-amt.
          WHEN 'BOOK'.
            wa_out-book = wa_trav1-amt.
          WHEN 'COMP'.
            wa_out-comp = wa_trav1-amt.
          WHEN 'CONF'.
            wa_out-conf = wa_trav1-amt.
          WHEN 'ENT'.
            wa_out-ent  = wa_trav1-amt.
          WHEN 'HTL'.
            wa_out-htl  = wa_trav1-amt.
          WHEN 'HTLE'.
            wa_out-htle = wa_trav1-amt.
          WHEN 'INTR'.
            wa_out-intr = wa_trav1-amt.
          WHEN 'LC'.
            wa_out-lc  = wa_trav1-amt.
          WHEN 'LOCA'.
            wa_out-loca = wa_trav1-amt.
          WHEN 'MISR'.
            wa_out-misr = wa_trav1-amt.
          WHEN 'MOBI'.
            wa_out-mobi = wa_trav1-amt.
          WHEN 'POST'.
            wa_out-post = wa_trav1-amt.
          WHEN 'PRNT'.
            wa_out-prnt = wa_trav1-amt.
          WHEN 'TAOA'.
            wa_out-taoa = wa_trav1-amt.
          WHEN 'TELE'.
            wa_out-tele = wa_trav1-amt.
          WHEN 'TRAN'.
            wa_out-tran = wa_trav1-amt.
          WHEN 'TREQ'.
            wa_out-treq = wa_trav1-amt.
          WHEN 'VCAR'.
            wa_out-vcar = wa_trav1-amt.
          WHEN 'VEHP'.
            wa_out-vehp = wa_trav1-amt.
          WHEN 'VEHR'.
            wa_out-vehr = wa_trav1-amt.
          WHEN 'VEIS'.
            wa_out-veis = wa_trav1-amt.
          WHEN 'VISA'.
            wa_out-visa = wa_trav1-amt.
          WHEN 'AIRP'.
*             BREAK-POINT.
*             move wa_trav-amt to v_netwr.
            wa_out-airp = wa_trav1-amt.
          WHEN 'STWF'.
            wa_out-stwf = wa_trav1-amt.
        ENDCASE.
      ENDLOOP.
      LOOP AT it_ptrv_srec INTO wa_ptrv_srec WHERE pernr = wa_out-pernr.
        CASE wa_ptrv_srec-exp_type.
          WHEN 'BOOK'.
            wa_out-c_book = wa_out-c_book + wa_ptrv_srec-loc_amount.
          WHEN 'CANC'.
            wa_out-c_canc = wa_out-c_canc + wa_ptrv_srec-loc_amount.
          WHEN 'ENT'.
            wa_out-c_ent = wa_out-c_ent + wa_ptrv_srec-loc_amount.
          when 'LC'.
            wa_out-c_lc  = wa_out-c_lc + wa_ptrv_srec-loc_amount.
          WHEN 'LOCA'.
            wa_out-c_loca = wa_out-c_loca + wa_ptrv_srec-loc_amount.
          WHEN 'MISR'.
            wa_out-c_misr = wa_out-c_misr + wa_ptrv_srec-loc_amount.
          WHEN 'POST'.
            wa_out-c_post = wa_out-c_post + wa_ptrv_srec-loc_amount.
          WHEN 'PRNT'.
            wa_out-c_prnt = wa_out-c_prnt + wa_ptrv_srec-loc_amount.
          WHEN 'PUJC'.
            wa_out-c_pujc = wa_out-c_pujc + wa_ptrv_srec-loc_amount.
          WHEN 'STWF'.
            wa_out-c_stwf = wa_out-c_stwf + wa_ptrv_srec-loc_amount.
          WHEN 'STWN'.
            wa_out-c_stwn = wa_out-c_stwn + wa_ptrv_srec-loc_amount.
          WHEN 'TELE'.
            wa_out-c_tele = wa_out-c_tele + wa_ptrv_srec-loc_amount.
          WHEN 'VCAR'.
            wa_out-c_vcar = wa_out-c_vcar + wa_ptrv_srec-loc_amount.
          WHEN 'VEHC'.
            wa_out-c_vehc = wa_out-c_vehc + wa_ptrv_srec-loc_amount.
          WHEN 'VEHR'.
            wa_out-c_vehr = wa_out-c_vehr + wa_ptrv_srec-loc_amount.
          WHEN 'VISA'.
            wa_out-c_visa = wa_out-c_visa + wa_ptrv_srec-loc_amount.

        ENDCASE.
      ENDLOOP.

      PERFORM get_pf.
      wa_out-s101 = wa_out-s101 - wa_out-bonus.   "After checking report  pls. ignore wage type 1110 ( Bonus Payment) from S-Total Gross Amount.
      wa_out-s101 = wa_out-s101 - wa_out-leave.   "Wage Type 1315 – PL FINAL SETTLEMENT ( Leave encashment) need to be remove from column S-GROSS AMOUNT.
      wa_out-s101 = wa_out-s101 - wa_out-pl.      "One more Wage Type need to be remove ie 1312 PL Regular

      wa_out-sub_sal = wa_out-s101 +
                   wa_out-s3610 +
                   wa_out-s3611 +
                   wa_out-s3618 +
                   wa_out-s3616 +
                   wa_out-eepfcont.

      wa_out-sub_trav = wa_out-aird +
                   wa_out-book +
                   wa_out-comp +
                   wa_out-conf +
                   wa_out-ent  +
                   wa_out-htl  +
                   wa_out-htle +
                   wa_out-intr +
                   wa_out-lc   +
                   wa_out-loca +
                   wa_out-misr +
                   wa_out-mobi +
                   wa_out-post +
                   wa_out-prnt +
                   wa_out-taoa +
                   wa_out-tele +
                   wa_out-tran +
                   wa_out-treq +
                   wa_out-vcar +
                   wa_out-vehp +
                   wa_out-vehr +
                   wa_out-veis +
                   wa_out-visa +
                   wa_out-airp +
                   wa_out-stwf.

      wa_out-sub_cash = wa_out-c_book +
                   wa_out-c_canc +
                   wa_out-c_ent +
                   wa_out-c_lc   +
                   wa_out-c_loca +
                   wa_out-c_misr +
                   wa_out-c_post +
                   wa_out-c_prnt +
                   wa_out-c_pujc +
                   wa_out-c_stwf +
                   wa_out-c_stwn +
                   wa_out-c_tele +
                   wa_out-c_vcar +
                   wa_out-c_vehc +
                   wa_out-c_vehr +
                   wa_out-c_visa.

      wa_out-tot = wa_out-sub_sal +
                   wa_out-sub_trav +
                   wa_out-sub_cash.

*      wa_out-tot = wa_out-s101 +
*                   wa_out-s3610 +
*                   wa_out-s3611 +
*                   wa_out-s3618 +
*                   wa_out-s3616 +
*                   wa_out-aird +
*                   wa_out-book +
*                   wa_out-comp +
*                   wa_out-conf +
*                   wa_out-ent  +
*                   wa_out-htl  +
*                   wa_out-htle +
*                   wa_out-intr +
*                   wa_out-loca +
*                   wa_out-mobi +
*                   wa_out-post +
*                   wa_out-prnt +
*                   wa_out-taoa +
*                   wa_out-tele +
*                   wa_out-tran +
*                   wa_out-treq +
*                   wa_out-vcar +
*                   wa_out-vehp +
*                   wa_out-vehr +
*                   wa_out-veis +
*                   wa_out-visa +
*                   wa_out-airp.
      APPEND wa_out TO it_out.
      CLEAR wa_out.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat .
  PERFORM get_fieldcat USING :
   'PERNR' text-033,
   'ENAME' text-040,
   'KOSTL' text-052,
   'KOSTXT' text-068,
*   'BUKRS' text-034,
*   'BUTXT' text-035,
*   'WERKS' text-036,
   'NAME1' text-037.
*   'ABKRS' text-038,
*   'ATEXT' text-039,

  IF s_month-high IS INITIAL.
    PERFORM get_fieldcat USING :
       'FROM_MONTH' text-069,
       'GJAHR' text-070.
  ELSE.
    PERFORM get_fieldcat USING :
       'FROM_MONTH' text-071,
       'TO_MONTH' text-072,
       'GJAHR' text-070.
  ENDIF.


  PERFORM get_fieldcat USING :
     'S101' text-024,
     'EEPFCONT' text-077, "'S - EePfCont',
     'S3610' text-041,
     'S3611' text-042,
     'S3616' text-043,
     'S3618' text-048,
     'SUB_SAL' text-049,
*    'S101' text-024,
*    'S110' text-025,
*   'S560' text-026,
*    'S460' text-027,
*    'S3F1' text-028,
*    'S3P1' text-029,
*    'S3E1' text-030,
*    'S3EA' text-031,
*    'S3L1' text-032,
    'AIRD' text-001,
    'BOOK' text-002,
    'COMP' text-003,
    'CONF' text-004,
    'ENT' text-005,
    'HTL' text-006,
    'HTLE' text-007,
    'INTR' text-008,
    'LC' text-076,
    'LOCA' text-009,
    'MISR' text-075,
    'MOBI' text-010,
    'POST' text-011,
    'PRNT' text-012,
    'TAOA' text-013,
    'TELE' text-014,
    'TRAN' text-015,
    'TREQ' text-016,
    'VCAR' text-017,
    'VEHP' text-018,
    'VEHR' text-019,
    'VEIS' text-020,
    'VISA' text-021,
    'AIRP' text-022,
    'STWF' text-073,
    'SUB_TRAV' text-050,
    'C_BOOK' text-053,
    'C_CANC' text-054,
    'C_ENT' text-055,
    'C_LC' text-056,
    'C_LOCA' text-056,
    'C_MISR' text-057,
    'C_POST' text-058,
    'C_PRNT' text-059,
    'C_PUJC' text-060,
    'C_STWF' text-061,
    'C_STWN' text-062,
    'C_TELE' text-063,
    'C_VCAR' text-064,
    'C_VEHC' text-065,
    'C_VEHR' text-066,
    'C_VISA' text-067,
    'SUB_CASH' text-051,
    'TOT' text-023.




ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCAT
*&---------------------------------------------------------------------*
* Fills the field catalog
*----------------------------------------------------------------------*
*      -->p_dataname output field name
*      -->p_tabname reference DB table name
*      -->P_fieldname refer DB field name
*      -->P_coltext column heading
*----------------------------------------------------------------------*
FORM get_fieldcat  USING    p_dataname  TYPE lvc_s_fcat-fieldname
                            p_coltext   TYPE lvc_s_fcat-coltext.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname   = p_dataname.
  wa_fieldcat-seltext_m      = p_coltext.
  wa_fieldcat-tabname = 'IT_OUT'.
  wa_fieldcat-outputlen = 15.
  APPEND wa_fieldcat TO it_fieldcat.
ENDFORM.                    " GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  SORT it_out1 BY pernr.
************************Start********************************     " added by NK on 23.10.2015
  IF IT_OUT1[] IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = PRG
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
*     IS_LAYOUT                         =
     IT_FIELDCAT                       = IT_FIELDCAT
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
     I_SAVE                            = G_SAVE
     IS_VARIANT                        = G_VARIANT
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = IT_OUT1[]
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ELSE.
  MESSAGE 'No records found / Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.
ENDIF.
*************************End*********************************
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'                         " commented by Naren Karra on 23.10.2015
*    EXPORTING
*      i_callback_program = sy-repid
*      it_fieldcat        = it_fieldcat
*    TABLES
*      t_outtab           = it_out1[]
*    EXCEPTIONS
*      program_error      = 1
*      OTHERS             = 2.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tab .
  CLEAR: v_begda, v_endda.
  REFRESH: it_out, i_pa0001, seltab, itab_list, vlist, it_trav, it_hd, it_trav1, it_cash, it_ptrv_srec, it_sal.
ENDFORM.                    " INIT_TAB
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .
  DATA: flag TYPE i VALUE 0.
  DO 12 TIMES.
    flag = flag + 1.
    IF flag IN s_month.
      wa_period-month = flag.
      wa_period-year = p_fyear1.
      APPEND wa_period TO it_period.
    ENDIF.
  ENDDO.
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  COLLECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_data .
*Gathers data monthwise.
  LOOP AT it_out INTO wa_out.
    wa_out1 = wa_out.
    COLLECT wa_out1 INTO it_out1.
    CLEAR: wa_out, wa_out1.
  ENDLOOP.
ENDFORM.                    " COLLECT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_PF .

   data: begin of vlist occurs 0,
             line(1024) type c,
         end of vlist.
   data: list_tab like standard table of abaplist,
         blank type char8,
         lines type i.
   data: begin of wa_wages,
           cocd       type char4,
           comp_name  type char32,
           pa         type char4,
           pa_txt     type char32,
           pyarea     type char16,
           pyarea_txt type char32,
           perpa      type char8,
           name       type char32,
           for_period type char16,
           pmt_dt     type char16,
           py_type    type char8,
           py_id      type char8,
           cgrp       type  char8,
           wt         type char8,
           wt_txt     type char32,
           no_of      type char16,
           amount     type char32,
           crcy       type char8,
         end of wa_wages,
         it_wages like standard table of wa_wages.

   refresh: seltab, list_tab, vlist, it_wages.
   "Persnal Number
   seltab_wa-selname = 'PNPPERNR'.
   seltab_wa-kind = 'P'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = wa_out-pernr.
   append seltab_wa to seltab.
   clear seltab_wa.

   "Begin Date
   seltab_wa-selname = 'BEGD_CAL'.
   seltab_wa-kind = 'P'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = v_begda.
   append seltab_wa to seltab.
   clear seltab_wa.

   "End Date
   seltab_wa-selname = 'ENDD_CAL'.
   seltab_wa-kind = 'P'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = v_endda.
   append seltab_wa to seltab.
   clear seltab_wa.

   "PF_EE_CONTR
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '/3F1'.
   seltab_wa-high   = '0'.
   append seltab_wa to seltab.
   clear seltab_wa.

   "Bonus
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '1110'.
   seltab_wa-high   = '0'.
   append seltab_wa to seltab.
   clear seltab_wa.

   "Leave Encashment
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '1315'.
   seltab_wa-high   = '0'.
   append seltab_wa to seltab.
   clear seltab_wa.

   "PL
   seltab_wa-selname = 'S_LGART'.
   seltab_wa-kind = 'S'.
   seltab_wa-sign    = 'I'.
   seltab_wa-option  = 'EQ'.
   seltab_wa-low    = '1312'.
   seltab_wa-high   = '0'.
   append seltab_wa to seltab.
   clear seltab_wa.

   submit h99cwtr0
     with selection-table seltab
     exporting list to memory
     and return.

* From memory transfer the program output into internal table through below FM :

   call function 'LIST_FROM_MEMORY'
     tables
       listobject = list_tab.

   if sy-subrc <> 0.
     message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   endif.

* Convert a (Saved) List Object to ASCI by using below FM.
   if list_tab[] is not initial.

     call function 'LIST_TO_ASCI'
       exporting
         list_index = -1
       tables
         listasci   = vlist
         listobject = list_tab.

     if sy-subrc <> '0'.
       write:/ 'LIST_TO_ASCI error !! ', sy-subrc.
     else.

     endif.
   endif.

   loop at vlist where not line cs '-----' .
     split vlist-line at '|' into  blank
                                   wa_wages-cocd
                                   wa_wages-comp_name
                                   wa_wages-pa
                                   wa_wages-pa_txt
                                   wa_wages-pyarea
                                   wa_wages-pyarea_txt
                                   wa_wages-perpa
                                   wa_wages-name
                                   wa_wages-for_period
                                   wa_wages-pmt_dt
                                   wa_wages-py_type
                                   wa_wages-py_id
                                   wa_wages-cgrp
                                   wa_wages-wt
                                   wa_wages-wt_txt
                                   wa_wages-no_of
                                   wa_wages-amount
                                   wa_wages-crcy.
     replace all occurrences of ',' in wa_wages-amount with ''.
     condense : wa_wages-cocd,
               wa_wages-comp_name,
               wa_wages-pa,
               wa_wages-pa_txt,
               wa_wages-pyarea,
               wa_wages-pyarea_txt,
               wa_wages-perpa,
               wa_wages-name,
               wa_wages-for_period,
               wa_wages-pmt_dt,
               wa_wages-py_type,
               wa_wages-py_id,
               wa_wages-cgrp,
               wa_wages-wt,
               wa_wages-wt_txt,
               wa_wages-no_of,
               wa_wages-amount,
               wa_wages-crcy.

     append wa_wages to it_wages.
     clear wa_wages.
   endloop.
   delete it_wages index 1.

   loop at it_wages into wa_wages.
     case wa_wages-wt.
       when '/3F1'.
         wa_out-eepfcont = wa_out-eepfcont + wa_wages-amount.
       when '1110'.
         wa_out-bonus = wa_out-bonus + wa_wages-amount.
       when '1315'.
         wa_out-leave = wa_out-leave + wa_wages-amount.
       when '1312'.
         wa_out-pl = wa_out-pl + wa_wages-amount.
     endcase.
   endloop.

   CLEAR: wa_wages.
   refresh: seltab, list_tab, vlist, it_wages.
endform.                    " GET_PF
*&---------------------------------------------------------------------*
*&      Form  CHK_AUTH_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_AUTH_OBJ .

 AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                     ID 'BUKRS' FIELD P_BUKRS
                     ID 'ACTVT' FIELD '03'.
 IF SY-SUBRC NE 0.
   MESSAGE 'Company Code not Authorized' TYPE 'I' DISPLAY LIKE 'E'.
   LEAVE LIST-PROCESSING.
 ENDIF.

 TYPES: BEGIN OF TY_T549A,
   ABKRS TYPE ABKRS,
   END OF TY_T549A.
DATA: WA_T549A TYPE TY_T549A, I_T549A TYPE STANDARD TABLE OF TY_T549A.

SELECT ABKRS
  FROM T549A
  INTO TABLE I_T549A
  WHERE ABKRS IN S_ABKRS.

  CLEAR: GV_AUTH_ABKRS_FLG, S_ABKRS.
  REFRESH: S_ABKRS[].
 IF I_T549A IS NOT INITIAL.
   LOOP AT I_T549A INTO WA_T549A.
     AUTHORITY-CHECK OBJECT 'P_PCR'
                         ID 'ABRKS' FIELD WA_T549A-ABKRS         " the field name is ABRKS in SU21 -> P_PCR object ( :-) SAP's does make mistakes ! )
                         ID 'ACTVT' FIELD '03'.
     IF SY-SUBRC EQ 0.
       S_ABKRS-SIGN = 'I'.
       S_ABKRS-OPTION = 'EQ'.
       S_ABKRS-LOW = WA_T549A-ABKRS.
       APPEND S_ABKRS.
     ELSE.
       IF GV_AUTH_ABKRS_FLG IS INITIAL.
         GV_AUTH_ABKRS_FLG = 'X'.
       ENDIF.
     ENDIF.
     CLEAR WA_T549A.
   ENDLOOP.
 ENDIF.

 IF S_ABKRS[] IS INITIAL.
   S_ABKRS-SIGN = 'I'.
   S_ABKRS-OPTION = 'EQ'.
   S_ABKRS-LOW = ''.
   APPEND S_ABKRS.
 ENDIF.
ENDFORM.                    " CHK_AUTH_OBJ
*&---------------------------------------------------------------------*
*&      Form  PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PAI_OF_SELECTION_SCREEN .
PERFORM INITIALIZE_VARIANT.
ENDFORM.                    " PAI_OF_SELECTION_SCREEN
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
G_VARIANT-REPORT = REPID.
G_VARIANT-VARIANT = P_VARI.
GX_VARIANT = G_VARIANT.

CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
 EXPORTING
   I_SAVE              = G_SAVE
  CHANGING
    cs_variant          = GX_VARIANT
 EXCEPTIONS
*   WRONG_INPUT         = 1
   NOT_FOUND           = 2
*   PROGRAM_ERROR       = 3
*   OTHERS              = 4
          .
IF sy-subrc EQ 0.
  P_VARI = GX_VARIANT-VARIANT.
  G_VARIANT = GX_VARIANT.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDFORM.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FOR_VARIANT .
CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
  EXPORTING
    is_variant                = G_VARIANT
*   I_TABNAME_HEADER          =
*   I_TABNAME_ITEM            =
*   IT_DEFAULT_FIELDCAT       =
   I_SAVE                    = G_SAVE
*   I_DISPLAY_VIA_GRID        = ' '
 IMPORTING
   E_EXIT                    = G_EXIT
   ES_VARIANT                = GX_VARIANT
 EXCEPTIONS
   NOT_FOUND                 = 1
*   PROGRAM_ERROR             = 2
*   OTHERS                    = 3
          .
IF sy-subrc EQ 2.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ELSE.
  IF G_EXIT = SPACE.
     P_VARI = GX_VARIANT-VARIANT.
  ENDIF.
ENDIF.

ENDFORM.                    " F4_FOR_VARIANT
