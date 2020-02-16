*&---------------------------------------------------------------------*
*& REPORT  Z6MM004R_WEIGHBRIGE_INTERFACE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:WEIGHBRIDGE INTEGRATION
* OBJECT TYPE       :                 FUNC. CONSULTANT  :
*          DEVELOPER:   SUPRIYA BHATNAGAR
*      CREATION DATE:   02.08.2010
*        DEV REQUEST:   IRDK900751
*  TCODE            :
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

REPORT  z6mm004r_weighbrige_interface.

TABLES : mkpf,
         mseg.

DATA :BEGIN OF imkpf_wa,
      mblnr TYPE mkpf-mblnr,
      mjahr TYPE mkpf-mjahr,
      bldat TYPE mkpf-bldat,
      blart TYPE mkpf-blart,
      budat TYPE mkpf-budat,
      cpudt TYPE mkpf-cpudt,
      END OF  imkpf_wa.

DATA :BEGIN OF imseg_wa,
      mblnr TYPE mseg-mblnr,
      mjahr TYPE mseg-mjahr,
      zeile TYPE mseg-zeile,
      bwart TYPE mseg-bwart,
      matnr TYPE mseg-matnr,
      werks TYPE mseg-werks,
      menge TYPE mseg-menge,
      meins TYPE mseg-meins,
      sjahr TYPE mseg-sjahr,
      smbln TYPE mseg-smbln,
      END OF imseg_wa.

DATA : BEGIN OF ifinal_wa,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        zeile TYPE mseg-zeile,
        bwart TYPE mseg-bwart,
        matnr TYPE mseg-matnr,
        werks TYPE mseg-werks,
        menge TYPE mseg-menge,
        meins TYPE mseg-meins,
        sjahr TYPE mseg-sjahr,
        smbln TYPE mseg-smbln,
        indi ,
        labor TYPE mara-labor,
        gaten TYPE z6mma_gt_ent_hd-gaten,
        vehno  TYPE z6mma_gt_ent_hd-vehno,
        lrnum  TYPE z6mma_gt_ent_hd-lrnum,
        lrdat  TYPE z6mma_gt_ent_hd-lrdat,
        redat  TYPE z6mma_gt_ent_hd-redat,
        gtdat  TYPE z6mma_gt_ent_hd-gtdat,
        are1n  TYPE z6mma_gt_ent_hd-are1n,
        aredt  TYPE z6mma_gt_ent_hd-aredt,
        gr_wt  TYPE z6mma_gt_ent_hd-gr_wt,
        tr_wt  TYPE z6mma_gt_ent_hd-tr_wt,
        net_wt  TYPE z6mma_gt_ent_hd-net_wt,
        sel,
        saved ,
       END OF ifinal_wa.

DATA : BEGIN OF iz6mma_gt_ent_hd_wa,
       mblnr  TYPE z6mma_gt_ent_hd-mblnr,
       mjahr  TYPE z6mma_gt_ent_hd-mjahr,
       vehno  TYPE z6mma_gt_ent_hd-vehno,
       lrnum  TYPE z6mma_gt_ent_hd-lrnum,
       lrdat  TYPE z6mma_gt_ent_hd-lrdat,
       gaten  TYPE z6mma_gt_ent_hd-gaten,
       redat  TYPE z6mma_gt_ent_hd-redat,
       gtdat  TYPE z6mma_gt_ent_hd-gtdat,
       are1n  TYPE z6mma_gt_ent_hd-are1n,
       aredt  TYPE z6mma_gt_ent_hd-aredt,
       saved,
       END OF iz6mma_gt_ent_hd_wa.

DATA :BEGIN OF iupload_wa,
      text TYPE string,
      END OF iupload_wa.

DATA :BEGIN OF iupload1_wa,
  text1  TYPE string,
  text2  TYPE string,
  text3  TYPE string,
  text4  TYPE string,
  text5  TYPE string,
  text6  TYPE string,
  text7  TYPE string,
  text8  TYPE string,
  text9  TYPE string,
  text10 TYPE string,
  text11 TYPE string,
  text12 TYPE string,
  text13 TYPE string,
  text14 TYPE string,
  text15 TYPE string,
  text16 TYPE string,
  text17 TYPE string,
END OF iupload1_wa.

DATA : iupdate_wa TYPE z6mma_gt_ent_hd.

DATA : imkpf LIKE STANDARD TABLE OF imkpf_wa,
       imseg LIKE STANDARD TABLE OF imseg_wa,
       ifinal LIKE STANDARD TABLE OF ifinal_wa,
       iz6mma_gt_ent_hd LIKE STANDARD TABLE OF iz6mma_gt_ent_hd_wa,
       iupload LIKE STANDARD TABLE OF iupload_wa,
       iupload1 LIKE STANDARD TABLE OF iupload1_wa,
       iupdate  LIKE STANDARD TABLE OF iupdate_wa.

DATA :  v_filename TYPE string,
        wa_gate_enno(14) TYPE c.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
PARAMETERS: p_cpudt TYPE mkpf-cpudt,
            p_werks TYPE mseg-werks,
            p_unlod TYPE mkpf-cpudt.

PARAMETERS : p_rad1 RADIOBUTTON GROUP rad,
             p_rad2 RADIOBUTTON GROUP rad.
SELECTION-SCREEN END   OF BLOCK b1 .

*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF p_rad2 = 'X'.
    IF p_unlod IS INITIAL.
      MESSAGE 'Please provide unloading Date' TYPE 'E'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM data_gather.
  PERFORM gate_entry_no.
*&---------------------------------------------------------------------*
*&      FORM  DATA_GATHER
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM data_gather .


  SELECT mblnr
         mjahr
         bldat
         blart
         budat
         cpudt FROM mkpf INTO TABLE imkpf
       WHERE cpudt EQ p_cpudt.
  IF sy-subrc = 0.
    SORT imkpf BY mblnr mjahr.
  ENDIF.

  IF NOT imkpf IS INITIAL.
    SELECT mblnr
           mjahr
           zeile
           bwart
           matnr
           werks
           menge
           meins
           sjahr
           smbln FROM mseg INTO TABLE imseg
           FOR ALL ENTRIES IN imkpf
           WHERE mblnr = imkpf-mblnr
           AND   mjahr = imkpf-mjahr
           AND   werks = p_werks
           AND   bwart IN (101,102).
    IF sy-subrc = 0.
      SORT imseg BY smbln sjahr.
    ENDIF.
  ENDIF.

  ifinal[] = imseg[].
*
  LOOP AT ifinal INTO ifinal_wa.
    READ TABLE imseg INTO imseg_wa WITH KEY smbln = ifinal_wa-mblnr
                                            sjahr = ifinal_wa-mjahr
                                            BINARY SEARCH.
    IF sy-subrc = 0.
      ifinal_wa-indi = 'R'.
    ENDIF.
    CLEAR ifinal_wa-labor.
    SELECT SINGLE labor INTO ifinal_wa-labor FROM mara
      WHERE matnr = ifinal_wa-matnr.
    MODIFY ifinal FROM ifinal_wa TRANSPORTING indi labor.
  ENDLOOP.

  DELETE ifinal WHERE indi  = 'R'.
  DELETE ifinal WHERE bwart = '102'.
  DELETE ifinal WHERE labor NE '012'.

  SORT ifinal BY mblnr mjahr.
  DELETE ADJACENT DUPLICATES FROM ifinal COMPARING mblnr mjahr.

ENDFORM.                    " DATA_GATHER
*&---------------------------------------------------------------------*
*&      FORM  GATE_ENTRY_NO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM gate_entry_no .

  IF NOT ifinal IS INITIAL.
    SELECT  mblnr
            mjahr
            vehno
            lrnum
            lrdat
            gaten
            redat
            gtdat
            are1n
            aredt
            saved FROM z6mma_gt_ent_hd INTO TABLE iz6mma_gt_ent_hd
            FOR ALL ENTRIES IN ifinal
            WHERE mblnr   = ifinal-mblnr
            AND   mjahr   = ifinal-mjahr.
    IF sy-subrc = 0.
      SORT iz6mma_gt_ent_hd BY mblnr mjahr.
    ENDIF.
  ENDIF.

*  delete IZ6MMA_GT_ENT_HD where saved = 'X'.
  CLEAR: ifinal_wa.
  LOOP AT ifinal INTO ifinal_wa.
    CLEAR iz6mma_gt_ent_hd_wa.
    READ TABLE iz6mma_gt_ent_hd INTO iz6mma_gt_ent_hd_wa
    WITH KEY mblnr = ifinal_wa-mblnr
             mjahr = ifinal_wa-mjahr
             BINARY SEARCH.
    IF sy-subrc = 0.
      ifinal_wa-gaten = iz6mma_gt_ent_hd_wa-gaten.
      ifinal_wa-vehno = iz6mma_gt_ent_hd_wa-vehno.
      ifinal_wa-lrnum = iz6mma_gt_ent_hd_wa-lrnum.
      ifinal_wa-lrdat = iz6mma_gt_ent_hd_wa-lrdat.
      ifinal_wa-redat = iz6mma_gt_ent_hd_wa-redat.
      ifinal_wa-gtdat = iz6mma_gt_ent_hd_wa-gtdat.
      ifinal_wa-are1n = iz6mma_gt_ent_hd_wa-are1n.
      ifinal_wa-aredt = iz6mma_gt_ent_hd_wa-aredt.
      ifinal_wa-saved = iz6mma_gt_ent_hd_wa-saved.
    ENDIF.

    MODIFY ifinal FROM ifinal_wa TRANSPORTING gaten
                                              vehno
                                              lrnum
                                              lrdat
                                              redat
                                              gtdat
                                              are1n
                                              aredt
                                              saved.
  ENDLOOP.

  DELETE ifinal WHERE saved = 'X'.
  IF NOT ifinal IS INITIAL.
    CALL SCREEN 2000.
  ELSE.
    MESSAGE 'No data' TYPE 'S'.
  ENDIF.

ENDFORM.                    " GATE_ENTRY_NO

*&SPWIZARD: DECLARATION OF TABLECONTROL 'WB_INT_DATA' ITSELF
CONTROLS: wb_int_data TYPE TABLEVIEW USING SCREEN 2000.

*&SPWIZARD: LINES OF TABLECONTROL 'WB_INT_DATA'
DATA:     g_wb_int_data_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TC 'WB_INT_DATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE wb_int_data_change_tc_attr OUTPUT.
  DESCRIBE TABLE ifinal LINES wb_int_data-lines.
ENDMODULE.                    "WB_INT_DATA_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'WB_INT_DATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE wb_int_data_get_lines OUTPUT.
  g_wb_int_data_lines = sy-loopc.
ENDMODULE.                    "WB_INT_DATA_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'WB_INT_DATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE wb_int_data_mark INPUT.
  DATA: g_wb_int_data_wa2 LIKE LINE OF ifinal.
  IF wb_int_data-line_sel_mode = 1
  AND ifinal_wa-sel = 'X'.
    LOOP AT ifinal INTO g_wb_int_data_wa2
      WHERE sel = 'X'.
      g_wb_int_data_wa2-sel = ''.
      MODIFY ifinal
        FROM g_wb_int_data_wa2
        TRANSPORTING sel.
    ENDLOOP.
  ENDIF.
  MODIFY ifinal
    FROM ifinal_wa
    INDEX wb_int_data-current_line
    TRANSPORTING sel.
ENDMODULE.                    "WB_INT_DATA_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'WB_INT_DATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE wb_int_data_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'WB_INT_DATA'
                              'IFINAL'
                              'SEL'
                     CHANGING ok_code.
  PERFORM user_comm.
  sy-ucomm = ok_code.
ENDMODULE.                    "WB_INT_DATA_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok              TYPE sy-ucomm,
        l_offset          TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = STRLEN( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
         EXPORTING
              entry_act             = <tc>-top_line
              entry_from            = 1
              entry_to              = <tc>-lines
              last_page_full        = 'X'
              loops                 = <lines>
              ok_code               = p_ok
              overlapping           = 'X'
         IMPORTING
              entry_new             = l_tc_new_top_line
         EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
              OTHERS                = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Form  USER_COMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_comm .

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'UPDT'.

      READ TABLE ifinal INTO ifinal_wa WITH KEY sel = 'X'.
      IF sy-subrc = 0.
        SELECT SINGLE path INTO v_filename FROM z6mma_params
          WHERE progname = 'Z6MM004R_WEIGHBRIGE_INTERFACE'
          and param1 = P_WERKS.
        IF sy-subrc NE 0.
          v_filename = 'C:\Documents and Settings\Administrator\Desktop'.
        ENDIF.
        CONDENSE v_filename.

   if p_rad1 = 'X'.

        CONCATENATE
*        'C:\Documents and Settings\Administrator\Desktop'
          v_filename
          '\TR'
          p_cpudt+6(2)
          p_cpudt+4(2)
          p_cpudt+2(2)
          '.TXT'
          INTO v_filename.

    elseif p_rad2 = 'X'.
      CONCATENATE
          v_filename
          '\TR'
          p_unlod+6(2)
          p_unlod+4(2)
          p_unlod+2(2)
          '.TXT'
          INTO v_filename.

     endif.

*        V_FILENAME = 'C:\Documents and Settings\Administrator\Desktop\TR271109.TXT'.

        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            filename                = v_filename
            filetype                = 'DAT'
            has_field_separator     = 'X'
          TABLES
            data_tab                = iupload
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            OTHERS                  = 17.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        LOOP AT iupload INTO iupload_wa.
          REPLACE ALL OCCURRENCES OF '"' IN iupload_wa-text WITH '' .
          SPLIT iupload_wa-text AT ',' INTO iupload1_wa-text1
                                            iupload1_wa-text2
                                            iupload1_wa-text3
                                            iupload1_wa-text4
                                            iupload1_wa-text5
                                            iupload1_wa-text6
                                            iupload1_wa-text7
                                            iupload1_wa-text8
                                            iupload1_wa-text9
                                            iupload1_wa-text10
                                            iupload1_wa-text11
                                            iupload1_wa-text12
                                            iupload1_wa-text13
                                            iupload1_wa-text14
                                            iupload1_wa-text15
                                            iupload1_wa-text16
                                            iupload1_wa-text17 .

          APPEND   iupload1_wa TO iupload1.
        ENDLOOP.

        CLEAR : iupload1_wa,ifinal_wa .
*      ----

        LOOP AT  ifinal INTO  ifinal_wa WHERE sel = 'X' .
          CLEAR :iupdate_wa,wa_gate_enno.
          CONDENSE ifinal_wa-gaten.
          wa_gate_enno = ifinal_wa-gaten+4(14).
*          read table iupload1 into iupload1_wa with key text14 = ifinal_Wa-GATEN.
          READ TABLE iupload1 INTO iupload1_wa WITH KEY text14 = wa_gate_enno.
          IF sy-subrc = 0.
            ifinal_wa-tr_wt =  iupload1_wa-text5.
            ifinal_wa-gr_wt =  iupload1_wa-text6.
            ifinal_wa-net_wt =  iupload1_wa-text7.
            MOVE-CORRESPONDING ifinal_wa TO iupdate_wa.
            iupdate_wa-saved = 'X'.
            APPEND iupdate_wa TO iupdate.
          ENDIF.
          MODIFY ifinal FROM ifinal_wa TRANSPORTING gr_wt tr_wt net_wt.
        ENDLOOP.

        IF iupdate IS INITIAL.
          MESSAGE 'No matching data found' TYPE 'S'.
        ELSE.
          MESSAGE 'Data Updated' TYPE 'S'.
        ENDIF.

      ELSE.
        MESSAGE 'Please select atleast one item' TYPE 'S'.
      ENDIF.
*      -----
    WHEN 'SAVE'.
      IF NOT iupdate IS INITIAL.
        MODIFY z6mma_gt_ent_hd FROM TABLE iupdate.
        IF sy-subrc = 0.
          MESSAGE 'Data Saved' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE 'No data changed' TYPE 'S'.
      ENDIF.

  ENDCASE.

ENDFORM.                    " USER_COMM
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'ZWBINTG'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_2000  OUTPUT
