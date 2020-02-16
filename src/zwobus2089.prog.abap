*****           Implementation of object type ZBUS2089             *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    employeenumber LIKE pskey-pernr,
    tripnumber     LIKE bapitrip-tripno,
  END OF key,
  secondlvlmanager  TYPE zapprover-agent OCCURS 0,
  employee_us       TYPE zapprover-agent,
  second_lvl_email  TYPE pa0105-usrid_long,
  firstlevelmanager TYPE pa0105-usrid_long,
  no_of_days(255),
  _pa0105           LIKE pa0105.
end_data object. " Do not change.. DATA is generated
*{insert TEDK913674
* global data
TABLES : pa0105.
DATA: t_1001   TYPE TABLE OF p1001,
      wa_1001  TYPE p1001,
      t_bdc    TYPE TABLE OF bdcdata,
      lv_schem TYPE ptrv_head-schem,
      lv_usrid TYPE sy-uname,
      lv_sbmod TYPE p0001-sbmod,
      wa_bdc   TYPE bdcdata.
CONSTANTS: c_p    TYPE otype VALUE 'P',
           c_s    TYPE otype VALUE 'S',
           c_us   TYPE otype VALUE 'US',
           c_p_s  TYPE subtyp VALUE 'B008', "pernr to position relation
           c_s_o  TYPE subtyp VALUE 'A003', "position to orgunit relatn
           c_o_o  TYPE subtyp VALUE 'A002', "reproting org unit relation
           c_s_p  TYPE subtyp VALUE 'A008', "Position to pernr relation
           c_s_us TYPE subtyp VALUE 'A008', "Position to user id relatn
           c_o    TYPE otype VALUE 'O'.
DEFINE gmac_read_relations.
  CALL FUNCTION 'RH_READ_INFTY_1001'
    EXPORTING
      authority              = 'DISP'
*   WITH_STRU_AUTH         = 'X'
      plvar                  = '01'
      otype                  = &1
      objid                  = &2
      istat                  = '1'
*   EXTEND                 = 'X'
      subty                  = &3
      begda                  = sy-datum
      endda                  = sy-datum
*   CONDITION              = '00000'
*   SORT                   = 'X'
*   WITH_EV                = ' '
*   ADATA                  = 'X'
*   AUTH_SOBID             = ' '
    TABLES
      i1001                  = t_1001
*   OBJECTS                =
    EXCEPTIONS
      nothing_found          = 1
      wrong_condition        = 2
      wrong_parameters       = 3
      OTHERS                 = 4
            .
  IF sy-subrc = 0.
    READ TABLE t_1001 INTO wa_1001 WITH KEY sclas = &4.
    IF sy-subrc = 0.
      &5 = wa_1001-sobid.
    ENDIF.
  ENDIF.

END-OF-DEFINITION.
*}insert TEDK913674
begin_method hr_trvl_administrator changing container.
DATA:
approver LIKE zapprover OCCURS 0.
DATA: subtype TYPE subty.

swc_get_element container 'Subtype' subtype.
*CALL FUNCTION 'ZF_FI_TRVLREQUEST_ACC_APPROVER'
*  EXPORTING
*    I_PERNR      = OBJECT-KEY-EMPLOYEENUMBER
*    I_SUBTY      = SUBTYPE
*  TABLES
*    ACC_APPROVER = APPROVER
*  EXCEPTIONS
*    NOBODY_FOUND = 1001
*    OTHERS       = 2.
*CASE SY-SUBRC.
*  WHEN 0. "OK
*  WHEN 1001.
*    EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  WHEN OTHERS.
*ENDCASE.
DATA: wa_uname TYPE swp_agent,
      lv_orgeh TYPE p0001-orgeh.

SELECT SINGLE uname INTO wa_uname
FROM pa0105
WHERE pernr = object-key-employeenumber.
SELECT SINGLE schem INTO lv_schem FROM ptrv_head
              WHERE pernr = object-key-employeenumber
                AND reinr = object-key-tripnumber.
SELECT SINGLE orgeh sbmod FROM pa0001 INTO (lv_orgeh, lv_sbmod)
                    WHERE pernr = object-key-employeenumber
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
IF lv_schem EQ '01'.
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                      WHERE werks EQ lv_sbmod
                        AND sachx EQ 'CSA'.

ELSEIF lv_schem EQ '02' .
  IF ( lv_orgeh EQ '50000060' OR lv_orgeh EQ '50000070' OR lv_orgeh EQ '50000071'
    OR lv_orgeh EQ '50002784' OR lv_orgeh EQ '50002785' OR lv_orgeh EQ '50002786'
    OR lv_orgeh EQ '50002787' OR lv_orgeh EQ '50002788' OR lv_orgeh EQ '50002789' ). "Speciality & Performance Chemical Division
    SELECT SINGLE usrid FROM t526 INTO lv_usrid
                       WHERE werks EQ lv_sbmod
                         AND sachx EQ 'SPC'.
  ELSE.
    SELECT SINGLE usrid FROM t526 INTO lv_usrid
                       WHERE werks EQ lv_sbmod
                         AND sachx EQ 'AAA'.
  ENDIF.

ELSEIF lv_schem EQ 'PL' .
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                  WHERE werks EQ lv_sbmod
                    AND sachx EQ 'CSA'.
ENDIF.
DATA : wa_approver TYPE zapprover.
IF sy-subrc EQ 0.
*   WA_APPROVER = LV_USRID.
  CONCATENATE 'US' lv_usrid INTO wa_approver.
*  CALL FUNCTION 'ZF_HR_SWX_GET_MANAGER_DIFF_LVL'
*    EXPORTING
*      I_USERID     = WA_UNAME
*      I_LEVEL      = 2
*    TABLES
*      APPROVER     = APPROVER
*    EXCEPTIONS
*      NOBODY_FOUND = 1001
*      OTHERS       = 2.
*
*  CASE SY-SUBRC.
*    WHEN 0. "ok
*    WHEN 1001.
*      EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    WHEN OTHERS.
*  ENDCASE.
*     WA_APPROVER = 'IBMEP01'.
  APPEND wa_approver TO approver.
*     OBJECT-SECONDLVLMANAGER  = APPROVER.

  swc_set_table container 'Approver' approver.
ENDIF.

swc_set_table container 'Approver' approver.

end_method.

begin_method hr_administrator changing container.
DATA:
      approver TYPE zapprover-agent OCCURS 0.
*CALL FUNCTION 'ZF_FI_TRVLREQUEST_HR_ADMIN'
*  EXPORTING
*    I_PERNR      = OBJECT-KEY-EMPLOYEENUMBER
*  TABLES
*    HR_ADMIN     = APPROVER
*  EXCEPTIONS
*    NOBODY_FOUND = 1001
*    OTHERS       = 2.
*
*CASE SY-SUBRC.
*  WHEN 0. "ok
*  WHEN 1001.
*    EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  WHEN OTHERS.
*ENDCASE.
DATA: wa_uname TYPE swp_agent.
SELECT SINGLE uname INTO wa_uname
FROM pa0105
WHERE pernr = object-key-employeenumber.
SELECT SINGLE schem INTO lv_schem FROM ptrv_head
              WHERE pernr = object-key-employeenumber
                AND reinr = object-key-tripnumber.
SELECT SINGLE sbmod FROM pa0001 INTO lv_sbmod
                    WHERE pernr = object-key-employeenumber
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
IF lv_schem EQ '01'.
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                      WHERE werks EQ lv_sbmod
                        AND sachx EQ 'CSA'.

ELSEIF lv_schem EQ '02' .
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                      WHERE werks EQ lv_sbmod
                        AND sachx EQ 'AAA'.

ENDIF.
DATA : wa_approver TYPE zapprover.
IF sy-subrc EQ 0.
  CONCATENATE 'US' lv_usrid INTO wa_approver.
*  CALL FUNCTION 'ZF_HR_SWX_GET_MANAGER_DIFF_LVL'
*    EXPORTING
*      I_USERID     = WA_UNAME
*      I_LEVEL      = 2
*    TABLES
*      APPROVER     = APPROVER
*    EXCEPTIONS
*      NOBODY_FOUND = 1001
*      OTHERS       = 2.
*
*  CASE SY-SUBRC.
*    WHEN 0. "ok
*    WHEN 1001.
*      EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    WHEN OTHERS.
*  ENDCASE.
*     WA_APPROVER = 'IBMEP01'.
  APPEND wa_approver TO approver.
*     OBJECT-SECONDLVLMANAGER  = APPROVER.

  swc_set_table container 'Approver' approver.
ENDIF.

swc_set_table container 'Approver' approver.
end_method.

begin_method hr_secondlvlmanager changing container.
DATA:
     approver TYPE zapprover-agent OCCURS 0.

DATA: wa_uname TYPE swp_agent.
SELECT SINGLE uname INTO wa_uname
FROM pa0105
WHERE pernr = object-key-employeenumber.
SELECT SINGLE schem INTO lv_schem FROM ptrv_head
              WHERE pernr = object-key-employeenumber
                AND reinr = object-key-tripnumber.
SELECT SINGLE sbmod FROM pa0001 INTO lv_sbmod
                    WHERE pernr = object-key-employeenumber
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
IF lv_schem EQ '01'.
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                      WHERE werks EQ lv_sbmod
                        AND sachx EQ 'CSA'.

ELSEIF lv_schem EQ '02' .
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                      WHERE werks EQ lv_sbmod
                        AND sachx EQ 'AAA'.

ENDIF.
DATA : wa_approver TYPE zapprover.
IF sy-subrc EQ 0.
  CONCATENATE 'US' lv_usrid INTO wa_approver.
*  CALL FUNCTION 'ZF_HR_SWX_GET_MANAGER_DIFF_LVL'
*    EXPORTING
*      I_USERID     = WA_UNAME
*      I_LEVEL      = 2
*    TABLES
*      APPROVER     = APPROVER
*    EXCEPTIONS
*      NOBODY_FOUND = 1001
*      OTHERS       = 2.
*
*  CASE SY-SUBRC.
*    WHEN 0. "ok
*    WHEN 1001.
*      EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    WHEN OTHERS.
*  ENDCASE.
*     WA_APPROVER = 'IBMEP01'.
  APPEND wa_approver TO approver.
*     OBJECT-SECONDLVLMANAGER  = APPROVER.

  swc_set_table container 'Approver' approver.
ENDIF.










end_method.

get_property secondlvlmanager changing container.
DATA:
     approver TYPE zapprover-agent OCCURS 0.

* code commented by Punam with referance to mail recevd from Neha sing IBM on dt: 18.02.2015
*DATA: wa_uname TYPE swp_agent.
*
*SELECT SINGLE uname INTO wa_uname
*FROM pa0105
*WHERE pernr = object-key-employeenumber.
DATA : wa_approver TYPE zapprover.
*IF sy-subrc EQ 0.
*  SELECT SINGLE schem INTO lv_schem FROM ptrv_head
*                WHERE pernr = object-key-employeenumber
*                  AND reinr = object-key-tripnumber.
*  SELECT SINGLE sbmod FROM pa0001 INTO lv_sbmod
*                      WHERE pernr = object-key-employeenumber
*                        AND begda LE sy-datum
*                        AND endda GE sy-datum.
*  IF lv_schem EQ '01'.
*    SELECT SINGLE usrid FROM t526 INTO lv_usrid
*                        WHERE werks EQ lv_sbmod
*                          AND sachx EQ 'CSA'.
*
*  ELSEIF lv_schem EQ '02' .
*    SELECT SINGLE usrid FROM t526 INTO lv_usrid
*                        WHERE werks EQ lv_sbmod
*                          AND sachx EQ 'AAA'.
*
*  ENDIF.
*BREAK 10106.
DATA: wa_uname TYPE swp_agent,
      lv_orgeh TYPE p0001-orgeh.
SELECT SINGLE uname INTO wa_uname
FROM pa0105
WHERE pernr = object-key-employeenumber.
SELECT SINGLE schem INTO lv_schem FROM ptrv_head
              WHERE pernr = object-key-employeenumber
              AND reinr = object-key-tripnumber.
SELECT SINGLE orgeh sbmod FROM pa0001 INTO (lv_orgeh , lv_sbmod )
           WHERE pernr = object-key-employeenumber
           AND begda LE sy-datum
           AND endda GE sy-datum.
IF lv_schem EQ '01' OR lv_schem EQ 'PL'.
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                     WHERE werks EQ lv_sbmod
                       AND sachx EQ 'CSA'.

ELSEIF lv_schem EQ '02'.
  IF ( lv_orgeh EQ '50000060' OR lv_orgeh EQ '50000070' OR lv_orgeh EQ '50000071' OR
       lv_orgeh EQ '50002784' OR lv_orgeh EQ '50002785' OR lv_orgeh EQ '50002786' OR
       lv_orgeh EQ '50002787' OR lv_orgeh EQ '50002788' OR lv_orgeh EQ '50002789' )." SPCD

    SELECT SINGLE usrid FROM t526 INTO lv_usrid
                    WHERE werks EQ lv_sbmod
                      AND sachx EQ 'SPC'.
  ELSE.

    SELECT SINGLE usrid FROM t526 INTO lv_usrid
                    WHERE werks EQ lv_sbmod
                      AND sachx EQ 'AAA'.

  ENDIF.
ENDIF.
**************************************** end code by Punam

**  CALL FUNCTION 'ZF_HR_SWX_GET_MANAGER_DIFF_LVL'
**    EXPORTING
**      I_USERID     = WA_UNAME
**      I_LEVEL      = 2
**    TABLES
**      APPROVER     = APPROVER
**    EXCEPTIONS
**      NOBODY_FOUND = 1001
**      OTHERS       = 2.
**
**  CASE SY-SUBRC.
**    WHEN 0.
**      OBJECT-SECONDLVLMANAGER = APPROVER.
**    WHEN 1001.
**      EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**    WHEN OTHERS.
**  ENDCASE.

IF lv_usrid IS NOT INITIAL.
  CONCATENATE 'US' lv_usrid INTO wa_approver.
  APPEND wa_approver TO approver.
  object-secondlvlmanager  = approver.
ENDIF.


swc_set_table container 'SecondlvlManager' object-secondlvlmanager.
end_property.
*{INSERT TEDK913674
begin_method getlocalcashier changing container.
DATA:
  lv_employee      TYPE persno,        "Employee pernr
  lv_objid         TYPE hrobjid,
  lv_ret_id        TYPE hrobjid,
  lv_cashier       TYPE swhactor,
  lv_error         TYPE c,              "Error flag
  lv_found_cashier TYPE c.      "flag to check if cashier found
CONSTANTS:
      lc_cashier TYPE subtyp VALUE 'BZ22'. "org unit to cashier relation
*Read employee pernr
swc_get_element container 'EmployeeNumber' lv_employee.
*Get Position from employee pernr
CLEAR lv_ret_id.
gmac_read_relations c_p lv_employee c_p_s c_s lv_ret_id.
IF lv_ret_id IS NOT INITIAL.
*read org unit of employee
  lv_objid = lv_ret_id.
  CLEAR lv_ret_id.
  gmac_read_relations c_s lv_objid c_s_o c_o lv_ret_id.
  IF lv_ret_id IS NOT INITIAL.
*loop till either cashier not found or error does not ocuur
    WHILE lv_found_cashier NE 'X' AND lv_error NE 'X'.
*get local cashier for org unit
      lv_objid = lv_ret_id.
      CLEAR lv_ret_id.
      gmac_read_relations c_o lv_objid lc_cashier c_s lv_ret_id.
      IF lv_ret_id IS NOT INITIAL.
        lv_found_cashier = 'X'.
      ELSE.
*get reporting org unit.
        gmac_read_relations c_o lv_objid c_o_o c_o lv_ret_id.
        IF lv_ret_id IS INITIAL.
          lv_error = 'X'.  "set error flag
        ENDIF.
      ENDIF.
    ENDWHILE.
    IF lv_error NE 'X'.       "If cashier found
*get pernr of cashier
      lv_objid = lv_ret_id.
      CLEAR lv_ret_id.
      gmac_read_relations c_s lv_objid c_s_p c_p lv_ret_id.
      IF lv_ret_id IS NOT INITIAL.
*get user id of cashier from pernr
        SELECT SINGLE usrid
                 FROM pa0105
                 INTO lv_cashier-objid
                WHERE pernr = lv_ret_id
                  AND subty = '0001'
                  AND endda GE sy-datum
                  AND begda LE sy-datum.
        IF sy-subrc = 0.
          lv_cashier-otype = c_us.
        ENDIF.
      ELSE.
*get user id directly from position
        gmac_read_relations c_s lv_objid c_s_p c_us lv_ret_id.
        IF lv_ret_id IS NOT INITIAL.
          lv_cashier-otype = c_us.
          lv_cashier-objid = lv_ret_id.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
swc_set_element container 'cashier' lv_cashier.
end_method.

begin_method gettraveldesk changing container.
DATA:
  lv_employee       TYPE persno,        "Employee pernr
  lv_objid          TYPE hrobjid,
  lv_ret_id         TYPE hrobjid,
  lv_traveldesk     TYPE swhactor,
  lv_error          TYPE c,              "Error flag
  lv_found_trvldesk TYPE c.      "flag to check if travel desk found
CONSTANTS:
      lc_trvldesk TYPE subtyp VALUE 'BZ14'."org unit to trvl desk relatn
*Read employee no
swc_get_element container 'EmployeeNumber' lv_employee.
*Get Position from employee pernr
CLEAR lv_ret_id.
gmac_read_relations c_p lv_employee c_p_s c_s lv_ret_id.
IF lv_ret_id IS NOT INITIAL.
*read org unit of employee
  lv_objid = lv_ret_id.
  CLEAR lv_ret_id.
  gmac_read_relations c_s lv_objid c_s_o c_o lv_ret_id.
  IF lv_ret_id IS NOT INITIAL.
*loop till either travel desk not found or error does not ocuur
    WHILE lv_found_trvldesk NE 'X' AND lv_error NE 'X'.
*get local cashier for org unit
      lv_objid = lv_ret_id.
      CLEAR lv_ret_id.
      gmac_read_relations c_o lv_objid lc_trvldesk c_s lv_ret_id.
      IF lv_ret_id IS NOT INITIAL.
        lv_found_trvldesk = 'X'.
      ELSE.
*get reporting org unit.
        gmac_read_relations c_o lv_objid c_o_o c_o lv_ret_id.
        IF lv_ret_id IS INITIAL.
          lv_error = 'X'.  "set error flag
        ENDIF.
      ENDIF.
    ENDWHILE.
    IF lv_error NE 'X'.       "If travel desk found
*get pernr of travel desk
      lv_objid = lv_ret_id.
      CLEAR lv_ret_id.
      gmac_read_relations c_s lv_objid c_s_p c_p lv_ret_id.
      IF lv_ret_id IS NOT INITIAL.
*get user id of travel desk from pernr
        SELECT SINGLE usrid
                 FROM pa0105
                 INTO lv_traveldesk-objid
                WHERE pernr = lv_ret_id
                  AND subty = '0001'
                  AND endda GE sy-datum
                  AND begda LE sy-datum.
        IF sy-subrc = 0.
          lv_traveldesk-otype = c_us.
        ENDIF.
      ELSE.
*get user id directly from position
        gmac_read_relations c_s lv_objid c_s_p c_us lv_ret_id.
        IF lv_ret_id IS NOT INITIAL.
          lv_traveldesk-otype = c_us.
          lv_traveldesk-objid = lv_ret_id.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

swc_set_element container 'TravelDesk' lv_traveldesk.
end_method.

begin_method sendrequestpdf changing container.
DATA:
  lv_receiver    TYPE swhactor,
  lv_pdf         TYPE fpcontent,
  lv_lines       TYPE i,
  lt_return      TYPE bapirettab,
  lt_receiver    TYPE TABLE OF somlreci1,
  wa_receiver    TYPE somlreci1,
  ls_doc_data    TYPE sodocchgi1,
  lt_contents    TYPE TABLE OF solisti1,
  wa_contents    TYPE solisti1,
  lt_pack_list   TYPE TABLE OF sopcklsti1,
  wa_pack_list   TYPE sopcklsti1,
  lt_attach      TYPE TABLE OF solisti1,
  wa_attach      TYPE solisti1,
  lt_lines       TYPE TABLE OF tline,
  wa_lines       TYPE tline,
  lv_traveller   TYPE emnam,
  lv_ret_id      TYPE hrobjid,
  lv_designation TYPE hrp1000-stext.
*get pdf data of travel request
CALL FUNCTION 'PTRM_WEB_FORM_PDF_GET'
  EXPORTING
    i_employeenumber = object-key-employeenumber
    i_tripnumber     = object-key-tripnumber
*   I_PERIODNUMBER   = '000'
    i_trip_component = 'R'
*   I_TRIP_DATA_SOURCE       = 'DB'
*   I_DISPLAY_FORM   = ' '
*   I_LANGUAGE       = SY-LANGU
  IMPORTING
    e_pdf_form       = lv_pdf
  TABLES
    et_return        = lt_return.
IF lv_pdf IS NOT INITIAL.
*convert raw string pdf data to binary data to send as pdf attachment.
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_pdf
*     APPEND_TO_TABLE       = ' '
* IMPORTING
*     OUTPUT_LENGTH         =
    TABLES
      binary_tab = lt_attach.
*Get Traveller name
  swc_get_property self 'TRAVELERNAME' lv_traveller.
*Get request Creators designation
*read position from employee pernr
  gmac_read_relations c_p object-key-employeenumber c_p_s c_s lv_ret_id.
  IF lv_ret_id IS NOT INITIAL.
    SELECT SINGLE stext
             FROM hrp1000
             INTO lv_designation
            WHERE plvar = '01'
              AND otype = c_s
              AND objid = lv_ret_id
              AND istat = '1'
              AND begda LE sy-datum
              AND endda GE sy-datum.
  ENDIF.
*Populate receiver.
  swc_get_element container 'Receiver' lv_receiver.
*to SAP Inbox
  wa_receiver-receiver = lv_receiver-objid.
  wa_receiver-rec_type = 'B'.
  APPEND wa_receiver TO lt_receiver.
  CLEAR wa_receiver.
*Populate subject
  CONCATENATE 'Please arrange travel for request no.'
  object-key-tripnumber INTO ls_doc_data-obj_descr SEPARATED BY space.
*populate body text
  CALL FUNCTION 'READ_STDTEXT'
    EXPORTING
      id              = 'ST'
      language        = sy-langu
      name            = 'ZFITVTRAVELDESKCONTENTS'
*     USE_AUX_LANGUAGE       = ' '
*     USE_THRUCLIENT  = ' '
*     LOCAL_CAT       = ' '
*   IMPORTING
*     HEADER          =
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      not_found       = 4
      reference_check = 5
      OTHERS          = 6.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  LOOP AT lt_lines INTO wa_lines.
    REPLACE '&TRIP&' IN wa_lines-tdline WITH object-key-tripnumber.
    REPLACE '&TRAVELERNAME&' IN wa_lines-tdline WITH lv_traveller.
    REPLACE '&DESIGNATION&' IN wa_lines-tdline WITH lv_designation.
    wa_contents = wa_lines-tdline.
    APPEND wa_contents TO lt_contents.
    CLEAR wa_contents.
  ENDLOOP.
*populate packing list
  DESCRIBE TABLE lt_attach LINES lv_lines.
  wa_pack_list-doc_size = lv_lines * 255.
  wa_pack_list-transf_bin = 'X'.
  wa_pack_list-head_start = 1.
  wa_pack_list-head_num = 1.
  wa_pack_list-body_start = 1.
  wa_pack_list-body_num = lv_lines.
  wa_pack_list-doc_type = 'PDF'.
  wa_pack_list-obj_name = 'Attachment'.
*Attachment Name
  wa_pack_list-obj_descr = 'Travel Request Details'.
  APPEND wa_pack_list TO lt_pack_list.
  CLEAR wa_pack_list.
  ls_doc_data-obj_name = 'MailText'.
  DESCRIBE TABLE lt_contents LINES lv_lines.
  ls_doc_data-doc_size = lv_lines * 255.
  wa_pack_list-head_start = 1.
  wa_pack_list-head_num = 0.
  wa_pack_list-body_start = 1.
  wa_pack_list-body_num = lv_lines.
  wa_pack_list-doc_type = 'RAW'.
  APPEND wa_pack_list TO lt_pack_list.
  CLEAR wa_pack_list.
  SORT lt_pack_list BY transf_bin head_num.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = ls_doc_data
*     PUT_IN_OUTBOX              = ' '
      commit_work                = 'X'
* IMPORTING
*     SENT_TO_ALL                =
*     NEW_OBJECT_ID              =
    TABLES
      packing_list               = lt_pack_list
*     object_header              =
      contents_bin               = lt_attach
      contents_txt               = lt_contents
*     CONTENTS_HEX               =
*     OBJECT_PARA                =
*     OBJECT_PARB                =
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDIF.
end_method.

begin_method mailcashier changing container.
DATA: lt_advance     TYPE TABLE OF ptk08,
      wa_advance     TYPE ptk08,
      lv_receiver    TYPE swhactor,
      lv_lines       TYPE i,
      lt_receiver    TYPE TABLE OF somlreci1,
      wa_receiver    TYPE somlreci1,
      ls_doc_data    TYPE sodocchgi1,
      lt_contents    TYPE TABLE OF solisti1,
      wa_contents    TYPE solisti1,
      lv_contents    TYPE solisti1,
      lv_char        TYPE solisti1,
      lt_pack_list   TYPE TABLE OF sopcklsti1,
      wa_pack_list   TYPE sopcklsti1,
      lt_lines       TYPE TABLE OF tline,
      wa_lines       TYPE tline,
      lv_traveller   TYPE emnam,
      lv_begindate   TYPE rebed,
      lv_begda_char  TYPE char10,
      lv_enddate     TYPE reend,
      lv_endda_char  TYPE char10,
      lv_reason      TYPE rkunde,
      lv_location    TYPE rzielort,
      lv_destination TYPE text_rgion,
      lv_ret_id      TYPE hrobjid,
      lv_designation TYPE hrp1000-stext.
*get Advance payment data
SELECT vorsc
     waers
     kursv
     ffact
     tfact
     vorhw
     kassa
     datvs
     paycurr
FROM ftpt_req_advance
INTO TABLE lt_advance
WHERE pernr = object-key-employeenumber
 AND reinr = object-key-tripnumber
 AND plan_request = 'R'.
IF sy-subrc = 0.
*get attribut values
  swc_get_property self 'TRAVELERNAME' lv_traveller.
  swc_get_property self 'BEGINDATE' lv_begindate.
  WRITE lv_begindate TO lv_begda_char.
  swc_get_property self 'ENDDATE' lv_enddate.
  WRITE lv_enddate TO lv_endda_char.
  swc_get_property self 'REASON' lv_reason.
  swc_get_property self 'LOCATION' lv_location.
  swc_get_property self 'DESTINATION' lv_destination.
*Get Designation Of Request Creator
*read position from employee pernr
  gmac_read_relations c_p object-key-employeenumber c_p_s c_s lv_ret_id.
  IF lv_ret_id IS NOT INITIAL.
    SELECT SINGLE stext
             FROM hrp1000
             INTO lv_designation
            WHERE plvar = '01'
              AND otype = c_s
              AND objid = lv_ret_id
              AND istat = '1'
              AND begda LE sy-datum
              AND endda GE sy-datum.
  ENDIF.
*Populate receiver.
  swc_get_element container 'Receiver' lv_receiver.
*to SAP inbox
  wa_receiver-receiver = lv_receiver-objid.
  wa_receiver-rec_type = 'B'.
  APPEND wa_receiver TO lt_receiver.
  CLEAR wa_receiver.
*Populate subject
  CONCATENATE 'Please pay advance for trip no.'
  object-key-tripnumber INTO ls_doc_data-obj_descr SEPARATED BY space.
*populate body text
  CALL FUNCTION 'READ_STDTEXT'
    EXPORTING
      id              = 'ST'
      language        = sy-langu
      name            = 'ZFITVCASHIERCONTENTS'
*     USE_AUX_LANGUAGE       = ' '
*     USE_THRUCLIENT  = ' '
*     LOCAL_CAT       = ' '
*   IMPORTING
*     HEADER          =
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      not_found       = 4
      reference_check = 5
      OTHERS          = 6.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  LOOP AT lt_lines INTO wa_lines.
    IF wa_lines-tdline CS '&AMOUNT&'.
      CLEAR lv_contents.
      LOOP AT lt_advance INTO wa_advance.
        IF lv_contents IS INITIAL.
          lv_contents = wa_advance-vorsc.
          CONDENSE lv_contents.
        ELSE.
          lv_char = wa_advance-vorsc.
          CONDENSE lv_char.
          CONCATENATE lv_contents lv_char INTO lv_contents
          SEPARATED BY ';'.
          CLEAR lv_char.
        ENDIF.
      ENDLOOP.
      REPLACE '&AMOUNT&' IN wa_lines-tdline WITH lv_contents.
    ELSEIF wa_lines-tdline CS '&DATE&'.
      CLEAR lv_contents.
      LOOP AT lt_advance INTO wa_advance.
        IF lv_contents IS INITIAL.
          WRITE wa_advance-datvs TO lv_contents.
          CONDENSE lv_contents.
        ELSE.
          WRITE wa_advance-datvs TO lv_char.
          CONDENSE lv_char.
          CONCATENATE lv_contents lv_char INTO lv_contents
          SEPARATED BY ';'.
          CLEAR lv_char.
        ENDIF.
      ENDLOOP.
      REPLACE '&DATE&' IN wa_lines-tdline WITH lv_contents.
    ELSEIF wa_lines-tdline CS '&CASH&'.
      CLEAR lv_contents.
      LOOP AT lt_advance INTO wa_advance.
        IF lv_contents IS INITIAL.
          IF wa_advance-kassa = 'X'.
            lv_contents = 'Yes'.
          ELSE.
            lv_contents = 'No'.
          ENDIF.
        ELSE.
          IF wa_advance-kassa = 'X'.
            CONCATENATE lv_contents 'Yes' INTO lv_contents
            SEPARATED BY ';'.
          ELSE.
            CONCATENATE lv_contents 'No' INTO lv_contents
            SEPARATED BY ';'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      REPLACE '&CASH&' IN wa_lines-tdline WITH lv_contents.
    ELSE.
      REPLACE '&EMPLOYEENUMBER&' IN wa_lines-tdline
      WITH object-key-employeenumber.
      REPLACE '&TRAVELERNAME&' IN wa_lines-tdline
      WITH lv_traveller.
      REPLACE '&BEGINDATE&' IN wa_lines-tdline
      WITH lv_begda_char.
      REPLACE '&ENDDATE&' IN wa_lines-tdline
      WITH lv_endda_char.
      REPLACE '&REASON&' IN wa_lines-tdline
      WITH lv_reason.
      REPLACE '&LOCATION&' IN wa_lines-tdline
      WITH lv_location.
      REPLACE '&DESTINATION&' IN wa_lines-tdline
      WITH lv_destination.
      REPLACE '&DESIGNATION&' IN wa_lines-tdline
      WITH lv_designation.
    ENDIF.
    wa_contents = wa_lines-tdline.
    APPEND wa_contents TO lt_contents.
    CLEAR wa_contents.
  ENDLOOP.
  ls_doc_data-obj_name = 'MailText'.
*populate packing list
  DESCRIBE TABLE lt_contents LINES lv_lines.
  ls_doc_data-doc_size = lv_lines * 255.
  wa_pack_list-head_start = 1.
  wa_pack_list-head_num = 0.
  wa_pack_list-body_start = 1.
  wa_pack_list-body_num = lv_lines.
  wa_pack_list-doc_type = 'RAW'.
  APPEND wa_pack_list TO lt_pack_list.
  CLEAR wa_pack_list.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = ls_doc_data
*     PUT_IN_OUTBOX              = ' '
      commit_work                = 'X'
*   IMPORTING
*     SENT_TO_ALL                =
*     NEW_OBJECT_ID              =
    TABLES
      packing_list               = lt_pack_list
*     object_header              =
*     CONTENTS_BIN               =
      contents_txt               = lt_contents
*     CONTENTS_HEX               =
*     OBJECT_PARA                =
*     OBJECT_PARB                =
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDIF.
end_method.

begin_method getaccountsofficer changing container.
DATA:
  lv_accounts       TYPE swhactor,
  lv_objid          TYPE hrobjid,
  lv_ret_id         TYPE hrobjid,
  lv_error          TYPE c,              "Error flag
  lv_found_accounts TYPE c.      "flag to check if acc officer found
CONSTANTS:
      lc_account TYPE subtyp VALUE 'BZ21'. "org unit to acc offcr relatn
*Get Position from employee pernr
CLEAR lv_ret_id.
gmac_read_relations c_p object-key-employeenumber c_p_s c_s lv_ret_id.
IF lv_ret_id IS NOT INITIAL.
*read org unit of employee
  lv_objid = lv_ret_id.
  CLEAR lv_ret_id.
  gmac_read_relations c_s lv_objid c_s_o c_o lv_ret_id.
  IF lv_ret_id IS NOT INITIAL.
*loop till either accounts officer not found or error does not ocuur
    WHILE lv_found_accounts NE 'X' AND lv_error NE 'X'.
*get local accounts officer for org unit
      lv_objid = lv_ret_id.
      CLEAR lv_ret_id.
      gmac_read_relations c_o lv_objid lc_account c_s lv_ret_id.
      IF lv_ret_id IS NOT INITIAL.
        lv_found_accounts = 'X'.
      ELSE.
*get reporting org unit.
        gmac_read_relations c_o lv_objid c_o_o c_o lv_ret_id.
        IF lv_ret_id IS INITIAL.
          lv_error = 'X'.  "set error flag
        ENDIF.
      ENDIF.
    ENDWHILE.
    IF lv_error NE 'X'.       "If accounts officer found
*get pernr of accounts officer
      lv_objid = lv_ret_id.
      CLEAR lv_ret_id.
      gmac_read_relations c_s lv_objid c_s_p c_p lv_ret_id.
      IF lv_ret_id IS NOT INITIAL.
*get user id of accounts officer from pernr
        SELECT SINGLE usrid
                 FROM pa0105
                 INTO lv_accounts-objid
                WHERE pernr = lv_ret_id
                  AND subty = '0001'
                  AND endda GE sy-datum
                  AND begda LE sy-datum.
        IF sy-subrc = 0.
          lv_accounts-otype = c_us.
        ENDIF.
      ELSE.
*get user id directly from position
        gmac_read_relations c_s lv_objid c_s_p c_us lv_ret_id.
        IF lv_ret_id IS NOT INITIAL.
          lv_accounts-otype = c_us.
          lv_accounts-objid = lv_ret_id.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
swc_set_element container 'AccountsOfficer' lv_accounts.
end_method.

begin_method sendtrippdf changing container.
DATA:
  lv_receiver    TYPE swhactor,
  lv_pdf         TYPE fpcontent,
  lv_lines       TYPE i,
  lt_return      TYPE bapirettab,
  lt_receiver    TYPE TABLE OF somlreci1,
  wa_receiver    TYPE somlreci1,
  ls_doc_data    TYPE sodocchgi1,
  lt_contents    TYPE TABLE OF solisti1,
  wa_contents    TYPE solisti1,
  lt_pack_list   TYPE TABLE OF sopcklsti1,
  wa_pack_list   TYPE sopcklsti1,
  lt_attach      TYPE TABLE OF solisti1,
  wa_attach      TYPE solisti1,
  lt_lines       TYPE TABLE OF tline,
  wa_lines       TYPE tline,
  lv_traveller   TYPE emnam,
  lv_ret_id      TYPE hrobjid,
  lv_designation TYPE hrp1000-stext.
*get pdf data of trip
CALL FUNCTION 'PTRM_WEB_FORM_PDF_GET'
  EXPORTING
    i_employeenumber = object-key-employeenumber
    i_tripnumber     = object-key-tripnumber
*   I_PERIODNUMBER   = '000'
*   I_TRIP_COMPONENT = ' '
*   I_TRIP_DATA_SOURCE       = 'DB'
*   I_DISPLAY_FORM   = ' '
*   I_LANGUAGE       = SY-LANGU
  IMPORTING
    e_pdf_form       = lv_pdf
  TABLES
    et_return        = lt_return.
IF lv_pdf IS NOT INITIAL.
*convert raw string pdf data to binary data to send as pdf attachment.
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_pdf
*     APPEND_TO_TABLE       = ' '
* IMPORTING
*     OUTPUT_LENGTH         =
    TABLES
      binary_tab = lt_attach.
*Get Traveller name
  swc_get_property self 'TRAVELERNAME' lv_traveller.
*Get request Creators designation
*read position from employee pernr
  gmac_read_relations c_p object-key-employeenumber c_p_s c_s lv_ret_id.
  IF lv_ret_id IS NOT INITIAL.
    SELECT SINGLE stext
             FROM hrp1000
             INTO lv_designation
            WHERE plvar = '01'
              AND otype = c_s
              AND objid = lv_ret_id
              AND istat = '1'
              AND begda LE sy-datum
              AND endda GE sy-datum.
  ENDIF.
*Populate receiver.
  swc_get_element container 'Receiver' lv_receiver.
*to SAP Inbox
  wa_receiver-receiver = lv_receiver-objid.
  wa_receiver-rec_type = 'B'.
  APPEND wa_receiver TO lt_receiver.
  CLEAR wa_receiver.
*Populate subject
  CONCATENATE 'Travel exps. of pernr' object-key-employeenumber
  'for trip' object-key-tripnumber INTO ls_doc_data-obj_descr
  SEPARATED BY space.
*populate body text
  CALL FUNCTION 'READ_STDTEXT'
    EXPORTING
      id              = 'ST'
      language        = sy-langu
      name            = 'ZFITVACCOUNTSCONTENTS'
*     USE_AUX_LANGUAGE       = ' '
*     USE_THRUCLIENT  = ' '
*     LOCAL_CAT       = ' '
*   IMPORTING
*     HEADER          =
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      not_found       = 4
      reference_check = 5
      OTHERS          = 6.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  LOOP AT lt_lines INTO wa_lines.
    REPLACE '&TRIP&' IN wa_lines-tdline WITH object-key-tripnumber.
    REPLACE '&EMPLOYEENUMBER&' IN wa_lines-tdline
    WITH object-key-employeenumber.
    REPLACE '&TRAVELERNAME&' IN wa_lines-tdline WITH lv_traveller.
    REPLACE '&DESIGNATION&' IN wa_lines-tdline WITH lv_designation.
    wa_contents = wa_lines-tdline.
    APPEND wa_contents TO lt_contents.
    CLEAR wa_contents.
  ENDLOOP.
*populate packing list
  DESCRIBE TABLE lt_attach LINES lv_lines.
  wa_pack_list-doc_size = lv_lines * 255.
  wa_pack_list-transf_bin = 'X'.
  wa_pack_list-head_start = 1.
  wa_pack_list-head_num = 1.
  wa_pack_list-body_start = 1.
  wa_pack_list-body_num = lv_lines.
  wa_pack_list-doc_type = 'PDF'.
  wa_pack_list-obj_name = 'Attachment'.
*Attachment Name
  wa_pack_list-obj_descr = 'Trip Details'.
  APPEND wa_pack_list TO lt_pack_list.
  CLEAR wa_pack_list.
  ls_doc_data-obj_name = 'MailText'.
  DESCRIBE TABLE lt_contents LINES lv_lines.
  ls_doc_data-doc_size = lv_lines * 255.
  wa_pack_list-head_start = 1.
  wa_pack_list-head_num = 0.
  wa_pack_list-body_start = 1.
  wa_pack_list-body_num = lv_lines.
  wa_pack_list-doc_type = 'RAW'.
  APPEND wa_pack_list TO lt_pack_list.
  CLEAR wa_pack_list.
  SORT lt_pack_list BY transf_bin head_num.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = ls_doc_data
*     PUT_IN_OUTBOX              = ' '
      commit_work                = 'X'
* IMPORTING
*     SENT_TO_ALL                =
*     NEW_OBJECT_ID              =
    TABLES
      packing_list               = lt_pack_list
*     object_header              =
      contents_bin               = lt_attach
      contents_txt               = lt_contents
*     CONTENTS_HEX               =
*     OBJECT_PARA                =
*     OBJECT_PARB                =
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDIF.
end_method.

begin_method changetravelrequest changing container.
DATA:
  ls_options TYPE ctu_params,
  lt_bdc     TYPE TABLE OF bdcdata.
* Set Parameter values to call tp04 for required travel request

"PERFORM BDC_DYNPRO USING 'SAPMP56T' '0003'.                 "MAWK036209
"PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'AENDERN'.

SET PARAMETER ID 'PER' FIELD object-key-employeenumber.
SET PARAMETER ID 'TNO' FIELD object-key-tripnumber.
"SET PARAMETER ID 'TRIPCHANGEINTERACTIV' FIELD 'X'.
SET PARAMETER ID 'TTC' FIELD 'ANTRAGAEN'.
ls_options-dismode  = 'A'.           "Display mode errors only
ls_options-updmode  = 'A'.           "Update mode asynchronous
ls_options-cattmode = space.
ls_options-defsize  = space.
ls_options-racommit = 'X'.
ls_options-nobinpt  = 'X'.
ls_options-nobiend  = 'X'.
*Call tcode to edit travel request
CALL TRANSACTION 'TP04_EWT'
USING t_bdc "blank but required to use option exit screen on commit
OPTIONS FROM ls_options.
end_method.

begin_method getmanager changing container.
DATA:
  actortab    LIKE swhactor OCCURS 0,
  lv_pernr    TYPE persno,
  accontainer LIKE swcont OCCURS 0.
swc_get_element container 'EmployeeNumber' lv_pernr.
swc_set_element accontainer 'OTYPE' c_p.
swc_set_element accontainer 'OBJID' lv_pernr.

CALL FUNCTION 'SWX_GET_MANAGER'
  TABLES
    actor_tab    = actortab
    ac_container = accontainer
  EXCEPTIONS
    nobody_found = 9001
    OTHERS       = 01.
CASE sy-subrc.
  WHEN 0.            " OK
  WHEN 9001.         " NOBODY_FOUND
    exit_return 9001 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  WHEN OTHERS.       " to be implemented
ENDCASE.
swc_set_table container 'ActorTab' actortab.
end_method.

begin_method changetrip changing container.
DATA:
  ls_options TYPE ctu_params,
  lt_bdc     TYPE TABLE OF bdcdata.
* Set Parameter values to call tp04 for required travel request

PERFORM bdc_dynpro USING 'SAPMP56T' '0003'.                 "MAWK036209
PERFORM bdc_field  USING 'BDC_OKCODE' 'AENDERN'.

SET PARAMETER ID 'PER' FIELD object-key-employeenumber.
SET PARAMETER ID 'TNO' FIELD object-key-tripnumber.
SET PARAMETER ID 'TRIPCHANGEINTERACTIV' FIELD 'X'.
"SET PARAMETER ID 'TTC' FIELD 'ANTRAGAEN'.
ls_options-dismode  = 'E'.           "Display mode errors only
ls_options-updmode  = 'A'.           "Update mode asynchronous
ls_options-cattmode = space.
ls_options-defsize  = space.
ls_options-racommit = 'X'.
ls_options-nobinpt  = 'X'.
ls_options-nobiend  = 'X'.
*Call tcode to edit travel request
CALL TRANSACTION 'TRIP_EWT'
USING t_bdc
OPTIONS FROM ls_options.
end_method.
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRV_PROG(SAPMP56T)  text
*      -->P_STARTING_SCREEN(1000)  text
*----------------------------------------------------------------------*
FORM  bdc_dynpro USING program TYPE bdcdata-program
                       dynpro  TYPE bdcdata-dynpro.
  CLEAR wa_bdc.
  wa_bdc-program  = program.
  wa_bdc-dynpro   = dynpro.
  wa_bdc-dynbegin = 'X'.
  APPEND wa_bdc TO t_bdc.
ENDFORM.                    "bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2506   text
*      -->P_L_PERSID  text
*----------------------------------------------------------------------*
FORM  bdc_field USING fnam TYPE bdcdata-fnam
                      fval TYPE any.
  CLEAR wa_bdc.
  wa_bdc-fnam = fnam.
  wa_bdc-fval = fval.
  APPEND wa_bdc TO t_bdc.
ENDFORM.                    " bdc_field

begin_method getdata changing container.
DATA:
  objectkey      TYPE swcont-value,
  employeenumber TYPE pa0105-pernr,
  employeename   TYPE pa0001-ename,
  tripnumber     TYPE bapitrip-tripno.
swc_get_element container 'ObjectKey' objectkey.
employeenumber = objectkey(8).
tripnumber = objectkey+8(10).
swc_set_element container 'EmployeeNumber' employeenumber.
swc_set_element container 'TripNumber' tripnumber.
*Employee name
SELECT SINGLE ename
         FROM pa0001
         INTO employeename
        WHERE pernr = employeenumber
          AND endda GE sy-datum
          AND begda LE sy-datum.
IF sy-subrc = 0.
  swc_set_element container 'EmployeeName' employeename.
ENDIF.
end_method.
*}Insert

begin_method displaytrip changing container.
DATA: lv_status TYPE ptrv_perio-antrg,
      lt_return TYPE bapirettab,
      lv_type   TYPE plan_request.
swc_get_property self 'Approval' lv_status.
IF lv_status = '1' OR lv_status = '2'.
  lv_type = 'R'.  "Travel Request
ELSE.
  lv_type = ' '.
ENDIF.
CALL FUNCTION 'PTRM_WEB_FORM_PDF_GET'
  EXPORTING
    i_employeenumber = object-key-employeenumber
    i_tripnumber     = object-key-tripnumber
*   I_PERIODNUMBER   = '000'
    i_trip_component = lv_type
*   I_TRIP_DATA_SOURCE       = 'DB'
    i_display_form   = 'X'
*   I_LANGUAGE       = SY-LANGU
*   IMPORTING
*   E_PDF_FORM       =
  TABLES
    et_return        = lt_return.
end_method.


*
get_table_property pa0105.
DATA subrc LIKE sy-subrc.
* Fill TABLES PA0105 to enable Object Manager Access to Table Properties
PERFORM select_table_pa0105 USING subrc.
IF subrc NE 0.
  exit_object_not_found.
ENDIF.
end_property.
*
* Use Form also for other(virtual) Properties to fill TABLES PA0105
FORM select_table_pa0105 USING subrc LIKE sy-subrc.
* Select single * from PA0105, if OBJECT-_PA0105 is initial
  IF object-_pa0105-mandt IS INITIAL
  AND object-_pa0105-pernr IS INITIAL
  AND object-_pa0105-subty IS INITIAL
  AND object-_pa0105-objps IS INITIAL
  AND object-_pa0105-sprps IS INITIAL
  AND object-_pa0105-endda IS INITIAL
  AND object-_pa0105-begda IS INITIAL
  AND object-_pa0105-seqnr IS INITIAL.
    SELECT SINGLE * FROM pa0105 CLIENT SPECIFIED
        WHERE mandt = sy-mandt
        AND pernr = object-key-employeenumber
        AND subty = '0010'.
*        AND OBJPS = ??????????
*        AND SPRPS = ??????????
*        AND ENDDA = ??????????
*        AND BEGDA = ??????????
*        AND SEQNR = ??????????.
    subrc = sy-subrc.
    IF subrc NE 0. EXIT. ENDIF.
*     CONCATENATE 'US' OBJECT-_PA0105-USRID_LONG
*     INTO OBJECT-_PA0105-USRID.
    object-_pa0105 = pa0105.
  ELSE.
    subrc = 0.
    pa0105 = object-_pa0105.
*    CONCATENATE 'US' OBJECT-_PA0105-USRID_LONG
*                                   INTO OBJECT-_PA0105-USRID.
    pa0105 = object-_pa0105.
  ENDIF.
ENDFORM.                    "SELECT_TABLE_PA0105

get_property second_lvl_email changing container.
DATA: wa_uname TYPE swp_agent,
      lv_orgeh TYPE p0001-orgeh.
SELECT SINGLE uname INTO wa_uname
FROM pa0105
WHERE pernr = object-key-employeenumber.
SELECT SINGLE schem INTO lv_schem FROM ptrv_head
              WHERE pernr = object-key-employeenumber
                AND reinr = object-key-tripnumber.
SELECT SINGLE orgeh sbmod FROM pa0001 INTO (lv_orgeh, lv_sbmod)
                    WHERE pernr = object-key-employeenumber
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
IF lv_schem EQ '01' OR lv_schem EQ 'PL'.
  SELECT SINGLE usrid FROM t526 INTO lv_usrid
                      WHERE werks EQ lv_sbmod
                        AND sachx EQ 'CSA'.

ELSEIF lv_schem EQ '02' .
  IF ( lv_orgeh EQ '50000060' OR lv_orgeh EQ '50000070' OR lv_orgeh EQ '50000071'
    OR lv_orgeh EQ '50002784' OR lv_orgeh EQ '50002785' OR lv_orgeh EQ '50002786'
    OR lv_orgeh EQ '50002787' OR lv_orgeh EQ '50002788' OR lv_orgeh EQ '50002789' ). "Speciality & Performance Chemical Division
    SELECT SINGLE usrid FROM t526 INTO lv_usrid
                       WHERE werks EQ lv_sbmod
                         AND sachx EQ 'SPC'.
  ELSE.
    SELECT SINGLE usrid FROM t526 INTO lv_usrid
                       WHERE werks EQ lv_sbmod
                         AND sachx EQ 'AAA'.
  ENDIF.
ENDIF.
DATA : wa_approver TYPE zapprover.
IF sy-subrc EQ 0.
*   WA_APPROVER = LV_USRID.
*   CONCATENATE 'US' LV_USRID INTO WA_APPROVER.
*  CALL FUNCTION 'ZF_HR_SWX_GET_MANAGER_DIFF_LVL'
*    EXPORTING
*      I_USERID     = WA_UNAME
*      I_LEVEL      = 2
*    TABLES
*      APPROVER     = APPROVER
*    EXCEPTIONS
*      NOBODY_FOUND = 1001
*      OTHERS       = 2.
*
*  CASE SY-SUBRC.
*    WHEN 0. "ok
*    WHEN 1001.
*      EXIT_RETURN 1001 SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    WHEN OTHERS.
*  ENDCASE.
*     WA_APPROVER = 'IBMEP01'.
  DATA : lv_pernr TYPE pernr_d.
  SELECT SINGLE pernr FROM pa0105 INTO lv_pernr
                         WHERE usrty = '0001'
                          AND usrid = lv_usrid
                          AND begda LE sy-datum
                          AND endda GE sy-datum.
  SELECT SINGLE usrid_long  FROM pa0105 INTO
                                            object-second_lvl_email
                         WHERE pernr EQ lv_pernr
                           AND subty EQ '0010'
                           AND begda LE sy-datum
                           AND endda GE sy-datum.
ENDIF.

swc_set_element container 'Second_Lvl_Email' object-second_lvl_email.
end_property.

get_property employee_us changing container.
SELECT SINGLE usrid  FROM pa0105 INTO
                                          object-employee_us
                       WHERE pernr EQ object-key-employeenumber
                         AND subty EQ '0001'
                         AND begda LE sy-datum
                         AND endda GE sy-datum.
CONCATENATE 'US' object-employee_us INTO object-employee_us.
swc_set_element container 'Employee_US' object-employee_us.
end_property.

get_property firstlevelmanager changing container.
DATA:
  actortab    LIKE swhactor OCCURS 0,
  wa_actortab TYPE swhactor,
  lv_pernr    TYPE persno,
  accontainer LIKE swcont OCCURS 0.

swc_get_element container 'EmployeeNumber' lv_pernr.
lv_pernr = object-key-employeenumber.
swc_set_element accontainer 'OTYPE' c_p.
swc_set_element accontainer 'OBJID' lv_pernr.


CALL FUNCTION 'SWX_GET_MANAGER'
  TABLES
    actor_tab    = actortab
    ac_container = accontainer
  EXCEPTIONS
    nobody_found = 9001
    OTHERS       = 01.
CASE sy-subrc.
  WHEN 0.            " OK
    LOOP AT actortab INTO wa_actortab  WHERE otype = 'P'.
      SELECT SINGLE usrid_long  FROM pa0105 INTO
                                                object-firstlevelmanager
                             WHERE pernr EQ  wa_actortab-objid
                               AND subty EQ '0010'
                               AND begda LE sy-datum
                               AND endda GE sy-datum.
    ENDLOOP.

  WHEN 9001.         " NOBODY_FOUND
    exit_return 9001 sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  WHEN OTHERS.       " to be implemented
ENDCASE.


swc_set_element container 'FirstLevelManager'
     object-firstlevelmanager.
end_property.

get_property no_of_days changing container.
DATA : v_bgdat TYPE sy-datum.
SELECT SINGLE datv1 FROM ptrv_head INTO v_bgdat
               WHERE pernr EQ object-key-employeenumber
                 AND reinr EQ object-key-tripnumber.
object-no_of_days =  v_bgdat - sy-datum.
swc_set_element container 'No_of_Days' object-no_of_days.
end_property.

begin_method cancel_trip changing container.

DATA : return       TYPE bapireturn,
       periodnumber TYPE bapitrvxxx-period,
       new_app      TYPE ptrv_perio-antrg,
       new_acc      TYPE ptrv_perio-abrec.

swc_get_element container 'PERIODNUMBER' periodnumber.

CALL FUNCTION 'BAPI_TRIP_CANCEL'
  EXPORTING
    employeenumber = object-key-employeenumber
    tripnumber     = object-key-tripnumber
  IMPORTING
    return         = return
  EXCEPTIONS
    OTHERS         = 01.
CASE sy-subrc.
  WHEN 0.            " OK
  WHEN OTHERS.       " to be implemented
ENDCASE.

* Change status to Initial on trip rejection by accountant
new_app = '3'.  "Trip Completed
new_acc = '1'.  "To Be Settled

CALL FUNCTION 'BAPI_TRIP_CHANGE_STATUS'
  EXPORTING
    employeenumber = object-key-employeenumber
    tripnumber     = object-key-tripnumber
*   PERIODNUMBER   = '000'                      "XCIPSDETRG
    periodnumber   = periodnumber               "XCIPSDETRG
    approved_new   = new_app
    account_new    = new_acc
  IMPORTING
    return         = return
  EXCEPTIONS
    OTHERS         = 0.

swc_set_element container 'Return' return.
end_method.

begin_method wait_for_event_change changing container.

* Wait for 5 min
WAIT UP TO 30 SECONDS.

end_method.

begin_method checkclaimsubmit changing container.

DATA:
  tripbegindate TYPE ptrv_head-datv1,
  tripenddate   TYPE ptrv_head-datb1,
  lv_begindate  TYPE ptrv_head-datv1,
  lv_enddate    TYPE ptrv_head-datb1.

DATA :
  date1      TYPE p0001-begda,
  date2      TYPE p0001-begda,
  days       TYPE p0347-scrdd,
  app_count  TYPE pa0001-seqnr,
  lv_pernr   TYPE persno,
  next_level TYPE tojtb-active,
  lv_ergru   TYPE pa0017-ergru,
  lv_orgeh   TYPE pa0001-orgeh,
  lv_sobid   TYPE hrp1001-sobid.

DATA:
  it_days TYPE TABLE OF zhr_claim_days,
  wa_days TYPE zhr_claim_days.

swc_get_element container 'NEXT_LEVEL' next_level.
swc_get_element container 'TripBeginDate' tripbegindate.
swc_get_element container 'TripEndDate' tripenddate.

SELECT * INTO TABLE it_days FROM zhr_claim_days.

SELECT SINGLE datb1 INTO lv_enddate  FROM ptrv_head
              WHERE pernr = object-key-employeenumber
                AND reinr = object-key-tripnumber.

SELECT SINGLE ergru INTO lv_ergru FROM pa0017
       WHERE pernr = object-key-employeenumber
         AND endda >= sy-datum.

SELECT SINGLE orgeh INTO lv_orgeh FROM pa0001
       WHERE pernr = object-key-employeenumber
         AND endda >= sy-datum.

SELECT SINGLE sobid INTO lv_sobid FROM hrp1001
       WHERE objid = lv_orgeh
        AND  otype = 'O'
        AND  rsign = 'A'
        AND  relat = '002'
        AND endda >= sy-datum.

date1 = sy-datum.
date2 = lv_enddate.

CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
  EXPORTING
    date1                       = date1
    date2                       = date2
    output_format               = '03'
  IMPORTING
*   YEARS                       =
*   MONTHS                      =
    days                        = days
  EXCEPTIONS
    overflow_long_years_between = 1
    invalid_dates_specified     = 2
    OTHERS                      = 3.
IF sy-subrc <> 0.

ENDIF.

IF days > 15.
  IF lv_ergru = 1 AND lv_sobid = '50000095'.
    LOOP AT it_days INTO wa_days.
      IF days > wa_days-zdaysgrt AND days <= wa_days-zdaysless.
        app_count = wa_days-zlevelno.
        next_level = 'X'.
      ENDIF.
    ENDLOOP.
  ELSE.
    next_level = ''.
  ENDIF.
ELSE.
  next_level = ''.
ENDIF.

swc_set_element container 'NEXT_LEVEL' next_level.
swc_set_element container 'APP_COUNT' app_count.

end_method.

begin_method getnextapprover changing container.

TYPES: BEGIN OF ty_agent,
         wi_aagent TYPE swwwihead-wi_aagent,
         wi_cd     TYPE swwwihead-wi_cd,
         wi_ct     TYPE swwwihead-wi_ct,
       END OF ty_agent.

DATA : ls_ac_container TYPE swcont,
       lt_ac_container TYPE TABLE OF swcont,
       lt_approver     TYPE TABLE OF swhactor,
       ls_approver     TYPE swhactor,
       lv_next         TYPE swp_agent.

DATA:
      lv_agent TYPE usr02-bname.
DATA: next_agent TYPE usr02-bname.

DATA: it_agent TYPE TABLE OF ty_agent,
      wa_agent TYPE ty_agent.

*  CONCATENATE 'US' <User Id> INTO lv_creator.
swc_get_element container 'NEXT_AGENT' next_agent.
swc_get_element container 'ORG_OBJECT' lv_next.

IF next_agent IS INITIAL.
  MOVE lv_agent TO  next_agent.
  next_agent = lv_next.
ENDIF.

CLEAR ls_ac_container.
MOVE :   'ORG_OBJECT'  TO ls_ac_container-element ,
         '000000'      TO ls_ac_container-tab_index ,
         '014'         TO ls_ac_container-elemlength ,
         'C'           TO ls_ac_container-type ,
         next_agent    TO ls_ac_container-value .
APPEND ls_ac_container TO lt_ac_container.

REFRESH lt_approver.

CALL FUNCTION 'SWX_GET_MANAGER'
  TABLES
    actor_tab    = lt_approver
    ac_container = lt_ac_container
  EXCEPTIONS
    nobody_found = 1
    OTHERS       = 2.

CASE sy-subrc.
  WHEN 0.
    LOOP AT lt_approver INTO ls_approver  WHERE otype = 'US'.
      CONCATENATE ls_approver-otype ls_approver-objid INTO next_agent.
    ENDLOOP.
  WHEN 9001.
    next_agent = 'NOAGENT'.
  WHEN OTHERS.
ENDCASE.

swc_set_element container 'NEXT_AGENT' next_agent.
swc_set_element container 'ORG_OBJECT' next_agent.

end_method.
