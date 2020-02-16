*&---------------------------------------------------------------------*
*& Report  Z6HR001R_PAYSLIP_MAILER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6HR001R_PAYSLIP_MAILER.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Pay Slip Mailer Program
* OBJECT TYPE       : Report             FUNC. CONSULTANT  :Ram Manohar
*          DEVELOPER: Ramakrishna Konda
*      CREATION DATE: 14.06.2010
*        DEV REQUEST: IRDK900136
*              TCODE: ZHR001
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

************************************************************************
* Modification       : 1.0
* Author's name      : Kamalakar B
* Date written       : 22 jan 2008
* Change description : To use the Z table instead of text .
*
************************************************************************
************************************************************************
* Modification       : 1.1
* Author's name      : Patha Sridhar
* Date written       : 20 Oct 2008
* Change description : Addition of Content to Pay slip mail
* Request no         : ECDK906513  srch code:ECDK906513
************************************************************************
************************************************************************
* Modification       : 1.2
* Author's name      : Patha Sridhar
* Date written       : 29 Oct 2008
* Change description : Addition of Content to Pay slip mail
* Request no         : ECDK907186  srch code ECDK907186
************************************************************************
************************************************************************
* Modification       : 1.3
* Author's name      : Ravi Kumar
* Date written       : 17 March 2009
* Change description : Included the selection based on Payroll area
* Request no         : ECDK914202  srch code ECDK914202
************************************************************************
************************************************************************
* Modification       : 1.4
* Author's name      : Patha Sridhar
* Date written       : 26 March 2009
* Change description : Authorization Checks based on Payroll area
* Request no         :  ECDK914576  srch code  ECDK914576
************************************************************************
************************************************************************
* Modification       : 1.5
* Author's name      : Patha Sridhar
* Date written       : 30 March 2009
* Change description : Changes content
* Request no         :  ECDK914709  srch code  ECDK914709
************************************************************************
************************************************************************
* Modification       : 1.6
* Author's name      : Patha Sridhar
* Date written       : 30 April 2009
* Change description : Add content
* Request no         : ECDK916001  srch code ECDK916001
************************************************************************
************************************************************************
* Modification       : 1.7
* Author's name      : Ravi Kumar
* Date written       : 18 Aug 2009
* Change description : Different mail content added for
*                      payroll area = 06
* Request no         : ECDK921140 srch code ECDK921140
************************************************************************
************************************************************************
* Modification       : 1.8
* Author's name      : Prerna Gupta
* Date written       : 15 Sept 2009
* Change description : Changes for fetching the email id's
* from the ZPYT_MAILID_ICI table employees belonging to ICI.
* Request no         : ECDK922472 srch code ECDK922472
************************************************************************
************************************************************************
* Modification       : 1.7
* Author's name      : Patha Sridhar
* Date written       : 30 April 2009
* Change description : un comment content
* Request no         : ECDK927375
************************************************************************
TABLES: pa0000.
TABLES: tsp01.

DATA: objpack   LIKE sopcklsti1 OCCURS 2 WITH HEADER LINE.
DATA: objhead   LIKE solisti1 OCCURS 1 WITH HEADER LINE.
DATA: objbin    LIKE solisti1 OCCURS 10 WITH HEADER LINE.
DATA: objtxt    LIKE solisti1 OCCURS 10 WITH HEADER LINE.
DATA: reclist   LIKE somlreci1 OCCURS 5 WITH HEADER LINE.
DATA: objhex LIKE solix OCCURS 0 WITH HEADER LINE.
DATA: doc_chng  LIKE sodocchgi1.
DATA: tab_lines LIKE sy-tabix.
DATA : subject LIKE doc_chng-obj_descr.

DATA : sender LIKE somlreci1-receiver VALUE ''.
DATA : sender_type LIKE soextreci1-adr_typ.


DATA li_spoolno LIKE tsp01-rqident.
DATA lc_filename(110).
DATA lc_spoolid LIKE tsp01_sp0r-rqid_char.
DATA pdf LIKE tline OCCURS 100 WITH HEADER LINE.
DATA pdfspoolid LIKE tsp01-rqident.
DATA: numbytes TYPE i.

****modification 01****
DATA: lv_pytext(40) TYPE C.


**end of modification 01 ***

DATA:   bdcdata LIKE bdcdata  OCCURS 0 WITH HEADER LINE.
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: gd_buffer TYPE string.

types: begin of i_pernr,
      pernr type pernr_d,
      mailid type AD_SMTPADR,
      end of i_pernr.

data : it_pernr type table of i_pernr.

DATA: wa_pernr LIKE LINE OF it_pernr.
DATA: month(9),
      year TYPE n LENGTH 4.

DATA: msg TYPE string.

SELECT-OPTIONS: empno FOR pa0000-pernr.

*PARAMETERS: filepath like rlgrap-filename default 'C:\'.
PARAMETERS: pmonth TYPE n LENGTH 2.
PARAMETERS: pyear TYPE n LENGTH 4.
PARAMETERS: pform LIKE rpcedtx0-prt_form.
PARAMETERS p_persar LIKE qppnp-xabkr .

START-OF-SELECTION.

* Added By PAtha Sridhar on 26/03/2009 " ECDK914576
*  AUTHORITY-CHECK OBJECT 'ZPY_P_PCR'
*           ID 'ABRKS' FIELD p_persar.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZH' TYPE 'A' NUMBER 115 WITH p_persar.
*  ENDIF.
* Ended By PAtha Sridhar on 26/03/2009 " ECDK914576

****Modification-log-1.3(ECDK914202)*********************
*By-Ravi Kumar
*On-17/03/2009
*For-To include selection based on Payroll area
********************************************************************
********************************************************************
*************************ECDK922472 Start*******************************
*  IF p_persar+0(1) = 'P'.
*    SELECT * INTO TABLE it_pernr
*    FROM zpyt_mailid_bpo
*    WHERE empno IN empno
*    AND empno < '01000000'.
*  ELSE.
*    SELECT * INTO TABLE it_pernr
*    FROM zpy_mailid
*    WHERE empno IN empno
*    AND empno < '01000000'.
*  ENDIF.



*  IF P_PERSAR+0(1) = 'P'.
*    SELECT * INTO TABLE IT_PERNR
*    FROM ZPYT_MAILID_BPO
*    WHERE EMPNO IN EMPNO
*    AND EMPNO < '01000000'.
*  ELSEIF P_PERSAR+0(1) = '1'.
*    SELECT * INTO TABLE IT_PERNR
*      FROM ZPYT_MAILID_ICI
*      WHERE EMPNO IN EMPNO
*      AND EMPNO < '01000000'.
*  ELSE.
*    SELECT * INTO TABLE IT_PERNR
*    FROM ZPY_MAILID
*    WHERE EMPNO IN EMPNO
*    AND EMPNO < '01000000'.
*  ENDIF.
********************************************************************
********************************************************************
*************************ECDK922472 End*******************************
********************************************************************
  year = pyear.
  month = pmonth.

  IF month <= 9.
    month = month + 3.
  ENDIF.

  IF pmonth > 9.
    year = year + 1.
    month = pmonth - 9.
  ENDIF.



  SELECT SINGLE ltx INTO month FROM t247 WHERE spras = 'EN'
  AND mnr = month.

***modification 01***

*  SELECT SINGLE pytext INTO lv_pytext FROM zpyt_pytext.
*
*  IF sy-subrc <> 0.
*    MESSAGE e198(zf).
*  ENDIF.

***end of modification 01***

*select single usrid from pa0105 into sender
*where pernr = '44444'.

  PERFORM mail_content.

  LOOP AT it_pernr INTO wa_pernr.
*  IF sy-subrc <> 0.
**    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    continue.
*    clear wa_pernr.
*  ENDIF.
    PERFORM call_transaction USING wa_pernr-pernr.
    PERFORM pdf_download.
    IF NOT li_spoolno IS INITIAL.
      PERFORM send_mail.
      CONCATENATE 'Payslip sent to' wa_pernr-pernr INTO msg.
      MESSAGE msg  TYPE 'S'.
    ENDIF.
    CLEAR wa_pernr.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail .

  DATA: it_header LIKE STANDARD TABLE OF tline WITH HEADER LINE.


**modification-1.3 by Ravi Kumar on 18/03/2009
*  REFRESH objtxt. CLEAR objtxt.
***End of modification************************
  REFRESH objpack. CLEAR objpack.
  REFRESH objhead. CLEAR objhead.
  REFRESH reclist. CLEAR reclist.

  doc_chng-obj_name = 'Payslip'.
  PACK wa_pernr-pernr TO wa_pernr-pernr.
  CONCATENATE 'Emp # ' wa_pernr-pernr
            month+0(3) year ' - E -Payslip'
            INTO subject.
  doc_chng-obj_descr = subject.
*  MOVE subject TO objhead.
  APPEND objhead.

* Mail contents
*  CALL FUNCTION 'READ_TEXT'
*    EXPORTING
**     CLIENT                        = SY-MANDT
*      id                            = 'HR_G'
*      language                      = sy-langu
*      name                          = 'ZHR_PAYSLIP_HEADER'
*      object                        = 'TEXT'
**     ARCHIVE_HANDLE                = 0
**     LOCAL_CAT                     = ' '
**   IMPORTING
**     HEADER                        =
*    tables
*      lines                         = it_header
**   EXCEPTIONS
**     ID                            = 1
**     LANGUAGE                      = 2
**     NAME                          = 3
**     NOT_FOUND                     = 4
**     OBJECT                        = 5
**     REFERENCE_CHECK               = 6
**     WRONG_ACCESS_TO_ARCHIVE       = 7
**     OTHERS                        = 8
*            .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

**Modification-1.3 by Ravi Kumar on 18/03/2009(ECDK914202)*******
**Commented whole block for objtxt and took out of the loop
**in form 'mail_content'.
*  objtxt = '<html><head>'.
*  APPEND objtxt.
*  objtxt = '<style type="text/css">'.
*  APPEND objtxt.
*  objtxt =
*  '.content {font-family: Verdana, sans-serif;font-size: 11px;}'.
*  APPEND objtxt.
*  objtxt =
*'.orange {font-family: Verdana, sans-serif;font-size: 11px;color:#ff6' &
*'600}'
*  .
*  APPEND objtxt.
**  objtxt = name="verdana" size="11px">'.
**  APPEND objtxt.
*  objtxt = '</style></head>'.
*  APPEND objtxt.
*
*  objtxt = '<body>'.
*  APPEND objtxt.
*
*  objtxt = '<font class="content"><B>Dear Infoscion,</B><BR><BR>'.
*  APPEND objtxt.
*
*  CONCATENATE
*    'Please find attached your pay slip for the month of &nbsp;'
*    month ',&nbsp;' year '.' '<BR>'  INTO objtxt.
*  APPEND objtxt.
*
** commented content Patha Sridhar on 20/10/2008 ECDK906513
**  objtxt = '<BR>For all your salary and other related queries,'.
** Added content Patha Sridhar on 20/10/2008  ECDK906513
*  objtxt = '<BR>For all your salary related queries,'.
*  append objtxt.
** Endded ECDK906513
*    objtxt =
*  'please log in to ServCentralé using the following links.<BR>'.
*  APPEND objtxt.
*  CONCATENATE '<BR>Intranet users:'
*  '<A href="http://172.25.103.20/rtracker/aspx/default.aspx?page=NEW">'
*              INTO objtxt.
*  APPEND objtxt.
*   objtxt =
*  'http://172.25.103.20/rtracker/aspx/default.aspx?page=NEW</a>'.
*  APPEND objtxt.
*  CONCATENATE '<BR>Extranet users:'
*'<A href="https://xnet.infosys.com/RTRAcker/aspx/default.aspx?page=NE' &
*'W">'
*  INTO objtxt.
*  APPEND objtxt.
*  objtxt =
*  'https://xnet.infosys.com/RTRAcker/aspx/default.aspx?page=NEW</a><BR>'
*  .
*  APPEND objtxt.
** Added content Patha Sridhar on 20/10/2008 ECDK906513
*  objtxt =
*'<BR>Please raise a request in Servcentrale under the respective '.
*  append objtxt.
*
*  objtxt =
*'service groups for the following type of clarifications:.<BR>'.
*  append objtxt.
*
*  objtxt =
*   '<B><BR>Facilities:</B> Guest House, Hostel rent, Gym, Membership fees, MLPL charges,'.
*  append objtxt.
*  objtxt =
*'Telephone and Transport deductions.<BR>'.
*  append objtxt.
*
*  objtxt =
*'<B><BR>HRD (Compensation & Benefit):</B> Computation of Variable payouts like VCPI, IPI '.
*  append objtxt.
*
*  objtxt =
*' and Differential AVP, Compensation review and Salary revision.<BR>'.
*  append objtxt.
*
*  objtxt =
*'<B><BR>Corporate Accounting Group(Loans & CLA):</B> Vehicle loan allowance, Housing loan allowance '.
*  append objtxt.
*
*  objtxt =
*' and Soft loan, Salary advance, Salary loan, Third party loan deductions.<BR>'.
*  append objtxt.
*
** Ennded ECDK906513
*
*  objtxt =
*  '<BR>To understand and know more about your pay computation, '.
*  APPEND objtxt.
*
*  objtxt =
*  'you may go through the salary FAQs using the link below.<BR>'.
*  APPEND objtxt.
*
*  objtxt =
*'<a href=http://sparsh/V1/myUnit/Accounts/Accounts_Salaries_Salary.htm>'
*  .
*  APPEND objtxt.
*  objtxt =
*'http://sparsh/V1/myUnit/Accounts/Accounts_Salaries_Salary.htm</a><BR>'
*  .
*  APPEND objtxt.
*  objtxt =
*  '<BR><B>Note: We would like to inform that the salary for the'.
*****modification 01****
*  CONCATENATE objtxt ' current month will be credited on&nbsp;'
*  lv_pytext '.' '</B><BR>' INTO objtxt.
*  append objtxt.
****end of modification 01***
**  ECDK907186
**CONCATENATE objtxt '(the last working day of the month up to 12:00 Midnight)</B><BR>'
**              INTO objtxt.
**  APPEND objtxt.
*  objtxt = '<BR>Regards,<BR>'.
*  APPEND objtxt.
*  objtxt = '<B>Team - India Payroll </B><BR>'.
*  APPEND objtxt.
*  objtxt = 'Corporate Accounting Group<BR>'.
*  APPEND objtxt.
*  objtxt = 'Infosys Technologies Limited'.
*  APPEND objtxt.
*  objtxt = '</font>'.
*  APPEND objtxt.
*  objtxt = '</font></body> </html>'.
*  APPEND objtxt.
**End of modification-1.3******************************************

  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + STRLEN( objtxt ).


* Creation of the entry for the compressed document
*  objpack-transf_bin = 'X'.
  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num = 0.
  objpack-body_start = 1.
  objpack-body_num = tab_lines.
  objpack-doc_type = 'HTM'.
*  objpack-doc_type = 'PDF'.
  objpack-doc_size = tab_lines * 255.
*  objpack-obj_name = 'C:\00044444_2007_2008.PDF'.
*  objpack-doc_type = 'RAW'.
  APPEND objpack.
*  LOOP AT it_tab.
*    objbin-line = it_tab-string.
*    APPEND objbin.
*  ENDLOOP.

*  LOOP AT PDF.
*    objbin-line = PDF-tdformat.
*    append objbin.
*    objbin-line = PDF-tdline.
*  ENDLOOP.


  CLEAR objpack.
  DESCRIBE TABLE objbin LINES tab_lines.

  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num = 1.
  objpack-body_start = 1.
  objpack-body_num = tab_lines.
  objpack-doc_type = 'PDF'.
  objpack-doc_size = tab_lines * 255.
  CONCATENATE wa_pernr-pernr '_'
              month+0(3) '_' year INTO
              objpack-obj_descr.
*  objpack-obj_name = '00044444_2007_2008.PDF'.
*  objpack-obj_descr = '00044444_2007_2008.PDF'.
  APPEND objpack.

*  clear objpack.
*  DESCRIBE table objhex lines tab_lines.
**  clear objpack-transf_bin.
*  objpack-transf_bin = 'X'.
*  objpack-head_start = 1.
*  objpack-head_num = 2.
*  objpack-body_start = 1.
*  objpack-body_num = tab_lines.
*  objpack-doc_type = 'HEX'.
*  objpack-doc_size = tab_lines * 255.
**  concatenate wa_pernr-empno '_'
**              month+0(3) '_' pyear into
**              objpack-obj_descr.
**  objpack-obj_name = '00044444_2007_2008.PDF'.
**  objpack-obj_descr = '00044444_2007_2008.PDF'.
*  APPEND objpack.


* Completing the recipient list

  reclist-receiver = wa_pernr-mailid.
  reclist-rec_type = 'U'.
  reclist-notif_ndel = 'X'.
  APPEND reclist.

**Modification-1.3 by Ravi Kumar on 18/03/2009
  IF p_persar+0(1) = 'P'.
    sender = 'BPO_SALARY@infosys.com'.
  ELSEIF P_PERSAR+0(1) = '1'.
    sender = 'ICIPayroll@infosys.com'.
  ELSE.
    sender = 'CAG@infosys.com'.
  ENDIF.
***End of modification-1.3********************
*** If no sender specified - default blank
**  IF sender EQ space.
**    sender_type = space.
**  ELSE.
  sender_type = 'INT'.
**  ENDIF.
*
  IF NOT reclist[] IS INITIAL.
    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        put_in_outbox              = ''
        sender_address             = sender
        sender_address_type        = sender_type
        commit_work                = 'X'
      TABLES
        packing_list               = objpack
*        object_header              = objhead
        contents_txt               =  objtxt
        contents_bin               = objbin
        receivers                  = reclist
*        contents_hex               = objhex
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.

  ENDIF.
  CASE sy-subrc.
    WHEN 0.
      LOOP AT reclist.
        IF reclist-retrn_code = 0.
          MESSAGE s174(zf).
*   Document was sent successfully
        ELSE.
          MESSAGE e175(zf).
*   The document could not be sent
        ENDIF.
      ENDLOOP.
    WHEN 1.
      MESSAGE e176(zf).
*   No authorization for sending to the recipients
    WHEN 2.
      MESSAGE e177(zf).
*   Document could not be sent to the recipient
    WHEN 4.
      MESSAGE e178(zf).
*   No send authorization
    WHEN OTHERS.
      MESSAGE e179(zf).
*   Error occurred while sending

  ENDCASE.

ENDFORM.                    " send_mail


*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text Calling the Print program to Print the File
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_transaction USING pernr.
  CLEAR bdcdata.
  REFRESH bdcdata.
  CLEAR messtab.
  REFRESH messtab.

  PERFORM bdc_dynpro      USING 'HINCEDT0' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PRT_PROT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM bdc_field       USING 'PNPXABKR'
                                p_persar.
  PERFORM bdc_field       USING 'PNPTIMR9'
                                ''.
  PERFORM bdc_field       USING 'PNPTIMRA'
                                'X'.
  PERFORM bdc_field       USING 'PNPPABRP'
                                 pmonth.
  PERFORM bdc_field       USING 'PNPPABRJ'
                                pyear.
  PERFORM bdc_field       USING 'PNPPERNR-LOW'
                                pernr.
  PERFORM bdc_field       USING 'FORMULAR'
                                pform.
  PERFORM bdc_field       USING 'ANDRUCK'
                                'A'.
  PERFORM bdc_field       USING 'RUECKD'
                                ' '.
  PERFORM bdc_field       USING 'RUECKR'
                                'D'.
  PERFORM bdc_field       USING 'SORT_RR'
                                '1'.
  PERFORM bdc_field       USING 'SPRACHE'
                                'B'.
  PERFORM bdc_field       USING 'PRT_PROT'
                                ''.
  PERFORM bdc_field       USING 'CUR_FP'
                                'X'.
  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PZ 24'.
  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PRI'.
  PERFORM bdc_dynpro      USING 'SAPLSPRI' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PRIN'.
  PERFORM bdc_field       USING 'PRI_PARAMS-PDEST'
                                'LOCL'.
  PERFORM bdc_field       USING 'PRI_PARAMS-PRCOP'
                                '1'.
  PERFORM bdc_field       USING 'RADIO0500_1'
                                'X'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'PRIPAR_DYN-PRIMM2'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PRIPAR_EXT-ORIENT'.
  PERFORM bdc_field       USING 'PRIPAR_EXT-ORIENT'
                                'L'.
  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BACK'.
  PERFORM bdc_dynpro      USING 'HINCEDT0' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/EE'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PNPXABKR'.

  CALL TRANSACTION 'PC00_M40_CEDT' USING bdcdata MODE 'N' MESSAGES
  INTO messtab.

*
*  IF sy-subrc <> 0.
*    err_int = 1.
*  ENDIF.
*  li_spoolno = sy-msgv1.

ENDFORM.                    " CALL_TRANSACTION


*&---------------------------------------------------------------------*
*&      Form  PDF_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pdf_download.


  DATA: lc_rq2name LIKE tsp01-rq2name.
  DATA: msg TYPE string.

  REFRESH objbin.
  CLEAR li_spoolno.
  CONCATENATE 'HINCEDT0_'  sy-uname+0(3)  INTO lc_rq2name.
  SELECT * FROM tsp01
           WHERE rq2name = lc_rq2name
           AND rqowner = sy-uname
           ORDER BY rqcretime DESCENDING.
    li_spoolno = tsp01-rqident.
    EXIT.
  ENDSELECT.

  IF NOT li_spoolno IS INITIAL.
    CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid                    = li_spoolno
          no_dialog                      = ' '
*         DST_DEVICE                     =
*         PDF_DESTINATION                =
        IMPORTING
          pdf_bytecount                  = numbytes
          pdf_spoolid                    = pdfspoolid
*         LIST_PAGECOUNT                 =
*          btc_jobname                    = jobname
*          btc_jobcount                   = jobcount
        TABLES
          pdf                            = pdf
        EXCEPTIONS
          err_no_abap_spooljob           = 1
          err_no_spooljob                = 2
          err_no_permission              = 3
          err_conv_not_possible          = 4
          err_bad_destdevice             = 5
          user_cancelled                 = 6
          err_spoolerror                 = 7
          err_temseerror                 = 8
          err_btcjob_open_failed         = 9
          err_btcjob_submit_failed       = 10
          err_btcjob_close_failed        = 11.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        WRITE: / 'No ABAP spool job'(002)
              COLOR COL_NEGATIVE.
        EXIT.
      WHEN 2.
        WRITE: / 'No spool job'(003)
              COLOR COL_NEGATIVE.
        EXIT.
      WHEN 3.
        WRITE: / 'Permission denied'(004)
              COLOR COL_NEGATIVE.
        EXIT.
      WHEN OTHERS.
        WRITE: / 'Unknown error'(007)
                  COLOR COL_NEGATIVE.
        EXIT.
    ENDCASE.


*    MESSAGE S060(ZPYIN) WITH LC_FILENAME.
*  delete spool request

    lc_spoolid = li_spoolno.
    CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
         EXPORTING
              spoolid = lc_spoolid
*      IMPORTING
*           RC      =
*           STATUS  =
  .

    LOOP AT pdf.
      TRANSLATE pdf USING ' ~'.
      CONCATENATE gd_buffer pdf INTO gd_buffer.
    ENDLOOP.
    TRANSLATE gd_buffer USING '~ '.
    DO.
      objbin = gd_buffer.
      APPEND objbin.
      SHIFT gd_buffer LEFT BY 255 PLACES.
      IF gd_buffer IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    " PDF_DOWNLOAD

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mail_content ." Part of modification-1.3(ECDK914202) by Ravi Kumar
** on 18/03/2009 to take content out of the loop

  REFRESH objtxt. CLEAR objtxt.

  objtxt = '<html><head>'.
  APPEND objtxt.
  objtxt = '<style type="text/css">'.
  APPEND objtxt.
  objtxt =
  '.content {font-family: Verdana, sans-serif;font-size: 11px;}'.
  APPEND objtxt.
  objtxt =
'.orange {font-family: Verdana, sans-serif;font-size: 11px;color:#ff6' &
'600}'
  .
  APPEND objtxt.
*  objtxt = name="verdana" size="11px">'.
*  APPEND objtxt.
  objtxt = '</style></head>'.
  APPEND objtxt.

  objtxt = '<body>'.
  APPEND objtxt.
****************Begin of mod01(ECDK921140)********************
  IF P_PERSAR = '06'.
    objtxt = '<font class="content"><B>Dear Intern,</B><BR><BR>'.
    APPEND objtxt.

    CONCATENATE
    'Please find attached your stipend pay slip for the month of &nbsp;'
       month ',&nbsp;' year '.' '<BR>'  INTO objtxt.
    APPEND objtxt.

    objtxt = '<BR>For all your stipend related queries,'.
    APPEND objtxt.

    objtxt = 'please contact <B>binu_jacob@infosys.com</B>.<BR>'.
    APPEND objtxt.

  ELSE.
**********************END OF mod01****************************
    objtxt = '<font class="content"><B>Dear Infoscion,</B><BR><BR>'.
    APPEND objtxt.

    CONCATENATE
      'Please find attached your pay slip for the month of &nbsp;'
      month ',&nbsp;' year '.' '<BR>'  INTO objtxt.
    APPEND objtxt.


**Modification-1.3(ECDK914202)**********************
**Content added by Ravi Kumar on 18/03/09 based on payroll area
* Commented By Patha Sridhar on 30/03/2009 SerchCode:ECDK914709
    IF p_persar+0(1) = 'P'.
*    CONCATENATE '<BR>Kindly check your Tax computation'
*    'statement and do get back if there are any '
*    'discrepancies in the computation.' INTO objtxt.
*    APPEND objtxt.
* Commented By Patha Sridhar on 30/03/2009 SerchCode:ECDK914709

* Commented By Patha Sridhar on 30/04/2009 SerchCode:ECDK916001
* Added By Patha Sridhar on 30/03/2009 SerchCode:ECDK914709
*  objtxt =
*'<BR>Kindly check your Tax computation statement for the financial year in harmony after 01.04.2009  '.
*  APPEND objtxt.

* Added By Patha Sridhar on 30/04/2009 SerchCode:ECDK916001
      objtxt =
    '<BR>Kindly check your Tax computation statement for the financial year in harmony after 5th of next month. <BR> '.
      APPEND objtxt.
* Ended By Patha Sridhar on 30/04/2009 SerchCode:ECDK916001

* Ended By Patha Sridhar on 30/03/2009 SerchCode:ECDK914709

    ENDIF.

***End of modification-1.3************************************
* commented content Patha Sridhar on 20/10/2008 ECDK906513
*  objtxt = '<BR>For all your salary and other related queries,'.
* Added content Patha Sridhar on 20/10/2008  ECDK906513

    objtxt = '<BR>For all your salary related queries,'.
    APPEND objtxt.
* Endded ECDK906513
 IF p_persar+0(1) NE '1'.
    objtxt =
  'please log in to ServCentralé using the following links.<BR>'.
    APPEND objtxt.

    CONCATENATE '<BR>Intranet users:'
    '<A href="http://172.25.103.20/rtracker/aspx/default.aspx?page=NEW">'
                INTO objtxt.
    APPEND objtxt.

    objtxt =
   'http://172.25.103.20/rtracker/aspx/default.aspx?page=NEW</a>'.
    APPEND objtxt.
  ENDIF.
**Modification-1.3(ECDK914202) by Ravi Kumar on 18/03/2009************
**Extranet link will not come for iBPO
    IF p_persar+0(1) NE 'P' and p_persar+0(1) NE '1'.
      CONCATENATE '<BR>Extranet users:'
    '<A href="https://xnet.infosys.com/RTRAcker/aspx/default.aspx?page=NE' &
    'W">'
      INTO objtxt.
      APPEND objtxt.

      objtxt =
      'https://xnet.infosys.com/RTRAcker/aspx/default.aspx?page=NEW</a><BR>'
      .
      APPEND objtxt.
    ENDIF.
*******************************************************************
* Added content Patha Sridhar on 20/10/2008 ECDK906513
    If p_persar+0(1) NE '1'.
    objtxt =
  '<BR>Please raise a request in Servcentrale under the respective '.
    APPEND objtxt.

    objtxt =
  'service groups for the following type of clarifications:.<BR>'.
    APPEND objtxt.
ENDIF.
***Modification-1.3(ECDK914202) by Ravi Kumar on 18/03/2009
**Content based on payroll area (added because of iBPO)
    IF p_persar+0(1) = 'P'.
      objtxt =
       '<B><BR>Facilities:</B> Telephone deduction, Transport deduction GYM and MLPL charges.<BR>'.
      APPEND objtxt.

      objtxt =
      '<B><BR>HRD (Compensation & Benefit):</B> Salary changes, Computation of Variable pay,'.
      APPEND objtxt.

      objtxt = 'Salary changes, Compensation review and Salary revision.<BR> '.
      APPEND objtxt.

      objtxt =
      '<B><BR>Finance - IBPO(Loans):</B> Personal loan, Salary advance, Rent deposit loan.<BR> '.
      APPEND objtxt.

*Added by Vineeta on 27 Oct 09 for ECDK924235 changes
    ELSEIF p_persar+0(1) = '1'.

   CONCATENATE 'please mail to '
    '<A href="mailto:ICIPayroll@infosys.com">' INTO objtxt.

    APPEND objtxt.

    objtxt =
           'ICIPayroll@infosys.com</a><BR>'.
    APPEND objtxt.


    ELSE.
      objtxt =
       '<B><BR>Facilities:</B> Guest House, Hostel rent, Gym, Membership fees, MLPL charges,'.
      APPEND objtxt.
      objtxt =
    'Telephone and Transport deductions.<BR>'.
      APPEND objtxt.

      objtxt =
    '<B><BR>HRD (Compensation & Benefit):</B> Computation of Variable payouts like VCPI, IPI '.
      APPEND objtxt.

      objtxt =
    ' and Differential AVP, Compensation review and Salary revision.<BR>'.
      APPEND objtxt.

      objtxt =
    '<B><BR>Corporate Accounting Group(Loans & CLA):</B> Vehicle loan allowance, Housing loan allowance '.
      APPEND objtxt.

      objtxt =
    ' and Soft loan, Salary advance, Salary loan, Third party loan deductions.<BR>'.
      APPEND objtxt.

* Ennded ECDK906513

      objtxt =
      '<BR>To understand and know more about your pay computation, '.
      APPEND objtxt.

      objtxt =
      'you may go through the salary FAQs using the link below.<BR>'.
      APPEND objtxt.

      objtxt =
    '<a href=http://sparsh/V1/myUnit/Accounts/Accounts_Salaries_Salary.htm>'
      .
      APPEND objtxt.
      objtxt =
    'http://sparsh/V1/myUnit/Accounts/Accounts_Salaries_Salary.htm</a><BR>'
      .
      APPEND objtxt.
* Added By Patha Sridhar on 30/03/2009 SerchCode:ECDK914709
      IF p_persar+0(1) = '0'."ECDK927375
      objtxt =
  '<BR><B>Note: We would like to inform that the salary for the'.
****modification 01****
      CONCATENATE objtxt ' current month will be credited on&nbsp;'
      lv_pytext '.' '</B><BR>' INTO objtxt.
      APPEND objtxt.
      endif."ECDK927375

    ENDIF.
  ENDIF.


  objtxt = '<BR>Regards,<BR>'.
  APPEND objtxt.

  IF p_persar+0(1) = '1'.
  objtxt = '<B>Team - ICI Payroll </B><BR>'.
  APPEND objtxt.
  ELSE.

  objtxt = '<B>Team - India Payroll </B><BR>'.
  APPEND objtxt.

  IF p_persar+0(1) = 'P'.
    objtxt = ' '.
    APPEND objtxt.

  ELSE.
    objtxt = ''.
    APPEND objtxt.
    objtxt = ''.
    APPEND objtxt.
  ENDIF.
 ENDIF.  objtxt = '</font>'.
  APPEND objtxt.
  objtxt = '</font></body> </html>'.
  APPEND objtxt.
ENDFORM.                    " MAIL_CONTENT
