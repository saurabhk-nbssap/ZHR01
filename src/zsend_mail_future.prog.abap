*&---------------------------------------------------------------------*
*& Report  ZSEND_MAIL_FUTURE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zsend_mail_future.
tables: somlreci1.
constants:
*-- Constants used in the body of the Email (HTML)
  c_htm         type char3   value 'HTM',
  c_style_start type char255 value '<FONT face=Arial size=2>'.
data: c_new_line  type char255 value 'Dear ', "<br>',
      c_link_text type char32  value '',
      c_link_end  type char4   value '</A>',
      c_space(6)  type c       value '&nbsp;',

*-- Used as an Example for displaying space between texts in Email body
      c_emp1(6)   type c       value 101001,
      c_emp2(6)   type c       value 101002,
      c_emp3(6)   type c       value 101003.

data: zdate type sy-datum."(10).
data: lv_timestamp type timestamp.

get time stamp field lv_timestamp.

*-- Local data declaration for sending mail
data: l_send_request  type ref to cl_bcs,
      l_document      type ref to cl_document_bcs,
      l_sender        type ref to cl_cam_address_bcs, "CL_SAPUSER_BCS,
      l_sub           type char50,
      l_recipient     type ref to if_recipient_bcs,
      i_copy          type os_boolean,
      tl_contents     type standard table of soli,
      l_doc_len       type so_obj_len,
      l_cnt           type sy-tabix,
      l_rcv_email     type adr6-smtp_addr,
      l_result        type sy-binpt,
      l_bcs_exception type ref to cx_bcs,
      l_subj          type string.

data: sender type sy-uname.

data: t_mailhex   type standard table of solix,
      t_contents  type standard table of solisti1,
      wa_contents type solisti1,
      w_file      type dsvasdocid,
      w_extn(5)   type c,
      w_mail_subj type string,
      w_document  type ref to cl_document_bcs.

data: s_mailid like standard table of somlreci1.
data: wl_mailid like line of s_mailid.

data: gv_path        type string value '/usr/sap/trans/hrappraisal/Probationer_Appraisal_Form.doc',
      ls_bin         type solix,
      binary_content type solix_tab,
      xstr           type xstring.
**      document       TYPE REF TO cl_document_bcs.

field-symbols <hex_container> type x.

selection-screen begin of block blk1.
parameters: p_date type sy-datum.
parameters: m_subj type string,
            persg  type persg.
select-options: p_rec for somlreci1-receiver.

selection-screen end of block blk1.

loop at p_rec.

  try.
*-- Add the recipients to the Send mail
      wl_mailid-receiver = p_rec-low ."'pshinde-icc@modi.com'.

      refresh t_contents.
      import wa_date-t_contents to t_contents from memory id 'MAIL_BODY'. " exported from zhr_ps
*      PERFORM mail_body.

*-- Create persistent send request
      l_send_request = cl_bcs=>create_persistent( ).
      tl_contents[] = t_contents[].

*-- Get the length of the Document
      describe table tl_contents lines l_cnt.
      read table tl_contents into wa_contents index l_cnt.
      l_doc_len = ( l_cnt - 1 ) * 255 + strlen( wa_contents ).
*-- Subject of the mail
      l_sub = m_subj. "w_mail_subj.
*-- Create Document
      l_document = cl_document_bcs=>create_document(
                   i_type       = c_htm
                   i_text       = tl_contents
                   i_length     = l_doc_len
                   i_subject    = l_sub
                   i_language   = sy-langu
                   i_importance = '1' ).
*-- Subject of the mail
      move w_mail_subj to l_subj.
      w_document = l_document.

      try.
*-- Set the Message Subject
          call method l_send_request->set_message_subject
            exporting
              ip_subject = l_subj.
        catch cx_sy_dyn_call_illegal_method.
      endtry.

*****************************************************************************************************
*    open dataset gv_path for input in binary mode.
*    do.
*      assign ls_bin to <hex_container> CASTING.
*      read dataset gv_path into <hex_container>.
*      if sy-subrc eq 0.
*        append ls_bin to binary_content.
*      else.
*        exit.
*      endif.
*    enddo.
*    close dataset gv_path.

      open dataset gv_path for input in binary mode.
      if sy-subrc = 0.
        read dataset gv_path into xstr.  "all in one go
        close dataset gv_path.
      endif.

      binary_content = cl_document_bcs=>xstring_to_solix( xstr ).

      call method l_document->add_attachment
        exporting
          i_attachment_type    = 'DOC'
          i_attachment_subject = 'Probationer Appraisal Form'
          i_att_content_hex    = binary_content.
*****************************************************************************************************

*-- Add document to send request
      call method l_send_request->set_document( l_document ).

*-- Do send delivery info for successful mails
      call method l_send_request->set_status_attributes
        exporting
          i_requested_status = 'E'
          i_status_mail      = 'A'.
*-- Set sender
      data: v_sender type ad_smtpadr.
      data l_visname type ad_namelas.

      v_sender = 'sapautomail@indofil.com'.

      l_sender = cl_cam_address_bcs=>create_internet_address( v_sender ).

      call method l_send_request->set_sender
        exporting
          i_sender = l_sender.

*-- To frame the attachments for the mail
      perform frame_attachments.
**-- Add the recipients to the Send mail
      l_rcv_email = wl_mailid-receiver.

      check not l_rcv_email is initial.
      l_recipient = cl_cam_address_bcs=>create_internet_address(
                                                    l_rcv_email ).
      call method l_send_request->add_recipient
        exporting
          i_recipient = l_recipient
*         i_copy      = ''
          i_express   = 'X'.

*lv_timestamp = CL_ABAP_TSTMP=>ADD( tstmp = lv_timestamp  secs = 600 ).
      zdate = p_date." sy-datum + 1.

      call function 'ABI_TIMESTAMP_CONVERT_INTO'
        exporting
          iv_date          = zdate
          iv_time          = sy-uzeit
        importing
          ev_timestamp     = lv_timestamp
        exceptions
          conversion_error = 1
          others           = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      l_send_request->send_request->set_send_at( lv_timestamp ).

*-- Send Email
      call method l_send_request->send(
        exporting
          i_with_error_screen = 'X'
        receiving
          result              = l_result ).

      commit work.
    catch cx_bcs into l_bcs_exception.
      data(ztext) = l_bcs_exception->get_text( ).
      rollback work.
*      if l_result ne 'X'.
*        message s999(zz) with
*        'Mail Not Successful'(004).
*      endif.
  endtry.

  " IHDK900610
  free memory id 'MAIL_BODY'.
  if l_result ne abap_true.
    data(lv_result) = 'F'.
    export lv_result from lv_result to memory id 'EMAIL_RES'.
    clear lv_result.
  endif.

endloop.
*&---------------------------------------------------------------------*
*&      Form  MAIL_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mail_body .

*-- Local data declaration to hold the textpool
  data: tl_textpool type standard table of textpool,
        wl_textpool type textpool.


  clear: tl_textpool, wl_textpool , t_contents , wa_contents.

*-- Read the Entire Textpool into an Internal table
  read textpool sy-repid into tl_textpool language sy-langu.
  if sy-subrc is initial.
    sort tl_textpool by id key.
  endif.

*-- Font start
  clear wa_contents.
  wa_contents-line = c_style_start.
  append wa_contents to t_contents.

  clear wa_contents.
  c_new_line = 'Dear Sir, '.
  wa_contents-line = c_new_line.
  append wa_contents to t_contents.

  clear wa_contents.
  c_new_line = 'Please find Details of Employee: '.
  wa_contents-line = c_new_line.
  append wa_contents to t_contents.

  clear: wa_contents , c_new_line.
  c_new_line = '<br>'.

  wa_contents-line = c_new_line.
  append wa_contents to t_contents.

  w_mail_subj = m_subj."'2 Months completed after hiring !!' .

endform.
*&---------------------------------------------------------------------*
*&      Form  FRAME_ATTACHMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frame_attachments .
**-- Local Data declaration
*  DATA: L_SUBJECT   TYPE SO_OBJ_DES,
*        L_ATT_TYPE  TYPE SOODK-OBJTP.
*
**-- Subject of the Attachment
*  L_SUBJECT  = W_FILE.
**-- Format of the Attachment
*  L_ATT_TYPE = W_EXTN.
*
*  IF T_MAILHEX[] IS NOT INITIAL.
*    TRY.
**-- Add Attachment to the Document
*        CALL METHOD W_DOCUMENT->ADD_ATTACHMENT
*          EXPORTING
*            I_ATTACHMENT_TYPE    = L_ATT_TYPE
*            I_ATTACHMENT_SUBJECT = L_SUBJECT
*            I_ATT_CONTENT_HEX    = T_MAILHEX.
*
*      CATCH CX_DOCUMENT_BCS.
*    ENDTRY.
*  ENDIF.


endform.
