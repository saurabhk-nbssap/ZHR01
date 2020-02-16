*&---------------------------------------------------------------------*
*& Report Z_ON_SEPERATION_DUE_DEACTIVE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_on_seperation_due_deactive.


CLASS lcl_deactivation DEFINITION.
  PUBLIC SECTION.
    DATA: lv_pastdate TYPE sy-datum,
          zpernr      TYPE zatr_user_tmap-user_id,
          zpernr_out  TYPE zatr_user_tmap-user_id.

    DATA: it_pa0000      TYPE TABLE OF pa0000,
          wa_pa0000      TYPE pa0000,
          wa_zatr_user_m TYPE zatr_user_m,
          it_atr         TYPE TABLE OF zatr_user_tmap,
          wa_atr         LIKE LINE OF it_atr,
          it_knvp        TYPE TABLE OF knvp,
          wa_knvp        TYPE knvp,
          lv_kunnr_out   TYPE knvp-kunnr.

    METHODS: get_data,deactivate,process,send_mail,update_indonet_master,update_pf.


ENDCLASS.
CLASS lcl_deactivation IMPLEMENTATION.
  METHOD process.
    "fetch the records from PA0000 table from the past 3 months
    "who have Action Type eq Seperation indicator(I7)
    get_data( ).

    CHECK it_pa0000[] IS NOT INITIAL.
    "Deactivating the person and deleting the territories and updating the BP's
    deactivate( ).

  ENDMETHOD.
  METHOD get_data.
    CALL FUNCTION 'CCM_GO_BACK_MONTHS'
      EXPORTING
        currdate   = sy-datum
        backmonths = '003'
      IMPORTING
        newdate    = lv_pastdate.

    SELECT * FROM pa0000 INTO TABLE it_pa0000
             WHERE begda BETWEEN lv_pastdate AND sy-datum
             AND   massn EQ 'I7'.
  ENDMETHOD.
  METHOD deactivate.

    LOOP AT it_pa0000 INTO wa_pa0000.

      zpernr = CONV zpernr( |{ wa_pa0000-pernr ALPHA = IN }| ).
      zpernr_out = CONV zpernr( |{ wa_pa0000-pernr ALPHA = OUT }| ).

      SELECT SINGLE * FROM zatr_user_m INTO wa_zatr_user_m WHERE user_id EQ zpernr
                                                           AND   status = ''.
      IF sy-subrc EQ 0.
        wa_zatr_user_m-status = 'X'.
        MODIFY zatr_user_m FROM wa_zatr_user_m.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        FORMAT COLOR = 6 INTENSIFIED ON.
        WRITE:/ zpernr_out && ' Employee is Deactivated in Indonet Master'.
        FORMAT RESET.

        update_indonet_master( ).

        "After the Deactivation and removal of territory records from the ATR
        "We need to delete the deactivated numbers in the partner function in all the BP's
        "for removing the deactivated numbers from BP below method is used.
        update_pf( ).

        send_mail( ).

        CLEAR: it_atr[],it_knvp[].
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD send_mail.

    DATA: body       TYPE soli_tab,
          wa_body    LIKE LINE OF body,
          subject    TYPE string,
          recipient  TYPE zfi_s_vp_recipient,
          recipients LIKE TABLE OF recipient,
          sent       TYPE abap_bool.

    DATA: zwerks TYPE pa0001-werks,
          zgrade TYPE pa0001-persk,
          zname  TYPE pa0001-ename,
          zterr  TYPE tvgrt-bezei.


****** Attaching the Body of Mail**********

    SELECT SINGLE werks persk ename FROM pa0001 INTO (zwerks , zgrade , zname)
                                    WHERE pernr =  wa_pa0000-pernr
                                    AND   endda = '99991231'.

    SELECT SINGLE name1 FROM t500p INTO @DATA(zregion) WHERE persa = @zwerks.

    SELECT SINGLE usrid_long FROM pa0105 INTO @DATA(zemail_105)
                                         WHERE pernr =  @wa_pa0000-pernr
                                         AND   subty = '0010'.

    REFRESH body[].
    CLEAR wa_body.
    wa_body-line = 'Details of new Employee -'.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = | Employee Number: { zpernr_out }|.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = | Employee Name: { zname }|.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = | Employee Region: { zregion }|.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = | Employee Grade: { zgrade }|.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = | Separation date: { wa_pa0000-begda DATE = USER }|.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = |Email : { zemail_105 }|.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = 'Below Territory Records deleted from ATR User-Territory Master .'.
    APPEND wa_body TO body.

    LOOP AT it_atr INTO wa_atr.
      CLEAR: zterr.
      SELECT SINGLE bezei FROM tvgrt INTO zterr WHERE spras = sy-langu AND vkgrp = wa_atr-vkgrp.

      CLEAR wa_body.
      wa_body-line = |Territory : { wa_atr-vkgrp },{ zterr }|.
      APPEND wa_body TO body.
    ENDLOOP.

    CLEAR wa_body.
    wa_body-line = 'Below Patner Functions are deleted from Customers'.
    APPEND wa_body TO body.

    LOOP AT it_knvp INTO wa_knvp.
      CLEAR wa_body.
      wa_body-line = |Customer : { lv_kunnr_out },{ wa_knvp-parvw }|.
      APPEND wa_body TO body.
    ENDLOOP.

    CLEAR wa_body.
    wa_body-line = '|-------------------------------------------------------------------------------------------------|'.
    APPEND wa_body TO body.

    CLEAR wa_body.
    wa_body-line = ''.
    APPEND wa_body TO body.

*******Add subject to the mail*******
    CLEAR subject.
    subject = | Separation On: { wa_pa0000-begda DATE = USER } Employee: { zpernr_out }|.

******* Add Recepients************
    REFRESH: recipients[].
    CLEAR: recipient.
    SELECT email FROM zfi044_emailid INTO TABLE @DATA(it_rec) WHERE egroup = 'INDONET'.

    IF it_rec[] IS NOT INITIAL.
      LOOP AT it_rec INTO DATA(wa_rec).
        recipient-copy = abap_false.
        recipient-recipient = wa_rec-email.

        APPEND recipient TO recipients.
        CLEAR: wa_rec,recipient.
      ENDLOOP.
    ENDIF.

******Send Mail*******
    CHECK subject IS NOT INITIAL AND recipients IS NOT INITIAL.
    CLEAR sent.

    NEW zcl_email( )->send_email( EXPORTING subject = subject
                                            sender  = 'sapautomail-icc@modi.com'
                                            body    = body
                                            recipients = recipients
                                  IMPORTING sent = sent ).

    sent = abap_true.
    FORMAT COLOR 5 INTENSIFIED OFF.
    WRITE : / 'Mail send Successfully!!!' , zpernr_out.
    WRITE: /'-------------------------------------------------------------------------------------------------------------'.
    FORMAT RESET.

  ENDMETHOD.
  METHOD update_indonet_master.

    SELECT * FROM zatr_user_tmap INTO TABLE it_atr WHERE user_id = zpernr.
    IF sy-subrc EQ 0.

      LOOP AT it_atr INTO DATA(wa_atr).
        DELETE zatr_user_tmap FROM wa_atr.
        IF sy-subrc EQ 0.
          COMMIT WORK.
          SELECT SINGLE bezei INTO @DATA(lv_bezei) FROM tvgrt WHERE spras = 'E' AND vkgrp = @wa_atr-vkgrp.
          FORMAT COLOR = 7 INTENSIFIED OFF.
          WRITE:/ wa_atr-vkgrp && '(' && lv_bezei && ')' && ' Territory is deleted from the above user'.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD update_pf.

    CONSTANTS:
      " task
      gc_object_task_insert  TYPE bus_ei_object_task VALUE 'I',
      gc_object_task_update  TYPE bus_ei_object_task VALUE 'U',
      gc_object_task_modify  TYPE bus_ei_object_task VALUE 'M',
      gc_object_task_delete  TYPE bus_ei_object_task VALUE 'D',
      gc_object_task_current TYPE bus_ei_object_task VALUE 'C',

      " object
      gc_object_bp           TYPE bu_object VALUE 'BusinessPartner',

      " role
      gc_role_bp             TYPE bapibus1006_bproles-partnerrole VALUE '000000',
      gc_role_sal            TYPE bapibus1006_bproles-partnerrole VALUE 'FLCU00',
      gc_role_fi             TYPE bapibus1006_bproles-partnerrole VALUE 'FLCU01'.

    DATA: ls_data TYPE cvis_ei_extern,
          lt_data TYPE cvis_ei_extern_t.

    SELECT * FROM knvp INTO TABLE it_knvp WHERE pernr EQ wa_pa0000-pernr.

    IF sy-subrc EQ 0.
      LOOP AT it_knvp INTO DATA(wa_knvp).

        zcl_bupa_utilities=>get_bp_assigned_to_customer(
          EXPORTING
            iv_customer     = wa_knvp-kunnr
          IMPORTING
            ev_partner      = DATA(lv_partner)
            ev_partner_guid = DATA(lv_partner_guid) ).

        IF lv_partner IS NOT INITIAL AND lv_partner_guid IS NOT INITIAL.
          zcl_bupa_utilities=>extract_complex_all_with_bp(
            EXPORTING
              iv_partner = lv_partner
            IMPORTING
              es_data     = DATA(ls_data_db)
              et_messages = DATA(lt_extract_messages) ).

          IF ls_data_db IS NOT INITIAL.
            cl_md_bp_maintain=>validate_single_current_state(
              EXPORTING
                i_data    = ls_data_db
              IMPORTING
                et_return = DATA(lt_return_val_curr) ).

            " current state validation disabled;
            REFRESH lt_return_val_curr.

            DELETE lt_return_val_curr WHERE type EQ 'S' OR type EQ 'W' OR type EQ 'I'.
            IF lt_return_val_curr IS INITIAL.
              CLEAR: ls_data.
              ls_data-partner-header-object_task = gc_object_task_update.
              ls_data-partner-header-object = gc_object_bp.
              ls_data-partner-header-object_instance-bpartner = lv_partner.
              ls_data-partner-header-object_instance-bpartnerguid = lv_partner_guid.

              ls_data-customer-header-object_instance-kunnr = wa_knvp-kunnr.
              ls_data-customer-header-object_task = gc_object_task_update.

              APPEND INITIAL LINE TO ls_data-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<sale>).
              IF <sale> IS ASSIGNED.
                " task
                <sale>-task = gc_object_task_update.
                " key fields
                <sale>-data_key-vkorg = wa_knvp-vkorg.
                <sale>-data_key-vtweg = wa_knvp-vtweg.
                <sale>-data_key-spart = wa_knvp-spart.

                " partner function data fields

                APPEND INITIAL LINE TO <sale>-functions-functions ASSIGNING FIELD-SYMBOL(<function>).
                IF <function> IS ASSIGNED.
                  <function>-task = gc_object_task_delete.
                  <function>-data_key-parvw = wa_knvp-parvw.
                  <function>-data_key-parza = wa_knvp-parza.
                  <function>-data-partner  = wa_knvp-pernr.
                  <function>-datax-partner = abap_true.
                ENDIF.
                UNASSIGN <function>.
              ENDIF.

              cl_md_bp_maintain=>validate_single(
                EXPORTING
                  i_data        = ls_data
*                  i_data_db     = ls_data_db " IHDK900433
                IMPORTING
                  et_return_map = DATA(lt_return_validation) ).

              "Suppress the mandatory fields checks from validate in update mode
              IF ls_data-partner-header-object_task = gc_object_task_update.
                DELETE lt_return_validation WHERE id = 'R11' AND number = '401'.  " IHDK900432
              ENDIF.
              DELETE lt_return_validation WHERE type EQ 'S' OR type EQ 'W' OR type EQ 'I'.

              IF lt_return_validation IS INITIAL.
                REFRESH lt_data.
                APPEND ls_data TO lt_data.
                cl_md_bp_maintain=>maintain(
                  EXPORTING
                    i_data    = lt_data
                  IMPORTING
                    e_return  = DATA(lt_return) ).

                READ TABLE lt_return INTO DATA(ls_return) INDEX 1.  " since we send 1 bp at a time
                IF sy-subrc = 0.
                  DELETE ls_return-object_msg WHERE type EQ 'S' OR type EQ 'W' OR type EQ 'I'.
                ENDIF.

                IF ls_return-object_msg IS INITIAL.
                  WAIT UP TO 2 SECONDS.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
                  IF sy-subrc = 0.
                    lv_kunnr_out = |{ wa_knvp-kunnr ALPHA = OUT }|.
                    FORMAT COLOR = 7 INTENSIFIED OFF.
                    WRITE:/ 'Employee' && ` ` && zpernr_out && ` ` && 'is deleted from BP Customer Partner Function'
                            && ` ` && lv_kunnr_out && '(' && wa_knvp-parvw && ')'.
                  ENDIF.
                ELSE.
                  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

                ENDIF.
              ENDIF.
            ELSE.
              MESSAGE |Errors reported by existing data validation - { wa_knvp-kunnr }/{ lv_partner }.| TYPE 'E'.
            ENDIF.
          ELSE.
            MESSAGE |Unable to extract BP data for validation - { wa_knvp-kunnr }/{ lv_partner }.| TYPE 'E'.
          ENDIF.
        ELSE.
          MESSAGE |BP not found for customer - { wa_knvp-kunnr }.| TYPE 'E'.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS start.
ENDCLASS.
CLASS lcl_main IMPLEMENTATION.
  METHOD start.
    DATA: lo_start TYPE REF TO lcl_deactivation.

    FREE lo_start.

    lo_start = NEW lcl_deactivation( ).

    IF lo_start IS BOUND.
      lo_start->process( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_main=>start( ).
