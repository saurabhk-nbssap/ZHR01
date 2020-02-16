class zcl_im_fitv_vendor_sync definition
  public
  final
  create public .

  public section.

    interfaces if_badi_interface .
    interfaces if_fitv_vendor_sync ...
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_IM_FITV_VENDOR_SYNC IMPLEMENTATION.


  method if_fitv_vendor_sync~is_vendor_to_be_created.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_addresses.
*    if i_employee_data-opera eq 'I'. " insert, initial creation only
    assign c_address_data to field-symbol(<ls_address_data>).
    if <ls_address_data> is assigned.
      read table <ls_address_data>-addresses assigning field-symbol(<ls_address>) index 1.
      if <ls_address> is assigned and <ls_address> is not initial and <ls_address>-task eq 'I'.
        append initial line to <ls_address>-data-communication-phone-phone assigning field-symbol(<ls_phone>).
        if <ls_phone> is assigned.
          <ls_phone>-contact-task = i_employee_data-opera.  " IHDK901749
          <ls_phone>-contact-data-telephone   = i_employee_data-cell_phone-usrid.
          <ls_phone>-contact-data-r_3_user    = '3'.
          <ls_phone>-contact-data-home_flag   = abap_true.
          <ls_phone>-contact-data-std_no      = abap_true.

          <ls_phone>-contact-datax-telephone  = abap_true.
          <ls_phone>-contact-datax-r_3_user   = abap_true.
          <ls_phone>-contact-datax-home_flag  = abap_true.
          <ls_phone>-contact-datax-std_no     = abap_true.
        endif.
        append initial line to <ls_address>-data-communication-smtp-smtp assigning field-symbol(<ls_email>).
        if <ls_email> is assigned.
          <ls_email>-contact-task = i_employee_data-opera.  " IHDK901749
          <ls_email>-contact-data-e_mail      = i_employee_data-business_email-usrid_long.
          <ls_email>-contact-data-email_srch  = i_employee_data-business_email-usrid_long.
          <ls_email>-contact-data-home_flag   = abap_true.
          <ls_email>-contact-data-std_no      = abap_true.

          <ls_email>-contact-datax-e_mail     = abap_true.
          <ls_email>-contact-datax-email_srch = abap_true.
          <ls_email>-contact-datax-home_flag  = abap_true.
          <ls_email>-contact-datax-std_no     = abap_true.
        endif.
      endif.
    endif.
*    endif.
  endmethod.


  method if_fitv_vendor_sync~modify_banking_data.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_bp_group_number.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_company_code_data.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_complete_data.
*    if i_employee_data-opera eq 'I'. " insert, initial creation only
    read table ct_bp_data_tab assigning field-symbol(<ls_data>) index 1.
    if <ls_data> is assigned and <ls_data> is not initial.
      append initial line to <ls_data>-partner-central_data-communication-phone-phone assigning field-symbol(<ls_phone>).
      if <ls_phone> is assigned.
        <ls_phone>-contact-task = i_employee_data-opera.  " IHDK901749
        <ls_phone>-contact-data-telephone   = i_employee_data-cell_phone-usrid.
        <ls_phone>-contact-data-r_3_user    = '3'.
        <ls_phone>-contact-data-home_flag   = abap_true.
        <ls_phone>-contact-data-std_no      = abap_true.

        <ls_phone>-contact-datax-telephone  = abap_true.
        <ls_phone>-contact-datax-r_3_user   = abap_true.
        <ls_phone>-contact-datax-home_flag  = abap_true.
        <ls_phone>-contact-datax-std_no     = abap_true.
      endif.
    endif.
*    endif.
  endmethod.


  method if_fitv_vendor_sync~modify_identification_numbers.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_person_central_data.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_roles.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_tax_numbers.

*    if i_employee_data-opera eq 'I'. " insert, initial creation only
    assign c_tax_numbers to field-symbol(<ls_tax_numbers>).
    if <ls_tax_numbers> is assigned.
      <ls_tax_numbers>-common-data-nat_person = abap_true.
      <ls_tax_numbers>-common-datax-nat_person = abap_true.

      append initial line to <ls_tax_numbers>-taxnumbers assigning field-symbol(<ls_taxnumber>).
      if <ls_taxnumber> is assigned.
        <ls_taxnumber>-task = i_employee_data-opera.  " IHDK901749
        <ls_taxnumber>-data_key-bapi-taxnumber = 'Employee_Vendor'.
        <ls_taxnumber>-data_key-bapi-taxtype = 'IN3'.
      endif.
    endif.
*    endif.

  endmethod.


  method if_fitv_vendor_sync~modify_vendor_general_data.
    .
  endmethod.


  method if_fitv_vendor_sync~modify_vendor_group_number.
*    if i_employee_data-opera eq 'I'. " insert, initial creation only " IHDK901749
      c_vendor_number = i_employee_data-pernr.
      c_vendor_number = |{ c_vendor_number alpha = in }|.
*    endif.
  endmethod.
ENDCLASS.
