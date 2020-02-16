*----------------------------------------------------------------------*
*   INCLUDE PCSAPIN0                                                   *
*----------------------------------------------------------------------*
* This include contains the frequently used modules related to         *
* SAPscript                                                            *
*----------------------------------------------------------------------*
* WARNING: Please do not change  the parameters for the respective     *
*          FORM modules those are used in many report programs.        *
*          If any change required please insert the TAG and do the same*
* History :                                                            *
*          Created by      - Venkatesh Sundaram (i011196)06/12/1999    *
*          Last changed by - Pavan Kumar T.P. (I020961) on 25/11/2002  *
*                            Kiran Kumar Mohan(I031024) on 09/01/2006  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  START_FORM
*&---------------------------------------------------------------------*
*  This module opens a SAPScript for printing                          *
*----------------------------------------------------------------------*
*      --> $FORMNAME  Name of the SAPScript form                       *
*      --> $LANG      Script Language                                  *
*      --> $PAGE      Name of the page                                 *
*----------------------------------------------------------------------*
form start_form using $formname $lang $page.

  call function 'START_FORM'
       exporting
*           ARCHIVE_INDEX    = ' '
            form             = $formname
            language         = $lang
            startpage        = $page
*           PROGRAM          = ' '
*           MAIL_APPL_OBJECT =
*      IMPORTING
*           LANGUAGE      =
       exceptions
            form          = 1
            format        = 2
            unended       = 3
            unopened      = 4
            unused        = 5
            others        = 6.

  case sy-subrc.
    when 1.
*     Layout set $formname in language $lang does not exist
      message  e108(hrpadin01) with $formname $lang.
  endcase.
endform.                               " START_FORM

*&---------------------------------------------------------------------*
*&      Form  END_FORM
*&---------------------------------------------------------------------*
*       This module closes the form                                    *
*----------------------------------------------------------------------*
form end_form.
  call function 'END_FORM'
*    IMPORTING
*         RESULT                   =
      exceptions
           unopened                 = 1
           bad_pageformat_for_print = 2
           others                   = 3
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                               " END_FORM
*&---------------------------------------------------------------------*
*&      Form  WRITE_FORM
*&---------------------------------------------------------------------*
* This module converts the program variables into the Script variable  *
*----------------------------------------------------------------------*
*      -->  $ELE   Element name in the SAPScript,If the script variable*
*                  is not belongs to a element then this is a blank    *
*      -->  $FUN   Name of the function, Ex :APPEND, SET, DELETE       *
*      -->  $TYP   where to write Ex :BODY, HEADER, FOOTER             *
*      -->  $WIN   Type of the window, Ex :Main, VAR                   *
*----------------------------------------------------------------------*
form write_form using  $ele $fun $typ $win.

  call function 'WRITE_FORM'
       exporting
            element               = $ele
            function              = $fun
            type                  = $typ
            window                = $win
*    IMPORTING
*          PENDING_LINES           =
       exceptions
         element                  = 1
         function                 = 2
         type                     = 3
         unopened                 = 4
         unstarted                = 5
         window                   = 6
         bad_pageformat_for_print = 7
         others                   = 8
          .

  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                               " WRITE_FORM

*&---------------------------------------------------------------------*
*&      Form  OPEN_FORM
*&---------------------------------------------------------------------*
*  This modules opens a form for printing                              *
*----------------------------------------------------------------------*
*      --> $LANG      Script Language                                  *
*----------------------------------------------------------------------*
form open_form using $lang.

* BEGIN OF ESS CHANGES
  tables: itcpo.

  data: OPTIONS like itcpo.
  DATA: DIALOG(1).
  DATA: pri_PARAMS LIKE PRI_PARAMS.

  IF PNPESSCF = 'X'.
    OPTIONS-TDNOPRINT = 'X'.
    OPTIONS-TDPREVIEW = 'X'.
    OPTIONS-TDTITLE = SPACE.
    options-tdgetotf = 'X'.
    options-Tdprinter = 'POSTSCPT'.

    DIALOG = SPACE.
  ELSE.
    DIALOG = 'X'.
  ENDIF.
* END OF ESS CHANGES

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
         EXPORTING  NO_DIALOG             = 'X'
                    MODE                  = 'CURRENT'
*                  NEW_LIST_ID           = $PRNEW
         IMPORTING  OUT_PARAMETERS       =  pri_PARAMS.

  options-TDCOPIES = pri_params-PRCOP.
  options-TDDEST = pri_params-PDEST.
  options-TDNEWID = pri_params-prnew.
  options-TDIMMED = pri_params-primm.
  options-TDDELETE = pri_params-prrel.
  options-TDLIFETIME = pri_params-PEXPI.
  options-TDTITLE = pri_params-PRTXT.
  options-TDCOVER = pri_params-PRSAP.
  options-TDCOVTITLE = pri_params-PRTXT.
  options-TDRECEIVER = pri_params-PRREC.
  options-TDDIVISION = pri_params-PRABT.
  options-TDAUTORITY = pri_params-PRBER.

  call function 'OPEN_FORM'
      exporting
           application                 = 'TX'
*         ARCHIVE_INDEX               =
*         ARCHIVE_PARAMS              =
           device                      = 'PRINTER'
           dialog                      = DIALOG
*         FORM                        = ' '
           language                    = $lang
          OPTIONS                     = OPTIONS
*         MAIL_SENDER                 =
*         MAIL_RECIPIENT              =
*         MAIL_APPL_OBJECT            =
*         RAW_DATA_INTERFACE          = '*'
*    IMPORTING
*         LANGUAGE                    =
*         NEW_ARCHIVE_PARAMS          =
*         RESULT                      =
      exceptions
           canceled                    = 1
           device                      = 2
           form                        = 3
           options                     = 4
           unclosed                    = 5
           mail_options                = 6
           archive_error               = 7
           invalid_fax_number          = 8
           more_params_needed_in_batch = 9
           others                      = 10
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                               " OPEN_FORM

*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM
*&---------------------------------------------------------------------*
*       Closes the SAPscript
*----------------------------------------------------------------------*
form close_form.

* Data for ESS scenario.
  data: otf_table  like itcoo occurs 0 with header line.
* Data for ESS scenario

  call function 'CLOSE_FORM'
*    IMPORTING
*         RESULT                   =
*         RDI_RESULT               =
     TABLES
          OTFDATA                  = OTF_TABLE
      exceptions
           unopened                 = 1
           bad_pageformat_for_print = 2
           others                   = 3
            .
  if sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
* BEGIN OF ESS CHANGES
  IF PNPESSCF = 'X'.
*   perform report_data_convert(saplhress00_rep) tables otf_table[].
    perform report_data_convert tables otf_table[] .        "RK1016444

  ENDIF.
* END OF ESS CHANGES

endform.                               " CLOSE_FORM

*&---------------------------------------------------------------------*
*&      Form  CURR_TO_CHAR
*&---------------------------------------------------------------------*
*   Converts the value in currency into the character
*----------------------------------------------------------------------*
*      --> $VAR   Name of the variable in SAPscript                    *
*      --> $AMT   Currency variable                                    *
*----------------------------------------------------------------------*
form curr_to_char using value($var) value($amt).
* Data declaration CURR related to number decimal places in AMT
  data: curr(2).
  data: val1(15).
  clear val1.
  write $amt currency curr to val1.
  perform convert_to_scriptvar using $var val1.

endform.                               " WRITE_SYBL

*&---------------------------------------------------------------------*
*&      Form  convert_to_scriptvar                                     *
*&---------------------------------------------------------------------*
*   Converts the number into the character
*----------------------------------------------------------------------*
*      --> $NAME   Name of the variable in SAPscript                   *
*      --> $WERT   Name variable (Numeric type)                        *
*----------------------------------------------------------------------*
form convert_to_scriptvar using  value($name) value($wert).
  data: fname(22) type c.

  fname+00(1) = '&'.
  fname+01(20) = $name.
  fname+21(1) = '&'.
  condense fname no-gaps.
  call function 'TEXT_SYMBOL_SETVALUE'
    EXPORTING
      name         = fname
      value        = $wert
      value_length = 0.

endform.                               " convert_to_scriptvar

*&---------------------------------------------------------------------*
*&      Form  ADDRESS
*&---------------------------------------------------------------------*
*       Get the address using the Company code
*----------------------------------------------------------------------*
*  -->  $bukrs   Company code
*----------------------------------------------------------------------*
form address using $bukrs
                   address_value.                  "MKINT910704
  data:  aa(1), bb(1), cc(1), dd(1), ee(1),
         ff(1), gg(1), hh(1), ii(1).

  data:  adrnr like hrca_company-address.
  data: sadr1 like sadr occurs 10 with header line.

  data:  selection like addr1_sel.
*         address_value like addr1_val.

  call function 'HRCA_COMPANYCODE_GETDETAIL'
         exporting
               companycode = $bukrs
               language    = sy-langu
         importing
*            COMP_NAME
*            CITY
*            COUNTRY
*            CURRENCY
*            LANGU
*            CHRT_ACCTS
*            FY_VARIANT
*            FI_MANAGEMENTAREA
*            JURISDICTION
*            VALUE(RATE_DEVIATION) LIKE  HRCA_COMPANY-RATE_DEVIAT
               address = adrnr
         exceptions
                not_found.

* To get the address for the company specified.

  selection-addrnumber = adrnr.        "SADR40A (check TVKO-ADRNR)

  call function 'ADDR_GET'
    EXPORTING
      address_selection = selection
      address_group     = 'CA01' "to read SADR for unchanged
    IMPORTING                       "data
      address_value     = address_value  "both structures filled
      sadr              = sadr  "choose one of them
    EXCEPTIONS
      address_not_exist = 1
      others            = 2.                                "SADR40A

  if sadr-name1 is initial. aa = 'N'. endif.
  if sadr-name2 is initial. bb = 'N'. endif.
  if sadr-name3 is initial. cc = 'N'. endif.
  if sadr-name4 is initial. dd = 'N'. endif.
  if sadr-stras is initial. ee = 'N'. endif.
  if sadr-pfach is initial. ff = 'N'. endif.
  if sadr-pstlz is initial. gg = 'N'. endif.
  if sadr-ort01 is initial. hh = 'N'. endif.
  if sadr-ort02 is initial. ii = 'N'. endif.

  perform convert_to_scriptvar using 'A' aa.
  perform convert_to_scriptvar using 'B' bb.
  perform convert_to_scriptvar using 'C' cc.
  perform convert_to_scriptvar using 'D' dd.
  perform convert_to_scriptvar using 'E' ee.
  perform convert_to_scriptvar using 'F' ff.
  perform convert_to_scriptvar using 'G' gg.
  perform convert_to_scriptvar using 'H' hh.
  perform convert_to_scriptvar using 'I' ii.
endform.                               " ADDRESS

*&---------------------------------------------------------------------*
*&      Form  GET_ADDRESS
*&---------------------------------------------------------------------*
*       Get the address using the address number
*----------------------------------------------------------------------*
*  -->  $adrnum   Address number
*----------------------------------------------------------------------*
form GET_address using $adrnum.
  data:  aa(1), bb(1), cc(1), dd(1), ee(1),
         ff(1), gg(1), hh(1), ii(1).

  data:  adrnr like hrca_company-address.
  data: sadr1 like sadr occurs 10 with header line.

  data:  selection like addr1_sel,
         address_value like addr1_val.

* To get the address for the number specified.

  selection-addrnumber = $adrnum.        "SADR40A (check TVKO-ADRNR)

  call function 'ADDR_GET'
    EXPORTING
      address_selection = selection
      address_group     = 'CA01' "to read SADR for unchanged
    IMPORTING                       "data
      address_value     = address_value  "both structures filled
      sadr              = sadr  "choose one of them
    EXCEPTIONS
      address_not_exist = 1
      others            = 2.                                "SADR40A

  if sadr-name1 is initial. aa = 'N'. endif.
  if sadr-name2 is initial. bb = 'N'. endif.
  if sadr-name3 is initial. cc = 'N'. endif.
  if sadr-name4 is initial. dd = 'N'. endif.
  if sadr-stras is initial. ee = 'N'. endif.
  if sadr-pfach is initial. ff = 'N'. endif.
  if sadr-pstlz is initial. gg = 'N'. endif.
  if sadr-ort01 is initial. hh = 'N'. endif.
  if sadr-ort02 is initial. ii = 'N'. endif.

  perform convert_to_scriptvar using 'A' aa.
  perform convert_to_scriptvar using 'B' bb.
  perform convert_to_scriptvar using 'C' cc.
  perform convert_to_scriptvar using 'D' dd.
  perform convert_to_scriptvar using 'E' ee.
  perform convert_to_scriptvar using 'F' ff.
  perform convert_to_scriptvar using 'G' gg.
  perform convert_to_scriptvar using 'H' hh.
  perform convert_to_scriptvar using 'I' ii.
endform.                               " ADDRESS

*&---------------------------------------------------------------------*
*&      Form  FILL_SPACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EPFH1-FILLER  text                                         *
*      -->P_0045   text                                                *
*----------------------------------------------------------------------*
FORM FILL_SPACE USING  $VALUE CHANGING $FILLER.
  DATA CNT TYPE I.
  CNT = $VALUE.
  WHILE CNT NE 0.
    CNT = CNT - 1.
    MOVE ' ' TO $FILLER+CNT(1).
  ENDWHILE.
ENDFORM.                    "FILL_SPACE



*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FORMNAME output form name                                    *
*      -->END_DATE period end date                                     *
*      -->P_SCRIPT if set,    output type = SAP Script                 *
*                  otherwise, output type = Adobe Form                 *
*----------------------------------------------------------------------*
FORM GET_OUTPUT_TYPE USING    END_DATE
                     CHANGING FORMNAME
       P_SCRIPT.

  DATA: OUTPUT_TYPE TYPE T5F99OSFT-FOTYPE.

  CALL FUNCTION 'HR_99S_GET_LFORM_PROPERTIES'
    EXPORTING
      I_FOLNAME                      = FORMNAME
*   I_FOVARIANT                    =
      I_DATE                         = END_DAte
*   I_FOTYPE                       = OUTPUT_TYPE
    IMPORTING
      E_FOPNAME                      = FORMNAME
      E_FOTYPE                       = OUTPUT_TYPE
    EXCEPTIONS
      TYPE_NAME_NOT_MAINTAINED       = 1
      FORM_NAME_NOT_FOUND            = 2
      OTHERS                         = 3
            .
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E262(HRPADIN01).
      WHEN 2.
        MESSAGE E263(HRPADIN01).
      WHEN 3.
        MESSAGE E264(HRPADIN01).
    ENDCASE.
  ENDIF.

  IF OUTPUT_TYPE = 'SSC'.
    p_script = 'X'.
  ELSE.
    CLEAR p_script.
  ENDIF.

ENDFORM.                    "GET_OUTPUT_TYPE
*&---------------------------------------------------------------------*
*&      Form  report_data_convert     "RK1016444
*&---------------------------------------------------------------------*
Form report_data_convert tables otf_table structure itcoo.

  data: otf_Tab like itcoo occurs 0 with header line.
  statics: cp like tcp00-cpcodepage.
  data: pdf_table type  rcl_bag_tline,
        pdf_string_X type xstring,
        pdf_fsize type  i.

  otf_tab[] = otf_table[].

  call function 'SYSTEM_CODEPAGE'
    IMPORTING
      current_dynamic_codepage = cp.

* If Unicode system
  if cp(1) = '4'.
    call function 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = pdf_fsize
        bin_file              = pdf_string_x
      TABLES
        otf                   = otf_table
        lines                 = pdf_table
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        others                = 5.
    IF PNPESSCF = 'X'.
      IMPORT pdf_string_x FROM MEMORY ID 'F16_PDF'.
    ENDIF.
    export pdf_string_x to memory id 'PDFT'.
    export pdf_fsize to memory id 'PDSZ'.
    export pdf_string_x to memory id 'PDFF'.
    export pdf_string_x to memory id 'PDFF_IN'.
    export pdf_table to memory id 'PDFT_IN'.
    export pdf_fsize to memory id 'PDSZ_IN'.

  else.
    call function 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = pdf_fsize
*        bin_file              = pdf_string_x
      TABLES
        otf                   = otf_tab
        lines                 = pdf_table
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        others                = 4.

    export pdf_table to memory id 'PDFT'.
    export pdf_fsize to memory id 'PDSZ'.
*    export pdf_string_x to memory id 'PDFF'.
*    export pdf_string_x to memory id 'PDFF_IN'.
    export pdf_table to memory id 'PDFT_IN'.
    export pdf_fsize to memory id 'PDSZ_IN'.

    call function 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = pdf_fsize
        bin_file              = pdf_string_x
      TABLES
        otf                   = otf_tab
        lines                 = pdf_table
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        others                = 4.
    IF PNPESSCF = 'X'.
      IMPORT pdf_string_x FROM MEMORY ID 'F16_PDF'.
    ENDIF.
*    export pdf_table to memory id 'PDFT'.
*    export pdf_fsize to memory id 'PDSZ'.
    export pdf_string_x to memory id 'PDFF'.
    export pdf_string_x to memory id 'PDFF_IN'.
*    export pdf_table to memory id 'PDFT_IN'.
*    export pdf_fsize to memory id 'PDSZ_IN'.


  endif.
endform.                    "report_data_convert
