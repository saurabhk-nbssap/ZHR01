*&---------------------------------------------------------------------*
*& Report  ZPA0001_DATA
*&***********************************************************************************************&*
*& OBJECT NAME          : ZPA0001_DATA                                                           &*
*& TECHNICAL CONSULTANT : PRADEEP KODINAGULA                                                     &*
*& MODULE NAME          : HR                                                                     &*
*& PROGRAM TYPE         : REPORT                                                                 &*
*& CREATE DATE          : MAY 14,2015                                                            &*
*& TRANSPORT NO         : IRDK919525                                                             &*
*& DESCRIPTION          : THIS PROGRAM WILL FETCH ALL THE PERSONAL NUMBERS WHOSE ADMIN DATA IS   &*
*&                        NOT MAINTAINED AND CONVERT INTO EXCEL                                 &*
* REVISION HISTORY--------------------------------------------------------------------------------*
*                                                                                                 *
*   CHANGED BY:                                                                                   *
*   CHANGE ON:                                                                                    *
*   REASON FOR CHANGE:                                                                            *
*                                                                                                 *
*                                                                                                 *
* REVISION HISTORY--------------------------------------------------------------------------------*

REPORT  ZPA0001_DATA.

*&*********************************************************************&*
*&                Data Declaration for processing XML Data             &*
*&*********************************************************************&*

TYPES : BEGIN OF TY_PA0001,
PERNR TYPE PA0001-PERNR,
SBMOD TYPE PA0001-SBMOD,
END OF TY_PA0001.

DATA :      GT_FINAL TYPE TABLE OF TY_PA0001,
            GS_FINAL TYPE TY_PA0001.


  TYPES: BEGIN OF XML_LINE,
          DATA(255) TYPE X,
         END OF XML_LINE.

  DATA: L_IXML                TYPE REF TO IF_IXML,
        L_STREAMFACTORY       TYPE REF TO IF_IXML_STREAM_FACTORY,
        L_OSTREAM             TYPE REF TO IF_IXML_OSTREAM,
        L_RENDERER            TYPE REF TO IF_IXML_RENDERER,
        L_DOCUMENT            TYPE REF TO IF_IXML_DOCUMENT.

  DATA: L_ELEMENT_ROOT        TYPE REF TO IF_IXML_ELEMENT,
        NS_ATTRIBUTE          TYPE REF TO IF_IXML_ATTRIBUTE,
        R_ELEMENT_PROPERTIES  TYPE REF TO IF_IXML_ELEMENT,
        R_WORKSHEET           TYPE REF TO IF_IXML_ELEMENT,
        R_TABLE               TYPE REF TO IF_IXML_ELEMENT,
        R_COLUMN              TYPE REF TO IF_IXML_ELEMENT,
        R_ROW                 TYPE REF TO IF_IXML_ELEMENT,
        R_CELL                TYPE REF TO IF_IXML_ELEMENT,
        R_DATA                TYPE REF TO IF_IXML_ELEMENT,
        L_VALUE               TYPE STRING,
        R_STYLES              TYPE REF TO IF_IXML_ELEMENT,
        R_STYLE               TYPE REF TO IF_IXML_ELEMENT,
        R_STYLE1              TYPE REF TO IF_IXML_ELEMENT,
        R_FORMAT              TYPE REF TO IF_IXML_ELEMENT,
        R_BORDER              TYPE REF TO IF_IXML_ELEMENT.


  DATA: L_XML_TABLE           TYPE TABLE OF XML_LINE,
        L_XML_SIZE            TYPE I,
        L_RC                  TYPE I,
        FILE                  TYPE STRING.

*&*********************************************************************&*
*&                   SELECTION SCREEN                                  &*
*&*********************************************************************&*
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-001.
PARAMETERS:  FILE_P  TYPE RLGRAP-FILENAME OBLIGATORY.     " INPUT PARAMETER FOR FILE PATH TO DOWNLOAD EXCEL
SELECTION-SCREEN END OF BLOCK A.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE_P.
*------------------------------------------------" READ ERROR FILE PATH

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = ' '
    IMPORTING
      FILE_NAME     = FILE_P.

  FILE = FILE_P.


START-OF-SELECTION.
*------------------------------------------------" START OF SELECTION

SELECT PERNR SBMOD FROM PA0001 INTO TABLE GT_FINAL
       WHERE ENDDA EQ '99991231'
       AND   SACHA EQ ''
       AND   SACHP EQ ''
       AND   SACHZ EQ ''.

*------------------------------------------------" Excel Sheet Generation
* Creating a ixml Factory
  l_ixml = cl_ixml=>create( ).

* Creating the DOM Object Model
  l_document = l_ixml->create_document( ).

* Create Root Node 'Workbook'
  l_element_root  = l_document->create_simple_element( name = 'Workbook'  parent = l_document ).
  l_element_root->set_attribute( name = 'xmlns'  value = 'urn:schemas-microsoft-com:office:spreadsheet' ).

  ns_attribute = l_document->create_namespace_decl( name = 'ss'  prefix = 'xmlns'  uri = 'urn:schemas-microsoft-com:office:spreadsheet' ).
  l_element_root->set_attribute_node( ns_attribute ).

  ns_attribute = l_document->create_namespace_decl( name = 'x'  prefix = 'xmlns'  uri = 'urn:schemas-microsoft-com:office:excel' ).
  l_element_root->set_attribute_node( ns_attribute ).

* Create node for document properties.
  r_element_properties = l_document->create_simple_element( name = 'TEST_REPORT'  parent = l_element_root ).
  l_value = sy-uname.
  l_document->create_simple_element( name = 'Author'  value = l_value  parent = r_element_properties  ).

* Styles
  r_styles = l_document->create_simple_element( name = 'Styles'  parent = l_element_root  ).

*  Style for Header
  r_style  = l_document->create_simple_element( name = 'Style'   parent = r_styles  ).
  r_style->set_attribute_ns( name = 'ID'  prefix = 'ss'  value = 'Header' ).

  r_format  = l_document->create_simple_element( name = 'Font'  parent = r_style  ).
  r_format->set_attribute_ns( name = 'Bold'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Interior' parent = r_style  ).
  r_format->set_attribute_ns( name = 'Color'   prefix = 'ss'  value = '#92D050' ).
  r_format->set_attribute_ns( name = 'Pattern' prefix = 'ss'  value = 'Solid' ).

  r_format  = l_document->create_simple_element( name = 'Alignment'  parent = r_style  ).
  r_format->set_attribute_ns( name = 'Vertical'  prefix = 'ss'  value = 'Center' ).
  r_format->set_attribute_ns( name = 'WrapText'  prefix = 'ss'  value = '1' ).

  r_border  = l_document->create_simple_element( name = 'Borders'  parent = r_style ).
  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Bottom' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Left' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Top' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Right' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

* Style for Data
  r_style1  = l_document->create_simple_element( name = 'Style'   parent = r_styles  ).
  r_style1->set_attribute_ns( name = 'ID'  prefix = 'ss'  value = 'Data' ).

  r_border  = l_document->create_simple_element( name = 'Borders'  parent = r_style1 ).
  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Bottom' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Left' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Top' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

  r_format  = l_document->create_simple_element( name = 'Border'   parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'  prefix = 'ss'  value = 'Right' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'  value = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'  prefix = 'ss'  value = '1' ).

* Worksheet
  r_worksheet = l_document->create_simple_element( name = 'Worksheet'  parent = l_element_root ).
  r_worksheet->set_attribute_ns( name = 'Name'  prefix = 'ss'  value = 'Sheet1' ).

* Table
  r_table = l_document->create_simple_element( name = 'Table'  parent = r_worksheet ).
  r_table->set_attribute_ns( name = 'FullColumns'  prefix = 'x'  value = '1' ).
  r_table->set_attribute_ns( name = 'FullRows'     prefix = 'x'  value = '1' ).


* Column Formatting
  r_column = l_document->create_simple_element( name = 'Column'  parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'  prefix = 'ss'  value = '100' ).

  r_column = l_document->create_simple_element( name = 'Column'  parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'  prefix = 'ss'  value = '100' ).



* Blank Row
  r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).

* Column Headers Row
  r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).
  r_row->set_attribute_ns( name = 'AutoFitHeight'  prefix = 'ss'  value = '1' ).


"Personnel No
 r_cell = l_document->create_simple_element( name = 'Cell'  parent = r_row ).
 r_cell->set_attribute_ns( name = 'StyleID'  prefix = 'ss'  value = 'Header' ).
 r_data = l_document->create_simple_element( name = 'Data'  value = 'Personnel No'  parent = r_cell ).
 r_data->set_attribute_ns( name = 'Type'  prefix = 'ss' value = 'String' ).

* GROUP
 r_cell = l_document->create_simple_element( name = 'Cell'  parent = r_row ).
 r_cell->set_attribute_ns( name = 'StyleID'  prefix = 'ss'  value = 'Header' ).
 r_data = l_document->create_simple_element( name = 'Data'  value = 'Group'  parent = r_cell ).
 r_data->set_attribute_ns( name = 'Type'  prefix = 'ss' value = 'String' ).


* Data TABLE
   LOOP AT  GT_FINAL INTO GS_FINAL.

   r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).

   r_cell = l_document->create_simple_element( name = 'Cell'  parent = r_row ).
   r_cell->set_attribute_ns( name = 'StyleID'  prefix = 'ss'  value = 'Data' ).
   l_value = gs_final-pernr.
   r_data = l_document->create_simple_element( name = 'Data'  value = l_value   parent = r_cell ).
   r_data->set_attribute_ns( name = 'Type'  prefix = 'ss'  value = 'String' ).

   r_cell = l_document->create_simple_element( name = 'Cell'  parent = r_row ).
   r_cell->set_attribute_ns( name = 'StyleID'  prefix = 'ss'  value = 'Data' ).
   l_value = gs_final-sbmod.
   r_data = l_document->create_simple_element( name = 'Data'  value = l_value   parent = r_cell ).
   r_data->set_attribute_ns( name = 'Type'  prefix = 'ss'  value = 'String' ).

   CLEAR GS_FINAL.

   ENDLOOP.


* CREATING A STREAM FACTORY
            l_streamfactory = l_ixml->create_stream_factory( ).

* CONNECT INTERNAL XML TABLE TO STREAM FACTORY
            l_ostream = l_streamfactory->create_ostream_itable( table = l_xml_table ).

* RENDERING THE DOCUMENT
            l_renderer = l_ixml->create_renderer( ostream  = l_ostream  document = l_document ).
            l_rc = l_renderer->render( ).

* SAVING THE XML DOCUMENT
            l_xml_size = l_ostream->get_num_written_raw( ).

 CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = l_xml_size
        filename     = FILE
        filetype     = 'BIN'
      CHANGING
        data_tab     = l_xml_table
      EXCEPTIONS
        OTHERS       = 24.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
