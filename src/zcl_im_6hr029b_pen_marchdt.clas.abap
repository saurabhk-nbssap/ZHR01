class ZCL_IM_6HR029B_PEN_MARCHDT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6HR029B_PEN_MARCHDT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_HR_IN_PF_REP_MARCH .
protected section.
*"* protected components of class ZCL_IM_6HR029B_PEN_MARCHDT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6HR029B_PEN_MARCHDT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6HR029B_PEN_MARCHDT IMPLEMENTATION.


method IF_EX_HR_IN_PF_REP_MARCH~GET_MARCH_DATA.
endmethod.


method IF_EX_HR_IN_PF_REP_MARCH~GET_MARCH_DATA_PEN.
DATA: pathname TYPE string,
       filename TYPE rlgrap-filename,
       file_length TYPE i.

DATA:  tmp_filename TYPE rlgrap-filename,
       ftype TYPE char10.

* Get the file name
      clear pathname.
*      perform get_filename using filename 'O' changing pathname.

      call function 'WS_FILENAME_GET'
           exporting
                def_filename     = filename
                def_path         = pathname
                mask             = ',*.*,*.*.'
                mode             = 'O'
           importing
                filename         = tmp_filename
           exceptions
                inv_winsys       = 01
                no_batch         = 02
                selection_cancel = 03
                selection_error  = 04.

      if sy-subrc = 0.
        pathname = tmp_filename.
      endif.


      ftype = 'DAT'.
*     ftype = 'ASC'.
* Upload the data from the mentioned file name
      if not pathname is initial.
*        perform upload using pathname ftype file_length.

*      call function 'WS_UPLOAD'
*           exporting
*                codepage                = 'IBM'
*                filename                = pathname
*                filetype                = ftype
*                user_form               = ' '
*                user_prog               = ' '
*                dat_d_format            = ' '
*           importing
*                filelength              = file_length
*           tables
*                data_tab                = user_temp_tab
*           exceptions
*                conversion_error        = 1
*                file_open_error         = 2
*                file_read_error         = 3
*                invalid_type            = 4
*                no_batch                = 5
*                unknown_error           = 6
*                invalid_table_width     = 7
*                gui_refuse_filetransfer = 8
*                customer_error          = 9
*                others                  = 10.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          FILENAME                      = pathname
           FILETYPE                      = ftype
*          HAS_FIELD_SEPARATOR           = ' '
*          HEADER_LENGTH                 = 0
*          READ_BY_LINE                  = 'X'
*          DAT_MODE                      = ' '
*          CODEPAGE                      = 'IBM'
        IMPORTING
         FILELENGTH                    = file_length
*         HEADER                        =
        TABLES
          DATA_TAB                      = user_temp_tab
       EXCEPTIONS
          FILE_OPEN_ERROR               = 1
          FILE_READ_ERROR               = 2
          NO_BATCH                      = 3
          GUI_REFUSE_FILETRANSFER       = 4
          INVALID_TYPE                  = 5
          NO_AUTHORITY                  = 6
          UNKNOWN_ERROR                 = 7
          BAD_DATA_FORMAT               = 8
          HEADER_NOT_ALLOWED            = 9
          SEPARATOR_NOT_ALLOWED         = 10
          HEADER_TOO_LONG               = 11
          UNKNOWN_DP_ERROR              = 12
          ACCESS_DENIED                 = 13
          DP_OUT_OF_MEMORY              = 14
          DISK_FULL                     = 15
          DP_TIMEOUT                    = 16
          OTHERS                        = 17.
      endif.

endmethod.
ENDCLASS.
