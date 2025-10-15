REPORT zsd_carga_imagem.

DATA: lr_mime_rep TYPE REF TO if_mr_api.

DATA: lv_filename TYPE string.
DATA: lv_path     TYPE string.
DATA: lv_fullpath TYPE string.
DATA: lv_content  TYPE xstring.
DATA: lv_length   TYPE i.
DATA: lv_rc TYPE sy-subrc.

DATA: lt_file TYPE filetable.
DATA: ls_file LIKE LINE OF lt_file.


DATA: lt_data TYPE STANDARD TABLE OF x255.

PARAMETERS: p_path TYPE string
*           DEFAULT '/SAP/PUBLIC/AMAGGI/Pedido_Importacao.jpg'.   "<<-- Mime path, save to path
            DEFAULT '/SAP/PUBLIC/AMAGGI/Frete_Aquaviario.jpg'.   "<<-- Mime path, save to path


cl_gui_frontend_services=>file_open_dialog(
 CHANGING
   file_table              =  lt_file  " Table Holding Selected Files
   rc                      =  lv_rc  ). " Return Code, Number of Files or -1 If Error Occurred

READ TABLE lt_file INTO ls_file INDEX 1.
IF sy-subrc = 0.
  lv_filename = ls_file-filename.
ENDIF.

cl_gui_frontend_services=>gui_upload(
 EXPORTING
   filename                = lv_filename    " Name of file
   filetype                = 'BIN'
 IMPORTING
   filelength              =  lv_length   " File length
 CHANGING
   data_tab                = lt_data    " Transfer table for file contents
 EXCEPTIONS
   OTHERS                  = 19 ).

CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
  EXPORTING
    input_length = lv_length
*   first_line   = 0
*   last_line    = 0
  IMPORTING
    buffer       = lv_content
  TABLES
    binary_tab   = lt_data
  EXCEPTIONS
    failed       = 1
    OTHERS       = 2.

lr_mime_rep = cl_mime_repository_api=>if_mr_api~get_api( ).

lr_mime_rep->put(
 EXPORTING
   i_url                     = p_path
   i_content                 = lv_content
 EXCEPTIONS
   parameter_missing         = 1
   error_occured             = 2
   cancelled                 = 3
   permission_failure        = 4
   data_inconsistency        = 5
   new_loio_already_exists   = 6
   is_folder                 = 7 ).
