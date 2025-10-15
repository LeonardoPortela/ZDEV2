*&---------------------------------------------------------------------*
*& Report  ZMMCESTOQUE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmmcmr22 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_line,
         line TYPE char600,
       END   OF type_line.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_line TYPE TABLE OF type_line.

DATA:
* Tabela de mapeamento de tela da transação do BI
      t_bdc     TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      v_mode    TYPE c VALUE 'N',
      t_messtab TYPE TABLE OF bdcmsgcoll.

DATA: itab TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_filetxt   TYPE char26 VALUE 'Files CSV (*.CSV)|*.CSV|'  ,
           c_inicial   TYPE char3  VALUE 'C:\'                       ,
           c_x         TYPE char1  VALUE 'X'                         .

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
PARAMETERS
  p_file   TYPE sdba_a_nam OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

* Ajuda Pesquiza Campo Filename
  PERFORM z_ajuda_filename.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Upload do Arquivo
  PERFORM: z_upload  ,
* Processa Arquivo
           z_processa.

*&---------------------------------------------------------------------*
*&      Form  Z_AJUDA_FILENAME                                         *
*&---------------------------------------------------------------------*
*                      Ajuda Pesquiza Campo Filename                   *
*----------------------------------------------------------------------*
FORM z_ajuda_filename.

  DATA: vl_title   TYPE string    ,
        vl_filter  TYPE string    ,
        vl_initial TYPE string    ,
        vl_rc      TYPE i         ,
        tl_file    TYPE filetable .

  vl_title   = text-002.
  vl_filter  = c_filetxt.
  vl_initial = c_inicial.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = vl_title
      file_filter             = vl_filter
      initial_directory       = vl_initial
    CHANGING
      file_table              = tl_file
      rc                      = vl_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  READ TABLE tl_file INTO p_file INDEX 1.

ENDFORM.                    " Z_AJUDA_FILENAME


*&---------------------------------------------------------------------*
*&      Form  Z_UPLOAD                                                 *
*&---------------------------------------------------------------------*
*                              Upload Arquivo                          *
*----------------------------------------------------------------------*
FORM z_upload.

  DATA vl_file TYPE string.

  REFRESH t_line.

  vl_file = p_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = vl_file
      filetype                = 'ASC'
    CHANGING
      data_tab                = t_line
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
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  DELETE t_line INDEX 1.

  IF t_line[] IS INITIAL.
    MESSAGE i836 WITH text-003.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA                                               *
*&---------------------------------------------------------------------*
*                             Processa Arquivo                         *
*----------------------------------------------------------------------*
FORM z_processa.

  DATA: sl_line  TYPE type_line,
        BEGIN OF wa_t0002,
          budat	   TYPE c LENGTH 10,
          bukrs    TYPE bukrs,
          werks    TYPE werks,
          bktxt    TYPE bktxt,
          matnr    TYPE matnr,
          zuumb    TYPE string,
        END OF wa_t0002,
        tl_t0002 LIKE STANDARD TABLE OF wa_t0002.

  LOOP AT t_line INTO sl_line.

    CLEAR wa_t0002.

    SPLIT sl_line-line AT ';' INTO wa_t0002-budat
                                   wa_t0002-bukrs
                                   wa_t0002-werks
                                   wa_t0002-bktxt
                                   wa_t0002-matnr
                                   wa_t0002-zuumb.

    APPEND wa_t0002 TO tl_t0002.

    CLEAR sl_line.

  ENDLOOP.

  LOOP AT tl_t0002 INTO wa_t0002.

    CLEAR: t_bdc[], t_messtab.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_t0002-matnr
      IMPORTING
        output       = wa_t0002-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    PERFORM f_bdc_field USING: 'X' 'SAPRCKM_MR22'	            '0201',
                               ' ' 'BDC_CURSOR'	              'MR21HEAD-BKTXT',
                               ' ' 'BDC_OKCODE'	              '=ENTR',
                               ' ' 'MR21HEAD-BUDAT'           wa_t0002-budat,
                               ' ' 'MR21HEAD-BUKRS'	          wa_t0002-bukrs,
                               ' ' 'MR21HEAD-WERKS'	          wa_t0002-werks,
                               ' ' 'MR21HEAD-BKTXT'	          wa_t0002-bktxt.

    PERFORM f_bdc_field USING: 'X' 'SAPRCKM_MR22'	            '0201',
                               ' ' 'BDC_OKCODE'	              '=ENTR',
                               ' ' 'BDC_SUBSCR'	              'SAPRCKM_MR22',
                               ' ' 'BDC_CURSOR'	              'CKI_MR22_0250-ZUUMB(01)',
                               ' ' 'CKI_MR22_0250-MATNR(01)'  wa_t0002-matnr,
                               ' ' 'CKI_MR22_0250-ZUUMB(01)'  wa_t0002-zuumb.

    PERFORM f_bdc_field USING: 'X' 'SAPRCKM_MR22'	            '0201',
                               ' ' 'BDC_OKCODE'               '=SAVE'.

    CALL TRANSACTION 'MR22' USING t_bdc MODE 'N' UPDATE 'S' MESSAGES INTO t_messtab.

  ENDLOOP.

ENDFORM.                    " Z_PROCESSA

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    value(p_flag)
                           value(p_fnam)
                           value(p_fval).

  CLEAR t_bdc.
  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.
  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD
