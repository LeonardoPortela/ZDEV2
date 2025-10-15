*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZLESC0001                                               *
* Descrição  : Carga Tabela Veículos                                   *
* Módulo     :                                   Transação:            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 30/09/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*


REPORT zlesc0001 NO STANDARD PAGE HEADING MESSAGE-ID sd.

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

  DATA: sl_line  TYPE type_line         ,
        sl_t0002 TYPE zlest0002         ,
        tl_t0002 TYPE TABLE OF zlest0002.

  LOOP AT t_line INTO sl_line.

    CLEAR sl_t0002.

    SPLIT sl_line-line AT ';' INTO sl_t0002-pc_veiculo
                                   sl_t0002-proprietario
                                   sl_t0002-cd_cidade
                                   sl_t0002-cd_uf
                                   sl_t0002-chassi
                                   sl_t0002-ano
                                   sl_t0002-ct_veiculo
                                   sl_t0002-tp_carroceria
                                   sl_t0002-cd_renavam
                                   sl_t0002-observacoes
                                   sl_t0002-irregularidade
                                   sl_t0002-agregado
                                   sl_t0002-st_bloqueio
                                   sl_t0002-tp_veiculo
                                   sl_t0002-tp_rodado
                                   sl_t0002-tp_carroceria2
                                   sl_t0002-tara
                                   sl_t0002-cap_kg
                                   sl_t0002-cap_m3
                                   sl_t0002-qt_eixo
                                   sl_t0002-status
                                   sl_t0002-erdat
                                   sl_t0002-erzet
                                   sl_t0002-ernam
                                   sl_t0002-country
                                   sl_t0002-taxjurcode
                                   sl_t0002-kalsm
                                   sl_t0002-spras.

    APPEND sl_t0002 TO tl_t0002.

    CLEAR sl_line.

  ENDLOOP.

  IF NOT tl_t0002[] IS INITIAL.
    MODIFY zlest0002 FROM TABLE tl_t0002.
    MESSAGE i836 WITH text-004.
  ENDIF.

ENDFORM.                    " Z_PROCESSA
