*&---------------------------------------------------------------------*
*& Report  ZMMCESTOQUE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zfiaacabavn NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                 type-pools                           *
*----------------------------------------------------------------------*
TYPE-POOLS icon.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_line,
         line TYPE char600,
       END   OF type_line.

TYPES: BEGIN OF tp_messagem.
        INCLUDE STRUCTURE bdcmsgcoll.
TYPES: icone  TYPE c LENGTH 4.
TYPES: END OF tp_messagem.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_line TYPE TABLE OF type_line.

DATA:
* Tabela de mapeamento de tela da transação do BI
      t_bdc      TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      v_mode     TYPE c VALUE 'N',
      t_messtab  TYPE TABLE OF bdcmsgcoll,
      w_messtab  TYPE bdcmsgcoll,
      ok_code    TYPE sy-ucomm.

DATA: t_messtab2 TYPE TABLE OF tp_messagem INITIAL SIZE 0 WITH HEADER LINE,
      w_messtab2 TYPE tp_messagem.

DATA: itab TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

CONTROLS: tab_losg TYPE TABLEVIEW USING SCREEN '0001'.
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
          bukrs    TYPE bukrs,
          anln1    TYPE anln1,
          anln2    TYPE anln2,
          dtdoc	   TYPE c LENGTH 10,
          dtlac	   TYPE c LENGTH 10,
          dtref	   TYPE c LENGTH 10,
          texto    TYPE string,
          monat    TYPE c LENGTH 2,
          blart    TYPE c LENGTH 2,
          prozs    TYPE string,
          xaalt    TYPE c LENGTH 1,
          xaneu    TYPE c LENGTH 1,
        END OF wa_t0002,

        tl_t0002 LIKE STANDARD TABLE OF wa_t0002.

  LOOP AT t_line INTO sl_line.
    CLEAR wa_t0002.
    SPLIT sl_line-line AT ';' INTO wa_t0002-bukrs
                                   wa_t0002-anln1
                                   wa_t0002-anln2
                                   wa_t0002-dtdoc
                                   wa_t0002-dtlac
                                   wa_t0002-dtref
                                   wa_t0002-texto
                                   wa_t0002-monat
                                   wa_t0002-blart
                                   wa_t0002-prozs
                                   wa_t0002-xaalt
                                   wa_t0002-xaneu.
    APPEND wa_t0002 TO tl_t0002.
    CLEAR sl_line.
  ENDLOOP.

  CLEAR t_messtab2[].

  LOOP AT tl_t0002 INTO wa_t0002.

    CLEAR: t_bdc[], t_messtab.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_t0002-anln1
      IMPORTING
        output = wa_t0002-anln1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_t0002-anln2
      IMPORTING
        output = wa_t0002-anln2.

    PERFORM f_bdc_field USING: 'X' 'SAPLAMDP'	        '0100',
                               ' ' 'BDC_OKCODE'	      '/ENEWC'.

    PERFORM f_bdc_field USING: 'X' 'SAPLSPO4'  	      '0300',
                               ' ' 'BDC_CURSOR'	      'SVALD-VALUE(01)',
                               ' ' 'BDC_OKCODE'	      '=FURT',
                               ' ' 'SVALD-VALUE(01)'  wa_t0002-bukrs.

    PERFORM f_bdc_field USING: 'X' 'SAPLAMDP'	        '0100',
                               ' ' 'BDC_OKCODE'	      '=TAB02',
                               ' ' 'RAIFP2-ANLN1'	    wa_t0002-anln1,
                               ' ' 'RAIFP2-ANLN2'	    wa_t0002-anln2,
                               ' ' 'RAIFP1-BLDAT'	    wa_t0002-dtlac,
                               ' ' 'RAIFP1-BUDAT'	    wa_t0002-dtdoc,
                               ' ' 'RAIFP1-BZDAT'	    wa_t0002-dtref,
                               ' ' 'BDC_CURSOR'	      'RAIFP2-SGTXT',
                               ' ' 'RAIFP2-SGTXT'	    wa_t0002-texto.

    PERFORM f_bdc_field USING: 'X' 'SAPLAMDP'	        '0100',
                               ' ' 'BDC_OKCODE'	      '=TAB03',
                               ' ' 'RAIFP2-MONAT'	    wa_t0002-monat,
                               ' ' 'BDC_CURSOR'	      'RAIFP1-BLART',
                               ' ' 'RAIFP1-BLART'	    wa_t0002-blart.

    PERFORM f_bdc_field USING: 'X' 'SAPLAMDP'	        '0100',
                               ' ' 'BDC_OKCODE'	      '=SAVE',
                               ' ' 'BDC_CURSOR'	      'RAIFP2-XAALT',
                               ' ' 'RAIFP2-PROZS'	    wa_t0002-prozs.

    IF wa_t0002-xaalt IS NOT INITIAL.
      PERFORM f_bdc_field USING: ' ' 'RAIFP2-XAALT'	'X'.
    ELSEIF wa_t0002-xaneu IS NOT INITIAL.
      PERFORM f_bdc_field USING: ' ' 'RAIFP2-XANEU'	'X'.
    ENDIF.

    CALL TRANSACTION 'ABAVN' USING t_bdc MODE 'N' UPDATE 'S' MESSAGES INTO t_messtab.

    LOOP AT t_messtab INTO w_messtab.

      MOVE-CORRESPONDING w_messtab TO w_messtab2.

      IF w_messtab2-msgtyp EQ 'S'.
        w_messtab2-icone = icon_hint.
      ELSEIF w_messtab2-msgtyp EQ 'W'.
        w_messtab2-icone = icon_warning.
      ELSEIF w_messtab2-msgtyp EQ 'E'.
        w_messtab2-icone = icon_alert.
      ENDIF.

      APPEND w_messtab2 TO t_messtab2.

    ENDLOOP.

  ENDLOOP.

  IF t_messtab2[] IS NOT INITIAL.
    PERFORM mostra_log.
  ENDIF.

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

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostra_log .
  CALL SCREEN 0001 STARTING AT 10 05.
ENDFORM.                    " MOSTRA_LOG

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code.
    WHEN 'QUIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'PFLOG'.
  SET TITLEBAR 'TLLOG'.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_LOSG_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_losg_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_messtab2 LINES tab_losg-lines.
ENDMODULE.                 " TAB_LOSG_CHANGE_TC_ATTR  OUTPUT
