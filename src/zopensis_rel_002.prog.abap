************************************************************************
* Program        : ZOPENSIS_REL_002                                    *
* Transaction    : ZOPENSIS_003                                        *
* Title          : Relatório de Histórico de Justificativas            *
* Developer      : Fernando Oliveira                                   *
* Date           : 07/07/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     07/07/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
REPORT  zopensis_rel_002.

*======================================================================*
*        T  A  B  L  E  S                                              *
*======================================================================*
TABLES: ztopensis_001, sscrfields.

*======================================================================*
*         T   Y   P   E   S                                            *
*======================================================================*

*======================================================================*
*         S  T  R  U  C  T  U  R  E  S                                 *
*======================================================================*

*======================================================================*
*        T  A  B  L  E  S                                              *
*======================================================================*
DATA: t_log     TYPE TABLE OF ztopensis_001 WITH HEADER LINE,
      t_log_all TYPE TABLE OF ztopensis_001,
      t_out     TYPE TABLE OF ztopensis_001.

*======================================================================*
*        W O R K - A R E A S                                           *
*======================================================================*
DATA: w_out TYPE ztopensis_001,
      w_log TYPE ztopensis_001.

*======================================================================*
*        R A N G E S                                                   *
*======================================================================*


*======================================================================*
*        C  O  N  S  T  A  N  T  S                                     *
*======================================================================*


*======================================================================*
*        F. S  Y  M  B  O  L  S                                        *
*======================================================================*


*======================================================================*
*        V  A  R  I  A  N  T  S                                        *
*======================================================================*
DATA: gc_error ,
      gv_auth  TYPE c,
      gc_lines TYPE c LENGTH 08,
      vi_lines TYPE i.

DATA: t_fieldcat     TYPE lvc_t_fcat,
      t_fieldcat_bkp TYPE lvc_t_fcat.

"Variáveis para o ALV
DATA: w_fieldcat          TYPE        lvc_s_fcat,
      w_variant           TYPE        disvariant,
      g_custom_container1 TYPE REF TO cl_gui_custom_container,
      g_grid1             TYPE REF TO cl_gui_alv_grid,
      g_container1        TYPE        scrfname VALUE 'CONTAINER1',
      g_layout1           TYPE        lvc_s_layo,
      gs_f4               TYPE        lvc_s_f4                    , "F4
      gt_f4               TYPE        lvc_t_f4                    . "F4

*======================================================================*
*        C  L  A  S  S                                                 *
*======================================================================*
*======================================================================*
*  CLASS lcl_event_handler IMPLEMENTATION                              *
*======================================================================*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      "Overrinding Standard Functions.
      handle_before_user_command   FOR EVENT before_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      "Overrinding Standard Functions.
      handle_after_user_command    FOR EVENT after_user_command  OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_before_user_command.

    CASE e_ucomm.
      WHEN '&XXL' OR '&PC'.

        t_fieldcat_bkp[] = t_fieldcat[].

        LOOP AT t_fieldcat INTO w_fieldcat.
          w_fieldcat-checkbox = ''.
          MODIFY t_fieldcat FROM w_fieldcat INDEX sy-tabix.
        ENDLOOP.

        CALL METHOD g_grid1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcat.

        CALL METHOD g_grid1->refresh_table_display
          EXPORTING
            i_soft_refresh = 'X'.

    ENDCASE.

  ENDMETHOD.                    "handle_before_user_command

  METHOD handle_after_user_command.

    CASE e_ucomm.
      WHEN '&XXL' OR '&PC'.
        t_fieldcat[] = t_fieldcat_bkp[].

        CALL METHOD g_grid1->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcat[].

        CALL METHOD g_grid1->refresh_table_display
          EXPORTING
            i_soft_refresh = 'X'.

    ENDCASE.

  ENDMETHOD.                    "handle_after_user_command

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: event_receiver       TYPE REF TO lcl_event_receiver.

*======================================================================*
*        A L V                                                         *
*======================================================================*
DATA: o_colu        TYPE REF TO cl_salv_column,
      o_alv         TYPE REF TO cl_salv_table,
      o_columns     TYPE REF TO cl_salv_columns_table,
      o_column      TYPE REF TO cl_salv_column_table,
      gr_sorts      TYPE REF TO cl_salv_sorts,
      gr_display    TYPE REF TO cl_salv_display_settings,
      o_grid        TYPE REF TO cl_salv_form_layout_grid,
      o_grid_end    TYPE REF TO cl_salv_form_layout_grid,
      o_content     TYPE REF TO cl_salv_form_element,
      o_content_end TYPE REF TO cl_salv_form_element.

DATA: t_sort  TYPE lvc_t_sort,
      fs_sort TYPE lvc_s_sort.

*======================================================================*
*        S  C  R  E  E  N                                              *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

"Data
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (50) text-t01.
SELECT-OPTIONS: sc_data  FOR ztopensis_001-data_avaliacao  .
SELECTION-SCREEN END OF LINE.

"Hora
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (50) text-t02.
SELECT-OPTIONS: sc_hora  FOR ztopensis_001-hora_avaliazao  .
SELECTION-SCREEN END OF LINE.

"Usuário
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (50) text-t03.
SELECT-OPTIONS: sc_usro  FOR ztopensis_001-avaliador       .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

*======================================================================*
*        I  N  I  T  I  A  L  I  Z  A  T  I  O  N                      *
*======================================================================*

*======================================================================*
*        S  E  L  .      S  C  R  E  E  N                              *
*======================================================================*
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*  PERFORM zf_listbox USING 'PC_LJ_RE'.

*======================================================================*
*        M  A  I  N       P  R  O  C  E  S  S  I  N  G                 *
*======================================================================*
START-OF-SELECTION.

  PERFORM z_sel_dados.

  CHECK gc_error IS INITIAL.

  PERFORM z_org_dados.

  CHECK gc_error IS INITIAL.

  PERFORM z_exibe_relatorio.

END-OF-SELECTION.
*======================================================================*
*        F  O  R  M  S                                                 *
*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  Z_SEL_DADOS                                              *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM z_sel_dados .

  CLEAR gc_error.

  SELECT *
    FROM ztopensis_001
    INTO TABLE t_log
    WHERE data_avaliacao IN sc_data AND
          hora_avaliazao IN sc_hora AND
          avaliador      IN sc_usro.

ENDFORM.                    " Z_SEL_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_ORG_DADOS                                              *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM z_org_dados .

*-------------------------------------------------------*
* Tratar os dados de Log
*-------------------------------------------------------*
  LOOP AT t_log.
    MOVE-CORRESPONDING t_log TO w_out.
    APPEND w_out TO t_out.
    CLEAR w_out.
  ENDLOOP.

ENDFORM.                    " Z_ORG_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_RELATORIO                                        *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM z_exibe_relatorio .

  IF t_out[] IS NOT INITIAL.

    SORT t_out BY data_avaliacao DESCENDING hora_avaliazao DESCENDING avaliador ASCENDING.
    DELETE ADJACENT DUPLICATES FROM t_out COMPARING ALL FIELDS.
    CALL SCREEN 9000.

  ENDIF.

ENDFORM.                    " Z_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_CUSTOM_ALV                                            *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM zf_custom_alv .

  PERFORM zf_append_fieldcat USING:
 "01              02      03       04   05 06 07 08  09 10 11   12  13  14  15
 'DATA_AVALIACAO' 'T_OUT' text-c01 '08' '' '' '' 'C' '' '' '1'  ' ' ' ' ' ' '', "Data
 'HORA_AVALIAZAO' 'T_OUT' text-c02 '08' '' '' '' 'C' '' '' '2'  ' ' ' ' ' ' '', "Hora
 'AVALIADOR'      'T_OUT' text-c03 '12' '' '' '' 'L' '' '' '3'  ' ' ' ' ' ' '', "Avaliador
 'STATUS'         'T_OUT' text-c20 '06' '' '' '' 'L' '' '' '3'  ' ' ' ' ' ' '', "Status
 'CHANGENR'       'T_OUT' text-c04 '12' '' '' '' 'L' '' '' '4'  ' ' ' ' ' ' '', "N° Documento
 'TABKEY'         'T_OUT' text-c19 '18' '' '' '' 'L' '' '' '4'  ' ' ' ' ' ' '', "Doc. Rastreio
 'TABNAME'        'T_OUT' text-c05 '05' '' '' '' 'L' '' '' '5'  ' ' ' ' ' ' '', "Tabela
 'FNAME'          'T_OUT' text-c06 '06' '' '' '' 'L' '' '' '6'  ' ' ' ' ' ' '', "Campo
 'BNAME'          'T_OUT' text-c07 '12' '' '' '' 'L' '' '' '7'  ' ' ' ' ' ' '', "Usuário
 'NAME_TEXT'      'T_OUT' text-c08 '25' '' '' '' 'L' '' '' '8'  ' ' ' ' ' ' '', "Nome Completo Usuário
 'DEPARTMENT'     'T_OUT' text-c09 '18' '' '' '' 'L' '' '' '9'  ' ' ' ' ' ' '', "Departamento
 'TCODE'          'T_OUT' text-c10 '12' '' '' '' 'L' '' '' '10' ' ' ' ' ' ' '', "Transação
 'TTEXT'          'T_OUT' text-c11 '17' '' '' '' 'L' '' '' '11' ' ' ' ' ' ' '', "Texto da Transação
 'UDATE'          'T_OUT' text-c12 '08' '' '' '' 'C' '' '' '12' ' ' ' ' ' ' '', "Data Edição
 'UTIME'          'T_OUT' text-c13 '08' '' '' '' 'C' '' '' '13' ' ' ' ' ' ' '', "Hora Edição
 'DDTEXT'         'T_OUT' text-c14 '15' '' '' '' 'L' '' '' '14' ' ' ' ' ' ' '', "Texto
 'VALOR_OLD'      'T_OUT' text-c15 '15' '' '' '' 'L' '' '' '15' ' ' ' ' ' ' '', "Valor Antigo
 'VALOR_NEW'      'T_OUT' text-c16 '15' '' '' '' 'L' '' '' '16' ' ' ' ' ' ' '', "Valor Novo
 'JUSTIFICAVEL_S' 'T_OUT' text-c17 '12' '' '' '' 'C' '' '' '17' ' ' ' ' ' ' '', "Justificável Sim
 'JUSTIFICAVEL_N' 'T_OUT' text-c18 '12' '' '' '' 'C' '' '' '18' ' ' ' ' ' ' ''. "Justificável Não

ENDFORM.                    " ZF_CUSTOM_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

  PERFORM zf_custom_alv.
  PERFORM zf_sort_alv.
  PERFORM zf_display_alv.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN  'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_display_alv .

  IF g_custom_container1 IS INITIAL.

    CREATE OBJECT g_custom_container1
      EXPORTING
        container_name = g_container1.

    CREATE OBJECT g_grid1
      EXPORTING
        i_parent = g_custom_container1.

    g_layout1-zebra       = 'X'    .
    g_layout1-sel_mode    = 'X'    .
    g_layout1-no_f4       = space  .
    g_layout1-stylefname  = 'CELLTAB'.
    DESCRIBE TABLE t_out LINES vi_lines.
    gc_lines = vi_lines.
    CONDENSE gc_lines NO-GAPS.
    CONCATENATE 'Total de'
                'Registros Encontrados:'
                gc_lines
                INTO g_layout1-grid_title  SEPARATED BY ' '.

    w_variant-report = sy-repid.

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_before_user_command FOR g_grid1. "EXCEL
    SET HANDLER event_receiver->handle_after_user_command  FOR g_grid1. "EXCEL

    CALL METHOD g_grid1->set_table_for_first_display
      EXPORTING
        i_save          = 'A'
        i_default       = 'X'
        is_layout       = g_layout1
        is_variant      = w_variant
      CHANGING
        it_fieldcatalog = t_fieldcat[]
        it_outtab       = t_out[]
        it_sort         = t_sort.

    CALL METHOD g_grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid1->set_toolbar_interactive.

  ENDIF.

ENDFORM.                    " ZF_DISPLAY_ALV
*  &---------------------------------------------------------------------*
*  &      Form  ZF_APPEND_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
FORM zf_append_fieldcat USING p_fieldname  "01-Nome do Campo
                              p_tabname    "02-Nome da Tabela Interna
                              p_coltext    "03-Texto Coluna
                              p_outputlen  "04-Largura da Coluna
                              p_rollname   "05-Elemento de Dados
                              p_inttype    "06-Tipo de Dados
                              p_f4         "07-Ação do F4
                              p_just       "08-Alinhar Coluna
                              p_hotspot    "09-Ação do Click
                              p_no_out     "10-Não Saida no Relatório
                              p_position   "11-Posição da Coluna
                              p_ref_field  "12-Referencia Standard Campo
                              p_ref_table  "13-Referencia Standard Tabela
                              p_checkbox   "14-Checkbox
                              p_convexit . "15-Exit de conversão

  CLEAR w_fieldcat.
  w_fieldcat-fieldname        = p_fieldname. "01-Nome do Campo
  w_fieldcat-tabname          = p_tabname  . "02-Nome da Tabela Interna
  w_fieldcat-coltext          = p_coltext  . "03-Texto Coluna
  w_fieldcat-outputlen        = p_outputlen. "04-Largura da Coluna
  w_fieldcat-rollname         = p_rollname . "05-Elemento de Dados
  w_fieldcat-inttype          = p_inttype  . "06-Typo de Dados
  w_fieldcat-f4availabl       = p_f4       . "07-Ação do F4
  w_fieldcat-just             = p_just     . "08-Alinhar Coluna
  w_fieldcat-hotspot          = p_hotspot  . "09-Ação do Click
  w_fieldcat-no_out           = p_no_out   . "10-Não Saida no Relatório
  w_fieldcat-col_pos          = p_position . "11-Posição da Coluna
  w_fieldcat-ref_field        = p_ref_field. "12-Referencia Standard Campo
  w_fieldcat-ref_table        = p_ref_table. "13-Referencia Standard Tabela
  w_fieldcat-checkbox         = p_checkbox . "14-Checkbox
  w_fieldcat-convexit         = p_convexit.  "15-Exit de conversão

  APPEND w_fieldcat TO t_fieldcat    .

ENDFORM.                    " ZF_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_sort_alv .

  fs_sort-spos      = '1'.
  fs_sort-fieldname = 'DATA_AVALIACAO'.
  fs_sort-down      = 'X'.
*  fs_sort-subtot    = 'X'.
  APPEND fs_sort TO t_sort.
  CLEAR fs_sort.

  fs_sort-spos      = '2'.
  fs_sort-fieldname = 'HORA_AVALIAZAO'.
  fs_sort-down      = 'X'.
  APPEND fs_sort TO t_sort.
  CLEAR fs_sort.

ENDFORM.
