*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDR0004                                                *
* Descrição  : Relação de Romaneios                                    *
* Módulo     : SD                                Transação: ZSDT0023   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 05/11/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdr0004 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TYPE-POOLS icon.

TABLES: zsdt0001,
        t001w   .

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_vbak,
         vbeln TYPE vbak-vbeln,
         tknum TYPE vttk-tknum,
       END   OF type_vbak.

DATA BEGIN OF type_0001.
INCLUDE TYPE zsdt0001.
DATA vbeln_va TYPE vbeln_va.
DATA END OF type_0001.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0001  LIKE TABLE OF type_0001 ,
      t_fcat      TYPE TABLE OF lvc_s_fcat,
      t_tool      TYPE ui_functions       ,
      t_vbak      TYPE TABLE OF type_vbak .

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cont   TYPE REF TO cl_gui_custom_container,
      s_alv    TYPE REF TO cl_gui_alv_grid        ,
      s_layout TYPE lvc_s_layo                    .

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_table TYPE char10 VALUE 'T_ZSDT0001'.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
       zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
         IMPORTING
           e_object e_interactive,

       zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
         IMPORTING e_ucomm,

       zm_handle_hotspot      FOR EVENT hotspot_click OF cl_gui_alv_grid
         IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD zm_handle_hotspot.
*   Click Hotspot
    PERFORM z_handle_hotspot USING e_row_id
                                   e_column_id
                                   es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
SELECT-OPTIONS:
  s_werks FOR t001w-werks NO-EXTENSION
                          NO INTERVALS
                          OBLIGATORY,
  s_num   FOR zsdt0001-nr_romaneio ,
  s_lote  FOR zsdt0001-nr_safra    ,
  s_data  FOR zsdt0001-dt_movimento.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Seleciona dados
  PERFORM: z_seleciona_dados,
           z_monta_fieldcat .

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
*----------------------------------------------------------------------*
FORM z_handle_toolbar USING p_object      TYPE REF TO cl_alv_event_toolbar_set
                            p_interactive TYPE char1.

** Constants for button type
*  CONSTANTS:
*        c_button_normal           TYPE i VALUE 0,
*        c_menu_and_default_button TYPE i VALUE 1,
*        c_menu                    TYPE i VALUE 2,
*        c_separator               TYPE i VALUE 3,
*        c_radio_button            TYPE i VALUE 4,
*        c_checkbox                TYPE i VALUE 5,
*        c_menu_entry              TYPE i VALUE 6.
*
*  DATA sl_toolbar TYPE stb_button.
*
** Append Seperator
*  MOVE c_separator  TO sl_toolbar-butn_type.
*  APPEND sl_toolbar TO p_object->mt_toolbar.
*
** Botão Vincular NF's
*  CLEAR sl_toolbar.
*  MOVE: 'REMESSA'          TO sl_toolbar-function ,
*         icon_create       TO sl_toolbar-icon     ,
*         text-004          TO sl_toolbar-quickinfo,
*         text-004          TO sl_toolbar-text     ,
*         space             TO sl_toolbar-disabled .
*  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm.

*  CASE p_ucomm.
*    WHEN 'REMESSA'.
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat,
           t_tool.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'NR_ROMANEIO'  text-005 12 space space,
    c_table 'PARID'        text-006 10 space space,
    c_table 'BRANCH'       text-007 06 space space,
    c_table 'PLACA_CAV'    text-008 07 space space,
    c_table 'DT_MOVIMENTO' text-009 10 space space,
    c_table 'PESO_LIQ'     text-010 16 space space,
    c_table 'TP_MOVIMENTO' text-011 06 space space,
    c_table 'MATNR'        text-012 14 'X'   space,
    c_table 'TP_FRETE'     text-013 07 space space,
    c_table 'NR_SAFRA'     text-015 10 space space,
    c_table 'VBELN'        text-004 10 space 'X'  ,
    c_table 'VBELN_VA'     text-016 10 space 'X'  ,
    c_table 'DOC_REM'      text-014 10 space 'X'  ,
    c_table 'TKNUM'        text-003 10 space 'X'  .

* Monta Layout
  PERFORM z_layout.

* Deleta Botões
  PERFORM z_deleta_bot USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_table   TYPE c
                               p_field   TYPE c
                               p_desc    TYPE c
                               p_len     TYPE n
                               p_zero    TYPE c
                               p_hotspot TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-no_zero   = p_zero.
  sl_fcat-hotspot   = p_hotspot.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.
  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM z_deleta_bot USING p_bot TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_bot.
  APPEND sl_tool TO t_tool.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-014.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent          = s_cont
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-015.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  CHECK s_event IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER: s_event->zm_handle_user_command FOR s_alv,
               s_event->zm_handle_toolbar      FOR s_alv,
               s_event->zm_handle_hotspot      FOR s_alv.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_default                     = 'X'
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_zsdt0001
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                           Seleciona dados                            *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona ZSDT0001
  PERFORM: z_seleciona_zsdt0001,
* Seleciona VBAK
           z_seleciona_vbak    .

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0001                                     *
*&---------------------------------------------------------------------*
*                            Seleciona ZSDT0001                        *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0001.

  REFRESH t_zsdt0001.

  SELECT *
    FROM zsdt0001
    INTO TABLE t_zsdt0001
  WHERE  nr_romaneio  IN s_num
    AND  dt_movimento IN s_data
    AND  nr_safra     IN s_lote
    AND  branch       IN s_werks.

  IF t_zsdt0001[] IS INITIAL.
    MESSAGE i836 WITH text-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT t_zsdt0001 BY nr_romaneio  ASCENDING
                     vbeln        ASCENDING
                     dt_movimento ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZSDT0001

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT                                         *
*&---------------------------------------------------------------------*
*                               Click Hotspot                          *
*----------------------------------------------------------------------*
FORM z_handle_hotspot USING p_row_id    TYPE lvc_s_row
                            p_column_id TYPE lvc_s_col
                            p_row_no    TYPE lvc_s_roid.

  DATA sl_zsdt0001 like LINE OF t_zsdt0001.

  READ TABLE t_zsdt0001 INTO sl_zsdt0001 INDEX p_row_id.

  CASE p_column_id.
    WHEN 'VBELN'.
      CHECK NOT sl_zsdt0001-vbeln IS INITIAL.
      SET PARAMETER ID 'AUN' FIELD sl_zsdt0001-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    WHEN 'TKNUM'.
      CHECK NOT sl_zsdt0001-tknum IS INITIAL.
      SET PARAMETER ID 'TNR' FIELD sl_zsdt0001-tknum.
      CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
    WHEN 'DOC_REM'.
      CHECK NOT sl_zsdt0001-doc_rem IS INITIAL.
      SET PARAMETER ID 'VL' FIELD sl_zsdt0001-doc_rem .
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
    WHEN 'VBELN_VA'.
      CHECK NOT sl_zsdt0001-vbeln_va IS INITIAL.
      SET PARAMETER ID 'AUN' FIELD sl_zsdt0001-vbeln_va.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAK                                         *
*&---------------------------------------------------------------------*
*                       Seleciona VBAK                                 *
*----------------------------------------------------------------------*
FORM z_seleciona_vbak.

  DATA: tl_zsdt0001 LIKE TABLE OF type_0001 ,
        sl_zsdt0001 LIKE LINE OF tl_zsdt0001,
        sl_vbak     TYPE type_vbak          ,
        vl_tabix    TYPE i                  .

  REFRESH t_vbak.

  CHECK NOT t_zsdt0001[] IS INITIAL.
  tl_zsdt0001[] = t_zsdt0001[].
  SORT tl_zsdt0001 BY tknum ASCENDING.
  DELETE tl_zsdt0001 WHERE tknum IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM tl_zsdt0001 COMPARING tknum.

  CHECK NOT tl_zsdt0001[] IS INITIAL.

  SELECT vbeln tknum
    FROM vbak
    INTO TABLE t_vbak
    FOR ALL ENTRIES IN tl_zsdt0001
  WHERE  tknum EQ tl_zsdt0001-tknum.

  SORT t_vbak BY tknum ASCENDING.

  CHECK NOT t_vbak[] IS INITIAL.

  LOOP AT t_zsdt0001 INTO sl_zsdt0001.

    vl_tabix = sy-tabix.
    READ TABLE t_vbak INTO sl_vbak
      WITH KEY tknum = sl_zsdt0001-tknum
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      sl_zsdt0001-vbeln_va = sl_vbak-vbeln.
      MODIFY t_zsdt0001 FROM sl_zsdt0001
        INDEX vl_tabix
        TRANSPORTING vbeln_va.
    ENDIF.

    CLEAR: sl_zsdt0001,
           sl_vbak    .

  ENDLOOP.

ENDFORM.                    " Z_SELECIONA_VBAK
