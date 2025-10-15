*&---------------------------------------------------------------------*
*& Report  ZSDT0066
*& ***Validar Quantidade Exportada***
*&---------------------------------------------------------------------*
*& Autor  : Marcos Faneli
*& Data   : 20.05.2014
*& Empresa: Amaggi Exportação e Importação Ltda
*&---------------------------------------------------------------------*
*& Histório de Alteração
*& Autor    Data    Request     Descrição
*&---------------------------------------------------------------------*

REPORT  zsdt0066.

TYPE-POOLS: slis.

TABLES: zsdt_export,
        sscrfields.

* Estruturas
*----------------------------------------------------------------------
TYPES: BEGIN OF ty_zsdt_export.
        INCLUDE TYPE zsdt_export.
TYPES:
END OF ty_zsdt_export,

BEGIN OF ty_saida.
        INCLUDE TYPE zsdt_export.
TYPES: alt(1),
END OF ty_saida,

BEGIN OF ty_estrutura.
        INCLUDE TYPE slis_fieldcat_main.
        INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES:
END OF ty_estrutura.

* Tabelas internas
*---------------------------------------------------------------------*
DATA: it_zsdt_export TYPE TABLE OF ty_zsdt_export,
      it_saida TYPE TABLE OF ty_saida.

* Work Área
*---------------------------------------------------------------------*
DATA: wa_zsdt_export TYPE ty_zsdt_export,
      wa_saida TYPE ty_saida.

* ALV
*---------------------------------------------------------------------*
DATA: wa_evento       TYPE          slis_alv_event,
      it_layout       TYPE          slis_layout_alv,
      wa_estrutura    TYPE          ty_estrutura,
      it_estrutura    TYPE TABLE OF ty_estrutura,
      it_evento       TYPE          slis_t_event,
      grid1           TYPE REF TO   cl_gui_alv_grid,
      it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = ls_good-fieldname
          i_value     = lv_value.

    ENDLOOP.

    TRY.
        IF ls_good IS NOT INITIAL.
          CLEAR wa_saida.
          MOVE: 'X' TO wa_saida-alt.
          MODIFY it_saida FROM wa_saida INDEX ls_good-row_id TRANSPORTING alt.
        ENDIF.
      CATCH cx_root.
        MESSAGE e386(sd) WITH 'Erro atualizando dados!'.
    ENDTRY.
  ENDMETHOD.                    "ON_DATA_CHANGED
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

* Tela de filtro
*---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
"PARAMETERS      "P_DOC_N(10). "FOR ZSDT_EXPORT-DOCNUM NO-EXTENSION NO INTERVALS,
SELECT-OPTIONS:
                p_doc_n FOR zsdt_export-docnum     NO INTERVALS,
                p_nf_r  FOR zsdt_export-nf_retorno NO INTERVALS,
                p_doc_e FOR zsdt_export-ordem      NO INTERVALS,
                p_centr FOR zsdt_export-werks      NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN:  BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON (20) text-040 USER-COMMAND new.
SELECTION-SCREEN: END OF LINE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_doc_n-low.
  PERFORM onf4.

*&---------------------------------------------------------------------*
*&      Form  onf4
*&---------------------------------------------------------------------*
FORM onf4.

  DATA: it_return LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        BEGIN OF it_final OCCURS 0,
          docnum TYPE zsdt_export-docnum,
        END OF it_final.

  SELECT docnum
    FROM zsdt_export
    INTO TABLE it_final.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
      retfield               = 'DOCNUM'   "field of internal table
      value_org              = 'S'
    TABLES
      value_tab              = it_final
*   FIELD_TAB              =
      return_tab             = it_return.

  WRITE it_return-fieldval TO p_doc_n.
ENDFORM.                    "onf4

AT SELECTION-SCREEN.
  IF sscrfields-ucomm EQ 'NEW'.
    CALL SCREEN 0100
      STARTING AT 01 01
      ENDING   AT 100 10.
  ENDIF.
* Exibir os dados
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_doc_n IS INITIAL.
    MESSAGE s836(sd) WITH 'Campo Nº. Documento é obrigatório'.
  ELSE.
    PERFORM:selecionar_dados,
            organizar_dados,
            imprimir_dados.
  ENDIF.
*&--------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&--------------------------------------------------------------------*
FORM selecionar_dados .
  SELECT *
    FROM zsdt_export
    INTO TABLE it_zsdt_export
    WHERE docnum     IN p_doc_n
     AND  nf_retorno IN p_nf_r
     AND  ordem      IN p_doc_e
     AND  werks      IN p_centr.
ENDFORM.                    " SELECIONAR_DADOS


*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM organizar_dados.
  REFRESH: it_saida.

  LOOP AT it_zsdt_export INTO wa_zsdt_export.
    CLEAR: wa_saida.
    MOVE-CORRESPONDING: wa_zsdt_export TO wa_saida.
    APPEND wa_saida TO it_saida.
  ENDLOOP.
ENDFORM.                    "ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
FORM imprimir_dados .
  PERFORM: definir_eventos,
           montar_layout,
           definir_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     i_callback_program = sy-repid
     it_fieldcat        = it_estrutura[]
     is_layout          = it_layout
*     I_SAVE             = 'A'
     it_events          = it_evento
*     IS_PRINT           = T_PRINT
    TABLES
      t_outtab          = it_saida
    EXCEPTIONS
      program_error     = 1.
ENDFORM.                    " IMPRIMIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
FORM definir_eventos .
  CLEAR wa_evento.
  wa_evento-name = slis_ev_pf_status_set.
  wa_evento-form = 'CRIAR_OBJETOS'.
  APPEND wa_evento TO it_evento.

  CLEAR wa_evento.
  wa_evento-name = slis_ev_user_command.
  wa_evento-form = 'USER_COMMAND'.
  APPEND wa_evento TO it_evento.
ENDFORM.                    " DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM montar_layout .
  REFRESH:it_estrutura.

  PERFORM montar_estrutura USING:
        1 ''              ''               'IT_SAIDA'   'DOCNUM'        'Nº documento'   ' '   ' ',
        1 ''              ''               'IT_SAIDA'   'WERKS'         'Centro'         ' '   ' ',
        1 'ZSDT_EXPORT'   'NF_RETORNO'     'IT_SAIDA'   'NF_RETORNO'    ' '              ' '   'X',
        1 'ZSDT_EXPORT'   'ORDEM'          'IT_SAIDA'   'ORDEM'         ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'DATA_CRIACAO'   'IT_SAIDA'   'DATA_CRIACAO'  ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'QUANT'          'IT_SAIDA'   'QUANT'         ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'VALOR_TOTAL'    'IT_SAIDA'   'VALOR_TOTAL'   ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'EXPORT'         'IT_SAIDA'   'EXPORT'        ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'MATNR'          'IT_SAIDA'   'MATNR'         ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'STATUS'         'IT_SAIDA'   'STATUS'        ' '   ' '              'X',
        1 'ZSDT_EXPORT'   'FINALIDADE'     'IT_SAIDA'   'FINALIDADE'    ' '   ' '              'X'.
ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura  USING value(p_col_pos)       TYPE i
                             value(p_ref_tabname)   LIKE dd02d-tabname
                             value(p_ref_fieldname) LIKE dd03d-fieldname
                             value(p_tabname)       LIKE dd02d-tabname
                             value(p_field)         LIKE dd03d-fieldname
                             value(p_scrtext_l)     LIKE dd03p-scrtext_l
                             value(p_outputlen)
                             value(p_edit).

  CLEAR wa_estrutura.

  wa_estrutura-edit          = p_edit.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  IF p_field = 'EXPORT'.
    wa_estrutura-checkbox = 'X'.
  ENDIF.

  APPEND wa_estrutura TO it_estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*----------------------------------------------------------------------*
FORM criar_objetos USING ucomm TYPE kkblo_t_extab.
  DATA: gr_events       TYPE REF TO lcl_event_handler,
        ls_sel_hide     TYPE slis_sel_hide_alv,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = grid1.

  CALL METHOD grid1->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD grid1->register_edit_event "Habilita tecla enter
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD grid1->register_edit_event "Habilita alteração na grid(Clique no checkbox)
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  CREATE OBJECT gr_events.
  SET HANDLER: gr_events->on_data_changed FOR grid1.

  SET PF-STATUS 'STANDARD_FULLSCREEN'." EXCLUDING TL_FCODE.
ENDFORM.                    "CRIAR_OBJETOS

*&---------------------------------------------------------------------*
*&      Form  DEFINIR_LAYOUT
*&---------------------------------------------------------------------*
FORM definir_layout .
  it_layout-zebra = 'X' .
  it_layout-colwidth_optimize = 'X'.
ENDFORM.                    " DEFINIR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.
  CASE ucomm.
    WHEN '&DATA_SAVE'.
      REFRESH: it_zsdt_export.
      LOOP AT it_saida INTO wa_saida WHERE alt = 'X'.
        MOVE-CORRESPONDING wa_saida TO wa_zsdt_export.
        APPEND wa_zsdt_export TO it_zsdt_export.
      ENDLOOP.
      MODIFY zsdt_export FROM TABLE it_zsdt_export.
      MESSAGE s836(sd) WITH 'As alterações foram salvas!'.
    WHEN '&NEW'.
      CALL SCREEN 0100
        STARTING AT 01 01
        ENDING   AT 100 10.
  ENDCASE.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: wl_zsdt_export TYPE zsdt_export.

  SET PF-STATUS 'STATUS_POPUP'.
  CASE sy-ucomm.
    WHEN '&CLOSE'.
      LEAVE TO SCREEN 0.
    WHEN '&CREATE'.
      CLEAR: wa_zsdt_export.

      wa_zsdt_export-docnum       = zsdt_export-docnum.
      wa_zsdt_export-werks        = zsdt_export-werks.
      wa_zsdt_export-nf_retorno   = zsdt_export-nf_retorno.
      wa_zsdt_export-ordem        = zsdt_export-ordem.
      wa_zsdt_export-data_criacao = zsdt_export-data_criacao.
      wa_zsdt_export-quant        = zsdt_export-quant.
      wa_zsdt_export-valor_total  = zsdt_export-valor_total.
      wa_zsdt_export-export       = zsdt_export-export.
      wa_zsdt_export-matnr        = zsdt_export-matnr.
      wa_zsdt_export-status       = zsdt_export-status.
      wa_zsdt_export-finalidade   = zsdt_export-finalidade.

      MODIFY zsdt_export FROM wa_zsdt_export.

      CLEAR: zsdt_export-docnum,
             zsdt_export-werks,
             zsdt_export-nf_retorno,
             zsdt_export-ordem,
             zsdt_export-data_criacao,
             zsdt_export-quant,
             zsdt_export-valor_total,
             zsdt_export-export,
             zsdt_export-matnr,
             zsdt_export-status,
             zsdt_export-finalidade.

      MESSAGE 'Registro inserido com sucesso!' TYPE 'I'.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " STATUS_0100  OUTPUT
