*&---------------------------------------------------------------------*
*& Report  ZLESR0134
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0135 MESSAGE-ID zles.

CLASS lcl_alv_toolbar DEFINITION DEFERRED.
CLASS lcl_event_handler DEFINITION DEFERRED.

DATA: dg_splitter          TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer      TYPE REF TO cl_gui_container,
      ctl_cccontainer_html TYPE REF TO cl_gui_container,
      ctl_alv              TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog      TYPE lvc_t_fcat,
      gs_variant           TYPE disvariant,
      gs_layout            TYPE lvc_s_layo,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      obj_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      lc_open_browser      TYPE char01,
      event_handler        TYPE REF TO lcl_event_handler.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor  IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_execute_object.
    ty_toolbar-function  = 'CONSULTAR'.
    ty_toolbar-quickinfo = text-004.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CALL METHOD obj_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'CONSULTAR'.
        PERFORM seleciona_viagens.
    ENDCASE.

    ctl_alv->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

TABLES: zlest0185.

DATA: it_zlest0185 TYPE TABLE OF zde_zlest0185_alv,
      ok_code      TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK viag01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p0001 FOR zlest0185-viagem_id,
                p0002 FOR zlest0185-id_integracao,
                p0003 FOR zlest0185-ck_processada,
                p0004 FOR zlest0185-ck_integrada,
                p0005 FOR zlest0185-ck_autorizada,
                p0006 FOR zlest0185-dt_registro DEFAULT sy-datum,
                p0007 FOR zlest0185-hr_registro,
                p0008 FOR zlest0185-us_registro,
                p0009 FOR zlest0185-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY MEMORY ID buk,
                p0010 FOR zlest0185-id_lote_frete,
                p0011 FOR zlest0185-id_ordem,
                p0012 FOR zlest0185-vbeln,
                p0013 FOR zlest0185-ebeln.
SELECTION-SCREEN END OF BLOCK viag01.


AT SELECTION-SCREEN.

  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'                       "1375894
    ID 'ACTVT' FIELD '03'       "display                    "1375894
    ID 'BUKRS' FIELD p0009-low.                             "1375894

  IF sy-subrc IS NOT INITIAL.                               "1375894
    SET CURSOR FIELD 'P0009-LOW'.                           "1375894
    MESSAGE e091(8b) WITH p0009-low.                        "1375894
  ENDIF.                                                    "1375894

START-OF-SELECTION.

  PERFORM seleciona_viagens.

END-OF-SELECTION.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_LOTES_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_viagens .

  DATA: wa_zlest0185 TYPE zde_zlest0185_alv.

  CLEAR: it_zlest0185[], it_zlest0185.

  SELECT * INTO TABLE @DATA(it_dados)
    FROM zlest0185
   WHERE viagem_id     IN @p0001
     AND id_integracao IN @p0002
     AND ck_processada IN @p0003
     AND ck_integrada  IN @p0004
     AND ck_autorizada IN @p0005
     AND dt_registro   IN @p0006
     AND hr_registro   IN @p0007
     AND us_registro   IN @p0008
     AND bukrs         IN @p0009
     AND id_lote_frete IN @p0010
     AND id_ordem      IN @p0011
     AND vbeln         IN @p0012
     AND ebeln         IN @p0013.

  LOOP AT it_dados INTO DATA(wa_dados).
    CLEAR: wa_zlest0185.
    MOVE-CORRESPONDING wa_dados TO wa_zlest0185.

    CASE wa_dados-ck_processada.
      WHEN abap_true.

        CASE wa_dados-ck_autorizada.
          WHEN abap_true.
            CASE wa_dados-ck_integrada.
              WHEN abap_true.
                wa_zlest0185-line_color = 'C100'.
              WHEN abap_false.
                wa_zlest0185-line_color = 'C700'.
            ENDCASE.
          WHEN abap_false.
            wa_zlest0185-line_color = 'C200'.
        ENDCASE.

      WHEN abap_false.

        wa_zlest0185-line_color = 'C400'.

    ENDCASE.

    APPEND wa_zlest0185 TO it_zlest0185.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.

  PERFORM sair_0001.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.



ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = 2.

    ctl_cccontainer      = dg_splitter->get_container( EXPORTING row = 1 column = 1 ).
    ctl_cccontainer_html = dg_splitter->get_container( EXPORTING row = 1 column = 2 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = ctl_cccontainer.

    PERFORM fill_it_fieldcatalog.

    PERFORM fill_gs_variant.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = ctl_alv.
*
    SET HANDLER obg_toolbar->on_toolbar FOR ctl_alv.
    SET HANDLER obg_toolbar->handle_user_command FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
        "IT_EXCEPT_QINFO = ZCL_INTEGRACAO=>ZIF_INTEGRACAO~GET_QINFO_ALV( )
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_zlest0185[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv.
    SET HANDLER event_handler->handle_double_click  FOR ctl_alv.

  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZLEST0185_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.

    IF <fs_cat>-fieldname = 'VIAGEM_ID' OR
       <fs_cat>-fieldname = 'ID_INTEGRACAO_APROVAR' OR
       <fs_cat>-fieldname = 'ID_INTEGRACAO_REJEITAR' OR
       <fs_cat>-fieldname = 'ID_INTEGRACAO' OR
       <fs_cat>-fieldname = 'ID_INTEGRACAO_SOL_OC' OR
       <fs_cat>-fieldname = 'ID_INTEGRACAO_CARREGAR' OR
       <fs_cat>-fieldname = 'ID_INTEGRACAO_DESCARGA'.
      <fs_cat>-hotspot = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0001'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

  gs_layout-sel_mode     = 'A'.
  gs_layout-info_fname   = 'LINE_COLOR'.
  gs_layout-stylefname   = 'STYLE'.
  gs_layout-ctab_fname   = 'COLOR_CELL'.
  gs_layout-zebra        = abap_false.
  gs_layout-cwidth_opt   = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SAIR_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair_0001 .

  CLEAR: event_handler, obg_toolbar.

  IF lc_open_browser = abap_true.
    cl_abap_browser=>close_browser( ).
  ENDIF.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.
  CLEAR: ctl_alv.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF ctl_cccontainer_html IS NOT INITIAL.
    ctl_cccontainer_html->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_html.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_zlest0185 INDEX row_id INTO DATA(wa_0185).

  CASE fieldname.
    WHEN 'VIAGEM_ID'.
      PERFORM mostrar_mensagens USING wa_0185.
      "PERFORM MOSTRAR_MENSAGENS_DOCKING USING WA_0185.

    WHEN 'ID_INTEGRACAO'.
      CHECK wa_0185-id_integracao IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_0185-id_integracao.

    WHEN 'ID_INTEGRACAO_APROVAR'.
      CHECK wa_0185-id_integracao_aprovar IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_0185-id_integracao_aprovar.

    WHEN 'ID_INTEGRACAO_REJEITAR'.
      CHECK wa_0185-id_integracao_rejeitar IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_0185-id_integracao_rejeitar.

    WHEN 'ID_INTEGRACAO_SOL_OC'.
      CHECK wa_0185-id_integracao_sol_oc IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_0185-id_integracao_sol_oc.

    WHEN 'ID_INTEGRACAO_CARREGAR'.
      CHECK wa_0185-id_integracao_carregar IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_0185-id_integracao_carregar.

    WHEN 'ID_INTEGRACAO_DESCARGA'.
      CHECK wa_0185-id_integracao_descarga IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_0185-id_integracao_descarga.

  ENDCASE.


ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.
    READ TABLE it_zlest0185 INDEX p_row-index INTO DATA(wa_0185).
    PERFORM mostrar_mensagens USING wa_0185.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0185  text
*----------------------------------------------------------------------*
FORM mostrar_mensagens  USING p_wa_0185 TYPE zde_zlest0185_alv.


  IF lc_open_browser = abap_true.
    cl_abap_browser=>close_browser( ).
    lc_open_browser = abap_false.
  ENDIF.

  lc_open_browser = abap_true.

  cl_abap_browser=>show_html(
   EXPORTING
     html_string = zcl_integracao_ord_carrega=>get_txt_html_viagem( EXPORTING i_viagem_id = p_wa_0185-viagem_id )
     modal       = abap_false
     format      = cl_abap_browser=>landscape
     size        = cl_abap_browser=>small
     container   = ctl_cccontainer_html ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_INTEGRACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0185_ID_INTEGRACAO_APROVAR  text
*----------------------------------------------------------------------*
FORM chamar_integracao  USING p_id_integracao TYPE zde_id_integracao.

  CALL FUNCTION 'ZLES_CARGUERO_0004'
    EXPORTING
      id_integracao = p_id_integracao.

ENDFORM.
