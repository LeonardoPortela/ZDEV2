*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0301 .
*----------------------------------------------------------------------*

" Esta Include está projetada para informar dados de faturamento por
" documento de transporte

*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_0301 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_0301  TYPE REF TO lcl_event_handler_0301.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_0301 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_0301 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: wa_0301_ter TYPE TABLE OF zib_cte_dist_ter    WITH HEADER LINE,
      it_0301_n55 TYPE TABLE OF zib_cte_dist_n55    WITH HEADER LINE,
      it_0301_n01 TYPE TABLE OF zib_cte_dist_n01    WITH HEADER LINE,
      it_0301_nit TYPE TABLE OF zib_cte_dist_nit    WITH HEADER LINE,
      it_0301_dup TYPE TABLE OF zib_cte_dist_dup    WITH HEADER LINE,
      it_0301_vt  TYPE TABLE OF zde_cte_dist_vt_alv WITH HEADER LINE.

DATA: ctl_alv_0301        TYPE REF TO cl_gui_alv_grid,
      ctl_con_0301        TYPE REF TO cl_gui_custom_container,
      gs_lay_0301         TYPE lvc_s_layo,
      gs_var_0301         TYPE disvariant,
      gs_scroll_col_0301  TYPE lvc_s_col,
      gs_scroll_row_0301  TYPE lvc_s_roid,
      it_catalog_0301     TYPE lvc_t_fcat,
      lc_index            TYPE sy-tabix,
      lc_qtd_linhas       TYPE i,
      lc_ctr_altera_frete TYPE char01,
      lc_ctr_calcul_frete TYPE char01,
      gb_ultima_dt_venc   TYPE zdt_vencto.

DATA: it_exclude_0301 TYPE ui_functions,
      wa_exclude_0301 LIKE LINE OF it_exclude_fcode.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0301
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_0301
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_0301_vt INDEX row_id.
  lc_index = row_id.

  CASE fieldname.
    WHEN 'IC_EDITAR'.
      PERFORM editar_0301_vt.
      LEAVE TO SCREEN 0301.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TELA_FATURAMENTO
*&---------------------------------------------------------------------*
FORM chama_tela_faturamento CHANGING p_cte_ter TYPE zib_cte_dist_ter.

  CLEAR: it_0301_vt[], it_0301_n55[], it_0301_n01[], it_0301_nit[].

  FIELD-SYMBOLS: <fs_0301_vt> TYPE zde_cte_dist_vt_alv.

  wa_0301_ter = p_cte_ter.

  IF wa_0301_ter-zdt_vencto IS INITIAL AND gb_ultima_dt_venc IS NOT INITIAL.
    wa_0301_ter-zdt_vencto = gb_ultima_dt_venc.
  ENDIF.

  "Sugerir data de Vencimento
  IF wa_0301_ter-zdt_vencto IS INITIAL.
    SELECT *
      INTO TABLE it_0301_dup
      FROM zib_cte_dist_dup
     WHERE cd_chave_cte  EQ p_cte_ter-cd_chave_cte
       AND dt_vencimento GE sy-datum
      ORDER BY dt_vencimento.

    IF sy-subrc IS INITIAL.
      READ TABLE it_0301_dup INDEX 1.
      wa_0301_ter-zdt_vencto = it_0301_dup-dt_vencimento.
    ENDIF.
  ENDIF.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto

  CASE p_cte_ter-cd_tipo_cte.
    WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

      SELECT *
        INTO TABLE it_0301_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ p_cte_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ p_cte_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
      DATA: lc_ck_autorizados TYPE c LENGTH 1,
            lc_matnr          TYPE matnr,
            lc_grupo          TYPE matkl,
            lc_tipo           TYPE zde_tp_aut_frete.

      lc_ck_autorizados = abap_true.

      LOOP AT it_0301_nit.

        SELECT SINGLE matnr INTO lc_matnr
          FROM j_1bnflin
         WHERE docnum EQ it_0301_nit-docnum
           AND itmnum EQ it_0301_nit-itmnum.

        SELECT SINGLE matkl INTO lc_grupo
          FROM mara
         WHERE matnr EQ lc_matnr.

        SELECT SINGLE tp_aut_frete
          INTO lc_tipo
          FROM zib_cte_dist_gm
         WHERE matkl EQ lc_grupo.

        IF ( sy-subrc IS INITIAL ) AND ( it_0301_nit-ck_autorizado NE abap_true ) AND ( p_cte_ter-cd_modal NE '04' ).
          lc_ck_autorizados = abap_false.
        ENDIF.
      ENDLOOP.
      "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      LOOP AT it_0301_n55.
        CLEAR: it_0301_vt.
        READ TABLE it_0301_nit WITH KEY docnum = it_0301_n55-docnum_nfe.
        it_0301_vt-tknum            = it_0301_n55-tknum.
        it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
        it_0301_vt-zvlr_vi          = it_0301_n55-zvlr_vi.
        it_0301_vt-zvlr_mercadoria  = it_0301_n55-zvlr_mercadoria.

        "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
        IF it_0301_nit-ck_autorizado EQ abap_true.
          it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
        ELSE.
          it_0301_vt-zvlr_frete	      = it_0301_n55-zvlr_frete.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
        ENDIF.

        it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
        it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
        it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
        it_0301_vt-zquebra          = it_0301_nit-zquebra.
        it_0301_vt-zperda           = it_0301_nit-zperda.
        it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
        it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
        it_0301_vt-zvlr_quebra      = it_0301_n55-zvlr_quebra.
        it_0301_vt-zvlr_perda	      = it_0301_n55-zvlr_perda.
        it_0301_vt-zvlr_liq_pagar	  = it_0301_n55-zvlr_liq_pagar.
        it_0301_vt-ck_peso_digitado = it_0301_n55-ck_peso_digitado.
        it_0301_vt-docnum           = it_0301_nit-docnum.
        it_0301_vt-itmnum           = it_0301_nit-itmnum.

        IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( p_cte_ter-ck_peso_chegada EQ abap_true ).
          it_0301_vt-ic_editar = icon_set_state.
        ELSE.
          it_0301_vt-ic_editar = icon_change_number.
        ENDIF.
        APPEND it_0301_vt.
      ENDLOOP.

      LOOP AT it_0301_n01.
        CLEAR: it_0301_vt.
        READ TABLE it_0301_nit WITH KEY docnum = it_0301_n01-docnum_nf.
        it_0301_vt-tknum            = it_0301_n01-tknum.
        it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
        it_0301_vt-zvlr_vi          = it_0301_n01-zvlr_vi.
        it_0301_vt-zvlr_mercadoria  = it_0301_n01-zvlr_mercadoria.

        "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
        IF it_0301_nit-ck_autorizado EQ abap_true.
          it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
        ELSE.
          it_0301_vt-zvlr_frete	      = it_0301_n01-zvlr_frete.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
        ENDIF.

        it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
        it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
        it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
        it_0301_vt-zquebra          = it_0301_nit-zquebra.
        it_0301_vt-zperda           = it_0301_nit-zperda.
        it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
        it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
        it_0301_vt-zvlr_quebra      = it_0301_n01-zvlr_quebra.
        it_0301_vt-zvlr_perda	      = it_0301_n01-zvlr_perda.
        it_0301_vt-zvlr_liq_pagar	  = it_0301_n01-zvlr_liq_pagar.
        it_0301_vt-ck_peso_digitado = it_0301_n01-ck_peso_digitado.

        IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( p_cte_ter-ck_peso_chegada EQ abap_true ).
          it_0301_vt-ic_editar = icon_set_state.
        ELSE.
          it_0301_vt-ic_editar = icon_change_number.
        ENDIF.

        APPEND it_0301_vt.
      ENDLOOP.

      "Conhecimento de Transporte - Modal: Ferroviário
      IF wa_0301_ter-cd_modal = '04'.
        IF wa_0301_ter-dt_chegada IS INITIAL.
          wa_0301_ter-dt_chegada = wa_0301_ter-dt_emissao.
        ENDIF.
        LOOP AT it_0301_vt ASSIGNING <fs_0301_vt>.
          IF <fs_0301_vt>-peso_chegada IS INITIAL.
            <fs_0301_vt>-peso_chegada = <fs_0301_vt>-peso_origem.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN 1. "CT-e de Complemento de Valores
      it_0301_vt-zvlr_vi          = 0.
      it_0301_vt-zvlr_frete       = p_cte_ter-zvlr_frete.
      it_0301_vt-zvlr_mercadoria  = 0.
      it_0301_vt-peso_origem      = 0.
      it_0301_vt-peso_chegada     = 0.
      it_0301_vt-zpeso_diferenca  = 0.
      it_0301_vt-zvlr_kg_transp   = 0.
      it_0301_vt-zvlr_kg_mercad   = 0.
      it_0301_vt-zquebra          = 0.
      it_0301_vt-zperda           = 0.
      it_0301_vt-zvlr_quebra      = 0.
      it_0301_vt-zvlr_perda       = 0.
      it_0301_vt-zvlr_liq_pagar   = p_cte_ter-zvlr_frete.
      it_0301_vt-pc_quebra        = 0.
      it_0301_vt-pc_tolerancia    = 0.
      APPEND it_0301_vt.
  ENDCASE.

  DESCRIBE TABLE it_0301_vt LINES lc_qtd_linhas.
  CLEAR: it_0301_vt.

  IF ( lc_ck_autorizados = abap_false ) AND ( p_cte_ter-cd_tipo_cte NE '1' ).
    MESSAGE s107.
  ELSEIF lc_qtd_linhas EQ 0.
    MESSAGE s088.
  ELSEIF lc_qtd_linhas EQ 1.
    "Chama Tela com TV já Selecionada
    READ TABLE it_0301_vt INDEX 1.
    lc_index = 1.
    PERFORM editar_0301_vt.
  ELSE.
    SORT it_0301_vt BY tknum.
    DELETE ADJACENT DUPLICATES FROM it_0301_vt COMPARING tknum.

    LOOP AT it_0301_vt ASSIGNING <fs_0301_vt> WHERE ck_peso_digitado EQ abap_false.
      <fs_0301_vt>-zvlr_mercadoria = 0.
      <fs_0301_vt>-peso_origem     = 0.
      LOOP AT it_0301_n55 WHERE tknum EQ <fs_0301_vt>-tknum.
        LOOP AT it_0301_nit WHERE docnum EQ it_0301_n55-docnum_nfe.
          ADD it_0301_n55-zvlr_mercadoria TO <fs_0301_vt>-zvlr_mercadoria.
          IF it_0301_nit-ck_autorizado EQ abap_true.
            ADD it_0301_nit-peso_origem_apro TO <fs_0301_vt>-peso_origem.
          ELSE.
            ADD it_0301_nit-peso_origem TO <fs_0301_vt>-peso_origem.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DESCRIBE TABLE it_0301_vt LINES lc_qtd_linhas.

    IF lc_qtd_linhas EQ 1.
      "Chama Tela com TV já Selecionada
      READ TABLE it_0301_vt INDEX 1.
      lc_index = 1.
      PERFORM editar_0301_vt.
    ELSE .
      CALL SCREEN 0301 STARTING AT 15 02.
    ENDIF.
    "Chama Tela para Selecionar a TV
  ENDIF.

ENDFORM.                    " CHAMA_TELA_FATURAMENTO

*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.

  "WA_EXCLUDE_FCODE = OK_SALVAR.
  "APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  "SET PF-STATUS 'PFMODAL' EXCLUDING IT_EXCLUDE_FCODE.
  SET PF-STATUS 'PFMODAL'.
  SET TITLEBAR 'TLTKNUM'.

  IF ctl_con_0301 IS INITIAL.

    CREATE OBJECT ctl_con_0301
      EXPORTING
        container_name = 'ALV_TKNUM'.

    CREATE OBJECT ctl_alv_0301
      EXPORTING
        i_parent = ctl_con_0301.

    PERFORM fill_it_fieldcatalog_0301.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0301.
*   Set layout parameters for ALV grid

    "GS_LAY_N55-GRID_TITLE = TEXT-101.
    gs_lay_0301-sel_mode   = space.
    gs_lay_0301-zebra      = abap_true.
    "GS_LAY_N55-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD ctl_alv_0301->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0301
        is_variant           = gs_var_0301
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0301
      CHANGING
        it_fieldcatalog      = it_catalog_0301
        it_outtab            = it_0301_vt[].

    CALL METHOD ctl_alv_0301->refresh_table_display.

    CREATE OBJECT event_handler_0301.
    SET HANDLER event_handler_0301->handle_hotspot_click
            FOR ctl_alv_0301.

  ELSE.
    CALL METHOD ctl_alv_0301->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0301->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0301
      es_row_no   = gs_scroll_row_0301.

ENDMODULE.                 " STATUS_0301  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  EDITAR_0301_VT
*&---------------------------------------------------------------------*
FORM editar_0301_vt.

  it_0301_vt-ck_peso_digitado = abap_false.

  "Buscar Peso de Chegada do Item Transportado.
  "IT_0301_VT-PESO_CHEGADA - Peso de Chegada
  "WA_0301_TER-DT_CHEGADA  - Data de Chegada
  PERFORM busca_peso_chegada CHANGING it_0301_vt wa_0301_ter sy-subrc.

  "Se achar o peso informado calcular quebra e perda
  IF sy-subrc IS INITIAL.
    CALL METHOD obj_cte->calcula_quebra_perda
      EXPORTING
        p_cod_mercadoria    = it_0301_vt-zmatnr_merc
        p_peso_origem       = it_0301_vt-peso_origem
        p_peso_destino      = it_0301_vt-peso_chegada
        p_vlr_frete         = it_0301_vt-zvlr_frete
        p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
        p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
      IMPORTING
        e_peso_diferenca    = it_0301_vt-zpeso_diferenca
        e_peso_quebra       = it_0301_vt-zquebra
        e_peso_perda        = it_0301_vt-zperda
        e_vlr_quebra        = it_0301_vt-zvlr_quebra
        e_vlr_perda         = it_0301_vt-zvlr_perda
        e_vlr_liq_pagar     = it_0301_vt-zvlr_liq_pagar
        e_pc_quebra         = it_0301_vt-pc_quebra
        e_pc_tolerancia     = it_0301_vt-pc_tolerancia.
  ENDIF.
  lc_ctr_altera_frete = abap_false.
  lc_ctr_calcul_frete = abap_false.

  CALL SCREEN 0302 STARTING AT 30 10.

  IF it_0301_vt-ck_peso_digitado EQ abap_true.
    IF it_0301_vt-ck_peso_digitado EQ abap_true.
      it_0301_vt-ic_editar = icon_set_state.
    ELSE.
      it_0301_vt-ic_editar = icon_change_number.
    ENDIF.
    MODIFY it_0301_vt INDEX lc_index.

    "CT-e Possuir somente uma VT, e o usuário confirmou ou salvou, deve ser gravado
    IF lc_qtd_linhas EQ 1.
      PERFORM salvar_informacoes_doc.
    ENDIF.
  ENDIF.

ENDFORM.                    " EDITAR_0301_VT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0301 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0301> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_CTE_DIST_VT_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_0301.


  LOOP AT it_catalog_0301 ASSIGNING <fs_cat_0301>.
    CASE <fs_cat_0301>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_0301>-col_pos = 1.
    ENDCASE.
  ENDLOOP.

  lc_col_pos = 2.

  LOOP AT it_catalog_0301 ASSIGNING <fs_cat_0301>.
    IF <fs_cat_0301>-col_pos IS INITIAL.
      <fs_cat_0301>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.
    <fs_cat_0301>-tabname = 'IT_0301_VT'.
    CASE <fs_cat_0301>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_0301>-key     = abap_true.
        <fs_cat_0301>-hotspot = abap_true.
        <fs_cat_0301>-just    = 'C'.
      WHEN 'ZVLR_VI' OR 'ZVLR_FRETE' OR 'ZVLR_MERCADORIA' OR 'PESO_ORIGEM' OR 'PESO_CHEGADA' OR 'ZPESO_DIFERENCA' OR
           'ZQUEBRA' OR 'ZPERDA' OR 'ZVLR_QUEBRA' OR 'ZVLR_PERDA' OR 'ZVLR_LIQ_PAGAR'.
        <fs_cat_0301>-do_sum  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0301

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0301 .

  gs_var_0301-report      = sy-repid.
  gs_var_0301-handle      = '0301'.
  gs_var_0301-log_group   = abap_false.
  gs_var_0301-username    = abap_false.
  gs_var_0301-variant     = abap_false.
  gs_var_0301-text        = abap_false.
  gs_var_0301-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0301

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit0301 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_EXIT0301  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301 INPUT.

  CASE ok_code.
    WHEN ok_salvar.
      PERFORM salvar_informacoes_doc.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0301  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0302 OUTPUT.

  CLEAR: it_exclude_fcode.

  IF wa_0301_ter-ck_finalizado EQ abap_true.
    wa_exclude_fcode = ok_salvar.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = ok_confirmar.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    LOOP AT SCREEN.
      IF screen-input NE 0.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
    CASE wa_0301_ter-cd_tipo_cte.
      WHEN 1. "1  CT-e de Complemento de Valores
        LOOP AT SCREEN.
          IF ( screen-input NE 0 ) AND
             ( screen-name EQ 'WA_0301_TER-MWSKZ' OR
               screen-name EQ 'WA_0301_TER-DT_CHEGADA' OR
               screen-name EQ 'IT_0301_VT-ZVLR_FRETE' OR
               screen-name EQ 'IT_0301_VT-PESO_ORIGEM' OR
               screen-name EQ 'IT_0301_VT-PESO_CHEGADA' ).
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    "Valores informados na autorização de pagamento
    IF it_0301_vt-ck_autorizado EQ abap_true.
      LOOP AT SCREEN.
        IF ( screen-input NE 0 ) AND
           ( screen-name EQ 'WA_0301_TER-DT_CHEGADA' OR
             screen-name EQ 'IT_0301_VT-ZVLR_FRETE' OR
             screen-name EQ 'IT_0301_VT-PESO_ORIGEM' OR
             screen-name EQ 'IT_0301_VT-PESO_CHEGADA' ).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF wa_0301_ter-cd_modal EQ '04'.
      LOOP AT SCREEN.
        IF ( screen-input NE 0 ) AND
           ( screen-name EQ 'WA_0301_TER-DT_CHEGADA' OR
             screen-name EQ 'IT_0301_VT-PESO_CHEGADA' OR
             screen-name EQ 'WA_0301_TER-MWSKZ' ).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  "Se existir somente uma VT não precisa confirmar
  IF lc_qtd_linhas EQ 1.
    wa_exclude_fcode = ok_confirmar.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

  SET PF-STATUS 'PFTELA' EXCLUDING it_exclude_fcode.
  SET TITLEBAR 'TLTKNUMX' WITH it_0301_vt-tknum.

  IF wa_0301_ter-zbvtyp IS INITIAL.
    wa_0301_ter-zbvtyp = '0001'.
  ENDIF.

  IF wa_0301_ter-zbvtyp IS NOT INITIAL.
    CLEAR: wa_info_forne.

    CALL METHOD obj_cte->busca_banco_parceiro
      IMPORTING
        e_lfbk     = e_lfbk
        e_bnka     = e_bnka
      CHANGING
        p_cte      = wa_0301_ter
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      wa_info_forne-bvtyp = e_lfbk-bvtyp.
      wa_info_forne-bankl = e_bnka-bankl(3).
      wa_info_forne-banka = e_bnka-banka.
      wa_info_forne-bankn = e_lfbk-bankn.

      IF NOT e_lfbk-bkont IS INITIAL.
        CONCATENATE e_lfbk-bankl+4(11) '-' e_lfbk-bkont INTO wa_info_forne-agenc.
      ELSE.
        wa_info_forne-agenc = e_lfbk-bankl+4(11).
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: wa_info_forne.
  ENDIF.

  IF wa_0301_ter-zdt_mov IS INITIAL.
    wa_0301_ter-zdt_mov = sy-datum.
  ELSEIF wa_0301_ter-zdt_mov IS NOT INITIAL AND wa_0301_ter-belnr IS INITIAL.
    wa_0301_ter-zdt_mov = sy-datum.
  ENDIF.

ENDMODULE.                 " STATUS_0302  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR_INFORMACOES_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_informacoes_doc .

  FIELD-SYMBOLS: <n55> TYPE zib_cte_dist_n55,
                 <n01> TYPE zib_cte_dist_n01,
                 <nit> TYPE zib_cte_dist_nit.


*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
  CASE wa_0301_ter-cd_tipo_cte.
    WHEN 0 OR 3. "0  CT-e Normal/CT-e Substituto

      CLEAR: it_0301_n55[], it_0301_n01[], it_0301_nit[].

      SELECT *
        INTO TABLE it_0301_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ wa_0301_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ wa_0301_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      wa_0301_ter-zvlr_vi         = 0.
      wa_0301_ter-zvlr_frete      = 0.
      wa_0301_ter-zvlr_mercadoria = 0.
      wa_0301_ter-peso_origem     = 0.
      wa_0301_ter-peso_chegada    = 0.
      wa_0301_ter-zpeso_diferenca = 0.
      wa_0301_ter-zquebra         = 0.
      wa_0301_ter-zperda          = 0.
      wa_0301_ter-zvlr_quebra     = 0.
      wa_0301_ter-zvlr_perda      = 0.
      wa_0301_ter-zvlr_liq_pagar  = 0.
      wa_0301_ter-ck_peso_chegada = abap_true.

      LOOP AT it_0301_vt.
        ADD it_0301_vt-zvlr_vi         TO wa_0301_ter-zvlr_vi.
        ADD it_0301_vt-zvlr_frete      TO wa_0301_ter-zvlr_frete.
        ADD it_0301_vt-zvlr_mercadoria TO wa_0301_ter-zvlr_mercadoria.
        ADD it_0301_vt-zpeso_diferenca TO wa_0301_ter-zpeso_diferenca.
        ADD it_0301_vt-zquebra         TO wa_0301_ter-zquebra.
        ADD it_0301_vt-zperda          TO wa_0301_ter-zperda.
        ADD it_0301_vt-zvlr_quebra     TO wa_0301_ter-zvlr_quebra.
        ADD it_0301_vt-zvlr_perda      TO wa_0301_ter-zvlr_perda.
        ADD it_0301_vt-zvlr_liq_pagar  TO wa_0301_ter-zvlr_liq_pagar.
        ADD it_0301_vt-peso_chegada    TO wa_0301_ter-peso_chegada.

        IF it_0301_vt-ck_autorizado EQ abap_false.
          ADD it_0301_vt-peso_origem  TO wa_0301_ter-peso_origem.
        ENDIF.

        "Ajustando valores notas 55
        LOOP AT it_0301_n55 ASSIGNING <n55> WHERE tknum EQ it_0301_vt-tknum.
          <n55>-zvlr_frete       = it_0301_vt-zvlr_frete.
          <n55>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n55>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
          <n55>-zvlr_perda       = it_0301_vt-zvlr_perda.
          <n55>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
          <n55>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n55>-docnum_nfe.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.

            IF it_0301_vt-ck_autorizado EQ abap_false.
              <nit>-peso_origem    = it_0301_vt-peso_origem.
              <nit>-peso_chegada   = it_0301_vt-peso_chegada.
            ELSE.
              ADD <nit>-peso_origem TO wa_0301_ter-peso_origem.
            ENDIF.

            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-zquebra          = it_0301_vt-zquebra.
            <nit>-zperda           = it_0301_vt-zperda.
            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
            IF it_0301_vt-ck_peso_digitado EQ abap_false.
              wa_0301_ter-ck_peso_chegada = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "Ajustando valores notas 01
        LOOP AT it_0301_n01 ASSIGNING <n01> WHERE tknum EQ it_0301_vt-tknum.
          <n01>-zvlr_frete       = it_0301_vt-zvlr_frete.
          <n01>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n01>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
          <n01>-zvlr_perda       = it_0301_vt-zvlr_perda.
          <n01>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar .
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n01>-docnum_nf.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
            <nit>-peso_origem      = it_0301_vt-peso_origem.
            <nit>-peso_chegada     = it_0301_vt-peso_chegada.
            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-zquebra          = it_0301_vt-zquebra.
            <nit>-zperda           = it_0301_vt-zperda.
            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
            IF it_0301_vt-ck_peso_digitado EQ abap_false.
              wa_0301_ter-ck_peso_chegada = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      wa_0301_ter-zbase_icms    = 0.
      wa_0301_ter-zbase_pis     = 0.
      wa_0301_ter-zbase_cofins  = 0.
      wa_0301_ter-zrate_icms    = 0.
      wa_0301_ter-zrate_pis     = 0.
      wa_0301_ter-zrate_cofins  = 0.
      wa_0301_ter-zvalor_icms   = 0.
      wa_0301_ter-zvalor_pis    = 0.
      wa_0301_ter-zvalor_cofins = 0.

      IF it_0301_n55[] IS NOT INITIAL.
        MODIFY zib_cte_dist_n55 FROM TABLE it_0301_n55.
      ENDIF.

      IF it_0301_n01[] IS NOT INITIAL.
        MODIFY zib_cte_dist_n01 FROM TABLE it_0301_n01.
      ENDIF.

      IF it_0301_nit[] IS NOT INITIAL.
        MODIFY zib_cte_dist_nit FROM TABLE it_0301_nit.
      ENDIF.

    WHEN 1. "1  CT-e de Complemento de Valores
      wa_0301_ter-ck_peso_chegada = abap_true.
  ENDCASE.

  "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
    FROM j_1bnflin AS l
   INNER JOIN zib_cte_dist_n55 AS n ON n~docnum_nfe EQ l~docnum
   WHERE n~cd_chave_cte EQ @wa_0301_ter-cd_chave_cte
     AND n~docnum_nfe   NE @space.

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE l~matnr INTO lc_matnr
      FROM j_1bnflin AS l
     INNER JOIN zib_cte_dist_n01 AS n ON n~docnum_nf EQ l~docnum
     WHERE n~cd_chave_cte EQ wa_0301_ter-cd_chave_cte
       AND n~docnum_nf    NE space.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL METHOD obj_cte->busca_impostos_taxas
    EXPORTING
      p_iva            = wa_0301_ter-mwskz
      p_data_documento = wa_0301_ter-dt_emissao
      p_shipfrom       = wa_0301_ter-inicio_uf
      p_shipto         = wa_0301_ter-termino_uf
      e_tomadora       = wa_0301_ter-e_tomadora
      f_tomadora       = wa_0301_ter-f_tomadora
      p_emissora       = wa_0301_ter-p_emissor
      p_matnr          = lc_matnr
    IMPORTING
      e_rate_icms      = wa_0301_ter-zrate_icms
      e_rate_pis       = wa_0301_ter-zrate_pis
      e_rate_cofins    = wa_0301_ter-zrate_cofins
    EXCEPTIONS
      sem_iva          = 1
      OTHERS           = 2.

  IF sy-subrc IS INITIAL.
    IF wa_0301_ter-zrate_icms GT 0.
      wa_0301_ter-zbase_icms  = wa_0301_ter-zvlr_frete.
      wa_0301_ter-zvalor_icms = wa_0301_ter-zvlr_frete * ( wa_0301_ter-zrate_icms / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_icms  = 0.
      zib_cte_dist_ter-zvalor_icms = 0.
    ENDIF.

    DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( wa_0301_ter-zvlr_frete )
                                                                           i_valor_icms  =   CONV #( wa_0301_ter-zvalor_icms ) ).

    IF wa_0301_ter-zrate_pis GT 0.
      wa_0301_ter-zbase_pis  = lva_base_calc_pis_cofins.
      wa_0301_ter-zvalor_pis = lva_base_calc_pis_cofins * ( wa_0301_ter-zrate_pis / 100 ).
    ELSE.
      wa_0301_ter-zbase_pis  = 0.
      wa_0301_ter-zvalor_pis = 0.
    ENDIF.

    IF wa_0301_ter-zrate_cofins GT 0.
      wa_0301_ter-zbase_cofins  = lva_base_calc_pis_cofins.
      wa_0301_ter-zvalor_cofins = lva_base_calc_pis_cofins * ( wa_0301_ter-zrate_cofins / 100 ).
    ELSE.
      wa_0301_ter-zbase_cofins  = 0.
      wa_0301_ter-zvalor_cofins = 0.
    ENDIF.
  ENDIF.

  gb_ultima_dt_venc = wa_0301_ter-zdt_vencto.
  "ZDT_MOV.

  MODIFY zib_cte_dist_ter FROM wa_0301_ter.
  COMMIT WORK.

  MESSAGE s059.

ENDFORM.                    " SALVAR_INFORMACOES_DOC

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0302 INPUT.

  IF lc_ctr_altera_frete EQ abap_true.
    ok_code             = ok_verificar.
    lc_ctr_altera_frete = abap_false.
  ELSEIF lc_ctr_calcul_frete EQ abap_true.
    ok_code             = ok_confirmar.
  ENDIF.

  CASE ok_code.
    WHEN ok_confirmar OR ok_salvar.

      IF wa_0301_ter-zdt_vencto LT sy-datum.
        MESSAGE s100 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      "Salvar caso somente exista um registro de VT na CT-e
      "Confirmar caso existe mais de um regsitro de VT na CT-e
      it_0301_vt-ck_peso_digitado = abap_true.
      LEAVE TO SCREEN 0000.

    WHEN ok_verificar.

      IF it_0301_vt-zvlr_frete GT 0 AND it_0301_vt-peso_origem GT 0.
        it_0301_vt-zvlr_kg_transp = it_0301_vt-zvlr_frete / it_0301_vt-peso_origem.
      ENDIF.

      CALL METHOD obj_cte->calcula_quebra_perda
        EXPORTING
          p_cod_mercadoria    = it_0301_vt-zmatnr_merc
          p_peso_origem       = it_0301_vt-peso_origem
          p_peso_destino      = it_0301_vt-peso_chegada
          p_vlr_frete         = it_0301_vt-zvlr_frete
          p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
          p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
        IMPORTING
          e_peso_diferenca    = it_0301_vt-zpeso_diferenca
          e_peso_quebra       = it_0301_vt-zquebra
          e_peso_perda        = it_0301_vt-zperda
          e_vlr_quebra        = it_0301_vt-zvlr_quebra
          e_vlr_perda         = it_0301_vt-zvlr_perda
          e_vlr_liq_pagar     = it_0301_vt-zvlr_liq_pagar
          e_pc_quebra         = it_0301_vt-pc_quebra
          e_pc_tolerancia     = it_0301_vt-pc_tolerancia.

      lc_ctr_calcul_frete = abap_true.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0302  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_PESO_CHEGADA
*&---------------------------------------------------------------------*
*       Importação de Peso Digitado
*----------------------------------------------------------------------*
FORM busca_peso_chegada  CHANGING p_digitar  TYPE zde_cte_dist_vt_alv
                                  p_terceito TYPE zib_cte_dist_ter
                                  p_achou    TYPE sy-subrc.

  DATA: wa_0039 TYPE zlest0039.

*01	Rodoviário
*02	Aéreo
*03	Aquaviário
*04	Ferroviário
*05	Dutoviário

  IF p_digitar-peso_chegada IS INITIAL AND p_terceito-dt_chegada IS INITIAL.

    CASE p_terceito-cd_modal.
      WHEN '01'.  "Rodoviário
        "Comparativo de saidas e chegadas
        SELECT SINGLE * INTO wa_0039 FROM zlest0039 WHERE docnum EQ p_digitar-docnum.
        p_achou = sy-subrc.
        IF sy-subrc IS INITIAL.
          IF wa_0039-pontotransb IS INITIAL.
            p_digitar-peso_chegada = wa_0039-pesochegada.
            p_terceito-dt_chegada  = wa_0039-datachegada.
          ELSE.
            p_digitar-peso_chegada = wa_0039-pesotransb.
            p_terceito-dt_chegada  = wa_0039-datatransb.
          ENDIF.
        ENDIF.
      WHEN '02'.  "Aéreo
      WHEN '03'.  "Aquaviário
      WHEN '04'.  "Ferroviário
      WHEN '05'.  "Dutoviário
    ENDCASE.
  ENDIF.

ENDFORM.                    " BUSCA_PESO_CHEGADA

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VALOR_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_valor_frete INPUT.
  lc_ctr_altera_frete = abap_true.
ENDMODULE.                 " ALTEROU_VALOR_FRETE  INPUT

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_INFORMACAO_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_informacao_frete INPUT.
  lc_ctr_calcul_frete = abap_false.
ENDMODULE.                 " ALTEROU_INFORMACAO_FRETE  INPUT
