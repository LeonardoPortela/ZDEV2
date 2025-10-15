*&---------------------------------------------------------------------*
*&  Include           ZMMR105_0201
*&---------------------------------------------------------------------*

DATA: ctl_alv_nit       TYPE REF TO cl_gui_alv_grid,
      ctl_con_nit       TYPE REF TO cl_gui_custom_container,
      gs_lay_nit        TYPE lvc_s_layo,
      gs_var_nit        TYPE disvariant,
      gs_scroll_col_nit TYPE lvc_s_col,
      gs_scroll_row_nit TYPE lvc_s_roid,
      it_catalog_nit    TYPE lvc_t_fcat,
      ck_salvar         TYPE c LENGTH 1.

DATA: it_exclude_nit TYPE ui_functions,
      wa_exclude_nit LIKE LINE OF it_exclude_fcode,
      it_cte_nit_loc TYPE TABLE OF zde_cte_dist_nit_alv,
      wa_cte_nit_loc TYPE zde_cte_dist_nit_alv.

*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_nit DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_nit TYPE REF TO lcl_event_handler_nit.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_nit IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_nit USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  EDITAR_LINHA_NOTA_N55
*&---------------------------------------------------------------------*
FORM editar_linha_nota_n55 USING p_rowid TYPE int4 CHANGING p_n55 LIKE wa_cte_n55.


  IF p_n55-docnum_nfe IS NOT INITIAL.

    CLEAR: it_cte_nit_loc.

    PERFORM chama_tela_edicao_nxx USING p_n55-docnum_nfe.

    PERFORM atualiza_registro_cabecalho.

    CALL METHOD ctl_alv_n55->refresh_table_display.
    CALL METHOD cl_gui_cfw=>flush.

  ELSE.
    MESSAGE i075.
  ENDIF.

ENDFORM.                    " EDITAR_LINHA_NOTA_NXX

*&---------------------------------------------------------------------*
*&      Form  EDITAR_LINHA_NOTA_N01
*&---------------------------------------------------------------------*
FORM editar_linha_nota_n01 USING p_rowid TYPE int4 CHANGING p_n01 LIKE wa_cte_n01.

  IF p_n01-docnum_nf IS NOT INITIAL.

    CLEAR: it_cte_nit_loc.

    PERFORM chama_tela_edicao_nxx USING p_n01-docnum_nf.

    PERFORM atualiza_registro_cabecalho.

    CALL METHOD ctl_alv_n01->refresh_table_display.
    CALL METHOD cl_gui_cfw=>flush.

  ELSE.
    MESSAGE i075.
  ENDIF.

ENDFORM.                    " EDITAR_LINHA_NOTA_N01

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TELA_EDICAO_NXX
*&---------------------------------------------------------------------*
FORM chama_tela_edicao_nxx USING p_docnum TYPE j_1bdocnum.

  DATA: it_makt      TYPE TABLE OF makt WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_nit> TYPE zde_cte_dist_nit_alv.

  CLEAR: it_cte_nit_loc.

  LOOP AT it_cte_nit INTO wa_cte_nit WHERE docnum EQ p_docnum.
    MOVE-CORRESPONDING wa_cte_nit TO wa_cte_nit_loc.
    APPEND wa_cte_nit_loc TO it_cte_nit_loc.
  ENDLOOP.

  IF it_cte_nit_loc IS NOT INITIAL.
    SELECT * INTO TABLE it_makt
      FROM makt
       FOR ALL ENTRIES IN it_cte_nit_loc
     WHERE spras EQ sy-langu
       AND matnr EQ it_cte_nit_loc-zmatnr_merc.
  ENDIF.

  LOOP AT it_cte_nit_loc ASSIGNING <fs_nit>.
    IF ( <fs_nit>-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
      <fs_nit>-ic_editar = icon_set_state.
    ELSE.
      <fs_nit>-ic_editar = icon_change_number.
    ENDIF.

    READ TABLE it_makt WITH KEY matnr = <fs_nit>-zmatnr_merc.
    IF sy-subrc IS INITIAL.
      <fs_nit>-maktx = it_makt-maktx.
    ENDIF.
  ENDLOOP.

  ck_salvar = abap_off.

  CLEAR: wa_tela_nxx.

  CALL SCREEN 0201 STARTING AT 10 05.

ENDFORM.                    " CHAMA_TELA_EDICAO_NXX

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG_NXX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag_nxx INPUT.
  wa_tela_nxx-ck_peso_digitado = abap_true.
ENDMODULE.                 " SET_UPDATE_FLAG_NXX  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0201 OUTPUT.

  CLEAR: it_exclude_fcode.

  IF zib_cte_dist_ter-ck_finalizado IS NOT INITIAL.
    wa_exclude_fcode = ok_salvar.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

  IF wa_tela_nxx IS INITIAL.
    wa_exclude_fcode = ok_confirmar.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

  SET PF-STATUS 'PFTELA' EXCLUDING it_exclude_fcode.
  SET TITLEBAR 'TLVOLU' WITH wa_tela_nxx-model wa_tela_nxx-nfenum wa_tela_nxx-series.

  IF ctl_con_nit IS INITIAL.

    CREATE OBJECT ctl_con_nit
      EXPORTING
        container_name = 'ALV_NIT'.

    CREATE OBJECT ctl_alv_nit
      EXPORTING
        i_parent = ctl_con_nit.

    PERFORM fill_it_fieldcatalog_nit.
*   Fill info for layout variant

    PERFORM fill_gs_variant_nit.
*   Set layout parameters for ALV grid

    "GS_LAY_N55-GRID_TITLE = TEXT-101.
    gs_lay_nit-sel_mode   = space.
    gs_lay_nit-zebra      = abap_true.
    gs_lay_nit-info_fname = 'ROWCOLOR'.
    "GS_LAY_N55-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD ctl_alv_nit->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_nit
        is_variant           = gs_var_nit
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_nit
      CHANGING
        it_fieldcatalog      = it_catalog_nit
        it_outtab            = it_cte_nit_loc.

    CALL METHOD ctl_alv_nit->refresh_table_display.

    CREATE OBJECT event_handler_nit.
    SET HANDLER event_handler_nit->handle_hotspot_click
            FOR ctl_alv_nit.

  ELSE.
    CALL METHOD ctl_alv_nit->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_nit->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_nit
      es_row_no   = gs_scroll_row_nit.

ENDMODULE.                 " STATUS_0201  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT0201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit0201 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_EXIT0201  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0201 INPUT.

  DATA: p_vlr_kg_trasport   TYPE zde_vlr_kg_trans,
        p_vlr_kg_mercadoria TYPE zde_vlr_kg_merca.

  CASE ok_code.
    WHEN ok_confirmar.
      PERFORM atualiza_linha_nota_nit.

    WHEN ok_verificar.

      IF wa_tela_nxx-zvlr_frete GT 0 AND wa_tela_nxx-peso_origem GT 0.
        wa_tela_nxx-zvlr_kg_transp = wa_tela_nxx-zvlr_frete / wa_tela_nxx-peso_origem.
      ENDIF.

      CALL METHOD obj_cte->calcula_quebra_perda
        EXPORTING
          p_cod_mercadoria    = wa_tela_nxx-zmatnr_merc
          p_peso_origem       = wa_tela_nxx-peso_origem
          p_peso_destino      = wa_tela_nxx-peso_chegada
          p_vlr_frete         = wa_tela_nxx-zvlr_frete
          p_vlr_kg_trasport   = wa_tela_nxx-zvlr_kg_transp
          p_vlr_kg_mercadoria = wa_tela_nxx-zvlr_kg_mercad
        IMPORTING
          e_peso_diferenca    = wa_tela_nxx-zpeso_diferenca
          e_peso_quebra       = wa_tela_nxx-zquebra
          e_peso_perda        = wa_tela_nxx-zperda
          e_vlr_quebra        = wa_tela_nxx-zvlr_quebra
          e_vlr_perda         = wa_tela_nxx-zvlr_perda
          e_vlr_liq_pagar     = wa_tela_nxx-zvlr_liq_pagar
          e_pc_quebra         = wa_tela_nxx-pc_quebra
          e_pc_tolerancia     = wa_tela_nxx-pc_tolerancia.

    WHEN ok_salvar.
      ck_salvar = abap_on.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0201  INPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_REGSITRO_CABECALHO
*&---------------------------------------------------------------------*
FORM atualiza_registro_cabecalho .

  FIELD-SYMBOLS: <wa_n55> LIKE wa_cte_n55,
                 <wa_n01> LIKE wa_cte_n01,
                 <wa_nit> LIKE wa_cte_nit.

  IF ck_salvar EQ abap_on.

    zib_cte_dist_ter-zvlr_frete      = 0.
    zib_cte_dist_ter-zvlr_mercadoria = 0.
    zib_cte_dist_ter-peso_origem     = 0.
    zib_cte_dist_ter-peso_chegada    = 0.
    zib_cte_dist_ter-zpeso_diferenca = 0.
    zib_cte_dist_ter-zquebra         = 0.
    zib_cte_dist_ter-zperda          = 0.
    zib_cte_dist_ter-zvlr_quebra     = 0.
    zib_cte_dist_ter-zvlr_perda      = 0.
    zib_cte_dist_ter-zvlr_liq_pagar  = 0.

    LOOP AT it_cte_n55 ASSIGNING <wa_n55>.
      READ TABLE it_cte_nit_loc INTO wa_cte_nit_loc WITH KEY docnum = <wa_n55>-docnum_nfe.
      IF sy-subrc IS INITIAL.
        <wa_n55>-zvlr_frete      = 0.
        <wa_n55>-zvlr_mercadoria = 0.
        <wa_n55>-zvlr_quebra     = 0.
        <wa_n55>-zvlr_perda      = 0.
        <wa_n55>-zvlr_liq_pagar  = 0.
        LOOP AT it_cte_nit_loc INTO wa_cte_nit_loc.
          ADD wa_cte_nit_loc-zvlr_frete      TO <wa_n55>-zvlr_frete.
          ADD wa_cte_nit_loc-zvlr_mercadoria TO <wa_n55>-zvlr_mercadoria.
          ADD wa_cte_nit_loc-zvlr_quebra     TO <wa_n55>-zvlr_quebra.
          ADD wa_cte_nit_loc-zvlr_perda      TO <wa_n55>-zvlr_perda.
          ADD wa_cte_nit_loc-zvlr_liq_pagar  TO <wa_n55>-zvlr_liq_pagar.
          LOOP AT it_cte_nit ASSIGNING <wa_nit> WHERE cd_chave_cte EQ wa_cte_nit_loc-cd_chave_cte
                                                  AND docnum       EQ wa_cte_nit_loc-docnum
                                                  AND itmnum       EQ wa_cte_nit_loc-itmnum.
            <wa_nit>-zvlr_frete       = wa_cte_nit_loc-zvlr_frete.
            <wa_nit>-zvlr_mercadoria  = wa_cte_nit_loc-zvlr_mercadoria.
            <wa_nit>-zvlr_quebra      = wa_cte_nit_loc-zvlr_quebra.
            <wa_nit>-zvlr_perda       = wa_cte_nit_loc-zvlr_perda.
            <wa_nit>-zvlr_liq_pagar   = wa_cte_nit_loc-zvlr_liq_pagar.
            <wa_nit>-peso_origem      = wa_cte_nit_loc-peso_origem.
            <wa_nit>-peso_chegada     = wa_cte_nit_loc-peso_chegada.
            <wa_nit>-zpeso_diferenca  = wa_cte_nit_loc-zpeso_diferenca.
            <wa_nit>-zvlr_kg_transp   = wa_cte_nit_loc-zvlr_kg_transp.
            <wa_nit>-zvlr_kg_mercad   = wa_cte_nit_loc-zvlr_kg_mercad.
            <wa_nit>-zquebra          = wa_cte_nit_loc-zquebra.
            <wa_nit>-zperda           = wa_cte_nit_loc-zperda.
            <wa_nit>-ck_peso_digitado = wa_cte_nit_loc-ck_peso_digitado.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      ADD <wa_n55>-zvlr_frete      TO zib_cte_dist_ter-zvlr_frete.
      ADD <wa_n55>-zvlr_mercadoria TO zib_cte_dist_ter-zvlr_mercadoria.
      ADD <wa_n55>-zvlr_quebra     TO zib_cte_dist_ter-zvlr_quebra.
      ADD <wa_n55>-zvlr_perda      TO zib_cte_dist_ter-zvlr_perda.
      ADD <wa_n55>-zvlr_liq_pagar  TO zib_cte_dist_ter-zvlr_liq_pagar.
    ENDLOOP.

    LOOP AT it_cte_n01 ASSIGNING <wa_n01>.
      READ TABLE it_cte_nit_loc INTO wa_cte_nit_loc WITH KEY docnum = <wa_n01>-docnum_nf.
      IF sy-subrc IS INITIAL.
        <wa_n01>-zvlr_frete      = 0.
        <wa_n01>-zvlr_mercadoria = 0.
        <wa_n01>-zvlr_quebra     = 0.
        <wa_n01>-zvlr_perda      = 0.
        <wa_n01>-zvlr_liq_pagar  = 0.
        LOOP AT it_cte_nit_loc INTO wa_cte_nit_loc.
          ADD wa_cte_nit_loc-zvlr_frete      TO <wa_n01>-zvlr_frete.
          ADD wa_cte_nit_loc-zvlr_mercadoria TO <wa_n01>-zvlr_mercadoria.
          ADD wa_cte_nit_loc-zvlr_quebra     TO <wa_n01>-zvlr_quebra.
          ADD wa_cte_nit_loc-zvlr_perda      TO <wa_n01>-zvlr_perda.
          ADD wa_cte_nit_loc-zvlr_liq_pagar  TO <wa_n01>-zvlr_liq_pagar.
          LOOP AT it_cte_nit ASSIGNING <wa_nit> WHERE cd_chave_cte EQ wa_cte_nit_loc-cd_chave_cte
                                                  AND docnum       EQ wa_cte_nit_loc-docnum
                                                  AND itmnum       EQ wa_cte_nit_loc-itmnum.
            <wa_nit>-zvlr_frete       = wa_cte_nit_loc-zvlr_frete.
            <wa_nit>-zvlr_mercadoria  = wa_cte_nit_loc-zvlr_mercadoria.
            <wa_nit>-zvlr_quebra      = wa_cte_nit_loc-zvlr_quebra.
            <wa_nit>-zvlr_perda       = wa_cte_nit_loc-zvlr_perda.
            <wa_nit>-zvlr_liq_pagar   = wa_cte_nit_loc-zvlr_liq_pagar.
            <wa_nit>-peso_origem      = wa_cte_nit_loc-peso_origem.
            <wa_nit>-peso_chegada     = wa_cte_nit_loc-peso_chegada.
            <wa_nit>-zpeso_diferenca  = wa_cte_nit_loc-zpeso_diferenca.
            <wa_nit>-zvlr_kg_transp   = wa_cte_nit_loc-zvlr_kg_transp.
            <wa_nit>-zvlr_kg_mercad   = wa_cte_nit_loc-zvlr_kg_mercad.
            <wa_nit>-zquebra          = wa_cte_nit_loc-zquebra.
            <wa_nit>-zperda           = wa_cte_nit_loc-zperda.
            <wa_nit>-ck_peso_digitado = wa_cte_nit_loc-ck_peso_digitado.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      ADD wa_cte_n01-zvlr_frete      TO zib_cte_dist_ter-zvlr_frete.
      ADD wa_cte_n01-zvlr_mercadoria TO zib_cte_dist_ter-zvlr_mercadoria.
      ADD wa_cte_n01-zvlr_quebra     TO zib_cte_dist_ter-zvlr_quebra.
      ADD wa_cte_n01-zvlr_perda      TO zib_cte_dist_ter-zvlr_perda.
      ADD wa_cte_n01-zvlr_liq_pagar  TO zib_cte_dist_ter-zvlr_liq_pagar.
    ENDLOOP.

    LOOP AT it_cte_nit INTO wa_cte_nit.
      ADD wa_cte_nit-peso_origem     TO zib_cte_dist_ter-peso_origem.
      ADD wa_cte_nit-peso_chegada    TO zib_cte_dist_ter-peso_chegada.
      ADD wa_cte_nit-zpeso_diferenca TO zib_cte_dist_ter-zpeso_diferenca.
      ADD wa_cte_nit-zquebra         TO zib_cte_dist_ter-zquebra.
      ADD wa_cte_nit-zperda          TO zib_cte_dist_ter-zperda.
    ENDLOOP.

    "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
      FROM j_1bnflin AS l
     INNER JOIN zib_cte_dist_n55 AS n ON n~docnum_nfe EQ l~docnum
     WHERE n~cd_chave_cte EQ @zib_cte_dist_ter-cd_chave_cte
       AND n~docnum_nfe   NE @space.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE l~matnr INTO lc_matnr
        FROM j_1bnflin AS l
       INNER JOIN zib_cte_dist_n01 AS n ON n~docnum_nf EQ l~docnum
       WHERE n~cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
         AND n~docnum_nf    NE space.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Calcular Impostos devidos/retidos
    CALL METHOD obj_cte->busca_impostos_taxas
      EXPORTING
        p_iva            = zib_cte_dist_ter-mwskz
        p_data_documento = zib_cte_dist_ter-dt_emissao
        p_shipfrom       = zib_cte_dist_ter-inicio_uf
        p_shipto         = zib_cte_dist_ter-termino_uf
        e_tomadora       = zib_cte_dist_ter-e_tomadora
        f_tomadora       = zib_cte_dist_ter-f_tomadora
        p_emissora       = zib_cte_dist_ter-p_emissor
        p_matnr          = lc_matnr
      IMPORTING
        e_rate_icms      = zib_cte_dist_ter-zrate_icms
        e_rate_pis       = zib_cte_dist_ter-zrate_pis
        e_rate_cofins    = zib_cte_dist_ter-zrate_cofins
      EXCEPTIONS
        sem_iva          = 1
        OTHERS           = 2.

    IF sy-subrc IS INITIAL.

      IF zib_cte_dist_ter-zrate_icms GT 0.
        zib_cte_dist_ter-zbase_icms  = zib_cte_dist_ter-zvlr_frete.
        zib_cte_dist_ter-zvalor_icms = zib_cte_dist_ter-zvlr_frete * ( zib_cte_dist_ter-zrate_icms / 100 ).
      ELSE.
        zib_cte_dist_ter-zbase_icms  = 0.
        zib_cte_dist_ter-zvalor_icms = 0.
      ENDIF.

      DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( zib_cte_dist_ter-zvlr_frete )
                                                                             i_valor_icms  =   CONV #( zib_cte_dist_ter-zvalor_icms ) ).

      IF zib_cte_dist_ter-zrate_pis GT 0.
        zib_cte_dist_ter-zbase_pis  = lva_base_calc_pis_cofins.
        zib_cte_dist_ter-zvalor_pis = lva_base_calc_pis_cofins * ( zib_cte_dist_ter-zrate_pis / 100 ).
      ELSE.
        zib_cte_dist_ter-zbase_pis  = 0.
        zib_cte_dist_ter-zvalor_pis = 0.
      ENDIF.

      IF zib_cte_dist_ter-zrate_cofins GT 0.
        zib_cte_dist_ter-zbase_cofins  = lva_base_calc_pis_cofins.
        zib_cte_dist_ter-zvalor_cofins = lva_base_calc_pis_cofins * ( zib_cte_dist_ter-zrate_cofins / 100 ).
      ELSE.
        zib_cte_dist_ter-zbase_cofins  = 0.
        zib_cte_dist_ter-zvalor_cofins = 0.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " ATUALIZA_REGSITRO_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_N55
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_nit
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  FIELD-SYMBOLS: <nit> TYPE zde_cte_dist_nit_alv.

  IF row_id IS NOT INITIAL.

    READ TABLE it_cte_nit_loc INDEX row_id ASSIGNING <nit>.

    CASE fieldname.
      WHEN 'IC_EDITAR'.
        PERFORM dismarcar_todos.
        PERFORM editar_linha_nota_nit CHANGING <nit>.
    ENDCASE.

  ENDIF.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  EDITAR_LINHA_NOTA_NIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM editar_linha_nota_nit CHANGING p_nit TYPE zde_cte_dist_nit_alv.

  DATA: wa_j_1bnfdoc TYPE j_1bnfdoc.

  SELECT SINGLE * INTO wa_j_1bnfdoc
    FROM j_1bnfdoc
   WHERE docnum EQ p_nit-docnum.

  wa_tela_nxx-bukrs   = wa_j_1bnfdoc-bukrs.
  wa_tela_nxx-branch  = wa_j_1bnfdoc-branch.
  wa_tela_nxx-parid   = wa_j_1bnfdoc-parid.
  wa_tela_nxx-series  = wa_j_1bnfdoc-series.
  wa_tela_nxx-model   = wa_j_1bnfdoc-model.
  wa_tela_nxx-brgew   = wa_j_1bnfdoc-brgew.
  wa_tela_nxx-ntgew   = wa_j_1bnfdoc-ntgew.
  wa_tela_nxx-gewei   = wa_j_1bnfdoc-gewei.

  CASE wa_j_1bnfdoc-nfe.
    WHEN abap_true.
      wa_tela_nxx-nfenum = wa_j_1bnfdoc-nfenum.
    WHEN abap_false.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_j_1bnfdoc-nfnum
        IMPORTING
          output = wa_tela_nxx-nfenum.
  ENDCASE.

  p_nit-rowcolor              = cl_c710.
  wa_tela_nxx-docnum          = p_nit-docnum.
  wa_tela_nxx-itmnum          = p_nit-itmnum.
  wa_tela_nxx-zvlr_frete      = p_nit-zvlr_frete.
  wa_tela_nxx-zvlr_mercadoria = p_nit-zvlr_mercadoria.
  wa_tela_nxx-peso_origem     = p_nit-peso_origem.
  wa_tela_nxx-peso_chegada    = p_nit-peso_chegada.
  wa_tela_nxx-zpeso_diferenca = p_nit-zpeso_diferenca.
  wa_tela_nxx-zquebra         = p_nit-zquebra.
  wa_tela_nxx-zperda          = p_nit-zperda.
  wa_tela_nxx-zvlr_quebra     = p_nit-zvlr_quebra.
  wa_tela_nxx-zvlr_perda      = p_nit-zvlr_perda.
  wa_tela_nxx-zvlr_liq_pagar  = p_nit-zvlr_liq_pagar.
  wa_tela_nxx-zmatnr_merc     = p_nit-zmatnr_merc.
  wa_tela_nxx-zvlr_kg_transp  = p_nit-zvlr_kg_transp.
  wa_tela_nxx-zvlr_kg_mercad  = p_nit-zvlr_kg_mercad.

  CALL METHOD ctl_alv_nit->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_nit
      es_row_no   = gs_scroll_row_nit.

  LEAVE TO SCREEN 0201.

ENDFORM.                    " EDITAR_LINHA_NOTA_NIT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_NIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_nit .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_nit> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_CTE_DIST_NIT_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_nit.

  LOOP AT it_catalog_nit ASSIGNING <fs_cat_nit>.
    CASE <fs_cat_nit>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_nit>-col_pos = 1.
      WHEN 'ITMNUM'.
        <fs_cat_nit>-col_pos = 2.
      WHEN 'MAKTX'.
        <fs_cat_nit>-col_pos = 3.
    ENDCASE.
  ENDLOOP.

  lc_col_pos = 4.

  LOOP AT it_catalog_nit ASSIGNING <fs_cat_nit>.
    IF <fs_cat_nit>-col_pos IS INITIAL.
      <fs_cat_nit>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.
    <fs_cat_nit>-tabname = 'IT_CTE_NIT'.
    CASE <fs_cat_nit>-fieldname.
      WHEN 'ROWCOLOR'.
        <fs_cat_nit>-no_out  = abap_true.
      WHEN 'IC_EDITAR'.
        <fs_cat_nit>-key     = abap_true.
        <fs_cat_nit>-hotspot = abap_true.
        <fs_cat_nit>-just    = 'C'.
      WHEN 'ZVLR_FRETE'  OR 'ZVLR_MERCADORIA' OR 'PESO_ORIGEM' OR 'PESO_CHEGADA' OR 'ZPESO_DIFERENCA' OR
           'ZVLR_QUEBRA' OR 'ZVLR_PERDA' OR 'ZVLR_LIQ_PAGAR' OR 'ZQUEBRA' OR 'ZPERDA'.
        <fs_cat_nit>-do_sum  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_NIT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_NIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_nit .
  gs_var_nit-report     = sy-repid.
  gs_var_nit-handle     = '0005'.
  gs_var_nit-log_group  = abap_false.
  gs_var_nit-username   = abap_false.
  gs_var_nit-variant    = abap_false.
  gs_var_nit-text       = abap_false.
  gs_var_nit-dependvars = abap_false.
ENDFORM.                    " FILL_GS_VARIANT_NIT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_LINHA_NOTA_NIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_linha_nota_nit .

  FIELD-SYMBOLS <nit> TYPE zde_cte_dist_nit_alv.

  READ TABLE it_cte_nit_loc ASSIGNING <nit> WITH KEY docnum = wa_tela_nxx-docnum
                                                     itmnum = wa_tela_nxx-itmnum.
  IF sy-subrc IS INITIAL.
    <nit>-zvlr_frete       = wa_tela_nxx-zvlr_frete.
    <nit>-zvlr_kg_transp   = wa_tela_nxx-zvlr_kg_transp.
    <nit>-zvlr_kg_mercad   = wa_tela_nxx-zvlr_kg_mercad.
    <nit>-zvlr_mercadoria  = wa_tela_nxx-zvlr_mercadoria.
    <nit>-peso_origem      = wa_tela_nxx-peso_origem.
    <nit>-peso_chegada     = wa_tela_nxx-peso_chegada.
    <nit>-zpeso_diferenca  = wa_tela_nxx-zpeso_diferenca.
    <nit>-zquebra          = wa_tela_nxx-zquebra.
    <nit>-zvlr_quebra      = wa_tela_nxx-zvlr_quebra.
    <nit>-zperda           = wa_tela_nxx-zperda.
    <nit>-zvlr_perda       = wa_tela_nxx-zvlr_perda.
    <nit>-zvlr_liq_pagar   = wa_tela_nxx-zvlr_liq_pagar.
    <nit>-ck_peso_digitado = abap_true.
    <nit>-ic_editar        = icon_set_state.
  ENDIF.
  PERFORM dismarcar_todos.
  CLEAR: wa_tela_nxx.
  CALL METHOD ctl_alv_nit->refresh_table_display.

ENDFORM.                    " ATUALIZA_LINHA_NOTA_NIT

*&---------------------------------------------------------------------*
*&      Form  DISMARCAR_TODOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dismarcar_todos .

  FIELD-SYMBOLS: <nit> TYPE zde_cte_dist_nit_alv.

  LOOP AT it_cte_nit_loc ASSIGNING <nit>.
    CLEAR: <nit>-rowcolor.
  ENDLOOP.

ENDFORM.                    " DISMARCAR_TODOS
