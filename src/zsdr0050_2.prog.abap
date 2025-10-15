REPORT zsdr0050_2.

TYPE-POOLS: icon.

TABLES: vbak, zsdt0041.

SELECTION-SCREEN: BEGIN OF SCREEN 0100 AS SUBSCREEN.
  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME  TITLE TEXT-001.

    SELECT-OPTIONS: v_org FOR vbak-vkorg MODIF ID t1,                            " Organização de Vendas
                    v_spa FOR vbak-spart MODIF ID t1,                            "  Setor de Atividade
                    v_bur FOR vbak-vkbur MODIF ID t1,                            " Escritório de Vendas
                    v_grp FOR vbak-vkgrp MODIF ID t1 NO-EXTENSION NO INTERVALS,  "  Vendedor
                    v_aua FOR vbak-auart MODIF ID t1,                            " Tipo de Ordem Venda
                    v_dat FOR vbak-erdat MODIF ID t1,                            " Data Ordem de Venda
                    v_kur FOR vbak-kunnr MODIF ID t1,                                       " Cliente da Documento de Simulação
                    v_doc FOR zsdt0041-doc_simulacao MODIF ID t1 NO-EXTENSION NO INTERVALS, " Documento de Simulação
                    v_ov  FOR zsdt0041-vbeln MODIF ID t1 NO-EXTENSION NO INTERVALS.
  SELECTION-SCREEN: END OF BLOCK b1.

  SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_aur RADIOBUTTON GROUP a1 USER-COMMAND mudar_tela DEFAULT 'X', " Autorizar
                p_aua RADIOBUTTON GROUP a1,             " Autorizada
                p_wor RADIOBUTTON GROUP a1,             " Workflow
                p_rej RADIOBUTTON GROUP a1.             " Rejeitada
  SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: END OF SCREEN 0100.


TYPES:
  BEGIN OF  ty_saida,
    status    TYPE c LENGTH 4,
    vbeln     TYPE vbak-vbeln,
    "seq       TYPE zsde_embarque_seq,
    doc_41    TYPE zsdt0041-doc_simulacao,
    doc_90    TYPE zsdt0090-doc_simulacao,
    vkorg     TYPE vbak-vkorg,
    kunnr     TYPE vbak-kunnr,
    name1     TYPE kna1-name1,
    vkbur     TYPE vbak-vkbur,
    posnr     TYPE vbap-posnr,
    matnr     TYPE vbap-matnr,
    arktx     TYPE vbap-arktx,
    wrkst     TYPE mara-wrkst,
    meins     TYPE vbap-meins,
    kwmeng    TYPE vbap-kwmeng,
    " 13.06.2025 - RAMON - 169497 -->
    menge_l   TYPE vbap-kwmeng,
    meins_l   TYPE vbap-meins,
    kwert_l   TYPE netwr_ak,
    kwert_usd TYPE netwr_ak,
    tipo_reg  TYPE flag,
    " 13.06.2025 - RAMON - 169497 --<
    kwert     TYPE konv-kwert,
    waerk     TYPE vbak-waerk,
*#140282 -  ITSOUZA - 24.05.2024 11:08:30 - Inicio
    kursf     TYPE zsdt0040-kursf,
    dtpgtcult TYPE zsdt0040-dtpgtcult,
    cultura   TYPE zsdt0040-cultura, " 13.06.2025 - RAMON - 169497
    safra     TYPE zsdt0040-safra, "#140282 -  AHSS - 31.05.2024"
    safra_apl TYPE zsdt0041-safra_apl,
    tpsim     TYPE zsdt0040-tpsim, "##149354 - AHSS - 22.08.2024
*#140282 -  ITSOUZA - 24.05.2024 11:08:30 - Fim
  END OF ty_saida,

  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln,
    kunnr TYPE vbak-kunnr,
    vkbur TYPE vbak-vkbur,
  END OF ty_vbak.

TYPES: BEGIN OF ty_centro,
         bukrs TYPE vbak-vkbur.
TYPES END OF ty_centro.

TYPES: BEGIN OF ty_zsdt0116.
         INCLUDE STRUCTURE zsdt0116.
TYPES desc_status(30).
TYPES END OF ty_zsdt0116.

TYPES: it_saida TYPE TABLE OF ty_saida WITH DEFAULT KEY.

DATA:
  it_vbak      TYPE TABLE OF vbak,
  it_vbap      TYPE TABLE OF vbap,
  it_vbap_ttl  TYPE TABLE OF vbap, " 18.06.2025 - RAMON - 169497
  it_0116_sum  TYPE TABLE OF zsd_in_est_zsdt0116_sum,
  it_0040      TYPE TABLE OF zsdt0040,
  it_0041      TYPE TABLE OF zsdt0041,
  it_0090      TYPE TABLE OF zsdt0090,
  it_0060      TYPE TABLE OF zsdt0060,
  it_0116      TYPE TABLE OF zsdt0116,
  it_0115      TYPE TABLE OF zsdt0115,
  it_0116_aux  TYPE TABLE OF zsdt0116,
  it_0116_new  TYPE TABLE OF zsdt0116,
  "it_mara      TYPE TABLE OF mara,  " 18.06.2025 - RAMON - 169497
  it_kna1      TYPE TABLE OF kna1,
  it_saida1    TYPE TABLE OF ty_saida,
  it_saida_l_c TYPE TABLE OF ty_saida,
  wa_0116      TYPE zsdt0116,
  wa_vbap      TYPE vbap,
  wa_0041      TYPE zsdt0041,
  wa_0090      TYPE zsdt0090,
  it_centro    TYPE TABLE OF ty_centro WITH HEADER LINE,
  it_historico TYPE TABLE OF ty_zsdt0116,
  it_his_saida TYPE TABLE OF ty_zsdt0116,
  it_estrat    TYPE TABLE OF zsd_estrategia_ov,
  wa_estrat    TYPE zsd_estrategia_ov,
  wa_historico TYPE ty_zsdt0116,
  wa_his_saida TYPE ty_zsdt0116,
  rg_vkbur     TYPE RANGE OF vkbur,
  wa_vkbur     LIKE LINE OF rg_vkbur,
  r_vbeln      TYPE RANGE OF vbeln,
  r_simu       TYPE RANGE OF zsdt0041-doc_simulacao.

DATA:
  c_alv_tm    TYPE REF TO cl_alv_grid_toolbar_manager,
  ty_toolbar  TYPE stb_button,
  wa_cont     TYPE REF TO cl_gui_custom_container,
  wa_cont_h   TYPE REF TO cl_gui_custom_container,
  wa_cont_est TYPE REF TO cl_gui_custom_container,
  wa_alv      TYPE REF TO cl_gui_alv_grid,
  wa_alv_h    TYPE REF TO cl_gui_alv_grid,
  wa_alv_est  TYPE REF TO cl_gui_alv_grid,
  it_fcat     TYPE lvc_t_fcat,
  it_fcat_h   TYPE lvc_t_fcat,
  it_fcat_est TYPE lvc_t_fcat,
  wa_fcat     TYPE lvc_s_fcat,
  wa_variante TYPE disvariant,
  wa_stable   TYPE lvc_s_stbl,
  wa_layout   TYPE lvc_s_layo,
  it_sel_rows TYPE lvc_t_row,
  wa_sel_rows TYPE lvc_s_row.

DATA: p_var        TYPE rsvar-variant,
      p_dvar       TYPE rsvar-vtext,
      ok_code      TYPE sy-ucomm,
      g_sel_var    TYPE rsvar-variant,
      g_sel_vartxt TYPE rsvar-vtext.

START-OF-SELECTION.

  CALL SCREEN 0101.

END-OF-SELECTION.

CLASS zcl_events DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_alv_grid  TYPE REF TO cl_gui_alv_grid,
      on_handle   FOR EVENT user_command          OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_dt_ch    FOR EVENT data_changed          OF cl_gui_alv_grid IMPORTING er_data_changed  e_onf4 e_onf4_before e_onf4_after e_ucomm,
      on_toolbar  FOR EVENT toolbar               OF cl_gui_alv_grid IMPORTING e_object e_interactive sender,
      double_click FOR EVENT double_click          OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.

ENDCLASS.

CLASS zcl_aprovadores DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS: check_sld_comp_ov IMPORTING
                                       i_posnr TYPE vbap-posnr
                                       i_vbeln TYPE vbak-vbeln
                                     EXPORTING
                                       e_saldo TYPE c.

    METHODS: m_check_campos,
      m_seleciona_dados,
      m_seleciona_dados_2,
      m_agrupa_dados,
      m_agrupa_item IMPORTING iv_index TYPE i CHANGING cs_saida TYPE ty_saida, " 17.06.2025 - RAMON -- 169497
      m_monta_alv,
      montar_lay,
      cria_alv,
      embarque_l,
      " 17.06.2025 - RAMON -- 169497 -->
      embarque_l_2,

      popup_to_inform IMPORTING it_ret TYPE bapiret2_t,

      calcula_preco IMPORTING iv_preco          TYPE kwert
                              iv_menge_l        TYPE kwmeng
                              iv_menge_t        TYPE kwmeng
                    RETURNING VALUE(rv_preco_n) TYPE kwert,

      " 17.06.2025 - RAMON -- 169497 --<

      embarque_c,
      historico,
      estrategia.

ENDCLASS.

CLASS zcl_events IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT c_alv_tm
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "CONTRUCTOR

  METHOD on_handle.

    FREE: it_sel_rows[], wa_sel_rows, it_saida_l_c.

    CALL METHOD wa_alv->get_selected_rows
      IMPORTING
        et_index_rows = it_sel_rows.

    IF it_sel_rows IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-013.
    ENDIF.

    CHECK NOT it_sel_rows IS INITIAL.

    LOOP AT it_sel_rows INTO wa_sel_rows.
      READ TABLE it_saida1 INTO DATA(wa_saida) INDEX wa_sel_rows-index.
      APPEND wa_saida TO it_saida_l_c.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'LIBERAR'.

        " 13.06.2025 - RAMON - 169497 -->
*        IF sy-uname NE 'RBLIMA'.
*          NEW zcl_aprovadores( )->embarque_l( ).
*        ELSE.
        NEW zcl_aprovadores( )->embarque_l_2( ).
*        ENDIF.
        " 13.06.2025 - RAMON - 169497 --<

      WHEN 'CANCELAR'.
        NEW zcl_aprovadores( )->embarque_c( ).
      WHEN 'HISTORICO'.
        NEW zcl_aprovadores( )->historico( ).
      WHEN 'ESTRAT'.
        NEW zcl_aprovadores( )->estrategia( ).
    ENDCASE.

    COMMIT WORK.

    "IF sy-uname = 'RBLIMA'.
    NEW zcl_aprovadores( )->m_seleciona_dados_2( ).
    "ELSE.
    "NEW zcl_aprovadores( )->m_seleciona_dados( ).
    "ENDIF.

    CALL METHOD wa_alv->refresh_table_display.

  ENDMETHOD.                    "ON_DEL


  METHOD on_dt_ch.

    DATA lv_kwmeng TYPE kwmeng.
    DATA lv_diaant TYPE sydatum.
    DATA lv_fieldname TYPE c LENGTH 30.
    DATA lv_value TYPE p LENGTH 8 DECIMALS 3.

    DATA ls_stable TYPE lvc_s_stbl.
    DATA: obj_taxa TYPE REF TO zcl_util_sd.
    DATA: s_data  TYPE gdatu_inv,
          s_ukurs TYPE ukurs_curr.

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    lv_diaant = sy-datum - 1.

    s_data = lv_diaant.

    CREATE OBJECT obj_taxa.
    obj_taxa->set_kurst( 'B' ).
    obj_taxa->set_waerk( 'USD' ).
    obj_taxa->set_tcurr( 'BRL' ).

    obj_taxa->set_data( s_data ).
    s_ukurs = obj_taxa->taxa_cambio( ).

    " 13.06.2025 - RAMON - 169497 -->
    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_mod>).

      READ TABLE it_saida1 ASSIGNING FIELD-SYMBOL(<fs_alv>)
        INDEX <fs_mod>-row_id.

      CHECK sy-subrc EQ 0.

      lv_fieldname = '<FS_ALV>-' && <fs_mod>-fieldname.

      ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_field>).

      CHECK sy-subrc EQ 0.

      <fs_field> = <fs_mod>-value.

*      IF <fs_alv>-menge_l IS NOT INITIAL.
*
*        <fs_alv>-meins_l = <fs_alv>-meins.
*        "<fs_alv>-kwert_l = ( <fs_alv>-kwert * <fs_alv>-menge_l ) / <fs_alv>-kwmeng.
*
*        <fs_alv>-kwert_l = NEW zcl_aprovadores( )->calcula_preco(
*                                      EXPORTING iv_preco = <fs_alv>-kwert
*                                                iv_menge_l = <fs_alv>-menge_l
*                                                iv_menge_t = <fs_alv>-kwmeng ).
*
*      ELSE.
*
*        CLEAR <fs_alv>-meins_l.
*
*        <fs_alv>-kwert_l = 0.
*
*      ENDIF.

      " 18.06.2025 - RAMON - 169497 -->
      IF <fs_alv>-kwert_l IS NOT INITIAL.

        IF <fs_alv>-kwert_l > <fs_alv>-kwert.

          <fs_alv>-kwert_l = <fs_alv>-kwert.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = <fs_mod>-row_id
              i_fieldname = 'KWERT_L'
              i_value     = <fs_alv>-kwert_l.

        ENDIF.

        IF <fs_alv>-waerk <> 'USD'.
          IF <fs_alv>-kursf IS NOT INITIAL.
            <fs_alv>-kwert_usd = <fs_alv>-kwert_l / <fs_alv>-kursf.
          ELSE.

*        SE CONDIÇÃO DE PAGAMENTO FOR BN OU PM APLICAR A REGRA ABAIXO:
*       Gabriel Ávila - 12.08.2025
            IF ( <fs_alv>-tpsim EQ 'PM' OR <fs_alv>-tpsim EQ 'BN' ) AND s_ukurs IS NOT INITIAL.

              <fs_alv>-kwert_usd = <fs_alv>-kwert_l / s_ukurs.

            ELSE.
              <fs_alv>-kwert_usd = <fs_alv>-kwert_l / 1.
            ENDIF.
*       Gabriel Ávila - 12.08.2025

          ENDIF.

        ELSE.
          <fs_alv>-kwert_usd = <fs_alv>-kwert_l.
        ENDIF.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = <fs_mod>-row_id
            i_fieldname = 'KWERT_USD'
            i_value     = <fs_alv>-kwert_usd.


        <fs_alv>-menge_l = ( <fs_alv>-kwert_l * <fs_alv>-kwmeng  ) /  <fs_alv>-kwert.

        "lv_kwmeng = <fs_alv>-menge_l mod 2.

*        IF .
*
*        ENDIF.

      ENDIF.
      " 18.06.2025 - RAMON - 169497 --<

    ENDLOOP.




    IF NOT wa_alv IS INITIAL.
      CALL METHOD wa_alv->refresh_table_display
        EXPORTING
          is_stable = ls_stable.
    ENDIF.

    " 13.06.2025 - RAMON - 169497 --<

  ENDMETHOD.

  METHOD double_click.

    READ TABLE it_saida1 ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX e_row.

    CHECK sy-subrc EQ 0.


    CASE e_column.

      WHEN 'VBELN'.

        SET PARAMETER ID 'AUN' FIELD <fs_alv>-vbeln.

        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.

  METHOD on_toolbar.

* Incluir novos Botoes
    FREE: ty_toolbar.
    DEFINE toobar.
      ty_toolbar-icon      = &1.
      ty_toolbar-function  = &2.
      ty_toolbar-quickinfo = &3.
      ty_toolbar-text      = &4.
      ty_toolbar-butn_type = &5.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    END-OF-DEFINITION.

    IF it_0060[] IS INITIAL AND it_0115[] IS NOT INITIAL AND it_saida1[] IS NOT INITIAL.

      CASE p_aur.

        WHEN 'X'.
          toobar:
                  '@4A@' 'LIBERAR'  'Autorizar para Embarque' 'Autorizar' 0,
                  ''     ''         ''                        ''          3.
        WHEN ''.
          toobar:
                  '@UR@' 'CANCELAR' 'Cancelar Autorização'    'Cancelar'  0,
                  ''     ''         ''                        ''          3.
      ENDCASE.

    ENDIF.

*AH
    toobar:
        '@16@' 'HISTORICO' 'Histórico'    'Histórico'  0,
        ''     ''          ''             ''           3.

    toobar:
        '@OQ@' 'ESTRAT'   'Estratégia Liberação'      'Estratégia Liberação'  0,
        ''     ''          ''             ''           3.

    CALL METHOD c_alv_tm->reorganize( io_alv_toolbar = e_object ).

  ENDMETHOD.

ENDCLASS.

CLASS zcl_aprovadores IMPLEMENTATION.

  METHOD m_check_campos.

    DATA:
*          it_0115           TYPE TABLE OF zsdt0115,
*          wa_0115           TYPE zsdt0115,
      centro_block(255),
      cont              TYPE n,
      w_bur             LIKE v_bur,
      wa_centro         TYPE ty_centro.

    FREE: it_vbak, it_vbap, it_vbap_ttl, "it_mara, " 18.06.2025 - RAMON - 169497
          it_0041,
          it_0090, it_kna1, it_0116, it_0060, it_0115, it_saida1.

    IF NOT wa_alv IS INITIAL.
      CALL METHOD wa_alv->refresh_table_display.
    ENDIF.


    IF v_org-low IS INITIAL. MESSAGE s836(sd) WITH TEXT-004 TEXT-007. EXIT. ENDIF.
    IF v_bur-low IS INITIAL. MESSAGE s836(sd) WITH TEXT-004 TEXT-008. EXIT. ENDIF.
    IF v_bur-low EQ '*'.     MESSAGE s836(sd) WITH          TEXT-011. EXIT. ENDIF.
*#140787 -  ITSOUZA - 28.05.2024 22:22:28 - Inicio
*    IF v_aua-low IS INITIAL. MESSAGE s836(sd) WITH TEXT-004 TEXT-005. EXIT. ENDIF.
*#140787 -  ITSOUZA - 28.05.2024 22:22:28 - Fim
    IF v_dat-low IS INITIAL. MESSAGE s836(sd) WITH TEXT-004 TEXT-006. EXIT. ENDIF.


*    SELECT *
*      FROM zsdt0115
*      INTO TABLE it_0115
*      WHERE werks IN v_bur
*        AND uname EQ sy-uname.
*
*    IF NOT v_bur-high IS INITIAL.
*      WHILE v_bur-low NE v_bur-high.
*        ADD 1 TO cont.
*
*        READ TABLE it_0115 TRANSPORTING NO FIELDS WITH KEY werks = v_bur-low.
*        IF NOT sy-subrc IS INITIAL.
*          CONCATENATE centro_block v_bur-low INTO centro_block SEPARATED BY ','.
*          MOVE v_bur-low TO wa_centro.
*          APPEND wa_centro TO it_centro.
*
*          IF cont EQ 3.
*            CONCATENATE centro_block '...' INTO centro_block.
*            EXIT.
*          ENDIF.
*
*        ENDIF.
*
*        v_bur-low = v_bur-low + 1.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = v_bur-low
*          IMPORTING
*            output = v_bur-low.
*
*      ENDWHILE.
*    ELSE.
*
*      LOOP AT v_bur INTO w_bur.
*
*        READ TABLE it_0115 TRANSPORTING NO FIELDS WITH KEY werks = w_bur-low.
*        IF NOT sy-subrc IS INITIAL.
*          CONCATENATE centro_block v_bur-low INTO centro_block SEPARATED BY ','.
*          MOVE v_bur-low TO wa_centro.
*          APPEND wa_centro TO it_centro.
*
*          IF sy-tabix EQ 3.
*            CONCATENATE centro_block '...' INTO centro_block.
*            EXIT.
*          ENDIF.
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    IF NOT centro_block IS INITIAL.
*      SHIFT centro_block LEFT DELETING LEADING ','.
*      MESSAGE s836(sd) WITH text-002 text-009 centro_block.
*      EXIT.
*    ENDIF.

    "IF sy-uname = 'RBLIMA'.
    NEW zcl_aprovadores( )->m_seleciona_dados_2( ).
    "ELSE.
    "NEW zcl_aprovadores( )->m_seleciona_dados( ).
    "ENDIF.


* RJF - Ini - 60880 - CS2021000635 ZSDT0100 - Liberar visualização sem os botões de ação de liberar/reprovar para não aprovadores;
    DATA(it_vkbur) = it_saida1[].

    IF it_vkbur[] IS NOT INITIAL.

      DELETE ADJACENT DUPLICATES FROM it_vkbur COMPARING vkbur.

      LOOP AT it_vkbur ASSIGNING FIELD-SYMBOL(<fs_vkbur>).
        wa_vkbur-sign   = 'I'.
        wa_vkbur-option = 'EQ'.
        wa_vkbur-low    = <fs_vkbur>-vkbur.
        APPEND wa_vkbur TO rg_vkbur.
        CLEAR wa_vkbur.
      ENDLOOP.

* Verificar se usuário esteja na tabela zsdt0115 ( GERENTE ).
      SELECT *
      FROM zsdt0115
      INTO TABLE @it_0115
      WHERE werks IN @rg_vkbur
        AND uname EQ @sy-uname.

      IF sy-subrc IS INITIAL.

        CLEAR: wa_vkbur, rg_vkbur[].
        LOOP AT it_0115 ASSIGNING FIELD-SYMBOL(<fs_0115>).
          wa_vkbur-sign   = 'I'.
          wa_vkbur-option = 'EQ'.
          wa_vkbur-low    = <fs_0115>-werks.
          APPEND wa_vkbur TO rg_vkbur.
        ENDLOOP.

        SELECT * FROM tvko
          INTO TABLE @DATA(it_tvko)
          WHERE vkorg IN @v_org.

        IF sy-subrc IS INITIAL.
*---> 04/07/2023 - Migração S4 - WS
          SORT it_tvko  BY vkorg.
*<--- 04/07/2023 - Migração S4 - WS
          DELETE ADJACENT DUPLICATES FROM it_tvko COMPARING vkorg.
          DATA(lv_l_tvko) = lines( it_tvko ).
          DATA(lv_l_0115) = lines( it_0115 ).
          IF lv_l_0115 LT lv_l_tvko.
            FREE it_saida1[].
          ENDIF.
        ENDIF.

*          DELETE it_saida1[] WHERE vkbur NOT IN rg_vkbur[].

        IF it_saida1[] IS INITIAL.
          MESSAGE s836(sd) WITH TEXT-002 TEXT-009 'ou (os)...'(019)."wa_vkbur. "centro_block.
          CLEAR wa_vkbur.
          EXIT.
        ENDIF.

      ELSE.
* Verificar se usuário esteja na tabela zsdt0060.
        SELECT *
       FROM zsdt0060
       INTO TABLE @it_0060
       WHERE usnam    EQ @sy-uname
         AND programa EQ 'ZSDR016'"@sy-cprog // Alterado para verificar se tem cadastro na transação ZSDT0044.
         AND vkbur IN @rg_vkbur.

        IF sy-subrc IS INITIAL.

          CLEAR: wa_vkbur, rg_vkbur[].
          LOOP AT it_0060 ASSIGNING FIELD-SYMBOL(<fs_0060>).
            wa_vkbur-sign   = 'I'.
            wa_vkbur-option = 'EQ'.
            wa_vkbur-low    = <fs_0060>-vkbur.
            APPEND wa_vkbur TO rg_vkbur.
          ENDLOOP.

          DELETE it_saida1[] WHERE vkbur NOT IN rg_vkbur[].

          IF it_saida1[] IS INITIAL.
            MESSAGE s836(sd) WITH TEXT-002 TEXT-009."wa_vkbur. "centro_block.
            CLEAR wa_vkbur.
            EXIT.
          ENDIF.
        ELSE.
          IF it_saida1[] IS NOT INITIAL.
            FREE it_saida1.
            MESSAGE s836(sd) WITH TEXT-002 TEXT-009."wa_vkbur. "centro_block.
            CLEAR wa_vkbur.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
* RJF - Fim - 60880 - CS2021000635 ZSDT0100 - Liberar visualização sem os botões de ação de liberar/reprovar para não aprovadores;

  ENDMETHOD.

  METHOD m_seleciona_dados_2.

    CLEAR: it_saida1[],  r_vbeln[],r_simu[].

    DATA lv_antigo.
    DATA(lr_vkorg) = v_org[].
    DATA(lr_spart) = v_spa[].
    DATA(lr_vkbur) = v_bur[].
    DATA(lr_vkgrp) = v_grp[].
    DATA(lr_auart) = v_aua[].
    DATA(lr_erdat) = v_dat[].
    DATA lv_arredond TYPE netwr.
    DATA lv_arredo_dolar TYPE netwr.

    IF v_doc IS NOT INITIAL.
      APPEND v_doc TO r_simu.
      CLEAR: lr_vkorg, lr_spart, lr_vkbur, lr_vkgrp, lr_auart, lr_erdat.
    ENDIF.

    IF v_ov IS NOT INITIAL.
      APPEND v_ov TO r_vbeln.
      CLEAR: lr_vkorg, lr_spart, lr_vkbur, lr_vkgrp, lr_auart, lr_erdat.
    ENDIF.

    TRY .
        SELECT SINGLE low FROM tvarvc
          INTO @DATA(lv_vlr)
            WHERE name = 'ZSDT0100_ARREDON_DOLAR'.

        lv_arredond = lv_vlr.

      CATCH cx_sy_conversion_no_number.
        lv_arredond = '1.00'.
    ENDTRY.

    SELECT doc_simulacao, vbeln,vkorg, kunnr,name1 FROM zi_est_filtro_01
      INTO TABLE @DATA(lt_simu)
        WHERE vkorg IN @lr_vkorg
         AND spart IN @lr_spart
         AND vkbur IN @lr_vkbur
         AND vkgrp IN @lr_vkgrp
         AND auart IN @lr_auart
         AND erdat IN @lr_erdat
         AND vbeln IN @r_vbeln
         AND kunnr IN @v_kur
         AND doc_simulacao IN @r_simu
          AND  auart NOT IN ('ZRPF','ZROB','ZREB').

    CHECK sy-subrc EQ 0.

    SELECT * FROM zsd_in_est_saldo_01
      INTO TABLE @DATA(lt_docs)
        FOR ALL ENTRIES IN @lt_simu
          WHERE doc_simulacao = @lt_simu-doc_simulacao
            AND vbeln = @lt_simu-vbeln.

    CHECK sy-subrc EQ 0.

    " 28.08.2025 -->
    SELECT * FROM zsdt0116
        FOR ALL ENTRIES IN @lt_docs
          WHERE vbeln = @lt_docs-vbeln
            AND posnr IS NOT INITIAL
            AND status <> 'X'
    INTO TABLE @DATA(lt_antigos).

    " 28.08.2025 --<

    LOOP AT lt_docs ASSIGNING FIELD-SYMBOL(<fs_docs>).

      CLEAR lv_antigo.

      DATA(ls_antigo) = VALUE #( lt_antigos[ vbeln = <fs_docs>-vbeln ] DEFAULT '' ).

      IF ls_antigo IS NOT INITIAL.
        lv_antigo = abap_true.
      ENDIF.

      IF <fs_docs>-saldo_disponivel_ov_m > 0.

        IF <fs_docs>-waerk <> 'USD'.
          IF <fs_docs>-kurrf > 0.
            lv_arredo_dolar = <fs_docs>-saldo_disponivel_ov_m / <fs_docs>-kurrf.
          ELSE.
            lv_arredo_dolar = <fs_docs>-saldo_disponivel_ov / 1.
          ENDIF.
        ELSE.
          lv_arredo_dolar = <fs_docs>-saldo_disponivel_ov.
        ENDIF.

        CHECK lv_arredo_dolar > lv_arredond.

      ENDIF.

      APPEND INITIAL LINE TO it_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida>).

      READ TABLE lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>)
        WITH KEY vbeln = <fs_docs>-vbeln.

      <fs_saida>-vbeln    = <fs_docs>-vbeln.
      "<fs_saida>-seq    = <fs_docs>-seq.
      <fs_saida>-doc_41   = <fs_docs>-doc_simulacao.
      <fs_saida>-kunnr    = <fs_docs>-kunnr.
      <fs_saida>-name1    = <fs_simu>-name1.
      <fs_saida>-vkorg = <fs_docs>-vkorg.
      <fs_saida>-vkbur = <fs_docs>-vkbur.
      <fs_saida>-waerk = <fs_docs>-waerk.
      <fs_saida>-cultura = <fs_docs>-cultura.
      <fs_saida>-safra = <fs_docs>-safra.
      <fs_saida>-safra_apl = <fs_docs>-safra.
      <fs_saida>-dtpgtcult = <fs_docs>-venci.
      <fs_saida>-tpsim = <fs_docs>-tpsim.
      <fs_saida>-kursf = <fs_docs>-kurrf.

      CASE abap_true.
        WHEN p_aur.

          <fs_saida>-status = '@AH@'.

          IF <fs_docs>-saldo_disponivel_ov > 0.
            <fs_saida>-tipo_reg = 'A'. "<- para autorizar
            <fs_saida>-kwert = <fs_docs>-saldo_disponivel_ov_m.
            <fs_saida>-kwert_l = 0.
            " 06.10.2025 -->
*          ELSEIF <fs_docs>-saldo_pendente_ov > 0.
*            <fs_saida>-tipo_reg = 'A'. "<- para autorizar
*            <fs_saida>-kwert = <fs_docs>-saldo_disponivel_ov_m.
*            <fs_saida>-kwert_l = saldo_pendente_ov.
          ENDIF.
          " 06.10.2025 --<
        WHEN p_aua.

          <fs_saida>-status = '@MF@'.

          IF <fs_docs>-vlrtotalautorizado = <fs_docs>-valor_total_ov.
            <fs_saida>-tipo_reg = 'T'. "<- 100% aprovada

            <fs_saida>-kwert = <fs_docs>-valor_total_ov_m.
            <fs_saida>-kwert_l = <fs_saida>-kwert.


          ELSEIF <fs_docs>-vlrtotalautorizado > 0.

            <fs_saida>-tipo_reg = 'T'.
            <fs_saida>-kwert = <fs_docs>-valor_total_ov_m.
            <fs_saida>-kwert_l = <fs_docs>-vlrtotalautorizado_m.

            " 28.08.2025 -->
          ELSEIF lv_antigo = abap_true.

            IF ls_antigo-status_workflow = 'A'.
              <fs_saida>-tipo_reg = 'T'.
            ENDIF.

            " 28.08.2025 --<
          ENDIF.

          IF <fs_saida>-waerk = 'USD'.
            <fs_saida>-kwert_usd = <fs_saida>-kwert_l.
          ELSE.
            <fs_saida>-kwert_usd = <fs_docs>-vlrtotalautorizado.
          ENDIF.

          " 28.08.2025 -->
          IF <fs_saida>-kwert_usd IS INITIAL AND lv_antigo = abap_true.
            <fs_saida>-kwert = <fs_docs>-valor_total_ov.
          ENDIF.
          " 28.08.2025 --<

        WHEN p_wor.

          <fs_saida>-status = '@OQ@'.

          IF <fs_docs>-valor_liberado_ov_apr_m > 0.
            <fs_saida>-tipo_reg = 'W'. " <- Liberado WF

            <fs_saida>-kwert = <fs_docs>-valor_total_ov_m.
            <fs_saida>-kwert_l = <fs_docs>-valor_liberado_ov_apr_m.

            " 01.09.2025 -->
          ELSEIF lv_antigo = abap_true.

            IF ls_antigo-status_workflow = 'L'.
              <fs_saida>-tipo_reg = 'W'.
            ENDIF.

            " 01.09.2025 --<
          ENDIF.
          "ENDIF.

          " 01.09.2025 -->
          IF lv_antigo = abap_true.

**            IF <fs_saida>-waerk = 'USD'.
**              <fs_saida>-kwert_usd = <fs_saida>-kwert_l. <-- colocar valor do item da ov
**            ELSE.
**              <fs_saida>-kwert_usd = <fs_docs>-valor_liberado_ov_apr. <-- colocar valor do item da ov
**            ENDIF.

          ELSE.

            IF <fs_saida>-waerk = 'USD'.
              <fs_saida>-kwert_usd = <fs_saida>-kwert_l.
            ELSE.
              <fs_saida>-kwert_usd = <fs_docs>-valor_liberado_ov_apr.
            ENDIF.

          ENDIF.
          " 01.09.2025 --<


        WHEN p_rej.

          <fs_saida>-status = '@8Y@'.

          IF <fs_docs>-valor_rejeitado_apr > 0.
            <fs_saida>-tipo_reg = 'R'. "<- Rejeitada
            <fs_saida>-kwert = <fs_docs>-valor_total_ov.
            <fs_saida>-kwert_l = <fs_docs>-valor_rejeitado_apr.

            " 01.09.2025 -->
          ELSEIF lv_antigo = abap_true.

            IF ls_antigo-status_workflow = 'R' OR ls_antigo-status = 'X'.
              <fs_saida>-tipo_reg = 'R'. "<- Rejeitada
            ENDIF.

            " 01.09.2025 --<
          ENDIF.
          "ENDIF.

      ENDCASE.

    ENDLOOP.

    SORT it_saida1 BY vbeln.

    DELETE it_saida1 WHERE tipo_reg IS INITIAL.

    DELETE it_saida1 WHERE ( kwert IS INITIAL AND kwert_l IS INITIAL ).

    " 28.08.2025 -->
    LOOP AT lt_antigos ASSIGNING FIELD-SYMBOL(<fs_antigos>).

      CASE 'X'.
        WHEN p_aur. "Autorizar
          DELETE it_saida1 WHERE vbeln = <fs_antigos>-vbeln.
        WHEN p_aua. "Autorizadas

        WHEN p_wor. "Workflow
        WHEN p_rej. "Rejeitadas
      ENDCASE.

    ENDLOOP.

    " 28.08.2025 --<


  ENDMETHOD.

  METHOD m_seleciona_dados.

    FREE: it_vbak, it_vbap, it_vbap_ttl, "it_mara, " 18.06.2025 - RAMON - 169497
     it_0041,
          it_0090, it_kna1, it_0116, it_saida1, r_vbeln.


    IF v_ov IS NOT INITIAL.
      r_vbeln[] = v_ov[].
    ELSE.


      IF v_doc IS NOT INITIAL.

        SELECT
          'I'   AS sign,
          'EQ'  AS option,
          vbeln AS low
            FROM zsdt0041
             INTO TABLE @r_vbeln
              WHERE doc_simulacao IN @v_doc.

        SELECT
          'I'   AS sign,
          'EQ'  AS option,
          vbeln AS low
          FROM zsdt0090
          APPENDING TABLE @r_vbeln
              WHERE doc_simulacao IN @v_doc.

      ENDIF.

    ENDIF.

    SELECT *
      FROM zsdt0116
      INTO TABLE it_0116
        WHERE status NE 'X'
          AND vbeln IN r_vbeln.

    CASE 'X'.
      WHEN p_aur.

        SELECT *
          FROM vbak
        INTO TABLE it_vbak
            WHERE vkorg IN v_org
             AND spart IN v_spa
             AND vkbur IN v_bur
             AND vkgrp IN v_grp
             AND auart IN v_aua
             AND erdat IN v_dat
             AND vbeln IN r_vbeln.

      WHEN p_aua.

        IF NOT it_0116 IS INITIAL.

          SELECT * FROM vbak
            INTO TABLE it_vbak
              FOR ALL ENTRIES IN it_0116
              WHERE vbeln EQ it_0116-vbeln
                AND vkorg IN v_org
                AND spart IN v_spa
                AND vkbur IN v_bur
                AND vkgrp IN v_grp
                AND auart IN v_aua
                AND erdat IN v_dat.

        ENDIF.

      WHEN p_wor OR p_rej.

        IF NOT it_0116 IS INITIAL.

          SELECT * FROM vbak
            INTO TABLE it_vbak
              FOR ALL ENTRIES IN it_0116
              WHERE vbeln EQ it_0116-vbeln
                AND vkorg IN v_org
                AND spart IN v_spa
                AND vkbur IN v_bur
                AND vkgrp IN v_grp
                AND auart IN v_aua
                AND erdat IN v_dat.
        ENDIF.

    ENDCASE.

    CHECK NOT it_vbak IS INITIAL.

    SELECT * FROM zsd_in_est_zsdt0116_sum
      INTO TABLE @it_0116_sum
        FOR ALL ENTRIES IN @it_vbak
          WHERE vbeln EQ @it_vbak-vbeln.

*    SELECT vbap~* FROM vbap
*      INNER JOIN vbep
*      ON vbap~vbeln = vbep~vbeln
*      AND vbap~posnr = vbep~posnr
*      INTO TABLE @it_vbap
*        FOR ALL ENTRIES IN @it_vbak
*          WHERE vbap~vbeln EQ @it_vbak-vbeln
*                AND vbep~lifsp NE '12' .

    "IF NOT it_0116 IS INITIAL.

    DATA ls_vbap TYPE vbap.

    LOOP AT it_0116_sum ASSIGNING FIELD-SYMBOL(<fs_116>).

      CASE 'X'.
        WHEN p_aur.
          IF <fs_116>-saldo_disponivel IS INITIAL.
            DELETE it_vbak WHERE vbeln = <fs_116>-vbeln.
            CONTINUE.
          ENDIF.

        WHEN p_aua.

          IF <fs_116>-vlrtotalaprovado = <fs_116>-valor_ov.
            DELETE it_vbak WHERE vbeln = <fs_116>-vbeln.
            CONTINUE.
          ENDIF.

          " WHEN p_wor OR p_rej.
      ENDCASE.

      ls_vbap-vbeln = <fs_116>-vbeln.
      ls_vbap-netwr = <fs_116>-saldo_disponivel.

      COLLECT ls_vbap INTO it_vbap_ttl.

    ENDLOOP.


*      SELECT * FROM mara
*        INTO TABLE it_mara
*          FOR ALL ENTRIES IN it_vbap
*            WHERE matnr EQ it_vbap-matnr.

    "ENDIF.

    SELECT * FROM zsdt0041
      INTO TABLE it_0041
        FOR ALL ENTRIES IN it_vbak
          WHERE vbeln EQ it_vbak-vbeln
            AND doc_simulacao IN v_doc.

*#140282 -  ITSOUZA - 24.05.2024 11:12:07 - Inicio
    SELECT * FROM zsdt0040
      INTO TABLE it_0040
        FOR ALL ENTRIES IN it_0041
          WHERE doc_simulacao EQ it_0041-doc_simulacao.
*#140282 -  ITSOUZA - 24.05.2024 11:12:07 - Fim

    SELECT * FROM zsdt0090
      INTO TABLE it_0090
        FOR ALL ENTRIES IN it_vbak
          WHERE vbeln EQ it_vbak-vbeln
            AND doc_simulacao IN v_doc.

*#140282 -  ITSOUZA - 24.05.2024 11:12:07 - Inicio
    SELECT * FROM zsdt0040
      APPENDING TABLE it_0040
        FOR ALL ENTRIES IN it_0090
          WHERE doc_simulacao EQ it_0090-doc_simulacao.
*#140282 -  ITSOUZA - 24.05.2024 11:12:07 - Fim

    SELECT * FROM kna1
      INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_vbak
          WHERE kunnr EQ it_vbak-kunnr.

    NEW zcl_aprovadores( )->m_agrupa_dados( ).

  ENDMETHOD.

  METHOD m_agrupa_dados.

    DATA: v_kwert TYPE konv-kwert.
    DATA wa_saida TYPE ty_saida.
    DATA lr_tpsim TYPE RANGE OF zsdt0040-tpsim.

    lr_tpsim = VALUE #( sign = 'I' option = 'EQ' ( low = 'AD' )
                                                 ( low = 'VP' )
                                                 ( low = 'TS' )
                                                 ( low = 'TV' )
                                                 ( low = 'VF' ) ).

    LOOP AT it_vbak INTO DATA(wa_vbak).

      CASE 'X'.
        WHEN p_aur.
          wa_saida-status = '@AH@'.
        WHEN p_aua.
          wa_saida-status = '@MF@'.
        WHEN p_wor.
          wa_saida-status = '@OQ@'.
        WHEN p_rej.
          wa_saida-status = '@8Y@'.
      ENDCASE.

      MOVE wa_vbak-vbeln TO wa_saida-vbeln.
      MOVE wa_vbak-kunnr TO wa_saida-kunnr.
      MOVE wa_vbak-vkbur TO wa_saida-vkbur.
      MOVE wa_vbak-waerk TO wa_saida-waerk.

      LOOP AT it_vbap_ttl INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.
        "LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln. " 13.06.2025 - RAMON - 169497

        wa_saida-posnr  = wa_vbap-posnr.
        wa_saida-matnr  = wa_vbap-matnr.
        wa_saida-arktx  = wa_vbap-arktx.
        wa_saida-meins  = wa_vbap-meins.
        IF wa_vbap-meins = 'BIG'.
          wa_saida-meins = 'BAG'.
        ENDIF.
        wa_saida-kwmeng = wa_vbap-kwmeng.

        " 18.06.2025 - RAMON - 169497 -->

        READ TABLE it_0116_sum ASSIGNING FIELD-SYMBOL(<fs_sum>)
          WITH KEY vbeln = wa_vbak-vbeln.

        IF sy-subrc EQ 0.

*          IF <fs_sum>-saldo_disponivel IS NOT INITIAL.
*            wa_saida-tipo_reg = 'A'. "<- para autorizar
*          ENDIF.
*
*          IF <fs_sum>-valor_ov = <fs_sum>-vlrtotalaprovado.
*            wa_saida-tipo_reg = 'T'. "<- 100% aprovada
*          ENDIF.
*
*          IF <fs_sum>-valor_rejeitado_apr > 0.
*            wa_saida-tipo_reg = 'R'. "<- Rejeitada
*          ENDIF.
*
*          IF <fs_sum>-valor_liberado_apr > 0.
*            wa_saida-tipo_reg = 'W'. "<- WF
*          ENDIF.

          CASE abap_true.
            WHEN p_aur.
              wa_saida-kwert = <fs_sum>-saldo_disponivel.
            WHEN p_aua.
              wa_saida-kwert_l = <fs_sum>-vlrtotalaprovado.
              wa_saida-kwert = <fs_sum>-vlrtotalaprovado + <fs_sum>-saldo_disponivel.
            WHEN p_wor.
              wa_saida-kwert_l = <fs_sum>-valor_liberado_sap + <fs_sum>-valor_liberado_opus.
              wa_saida-kwert = <fs_sum>-saldo_disponivel.
            WHEN p_rej.
              wa_saida-kwert_l = <fs_sum>-valor_rejeitado_sap + <fs_sum>-valor_rejeitado_opus.
              wa_saida-kwert = <fs_sum>-saldo_disponivel.
          ENDCASE.


        ENDIF.



*        TRY.
*
*            cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
*              EXPORTING it_selection_attribute = VALUE #(
*             ( fieldname = 'KNUMV' value = wa_vbak-knumv )
*             ( fieldname = 'KPOSN' value = wa_vbap-posnr )
*             ( fieldname = 'KSCHL' value = 'PR00' )
*             )
*              IMPORTING et_prc_element_classic_format = DATA(etl582c8r4914) ).
*            v_kwert = etl582c8r4914[ 1 ]-kwert.
*          CATCH cx_prc_result cx_sy_itab_line_not_found .
*            sy-subrc = 4.
*        ENDTRY.
*
*        IF sy-subrc EQ 0.
*          MOVE v_kwert TO wa_saida-kwert.
*        ENDIF.

        " 18.06.2025 - RAMON - 169497 --<



        TRY .
            wa_saida-name1  = it_kna1[ kunnr = wa_vbak-kunnr ]-name1.
          CATCH cx_sy_itab_line_not_found.
            wa_saida-name1 = ''.
        ENDTRY.

*        TRY .
*            wa_saida-wrkst  = it_mara[ matnr = it_vbap[ vbeln = wa_vbak-vbeln ]-matnr ]-wrkst.
*          CATCH cx_sy_itab_line_not_found.
*            wa_saida-wrkst = ''.
*        ENDTRY.

*#140282 -  ITSOUZA - 24.05.2024 11:21:21 - Inicio
        READ TABLE it_0041 INTO DATA(ls_0041) WITH KEY vbeln = wa_vbap-vbeln.
        IF sy-subrc EQ 0.
          wa_saida-doc_41 = ls_0041-doc_simulacao.
          wa_saida-safra_apl = ls_0041-safra_apl.
        ELSE.
          wa_saida-doc_41    = ''.
          wa_saida-safra_apl = ''.
        ENDIF.

*        TRY .
*            wa_saida-doc_41 = it_0041[ vbeln = wa_vbap-vbeln ]-doc_simulacao.
*          CATCH cx_sy_itab_line_not_found.
*            wa_saida-doc_41 = ''.
*        ENDTRY.
*#140282 -  ITSOUZA - 24.05.2024 11:21:21 - Fim


        IF wa_saida-doc_41 IS INITIAL.
          TRY .
              wa_saida-doc_41 = it_0090[ vbeln = wa_vbap-vbeln ]-doc_simulacao.
            CATCH cx_sy_itab_line_not_found.
              wa_saida-doc_41 = ''.
          ENDTRY.
        ENDIF.

*#140282 -  ITSOUZA - 24.05.2024 11:21:21 - Inicio

        IF wa_saida-safra_apl IS INITIAL.
          SELECT SINGLE safra_apl INTO wa_saida-safra_apl
           FROM zsdt0041
            WHERE doc_simulacao EQ wa_saida-doc_41.
        ENDIF.

        "#140282 -  AHSS - 31.05.2024 - Inicio"
        "**********************************************************************
        "Le as tabelas"
*        READ TABLE it_0090 INTO DATA(ls_0090) WITH KEY doc_simulacao = wa_saida-doc_41
*                                                       vbelv         = wa_saida-vbeln
*                                                       categoria     = 'V'
*                                                       estorno       = abap_false .

        SELECT SINGLE * FROM zsdt0090
          WHERE vbelv = @wa_saida-vbeln
            AND doc_simulacao = @wa_saida-doc_41
            AND categoria     = 'V'
            AND estorno       = @abap_false
          INTO @DATA(l_zsdt0090).

        READ TABLE it_0040 INTO DATA(ls_0040) WITH KEY doc_simulacao = wa_saida-doc_41.
**********************************************************************
        "Pega o campo DATA DE VENCIMENTO"

        IF l_zsdt0090-valdt IS NOT INITIAL.
          wa_saida-dtpgtcult = l_zsdt0090-valdt .
        ELSE.
          IF ls_0040 IS NOT INITIAL.
            wa_saida-dtpgtcult = COND #( WHEN ls_0040-tpsim IN lr_tpsim THEN ls_0040-dtpgtcult
                                       ELSE ls_0040-dtvencov ).
          ELSE.
            wa_saida-dtpgtcult = ''.
          ENDIF.
        ENDIF.

        CLEAR l_zsdt0090.
**********************************************************************
        "Pega o campo SAFRA"
        IF ls_0040-safra IS NOT INITIAL.
          wa_saida-safra = ls_0040-safra. "#140282 -  AHSS - 31.05.2024"
        ELSE.
          wa_saida-safra = ''.            "#140282 -  AHSS - 31.05.2024"
        ENDIF.

        IF ls_0040-cultura IS NOT INITIAL.
          wa_saida-cultura = ls_0040-cultura.
        ENDIF.


        "##149354 - AHSS - 22.08.2024 Inicio
        "Pega o campo Condição de Pagamento"
        IF ls_0040-tpsim  IS NOT INITIAL.
          wa_saida-tpsim = ls_0040-tpsim.
        ELSE.
          wa_saida-tpsim = ''.
        ENDIF.
        "##149354 - AHSS - 22.08.2024 Fim

**********************************************************************
        "#140282 -  AHSS - 31.05.2024 - Fim"

        CLEAR: ls_0041,ls_0040.
*#140282 -  ITSOUZA - 24.05.2024 11:21:21 - Fim

        IF NOT wa_saida-doc_41 IS INITIAL.
          APPEND wa_saida TO it_saida1.
        ENDIF.


      ENDLOOP.
    ENDLOOP.

****    CASE 'X'.
****      WHEN p_aur. "Autorizar
****
****        " 17.06.2025 - RAMON -- 169497 -->
*****        LOOP AT it_0116 INTO DATA(wa_0116).
*****          DELETE it_saida1 WHERE vbeln EQ wa_0116-vbeln AND posnr EQ wa_0116-posnr.
*****        ENDLOOP.
****
*****        LOOP AT it_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida1>).
*****
*****          DATA(lv_index) = sy-tabix.
*****
*****          READ TABLE it_0116 INTO DATA(wa_0116)
*****            WITH KEY vbeln = <fs_saida1>-vbeln
*****                     posnr = <fs_saida1>-posnr.
*****
*****          CHECK sy-subrc EQ 0.
*****
*****          DELETE it_saida1 INDEX lv_index.
*****        ENDLOOP.
****
****        LOOP AT it_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida1>).
****
****          me->m_agrupa_item( EXPORTING iv_index = sy-tabix CHANGING cs_saida = <fs_saida1> ).
****
****        ENDLOOP.
****
****        " 17.06.2025 - RAMON -- 169497 --<
****
****      WHEN p_aua. "Autorizadas
****
****        LOOP AT it_saida1 INTO wa_saida.
****          READ TABLE it_0116 INTO wa_0116 WITH KEY vbeln = wa_saida-vbeln
****                                                   posnr = wa_saida-posnr.
****          IF ( sy-subrc NE 0 ) OR
****             ( sy-subrc EQ 0  AND
****               wa_0116-status_workflow NE 'A' AND
****               wa_0116-status_workflow IS NOT INITIAL ).
****            DELETE it_saida1
****                        WHERE vbeln EQ wa_saida-vbeln AND
****                              posnr EQ wa_saida-posnr.
****          ENDIF.
****
****        ENDLOOP.
****
****      WHEN p_wor. "Workflow
****
****        LOOP AT it_saida1 INTO wa_saida.
****          READ TABLE it_0116 INTO wa_0116 WITH KEY vbeln = wa_saida-vbeln
****                                                   posnr = wa_saida-posnr.
****
****          IF ( sy-subrc NE 0 ) OR
****             ( sy-subrc EQ 0 AND wa_0116-status_workflow IS INITIAL ) OR
****             ( sy-subrc EQ 0  AND
****               wa_0116-status_workflow NE 'L' AND
****               wa_0116-status_workflow IS NOT INITIAL ).
****            DELETE it_saida1
****                        WHERE vbeln EQ wa_saida-vbeln AND
****                              posnr EQ wa_saida-posnr.
****          ENDIF.
****
****        ENDLOOP.
****
****      WHEN p_rej. "Rejeitadas
****
****        LOOP AT it_saida1 INTO wa_saida.
****          READ TABLE it_0116 INTO wa_0116 WITH KEY vbeln = wa_saida-vbeln
****                                                   posnr = wa_saida-posnr.
****
****          IF ( sy-subrc NE 0 ) OR
****             ( sy-subrc EQ 0 AND wa_0116-status_workflow NE 'R' ).
*****               WA_0116-STATUS_WORKFLOW NE 'R' AND
*****               WA_0116-STATUS_WORKFLOW IS NOT INITIAL ).
****            DELETE it_saida1
****                        WHERE vbeln EQ wa_saida-vbeln AND
****                              posnr EQ wa_saida-posnr.
****          ENDIF.
****
****        ENDLOOP.
****
****
****    ENDCASE.

    SORT it_saida1 BY vbeln.

  ENDMETHOD.

  METHOD calcula_preco.
    rv_preco_n = ( iv_preco * iv_menge_l ) / iv_menge_t.
  ENDMETHOD.

  METHOD m_monta_alv.

    FREE: it_fcat, wa_fcat.

    DEFINE alv.
      wa_fcat-hotspot   = &1.
      wa_fcat-ref_table = &2.
      wa_fcat-ref_field = &3.
      wa_fcat-tabname   = &4.
      wa_fcat-fieldname = &5.
      wa_fcat-scrtext_l = &6.
      wa_fcat-scrtext_m = &6.
      wa_fcat-no_zero   = &7.
      wa_fcat-outputlen = &8.
      wa_fcat-edit      = &9.

      APPEND wa_fcat TO it_fcat.
      CLEAR wa_fcat.
    END-OF-DEFINITION.

    alv:
      '' '' '' 'IT_SAIDA_ALV' 'STATUS' 'Status'          ' '  '02' ' ',
      '' '' '' 'IT_SAIDA_ALV' 'VBELN'  'Ordem de Venda'  ' '  '15' ' ',
      '' '' '' 'IT_SAIDA_ALV' 'DOC_41' 'Simulador Venda' ' '  ' '  ' ',
      '' '' '' 'IT_SAIDA_ALV' 'TPSIM'  'Cond. Pagamento' ' '  '02' ' ',"##149354 - AHSS - 22.08.2024
      '' '' '' 'IT_SAIDA_ALV' 'KUNNR'  'Cliente'         'X'  ' '  ' ',
      '' '' '' 'IT_SAIDA_ALV' 'NAME1'  'Nome'            ' '  '20' ' ',
      '' '' '' 'IT_SAIDA_ALV' 'VKBUR'  'Escr. Venda'     'X'  ' '  ' ',
      '' '' '' 'IT_SAIDA_ALV' 'POSNR'  'Item'            ' '  ' '  ' ',
      '' '' '' 'IT_SAIDA_ALV' 'MATNR'  'Material'        'X'  ' '  ' ',
      '' '' '' 'IT_SAIDA_ALV' 'ARKTX'  'Desc Material'   ' '  '20' ' ',
*#140282 -  ITSOUZA - 24.05.2024 09:08:32 - Inicio
      '' '' '' 'IT_SAIDA_ALV' 'DTPGTCULT'  'Vencimento'  ' '  '10' ' ',
*     '' '' '' 'IT_SAIDA_ALV' 'SAFRA'      'Safra'       ' '  '5'  ' ',"#140282 -  AHSS - 31.05.2024"
      '' '' '' 'IT_SAIDA_ALV' 'CULTURA'    'Cultura'     ' '  '7'  ' ',"#140282 -  AHSS - 31.05.2024"
      '' '' '' 'IT_SAIDA_ALV' 'SAFRA_APL'  'Safra'       ' '  '5'  ' ',"#140282 -  AHSS - 31.05.2024"
*#140282 -  ITSOUZA - 24.05.2024 09:08:32 - Fim
*     '' '' '' 'IT_SAIDA_ALV' 'WRKST'  'Marca'           ' '  ' '  ' ',##149354 - AHSS - 22.08.2024
      'T006A' 'MSEHI' '' 'IT_SAIDA_ALV' 'MEINS'  'Unid.'           ' '  ' '  ' ',
      '' '' '' 'IT_SAIDA_ALV' 'KWMENG' 'Qtd. Ordem'      ' '  '15' ' ',

      " 13.06.2025 - RAMON - 169497 -->
      '' '' '' 'IT_SAIDA_ALV' 'KWERT_L' 'Vlr.Liberado'   ' '  '13' '',
      '' '' '' 'IT_SAIDA_ALV' 'MENGE_L' 'Qtd.Liberada'   ' '  '14' '',
      " 13.06.2025 - RAMON - 169497 --<

      '' '' '' 'IT_SAIDA_ALV' 'KWERT'  'Valor Total'     ' '  '13' ' ',
      '' '' '' 'IT_SAIDA_ALV' 'WAERK'  'Moeda'           ' '  '05' ' ',
      '' '' '' 'IT_SAIDA_ALV' 'KWERT_USD' 'Vlr.Liber.USD'   ' ' '15' ''.

  ENDMETHOD.

  METHOD montar_lay.

    CLEAR: wa_layout, wa_variante.

    wa_layout-zebra      = abap_true.
    wa_layout-no_rowins  = abap_true.
    wa_layout-stylefname = 'ESTILO'.
    wa_layout-info_fname = 'LINE_COLOR'.
    wa_layout-sel_mode   = 'C'.
    wa_stable-row        = abap_true.

*    wa_variante-report  = sy-repid.

  ENDMETHOD.                    "MONTAR_LAY

  METHOD cria_alv.

    DATA obj_events TYPE REF TO zcl_events.

    IF wa_cont IS INITIAL.

      CREATE OBJECT wa_cont
        EXPORTING
          container_name = 'C_01'.

      CREATE OBJECT wa_alv
        EXPORTING
          i_shellstyle    = 0
          i_parent        = wa_cont
          i_appl_events   = space
          i_fcat_complete = space.

      CREATE OBJECT obj_events
        EXPORTING
          io_alv_grid = wa_alv.


      " 13.06.2025 - RAMON -->
      CALL METHOD wa_alv->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.
      " 13.06.2025 - RAMON --<

      SET HANDLER: obj_events->on_handle   FOR wa_alv,
                   obj_events->on_dt_ch    FOR wa_alv,
                   obj_events->on_toolbar  FOR wa_alv,
                   obj_events->double_click FOR wa_alv.


      READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_cat>) WITH KEY fieldname = 'KWERT_L'.

      IF sy-subrc EQ 0.

        "<fs_cat>-tabname = 'ZSDT0116'.

        "<fs_cat>-qfieldname = 'GEWEI'.
        <fs_cat>-datatype = 'CURR'.
        <fs_cat>-inttype = 'P'.
        <fs_cat>-intlen = '000015'.
        "<fs_cat>-domname = 'MENG15'.
        <fs_cat>-decimals = '000002'.
        <fs_cat>-edit = p_aur.

        IF p_rej = abap_true.
          <fs_cat>-scrtext_l = 'Valor Rejeitado'.
        ENDIF.


      ENDIF.

      DELETE it_fcat WHERE fieldname = 'MATNR'.
      DELETE it_fcat WHERE fieldname = 'ARKTX'.
      DELETE it_fcat WHERE fieldname = 'POSNR'.
      DELETE it_fcat WHERE fieldname = 'MEINS'.
      DELETE it_fcat WHERE fieldname = 'KWMENG'.
      DELETE it_fcat WHERE fieldname = 'MENGE_L'.

      " 17.06.2025 - RAMON -- 169497 -->
      DATA lt_exclude TYPE ui_functions.
      DATA ls_exclude TYPE ui_func.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND ls_exclude TO lt_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND ls_exclude TO lt_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND ls_exclude TO lt_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND ls_exclude TO lt_exclude.
      " 17.06.2025 - RAMON -- 169497 --<

      CALL METHOD wa_alv->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = lt_exclude
          is_layout            = wa_layout
          is_variant           = wa_variante
          i_save               = 'X'
        CHANGING
          it_outtab            = it_saida1
          it_fieldcatalog      = it_fcat.

    ELSE.

      "BREAK rblima.
      CALL METHOD wa_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

      READ TABLE lt_fieldcat ASSIGNING <fs_cat> WITH KEY fieldname = 'KWERT_L'.

      IF sy-subrc EQ 0.
        <fs_cat>-edit = p_aur.
      ENDIF.

      CALL METHOD wa_alv->set_frontend_fieldcatalog( lt_fieldcat ).

      CALL METHOD wa_alv->refresh_table_display.

    ENDIF.

  ENDMETHOD.


  METHOD embarque_l.

    DATA: var_answer       TYPE c,
          vl_saldo         TYPE c,
          it_saida_sel_aux TYPE TABLE OF ty_saida.

    DATA: vl_tpsim TYPE tvarvc-low.
    CONSTANTS c_name_tpsim TYPE tvarvc-name VALUE 'ZSDT0100_TPSIM'.


    FREE it_0116_aux.

    LOOP AT it_saida_l_c INTO DATA(wa_saida).

      CLEAR: wa_0116-status_workflow, wa_0116-just_workflow, vl_saldo.

      check_sld_comp_ov( EXPORTING i_posnr = wa_saida-posnr
                                   i_vbeln = wa_saida-vbeln
                         IMPORTING e_saldo = vl_saldo ).

      "Buscar Documento Simulação
      SELECT SINGLE doc_simulacao
        FROM zsdt0041 INTO @DATA(_doc_simu)
       WHERE vbeln         EQ @wa_saida-vbeln
         AND doc_simulacao NE '0000000000'.

      IF _doc_simu IS INITIAL.
        SELECT SINGLE doc_simulacao
          FROM zsdt0090 INTO _doc_simu
         WHERE vbeln         EQ wa_saida-vbeln
           AND doc_simulacao NE '0000000000'
           AND estorno       NE 'X'.
      ENDIF.

      CHECK _doc_simu IS NOT INITIAL.

      "Busca tipo do Simulador
      SELECT SINGLE tpsim
       FROM zsdt0040 INTO @DATA(_tpsim)
      WHERE doc_simulacao EQ @_doc_simu.

      "Caso a Venda seja Permuta ou Bonificação não vai para Workflow- CS2018002554
*      IF ( _tpsim NE 'PM' ) AND ( _tpsim NE 'BN' ). "Comentado Regra será verificada na Tvarv - DEVK9A23NT #145406 RSA
      IF vl_saldo NE 'X'.

        vl_tpsim = _tpsim. "Verificar TP_SIM, se encontrar, seguir para aprovação. - DEVK9A23NT #145406 RSA
        SELECT low UP TO 1 ROWS
               FROM tvarvc
               INTO @DATA(vl_low)
               WHERE name EQ @c_name_tpsim
               AND   low  EQ @vl_tpsim.
        ENDSELECT.

        IF NOT vl_low IS INITIAL."Verificar TP_SIM, se encontrar, seguir para aprovação. - DEVK9A23NT #145406 RSA

          IF wa_saida-vkbur <> '0101'.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = |Deseja enviar O.V { wa_saida-vbeln } para aprovação por Workflow?|
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            CHECK var_answer EQ '1'.

            CALL SCREEN 0105 STARTING AT 10 10 ENDING AT 137 02.

            CHECK wa_0116-just_workflow IS NOT INITIAL.

            wa_0116-status_workflow = 'L'.

          ENDIF.

        ELSE."Verificar TP_SIM, se não encontrar, exibir mensagem ao usuário. - DEVK9A23NT #145406 RSA

          "VENDA SEM RECEBIMENTO FINANCEIRO, E A CONDIÇÃO DE PAGAMENTO NÃO PERMITI O ENVIO PARA WORKFLOW.
          "EM CASO DE DÚVIDA, ENTAR EM CONTATO COM A EQUIPE DO INSUMOS CORPORTATIVO.
          CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
              titel = TEXT-023
              txt1  = TEXT-020
              txt2  = TEXT-022
              txt3  = ' '
              txt4  = TEXT-021.

          RETURN.

        ENDIF.

      ENDIF.
*      ENDIF. "Comentado Regra será verificado na Tvarv - DEVK9A23NT #145406 RSA

      it_saida_sel_aux[] = it_saida1[].

      DELETE it_saida_sel_aux WHERE vbeln NE wa_saida-vbeln.
      DELETE it_saida_l_c     WHERE vbeln EQ wa_saida-vbeln.

      LOOP AT it_saida_sel_aux INTO DATA(wa_saida_sel_aux).

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZSEQ_0116_'
          IMPORTING
            number      = wa_0116-seq.

        IF wa_0116-seq IS INITIAL.
          ROLLBACK WORK.
          MESSAGE 'Objeto numeração ZSEQ_0116_ não configurado!' TYPE 'S'.
          RETURN.
        ENDIF.

        MOVE wa_saida_sel_aux-vbeln TO wa_0116-vbeln.
        MOVE wa_saida_sel_aux-posnr TO wa_0116-posnr.
        MOVE sy-uname       TO wa_0116-user_apv.
        MOVE sy-datum       TO wa_0116-dt_apv.
        MOVE sy-uzeit       TO wa_0116-hr_apv.
*#138089 -  ITSOUZA - 24.05.2024 15:32:58 - Inicio
        MOVE sy-uname       TO wa_0116-user_solicitante.
        MOVE sy-datum       TO wa_0116-data_solicitante.
        MOVE sy-uzeit       TO wa_0116-hora_solicitante.
*#138089 -  ITSOUZA - 24.05.2024 15:32:58 - Fim


        APPEND wa_0116 TO it_0116_aux.

      ENDLOOP.

    ENDLOOP.

    IF it_0116_aux[] IS NOT INITIAL.
      INSERT zsdt0116 FROM TABLE it_0116_aux.
    ENDIF.

  ENDMETHOD.

  METHOD m_agrupa_item.

    DATA lv_menge_l TYPE kwmeng.
    DATA lv_liberado TYPE menge_d.

    CHECK cs_saida IS NOT INITIAL.

    CLEAR lv_liberado.

    LOOP AT it_0116 ASSIGNING FIELD-SYMBOL(<fs_0116>)
      WHERE vbeln = cs_saida-vbeln
        AND posnr = cs_saida-posnr.

      "ADD <fs_0116>-menge_liberado TO lv_liberado.

    ENDLOOP.

    IF lv_liberado > 0.

      IF lv_liberado < cs_saida-kwmeng.

        lv_menge_l = cs_saida-kwmeng - lv_liberado.

        cs_saida-kwert_l = NEW zcl_aprovadores( )->calcula_preco(
                                      EXPORTING iv_preco = cs_saida-kwert
                                                iv_menge_l = lv_menge_l
                                                iv_menge_t = cs_saida-kwmeng ).

        cs_saida-kwmeng = lv_menge_l.

      ELSE.

        DELETE it_saida1 INDEX iv_index.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD embarque_l_2.

    DATA: var_answer       TYPE c,
          vl_saldo         TYPE c,
          it_saida_sel_aux TYPE TABLE OF ty_saida.

    DATA: vl_tpsim TYPE tvarvc-low.

    DATA lv_erro.
    DATA lt_mess TYPE TABLE OF bapiret2.

    CONSTANTS c_name_tpsim TYPE tvarvc-name VALUE 'ZSDT0100_TPSIM'.

    FREE it_0116_aux.

    FREE it_0116_new.

    CHECK it_saida_l_c IS NOT INITIAL.

    READ TABLE it_saida_l_c TRANSPORTING NO FIELDS
      WITH KEY kwert_l = 0.

    IF sy-subrc EQ 0.
      MESSAGE `Informar o valor a liberar` TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT * FROM zi_in_simu_ov
      INTO TABLE @DATA(lt_simu)
        FOR ALL ENTRIES IN @it_saida_l_c
          WHERE vbeln = @it_saida_l_c-vbeln.

*    BREAK rblima.

    LOOP AT it_saida_l_c INTO DATA(wa_saida).

      READ TABLE lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>)
        WITH KEY vbeln = wa_saida-vbeln.

      CHECK sy-subrc EQ 0.

      IF wa_saida-kwert_l > wa_saida-kwert.
        wa_saida-kwert_l = wa_saida-kwert.
      ENDIF.

      CHECK <fs_simu>-doc_simulacao IS NOT INITIAL.

      " 10.07.2025 - ramon - retiramos para toda nova liberação passar pelo processo novo ov exemplo: 13601791 -->
****      "Busca tipo do Simulador
****      SELECT SINGLE tpsim
****       FROM zsdt0040 INTO @DATA(_tpsim)
****      WHERE doc_simulacao EQ @<fs_simu>-doc_simulacao.
****
****      vl_tpsim = _tpsim. "Verificar TP_SIM, se encontrar, seguir para aprovação. - DEVK9A23NT #145406 RSA
****
****      SELECT low UP TO 1 ROWS
****             FROM tvarvc
****             INTO @DATA(vl_low)
****             WHERE name EQ @c_name_tpsim
****             AND   low  EQ @vl_tpsim.
****      ENDSELECT.
****
****      IF vl_low IS NOT INITIAL."Verificar TP_SIM, se encontrar, seguir para aprovação. - DEVK9A23NT #145406 RSA

****        IF wa_saida-vkbur <> '0101'.
****
****          CALL FUNCTION 'POPUP_TO_CONFIRM'
****            EXPORTING
****              titlebar              = 'Confirmação'
****              text_question         = |Deseja enviar O.V { wa_saida-vbeln } para aprovação por Workflow?|
****              text_button_1         = 'Sim'
****              text_button_2         = 'Não'
****              default_button        = '1'
****              display_cancel_button = ''
****            IMPORTING
****              answer                = var_answer
****            EXCEPTIONS
****              text_not_found        = 1
****              OTHERS                = 2.
****
****          CHECK var_answer EQ '1'.
****
****          CALL SCREEN 0105 STARTING AT 10 10 ENDING AT 137 02.
****
****          CHECK wa_0116-just_workflow IS NOT INITIAL.
****
****          wa_0116-status_workflow = 'L'.
****
****          CALL FUNCTION 'NUMBER_GET_NEXT'
****            EXPORTING
****              nr_range_nr = '01'
****              object      = 'ZSEQ_0116_'
****            IMPORTING
****              number      = wa_0116-seq.
****
****          wa_0116-vbeln = wa_saida-vbeln.
****
****          wa_0116-user_apv = wa_0116-user_solicitante = sy-uname.
****          wa_0116-dt_apv = wa_0116-data_solicitante = sy-datum.
****          wa_0116-hr_apv = wa_0116-hora_solicitante = sy-uzeit.
****          wa_0116-saldo_origem = 'S'.
****
****          APPEND wa_0116 TO it_0116_aux.
****          CONTINUE.
****
****        ENDIF.
****      ENDIF.
      " 10.07.2025 - ramon - retiramos para toda nova liberação passar pelo processo novo ov exemplo: 13601791 --<

      CLEAR lv_erro.
      CLEAR it_0116_new[].

*      IF sy-uname = 'RBLIMA'.

      CALL FUNCTION 'Z_OV_LIBERACAO_EMBARQUE'
        EXPORTING
          iv_simu     = <fs_simu>-doc_simulacao
          iv_bukrs    = wa_saida-vkorg
          iv_waerk    = wa_saida-waerk
          iv_kursf    = wa_saida-kursf
          iv_tpsim    = wa_saida-tpsim
          iv_vkbur    = wa_saida-vkbur
          iv_vbeln    = wa_saida-vbeln
          iv_kwert    = wa_saida-kwert_usd
          iv_kwert_m  = wa_saida-kwert_l
        IMPORTING
          et_zsdt0116 = it_0116_new
          ev_erro     = lv_erro
          et_message  = lt_mess.

      IF lt_mess IS NOT INITIAL.
        me->popup_to_inform( lt_mess ).
      ENDIF.

      IF lv_erro = abap_true.
        CONTINUE.
      ENDIF.

      APPEND LINES OF it_0116_new TO it_0116_aux.

    ENDLOOP.

    IF it_0116_aux[] IS NOT INITIAL.

      INSERT zsdt0116 FROM TABLE it_0116_aux.

      MESSAGE s836(sd) WITH 'Liberação efetuada'.

      CALL FUNCTION 'Z_OV_JOB_RECEBIMENTO'
        EXPORTING
          iv_vbeln = wa_saida-vbeln.

    ENDIF.

  ENDMETHOD.
  METHOD popup_to_inform.

    DATA lv_txt1 TYPE sta_text.
    DATA lv_txt2 TYPE sta_text.
    DATA lv_txt3 TYPE sta_text.
    DATA lv_txt4 TYPE sta_text.
    DATA lv_var TYPE c LENGTH 30.


    LOOP AT it_ret ASSIGNING FIELD-SYMBOL(<fs_ret>).

      lv_var = 'LV_TXT' && sy-index.

      CHECK <fs_ret>-message IS NOT INITIAL.

      ASSIGN (lv_var) TO FIELD-SYMBOL(<fs_var>).

      CHECK sy-subrc EQ 0.

      <fs_var> = <fs_ret>-message.

    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = TEXT-023
        txt1  = lv_txt1
        txt2  = lv_txt2
        txt3  = lv_txt3
        txt4  = lv_txt4.

  ENDMETHOD.
  METHOD embarque_c.

    DATA: wa_0082    TYPE zsdt0082,
          var_answer TYPE c.

    LOOP AT it_saida_l_c INTO DATA(wa_saida).

*     verifica se a Ordem já foi para embarque
      SELECT SINGLE * FROM zsdt0082 INTO wa_0082 WHERE vbeln EQ wa_saida-vbeln
                                                   "AND POSNR EQ WA_SAIDA-POSNR
                                                   AND seq   NE 1
                                                   AND status IN (2,5) .
      IF sy-subrc IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-010.
      ELSE.

*     verifica se a Ordem já possui Solicitação para embarque
        SELECT SINGLE * FROM zsdt0082 INTO wa_0082 WHERE vbeln EQ wa_saida-vbeln
                                                     "AND POSNR EQ WA_SAIDA-POSNR
                                                     AND seq   EQ 1
                                                     AND status IN (1).
        IF sy-subrc IS INITIAL.
          MESSAGE s836(sd) WITH TEXT-012.
        ELSE.

          SELECT SINGLE *
            FROM zsdt0116 INTO @DATA(wl_0116)
           WHERE vbeln  EQ @wa_saida-vbeln
             AND status NE 'X'
             AND status_workflow EQ 'A'.

          IF sy-subrc = 0.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = |O.V: { wa_saida-vbeln } já aprovada! Será necessário uma nova aprovação! Confirma cancelamento?|
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            CHECK var_answer EQ '1'.

            "Deleta Registro de Aprovações para a O.V
            DELETE FROM zsdt0142 WHERE vbeln = wa_saida-vbeln.

          ENDIF.

          UPDATE zsdt0116
            SET status   = abap_true
                user_can = sy-uname
                dt_can   = sy-datum
                hora_can = sy-uzeit
          WHERE vbeln EQ wa_saida-vbeln.
          "AND POSNR EQ WA_SAIDA-POSNR.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD historico.

    FREE: it_his_saida.
    IF lines( it_saida_l_c ) EQ 1.
      READ TABLE it_saida_l_c INTO DATA(wa_saida) INDEX 1.

      SELECT *
        FROM zsdt0116
          INTO TABLE it_historico
             WHERE vbeln EQ wa_saida-vbeln.
      "AND posnr EQ wa_saida-posnr. " 10.09.2025

      IF NOT it_historico IS INITIAL.
        LOOP AT it_historico INTO wa_historico.
          MOVE-CORRESPONDING wa_historico TO wa_his_saida.

          CASE wa_historico-status.
            WHEN ''.
              CASE wa_historico-status_workflow.
                WHEN ''.
                  wa_his_saida-desc_status = 'Aprovado'.
                WHEN 'L'.
                  wa_his_saida-desc_status = 'Aguardando Aprovação'.
                WHEN 'A'.
                  wa_his_saida-desc_status = 'Aprovado Via Workflow'.
                WHEN 'R'.
                  wa_his_saida-desc_status = 'Reprovado Via Workflow'.
              ENDCASE.
            WHEN 'X'.
              wa_his_saida-desc_status = 'Cancelado'.
          ENDCASE.

          APPEND wa_his_saida TO it_his_saida.

        ENDLOOP.

        CALL SCREEN 103 ENDING AT 150 14 STARTING AT 3 3.
        "CALL SCREEN 103 ENDING AT 120 14 STARTING AT 3 3.

      ELSE.
        MESSAGE s836(sd) WITH TEXT-015.
      ENDIF.


    ELSE.
      MESSAGE s836(sd) WITH TEXT-014.
    ENDIF.

  ENDMETHOD.

  METHOD estrategia.

    DATA: v_msg    TYPE char50,
          t_ordens TYPE TABLE OF zsd_ord_vendas_est,
          t_estra  TYPE TABLE OF zsd_estrategia_ov,
          t_itens  TYPE TABLE OF zsd_itens_ov_est.

    FREE: it_estrat.
    IF lines( it_saida_l_c ) EQ 1.
      READ TABLE it_saida_l_c INTO DATA(wa_saida) INDEX 1.

      CHECK sy-subrc = 0 AND wa_saida-vbeln IS NOT INITIAL.

      CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA'
        EXPORTING
          i_usuario    = sy-uname
          i_vbeln      = wa_saida-vbeln
          i_visualizar = abap_true
        IMPORTING
          e_msg        = v_msg
        TABLES
          t_ordens     = t_ordens
          t_estra      = t_estra
          t_itens      = t_itens.

      LOOP AT t_estra INTO DATA(w_estra).
        MOVE-CORRESPONDING w_estra TO wa_estrat.

        CLEAR wa_estrat-seq. " 17.07.2025 - RAMON -
        APPEND wa_estrat TO it_estrat.
      ENDLOOP.

      SORT it_estrat.

      DELETE ADJACENT DUPLICATES FROM it_estrat COMPARING vbeln.

      CALL SCREEN 0106 STARTING AT 050 3
                       ENDING   AT 125 12.
    ELSE.
      MESSAGE s836(sd) WITH TEXT-014.
    ENDIF.

  ENDMETHOD.

  METHOD check_sld_comp_ov.

    TYPES: BEGIN OF ty_ordem_sim,
             vbeln TYPE zfit0026-vbeln,
           END OF ty_ordem_sim.

    DATA: tg_ordem_sim TYPE TABLE OF ty_ordem_sim,
          tg_0026      TYPE TABLE OF zfit0026,
          vl_saldo_sim TYPE bsad_view-dmbtr,
          vl_saldo_aut TYPE bsad_view-dmbtr,
          vl_saldo_m1  TYPE bsad_view-dmbtr,
          vl_saldo_m2  TYPE bsad_view-dmbe2.

    DATA(obj_set) = NEW zcl_taxa_curva( ).

    CLEAR: e_saldo, vl_saldo_m1, vl_saldo_m2, vl_saldo_sim, vl_saldo_aut.


    "Buscar Valor Ordem
*    SELECT SUM( NETWR )
*      FROM VBAK INTO @DATA(_VL_NETWR)
*     WHERE VBELN EQ @I_VBELN.

*   se existir item bloqueado na VBEP sai do processo.
    SELECT COUNT(*)
    FROM vbap
     INNER JOIN vbep ON vbep~vbeln EQ vbap~vbeln AND
                        vbep~posnr EQ vbap~posnr
       WHERE vbap~vbeln EQ i_vbeln
         AND vbap~posnr EQ i_posnr
         "AND vbep~lifsp EQ '12'.
         AND vbep~lifsp <> '12'. "131069 CS2023000966 - Não exibir itens bloqueio 12 - PSA

    CHECK sy-subrc EQ 0.

    SELECT SUM( netwr )
    FROM vbap
     INNER JOIN vbep ON vbep~vbeln EQ vbap~vbeln AND
                        vbep~posnr EQ vbap~posnr
     INTO @DATA(_vl_netwr)
       WHERE vbap~vbeln EQ @i_vbeln
         AND vbep~lifsp NE '12'
         AND vbep~etenr EQ '0001'.

    IF sy-subrc = 0.
      DATA(_vlr_ordem_lib) = _vl_netwr.
    ENDIF.

    "Buscar Documento Simulação
    SELECT SINGLE doc_simulacao
      FROM zsdt0041 INTO @DATA(_doc_simulacao)
     WHERE vbeln         EQ @i_vbeln
       AND doc_simulacao NE '0000000000'.

    IF _doc_simulacao IS INITIAL.
      SELECT SINGLE doc_simulacao
        FROM zsdt0090 INTO _doc_simulacao
       WHERE vbeln         EQ i_vbeln
         AND doc_simulacao NE '0000000000'
         AND estorno       NE 'X'.
    ENDIF.

    CHECK _doc_simulacao IS NOT INITIAL.

    "Buscar Ordens relacionados ao Documento de Simulação
    SELECT vbeln
      FROM zsdt0041 INTO CORRESPONDING FIELDS OF TABLE tg_ordem_sim
     WHERE doc_simulacao EQ _doc_simulacao
       AND vbeln         NE ''.

    SELECT vbeln
      FROM zsdt0090 APPENDING CORRESPONDING FIELDS OF TABLE tg_ordem_sim
     WHERE doc_simulacao EQ _doc_simulacao
       AND vbeln         NE ''
       AND estorno       NE 'X'
       AND categoria     NE 'Y'. "Desconsiderar Recusas/Devoluções.

    SORT tg_ordem_sim BY vbeln.
    DELETE ADJACENT DUPLICATES FROM tg_ordem_sim COMPARING vbeln.

    CHECK tg_ordem_sim[] IS NOT INITIAL.

    "Busca saldo já compensado para as Ordens do Simulador.
    SELECT *
      FROM zfit0026 INTO TABLE tg_0026
       FOR ALL ENTRIES IN tg_ordem_sim
     WHERE vbeln   EQ tg_ordem_sim-vbeln
       AND docnum  NE '0000000000'.

    SORT tg_0026 BY zid_lanc.
    DELETE ADJACENT DUPLICATES FROM tg_0026 COMPARING zid_lanc.

    CHECK tg_0026[] IS NOT INITIAL.

    LOOP AT tg_0026 INTO DATA(_wa_0026).

      SELECT SINGLE *
        FROM bsad_view INTO @DATA(_wl_bsad)
       WHERE bukrs = @_wa_0026-bukrs
         AND belnr = @_wa_0026-docnum.

      CHECK sy-subrc = 0. "Considerar somente documentos compensados.

      ADD _wl_bsad-dmbtr TO vl_saldo_m1.
      ADD _wl_bsad-dmbe2 TO vl_saldo_m2.
      ADD _wa_0026-mont_moeda TO vl_saldo_sim.
    ENDLOOP.

    "Buscar Saldo de Ordens já autorizados do Simulador
    LOOP AT tg_ordem_sim INTO DATA(wl_ordem_sim).
      SELECT SINGLE *
        FROM zsdt0116 INTO @DATA(_wa_0016)
       WHERE vbeln  EQ @wl_ordem_sim-vbeln
         AND status NE 'X'.

      CHECK sy-subrc = 0.

      IF ( _wa_0016-status_workflow IS INITIAL OR _wa_0016-status_workflow = 'A' ).
        CLEAR: _vl_netwr.

*        SELECT SUM( NETWR )
*          FROM VBAK INTO _VL_NETWR
*         WHERE VBELN EQ _WA_0016-VBELN.

*   se existir item bloqueado na VBEP sai do processo.
        SELECT COUNT(*)
        FROM vbap
         INNER JOIN vbep ON vbep~vbeln EQ vbap~vbeln AND
                            vbep~posnr EQ vbap~posnr
           WHERE vbap~vbeln EQ i_vbeln
             "AND vbep~lifsp EQ '12'.
             AND vbep~lifsp <> '12'. "131069 CS2023000966 - Não exibir itens bloqueio 12 - PSA

        CHECK sy-subrc IS NOT INITIAL.


*        SELECT SUM( NETWR )
*        FROM VBAP
*        INNER JOIN VBEP ON VBEP~VBELN EQ VBAP~VBELN AND
*                             VBEP~POSNR EQ VBAP~POSNR
*        INTO _VL_NETWR
*        WHERE VBAP~VBELN EQ _WA_0016-VBELN.
*              AND VBEP~LIFSP NE '12'.

        SELECT
          SUM( netwr )
        FROM
          vbap AS a
        INTO
          _vl_netwr
        WHERE
          a~vbeln EQ _wa_0016-vbeln
          AND NOT EXISTS ( SELECT * FROM vbep AS b WHERE  b~vbeln = a~vbeln AND b~posnr = a~posnr AND b~lifsp = '12'  ).

        CHECK sy-subrc = 0.

        ADD _vl_netwr TO vl_saldo_aut.

        "Verifica se tem recusa/devolução para a fatura em questão
        SELECT vbeln
          FROM vbfa INTO TABLE @DATA(tg_ordens_dev)
         WHERE vbelv   = @_wa_0016-vbeln
           AND vbtyp_n = 'H'  "Recusa/Devolução
           AND vbtyp_v = 'C'. "Ordem

        LOOP AT tg_ordens_dev INTO DATA(wl_ordens_dev) WHERE vbeln IS NOT INITIAL.
          SELECT SINGLE a~vbeln
            FROM vbfa AS a INTO @DATA(_fat_dev)
           WHERE a~vbelv   = @wl_ordens_dev-vbeln
             AND a~vbtyp_n = 'O'  "Nota de crédito
             AND a~vbtyp_v = 'H'  "Recusa/Devolução
             AND NOT EXISTS ( SELECT *
                                FROM vbfa AS b
                               WHERE b~vbelv   = a~vbeln
                                 AND b~vbtyp_n = 'S'  " Estorno nota créd.
                             ).

          IF sy-subrc = 0.
            SELECT SUM( netwr )
              FROM vbak INTO @DATA(_vl_netwr_dev)
             WHERE vbeln EQ @wl_ordens_dev-vbeln.
            IF sy-subrc = 0.
              SUBTRACT _vl_netwr_dev FROM vl_saldo_aut.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    DATA(r_tolerancia) = obj_set->get_auart( 'ZSDT0100_TOLERANCIA' ).

    TRY .
        DATA(tolerancia) = r_tolerancia[ 1 ]-low.
        SUBTRACT tolerancia FROM _vlr_ordem_lib.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    SUBTRACT vl_saldo_aut FROM vl_saldo_sim. "Subtraindo saldo já Autorizado de Ordens do Saldo Compensado

* ---> S4 Migration - 10/06/2023 - DG
    "IF _vlr_ordem_lib <= vl_saldo_sim.
    DATA: lv_ordem_lib TYPE dmbtr.

    lv_ordem_lib = CONV #( _vlr_ordem_lib ).

    IF lv_ordem_lib <= vl_saldo_sim.
* <--- S4 Migration - 10/06/2023 - DG
      e_saldo = 'X'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&      Module  PBO_0101  OUTPUT
MODULE pbo_0101 OUTPUT.
  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TI0101'.
ENDMODULE.

*&      Module  PAI_0101  INPUT
MODULE pai_0101 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXE'.
      NEW zcl_aprovadores( )->m_check_campos( ).
      NEW zcl_aprovadores( )->m_monta_alv( ).
      NEW zcl_aprovadores( )->montar_lay( ).
      NEW zcl_aprovadores( )->cria_alv( ).
    WHEN 'SAVE'.
      CLEAR: p_var, p_dvar.
      CALL SCREEN 0104 STARTING AT 10 5.
    WHEN 'VARIANT'.
      PERFORM carrega_variantes.
  ENDCASE.

ENDMODULE.

AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
*&      Module  PBO_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0103 OUTPUT.
  SET PF-STATUS 'PF01031'.
  SET TITLEBAR 'TI0103'.

  FREE: it_fcat_h, wa_fcat.

  DEFINE alv.
    wa_fcat-hotspot   = &1.
    wa_fcat-ref_table = &2.
    wa_fcat-ref_field = &3.
    wa_fcat-tabname   = &4.
    wa_fcat-fieldname = &5.
    wa_fcat-scrtext_l = &6.
    wa_fcat-scrtext_m = &6.
    wa_fcat-no_zero   = &7.
    wa_fcat-outputlen = &8.
    wa_fcat-edit      = &9.

    APPEND wa_fcat TO it_fcat_h.
    CLEAR wa_fcat.
  END-OF-DEFINITION.

  alv:
    '' '' '' 'IT_HISTORICO' 'SEQ'         'Seq'           ' ' '04' ' ',
    '' '' '' 'IT_HISTORICO' 'VBELN'       'Ordem'         ' ' ' ' ' ',
    '' '' '' 'IT_HISTORICO' 'POSNR'       'item'          ' ' '05' ' ',
    '' '' '' 'IT_HISTORICO' 'USER_APV'    'Usuário Ação' ' ' ' ' ' ',
    "09.09.2025 -->
    '' '' '' 'IT_HISTORICO' 'VLR_LIBERADO'       'Vlr.Liberado'        ' ' '18' ' ',
    '' '' '' 'IT_HISTORICO' 'VLR_LIBERADO_MOEDA' 'Vlr.Liberado Moeda'        ' ' '18' ' ',
  "09.09.2025 --<
    '' '' '' 'IT_HISTORICO' 'DT_APV'      'Dt. Ação'     ' ' '10' ' ',
    '' '' '' 'IT_HISTORICO' 'HR_APV'      'Hr. Ação'     ' ' '10' ' ',
    '' '' '' 'IT_HISTORICO' 'USER_CAN'    'Usuário Canc'  ' ' ' ' ' ',
    '' '' '' 'IT_HISTORICO' 'DT_CAN'      'Dt. Canc'      ' ' '10' ' ',
    '' '' '' 'IT_HISTORICO' 'HORA_CAN'    'Hora Canc'     ' ' '10' ' ',
    '' '' '' 'IT_HISTORICO' 'DESC_STATUS' 'Status'        ' ' '12' ' '.

  IF wa_cont_h IS INITIAL.

    CREATE OBJECT wa_cont_h
      EXPORTING
        container_name = 'C_HIS'.

    CREATE OBJECT wa_alv_h
      EXPORTING
        i_shellstyle    = 0
        i_parent        = wa_cont_h
        i_appl_events   = space
        i_fcat_complete = space.

    CALL METHOD wa_alv_h->set_table_for_first_display
      CHANGING
        it_outtab       = it_his_saida
        it_fieldcatalog = it_fcat_h.

  ELSE.
    CALL METHOD wa_alv_h->refresh_table_display.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0103 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0104 OUTPUT.
  SET PF-STATUS 'PF01041'.
  SET TITLEBAR 'TI0104'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.
  CASE ok_code.
    WHEN 'OK'.
      IF NOT p_var IS INITIAL AND NOT p_dvar IS INITIAL.
        PERFORM salvar_variant.
        MESSAGE TEXT-016 TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_variant .

  DATA: BEGIN OF rsparams_tab OCCURS 10.
          INCLUDE STRUCTURE rsparams.
  DATA: END OF rsparams_tab.
  DATA: BEGIN OF varid_tab.
          INCLUDE STRUCTURE varid.
  DATA: END OF varid_tab.
  DATA: BEGIN OF varit_tab OCCURS 2.
          INCLUDE STRUCTURE varit.
  DATA: END OF varit_tab.
  DATA: rc  TYPE syst-subrc.
* Tabellen initialisieren
  CLEAR: varid_tab.
  REFRESH varit_tab.
  REFRESH rsparams_tab.
*fill VARID structure - variant description
  varid_tab-report  = sy-cprog.
  varid_tab-variant = p_var.
  varid_tab-environmnt   = 'A'.
  varit_tab-mandt   = sy-mandt.
  varit_tab-langu   = sy-langu.
  varit_tab-report  = varid_tab-report.
  varit_tab-variant = varid_tab-variant.
  varit_tab-vtext   = p_dvar.
  APPEND varit_tab.
*fill RSPARAMS structure - Selektionswerte; variant contents
  LOOP AT v_org.
    rsparams_tab-selname = 'V_ORG'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_org-sign.
    rsparams_tab-option  = v_org-option.
    rsparams_tab-low     = v_org-low.
    rsparams_tab-high    = v_org-high.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_spa.
    rsparams_tab-selname = 'V_SPA'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_spa-sign.
    rsparams_tab-option  = v_spa-option.
    rsparams_tab-low     = v_spa-low.
    rsparams_tab-high    = v_spa-high.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_bur.
    rsparams_tab-selname = 'V_BUR'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_bur-sign.
    rsparams_tab-option  = v_bur-option.
    rsparams_tab-low     = v_bur-low.
    rsparams_tab-high    = v_bur-high.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_grp.
    rsparams_tab-selname = 'V_GRP'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_grp-sign.
    rsparams_tab-option  = v_grp-option.
    rsparams_tab-low     = v_grp-low.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_aua.
    rsparams_tab-selname = 'V_AUA'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_aua-sign.
    rsparams_tab-option  = v_aua-option.
    rsparams_tab-low     = v_aua-low.
    rsparams_tab-high    = v_aua-high.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_dat.
    rsparams_tab-selname = 'V_DAT'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_dat-sign.
    rsparams_tab-option  = v_dat-option.
    rsparams_tab-low     = v_dat-low.
    rsparams_tab-high    = v_dat-high.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_kur.
    rsparams_tab-selname = 'V_KUR'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_kur-sign.
    rsparams_tab-option  = v_kur-option.
    rsparams_tab-low     = v_kur-low.
    rsparams_tab-high    = v_kur-high.
    APPEND rsparams_tab.
  ENDLOOP.

  LOOP AT v_doc.
    rsparams_tab-selname = 'V_DOC'.
    rsparams_tab-kind    = 'S'.
    rsparams_tab-sign    = v_doc-sign.
    rsparams_tab-option  = v_doc-option.
    rsparams_tab-low     = v_doc-low.
    APPEND rsparams_tab.
  ENDLOOP.

*Check variant
  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report              = varid_tab-report
      variant             = varid_tab-variant
    IMPORTING
      r_c                 = rc
    EXCEPTIONS
      not_authorized      = 01
      no_report           = 02
      report_not_existent = 03
      report_not_supplied = 04.

  IF sy-subrc <> 0.
    MESSAGE e001(vl) WITH TEXT-e22 DISPLAY LIKE 'S'.
  ENDIF.

  IF rc = 0.                    " Variante existiert
    CALL FUNCTION 'RS_CHANGE_CREATED_VARIANT'
      EXPORTING
        curr_report               = varid_tab-report
        curr_variant              = varid_tab-variant
        vari_desc                 = varid_tab
      TABLES
        vari_contents             = rsparams_tab
        vari_text                 = varit_tab
      EXCEPTIONS
        illegal_report_or_variant = 01
        illegal_variantname       = 02
        not_authorized            = 03
        not_executed              = 04
        report_not_existent       = 05
        report_not_supplied       = 06
        variant_doesnt_exist      = 07
        variant_locked            = 08
        selections_no_match       = 09.
  ELSE.
    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = varid_tab-report
        curr_variant              = varid_tab-variant
        vari_desc                 = varid_tab
      TABLES
        vari_contents             = rsparams_tab
        vari_text                 = varit_tab
      EXCEPTIONS
        illegal_report_or_variant = 01
        illegal_variantname       = 02
        not_authorized            = 03
        not_executed              = 04
        report_not_existent       = 05
        report_not_supplied       = 06
        variant_exists            = 07
        variant_locked            = 08.
  ENDIF.
  rc = sy-subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_VARIANTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_variantes .

  PERFORM escolher_variante CHANGING g_sel_var.

  IF g_sel_var NE space.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-repid
        variant              = g_sel_var
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESCOLHER_VARIANTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_SEL_VAR  text
*----------------------------------------------------------------------*
FORM escolher_variante  CHANGING p_g_sel_var.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
      masked               = 'X'
    IMPORTING
      sel_variant          = p_g_sel_var
      sel_variant_text     = g_sel_vartxt
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

ENDFORM.

MODULE status_0105 OUTPUT.
  SET PF-STATUS 'PF0105'.
  SET TITLEBAR 'T0105'.
ENDMODULE.

MODULE user_command_0105 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      IF wa_0116-just_workflow IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-018.
        EXIT.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      CLEAR: wa_0116-just_workflow.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE status_0106 OUTPUT.
  SET PF-STATUS 'PF0106'.
  SET TITLEBAR 'T0106'.
ENDMODULE.

MODULE pbo_0106 OUTPUT.

  DATA: tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.

  wa_layout-zebra      = 'X'.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = 'X'.
  wa_layout-grid_title = ' '.

  FREE: it_fcat_est, wa_fcat.

  DEFINE alv.
    wa_fcat-hotspot   = &1.
    wa_fcat-ref_table = &2.
    wa_fcat-ref_field = &3.
    wa_fcat-tabname   = &4.
    wa_fcat-fieldname = &5.
    wa_fcat-scrtext_l = &6.
    wa_fcat-scrtext_m = &6.
    wa_fcat-no_zero   = &7.
    wa_fcat-outputlen = &8.
    wa_fcat-edit      = &9.

    APPEND wa_fcat TO it_fcat_est.
    CLEAR wa_fcat.
  END-OF-DEFINITION.

  alv:
    '' 'ZGLT037'    'VALOR_DE'  'IT_ESTRAT' 'VALOR_DE'     'Valor de (USD)'  ' ' '15' ' ',
    '' 'ZGLT037'    'VALOR_ATE' 'IT_ESTRAT' 'VALOR_ATE'    'Valor Até (USD)' ' ' '15' ' ',
    '' 'ZGLT037'    'APROVADOR' 'IT_ESTRAT' 'APROVADOR'    'Aprovador'       ' ' '20' ' ',
    '' ' '          'ESTADO'    'IT_ESTRAT' 'ESTADO'       'Estado'          ' ' '10' ' ',
    '' ' '          'OPCOES'    'IT_ESTRAT' 'OPCOES'       'Opções Liber.'   ' ' '12' ' '.

  IF wa_cont_est IS INITIAL.
    CREATE OBJECT wa_cont_est
      EXPORTING
        container_name = 'CC_ESTRA'.

    CREATE OBJECT wa_alv_est
      EXPORTING
        i_parent = wa_cont_est.


    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Estratégia de Liberação'.
    wa_layout-no_toolbar = 'X'.

    CALL METHOD wa_alv_est->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = it_fcat_est[]
        it_outtab            = it_estrat[].

  ELSE.
    CALL METHOD wa_alv_est->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.

MODULE pai_0106 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
