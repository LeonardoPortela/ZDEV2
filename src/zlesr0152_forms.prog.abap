*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* set cockpit
**********************************************************************
FORM f_set_cockpit.
  FREE: l_cockpit.

* US - 92464 - Inicio - CBRAND
  IF p_nfprop = 'X'.
    l_cockpit = '03'.
  ELSE.
* US - 92464 - Fim - CBRAND
    CASE abap_true.
      WHEN p_opcao1.
        l_cockpit = '01'.
    ENDCASE.
  ENDIF.

ENDFORM.

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  DATA: l_candat TYPE j_1bnfdoc-candat.

  CREATE OBJECT zcl_util.

  FREE: l_candat, r_cfop, r_matkl, t_set, t_zlest0210.

*---------------------------------
* ler set
*---------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_CFOP_VENDA_IND'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_set INTO w_set.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_cfop.
  ENDLOOP.

*---------------------------------
* ler TVARVset
*---------------------------------
  SELECT *
    FROM tvarvc
    INTO TABLE t_tvarvc
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_tvarvc INTO w_tvarvc.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_matkl.
  ENDLOOP.

  SELECT *
    FROM vbfa
    INTO TABLE t_vbfa
   WHERE vbelv IN s_vbeln
     AND erdat IN s_datanf
     AND vbtyp_n = 'M'
     AND vbtyp_v = 'C'.

  CHECK t_vbfa[] IS NOT INITIAL.

  SELECT *
    FROM vbak
    INTO TABLE t_vbak
     FOR ALL ENTRIES IN t_vbfa
   WHERE vbeln  = t_vbfa-vbelv
     AND erdat IN s_dataov
     AND kunnr IN s_kunnr
     AND EXISTS ( SELECT * FROM vbap WHERE vbap~vbeln = vbak~vbeln
                                     AND   vbap~werks IN s_werks )
     AND EXISTS ( SELECT * FROM vbkd WHERE vbkd~vbeln = vbak~vbeln        "*-CS2024000522-18.07.2024-JT-#143588
                                     AND   vbkd~inco1 IN ('CIF','CPT') ). "*-CS2024000522-18.07.2024-JT-#143588

  SORT t_vbak BY vbeln.

*-CS2024000522-12.09.2024-JT-#151251-inicio
  SELECT *
    INTO TABLE t_zsdt0011
    FROM zsdt0011
     FOR ALL ENTRIES IN t_vbak
   WHERE auart        = t_vbak-auart
     AND tp_movimento = 'S'.
*-CS2024000522-12.09.2024-JT-#151251-fim

  LOOP AT t_vbfa INTO w_vbfa.
    l_tabix = sy-tabix.

    READ TABLE t_vbak INTO w_vbak WITH KEY vbeln = w_vbfa-vbelv
                                  BINARY SEARCH.
    IF sy-subrc <> 0.
      DELETE t_vbfa INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  CHECK t_vbfa[] IS NOT INITIAL.

  LOOP AT t_vbfa  INTO w_vbfa.
    w_vbfa-refkey    = w_vbfa-vbeln.
    MODIFY t_vbfa FROM w_vbfa INDEX sy-tabix.
  ENDLOOP.

  SELECT j_1bnflin~docnum
         j_1bnfdoc~nfenum
         j_1bnfdoc~parid
         j_1bnfdoc~stains
         j_1bnflin~refkey
         j_1bnfdoc~brgew    "*-CS2024000522-18.07.2024-JT
         j_1bnflin~cfop     "*-CS2024000522-18.07.2024-JT
         j_1bnfdoc~docdat   "*-CS2024000522-10.09.2024-JT-#151259
    FROM j_1bnflin
   INNER JOIN j_1bnfdoc      ON j_1bnfdoc~docnum       = j_1bnflin~docnum
                            AND j_1bnfdoc~cancel       = abap_off
                            AND j_1bnfdoc~candat       = l_candat
   INNER JOIN j_1bnfe_active ON j_1bnfe_active~docnum  = j_1bnflin~docnum
                            AND j_1bnfe_active~docsta  = '1'
                            AND j_1bnfe_active~scssta <> '2'
    INTO TABLE t_jlin
     FOR ALL ENTRIES IN t_vbfa
   WHERE j_1bnflin~refkey  = t_vbfa-refkey
     AND j_1bnflin~cfop   IN r_cfop
     AND j_1bnflin~matkl  IN r_matkl
     AND j_1bnfdoc~docnum IN s_docnum. "*-CS2024000522-10.09.2024-JT-#151259

  CHECK t_jlin[] IS NOT INITIAL.

  SORT t_jlin BY nfenum.
  SORT t_vbfa BY refkey.
  SORT t_vbak BY vbeln.

  LOOP AT t_jlin  INTO w_jlin.
    l_tabix = sy-tabix.
    w_jlin-chave_nfe = zcl_util->get_chave_nfe( w_jlin-docnum ).
    MODIFY t_jlin FROM w_jlin INDEX l_tabix.
  ENDLOOP.

  SELECT *
    FROM zlest0210
    INTO TABLE t_zlest0210
     FOR ALL ENTRIES IN t_jlin
   WHERE chave_nf_venda = t_jlin-chave_nfe.

  SORT t_zlest0210 BY chave_nf_venda.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  FREE: l_form_saida.

  CASE l_cockpit.
    WHEN '01'.
      l_form_saida = 'F_PROCESSA_SAIDA_' && l_cockpit.
    WHEN '03'.
      l_form_saida = 'F_PROCESSA_SAIDA_' && l_cockpit.
  ENDCASE.

  PERFORM (l_form_saida) IN PROGRAM zlesr0152 IF FOUND.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_saida_01.

  FREE: t_alv.

  LOOP AT t_jlin INTO w_jlin.

    FREE: t_style, w_vbfa, w_vbak, w_zlest0210, w_alv.

    READ TABLE t_vbfa      INTO w_vbfa WITH KEY refkey = w_jlin-refkey
                                       BINARY SEARCH.
    READ TABLE t_vbak      INTO w_vbak WITH KEY vbeln  = w_vbfa-vbelv
                                       BINARY SEARCH.
    READ TABLE t_zlest0210 INTO w_zlest0210 WITH KEY chave_nf_venda = w_jlin-chave_nfe
                                            BINARY SEARCH.

*   w_campos_nfe = zcl_util->get_atributos_nfe( w_zlest0210-chave_nf_cta_ordem ).

    w_alv-kunnr           = w_vbak-kunnr.
    w_alv-vbeln_venda     = w_vbak-vbeln.
    w_alv-nf_venda        = w_jlin-nfenum.
    w_alv-docdat          = w_jlin-docdat. "*-CS2024000522-10.09.2024-JT-#151259
    w_alv-cfop            = w_jlin-cfop.   "*-CS2024000522-18.07.2024-JT-#143588
    w_alv-quantidade_nf   = w_jlin-brgew.  "*-CS2024000522-18.07.2024-JT-#143588

    SELECT *
      INTO TABLE @DATA(tlin)
      FROM j_1bnflin
      WHERE docnum =  @w_jlin-docnum
      ORDER BY netwrt DESCENDING.

    LOOP AT tlin INTO DATA(wlin).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wlin-matnr
        IMPORTING
          output = wlin-matnr.
      CONCATENATE wlin-matnr '-' wlin-maktx INTO  w_alv-material.
      EXIT.
    ENDLOOP.
    REFRESH tlin.


    w_alv-refkey          = w_jlin-refkey.
    w_alv-chave_nfe_venda = w_jlin-chave_nfe.
    w_alv-docnum          = w_jlin-docnum.

*   IF w_campos_nfe-nfnum9 IS INITIAL.
*     w_alv-nf_remessa    = icon_linked_document.
*   ELSE.
*     w_alv-nf_remessa    = w_campos_nfe-nfnum9.
*   ENDIF.

    w_alv-nf_remessa      = zcl_remessa_terceiro=>zif_remessa_terceiro~get_status_nf_remessa(    i_chave_nfe      = w_jlin-chave_nfe ).
    w_alv-ov_dummy = zcl_remessa_terceiro=>zif_remessa_terceiro~get_status_ov_dummy( i_vbeln          = w_vbak-vbeln
                                                                                     i_chave_nf_venda = w_jlin-chave_nfe ).
    w_alv-remessa_dummy = zcl_remessa_terceiro=>zif_remessa_terceiro~get_status_remessa_dummy( i_vbeln_venda    = w_vbak-vbeln
                                                                                               i_nf_venda       = w_alv-nf_venda
                                                                                               i_chave_nf_venda = w_jlin-chave_nfe
                                                                                               i_vbeln_dummy    = w_alv-ov_dummy ).
    w_alv-transp = zcl_remessa_terceiro=>zif_remessa_terceiro~get_status_vt( EXPORTING i_vbeln_venda   = w_vbak-vbeln
                                                                                       i_remessa_dummy = w_alv-remessa_dummy
                                                                                       i_nf_venda      = w_alv-nf_venda
                                                                             IMPORTING e_shtyp         = w_alv-shtyp ).  "*-CS2024000522-10.09.2024-JT-#151259

    zcl_remessa_terceiro=>zif_remessa_terceiro~get_dados_transporte(   EXPORTING i_ov_dummy      = w_alv-ov_dummy
                                                                                 i_remessa_dummy = w_alv-remessa_dummy
                                                                       IMPORTING e_placa         = w_alv-placa
                                                                                 e_quantidade    = w_alv-quantidade
                                                                                 e_tp_frete      = w_alv-tp_frete
                                                                                 e_itinerario    = w_alv-itinerario
                                                                                 e_vlr_frete     = w_alv-vlr_frete
                                                                                 e_unid_cond     = w_alv-unid_cond
                                                                                 e_dados_transp  = w_alv-dados_transp
                                                                                 e_lock_ag_frete = l_lock_ag_frete
                                                                       CHANGING  c_ag_frete      = w_alv-ag_frete ).

    zcl_remessa_terceiro=>zif_remessa_terceiro~get_status_outros_docs( EXPORTING i_ov_dummy      = w_alv-ov_dummy
                                                                                 i_remessa_dummy = w_alv-remessa_dummy
                                                                       IMPORTING e_doc_custo     = w_alv-doc_custo
                                                                                 e_ordem_serv    = w_alv-ov_serv
                                                                                 e_fatura_serv   = w_alv-fat_serv
                                                                                 e_dacte         = w_alv-dacte ).


    CLEAR: w_zsdt0011.                                                     "*-CS2024000522-12.09.2024-JT-#151251-inicio
    READ TABLE  t_zsdt0011 INTO w_zsdt0011 WITH KEY auart = w_vbak-auart.  "*-CS2024000522-12.09.2024-JT-#151251-inicio
    w_alv-shtyp               = w_zsdt0011-shtyp.                          "*-CS2024000522-12.09.2024-JT-#151251-inicio

*-CS2024000522-18.07.2024-JT-#143588-inicio
    zcl_remessa_terceiro=>zif_remessa_terceiro~get_agente_frete(       EXPORTING i_chave_nfe     = w_zlest0210-chave_nf_cta_ordem
                                                                                 i_vbeln         = w_alv-vbeln_venda
                                                                        CHANGING e_agente_frete  = w_alv-ag_frete ).
    PERFORM f_atualiza_valor_frete    USING w_alv-vbeln_venda
                                            w_alv-nf_venda
                                            w_alv-remessa_dummy
                                            w_alv-ag_frete
                                            w_alv-transp
                                            w_alv-doc_custo
                                   CHANGING w_alv-vlr_frete.
*-CS2024000522-18.07.2024-JT-#143588-fim

    IF l_lock_ag_frete = abap_true.
      w_style-fieldname  = 'AG_FRETE'.
      w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
      APPEND w_style    TO t_style.
      w_alv-cellstyles[] = t_style[].
    ELSE.
      w_style-fieldname  = 'AG_FRETE'.
      w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
      APPEND w_style    TO t_style.
      w_alv-cellstyles[] = t_style[].
    ENDIF.

    APPEND w_alv         TO t_alv.
  ENDLOOP.

ENDFORM.

**********************************************************************
* recalcular valor do Frete
**********************************************************************
FORM f_atualiza_valor_frete    USING p_vbeln_venda
                                     p_nf_venda
                                     p_remessa_dummy
                                     p_ag_frete
                                     p_transp
                                     p_doc_custo
                            CHANGING p_vlr_frete.

  DATA: l_unid_cond TYPE konwa,
        l_vlr_frete TYPE kbetr_kond.

  CHECK p_remessa_dummy IS NOT INITIAL AND p_remessa_dummy(1) <> '@'.

  SELECT SINGLE vbeln, posnr
    INTO @DATA(_zlest0211)
    FROM zlest0211
   WHERE vbeln = @p_remessa_dummy.

  CHECK sy-subrc = 0.

  CHECK p_transp    IS INITIAL OR p_transp(1)    = '@'.
  CHECK p_doc_custo IS INITIAL OR p_doc_custo(1) = '@'.

  TRY.
      l_vlr_frete = zcl_remessa_terceiro=>zif_remessa_terceiro~set_calcula_frete(
                       EXPORTING i_vbeln_venda    = p_vbeln_venda
                                 i_remessa_dummy  = p_remessa_dummy
                                 i_nf_venda       = p_nf_venda
                                 i_agente_frete   = p_ag_frete
                       IMPORTING e_unid_cond      = l_unid_cond ).
    CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
      EXIT.
  ENDTRY.

  CHECK p_vlr_frete <> l_vlr_frete AND l_vlr_frete <> 0.

  p_vlr_frete = l_vlr_frete.

  UPDATE zlest0211 SET vlr_frete = l_vlr_frete
                       kbetr     = l_vlr_frete
                       unid_cond = l_unid_cond
                 WHERE vbeln     = _zlest0211-vbeln
                   AND posnr     = _zlest0211-posnr.

  COMMIT WORK AND WAIT.

ENDFORM.

**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

*-CS2024000522-11.09.2024-JT-#151751-inicio
  IF p_popup = abap_false.
    CALL SCREEN 100.
  ELSE.
    CALL SCREEN 100 STARTING AT 10   2
                      ENDING AT 180  21.
  ENDIF.
*-CS2024000522-11.09.2024-JT-#151751-fim

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  FREE: lt_sort.

  PERFORM f_funcoes.
  PERFORM f_fieldcatalog.

*-CS2024000522-26.08.2024-JT-#147087-inicio
  CLEAR wa_sort.
  wa_sort-fieldname = 'REM_VBELN'.
  wa_sort-spos      = 1.
  wa_sort-down      = 'X'.
  APPEND wa_sort   TO lt_sort.
*-CS2024000522-26.08.2024-JT-#147087-fim

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

*  IF g_custom_container IS INITIAL.
*    CREATE OBJECT g_custom_container
*      EXPORTING
*        container_name              = 'CONTAINER'
*      EXCEPTIONS
*        cntl_error                  = 1
*        cntl_system_error           = 2
*        create_error                = 3
*        lifetime_error              = 4
*        lifetime_dynpro_dynpro_link = 5
*        OTHERS                      = 6.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.
*    ENDIF.

    SET HANDLER lcl_event_handler=>on_hotspot_click     FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed      FOR g_grid.
    SET HANDLER lcl_event_handler=>handle_toolbar       FOR g_grid. "US - 92467 - CBRAND
    SET HANDLER lcl_event_handler=>handle_user_command  FOR g_grid. "US - 92467 - CBRAND

    CASE l_cockpit.
      WHEN '01'.

        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
            it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN '03'.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
            it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_alv_transf[]
            it_fieldcatalog               = t_fieldcat
            it_sort                       = lt_sort  "*-CS2024000522-26.08.2024-JT-#147087-inicio
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
    ENDCASE.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*   CALL METHOD g_grid->set_ready_for_input
*     EXPORTING
*       i_ready_for_input = 1.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.


  IF l_cockpit = '03'.

    wl_linha = 'Frete Sobre NF de Transferência'.

    wl_text = wl_linha.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>extra_large
        sap_color    = cl_dd_area=>list_heading_int.


    IF s_reswk[] IS NOT INITIAL.
      CONCATENATE  'Filial Saída:' s_reswk-low
              INTO wl_linha SEPARATED BY space.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.

    IF s_aedat[] IS NOT INITIAL.
      READ TABLE s_aedat INDEX 1.

      wl_data1 = s_aedat-low+6(2) && '.' && s_aedat-low+4(2) && '.' && s_aedat-low(4).
      wl_data2 = s_aedat-high+6(2) && '.' && s_aedat-high+4(2) && '.' && s_aedat-high(4).

      IF s_dataov-high IS NOT INITIAL.
        CONCATENATE  'Data do Pedido.:' wl_data1 'a' wl_data2
               INTO wl_linha SEPARATED BY space.
      ELSE.
        CONCATENATE  'Data do Pedido.:' wl_data1
               INTO wl_linha SEPARATED BY space.
      ENDIF.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.

    IF s_erdat[] IS NOT INITIAL.
      READ TABLE s_erdat INDEX 1.

      wl_data1 = s_erdat-low+6(2) && '.' && s_erdat-low+4(2) && '.' && s_erdat-low(4).
      wl_data2 = s_erdat-high+6(2) && '.' && s_erdat-high+4(2) && '.' && s_erdat-high(4).

      IF s_datanf-high IS NOT INITIAL.
        CONCATENATE  'Data da Remessa...:' wl_data1 'a' wl_data2
               INTO wl_linha SEPARATED BY space.
      ELSE.
        CONCATENATE  'Data da Remessa...:' wl_data1
               INTO wl_linha SEPARATED BY space.
      ENDIF.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.

*-CS2024000522-12.09.2024-JT-#151251-inicio
    IF s_ebeln[] IS NOT INITIAL.
      SELECT ebeln
        INTO TABLE @DATA(t_ped)
        FROM ekko
       WHERE ebeln IN @s_ebeln.

      wl_linha = 'Pedido(s)...............: '.

      LOOP AT t_ped INTO DATA(w_ped).
        IF sy-tabix = 1.
          CONCATENATE wl_linha     w_ped-ebeln INTO wl_linha SEPARATED BY space.
        ELSE.
          CONCATENATE wl_linha '/' w_ped-ebeln INTO wl_linha SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.
      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.
*-CS2024000522-12.09.2024-JT-#151251-inicio

  ELSE.
    wl_linha = 'Emissão de Frete sobre Nota de Terceiros'.

    wl_text = wl_linha.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>extra_large
        sap_color    = cl_dd_area=>list_heading_int.

    IF s_vbeln[] IS NOT INITIAL.
      CONCATENATE  'Ordem de Venda:' s_vbeln-low
              INTO wl_linha SEPARATED BY space.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.

* CALL METHOD obj_dyndoc_id->new_line.

    IF s_kunnr[] IS NOT INITIAL.
      CONCATENATE  'Cliente...............:' s_kunnr-low
             INTO wl_linha SEPARATED BY space.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.

    IF s_dataov[] IS NOT INITIAL.
      READ TABLE s_dataov INDEX 1.

      wl_data1 = s_dataov-low+6(2) && '.' && s_dataov-low+4(2) && '.' && s_dataov-low(4).
      wl_data2 = s_dataov-high+6(2) && '.' && s_dataov-high+4(2) && '.' && s_dataov-high(4).

      IF s_dataov-high IS NOT INITIAL.
        CONCATENATE  'Data Criação OV.:' wl_data1 'a' wl_data2
               INTO wl_linha SEPARATED BY space.
      ELSE.
        CONCATENATE  'Data Criação OV.:' wl_data1
               INTO wl_linha SEPARATED BY space.
      ENDIF.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.

    IF s_datanf[] IS NOT INITIAL.
      READ TABLE s_datanf INDEX 1.

      wl_data1 = s_datanf-low+6(2) && '.' && s_datanf-low+4(2) && '.' && s_datanf-low(4).
      wl_data2 = s_datanf-high+6(2) && '.' && s_datanf-high+4(2) && '.' && s_datanf-high(4).

      IF s_datanf-high IS NOT INITIAL.
        CONCATENATE  'Data NF Venda...:' wl_data1 'a' wl_data2
               INTO wl_linha SEPARATED BY space.
      ELSE.
        CONCATENATE  'Data NF Venda...:' wl_data1
               INTO wl_linha SEPARATED BY space.
      ENDIF.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.
    ENDIF.
  ENDIF.
ENDFORM.                    " ZF_ALV_HEADER

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: t_function.

  w_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND w_function TO t_function.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  CASE l_cockpit.
    WHEN '01'.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV' 'KUNNR'         'Cliente'                 '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        02  ''      ''       'T_ALV' 'VBELN_VENDA'   'OV Venda'                '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''      ''       'T_ALV' 'NF_VENDA'      'NF Venda'                '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''      ''       'T_ALV' 'DOCDAT'        'Dt.NF Venda'             '14'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X', "*-CS2024000522-10.09.2024-JT-#151259
        05  ''      ''       'T_ALV' 'CFOP'          'CFOP'                    '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-18.07.2024-JT-#143588
        06  ''      ''       'T_ALV' 'QUANTIDADE_NF' 'Qtdade.NF'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-18.07.2024-JT-#143588
        07  ''      ''       'T_ALV' 'MATERIAL'      'Produto'                 '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_ALV' 'NF_REMESSA'    'NF Rem.Conta Ordem'      '23'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        09  ''      ''       'T_ALV' 'OV_DUMMY'      'OV Dummy'                '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        10  ''      ''       'T_ALV' 'REMESSA_DUMMY' 'Remessa Dummy'           '15'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        11  ''      ''       'T_ALV' 'TP_FRETE'      'Tipo Frete'              '11'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-10.09.2024-JT-#151259
        12  'LFA1'  'LIFNR'  'T_ALV' 'AG_FRETE'      'Ag.Frete'                '10'  'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ' ' ',
        13  ''      ''       'T_ALV' 'DADOS_TRANSP'  'Transp.'                 '07'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        14  ''      ''       'T_ALV' 'PLACA'         'Placa'                   '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        15  ''      ''       'T_ALV' 'QUANTIDADE'    'Qtdade.Rem.Dummy'        '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-18.07.2024-JT-#143588
*       13  ''      ''       'T_ALV' 'TP_FRETE'      'Tipo.Frete'              '11'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-10.09.2024-JT-#151259
        16  ''      ''       'T_ALV' 'ITINERARIO'    'Itinerário'              '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        17  ''      ''       'T_ALV' 'SHTYP'         'Tipo Transp'             '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-10.09.2024-JT-#151259
        18  ''      ''       'T_ALV' 'VLR_FRETE'     'Vlr.Frete'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        19  ''      ''       'T_ALV' 'UNID_COND'     'Unid.Cond.'              '11'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        20  ''      ''       'T_ALV' 'TRANSP'        'Doc.Transp.'             '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        21  ''      ''       'T_ALV' 'DOC_CUSTO'     'Doc.Custo'               '10'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        22  ''      ''       'T_ALV' 'OV_SERV'       'OV Serviço'              '10'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        23  ''      ''       'T_ALV' 'FAT_SERV'      'Fatura Serv.'            '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        24  ''      ''       'T_ALV' 'DACTE'         'Dacte'                   '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' '.

    WHEN '03'.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ALV_TRANSF' 'REM_KUNNR'    'Destino'             '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
       "02  ''      ''       'T_ALV_TRANSF' 'REM_VGBEL'    'Nr. Pedido .'        '12'  'X'    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        02  ''      ''       'T_ALV_TRANSF' 'EBELN_IC'     'Nr. Pedido .'        '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        03  ''      ''       'T_ALV_TRANSF' 'REM_VGPOS'    'It.Ped.'             '06'  'X'    ' ' ' ' ' '  ' ' ' ' ' ' 'X' ' ' 'X',
        04  ''      ''       'T_ALV_TRANSF' 'REM_VBELN'    'Remessa'             '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''      ''       'T_ALV_TRANSF' 'REM_LFDAT'    'Dt.Remessa'          '13'  ' '    ' ' 'C' ' '  ' ' ' ' ' ' ' ' ' ' 'X', "*-CS2024000522-12.09.2024-JT-#151251-inicio
        06  ''      ''       'T_ALV_TRANSF' 'REM_FAT'      'Fatura'              '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        07  ''      ''       'T_ALV_TRANSF' 'REM_DOC_NF'   'Danfe'               '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_ALV_TRANSF' 'REM_NR_NF'    'NF. Transf'          '15'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''      ''       'T_ALV_TRANSF' 'REM_BRGEW'    'Quantidade'          '12'  ' '    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ' ' ',
*       09  ''      ''       'T_ALV_TRANSF' 'REM_INCO1'    'Tipo Frete'          '07'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''      ''       'T_ALV_TRANSF' 'TP_FRETE'     'Tipo Frete'          '10'  ' '    ' ' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-26.08.2024-JT-#147087-inicio
        11  'LFA1'  'LIFNR'  'T_ALV_TRANSF' 'AG_FRETE'     'Ag. Frete'           '09'  'X'    ' ' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        12  ''      ''       'T_ALV_TRANSF' 'DADOS_TRANSP' 'Transp.'             '08'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        13  ''      ''       'T_ALV_TRANSF' 'PLACA'        'Placa'               '10'  ' '    ' ' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        14  ''      ''       'T_ALV_TRANSF' 'REM_ROUTE'    'Itinerário'          '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
*       14  ''      ''       'T_ALV_TRANSF' 'TP_FRETE'     'Tipo Frete'          '10'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-26.08.2024-JT-#147087-inicio
        15  ''      ''       'T_ALV_TRANSF' 'FRETE_SHTYP'  'Tipo Transp'         '10'  ' '    ' ' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-26.08.2024-JT-#147087-inicio
        16  ''      ''       'T_ALV_TRANSF' 'VLR_FRETE'    'Vlr. Frete'          '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        17  ''      ''       'T_ALV_TRANSF' 'UNID_COND'    'Unid. Cond'          '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        18  ''      ''       'T_ALV_TRANSF' ' '            'Tp. Transp.'         '12'  ' '    ' ' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        19  ''      ''       'T_ALV_TRANSF' 'TRANSP'       'Doc. Transp.'        '10'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        20  ''      ''       'T_ALV_TRANSF' 'DOC_CUSTO'    'Doc. Custo'          '10'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        21  ''      ''       'T_ALV_TRANSF' 'OV_SERV'      'Ov. Serviço'         '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        22  ''      ''       'T_ALV_TRANSF' 'FAT_SERV'     'Fatura Serv.'        '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        23  ''      ''       'T_ALV_TRANSF' 'DACTE'        'Dacte'               '12'  ' '    ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' '.

  ENDCASE.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "16

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
* GERAR OV DUMMY
**********************************************************************
FORM f_gera_ov_dummy    USING p_ov_venda
                              p_chave_nfe
                              p_nf_venda
                     CHANGING p_ov_dummy.

  DATA: l_status TYPE char1,
        l_erro   TYPE char1.

  FREE: l_erro,
        p_ov_dummy.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Criando OV Dummy...'.

  TRY .
      zcl_remessa_terceiro=>zif_remessa_terceiro~get_instance(
        )->set_ordem_venda(   EXPORTING i_vbeln_princ    = p_ov_venda
                                        i_vbeln          = p_ov_venda
                                        i_nf_venda       = p_nf_venda
                                        i_etapa_proc     = '01'
        )->set_cria_ov_dummy( EXPORTING i_nf_venda       = p_nf_venda
                                        i_chave_nf_venda = p_chave_nfe
                              IMPORTING e_ov_dummy       = p_ov_dummy
        ).
    CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    p_ov_dummy = icon_message_error.
  ELSE.
    IF p_ov_dummy IS INITIAL.
      p_ov_dummy = icon_message_error.
    ELSE.
      p_ov_dummy = p_ov_dummy.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* GERAR REMESSA DUMMY
**********************************************************************
FORM f_gera_remessa_dummy    USING p_ov_venda
                                   p_chave_nfe
                                   p_ov_dummy
                                   p_nf_venda
                                   p_docnum
                          CHANGING p_remessa_dummy.

  DATA: l_status TYPE char1,
        l_erro   TYPE char1.

  FREE: l_erro,
        p_remessa_dummy.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Criando Remessa Dummy...'.

  TRY .
      zcl_remessa_terceiro=>zif_remessa_terceiro~get_instance(
        )->set_ordem_venda(        EXPORTING i_vbeln_princ    = p_ov_venda
                                             i_vbeln          = p_ov_dummy
                                             i_nf_venda       = p_nf_venda
                                             i_etapa_proc     = '02'
        )->set_cria_remessa_dummy( EXPORTING i_vbeln          = p_ov_venda
                                             i_chave_nf_venda = p_chave_nfe
                                             i_nf_venda       = p_nf_venda
                                             i_docnum         = p_docnum
                                   IMPORTING e_remessa_dummy  = p_remessa_dummy
        ).
    CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    p_remessa_dummy = icon_message_error.
  ELSE.
    IF p_remessa_dummy IS INITIAL.
      p_remessa_dummy = icon_message_error.
    ELSE.
      p_remessa_dummy = p_remessa_dummy.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* GERAR DOC TRANSPORTE
**********************************************************************
FORM f_gera_transporte       USING p_ov_venda
                                   p_ov_dummy
                                   p_remessa_dummy
                                   p_nf_venda
                                   p_vlr_frete        "*-CS2024000522-18.07.2024-JT-#143588
                                   p_chave_nfe_venda  "*-CS2024000522-18.07.2024-JT-#143588
                          CHANGING p_transp.

  DATA: l_status         TYPE char1,
        l_erro           TYPE char1,
        l_agente_frete   TYPE lfa1-lifnr,
        l_vlr_unit_frete TYPE zde_vlr15_02.


  FREE: l_erro,
        p_transp.

*-CS2024000522-18.07.2024-JT-#143588-inicio
*---------------------------
*- validacoes---------------
*---------------------------
  SELECT SINGLE inco1
           INTO @DATA(_inco1)
           FROM vbkd
          WHERE vbeln = @p_ov_venda.

  IF sy-subrc = 0 AND ( _inco1 = 'CIF' OR _inco1 = 'CPT' ).
    IF _inco1 = 'CIF'.
      IF p_vlr_frete IS INITIAL.
        p_transp = icon_message_error.
        MESSAGE s024(sd) WITH 'Valor do Frete deve ser maior que Zero!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF _inco1 = 'CPT'.
      READ TABLE t_zlest0210 INTO w_zlest0210 WITH KEY chave_nf_venda = p_chave_nfe_venda. "*-CS2024000522-18.07.2024-JT-#143588
      zcl_remessa_terceiro=>zif_remessa_terceiro~get_agente_frete( EXPORTING i_chave_nfe      = w_zlest0210-chave_nf_cta_ordem
                                                                             i_vbeln          = p_ov_venda
                                                                   IMPORTING e_vlr_unit_frete = l_vlr_unit_frete
                                                                    CHANGING e_agente_frete   = l_agente_frete ).
      IF p_vlr_frete <> l_vlr_unit_frete.
        p_transp = icon_message_error.
        MESSAGE s024(sd) WITH 'Tarifas Divergentes: TK = ' && p_vlr_frete && ' - ' 'XML = ' && l_vlr_unit_frete DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
*-CS2024000522-18.07.2024-JT-#143588-fim

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Gerando Documento Transporte...'.

  TRY .
      zcl_remessa_terceiro=>zif_remessa_terceiro~get_instance(
        )->set_cria_vt( EXPORTING i_vbeln_venda   = p_ov_venda
                                  i_ov_dummy      = p_ov_dummy
                                  i_remessa_dummy = p_remessa_dummy
                                  i_nf_venda      = p_nf_venda
                        IMPORTING e_doc_transp    = p_transp
        ).
    CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    p_transp = icon_message_error.
  ELSE.
    IF p_transp IS INITIAL.
      p_transp = icon_message_error.
    ELSE.
      p_transp = p_transp.
    ENDIF.
  ENDIF.

ENDFORM.
**********************************************************************
* GERAR DOC TRANSPORTE TRANFERENCIA
**********************************************************************
FORM f_gera_transporte_transf  USING w_alv_transf TYPE  ty_alv_transf
                          CHANGING p_transp.

  DATA: l_status TYPE char1,
        l_erro   TYPE char1,
        l_lifnr  TYPE lifnr,
        l_kunnr  TYPE kunnr,
        l_tknum  TYPE vttk-tknum.

  FREE: l_erro,
        p_transp.


  DATA: st_headerdata TYPE bapishipmentheader,
        st_stagedata  TYPE bapishipmentstage,
        st_itemdata   TYPE bapishipmentitem.

  DATA: t_return_vt TYPE bapiret2.

  IF w_alv_transf-vlr_frete <= 0  . "NFe Não Autorizada.
    MESSAGE s024(sd) WITH TEXT-149 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    IF w_alv_transf-rem_nr_nf IS INITIAL.
      MESSAGE s024(sd) WITH TEXT-150 DISPLAY LIKE 'E'.
      EXIT.
    ELSE.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 90
          text       = 'Gerando Documento Transporte...'.

      SELECT *
        INTO @DATA(w_zlest0211)
        FROM zlest0211
          UP TO 1 ROWS
        WHERE vbeln = @w_alv_transf-rem_vbeln
          AND posnr =	@w_alv_transf-rem_posnr
          AND ebeln = @w_alv_transf-rem_vgbel
          AND ebelp = @w_alv_transf-rem_vgpos.
      ENDSELECT.

      l_lifnr = w_zlest0211-cod_loc_coleta.
      l_kunnr = w_zlest0211-cod_loc_entrega.

      IF l_lifnr IS INITIAL OR l_kunnr IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-147 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT lifnr, lzone, ktokk
        FROM lfa1
        INTO @DATA(w_lfa1)
          UP TO 1 ROWS
       WHERE lifnr   = @l_lifnr.
      ENDSELECT.

      SELECT *
      FROM tvro
      INTO @DATA(w_tvro)
        UP TO 1  ROWS
       WHERE route  = @w_alv_transf-rem_route.
      ENDSELECT.

      SELECT shtyp
        INTO @DATA(l_shtyp)
        FROM zsdt0011
          UP TO 1 ROWS
       WHERE bsart = @w_alv_transf-ped_bsart.
      ENDSELECT.

      SELECT shtyp, laufk
        FROM tvtk
        INTO @DATA(w_tvtk)
          UP TO 1 ROWS
       WHERE shtyp = @l_shtyp.
      ENDSELECT.

*-----------------------------------------
*-- montar header
*-----------------------------------------
      CLEAR st_headerdata.

      st_headerdata-service_agent_id        = w_zlest0211-agente_frete.
      st_headerdata-service_level           = '1'.
      st_headerdata-shipping_type           = '01'.
      st_headerdata-status_plan             = 'X'.
      st_headerdata-status_checkin          = 'X'.
      st_headerdata-status_load_start       = 'X'.
      st_headerdata-special_procedure_id 	  = '0001'.
      st_headerdata-shpmnt_cost_rel         = 'X'.
      st_headerdata-shipment_type           = l_shtyp.
      st_headerdata-trans_plan_pt           = w_alv_transf-rem_filial.
      st_headerdata-shipment_route          = w_alv_transf-rem_route.
      st_headerdata-distance                = w_tvro-distz.
      st_headerdata-distance_unit           = w_tvro-medst.
      st_headerdata-time_travel             =	w_tvro-traztd.
      st_headerdata-time_unit               =	'H'.

*-----------------------------------------
*-- etapa
*-----------------------------------------
      CLEAR st_stagedata.
      st_stagedata-stage_cat      = '1'.
      st_stagedata-stage_seq      = '0001'.
      st_stagedata-shipping_type  =  '01'.
      st_stagedata-service_agent  = w_alv_transf-ag_frete.

      IF w_lfa1-ktokk = 'ZFIC'.
        st_stagedata-org_shipp_dpmnt = w_lfa1-lifnr+6(4).
      ELSE.
        st_stagedata-org_suppl       = w_lfa1-lifnr.
      ENDIF.

      st_stagedata-leg_indicator = w_tvtk-laufk. "Código de Percurso

      "Se Código de Percurso  igual a 1:  Percurso preliminar
      IF w_tvtk-laufk = 1.
        SELECT SINGLE knote
          FROM tvkn
          INTO @DATA(l_knote)
         WHERE kunnr   = @l_kunnr.

        IF sy-subrc = 0.
          st_stagedata-dest_point   = l_knote.
        ELSE.
          st_stagedata-dest_point   = l_kunnr.
        ENDIF.

        "Se Código de Percurso  igual a 4:  Percurso direto
      ELSEIF w_tvtk-laufk = 4.
        IF st_headerdata-shipment_type  =  'Z004'.
          SELECT SINGLE stcd1
            FROM kna1
            INTO @DATA(v_stcd1)
           WHERE kunnr = @l_kunnr.

          SELECT SINGLE lifnr
            FROM lfa1
            INTO @DATA(v_lifnr)
           WHERE stcd1 = @v_stcd1.

          st_stagedata-dest_suppl  = v_lifnr.

        ELSE.
          st_stagedata-dest_cust   = l_kunnr. "Local de entrega (V_KUNNR)
        ENDIF.
      ENDIF.


*-----------------------------------------
*-- itens
*-----------------------------------------
      CLEAR st_itemdata.

      st_itemdata-delivery    = w_alv_transf-rem_vbeln.
      st_itemdata-itenerary   = '000010'.

      CLEAR: t_return.

      TRY .
          zcl_transportation_utils=>zif_transportation_utils~get_instance(
            )->gerar_vt( EXPORTING i_rem_vbeln  = w_alv_transf-rem_vbeln
                                   i_headerdata = st_headerdata
                                   i_itemdata   = st_itemdata
                                   i_stagedata  = st_stagedata
                                   "i_return_vt  = t_return_vt
                            IMPORTING e_doc_transp = p_transp
                                      e_return     = t_return
            ).
        CATCH zcx_transportation_utils INTO DATA(ex_zcx_transportation_utils).
          l_erro = abap_true.
      ENDTRY.

      IF l_erro = abap_true OR p_transp IS INITIAL.
        p_transp = icon_message_error.
        "t_return = zcl_transportation_utils=>zif_transportation_utils~get_tab_return( ).

        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                  i_ebeln      = w_alv_transf-rem_vgbel
                                                                                  i_ebelp      = w_alv_transf-rem_vgpos
                                                                                  i_etapa_proc = '04' "C_ETAPA_GERAR_VT
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_return[] ).

      ELSE.
        p_transp = p_transp.

*-----------------------------
*---- Atualiza documento
*-----------------------------
        UPDATE zlest0211 SET doc_transp =  p_transp
                             st_proc    = '04'
                       WHERE vbeln = w_alv_transf-rem_vbeln
                         AND ebeln = w_alv_transf-rem_vgbel
                         AND ebelp = w_alv_transf-rem_vgpos.

        COMMIT WORK.

        CLEAR: t_return.
        TRY .
            zcl_transportation_utils=>zif_transportation_utils~get_instance(
              )->finalizar_vt( EXPORTING i_rem_vbeln  = w_alv_transf-rem_vbeln
                                         i_tknum =  p_transp
                               IMPORTING e_doc_transp = p_transp
                                         e_return     = t_return
              ).
          CATCH zcx_transportation_utils INTO ex_zcx_transportation_utils.
            l_erro = abap_true.
        ENDTRY.

        IF l_erro = abap_true OR p_transp IS INITIAL.
          p_transp = icon_message_error.

          UPDATE zlest0211 SET doc_transp =  ''
                         st_proc    = '03'
                   WHERE vbeln = w_alv_transf-rem_vbeln
                     AND ebeln = w_alv_transf-rem_vgbel
                     AND ebelp = w_alv_transf-rem_vgpos.


          "t_return = zcl_transportation_utils=>zif_transportation_utils~get_tab_return( ).

          zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                    i_ebeln      = w_alv_transf-rem_vgbel
                                                                                    i_ebelp      = w_alv_transf-rem_vgpos
                                                                                    i_etapa_proc = '04' "C_ETAPA_GERAR_VT
                                                                                    i_commit     = abap_true
                                                                          CHANGING  t_return     = t_return[] ).
        ELSE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.


**********************************************************************
* GERAR DOC CUSTO
**********************************************************************
FORM f_gera_doc_custo        USING p_transp
                                   p_vbeln_venda
                                   p_nf_venda
                                   p_remessa_dummy
                                   p_agente_frete
                          CHANGING p_doc_custo
                                   p_ordem_serv
                                   p_fatura_serv
                                   p_dacte.

  DATA: l_status TYPE char1,
        l_erro   TYPE char1.

  FREE: l_erro,
        p_doc_custo,
        p_ordem_serv,
        p_fatura_serv.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Gerando Documentos de Custo / Ordem Serviço/ Fatura / DACTE...'.

  TRY .
      zcl_remessa_terceiro=>zif_remessa_terceiro~get_instance(
        )->set_cria_doc_custo( EXPORTING i_tknum         = p_transp
                                         i_vbeln_venda   = p_vbeln_venda
                                         i_nf_venda      = p_nf_venda
                                         i_remessa_dummy = p_remessa_dummy
                                         i_agente_frete  = p_agente_frete
                               IMPORTING e_doc_custo     = p_doc_custo
                                         e_ordem_serv    = p_ordem_serv
                                         e_fatura_serv   = p_fatura_serv
                                         e_dacte         = p_dacte
        ).
    CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    p_doc_custo     = icon_icon_list.
    p_ordem_serv    = icon_icon_list.
    p_fatura_serv   = icon_icon_list.
    p_dacte         = icon_icon_list.
  ELSE.
    IF p_doc_custo IS INITIAL.
      p_doc_custo   = icon_icon_list.
    ELSE.
      p_doc_custo   = p_doc_custo.
    ENDIF.

    IF p_ordem_serv IS INITIAL.
      p_ordem_serv  = icon_icon_list.
    ELSE.
      p_ordem_serv  = p_ordem_serv.
    ENDIF.

    IF p_fatura_serv IS INITIAL.
      p_fatura_serv = icon_icon_list.
    ELSE.
      p_fatura_serv = p_fatura_serv.
    ENDIF.

    IF p_dacte IS INITIAL.
      p_dacte       = icon_icon_list.
    ELSE.
      p_dacte       = p_dacte.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* GERAR DOC CUSTO TRANSF
**********************************************************************
FORM f_gera_doc_custo_transf  USING w_alv_transf TYPE  ty_alv_transf
                                  CHANGING p_doc_custo
                                           p_ordem_serv
                                           p_fatura_serv
                                           p_dacte.

  DATA: l_status TYPE char1,
        l_erro   TYPE char1.

  FREE: l_erro,
        p_doc_custo,
        p_ordem_serv,
        p_fatura_serv.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Gerando Documentos de Custo / Ordem Serviço/ Fatura / DACTE...'.

  CLEAR: t_return.
  TRY .
      zcl_transportation_utils=>zif_transportation_utils~get_instance(
        )->gerar_doc_custo(    EXPORTING i_tknum         = w_alv_transf-transp
                                         i_vbeln         = w_alv_transf-rem_vbeln
                                         i_agente_frete  = w_alv_transf-ag_frete
                               IMPORTING e_doc_custo     = p_doc_custo
                                         e_ordem_serv    = p_ordem_serv
                                         e_fatura_serv   = p_fatura_serv
                                         e_dacte         = p_dacte
                                         e_return        = t_return
        ).
    CATCH zcx_transportation_utils INTO DATA(ex_zcx_transportation_utils).
      l_erro = abap_true.
  ENDTRY.

  "t_return = zcl_transportation_utils=>zif_transportation_utils~get_tab_return( ).
  IF t_return[] IS NOT INITIAL.

    zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                              i_ebeln      = w_alv_transf-rem_vgbel
                                                                              i_ebelp      = w_alv_transf-rem_vgpos
                                                                              i_etapa_proc = '99'
                                                                              i_commit     = abap_true
                                                                    CHANGING  t_return     = t_return[] ).
  ENDIF.

  IF l_erro = abap_true.
    p_doc_custo     = icon_icon_list.
    p_ordem_serv    = icon_icon_list.
    p_fatura_serv   = icon_icon_list.
    p_dacte         = icon_icon_list.
  ELSE.
    IF p_doc_custo IS INITIAL.
      p_doc_custo   = icon_icon_list.
    ELSE.
      p_doc_custo   = p_doc_custo.

      UPDATE zlest0211 SET fknum   = p_doc_custo
                           st_proc = '05'
        WHERE vbeln  = w_alv_transf-rem_vbeln
          AND ebeln  = w_alv_transf-rem_vgbel
          AND ebelp  = w_alv_transf-rem_vgpos.

      COMMIT WORK.

    ENDIF.

    IF p_ordem_serv IS INITIAL.
      p_ordem_serv  = icon_icon_list.
    ELSE.
      p_ordem_serv  = p_ordem_serv.

      UPDATE zlest0211 SET ov_frete     = p_ordem_serv
                           st_proc      = '06'
          WHERE vbeln  = w_alv_transf-rem_vbeln
            AND ebeln  = w_alv_transf-rem_vgbel
            AND ebelp  = w_alv_transf-rem_vgpos.

      COMMIT WORK.

    ENDIF.

    IF p_fatura_serv IS INITIAL.
      p_fatura_serv = icon_icon_list.

    ELSE.
      p_fatura_serv = p_fatura_serv.

      UPDATE zlest0211 SET
               fatura_frete = p_fatura_serv
               st_proc      = '07'
      WHERE vbeln = w_alv_transf-rem_vbeln
      AND ebeln   = w_alv_transf-rem_vgbel
      AND ebelp   = w_alv_transf-rem_vgpos.

      COMMIT WORK.
    ENDIF.

    IF p_dacte IS INITIAL.
      p_dacte       = icon_icon_list.
    ELSE.
      p_dacte       = p_dacte.

      UPDATE zlest0211 SET nro_nf_frete = p_dacte
                           st_proc      = '08'
      WHERE vbeln = w_alv_transf-rem_vbeln
        AND ebeln = w_alv_transf-rem_vgbel
        AND ebelp = w_alv_transf-rem_vgpos.

      COMMIT WORK.

    ENDIF.
  ENDIF.
ENDFORM.


**********************************************************************
* estorno CTE
**********************************************************************
FORM f_estorno_cte.

  DATA: l_answer TYPE c,
        l_erro   TYPE char1.

*---------------------------------------------
* confirmacao estorno
*---------------------------------------------
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma o Estorno?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_answer = '1'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Estornando Documentos...'.

*---------------------------------------------
* efetua estorno
*---------------------------------------------
  LOOP AT t_rows     INTO w_rows.
    READ TABLE t_alv INTO w_alv INDEX w_rows-index.

    TRY .
        zcl_remessa_terceiro=>zif_remessa_terceiro~get_instance(
          )->set_estorna_cte( EXPORTING i_vbeln_venda   = w_alv-vbeln_venda
                                        i_nf_venda      = w_alv-nf_venda
                                        i_remessa_dummy = w_alv-remessa_dummy
          ).
      CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
        l_erro = abap_true.
    ENDTRY.
  ENDLOOP.

ENDFORM.

**********************************************************************
* estorno transf
**********************************************************************
FORM f_estorno_transf.

  DATA: l_answer TYPE c,
        l_erro   TYPE char1.

*---------------------------------------------
* confirmacao estorno
*---------------------------------------------
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma o Estorno?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_answer = '1'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Estornando Documentos...'.

*---------------------------------------------
* efetua estorno
*---------------------------------------------
  LOOP AT t_rows     INTO w_rows.

    READ TABLE t_alv_transf INTO w_alv_transf INDEX w_rows-index.

*------------------------------------
*-- checa doctos criados
*------------------------------------
    SELECT *
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @w_alv_transf-rem_vbeln
       AND ebeln = @w_alv_transf-rem_vgbel
       AND ebelp = @w_alv_transf-rem_vgpos.

    ENDSELECT.

    CHECK sy-subrc = 0.

*------------------------------------
*-- estorno fatura servico
*------------------------------------
    IF w_zlest0211-fatura_frete IS NOT INITIAL.
      TRY .
          zcl_transportation_utils=>zif_transportation_utils~get_instance(
            )->estornar_fatura_servico( EXPORTING i_document = w_zlest0211-fatura_frete
                                        IMPORTING e_return   = t_return ).

        CATCH zcx_transportation_utils INTO DATA(ex_zcx_transportation_utils).
          l_erro = abap_true.
      ENDTRY.

      IF t_return[] IS NOT INITIAL.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                  i_ebeln      = w_alv_transf-rem_vgbel
                                                                                  i_ebelp      = w_alv_transf-rem_vgpos
                                                                                  i_etapa_proc = '06'
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_return[] ).

      ENDIF.

      IF l_erro IS INITIAL AND t_return[] IS INITIAL.
        UPDATE zlest0211 SET fatura_frete = abap_off
                     st_proc = '06'
               WHERE vbeln   = w_alv_transf-rem_vbeln
                 AND ebeln   = w_alv_transf-rem_vgbel
                 AND ebelp   = w_alv_transf-rem_vgpos.

        COMMIT WORK.

      ENDIF.
    ENDIF.
*------------------------------------
*-- estorno OV frete
*------------------------------------
    IF w_zlest0211-ov_frete IS NOT INITIAL.
      TRY .
          zcl_transportation_utils=>zif_transportation_utils~get_instance(
            )->estornar_ov_servico( EXPORTING i_salesdocument   = w_zlest0211-ov_frete
                                    IMPORTING e_return = t_return ).
        CATCH zcx_transportation_utils INTO ex_zcx_transportation_utils.
          l_erro = abap_true.
      ENDTRY.
      "t_return = zcl_transportation_utils=>zif_transportation_utils~get_tab_return( ).

      IF t_return[] IS NOT INITIAL.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                  i_ebeln      = w_alv_transf-rem_vgbel
                                                                                  i_ebelp      = w_alv_transf-rem_vgpos
                                                                                  i_etapa_proc = '05'
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_return[] ).

      ENDIF.

      IF l_erro IS INITIAL AND  t_return[] IS INITIAL.
        UPDATE zlest0211 SET ov_frete = abap_off
                             st_proc  = '05'
                       WHERE vbeln   =  w_alv_transf-rem_vbeln
                         AND ebeln   =  w_alv_transf-rem_vgbel
                         AND ebelp   =  w_alv_transf-rem_vgpos.
        COMMIT WORK.

      ENDIF.
    ENDIF.
*------------------------------------
*-- estorno doc custo
*------------------------------------
    IF w_zlest0211-fknum IS NOT INITIAL.

*--------------------------------------
*-- cif / ctp
*--------------------------------------
      SELECT ktokk
        INTO @DATA(l_ktokk)
        FROM lfa1
          UP TO 1 ROWS
        WHERE lifnr = @w_zlest0211-agente_frete.
      ENDSELECT.

      TRY .
          zcl_transportation_utils=>zif_transportation_utils~get_instance(
            )->estornar_doc_custo( EXPORTING  i_fknum  = w_zlest0211-fknum
                                              i_ktokk  = l_ktokk
                                   IMPORTING  e_return = t_return


            ).
        CATCH zcx_transportation_utils INTO ex_zcx_transportation_utils.
          l_erro = abap_true.
      ENDTRY.

      IF t_return[] IS NOT INITIAL.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                  i_ebeln      = w_alv_transf-rem_vgbel
                                                                                  i_ebelp      = w_alv_transf-rem_vgpos
                                                                                  i_etapa_proc = '04'
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_return[] ).

      ENDIF.

      IF l_erro IS INITIAL AND t_return[] IS INITIAL.
        UPDATE zlest0211 SET fknum    = abap_off
                             st_proc  = '04'
                       WHERE vbeln    =  w_alv_transf-rem_vbeln
                          AND ebeln   =  w_alv_transf-rem_vgbel
                          AND ebelp   =  w_alv_transf-rem_vgpos.

        COMMIT WORK.
      ENDIF.
    ENDIF.

*------------------------------------
*-- estorno doc transporte
*------------------------------------
    IF w_zlest0211-doc_transp IS NOT INITIAL.

      TRY .
          zcl_transportation_utils=>zif_transportation_utils~get_instance(
            )->eliminar_vt( EXPORTING i_tknum     = w_zlest0211-doc_transp
                                      i_delivery  = w_alv_transf-rem_vbeln
                            IMPORTING  e_return = t_return ).
        CATCH zcx_transportation_utils INTO ex_zcx_transportation_utils.
          l_erro = abap_true.
      ENDTRY.

      " t_return = zcl_transportation_utils=>zif_transportation_utils~get_tab_return( ).
      IF t_return[] IS NOT INITIAL.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                  i_ebeln      = w_alv_transf-rem_vgbel
                                                                                  i_ebelp      = w_alv_transf-rem_vgpos
                                                                                  i_etapa_proc = '03'
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_return[] ).

      ENDIF.
      IF l_erro IS INITIAL AND t_return[] IS INITIAL.
*-----------------------------
*---- Atualiza documento
*-----------------------------
        IF w_alv_transf-rem_vbeln IS NOT INITIAL.
          UPDATE zlest0211 SET doc_transp = abap_off
                               st_proc    = '03'
                         WHERE vbeln   = w_alv_transf-rem_vbeln
                           AND ebeln   =  w_alv_transf-rem_vgbel
                           AND ebelp   =  w_alv_transf-rem_vgpos.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_TELA
*&---------------------------------------------------------------------*
FORM f_modifica_tela .
  LOOP AT SCREEN.
    IF screen-group1 = 'SC1' AND p_nfprop = 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF screen-group1 = 'SC2' AND p_nfterc = 'X'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.
***********************************************************************
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  F_SELECAO_DADOS_NF_PROPRIA
*&---------------------------------------------------------------------*
FORM f_selecao_dados_nf_propria .

  DATA: lva_refkey TYPE j_1bnflin-refkey.


  RANGES: r_icon FOR likp-inco1,
          r_tcode FOR likp-tcode.


  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CIF'  ) TO r_icon.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CPT'  ) TO r_icon.


  APPEND VALUE #( sign = 'I' option = 'EQ' low =  'VL10X' )   TO r_tcode.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ZLES0200' ) TO r_tcode.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ZLES0136' ) TO r_tcode.  "*-CS2024000522-12.09.2024-JT-#151251-inicio
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'SE37'     ) TO r_tcode.  "*-CS2024000522-12.09.2024-JT-#151251-inicio
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'SE38'     ) TO r_tcode.  "*-CS2024000522-12.09.2024-JT-#151251-inicio

  PERFORM f_refresh.
* 2 - Seleções
* 2.1 - Seleção principal
* Se informado o campo Pedido

  IF s_ebeln[] IS NOT INITIAL.
    SELECT *
      FROM ekko
      INTO TABLE t_ekko
     WHERE bsart  = 'ZUB'
       AND reswk IN s_reswk
*      AND aedat IN s_aedat   "*-CS2024000522-18.07.2024-JT-#143588
       AND ebeln IN s_ebeln.

    IF t_dados_remessa[] IS NOT INITIAL.
      SELECT *
        FROM ekko
        APPENDING  TABLE t_ekko
        FOR ALL ENTRIES IN t_dados_remessa
       WHERE bsart  = 'ZUB'
         AND ebeln  = t_dados_remessa-ped_ebeln.
*     REFRESH t_dados_remessa.  "*-CS2024000522-18.07.2024-JT-#143588 comentado
    ENDIF.

*-CS2024000522-18.07.2024-JT-#147087-inicio
    IF t_ekko[] IS NOT INITIAL.
      SELECT *
        FROM ekbe
        INTO TABLE t_ekbe_aux
         FOR ALL ENTRIES IN t_ekko
        WHERE ebeln  = t_ekko-ebeln
          AND budat IN s_aedat.

      SORT t_ekbe_aux BY ebeln.
      DELETE ADJACENT DUPLICATES FROM t_ekbe_aux COMPARING ebeln.

      LOOP AT t_ekko INTO w_ekko.
        l_tabix = sy-tabix.
        READ TABLE t_ekbe_aux INTO w_ekbe_aux WITH KEY ebeln = w_ekko-ebeln
                                              BINARY SEARCH.
        IF sy-subrc <> 0.
          DELETE t_ekko INDEX l_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
*-CS2024000522-18.07.2024-JT-#147087-inicio

    SORT t_ekko BY ebeln .

    CHECK t_ekko[] IS NOT INITIAL.

    SELECT lips~*                               "*-CS2024000522-18.07.2024-JT-#147087-inicio
      FROM lips
     INNER JOIN likp ON likp~vbeln = lips~vbeln "*-CS2024000522-18.07.2024-JT-#147087-inicio
      INTO TABLE @t_lips
       FOR ALL ENTRIES IN @t_ekko
     WHERE lips~vgbel  = @t_ekko-ebeln
       AND likp~erdat IN @s_erdat .             "*-CS2024000522-18.07.2024-JT-#147087-inicio
  ELSE.
* Senão não informado o campo pedido
    SELECT *
      FROM likp
      INTO TABLE t_likp
     WHERE vstel  IN s_reswk
       AND erdat  IN s_erdat .

    CHECK t_likp[] IS NOT INITIAL.

    SELECT *
      FROM lips
      INTO TABLE t_lips
       FOR ALL ENTRIES IN t_likp
     WHERE vbeln  = t_likp-vbeln.

    CHECK t_lips[] IS NOT INITIAL.

    SELECT *
      FROM ekko
      INTO TABLE t_ekko
       FOR ALL ENTRIES IN t_lips
     WHERE bsart   = 'ZUB'
      AND ebeln  = t_lips-vgbel.  "*-CS2024000522-18.07.2024-JT-#143588
*     AND aedat IN s_aedat.

  ENDIF.

* 2.1.2  Pegar remessas que somente sejam de pedido
  DELETE t_lips WHERE vgtyp <> 'V'.

* 2.1.1-  Busca dados LIKP
  IF t_lips[] IS NOT INITIAL.

    CLEAR: t_likp.
    SELECT *
      FROM likp
      INTO TABLE t_likp
      FOR ALL ENTRIES IN t_lips
     WHERE vbeln  = t_lips-vbeln.

* 2.1.3 - Busca dados  de faturamento

    LOOP AT t_lips  INTO w_lips.
      w_lips-xblnr = w_lips-vbeln.
      MODIFY t_lips FROM w_lips INDEX sy-tabix.
    ENDLOOP.


    SELECT *
      FROM ekbe
      INTO TABLE t_ekbe_aux
      FOR ALL ENTRIES IN t_lips
     WHERE ebeln = t_lips-vgbel
      AND  ebelp = t_lips-vgpos+1(5)
      AND  vgabe = '6'
      AND  bewtp = 'U'
      AND  bwart = '862'
      AND  xblnr = t_lips-xblnr.


* 2.1.4 -  Regra
    DELETE t_likp WHERE tcode NOT IN r_tcode..
    DELETE t_likp WHERE inco1 NOT IN  r_icon.

* 2.2 - Busca Outros Dados do Pedido
* 2.2.1 - Busca Parceiros do Pedido
    SELECT *
      FROM ekpa
      INTO TABLE t_ekpa
       FOR ALL ENTRIES IN t_ekko
     WHERE ebeln  = t_ekko-ebeln
      AND  parvw  = 'PR'.

*-CS2024000522-12.09.2024-JT-#151251-inicio
    SELECT *
      INTO TABLE t_zsdt0011
      FROM zsdt0011
       FOR ALL ENTRIES IN t_ekko
     WHERE bsart        = t_ekko-bsart
       AND tp_movimento = 'S'.
*-CS2024000522-12.09.2024-JT-#151251-fim

* 2.2.2 - Busca item do pedido
    SELECT *
      FROM ekpo
      INTO TABLE t_ekpo
       FOR ALL ENTRIES IN t_ekko
     WHERE ebeln  = t_ekko-ebeln.

    SELECT *
      FROM ekpv
      INTO TABLE t_ekpv
       FOR ALL ENTRIES IN t_ekpo
     WHERE ebeln = t_ekpo-ebeln
      AND  ebelp = t_ekpo-ebelp.

    SELECT *
      FROM eket
      INTO TABLE t_eket
       FOR ALL ENTRIES IN t_ekpo
      WHERE ebeln = t_ekpo-ebeln
      AND  ebelp = t_ekpo-ebelp.

* 3 - Outras  seleções
* 3.1 - Busca dados J_1BNFLIN
    LOOP AT t_ekbe_aux INTO w_ekbe_aux.
      CONCATENATE w_ekbe_aux-belnr w_ekbe_aux-gjahr   INTO lva_refkey.

      w_ekbe-ebeln  = w_ekbe_aux-ebeln.
      w_ekbe-ebelp  = w_ekbe_aux-ebelp.
      w_ekbe-belnr  = w_ekbe_aux-belnr.
      w_ekbe-gjahr  = w_ekbe_aux-gjahr.
      w_ekbe-xblnr  = w_ekbe_aux-xblnr.
      w_ekbe-refkey = lva_refkey.

      APPEND w_ekbe TO t_ekbe.
      CLEAR: w_ekbe, lva_refkey.

    ENDLOOP.


    IF t_ekbe[] IS NOT INITIAL.
      SELECT *
      FROM j_1bnflin
      INTO TABLE t_j_1bnflin
       FOR ALL ENTRIES IN t_ekbe
      WHERE refkey = t_ekbe-refkey.

* 3.2 - Busca dados J_1BNFDOC
      IF  t_j_1bnflin[] IS NOT INITIAL.  "*-CS2024000522-26.08.2024-JT-#147087-inicio
        SELECT *
        FROM j_1bnfdoc
        INTO TABLE t_j_1bnfdoc
         FOR ALL ENTRIES IN t_j_1bnflin
        WHERE docnum = t_j_1bnflin-docnum.

* 3.3 - Busca dados J_1BNFSTX
        SELECT *
          FROM j_1bnfstx
          INTO TABLE t_j_1bnfstx
            FOR ALL ENTRIES IN t_j_1bnflin
          WHERE docnum = t_j_1bnflin-docnum
          AND itmnum  =  t_j_1bnflin-itmnum
          AND taxtyp  =  'ICM0'.
      ENDIF.

* 3.4 - Busca dados J_1BNFE_ACTIVE
      IF  t_j_1bnfdoc[] IS NOT INITIAL.  "*-CS2024000522-26.08.2024-JT-#147087-inicio
        SELECT *
        FROM j_1bnfe_active
        INTO TABLE t_j_1bnfe_active
         FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum = t_j_1bnfdoc-docnum.
      ENDIF.
    ENDIF.

* 4 - Recuperar documento de transporte rodoviário, caso exista
* 4.1 - Busca TKNUM ( VT)
    IF  t_lips[] IS NOT INITIAL.  "*-CS2024000522-26.08.2024-JT-#147087-inicio
      SELECT *
        FROM vttp
        INTO TABLE t_vttp
        FOR ALL ENTRIES IN t_lips
       WHERE vbeln  = t_lips-vbeln.
    ENDIF.

*   IF sy-subrc = 0.
    IF t_vttp[] IS NOT INITIAL. "*-CS2024000522-26.08.2024-JT-#147087-inicio
* 4.2 - Busca dados do transporte
      SELECT *
        FROM vttk
        INTO TABLE t_vttk
        FOR ALL ENTRIES IN t_vttp
       WHERE tknum = t_vttp-tknum.

* 4.3 - Busca dado do Custo
      SELECT *
      FROM vfkp
      INTO TABLE t_vfkp
      FOR ALL ENTRIES IN t_vttp
      WHERE rebel = t_vttp-tknum.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS_NF_PROPRIA
*&---------------------------------------------------------------------*
FORM f_processa_saida_03.

  FREE: t_alv_transf.

  LOOP AT t_lips INTO w_lips.

    CLEAR: w_ekko.
    READ TABLE t_ekko INTO w_ekko WITH KEY ebeln = w_lips-vgbel.

    w_alv_transf-ped_ebeln    = w_ekko-ebeln.
    w_alv_transf-ped_data     = w_ekko-aedat.
    w_alv_transf-ped_bsart    = w_ekko-bsart.
    w_alv_transf-ped_reswr    = w_ekko-reswk.

    w_alv_transf-rem_vbeln    =  w_lips-vbeln.
    w_alv_transf-rem_posnr    =  w_lips-posnr.
    w_alv_transf-rem_matnr    =  w_lips-matnr.
    w_alv_transf-rem_gp_matnr =  w_lips-matkl.
    w_alv_transf-rem_lgort    =  w_lips-lgort.
    w_alv_transf-rem_charg    =  w_lips-charg.
    w_alv_transf-rem_vgbel    =  w_lips-vgbel.
    w_alv_transf-rem_vgpos    =  w_lips-vgpos.
    w_alv_transf-rem_vgtyp    =  w_lips-vgtyp.


    CLEAR: w_likp .
    READ TABLE t_likp INTO w_likp WITH KEY vbeln = w_lips-vbeln.

    w_alv_transf-rem_lfdat   =   w_likp-erdat. "*-CS2024000522-12.09.2024-JT-#151251-inicio
    w_alv_transf-rem_inco1   =   w_likp-inco1.
    w_alv_transf-rem_route   =   w_likp-route.
    w_alv_transf-rem_kunnr   =   w_likp-kunnr.
    w_alv_transf-rem_brgew   =   w_likp-btgew.
    w_alv_transf-rem_gewei   =   w_likp-gewei.
    w_alv_transf-rem_tcode   =   w_likp-tcode.
    w_alv_transf-rem_data    =   w_likp-erdat.
    w_alv_transf-rem_filial  =   w_likp-vstel.


    IF  w_alv_transf-rem_vgtyp  = 'V'.
      CLEAR: w_ekbe.
      READ TABLE t_ekbe INTO w_ekbe WITH KEY ebeln    = w_alv_transf-rem_vgbel
                                             ebelp    = w_alv_transf-rem_vgpos
                                             xblnr    = w_lips-xblnr.

      w_alv_transf-rem_fat      = w_ekbe-belnr.
      w_alv_transf-rem_fat_ano  = w_ekbe-gjahr.
    ENDIF.

    CLEAR: w_ekpa.
    READ TABLE t_ekpa INTO w_ekpa WITH KEY  ebeln    =  w_alv_transf-ped_ebeln.
    w_alv_transf-ped_coleta = w_ekpa-lifn2.

    CLEAR: w_ekpo.
    READ TABLE t_ekpo INTO w_ekpo WITH KEY ebeln  = w_alv_transf-ped_ebeln.

    w_alv_transf-ped_ebelp    =  w_ekpo-ebelp .

    w_alv_transf-ped_matnr    =  w_ekpo-matnr .
    w_alv_transf-ped_bukrs    =  w_ekpo-bukrs .
    w_alv_transf-ped_menge    =  w_ekpo-menge .
    w_alv_transf-ped_meins    =  w_ekpo-meins .
    w_alv_transf-ped_icon1    =  w_ekpo-inco1 .
    w_alv_transf-ped_wekrs    =  w_ekpo-werks .
    w_alv_transf-ped_lgort    =  w_ekpo-lgort .
    w_alv_transf-ped_gp_matnr =  w_ekpo-matkl .

*    Transformar XPED-RESWR é um código de fornecedor
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_ekpo-werks
      IMPORTING
        output = w_alv_transf-ped_entrega.

    CLEAR: w_ekpv.
    READ TABLE t_ekpv INTO w_ekpv WITH KEY  ebeln =  w_alv_transf-ped_ebeln
                                            ebelp =  w_alv_transf-ped_ebelp.
    w_alv_transf-ped_route  = w_ekpv-route.

    CLEAR: w_eket.
    READ TABLE t_eket INTO w_eket WITH KEY  ebeln =  w_alv_transf-ped_ebeln
                                         ebelp =  w_alv_transf-ped_ebelp.
    w_alv_transf-ped_charg  = w_eket-charg.

    CONCATENATE w_alv_transf-rem_fat w_alv_transf-rem_fat_ano INTO DATA(lva_refkey).

    CLEAR: w_j_1bnflin.
    READ TABLE t_j_1bnflin INTO w_j_1bnflin WITH KEY refkey = lva_refkey.
    w_alv_transf-rem_doc_nf = w_j_1bnflin-docnum.

    CLEAR: w_j_1bnfdoc.
    READ TABLE t_j_1bnfdoc INTO w_j_1bnfdoc WITH KEY docnum = w_alv_transf-rem_doc_nf .
    w_alv_transf-rem_nr_nf = w_j_1bnfdoc-nfenum.

    CLEAR: w_j_1bnfstx.
    READ TABLE t_j_1bnfstx INTO w_j_1bnfstx WITH KEY docnum = w_alv_transf-rem_doc_nf
                                                     itmnum  =  w_j_1bnflin-itmnum.
    w_alv_transf-base   = w_j_1bnfstx-base  .
    w_alv_transf-taxval = w_j_1bnfstx-taxval.

    CLEAR: w_j_1bnfe_active.
    READ TABLE  t_j_1bnfe_active INTO w_j_1bnfe_active WITH KEY docnum = w_alv_transf-rem_doc_nf.

    CONCATENATE
    w_j_1bnfe_active-regio
    w_j_1bnfe_active-nfyear
    w_j_1bnfe_active-nfmonth
    w_j_1bnfe_active-stcd1
    w_j_1bnfe_active-model
    w_j_1bnfe_active-serie
    w_j_1bnfe_active-nfnum9
    w_j_1bnfe_active-docnum9
    w_j_1bnfe_active-cdv INTO DATA(lva_chv_nfe).

    w_alv_transf-chave_nf = lva_chv_nfe.


    CLEAR: w_vttp.
    READ TABLE  t_vttp INTO w_vttp WITH KEY vbeln  = w_alv_transf-rem_vbeln.
    w_alv_transf-frete_tknum = w_vttp-tknum.

    CLEAR: w_vttk.
    READ TABLE  t_vttk INTO w_vttk WITH KEY tknum =  w_alv_transf-frete_tknum.

    CLEAR: w_zsdt0011.                                                     "*-CS2024000522-12.09.2024-JT-#151251-inicio
    READ TABLE  t_zsdt0011 INTO w_zsdt0011 WITH KEY bsart = w_ekko-bsart.  "*-CS2024000522-12.09.2024-JT-#151251-inicio

    w_alv_transf-frete_shtyp   = w_zsdt0011-shtyp. "w_vttk-shtyp.          "*-CS2024000522-12.09.2024-JT-#151251-inicio
    w_alv_transf-frete_placa   = w_vttk-text1(7).  "pegar somente os 7 primeiros caracteres
    w_alv_transf-frete_agente  = w_vttk-tdlnr.

    CLEAR: w_vfkp.
    READ TABLE  t_vfkp INTO w_vfkp WITH KEY rebel =  w_alv_transf-frete_tknum.
    w_alv_transf-frete_custo  = w_vfkp-fknum .

    w_alv_transf-transp      = zcl_frete_remessa_trans=>zif_frete_remessa_trans~get_status_vt(   i_rem_vbeln  = w_alv_transf-rem_vbeln ).

    IF   w_alv_transf-transp  IS INITIAL.
      w_alv_transf-transp      = icon_execute_object.
    ENDIF.

    CLEAR: l_lock_ag_frete.
    zcl_frete_remessa_trans=>zif_frete_remessa_trans~get_dados_transporte( EXPORTING i_rem_vbeln     = w_alv_transf-rem_vbeln
                                                                                     i_ebeln         = w_alv_transf-rem_vgbel
                                                                                     i_ebelp         = w_alv_transf-rem_vgpos
                                                                           IMPORTING e_placa         = w_alv_transf-placa
                                                                                     e_quantidade    = w_alv_transf-quantidade
                                                                                     e_tp_frete      = w_alv_transf-tp_frete
                                                                                     e_itinerario    = w_alv_transf-itinerario
                                                                                     e_vlr_frete     = w_alv_transf-vlr_frete
                                                                                     e_unid_cond     = w_alv_transf-unid_cond
                                                                                     e_dados_transp  = w_alv_transf-dados_transp
                                                                                     e_lock_ag_frete = l_lock_ag_frete
                                                                           CHANGING  c_ag_frete      = w_alv_transf-ag_frete ).



    zcl_frete_remessa_trans=>zif_frete_remessa_trans~get_status_outros_docs( EXPORTING i_rem_vbeln   = w_alv_transf-rem_vbeln
                                                                                       i_ebeln       = w_alv_transf-rem_vgbel
                                                                                       i_ebelp       = w_alv_transf-rem_vgpos
                                                                             IMPORTING e_doc_custo   = w_alv_transf-doc_custo
                                                                                       e_ordem_serv  = w_alv_transf-ov_serv
                                                                                       e_fatura_serv = w_alv_transf-fat_serv
                                                                                       e_dacte       = w_alv_transf-dacte ).

    IF NOT w_alv_transf-ped_ebeln IS INITIAL AND "DEVK9A1UVH - 02.02.2024 - #84717 RSA
       NOT w_alv_transf-rem_vbeln IS INITIAL.
      w_alv_transf-ebeln_ic = w_alv_transf-ped_ebeln. "icon_other_object.  "*-CS2024000522-12.09.2024-JT-#151251-inicio
    ELSEIF w_alv_transf-rem_vbeln IS INITIAL.
      w_alv_transf-ebeln_ic = icon_space.
    ENDIF.

    IF w_alv_transf-doc_custo IS INITIAL.
      w_alv_transf-doc_custo   = icon_icon_list.
    ENDIF.

    IF w_alv_transf-ov_serv   IS INITIAL.
      w_alv_transf-ov_serv     = icon_icon_list.
    ENDIF.

    IF w_alv_transf-fat_serv IS INITIAL.
      w_alv_transf-fat_serv    = icon_icon_list.
    ENDIF.

    IF w_alv_transf-dacte IS INITIAL.
      w_alv_transf-dacte       = icon_icon_list.
    ENDIF.


    CLEAR: t_style[], w_style.
    w_style-fieldname  = 'REM_VGBEL'.
    w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_style    INTO TABLE t_style.

    CLEAR: w_style.
    w_style-fieldname  = 'REM_VGPOS'.
    w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_style    INTO TABLE t_style.

    IF l_lock_ag_frete = abap_true.
      CLEAR: w_style.
      w_style-fieldname  = 'AG_FRETE'.
      w_style-style      = cl_gui_alv_grid=>mc_style_disabled.
      INSERT w_style    INTO TABLE t_style.
    ELSE.
      CLEAR: w_style.
      w_style-fieldname  = 'AG_FRETE'.
      w_style-style      = cl_gui_alv_grid=>mc_style_enabled.
      INSERT w_style    INTO TABLE t_style.
    ENDIF.

    w_alv_transf-cellstyles[] = t_style[].

    APPEND w_alv_transf TO t_alv_transf.
    CLEAR:  w_alv_transf, lva_chv_nfe, w_lips.

  ENDLOOP.

  SORT t_alv_transf BY rem_vbeln.
  DELETE ADJACENT DUPLICATES FROM t_alv_transf COMPARING rem_vbeln.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM f_handle_toolbar USING i_object TYPE REF TO cl_alv_event_toolbar_set .
  DATA: ls_toolbar TYPE stb_button.

* US - 92464 - Inicio - CBRAND
  IF p_nfprop = 'X'.

    CLEAR ls_toolbar.
    MOVE 'GER_REM' TO ls_toolbar-function.                  "#EC NOTEXT
    MOVE 0 TO ls_toolbar-butn_type.
    "MOVE icon_calculation TO ls_toolbar-icon.
    MOVE 'Gerar Remessa'  TO ls_toolbar-quickinfo.
    MOVE 'Gerar Remessa' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.                        "#EC NOTEXT
    APPEND ls_toolbar TO i_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'EST_REM' TO ls_toolbar-function.                  "#EC NOTEXT
    MOVE 0 TO ls_toolbar-butn_type.
    "MOVE icon_calculation TO ls_toolbar-icon.
    MOVE 'Estornar Remessa'  TO ls_toolbar-quickinfo.
    MOVE 'Estornar Remessa' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.                        "#EC NOTEXT
    APPEND ls_toolbar TO i_object->mt_toolbar.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ZTRANSF
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM f_ztransf USING  p_index  TYPE lvc_s_row-index.

  DATA: v_due_date   TYPE ledat,
        v_deliv_numb TYPE bapishpdelivnumb-deliv_numb,
        v_erro       TYPE c LENGTH 1.

  CLEAR: t_retorno, t_items.

  IF w_alv_transf-rem_vgbel IS INITIAL OR  w_alv_transf-rem_vgpos IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-143 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.

    SELECT SINGLE *
      FROM ekko INTO @DATA(wl_ekko)
     WHERE ebeln = @w_alv_transf-rem_vgbel
       AND bsart = 'ZUB'.
    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH TEXT-143 DISPLAY LIKE 'E'.
      EXIT.
    ELSE.

      SELECT SINGLE *
        FROM ekpo INTO @DATA(wl_ekpo)
       WHERE ebeln = @w_alv_transf-rem_vgbel
         AND ebelp = @w_alv_transf-rem_vgpos.

      SELECT SINGLE *
         FROM lips INTO @DATA(wl_lips)
        WHERE vgbel = @wl_ekpo-ebeln.

      IF sy-subrc = 0.
        MESSAGE s024(sd) WITH TEXT-144 DISPLAY LIKE 'E'.
        EXIT.
      ELSE.

        CLEAR: t_itens[], w_itens.
        w_itens-ref_doc        = w_alv_transf-rem_vgbel.
        w_itens-ref_item       = w_alv_transf-rem_vgpos.
        w_itens-dlv_qty        = w_alv_transf-ped_menge.
        w_itens-sales_unit     = w_alv_transf-ped_meins.
        w_itens-sales_unit_iso = w_alv_transf-ped_meins .

        APPEND w_itens TO t_itens.

      ENDIF.
    ENDIF.
  ENDIF.
  IF t_itens[] IS NOT INITIAL.

    v_due_date = sy-datum.

    CLEAR: t_retorno[].
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_STO'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        due_date          = v_due_date
      IMPORTING
        delivery          = v_deliv_numb
      TABLES
        stock_trans_items = t_itens
        return            = t_retorno
        deliveries        = t_items.

    READ TABLE t_retorno WITH KEY type = 'S'.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "Forçar a liberação do documento gerado
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.

      CLEAR:  t_retorno2[], header_partner[], item_data[], item_control[], item_data_spl[].


* parceiro local de coleta
      SELECT SINGLE lifn2
          FROM ekpa INTO w_alv_transf-ped_coleta
            WHERE ebeln = w_alv_transf-rem_vgbel
              AND parvw  = 'PR'.


      w_alv_transf-rem_vbeln =  v_deliv_numb.

      w_header_data-deliv_numb     = v_deliv_numb.
      w_header_control-deliv_numb  = 'X'.

      header_partner-upd_mode_partn = 'I'.
      header_partner-deliv_numb     = v_deliv_numb.
      header_partner-itm_number     = '000010'.
      header_partner-partn_role     = 'PC'.
      header_partner-partner_no     = w_alv_transf-ped_coleta.
      APPEND header_partner.

      header_partner-upd_mode_partn = 'I'.
      header_partner-deliv_numb     = v_deliv_numb.
      header_partner-itm_number     = '000010'.
      header_partner-partn_role     = 'LR'.
      header_partner-partner_no     = w_alv_transf-ped_entrega.
      APPEND header_partner.

      item_data-deliv_numb          = v_deliv_numb.
      item_data-deliv_item          = '000010'.
      item_data-batch               = w_alv_transf-ped_charg.
      item_data-dlv_qty             = w_alv_transf-ped_menge.
      item_data-dlv_qty_imunit      = w_alv_transf-ped_menge.
      item_data-fact_unit_nom       = 1.
      item_data-fact_unit_denom     = 1.
      item_data-gross_wt            = w_alv_transf-ped_menge.
      item_data-net_weight          = w_alv_transf-ped_menge.
      APPEND item_data.

      item_control-deliv_numb       = v_deliv_numb.
      item_control-deliv_item       = '000010'.
      item_control-chg_delqty       = 'X'.
      item_control-volume_flg       = 'X'.
      item_control-net_wt_flg       = 'X'.
      item_control-gross_wt_flg     = 'X'.
      APPEND item_control.

      item_data_spl-deliv_numb      = v_deliv_numb.
      item_data_spl-deliv_item      = '000010'.
      item_data_spl-stge_loc        = w_alv_transf-ped_lgort.
      APPEND item_data_spl.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = w_header_data
          header_control = w_header_control
          delivery       = v_deliv_numb
        TABLES
          header_partner = header_partner
          item_data      = item_data
          item_control   = item_control
          return         = t_retorno2
          item_data_spl  = item_data_spl.

      nlinhas = 0.
      DESCRIBE TABLE t_retorno2 LINES nlinhas.

      IF nlinhas IS INITIAL.
        CLEAR: tl_vbpok,sl_vbpok.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        sl_vbkok_wa-vbeln_vl    = v_deliv_numb.
        sl_vbkok_wa-vbeln       = v_deliv_numb.
        sl_vbkok_wa-wabuc       = 'X'.
        sl_vbkok_wa-wadat_ist   = sy-datum. "data atual .
        sl_vbpok-vbeln_vl       = v_deliv_numb.
        sl_vbpok-posnr_vl       = '000010'.
        sl_vbpok-vbeln          = v_deliv_numb.
        sl_vbpok-posnn          = '000010'.

        sl_vbpok-matnr          =  w_alv_transf-rem_matnr.
        sl_vbpok-pikmg          =  w_alv_transf-ped_menge.
        sl_vbpok-charg          =  w_alv_transf-ped_charg.
        sl_vbpok-lgort          =  w_alv_transf-ped_lgort.

        sl_vbpok-brgew          = w_alv_transf-ped_menge.
        sl_vbpok-ntgew          = w_alv_transf-ped_menge.
        sl_vbpok-gewei          = 'KG'.
        sl_vbpok-vbtyp_n        = 'V'.
        APPEND sl_vbpok TO tl_vbpok.

        CLEAR: tl_prot.
        CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
          EXPORTING
            vbkok_wa                 = sl_vbkok_wa
            synchron                 = 'X'
            if_error_messages_send_1 = 'X'
          TABLES
            vbpok_tab                = tl_vbpok
            prot                     = tl_prot.

        IF NOT tl_prot[] IS INITIAL.
          v_erro = 'X'.
        ENDIF.

        IF v_erro IS INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          "Forçar a liberação do documento gerado
          CALL FUNCTION 'DEQUEUE_ALL'
            EXPORTING
              _synchron = 'X'.

          MODIFY t_alv_transf FROM w_alv_transf INDEX p_index.

          LOOP AT s_ebeln.
            IF ( s_ebeln-low NE w_alv_transf-rem_vgbel ) AND ( s_ebeln-low IS NOT INITIAL ) .
              APPEND VALUE #( sign = 'I' option = 'EQ' low =  w_alv_transf-rem_vgbel )   TO s_ebeln.
            ENDIF.
          ENDLOOP.

          PERFORM f_selecao_dados_nf_propria.
          PERFORM f_processa_dados.

          REFRESH s_ebeln.


        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          PERFORM f_apaga_delivery USING v_deliv_numb.

          CLEAR:  t_return.
          LOOP AT tl_prot INTO sl_prot.
*            w_log_erros-type     = sl_prot-msgty.
*            w_log_erros-number   = 'PIK'.
*
*            MESSAGE ID sl_prot-msgid
*                      TYPE sl_prot-msgty
*                    NUMBER sl_prot-msgno
*                      WITH sl_prot-msgv1
*                           sl_prot-msgv2
*                           sl_prot-msgv3
*                           sl_prot-msgv4
*                      INTO w_log_erros-mensagem.
*            APPEND w_log_erros TO t_log_warni.


            w_return-type    = sl_prot-msgty.
            w_return-id      = sl_prot-msgid.
            w_return-number  = sl_prot-msgno.
            w_return-message_v1 = sl_prot-msgv1.
            w_return-message_v2 = sl_prot-msgv2.
            w_return-message_v3 = sl_prot-msgv3.
            w_return-message_v4 = sl_prot-msgv4.

            APPEND  w_return TO t_return.
            CLEAR: w_return .

          ENDLOOP.
          IF t_return[] IS NOT INITIAL.

            w_alv_transf-rem_vbeln = ''.
            MODIFY t_alv_transf FROM w_alv_transf INDEX p_index.

            zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                      i_ebeln      = w_alv_transf-rem_vgbel
                                                                                      i_ebelp      = w_alv_transf-rem_vgpos
                                                                                      i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                      i_commit     = abap_true
                                                                            CHANGING  t_return     = t_return[] ).

            MESSAGE s024(sd) WITH TEXT-148 DISPLAY LIKE 'E'.
            EXIT.

          ENDIF.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                  i_ebeln      = w_alv_transf-rem_vgbel
                                                                                  i_ebelp      = w_alv_transf-rem_vgpos
                                                                                  i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = t_retorno[] ).
        PERFORM f_apaga_delivery USING v_deliv_numb.
        MESSAGE s024(sd) WITH TEXT-148 DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = w_alv_transf-rem_vbeln
                                                                                i_ebeln      = w_alv_transf-rem_vgbel
                                                                                i_ebelp      = w_alv_transf-rem_vgpos
                                                                                i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                i_commit     = abap_true
                                                                      CHANGING  t_return     = t_retorno[] ).
      MESSAGE s024(sd) WITH TEXT-148 DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APAGA_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_DELIV_NUMB  text
*----------------------------------------------------------------------*
FORM f_apaga_delivery  USING    p_v_deliv_numb  TYPE bapishpdelivnumb-deliv_numb.

  sl_hdata-deliv_numb = p_v_deliv_numb.
  sl_hcont-deliv_numb = p_v_deliv_numb.
  sl_hcont-dlv_del    = 'X'.
  vl_delivery         = p_v_deliv_numb.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    TABLES
      return         = tl_bapiret2.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " APAGA_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  F_ESTORNA_REMESSA
*&---------------------------------------------------------------------*
FORM f_estorna_remessa  USING    w_alv_transf TYPE  ty_alv_transf
                        CHANGING p_estorno.

  DATA: wl_erro(1).

  FREE: p_estorno.

  SELECT SINGLE tcode
    FROM likp
    INTO @DATA(l_tcode)
   WHERE vbeln   = @w_alv_transf-rem_vbeln.

  IF l_tcode = 'ZLES0200' OR
     l_tcode = 'ZLES0136'.   "*-CS2024000522-11.09.2024-JT-#151751
    zcl_frete_remessa_trans=>zif_frete_remessa_trans~get_status_outros_docs( EXPORTING i_rem_vbeln   = w_alv_transf-rem_vbeln
                                                                                       i_ebeln       = w_alv_transf-rem_vgbel
                                                                                       i_ebelp       = w_alv_transf-rem_vgpos
                                                                             IMPORTING e_doc_custo   = w_alv_transf-doc_custo
                                                                                       e_ordem_serv  = w_alv_transf-ov_serv
                                                                                       e_fatura_serv = w_alv_transf-fat_serv
                                                                                       e_dacte       = w_alv_transf-dacte ).

    IF w_alv_transf-doc_custo  = icon_icon_list AND
       w_alv_transf-ov_serv    = icon_icon_list AND
       w_alv_transf-fat_serv   = icon_icon_list AND
       w_alv_transf-dacte      = icon_icon_list AND
       w_alv_transf-transp     = icon_execute_object AND
       w_alv_transf-rem_vbeln  IS NOT INITIAL.

      "Estornar Picking
      PERFORM f_estorno_picking_rem USING  w_alv_transf-rem_vbeln
                                 CHANGING wl_erro.
      CHECK wl_erro IS INITIAL.

      "Estornar Remessa
      PERFORM f_estorno_remessa USING w_alv_transf-rem_vbeln
                             CHANGING wl_erro.


      IF wl_erro IS INITIAL.
        p_estorno = abap_true.
*       MESSAGE 'Estorno Realizado com Sucesso!' TYPE 'I'.  "*-CS2024000522-18.07.2024-JT-#143588-inicio
        LOOP AT s_ebeln.
          IF ( s_ebeln-low NE w_alv_transf-rem_vgbel ) AND ( s_ebeln-low IS NOT INITIAL ) .
            APPEND VALUE #( sign = 'I' option = 'EQ' low =  w_alv_transf-rem_vgbel )  TO s_ebeln.
          ENDIF.
        ENDLOOP.

*-CS2024000522-18.07.2024-JT-#143588-inicio
*        PERFORM f_selecao_dados_nf_propria.
*        PERFORM f_processa_dados.
*-CS2024000522-18.07.2024-JT-#143588-fim

*       REFRESH s_ebeln.   "*-CS2024000522-18.07.2024-JT-#143588-inicio comentado
        EXIT.
      ELSE.
        MESSAGE 'Estorno com erros!' TYPE 'I'.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE 'Existem Documentos CTe sem estorno!' TYPE 'I'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Remessa sem origem ZLES0200!' TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_PICKING_REM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ALV_TRANSF_REM_VBELN  text
*      <--P_WL_ERRO  text
*      <--P_P_SAIDA  text
*----------------------------------------------------------------------*
FORM f_estorno_picking_rem USING p_vbeln TYPE likp-vbeln
                        CHANGING p_erro.


  DATA: vl_mjahr TYPE vbfa-mjahr,
        vl_vbeln TYPE vbfa-vbeln.

  DATA: fp_budat TYPE sy-datlo,
        fp_tcode TYPE sy-tcode   VALUE 'VL09',
        fp_vbtyp TYPE likp-vbtyp VALUE 'J'.

*  DATA: it_mesg     TYPE STANDARD TABLE OF mesg,
*        tl_bapiret2 TYPE bapiret2_t.

  "Verifica se picking já não foi estornado.
  SELECT SINGLE vbeln mjahr
    FROM vbfa INTO (vl_vbeln,vl_mjahr)
   WHERE vbelv = p_vbeln
     AND vbtyp_n  = 'h'
     AND vbtyp_v  = 'J'.

  CHECK sy-subrc NE 0. "Prosseguir se não encontrou estorno mov. mercadoria

  SELECT SINGLE *
    FROM likp INTO @DATA(_wl_likp)
    WHERE vbeln EQ @p_vbeln.

  CHECK ( sy-subrc EQ 0 ) AND ( _wl_likp-wadat_ist IS NOT INITIAL ). "Existe Picking.

  fp_budat = sy-datlo.

  CALL FUNCTION 'ZMM_REVERSE_GOODS'
    STARTING NEW TASK p_vbeln
    PERFORMING receive_results_reverse ON END OF TASK
    EXPORTING
      vbeln  = p_vbeln
      budat  = fp_budat
      tcode  = fp_tcode
      vbtyp  = fp_vbtyp
    TABLES
      t_mesg = it_mesg.

  WAIT UNTIL results_received = 'X' UP TO 60 SECONDS.
  IF error_reverse_goods_issue NE 0.
    p_erro = 'X'.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

*  CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
*    EXPORTING
*      i_vbeln                   = p_vbeln
*      i_budat                   = fp_budat
*      i_tcode                   = fp_tcode
*      i_vbtyp                   = fp_vbtyp
*    TABLES
*      t_mesg                    = it_mesg
*    EXCEPTIONS
*      error_reverse_goods_issue = 1
*      OTHERS                    = 2.
*
*  IF sy-subrc NE 0.
*    p_erro = 'X'.
*    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    EXIT.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
*
*  WAIT UP TO 3 SECONDS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_REMESSA
*&---------------------------------------------------------------------*
FORM f_estorno_remessa USING p_vbeln TYPE likp-vbeln
                    CHANGING p_erro.

  DATA: it_mesg     TYPE STANDARD TABLE OF mesg,
        sl_hdata    TYPE bapiobdlvhdrchg,
        sl_hcont    TYPE bapiobdlvhdrctrlchg,
        tl_bapiret2 TYPE bapiret2_t.

  CLEAR: p_erro, it_mesg[].

  IF ( p_vbeln  IS INITIAL             ) OR
     ( p_vbeln  EQ icon_execute_object ) OR
     ( p_vbeln  EQ icon_icon_list      ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número da remessa não atribuído!'.
    EXIT.
  ENDIF.

  "Deleta Delivery Criado
  sl_hdata-deliv_numb = p_vbeln.
  sl_hcont-deliv_numb = p_vbeln.
  vl_delivery = p_vbeln.
  sl_hcont-dlv_del    = 'X'.

  CLEAR: tl_bapiret2[].

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
    EXPORTING
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    TABLES
      return         = tl_bapiret2.

  IF sy-subrc NE 0.
    p_erro = 'X'.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  LOOP AT tl_bapiret2 INTO DATA(wl_bapiret) WHERE type = 'E'.
    p_erro = 'X'.
    MESSAGE ID wl_bapiret-id TYPE 'I' NUMBER wl_bapiret-number WITH wl_bapiret-message_v1 wl_bapiret-message_v2 wl_bapiret-message_v3 wl_bapiret-message_v4.
    RETURN.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_refresh .
  CLEAR:  t_ekko,
          t_lips,
          t_likp,
          t_ekbe_aux,
          t_ekpa,
          t_ekpo,
          t_ekpv,
          t_eket,
          t_ekbe,
          t_j_1bnflin,
          t_j_1bnfdoc,
          t_j_1bnfstx,
          t_j_1bnfe_active,
          t_dados_remessa,  "*-CS2024000522-29.08.2024-JT-#150113-inicio
          t_vttp,
          t_vttk,
          t_vfkp.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form receive_results_reverse
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_MESG
*&      --> =
*&      --> IT_MESG
*&---------------------------------------------------------------------*
FORM receive_results_reverse  USING i_taskname.

  RECEIVE RESULTS FROM FUNCTION 'ZMM_REVERSE_GOODS'
    IMPORTING
      error_reverse_goods_issue = error_reverse_goods_issue
    TABLES
      t_mesg              = it_mesg.

  results_received = abap_true.
ENDFORM.
