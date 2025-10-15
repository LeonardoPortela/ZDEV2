*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  CHECK p_pend IS NOT INITIAL OR
        p_conc IS NOT INITIAL.

  SELECT zsdt0302~nro_cgd
         zsdt0302~ch_referencia
         zsdt0302~gera_solicitacao_ra
         zsdt0302~qtd_solicitacao_ra
         zsdt0001~nr_romaneio
         zsdt0001~branch
    INTO TABLE t_zsdt0302
    FROM zsdt0302
    INNER JOIN zsdt0001 ON zsdt0001~ch_referencia = zsdt0302~ch_referencia
   WHERE zsdt0001~bukrs               = p_bukrs
     AND zsdt0001~branch             IN s_branch
     AND zsdt0001~nr_safra           IN s_safra
     AND zsdt0001~nro_cg             IN s_nr_cg
     AND zsdt0001~nr_romaneio        IN s_nr_rom
     AND zsdt0302~gera_solicitacao_ra = abap_true.

  CHECK t_zsdt0302[] IS NOT INITIAL.

  SELECT nro_cgd
         ch_referencia
         id
         receitakey
         data_atual
         tipo_assinatura
    INTO TABLE t_zsdt0298
    FROM zsdt0298
     FOR ALL ENTRIES    IN t_zsdt0302
   WHERE nro_cgd         = t_zsdt0302-nro_cgd
     AND ch_referencia   = t_zsdt0302-ch_referencia
     AND cancelado       = abap_off.
*     AND data_atual     IN s_dt_sol.

  IF t_zsdt0298[] IS NOT INITIAL.
    SELECT numeroreceita
           numeropedido
           cpfrt
           receitakey
           dataemissao
           tipo_assinatura
           chave_workflow
           chave_pdf_assinado
      INTO TABLE t_zsdt0218
      FROM zsdt0218
       FOR ALL ENTRIES IN t_zsdt0298
     WHERE receitakey = t_zsdt0298-receitakey
       AND cancelada  = abap_off.

*    SELECT id_referencia
*           id_processo
*           etapa
*           chave_coleta
*           log_date
*      INTO TABLE t_assina01
*      FROM zint_assina01
*       FOR ALL ENTRIES IN t_zsdt0298
*     WHERE id_referencia = t_zsdt0298-receitakey
*       AND id_processo   = '02'.
  ENDIF.

  SELECT zsdt0139~nro_cgd
         zsdt0139~cpf_rtc
         zsdt0259~nome
         zsdt0259~ass_eletronica
    INTO TABLE t_zsdt0139
    FROM zsdt0139
   INNER JOIN zsdt0259 ON zsdt0259~cpf  = zsdt0139~cpf_rtc
     FOR ALL ENTRIES      IN t_zsdt0302
   WHERE zsdt0139~nro_cgd  = t_zsdt0302-nro_cgd.

  SORT t_zsdt0298 BY nro_cgd ch_referencia.

  SORT t_zsdt0218 BY receitakey.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0218
                        COMPARING receitakey.

  SORT t_assina01 BY id_referencia.
  DELETE ADJACENT DUPLICATES FROM t_assina01
                        COMPARING id_referencia.

  SORT t_zsdt0139 BY nro_cgd.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0139
                        COMPARING nro_cgd.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  t_0298[] = t_zsdt0298[].

  FREE: t_alv.

*-----------------------------------------------
* etapas do processo
*-----------------------------------------------
* E1 - Pendente de Solicitacao de RA
* E2 - Pendente de Gerar Receituario
* E3 - Pendente de ENVIO para Assinatura Eletrônica
* E4 - Pendente de Assinatura Eletrônica
* S1 - Concluido de Solicitacao de RA
* S2 - Concluido Gerar Receituário
* S3 - Concluido para assinatura
* S4 - Concluido o ENVIO para Assinatura
* S5 - Concluido a Assinatura Eletrônica
*-----------------------------------------------

*-----------------------------------------------
* monta tabela de mensagens
*-----------------------------------------------
  t_tab_err = VALUE #( ( etapa = 'E1' mensagem = 'Pendente de Solicitacao de RA'                )
                       ( etapa = 'E2' mensagem = 'Pendente de Gerar Receituario'                )
                       ( etapa = 'E3' mensagem = 'Pendente de ENVIO para Assinatura Eletrônica' )
                       ( etapa = 'E4' mensagem = 'Pendente de Assinatura Eletrônica'            )
                       ( etapa = 'S1' mensagem = 'Concluido de Solicitacao de RA'               )
                       ( etapa = 'S2' mensagem = 'Concluido Gerar Receituário'                  )
                       ( etapa = 'S3' mensagem = 'Concluido para Assinatura'                    )
                       ( etapa = 'S4' mensagem = 'Concluido o ENVIO para Assinatura'            )
                       ( etapa = 'S5' mensagem = 'Concluido a Assinatura Eletrônica'            ) ).

*-----------------------------------------------
* monta saida alv
*-----------------------------------------------
  LOOP AT t_zsdt0302 INTO w_zsdt0302.

    CLEAR: w_zsdt0298, l_qtd.

    LOOP AT t_0298 INTO w_0298 WHERE nro_cgd       = w_zsdt0302-nro_cgd
                                 AND ch_referencia = w_zsdt0302-ch_referencia.
      l_qtd = l_qtd + 1.
    ENDLOOP.

    LOOP AT t_zsdt0298 INTO w_zsdt0298 WHERE nro_cgd       = w_zsdt0302-nro_cgd
                                         AND ch_referencia = w_zsdt0302-ch_referencia.
      PERFORM f_linha_alv.
    ENDLOOP.

    IF sy-subrc <> 0.
      PERFORM f_linha_alv.
    ENDIF.

  ENDLOOP.

  IF     p_pend = abap_true  AND p_conc = abap_false.
    DELETE t_alv WHERE etapa(1) = 'S'.
  ELSEIF p_pend = abap_false AND p_conc = abap_true.
    DELETE t_alv WHERE etapa(1) = 'E'.
  ENDIF.

ENDFORM.

**********************************************************************
* append saida
**********************************************************************
FORM f_linha_alv.

  CLEAR: w_alv, w_zsdt0218, w_zsdt0139, w_assina01.

  READ TABLE t_zsdt0218 INTO w_zsdt0218 WITH KEY receitakey    = w_zsdt0298-receitakey
                                        BINARY SEARCH.
  READ TABLE t_zsdt0139 INTO w_zsdt0139 WITH KEY nro_cgd       = w_zsdt0302-nro_cgd
                                        BINARY SEARCH.
  READ TABLE t_assina01 INTO w_assina01 WITH KEY id_referencia = w_zsdt0298-receitakey
                                        BINARY SEARCH.

  w_alv-nro_cgd            = w_zsdt0302-nro_cgd.
  w_alv-ch_referencia      = w_zsdt0302-ch_referencia.
  w_alv-nr_romaneio        = w_zsdt0302-nr_romaneio.
  w_alv-qtd_solicitacao_ra = w_zsdt0302-qtd_solicitacao_ra.
  w_alv-id                 = w_zsdt0298-id.
  w_alv-data_atual         = w_zsdt0298-data_atual.
  w_alv-nome               = w_zsdt0139-nome.
  w_alv-receitakey         = w_zsdt0298-receitakey.
  w_alv-numeroreceita      = w_zsdt0218-numeroreceita.
  w_alv-branch             = w_zsdt0302-branch.
  w_alv-dataemissao        = w_zsdt0218-dataemissao.
  w_alv-tipo_assinatura    = w_zsdt0298-tipo_assinatura.

  IF w_alv-tipo_assinatura IS INITIAL.
*    IF w_zsdt0139-ass_eletronica = abap_true.
*      w_alv-tipo_assinatura = '1'.
*    ELSE.
*      w_alv-tipo_assinatura = '0'.
*    ENDIF.
    w_alv-desc_assinatura = 'Não Identificada'.
  ELSE.
      w_alv-desc_assinatura    = COND #( WHEN w_alv-tipo_assinatura = '0' THEN 'Manual'
                                                                      ELSE 'Eletrônica' ).
  ENDIF.

  w_alv-chave_coleta       = w_assina01-chave_coleta.
  w_alv-log_date           = w_assina01-log_date.

*---------------------------
*---- regras para cada estagio da carga
*---------------------------
  IF w_zsdt0302-qtd_solicitacao_ra <> l_qtd OR
     w_zsdt0298 IS INITIAL.
    w_alv-etapa  = 'E1'.
    PERFORM f_append USING w_alv-etapa.
    EXIT.
  ELSE.
    w_alv-etapa  = 'S1'.
*   PERFORM f_append USING w_alv-etapa.
  ENDIF.

  IF w_zsdt0218 IS NOT INITIAL.
    w_alv-etapa  = 'S2'.
*   PERFORM f_append USING w_alv-etapa.
  ELSE.
    w_alv-etapa  = 'E2'.
    PERFORM f_append USING w_alv-etapa.
    EXIT.
  ENDIF.

  IF w_zsdt0218-tipo_assinatura = '0'.
    w_alv-etapa  = 'S3'.
    PERFORM f_append USING w_alv-etapa.
  ELSE.
*** Com a mudança da Assinatura dentro do portal da Agriq essa etapa não tem mai necessidade de checagem
*    IF w_zsdt0218-chave_workflow = abap_off.
*      w_alv-etapa = 'E3'.
*      PERFORM f_append USING w_alv-etapa.
*      EXIT.
*    ELSE.
*      w_alv-etapa = 'S4'.
*     PERFORM f_append USING w_alv-etapa.
*    ENDIF.

    IF w_zsdt0218-chave_pdf_assinado = abap_off.
      w_alv-etapa = 'E4'.
      PERFORM f_append USING w_alv-etapa.
      EXIT.
    ELSE.
      w_alv-etapa = 'S5'.
      PERFORM f_append USING w_alv-etapa.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* append saida
**********************************************************************
FORM f_append USING p_tipo.

  DATA: l_color TYPE i.

  FREE w_alv-cellcolor.

  l_color = COND #( WHEN p_tipo(1) = 'E' THEN 6
                                         ELSE 5 ).
  t_color = VALUE #( ( fname = 'MENSAGEM' color-col = l_color ) ).

  w_alv-etapa_head = w_alv-etapa.
  w_alv-mensagem   = t_tab_err[ etapa = p_tipo ]-mensagem.
  w_alv-cellcolor  = t_color[].
  w_alv-status     = COND #( WHEN p_tipo(1) = 'E' THEN icon_red_light
                                                  ELSE icon_green_light ).
  APPEND w_alv   TO t_alv.

ENDFORM.

**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* gerar solicitacao RA
**********************************************************************
FORM f_gerar_solicitacao.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF lines( t_rows[] ) <> 1.
    MESSAGE s024(sd) WITH 'Selecione uma linha para gerar Solicitação!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  w_rows = t_rows[ 1 ].
  w_alv  = t_alv[ w_rows-index ].

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Gerando Receituário Agronômico...'.

*----------------------------------
* gerar solicitacao receita
*----------------------------------
  TRY.
      zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
         )->set_gerar_sol_ra( EXPORTING i_nro_cgd       = w_alv-nro_cgd
                                        i_ch_referencia = w_alv-ch_referencia
                                        i_exibe_popup   = abap_true ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      l_erro = abap_true.
      ex_integra->zif_error~published_erro( i_msgty = 'S' ).
    CATCH zcx_error      INTO DATA(ex_error).
      l_erro = abap_true.
      ex_error->zif_error~published_erro(   i_msgty = 'S' ).
  ENDTRY.

  IF l_erro = abap_false.
    MESSAGE s024(sd) WITH 'Receituário Agronômico gerado com Sucesso!'.
  ENDIF.

ENDFORM.

**********************************************************************
* cancelar solicitacao RA
**********************************************************************
FORM f_cancelar_solicitacao.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF lines( t_rows[] ) <> 1.
    MESSAGE s024(sd) WITH 'Selecione uma linha para cancelar Solicitação!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  w_rows = t_rows[ 1 ].
  w_alv  = t_alv[ w_rows-index ].

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Não será possível desfazer esta ação. Deseja prosseguir?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_resp
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_resp = '1'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Cancelando Receituário Agronômico...'.

*----------------------------------
* cancelar solicitacao receita
*----------------------------------
  TRY.
      zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
         )->set_cancelar_sol_ra( EXPORTING i_nro_cgd       = w_alv-nro_cgd
                                           i_ch_referencia = w_alv-ch_referencia ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      l_erro = abap_true.
      ex_integra->zif_error~published_erro( i_msgty = 'S' ).
    CATCH zcx_error      INTO DATA(ex_error).
      l_erro = abap_true.
      ex_error->zif_error~published_erro(   i_msgty = 'S' ).
  ENDTRY.

  IF l_erro = abap_false.
    MESSAGE s024(sd) WITH 'Receituário Agronômico cancelado com Sucesso!'.
  ENDIF.

ENDFORM.

**********************************************************************
* envia assinatura bry
**********************************************************************
FORM f_envia_assinatura_bry.

  FREE: t_rows, l_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF lines( t_rows[] ) <> 1.
    MESSAGE s024(sd) WITH 'Selecione uma linha para efetuar assinatura!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  w_rows = t_rows[ 1 ].
  w_alv  = t_alv[ w_rows-index ].

  IF w_alv-tipo_assinatura <> '1'.  "Assinatura eletronica.
    MESSAGE s024(sd) WITH 'Assinatura deste RTC não é Eletrônica!'.
    EXIT.
  ENDIF.

  IF w_alv-chave_coleta <> abap_off.
    MESSAGE s024(sd) WITH 'Receita já possui coleta para assinatura eletrônica!'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Enviando para assinatura Bry...'.

*----------------------------
*-efetua assinatura digital BRY
*----------------------------
  TRY .
      zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
         )->set_enviar_assinatura( EXPORTING i_receitakey  = w_alv-receitakey ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      l_erro = abap_true.
      ex_integra->zif_error~published_erro( i_msgty = 'S' ).
    CATCH zcx_error INTO DATA(ex_error).    "  "
      l_erro = abap_true.
      ex_error->zif_error~published_erro(   i_msgty = 'S' ).
  ENDTRY.

  IF l_erro = abap_false.
    MESSAGE s024(sd) WITH 'Assinatura enviada a Bry com Sucesso!'.
  ENDIF.

ENDFORM.

**********************************************************************
* imprime RA
**********************************************************************
FORM f_imprimir_ra_assinada.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF lines( t_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para Imprimir R.A. assinada' TYPE 'I'.
    EXIT.
  ENDIF.

  w_rows = t_rows[ 1 ].
  w_alv  = t_alv[ w_rows-index ].

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = |Verificando documentos. Aguarde...|.

*----------------------------------
* Imprimir RA
*----------------------------------
  CALL FUNCTION 'ZSD_EXIBIR_RA_ASSINADA'
    EXPORTING
      i_nro_cg        = w_alv-nro_cgd
      i_ch_referencia = w_alv-ch_referencia
    EXCEPTIONS
      pdf_not_found   = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Receituário Agronômico não pode ser impresso.' TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-zebra        = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
  w_layout-ctab_fname   = 'CELLCOLOR'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

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

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_alv[]
        it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

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

  wl_linha = 'Gerenciamento Receitas Agronômicas'.
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.
  CALL METHOD obj_dyndoc_id->new_line.
  CALL METHOD obj_dyndoc_id->new_line.

  IF     p_pend = abap_true  AND p_conc = abap_false.
    wl_linha = 'Exibindo Receituários Pendentes'.
  ELSEIF p_pend = abap_false AND p_conc = abap_true.
    wl_linha = 'Exibindo Receituários Concluídos'.
  ELSE.
    wl_linha = 'Exibindo Receituários Pendentes e Concluídos'.
  ENDIF.

  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text = wl_text.

  CALL METHOD obj_dyndoc_id->new_line.

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
FORM f_sort.

  FREE: t_sort.

  w_sort-fieldname = 'ETAPA_HEAD'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

  w_sort-fieldname = 'NRO_CGD'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 2.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

  w_sort-fieldname = 'NR_ROMANEIO'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 3.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''      ''       'T_ALV' 'STATUS'              'Status'                   '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    01  ''      ''       'T_ALV' 'ETAPA_HEAD'          ' '                        '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' 'X' ' ' 'X',
    02  ''      ''       'T_ALV' 'NRO_CGD'             'No.Carga'                 '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    03  ''      ''       'T_ALV' 'NR_ROMANEIO'         'No.Romaneio'              '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    04  ''      ''       'T_ALV' 'QTD_SOLICITACAO_RA'  'Qtd.Sol.RA Calc.'         '16'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    05  ''      ''       'T_ALV' 'ID'                  'Id.Solicitacao RA'        '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  ''      ''       'T_ALV' 'DATA_ATUAL'          'Dt.Solic.RA'              '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_ALV' 'NOME'                'Agrônomo'                 '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''      ''       'T_ALV' 'RECEITAKEY'          'ID.Receituário'           '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''      ''       'T_ALV' 'NUMERORECEITA'       'No.Receituário'           '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''      ''       'T_ALV' 'BRANCH'              'Filial Fatur.'            '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  ''      ''       'T_ALV' 'DATAEMISSAO'         'Dt.Receituário'           '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    12  ''      ''       'T_ALV' 'DESC_ASSINATURA'     'Tipo Assinatura'          '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
*    13  ''      ''       'T_ALV' 'CHAVE_COLETA'        'Chave Coleta Assinatura'  '50'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    13  ''      ''       'T_ALV' 'LOG_DATE'            'Dt.Assinatura'            '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    14  ''      ''       'T_ALV' 'MENSAGEM'            'Mensagem'                 '40'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.


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
                           VALUE(p_fix).

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
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
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
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
**********************************************************************
