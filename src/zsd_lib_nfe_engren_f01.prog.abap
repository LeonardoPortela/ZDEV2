*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form top_of_page.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = t_list_top_of_page.

endform.                    "TOP_OF_PAGE
*---------------------------------------------------------------------*
*       FORM F_COMMENT_BUILD                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E04_LT_TOP_OF_PAGE                                            *
*---------------------------------------------------------------------*
form f_comment_build using e04_lt_top_of_page type slis_t_listheader.

  data: ls_line type slis_listheader.

  refresh: t_list_top_of_page[].

  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Empresa:'.
  ls_line-info = p_bukrs.
  append ls_line to e04_lt_top_of_page.

endform.                    "F_COMMENT_BUILD

*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form processar_dados .

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 0
      text       = 'Atualizando...'
    exceptions
      others     = 1.

endform.                   " PROCESSAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  APRESENTAR_RESULTADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apresentar_resultados .


  perform f_comment_build using t_list_top_of_page[].
  perform f_fieldcatalog  using t_fieldcat.
  perform f_layout        using wa_layout.
  perform f_init_alv_events.

  perform f_display.

endform.                    " APRESENTAR_RESULTADOS
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDCAT  text
*----------------------------------------------------------------------*
form f_fieldcatalog  using    p_fieldcat type slis_t_fieldcat_alv.

  field-symbols:
                 <fs_fieldcat> type slis_fieldcat_alv.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_internal_tabname = v_tabname_header
      i_structure_name   = 'ZSDS030'
    changing
      ct_fieldcat        = p_fieldcat[].

  loop at p_fieldcat assigning <fs_fieldcat>.
    case <fs_fieldcat>-fieldname.

      when 'STATCOD'.
        <fs_fieldcat>-seltext_s    = 'Cod. Sefaz'.
        <fs_fieldcat>-seltext_m    = 'Cod. Sefaz'.
        <fs_fieldcat>-seltext_l    = 'Cod. Sefaz'.
        <fs_fieldcat>-reptext_ddic = 'Cod. Sefaz'.
        <fs_fieldcat>-outputlen    = 10.

      when 'ACTSTAT'.
        <fs_fieldcat>-seltext_s    = 'NFE: Status'.
        <fs_fieldcat>-seltext_m    = 'NFE: Status'.
        <fs_fieldcat>-seltext_l    = 'NFE: Status'.
        <fs_fieldcat>-reptext_ddic = 'NFE: Status'.
        <fs_fieldcat>-outputlen    = 11.

      when 'LIGHT_NFE'.
        <fs_fieldcat>-seltext_s    = 'NFE: Farol'.
        <fs_fieldcat>-seltext_m    = 'NFE: Farol'.
        <fs_fieldcat>-seltext_l    = 'NFE: Farol'.
        <fs_fieldcat>-reptext_ddic = 'NFE: Farol'.
        <fs_fieldcat>-outputlen    = 10.

      when 'BATCHID'.
        <fs_fieldcat>-seltext_s    = 'Número do Lote'.
        <fs_fieldcat>-seltext_m    = 'Número do Lote'.
        <fs_fieldcat>-seltext_l    = 'Número do Lote'.
        <fs_fieldcat>-reptext_ddic = 'Número do Lote'.
        <fs_fieldcat>-outputlen    = 14.

      when 'STATCODE'.
        <fs_fieldcat>-seltext_s    = 'Lote: Cod. Sefaz'.
        <fs_fieldcat>-seltext_m    = 'Lote: Cod. Sefaz'.
        <fs_fieldcat>-seltext_l    = 'Lote: Cod. Sefaz'.
        <fs_fieldcat>-reptext_ddic = 'Lote: Cod. Sefaz'.
        <fs_fieldcat>-outputlen    = 16.

      when 'ACTSTAT_B'.
        <fs_fieldcat>-seltext_s    = 'Lote: Status'.
        <fs_fieldcat>-seltext_m    = 'Lote: Status'.
        <fs_fieldcat>-seltext_l    = 'Lote: Status'.
        <fs_fieldcat>-reptext_ddic = 'Lote: Status'.
        <fs_fieldcat>-outputlen    = 12.

      when 'LIGHT_LOT'.
        <fs_fieldcat>-seltext_s    = 'Lote: Farol'.
        <fs_fieldcat>-seltext_m    = 'Lote: Farol'.
        <fs_fieldcat>-seltext_l    = 'Lote: Farol'.
        <fs_fieldcat>-reptext_ddic = 'Lote: Farol'.
        <fs_fieldcat>-outputlen    = 11.

      when 'SEL'.
        <fs_fieldcat>-no_out       = abap_true.

      when 'GUID'.
        <fs_fieldcat>-no_out       = abap_true.

      when 'MSGV'.
        <fs_fieldcat>-seltext_s    = 'Mensagem'.
        <fs_fieldcat>-seltext_m    = 'Mensagem'.
        <fs_fieldcat>-seltext_l    = 'Mensagem'.
        <fs_fieldcat>-reptext_ddic = 'Mensagem'.
        <fs_fieldcat>-outputlen    = 50.

      when others.
    endcase.

  endloop.

endform.                    " F_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_display .

  v_repid =  sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = v_repid
      i_callback_user_command  = v_user_command
      i_callback_pf_status_set = v_pf_status_set
      is_variant               = wa_variant
      i_default                = 'X'
      is_layout                = wa_layout
      it_fieldcat              = t_fieldcat[]
      i_save                   = v_save
      it_events                = t_events[]
      it_event_exit            = t_event_exit[]
    tables
      t_outtab                 = t_alv
    exceptions
      program_error            = 1
      others                   = 2.

endform.                    " F_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LAYOUT  text
*----------------------------------------------------------------------*
form f_layout  using    p_layout  type slis_layout_alv.

* Configurações do ALV
  p_layout-zebra             = 'X'.
  p_layout-box_fieldname     = 'SEL'.

endform.                    " F_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_LOGS
*&---------------------------------------------------------------------*
*       Registra logs iniciais e finais
*----------------------------------------------------------------------*
*      -->P_FASE  Início/Final
*----------------------------------------------------------------------*
form f_trata_logs  using    p_fase type flag.

  if p_fase = abap_true. "Início

* Limpar memória principal de log
    call function 'ZFGL_LOG_FLUSH'.

* Data e hora de inicio de processamento
    call function 'ZFGL_LOG_ADD'
      exporting
        i_msgty = c_msgty_s
        i_msgid = c_msgid_zgl
        i_msgno = c_msgno_005
        i_msgv1 = sy-datum
        i_msgv2 = sy-uzeit
        i_write = abap_true.

  else.

* Número de registros gravados
    call function 'ZFGL_LOG_ADD'
      exporting
        i_msgty = c_msgty_s
        i_msgid = c_msgid_zgl
        i_msgno = c_msgno_008
        i_msgv1 = v_sucesso
        i_write = abap_true.

* Número de registros gravados com sucesso
    call function 'ZFGL_LOG_ADD'
      exporting
        i_msgty = c_msgty_s
        i_msgid = c_msgid_zgl
        i_msgno = c_msgno_015
        i_msgv1 = v_erro
        i_write = abap_true.

* Data e hora de fim de processamento
    call function 'ZFGL_LOG_ADD'
      exporting
        i_msgty = c_msgty_s
        i_msgid = c_msgid_zgl
        i_msgno = c_msgno_006
        i_msgv1 = sy-datum
        i_msgv2 = sy-uzeit
        i_write = abap_true.

* Responsável pelo processamento
    call function 'ZFGL_LOG_ADD'
      exporting
        i_msgty = c_msgty_s
        i_msgid = c_msgid_zgl
        i_msgno = c_msgno_009
        i_msgv1 = sy-uname.

* Salva o log (SLG1)
    call function 'ZFGL_LOG_SAVE'
      exporting
        i_extnumber = sy-tcode
        i_alprog    = sy-repid.

* Persistir modificações em banco de dados
    commit work.

  endif.
endform.                    " F_TRATA_LOGS
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_LOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_display_logs using f_ucomm     type sy-ucomm
                                fw_selfield type slis_selfield.
  data: lc_answer.

  clear: p_erro, v_msgv1.

  case f_ucomm.

      " Reiniciar lote grc
    when 'ZPROC'.
      " quando acessar GRC mandar apenas o lote
      t_alv_lote[] = t_alv[].
      delete t_alv_lote where sel eq abap_false.
      sort t_alv_lote by batchid.
      delete adjacent duplicates from t_alv_lote comparing batchid.
      loop at t_alv_lote into wa_alv where sel eq abap_true.
        v_tabix =  sy-tabix.
        " somente lote com status de lote vermelho
        if wa_alv-light_lot eq g_icons-icon_red_light.
          " Reenvio de Lote GRC
          perform f_reenvio_lote_grc.
        endif.
      endloop.

      " Atualizar ALV
      perform f_atualizar_alv.
      fw_selfield-refresh = abap_true.

      " Atualizar ALV
    when 'REFR'.
      perform f_atualizar_alv.
      fw_selfield-refresh = abap_true.

    when others.

  endcase.

  " Reiniciar lote grc
  if f_ucomm eq 'ZPROC'.
    if v_msgv1 is initial.
      v_msgv1 = text-e04.
    endif.
    message i368(00) with v_msgv1.
  endif.

endform.                    " F_DISPLAY_LOGS
*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_pf_status_set using rt_extab type kkblo_t_extab.     "#EC CALLED

  set pf-status 'ZSTANDARD_FULLSCREEN'.

endform.                    "F_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_pf_status_set_i using rt_extab type kkblo_t_extab.   "#EC CALLED

  data:
    lt_extab type slis_t_extab,
    lw_extab type slis_extab.

  clear: lw_extab.
  lw_extab-fcode = 'ZESTOR'.
  append lw_extab to lt_extab.
  clear: lw_extab.
  set pf-status 'ZSTANDARD_FULLSCREEN' excluding lt_extab   immediately.

endform.                    "F_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  F_SELECAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_selecao_dados .

  perform f_ler_nfe_ecc.
  perform f_ler_grc.

endform.                    " F_SELECAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_iniciar_alv .

  v_save = 'A'.   "The user may save all types of a layout

  clear v_variante.
  v_repid = sy-repid.

  concatenate '/' sy-uname into v_variante-variant.
  v_variante-report = v_repid.

  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save     = v_save
    changing
      cs_variant = v_variante
    exceptions
      not_found  = 2.

endform.                    " F_INICIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  F_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_variant .

  clear v_variante-handle.
  v_variante-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = v_variante
      i_save     = 'A'
    importing
      es_variant = v_variante
    exceptions
      not_found  = 2.

  if sy-subrc eq 2.
    message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " F_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_INIT_ALV_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_init_alv_events .

  data: ls_event type slis_alv_event.

  call function 'REUSE_ALV_EVENTS_GET'
    importing
      et_events = t_events[].

  read table t_events with key name =  slis_ev_top_of_page
                           into ls_event.
  if sy-subrc = 0.
    move slis_ev_top_of_page to ls_event-form.
    append ls_event to t_events.
  endif.

  define add_event_exit.
    clear t_event_exit.
    t_event_exit-ucomm  = &1.
    t_event_exit-before = &2.
    t_event_exit-after  = &3.
    append t_event_exit.
  end-of-definition.


  add_event_exit '&F03' con_on con_off.
  add_event_exit '&F12' con_on con_off.
  add_event_exit '&F15' con_on con_off.

endform.                    " F_INIT_ALV_EVENTS
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_validar .

  authority-check object 'F_BKPF_BUK'
      id 'ACTVT' field '03'
      id 'BUKRS' field p_bukrs.
  if sy-subrc ne 0.
    message e368(00) with 'Sem autorização para' p_bukrs display like 'I'.
    p_erro = abap_true.
    exit.
  endif.

  clear: p_erro.

endform.                    " F_VALIDAR
*&---------------------------------------------------------------------*
*&      Form  F_LER_NFE_ECC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_ler_nfe_ecc .

  data:
       ls_active     type j_1bnfe_active.

  clear: t_active[], wa_active.
  select single docnum bukrs branch model
    from j_1bnfe_active
    into wa_active
  where docnum in s_docnum
    and bukrs  eq p_bukrs.

  if wa_active is initial.
    message s030(j1b_nfe) display like 'I' .
    p_erro = abap_true.
    stop.
  endif.

  clear: ls_active,  v_rfcdest.
  move-corresponding wa_active to ls_active.
  " Retrieve the GRC rfc connection
  perform get_rfc_destination in program j_1bnfe_monitor if found
    using ls_active
    changing v_rfcdest.

  if v_rfcdest is initial.
    message i066(j1b_nfe) with v_rfcdest.
    p_erro = abap_true.
    leave to list-processing.
    stop.
  endif.

  " Status engrenagem NFE grupos erro (E), sucesso (S), ou alerta (W).
  refresh: t_status.
  select *
    from ztsd_stats_nfe
    into table t_status.
  sort t_status by tipo statcode stat_acao.

endform.                    " F_LER_NFE_ECC
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_NFE_LIBERADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_consulta_nfe_liberada .

  data: fu_to_excl like vimexclfun occurs 0 with header line.

* Table needed to open the view in Read-Only mode
  refresh fu_to_excl.
  append 'AEND' to fu_to_excl.

  call function 'VIEW_MAINTENANCE_CALL'
    exporting
      action         = 'S'
      view_name      = 'ZTMM_STATS_NFE'
    tables
      excl_cua_funct = fu_to_excl
    exceptions
      others         = 1.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_LER_GRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_ler_grc .

  check v_rfcdest is not initial.

  clear: t_return[], t_alv[].
  call function 'ZSD_LIB_NFE_ENGREN'
    destination v_rfcdest
    exporting
      so_docnum    = s_docnum[]
    importing
      t_return     = t_return[]
    exceptions
      no_nfe_found = 1
      others       = 2.
  if sy-subrc ne 0.
* Implement suitable error handling here
  endif.
  sort t_return by docnum.

  loop at t_return into wa_return.
    clear: wa_alv.
    move-corresponding wa_return to wa_alv.
    " Status engrenagem NFE grupos erro (E), sucesso (S), ou alerta (W).

    " NFE
    clear: wa_status.
    read table t_status into wa_status with key tipo = 'N' " NFE
                            statcode = wa_return-actstat  binary search.
    "    if sy-subrc eq 0.
    "    endif.
    case wa_status-stat_acao.
      when 'E'. "	erro
        wa_alv-light_nfe = g_icons-icon_red_light.
      when 'S'. "	sucesso
        wa_alv-light_nfe = g_icons-icon_green_light.
      when 'W'. "	alerta
        wa_alv-light_nfe = g_icons-icon_yellow_light.
      when others. "  alerta
        wa_alv-light_nfe = g_icons-icon_yellow_light.
    endcase.

    " Lote
    clear: wa_status.
    read table t_status into wa_status with key tipo = 'L' " Lote
                            statcode = wa_return-actstat_b  binary search.
    "    if sy-subrc eq 0.
    "    endif.
    case wa_status-stat_acao.
      when 'E'. "	erro
        wa_alv-light_lot = g_icons-icon_red_light.
      when 'S'. "	sucesso
        wa_alv-light_lot = g_icons-icon_green_light.
      when 'W'. "	alerta
        wa_alv-light_lot = g_icons-icon_yellow_light.
      when others. "  alerta
        wa_alv-light_lot = g_icons-icon_yellow_light.
    endcase.

    append  wa_alv to t_alv.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_REENVIO_LOTE_GRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_reenvio_lote_grc .

  check v_rfcdest is not initial.

  clear: v_msgty,v_msgv1.

  call function 'ZSD_REENVIO_LOTE_GRC'
    destination v_rfcdest
    exporting
      iv_guid = wa_alv-guid
    importing
      msgty   = v_msgty
      msgv1   = v_msgv1.

  if sy-subrc ne 0 or v_msgty eq 'E'.
    v_msgv1 = text-e03.
  else.
    v_msgv1 = text-e02.
    "perform f_atualizar_historico_nfe.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_iniciar .

  perform f_validar.

  call function 'ICON_CREATE'
    exporting
      name   = 'ICON_GREEN_LIGHT'
    importing
      result = g_icons-icon_green_light.

  call function 'ICON_CREATE'
    exporting
      name   = 'ICON_YELLOW_LIGHT'
    importing
      result = g_icons-icon_yellow_light.

  call function 'ICON_CREATE'
    exporting
      name   = 'ICON_RED_LIGHT'
    importing
      result = g_icons-icon_red_light.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_HISTORICO_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_atualizar_historico_nfe .

  data:
       lw_alv type zsds030.

  loop at t_alv into lw_alv where batchid eq wa_alv-batchid.

    clear wa_stats_nfe.
    wa_stats_nfe-docnum = lw_alv-docnum.
    wa_stats_nfe-uname  = sy-uname.
    wa_stats_nfe-datum  = sy-datum.
    wa_stats_nfe-uzeit  = sy-uzeit.
    insert ztmm_stats_nfe from wa_stats_nfe.

  endloop.

  if sy-subrc = 0.
    commit work.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_atualizar_alv .

  perform f_selecao_dados.

endform.
