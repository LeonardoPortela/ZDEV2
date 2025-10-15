*----------------------------------------------------------------------*
* Programa..: ZFIR0091                                                 *
* Tipo......: R - Report                                               *
* Transação.: ZFI0130                                                  *
* Descrição.: Compensar Partidas Negociadas com a AL5 Bank             *
* Autor.....: CBRAND                                                   *
* Data......: 21.04.2021                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 21.04.2021   |  |CBRAND     | Codificação Inicial                    *
*----------------------------------------------------------------------*
report zfir0091.

tables: zfit0170.
*---------------------------------------------------------------------*
* Declaração de Types
*---------------------------------------------------------------------*
types:
  begin of ty_saida,
    icon(5)      type c,
    mark(1)      type c,
    bukrs        type bsik-bukrs,
    lifnr        type bsik-lifnr,
    name1        type lfa1-name1,
    ebeln        type bsik-ebeln,
    belnr        type zfit0170-belnr,
    xblnr        type bsak-xblnr,
    budat        type bsak-budat,
    zfbdt        type bsik-zfbdt,
    dmbtr        type bsak-dmbtr,
    dmbe2        type bsak-dmbe2,
    id_oper_al5  type zfit0170-id_oper_al5,
    dt_oper_al5  type zfit0170-dt_oper_al5,
    augbl        type zfit0170-augbl,
    bldat        type bsik-bldat,
    gsber        type bsik-gsber,
    waers        type bsik-waers,
    zuonr        type bsik-zuonr,
    gjahr        type bsik-gjahr,
    buzei        type bsik-buzei,
    blart        type bsik-blart,
    message(100) type c,
  end of ty_saida.

*----------------------------------------------------------------------*
* Declaração de Tabelas
*----------------------------------------------------------------------*
data: git_zfit0170 type table of zfit0170,
      git_bsik     type table of bsik,
      git_bsak     type table of bsak,
      git_lfa1     type table of lfa1,
      git_codal5   type table of setleaf with header line,
      git_saida    type table of ty_saida.

data: begin of git_msg occurs 0.
        include structure bdcmsgcoll.
data: end of git_msg.

*----------------------------------------------------------------------*
* Declaração de Estruturas
*----------------------------------------------------------------------*
data: gwa_zfit0170 like line of  git_zfit0170,
      gwa_bsik     like line of  git_bsik,
      gwa_bsak     like line of  git_bsak,
      gwa_lfa1     like line of  git_lfa1,
      gwa_saida    like line of  git_saida.

data: fcode     type table of sy-ucomm,
      gwa_fcode type sy-ucomm.

data: gwa_ret_document type zfie_ret_document,
      git_ret_document like standard table of gwa_ret_document.

data: git_bdcdata type standard table of bdcdata ,   "Guarda o mapeamento
      gwa_bdcdata like line of git_bdcdata.

data: gva_augbl type zfit0170-augbl,
      gva_line  type sy-tabix.

*----------------------------------------------------------------------*
* Declaração de Ranges
*----------------------------------------------------------------------*
ranges: gra_lifnr for lfa1-lifnr.
*----------------------------------------------------------------------*
* Declaração de Constantes
*----------------------------------------------------------------------*
constants: c_msgid like git_msg-msgid value 'F5',
           c_msgnr like git_msg-msgnr value '312',
           c_msgne like git_msg-msgnr value '539'.


*----------------------------------------------------------------------*
* Parâmetros de seleção
*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.
  select-options: p_bukrs for zfit0170-bukrs  obligatory default '0001'.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
  parameters: p_ab   radiobutton group g1 user-command modifica_tela default 'X',
              p_comp radiobutton group g1.
selection-screen end of block b2.

*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*---------------------------------------------------------------------*
start-of-selection.
  perform fm_seleciona_dados.
  perform fm_manipula_dados.
  perform fm_chama_alv.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_DADOS
*&---------------------------------------------------------------------*
form fm_seleciona_dados .
  if p_ab = 'X'. "Em aberto Compensar

*    SELECT *
*      FROM setleaf INTO TABLE git_codal5
*      WHERE setname EQ 'MAGGI_CODFORAL5'.
*
*    IF git_codal5[] IS NOT INITIAL.
*
*      LOOP AT git_codal5.
*        gra_lifnr-sign = 'I'.
*        gra_lifnr-option = 'EQ'.
*        gra_lifnr-low  =  git_codal5-valfrom.
*        APPEND gra_lifnr.
*      ENDLOOP.
*    ENDIF.

    select *
    into table git_zfit0170
    from zfit0170
      where bukrs in p_bukrs
        and augbl eq ''
        and id_oper_al5 ne ''
        and buzei ne '000'.

    if git_zfit0170 is not initial.

      select *
      into table git_bsik
      from bsik
        for all entries in git_zfit0170
        where bukrs eq git_zfit0170-bukrs
          and gjahr eq git_zfit0170-gjahr
          and belnr eq git_zfit0170-belnr
          and buzei eq git_zfit0170-buzei.
      " AND lifnr IN gra_lifnr.

      select *
      into table git_lfa1
       from lfa1
          for all entries in git_bsik
          where lifnr eq  git_bsik-lifnr.

    else.
      message 'Dados não econtrados!' type 'S'.
      stop.
    endif.

  else. "Compensadas

    select *
      into table git_zfit0170
      from zfit0170
        where bukrs in p_bukrs
          and augbl ne ''
          and id_oper_al5 ne ''
          and buzei ne '000'.

    if git_zfit0170 is not initial.

      select *
      into table git_bsak
      from bsak
        for all entries in git_zfit0170
        where bukrs eq git_zfit0170-bukrs
          and gjahr eq git_zfit0170-gjahr
          and belnr eq git_zfit0170-belnr
          and buzei eq git_zfit0170-buzei.

      check sy-subrc = 0.

      select *
        into table git_lfa1
     from lfa1
        for all entries in git_bsak
        where lifnr eq  git_bsak-lifnr.

    else.
      message 'Dados não econtrados!' type 'S'.
      stop.
    endif.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_MANIPULA_DADOS
*&---------------------------------------------------------------------*
form fm_manipula_dados .
  if p_ab = 'X'. "Em aberto Compensar

    loop at git_zfit0170 into gwa_zfit0170.
      read table git_bsik into gwa_bsik with key  bukrs = gwa_zfit0170-bukrs
                                                  gjahr = gwa_zfit0170-gjahr
                                                  belnr = gwa_zfit0170-belnr
                                                  buzei = gwa_zfit0170-buzei.
      gwa_saida-bukrs       = gwa_zfit0170-bukrs.
      gwa_saida-ebeln       = gwa_bsik-ebeln.
      gwa_saida-belnr       = gwa_zfit0170-belnr.
      gwa_saida-xblnr       = gwa_bsik-xblnr.
      gwa_saida-budat       = gwa_bsik-budat.
      gwa_saida-zfbdt       = gwa_bsik-zbd1t + gwa_bsik-zfbdt.
      gwa_saida-dmbtr       = gwa_bsik-dmbtr.
      gwa_saida-dmbe2       = gwa_bsik-dmbe2.
      gwa_saida-id_oper_al5 = gwa_zfit0170-id_oper_al5.
      gwa_saida-dt_oper_al5 = gwa_zfit0170-dt_oper_al5.
      gwa_saida-augbl       = gwa_zfit0170-augbl .
      gwa_saida-bldat       = gwa_bsik-bldat.
      gwa_saida-gsber       = gwa_bsik-gsber.
      gwa_saida-waers       = gwa_bsik-waers.
      gwa_saida-zuonr       = gwa_bsik-zuonr.
      gwa_saida-gjahr       = gwa_bsik-gjahr.
      gwa_saida-buzei       = gwa_bsik-buzei.
      gwa_saida-blart       = gwa_bsik-blart.

      read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_bsik-lifnr.

      gwa_saida-lifnr = gwa_lfa1-lifnr.
      gwa_saida-name1 = gwa_lfa1-name1.

      if gwa_saida-augbl is initial.
        move icon_generate to gwa_saida-icon.
      endif.

      append gwa_saida to git_saida.
      clear: gwa_saida,
             gwa_bsik,
             gwa_lfa1.

    endloop.

  else.
    loop at git_zfit0170 into gwa_zfit0170.
      read table git_bsak into gwa_bsak with key  bukrs = gwa_zfit0170-bukrs
                                                  gjahr = gwa_zfit0170-gjahr
                                                  belnr = gwa_zfit0170-belnr.
      gwa_saida-bukrs       = gwa_zfit0170-bukrs.
      gwa_saida-ebeln       = gwa_bsak-ebeln.
      gwa_saida-belnr       = gwa_zfit0170-belnr.
      gwa_saida-xblnr       = gwa_bsak-xblnr.
      gwa_saida-budat       = gwa_bsak-budat.
      gwa_saida-zfbdt       = gwa_bsak-zbd1t + gwa_bsak-zfbdt.
      gwa_saida-dmbtr       = gwa_bsak-dmbtr.
      gwa_saida-dmbe2       = gwa_bsak-dmbe2.
      gwa_saida-id_oper_al5 = gwa_zfit0170-id_oper_al5.
      gwa_saida-dt_oper_al5 = gwa_zfit0170-dt_oper_al5.
      gwa_saida-augbl       = gwa_zfit0170-augbl .
      gwa_saida-bldat       = gwa_bsak-bldat.
      gwa_saida-gsber       = gwa_bsak-gsber.
      gwa_saida-waers       = gwa_bsak-waers.
      gwa_saida-zuonr       = gwa_bsak-zuonr.
      gwa_saida-gjahr       = gwa_bsak-gjahr.
      gwa_saida-buzei       = gwa_bsak-buzei.
      gwa_saida-blart       = gwa_bsak-blart.

      read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_bsak-lifnr.
      gwa_saida-lifnr = gwa_lfa1-lifnr.
      gwa_saida-name1 = gwa_lfa1-name1.

      if gwa_saida-augbl is initial.
        move icon_generate to gwa_saida-icon.
      endif.

      append gwa_saida to git_saida.
      clear: gwa_saida,
             gwa_bsak,
             gwa_lfa1.

    endloop.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CHAMA_ALV
*&---------------------------------------------------------------------*
form fm_chama_alv .
  data: lwa_layout   type  slis_layout_alv,
        lwa_print    type  slis_print_alv,
        lwa_keyinfo  type  slis_keyinfo_alv,
        lit_fieldcat type  slis_t_fieldcat_alv,
        lit_exctab   type  slis_t_extab,
        lit_sorttab  type  slis_t_sortinfo_alv,
        lit_events   type  slis_t_event.

  perform alv_init_report_layout tables  lit_fieldcat
                                         lit_exctab
                                         lit_sorttab
                                changing lwa_layout
                                         lwa_print
                                         lwa_keyinfo.

  perform alv_init_report_events tables lit_events.

  lwa_layout-box_fieldname = 'MARK'.
  lwa_layout-box_tabname =  'GIT_SAIDA'.


  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program          = sy-cprog
      i_callback_user_command     = 'USER_COMMAND'
      i_structure_name            = 'GIT_SAIDA'
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_background_id             = ' '
      is_layout                   = lwa_layout
      it_fieldcat                 = lit_fieldcat
      it_excluding                = lit_exctab
      it_sort                     = lit_sorttab
      i_save                      = 'A'
      is_print                    = lwa_print
      i_callback_pf_status_set    = 'SET_STATUS'
    tables
      t_outtab                    = git_saida
    exceptions
      program_error               = 1
      others                      = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  ALV_INIT_REPORT_LAYOUT
*&---------------------------------------------------------------------*
form alv_init_report_layout  tables   ot_fieldcat type slis_t_fieldcat_alv
                                      ot_exctab   type slis_t_extab
                                      ot_sorttab  type slis_t_sortinfo_alv
                             changing os_layout   type slis_layout_alv
                                      os_print    type slis_print_alv
                                      os_keyinfo  type slis_keyinfo_alv.

  refresh: ot_fieldcat,
           ot_exctab,
           ot_sorttab .

  perform alv_build_fieldcat tables ot_fieldcat.

  os_layout-colwidth_optimize = 'X'.
  os_print-no_print_selinfos  = 'X'.
  os_print-no_print_listinfos = 'X'.


endform.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
form user_command using value(iv_ucom) like sy-ucomm
                             is_selfield type slis_selfield.
* ALV Stable
  data: wa_stable        type lvc_s_stbl.

  case iv_ucom.
    when '&IC1'.
      perform display_records using is_selfield.
    when 'BACK'.
      leave screen.
    when 'EXIT'.
      leave screen.
    when 'GER_PARTID'.
      perform fm_gera_partida.

      is_selfield-refresh = 'X'.

  endcase.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
form set_status using t_extab type slis_t_extab.

  clear: fcode , gwa_fcode.
  if p_ab is initial.
    gwa_fcode = 'GER_PARTID'. append gwa_fcode to fcode.
  endif.
  set pf-status 'PF_ZFIR0091_S' excluding fcode.

endform.
*&---------------------------------------------------------------------*
*&      Form  ALV_INIT_REPORT_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form alv_init_report_events  tables  ot_events type slis_t_event.
  clear:  ot_events.
  refresh:  ot_events.
endform.

*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
form alv_build_fieldcat  tables ot_fieldcat.
  data: lwa_fieldcat type slis_fieldcat_alv.

  if p_ab = 'X'.
    clear: lwa_fieldcat.
    lwa_fieldcat-tabname   = 'git_saida'.
    lwa_fieldcat-fieldname = 'ICON'.
    lwa_fieldcat-seltext_m = 'Status'.
    lwa_fieldcat-ddictxt   = 'M'.
    lwa_fieldcat-just      = 'C'.
    append lwa_fieldcat to ot_fieldcat.
  endif.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'LIFNR'.
  lwa_fieldcat-seltext_m     = 'Fornecedor'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSAK'.
  lwa_fieldcat-ref_fieldname = 'LIFNR'.
  lwa_fieldcat-no_zero       = 'X'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'NAME1'.
  lwa_fieldcat-seltext_m     = 'Nome'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'LFA1'.
  lwa_fieldcat-ref_fieldname = 'NAME1'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'EBELN'.
  lwa_fieldcat-seltext_m     = 'Pedido'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-hotspot       = 'X'.
  lwa_fieldcat-ref_tabname   = 'BSIK'.
  lwa_fieldcat-ref_fieldname = 'EBELN'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'BELNR'.
  lwa_fieldcat-seltext_m     = 'Documento'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-hotspot       = 'X'.
  lwa_fieldcat-ref_tabname   = 'BSIK'.
  lwa_fieldcat-ref_fieldname = 'BELNR'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'XBLNR'.
  lwa_fieldcat-seltext_m     = 'Nro. Nota'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSIK'.
  lwa_fieldcat-ref_fieldname = 'XBLNR'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname        = 'git_saida'.
  lwa_fieldcat-fieldname      = 'BUDAT'.
  lwa_fieldcat-seltext_m      = 'Dt.lcto'.
  lwa_fieldcat-ddictxt        = 'M'.
  lwa_fieldcat-ref_tabname    = 'BSAK'.
  lwa_fieldcat-ref_fieldname  = 'BUDAT'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'ZFBDT'.
  lwa_fieldcat-seltext_m     = 'Dt.Vencimento'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSAK'.
  lwa_fieldcat-ref_fieldname = 'ZFBDT'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'DMBTR'.
  lwa_fieldcat-seltext_m     = 'Valor BRL'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'BSAK'.
  lwa_fieldcat-ref_fieldname = 'DMBTR'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'ID_OPER_AL5'.
  lwa_fieldcat-seltext_m     = 'ID.Oper.AL5'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'ID_OPER_AL5'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'DT_OPER_AL5'.
  lwa_fieldcat-seltext_m     = 'Dt.Oper.AL5'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'DT_OPER_AL5'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'AUGBL'.
  lwa_fieldcat-seltext_m     = 'Doc.Comp.'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-hotspot       = 'X'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'AUGBL'.
  append lwa_fieldcat to ot_fieldcat.

  clear: lwa_fieldcat.
  lwa_fieldcat-tabname       = 'git_saida'.
  lwa_fieldcat-fieldname     = 'BUKRS'.
  lwa_fieldcat-seltext_m     = 'Empresa'.
  lwa_fieldcat-ddictxt       = 'M'.
  lwa_fieldcat-hotspot       = 'X'.
  lwa_fieldcat-ref_tabname   = 'ZFIT0170'.
  lwa_fieldcat-ref_fieldname = 'BUKRS'.
  append lwa_fieldcat to ot_fieldcat.

  if p_ab = 'X'.
    clear: lwa_fieldcat.
    lwa_fieldcat-tabname   = 'git_saida'.
    lwa_fieldcat-fieldname = 'MESSAGE'.
    lwa_fieldcat-seltext_m = 'Mensagem'.
    lwa_fieldcat-ddictxt   = 'M'.
    lwa_fieldcat-just      = 'C'.
    append lwa_fieldcat to ot_fieldcat.
  endif.



endform.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_records  using is_selfield type slis_selfield.

  data: lva_gjahr type bsik-gjahr.
  clear: gwa_saida.

  if is_selfield-fieldname = 'BELNR'.
    read table git_saida into gwa_saida index is_selfield-tabindex.

    check gwa_saida-belnr is not initial.

    lva_gjahr = gwa_saida-budat(4).

    set parameter id 'BUK'  field  gwa_saida-bukrs.
    set parameter id 'BLN'  field  gwa_saida-belnr.
    set parameter id 'GJR'  field  lva_gjahr .
    call transaction 'FB03' and skip first screen.

  else.
    if is_selfield-fieldname = 'AUGBL'.
      read table git_saida into gwa_saida index is_selfield-tabindex.

      check gwa_saida-augbl is not initial.

      lva_gjahr = gwa_saida-budat(4).

      set parameter id 'BUK'  field  gwa_saida-bukrs.
      set parameter id 'BLN'  field  gwa_saida-augbl.
      set parameter id 'GJR'  field  lva_gjahr .
      call transaction 'FB03' and skip first screen.

    else.
      if is_selfield-fieldname = 'EBELN'.
        read table git_saida into gwa_saida index is_selfield-tabindex.

        check gwa_saida-ebeln is not initial.

        set parameter id 'BES' field gwa_saida-ebeln.
        "SET PARAMETER ID 'BSP' FIELD gwa_saida-ebelp.

        call transaction 'ME23N' and skip first screen.
      endif.
    endif.
  endif.
endform.
form html_top_of_page using o_dd_left type ref to cl_dd_document.

  data: o_dd_right type ref to cl_dd_area.
  data: o_dd_middle type ref to cl_dd_area.

  call method o_dd_left->vertical_split
    exporting
      split_area  = o_dd_left
      split_width = '70%'
    importing
      right_area  = o_dd_right.

  call method o_dd_left->vertical_split
    exporting
      split_area  = o_dd_left
      split_width = '50%'
    importing
      right_area  = o_dd_middle.

  data: lit_txt type sdydo_text_table,
        lwa_txt like line of lit_txt.


  call method o_dd_left->add_text
    exporting
      text         = 'Parâmetros:'
      sap_style    = space
      sap_color    = space
      sap_fontsize = cl_dd_document=>medium
      sap_emphasis = cl_dd_document=>strong
      style_class  = space.

  call method o_dd_left->new_line.

  concatenate 'Empresa:' p_bukrs-low into lwa_txt separated by space.
  append  lwa_txt to lit_txt.
  if p_ab = 'X'.
    concatenate 'Opção:' space ' Em aberto para compensar' into lwa_txt.
    append  lwa_txt to lit_txt.
  else.
    concatenate 'Opção:' space  ' Compensadas' into lwa_txt.
    append  lwa_txt to lit_txt.
  endif.

  call method o_dd_left->add_text
    exporting
      text_table = lit_txt
      fix_lines  = 'X'.

  if p_ab = 'X'.
    call method o_dd_right->add_icon(
      exporting
        tabindex = 1
        sap_icon = 'ICON_GENERATE' ).

    call method o_dd_right->add_text
      exporting
        text = '- Gerar Partida AL5 BANK'.

    call method o_dd_right->new_line.

    call method o_dd_right->add_icon(
      exporting
        tabindex = 1
        sap_icon = 'ICON_COMPLETE' ).

    call method o_dd_right->add_text
      exporting
        text = '- Partida Gerada AL5 BANK'.

    call method o_dd_right->new_line.

    call method o_dd_right->add_icon(
      exporting
        tabindex = 1
        sap_icon = 'ICON_DEFECT' ).

    call method o_dd_right->add_text
      exporting
        text = '- Partida Gerada com erro!'.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_GERA_PARTIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_gera_partida .

  data: lva_error type c,
        lva_augbl type zfit0170-augbl.

  loop at git_saida into gwa_saida where mark eq 'X'.
    clear: gva_line.
    move sy-tabix to gva_line.

    clear: lva_augbl.

    perform fm_shdb_f51 using abap_false
                        changing gwa_saida
                                 lva_error
                                 lva_augbl.


    read table git_msg with key msgtyp = 'E'.
    if sy-subrc = 0.
      move icon_defect  to gwa_saida-icon.
      gwa_saida-message = git_msg-msgv1.
      modify git_saida from gwa_saida index gva_line transporting icon message.
    endif.

    read table git_msg  with key msgid = c_msgid
                          msgnr = c_msgnr
                          msgtyp = 'S'.

    if sy-subrc = 0 and lva_augbl is not initial..
      move icon_complete to gwa_saida-icon.
      gwa_saida-message = 'Gerado com sucesso!'.
      gwa_saida-augbl = lva_augbl.
      modify git_saida from gwa_saida index gva_line transporting icon message augbl.
    endif.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_SHDB_F51
*&---------------------------------------------------------------------*
form fm_shdb_f51 using p_residual_comp  type c  "Opção para deixar residual na Compensação
             changing p_saida     type ty_saida
                      p_erro
                      p_augbl    type zfit0170-augbl.

  data: lva_data(10),
        lva_bldat(10),
        lva_kidno         type bseg-kidno,
        lva_data_venc(10),
        lva_dt_mov        type sy-datum,
        lva_sgtxt         type bsik-sgtxt,
        lva_vlrc(16),
        lva_vlrc2(16),
        vhbkid            type bseg-hbkid.

  data: lva_vlrn  type p decimals 2,
        lva_vlrn2 type p decimals 2.

  data: lt_bkdf type table of bkdf,
        lt_bkpf type table of bkpf,
        lt_bsed type table of bsed,
        lt_bseg type table of bseg,
        lt_bsec type table of bsec,
        lt_bset type table of bset,

        wa_bseg type bseg,
        wa_bkpf type bkpf.

  refresh git_bdcdata.

  data: git_forn         type standard table of rgsb4 with header line.

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class           = '0000'
      setnr           = 'MAGGI_CODFORAL5'
      no_descriptions = abap_false
    tables
      set_values      = git_forn
    exceptions
      set_not_found   = 1
      others          = 2.

  read table git_forn index 1.
  vhbkid = zcl_miro=>get_banco_forma_pagamento( i_bukrs = p_saida-bukrs i_forma_pagamento  = 'S' ).

  lva_dt_mov = sy-datum.
  concatenate  lva_dt_mov+6(2) lva_dt_mov+4(2) lva_dt_mov(4) into lva_data separated by '.'.
  concatenate  p_saida-bldat+6(2)  p_saida-bldat+4(2)  p_saida-bldat(4) into lva_bldat separated by '.'.
  concatenate  p_saida-lifnr '-' p_saida-name1 into lva_sgtxt separated by space.
  concatenate  p_saida-zfbdt+6(2) p_saida-zfbdt+4(2) p_saida-zfbdt(4) into lva_data_venc separated by '.'.
  concatenate 'AL5-OPER' p_saida-id_oper_al5 into lva_kidno.


*---> 09/06/2023 - Migração S4 - JS
*            lva_vlrn = p_saida-dmbtr.
  lva_vlrn = conv #( p_saida-dmbtr ).
*<--- 09/06/2023 - Migração S4 - JS
  write: lva_vlrn to lva_vlrc.

*---> 09/06/2023 - Migração S4 - JS
*            lva_vlrn2 = p_saida-dmbe2.
  lva_vlrn2 = conv #( p_saida-dmbe2 ).
*<--- 09/06/2023 - Migração S4 - JS

  write: lva_vlrn2 to lva_vlrc2.

  perform fm_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''            '',
    ''          ''      ''   'BDC_CURSOR'  'RF05A-NEWKO',
    ''          ''      ''   'BDC_OKCODE'  '/00',
    ''          ''      ''   'BKPF-BLDAT'  lva_bldat,      "(BSIK-BLDAT)
    ''          ''      ''   'BKPF-BLART'  'AB', "p_saida-blart,  "(BSIK-BLART)
    ''          ''      ''   'BKPF-BUKRS'  p_saida-bukrs,  "(BSIK-BUKRS)
    ''          ''      ''   'BKPF-BUDAT'  lva_data,       "(Data do Sistema)
    ''          ''      ''   'BKPF-MONAT'  lva_dt_mov+4(2),"(Mês da Data do Ssitema)
    ''          ''      ''   'BKPF-WAERS'  p_saida-waers,  "(BSIK-WAERS)
    ''          ''      ''   'BKPF-XBLNR'  p_saida-xblnr,  "(BSIK-XBLNR)
    ''          ''      ''   'BKPF-BKTXT'  p_saida-belnr,  "5189327632 (ZFIT0170-BELNR)
    ''          ''      ''   'RF05A-NEWBS' '39',
    ''          ''      ''   'RF05A-NEWKO' git_forn-from,       "(SET MAGGI_CODFORAL5)
    ''          ''      ''   'RF05A-NEWUM' '"',             "US161330

    'SAPMF05A' '0304'  'X'   ''            '',
    ''          ''      ''   'BDC_CURSOR'  'BSEG-ZFBDT', "p_saida-zuonr,  "(bsik-zuonr)
    ''          ''      ''   'BDC_OKCODE'  '=ZK',
    ''          ''      ''   'BSEG-WRBTR'  lva_vlrc,   "(bsik-dmbtr)
    ''          ''      ''   'BSEG-GSBER'  p_saida-gsber,  "(bsik-gsber)
*    ''          ''      ''   'BSEG-ZTERM'  '0004',
    ''          ''      ''   'BSEG-ZFBDT'  lva_data_venc, " (bsik-zbd1t+ bsik-zbfbdt)
    ''          ''      ''   'BSEG-ZLSCH'  'S',
    ''          ''      ''   'BSEG-KIDNO'  lva_kidno,  "OPER.1234566789012345 (OPER. ZFIT0170-ID_OPER_AL5)
    ''          ''      ''   'BSEG-ZUONR'  p_saida-zuonr,        "4500919537 (BSIK-ZUONR)
    ''          ''      ''   'BSEG-SGTXT'  lva_sgtxt, "100924-CARGOCENTER AGENCIA DE CARGAS (BSIK-LIFNR – LFA1-NAME1)
*
    'SAPMF05A'  '0332'  'X'   ''            '',
    ''          ''      ''    'BDC_CURSOR'  'BSEG-DMBE2',
    ''          ''      ''    'BDC_OKCODE'  '=SL',
    ''          ''      ''    'BSEG-DMBE2'  lva_vlrc2, "154,70 (BSIK-DMBE2)
"    ''          ''      ''    'BSEG-DMBE3'  ' '   ,    " 549,76 (deixa calcular)
    ''          ''      ''    'BSEG-BVTYP'  git_forn-title,
    ''          ''      ''    'BSEG-HBKID'  vhbkid,

    'SAPMF05A'  '0710'  'X'   ''                  '',
    ''          ''      ''    'BDC_CURSOR'        'RF05A-XPOS1(03)',
    ''          ''      ''    'BDC_OKCODE'        '/00',
    ''          ''      ''    'RF05A-AGBUK'       p_saida-bukrs,  "0001 (BSIK-BUKRS)
    ''          ''      ''    'RF05A-AGKON'       p_saida-lifnr,  "100924 (BSIK-LIFNR)
    ''          ''      ''    'RF05A-AGKOA'       'K',
    ''          ''      ''    'RF05A-XNOPS'       'X',
    ''          ''      ''    'RF05A-XPOS1(01)'   ' ',
    ''          ''      ''    'RF05A-XPOS1(03)'   'X',
*
    'SAPMF05A'  '0731'  'X'   ''           '',
    ''          ''      ''   'BDC_CURSOR'  'RF05A-SEL01(01)',
    ''          ''      ''   'BDC_OKCODE'  '=PA',
    ''          ''      ''   'RF05A-SEL01(01)' p_saida-belnr, "  5103922092 (ZFIT0170-BELNR)

    'SAPDF05X'  '3100'  'X'   ''           '',
    ''          ''      ''   'BDC_OKCODE'  '=BS',
    ''          ''      ''   'BDC_SUBSCR'  'SAPDF05X' ,                               "6102PAGE
    ''          ''      ''   'BDC_CURSOR'  'DF05B-PSBET(01)',
    ''          ''      ''   'RF05A-ABPOS'  '1',

    'SAPMF05A'  '0700'  'X'   ''           '',
    ''          ''      ''   'BDC_CURSOR'  'RF05A-NEWBS',
    ''          ''      ''   'BDC_OKCODE'  '=BU'.


  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.

  if p_erro is initial and gva_augbl is not initial.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = gva_augbl
      importing
        output = gva_augbl.


    p_augbl  = gva_augbl.

    update zfit0170 set augbl = gva_augbl
      where bukrs = p_saida-bukrs
      and   belnr = p_saida-belnr.
    commit work.
    "DEsbloqueio pagto
    clear wa_bkpf.
    refresh lt_bkpf.
    select single *
      from bkpf
      into wa_bkpf
    where bukrs = p_saida-bukrs
    and   belnr = gva_augbl
    and   gjahr = sy-datum+0(4).
    append wa_bkpf to lt_bkpf.

    clear wa_bseg.
    refresh  lt_bseg.
    select single *
         from bseg
         into  wa_bseg
          where bukrs = wa_bkpf-bukrs
          and   belnr = wa_bkpf-belnr
          and   gjahr = wa_bkpf-gjahr
          and   bschl in ( '39' ).
    wa_bseg-zlspr = ''.
    append wa_bseg to lt_bseg.

    call function 'CHANGE_DOCUMENT'
      tables
        t_bkdf = lt_bkdf
        t_bkpf = lt_bkpf
        t_bsec = lt_bsec
        t_bsed = lt_bsed
        t_bseg = lt_bseg
        t_bset = lt_bset.

    if sy-subrc = 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.
    endif.
    "DEsbloqueio pagto

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
form fm_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear gwa_bdcdata.
  gwa_bdcdata-program   = p_program.
  gwa_bdcdata-dynpro    = p_dynpro.
  gwa_bdcdata-dynbegin  = p_start.
  gwa_bdcdata-fnam      = p_fnam.
  gwa_bdcdata-fval      = p_fval.
  append gwa_bdcdata to git_bdcdata.

endform.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
form zf_call_transaction using p_trans changing p_erro.
  clear: gva_augbl.

  refresh git_msg.

  data: lva_mode(1).

  "lva_mode = 'E'.
  lva_mode = 'N'.

  call transaction p_trans using git_bdcdata
       mode lva_mode
       update  'S'
       messages into git_msg.

  read table git_msg with key msgtyp = 'A'.
  if sy-subrc = 0.
    p_erro = 'X'.
  else.
    read table git_msg with key msgtyp = 'E'.
    if sy-subrc = 0.
      p_erro = 'X'.
    endif.
  endif.

  if p_erro is initial.
    read table git_msg  with key msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

    if sy-subrc = 0.
      move git_msg-msgv1 to gva_augbl.
    endif.

  endif.

endform.
