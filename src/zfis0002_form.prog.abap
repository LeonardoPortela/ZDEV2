*======================================================================*
* PROJETO            : SAP Ninjas                                      *
* PROGRAMA           : ZFIS0002                                        *
* TRANSACAO          : ZFIS0001                                        *
* DESCRICAO          : Relatório Crédito Presumido Sobre Vendas Fabr.  *
*======================================================================*
* AUTOR              : Ronaldo Freitas                                 *
* Solicitante        : Adelson Silva                                   *
* DATA               : 15.08.2024                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*&---------------------------------------------------------------------*
*&  Include           ZFIS0002_FORM
*&---------------------------------------------------------------------*

form fm_selecao.
*======================================================================*
* Busca informações relevantes para o processo
*======================================================================*

  data: rg_matnr type range of matnr,
        lv_cont  type char02 value '00'.


* Buscar Impostos SD (MT) Selecionar na tabela ZSDT0245 o valor dos campos abaixo:
  select auart, kschl, werks, matnr, matkl, perc
    from zsdt0245 " Condições impostos MT (SD)
    into table @data(it_zsdt0245)
  where kschl in ('ZCP1', 'ZPP1')
    and auart in @s_fkart
    and werks in @s_werks
    and matnr in @s_matnr.


  if it_zsdt0245 is not initial.
    sort it_zsdt0245 by matkl.
    data(it_zsdt0245_aux) = it_zsdt0245[].
    delete it_zsdt0245_aux where matkl eq space.
    delete it_zsdt0245 where matkl ne space.

    sort it_zsdt0245 by matnr werks kschl.

    select matnr matkl from mara into table it_material
      for all entries in it_zsdt0245_aux
      where matkl eq it_zsdt0245_aux-matkl.
    if sy-subrc eq 0.
      sort it_zsdt0245_aux by matkl kschl.
      loop at it_zsdt0245_aux into data(ws_zsdt0245).
        loop at it_material assigning field-symbol(<ws_material>) where matkl eq ws_zsdt0245-matkl.
          ws_zsdt0245-matnr = <ws_material>-matnr.
          append ws_zsdt0245 to it_zsdt0245.
        endloop.
      endloop.
    endif.

    sort it_zsdt0245 by matnr werks kschl.
*    delete adjacent duplicates from it_zsdt0245 comparing matnr werks kschl.

    select vk~fkart, vk~vbeln, vk~kunag, k~name1 as namek,
           vk~vkorg, vk~fkdat,
           vp~matnr, vp~werks, tw~name1, vp~arktx,
           vp~netwr, lin~docnum
      from vbrk as vk
      inner join vbrp as vp
      on vk~vbeln = vp~vbeln
      inner join kna1 as k
      on k~kunnr = vk~kunag
      inner join t001w as tw
      on vp~werks = tw~werks
      left join j_1bnflin as lin
      on lin~refkey = vk~vbeln
      into table @data(it_vbrkp)
      for all entries in @it_zsdt0245
       where vk~fkart eq @it_zsdt0245-auart
         and vk~vbeln in @s_vbeln
         and vp~werks eq @it_zsdt0245-werks
*         and vp~matnr eq @it_zsdt0245-matnr
         and vp~matnr in @rg_matnr "
         and vk~fkdat in @s_fkdat.

    if sy-subrc is initial.

      select * from kna1
        into table @data(it_kna1)
        for all entries in @it_vbrkp
        where kunnr eq @it_vbrkp-kunag.

      if sy-subrc is initial.
        sort it_kna1 by kunnr.
      endif.

      select * from t001w
      into table @data(it_t001w)
        for all entries in @it_vbrkp
        where werks eq @it_vbrkp-werks.

      if sy-subrc is initial.
        sort it_t001w by werks.
      endif.

      free: lt_vbrkp.
      loop at it_vbrkp assigning field-symbol(<fs_vbrkp>).
        wa_vbrkp = corresponding #( <fs_vbrkp> ).
        wa_vbrkp-objk = |ZFIS{ <fs_vbrkp>-vbeln }00{ <fs_vbrkp>-fkdat(4) }|.
        append wa_vbrkp to lt_vbrkp.
        clear wa_vbrkp.
      endloop.

      "Verifica se a documento da chave foi estornado.
      select * from zfit0229 into table @data(it_zfit0229)
      for all entries in @lt_vbrkp
      where obj_key_ref eq @lt_vbrkp-objk.
      if it_zfit0229 is not initial.
        sort it_zfit0229 by obj_key.
        loop at lt_vbrkp assigning field-symbol(<ws_vbrkp>).
          read table it_zfit0229 into data(ws_zfit0229) with key obj_key_ref = <ws_vbrkp>-objk.
          if sy-subrc eq 0.
            "===============================================================Alteração nova chave do objeto devido estorno.
            lv_cont = |{ ws_zfit0229-obj_key+14(2) }|.
            lv_cont = lv_cont + 01.
            lv_cont = |{ lv_cont alpha = in }|.
            <ws_vbrkp>-objk = |ZFIS{ <ws_vbrkp>-vbeln }{ lv_cont }{ <ws_vbrkp>-fkdat(4) }|.
          endif.
          "===============================================================Alteração nova chave do objeto devido estorno.
        endloop.
      endif.

      select * from zib_contabil_chv
        into table @data(it_contabil_chv)
        for all entries in @lt_vbrkp
        where obj_key eq @lt_vbrkp-objk.

      if sy-subrc is not initial.
        select * from zib_contabil_err
          into table @data(it_contabil_err)
          for all entries in @lt_vbrkp
          where obj_key eq @lt_vbrkp-objk.
      endif.

      if it_contabil_chv is not initial.
        select *
        from bkpf into table @data(it_bkpf)
            for all entries in @it_contabil_chv
       where bukrs = @it_contabil_chv-bukrs
         and belnr = @it_contabil_chv-belnr
         and gjahr = @it_contabil_chv-gjahr
         and stblg ne @space.
      endif.

      if lt_vbrkp is not initial.
        "j_1bnfe_active
*        free: it_j_1bnfe_active.
        select *
        from j_1bnfe_active
        into table @data(it_j_1bnfe_active)
        for all entries in @lt_vbrkp
        where docnum eq @lt_vbrkp-docnum.
      endif.

      free it_saida.
      data: lv_wrbtr type	tslxx12.
      loop at lt_vbrkp assigning field-symbol(<fs_vbrkpx>).
        append initial line to it_saida assigning field-symbol(<fs_saida>).

        <fs_saida>-vkorg = <fs_vbrkpx>-vkorg.  "*Empresa            =  VBRK-VKORG
        <fs_saida>-kunag = <fs_vbrkpx>-kunag.  "*Cliente            =  VBRK-KUNAG
        <fs_saida>-fkart = <fs_vbrkpx>-fkart.
        <fs_saida>-obj_key = <fs_vbrkpx>-objk.
        <fs_saida>-docnum  = <fs_vbrkpx>-docnum.

        read table it_j_1bnfe_active into data(wa_j_1bnfe_active) with key docnum = <fs_vbrkpx>-docnum.
        if wa_j_1bnfe_active-docsta eq ' ' and wa_j_1bnfe_active-scssta eq ' ' .
          <fs_saida>-status_doc_fiscal = 'Não Enviada'.
        elseif wa_j_1bnfe_active-docsta eq '  '.
          <fs_saida>-status_doc_fiscal = 'Aguardando resposta'.
        elseif wa_j_1bnfe_active-docsta eq 1 and ( wa_j_1bnfe_active-cancel eq 'X' or wa_j_1bnfe_active-scssta eq '2' ).
          <fs_saida>-status_doc_fiscal = 'Cancelada'.
        elseif wa_j_1bnfe_active-docsta eq 1 and ( wa_j_1bnfe_active-cancel ne 'X' and wa_j_1bnfe_active-scssta ne '2' ).
          <fs_saida>-status_doc_fiscal = 'autorizada'.
        elseif wa_j_1bnfe_active-docsta eq 2.
          <fs_saida>-status_doc_fiscal = 'rejeitada'.
        elseif wa_j_1bnfe_active-docsta eq 3.
          <fs_saida>-status_doc_fiscal = 'recusada'.
        else.
          <fs_saida>-status_doc_fiscal =  wa_j_1bnfe_active-docsta .
        endif.

        read table it_kna1 assigning field-symbol(<fs_kna1>) with key kunnr = <fs_vbrkpx>-kunag
                                                             binary search.
        if sy-subrc is initial.
          <fs_saida>-name1 = <fs_kna1>-name1.   "*Nome do Cliente    =  KNA1-NAME1
        endif.
        <fs_saida>-werks = <fs_vbrkpx>-werks.  "*Filial             =  VBRP-WERKS

        read table it_t001w assigning field-symbol(<fs_t001w>) with key werks = <fs_vbrkpx>-werks
                                                              binary search.
        if sy-subrc is initial.
          <fs_saida>-fname1 = <fs_t001w>-name1.  "*Nome Filial        =  T001W-NAME1
        endif.

        <fs_saida>-matnr = <fs_vbrkpx>-matnr.  "*Material           =  VBRP-MATNR
        <fs_saida>-arktx = <fs_vbrkpx>-arktx.  "*Descrição Material =  VBRP-ARKTX
        <fs_saida>-vbeln = <fs_vbrkpx>-vbeln.  "*Doc.Faturamento    =  VBRK-VBELN
        <fs_saida>-fkdat = <fs_vbrkpx>-fkdat+6(2) && '/' && <fs_vbrkpx>-fkdat+4(2) && '/' && <fs_vbrkpx>-fkdat(4).  "*Data Faturamento   =  VBRK-FKDAT
        <fs_saida>-netwr = ( <fs_vbrkpx>-netwr ).  "*Valor Venda R$     =  VBRP-NETWR

        read table it_zsdt0245 assigning field-symbol(<fs_zsdt0245>) with key  matnr = <fs_vbrkpx>-matnr
                                                                               werks = <fs_vbrkpx>-werks
                                                                               kschl = 'ZPP1'
                                                                     binary search.
        if sy-subrc is initial.
          <fs_saida>-apis  = <fs_zsdt0245>-perc.                "*Aliq.PIS           =  variável XPERC_PIS
          lv_wrbtr  = ( ( <fs_vbrkpx>-netwr * ( <fs_saida>-apis ) ) / 100 ).               "*Valor PIS          =  Variável XPIS
          <fs_saida>-vpis = conv #( lv_wrbtr ).

          <fs_saida>-apisv = conv #( <fs_zsdt0245>-perc ).
        endif.

        read table it_zsdt0245 assigning <fs_zsdt0245> with key  matnr = <fs_vbrkpx>-matnr
                                                                 werks = <fs_vbrkpx>-werks
                                                                 kschl = 'ZCP1'
                                                       binary search.
        if sy-subrc is initial.
          <fs_saida>-acofins = <fs_zsdt0245>-perc.              "*Aliq.Cofins        =  variável XPERC_COFINS
          <fs_saida>-vcofins = ( ( <fs_vbrkpx>-netwr * ( <fs_saida>-acofins ) ) / 100 ).              "*Valor COFINS       =  Variável XCOFINS
        endif.

        read table it_contabil_chv assigning field-symbol(<fs_contabil_chv>) with key obj_key = <fs_vbrkpx>-objk.
        if sy-subrc is initial and <fs_contabil_chv>-belnr is not initial.
          <fs_saida>-belnr   = <fs_contabil_chv>-belnr. "*Doc. Contabil      =  ZIB_CONTABIL_CHV-BELNR
          <fs_saida>-gjahr   = <fs_contabil_chv>-gjahr. "
          <fs_saida>-status = '@DF@'.               "*Status             =  Regra STATUS (conforme abaixo)


          "Verifica se o documento foi estornado.
          read table it_bkpf into data(wa_bkpf) with key bukrs = <fs_contabil_chv>-bukrs
                                                         belnr = <fs_contabil_chv>-belnr
                                                         gjahr = <fs_contabil_chv>-gjahr.
          if sy-subrc eq 0.
            <fs_saida>-stblg = ''.
            <fs_saida>-belnr = ''.
            <fs_saida>-gjahr = ''.
            <fs_saida>-status = '@5D@'.


          endif.

        elseif sy-subrc is initial and <fs_contabil_chv>-belnr is initial.
          read table it_contabil_err assigning field-symbol(<fs_contabil_err>) with key obj_key = <fs_vbrkpx>-objk.
          if sy-subrc is not initial.
            <fs_saida>-status = '@5D@'.               "*Status             =  Regra STATUS (conforme abaixo) am
          else.
            read table it_contabil_err assigning <fs_contabil_err> with key obj_key = <fs_vbrkpx>-objk.
            <fs_saida>-status = '@F1@'.               "*Status             =  Regra STATUS (conforme abaixo) BV
          endif.
        else.
          read table it_contabil_err assigning <fs_contabil_err> with key obj_key = <fs_vbrkpx>-objk.
          if sy-subrc is initial.
            <fs_saida>-status = '@F1@'.               "*Status             =  Regra STATUS (conforme abaixo) BV

            <fs_saida>-status = '@DH@'.  "Lista erros
          else.
            <fs_saida>-status = '@5D@'.               "*Status             =  Regra STATUS (conforme abaixo) AM
          endif.
        endif.

        if <fs_saida>-status_doc_fiscal = 'Cancelada' and <fs_saida>-belnr is initial.
          <fs_saida>-check_canc = abap_true.
        endif.
      endloop.

    else.

      message s836(sd) with 'Para os parâmetros informados não ' 'foi encontrado Faturamento.' display like 'E'.
      exit.
    endif.
  else.
    message s836(sd) with 'Parâmetros informados não foi encontrado '  'na Transação ZSDT0167.' display like 'E'.
    exit.
  endif.

  sort it_saida by check_canc.
  delete it_saida where check_canc eq abap_true.

  sort it_saida by vkorg werks name1.
endform.
module user_command_0100 input.
  case sy-ucomm.
    when 'CANCEL'.
      leave to screen 0. "LEAVE PROGRAM.
    when 'SAIR'.
      leave program.
    when 'VOLTAR'.
      leave to screen 0.
  endcase.
endmodule.

form fm_cria_fieldcat .
  types: lit_fieldcat_aux type table of lvc_s_fcat with default key.

  git_fcat_pend = value lit_fieldcat_aux(
( tabname = 'IT_SAIDA'  fieldname = 'STATUS'                 coltext = 'Status               ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' STATUS    '  hotspot = 'X'       )
( tabname = 'IT_SAIDA'  fieldname = ' VKORG     '            coltext = '  Empresa            ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VKORG     '         )
( tabname = 'IT_SAIDA'  fieldname = ' KUNAG     '            coltext = '  Cliente            ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' KUNAG     '         )
( tabname = 'IT_SAIDA'  fieldname = ' NAME1     '            coltext = '  Nome do Cliente    ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' NAME1     '         )
( tabname = 'IT_SAIDA'  fieldname = ' WERKS     '            coltext = '  Filial             ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' WERKS     '         )
( tabname = 'IT_SAIDA'  fieldname = ' FNAME1    '            coltext = '  Nome Filial        ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FNAME1    '         )
( tabname = 'IT_SAIDA'  fieldname = ' MATNR     '            coltext = '  Material           ' col_opt = 'X' no_zero = 'X' ref_table = 'TY_SAIDA' ref_field = ' MATNR     '         )
( tabname = 'IT_SAIDA'  fieldname = ' ARKTX     '            coltext = '  Descrição Material ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' ARKTX     '         )
( tabname = 'IT_SAIDA'  fieldname = ' VBELN     '            coltext = '  Doc.Faturamento    ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VBELN     '         )
( tabname = 'IT_SAIDA'  fieldname = ' FKDAT     '            coltext = '  Data Faturamento   ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FKDAT     '         )
( tabname = 'IT_SAIDA'  fieldname = ' NETWR     '            coltext = '  Valor Venda R$     ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' NETWR     '   currency = 'BRL'   )
( tabname = 'IT_SAIDA'  fieldname = ' APISV      '           coltext = '  Aliq.PISN          ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' APISV     '        )
( tabname = 'IT_SAIDA'  fieldname = ' VPIS      '            coltext = '  Valor PIS          ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VPIS      '  currency = 'BRL'       )
( tabname = 'IT_SAIDA'  fieldname = ' ACOFINS   '            coltext = '  Aliq.Cofins        ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' ACOFINS   '         )
( tabname = 'IT_SAIDA'  fieldname = ' VCOFINS   '            coltext = '  Valor Cofins       ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VCOFINS   '  currency = 'BRL'       )
( tabname = 'IT_SAIDA'  fieldname = ' BELNR     '            coltext = '  Documento contábil ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' BELNR     '  hotspot = 'X'       )
*( tabname = 'IT_SAIDA'  fieldname = ' GJAHR     '            coltext = '  Ano doc contábil   ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' GJAHR     '  hotspot = 'X'       )
*( tabname = 'IT_SAIDA'  fieldname = ' STBLG     '            coltext = '  Documento storno   ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' STBLG     '  hotspot = 'X'       )
( tabname = 'IT_SAIDA'  fieldname = ' DOCNUM     '           coltext = '  Docnum fiscal      ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' DOCNUM    '   hotspot = 'X'      )
( tabname = 'IT_SAIDA'  fieldname = ' STATUS_DOC_FISCAL '    coltext = '  Status doc fiscal  ' col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = '           '   hotspot = ' '      )
).


endform.

form fm_alv.

  data: i_filtros	    type zif_screen_linha_filtro_t,
        dg_splitter_1 type ref to cl_gui_splitter_container.
  gs_variant-report = sy-repid.

  perform fm_cria_fieldcat.

  concatenate sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) into lva_data.

  if gob_gui_alv_grid is initial.

    clear: i_filtros.
    append value #( parametro = 'Filial:' valor = s_werks-low ) to i_filtros.
    append value #( parametro = 'Material:' valor = s_matnr-low ) to i_filtros.
    append value #( parametro = 'Doc. Faturamento:' valor = s_fkart-low ) to i_filtros.
    append value #( parametro = 'Data Faturamento:' valor = s_fkdat-low ) to i_filtros.

    if zcl_screen=>zif_screen~set_criar_tela_padrao_report(

    exporting
      i_titulo  = '' "'Crédito Presumido Sobre Vendas Fábricas. - ' &&  lva_data
          i_filtros = i_filtros
    changing
      split     = dg_splitter_1
      alv = gob_gui_alv_grid
      )
      eq abap_true.

      wa_layout-sel_mode   = 'A'.
      wa_layout-zebra      = 'X'.

      create object o_event
        exporting
          io_alv_grid = gob_gui_alv_grid.

      set handler: o_event->on_toolbar               for gob_gui_alv_grid,
                   o_event->on_handle                for gob_gui_alv_grid,
                   o_event->zm_handle_hotspot_report for gob_gui_alv_grid.

      wa_layout-cwidth_opt = 'X'.
*      o_status_alv->set_frontend_layout( is_layout = ls_layout ).

      call method gob_gui_alv_grid->set_table_for_first_display
        exporting
          is_variant                    = gs_variant
          i_save                        = 'A'
          is_layout                     = wa_layout
          i_default                     = 'X'
        changing
          it_outtab                     = it_saida
          it_fieldcatalog               = git_fcat_pend
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4.


*      CALL METHOD gob_gui_alv_grid->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.

    endif.
  else.


    call method gob_gui_alv_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.
endform.

form fm_exibirdados .
  if it_saida[] is not initial.
    call screen '0100'.
  else.
    message s836(sd) with 'Nenhum documento encontrado!' display like 'E'.
    exit.
  endif.
endform.
form fm_exibirdadosv2 .
  if it_saida[] is not initial.
    leave to screen '0100'.
  else.
    message s836(sd) with 'Nenhum documento encontrado!' display like 'E'.
    exit.
  endif.
endform.

module status_0100 output.
  set pf-status 'ST0100'.
  set titlebar  'ZFIS0002'.
  perform fm_alv.
endmodule.
*&---------------------------------------------------------------------*
*& Form user_command_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
form user_command_0100  using e_row_id type lvc_s_row
                             p_e_column_id type lvc_s_col
                             p_es_eow_no type lvc_s_roid.

  read table it_saida[] into data(lwa_saida) index e_row_id-index.

  check ( sy-subrc = 0 ).

  condense p_e_column_id-fieldname no-gaps.

  case p_e_column_id-fieldname.
    when 'STATUS'.
      if lwa_saida-status is not initial and ( lwa_saida-status eq '@DH@' or lwa_saida-status eq '@F1@' ).
        perform f_exibe_erro_zib using lwa_saida-obj_key.
      endif.

    when 'BELNR' or 'STBLG'.
      data: v_belnr type belnr.
*      check lwa_saida-belnr is not initial or lwa_saida-stblg is not initial.
*      clear: v_belnr.
*      if lwa_saida-belnr is not initial.
*        v_belnr = lwa_saida-belnr.
*      elseif
*        v_belnr = lwa_saida-stblg.
*      endif.

*      select single vkorg from t001w where werks = @lwa_saida-werks into @data(_empresa).
      set parameter id 'BUK' field lwa_saida-vkorg.
      set parameter id 'BLN'  field lwa_saida-belnr.
      set parameter id 'GJR'  field lwa_saida-gjahr.
      call transaction 'FB03' and skip first screen.
    when 'DOCNUM'.
      check lwa_saida-docnum is not initial.
      set parameter id 'JEF'  field lwa_saida-docnum.
      call transaction 'J1B3N' and skip first screen.

  endcase.

*    CALL METHOD gob_gui_alv_grid->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable.

endform.
*&---------------------------------------------------------------------*
*& Form f_exibe_erro_zib
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LVA_CHAVE_ZIB
*&---------------------------------------------------------------------*
form f_exibe_erro_zib  using    p_lva_chave_zib.

  types: begin of ty_zib_err,
           obj_key        type zib_contabil_err-obj_key,
           dt_atualizacao type zib_contabil_err-dt_atualizacao,
           hr_atualizacao type zib_contabil_err-hr_atualizacao,
           message        type zib_contabil_err-message,
         end of ty_zib_err.
  data: lit_zib_err type table of ty_zib_err.

  check ( p_lva_chave_zib is not initial ).

  select obj_key, dt_atualizacao, hr_atualizacao, message
     from zib_contabil_err into table @lit_zib_err
    where obj_key = @p_lva_chave_zib.
  if ( sy-subrc = 0 ).
    cl_demo_output=>new(
      )->begin_section( `ZIB_CONTABIL_ERR:`
      )->write_text( |Erro(s) encontrado(s) para o documento: \n|
      )->write_data( lit_zib_err[]
      )->end_section(
      )->display( ).
  endif.
endform.
