*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
********************************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                                     *
* Data desenv ...: 07.12.2011                                                              *
* Objetivo    ...: Relatório de Comparativo de Saída e Chegada - MODAL FERROVIÁRIO         *
* Transação   ...: ZFIR                                                                *
* Autor       ...: Victor Hugo                                                             *
********************************************************************************************
report  zfir0021.


tables: zfit0141, zfit0142, zfit0230, sscrfields, zib_cte_dist_ter.

type-pools: vrm.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: zib_nfe_forn, lfa1, zib_nfe_dist_ter.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
types:
  begin of ty_zib_nfe_forn,
    bukrs             type zib_nfe_forn-bukrs,
    branch            type zib_nfe_forn-branch,
    nu_chave_cnpj     type zib_nfe_forn-nu_chave_cnpj,
    nu_chave_modelo   type zib_nfe_forn-nu_chave_modelo,
    nu_chave_serie    type zib_nfe_forn-nu_chave_serie,
    nu_chave_numero   type zib_nfe_forn-nu_chave_numero,
    dt_emissao        type zib_nfe_forn-dt_emissao,
    st_nota           type zib_nfe_forn-st_nota,
    nu_ie             type zib_nfe_forn-nu_ie,
    nu_chave          type zib_nfe_forn-nu_chave,
    docnum            type zib_nfe_forn-docnum,
    docnum_ref        type zib_nfe_forn-docnum_ref,
    nu_chave_cnpj_aux type lfa1-stcd1,
    del(1),
  end of ty_zib_nfe_forn,

  begin of ty_chv,
    nu_chave type zib_nfe_forn-nu_chave,
  end of ty_chv,

  begin of ty_lfa1,
    name1 type lfa1-name1,
    stcd1 type lfa1-stcd1,
*Inicio de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
    stcd2 type lfa1-stcd2,
*Fim de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
    stcd3 type lfa1-stcd3,
    lifnr type lfa1-lifnr,
    regio type lfa1-regio, "US #182015 - MMSILVA - 16.06.2025
    pstlz type lfa1-pstlz, "US #182015 - MMSILVA - 16.06.2025
    txjcd type lfa1-txjcd, "US #182015 - MMSILVA - 16.06.2025
  end of ty_lfa1,

  begin of ty_saida,
    bukrs           type zib_nfe_forn-bukrs,
    branch          type zib_nfe_forn-branch,
    nu_chave_cnpj   type zib_nfe_forn-nu_chave_cnpj,
    nu_chave_modelo type zib_nfe_forn-nu_chave_modelo,
    nu_chave_serie  type zib_nfe_forn-nu_chave_serie,
    nu_chave_numero type zib_nfe_forn-nu_chave_numero,
    dt_emissao      type zib_nfe_forn-dt_emissao,
    name1           type lfa1-name1,
    stcd1           type lfa1-stcd1,
    lifnr           type lfa1-lifnr,
    st_nota         type zib_nfe_forn-st_nota,
    line_color(4)   type c, "Used to store row color attributes
    st_distrib      type c length 4,
    cd_operacao     type  zsdt0127-cd_operacao,
    ds_operacao     type char80,
    authcode        type zsdt0127-authcode,
    msg_retorno     type zsdt0127-msg_retorno,
    autorizado      type zsdt0127-autorizado,
    docnum          type zib_nfe_forn-docnum,
    pstdat          type j_1bnfdoc-pstdat,
    docnum_r        type zmmt_ee_zgr_docs-docnum,
    nu_chave        type zib_nfe_forn-nu_chave,
    nu_ie           type zib_nfe_forn-nu_ie,
    tpnf            type char20,
    finnfe          type char20,
    tpcte           type char20,
    tpserv          type char20,
    modal           type char20,
    doc_ref_key     type char44,
    cfop            type string, "US #182015 - MMSILVA - 16.06.2025
    modelo_nota(31) type c, "US #182015 - MMSILVA - 16.06.2025
    uf_forn         type regio, "US #182015 - MMSILVA - 16.06.2025
    uf_dest         type regio, "US #182015 - MMSILVA - 16.06.2025
    vl_total        type zib_nfe_dist_ter-vl_total_fatura, "US #182015 - MMSILVA - 16.06.2025
    gp_mercadorias  type string, "US #182015 - MMSILVA - 16.06.2025
    pstlz_forn      type lfa1-pstlz, "US #182015 - MMSILVA - 16.06.2025
    txjcd_forn      type lfa1-txjcd, "US #182015 - MMSILVA - 16.06.2025
    pstlz_dest      type lfa1-pstlz, "US #182015 - MMSILVA - 16.06.2025
    txjcd_dest      type lfa1-txjcd, "US #182015 - MMSILVA - 16.06.2025
    nro_dias        type zfit259-nro_dias,
    dt_prevista     type zib_nfe_forn-dt_emissao,
    omisso(3),
    data_bipagem    type zib_nfe_dist_ter-data_bipagem,
    hora_bipagem    type zib_nfe_dist_ter-hora_bipagem,
    usuario_bipagem type zib_nfe_dist_ter-usuario_bipagem,
  end of ty_saida.

*** US #182015 - MMSILVA - 16.06.2025 - Ini ***
data: begin of ls_cte_data,
        valor_prestacao type zib_cte_dist_ter-valor_prestacao,
        codg_cfop       type zib_cte_dist_ter-codg_cfop,
        ebeln           type zib_cte_dist_ter-ebeln,
        ds_prod_pred    type zib_cte_dist_ter-ds_prod_pred,
        cfotxt          type j_1bagnt-cfotxt,
        matkl           type ekpo-matkl,
      end of ls_cte_data.
*** US #182015 - MMSILVA - 16.06.2025 - Fim ***

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

data: it_zib_nfe_forn     type table of ty_zib_nfe_forn,
      it_zib_nfe_forn_aux type table of ty_zib_nfe_forn,
      it_zib_nfe_forn_dep type table of ty_zib_nfe_forn,
      tg_zib_nfe_dist_ter type table of zib_nfe_dist_ter with header line,
      tg_zib_cte_dist_ter type table of zib_cte_dist_ter with header line,
      it_lfa1             type table of ty_lfa1,
      wa_mensagem         type char30,
      it_msg_return       type table of zfiwrs0002,
      wa_msg_return       type zfiwrs0002,
      it_saida            type table of ty_saida,
      tg_xml              type znfe_xml_sefaz_auth,
      tg_xml_cte          type zcte_xml_sefaz_auth,
      tg_zsdt0127_aut     type table of zsdt0127            with header line,
      it_j_1bnfe_active   type table of j_1bnfe_active.

data: w_zib_nfe_dist_ter type table of zib_nfe_dist_ter,
      w_zib_cte_dist_ter type table of zib_cte_dist_ter,
      ztipo              type char01,
      p_pass             type char01, "US #182015 - MMSILVA - 16.06.2025
      ck_mnf1            type char01. "US #182015 - MMSILVA - 16.06.2025

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

data:
  wa_zib_nfe_forn     type ty_zib_nfe_forn,
  wa_zib_nfe_forn_aux type ty_zib_nfe_forn,
  wa_lfa1             type ty_lfa1,
  wa_saida            type ty_saida,
  wa_cont             type ref to cl_gui_custom_container,
  wa_alv              type ref to  cl_gui_alv_grid,
  wa_layout           type lvc_s_layo,
  wa_stable           type lvc_s_stbl,
  tl_fiscal           type table of zsdt0127_fiscal,
  w_fiscal            like zsdt0127_fiscal.


*** US #182015 - MMSILVA - 17.06.2025 - Ini ***
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
ranges: r_st_mnf  for zsdt0127-cd_operacao.
*** US #182015 - MMSILVA - 17.06.2025 - Fim ***


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
data: it_fcat       type table of lvc_s_fcat.
data: it_list    type vrm_values,
      list_value type vrm_value.

data: it_sel_rows type lvc_t_row,
      wa_sel_rows type lvc_s_row.

data: it_selected_rows    type lvc_t_row,
      wa_selected_rows    type lvc_s_row,
      it_selected_rows_cc type lvc_t_row, "Carta de correção
      wa_selected_rows_cc type lvc_s_row.


class lcl_event_handler_0100 definition.
  public section.
    class-methods: handle_hotspot_click for event hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.
endclass.


class lcl_event_handler_0100 implementation.                 "

  method handle_hotspot_click.
    perform handle_hotspot_click using '0100' e_row_id e_column_id es_row_no.
  endmethod.

endclass.

data: lcl_event_handler_0100 type ref to lcl_event_handler_0100.



*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.
  select-options: p_bukrs    for  zib_nfe_dist_ter-bukrs,      " OBLIGATORY,
                  p_branch   for  zib_nfe_dist_ter-branch,     " OBLIGATORY,
                  p_lifnr    for lfa1-lifnr modif id aut, "US # 182015 - MMSILVA - 16.06.2025
                  p_cnpj     for zib_nfe_forn-nu_chave_cnpj,
                  p_stcd3    for lfa1-stcd3 ,
                  p_data     for zib_nfe_forn-dt_emissao,
                  p_nota     for zib_nfe_forn-nu_chave_numero,
                  p_cfop     for zib_cte_dist_ter-codg_cfop modif id aut, "US # 182015 - MMSILVA - 16.06.2025
                  p_chave    for zfit0230-chave_acesso. "US #172277 - MMSILVA - 28.03.2025
*                  p_chave    for zib_nfe_forn-nu_chave. "US #172277 - MMSILVA - 28.03.2025 - Comentado devido nova formatação
  parameters      p_data_b   type  zib_nfe_forn-dt_emissao modif id a obligatory.

selection-screen: end of block b1.


selection-screen begin of block b2 with frame.
  parameters             p_list(10) type c as listbox
  visible length 10.
selection-screen end of block b2.

*** US # 182015 - MMSILVA - 16.06.2025 - Ini ***
selection-screen begin of block b3 with frame title text-041.
  selection-screen comment /1(50) descr1 modif id aut.
  parameters: p_mnf1 as checkbox modif id aut,  "Sem manifesto
              p_mnf2 as checkbox modif id aut,  "Ciencia da operacao
              p_mnf3 as checkbox modif id aut,  "Confirmação da operacao
              p_mnf4 as checkbox modif id aut,  "Operação nao realizada
              p_mnf5 as checkbox modif id aut,  "Desconhecimento da Operacao
              p_mnf6 as checkbox modif id aut.  "Desacordo de Entrega de Serviços (CT-e)
selection-screen end of block b3.

selection-screen function key 1.
*** US # 182015 - MMSILVA - 16.06.2025 - Fim ***

initialization.
  if sy-tcode = 'ZFIS74'.
    sy-title = 'Relatorio passivo omisso'.
    p_mnf1 = 'X'.
  endif.
  list_value-key = '1'.
  list_value-text = '1-Ativo'.
  append list_value to it_list.

  list_value-key = '2'.
  list_value-text = '2-Cancelado'.
  append list_value to it_list.

  list_value-key = '3'.
  list_value-text = '3-Cancelado'.
  append list_value to it_list.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_LIST'
      values = it_list.
*ALRS
**** US # 182015 - MMSILVA - 16.06.2025 - Ini ***
*  move 'Relatório Passivo Omisso' to sscrfields-functxt_01.
*  move 'Relatório Passivo Omisso' to sscrfields-functxt_02.
*  move 'REL_PASS'                 to sscrfields-ucomm.
**** US # 182015 - MMSILVA - 16.06.2025 - Fim ***


* US #172277 - MMSILVA - 28.03.2025 - Inicio
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.

  loop at p_chave.

    replace all occurrences of regex '[^\d]' in p_chave-low with ''.
    condense p_chave-low no-gaps.

    modify p_chave.

  endloop.
* US #172277 - MMSILVA - 28.03.2025 - Fim

*** US #182015 - MMSILVA - 16.06.2025 - Ini ***
*  case sscrfields-ucomm.
*    when'FC01'.
  if sy-tcode = 'ZFIS74'.
    if p_bukrs is initial or p_branch is initial or p_data is initial.
      message 'Preencher os campos Empresa, Centro e Data de Emissão.' type 'S' display like 'E'.
      stop.
    else.
      p_pass = abap_true.

      loop at p_cnpj assigning field-symbol(<fs_cnpj>).
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <fs_cnpj>-low
          importing
            output = <fs_cnpj>-low.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <fs_cnpj>-high
          importing
            output = <fs_cnpj>-high.
      endloop.

      loop at p_nota assigning field-symbol(<fs_nota>).
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <fs_nota>-low
          importing
            output = <fs_nota>-low.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <fs_nota>-high
          importing
            output = <fs_nota>-high.
      endloop.
*
*      perform: f_seleciona_dados.
*
*      call screen 0100.
    endif.
  endif.
*  endcase.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
at selection-screen output.
  descr1 = 'Manifesto'.
  if sy-tcode ne 'ZFIS74'.
    loop at screen.
      if screen-group1 = 'A'.
        screen-invisible = 1.
        screen-input = 0.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.
  "
  if  sy-tcode eq 'ZFIS74'.
    loop at screen.
      if screen-group1 = 'AUT'.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
start-of-selection.

  if ( p_bukrs is initial ) and ( p_stcd3 is initial ) and ( p_chave is initial ).
    message 'Informe o CNPJ ou IE do emissor!' type 'S'.
    stop.
  endif.

  loop at p_cnpj assigning field-symbol(<fs_cnpj>).
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_cnpj>-low
      importing
        output = <fs_cnpj>-low.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_cnpj>-high
      importing
        output = <fs_cnpj>-high.
  endloop.

  loop at p_nota assigning field-symbol(<fs_nota>).
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_nota>-low
      importing
        output = <fs_nota>-low.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_nota>-high
      importing
        output = <fs_nota>-high.
  endloop.



  perform: f_seleciona_dados.

  call screen 0100.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
form f_seleciona_dados .

*** US #182015 - MMSILVA - 16.06.2025 - Ini ***
  perform f_manifest_ranges.
*** US #182015 - MMSILVA - 16.06.2025 - Fim ***

  ranges: r_stcd3_aux for lfa1-stcd3.

  clear: tg_zib_nfe_dist_ter[], tg_zib_cte_dist_ter[], it_saida[], it_zib_nfe_forn_aux[], it_zib_nfe_forn[], it_lfa1[].

  data: vstatus(3) type c,
        linhas     type i.

  if p_list is initial.
    vstatus = ' 123'.
  else.
    concatenate p_list p_list p_list into vstatus.
  endif.

  if p_stcd3[] is not initial.

    r_stcd3_aux[] = p_stcd3[].

    read table r_stcd3_aux index 1.
    r_stcd3_aux-option = 'CP'.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = r_stcd3_aux-low
      importing
        output = r_stcd3_aux-low.

    r_stcd3_aux-low = '*' && r_stcd3_aux-low.
    modify r_stcd3_aux index 1.
  endif.

*** US #182015 - MMSILVA - 16.05.2025 - Ini ***
  if p_lifnr[] is not initial.
    select stcd2, stcd3
      from lfa1
      into table @data(lt_lifnr)
      where lifnr in @p_lifnr.

    clear: p_lifnr[].

    loop at lt_lifnr into data(ls_lifnr).
      p_lifnr-sign = 'I'.
      p_lifnr-option = 'EQ'.
      if ls_lifnr-stcd2 is not initial.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = ls_lifnr-stcd2
          importing
            output = p_lifnr-low.
      else.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = ls_lifnr-stcd3
          importing
            output = p_lifnr-low.
      endif.
      append p_lifnr to p_cnpj[].
    endloop.
  endif.

  if p_cfop is not initial.
    select distinct a~bukrs, a~branch, a~nu_chave_cnpj, a~nu_chave_modelo, a~nu_chave_serie,
                    a~nu_chave_numero, a~dt_emissao, a~st_nota, a~nu_ie,
                    a~nu_chave, a~docnum, a~docnum_ref
      from zib_nfe_forn as a
      left join zib_nfe_dist_itm as b
        on b~chave_nfe = a~nu_chave
      left join zib_cte_dist_ter as c
        on c~cd_chave_cte = a~nu_chave_numero
      where a~bukrs           in @p_bukrs
        and a~branch          in @p_branch
        and a~nu_chave_cnpj   in @p_cnpj
        and a~dt_emissao      in @p_data
        and a~nu_chave_numero in @p_nota
        and a~nu_ie           in @r_stcd3_aux
        and a~nu_chave        in @p_chave
        and ( b~prod_cfop in @p_cfop or c~codg_cfop in @p_cfop )
      into table @it_zib_nfe_forn_aux.
  else.
*** US #182015 - MMSILVA - 16.05.2025 - Fim ***
    if p_list ne '1'.
      select bukrs branch nu_chave_cnpj nu_chave_modelo nu_chave_serie nu_chave_numero dt_emissao st_nota nu_ie
             nu_chave docnum docnum_ref
        from zib_nfe_forn
        into table it_zib_nfe_forn_aux
        where bukrs in p_bukrs
        and   branch in p_branch
        and nu_chave_cnpj in p_cnpj
        and dt_emissao      in p_data
        and nu_chave_numero in p_nota
        and nu_ie           in r_stcd3_aux
        and nu_chave        in p_chave.
    else.
      select bukrs branch nu_chave_cnpj nu_chave_modelo nu_chave_serie nu_chave_numero dt_emissao st_nota nu_ie
           nu_chave docnum docnum_ref
      from zib_nfe_forn
      into table it_zib_nfe_forn_aux
      where bukrs in p_bukrs
      and   branch in p_branch
      and nu_chave_cnpj in p_cnpj
      and dt_emissao      in p_data
      and nu_chave_numero in p_nota
      and nu_ie           in r_stcd3_aux
      and nu_chave        in p_chave
      and not exists ( select * from zib_nfe_forn as canc
                       where canc~nu_chave_cnpj  = zib_nfe_forn~nu_chave_cnpj
                       and   canc~nu_chave_serie = zib_nfe_forn~nu_chave_serie
                       and   canc~nu_chave_numero = zib_nfe_forn~nu_chave_numero
                       and   canc~st_nota         = '2' ).
    endif.


  endif.



  delete it_zib_nfe_forn_aux where st_nota na vstatus.

  if it_zib_nfe_forn_aux[] is not initial.

*** US #182015 - MMSILVA - 16.05.2025 - Ini ***
    if p_pass is not initial.
      select zib_nfe_forn~*
          from zib_nfe_forn
           inner join j_1bnfdoc
           on  j_1bnfdoc~docnum = zib_nfe_forn~docnum
           and j_1bnfdoc~pstdat > @p_data_b
           into corresponding fields of table @it_zib_nfe_forn_dep
           where zib_nfe_forn~bukrs in @p_bukrs
           and   zib_nfe_forn~branch in @p_branch
           and   zib_nfe_forn~nu_chave_cnpj   in @p_cnpj
           and   zib_nfe_forn~dt_emissao      in @p_data
           and   zib_nfe_forn~nu_chave_numero in @p_nota
           and   zib_nfe_forn~nu_ie           in @r_stcd3_aux
           and   zib_nfe_forn~nu_chave        in @p_chave.
      perform f_passivo_omisso.
      check it_zib_nfe_forn_aux[] is not initial.
    endif.

    perform f_manifest_select.
*** US #182015 - MMSILVA - 16.05.2025 - Fim ***

    select * from j_1bnfe_active
      into table it_j_1bnfe_active
      for all entries in it_zib_nfe_forn_aux
      where docnum eq it_zib_nfe_forn_aux-docnum_ref.

    select *
      from zib_nfe_dist_ter into table tg_zib_nfe_dist_ter
      for all entries in it_zib_nfe_forn_aux
     where chave_nfe = it_zib_nfe_forn_aux-nu_chave.

    select *
      from zib_cte_dist_ter into table tg_zib_cte_dist_ter
      for all entries in it_zib_nfe_forn_aux
     where cd_chave_cte = it_zib_nfe_forn_aux-nu_chave.

  endif.

  describe table it_zib_nfe_forn_aux lines linhas.
  if linhas > 0.
    loop at it_zib_nfe_forn_aux into wa_zib_nfe_forn_aux.

      wa_zib_nfe_forn-nu_chave_cnpj_aux = wa_zib_nfe_forn_aux-nu_chave_cnpj.
      wa_zib_nfe_forn-bukrs             = wa_zib_nfe_forn_aux-bukrs. "
      wa_zib_nfe_forn-branch            = wa_zib_nfe_forn_aux-branch. "
      wa_zib_nfe_forn-nu_chave_cnpj     = wa_zib_nfe_forn_aux-nu_chave_cnpj. "
      wa_zib_nfe_forn-nu_chave_modelo   = wa_zib_nfe_forn_aux-nu_chave_modelo.
      wa_zib_nfe_forn-nu_chave_serie    = wa_zib_nfe_forn_aux-nu_chave_serie. "
      wa_zib_nfe_forn-nu_chave_numero   = wa_zib_nfe_forn_aux-nu_chave_numero.
      wa_zib_nfe_forn-nu_ie             = wa_zib_nfe_forn_aux-nu_ie. "
      wa_zib_nfe_forn-dt_emissao        = wa_zib_nfe_forn_aux-dt_emissao. "
      wa_zib_nfe_forn-st_nota           = wa_zib_nfe_forn_aux-st_nota. "
      wa_zib_nfe_forn-nu_chave          = wa_zib_nfe_forn_aux-nu_chave. "
      wa_zib_nfe_forn-docnum            = wa_zib_nfe_forn_aux-docnum.
      wa_zib_nfe_forn-docnum_ref        = wa_zib_nfe_forn_aux-docnum_ref.

      perform f_tratar_ie using wa_zib_nfe_forn-nu_ie.

      append wa_zib_nfe_forn to it_zib_nfe_forn.
    endloop.
*Inicio de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
*    SELECT name1 stcd1 stcd3 lifnr
    select name1 stcd1 stcd2 stcd3 lifnr regio pstlz txjcd "US #182015 - MMSILVA - 16.06.2025 - Acrescentado REGIO, PSTLZ e TXJCD
*Fim de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
     from lfa1
     into table it_lfa1
     for all entries in it_zib_nfe_forn
*Inicio de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
*    WHERE stcd1 EQ it_zib_nfe_forn-nu_chave_cnpj_aux.
     where ( stcd1 eq it_zib_nfe_forn-nu_chave_cnpj_aux
          or stcd2 eq it_zib_nfe_forn-nu_chave_cnpj_aux(11) ) .
*Fim de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
    "Ajustar IE Fornecedores
    loop at it_lfa1 into wa_lfa1.
      data(_tabix) = sy-tabix.
      data(_delete) = ''.
      try.
          zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = wa_lfa1-lifnr
          )->ck_ativo( ).
        catch zcx_parceiros into data(ex_parceiros_k).
          _delete = 'X'.
      endtry.

      perform f_tratar_ie using wa_lfa1-stcd3.

      if _delete is not initial.
        delete it_lfa1 index _tabix.
      else.
        modify it_lfa1 from wa_lfa1.
      endif.
    endloop.

    " LP - USER STORY 78021
    "status de manifesto.

    select *
      from zsdt0127 into table @data(tg_zsdt0127)
      for all entries in @it_zib_nfe_forn
     where chave = @it_zib_nfe_forn-nu_chave.
    sort  tg_zsdt0127 by dt_atualizado hr_atualizado.

    tg_zsdt0127_aut[] = tg_zsdt0127[].

**======================================================= Atendimento IR163416 / Para atender fiscal ( João Haagsma ) para mostrar o status mesmo não estando autorizado.
*    DELETE tg_zsdt0127_aut WHERE autorizado EQ abap_false.
**======================================================= Atendimento IR163416 / Para atender fiscal ( João Haagsma ) para mostrar o status mesmo não estando autorizado.

    sort tg_zsdt0127_aut by dt_atualizado hr_atualizado.


    "CHECK NOT IT_LFA1[] IS INITIAL.
    perform: f_saida.

  else.
    message s000(z01) with 'Registro não encontrado!'.
    stop.
  endif.
endform.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
form f_saida .

  data: v_stcd3 type stcd3.

** PBI - 65246 - Inicio - CSB
  data: lc_numnf      type  j_1bnfnum9,
        lc_dt_emissao type  zde_dt_emissao,
        lc_serie      type  j_1bseries,
        lc_cnpj       type  stcd1,
        lc_cpf        type  stcd2.
  data: c_retorno type  zde_retorno_proc.
  data: it_j_1bnfdoc type table of j_1bnfdoc.

  if it_zib_nfe_forn[] is not initial.
    select *
      from j_1bnfdoc
      into table it_j_1bnfdoc
      for all entries in it_zib_nfe_forn
      where docnum = it_zib_nfe_forn-docnum.
  endif.
** PBI - 65246 - Fim - CSB

  sort: it_lfa1 by stcd1 stcd3.
  sort: it_zib_nfe_forn by docnum.
  sort: it_j_1bnfdoc by docnum.

  loop at it_zib_nfe_forn into wa_zib_nfe_forn.

    clear: wa_saida, tg_xml, tg_xml_cte..

    wa_saida-bukrs           = wa_zib_nfe_forn-bukrs.
    wa_saida-branch          = wa_zib_nfe_forn-branch.
    wa_saida-nu_chave_cnpj   = wa_zib_nfe_forn-nu_chave_cnpj.
    wa_saida-nu_chave_modelo = wa_zib_nfe_forn-nu_chave_modelo.
    wa_saida-nu_chave_serie  = wa_zib_nfe_forn-nu_chave_serie.
    wa_saida-nu_chave_numero = wa_zib_nfe_forn-nu_chave_numero.
    wa_saida-dt_emissao      = wa_zib_nfe_forn-dt_emissao.
    wa_saida-st_nota         = wa_zib_nfe_forn-st_nota.
    wa_saida-docnum          = wa_zib_nfe_forn-docnum.
    read table it_j_1bnfdoc into data(w_nota) with key docnum = wa_zib_nfe_forn-docnum binary search.
    if sy-subrc = 0.
      wa_saida-pstdat = w_nota-pstdat.
    endif.
    wa_saida-nu_chave        = wa_zib_nfe_forn-nu_chave.
    wa_saida-nu_ie           = wa_zib_nfe_forn-nu_ie.


    if wa_zib_nfe_forn-nu_chave is not initial.
      try.
          "Buscar informações do XML NFE / GRC.
          call function 'Z_DETALHAMENTO_NFE'
            exporting
              i_chave_nfe = wa_zib_nfe_forn-nu_chave   " Chave de Documento Fiscal Eletrônico
            importing
              e_xml_nfe   = tg_xml   " Estrutura tipo tabela XML NF-e SEFAZ Autorizado
            exceptions
              no_found    = 1
              others      = 2.
          if sy-subrc eq 0.


            case tg_xml-nfeproc-nfe-infnfe-ide-tpnf.
              when 0.
                wa_saida-tpnf = |0 - Entrada|.
              when 1.
                wa_saida-tpnf = |1 - Saida|.
              when others.
            endcase.


            case tg_xml-nfeproc-nfe-infnfe-ide-finnfe.
              when '1'.
                wa_saida-finnfe = |1 - NFe normal|.
              when '2'.
                wa_saida-finnfe  = |2 - NFe complementar|.

              when '3'.
                wa_saida-finnfe  = |3 - NFe de ajuste|.

              when '4'.
                wa_saida-finnfe  = |4 - Devolução de mercadoria|.
              when others.
            endcase.
          endif.
        catch cx_sy_dyn_call_param_not_found.
      endtry.

      try .
          "Buscar informações do XML CTE / GRC .
          call function 'Z_DETALHAMENTO_CTE'
            exporting
              i_chave_nfe = wa_zib_nfe_forn-nu_chave   " Chave de Documento Fiscal Eletrônico
            importing
              e_xml_cte   = tg_xml_cte   " Estrutura tipo tabela XML NF-e SEFAZ Autorizado
            exceptions
              no_found    = 1
              others      = 2.
          if sy-subrc eq 0.

            case tg_xml_cte-cteproc-cte-infcte-ide-tpcte.
              when 0.
                wa_saida-tpcte = |0 - CT-e Normal|.
              when 1.
                wa_saida-tpcte = |1 - CT-e de Compl.valores|.
              when 2.
                wa_saida-tpcte = |2 - CT-e de Anulação|.
              when 3.
                wa_saida-tpcte = |3 - CT-e de Substituição|.

              when others.
            endcase.


            case tg_xml_cte-cteproc-cte-infcte-ide-modal.
              when 1.
                wa_saida-modal = |1 - Rodoviário|.
              when 2.
                wa_saida-modal = |2 - Aéreo|.
              when 3.
                wa_saida-modal = |3 - Aquaviário|.
              when 4.
                wa_saida-modal = |4 - Ferroviário|.
              when 5.
                wa_saida-modal = |5 - Dutoviário|.
              when 6.
                wa_saida-modal = |6 - Multimodal|.

              when others.
            endcase.

            case tg_xml_cte-cteproc-cte-infcte-ide-tpserv.
              when '0'.
                wa_saida-tpserv  = |0 - Normal|.
              when '1'.
                wa_saida-tpserv  = |1 - Subcontratação|.
              when '2'.
                wa_saida-tpserv  = |2 - Redespacho|.

              when '3'.
                wa_saida-tpserv  = |3 - Redespacho Intermediário|.

              when '4'.
                wa_saida-tpserv  = |4 - Serviço Vinc.Multimodal|.
              when others.
            endcase.
          endif.
        catch cx_sy_dyn_call_param_not_found.

      endtry.
    endif.

    read table it_lfa1 into wa_lfa1 with key stcd1 = wa_zib_nfe_forn-nu_chave_cnpj_aux
                                             stcd3 = wa_zib_nfe_forn-nu_ie binary search.
    if ( sy-subrc is initial ).
      wa_saida-name1      = wa_lfa1-name1.
      wa_saida-stcd1      = wa_lfa1-stcd1.
      wa_saida-lifnr      = wa_lfa1-lifnr.
      wa_saida-uf_forn    = wa_lfa1-regio. "US #182015 - MMSILVA - 16.06.2025
      wa_saida-pstlz_forn = wa_lfa1-pstlz. "US #182015 - MMSILVA - 16.06.2025
      wa_saida-txjcd_forn = wa_lfa1-txjcd. "US #182015 - MMSILVA - 16.06.2025
    else.

*Inicio de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
      read table it_lfa1 into wa_lfa1 with key stcd2 = wa_zib_nfe_forn-nu_chave_cnpj_aux(11)
                                               stcd3 = wa_zib_nfe_forn-nu_ie .
      if sy-subrc eq 0.
        wa_saida-name1      = wa_lfa1-name1.
        wa_saida-stcd1      = wa_lfa1-stcd1.
        wa_saida-lifnr      = wa_lfa1-lifnr.
        wa_saida-uf_forn    = wa_lfa1-regio. "US #182015 - MMSILVA - 16.06.2025
        wa_saida-pstlz_forn = wa_lfa1-pstlz. "US #182015 - MMSILVA - 16.06.2025
        wa_saida-txjcd_forn = wa_lfa1-txjcd. "US #182015 - MMSILVA - 16.06.2025
      endif.
*Fim de alteração - FERNANDA.MARTINSFM - RMNI - CS0995026 - 03/06/2022
      if (
           ( ( wa_zib_nfe_forn-nu_chave_serie  ge '890' ) and ( wa_zib_nfe_forn-nu_chave_serie le '899' ) ) or
           ( ( wa_zib_nfe_forn-nu_chave_serie  ge '900' ) and ( wa_zib_nfe_forn-nu_chave_serie le '999' ) )
         )
        and ( wa_zib_nfe_forn-nu_ie is not initial ) and ( wa_zib_nfe_forn-nu_ie ne 'ISENTO' ).

        v_stcd3 = wa_zib_nfe_forn-nu_ie.
        try.
            zcl_fornecedores=>zif_parceiros~get_instance(
            )->set_parceiro_ie( i_insc_estatual = v_stcd3
            )->get_id_parceiro( importing e_parceiro =  wa_saida-lifnr
            )->get_name( importing e_name =  wa_saida-name1 ).
          catch zcx_parceiros into data(ex_parceiros_k).
        endtry.
      endif.
    endif.

    if ( wa_zib_nfe_forn-nu_ie is not initial ) and ( p_stcd3-low is not initial ).
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_zib_nfe_forn-nu_ie
        importing
          output = wa_zib_nfe_forn-nu_ie.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = p_stcd3-low
        importing
          output = v_stcd3.

      if wa_zib_nfe_forn-nu_ie <> v_stcd3.
        continue.
      endif.
    endif.

    if wa_saida-st_nota ca '23'.
      wa_saida-line_color  = 'C610'.
    endif.

    "Check Distribuição Registro
    case wa_saida-nu_chave_modelo.
      when '55'.
        read table tg_zib_nfe_dist_ter with key chave_nfe    = wa_zib_nfe_forn-nu_chave.
        if sy-subrc = 0.
          wa_saida-data_bipagem = tg_zib_nfe_dist_ter-data_bipagem.
          wa_saida-hora_bipagem = tg_zib_nfe_dist_ter-hora_bipagem.
          wa_saida-usuario_bipagem = tg_zib_nfe_dist_ter-usuario_bipagem.
        endif.

      when '57'.
        read table tg_zib_cte_dist_ter with key cd_chave_cte = wa_zib_nfe_forn-nu_chave.
    endcase.

    if sy-subrc = 0.
      wa_saida-st_distrib = icon_led_green.
    else.
      wa_saida-st_distrib = icon_led_red.
    endif.

    if wa_saida-nu_chave_modelo eq '67'. "CT-e O.S
      wa_saida-st_distrib = icon_led_yellow.
    endif.

    if wa_saida-lifnr is initial.
      wa_saida-st_distrib = icon_led_inactive.
    endif.

*** PBI - 65246 - CSB - Inicio
    if wa_zib_nfe_forn-docnum_ref is not initial.

      wa_saida-docnum_r = wa_zib_nfe_forn-docnum_ref.

      select single candat from j_1bnfdoc
            into @data(lva_candat)
            where docnum = @wa_zib_nfe_forn-docnum_ref.
      if sy-subrc = 0.
        if lva_candat is not initial.
          update zib_nfe_forn set docnum_ref = ''
               where nu_chave eq wa_saida-nu_chave.
          wa_saida-docnum_r = ''.
        endif.
      endif.

*133286 - Incluir campo Chave doc ref | ITSOUZA
      read table it_j_1bnfe_active into data(wa_j_1bnfe_active) with key docnum = wa_saida-docnum_r.
      if sy-subrc eq 0.
        concatenate wa_j_1bnfe_active-regio
                    wa_j_1bnfe_active-nfyear
                    wa_j_1bnfe_active-nfmonth
                    wa_j_1bnfe_active-stcd1
                    wa_j_1bnfe_active-model
                    wa_j_1bnfe_active-serie
                    wa_j_1bnfe_active-nfnum9
                    wa_j_1bnfe_active-docnum9
                    wa_j_1bnfe_active-cdv
                    into wa_saida-doc_ref_key.
      endif.

    endif.
*** PBI - 65246 - CSB - Fim


    "USER STORY 78021
    wa_saida-cd_operacao =  '000000'.
    loop at tg_zsdt0127_aut where chave         eq wa_saida-nu_chave
                                  and autorizado    eq 'X'
                                  and doc_manifesto is not initial.

      wa_saida-cd_operacao  = tg_zsdt0127_aut-cd_operacao.
      wa_saida-msg_retorno  = tg_zsdt0127_aut-msg_retorno.
      wa_saida-authcode = tg_zsdt0127_aut-authcode.
      wa_saida-autorizado = tg_zsdt0127_aut-autorizado.
      wa_saida-msg_retorno = tg_zsdt0127_aut-msg_retorno.

    endloop.

* ----> CS1129676 / IR147654 --->
    if wa_saida-cd_operacao =  '000000'.
      perform f_atualiza_fiscal.
    endif.
* <---- CS1129676 / IR147654 <---


    perform f_get_ds_manifesto using wa_saida-cd_operacao
                          changing wa_saida-ds_operacao.



*** US #182015 - MMSILVA - 16.06.2025 - Ini ***
    if p_pass is not initial.
      perform f_select_passivo using wa_saida changing wa_saida.
    endif.
*** US #182015 - MMSILVA - 16.06.2025 - Fim ***

                                                            "US190660
    wa_saida-nro_dias    = 0.
    clear wa_saida-dt_prevista.
    wa_saida-omisso       = 'Não'.

    select single *
      from zfit259
      into @data(w259)
      where uf_orig = @wa_saida-uf_forn
      and   uf_dest = @wa_saida-uf_dest.

    if sy-subrc = 0.
      wa_saida-nro_dias    = w259-nro_dias.
      wa_saida-dt_prevista = w259-nro_dias + wa_saida-dt_emissao.
      if wa_saida-dt_emissao+4(2) = wa_saida-dt_prevista+4(2) or wa_saida-dt_prevista lt p_data_b.
        wa_saida-omisso       = 'Sim'.
      endif.
    endif.
                                                            "US190660

    append wa_saida to it_saida.

    clear: wa_saida, wa_zib_nfe_forn, wa_lfa1, tg_xml, tg_xml_cte.
  endloop.



endform.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
form f_alv.
  perform       f_build_layout.

  if p_pass is initial. "US #182015 - MMSILVA - 16.06.2025
    perform alv_preenche_cat using:
        'BUKRS'           text-010 '7'  '' ''  '' '' '', " Empresa
        'LIFNR'           text-018 '10'  '' '' '' '' '', " Fornecedor
        'NAME1'           text-011 '35' '' ''  '' '' '', " Nome
        'BRANCH'          text-012 '6'  '' ''  '' '' '', " Filial
        'NU_CHAVE_CNPJ'   text-013 '14' '' ''  '' '' '', " CNPJ/CPF
        'NU_IE'           text-022 '14' '' ''  '' '' '', " IE
        'NU_CHAVE_MODELO' text-014 '6'  '' ''  '' '' '', " MODELO
        'NU_CHAVE_NUMERO' text-015 '9'  '' 'X' '' '' '', " NUMERO
        'NU_CHAVE_SERIE'  text-016 '5'  '' ''  '' '' '', " SERIE
        'DT_EMISSAO'      text-017 '13' '' ''  '' '' '', " DATA DE EMISSÃO
        'ST_NOTA'         text-019 '10'  '' '' '' '' '', " STATUS NOTA
        'DOCNUM'          text-021 '10'  '' '' '' '' 'X', " DOCNUM
        'PSTDAT'          text-045 '10'  '' '' '' '' 'X', " DOCNUM
        'DOCNUM_R'        text-024 '10'  '' '' '' '' '', " DOC. REF
        'ST_DISTRIB'      text-020 '10'  '' '' '' 'C' '', " ST_DISTRIB
        'CD_OPERACAO'     text-030 '44'  '' '' '' '' '', " Codigo Evento Manifesto
        'DS_OPERACAO'     text-031 '44'  '' '' '' '' '', " Status Manifesto
        'AUTHCODE'       text-032 '44'  '' '' '' '' '', " Protocolo Manifesto
        'MSG_RETORNO'     text-033 '44'  '' '' '' '' '', " Protocolo Manifesto
        'AUTORIZADO'      text-034 '44'  '' '' '' '' '', " Protocolo Manifesto
        'NU_CHAVE'        text-023 '44'  '' '' '' '' '', " Chave
        'TPNF'            text-025 '20'  '' '' '' '' '', " Tipo de nota fiscal
        'FINNFE'          text-026 '20'  '' '' '' '' '', " Finalidade nota fiscal
        'TPSERV'          text-027 '20'  '' '' '' '' '', " Tipo de serviço
        'TPCTE'           text-028 '20'  '' '' '' '' '', " Tipo CT-e
        'MODAL'           text-029 '20'  '' '' '' '' '', " Modal
        'DOC_REF_KEY'     text-035 '44'  '' '' '' '' ''. " Chave Doc.Ref.
*** US #182015 - MMSILVA - 16.06.2025 - Ini ***
  else.
    perform alv_preenche_cat using:
        'BUKRS'           text-010 '7'  ''  ''  '' '' '',   " Empresa
        'LIFNR'           text-018 '10' ''  ''  '' '' '',   " Fornecedor
        'NAME1'           text-011 '35' ''  ''  '' '' '',   " Nome
        'PSTLZ_FORN'      text-041 '15' ''  ''  '' '' '',   " CEP Fornecedor
        'TXJCD_FORN'      text-042 '22' ''  ''  '' '' '',   " Domicílio Fiscal Fornecedor
        'BRANCH'          text-012 '6'  ''  ''  '' '' '',   " Filial
        'UF_DEST'         text-037 '4'  ''  ''  '' '' '',   " UF Destinatário
        'NU_CHAVE_CNPJ'   text-013 '14' ''  ''  '' '' '',   " CNPJ/CPF
        'NU_IE'           text-022 '14' ''  ''  '' '' '',   " IE
        'UF_FORN'         text-038 '4'  ''  ''  '' '' '',   " UF Fornecedor
        'PSTLZ_DEST'      text-043 '15' ''  ''  '' '' '',   " CEP Destinatário
        'TXJCD_DEST'      text-044 '22' ''  ''  '' '' '',   " Domicílio Fiscal Destinatário
        'NU_CHAVE_NUMERO' text-015 '9'  ''  'X' '' '' '',   " NUMERO
        'MODELO_NOTA'     text-014 '20' ''  ''  '' '' '',   " MODELO
        'NU_CHAVE_SERIE'  text-016 '5'  ''  ''  '' '' '',   " SERIE
        'DOCNUM'          text-021 '10'  '' '' '' '' 'X', " DOCNUM
        'PSTDAT'          text-045 '10'  '' '' '' '' 'X', " DOCNUM
        'CFOP'            text-036 '4'  ''  ''  '' '' '',   " CFOP
        'DT_EMISSAO'      text-017 '13' ''  ''  '' '' '',   " Data de Emissão
        'VL_TOTAL'        text-039 '13' ''  ''  '' '' '',   " Valor da Fatura
        'DS_OPERACAO'     text-031 '44' ''  ''  '' '' '',   " Status Manifesto
        'GP_MERCADORIAS'  text-040 '20' ''  ''  '' '' '',   " Grupo de Mercadorias
        'NU_CHAVE'        text-023 '44' ''  ''  '' '' '',   " Chave
        'NRO_DIAS'        text-046 '44' ''  ''  '' '' '',   " lead time
        'DT_PREVISTA'     text-047 '44' ''  ''  '' '' '',   " data prevista
        'OMISSO'          text-048 '44' ''  ''  '' '' '',   " omisso?
        'DATA_BIPAGEM'    text-049 '44' ''  ''  '' '' '',   " DATA_BIPAGEM?
        'HORA_BIPAGEM'    text-050 '44' ''  ''  '' '' '',   " HORA_BIPAGEM?
        'USUARIO_BIPAGEM' text-051 '44' ''  ''  '' '' ''.   " USUARIO_BIPAGEM?
  endif.
*** US #182015 - MMSILVA - 16.06.2025 - Fim ***



endform.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
module z_status output.

  data: tl_parametros type ustyp_t_parameters.
  data: tg_fcode type table of sy-ucomm with header line.

  clear: tg_fcode[], tl_parametros[].

  call function 'SUSR_USER_PARAMETERS_GET'
    exporting
      user_name           = sy-uname
    tables
      user_parameters     = tl_parametros
    exceptions
      user_name_not_exist = 1
      others              = 2.

  select single *
    from setleaf into @data(_wl_setleaf)
   where setname = 'MAGGI_ZFIS25_ZFC_001'
     and valfrom = @sy-uname.

  if sy-subrc ne 0.
    tg_fcode = 'ZFC_001'.
    append tg_fcode.
  endif.

  select single *
    from setleaf into _wl_setleaf
   where setname = 'MAGGI_ZFIS25_ZFC_002'
     and valfrom = sy-uname.

  if sy-subrc ne 0.
    tg_fcode = 'ZFC_002'.
    append tg_fcode.
  endif.

  select single *
    from setleaf into _wl_setleaf
   where setname = 'MAGGI_ZFIS25_ZFC_004'
     and valfrom = sy-uname.


  select single *
    from setleaf into _wl_setleaf
   where setname = 'MAGGI_ZFIS25_ZFC_005'
     and valfrom = sy-uname.

  if sy-subrc ne 0.
    tg_fcode = 'ZFC_005'.
    append tg_fcode.
  endif.


  read table tl_parametros into data(wl_parametros) with key parid = 'ZTOL_DIF_VLR_XML'.
  if sy-subrc ne 0 or sy-tcode = 'ZFIS74'.
    tg_fcode = 'TOL_DIF_VL'.
    append tg_fcode.
  endif.

  set pf-status 'FF0100' excluding tg_fcode.
  if sy-tcode = 'ZFIS74'.
    set titlebar  'TB0101'.
  else.
    set titlebar  'TB0100'.
  endif.
endmodule.                 " Z_STATUS  OUTPUT

class lcl_event_receiver definition deferred.
data wa_event type ref to lcl_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
class lcl_event_receiver definition.

  public section.
    methods:

      zm_handle_hotspot for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no,

      zm_handle_toolbar for event toolbar of cl_gui_alv_grid
        importing
          e_object e_interactive,

      zm_handle_user_command for event user_command of cl_gui_alv_grid
        importing
          e_ucomm,

      zm_handle_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.


endclass.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

  method: zm_handle_hotspot.
    perform z_handle_hotspot using    e_row_id
                                      e_column_id
                                      es_row_no.
  endmethod.                    "zm_handle_hotspot

  method zm_handle_toolbar.
*   Incluindo Botão ALV
    perform z_handle_toolbar using e_object
                                   e_interactive.
  endmethod.                    "zm_handle_toolbar

  method zm_handle_user_command.
*   User Command Botões Incluidos
    perform z_handle_command using e_ucomm.
  endmethod.                    "zm_handle_user_command

  method zm_handle_double_click.
    check e_row-rowtype(1) eq space.
    check e_row-index is not initial. "US #182015 - MMSILVA - 20.06.2025 - Correção DUMP
    data(wa_saida) = it_saida[ e_row ].

    case e_column-fieldname.
      when 'DOCNUM'.
        check ( sy-subrc = 0 ) and ( wa_saida-docnum is not initial ).
        set parameter id 'JEF'  field wa_saida-docnum.
        call transaction 'J1B3N' and skip first screen.

    endcase.
  endmethod.
endclass.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
module z_exibe_alv output.

  if wa_alv is initial.

    perform f_alv.

    if wa_cont is initial and not cl_gui_alv_grid=>offline( ).

      create object wa_cont
        exporting
          container_name              = 'CC_ALV'
        exceptions
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          others                      = 6.
    endif.

    if ( wa_cont is not initial ) or cl_gui_alv_grid=>offline( ).

      create object wa_alv
        exporting
          i_parent          = wa_cont
        exceptions
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5.
    endif.

    if wa_event is initial.

      create object wa_event.
      set handler: wa_event->zm_handle_hotspot for wa_alv.
      set handler: wa_event->zm_handle_toolbar for wa_alv.
      set handler: wa_event->zm_handle_user_command for wa_alv.
      set handler: wa_event->zm_handle_double_click for wa_alv.


    endif.

    wa_layout-sel_mode = 'A'.
    wa_layout-cwidth_opt = abap_true.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.


    call method wa_alv->set_table_for_first_display
      exporting
        is_layout                     = wa_layout
        i_structure_name              = 'IT_SAIDA'
      changing
        it_outtab                     = it_saida
        it_fieldcatalog               = it_fcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  else.
    call method wa_alv->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  set handler lcl_event_handler_0100->handle_hotspot_click for wa_alv.

endmodule.                 " Z_EXIBE_ALV  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
module z_user_command input.
  if sy-dynnr eq '0100'.
    case sy-ucomm.
      when 'REFRESH'.

        perform: f_seleciona_dados.

        call method wa_alv->refresh_table_display
          exporting
            is_stable = wa_stable.

      when 'TOL_DIF_VL'.
        perform f_tol_dif_vlr.
      when 'ZFC_001'.
        perform f_atualiza_chave_doc_ent.
      when 'ZFC_002'.
        perform f_reenvia_dist_sap.
      when 'ZFC_003'.
        perform f_atualiza_chave_doc_ent2.
      when 'ZFC_004'.
        ."'ZSDT0174'.
        call transaction 'ZFIS59'.
      when 'ZFC_005'.
        perform f_download_pdf_by_excel.
      when 'BACK' or
           'CANC' or
           'EXIT'  .
        leave to screen 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
      when 'DANFE'.
        perform f_imprimir_danfe.

*-CS2020001253 - 19.07.2021 - JT - Inicio
      when 'VISAO_NFE'.
        perform f_detalhe_nfe.
*-CS2020001253 - 19.07.2021 - JT - fim

      when 'COMP'.

        authority-check object 'ZEDIT_COMP' id 'ACTVT' field '16'.
        if ( sy-subrc eq 0 ).
          perform z_handle_command using sy-ucomm.
        endif.

      when 'ARQPDF'.
        free: it_msg_return.
        perform donwload_pdf.

        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen   = '100'
            i_show     = ''
            i_repid    = sy-repid
          importing
            e_messagem = wa_mensagem
          tables
            it_msgs    = it_msg_return.

      when 'ARQXML'.
        free: it_msg_return.
        perform download_xml.

        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen   = '100'
            i_show     = ''
            i_repid    = sy-repid
          importing
            e_messagem = wa_mensagem
          tables
            it_msgs    = it_msg_return.

      when 'ARQXLS'.
        free: it_msg_return.
        perform download_xls.

        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen   = '100'
            i_show     = ''
            i_repid    = sy-repid
          importing
            e_messagem = wa_mensagem
          tables
            it_msgs    = it_msg_return.


      when 'SHOW_MSG'.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen   = '100'
            i_show     = 'X'
            i_repid    = sy-repid
          importing
            e_messagem = wa_mensagem
          tables
            it_msgs    = it_msg_return.
      when others.
    endcase.
  endif.
endmodule.                 " Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
form z_handle_command   using p_ucomm type syucomm.

endform.                    " Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
form z_handle_hotspot  using    p_e_row_id type lvc_s_row
                                p_e_column_id type  lvc_s_col
                                p_es_row_no type  lvc_s_roid.



endform.                    " Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
form z_handle_toolbar  using    p_e_object
                                p_e_interactive.

endform.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
form alv_preenche_cat  using   p_campo type c
                               p_desc  type c
                               p_tam   type c
                               p_hot   type c
                               p_zero  type c
                               p_sum   type c
                               p_just  type c
                               p_hotspot type c.

  data: wl_fcat type lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum   =  p_sum.
  wl_fcat-outputlen = p_tam.
  wl_fcat-just      = p_just.
  wl_fcat-hotspot   = p_hotspot.

  append wl_fcat to it_fcat.

endform.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_build_layout .
*  WA_LAYOUT-EXCP_FNAME = 'ST_NOTA'.
  wa_layout-info_fname = 'LINE_COLOR'.
endform.                    " F_BUILD_LAYOUT

form f_tratar_ie using p_ie.

  data: vl_ie_num type p.

  check p_ie is not initial.

  clear: vl_ie_num.

  replace all occurrences of  '.'  in p_ie  with '' ignoring case.
  replace all occurrences of  '/'  in p_ie  with '' ignoring case.
  replace all occurrences of  '\'  in p_ie  with '' ignoring case.
  replace all occurrences of  '-'  in p_ie  with '' ignoring case.

  condense p_ie  no-gaps.

  try.
      vl_ie_num  = p_ie.
      p_ie       = vl_ie_num.
      condense p_ie no-gaps.
    catch cx_sy_conversion_no_number.
    catch cx_sy_conversion_overflow.
  endtry.

endform.

form f_atualiza_chave_doc_ent.

  data: var_answer type c.

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows is not initial.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Confirmação'
      text_question         = 'Atualizar chave Doc. Entrada p/ o(s) registro(s) selecionado(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    importing
      answer                = var_answer
    exceptions
      text_not_found        = 1
      others                = 2.

  check var_answer eq '1'.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida into wa_saida index wa_sel_rows-index.
    check ( sy-subrc = 0 ) and ( wa_saida-docnum is not initial ) or ( wa_saida-nu_chave is not initial ).

    if wa_saida-docnum is not initial.
      call function 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
        exporting
          i_docnum = wa_saida-docnum.
    elseif wa_saida-nu_chave is not initial.
      call function 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
        exporting
          i_chave_nfe = wa_saida-nu_chave.
    endif.
  endloop.

endform.

form f_reenvia_dist_sap .

  data: var_answer     type c,
        wl_zob_ret_msg type zob_ret_msg.

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows is not initial.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Confirmação'
      text_question         = 'Reeenviar distribuição p/ o(s) registro(s) selecionado(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    importing
      answer                = var_answer
    exceptions
      text_not_found        = 1
      others                = 2.

  check var_answer eq '1'.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida into wa_saida index wa_sel_rows-index.
    check ( sy-subrc = 0 ) and ( wa_saida-nu_chave is not initial ).

    clear: wl_zob_ret_msg.

    wl_zob_ret_msg-msg_v1         = wa_saida-nu_chave.
    wl_zob_ret_msg-cd_processo    = '002'.

    call function 'Z_SOLIC_REENVIO_MSG_SAP'
      exporting
        i_zob_ret_msg = wl_zob_ret_msg.

    if sy-subrc ne 0.
      return.
    endif.
  endloop.

endform.

form f_tol_dif_vlr.

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows[] is not initial.

  if lines( it_sel_rows[] ) > 1.
    message 'Selecione somente uma linha!' type 'S'.
    exit.
  endif.

  read table it_sel_rows into wa_sel_rows index 1.

  check sy-subrc = 0.

  read table it_saida into wa_saida index wa_sel_rows-index.

  check ( sy-subrc = 0 ) and ( wa_saida-nu_chave is not initial ).

  clear: zfit0141.

  select single *
    from zfit0141 into zfit0141
   where chave = wa_saida-nu_chave.

  call screen 0101 starting at 40 10 ending at 70 02.

endform.

module status_0101 output.
  set pf-status 'PF0101'.
*  SET TITLEBAR 'xxx'.
endmodule.

module user_command_0101 input.

  data: var_answer type c.

  case sy-ucomm.
    when 'CONFIRM'.

      zfit0141-chave = wa_saida-nu_chave.

      if ( zfit0141-chave is initial ) or ( strlen( zfit0141-chave ) <> 44 ).
        message 'Chave não definida corretamente!' type 'S'.
        exit.
      endif.

      if zfit0141-tolerancia <= 0.
        message 'Tolerância deve ser maior que 0!' type 'S'.
        exit.
      endif.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente definir essa tolerância de divergência de valor para esse XML?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        importing
          answer                = var_answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check var_answer eq '1'.

      modify zfit0141 from zfit0141.
      if sy-subrc ne 0.
        rollback work.
        message 'Houve um erro ao gravar o registro!' type 'S'.
        exit.
      endif.

      clear: zfit0142.

      move-corresponding zfit0141 to zfit0142.

      select max( id_log )
        from zfit0142 into zfit0142-id_log
       where chave = zfit0141-chave.

      add 1 to zfit0142-id_log.

      if zfit0142-id_log is initial.
        rollback work.
        message 'Não foi possível determinar o identificador de Log!' type 'S'.
        exit.
      endif.

      zfit0142-dt_registro = sy-datum.
      zfit0142-hr_registro = sy-uzeit.
      zfit0142-us_registro = sy-uname.

      modify zfit0142 from zfit0142.
      if sy-subrc ne 0.
        rollback work.
        message 'Houve um erro ao gravar o registro de log!' type 'S'.
        exit.
      endif.

      leave to screen 0.

    when 'CANCEL'.
      leave to screen 0.
  endcase.

endmodule.

form f_imprimir_danfe .

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows is not initial.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida into wa_saida index wa_sel_rows-index.
    check sy-subrc = 0.

    if ( wa_saida-nu_chave_modelo ne '55' and  wa_saida-nu_chave_modelo ne '57' ).
      message 'Documento não é um DANFE/DACTE' type 'S'.
      continue.
    endif.

    try.
        zcl_doc_eletronico=>zif_doc_eletronico~imprimir_documento_aux(
          exporting
            i_chave = conv #( wa_saida-nu_chave ) ).
      catch zcx_doc_eletronico.
        raise exception type zcx_doc_eletronico
          exporting
            textid = value #( msgid = zcx_doc_eletronico=>zcx_doc_cabe_nao_enc-msgid msgno = zcx_doc_eletronico=>zcx_doc_cabe_nao_enc-msgno attr1 = conv #( wa_saida-nu_chave ) )
            msgid  = zcx_doc_eletronico=>zcx_doc_cabe_nao_enc-msgid
            msgno  = zcx_doc_eletronico=>zcx_doc_cabe_nao_enc-msgno
            msgv1  = conv #( wa_saida-nu_chave )
            msgty  = 'E'.
    endtry.
*    TRY.
*        ZCL_NFE_INBOUND=>DANFE( I_CHAVE_NFE = CONV #( WA_SAIDA-NU_CHAVE ) ).
*      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO DATA(EX_NFE_INBOUND).
*        EX_NFE_INBOUND->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
*    ENDTRY.
  endloop.


endform.

form f_atualiza_chave_doc_ent2.

  data: var_answer type c.

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows is not initial.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Confirmação'
      text_question         = 'Atualizar chave Doc. Entrada p/ o(s) registro(s) selecionado(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    importing
      answer                = var_answer
    exceptions
      text_not_found        = 1
      others                = 2.

  check var_answer eq '1'.

  loop at it_sel_rows into wa_sel_rows.
    read table it_saida into wa_saida index wa_sel_rows-index.
    check ( sy-subrc = 0 ) and ( wa_saida-nu_chave is not initial ).

    update zib_nfe_forn set docnum     = '0000000000'
                            atualizado = space
     where nu_chave eq wa_saida-nu_chave.

    call function 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
      exporting
        i_chave_nfe = wa_saida-nu_chave.

  endloop.

endform.

*-CS2020001253 - 19.07.2021 - JT - Inicio
form f_detalhe_nfe.

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows is not initial.

  describe table it_sel_rows lines data(l_lines).

  if l_lines > 1.
    message 'Selecionar apenas uma linha' type 'S'.
    exit.
  endif.

  read table it_sel_rows into wa_sel_rows index 1.
  check sy-subrc = 0.

  read table it_saida into wa_saida index wa_sel_rows-index.
  check sy-subrc = 0.

  if wa_saida-nu_chave_modelo ne '55'.
    message 'Documento não é um DANFE/DACTE' type 'S'.
    exit.
  endif.

  call function 'Z_SHOW_DETALHAMENTO_NFE'
    exporting
      i_chave_nfe = wa_saida-nu_chave
    exceptions
      no_found    = 1
      others      = 2.

  if sy-subrc <> 0.
    message 'XML não foi localizado.' type 'S'.
    exit.
  endif.

endform.
*-CS2020001253 - 19.07.2021 - JT - fim

*&---------------------------------------------------------------------*
*&      Form  DONWLOAD_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form donwload_pdf .

  data: selected_folder	type string,
        lt_pdf          type table of char80,
        tamanho         type i.

  data: r_arquivo type xstring,
        name_file type string,
        s_data    type w3mime,
        l_data    type table of w3mime.

  data: i_docnum    type j_1bdocnum,
        i_chave_nfe type string,
        e_name      type string,
        e_tipo      type string.

  data: lt_contents    type table of sdokcntbin,
        ls_contents    type sdokcntbin,
        lv_file_length type i,
        lv_flag        type c,
        lv_off         type i,
        lv_len         type i.

  data: lt_tab         type table of string,
        lv_codepage(4) type n value '1160',
        lv_html        type string,
        lv_receipt     type xstring.



  perform row_selection.

  check it_sel_rows is not initial.

  cl_gui_frontend_services=>directory_browse(
    exporting
      window_title         = 'Pasta para salvar arquivos PDF'
    changing
      selected_folder      = selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4 ).

  check sy-subrc is initial.

  loop at it_sel_rows into wa_selected_rows.
    read table it_saida into data(wa_saida) index wa_selected_rows-index.
    if sy-subrc eq 0.
      select * from zib_nfe_dist_ter into table w_zib_nfe_dist_ter where chave_nfe eq wa_saida-nu_chave.
      if w_zib_nfe_dist_ter is not initial.
        ztipo = 'T'. "Tipo terceiro
      endif.

      select * from zib_cte_dist_ter into table w_zib_cte_dist_ter where cd_chave_cte eq wa_saida-nu_chave.
      if w_zib_cte_dist_ter is not initial.
        ztipo = 'T'. "Tipo terceiro
      endif.
    else.
      continue.
    endif.

    if ztipo ne 'T'.
      "Downloado PDF Próprio.
      try .
          zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_saida-docnum
            )->set_registro( exporting i_docnum = wa_saida-docnum i_sem_bloqueio = abap_true
            )->get_pdf( importing e_pdf = data(e_pdf)
            )->get_registro( importing e_info_doc_eletronico = data(e_info_doc_eletronico)
            ).

          data(filename) = selected_folder && '\' &&
                           e_info_doc_eletronico-regio &&
                           e_info_doc_eletronico-nfyear &&
                           e_info_doc_eletronico-nfmonth &&
                           e_info_doc_eletronico-stcd1 &&
                           e_info_doc_eletronico-model &&
                           e_info_doc_eletronico-serie &&
                           e_info_doc_eletronico-nfnum9 &&
                           e_info_doc_eletronico-docnum9 &&
                           e_info_doc_eletronico-cdv &&
                           '.pdf'.

          call function 'SCMS_XSTRING_TO_BINARY'
            exporting
              buffer        = e_pdf
            importing
              output_length = tamanho
            tables
              binary_tab    = lt_pdf.

          cl_gui_frontend_services=>gui_download(
            exporting
              bin_filesize            = tamanho
              filename                = filename
              filetype                = 'BIN'
            changing
              data_tab                = lt_pdf
            exceptions
              file_write_error        = 1
              no_batch                = 2
              gui_refuse_filetransfer = 3
              invalid_type            = 4
              no_authority            = 5
              unknown_error           = 6
              header_not_allowed      = 7
              separator_not_allowed   = 8
              filesize_not_allowed    = 9
              header_too_long         = 10
              dp_error_create         = 11
              dp_error_send           = 12
              dp_error_write          = 13
              unknown_dp_error        = 14
              access_denied           = 15
              dp_out_of_memory        = 16
              disk_full               = 17
              dp_timeout              = 18
              file_not_found          = 19
              dataprovider_exception  = 20
              control_flush_error     = 21
              not_supported_by_gui    = 22
              error_no_gui            = 23
              others                  = 24
          ).

          if sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            clear: wa_msg_return.
            wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - próprio|.
            append wa_msg_return to it_msg_return.
            continue.
          endif.

        catch zcx_doc_eletronico into data(ex_doc_eletronico).    " .
*          ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
          clear: wa_msg_return.
          wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - próprio|.
          append wa_msg_return to it_msg_return.
          continue.
      endtry.

    else.
      "Downloado PDF Terceiro.

      clear: lv_len, lv_off, lv_file_length, lv_flag, ls_contents, name_file, lv_html, lv_codepage, r_arquivo, e_name.
      free: lt_contents, lt_tab.

      i_chave_nfe = conv #( wa_saida-nu_chave ).
      try .
          call function 'Z_GRC_ARQUIVO_DOC'
            exporting
*             i_docnum = i_docnum
              i_chave = i_chave_nfe
              i_tipo  = 'PDF'
            importing
              out     = r_arquivo
              e_name  = e_name.

          if sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            clear: wa_msg_return.
            wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - terceiro|.
            append wa_msg_return to it_msg_return.
            continue.
          endif.

        catch zcx_doc_eletronico into ex_doc_eletronico.    " .
          clear: wa_msg_return.
          wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - terceiro|.
          append wa_msg_return to it_msg_return.
          continue.
      endtry.

      if r_arquivo is not initial.
        clear: filename.
        filename = selected_folder && '\' &&
                             wa_saida-nu_chave  &&
                             '.pdf'.

        try.
            lv_len = xstrlen( r_arquivo ).
            lv_file_length = lv_len.

            while lv_flag is initial.

              if lv_len le 1022.
                ls_contents-line = r_arquivo+lv_off(lv_len).
                lv_flag = abap_true.
              else.
                ls_contents-line = r_arquivo+lv_off(1022).
                lv_off = lv_off + 1022.
                lv_len = lv_len - 1022.
              endif.

              append ls_contents to lt_contents.
            endwhile.

            clear: l_data.
            call function 'GUI_DOWNLOAD'
              exporting
                filename                = filename "Local onde sera gravado o arquivo.
                filetype                = 'BIN' "Definição do tipo do arquivo.
              tables
                data_tab                = lt_contents "Dados Binario do arquivo.
              exceptions
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                others                  = 22.

            if sy-subrc <> 0.
*              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              clear: wa_msg_return.
              wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - terceiro|.
              append wa_msg_return to it_msg_return.
              continue.
            endif.

          catch zcx_doc_eletronico into ex_doc_eletronico.    " .
*            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
            clear: wa_msg_return.
            wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - terceiro|.
            append wa_msg_return to it_msg_return.
            continue.
        endtry.
      else.
        clear: wa_msg_return.
        wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do pdf - terceiro|.
        append wa_msg_return to it_msg_return.
        continue.
      endif.
    endif.
    clear: w_zib_cte_dist_ter, w_zib_nfe_dist_ter, ztipo.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  DONWLOAD_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form download_xml .
  data: selected_folder	type string,
        lt_xml          type table of char80,
        tamanho         type i.

  data: r_arquivo type xstring,
        name_file type string,
        s_data    type w3mime,
        l_data    type table of w3mime.

  data: lt_contents    type table of sdokcntbin,
        ls_contents    type sdokcntbin,
        lv_file_length type i,
        lv_flag        type c,
        lv_off         type i,
        lv_len         type i.

  data: i_docnum    type j_1bdocnum,
        i_chave_nfe type string,
        e_name      type string,
        e_tipo      type string.

  data: lt_tab         type table of string,
        lv_codepage(4) type n value '1160',
        lv_html        type string,
        lv_receipt     type xstring.

  perform row_selection.

  check it_sel_rows is not initial.

  cl_gui_frontend_services=>directory_browse(
    exporting
      window_title         = 'Pasta para salvar arquivos XML'
    changing
      selected_folder      = selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4 ).


  check sy-subrc is initial.

  loop at it_sel_rows into wa_selected_rows.
    read table it_saida into data(wa_saida) index wa_selected_rows-index.
    if sy-subrc eq 0.
      select * from zib_nfe_dist_ter into table w_zib_nfe_dist_ter where chave_nfe eq wa_saida-nu_chave.
      if w_zib_nfe_dist_ter is not initial.
        ztipo = 'T'. "Tipo terceiro
      endif.

      select * from zib_cte_dist_ter into table w_zib_cte_dist_ter where cd_chave_cte eq wa_saida-nu_chave.
      if w_zib_cte_dist_ter is not initial.
        ztipo = 'T'. "Tipo terceiro
      endif.
    else.
      continue.
    endif.

    if ztipo ne 'T'.
      "Download XML Próprio.
      try .
          zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_saida-docnum
            )->set_registro( exporting i_docnum = wa_saida-docnum i_sem_bloqueio = abap_true
            )->get_xml( importing e_xml = data(e_xml)
            )->get_registro( importing e_info_doc_eletronico = data(e_info_doc_eletronico)
            ).

          data(filename) = selected_folder && '\' &&
                           e_info_doc_eletronico-regio &&
                           e_info_doc_eletronico-nfyear &&
                           e_info_doc_eletronico-nfmonth &&
                           e_info_doc_eletronico-stcd1 &&
                           e_info_doc_eletronico-model &&
                           e_info_doc_eletronico-serie &&
                           e_info_doc_eletronico-nfnum9 &&
                           e_info_doc_eletronico-docnum9 &&
                           e_info_doc_eletronico-cdv &&
                           '.xml'.

          call function 'SCMS_XSTRING_TO_BINARY'
            exporting
              buffer        = e_xml
            importing
              output_length = tamanho
            tables
              binary_tab    = lt_xml.

          cl_gui_frontend_services=>gui_download(
            exporting
              bin_filesize            = tamanho
              filename                = filename
              filetype                = 'BIN'
            changing
              data_tab                = lt_xml
            exceptions
              file_write_error        = 1
              no_batch                = 2
              gui_refuse_filetransfer = 3
              invalid_type            = 4
              no_authority            = 5
              unknown_error           = 6
              header_not_allowed      = 7
              separator_not_allowed   = 8
              filesize_not_allowed    = 9
              header_too_long         = 10
              dp_error_create         = 11
              dp_error_send           = 12
              dp_error_write          = 13
              unknown_dp_error        = 14
              access_denied           = 15
              dp_out_of_memory        = 16
              disk_full               = 17
              dp_timeout              = 18
              file_not_found          = 19
              dataprovider_exception  = 20
              control_flush_error     = 21
              not_supported_by_gui    = 22
              error_no_gui            = 23
              others                  = 24
          ).

          if sy-subrc <> 0.
            clear: wa_msg_return.
            wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - próprio|.
            append wa_msg_return to it_msg_return.
            continue.
          endif.

        catch zcx_doc_eletronico into data(ex_doc_eletronico).    " .
          clear: wa_msg_return.
          wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - próprio|.
          append wa_msg_return to it_msg_return.
          continue.
*          ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
      endtry.
    else.

*      IF wa_saida-nu_chave IS NOT INITIAL.
      i_chave_nfe = conv #( wa_saida-nu_chave ).
      "Download XML terceiro.

      clear: lv_len, lv_off, lv_file_length, lv_flag, ls_contents, name_file, lv_html, lv_codepage, r_arquivo, e_name.
      free: lt_contents, lt_tab.

      try.
          call function 'Z_GRC_ARQUIVO_DOC'
            exporting
*             i_docnum = i_docnum
              i_chave = i_chave_nfe
              i_tipo  = 'XML'
            importing
              out     = r_arquivo
              e_name  = e_name.

          if sy-subrc <> 0.
*              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            clear: wa_msg_return.
            wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - terceiro|.
            append wa_msg_return to it_msg_return.
            continue.
          endif.

        catch zcx_doc_eletronico into ex_doc_eletronico.    " .
*            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
          clear: wa_msg_return.
          wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - terceiro|.
          append wa_msg_return to it_msg_return.
          continue.
      endtry.

      if r_arquivo is not initial.

        clear: filename.
        filename = selected_folder && '\' &&
                     wa_saida-nu_chave
                     && '.xml'.
        try.
**/convert binary receipt to string
            call function 'ECATT_CONV_XSTRING_TO_STRING'
              exporting
                im_xstring  = r_arquivo
                im_encoding = '1100'
              importing
                ex_string   = lv_html.


            call function 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
              exporting
                external_name = lv_html
              importing
                sap_codepage  = lv_codepage
              exceptions
                not_found     = 1
                others        = 2.

            append lv_html to lt_tab.


            "Executando o download o arquivo no diretorio definido.
            call function 'GUI_DOWNLOAD'
              exporting
                filename                = filename "Local onde sera gravado o arquivo.
*               filetype                = 'BIN' "Definição do tipo do arquivo.
              tables
                data_tab                = lt_tab
              exceptions
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                others                  = 22.
            if sy-subrc <> 0.
*                MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              clear: wa_msg_return.
              wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - terceiro|.
              append wa_msg_return to it_msg_return.
              continue.
            endif.

          catch zcx_doc_eletronico into ex_doc_eletronico.    " .
            clear: wa_msg_return.
            wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - terceiro|.
            append wa_msg_return to it_msg_return.
            continue.
        endtry.
      else.
        clear: wa_msg_return.
        wa_msg_return-msg = |'Não foi possivél realizar o download chave -{ wa_saida-nu_chave } do xml - terceiro|.
        append wa_msg_return to it_msg_return.
        continue.
      endif.
    endif.
    clear: ztipo, w_zib_cte_dist_ter, w_zib_nfe_dist_ter.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  ROW_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form row_selection .

  data: var_answer type c.

  clear: it_sel_rows[], wa_sel_rows.

  call method wa_alv->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

endform.



form handle_hotspot_click  using    p_screen
                                    i_row_id     type lvc_s_row
                                    i_column_id  type lvc_s_col
                                    is_row_no    type lvc_s_roid.

  case p_screen.
      clear : wa_saida.
    when '0100'.
      case i_column_id.
        when 'DOCNUM'.
          read table it_saida into wa_saida index i_row_id-index.

          check ( sy-subrc = 0 ) and ( wa_saida-docnum is not initial ).
          set parameter id 'JEF'  field wa_saida-docnum.
          call transaction 'J1B3N' and skip first screen.

      endcase.

  endcase.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DS_MANIFESTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA_0100_CD_OPERACAO  text
*      <--P_WA_SAIDA_0100_DS_OPERACAO  text
*----------------------------------------------------------------------*
form f_get_ds_manifesto  using    p_wa_saida_0100_cd_operacao
                         changing p_wa_saida_0100_ds_operacao.

  case p_wa_saida_0100_cd_operacao.
    when '000000'.
      p_wa_saida_0100_ds_operacao  = 'Sem Manifesto'.
    when '210210'.
      p_wa_saida_0100_ds_operacao  = 'Ciência da Operação'.
    when '210200'.
      p_wa_saida_0100_ds_operacao  = 'Confirmação da Operação'.
    when '210240'.
      p_wa_saida_0100_ds_operacao  = 'Operação não Realizada'.
    when '210220'.
      p_wa_saida_0100_ds_operacao  = 'Desconhecimento da Operação'.
    when '610110'.
      p_wa_saida_0100_ds_operacao  = 'Desacordo de Entrega de Serviços(CT-e)'.

  endcase.


endform.

form f_download_pdf_by_excel .



  types: begin of ty_excel_data,
           url          type string,
           nome_arquivo type string,
         end of ty_excel_data.

  data: lva_excel_file type  rlgrap-filename, "VALUE 'C:\Rogerval\excel.xlsx',
        lva_pdf_dir    type string. "VALUE 'C:\Rogerval\download\'.

  data: gt_planilha   like standard table of alsmex_tabline,
        wl_planilha   like alsmex_tabline,
        lwa_row_excel type ty_excel_data.

  data: lt_pdf      type table of char80,
        lv_bin_size type i,
        gt_data     type standard table of soli,
        gs_data     like line of gt_data.

  cl_gui_frontend_services=>directory_browse(
    exporting
      window_title         = 'Pasta para salvar arquivos PDF'
    changing
      selected_folder      = lva_pdf_dir
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4 ).

  lva_pdf_dir = lva_pdf_dir && '\'.

  zcl_arquivo=>get_file_name(
    exporting
      i_titulo            = 'Selectione os arquivos para importação'
      i_default_extension = 'XLSX'
      i_multiselection    = abap_true
    importing
      e_arquivo           = data(lit_arquivo_sel)
  ).

  loop at lit_arquivo_sel into data(lwa_arquivo).

    clear: gt_planilha[].

    lva_excel_file = lwa_arquivo-filename.


*     Importar dados do Excel
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename    = lva_excel_file
        i_begin_col = 1
        i_begin_row = 2
        i_end_col   = 55
        i_end_row   = 10000
      tables
        intern      = gt_planilha.

*   Iterar pelas linhas do Excel e fazer o download dos PDFs
    loop at gt_planilha into wl_planilha.
      at new row.
        clear: lwa_row_excel.
      endat.

      case wl_planilha-col.
        when 1 or 2 or 3. "URL PDF parte 1/2/3
          lwa_row_excel-url = lwa_row_excel-url && wl_planilha-value.
        when 4. "Nome Arquivo
          lwa_row_excel-nome_arquivo = wl_planilha-value.
      endcase.

      at end of row.
        data(pdf_name) = lwa_row_excel-nome_arquivo && '.pdf'.
        data(pdf_url)  = lwa_row_excel-url.

        try.
            cl_http_client=>create_by_url( exporting  url                = pdf_url
                                           importing  client             = data(e_http)
                                           exceptions argument_not_found = 1
                                                      plugin_not_active  = 2
                                                      internal_error     = 3
                                                     ).


            call method e_http->send
              exceptions
                http_communication_failure = 1
                http_invalid_state         = 2
                http_processing_failed     = 3
                http_invalid_timeout       = 4.

            if sy-subrc is not initial.
              write: / 'PDF "', pdf_name, '" erro download.'.
              continue.
            endif.

            call method e_http->receive
              exceptions
                http_communication_failure = 1
                http_invalid_state         = 2
                http_processing_failed     = 3.

            if sy-subrc is not initial.
              write: / 'PDF "', pdf_name, '" erro download.'.
              continue.
            endif.

            data(e_data) = e_http->response->get_data( ).

            if e_data is initial.
              write: / 'PDF "', pdf_name, '" erro download.'.
              continue.
            endif.

            concatenate lva_pdf_dir  pdf_name into data(file_path).


            call function 'SCMS_XSTRING_TO_BINARY'
              exporting
                buffer        = e_data
              importing
                output_length = lv_bin_size
              tables
                binary_tab    = lt_pdf.

            cl_gui_frontend_services=>gui_download(
              exporting
                bin_filesize            = lv_bin_size
                filename                = conv #( file_path )
                filetype                = 'BIN'
              changing
                data_tab                = lt_pdf
              exceptions
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                not_supported_by_gui    = 22
                error_no_gui            = 23
                others                  = 24
            ).

          catch cx_root into data(exc).
            write: / 'Erro ao baixar PDF "', pdf_name, '":', exc->get_text( ).
        endtry.

      endat.

    endloop.

  endloop.

endform.


*&*********************************************************************
*& CS: CS2023000085 ZFIS25 - Exportar dados Excel
*& historia: 103387
*& Transação: ZFIS25
*& Report: ZFIR0021
*& Request:
*&*********************************************************************
form download_xls .

  types:
    begin of ty_dadosnf,
      chave_nfe               type      zib_nfe_dist_ter-chave_nfe,
      forne_cnpj              type      zib_nfe_dist_ter-forne_cnpj,
      forne_ie                type      zib_nfe_dist_ter-forne_ie,
      forne_razao             type      zib_nfe_dist_ter-forne_razao,
      destino_cnpj            type      zib_nfe_dist_ter-destino_cnpj,
      destino_ie              type      zib_nfe_dist_ter-destino_ie,
      numero                  type      zib_nfe_dist_ter-numero,
      dt_emissao              type      zib_nfe_dist_ter-dt_emissao,
      hr_emissao              type      zib_nfe_dist_ter-hr_emissao,
      serie                   type      zib_nfe_dist_ter-serie,
      model                   type      zib_nfe_dist_ter-model,
      regio                   type      zib_nfe_dist_ter-regio,
      nfyear                  type      zib_nfe_dist_ter-nfyear,
      nfmonth                 type      zib_nfe_dist_ter-nfmonth,
      docnum9                 type      zib_nfe_dist_ter-docnum9,
      cdv                     type      zib_nfe_dist_ter-cdv,
      ds_nat_operacao         type      zib_nfe_dist_ter-ds_nat_operacao,
      indicador_presenca      type      zib_nfe_dist_ter-indicador_presenca, "USER STORY 170650 / AOENNING
      cd_form_pag             type      zib_nfe_dist_ter-cd_form_pag,
      cd_tipo_doc             type      zib_nfe_dist_ter-cd_tipo_doc,
      cd_form_emissao         type      zib_nfe_dist_ter-cd_form_emissao,
      dt_saida                type      zib_nfe_dist_ter-dt_saida,
      hr_saida                type      zib_nfe_dist_ter-hr_saida,
      cd_fina_emissao         type      zib_nfe_dist_ter-cd_fina_emissao,
      nr_protocolo            type      zib_nfe_dist_ter-nr_protocolo,
      dt_protocolo            type      zib_nfe_dist_ter-dt_protocolo,
      hr_protocolo            type      zib_nfe_dist_ter-hr_protocolo,
      nr_ped_compra           type      zib_nfe_dist_ter-nr_ped_compra,
      nr_ctr_compra           type      zib_nfe_dist_ter-nr_ctr_compra,
      nr_fatura               type      zib_nfe_dist_ter-nr_fatura,
      vl_total_fatura         type      zib_nfe_dist_ter-vl_total_fatura,
      vl_desco_fatura         type      zib_nfe_dist_ter-vl_desco_fatura,
      vl_liquido              type      zib_nfe_dist_ter-vl_liquido,
      vl_icms_base            type      zib_nfe_dist_ter-vl_icms_base,
      vl_icms_total           type      zib_nfe_dist_ter-vl_icms_total,
      vl_icms_st_base         type      zib_nfe_dist_ter-vl_icms_st_base,
      vl_icms_st_total        type      zib_nfe_dist_ter-vl_icms_st_total,
      vl_produtos             type      zib_nfe_dist_ter-vl_produtos,
      vl_frete                type      zib_nfe_dist_ter-vl_frete,
      vl_seguro               type      zib_nfe_dist_ter-vl_seguro,
      vl_desconto             type      zib_nfe_dist_ter-vl_desconto,
      vl_ii_total             type      zib_nfe_dist_ter-vl_ii_total,
      vl_ipi_total            type      zib_nfe_dist_ter-vl_ipi_total,
      vl_pis_total            type      zib_nfe_dist_ter-vl_pis_total,
      vl_cof_total            type      zib_nfe_dist_ter-vl_cof_total,
      vl_despesas             type      zib_nfe_dist_ter-vl_despesas,
      vl_total                type      zib_nfe_dist_ter-vl_total,
      bukrs                   type      zib_nfe_dist_ter-bukrs,
      branch                  type      zib_nfe_dist_ter-branch,
      cd_msg_sefaz            type      zib_nfe_dist_ter-cd_msg_sefaz,
      docsta                  type      zib_nfe_dist_ter-docsta,
      cancel                  type      zib_nfe_dist_ter-cancel,
      inc_manual              type      zib_nfe_dist_ter-inc_manual,
      docnum_nfe              type      zib_nfe_dist_ter-docnum_nfe,
      e_tomadora              type      zib_nfe_dist_ter-e_tomadora,
      f_tomadora              type      zib_nfe_dist_ter-f_tomadora,
      p_emissor               type      zib_nfe_dist_ter-p_emissor,
      ebeln                   type      zib_nfe_dist_ter-ebeln,
      belnr                   type      zib_nfe_dist_ter-belnr,
      gjahr                   type      zib_nfe_dist_ter-gjahr,
      st_fiscal               type      zib_nfe_dist_ter-st_fiscal,
      st_fisico               type      zib_nfe_dist_ter-st_fisico,
      st_documento            type      zib_nfe_dist_ter-st_documento,
      cd_departamento         type      zib_nfe_dist_ter-cd_departamento,
      f_armazem               type      zib_nfe_dist_ter-f_armazem,
      armazem_cnpj            type      zib_nfe_dist_ter-armazem_cnpj,
      armazem_ie              type      zib_nfe_dist_ter-armazem_ie,
      armazem_razao           type      zib_nfe_dist_ter-armazem_razao,
      waerk                   type      zib_nfe_dist_ter-waerk,
      manual                  type      zib_nfe_dist_ter-manual,
      ctr_waers               type      zib_nfe_dist_ter-ctr_waers,
      ctr_wkurs               type      zib_nfe_dist_ter-ctr_wkurs,
      ctr_kufix               type      zib_nfe_dist_ter-ctr_kufix,
      ctr_sinal               type      zib_nfe_dist_ter-ctr_sinal,
      ctr_valor_total         type      zib_nfe_dist_ter-ctr_valor_total,
      ctr_zterm               type      zib_nfe_dist_ter-ctr_zterm,
      vl_icms_desonerado      type      zib_nfe_dist_ter-vl_icms_desonerado,
      xmlvers                 type      zib_nfe_dist_ter-xmlvers,
      st_armazem              type      zib_nfe_dist_ter-st_armazem,
      ck_possui_frete         type      zib_nfe_dist_ter-ck_possui_frete,
      f_transporte            type      zib_nfe_dist_ter-f_transporte,
      vbeln                   type      zib_nfe_dist_ter-vbeln,
      tknum                   type      zib_nfe_dist_ter-tknum,
      fknum                   type      zib_nfe_dist_ter-fknum,
      ck_fiscal               type      zib_nfe_dist_ter-ck_fiscal,
      ck_fisico               type      zib_nfe_dist_ter-ck_fisico,
      ck_armazem              type      zib_nfe_dist_ter-ck_armazem,
      mwskz                   type      zib_nfe_dist_ter-mwskz,
      mblnr                   type      zib_nfe_dist_ter-mblnr,
      mjahr                   type      zib_nfe_dist_ter-mjahr,
      cd_romaneio             type      zib_nfe_dist_ter-cd_romaneio,
      nr_fase                 type      zib_nfe_dist_ter-nr_fase,
      dt_vencimento           type      zib_nfe_dist_ter-dt_vencimento,
      zbvtyp                  type      zib_nfe_dist_ter-zbvtyp,
      zlspr                   type      zib_nfe_dist_ter-zlspr,
      land1                   type      zib_nfe_dist_ter-land1,
      pymt_meth               type      zib_nfe_dist_ter-pymt_meth,
      housebankid             type      zib_nfe_dist_ter-housebankid,
      pc_partiner             type      zib_nfe_dist_ter-pc_partiner,
      lr_partiner             type      zib_nfe_dist_ter-lr_partiner,
      ck_compra_futura        type      zib_nfe_dist_ter-ck_compra_futura,
      tp_compra_futura        type      zib_nfe_dist_ter-tp_compra_futura,
      vlr_desconto            type      zib_nfe_dist_ter-vlr_desconto,
      obs_financeira          type      zib_nfe_dist_ter-obs_financeira,
      transportador_cnpj      type      zib_nfe_dist_ter-transportador_cnpj,
      transportador_cpf       type      zib_nfe_dist_ter-transportador_cpf,
      boleto                  type      zib_nfe_dist_ter-boleto,
      id_simetrya             type      zib_nfe_dist_ter-id_simetrya,
      se_status               type      zib_nfe_dist_ter-se_status,
      se_code                 type      zib_nfe_dist_ter-se_code,
      se_detail               type      zib_nfe_dist_ter-se_detail,
      se_recordkey            type      zib_nfe_dist_ter-se_recordkey,
      se_recordid             type      zib_nfe_dist_ter-se_recordid,
      forne_cpf               type      zib_nfe_dist_ter-forne_cpf,
      us_miro                 type      zib_nfe_dist_ter-us_miro,
      ck_revisao              type      zib_nfe_dist_ter-ck_revisao,
      ck_trans_nf_propri      type      zib_nfe_dist_ter-ck_trans_nf_propri,
      mblnr_arm               type      zib_nfe_dist_ter-mblnr_arm,
      mjahr_arm               type      zib_nfe_dist_ter-mjahr_arm,
      docnum_arm              type      zib_nfe_dist_ter-docnum_arm,
      transportador_ie        type      zib_nfe_dist_ter-transportador_ie,
      urldanfe                type      zib_nfe_dist_ter-urldanfe,
      mblnr_dev               type      zib_nfe_dist_ter-mblnr_dev,
      mjahr_dev               type      zib_nfe_dist_ter-mjahr_dev,
      docnum_dev              type      zib_nfe_dist_ter-docnum_dev,
      belnr_dev               type      zib_nfe_dist_ter-belnr_dev,
      gjahr_dev               type      zib_nfe_dist_ter-gjahr_dev,
      ctr_valor_total_liquido type      zib_nfe_dist_ter-ctr_valor_total_liquido,
      bukrs_e                 type      zib_nfe_dist_ter-bukrs_e,
      branch_e                type      zib_nfe_dist_ter-branch_e,
      aut_embarque            type      zib_nfe_dist_ter-aut_embarque,
      placa_cav               type      zib_nfe_dist_ter-placa_cav,
      prod_item               type      zib_nfe_dist_itm-prod_item,
      prod_cfop               type      zib_nfe_dist_itm-prod_cfop,
      prod_descricao          type      zib_nfe_dist_itm-prod_descricao,
      prod_ean                type      zib_nfe_dist_itm-prod_ean,
      prod_ncm                type      zib_nfe_dist_itm-prod_ncm,
      prod_extipi             type      zib_nfe_dist_itm-prod_extipi,
      prod_ncm_genero         type      zib_nfe_dist_itm-prod_ncm_genero,
      prod_und_comerci        type      zib_nfe_dist_itm-prod_und_comerci,
      prod_qtd_comerci        type      zib_nfe_dist_itm-prod_qtd_comerci,
      prod_vlr_und_com        type      zib_nfe_dist_itm-prod_vlr_und_com,
      prod_vlr_total_b        type      zib_nfe_dist_itm-prod_vlr_total_b,
      prod_ean_trib           type      zib_nfe_dist_itm-prod_ean_trib,
      prod_und_trib           type      zib_nfe_dist_itm-prod_und_trib,
      prod_qtd_trib           type      zib_nfe_dist_itm-prod_qtd_trib,
      prod_vlr_und_tri        type      zib_nfe_dist_itm-prod_vlr_und_tri,
      prod_vl_frete           type      zib_nfe_dist_itm-prod_vl_frete,
      prod_vl_seguro          type      zib_nfe_dist_itm-prod_vl_seguro,
      prod_vl_desconto        type      zib_nfe_dist_itm-prod_vl_desconto,
      prod_vl_outro           type      zib_nfe_dist_itm-prod_vl_outro,
      prod_ind_total          type      zib_nfe_dist_itm-prod_ind_total,
      prod_pedido_comp        type      zib_nfe_dist_itm-prod_pedido_comp,
      prod_nr_ped_comp        type      zib_nfe_dist_itm-prod_nr_ped_comp,
      icms_origem_mec         type      zib_nfe_dist_itm-icms_origem_mec,
      icms_cst                type      zib_nfe_dist_itm-icms_cst,
      icms_md_base            type      zib_nfe_dist_itm-icms_md_base,
      icms_base               type      zib_nfe_dist_itm-icms_base,
      icms_aqt                type      zib_nfe_dist_itm-icms_aqt,
      icms_red_base           type      zib_nfe_dist_itm-icms_red_base,
      icms_valor              type      zib_nfe_dist_itm-icms_valor,
      icms_st_md_base         type      zib_nfe_dist_itm-icms_st_md_base,
      icms_st_margem          type      zib_nfe_dist_itm-icms_st_margem,
      icms_st_red_base        type      zib_nfe_dist_itm-icms_st_red_base,
      icms_st_aqt             type      zib_nfe_dist_itm-icms_st_aqt,
      icms_st_base            type      zib_nfe_dist_itm-icms_st_base,
      icms_st_valor           type      zib_nfe_dist_itm-icms_st_valor,
      icms_mt_desonera        type      zib_nfe_dist_itm-icms_mt_desonera,
      ipi_cod_enquadra        type      zib_nfe_dist_itm-ipi_cod_enquadra,
      ipi_cla_enquadra        type      zib_nfe_dist_itm-ipi_cla_enquadra,
      ipi_cnpj_prod           type      zib_nfe_dist_itm-ipi_cnpj_prod,
      ipi_cod_selo_con        type      zib_nfe_dist_itm-ipi_cod_selo_con,
      ipi_qtd_selo_con        type      zib_nfe_dist_itm-ipi_qtd_selo_con,
      ipi_cst                 type      zib_nfe_dist_itm-ipi_cst,
      ipi_base                type      zib_nfe_dist_itm-ipi_base,
      ipi_qtd_tributad        type      zib_nfe_dist_itm-ipi_qtd_tributad,
      ipi_vlr_unitario        type      zib_nfe_dist_itm-ipi_vlr_unitario,
      ipi_aqt                 type      zib_nfe_dist_itm-ipi_aqt,
      ipi_valor               type      zib_nfe_dist_itm-ipi_valor,
      pis_cst                 type      zib_nfe_dist_itm-pis_cst,
      pis_base                type      zib_nfe_dist_itm-pis_base,
      pis_aqt                 type      zib_nfe_dist_itm-pis_aqt,
      pis_valor               type      zib_nfe_dist_itm-pis_valor,
      pis_qtd_vendida         type      zib_nfe_dist_itm-pis_qtd_vendida,
      pis_aqt_reais           type      zib_nfe_dist_itm-pis_aqt_reais,
      pis_st_base             type      zib_nfe_dist_itm-pis_st_base,
      pis_st_aqt              type      zib_nfe_dist_itm-pis_st_aqt,
      pis_st_qtd_vendi        type      zib_nfe_dist_itm-pis_st_qtd_vendi,
      pis_st_aqt_reais        type      zib_nfe_dist_itm-pis_st_aqt_reais,
      pis_st_valor            type      zib_nfe_dist_itm-pis_st_valor,
      cof_cst                 type      zib_nfe_dist_itm-cof_cst,
      cof_base                type      zib_nfe_dist_itm-cof_base,
      cof_aqt                 type      zib_nfe_dist_itm-cof_aqt,
      cof_valor               type      zib_nfe_dist_itm-cof_valor,
      cof_qtd_vendida         type      zib_nfe_dist_itm-cof_qtd_vendida,
      cof_aqt_reais           type      zib_nfe_dist_itm-cof_aqt_reais,
      cof_st_base             type      zib_nfe_dist_itm-cof_st_base,
      cof_st_aqt              type      zib_nfe_dist_itm-cof_st_aqt,
      cof_st_qtd_vendi        type      zib_nfe_dist_itm-cof_st_qtd_vendi,
      cof_st_aqt_reais        type      zib_nfe_dist_itm-cof_st_aqt_reais,
      cof_st_valor            type      zib_nfe_dist_itm-cof_st_valor,
      itmnum_nfe              type      zib_nfe_dist_itm-itmnum_nfe,
      matnr                   type      zib_nfe_dist_itm-matnr,
      ebelp                   type      zib_nfe_dist_itm-ebelp,
      menge                   type      zib_nfe_dist_itm-menge,
      meins                   type      zib_nfe_dist_itm-meins,
      netpr                   type      zib_nfe_dist_itm-netpr,
      netwr                   type      zib_nfe_dist_itm-netwr,
      prod_codigo             type      zib_nfe_dist_itm-prod_codigo,
      belnr_ft                type      zib_nfe_dist_itm-belnr_ft,
      gjahr_ft                type      zib_nfe_dist_itm-gjahr_ft,
      buzei_ft                type      zib_nfe_dist_itm-buzei_ft,
      deliv_numb              type      zib_nfe_dist_itm-deliv_numb,
      deliv_item              type      zib_nfe_dist_itm-deliv_item,
      prod_item_origem        type      zib_nfe_dist_itm-prod_item_origem,
      zeile                   type      zib_nfe_dist_itm-zeile,
      icms_vl_desonerado      type      zib_nfe_dist_itm-icms_vl_desonerado,
      lgort                   type      zib_nfe_dist_itm-lgort,
      brtwr                   type      zib_nfe_dist_itm-brtwr,
      bicms                   type      zib_nfe_dist_itm-bicms,
      picms                   type      zib_nfe_dist_itm-picms,
      bpis                    type      zib_nfe_dist_itm-bpis,
      ppis                    type      zib_nfe_dist_itm-ppis,
      bcofins                 type      zib_nfe_dist_itm-bcofins,
      pcofins                 type      zib_nfe_dist_itm-pcofins,
      liquido                 type      zib_nfe_dist_itm-liquido,
    end of ty_dadosnf.

  data: selected_folder	type string,
        lt_pdf          type table of char80,
        tamanho         type i.

  data: r_arquivo type xstring,
        name_file type string,
        s_data    type w3mime,
        l_data    type table of w3mime.

  data: i_docnum    type j_1bdocnum,
        i_chave_nfe type string,
        e_name      type string,
        e_tipo      type string.

  data: lt_contents    type table of sdokcntbin,
        ls_contents    type sdokcntbin,
        lv_file_length type i,
        lv_flag        type c,
        lv_off         type i,
        lv_len         type i.

  data: lt_tab         type table of string,
        lv_codepage(4) type n value '1160',
        lv_html        type string,
        lv_receipt     type xstring.


  data: it_dadosnf type table of ty_dadosnf,
        wa_dadosnf type ty_dadosnf.

  types: begin of ty_data,
           fieldname type c length 1024,
         end of ty_data.

  data: lt_fieldname type standard table of ty_data,
        ls_fieldname type ty_data.

  data: ls_file_name type string.

  data lra_chave_nfe type range of zde_chave_doc_e.

  free: ls_file_name.

  perform row_selection.

  check it_sel_rows is not initial.

  cl_gui_frontend_services=>directory_browse(
    exporting
      window_title         = 'Pasta para salvar arquivos XLS'
    changing
      selected_folder      = selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4 ).

  check sy-subrc is initial.

  data: namefilexls type string.

  namefilexls = sy-repid && sy-uname && sy-datlo && sy-timlo.

  ls_file_name = selected_folder && '\' && namefilexls && '.xls'.

  loop at it_sel_rows into wa_selected_rows.

    read table  it_saida into wa_saida index wa_selected_rows-index.

    if sy-subrc is initial.

      if wa_saida-nu_chave is not initial.
        append value #( sign = 'I' option = 'EQ' low = wa_saida-nu_chave ) to lra_chave_nfe.
      endif.

    endif.

  endloop.


  if lra_chave_nfe is not initial.
    select
        a~chave_nfe
        a~forne_cnpj
        a~forne_ie
        a~forne_razao
        a~destino_cnpj
        a~destino_ie
        a~numero
        a~dt_emissao
        a~hr_emissao
        a~serie
        a~model
        a~regio
        a~nfyear
        a~nfmonth
        a~docnum9
        a~cdv
        a~ds_nat_operacao
        a~indicador_presenca "USER STORY 170650 / AOENNING
        a~cd_form_pag
        a~cd_tipo_doc
        a~cd_form_emissao
        a~dt_saida
        a~hr_saida
        a~cd_fina_emissao
        a~nr_protocolo
        a~dt_protocolo
        a~hr_protocolo
        a~nr_ped_compra
        a~nr_ctr_compra
        a~nr_fatura
        a~vl_total_fatura
        a~vl_desco_fatura
        a~vl_liquido
        a~vl_icms_base
        a~vl_icms_total
        a~vl_icms_st_base
        a~vl_icms_st_total
        a~vl_produtos
        a~vl_frete
        a~vl_seguro
        a~vl_desconto
        a~vl_ii_total
        a~vl_ipi_total
        a~vl_pis_total
        a~vl_cof_total
        a~vl_despesas
        a~vl_total
        a~bukrs
        a~branch
        a~cd_msg_sefaz
        a~docsta
        a~cancel
        a~inc_manual
        a~docnum_nfe
        a~e_tomadora
        a~f_tomadora
        a~p_emissor
        a~ebeln
        a~belnr
        a~gjahr
        a~st_fiscal
        a~st_fisico
        a~st_documento
        a~cd_departamento
        a~f_armazem
        a~armazem_cnpj
        a~armazem_ie
        a~armazem_razao
        a~waerk
        a~manual
        a~ctr_waers
        a~ctr_wkurs
        a~ctr_kufix
        a~ctr_sinal
        a~ctr_valor_total
        a~ctr_zterm
        a~vl_icms_desonerado
        a~xmlvers
        a~st_armazem
        a~ck_possui_frete
        a~f_transporte
        a~vbeln
        a~tknum
        a~fknum
        a~ck_fiscal
        a~ck_fisico
        a~ck_armazem
        a~mwskz
        a~mblnr
        a~mjahr
        a~cd_romaneio
        a~nr_fase
        a~dt_vencimento
        a~zbvtyp
        a~zlspr
        a~land1
        a~pymt_meth
        a~housebankid
        a~pc_partiner
        a~lr_partiner
        a~ck_compra_futura
        a~tp_compra_futura
        a~vlr_desconto
        a~obs_financeira
        a~transportador_cnpj
        a~transportador_cpf
        a~boleto
        a~id_simetrya
        a~se_status
        a~se_code
        a~se_detail
        a~se_recordkey
        a~se_recordid
        a~forne_cpf
        a~us_miro
        a~ck_revisao
        a~ck_trans_nf_propri
        a~mblnr_arm
        a~mjahr_arm
        a~docnum_arm
        a~transportador_ie
        a~urldanfe
        a~mblnr_dev
        a~mjahr_dev
        a~docnum_dev
        a~belnr_dev
        a~gjahr_dev
        a~ctr_valor_total_liquido
        a~bukrs_e
        a~branch_e
        a~aut_embarque
        a~placa_cav
        b~prod_item
        b~prod_cfop
        b~prod_descricao
        b~prod_ean
        b~prod_ncm
        b~prod_extipi
        b~prod_ncm_genero
        b~prod_und_comerci
        b~prod_qtd_comerci
        b~prod_vlr_und_com
        b~prod_vlr_total_b
        b~prod_ean_trib
        b~prod_und_trib
        b~prod_qtd_trib
        b~prod_vlr_und_tri
        b~prod_vl_frete
        b~prod_vl_seguro
        b~prod_vl_desconto
        b~prod_vl_outro
        b~prod_ind_total
        b~prod_pedido_comp
        b~prod_nr_ped_comp
        b~icms_origem_mec
        b~icms_cst
        b~icms_md_base
        b~icms_base
        b~icms_aqt
        b~icms_red_base
        b~icms_valor
        b~icms_st_md_base
        b~icms_st_margem
        b~icms_st_red_base
        b~icms_st_aqt
        b~icms_st_base
        b~icms_st_valor
        b~icms_mt_desonera
        b~ipi_cod_enquadra
        b~ipi_cla_enquadra
        b~ipi_cnpj_prod
        b~ipi_cod_selo_con
        b~ipi_qtd_selo_con
        b~ipi_cst
        b~ipi_base
        b~ipi_qtd_tributad
        b~ipi_vlr_unitario
        b~ipi_aqt
        b~ipi_valor
        b~pis_cst
        b~pis_base
        b~pis_aqt
        b~pis_valor
        b~pis_qtd_vendida
        b~pis_aqt_reais
        b~pis_st_base
        b~pis_st_aqt
        b~pis_st_qtd_vendi
        b~pis_st_aqt_reais
        b~pis_st_valor
        b~cof_cst
        b~cof_base
        b~cof_aqt
        b~cof_valor
        b~cof_qtd_vendida
        b~cof_aqt_reais
        b~cof_st_base
        b~cof_st_aqt
        b~cof_st_qtd_vendi
        b~cof_st_aqt_reais
        b~cof_st_valor
        b~itmnum_nfe
        b~matnr
        b~ebelp
        b~menge
        b~meins
        b~netpr
        b~netwr
        b~prod_codigo
        b~belnr_ft
        b~gjahr_ft
        b~buzei_ft
        b~deliv_numb
        b~deliv_item
        b~prod_item_origem
        b~zeile
        b~icms_vl_desonerado
        b~lgort
        b~brtwr
        b~bicms
        b~picms
        b~bpis
        b~ppis
        b~bcofins
        b~pcofins
        b~liquido
    from zib_nfe_dist_ter as a
    inner join zib_nfe_dist_itm as b on b~chave_nfe = a~chave_nfe
    into corresponding fields of table it_dadosnf
    where a~chave_nfe in lra_chave_nfe."= wa_saida-nu_chave.

  endif.



  ls_fieldname-fieldname = 'CHAVE_NFE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'FORNE_CNPJ'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'FORNE_IE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'FORNE_RAZAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DESTINO_CNPJ'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DESTINO_IE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NUMERO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DT_EMISSAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'HR_EMISSAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'SERIE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MODEL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'REGIO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NFYEAR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NFMONTH'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DOCNUM9'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CDV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DS_NAT_OPERACAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'INDICADOR_PRESENCA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname. "USER STORY 170650 / AOENNING
  ls_fieldname-fieldname = 'CD_FORM_PAG'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CD_TIPO_DOC'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CD_FORM_EMISSAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DT_SAIDA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'HR_SAIDA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CD_FINA_EMISSAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NR_PROTOCOLO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DT_PROTOCOLO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'HR_PROTOCOLO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NR_PED_COMPRA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NR_CTR_COMPRA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NR_FATURA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_TOTAL_FATURA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_DESCO_FATURA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_LIQUIDO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_ICMS_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_ICMS_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
*  ls_fieldname-fieldname = 'VL_ICMS_ST_BASE'. APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
  ls_fieldname-fieldname = 'VBCST'.           append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_ICMS_ST_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_PRODUTOS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_FRETE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_SEGURO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_DESCONTO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_II_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_IPI_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_PIS_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_COF_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_DESPESAS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BUKRS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BRANCH'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CD_MSG_SEFAZ'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DOCSTA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CANCEL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'INC_MANUAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DOCNUM_NFE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'E_TOMADORA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'F_TOMADORA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'P_EMISSOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'EBELN'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BELNR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'GJAHR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ST_FISCAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ST_FISICO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ST_DOCUMENTO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CD_DEPARTAMENTO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'F_ARMAZEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ARMAZEM_CNPJ'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ARMAZEM_IE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ARMAZEM_RAZAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'WAERK'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MANUAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_WAERS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_WKURS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_KUFIX'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_SINAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_VALOR_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_ZTERM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VL_ICMS_DESONERADO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'XMLVERS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ST_ARMAZEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_POSSUI_FRETE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'F_TRANSPORTE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VBELN'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'TKNUM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'FKNUM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_FISCAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_FISICO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_ARMAZEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MWSKZ'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MBLNR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MJAHR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CD_ROMANEIO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NR_FASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DT_VENCIMENTO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ZBVTYP'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ZLSPR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'LAND1'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PYMT_METH'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'HOUSEBANKID'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PC_PARTINER'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'LR_PARTINER'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_COMPRA_FUTURA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'TP_COMPRA_FUTURA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'VLR_DESCONTO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'OBS_FINANCEIRA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'TRANSPORTADOR_CNPJ'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'TRANSPORTADOR_CPF'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BOLETO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ID_SIMETRYA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'SE_STATUS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'SE_CODE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'SE_DETAIL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'SE_RECORDKEY'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'SE_RECORDID'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'FORNE_CPF'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'US_MIRO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_REVISAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CK_TRANS_NF_PROPRI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MBLNR_ARM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MJAHR_ARM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DOCNUM_ARM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'TRANSPORTADOR_IE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'URLDANFE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MBLNR_DEV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MJAHR_DEV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DOCNUM_DEV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BELNR_DEV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'GJAHR_DEV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'CTR_VALOR_TOTAL_LIQUIDO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BUKRS_E'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BRANCH_E'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'AUT_EMBARQUE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PLACA_CAV'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_ITEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_CFOP'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_DESCRICAO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_EAN'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_NCM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_EXTIPI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_NCM_GENERO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_UND_COMERCI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_QTD_COMERCI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VLR_UND_COM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VLR_TOTAL_B'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_EAN_TRIB'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_UND_TRIB'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_QTD_TRIB'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VLR_UND_TRI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VL_FRETE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VL_SEGURO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VL_DESCONTO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_VL_OUTRO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_IND_TOTAL'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_PEDIDO_COMP'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_NR_PED_COMP'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ORIGEM_MEC'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_CST'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_MD_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_RED_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ST_MD_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ST_MARGEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ST_RED_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ST_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ST_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_ST_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_MT_DESONERA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_COD_ENQUADRA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_CLA_ENQUADRA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_CNPJ_PROD'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_COD_SELO_CON'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_QTD_SELO_CON'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_CST'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_QTD_TRIBUTAD'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_VLR_UNITARIO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'IPI_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_CST'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_QTD_VENDIDA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_AQT_REAIS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_ST_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_ST_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_ST_QTD_VENDI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_ST_AQT_REAIS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PIS_ST_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_CST'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_QTD_VENDIDA'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_AQT_REAIS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_ST_BASE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_ST_AQT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_ST_QTD_VENDI'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_ST_AQT_REAIS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'COF_ST_VALOR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ITMNUM_NFE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MATNR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'EBELP'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MENGE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'MEINS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NETPR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'NETWR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_CODIGO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BELNR_FT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'GJAHR_FT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BUZEI_FT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DELIV_NUMB'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'DELIV_ITEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PROD_ITEM_ORIGEM'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ZEILE'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'ICMS_VL_DESONERADO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'LGORT'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BRTWR'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BICMS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PICMS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BPIS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PPIS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'BCOFINS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'PCOFINS'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.
  ls_fieldname-fieldname = 'LIQUIDO'. append ls_fieldname to lt_fieldname. clear: ls_fieldname.



  call function 'GUI_DOWNLOAD'
    exporting
      filename                = ls_file_name
      filetype                = 'DBF'
      write_field_separator   = 'X'
      trunc_trailing_blanks   = 'X'
    tables
      data_tab                = it_dadosnf
      fieldnames              = lt_fieldname
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      others                  = 22.
  if sy-subrc <> 0.
  endif.

  clear: wa_saida.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_atualiza_fiscal .
* ----> CS1102269 / IR140174 --->
* ----> CS1129676 / IR147654 --->

  select single * from zsdt0127_fiscal into w_fiscal
     where chave eq wa_saida-nu_chave
     and atualiza eq 'X'.
  if sy-subrc eq 0.
    wa_saida-cd_operacao = w_fiscal-cd_operacao.
  endif.

* ----> CS1129676 / IR147654 --->
* ----> CS1102269 / IR140174 --->


endform.

*** US #182015 - MMSILVA - 17.06.2025 - Ini ***
form f_passivo_omisso.
  if it_zib_nfe_forn_aux is not initial.
    " Deletar as notas que possuem documento
    sort it_zib_nfe_forn_dep by docnum.
    loop at it_zib_nfe_forn_aux  assigning field-symbol(<fs_forn>).
      if <fs_forn>-docnum is not initial.
        read table it_zib_nfe_forn_dep into data(w_forn) with key docnum = <fs_forn>-docnum binary search.
        if sy-subrc ne 0.
          <fs_forn>-del = 'X'.
        endif.
      endif.
    endloop.

    delete it_zib_nfe_forn_aux where del = 'X'.
*    delete it_zib_nfe_forn_aux where docnum is not initial.
    check it_zib_nfe_forn_aux[] is not initial.

*     Selecionar os CFOP's das notas encontradas
    select distinct chave_nfe
      from zib_nfe_dist_itm
      into table @data(lt_zib_nfe_dist_itm)
      for all entries in @it_zib_nfe_forn_aux
      where chave_nfe eq @it_zib_nfe_forn_aux-nu_chave
      and   prod_cfop in @p_cfop
      and   not exists ( select * from zmmt0186 where cfop = zib_nfe_dist_itm~prod_cfop ).

    select distinct cd_chave_cte
        from zib_cte_dist_ter
        into table @data(lt_zib_cte_dist_ter)
        for all entries in @it_zib_nfe_forn_aux
        where cd_chave_cte eq @it_zib_nfe_forn_aux-nu_chave
        and   codg_cfop in @p_cfop
        and   not exists ( select * from zmmt0186 where cfop = zib_cte_dist_ter~codg_cfop ).

    sort lt_zib_nfe_dist_itm by chave_nfe.
    sort lt_zib_cte_dist_ter by cd_chave_cte.
    loop at it_zib_nfe_forn_aux  assigning <fs_forn>.
      read table lt_zib_nfe_dist_itm into data(wnfe) with key chave_nfe = <fs_forn>-nu_chave binary search.
      if sy-subrc ne 0.
        read table lt_zib_cte_dist_ter into data(wcte) with key cd_chave_cte = <fs_forn>-nu_chave binary search.
        if sy-subrc ne 0.
          <fs_forn>-del = 'X'.
        endif.
      endif.
    endloop.
    delete it_zib_nfe_forn_aux where del = 'X'.

    " Inserir na LT_CHAVES_CANCELADAS as notas que possuem ST_NOTA = 2
    data lt_chaves_canceladas type standard table of zib_nfe_dist_ter-chave_nfe.
    clear lt_chaves_canceladas.
    loop at it_zib_nfe_forn_aux assigning field-symbol(<fs_zib_nfe_forn_aux>)
      where st_nota = 2.
      if not line_exists( lt_chaves_canceladas[ table_line = <fs_zib_nfe_forn_aux>-nu_chave ] ).
        append <fs_zib_nfe_forn_aux>-nu_chave to lt_chaves_canceladas.
      endif.
    endloop.

    " Deletar as notas da IT_ZIB_NFE_FORN_AUX que estão na LT_CHAVES_CANCELADAS
    loop at lt_chaves_canceladas assigning field-symbol(<fs_chaves_canceladas>).
      delete it_zib_nfe_forn_aux where nu_chave = <fs_chaves_canceladas>.
    endloop.
  endif.

endform.

form f_manifest_ranges.
  if p_mnf1 is not initial.
    append value #( sign   = 'I' option = 'EQ' low    = '000000' high   = '000000' ) to r_st_mnf.
  endif.
  if p_mnf2 is not initial.
    append value #( sign   = 'I' option = 'EQ' low    = '210210' high   = '210210' ) to r_st_mnf.
  endif.
  if p_mnf3 is not initial.
    append value #( sign   = 'I' option = 'EQ' low    = '210200' high   = '210200' ) to r_st_mnf.
  endif.

  if p_mnf4 is not initial.
    append value #( sign   = 'I' option = 'EQ' low    = '210240' high   = '210240' ) to r_st_mnf.
  endif.

  if p_mnf5 is not initial.
    append value #( sign   = 'I' option = 'EQ' low    = '210220' high   = '210220' ) to r_st_mnf.
  endif.

  if p_mnf6 is not initial.
    append value #( sign   = 'I' option = 'EQ' low    = '610110' high   = '610110' ) to r_st_mnf.
  endif.
endform.

form f_manifest_select.
  select * from zsdt0127
    into table @data(lt_zsdt0127)
    for all entries in @it_zib_nfe_forn_aux
    where chave = @it_zib_nfe_forn_aux-nu_chave.

  data lv_idx type sy-tabix.
  lv_idx = lines( it_zib_nfe_forn_aux ).

  while lv_idx > 0.
    read table it_zib_nfe_forn_aux assigning field-symbol(<fs_zib_nfe_forn_manifest>) index lv_idx.
    read table lt_zsdt0127 assigning field-symbol(<fs_zsdt0127>) with key chave = <fs_zib_nfe_forn_manifest>-nu_chave.

    " Verifica se manifesto 1 (sem manifesto) foi selecionado
    data(lv_sem_manifesto) = abap_false.
    loop at r_st_mnf where low = '000000'.
      lv_sem_manifesto = abap_true.
      exit.
    endloop.

    if lv_sem_manifesto = abap_true and lines( r_st_mnf ) = 1.
      if <fs_zsdt0127> is assigned.
        delete it_zib_nfe_forn_aux index lv_idx.
      endif.
    elseif lv_sem_manifesto = abap_true and lines( r_st_mnf ) > 1.
      if <fs_zsdt0127> is assigned.
        if <fs_zsdt0127>-cd_operacao not in r_st_mnf.
          delete it_zib_nfe_forn_aux index lv_idx.
        endif.
      endif.
    else.
      if <fs_zsdt0127> is assigned.
        if <fs_zsdt0127>-cd_operacao not in r_st_mnf.
          delete it_zib_nfe_forn_aux index lv_idx.
        endif.
      elseif r_st_mnf[] is not initial.
        delete it_zib_nfe_forn_aux index lv_idx.
      endif.
    endif.

    lv_idx = lv_idx - 1.
    unassign <fs_zsdt0127>.
  endwhile.

  if it_zib_nfe_forn_aux is initial.
    message s000(z01) with 'Registro não encontrado!'.
    stop.
  endif.
endform.

form f_select_passivo using    wa_saida         type ty_saida
                      changing wa_saida_passivo type ty_saida.

  data: ls_t023t type t023t-wgbez60,
        lv_cfop  type string.

  case wa_saida-nu_chave_modelo.
    when '55'.
      wa_saida_passivo-modelo_nota = '55 - Nota Fiscal'.

      select single ekpo~matkl
        from zib_nfe_dist_itm
        inner join ekpo on ekpo~ebeln = zib_nfe_dist_itm~prod_pedido_comp
        into @wa_saida_passivo-gp_mercadorias
        where zib_nfe_dist_itm~chave_nfe = @wa_saida-nu_chave.

      if sy-subrc is not initial.
        with +rs as (
          select
            ekko~lifnr,
            ekpo~ebeln,
            ekpo~ebelp,
            sum( case when ekbe~shkzg = 'S' then ekbe~menge else 0 end ) as entrada,
            sum( case when ekbe~shkzg = 'H' then ekbe~menge else 0 end ) as saida,
            ekpo~menge
          from ekko
          inner join ekpo on  ekpo~ebeln = ekko~ebeln
                          and ekpo~bukrs = @wa_saida-bukrs
                          and ekpo~werks = @wa_saida-branch
          inner join ekbe on ekbe~ebeln = ekpo~ebeln
                         and ekbe~ebelp = ekpo~ebelp
                         and ekbe~vgabe = '1'
          where ekko~lifnr = @wa_saida-lifnr
          group by ekko~lifnr, ekpo~ebeln, ekpo~ebelp, ekpo~menge
        )
        select
          lifnr,
          ebeln,
          ebelp,
          menge - ( entrada - saida ) as saldo_aberto
        from +rs
        where menge - ( entrada - saida ) <> 0
        order by lifnr, ebeln, ebelp
        into table @data(result_nfe).

        read table result_nfe index 1 into data(ls_item_aberto_nfe).
        if sy-subrc = 0.
          select single matkl
            from ekpo
            where ebeln = @ls_item_aberto_nfe-ebeln
              and ebelp = @ls_item_aberto_nfe-ebelp
            into @wa_saida_passivo-gp_mercadorias.
        endif.
      endif.

      " Se permanecer vazio e preencher com o texto do grupo de mercadorias
      if wa_saida_passivo-gp_mercadorias is initial.
        select single prod_descricao from zib_nfe_dist_itm into @wa_saida_passivo-gp_mercadorias where chave_nfe = @wa_saida-nu_chave and prod_item = '000001'.
      else.
        select single wgbez60 from t023t into @ls_t023t where spras = 'PT' and matkl = @wa_saida_passivo-gp_mercadorias.
        wa_saida_passivo-gp_mercadorias = |{ wa_saida_passivo-gp_mercadorias } - { ls_t023t }|.
        clear: ls_t023t.
      endif.

      select single vl_total_fatura
        from zib_nfe_dist_ter
        into @wa_saida_passivo-vl_total
        where chave_nfe = @wa_saida-nu_chave.

      select  * from zib_nfe_dist_itm into table @data(lt_zib_nfe_dist_itm) where chave_nfe = @wa_saida-nu_chave.
      if sy-subrc is initial.
        loop at lt_zib_nfe_dist_itm into data(ls_zib_nfe_dist_itm).
          if wa_saida_passivo-cfop is initial.
            wa_saida_passivo-cfop = ls_zib_nfe_dist_itm-prod_cfop.
          else.
            wa_saida_passivo-cfop = wa_saida_passivo-cfop && ',' && space && ls_zib_nfe_dist_itm-prod_cfop.
          endif.
        endloop.

        if lines( lt_zib_nfe_dist_itm ) = 1.
          lv_cfop = wa_saida_passivo-cfop && 'AA'.

          select single cfotxt
            from j_1bagnt
            where cfop = @lv_cfop
            into @data(ls_nfe_cfotxt).
          if ls_nfe_cfotxt is not initial.
            wa_saida_passivo-cfop = |{ wa_saida_passivo-cfop } - { ls_nfe_cfotxt }|.
          endif.
        endif.
      endif.

    when '57'.
      wa_saida_passivo-modelo_nota = '57 - Conhecimento de Transporte'.

      select single valor_prestacao, codg_cfop, ebeln, ds_prod_pred
        from zib_cte_dist_ter
        where cd_chave_cte = @wa_saida-nu_chave
        into @ls_cte_data.

      if sy-subrc = 0.
        wa_saida_passivo-vl_total       = ls_cte_data-valor_prestacao.

        lv_cfop = ls_cte_data-codg_cfop && 'AA'.

        select single cfotxt
          from j_1bagnt
          where cfop = @lv_cfop
          into @ls_cte_data-cfotxt.
        if ls_cte_data-cfotxt is not initial.
          wa_saida_passivo-cfop = |{ ls_cte_data-codg_cfop } - { ls_cte_data-cfotxt }|.
        else.
          wa_saida_passivo-cfop = ls_cte_data-codg_cfop.
        endif.

        select single matkl
          from ekpo
          where ebeln = @ls_cte_data-ebeln
          into @ls_cte_data-matkl.
        if sy-subrc is not initial.
          with +rs as (
            select
              ekko~lifnr,
              ekpo~ebeln,
              ekpo~ebelp,
              sum( case when ekbe~shkzg = 'S' then ekbe~menge else 0 end ) as entrada,
              sum( case when ekbe~shkzg = 'H' then ekbe~menge else 0 end ) as saida,
              ekpo~menge
            from ekko
            inner join ekpo on  ekpo~ebeln = ekko~ebeln
                            and ekpo~bukrs = @wa_saida-bukrs
                            and ekpo~werks = @wa_saida-branch
            inner join ekbe on ekbe~ebeln = ekpo~ebeln
                           and ekbe~ebelp = ekpo~ebelp
                           and ekbe~vgabe = '1'
            where ekko~lifnr = @wa_saida-lifnr
            group by ekko~lifnr, ekpo~ebeln, ekpo~ebelp, ekpo~menge
          )
          select
            lifnr,
            ebeln,
            ebelp,
            menge - ( entrada - saida ) as saldo_aberto
          from +rs
          where menge - ( entrada - saida ) <> 0
          order by lifnr, ebeln, ebelp
          into table @data(result_cte).

          read table result_cte index 1 into data(ls_item_aberto_cte).
          if sy-subrc = 0.
            select single matkl
              from ekpo
              where ebeln = @ls_item_aberto_cte-ebeln
                and ebelp = @ls_item_aberto_cte-ebelp
              into @wa_saida_passivo-gp_mercadorias.
          endif.
        elseif ls_cte_data-matkl is not initial.
          wa_saida_passivo-gp_mercadorias = ls_cte_data-matkl.
        endif.

        " Se permanecer vazio e preencher com o texto do grupo de mercadorias
        if wa_saida_passivo-gp_mercadorias is initial.
          wa_saida_passivo-gp_mercadorias = ls_cte_data-ds_prod_pred.
        else.
          select single wgbez60 from t023t into @ls_t023t where spras = 'PT' and matkl = @wa_saida_passivo-gp_mercadorias.
          wa_saida_passivo-gp_mercadorias = |{ wa_saida_passivo-gp_mercadorias } - { ls_t023t }|.
          clear: ls_t023t.
        endif.
      endif.
  endcase.

  select single regio, pstlz, txjcd
  from t001w
  into (@wa_saida_passivo-uf_dest, @wa_saida_passivo-pstlz_dest, @wa_saida_passivo-txjcd_dest)
  where werks = @wa_saida-branch.


  clear: lv_cfop, ls_nfe_cfotxt, ls_cte_data.
endform.
*** US #182015 - MMSILVA - 17.06.2025 - Fim ***
