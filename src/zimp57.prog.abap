*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 06/03/2013                                              &*
*& Descrição: PGTO IMPOSTOS – Relatório de Lote Pagamento	            &*
*& Transação: ZIMP57                                                  &*
*---------------------------------------------------------------------&*
*& Histórico de alterações                                            &*
*&--------------------------------------------------------------------&*
*& Autor            Request       Data                                &*
*& Marcos Faneli    DEVK937325    02.05.2014                          &*
*&--------------------------------------------------------------------&*

report  zimp57.

tables: t001.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
types: begin of ty_zib_contabil.
         include structure zib_contabil.
types:   mark type c,
       end of ty_zib_contabil.

"tables : ZIMP_CAD_LOTE.
types:
  begin of ty_zimp_cad_lote,
    lote        type zimp_cad_lote-lote,
    bukrs       type zimp_cad_lote-bukrs,
    loekz       type zimp_cad_lote-loekz,
    status_lote type zimp_cad_lote-status_lote,
    usuario     type zimp_cad_lote-usuario,
    descr_lote  type zimp_cad_lote-descr_lote,
    usnam       type zimp_cad_lote-usnam,           "CS2017000143
    dep_resp    type zimp_cad_lote-dep_resp,        "CS2017000143
  end of ty_zimp_cad_lote,

  begin of ty_texto,
    name_empresa type t001-butxt,
    descr_lote   type zimp_cad_lote-descr_lote,
  end of ty_texto,

  " Lançamento de Impostos
  begin of ty_zimp_lanc_impost,
    doc_imposto  type  zimp_lanc_impost-doc_imposto,
    bukrs        type  zimp_lanc_impost-bukrs,
    gsber        type  zimp_lanc_impost-gsber,
    lote         type  zimp_lanc_impost-lote,
    dt_venc      type  zimp_lanc_impost-dt_venc,
    dt_apuracao  type  zimp_lanc_impost-dt_apuracao,
    mes_apuracao type  zimp_lanc_impost-mes_apuracao,
    ano_apuracao type  zimp_lanc_impost-ano_apuracao,
    observacao   type  zimp_lanc_impost-observacao,
    cod_imposto  type  zimp_lanc_impost-cod_imposto,
    ref_imposto  type  zimp_lanc_impost-ref_imposto,
    tp_imposto   type  zimp_lanc_impost-tp_imposto,
    cod_pgto     type  zimp_lanc_impost-cod_pgto,
    conv_banco   type  zimp_lanc_impost-conv_banco,
    hbkid        type  zimp_lanc_impost-hbkid,
    data_atual   type  zimp_lanc_impost-data_atual,
    hora_atual   type  zimp_lanc_impost-hora_atual,
    usuario      type  zimp_lanc_impost-usuario,
    loekz        type  zimp_lanc_impost-loekz,
    cod_barras   type  zimp_lanc_impost-cod_barras,
    doc_contabil type  zib_contabil_chv-belnr, " contabil
    gjahr        type  zib_contabil_chv-gjahr,
    budat        type  bkpf-budat,
    augbl        type  bsak-augbl,
    flg_err(1),
  end of ty_zimp_lanc_impost,

  begin of ty_zimp_lanc_imp_ct,
    bukrs        type zimp_lanc_imp_ct-bukrs,
    doc_imposto  type zimp_lanc_imp_ct-doc_imposto,
    cod_imposto  type zimp_lanc_imp_ct-cod_imposto,
    cod_abertura type zimp_lanc_imp_ct-cod_abertura,
    bschl        type zimp_lanc_imp_ct-bschl,
    hkont        type zimp_lanc_imp_ct-hkont,
    lifnr        type zimp_lanc_imp_ct-lifnr,
    kostl        type zimp_lanc_imp_ct-kostl,
    prctr        type zimp_lanc_imp_ct-prctr,
    matnr        type zimp_lanc_imp_ct-matnr,
    gsber        type zimp_lanc_imp_ct-gsber,
    valor_imp    type zimp_lanc_imp_ct-valor_imp,
    valor_for    type zimp_lanc_imp_ct-valor_for,
    data_atual   type zimp_lanc_imp_ct-data_atual,
    hora_atual   type zimp_lanc_imp_ct-hora_atual,
    usuario      type zimp_lanc_imp_ct-usuario,
  end of ty_zimp_lanc_imp_ct,

  begin of ty_zimp_cad_imposto,
    cod_imposto   type zimp_cad_imposto-cod_imposto,
    descr_imposto type zimp_cad_imposto-descr_imposto,
  end of ty_zimp_cad_imposto,

  begin of ty_lfa1,
    lifnr type lfa1-lifnr,
    name1 type lfa1-name1,
  end of ty_lfa1,

  begin of ty_t001,
    bukrs type t001-bukrs,
    butxt type t001-butxt,
  end of ty_t001,

  begin of ty_zimp_tipos_impos,
    tp_arrec    type zimp_tipos_impos-tp_arrec,
    arrecadacao type zimp_tipos_impos-arrecadacao,
  end of ty_zimp_tipos_impos,

  begin of ty_zimp_campos_guia,
    cod_abertura    type zimp_campos_guia-cod_camp_guia,
    descr_camp_guia type zimp_campos_guia-descr_camp_guia,
  end of ty_zimp_campos_guia,

  begin of ty_zib_contabil_chv,
    obj_key type zib_contabil_chv-obj_key,
    belnr   type zib_contabil_chv-belnr,
    bukrs   type zib_contabil_chv-bukrs,
    gjahr   type zib_contabil_chv-gjahr,
  end of ty_zib_contabil_chv,

  begin of ty_bkpf,
    bukrs type bkpf-bukrs,
    belnr type bkpf-belnr,
    gjahr type bkpf-gjahr,
    budat type bkpf-budat,
  end of ty_bkpf,

  begin of ty_bsak,
    bukrs type bsak-bukrs,
    belnr type bsak-belnr,
    gjahr type bsak-gjahr,
    augbl type bsak-augbl,
  end of ty_bsak,

  begin of ty_zimp_lotes_aprov,
    bukrs      type zimp_lotes_aprov-bukrs,
    lote       type zimp_lotes_aprov-lote,
    nivel      type zimp_lotes_aprov-nivel,
    aprovador  type zimp_lotes_aprov-aprovador,
    valor_de   type zimp_lotes_aprov-valor_de,
    valor_ate  type zimp_lotes_aprov-valor_ate,
    data_atual type zimp_lotes_aprov-data_atual,
    hora_atual type zimp_lotes_aprov-hora_atual,
    usuario    type zimp_lotes_aprov-usuario,
  end of ty_zimp_lotes_aprov,

  begin of ty_estra ,
    bukrs     type zglt038-bukrs,
    lote      type zglt038-lote,
    valor_de  type zglt037-valor_de,
    valor_ate type zglt037-valor_ate,
    aprovador type zglt037-aprovador,
    nivel     type zglt037-nivel,
    estado(4),
    opcoes(4),
  end of ty_estra,

  begin of ty_saida,
    icon(4)        type c,
    aprov(20),
    doc_contabil   type zib_contabil_chv-belnr, " contabil
    augbl          type bsak-augbl,
    lote           type zimp_lanc_impost-lote, " Lote.
    gsber          type zimp_lanc_impost-gsber, " Filial.
    doc_imposto    type zimp_lanc_impost-doc_imposto, " Doc.Imp.
    cod_imposto    type zimp_lanc_impost-cod_imposto, "Cod.Imp.
    descr_imposto  type zimp_cad_imposto-descr_imposto, "Descr.Imposto
    tp_imposto(50) type c, "Tp.Imposto
    cod_pgto       type zimp_lanc_impost-cod_pgto, "Cond.Pgto
    conv_banco     type zimp_lanc_impost-conv_banco, "Conv.Banco
    hbkid(20)      type c, "Bco.Empresa
    dt_apuracao    type zimp_lanc_impost-dt_apuracao, "Per.Apuração
    mes_ano(7)     type c, "Mês\ano Apuração
    dt_venc        type zimp_lanc_impost-dt_venc, "Dt.Vencimento
    lifnr(50)      type c, "Fornecedor
    vlr_total      type zimp_lanc_imp_ct-valor_imp, " Valor total do fornecedor
    vlr_total_usd  type zimp_lanc_imp_ct-valor_for, " Valor total do fornecedor 2ª Moeda
    bukrs          type zib_contabil_chv-bukrs,
    gjahr          type zib_contabil_chv-gjahr,
    budat          type bkpf-budat,
    cod_barras     type zimp_lanc_impost-cod_barras,
    observacao     type zimp_lanc_impost-observacao,
    usnam          type zimp_cad_lote-usnam,            "CS2017000143
    dep_resp       type zimp_cad_lote-dep_resp,         "CS2017000143
  end of ty_saida.


types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*
data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.
data: wl_mode(1),
      wg_documento(10).


*----------------------------------------------------------------------*
* Field-symbols
*----------------------------------------------------------------------*
* <fs_data> p/ser a tabela dinâmica onde constaram os dados de exibição
field-symbols: <fs_data>  type any table,

*work-área p/ trabalhar os dados antes de inclui <fs_data>
               <wa_data>  type any,

*campo que recebera dados e apontara p/ os campos dinâmicos da wa.
               <fs_campo> type any.

*----------------------------------------------------------------------*
* Tabelas Interna
*----------------------------------------------------------------------*
*Tabela dinâmica de exibição do ALV
data: t_data type ref to data.

*----------------------------------------------------------------------*
* Estrutura de dados
*----------------------------------------------------------------------*
* Work-Área p/ montar dados dos campos
data: wa_fcat_lvc type lvc_s_fcat,

* Tabela sem cabeçalho p/ receber dados da wa acima e passar informações
* de campos p/ gerar a tabela dinâmica
      lt_fcat_lvc type lvc_t_fcat.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

data: t_bdc               type table of bdcdata with header line initial size 0,
      t_messtab           type table of bdcmsgcoll,

      it_zimp_cad_lote    type table of ty_zimp_cad_lote,
      it_zimp_lanc_impost type table of ty_zimp_lanc_impost,
      it_zimp_lanc_imp_ct type table of ty_zimp_lanc_imp_ct,
      it_zimp_lanc_imp_ab type table of ty_zimp_lanc_imp_ct,
      it_zimp_cad_imposto type table of ty_zimp_cad_imposto,
      it_zimp_campos_guia type table of ty_zimp_campos_guia,
      it_zimp_tipos_impos type table of ty_zimp_tipos_impos,
      it_zib_contabil_chv type table of ty_zib_contabil_chv,
      it_zimp_lotes_aprov type table of ty_zimp_lotes_aprov,
      it_bkpf             type table of ty_bkpf,
      it_bsak             type table of ty_bsak,
      it_lfa1             type table of ty_lfa1,
      it_zib_contabil     type table of ty_zib_contabil,
      it_saida            type table of ty_saida,

      it_color            type table of lvc_s_scol,

      it_bdcdata          type standard table of bdcdata.

data: it_zimp_lanc_aux   type table of zimp_lanc_impost with header line,
      it_zimp_lanc_ctaux type table of zimp_lanc_imp_ct with header line,
      it_zimp_cad_impaux type table of zimp_cad_imposto with header line.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
data:
  wa_cont             type ref to cl_gui_custom_container,
  wa_alv              type ref to cl_gui_alv_grid,
  wa_layout           type lvc_s_layo,

  wa_zimp_lanc_impost type ty_zimp_lanc_impost,
  wa_zimp_lanc_imp_ct type ty_zimp_lanc_imp_ct,
  wa_zimp_lanc_imp_ab type ty_zimp_lanc_imp_ct,
  wa_zimp_cad_imposto type ty_zimp_cad_imposto,
  wa_zimp_campos_guia type ty_zimp_campos_guia,
  wa_zimp_tipos_impos type ty_zimp_tipos_impos,
  wa_zimp_cad_lote    type ty_zimp_cad_lote,
  wa_zib_contabil_chv type ty_zib_contabil_chv,
  wa_zimp_lotes_aprov type ty_zimp_lotes_aprov,
  wa_bkpf             type ty_bkpf,
  wa_bsak             type ty_bsak,
  wa_lfa1             type ty_lfa1,
  wa_zib_contabil     type ty_zib_contabil,
  wa_t001             type ty_t001,
  wa_saida            type ty_saida,

  wa_color            type lvc_s_scol,

  wa_bdcdata          like line of it_bdcdata.
*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
data:
  it_fcat       type table of ty_estrutura,
  s_variant     type disvariant           , " Tabela Estrutura co
  t_top         type slis_t_listheader,
  xs_events     type slis_alv_event,
  events        type slis_t_event,
  gd_layout     type slis_layout_alv,
  t_print       type slis_print_alv,
  v_report      like sy-repid,
  t_sort        type slis_t_sortinfo_alv with header line,
  it_setleaf    like table of setleaf initial size 0 with header line,
  estrutura     type table of ty_estrutura,
  vg_i          type i,
  v_repid       like sy-repid,
  v_camp(7),      " variável p/ montar campo dinâmico
  v_text(100),    " variável p/ montar texto dinâmico
  v_continua(1).

data: ok-code             type sy-ucomm,
      grid2               type ref to cl_gui_alv_grid,
      obg_conteiner_estra type ref to cl_gui_custom_container,
      g_cc_estra          type scrfname value 'CC_ESTRA',
      wa_stable           type lvc_s_stbl,
      t_fieldcatalog      type lvc_t_fcat,
      w_fieldcatalog      type lvc_s_fcat,
      tg_estra            type table of ty_estra,
      wg_estra            type ty_estra.

data: repid            like sy-repid.
data: s_fieldcat       type slis_t_fieldcat_alv with header line.
data: s_layout         type slis_layout_alv.
data: s_print          type slis_print_alv.
data: s_sort           type slis_t_sortinfo_alv with header line.
data: variante         like disvariant.
data: def_variante     like disvariant.
data: s_selfield       type slis_selfield.
data: list_top_of_page type slis_t_listheader.

data wg_texto type ty_texto.

define mc_preenche_class.
  vg_i = vg_i + 1.
  clear t_sort.
  t_sort-spos      = vg_i.
  t_sort-fieldname = &1.
  t_sort-group     = &2.
  t_sort-up        = &3.
  t_sort-subtot    = &4.
  append t_sort.
end-of-definition.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
selection-screen: begin of block b1 with frame title text-001.
  select-options: p_bukrs     for  t001-bukrs obligatory.
  select-options: p_lote      for  wa_zimp_cad_lote-lote   ,
                  p_dtv       for  wa_zimp_lanc_impost-dt_venc,
                  p_dtl       for  wa_zimp_lanc_impost-dt_venc,
                  p_cod       for  wa_zimp_lanc_impost-cod_imposto,                   "CS2017000143
                  p_user      for  wa_zimp_cad_lote-usnam,                            "CS2017000143
                  p_dep       for  wa_zimp_cad_lote-dep_resp.                         "CS2017000143
  parameters:
    p_lot1 radiobutton group rad1,
    p_lot2 radiobutton group rad1 default 'X',
    p_lot3 radiobutton group rad1,
    p_lot4 radiobutton group rad1.
selection-screen: end of block b1.

at selection-screen on value-request for p_cod-low.
  perform f4_p_cod.

at selection-screen on value-request for p_cod-high.
  perform f4_p_cod.

at selection-screen on value-request for p_user-low.
  perform f4_p_user.

at selection-screen on value-request for p_user-high.
  perform f4_p_user.

at selection-screen on value-request for p_dep-low.
  perform f4_p_dep.

at selection-screen on value-request for p_dep-high.
  perform f4_p_dep.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
start-of-selection.
  v_continua = 'S'.
  if p_bukrs[] is initial .
    message 'Informe a Empresa!' type 'I'.
  else.


    select * into table @data(it_empresa)
from t001
where bukrs in @p_bukrs.

    loop at it_empresa into data(wa_empresa).

      authority-check object 'F_SKA1_BUK'
             id 'ACTVT' field '03'       "display
             id 'BUKRS' field  wa_empresa-bukrs.
      if sy-subrc <> 0.
        delete it_empresa index sy-tabix.
      endif.

    endloop.

    if it_empresa[] is not initial.

      perform:
                f_iniciar_variaves, " Cabeçalho
                f_seleciona_dados, " Form seleciona dados
                f_saida, " Form de saida
                f_imprime_dados.
    else.
      message s000(z_fi) with 'Sem autorização para esta Empresa ' display like 'E'.
      return.
    endif.

  endif.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados .
  data vobj_key type zib_contabil_err-obj_key.



  check it_empresa[] is not initial.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Preparando dados'.

  ranges: rg_status_lote for zimp_cad_lote-status_lote.

  clear: rg_status_lote[].

  case abap_true.
    when p_lot1. "Todos
    when p_lot2. "Lotes Não Liberados
      append value #( sign = 'I' option = 'EQ' low = space ) to rg_status_lote.
    when p_lot3. "Lotes Não Aprovados
      append value #( sign = 'I' option = 'EQ' low = 'L' ) to rg_status_lote.
    when p_lot4. "Aprovados
      append value #( sign = 'I' option = 'EQ' low = 'A' ) to rg_status_lote.
  endcase.

  select  lote bukrs loekz status_lote usuario usnam dep_resp       "CS2017000143
    from zimp_cad_lote
    into corresponding fields of table it_zimp_cad_lote
    where lote in p_lote
    and   bukrs in p_bukrs
    and   loekz = ' '
    and   dt_venc in p_dtv
    and usnam in p_user                                             "CS2017000143
    and dep_resp in p_dep                                          "CS2017000143
    and   status_lote in rg_status_lote.

  check it_zimp_cad_lote[] is not initial.

  select  doc_imposto bukrs gsber lote dt_venc dt_apuracao mes_apuracao ano_apuracao observacao cod_imposto ref_imposto tp_imposto cod_pgto conv_banco
    hbkid  data_atual hora_atual usuario loekz cod_barras
    from zimp_lanc_impost
    into table it_zimp_lanc_impost
    for all entries in it_zimp_cad_lote
    where bukrs = it_zimp_cad_lote-bukrs
    and   lote  = it_zimp_cad_lote-lote
    and   loekz = ''
    and cod_imposto in p_cod.                                        "CS2017000143

  check it_zimp_lanc_impost[] is not initial.

  loop at it_zimp_lanc_impost into wa_zimp_lanc_impost.
    concatenate 'ZP' wa_zimp_lanc_impost-bukrs wa_zimp_lanc_impost-doc_imposto '%' into vobj_key.

    select single obj_key belnr bukrs gjahr
    from zib_contabil_chv
    into wa_zib_contabil_chv
    where obj_key like vobj_key
    and   bukrs = wa_zimp_lanc_impost-bukrs. " P_BUKRS.
    if sy-subrc ne 0.
      concatenate 'ZIMP' wa_zimp_lanc_impost-doc_imposto '%' into vobj_key.
      select single obj_key belnr bukrs gjahr
        from zib_contabil_chv
        into wa_zib_contabil_chv
      where obj_key like vobj_key
      and   bukrs = wa_zimp_lanc_impost-bukrs. " IN P_BUKRS.
    endif.

    if sy-subrc = 0.
      wa_zimp_lanc_impost-doc_contabil = wa_zib_contabil_chv-belnr.
      wa_zimp_lanc_impost-gjahr        = wa_zib_contabil_chv-gjahr.

      modify it_zimp_lanc_impost from wa_zimp_lanc_impost index sy-tabix transporting doc_contabil .

      select single bukrs belnr gjahr budat
        from bkpf
        into wa_bkpf
        where bukrs = wa_zib_contabil_chv-bukrs
      and   belnr = wa_zib_contabil_chv-belnr
      and   gjahr = wa_zib_contabil_chv-gjahr.

      if sy-subrc = 0.
        wa_zimp_lanc_impost-budat = wa_bkpf-budat.
        modify it_zimp_lanc_impost from wa_zimp_lanc_impost index sy-tabix transporting budat .
      endif.

      select single bukrs belnr gjahr augbl
        from bsak
        into wa_bsak
        where  bukrs =  wa_zib_contabil_chv-bukrs
      and    belnr =  wa_zib_contabil_chv-belnr
      and    gjahr =  wa_zib_contabil_chv-gjahr.
      if sy-subrc = 0.
        wa_zimp_lanc_impost-augbl = wa_bsak-augbl.
        modify it_zimp_lanc_impost from wa_zimp_lanc_impost index sy-tabix transporting augbl.
      endif.
    else.
      concatenate 'ZP' wa_zimp_lanc_impost-bukrs wa_zimp_lanc_impost-doc_imposto '%' into vobj_key.
      select single obj_key
        from zib_contabil_err
      into wa_zib_contabil_chv
      where obj_key like vobj_key.
      if sy-subrc = 0.
        wa_zimp_lanc_impost-flg_err = 'S'.
        modify it_zimp_lanc_impost from wa_zimp_lanc_impost index sy-tabix transporting flg_err.
      endif.
    endif.
  endloop.
  "
  if p_dtl is not initial.
    if p_dtl-high is initial.
      delete it_zimp_lanc_impost where budat ne p_dtl-low .
    else.
      delete it_zimp_lanc_impost where budat not between p_dtl-low and p_dtl-high.
    endif.
  endif.

  select bukrs doc_imposto cod_imposto cod_abertura bschl hkont lifnr kostl prctr matnr gsber valor_imp valor_for data_atual hora_atual usuario
    from zimp_lanc_imp_ct
    into table it_zimp_lanc_imp_ct
    for all entries in it_zimp_lanc_impost
  where doc_imposto = it_zimp_lanc_impost-doc_imposto
  and   bukrs       = it_zimp_lanc_impost-bukrs.

  select cod_imposto descr_imposto
    from zimp_cad_imposto
    into table it_zimp_cad_imposto
  for all entries in it_zimp_lanc_impost
  where cod_imposto = it_zimp_lanc_impost-cod_imposto.

  select tp_arrec arrecadacao
    from zimp_tipos_impos
    into table it_zimp_tipos_impos
  for all entries in it_zimp_lanc_impost
  where tp_arrec = it_zimp_lanc_impost-tp_imposto.

  select lifnr name1
    from lfa1
    into table it_lfa1
  for all entries in  it_zimp_lanc_imp_ct
  where lifnr = it_zimp_lanc_imp_ct-lifnr.

  delete it_zimp_lanc_imp_ct where valor_imp eq 0.
  it_zimp_lanc_imp_ab[] = it_zimp_lanc_imp_ct[].
  sort it_zimp_lanc_imp_ab by cod_abertura.
  delete adjacent duplicates from it_zimp_lanc_imp_ab comparing cod_abertura.

  select cod_camp_guia descr_camp_guia
    from zimp_campos_guia
    into table it_zimp_campos_guia
  for all entries in it_zimp_lanc_imp_ab
  where cod_camp_guia eq it_zimp_lanc_imp_ab-cod_abertura.


endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_saida .

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Gerando relatório'.

  data: vlifnr type zimp_lanc_imp_ct-lifnr.
  sort: it_zimp_lanc_impost by bukrs doc_imposto,
        it_zimp_lanc_imp_ct by bukrs doc_imposto,
        it_zimp_lanc_impost by bukrs cod_imposto,
        it_zimp_cad_imposto by cod_imposto,
        it_zimp_tipos_impos by tp_arrec,
        it_zimp_cad_lote    by lote,
        it_lfa1             by lifnr.


  loop at it_zimp_lanc_impost into wa_zimp_lanc_impost.
    read table it_zimp_cad_lote into wa_zimp_cad_lote with key lote = wa_zimp_lanc_impost-lote binary search.
    if sy-subrc is initial.
      wa_saida-usnam = wa_zimp_cad_lote-usnam.                                        "CS2017000143
      wa_saida-dep_resp = wa_zimp_cad_lote-dep_resp.                                  "CS2017000143
    endif.
    if wa_zimp_cad_lote-status_lote = ' '  .
      wa_saida-aprov =  'Não liberado'.
    elseif wa_zimp_cad_lote-status_lote = 'L' .
      wa_saida-aprov =  'Não aprovado'.
    elseif wa_zimp_cad_lote-status_lote = 'A' .
      wa_saida-aprov =  'Aprovado'.
    endif.
    if wa_zimp_lanc_impost-doc_contabil is not initial.
      wa_saida-icon = icon_checked.
    elseif wa_zimp_lanc_impost-flg_err = 'S'.
      wa_saida-icon = icon_message_error.
    else.
      wa_saida-icon = icon_led_yellow.
    endif.

    "reprocessa tambem se não gravou na ZIB após aprovação 08.05.2017
    if wa_zimp_cad_lote-status_lote = 'A'.
      concatenate 'ZP' wa_zimp_lanc_impost-bukrs wa_zimp_lanc_impost-doc_imposto '%' into wa_zib_contabil-obj_key.
      select single *
        from zib_contabil
        into wa_zib_contabil
        where obj_key like wa_zib_contabil-obj_key.
      if sy-subrc ne 0.
        wa_saida-icon = icon_failure.
      endif.
    endif.

    wa_saida-lote           = wa_zimp_lanc_impost-lote.
    wa_saida-doc_contabil   = wa_zimp_lanc_impost-doc_contabil.
    wa_saida-augbl          = wa_zimp_lanc_impost-augbl.
    wa_saida-bukrs          = wa_zimp_lanc_impost-bukrs.
    wa_saida-gsber          = wa_zimp_lanc_impost-gsber.
    wa_saida-gjahr          = wa_zimp_lanc_impost-gjahr.
    wa_saida-doc_contabil   = wa_zimp_lanc_impost-doc_contabil.
    wa_saida-doc_imposto    = wa_zimp_lanc_impost-doc_imposto.
    wa_saida-cod_imposto    = wa_zimp_lanc_impost-cod_imposto.
    wa_saida-observacao     = wa_zimp_lanc_impost-observacao.
    wa_saida-cod_barras     = wa_zimp_lanc_impost-cod_barras.
    wa_saida-budat          = wa_zimp_lanc_impost-budat.

    read table it_zimp_cad_imposto into wa_zimp_cad_imposto with key cod_imposto = wa_zimp_lanc_impost-cod_imposto binary search.
    if sy-subrc = 0.
      wa_saida-descr_imposto  = wa_zimp_cad_imposto-descr_imposto.
    endif.

    clear: vlifnr.
    loop at it_zimp_lanc_imp_ct into wa_zimp_lanc_imp_ct where bukrs       = wa_zimp_lanc_impost-bukrs
                                                           and doc_imposto = wa_zimp_lanc_impost-doc_imposto.

      if wa_zimp_lanc_imp_ct-lifnr is not initial.
        vlifnr          = wa_zimp_lanc_imp_ct-lifnr.
      endif.
    endloop.

    if vlifnr is not initial.
      read table it_lfa1 into wa_lfa1 with key lifnr = vlifnr binary search.

      if sy-subrc = 0.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = vlifnr
          importing
            output = vlifnr.

        concatenate  vlifnr '-' wa_lfa1-name1 into wa_saida-lifnr.
      endif.
    endif.

    read table it_zimp_tipos_impos into wa_zimp_tipos_impos with key tp_arrec = wa_zimp_lanc_impost-tp_imposto binary search.
    concatenate wa_zimp_lanc_impost-tp_imposto '-' wa_zimp_tipos_impos-arrecadacao  into wa_saida-tp_imposto .

    wa_saida-cod_pgto       = wa_zimp_lanc_impost-cod_pgto.
    wa_saida-conv_banco     = wa_zimp_lanc_impost-conv_banco.
    if wa_zimp_lanc_impost-hbkid = 'BBD'.
      concatenate wa_zimp_lanc_impost-hbkid '-Bradesco' into wa_saida-hbkid.
    else.
      concatenate wa_zimp_lanc_impost-hbkid '-Banco do Brasil' into wa_saida-hbkid.
    endif.
    wa_saida-dt_apuracao    = wa_zimp_lanc_impost-dt_apuracao.
    concatenate wa_zimp_lanc_impost-mes_apuracao '/' wa_zimp_lanc_impost-ano_apuracao into wa_saida-mes_ano.
    wa_saida-dt_venc        = wa_zimp_lanc_impost-dt_venc.
    append wa_saida to  it_saida.
    clear: wa_saida, vlifnr.

  endloop.

endform.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_imprime_dados .
  perform f_definir_eventos.
  perform f_alv_sort.
  perform f_alv.

endform.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_definir_eventos .
  perform f_carregar_eventos using:
                                     slis_ev_top_of_page  'XTOP_OF_PAGE'.

endform.                    " F_DEFINIR_EVENTOS

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
form top_of_page.

* Cabeçalho Logo
  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = list_top_of_page[].
  "I_LOGO             = 'WELLA_LOGO'.

endform.        " top_of_page.

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
form xtop_of_page.                                          "#EC CALLED

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = t_top.

endform. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to events.
endform.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv_sort .

endform.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv .

  perform alv_preenche_cat using:
               'ICON'          'St.Contábil'   '10'       ' '     ' '    ' ' , "Icon
               'APROV'         'St.Aprovação'  '20'       ' '     ' '    ' ' , "Aprovação
               'DOC_CONTABIL'  text-014        '15'       'X'     ' '    ' ' , "Doc.contábil.
               'AUGBL'         text-019        '15'       ' '     ' '    ' ' , "Documento de Compensação .
               'LOTE'          text-015        '10'       ' '     ' '    ' ' , "Lote

               'BUKRS'         text-022        '07'       ' '     ' '    ' ' , "Empresa
               'GSBER'         text-023        '07'       ' '     ' '    ' ' , "Filial

               'DOC_IMPOSTO'   text-002        '15'       'X'     ' '    ' ' , "Doc.Imp.
               'COD_IMPOSTO'   text-003        '10'       ' '     ' '    ' ' , "Cod.Imp.
               'DESCR_IMPOSTO' text-004        '20'       ' '     ' '    ' ' , "Descr.Imposto
               'TP_IMPOSTO'    text-005        '40'       ' '     ' '    ' ' , "Tp.Imposto
               'COD_PGTO'      text-006        '15'       ' '     ' '    ' ' , "Cond.Pgto
               'CONV_BANCO'    text-007        '05'       ' '     ' '    ' ' , "Conv.Banco
               'HBKID'         text-008        '15'       ' '     ' '    ' ' , "Bco.Empresa
               'DT_APURACAO'   text-009        '15'       ' '     ' '    ' ' , "Per.Apuração
               'BUDAT'         text-016        '15'       ' '     ' '    ' ' , "Dt.Lançamento
               'MES_ANO'       text-010        '15'       ' '     ' '    ' ' , "Mês\ano Apuração
               'DT_VENC'       text-011        '15'       ' '     ' '    ' ' , "Dt.Vencimento
               'COD_BARRAS'    text-017        '30'       ' '     ' '    ' ' , "Cód.Barras
               'OBSERVACAO'    text-018        '40'       ' '     ' '    ' ' , "Observação
               'LIFNR'         text-012        '50'       ' '     ' '    ' ' , "Fornecedor
               'VLR_TOTAL'     text-013        '15'       ' '     ' '    ' ' , "Valor Total R$
               'VLR_TOTAL_USD' text-024        '15'       ' '     ' '    ' ' . "Valor Total U$

  perform monta_fieldcat using:
               'ICON'          ' '                ' '             '04'       ' ' , "Icon
               'APROV'         ' '                ' '             '20'       ' ' , "Aprovação
               'DOC_CONTABIL'  ' '                text-014        '15'       ' ' , "Doc.Imp.
               'AUGBL'         ' '                text-019        '15'       ' ' , "Documento de Compensação .
               'LOTE'          'ZIMP_LANC_IMPOST' text-015        '10'       'LOTE', "Lote.

               'BUKRS'         'ZIMP_LANC_IMPOST' text-022        '10'       'BUKRS', "Empresa.
               'GSBER'         'ZIMP_LANC_IMPOST' text-023        '10'       'GSBER', "Filial.

               'DOC_IMPOSTO'   'ZIMP_LANC_IMPOST' text-002        '15'       'DOC_IMPOSTO' , "Doc.Imp.
               'COD_IMPOSTO'   'ZIMP_LANC_IMPOST' text-003        '10'       'COD_IMPOSTO' , "Cod.Imp.
               'DESCR_IMPOSTO' 'ZIMP_CAD_IMPOSTO' text-004        '20'       'DESCR_IMPOSTO' , "Descr.Imposto
               'TP_IMPOSTO'    ' '                text-005        '40'       ' ' , "Tp.Imposto
               'COD_PGTO'      'ZIMP_LANC_IMPOST' text-006        '15'       'COD_PGTO' , "Cond.Pgto
               'CONV_BANCO'    'ZIMP_LANC_IMPOST' text-007        '05'       'CONV_BANCO' , "Conv.Banco
               'HBKID'         ' '                text-008        '20'       ' ' , "Bco.Empresa
               'DT_APURACAO'   'ZIMP_LANC_IMPOST' text-009        '15'       'DT_APURACAO' , "Per.Apuração
               'BUDAT'         'BKPF'             text-016        '15'       'BUDAT' , "DATA LANÇAMENTO
               'MES_ANO'       ' '                text-010        '15'       ' ' , "Mês\ano Apuração
               'DT_VENC'       'ZIMP_LANC_IMPOST' text-011        '15'       'DT_VENC' , "Dt.Vencimento
               'COD_BARRAS'    'ZIMP_LANC_IMPOST' text-017        '30'       'COD_BARRAS' , "
               'OBSERVACAO'    'ZIMP_LANC_IMPOST' text-018        '40'       'OBSERVACAO' , "
               'LIFNR'         ' '                text-012        '50'       ' ' , "Fornecedor
               'VLR_TOTAL'     'ZIMP_LANC_IMP_CT' text-013        '15'       'VALOR_IMP' ,
               'VLR_TOTAL_USD' 'ZIMP_LANC_IMP_CT' text-024        '15'       'VALOR_FOR' .

  sort it_zimp_campos_guia by cod_abertura.
  loop at it_zimp_lanc_imp_ab into wa_zimp_lanc_imp_ab.
    clear: v_camp,
         v_text.
    if wa_zimp_lanc_imp_ab-lifnr is initial .
      read table it_zimp_campos_guia into wa_zimp_campos_guia with key cod_abertura = wa_zimp_lanc_imp_ab-cod_abertura binary search.
      concatenate  'VAL'
                   wa_zimp_lanc_imp_ab-cod_abertura
                   into v_camp.
      v_text = wa_zimp_campos_guia-descr_camp_guia.

      perform monta_fieldcat using
             v_camp   'ZIMP_LANC_IMP_CT' v_text            '15' 'VALOR_IMP'.
      perform alv_preenche_cat using:
              v_camp   v_text       '15'       ' '     ' '    ' ' .
    endif.
  endloop.

  perform alv_preenche_cat using:                                                                                 "CS2017000143
            'USNAM'         text-020        '13'       ' '     ' '    ' ' , "Usuário do Lançamento
            'DEP_RESP'      text-021        '12'       ' '     ' '    ' ' . "Departamento

  perform monta_fieldcat using:                                                                                   "CS2017000143
            'USNAM'         'ZIMP_CAD_LOTE'    text-020        '13'       'USNAM',      "Usuário do Lançamento
            'DEP_RESP'      'ZIMP_CAD_LOTE'    text-021        '12'       'DEP_RESP'.   "Departamento

*  TABELA DINAMICA
  data: t_alvdata type ref to data.

* Monta tabela dinâmica
  call method cl_alv_table_create=>create_dynamic_table
    exporting
      i_style_table   = ' '
*                     tab com as informações de campo
      it_fieldcatalog = lt_fcat_lvc
    importing
*                     retorna tab dinâmica com campos informados
      ep_table        = t_data.

* Carrega <fs_data> com a estrutura dos campos passados para o metodo
  assign t_data->* to <fs_data>.
  create data t_alvdata like line of <fs_data>.
  assign t_alvdata->* to <wa_data>.

  refresh <fs_data>.

  sort it_saida by lote doc_imposto.
  loop at it_saida into wa_saida.

* Campos fixos
    perform f_carrega_dados using:
        wa_saida-icon          'ICON'   ,
        wa_saida-aprov         'APROV'  ,
        wa_saida-doc_contabil  'DOC_CONTABIL' ,
        wa_saida-augbl         'AUGBL'        ,
        wa_saida-lote          'LOTE'         ,
        wa_saida-bukrs         'BUKRS'        ,
        wa_saida-gsber         'GSBER'        ,
        wa_saida-doc_imposto   'DOC_IMPOSTO'  ,
        wa_saida-cod_imposto   'COD_IMPOSTO'  ,
        wa_saida-descr_imposto 'DESCR_IMPOSTO',
        wa_saida-tp_imposto   'TP_IMPOSTO'    ,
        wa_saida-cod_pgto     'COD_PGTO'      ,
        wa_saida-conv_banco   'CONV_BANCO'    ,
        wa_saida-hbkid        'HBKID'         ,
        wa_saida-dt_apuracao  'DT_APURACAO'   ,
        wa_saida-mes_ano      'MES_ANO'       ,
        wa_saida-dt_venc      'DT_VENC'       ,
        wa_saida-lifnr        'LIFNR'         ,
        wa_saida-cod_barras   'COD_BARRAS'    ,
        wa_saida-observacao   'OBSERVACAO'    ,
        wa_saida-budat        'BUDAT'         .

    clear wa_saida-vlr_total.
    loop at it_zimp_lanc_imp_ct into wa_zimp_lanc_imp_ct where bukrs       = wa_saida-bukrs
                                                           and doc_imposto = wa_saida-doc_imposto.

      if wa_zimp_lanc_imp_ct-lifnr is initial.
        clear v_camp.
        concatenate 'VAL'
                     wa_zimp_lanc_imp_ct-cod_abertura
                     into v_camp.
        perform f_carrega_dados using:
            wa_zimp_lanc_imp_ct-valor_imp     v_camp.
        add wa_zimp_lanc_imp_ct-valor_imp to wa_saida-vlr_total.
        add wa_zimp_lanc_imp_ct-valor_for to wa_saida-vlr_total_usd.
      endif.
    endloop.

    perform f_carrega_dados using:
           wa_saida-vlr_total    'VLR_TOTAL'.

    perform f_carrega_dados using:
           wa_saida-vlr_total_usd 'VLR_TOTAL_USD'.

*  Inclui dados da work-área dinâmica na tabela dinâmica

    perform f_carrega_dados using:
         wa_saida-usnam        'USNAM'         ,     "CS2017000143
         wa_saida-dep_resp     'DEP_RESP'      .     "CS2017000143

    perform f_carrega_alv using <fs_data>
                                <wa_data>.
    clear           <wa_data>..
  endloop.

* Impressão do ALV passando tabela dinâmica
  perform f_imprime_dados_alv using <fs_data>.
endform.                    " F_AL

*&---------------------------------------------------------------------*
*&      Form  f_carrega_alv
*&---------------------------------------------------------------------*
*      -->P_TAB  tabela
*      -->P_WA   work-área
*----------------------------------------------------------------------*
form f_carrega_alv using    p_tab type table
                            p_wa.
*  Inclui dados da work-área dinâmica na tabela dinâmica
  append p_wa to p_tab.

endform.                    " f_carrega_alv
*&---------------------------------------------------------------------*
*&      Form  f_imprime_dados
*&---------------------------------------------------------------------*
form f_imprime_dados_alv using p_itab_output type table.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = v_report
      is_layout                = gd_layout
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = it_fcat[]
      it_sort                  = t_sort[]
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
*     IS_VARIANT               = VG_VARIANT
    tables
      t_outtab                 = p_itab_output.

endform.                    " f_imprime_dados

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
form set_pf_status using rt_extab type slis_t_extab.        "#EC CALLED
  data: fcode  type table of sy-ucomm,
        _param type  ustyp_t_parameters.


* // pega os parametros do usuario
  call function 'SUSR_USER_PARAMETERS_GET'
    exporting
      user_name           = sy-uname
    tables
      user_parameters     = _param
    exceptions
      user_name_not_exist = 1
      others              = 2.

  read table _param assigning field-symbol(<ws_param>) with key parid = 'ZPARAM_ZIMP57_ESTORN'.
  if sy-subrc ne 0.
    append '&EST' to fcode.
  endif.

  describe table rt_extab. "Avoid Extended Check Warning
  set pf-status 'STANDARD_FULLSCREEN' excluding fcode..
endform. "Set_pf_status

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

form user_command using r_ucomm     like sy-ucomm           "#EC CALLED
                        rs_selfield type slis_selfield.

  data: v_msg   type char50,
        t_lotes type table of zfi_lotes_imp,
        w_lotes type          zfi_lotes_imp,
        t_estra type table of zfi_estrategia_imp,
        w_estra type          zfi_estrategia_imp,
        t_docs  type table of zfi_docs_imp,
        w_docs  type          zfi_docs_imp.


  read table it_saida into wa_saida index rs_selfield-tabindex.

  case rs_selfield-fieldname.

    when 'DOC_CONTABIL'.

      read table it_saida into wa_saida with key doc_contabil = rs_selfield-value.
      check sy-subrc = 0.

      if wa_saida-doc_contabil is not initial.
        set parameter id 'BLN' field wa_saida-doc_contabil.
        set parameter id 'BUK' field wa_saida-bukrs.
        set parameter id 'GJR' field wa_saida-gjahr.
        call transaction 'FB03' and skip first screen.
      endif.
    when 'DOC_IMPOSTO'.

      read table it_saida into wa_saida with key doc_imposto = rs_selfield-value.
      check sy-subrc = 0.

      if wa_saida-doc_imposto is not initial.
        set parameter id 'BUK' field wa_saida-bukrs.
        set parameter id 'BLN' field wa_saida-doc_imposto.
        call transaction 'ZIMP53' and skip first screen.
      endif.
  endcase.

  if r_ucomm eq '&ESTRA'.
    call function 'Z_FI_ESTRATEGIA_LISTA'
      exporting
        v_usuario = sy-uname
        v_lote    = wa_saida-lote
      importing
        msg       = v_msg
      tables
        t_lotes   = t_lotes
        t_estra   = t_estra
        t_docs    = t_docs.
    refresh:  tg_estra.
    loop at t_estra into w_estra.
      move-corresponding w_estra to wg_estra.
      append wg_estra to tg_estra.
    endloop.

    call screen 0100 starting at 050 3
                    ending   at 125 12.

  elseif r_ucomm eq '&LOG'.
    data: begin of itab occurs 0,
            name(80) type c,
          end of itab.
    data: vdata(10),vhora(10), msg_lote type char80.

    read table it_saida into wa_saida index rs_selfield-tabindex.
    select bukrs lote nivel aprovador valor_de valor_ate data_atual hora_atual usuario
       from zimp_lotes_aprov
    into table it_zimp_lotes_aprov
    where  lote  =  wa_saida-lote.
    sort it_zimp_lotes_aprov by nivel.
    itab-name    = 'NIVEL|APROVADOR    |DATA       |HORA'.
    append itab .
    clear itab.
    loop at it_zimp_lotes_aprov into wa_zimp_lotes_aprov.
      concatenate wa_zimp_lotes_aprov-hora_atual+0(2) wa_zimp_lotes_aprov-hora_atual+2(2) wa_zimp_lotes_aprov-hora_atual+4(2) into vhora separated by ':'.
      concatenate wa_zimp_lotes_aprov-data_atual+6(2) wa_zimp_lotes_aprov-data_atual+4(2) wa_zimp_lotes_aprov-data_atual+0(4) into vdata separated by '.'.

      itab-name+00(04) = wa_zimp_lotes_aprov-nivel.
      itab-name+05(01) = '|'.
      itab-name+06(12) = wa_zimp_lotes_aprov-aprovador.
      itab-name+19(01) = '|'.
      itab-name+20(10) = vdata.
      itab-name+31(01) = '|'.
      itab-name+32(10) = vhora.
      append itab .
      clear itab.
    endloop.
    concatenate 'Lote ' wa_saida-lote into msg_lote separated by space.
    call function 'POPUP_WITH_TABLE_DISPLAY'
      exporting
        endpos_col   = 140
        endpos_row   = 20
        startpos_col = 60
        startpos_row = 15
        titletext    = msg_lote
      tables
        valuetab     = itab
      exceptions
        break_off    = 1
        others       = 2.
  elseif r_ucomm eq '&REPROC'.

    if wa_saida-icon = icon_message_error or wa_saida-icon = icon_failure.
      refresh: it_zimp_lanc_aux, it_zimp_lanc_ctaux,it_zimp_cad_impaux.
      select *
       into table it_zimp_lanc_aux
       from zimp_lanc_impost
      where doc_imposto = wa_saida-doc_imposto
      and   bukrs       = wa_saida-bukrs.

      check it_zimp_lanc_aux[] is not initial.

      select *
        into table it_zimp_lanc_ctaux
        from zimp_lanc_imp_ct
        for all entries in it_zimp_lanc_aux
      where doc_imposto = it_zimp_lanc_aux-doc_imposto
      and   bukrs       = it_zimp_lanc_aux-bukrs.

      select *
        into table it_zimp_cad_impaux
        from zimp_cad_imposto
      for all entries in it_zimp_lanc_ctaux
      where cod_imposto = it_zimp_lanc_ctaux-cod_imposto.

      concatenate 'ZIMP' wa_saida-doc_imposto sy-datum+0(4) into wa_zib_contabil-obj_key.
      delete from zib_contabil_err where obj_key = wa_zib_contabil-obj_key.
      delete from zib_contabil     where obj_key = wa_zib_contabil-obj_key.

      concatenate 'ZP' wa_saida-bukrs wa_saida-doc_imposto sy-datum+0(4) into wa_zib_contabil-obj_key.
      delete from zib_contabil_err where obj_key = wa_zib_contabil-obj_key.
      delete from zib_contabil     where obj_key = wa_zib_contabil-obj_key.


      data vseq type i value 0.
      sort: it_zimp_lanc_aux   by doc_imposto bukrs,
            it_zimp_lanc_ctaux by doc_imposto bukrs,
            it_zimp_cad_impaux by cod_imposto.

      loop at it_zimp_lanc_aux .
        vseq = 0.
        loop at it_zimp_lanc_ctaux where doc_imposto = it_zimp_lanc_aux-doc_imposto
                                   and   bukrs       = it_zimp_lanc_aux-bukrs.
          if it_zimp_lanc_ctaux-valor_imp ne 0.
            if it_zimp_lanc_ctaux-seqitem ne 0.
              vseq = it_zimp_lanc_ctaux-seqitem.
            else.
              add 1 to vseq.
            endif.
            "CONCATENATE 'ZIMP' IT_ZIMP_LANC_AUX-DOC_IMPOSTO SY-DATUM+0(4) INTO WA_ZIB_CONTABIL-OBJ_KEY.
            concatenate 'ZP' it_zimp_lanc_aux-bukrs it_zimp_lanc_aux-doc_imposto sy-datum+0(4) into wa_zib_contabil-obj_key.
            wa_zib_contabil-seqitem   = vseq.
            concatenate 'LOTE-' it_zimp_lanc_aux-lote into wa_zib_contabil-xblnr.
            wa_zib_contabil-bschl     = it_zimp_lanc_ctaux-bschl.
            wa_zib_contabil-gsber     = it_zimp_lanc_aux-gsber.
            wa_zib_contabil-bukrs     = it_zimp_lanc_aux-bukrs.
            wa_zib_contabil-interface   = '35'.
            wa_zib_contabil-bktxt     = ''.
            concatenate  sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) into wa_zib_contabil-bldat separated by '.'.
            concatenate  sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) into wa_zib_contabil-budat separated by '.'.

            wa_zib_contabil-gjahr     = sy-datum+0(4).
            wa_zib_contabil-monat     = sy-datum+4(2).
            wa_zib_contabil-blart     = 'TB'.
            wa_zib_contabil-zlsch     = 'P'.

*            IF IT_ZIMP_LANC_CTAUX-LIFNR IS INITIAL.
*              WA_ZIB_CONTABIL-HKONT     = IT_ZIMP_LANC_CTAUX-HKONT.
*            ELSE.
*              WA_ZIB_CONTABIL-HKONT     = IT_ZIMP_LANC_CTAUX-LIFNR.
*            ENDIF.

            if it_zimp_lanc_ctaux-lifnr is initial and it_zimp_lanc_ctaux-kunnr is initial.
              wa_zib_contabil-hkont     = it_zimp_lanc_ctaux-hkont.
            elseif it_zimp_lanc_ctaux-kunnr is not initial.
              wa_zib_contabil-hkont     = it_zimp_lanc_ctaux-kunnr.
            else.
              wa_zib_contabil-hkont     = it_zimp_lanc_ctaux-lifnr.
            endif.

            wa_zib_contabil-kostl     = it_zimp_lanc_ctaux-kostl.
            wa_zib_contabil-prctr     = it_zimp_lanc_ctaux-prctr.
            wa_zib_contabil-matnr     = it_zimp_lanc_ctaux-matnr.

            if it_zimp_lanc_aux-waers = 'BRL'.
              if it_zimp_lanc_ctaux-valor_imp lt 0.
                wa_zib_contabil-wrbtr     = it_zimp_lanc_ctaux-valor_imp * -1.
              else.
                wa_zib_contabil-wrbtr     = it_zimp_lanc_ctaux-valor_imp.
              endif.
            else.
              if it_zimp_lanc_ctaux-valor_for lt 0.
                wa_zib_contabil-wrbtr     = it_zimp_lanc_ctaux-valor_for * -1.
              else.
                wa_zib_contabil-wrbtr     = it_zimp_lanc_ctaux-valor_for .
              endif.
            endif.
            wa_zib_contabil-waers     = it_zimp_lanc_aux-waers.


            wa_zib_contabil-bupla     = it_zimp_lanc_ctaux-gsber.

            read table it_zimp_cad_impaux with key cod_imposto = it_zimp_lanc_ctaux-cod_imposto binary search.
            concatenate  it_zimp_lanc_ctaux-cod_imposto it_zimp_cad_impaux-descr_imposto into wa_zib_contabil-sgtxt separated by '-'.

            wa_zib_contabil-waers_i   = 'BRL'. "IT_ZIMP_LANC_AUX-WAERS.
            if it_zimp_lanc_ctaux-valor_imp lt 0.
              wa_zib_contabil-dmbtr     = it_zimp_lanc_ctaux-valor_imp * -1.
            else.
              wa_zib_contabil-dmbtr     = it_zimp_lanc_ctaux-valor_imp .
            endif.

            if not it_zimp_lanc_aux-waers_f is initial.
              wa_zib_contabil-waers_f   = it_zimp_lanc_aux-waers_f.
              if it_zimp_lanc_ctaux-valor_for lt 0.
                wa_zib_contabil-dmbe2     = it_zimp_lanc_ctaux-valor_for * -1.
              else.
                wa_zib_contabil-dmbe2     = it_zimp_lanc_ctaux-valor_for .
              endif.
            else.
              wa_zib_contabil-waers_f   = ''.
              wa_zib_contabil-dmbe2     = 0.
            endif.

            wa_zib_contabil-rg_atualizado	=	'N'.
            concatenate  it_zimp_lanc_aux-dt_venc+6(2) it_zimp_lanc_aux-dt_venc+4(2) it_zimp_lanc_aux-dt_venc+0(4) into wa_zib_contabil-zfbdt separated by '.'.
            wa_zib_contabil-hbkid     = it_zimp_lanc_aux-hbkid.

            insert into  zib_contabil values wa_zib_contabil.
            if sy-subrc ne 0.
              rollback work.
            else.
              commit work.
            endif.
            clear  wa_zib_contabil.
          endif.
        endloop.
      endloop.
      wa_saida-icon = icon_led_yellow.
      modify it_saida from wa_saida index rs_selfield-tabindex transporting icon.
      perform f_carrega_dados using:
      wa_saida-icon          'ICON'   .

      rs_selfield-refresh = 'X'.
    else.
      message 'Documento já reprocessado ou  sem erros' type 'I'.
    endif.
  elseif r_ucomm eq '&EST'.

    data: lote        type zimp_cad_lote-lote,
          cont        type i,
          msg(107)    type c,
          p_erro(100) type c.

    cont = 1.

    loop at it_saida into wa_saida.
      if cont = 1.
        lote = wa_saida-lote.
      else.
        if lote <> wa_saida-lote.
          msg = 'Há mais de um lote em exibição. Voltar na tela de parâmetros e informar o lote individual a ser estornado.'.
        endif.
      endif.
      cont = cont + 1.
    endloop.

    if msg is not initial.
      message msg type 'I'.
    else.
      loop at it_saida into wa_saida.
        if wa_saida-doc_contabil is not initial.
          refresh it_bdcdata.
          perform f_bdc_data using:
            'SAPMF05A'  '0105'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'	      '=BU',
            ''          ''      ''   'RF05A-BELNS'      wa_saida-doc_contabil,
            ''          ''      ''   'BKPF-BUKRS'       wa_saida-bukrs,
            ''          ''      ''   'RF05A-GJAHS'      wa_saida-budat+0(4),
            ''          ''      ''   'UF05A-STGRD'      '01'.

          perform zf_call_transaction using 'FB08' changing p_erro.
        endif.
      endloop.
    endif.
  endif.

endform.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  append wa_bdcdata to it_bdcdata.

endform.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
form zf_call_transaction using p_trans changing p_erro.
  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  refresh it_msg.

  wl_mode = 'E'.
  call transaction p_trans using it_bdcdata
        mode wl_mode
        messages into it_msg.

  read table it_msg with key msgtyp = 'A'.
  if sy-subrc = 0.
    p_erro = 'X'.
  else.
    read table it_msg with key msgtyp = 'E'.
    if sy-subrc = 0.
      p_erro = 'X'.
    endif.
  endif.

  clear wg_documento.

  if sy-subrc = 0.
    move it_msg-msgv1 to wg_documento.
  endif.

  if  wg_documento is initial.
    p_erro = 'X'.
  else.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_documento
      importing
        output = wg_documento.
  endif.

endform.                    "ZF_CALL_TRANSACTION

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
form f_monta_top_of_page using p_list_top_of_page type
                               slis_t_listheader.

  data: t_header   type slis_listheader,
        v_data(10) type c.

  t_header-typ  = 'H'.
  t_header-info = 'Relatório de Lote Pagamento de Impostos '(t01).
  append t_header to p_list_top_of_page.
  clear t_header.
  write sy-datum using edit  mask '__.__.____' to v_data.
  concatenate 'Data : '(023)  v_data into t_header-key separated by
  space.
  t_header-typ  = 'S'.
  append t_header to p_list_top_of_page.

endform.                    " f_monta_top_of_page


*----------------------------------------------------------------------*
*       Form  f_carrega_dados
*----------------------------------------------------------------------*
*   Carrega dados para work-área dinâmica
*----------------------------------------------------------------------*
*      -->P_valor   valor
*      -->P_campo   campo
*----------------------------------------------------------------------*
form f_carrega_dados using    p_valor
                              p_campo.

*Aponta <fs_campo> para <wa_data>-campo montado
  assign component p_campo  of structure <wa_data> to <fs_campo>.

*Move valor para <fs_campo> que esta apontando p/<wa_data>-campo montado
  move p_valor to <fs_campo>.

endform.                    " f_carrega_dados
*&---------------------------------------------------------------------*
*&      Form  MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD      text
*      -->P_TABREF     text
*      -->P_TEXT       text
*      -->P_OUT        text
*      -->P_REF_FIELD  text
*----------------------------------------------------------------------*
form monta_fieldcat using p_field
*                          p_tab
                          p_tabref
                          p_text
                          p_out
                          p_ref_field.
**** Se o programa for um ALV, pode aproveitar p/ carregar fieldcat com
* os atributos necessários, caso não se trate de um ALV basta informar o
* campo de referencia, a tabela de referência, o campo  e a tabela.

  clear: s_fieldcat, wa_fcat_lvc.
  wa_fcat_lvc-fieldname   = s_fieldcat-fieldname   = p_field.
  wa_fcat_lvc-tabname     = s_fieldcat-tabname     = '<FS_DATA>'.
  wa_fcat_lvc-ref_table   = s_fieldcat-ref_tabname = p_tabref.
  wa_fcat_lvc-seltext     = s_fieldcat-seltext_l   = p_text.

  s_fieldcat-seltext_m    = p_text.
  s_fieldcat-seltext_l    = p_text.
  s_fieldcat-seltext_s    = p_text.

  wa_fcat_lvc-outputlen   = s_fieldcat-outputlen   = p_out.
  wa_fcat_lvc-ref_field   = s_fieldcat-ref_fieldname   = p_ref_field.

* carrega fieldcat do alv
  append s_fieldcat.

*inclui dados da work-área p/ tabela sem cab.
  append wa_fcat_lvc to lt_fcat_lvc.

endform.                    " monta_fieldcat
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
form alv_preenche_cat  using   p_campo  type c
                               p_desc   type c
                               p_tam    type c
                               p_hot    type c
                               p_zero   type c
                               p_soma   type c.

  data: wl_fcat type ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.

  if p_campo = 'ICON'.
    wl_fcat-icon      = 'X'.
  endif.

  append wl_fcat to it_fcat.
endform.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_iniciar_variaves .
  data:
    w_texto1(10),
    w_texto2(10),
    w_texto3(40),

    w_empresa_texto(40),
    w_exer_texto(40),
    w_per_texto(40),

    empresa             type c length 99,
    lote                type c length 99,
    cabec               type c length 50.


  v_report = sy-repid.

  w_texto3 = 'Lotes Pagamento de Impostos'.
  perform f_construir_cabecalho using 'H' w_texto3.

  if p_bukrs[] is not initial.

    select * into table @data(it_t001)
      from t001
     where bukrs in @p_bukrs.

    describe table it_t001 lines data(linhas_empresa).

    case linhas_empresa.
      when 1.
        read table it_t001 index 1 into data(wa_t001_).
        w_empresa_texto = 'Empresa    :'.
        concatenate w_empresa_texto wa_t001_-bukrs '-' wa_t001_-butxt into empresa separated by space.
      when others.
        clear: empresa.
        loop at it_t001 into wa_t001_.
          if empresa is not initial.
            empresa = empresa && ','.
          endif.
          empresa = |{ empresa } { wa_t001_-bukrs }|.
        endloop.
        empresa = |Empresas    :{ empresa }|.
    endcase.

    perform f_construir_cabecalho using 'S' empresa.

  endif.

  if p_lote is not initial.
    w_exer_texto = 'Lote  :'.
    concatenate w_exer_texto p_lote-low '-' p_lote-high  into lote separated by space.
    perform f_construir_cabecalho using 'S' lote.
  endif.

  if p_dtv is not initial.
    w_exer_texto = 'Data Vencimento :'.
    concatenate p_dtv-low+6(2) p_dtv-low+4(2) p_dtv-low+0(4)    into cabec separated by '.'.
    if p_dtv-high is not initial.
      concatenate cabec ' a ' p_dtv-high+6(2) '.' p_dtv-high+4(2) '.' p_dtv-high+0(4) into cabec .
    endif.
    concatenate w_exer_texto cabec  into cabec separated by space.
    perform f_construir_cabecalho using 'S' cabec.
  endif.

  if p_dtl is not initial.
    w_exer_texto = 'Data Lançamento contábil :'.
    concatenate p_dtl-low+6(2) p_dtl-low+4(2) p_dtl-low+0(4)    into cabec separated by '.'.
    if p_dtv-high is not initial.
      concatenate cabec ' a ' p_dtl-high+6(2) '.' p_dtl-high+4(2) '.' p_dtl-high+0(4) into cabec .
    endif.
    concatenate w_exer_texto cabec  into cabec separated by space.
    perform f_construir_cabecalho using 'S' cabec.
  endif.

endform.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  f_construir_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
form f_construir_cabecalho    using typ text.
  data: ls_line type slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  append ls_line to t_top.
endform.                    " F_CONSTRUIR_CABECALHO

*&---------------------------------------------------------------------*
*&      Module  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_lote input.
  data: tl_return_tab type table of ddshretval with header line,
        tl_dselc      type table of dselc      with header line.

  data: begin of tl_lote occurs 0,
          lote       type zimp_cad_lote-lote,
          descr_lote type zimp_cad_lote-descr_lote,
          bukrs      type zimp_cad_lote-bukrs,
        end of tl_lote.
  data: l_dynpfields like dynpread occurs 0 with header line.
  refresh l_dynpfields.
  clear   l_dynpfields.
  if p_bukrs is  initial.
    l_dynpfields-fieldname  = 'P_BUKRS'.
    append l_dynpfields.

    call function 'DYNP_VALUES_READ'
      exporting
        dyname     = sy-repid
        dynumb     = sy-dynnr
      tables
        dynpfields = l_dynpfields.
    read table l_dynpfields index 1.
    move l_dynpfields-fieldvalue to p_bukrs.
  endif.

  select lote descr_lote bukrs
    from zimp_cad_lote
    into table tl_lote
    where loekz = ''
  and status_lote ne 'A'
  and bukrs in p_bukrs.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'LOTE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_LOTE-LOTE'
      value_org       = 'S'
    tables
      value_tab       = tl_lote
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
endmodule.                 " SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status '0100'.
  set titlebar '0100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.
  data: "EVENT TYPE CNTL_SIMPLE_EVENT,
    "EVENTS TYPE CNTL_SIMPLE_EVENTS,
    tl_filter   type lvc_t_filt,
    wl_filter   type lvc_s_filt,
    tl_function type ui_functions,
    wl_function like tl_function with header line.

  wa_layout-zebra      = 'X'.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = 'X'.
  wa_layout-grid_title = ' '.

  "GRID2
  if obg_conteiner_estra is initial.
    create object obg_conteiner_estra
      exporting
        container_name = g_cc_estra.


    create object grid2
      exporting
        i_parent = obg_conteiner_estra.


    perform montar_layout_estra.

    refresh: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function to tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Estratégia de Liberação'.
    wa_layout-no_toolbar = 'X'.
    perform montar_layout_estra.

    call method grid2->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

    call method grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  else.
    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case ok-code.
    when 'SAIR'.
      set screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_estra .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZIMP_APROVADOR'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         'Valor de'      '15' ' ' ' ' ' ',
        1 'ZIMP_APROVADOR'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        'Valor ate'     '15' ' ' ' ' ' ',
        1 'ZIMP_APROVADOR'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        'Aprovador'     '20' ' ' ' ' ' ',
        1 ' '                        ' '               'TG_ESTRA' 'ESTADO'           'Estado'        '10' ' ' ' ' ' ',
        1 ' '                        ' '               'TG_ESTRA' 'OPCOES'           'Opções Liber.' '12' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_ESTRA

*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
*----------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_edit)
                            value(p_sum)
                            value(p_emphasize).

  clear w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  if p_outputlen is not initial.
    w_fieldcatalog-outputlen      = p_outputlen.
  endif.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  if p_field eq 'OPCOES'. " OR P_FIELD EQ 'DOC_LCTO'.
    w_fieldcatalog-hotspot = 'X'.
  endif.

  append w_fieldcatalog to t_fieldcatalog.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  F4_P_IMP
*&---------------------------------------------------------------------*
*       CS2017000143
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_p_cod .

  types: begin of ty_f4_p_cod,
           cod_imposto   type zimp_lanc_impost-cod_imposto,
           descr_imposto type zimp_cad_imposto-descr_imposto.
  types: end of ty_f4_p_cod.

  data: it_f4_p_cod         type standard table of ty_f4_p_cod,
        wa_f4_p_cod         type ty_f4_p_cod,
        it_zimp_cad_imposto type standard table of zimp_cad_imposto,
        wa_zimp_cad_imposto type zimp_cad_imposto,
        cont                type i.

  select distinct cod_imposto
  from zimp_lanc_impost
  into corresponding fields of table it_f4_p_cod.

  select *
    from zimp_cad_imposto
    into table it_zimp_cad_imposto
  for all entries in it_f4_p_cod
  where cod_imposto = it_f4_p_cod-cod_imposto.

  loop at it_f4_p_cod into wa_f4_p_cod.
    cont = sy-tabix.
    read table it_zimp_cad_imposto into wa_zimp_cad_imposto with key cod_imposto = wa_f4_p_cod-cod_imposto.
    if sy-subrc is initial.
      wa_f4_p_cod-descr_imposto = wa_zimp_cad_imposto-descr_imposto.
      modify it_f4_p_cod from wa_f4_p_cod index cont.
    endif.
  endloop.

  sort it_f4_p_cod by cod_imposto.
  delete adjacent duplicates from it_f4_p_cod comparing cod_imposto.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'COD_IMPOSTO'
      dynpprog        = sy-repid      " Program name
      dynpnr          = sy-dynnr      " Screen number
      dynprofield     = 'P_COD'            " F4 help need field
      value_org       = 'S'
    tables
      value_tab       = it_f4_p_cod   " F4 help values
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

endform.
*&---------------------------------------------------------------------*
*&      Form  F4_P_USER
*&---------------------------------------------------------------------*
*       CS2017000143
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_p_user .

  types: begin of ty_f4_p_user,
           usnam type zimp_cad_lote-usnam.
  types: end of ty_f4_p_user.

  data: it_f4_p_user type standard table of ty_f4_p_user.

  select distinct usnam
  from zimp_cad_lote
  into corresponding fields of table it_f4_p_user.

  sort it_f4_p_user by usnam.
  delete adjacent duplicates from it_f4_p_user comparing usnam.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'USNAM'
      dynpprog        = sy-repid      " Program name
      dynpnr          = sy-dynnr      " Screen number
      dynprofield     = 'P_USER'            " F4 help need field
      value_org       = 'S'
    tables
      value_tab       = it_f4_p_user   " F4 help values
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

endform.
*&---------------------------------------------------------------------*
*&      Form  F4_P_DEP
*&---------------------------------------------------------------------*
*       CS2017000143
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_p_dep .

  types: begin of ty_f4_p_dep,
           dep_resp      type zimp_cad_lote-dep_resp,
           dep_resp_desc type zimp_cad_depto-dep_resp_desc.
  types: end of ty_f4_p_dep.

  data: it_f4_p_dep       type standard table of ty_f4_p_dep,
        wa_f4_p_dep       type ty_f4_p_dep,
        it_zimp_cad_depto type standard table of zimp_cad_depto,
        wa_zimp_cad_depto type zimp_cad_depto,
        cont              type i.

  select distinct dep_resp
  from zimp_cad_lote
  into corresponding fields of table it_f4_p_dep.

  select *
    from zimp_cad_depto
    into table it_zimp_cad_depto
  for all entries in it_f4_p_dep
  where dep_resp = it_f4_p_dep-dep_resp.

  loop at it_f4_p_dep into wa_f4_p_dep.
    cont = sy-tabix.
    read table it_zimp_cad_depto into wa_zimp_cad_depto with key dep_resp = wa_f4_p_dep-dep_resp.
    if sy-subrc is initial.
      wa_f4_p_dep-dep_resp_desc = wa_zimp_cad_depto-dep_resp_desc.
      modify it_f4_p_dep from wa_f4_p_dep index cont.
    endif.
  endloop.

  sort it_f4_p_dep by dep_resp.
  delete adjacent duplicates from it_f4_p_dep comparing dep_resp.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'DEP_RESP'
      dynpprog        = sy-repid      " Program name
      dynpnr          = sy-dynnr      " Screen number
      dynprofield     = 'P_DEP'            " F4 help need field
      value_org       = 'S'
    tables
      value_tab       = it_f4_p_dep   " F4 help values
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

endform.
