*&---------------------------------------------------------------------*
*& Report  zsdr0113_5d
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zsdr0113_5d.

"110301 CS2023000290 Melhoria de Job de email Fluxo de Caixa Insumos

initialization.

  types: begin of ty_saida,
           vkbur       type vbak-vkbur,  " Escr.Venda
           vbeln_s     type vbak-vbeln, " Nr Simulador
           vbeln_p     type vbak-vbeln, " Nro OV Principal
           vbeln_g     type vbak-vbeln, " Nro OV
           auart       type vbak-auart, " Tipo O.V.
           safra       type zsdt0040-safra, " Safra
           meio_pago   type string, "Meio Pagamento
           kunnr       type vbak-kunnr, " Cod.cliente
           name1       type kna1-name1,  " Cliente
           text1       type t052u-text1, " Cond. Pgto
           waerk       type vbak-waerk, " Moeda
           salusd      type p decimals 2,
           salbrl      type p decimals 2,
           saldo       type decfloat34,
*           TOTVL_OV(15) TYPE P DECIMALS 2,
           data_venc   type char10, "ZFIT0026-DATA_VENC, " Dt.Vcto.
           flux_cx(20) type c,
         end of ty_saida.

  data: begin of it_entrada occurs 0,
          werks       type vbap-werks,
          vkbur       type vbak-vkbur,
          kunnr       type vbak-kunnr,
          name1       type kna1-name1,
          auart       type vbak-auart,
          zterm       type vbkd-zterm,
          text1       type t052u-text1,
          vbeln_s     type vbak-vbeln,
          vbeln_p     type vbak-vbeln,
          vbeln       type vbak-vbeln,
          vbeln_g     type vbak-vbeln,
          erdat       type vbak-erdat,
          waerk       type vbak-waerk,
          totalq_ov   type zfit0026-mont_moeda,
          totvl_ov    type zfit0026-mont_moeda,
          netwr_l     type vbap-netwr,
          mwsbp       type vbap-mwsbp,
          data_venc   type zfit0026-data_venc,
          forma_pag   type zfit0026-forma_pag,
          taxa        type zfit0026-taxa,
          mont_moeda  type zfit0026-mont_moeda,
          mont_mi     type zfit0026-mont_mi,
          docnum      type zfit0026-docnum,
          moeda_forte type zfit0026-mont_moeda,
          moeda_inter type zfit0026-mont_mi,
          augbl       type bsad-augbl,
          budat       type bsad-budat,
          dmbe2       type bsad-dmbe2,
          dmbtr       type bsad-dmbtr,
          salus       type bsad-dmbe2,
          salre       type bsad-dmbtr,
          rfmng       type rfmng,
          observacao  type zfit0026-observacao,
          safra       type zsdt0040-safra,
          meio_pago   type string.
  data: end of it_entrada.

  types: begin of ty_vkbur,
           vkbur type vbak-vkbur,
           text  type tvkbt-bezei,
         end of ty_vkbur.




  field-symbols: <t_data>      type any table,
                 <t_data_line> type any table,
                 <w_data>      type any,
                 <w_data_line> type any.

  data: l_data            type ref to data,
        l_data_line       type ref to data,
        l_data_descr      type ref to cl_abap_datadescr,
        l_data_line_descr type ref to cl_abap_datadescr.

  data: it_saida_brl type table of ty_saida,
        wa_saida_brl type ty_saida,
        it_saida_usd type table of ty_saida,
        wa_saida_usd type ty_saida,
        it_saida     type table of ty_saida initial size 0,
        it_saida_xls type table of ty_saida initial size 0,
        wa_saida     type ty_saida,
        it_vkbur     type table of ty_vkbur,
        wa_vkbur     type ty_vkbur.

  data: it_rsparams type table of rsparams,
        wa_rsparams type rsparams.

  data: lt_mailsubject     type sodocchgi1,
        lt_mailrecipientes type standard table of somlrec90 initial size 0,
        lt_mailtxt         type standard table of soli initial size 0.


  data dt_vencimento type sy-datum.
  data dt_vencimento_txt(12) type c.
  data lr_vkbur type range of vbak-vkbur.
  data aux_empresa type standard table of vbak-vkbur with header line.
  data wa_empresa type vbak-vkbur.


  data lt_bintab type solix_tab. "TYPE solix_tab.
  data dt_exec(12) type c.
  data dt_exec_sendfile(10) type c.
  data nm_sendfile type string.
  data nm_titulo type so_obj_des.
  data nm_subject type sood-objdes.
  data lv_size     type i.
  data it_contents type standard table of solisti1.
  data     html        type solisti1.

  data: vg_job      type i.

start-of-selection.

  perform busca_dados_zsdt0060.

  perform trata_dados.

  perform monta_email.



*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_ZSDT0060
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados_zsdt0060 .


  clear it_rsparams[].

  wa_rsparams-selname  = 'RB1'.
  wa_rsparams-kind     = 'P'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = ''.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'RB2'.
  wa_rsparams-kind     = 'P'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = 'X'.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'RB3'.
  wa_rsparams-kind     = 'P'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = ''.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'RB4'.
  wa_rsparams-kind     = 'P'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = ''.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'RB5'.
  wa_rsparams-kind     = 'P'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = ''.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'RB6'.
  wa_rsparams-kind     = 'P'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = 'X'.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'P_BUKRS'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = '0001'.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'P_AUART'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = 'ZFTE'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZDEF'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZREM'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZOFE'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZTRI'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZSEM'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZOFM'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZODF'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZFUT'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZOSM'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZCOP'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZRPF'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZROB'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZCPV'.
  append wa_rsparams to it_rsparams.
  wa_rsparams-low      = 'ZREB'.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  wa_rsparams-selname  = 'P_VKBUR'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'NE'.
  wa_rsparams-low      = '0'.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  perform z_trata_data.

  wa_rsparams-selname  = 'P_VENC'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = dt_vencimento.
  append wa_rsparams to it_rsparams.
  clear wa_rsparams.

  append value #( selname = 'P_EXTR' kind = 'P' sign = 'I' option = 'EQ' low = 'X' ) to it_rsparams.


**<<<------ir226476 /bug #169582 / aoenning------>>>
* Veridica se o processamento é background.
*  if not sy-batch is initial.
*    data(lv_jobname) = 'ZSDR0113_5D'.
*    select single count(*)
*      from tbtco
*      into @data(gv_job)
*    where jobname eq @lv_jobname
*      and status eq @sy-abcde+17(1). "R - Running
*
*    if vg_job ne 1.
** &2 job atualmente em execução.
*      message s001(finoc) with lv_jobname.
*      exit.
*    endif.
*    clear vg_job.
*  endif.

if sy-batch eq abap_true.
    try.
        zcl_job=>get_ck_program_execucao( exporting i_nome_program = sy-cprog importing e_qtd = data(e_qtd) ).
      catch zcx_job.
    endtry.

    if e_qtd gt 1.
      leave program.
    endif.
  endif.

**<<<------IR226476 /BUG #169582 / AOENNING------>>>


  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = sy-tabix
      text       = 'Extraindo dados em ZSDT0060'.

  perform f_prepare_run_time_info.

  submit zsdr0020 with selection-table it_rsparams
  and return.

  perform f_get_runtime_info.

  if <t_data> is assigned.
    loop at <t_data> assigning <w_data>.
      clear: it_entrada.
      move-corresponding <w_data> to it_entrada.
      append it_entrada.
    endloop.
  endif.

endform.

form f_prepare_run_time_info.

  if <t_data> is assigned.
    clear: <t_data>[].
  endif.

  if <t_data_line> is assigned.
    clear: <t_data_line>[].
  endif.

  if <t_data> is assigned.
    clear: <t_data>.
  endif.

  if <t_data_line> is assigned.
    clear: <t_data_line>.
  endif.

  free: l_data,  l_data_line,  l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( exporting display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).
endform.


form f_get_runtime_info.

  try.
      cl_salv_bs_runtime_info=>get_data_ref(
        importing
          r_data_descr      = l_data_descr
          r_data_line_descr = l_data_line_descr ).

      check ( l_data_descr is not initial ) or ( l_data_line_descr is  not initial ).

      create data l_data      type handle  l_data_descr.
      create data l_data_line type handle  l_data_line_descr.

      assign l_data->* to <t_data>.
      assign l_data_line->* to <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( importing t_data      = <t_data>
                                                   t_data_line = <t_data_line> ).
    catch cx_salv_bs_sc_runtime_info.
  endtry.

  cl_salv_bs_runtime_info=>clear_all( ).

  assign l_data->*        to <w_data>.
  assign l_data_line->*   to <w_data_line>.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_trata_data.


  types: begin of ty_data,
           dia  type i,
           date type iscal_day-date,
         end of ty_data.

  data: dt_atual type p0001-begda,
        mes      like t5a4a-dlymo,
        ano      like t5a4a-dlyyr,
        dia      type t5a4a-dlydy value 1.

  data: vg_last_day       type sy-datum,
        it_sab_dom_fer    type table of iscal_day,
        it_day_attributes type table of  casdayattr,
        dt_fim            like p0001-begda..

  data: it_data type table of ty_data,
        wa_data type ty_data.

  free: it_sab_dom_fer, it_day_attributes, it_data.

**********************************************************************
*Se for Executado em QAS "110301 CS2023000290 Melhoria de Job de email Fluxo de Caixa Insumos
**********************************************************************
  if sy-sysid = 'QAS'.
    clear: dt_atual.
    dt_atual = '20210318'."sy-datum.
  else.
    dt_atual = sy-datum.
  endif.
**********************************************************************

  concatenate sy-datum+0(4) '1201' into dt_fim.

  call function 'FKK_LAST_DAY_OF_MONTH'
    exporting
      day_in            = dt_fim
    importing
      last_day_of_month = vg_last_day.


  call function 'DAY_ATTRIBUTES_GET'
    exporting
      date_from      = dt_atual
      date_to        = vg_last_day
      language       = sy-langu
    tables
      day_attributes = it_day_attributes.


  call function 'HOLIDAY_GET'
    exporting
      factory_calendar = 'ZF'
      date_from        = dt_atual
      date_to          = vg_last_day
    tables
      holidays         = it_sab_dom_fer.


  delete it_day_attributes where date    eq dt_atual.
  delete it_day_attributes where weekday eq '6'.
  delete it_day_attributes where weekday eq '7'.


  loop at it_day_attributes into data(wa_day).
    read table it_sab_dom_fer into data(wa_sab) with key date = wa_day-date.
    if sy-subrc = 4.

      wa_data-dia  =  dia.
      wa_data-date = wa_day-date.
      append wa_data to it_data.
      clear wa_data.
      dia = dia + 1.

    endif.
  endloop.

  read table it_data into wa_data with key dia = 5."3. PSA 72hrs
  dt_vencimento = wa_data-date.
  dt_vencimento_txt = | { dt_vencimento+6(2) }/{ dt_vencimento+4(2) }/{ dt_vencimento+0(4) } |.

endform.
*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form trata_dados.
  data: obj_taxa type ref to zcl_util_sd.
  data: s_data  type gdatu_inv,
        s_ukurs type ukurs_curr.

  free it_vkbur[].

  s_data = dt_vencimento.


  select distinct * from tvkbt into table  @data(it_tvkbt)
    for all entries in @it_entrada
    where vkbur eq @it_entrada-vkbur
    and   spras eq @sy-langu.


  create object obj_taxa.
  obj_taxa->set_kurst( 'B' ).
  obj_taxa->set_waerk( 'USD' ).
  obj_taxa->set_tcurr( 'BRL' ).

  obj_taxa->set_data( s_data ).
  s_ukurs = obj_taxa->taxa_cambio( ).


  loop at it_entrada into data(wa_entrada)
    where ( zterm = 'I005' or zterm = 'I003' )
    and    waerk = 'BRL'.

    read table it_tvkbt into data(wa_tvkbt) with key vkbur =  wa_entrada-vkbur.

    wa_vkbur-vkbur = wa_entrada-vkbur.
    wa_vkbur-text  = wa_tvkbt-bezei.
    translate wa_vkbur-text to lower case.
    append wa_vkbur to it_vkbur.

    wa_saida_brl-vkbur        = wa_entrada-vkbur.
    wa_saida_brl-vbeln_s      = wa_entrada-vbeln_s.
    wa_saida_brl-vbeln_p      = wa_entrada-vbeln_p.
    wa_saida_brl-vbeln_g      = wa_entrada-vbeln_g.
    wa_saida_brl-auart        = wa_entrada-auart.
    wa_saida_brl-safra        = wa_entrada-safra.
    wa_saida_brl-kunnr        = wa_entrada-kunnr.
    wa_saida_brl-name1        = wa_entrada-name1.
    wa_saida_brl-text1        = wa_entrada-text1.
    wa_saida_brl-waerk        = wa_entrada-waerk.
    wa_saida_brl-salbrl       = abs( wa_entrada-moeda_inter ).

    concatenate wa_entrada-data_venc+6(2) '.'  wa_entrada-data_venc+4(2) '.'  wa_entrada-data_venc+0(4) into wa_saida_brl-data_venc.

    select single
      case
      when meio_pago = 'U' then 'Deposito em Conta'
      when meio_pago = 'A' then 'Acerto'
      when meio_pago = 'D' then 'Boleto Bancário'
      when meio_pago = 'X' then 'Pix'
      end as meio_pago
      from zsdt0040 where doc_simulacao = @wa_entrada-vbeln_s into ( @wa_saida_brl-meio_pago ).


    append wa_saida_brl to it_saida_brl.


**********************************************************************
*Verifica se te um Nº da OV na Transação ZFI0102 - Programa ZFIR0068 - Tabela ZFIT0115 "110301 CS2023000290 Melhoria de Job de email Fluxo de Caixa Insumos
**********************************************************************

    select single vbeln
      into @data(wa_contem_vbeln_brl)
      from zfit0115 where vbeln = @wa_entrada-vbeln_g .

    if wa_contem_vbeln_brl is not initial.
      delete it_saida_brl where vbeln_g = wa_contem_vbeln_brl.
    endif.

**********************************************************************

    clear: wa_saida_brl, wa_entrada, wa_vkbur,wa_contem_vbeln_brl.
  endloop.


  loop at it_entrada into wa_entrada
    where ( zterm = 'I005' or zterm = 'I003' )
    and    waerk = 'USD'.


    read table it_tvkbt into wa_tvkbt with key vkbur =  wa_entrada-vkbur.

    wa_vkbur-vkbur = wa_entrada-vkbur.
    wa_vkbur-text  = wa_tvkbt-bezei.
    translate wa_vkbur-text to lower case.

    append wa_vkbur to it_vkbur.

    wa_saida_usd-vkbur        = wa_entrada-vkbur.
    wa_saida_usd-vbeln_s      = wa_entrada-vbeln_s.
    wa_saida_usd-vbeln_p      = wa_entrada-vbeln_p.
    wa_saida_usd-vbeln_g      = wa_entrada-vbeln_g.
    wa_saida_usd-auart        = wa_entrada-auart.
    wa_saida_usd-safra        = wa_entrada-safra.
    wa_saida_usd-kunnr        = wa_entrada-kunnr.
    wa_saida_usd-name1        = wa_entrada-name1.
    wa_saida_usd-text1        = wa_entrada-text1.
    wa_saida_usd-waerk        = wa_entrada-waerk.
    wa_saida_usd-salusd       = abs( wa_entrada-moeda_forte ).

    concatenate wa_entrada-data_venc+6(2) '.'  wa_entrada-data_venc+4(2) '.'  wa_entrada-data_venc+0(4) into wa_saida_usd-data_venc.

    select single ""110301 CS2023000290 Melhoria de Job de email Fluxo de Caixa Insumos
      case
      when meio_pago = 'U' then 'Deposito em Conta'
      when meio_pago = 'A' then 'Acerto'
      when meio_pago = 'D' then 'Boleto Bancário'
      when meio_pago = 'X' then 'Pix'
      end as meio_pago
      from zsdt0040 where doc_simulacao = @wa_entrada-vbeln_s into ( @wa_saida_usd-meio_pago ).

    append wa_saida_usd to it_saida_usd.


**********************************************************************
*Verifica se te um Nº da OV na Transação ZFI0102 - Programa ZFIR0068 - Tabela ZFIT0115 "110301 CS2023000290 Melhoria de Job de email Fluxo de Caixa Insumos
**********************************************************************

    select single vbeln
      into @data(wa_contem_vbeln_usd)
      from zfit0115 where vbeln = @wa_saida_usd-vbeln_g .

    if wa_contem_vbeln_usd is not initial.
      delete it_saida_usd where vbeln_g = wa_contem_vbeln_usd.
    endif.
**********************************************************************
    clear: wa_saida_usd, wa_entrada, wa_vkbur, wa_contem_vbeln_usd.

  endloop.

  sort it_saida_brl by salbrl descending.
  sort it_saida_usd by salusd descending.
  sort it_vkbur     by vkbur.
  delete it_vkbur where vkbur not in lr_vkbur.

  delete adjacent duplicates from it_vkbur.
  delete adjacent duplicates from it_saida_brl.
  delete adjacent duplicates from it_saida_usd.
  delete it_saida_brl where salbrl = 0 .
  delete it_saida_brl where meio_pago = 'Acerto' .
  delete it_saida_brl where meio_pago is initial .
  delete it_saida_usd where salusd = 0 .
  delete it_saida_usd where meio_pago = 'Acerto' .
  delete it_saida_usd where meio_pago is initial .


  append lines of it_saida_brl to it_saida.
  append lines of it_saida_usd to it_saida.

  loop at it_saida assigning field-symbol(<it_saida>).

    data valor type decfloat34.

    if <it_saida>-salbrl is not initial.
      valor = <it_saida>-salbrl.
    endif.

    if <it_saida>-salusd is not initial.
      valor = <it_saida>-salusd.

    endif.

    <it_saida>-saldo = valor.
    clear: valor.

    aux_empresa = <it_saida>-vkbur.
    append aux_empresa.

  endloop.

  sort it_saida by vkbur waerk saldo.
  delete it_saida where vkbur is initial.


  delete adjacent duplicates from aux_empresa.


endform.
*&---------------------------------------------------------------------*
*&      Form  MONTA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form monta_email .

  if it_saida is not initial.


    perform envia.


  else.

    exit.

  endif.

endform.

form envia.

*** Stefanini - IR229413 - 27/05/2025 - LAZAROSR - Início de Alteração
  SORT aux_empresa.

  DELETE ADJACENT DUPLICATES FROM aux_empresa.
*** Stefanini - IR229413 - 27/05/2025 - LAZAROSR - Fim de Alteração

  loop at aux_empresa into wa_empresa.

    append lines of it_saida to it_saida_xls.

    delete it_saida_xls where vkbur <> wa_empresa.

    perform monta_dados.

**********************************************************************
*Envia E-mail
**********************************************************************
    data main_text      type bcsy_text.

    data : it_contents type standard table of solisti1,
           html        type standard table of solisti1 with header line.

    data lt_att_head type soli_tab.
    data xhtml_string type xstring.
    data t_hex type solix_tab.
* create persistent send request
    data(send_request) = cl_bcs=>create_persistent( ).
*Texto do Corpo E-mail
    nm_sendfile = |CR INS FLUXO DE CAIXA { wa_empresa } Venc. { dt_vencimento_txt }|.
    nm_subject = nm_sendfile.


    append '<html>' to html.
    append '<style>' to html.
    append 'table, th, td {border:1px solid black;}' to html.
    append '</style>' to html.
    append '<body>' to html.
    append 'Senhores(as) segue relação de contas a receber insumos para os próximos 5 dias úteis.<br>' to html.
    append 'Precisamos do retorno se haverá depósito o quanto antes sobre a confirmação da OV abaixo, devido ao impacto em fluxo de caixa e hedge.' to html.
    append '<br>' to html.
    append '<br>' to html.
    append '<i>Favor informar na coluna Fluxo de Caixa Deposito ou Acerto.</i>' to html.
    append '<br>' to html.
    append '<br>' to html.
    append '<br>' to html.
    append '<table style="width:100%">' to html.
    append '<tr>'to html.
    append '<th>Escr.Venda</th>' to html.
    append '<th>Nr Simulador</th>' to html.
    append '<th>Nro OV Principal</th>' to html.
    append '<th>Nro O.V</th>' to html.
    append '<th>Tipo O.V.</th>' to html.
    append '<th>Safra</th>' to html.
    append '<th>Meio Pagamento</th>' to html.
    append '<th>Cod.Cliente</th>' to html.
    append '<th>Cliente</th>' to html.
    append '<th>Cond.Pgto</th>' to html.
    append '<th>Moeda</th>' to html.
    append '<th>Vlr. a Receber O.V</th>' to html.
    append '<th>Dt.Vcto</th>' to html.
    append '<th>Fluxo de Caixa</th>' to html.
    append '</tr>' to html.

    loop at it_saida_xls into wa_saida .

      data saldo type string.
      saldo = |{ wa_saida-saldo sign = leftspace number = raw }|.
      replace '.' in saldo with ','.

      append '<tr>' to html.
      concatenate '<td>' wa_saida-vkbur '</td>' into html.    append html.
      concatenate '<td>' wa_saida-vbeln_s '</td>' into html.    append html.
      concatenate '<td>' wa_saida-vbeln_p '</td>' into html.    append html.
      concatenate '<td>' wa_saida-vbeln_g '</td>' into html.    append html.
      concatenate '<td>' wa_saida-auart '</td>' into html.    append html.
      concatenate '<td>' wa_saida-safra '</td>' into html.    append html.
      concatenate '<td>' wa_saida-meio_pago '</td>' into html.    append html.
      concatenate '<td>' wa_saida-kunnr '</td>' into html.    append html.
      concatenate '<td>' wa_saida-name1 '</td>' into html.    append html.
      concatenate '<td>' wa_saida-text1 '</td>' into html.    append html.
      concatenate '<td>' wa_saida-waerk '</td>' into html.    append html.
      concatenate '<td>' saldo '</td>' into html.    append html.
      concatenate '<td>' wa_saida-data_venc '</td>' into html.    append html.
      append '<td></td>' to html.
      append '</tr>' to html.

    endloop.

    clear: saldo.

    append '</table>' to html.
    append '</body>' to html.
    append '</html>' to html.

    append lines of html to it_contents.

    data(document) = cl_document_bcs=>create_document(
      i_type    = 'HTM'
      i_text    = it_contents
      "i_text = main_text
      i_subject = |{ nm_sendfile }| ). "Título do e-mail
    append |<(>&< )>SO_FILENAME={ nm_sendfile }.xlsx| to lt_att_head.
* add the spread sheet as attachment to document object
    document->add_attachment(
      i_attachment_type    = 'xls'
      i_attachment_subject = nm_subject
      i_attachment_size    = conv so_obj_len( lv_size )
      i_attachment_header  = lt_att_head
      i_att_content_hex    = lt_bintab ).

    send_request->set_document( document ).

**********************************************************************
*Pega email da TVARC e adiciona ao recipient
**********************************************************************

    data: lt_mail         type standard table of tvarvc,
          email_addresses type adr6-smtp_addr.

**********************************************************************
*Se for Executado em QAS "110301 CS2023000290 Melhoria de Job de email Fluxo de Caixa Insumos
**********************************************************************
    if sy-sysid = 'QAS'.
      append value #( receiver =   'samuel.cabana@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) to lt_mailrecipientes.
      append value #( receiver =   'pablo.alves@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) to lt_mailrecipientes.
      append value #( receiver =   'marlla.lucas@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) to lt_mailrecipientes.
      append value #( receiver =   'larissa.pereira@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) to lt_mailrecipientes.
    else.

      select email as receiver
      from zsdt0060 into table @data(it_zsdt0060)
    where vkbur   = @wa_empresa
      and programa eq 'ZSDR016'
      and email like '%@%'.

      delete it_zsdt0060 where receiver is initial.

      sort it_zsdt0060 by receiver descending.

      loop at it_zsdt0060 into data(aux_email).
        append value #( receiver = aux_email-receiver  rec_type =   'U'  rec_date =   '00000000' ) to lt_mailrecipientes.
      endloop.
    endif.
**********************************************************************

    loop at lt_mailrecipientes assigning field-symbol(<address>).
      email_addresses = <address>-receiver.
      translate email_addresses to lower case.

      try.
          data(recipient) = cl_cam_address_bcs=>create_internet_address( email_addresses ).
          send_request->add_recipient( recipient ).
        catch cx_address_bcs.

      endtry.



    endloop.
**********************************************************************

    data(sent_to_all) = send_request->send( i_with_error_screen = 'X' ).

    commit work.

    clear: html,it_saida_xls,wa_saida,it_contents,lt_bintab,lv_size,lt_mailrecipientes,lt_att_head,recipient,it_zsdt0060,wa_empresa,aux_email.
    free: html,it_saida_xls,wa_saida,it_contents,lt_bintab,lv_size,lt_mailrecipientes,lt_att_head,recipient,it_zsdt0060,wa_empresa,aux_email.

  endloop.

endform.

form monta_dados.

  "CLEAR: it_saida_xls.

**********************************************************************
*Monta Colunas Excel
**********************************************************************

  data:
    lr_table     type ref to cl_salv_table,
    o_functions  type ref to cl_salv_functions_list,
    o_selections type ref to cl_salv_selections.

  data:
    o_columns type ref to cl_salv_columns_table,
    o_column  type ref to cl_salv_column_table.

  cl_salv_table=>factory( importing r_salv_table = lr_table
                          changing  t_table      = it_saida_xls ).

  o_functions = lr_table->get_functions( ).
  o_functions->set_all( abap_true ).

  o_columns = lr_table->get_columns( ).
  o_columns->set_optimize( abap_true ).



  try.
      o_column ?= o_columns->get_column( 'VKBUR' ).
      o_column->set_short_text( 'Escritório' ).
      o_column->set_medium_text( 'Escritório' ).
      o_column->set_long_text( 'Escritório' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'VBELN_S' ).
      o_column->set_short_text( 'Nº Sim.' ).
      o_column->set_medium_text( 'Nº Sim.' ).
      o_column->set_long_text( 'Nº Sim.' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'VBELN_P' ).
      o_column->set_short_text( 'Nº O.V P.' ).
      o_column->set_medium_text( 'Nº O.V P.' ).
      o_column->set_long_text( 'Nº O.V P.' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'VBELN_G' ).
      o_column->set_short_text( 'Nº O.V' ).
      o_column->set_medium_text( 'Nº O.V' ).
      o_column->set_long_text( 'Nº O.V' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'AUART' ).
      o_column->set_short_text( 'Tipo O.V' ).
      o_column->set_medium_text( 'Tipo O.V' ).
      o_column->set_long_text( 'Tipo O.V' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'SAFRA' ).
      o_column->set_short_text( 'Safra' ).
      o_column->set_medium_text( 'Safra' ).
      o_column->set_long_text( 'Safra' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'MPAGDESC' ).
      o_column->set_short_text( 'Meio Pag.' ).
      o_column->set_medium_text( 'Meio Pag.' ).
      o_column->set_long_text( 'Meio pag.' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'KUNNR' ).
      o_column->set_short_text( 'Cd.Cli.' ).
      o_column->set_medium_text( 'Cd.Cli.' ).
      o_column->set_long_text( 'Cd.Cli.' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'NAME1' ).
      o_column->set_short_text( 'Cliente' ).
      o_column->set_medium_text( 'Cliente' ).
      o_column->set_long_text( 'Cliente' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'TEXT1' ).
      o_column->set_short_text( 'Cond.Pgto' ).
      o_column->set_medium_text( 'Cond.Pgto' ).
      o_column->set_long_text( 'Cond.Pgto' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'WAERK' ).
      o_column->set_short_text( 'Moeda' ).
      o_column->set_medium_text( 'Moeda' ).
      o_column->set_long_text( 'Moeda' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'SALDO' ).
      o_column->set_short_text( 'Vlr.O.V' ).
      o_column->set_medium_text( 'Vlr.O.V' ).
      o_column->set_long_text( 'Vlr.O.V' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'DATA_VENC' ).
      o_column->set_short_text( 'Dt.Vcto' ).
      o_column->set_medium_text( 'Dt.Vcto' ).
      o_column->set_long_text( 'Dt.Vcto' ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'FLUX_CX' ).
      o_column->set_short_text( 'F. Caixa' ).
      o_column->set_medium_text( 'Fluxo de Caixa' ).
      o_column->set_long_text( 'Fluxo de Caixa' ).
    catch cx_salv_not_found.
  endtry.

**********************************************************************
  try.
      o_column ?= o_columns->get_column( 'SALBRL' ).
      o_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      o_column ?= o_columns->get_column( 'SALUSD' ).
      o_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      o_column ?= o_columns->get_column( 'LANCTB' ).
      o_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      o_column ?= o_columns->get_column( 'MEIO_PAGO' ).
      o_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.


  try.
      o_column ?= o_columns->get_column( 'SALUSD' ).
      o_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.

  try.
      o_column ?= o_columns->get_column( 'SALBRL' ).
      o_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.


**********************************************************************
*Lê tabela e convert para XML e depois XLS
**********************************************************************

  data: lr_xldimension type ref to if_ixml_node,
        lr_xlworksheet type ref to if_ixml_element.

  data(lv_xlsx) = lr_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).
  data(lr_zip) = new cl_abap_zip( ).
  lr_zip->load( lv_xlsx ).
  lr_zip->get( exporting name = 'xl/worksheets/sheet1.xml' importing content = data(lv_file) ).

  data(lr_file) = new cl_xml_document( ).
  lr_file->parse_xstring( lv_file ).
* Row elements are under SheetData
  data(lr_xlnode) = lr_file->find_node( 'sheetData' ).
  data(lr_xlrows) = lr_xlnode->get_children( ).
* Create new element in the XML file
  lr_xlworksheet ?= lr_file->find_node( 'worksheet' ).
  data(lr_xlsheetpr)   = cl_ixml=>create( )->create_document( )->create_element( name = 'sheetPr' ).
  data(lr_xloutlinepr) = cl_ixml=>create( )->create_document( )->create_element( name = 'outlinePr' ).
  lr_xlsheetpr->if_ixml_node~append_child( lr_xloutlinepr ).
  lr_xloutlinepr->set_attribute( name = 'summaryBelow' value = 'false' ).
  lr_xldimension ?= lr_file->find_node( 'dimension' ).
  lr_xlworksheet->if_ixml_node~insert_child( new_child = lr_xlsheetpr ref_child = lr_xldimension ).
* Create xstring and move it to XLSX
  lr_file->render_2_xstring( importing stream = lv_file ).
  lr_zip->delete( exporting name = 'xl/worksheets/sheet1.xml' ).
  lr_zip->add( exporting name = 'xl/worksheets/sheet1.xml' content = lv_file ).
  lv_xlsx = lr_zip->save( ).


* Convert to binary
  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = lv_xlsx
    importing
      output_length = lv_size
    tables
      binary_tab    = lt_bintab.
endform.
