*&---------------------------------------------------------------------*
*& Report  ZSDR0115
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zsdr0115.

tables: vbak, zsdt0060.

initialization.


  types: begin of ty_saida,
           vkbur         type vbak-vkbur,  " Escr.Venda
           vbeln_s       type vbak-vbeln, " Nr Simulador
           vbeln_p       type vbak-vbeln, " Nro OV Principal
           vbeln_g       type vbak-vbeln, " Nro OV
           auart         type vbak-auart, " Tipo O.V.
           safra         type zsdt0040-safra, " Safra
           kunnr         type vbak-kunnr, " Cod.cliente
           name1         type kna1-name1,  " Cliente
           text1         type t052u-text1, " Cond. Pgto
           waerk         type vbak-waerk, " Moeda
           salusd        type p decimals 2, "saldo da OV
           salbrl        type p decimals 2, "saldo da OV
           sald_fin      type zfit0026-mont_rbdo, "saldo de Juros/multa
           sald_fin_brl  type zfit0026-mont_rbdo, "saldo de Juros/multa
           data_venc     type char10, "ZFIT0026-DATA_VENC, " Dt.Vcto.
           data_venc_ord type zfit0026-data_venc, "ZFIT0026-DATA_VENC, " Dt.Vcto.
           pd_entregue   type p decimals 2,
         end of ty_saida.


  types: begin of ty_entrada,
           werks            type vbap-werks,
           vkbur            type vbak-vkbur,
           kunnr            type vbak-kunnr,
           name1            type kna1-name1,
           auart            type vbak-auart,
           zterm            type vbkd-zterm,
           text1            type t052u-text1,
           vbeln_s          type vbak-vbeln,
           vbeln_p          type vbak-vbeln,
           vbeln            type vbak-vbeln,
           vbeln_g          type vbak-vbeln,
           erdat            type vbak-erdat,
           waerk            type vbak-waerk,
           totalq_ov        type zfit0026-mont_moeda,
           totvl_ov         type zfit0026-mont_moeda,
           netwr_l          type vbap-netwr,
           mwsbp            type vbap-mwsbp,
           data_venc        type zfit0026-data_venc,
           forma_pag        type zfit0026-forma_pag,
           taxa             type zfit0026-taxa,
           mont_moeda       type zfit0026-mont_moeda,
           mont_mi          type zfit0026-mont_mi,
           docnum           type zfit0026-docnum,
           moeda_forte      type zfit0026-mont_moeda,
           moeda_inter      type zfit0026-mont_mi,
           vlr_sald_fin     type zfit0026-mont_rbdo,
           vlr_sald_fin_brl type zfit0026-mont_rbdo,
           augbl            type bsad-augbl,
           budat            type bsad-budat,
           dmbe2            type bsad-dmbe2,
           dmbtr            type bsad-dmbtr,
           salus            type bsad-dmbe2,
           salre            type bsad-dmbtr,
           rfmng            type rfmng,
           observacao       type zfit0026-observacao,
           safra            type zsdt0040-safra,
         end of ty_entrada.


  types: begin of ty_vkbur,
           vkbur type vbak-vkbur,
           text  type tvkbt-bezei,
         end of ty_vkbur.



  types: begin of t_vkbur,
           vkbur type vbak-vkbur,
         end of t_vkbur.

  field-symbols: <t_data>      type any table,
                 <t_data_line> type any table,
                 <w_data>      type any,
                 <w_data_line> type any.

  data: l_data            type ref to data,
        l_data_line       type ref to data,
        l_data_descr      type ref to cl_abap_datadescr,
        l_data_line_descr type ref to cl_abap_datadescr.


  data: it_saida_brl      type table of ty_saida,
        wa_saida_brl      type ty_saida,
        it_saida_usd      type table of ty_saida,
        wa_saida_usd      type ty_saida,
        it_zsdt0060       type table of zsdt0060,
        wa_zsdt0060       type zsdt0060,
        it_vkbur          type table of ty_vkbur,
        wa_vkbur          type ty_vkbur,
        t_filial          type table of t_vkbur,
        it_entrada        type table of ty_entrada,
        wa_entrada        type ty_entrada,
        it_entrada_aux    type table of ty_entrada,
        wa_entrada_aux    type ty_entrada,
        it_entrada_agrupa type table of ty_entrada,
        wa_entrada_agrupa type ty_entrada.

  data: it_rsparams type table of rsparams,
        wa_rsparams type rsparams.

  data: lt_mailsubject     type sodocchgi1,
        lt_mailrecipientes type standard table of somlrec90 with header line,
        lt_mailtxt         type standard table of soli with header line.

  data: dt_inicio type sy-datum,
        dt_fim    type sy-datum.

  data: vl_dt_ini_venc type sy-datum,
        vl_dt_fim_venc type sy-datum.

  data: vg_job      type i.

*-CS2021000204 - 29.07.2021 - JT - inicio
  selection-screen begin   of block b1 with frame title text-001.
    select-options: s_vkbur  for vbak-vkbur,
                    s_vbelns for vbak-vbeln,
                    s_usnam  for zsdt0060-usnam.
  selection-screen end     of block b1.
*-CS2021000204 - 29.07.2021 - JT - fim

start-of-selection.
  perform busca_dados_zsdt0060.
  perform tratar_dados.
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

  append value #( selname = 'RB1' kind = 'P' sign = 'I' option = 'EQ' low = '' ) to it_rsparams.
  append value #( selname = 'RB2' kind = 'P' sign = 'I' option = 'EQ' low = 'X' ) to it_rsparams.
  append value #( selname = 'RB3' kind = 'P' sign = 'I' option = 'EQ' low = '' ) to it_rsparams.
  append value #( selname = 'RB7' kind = 'P' sign = 'I' option = 'EQ' low = 'X' ) to it_rsparams.
  append value #( selname = 'RB8' kind = 'P' sign = 'I' option = 'EQ' low = '' ) to it_rsparams.
  append value #( selname = 'P_EXTR' kind = 'P' sign = 'I' option = 'EQ' low = 'X' ) to it_rsparams.
  append value #( selname = 'P_BUKRS' kind = 'S' sign = 'I' option = 'EQ' low = '0001' ) to it_rsparams.

  perform z_trata_data.

  append value #( selname = 'P_VENC' kind = 'S' sign = 'I' option = 'BT' low = dt_inicio high = dt_fim ) to it_rsparams.


  select b~vkbur
      from vbkd as a
      inner join vbak as b on a~vbeln	=	b~vbeln
      into table t_filial
      where a~valdt	>= dt_inicio
        and a~valdt <= dt_fim
*-CS2021000204 - 29.07.2021 - JT - inicio
        and b~vkbur in s_vkbur
*-CS2021000204 - 29.07.2021 - JT - fim
        and b~bukrs_vf = '0001'
        and b~spart in ('02','03','04').

  sort t_filial by vkbur.
  delete adjacent duplicates from t_filial comparing vkbur.
  delete t_filial where vkbur is initial. "// IR069358 remoção das linhas em branco
  delete it_rsparams where selname eq ' '.

**<<<------ir226476 /bug #169582 / aoenning------>>>
* Veridica se o processamento é background.
*  if not sy-batch is initial.
*  data(lv_jobname) = 'JOB_INS_ORDEM_VENC'.
*  select single count(*)
*    from tbtco
*    into @data(gv_job)
*  where jobname eq @lv_jobname
*    and status eq @sy-abcde+17(1). "R - Running
*
*  if vg_job ne 1.
** &2 job atualmente em execução.
*    message s001(finoc) with lv_jobname.
*    exit.
*  endif.
*  clear vg_job.

  if sy-batch eq abap_true.
    try.
        zcl_job=>get_ck_program_execucao( exporting i_nome_program = sy-cprog importing e_qtd = data(e_qtd) ).
      catch zcx_job.
    endtry.

    if e_qtd gt 1.
      leave program.
    endif.
  endif.
*endif.
**<<<------IR226476 /BUG #169582 / AOENNING------>>>

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = sy-tabix
      text       = 'Extraindo dados em ZSDT0060'.

  loop at t_filial into data(w_filial).

    delete it_rsparams where selname eq 'P_VKBUR'.

    append value #( selname = 'P_VKBUR' kind = 'S' sign = 'I' option = 'EQ' low = w_filial-vkbur ) to it_rsparams.

    perform f_prepare_run_time_info.

    submit zsdr0020 with selection-table it_rsparams
    and return.

    perform f_get_runtime_info.

    if <t_data> is assigned.
      loop at <t_data> assigning <w_data>.
        clear: wa_entrada.
        move-corresponding <w_data> to wa_entrada.
        append wa_entrada to it_entrada.
      endloop.
    endif.

  endloop.

endform.


*&---------------------------------------------------------------------*
*&      Form  TRATAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form tratar_dados .

  data: obj_taxa type ref to zcl_util_sd.
  data: s_data  type gdatu_inv,
        s_ukurs type ukurs_curr.

  s_data = dt_fim.

  select  * from tvkbt into table  @data(it_tvkbt)
    for all entries in @it_entrada
    where vkbur eq @it_entrada-vkbur
    and   spras eq @sy-langu.


  create object obj_taxa.
  obj_taxa->set_kurst( 'B' ).
  obj_taxa->set_waerk( 'USD' ).
  obj_taxa->set_tcurr( 'BRL' ).

  obj_taxa->set_data( s_data ).
  s_ukurs = obj_taxa->taxa_cambio( ).

  sort it_entrada by zterm.
  delete  it_entrada where zterm = 'I007'.
  delete  it_entrada where zterm = 'I006'.

*-CS2021000204 - 29.07.2021 - JT - inicio
  delete it_entrada where vbeln_s not in s_vbelns[].
*-CS2021000204 - 29.07.2021 - JT - fim

  it_entrada_aux = it_entrada.
  clear: it_entrada_agrupa, wa_entrada_agrupa.

  sort it_entrada by vbeln_p vbeln.
  sort it_entrada_aux by vbeln_p vbeln.
  delete adjacent duplicates from it_entrada_aux comparing vbeln_p.
  sort it_entrada_aux by vbeln_p.

*-CS2021000204 - 29.07.2021 - JT - inicio
  sort it_entrada by vbeln_p vbeln moeda_forte moeda_inter vlr_sald_fin vlr_sald_fin_brl docnum.
  delete adjacent duplicates from it_entrada
                        comparing vbeln_p vbeln moeda_forte moeda_inter vlr_sald_fin vlr_sald_fin_brl docnum.
*-CS2021000204 - 29.07.2021 - JT - fim

  loop at it_entrada_aux into wa_entrada_aux.

*    wa_entrada_agrupa-werks                =  wa_entrada_aux-werks.
*    wa_entrada_agrupa-vkbur                =  wa_entrada_aux-vkbur.
    wa_entrada_agrupa-kunnr                =  wa_entrada_aux-kunnr.
    wa_entrada_agrupa-name1                =  wa_entrada_aux-name1.
    wa_entrada_agrupa-auart                =  wa_entrada_aux-auart.
    wa_entrada_agrupa-zterm                =  wa_entrada_aux-zterm.
    wa_entrada_agrupa-text1                =  wa_entrada_aux-text1.
    wa_entrada_agrupa-vbeln_s              =  wa_entrada_aux-vbeln_s.
    wa_entrada_agrupa-vbeln_p              =  wa_entrada_aux-vbeln_p.
    wa_entrada_agrupa-vbeln                =  wa_entrada_aux-vbeln.
    wa_entrada_agrupa-vbeln_g              =  wa_entrada_aux-vbeln_g.
    wa_entrada_agrupa-erdat                =  wa_entrada_aux-erdat.
    wa_entrada_agrupa-waerk                =  wa_entrada_aux-waerk.
*    wa_entrada_agrupa-totalq_ov            =  wa_entrada_aux-totalq_ov.
*    wa_entrada_agrupa-totvl_ov             =  wa_entrada_aux-totvl_ov.
*    wa_entrada_agrupa-netwr_l              =  wa_entrada_aux-netwr_l.
*    wa_entrada_agrupa-mwsbp                =  wa_entrada_aux-mwsbp.
    wa_entrada_agrupa-data_venc            =  wa_entrada_aux-data_venc.
    wa_entrada_agrupa-forma_pag            =  wa_entrada_aux-forma_pag.
    wa_entrada_agrupa-taxa                 =  wa_entrada_aux-taxa.
*    wa_entrada_agrupa-mont_moeda           =  wa_entrada_aux-mont_moeda.
*    wa_entrada_agrupa-mont_mi              =  wa_entrada_aux-mont_mi.
*    wa_entrada_agrupa-docnum               =  wa_entrada_aux-docnum.
    wa_entrada_agrupa-augbl                =  wa_entrada_aux-augbl.
    wa_entrada_agrupa-budat                =  wa_entrada_aux-budat.
    wa_entrada_agrupa-dmbe2                =  wa_entrada_aux-dmbe2.
    wa_entrada_agrupa-dmbtr                =  wa_entrada_aux-dmbtr.
    wa_entrada_agrupa-salus                =  wa_entrada_aux-salus.
    wa_entrada_agrupa-salre                =  wa_entrada_aux-salre.
    wa_entrada_agrupa-rfmng                =  wa_entrada_aux-rfmng.
    wa_entrada_agrupa-observacao           =  wa_entrada_aux-observacao.
*    wa_entrada_agrupa-safra                =  wa_entrada_aux-safra.

    loop at it_entrada into wa_entrada
      where vbeln_p eq wa_entrada_aux-vbeln_p.

      if wa_entrada_agrupa-vkbur is initial.
        if wa_entrada_aux-vkbur is not initial.
          wa_entrada_agrupa-vkbur    =  wa_entrada-vkbur.
        endif.
      endif.

      wa_entrada_agrupa-moeda_forte          = wa_entrada_agrupa-moeda_forte + wa_entrada-moeda_forte.
      wa_entrada_agrupa-moeda_inter          = wa_entrada_agrupa-moeda_inter + wa_entrada-moeda_inter.
      wa_entrada_agrupa-vlr_sald_fin         = wa_entrada_agrupa-vlr_sald_fin + wa_entrada-vlr_sald_fin.
      wa_entrada_agrupa-vlr_sald_fin_brl     = wa_entrada_agrupa-vlr_sald_fin_brl + wa_entrada-vlr_sald_fin_brl.

      if wa_entrada_agrupa-safra is initial.
        wa_entrada_agrupa-safra    =  wa_entrada-safra.
      endif.
    endloop.

    append wa_entrada_agrupa to it_entrada_agrupa.
    clear: wa_entrada_agrupa, wa_entrada_aux.

  endloop.


  clear: it_entrada.
  it_entrada = it_entrada_agrupa.



  loop at it_entrada into wa_entrada
    where  waerk = 'BRL'
     and  ( moeda_inter >= 10 or vlr_sald_fin_brl >= 10 )
     and  data_venc between dt_inicio and dt_fim.


    read table it_tvkbt into data(wa_tvkbt) with key vkbur =  wa_entrada-vkbur.

    wa_vkbur-vkbur = wa_entrada-vkbur.
    wa_vkbur-text  = wa_tvkbt-bezei.
    translate wa_vkbur-text to lower case.
    append wa_vkbur to it_vkbur.

*-CS2021000204 - 29.07.2021 - JT - inicio
    if wa_entrada-moeda_inter < 0.
      wa_entrada-moeda_inter = 0.
    endif.
    if wa_entrada-vlr_sald_fin_brl < 0.
      wa_entrada-vlr_sald_fin_brl = 0.
    endif.
*-CS2021000204 - 29.07.2021 - JT - fim

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
    wa_saida_brl-sald_fin_brl = abs( wa_entrada-vlr_sald_fin_brl ).

*    IF wa_entrada-totalq_ov EQ 0 .
*      wa_saida_brl-pd_entregue = '0.00'.
*    ELSE.
*      wa_saida_brl-pd_entregue  = ( wa_entrada-rfmng / wa_entrada-totalq_ov  ) * 100.
*    ENDIF.

    concatenate wa_entrada-data_venc+6(2) '.'  wa_entrada-data_venc+4(2) '.'  wa_entrada-data_venc+0(4) into wa_saida_brl-data_venc.
    concatenate  wa_entrada-data_venc+0(4) wa_entrada-data_venc+4(2) wa_entrada-data_venc+6(2) into wa_saida_brl-data_venc_ord.

    append wa_saida_brl to it_saida_brl.
    clear: wa_saida_brl, wa_entrada.
  endloop.


  loop at it_entrada into wa_entrada
    where  waerk = 'USD'
    and ( moeda_forte > 10 or vlr_sald_fin >= 10 )
    and data_venc between dt_inicio and dt_fim.

    read table it_tvkbt into wa_tvkbt with key vkbur =  wa_entrada-vkbur.

    wa_vkbur-vkbur = wa_entrada-vkbur.
    wa_vkbur-text  = wa_tvkbt-bezei.
    translate wa_vkbur-text to lower case.
    append wa_vkbur to it_vkbur.

*-CS2021000204 - 29.07.2021 - JT - inicio
    if wa_entrada-moeda_forte < 0.
      wa_entrada-moeda_forte = 0.
    endif.
    if wa_entrada-vlr_sald_fin < 0.
      wa_entrada-vlr_sald_fin = 0.
    endif.
*-CS2021000204 - 29.07.2021 - JT - fim

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
    wa_saida_usd-sald_fin     = abs( wa_entrada-vlr_sald_fin ).

*    IF wa_entrada-totalq_ov  EQ 0 .
*      wa_saida_usd-pd_entregue = '0.00'.
*    ELSE.
*      wa_saida_usd-pd_entregue  = ( wa_entrada-rfmng / wa_entrada-totalq_ov  ) * 100.
*    ENDIF.

    concatenate wa_entrada-data_venc+6(2) '.'  wa_entrada-data_venc+4(2) '.'  wa_entrada-data_venc+0(4) into wa_saida_usd-data_venc.
    concatenate  wa_entrada-data_venc+0(4) wa_entrada-data_venc+4(2) wa_entrada-data_venc+6(2) into wa_saida_usd-data_venc_ord.

    append wa_saida_usd to it_saida_usd.
    clear: wa_saida_usd, wa_entrada.
  endloop.

*  SORT it_saida_brl BY salbrl DESCENDING.
*  SORT it_saida_usd BY salusd DESCENDING.
  sort it_saida_brl by data_venc_ord ascending.
  sort it_saida_usd by data_venc_ord ascending.
  sort it_vkbur     by vkbur.

  delete adjacent duplicates from it_vkbur.
  delete it_vkbur where vkbur eq ' '.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_trata_data .

  data: mes      like  t5a4a-dlymo,
        ano(4)   type  n,
        dt_atual type p0001-begda.

  dt_inicio = '20190101'.
  mes = sy-datum+4(2) .

  if mes = '01'.
    mes = '12'.
    ano = sy-datum+0(4) - 1.
  else.
    mes = mes - 1.
    ano = sy-datum+0(4).
  endif.

  concatenate ano mes '01' into dt_atual.

  call function 'FKK_LAST_DAY_OF_MONTH'
    exporting
      day_in            = dt_atual
    importing
      last_day_of_month = dt_fim
    exceptions
      day_in_no_date    = 1
      others            = 2.


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
*&      Form  MONTA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form monta_email .

  data: saldo_ov(15)         type n,
        saldo_financeiro(15) type n,
        pd_entrg(15)         type n,
        data_in(10)          type c,
        data_fim(10)         type c.


  select *
    from zsdt0060 into table it_zsdt0060
     for all entries in it_vkbur
   where vkbur     eq it_vkbur-vkbur
    and  programa  eq 'ZSDR016'
*-CS2021000204 - 29.07.2021 - JT - inicio
    and  usnam     in s_usnam
*-CS2021000204 - 29.07.2021 - JT - fim
    and  email     ne ' '.

  sort it_zsdt0060 by vkbur usnam.

  loop at it_vkbur into data(wa_vkbur).

    refresh: lt_mailrecipientes[],  lt_mailtxt[].

    loop at it_zsdt0060 into wa_zsdt0060
      where vkbur eq wa_vkbur-vkbur.

      lt_mailrecipientes-rec_type = 'U'.
      lt_mailrecipientes-receiver = wa_zsdt0060-email.
      append lt_mailrecipientes.

    endloop.

    lt_mailsubject-obj_langu = sy-langu.
    concatenate 'CR Ins. OVs Vencidas - Filial ' wa_vkbur-text into  lt_mailsubject-obj_descr separated by space.

    lt_mailtxt = '<!DOCTYPE html><html><head>'.
    append lt_mailtxt.

    lt_mailtxt = '<style> table {'.
    append lt_mailtxt.

    lt_mailtxt = 'font-family: arial, sans-serif; border-collapse: collapse; width: 100%; }'.
    append lt_mailtxt.

    lt_mailtxt = 'td, th { boder: 1px solid #dddddd; text-align: left; padding: 8px; }'.
    append lt_mailtxt.

    lt_mailtxt = 'tr:nth-child(even) { background-color: #dddddd; } '.
    append lt_mailtxt.

    lt_mailtxt = '</style></head>'.
    append lt_mailtxt.

    lt_mailtxt = '<body><br>'.
    append lt_mailtxt.

    lt_mailtxt = 'Bom dia,'.
    append lt_mailtxt.
    lt_mailtxt = '<br>'.
    append lt_mailtxt.

    concatenate dt_inicio+6(2) '/' dt_inicio+4(2) '/' dt_inicio+0(4) into data_in.
    concatenate dt_fim+6(2)    '/' dt_fim+4(2)    '/' dt_fim+0(4)    into data_fim.

    concatenate ' Senhores(as) segue a relação de contas a receber de insumos, posição ' data_in 'a' data_fim '.' into lt_mailtxt separated by space.
    append lt_mailtxt.
    lt_mailtxt = '<br>'.
    append lt_mailtxt.

    lt_mailtxt = 'Esse relatório pode ser gerado pela transação ZSDT0060, escolhendo o layout SALDO DE OV.'.
    append lt_mailtxt.
    lt_mailtxt = '<br>'.
    append lt_mailtxt.

    lt_mailtxt = 'Em caso de dúvida favor entrar em contato com contas.receber.'.
    append lt_mailtxt.

    lt_mailtxt = '<br><br><br>'.
    append lt_mailtxt.


    read table  it_saida_brl into data(wa_sd_blr) with key  vkbur = wa_vkbur-vkbur.
    if sy-subrc = 0.

      lt_mailtxt = '<table border="1"><colgroup span="3"></colgroup>'.
      append lt_mailtxt.
      lt_mailtxt = '<tr>'.
      append lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Escr.Venda</p></b></td>'
                   '<td><b><p style="font-size:90%">Nr Simulador</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Nro OV Principal</p></b></td>'
*                  '<td><b><p style="font-size:90%">Nro.O.V</p></b></td>'
                  '<td><b><p style="font-size:90%">Tipo O.V.</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

*      CONCATENATE '<td><b><p style="font-size:90%">Tipo O.V.</p></b></td>'
*                  '<td><b><p style="font-size:90%">Produto Entregue(%)</p></b></td>'  INTO lt_mailtxt.
*      APPEND lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Safra</p></b></td>'
                  '<td><b><p style="font-size:90%">Cod.Cliente</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate  '<td><b><p style="font-size:90%">Cliente</p></b></td>'
                   '<td><b><p style="font-size:90%">Cond.Pgto</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate  '<td><b><p style="font-size:90%">Moeda</p></b></td>'
                   '<td><b><p style="font-size:90%">Vlr. a Receber OV</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Saldo Juros em Aberto.</p></b></td>'
                  '<td><b><p style="font-size:90%">Dt.Vcto</p></b></td>'  into lt_mailtxt.
      append lt_mailtxt.

*      lt_mailtxt =  '<td><b><p style="font-size:90%">Dt.Vcto</p></b></td>'.
*      APPEND lt_mailtxt.

      lt_mailtxt = '</tr>'.
      append lt_mailtxt.

      loop at it_saida_brl into wa_saida_brl
        where vkbur eq wa_vkbur-vkbur.

        lt_mailtxt = '<tr>'.
        append lt_mailtxt.

        concatenate '<td><p style="font-size:80%">' wa_saida_brl-vkbur '</p></td>'  '<td><p style="font-size:80%">' wa_saida_brl-vbeln_s '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

*        CONCATENATE '<td><p style="font-size:80%">' wa_saida_brl-vbeln_p '</p></td>'  '<td><p style="font-size:80%">' wa_saida_brl-vbeln_g '</p></td>' INTO lt_mailtxt.
        concatenate '<td><p style="font-size:80%">' wa_saida_brl-vbeln_p '</p></td>'  '<td><p style="font-size:80%">' wa_saida_brl-auart '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

*        WRITE wa_saida_brl-pd_entregue TO pd_entrg.
*        CONCATENATE '<td><p style="font-size:80%">' wa_saida_brl-auart '</p></td>'  '<td><p style="font-size:80%">' pd_entrg '</p></td>' INTO lt_mailtxt.
*        APPEND lt_mailtxt.

        concatenate '<td><p style="font-size:80%">' wa_saida_brl-safra '</p></td>'  '<td><p style="font-size:80%">' wa_saida_brl-kunnr  '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        concatenate '<td><p style="font-size:80%">' wa_saida_brl-name1 '</p></td>'  '<td><p style="font-size:80%">' wa_saida_brl-text1 '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        write wa_saida_brl-salbrl to saldo_ov.
        concatenate '<td><p style="font-size:80%">' wa_saida_brl-waerk '</p></td>'  '<td><p style="font-size:80%">' saldo_ov  '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        write wa_saida_brl-sald_fin_brl to saldo_financeiro.
        concatenate '<td><p style="font-size:80%">' saldo_financeiro '</p></td>'  '<td><p style="font-size:80%">' wa_saida_brl-data_venc '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        lt_mailtxt = '</tr>'.
        append lt_mailtxt.

        clear:  wa_saida_brl, saldo_ov, pd_entrg.
      endloop.

      lt_mailtxt = '</table>'.
      append lt_mailtxt.

      lt_mailtxt = '<br><br><br>'.
      append lt_mailtxt.
    endif.



    read table it_saida_usd into data(wa_sd_usd) with key vkbur = wa_vkbur-vkbur.
    if sy-subrc = 0.

      lt_mailtxt = '<table border="1"><colgroup span="3"></colgroup>'.
      append lt_mailtxt.
      lt_mailtxt = '<tr>'.
      append lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Escr.Venda</p></b></td>'
                   '<td><b><p style="font-size:90%">Nr Simulador</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Nro OV Principal</p></b></td>'
*                  '<td><b><p style="font-size:90%">Nro.O.V</p></b></td>'
                  '<td><b><p style="font-size:90%">Tipo O.V.</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

*      CONCATENATE '<td><b><p style="font-size:90%">Tipo O.V.</p></b></td>'
*                  '<td><b><p style="font-size:90%">Produto Entregue(%)</p></b></td>'  INTO lt_mailtxt.
*      APPEND lt_mailtxt.

      concatenate '<td><b><p style="font-size:90%">Safra</p></b></td>'
                  '<td><b><p style="font-size:90%">Cod.Cliente</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate  '<td><b><p style="font-size:90%">Cliente</p></b></td>'
                   '<td><b><p style="font-size:90%">Cond.Pgto</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate  '<td><b><p style="font-size:90%">Moeda</p></b></td>'
                   '<td><b><p style="font-size:90%">Vlr. a Receber OV</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

      concatenate  '<td><b><p style="font-size:90%">Saldo Juros em Aberto</p></b></td>'
                   '<td><b><p style="font-size:90%">Dt.Vcto</p></b></td>' into lt_mailtxt.
      append lt_mailtxt.

*      lt_mailtxt =  '<td><b><p style="font-size:90%">Dt.Vcto</p></b></td>'.
*      APPEND lt_mailtxt.

      lt_mailtxt = '</tr>'.
      append lt_mailtxt.


      loop at it_saida_usd into wa_saida_usd
              where vkbur eq wa_vkbur-vkbur.

        lt_mailtxt = '<tr>'.
        append lt_mailtxt.

        concatenate '<td><p style="font-size:80%">' wa_saida_usd-vkbur '</p></td>'  '<td><p style="font-size:80%">' wa_saida_usd-vbeln_s '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

*        CONCATENATE '<td><p style="font-size:80%">' wa_saida_usd-vbeln_p '</p></td>'  '<td><p style="font-size:80%">' wa_saida_usd-vbeln_g '</p></td>' INTO lt_mailtxt.
        concatenate '<td><p style="font-size:80%">' wa_saida_usd-vbeln_p '</p></td>'  '<td><p style="font-size:80%">' wa_saida_usd-auart '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

*        WRITE wa_saida_usd-pd_entregue TO pd_entrg.
*        CONCATENATE '<td><p style="font-size:80%">' wa_saida_usd-auart '</p></td>'  '<td><p style="font-size:80%">' pd_entrg '</p></td>' INTO lt_mailtxt.
*        APPEND lt_mailtxt.

        concatenate '<td><p style="font-size:80%">' wa_saida_usd-safra '</p></td>'  '<td><p style="font-size:80%">' wa_saida_usd-kunnr  '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        concatenate '<td><p style="font-size:80%">' wa_saida_usd-name1 '</p></td>'  '<td><p style="font-size:80%">' wa_saida_usd-text1 '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        write wa_saida_usd-salusd to saldo_ov.
        concatenate '<td><p style="font-size:80%">' wa_saida_usd-waerk '</p></td>'  '<td><p style="font-size:80%">' saldo_ov  '</p></td>' into lt_mailtxt.
        append lt_mailtxt.


        write wa_saida_usd-sald_fin to saldo_financeiro.
        concatenate '<td><p style="font-size:80%">' saldo_financeiro '</p></td>' '<td><p style="font-size:80%">' wa_saida_usd-data_venc '</p></td>' into lt_mailtxt.
        append lt_mailtxt.

        lt_mailtxt = '</tr>'.
        append lt_mailtxt.

        clear:  wa_saida_usd, saldo_ov, pd_entrg.
      endloop.

      lt_mailtxt = '</table>'.
      append lt_mailtxt.
    endif.

    lt_mailtxt = '</body></html>'.
    append lt_mailtxt.

    perform enviar.

    clear: lt_mailrecipientes, lt_mailtxt, lt_mailsubject, wa_zsdt0060, data_in,  data_fim.
  endloop.

endform.

form enviar.

  data: vuser         type sy-uname.

  vuser = sy-uname.
  sy-uname = 'JOBADM'.

  "Colocado essa condição para quando for QAS nao dispare email para todos os usuarios das filiais - SMC
  if sy-sysid = 'QAS'.
    clear lt_mailrecipientes.
    free lt_mailrecipientes[].
    append value #( receiver =   'samuel.cabana@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) to lt_mailrecipientes.
  else.
  endif.
  "Colocado essa condição para quando for QAS nao dispare email para todos os usuarios das filiais - SMC

  call function 'SO_NEW_DOCUMENT_SEND_API1'
    exporting
      document_data              = lt_mailsubject
      document_type              = 'HTM'
    tables
      object_content             = lt_mailtxt
      receivers                  = lt_mailrecipientes
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      others                     = 8.

  sy-uname = vuser.

  if sy-subrc eq 0.
    commit work.
  endif.
endform.
