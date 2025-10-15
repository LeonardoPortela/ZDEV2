*&--------------------------------------------------------------------&*
*&                        WAYON - Consultoria                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Alexandre Suzan                                         &*
*& Data.....: 15/12/2010                                              &*
*& Descrição: Criação de Nota fiscal. Execução via JOB                &*
*& Transação: ZNFJ0003                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*of
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
report  zwrj0003.

tables: zfiwrt0021.

*&---------------------------------------------------------------------*
*&      types
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Tabelas internas
*&---------------------------------------------------------------------*
data: tg_0021 type table of zfiwrt0021 with header line.

*&---------------------------------------------------------------------*
*&      Variaveis
*&---------------------------------------------------------------------*

data:
  it_zfiwrt0001       type table of zfiwrt0001,
  wa_zfiwrt0001       type zfiwrt0001,
  it_zfiwrt0002       type table of zfiwrt0002,
  wa_zfiwrt0002       type zfiwrt0002,
  it_zfiwrt0003       type table of zfiwrt0003,
  wa_zfiwrt0003       type zfiwrt0003,
  it_zfiwrt0004       type table of zfiwrt0004,
  wa_zfiwrt0004       type zfiwrt0004,
  it_zfiwrt0005       type table of zfiwrt0005,
  wa_zfiwrt0005       type zfiwrt0005,
  it_zfiwrt0006       type table of zfiwrt0006,
  wa_zfiwrt0006       type zfiwrt0006,
  it_zfiwrt0007       type table of zfiwrt0007,
  wa_zfiwrt0007       type zfiwrt0007,
  it_zfiwrt0008       type table of zfiwrt0008,
  wa_zfiwrt0008       type zfiwrt0008,
  it_zfiwrt8          type table of zfiwrt0008,
  wa_zfiwrt8          type zfiwrt0008,
  it_zfiwrt0009       type table of zfiwrt0009,
  wa_zfiwrt0009       type zfiwrt0009,
  it_zfiwrt0010       type table of zfiwrt0010,
  wa_zfiwrt0010       type zfiwrt0010,
  it_zfiwrt0011       type table of zfiwrt0011,
  wa_zfiwrt0011       type zfiwrt0011,
  it_zfiwrt0012       type table of zfiwrt0012,
  wa_zfiwrt0012       type  zfiwrt0012,
  it_zfiwrt0013       type table of zfiwrt0013,
  wa_zfiwrt0013       type  zfiwrt0013,
  it_zfiwrt0015       type table of zfiwrt0015,
  wa_zfiwrt0015       type  zfiwrt0015,
  it_zfiwrt0021       type table of zfiwrt0021,
  wa_zfiwrt0021       type zfiwrt0021,
  it_zfiwrt0022       type table of zfiwrt0022,
  wa_zfiwrt0022       type zfiwrt0022,
  it_zfiwrt22         type table of zfiwrt0022,
  wa_zfiwrt22         type zfiwrt0022,
  it_zfiwrt1000       type table of  zfiwrt1000,
  wa_zfiwrt1000       type zfiwrt1000,

  tl_1000             type table of zfiwrt1000,
  wl_1000             type zfiwrt1000,

  tl_0008             type table of zfiwrt0008 with header line,
  tl_zib_chv          type table of zib_contabil_chv with header line,
  tl_zib_err          type table of zib_contabil_err with header line,
  tl_zib_cont         type table of zib_contabil with header line,
  tl_0011             type table of zfiwrt0011 with header line,

  it_j_1baj           type table of j_1baj,
  wa_j_1baj           type j_1baj,
  it_j_1bajt          type table of j_1bajt,
  wa_j_1bajt          type j_1bajt,
  it_tbsl             type table of tbsl,
  wa_tbsl             type tbsl,
  it_skat             type table of skat,
  wa_skat             type skat,
*      IT_CSKB             TYPE TABLE OF CSKB,
  wa_cskb             type cskb,
*      IT_USER_ADDR        TYPE TABLE OF USER_ADDR,
*      WA_USER_ADDR        TYPE  USER_ADDR,
  it_mara             type table of mara,
  wa_mara             type mara,
  it_marc             type table of marc,
  wa_marc             type marc,
  it_t001             type table of t001,
  wa_t001             type t001,
  it_t001w            type table of t001w,
  wa_t001w            type  t001w,
  it_kna1             type table of kna1,
  wa_kna1             type kna1,
  wa_lfa1             type lfa1,
  it_j_1baa           type table of j_1baa,
  wa_j_1baa           type j_1baa,
  tl_texto            type catsxt_longtext_itab,
  wl_texto            type line of catsxt_longtext_itab,
  wl_texto_fiscal(30),
  wl_indcoper         type zfiwrt0006-indcoper,
  wg_shipfrom         type lfa1-regio,
  wg_shipto           type lfa1-regio.

data: tl_docest    type table of zfiwrs0003 with header line.
data: wl_color type kkblo_specialcol,
      begin of tl_infogrid,
        color type kkblo_specialcol occurs 1,
      end of tl_infogrid.

data: wl_cont    type sy-tabix,
      wg_flag,
      p_seq_lcto type zfiwrt0008-seq_lcto.

data: tl_bdc type table of bdcdata,
      wl_bdc type bdcdata.

data: begin of tg_itens occurs 0,
        mark(1),
        itmnum  type zfiwrt0009-itmnum,
        matnr   type zfiwrt0009-matnr,
        maktx   type makt-maktx,
        cfop    type zfiwrt0006-cfop,
        charg   type zfiwrt0009-charg,
        werks   type t001w-werks,
        lgort   type zfiwrt0009-lgort,
        menge   type zfiwrt0009-menge,
        meins   type zfiwrt0009-meins,
        netpr   type zfiwrt0009-netpr,
        netwr   type zfiwrt0009-netwr,
        anln1   type zfiwrt0009-anln1,
        anln2   type zfiwrt0009-anln2,
        steuc   type marc-steuc,
      end of tg_itens.

data: begin of tg_mensagens occurs 0,
        seqnum  type  zfiwrt0005-seqnum,
        linnum  type  zfiwrt0005-linnum,
        message type  zfiwrt0005-message,
      end of  tg_mensagens.

data: begin of tg_impo occurs 0,
        taxtyp   type zfiwrt0010-taxtyp,
        ttypetxt type j_1bttytxt,
        taxgrp   type j_1btaxgrp,
        base     type zfiwrt0010-base,
        rate     type zfiwrt0010-rate,
        taxval   type zfiwrt0010-taxval,
        excbas   type zfiwrt0010-excbas,
        othbas   type zfiwrt0010-othbas,
        line     TYPE i,  " Alteracao feita por Alexandre ref; CS1016278 - IR107256 19.04.2023
      end of  tg_impo.

data:begin of tg_impo_comp occurs 0,
       itmnum type zfiwrt0010-itmnum.
       include structure tg_impo.
data: end of tg_impo_comp.

data: begin of tg_contab occurs 0,
        bschl   type zfiwrt0011-bschl,
        hkont   type zfiwrt0011-hkont,
        txt50   type skat-txt50,
        dmbtr   type zfiwrt0011-dmbtr,
        taxtyp  type zfiwrt0011-taxtyp,
        estorno type zfiwrt0011-estorno,
        newbw   type zfiwrt0011-newbw,
        zlsch   type zfiwrt0011-zlsch,
        zfbdt   type zfiwrt0011-zfbdt,
        kostl   type zfiwrt0011-kostl,
        umskz   type zfiwrt0011-umskz,
        vbund   type zfiwrt0011-vbund,
        style2  type lvc_t_styl,
      end of tg_contab.

data: begin of tg_movest occurs 0,
        bwart   type zfiwrt0004-bwart,
        tcode   type zfiwrt0004-tcode,
        mwskz1  type zfiwrt0004-mwskz1,
        estorno type zfiwrt0004-estorno,
      end of tg_movest.

types: begin of ty_parc,
         parvw    type zfiwrt0015-parvw,
         parid    type zfiwrt0015-parid,
         nome(80),
         style    type lvc_t_styl,
       end of ty_parc.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

data: tl_impo_aux like table of tg_impo with header line,
      tg_parc     type table of ty_parc  with header line.

data: msg_id   like t100-arbgb,
      msg_no   like t100-msgnr,
      msg_var1 like balm-msgv1,
      msg_var2 like balm-msgv2,
      msg_var3 like balm-msgv3,
      msg_var4 like balm-msgv4,
      wmessage type bapi_msg.

selection-screen: begin of block b1 with frame.
  select-options:  s_bukrs  for zfiwrt0021-bukrs.
selection-screen: end of block b1.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
start-of-selection.
  data: vg_job      type i.

  select single count( * ) into vg_job
    from tbtco
   where jobname eq 'ZWRJ0003_CTRL_AUT'
     and status eq 'R'.

  if ( vg_job eq 1 ).
    if sy-uzeit gt '080000' and sy-uzeit le '170000'.
      perform limpa_estruturas.
      perform seleciona_dados.
      perform executa_lancamento.
    endif.
  endif.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados .

  data: lv_dat_ini type sy-datum.
  data: lv_dat_fim type sy-datum.

  data: lv_dat type sy-datum.
  data: lv_ano type n length 4.
  data: lv_mes type n length 2.
  data: lv_dia type n length 2.
  data: lv_dia_u type n length 2.

  data: lt_eth_dats type table of rke_dat.

  lv_dat = sy-datum.

  lv_ano = lv_dat(4).
  lv_mes = lv_dat+4(2).
  lv_dia = lv_dat+6(2).

  select *
    from zfiwrt0021
    into table tg_0021
     where bukrs in s_bukrs
      and  ano   eq lv_ano
      and  loekz  ne 'X'
      and  ger_autom eq 'X'
      and  fatura_atu eq  'X'
      and  not exists
      ( select * from  zfiwrt0022 where contrato eq zfiwrt0021~contrato
                            and   bukrs    eq zfiwrt0021~bukrs
                            and   branch   eq zfiwrt0021~branch
                            and   kunnr    eq zfiwrt0021~kunnr
                            and   ano      eq lv_ano
                            and   mes      eq lv_mes ).

  call function 'CCM_GO_BACK_MONTHS'
    exporting
      currdate   = lv_dat
      backmonths = 1
    importing
      newdate    = lv_dat.

  lv_ano = lv_dat(4).
  lv_mes = lv_dat+4(2).

  select *
        from zfiwrt0021
        appending table tg_0021
       where bukrs in s_bukrs
        and  ano   eq lv_ano
        and  loekz  ne 'X'
        and  ger_autom eq 'X'
        and  fatura_atu eq  ' '
        and  not exists
        ( select * from  zfiwrt0022 where contrato eq zfiwrt0021~contrato
                              and   bukrs    eq zfiwrt0021~bukrs
                              and   branch   eq zfiwrt0021~branch
                              and   kunnr    eq zfiwrt0021~kunnr
                              and   ano      eq lv_ano
                              and   mes      eq lv_mes ).

  sort tg_0021 by fatura_f.

  delete tg_0021 where fatura_f is not initial and fatura_f ne lv_dia.

  lv_dat_ini = sy-datum(6) && '01'.
  lv_dat_fim = sy-datum.

  call function 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
    exporting
      i_datab               = lv_dat_ini
      i_datbi               = lv_dat_fim
      i_factid              = 'ZT'
    tables
      eth_dats              = lt_eth_dats
    exceptions
      date_conversion_error = 1
      others                = 2.

  clear: lv_dia_u.
  if lt_eth_dats[] is not initial.
    describe table lt_eth_dats lines lv_dia_u.
  endif.
  sort tg_0021 by fatura_u.
  delete tg_0021 where fatura_u is not initial and fatura_u ne lv_dia_u.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_lancamento
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form executa_lancamento .
  data: vg_job      type i.

  write: 'Log Registros Processados'.
  write: /.
  write: 'DOCNUM;NFENUM;MENSAGEM'.
  if tg_0021[] is not initial.
    perform z_gerar_nf .
  else.
    write: 'NENHUM REGISTRO ENCONTRADO'.
  endif.

  perform z_envia_sefaz .
endform.                    " EXECUTA_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  LIMPA_ESTRUTURAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_estruturas .
  clear: tg_0021.
  refresh: tg_0021.
endform.                    " LIMPA_ESTRUTURAS

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_gerar_nf .

  data: lv_ano type n length 4.
  data: lv_mes type n length 2.
  data: lv_dat type sy-datum.

  data: v_zfbdt      type zfiwrt0011-zfbdt,
        v_data_aux   type sy-datum,
        wl_cont      type sy-tabix,
        wl_linha     type sy-tabix,
        wl_cont_aux  type sy-tabix,
        wl_cont_aux2 type sy-tabix,
        v_matnr18    type matnr18.


  data: l_data   type  p0001-begda,
        l_days   type  t5a4a-dlydy,
        l_months type  t5a4a-dlymo,
        l_signum type  t5a4a-split,
        l_years  type  t5a4a-dlyyr.

  data: lv_mes_ant type n length 2.
  data: lv_campo   type c length 50.

  field-symbols: <row>       type any.
  field-symbols: <rowt>      type any.


  select *  from  zfiwrt0001  into table it_zfiwrt0001
    for all entries in tg_0021
  where operacao eq tg_0021-operacao.

  if sy-subrc <> 0.
    message 'Não existe Operação Nota Writer Cadastado' type 'I'.
    exit.
  endif.

  select  * from t001w into table it_t001w
    for all entries in tg_0021
    where werks eq tg_0021-branch.

  select  * from j_1baa into table  it_j_1baa
   for all entries in it_zfiwrt0001
    where nftype eq it_zfiwrt0001-nftype.

  select  *  from kna1 into table it_kna1
    for all entries in tg_0021
    where kunnr eq tg_0021-kunnr.

  select *  from zfiwrt0002  into table it_zfiwrt0002
    for all entries in tg_0021
    where operacao eq tg_0021-operacao.

  if sy-subrc is initial.

    select * from j_1baj  into table it_j_1baj
      for all entries in it_zfiwrt0002
      where taxtyp eq it_zfiwrt0002-taxtyp.

    select *  from j_1bajt into table it_j_1bajt
      for all entries in it_zfiwrt0002
      where spras  eq sy-langu
      and   taxtyp eq it_zfiwrt0002-taxtyp.

  endif.

  select *  from zfiwrt0003  into table it_zfiwrt0003
    for all entries in it_zfiwrt0021
     where operacao eq it_zfiwrt0021-operacao.

  if sy-subrc is initial.

    select  *  from tbsl  into table it_tbsl
      for all entries in it_zfiwrt0003
       where bschl eq it_zfiwrt0003-bschl.

    select  *  from skat into table it_skat
      for all entries in it_zfiwrt0003
        where spras eq sy-langu
          and ktopl eq '0050'
          and saknr eq it_zfiwrt0003-hkont.

  endif.

  select  *  from zfiwrt0004 into table  it_zfiwrt0004
    for all entries in tg_0021
     where operacao eq tg_0021-operacao.

  select  * from zfiwrt0005  into table it_zfiwrt0005
    for all entries in tg_0021
   where operacao eq tg_0021-operacao.


  select * from zfiwrt0006  into table it_zfiwrt0006
    for all entries in tg_0021
   where operacao eq tg_0021-operacao.

  select  * from zfiwrt0007   into table it_zfiwrt0007
    for all entries in tg_0021
      where operacao eq tg_0021-operacao
        and branch   eq tg_0021-branch
        and tipo     eq 'W'.

  sort: it_zfiwrt0004 by operacao,
        it_zfiwrt0005 by operacao seqnum linnum,
        it_zfiwrt0006 by operacao indcoper,
        it_zfiwrt0007 by operacao branch.

  loop at tg_0021 into tg_0021.

    refresh: it_zfiwrt0008,
             it_zfiwrt0009,
             it_zfiwrt0010,
             it_zfiwrt0011,
             it_zfiwrt0012,
             it_zfiwrt0013,
             it_zfiwrt0015,
             it_zfiwrt22.

    clear: wa_zfiwrt0001, wa_zfiwrt0002,
           wa_zfiwrt0003, wa_zfiwrt0004, wa_zfiwrt0005, wa_zfiwrt0006,
           wa_zfiwrt0007, wa_zfiwrt0008, wa_zfiwrt0009, wa_zfiwrt0010,
           wa_zfiwrt0011, wa_zfiwrt0012, wa_zfiwrt0013, wa_zfiwrt0015,
           wa_zfiwrt22,   wa_t001w,      wa_j_1baa,     wa_kna1,
           wa_j_1baj,     wa_j_1bajt,    wa_tbsl,       wa_skat,
           wa_cskb,       wa_marc,       wa_mara.
*
    lv_mes_ant = sy-datum+4(2).


*    IF tg_0021-fatura_atu IS INITIAL AND lv_mes_ant = 01.
*      CONTINUE.
*    ENDIF.

    read table it_zfiwrt0001 into wa_zfiwrt0001 with key  operacao = tg_0021-operacao.

    read table it_j_1baa into wa_j_1baa with key  nftype =  wa_zfiwrt0001-nftype.

    read table it_kna1 into wa_kna1 with key kunnr = tg_0021-kunnr.

    read table it_t001w into wa_t001w  with key werks = tg_0021-branch.
*
    wl_indcoper = 'D'.
    perform f_define_origem_destino in program zwrr0009 using    wa_kna1
                                             wa_lfa1
                                             wa_t001w
                                             wa_j_1baa
                                             wa_zfiwrt0001  "BUG 63832
                                    changing wl_indcoper
                                             wl_texto_fiscal.


    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = tg_0021-matnr
      importing
        output = v_matnr18.

    tg_0021-matnr = v_matnr18.
    select single * from mara
      into wa_mara
      where matnr eq tg_0021-matnr.

    read table it_zfiwrt0006 into wa_zfiwrt0006 with key  operacao = tg_0021-operacao
                                                          indcoper = wl_indcoper binary search.

    if sy-subrc ne 0.
*      MESSAGE |Erro na operacao { tg_0021-operacao } | TYPE 'I'.
      continue.
    endif.

    move:  sy-mandt                         to wa_zfiwrt0008-mandt,
           tg_0021-operacao                 to wa_zfiwrt0008-operacao,
           tg_0021-bukrs                    to wa_zfiwrt0008-bukrs,
           tg_0021-branch                   to wa_zfiwrt0008-branch,
           wa_zfiwrt0001-parvw              to wa_zfiwrt0008-parvw,
           tg_0021-kunnr                    to wa_zfiwrt0008-parid,
           wa_zfiwrt0001-nftype             to wa_zfiwrt0008-nftype,
           tg_0021-branch                   to wa_zfiwrt0008-move_plant,
           wa_zfiwrt0001-energia            to wa_zfiwrt0008-energia,
           wa_zfiwrt0001-servico            to wa_zfiwrt0008-servico,
           wa_zfiwrt0001-complemento        to wa_zfiwrt0008-complemento,
           'CIF'                            to wa_zfiwrt0008-inco1,
           'CIF'                            to wa_zfiwrt0008-inco2,
           wa_zfiwrt0001-referencia         to wa_zfiwrt0008-referencia,
           wa_zfiwrt0006-cfop               to wa_zfiwrt0008-cfop,
           wa_zfiwrt0006-taxlw1             to wa_zfiwrt0008-taxlw1,
           wa_zfiwrt0006-taxlw2             to wa_zfiwrt0008-taxlw2,
           wa_zfiwrt0006-taxlw4             to wa_zfiwrt0008-taxlw4,
           wa_zfiwrt0006-taxlw5             to wa_zfiwrt0008-taxlw5,
           wa_zfiwrt0006-opertyp            to wa_zfiwrt0008-opertyp,
           wa_zfiwrt0006-taxcode            to wa_zfiwrt0008-taxcode,
*-CS2021000723 - 18.10.2021 - JT - inicio
*          'A'                              TO wa_zfiwrt0008-status,
           abap_off                         to wa_zfiwrt0008-status,
*-CS2021000723 - 18.10.2021 - JT - fim
           sy-uname                         to wa_zfiwrt0008-usuario_ult_mod,
           sy-datum                         to wa_zfiwrt0008-dt_ult_mod,
           sy-uzeit                         to wa_zfiwrt0008-hr_ult_mod,
           sy-datum                         to wa_zfiwrt0008-budat,
           sy-datum                         to wa_zfiwrt0008-bldat.
    append wa_zfiwrt0008 to it_zfiwrt0008.

    move: sy-mandt                   to wa_zfiwrt22-mandt,
          tg_0021-contrato           to wa_zfiwrt22-contrato,
          tg_0021-bukrs              to wa_zfiwrt22-bukrs,
          tg_0021-branch             to wa_zfiwrt22-branch,
          tg_0021-kunnr              to wa_zfiwrt22-kunnr,
          tg_0021-ano                to wa_zfiwrt22-ano,
          sy-datum+4(2)              to wa_zfiwrt22-mes,
          tg_0021-total_montante     to wa_zfiwrt22-montante,
          tg_0021-tarifa01           to wa_zfiwrt22-tarifa,
          wa_zfiwrt0008-seq_lcto     to wa_zfiwrt22-seq_lcto,
          sy-datum                   to wa_zfiwrt22-data_fatura,
          sy-uname                   to wa_zfiwrt22-usuario,
          sy-datum                   to wa_zfiwrt22-dt_modf,
          sy-uzeit                   to wa_zfiwrt22-hr_modf.

    lv_mes_ant = sy-datum+4(2).

    lv_dat = sy-datum.
    lv_ano = lv_dat(4).
    lv_mes = lv_dat+4(2).

    "Preço mes atual
    wa_zfiwrt22-ano = lv_ano.
    wa_zfiwrt22-mes = lv_mes.

    if tg_0021-fatura_atu is initial.
      call function 'CCM_GO_BACK_MONTHS'
        exporting
          currdate   = sy-datum
          backmonths = 1
        importing
          newdate    = lv_dat.

      lv_ano = lv_dat(4).
      lv_mes = lv_dat+4(2).
      "
      wa_zfiwrt22-ano = lv_ano.
      wa_zfiwrt22-mes = lv_mes.

      "Preço mes anterior
      lv_mes_ant = lv_mes.
    endif.

    "montante
    lv_campo = 'TG_0021-MONTANTE' && lv_mes_ant.
    condense lv_campo no-gaps.
    assign (lv_campo) to <row>.
    "
    " tarifa
    lv_campo = 'TG_0021-TARIFA' && lv_mes_ant.
    condense lv_campo no-gaps.
    assign (lv_campo) to <rowt>.

    if <rowt> = 0 or <row> = 0. "IR074857 19/10/21
      continue.
    endif.

    wa_zfiwrt22-montante = <row>.
    wa_zfiwrt22-tarifa  = <rowt>.
    append wa_zfiwrt22 to it_zfiwrt22.

    select single *  from marc  into wa_marc
      where matnr eq  tg_0021-matnr.


    refresh  tg_itens.
    tg_itens-itmnum  = 10.
    tg_itens-matnr   = tg_0021-matnr.
    select single maktx from makt into tg_itens-maktx where matnr eq tg_0021-matnr.
    tg_itens-cfop    = wa_zfiwrt0006-cfop.
    tg_itens-werks   = tg_0021-branch.
    tg_itens-menge = <row>.
    tg_itens-meins   = wa_mara-meins.
    tg_itens-netpr   = <rowt>.
    tg_itens-netwr   = ( tg_itens-netpr  * tg_itens-menge ).
    tg_itens-steuc   = wa_marc-steuc.
    append tg_itens.
*
    loop at tg_itens.
      move: sy-mandt          to wa_zfiwrt0009-mandt,
       wa_zfiwrt0008-seq_lcto to wa_zfiwrt0009-seq_lcto,
       tg_itens-itmnum        to wa_zfiwrt0009-itmnum,
       tg_itens-matnr         to wa_zfiwrt0009-matnr,
       tg_itens-cfop          to wa_zfiwrt0009-cfop,
       tg_itens-charg         to wa_zfiwrt0009-charg,
       tg_itens-menge         to wa_zfiwrt0009-menge,
       tg_itens-meins         to wa_zfiwrt0009-meins,
       tg_itens-netpr         to wa_zfiwrt0009-netpr,
       tg_itens-netwr         to wa_zfiwrt0009-netwr,
       wa_zfiwrt0001-itmtyp   to wa_zfiwrt0009-itmtyp,
       tg_itens-werks         to wa_zfiwrt0009-bwkey,
       tg_itens-lgort         to wa_zfiwrt0009-lgort,
       tg_itens-anln1         to wa_zfiwrt0009-anln1,
       tg_itens-anln2         to wa_zfiwrt0009-anln2.
      append wa_zfiwrt0009 to it_zfiwrt0009.

      "texto
      wl_linha = 0.
*      if tg_0021-texto_nota is not initial.
*        wl_cont = strlen( tg_0021-texto_nota ).
*        wl_cont_aux = wl_cont / 72.
*        do.
*          wl_linha  = sy-index.
*          tg_mensagens-linnum    =  '01'.
*          tg_mensagens-seqnum    =  wl_linha.
*          move: tg_0021-texto_nota+wl_cont_aux2 to tg_mensagens-message.
*          "
*          append tg_mensagens.
*          "
*          add 72 to wl_cont_aux2.
*          if wl_cont_aux2 gt wl_cont.
*            exit.
*          endif.
*        enddo.
*      endif.
      "ALRS
      l_data = sy-datum.

      if tg_0021-fatura_atu = 'X'.
        tg_0021-pgto_mes_seguinte = 'X'.
      endif.

      if tg_0021-pgto_mes_seguinte is not initial.

        l_months = 1.
        l_signum = '+'.

        call function 'RP_CALC_DATE_IN_INTERVAL'
          exporting
            date      = l_data
            days      = l_days
            months    = l_months
            signum    = l_signum
            years     = l_years
          importing
            calc_date = l_data.

      endif.


      if tg_0021-vencimento gt 0. "X dia útil
        concatenate l_data+0(06) '01' into v_zfbdt.
        v_data_aux = v_zfbdt.
        do tg_0021-vencimento times.
          "Jogar para o dia útil anterior
          zcl_miro=>get_proximo_dia_util(
            exporting
              i_data_base = v_data_aux
              i_signum    = '+'
            receiving
              r_data      = v_data_aux
            exceptions
              erro        = 1
              others      = 2 ).

          if v_data_aux is not initial.
            v_zfbdt = v_data_aux.
          endif.
          add 1 to v_data_aux.
        enddo.
      else.
        concatenate l_data+0(06) '01' into v_zfbdt.
        v_zfbdt = v_zfbdt + tg_0021-vencimento_f.
        v_data_aux = v_zfbdt - 1.

        "Jogar para o dia útil anterior
        zcl_miro=>get_proximo_dia_util(
          exporting
            i_data_base = v_data_aux
            i_signum    = '+'
          receiving
            r_data      = v_data_aux
          exceptions
            erro        = 1
            others      = 2 ).

        if v_data_aux is not initial.
          v_zfbdt = v_data_aux.
        endif.
      endif.

      wa_zfiwrt0008-zfbdt = v_zfbdt.
      wa_zfiwrt0008-zlsch = 'U'.


      add 1 to wl_linha.
      tg_mensagens-linnum    =  '01'.
      tg_mensagens-seqnum    =  wl_linha.

      concatenate 'VENCIMENTO:' ''                     into tg_mensagens-message separated by space.
      append tg_mensagens.

      add 1 to wl_linha.
      tg_mensagens-linnum    =  '01'.
      tg_mensagens-seqnum    =  wl_linha.
      concatenate v_zfbdt+6(2) '/' v_zfbdt+4(2) '/' v_zfbdt+0(4)              into tg_mensagens-message.
      concatenate  tg_mensagens-message tg_0021-banco '- AG:' tg_0021-agencia_banc 'C/C:' tg_0021-conta_banc into tg_mensagens-message separated by space.
      append tg_mensagens.

      refresh tg_impo.

      loop at it_zfiwrt0002 into wa_zfiwrt0002 where operacao = wa_zfiwrt0008-operacao.
        read table it_j_1baj into wa_j_1baj with key taxtyp = wa_zfiwrt0002-taxtyp.

        read table it_j_1bajt into wa_j_1bajt  with key taxtyp = wa_zfiwrt0002-taxtyp.

        move: wa_zfiwrt0002-taxtyp     to tg_impo-taxtyp,
              wa_j_1bajt-ttypetxt      to tg_impo-ttypetxt,
              wa_j_1baj-taxgrp         to tg_impo-taxgrp.

        append tg_impo.
        clear: tg_impo.
      endloop.
      "
      refresh: tg_contab, tl_impo_aux.

      loop at it_zfiwrt0003 into data(tl_0003) where operacao = wa_zfiwrt0008-operacao.
        read table it_skat into data(tl_skat)
          with key saknr = tl_0003-hkont.

        read table it_tbsl into data(tl_tbsl)
          with key bschl = tl_0003-bschl.

        if tl_tbsl-koart eq 'K'
        or tl_tbsl-koart eq 'D'.

          move tg_0021-kunnr to tg_contab-hkont.
          move: v_zfbdt to tg_contab-zfbdt,
                'U'     to tg_contab-zlsch.
        else.
          move: tl_0003-hkont   to tg_contab-hkont.
        endif.
        move: tl_0003-bschl   to tg_contab-bschl,
              tl_skat-txt50   to tg_contab-txt50,
              tl_0003-taxtyp  to tg_contab-taxtyp,
              tl_0003-estorno to tg_contab-estorno,
              tl_0003-newbw   to tg_contab-newbw,
              tl_0003-umskz   to tg_contab-umskz.

        append tg_contab.
        clear: tg_contab.
      endloop.

      perform monta_contabil.

      loop at tg_contab.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = tg_contab-hkont
          importing
            output = tg_contab-hkont.
        "
        move: sy-mandt                to wa_zfiwrt0011-mandt,
              wa_zfiwrt0008-seq_lcto  to wa_zfiwrt0011-seq_lcto,
              tg_contab-bschl         to wa_zfiwrt0011-bschl,
              tg_contab-hkont         to wa_zfiwrt0011-hkont,
              tg_contab-taxtyp        to wa_zfiwrt0011-taxtyp,
              tg_contab-dmbtr         to wa_zfiwrt0011-dmbtr,
              tg_contab-estorno       to wa_zfiwrt0011-estorno,
              tg_contab-zlsch         to wa_zfiwrt0011-zlsch,
              tg_contab-zfbdt         to wa_zfiwrt0011-zfbdt,
              tg_contab-kostl         to wa_zfiwrt0011-kostl,
              tg_contab-umskz         to wa_zfiwrt0011-umskz,
              tg_contab-vbund         to wa_zfiwrt0011-vbund.

        wa_zfiwrt0011-buzei  = sy-tabix.

        append wa_zfiwrt0011 to it_zfiwrt0011.
        clear: wa_zfiwrt0011.
      endloop.

      loop at tl_impo_aux.
        move: sy-mandt               to wa_zfiwrt0010-mandt,
              wa_zfiwrt0008-seq_lcto to wa_zfiwrt0010-seq_lcto,
              wa_zfiwrt0009-itmnum   to wa_zfiwrt0010-itmnum,
              tl_impo_aux-taxtyp     to wa_zfiwrt0010-taxtyp,
              tl_impo_aux-base       to wa_zfiwrt0010-base,
              tl_impo_aux-rate       to wa_zfiwrt0010-rate,
              tl_impo_aux-taxval     to wa_zfiwrt0010-taxval,
              tl_impo_aux-excbas     to wa_zfiwrt0010-excbas,
              tl_impo_aux-othbas     to wa_zfiwrt0010-othbas.
        append wa_zfiwrt0010 to it_zfiwrt0010.
      endloop.
*      CLEAR wa_zfiwrt0009.
    endloop.

    loop at tg_movest.
      move: sy-mandt               to  wa_zfiwrt0012-mandt,
           wa_zfiwrt0008-seq_lcto  to  wa_zfiwrt0012-seq_lcto,
           tg_movest-bwart         to  wa_zfiwrt0012-bwart,
           tg_movest-tcode         to  wa_zfiwrt0012-tcode,
           tg_movest-mwskz1        to  wa_zfiwrt0012-mwskz1,
           tg_movest-estorno       to  wa_zfiwrt0012-estorno.

      append wa_zfiwrt0012 to it_zfiwrt0012.
      clear: wa_zfiwrt0012.
    endloop.

    loop at tg_mensagens.
      move: sy-mandt                 to wa_zfiwrt0013-mandt,
            wa_zfiwrt0008-seq_lcto   to wa_zfiwrt0013-seq_lcto,
            tg_mensagens-seqnum      to wa_zfiwrt0013-seqnum,
            tg_mensagens-linnum      to wa_zfiwrt0013-linnum,
            tg_mensagens-message     to wa_zfiwrt0013-message.
      append wa_zfiwrt0013 to it_zfiwrt0013.
      move-corresponding wa_zfiwrt0013 to wa_zfiwrt0005.
      append wa_zfiwrt0005 to it_zfiwrt0005.
      clear: wa_zfiwrt0013.
    endloop.

    refresh tg_parc.
    tg_parc-parvw = wa_zfiwrt0008-parvw.
    tg_parc-parid = wa_zfiwrt0008-parid.
    append tg_parc.

    loop at tg_parc.
      move: sy-mandt                 to wa_zfiwrt0015-mandt,
            wa_zfiwrt0008-seq_lcto   to wa_zfiwrt0015-seq_lcto,
            tg_parc-parvw            to wa_zfiwrt0015-parvw,
            tg_parc-parid            to wa_zfiwrt0015-parid.

      append wa_zfiwrt0015 to it_zfiwrt0015.
*      CLEAR: wa_zfiwrt0015.
    endloop.

    " a classe vai calcular o imposto / contabil
    refresh: it_zfiwrt0011, it_zfiwrt0010.
    " a classe vai calcular o imposto / contabil

*--------------------------------------------------------------------------------------------------------*
* ------montar tabelas / gravar documento
*--------------------------------------------------------------------------------------------------------*
    try.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                                   )->set_cabecalho(       exporting i_cabecalho   = wa_zfiwrt0008
                                                   )->set_fatura_energia(  exporting i_fatura      = wa_zfiwrt22
                                                   )->add_item(            exporting i_item        = wa_zfiwrt0009
                                                   )->validar_registro(
                                                   )->prepara_lancamento(  exporting i_impostos    = it_zfiwrt0010[]
                                                                                     i_contabil    = it_zfiwrt0011[]
                                                   )->add_parceiro(        exporting i_parceiro    = wa_zfiwrt0015
                                                   )->set_monta_mensagens( exporting i_mensagens   = it_zfiwrt0005[]
                                                   )->gravar_documento(    exporting i_nao_valida  = abap_true
                                                                                     i_nao_prepara = abap_true
                                                                           importing e_seq_lcto    = data(_seq_lcto_gerado)
                                                   ).

        commit work and wait.

        if _seq_lcto_gerado is not initial.

          call function 'ZNFW_PROCESSA_SEQ_LCTO'
            exporting
              i_seq_lcto = _seq_lcto_gerado.

        else.
*      MESSAGE |Houve um erro ao gravar o lançamento!| TYPE 'S'.
        endif.

      catch zcx_nf_writer into data(zcx_nf_writer).
        zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    endtry.

*    MODIFY zfiwrt0008 FROM TABLE it_zfiwrt0008.
*    MODIFY zfiwrt0009 FROM TABLE it_zfiwrt0009.
*    MODIFY zfiwrt0010 FROM TABLE it_zfiwrt0010.
*    MODIFY zfiwrt0011 FROM TABLE it_zfiwrt0011.
*    MODIFY zfiwrt0012 FROM TABLE it_zfiwrt0012.
*    MODIFY zfiwrt0013 FROM TABLE it_zfiwrt0013.
*    MODIFY zfiwrt0015 FROM TABLE it_zfiwrt0015.
*    MODIFY zfiwrt0022 FROM TABLE it_zfiwrt22.
*    COMMIT WORK.
*
*    CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
*      EXPORTING
*        i_seq_lcto = wa_zfiwrt0008-seq_lcto.

    " Se houver aprovadores Marca status = ''

    read table it_zfiwrt0007 into wa_zfiwrt0007 with key operacao = wa_zfiwrt0008-operacao
                                                         branch   = wa_zfiwrt0008-branch binary search.
    if sy-subrc = 0.
      update zfiwrt0008 set status = ' '
      where seq_lcto = wa_zfiwrt0008-seq_lcto.
      commit work.
    endif.

*
    clear: wa_zfiwrt0001, wa_zfiwrt0002,
           wa_zfiwrt0003, wa_zfiwrt0004, wa_zfiwrt0005, wa_zfiwrt0006,
           wa_zfiwrt0007, wa_zfiwrt0008, wa_zfiwrt0009, wa_zfiwrt0010,
           wa_zfiwrt0011, wa_zfiwrt0012, wa_zfiwrt0013, wa_zfiwrt0015,
           wa_zfiwrt22,   wa_t001w,      wa_j_1baa,     wa_kna1,
           wa_j_1baj,     wa_j_1bajt,    wa_tbsl,       wa_skat,
           wa_cskb,       wa_marc,       wa_mara.

    data(reg_ok) = abap_true.

  endloop.

  refresh: it_zfiwrt0001,
         it_t001w,
         it_j_1baa,
         it_zfiwrt0002,
         it_j_1bajt,
         it_zfiwrt0003,
         it_tbsl,
         it_skat,
         it_zfiwrt0004,
         it_zfiwrt0005,
         it_zfiwrt0006,
         it_zfiwrt0007.

  refresh:  it_zfiwrt0008,
          it_zfiwrt0009,
          it_zfiwrt0010,
          it_zfiwrt0011,
          it_zfiwrt0012,
          it_zfiwrt0013,
          it_zfiwrt0015,
          it_zfiwrt22.

endform.


form monta_contabil.
  data:
    tl_impo  like table of tg_impo with header line,
    tg_tbsl  type table of tbsl with header line,
    t_hkont  type standard table of  rgsb4 with header line,
    wa_tka02 type tka02,
    v_koart  type tbsl-koart,
    v_vbund  type bseg-vbund,
    wl_tabix type sy-tabix.

  refresh: tl_impo, tl_impo_aux.
  clear: tl_impo, tl_impo_aux.

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'CONTAS_EC-CS'
    tables
      set_values    = t_hkont
    exceptions
      set_not_found = 1
      others        = 2.

  select single *
    from tka02
    into wa_tka02
    where bukrs = wa_zfiwrt0008-bukrs.

  concatenate 'CE4' wa_tka02-kokrs '_ACCT' into data(tabco1).
  concatenate 'CE4' wa_tka02-kokrs         into data(tabco2).

  tg_tbsl[] = it_tbsl[].
  loop at tg_contab.
    read table tg_tbsl
    with key bschl = tg_contab-bschl.
    if tg_tbsl-koart = 'D' or tg_tbsl-koart = 'K'.
      v_koart = tg_tbsl-koart.
    endif.
    move: 0 to tg_contab-dmbtr.
    modify tg_contab.
  endloop.

  loop at tg_itens.
    perform monta_impostos tables tl_impo_aux
                            using sy-tabix.

    loop at tl_impo_aux.
      move-corresponding tl_impo_aux to tl_impo.
      collect tl_impo.
    endloop.
  endloop.
  tl_impo_aux[] = tl_impo[].

  loop at tg_contab.
    wl_tabix = sy-tabix.
    read table tg_tbsl
    with key bschl = tg_contab-bschl.

    "Sociedade Parceira
    clear: tg_contab-vbund, v_vbund.
    read table t_hkont with key from = tg_contab-hkont.
    if sy-subrc = 0.
      if v_koart = 'D'.
        select single vbund into v_vbund from kna1
          where kunnr = wa_zfiwrt0008-parid "soc parceira do emissor
          and   ktokd in ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
      elseif v_koart = 'K'.
        select single vbund into v_vbund from lfa1
          where lifnr = wa_zfiwrt0008-parid "soc parceira do emissor
          and   ktokk in ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
      endif.
      tg_contab-vbund = v_vbund.
      modify tg_contab index wl_tabix transporting vbund.
    endif.

    if tg_contab-taxtyp is initial.
      if wa_zfiwrt0001-complemento = 'S'.
        loop at tl_impo where taxtyp eq 'ICM3'.
          if tg_tbsl-shkzg eq 'H'.
            subtract tl_impo-taxval from tg_contab-dmbtr.
          else.
            add tl_impo-taxval to tg_contab-dmbtr.
          endif.
        endloop.
      endif.
      modify tg_contab index wl_tabix.
      if wa_zfiwrt0001-energia eq 'N'.
        loop at tg_itens.
          if tg_tbsl-shkzg eq 'H'.
            subtract tg_itens-netwr from tg_contab-dmbtr.
          else.
            add tg_itens-netwr to tg_contab-dmbtr.
          endif.
        endloop.
      elseif wa_zfiwrt0001-energia eq 'S'.
        loop at tl_impo  where taxtyp eq 'ICS1'.
          if tg_tbsl-shkzg eq 'H'.
            subtract tl_impo-base from tg_contab-dmbtr.
          else.
            add tl_impo-base to tg_contab-dmbtr.
          endif.
        endloop.
      endif.
      modify tg_contab index wl_tabix.
    else.
      read table tl_impo with key taxtyp = tg_contab-taxtyp.
      if sy-subrc is initial.
        if tg_tbsl-shkzg eq 'H'.
          move: tl_impo-taxval to tg_contab-dmbtr.
          multiply tg_contab-dmbtr by -1.
        else.
          move tl_impo-taxval to tg_contab-dmbtr.
        endif.
        modify  tg_contab index wl_tabix.
      endif.
    endif.

    clear: wl_tabix, tl_impo, tg_tbsl.
  endloop.
  sort tg_contab by taxtyp bschl.

endform.

FORM monta_impostos TABLES tl_impo STRUCTURE tg_impo
                    USING e_row.

  DATA: BEGIN OF wl_1btxic,
          rate TYPE j_1btxic3-rate,
          base TYPE j_1btxic3-base,
        END OF wl_1btxic.

  DATA: wl_itens     LIKE LINE  OF tg_itens,
        wl_1baa      TYPE j_1baa,
        wl_base_aux  TYPE j_1btxic3-base,
        wl_a924      TYPE a924,
        wl_konp      TYPE konp,
        wl_t001w     TYPE t001w,
        wl_1btxsdc   TYPE j_1btxsdc,
        wl_1btxpis   TYPE j_1btxpis,
        wl_1btxcof   TYPE j_1btxcof,
        wl_impo_comp LIKE LINE OF tg_impo_comp.


**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

*data:   tg_impo_aux2  like table of tg_impo2 with header line,
*        wa_impo_aux2  like tg_impo2,
*        wa_impo_aux3  like tg_impo,
  DATA: v_line TYPE i,
        v_ics1 TYPE zfiwrt0010-taxval.
*
  v_ics1 = 0.
  v_line = 1.

** Fim Alteração feita por Alexandre

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

  READ TABLE tg_impo WITH KEY taxtyp = 'ICS1'.
*
  IF sy-subrc = 0.
    MOVE 99 TO tg_impo-line.
    MODIFY tg_impo INDEX sy-tabix TRANSPORTING line.
    SORT tg_impo BY line DESCENDING.
  ENDIF.



*  loop at tg_impo.
*
*    move tg_impo-taxtyp    to wa_impo_aux2-taxtyp.
*    move tg_impo-ttypetxt to wa_impo_aux2-ttypetxt.
*    move tg_impo-taxgrp   to wa_impo_aux2-taxgrp.
*    move tg_impo-base     to wa_impo_aux2-base.
*    move tg_impo-rate     to wa_impo_aux2-rate.
*    move tg_impo-taxval   to wa_impo_aux2-taxval.
*    move tg_impo-excbas   to wa_impo_aux2-excbas.
*    move tg_impo-othbas   to wa_impo_aux2-othbas.
*
*    if wa_impo_aux2-taxtyp = 'ICS1'.
*      wa_impo_aux2-line = 1.
*    else.
*      wa_impo_aux2-line = v_line + 1.
*    endif.
*    v_line = v_line + 1.
*    append wa_impo_aux2 to tg_impo_aux2.
*  endloop.
*
*  sort tg_impo_aux2 by line.
*  refresh tg_impo.
*  clear wa_impo_aux2.
*  loop at tg_impo_aux2.
*
*    move tg_impo_aux2-taxtyp   to wa_impo_aux3-taxtyp.
*    move tg_impo_aux2-ttypetxt to wa_impo_aux3-ttypetxt.
*    move tg_impo_aux2-taxgrp   to wa_impo_aux3-taxgrp.
*    move tg_impo_aux2-base     to wa_impo_aux3-base.
*    move tg_impo_aux2-rate     to wa_impo_aux3-rate.
*    move tg_impo_aux2-taxval   to wa_impo_aux3-taxval.
*    move tg_impo_aux2-excbas   to wa_impo_aux3-excbas.
*    move tg_impo_aux2-othbas   to wa_impo_aux3-othbas.
*
*   append wa_impo_aux3 to tg_impo.
*  endloop.
*  refresh tg_impo_aux2.
*  clear wa_impo_aux3.
*endif.
** Fim Alteração feita por Alexandre


  READ TABLE tg_itens INTO wl_itens INDEX 1.

  SELECT SINGLE * FROM j_1baa INTO wl_1baa
    WHERE nftype EQ wa_zfiwrt0001-nftype.


  IF ( wl_1baa-direct EQ '1'  ).
    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.

    SELECT SINGLE * FROM j_1btxsdc INTO wl_1btxsdc
      WHERE taxcode EQ wa_zfiwrt0006-taxcode.

    LOOP AT tg_impo.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF sy-subrc EQ 0 AND wa_zfiwrt0001-complemento EQ 'S'.
        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.
        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ 'ICM3'.
          IF wa_zfiwrt0006-opertyp EQ 'I'.
            IF wl_1baa-entrad EQ 'X'.
              SELECT SINGLE rate base FROM j_1btxic3 INTO wl_1btxic
                WHERE land1    EQ 'BR'
                AND   shipfrom EQ wg_shipfrom
                AND   shipto   EQ wg_shipto
                AND   gruop    EQ '30'
                AND   value    EQ wa_zfiwrt0008-parid
                AND   value2   EQ wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base FROM j_1btxic3 INTO wl_1btxic
                  WHERE land1     EQ 'BR'
                  AND   shipfrom  EQ wg_shipfrom
                  AND   shipto    EQ wg_shipto
                  AND   gruop     EQ '40'
                  AND   value     EQ wa_zfiwrt0008-parid.

                IF sy-subrc IS NOT INITIAL.
                  IF wa_zfiwrt0008-parvw NE 'BR' AND
                     wa_zfiwrt0008-parvw NE 'AG'.
                    SELECT SINGLE rate base FROM j_1btxic2 INTO wl_1btxic
                     WHERE land1     EQ 'BR'
                       AND shipfrom  EQ wg_shipfrom
                       AND shipto    EQ wg_shipto
                       AND matnr     EQ wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate FROM j_1btxic1 INTO wl_1btxic
                      WHERE  land1     EQ 'BR'
                         AND shipfrom  EQ wg_shipfrom
                         AND shipto    EQ wg_shipto.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              SELECT SINGLE  rate base FROM j_1btxic3 INTO wl_1btxic
                WHERE land1    EQ 'BR'
                AND   shipfrom EQ wg_shipfrom
                AND   shipto   EQ wg_shipto
                AND   gruop    EQ '76'
                AND   value    EQ wa_zfiwrt0008-parid
                AND   value2   EQ wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                IF wa_zfiwrt0008-parid NE 'BR' AND wa_zfiwrt0008-parid NE 'AG'.
                  SELECT SINGLE rate base FROM j_1btxic2 INTO wl_1btxic
                    WHERE land1     EQ 'BR'
                    AND   shipfrom  EQ wg_shipfrom
                    AND   shipto    EQ wg_shipto
                    AND   matnr     EQ tg_itens-matnr.
                ENDIF.
                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate FROM j_1btxic1  INTO wl_1btxic
                    WHERE land1 EQ 'BR'
                    AND   shipfrom  EQ wg_shipfrom
                    AND   shipto    EQ wg_shipto.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE * FROM t001w INTO wa_t001w
              WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.
              IF ( wl_1baa-direct NE '1'  ).
                SELECT SINGLE * FROM a924 INTO wl_a924
                  WHERE kschl    EQ 'ZIVP'
                  AND  aland     EQ 'BR'
                  AND  txreg_sf  EQ wa_t001w-regio
                  AND  matnr     EQ wl_itens-matnr
                  AND  datab     LE sy-datum
                  AND  datbi     GE sy-datum.
                IF sy-subrc IS INITIAL.
                  SELECT SINGLE * FROM konp INTO wl_konp
                    WHERE knumh EQ wl_a924-knumh.
                ENDIF.
              ENDIF.
            ENDIF.
            IF wl_1btxic-base IS INITIAL.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge *   wl_konp-kbetr.
              ENDIF.
              tl_impo-base = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.
            ELSE.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge *  wl_konp-kbetr.
              ENDIF.
              tl_impo-base = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
              tl_impo-othbas = wl_itens-netwr - tl_impo-base.
            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wa_zfiwrt0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate ,tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'I'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wa_zfiwrt0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'N'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wa_zfiwrt0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF   wl_1btxsdc-pis EQ 'X'
           AND tg_impo-taxtyp EQ 'IPIS'.


          SELECT SINGLE *  FROM j_1btxpis  INTO wl_1btxpis
          WHERE country EQ 'BR'
           AND  gruop   EQ '72'
           AND  value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ 'X' AND tg_impo-taxtyp EQ 'ICOF'.
          SELECT SINGLE * FROM j_1btxcof INTO wl_1btxcof
            WHERE country EQ 'BR'
            AND   gruop   EQ '71'
            AND   value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate = wl_1btxcof-rate.
            IF tl_impo-base > 0 AND wl_1btxcof-rate > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF tg_impo-taxtyp EQ 'ICS1'.
          SELECT SINGLE * FROM j_1baa INTO wl_1baa
            WHERE itmtyp EQ wa_zfiwrt0001-itmtyp.

          IF wl_1baa-entrad EQ 'X'.
            SELECT SINGLE rate base FROM  j_1btxic3 INTO wl_1btxic
              WHERE land1    EQ 'BR'
              AND   shipfrom EQ  wg_shipfrom
              AND   shipto   EQ  wg_shipto
              AND   gruop    EQ '30'
              AND   value    EQ wa_zfiwrt0008-parid
              AND   value2   EQ wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '40'
                   AND value    = wa_zfiwrt0008-parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate FROM j_1btxic1 INTO wl_1btxic
                  WHERE land1    EQ  'BR'
                  AND   shipfrom EQ wg_shipfrom
                  AND   shipto   EQ wg_shipto.
              ENDIF.
            ENDIF.
          ELSE.
            SELECT SINGLE rate base FROM j_1btxic3 INTO wl_1btxic
              WHERE land1     EQ 'BR'
              AND   shipfrom  EQ wg_shipfrom
              AND   shipto    EQ wg_shipto
              AND   gruop     EQ '76'
              AND   value     EQ wa_zfiwrt0008-parid
              AND   value2    EQ wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate FROM j_1btxic1  INTO wl_1btxic
               WHERE land1    EQ 'BR'
                AND shipfrom EQ wg_shipfrom
                AND shipto   EQ wg_shipto.
            ENDIF.
          ENDIF.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          tl_impo-rate = wl_1btxic-rate.
          IF wl_1btxic-base > 0 AND wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
          ENDIF.


          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            IF  wl_1btxic-base > 0.

**Inicio USER STORY #81382 - Anderson Oenning
              tl_impo-base   = tl_impo-base * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ELSE.
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ENDIF.
**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
            v_ics1 = tl_impo-taxval.
**Fim USER STORY #81382 - Anderson Oenning
          ENDIF.

          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
        ELSE.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                    tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF ( wl_1baa-direct EQ '2' ).

    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.
*  WL_1BTXIC3 TYPE J_1BTXIC3.
    SELECT SINGLE *
      FROM j_1btxsdc
      INTO wl_1btxsdc
       WHERE taxcode EQ wa_zfiwrt0006-taxcode.

    LOOP AT tg_impo.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF ( sy-subrc EQ 0 AND  wa_zfiwrt0008-complemento EQ 'S' ).
        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.
        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ 'ICM3'.
          IF wa_zfiwrt0006-opertyp EQ 'T'.
            SELECT SINGLE *
              FROM j_1baa
              INTO wl_1baa
               WHERE itmtyp EQ wa_zfiwrt0008-itmtyp.

            IF wl_1baa-entrad EQ 'X'.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '30'
                   AND value    = wa_zfiwrt0008-parid
                   AND value2	  =	wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = 'BR'
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto
                     AND gruop    = '40'
                     AND value    = wa_zfiwrt0008-parid.

                IF sy-subrc IS NOT INITIAL.
                  IF wa_zfiwrt0008-parvw NE 'BR'
                  AND wa_zfiwrt0008-parvw NE 'AG'.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = 'BR'
                         AND shipfrom	=	wg_shipfrom
                         AND shipto	  =	wg_shipto
                         AND matnr    = wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = 'BR'
                         AND shipfrom	=	wg_shipfrom
                         AND shipto	  =	wg_shipto.

                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '76'
                   AND value    = wa_zfiwrt0008-parid
                   AND value2	  =	wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                IF wa_zfiwrt0008-parvw NE 'BR'
                AND wa_zfiwrt0008-parvw NE 'AG'.
                  SELECT SINGLE rate base
                    FROM j_1btxic2
                    INTO wl_1btxic
                     WHERE land1    = 'BR'
                       AND shipfrom	=	wg_shipfrom
                       AND shipto	  =	wg_shipto
                       AND matnr    = wl_itens-matnr.
                ENDIF.
                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate
                    FROM j_1btxic1
                    INTO wl_1btxic
                     WHERE land1    = 'BR'
                       AND shipfrom = wg_shipfrom
                       AND shipto   = wg_shipto.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE *
              FROM t001w
              INTO wl_t001w
               WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.


              SELECT SINGLE *
                FROM a924
                INTO wl_a924
                 WHERE kschl    EQ 'ZIVP'
                   AND aland    EQ 'BR'
                   AND txreg_sf EQ wl_t001w-regio
                   AND matnr    EQ wl_itens-matnr
                   AND datab    LE sy-datum
                   AND datbi    GE sy-datum.

              IF sy-subrc IS INITIAL.


                SELECT SINGLE *
                  FROM konp
                  INTO wl_konp
                   WHERE knumh EQ wl_a924-knumh.

              ENDIF.

            ENDIF.
            IF wl_1btxic-base IS INITIAL.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              ENDIF.
              tl_impo-base   = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.

            ELSE.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              ENDIF.
              tl_impo-base   = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
              tl_impo-othbas = wl_itens-netwr - tl_impo-base.

            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wa_zfiwrt0008-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'I'.
**  aqui outros tipos de operacoes
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wa_zfiwrt0008-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'N'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wa_zfiwrt0008-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF wl_1btxsdc-pis EQ 'X'
           AND tg_impo-taxtyp EQ 'IPIS'.

          SELECT SINGLE *
            FROM j_1btxpis
            INTO wl_1btxpis
             WHERE country EQ 'BR'
               AND gruop   EQ '72'
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ 'X'
           AND tg_impo-taxtyp EQ 'ICOF'.
          SELECT SINGLE *
            FROM j_1btxcof
            INTO wl_1btxcof
             WHERE country EQ 'BR'
               AND gruop   EQ '71'
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate   = wl_1btxcof-rate.
            IF  tl_impo-base > 0 AND wl_1btxcof-rate  > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF  tg_impo-taxtyp EQ 'ICS1'.
          SELECT SINGLE *
           FROM j_1baa
           INTO wl_1baa
            WHERE itmtyp EQ wa_zfiwrt0008-itmtyp.

          IF wl_1baa-entrad EQ 'X'.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = 'BR'
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = '30'
                 AND value    = wa_zfiwrt0008-parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '40'
                   AND value    = wa_zfiwrt0008-parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = 'BR'
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto.

              ENDIF.

            ENDIF.

          ELSE.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = 'BR'
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = '76'
                 AND value    = wa_zfiwrt0008-parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate
                FROM j_1btxic1
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom = wg_shipfrom
                   AND shipto   = wg_shipto.
            ENDIF.

          ENDIF.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.

          tl_impo-rate =  wl_1btxic-rate .
          IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
          ENDIF.


          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            IF  wl_1btxic-base > 0.
**Inicio USER STORY #81382 - Anderson Oenning
              tl_impo-base = tl_impo-base * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ELSE.
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ENDIF.
**Fim USER STORY #81382 - Anderson Oenning
**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
            v_ics1 = tl_impo-taxval.
          ENDIF.


          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZAR_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_envia_sefaz .
  data: ls_t0008 type zfiwrt0008.
  data: ls_t0022 type zfiwrt0022.
  data: ls_t0021 type zfiwrt0021.
  data: vano type zfiwrt0021-ano.

  data: lt_nfdoc type table of j_1bnfdoc,
        ls_nfdoc type j_1bnfdoc.

  data: lt_0021 type table of zfiwrt0021.

  vano = sy-datum(4).
  select *
    from zfiwrt0021
    into table lt_0021
     where bukrs in s_bukrs
      and  ano   eq vano
      and  loekz  ne 'X'
      and  ger_autom eq 'X'.

  if  sy-datum+4(2) = '01'.
    vano = vano - 1.
    select *
     from zfiwrt0021
     appending table lt_0021
      where bukrs in s_bukrs
       and  ano   eq vano
       and  loekz  ne 'X'
       and  ger_autom eq 'X'.
  endif.
  if lt_0021[] is not initial.

    select *
      from zfiwrt0022 into table @data(lt_t0022)
      for all entries in @lt_0021
     where contrato eq @lt_0021-contrato
      and  bukrs    eq @lt_0021-bukrs
      and  branch   eq @lt_0021-branch
      and  kunnr    eq @lt_0021-kunnr.
  endif.
  sort lt_t0022 by seq_lcto.
  sort lt_0021 by contrato
                   bukrs
                   branch
                   kunnr
                   ano.

  if lt_t0022[] is not initial.
    select * from zfiwrt0008 into table @data(lt_t0008)
      for all entries in @lt_t0022
      where seq_lcto = @lt_t0022-seq_lcto.
    sort lt_t0008 by docnum.
    delete lt_t0008 where docnum is initial.
  endif.

  if lt_t0008[] is not initial.
    select * into table lt_nfdoc
      from j_1bnfdoc
      for all entries in lt_t0008
      where docnum = lt_t0008-docnum.
    sort lt_nfdoc by docnum.
  endif.

  loop at lt_t0008 into ls_t0008.
    read table lt_nfdoc into ls_nfdoc with key docnum = ls_t0008-docnum binary search.
    if sy-subrc ne 0 or ls_nfdoc-nfenum is initial.
      select  single *  from  zib_contabil_chv into @data(wl_zib_chv)
              where obj_key eq @ls_t0008-obj_key.
      if sy-subrc = 0 and wl_zib_chv-belnr is not initial. "Se gerou contabil
        try.
            zcl_nfe=>zif_doc_eletronico~get_instance(
            exporting
              i_docnum = ls_t0008-docnum
            )->set_registro(
              exporting
                i_docnum       = ls_t0008-docnum
                i_sem_bloqueio = abap_true
            )->set_autorizar(
            exporting
              i_aguardar = abap_false

            )->get_ck_autorizado_uso(

            )->get_registro(
            importing
              e_documento = data(e_documento)
            )->set_clear( ).

          catch zcx_doc_eletronico into data(ex_doc_eletronico).
            msg_id   = ex_doc_eletronico->msgid.
            msg_no   = ex_doc_eletronico->msgno.
            msg_var1 = ex_doc_eletronico->msgv1.
            msg_var2 = ex_doc_eletronico->msgv2.
            msg_var3 = ex_doc_eletronico->msgv3.
            msg_var4 = ex_doc_eletronico->msgv4.
            call function 'MESSAGE_PREPARE'
              exporting
                language = 'P'
                msg_id   = msg_id
                msg_no   = msg_no
                msg_var1 = msg_var1
                msg_var2 = msg_var2
                msg_var3 = msg_var3
                msg_var4 = msg_var4
              importing
                msg_text = wmessage.

            write: ls_t0008-docnum, '; ;KO - ', wmessage.
            clear wmessage.
            continue.
        endtry.
        write: ls_t0008-docnum, ';', e_documento-nfenum,';OK - Enviado a SEFAZ'.
      endif.
    endif.
  endloop.

endform.
