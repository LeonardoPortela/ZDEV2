*----------------------------------------------------------------------*
* Programa..: ZFIR0090                                                 *
* Tipo......: R - Report                                               *
* Transação.:                                                          *
* Descrição.: PROCESSO DE ENVIO DE RECEBIVEIS PARA AL5 BANK            *
* Autor.....: CBRAND                                                   *
* Data......: 16.04.2021                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data      |Request   |Autor       |Alteração                         *
*----------------------------------------------------------------------*
* 16.04.2021|          |CBRAND      |Codificação Inicial               *
*----------------------------------------------------------------------*
* 15/07/2025|DEVK9A2OOH|NSEGATIN    |Implementação Integra IDTrust/AL5.*
*                                   |Chamado: 185092.                  *
*----------------------------------------------------------------------*
report zfir0090.

tables: zfit0170 ,rgsbs ,lfa1.
*&---------------------------------------------------------------------*
* Declaração de Types
*&---------------------------------------------------------------------*
types:
  begin of ty_bsik,
    bukrs     type bsik-bukrs,
    gjahr     type bsik-gjahr,
    belnr     type bsik-belnr,
    buzei     type bsik-buzei,   "<<<------"184694 - NMS ------->>>
    lifnr     type bsik-lifnr,
    xblnr     type bsik-xblnr,
    blart     type bsik-blart,
    dmbtr     type bsik-dmbtr,
    ebeln     type bsik-ebeln,
    zfbdt     type bsik-zfbdt,
    zbd1t     type bsik-zbd1t,
    zlsch     type bsik-zlsch,
    budat     type bsik-budat,
    bldat     type bsik-bldat,
    bvtyp     type bsik-bvtyp,
    zuonr     type bsik-zuonr,
    zlspr     type bsik-zlspr,
    xdtvcto   type bsik-zfbdt,
    remove(1) type c,
  end of  ty_bsik,

  begin of ty_bkpf,
    bukrs  type bkpf-bukrs,
    belnr  type bkpf-belnr,
    gjahr  type bkpf-gjahr,
    awkey  type bkpf-awkey,
    refkey type j_1bnflin-refkey,
  end of ty_bkpf,
**<<<------"184694 - NMS - INI------>>>
*  BEGIN OF ty_header_arquivo,
*    cod_banco(3)              TYPE c,
*    lote_servico(4)           TYPE c,
*    tipo_registro(1)          TYPE c,
*    tipo_inscricao_empresa(1) TYPE c,
*    num_inscricao_empresa(14) TYPE c,
*    nome_empresa(30)          TYPE c,
*    reservado(100)            TYPE c,
*    cod_remessa_retorno(1)    TYPE c,
*    data_geracao_arquivo(8)   TYPE c,
*    hora_geracao_arquivo(6)   TYPE c,
*    num_sequencial_arquivo(6) TYPE c,
*    num_vs_layout_arquivo(3)  TYPE c,
*    reservado_banco(63)       TYPE c,
*  END OF ty_header_arquivo,
*
*  BEGIN OF ty_segmento_n1,
*    lote_servico(4)           TYPE c,
*    tipo_registro(1)          TYPE c,
*    cod_segmento(1)           TYPE c,
*    nr_segmento(1)            TYPE c,
*    tipo_movimento(1)         TYPE c,
*    tipo_inscricao_empresa(1) TYPE c,
*    num_inscricao_empresa(14) TYPE c,
*    nome_empresa(30)          TYPE c,
*    serie(3)                  TYPE c,
*    numero(9)                 TYPE c,
*    numero_referencia(15)     TYPE c,
*    valor(15)                 TYPE c,
*    data_emissao(08)          TYPE c,
*    tipo_nota(1)              TYPE c,
*    codigo_chave(44)          TYPE c,
*    reservado(92)             TYPE c,
*  END OF ty_segmento_n1,
*
*  BEGIN OF ty_segmento_d,
*    lote_servico(4)        TYPE c,
*    tipo_registro(1)       TYPE c,
*    seq_registro(5)        TYPE c,
*    cod_segmento(1)        TYPE c,
*    tipo_movimento(1)      TYPE c,
*    numero_dp(15)          TYPE c,
*    valor_dp(15)           TYPE c,
*    vencimento_dp(8)       TYPE c,
*    situcao_dp(2)          TYPE c,
*    serie_nf(03)           TYPE c,
*    numero_nf(09)          TYPE c,
*    data_emissao_nf(08)    TYPE c,
*    nr_insc_fornecedor(14) TYPE c,
*    reservado_01(87)       TYPE c,
*    reservado_02(67)       TYPE c,
*  END OF ty_segmento_d,
*
*  BEGIN OF ty_trailer_arquivo,
*    cod_banco(3)             TYPE c,
*    lote_servico(4)          TYPE c,
*    tipo_registro(1)         TYPE c,
*    reservado(9)             TYPE c,
*    qtd_reg_n1(6)            TYPE c,
*    qtd_registros_arquivo(6) TYPE c,
*    reservado_1(6)           TYPE c,
*    reservado_2(205)         TYPE c,
*  END OF ty_trailer_arquivo,
*
*  ty_arquivo(463) TYPE c,
**<<<------"184694 - NMS - FIM------>>>
  begin of ty_bukrs,
    bukrs type bsik-bukrs,
  end of ty_bukrs,
**<<<------"184694 - NMS - INI------>>>
*  BEGIN OF ty_split,
*    valor TYPE char3000,
*  END OF ty_split,
**<<<------"184694 - NMS - FIM------>>>
  begin of ty_f51,
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
  end of ty_f51.

data:begin of git_msg occurs 0.
       include structure bdcmsgcoll.
data: end of git_msg.


*&---------------------------------------------------------------------*
* Declaração de Tabelas
*&---------------------------------------------------------------------*
data:git_bsik           type table of ty_bsik,
     git_bsik_aux       type table of bsik,
     git_bukrs          type table of ty_bukrs with header line,
     git_zfit0169       type table of zfit0169,
     git_zfit0170       type table of zfit0170,
     tg_zfit0170_new    type table of zfit0170,      "<<<------"184694 - NMS ------->>>
     git_bkpf           type table of ty_bkpf,
     git_bsak           type table of bsak,
     git_ekko           type table of ekko,
     git_j_1bnflin      type table of j_1bnflin,
     git_j_1bnfe_active type table of j_1bnfe_active,
     git_lfa1           type table of lfa1,
     git_lfa1_pag       type table of lfa1 with header line,
**<<<------"184694 - NMS - INI------>>>
*     git_arquivo         TYPE TABLE OF ty_arquivo,
*     git_arquivo1        TYPE TABLE OF ty_arquivo,
*     git_locfile         TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
*     git_set_line_titles TYPE STANDARD TABLE OF rgsblt  WITH HEADER LINE,
**<<<------"184694 - NMS - FIM------>>>
     git_bdcdata        type standard table of bdcdata ,   "Guarda o mapeamento
     gwa_bdcdata        like line of git_bdcdata,
     git_f51            type table of ty_f51,
     t_zfit0170_compens type table of zfit0170,
     t_bsak_compens     type table of bsak,
**<<<------"184694 - NMS - INI------>>>
*     t_j_1bnflin_compens TYPE TABLE OF j_1bnflin,
*     t_j_1bnfe_compens   TYPE TABLE OF j_1bnfe_active,
*     t_bkpf_compens      TYPE TABLE OF ty_bkpf,
*     t_lfa1_compens      TYPE TABLE OF lfa1,
**<<<------"184694 - NMS - FIM------>>>
     t_zfit0170_vencto  type table of zfit0170,
     t_bsik_vencto      type table of bsak.
**<<<------"184694 - NMS - INI------>>>
*     t_j_1bnflin_vencto  TYPE TABLE OF j_1bnflin,
*     t_j_1bnfe_vencto    TYPE TABLE OF j_1bnfe_active,
*     t_bkpf_vencto       TYPE TABLE OF ty_bkpf,
*     t_lfa1_vencto       TYPE TABLE OF lfa1.
**<<<------"184694 - NMS - FIM------>>>
*----------------------------------------------------------------------*
* Declaração de Estruturas
*----------------------------------------------------------------------*
data: gwa_bsik           like line of  git_bsik,
      gwa_bsik_aux       like line of  git_bsik_aux,
      gwa_zfit0169       like line of  git_zfit0169,
      gwa_zfit0170       like line of  git_zfit0170,
      gwa_bkpf           like line of  git_bkpf,
      gwa_ekko           like line of  git_ekko,
      gwa_j_1bnflin      like line of  git_j_1bnflin,
      gwa_j_1bnfe_active like line of  git_j_1bnfe_active,
      gwa_lfa1           like line of  git_lfa1,
      gwa_lfa1_pag       like line of  git_lfa1_pag,
**<<<------"184694 - NMS - INI------>>>
*      gwa_header_arquivo  TYPE ty_header_arquivo,
*      gwa_segmento_n1     TYPE ty_segmento_n1,
*      gwa_segmento_d      TYPE ty_segmento_d,
*      gwa_trailer_arquivo TYPE ty_trailer_arquivo,
*      gwa_arquivo         LIKE LINE OF git_arquivo,
**<<<------"184694 - NMS - FIM------>>>
      gwa_f51            like line of git_f51.
*      lva_chave_doc(44).                          "<<<------"184694 - NMS ------->>>

*----------------------------------------------------------------------*
* Declaração de Ranges
*----------------------------------------------------------------------*
ranges: gra_tipo_ped for zfit0169-tipo_ped,
        gra_zlsch    for zfit0169-zlsch.

*&---------------------------------------------------------------------*
* Declaração de Constantes
*&---------------------------------------------------------------------*
field-symbols: <string>     type string.

*----------------------------------------------------------------------*
* Declaração de Variáveis
*----------------------------------------------------------------------*
data: split_tipo_ped  type table of string,
      split_form_pgto type table of string,
**<<<------"184694 - NMS - INI------>>>
*      gva_qnt_reg_n1  TYPE i,
*      gva_qnt_reg_d   TYPE i,
*      gva_qnt_arquivo TYPE i,
**<<<------"184694 - NMS - FIM------>>>
      gva_line        type sy-tabix,
*      gva_line_aux(3) TYPE c,           "<<<------"184694 - NMS ------->>>
      gva_lifnr       type lfa1-lifnr,
**<<<------"184694 - NMS - INI------>>>
*      gva_len0        TYPE i,
*      gva_len1        TYPE i,
*      gva_len2        TYPE i,
*      gva_bukrs       TYPE t001z-bukrs,
*      gva_paval       TYPE t001z-paval,
**<<<------"184694 - NMS - FIM------>>>
      gva_augbl       type zfit0170-augbl.
*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
selection-screen begin of block b01 with frame title text-t01.
  parameters: p_envio type char1 radiobutton group rgr1 default 'X' user-command usr,
              p_receb type char1 radiobutton group rgr1.
  select-options s_forn for  lfa1-lifnr.
selection-screen end of block b01.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------

start-of-selection.
  data: lva_job_e type i,
        lva_job_r type i.

  if p_envio = 'X'.
    select single count( * ) into lva_job_e
      from tbtco
     where jobname eq 'MAGGI_ZFIR0090_E'
       and status eq 'R'.

    check ( lva_job_e   eq 1 ).

    perform f_seleciona_dados.
    perform f_seleciona_compens.
    perform f_seleciona_vencto.

    if git_bukrs[] is initial.
      message 'Dados não econtrados.' type 'E'.
      exit.
    endif.
**<<<------"184694 - NMS - INI------>>>
*    perform f_trata_dados.
* Monta dados para chamada da API de comunicação.
    perform zf_monta_dados.
**<<<------"184694 - NMS - FIM------>>>
*    PERFORM f_salva_arquivo.
  else.

    select single count( * ) into lva_job_r
      from tbtco
     where jobname eq 'MAGGI_ZFIR0090_R'
       and status eq 'R'.

    check ( lva_job_r   eq 1 ).
**<<<------"184694 - NMS - INI------>>>
*    PERFORM f_busca_arquivo.
* Busca movimento - API Finanfor.
    perform zf_busca_movimentos.
**<<<------"184694 - NMS - FIM------>>>
    perform f_atu_doc_fornecedor.
  endif.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
form f_seleciona_dados .

  data: lva_datavenc type sy-datum,
        lv_data      type sy-datum,
        lv_data_util type sy-datum,
        htype        like dd01v-datatype.


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

  select * into table git_zfit0169
    from zfit0169.

  clear: split_tipo_ped.
  loop at git_zfit0169 into gwa_zfit0169.
    condense gwa_zfit0169-tipo_ped  no-gaps.
    split gwa_zfit0169-tipo_ped at '/' into table split_tipo_ped.
    condense gwa_zfit0169-zlsch  no-gaps.
    split gwa_zfit0169-zlsch at '/' into table split_form_pgto.
  endloop.

  refresh: gra_tipo_ped.
  loop at split_tipo_ped assigning <string>.
    gra_tipo_ped-sign   = 'I' .
    gra_tipo_ped-option = 'EQ'.
    gra_tipo_ped-low    = <string>.
    append gra_tipo_ped.
  endloop.

  refresh: gra_zlsch.
  loop at split_form_pgto assigning <string>.
    gra_zlsch-sign   = 'I' .
    gra_zlsch-option = 'EQ'.
    gra_zlsch-low    = <string>.
    append gra_zlsch.
  endloop.

  lv_data = sy-datum - 1. "alterado de -2 para -1

  lv_data_util = zcl_miro=>get_proximo_dia_util( exporting i_data_base = lv_data ).

  select bukrs
         gjahr
         belnr
         buzei   "<<<------"184694 - NMS ------->>>
         lifnr
         xblnr
         blart
         dmbtr
         ebeln
         zfbdt
         zbd1t
         zlsch
         budat
         bldat
         bvtyp
         zuonr
         zlspr
  into table git_bsik
  from bsik
    for all entries in git_zfit0169
    where bukrs eq git_zfit0169-bukrs
      and zlsch in gra_zlsch
      and zlspr	eq ''
      and ( ebeln ne '' or zuonr ne '' )
      and shkzg eq 'H'                                      "US150476
      and bstat ne 'S'                                      "partida memo não vai
      and gjahr eq sy-datum+0(4)
      and budat le lv_data_util
      and lifnr in s_forn.

  delete git_bsik where blart eq 'ZG'.
  delete git_bsik where blart eq 'ZH'.
  delete git_bsik where blart eq 'ZI'.
  delete git_bsik where lifnr eq git_forn-from.

  loop at git_bsik into gwa_bsik.
    clear: gva_line.
    move sy-tabix to gva_line.
    read table git_zfit0169 into gwa_zfit0169 with key bukrs = gwa_bsik-bukrs.
    lva_datavenc = ( sy-datum + gwa_zfit0169-qte_dias_pg ).
    gwa_bsik-xdtvcto =  gwa_bsik-zbd1t + gwa_bsik-zfbdt.
    if gwa_bsik-xdtvcto >= lva_datavenc.
      modify git_bsik from gwa_bsik index gva_line transporting xdtvcto.
    endif.
    clear htype.
    call function 'NUMERIC_CHECK' "Editing and checking numeric fields
      exporting
        string_in = gwa_bsik-zuonr
      importing
        htype     = htype.
    if htype = 'NUMC' and gwa_bsik-ebeln is initial.
      if strlen( gwa_bsik-zuonr ) between 8 and 10.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = gwa_bsik-zuonr
          importing
            output = gwa_bsik-ebeln.
        modify git_bsik from gwa_bsik index gva_line transporting ebeln.
      endif.
    endif.
  endloop.

  delete git_bsik where ebeln is initial.
  delete git_bsik where xdtvcto is initial.

  if git_bsik is not initial.

    select *
      into table git_ekko
    from ekko
      for all entries in git_bsik
      where ebeln eq git_bsik-ebeln
        and bsart in gra_tipo_ped.

    if git_ekko is not initial.
      loop at git_bsik into gwa_bsik .
        clear: gva_line.
        move sy-tabix to gva_line.
        read table git_ekko into gwa_ekko with key ebeln = gwa_bsik-ebeln.
        if sy-subrc is not initial.
          gwa_bsik-remove = 'X'.
          modify git_bsik from gwa_bsik index gva_line transporting remove.
        endif.
      endloop.
      delete git_bsik where remove = 'X'.
    endif.

    if git_bsik is not initial.
      select *
        into table git_zfit0170
        from zfit0170
          for all entries in git_bsik
          where bukrs eq git_bsik-bukrs
            and belnr eq git_bsik-belnr
            and buzei eq git_bsik-buzei    "<<<------"184694 - NMS ------->>>
            and gjahr eq git_bsik-gjahr.   "<<<------"184694 - NMS ------->>>

      if git_zfit0170 is not initial.
        sort git_bsik by bukrs belnr buzei gjahr.   "<<<------"184694 - NMS ------->>>
        sort git_zfit0170 by bukrs belnr buzei gjahr dt_envio hr_envio.
        loop at git_zfit0170 into gwa_zfit0170.
          read table git_bsik into gwa_bsik with key bukrs = gwa_zfit0170-bukrs
                                                     belnr = gwa_zfit0170-belnr
                                                     buzei = gwa_zfit0170-buzei                  "<<<------"184694 - NMS ------->>>
                                                     gjahr = gwa_zfit0170-gjahr binary search.   "<<<------"184694 - NMS ------->>>
          if sy-subrc         is initial.
            if gwa_zfit0170-zlspr is not initial. " Se tinha sido enviado o bloqueio e desbloqueou reeenvia
              gwa_bsik-remove = ''.
            else.
              gwa_bsik-remove = 'X'.
            endif.
            modify git_bsik from gwa_bsik index sy-tabix transporting remove.
          endif.

        endloop.
        delete git_bsik where remove = 'X'.
      endif.
    endif.

    "RETIRAR obrigatoriedade aceite
    if git_bsik is not initial.

      select *
         into table git_lfa1
      from lfa1
         for all entries in git_bsik
         where lifnr eq  git_bsik-lifnr.

*      loop at git_bsik into gwa_bsik.
*        clear: gva_line.
*        move sy-tabix to gva_line.
*
*        read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_bsik-lifnr.
*
*        if gwa_lfa1-stcd2 is not initial or
*           gwa_lfa1-j_1kftind ne 'AL5BAN-A'. " CPF  e termo de aceite US154129
*          gwa_bsik-remove = 'X'.
*          modify git_bsik from gwa_bsik index gva_line transporting remove.
*        endif.
*
*      endloop.
*      delete git_bsik where remove = 'X'.
      "RETIRAR obrigatoriedade aceite

    endif.


    if git_bsik is not initial.

      select bukrs
             belnr
             gjahr
             awkey
            into table git_bkpf
            from bkpf
              for all entries in git_bsik
              where bukrs   eq git_bsik-bukrs
                and belnr   eq git_bsik-belnr
                and gjahr	  eq git_bsik-gjahr.

*      loop at git_bkpf into data(lwa_bkpf).
*        lwa_bkpf-refkey = lwa_bkpf-awkey.
*        modify git_bkpf from lwa_bkpf transporting refkey.
*      endloop.

      " se for doc originado de compensação, busca o original
      select *
        from bsak
        into table git_bsak
        for all entries in git_bsik
         where bukrs   eq git_bsik-bukrs
          and  augbl   eq git_bsik-belnr
          and  gjahr   eq git_bsik-gjahr
          and  buzei   eq git_bsik-buzei   "<<<------"184694 - NMS ------->>>
          and  augbl   ne bsak~belnr.

      sort git_bsak by bukrs augbl gjahr buzei.   "<<<------"184694 - NMS ------->>>

      if sy-subrc = 0.
        select bukrs
              belnr
              gjahr
              awkey
             appending table git_bkpf
             from bkpf
               for all entries in git_bsak
               where bukrs   eq git_bsak-bukrs
                 and belnr   eq git_bsak-belnr
                 and gjahr   eq git_bsak-gjahr.
      endif.
      " se for doc originado de compensação, busca o original

      loop at git_bkpf into data(lwa_bkpf).
        lwa_bkpf-refkey = lwa_bkpf-awkey.
        modify git_bkpf from lwa_bkpf transporting refkey.
      endloop.

      if git_bkpf is not initial.
        select *
          into table git_j_1bnflin
        from j_1bnflin
          for all entries in git_bkpf
          where refkey eq git_bkpf-refkey.

        if git_j_1bnflin is not initial.
          select *
            into table git_j_1bnfe_active
          from j_1bnfe_active
            for all entries in git_j_1bnflin
             where docnum  eq  git_j_1bnflin-docnum.
        endif.
      endif.

*      select bsik~bukrs,
*             bsik~belnr,
*             bsik~buzei,
*             bsik~gjahr,
*             bkpf~xblnr
*          from bsik
*          inner join bkpf
*          on  bkpf~belnr = bsik~rebzg
*          and bkpf~gjahr = bsik~rebzj
*          into table @data(git_bsik_nota)
*          for all entries in  @git_bsik
*          where bsik~belnr = @git_bsik-belnr
*          and   bsik~bukrs = @git_bsik-bukrs
*          and   bsik~buzei = @git_bsik-buzei.
*
*      sort git_bsik_nota by bukrs belnr buzei gjahr.

      loop at git_bsik into gwa_bsik.
*        data(tabix) = sy-tabix.
*        if gwa_bsik-xblnr is initial.
*          read table git_bsik_nota into data(w_nota) with key bukrs = gwa_bsik-bukrs
*                                                              belnr = gwa_bsik-belnr
*                                                              buzei = gwa_bsik-buzei
*                                                              gjahr = gwa_bsik-gjahr binary search.
*          if sy-subrc = 0.
*            gwa_bsik-xblnr = w_nota-xblnr.
*            modify git_bsik from gwa_bsik index tabix transporting xblnr.
*          endif.
*        endif.

        concatenate gwa_bsik-bukrs+2(2) '01' into gva_lifnr.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = gva_lifnr
          importing
            output = gva_lifnr.

        select *
           appending table git_lfa1_pag
        from lfa1
           where lifnr eq  gva_lifnr.
        clear  gva_lifnr.

        append gwa_bsik-bukrs to git_bukrs.

        gwa_zfit0170-bukrs         = gwa_bsik-bukrs.
        gwa_zfit0170-belnr         = gwa_bsik-belnr.
        gwa_zfit0170-buzei         = gwa_bsik-buzei.   "<<<------"184694 - NMS ------->>>
        gwa_zfit0170-gjahr         = gwa_bsik-gjahr.
        gwa_zfit0170-status_env    = 'X'.
        gwa_zfit0170-dt_envio      = sy-datum.
        gwa_zfit0170-hr_envio      = sy-uzeit.
        gwa_zfit0170-tipo_mvto     = '0'.
        gwa_zfit0170-zfbdt         = gwa_bsik-zfbdt.
        gwa_zfit0170-zbd1t         = gwa_bsik-zbd1t.
        gwa_zfit0170-zlspr         = gwa_bsik-zlspr.

        modify zfit0170 from gwa_zfit0170.
        commit work.
        append gwa_zfit0170 to tg_zfit0170_new.     "<<<------"184694 - NMS ------->>>
        clear gwa_zfit0170.

      endloop.

      sort git_lfa1_pag[] by lifnr.
      sort git_bukrs[]    by bukrs.

      if git_lfa1_pag[] is not initial.
        delete adjacent duplicates from git_lfa1_pag[] comparing lifnr.
        delete adjacent duplicates from git_bukrs[]    comparing bukrs.
      endif.

    endif.

  endif.

endform.
**<<<------"184694 - NMS - INI------>>>
**&---------------------------------------------------------------------*
**&      Form  ZF_TRATA_DADOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_trata_dados .
*
*  gva_qnt_reg_n1  = 0.
*  gva_qnt_reg_d   = 0.
*  gva_qnt_arquivo = 0.
*
*  SORT git_bukrs.
*  DELETE ADJACENT DUPLICATES FROM git_bukrs.
*
*  LOOP AT git_bukrs.
*    CLEAR: git_arquivo1[].
*
*    PERFORM f_prenche_header_arquivo.
*    APPEND gwa_header_arquivo TO git_arquivo1.
*
*    PERFORM f_preenche_segmento_n1.
*    PERFORM f_preenche_segmento_n1_compens.
*    PERFORM f_preenche_segmento_n1_vencto.
*    PERFORM f_preenche_trailer_arquivo.
*    PERFORM f_salva_arquivo.
*
*  ENDLOOP.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  PRENCHE_HEADER_ARQUIVO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_prenche_header_arquivo .
*
*  CLEAR: gva_qnt_arquivo,
*         gwa_header_arquivo.
*
*  gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*  DATA: lva_proximo(6)        TYPE c.
*
*  gwa_header_arquivo-cod_banco              = '000'.
*  gwa_header_arquivo-lote_servico           = '0000'.
*  gwa_header_arquivo-tipo_registro          = '0'.
*
*  gwa_header_arquivo-tipo_inscricao_empresa = '2'.
*
*  CLEAR: gva_lifnr.
*  CONCATENATE git_bukrs+2(2) '01' INTO gva_lifnr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = gva_lifnr
*    IMPORTING
*      output = gva_lifnr.
*
*  READ TABLE git_lfa1_pag WITH KEY lifnr = gva_lifnr .
*
*  gwa_header_arquivo-num_inscricao_empresa  = git_lfa1_pag-stcd1.
*  gwa_header_arquivo-nome_empresa = git_lfa1_pag-name1.
*
*  PERFORM f_add_zero_esquerda    USING gwa_header_arquivo-num_inscricao_empresa.
*  PERFORM f_monta_espaco USING gwa_header_arquivo-nome_empresa.
*
*
*  PERFORM f_monta_espaco USING gwa_header_arquivo-reservado.
*
*  gwa_header_arquivo-cod_remessa_retorno = '1'. "Cliente para VERAGI
*
*  CONCATENATE  sy-datum+6(2)  sy-datum+4(2)  sy-datum+0(4) INTO gwa_header_arquivo-data_geracao_arquivo.
*  gwa_header_arquivo-hora_geracao_arquivo    = sy-uzeit.
*
*  PERFORM f_obtem_proximo_arquivo USING lva_proximo.
*
*  PERFORM f_add_zero_esquerda USING lva_proximo.
*  gwa_header_arquivo-num_sequencial_arquivo = lva_proximo.
*
*  gwa_header_arquivo-num_vs_layout_arquivo = '001'.
*
*  PERFORM f_monta_espaco USING gwa_header_arquivo-reservado_banco.
*
*  PERFORM f_format       USING gwa_header_arquivo.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  PREENCHE_SEGMENTO_N1
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_preenche_segmento_n1 .
*
*  CLEAR: gwa_segmento_n1.
*  DATA(_ok) = 'N'.
*  LOOP AT git_bsik INTO gwa_bsik WHERE bukrs = git_bukrs-bukrs.
*    _ok = 'S'.
*    EXIT.
*  ENDLOOP.
*  IF _ok = 'N'.
*    EXIT.
*  ENDIF.
*
**** Controle
*  gwa_segmento_n1-lote_servico   = '0001'.
*  gwa_segmento_n1-tipo_registro  = '3'.
*  gwa_segmento_n1-cod_segmento   = 'N'.
*  gwa_segmento_n1-nr_segmento    = '1'.
*  gwa_segmento_n1-tipo_movimento = '0'. "Inclusão.
*
*  LOOP AT git_bsik INTO gwa_bsik WHERE bukrs = git_bukrs-bukrs.
*
**** Dados do forncedor
*    READ TABLE git_lfa1 INTO gwa_lfa1 WITH KEY lifnr = gwa_bsik-lifnr.
*
*    IF gwa_lfa1-stcd2 IS NOT INITIAL. " CPF
*      gwa_segmento_n1-tipo_inscricao_empresa = '1'.
*      gwa_segmento_n1-num_inscricao_empresa = gwa_lfa1-stcd2.
*    ELSE.
*      gwa_segmento_n1-tipo_inscricao_empresa = '2'.
*      gwa_segmento_n1-num_inscricao_empresa = gwa_lfa1-stcd1.
*    ENDIF.
*
*    PERFORM f_add_zero_esquerda USING  gwa_segmento_n1-num_inscricao_empresa.
*
*    gwa_segmento_n1-nome_empresa = gwa_lfa1-name1.
*    PERFORM f_monta_espaco USING gwa_segmento_n1-nome_empresa.
*
**** nota fiscal
*    CLEAR: gva_len0,
*           gva_len1,
*           gva_len2.
*
*    IF gwa_bsik-xblnr IS NOT INITIAL.
*      IF gwa_bsik-xblnr CA '-' .
*        gva_len0 = sy-fdpos + 1.
*        gva_len1 = strlen( gwa_bsik-xblnr ) - gva_len0.
*
*        IF gva_len1 > 0.
*
*          gwa_segmento_n1-serie = gwa_bsik-xblnr+gva_len0(gva_len1).
*
*        ENDIF.
*
*        PERFORM f_add_zero_esquerda USING gwa_segmento_n1-serie.
*
*        gva_len2 = gva_len0 - 1.
*        gwa_segmento_n1-numero = gwa_bsik-xblnr+0(gva_len2).
*        PERFORM f_add_zero_esquerda USING gwa_segmento_n1-numero.
*      ELSE.
*        gwa_segmento_n1-numero  = gwa_bsik-xblnr.
*        PERFORM f_add_zero_esquerda USING gwa_segmento_n1-numero.
*        gwa_segmento_n1-serie   = '000'.
*      ENDIF.
*    ENDIF.
*
*    gwa_segmento_n1-numero_referencia = gwa_bsik-belnr.
*    PERFORM f_monta_espaco USING gwa_segmento_n1-numero_referencia.
*
*    MOVE gwa_bsik-dmbtr TO gwa_segmento_n1-valor.
*    REPLACE ',' IN gwa_segmento_n1-valor WITH  ' '.
*    REPLACE '.' IN gwa_segmento_n1-valor WITH  ' '.
*    CONDENSE gwa_segmento_n1-valor NO-GAPS.
*
*    PERFORM f_add_zero_esquerda USING  gwa_segmento_n1-valor.
*
*    CONCATENATE  gwa_bsik-bldat+6(2)  gwa_bsik-bldat+4(2)  gwa_bsik-bldat+0(4) INTO gwa_segmento_n1-data_emissao.
*
*    gwa_segmento_n1-tipo_nota = '1'.
*
*    CLEAR lva_chave_doc.
*    READ TABLE git_bkpf INTO gwa_bkpf WITH KEY bukrs = gwa_bsik-bukrs
*                                               belnr = gwa_bsik-belnr
*                                               gjahr = gwa_bsik-gjahr.
*
*    IF sy-subrc = 0.
*      READ TABLE git_bsak INTO DATA(gwa_bsak) WITH KEY bukrs = gwa_bsik-bukrs "Compensafo
*                                                       augbl = gwa_bsik-belnr
*                                                       gjahr = gwa_bsik-gjahr.
*      IF sy-subrc = 0.
*        READ TABLE git_bkpf INTO gwa_bkpf WITH KEY bukrs = gwa_bsak-bukrs
*                                                   belnr = gwa_bsak-belnr
*                                                   gjahr = gwa_bsak-gjahr.
*      ELSE.
*        sy-subrc = 0.
*      ENDIF.
*    ENDIF.
*
*    IF sy-subrc = 0.
*      READ TABLE git_j_1bnflin INTO gwa_j_1bnflin WITH KEY refkey  = gwa_bkpf-awkey.
*      IF sy-subrc = 0.
*        READ TABLE git_j_1bnfe_active INTO gwa_j_1bnfe_active  WITH  KEY docnum   = gwa_j_1bnflin-docnum.
*        IF sy-subrc = 0.
*          CONCATENATE gwa_j_1bnfe_active-regio gwa_j_1bnfe_active-nfyear gwa_j_1bnfe_active-nfmonth gwa_j_1bnfe_active-stcd1
*                    gwa_j_1bnfe_active-model gwa_j_1bnfe_active-serie  gwa_j_1bnfe_active-nfnum9  gwa_j_1bnfe_active-docnum9
*                    gwa_j_1bnfe_active-cdv INTO lva_chave_doc.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**    call function 'CONVERSION_EXIT_ALPHA_INPUT'
**      exporting
**        input  = lva_chave_doc
**      importing
**        output = lva_chave_doc.
*
*    IF strlen( lva_chave_doc ) NE 44. "Serviço municipal
*      gwa_segmento_n1-codigo_chave = '0'.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = gwa_segmento_n1-codigo_chave
*        IMPORTING
*          output = gwa_segmento_n1-codigo_chave.
*      PERFORM f_monta_espaco USING gwa_segmento_n1-codigo_chave.
*    ELSE.
*      gwa_segmento_n1-codigo_chave = lva_chave_doc.
*      PERFORM f_monta_espaco USING gwa_segmento_n1-codigo_chave.
*    ENDIF.
*
*    PERFORM f_monta_espaco USING gwa_segmento_n1-reservado.
*    PERFORM f_format       USING gwa_segmento_n1.
*
*    APPEND gwa_segmento_n1    TO git_arquivo1.
*
*    gva_qnt_reg_n1 = gva_qnt_reg_n1 + 1.
*    gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*    CLEAR: gwa_lfa1,
*           gwa_bkpf,
*           gwa_j_1bnflin,
*           gwa_j_1bnfe_active,
*           lva_chave_doc.
*
*    PERFORM f_preenche_segmento_d.
*
*  ENDLOOP.
*
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  PREENCHE_SEGMENTO_N1_COMPENS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_preenche_segmento_n1_compens .
*
*  TYPES:
*    BEGIN OF ty_bkpf,
*      bukrs  TYPE bkpf-bukrs,
*      belnr  TYPE bkpf-belnr,
*      gjahr  TYPE bkpf-gjahr,
*      awkey  TYPE bkpf-awkey,
*      refkey TYPE j_1bnflin-refkey,
*    END OF ty_bkpf.
*
*  DATA: lv_dt       TYPE datum,
*        lt_bkpf     TYPE TABLE OF ty_bkpf,
*        lt_zfit0170 TYPE TABLE OF zfit0170.
*
*  CLEAR: gwa_segmento_n1.
*
*  DATA(_ok) = 'N'.
*  LOOP AT t_bsak_compens ASSIGNING FIELD-SYMBOL(<fs_bsak>) WHERE bukrs = git_bukrs.
*    _ok = 'S'.
*    EXIT.
*  ENDLOOP.
*  IF _ok = 'N'.
*    EXIT.
*  ENDIF.
*
*
*  SORT t_bsak_compens BY bukrs belnr gjahr.
*
**** Controle
*  gwa_segmento_n1-lote_servico   = '0001'.
*  gwa_segmento_n1-tipo_registro  = '3'.
*  gwa_segmento_n1-cod_segmento   = 'N'.
*  gwa_segmento_n1-nr_segmento    = '1'.
*  gwa_segmento_n1-tipo_movimento = '9'. "Inclusão.
*
*  CHECK t_zfit0170_compens IS NOT INITIAL.
*
*  LOOP AT t_bsak_compens ASSIGNING <fs_bsak> WHERE bukrs = git_bukrs.
*
**** Dados do forncedor
*    READ TABLE t_lfa1_compens INTO gwa_lfa1
*    WITH KEY lifnr = <fs_bsak>-lifnr
*    BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      IF gwa_lfa1-stcd2 IS NOT INITIAL. " CPF
*        gwa_segmento_n1-tipo_inscricao_empresa = '1'.
*        gwa_segmento_n1-num_inscricao_empresa = gwa_lfa1-stcd2.
*      ELSE.
*        gwa_segmento_n1-tipo_inscricao_empresa = '2'.
*        gwa_segmento_n1-num_inscricao_empresa = gwa_lfa1-stcd1.
*      ENDIF.
*    ENDIF.
*
*    PERFORM f_add_zero_esquerda USING  gwa_segmento_n1-num_inscricao_empresa.
*
*    gwa_segmento_n1-nome_empresa = gwa_lfa1-name1.
*    PERFORM f_monta_espaco USING gwa_segmento_n1-nome_empresa.
*
**** nota fiscal
*    CLEAR: gva_len0,
*           gva_len1,
*           gva_len2.
*
*                                                            "BUG183801
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = <fs_bsak>-belnr
*      IMPORTING
*        output = gwa_segmento_n1-numero.
*    gwa_segmento_n1-serie   = '000'.
*
**    if <fs_bsak>-xblnr is not initial.
**      if <fs_bsak>-xblnr ca '-' .
**        gva_len0 = sy-fdpos + 1.
**        gva_len1 = strlen( <fs_bsak>-xblnr ) - gva_len0.
**
**        if gva_len1 > 0.
**
**          gwa_segmento_n1-serie = <fs_bsak>-xblnr+gva_len0(gva_len1).
**
**        endif.
**
**        perform f_add_zero_esquerda using gwa_segmento_n1-serie.
**
**        gva_len2 = gva_len0 - 1.
**        gwa_segmento_n1-numero = <fs_bsak>-xblnr+0(gva_len2).
**        perform f_add_zero_esquerda using gwa_segmento_n1-numero.
**      elseif <fs_bsak>-xblnr is not initial.
**        gwa_segmento_n1-numero  = <fs_bsak>-xblnr.
**        perform f_add_zero_esquerda using gwa_segmento_n1-numero.
**        gwa_segmento_n1-serie   = '000'.
**      endif.
**    endif.
*                                                            "BUG183801
*
*    gwa_segmento_n1-numero_referencia = <fs_bsak>-belnr.
*    PERFORM f_monta_espaco USING gwa_segmento_n1-numero_referencia.
*
*    MOVE <fs_bsak>-dmbtr TO gwa_segmento_n1-valor.
*    REPLACE ',' IN gwa_segmento_n1-valor WITH  ' '.
*    REPLACE '.' IN gwa_segmento_n1-valor WITH  ' '.
*    CONDENSE gwa_segmento_n1-valor NO-GAPS.
*
*    PERFORM f_add_zero_esquerda USING  gwa_segmento_n1-valor.
*
*    CONCATENATE  <fs_bsak>-bldat+6(2)  <fs_bsak>-bldat+4(2)  <fs_bsak>-bldat+0(4) INTO gwa_segmento_n1-data_emissao.
*
*    gwa_segmento_n1-tipo_nota = '1'.
*
*    CLEAR lva_chave_doc.
*    READ TABLE t_bkpf_compens ASSIGNING FIELD-SYMBOL(<fs_bkpf>)
*    WITH KEY bukrs = <fs_bsak>-bukrs
*             belnr = <fs_bsak>-belnr
*             gjahr = <fs_bsak>-gjahr   BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      READ TABLE t_j_1bnflin_compens ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
*      WITH KEY refkey  = <fs_bkpf>-awkey
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        READ TABLE t_j_1bnfe_compens ASSIGNING FIELD-SYMBOL(<fs_j_1bnfe>)
*        WITH  KEY docnum = <fs_j_1bnflin>-docnum
*        BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          CONCATENATE <fs_j_1bnfe>-regio <fs_j_1bnfe>-nfyear <fs_j_1bnfe>-nfmonth <fs_j_1bnfe>-stcd1
*                    <fs_j_1bnfe>-model <fs_j_1bnfe>-serie  <fs_j_1bnfe>-nfnum9  <fs_j_1bnfe>-docnum9
*                    <fs_j_1bnfe>-cdv INTO lva_chave_doc.
*          gwa_segmento_n1-codigo_chave = lva_chave_doc.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*    IF strlen( lva_chave_doc ) NE 44. "Serviço municipal
*      gwa_segmento_n1-codigo_chave = '0'.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = gwa_segmento_n1-codigo_chave
*        IMPORTING
*          output = gwa_segmento_n1-codigo_chave.
*      PERFORM f_monta_espaco USING gwa_segmento_n1-codigo_chave.
*    ELSE.
*      gwa_segmento_n1-codigo_chave = lva_chave_doc.
*      PERFORM f_monta_espaco USING gwa_segmento_n1-codigo_chave.
*    ENDIF.
*
*
*    PERFORM f_monta_espaco USING gwa_segmento_n1-reservado.
*    PERFORM f_format       USING gwa_segmento_n1.
*
*    APPEND gwa_segmento_n1    TO git_arquivo1.
*
*    gva_qnt_reg_n1 = gva_qnt_reg_n1 + 1.
*    gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*    CLEAR: gwa_lfa1,
*           gwa_j_1bnflin,
*           gwa_j_1bnfe_active,
*           lva_chave_doc.
*
*    PERFORM f_preenche_segmento_d_compens USING <fs_bsak>.
*
*    READ TABLE t_zfit0170_compens ASSIGNING FIELD-SYMBOL(<fs_zfit0170_compens>)
*    WITH KEY bukrs = <fs_bsak>-bukrs
*             belnr = <fs_bsak>-belnr
*             gjahr = <fs_bsak>-gjahr
*    BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      <fs_zfit0170_compens>-tipo_mvto = 'X'. "para não selecionar mais
*      APPEND INITIAL LINE TO lt_zfit0170 ASSIGNING FIELD-SYMBOL(<fs_zfit0170>).
*      <fs_zfit0170> = <fs_zfit0170_compens>.
*
*      <fs_zfit0170>-tipo_mvto = '9'.
*      <fs_zfit0170>-zfbdt     = <fs_bsak>-zfbdt.
*      <fs_zfit0170>-zbd1t     = <fs_bsak>-zbd1t.
*      <fs_zfit0170>-dt_envio = sy-datum.
*      <fs_zfit0170>-hr_envio = sy-uzeit.
*
*    ENDIF.
*
*  ENDLOOP.
*
*  IF lt_zfit0170 IS NOT INITIAL.
*    MODIFY zfit0170 FROM TABLE t_zfit0170_compens. "marca "X" no que alterou pra "9"
*    MODIFY zfit0170 FROM TABLE lt_zfit0170.
*    IF sy-subrc IS INITIAL.
*      COMMIT WORK.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  PREENCHE_SEGMENTO_N1_COMPENS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_preenche_segmento_n1_vencto .
*
*  TYPES:
*    BEGIN OF ty_bkpf,
*      bukrs  TYPE bkpf-bukrs,
*      belnr  TYPE bkpf-belnr,
*      gjahr  TYPE bkpf-gjahr,
*      awkey  TYPE bkpf-awkey,
*      refkey TYPE j_1bnflin-refkey,
*    END OF ty_bkpf.
*
*  DATA: lv_dt        TYPE datum,
*        lt_bkpf      TYPE TABLE OF ty_bkpf,
*        lt_zfit0170  TYPE TABLE OF zfit0170,
*        lva_datavenc TYPE sy-datum.
*
*  CLEAR: gwa_segmento_n1.
*
*  DATA(_ok) = 'N'.
*  LOOP AT t_bsik_vencto ASSIGNING FIELD-SYMBOL(<fs_bsik>) WHERE bukrs = git_bukrs.
*    _ok = 'S'.
*    EXIT.
*  ENDLOOP.
*  IF _ok = 'N'.
*    EXIT.
*  ENDIF.
*
*  SORT:  t_zfit0170_vencto BY bukrs belnr gjahr,
*         t_bsik_vencto     BY bukrs belnr gjahr,
*         t_lfa1_vencto     BY lifnr.
*
**** Controle
*  gwa_segmento_n1-lote_servico   = '0001'.
*  gwa_segmento_n1-tipo_registro  = '3'.
*  gwa_segmento_n1-cod_segmento   = 'N'.
*  gwa_segmento_n1-nr_segmento    = '1'.
*  gwa_segmento_n1-tipo_movimento = '5'. "Inclusão.
*
*
*  CHECK t_zfit0170_vencto IS NOT INITIAL.
*
*  LOOP AT t_bsik_vencto ASSIGNING <fs_bsik> WHERE bukrs = git_bukrs.
*
*    READ TABLE t_zfit0170_vencto ASSIGNING FIELD-SYMBOL(<fs_zfit0170_vencto>)
*    WITH KEY bukrs = <fs_bsik>-bukrs
*             belnr = <fs_bsik>-belnr
*             gjahr = <fs_bsik>-gjahr
*    BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      IF <fs_zfit0170_vencto>-zfbdt <> <fs_bsik>-zfbdt OR
*         <fs_zfit0170_vencto>-zbd1t <> <fs_bsik>-zbd1t OR
*         <fs_bsik>-zlspr IS NOT INITIAL. "bloqueado
*        <fs_zfit0170_vencto>-tipo_mvto = 'X'.
*        APPEND INITIAL LINE TO lt_zfit0170 ASSIGNING FIELD-SYMBOL(<fs_zfit0170>).
*        <fs_zfit0170> = <fs_zfit0170_vencto>.
*        IF <fs_bsik>-zlspr IS NOT INITIAL. "bloqueado
*          <fs_zfit0170>-tipo_mvto = '5'.
*          gwa_segmento_n1-tipo_movimento = '5'.
*        ELSE.
*          <fs_zfit0170>-tipo_mvto = '5'.
*          gwa_segmento_n1-tipo_movimento = '5'.
*        ENDIF.
*        <fs_zfit0170>-zfbdt = <fs_bsik>-zfbdt.
*        <fs_zfit0170>-zbd1t = <fs_bsik>-zbd1t.
*        <fs_zfit0170>-dt_envio = sy-datum.
*        <fs_zfit0170>-hr_envio = sy-uzeit.
*
*        " se passar dos dias exclui
*        READ TABLE git_zfit0169 INTO gwa_zfit0169 WITH KEY bukrs = <fs_bsik>-bukrs.
*        lva_datavenc = ( sy-datum + gwa_zfit0169-qte_dias_pg ).
*        gwa_bsik-xdtvcto =  <fs_bsik>-zbd1t + <fs_bsik>-zfbdt.
*        IF gwa_bsik-xdtvcto < lva_datavenc.
*          <fs_zfit0170>-tipo_mvto = '9'.
*          gwa_segmento_n1-tipo_movimento = '9'.
*        ENDIF.
*
**** Dados do forncedor
*        READ TABLE t_lfa1_vencto INTO gwa_lfa1
*        WITH KEY lifnr = <fs_bsik>-lifnr
*        BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          IF gwa_lfa1-stcd2 IS NOT INITIAL. " CPF
*            gwa_segmento_n1-tipo_inscricao_empresa = '1'.
*            gwa_segmento_n1-num_inscricao_empresa = gwa_lfa1-stcd2.
*          ELSE.
*            gwa_segmento_n1-tipo_inscricao_empresa = '2'.
*            gwa_segmento_n1-num_inscricao_empresa = gwa_lfa1-stcd1.
*          ENDIF.
*        ENDIF.
*
*        PERFORM f_add_zero_esquerda USING  gwa_segmento_n1-num_inscricao_empresa.
*
*        gwa_segmento_n1-nome_empresa = gwa_lfa1-name1.
*        PERFORM f_monta_espaco USING gwa_segmento_n1-nome_empresa.
*
**** nota fiscal
*        CLEAR: gva_len0,
*               gva_len1,
*               gva_len2.
*
*        IF <fs_bsik>-xblnr IS NOT INITIAL.
*          IF <fs_bsik>-xblnr CA '-' .
*            gva_len0 = sy-fdpos + 1.
*            gva_len1 = strlen( <fs_bsik>-xblnr ) - gva_len0.
*
*            IF gva_len1 > 0.
*              gwa_segmento_n1-serie = <fs_bsik>-xblnr+gva_len0(gva_len1).
*            ENDIF.
*
*            PERFORM f_add_zero_esquerda USING gwa_segmento_n1-serie.
*
*            gva_len2 = gva_len0 - 1.
*            gwa_segmento_n1-numero = <fs_bsik>-xblnr+0(gva_len2).
*            PERFORM f_add_zero_esquerda USING gwa_segmento_n1-numero.
*          ELSE.
*            gwa_segmento_n1-numero  = <fs_bsik>-xblnr.
*            PERFORM f_add_zero_esquerda USING gwa_segmento_n1-numero.
*            gwa_segmento_n1-serie   = '000'.
*          ENDIF.
*        ENDIF.
*
*        gwa_segmento_n1-numero_referencia = <fs_bsik>-belnr.
*        PERFORM f_monta_espaco USING gwa_segmento_n1-numero_referencia.
*
*        MOVE <fs_bsik>-dmbtr TO gwa_segmento_n1-valor.
*        REPLACE ',' IN gwa_segmento_n1-valor WITH  ' '.
*        REPLACE '.' IN gwa_segmento_n1-valor WITH  ' '.
*        CONDENSE gwa_segmento_n1-valor NO-GAPS.
*
*        PERFORM f_add_zero_esquerda USING  gwa_segmento_n1-valor.
*
*        CONCATENATE  <fs_bsik>-bldat+6(2)  <fs_bsik>-bldat+4(2)  <fs_bsik>-bldat+0(4) INTO gwa_segmento_n1-data_emissao.
*
*        gwa_segmento_n1-tipo_nota = '1'.
*
*        CLEAR lva_chave_doc.
*        READ TABLE t_bkpf_vencto ASSIGNING FIELD-SYMBOL(<fs_bkpf>)
*        WITH KEY bukrs = <fs_bsik>-bukrs
*                 belnr = <fs_bsik>-belnr
*                 gjahr = <fs_bsik>-gjahr     BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          READ TABLE t_j_1bnflin_vencto ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
*          WITH KEY refkey  = <fs_bkpf>-awkey
*          BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            READ TABLE t_j_1bnfe_vencto ASSIGNING FIELD-SYMBOL(<fs_j_1bnfe>)
*            WITH  KEY docnum = <fs_j_1bnflin>-docnum
*            BINARY SEARCH.
*            IF sy-subrc IS INITIAL.
*              CONCATENATE <fs_j_1bnfe>-regio <fs_j_1bnfe>-nfyear <fs_j_1bnfe>-nfmonth <fs_j_1bnfe>-stcd1
*                        <fs_j_1bnfe>-model <fs_j_1bnfe>-serie  <fs_j_1bnfe>-nfnum9  <fs_j_1bnfe>-docnum9
*                        <fs_j_1bnfe>-cdv INTO lva_chave_doc.
*              gwa_segmento_n1-codigo_chave = lva_chave_doc.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*        IF strlen( lva_chave_doc ) NE 44. "Serviço municipal
*          gwa_segmento_n1-codigo_chave = '0'.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_segmento_n1-codigo_chave
*            IMPORTING
*              output = gwa_segmento_n1-codigo_chave.
*          PERFORM f_monta_espaco USING gwa_segmento_n1-codigo_chave.
*        ELSE.
*          gwa_segmento_n1-codigo_chave = lva_chave_doc.
*          PERFORM f_monta_espaco USING gwa_segmento_n1-codigo_chave.
*        ENDIF.
*
*        PERFORM f_monta_espaco USING gwa_segmento_n1-reservado.
*        PERFORM f_format       USING gwa_segmento_n1.
*
*        APPEND gwa_segmento_n1    TO git_arquivo1.
*
*        gva_qnt_reg_n1 = gva_qnt_reg_n1 + 1.
*        gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*        CLEAR: gwa_lfa1,
*               git_bkpf,
*               gwa_j_1bnflin,
*               gwa_j_1bnfe_active,
*               lva_chave_doc.
*
*        PERFORM f_preenche_segmento_d_vencto USING <fs_bsik>.
*
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
*
*  IF lt_zfit0170 IS NOT INITIAL.
*    MODIFY zfit0170 FROM TABLE t_zfit0170_vencto.
*    MODIFY zfit0170 FROM TABLE lt_zfit0170.
*    IF sy-subrc IS INITIAL.
*      COMMIT WORK.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_PREENCHE_SEGMENTO_D
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_preenche_segmento_d .
*
*
*  CLEAR: gwa_segmento_d.
*
**** Controle
*  gwa_segmento_d-lote_servico = '0001'.
*  gwa_segmento_d-tipo_registro = '3'.
*  gwa_segmento_d-seq_registro = gva_qnt_reg_n1.
*
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-seq_registro.
*
*  gwa_segmento_d-cod_segmento   = 'D'.
*  gwa_segmento_d-tipo_movimento = '0'. " 0 - inclusão e 9 é exclusão.
*
**** Duplicata
*  gwa_segmento_d-numero_dp = gwa_bsik-ebeln.
*  PERFORM f_monta_espaco USING gwa_segmento_d-numero_dp.
*
*  MOVE gwa_bsik-dmbtr TO gwa_segmento_d-valor_dp.
*  REPLACE ',' IN gwa_segmento_d-valor_dp WITH  ' '.
*  REPLACE '.' IN gwa_segmento_d-valor_dp WITH  ' '.
*  CONDENSE gwa_segmento_d-valor_dp NO-GAPS.
*
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-valor_dp.
*
*  CONCATENATE  gwa_bsik-xdtvcto+6(2)  gwa_bsik-xdtvcto+4(2)  gwa_bsik-xdtvcto+0(4) INTO gwa_segmento_d-vencimento_dp.
*  gwa_segmento_d-situcao_dp = '01'.
*
**** Nota Fiscal
*
*  CLEAR: gva_len0,
*         gva_len1,
*         gva_len2.
*
*  IF gwa_bsik-xblnr IS NOT INITIAL.
*
*    IF gwa_bsik-xblnr CA '-' .
*
*      gva_len0 = sy-fdpos + 1.
*      gva_len1 = strlen( gwa_bsik-xblnr ) - gva_len0 .
*
*      IF gva_len1 > 0.
*        gwa_segmento_d-serie_nf = gwa_bsik-xblnr+gva_len0(gva_len1).
*      ENDIF.
*
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-serie_nf.
*
*      gva_len2 = gva_len0 - 1.
*      gwa_segmento_d-numero_nf = gwa_bsik-xblnr+0(gva_len2).
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-numero_nf.
*    ELSE.
*      gwa_segmento_d-numero_nf = gwa_bsik-xblnr.
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-numero_nf.
*      gwa_segmento_d-serie_nf = '000'.
*    ENDIF.
*  ENDIF.
*
*  CONCATENATE  gwa_bsik-bldat+6(2)  gwa_bsik-bldat+4(2)  gwa_bsik-bldat+0(4) INTO gwa_segmento_d-data_emissao_nf.
*
*  CLEAR: gwa_lfa1.
*  READ TABLE git_lfa1 INTO gwa_lfa1 WITH KEY lifnr = gwa_bsik-lifnr.
*
*  IF gwa_lfa1-stcd2 IS NOT INITIAL. " CPF
*    gwa_segmento_d-nr_insc_fornecedor = gwa_lfa1-stcd2.
*  ELSE.
*    gwa_segmento_d-nr_insc_fornecedor = gwa_lfa1-stcd1.
*  ENDIF.
*  PERFORM f_add_zero_esquerda USING  gwa_segmento_d-nr_insc_fornecedor.
*
*  gwa_segmento_d-reservado_01 = '0'.
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-reservado_01.
*  PERFORM f_monta_espaco USING gwa_segmento_d-reservado_02.
*
*  APPEND gwa_segmento_d TO git_arquivo1.
*
*  gva_qnt_reg_d = gva_qnt_reg_d + 1.
*  gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_PREENCHE_SEGMENTO_D
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_preenche_segmento_d_compens USING p_bsak TYPE bsak.
*
*
*  CLEAR: gwa_segmento_d.
*
**** Controle
*  gwa_segmento_d-lote_servico = '0001'.
*  gwa_segmento_d-tipo_registro = '3'.
*  gwa_segmento_d-seq_registro = gva_qnt_reg_n1.
*
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-seq_registro.
*
*  gwa_segmento_d-cod_segmento   = 'D'.
*  gwa_segmento_d-tipo_movimento = '9'. " 0 - inclusão e 9 é exclusão.
*
**** Duplicata
*  gwa_segmento_d-numero_dp = p_bsak-ebeln.
*  PERFORM f_monta_espaco USING gwa_segmento_d-numero_dp.
*
*  MOVE p_bsak-dmbtr TO gwa_segmento_d-valor_dp.
*  REPLACE ',' IN gwa_segmento_d-valor_dp WITH  ' '.
*  REPLACE '.' IN gwa_segmento_d-valor_dp WITH  ' '.
*  CONDENSE gwa_segmento_d-valor_dp NO-GAPS.
*
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-valor_dp.
*
*  p_bsak-zfbdt =  p_bsak-zbd1t + p_bsak-zfbdt.
*  CONCATENATE  p_bsak-zfbdt+6(2)  p_bsak-zfbdt+4(2)  p_bsak-zfbdt+0(4) INTO gwa_segmento_d-vencimento_dp.
*  gwa_segmento_d-situcao_dp = '01'.
*  IF p_bsak-augbl+0(2) = '15' OR
*     p_bsak-augbl+0(2) = '20'.
*    gwa_segmento_d-situcao_dp = '05'.
*  ENDIF.
*
**** Nota Fiscal
*
*  CLEAR: gva_len0,
*         gva_len1,
*         gva_len2.
*
*  IF p_bsak-xblnr IS NOT INITIAL.
*
*    IF p_bsak-xblnr CA '-' .
*
*      gva_len0 = sy-fdpos + 1.
*      gva_len1 = strlen( p_bsak-xblnr ) - gva_len0 .
*
*      IF gva_len1 > 0.
*        gwa_segmento_d-serie_nf = p_bsak-xblnr+gva_len0(gva_len1).
*      ENDIF.
*
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-serie_nf.
*
*      gva_len2 = gva_len0 - 1.
*      gwa_segmento_d-numero_nf = p_bsak-xblnr+0(gva_len2).
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-numero_nf.
*    ELSE.
*      gwa_segmento_d-numero_nf = p_bsak-xblnr.
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-numero_nf.
*      gwa_segmento_d-serie_nf = '000'.
*    ENDIF.
*  ENDIF.
*
*  CONCATENATE  p_bsak-bldat+6(2)  p_bsak-bldat+4(2)  p_bsak-bldat+0(4) INTO gwa_segmento_d-data_emissao_nf.
*
*  CLEAR: gwa_lfa1.
*  READ TABLE t_lfa1_compens ASSIGNING FIELD-SYMBOL(<fs_lfa1>) WITH KEY lifnr = p_bsak-lifnr.
*  IF sy-subrc IS INITIAL.
*
*    IF <fs_lfa1>-stcd2 IS NOT INITIAL. " CPF
*      gwa_segmento_d-nr_insc_fornecedor = <fs_lfa1>-stcd2.
*    ELSE.
*      gwa_segmento_d-nr_insc_fornecedor = <fs_lfa1>-stcd1.
*    ENDIF.
*
*  ENDIF.
*
*  PERFORM f_add_zero_esquerda USING  gwa_segmento_d-nr_insc_fornecedor.
*
*  gwa_segmento_d-reservado_01 = '0'.
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-reservado_01.
*  PERFORM f_monta_espaco USING gwa_segmento_d-reservado_02.
*
*  APPEND gwa_segmento_d TO git_arquivo1.
*
*  gva_qnt_reg_d = gva_qnt_reg_d + 1.
*  gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_PREENCHE_SEGMENTO_D
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_preenche_segmento_d_vencto USING p_bsik TYPE bsik.
*
*
*  CLEAR: gwa_segmento_d.
*
**** Controle
*  gwa_segmento_d-lote_servico = '0001'.
*  gwa_segmento_d-tipo_registro = '3'.
*  gwa_segmento_d-seq_registro = gva_qnt_reg_n1.
*
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-seq_registro.
*
*  gwa_segmento_d-cod_segmento   = 'D'.
*  IF gwa_segmento_n1-tipo_movimento = '5'.
**    gwa_segmento_d-tipo_movimento = '0'. " 0 - inclusão e 9 é exclusão.
*    gwa_segmento_d-tipo_movimento = '5'. " 0 - inclusão e 9 é exclusão.1
*  ELSE.
*    gwa_segmento_d-tipo_movimento = '9'. " EXCLUIR SE DATA VENCIMENTO ALTERADA É MENOR QUE PARAMETRO OU BLOQUEADO
*  ENDIF.
*
**** Duplicata
*  gwa_segmento_d-numero_dp = p_bsik-ebeln.
*  PERFORM f_monta_espaco USING gwa_segmento_d-numero_dp.
*
*  MOVE p_bsik-dmbtr TO gwa_segmento_d-valor_dp.
*  REPLACE ',' IN gwa_segmento_d-valor_dp WITH  ' '.
*  REPLACE '.' IN gwa_segmento_d-valor_dp WITH  ' '.
*  CONDENSE gwa_segmento_d-valor_dp NO-GAPS.
*
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-valor_dp.
*  p_bsik-zfbdt =  p_bsik-zbd1t + p_bsik-zfbdt.
*  CONCATENATE  p_bsik-zfbdt+6(2)  p_bsik-zfbdt+4(2)  p_bsik-zfbdt+0(4) INTO gwa_segmento_d-vencimento_dp.
*  gwa_segmento_d-situcao_dp = '01'.
*  IF p_bsik-zlspr IS NOT INITIAL. "bloqueado.
*    gwa_segmento_d-situcao_dp = '99'.
*  ENDIF.
*
**** Nota Fiscal
*
*  CLEAR: gva_len0,
*         gva_len1,
*         gva_len2.
*
*  IF p_bsik-xblnr IS NOT INITIAL.
*
*    IF p_bsik-xblnr CA '-' .
*
*      gva_len0 = sy-fdpos + 1.
*      gva_len1 = strlen( p_bsik-xblnr ) - gva_len0 .
*
*      IF gva_len1 > 0.
*        gwa_segmento_d-serie_nf = p_bsik-xblnr+gva_len0(gva_len1).
*      ENDIF.
*
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-serie_nf.
*
*      gva_len2 = gva_len0 - 1.
*      gwa_segmento_d-numero_nf = p_bsik-xblnr+0(gva_len2).
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-numero_nf.
*    ELSE.
*      gwa_segmento_d-numero_nf = p_bsik-xblnr.
*      PERFORM f_add_zero_esquerda USING gwa_segmento_d-numero_nf.
*      gwa_segmento_d-serie_nf   = '000'.
*    ENDIF.
*
*  ENDIF.
*
*  CONCATENATE  p_bsik-bldat+6(2)  p_bsik-bldat+4(2)  p_bsik-bldat+0(4) INTO gwa_segmento_d-data_emissao_nf.
*
*  CLEAR: gwa_lfa1.
*  READ TABLE t_lfa1_vencto ASSIGNING FIELD-SYMBOL(<fs_lfa1>) WITH KEY lifnr = p_bsik-lifnr.
*  IF sy-subrc IS INITIAL.
*
*    IF <fs_lfa1>-stcd2 IS NOT INITIAL. " CPF
*      gwa_segmento_d-nr_insc_fornecedor = <fs_lfa1>-stcd2.
*    ELSE.
*      gwa_segmento_d-nr_insc_fornecedor = <fs_lfa1>-stcd1.
*    ENDIF.
*
*  ENDIF.
*
*  PERFORM f_add_zero_esquerda USING  gwa_segmento_d-nr_insc_fornecedor.
*
*  gwa_segmento_d-reservado_01 = '0'.
*  PERFORM f_add_zero_esquerda USING gwa_segmento_d-reservado_01.
*  PERFORM f_monta_espaco USING gwa_segmento_d-reservado_02.
*
*  APPEND gwa_segmento_d TO git_arquivo1.
*
*  gva_qnt_reg_d = gva_qnt_reg_d + 1.
*  gva_qnt_arquivo = gva_qnt_arquivo + 1.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_PREENCHE_TRAILER_ARQUIVO
**&---------------------------------------------------------------------*
*FORM f_preenche_trailer_arquivo .
*
*  CLEAR: gwa_trailer_arquivo.
*
**** Controle
*  gwa_trailer_arquivo-cod_banco     = '000'.
*  gwa_trailer_arquivo-lote_servico  = '9999'.
*  gwa_trailer_arquivo-tipo_registro = '9'.
*  PERFORM f_monta_espaco USING gwa_trailer_arquivo-reservado.
*
**** Totais
*  gwa_trailer_arquivo-qtd_reg_n1 = gva_qnt_reg_n1.
*  PERFORM f_add_zero_esquerda USING gwa_trailer_arquivo-qtd_reg_n1.
*
*  gwa_trailer_arquivo-qtd_registros_arquivo = gva_qnt_arquivo + 1.
*  PERFORM f_add_zero_esquerda USING gwa_trailer_arquivo-qtd_registros_arquivo.
*
*  gwa_trailer_arquivo-reservado_1 = '0'.
*  PERFORM f_add_zero_esquerda USING gwa_trailer_arquivo-reservado_1.
*
*  PERFORM f_monta_espaco USING  gwa_trailer_arquivo-reservado_2.
*
*  APPEND gwa_trailer_arquivo  TO git_arquivo1.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_SALVA_ARQUIVO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_salva_arquivo .
*
*  DATA: lva_tabname(15),
*        lva_index(2),
*        lva_shortname TYPE rgsbl-title.
*
*  CLEAR: git_arquivo[].
*
*  FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.
*
*  DO 1 TIMES.
*    MOVE sy-index TO lva_index.
*    CONCATENATE 'git_arquivo' lva_index '[]' INTO lva_tabname.
*    ASSIGN (lva_tabname) TO <fs_tab>.
*    IF sy-subrc = 0.
*      IF NOT <fs_tab> IS INITIAL.
*        MOVE <fs_tab>  TO git_arquivo[].
*      ENDIF.
*    ENDIF.
*  ENDDO.
*
*  IF sy-batch IS NOT INITIAL.
*    DATA(_lines) = lines( git_arquivo ).
*    IF git_arquivo[] IS NOT INITIAL AND _lines GT 2.
*
*      DATA: lva_nome_arquivo TYPE string,
*            lva_arqs_proc    TYPE i.
*
*
*      CLEAR: git_locfile.
*      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*        EXPORTING
*          class         = '0000'
*          setnr         = 'MAGGI_AL5BANK'
*        TABLES
*          set_values    = git_locfile
*        EXCEPTIONS
*          set_not_found = 1
*          OTHERS        = 2.
*
*      CLEAR: gva_line ,
*             gva_line_aux,
*             lva_nome_arquivo.
*
*      READ TABLE  git_locfile WITH  KEY from = 'ENVIO'.
*      MOVE sy-tabix TO gva_line.
*      MOVE gva_line TO gva_line_aux.
*
*      PERFORM f_add_zero_esquerda USING gva_line_aux.
*
*      CLEAR: git_set_line_titles.
*      CALL FUNCTION 'G_SET_GET_TITLES'
*        EXPORTING
*          setclass        = '0000'
*          setname         = 'MAGGI_AL5BANK'
*          tabname         = rgsbs-table
*          language_vector = sy-langu
*        TABLES
*          set_line_titles = git_set_line_titles
*        EXCEPTIONS
*          OTHERS          = 0.
*
*      READ TABLE git_set_line_titles WITH KEY line = gva_line_aux.
*
*      lva_nome_arquivo = git_set_line_titles-title.
*
*      CONCATENATE  lva_nome_arquivo '/' 'AL5RECEB' git_bukrs sy-datum '.txt' INTO lva_nome_arquivo.
*      CONDENSE lva_nome_arquivo NO-GAPS.
*
*      " OPEN DATASET lva_nome_arquivo FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*
*      OPEN DATASET lva_nome_arquivo FOR OUTPUT IN TEXT MODE ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
*
*      IF sy-subrc <> 0.
*        MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!'
*        TYPE 'A'.
*      ENDIF.
*
*      CLEAR: gwa_arquivo.
*      LOOP AT git_arquivo INTO gwa_arquivo.
*        TRANSFER gwa_arquivo TO lva_nome_arquivo.
*      ENDLOOP.
*
*      CLOSE DATASET lva_nome_arquivo.
*    ENDIF.
*  ENDIF.
*
**  ELSE.
**
**    MOVE p_path TO lva_nome_arquivo.
**
**    DATA: lva_filename TYPE string.
**
**    CONCATENATE  lva_nome_arquivo '/' 'AL5RECEB' git_bukrs sy-datum '.txt' INTO lva_nome_arquivo.
**    CONDENSE lva_nome_arquivo NO-GAPS.
**
**    MOVE: lva_nome_arquivo TO lva_filename.
**
**
**    IF lva_filename EQ 'C:\' OR lva_filename EQ 'C:' OR lva_filename EQ 'C'.
**      MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!' TYPE 'E'.
**      EXIT.
**    ENDIF.
**
**    CALL FUNCTION 'GUI_DOWNLOAD'
**      EXPORTING
**        filename                  = lva_filename
**        filetype                  = 'ASC'
**        trunc_trailing_blanks     = 'X'
**        trunc_trailing_blanks_eol = 'X'
**      TABLES
**        data_tab                  = git_arquivo
**      EXCEPTIONS
**        file_write_error          = 1
**        no_batch                  = 2
**        gui_refuse_filetransfer   = 3
**        invalid_type              = 4
**        no_authority              = 5
**        unknown_error             = 6
**        header_not_allowed        = 7
**        separator_not_allowed     = 8
**        filesize_not_allowed      = 9
**        header_too_long           = 10
**        dp_error_create           = 11
**        dp_error_send             = 12
**        dp_error_write            = 13
**        unknown_dp_error          = 14
**        access_denied             = 15
**        dp_out_of_memory          = 16
**        disk_full                 = 17
**        dp_timeout                = 18
**        file_not_found            = 19
**        dataprovider_exception    = 20
**        control_flush_error       = 21
**        OTHERS                    = 22.
**
**  ENDIF.
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**&      Form  F_MONTA_ESPACO
**&---------------------------------------------------------------------*
*FORM f_monta_espaco USING p_campo TYPE any.
*
*  DATA: lva_campo_espc      TYPE string,            "Espaço convertido
*        lva_campo_qtde      TYPE i,                 "Quantidade caracteres
*        lva_qtde_vezes      TYPE i,                 "Quantidade espaços a serem inseridos
*        lva_campo_size(100) TYPE c.                 "Tamanho real do campo
*
*
*  "lva_campo_espc = cl_abap_conv_in_ce=>uccp( '00a0' ).
*  "lva_campo_espc = cl_abap_conv_in_ce=>uccp( '3000' ).
*  lva_campo_espc = space.
*
*  DESCRIBE FIELD p_campo LENGTH lva_campo_size IN CHARACTER MODE.
*  lva_campo_qtde = strlen( p_campo ).
*  lva_qtde_vezes = lva_campo_size  - lva_campo_qtde.
*
*  DO lva_qtde_vezes TIMES.
*    p_campo+lva_campo_qtde(1) = lva_campo_espc.
*    lva_campo_qtde = lva_campo_qtde + 1.
*  ENDDO.
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  F_OBTEM_PROXIMO_ARQUIVO
**&---------------------------------------------------------------------*
*FORM f_obtem_proximo_arquivo USING p_proximo_arquivo TYPE any.
*
*  DATA: lva_number TYPE i.
*
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      nr_range_nr             = '1'
*      object                  = 'ZSEQ_VERAG'
*    IMPORTING
*      number                  = lva_number
*    EXCEPTIONS
*      interval_not_found      = 1
*      number_range_not_intern = 2
*      object_not_found        = 3
*      quantity_is_0           = 4
*      quantity_is_not_1       = 5
*      interval_overflow       = 6
*      buffer_overflow         = 7
*      OTHERS                  = 8.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    p_proximo_arquivo = lva_number.
*  ENDIF.
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  F_ADD_ZERO_ESQUERDA
**&---------------------------------------------------------------------*
*FORM f_add_zero_esquerda  USING p_numero TYPE any.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = p_numero
*    IMPORTING
*      output = p_numero.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_FORMAT
**&---------------------------------------------------------------------*
*FORM f_format USING wl_dados TYPE any.
*
*  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN wl_dados WITH 'a' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN wl_dados WITH 'e' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        'í'     IN wl_dados WITH 'i' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN wl_dados WITH 'o' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN wl_dados WITH 'u' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN wl_dados WITH 'c' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '&'     IN wl_dados WITH 'e'.
*  REPLACE ALL OCCURRENCES OF        'º'     IN wl_dados WITH 'o' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '-'     IN wl_dados WITH | | IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '_'     IN wl_dados WITH | | IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '/'     IN wl_dados WITH | | IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '\'     IN wl_dados WITH | | IGNORING CASE.
*
*  TRANSLATE wl_dados TO UPPER CASE.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  F_BUSCA_ARQUIVO
**&---------------------------------------------------------------------*
*FORM f_busca_arquivo .
*
*  DATA: lit_dlist               LIKE epsfili OCCURS 0 WITH HEADER LINE,
*        lit_file                TYPE TABLE OF zarq_bloomberg WITH HEADER LINE,
*        lit_split               TYPE TABLE OF ty_split WITH HEADER LINE,
*        lwa_file                LIKE LINE OF lit_file,
*        lva_string              TYPE string,
*        lva_len                 TYPE i,
*        lva_fin                 TYPE string,
*        lva_c                   TYPE c,
*        lva_flag                TYPE c,
*        lva_count               TYPE i,
*        lva_dir_backup(60)      TYPE c,
*        lva_dir_retorno(60)     TYPE c,
*        lva_filename_string     TYPE rlgrap-filename,
*        lva_filename_string_bkp TYPE rlgrap-filename,
*        lva_nome_arquivo_rec    TYPE string,
*        lva_nome_arquivo_bkp    TYPE string,
*        lva_nome_arquivo        TYPE string.
*  "lva_dir_name        TYPE epsf-epsdirnam.
*
*
*  CLEAR: git_locfile.
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      class         = '0000'
*      setnr         = 'MAGGI_AL5BANK'
*    TABLES
*      set_values    = git_locfile
*    EXCEPTIONS
*      set_not_found = 1
*      OTHERS        = 2.
*
*  CLEAR: gva_line,
*         gva_line_aux.
*
*  READ TABLE  git_locfile WITH  KEY from = 'RETORNO'.
*  MOVE sy-tabix TO gva_line.
*
*  MOVE gva_line TO gva_line_aux.
*
*  PERFORM f_add_zero_esquerda USING gva_line_aux.
*
*  CLEAR: git_set_line_titles.
*  CALL FUNCTION 'G_SET_GET_TITLES'
*    EXPORTING
*      setclass        = '0000'
*      setname         = 'MAGGI_AL5BANK'
*      tabname         = rgsbs-table
*      language_vector = sy-langu
*    TABLES
*      set_line_titles = git_set_line_titles
*    EXCEPTIONS
*      OTHERS          = 0.
*
*  READ TABLE git_set_line_titles WITH KEY line = gva_line_aux.
*  lva_dir_retorno = git_set_line_titles-title.
*
*  CLEAR: gva_line, gva_line_aux.
*  READ TABLE  git_locfile WITH  KEY from = 'BACKUP'.
*  MOVE sy-tabix TO gva_line.
*  MOVE gva_line TO gva_line_aux.
*
*  PERFORM f_add_zero_esquerda USING gva_line_aux.
*
*  READ TABLE git_set_line_titles WITH KEY line = gva_line_aux.
*  lva_dir_backup = git_set_line_titles-title.
*
*  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
*    EXPORTING
*      dir_name               = lva_dir_retorno
*    TABLES
*      dir_list               = lit_dlist
*    EXCEPTIONS
*      invalid_eps_subdir     = 1
*      sapgparam_failed       = 2
*      build_directory_failed = 3
*      no_authorization       = 4
*      read_directory_failed  = 5
*      too_many_read_errors   = 6
*      empty_directory_list   = 7
*      OTHERS                 = 8.
*
*  IF sy-subrc = 0 .
*
*    LOOP AT lit_dlist.
*
*
*      CONCATENATE  lva_dir_retorno '/'  lit_dlist-name INTO lva_filename_string.
*      CONCATENATE  lva_dir_backup  '/'  lit_dlist-name INTO lva_filename_string_bkp.
*
*      MOVE lva_filename_string      TO lva_nome_arquivo.
*      MOVE lva_filename_string      TO lva_nome_arquivo_rec.
*      MOVE lva_filename_string_bkp  TO lva_nome_arquivo_bkp.
*
*      " Para na pegar possiveis enter/return WITH WINDOWS LINEFEED.
*      OPEN DATASET lva_nome_arquivo FOR INPUT IN TEXT MODE ENCODING DEFAULT. "IN TEXT MODE ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
*
*      CLEAR: lit_file[].
*      DO.
*        READ DATASET lva_nome_arquivo INTO lit_file.
*        IF sy-subrc  IS INITIAL.
*          APPEND lit_file.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDDO.
*
*      CLOSE DATASET lva_nome_arquivo.
*
*      gwa_zfit0170-nome_arq = lit_dlist-name.
*
*      LOOP AT lit_file INTO lwa_file .
*        IF sy-tabix = 1.
*          CONTINUE.
*        ELSE.
*          SPLIT lwa_file-linha AT ';' INTO TABLE lit_split.
*          CLEAR: gva_paval.
*          LOOP AT lit_split .
*            CASE sy-tabix.
*              WHEN 7.
*                gva_paval = lit_split+0(8).
*                PERFORM f_get_bukrs.
*                gwa_zfit0170-bukrs = gva_bukrs.
*              WHEN 14.
*                gwa_zfit0170-id_oper_al5 = lit_split.
*              WHEN 15.
*                CONCATENATE    lit_split+4(4) lit_split+2(2) lit_split+0(2)  INTO gwa_zfit0170-dt_oper_al5.
*              WHEN 24.
*                gwa_zfit0170-belnr = lit_split .
*            ENDCASE.
*          ENDLOOP.
*          gwa_zfit0170-status_ret = 'X'.
*          gwa_zfit0170-dt_ret =  sy-datum.
*          gwa_zfit0170-hr_ret = sy-uzeit.
*        ENDIF.
*
*        UPDATE zfit0170 SET
*            status_ret  = gwa_zfit0170-status_ret
*            dt_ret      = gwa_zfit0170-dt_ret
*            hr_ret      = gwa_zfit0170-hr_ret
*            id_oper_al5 = gwa_zfit0170-id_oper_al5
*            dt_oper_al5 = gwa_zfit0170-dt_oper_al5
*            nome_arq    = gwa_zfit0170-nome_arq
*        WHERE bukrs = gwa_zfit0170-bukrs
*        AND   belnr = gwa_zfit0170-belnr.
*        COMMIT WORK.
*
*        CLEAR gwa_zfit0170.
*
*      ENDLOOP.
*
*      "MOVER PARA OUTRA PASTA BKP
*      OPEN DATASET lva_nome_arquivo_bkp FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*      LOOP AT lit_file.
*        TRANSFER lit_file TO lva_nome_arquivo_bkp.
*      ENDLOOP.
*
*      CLOSE DATASET lva_nome_arquivo_bkp.
*
*      DELETE DATASET lva_nome_arquivo.
*
*
*    ENDLOOP.
*
*  ENDIF.
*
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  F_GET_BUKRS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_get_bukrs .
*
*  CLEAR: gva_bukrs.
*  SELECT SINGLE bukrs
*    FROM t001z
*  INTO gva_bukrs
*    WHERE party  = 'J_1BCG'
*      AND paval = gva_paval.
*
*ENDFORM.
**<<<------"184694 - NMS - FIM------>>>
*&---------------------------------------------------------------------*
*&      Form  F_ATU_DOC_FORNECEDOR
*&---------------------------------------------------------------------*
form f_atu_doc_fornecedor .
  clear: git_zfit0170.

  data: lva_kidno         type bseg-kidno,
        lva_data_venc(10),
        lva_error         type c,
        lva_gera_f51(1)   type c.

  data: lt_bkpf type table of bkpf,
        wa_bkpf type bkpf,
        wa_bseg type bseg,
        wa_bsik type bsik,
        lt_bseg type table of bseg,
        lt_bkdf type table of bkdf,
        lt_bsec type table of bsec,
        lt_bsed type table of bsed,
        lt_bset type table of bset.

  data: lit_compensa type table of setleaf with header line.

  select *
    from setleaf into table lit_compensa
   where setname = 'MAGGI_AL5_COMP'.

  read table lit_compensa index 1.
  lva_gera_f51 = lit_compensa-valfrom.

  select * into table git_zfit0170
  from zfit0170
    where augbl      = ''
    and   status_ret = 'X'.

  if git_zfit0170 is not initial.
    delete git_zfit0170 where id_created is initial.   "<<<------"184694 - NMS ------->>>

    loop at git_zfit0170 into gwa_zfit0170.

      clear: lva_kidno, lva_data_venc.

      concatenate 'AL5-OPER.' gwa_zfit0170-id_oper_al5 into lva_kidno.

      clear wa_bsik.
      select single *
        from bsik
        into wa_bsik
      where bukrs = gwa_zfit0170-bukrs
        and belnr = gwa_zfit0170-belnr
        and gjahr = gwa_zfit0170-gjahr
        and buzei = gwa_zfit0170-buzei.   "<<<------"184694 - NMS ------->>>

      if sy-subrc = 0.

        clear wa_bseg.
        refresh lt_bseg.
        data etl1280c8r6642 type table of bseg.
        data rldnr_l1280c8r9801 type rldnr.
* Buca o Ledger contábil.
        call function 'FAGL_GET_LEADING_LEDGER'
          importing
            e_rldnr       = rldnr_l1280c8r9801
          exceptions
            not_found     = 1
            more_than_one = 2.
        if sy-subrc = 0.
* Verifica se o Documento Contábil está na Tabela Diário Universal (ACDOCA).
          call function 'FAGL_GET_GL_DOCUMENT'
            exporting
              i_rldnr   = rldnr_l1280c8r9801
              i_bukrs   = wa_bsik-bukrs
              i_belnr   = wa_bsik-belnr
              i_gjahr   = wa_bsik-gjahr
              i_buzei   = wa_bsik-buzei
            importing
              et_bseg   = etl1280c8r6642
            exceptions
              not_found = 1.
        endif.
        if sy-subrc = 0 and lines( etl1280c8r6642 ) = 1.
          wa_bseg = etl1280c8r6642[ 1 ].
          sy-dbcnt = 1.
        else.
          sy-subrc = 4.
          sy-dbcnt = 0.
**<<<------"184694 - NMS - INI------>>>
          message |Atualiza. Doc. Contb. não existe na Tab. Diário Universal Reg.=> Empresa: { gwa_zfit0170-bukrs }, Doc.Cont: { gwa_zfit0170-belnr }, Ano: { gwa_zfit0170-gjahr }, Item: { gwa_zfit0170-buzei }| type 'S' display like 'E'.
          continue.
**<<<------"184694 - NMS - FIM------>>>
        endif.

        wa_bseg-zterm =  'Z002'.
        wa_bseg-zlspr =  '5'.
        wa_bseg-zlsch =  'E'.
        wa_bseg-kidno =  lva_kidno.
        append wa_bseg to lt_bseg.

        clear wa_bkpf.
        refresh lt_bkpf.
        select single *
          from bkpf
          into wa_bkpf
        where bukrs = wa_bseg-bukrs
        and   belnr = wa_bseg-belnr
        and   gjahr = wa_bseg-gjahr.
        append wa_bkpf to lt_bkpf.

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

          if lva_gera_f51 = 'X'.
            perform f_get_dados using gwa_zfit0170 .
**<<<------"184694 - NMS - INI------>>>
*            CHECK gwa_f51 IS NOT INITIAL.
            if gwa_f51 is initial.
              message |Atualiza. Erro ao preparar Doc. Contb. Reg.=> Empresa: { gwa_zfit0170-bukrs }, Doc.Cont: { gwa_zfit0170-belnr }, Ano: { gwa_zfit0170-gjahr }, Item: { gwa_zfit0170-buzei }| type 'S' display like 'E'.
              continue.

            endif.
**<<<------"184694 - NMS - FIM------>>>
            if 1 = 1.
              perform f_bapi_f51 using abap_false
                              changing gwa_f51
                                       lva_error.
            else.
              perform f_shdb_f51 using abap_false
                              changing gwa_f51
                                       lva_error.
            endif.
**<<<------"184694 - NMS - INI------>>>
            if not lva_error is initial.
              message |Atualiza. Erro ao Atualiza Doc. Contb. via F-51. Reg.=> Empresa: { gwa_zfit0170-bukrs }, Doc.Cont: { gwa_zfit0170-belnr }, Ano: { gwa_zfit0170-gjahr }, Item: { gwa_zfit0170-buzei }| type 'S' display like 'E'.
              continue.

            endif.
**<<<------"184694 - NMS - FIM------>>>
          endif.
        endif.
      endif.
      clear: gwa_zfit0170.
    endloop.

  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
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

  constants: c_msgid like git_msg-msgid value 'F5',
             c_msgnr like git_msg-msgnr value '312',
             c_msgne like git_msg-msgnr value '539'.

  refresh git_msg.
  clear:  gva_augbl.
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

form f_bapi_f51 using p_residual_comp  type c  "Opção para deixar residual na Compensação
               changing p_f51          type ty_f51
                        p_erro.


  data: l_auglv   type t041a-auglv   value 'UMBUCHNG', "Posting with Clearing
        l_tcode   type sy-tcode      value 'FB05',     "You get an error with any other value
        l_sgfunct type rfipi-sgfunct value 'C'.        "Post immediately

  data: lt_blntab  type standard table of blntab  with header line,
        lt_ftclear type standard table of ftclear with header line,
        lt_ftpost  type standard table of ftpost  with header line,
        lt_fttax   type standard table of fttax   with header line,
        lds_return type bapiret2.

  data: vdata(10),
        lva_data(10),
        lva_bldat(10),
        lva_kidno         type bseg-kidno,
        lva_data_venc(10),
        lva_dt_mov        type sy-datum,
        vdata_venc(10),
        lva_sgtxt         type bsik-sgtxt,
        cnum_seq(2),
        wl_taxa(16),
        vcampo(15),
        v_kur             type bkpf-kursf,
        vvalor_bax        type zfit0042-dmbe2,
        msg_no            type t100-msgnr,
        msg_text          type string,
        p_mode            like rfpdo-allgazmd,
        vl_dt_mov         type sy-datum,
        count_ft          type ftpost-count,
        v_xsimu           type char1,
        vhbkid            type bseg-hbkid.

  data: lt_bkdf type table of bkdf,
        lt_bkpf type table of bkpf,
        lt_bsed type table of bsed,
        lt_bseg type table of bseg,
        lt_bsec type table of bsec,
        lt_bset type table of bset,

        wa_bseg type bseg,
        wa_bkpf type bkpf.

  data: lva_vlrn      type p decimals 2,
        lva_vlrn2     type p decimals 2,
        lva_vlrc(16),
        lva_vlrc2(16).

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
  vhbkid = zcl_miro=>get_banco_forma_pagamento( i_bukrs = p_f51-bukrs i_forma_pagamento  = 'S' ).


  lva_dt_mov = sy-datum.
  concatenate  lva_dt_mov+6(2) lva_dt_mov+4(2) lva_dt_mov(4) into lva_data separated by '.'.
  concatenate  p_f51-bldat+6(2)  p_f51-bldat+4(2)  p_f51-bldat(4) into lva_bldat separated by '.'.
  concatenate  p_f51-lifnr '-' p_f51-name1 into lva_sgtxt separated by space.
  concatenate  p_f51-zfbdt+6(2) p_f51-zfbdt+4(2) p_f51-zfbdt(4) into lva_data_venc separated by '.'.
  concatenate 'AL5-OPER' p_f51-id_oper_al5 into lva_kidno.


  lva_vlrn = conv #( p_f51-dmbtr ).
  write: lva_vlrn to lva_vlrc.


  lva_vlrn2 = conv #( p_f51-dmbe2 ).
  write: lva_vlrn2 to lva_vlrc2.



  p_mode = 'N'.

  call function 'POSTING_INTERFACE_START'
    exporting
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    exceptions
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      others             = 6.

  if sy-subrc ne 0.
    p_erro = 'X'.
    rollback work.
    message 'Houve ao efeturar a compensação' type 'S'.
    return.
  endif.


  clear: lt_blntab,   lt_blntab[],
        lt_ftclear,  lt_ftclear[],
        lt_ftpost,   lt_ftpost[],
        lt_fttax,    lt_fttax[],
        lds_return, p_erro.

  count_ft = 1.

  lt_ftpost-stype = 'K'."Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = p_f51-bukrs.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  lt_ftpost-fval = p_f51-waers.
  append lt_ftpost.

*  lt_ftpost-fnam = 'BKPF-KURSF'.
*  lt_ftpost-fval = wl_taxa.
*  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval = lva_bldat.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = lva_data.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  lva_dt_mov+4(2).
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  lt_ftpost-fval = 'AB'.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-XBLNR'.
  lt_ftpost-fval = p_f51-xblnr.
  append lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BKTXT'.
  lt_ftpost-fval = p_f51-belnr.
  append lt_ftpost.


  add 1 to count_ft.
  lt_ftpost-stype = 'P'.
  lt_ftpost-count = count_ft .

  lt_ftpost-fnam = 'RF05A-NEWBS'.
  lt_ftpost-fval =  '39'.
  append lt_ftpost.

  lt_ftpost-fnam = 'RF05A-NEWUM'.
  lt_ftpost-fval = '"'.
  append lt_ftpost.

  lt_ftpost-fnam = 'RF05A-NEWKO'.
  lt_ftpost-fval = git_forn-from.
  append lt_ftpost.


  lt_ftpost-fnam = 'BSEG-WRBTR'.
  lt_ftpost-fval =  lva_vlrc.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-DMBE2'.
  lt_ftpost-fval = lva_vlrc2.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-ZFBDT'.
  lt_ftpost-fval = lva_data_venc.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-KIDNO'.
  lt_ftpost-fval =  lva_kidno.
  append lt_ftpost.


  lt_ftpost-fnam = 'BSEG-GSBER'.
  lt_ftpost-fval =  p_f51-gsber.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-ZLSCH'.
  lt_ftpost-fval = 'S'.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-ZUONR'.
  lt_ftpost-fval =  p_f51-zuonr.
  append lt_ftpost.


  lt_ftpost-fnam = 'BSEG-SGTXT'.
  lt_ftpost-fval = lva_sgtxt.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-BVTYP'.
  lt_ftpost-fval = git_forn-title.
  append lt_ftpost.

  lt_ftpost-fnam = 'BSEG-HBKID'.
  lt_ftpost-fval = vhbkid.
  append lt_ftpost.

  lt_ftclear-agkoa  = 'K'.
  lt_ftclear-agkon  = p_f51-lifnr.
  lt_ftclear-agums  = ''.
  lt_ftclear-agbuk  = p_f51-bukrs.
  lt_ftclear-xnops  = 'X'.
  lt_ftclear-selfd  = 'BELNR'.
  concatenate  p_f51-belnr p_f51-gjahr p_f51-buzei into lt_ftclear-selvon.
  append lt_ftclear.


  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
      i_xsimu                    = v_xsimu
    importing
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    tables
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    exceptions
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      others                     = 10.


  if lt_blntab[] is initial.
    p_erro = 'X'.
    write lds_return-number to msg_no.
    call function 'MESSAGE_PREPARE'
      exporting
        msg_id                 = lds_return-id
        msg_no                 = msg_no
        msg_var1               = lds_return-message_v1
        msg_var2               = lds_return-message_v2
        msg_var3               = lds_return-message_v3
        msg_var4               = lds_return-message_v4
      importing
        msg_text               = msg_text
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.
    message msg_text type 'S'.
  else.
    read table lt_blntab index 1.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = lt_blntab-belnr
      importing
        output = gva_augbl.

    update zfit0170 set augbl = gva_augbl
      where bukrs = p_f51-bukrs
      and   belnr = p_f51-belnr    "<<<------"184694 - NMS ------->>>
      and   buzei = p_f51-buzei    "<<<------"184694 - NMS ------->>>
      and   gjahr = p_f51-gjahr.   "<<<------"184694 - NMS ------->>>
    commit work.

    "DEsbloqueio pagto
    clear wa_bkpf.
    refresh lt_bkpf.
    select single *
      from bkpf
      into wa_bkpf
    where bukrs = p_f51-bukrs
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

  "fim
  call function 'POSTING_INTERFACE_END'
    exporting
      i_bdcimmed              = 'X'
    exceptions
      session_not_processable = 1
      others                  = 2.

  if sy-subrc <> 0.
    exit.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_SHDB_F51
*&---------------------------------------------------------------------*
form f_shdb_f51 using p_residual_comp  type c  "Opção para deixar residual na Compensação
             changing p_f51     type ty_f51
                      p_erro.

  data: lva_data(10),
        lva_bldat(10),
        lva_kidno         type bseg-kidno,
        lva_data_venc(10),
        lva_dt_mov        type sy-datum,
        lva_sgtxt         type bsik-sgtxt,
        lva_vlrc(16),
        lva_vlrc2(16),
        vhbkid            type bseg-hbkid.

  data: lt_bkdf type table of bkdf,
        lt_bkpf type table of bkpf,
        lt_bsed type table of bsed,
        lt_bseg type table of bseg,
        lt_bsec type table of bsec,
        lt_bset type table of bset,

        wa_bseg type bseg,
        wa_bkpf type bkpf.

  data: lva_vlrn  type p decimals 2,
        lva_vlrn2 type p decimals 2.

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
  vhbkid = zcl_miro=>get_banco_forma_pagamento( i_bukrs = p_f51-bukrs i_forma_pagamento  = 'S' ).

  lva_dt_mov = sy-datum.
  concatenate  lva_dt_mov+6(2) lva_dt_mov+4(2) lva_dt_mov(4) into lva_data separated by '.'.
  concatenate  p_f51-bldat+6(2)  p_f51-bldat+4(2)  p_f51-bldat(4) into lva_bldat separated by '.'.
  concatenate  p_f51-lifnr '-' p_f51-name1 into lva_sgtxt separated by space.
  concatenate  p_f51-zfbdt+6(2) p_f51-zfbdt+4(2) p_f51-zfbdt(4) into lva_data_venc separated by '.'.
  concatenate 'AL5-OPER' p_f51-id_oper_al5 into lva_kidno.

*---> 09/06/2023 - Migração S4 - JS
*             lva_vlrn = p_f51-dmbtr.
  lva_vlrn = conv #( p_f51-dmbtr ).
*<--- 09/06/2023 - Migração S4 - JS

  write: lva_vlrn to lva_vlrc.

*---> 09/06/2023 - Migração S4 - JS
*            lva_vlrn2 = p_f51-dmbe2.
  lva_vlrn2 = conv #( p_f51-dmbe2 ).
*<--- 09/06/2023 - Migração S4 - JS
  write: lva_vlrn2 to lva_vlrc2.

  perform f_bdc_data using:
    'SAPMF05A'  '0122'  'X'  ''            '',
    ''          ''      ''   'BDC_CURSOR'  'RF05A-NEWKO',
    ''          ''      ''   'BDC_OKCODE'  '/00',
    ''          ''      ''   'BKPF-BLDAT'  lva_bldat,    "(BSIK-BLDAT)
    ''          ''      ''   'BKPF-BLART'  'AB',  "(BSIK-BLART)
    ''          ''      ''   'BKPF-BUKRS'  p_f51-bukrs,  "(BSIK-BUKRS)
    ''          ''      ''   'BKPF-BUDAT'  lva_data,       "(Data do Sistema)
    ''          ''      ''   'BKPF-MONAT'  lva_dt_mov+4(2),"(Mês da Data do Ssitema)
    ''          ''      ''   'BKPF-WAERS'  p_f51-waers,  "(BSIK-WAERS)
    ''          ''      ''   'BKPF-XBLNR'  p_f51-xblnr,  "(BSIK-XBLNR)
    ''          ''      ''   'BKPF-BKTXT'  p_f51-belnr,  "5189327632 (ZFIT0170-BELNR)
    ''          ''      ''   'RF05A-NEWBS' '39',            "US161330
    ''          ''      ''   'RF05A-NEWKO' git_forn-from, "'145241',       "(SET MAGGI_CODFORAL5) BUG - 78952 - CBRAND
    ''          ''      ''   'RF05A-NEWUM' '"',             "US161330

    'SAPMF05A' '0304'  'X'   ''            '',
    ''          ''      ''   'BDC_CURSOR'  'BSEG-ZFBDT', "p_f51-zuonr,  "(bsik-zuonr)
    ''          ''      ''   'BDC_OKCODE'  '=ZK',
    ''          ''      ''   'BSEG-WRBTR'  lva_vlrc,   "(bsik-dmbtr)
    ''          ''      ''   'BSEG-GSBER'  p_f51-gsber,  "(bsik-gsber)
*    ''          ''      ''   'BSEG-ZTERM'  '0004',
    ''          ''      ''   'BSEG-ZFBDT'  lva_data_venc, " (bsik-zbd1t+ bsik-zbfbdt)
    ''          ''      ''   'BSEG-ZLSCH'  'S',
    ''          ''      ''   'BSEG-KIDNO'  lva_kidno,  "OPER.1234566789012345 (OPER. ZFIT0170-ID_OPER_AL5)
    ''          ''      ''   'BSEG-ZUONR'  p_f51-zuonr,        "4500919537 (BSIK-ZUONR)
    ''          ''      ''   'BSEG-SGTXT'  lva_sgtxt, "100924-CARGOCENTER AGENCIA DE CARGAS (BSIK-LIFNR – LFA1-NAME1)
*
    'SAPMF05A'  '0332'  'X'   ''            '',
    ''          ''      ''    'BDC_CURSOR'  'BSEG-DMBE2',
    ''          ''      ''    'BDC_OKCODE'  '=SL',
    ''          ''      ''    'BSEG-DMBE2'  lva_vlrc2, "154,70 (BSIK-DMBE2)
    ''          ''      ''    'BSEG-BVTYP'  git_forn-title, "'0001', BUG - 78952 - CBRAND
    ''          ''      ''    'BSEG-HBKID'  vhbkid, " 'ITAU2', BUG - 78952 - CBRAND

    'SAPMF05A'  '0710'  'X'   ''                  '',
    ''          ''      ''    'BDC_CURSOR'        'RF05A-XPOS1(03)',
    ''          ''      ''    'BDC_OKCODE'        '/00',
    ''          ''      ''    'RF05A-AGBUK'       p_f51-bukrs,  "0001 (BSIK-BUKRS)
    ''          ''      ''    'RF05A-AGKON'       p_f51-lifnr,  "100924 (BSIK-LIFNR)
    ''          ''      ''    'RF05A-AGKOA'       'K',
    ''          ''      ''    'RF05A-XNOPS'       'X',
    ''          ''      ''    'RF05A-XPOS1(01)'   ' ',
    ''          ''      ''    'RF05A-XPOS1(03)'   'X',
*
    'SAPMF05A'  '0731'  'X'   ''           '',
    ''          ''      ''   'BDC_CURSOR'  'RF05A-SEL01(01)',
    ''          ''      ''   'BDC_OKCODE'  '=PA',
    ''          ''      ''   'RF05A-SEL01(01)' p_f51-belnr, "  5103922092 (ZFIT0170-BELNR)


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

    update zfit0170 set augbl = gva_augbl
      where bukrs = p_f51-bukrs
      and   belnr = p_f51-belnr    "<<<------"184694 - NMS ------->>>
      and   buzei = p_f51-buzei    "<<<------"184694 - NMS ------->>>
      and   gjahr = p_f51-gjahr.   "<<<------"184694 - NMS ------->>>
    commit work.

    "DEsbloqueio pagto
    clear wa_bkpf.
    refresh lt_bkpf.
    select single *
      from bkpf
      into wa_bkpf
    where bukrs = p_f51-bukrs
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
*&      Form  F_GET_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_dados using gwa_zfit0170  type zfit0170 .

  clear: git_f51,
         gwa_f51,
         git_bsik_aux,
         git_lfa1,
         gwa_bsik_aux,
         gwa_lfa1.


  select *
  into table git_bsik_aux
    from bsik
      where bukrs eq gwa_zfit0170-bukrs
        and gjahr eq gwa_zfit0170-gjahr
        and belnr eq gwa_zfit0170-belnr    "<<<------"184694 - NMS ------->>>
        and buzei eq gwa_zfit0170-buzei.   "<<<------"184694 - NMS ------->>>

  if git_bsik_aux is not initial.

    select *
      into table git_lfa1
       from lfa1
          for all entries in git_bsik_aux
          where lifnr eq  git_bsik_aux-lifnr.


    read table git_bsik_aux into gwa_bsik_aux with key  bukrs = gwa_zfit0170-bukrs
                                                        gjahr = gwa_zfit0170-gjahr
                                                        belnr = gwa_zfit0170-belnr    "<<<------"184694 - NMS ------->>>
                                                        buzei = gwa_zfit0170-buzei.   "<<<------"184694 - NMS ------->>>
    gwa_f51-bukrs       = gwa_bsik_aux-bukrs.
    gwa_f51-ebeln       = gwa_bsik_aux-ebeln.
    gwa_f51-belnr       = gwa_zfit0170-belnr.
    gwa_f51-xblnr       = gwa_bsik_aux-xblnr.
    gwa_f51-budat       = gwa_bsik_aux-budat.
    gwa_f51-zfbdt       = gwa_bsik_aux-zbd1t + gwa_bsik_aux-zfbdt.
    gwa_f51-dmbtr       = gwa_bsik_aux-dmbtr.
    gwa_f51-dmbe2       = gwa_bsik_aux-dmbe2.
    gwa_f51-id_oper_al5 = gwa_zfit0170-id_oper_al5.
    gwa_f51-dt_oper_al5 = gwa_zfit0170-dt_oper_al5.
    gwa_f51-augbl       = gwa_zfit0170-augbl .
    gwa_f51-bldat       = gwa_bsik_aux-bldat.
    gwa_f51-gsber       = gwa_bsik_aux-gsber.
    gwa_f51-waers       = gwa_bsik_aux-waers.
    gwa_f51-zuonr       = gwa_bsik_aux-zuonr.
    gwa_f51-gjahr       = gwa_bsik_aux-gjahr.
    gwa_f51-buzei       = gwa_bsik_aux-buzei.
    gwa_f51-blart       = gwa_bsik_aux-blart.

    read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_bsik_aux-lifnr.

    gwa_f51-lifnr = gwa_lfa1-lifnr.
    gwa_f51-name1 = gwa_lfa1-name1.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form F_SELECIONA_COMPENS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_seleciona_compens .

**<<<------"184694 - NMS - INI------>>>
*  DATA: lv_dt   TYPE datum,
*        lt_bkpf TYPE TABLE OF ty_bkpf.
**<<<------"184694 - NMS - FIM------>>>
  select *
    from zfit0170
    into table t_zfit0170_compens
    where id_oper_al5 = space "Ja negociado nao vai
    and   tipo_mvto   in ( '0', '5' )
    and   not exists ( select * from zfit0170 as a
                     where a~bukrs = zfit0170~bukrs
                     and   a~belnr = zfit0170~belnr
                     and   a~buzei = zfit0170~buzei     "<<<------"184694 - NMS ------->>>
                     and   a~gjahr = zfit0170~gjahr
                     and   a~tipo_mvto = '9').
  if sy-subrc is initial.
**<<<------"184694 - NMS - INI------>>>
* Elimina seleção de registros antigos.
    delete t_zfit0170_compens where buzei is initial
                                 or buzei eq '000'.
**<<<------"184694 - NMS - FIM------>>>
    data(lt_zfit0170_aux) = t_zfit0170_compens.
    delete lt_zfit0170_aux where tipo_mvto <> 9.
    sort lt_zfit0170_aux by bukrs belnr buzei gjahr.    "<<<------"184694 - NMS ------->>>

    loop at t_zfit0170_compens assigning field-symbol(<fs_zfit0170_compens>).
      data(lv_tabix) = sy-tabix.
      read table lt_zfit0170_aux transporting no fields
      with key bukrs = <fs_zfit0170_compens>-bukrs
               belnr = <fs_zfit0170_compens>-belnr
               buzei = <fs_zfit0170_compens>-buzei      "<<<------"184694 - NMS ------->>>
               gjahr = <fs_zfit0170_compens>-gjahr
      binary search.
      if sy-subrc is initial.
        delete t_zfit0170_compens index lv_tabix.
      endif.
    endloop.
  endif.

  check t_zfit0170_compens is not initial.

  sort t_zfit0170_compens by bukrs belnr buzei gjahr.   "<<<------"184694 - NMS ------->>>

  lt_zfit0170_aux = t_zfit0170_compens.
  sort lt_zfit0170_aux by bukrs belnr buzei gjahr.      "<<<------"184694 - NMS ------->>>
  delete adjacent duplicates from lt_zfit0170_aux comparing bukrs belnr buzei gjahr.   "<<<------"184694 - NMS ------->>>

  select *
    from bsak
    into table t_bsak_compens
    for all entries in lt_zfit0170_aux
    where bukrs = lt_zfit0170_aux-bukrs
      and belnr = lt_zfit0170_aux-belnr
      and buzei = lt_zfit0170_aux-buzei   "<<<------"184694 - NMS ------->>>
      and gjahr = lt_zfit0170_aux-gjahr
      and not exists ( select *
                        from bsik
                        where bsik~bukrs = bsak~bukrs
                        and   bsik~belnr = bsak~belnr ).

  if sy-subrc is initial.
**<<<------"184694 - NMS - INI------>>>
*    DATA(lt_bsak_aux) = t_bsak_compens.
*    SORT lt_bsak_aux BY lifnr.
*    DELETE ADJACENT DUPLICATES FROM lt_bsak_aux COMPARING lifnr.
*
*    SELECT *
*      FROM lfa1
*      INTO TABLE t_lfa1_compens
*      FOR ALL ENTRIES IN lt_bsak_aux
*      WHERE lifnr = lt_bsak_aux-lifnr.
*    IF sy-subrc IS INITIAL.
*      SORT t_lfa1_compens BY lifnr.
*    ENDIF.
*
*    lt_bsak_aux = t_bsak_compens.
*    SORT lt_bsak_aux BY bukrs belnr gjahr.
*    DELETE ADJACENT DUPLICATES FROM lt_bsak_aux COMPARING bukrs belnr gjahr.
*
*    SELECT bukrs belnr gjahr awkey
*      FROM bkpf
*      INTO TABLE t_bkpf_compens
*      FOR ALL ENTRIES IN lt_bsak_aux
*      WHERE bukrs = lt_bsak_aux-bukrs
*        AND belnr = lt_bsak_aux-belnr
*        AND gjahr = lt_bsak_aux-gjahr.
*    IF sy-subrc IS INITIAL.
*      SORT t_bkpf_compens BY bukrs belnr gjahr.
*
*      DATA(lt_bkpf_aux) = t_bkpf_compens.
*      SORT lt_bkpf_aux BY bukrs.
*      DELETE ADJACENT DUPLICATES FROM lt_bkpf_aux COMPARING bukrs.
*
*      APPEND LINES OF lt_bkpf_aux TO git_bukrs[].
*
*      LOOP AT lt_bkpf_aux ASSIGNING FIELD-SYMBOL(<fs_bkpf>).
*        CONCATENATE <fs_bkpf>-bukrs+2(2) '01' INTO gva_lifnr.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = gva_lifnr
*          IMPORTING
*            output = gva_lifnr.
*
*        SELECT *
*           APPENDING TABLE git_lfa1_pag
*           FROM lfa1
*           WHERE lifnr EQ  gva_lifnr.
*        CLEAR  gva_lifnr.
*
*      ENDLOOP.
*
*      LOOP AT t_bkpf_compens ASSIGNING <fs_bkpf>.
*        <fs_bkpf>-refkey = <fs_bkpf>-awkey.
*      ENDLOOP.
*
*      lt_bkpf_aux = t_bkpf_compens.
*      SORT lt_bkpf_aux BY refkey.
*      DELETE ADJACENT DUPLICATES FROM lt_bkpf_aux COMPARING refkey.
*
*      SELECT *
*        FROM j_1bnflin
*        INTO TABLE t_j_1bnflin_compens
*        FOR ALL ENTRIES IN lt_bkpf_aux
*        WHERE refkey = lt_bkpf_aux-refkey.
*      IF sy-subrc IS INITIAL.
*        SORT t_j_1bnflin_compens BY refkey.
*
*        DATA(lt_j_1bnflin_aux) = t_j_1bnflin_compens.
*        SORT lt_j_1bnflin_aux BY docnum.
*        DELETE ADJACENT DUPLICATES FROM lt_j_1bnflin_aux COMPARING docnum.
*
*        SELECT *
*          FROM j_1bnfe_active
*          INTO TABLE t_j_1bnfe_compens
*          FOR ALL ENTRIES IN lt_j_1bnflin_aux
*          WHERE docnum = lt_j_1bnflin_aux-docnum.
*        IF sy-subrc IS INITIAL.
*          SORT t_j_1bnfe_compens BY docnum.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
    data(tl_bsak_aux) = t_bsak_compens.
    sort tl_bsak_aux by bukrs belnr gjahr.
    delete adjacent duplicates from tl_bsak_aux comparing bukrs belnr gjahr.

    select distinct bukrs from bkpf
      appending table git_bukrs
      for all entries in tl_bsak_aux
    where bukrs eq tl_bsak_aux-bukrs
      and belnr eq tl_bsak_aux-belnr
      and gjahr eq tl_bsak_aux-gjahr.
**<<<------"184694 - NMS - FIM------>>>
  endif.

endform.

*&---------------------------------------------------------------------*
*& Form F_SELECIONA_VENCTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_seleciona_vencto.

**<<<------"184694 - NMS - INI------>>>
*  DATA: lv_dt   TYPE datum,
*        lt_bkpf TYPE TABLE OF ty_bkpf.
  data: lv_dt type datum.
**<<<------"184694 - NMS - FIM------>>>
  select *
    from zfit0170
    into table t_zfit0170_vencto
    where id_oper_al5 = space
      and zfbdt <> lv_dt
      and   tipo_mvto   in ( '0', '5' )
      and   not exists ( select * from zfit0170 as a
                     where a~bukrs = zfit0170~bukrs
                     and   a~belnr = zfit0170~belnr
                     and   a~buzei = zfit0170~buzei   "<<<------"184694 - NMS ------->>>
                     and   a~gjahr = zfit0170~gjahr
                     and   a~tipo_mvto = '9').
  if sy-subrc is initial.
**<<<------"184694 - NMS - INI------>>>
* Elimina seleção de registros antigos.
    delete t_zfit0170_vencto where buzei is initial
                                or buzei eq '000'.
**<<<------"184694 - NMS - FIM------>>>
    data(lt_zfit0170_aux) = t_zfit0170_vencto.
    delete lt_zfit0170_aux where tipo_mvto <> 9.
    sort lt_zfit0170_aux by bukrs belnr buzei gjahr.   "<<<------"184694 - NMS ------->>>

    loop at t_zfit0170_vencto assigning field-symbol(<fs_zfit0170_vencto>).
      data(lv_tabix) = sy-tabix.

      read table lt_zfit0170_aux transporting no fields
      with key bukrs = <fs_zfit0170_vencto>-bukrs
               belnr = <fs_zfit0170_vencto>-belnr
               buzei = <fs_zfit0170_vencto>-buzei   "<<<------"184694 - NMS ------->>>
               gjahr = <fs_zfit0170_vencto>-gjahr
      binary search.
      if sy-subrc is initial.
        delete t_zfit0170_vencto index lv_tabix.
      endif.
      "compensados
      read table t_bsak_compens transporting no fields
       with key bukrs = <fs_zfit0170_vencto>-bukrs
                belnr = <fs_zfit0170_vencto>-belnr
                buzei = <fs_zfit0170_vencto>-buzei   "<<<------"184694 - NMS ------->>>
                gjahr = <fs_zfit0170_vencto>-gjahr
     binary search.
      if sy-subrc is initial.
        delete t_zfit0170_vencto index lv_tabix.
      endif.
    endloop.

  endif.

  check t_zfit0170_vencto is not initial.

  lt_zfit0170_aux = t_zfit0170_vencto.
  sort lt_zfit0170_aux by bukrs belnr gjahr.
  delete adjacent duplicates from lt_zfit0170_aux comparing bukrs belnr gjahr.

  select *
    from bsik
    into table t_bsik_vencto
    for all entries in lt_zfit0170_aux
    where bukrs = lt_zfit0170_aux-bukrs
      and belnr = lt_zfit0170_aux-belnr
      and buzei = lt_zfit0170_aux-buzei   "<<<------"184694 - NMS ------->>>
      and gjahr = lt_zfit0170_aux-gjahr.

  if sy-subrc is initial.
**<<<------"184694 - NMS - INI------>>>
*    DATA(lt_bsik_aux) = t_bsik_vencto.
*    SORT lt_bsik_aux BY lifnr.
*    DELETE ADJACENT DUPLICATES FROM lt_bsik_aux COMPARING lifnr.
*
*    SELECT *
*      FROM lfa1
*      INTO TABLE t_lfa1_vencto
*      FOR ALL ENTRIES IN lt_bsik_aux
*      WHERE lifnr = lt_bsik_aux-lifnr.
*    IF sy-subrc IS INITIAL.
*      SORT t_lfa1_vencto BY lifnr.
*    ENDIF.
*
*    lt_bsik_aux = t_bsik_vencto.
*    SORT lt_bsik_aux BY bukrs belnr gjahr.
*    DELETE ADJACENT DUPLICATES FROM lt_bsik_aux COMPARING bukrs belnr gjahr.
*
*    SELECT bukrs belnr gjahr awkey
*      FROM bkpf
*      INTO TABLE t_bkpf_vencto
*      FOR ALL ENTRIES IN lt_bsik_aux
*      WHERE bukrs = lt_bsik_aux-bukrs
*        AND belnr = lt_bsik_aux-belnr
*        AND gjahr = lt_bsik_aux-gjahr.
*    IF sy-subrc IS INITIAL.
*      SORT t_bkpf_vencto BY bukrs belnr gjahr.
*
*      DATA(lt_bkpf_aux) = t_bkpf_vencto.
*      SORT lt_bkpf_aux BY bukrs.
*      DELETE ADJACENT DUPLICATES FROM lt_bkpf_aux COMPARING bukrs.
*
*      APPEND LINES OF lt_bkpf_aux TO git_bukrs[].
*
*      LOOP AT lt_bkpf_aux ASSIGNING FIELD-SYMBOL(<fs_bkpf>).
*        CONCATENATE <fs_bkpf>-bukrs+2(2) '01' INTO gva_lifnr.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = gva_lifnr
*          IMPORTING
*            output = gva_lifnr.
*
*        SELECT *
*           APPENDING TABLE git_lfa1_pag
*           FROM lfa1
*           WHERE lifnr EQ  gva_lifnr.
*        CLEAR  gva_lifnr.
*
*      ENDLOOP.
*
*      LOOP AT t_bkpf_vencto ASSIGNING <fs_bkpf>.
*        <fs_bkpf>-refkey = <fs_bkpf>-awkey.
*      ENDLOOP.
*
*      lt_bkpf_aux = t_bkpf_vencto.
*      SORT lt_bkpf_aux BY refkey.
*      DELETE ADJACENT DUPLICATES FROM lt_bkpf_aux COMPARING refkey.
*
*      SELECT *
*        FROM j_1bnflin
*        INTO TABLE t_j_1bnflin_vencto
*        FOR ALL ENTRIES IN lt_bkpf_aux
*        WHERE refkey = lt_bkpf_aux-refkey.
*      IF sy-subrc IS INITIAL.
*        SORT t_j_1bnflin_vencto BY refkey.
*
*        DATA(lt_j_1bnflin_aux) = t_j_1bnflin_vencto.
*        SORT lt_j_1bnflin_aux BY docnum.
*        DELETE ADJACENT DUPLICATES FROM lt_j_1bnflin_aux COMPARING docnum.
*
*        SELECT *
*          FROM j_1bnfe_active
*          INTO TABLE t_j_1bnfe_vencto
*          FOR ALL ENTRIES IN lt_j_1bnflin_aux
*          WHERE docnum = lt_j_1bnflin_aux-docnum.
*        IF sy-subrc IS INITIAL.
*          SORT t_j_1bnfe_vencto BY docnum.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
    data(tl_bsik_aux) = t_bsik_vencto.
    sort tl_bsik_aux by bukrs belnr gjahr buzei.
    delete adjacent duplicates from tl_bsik_aux comparing bukrs belnr gjahr.

    select distinct bukrs from bkpf
      appending table git_bukrs
      for all entries in tl_bsik_aux
    where bukrs eq tl_bsik_aux-bukrs
      and belnr eq tl_bsik_aux-belnr
      and gjahr eq tl_bsik_aux-gjahr.
**<<<------"184694 - NMS - FIM------>>>
  endif.

endform.
**<<<------"184694 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_monta_dados
*&---------------------------------------------------------------------*
*& Monta dados para chamada da API de comunicação
*&---------------------------------------------------------------------*
form zf_monta_dados.

  sort git_bukrs.
  delete adjacent duplicates from git_bukrs.

  loop at git_bukrs.
    concatenate git_bukrs+2(2) '01' into gva_lifnr.
    gva_lifnr = |{ gva_lifnr alpha = in width = 10 }|.

    perform: zf_cria_movimento, "Criar movimento - API Finanfor
             zf_aprv_repr_movi, "Aprova/Reprova movimento - API Finanfor
             zf_atua_repr_movi. "Atualiza/Reprova movimento - API Finanfor

  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form zf_cria_movimento
*&---------------------------------------------------------------------*
*& Criar movimento - API Finanfor
*&---------------------------------------------------------------------*
form zf_cria_movimento.

  data: el_mov_criar   type zfie_finanfor_mov_criar,
        el_mov_get_par type zfie_finanfor_mov_get_param,
        el_mov_get     type zfie_finanfor_mov_get.

  data: vl_external_id type char21,
        vl_qt_criada   type i.

  constants: cl_asset    type string value 'asset',
             cl_invoices type string value 'invoices',
             cl_fatura   type string value 'Fatura',
             cl_seller   type string value 'seller',
             cl_buyer    type string value 'buyer',
             cl_dispon   type string value 'Disponível',
             cl_created  type string value 'created'.

  if git_bsik[] is not initial.
    select distinct bsik~bukrs,
                    bsik~belnr,
                    bsak~xblnr
          from bsik
          inner join bsak
          on  bsak~bukrs eq bsik~bukrs
          and bsak~augbl eq bsik~belnr
          and bsak~gjahr eq bsik~gjahr
          and bsak~belnr ne bsik~belnr
          into table @data(git_bsak_nota)
          for all entries in @git_bsik
          where bsik~belnr eq @git_bsik-belnr
          and   bsik~bukrs eq @git_bsik-bukrs.
    sort git_bsak_nota by bukrs belnr.
  endif.

  loop at git_bsik into gwa_bsik where bukrs = git_bukrs-bukrs.
*** TYPE
    el_mov_criar-type-id            = cl_asset.
    el_mov_criar-type-sub_type      = cl_invoices.
    el_mov_criar-type-title         = cl_fatura.
    el_mov_criar-type-external_type = cl_fatura.
*** FROM
    clear gwa_lfa1.
* Dados do Forncedor - Recebedor
    read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_bsik-lifnr.
    el_mov_criar-from-name = gwa_lfa1-name1.

    if gwa_lfa1-stcd2 is not initial. "CPF
      el_mov_criar-from-data-tax_number = gwa_lfa1-stcd2.

    else.
      el_mov_criar-from-data-tax_number = gwa_lfa1-stcd1.

    endif.

    el_mov_criar-from-data-type = cl_seller.
*** TO
    clear gwa_lfa1.
* Dados do Sacado - Pagador
    read table git_lfa1_pag into gwa_lfa1 with key lifnr = gva_lifnr.
    el_mov_criar-to-name = gwa_lfa1-name1.

    if gwa_lfa1-stcd2 is not initial. "CPF
      el_mov_criar-to-data-tax_number = gwa_lfa1-stcd2.

    else.
      el_mov_criar-to-data-tax_number = gwa_lfa1-stcd1.

    endif.

    el_mov_criar-to-data-type = cl_buyer.

    clear: gwa_bkpf.
    read table git_bkpf into gwa_bkpf with key bukrs = gwa_bsik-bukrs
                                               belnr = gwa_bsik-belnr
                                               gjahr = gwa_bsik-gjahr.

    if sy-subrc is initial.
      read table git_bsak into data(gwa_bsak) with key bukrs = gwa_bsik-bukrs "Compensado
                                                       augbl = gwa_bsik-belnr
                                                       gjahr = gwa_bsik-gjahr.
      if sy-subrc is initial.
        read table git_bkpf into gwa_bkpf with key bukrs = gwa_bsak-bukrs
                                                   belnr = gwa_bsak-belnr
                                                   gjahr = gwa_bsak-gjahr.

      else.
        sy-subrc = 0.

      endif.

    endif.

    if sy-subrc is initial.
      read table git_j_1bnflin into gwa_j_1bnflin with key refkey	=	gwa_bkpf-awkey.

      if sy-subrc is initial.
        read table git_j_1bnfe_active into gwa_j_1bnfe_active  with  key docnum = gwa_j_1bnflin-docnum.

        if sy-subrc is initial.
          data(vl_chave_doc) = gwa_j_1bnfe_active-regio && gwa_j_1bnfe_active-nfyear && gwa_j_1bnfe_active-nfmonth && gwa_j_1bnfe_active-stcd1 &&
                               gwa_j_1bnfe_active-model && gwa_j_1bnfe_active-serie  && gwa_j_1bnfe_active-nfnum9  && gwa_j_1bnfe_active-docnum9 &&
                               gwa_j_1bnfe_active-cdv.

          if strlen( vl_chave_doc ) ne 44. "Serviço municipal
            vl_chave_doc = '0'.
            vl_chave_doc = |{ vl_chave_doc alpha = in width = 44 }|.

          endif.

        endif.

      endif.

    endif.
* Calcula a data de vencimento.
    call function 'NET_DUE_DATE_GET'
      exporting
        i_zfbdt = gwa_bsik-zfbdt
        i_zbd1t = gwa_bsik-zbd1t
        i_zbd2t = 0
        i_zbd3t = 0
        i_shkzg = space
        i_rebzg = space
      importing
        e_faedt = gwa_bsik-zfbdt.
*** DATA
    el_mov_criar-data-amount          = conv string( gwa_bsik-dmbtr ).
    condense  el_mov_criar-data-amount no-gaps.
    el_mov_criar-data-date            = |{ gwa_bsik-budat date = iso }|.
    el_mov_criar-data-external_id     = gwa_bsik-bukrs && gwa_bsik-belnr && gwa_bsik-buzei && gwa_bsik-gjahr.
    el_mov_criar-data-post_date       = |{ gwa_bsik-zfbdt date = iso }|.

    if gwa_bsik-xblnr is initial.
      clear el_mov_criar-data-number.
      loop at git_bsak_nota into data(wit_bsak_nota) where bukrs = gwa_bsik-bukrs
                                                     and   belnr = gwa_bsik-belnr.
        if el_mov_criar-data-number is initial.
          el_mov_criar-data-number = wit_bsak_nota-xblnr.
        else.
          concatenate el_mov_criar-data-number wit_bsak_nota-xblnr into el_mov_criar-data-number separated by ';'.
        endif.
      endloop.
    else.
      el_mov_criar-data-number          = gwa_bsik-xblnr.
    endif.


    el_mov_criar-data-nfe_key         = vl_chave_doc.
    el_mov_criar-data-status-name     = cl_dispon.
    el_mov_criar-data-status-category = cl_created.
    "novos campos estrutura DATA
    el_mov_criar-data-accounting_document = gwa_bsik-belnr.
    el_mov_criar-data-supplier_code       = gwa_bsik-lifnr.
    el_mov_criar-data-purchase_order      = gwa_bsik-ebeln.
    "novos campos estrutura DATA


    vl_external_id                    = el_mov_criar-data-external_id.
* Chamada da API Integração Finanfor
    try.
* Criar Movimento.
        zcl_int_ob_finanfor_mov_criar=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = el_mov_criar
                                                                                                 importing e_id_integracao = data(result_id)
                                                                                                           e_integracao    = data(result_json)
                                                                                                ).

        loop at tg_zfit0170_new assigning field-symbol(<fs_zfit0170_new>) where bukrs eq gwa_bsik-bukrs
                                                                            and belnr eq gwa_bsik-belnr
                                                                            and buzei eq gwa_bsik-buzei
                                                                            and gjahr eq gwa_bsik-gjahr.

          data(vl_tabix) = sy-tabix.

          update zfit0170
             set id_created = el_mov_criar-data-external_id
          where bukrs eq <fs_zfit0170_new>-bukrs
            and belnr eq <fs_zfit0170_new>-belnr
            and buzei eq <fs_zfit0170_new>-buzei
            and gjahr eq <fs_zfit0170_new>-gjahr.

          if sy-subrc is initial.
            commit work.
            delete tg_zfit0170_new index vl_tabix.
            vl_qt_criada += 1.

          endif.

        endloop.

      catch zcx_integracao into data(zcx_integracao).
        message id zcx_integracao->msgid type 'E' number zcx_integracao->msgno with zcx_integracao->msgv1 zcx_integracao->msgv2 zcx_integracao->msgv3 zcx_integracao->msgv4 into data(vl_texto).
        vl_texto = |WSC Finanfor - Criar Movimento.| && vl_texto && |. ID_External = { vl_external_id }|.
        message vl_texto type 'S' display like 'E'.
        clear zcx_integracao.

      catch zcx_error into data(zcx_error).
        if result_id is not initial.
* Retorno de erro classe ZCX_ERROR.
          perform zf_return_err_class_zcx_error using zcx_error
                                                      result_id
                                                      'Cria Movimento'
                                                      vl_external_id.

        endif.

    endtry.

    clear: el_mov_get_par, el_mov_get, result_json, result_id, vl_external_id, el_mov_criar, vl_texto.

  endloop.

  if vl_qt_criada is initial.
    message |WSC Finanfor - Criar Movimento. Não há movimentos para serem criados. Empresa { git_bukrs-bukrs }.| type 'S'.

  else.
    message |WSC Finanfor - Criar Movimento. { vl_qt_criada } movimentos criados com sucesso. Empresa { git_bukrs-bukrs }.| type 'S'.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form zf_aprv_repr_movi
*&---------------------------------------------------------------------*
*& Aprova/Reprova movimento - API Finanfor
*&---------------------------------------------------------------------*
form zf_aprv_repr_movi.

  data: tl_zfit0170    type table of zfit0170.

  data: el_mov_apr_rep type zfie_finanfor_mov_apr_rep.

  data: vl_external_id type char21.

  constants: cl_asset  type string value 'asset',
             cl_act_st type string value 'change-status',
             cl_rejeit type string value 'Rejeitado'.

  sort: t_bsak_compens     by bukrs belnr buzei gjahr,
        t_zfit0170_compens by bukrs belnr buzei gjahr.

  check t_zfit0170_compens is not initial.

  loop at t_bsak_compens assigning field-symbol(<fs_bsak>) where bukrs eq git_bukrs.

    read table t_zfit0170_compens assigning field-symbol(<fs_zfit0170_compens>) with key bukrs = <fs_bsak>-bukrs
                                                                                         belnr = <fs_bsak>-belnr
                                                                                         buzei = <fs_bsak>-buzei
                                                                                         gjahr = <fs_bsak>-gjahr
                                                                                binary search.

    if sy-subrc is initial.
      append initial line to tl_zfit0170 assigning field-symbol(<fs_zfit0170>).
      <fs_zfit0170>           = <fs_zfit0170_compens>.
      <fs_zfit0170>-tipo_mvto = '9'.
      <fs_zfit0170>-zfbdt     = <fs_bsak>-zfbdt.
      <fs_zfit0170>-zbd1t     = <fs_bsak>-zbd1t.

    endif.
*** RAÍZ
    el_mov_apr_rep-action           = cl_act_st.
*** DATA
    el_mov_apr_rep-data-id          = <fs_bsak>-bukrs && <fs_bsak>-belnr && <fs_bsak>-buzei && <fs_bsak>-gjahr.
    el_mov_apr_rep-data-date        = |{ <fs_bsak>-budat date = iso }|.
    el_mov_apr_rep-data-category    = cl_asset.
    el_mov_apr_rep-data-external_id = <fs_bsak>-bukrs && <fs_bsak>-belnr && <fs_bsak>-buzei && <fs_bsak>-gjahr.
*** STATUS
    el_mov_apr_rep-data-status-name = cl_rejeit.
    vl_external_id                  = el_mov_apr_rep-data-external_id.
* Chamada da API Integração Finanfor - Aprova/Reprova Movimentos
    try.
        zcl_int_ob_finanfor_mv_apr_rep=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request  = el_mov_apr_rep
                                                                                                  importing e_id_integracao = data(result_id)
                                                                                                            e_integracao    = data(result_json)
                                                                                                 ).
        message |WSC Finanfor - Movimento Reprovado por compensação. Id Externo = { vl_external_id }. Empresa { git_bukrs-bukrs }.| type 'S'.

      catch zcx_integracao into data(zcx_integracao).
        message id zcx_integracao->msgid type 'E' number zcx_integracao->msgno with zcx_integracao->msgv1 zcx_integracao->msgv2 zcx_integracao->msgv3 zcx_integracao->msgv4 into data(vl_texto).
        vl_texto = |WSC Finanfor - Movimento Reprovado.| && vl_texto && |. ID_External = { vl_external_id }. Empresa { git_bukrs-bukrs }.|.
        message vl_texto type 'S' display like 'E'.
        data(vl_erro) = abap_on.
        clear zcx_integracao.

      catch zcx_error into data(zcx_error).
        if result_id is not initial.
* Retorno de erro classe ZCX_ERROR.
          perform zf_return_err_class_zcx_error using zcx_error
                                                      result_id
                                                      'Movimento Reprovar'
                                                      vl_external_id.
          vl_erro = abap_on.

        endif.

    endtry.
* Verifica se deu erro na chamada API de Atualiza/Reprova Movimento.
    if not vl_erro is initial.
      clear vl_erro.
      delete tl_zfit0170 where bukrs eq <fs_zfit0170>-bukrs
                           and belnr eq <fs_zfit0170>-belnr
                           and buzei eq <fs_zfit0170>-buzei
                           and gjahr eq <fs_zfit0170>-gjahr.

    endif.

    clear: el_mov_apr_rep, result_id, result_json, vl_external_id, vl_erro.

  endloop.

  if tl_zfit0170 is not initial.
    modify zfit0170 from table tl_zfit0170.

    if sy-subrc is initial.
      commit work.

    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form zf_atua_repr_movi
*&---------------------------------------------------------------------*
*& Atualiza/Reprova movimento - API Finanfor
*&---------------------------------------------------------------------*
form zf_atua_repr_movi.

  data: lt_bkpf     type table of ty_bkpf,
        tl_zfit0170 type table of zfit0170.

  data: el_mov_update  type       zfie_finanfor_mov_update,
        el_mov_apr_rep type       zfie_finanfor_mov_apr_rep.

  data: vl_datavenc    type sy-datum,
        vl_zbd1t       type dzbd1t,
        vl_endpoint    type char40,
        vl_external_id type char21.

  constants: cl_asset  type string value 'asset',
             cl_act_dt type string value 'change-maturity',
             cl_act_st type string value 'change-status',
             cl_rejeit type string value 'Rejeitado'.

  sort: t_zfit0170_vencto  by bukrs belnr buzei gjahr,
        t_bsik_vencto      by bukrs belnr buzei gjahr.

  check t_zfit0170_vencto is not initial.

  loop at t_bsik_vencto assigning field-symbol(<fs_bsik>) where bukrs eq git_bukrs.
    read table t_zfit0170_vencto assigning field-symbol(<fs_zfit0170_vencto>) with key bukrs = <fs_bsik>-bukrs
                                                                                       belnr = <fs_bsik>-belnr
                                                                                       buzei = <fs_bsik>-buzei
                                                                                       gjahr = <fs_bsik>-gjahr
                                                                              binary search.

    if sy-subrc is initial.
      if <fs_zfit0170_vencto>-zfbdt ne <fs_bsik>-zfbdt or
         <fs_zfit0170_vencto>-zbd1t ne <fs_bsik>-zbd1t or
         <fs_bsik>-zlspr            is not initial.        "bloqueado
        append initial line to tl_zfit0170 assigning field-symbol(<fs_zfit0170>).
        <fs_zfit0170>           = <fs_zfit0170_vencto>.
        <fs_zfit0170>-tipo_mvto = '5'.
        <fs_zfit0170>-zfbdt     = <fs_bsik>-zfbdt.
        <fs_zfit0170>-zbd1t     = <fs_bsik>-zbd1t.
* Validação da data de vencimento do regisdtro com a de parâmetro.
        read table git_zfit0169 into gwa_zfit0169 with key bukrs = <fs_bsik>-bukrs.
        vl_zbd1t = gwa_zfit0169-qte_dias_pg.
* Calcula a data de vencimento de parâmetro.
        call function 'NET_DUE_DATE_GET'
          exporting
            i_zfbdt = sy-datum
            i_zbd1t = vl_zbd1t
            i_zbd2t = 0
            i_zbd3t = 0
            i_shkzg = space
            i_rebzg = space
          importing
            e_faedt = vl_datavenc.
* Calcula a data de vencimento do registro.
        call function 'NET_DUE_DATE_GET'
          exporting
            i_zfbdt = <fs_bsik>-zfbdt
            i_zbd1t = <fs_bsik>-zbd1t
            i_zbd2t = 0
            i_zbd3t = 0
            i_shkzg = space
            i_rebzg = space
          importing
            e_faedt = gwa_bsik-xdtvcto.
* Verifica se passa dos dias para excluir.
        if gwa_bsik-xdtvcto lt vl_datavenc or
           <fs_bsik>-zlspr  is not initial. "Elimina se estiver bloqueado
          <fs_zfit0170>-tipo_mvto = '9'.
          <fs_zfit0170>-zlspr     = <fs_bsik>-zlspr.
        endif.
* Chamada da API Integração Finanfor.
        try.
            case <fs_zfit0170>-tipo_mvto.
* Atualiza Movimentos.
              when '5'. "Alterado
*** RAÍZ
                el_mov_update-action = cl_act_dt.
* Calcula a data de vencimento.
                call function 'NET_DUE_DATE_GET'
                  exporting
                    i_zfbdt = <fs_bsik>-zfbdt
                    i_zbd1t = <fs_bsik>-zbd1t
                    i_zbd2t = 0
                    i_zbd3t = 0
                    i_shkzg = space
                    i_rebzg = space
                  importing
                    e_faedt = <fs_bsik>-zfbdt.
*** DATA
                el_mov_update-data-id          = <fs_bsik>-bukrs && <fs_bsik>-belnr && <fs_bsik>-buzei && <fs_bsik>-gjahr.
                el_mov_update-data-date        = |{ <fs_bsik>-budat date = iso }|.
                el_mov_update-data-category    = cl_asset.
                el_mov_update-data-external_id = <fs_bsik>-bukrs && <fs_bsik>-belnr && <fs_bsik>-buzei && <fs_bsik>-gjahr.
                el_mov_update-data-post_date   = |{ <fs_bsik>-zfbdt date = iso }|.
                vl_external_id                 = el_mov_update-data-external_id.
* Atualiza Movimentos.
                vl_endpoint = 'Atualiza Movimento'.
                zcl_int_ob_finanfor_mov_update=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request  = el_mov_update
                                                                                                          importing e_id_integracao = data(result_id)
                                                                                                                    e_integracao    = data(result_json)
                                                                                                         ).
                message |WSC Finanfor - Movimento Atualiza - Dt.Vncto. Id Externo = { vl_external_id }| type 'S'.
* Aprova/Reprova Movimentos.
              when '9'. "Estorno
*** RAÍZ
                el_mov_apr_rep-action = cl_act_st.
*** DATA
                el_mov_apr_rep-data-id          = <fs_bsik>-bukrs && <fs_bsik>-belnr && <fs_bsik>-buzei && <fs_bsik>-gjahr.
                el_mov_apr_rep-data-date        = |{ <fs_bsik>-budat date = iso }|.
                el_mov_apr_rep-data-category    = cl_asset.
                el_mov_apr_rep-data-external_id = <fs_bsik>-bukrs && <fs_bsik>-belnr && <fs_bsik>-buzei && <fs_bsik>-gjahr.
*** STATUS
                el_mov_apr_rep-data-status-name = cl_rejeit.
                vl_external_id                  = el_mov_apr_rep-data-external_id.
* Aprova/Reprova Movimentos.
                vl_endpoint = 'Reprova Movimento'.
                zcl_int_ob_finanfor_mv_apr_rep=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request  = el_mov_apr_rep
                                                                                                          importing e_id_integracao = result_id
                                                                                                                    e_integracao    = result_json
                                                                                                         ).
                message |WSC Finanfor - Movimento Reprovado - Dt.Vncto. Id Externo = { vl_external_id }. Empresa { git_bukrs-bukrs }.| type 'S'.

              when others.
*       Do nothing
            endcase.

          catch zcx_integracao into data(zcx_integracao).
            message id zcx_integracao->msgid type 'E' number zcx_integracao->msgno with zcx_integracao->msgv1 zcx_integracao->msgv2 zcx_integracao->msgv3 zcx_integracao->msgv4 into data(vl_texto).
            vl_texto = |WSC Finanfor - { vl_endpoint }| && vl_texto && |. ID_External = { vl_external_id }. Empresa { git_bukrs-bukrs }.|.
            message vl_texto type 'S' display like 'E'.
            data(vl_erro) = abap_on.

          catch zcx_error into data(zcx_error).
            if result_id is not initial.
* Retorno de erro classe ZCX_ERROR.
              perform zf_return_err_class_zcx_error using zcx_error
                                                          result_id
                                                          vl_endpoint
                                                          vl_external_id.
              vl_erro = abap_on.

            endif.

        endtry.
* Verifica se deu erro na chamada API de Atualiza/Reprova Movimento.
        if not vl_erro is initial.
          clear vl_erro.
          delete tl_zfit0170 where bukrs eq <fs_zfit0170>-bukrs
                               and belnr eq <fs_zfit0170>-belnr
                               and buzei eq <fs_zfit0170>-buzei
                               and gjahr eq <fs_zfit0170>-gjahr.

        endif.

      endif.

    endif.

    clear: el_mov_update, result_id, result_json, vl_erro, vl_external_id.

  endloop.

  if tl_zfit0170 is not initial.
    modify zfit0170 from table tl_zfit0170.
    if sy-subrc is initial.
      commit work.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form zf_busca_movimentos
*&---------------------------------------------------------------------*
*& Busca movimento - API Finanfor
*&---------------------------------------------------------------------*
form zf_busca_movimentos.

  data: tl_zfit0170 type table of zfit0170.

  data: el_return_erro  type zstruct_return_api_eudr,
        el_mov_get_par  type zfie_finanfor_mov_get_param,
        el_mov_get      type zfie_finanfor_mov_get,
        el_mov_get_done type zfie_finanfor_mov_get_done,
        begin of el_id_extr,
          bukrs type bukrs,
          belnr type belnr_d,
          buzei type buzei,
          gjahr type gjahr,
        end of el_id_extr.

  data: vl_data        type dats.

  constants: cl_done     type string    value 'done',
             cl_trades   type string    value 'trades',
             cl_children type string    value '/children?page=0&size=100&category=asset',
             cl_tax_num  type string    value 'tax-number',
             cl_tax_num2 type string    value 'tax_number',
             cl_nome_arq type znome_arq value 'API Finanfor'.

  vl_data            = sy-datum - 1.
  data(vl_data_util) = zcl_miro=>get_proximo_dia_util( exporting i_data_base = conv #( vl_data ) ).

* Chamada da API Integração Finanfor
  try.
*** Busca Pacote de Movimento Negociado.
** FILTERS
* Nome do Status
      append initial line to el_mov_get_par-filters assigning field-symbol(<fs_filters>).
      <fs_filters>-condition = 'equals'.
      <fs_filters>-key       = 'status.category'.
      append initial line to <fs_filters>-values assigning field-symbol(<fs_values>).
      <fs_values> = cl_done.
* Data de Criação.
      append initial line to el_mov_get_par-filters assigning <fs_filters>.
      <fs_filters>-condition = 'between'.
      <fs_filters>-key       = 'date'.
      append initial line to <fs_filters>-values assigning <fs_values>.
      <fs_values> = |{ vl_data_util date = iso }|.
      append initial line to <fs_filters>-values assigning <fs_values>.
      <fs_values> = |{ sy-datum date = iso }|.
** PAGINATION
      el_mov_get_par-pagination-page       = '0'.
      el_mov_get_par-pagination-size       = '100'.
      el_mov_get_par-pagination-sort_by    = 'date'.
      el_mov_get_par-pagination-sort_order = 'asc'.

      zcl_int_ob_finanfor_mov_get=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request  = el_mov_get_par
                                                                                             importing e_id_integracao = data(result_id)
                                                                                                       e_integracao    = data(result_json)
                                                                                            ).
* Recupera os dados do JSON
      /ui2/cl_json=>deserialize( exporting json = result_json-ds_data_retorno changing data = el_mov_get_done ).
* Verifica se retornou dados da busca da API Finanfor Filtrar Movimento Negociado.
      if not el_mov_get_done-content is initial.
        loop at el_mov_get_done-content into data(el_content2).
* sy-abcde+12(1) = M - Busca Movimento de Pacote Negociado
          data(lv_info_request) = sy-abcde+12(1) && el_content2-data-id && cl_children.
* Chamada da API Integração Finanfor
          try.
*** Busca Movimento de Pacote Negociado.
              zcl_int_ob_finanfor_mov_get=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request  = lv_info_request
                                                                                                     importing e_id_integracao = result_id
                                                                                                               e_integracao    = result_json
                                                                                                    ).
* Recupera os dados do JSON
              replace all occurrences of cl_tax_num in result_json-ds_data_retorno with cl_tax_num2.
              /ui2/cl_json=>deserialize( exporting json = result_json-ds_data_retorno changing data = el_mov_get ).
* Verifica se retornou dados da busca da API Finanfor Filtrar Movimento.
              if not el_mov_get-content is initial.
                loop at el_mov_get-content into data(el_content) where data-parent_movement_id eq el_content2-data-id.
                  el_id_extr = el_content-data-external_id.

                  select single * from zfit0170
                    into @data(el_zfit0170)
                  where bukrs      eq @el_id_extr-bukrs
                    and belnr      eq @el_id_extr-belnr
                    and buzei      eq @el_id_extr-buzei
                    and gjahr      eq @el_id_extr-gjahr
                    and augbl      eq @space
                    and status_ret eq @abap_off.

                  check sy-subrc is initial.
                  data(vl_dt_oper_al5) = el_content-data-parent_movement_date(4) && el_content-data-parent_movement_date+5(2) && el_content-data-parent_movement_date+8(2).
                  el_zfit0170-id_oper_al5 = el_content-data-parent_movement_external_id.
                  el_zfit0170-dt_oper_al5 = vl_dt_oper_al5.
                  el_zfit0170-status_ret  = abap_on.
                  el_zfit0170-dt_ret      = sy-datum.
                  el_zfit0170-hr_ret      = sy-uzeit.
                  el_zfit0170-nome_arq    = cl_nome_arq.
                  append el_zfit0170 to tl_zfit0170.
                  clear: el_zfit0170, el_id_extr.

                endloop.

                check not tl_zfit0170[] is initial.
                modify zfit0170 from table tl_zfit0170.
                data(vl_data_save) = abap_on.
                check sy-subrc is initial.
                commit work and wait.

              else.
                message |WSC Finanfor - Busca Movimento. Não há dados nogociados para atualizar.| type 'S' display like 'W'.

              endif.

            catch zcx_integracao into data(zcx_integracao).
              message id zcx_integracao->msgid type 'E' number zcx_integracao->msgno with zcx_integracao->msgv1 zcx_integracao->msgv2 zcx_integracao->msgv3 zcx_integracao->msgv4 into data(vl_texto).
              vl_texto = |WSC Finanfor - Busca Movimento.| && vl_texto.
              message vl_texto type 'S' display like 'E'.
              clear: zcx_integracao.

            catch zcx_error into data(zcx_error).
              if result_id is not initial.
* Retorno de erro classe ZCX_ERROR.
                perform zf_return_err_class_zcx_error using zcx_error
                                                            result_id
                                                            'Busca Movimento'
                                                            space.

              endif.

          endtry.

        endloop.

      else.
        message |WSC Finanfor - Busca Movimento Negociado. Não há dados nogociados para atualizar.| type 'S' display like 'W'.
        vl_data_save = sy-abcde+13(1). "N - Não exibir mensagem final

      endif.

    catch zcx_integracao into zcx_integracao.
      message id zcx_integracao->msgid type 'E' number zcx_integracao->msgno with zcx_integracao->msgv1 zcx_integracao->msgv2 zcx_integracao->msgv3 zcx_integracao->msgv4 into vl_texto.
      vl_texto = |WSC Finanfor - Busca Movimento.| && vl_texto.
      message vl_texto type 'S' display like 'E'.
      clear: zcx_integracao.

    catch zcx_error into zcx_error.
      if result_id is not initial.
* Retorno de erro classe ZCX_ERROR.
        perform zf_return_err_class_zcx_error using zcx_error
                                                    result_id
                                                    'Busca Movimento'
                                                    space.

      endif.

  endtry.

  if vl_data_save is initial.
    message |WSC Finanfor - Busca Movimento. Não há dados nogociados para atualizar.| type 'S' display like 'W'.

  else.
    check vl_data_save ne sy-abcde+13(1). "N - Não exibir mensagem final
    message |WSC Finanfor - Busca Movimento. Houve Dado(s) nogociado(s) atualizado(s) com sucesso.| type 'S'.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form zf_return_err_class_zcx_error
*&---------------------------------------------------------------------*
*& Retorno de erro classe ZCX_ERROR
*&---------------------------------------------------------------------*
*&      --> UCX_ERROR    Classe de Erro Genérica
*&      --> UV_RESULT_ID Id. de Integração da tabela ZINTEGRACAO_LOG
*&      --> UV_ENDPOINT  Nome do Endpoint em execução no momento
*&---------------------------------------------------------------------*
form zf_return_err_class_zcx_error using ucx_error      type ref to zcx_error
                                         uv_result_id   type zde_id_integracao
                                         uv_endpoint    type char40
                                         uv_external_id type char21.

  select single nm_code, ds_data_retorno, ds_erro
    from zintegracao_log
    into ( @data(vl_nm_code), @data(vl_data_return_erro), @data(vl_ds_erro) )
  where id_integracao eq @uv_result_id.

  if sy-subrc is initial.

    case vl_nm_code.
      when '0400'.
        data(vl_texto) = |Bad Request. Dados inconsistente.|.

      when '0401'.
        vl_texto = |Os dados de autenticação do usuário não são válidos.|.

      when '0403'.
        vl_texto = |O usuário não está autorizado a realizar essa operação.|.

      when others.
        message id ucx_error->msgid type 'S' number ucx_error->msgno with ucx_error->msgv1 ucx_error->msgv2 ucx_error->msgv3 ucx_error->msgv4 into vl_texto.

    endcase.

  else.
    message id ucx_error->msgid type 'S' number ucx_error->msgno with ucx_error->msgv1 ucx_error->msgv2 ucx_error->msgv3 ucx_error->msgv4 into vl_texto.

  endif.

  if uv_endpoint    eq 'Busca Movimento' and
     uv_external_id is initial.
    vl_texto = |WSC Finanfor - { uv_endpoint } | && vl_texto.

  else.
    vl_texto = |WSC Finanfor - { uv_endpoint } | && vl_texto && | ID_External = { uv_external_id } Empresa { git_bukrs-bukrs }.|.

  endif.

  message vl_texto type 'S' display like 'E'.

  clear: ucx_error.

endform.
**<<<------"184694 - NMS - FIM------>>>
