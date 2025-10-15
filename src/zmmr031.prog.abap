*&--------------------------------------------------------------------&*
*& Report Name    : Relatório de Informações Gerais de Compras.       *&
*& Author         : Victor Hugo                                       *&
*& Date           : 07.06.2012                                        *&
*& Funcional Area : MM                                                *&
*&                                                                    *&
*&--------------------------------------------------------------------&*
report  zmmr031.


*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
tables: rbkp, rseg, ekbe, ekko, ekpo, lfa1, t023t, t024, konv, cskt.

*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*

types:
  begin of ty_rbkp,
    bukrs type rbkp-bukrs,
    budat type rbkp-budat,
    blart type rbkp-blart,
    belnr type rbkp-belnr,
    usnam type rbkp-usnam,
    cputm type rbkp-cputm,
    xblnr type rbkp-xblnr,
    lifnr type rbkp-lifnr,
    waers type rbkp-waers,
    stblg type rbkp-stblg,
    stjah type rbkp-stjah,
    bldat type rbkp-bldat,
    cpudt type rbkp-cpudt,
    kursf type rbkp-kursf,
    gjahr type rbkp-gjahr,
    zterm type rbkp-zterm,
    zbd1t type rbkp-zbd1t,
    zfbdt type rbkp-zfbdt,
    rmwwr type rbkp-rmwwr,
  end of ty_rbkp,

  begin of ty_rseg,
    belnr  type rseg-belnr,
    buzei  type rseg-buzei,
    ebeln  type rseg-ebeln,
    ebelp  type rseg-ebelp,
    matnr  type rseg-matnr,
    werks  type rseg-werks,
    shkzg  type rseg-shkzg,
    mwskz  type rseg-mwskz,
    bprme  type rseg-bprme,
    lfbnr  type rseg-lfbnr,
    lfgja  type rseg-lfgja,
    wrbtr  type rseg-wrbtr,
    menge  type rseg-menge,
    bnkan  type rseg-bnkan,
    gjahr  type rseg-gjahr,
    packno type rseg-packno,
  end of ty_rseg,

  begin of ty_ekbe,
    ebeln type ekbe-ebeln,
    ebelp type ekbe-ebelp,
    belnr type ekbe-belnr,
    gjahr type ekbe-gjahr,
    vgabe type ekbe-vgabe,
  end of ty_ekbe,

  begin of ty_eket,
    ebeln type eket-ebeln,
    ebelp type eket-ebelp,
    eindt type eket-eindt,
  end of ty_eket,

*-US 135191-25-06-2024-#140934-RJF-inicio
  begin of ty_ekbe_m,
    ebeln type ekbe-ebeln,
    belnr type ekbe-belnr,
    cpudt type ekbe-cpudt,
    lfbnr type ekbe-lfbnr,
    bwart type ekbe-bwart,
    bewtp type ekbe-bewtp,
  end of ty_ekbe_m,
*-US 135191-25-06-2024-#140934-RJF-fim

  begin of ty_ekkn,
    ebeln type ekkn-ebeln,
    ebelp type ekkn-ebelp,
    kostl type ekkn-kostl,
  end of ty_ekkn,


  begin of ty_ekko,
    ebeln type ekko-ebeln,
    bsart type ekko-bsart,
    ernam type ekko-ernam,
    aedat type ekko-aedat,
    lifnr type ekko-lifnr,
    ekorg type ekko-ekorg,
    ekgrp type ekko-ekgrp,
    waers type ekko-waers,
    inco1 type ekko-inco1,
    inco2 type ekko-inco2,
    bedat type ekko-bedat,
    knumv type ekko-knumv,
    angnr type ekko-angnr,
    ihran type ekko-ihran,
    zterm type ekko-zterm,
  end of ty_ekko,

  begin of ty_ekpo,
    ebeln  type ekpo-ebeln,
    loekz  type ekpo-loekz,
    txz01  type ekpo-txz01,
    matnr  type ekpo-matnr,
    matkl  type ekpo-matkl,
    werks  type ekpo-werks,
    lgort  type ekpo-lgort,
    meins  type ekpo-meins,
    mwskz  type ekpo-mwskz,
    banfn  type ekpo-banfn,
    bnfpo  type ekpo-bnfpo,
    menge  type ekpo-menge,
    netpr  type ekpo-netpr,
    netwr  type ekpo-netwr,
    ebelp  type ekpo-ebelp,
    konnr  type ekpo-konnr,
    packno type ekpo-packno,
    prdat  type ekpo-prdat,
  end of ty_ekpo,

  begin of ty_servico,
    ebeln  type rseg-ebeln,
    ebelp  type rseg-ebelp,
    ktext1 type esll-ktext1,
    srvpos type esll-srvpos,
  end of ty_servico,

  begin of ty_lfa1,
    lifnr type lfa1-lifnr,
    name1 type lfa1-name1,
    regio type lfa1-regio,
    stcd1 type lfa1-stcd1,
    stcd2 type lfa1-stcd2,
  end of ty_lfa1,

  begin of ty_t023t,
    spras   type t023t-spras,
    matkl   type t023t-matkl,
    wgbez60 type t023t-wgbez60,
  end of ty_t023t,

  begin of ty_t024,
    ekgrp type t024-ekgrp,
    eknam type t024-eknam,
  end of ty_t024,

  begin of ty_mara,
    matnr type mara-matnr,
    mtart type mara-mtart,
  end of ty_mara,

  begin of ty_cskt,
    ltext type cskt-ltext,
    kostl type cskt-kostl,
  end of ty_cskt,

  begin of ty_saida,

    belnr          type rbkp-belnr,
    gjahr          type rbkp-gjahr,
    buzei          type rseg-buzei,
    blart          type rbkp-blart,
    usnam          type rbkp-usnam,
    cputm          type rbkp-cputm,
    xblnr          type rbkp-xblnr,
    bukrs          type rbkp-bukrs,
    lifnr          type rbkp-lifnr,
    waers          type rbkp-waers,
    stblg          type rbkp-stblg,
    stjah          type rbkp-stjah,
    bldat          type rbkp-bldat,
    budat          type rbkp-budat,
    cpudt          type rbkp-cpudt,
    kursf          type rbkp-kursf,


    name1_m        type lfa1-name1,
    regio_m        type lfa1-name1,

*-US 140934-25-06-2024-#140934-RJF-inicio
    belnr_m        type ekbe-belnr,
    cpudt_m        type ekbe-cpudt,
    bwart_m        type ekbe-bwart,
*-US 140934-25-06-2024-#140934-RJF-fim

    ebeln          type rseg-ebeln,
    ebelp          type rseg-ebelp,
    saving         type konv-kbetr,
    xzsv3          type konv-kbetr,
    xzsv4          type konv-kbetr,
    matnr          type rseg-matnr,
    mtart          type mara-mtart,
    werks          type rseg-werks,
    name1	         type name1,
    kostl          type ekkn-kostl,
    shkzg          type rseg-shkzg,
    mwskz          type rseg-mwskz,
    lfbnr          type rseg-lfbnr,
    menge          type rseg-menge,
    wrbtr          type rseg-wrbtr,
    wmwst          type rbtx-wmwst,
    bruto          type rseg-wrbtr,

    vgabe          type ekbe-vgabe,

    bsart          type ekko-bsart,
    ernam          type ekko-ernam,
    aedat          type ekko-aedat,
    ekorg          type ekko-ekorg,
    ekgrp          type ekko-ekgrp,

    lifnr_p        type ekko-lifnr,
    name1_p        type lfa1-name1,
    regio_p        type lfa1-name1,

    eknam          type t024-eknam,

    waers_p        type ekko-waers,
    inco1          type ekko-inco1,
    inco2          type ekko-inco2,
    bedat          type ekko-bedat,

    loekz          type ekpo-loekz,
    txz01          type ekpo-txz01,
    matnr_p        type ekpo-matnr,
    matkl          type ekpo-matkl,
    lgort          type ekpo-lgort,
    meins          type ekpo-meins,
    banfn          type ekpo-banfn,
    bnfpo          type ekpo-bnfpo,
    menge_p        type ekpo-menge,
    netpr          type ekpo-netpr,
    netwr          type ekpo-netwr,
    konnr          type ekpo-konnr,

    wgbez60        type t023t-wgbez60,
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
    zterm          type rbkp-zterm,
    vtext          type tvzbt-vtext,
    zbd1t          type rbkp-zbd1t,
    zfbdt          type rbkp-zfbdt,
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM

    " Includir cotação e data contação "Anderson Oenning.
    angnr          type ekko-angnr,
    ihran          type ekko-ihran,
    text1          type t052u-text1,

    "INCLUIR DESCRIÇÃO DO CENTRO DE CUSTO - PABLO
    ltext          type cskt-ltext,

    atwrt          type ausp-atwrt,
    asktx          type asmdt-asktx,
    packno         type esll-packno,
    sub_packno     type esll-sub_packno,
    ktext1         type esll-ktext1,
    srvpos         type esll-srvpos,
    stcd1          type lfa1-stcd1,
    prdat          type ekpo-prdat,
    for_cnpj       type stcd1,
    for_dt_remessa type sy-datum,
  end of ty_saida,

  begin of ty_konv,
    knumv type konv-knumv,
    kposn type konv-kposn,
    stunr type konv-stunr,
    zaehk type konv-zaehk,
    kschl type konv-kschl,
    kbetr type konv-kbetr,
  end of ty_konv.

types y_kschl type range of konv-kschl.
*&--------------------------------------------------------------------&*
*& INTERNAL TABLE
*&--------------------------------------------------------------------&*

data: begin of t_tvzbt occurs 0.
        include structure tvzbt.
data: end of t_tvzbt.

data: it_eket        type table of ty_eket with header line,
      it_rbkp        type table of ty_rbkp with header line,
      it_rbkp_aux    type table of ty_rbkp with header line,
      it_rseg        type table of ty_rseg with header line,
      it_rseg_aux    type table of ty_rseg with header line,
      it_rbtx        type table of rbtx with header line,

      it_ekbe        type table of ty_ekbe with header line,
      it_ekbe_m      type table of ty_ekbe_m with header line,
      it_ekbe_mi     type table of ty_ekbe_m with header line,
      it_ekbe_mir    type table of ty_ekbe_m with header line,
      it_ekbe_aux    type table of ty_ekbe with header line,

      it_ekkn        type table of ty_ekkn with header line,
      it_ekko        type table of ty_ekko with header line,
      it_ekko_aux    type table of ty_ekko with header line,
      it_ekpo        type table of ty_ekpo with header line,
      it_ekpo_aux    type table of ty_ekpo with header line,
      it_ekpoaux2    type table of ty_ekpo with header line,
      it_mara        type table of ty_mara with header line,
      it_ausp        type table of ausp with header line,
      it_asmdt       type table of asmdt with header line,

      it_lfa1_miro   type table of ty_lfa1,
      it_lfa1_pedido type table of ty_lfa1,

      it_t023t       type table of ty_t023t,
      it_t024        type table of ty_t024,

      it_saida       type table of ty_saida with header line,
      it_t001w       type table of t001w    with header line,

      it_konv        type table of ty_konv,
      it_cskt        type table of ty_cskt,

      it_servico     type table of ty_servico. " WITH HEADER LINE.
*&--------------------------------------------------------------------&*
*& WORK AREA
*&--------------------------------------------------------------------&*
data: wa_rbkp        type ty_rbkp,
      wa_rseg        type ty_rseg,

      wa_ekbe        type ty_ekbe,
      wa_ekbe_aux    type ty_ekbe,

      wa_ekkn        type ty_ekkn,

      wa_ekko        type ty_ekko,

      wa_ekpo        type ty_ekpo,
      wa_ekpoaux2    type ty_ekpo,

      wa_lfa1_miro   type ty_lfa1,
      wa_lfa1_pedido type ty_lfa1,

      wa_t023t       type ty_t023t,
      wa_t024        type ty_t024,
      wa_saida       type ty_saida,

      wa_cskt        type ty_cskt,

      wa_konv        type ty_konv,
      wa_servico     type ty_servico.



*&--------------------------------------------------------------------&*
*& Constantes
*&--------------------------------------------------------------------&*
constants: c_kschl_zsv1 type konv-kschl value 'ZSV1',
           c_kbetr_10   type i value '10'.

*&--------------------------------------------------------------------&*
*& Ranges
*&--------------------------------------------------------------------&*
data: r_kschl type y_kschl with header line.

*&--------------------------------------------------------------------&*
*& ALV
*&--------------------------------------------------------------------&*
data: cl_container type ref to cl_gui_custom_container,
      cl_grid      type ref to cl_gui_alv_grid,
      it_fcat      type lvc_t_fcat,
      wa_fcat      type lvc_s_fcat,
      wa_layout    type lvc_s_layo,
      wa_variant   type disvariant.

data: t_bdc     type table of bdcdata with header line initial size 0,
      t_messtab type table of bdcmsgcoll.

*&--------------------------------------------------------------------&*
*& PARAMETERS
*&--------------------------------------------------------------------&*
selection-screen: begin of block b1 with frame title text-001.
  select-options:  p_bukrs for rbkp-bukrs no-extension,
                   p_budat for rbkp-budat no-extension,
                   p_kostlp for cskt-kostl,
                   p_blart for rbkp-blart,
                   p_bsart for ekko-bsart,
                   p_belnr for rbkp-belnr,
                   p_ebeln for rseg-ebeln.
selection-screen: end of block b1.


*&---------------------------------------------------------------------*
*&      Form  HANDLER
*&---------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.
    methods:
      handle_hotspot_click for event  hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.
endclass.                    "LCL_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_handler implementation.
  method handle_hotspot_click.
    perform: handle_hotspot_click using e_row_id e_column_id es_row_no.
  endmethod.                    "handle_hotspot_click
endclass.                    "lcl_event_handler IMPLEMENTATION

*&--------------------------------------------------------------------&*
*& SELECTION
*&--------------------------------------------------------------------&*

start-of-selection.

  perform: seleciona_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
form seleciona_dados .

  " Cabeçalho doc.da fatura recebida
  select distinct
         rbkp~bukrs rbkp~budat rbkp~blart rbkp~belnr rbkp~usnam rbkp~cputm rbkp~xblnr
         rbkp~lifnr rbkp~waers rbkp~stblg rbkp~stjah rbkp~bldat rbkp~cpudt rbkp~kursf rbkp~gjahr
         rbkp~zterm rbkp~zbd1t rbkp~zfbdt rbkp~rmwwr
    from rbkp
    inner join rseg
    on  rseg~belnr eq rbkp~belnr
    and rseg~gjahr eq rbkp~gjahr
    into table it_rbkp
  where rbkp~bukrs in p_bukrs
    and rbkp~budat in p_budat
    and rbkp~blart in p_blart
    and rbkp~belnr in p_belnr
    and rseg~ebeln in p_ebeln.

  check it_rbkp[] is not initial.

  " Impostos da MIRO
  select *
   from rbtx
   into table it_rbtx
   for all entries in it_rbkp
 where belnr eq it_rbkp-belnr
   and gjahr eq it_rbkp-gjahr.

  " Item de documento fatura recebida
  select belnr buzei ebeln ebelp matnr werks shkzg mwskz bprme lfbnr lfgja wrbtr menge bnkan gjahr packno
    from rseg
    into table it_rseg
    for all entries in it_rbkp
  where belnr eq it_rbkp-belnr
    and gjahr eq it_rbkp-gjahr
    and ebeln in p_ebeln.


  it_rseg_aux[] = it_rseg[].
  sort it_rseg_aux by matnr.
  delete it_rseg_aux where matnr is initial.
  if it_rseg_aux[] is not initial.
    select matnr mtart
      from mara
      into table it_mara
      for all entries in it_rseg_aux
      where matnr = it_rseg_aux-matnr.
    sort it_mara by matnr.
    refresh it_rseg_aux.
  endif.

  "ESLL pega a partir do pedido se tem o serviço
  if it_rseg[] is not initial.
    select ekpo~ebeln, ekpo~ebelp, esll2~ktext1 , esll2~srvpos
    from ekpo
      left join esll as esll1 on esll1~packno = ekpo~packno
      left join esll as esll2 on esll2~packno = esll1~sub_packno
      into table @data(it_servico_)
      for all entries in @it_rseg
      where ekpo~ebeln eq @it_rseg-ebeln
      and   ekpo~ebelp eq @it_rseg-ebelp
      and   ekpo~matnr eq @it_rseg-matnr.
    it_servico[] = it_servico_[].
  endif.

  "classif contabil pedido
  select ebeln ebelp kostl
    from ekkn
    into table it_ekkn
    for all entries in it_rseg
    where ebeln eq it_rseg-ebeln
    and ebelp eq it_rseg-ebelp
    and kostl in  p_kostlp.

  " Histórico para o documento de compra
  select  ebeln ebelp belnr gjahr vgabe
    from ekbe
    into table it_ekbe_aux
    for all entries in it_rseg
  where ebeln eq it_rseg-ebeln
    and ebelp eq it_rseg-ebelp
    and belnr eq it_rseg-belnr.

*-US 135191-25-06-2024-#140934-RJF-inicio
  if it_rseg[] is not initial.
    select  belnr ebeln lfbnr
      from ekbe
      into corresponding fields of table it_ekbe_m
      for all entries in it_rseg
    where ebeln eq it_rseg-ebeln
      and belnr eq it_rseg-belnr.
    if sy-subrc is initial.
      select  belnr ebeln cpudt bwart
        from ekbe
        into corresponding fields of table  it_ekbe_mi
        for all entries in it_ekbe_m
      where belnr eq it_ekbe_m-lfbnr
      and vgabe eq '1'.

      select  belnr ebeln cpudt bwart lfbnr
           from ekbe
            into corresponding fields of table it_ekbe_mir
            for all entries in it_ekbe_m
          where ebeln eq it_ekbe_m-ebeln
            and lfbnr eq it_ekbe_m-lfbnr
            and vgabe eq '1'.
    endif.
  endif.
*-US 135191-25-06-2024-#140934-RJF-fim


  "Textos de centros de custo

  select ltext kostl
      from cskt
    into table it_cskt
    for all entries in it_ekkn
    where kostl = it_ekkn-kostl and spras = sy-langu.



  loop at it_ekbe_aux into wa_ekbe_aux.

    read table it_rbkp into wa_rbkp with key gjahr = wa_ekbe_aux-gjahr.

    if ( sy-subrc eq 0 ).

      wa_ekbe-ebeln  = wa_ekbe_aux-ebeln.
      wa_ekbe-ebelp  = wa_ekbe_aux-ebelp.
      wa_ekbe-belnr  = wa_ekbe_aux-belnr.
      wa_ekbe-gjahr  = wa_ekbe_aux-gjahr.
      wa_ekbe-vgabe  = wa_ekbe_aux-vgabe.

      append wa_ekbe to it_ekbe.

    endif.

    clear: wa_ekbe_aux, wa_rbkp, wa_ekbe.
  endloop.

  clear it_rseg_aux[].
  move it_rseg[] to it_rseg_aux[].
  sort it_rseg_aux by ebeln.
  delete adjacent duplicates from it_rseg_aux comparing ebeln.

  " Cabeçalho do documento de compra
  select ebeln bsart ernam aedat lifnr ekorg ekgrp waers inco1 inco2 bedat knumv angnr ihran zterm
    from ekko
    into table it_ekko
    for all entries in it_rseg_aux
  where ebeln eq it_rseg_aux-ebeln.

  if it_ekko[] is not initial.

* Condições (dados de operação) - Preenche range de Condições

    perform  zf_kschl.

    select from v_konv fields knumv , kposn , stunr , zaehk , kschl , kbetr for all entries in @it_ekko where knumv = @it_ekko-knumv and kschl in @r_kschl into table @it_konv .

  endif.

  select ebeln loekz txz01 matnr matkl werks lgort meins mwskz banfn bnfpo menge netpr netwr ebelp konnr packno prdat
    from ekpo
    into table it_ekpo
    for all entries in it_rseg
  where ebeln eq it_rseg-ebeln
    and ebelp eq it_rseg-ebelp.

  if it_ekpo[] is not initial.
    select *
     from eket
     into corresponding fields of table it_eket
     for all entries in it_ekpo
   where ebeln eq it_ekpo-ebeln
     and ebelp eq it_ekpo-ebelp.
  endif.



  clear: it_rbkp_aux[].
  move it_rbkp[] to it_rbkp_aux[].
  sort it_rbkp_aux by lifnr.
  delete adjacent duplicates from it_rbkp_aux comparing lifnr.

  " Dados do Fornecedor - Miro
  select lifnr name1 regio stcd1 stcd2
    from lfa1
    into table it_lfa1_miro
    for all entries in it_rbkp_aux
  where lifnr eq it_rbkp_aux-lifnr.

  clear: it_ekko_aux[].
  move it_ekko[] to it_ekko_aux[].
  sort it_ekko_aux by lifnr.
  delete adjacent duplicates from it_ekko_aux comparing lifnr.

  " Dados do Fornecedor - Pedido
  select lifnr name1 regio stcd1 stcd2
    from lfa1
    into table it_lfa1_pedido
    for all entries in it_ekko_aux
  where lifnr eq it_ekko_aux-lifnr.

  clear: it_ekpo_aux[].
  move it_ekpo[] to it_ekpo_aux[].
  sort it_ekpo_aux by matkl.
  delete adjacent duplicates from it_ekpo_aux comparing matkl.

  " Denominações para grupos de mercadoria
  select spras matkl wgbez60
    from t023t
    into table it_t023t
    for all entries in it_ekpo_aux
  where matkl eq it_ekpo_aux-matkl
    and spras eq 'P'.

  clear: it_ekko_aux[].
  move it_ekko[] to it_ekko_aux[].
  sort it_ekko_aux by ekgrp.
  delete adjacent duplicates from it_ekko_aux comparing ekgrp.

  " Grupos de compra
  select ekgrp eknam
    from t024
    into table it_t024
    for all entries in it_ekko_aux
  where ekgrp eq it_ekko_aux-ekgrp.


*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
  select * from tvzbt into table t_tvzbt
    where spras = sy-langu.
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM

  perform: seleciona_saida.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_SAIDA
*&---------------------------------------------------------------------*
form seleciona_saida .

  data: vg_tabix type sy-tabix,
        v_belnr  type rbkp-belnr,
        v_gjahr  type rbkp-gjahr,
        v_wrbtr  type rseg-wrbtr.

  select * into table it_t001w
    from t001w.

  clear it_rseg_aux[].
  move it_rseg[] to it_rseg_aux[].


  sort: it_ekkn by ebeln ebelp,
        it_rbkp by belnr gjahr,
        it_rseg by belnr gjahr,
        it_rseg_aux by belnr gjahr,
        it_ekko by ebeln,
        it_servico by ebeln ebelp,
        it_ekbe by ebeln ebelp belnr gjahr,
        it_ekpo by ebeln ebelp,
        it_lfa1_pedido by lifnr,
        it_rbtx by belnr gjahr mwskz,
        it_konv by knumv kposn.

*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
  sort t_tvzbt by spras zterm.
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM

  clear: v_belnr, v_gjahr.

  loop at it_rseg into wa_rseg.
    clear: wa_cskt, wa_saida, wa_rbkp, v_wrbtr, it_rseg_aux, wa_lfa1_miro, wa_servico, wa_konv.
*    IF v_belnr NE wa_rseg-belnr OR v_gjahr NE wa_rseg-gjahr. "Comentado devido a MIRO existir mais de uma linha/por item/ 08/02/2024 - AOENNING.
    read table it_rbkp into wa_rbkp with key belnr = wa_rseg-belnr
                                             gjahr = wa_rseg-gjahr binary search.
    wa_saida-belnr = wa_rbkp-belnr.
    wa_saida-gjahr = wa_rbkp-gjahr.
    wa_saida-blart = wa_rbkp-blart.
    wa_saida-usnam = wa_rbkp-usnam.
    wa_saida-cputm = wa_rbkp-cputm.
    wa_saida-xblnr = wa_rbkp-xblnr.
    wa_saida-bukrs = wa_rbkp-bukrs.
    wa_saida-lifnr = wa_rbkp-lifnr.
    wa_saida-waers = wa_rbkp-waers.
    wa_saida-stblg = wa_rbkp-stblg.
    wa_saida-stjah = wa_rbkp-stjah.
    wa_saida-bldat = wa_rbkp-bldat.
    wa_saida-budat = wa_rbkp-budat.
    wa_saida-cpudt = wa_rbkp-cpudt.
    wa_saida-kursf = wa_rbkp-kursf.

*-US 135191-25-06-2024-#140934-RJF-inicio
    read table it_ekbe_m into data(wa_ekbe_m) with key belnr = wa_rseg-belnr
                                                       ebeln = wa_rseg-ebeln.

    if sy-subrc is initial.

      read table it_ekbe_mi into data(wa_ekbe_mi) with key belnr = wa_ekbe_m-lfbnr.
*                                                           bewtp = 'E'.

      if sy-subrc is initial.
        wa_saida-belnr_m = wa_ekbe_mi-belnr.
        wa_saida-cpudt_m = wa_ekbe_mi-cpudt.
        wa_saida-bwart_m = wa_ekbe_mi-bwart.
      else.

        read table it_ekbe_mir into data(wa_ekbe_mir) with key lfbnr = wa_ekbe_m-lfbnr
                                                               ebeln = wa_ekbe_m-ebeln.

        if sy-subrc is initial.
          wa_saida-belnr_m = wa_ekbe_mir-belnr.
          wa_saida-cpudt_m = wa_ekbe_mir-cpudt.
          wa_saida-bwart_m = wa_ekbe_mir-bwart.
        endif.
      endif.
    endif.

*-US 135191-25-06-2024-#140934-RJF-fim

    read table t_tvzbt with key spras = sy-langu
                                zterm = wa_rbkp-zterm
                                binary search.
    if sy-subrc = 0.
      wa_saida-vtext = t_tvzbt-vtext.
    else.
      clear wa_saida-vtext.
    endif.
*      wa_saida-zfbdt = wa_rbkp-zfbdt.
*      wa_saida-zterm = wa_rbkp-zterm.
    wa_saida-zbd1t = wa_rbkp-zbd1t.
    wa_saida-zfbdt = wa_rbkp-zfbdt + wa_saida-zbd1t.

*    ENDIF. "Comentado devido a MIRO existir mais de uma linha/por item/ 08/02/2024 - AOENNING.

*    CLEAR: wa_saida-text1.
*    IF wa_saida-zterm IS NOT INITIAL.
*      SELECT SINGLE text1 FROM t052u INTO wa_saida-text1
*        WHERE spras EQ sy-langu
*          AND zterm EQ wa_saida-zterm
*          AND ztagg EQ space.
*    ENDIF.

    "
    v_belnr = wa_rseg-belnr.
    v_gjahr = wa_rseg-gjahr.
    "

    read table it_lfa1_miro into wa_lfa1_miro with key lifnr = wa_rbkp-lifnr.
    wa_saida-name1_m = wa_lfa1_miro-name1.
    wa_saida-regio_m = wa_lfa1_miro-regio.

    read table it_servico into wa_servico with key ebeln =  wa_rseg-ebeln ebelp =  wa_rseg-ebelp binary search.
    wa_saida-srvpos = wa_servico-srvpos.
    wa_saida-ktext1 = wa_servico-ktext1.

    if ( wa_rseg-wrbtr eq 0 ).

      if ( wa_rseg-shkzg eq 'H' ).
        wa_saida-wrbtr = wa_rseg-bnkan * -1.
      else.
        wa_saida-wrbtr = wa_rseg-bnkan.
      endif.

    else.


      if ( wa_rseg-shkzg eq 'H' ).
        wa_saida-wrbtr = wa_rseg-wrbtr * -1.
      else.
        wa_saida-wrbtr = wa_rseg-wrbtr.
      endif.

    endif.

    "Impostos
    clear: wa_saida-wmwst, wa_saida-bruto, v_wrbtr.
    read table it_rbtx with key belnr = wa_rseg-belnr
                                gjahr = wa_rseg-gjahr
                                mwskz = wa_rseg-mwskz binary search.

    if sy-subrc = 0.
      loop at it_rseg_aux where belnr = wa_rseg-belnr
                          and   gjahr = wa_rseg-gjahr
                          and   mwskz = wa_rseg-mwskz.
        add wa_rseg-wrbtr to v_wrbtr.
      endloop.
      wa_saida-wmwst = ( wa_rseg-wrbtr / v_wrbtr ) * it_rbtx-wmwst.
      if ( wa_rseg-shkzg eq 'H' ).
        multiply wa_saida-wmwst by -1.
      endif.
      wa_saida-bruto = wa_rseg-wrbtr + wa_saida-wmwst.
    endif.

    wa_saida-matnr = wa_rseg-matnr.

    if wa_rseg-matnr is not initial.
      read table it_mara with key matnr = wa_rseg-matnr binary search.
      if sy-subrc = 0.
        wa_saida-mtart = it_mara-mtart.
      endif.
    endif.

    wa_saida-ebeln   = wa_rseg-ebeln.
    wa_saida-buzei   = wa_rseg-buzei.
    wa_saida-ebelp   = wa_rseg-ebelp.
    wa_saida-matnr_p = wa_rseg-matnr.
    wa_saida-werks   = wa_rseg-werks.
    wa_saida-shkzg   = wa_rseg-shkzg.
    wa_saida-mwskz   = wa_rseg-mwskz.
    wa_saida-lfbnr   = wa_rseg-lfbnr.

    if ( wa_rseg-shkzg eq 'H' ).
      wa_saida-menge   = wa_rseg-menge * -1.
    else.
      wa_saida-menge   = wa_rseg-menge.
    endif.

    read table it_ekkn into wa_ekkn with key ebeln = wa_rseg-ebeln
                                             ebelp = wa_rseg-ebelp binary search.

    if wa_ekkn-kostl is not initial and sy-subrc is initial.

      read table it_cskt into wa_cskt with key kostl = wa_ekkn-kostl.
      if sy-subrc is initial.
        wa_saida-ltext   = wa_cskt-ltext.
        wa_saida-kostl   = wa_cskt-kostl.
      else.
        clear wa_cskt.
      endif.
    else.
      clear wa_ekkn.
    endif.

    read table it_ekbe into wa_ekbe with key ebeln = wa_rseg-ebeln
                                             ebelp = wa_rseg-ebelp
                                             belnr = wa_rseg-belnr
                                             gjahr = wa_rbkp-gjahr binary search.

    wa_saida-vgabe = wa_ekbe-vgabe.

    read table it_ekko into wa_ekko with key ebeln = wa_rseg-ebeln binary search.

    wa_saida-bsart  = wa_ekko-bsart.
    wa_saida-ernam  = wa_ekko-ernam.
    wa_saida-aedat  = wa_ekko-aedat.
    wa_saida-ekorg  = wa_ekko-ekorg.
    wa_saida-ekgrp  = wa_ekko-ekgrp.
    wa_saida-inco1  = wa_ekko-inco1.
    wa_saida-inco2  = wa_ekko-inco2.
    wa_saida-bedat  = wa_ekko-bedat.
    wa_saida-angnr  = wa_ekko-angnr.
    wa_saida-ihran  = wa_ekko-ihran.
    wa_saida-zterm  = wa_ekko-zterm.

    clear: wa_saida-text1.
    if wa_saida-zterm is not initial.
      select single text1 from t052u into wa_saida-text1
        where spras eq sy-langu
          and zterm eq wa_saida-zterm
          and ztagg eq space.
    endif.


    if p_bsart is not initial and wa_ekko-bsart not in p_bsart .
      clear:  wa_ekbe, wa_ekko, wa_ekpo, wa_lfa1_pedido, wa_t023t, wa_t024, wa_saida.
      continue.
    endif.
    "VALOR ITEM MIRO
    if ( wa_saida-wrbtr is initial or wa_saida-wrbtr = 0 ) and wa_saida-bsart = 'ZDBP'.
      wa_saida-wrbtr = wa_rbkp-rmwwr.
    endif.

    read table it_konv into wa_konv with key knumv = wa_ekko-knumv
                                             kschl = 'ZSV4' binary search.
    if sy-subrc is initial.
      wa_saida-saving = wa_konv-kbetr / c_kbetr_10.
    endif.

    read table it_konv into wa_konv with key knumv = wa_ekko-knumv
                                             kschl = 'ZSV3' binary search.
    if sy-subrc is initial.
      wa_saida-xzsv3 = wa_konv-kbetr / c_kbetr_10.
    endif.

    read table it_konv into wa_konv with key knumv = wa_ekko-knumv
                                             kschl = 'ZSV4' binary search.
    if sy-subrc is initial.
      wa_saida-xzsv4 = wa_konv-kbetr / c_kbetr_10.
    endif.

*        read table it_ekpo into wa_ekpo with key ebeln = wa_ekko-ebeln.
    read table it_ekpo into wa_ekpo with key ebeln = wa_rseg-ebeln
                                             ebelp = wa_rseg-ebelp binary search.

    wa_saida-loekz      = wa_ekpo-loekz.
    wa_saida-txz01      = wa_ekpo-txz01.
    wa_saida-matnr_p    = wa_ekpo-matnr.
    wa_saida-matkl      = wa_ekpo-matkl.
    wa_saida-lgort      = wa_ekpo-lgort.
    wa_saida-meins      = wa_ekpo-meins.
    wa_saida-banfn      = wa_ekpo-banfn.
    wa_saida-bnfpo      = wa_ekpo-bnfpo.
    wa_saida-menge_p    = wa_ekpo-menge.

    wa_saida-netpr      = wa_ekpo-netpr.
    wa_saida-netwr      = wa_ekpo-netwr.
    wa_saida-konnr      = wa_ekpo-konnr.
    wa_saida-packno     = wa_ekpo-packno.

    read table it_eket with key ebeln = wa_ekpo-ebeln ebelp = wa_ekpo-ebelp.
    if sy-subrc eq 0.
      wa_saida-prdat      = it_eket-eindt.
    endif.



    read table it_lfa1_pedido into wa_lfa1_pedido with key lifnr = wa_ekko-lifnr binary search.

    wa_saida-lifnr_p = wa_lfa1_pedido-lifnr.
    wa_saida-name1_p = wa_lfa1_pedido-name1.
    wa_saida-regio_p = wa_lfa1_pedido-regio.
    if wa_lfa1_pedido-stcd1 is not initial.
      wa_saida-stcd1 = wa_lfa1_pedido-stcd1.
    else.
      wa_saida-stcd1 = wa_lfa1_pedido-stcd2.
    endif.


    read table it_t023t into wa_t023t with key spras = 'PT'
                                               matkl = wa_ekpo-matkl.

    wa_saida-wgbez60 = wa_t023t-wgbez60.


    read table it_t024 into wa_t024 with key ekgrp = wa_ekko-ekgrp.

    wa_saida-eknam = wa_t024-eknam.

    read table it_t001w with key werks = wa_saida-werks.
    if sy-subrc is initial.
      wa_saida-name1 = it_t001w-name1.
    endif.

    "PSA
**********************************************************************155645 AJUSTAR  ZMM0034 colunas PSA
    read table it_lfa1_miro assigning field-symbol(<_lfa1>) with key lifnr = wa_saida-lifnr.

    if <_lfa1>-stcd1 is not initial.
      wa_saida-for_cnpj = <_lfa1>-stcd1.
    endif.

    read table it_eket assigning field-symbol(<_eket>) with key ebeln = wa_saida-ebeln ebelp = wa_saida-ebelp.

    if <_eket>-eindt is not initial.
      wa_saida-for_dt_remessa = <_eket>-eindt.
    endif.
**********************************************************************
    append wa_saida to it_saida.

    clear:  wa_saida, wa_ekbe, wa_ekko, wa_ekpo, wa_lfa1_pedido, wa_t023t, wa_t024, it_eket.
  endloop.

  call screen 0100.

endform.                    " SELECIONA_SAIDA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module pbo output.

  if ( cl_container is initial ).
    perform: create_object.
  endif.


  set pf-status 'PF0100'.
  set titlebar  'TB0100'.


endmodule.                 "
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module pai input.

  case sy-ucomm.
    when: 'BACK'.
      leave to screen 0.
    when: 'CANC'.
      leave list-processing and return to screen 0.
    when: 'EXIT'.
      leave program.
  endcase.

endmodule.                 "
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
form create_object .

  data: wa_event     type ref to lcl_event_handler.

  create object cl_container
    exporting
      container_name              = 'CONTAINER_PRINCIPAL'
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      others                      = 6.

  create object cl_grid
    exporting
      i_parent = cl_container.

  perform: fcat.

  if ( wa_event  is initial ).

    create object wa_event.
    set handler: wa_event->handle_hotspot_click for cl_grid.

  endif.

  wa_variant-report = sy-repid.
  wa_variant-handle = '0100'.

  call method cl_grid->set_table_for_first_display
    exporting
      is_layout                     = wa_layout
      is_variant                    = wa_variant
      i_save                        = 'A'
    changing
      it_outtab                     = it_saida[]
      it_fieldcatalog               = it_fcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.

endform.                    " CREATE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
form fcat .

  perform catalog using:

    " Miro
    'BELNR'   'Doc. MIRO'          '' '' 'X' 'C200' '' '' '',
    'STCD1'   'CNPJ Fornecedor'    '' '' 'X' 'C200' '' '' '',
    'PRDAT'   'Data Remessa'       '' '' 'X' 'C200' '' '' '',
    'GJAHR'   'Ano MIRO'           '' '' '' 'C200'  '' '' '',
    'VGABE'   'Tp. Op.'            '' '' '' 'C200'  '' '' '',
    'BLART'   'Tp Doc. MIRO'       '' '' '' 'C200'  '' '' '',
    'USNAM'   'US MIRO'            '' '' '' 'C200'  '' '' '',
    'CPUTM'   'Hora MIRO'          '' '' '' 'C200'  '' '' '',
    'XBLNR'   'Nr. NF'             '' '' '' 'C200'  '' '' '',
    'BUKRS'   'Empresa'            '' '' '' 'C200'  '' '' '',
    'LIFNR'   'Fornecedor MIRO'    '' '' '' 'C200'  '' '' '',
    'NAME1_M' 'Descr. Forn. MIRO'  '' '' '' 'C200'  '' '' '',
    'REGIO_M' 'UF Forn. Miro'      '' '' '' 'C200'  '' '' '',
    'WAERS'   'Moeda MIRO'         '' '' '' 'C200'  '' '' '',
    'STBLG'   'Doc. Estorno MIRO'  '' '' '' 'C200'  '' '' '',
    'STJAH'   'Ano Doc. Est. MIRO' '' '' '' 'C200'  '' '' '',
    'BLDAT'   'Dt. Doc. MIRO'      '' '' '' 'C200'  '' '' '',
    'BUDAT'   'Dt. Lçto. MIRO'     '' '' '' 'C200'  '' '' '',
    'CPUDT'   'Dt. Criação MIRO'   '' '' '' 'C200'  '' '' '',
    'KURSF'   'Tx. Cambio MIRO'    '' '' '' 'C200'  '' '' '',
    'BUZEI'   'Item'               '' '' '' 'C200'  '' '' '',
    'ANGNR'   'Cotação'              '' '' '' 'C100'  '' '' '',
    'IHRAN'   'Data contação'        '' '' '' 'C100'  '' '' '',

*-US 140934-25-06-2024-#140934-RJF-inicio
    'BELNR_M'   'Nro. Doc. Ref. MIGO'   '' '' '' 'C100'  '' '' '',
    'CPUDT_M'   'Dt.Lçto.MIGO'          '' '' '' 'C100'  '' '' '',
    'BWART_M'   'Tipo de Movimento'     '' '' '' 'C100'  '' '' '',
*-US 140934-25-06-2024-#140934-RJF-fim

    " Pedido
    'EBELN'   'Nr. Pedido'           '' '' 'X' 'C100' '' '' '',
    'EBELP'   'Item Pedido'          '' '' '' 'C100'  '' '' '',
    'KONNR'   'Contrato'             '' '' '' 'C100'  '' '' '',
    'SAVING'  '% Saving Negociação'  '' '' '' 'C100'  '' '' '',
    'XZSV3'   '% Saving Fat.Direto'  '' '' '' 'C100'  '' '' '',
    'XZSV4'   '% Saving Ben.Fiscais' '' '' '' 'C100'  '' '' '',

    'MATNR'   'Material MIRO'       '' 'X' '' 'C200' '' '' '',
    'MTART'   'Tipo Material'       '' 'X' '' 'C100' '' '' '',
    'WERKS'   'Centro'              '' '' '' 'C100'  '' '' '',
    'NAME1'   'Descrição Centro'    '20' '' '' 'C100'  '' '' '',
    'KOSTL'   'Centro de Custo'     '' '' '' 'C200'  '' '' '',
    'LTEXT'   'Centro de Custo Descrição'     '' '' '' 'C200'  '' '' '',
    'SHKZG'   'D/C'                 '' '' '' 'C100'  '' '' '',
    'MWSKZ'   'IVA'                 '' '' '' 'C100'  '' '' '',
    'LFBNR'   'Nro. Doc. Ref. MIRO' '' '' '' 'C200'  '' '' '',
    'MENGE'   'Qtd. MIRO'           '' '' '' 'C200'  '' '' '',
    'WRBTR'   'Valor Item MIRO'     '' '' '' 'C200'  '' '' '',
    'WMWST'   'Imposto Item MIRO'   '' '' '' 'C200'  '' '' '',
    'BRUTO'   'Bruto Item MIRO'     '' '' '' 'C200'  '' '' '',
    'BSART'   'Tipo Pedido'         '' '' '' 'C100'  '' '' '',
    'ERNAM'   'Usuário Pedido'      '' '' '' 'C100'  '' '' '',
    'AEDAT'   'Dta. Criação Pedido' '' '' '' 'C100'  '' '' '',
    'LIFNR_P' 'Fornecedor Pedido'   '' '' '' 'C100'  '' '' '',
    'NAME1_P' 'Descr. Forn. Pedido' '' '' '' 'C100'  '' '' '',
    'EKORG'   'Org. Compras'        '' '' '' 'C100'  '' '' '',
    'EKGRP'   'Cod. Comprador'      '' '' '' 'C100'  '' '' '',
    'EKNAM'   'Nome do Comprador'   '' '' '' 'C100'  '' '' '',
    'WAERS'   'Moeda Pedido'        '' '' '' 'C100'  '' '' '',
    'INCO1'   'Incoterms 1'         '' '' '' 'C100'  '' '' '',
    'INCO2'   'Incoterms 1'         '' '' '' 'C100'  '' '' '',
    'BEDAT'   'Dt. Pedido'          '' '' '' 'C100'  '' '' '',

    'LOEKZ'   'St. Elim. Pedido'    '' '' '' 'C100'  '' '' '',
    'TXZ01'   'Texto Material'      '' '' '' 'C100'  '' '' '',
    'MATNR_P' 'Material Pedido'     '' 'X' '' 'C100' '' '' '',
    'MATKL'   'Grp. Mercadoria'     '' '' '' 'C100'  '' '' '',
    'WGBEZ60' 'Denominação'         '' '' '' 'C100'  '' '' '',
    'LGORT'   'Deposito'            '' '' '' 'C100'  '' '' '',
    'MEINS'   'Un. Medida'          '' '' '' 'C100'  '' '' 'CUNIT',
    'BANFN'   'Requisição Compr.'   '' '' '' 'C100'  '' '' '',
    'BNFPO'   'Item'                '' '' '' 'C100'  '' '' '',
    'MENGE_P'  'Qtd. Pedido'       '' '' '' 'C100'  '' '' '',
    'NETPR'   'Vlr. Unit. Pedido'   '' '' '' 'C100'  '' '' '',
    'NETWR'   'Vlr. Liquido Pedido' '' '' '' 'C100'  '' '' '',

*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
'ZTERM'   'Cond.pagamento PC'   '' '' '' 'C100'  'X' '' '',
'TEXT1'   'Desc.cond pagamento PC' '' '' '' 'C100'  'X' '' '',
*'VTEXT'   'Descr.Cond.' '' '' '' 'C100'  'X' '' '',
'ZBD1T'   'Prazo'       '' '' '' 'C100'  'X' '' '',
'ZFBDT'   'Data Vencimento Miro'  '12' '' '' 'C100'  'X' '' '',
"Código e descrição de Serviço
'SRVPOS'   'Código do Serviço'  '12' '' '' 'C100'  'X' '' '',
'KTEXT1'   'Descrição do Serviço'  '12' '' '' 'C100'  'X' '' '',
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM

**********************************************************************155645 AJUSTAR  ZMM0034 colunas PSA
    'FOR_CNPJ'         'For. CNPJ'            '' '' '' ''  '' '' '',
    'FOR_DT_REMESSA'   'For. Dt Remessa'      '' '' '' ''  '' '' ''.
**********************************************************************

endform.                    " FCAT
*&---------------------------------------------------------------------*
*&      Form  CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form catalog   using    value(p_fieldname)
                        value(p_desc)
                        value(p_tam)
                        value(p_no_zero)
                        value(p_hotspot)
                        value(p_cor)
                        value(p_just)
                        value(p_sum)
                        value(p_exit).
  clear: wa_fcat.
  wa_fcat-tabname   = 'IT_SAIDA'.
  wa_fcat-fieldname = p_fieldname.
  wa_fcat-scrtext_l = p_desc.
  wa_fcat-scrtext_m = p_desc.
  wa_fcat-scrtext_s = p_desc.
  wa_fcat-outputlen = p_tam.
  wa_fcat-no_zero   = p_no_zero.
  wa_fcat-hotspot   = p_hotspot.
  wa_fcat-emphasize = p_cor.
  wa_fcat-just      = p_just.
  wa_fcat-do_sum    = p_sum.
  wa_fcat-convexit  = p_exit.

  append wa_fcat to it_fcat.

endform.                    " CATALOG
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
form handle_hotspot_click   using   p_e_row_id
                                    p_e_column_id
                                    p_es_row_no.


  data: opt type ctu_params.


  read table it_saida into wa_saida index p_e_row_id.

  if not ( wa_saida is initial ).

    case p_e_column_id.

      when: 'BELNR'.

        set parameter id 'RBN' field wa_saida-belnr.
        set parameter id 'GJR' field wa_saida-gjahr.
        call transaction  'MIR4' and skip first screen.

      when: 'EBELN'.
        clear: t_bdc[], t_messtab.
        perform f_bdc_field using: 'X' 'SAPLMEGUI'           '0014'             ,
                                   ' ' 'BDC_OKCODE'	         '=MECHOB'          ,
                                   ' ' 'DYN_6000-LIST'       '1'                ,
                                   'X' 'SAPLMEGUI'           '0002'             ,
                                   ' ' 'BDC_OKCODE'	         '=MEOK'            ,
                                   ' ' 'BDC_SUBSCR'	         'SAPLMEGUI'        ,
                                   ' ' 'BDC_CURSOR'	         'MEPO_SELECT-EBELN',
                                   ' ' 'MEPO_SELECT-EBELN'   wa_saida-ebeln     ,
                                   ' ' 'MEPO_SELECT-BSTYP_F' 'X'.

        opt-dismode = 'E'.
        opt-defsize = 'X'.
        call transaction 'ME23N' using t_bdc options from opt messages into t_messtab.


    endcase.

  endif.
endform.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
form f_bdc_field  using    value(p_flag)
                           value(p_fnam)
                           value(p_fval).

  clear t_bdc.
  if not p_flag is initial.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  else.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  endif.
  append t_bdc.

endform.                    " F_BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  ZF_KSCHL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_kschl .

  refresh: r_kschl.

  r_kschl-sign   = 'I'.
  r_kschl-option = 'EQ'.
  r_kschl-low    = 'ZSV1'.
  append r_kschl.

  r_kschl-sign   = 'I'.
  r_kschl-option = 'EQ'.
  r_kschl-low    = 'ZSV2'.
  append r_kschl.

  r_kschl-sign   = 'I'.
  r_kschl-option = 'EQ'.
  r_kschl-low    = 'ZSV3'.
  append r_kschl.

endform.
