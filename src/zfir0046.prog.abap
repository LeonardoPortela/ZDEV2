*&---------------------------------------------------------------------*
*& Report  ZFIR0046
*&
*&---------------------------------------------------------------------*
*&TITULO: Geração Lançamentos Manuais - Auditoria
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 19.03.2014
*TRANSACAO: ZFI0048
*&---------------------------------------------------------------------*
*&MODIFICAÇÕES:
*&Developer: Enio Jesus | Data: 13.10.2015 | Request: DEVK950972
*&---------------------------------------------------------------------*
report  zfir0046.


*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools:truxs.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: bkpf.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

types:
  begin of ty_bkpf,
    bukrs type bkpf-bukrs,
    gjahr type bkpf-gjahr,
    budat type bkpf-budat,
    tcode type bkpf-tcode,
    blart type bkpf-blart,
    bldat type bkpf-bldat,
    bktxt type bkpf-bktxt,
    waers type bkpf-waers,
    xblnr type bkpf-xblnr,
    usnam type bkpf-usnam,
    cpudt type bkpf-cpudt,
    cputm type bkpf-cputm,
    sname type bkpf-sname,
    aedat type bkpf-aedat,
    upddt type bkpf-upddt,
    xrueb type bkpf-xrueb,
    psodt type bkpf-psodt,
    monat type bkpf-monat,
    belnr type bkpf-belnr,
    stblg type bkpf-stblg, "Documento estornado
  end of ty_bkpf,

  begin of ty_faglflexa,
    ryear  type faglflexa-ryear,
    docnr  type faglflexa-docnr,
    rbukrs type faglflexa-rbukrs,
    rldnr  type faglflexa-rldnr,
    racct  type faglflexa-racct,
    rcntr  type faglflexa-rcntr,
    prctr  type faglflexa-prctr,
    buzei  type faglflexa-buzei,
    bschl  type faglflexa-bschl,
    tsl    type faglflexa-tsl,
    hsl    type faglflexa-hsl,
    ksl    type faglflexa-ksl,
    drcrk  type faglflexa-drcrk,
  end of ty_faglflexa,

  begin of ty_bsas,
    bukrs type bukrs,
    belnr type belnr,
    buzei type buzei,
    gjahr type gjahr,
    buzid type buzid,
    gsber type gsber,
    valut type valut,
    pswsl type pswsl,
    sgtxt type sgtxt,
  end of ty_bsas,

  begin of ty_skb1,
    bukrs type skb1-bukrs,
    saknr type skb1-saknr,
    mitkz type skb1-mitkz,
  end of ty_skb1,

  begin of ty_linha,
    bukrs(04),
    pontov1(1),
    belnr(10),
    pontov2(1),
    gjahr(4),
    pontov3(1),
    blart(2),
    pontov4(1),
    bldat(10),
    pontov5(1),
    budat(10),
    pontov6(1),
    bktxt(25),
    pontov7(1),
    waers(5),
    pontov8(1),
    xblnr(16),
    pontov9(1),
    usnam(12),
    pontov10(1),
    tcode(20),
    pontov11(1),
    cpudt(10),
    pontov12(1),
    cputm(10),
    pontov13(1),
    sname(12),
    pontov14(1),
    aedat(10),
    pontov15(1),
    upddt(10),
    pontov16(1),
    xrueb(1),
    pontov17(1),
    psodt(10),
    pontov18(1),
    monat(2),
    pontov19(1),
    buzei(3),
    pontov20(1),
    buzid(1),
    pontov21(1),
    racct(10),
    pontov22(1),
    mitkz(1),
    pontov23(1),
    prctr(10),
    pontov24(1),
    rcntr(10),
    pontov25(1),
    gsber(4),
    pontov26(1),
    valut(10),
    pontov27(1),
    tsl(25),
    pontov28(1),
    hsl(25),
    pontov29(1),
    ksl(25),
    pontov30(1),
    pswsl(5),
    pontov31(1),
    bschl(2),
    pontov32(1),
    sgtxt(51),
    pontov33(1),
    drcrk(1),
    pontov34(1),
    augbl(10),
    pontov35(1),
    augdt(10),
    pontov36(1),
    stblg(20),  "Documento estornado
  end of ty_linha,

  begin of ty_cabec,
    bukrs(20), "Empresa
    pontov1(1),
    belnr(20), "Documento
    pontov2(1),
    gjahr(20), "Ano
    pontov3(1),
    blart(20), "Tipo Doc.
    pontov4(1),
    bldat(20), "Data Doc.
    pontov5(1),
    budat(20), "Data Lcto
    pontov6(1),
    bktxt(20), "Cab.Documento
    pontov7(1),
    waers(20), "Moeda
    pontov8(1),
    xblnr(20), "Nº Referência
    pontov9(1),
    usnam(20), "Usuário
    pontov10(1),
    tcode(20), "Cod.Transação
    pontov11(1),
    cpudt(20), "Dt Entrada Doc
    pontov12(1),
    cputm(20), "Hora Entrada Doc
    pontov13(1),
    sname(20), "Usuário
    pontov14(1),
    aedat(20), "Data Mod. Doc
    pontov15(1),
    upddt(20), "Data Atual. Doc
    pontov16(1),
    xrueb(20), "Cod. Reg. Precedente
    pontov17(1),
    psodt(20), "Dt ultima modif.
    pontov18(1),
    monat(20), "Mês
    pontov19(1),
    buzei(20), "Nº linha Lcto
    pontov20(1),
    buzid(20), "Ident. Linha Lcto
    pontov21(1),
    racct(20), "Nº Conta
    pontov22(1),
    mitkz(20), "Conta Conciliação
    pontov23(1),
    prctr(20), "Centro Lucro
    pontov24(1),
    rcntr(20), "Centro Custo
    pontov25(1),
    gsber(15), "Divisão
    pontov26(1),
    valut(20), "Data efetiva
    pontov27(1),
    tsl(20), "Moeda Transação
    pontov28(1),
    hsl(20), "Moeda Interna
    pontov29(1),
    ksl(20), "Moeda Grupo
    pontov30(1),
    pswsl(20), "Moeda Atualização
    pontov31(1),
    bschl(15), "Chave Lcto
    pontov32(1),
    sgtxt(51), "Texto Item
    pontov33(1),
    drcrk(15), "Débito/Credito
    pontov34(1),
    augbl(20), "Doc. Compensação
    pontov35(1),
    augdt(20), "Data Compensação
    pontov36(1),
    stblg(20),  "Documento estornado
  end of ty_cabec,


*  TY_ARQUIVO(425) TYPE C,
*  ty_arquivo(825) TYPE c,
  ty_arquivo type string,

  begin of ty_saida,
    bukrs type bkpf-bukrs,
    belnr type bkpf-belnr,
    gjahr type bkpf-gjahr,
    blart type bkpf-blart,
    bldat type bkpf-bldat,
    budat type bkpf-budat,
    bktxt type bkpf-bktxt,
    waers type bkpf-waers,
    xblnr type bkpf-xblnr,
    usnam type bkpf-usnam,
    tcode type bkpf-tcode,
    cpudt type bkpf-cpudt,
    cputm type bkpf-cputm,
    sname type bkpf-sname,
    aedat type bkpf-aedat,
    upddt type bkpf-upddt,
    xrueb type bkpf-xrueb,
    psodt type bkpf-psodt,
    monat type bkpf-monat,
    buzei type faglflexa-buzei,
    buzid type buzid,
    racct type faglflexa-racct,
    mitkz type skb1-mitkz,
    prctr type faglflexa-prctr,
    rcntr type faglflexa-rcntr,
    gsber type gsber,
    valut type valut,
    tsl   type faglflexa-tsl,
    hsl   type faglflexa-hsl,
    ksl   type faglflexa-ksl,
    pswsl type pswsl,
    bschl type faglflexa-bschl,
    sgtxt type sgtxt,
    drcrk type faglflexa-drcrk,
    stblg type bkpf-stblg, "Documento estornado
  end of ty_saida,

  begin of ty_saida2,
    bukrs  type bkpf-bukrs,
    belnr  type bkpf-belnr,
    gjahr  type bkpf-gjahr,
    blart  type bkpf-blart,
    bldat  type bkpf-bldat,
    budat  type bkpf-budat,
    bktxt  type bkpf-bktxt,
    waers  type bkpf-waers,
    xblnr  type bkpf-xblnr,
    usnam  type bkpf-usnam,
    tcode  type bkpf-tcode,
    cpudt  type bkpf-cpudt,
    cputm  type bkpf-cputm,
    sname  type bkpf-sname,
    aedat  type bkpf-aedat,
    upddt  type bkpf-upddt,
    xrueb  type bkpf-xrueb,
    psodt  type bkpf-psodt,
    monat  type bkpf-monat,
    buzei  type faglflexa-buzei,
    racct  type faglflexa-racct,
    mitkz  type skb1-mitkz,
    prctr  type faglflexa-prctr,
    rcntr  type faglflexa-rcntr,
    tsl    type faglflexa-tsl,
    hsl    type faglflexa-hsl,
    ksl    type faglflexa-ksl,
    bschl  type faglflexa-bschl,
    drcrk  type faglflexa-drcrk,
    buzid  type buzid,
    gsber  type gsber,
    valut  type valut,
    pswsl  type pswsl,
    sgtxt  type sgtxt,
    buzida type buzid,
    gsbera type gsber,
    valuta type valut,
    pswsla type pswsl,
    sgtxta type sgtxt,
    augbl  type augbl,
    augdt  type augdt,
    stblg  type stblg, "Documento estornado
    sgtxtt type sgtxt,
  end of ty_saida2.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
data:
  it_bkpf      type table of ty_bkpf,
  it_faglflexa type table of ty_faglflexa,
  it_bsas      type table of ty_bsas,
  it_bsis      type table of ty_bsas,
  it_skb1      type table of ty_skb1,
  t_arquivo    type table of ty_arquivo,
  it_saida     type table of ty_saida,
  it_saida2    type table of ty_saida2.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
data:
  wa_bkpf      type ty_bkpf,
  wa_faglflexa type ty_faglflexa,
  wa_bsas      type ty_bsas,
  wa_bsis      type ty_bsas,
  wa_skb1      type ty_skb1,
  w_arquivo    type ty_arquivo,
  w_linha      type ty_linha,
  w_cabec      type ty_cabec,
  wa_saida     type ty_saida,
  wa_saida2    type ty_saida2.



*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.

  select-options:
                 p_bukrs  for bkpf-bukrs  no-extension no intervals obligatory, " Empresa
                 p_gjahr  for bkpf-gjahr  no-extension no intervals,
                 p_budat  for bkpf-budat  no-extension obligatory,
                 p_blart  for bkpf-blart,
                 p_tcode  for bkpf-tcode.

  parameters: p_path(250) default 'C:\' lower case obligatory.



selection-screen: end of block b1.

selection-screen begin of block c with frame title text-003.
  parameters: r_local radiobutton group 1
                      default 'X'
                      user-command scr,
              r_unix  radiobutton group 1.
selection-screen end of block c.


selection-screen begin of block b4 with frame title text-004.
  selection-screen begin of line.
*SELECTION-SCREEN COMMENT (13) text-001.
    parameters p_nor as checkbox user-command check1 default 'X'.
    selection-screen comment (13) text-006.

    parameters p_est as checkbox user-command check1 default 'X'.
    selection-screen comment (15) text-007.

  selection-screen end of line.
selection-screen end of block b4.



at selection-screen output.
  if r_local = 'X'.
    "  P_PATH = 'C:\'.
  elseif p_path(1) ne '/'.
    p_path = '/usr/sap/interfaces2/'.
  endif.

at selection-screen on value-request for p_path.

  if r_local = 'X'.
    call function 'WS_FILENAME_GET'
      exporting
        def_filename     = ' '
        def_path         = 'C:\'
        mask             = '*.CSV'
        mode             = 'S'
        title            = 'Busca de Arquivo'
      importing
        filename         = p_path
      exceptions
        inv_winsys       = 1
        no_batch         = 2
        selection_cancel = 3
        selection_error  = 4
        others           = 5.
  else.
    p_path = '/usr/sap/interfaces2/'.
  endif.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
start-of-selection.

  if p_nor is initial and p_est is initial.
    message i024(sd) with 'Selecionar status do documento'.
    exit.
  endif.

  perform: f_seleciona_dados,
           f_cria_arq.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados.

*-US 128512-25-06-2024-#128512-RJF-inicio - Melhoria Performance ZFI0048
  try.
*-US 128512-25-06-2024-#128512-RJF-fim - Melhoria Performance ZFI0048

      "Se opção com estorno e sem estorno estiver marcado.
      if p_nor is not initial and p_est is not initial.
*    SELECT
*          bkpf~bukrs
*          bkpf~belnr
*          bkpf~gjahr
*          bkpf~blart
*          bkpf~bldat
*          bkpf~budat
*          bkpf~bktxt
*          bkpf~waers
*          bkpf~xblnr
*          bkpf~usnam
*          bkpf~tcode
*          bkpf~cpudt
*          bkpf~cputm
*          bkpf~sname
*          bkpf~aedat
*          bkpf~upddt
*          bkpf~xrueb
*          bkpf~psodt
*          bkpf~monat
*          faglflexa~buzei
*          faglflexa~racct
*          skb1~mitkz
*          faglflexa~prctr
*          faglflexa~rcntr
*          faglflexa~tsl
*          faglflexa~hsl
*          faglflexa~ksl
*          faglflexa~bschl
*          faglflexa~drcrk
*          bsis~buzid
*          bsis~gsber
*          bsis~valut
*          bsis~pswsl
*          bsis~sgtxt
*          bsas~buzid
*          bsas~gsber
*          bsas~valut
*          bsas~pswsl
*          bsas~sgtxt
*          bsas~augbl
*          bsas~augdt
*          bkpf~stblg
*          bseg~sgtxt
*       FROM bkpf
*       INNER JOIN faglflexa
*        ON   faglflexa~ryear  EQ bkpf~gjahr
*        AND  faglflexa~docnr  EQ bkpf~belnr
*        AND  faglflexa~rbukrs EQ bkpf~bukrs
*        AND  faglflexa~rldnr  EQ  '0L'
*       LEFT OUTER JOIN bsis
*       ON  faglflexa~rbukrs EQ bsis~bukrs
*       AND faglflexa~docnr  EQ bsis~belnr
*       AND faglflexa~buzei  EQ bsis~buzei
*       AND faglflexa~ryear  EQ bsis~gjahr
*       LEFT OUTER JOIN bsas
*       ON  faglflexa~rbukrs EQ bsas~bukrs
*       AND faglflexa~docnr  EQ bsas~belnr
*       AND faglflexa~buzei  EQ bsas~buzei
*       AND faglflexa~ryear  EQ bsas~gjahr
*       LEFT OUTER JOIN skb1
*       ON  faglflexa~rbukrs EQ skb1~bukrs
*       AND faglflexa~racct  EQ skb1~saknr
*      LEFT OUTER JOIN bseg
*       ON  faglflexa~rbukrs EQ bseg~bukrs
*       AND faglflexa~docnr  EQ bseg~belnr
*       AND faglflexa~ryear  EQ bseg~gjahr
*      INTO TABLE it_saida2
*      WHERE bkpf~bukrs  IN p_bukrs
*      AND   bkpf~gjahr  IN p_gjahr
*      AND   bkpf~budat  IN p_budat
*      AND   bkpf~tcode  IN p_tcode
*      AND   bkpf~blart  IN p_blart.


*-US 128512-25-06-2024-#128512-RJF-inicio - Melhoria Performance ZFI0048
        select
              bkpf~bukrs,
              bkpf~belnr,
              bkpf~gjahr,
              bkpf~blart,
              bkpf~bldat,
              bkpf~budat,
              bkpf~bktxt,
              bkpf~waers,
              bkpf~xblnr,
              bkpf~usnam,
              bkpf~tcode,
              bkpf~cpudt,
              bkpf~cputm,
              bkpf~sname,
              bkpf~aedat,
              bkpf~upddt,
              bkpf~xrueb,
              bkpf~psodt,
              bkpf~monat,
              bkpf~stblg,
              faglflexa~buzei,
              faglflexa~racct,
              faglflexa~prctr,
              faglflexa~rcntr,
              faglflexa~tsl,
              faglflexa~hsl,
              faglflexa~ksl,
              faglflexa~bschl,
              faglflexa~drcrk
          from bkpf
           inner join faglflexa
            on   faglflexa~ryear  eq bkpf~gjahr
            and  faglflexa~docnr  eq bkpf~belnr
            and  faglflexa~rbukrs eq bkpf~bukrs
          into table @data(it_saidabkpf)
          where bkpf~bukrs       in @p_bukrs
          and   bkpf~gjahr       in @p_gjahr
          and   bkpf~budat       in @p_budat
          and   bkpf~tcode       in @p_tcode
          and   bkpf~blart       in @p_blart
          and   faglflexa~rldnr  eq  '0L'.

        if sy-subrc is initial and it_saidabkpf[] is not initial.
          sort it_saidabkpf by bukrs
                               belnr
                               buzei
                               gjahr.

          select bsis~bukrs,
                 bsis~belnr,
                 bsis~buzei,
                 bsis~gjahr,
                 bsis~buzid,
                 bsis~gsber,
                 bsis~valut,
                 bsis~pswsl,
                 bsis~sgtxt,
                 bsas~buzid as buzida,
                 bsas~gsber as gsbera,
                 bsas~valut as valuta,
                 bsas~pswsl as pswsla,
                 bsas~sgtxt as sgtxta,
                 bsas~augbl,
                 bsas~augdt
                 from bsis as bsis
                 left outer join bsas as bsas
                  on bsis~bukrs  eq bsas~bukrs
                 and bsis~belnr  eq bsas~belnr
                 and bsis~buzei  eq bsas~buzei
                 and bsis~gjahr  eq bsas~gjahr
                 into table @data(it_saidabsis)
                for all entries in @it_saidabkpf
          where
              bsis~bukrs eq @it_saidabkpf-bukrs
          and bsis~belnr eq @it_saidabkpf-belnr
          and bsis~buzei eq @it_saidabkpf-buzei
          and bsis~gjahr eq @it_saidabkpf-gjahr.

          if sy-subrc is initial.
            sort it_saidabsis by  bukrs
                                  belnr
                                  buzei
                                  gjahr.
          endif.

          select bseg~bukrs,
                 bseg~belnr,
                 bseg~buzei,
                 bseg~gjahr,
                 bseg~sgtxt,
                 skb1~saknr,
                 skb1~mitkz
                 from bseg
          left outer join skb1
           on  bseg~bukrs eq skb1~bukrs
           into table @data(it_saidabseg)
           for all entries in @it_saidabkpf
          where
              bseg~bukrs eq @it_saidabkpf-bukrs
          and bseg~belnr eq @it_saidabkpf-belnr
          and bseg~buzei eq @it_saidabkpf-buzei
          and bseg~gjahr eq @it_saidabkpf-gjahr
          and skb1~saknr eq @it_saidabkpf-racct.

          if sy-subrc is initial.
            sort it_saidabseg by  bukrs
                                  belnr
                                  buzei
                                  gjahr
                                  saknr.

          endif.

          loop at it_saidabkpf assigning field-symbol(<fs_saidabkpf>).
            append initial line to it_saida2 assigning field-symbol(<fs_saida2>).
            <fs_saida2> = corresponding #( <fs_saidabkpf> ).
            read table it_saidabsis assigning field-symbol(<fs_saidabsis>) with key bukrs = <fs_saidabkpf>-bukrs
                                                                                    belnr = <fs_saidabkpf>-belnr
                                                                                    buzei = <fs_saidabkpf>-buzei
                                                                                    gjahr = <fs_saidabkpf>-gjahr
                                                                           binary search.
            if sy-subrc is initial.
*              <fs_saida2> = corresponding #( <fs_saidabsis> ). "BUG SOLTO 155616 / AOENNING
              move-corresponding  <fs_saidabsis> to <fs_saida2>. "BUG SOLTO 155616 / AOENNING
            endif.
            read table it_saidabseg assigning field-symbol(<fs_saidabseg>) with key bukrs = <fs_saidabkpf>-bukrs
                                                                                    belnr = <fs_saidabkpf>-belnr
                                                                                    buzei = <fs_saidabkpf>-buzei
                                                                                    gjahr = <fs_saidabkpf>-gjahr
                                                                                    saknr = <fs_saidabkpf>-racct
                                                                           binary search.
            if sy-subrc is initial.
*              <fs_saida2> = corresponding #( <fs_saidabseg> ). "BUG SOLTO 155616 / AOENNING
              move-corresponding <fs_saidabseg> to <fs_saida2>. "BUG SOLTO 155616 / AOENNING
            endif.
          endloop.
        endif.
*-US 128512-25-06-2024-#128512-RJF-fim - Melhoria Performance ZFI0048

      elseif p_nor is not initial and  p_est is initial.

*    SELECT
*              bkpf~bukrs
*              bkpf~belnr
*              bkpf~gjahr
*              bkpf~blart
*              bkpf~bldat
*              bkpf~budat
*              bkpf~bktxt
*              bkpf~waers
*              bkpf~xblnr
*              bkpf~usnam
*              bkpf~tcode
*              bkpf~cpudt
*              bkpf~cputm
*              bkpf~sname
*              bkpf~aedat
*              bkpf~upddt
*              bkpf~xrueb
*              bkpf~psodt
*              bkpf~monat
*              faglflexa~buzei
*              faglflexa~racct
*              skb1~mitkz
*              faglflexa~prctr
*              faglflexa~rcntr
*              faglflexa~tsl
*              faglflexa~hsl
*              faglflexa~ksl
*              faglflexa~bschl
*              faglflexa~drcrk
*              bsis~buzid
*              bsis~gsber
*              bsis~valut
*              bsis~pswsl
*              bsis~sgtxt
*              bsas~buzid
*              bsas~gsber
*              bsas~valut
*              bsas~pswsl
*              bsas~sgtxt
*              bsas~augbl
*              bsas~augdt
*              bkpf~stblg
*              bseg~sgtxt
*           FROM bkpf
*           INNER JOIN faglflexa
*            ON   faglflexa~ryear  EQ bkpf~gjahr
*            AND  faglflexa~docnr  EQ bkpf~belnr
*            AND  faglflexa~rbukrs EQ bkpf~bukrs
*            AND  faglflexa~rldnr  EQ  '0L'
*           LEFT OUTER JOIN bsis
*           ON  faglflexa~rbukrs EQ bsis~bukrs
*           AND faglflexa~docnr  EQ bsis~belnr
*           AND faglflexa~buzei  EQ bsis~buzei
*           AND faglflexa~ryear  EQ bsis~gjahr
*           LEFT OUTER JOIN bsas
*           ON  faglflexa~rbukrs EQ bsas~bukrs
*           AND faglflexa~docnr  EQ bsas~belnr
*           AND faglflexa~buzei  EQ bsas~buzei
*           AND faglflexa~ryear  EQ bsas~gjahr
*           LEFT OUTER JOIN skb1        "#EC CI_DB_OPERATION_OK[2431747]
*           ON  faglflexa~rbukrs EQ skb1~bukrs
*           AND faglflexa~racct  EQ skb1~saknr
*           LEFT OUTER JOIN bseg
*            ON  faglflexa~rbukrs EQ bseg~bukrs
*            AND faglflexa~docnr  EQ bseg~belnr
*            AND faglflexa~ryear  EQ bseg~gjahr
*          INTO TABLE it_saida2
*          WHERE bkpf~bukrs  IN p_bukrs
*          AND   bkpf~gjahr  IN p_gjahr
*          AND   bkpf~budat  IN p_budat
*          AND   bkpf~tcode  IN p_tcode
*          AND   bkpf~blart  IN p_blart.
**          AND   bkpf~stblg  EQ space.
*
*    SORT it_saida2 BY stblg.
*    DELETE it_saida2 WHERE stblg NE space.
*
*    SORT it_saida2 BY belnr.


*-US 128512-25-06-2024-#128512-RJF-inicio - Melhoria Performance ZFI0048
        select
              bkpf~bukrs,
              bkpf~belnr,
              bkpf~gjahr,
              bkpf~blart,
              bkpf~bldat,
              bkpf~budat,
              bkpf~bktxt,
              bkpf~waers,
              bkpf~xblnr,
              bkpf~usnam,
              bkpf~tcode,
              bkpf~cpudt,
              bkpf~cputm,
              bkpf~sname,
              bkpf~aedat,
              bkpf~upddt,
              bkpf~xrueb,
              bkpf~psodt,
              bkpf~monat,
              bkpf~stblg,
              faglflexa~buzei,
              faglflexa~racct,
              faglflexa~prctr,
              faglflexa~rcntr,
              faglflexa~tsl,
              faglflexa~hsl,
              faglflexa~ksl,
              faglflexa~bschl,
              faglflexa~drcrk
          from bkpf
           inner join faglflexa
            on   faglflexa~ryear  eq bkpf~gjahr
            and  faglflexa~docnr  eq bkpf~belnr
            and  faglflexa~rbukrs eq bkpf~bukrs
          into table @it_saidabkpf
          where bkpf~bukrs       in @p_bukrs
          and   bkpf~gjahr       in @p_gjahr
          and   bkpf~budat       in @p_budat
          and   bkpf~tcode       in @p_tcode
          and   bkpf~blart       in @p_blart
          and   faglflexa~rldnr  eq  '0L'.

        if sy-subrc is initial and it_saidabkpf[] is not initial.
*** Stefanini - 2000043780 - 30/05/2025 - FINC - Início de Alteração
"          delete it_saidabkpf[] where stblg ne space.
*** Stefanini - 2000043780 - 30/05/2025 - FINC - Fim de Alteração
          sort it_saidabkpf by bukrs
                               belnr
                               buzei
                               gjahr.
          if it_saidabkpf[] is not initial.

            select bsis~bukrs,
                   bsis~belnr,
                   bsis~buzei,
                   bsis~gjahr,
                   bsis~buzid,
                   bsis~gsber,
                   bsis~valut,
                   bsis~pswsl,
                   bsis~sgtxt,
                   bsas~buzid as buzida,
                   bsas~gsber as gsbera,
                   bsas~valut as valuta,
                   bsas~pswsl as pswsla,
                   bsas~sgtxt as sgtxta,
                   bsas~augbl,
                   bsas~augdt
                   from bsis as bsis
                   left outer join bsas as bsas
                    on bsis~bukrs  eq bsas~bukrs
                   and bsis~belnr  eq bsas~belnr
                   and bsis~buzei  eq bsas~buzei
                   and bsis~gjahr  eq bsas~gjahr
                   into table @it_saidabsis
                  for all entries in @it_saidabkpf
            where
                bsis~bukrs eq @it_saidabkpf-bukrs
            and bsis~belnr eq @it_saidabkpf-belnr
            and bsis~buzei eq @it_saidabkpf-buzei
            and bsis~gjahr eq @it_saidabkpf-gjahr.

            if sy-subrc is initial.
              sort it_saidabsis by  bukrs
                                    belnr
                                    buzei
                                    gjahr.
            endif.

            select bseg~bukrs,
                   bseg~belnr,
                   bseg~buzei,
                   bseg~gjahr,
                   bseg~sgtxt,
                   skb1~saknr,
                   skb1~mitkz
                   from bseg
            left outer join skb1
             on  bseg~bukrs eq skb1~bukrs
             into table @it_saidabseg
             for all entries in @it_saidabkpf
            where
                bseg~bukrs eq @it_saidabkpf-bukrs
            and bseg~belnr eq @it_saidabkpf-belnr
            and bseg~buzei eq @it_saidabkpf-buzei
            and bseg~gjahr eq @it_saidabkpf-gjahr
            and skb1~saknr eq @it_saidabkpf-racct.

            if sy-subrc is initial.
              sort it_saidabseg by  bukrs
                                    belnr
                                    buzei
                                    gjahr
                                    saknr.

            endif.

          endif.

          loop at it_saidabkpf assigning <fs_saidabkpf>.
            append initial line to it_saida2 assigning <fs_saida2>.
            <fs_saida2> = corresponding #( <fs_saidabkpf> ).
            read table it_saidabsis assigning <fs_saidabsis> with key bukrs = <fs_saidabkpf>-bukrs
                                                                                    belnr = <fs_saidabkpf>-belnr
                                                                                    buzei = <fs_saidabkpf>-buzei
                                                                                    gjahr = <fs_saidabkpf>-gjahr
                                                                           binary search.
            if sy-subrc is initial.
*              <fs_saida2> = CORRESPONDING #( <fs_saidabsis> ). "BUG SOLTO 155616 / AOENNING
              move-corresponding <fs_saidabsis> to <fs_saida2>. "BUG SOLTO 155616 / AOENNING
            endif.
            read table it_saidabseg assigning <fs_saidabseg> with key bukrs = <fs_saidabkpf>-bukrs
                                                                                    belnr = <fs_saidabkpf>-belnr
                                                                                    buzei = <fs_saidabkpf>-buzei
                                                                                    gjahr = <fs_saidabkpf>-gjahr
                                                                                    saknr = <fs_saidabkpf>-racct
                                                                           binary search.
            if sy-subrc is initial.
*              <fs_saida2> = corresponding #( <fs_saidabseg> ). "BUG SOLTO 155616 / AOENNING
              move-corresponding <fs_saidabseg> to <fs_saida2>. "BUG SOLTO 155616 / AOENNING
            endif.
          endloop.
        endif.
*-US 128512-25-06-2024-#128512-RJF-fim - Melhoria Performance ZFI0048



      elseif p_nor is initial and  p_est is not initial.
*    SELECT
*              bkpf~bukrs
*              bkpf~belnr
*              bkpf~gjahr
*              bkpf~blart
*              bkpf~bldat
*              bkpf~budat
*              bkpf~bktxt
*              bkpf~waers
*              bkpf~xblnr
*              bkpf~usnam
*              bkpf~tcode
*              bkpf~cpudt
*              bkpf~cputm
*              bkpf~sname
*              bkpf~aedat
*              bkpf~upddt
*              bkpf~xrueb
*              bkpf~psodt
*              bkpf~monat
*              faglflexa~buzei
*              faglflexa~racct
*              skb1~mitkz
*              faglflexa~prctr
*              faglflexa~rcntr
*              faglflexa~tsl
*              faglflexa~hsl
*              faglflexa~ksl
*              faglflexa~bschl
*              faglflexa~drcrk
*              bsis~buzid
*              bsis~gsber
*              bsis~valut
*              bsis~pswsl
*              bsis~sgtxt
*              bsas~buzid
*              bsas~gsber
*              bsas~valut
*              bsas~pswsl
*              bsas~sgtxt
*              bsas~augbl
*              bsas~augdt
*              bkpf~stblg
*              bseg~sgtxt
*           FROM bkpf
*           INNER JOIN faglflexa
*            ON   faglflexa~ryear  EQ bkpf~gjahr
*            AND  faglflexa~docnr  EQ bkpf~belnr
*            AND  faglflexa~rbukrs EQ bkpf~bukrs
*            AND  faglflexa~rldnr  EQ  '0L'
*           LEFT OUTER JOIN bsis
*           ON  faglflexa~rbukrs EQ bsis~bukrs
*           AND faglflexa~docnr  EQ bsis~belnr
*           AND faglflexa~buzei  EQ bsis~buzei
*           AND faglflexa~ryear  EQ bsis~gjahr
*           LEFT OUTER JOIN bsas
*           ON  faglflexa~rbukrs EQ bsas~bukrs
*           AND faglflexa~docnr  EQ bsas~belnr
*           AND faglflexa~buzei  EQ bsas~buzei
*           AND faglflexa~ryear  EQ bsas~gjahr
*           LEFT OUTER JOIN skb1
*           ON  faglflexa~rbukrs EQ skb1~bukrs
*           AND faglflexa~racct  EQ skb1~saknr
*           LEFT OUTER JOIN bseg
*            ON  faglflexa~rbukrs EQ bseg~bukrs
*           AND faglflexa~docnr  EQ bseg~belnr
*           AND faglflexa~ryear  EQ bseg~gjahr
*          INTO TABLE it_saida2
*          WHERE bkpf~bukrs  IN p_bukrs
*          AND   bkpf~gjahr  IN p_gjahr
*          AND   bkpf~budat  IN p_budat
*          AND   bkpf~tcode  IN p_tcode
*          AND   bkpf~blart  IN p_blart
*          AND   bkpf~stblg  NE space.


*-US 128512-25-06-2024-#128512-RJF-inicio - Melhoria Performance ZFI0048
        select
              bkpf~bukrs,
              bkpf~belnr,
              bkpf~gjahr,
              bkpf~blart,
              bkpf~bldat,
              bkpf~budat,
              bkpf~bktxt,
              bkpf~waers,
              bkpf~xblnr,
              bkpf~usnam,
              bkpf~tcode,
              bkpf~cpudt,
              bkpf~cputm,
              bkpf~sname,
              bkpf~aedat,
              bkpf~upddt,
              bkpf~xrueb,
              bkpf~psodt,
              bkpf~monat,
              bkpf~stblg,
              faglflexa~buzei,
              faglflexa~racct,
              faglflexa~prctr,
              faglflexa~rcntr,
              faglflexa~tsl,
              faglflexa~hsl,
              faglflexa~ksl,
              faglflexa~bschl,
              faglflexa~drcrk
          from bkpf
           inner join faglflexa
            on   faglflexa~ryear  eq bkpf~gjahr
            and  faglflexa~docnr  eq bkpf~belnr
            and  faglflexa~rbukrs eq bkpf~bukrs
          into table @it_saidabkpf
          where bkpf~bukrs       in @p_bukrs
          and   bkpf~gjahr       in @p_gjahr
          and   bkpf~budat       in @p_budat
          and   bkpf~tcode       in @p_tcode
          and   bkpf~blart       in @p_blart
          and   bkpf~stblg       ne @space
          and   faglflexa~rldnr  eq  '0L'.

        if sy-subrc is initial and it_saidabkpf[] is not initial.
*** Stefanini - 2000043780 - 30/05/2025 - FINC - Início de Alteração
*          delete it_saidabkpf[] where stblg ne space.
*** Stefanini - 2000043780 - 30/05/2025 - FINC - Fim de Alteração
          sort it_saidabkpf by bukrs
                               belnr
                               buzei
                               gjahr.

          if it_saidabkpf[] is not initial.

            select bsis~bukrs,
                   bsis~belnr,
                   bsis~buzei,
                   bsis~gjahr,
                   bsis~buzid,
                   bsis~gsber,
                   bsis~valut,
                   bsis~pswsl,
                   bsis~sgtxt,
                   bsas~buzid as buzida,
                   bsas~gsber as gsbera,
                   bsas~valut as valuta,
                   bsas~pswsl as pswsla,
                   bsas~sgtxt as sgtxta,
                   bsas~augbl,
                   bsas~augdt
                   from bsis
                   left outer join bsas as bsas
                    on bsis~bukrs  eq bsas~bukrs
                   and bsis~belnr  eq bsas~belnr
                   and bsis~buzei  eq bsas~buzei
                   and bsis~gjahr  eq bsas~gjahr
                   into table @it_saidabsis
                  for all entries in @it_saidabkpf
            where
                bsis~bukrs eq @it_saidabkpf-bukrs
            and bsis~belnr eq @it_saidabkpf-belnr
            and bsis~buzei eq @it_saidabkpf-buzei
            and bsis~gjahr eq @it_saidabkpf-gjahr.

            if sy-subrc is initial.
              sort it_saidabsis by  bukrs
                                    belnr
                                    buzei
                                    gjahr.
            endif.

            select bseg~bukrs,
                   bseg~belnr,
                   bseg~buzei,
                   bseg~gjahr,
                   bseg~sgtxt,
                   skb1~saknr,
                   skb1~mitkz
                   from bseg
            left outer join skb1
             on  bseg~bukrs eq skb1~bukrs
             into table @it_saidabseg
             for all entries in @it_saidabkpf
            where
                bseg~bukrs eq @it_saidabkpf-bukrs
            and bseg~belnr eq @it_saidabkpf-belnr
            and bseg~buzei eq @it_saidabkpf-buzei
            and bseg~gjahr eq @it_saidabkpf-gjahr
            and skb1~saknr eq @it_saidabkpf-racct.

            if sy-subrc is initial.
              sort it_saidabseg by  bukrs
                                    belnr
                                    buzei
                                    gjahr
                                    saknr.

            endif.

          endif.

          loop at it_saidabkpf assigning <fs_saidabkpf>.
            append initial line to it_saida2 assigning <fs_saida2>.
            <fs_saida2> = corresponding #( <fs_saidabkpf> ).
            read table it_saidabsis assigning <fs_saidabsis> with key bukrs = <fs_saidabkpf>-bukrs
                                                                                    belnr = <fs_saidabkpf>-belnr
                                                                                    buzei = <fs_saidabkpf>-buzei
                                                                                    gjahr = <fs_saidabkpf>-gjahr
                                                                           binary search.
            if sy-subrc is initial.
*              <fs_saida2> = corresponding #( <fs_saidabsis> ). "BUG SOLTO 155616 / AOENNING
              MOVE-CORRESPONDING <fs_saidabsis> TO <fs_saida2>. "BUG SOLTO 155616 / AOENNING
            endif.
            read table it_saidabseg assigning <fs_saidabseg> with key bukrs = <fs_saidabkpf>-bukrs
                                                                                    belnr = <fs_saidabkpf>-belnr
                                                                                    buzei = <fs_saidabkpf>-buzei
                                                                                    gjahr = <fs_saidabkpf>-gjahr
                                                                                    saknr = <fs_saidabkpf>-racct
                                                                           binary search.
            if sy-subrc is initial.
*              <fs_saida2> = corresponding #( <fs_saidabseg> ). "BUG SOLTO 155616 / AOENNING
            MOVE-CORRESPONDING <fs_saidabseg> TO <fs_saida2>.   "BUG SOLTO 155616 / AOENNING
            endif.
          endloop.
        endif.
*-US 128512-25-06-2024-#128512-RJF-fim - Melhoria Performance ZFI0048

      endif.

*-US 128512-25-06-2024-#128512-RJF-inicio - Melhoria Performance ZFI0048
    catch cx_sy_open_sql_data_error.
      message 'Problema na OpenSQL verificar parâmetros de seleção!' type 'E'.
  endtry.
*-US 128512-25-06-2024-#128512-RJF-fim - Melhoria Performance ZFI0048

  refresh it_saida.
  w_cabec-bukrs = 'Empresa'.
  w_cabec-belnr = 'Documento'.
  w_cabec-gjahr = 'Ano'.
  w_cabec-blart = 'Tipo Doc'.
  w_cabec-bldat = 'Data Doc'.
  w_cabec-budat = 'Data Lcto'.
  w_cabec-bktxt = 'Cab.Documento'.
  w_cabec-waers = 'Moeda'.
  w_cabec-xblnr = 'Nº Referência'.
  w_cabec-usnam = 'Usuário'.
  w_cabec-tcode = 'Cod.Transação'.
  w_cabec-cpudt = 'Dt Entrada Doc'.
  w_cabec-cputm = 'Hora Entrada Doc'.
  w_cabec-sname = 'Usuário'.
  w_cabec-aedat = 'Data Mod. Doc'.
  w_cabec-upddt = 'Data Atual. Doc'.
  w_cabec-xrueb = 'Cod. Reg. Precedente'.
  w_cabec-psodt = 'Dt ultima modif'.
  w_cabec-monat = 'Mês'.
  w_cabec-buzei = 'Nº linha Lcto'.
  w_cabec-buzid = 'Ident. Linha Lcto'.
  w_cabec-racct = 'Nº Conta'.
  w_cabec-mitkz = 'Conta Conciliação'.
  w_cabec-prctr = 'Centro Lucro'.
  w_cabec-rcntr = 'Centro Custo'.
  w_cabec-gsber = 'Divisão'.
  w_cabec-valut = 'Data efetiva'.
  w_cabec-tsl = 'Moeda Transação'.
  w_cabec-hsl = 'Moeda Interna'.
  w_cabec-ksl = 'Moeda Grupo'.
  w_cabec-pswsl = 'Moeda Atualização'.
  w_cabec-bschl = 'Chave Lcto'.
  w_cabec-sgtxt = 'Texto Item'.
  w_cabec-drcrk = 'Débito/Credito'.
  w_cabec-augbl = 'Doc. Compensação'.
  w_cabec-augdt = 'Data Compensação'.
  w_cabec-stblg = 'Doc estornado'.
  w_cabec-pontov1  = w_cabec-pontov2  = w_cabec-pontov3  = w_cabec-pontov4  = w_cabec-pontov5  = w_cabec-pontov6 = ';'.
  w_cabec-pontov7  = w_cabec-pontov8  = w_cabec-pontov9  = w_cabec-pontov10 = w_cabec-pontov11 = w_cabec-pontov12 = ';'.
  w_cabec-pontov13 = w_cabec-pontov14 = w_cabec-pontov15 = w_cabec-pontov16 = w_cabec-pontov17 = w_cabec-pontov18 = ';'.
  w_cabec-pontov19 = w_cabec-pontov20 = w_cabec-pontov21 = w_cabec-pontov22 = w_cabec-pontov23 = w_cabec-pontov24 = ';'.
  w_cabec-pontov25 = w_cabec-pontov26 = w_cabec-pontov27 = w_cabec-pontov28 = w_cabec-pontov29 = w_cabec-pontov30 = ';'.
  w_cabec-pontov31 = w_cabec-pontov32 = w_cabec-pontov33 = w_cabec-pontov34 = w_cabec-pontov35 = w_cabec-pontov36 = ';'.
  w_arquivo = w_cabec.
  condense w_arquivo no-gaps.
  append w_arquivo to t_arquivo.
  "
  loop at it_saida2 into wa_saida2.
    move-corresponding wa_saida2 to w_linha.
    if wa_saida2-tsl lt 0.
      multiply  wa_saida2-tsl by -1.
      write wa_saida2-tsl to w_linha-tsl.
      condense w_linha-tsl no-gaps.
      concatenate '-' w_linha-tsl into w_linha-tsl.
    else.
      write wa_saida2-tsl to w_linha-tsl.
    endif.

    if wa_saida2-hsl lt 0.
      multiply  wa_saida2-hsl by -1.
      write wa_saida2-hsl to w_linha-hsl.
      condense w_linha-hsl no-gaps.
      concatenate '-' w_linha-hsl into w_linha-hsl.
    else.
      write wa_saida2-hsl to w_linha-hsl.
    endif.
    if wa_saida2-ksl lt 0.
      multiply  wa_saida2-ksl by -1.
      write wa_saida2-ksl to w_linha-ksl.
      condense w_linha-ksl no-gaps.
      concatenate '-' w_linha-ksl into w_linha-ksl.
    else.
      write wa_saida2-ksl to w_linha-ksl.
    endif.
    w_linha-pontov1  = w_linha-pontov2  = w_linha-pontov3  = w_linha-pontov4  = w_linha-pontov5  = w_linha-pontov6 = ';'.
    w_linha-pontov7  = w_linha-pontov8  = w_linha-pontov9  = w_linha-pontov10 = w_linha-pontov11 = w_linha-pontov12 = ';'.
    w_linha-pontov13 = w_linha-pontov14 = w_linha-pontov15 = w_linha-pontov16 = w_linha-pontov17 = w_linha-pontov18 = ';'.
    w_linha-pontov19 = w_linha-pontov20 = w_linha-pontov21 = w_linha-pontov22 = w_linha-pontov23 = w_linha-pontov24 = ';'.
    w_linha-pontov25 = w_linha-pontov26 = w_linha-pontov27 = w_linha-pontov28 = w_linha-pontov29 = w_linha-pontov30 = ';'.
    w_linha-pontov31 = w_linha-pontov32 = w_linha-pontov33 = w_linha-pontov34 = w_linha-pontov35 = w_linha-pontov36 = ';'.

    concatenate wa_saida2-bldat+6(2) '.' wa_saida2-bldat+4(2) '.' wa_saida2-bldat+0(4) into w_linha-bldat.
    concatenate wa_saida2-budat+6(2) '.' wa_saida2-budat+4(2) '.' wa_saida2-budat+0(4) into w_linha-budat.
    concatenate wa_saida2-aedat+6(2) '.' wa_saida2-aedat+4(2) '.' wa_saida2-aedat+0(4) into w_linha-aedat.
    concatenate wa_saida2-valut+6(2) '.' wa_saida2-valut+4(2) '.' wa_saida2-valut+0(4) into w_linha-valut.
    concatenate wa_saida2-cpudt+6(2) '.' wa_saida2-cpudt+4(2) '.' wa_saida2-cpudt+0(4) into w_linha-cpudt.
    concatenate wa_saida2-upddt+6(2) '.' wa_saida2-upddt+4(2) '.' wa_saida2-upddt+0(4) into w_linha-upddt.
    concatenate wa_saida2-augdt+6(2) '.' wa_saida2-augdt+4(2) '.' wa_saida2-augdt+0(4) into w_linha-augdt.

    concatenate wa_saida2-cputm+0(2) ':' wa_saida2-cputm+2(2) ':' wa_saida2-cputm+4(2) into w_linha-cputm.

    if wa_saida2-mitkz = ''.
      w_linha-mitkz  = 'S'.
    endif.

    if wa_saida2-sgtxt is initial and wa_saida2-sgtxta is not initial.
      w_linha-sgtxt = wa_saida2-sgtxta.
      w_linha-buzid = wa_saida2-buzida.
      w_linha-gsber = wa_saida2-gsbera.
      w_linha-valut = wa_saida2-valuta.
      w_linha-pswsl = wa_saida2-pswsla.
    endif.

    if wa_saida2-sgtxt is initial and wa_saida2-sgtxtt is not initial.
      w_linha-sgtxt = wa_saida2-sgtxtt.
    endif.

    w_arquivo = w_linha.
    condense w_arquivo no-gaps.

    append w_arquivo to t_arquivo.
    clear: w_linha, wa_saida2, w_arquivo.

  endloop.

*
*  SELECT BUKRS GJAHR BUDAT TCODE BLART BLDAT BKTXT WAERS XBLNR USNAM CPUDT CPUTM SNAME AEDAT UPDDT XRUEB PSODT MONAT BELNR
*    FROM BKPF
*    INTO TABLE IT_BKPF
*    WHERE BUKRS  IN P_BUKRS
*    AND   GJAHR  IN P_GJAHR
*    AND   BUDAT  IN P_BUDAT
*    AND   TCODE  IN P_TCODE
*    AND   BLART  IN P_BLART.
*
*  CHECK SY-SUBRC = 0.
*
*  SELECT RYEAR DOCNR RBUKRS RLDNR RACCT RCNTR PRCTR BUZEI BSCHL TSL HSL KSL DRCRK
*    FROM FAGLFLEXA
*    INTO TABLE IT_FAGLFLEXA
*    FOR ALL ENTRIES IN IT_BKPF
*    WHERE RYEAR   EQ IT_BKPF-GJAHR
*    AND   DOCNR   EQ IT_BKPF-BELNR
*    AND   RBUKRS EQ IT_BKPF-BUKRS
*    AND   RLDNR   EQ '0L'.
*
*  CHECK SY-SUBRC = 0.
*
*  SELECT BUKRS BELNR BUZEI GJAHR BUZID GSBER VALUT PSWSL SGTXT
*    FROM BSIS
*    INTO TABLE IT_BSIS
*    FOR ALL ENTRIES IN IT_FAGLFLEXA
*    WHERE BUKRS    EQ IT_FAGLFLEXA-RBUKRS
*    AND   BELNR   EQ IT_FAGLFLEXA-DOCNR
*    AND   BUZEI   EQ IT_FAGLFLEXA-BUZEI
*    AND   GJAHR   EQ IT_FAGLFLEXA-RYEAR.
*
*  SELECT BUKRS BELNR BUZEI GJAHR BUZID GSBER VALUT PSWSL SGTXT
*    FROM BSAS
*    INTO TABLE IT_BSAS
*    FOR ALL ENTRIES IN IT_FAGLFLEXA
*    WHERE BUKRS    EQ IT_FAGLFLEXA-RBUKRS
*    AND   BELNR   EQ IT_FAGLFLEXA-DOCNR
*    AND   BUZEI   EQ IT_FAGLFLEXA-BUZEI
*    AND   GJAHR   EQ IT_FAGLFLEXA-RYEAR.
*
*  SELECT  BUKRS SAKNR MITKZ
*     FROM  SKB1
*     INTO TABLE IT_SKB1
*     FOR ALL ENTRIES IN IT_FAGLFLEXA
*     WHERE BUKRS EQ  IT_FAGLFLEXA-RBUKRS
*     AND   SAKNR EQ  IT_FAGLFLEXA-RACCT.


endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_CRIA_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_cria_arq .
  data:  lv_nome_arquivo type string.

  data: fg_bsas(1), fg_bsis(1),fg_flexa(1),p_caminho type string.

*  SORT: IT_FAGLFLEXA BY RYEAR DOCNR RBUKRS,
*        IT_BSIS      BY BUKRS BELNR BUZEI GJAHR,
*        IT_BSAS      BY BUKRS BELNR BUZEI GJAHR,
*        IT_SKB1      BY BUKRS SAKNR.
*
*  REFRESH: IT_SAIDA.
*  LOOP AT IT_BKPF INTO WA_BKPF.
*    WA_SAIDA-BUKRS    = WA_BKPF-BUKRS.
*    WA_SAIDA-BELNR    = WA_BKPF-BELNR.
*    WA_SAIDA-GJAHR    = WA_BKPF-GJAHR.
*    WA_SAIDA-BLART    = WA_BKPF-BLART.
*    WA_SAIDA-BLDAT    = WA_BKPF-BLDAT.
*    WA_SAIDA-BUDAT    = WA_BKPF-BUDAT.
*    WA_SAIDA-BKTXT    = WA_BKPF-BKTXT.
*    WA_SAIDA-WAERS    = WA_BKPF-WAERS.
*    WA_SAIDA-XBLNR    = WA_BKPF-XBLNR.
*    WA_SAIDA-USNAM    = WA_BKPF-USNAM.
*    WA_SAIDA-TCODE    = WA_BKPF-TCODE.
*    WA_SAIDA-CPUDT    = WA_BKPF-CPUDT.
*    WA_SAIDA-CPUTM    = WA_BKPF-CPUTM.
*    WA_SAIDA-SNAME    = WA_BKPF-SNAME.
*    WA_SAIDA-AEDAT    = WA_BKPF-AEDAT.
*    WA_SAIDA-UPDDT    = WA_BKPF-UPDDT.
*    WA_SAIDA-XRUEB    = WA_BKPF-XRUEB.
*    WA_SAIDA-PSODT    = WA_BKPF-PSODT.
*    WA_SAIDA-MONAT    = WA_BKPF-MONAT.
*    CLEAR FG_FLEXA.
*    LOOP AT IT_FAGLFLEXA INTO WA_FAGLFLEXA WHERE RYEAR  = WA_BKPF-GJAHR
*                                           AND   DOCNR  = WA_BKPF-BELNR
*                                           AND   RBUKRS = WA_BKPF-BUKRS.
*
*      WA_SAIDA-BUZEI    = WA_FAGLFLEXA-BUZEI.
*      WA_SAIDA-RACCT    = WA_FAGLFLEXA-RACCT.
*      WA_SAIDA-PRCTR    = WA_FAGLFLEXA-PRCTR.
*      WA_SAIDA-RCNTR    = WA_FAGLFLEXA-RCNTR.
*      WA_SAIDA-TSL      = WA_FAGLFLEXA-TSL.
*      WA_SAIDA-HSL      = WA_FAGLFLEXA-HSL.
*      WA_SAIDA-KSL      = WA_FAGLFLEXA-KSL.
*      WA_SAIDA-BSCHL    = WA_FAGLFLEXA-BSCHL.
*      WA_SAIDA-DRCRK    = WA_FAGLFLEXA-DRCRK.
*
*      READ TABLE IT_SKB1 INTO WA_SKB1 WITH KEY BUKRS = WA_FAGLFLEXA-RBUKRS
*                                               SAKNR = WA_FAGLFLEXA-RACCT BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        IF WA_SKB1-MITKZ = ''.
*          WA_SAIDA-MITKZ    = 'S'.
*        ELSE.
*          WA_SAIDA-MITKZ    = WA_SKB1-MITKZ.
*        ENDIF.
*      ELSE.
*        WA_SAIDA-MITKZ    = 'S'.
*      ENDIF.
*      CLEAR: FG_BSAS,FG_BSIS.
*      LOOP AT IT_BSIS INTO WA_BSIS WHERE BUKRS = WA_FAGLFLEXA-RBUKRS
*                                   AND   BELNR = WA_FAGLFLEXA-DOCNR
*                                   AND   BUZEI = WA_FAGLFLEXA-BUZEI
*                                   AND   GJAHR = WA_FAGLFLEXA-RYEAR.
*        FG_BSIS = 'X'.
*        WA_SAIDA-BUZID    = WA_BSIS-BUZID.
*        WA_SAIDA-GSBER    = WA_BSIS-GSBER.
*        WA_SAIDA-VALUT    = WA_BSIS-VALUT.
*        WA_SAIDA-PSWSL    = WA_BSIS-PSWSL.
*        WA_SAIDA-SGTXT    = WA_BSIS-SGTXT.
*        APPEND WA_SAIDA TO IT_SAIDA.
*      ENDLOOP.
*
*      LOOP AT IT_BSAS INTO WA_BSAS WHERE BUKRS = WA_FAGLFLEXA-RBUKRS
*                                   AND   BELNR = WA_FAGLFLEXA-DOCNR
*                                   AND   BUZEI = WA_FAGLFLEXA-BUZEI
*                                   AND   GJAHR = WA_FAGLFLEXA-RYEAR.
*        FG_BSAS = 'X'.
*        WA_SAIDA-BUZID    = WA_BSAS-BUZID.
*        WA_SAIDA-GSBER    = WA_BSAS-GSBER.
*        WA_SAIDA-VALUT    = WA_BSAS-VALUT.
*        WA_SAIDA-PSWSL    = WA_BSAS-PSWSL.
*        WA_SAIDA-SGTXT    = WA_BSAS-SGTXT.
*        APPEND WA_SAIDA TO IT_SAIDA.
*      ENDLOOP.
*      IF FG_BSAS = '' AND FG_BSIS = ''.
*        CLEAR: WA_SAIDA-BUZID,
*               WA_SAIDA-GSBER,
*               WA_SAIDA-VALUT,
*               WA_SAIDA-PSWSL,
*               WA_SAIDA-SGTXT.
*
*        APPEND WA_SAIDA TO IT_SAIDA.
*        FG_FLEXA = 'X'.
*      ENDIF.
*
*    ENDLOOP.
*    CLEAR WA_SAIDA.
*  ENDLOOP.


*
*  CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
* EXPORTING
*   I_FIELD_SEPERATOR          = ';'
*   I_LINE_HEADER              = 'X'
**   I_FILENAME                 = ''
**   I_APPL_KEEP                = ' '
*  TABLES
*    I_TAB_SAP_DATA             = IT_SAIDA
* CHANGING
*   I_TAB_CONVERTED_DATA       = CSV_CONVERTED_TABLE
* EXCEPTIONS
*   CONVERSION_FAILED          = 1
*   OTHERS                     = 2 .

  move p_path          to lv_nome_arquivo.
  if r_local = 'X'.
    data: wl_filename type rlgrap-filename.
    move: lv_nome_arquivo to wl_filename.
    call function 'WS_DOWNLOAD'
      exporting
        filename                = wl_filename
      tables
        data_tab                = t_arquivo
      exceptions
        file_open_error         = 1
        file_write_error        = 2
        invalid_filesize        = 3
        invalid_type            = 4
        no_batch                = 5
        unknown_error           = 6
        invalid_table_width     = 7
        gui_refuse_filetransfer = 8
        customer_error          = 9
        no_authority            = 10
        others                  = 11.
    if sy-subrc <> 0.
      message id '00' type 'E' number '398' with
         'Erro ao criar o arquivo'
         'na pasta'
         p_path .
    endif.

    message 'Arquivo gerado com sucesso!' type 'I'.
  else.
    open dataset  lv_nome_arquivo for output in text mode    "smart: 11/01/10 E111
    encoding non-unicode with windows linefeed.
    if sy-subrc <> 0.
      message 'Caminho ou nome de arquivo inválido. Impossível continuar!'
      type 'A'.
    endif.

    loop at t_arquivo into w_arquivo.
      transfer w_arquivo to lv_nome_arquivo.
    endloop.

    close dataset lv_nome_arquivo.
    if sy-batch = ''.
      message 'Arquivo gerado com sucesso!' type 'I'.
    endif.
  endif.



endform.                    " F_CRIA_ARQ
