************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 03.07.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Start RFC de outbond de AP/AR - Realizado/Previsão  *
*                  XRT                                                 *
************************************************************************

report  ZFIR0004.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*

data: AUX_CONT type I.

data: begin of WA_PREVISAO,
        BUKRS  like BSIK-BUKRS,
        GJAHR  like BSIK-GJAHR,
        BELNR  like BSIK-BELNR,
        BUZEI  like BSIK-BUZEI,
        BUDAT  like BSIK-BUDAT,
        WAERS  like BSIK-WAERS,
        BLART  like BSIK-BLART,
        GSBER  like BSIK-GSBER,
        DMBTR  like BSIK-DMBTR,
        WRBTR  like BSIK-WRBTR,
        SGTXT  like BSIK-SGTXT,
        HKONT  like BSIK-HKONT,
        ZFBDT  like BSIK-ZFBDT,
        ZBD1T  like BSIK-ZBD1T,
        ZLSCH  like BSIK-ZLSCH,
        ZLSPR  like BSIK-ZLSPR,
        HBKID  like BSIK-HBKID,
        DMBE2  like BSIK-DMBE2,
        UMSKZ  like BSIK-UMSKZ,
        XREF1  like BSIK-XREF1,
        XREF2  like BSIK-XREF2,
        XREF3  like BSIK-XREF3,
        ZUONR  like BSIK-ZUONR,
        ANLN1  like BSIK-ANLN1,
        BSCHL  like BSIK-BSCHL,
        CLIFOR like BSIK-LIFNR,
        EBELN  like BSIK-EBELN,
        EBELP  like BSIK-EBELP,
        UMSKS  like BSIK-UMSKS,
        LIFNR  like VBSEGK-LIFNR,
        AUSBK  like VBSEGK-AUSBK,
        AUFNR  like BSIK-AUFNR,
        VBEL2  like BSID-VBEL2,
        POSN2  like BSID-POSN2,
        KUNNR  like BSID-KUNNR,
      end   of WA_PREVISAO,

      begin of WA_REALIZADO,
        BUKRS  like BSAK-BUKRS,
        AUGBL  like BSAK-AUGBL,
        GJAHR  like BSAK-GJAHR,
        BELNR  like BSAK-BELNR,
        BUZEI  like BSAK-BUZEI,
        BUDAT  like BSAK-BUDAT,
        AUGDT  like BSAK-AUGDT,
        WAERS  like BSAK-WAERS,
        BLART  like BSAK-BLART,
        GSBER  like BSAK-GSBER,
* Montante em moeda interna
        DMBTR  like BSAK-DMBTR,
* Montante em moeda do documento
        WRBTR  like BSAK-WRBTR,
        SGTXT  like BSAK-SGTXT,
        HKONT  like BSAK-HKONT,
        ZFBDT  like BSAK-ZFBDT,
        ZBD1T  like BSAK-ZBD1T,
        ZLSCH  like BSAK-ZLSCH,
        ZLSPR  like BSAK-ZLSPR,
        HBKID  like BSAK-HBKID,
        DMBE2  like BSAK-DMBE2,
        UMSKZ  like BSIK-UMSKZ,
        XREF1  like BSIK-XREF1,
        XREF2  like BSIK-XREF2,
        XREF3  like BSIK-XREF3,
        ZUONR  like BSIK-ZUONR,
        ANLN1  like BSIK-ANLN1,
        BSCHL  like BSIK-BSCHL,
        CLIFOR like BSIK-LIFNR,
        EBELN  like BSIK-EBELN,
        EBELP  like BSIK-EBELP,
        UMSKS  like BSIK-UMSKS,
        AUFNR  like BSIK-AUFNR,
        AUGGJ  like BSIK-AUGGJ,
        VBEL2  like BSAD-VBEL2,
        POSN2  like BSAD-POSN2,
        KUNNR  like BSID-KUNNR,
      end   of WA_REALIZADO,

      begin of WA_T001B,
        BUKRS like T001B-BUKRS,
        MKOAR like T001B-MKOAR,
        FRPE1 like T001B-FRPE1,
        FRYE1 like T001B-FRYE1,
        FRPE2 like T001B-FRPE2,
        FRYE2 like T001B-FRYE2,
      end   of WA_T001B.

data: begin of WA_EKPO,
        EBELN like EKPO-EBELN,
        EBELP like EKPO-EBELP,
        KNTTP like EKPO-KNTTP,
        MATKL like EKPO-MATKL,
      end of WA_EKPO.

data: begin of WA_BSIS,
        BUKRS like BSIS-BUKRS,
        HKONT like BSIS-HKONT,
        GJAHR like BSIS-GJAHR,
        BELNR like BSIS-BELNR,
        BUZEI like BSIS-BUZEI,
        SGTXT like BSIS-SGTXT,
        KOSTL like BSIS-KOSTL,
        PRCTR like BSIS-PRCTR,
        AUFNR like BSIS-AUFNR,
        BSCHL like BSIS-BSCHL,
      end of WA_BSIS.

data: begin of WA_BSEG,
        BUKRS like BSEG-BUKRS,
        BELNR like BSEG-BELNR,
        GJAHR like BSEG-GJAHR,
        EBELN like BSEG-EBELN,
        EBELP like BSEG-EBELP,
        MATNR like BSEG-MATNR,
        HKONT like BSEG-HKONT,
        BSCHL like BSEG-BSCHL,
      end of WA_BSEG.

data: begin of WA_EKKN,
        EBELN like EKKN-EBELN,
        EBELP like EKKN-EBELP,
        SAKTO like EKKN-SAKTO,
      end of WA_EKKN.

data: begin of WA_VBKPF,
        BUDAT like VBKPF-BUDAT,
        WAERS like VBKPF-WAERS,
        BLART like VBKPF-BLART,
        BELNR like VBKPF-BELNR,
        BUKRS like VBKPF-BUKRS,
        AUSBK like VBKPF-AUSBK,
        GJAHR like VBKPF-GJAHR,
      end of WA_VBKPF.

data: begin of WA_ZGL008,
        CONTA_REC    like ZGL008-CONTA_REC,
        CONTA_PART   like ZGL008-CONTA_PART,
        CHLANC       like ZGL008-CHLANC,
        UMSKZ        like ZGL008-UMSKZ,
        BLART        like ZGL008-BLART,
        XREF1        like ZGL008-XREF1,
        XREF2        like ZGL008-XREF2,
        XREF3        like ZGL008-XREF3,
        KOSTL        like ZGL008-KOSTL,
        PRCTR        like ZGL008-PRCTR,
        ZUONR        like ZGL008-ZUONR,
        AUFNR        like ZGL008-AUFNR,
        ANLN1        like ZGL008-ANLN1,
        KNTTP        like ZGL008-KNTTP,
        MATKL        like ZGL008-MATKL,
        CONTA_FINANC like ZGL008-CONTA_FINANC,
        CHAVE(200),
      end of WA_ZGL008.

data: begin of WA_BKPF,
        BELNR   like BKPF-BELNR,
        GJAHR   like BKPF-GJAHR,
        AWKEY   like BKPF-AWKEY,
        AWKEY10 like RBKP-BELNR,
        BUKRS   like BKPF-BUKRS,
        WAERS   like BKPF-WAERS,
        BUDAT   like BKPF-BUDAT,
        BLART   like BKPF-BLART,
        BKTXT   like BKPF-BKTXT,
        AUSBK   like BKPF-AUSBK,
        STBLG   like BKPF-STBLG,
        STJAH   like BKPF-STJAH,
      end of WA_BKPF,

      begin of WA_AUFK,
        AUART like AUFK-AUART,
      end of WA_AUFK.

data: begin of WA_VBAK,
        VBELN like VBAK-VBELN,
        KNUMV like VBAK-KNUMV,
        VKORG like VBAK-VKORG,
        KUNNR like VBAK-KUNNR,
        WAERK like VBAK-WAERK,
        KALSM like VBAK-KALSM,
        VTWEG like VBAK-VTWEG,
      end of WA_VBAK,

      begin of WA_VBAP,
        VBELN  like VBAP-VBELN,
        POSNR  like VBAP-POSNR,
        KWMENG like VBAP-KWMENG,
        MATNR  like VBAP-MATNR,
        GSBER  like VBAP-GSBER,
        VLRAB  like BSID-DMBTR,
        VLRLQ  like BSID-DMBTR,
        VLRAD  like BSID-DMBTR,
        VLRKV  like BSID-DMBTR,
        VLRTT  like BSID-DMBTR,
      end of WA_VBAP,

      begin of WA_VBFA,
        VBELV   like VBFA-VBELV,
        POSNV   like VBFA-POSNV,
        VBELN   like VBFA-VBELN,
        KNUMV   like VBAK-KNUMV,
        RFMNG   like VBFA-RFMNG,
        VBTYP_N like VBFA-VBTYP_N,
        ERDAT   like VBFA-ERDAT,
        AWKEY   like BKPF-AWKEY,
        GJAHR   like BKPF-GJAHR,
        MATNR   like VBAP-MATNR,
        BUKRS   like VBAK-VKORG,
        KUNNR   like VBAK-KUNNR,
        VTWEG   like VBAK-VTWEG,
        AKONT   like KNB1-AKONT,
        KALSM   like VBAK-KALSM,
        KVSL1   like T683S-KVSL1,
        KTGRM   like MVKE-KTGRM,
        MATKL   like MARA-MATKL,
        KTGRD   like KNVV-KTGRD,
        SAKN1   like C001-SAKN1,
        VALDT   like VBKD-VALDT,
        ZTERM   like VBKD-ZTERM,
        ZLSCH   like VBKD-ZLSCH,
        ZTAG1   like T052-ZTAG1,
      end of WA_VBFA,

      begin of WA_VBFA_TOT,
        VBELV like VBFA-VBELV,
        POSNV like VBFA-POSNV,
        RFMNG like VBFA-RFMNG,
      end of WA_VBFA_TOT,

      begin of WA_TOT_AB,
        VBEL2 like BSID-VBEL2,
        POSN2 like BSID-POSN2,
        DMBTR like BSID-DMBTR,
        DMBE2 like BSID-DMBE2,
      end of WA_TOT_AB,

      begin of WA_KONV,
        KNUMV like KONV-KNUMV,
        KPOSN like KONV-KPOSN,
        KSCHL like KONV-KSCHL,
        KWERT like KONV-KWERT,
      end of WA_KONV,

      begin of WA_KNB1,
        KUNNR like KNB1-KUNNR,
        BUKRS like KNB1-BUKRS,
        AKONT like KNB1-AKONT,
      end of WA_KNB1,

      begin of WA_T683S,
        KALSM like T683S-KALSM,
        KVSL1 like T683S-KVSL1,
      end of WA_T683S,

      begin of WA_MVKE,
        MATNR like MVKE-MATNR,
        VKORG like MVKE-VKORG,
        VTWEG like MVKE-VTWEG,
        KTGRM like MVKE-KTGRM,
      end of WA_MVKE,

      begin of WA_MARA,
        MATNR like MARA-MATNR,
        MATKL like MARA-MATKL,
      end of WA_MARA,

      begin of WA_KNVV,
        KUNNR like KNVV-KUNNR,
        VKORG like KNVV-VKORG,
        VTWEG like KNVV-VTWEG,
        KTGRD like KNVV-KTGRD,
      end of WA_KNVV,

      begin of WA_C001,
        KTOPL like C001-KTOPL,
        VKORG like C001-VKORG,
        KTGRD like C001-KTGRD,
        KTGRM like C001-KTGRM,
        KVSL1 like C001-KVSL1,
        SAKN1 like C001-SAKN1,
      end of WA_C001,

      begin of WA_VBKD,
        VBELN like VBKD-VBELN,
        POSNR like VBKD-POSNR,
        VALDT like VBKD-VALDT,
        ZTERM like VBKD-ZTERM,
        ZLSCH like VBKD-ZLSCH,
      end of WA_VBKD,

      begin of WA_T052,
        ZTERM like T052-ZTERM,
        ZTAG1 like T052-ZTAG1,
      end of WA_T052.

data: WA_MOVXRT_PRE like ZFIE_MOV_XRT,
      WA_MOVXRT_REA like ZFIE_MOV_XRT,
      IT_PREVISAO   like standard table of WA_PREVISAO,
      IT_PREVISAO2  like standard table of WA_PREVISAO,
      IT_REALIZADO  like standard table of WA_REALIZADO,
      IT_T001B      like standard table of WA_T001B,
      IT_MOVXRT_PRE like standard table of WA_MOVXRT_PRE,
      IT_MOVXRT_REA like standard table of WA_MOVXRT_REA,
      IT_EKPO       like standard table of WA_EKPO,
      IT_BSIS       like standard table of WA_BSIS,
      IT_BSEG       like standard table of WA_BSEG,
      IT_EKKN       like standard table of WA_EKKN,
      IT_ZGL008     like standard table of WA_ZGL008,
      IT_ZGL008_2   like standard table of WA_ZGL008,
      IT_VBKPF      like standard table of WA_VBKPF,
      IT_BKPF       like standard table of WA_BKPF,

      IT_VBAK       like standard table of WA_VBAK,
      IT_VBAK2      like standard table of WA_VBAK,
      IT_VBAP       like standard table of WA_VBAP,
      IT_VBFA       like standard table of WA_VBFA,
      IT_VBFA2      like standard table of WA_VBFA,
      IT_VBFA_TOT   like standard table of WA_VBFA_TOT,
      IT_TOT_AB     like standard table of WA_TOT_AB,
      IT_TOT_LQ     like standard table of WA_TOT_AB,
      IT_TOT_AD     like standard table of WA_TOT_AB,
      IT_KONV       like standard table of WA_KONV,
      IT_KNB1       like standard table of WA_KNB1,
      IT_T683S      like standard table of WA_T683S,
      IT_MVKE       like standard table of WA_MVKE,
      IT_MARA       like standard table of WA_MARA,
      IT_KNVV       like standard table of WA_KNVV,
      IT_C001       like standard table of WA_C001,
      IT_VBKD       like standard table of WA_VBKD,
      IT_VBKD2      like standard table of WA_VBKD,
      IT_T052       like standard table of WA_T052.

data: begin of WA_BSAK,
        AUGBL like BSAK-AUGBL,
        BELNR like BSAK-BELNR,
        BUZEI like BSAK-BUZEI,
      end   of WA_BSAK,

      begin of WA_BLART,
        BLART like BSAK-BLART,
        AUGBL like BSAK-AUGBL,
        AUGGJ like BSAK-AUGGJ,
        BUDAT like BSAK-BUDAT,
        AUGDT like BSAK-AUGDT,
      end of WA_BLART.

data : IT_BSAK  like standard table of WA_BSAK,
       VG_TCODE type C length 7.
data : VL_AWKEY          like ZFIE_MOV_XRT-ORIGEM_PK.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
initialization.

  perform UNICO_DOCUMENTO.

start-of-selection.

  clear: VG_TCODE.
* Busca periodo valido de retorno.
  perform F_BUSCA_PERIODO.
* Retorno AP - Previsáo
  perform F_PREVISAO_AP.
* Retorno AP - Previsáo
  perform F_PREVISAO_AP_PRE_EDITADO.
* Retorno AR - Previsão
  perform F_PREVISAO_AR.
* Retorno AP - Realizado
  perform F_REALIZADO_AP.
* Retorno AR - Realizado
  perform F_REALIZADO_AR.
* Retorna Saldo de Previsão de Ordens de Venda
  perform F_PREVISAO_AR_ORDENS.
* Chamada da RFC
  perform F_CHAMA_RFC.

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_PERIODO
*&---------------------------------------------------------------------*
*       Busco empresas e periodos para seleção de documentos.
*----------------------------------------------------------------------*
form F_BUSCA_PERIODO .

  select BUKRS MKOAR FRPE1 FRYE1 FRPE2 FRYE2
    from T001B
    into corresponding fields of table IT_T001B
   where ( ( MKOAR eq 'D' ) or ( MKOAR eq 'K' ) )
     and BUKRS not in ('0002','0005','0023','0032').

  select CONTA_REC CONTA_PART CHLANC UMSKZ
         BLART XREF1 XREF2 XREF3 KOSTL PRCTR
         ZUONR AUFNR ANLN1 KNTTP MATKL CONTA_FINANC
    from ZGL008
    into table IT_ZGL008_2.

  loop at IT_ZGL008_2 into WA_ZGL008.
    concatenate WA_ZGL008-CONTA_REC WA_ZGL008-CONTA_PART
                WA_ZGL008-CHLANC    WA_ZGL008-UMSKZ
                WA_ZGL008-BLART
*               wa_zgl008-xref1 wa_zgl008-xref2
                WA_ZGL008-XREF3 WA_ZGL008-KOSTL WA_ZGL008-PRCTR
                WA_ZGL008-ZUONR WA_ZGL008-AUFNR WA_ZGL008-ANLN1
                WA_ZGL008-KNTTP WA_ZGL008-MATKL into WA_ZGL008-CHAVE.
    append WA_ZGL008 to IT_ZGL008.
  endloop.

  clear: WA_ZGL008.

endform.                    " F_BUSCA_PERIODO
*&---------------------------------------------------------------------*
*&      Form  F_PREVISAO_AP
*&---------------------------------------------------------------------*
*       Busco dados para retorno de Previsão AR
*----------------------------------------------------------------------*
form F_PREVISAO_AP .
  data: VL_DTINI      type SY-DATUM,
        VL_DTFIM      type SY-DATUM,
        VL_BKPF       like BKPF-BELNR,
        IT_PREVISAO_2 like standard table of WA_PREVISAO.

  loop at IT_T001B into WA_T001B where MKOAR eq 'K'.

    if WA_T001B-FRPE1+1(2) gt '12'.
      concatenate WA_T001B-FRYE1
                  '12'
                  '01' into VL_DTINI.
    else.
      concatenate WA_T001B-FRYE1
                  WA_T001B-FRPE1+1(2)
                  '01' into VL_DTINI.
    endif.

    if WA_T001B-FRPE2+1(2) gt '12'.
      concatenate WA_T001B-FRYE2
                  '12'
                  '01' into VL_DTFIM.
    else.
      concatenate WA_T001B-FRYE2
                  WA_T001B-FRPE2+1(2)
                  '01' into VL_DTFIM.
    endif.

    call function 'RP_LAST_DAY_OF_MONTHS'
      exporting
        DAY_IN            = VL_DTFIM
      importing
        LAST_DAY_OF_MONTH = VL_DTFIM.

    clear: IT_BKPF,
           IT_PREVISAO_2,
           IT_PREVISAO.

* DOCUMENTOS ANTIGOS ALTERADOS.

    select BELNR BUKRS GJAHR
      from BKPF
      into corresponding fields of table IT_BKPF
     where BUKRS eq WA_T001B-BUKRS
       and AEDAT eq SY-DATUM
       and CPUDT lt VL_DTINI.

    if IT_BKPF is not initial.
      select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
             BLART GSBER DMBTR WRBTR SGTXT HKONT
             ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
             UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
             BSCHL LIFNR EBELN EBELP UMSKS AUFNR
        from BSIK
        into table IT_PREVISAO_2
         for all entries in IT_BKPF
       where BUKRS eq IT_BKPF-BUKRS
         and GJAHR eq IT_BKPF-GJAHR
         and BELNR eq IT_BKPF-BELNR
         and SHKZG eq 'H'.
    endif.

    select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
           BLART GSBER DMBTR WRBTR SGTXT HKONT
           ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
           UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
           BSCHL LIFNR EBELN EBELP UMSKS AUFNR
      from BSIK
      into table IT_PREVISAO
     where BUKRS eq WA_T001B-BUKRS
       and CPUDT ge VL_DTINI
       and SHKZG eq 'H'.

    loop at IT_PREVISAO_2 into WA_PREVISAO.
      append WA_PREVISAO to IT_PREVISAO.
    endloop.

    clear: IT_BKPF.

    select BELNR BUKRS GJAHR AWKEY
      from BKPF
      into corresponding fields of table IT_BKPF
      for all entries in IT_PREVISAO
     where BUKRS eq IT_PREVISAO-BUKRS
       and GJAHR eq IT_PREVISAO-GJAHR
       and BELNR eq IT_PREVISAO-BELNR.

    perform PROCESSA_PREVISAO_AP.

***  Lançamentos Estornados no dia.
    select BUKRS GJAHR BELNR BLART WAERS BUDAT BKTXT AUSBK STBLG STJAH
      from BKPF
      into corresponding fields of table IT_BKPF
     where BUKRS eq WA_T001B-BUKRS
       and BUDAT eq SY-DATUM
       and STBLG ne ''
       and STJAH ne ''
       and TCODE eq 'FB08'.

    loop at IT_BKPF into WA_BKPF.
      clear: WA_PREVISAO.
      WA_PREVISAO-BUKRS  = WA_BKPF-BUKRS.
      WA_PREVISAO-GJAHR  = WA_BKPF-STJAH.
      WA_PREVISAO-BELNR  = WA_BKPF-STBLG.
      WA_PREVISAO-BLART  = WA_BKPF-BLART.
      WA_PREVISAO-WAERS  = WA_BKPF-WAERS.
      WA_PREVISAO-BUDAT  = WA_BKPF-BUDAT.
      WA_PREVISAO-SGTXT  = WA_BKPF-BKTXT.
      WA_PREVISAO-AUSBK  = WA_BKPF-AUSBK.
      WA_PREVISAO-ZFBDT  = WA_BKPF-BUDAT.
      WA_PREVISAO-ZBD1T  = 0.
      WA_PREVISAO-DMBTR  = 0.
      WA_PREVISAO-WRBTR  = 0.
      VG_TCODE = 'ESTORNO'.
      perform F_INSERT_PREV using WA_PREVISAO 'AP' '4'.
      clear VG_TCODE.
    endloop.

  endloop.
endform.                    " F_PREVISAO_AP
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_PREV
*&---------------------------------------------------------------------*
*       Monto dados na estrutura de retorno AR e PR - Previsão
*----------------------------------------------------------------------*
*      -->PT_PREVISAO  Dados a inserir
*      <--PT_MOVXRT    Dados para outbound
*----------------------------------------------------------------------*
form F_INSERT_PREV     using PW_PREVISAO like WA_PREVISAO
                             P_TP
                             P_TEMPERATURA.
  data: VL_BANKL        like T012-BANKL,
        VL_BANKN        like T012K-BANKN,
        NU_DIAS         like BSID-ZBD1T,
        DIAS            like T5A4A-DLYDY,
        VL_CONTA_FINANC like ZGL008-CONTA_FINANC,
        VL_CONTA_PART   like ZGL008-CONTA_PART,
        VL_ZUONR        like ZGL008-ZUONR,
        VL_ANLN1        like ZGL008-ANLN1,
        VL_CHAVE(200),
        VL_KOSTL        like ZGL008-KOSTL,
        VL_PRCTR        like ZGL008-PRCTR,
        VL_AUFNR        like ZGL008-AUFNR,
        VL_XREF3        like ZGL008-XREF3,
        VL_BSCHL        like BSIK-BSCHL,
        VL_SGTXT        like BSIS-SGTXT,
        VL_HKONT        like BSIS-HKONT,
        VL_CLIFOR       like LFA1-NAME1,
        VL_BELNR        like BSIK-BELNR,
        VL_MATKL        like VBAP-MATKL,
        VG_ZUONR        like BSEG-ZUONR.

  perform VALIDA_ZUONR using VL_ZUONR PW_PREVISAO-ZUONR.
  perform VALIDA_ZUONR using VL_XREF3 PW_PREVISAO-XREF3.
  perform VALIDA_ANLN1 using VL_ANLN1 PW_PREVISAO-ANLN1.

  if ( PW_PREVISAO-BLART eq 'MA' ) or
     ( PW_PREVISAO-BLART eq 'NG' ) or
     ( PW_PREVISAO-BLART eq 'NC' ) or
     ( PW_PREVISAO-BLART eq 'NL' ).
    VL_XREF3 = PW_PREVISAO-XREF3.
  endif.

  clear WA_MOVXRT_PRE.

  select single BANKN
    from T012K
    into VL_BANKN
   where BUKRS eq PW_PREVISAO-BUKRS
     and HBKID eq PW_PREVISAO-HBKID.

  select single BANKL
    from T012
    into VL_BANKL
   where BUKRS eq PW_PREVISAO-BUKRS
     and HBKID eq PW_PREVISAO-HBKID.

  clear VL_AWKEY.

  select single AWKEY
    from BKPF
    into VL_AWKEY
   where BUKRS eq PW_PREVISAO-BUKRS
     and BELNR eq PW_PREVISAO-BELNR
     and GJAHR eq PW_PREVISAO-GJAHR.

  clear: VL_BSCHL,
         VL_KOSTL,
         VL_PRCTR,
         VL_AUFNR,
         VL_HKONT.

  if PW_PREVISAO-UMSKZ is not initial.

    VL_BSCHL = PW_PREVISAO-BSCHL.
    VL_SGTXT = PW_PREVISAO-SGTXT.

  else.

    select BUKRS HKONT GJAHR BELNR BUZEI SGTXT KOSTL PRCTR AUFNR BSCHL
      from BSIS
      into table IT_BSIS
     where BUKRS eq PW_PREVISAO-BUKRS
       and HKONT ne PW_PREVISAO-HKONT
       and GJAHR eq PW_PREVISAO-GJAHR
       and BELNR eq PW_PREVISAO-BELNR.

    read table IT_BSIS into WA_BSIS with key BUKRS = PW_PREVISAO-BUKRS
                                             GJAHR = PW_PREVISAO-GJAHR
                                             BELNR = PW_PREVISAO-BELNR.
    if SY-SUBRC eq 0.
      VL_KOSTL = WA_BSIS-KOSTL.
      VL_PRCTR = WA_BSIS-PRCTR.
      VL_AUFNR = WA_BSIS-AUFNR.
      VL_SGTXT = WA_BSIS-SGTXT.
      if PW_PREVISAO-ANLN1 is initial.
        VL_HKONT = WA_BSIS-HKONT.
      endif.
    else.
      VL_SGTXT = PW_PREVISAO-SGTXT.
    endif.

  endif.

  if P_TP = 'AP'.

    select single NAME1
      from LFA1
      into VL_CLIFOR
     where LIFNR eq PW_PREVISAO-CLIFOR.

    clear: VL_KOSTL,
           VL_PRCTR,
           VL_AUFNR.

    if PW_PREVISAO-UMSKS eq 'A'.

* Limpar work area e tabela interna, pois se trata de uma variavel global
* com isso estava acontecendo no select abaixo que a tabela interna
* estava sendo limpa, pois ebeln e ebelp estavam vazios, porem a work area
* no read table, quando retronado sy-subrc igual a 4, ela não é limpa,
* portanto a work area poderia ter "sujeira" de um outro read table bem
* sucedido.

      clear: IT_EKPO, WA_EKPO.

      if ( PW_PREVISAO-EBELN is not initial ) and
         ( PW_PREVISAO-EBELP is not initial ).

        select EBELN EBELP KNTTP MATKL
          from EKPO
          into table IT_EKPO
         where EBELN eq PW_PREVISAO-EBELN
           and EBELP eq PW_PREVISAO-EBELP.

      endif.

      read table IT_EKPO into WA_EKPO with key EBELN = PW_PREVISAO-EBELN
                                               EBELP = PW_PREVISAO-EBELP.

      if ( WA_EKPO-KNTTP is initial ) and ( IT_EKPO is not initial ).
        concatenate PW_PREVISAO-HKONT VL_HKONT VL_BSCHL
                    PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                    VL_XREF3 VL_KOSTL
                    VL_PRCTR VL_ZUONR
                    VL_AUFNR VL_ANLN1
                    WA_EKPO-MATKL
                    into VL_CHAVE.
      elseif ( WA_EKPO-KNTTP eq 'A' ) and ( IT_EKPO is not initial ).
        concatenate PW_PREVISAO-HKONT VL_HKONT VL_BSCHL
                    PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                    VL_XREF3 VL_KOSTL
                    VL_PRCTR VL_ZUONR
                    VL_AUFNR VL_ANLN1
                    WA_EKPO-KNTTP
                    into VL_CHAVE.
      else.
        concatenate PW_PREVISAO-HKONT VL_HKONT VL_BSCHL
                    PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                    VL_XREF3 VL_KOSTL
                    VL_PRCTR VL_ZUONR
                    VL_AUFNR VL_ANLN1
                    into VL_CHAVE.
      endif.

    else.
      if PW_PREVISAO-BLART eq 'RE'.

        clear: WA_BSEG.

        if PW_PREVISAO-BSCHL eq 34.

          select AUGBL BELNR
            from BSAK
            into table IT_BSAK
           where BUKRS eq PW_PREVISAO-BUKRS
             and LIFNR eq PW_PREVISAO-CLIFOR
             and AUGBL ne PW_PREVISAO-BELNR
             and BLART eq PW_PREVISAO-BLART.

          loop at IT_BSAK into WA_BSAK.
            if WA_BSAK-AUGBL ne WA_BSAK-BELNR.
              clear: IT_BSEG.
              data ETL739C14R7433 type table of BSEG.
              data LT_FIELDS_L739C14R8330 type FAGL_T_FIELD.
              LT_FIELDS_L739C14R8330 = value #( ( LINE = 'BUKRS' )
               ( LINE = 'BELNR' )
               ( LINE = 'GJAHR' )
               ( LINE = 'EBELN' )
               ( LINE = 'EBELP' )
               ( LINE = 'MATNR' )
               ( LINE = 'HKONT' )
               ( LINE = 'BSCHL' )
               ).
              data RLDNR_L739C14R9309 type RLDNR.
              call function 'FAGL_GET_LEADING_LEDGER'
                importing
                  E_RLDNR       = RLDNR_L739C14R9309
                exceptions
                  NOT_FOUND     = 1
                  MORE_THAN_ONE = 2.
              if SY-SUBRC = 0.
                call function 'FAGL_GET_GL_DOCUMENT'
                  exporting
                    I_RLDNR         = RLDNR_L739C14R9309
                    I_BUKRS         = PW_PREVISAO-BUKRS
                    I_BELNR         = WA_BSAK-BELNR
                    I_GJAHR         = PW_PREVISAO-GJAHR
                    IT_FIELDLIST    = LT_FIELDS_L739C14R8330
                    IT_WHERE_CLAUSE = value TT_RSDSWHERE( ( |HKONT NE { CL_ABAP_DYN_PRG=>QUOTE( PW_PREVISAO-HKONT ) }| ) ( | AND | ) ( |BUZID NE 'P'| ) )
                  importing
                    ET_BSEG         = ETL739C14R7433
                  exceptions
                    NOT_FOUND       = 1.
              endif.
              if SY-SUBRC = 0 and LINES( ETL739C14R7433 ) > 0.
                move-corresponding ETL739C14R7433 to IT_BSEG.
                SY-DBCNT = LINES( ETL739C14R7433 ).
              else.
                SY-SUBRC = 4.
                SY-DBCNT = 0.
              endif.

              VL_BELNR = WA_BSAK-BELNR.
            endif.
          endloop.
          read table IT_BSEG into WA_BSEG with key BUKRS = PW_PREVISAO-BUKRS
                                                   BELNR = VL_BELNR
                                                   GJAHR = PW_PREVISAO-GJAHR.
        else.

* ---> S4 Migration - 15/06/2023 - MA
*          select BUKRS BELNR GJAHR EBELN EBELP MATNR HKONT BSCHL
*            from BSEG
*            into table IT_BSEG
*           where BUKRS   eq PW_PREVISAO-BUKRS
*             and BELNR  eq PW_PREVISAO-BELNR
*             and GJAHR  eq PW_PREVISAO-GJAHR
*             and HKONT  in ('0000212100','0000511002')
*             and EBELN  ne ''.
          data:   LT_BSEG type FAGL_T_BSEG.
          types: LR_HKONT_TYPE type range of HKONT.
          data : LR_HKONT type LR_HKONT_TYPE.

          LR_HKONT = value LR_HKONT_TYPE( let S = 'I'
                                              O = 'BT'
                                          in SIGN   = S
                                             OPTION = O
                                             ( LOW = '0000212100' )
                                             ( LOW = '0000511002' ) ).

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS   = PW_PREVISAO-BUKRS
              I_BELNR   = PW_PREVISAO-BELNR
              I_GJAHR   = PW_PREVISAO-GJAHR
            importing
              ET_BSEG   = LT_BSEG
            exceptions
              NOT_FOUND = 1
              others    = 2.

          delete LT_BSEG where HKONT not in LR_HKONT and EBELN eq ' '.

          if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
            move-corresponding LT_BSEG to IT_BSEG.
            SY-DBCNT = LINES( LT_BSEG ).
          else.
            SY-SUBRC = 4.
            SY-DBCNT = 0.
          endif.

*<--- S4 Migration - 15/06/2023 - MA

          if SY-SUBRC eq 0.
            read table IT_BSEG into WA_BSEG with key BUKRS = PW_PREVISAO-BUKRS
                                                     BELNR = PW_PREVISAO-BELNR
                                                     GJAHR = PW_PREVISAO-GJAHR.
          endif.
        endif.

        if ( WA_BSEG-HKONT eq '0000212100' ) or
           ( WA_BSEG-HKONT eq '0000511002' ).
          select EBELN EBELP SAKTO
            from EKKN
            into table IT_EKKN
           where EBELN eq WA_BSEG-EBELN
             and EBELP eq WA_BSEG-EBELP.

          clear: WA_EKKN.

          read table IT_EKKN into WA_EKKN with key EBELN = WA_BSEG-EBELN
                                                   EBELP = WA_BSEG-EBELP.
          if ( SY-SUBRC eq 0 ) and ( PW_PREVISAO-ANLN1 is initial ).
            VL_CONTA_PART = WA_EKKN-SAKTO.
          endif.
        else.
          VL_CONTA_PART = WA_BSIS-HKONT.
        endif.

        clear: IT_EKPO, WA_EKPO.

        if ( WA_BSEG-EBELN is not initial ) and
           ( WA_BSEG-EBELP is not initial ).
          select EBELN EBELP KNTTP MATKL
            from EKPO
            into table IT_EKPO
           where EBELN eq WA_BSEG-EBELN
             and EBELP eq WA_BSEG-EBELP.
        endif.

        if IT_EKPO is not initial.
          read table IT_EKPO into WA_EKPO with key EBELN = WA_BSEG-EBELN
                                                   EBELP = WA_BSEG-EBELP.
        endif.

        if ( WA_EKPO-KNTTP is initial ) and ( WA_EKPO is not initial ).
          concatenate PW_PREVISAO-HKONT VL_CONTA_PART VL_BSCHL
                      PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                      VL_XREF3 VL_KOSTL VL_PRCTR
                      VL_ZUONR VL_AUFNR VL_ANLN1
                      WA_EKPO-MATKL into VL_CHAVE.
        else.
          concatenate PW_PREVISAO-HKONT VL_CONTA_PART VL_BSCHL
                      PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                      VL_XREF3 VL_KOSTL VL_PRCTR
                      VL_ZUONR VL_AUFNR VL_ANLN1 into VL_CHAVE.
        endif.
      else.
        concatenate PW_PREVISAO-HKONT VL_HKONT VL_BSCHL
                    PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                    VL_XREF3 VL_KOSTL VL_PRCTR
                    VL_ZUONR VL_AUFNR VL_ANLN1
                    into VL_CHAVE.
      endif.
    endif.

  elseif P_TP = 'AR'.

    case PW_PREVISAO-BLART.
      when 'RV'.
        check PW_PREVISAO-ZLSCH ne 'P'.

        select single MATKL into VL_MATKL
          from VBAP
         where VBELN eq PW_PREVISAO-VBEL2.

        concatenate PW_PREVISAO-HKONT
                    VL_HKONT
                    PW_PREVISAO-BLART
                    VL_MATKL into VL_CHAVE.

      when 'DZ'.
        check PW_PREVISAO-ZLSCH ne 'P'.

* ---> S4 Migration - 15/06/2023 - MA
*        SELECT SINGLE zuonr INTO vg_zuonr
*          FROM bseg
*         WHERE bukrs EQ pw_previsao-bukrs
*           AND belnr EQ pw_previsao-belnr
*           AND gjahr EQ pw_previsao-gjahr.

*        data:   LT_BSEG type FAGL_T_BSEG.

        call function 'FAGL_GET_BSEG'
          exporting
            I_BUKRS   = PW_PREVISAO-BUKRS
            I_BELNR   = PW_PREVISAO-BELNR
            I_GJAHR   = PW_PREVISAO-GJAHR
          importing
            ET_BSEG   = LT_BSEG
          exceptions
            NOT_FOUND = 1
            others    = 2.

        read table LT_BSEG into data(LS_BSEG) index 1.

        if SY-SUBRC = 0.
          move LS_BSEG-ZUONR to VG_ZUONR.
        endif.
*<--- S4 Migration - 15/06/2023 - MA
        if SY-SUBRC is initial.
          PW_PREVISAO-VBEL2 = VG_ZUONR(10).

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              INPUT  = PW_PREVISAO-VBEL2
            importing
              OUTPUT = PW_PREVISAO-VBEL2.

          select single MATKL into VL_MATKL
            from VBAP
           where VBELN eq PW_PREVISAO-VBEL2.

          concatenate PW_PREVISAO-HKONT
                      PW_PREVISAO-BSCHL
                      PW_PREVISAO-UMSKZ
                      PW_PREVISAO-BLART
                      VL_MATKL into VL_CHAVE.
        endif.

      when others.

        select single NAME1
          from KNA1
          into VL_CLIFOR
         where KUNNR eq PW_PREVISAO-CLIFOR.

        concatenate PW_PREVISAO-HKONT VL_HKONT VL_BSCHL
                PW_PREVISAO-UMSKZ PW_PREVISAO-BLART
                VL_XREF3 VL_KOSTL VL_PRCTR
                VL_ZUONR VL_AUFNR VL_ANLN1
                into VL_CHAVE.
    endcase.

  endif.

  perform PESQUISA_CHAVE_PREV
    using PW_PREVISAO VL_AWKEY VL_CHAVE VL_CONTA_FINANC.

  if VL_CONTA_FINANC is not initial.

    WA_MOVXRT_PRE-ACRESC_BLQTO  = 0.
    WA_MOVXRT_PRE-AGE_CODIGO    = VL_BANKL+5.
    WA_MOVXRT_PRE-BAN_CODIGO    = VL_BANKL(4).
    WA_MOVXRT_PRE-CNT_CODIGO    = VL_BANKN.

    NU_DIAS = PW_PREVISAO-ZBD1T.
    WA_MOVXRT_PRE-DT_COMPETENCIA = PW_PREVISAO-ZFBDT.

    while NU_DIAS ne 0.

      if NU_DIAS > 30.
        NU_DIAS = NU_DIAS - 30.
        DIAS = 30.
      else.
        DIAS = NU_DIAS.
        NU_DIAS = 0.
      endif.

      call function 'RP_CALC_DATE_IN_INTERVAL'
        exporting
          DATE      = WA_MOVXRT_PRE-DT_COMPETENCIA
          DAYS      = DIAS
          MONTHS    = 0
          YEARS     = 0
        importing
          CALC_DATE = WA_MOVXRT_PRE-DT_COMPETENCIA.

    endwhile.

    WA_MOVXRT_PRE-DT_CONTABIL       = PW_PREVISAO-BUDAT.
    WA_MOVXRT_PRE-DT_PAGAMENTO      = WA_MOVXRT_PRE-DT_COMPETENCIA.
    WA_MOVXRT_PRE-DOC_PAGADOR       = PW_PREVISAO-BELNR.
    WA_MOVXRT_PRE-EMITIDO_FLAG      = 'S'.
    concatenate VL_SGTXT ' - ' VL_CLIFOR into WA_MOVXRT_PRE-FINALIDADE.
    concatenate PW_PREVISAO-BLART ' - Ch.: ' VL_CHAVE into WA_MOVXRT_PRE-HISTORICO.
    if PW_PREVISAO-BLART eq 'RV'.
      concatenate 'Cliente:' PW_PREVISAO-KUNNR 'Ordem:' PW_PREVISAO-VBEL2 'Ref.doc:' PW_PREVISAO-BELNR into WA_MOVXRT_PRE-INF_CONTABEIS separated by SPACE.
    else.
      concatenate 'Ref.doc.' PW_PREVISAO-BELNR into WA_MOVXRT_PRE-INF_CONTABEIS separated by SPACE.
    endif.
    WA_MOVXRT_PRE-MAN_AUT           = 'N'.
    WA_MOVXRT_PRE-MMI_CODIGO        = VL_CONTA_FINANC.

    if P_TP eq 'AP'.
      WA_MOVXRT_PRE-MMI_ENT_SAI     = 'S'.
    elseif P_TP eq 'AR'.
      WA_MOVXRT_PRE-MMI_ENT_SAI     = 'E'.
    endif.

    concatenate PW_PREVISAO-BLART '-' P_TP into WA_MOVXRT_PRE-ORIGEM_CONTABIL.

    concatenate VL_AWKEY PW_PREVISAO-BUZEI into WA_MOVXRT_PRE-ORIGEM_PK.

    WA_MOVXRT_PRE-ORIGEM_PROCESSO   = 'ITF'.
    WA_MOVXRT_PRE-ORIGEM_SISTEMA    = 'SAP'.
    WA_MOVXRT_PRE-PFJ_CODIGO        = PW_PREVISAO-BUKRS.
    WA_MOVXRT_PRE-PFJ_ORIG_DEST     = PW_PREVISAO-GSBER.
    WA_MOVXRT_PRE-TX_CONV_CNT_FIXA  = 'N'.
    WA_MOVXRT_PRE-TX_CONV_CNT       = 1.
    WA_MOVXRT_PRE-TX_CONV_CORR_FIXA = 'N'.
    WA_MOVXRT_PRE-TDO_CODIGO        = PW_PREVISAO-BLART.
    if ( VG_TCODE = 'FBV1Z' ) or ( VG_TCODE = 'ESTORNO' ).
      WA_MOVXRT_PRE-TDP_CODIGO        = VG_TCODE.
    else.
      WA_MOVXRT_PRE-TDP_CODIGO        = PW_PREVISAO-ZLSCH.
    endif.
    WA_MOVXRT_PRE-TEMPERATURA       = P_TEMPERATURA.
    WA_MOVXRT_PRE-VALOR_ORIGINAL    = PW_PREVISAO-WRBTR.

    if PW_PREVISAO-WAERS ne 'BRL'.
      WA_MOVXRT_PRE-IND_CODIGO_REAL = PW_PREVISAO-WAERS.
      if PW_PREVISAO-WAERS ne 'USD'.
        WA_MOVXRT_PRE-VALOR         = PW_PREVISAO-WRBTR.
      else.
        WA_MOVXRT_PRE-VALOR         = PW_PREVISAO-DMBE2.
      endif.
      if ( PW_PREVISAO-DMBE2 eq 0 ) and ( PW_PREVISAO-DMBE2 is initial ).
        WA_MOVXRT_PRE-TX_CONV_CORR  = 0.
      else.
        WA_MOVXRT_PRE-TX_CONV_CORR  = ( PW_PREVISAO-DMBTR / PW_PREVISAO-DMBE2 ). "#EC CI_FLDEXT_OK[2610650]
      endif.
    else.
      WA_MOVXRT_PRE-VALOR           = PW_PREVISAO-DMBTR.
      WA_MOVXRT_REA-TX_CONV_CORR    = 1.
    endif.

    append WA_MOVXRT_PRE to IT_MOVXRT_PRE.

  else.

    WA_MOVXRT_PRE-ACRESC_BLQTO  = 0.
    WA_MOVXRT_PRE-AGE_CODIGO    = VL_BANKL+5.
    WA_MOVXRT_PRE-BAN_CODIGO    = VL_BANKL(4).
    WA_MOVXRT_PRE-CNT_CODIGO    = VL_BANKN.

    NU_DIAS = PW_PREVISAO-ZBD1T.
    WA_MOVXRT_PRE-DT_COMPETENCIA = PW_PREVISAO-ZFBDT.

    while NU_DIAS ne 0.
      if NU_DIAS > 30.
        NU_DIAS = NU_DIAS - 30.
        DIAS = 30.
      else.
        DIAS = NU_DIAS.
        NU_DIAS = 0.
      endif.

      call function 'RP_CALC_DATE_IN_INTERVAL'
        exporting
          DATE      = WA_MOVXRT_PRE-DT_COMPETENCIA
          DAYS      = DIAS
          MONTHS    = 0
          YEARS     = 0
        importing
          CALC_DATE = WA_MOVXRT_PRE-DT_COMPETENCIA.
    endwhile.

    WA_MOVXRT_PRE-DT_CONTABIL       = PW_PREVISAO-BUDAT.
    WA_MOVXRT_PRE-DT_PAGAMENTO      = WA_MOVXRT_PRE-DT_COMPETENCIA.
    WA_MOVXRT_PRE-DOC_PAGADOR       = PW_PREVISAO-BELNR.
    WA_MOVXRT_PRE-EMITIDO_FLAG      = 'S'.
    concatenate VL_SGTXT ' - ' VL_CLIFOR into WA_MOVXRT_PRE-FINALIDADE.
    concatenate PW_PREVISAO-BLART ' - Ch.: ' VL_CHAVE into WA_MOVXRT_PRE-HISTORICO.

    if PW_PREVISAO-BLART eq 'RV'.
      concatenate 'Ref.doc.' PW_PREVISAO-BELNR 'Cliente:' PW_PREVISAO-KUNNR 'Ordem:' PW_PREVISAO-VBEL2 into WA_MOVXRT_PRE-INF_CONTABEIS separated by SPACE.
    else.
      concatenate 'Ref.doc.' PW_PREVISAO-BELNR into WA_MOVXRT_PRE-INF_CONTABEIS separated by SPACE.
    endif.

    WA_MOVXRT_PRE-MAN_AUT           = 'N'.
    WA_MOVXRT_PRE-MMI_CODIGO        = '2.99.99'.

    if P_TP eq 'AP'.
      WA_MOVXRT_PRE-MMI_ENT_SAI     = 'S'.
    elseif P_TP eq 'AR'.
      WA_MOVXRT_PRE-MMI_ENT_SAI     = 'E'.
    endif.

    concatenate PW_PREVISAO-BLART '-' P_TP into WA_MOVXRT_PRE-ORIGEM_CONTABIL.

    concatenate VL_AWKEY PW_PREVISAO-BUZEI into WA_MOVXRT_PRE-ORIGEM_PK.

    WA_MOVXRT_PRE-ORIGEM_PROCESSO   = 'ITF'.
    WA_MOVXRT_PRE-ORIGEM_SISTEMA    = 'SAP'.
    WA_MOVXRT_PRE-PFJ_CODIGO        = PW_PREVISAO-BUKRS.
    WA_MOVXRT_PRE-PFJ_ORIG_DEST     = PW_PREVISAO-GSBER.
    WA_MOVXRT_PRE-TX_CONV_CNT_FIXA  = 'N'.
    WA_MOVXRT_PRE-TX_CONV_CNT       = 1.
    WA_MOVXRT_PRE-TX_CONV_CORR_FIXA = 'N'.
    WA_MOVXRT_PRE-TDO_CODIGO        = PW_PREVISAO-BLART.

    if ( VG_TCODE = 'FBV1Z' ) or ( VG_TCODE = 'ESTORNO' ).
      WA_MOVXRT_PRE-TDP_CODIGO        = VG_TCODE.
    else.
      WA_MOVXRT_PRE-TDP_CODIGO        = PW_PREVISAO-ZLSCH.
    endif.

    WA_MOVXRT_PRE-TEMPERATURA       = P_TEMPERATURA.
    WA_MOVXRT_PRE-VALOR_ORIGINAL    = PW_PREVISAO-WRBTR.

    if PW_PREVISAO-WAERS ne 'BRL'.
      WA_MOVXRT_PRE-IND_CODIGO_REAL = PW_PREVISAO-WAERS.
      if PW_PREVISAO-WAERS ne 'USD'.
        WA_MOVXRT_PRE-VALOR         = PW_PREVISAO-WRBTR.
      else.
        WA_MOVXRT_PRE-VALOR         = PW_PREVISAO-DMBE2.
      endif.
      if ( PW_PREVISAO-DMBE2 eq 0 ) and ( PW_PREVISAO-DMBE2 is initial ).
        WA_MOVXRT_PRE-TX_CONV_CORR  = 0.
      else.
        WA_MOVXRT_PRE-TX_CONV_CORR  = ( PW_PREVISAO-DMBTR / PW_PREVISAO-DMBE2 ). "#EC CI_FLDEXT_OK[2610650]
      endif.
    else.
      WA_MOVXRT_PRE-VALOR           = PW_PREVISAO-DMBTR.
      WA_MOVXRT_REA-TX_CONV_CORR    = 1.
    endif.

    append WA_MOVXRT_PRE to IT_MOVXRT_PRE.
  endif.

endform.                    " F_INSERT_PREV
*&---------------------------------------------------------------------*
*&      Form  F_PREVISAO_AR
*&---------------------------------------------------------------------*
*       Busco dados para retorno de Previsão AR
*----------------------------------------------------------------------*
form F_PREVISAO_AR .

  data: VL_DTINI      type SY-DATUM,
        VL_DTFIM      type SY-DATUM,
        IT_PREVISAO_2 like standard table of WA_PREVISAO.

  loop at IT_T001B into WA_T001B where MKOAR eq 'D'.

    if WA_T001B-FRPE1+1(2) gt '12'.
      concatenate WA_T001B-FRYE1
                  '12'
                  '01' into VL_DTINI.
    else.
      concatenate WA_T001B-FRYE1
                  WA_T001B-FRPE1+1(2)
                  '01' into VL_DTINI.
    endif.

    if WA_T001B-FRPE2+1(2) gt '12'.
      concatenate WA_T001B-FRYE2
                  '12'
                  '01' into VL_DTFIM.
    else.
      concatenate WA_T001B-FRYE2
                  WA_T001B-FRPE2+1(2)
                  '01' into VL_DTFIM.
    endif.

    call function 'RP_LAST_DAY_OF_MONTHS'
      exporting
        DAY_IN            = VL_DTFIM
      importing
        LAST_DAY_OF_MONTH = VL_DTFIM.

    clear: IT_BKPF,
           IT_PREVISAO_2,
           IT_PREVISAO.

    select BELNR BUKRS GJAHR
      from BKPF
      into corresponding fields of table IT_BKPF
     where BUKRS eq WA_T001B-BUKRS
       and AEDAT eq SY-DATUM
       and CPUDT lt VL_DTINI.

    if IT_BKPF is not initial.
      select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
             BLART GSBER DMBTR WRBTR SGTXT HKONT
             ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
             UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
             BSCHL KUNNR AUFNR VBEL2 KUNNR
        from BSID
        into corresponding fields of table IT_PREVISAO_2
         for all entries in IT_BKPF
       where BUKRS eq IT_BKPF-BUKRS
         and GJAHR eq IT_BKPF-GJAHR
         and BELNR eq IT_BKPF-BELNR
         and SHKZG eq 'S'.
    endif.

    select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
           BLART GSBER DMBTR WRBTR SGTXT HKONT
           ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
           UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
           BSCHL KUNNR AUFNR VBEL2 KUNNR
      from BSID
      into corresponding fields of table IT_PREVISAO
     where BUKRS eq WA_T001B-BUKRS
       and CPUDT ge VL_DTINI
       and SHKZG eq 'S'.

    loop at IT_PREVISAO_2 into WA_PREVISAO.
      append WA_PREVISAO to IT_PREVISAO.
    endloop.

    clear: IT_BKPF.

    perform PROCESSA_PREVISAO_AR.

  endloop.
endform.                    " F_PREVISAO_AR
*&---------------------------------------------------------------------*
*&      Form  F_REALIZADO_AP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form F_REALIZADO_AP .
  data: VL_DTINI type SY-DATUM,
        VL_DTFIM type SY-DATUM.
  loop at IT_T001B into WA_T001B where MKOAR eq 'K'.

    VL_DTFIM = SY-DATUM + 4.
    VL_DTINI = SY-DATUM - 4.

    select BUKRS AUGBL GJAHR BELNR BUZEI BUDAT
           AUGDT WAERS BLART GSBER DMBTR WRBTR
           SGTXT HKONT ZFBDT ZBD1T ZLSCH ZLSPR
           HBKID DMBE2 UMSKZ XREF1 XREF2 XREF3
           ZUONR ANLN1 BSCHL LIFNR EBELN EBELP
           UMSKS AUFNR AUGGJ
      from BSAK
      into corresponding fields of table IT_REALIZADO
     where BUKRS eq WA_T001B-BUKRS
       and AUGDT between VL_DTINI and VL_DTFIM
       and SHKZG eq 'H'.

    perform PROCESSA_REALIZADO_AP.

  endloop.
endform.                    " F_REALIZADO_AP
*&---------------------------------------------------------------------*
*&      Form  F_REALIZADO_AR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form F_REALIZADO_AR .
  data: VL_DTINI type SY-DATUM,
        VL_DTFIM type SY-DATUM.

  loop at IT_T001B into WA_T001B where MKOAR eq 'D'.

    VL_DTFIM = SY-DATUM + 4.
    VL_DTINI = SY-DATUM - 4.

    select BUKRS AUGBL GJAHR BELNR BUZEI BUDAT
           AUGDT WAERS BLART GSBER DMBTR WRBTR
           SGTXT HKONT ZFBDT ZBD1T ZLSCH ZLSPR
           HBKID DMBE2 UMSKZ XREF1 XREF2 XREF3
           ZUONR ANLN1 BSCHL KUNNR AUGGJ VBEL2
           KUNNR
      from BSAD
      into corresponding fields of table IT_REALIZADO
     where BUKRS eq WA_T001B-BUKRS
       and AUGDT between VL_DTINI and VL_DTFIM
       and SHKZG eq 'S'.

    perform PROCESSA_REALIZADO_AR.

  endloop.
endform.                    " F_REALIZADO_AR
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_REA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PW_REALIZADO  Dados para retorno de Previsto
*      -->P_TP          Tipo (AP/AR)
*----------------------------------------------------------------------*
form F_INSERT_REA  using    PW_REALIZADO  like WA_REALIZADO
                            P_TP.

  data: VL_BANKL        like T012-BANKL,
        NU_DIAS         like BSAD-ZBD1T,
        DIAS            like T5A4A-DLYDY,
        VL_BANKN        like T012K-BANKN,
        VL_CONTA_FINANC like ZGL008-CONTA_FINANC,
        VL_CONTA_PART   like ZGL008-CONTA_PART,
        VL_ZUONR        like ZGL008-ZUONR,
        VL_XREF3        like ZGL008-XREF3,
        VL_ANLN1        like ZGL008-ANLN1,
        VL_CHAVE(200),
        VL_KOSTL        like ZGL008-KOSTL,
        VL_PRCTR        like ZGL008-PRCTR,
        VL_AUFNR        like ZGL008-AUFNR,
        VL_BSCHL        like BSIK-BSCHL,
        VL_SGTXT        like BSIS-SGTXT,
        VL_HKONT        like BSIS-HKONT,
        VL_CLIFOR       like LFA1-NAME1,
        VL_MATKL        like VBAP-MATKL,
        VG_ZUONR        like BSEG-ZUONR.

  perform VALIDA_ZUONR using VL_ZUONR PW_REALIZADO-ZUONR.
  perform VALIDA_ZUONR using VL_XREF3 PW_REALIZADO-XREF3.
  perform VALIDA_ANLN1 using VL_ANLN1 PW_REALIZADO-ANLN1.

  if ( PW_REALIZADO-BLART eq 'MA' ) or
     ( PW_REALIZADO-BLART eq 'NG' ) or
     ( PW_REALIZADO-BLART eq 'NC' ) or
     ( PW_REALIZADO-BLART eq 'NL' ).
    VL_XREF3 = PW_REALIZADO-XREF3.
  endif.

  clear WA_MOVXRT_REA.

  clear: WA_BLART.

  select single BLART AUGBL AUGGJ BUDAT AUGDT
    into WA_BLART
    from BSAK
   where BUKRS eq PW_REALIZADO-BUKRS
     and BELNR eq PW_REALIZADO-AUGBL
     and GJAHR eq PW_REALIZADO-AUGGJ
     and AUGBL ne PW_REALIZADO-AUGBL.

  if WA_BLART-BLART eq 'AG'.
    perform PESQUISA_CONTAS_BANCOS_EMPRESA
      using PW_REALIZADO-BUKRS WA_BLART-AUGBL WA_BLART-AUGGJ VL_BANKL VL_BANKN.
    PW_REALIZADO-BUDAT = WA_BLART-BUDAT.
    PW_REALIZADO-AUGDT = WA_BLART-AUGDT.
  else.
    perform PESQUISA_CONTAS_BANCOS_EMPRESA
      using PW_REALIZADO-BUKRS PW_REALIZADO-AUGBL PW_REALIZADO-AUGGJ VL_BANKL VL_BANKN.
  endif.

  clear: VL_BSCHL,
         VL_KOSTL,
         VL_PRCTR,
         VL_AUFNR,
         VL_HKONT.

  if PW_REALIZADO-UMSKZ is not initial.

    VL_BSCHL = PW_REALIZADO-BSCHL.
    VL_SGTXT = PW_REALIZADO-SGTXT.

  else.

    select BUKRS HKONT GJAHR BELNR BUZEI SGTXT KOSTL PRCTR AUFNR BSCHL
      from BSIS
      into table IT_BSIS
     where BUKRS eq PW_REALIZADO-BUKRS
       and HKONT ne PW_REALIZADO-HKONT
       and GJAHR eq PW_REALIZADO-GJAHR
       and BELNR eq PW_REALIZADO-BELNR.

    read table IT_BSIS into WA_BSIS with key BUKRS = PW_REALIZADO-BUKRS
                                             GJAHR = PW_REALIZADO-GJAHR
                                             BELNR = PW_REALIZADO-BELNR.
    if SY-SUBRC eq 0.
      VL_KOSTL = WA_BSIS-KOSTL.
      VL_PRCTR = WA_BSIS-PRCTR.
      VL_AUFNR = WA_BSIS-AUFNR.
      VL_SGTXT = WA_BSIS-SGTXT.
      if PW_REALIZADO-ANLN1 is initial.
        VL_HKONT = WA_BSIS-HKONT.
      endif.
    endif.

  endif.

  if P_TP = 'AP'.

    select single NAME1
      from LFA1
      into VL_CLIFOR
     where LIFNR eq PW_REALIZADO-CLIFOR.

    clear: VL_KOSTL,
           VL_PRCTR,
           VL_AUFNR.

    if PW_REALIZADO-UMSKS eq 'A'.

* Limpar work area e tabela interna, pois se trata de uma variavel global
* com isso estava acontecendo no select abaixo que a tabela interna
* estava sendo limpa, pois ebeln e ebelp estavam vazios, porem a work area
* no read table, quando retronado sy-subrc igual a 4, ela não é limpa,
* portanto a work area poderia ter "sujeira" de um outro read table bem
* sucedido.

      clear: IT_EKPO, WA_EKPO.

      if ( PW_REALIZADO-EBELN is not initial ) and
         ( PW_REALIZADO-EBELP is not initial ).

        select EBELN EBELP KNTTP MATKL
          from EKPO
          into table IT_EKPO
         where EBELN eq PW_REALIZADO-EBELN
           and EBELP eq PW_REALIZADO-EBELP.

      endif.

      read table IT_EKPO into WA_EKPO with key EBELN = PW_REALIZADO-EBELN
                                               EBELP = PW_REALIZADO-EBELP.

      if ( WA_EKPO-KNTTP is initial ) and ( IT_EKPO is not initial ).
        concatenate PW_REALIZADO-HKONT VL_HKONT VL_BSCHL
                    PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                    VL_XREF3 VL_KOSTL
                    VL_PRCTR VL_ZUONR
                    VL_AUFNR VL_ANLN1
                    WA_EKPO-MATKL
                    into VL_CHAVE.
      elseif ( WA_EKPO-KNTTP eq 'A' ) and ( IT_EKPO is not initial ).
        concatenate PW_REALIZADO-HKONT VL_HKONT VL_BSCHL
                  PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                  VL_XREF3 VL_KOSTL
                  VL_PRCTR VL_ZUONR
                  VL_AUFNR VL_ANLN1
                  WA_EKPO-KNTTP
                  into VL_CHAVE.
      else.
        concatenate PW_REALIZADO-HKONT VL_HKONT VL_BSCHL
                 PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                 VL_XREF3 VL_KOSTL
                 VL_PRCTR VL_ZUONR
                 VL_AUFNR VL_ANLN1
                 into VL_CHAVE.
      endif.
    else.
      if PW_REALIZADO-BLART eq 'RE'.
* ---> S4 Migration - 16/06/2023 - MA
*        select BUKRS BELNR GJAHR EBELN EBELP MATNR HKONT BSCHL
*          from BSEG
*          into table IT_BSEG
*         where BUKRS   eq PW_REALIZADO-BUKRS
*           and BELNR  eq PW_REALIZADO-BELNR
*           and GJAHR  eq PW_REALIZADO-GJAHR
*           and HKONT  in ('0000212100','0000511002')
*           and EBELN  ne ''.

        data:   LT_BSEG type FAGL_T_BSEG.
        types: LR_HKONT_TYPE type range of HKONT.
        data : LR_HKONT type LR_HKONT_TYPE.

        LR_HKONT = value LR_HKONT_TYPE( let S = 'I'
                                            O = 'BT'
                                        in SIGN   = S
                                           OPTION = O
                                           ( LOW = '0000212100' )
                                           ( LOW = '0000511002' ) ).

        call function 'FAGL_GET_BSEG'
          exporting
            I_BUKRS   = PW_REALIZADO-BUKRS
            I_BELNR   = PW_REALIZADO-BELNR
            I_GJAHR   = PW_REALIZADO-GJAHR
          importing
            ET_BSEG   = LT_BSEG
          exceptions
            NOT_FOUND = 1
            others    = 2.
        if SY-SUBRC <> 0.
* Implement suitable error handling here
        endif.

        delete LT_BSEG where BELNR eq ''.

        if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
          move-corresponding LT_BSEG to IT_BSEG.
          SY-DBCNT = LINES( LT_BSEG ).
        else.
          SY-SUBRC = 4.
          SY-DBCNT = 0.
        endif.
* <--- S4 Migration - 16/06/2023 - MA
        clear WA_BSEG.

        if SY-SUBRC eq 0.
          read table IT_BSEG into WA_BSEG with key BUKRS = PW_REALIZADO-BUKRS
                                                   BELNR = PW_REALIZADO-BELNR
                                                   GJAHR = PW_REALIZADO-GJAHR.
          select EBELN EBELP KNTTP MATKL
            from EKPO
            into table IT_EKPO
           where EBELN eq WA_BSEG-EBELN
             and EBELP eq WA_BSEG-EBELP.
        endif.

        if ( WA_BSEG-HKONT eq '0000212100' ) or
           ( WA_BSEG-HKONT eq '0000511002' ).

          select EBELN EBELP SAKTO
            from EKKN
            into table IT_EKKN
           where EBELN eq WA_BSEG-EBELN
             and EBELP eq WA_BSEG-EBELP.

          read table IT_EKKN into WA_EKKN with key EBELN = WA_BSEG-EBELN
                                                   EBELP = WA_BSEG-EBELP.
          clear VL_CONTA_PART.

          if ( SY-SUBRC eq 0 ) and ( PW_REALIZADO-ANLN1 is initial ).
            VL_CONTA_PART = WA_EKKN-SAKTO.
          endif.
        else.
          VL_CONTA_PART = WA_BSIS-HKONT.
        endif.

        clear WA_EKPO.

        if ( WA_BSEG-EBELN is not initial ) and
           ( WA_BSEG-EBELP is not initial ).
          read table IT_EKPO into WA_EKPO with key EBELN = WA_BSEG-EBELN
                                                   EBELP = WA_BSEG-EBELP.
        endif.

        if ( WA_EKPO-KNTTP is initial ) and ( WA_EKPO is not initial ).
          concatenate PW_REALIZADO-HKONT VL_CONTA_PART VL_BSCHL
                      PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                      VL_XREF3 VL_KOSTL VL_PRCTR
                      VL_ZUONR VL_AUFNR VL_ANLN1
                      WA_EKPO-KNTTP WA_EKPO-MATKL
                      into VL_CHAVE.
        else.
          concatenate PW_REALIZADO-HKONT VL_CONTA_PART VL_BSCHL
                      PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                      VL_XREF3 VL_KOSTL VL_PRCTR
                      VL_ZUONR VL_AUFNR VL_ANLN1
                      into VL_CHAVE.
        endif.
      else.
        concatenate PW_REALIZADO-HKONT VL_HKONT VL_BSCHL
                    PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                    VL_XREF3 VL_KOSTL VL_PRCTR
                    VL_ZUONR VL_AUFNR VL_ANLN1
                    into VL_CHAVE.
      endif.
    endif.

  else.

    case PW_REALIZADO-BLART.
      when 'RV'.
        check PW_REALIZADO-ZLSCH ne 'P'.

        select single MATKL into VL_MATKL
          from VBAP
         where VBELN eq PW_REALIZADO-VBEL2.

        concatenate PW_REALIZADO-HKONT
                    VL_HKONT
                    PW_REALIZADO-BLART
                    VL_MATKL           into VL_CHAVE.

      when 'DZ'.
        check PW_REALIZADO-ZLSCH ne 'P'.

* ---> S4 Migration - 15/06/2023 - MA
*        select single ZUONR into VG_ZUONR
*          from BSEG
*         where BUKRS eq PW_REALIZADO-BUKRS
*           and BELNR eq PW_REALIZADO-BELNR
*           and GJAHR eq PW_REALIZADO-GJAHR.

        call function 'FAGL_GET_BSEG'
          exporting
            I_BUKRS   = PW_REALIZADO-BUKRS
            I_BELNR   = PW_REALIZADO-BELNR
            I_GJAHR   = PW_REALIZADO-GJAHR
          importing
            ET_BSEG   = LT_BSEG
          exceptions
            NOT_FOUND = 1
            others    = 2.

        read table LT_BSEG into data(LS_BSEG) index 1.

        if SY-SUBRC = 0.
          move LS_BSEG-ZUONR to VG_ZUONR.
        endif.

*<--- S4 Migration - 15/06/2023 - MA
        if SY-SUBRC is initial.
          PW_REALIZADO-VBEL2 = VG_ZUONR(10).

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              INPUT  = PW_REALIZADO-VBEL2
            importing
              OUTPUT = PW_REALIZADO-VBEL2.

          select single MATKL into VL_MATKL
            from VBAP
           where VBELN eq PW_REALIZADO-VBEL2.

          concatenate PW_REALIZADO-BLART VL_MATKL into VL_CHAVE.
        endif.

      when others.

        select single NAME1
          from KNA1
          into VL_CLIFOR
         where KUNNR eq PW_REALIZADO-CLIFOR.

        concatenate PW_REALIZADO-HKONT VL_HKONT VL_BSCHL
                    PW_REALIZADO-UMSKZ PW_REALIZADO-BLART
                    VL_XREF3 VL_KOSTL VL_PRCTR
                    VL_ZUONR VL_AUFNR VL_ANLN1
                    into VL_CHAVE.
    endcase.

  endif.

  perform PESQUISA_CHAVE_REAL
    using PW_REALIZADO VL_AWKEY VL_CHAVE VL_CONTA_FINANC.

  if PW_REALIZADO-WAERS ne 'BRL'.

*    DATA: BEGIN OF WA_REAIS,
*            DMBTR LIKE BSEG-DMBTR,
*            RDIFF LIKE BSEG-RDIFF,
*          END OF WA_REAIS.
*
*    SELECT SINGLE DMBTR RDIFF
*      INTO WA_REAIS
*      FROM BSEG
*     WHERE BUKRS EQ PW_REALIZADO-BUKRS
*       AND BELNR EQ PW_REALIZADO-BELNR
*       AND GJAHR EQ PW_REALIZADO-GJAHR
*       AND AUGBL EQ PW_REALIZADO-AUGBL
*       AND BUZEI EQ PW_REALIZADO-BUZEI.
*
*    IF SY-SUBRC EQ 0.
*      PW_REALIZADO-DMBTR = WA_REAIS-DMBTR + WA_REAIS-RDIFF.
*    ENDIF.

    data: TX_COTACAO_MOEDA like BKPF-KURSF.

    select single KURSF
      into TX_COTACAO_MOEDA
      from BKPF
     where BUKRS eq PW_REALIZADO-BUKRS
       and BELNR eq PW_REALIZADO-AUGBL
       and GJAHR eq PW_REALIZADO-AUGGJ.

  endif.

  if VL_CONTA_FINANC is not initial.

    WA_MOVXRT_REA-ACRESC_BLQTO      = 0.
    WA_MOVXRT_REA-AGE_CODIGO        = VL_BANKL+5.
    WA_MOVXRT_REA-BAN_CODIGO        = VL_BANKL(4).
    WA_MOVXRT_REA-CNT_CODIGO        = VL_BANKN.

    NU_DIAS = PW_REALIZADO-ZBD1T.
    WA_MOVXRT_REA-DT_COMPETENCIA = PW_REALIZADO-ZFBDT.

    while NU_DIAS ne 0.
      if NU_DIAS > 30.
        NU_DIAS = NU_DIAS - 30.
        DIAS = 30.
      else.
        DIAS = NU_DIAS.
        NU_DIAS = 0.
      endif.

      call function 'RP_CALC_DATE_IN_INTERVAL'
        exporting
          DATE      = WA_MOVXRT_REA-DT_COMPETENCIA
          DAYS      = DIAS
          MONTHS    = 0
          YEARS     = 0
        importing
          CALC_DATE = WA_MOVXRT_REA-DT_COMPETENCIA.
    endwhile.

    WA_MOVXRT_REA-DT_CONTABIL       = PW_REALIZADO-BUDAT.
    WA_MOVXRT_REA-DT_PAGAMENTO      = PW_REALIZADO-AUGDT.
    WA_MOVXRT_REA-DOC_PAGADOR       = PW_REALIZADO-AUGBL.
    WA_MOVXRT_REA-DOC_ORIGEM        = PW_REALIZADO-BELNR.
    WA_MOVXRT_REA-EMITIDO_FLAG      = 'S'.
    concatenate VL_SGTXT ' - ' VL_CLIFOR into WA_MOVXRT_REA-FINALIDADE.
    concatenate PW_REALIZADO-BLART ' - Ch.: ' VL_CHAVE into WA_MOVXRT_REA-HISTORICO.

    if PW_REALIZADO-BLART eq 'RV'.
      concatenate 'Ref.doc.' PW_REALIZADO-BELNR 'Cliente:' PW_REALIZADO-KUNNR 'Ordem:' PW_REALIZADO-VBEL2 into WA_MOVXRT_REA-INF_CONTABEIS separated by SPACE.
    else.
      concatenate 'Ref.doc.' PW_REALIZADO-BELNR into WA_MOVXRT_REA-INF_CONTABEIS separated by SPACE.
    endif.

    WA_MOVXRT_REA-MAN_AUT           = 'N'.
    WA_MOVXRT_REA-MMI_CODIGO        = VL_CONTA_FINANC.
    if P_TP eq 'AP'.
      WA_MOVXRT_REA-MMI_ENT_SAI     = 'S'.
    elseif P_TP eq 'AR'.
      WA_MOVXRT_REA-MMI_ENT_SAI     = 'E'.
    endif.

    concatenate PW_REALIZADO-BLART '-' P_TP into WA_MOVXRT_REA-ORIGEM_CONTABIL.
    WA_MOVXRT_REA-ORIGEM_PK         = VL_AWKEY.
    WA_MOVXRT_REA-ORIGEM_PROCESSO   = 'ITF'.
    WA_MOVXRT_REA-ORIGEM_SISTEMA    = 'SAP'.
    WA_MOVXRT_REA-PFJ_CODIGO        = PW_REALIZADO-BUKRS.
    WA_MOVXRT_REA-PFJ_ORIG_DEST     = PW_REALIZADO-GSBER.
    WA_MOVXRT_REA-TX_CONV_CNT_FIXA  = 'N'.
    WA_MOVXRT_REA-TX_CONV_CNT       = 1.
    WA_MOVXRT_REA-TX_CONV_CORR_FIXA = 'N'.
    WA_MOVXRT_REA-TDO_CODIGO        = PW_REALIZADO-BLART.
    WA_MOVXRT_REA-TDP_CODIGO        = PW_REALIZADO-ZLSCH.
    WA_MOVXRT_REA-TEMPERATURA       = '-1'.
    WA_MOVXRT_REA-VALOR_ORIGINAL    = PW_REALIZADO-WRBTR.


    if PW_REALIZADO-WAERS ne 'BRL'.
      WA_MOVXRT_REA-IND_CODIGO_REAL = PW_REALIZADO-WAERS.
      if PW_REALIZADO-WAERS ne 'USD'.
        WA_MOVXRT_REA-VALOR         = PW_REALIZADO-WRBTR.
      else.
        WA_MOVXRT_REA-VALOR         = PW_REALIZADO-DMBE2.
      endif.
      if ( PW_REALIZADO-DMBE2 eq 0 ) and ( PW_REALIZADO-DMBE2 is initial ).
        WA_MOVXRT_PRE-TX_CONV_CORR  = 0.
      else.
*        WA_MOVXRT_REA-TX_CONV_CORR  = ( PW_REALIZADO-DMBTR / PW_REALIZADO-DMBE2 ).
        WA_MOVXRT_REA-TX_CONV_CORR  = TX_COTACAO_MOEDA.
      endif.
    else.
      WA_MOVXRT_REA-VALOR           = PW_REALIZADO-DMBTR.
      WA_MOVXRT_REA-TX_CONV_CORR    = 1.
    endif.

    append WA_MOVXRT_REA to IT_MOVXRT_REA.

  else.

    WA_MOVXRT_REA-ACRESC_BLQTO      = 0.
    WA_MOVXRT_REA-AGE_CODIGO        = VL_BANKL+5.
    WA_MOVXRT_REA-BAN_CODIGO        = VL_BANKL(4).
    WA_MOVXRT_REA-CNT_CODIGO        = VL_BANKN.

    NU_DIAS = PW_REALIZADO-ZBD1T.
    WA_MOVXRT_REA-DT_COMPETENCIA = PW_REALIZADO-ZFBDT.

    while NU_DIAS ne 0.
      if NU_DIAS > 30.
        NU_DIAS = NU_DIAS - 30.
        DIAS = 30.
      else.
        DIAS = NU_DIAS.
        NU_DIAS = 0.
      endif.

      call function 'RP_CALC_DATE_IN_INTERVAL'
        exporting
          DATE      = WA_MOVXRT_REA-DT_COMPETENCIA
          DAYS      = DIAS
          MONTHS    = 0
          YEARS     = 0
        importing
          CALC_DATE = WA_MOVXRT_REA-DT_COMPETENCIA.
    endwhile.

    WA_MOVXRT_REA-DT_CONTABIL       = PW_REALIZADO-BUDAT.
    WA_MOVXRT_REA-DT_PAGAMENTO      = PW_REALIZADO-AUGDT.
    WA_MOVXRT_REA-DOC_PAGADOR       = PW_REALIZADO-AUGBL.
    WA_MOVXRT_REA-DOC_ORIGEM        = PW_REALIZADO-BELNR.
    WA_MOVXRT_REA-EMITIDO_FLAG      = 'S'.
    concatenate VL_SGTXT ' - ' VL_CLIFOR into WA_MOVXRT_REA-FINALIDADE.
    concatenate PW_REALIZADO-BLART ' - Ch.: ' VL_CHAVE into WA_MOVXRT_REA-HISTORICO.

    if PW_REALIZADO-BLART eq 'RV'.
      concatenate 'Ref.doc.' PW_REALIZADO-BELNR 'Cliente:' PW_REALIZADO-KUNNR 'Ordem:' PW_REALIZADO-VBEL2 into WA_MOVXRT_REA-INF_CONTABEIS separated by SPACE.
    else.
      concatenate 'Ref.doc.' PW_REALIZADO-BELNR into WA_MOVXRT_REA-INF_CONTABEIS separated by SPACE.
    endif.

    WA_MOVXRT_REA-MAN_AUT           = 'N'.
    WA_MOVXRT_REA-MMI_CODIGO        = '2.99.99'.
    if P_TP eq 'AP'.
      WA_MOVXRT_REA-MMI_ENT_SAI     = 'S'.
    elseif P_TP eq 'AR'.
      WA_MOVXRT_REA-MMI_ENT_SAI     = 'E'.
    endif.

    concatenate PW_REALIZADO-BLART '-' P_TP into WA_MOVXRT_REA-ORIGEM_CONTABIL.
    WA_MOVXRT_REA-ORIGEM_PK         = VL_AWKEY.

    WA_MOVXRT_REA-ORIGEM_PROCESSO   = 'ITF'.
    WA_MOVXRT_REA-ORIGEM_SISTEMA    = 'SAP'.
    WA_MOVXRT_REA-PFJ_CODIGO        = PW_REALIZADO-BUKRS.
    WA_MOVXRT_REA-PFJ_ORIG_DEST     = PW_REALIZADO-GSBER.
    WA_MOVXRT_REA-TX_CONV_CNT_FIXA  = 'N'.
    WA_MOVXRT_REA-TX_CONV_CNT       = 1.
    WA_MOVXRT_REA-TX_CONV_CORR_FIXA = 'N'.
    WA_MOVXRT_REA-TDO_CODIGO        = PW_REALIZADO-BLART.
    WA_MOVXRT_REA-TDP_CODIGO        = PW_REALIZADO-ZLSCH.
    WA_MOVXRT_REA-TEMPERATURA       = '-1'.
    WA_MOVXRT_REA-VALOR_ORIGINAL    = PW_REALIZADO-WRBTR.

    if PW_REALIZADO-WAERS ne 'BRL'.
      WA_MOVXRT_REA-IND_CODIGO_REAL = PW_REALIZADO-WAERS.
      if PW_REALIZADO-WAERS ne 'USD'.
        WA_MOVXRT_REA-VALOR         = PW_REALIZADO-WRBTR.
      else.
        WA_MOVXRT_REA-VALOR         = PW_REALIZADO-DMBE2.
      endif.
      if ( PW_REALIZADO-DMBE2 eq 0 ) and ( PW_REALIZADO-DMBE2 is initial ).
        WA_MOVXRT_PRE-TX_CONV_CORR  = 0.
      else.
*        WA_MOVXRT_REA-TX_CONV_CORR  = ( PW_REALIZADO-DMBTR / PW_REALIZADO-DMBE2 ).
        WA_MOVXRT_REA-TX_CONV_CORR  = TX_COTACAO_MOEDA.
      endif.
    else.
      WA_MOVXRT_REA-VALOR           = PW_REALIZADO-DMBTR.
      WA_MOVXRT_REA-TX_CONV_CORR    = 1.
    endif.

    append WA_MOVXRT_REA to IT_MOVXRT_REA.

  endif.
endform.                    " F_INSERT_REA
*&---------------------------------------------------------------------*
*&      Form  F_CHAMA_RFC
*&---------------------------------------------------------------------*
*       Chama RCF
*----------------------------------------------------------------------*
form F_CHAMA_RFC .
  if ( not IT_MOVXRT_REA[] is initial ).

    call function 'Z_FI_OUTBOUND_APAR_XRT_REA' in background task
      destination 'XI_XRT_AR_REA'
      as separate unit
      tables
        OUTDOCUMENT = IT_MOVXRT_REA.

    commit work.
  endif.

  if ( not IT_MOVXRT_PRE[] is initial ).

    call function 'Z_FI_OUTBOUND_APAR_XRT_PRE' in background task
      destination 'XI_XRT_AR_PRE'
      as separate unit
      tables
        OUTDOCUMENT = IT_MOVXRT_PRE.

    commit work.
  endif.

  clear: IT_MOVXRT_REA, IT_MOVXRT_PRE.

endform.                    " F_CHAMA_RFC

*&---------------------------------------------------------------------*
*&      Form  VALIDA_ANLN1
*&---------------------------------------------------------------------*
*       Verifica Valor do numero da atribuição
*----------------------------------------------------------------------*
form VALIDA_ANLN1  using    P_VL_ANLN1
                            P_PW_PREVISAO_ANLN1.
  clear: P_VL_ANLN1.
  if P_PW_PREVISAO_ANLN1 is not initial.
    if ( P_PW_PREVISAO_ANLN1(3) eq 'OBR' ).
      P_VL_ANLN1 = P_PW_PREVISAO_ANLN1(3).
    endif.
  endif.
endform.                    " VALIDA_ANLN1


*&---------------------------------------------------------------------*
*&      Form  VALIDA_ZUONR
*&---------------------------------------------------------------------*
*       Verifica Valor do numero da atribuição
*----------------------------------------------------------------------*
form VALIDA_ZUONR  using    P_VL_ZUONR
                            P_PW_PREVISAO_ZUONR.
  clear: P_VL_ZUONR.
  if P_PW_PREVISAO_ZUONR is not initial.
    if ( P_PW_PREVISAO_ZUONR+1(1) eq '.' ) and
       ( P_PW_PREVISAO_ZUONR+4(1) eq '.' ).
      if ( ( P_PW_PREVISAO_ZUONR+0(1) ge '0' ) and ( P_PW_PREVISAO_ZUONR+0(1) le '9' ) ) and
         ( ( P_PW_PREVISAO_ZUONR+2(1) ge '0' ) and ( P_PW_PREVISAO_ZUONR+2(1) le '9' ) ) and
         ( ( P_PW_PREVISAO_ZUONR+3(1) ge '0' ) and ( P_PW_PREVISAO_ZUONR+3(1) le '9' ) ) and
         ( ( P_PW_PREVISAO_ZUONR+5(1) ge '0' ) and ( P_PW_PREVISAO_ZUONR+5(1) le '9' ) ) and
         ( ( P_PW_PREVISAO_ZUONR+6(1) ge '0' ) and ( P_PW_PREVISAO_ZUONR+6(1) le '9' ) ).
        P_VL_ZUONR = P_PW_PREVISAO_ZUONR.
      endif.
    endif.
  endif.
endform.                    " VALIDA_ZUONR

*&---------------------------------------------------------------------*
*&      Form  F_PREVISAO_AP_PRE_EDITADO
*&---------------------------------------------------------------------*
*       Lançamentos de pagamento de documentos pré-editados.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form F_PREVISAO_AP_PRE_EDITADO .
  data: VL_DTINI type SY-DATUM,
        VL_DTFIM type SY-DATUM.
  loop at IT_T001B into WA_T001B where MKOAR eq 'K'.

    VL_DTFIM = SY-DATUM.
    VL_DTINI = VL_DTFIM - 1.

    select BUDAT WAERS BLART BELNR BUKRS AUSBK GJAHR
      from VBKPF
      into corresponding fields of table IT_VBKPF
     where AUSBK eq WA_T001B-BUKRS
       and CPUDT between VL_DTINI and VL_DTFIM.

    loop at IT_VBKPF into WA_VBKPF.

      select BUKRS GJAHR BELNR BUZEI GSBER
             DMBTR WRBTR SGTXT HKONT ZFBDT
             ZBD1T ZLSCH ZLSPR HBKID DMBE2
             UMSKZ XREF1 XREF2 XREF3 ZUONR
             BSCHL LIFNR UMSKS AUSBK
        from VBSEGK
        into corresponding fields of table IT_PREVISAO
       where AUSBK eq WA_VBKPF-AUSBK
         and BELNR eq WA_VBKPF-BELNR
         and GJAHR eq WA_VBKPF-GJAHR.

      loop at IT_PREVISAO into WA_PREVISAO.
        WA_PREVISAO-BUKRS = WA_PREVISAO-AUSBK.
        WA_PREVISAO-BUDAT = WA_VBKPF-BUDAT.
        WA_PREVISAO-WAERS = WA_VBKPF-WAERS.
        WA_PREVISAO-BLART = WA_VBKPF-BLART.
        WA_PREVISAO-CLIFOR = WA_PREVISAO-LIFNR.

        check WA_PREVISAO-BLART ne 'XR'.

        AUX_CONT = AUX_CONT + 1.
        if WA_PREVISAO-ZLSPR is initial.
          perform F_INSERT_PREV using WA_PREVISAO 'AP' '4'.
        else.
          perform F_INSERT_PREV using WA_PREVISAO 'AP' '8'.
        endif.
        if AUX_CONT > 999.
          perform F_CHAMA_RFC.
          AUX_CONT = 0.
        endif.
      endloop.

    endloop.

    select BUKRS GJAHR BELNR BLART WAERS BUDAT BKTXT AUSBK
      from BKPF
      into corresponding fields of table IT_BKPF
     where BUKRS eq WA_T001B-BUKRS
       and CPUDT between VL_DTINI and VL_DTFIM
       and BSTAT eq 'Z'.

    loop at IT_BKPF into WA_BKPF.

      clear: WA_PREVISAO.
      WA_PREVISAO-BUKRS  = WA_BKPF-BUKRS.
      WA_PREVISAO-GJAHR  = WA_BKPF-GJAHR.
      WA_PREVISAO-BELNR  = WA_BKPF-BELNR.
      WA_PREVISAO-BLART  = WA_BKPF-BLART.
      WA_PREVISAO-WAERS  = WA_BKPF-WAERS.
      WA_PREVISAO-BUDAT  = WA_BKPF-BUDAT.
      WA_PREVISAO-SGTXT  = WA_BKPF-BKTXT.
      WA_PREVISAO-AUSBK  = WA_BKPF-AUSBK.
      WA_PREVISAO-ZFBDT  = WA_BKPF-BUDAT.
      WA_PREVISAO-ZBD1T  = 0.
      WA_PREVISAO-DMBTR  = 0.
      WA_PREVISAO-WRBTR  = 0.
      VG_TCODE = 'FBV1Z'.
      perform F_INSERT_PREV using WA_PREVISAO 'AP' '4'.
      clear VG_TCODE.

    endloop.

  endloop.

endform.                    " F_PREVISAO_AP_PRE_EDITADO


*&---------------------------------------------------------------------*
*&      Form  PESQUISA_CHAVE_PREV
*&---------------------------------------------------------------------*
*       Procedimento de Busca de Conta Financeira XRT - Previsão
*----------------------------------------------------------------------*
*      -->P_PW_PREVISAO     - Registro de Previsão
*      -->P_VL_AWKEY        - Chave de Referência
*      -->P_VL_CHAVE        - Chave Montada de acordo com algoritmo
*      -->P_VL_CONTA_FINANC - Conta Financeira XRT (Resultado)
*----------------------------------------------------------------------*
form PESQUISA_CHAVE_PREV  using    P_PW_PREVISAO like WA_PREVISAO
                                   P_VL_AWKEY
                                   P_VL_CHAVE
                                   P_VL_CONTA_FINANC.

  if P_PW_PREVISAO-BLART eq 'MG'.
    perform PESQUISA_CHAVE_MG
      using P_PW_PREVISAO-BUKRS P_PW_PREVISAO-GJAHR
            P_PW_PREVISAO-BELNR P_PW_PREVISAO-BUZEI
            P_VL_AWKEY P_VL_CHAVE P_VL_CONTA_FINANC.
  else.
    perform PESQUISA_CHAVE using P_VL_AWKEY P_VL_CHAVE P_VL_CONTA_FINANC.
  endif.

endform.                    " PESQUISA_CHAVE_PREV

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_CHAVE_REAL
*&---------------------------------------------------------------------*
*       Procedimento de Busca de Conta Financeira XRT - Realizado
*----------------------------------------------------------------------*
*      -->P_PW_REALIZADO     - Registro de Realizado
*      -->P_VL_AWKEY         - Chave de Referência
*      -->P_VL_CHAVE         - Chave Montada de acordo com algoritmo
*      -->P_VL_CONTA_FINANC  - Conta Financeira XRT (Resultado)
*----------------------------------------------------------------------*
form PESQUISA_CHAVE_REAL  using    P_PW_REALIZADO like WA_REALIZADO
                                   P_VL_AWKEY
                                   P_VL_CHAVE
                                   P_VL_CONTA_FINANC.

  if P_PW_REALIZADO-BLART eq 'MG'.
    perform PESQUISA_CHAVE_MG
      using P_PW_REALIZADO-BUKRS P_PW_REALIZADO-GJAHR
            P_PW_REALIZADO-BELNR P_PW_REALIZADO-BUZEI
            P_VL_AWKEY P_VL_CHAVE P_VL_CONTA_FINANC.
  else.
    perform PESQUISA_CHAVE using P_VL_AWKEY P_VL_CHAVE P_VL_CONTA_FINANC.
  endif.

endform.                    " PESQUISA_CHAVE_REAL

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_CHAVE
*&---------------------------------------------------------------------*
*       Procedimento de Busca de Conta Financeira XRT
*----------------------------------------------------------------------*
*      -->P_VL_AWKEY         - Chave de Referência
*      -->P_VL_CHAVE         - Chave Montada de acordo com algoritmo
*      -->P_VL_CONTA_FINANC  - Conta Financeira XRT (Resultado)
*----------------------------------------------------------------------*
form PESQUISA_CHAVE  using    P_VL_AWKEY
                              P_VL_CHAVE
                              P_VL_CONTA_FINANC.

  clear: P_VL_CONTA_FINANC.

  perform PESQUISA_PEDIDO using P_VL_AWKEY P_VL_CONTA_FINANC.

  if P_VL_CONTA_FINANC is initial.
    loop at IT_ZGL008 into WA_ZGL008.
      if WA_ZGL008-CHAVE eq P_VL_CHAVE.
        P_VL_CONTA_FINANC = WA_ZGL008-CONTA_FINANC.
      endif.
    endloop.
  endif.

endform.                    " PESQUISA_CHAVE

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_PEDIDO
*&---------------------------------------------------------------------*
*       Localiza a ordem interna do pedido da fatura do documento contábil
*       Validando se esta ordem é do tipo ZOAN.
*----------------------------------------------------------------------*
*      -->P_VL_AWKEY  Chave de referencia do cabeçalho do documento contábil
*      -->P_VL_CONTA_FINANC  Conta Financeira XRT
*----------------------------------------------------------------------*
form PESQUISA_PEDIDO  using    P_VL_AWKEY
                               P_VL_CONTA_FINANC.

  data: VA_AUFNR like BSEG-AUFNR,
        VA_ZUONR like RBKP-ZUONR,

        begin of WA_EKKO,
          EBELN like EKKO-EBELN,
          LPONR like EKKO-LPONR,
        end of WA_EKKO.

  clear: VA_AUFNR,
         VA_ZUONR,
         WA_EKKO.

  select single ZUONR
    into VA_ZUONR
    from RBKP
   where BELNR eq P_VL_AWKEY(10)
     and GJAHR eq P_VL_AWKEY+10(4).

  if VA_ZUONR is not initial.

    select single EBELN LPONR
      from EKKO
      into WA_EKKO
     where EBELN eq VA_ZUONR
       and BSTYP eq 'F'.

    if WA_EKKO is not initial.

      select single AUFNR
        into VA_AUFNR
        from EKKN
       where EBELN eq WA_EKKO-EBELN
         and EBELP eq WA_EKKO-LPONR.

      if VA_AUFNR is not initial.

        clear WA_AUFK.

        select single AUART
          into WA_AUFK-AUART
          from AUFK
         where AUFNR = VA_AUFNR.

        if WA_AUFK-AUART = 'ZOAN'.
          P_VL_CONTA_FINANC = '4.02.01'.
        endif.

      endif.
    endif.
  endif.

endform.                    " PESQUISA_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_CONTAS_BANCOS_EMPRESA
*&---------------------------------------------------------------------*
*       Pesquisar Banco da Conta do Lançamento
*----------------------------------------------------------------------*
*      -->P_PW_PREVISAO_BUKRS  - Empresa
*      -->P_PW_PREVISAO_AUGBL  - Documento de Compensação
*      -->P_PW_REALIZADO_GJAHR - Ano do Documento
*      -->P_VL_BANKL           - Chave do banco (Retorno)
*      -->P_VL_BANKN           - Nº conta bancária (Retorno)
*----------------------------------------------------------------------*
form PESQUISA_CONTAS_BANCOS_EMPRESA  using    P_PW_REALIZADO_BUKRS
                                              P_PW_REALIZADO_AUGBL
                                              P_PW_REALIZADO_AUGGJ
                                              P_VL_BANKL
                                              P_VL_BANKN.

  data: P_SKB1_SAKNR like SKB1-SAKNR.

  data: begin of WA_T012K,
          BANKN like T012K-BANKN,
          HBKID like T012K-HBKID,
        end of WA_T012K,

        begin of WA_HKONT,
          HKONT like BSIS-HKONT,
        end of WA_HKONT.

  data: IT_HKONT  like standard table of WA_HKONT.

  clear : P_VL_BANKL, P_VL_BANKN, WA_T012K, P_SKB1_SAKNR, IT_HKONT.

  select HKONT
    from BSIS
    into table IT_HKONT
   where BUKRS eq P_PW_REALIZADO_BUKRS
     and BELNR eq P_PW_REALIZADO_AUGBL
     and GJAHR eq P_PW_REALIZADO_AUGGJ.

  loop at IT_HKONT into WA_HKONT.

    select single SAKNR "#EC CI_DB_OPERATION_OK[2431747]
      into P_SKB1_SAKNR
      from SKB1
     where BUKRS = P_PW_REALIZADO_BUKRS
       and SAKNR = WA_HKONT-HKONT
       and FDLEV = 'F0'.

    if SY-SUBRC eq 0.

      select single BANKN HBKID
        from T012K
        into WA_T012K
       where BUKRS eq P_PW_REALIZADO_BUKRS
         and HKONT eq WA_HKONT-HKONT.

      P_VL_BANKN = WA_T012K-BANKN.

      select single BANKL
        from T012
        into P_VL_BANKL
       where BUKRS eq P_PW_REALIZADO_BUKRS
         and HBKID eq WA_T012K-HBKID.

    endif.

  endloop.

endform.                    " PESQUISA_CONTAS_BANCOS_EMPRESA

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_CHAVE_MG
*&---------------------------------------------------------------------*
*       Procedimento de Busca de Conta Financeira XRT - Documento MG
*----------------------------------------------------------------------*
*      -->PMG_BUKRS           - Empresa
*      -->PMG_GJAHR           - Ano de Exercício
*      -->PMG_BELNR           - Nr. Documento
*      -->PMG_VL_AWKEY        - Chave de Referência
*      -->PMG_VL_CHAVE        - Chave Montada de acordo com algoritmo
*      -->PMG_VL_CONTA_FINANC - Conta Financeira XRT - (Resultado)
*----------------------------------------------------------------------*
form PESQUISA_CHAVE_MG  using    PMG_BUKRS
                                 PMG_GJAHR
                                 PMG_BELNR
                                 PMG_BUZEI
                                 PMG_VL_AWKEY
                                 PMG_VL_CHAVE
                                 PMG_VL_CONTA_FINANC.

  data: PMG_BSIS_PRCTR    like BSIS-PRCTR,
        AUX_VL_CHAVE(200).

  select single PRCTR
    into PMG_BSIS_PRCTR
    from BSIS
   where BUKRS eq PMG_BUKRS
     and GJAHR eq PMG_GJAHR
     and BELNR eq PMG_BELNR
     and BUZEI ne PMG_BUZEI.

  if PMG_BSIS_PRCTR is initial.
    perform PESQUISA_CHAVE using PMG_VL_AWKEY PMG_VL_CHAVE PMG_VL_CONTA_FINANC.
  else.
    concatenate PMG_VL_CHAVE PMG_BSIS_PRCTR into AUX_VL_CHAVE.
    perform PESQUISA_CHAVE using PMG_VL_AWKEY AUX_VL_CHAVE PMG_VL_CONTA_FINANC.

    if PMG_VL_CONTA_FINANC is initial.
      perform PESQUISA_CHAVE using PMG_VL_AWKEY PMG_VL_CHAVE PMG_VL_CONTA_FINANC.
    else.
      PMG_VL_CHAVE = AUX_VL_CHAVE.
    endif.
  endif.

endform.                    " PESQUISA_CHAVE_MG

*&---------------------------------------------------------------------*
*&      Form  PROCESSA_REALIZADO_AP
*&---------------------------------------------------------------------*
*       Processamento de realizado pagamento
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form PROCESSA_REALIZADO_AP .

  data VG_BLART like BSIK-BLART.

  loop at IT_REALIZADO into WA_REALIZADO.

    check WA_REALIZADO-AUGBL ne WA_REALIZADO-BELNR.
    check WA_REALIZADO-BLART ne 'AG'.
    check WA_REALIZADO-BLART ne 'XR'.

    if ( ( WA_REALIZADO-ZLSCH ne 'P' ) and
         ( WA_REALIZADO-ZLSCH is not initial ) ) or
       ( ( WA_REALIZADO-BLART eq 'TB' ) and
         ( WA_REALIZADO-ZLSCH eq 'P' ) ).

      clear: VL_AWKEY, VG_BLART.

      select single BLART
        into VG_BLART
        from BSIK
       where BELNR eq WA_REALIZADO-AUGBL
         and BUKRS eq WA_REALIZADO-BUKRS
         and GJAHR eq WA_REALIZADO-GJAHR.

      if VG_BLART ne 'AG'.

        if ( WA_REALIZADO-UMSKS is initial ) and ( WA_REALIZADO-BSCHL = 27 ).
          select AUGBL BELNR BUZEI
            from BSAK
            into corresponding fields of table IT_BSAK
           where BUKRS eq WA_REALIZADO-BUKRS
             and LIFNR eq WA_REALIZADO-CLIFOR
             and AUGBL eq WA_REALIZADO-AUGBL
             and BELNR ne WA_REALIZADO-AUGBL
             and BLART eq WA_REALIZADO-BLART.

          if SY-SUBRC eq 0.
            read table IT_BSAK into WA_BSAK with key AUGBL = WA_REALIZADO-AUGBL.
            concatenate WA_BSAK-BELNR WA_BSAK-BUZEI into VL_AWKEY.
            WA_REALIZADO-DMBE2 = 0.
            WA_REALIZADO-DMBTR = 0.
          endif.
        else.
          select single AWKEY
            from BKPF
            into VL_AWKEY
           where BUKRS eq WA_REALIZADO-BUKRS
             and BELNR eq WA_REALIZADO-BELNR
             and GJAHR eq WA_REALIZADO-GJAHR.

          concatenate VL_AWKEY WA_REALIZADO-BUZEI into VL_AWKEY.
        endif.

        AUX_CONT = AUX_CONT + 1.

        perform F_INSERT_REA using WA_REALIZADO 'AP'.
        if AUX_CONT > 999.
          perform F_CHAMA_RFC.
          AUX_CONT = 0.
        endif.
      else.
** Envio de documento agrupado sem pagamento de agrupamento.
************************************************************
        select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
               BLART GSBER DMBTR WRBTR SGTXT HKONT
               ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
               UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
               BSCHL LIFNR EBELN EBELP UMSKS AUFNR
          from BSAK
          into corresponding fields of table IT_PREVISAO
         where BUKRS eq WA_REALIZADO-BUKRS
           and GJAHR eq WA_REALIZADO-GJAHR
           and BELNR eq WA_REALIZADO-BELNR
           and SHKZG eq 'H'.

        select BELNR BUKRS GJAHR AWKEY
          from BKPF
          into corresponding fields of table IT_BKPF
          for all entries in IT_PREVISAO
         where BUKRS eq IT_PREVISAO-BUKRS
           and GJAHR eq IT_PREVISAO-GJAHR
           and BELNR eq IT_PREVISAO-BELNR.

        loop at IT_PREVISAO into WA_PREVISAO.

          check WA_PREVISAO-BLART ne 'XR'.

          if WA_PREVISAO-ZLSPR is initial.
            perform F_INSERT_PREV using WA_PREVISAO 'AP' '4'.
          else.
            perform F_INSERT_PREV using WA_PREVISAO 'AP' '8'.
          endif.

        endloop.
************************************************************
      endif.
    endif.
  endloop.

endform.                    " PROCESSA_REALIZADO_AP

*&---------------------------------------------------------------------*
*&      Form  PROCESSA_REALIZADO_AR
*&---------------------------------------------------------------------*
*       Processamento de realizado recebimento
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form PROCESSA_REALIZADO_AR .

  loop at IT_REALIZADO into WA_REALIZADO.

    check WA_REALIZADO-AUGBL ne WA_REALIZADO-BELNR.
    check WA_REALIZADO-BLART ne 'XR'.

    AUX_CONT = AUX_CONT + 1.
    if WA_REALIZADO-ZLSCH is not initial.

      clear VL_AWKEY.

      select single AWKEY
        from BKPF
        into VL_AWKEY
       where BUKRS eq WA_REALIZADO-BUKRS
         and BELNR eq WA_REALIZADO-BELNR
         and GJAHR eq WA_REALIZADO-GJAHR.

      concatenate VL_AWKEY WA_REALIZADO-BUZEI into VL_AWKEY.

      perform F_INSERT_REA using WA_REALIZADO 'AR'.
    endif.
    if AUX_CONT > 999.
      perform F_CHAMA_RFC.
      AUX_CONT = 0.
    endif.
  endloop.

endform.                    " PROCESSA_REALIZADO_AR

*&---------------------------------------------------------------------*
*&      Form  PROCESSA_PREVISAO_AR
*&---------------------------------------------------------------------*
*       Processamento de previsão de recebimento
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form PROCESSA_PREVISAO_AR .

  loop at IT_PREVISAO into WA_PREVISAO.

    check WA_PREVISAO-BLART ne 'XR'.

    if WA_PREVISAO-ZLSCH is not initial.
      AUX_CONT = AUX_CONT + 1.
      perform F_INSERT_PREV using WA_PREVISAO 'AR' '4'.
    endif.
    if AUX_CONT > 999.
      perform F_CHAMA_RFC.
      AUX_CONT = 0.
    endif.
  endloop.

endform.                    " PROCESSA_PREVISAO_AR

*&---------------------------------------------------------------------*
*&      Form  PROCESSA_PREVISAO_AP
*&---------------------------------------------------------------------*
*       Processamento de Previsão de pagamento
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form PROCESSA_PREVISAO_AP .

  data: VL_STBLG like RBKP-STBLG.

  loop at IT_PREVISAO into WA_PREVISAO.

    check WA_PREVISAO-BLART ne 'XR'.
* Alteração 1.13 ***************************************

    if ( ( WA_PREVISAO-BLART ne 'AG' ) and
         ( ( WA_PREVISAO-ZLSCH ne 'P' ) and
           ( WA_PREVISAO-ZLSCH is not initial ) ) ) or
       ( ( WA_PREVISAO-BLART eq 'TB' ) and
         ( WA_PREVISAO-ZLSCH eq 'P' ) ).

* Alteração 1.10 ***************************************
      clear: VL_STBLG.

      read table IT_BKPF into WA_BKPF with key BELNR = WA_PREVISAO-BELNR.
      if SY-SUBRC eq 0.
        WA_BKPF-AWKEY10 = WA_BKPF-AWKEY(10).

        select single STBLG
          from RBKP
          into VL_STBLG
         where BELNR eq WA_BKPF-AWKEY10
           and GJAHR eq WA_BKPF-GJAHR.

        if SY-SUBRC eq 0.
          if VL_STBLG is not initial.
            WA_PREVISAO-DMBTR = 0.
            WA_PREVISAO-DMBE2 = 0.
          endif.
        endif.

      endif.
* Alteração 1.10 ***************************************
* Alteração 1.13 ***************************************
      AUX_CONT = AUX_CONT + 1.
      if WA_PREVISAO-ZLSPR is initial.
        perform F_INSERT_PREV using WA_PREVISAO 'AP' '4'.
      else.
        perform F_INSERT_PREV using WA_PREVISAO 'AP' '8'.
      endif.
      if AUX_CONT > 999.
        perform F_CHAMA_RFC.
        AUX_CONT = 0.
      endif.
    endif.
  endloop.

endform.                    " PROCESSA_PREVISAO_AP

*&---------------------------------------------------------------------*
*&      Form  UNICO_DOCUMENTO
*&---------------------------------------------------------------------*
*       Retorno de um único documento
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form UNICO_DOCUMENTO .

  data: VG_BUKRS like BSAK-BUKRS,
        VG_BELNR like BSAK-AUGBL,
        VG_GJAHR like BSAK-GJAHR,
        VG_UNICO type C length 1 value 'N'.

  if VG_UNICO ne 'N'.

    perform UNICO_DOCUMENTO_PREVISSAO_AP using VG_BUKRS VG_BELNR VG_GJAHR.
    perform UNICO_DOCUMENTO_PREVISSAO_AR using VG_BUKRS VG_BELNR VG_GJAHR.
    perform UNICO_DOCUMENTO_REALIZADO_AP using VG_BUKRS VG_BELNR VG_GJAHR.
    perform UNICO_DOCUMENTO_REALIZADO_AR using VG_BUKRS VG_BELNR VG_GJAHR.

    if ( not IT_MOVXRT_REA[] is initial ) or
       ( not IT_MOVXRT_PRE[] is initial ).
      perform F_CHAMA_RFC.
    endif.

    leave program.

  endif.

endform.                    " UNICO_DOCUMENTO

*&---------------------------------------------------------------------*
*&      Form  UNICO_DOCUMENTO_PREVISSAO_AP
*&---------------------------------------------------------------------*
*       Retorno de um único documento de
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form UNICO_DOCUMENTO_PREVISSAO_AP  using    P_VG_BUKRS
                                            P_VG_BELNR
                                            P_VG_GJAHR.

  if P_VG_BUKRS is not initial.

    select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
           BLART GSBER DMBTR WRBTR SGTXT HKONT
           ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
           UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
           BSCHL LIFNR EBELN EBELP UMSKS AUFNR
      from BSIK
      into table IT_PREVISAO
     where BUKRS eq P_VG_BUKRS
       and BELNR eq P_VG_BELNR
       and GJAHR eq P_VG_GJAHR
       and SHKZG eq 'H'.

    clear: IT_BKPF.

    select BELNR BUKRS GJAHR AWKEY
      from BKPF
      into corresponding fields of table IT_BKPF
      for all entries in IT_PREVISAO
     where BUKRS eq IT_PREVISAO-BUKRS
       and GJAHR eq IT_PREVISAO-GJAHR
       and BELNR eq IT_PREVISAO-BELNR.

    perform PROCESSA_PREVISAO_AP.

  endif.

endform.                    " UNICO_DOCUMENTO_PREVISSAO_AP

*&---------------------------------------------------------------------*
*&      Form  UNICO_DOCUMENTO_PREVISSAO_AR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form UNICO_DOCUMENTO_PREVISSAO_AR  using    P_VG_BUKRS
                                            P_VG_BELNR
                                            P_VG_GJAHR.

  if P_VG_BUKRS is not initial.

    select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
           BLART GSBER DMBTR WRBTR SGTXT HKONT
           ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
           UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
           BSCHL KUNNR AUFNR
      from BSID
      into corresponding fields of table IT_PREVISAO
     where BUKRS eq P_VG_BUKRS
       and BELNR eq P_VG_BELNR
       and GJAHR eq P_VG_GJAHR
       and SHKZG eq 'S'.

    perform PROCESSA_PREVISAO_AR.

  endif.

endform.                    " UNICO_DOCUMENTO_PREVISSAO_AR

*&---------------------------------------------------------------------*
*&      Form  UNICO_DOCUMENTO_REALIZADO_AP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form UNICO_DOCUMENTO_REALIZADO_AP  using    P_VG_BUKRS
                                            P_VG_BELNR
                                            P_VG_GJAHR.

  if P_VG_BUKRS is not initial.

    select BUKRS AUGBL GJAHR BELNR BUZEI BUDAT
           AUGDT WAERS BLART GSBER DMBTR WRBTR
           SGTXT HKONT ZFBDT ZBD1T ZLSCH ZLSPR
           HBKID DMBE2 UMSKZ XREF1 XREF2 XREF3
           ZUONR ANLN1 BSCHL LIFNR EBELN EBELP
           UMSKS AUFNR AUGGJ
      from BSAK
      into corresponding fields of table IT_REALIZADO
     where BUKRS eq P_VG_BUKRS
       and AUGBL eq P_VG_BELNR
       and AUGGJ eq P_VG_GJAHR
       and SHKZG eq 'H'.

    perform PROCESSA_REALIZADO_AP.

  endif.

endform.                    " UNICO_DOCUMENTO_REALIZADO_AP

*&---------------------------------------------------------------------*
*&      Form  UNICO_DOCUMENTO_REALIZADO_AR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form UNICO_DOCUMENTO_REALIZADO_AR  using    P_VG_BUKRS
                                            P_VG_BELNR
                                            P_VG_GJAHR.

  if P_VG_BUKRS is not initial.

    select BUKRS AUGBL GJAHR BELNR BUZEI BUDAT
           AUGDT WAERS BLART GSBER DMBTR WRBTR
           SGTXT HKONT ZFBDT ZBD1T ZLSCH ZLSPR
           HBKID DMBE2 UMSKZ XREF1 XREF2 XREF3
           ZUONR ANLN1 BSCHL KUNNR AUGGJ
      from BSAD
      into corresponding fields of table IT_REALIZADO
     where BUKRS eq P_VG_BUKRS
       and AUGBL eq P_VG_BELNR
       and AUGGJ eq P_VG_GJAHR
       and SHKZG eq 'S'.

    perform PROCESSA_REALIZADO_AR.

  endif.

endform.                    " UNICO_DOCUMENTO_REALIZADO_AR

*&---------------------------------------------------------------------*
*&      Form  F_PREVISAO_AR_ORDENS
*&---------------------------------------------------------------------*
*       Busca Ordens de Venda saldo
*----------------------------------------------------------------------*
form F_PREVISAO_AR_ORDENS .

  data: DT_CRIACAO    type ERDAT,
        IT_CATEG      type table of LXHME_RANGE_C1 with header line,
        VG_TABIX      type SY-TABIX,
        VG_TABI1      type SY-TABIX,
        VL_CHAVE(200),
        VL_CONTA_FINC like ZGL008-CONTA_FINANC,
        DIAS          like T5A4A-DLYDY,
        NU_DIAS       like T052-ZTAG1.

  DT_CRIACAO = SY-DATUM - 540.

  clear: IT_VBAK[],     IT_VBAP[],    IT_VBFA[],   IT_VBFA_TOT[], IT_BKPF[],
         IT_PREVISAO[], IT_TOT_AB[],  IT_TOT_LQ[], IT_KONV[],     IT_TOT_AD[],
         IT_KNB1[], IT_VBKD[], IT_VBKD2[], IT_T052.

  select VBELN KNUMV VKORG KUNNR WAERK KALSM VTWEG
    into corresponding fields of table IT_VBAK
    from VBAK
   where ERDAT ge DT_CRIACAO.

  check SY-SUBRC is initial.

  select VBELN POSNR KWMENG MATNR GSBER
    into corresponding fields of table IT_VBAP
    from VBAP
     for all entries in IT_VBAK
   where VBELN eq IT_VBAK-VBELN.

  check SY-SUBRC is initial.

  IT_CATEG-SIGN   = 'I'.
  IT_CATEG-OPTION = 'EQ'.
  IT_CATEG-LOW    = 'M'.
  IT_CATEG-HIGH   = 'M'.
  append IT_CATEG.

  IT_CATEG-LOW    = 'N'.
  IT_CATEG-HIGH   = 'N'.
  append IT_CATEG.

  select VBELV POSNV VBELN RFMNG VBTYP_N ERDAT
    into corresponding fields of table IT_VBFA
    from VBFA
     for all entries in IT_VBAK
   where VBELV   eq IT_VBAK-VBELN
     and VBTYP_N in IT_CATEG.

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.

    read table IT_VBFA_TOT into WA_VBFA_TOT with key VBELV = WA_VBFA-VBELV
                                                     POSNV = WA_VBFA-POSNV.
    if SY-SUBRC is initial.
      VG_TABI1 = SY-TABIX.
      if WA_VBFA-VBTYP_N eq 'M'.
        WA_VBFA_TOT-RFMNG = WA_VBFA_TOT-RFMNG + WA_VBFA-RFMNG.
      else.
        WA_VBFA_TOT-RFMNG = WA_VBFA_TOT-RFMNG - WA_VBFA-RFMNG.
      endif.
      modify IT_VBFA_TOT index VG_TABI1 from WA_VBFA_TOT transporting RFMNG.
    else.
      WA_VBFA_TOT-VBELV = WA_VBFA-VBELV.
      WA_VBFA_TOT-POSNV = WA_VBFA-POSNV.
      if WA_VBFA-VBTYP_N eq 'M'.
        WA_VBFA_TOT-RFMNG = WA_VBFA-RFMNG.
      else.
        WA_VBFA_TOT-RFMNG = WA_VBFA-RFMNG * -1.
      endif.
      append WA_VBFA_TOT to IT_VBFA_TOT.
    endif.

  endloop.

* Servia para eliminar os zerados
*  LOOP AT it_vbfa_tot INTO wa_vbfa_tot.
*
*    DELETE it_vbap WHERE vbeln  EQ wa_vbfa_tot-vbelv
*                     AND posnr  EQ wa_vbfa_tot-posnv
*                     AND kwmeng LE wa_vbfa_tot-rfmng.
*
*  ENDLOOP.
*
*  MOVE it_vbak[] TO it_vbak2[].
*
*  LOOP AT it_vbak2 INTO wa_vbak.
*    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln.
*    IF NOT sy-subrc IS INITIAL.
*      DELETE it_vbak WHERE vbeln = wa_vbak-vbeln.
*      DELETE it_vbfa WHERE vbelv = wa_vbak-vbeln.
*    ENDIF.
*  ENDLOOP.

  select VBELN POSNR VALDT ZTERM ZLSCH
    into corresponding fields of table IT_VBKD
    from VBKD
     for all entries in IT_VBAP
   where VBELN eq IT_VBAP-VBELN
     and POSNR eq IT_VBAP-POSNR.

  sort IT_VBAP by VBELN POSNR.
  loop at IT_VBKD into WA_VBKD.
    if WA_VBKD-ZLSCH eq 'P' or WA_VBKD-ZLSCH eq SPACE.
      read table IT_VBAP with key VBELN = WA_VBKD-VBELN POSNR = WA_VBKD-POSNR binary search transporting no fields.
      " Se encontrou o item da ordem entao sera necessario excluir da lista
      if SY-SUBRC is initial.
        delete IT_VBAP where VBELN eq WA_VBKD-VBELN and POSNR eq WA_VBKD-POSNR.
        delete IT_VBAK where VBELN eq WA_VBKD-VBELN.
      endif.
    endif.
  endloop.

  sort IT_VBKD by VBELN.
  loop at IT_VBAK into WA_VBAK.
    VG_TABIX = SY-TABIX.
    read table IT_VBKD with key VBELN = WA_VBAK-VBELN binary search transporting no fields.
    " Se nao encontrou a ordem entao sera necessario excluir da lista
    if SY-SUBRC is not initial.
      delete IT_VBAP where VBELN eq WA_VBAK-VBELN.
      delete IT_VBAK index VG_TABIX.
    endif.
  endloop.
  clear VG_TABIX.

  move IT_VBKD[] to IT_VBKD2[].

  delete IT_VBKD2 where VALDT eq SPACE.
  sort IT_VBKD2 by ZTERM.
  delete adjacent duplicates from IT_VBKD2 comparing ZTERM.

  select ZTERM ZTAG1
    into corresponding fields of table IT_T052
    from T052
     for all entries in IT_VBKD2
   where ZTERM eq IT_VBKD2-ZTERM.

  sort IT_VBAP by VBELN POSNR.
  sort IT_VBKD by VBELN POSNR.
  sort IT_T052 by ZTERM.

  loop at IT_VBFA into WA_VBFA.

    VG_TABIX = SY-TABIX.

    WA_VBFA-AWKEY = WA_VBFA-VBELN.
    WA_VBFA-GJAHR = WA_VBFA-ERDAT(4).

    read table IT_VBAK into WA_VBAK with key VBELN = WA_VBFA-VBELV.
    if SY-SUBRC is initial.
      WA_VBFA-BUKRS = WA_VBAK-VKORG.
      WA_VBFA-KUNNR = WA_VBAK-KUNNR.
      WA_VBFA-VTWEG = WA_VBAK-VTWEG.
      WA_VBFA-KALSM = WA_VBAK-KALSM.
      WA_VBFA-KNUMV = WA_VBAK-KNUMV.

      read table IT_VBAP into WA_VBAP with key VBELN = WA_VBFA-VBELV POSNR = WA_VBFA-POSNV binary search.
      if SY-SUBRC is initial.
        WA_VBFA-MATNR = WA_VBAP-MATNR.

        read table IT_VBKD into WA_VBKD with key VBELN = WA_VBFA-VBELV POSNR = WA_VBFA-POSNV binary search.
        if SY-SUBRC is initial.
          WA_VBFA-VALDT = WA_VBKD-VALDT.
          WA_VBFA-ZTERM = WA_VBKD-ZTERM.
          WA_VBFA-ZLSCH = WA_VBKD-ZLSCH.
          if WA_VBKD-VALDT is initial.
            read table IT_T052 into WA_T052 with key ZTERM = WA_VBKD-ZTERM binary search.
            if SY-SUBRC is initial.
              WA_VBFA-ZTAG1 = WA_T052-ZTAG1.
            endif.
          endif.
        endif.

      endif.

    endif.

    modify IT_VBFA index VG_TABIX from WA_VBFA transporting AWKEY GJAHR BUKRS KUNNR MATNR VTWEG KALSM KNUMV VALDT ZTERM ZLSCH ZTAG1.

  endloop.

  check not IT_VBFA is initial.

  select BELNR GJAHR AWKEY BUKRS WAERS BUDAT
         BLART BKTXT AUSBK STBLG STJAH
    into corresponding fields of table IT_BKPF
    from BKPF
     for all entries in IT_VBFA
   where BUKRS eq IT_VBFA-BUKRS
     and GJAHR eq IT_VBFA-GJAHR
     and AWKEY eq IT_VBFA-AWKEY
     and AWTYP eq 'VBRK'.

  check SY-SUBRC is initial.

  select A~BUKRS A~GJAHR A~BELNR A~BUZEI A~BUDAT A~WAERS
         A~BLART A~GSBER A~DMBTR A~WRBTR A~SGTXT A~HKONT
         A~ZFBDT A~ZBD1T A~ZLSCH A~ZLSPR A~HBKID A~DMBE2
         A~UMSKZ A~XREF1 A~XREF2 A~XREF3 A~ZUONR A~ANLN1
         A~BSCHL A~KUNNR A~AUFNR A~VBEL2 A~POSN2
    into corresponding fields of table IT_PREVISAO
    from BSID as A
   inner join BKPF as B on B~BUKRS eq A~BUKRS
                       and B~BELNR eq A~BELNR
                       and B~GJAHR eq A~GJAHR
                       and B~STBLG eq SPACE
     for all entries in IT_BKPF
   where A~BUKRS eq IT_BKPF-BUKRS
     and A~BELNR eq IT_BKPF-BELNR
     and A~GJAHR eq IT_BKPF-GJAHR
     and A~SHKZG eq 'S'
     and A~ZLSCH ne 'P'.

  loop at IT_PREVISAO into WA_PREVISAO.
    read table IT_TOT_AB into WA_TOT_AB with key VBEL2 = WA_PREVISAO-VBEL2 POSN2 = WA_PREVISAO-POSN2.
    if SY-SUBRC is initial.
      add WA_PREVISAO-DMBTR to WA_TOT_AB-DMBTR.
      add WA_PREVISAO-DMBE2 to WA_TOT_AB-DMBE2.
      modify IT_TOT_AB index SY-TABIX from WA_TOT_AB transporting DMBTR DMBE2.
    else.
      move-corresponding WA_PREVISAO to WA_TOT_AB.
      append WA_TOT_AB to IT_TOT_AB.
    endif.
  endloop.

  select A~BUKRS A~AUGBL A~GJAHR A~BELNR A~BUZEI A~BUDAT
         A~AUGDT A~WAERS A~BLART A~GSBER A~DMBTR A~WRBTR
         A~SGTXT A~HKONT A~ZFBDT A~ZBD1T A~ZLSCH A~ZLSPR
         A~HBKID A~DMBE2 A~UMSKZ A~XREF1 A~XREF2 A~XREF3
         A~ZUONR A~ANLN1 A~BSCHL A~KUNNR A~AUGGJ A~VBEL2
         A~POSN2
    into corresponding fields of table IT_REALIZADO
    from BSAD as A
   inner join BKPF as B on B~BUKRS eq A~BUKRS
                       and B~BELNR eq A~AUGBL
                       and B~GJAHR eq A~GJAHR
                       and B~STBLG eq SPACE
     for all entries in IT_BKPF
   where A~BUKRS eq IT_BKPF-BUKRS
     and A~BELNR eq IT_BKPF-BELNR
     and A~GJAHR eq IT_BKPF-GJAHR
     and A~SHKZG eq 'S'
     and A~ZLSCH ne 'P'.

  loop at IT_REALIZADO into WA_REALIZADO.
    read table IT_TOT_LQ into WA_TOT_AB with key VBEL2 = WA_REALIZADO-VBEL2 POSN2 = WA_REALIZADO-POSN2.
    if SY-SUBRC is initial.
      add WA_REALIZADO-DMBTR to WA_TOT_AB-DMBTR.
      add WA_REALIZADO-DMBE2 to WA_TOT_AB-DMBE2.
      modify IT_TOT_LQ index SY-TABIX from WA_TOT_AB transporting DMBTR DMBE2.
    else.
      move-corresponding WA_REALIZADO to WA_TOT_AB.
      append WA_TOT_AB to IT_TOT_LQ.
    endif.
  endloop.

  select BUKRS GJAHR BELNR BUZEI BUDAT WAERS
         BLART GSBER DMBTR WRBTR SGTXT HKONT
         ZFBDT ZBD1T ZLSCH ZLSPR HBKID DMBE2
         UMSKZ XREF1 XREF2 XREF3 ZUONR ANLN1
         BSCHL KUNNR AUFNR VBEL2 POSN2
    from BSID
    into corresponding fields of table IT_PREVISAO2
     for all entries in IT_VBAK
   where BUKRS eq IT_VBAK-VKORG
     and VBEL2 eq IT_VBAK-VBELN
     and KUNNR eq IT_VBAK-KUNNR
     and UMSKS eq 'A'.

  loop at IT_PREVISAO2 into WA_PREVISAO.
    read table IT_TOT_AD into WA_TOT_AB with key VBEL2 = WA_PREVISAO-VBEL2 POSN2 = WA_PREVISAO-POSN2.
    if SY-SUBRC is initial.
      add WA_PREVISAO-DMBTR to WA_TOT_AB-DMBTR.
      add WA_PREVISAO-DMBE2 to WA_TOT_AB-DMBE2.
      modify IT_TOT_AD index SY-TABIX from WA_TOT_AB transporting DMBTR DMBE2.
    else.
      move-corresponding WA_PREVISAO to WA_TOT_AB.
      append WA_TOT_AB to IT_TOT_AD.
    endif.
  endloop.

  clear: IT_VBFA2[].
  move: IT_VBFA[] to IT_VBFA2[].
  sort IT_VBFA2 by KNUMV POSNV.
  delete adjacent duplicates from IT_VBFA2 comparing KNUMV POSNV.

  select from V_KONV fields KNUMV , KPOSN , KSCHL , KWERT for all entries in @IT_VBFA2 where KNUMV eq @IT_VBFA2-KNUMV and KPOSN eq @IT_VBFA2-POSNV and KSCHL eq 'IBRX' into corresponding fields of table @IT_KONV .

  loop at IT_VBAK into WA_VBAK.

    loop at IT_VBAP into WA_VBAP where VBELN eq WA_VBAK-VBELN.

      VG_TABIX = SY-TABIX.

      WA_VBAP-VLRAB = 0. "Valor Aberto
      WA_VBAP-VLRLQ = 0. "Valor Liquidado
      WA_VBAP-VLRAD = 0. "Valor Adiantado
      WA_VBAP-VLRKV = 0. "Valor do Documento
      WA_VBAP-VLRTT = 0. "Valor Pendente de Movimento

      read table IT_TOT_AB into WA_TOT_AB with key VBEL2 = WA_VBAP-VBELN POSN2 = WA_VBAP-POSNR.
      if SY-SUBRC is initial.
        if WA_VBAK-WAERK eq 'BRL'.
          WA_VBAP-VLRAB = WA_TOT_AB-DMBTR.
        elseif WA_VBAK-WAERK eq 'USD'.
          WA_VBAP-VLRAB = WA_TOT_AB-DMBE2.
        endif.
      endif.

      read table IT_TOT_LQ into WA_TOT_AB with key VBEL2 = WA_VBAP-VBELN POSN2 = WA_VBAP-POSNR.
      if SY-SUBRC is initial.
        if WA_VBAK-WAERK eq 'BRL'.
          WA_VBAP-VLRLQ = WA_TOT_AB-DMBTR.
        elseif WA_VBAK-WAERK eq 'USD'.
          WA_VBAP-VLRLQ = WA_TOT_AB-DMBE2.
        endif.
      endif.

      read table IT_TOT_AD into WA_TOT_AB with key VBEL2 = WA_VBAP-VBELN POSN2 = WA_VBAP-POSNR.
      if SY-SUBRC is initial.
        if WA_VBAK-WAERK eq 'BRL'.
          WA_VBAP-VLRAD = WA_TOT_AB-DMBTR.
        elseif WA_VBAK-WAERK eq 'USD'.
          WA_VBAP-VLRAD = WA_TOT_AB-DMBE2.
        endif.
      endif.

      read table IT_KONV into WA_KONV with key KNUMV = WA_VBAK-KNUMV KPOSN = WA_VBAP-POSNR.
      if SY-SUBRC is initial.
        WA_VBAP-VLRKV = WA_KONV-KWERT.
      endif.

      WA_VBAP-VLRTT = WA_VBAP-VLRKV - ( WA_VBAP-VLRAB + WA_VBAP-VLRLQ + WA_VBAP-VLRAD ).

      modify IT_VBAP index VG_TABIX from WA_VBAP transporting VLRAB VLRLQ VLRAD VLRKV VLRTT.
    endloop.

  endloop.

  "DELETE it_vbap WHERE vlrtt LE 0.

  check not IT_VBAP is initial.

  clear: IT_VBAK2[].
  move IT_VBAK[] to IT_VBAK2[].
  sort IT_VBAK2 by KUNNR VKORG.
  delete adjacent duplicates from IT_VBAK2 comparing KUNNR VKORG.

  "Cta.de reconciliação na contabilidade geral
  select KUNNR BUKRS AKONT
    into corresponding fields of table IT_KNB1
    from KNB1
     for all entries in IT_VBAK2
   where KUNNR eq IT_VBAK2-KUNNR
     and BUKRS eq IT_VBAK2-VKORG.

  sort IT_VBFA by KUNNR BUKRS.
  sort IT_KNB1 by KUNNR BUKRS.

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.
    read table IT_KNB1 into WA_KNB1 with key KUNNR = WA_VBFA-KUNNR BUKRS = WA_VBFA-BUKRS binary search.
    if SY-SUBRC is initial.
      WA_VBFA-AKONT = WA_KNB1-AKONT.
      modify IT_VBFA index VG_TABIX from WA_VBFA transporting AKONT.
    endif.
  endloop.

  clear: IT_VBAK2[].
  move IT_VBAK[] to IT_VBAK2[].
  sort IT_VBAK2 by KALSM.
  delete adjacent duplicates from IT_VBAK2 comparing KALSM.

  "Chave de conta
  select KALSM KVSL1
    into corresponding fields of table IT_T683S
    from T683S
     for all entries in IT_VBAK2
   where KALSM eq IT_VBAK2-KALSM
     and KSCHL eq 'ICMI'.

  sort IT_VBFA  by KALSM.
  sort IT_T683S by KALSM.

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.
    read table IT_T683S into WA_T683S with key KALSM = WA_VBFA-KALSM binary search.
    if SY-SUBRC is initial.
      WA_VBFA-KVSL1 = WA_T683S-KVSL1.
      modify IT_VBFA index VG_TABIX from WA_VBFA transporting KVSL1.
    endif.
  endloop.

  clear: IT_VBFA2[].
  move: IT_VBFA[] to IT_VBFA2[].
  sort IT_VBFA2 by MATNR BUKRS VTWEG.
  delete adjacent duplicates from IT_VBFA2 comparing MATNR BUKRS VTWEG.

  "Grupo de classificação contábil - material
  select MATNR VKORG VTWEG KTGRM
    into corresponding fields of table IT_MVKE
    from MVKE
     for all entries in IT_VBFA2
   where MATNR eq IT_VBFA2-MATNR
     and VKORG eq IT_VBFA2-BUKRS
     and VTWEG eq IT_VBFA2-VTWEG.

  sort IT_VBFA by MATNR BUKRS VTWEG.
  sort IT_MVKE by MATNR VKORG VTWEG.

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.
    read table IT_MVKE into WA_MVKE with key MATNR = WA_VBFA-MATNR VKORG = WA_VBFA-BUKRS VTWEG = WA_VBFA-VTWEG binary search.
    if SY-SUBRC is initial.
      WA_VBFA-KTGRM = WA_MVKE-KTGRM.
      modify IT_VBFA index VG_TABIX from WA_VBFA transporting KTGRM.
    endif.
  endloop.

  clear: IT_VBFA2[].
  move: IT_VBFA[] to IT_VBFA2[].
  sort IT_VBFA2 by MATNR.
  delete adjacent duplicates from IT_VBFA2 comparing MATNR.

  "Grupo de mercadorias
  select MATNR MATKL
    into corresponding fields of table IT_MARA
    from MARA
     for all entries in IT_VBFA2
   where MATNR eq IT_VBFA2-MATNR.

  sort IT_VBFA by MATNR.
  sort IT_MVKE by MATNR.

*---> 05/07/2023 - Migração S4 - DL
SORT IT_MARA BY MATNR MATKL.
*<--- 05/07/2023 - Migração S4 - DL

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.
    read table IT_MARA into WA_MARA with key MATNR = WA_VBFA-MATNR binary search.
    if SY-SUBRC is initial.
      WA_VBFA-MATKL = WA_MARA-MATKL.
      modify IT_VBFA index VG_TABIX from WA_VBFA transporting MATKL.
    endif.
  endloop.

  clear: IT_VBFA2[].
  move: IT_VBFA[] to IT_VBFA2[].
  sort IT_VBFA2 by KUNNR BUKRS VTWEG.
  delete adjacent duplicates from IT_VBFA2 comparing KUNNR BUKRS VTWEG.

  "Grupo de classificação contábil do cliente
  select KUNNR VKORG VTWEG KTGRD
    into corresponding fields of table IT_KNVV
    from KNVV
     for all entries in IT_VBFA2
   where KUNNR eq IT_VBFA2-KUNNR
     and VKORG eq IT_VBFA2-BUKRS
     and VTWEG eq IT_VBFA2-VTWEG.

  sort IT_VBFA by KUNNR BUKRS VTWEG.
  sort IT_KNVV by KUNNR VKORG VTWEG.

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.
    read table IT_KNVV into WA_KNVV with key KUNNR = WA_VBFA-KUNNR VKORG = WA_VBFA-BUKRS VTWEG = WA_VBFA-VTWEG binary search.
    if SY-SUBRC is initial.
      WA_VBFA-KTGRD = WA_KNVV-KTGRD.
      modify IT_VBFA index VG_TABIX from WA_VBFA transporting KTGRD.
    endif.
  endloop.

  clear: IT_VBFA2[].
  move: IT_VBFA[] to IT_VBFA2[].
  sort IT_VBFA2 by BUKRS KTGRD KTGRM KVSL1.
  delete adjacent duplicates from IT_VBFA2 comparing BUKRS KTGRD KTGRM KVSL1.

  select VKORG KTGRD KTGRM KVSL1 SAKN1
    into corresponding fields of table IT_C001
    from C001
     for all entries in IT_VBFA2
   where VKORG eq IT_VBFA2-BUKRS
     and KTGRD eq IT_VBFA2-KTGRD
     and KTGRM eq IT_VBFA2-KTGRM
     and KVSL1 eq IT_VBFA2-KVSL1.

  sort IT_VBFA by BUKRS KTGRD KTGRM KVSL1.
  sort IT_C001 by VKORG KTGRD KTGRM KVSL1.

  loop at IT_VBFA into WA_VBFA.
    VG_TABIX = SY-TABIX.
    read table IT_C001 into WA_C001 with key VKORG = WA_VBFA-BUKRS KTGRD = WA_VBFA-KTGRD KTGRM = WA_VBFA-KTGRM KVSL1 = WA_VBFA-KVSL1 binary search.
    if SY-SUBRC is initial.
      WA_VBFA-SAKN1 = WA_C001-SAKN1.
      modify IT_VBFA index VG_TABIX from WA_VBFA transporting SAKN1.
    endif.
  endloop.

  loop at IT_VBAK into WA_VBAK.

    loop at IT_VBAP into WA_VBAP where VBELN eq WA_VBAK-VBELN.

      AUX_CONT = AUX_CONT + 1.

      clear: WA_VBFA, WA_MOVXRT_PRE.

      read table IT_VBFA into WA_VBFA with key VBELV = WA_VBAP-VBELN POSNV = WA_VBAP-POSNR.

      concatenate WA_VBFA-AKONT WA_VBFA-SAKN1 'OV' WA_VBFA-MATKL into VL_CHAVE.

      loop at IT_ZGL008 into WA_ZGL008.
        if WA_ZGL008-CHAVE eq VL_CHAVE.
          VL_CONTA_FINC = WA_ZGL008-CONTA_FINANC.
        endif.
      endloop.

      clear WA_MOVXRT_PRE.

      if not WA_VBFA-VALDT is initial.

        WA_MOVXRT_PRE-DT_COMPETENCIA = WA_VBFA-VALDT.

      else.

        move WA_VBFA-ZTAG1 to NU_DIAS.
        WA_MOVXRT_PRE-DT_COMPETENCIA = SY-DATUM.

        while NU_DIAS ne 0.

          if NU_DIAS > 30.
            NU_DIAS = NU_DIAS - 30.
            DIAS = 30.
          else.
            DIAS = NU_DIAS.
            NU_DIAS = 0.
          endif.

          call function 'RP_CALC_DATE_IN_INTERVAL'
            exporting
              DATE      = WA_MOVXRT_PRE-DT_COMPETENCIA
              DAYS      = DIAS
              MONTHS    = 0
              YEARS     = 0
            importing
              CALC_DATE = WA_MOVXRT_PRE-DT_COMPETENCIA.

        endwhile.

      endif.

      WA_MOVXRT_PRE-DT_CONTABIL       = WA_VBFA-ERDAT.
      WA_MOVXRT_PRE-DT_PAGAMENTO      = WA_MOVXRT_PRE-DT_COMPETENCIA.
      concatenate WA_VBAP-VBELN WA_VBAP-POSNR into WA_MOVXRT_PRE-DOC_PAGADOR.
      WA_MOVXRT_PRE-EMITIDO_FLAG      = 'S'.
      concatenate 'Ordem de Venda - ' WA_VBAK-VBELN into WA_MOVXRT_PRE-FINALIDADE.
      concatenate 'OV' ' - Ch.: ' VL_CHAVE into WA_MOVXRT_PRE-HISTORICO.
      concatenate 'Ref.doc. ' WA_VBAK-VBELN into WA_MOVXRT_PRE-INF_CONTABEIS.
      WA_MOVXRT_PRE-MAN_AUT           = 'N'.
      WA_MOVXRT_PRE-MMI_ENT_SAI       = 'E'.
      concatenate WA_VBAP-VBELN WA_VBAP-POSNR into WA_MOVXRT_PRE-ORIGEM_PK.
      concatenate 'OV' '-' 'AR' into WA_MOVXRT_PRE-ORIGEM_CONTABIL.
      WA_MOVXRT_PRE-ORIGEM_PROCESSO   = 'ITF'.
      WA_MOVXRT_PRE-ORIGEM_SISTEMA    = 'SAP'.
      WA_MOVXRT_PRE-PFJ_CODIGO        = WA_VBAK-VKORG.
      WA_MOVXRT_PRE-PFJ_ORIG_DEST     = WA_VBAP-GSBER.
      WA_MOVXRT_PRE-TX_CONV_CNT_FIXA  = 'N'.
      WA_MOVXRT_PRE-TX_CONV_CNT       = 1.
      WA_MOVXRT_PRE-TX_CONV_CORR_FIXA = 'N'.
      WA_MOVXRT_PRE-TDO_CODIGO        = 'OV'.

      if WA_VBFA-ZLSCH is initial.
        WA_MOVXRT_PRE-TDP_CODIGO        = 'U'.
      else.
        WA_MOVXRT_PRE-TDP_CODIGO        = WA_VBFA-ZLSCH.
      endif.

      WA_MOVXRT_PRE-TEMPERATURA       = -4.
      WA_MOVXRT_PRE-VALOR_ORIGINAL    = WA_VBAP-VLRTT.
      WA_MOVXRT_PRE-IND_CODIGO_REAL   = WA_VBAK-WAERK.
      WA_MOVXRT_PRE-VALOR             = WA_VBAP-VLRTT.

      if WA_VBAK-WAERK ne 'BRL'.
        WA_MOVXRT_PRE-TX_CONV_CORR  = 0.
      else.
        WA_MOVXRT_REA-TX_CONV_CORR    = 1.
      endif.

      if VL_CONTA_FINC is not initial.
        WA_MOVXRT_PRE-MMI_CODIGO = VL_CONTA_FINC.
      else.
        WA_MOVXRT_PRE-MMI_CODIGO = '2.99.99'.
      endif.

      append WA_MOVXRT_PRE to IT_MOVXRT_PRE.

      if AUX_CONT > 999.
        perform F_CHAMA_RFC.
        AUX_CONT = 0.
      endif.

    endloop.

  endloop.

endform.                    " F_PREVISAO_AR_ORDENS
