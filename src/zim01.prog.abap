*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 26/08/2009                                              &*
*& Descrição: Relatório de investimento                               &*
*& Transação: ZIM01                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK906359   26.08.2009                            &*
*&--------------------------------------------------------------------&*
report  ZIM01.
data: S_LAYOUT type LVC_S_LAYO.

*----------------------------------------------------------------------*
*                  TABELAS
*----------------------------------------------------------------------*
tables: T001, B120, IMAK, COAS, ANLA, EKKO, EKAB, EKBE, IMAKPA.
*----------------------------------------------------------------------*
*                  INCLUDES
*----------------------------------------------------------------------*
include <ICON>.
* -------------------------------------------------------------------- *
*    TABELAS                                                           *
* -------------------------------------------------------------------- *
data: W_PASTA(20),
      W_GROUP,
      W_PGMT,
      W_ANAL,
      W_IMPO,
      W_BUDAT_LOW(10),
      W_BUDAT_HIGH(10),
      W_CPUDT_LOW(10),
      W_CPUDT_HIGH(10),
      W_NOME(40),
      W_VISAO(40),
      W_FATOR          type TCURR-UKURS,
      W_GDATU          type TCURR-GDATU,
      W_DIAS(4)        type N,
      W_PAGAT          type DATUM.

* create container for alv-tree
data: L_TREE_CONTAINER_NAME(30) type C,
      L_CUSTOM_CONTAINER        type ref to CL_GUI_CUSTOM_CONTAINER.
L_TREE_CONTAINER_NAME = 'TREE1'.

data : CONTAINER1        type ref to CL_GUI_DOCKING_CONTAINER.

data: begin of T_SEL occurs 0,
        VISAO     like ZIM08_REL_INV2-VISAO,
        USUARIO   like ZIM08_REL_INV2-USUARIO,
        DATA_EXEC like ZIM08_REL_INV2-DATA_EXEC,
        HORA_EXEC like ZIM08_REL_INV2-HORA_EXEC,
      end of T_SEL.

data: begin of T_IMAKPA occurs 0,
        POSNR  like IMAKPA-POSNR,
        LFDNR  like IMAKPA-LFDNR,
        AKOSTL like IMAKPA-AKOSTL,
        AGSBER like IMAKPA-AGSBER,
        NAME   like J_1BBRANCH-NAME,
        LTEXT  like CSKT-LTEXT,
      end of T_IMAKPA.

data: begin of T_INV occurs 0.
        include structure ZIM01_SOL_AP_INV.
data: end of T_INV.

data: begin of T_IMAK occurs 0,
        POSNR  like IMAK-POSNR,
        ABUKRS like IMAK-ABUKRS,
        WERKS  like IMAK-WERKS,
        OBJNR  like IMAK-OBJNR,
        TXT50  like IMAKT-TXT50,
        AKOSTL like IMAKPA-AKOSTL,
        VGSBER like IMAK-VGSBER,
        NAME   like T001W-NAME1,
        DEL(1),
      end of T_IMAK.

data: begin of T_TOTAL occurs 0,
        POSNR     like IMAK-POSNR,
        AUFNR     like COAS-AUFNR,
        ANLN1     like IMAKA-ANLN1,
        CAMPO(30),
        DMBTR     like BSEG-DMBTR,
      end of T_TOTAL.

data: T_TOTALR like T_TOTAL occurs 0 with header line.

data: begin of T_161 occurs 0.
        include structure T161T.
data: end of T_161.

data: begin of T_CICLO occurs 0,
        KOKRS  like COBK-KOKRS,
        BELNR  like COBK-BELNR,
        BUZEI  like COEP-BUZEI,
        WOGBTR like COEP-WOGBTR,
        WTGBTR like COEP-WTGBTR,
        BEKNZ  like COEP-BEKNZ,
        OBJNR  like COEP-OBJNR,
        BLTXT  like COBK-BLTXT,
        BUDAT  like COBK-BUDAT,
      end of T_CICLO.


data: begin of T_MSEG occurs 0,
        MBLNR  like MSEG-MBLNR,
        MJAHR  like MSEG-MJAHR,
        ZEILE  like MSEG-ZEILE,
        ANLN1  like MSEG-ANLN1,
        ANLN2  like MSEG-ANLN2,
        AUFNR  like MSEG-AUFNR,
        DMBTR  like MSEG-DMBTR,
        MATNR  like MSEG-MATNR,
        SHKZG  like MSEG-SHKZG,
        TCODE2 like MKPF-TCODE2,
        BUDAT  like MKPF-BUDAT,
        AWKEY  like BKPF-AWKEY,
        MAKTX  like MAKT-MAKTX,
      end of T_MSEG.


data: begin of T_IMAKZ occurs 0.
        include structure IMAKZ.
data:   AUFNR like COAS-AUFNR.
data: KTEXT like COAS-KTEXT.
data: ANLN1 like COBRB-ANLN1.
data: end of T_IMAKZ.

data: begin of T_IMAKA occurs 0,
        POSNR like IMAKA-POSNR,
        LFDNR like IMAKA-LFDNR,
        BUKRS like IMAKA-BUKRS,
        ANLN1 like IMAKA-ANLN1,
        ANLN2 like IMAKA-ANLN2,
        ANLKL like ANLA-ANLKL,
        TXT50 like ANLA-TXT50,
*        INCLUDE STRUCTURE imaka.
      end of T_IMAKA.

data: begin of T_PV occurs 0,
        BUKRS like ANEKPV-BUKRS,
        ANLN1 like ANEKPV-ANLN1,
        ANLN2 like ANEKPV-ANLN2,
        GJAHR like ANEKPV-GJAHR,
        LNRAN like ANEKPV-LNRAN,
        AFABE like ANEKPV-AFABE,
        ZUJHR like ANEKPV-ZUJHR,
        ZUCOD like ANEKPV-ZUCOD,
        BELNR like ANEKPV-BELNR,
        BUDAT like ANEKPV-BUDAT,
      end of T_PV.

data: begin of T_COAS occurs 0,
        AUFNR like COAS-AUFNR,
        BUKRS like COAS-BUKRS,
        OBJNR like COAS-OBJNR,
        KTEXT like COAS-KTEXT,
        AUART like COAS-AUART,
        KOSTV like COAS-KOSTV,
      end of T_COAS.


data: begin of T_EKKO occurs 0,
        EBELN        like EKKO-EBELN,
        LIFNR        like EKKO-LIFNR,
        LIFRE        like EKKO-LIFRE,
        AEDAT        like EKKO-AEDAT,
        WKURS        like EKKO-WKURS,
        WAERS        like EKKO-WAERS,
        BSART        like EKKO-BSART,
        EBELP        like EKPO-EBELP,
        MATNR        like EKPO-MATNR,
        TXZ01        like EKPO-TXZ01,
        MENGE        like EKPO-MENGE,
        MEINS        like EKPO-MEINS,
        NETPR        like EKPO-NETPR,
        NETWR        like EKPO-NETWR,
        PACKNO       like EKPO-PACKNO,
        KONNR        like EKPO-KONNR,
        KTPNR        like EKPO-KTPNR,
        EFFWR        like EKPO-EFFWR,
        ZEKKN        like EKKN-ZEKKN,
        AUFNR        like EKKN-AUFNR,
        ANLN1        like EKKN-ANLN1,
        ANLN2        like EKKN-ANLN2,
*        zekkn LIKE ekkn-zekkn,
        VPROZ        like EKKN-VPROZ,

        PORCENTO(10) type P decimals 8,
      end of T_EKKO.

data: T_EKKO_AUX like T_EKKO occurs 0 with header line.
data: begin of T_EKKO3 occurs 0.
        include structure T_EKKO.
data:   BELNR type EKBE-BELNR,
      end of T_EKKO3.
data: WL_EKKO3 like line of T_EKKO3.
data: T_EKKO_POR like T_EKKO occurs 0 with header line.

data: begin of T_EKAB occurs 0,
        KONNR like EKAB-KONNR,
        KTPNR like EKAB-KTPNR,
        EBELN like EKAB-EBELN,
        EBELP like EKAB-EBELP,
        ZEKKN like EKKN-ZEKKN,
        AUFNR like EKKN-AUFNR,
        ANLN1 like EKKN-ANLN1,
        ANLN2 like EKKN-ANLN2,
*       zekkn LIKE ekkn-zekkn,
        VPROZ like EKKN-VPROZ,
      end of T_EKAB.

data: begin of T_EKKN occurs 0,
        EBELN like EKKN-EBELN,
        EBELP like EKKN-EBELP,
        ZEKKN like EKKN-ZEKKN,
*            vgabe LIKE ekkn-vgabe,
*            gjahr LIKE ekkn-gjahr,
*            belnr LIKE ekkn-belnr,
*            buzei LIKE ekkn-buzei,
        MENGE like EKKN-MENGE,
        NETWR like EKKN-NETWR,
        AUFNR like EKKN-AUFNR,
        ANLN1 like EKKN-ANLN1,
        ANLN2 like EKKN-ANLN2,
        VPROZ like EKKN-VPROZ,
      end of T_EKKN.

data: begin of T_PACK occurs 0,
        PACKNO     like ESLL-PACKNO,
        SUB_PACKNO like ESLL-SUB_PACKNO,
      end of T_PACK.

data: T_ESLL_1 like T_PACK occurs 0 with header line.


data: begin of T_ESLL_2 occurs 0,
        PACKNO     like ESLL-PACKNO,
        SUB_PACKNO like ESLL-SUB_PACKNO,
        SRVPOS     like ESLL-SRVPOS,
        KTEXT1     like ESLL-KTEXT1,
        EXTROW     like ESLL-EXTROW,
      end of T_ESLL_2.

data: begin of T_EKBE occurs 0,
        EBELN like EKBE-EBELN,
        EBELP like EKBE-EBELP,
        ZEKKN like EKBE-ZEKKN,
        VGABE like EKBE-VGABE,
        GJAHR like EKBE-GJAHR,
        BELNR like EKBE-BELNR,
        BUZEI like EKBE-BUZEI,
        BWART like EKBE-BWART,
        BUDAT like EKBE-BUDAT,
        MENGE like EKBE-MENGE,
        DMBTR like EKBE-DMBTR,
        WRBTR like EKBE-WRBTR,
        WAERS like EKBE-WAERS,
        LFBNR like EKBE-LFBNR,
        CPUDT like EKBE-CPUDT,
        SHKZG like EKBE-SHKZG,
        XBLNR like EKBE-XBLNR,
        ANLN1 like EKKN-ANLN1,
        AUFNR like EKKN-AUFNR,
        BEKKN like EKBE-BEKKN,
        CPUTM like EKBE-CPUTM,
*        lfbnr like ekbe-lfbnr,
        AWKEY like BKPF-AWKEY,
      end of T_EKBE.

data: begin of T_DOCS_AG occurs 0,
        BUKRS type BSAK-BUKRS,
        BELNR type BSAK-BELNR,
        AG(2),
      end of T_DOCS_AG.

data: T_EKBE_1 like T_EKBE occurs 0 with header line.
data: T_EKBE_9 like T_EKBE occurs 0 with header line.

data: begin of T_RBKP occurs 0,
        BELNR like RBKP-BELNR,
        GJAHR like RBKP-GJAHR,
        XBLNR like RBKP-XBLNR,
        LIFNR like RBKP-LIFNR,
        NAME1 like LFA1-NAME1,
      end of T_RBKP.

data: begin of T_RBKP_EST occurs 0,
        BELNR like RBKP-BELNR,
        GJAHR like RBKP-GJAHR,
        STBLG like RBKP-STBLG,
      end of T_RBKP_EST.

data: begin of T_BKPF_EST occurs 0,
        BELNR like BKPF-BELNR,
        GJAHR like BKPF-GJAHR,
        STBLG like BKPF-STBLG,
      end of T_BKPF_EST.

data: begin of T_RBKP1 occurs 0,
        BELNR like RBKP-BELNR,
        GJAHR like RBKP-GJAHR,
        XBLNR like RBKP-XBLNR,
        LIFNR like RBKP-LIFNR,
        NAME1 like LFA1-NAME1,
      end of T_RBKP1.

data: begin of T_TAXA occurs 0,
        BUKRS type BSIK-BUKRS,
        BELNR type BSIK-BELNR,
        GJAHR type BSIK-GJAHR,
        DMBTR type BSIK-DMBTR,
        DMBE2 type BSIK-DMBE2,
      end of T_TAXA.

data: W_EKBE like T_EKBE occurs 0 with header line.
data: E_EKBE like T_EKBE occurs 0 with header line.
data: E_EKBE2 like T_EKBE occurs 0 with header line.
data: T_EKBEC like T_EKBE occurs 0 with header line.
data: T_EKBE_AUX like T_EKBE occurs 0 with header line.
data: T_EKBEA like T_EKBE occurs 0 with header line.
data: T_EKBE_2 like T_EKBE occurs 0 with header line.

data: begin of WE_BKPF occurs 0,
        BUKRS like BKPF-BUKRS,
        BELNR like BKPF-BELNR,
        GJAHR like BKPF-GJAHR,
        AWKEY like BKPF-AWKEY,
        KURS2 like BKPF-KURS2,
        BLART type BKPF-BLART,
      end of WE_BKPF.

data: begin of WF_BKPF occurs 0,
        BUKRS like BKPF-BUKRS,
        BELNR like BKPF-BELNR,
        GJAHR like BKPF-GJAHR,
        AWKEY like BKPF-AWKEY,
      end of WF_BKPF.
*DATA: wf_bkpf LIKE we_bkpf OCCURS 0 WITH HEADER LINE.

data: begin of T_BKPF_LB occurs 0,
        BUKRS type BKPF-BUKRS,
        BELNR type BKPF-BELNR,
        GJAHR type BKPF-GJAHR,
        KURS2 type BKPF-KURS2,
      end of T_BKPF_LB.

data: T_BKPF_AG like table of T_BKPF_LB with header line.

data TREE1  type ref to CL_GUI_ALV_TREE.
*DATA tree1  TYPE REF TO cl_gui_column_tree.
*data mr_toolbar type ref to cl_gui_toolbar.
data: V_REPID like SY-REPID.

data: E_COLOR type KKBLO_SPECIALCOL,
      begin of T_INFOGRID,
        COLOR type KKBLO_SPECIALCOL occurs 1,
      end of T_INFOGRID.

types: begin of E_SAIDA.
         include structure ZES_IM01.
types:   ANLN2 type ANLN2,
         BUZEI type BSIK-BUZEI,
         ZEKKN type EKBE-ZEKKN,
         N0    type LVC_VALUE,
         N1    type LVC_VALUE,
         N2    type LVC_VALUE,
         N3    type LVC_VALUE,
         N4    type LVC_VALUE.
types: COLOR type KKBLO_SPECIALCOL occurs 1.
types: end of E_SAIDA.

data: begin of T_SAIDA occurs 0.
        include structure ZES_IM01.
data:   ANLN2 type ANLN2,
        BUZEI type BSIK-BUZEI,
        ZEKKN type EKBE-ZEKKN,
        N0    type LVC_VALUE,
        N1    type LVC_VALUE,
        N2    type LVC_VALUE,
        N3    type LVC_VALUE,
        N4    type LVC_VALUE.
data: COLOR        type   KKBLO_SPECIALCOL occurs 0.
data: end of T_SAIDA.

data: begin of TG_IMPOSTOS occurs 0,
        FIELD_IMP type ZIMT002-FIELD_IMP,
        SAKNR     type ZIMT002-SAKNR,
        BEZEI     type ZIMT002T-BEZEI,
      end of TG_IMPOSTOS.

data: TE_SAIDA      type E_SAIDA occurs 0 with header line,
*      TG_IMPOSTOS    TYPE STANDARD TABLE OF ZIMT002 WITH HEADER LINE,
      GS_COMP       type ABAP_COMPONENTDESCR,
      GD_TABNAM     type STRING,
      GD_TABFIELD   type STRING,
      GT_COMPONENTS type ABAP_COMPONENT_TAB,
      GS_COMPONENT  type ABAP_COMPDESCR,
*      GO_TABLE       TYPE REF TO CL_SALV_TABLE,
*      GD_TABNAM      TYPE STRING,
      GO_SDESCR     type ref to CL_ABAP_STRUCTDESCR,
      GO_SDESCR_NEW type ref to CL_ABAP_STRUCTDESCR,
      GO_TDESCR     type ref to CL_ABAP_TABLEDESCR,
      E_TABELA      type ref to  DATA.

field-symbols: <FS_IMPOSTOS> type standard table.

data: WL_HORA type UZEIT.

data: begin of T_08 occurs 0.
        include structure ZIM08_REL_INV2.
data: end of T_08.

data: begin of T_08_USD occurs 0.
        include structure ZIM08_REL_INV_US.
data: end of T_08_USD.

data: "t_saida TYPE zes_im01 OCCURS 0 WITH HEADER LINE,
  TW_SAIDA        type E_SAIDA occurs 0 with header line,
  T_ZIMT003       type table of ZIMT003 with header line,
  GT_FIELDCATALOG type LVC_T_FCAT, "Fieldcatalog
  OK_CODE         like SY-UCOMM.           "OK-Code

*Início Alteração Ricardo Furst
data: begin of T_ALV1 occurs 0,
        POSNR      type ZES_IM01-POSNR,
        DESCR      type IMAKT-TXT50,

        ABUKRS     type IMAK-ABUKRS,
        BUTXT      type T001-BUTXT,
        AGSBER     type IMAKPA-AGSBER,
        NAME       type J_1BBRANCH-NAME,
        AKOSTL     type IMAKPA-AKOSTL,
        LTEXT      type CSKT-LTEXT,

        TXTTIPO    type ZES_IM01-TXTTIPO,
        ANLN1(65)  type C,
        NETWR      type ZES_IM01-NETWR,
        DMBTR      type ZES_IM01-DMBTR,
        TOTPG      type ZES_IM01-TOTPG,
        TOTAPG     type ZES_IM01-TOTAPG,
        MODULO(15) type C,
        EBELN      type ZES_IM01-EBELN,
        EBELP      type ZES_IM01-EBELP,
        AEDAT      type ZES_IM01-AEDAT,
        BELNR2     type ZES_IM01-BELNR2,
        BELNR3     type ZES_IM01-BELNR3,
        BELNR6     type ZES_IM01-BELNR6,
        BELNR5     type ZES_IM01-BELNR5,
        NETPR      type ZES_IM01-NETPR,
        FORNECEDOR type ZES_IM01-FORNECEDOR,
        PAGO30     type ZES_IM01-PAGO30,
        PAGO60     type ZES_IM01-PAGO60,
        PAGO90     type ZES_IM01-PAGO90,
        PAGO120    type ZES_IM01-PAGO120,
        PAGO150    type ZES_IM01-PAGO150,
        PAGO180    type ZES_IM01-PAGO180,
        PAGO210    type ZES_IM01-PAGO210,
        PAGO999    type ZES_IM01-PAGO999,
        IMP_TOTAL  type ZES_IM01-IMP_TOTAL.
        include type ZES_IM02.
data:   TXTIMOB    type ZES_IM01-TXTIMOB,
        TXZ01      type ZES_IM01-TXZ01,
      end of T_ALV1.

data: begin of T_ALV2 occurs 0,
        POSNR      type ZES_IM01-POSNR,
        DESCR      type IMAKT-TXT50,

        ABUKRS     type IMAK-ABUKRS,
        BUTXT      type T001-BUTXT,
        AGSBER     type IMAKPA-AGSBER,
        NAME       type J_1BBRANCH-NAME,
        AKOSTL     type IMAKPA-AKOSTL,
        LTEXT      type CSKT-LTEXT,

        TXTTIPO    type ZES_IM01-TXTTIPO,
        ANLN1(65)  type C, "anln1      TYPE zes_im01-anln1,
*        anln2      TYPE zes_im01-anln2,
        NETWR      type ZES_IM01-NETWR,
        DMBTR      type ZES_IM01-DMBTR,
        MODULO(15) type C,
        DIFERENCA  type ZES_IM01-DIFERENCA,
        EBELN      type ZES_IM01-EBELN,
        EBELP      type ZES_IM01-EBELP,
        AEDAT      type ZES_IM01-AEDAT,
        BELNR2     type ZES_IM01-BELNR2,
        BELNR1     type ZES_IM01-BELNR1,
        BELNR3     type ZES_IM01-BELNR3,
        NFENUM     type ZES_IM01-NFENUM,
        NETPR      type ZES_IM01-NETPR,
        MATNR      type ZES_IM01-MATNR,
        TXZ01      type ZES_IM01-TXZ01,
        SRVPOS     type ZES_IM01-SRVPOS,
        KTEXT1     type ZES_IM01-KTEXT1,
        MENGE      type ZES_IM01-MENGE,
        MEINS      type ZES_IM01-MEINS,
        FORNECEDOR type ZES_IM01-FORNECEDOR,
        EMISSOR    type ZES_IM01-EMISSOR,
        TXTIMOB    type ZES_IM01-TXTIMOB,
        SGTXT      type ZES_IM01-SGTXT,
*        TXZ01      type zes_im01-TXZ01,
      end of T_ALV2.

data: begin of T_ALV3 occurs 0,
        POSNR      type ZES_IM01-POSNR,
        DESCR      type IMAKT-TXT50,

        ABUKRS     type IMAK-ABUKRS,
        BUTXT      type T001-BUTXT,
        AGSBER     type IMAKPA-AGSBER,
        NAME       type J_1BBRANCH-NAME,
        AKOSTL     type IMAKPA-AKOSTL,
        LTEXT      type CSKT-LTEXT,

        TXTTIPO    type ZES_IM01-TXTTIPO,
        ANLN1      type ZES_IM01-ANLN1,
        ORCADO     type ZES_IM01-ORCADO,
        MODULO(15) type C,
        APROVADO   type ZES_IM01-APROVADO,
        NETWR      type ZES_IM01-NETWR,
        DMBTR      type ZES_IM01-DMBTR,
        TOTPG      type ZES_IM01-TOTPG,
        TOTAPG     type ZES_IM01-TOTAPG,
        PED_AP     type ZES_IM01-PED_AP,
        REAL_AP    type ZES_IM01-REAL_AP,
        TXTIMOB    type ZES_IM01-TXTIMOB,
        TXZ01      type ZES_IM01-TXZ01,
      end of T_ALV3.

* Tabela interna com os dados do relatório
data: IT_OUTTAB1 like standard table of T_ALV1,
      IT_OUTTAB2 like standard table of T_ALV2,
      IT_OUTTAB3 like standard table of T_ALV3.

data: IT_LAYOUT            type LVC_S_LAYO.

data: IT_FIELDCATALOG    type LVC_T_FCAT,
      FC                 type line of LVC_T_FCAT,
      IT_SORT            type LVC_T_SORT,
      T_SORT             type line of LVC_T_SORT,
      I                  type LVC_S_FCAT-COL_POS,
      O_GRID             type ref to CL_GUI_ALV_GRID,
      O_CUSTOM_CONTAINER type ref to CL_GUI_DOCKING_CONTAINER.
*Fim Alteração Ricardo Furst.

data: begin of T_SKA1 occurs 0,
        KTOPL like SKA1-KTOPL,
        SAKNR like SKA1-SAKNR,
        KTOKS like SKA1-KTOKS,
      end of T_SKA1.

data: begin of T_SKB1 occurs 0,
        BUKRS like SKB1-BUKRS,
        SAKNR like SKB1-SAKNR,
      end of T_SKB1.

data: begin of T_BSIS occurs 0,
        BUKRS like BSIS-BUKRS,
        HKONT like BSIS-HKONT,
        AUGDT like BSIS-AUGDT,
        AUGBL like BSIS-AUGBL,
        ZUONR like BSIS-ZUONR,
        GJAHR like BSIS-GJAHR,
        BELNR like BSIS-BELNR,
        BUZEI like BSIS-BUZEI,
        DMBTR like BSIS-DMBTR,
        DMBE2 like BSIS-DMBE2,
        AUFNR like BSIS-AUFNR,
        SHKZG like BSIS-SHKZG,
        BUDAT like BSIS-BUDAT,
        SGTXT like BSIS-SGTXT,
      end of T_BSIS.


data: T_SKA1_6 like T_SKA1 occurs 0 with header line.
data: T_SKA1_3 like T_SKA1 occurs 0 with header line.
data: T_BSIS_3 like T_BSIS occurs 0 with header line.
data: T_BSIS_6 like T_BSIS occurs 0 with header line.


data: begin of T_SKB11 occurs 0,
        BUKRS type SKB1-BUKRS,
        SAKNR type SKB1-SAKNR,
      end of T_SKB11.

data: begin of T_SKB1_LB occurs 0,
        BUKRS type SKB1-BUKRS,
        SAKNR type SKB1-SAKNR,
        FDLEV type SKB1-FDLEV,
      end of T_SKB1_LB.

data: begin of T_BSAK occurs 0,
        BUKRS like BSAK-BUKRS,
        LIFNR like BSAK-LIFNR,
        UMSKS like BSAK-UMSKS,
        UMSKZ like BSAK-UMSKZ,
        AUGDT like BSAK-AUGDT,
        AUGBL like BSAK-AUGBL,
        ZUONR like BSAK-ZUONR,
        GJAHR like BSAK-GJAHR,
        BELNR like BSAK-BELNR,
        BUZEI like BSAK-BUZEI,
        BLART type BSAK-BLART,
        EBELN type BSAK-EBELN,
      end of T_BSAK.

data: T_BSAK_AP like T_BSAK occurs 0 with header line.
data: T_BSAK_A like T_BSAK occurs 0 with header line.
data: T_BSAK_B like T_BSAK occurs 0 with header line.
data: begin of T_BSAK_AG occurs 0.
        include structure T_BSAK.
data:   DMBTR type BSAK-DMBTR,
        DMBE2 type BSAK-DMBE2,
      end of T_BSAK_AG.
data: begin of T_BSIS_DP occurs 0,
        BUKRS like BSIS-BUKRS,
        HKONT like BSIS-HKONT,
        AUGDT like BSIS-AUGDT,
        AUGBL like BSIS-AUGBL,
        ZUONR like BSIS-ZUONR,
        GJAHR like BSIS-GJAHR,
        BELNR like BSIS-BELNR,
        BUZEI like BSIS-BUZEI,
        DMBTR like BSIS-DMBTR,
        DMBE2 like BSIS-DMBE2,
        BUDAT like BSIS-BUDAT,
        SHKZG like BSIS-SHKZG,
      end of T_BSIS_DP.

data: T_BSIS_A like T_BSIS_DP occurs 0 with header line.
data: T_BSIS_1 like T_BSIS_DP occurs 0 with header line.
data: T_BSIS_LB like T_BSIS_DP occurs 0 with header line, "liquida banco
      T_SETLEAF type table of SETLEAF  with header line.

data: begin of T_BSAS_LB occurs 0,
        BUKRS like BSAS-BUKRS,
        HKONT like BSAS-HKONT,
        AUGDT like BSAS-AUGDT,
        AUGBL like BSAS-AUGBL,
        ZUONR like BSAS-ZUONR,
        GJAHR like BSAS-GJAHR,
        BELNR like BSAS-BELNR,
        BUZEI like BSAS-BUZEI,
        DMBTR like BSAS-DMBTR,
        DMBE2 like BSAS-DMBE2,
        BUDAT like BSAS-BUDAT,
        SHKZG like BSAS-SHKZG,
      end of T_BSAS_LB.


data: begin of T_BSIK occurs 0,
        BUKRS like BSIK-BUKRS,
        LIFNR like BSIK-LIFNR,
        UMSKS like BSIK-UMSKS,
        UMSKZ like BSIK-UMSKZ,
        AUGDT like BSIK-AUGDT,
        AUGBL like BSIK-AUGBL,
        ZUONR like BSIK-ZUONR,
        GJAHR like BSIK-GJAHR,
        BELNR like BSIK-BELNR,
        BUZEI like BSIK-BUZEI,
        DMBTR like BSIK-DMBTR,
        DMBE2 like BSIK-DMBE2,
        ZFBDT like BSIK-ZFBDT,
        ZBD1T like BSIK-ZBD1T,
        BUDAT like BSIK-BUDAT,
        SHKZG like BSIK-SHKZG,
        BLART like BSIK-BLART,
        WRBTR like BSIK-WRBTR,
      end of T_BSIK.

data: T_BSIK_1 like T_BSIK occurs 0 with header line.
data: T_BSIK_AG like table of T_BSIK with header line.

data: begin of T_BKPF occurs 0,
        BUKRS like BKPF-BUKRS,
        BELNR like BKPF-BELNR,
        GJAHR like BKPF-GJAHR,
        TCODE like BKPF-TCODE,
      end of T_BKPF.

data: begin of T_BSEG occurs 0,
        BUKRS like BSEG-BUKRS,
        BELNR like BSEG-BELNR,
        GJAHR like BSEG-GJAHR,
        ANLN1 like BSEG-ANLN1,
        ANLN2 like BSEG-ANLN2,
        DMBTR like BSEG-DMBTR,
        DMBE2 like BSEG-DMBE2,
        BUZEI like BSEG-BUZEI,
        SHKZG like BSEG-SHKZG,
        ANBWA like BSEG-ANBWA,
        HKONT like BSEG-HKONT,
        SGTXT like BSEG-SGTXT,
        UMSKS like BSEG-UMSKS,
      end of T_BSEG.


data: begin of T_RBKP_1 occurs 0,
        BELNR like RBKP-BELNR,
        GJAHR like RBKP-GJAHR,
        XBLNR like RBKP-XBLNR,
      end of T_RBKP_1.

data: W_PEDIDO     type EKPO-NETWR,
      W_REALIZADO  type EKPO-NETWR,
      W_DIF        type EKPO-NETWR,
      W_CO_E(10),
      W_CO_U(10),
      W_DATA_E(10),
      W_DATA_U(10).

*Início Alteração Ricardo Furst.
*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*

type-pools: SLIS,
            KKBLO.

data: REPID           like SY-REPID.
data: FIELDCAT        type SLIS_T_FIELDCAT_ALV with header line.
data: LAYOUT          type SLIS_LAYOUT_ALV.
data: PRINT           type SLIS_PRINT_ALV.
data: SORT      type SLIS_T_SORTINFO_ALV with header line,
      EVENTS    type SLIS_T_EVENT,
      XS_EVENTS type SLIS_ALV_EVENT.

data: VARIANTE     like DISVARIANT,
      DEF_VARIANTE like DISVARIANT.

data: W_TIT(70).
* Fim Alteração Ricardo Furst.

* Início Alteração  IR080294 23-12-2021.
data: V_KOKRS type TKA02-KOKRS.

* Fim Alteração  IR080294 23-12-2021.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------

class LCL_EVENT_HANDLER definition.

  public section.

    class-methods:
      HANDLE_LINK_CLICK         for event LINK_CLICK
        of CL_GUI_ALV_TREE
        importing FIELDNAME NODE_KEY,

      HANDLE_EXPAND_NO_CHILDREN for event EXPAND_NC
        of CL_GUI_ALV_TREE
        importing NODE_KEY,

      HANDLE_ITEM_DOUBLE_CLICK  for event ITEM_DOUBLE_CLICK
        of CL_GUI_ALV_TREE
        importing NODE_KEY.
endclass.                    "lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class LCL_EVENT_HANDLER implementation.


  method HANDLE_EXPAND_NO_CHILDREN.

  endmethod.                    "HANDLE_EXPAND_NO_CHILDREN


  method HANDLE_ITEM_DOUBLE_CLICK.
    read table T_SAIDA into TW_SAIDA index NODE_KEY.
    if not TW_SAIDA-POSNR is initial and
           TW_SAIDA-EBELN is initial and
           TW_SAIDA-BELNR3 is initial.
      set parameter id 'IAF' field TW_SAIDA-POSNR.
      call transaction 'IMA3N' and skip first screen.
    endif.

    if not TW_SAIDA-AUFNR is initial and
           TW_SAIDA-EBELN is initial and
           TW_SAIDA-BELNR3 is initial.
      set parameter id 'ANR' field TW_SAIDA-AUFNR.
      call transaction 'KO03' and skip first screen.
    endif.

    if not TW_SAIDA-ANLN1 is initial and
           TW_SAIDA-EBELN is initial and
           TW_SAIDA-BELNR3 is initial.
      set parameter id 'AN1' field TW_SAIDA-ANLN1.
      call transaction 'AS03' and skip first screen.
    endif.

  endmethod.                    "handle_expand_no_children

  method HANDLE_LINK_CLICK.
    read table T_SAIDA into TW_SAIDA index NODE_KEY.
    case FIELDNAME.
      when 'EBELN'.
        set parameter id 'BES' field TW_SAIDA-EBELN.
        call transaction 'ME23N'.

      when 'BELNR3'.
        set parameter id 'BLN' field TW_SAIDA-BELNR3.
        set parameter id 'GJR' field TW_SAIDA-AEDAT(4).
        set parameter id 'BUK' field TW_SAIDA-ABUKRS.
        call transaction 'FB03' and skip first screen.

      when 'BELNR6'.
        set parameter id 'BLN' field TW_SAIDA-BELNR6.
        set parameter id 'GJR' field TW_SAIDA-AEDAT(4).
        set parameter id 'BUK' field TW_SAIDA-ABUKRS.
        call transaction 'FB03' and skip first screen.

      when 'BELNR1'.
        if TW_SAIDA-BELNR2(4) = 'MIGO' or
           TW_SAIDA-BELNR2(4) = 'MB1A'.
          call function 'MIGO_DIALOG'
            exporting
              I_ACTION            = 'A04'
              I_REFDOC            = 'R02'
              I_NOTREE            = 'X'
              I_SKIP_FIRST_SCREEN = 'X'
              I_OKCODE            = 'OK_GO'
              I_MBLNR             = TW_SAIDA-BELNR1
              I_MJAHR             = TW_SAIDA-AEDAT(4)
            exceptions
              ILLEGAL_COMBINATION = 1
              others              = 2.

        elseif TW_SAIDA-BELNR2(4) = 'MIRO'.
          set parameter id 'RBN' field TW_SAIDA-BELNR1.
          set parameter id 'GJR' field TW_SAIDA-AEDAT(4).
          call transaction 'MIR4' and skip first screen.

        endif.
      when 'BELNR5'.
        set parameter id 'RBN' field TW_SAIDA-BELNR5.
        set parameter id 'GJR' field TW_SAIDA-AEDAT(4).
        call transaction 'MIR4' and skip first screen.
    endcase.
  endmethod.                    "HANDLE_LINK_CLICK
endclass.                    "lcl_event_handler IMPLEMENTATION
data: G_APPLICATION type ref to LCL_EVENT_HANDLER.
* -------------------------------------------------------------------- *
*    Tela de Seleção                                                   *
* -------------------------------------------------------------------- *
selection-screen begin of block B0 with frame title text-S00.
  parameters: P_ON  radiobutton group G0 default 'X' user-command OCOM,
              P_OFF radiobutton group G0.

  select-options: S_USER for SY-UNAME no-extension no intervals matchcode object USER_COMP,
                  S_HORA for SY-UZEIT no-extension no intervals,
                  S_DATA for SY-DATUM no-extension no intervals.
selection-screen end of block B0.

selection-screen begin of block B3 with frame title text-S03.
  parameters: P_CONSOL radiobutton group G1 default 'X' user-command XCOM,
              P_ANALIT radiobutton group G1,
              P_SINCAX radiobutton group G1.
selection-screen end of block B3.

selection-screen begin of block B1 with frame title text-S01.
  select-options: S_BUKRS  for T001-BUKRS      no-extension no intervals,
                  S_WERKS  for IMAK-WERKS      ,"NO-EXTENSION NO INTERVALS,
                  S_KOSTL  for IMAKPA-AKOSTL,
                  S_POSNR  for IMAK-POSNR,
                  S_ANLN1  for ANLA-ANLN1,
                  S_AUFNR  for COAS-AUFNR     matchcode object ORDE,
                  S_EBELN  for EKKO-EBELN,
                  S_LIFNR  for EKKO-LIFNR,
                  S_LIFRE  for EKKO-LIFRE,
                  S_KONNR  for EKAB-KONNR matchcode object SPACE no-display,
                  S_BUDAT  for EKBE-BUDAT no-extension,
                  S_CPUDT  for EKBE-CPUDT                no-extension,
*                s_gjahr  FOR imak-gjahr.
                  S_GJAHR  for IMAK-GJAHR no-extension.
  selection-screen skip.
  selection-screen begin of block B2 with frame title text-S02.
    parameters: P_MOEDA    type ZIM01_MOEDA , "fcurr_curr OBLIGATORY ,
                P_HIST     as checkbox default 'X' user-command UCOM,
                P_DATAC    type DATUM,
                P_CARGA(1) no-display,
                P_TAXAC    type TCURR-UKURS.
  selection-screen end of block B2.
  selection-screen skip.
*Início Alteração Ricardo Furst.
  selection-screen begin of block B4 with frame title text-S04.
*PARAMETERS: p_alv AS CHECKBOX USER-COMMAND flag.
*PARAMETER: p_alv RADIOBUTTON GROUP grup." USER-COMMAND flag.
    parameter: P_TREE radiobutton group GRUP default 'X'  .
    parameter: P_ALV radiobutton group GRUP." USER-COMMAND flag.
    parameters: P_VARIA like DISVARIANT-VARIANT.
  selection-screen end of block B4.

*Fim Alteração Ricardo Furst.
selection-screen end of block B1.

*Início Alteração Ricardo Furst.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
data: VG_REPID   like SY-REPID,
      VG_VARIANT type DISVARIANT.


initialization.

  message S000(Z01) with 'Transação descontinuada,  dados referentes a '
                         'investimentos devem ser pegos no Power Bi.'
                         'Se você utiliza esta transação,  favor entrar'
                         'em contato com equipe de Investimentos' .

  if S_USER[] is initial.

    data: WL_VIS type ZIM08_REL_INV2-VISAO.

    case 'X'.
      when P_CONSOL.
        WL_VIS = '01'.
      when P_ANALIT.
        WL_VIS = '02'.
      when P_SINCAX .
        WL_VIS = '03'.
    endcase.

*    select visao usuario data_exec hora_exec from zim08_rel_inv2
*      into table t_sel  where visao = wl_vis order by visao ascending data_exec descending hora_exec descending.

    if S_BUKRS is not initial.
      select single AEDAT AENTIME AENUSER
        from ZIMT003
        into (S_DATA-LOW, S_HORA-LOW, S_USER-LOW)
         where BUKRS in S_BUKRS.
      "WHERE visao = wl_vis.

      if SY-SUBRC = 0.
*      read table t_sel with key visao = wl_vis.
*      s_user-low = t_sel-usuario.
*      s_data-low = t_sel-data_exec.
*      s_hora-low = t_sel-hora_exec.
        S_USER-SIGN   = S_DATA-SIGN   = S_HORA-SIGN   = 'I'.
        S_USER-OPTION = S_DATA-OPTION = S_HORA-OPTION = 'EQ'.
        append: S_USER, S_DATA, S_HORA.
      endif.

    endif.
  endif.

at selection-screen.


at selection-screen on value-request for P_VARIA.

*  IF NOT p_varia IS INITIAL.
  VG_REPID          = SY-REPID.
  VARIANTE-REPORT = VG_REPID.

  if ( not P_VARIA is initial ).
    VG_VARIANT-VARIANT = P_VARIA.
  endif.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      IS_VARIANT    = VARIANTE
      I_SAVE        = 'A'
    importing
      ES_VARIANT    = VARIANTE
    exceptions
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      others        = 3.

  if ( SY-SUBRC ne 0 ).
    message S000(Z01) with 'Não existe variante'.
    stop.
  else.
    move VARIANTE-VARIANT to P_VARIA.
  endif.
*  ENDIF.
*Fim Alteração Ricardo Furst.

* -------------------------------------------------------------------- *
*    Consistencia da tela de selecao                                   *
* -------------------------------------------------------------------- *
at selection-screen output.

  loop at screen.

    if SCREEN-NAME = 'P_DATAC' or
       SCREEN-NAME = 'P_TAXAC'.

      if P_MOEDA eq 'BRL'.
        SCREEN-INPUT = ''.
        clear: P_DATAC, P_TAXAC.
      else.

        if not P_HIST  is initial.
          clear: P_DATAC, P_TAXAC.
          SCREEN-INPUT = ''.
        else.
          if not P_TAXAC is initial.
            clear: P_DATAC.
            if SCREEN-NAME = 'P_DATAC'.
              SCREEN-INPUT = ''.
            endif.
          endif.
        endif.
      endif.
      modify screen.
    endif.

    case 'X'.

      when P_CONSOL or P_SINCAX.
        case SCREEN-NAME(7).
          when 'S_GJAHR' or '%_S_GJA'.
            SCREEN-ACTIVE = 0.
          when others.
            SCREEN-ACTIVE = 1.
        endcase.

      when P_ANALIT.
        case SCREEN-NAME(7).
          when 'S_BUDAT' or 'S_CPUDT' or
               '%_S_BUD' or '%_S_CPU'.
            SCREEN-ACTIVE = 0.
          when others.
            SCREEN-ACTIVE = 1.
        endcase.

    endcase.

    case 'X'.
      when P_ON.

        case SCREEN-NAME(6).
          when 'S_DATA' or 'S_HORA' or 'S_USER' or
               '%_S_DA' or '%_S_HO' or '%_S_US'.
            SCREEN-ACTIVE = 0.

          when others.
        endcase.

      when P_OFF.
        if S_BUKRS is not initial.
          case SCREEN-NAME(6).
            when 'S_DATA' or 'S_HORA' or 'S_USER' or
                 '%_S_BU' or '%_S_CP' or '%_S_USE'.
*            screen-active = 1.
              SCREEN-INPUT = 0.
            when others.
          endcase.
        else.

          case SCREEN-NAME(6).
            when 'S_DATA' or 'S_HORA' or 'S_USER' or
                 '%_S_DA' or '%_S_HO' or '%_S_US'.
              SCREEN-ACTIVE = 0.

            when others.
          endcase.
        endif.
      when others.
    endcase.

    modify screen.

  endloop.

  if P_OFF is not initial
    and   S_BUKRS is initial.
    message S000(Z01) with 'Preencher campo EMPRESA!'.
    move 'X' to P_ON.
    clear: P_OFF.
*    stop.
  endif.

  if not P_OFF is initial.
    data: WL_VIS type ZIM08_REL_INV2-VISAO.

    case 'X'.
      when P_CONSOL.
        WL_VIS = '01'.
      when P_ANALIT.
        WL_VIS = '02'.
      when P_SINCAX .
        WL_VIS = '03'.
    endcase.


    clear: S_USER, S_DATA, S_HORA. refresh: S_USER, S_DATA, S_HORA.

    if S_BUKRS is not initial.
      select single AEDAT AENTIME AENUSER
        from ZIMT003
        into (S_DATA-LOW, S_HORA-LOW, S_USER-LOW)
         where BUKRS in S_BUKRS.
*    select visao usuario data_exec hora_exec from zim08_rel_inv2
*      into table t_sel  where visao = wl_vis order by visao ascending data_exec descending hora_exec descending.

      if SY-SUBRC = 0.
*      read table t_sel with key visao = wl_vis.
*      s_user-low = t_sel-usuario.
*      s_data-low = t_sel-data_exec.
*      s_hora-low = t_sel-hora_exec.
        S_USER-SIGN   = S_DATA-SIGN   = S_HORA-SIGN   = 'I'.
        S_USER-OPTION = S_DATA-OPTION = S_HORA-OPTION = 'EQ'.
        append: S_USER, S_DATA, S_HORA.
      endif.
    endif.
  endif.
* -------------------------------------------------------------------- *
*    Processamento principal do programa                               *
* -------------------------------------------------------------------- *
start-of-selection.

  WL_HORA = SY-UZEIT.
  if P_CARGA is not initial.
    SY-TCODE = 'ZIM08'.
  endif.
  if P_MOEDA is initial.
    P_MOEDA = 'BRL'.
  endif.

* inicio alteração IR080294
  if S_BUKRS is not initial.
    select single KOKRS from TKA02 into V_KOKRS where BUKRS in S_BUKRS.
  endif.
* fim alteração IR080294

  if P_OFF eq 'X'.
*    CALL SCREEN 100.
    if not S_CPUDT[] is initial.
      if S_BUDAT[] is initial.
        message S000(Z01) with 'Preencher campo DATA DE LANÇAMENTO!'.
        stop.
      endif.
    endif.

    if P_ALV is initial.
      call screen 100.
    else.
      perform F_SELECIONA_DADOS_OFFLINE.

      select IMAK~POSNR IMAK~ABUKRS IMAK~VGSBER IMAK~OBJNR
             IMAKT~TXT50 IMAKPA~AKOSTL IMAK~VGSBER T001W~NAME1
        from IMAK inner join IMAKT on
          IMAK~POSNR eq IMAKT~POSNR
                  inner join IMAKPA on
          IMAK~POSNR eq IMAKPA~POSNR
                          inner join T001W on
          IMAK~VGSBER eq T001W~WERKS

        into table T_IMAK
        where IMAK~POSNR    in S_POSNR  and
              IMAK~ABUKRS   in S_BUKRS  and
              IMAK~VGSBER   in S_WERKS  and
              IMAKPA~AKOSTL in S_KOSTL and
              IMAKT~SPRAS   eq SY-LANGU."

      loop at T_08.
        move-corresponding T_08 to TW_SAIDA.
        select single BUTXT from T001 into TW_SAIDA-BUTXT
          where BUKRS eq TW_SAIDA-ABUKRS.


*INICIO ALTERACAO IR080294
*        SELECT SINGLE ltext FROM cskt INTO tw_saida-ltext
*          WHERE spras EQ sy-langu AND
*                kokrs EQ 'MAGI'  AND
*                kostl EQ tw_saida-akostl AND
*                datbi GE sy-datum .
        select single LTEXT from CSKT into TW_SAIDA-LTEXT
          where SPRAS eq SY-LANGU and
                KOKRS eq V_KOKRS   and
                KOSTL eq TW_SAIDA-AKOSTL and
                DATBI ge SY-DATUM .

*FIM ALTERACAO IR080294


        append TW_SAIDA.
      endloop.



      perform F_PREPARA_DADOS .
      perform ORGANIZA_TEXTO.

      perform F_MONTA_CATALOGO.
      perform F_MONTA_QUEBRA.
      call screen 200.
    endif.

  else.


    if SY-TCODE ne 'ZIM08' and SY-BATCH ne 'X'.
      perform F_CONSISTENCIA_DA_SELECAO.
    else.

      if not S_CPUDT[] is initial.
        if S_BUDAT[] is initial.
          message S000(Z01) with 'Preencher campo DATA DE LANÇAMENTO!'.
          stop.
        endif.
      endif.

      if S_BUDAT[] is initial.
        refresh S_BUDAT[].
        clear S_BUDAT.
        S_BUDAT-SIGN = 'I'.
        S_BUDAT-OPTION = 'BT'.

        concatenate S_GJAHR-LOW '0101'  into S_BUDAT-LOW.

        if S_GJAHR-HIGH is initial.
          concatenate S_GJAHR-LOW '1231'  into S_BUDAT-HIGH.
        else.
          concatenate S_GJAHR-HIGH '1231' into S_BUDAT-HIGH.
        endif.

        append S_BUDAT.
      endif.
    endif.

    if not P_ON is initial.

      perform F_SELECIONA_DADOS.

      sort T_EKKO by EBELN EBELP ANLN1 ANLN2 AUFNR ZEKKN.
      delete adjacent duplicates from T_EKKO comparing EBELN EBELP ANLN1 ANLN2 AUFNR ZEKKN.

*      IF p_sincax IS INITIAL.
      perform VALIDA_RATEIO.

*      ENDIF.

    else.

    endif.
*Início Alteração Ricardo Furst.

    if SY-TCODE eq 'ZIM08' or SY-BATCH eq 'X'.
      perform CREATE_HIERARCHY.
    else.

      if P_ALV is initial.
        call screen 100.
      else.
        perform F_PREPARA_DADOS .
        perform ORGANIZA_TEXTO.
        perform F_MONTA_CATALOGO.
        perform F_MONTA_QUEBRA.
        call screen 200.
      endif.
    endif.
  endif.
*Fim Alteração Ricardo Furst.
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTENCIA_DA_SELECAO
*&---------------------------------------------------------------------*
* Consiste a tela de seleçãp
*----------------------------------------------------------------------*
form F_CONSISTENCIA_DA_SELECAO .

  if S_BUKRS-LOW is initial.
    message I000(Z01) with 'Preencher campo Empresa!'.
    stop.
  endif.

  if P_MOEDA is initial.
    message I000(Z01) with 'Preencher campo Moeda!'.
    stop.
  endif.

  if not P_ANALIT is initial.
    if S_GJAHR-LOW is initial.
      message I000(Z01) with 'Preencher campo Ano de Execução!'.
      stop.
    endif.

    refresh S_BUDAT[].
    clear S_BUDAT.
    S_BUDAT-SIGN = 'I'.
    S_BUDAT-OPTION = 'BT'.

    concatenate S_GJAHR-LOW '0101'  into S_BUDAT-LOW.

    if S_GJAHR-HIGH is initial.
      concatenate S_GJAHR-LOW '1231'  into S_BUDAT-HIGH.
    else.
      concatenate S_GJAHR-HIGH '1231' into S_BUDAT-HIGH.
    endif.

*    CONCATENATE s_gjahr '01' INTO s_budat-low.
*    ADD 1 TO s_gjahr.
*    CONCATENATE s_gjahr '01' INTO s_budat-high.
*    SUBTRACT 1 FROM: s_budat-high,
*                     s_gjahr.
    append S_BUDAT.
  endif.

  if P_ON is not initial
  and ( SY-TCODE ne 'ZIM08'
    and SY-TCODE ne 'ZIM08_NEW'
    and SY-TCODE ne 'ZIM12' ).
    if P_ANALIT is initial.
      if S_BUDAT[] is initial.
        message I000(Z01) with 'Preencher campo Data de Lançamento!'.
      endif.
    endif.
  endif.
*** validar s_budat

  set parameter id 'BUK' field S_BUKRS-LOW.

  write: S_BUDAT-LOW  to W_BUDAT_LOW,
         S_BUDAT-HIGH to W_BUDAT_HIGH,
         S_CPUDT-LOW  to W_CPUDT_LOW,
         S_CPUDT-HIGH to W_CPUDT_HIGH.

  select single BUTXT from T001 into W_NOME
    where BUKRS in S_BUKRS.

  if not P_CONSOL is initial.
*    w_visao = 'Consolidado (Competência e Caixa)'.
    W_VISAO = 'Competência'.
  elseif not P_ANALIT is initial.
*    w_visao =   'Analitico/Sintético (Competência)'.
    W_VISAO = 'Consolidado (Competência e Caixa)'.
  elseif not P_SINCAX is initial.
    W_VISAO = 'Caixa'.
  endif.

  if P_MOEDA ne 'BRL' and
     P_MOEDA ne 'EUR' and
     P_MOEDA ne 'USD'.
    message S000(Z01) with 'Moeda não cadastrada para o relatório'.
    stop.
  endif.

  if S_BUKRS is initial.
    if not S_LIFNR[] is initial.
      message S000(Z01) with 'Preencher campo EMPRESA!'.
      stop.
    endif.

    if not S_LIFRE[] is initial.
      message S000(Z01) with 'Preencher campo EMPRESA!'.
      stop.
    endif.
  endif.

  if P_MOEDA ne 'BRL' and
     P_DATAC is initial and
     P_HIST  is initial and
     P_TAXAC is initial.
    message S000(Z01) with 'Preencher campo DATA DA COTAÇÃO!'.
    stop.
  endif.

  if not S_CPUDT[] is initial.
    if S_BUDAT[] is initial.
      message S000(Z01) with 'Preencher campo DATA DE LANÇAMENTO!'.
      stop.
    endif.
  endif.

  if P_ON is not initial
  and  ( SY-TCODE ne 'ZIM08'
    and SY-TCODE ne 'ZIM08_NEW'
    and SY-TCODE ne 'ZIM12' ).
    if S_BUDAT[] is initial.
      message S000(Z01) with 'Preencher campo DATA DE LANÇAMENTO!'.
      stop.
    endif.
  endif.
endform.                    " F_CONSISTENCIA_DA_SELECAO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona dados das tabelas
*----------------------------------------------------------------------*
form F_SELECIONA_DADOS .

  data: T_IMAKA_SORT like sorted table of T_IMAKA with unique key POSNR LFDNR BUKRS.
  data: T_IMAKZ_SORT like sorted table of T_IMAKZ with unique key POSNR OBJNR.
  data: T_COAS_HASH like hashed table of T_COAS with unique key AUFNR.



  perform F_MSG using 'Selecionando dados da solicitação'
                      '0'.

  select IMAK~POSNR IMAK~ABUKRS IMAK~VGSBER IMAK~OBJNR
         IMAKT~TXT50 IMAKPA~AKOSTL
*         imaka~anln1 imaka~anln2
    from IMAK inner join IMAKT on
      IMAK~POSNR eq IMAKT~POSNR
              inner join IMAKPA on
      IMAK~POSNR eq IMAKPA~POSNR
*              LEFT OUTER JOIN imaka ON
*    ( imak~posnr  EQ imaka~posnr AND
*      imak~abukrs EQ imaka~bukrs     )

    into table T_IMAK
    where IMAK~POSNR    in S_POSNR  and
          IMAK~ABUKRS   in S_BUKRS  and
          IMAK~VGSBER   in S_WERKS  and
          IMAKPA~AKOSTL in S_KOSTL and
          IMAKT~SPRAS   eq SY-LANGU." AND
*          imaka~anln1 IN s_anln1.

  if SY-SUBRC ne 0.
    message I000(Z01) with 'Não existe Solicitação de investimento'
                           '/Centro de custo para seleção informada'.
    stop.
  endif.

* Se apenas Imobilizado for opcao, nao buscar
  if not ( not S_AUFNR[] is initial and
               S_ANLN1[] is initial     ).
    perform F_BUSCA_IMOBILIZADOS.
  endif.

* Se apenas Imobilizado for opcao, nao buscar
  if not (     S_AUFNR[] is initial and
           not S_ANLN1[] is initial     ).
    perform F_BUSCA_ORDEM_INT_EST.
  endif.


  perform F_MSG using 'Selecionando dados do pedido'
                      '5'.
*** Busca Fornecedor
  if not S_EBELN[] is initial or
     not S_LIFNR[] is initial or
     not S_LIFRE[] is initial.

    if P_SINCAX is initial.
      select EKKO~EBELN EKKO~LIFNR EKKO~LIFRE EKKO~AEDAT
             EKKO~WKURS EKKO~WAERS EKKO~BSART EKPO~EBELP
             EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~MEINS
             EKPO~NETPR EKPO~NETWR EKPO~PACKNO EKPO~KONNR EKPO~KTPNR EKPO~EFFWR

             EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
             EKKN~VPROZ
        from EKKO inner join EKPO on
        EKKO~EBELN eq EKPO~EBELN
                  inner join EKKN on
      ( EKKO~EBELN eq EKKN~EBELN and
        EKPO~EBELP eq EKKN~EBELP     )

                    inner join EKBE on
        ( EKKO~EBELN eq EKBE~EBELN and
          EKPO~EBELP eq EKBE~EBELP )

        into table T_EKKO
        where EKKO~EBELN in S_EBELN and
              EKKO~LIFNR in S_LIFNR and
              EKKO~LIFRE in S_LIFRE and
              EKKO~BUKRS in S_BUKRS and
*            ekko~aedat IN s_budat AND
              EKKO~BSTYP eq 'F'     and
              EKKO~BSTYP eq 'F'     and
*            ekko~aedat IN s_budat AND
  EKBE~BUDAT in S_BUDAT and
              EKPO~KNTTP in ('A', 'F') and
              EKPO~LOEKZ ne 'X'.

    else.
      select EKKO~EBELN EKKO~LIFNR EKKO~LIFRE EKKO~AEDAT
             EKKO~WKURS EKKO~WAERS EKKO~BSART EKPO~EBELP
             EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~MEINS
             EKPO~NETPR EKPO~NETWR EKPO~PACKNO EKPO~KONNR EKPO~KTPNR EKPO~EFFWR

             EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
             EKKN~VPROZ
        from EKKO inner join EKPO on
        EKKO~EBELN eq EKPO~EBELN
                  inner join EKKN on
      ( EKKO~EBELN eq EKKN~EBELN and
        EKPO~EBELP eq EKKN~EBELP     )

                    inner join EKBE on
        ( EKKO~EBELN eq EKBE~EBELN and
          EKPO~EBELP eq EKBE~EBELP )

        into table T_EKKO
        where EKKO~EBELN in S_EBELN and
              EKKO~LIFNR in S_LIFNR and
              EKKO~LIFRE in S_LIFRE and
              EKKO~BUKRS in S_BUKRS and
*            ekko~aedat IN s_budat AND
              EKKO~BSTYP eq 'F'     and
              EKKO~BSTYP eq 'F'     and
*            ekko~aedat IN s_budat AND
*EKBE~BUDAT IN S_BUDAT AND
              EKPO~KNTTP in ('A', 'F') and
              EKPO~LOEKZ ne 'X'.

    endif.

    if SY-SUBRC <> 0.
      message I000(Z01) with 'Solicitação de Investimento não encontrada'.
    else.
      loop at T_EKKO.
        if not T_EKKO-ANLN1 is initial.
          read table T_IMAKA with key ANLN1 = T_EKKO-ANLN1
                                      ANLN2 = T_EKKO-ANLN2
                                      binary search.
        else.
          read table T_IMAKZ with key AUFNR = T_EKKO-AUFNR
                                      binary search.
        endif.
        if SY-SUBRC <> 0.
          delete T_EKKO.
        endif.
      endloop.
      if T_EKKO[] is initial.
        message I000(Z01) with 'Solicitação de Investimento não encontrada'.
      endif.
    endif.
  else.
*** Se nenhum dos campos acima forem preenchidos, fazer a busca invertida

    if not T_IMAKA[] is initial.
      if P_SINCAX is initial.
        select EKKO~EBELN EKKO~LIFNR EKKO~LIFRE EKKO~AEDAT
               EKKO~WKURS  EKKO~WAERS EKKO~BSART EKPO~EBELP
               EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~MEINS
               EKPO~NETPR EKPO~NETWR EKPO~PACKNO EKPO~KONNR EKPO~KTPNR EKPO~EFFWR

               EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
               EKKN~VPROZ
          from EKKO inner join EKPO on
          EKKO~EBELN eq EKPO~EBELN
                    inner join EKKN on
        ( EKKO~EBELN eq EKKN~EBELN and
          EKPO~EBELP eq EKKN~EBELP     )

                    inner join EKBE on
        ( EKKO~EBELN eq EKBE~EBELN and
          EKPO~EBELP eq EKBE~EBELP     )

          into table T_EKKO
          for all entries in T_IMAKA
          where EKKO~EBELN in S_EBELN      and
                EKKO~LIFNR in S_LIFNR       and
                EKKO~LIFRE in S_LIFRE       and
                EKKO~BUKRS in S_BUKRS       and
** MODIFICAÇÃO 18.04.2010
*              ekko~aedat IN s_budat       AND
                EKBE~BUDAT in S_BUDAT and
** MODIFICAÇÃO 18.04.2010
                EKKO~BSTYP eq 'F'     and
                EKPO~KNTTP in ('A') and
                EKPO~LOEKZ ne 'X'         and
                EKKN~ANLN1 eq T_IMAKA-ANLN1 and
                EKKN~ANLN2 eq T_IMAKA-ANLN2.
      else.
        select EKKO~EBELN EKKO~LIFNR EKKO~LIFRE EKKO~AEDAT
         EKKO~WKURS  EKKO~WAERS EKKO~BSART EKPO~EBELP
         EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~MEINS
         EKPO~NETPR EKPO~NETWR EKPO~PACKNO EKPO~KONNR EKPO~KTPNR EKPO~EFFWR

         EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
         EKKN~VPROZ
    from EKKO inner join EKPO on
    EKKO~EBELN eq EKPO~EBELN
              inner join EKKN on
  ( EKKO~EBELN eq EKKN~EBELN and
    EKPO~EBELP eq EKKN~EBELP     )

              inner join EKBE on
  ( EKKO~EBELN eq EKBE~EBELN and
    EKPO~EBELP eq EKBE~EBELP     )

    into table T_EKKO
    for all entries in T_IMAKA
    where EKKO~EBELN in S_EBELN      and
          EKKO~LIFNR in S_LIFNR       and
          EKKO~LIFRE in S_LIFRE       and
          EKKO~BUKRS in S_BUKRS       and
** MODIFICAÇÃO 18.04.2010
*              ekko~aedat IN s_budat       AND
*              EKBE~BUDAT IN S_BUDAT AND
** MODIFICAÇÃO 18.04.2010
          EKKO~BSTYP eq 'F'     and
          EKPO~KNTTP in ('A') and
          EKPO~LOEKZ ne 'X'         and
          EKKN~ANLN1 eq T_IMAKA-ANLN1 and
          EKKN~ANLN2 eq T_IMAKA-ANLN2.

      endif.
    endif.
    if not T_IMAKZ[] is initial.
      if P_SINCAX is initial.
        select EKKO~EBELN EKKO~LIFNR EKKO~LIFRE EKKO~AEDAT
               EKKO~WKURS  EKKO~WAERS EKKO~BSART EKPO~EBELP
               EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~MEINS
               EKPO~NETPR EKPO~NETWR EKPO~PACKNO EKPO~KONNR EKPO~KTPNR EKPO~EFFWR

               EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
               EKKN~VPROZ
          from EKKO inner join EKPO on
          EKKO~EBELN eq EKPO~EBELN
                    inner join EKKN on
        ( EKKO~EBELN eq EKKN~EBELN and
          EKPO~EBELP eq EKKN~EBELP     )

                    inner join EKBE on
        ( EKKO~EBELN eq EKBE~EBELN and
          EKPO~EBELP eq EKBE~EBELP     )

          appending table T_EKKO
          for all entries in T_IMAKZ
          where EKKO~EBELN in S_EBELN       and
                EKKO~LIFNR in S_LIFNR       and
                EKKO~LIFRE in S_LIFRE       and
                EKKO~BUKRS in S_BUKRS       and
*              ekko~aedat IN s_budat       AND
  EKBE~BUDAT in S_BUDAT and
                EKKO~BSTYP eq 'F'           and
                EKPO~KNTTP in ('F')    and
                EKPO~LOEKZ ne 'X'         and
                EKKN~AUFNR eq T_IMAKZ-AUFNR.

      else.
        select EKKO~EBELN EKKO~LIFNR EKKO~LIFRE EKKO~AEDAT
       EKKO~WKURS  EKKO~WAERS EKKO~BSART EKPO~EBELP
       EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~MEINS
       EKPO~NETPR EKPO~NETWR EKPO~PACKNO EKPO~KONNR EKPO~KTPNR EKPO~EFFWR

       EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
       EKKN~VPROZ
  from EKKO inner join EKPO on
  EKKO~EBELN eq EKPO~EBELN
            inner join EKKN on
( EKKO~EBELN eq EKKN~EBELN and
  EKPO~EBELP eq EKKN~EBELP     )

            inner join EKBE on
( EKKO~EBELN eq EKBE~EBELN and
  EKPO~EBELP eq EKBE~EBELP     )

  appending table T_EKKO
  for all entries in T_IMAKZ
  where EKKO~EBELN in S_EBELN       and
        EKKO~LIFNR in S_LIFNR       and
        EKKO~LIFRE in S_LIFRE       and
        EKKO~BUKRS in S_BUKRS       and
*              ekko~aedat IN s_budat       AND
*ekbe~budat in s_budat and
        EKKO~BSTYP eq 'F'           and
        EKPO~KNTTP in ('F')    and
        EKPO~LOEKZ ne 'X'         and
        EKKN~AUFNR eq T_IMAKZ-AUFNR.
      endif.
    endif.
  endif.

  perform F_MSG using 'Selecionando dados do contrato'
                      '10'.
*** Busca dados do Contrato
  if not S_KONNR[] is initial.
    select EKAB~KONNR EKAB~KTPNR EKAB~EBELN EKAB~EBELP
           EKKN~ZEKKN EKKN~AUFNR EKKN~ANLN1 EKKN~ANLN2
           EKKN~VPROZ
            from EKAB inner join EKPO on
          EKAB~EBELN eq EKPO~EBELN
                    inner join EKKN on
        ( EKAB~EBELN eq EKKN~EBELN and
          EKAB~EBELP eq EKKN~EBELP     )
          into table T_EKAB
          where EKAB~KONNR in S_KONNR and
*                ekko~lifnr IN s_lifnr AND
*                ekko~lifre IN s_lifre AND
                EKAB~BUKRS in S_BUKRS and
                EKPO~KNTTP in ('A', 'F')  and
                EKPO~LOEKZ eq SPACE.
    if SY-SUBRC = 0.
      loop at T_EKAB.
        read table T_IMAKA with key ANLN1 = T_EKAB-ANLN1
                                    ANLN2 = T_EKAB-ANLN2
                                    binary search.
        if SY-SUBRC <> 0.
          delete T_EKAB.
        endif.
      endloop.
      if T_EKAB[] is initial.
        message I000(Z01) with 'Solicitação de Investimento não'
                               'encontrada para Contrato  informado'.
      endif.

      loop at T_EKKO.
        read table T_EKAB with key EBELN = T_EKKO-EBELN
                                   EBELP = T_EKKO-EBELP.
        if SY-SUBRC <> 0.
          delete T_EKKO.
        else.
          move T_EKKO-EBELN to S_EBELN-LOW.
          append S_EBELN.
        endif.
      endloop.

    endif.
  endif.

*  IF s_konnr[] IS INITIAL.
*    IF NOT t_imaka[] IS INITIAL.
*      SELECT ekab~konnr ekab~ktpnr ekab~ebeln ekab~ebelp
*             ekkn~zekkn ekkn~aufnr ekkn~anln1 ekkn~anln2
*             ekkn~vproz
*              FROM ekab INNER JOIN ekpo ON
*            ekab~ebeln EQ ekpo~ebeln
*                      INNER JOIN ekkn ON
*          ( ekab~ebeln EQ ekkn~ebeln AND
*            ekab~ebelp EQ ekkn~ebelp     )
*            INTO TABLE t_ekab
*        FOR ALL ENTRIES IN t_imaka
*            WHERE ekab~konnr IN s_konnr AND
**                ekko~lifnr IN s_lifnr AND
**                ekko~lifre IN s_lifre AND
*                  ekab~bukrs IN s_bukrs AND
*                  ekpo~knttp EQ 'A'     AND
*                  ekpo~loekz EQ space   AND
*                  ekkn~anln1 EQ t_imaka-anln1 AND
*                  ekkn~anln2 EQ t_imaka-anln2.
*    ENDIF.
*
*    IF NOT t_imakz[] IS INITIAL.
*      SELECT ekab~konnr ekab~ktpnr ekab~ebeln ekab~ebelp
*             ekkn~zekkn ekkn~aufnr ekkn~anln1 ekkn~anln2
*             ekkn~vproz
*              FROM ekab INNER JOIN ekpo ON
*            ekab~ebeln EQ ekpo~ebeln
*                      INNER JOIN ekkn ON
*          ( ekab~ebeln EQ ekkn~ebeln AND
*            ekab~ebelp EQ ekkn~ebelp     )
*            APPENDING table t_ekab
*            FOR ALL ENTRIES IN t_imakz
*            WHERE ekab~konnr IN s_konnr AND
**                ekko~lifnr IN s_lifnr AND
**                ekko~lifre IN s_lifre AND
*                  ekab~bukrs IN s_bukrs AND
*                  ekpo~knttp EQ 'F'     AND
*                  ekpo~loekz EQ space   AND
*                  ekkn~aufnr EQ t_imakz-aufnr.
*    ENDIF.
*  ENDIF.

  perform F_MSG using 'Selecionando dados do serviço'
                      '12'.
*** Busca de dados se for serviço o item
  clear T_PACK. refresh T_PACK.
  loop at T_EKKO.
    if not T_EKKO-PACKNO is initial.
      move T_EKKO-PACKNO to T_PACK-PACKNO.
      collect T_PACK. clear T_PACK.
    endif.
  endloop.

  if not T_PACK[] is initial.
    select PACKNO SUB_PACKNO from ESLL
      into table T_ESLL_1
      for all entries in T_PACK
      where PACKNO = T_PACK-PACKNO.

    if SY-SUBRC is initial.
      select PACKNO SUB_PACKNO SRVPOS KTEXT1 EXTROW from ESLL
      into table T_ESLL_2
      for all entries in T_ESLL_1
      where PACKNO = T_ESLL_1-SUB_PACKNO.
    endif.
  endif.

  perform F_MSG using 'Selecionando dados MIGO/MIRO'
                      '18'.
*** Busca dos dados da MIGO e MIRO
  if not T_EKKO[] is initial.
    select EKBE~EBELN EKBE~EBELP EKBE~ZEKKN EKBE~VGABE EKBE~GJAHR EKBE~BELNR
           EKBE~BUZEI EKBE~BWART EKBE~BUDAT EKBE~MENGE EKBE~DMBTR EKBE~WRBTR EKBE~WAERS
           EKBE~LFBNR EKBE~CPUDT EKBE~SHKZG EKBE~XBLNR
           EKKN~ANLN1 EKKN~AUFNR EKBE~BEKKN EKBE~CPUTM"ekbe~lfbnr
      from EKBE "INNER JOIN ekkn ON
*      ( ekbe~ebeln = ekkn~ebeln AND
*        ekbe~ebelp = ekkn~ebelp AND
*        ekbe~zekkn = ekkn~zekkn     )
      left outer join EKKN on
      ( EKBE~EBELN = EKKN~EBELN and
        EKBE~EBELP = EKKN~EBELP and
        EKBE~ZEKKN = EKKN~ZEKKN     )
      into table T_EKBE
      for all entries in T_EKKO
      where EKBE~EBELN eq T_EKKO-EBELN and
            EKBE~EBELP eq T_EKKO-EBELP and
            EKBE~VGABE in ('1', '2', '3', '9')." and
*            ekbe~gjahr in s_budat.


    select EKBE~EBELN EKBE~EBELP EKBE~ZEKKN EKBE~VGABE EKBE~GJAHR EKBE~BELNR
           EKBE~BUZEI EKBE~BWART EKBE~BUDAT EKBE~MENGE EKBE~DMBTR EKBE~WRBTR EKBE~WAERS
           EKBE~LFBNR EKBE~CPUDT EKBE~SHKZG EKBE~XBLNR
           EKKN~ANLN1 EKKN~AUFNR EKBE~BEKKN EKBE~CPUTM"ekbe~lfbnr
      from EKBE "INNER JOIN ekkn ON
*      ( ekbe~ebeln = ekkn~ebeln AND
*        ekbe~ebelp = ekkn~ebelp AND
*        ekbe~zekkn = ekkn~zekkn     )
      left outer join EKKN on
      ( EKBE~EBELN = EKKN~EBELN and
        EKBE~EBELP = EKKN~EBELP and
        EKBE~BEKKN = EKKN~ZEKKN     )
      into table T_EKBE_9
      for all entries in T_EKKO
      where EKBE~EBELN eq T_EKKO-EBELN and
            EKBE~EBELP eq T_EKKO-EBELP and
            EKBE~VGABE in ('1', '2', '3', '9').

    delete T_EKBE_9 where ZEKKN ne '02'.

    sort T_EKBE[] by EBELN EBELP ZEKKN ascending.
    sort T_EKBE_9[] by EBELN EBELP ZEKKN ascending.

    loop at T_EKBE_9.

      read table T_EKBE with key EBELN = T_EKBE_9-EBELN
                                 EBELP = T_EKBE_9-EBELP
                                 ZEKKN = T_EKBE_9-ZEKKN binary search.
      if SY-SUBRC <> 0.
        clear T_EKBE.
        move-corresponding T_EKBE_9 to T_EKBE.
        append T_EKBE.
        clear T_EKBE.
      endif.
    endloop.


*    IF NOT s_aufnr[] IS INITIAL.
*      DELETE t_ekbe WHERE NOT aufnr IN s_aufnr.
*    ENDIF.
*
*    IF NOT s_anln1[] IS INITIAL.
*      DELETE t_ekbe WHERE NOT anln1 IN s_anln1.
*    ENDIF.

    data: T_9 like T_EKBE occurs 0 with header line.
    data: T_1 like T_EKBE occurs 0 with header line.
    T_9[] = T_EKBE[].
    delete T_9 where VGABE ne '9'.
    delete T_EKBE where VGABE eq '9'.
    T_1[] = T_EKBE[].
    delete T_1 where VGABE ne '1'.
*sort t_ekbe by ebeln ebelp zekkn vgabe belnr bekkn anln1 aufnr.

*    DELETE ADJACENT DUPLICATES FROM t_ekbe COMPARING ebeln ebelp zekkn vgabe belnr bekkn anln1 aufnr.
    data: begin of T_AUX occurs 0,
            EBELN like EKBE-EBELN,
            EBELP like EKBE-EBELP,
            ZEKKN like EKBE-ZEKKN,
            BEKKN like EKBE-BEKKN,
*            belnr LIKE ekbe-belnr,
            COUNT type EKBE-DMBTR,
          end of T_AUX.

    clear T_AUX. refresh T_AUX.
    loop at T_EKBE.
      move-corresponding T_EKBE to T_AUX.
      T_AUX-COUNT = 1.
      collect T_AUX.
      clear T_AUX.
    endloop.

    sort T_AUX[] by EBELN EBELP BEKKN ZEKKN ascending.
    sort T_9[] by EBELN EBELP LFBNR ascending.
    sort T_1[] by EBELN EBELP ZEKKN LFBNR ascending.


    data: begin of T_EKKN2 occurs 0,
            EBELN like EKKN-EBELN,
            EBELP like EKKN-EBELP,
            ZEKKN like EKKN-ZEKKN,
            AUFNR like EKKN-AUFNR,
            ANLN1 like EKKN-ANLN1,
            ANLN2 like EKKN-ANLN2,
            VPROZ like EKKN-VPROZ,
          end of T_EKKN2.
*
    select EBELN EBELP ZEKKN AUFNR ANLN1 ANLN2 VPROZ
      from EKKN into table T_EKKN2
      for all entries in T_EKBE
      where EBELN = T_EKBE-EBELN and
            EBELP = T_EKBE-EBELP and
            ZEKKN = T_EKBE-BEKKN.
    sort T_EKKN2 by EBELN EBELP ZEKKN ascending.
  endif.

  loop at T_EKBE.
    read table T_EKKN2 with key EBELN = T_EKBE-EBELN
                                EBELP = T_EKBE-EBELP
                                ZEKKN = T_EKBE-BEKKN
                               binary search.
    if SY-SUBRC = 0.
      T_EKBE-AUFNR = T_EKKN2-AUFNR.
      T_EKBE-ANLN1 = T_EKKN2-ANLN1.
      T_EKBE-DMBTR = T_EKBE-DMBTR.
*    ELSE.
*      CLEAR: t_ekbe-aufnr, t_ekbe-anln1.
    endif.
    if T_EKBE-VGABE ne 1.
      read table T_AUX with key EBELN = T_EKBE-EBELN
                                EBELP = T_EKBE-EBELP
                                BEKKN = T_EKBE-BEKKN
                                ZEKKN = T_EKBE-ZEKKN binary search.
*                              belnr = t_ekbe-belnr.
      if T_AUX-COUNT > 1.

        if T_EKBE-BEKKN is initial.
          read table T_9 with key     EBELN = T_EKBE-EBELN
                                      EBELP = T_EKBE-EBELP
                                      LFBNR = T_EKBE-LFBNR binary search.
          if SY-SUBRC = 0.
            read table T_1 with key     EBELN = T_EKBE-EBELN
                                        EBELP = T_EKBE-EBELP
                                        ZEKKN = T_EKBE-ZEKKN
                                        LFBNR = T_EKBE-LFBNR binary search.
            if SY-SUBRC = 0.
              read table T_EKKN2 with key EBELN = T_1-EBELN
                                          EBELP = T_1-EBELP
                                          ZEKKN = T_1-BEKKN
                                         binary search.
              T_EKBE-AUFNR = T_EKKN2-AUFNR.
              T_EKBE-ANLN1 = T_EKKN2-ANLN1.
              T_EKBE-DMBTR = T_EKBE-DMBTR.
            endif.
          endif.
        else.
          select single AUFNR ANLN1 from EKKN
            into (T_EKBE-AUFNR, T_EKBE-ANLN1)
            where EBELN = T_EKBE-EBELN and
                  EBELP = T_EKBE-EBELP and
                  ZEKKN = T_EKBE-BUZEI.
        endif.
      endif.

    endif.
    concatenate T_EKBE-BELNR T_EKBE-GJAHR
       into T_EKBE-AWKEY.
    modify T_EKBE.
  endloop.
  W_EKBE[] = T_EKBE[].

  sort T_EKBE[] by EBELN EBELP BELNR VGABE ascending.

*  DELETE w_ekbe WHERE lfbnr IS INITIAL.
  loop at W_EKBE where VGABE eq '2'.
    if W_EKBE-BELNR = W_EKBE-LFBNR.
      continue.
    endif.
***    IF w_ekbe-vgabe EQ '2'.
    read table T_EKBE assigning field-symbol(<LFS_EKBE>)
                      with key EBELN = W_EKBE-EBELN
                               EBELP = W_EKBE-EBELP
                               BELNR = W_EKBE-LFBNR
                               VGABE = '1' binary search.
    if SY-SUBRC = 0.
**      t_ekbe-lfbnr = w_ekbe-belnr.
**      MODIFY t_ekbe INDEX sy-tabix.
**      DELETE w_ekbe.
**      MOVE-CORRESPONDING w_ekbe TO e_ekbe.
**      APPEND e_ekbe. CLEAR e_ekbe.
      <LFS_EKBE>-LFBNR = W_EKBE-BELNR.
      append W_EKBE to E_EKBE.
    endif.
**  ENDIF.
  endloop.

*  DELETE t_ekbe WHERE vgabe EQ '2'.

  if not T_EKBE[] is initial.
    select BUKRS BELNR GJAHR AWKEY KURS2 BLART from BKPF into table WE_BKPF
      for all entries in T_EKBE
      where AWTYP in ('MKPF', 'RMRP') and
            AWKEY eq T_EKBE-AWKEY     and
            BUKRS in S_BUKRS      and
            GJAHR eq T_EKBE-GJAHR.

    sort WE_BKPF by AWKEY.
  endif.

  if WE_BKPF[] is not initial.
    select BUKRS BELNR GJAHR DMBTR DMBE2
      from BSAK
      into table T_TAXA
       for all entries in WE_BKPF
        where BELNR eq WE_BKPF-BELNR
          and BUKRS eq WE_BKPF-BUKRS
          and GJAHR eq WE_BKPF-GJAHR.

    select BUKRS BELNR GJAHR DMBTR DMBE2
      from BSIK
      appending table T_TAXA
       for all entries in WE_BKPF
        where BELNR eq WE_BKPF-BELNR
          and BUKRS eq WE_BKPF-BUKRS
          and GJAHR eq WE_BKPF-GJAHR.
  endif.


  if not T_EKBE[] is initial.
    select BELNR GJAHR XBLNR
      from RBKP into table T_RBKP_1
      for all entries in T_EKBE
      where BELNR = T_EKBE-BELNR and
            GJAHR = T_EKBE-GJAHR.
    sort T_RBKP_1 by BELNR GJAHR.
  endif.

**  IF t_ekko[] IS INITIAL.
*****    STOP.
**  ENDIF.

  T_IMAKA_SORT[] = T_IMAKA[].
  T_IMAKZ_SORT[] = T_IMAKZ[].
  data(T_EKKO_1) = T_EKKO[].
  data(T_EKKO_2) = T_EKKO[].

  sort T_EKKO_1[] by ANLN1 ANLN2 ascending.
  sort T_EKKO_2[] by AUFNR ascending.

  if not S_LIFRE[] is initial or
     not S_LIFNR[] is initial or
     not S_EBELN[] is initial.
    data: W_CLEAR(1).
    loop at T_IMAK.
      W_CLEAR = 'A'.
      loop at T_IMAKA_SORT assigning field-symbol(<LFS_IMAKA>)
                           where POSNR = T_IMAK-POSNR and
                                 BUKRS = T_IMAK-ABUKRS.

        read table T_EKKO_1 with key ANLN1 = <LFS_IMAKA>-ANLN1
                                     ANLN2 = <LFS_IMAKA>-ANLN2 binary search transporting no fields.
        if SY-SUBRC = 0.
          T_IMAK-DEL = 'X'.
        endif.
      endloop.
      check T_IMAK-DEL is initial.

      loop at T_IMAKZ_SORT assigning field-symbol(<LFS_IMAKZ>) where POSNR = T_IMAK-POSNR.
        loop at T_COAS_HASH assigning field-symbol(<LFS_COAS>) where AUFNR = <LFS_IMAKZ>-AUFNR.
          read table T_EKKO_2 with key AUFNR = <LFS_COAS>-AUFNR binary search transporting no fields.
          if SY-SUBRC = 0.
            T_IMAK-DEL = 'X'.
          endif.
        endloop.
      endloop.
      if T_IMAK-DEL is initial.
        delete T_IMAK.
      endif.
    endloop.
  endif.

  free: T_EKKO_1[], T_EKKO_2[], T_IMAKZ_SORT[], T_COAS_HASH[].

*  FIELD-SYMBOLS: <FS_TESTE2> TYPE ANY.

*
*  DATA: DREF_TAB %_PREDEFINED.
*
*  get reference of <fs_teste> into <FS_TESTE2>.

  perform F_MSG using 'Selecionando dados FI'
                      '22'.
  perform F_BUSCA_DADOS_FI.

  perform F_MSG using 'Selecionando dados cotação'
                      '28'.
  perform F_BUSCA_COTACAO.

  T_EKKO_AUX[] = T_EKKO[].
  delete T_EKKO_AUX where VPROZ = 0.

*  IF NOT t_ekko_aux[] IS INITIAL.
  if not T_EKKO[] is initial.
*    SELECT ebeln ebelp zekkn "vgabe gjahr belnr buzei
*           menge netwr aufnr anln1 anln2 vproz
*           FROM ekkn INTO TABLE t_ekkn
*      FOR ALL ENTRIES IN t_ekko_aux
*      WHERE ebeln EQ t_ekko_aux-ebeln AND
*            ebelp EQ t_ekko_aux-ebelp."vgabe IN ('1', '2', '3').

*else.
    select EBELN EBELP ZEKKN "vgabe gjahr belnr buzei
         MENGE NETWR AUFNR ANLN1 ANLN2 VPROZ
         from EKKN into table T_EKKN
    for all entries in T_EKKO
    where EBELN eq T_EKKO-EBELN and
          EBELP eq T_EKKO-EBELP."vgabe IN ('1', '2', '3').
*
  endif.

  if T_EKKO[] is initial.
*    STOP.
  endif.

  perform F_MSG using 'Selecionando dados caixa'
                    '33'.
* Seleciona registros para modo CAIXA
*  IF NOT p_sincax IS INITIAL.
  perform F_SELECIONA_CAIXA.
*  ENDIF.


  perform F_MSG using 'Selecionando dados competencia'
                      '35'.
  perform F_SELECIONA_COMPETENCIA.

  perform F_MSG using 'Ajustando valores'
                      '38'.
  if not T_EKKO[] is initial.
    perform F_AJUSTA_VALORES.
  endif.

  if not T_IMAKZ[] is initial.
    perform F_MSG using 'Selecionando Ciclo de Rateio'
                        '40'.

*    IF p_sincax IS INITIAL.
    perform F_SELECIONA_CILCO_RATEIO.
*    ENDIF.
  endif.
  T_BSIK_1[] = T_BSIK[].
  T_BSIS_1[] = T_BSIS_DP[].


  T_EKBE_2[] = T_EKBE[].
  delete T_EKBE_2 where VGABE ne 2.
  if not T_EKBE_2[] is initial.
    select RBKP~BELNR RBKP~GJAHR RBKP~XBLNR RBKP~LIFNR
           LFA1~NAME1
      from RBKP inner join LFA1 on
      RBKP~LIFNR = LFA1~LIFNR
      into table T_RBKP
      for all entries in T_EKBE_2
      where BELNR = T_EKBE_2-BELNR and
            GJAHR = T_EKBE_2-GJAHR.

    sort T_RBKP by BELNR GJAHR.
  endif.

  select * from T161T into table T_161
    where SPRAS = SY-LANGU.

endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0100 output.
  set titlebar '100'.
  perform COTACAO.

  if P_OFF is initial.
    loop at screen.

      case SCREEN-NAME.
        when 'EXECOFF' or 'USER' or 'S_USER-LOW' or 'HORA' or 'S_HORA-LOW' or 'DATA' or 'S_DATA-LOW'.
          SCREEN-ACTIVE = 0.
*          screen-DISPLAY = 0.
          modify screen.
        when others.
      endcase.

    endloop.
  endif.

  if P_CONSOL eq 'X'.
    set pf-status '100'.
  elseif P_SINCAX eq 'X'.
    set pf-status '110'.
  else.
    set pf-status '120'.
  endif.
  if TREE1 is initial.
    perform INIT_TREE.
  endif.
  call method CL_GUI_CFW=>FLUSH.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       Inicia criação da arvore
*----------------------------------------------------------------------*
form INIT_TREE .

  clear GT_FIELDCATALOG.
  refresh GT_FIELDCATALOG.

  data: EVENT  type CNTL_SIMPLE_EVENT,
        EVENTS type CNTL_SIMPLE_EVENTS.


*  IF SY-UCOMM EQ 'IMPO'.
*    PERFORM CRIA_TABELA_IMPOSTOS.
*  ENDIF.
* get fieldcatalog
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      I_STRUCTURE_NAME   = 'ZES_IM01'
      I_BYPASSING_BUFFER = 'X'
    changing
      CT_FIELDCAT        = GT_FIELDCATALOG.

  if not P_CONSOL is initial.
    if W_GROUP is initial.
      perform F_MODIFICA_LAYOUT_TELA.
    else.
      perform BUILD_FIELDCATALOG.
    endif.
  elseif not P_SINCAX is initial.

    if not W_ANAL is initial.
      clear W_ANAL.
      perform F_MODIFICA_LAYOUT_CX3.
    elseif not W_IMPO is initial.
      clear: W_IMPO.
      perform F_MODIFICA_LAYOUT_CX4.
    else.
      if W_GROUP is initial.
        perform F_MODIFICA_LAYOUT_CX1.
      else.
        perform F_MODIFICA_LAYOUT_CX2.
      endif.
    endif.
  else.
    perform MODIFICA_LAYOUT_ANALIT.
  endif.

  V_REPID = SY-REPID.
* create fieldcatalog for structure sflight

  if SY-TCODE ne 'ZIM08' and SY-BATCH ne 'X'.

    create object CONTAINER1
      exporting
        REPID = V_REPID
        DYNNR = '0100'
        SIDE  = CONTAINER1->DOCK_AT_BOTTOM
        RATIO = 80.

    if SY-SUBRC <> 0.
      message X208(00) with 'ERROR'.                        "#EC NOTEXT
    endif.
  endif.

* create tree control

  create object TREE1
    exporting
      PARENT                      = CONTAINER1
      NODE_SELECTION_MODE         = CL_GUI_LIST_TREE=>NODE_SEL_MODE_SINGLE
      NO_HTML_HEADER              = 'X'
      NO_TOOLBAR                  = ''
    exceptions
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      LIFETIME_ERROR              = 5.

* create Hierarchy-header
  data L_HIERARCHY_HEADER type TREEV_HHDR.
  perform BUILD_HIERARCHY_HEADER changing L_HIERARCHY_HEADER.


* repid for saving variants
  data: LS_VARIANT type DISVARIANT.
  LS_VARIANT-REPORT = SY-REPID.
  S_LAYOUT-CTAB_FNAME = 'COLOR'.
* create emty tree-control
  call method TREE1->SET_TABLE_FOR_FIRST_DISPLAY
    exporting
*     IS_LAYOUT           = s_layout
      IS_HIERARCHY_HEADER = L_HIERARCHY_HEADER
      I_SAVE              = 'A'
      IS_VARIANT          = LS_VARIANT
    changing
      IT_OUTTAB           = T_SAIDA[]
      IT_FIELDCATALOG     = GT_FIELDCATALOG.

  " link click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  append EVENT to EVENTS.

  " expand no children
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  append EVENT to EVENTS.

  " expand no children
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  append EVENT to EVENTS.

  call method TREE1->SET_REGISTERED_EVENTS
    exporting
      EVENTS                    = EVENTS
    exceptions
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  if SY-SUBRC <> 0.
*    MESSAGE a000.
  endif.

  if P_SINCAX is not initial.
    perform CREATE_HIERARCHY_2.
*    PERFORM create_hierarchy_3.
  else.
* create hierarchy
    perform CREATE_HIERARCHY.

  endif.

* add own functioncodes to the toolbar
*  perform change_toolbar.

  set handler: LCL_EVENT_HANDLER=>HANDLE_LINK_CLICK         for TREE1.
  set handler  LCL_EVENT_HANDLER=>HANDLE_EXPAND_NO_CHILDREN for TREE1.
  set handler:  LCL_EVENT_HANDLER=>HANDLE_ITEM_DOUBLE_CLICK   for TREE1.
* SET HANDLER  lcl_event_handler=>handle_expand_no_children FOR tree1. SET HANDLER:
*               lcl_event_handler=>item_keypress FOR tree1.
*
*  SET HANDLER:
*           lcl_event_handler=>node_double_click FOR tree1.
*    SET HANDLER:
*                 lcl_event_handler=>on_double_click FOR tree1.


endform.                    " INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Campos da arvore
*----------------------------------------------------------------------*
form BUILD_FIELDCATALOG .

** change fieldcatalog
  data: LS_FIELDCATALOG type LVC_S_FCAT.
*        w_pos TYPE i.
*  CLEAR w_pos.
  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.
    case LS_FIELDCATALOG-FIELDNAME.

      when 'NETWR' .
        LS_FIELDCATALOG-COL_POS = 1.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido'.
      when 'DMBTR'.
        LS_FIELDCATALOG-COL_POS = 2.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
        LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
        LS_FIELDCATALOG-COLTEXT = 'Realizado'.
*        ls_fieldcatalog-col_pos   = 18.
      when 'DIFERENCA'.
        LS_FIELDCATALOG-COL_POS = 3.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido (-) Realizado'.
*        ls_fieldcatalog-col_pos   = 18.

      when 'TXT050'.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Denominação do Objeto'.
      when 'EBELN'.
        LS_FIELDCATALOG-COL_POS = 4.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'EBELP'.
        LS_FIELDCATALOG-COL_POS = 5.
        LS_FIELDCATALOG-OUTPUTLEN = 8.
      when 'ZWAERS'.
        LS_FIELDCATALOG-COL_POS = 7.
        LS_FIELDCATALOG-OUTPUTLEN = 8.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Moeda'.
*********
      when 'TIPOPED'.
        LS_FIELDCATALOG-COL_POS = 6.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tp.Pedido '.
      when 'AEDAT'.
        LS_FIELDCATALOG-COL_POS = 8.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Data'.

      when 'KONNR'.
        LS_FIELDCATALOG-COL_POS = 9.
        LS_FIELDCATALOG-OUTPUTLEN = 10.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Contrato'.
      when 'KTPNR'.
        LS_FIELDCATALOG-COL_POS = 10.
        LS_FIELDCATALOG-OUTPUTLEN = 5.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Item Contrato'.
      when 'BELNR2'.
        LS_FIELDCATALOG-COL_POS = 11.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tipo Doc.'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
      when 'BELNR1' .
        LS_FIELDCATALOG-COL_POS = 12.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Docum. (MM)'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'BELNR3' .
        LS_FIELDCATALOG-COL_POS = 13.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Doc.Contábil'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.


      when 'OBJETO' .
        LS_FIELDCATALOG-COL_POS = 14.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Objeto'.
        LS_FIELDCATALOG-OUTPUTLEN = 30.
*        ls_fieldcatalog-hotspot = 'X'.

      when 'BELNR4' .
        LS_FIELDCATALOG-COL_POS = 15.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Centro de Custo'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
      when 'NFENUM' .
        LS_FIELDCATALOG-COL_POS = 15.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Nº NF'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
      when 'NETPR' .
        LS_FIELDCATALOG-COL_POS = 16.
*        ls_fieldcatalog-do_sum = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Valor do Documento'.

*** colocar campo de valor.
      when 'MATNR' . LS_FIELDCATALOG-COL_POS = 17.
      when 'TXZ01' . LS_FIELDCATALOG-COL_POS = 18.
      when 'SRVPOS' . LS_FIELDCATALOG-COL_POS = 19.
      when 'KTEXT1'.
        LS_FIELDCATALOG-COL_POS = 20.
        LS_FIELDCATALOG-SCRTEXT_S = 'Descrição'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Texto Breve (Desc.Serviço)'.
      when 'EXTROW'.
        LS_FIELDCATALOG-COL_POS = 21.
        LS_FIELDCATALOG-SCRTEXT_S = 'Linha Serviço'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Linha (Serviço)'.
      when 'MENTE' . LS_FIELDCATALOG-COL_POS = 22.
      when 'MEINS'.
        LS_FIELDCATALOG-COL_POS = 24.
        LS_FIELDCATALOG-OUTPUTLEN = 21.
      when 'FORNECEDOR'.
        LS_FIELDCATALOG-COL_POS = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Fornecedor'.
      when 'EMISSOR'.
        LS_FIELDCATALOG-COL_POS = 26.
        LS_FIELDCATALOG-SCRTEXT_S = 'Emissor'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Emissor da fatura'.
      when 'SGTXT'.
        LS_FIELDCATALOG-COL_POS = 27.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Texto do item'.
      when others.
        LS_FIELDCATALOG-NO_OUT = 'X'.
        LS_FIELDCATALOG-KEY    = ''.
    endcase.
    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.

endform.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  case SY-UCOMM.

    when 'BACK'.
      call method CONTAINER1->FREE.
      leave to screen 0.
    when 'GROUP'.
      call method CONTAINER1->FREE.
      clear TREE1.
      clear TW_SAIDA. refresh TW_SAIDA.

      if W_GROUP is initial.
        W_GROUP = 'X'.
      else.
        clear W_GROUP.
      endif.

    when 'PGMT'.
      call method CONTAINER1->FREE.
      clear TREE1.
      clear TW_SAIDA. refresh TW_SAIDA.

      if W_PGMT is initial.
        W_PGMT = 'X'.
      else.
        clear W_PGMT.
      endif.
    when 'ANAL'.
      if W_ANAL is initial.
        call method CONTAINER1->FREE.
        clear TREE1.
        clear TW_SAIDA. refresh TW_SAIDA.
        W_ANAL = 'X'.
      else.
        clear W_ANAL.
      endif.
    when 'IMPO'.
      if W_IMPO is initial.
        call method CONTAINER1->FREE.
        clear TREE1.
        clear TW_SAIDA. refresh TW_SAIDA.
        W_IMPO = 'X'.
      else.
        clear W_IMPO.
      endif.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
form BUILD_HIERARCHY_HEADER changing
                               P_HIERARCHY_HEADER type TREEV_HHDR.

  P_HIERARCHY_HEADER-HEADING = 'Investimentos'.             "#EC NOTEXT
  P_HIERARCHY_HEADER-TOOLTIP = 'Investimentos !'.           "#EC NOTEXT
  P_HIERARCHY_HEADER-WIDTH = 74.
  P_HIERARCHY_HEADER-WIDTH_PIX = 'X'.

endform.                               " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
form CREATE_HIERARCHY .
  types: begin of TYL_BKPF,
           BUKRS type BKPF-BUKRS,
           BELNR type BKPF-BELNR,
           GJAHR type BKPF-GJAHR,
           KURS2 type BKPF-KURS2,
         end of TYL_BKPF.

  data TL_BKPF type table of TYL_BKPF with header line.


  clear: T_08, TL_BKPF.
  refresh: T_08, TL_BKPF.

  data: WL_CRIA_FI,
        WL_CH type LVC_NKEY.

  clear: WL_CRIA_FI, WL_CH.

  clear: TW_SAIDA. refresh TW_SAIDA.

  data: WL_EBELN type E_SAIDA-EBELN,
        WL_POSNR type E_SAIDA-POSNR.

  data: begin of WE_SAIDA occurs 0.
          include structure ZES_IM01.
  data:   COLOR type   KKBLO_SPECIALCOL occurs 0.
  data: end of WE_SAIDA.


  data: "w_saida TYPE zes_im01,
    W_SAIDA        type E_SAIDA,
    W_TABIX        type SY-TABIX,
    W_TIPO(1),
    W_MUDA(1),
    W_PST(1),
    W_FI(1),
    W_PED          type LVC_NKEY,
    W_EBELN        type EKKO-EBELN,
* add data to tree
    L_POSNR_KEY    type LVC_NKEY,
    L_BUKRS_KEY    type LVC_NKEY,
    L_TXTTIPO_KEY  type LVC_NKEY,
    L_ANLN1_KEY    type LVC_NKEY,
    L_EBELN_KEY    type LVC_NKEY,
    L_PEDIDO_KEY   type LVC_NKEY,
    L_LAST_KEY     type LVC_NKEY,
    TL_ZIMT001     type table of ZIMT001 with header line,
    TL_ZIMT001_AUX type table of ZIMT001 with header line.


  if not P_ON is initial.
    perform F_PREPARA_DADOS.

    do.
      read table TW_SAIDA with key SGTXT = 'APAGAR'.
      if SY-SUBRC <> 0.
        exit.
      endif.
      clear TW_SAIDA-SGTXT.
      modify TW_SAIDA index SY-TABIX.
      WL_EBELN = TW_SAIDA-EBELN.
      WL_POSNR = TW_SAIDA-POSNR.
      loop at TW_SAIDA where EBELN = WL_EBELN and
                             POSNR = WL_POSNR and
                             ( BELNR2 <> 'PEDIDO' and
                               BELNR2 <> 'MIGO'       ).
        read table TW_SAIDA into W_SAIDA with key EBELN = WL_EBELN
                                                  POSNR = WL_POSNR
                                                  BELNR2 = 'PEDIDO'.
        if SY-SUBRC ne 0.
          clear TW_SAIDA-SGTXT.
        else.
          TW_SAIDA-SGTXT = W_SAIDA-SGTXT.
        endif.
        modify TW_SAIDA.
      endloop.
    enddo.


  else.
    perform F_SELECIONA_DADOS_OFFLINE.
    loop at T_08.
      move-corresponding T_08 to TW_SAIDA.
      append TW_SAIDA.
    endloop.

    perform ORGANIZA_TEXTO.

  endif.
  if not P_SINCAX is initial.
    delete TW_SAIDA where BELNR2 = 'MIGO'.
  endif.



  data: W_POSNR     like TW_SAIDA-POSNR,
        W_TXTTIPO   like TW_SAIDA-TXTTIPO,
        W_ANLN1(20),"   LIKE tw_saida-anln1,
        W_ANLN0(20),
        W_EB(1).

  clear: W_REALIZADO, W_PEDIDO, W_POSNR.

  loop at TW_SAIDA where TXT50 = 'Orçamento'.
    TW_SAIDA-AEDAT = S_BUDAT-LOW.
    modify TW_SAIDA.
  endloop.


  if P_SINCAX is initial.
    delete TW_SAIDA where not AEDAT in S_BUDAT and
                            ( BELNR2 ne 'PEDIDO' and
                              BELNR2 ne SPACE        ).
  else.
    delete TW_SAIDA where not BUDAT in S_BUDAT and
                            ( BELNR2 ne 'PEDIDO' and
                              BELNR2 ne SPACE    and
                              BELNR2 ne 'FIPA'   and
                              BELNR2 ne 'FIAP'   ) ."AND
*                              belnr2(4) NE 'MIRO'     ).
  endif.
  loop at TW_SAIDA where TXT50 = 'Orçamento'.
    clear: TW_SAIDA-AEDAT, TW_SAIDA-DMBTR.
    modify TW_SAIDA.
  endloop.

  if P_CONSOL eq 'X'.
*    DELETE ADJACENT DUPLICATES FROM tw_saida COMPARING ALL FIELDS.
  endif.


  if P_OFF is initial.
*---> 04/07/2023 - Migração S4 - WS
  SORT TW_SAIDA.
*<--- 04/07/2023 - Migração S4 - WS
    delete adjacent duplicates from TW_SAIDA comparing all fields.
  endif.

  select *
    from ZIMT001
    into table TL_ZIMT001
      where VISAO = '1'
        and ABUKRS in S_BUKRS
        and WERKS  in S_WERKS
        and BELNR4 in S_KOSTL
        and POSNR  in S_POSNR
        and ANLN1  in S_ANLN1
        and AUFNR  in S_AUFNR
        and EBELN  in S_EBELN
*        and aedat  in s_budat
        and ANULAR eq SPACE.

  delete TL_ZIMT001 where BELNR2 ne 'PEDIDO'
                      and STATUS ne 'E'
                      and AEDAT not in S_BUDAT.
*  endloop.

  TL_ZIMT001_AUX[] = TL_ZIMT001[].
  delete TL_ZIMT001_AUX where STATUS ne 'E'
                          and STATUS ne 'A'.

  loop at TL_ZIMT001_AUX.
    delete TW_SAIDA where POSNR   eq TL_ZIMT001_AUX-POSNR
                      and ABUKRS  eq TL_ZIMT001_AUX-ABUKRS
                      and EBELN   eq TL_ZIMT001_AUX-EBELN
                      and EBELP   eq TL_ZIMT001_AUX-EBELP
                      and AUFNR   eq TL_ZIMT001_AUX-AUFNR
                      and ANLN1   eq TL_ZIMT001_AUX-ANLN1
                      and ANLN2   eq TL_ZIMT001_AUX-ANLN2
                      and BELNR1  eq TL_ZIMT001_AUX-DOC_MM
                      and BELNR2(4) eq TL_ZIMT001_AUX-BELNR2(4).
  endloop.

  TL_ZIMT001_AUX[] = TL_ZIMT001[].
  delete TL_ZIMT001_AUX where STATUS ne 'I'
                          and STATUS ne 'A'.

  if TL_ZIMT001_AUX[] is not initial
  and P_MOEDA ne 'BRL'.
    select BUKRS BELNR GJAHR KURS2
      from BKPF
      into table TL_BKPF
       for all entries in TL_ZIMT001_AUX
       where BUKRS eq TL_ZIMT001_AUX-ABUKRS
         and BELNR eq TL_ZIMT001_AUX-BELNR6
         and GJAHR eq TL_ZIMT001_AUX-AEDAT(4).
  endif.

  data: WL_TABIX_AUX type SY-TABIX.
  clear: TW_SAIDA.
  sort: TL_BKPF by BUKRS BELNR GJAHR.
  loop at TL_ZIMT001_AUX.
    clear: W_TABIX.
    move-corresponding: TL_ZIMT001_AUX to TW_SAIDA.
    if P_MOEDA ne 'BRL'.
      if TL_ZIMT001_AUX-DMBE2 is not initial.
        if TW_SAIDA-DMBTR is not initial.
          move: TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-DMBTR.
        elseif TW_SAIDA-DIFERENCA is not initial.
          move: TL_ZIMT001_AUX-DIFERENCA to TW_SAIDA-DMBTR.
        endif.
      else.
        read table TL_BKPF
              with key BUKRS = TW_SAIDA-ABUKRS
                       BELNR = TW_SAIDA-BELNR6
                       GJAHR = TW_SAIDA-AEDAT(4)
                       binary search.

        if SY-SUBRC is initial.
          divide TW_SAIDA-DMBTR by TL_BKPF-KURS2.
          divide TW_SAIDA-DIFERENCA by TL_BKPF-KURS2.
          multiply TW_SAIDA-DMBTR by -1.
          multiply TW_SAIDA-DIFERENCA by -1.
        endif.
      endif.
    endif.
    move: TL_ZIMT001_AUX-DOC_MM to TW_SAIDA-BELNR1.
*    READ TABLE tw_saida TRANSPORTING NO FIELDS
*          WITH KEY abukrs  = tw_saida-abukrs
*                   posnr   = tw_saida-posnr
*                   txttipo = tw_saida-txttipo
*                   anln1   = tw_saida-anln1
*                   aufnr   = tw_saida-aufnr
*                   txt050  = tw_saida-txt050
*                   ebeln   = tw_saida-ebeln
*                   ebelp   = tw_saida-ebelp
*                   belnr2(4)  = tw_saida-belnr2(4).
*
*
*    IF sy-subrc IS NOT INITIAL.
    read table TW_SAIDA transporting no fields
          with key ABUKRS  = TW_SAIDA-ABUKRS
                   POSNR   = TW_SAIDA-POSNR
                   TXTTIPO = TW_SAIDA-TXTTIPO
                   ANLN1   = TW_SAIDA-ANLN1
                   AUFNR   = TW_SAIDA-AUFNR
                   TXT050  = TW_SAIDA-TXT050
                   EBELN   = TW_SAIDA-EBELN
                   EBELP   = TW_SAIDA-EBELP
                   BELNR2(4)  = TW_SAIDA-BELNR2(4).


    if SY-SUBRC is not initial.
      read table TW_SAIDA transporting no fields
        with key ABUKRS  = TW_SAIDA-ABUKRS
                 POSNR   = TW_SAIDA-POSNR
                 TXTTIPO = TW_SAIDA-TXTTIPO
                 ANLN1   = TW_SAIDA-ANLN1
                 AUFNR   = TW_SAIDA-AUFNR
                 TXT050  = TW_SAIDA-TXT050
                 EBELN   = TW_SAIDA-EBELN
                 EBELP   = TW_SAIDA-EBELP.
*                 belnr2(4)  = tw_saida-belnr2(4).


      if SY-SUBRC is not initial.
        read table TW_SAIDA transporting no fields
       with key ABUKRS  = TW_SAIDA-ABUKRS
                POSNR   = TW_SAIDA-POSNR
                TXTTIPO = TW_SAIDA-TXTTIPO
                ANLN1   = TW_SAIDA-ANLN1
                AUFNR   = TW_SAIDA-AUFNR
                TXT050  = TW_SAIDA-TXT050
                EBELN   = TW_SAIDA-EBELN.
*             ebelp   = tw_saida-ebelp.
        if SY-SUBRC is not initial.
          read table TW_SAIDA transporting no fields
            with key ABUKRS  = TW_SAIDA-ABUKRS
          POSNR   = TW_SAIDA-POSNR
          TXTTIPO = TW_SAIDA-TXTTIPO
          ANLN1   = TW_SAIDA-ANLN1
          AUFNR   = TW_SAIDA-AUFNR
          TXT050  = TW_SAIDA-TXT050.
*        ebeln   = tw_saida-ebeln.

          if SY-SUBRC is not initial.
            read table TW_SAIDA transporting no fields
             with key ABUKRS  = TW_SAIDA-ABUKRS
           POSNR   = TW_SAIDA-POSNR
           TXTTIPO = TW_SAIDA-TXTTIPO
           ANLN1   = TW_SAIDA-ANLN1
           AUFNR   = TW_SAIDA-AUFNR.
*         txt050  = tw_saida-txt050.
            if SY-SUBRC is not initial.
              read table TW_SAIDA transporting no fields
               with key ABUKRS  = TW_SAIDA-ABUKRS
             POSNR   = TW_SAIDA-POSNR
             TXTTIPO = TW_SAIDA-TXTTIPO
             ANLN1   = TW_SAIDA-ANLN1.
*         aufnr   = tw_saida-aufnr.

              if SY-SUBRC is not initial.
                read table TW_SAIDA transporting no fields
               with key ABUKRS  = TW_SAIDA-ABUKRS
             POSNR   = TW_SAIDA-POSNR
             TXTTIPO = TW_SAIDA-TXTTIPO.
                if SY-SUBRC is not initial.
                  read table TW_SAIDA transporting no fields
                 with key ABUKRS  = TW_SAIDA-ABUKRS
                          POSNR   = TW_SAIDA-POSNR.
                  if SY-SUBRC is not initial.
                    read table TW_SAIDA transporting no fields
                     with key ABUKRS  = TW_SAIDA-ABUKRS.
                    if SY-SUBRC is not initial.
                      append TW_SAIDA.
                      clear: TW_SAIDA.
                      continue.
                    endif.
                  else.
                    WL_TABIX_AUX = SY-TABIX + 1.
                  endif.
                else.
                  WL_TABIX_AUX = SY-TABIX + 1.
                endif.
              else.
                WL_TABIX_AUX = SY-TABIX + 1.
              endif.
            else.
              WL_TABIX_AUX = SY-TABIX + 1.
            endif.
          else.
            WL_TABIX_AUX = SY-TABIX + 1.
          endif.
        else.
          WL_TABIX_AUX = SY-TABIX + 1.
        endif.
      else.
        WL_TABIX_AUX = SY-TABIX + 1.
      endif.
    else.
      WL_TABIX_AUX = SY-TABIX + 1.
    endif.

    if WL_TABIX_AUX is initial.
      add 1 to WL_TABIX_AUX.
    endif.

    insert TW_SAIDA index  WL_TABIX_AUX.
    clear: TW_SAIDA.
  endloop.

  data: WL_TABIX      type C,
        WL_TABIX_AUX2 type SY-TABIX.

*---> 06/07/2023 - Migração S4 - WS
  SORT tw_saida BY txt50 posnr belnr2 ebeln.
*<--- 06/07/2023 - Migração S4 - WS
  loop at TW_SAIDA into W_SAIDA.
    WL_TABIX_AUX2 = SY-TABIX.
** Eduardo - Calculo
    W_SAIDA-REAL_AP = W_SAIDA-DMBTR - W_SAIDA-APROVADO. "#EC CI_FLDEXT_OK[2610650]
** Eduardo - Calculo

    if W_SAIDA-BELNR2(6) = 'MIRO-0'.
      W_SAIDA-BELNR2 = 'MIRO'.
      if W_SAIDA-NETWR is initial and
         W_SAIDA-SGTXT ne 'ZIM' .
        W_SAIDA-DMBTR = W_SAIDA-NETPR .
      endif.
    endif.


*    if w_saida-belnr2 = 'MIRO-NC'.
*      if w_saida-netwr is initial and
*         w_saida-sgtxt ne 'ZIM' .
*        w_saida-dmbtr = w_saida-netpr * ( - 1 ).
*      endif.
*    endif.


    if W_SAIDA-BELNR2 is initial.
      clear: W_SAIDA-EBELN, W_SAIDA-EBELP.
    endif.

    if W_SAIDA-BELNR2 = 'PEDIDO'.
      clear: W_SAIDA-BELNR3, W_SAIDA-DMBTR, W_SAIDA-NFENUM.
    endif.

    if W_SAIDA-BELNR2 = 'MIRO'.
      clear W_SAIDA-DIFERENCA.
    endif.

    W_SAIDA-DIFERENCA = W_SAIDA-NETWR - W_SAIDA-DMBTR.

    if W_SAIDA-BELNR2 = 'MIGO-DEV'.
      clear: W_SAIDA-NETWR, W_SAIDA-DIFERENCA, W_SAIDA-DMBTR.
    endif.


    read table TL_ZIMT001 transporting no fields
          with key ABUKRS  = W_SAIDA-ABUKRS
                   POSNR   = W_SAIDA-POSNR
                   TXTTIPO = W_SAIDA-TXTTIPO
                   AUFNR   = W_SAIDA-AUFNR
                   TXT050  = W_SAIDA-TXT050
                   EBELN   = W_SAIDA-EBELN
                   EBELP   = W_SAIDA-EBELP
                   BELNR2(4)  = W_SAIDA-BELNR2(4).


    if SY-SUBRC is not initial.
      if W_SAIDA-BELNR2 = 'MIGO'.
        clear W_SAIDA-DIFERENCA.
      endif.

      if W_SAIDA-BELNR2 = 'MIGO' and ( W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
        clear W_SAIDA-NETWR.
        W_SAIDA-DMBTR = W_SAIDA-NETPR.
        W_SAIDA-DIFERENCA = W_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
      elseif W_SAIDA-BELNR2 = 'MIGO'.
        clear: W_SAIDA-DMBTR, W_SAIDA-NETWR.
      endif.

    endif.
    SY-TABIX = WL_TABIX_AUX2.

    read table TL_ZIMT001 transporting no fields
          with key ABUKRS  = W_SAIDA-ABUKRS
                   POSNR   = W_SAIDA-POSNR
                   TXTTIPO = W_SAIDA-TXTTIPO
                   AUFNR   = W_SAIDA-AUFNR
                   TXT050  = W_SAIDA-TXT050
                   EBELN   = W_SAIDA-EBELN
                   EBELP   = W_SAIDA-EBELP
                   BELNR2(4)  = W_SAIDA-BELNR2(4).

    if SY-SUBRC is not initial.
      if W_SAIDA-BELNR2 = 'MIGO-EST'.

        clear: W_SAIDA-DIFERENCA, W_SAIDA-NETWR, W_SAIDA-DMBTR.
      endif.
    endif.
    SY-TABIX = WL_TABIX_AUX2.

    if W_SAIDA-BELNR2 = 'FIPA' or W_SAIDA-BELNR2 = 'FIAP'.
      clear: W_SAIDA-DMBTR.
    endif.

    W_SAIDA-ZWAERS = W_SAIDA-WAERS.

    if W_SAIDA-BELNR2 = 'MIRO' and ( W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
      clear: W_SAIDA-DMBTR, W_SAIDA-DIFERENCA.
    endif.
    if SY-TCODE eq 'ZIM08' or SY-BATCH eq 'X'.
      T_08-NUMERADOR = SY-TABIX.
      move-corresponding W_SAIDA to T_08.
      if W_SAIDA-AEDAT is initial.
        T_08-GJAHR = W_SAIDA-BUDAT.
      else.
        T_08-GJAHR = W_SAIDA-AEDAT.
      endif.
      move: SY-UNAME to  T_08-USUARIO,
            SY-DATUM to T_08-DATA_EXEC,
            WL_HORA  to T_08-HORA_EXEC.
      case 'X'.

        when P_CONSOL.
          T_08-VISAO = '01'.
        when P_ANALIT.
          T_08-VISAO = '02'.
        when P_SINCAX .
          T_08-VISAO = '03'.
      endcase.

*      APPEND t_08.
      collect T_08.
      clear T_08.
      continue.
    endif.


    if W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI'.
      clear W_SAIDA-SGTXT.
    endif.
*    MOVE-CORRESPONDING we_saida to w_saida.
    clear W_MUDA.
    if SY-UCOMM = 'GROUP'.
      W_TABIX = SY-TABIX.
      W_PASTA = 1.
    else.
      W_TABIX = 0.
    endif.
    add W_SAIDA-NETWR to W_PEDIDO.
    add W_SAIDA-DMBTR to W_REALIZADO . "#EC CI_FLDEXT_OK[2610650]

    if not W_SAIDA-AUFNR is initial.
      W_PASTA = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
    else.
      concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_PASTA.
      concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN0.
*      w_pasta = w_saida-anln1.
    endif.

    if W_SAIDA-BELNR2 = 'MB1A' or
       W_SAIDA-BELNR2 = 'MBST'.
      W_PASTA = W_SAIDA-EBELN + 1.
    endif.
    if not W_SAIDA-BELNR3 is initial and
           W_SAIDA-BELNR1 is initial.
      W_PASTA = W_SAIDA-EBELN + 1.
    endif.

    if  S_EBELN is not initial
    or S_LIFNR is not initial
    or S_KONNR is not initial
    or S_LIFRE is not initial.
      if ( W_SAIDA-BELNR2 eq 'MBST'
          or W_SAIDA-BELNR2 eq 'FB01'
          or W_SAIDA-BELNR2 eq 'MB1A'
          or W_SAIDA-BELNR2 eq 'FB08'
          or W_SAIDA-BELNR2 eq 'FB05'
          or W_SAIDA-BELNR2 eq 'FB50'
          or W_SAIDA-BELNR2 eq 'Ciclo Rate'
          or W_SAIDA-BELNR2 eq 'FBCJ'
          or W_SAIDA-BELNR2 eq 'FBR2'
          or W_SAIDA-BELNR2 eq 'FBA7'
          or W_SAIDA-BELNR2 eq 'FBZ2'
          or W_SAIDA-BELNR2 eq 'FBD5'
          or W_SAIDA-BELNR2 eq 'FBVB'
          or W_SAIDA-BELNR2 eq 'FB1K'
          or W_SAIDA-BELNR2 eq 'PRRW' ).
        continue.
      endif.
    endif.

*    IF w_saida-belnr2 = 'MIGO' OR w_saida-belnr1 = 'MIRO'.
*      w_pasta = w_saida-ebeln + 1.
*    ENDIF.

*    IF NOT w_saida-belnr3 IS INITIAL AND
*           w_saida-belnr2 NE 'MIRO'  .
*      IF w_saida-belnr2(2) = 'FI'.
*      ELSEIF w_saida-belnr2(2) = 'CI'.
*      ELSE.
*        CLEAR w_saida-ebeln.
*      ENDIF.
*    ENDIF.

    clear WL_TABIX.
    if SY-TABIX = 1.
      WL_TABIX = 'X'.
      perform ADD_BUKRS_LINE using    W_SAIDA
                                       ''
                              changing L_BUKRS_KEY.
    endif.

    if W_POSNR is initial.
*      w_tipo = 'X'.
      W_POSNR = W_SAIDA-POSNR.
      clear W_TXTTIPO.
      perform ADD_POSNR_LINE using    W_SAIDA
                                       L_BUKRS_KEY
                              changing L_POSNR_KEY.

      if W_SAIDA-TXTTIPO(5) eq 'OBRAS' or
         W_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( W_SAIDA-TXTTIPO(5) eq 'IMOBI' and not W_SAIDA-AUFNR is initial ) .
        W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
        W_ANLN0 = W_SAIDA-AUFNR.
        W_SAIDA-AUFNR = W_SAIDA-AUFNR.
      endif.

    elseif W_POSNR ne W_SAIDA-POSNR.
*      w_tipo = 'X'.
      W_POSNR = W_SAIDA-POSNR.
      clear: W_TXTTIPO, W_ANLN1.
      perform ADD_POSNR_LINE using    W_SAIDA
                                     L_BUKRS_KEY
                            changing L_POSNR_KEY.
    endif.

    if W_SAIDA-TXTTIPO(5) eq 'OBRAS' or
       W_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( W_SAIDA-TXTTIPO(5) eq 'IMOBI' and not W_SAIDA-AUFNR is initial ) ..
      W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
      W_SAIDA-AUFNR = W_SAIDA-AUFNR.
    endif.

    if W_TXTTIPO is initial.
      W_TXTTIPO = W_SAIDA-TXTTIPO.
      clear: W_ANLN1.
      perform ADD_TXTTIPO_LINE using   W_SAIDA
                                       L_POSNR_KEY
                              changing L_TXTTIPO_KEY.
    elseif W_TXTTIPO ne W_SAIDA-TXTTIPO.
      W_TXTTIPO = W_SAIDA-TXTTIPO.
      clear W_TIPO.
      perform ADD_TXTTIPO_LINE using   W_SAIDA
                                       L_POSNR_KEY
                              changing L_TXTTIPO_KEY.
    endif.

    if not W_TIPO is initial.
      clear W_TIPO.
      perform ADD_TXTTIPO_LINE using   W_SAIDA
                                       L_POSNR_KEY
                              changing L_TXTTIPO_KEY.
    endif.

    if  W_SAIDA-ANLN1 is initial.
      W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
    endif.

    if W_SAIDA-ANLN1 = 'PLANEJADO' or
      W_SAIDA-ANLN1 = 'EXTRA'.

      on change of W_SAIDA-TXT50 or W_SAIDA-POSNR.
        perform ADD_ANLN1_LINE using     W_SAIDA
                                         L_TXTTIPO_KEY
                                changing L_ANLN1_KEY.
      endon.

      perform ADD_COMPLETE_LINE using  W_SAIDA
                                       L_ANLN1_KEY
                              changing L_LAST_KEY.

      continue.
    endif.
    if W_ANLN1 is initial.
*      w_anln1 = w_saida-anln1.
      if W_SAIDA-AUFNR is initial.
        concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN1.
      else.
        W_ANLN1 = W_SAIDA-ANLN1.
      endif.
      W_FI = 'X'.
      W_PST = 'X'.
      WL_CRIA_FI = 'X'.
      if W_SAIDA-BELNR2 = 'PEDIDO'.
        WL_TABIX = 'X'.
      endif.
      perform ADD_ANLN1_LINE using     W_SAIDA
                                       L_TXTTIPO_KEY
                              changing L_ANLN1_KEY.
    elseif W_ANLN1 ne W_ANLN0. "w_saida-anln1.
*      w_anln1 = w_saida-anln1.
      if W_SAIDA-AUFNR is initial.
        concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN1.
      else.
        W_ANLN1 = W_SAIDA-ANLN1.
      endif.
      W_PST = 'X'.
      WL_CRIA_FI = 'X'.
      if W_SAIDA-BELNR2 = 'PEDIDO'.
        WL_TABIX = 'X'.
      endif.
      perform ADD_ANLN1_LINE using     W_SAIDA
                                       L_TXTTIPO_KEY
                              changing L_ANLN1_KEY.
      W_FI = 'X'.
    else.
      clear W_PST.
      if W_SAIDA-BELNR2 <> 'MB1A'.
        clear W_FI.
      endif.
    endif.

*    IF w_tabix = 1.
*      PERFORM add_ebeln_line USING     w_saida
*                                     l_anln1_key
*                            CHANGING l_ebeln_key.
*    ELSE.
    if W_SAIDA-BELNR2(2) = 'FI'.
*       w_saida-belnr2(2) = 'CI' or.
      W_PASTA = W_SAIDA-BELNR2(2).
    endif.
    on change of W_PASTA.
      if W_SAIDA-BELNR2(2) = 'FI' and
         P_SINCAX eq 'X'.
        clear W_EB.
      else.
        W_EB = 'X'.
      endif.
      clear W_PST.
*      if w_saida-belnr2 <> 'MB1A'.
      perform ADD_EBELN_LINE using     W_SAIDA
                                       L_ANLN1_KEY
                              changing L_EBELN_KEY.
    endon.
    if W_TABIX = 1 and W_EB is initial.
      W_EB = 'X'.
      perform ADD_EBELN_LINE using     W_SAIDA
                                       L_ANLN1_KEY
                              changing L_EBELN_KEY.
    elseif not W_PST is initial.
      clear W_PST.
      W_EB = 'X'.
      perform ADD_EBELN_LINE using     W_SAIDA
                                       L_ANLN1_KEY
                              changing L_EBELN_KEY.

    endif.

    clear W_FI.
    on change of W_SAIDA-BELNR2.
      W_FI = 'X'.
    endon.


    if not W_SAIDA-BELNR3 is initial and
           W_SAIDA-BELNR1 is initial.

      W_EBELN = W_SAIDA-EBELN.
      clear W_SAIDA-EBELN.

*      IF NOT w_eb = 'X'.
      W_PED = L_PEDIDO_KEY.
*      ELSE.
*        w_ped = l_pedido_key.
*      ENDIF.

      L_PEDIDO_KEY = L_EBELN_KEY.
    else.
      on change of W_SAIDA-EBELN.
        clear WL_TABIX.
        if W_SAIDA-BELNR2 ne 'MB1A'." AND p_sincax NE 'X'.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        else.
          L_PEDIDO_KEY = L_EBELN_KEY.
        endif.
        clear W_EB.
      endon.

** Eduado 10.05.2011
      if not WL_TABIX is initial ."AND p_sincax EQ 'X'.
        if W_SAIDA-BELNR2 ne 'MB1A' ."AND p_sincax NE 'X'.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        else.
          L_PEDIDO_KEY = L_EBELN_KEY.
        endif.
        clear W_EB.
      endif.
** Eduado 10.05.2011

      if P_SINCAX ne 'X'.
        if not W_EB is initial.
* eduardo 29.03.2011
*        and p_sincax ne 'X'        .
* eduardo 29.03.2011
          clear W_EB.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        endif.
      endif.
    endif.

*** Eduardo 05.04.2011 - CAIXA
*** Se for CAIXA, cria pasta para PAGOS e A PAGAR
    if not P_SINCAX is initial and
       W_SAIDA-BELNR2(2) = 'FI'.
      W_MUDA = 'X'.

      if not WL_CRIA_FI is initial.
*      ON CHANGE OF w_saida-belnr2 or w_anln1.

        clear WL_CRIA_FI.

*
*      IF w_fi = 'X'.
        clear W_MUDA.
        perform ADD_PEDIDO_LINE using     W_SAIDA
                                         L_EBELN_KEY
                                changing L_PEDIDO_KEY.
*      ENDIF.
*      ENDON.
        WL_CH = L_PEDIDO_KEY.

      endif.

    endif.
*** Eduardo 05.04.2011 - CAIXA

    if W_MUDA eq 'X'.
      clear W_MUDA.
*      l_pedido_key = w_ped.
      L_PEDIDO_KEY =  WL_CH.
    endif.

    if not W_EBELN is initial.
      W_SAIDA-EBELN = W_EBELN.
    endif.
    perform ADD_COMPLETE_LINE using  W_SAIDA
                                     L_PEDIDO_KEY
                            changing L_LAST_KEY.
    clear W_EBELN.
  endloop.
  clear W_SAIDA. free W_SAIDA.
  clear TW_SAIDA. refresh TW_SAIDA. free TW_SAIDA.
  W_DIF = W_PEDIDO - W_REALIZADO.

  if SY-TCODE ne 'ZIM08' and SY-BATCH ne 'X'.

* calculate totals
    call method TREE1->UPDATE_CALCULATIONS.

* this method must be called to send the data to the frontend
    call method TREE1->FRONTEND_UPDATE.
  else.

    if P_MOEDA = 'BRL'.

      modify ZIM08_REL_INV2 from table T_08.
    else.
      refresh: T_08_USD.
      loop at  T_08.
        move-corresponding: T_08 to T_08_USD.

        append  T_08_USD.
        clear: T_08_USD.
      endloop.

      modify ZIM08_REL_INV_US from table T_08_USD.
    endif.

    refresh: T_ZIMT003, T_08_USD.

    move: S_BUKRS-LOW to T_ZIMT003-BUKRS,
          SY-DATUM    to T_ZIMT003-AEDAT,
          SY-UZEIT    to T_ZIMT003-AENTIME,
          SY-UNAME    to T_ZIMT003-AENUSER.

    append T_ZIMT003.
    clear T_ZIMT003.
    modify  ZIMT003 from table T_ZIMT003.
    commit work and wait .
    refresh T_08.
  endif.

endform.                    " CREATE_HIERARCHY
data: begin of T_LFA1 occurs 0,
        LIFNR like LFA1-LIFNR,
        NAME1 like LFA1-NAME1,
      end of T_LFA1.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARA_DADOS
*&---------------------------------------------------------------------*
form F_PREPARA_DADOS .
  data: WL_TABIX      type SY-TABIX,
        TL_SET_AG     type table of SETLEAF with header line,
        TL_SETLINE_AG type table of SETLINET with header line,
        TL_RBKP_EKBE  like table of T_RBKP_EST with header line,
        WL_KURS2      type BKPF-KURS2,
        WL_AWKEY      type BKPF-AWKEY,
        WL_BELNR      type BKPF-BELNR,
        WL_GJAHR      type EKBE-GJAHR,
        WL_ZKKN       type BSAK-BUZEI,
        WL_WAERS      type BKPF-WAERS,
        WL_DMBTR      type BSIS-DMBTR,
        WL_DMBE2      type BSIS-DMBE2.

  refresh: TL_SET_AG, TL_SETLINE_AG, T_DOCS_AG, TL_RBKP_EKBE.


  T_EKBE_1[] = T_EKBE[].

  sort T_BSIS by BUKRS HKONT AUGDT AUGBL ZUONR GJAHR BELNR BUZEI.
  delete adjacent duplicates from T_BSIS comparing BUKRS HKONT AUGDT AUGBL ZUONR GJAHR BELNR BUZEI.

  data: begin of TL_CSKS occurs 0,
          KOKRS like CSKS-KOKRS,
          KOSTL like CSKS-KOSTL,
          DATBI like CSKS-DATBI,
          KTEXT like CSKT-KTEXT,
        end of TL_CSKS.


  data: begin of TL_COBRB occurs 0,
          OBJNR like COBRB-OBJNR,
          BUREG like COBRB-BUREG,
          LFDNR like COBRB-LFDNR,
          ANLN1 like COBRB-ANLN1,
          ANLN2 like COBRB-ANLN2,
          BUKRS like COBRB-BUKRS,
          TXT50 like ANLA-TXT50,
        end of TL_COBRB.

  data: begin of TL_ANLA occurs 0,
          BUKRS like ANLA-BUKRS,
          ANLN1 like ANLA-ANLN1,
          ANLN2 like ANLA-ANLN2,
          TXT50 like ANLA-TXT50,
        end of TL_ANLA.


  if not T_COAS[] is initial.
    select CSKS~KOKRS CSKS~KOSTL CSKS~DATBI CSKT~KTEXT
      from CSKS inner join CSKT on
      ( CSKS~KOKRS = CSKT~KOKRS and
        CSKS~KOSTL = CSKT~KOSTL )
      into table TL_CSKS
      for all entries in T_COAS
    where CSKS~KOSTL = T_COAS-KOSTV and
          CSKT~SPRAS = SY-LANGU.
    sort TL_CSKS by KOSTL.

    select COBRB~OBJNR COBRB~BUREG COBRB~LFDNR COBRB~ANLN1 COBRB~ANLN2 COBRB~BUKRS
      from COBRB into table TL_COBRB
      for all entries in T_COAS where
      OBJNR = T_COAS-OBJNR.

    if SY-SUBRC = 0.
      select BUKRS ANLN1 ANLN2 TXT50
        from ANLA into table TL_ANLA
        for all entries in TL_COBRB
        where BUKRS = TL_COBRB-BUKRS and
              ANLN1 = TL_COBRB-ANLN1 and
              ANLN2 = TL_COBRB-ANLN2.
    endif.

    sort TL_COBRB by OBJNR.
    sort TL_ANLA by BUKRS ANLN1 ANLN2.

  endif.

  select RBKP~BELNR RBKP~GJAHR RBKP~XBLNR RBKP~LIFNR
         LFA1~NAME1
    from RBKP inner join LFA1 on
    RBKP~LIFNR = LFA1~LIFNR
    into table T_RBKP1
    for all entries in T_EKBE_1
    where BELNR = T_EKBE_1-BELNR and
          GJAHR = T_EKBE_1-GJAHR.

  sort T_RBKP1 by BELNR GJAHR.


  data: V_TA    type SY-TABIX,
        V_BELNR like EKBE-BELNR.

  sort T_EKBE by EBELN EBELP.
  if T_EKBE[] is not initial.
    select BELNR GJAHR STBLG
      from RBKP
      into table TL_RBKP_EKBE
       for all entries in T_EKBE
       where BELNR eq T_EKBE-BELNR
         and GJAHR eq T_EKBE-GJAHR.

    sort TL_RBKP_EKBE by BELNR GJAHR.
  endif.
  sort T_BSAK.
  delete adjacent duplicates from T_BSAK comparing all fields.

  T_BSIK[] = T_BSIK_1[].
  T_BSIS_DP[] = T_BSIS_1[].

  T_EKBEA[] = T_EKBE[].

  clear: T_SAIDA, T_TOTAL, T_TOTALR.
  refresh: T_SAIDA, T_TOTAL, T_TOTALR.

  data: W_VGABE    type EKBE-VGABE,
        W_VAL(1),
        W_MSG(100),
        V_DATA     type SY-DATUM.

  if not T_EKKO[] is initial.
    select LIFNR NAME1 from LFA1 into table T_LFA1
      for all entries in T_EKKO
      where ( LIFNR = T_EKKO-LIFNR or
              LIFNR = T_EKKO-LIFRE ).
  endif.


  if not T_BSIK[] is initial.
    select LIFNR NAME1 from LFA1 appending table T_LFA1
      for all entries in T_BSIK
      where  LIFNR = T_BSIK-LIFNR.
  endif.


  if not T_BSAK[] is initial.
    select LIFNR NAME1 from LFA1 appending table T_LFA1
      for all entries in T_BSAK
      where LIFNR = T_BSAK-LIFNR.

  endif.

  sort T_LFA1 by LIFNR.
*  SORT t_imaka BY anln1 anln2.

  data W_TAB type SY-TABIX.
  data: W_EBEL type EKKO-EBELN.
  clear W_EBEL.
* W_EBEL = 9999990000.
  W_EBEL = 9999900000.
  sort T_IMAKA by POSNR BUKRS ANLN1 ANLN2.
  sort T_EKKO by ANLN1 ANLN2 AUFNR EBELN EBELP.


  if P_SINCAX is not initial.
    if T_EKBEA[] is not initial.
      select BELNR GJAHR STBLG
        from RBKP
        into table T_RBKP_EST
         for all entries in T_EKBEA
         where BELNR eq T_EKBEA-AWKEY(10)
           and GJAHR eq T_EKBEA-GJAHR
           and STBLG ne SPACE.

      loop at T_EKBEA.
        WL_TABIX = SY-TABIX.
        read table T_RBKP_EST
          with key BELNR = T_EKBEA-AWKEY(10)
                   GJAHR = T_EKBEA-GJAHR.

        if SY-SUBRC is initial.
          delete T_EKBEA index WL_TABIX.
        endif.
      endloop.
    endif.
    if T_EKBEC[] is not  initial.
      select BELNR GJAHR STBLG
            from RBKP
            into table T_RBKP_EST
             for all entries in T_EKBEC
             where BELNR eq T_EKBEC-AWKEY(10)
               and GJAHR eq T_EKBEC-GJAHR
               and STBLG ne SPACE.

      loop at T_EKBEC.
        WL_TABIX = SY-TABIX.
        read table T_RBKP_EST
          with key BELNR = T_EKBEC-AWKEY(10)
                   GJAHR = T_EKBEC-GJAHR.

        if SY-SUBRC is initial.
          delete T_EKBEC index WL_TABIX.
        endif.
      endloop.

      select BELNR GJAHR STBLG
             from BKPF
             appending table T_BKPF_EST
              for all entries in T_EKBEC
              where BUKRS in S_BUKRS
                and BELNR eq T_EKBEC-BELNR
                and GJAHR eq T_EKBEC-GJAHR
                and STBLG ne SPACE.

      loop at T_EKBEC.
        WL_TABIX = SY-TABIX.
        read table T_BKPF_EST
          with key BELNR = T_EKBEC-BELNR
                   GJAHR = T_EKBEC-GJAHR.

        if SY-SUBRC is initial.
          delete T_EKBEC index WL_TABIX.
        endif.
      endloop.

      select BUKRS HKONT AUGDT AUGBL ZUONR
             GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
           from BSIS into table T_BSIS_LB
           for all entries in T_EKBEC
           where BUKRS in S_BUKRS      and
                 GJAHR = T_EKBEC-GJAHR and
                 BELNR = T_EKBEC-BELNR.
      if SY-SUBRC is initial.
        select BUKRS SAKNR FDLEV "#EC CI_DB_OPERATION_OK[2431747]
          from SKB1
          into table T_SKB1_LB
           for all entries in T_BSIS_LB
           where BUKRS eq T_BSIS_LB-BUKRS
             and SAKNR eq T_BSIS_LB-HKONT
             and FDLEV eq 'F0'.
      endif.

      select BUKRS HKONT AUGDT AUGBL ZUONR
             GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
           from BSAS into table T_BSAS_LB
           for all entries in T_EKBEC
           where BUKRS in S_BUKRS      and
                 GJAHR = T_EKBEC-GJAHR and
                 BELNR = T_EKBEC-BELNR.
      if SY-SUBRC is initial.
        select BUKRS SAKNR FDLEV "#EC CI_DB_OPERATION_OK[2431747]
              from SKB1
              appending table T_SKB1_LB
               for all entries in T_BSAS_LB
               where BUKRS eq T_BSAS_LB-BUKRS
                 and SAKNR eq T_BSAS_LB-HKONT
                 and FDLEV eq 'F0'.
      endif.
    endif.

    if T_BSIS_LB[] is not initial.
      select BUKRS BELNR GJAHR KURS2
        from BKPF
        into table T_BKPF_LB
         for all entries in T_BSIS_LB
         where BUKRS eq T_BSIS_LB-BUKRS
           and BELNR eq T_BSIS_LB-BELNR
           and GJAHR eq T_BSIS_LB-GJAHR.
    endif.

    if T_BSAS_LB[] is not initial.
      select BUKRS BELNR GJAHR KURS2
        from BKPF
        appending table T_BKPF_LB
         for all entries in T_BSAS_LB
         where BUKRS eq T_BSAS_LB-BUKRS
           and BELNR eq T_BSAS_LB-BELNR
           and GJAHR eq T_BSAS_LB-GJAHR.
    endif.

    select *
      from SETLEAF
    into table TL_SET_AG
    where SETNAME eq 'MAGGI_ZIM01_AG'.

    if SY-SUBRC is initial.
      select *
        from SETLINET
        into table TL_SETLINE_AG
         for all entries in TL_SET_AG
          where LINEID  eq TL_SET_AG-LINEID
            and SETNAME eq TL_SET_AG-SETNAME.
    endif.
    loop at TL_SET_AG.
      read table TL_SETLINE_AG
        with key LINEID = TL_SET_AG-LINEID
                 SETNAME = TL_SET_AG-SETNAME.

      move: TL_SETLINE_AG-DESCRIPT(4)   to T_DOCS_AG-BUKRS,
            TL_SET_AG-VALFROM           to T_DOCS_AG-BELNR,
            TL_SETLINE_AG-DESCRIPT+4(1) to T_DOCS_AG-AG.

      append T_DOCS_AG.
      clear: T_DOCS_AG, TL_SETLINE_AG.

    endloop.

    select *
      from SETLEAF
    into table T_SETLEAF
    where SETNAME eq 'MAGGI_ZIM01_LIQ_BANCO'.

    if T_BSAK[] is not initial.
      select BUKRS LIFNR UMSKS UMSKZ AUGDT
           AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN DMBTR DMBE2
      from BSAK into table T_BSAK_AG
      for all entries in T_BSAK
      where BUKRS eq T_BSAK-BUKRS  and
            GJAHR eq T_BSAK-GJAHR and
            AUGBL eq T_BSAK-AUGBL.
    endif.

    if T_BSIK[] is not initial.
      select BUKRS LIFNR UMSKS UMSKZ AUGDT
           AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN DMBTR DMBE2
      from BSAK appending table T_BSAK_AG
   for all entries in T_BSIK
   where BUKRS eq T_BSIK-BUKRS and
         GJAHR eq T_BSIK-GJAHR and
         AUGBL eq T_BSIK-AUGBL.
    endif.
  endif.

  loop at T_IMAK.
    concatenate 'Processando solicitação:' T_IMAK-POSNR into W_MSG separated by SPACE.
    perform F_MSG  using    W_MSG
                     '70'.
    move-corresponding: T_IMAK  to TW_SAIDA.
    move T_IMAK-AKOSTL to TW_SAIDA-BELNR4.
** Analítico
    if not P_ANALIT is initial.
      perform F_MONTA_DADOS_ANALITICO using 'X'.
    endif.

    loop at T_IMAKA where POSNR = T_IMAK-POSNR and
                          BUKRS = T_IMAK-ABUKRS.

      move-corresponding T_IMAKA to TW_SAIDA.
      move T_IMAKA-TXT50 to TW_SAIDA-TXT050.
      add 1 to W_EBEL.

      read table T_EKKO with key ANLN1 = T_IMAKA-ANLN1
                                 ANLN2 = T_IMAKA-ANLN2.
      if SY-SUBRC <> 0.
        loop at T_EKKN where ANLN1 = T_IMAKA-ANLN1 and
                             ANLN2 = T_IMAKA-ANLN2.
          read table T_EKKO with key EBELN = T_EKKN-EBELN
                                     EBELP = T_EKKN-EBELP.
*                             anln1 = t_imaka-anln1.

          move-corresponding T_EKKN to T_EKKO.
          T_EKKO-ANLN2 = T_IMAKA-ANLN2.
          append T_EKKO. clear T_EKKO.
        endloop.
      endif.
*---> 05/07/2023 - Migração S4 - DL
      SORT T_161 BY BSART.
*<--- 05/07/2023 - Migração S4 - DL
      loop at T_EKKO where ANLN1 = T_IMAKA-ANLN1 and
                           ANLN2 = T_IMAKA-ANLN2.
        clear: TW_SAIDA-KONNR, TW_SAIDA-KTPNR, TW_SAIDA-SGTXT, TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR.

        if T_EKKO-BSART = 'ZIM' or T_EKKO-BSART = 'ZPI'.
          TW_SAIDA-SGTXT = T_EKKO-BSART.
        endif.

*** Teste dados de serviço
        clear: TW_SAIDA-SRVPOS, TW_SAIDA-KTEXT1.
*** Teste dados de serviço


        W_VGABE = 1.
        move-corresponding: T_IMAK  to TW_SAIDA,
                            T_IMAKZ to TW_SAIDA,
                            T_IMAKA to TW_SAIDA,
                            T_EKKO  to TW_SAIDA.
        if T_EKKO-VPROZ ne 0.
*        and p_sincax is initial.
          clear: TW_SAIDA-MENGE, TW_SAIDA-NETWR, TL_RBKP_EKBE.
          loop at T_EKBE
             where EBELN = T_EKKO-EBELN
               and EBELP = T_EKKO-EBELP
               and ANLN1 = T_IMAKA-ANLN1
               and ZEKKN = T_EKKO-ZEKKN
               and SHKZG ne 'H'.

            read table TL_RBKP_EKBE
              with key BELNR = T_EKBE-BELNR
                       GJAHR = T_EKBE-GJAHR
                       binary search.
*******          read table t_ekbe with key ebeln = t_ekko-ebeln
*******                                     ebelp = t_ekko-ebelp
*******                                     anln1 = t_imaka-anln1.
            if SY-SUBRC ne 0
            and TL_RBKP_EKBE-STBLG is initial.
              read table T_EKKN with key EBELN = T_EKKO-EBELN
                           EBELP = T_EKKO-EBELP
                           ANLN1 = T_IMAKA-ANLN1.
              if SY-SUBRC = 0.
*                MOVE-CORRESPONDING t_ekkn TO t_ekbe.
                move: T_EKKN-MENGE to TW_SAIDA-MENGE,
                      T_EKKN-NETWR to TW_SAIDA-NETWR.
                if TW_SAIDA-NETWR is initial.
                  TW_SAIDA-NETWR = T_EKKO-NETPR * ( T_EKKO-VPROZ / 100 ).
                endif.
                if TW_SAIDA-MENGE is initial.
                  TW_SAIDA-MENGE = T_EKKO-MENGE * ( T_EKKO-VPROZ / 100 ).
                endif.
              endif.
            else.
**                w_vgabe = 2.
**                if sy-subrc = 0.
***            w_ek = 'X'.
**********                move: t_ekbe-menge to tw_saida-menge,
**********                      t_ekbe-dmbtr to tw_saida-netwr.
**                  add: t_ekbe-menge to tw_saida-menge,
**                       t_ekbe-dmbtr to tw_saida-netwr.
**                endif.
            endif.
*            endif.
            clear: TL_RBKP_EKBE.
          endloop.  """"""""""
        else.
          move  T_EKKO-EFFWR to TW_SAIDA-NETWR.
        endif.

        read table T_PACK with key PACKNO = T_EKKO-PACKNO.
        if SY-SUBRC = 0.
          read table T_ESLL_1 with key PACKNO = T_PACK-PACKNO.
          if SY-SUBRC = 0.
            read table T_ESLL_2 with key PACKNO = T_ESLL_1-SUB_PACKNO.
            if SY-SUBRC = 0.
              move: T_ESLL_2-EXTROW to TW_SAIDA-EXTROW,
                    T_ESLL_2-SRVPOS to TW_SAIDA-SRVPOS,
                    T_ESLL_2-KTEXT1 to TW_SAIDA-KTEXT1.
            endif.
          endif.
        endif.

        move: 'Imobilizado' to TW_SAIDA-TXTIMOB,
              'Pedido'      to TW_SAIDA-TXTPED.

        read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                   binary search.
        if SY-SUBRC = 0.
          concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
            separated by ' - '.
        endif.
        read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                                 binary search.
        if SY-SUBRC = 0.
          concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
            separated by ' - '.
        endif.

*        IF tw_saida-anlkl NE '00050000'.
        TW_SAIDA-TXTTIPO = 'IMOBILIZADOS (AA-Imobilizado)'.

        concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2 into TW_SAIDA-OBJETO separated by '-'.
        TW_SAIDA-TXT050 = T_IMAKA-TXT50.
*        ELSE.
*          tw_saida-txttipo = 'OBRAS EM ANDAMENTO (CO-Ordem Interna)'.
*        ENDIF.

*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
        if P_MOEDA      eq 'BRL' and
           T_EKKO-WAERS ne 'BRL'.
          perform F_CONVERTE_MOEDA using T_EKKO-WKURS
                                changing TW_SAIDA-NETWR.
        elseif P_MOEDA ne 'BRL' and
               T_EKKO-WAERS ne P_MOEDA.

          if not P_HIST is initial.
            V_DATA = T_EKKO-AEDAT.
          else.
            V_DATA = P_DATAC.
          endif.
          perform F_CONVERTE_COTACAO using T_EKKO-WAERS
                                           V_DATA
                                  changing TW_SAIDA-NETWR.
        endif.
*** Fim da rotina da TAXA


        move TW_SAIDA-NETWR to TW_SAIDA-NETPR.
        move: T_IMAKA-TXT50 to TW_SAIDA-TXT50.
        read table T_161 with key BSART = T_EKKO-BSART
                                  binary search.
        if SY-SUBRC = 0.
          concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
          separated by ' - '.
        else.
          clear TW_SAIDA-TIPOPED.
        endif.
        TW_SAIDA-BELNR2 = 'PEDIDO'.
        TW_SAIDA-DIFERENCA = TW_SAIDA-NETWR.
*        tw_saida-ped_ap = tw_saida-real_ap = tw_saida-netwr * ( - 1 ).
        TW_SAIDA-PED_AP = TW_SAIDA-NETWR * ( - 1 ).
        read table TW_SAIDA transporting no fields
          with key EBELN = TW_SAIDA-EBELN
                   EBELP = TW_SAIDA-EBELP
                   ANLN1 = TW_SAIDA-ANLN1
                   ANLN2 = TW_SAIDA-ANLN2
                   POSNR = TW_SAIDA-POSNR.

        if SY-SUBRC is not initial.
          append TW_SAIDA.
        endif.
        clear: TW_SAIDA-PED_AP, TW_SAIDA-REAL_AP, TW_SAIDA-TIPOPED,
               TW_SAIDA-BELNR2.
        clear W_VAL.



*---> 05/07/2023 - Migração S4 - DL
        SORT T_161 BY BSART.
*<--- 05/07/2023 - Migração S4 - DL
        loop at T_EKBE where EBELN = T_EKKO-EBELN and
                             EBELP = T_EKKO-EBELP and
*                             ( zekkn = t_ekko-zekkn or
                               VGABE = 1."               ) .""  AND
*                             anln1 = t_imaka-anln1.
*                             vgabe = 2.
*          v_ta = sy-tabix.
          V_BELNR = T_EKBE-BELNR.
          if not T_EKBE-ANLN1 is initial.
            if T_EKBE-ANLN1 ne T_IMAKA-ANLN1 ."or
* eduardo - 20.03.2011
*               t_ekbe-anln2 NE t_imaka-anln2.
* eduardo - 20.03.2011
              continue.
            endif.
          endif.


*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
          if P_MOEDA      eq 'BRL' and
             T_EKBE-WAERS ne 'BRL'.
            if not P_HIST is initial.
              V_DATA = T_EKBE-BUDAT.
            else.
              V_DATA = P_DATAC.
            endif.

*            PERFORM f_converte_moeda USING t_ekko-wkurs
*                                  CHANGING t_ekbe-dmbtr.
          elseif P_MOEDA ne 'BRL' and
                 T_EKBE-WAERS ne P_MOEDA.

            if not P_HIST is initial.
              V_DATA = T_EKBE-BUDAT.
            else.
              V_DATA = P_DATAC.
            endif.
            perform F_CONVERTE_COTACAO using T_EKBE-WAERS
                                             V_DATA
                                    changing T_EKBE-DMBTR.
          elseif P_MOEDA ne 'BRL' and
               T_EKBE-WAERS eq P_MOEDA.
            T_EKBE-DMBTR = T_EKBE-WRBTR.
          endif.
*** Fim da rotina da TAXA


          move:
                T_EKBE-XBLNR to TW_SAIDA-NFENUM,
                T_EKBE-MENGE to TW_SAIDA-MENGE,
                T_EKBE-BUDAT to TW_SAIDA-AEDAT,
                T_EKBE-DMBTR to TW_SAIDA-NETPR.

          if T_EKBE-VGABE = 1.
            move: T_EKBE-BELNR to TW_SAIDA-BELNR1.
*                  t_ekbe-dmbtr TO tw_saida-dmbtr.
            clear TW_SAIDA-BELNR2.

            if T_EKBE-BWART = '102'.
              TW_SAIDA-BELNR2 = 'MIGO-EST'.
            elseif T_EKBE-BWART = '122'.
              TW_SAIDA-BELNR2 = 'MIGO-DEV'.
            else.
              TW_SAIDA-BELNR2 = 'MIGO'.
              if T_EKKO-BSART = 'ZIM'.
                TW_SAIDA-SGTXT = T_EKKO-BSART.
                TW_SAIDA-DMBTR = T_EKBE-DMBTR.

                if TW_SAIDA-DMBTR is initial.
                  clear: WL_BELNR, WL_GJAHR, WL_KURS2, WL_ZKKN, WL_WAERS, WL_DMBTR, WL_DMBE2.
                  select single DMBTR BELNR GJAHR ZEKKN from EKBE
                    into (TW_SAIDA-DMBTR, WL_BELNR, WL_GJAHR, WL_ZKKN)
                    where EBELN = T_EKBE-EBELN and
                          EBELP = T_EKBE-EBELP and
                          VGABE = 2.

                  concatenate WL_BELNR WL_GJAHR into WL_AWKEY.
                  select single KURS2 WAERS BELNR
                    from BKPF
                    into (WL_KURS2, WL_WAERS, WL_BELNR)
                     where BUKRS eq TW_SAIDA-ABUKRS
                       and GJAHR eq WL_GJAHR
                       and AWKEY eq WL_AWKEY.

                  if P_MOEDA ne 'BRL'.
                    if WL_WAERS eq 'USD'.
                      if SY-SUBRC is initial.
                        divide TW_SAIDA-DMBTR by WL_KURS2.
                        TW_SAIDA-NETPR = TW_SAIDA-DMBTR.
                      endif.
                    elseif WL_WAERS eq 'EUR'.
                      select single DMBTR DMBE2
                        from BSIK
                        into (WL_DMBTR, WL_DMBE2) "tw_saida-dmbtr
                         where BELNR eq WL_BELNR
                           and BUKRS eq TW_SAIDA-ABUKRS
                           and GJAHR eq WL_GJAHR
                           and BUZEI eq WL_ZKKN.

                      if SY-SUBRC is not initial.
                        select single DMBTR DMBE2
                          from BSAK
                          into (WL_DMBTR, WL_DMBE2)
                           where BELNR eq WL_BELNR
                             and BUKRS eq TW_SAIDA-ABUKRS
                             and GJAHR eq WL_GJAHR
                             and BUZEI eq WL_ZKKN.
                      endif.
                      try.
                          WL_KURS2 = WL_DMBTR / WL_DMBE2.
                          divide TW_SAIDA-DMBTR by WL_KURS2.
                        catch CX_SY_ZERODIVIDE.
                      endtry.
                      TW_SAIDA-NETPR = TW_SAIDA-DMBTR.
                    endif.
                  else.
                    TW_SAIDA-NETPR = TW_SAIDA-DMBTR.
                  endif.
                endif.

              endif.
            endif.




          else.
            read table T_RBKP with key BELNR = T_EKBE-BELNR
                                       GJAHR = T_EKBE-GJAHR
                                       binary search.
            if SY-SUBRC = 0.
              TW_SAIDA-NFENUM = T_RBKP-XBLNR .
            endif.

            move: T_EKBE-BELNR to TW_SAIDA-BELNR1, "2
                  T_EKBE-DMBTR to TW_SAIDA-DMBTR.
            clear: TW_SAIDA-BELNR2." tw_saida-dmbtr.
            if T_EKBE-VGABE = 2 and
               T_EKBE-SHKZG = 'H'.
              TW_SAIDA-BELNR2 = 'MIRO-NC'.
            else.
              TW_SAIDA-BELNR2 = 'MIRO'.
            endif.
          endif.

*            MOVE: t_ekbe-belnr TO tw_saida-belnr1.
*          IF t_ekbe-belnr NE t_ekbe-lfbnr.
*            MOVE: t_ekbe-lfbnr TO tw_saida-belnr2.
*          ELSE.
*          CLEAR tw_saida-belnr2.
*          ENDIF.

          read table WE_BKPF with key AWKEY = T_EKBE-AWKEY
                                      binary search.
          if SY-SUBRC = 0.
*            IF t_ekbe-vgabe = 1.
            if T_EKBE-VGABE eq 1.
              loop at WE_BKPF where AWKEY eq T_EKBE-AWKEY
                                and BLART ne 'ML'.

                move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
                exit.
              endloop.
            else.
              move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
            endif.
*            clear tw_saida-belnr4.
*            ELSE.
*              MOVE we_bkpf-belnr TO tw_saida-belnr4.
*              CLEAR tw_saida-belnr3.
*            ENDIF.
          else.
*            CLEAR tw_saida-belnr4.
          endif.

          if T_EKBE-SHKZG = 'H'.
            TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ) .
            TW_SAIDA-MENGE = TW_SAIDA-MENGE * ( - 1 ) .
          endif.

          if T_EKBE-VGABE eq 1.
          endif.

*Início Alteração Ricardo Furst.
          if P_ALV is initial.
            clear:   TW_SAIDA-EXTROW, "tw_saida-werks,
                   TW_SAIDA-TXT50, TW_SAIDA-TXTPED, "tw_saida-netpr,
                   "tw_saida-netwr,
                    TW_SAIDA-WAERS, TW_SAIDA-TXTORD,
                   TW_SAIDA-ANLKL," tw_saida-matnr, tw_saida-txz01, tw_saida-meins,
                   TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR.
          endif.
*Fim Alteração Ricardo Furst.
* tw_saida-ktext1, , tw_saida-aufnr  tw_saida-txttipo, tw_saida-srvpos, , tw_saida-txtimob


          read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                     binary search.
          if SY-SUBRC = 0.
            concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
              separated by ' - '.
          endif.

          if T_EKBE-VGABE = 1.
            read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                                     binary search.
            if SY-SUBRC = 0.
              concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
                separated by ' - '.
            endif.
          else.
            read table T_RBKP with key BELNR = T_EKBE-BELNR
                                       GJAHR = T_EKBE-GJAHR
                                       binary search.
            if SY-SUBRC = 0.
              concatenate T_RBKP-NAME1 T_RBKP-LIFNR into TW_SAIDA-EMISSOR
              separated by ' - '.
            endif.
          endif.

          TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).

          TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]

          read table T_161 with key BSART = T_EKKO-BSART
                                    binary search.
          if SY-SUBRC = 0.
            concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
            separated by ' - '.
          else.
            clear TW_SAIDA-TIPOPED.
          endif.

          TW_SAIDA-WAERS = T_EKBE-WAERS.
          TW_SAIDA-WERKS = T_IMAK-WERKS.

          append TW_SAIDA.
*          COLLECT tw_saida.

***eduardo 04052010
          clear: TW_SAIDA-NFENUM, TW_SAIDA-EMENGE, TW_SAIDA-DMBTR, TW_SAIDA-BELNR2, TW_SAIDA-BELNR1,
                 TW_SAIDA-BELNR3, TW_SAIDA-REAL_AP, TW_SAIDA-BELNR5.
        endloop.
*        CLEAR tw_saida.
        V_BELNR = '1'.
        perform Z_EKBE using V_BELNR changing V_DATA.






      endloop.
      clear: TW_SAIDA-TIPOPED, TW_SAIDA-SGTXT.


*************************************
*      ENDIF.

*Início Alteração Ricardo Furst.
      if P_ALV is initial.
        clear: TW_SAIDA-EXTROW,  TW_SAIDA-BELNR1, TW_SAIDA-NFENUM, "tw_saida-werks,
               TW_SAIDA-TXT50, TW_SAIDA-NETPR, TW_SAIDA-TXTPED, TW_SAIDA-BELNR2,
               TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD, TW_SAIDA-EBELP,
               TW_SAIDA-ANLKL, TW_SAIDA-MATNR, TW_SAIDA-TXZ01, TW_SAIDA-EBELP,
               TW_SAIDA-MENGE, TW_SAIDA-MEINS, TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR,
               TW_SAIDA-SRVPOS, TW_SAIDA-KTEXT1, TW_SAIDA-EXTROW.
      endif.
*Fim Alteração Ricardo Furst.

*      IF s_lifre[] IS INITIAL AND
*         s_lifnr[] IS INITIAL AND
*         s_ebeln[] IS INITIAL.
      TW_SAIDA-TXTTIPO = 'IMOBILIZADOS (AA-Imobilizado)'.
      concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2 into TW_SAIDA-OBJETO separated by '-'.
      TW_SAIDA-TXT050 = T_IMAKA-TXT50.
      loop at T_MSEG where ANLN1 = T_IMAKA-ANLN1 and
                           ANLN2 = T_IMAKA-ANLN2.
        clear: TW_SAIDA-EBELN, TW_SAIDA-EBELP.
        move: T_MSEG-MBLNR to TW_SAIDA-BELNR1,
              T_MSEG-DMBTR to TW_SAIDA-DMBTR,
              T_MSEG-DMBTR to TW_SAIDA-NETPR,
              T_MSEG-MATNR to TW_SAIDA-MATNR,
              T_MSEG-MAKTX to TW_SAIDA-TXZ01,
              T_MSEG-BUDAT to TW_SAIDA-AEDAT,
              T_MSEG-TCODE2 to TW_SAIDA-BELNR2.
*              t_mseg-dmbe2 TO tw_saida-emenge.

        read table WF_BKPF with key AWKEY = T_MSEG-AWKEY binary search.
        if SY-SUBRC = 0.
          move: WF_BKPF-BELNR to TW_SAIDA-BELNR3.
        endif.

        if TW_SAIDA-BELNR2 = 'MBST'.
          TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ).
        endif.

        if TW_SAIDA-BELNR2 = 'MB1A' and T_MSEG-SHKZG = 'S'.
          TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ).
        endif.

        TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).
        TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]
        TW_SAIDA-WERKS = T_IMAK-WERKS.

        append TW_SAIDA.
        clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE, TW_SAIDA-REAL_AP,
               TW_SAIDA-NETWR.
      endloop.


      loop at T_BSEG where BUKRS = T_IMAK-ABUKRS  and
                           ANLN1 = T_IMAKA-ANLN1 and
                           ANLN2 = T_IMAKA-ANLN2.
        if T_BSEG-ANBWA = 'LMC' or
           T_BSEG-ANBWA = 'LMD' or
           T_BSEG-ANBWA = '210' or
           T_BSEG-ANBWA = '180' or
           T_BSEG-ANBWA = '290' or
           T_BSEG-UMSKS = 'A'   or
           T_BSEG-ANBWA = '260'.
          continue.
        endif.

        if T_BSEG-HKONT = '0000512000' or
           T_BSEG-HKONT = '0000350000'.
          continue.
        endif.

        if T_BSEG-ANLN1 is not initial and
           T_BSEG-ANBWA is initial.
          continue.
        endif.

        clear: TW_SAIDA-EBELN, TW_SAIDA-EBELP.

        move: T_BSEG-BELNR to TW_SAIDA-BELNR3,
              T_BSEG-DMBTR to TW_SAIDA-DMBTR,
              T_BSEG-DMBTR to TW_SAIDA-NETPR,
              T_BSEG-DMBE2 to TW_SAIDA-EMENGE,
              T_IMAK-WERKS to TW_SAIDA-WERKS,
              T_BSEG-SGTXT to TW_SAIDA-SGTXT,
              T_BSEG-SGTXT to TW_SAIDA-TXZ01.

        if P_MOEDA eq 'USD'.
          move : T_BSEG-DMBE2 to TW_SAIDA-DMBTR.
        endif.
        if T_BSEG-SHKZG = 'H'.
          TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * -1.
        endif.

        read table T_BSIS with key GJAHR = T_BSEG-GJAHR
                                   BELNR = T_BSEG-BELNR
                                   BUZEI = T_BSEG-BUZEI.
        if SY-SUBRC = 0.
          move T_BSIS-BUDAT to TW_SAIDA-AEDAT.
        endif.

        read table T_BKPF with key BUKRS = T_BSEG-BUKRS
                                   GJAHR = T_BSEG-GJAHR
                                   BELNR = T_BSEG-BELNR.
        if SY-SUBRC = 0.
          move T_BKPF-TCODE to TW_SAIDA-BELNR2.
        endif.


        TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).
        TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]
        append TW_SAIDA.

        clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-SGTXT, TW_SAIDA-EMENGE, TW_SAIDA-REAL_AP.

      endloop.
*      CLEAR tw_saida.

*      IF NOT p_sincax IS INITIAL.
      if P_CONSOL is initial.
        loop at T_EKKO where ANLN1 = T_IMAKA-ANLN1 and
                             ANLN2 = T_IMAKA-ANLN2.
          perform F_MONTA_DADOS_CAIXA_PG.
        endloop.
*        LOOP AT t_ekko WHERE anln1 = t_imaka-anln1 AND
        loop at T_EKKO_POR into T_EKKO where ANLN1 = T_IMAKA-ANLN1 and
                  ANLN2 = T_IMAKA-ANLN2.
          perform F_MONTA_DADOS_CAIXA_APG.
        endloop.
      endif.
*      ENDIF.
    endloop.

    clear: TW_SAIDA-ANLN1, TW_SAIDA-ANLN2.
********************************
********************************
******************************** ORDENS
********************************
********************************
********************************
    loop at T_IMAKZ where POSNR = T_IMAK-POSNR.
      add 1 to W_EBEL.


      loop at T_COAS where AUFNR = T_IMAKZ-AUFNR.
        if T_COAS-AUART = 'ZOAN'.
          TW_SAIDA-TXTTIPO = 'IMOBILIZADO EM ANDAMENTO (CO-Ordem Interna)'.
*          tw_saida-objeto = t_imakz-aufnr.
*          tw_saida-txt050 = t_imak-txt50.
          read table TL_COBRB with key OBJNR = T_COAS-OBJNR binary search.
          if SY-SUBRC = 0.
            concatenate TL_COBRB-ANLN1 TL_COBRB-ANLN2 into TW_SAIDA-OBJETO separated by '-'.
            read table TL_ANLA with key BUKRS = TL_COBRB-BUKRS
                                        ANLN1 = TL_COBRB-ANLN1
                                        ANLN2 = TL_COBRB-ANLN2 binary search.
            if SY-SUBRC = 0.
              TW_SAIDA-TXT050 = TL_ANLA-TXT50.
            endif.
          else.
            clear: TW_SAIDA-OBJETO, TW_SAIDA-TXT050.
          endif.


        else.
          TW_SAIDA-TXTTIPO = 'CUSTOS/DESPESAS (CO-Ordem Estatistica)'.
          read table TL_CSKS with key KOSTL = T_COAS-KOSTV
                                      binary search.
          if SY-SUBRC = 0.
            TW_SAIDA-TXT050 = TL_CSKS-KTEXT.
          endif.
          TW_SAIDA-OBJETO = T_COAS-KOSTV.

        endif.
*        CLEAR tw_saida.
        move-corresponding: T_IMAK  to TW_SAIDA,
                            T_COAS to TW_SAIDA.
*                            t_imaka TO tw_saida,
*                            t_ekko  TO tw_saida.
        move T_COAS-KTEXT to TW_SAIDA-TXT50.

        TW_SAIDA-TXT050 = T_COAS-KTEXT.
        TW_SAIDA-OBJETO = T_COAS-KOSTV.
*        tw_saida-objeto = t_coas-aufnr.
*        READ TABLE t_ekko WITH KEY aufnr = t_imakz-aufnr.
*---> 05/07/2023 - Migração S4 - DL
        SORT T_161 BY BSART.
*<--- 05/07/2023 - Migração S4 - DL
        loop at T_EKKO where AUFNR = T_COAS-AUFNR.


*          tw_saida-objeto = t_coas-aufnr.
          clear: TW_SAIDA-KONNR, TW_SAIDA-KTPNR, TW_SAIDA-DMBTR, TW_SAIDA-SGTXT, TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR.
*        IF sy-subrc = 0.

          if T_EKKO-BSART = 'ZIM' or T_EKKO-BSART = 'ZPI'.
            TW_SAIDA-SGTXT = T_EKKO-BSART.
          endif.
*** Teste dados de serviço
          clear: TW_SAIDA-SRVPOS, TW_SAIDA-KTEXT1.
*** Teste dados de serviço

          move-corresponding T_EKKO to TW_SAIDA.

          if T_EKKO-VPROZ ne 0.
*          and p_sincax is initial.
            read table T_EKBE with key EBELN = T_EKKO-EBELN
                                       EBELP = T_EKKO-EBELP
                                       AUFNR = T_COAS-AUFNR.
            if SY-SUBRC ne 0.
              read table T_EKKN with key EBELN = T_EKKO-EBELN
                           EBELP = T_EKKO-EBELP
                           AUFNR = T_COAS-AUFNR.
              if SY-SUBRC = 0.
*                MOVE-CORRESPONDING t_ekkn TO t_ekbe.
                move: T_EKKN-MENGE to TW_SAIDA-MENGE,
                      T_EKKN-NETWR to TW_SAIDA-NETWR.
                if TW_SAIDA-NETWR is initial.
                  TW_SAIDA-NETWR = T_EKKO-NETPR * ( T_EKKO-VPROZ / 100 ).
                endif.
                if TW_SAIDA-MENGE is initial.
                  TW_SAIDA-MENGE = T_EKKO-MENGE * ( T_EKKO-VPROZ / 100 ).
                endif.
              endif.
            else.
              W_VGABE = 2.
              if SY-SUBRC = 0.
*            w_ek = 'X'.
                if T_EKKO-VPROZ < '99.9'.
*                and p_sincax is initial.

                  move: T_EKBE-MENGE to TW_SAIDA-MENGE,
                        T_EKBE-DMBTR to TW_SAIDA-NETWR.
                  if TW_SAIDA-NETWR is initial.
                    TW_SAIDA-NETWR = T_EKKO-NETPR * ( T_EKKO-VPROZ / 100 ).
                  endif.
                  if TW_SAIDA-MENGE is initial.
                    TW_SAIDA-MENGE = T_EKKO-MENGE * ( T_EKKO-VPROZ / 100 ).
                  endif.
                endif.
              endif.
            endif.
          else.
            move  T_EKKO-EFFWR to TW_SAIDA-NETWR.
          endif.



          read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                     binary search.
          if SY-SUBRC = 0.
            concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
              separated by ' - '.
          endif.
          read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                                   binary search.
          if SY-SUBRC = 0.
            concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
              separated by ' - '.
          endif.
          read table T_PACK with key PACKNO = T_EKKO-PACKNO.
          if SY-SUBRC = 0.
            read table T_ESLL_1 with key PACKNO = T_PACK-PACKNO.
            if SY-SUBRC = 0.
              read table T_ESLL_2 with key PACKNO = T_ESLL_1-SUB_PACKNO.
              if SY-SUBRC = 0.
                move: T_ESLL_2-EXTROW to TW_SAIDA-EXTROW,
                      T_ESLL_2-SRVPOS to TW_SAIDA-SRVPOS,
                      T_ESLL_2-KTEXT1 to TW_SAIDA-KTEXT1.
              endif.
            endif.
          endif.


          move: "t_coas-aufnr+2 to tw_saida-ebeln,
                'Ordem'       to TW_SAIDA-TXTIMOB,
                'Pedido'      to TW_SAIDA-TXTORD.

          if T_COAS-AUART = 'ZOAN'.
            TW_SAIDA-TXTTIPO = 'IMOBILIZADO EM ANDAMENTO (CO-Ordem Interna)'.
*            tw_saida-objeto = t_imakz-aufnr.
*            tw_saida-txt050 = t_imak-txt50.
            read table TL_COBRB with key OBJNR = T_COAS-OBJNR binary search.
            if SY-SUBRC = 0.
              concatenate TL_COBRB-ANLN1 TL_COBRB-ANLN2 into TW_SAIDA-OBJETO separated by '-'.
              read table TL_ANLA with key BUKRS = TL_COBRB-BUKRS
                                          ANLN1 = TL_COBRB-ANLN1
                                          ANLN2 = TL_COBRB-ANLN2 binary search.
              if SY-SUBRC = 0.
                TW_SAIDA-TXT050 = TL_ANLA-TXT50.
              endif.
            else.
              clear: TW_SAIDA-OBJETO, TW_SAIDA-TXT050.
            endif.
          else.
            TW_SAIDA-TXTTIPO = 'CUSTOS/DESPESAS (CO-Ordem Estatistica)'.
*            tw_saida-txt050 = t_coas-ktext.
*            tw_saida-objeto = t_coas-kostv.
            read table TL_CSKS with key KOSTL = T_COAS-KOSTV
                                        binary search.
            if SY-SUBRC = 0.
              TW_SAIDA-TXT050 = TL_CSKS-KTEXT.
            endif.
            TW_SAIDA-OBJETO = T_COAS-KOSTV.

          endif.



          move TW_SAIDA-NETWR to TW_SAIDA-NETPR.
          move T_COAS-KTEXT to TW_SAIDA-TXT50.


*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
          if P_MOEDA      eq 'BRL' and
             T_EKKO-WAERS ne 'BRL'.
            perform F_CONVERTE_MOEDA using T_EKKO-WKURS
                                  changing TW_SAIDA-NETWR.
          elseif P_MOEDA ne 'BRL' and
                 T_EKKO-WAERS ne P_MOEDA.

            if not P_HIST is initial.
              V_DATA = T_EKKO-AEDAT.
            else.
              V_DATA = P_DATAC.
            endif.
            perform F_CONVERTE_COTACAO using T_EKKO-WAERS
                                             V_DATA
                                    changing TW_SAIDA-NETWR.
          endif.
*** Fim da rotina da TAXA

          TW_SAIDA-DIFERENCA = TW_SAIDA-NETWR.

*          tw_saida-ped_ap = tw_saida-real_ap = tw_saida-netwr * ( - 1 ).
          TW_SAIDA-PED_AP = TW_SAIDA-NETWR * ( - 1 ).

          read table T_161 with key BSART = T_EKKO-BSART
                                    binary search.
          if SY-SUBRC = 0.
            concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
            separated by ' - '.
          else.
            clear TW_SAIDA-TIPOPED.
          endif.
          TW_SAIDA-BELNR2 = 'PEDIDO'.

          if P_SINCAX is not initial.
            read table TW_SAIDA transporting no fields
              with key EBELN  = T_EKKO-EBELN
                       EBELP  = T_EKKO-EBELP
                       AUFNR  = T_EKKO-AUFNR
                       BELNR2 = TW_SAIDA-BELNR2.

            if SY-SUBRC is initial.
              clear: TW_SAIDA-PED_AP, TW_SAIDA-REAL_AP, TW_SAIDA-TIPOPED, TW_SAIDA-BELNR2,
               TW_SAIDA-DMBTR, TW_SAIDA-DIFERENCA, TW_SAIDA-PED_AP, TW_SAIDA-REAL_AP.
              continue.
            endif.
          endif.
          append TW_SAIDA. "CLEAR tw_saida.
          clear: TW_SAIDA-PED_AP, TW_SAIDA-REAL_AP, TW_SAIDA-TIPOPED, TW_SAIDA-BELNR2,
                 TW_SAIDA-DMBTR, TW_SAIDA-DIFERENCA, TW_SAIDA-PED_AP, TW_SAIDA-REAL_AP.

          clear W_VAL.
*          LOOP AT t_ekbe WHERE ebeln = t_ekko-ebeln AND
*                               ebelp = t_ekko-ebelp." AND
**                               vgabe = 1.
*---> 05/07/2023 - Migração S4 - DL
          SORT T_161 BY BSART.
*<--- 05/07/2023 - Migração S4 - DL
          loop at T_EKBE where EBELN = T_EKKO-EBELN and
                               EBELP = T_EKKO-EBELP and
*                             ( zekkn = t_ekko-zekkn or
                                 VGABE = 1."

            if not T_EKBE-AUFNR is initial.
              if T_EKBE-AUFNR ne T_COAS-AUFNR.
                continue.
              endif.
            endif.

*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
            if P_MOEDA      eq 'BRL' and
               T_EKBE-WAERS ne 'BRL'.
              if not P_HIST is initial.
                V_DATA = T_EKBE-BUDAT.
              else.
                V_DATA = P_DATAC.
              endif.

*            PERFORM f_converte_moeda USING t_ekko-wkurs
*                                  CHANGING t_ekbe-dmbtr.
            elseif P_MOEDA ne 'BRL' and
                   T_EKBE-WAERS ne P_MOEDA.

              if not P_HIST is initial.
                V_DATA = T_EKBE-BUDAT.
              else.
                V_DATA = P_DATAC.
              endif.
              perform F_CONVERTE_COTACAO using T_EKBE-WAERS
                                               V_DATA
                                      changing T_EKBE-DMBTR.
            elseif P_MOEDA ne 'BRL' and
                 T_EKBE-WAERS eq P_MOEDA.
              T_EKBE-DMBTR = T_EKBE-WRBTR.
            endif.
*** Fim da rotina da TAXA

            move:
                  T_EKBE-XBLNR to TW_SAIDA-NFENUM,
                  T_EKBE-MENGE to TW_SAIDA-MENGE,
                  T_EKBE-BUDAT to TW_SAIDA-AEDAT,
                  T_EKBE-DMBTR to TW_SAIDA-NETPR.

            if T_EKBE-VGABE = 1.
              move: T_EKBE-BELNR to TW_SAIDA-BELNR1.",
*                    t_ekbe-dmbtr TO tw_saida-dmbtr.
              clear TW_SAIDA-BELNR2.
              if T_EKBE-BWART = '102'.
                TW_SAIDA-BELNR2 = 'MIGO-EST'.
              elseif T_EKBE-BWART = '122'.
                TW_SAIDA-BELNR2 = 'MIGO-DEV'.
              else.
                TW_SAIDA-BELNR2 = 'MIGO'.
              endif.
            else.
              read table T_RBKP with key BELNR = T_EKBE-BELNR
                                         GJAHR = T_EKBE-GJAHR
                                         binary search.
              if SY-SUBRC = 0.
                TW_SAIDA-NFENUM = T_RBKP-XBLNR .
              endif.

              move: T_EKBE-BELNR to TW_SAIDA-BELNR1,
                    T_EKBE-DMBTR to TW_SAIDA-DMBTR.
              clear: TW_SAIDA-BELNR2."  tw_saida-dmbtr.
              if T_EKBE-VGABE = 2 and
                 T_EKBE-SHKZG = 'H'.
                TW_SAIDA-BELNR2 = 'MIRO-NC'.
              else.
                TW_SAIDA-BELNR2 = 'MIRO'.
              endif.
            endif.

            read table WE_BKPF with key AWKEY = T_EKBE-AWKEY
                                        binary search.
            if SY-SUBRC = 0.
*              MOVE we_bkpf-belnr TO tw_saida-belnr4.
              if T_EKBE-VGABE = 1.

                loop at WE_BKPF where AWKEY eq T_EKBE-AWKEY
                                  and BLART ne 'ML'.

                  move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
                  exit.
                endloop.
**                move: we_bkpf-belnr to tw_saida-belnr3.
*                CLEAR tw_saida-belnr4.
              else.
                move: WE_BKPF-BELNR to TW_SAIDA-BELNR3. "4
*                CLEAR tw_saida-belnr4.
              endif.
            else.
*              CLEAR tw_saida-belnr4.
            endif.

            if T_EKBE-SHKZG = 'H'.
              TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ) .
              TW_SAIDA-MENGE = TW_SAIDA-MENGE * ( - 1 ) .
            endif.
            clear T_TOTAL.
            if not T_EKBE-DMBTR is initial.
              if W_VAL is initial.
*                MOVE t_ekbe-dmbtr TO tw_saida-dmbtr.
                W_VAL = 'X'.
*                MOVE: tw_saida-posnr TO t_total-posnr,
*                      tw_saida-anln1 TO t_total-anln1,
*                      tw_saida-aufnr TO t_total-aufnr,
*                      tw_saida-dmbtr TO t_total-dmbtr.
*                COLLECT t_total. CLEAR t_total.
*                MOVE: tw_saida-posnr   TO t_total-posnr,
*                      tw_saida-txttipo TO t_total-campo,
*                      tw_saida-dmbtr   TO t_total-dmbtr.
*                COLLECT t_total. CLEAR t_total.
*                MOVE: tw_saida-posnr TO t_total-posnr,
*                      tw_saida-anln1 TO t_total-anln1,
*                      tw_saida-aufnr TO t_total-aufnr,
*                      tw_saida-dmbtr TO t_total-dmbtr,
*                      'MM'           TO t_total-campo.
*                COLLECT t_total. CLEAR t_total.
              endif.
            endif.

*Início Alteração Ricardo Furst.
            clear: TW_SAIDA-NETWR.
            if P_ALV is initial.
              clear:   TW_SAIDA-EXTROW, "tw_saida-werks,
                     TW_SAIDA-TXT50, TW_SAIDA-TXTPED, "tw_saida-netpr,
                     TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD,
                     TW_SAIDA-ANLKL, "tw_saida-matnr, tw_saida-txz01,
                     TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR. "tw_saida-menge, tw_saida-meins,
            endif.
*Fim Alteração Ricardo Furst.
* tw_saida-ktext1, , tw_saida-aufnr  tw_saida-txttipo, tw_saida-srvpos, , tw_saida-txtimob

            read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                       binary search.
            if SY-SUBRC = 0.
              concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
                separated by ' - '.
            endif.
            if T_EKBE-VGABE = 1.
              read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                                       binary search.
              if SY-SUBRC = 0.
                concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
                  separated by ' - '.
              endif.
            else.
              read table T_RBKP with key BELNR = T_EKBE-BELNR
                                       GJAHR = T_EKBE-GJAHR
                                       binary search.
              if SY-SUBRC = 0.
                concatenate T_RBKP-NAME1 T_RBKP-LIFNR into TW_SAIDA-EMISSOR
                separated by ' - '.
              endif.
            endif.

            TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).
            TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]

            read table T_161 with key BSART = T_EKKO-BSART
                                      binary search.
            if SY-SUBRC = 0.
              concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
              separated by ' - '.
            else.
              clear TW_SAIDA-TIPOPED.
            endif.
            TW_SAIDA-WAERS = T_EKKO-WAERS.
            TW_SAIDA-WERKS = T_IMAK-WERKS.

            append TW_SAIDA.
*Início Alteração Ricardo Furst.
*            IF p_alv IS INITIAL.
            clear: TW_SAIDA-NFENUM, TW_SAIDA-EMENGE, TW_SAIDA-DMBTR, TW_SAIDA-BELNR2,
                   TW_SAIDA-BELNR1, TW_SAIDA-BELNR3, TW_SAIDA-REAL_AP.
*            ENDIF.
*Fim Alteração Ricardo Furst.
          endloop.
*          CLEAR tw_saida.
*          CLEAR: tw_saida-konnr, tw_saida-ktpnr.
          clear: TW_SAIDA-KTPNR.

          V_BELNR = '2'.

          perform Z_EKBE using V_BELNR changing V_DATA.

          clear: TW_SAIDA-KONNR, TW_SAIDA-KTPNR.

        endloop.

        clear: TW_SAIDA-TIPOPED, TW_SAIDA-SGTXT.
************ Dados - FI
*** 23.09.2006 - Dados de FI
        if P_ALV is initial.
          clear: TW_SAIDA-EXTROW,  TW_SAIDA-TXT50, TW_SAIDA-NETPR, "tw_saida-werks,
                 TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD, TW_SAIDA-EBELP,
                 TW_SAIDA-ANLKL, TW_SAIDA-MATNR, TW_SAIDA-TXZ01, TW_SAIDA-TXTPED,
                 TW_SAIDA-MENGE, TW_SAIDA-MEINS, TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR,
                 TW_SAIDA-SRVPOS, TW_SAIDA-KTEXT1, TW_SAIDA-EXTROW, TW_SAIDA-NFENUM.
        endif.
*        IF s_lifre[] IS INITIAL AND
*           s_lifnr[] IS INITIAL AND
*           s_ebeln[] IS INITIAL.
        move T_COAS-KTEXT to TW_SAIDA-TXT50.
        loop at T_MSEG where AUFNR = T_IMAKZ-AUFNR.

          move: T_MSEG-MBLNR to TW_SAIDA-BELNR1,
                T_MSEG-DMBTR to TW_SAIDA-DMBTR,
                T_MSEG-BUDAT to TW_SAIDA-AEDAT,
                T_MSEG-MATNR to TW_SAIDA-MATNR,
                T_MSEG-MAKTX to TW_SAIDA-TXZ01,
                T_MSEG-TCODE2 to TW_SAIDA-BELNR2,
                T_MSEG-DMBTR to TW_SAIDA-NETPR.
*              t_mseg-dmbe2 TO tw_saida-emenge.
          read table WF_BKPF with key AWKEY = T_MSEG-AWKEY binary search.
          if SY-SUBRC = 0.
            move: WF_BKPF-BELNR to TW_SAIDA-BELNR3.
          endif.

          if TW_SAIDA-BELNR2 = 'MBST'.
            TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ).
          endif.

          if TW_SAIDA-BELNR2 = 'MB1A' and T_MSEG-SHKZG = 'S'.
            TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ).
          endif.

          TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).
          TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]
          clear: TW_SAIDA-EBELN, TW_SAIDA-EBELP.
          TW_SAIDA-WERKS = T_IMAK-WERKS.
          append TW_SAIDA.
          clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE, TW_SAIDA-REAL_AP,
                 TW_SAIDA-NETWR.
        endloop.


*          LOOP AT t_ska1_6.
        move T_COAS-KTEXT to TW_SAIDA-TXT50.
*
        loop at T_BSIS where BUKRS = T_IMAK-ABUKRS  and
*                                 belnr = t_pv-belnr     and
*                                 gjahr = t_pv-gjahr.
                             AUFNR = T_IMAKZ-AUFNR.

*          LOOP AT t_bsis.
*            READ TABLE t_pv WITH KEY bukrs = t_imak-abukrs
*                                     belnr = t_bsis-belnr
*                                     gjahr = t_bsis-gjahr
*                                     anln1 = t_imaka-anln1
*                                     anln2 = t_imaka-anln2.
*            IF sy-subrc <> 0.
*              CONTINUE.
*            ENDIF.
*Início Alteração Ricardo Furst.
          if P_ALV is initial.
            clear:   TW_SAIDA-EXTROW, "tw_saida-werks, "tw_saida-txt50,
                   TW_SAIDA-NETPR, TW_SAIDA-TXTPED, TW_SAIDA-EBELN,
                   TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD, TW_SAIDA-EBELP,
                   TW_SAIDA-ANLKL, TW_SAIDA-MATNR, TW_SAIDA-TXZ01,
                   TW_SAIDA-MENGE, TW_SAIDA-MEINS, TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR,
                   TW_SAIDA-SRVPOS, TW_SAIDA-KTEXT1, TW_SAIDA-EXTROW.
          endif.
*Fim Alteração Ricardo Furst.
*** mudar
*move:                t_ekbe-xblnr TO tw_saida-nfenum,
*                t_ekbe-menge TO tw_saida-emenge,
*                t_ekbe-dmbtr TO tw_saida-dmbtr.

          move: T_BSIS-BELNR to TW_SAIDA-BELNR3,
*                t_bsis-dmbe2 TO tw_saida-emenge,
                T_BSIS-DMBTR to TW_SAIDA-DMBTR,
                T_BSIS-BUDAT to TW_SAIDA-AEDAT,
                T_BSIS-SGTXT to TW_SAIDA-SGTXT,
                T_BSIS-DMBTR to TW_SAIDA-NETPR.

          if P_MOEDA eq 'USD'.
            move: T_BSIS-DMBE2 to TW_SAIDA-DMBTR.
          endif.
*                  w_ebel       TO tw_saida-ebeln.
*             'FI (Financeiro)' to tw_saida-txttipo.


*Início Alteração Ricardo Furst.
          if P_ALV is initial.
            clear TW_SAIDA-BELNR1.
            clear TW_SAIDA-BELNR2.
          endif.


          read table T_BKPF with key  BUKRS = T_BSIS-BUKRS
                                      BELNR = T_BSIS-BELNR
                                      GJAHR = T_BSIS-GJAHR.

          if SY-SUBRC = 0.
            TW_SAIDA-BELNR2 = T_BKPF-TCODE.
          endif.

*Fim Alteração Ricardo Furst.
*          ENDIF.
          TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).
          TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]
          TW_SAIDA-WERKS = T_IMAK-WERKS.


** 01.05.2011
*INICIO alteração IR080294
*                                                              SELECT SINGLE ltext FROM cskt INTO tw_saida-txt050
*                                                                WHERE spras EQ sy-langu AND
*                                                                      kokrs EQ 'MAGI'   AND
*                                                                      kostl EQ tw_saida-objeto(10) AND
*                                                                      datbi GE sy-datum .
          select single LTEXT from CSKT into TW_SAIDA-TXT050
          where SPRAS eq SY-LANGU and
                KOKRS eq V_KOKRS   and
                KOSTL eq TW_SAIDA-OBJETO(10) and
                DATBI ge SY-DATUM .
*Fim alteração IR080294
** 01.05.2011



          append TW_SAIDA.
*          CLEAR tw_saida-sgtxt.
*Início Alteração Ricardo Furst.
          if P_ALV is initial.
            clear: TW_SAIDA-BELNR3, TW_SAIDA-EMENGE, TW_SAIDA-SGTXT, TW_SAIDA-DMBTR, TW_SAIDA-REAL_AP.
          endif.
*Fim Alteração Ricardo Furst.
        endloop.

*          CLEAR tw_saida.
*        ENDLOOP.
************ FIM Dados - FI

        if P_ALV is initial.
          clear:   TW_SAIDA-EXTROW, "tw_saida-werks, "tw_saida-txt50,
                 TW_SAIDA-NETPR, TW_SAIDA-TXTPED, TW_SAIDA-EBELN,
                 TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD, TW_SAIDA-EBELP,
                 TW_SAIDA-ANLKL, TW_SAIDA-MATNR, TW_SAIDA-TXZ01,
                 TW_SAIDA-MENGE, TW_SAIDA-MEINS, TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR,
                 TW_SAIDA-SRVPOS, TW_SAIDA-KTEXT1, TW_SAIDA-EXTROW.
        endif.
        move T_COAS-KTEXT to TW_SAIDA-TXT50.
        loop at T_CICLO where OBJNR = T_IMAKZ-OBJNR.

          if T_CICLO-BEKNZ = 'H'.
            if T_CICLO-WOGBTR > 0.
              T_CICLO-WOGBTR = T_CICLO-WOGBTR * ( - 1 ).
            endif.
            if T_CICLO-WTGBTR > 0.
              T_CICLO-WTGBTR = T_CICLO-WTGBTR * ( - 1 ).
            endif.
          endif.

          if P_MOEDA eq 'USD'.
            T_CICLO-WOGBTR = T_CICLO-WTGBTR.
          endif.

          move: T_CICLO-BELNR  to TW_SAIDA-BELNR3,
                'Ciclo Rateio' to TW_SAIDA-BELNR2,
                T_CICLO-WOGBTR to TW_SAIDA-DMBTR,
                T_CICLO-BUDAT  to TW_SAIDA-AEDAT,
                T_CICLO-BLTXT  to TW_SAIDA-SGTXT,
                T_CICLO-WOGBTR to TW_SAIDA-NETPR.
*                  w_ebel       TO tw_saida-ebeln.
*             'FI (Financeiro)' to tw_saida-txttipo.


*Início Alteração Ricardo Furst.
          if P_ALV is initial.
            clear TW_SAIDA-BELNR1.
*              CLEAR tw_saida-belnr2.
          endif.
*Fim Alteração Ricardo Furst.
*          ENDIF.
          TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).
          TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]
          TW_SAIDA-WERKS = T_IMAK-WERKS.
          append TW_SAIDA.
          clear TW_SAIDA-SGTXT.
          clear: TW_SAIDA-BELNR3, TW_SAIDA-BELNR1, TW_SAIDA-SGTXT, TW_SAIDA-BELNR2.
        endloop.



        if P_CONSOL is initial.
          loop at T_EKKO where AUFNR = T_COAS-AUFNR.
            perform F_MONTA_DADOS_CAIXA_PG.
          endloop.
          loop at T_EKKO where AUFNR = T_COAS-AUFNR.
            perform F_MONTA_DADOS_CAIXA_APG.
          endloop.
        endif.
*        ENDIF.
      endloop.
    endloop.
    if not P_ANALIT is initial.
      perform F_MONTA_DADOS_ANALITICO using ''.
    endif.
  endloop.

*  SORT tw_saida BY posnr txttipo anln1  belnr1 belnr3 ebeln ebelp aufnr txtped  netwr DESCENDING.

endform.                    " F_PREPARA_DADOS
*&---------------------------------------------------------------------*
*&      Form  add_carrid_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 1 to tree
*----------------------------------------------------------------------*
form ADD_POSNR_LINE using      PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: L_NODE_TEXT type LVC_VALUE,
*        ls_saida    TYPE zes_im01.
        LS_SAIDA    type E_SAIDA.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
  LS_ITEM_LAYOUT-T_IMAGE = '@A8@'.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
  LS_ITEM_LAYOUT-STYLE   =
                        CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

* add node
  L_NODE_TEXT =  PS_SAIDA-POSNR.
  shift L_NODE_TEXT left deleting leading '0'.

  read table T_IMAK with key POSNR = PS_SAIDA-POSNR.

  if SY-SUBRC = 0.
    concatenate L_NODE_TEXT T_IMAK-TXT50
          into L_NODE_TEXT separated by SPACE.
  else.
    select single TXT50 from IMAKT into T_IMAK-TXT50
      where SPRAS eq SY-LANGU and POSNR = PS_SAIDA-POSNR.
    if SY-SUBRC = 0.
      concatenate L_NODE_TEXT T_IMAK-TXT50
           into L_NODE_TEXT separated by SPACE.
    endif.
  endif.


  data: LS_NODE type LVC_S_LAYN.
  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.

  LS_SAIDA-POSNR = PS_SAIDA-POSNR.
  LS_SAIDA-BELNR4 = PS_SAIDA-BELNR4.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = LS_SAIDA
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.

endform.                               " add_posnr_line
*&---------------------------------------------------------------------*
*&      Form  add_carrid_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 1 to tree
*----------------------------------------------------------------------*
form ADD_BUKRS_LINE using      PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: L_NODE_TEXT type LVC_VALUE,
*        ls_saida    TYPE zes_im01.
        LS_SAIDA    type E_SAIDA.

  data: W_TXT type T001-BUTXT.
  select single BUTXT from T001 into W_TXT
    where BUKRS = PS_SAIDA-ABUKRS.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
  LS_ITEM_LAYOUT-T_IMAGE = '@A8@'.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
  LS_ITEM_LAYOUT-STYLE   =
                        CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

* add node
  L_NODE_TEXT =  PS_SAIDA-ABUKRS.
  shift L_NODE_TEXT left deleting leading '0'.

  read table T_IMAK with key POSNR = PS_SAIDA-POSNR.
  concatenate L_NODE_TEXT W_TXT
        into L_NODE_TEXT separated by SPACE.

  data: LS_NODE type LVC_S_LAYN.
  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.

  LS_SAIDA-POSNR = PS_SAIDA-POSNR.
  LS_SAIDA-BELNR4 = PS_SAIDA-BELNR4.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = LS_SAIDA
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.

endform.                               " add_bukrs_line
*&---------------------------------------------------------------------*
*&      Form  add_txttipo_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_SAIDA     text
*      -->P_RELAT_KEY  text
*      -->P_NODE_KEY   text
*----------------------------------------------------------------------*
form ADD_TXTTIPO_LINE using    PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: L_NODE_TEXT type LVC_VALUE,
        LS_SAIDA    type E_SAIDA.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
  LS_ITEM_LAYOUT-T_IMAGE = '@3Y@'.
  LS_ITEM_LAYOUT-STYLE   =
                        CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

** retirado
*  READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                              campo = ps_saida-txttipo(30).
*  IF sy-subrc = 0.
*    ls_saida-dmbtr = t_total-dmbtr.
*  ENDIF.
*
*  READ TABLE t_totalr WITH KEY posnr = ps_saida-posnr
*                            campo = ps_saida-txttipo(30).
*  IF sy-subrc = 0.
*    ls_saida-diferenca = t_totalr-dmbtr.
*    SUBTRACT ls_saida-dmbtr FROM ls_saida-diferenca.
*  ENDIF.
** retirado
* add node
  L_NODE_TEXT =  PS_SAIDA-TXTTIPO.
  data: RELAT type INT4.
  RELAT = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = RELAT
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = LS_SAIDA
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.

endform.                               "
*&---------------------------------------------------------------------*
*&      Form  add_anln1_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_SAIDA     text
*      -->P_RELAT_KEY  text
*      -->P_NODE_KEY   text
*----------------------------------------------------------------------*
form ADD_ANLN1_LINE using     PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: W_TEXT(30).

  W_TEXT = PS_SAIDA-ANLN1.
  if W_TEXT is initial.
    W_TEXT = PS_SAIDA-AUFNR.
  endif.

  if PS_SAIDA-AUFNR is initial.
    concatenate W_TEXT PS_SAIDA-ANLN2 into W_TEXT separated by '-'.
  endif.

  shift W_TEXT left deleting leading '0'.
  data: L_NODE_TEXT type LVC_VALUE,
        LS_SAIDA    type E_SAIDA.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
  LS_ITEM_LAYOUT-T_IMAGE = '@3Y@'.
  LS_ITEM_LAYOUT-STYLE   =
                        CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

* add node
*  l_node_text =  ps_saida-txtimob.
*  CONCATENATE ps_saida-txtimob w_text INTO l_node_text
**eduvoltar
  if PS_SAIDA-ANLN1 = 'PLANEJADO' or
    PS_SAIDA-ANLN1 = 'EXTRA'.
    L_NODE_TEXT = PS_SAIDA-TXT50.
  else.
    if not PS_SAIDA-TXT50 is initial.
      concatenate W_TEXT PS_SAIDA-TXT50
      into L_NODE_TEXT separated by '-'.
    else.
      concatenate W_TEXT PS_SAIDA-TXT050
      into L_NODE_TEXT separated by '-'.
    endif.
  endif.
*  READ TABLE t_total WITH KEY campo = ps_saida-anln1.
*  IF sy-subrc = 0.
*    ls_saida-dmbtr = t_total-dmbtr.
*  ENDIF.

** retirado
*  CLEAR ls_saida-dmbtr.
*  READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                              anln1 = ps_saida-anln1.
*  IF sy-subrc = 0.
*    ls_saida-dmbtr = t_total-dmbtr.
*  ELSE.
*    READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                                aufnr = ps_saida-aufnr.
*    IF sy-subrc = 0.
*      ls_saida-dmbtr = t_total-dmbtr.
*    ENDIF.
*  ENDIF.
*
*
*
*  CLEAR ls_saida-diferenca.
*  READ TABLE t_totalr WITH KEY posnr = ps_saida-posnr
*                               anln1 = ps_saida-anln1.
*  IF sy-subrc = 0.
*    ls_saida-diferenca = t_totalr-dmbtr.
*    SUBTRACT ls_saida-dmbtr FROM ls_saida-diferenca.
*  ELSE.
*    READ TABLE t_totalr WITH KEY posnr = ps_saida-posnr
*                                 aufnr = ps_saida-aufnr.
*    IF sy-subrc = 0.
*      ls_saida-diferenca = t_totalr-dmbtr.
*      SUBTRACT ls_saida-dmbtr FROM ls_saida-diferenca.
*    ENDIF.
*  ENDIF.
** retirado
  if not PS_SAIDA-AUFNR is initial.
    clear LS_SAIDA-ANLN1.
    LS_SAIDA-AUFNR = PS_SAIDA-AUFNR.
  else.
    clear LS_SAIDA-AUFNR.
    LS_SAIDA-ANLN1 = PS_SAIDA-ANLN1.
  endif.


  data: RELAT type INT4.
  RELAT = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = RELAT
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = LS_SAIDA
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.


endform.                               " add_connid_line
*&---------------------------------------------------------------------*
*&      Form  add_complete_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_SAIDA     text
*      -->P_RELAT_KEY  text
*      -->P_NODE_KEY   text
*----------------------------------------------------------------------*
form ADD_COMPLETE_LINE using   PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: L_NODE_TEXT type LVC_VALUE.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
*  ls_item_layout-class   = cl_gui_column_tree=>item_class_checkbox.
*  ls_item_layout-editable = 'X'.
  if ( ( not PS_SAIDA-BELNR3 is initial and
             PS_SAIDA-BELNR1 is initial        ) or
     (       PS_SAIDA-BELNR2 = 'MB1A' or
             PS_SAIDA-BELNR2 = 'MBST'     ) ).

    if PS_SAIDA-BELNR2 = 'MBST'.
      LS_ITEM_LAYOUT-T_IMAGE = '@11@'.
    else.
      LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
    endif.
  else.
    LS_ITEM_LAYOUT-T_IMAGE = '@96@'.
  endif.

  case PS_SAIDA-BELNR2.
    when 'MIGO'.     LS_ITEM_LAYOUT-T_IMAGE = '@PG@'.
    when 'MIGO-EST'. LS_ITEM_LAYOUT-T_IMAGE = '@11@'.
    when 'MIGO-DEV'. LS_ITEM_LAYOUT-T_IMAGE = '@2W@'.
    when 'MIRO'.     LS_ITEM_LAYOUT-T_IMAGE = '@BE@'.
  endcase.

  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.



*  CLEAR ls_item_layout.
*  ls_item_layout-fieldname = 'PLANETYPE'.
*  ls_item_layout-alignment = cl_gui_column_tree=>align_right.
*  APPEND ls_item_layout TO lt_item_layout.
  if PS_SAIDA-ANLN1 = 'PLANEJADO' or
    PS_SAIDA-ANLN1 = 'EXTRA'.
*    CONCATENATE " ps_saida-anln1'Item' ps_saida-ebelp ps_saida-txz01 INTO l_node_text SEPARATED BY ' - '.
*    CONCATENATE  ps_saida-ebelp ps_saida-anln1 ps_saida-txz01 INTO l_node_text SEPARATED BY ' - '.
    L_NODE_TEXT = PS_SAIDA-TXZ01 .
  else.
*    IF NOT ps_saida-belnr3 IS INITIAL AND
*           ps_saida-belnr1 IS INITIAL .
    if ( ( not PS_SAIDA-BELNR3 is initial and
               PS_SAIDA-BELNR1 is initial        ) or
       (       PS_SAIDA-BELNR2 = 'MB1A' or
               PS_SAIDA-BELNR2 = 'MBST'        ) ).
      concatenate 'Doc. Contábil -' PS_SAIDA-BELNR3
      into L_NODE_TEXT separated by SPACE.
    else.
      if P_SINCAX is initial.
        concatenate PS_SAIDA-EBELN PS_SAIDA-EBELP
           into L_NODE_TEXT separated by ' - '.

      else.
        move: PS_SAIDA-EBELN to L_NODE_TEXT.

      endif.
    endif.

    if PS_SAIDA-BELNR2 = 'PEDIDO'.
      concatenate PS_SAIDA-EBELN PS_SAIDA-EBELP
       into L_NODE_TEXT separated by ' - '.

    endif.

  endif.
*  l_node_text =  ps_saida-ebeln.
*  l_node_text = 'MM-Compra'.
  data: LS_NODE type LVC_S_LAYN.
  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.

  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      IS_OUTTAB_LINE   = PS_SAIDA
      I_NODE_TEXT      = L_NODE_TEXT
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.
endform.                               " add_complete_line
*&---------------------------------------------------------------------*
*&      Form  add_ebeln_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_SAIDA     text
*      -->P_RELAT_KEY  text
*      -->P_NODE_KEY   text
*----------------------------------------------------------------------*
form ADD_EBELN_LINE using     PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: L_NODE_TEXT type LVC_VALUE,
        LS_SAIDA    type E_SAIDA.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
  if ( PS_SAIDA-BELNR3 is initial and
      PS_SAIDA-BELNR2 ne 'MIRO'       )." or
*     ps_saida-belnr2 ne 'MIRO'       .
    LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
  else.
    LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.
  endif.


  if PS_SAIDA-BELNR2 = 'MB1A'.
    LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
  endif.
  LS_ITEM_LAYOUT-STYLE   =
                        CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.


* add node
  if P_SINCAX is initial.
    if not PS_SAIDA-BELNR3 is initial and
           PS_SAIDA-BELNR2 ne 'MIRO'  .
      if PS_SAIDA-BELNR2(2) = 'FI'.
        L_NODE_TEXT = 'FI (Financeiro)'.
        LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
      elseif PS_SAIDA-BELNR2(2) = 'CI'.
        L_NODE_TEXT = 'FI (Contábil)'.
        LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
      else.
        L_NODE_TEXT = 'FI (Contábil)'.
        LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
      endif.

*       l_node_text = 'FI (Contábil)'.
    else.
      L_NODE_TEXT = 'MM-Compras'.
      LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.
*    READ TABLE t_total WITH KEY campo = ps_saida-ebeln.
*    IF sy-subrc = 0.
*      ls_saida-dmbtr = t_total-dmbtr.
*    ENDIF.
    endif.

  else.
    if PS_SAIDA-BELNR2 ne 'MIRO'  .
      if PS_SAIDA-BELNR2(2) = 'FI'.
        L_NODE_TEXT = 'FI (Financeiro)'.
        LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
      elseif PS_SAIDA-BELNR2(2) = 'CI'.
        L_NODE_TEXT = 'FI (Contábil)'.
        LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
      else.
        L_NODE_TEXT = 'FI (Contábil)'.
        LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
      endif.
    else.
      L_NODE_TEXT = 'MM-Compras'.
      LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.
*    READ TABLE t_total WITH KEY campo = ps_saida-ebeln.
*    IF sy-subrc = 0.
*      ls_saida-dmbtr = t_total-dmbtr.
*    ENDIF.
    endif.
  endif.
  if PS_SAIDA-BELNR2 eq 'MIGO' or
     PS_SAIDA-BELNR2 eq 'PEDIDO' .
    L_NODE_TEXT = 'MM-Compras'.
    LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.
  endif.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

  if PS_SAIDA-BELNR2 = 'MB1A'.
    L_NODE_TEXT = 'FI (Contábil)'.
  endif.

** retirado
*  READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                              anln1 = ps_saida-anln1
*                              campo = l_node_text(2).
*  IF sy-subrc = 0.
*    ADD t_total-dmbtr TO ls_saida-dmbtr.
*  ELSE.
*    READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                                aufnr = ps_saida-aufnr
*                                campo = l_node_text(2).
*    IF sy-subrc = 0.
*      ADD t_total-dmbtr TO ls_saida-dmbtr.
*    ENDIF.
*  ENDIF.
*
*  READ TABLE t_totalr WITH KEY posnr = ps_saida-posnr
*                              anln1 = ps_saida-anln1
*                              campo = l_node_text(2).
*  IF sy-subrc = 0.
*    ADD t_totalr-dmbtr TO ls_saida-diferenca.
*  ELSE.
*
*    READ TABLE t_totalr WITH KEY posnr = ps_saida-posnr
*                                aufnr = ps_saida-aufnr
*                                campo = l_node_text(2).
*    IF sy-subrc = 0.
*      ADD t_totalr-dmbtr TO ls_saida-diferenca.
*    ENDIF.
*  ENDIF.
*
*  IF ls_saida-diferenca IS INITIAL.
*    IF ls_saida-dmbtr < 0.
**  SUBTRACT ls_saida-dmbtr FROM ls_saida-diferenca.
*      ls_saida-diferenca = ls_saida-dmbtr.
*    ELSE.
*      SUBTRACT ls_saida-dmbtr FROM ls_saida-diferenca.
*    ENDIF.
*  ELSE.
*    SUBTRACT ls_saida-dmbtr FROM ls_saida-diferenca.
*  ENDIF.
** retirado


*  l_node_text =  w_pasta.
  if L_NODE_TEXT is initial.
    L_NODE_TEXT = 'Ordem'.
  endif.
  data: RELAT type INT4.
  RELAT = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = RELAT
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = LS_SAIDA
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.

endform.                               " add_connid_line
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_LAYOUT_TELA
*&---------------------------------------------------------------------*
form F_MODIFICA_LAYOUT_TELA .

  data: LS_FIELDCATALOG type LVC_S_FCAT.
  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.

    case LS_FIELDCATALOG-FIELDNAME.
      when 'NETWR' .
        LS_FIELDCATALOG-COL_POS = 1.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido'.
      when 'DMBTR'.
        LS_FIELDCATALOG-COL_POS = 2.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
        LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
        LS_FIELDCATALOG-COLTEXT = 'Realizado'.
*        ls_fieldcatalog-col_pos   = 18.
      when 'DIFERENCA'.
        LS_FIELDCATALOG-COL_POS = 3.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido (-) Realizado'.

      when others.
        LS_FIELDCATALOG-NO_OUT = 'X'.
        LS_FIELDCATALOG-KEY    = ''.
    endcase.

    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.

endform.                    " F_MODIFICA_LAYOUT_TELA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_FI
*&---------------------------------------------------------------------*
form F_BUSCA_DADOS_FI .

  data: begin of T_MKPF occurs 0,
          MBLNR  like MKPF-MBLNR,
          MJAHR  like MKPF-MJAHR,
          TCODE2 like MKPF-TCODE2,
          BUDAT  like MKPF-BUDAT,
        end of T_MKPF.

  types: TY_RG_MATNR type range of MAKT-MATNR.

  sort T_IMAKZ by AUFNR.

  perform F_MSG using 'Selecionando dados FI (MKPF)'
                      '23'.
**  igor vilela
  select MBLNR MJAHR TCODE2 BUDAT
      from MKPF into table T_MKPF
    where BUDAT in S_BUDAT and
          MJAHR in S_BUDAT and
          VGART ne 'WE' order by MBLNR MJAHR ascending.
** igor vilela
***  SORT t_mkpf BY mblnr mjahr.

  if SY-SUBRC = 0.
    perform F_MSG using 'Selecionando dados FI (MSEG)'
                        '24'.
    select MBLNR MJAHR ZEILE ANLN1 ANLN2 AUFNR DMBTR MATNR SHKZG
      from MSEG into table T_MSEG
      for all entries in T_MKPF
      where MBLNR = T_MKPF-MBLNR and
            MJAHR = T_MKPF-MJAHR.

    delete T_MSEG where ANLN1 is initial and
                        AUFNR is initial.

    data: begin of T_MAKT occurs 0,
            MATNR like MAKT-MATNR,
            MAKTX like MAKT-MAKTX,
          end of T_MAKT.
    if ( T_MSEG[] is not initial ).

      data(RG_MATNR) = value TY_RG_MATNR( for LWA_MSEG in T_MSEG[] (
          SIGN   = 'I' OPTION = 'EQ' LOW = LWA_MSEG-MATNR HIGH = LWA_MSEG-MATNR ) ).
      sort RG_MATNR[] by LOW ascending.
      delete adjacent duplicates from RG_MATNR[] comparing LOW.

      select MATNR MAKTX from MAKT into table T_MAKT
***        FOR ALL ENTRIES IN t_mseg
        where MATNR in RG_MATNR[] and "= t_mseg-matnr AND
              SPRAS = SY-LANGU
      order by MATNR ascending.

***      SORT t_makt BY matnr.
    endif.

    loop at T_MSEG.
      read table T_MKPF with key MBLNR = T_MSEG-MBLNR
                                 MJAHR = T_MSEG-MJAHR
                                 binary search.
      move: T_MKPF-TCODE2 to T_MSEG-TCODE2,
            T_MKPF-BUDAT  to T_MSEG-BUDAT.
      concatenate T_MSEG-MBLNR T_MSEG-MJAHR into T_MSEG-AWKEY.
      read table T_MAKT with key MATNR = T_MSEG-MATNR
                                 binary search.
      if SY-SUBRC = 0.
        move T_MAKT-MAKTX to T_MSEG-MAKTX.
      endif.
      modify T_MSEG.

    endloop.
    perform F_MSG using 'Selecionando dados FI (BKPF)'
                        '25'.
    select BUKRS BELNR GJAHR AWKEY from BKPF into table WF_BKPF
  for all entries in T_MSEG
  where AWTYP in ('MKPF', 'RMRP') and
        AWKEY eq T_MSEG-AWKEY     and
        AWSYS eq SPACE            and
        BUKRS in S_BUKRS          and
        GJAHR eq T_MSEG-MJAHR.
    sort WF_BKPF by AWKEY.

  endif.


  select KTOPL SAKNR KTOKS from SKA1 "#EC CI_DB_OPERATION_OK[2389136]
    into table T_SKA1 "#EC CI_DB_OPERATION_OK[2431747]
    where KTOPL eq '0050' and
          KTOKS in ('YB06', 'YB03').

  T_SKA1_6[] = T_SKA1_3[] = T_SKA1[].
  delete T_SKA1_6 where KTOKS ne 'YB06'.
  delete T_SKA1_3 where KTOKS ne 'YB03'.

  if not T_SKA1_3[] is initial.
    perform F_MSG using 'Selecionando dados FI (SKB1)'
                        '25'.
    select BUKRS SAKNR from SKB1 into table T_SKB1 "#EC CI_DB_OPERATION_OK[2431747]
           for all entries in T_SKA1_3
           where BUKRS in S_BUKRS       and
                 SAKNR = T_SKA1_3-SAKNR and
                 MITKZ = 'A'.
    sort T_SKB1 by SAKNR.
*---> 05/07/2023 - Migração S4 - DL
    SORT T_SKB1 BY SAKNR.
    SORT T_SKA1 BY SAKNR KTOPL.
*<--- 05/07/2023 - Migração S4 - DL
    loop at T_SKA1_3.
      read table T_SKB1 with key SAKNR = T_SKA1_3-SAKNR
                                 binary search.
      if SY-SUBRC <> 0.
        delete T_SKA1_3.
        read table T_SKA1 with key KTOPL = T_SKA1_3-KTOPL
                                   SAKNR = T_SKA1_3-SAKNR
                                   binary search.
        delete T_SKA1 index SY-TABIX.
      endif.
    endloop.
  endif.




  perform F_MSG using 'Selecionando dados FI (BSIS)'
                      '26'.

*  CHECK NOT t_imakz[] IS INITIAL.
  if not T_IMAKZ[] is initial.
    ranges: R_AUFNR for COAS-AUFNR.
*  SELECT bukrs anln1 anln2 gjahr lnran afabe zujhr zucod belnr budat
*    FROM anekpv INTO TABLE t_pv
*    FOR ALL ENTRIES IN t_imaka
*    WHERE bukrs IN s_bukrs AND
*          anln1 = t_imaka-anln1 AND
*          anln2 = t_imaka-anln2 AND
*          budat IN s_budat.
*
*
*  CHECK sy-subrc IS INITIAL.
*  SELECT bukrs hkont augdt augbl
*         zuonr gjahr belnr buzei
*         dmbtr dmbe2 aufnr shkzg budat sgtxt
*  FROM bsis INTO TABLE t_bsis
**    FOR ALL ENTRIES IN t_ska1
*    FOR ALL ENTRIES IN t_pv
*    WHERE bukrs EQ t_pv-bukrs   AND
*          belnr EQ t_pv-belnr   AND
**          hkont EQ t_ska1-saknr AND
**          budat IN s_budat      AND
*          budat = t_pv-budat AND
*          gjahr = t_pv-gjahr.


    loop at T_IMAKZ.
      R_AUFNR-SIGN = 'I'.
      R_AUFNR-OPTION = 'EQ'.
      R_AUFNR-LOW = T_IMAKZ-AUFNR.
      collect R_AUFNR. clear R_AUFNR.
    endloop.

    select BUKRS HKONT AUGDT AUGBL
           ZUONR GJAHR BELNR BUZEI
           DMBTR DMBE2 AUFNR SHKZG BUDAT SGTXT
    from BSIS into table T_BSIS
      for all entries in T_SKA1
**    FOR ALL ENTRIES IN t_pv
**    WHERE bukrs EQ t_pv-bukrs   AND
**          belnr EQ t_pv-belnr   AND
*    where bukrs in s_bukrs and
*          hkont EQ t_ska1-saknr AND
*          budat IN s_budat      AND
*          gjahr in s_budat      and
*          aufnr in r_aufnr.
**          budat = t_pv-budat AND
**          gjahr = t_pv-gjahr.
      where HKONT eq T_SKA1-SAKNR and
            BUKRS in S_BUKRS and
            BUDAT in S_BUDAT      and
            GJAHR in S_BUDAT      and
            AUFNR in R_AUFNR.
*          budat = t_pv-budat AND
*          gjahr = t_pv-gjahr.

  endif.
*  BREAK-POINT.
  if not T_IMAKA[] is initial.

    data: begin of T_ANEP occurs 0.
            include structure ANEP.
    data: end of T_ANEP.

    select * from ANEP into table T_ANEP
      for all entries in T_IMAKA
      where BUKRS eq T_IMAKA-BUKRS and
            ANLN1 eq T_IMAKA-ANLN1 and
            ANLN2 eq T_IMAKA-ANLN2 and
*            bzdat IN s_budat       AND
            GJAHR in S_BUDAT          .
    if SY-SUBRC = 0.
      select BUKRS HKONT AUGDT AUGBL
           ZUONR GJAHR BELNR BUZEI
           DMBTR DMBE2 AUFNR SHKZG BUDAT SGTXT
    from BSIS appending table T_BSIS
      for all entries in T_ANEP
      where BUKRS eq T_ANEP-BUKRS and
*            budat EQ t_anep-bzdat AND
            GJAHR eq T_ANEP-GJAHR and
            BELNR eq T_ANEP-BELNR." AND
*            buzei EQ t_anep-buzei.

    endif.
  endif.


  sort: T_SKA1_6 by SAKNR,
        T_SKA1_3 by SAKNR.

  check not T_BSIS[] is initial.

  select BUKRS BELNR GJAHR TCODE
         from BKPF into table T_BKPF
     for all entries in T_BSIS
    where BUKRS = T_BSIS-BUKRS and
          BELNR = T_BSIS-BELNR and
          GJAHR = T_BSIS-GJAHR and
          ( AWTYP <> 'MKPF'  and
            AWTYP <> 'RMRP'      ).
  if SY-SUBRC = 0.
    sort T_BKPF by BUKRS BELNR GJAHR.
    loop at T_BSIS.
      if T_BSIS-SHKZG = 'H'.
        T_BSIS-DMBTR = T_BSIS-DMBTR * ( - 1 ).
        T_BSIS-DMBE2 = T_BSIS-DMBE2 * ( - 1 ).
      endif.
      read table T_BKPF with key BUKRS = T_BSIS-BUKRS
                                 BELNR = T_BSIS-BELNR
                                 GJAHR = T_BSIS-GJAHR
                                 binary search.
      if SY-SUBRC <> 0.
        delete T_BSIS.
      else.
        modify T_BSIS.
      endif.
    endloop.

    loop at T_BSIS.
      read table T_SKA1_6 with key SAKNR = T_BSIS-HKONT
                                   binary search.
      if SY-SUBRC = 0.
        read table T_IMAKZ with key AUFNR = T_BSIS-AUFNR
                                    binary search.
        if SY-SUBRC = 0.
          move-corresponding T_BSIS to T_BSIS_6.
          append T_BSIS_6. clear T_BSIS_6.
        endif.
      else.
        move-corresponding T_BSIS to T_BSIS_3.
        append T_BSIS_3. clear T_BSIS_3.
      endif.
    endloop.
* ---> S4 Migration - 15/06/2023 - MA
*    select BUKRS BELNR GJAHR ANLN1
*           ANLN2 DMBTR DMBE2 BUZEI
*           SHKZG ANBWA HKONT SGTXT UMSKS
*      from BSEG into table T_BSEG
*      for all entries in T_BSIS
*      where BUKRS = T_BSIS-BUKRS and
*            BELNR = T_BSIS-BELNR and
*            GJAHR = T_BSIS-GJAHR and
*            BUZEI = T_BSIS-BUZEI and
*            ANLN1 ne SPACE       and
*          ( ANBWA ne 'LMD' or ANBWA ne 'LMC'
*          or ANBWA ne '210' or ANBWA ne '180' or
*             ANBWA ne '290' ).

    data LT_FIELDS type FAGL_T_FIELD.
    data: LT_BSEG TYPE FAGL_T_BSEG.

    LT_FIELDS = value #( ( LINE = 'BUKRS' )
                         ( LINE = 'BELNR' )
                         ( LINE = 'GJAHR' )
                         ( LINE = 'ANLN1' )
                         ( LINE = 'ANLN2' )
                         ( LINE = 'DMBTR' )
                         ( LINE = 'DMBE2' )
                         ( LINE = 'BUZEI' )
                         ( LINE = 'SHKZG' )
                         ( LINE = 'ANBWA' )
                         ( LINE = 'HKONT' )
                         ( LINE = 'SGTXT' )
                         ( LINE = 'UMSKS' ) ).


    call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      exporting
        IT_FOR_ALL_ENTRIES = T_BSIS
        I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND BUZEI = IT_FOR_ALL_ENTRIES-BUZEI|
        IT_FIELDLIST       = LT_FIELDS
      importing
        ET_BSEG            = LT_BSEG
      exceptions
        NOT_FOUND          = 1.

    delete LT_BSEG where ANLN1 eq SPACE  and
    ( ANBWA eq 'LMD' or ANBWA eq 'LMC'
    or ANBWA eq '210' or ANBWA eq '180' or
       ANBWA eq '290' ).

    if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
      move-corresponding LT_BSEG to T_BSEG[].
      SY-DBCNT = LINES( LT_BSEG ).
    else.
      SY-SUBRC = 4.
      SY-DBCNT = 0.
    endif.
* <--- S4 Migration - 15/06/2023 - MA



    loop at T_BSEG.
      read table T_IMAKA with key ANLN1 = T_BSEG-ANLN1
                                  ANLN2 = T_BSEG-ANLN2
                                  binary search.
      if SY-SUBRC <> 0.
        delete T_BSEG.
      endif.

    endloop.
  else.
    refresh T_BSIS.
  endif.

*Selecionar na BKPF-BUKRS=BSIS-BUKRS e BKPF-BELNR=BSIS-BELNR e BKPF-GJHAR=BSIS-GJHAR
*e excluir os registros onde forem iguais BKPF-AWTYP= MKPF e RMRP
endform.                    " F_BUSCA_DADOS_FI
*&---------------------------------------------------------------------*
*&      Form  F_CONVERTE_MOEDA
*&---------------------------------------------------------------------*
form F_CONVERTE_MOEDA    using P_FAT
                      changing P_VALOR.

  P_VALOR = P_VALOR * P_FAT.

endform.                    " F_CONVERTE_MOEDA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_COTACAO
*&---------------------------------------------------------------------*
form F_BUSCA_COTACAO .

*  DATA: BEGIN OF t_tcurr OCCURS 0.
*          INCLUDE STRUCTURE tcurr.
*  DATA: END OF t_tcurr.

  check P_MOEDA <> 'BRL'.
  check not P_DATAC is initial.

  if not P_TAXAC is initial.
    W_FATOR = P_TAXAC.
    exit.
  endif.

  concatenate P_DATAC+6(2) P_DATAC+4(2) P_DATAC(4)
              into  W_GDATU.

  call function 'CONVERSION_EXIT_INVDT_INPUT'
    exporting
      INPUT  = W_GDATU
    importing
      OUTPUT = W_GDATU.



  select single UKURS from TCURR into W_FATOR
    where KURST = 'B'     and
          FCURR = P_MOEDA and
          TCURR = 'BRL'   and
          GDATU = W_GDATU.

  if SY-SUBRC ne 0.
    message I000(Z01) with 'Para está data não tem cotação cadastrada.'.
    stop.
  endif.

endform.                    " F_BUSCA_COTACAO
*&---------------------------------------------------------------------*
*&      Form  F_CONVERTE_COTACAO
*&---------------------------------------------------------------------*
form F_CONVERTE_COTACAO    using PU_MOEDA
                                 PU_DATA
                        changing P_VALOR.

  data: WL_FATOR type TCURR-UKURS,
        WL_DATA  type SY-DATUM.

  check P_VALOR > 0.

* Se tiver taxa na tela, usar para calculo!
  if not P_TAXAC is initial.
    P_VALOR = P_VALOR / P_TAXAC.
  endif.

  check P_TAXAC is initial.

  WL_DATA = PU_DATA.

  call function 'CONVERT_TO_LOCAL_CURRENCY'
    exporting
*     CLIENT           = SY-MANDT
      DATE             = PU_DATA
      FOREIGN_AMOUNT   = P_VALOR
      FOREIGN_CURRENCY = PU_MOEDA
      LOCAL_CURRENCY   = P_MOEDA
      RATE             = 0
      TYPE_OF_RATE     = 'B'
      READ_TCURR       = 'X'
    importing
      EXCHANGE_RATE    = WL_FATOR
    exceptions
      NO_RATE_FOUND    = 1
      OVERFLOW         = 2
      NO_FACTORS_FOUND = 3
      NO_SPREAD_FOUND  = 4
      DERIVED_2_TIMES  = 5
      others           = 6.
  if SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  else.
    if WL_FATOR < 0 .
      WL_FATOR = WL_FATOR * ( - 1 ) .
    endif.
    if P_MOEDA <> 'BRL'.
      P_VALOR = P_VALOR / WL_FATOR.
    else.
      P_VALOR = P_VALOR * WL_FATOR.
    endif.
  endif.
endform.                    " F_CONVERTE_COTACAO
*&---------------------------------------------------------------------*
*&      Form  add_pedido_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_SAIDA     text
*      -->P_RELAT_KEY  text
*      -->P_NODE_KEY   text
*----------------------------------------------------------------------*
form ADD_PEDIDO_LINE using     PS_SAIDA type E_SAIDA
                               P_RELAT_KEY type LVC_NKEY
                     changing  P_NODE_KEY type LVC_NKEY.

  data: L_NODE_TEXT type LVC_VALUE,
        LS_SAIDA    type E_SAIDA.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.
*  IF ps_saida-belnr3 IS INITIAL.
  if (  PS_SAIDA-BELNR3 is initial and
        PS_SAIDA-BELNR2 ne 'MIRO'       )."
    LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
  else.
    LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.

  endif.

  if PS_SAIDA-BELNR2 = 'MB1A'.
    LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
  endif.



  LS_ITEM_LAYOUT-STYLE   =
                        CL_GUI_COLUMN_TREE=>STYLE_INTENSIFIED.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.


* add node
  if (  PS_SAIDA-BELNR3 is initial and
        PS_SAIDA-BELNR2 ne 'MIRO'       )."

    L_NODE_TEXT = 'MM-Compras'.
    LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.

  else.
    L_NODE_TEXT = 'FI (Financeiro)'.
    LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
*    READ TABLE t_total WITH KEY campo = ps_saida-ebeln.
*    IF sy-subrc = 0.
*      ls_saida-dmbtr = t_total-dmbtr.
*    ENDIF.
  endif.
  if PS_SAIDA-BELNR2 eq 'PEDIDO' or
    PS_SAIDA-BELNR2  eq 'MIGO'.
    L_NODE_TEXT = 'MM-Compras'.
    LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.
  endif.

  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.
  if PS_SAIDA-BELNR2 = 'FIPA'.
    L_NODE_TEXT = 'Documentos Pagos'.
  elseif PS_SAIDA-BELNR2 = 'FIAP'.
    L_NODE_TEXT = 'Documentos a Pagar'.
  else.
    concatenate 'Pedido de Compras' PS_SAIDA-EBELN
          into L_NODE_TEXT separated by ' - '.
  endif.

  if PS_SAIDA-BELNR2 = 'MB1A'.
    L_NODE_TEXT = 'FI (Contábil)'.
    LS_ITEM_LAYOUT-T_IMAGE = '@FD@'.
  endif.

  if L_NODE_TEXT(6) = 'Pedido'.
    LS_ITEM_LAYOUT-T_IMAGE = '@K4@'.
  endif.

  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

** retirado
*  READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                              anln1 = ps_saida-anln1
*                              campo = l_node_text(2).
*  IF sy-subrc = 0.
*    ADD t_total-dmbtr TO ls_saida-dmbtr.
*  ELSE.
*    READ TABLE t_total WITH KEY posnr = ps_saida-posnr
*                                aufnr = ps_saida-aufnr
*                                campo = l_node_text(2).
*    IF sy-subrc = 0.
*      ADD t_total-dmbtr TO ls_saida-dmbtr.
*    ENDIF.
*  ENDIF.
*
*  READ TABLE t_totalr WITH KEY posnr = ps_saida-posnr
*                              anln1 = ps_saida-anln1
*                              campo = l_node_text(2).
** retirado
*  l_node_text =  ps_saida-ebeln.

*  IF l_node_text IS INITIAL.
*    l_node_text = 'Ordem'.
*  ENDIF.
  data: RELAT type INT4.
  RELAT = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = RELAT
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = LS_SAIDA
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.

endform.                               " add_connid_line
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_IMOBILIZADOS
*&---------------------------------------------------------------------*
form F_BUSCA_IMOBILIZADOS .

*** Busca Imobilizado
  select IMAKA~POSNR IMAKA~LFDNR IMAKA~BUKRS
         IMAKA~ANLN1  IMAKA~ANLN2 "anla~anlkl
                                                            "anla~txt50
    from IMAKA "INNER JOIN anla ON
*    ( imaka~bukrs EQ anla~bukrs AND
*      imaka~anln1 EQ anla~anln1 )"AND
*      imaka~anln2 EQ anla~anln2     )
    into table T_IMAKA
    for all entries in T_IMAK
    where IMAKA~POSNR eq T_IMAK-POSNR and
          IMAKA~BUKRS eq T_IMAK-ABUKRS and
          IMAKA~ANLN1 in S_ANLN1.

  if SY-SUBRC = 0.
    data: begin of T_ANLA occurs 0,
            BUKRS like ANLA-BUKRS,
            ANLN1 like ANLA-ANLN1,
            ANLN2 like ANLA-ANLN2,
            ANLKL like ANLA-ANLKL,
            TXT50 like ANLA-TXT50,
          end of T_ANLA.

    select BUKRS ANLN1 ANLN2 ANLKL TXT50 from ANLA
      into table T_ANLA
      for all entries in T_IMAKA
      where BUKRS = T_IMAKA-BUKRS and
            ANLN1 = T_IMAKA-ANLN1 and
            ANLN2 = T_IMAKA-ANLN2.

    if SY-SUBRC <> 0.
      refresh T_IMAKA.
    else.
      sort T_ANLA by BUKRS ANLN1 ANLN2.
      loop at T_IMAKA.
        read table T_ANLA with key BUKRS = T_IMAKA-BUKRS
                                   ANLN1 = T_IMAKA-ANLN1
                                   ANLN2 = T_IMAKA-ANLN2
                                   binary search.
        if SY-SUBRC <> 0.
          delete T_IMAKA.
        else.
          move-corresponding T_ANLA to T_IMAKA.
          modify T_IMAKA.
        endif.
      endloop.
    endif.

  endif.


  if not S_ANLN1[] is initial and
     not SY-SUBRC is initial.
    message I000(Z01) with 'Solicitação de Investimento não encontrada'
                           ' para Imobilizado informada'.
  endif.
  sort T_IMAKA by ANLN1 ANLN2.

endform.                    " F_BUSCA_IMOBILIZADOS
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_ORDEM_INT_EST
*&---------------------------------------------------------------------*
form F_BUSCA_ORDEM_INT_EST .
*** Busca Ordem Interna/Estatistica
  select * from IMAKZ into table T_IMAKZ
    for all entries in T_IMAK
    where POSNR eq T_IMAK-POSNR.

  if SY-SUBRC = 0.
    loop at T_IMAKZ.
      T_IMAKZ-AUFNR = T_IMAKZ-OBJNR+2.
      modify T_IMAKZ.
    endloop.
  endif.

* Se for informada a ORDEM eliminar a nao desejada
  if not S_AUFNR[] is initial.
    delete T_IMAKZ where not AUFNR in S_AUFNR.
    if T_IMAKZ[] is initial.
      message I000(Z01) with 'Solicitação de Investimento não encontrada'
                           'para Ordem Interna/Estatistica informada'.
      stop.
    endif.
    select AUFNR  BUKRS OBJNR KTEXT AUART KOSTV from COAS
      into table T_COAS
      for all entries in T_IMAKZ
      where AUFNR eq T_IMAKZ-AUFNR.

    data: begin of T_COBRB occurs 0,
            OBJNR like COBRB-OBJNR,
            ANLN1 like COBRB-ANLN1,
          end of T_COBRB.

    select OBJNR ANLN1 from COBRB into table T_COBRB
      for all entries in T_IMAKZ
      where OBJNR = T_IMAKZ-OBJNR.
    sort T_COBRB by OBJNR.


    sort T_COAS by AUFNR.
    loop at T_IMAKZ.

      read table T_COBRB with key OBJNR = T_IMAKZ-OBJNR
                                  binary search.
      if SY-SUBRC = 0.
        T_IMAKZ-ANLN1 = T_COBRB-ANLN1.
        modify T_IMAKZ.
      endif.

      read table T_COAS with key AUFNR = T_IMAKZ-AUFNR
                                binary search.
      if SY-SUBRC = 0.
        T_IMAKZ-KTEXT = T_COAS-KTEXT.
        modify T_IMAKZ.
      else.
        delete T_IMAKZ.
      endif.
    endloop.

    loop at T_IMAK.
      read table T_IMAKZ with key POSNR = T_IMAK-POSNR.
      if SY-SUBRC <> 0.
        delete T_IMAK.
      endif.
    endloop.

  else.
    if not T_IMAKZ[] is initial.
      select AUFNR  BUKRS OBJNR KTEXT AUART KOSTV from COAS
        into table T_COAS
        for all entries in T_IMAKZ
        where AUFNR eq T_IMAKZ-AUFNR.

      sort T_COAS by AUFNR.
      loop at T_IMAKZ.
        read table T_COAS with key AUFNR = T_IMAKZ-AUFNR
                                  binary search.
        if SY-SUBRC = 0.
          T_IMAKZ-KTEXT = T_COAS-KTEXT.
          modify T_IMAKZ.
        endif.
      endloop.
    endif.
  endif.

  sort T_IMAKZ by AUFNR.
endform.                    " F_BUSCA_ORDEM_INT_EST
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_CAIXA
*&---------------------------------------------------------------------*
form F_SELECIONA_CAIXA .

  data: W_EK like T_EKBEC occurs 0 with header line.
  data: begin of T_BK occurs 0,
          BUKRS like BKPF-BUKRS,
          BELNR like BKPF-BELNR,
          GJAHR like BKPF-GJAHR,
          AWKEY like BKPF-AWKEY,
          BLART like BKPF-BLART,
        end of T_BK.


*****************************************
** Rotina para buscar DOCUMENTOS PAGOS **
*****************************************

  check not T_EKKO[] is initial.
  perform F_MSG using 'Selecionando documentos pagos'
                      '33'.
  select EKBE~EBELN EKBE~EBELP EKBE~ZEKKN EKBE~VGABE EKBE~GJAHR EKBE~BELNR
         EKBE~BUZEI EKBE~BWART EKBE~BUDAT EKBE~MENGE EKBE~DMBTR EKBE~WRBTR EKBE~WAERS
         EKBE~LFBNR EKBE~CPUDT EKBE~SHKZG EKBE~XBLNR
         EKKN~ANLN1 EKKN~AUFNR
    from EKBE
    left outer join EKKN on
    ( EKBE~EBELN = EKKN~EBELN and
      EKBE~EBELP = EKKN~EBELP and
      EKBE~ZEKKN = EKKN~ZEKKN     )
    into table T_EKBEC
    for all entries in T_EKKO
    where EKBE~EBELN eq T_EKKO-EBELN and
          EKBE~EBELP eq T_EKKO-EBELP and
          EKBE~VGABE in ('3', '4', '2').
* Seleção Contas de Bancos
  select BUKRS SAKNR from SKB1 into table T_SKB11 "#EC CI_DB_OPERATION_OK[2431747]
    where BUKRS in S_BUKRS and
          FDLEV eq 'F0'.

  sort T_EKBEC by EBELN EBELP BELNR BUZEI.
  delete adjacent duplicates from T_EKBEC comparing EBELN EBELP BELNR BUZEI.

  sort T_SKB11 by BUKRS SAKNR.


  W_EK[] = T_EKBEC[].
*  DELETE w_ek WHERE vgabe NE 2.
*  DELETE t_ekbec WHERE vgabe EQ 2.

  delete W_EK where VGABE eq 4.
  delete T_EKBEC where VGABE ne 4.

  if not W_EK[] is initial.

    loop at W_EK.
      concatenate W_EK-BELNR W_EK-GJAHR into W_EK-AWKEY.
      modify W_EK.
    endloop.

    perform F_MSG using 'Selecionando documentos pagos'
                        '33'.
    select BUKRS BELNR GJAHR AWKEY BLART from BKPF into table T_BK
      for all entries in W_EK
      where AWTYP in ('MKPF', 'RMRP') and
            AWKEY eq W_EK-AWKEY     and
            BUKRS in S_BUKRS      and
            GJAHR eq W_EK-GJAHR.

    if SY-SUBRC = 0.
      sort T_BK[] by AWKEY GJAHR BLART ascending.

      loop at W_EK.
        if P_SINCAX is not initial.
          read table T_BK with key AWKEY = W_EK-AWKEY
                                 GJAHR = W_EK-GJAHR
                                 BLART = 'RE' binary search.
        else.
          read table T_BK with key AWKEY = W_EK-AWKEY
                                   GJAHR = W_EK-GJAHR.
        endif.
        if SY-SUBRC <> 0.
          delete W_EK.
        else.
          move-corresponding W_EK to T_EKBEC.
          move T_BK-BELNR to T_EKBEC-BELNR.
          append T_EKBEC. clear T_EKBEC.
        endif.
      endloop.
    endif.
  endif.

  if not T_EKBEC[] is initial.



    loop at T_BSEG.
      read table T_EKBEC with key BELNR = T_BSEG-BELNR
                                  GJAHR = T_BSEG-GJAHR.
      if SY-SUBRC = 0.
        delete T_BSEG.
      endif.
    endloop.

    T_EKBE_AUX[] = T_EKBEC[].

    perform F_MSG using 'Selecionando partidas pagas'
                        '33'.
*    CHECK NOT p_sincax IS INITIAL.
* Seleção Partidas Pagas
    select BUKRS LIFNR UMSKS UMSKZ AUGDT
           AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN
      from BSAK into table T_BSAK
      for all entries in T_EKBEC
      where BUKRS in S_BUKRS       and
            GJAHR eq T_EKBEC-GJAHR and
            BELNR eq T_EKBEC-BELNR.

*    IF sy-subrc <> 0.
    if SY-SUBRC is initial.
      select BUKRS LIFNR UMSKS UMSKZ AUGDT
           AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN
      from BSAK appending table T_BSAK
      for all entries in T_BSAK
      where BUKRS in S_BUKRS       and
            GJAHR eq T_BSAK-GJAHR and
*          augbl EQ t_bsak-belnr.
            BELNR eq T_BSAK-AUGBL.
    endif.

    if SY-SUBRC = 0.
      T_BSAK_B[] = T_BSAK[].

      do.

*        SELECT bukrs lifnr umsks umskz augdt
*             augbl zuonr gjahr belnr buzei
*        FROM bsak INTO TABLE t_bsak_a
*        FOR ALL ENTRIES IN t_bsak_b
*        WHERE bukrs EQ t_bsak_b-bukrs AND
*              gjahr EQ t_bsak_b-gjahr AND
*              augbl EQ t_bsak_b-belnr AND
*             ( belnr NE t_bsak_b-belnr AND belnr NE t_bsak_b-augbl ).

        perform F_MSG using 'Selecionando partidas pagas'
                            '33'.
        select BUKRS LIFNR UMSKS UMSKZ AUGDT
             AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN
        from BSAK into table T_BSAK_A
        for all entries in T_BSAK_B
        where BUKRS eq T_BSAK_B-BUKRS and
              GJAHR eq T_BSAK_B-GJAHR and
              BELNR eq T_BSAK_B-AUGBL and
             ( AUGBL ne T_BSAK_B-BELNR ). "AND belnr NE t_bsak_b-augbl ).


        if SY-SUBRC <> 0.
          exit.
        else.


          sort T_EKBEC by GJAHR BELNR.
          loop at T_BSAK_A.
            read table T_EKBEC with key GJAHR = T_BSAK_A-GJAHR
                                        BELNR = T_BSAK_A-BELNR
                                        binary search.
            if SY-SUBRC = 0.
              delete T_BSAK_A.
            endif.
          endloop.

          if T_BSAK_A[] is initial.
            exit.
          endif.
          append lines of T_BSAK_A to T_BSAK.
          T_BSAK_B[] = T_BSAK_A[].
          refresh T_BSAK_A.
        endif.

      enddo.
    endif.

    if not T_BSAK[] is initial.
      perform F_MSG using 'Selecionando partidas pagas'
                          '33'.
      select BUKRS HKONT AUGDT AUGBL ZUONR
             GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
        from BSIS into table T_BSIS_DP
        for all entries in T_BSAK
        where BUKRS = T_BSAK-BUKRS and
              GJAHR = T_BSAK-GJAHR and
              BELNR = T_BSAK-BELNR.

      perform F_MSG using 'Selecionando partidas pagas'
                          '33'.
      select BUKRS HKONT AUGDT AUGBL ZUONR
             GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
        from BSIS appending table T_BSIS_DP
        for all entries in T_BSAK
        where BUKRS = T_BSAK-BUKRS and
              GJAHR = T_BSAK-GJAHR and
              BELNR = T_BSAK-AUGBL.

      if P_SINCAX is not initial.
        select BUKRS HKONT AUGDT AUGBL ZUONR
               GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
          from BSIS appending table T_BSIS_DP
          for all entries in T_BSAK
          where BUKRS = T_BSAK-BUKRS and
                GJAHR = T_BSAK-AUGDT(4) and
                BELNR = T_BSAK-BELNR.

        perform F_MSG using 'Selecionando partidas pagas'
                            '33'.
        select BUKRS HKONT AUGDT AUGBL ZUONR
               GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
          from BSIS appending table T_BSIS_DP
          for all entries in T_BSAK
          where BUKRS = T_BSAK-BUKRS and
                GJAHR = T_BSAK-AUGDT(4) and
                BELNR = T_BSAK-AUGBL.
***     Bsas
        select BUKRS HKONT AUGDT AUGBL ZUONR
               GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT SHKZG
          from BSAS appending table T_BSIS_DP
          for all entries in T_BSAK
          where BUKRS = T_BSAK-BUKRS and
                GJAHR = T_BSAK-AUGDT(4) and
                BELNR = T_BSAK-AUGBL.

        if T_BSIS_DP[] is not initial.
          select BUKRS BELNR GJAHR KURS2
            from BKPF
            into table T_BKPF_AG
             for all entries in T_BSIS_DP
             where BUKRS eq T_BSIS_DP-BUKRS
               and BELNR eq T_BSIS_DP-BELNR
               and GJAHR eq T_BSIS_DP-GJAHR.

        endif.
      endif.

      if P_SINCAX is not initial.
        select *
          from SETLEAF
          into table T_SETLEAF
           where SETNAME eq 'MAGGI_ZIM01_LIQ_BANCO'.

        loop at T_BSIS_DP.
          read table T_SETLEAF
                with key VALFROM = T_BSIS_DP-HKONT.
          if SY-SUBRC is not initial.
            read table T_SKB11 with key BUKRS = T_BSIS_DP-BUKRS
                                        SAKNR = T_BSIS_DP-HKONT
                                        binary search.
            if SY-SUBRC <> 0.
              delete T_BSIS_DP.
            endif.
          endif.
        endloop.
      else.
        loop at T_BSIS_DP.
          read table T_SKB11 with key BUKRS = T_BSIS_DP-BUKRS
                                      SAKNR = T_BSIS_DP-HKONT
                                      binary search.
          if SY-SUBRC <> 0.
            delete T_BSIS_DP.
          endif.
        endloop.
      endif.
      if not T_BSIS_DP[] is initial.

        perform F_MSG using 'Selecionando partidas pagas'
                            '33'.
        select BUKRS HKONT AUGDT AUGBL ZUONR
               GJAHR BELNR BUZEI DMBTR DMBE2 BUDAT
          from BSIS into table T_BSIS_A
          for all entries in T_BSIS_DP
          where BUKRS = T_BSIS_DP-BUKRS and
                GJAHR = T_BSIS_DP-GJAHR and
                BELNR = T_BSIS_DP-AUGBL.

*
*
*        LOOP AT t_bsis_a.
*          READ TABLE t_skb11 WITH KEY bukrs = t_bsis_a-bukrs
*                                      saknr = t_bsis_a-hkont
*                                      BINARY SEARCH.
*          IF sy-subrc <> 0.
*            DELETE t_bsis_a.
*          ENDIF.
*        ENDLOOP.

      endif.

    endif.
  endif.

  sort T_BSIS_DP.
  delete adjacent duplicates from T_BSIS_DP comparing all fields.
*******************************************
** Rotina para buscar DOCUMENTOS A PAGAR **
*******************************************

  check not WE_BKPF[] is initial.

  perform F_MSG using 'Selecionando documentos a pagar'
                      '34'.
  select BUKRS LIFNR UMSKS UMSKZ AUGDT AUGBL ZUONR
         GJAHR BELNR BUZEI DMBTR DMBE2 ZFBDT ZBD1T BUDAT SHKZG
         BLART WRBTR
    from BSIK into table T_BSIK
    for all entries in WE_BKPF
    where BUKRS eq WE_BKPF-BUKRS and
          GJAHR eq WE_BKPF-GJAHR and
          BELNR eq WE_BKPF-BELNR.

  perform F_MSG using 'Selecionando documentos a pagar'
                      '34'.
  select BUKRS LIFNR UMSKS UMSKZ AUGDT
         AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN
    from BSAK into table T_BSAK_AP
    for all entries in WE_BKPF
    where BUKRS eq WE_BKPF-BUKRS  and
          GJAHR eq WE_BKPF-GJAHR and
          BELNR eq WE_BKPF-BELNR.

  check SY-SUBRC is initial.
  T_BSAK_B[] = T_BSAK_AP[].
  if P_SINCAX is initial.
    do.
      perform F_MSG using 'Selecionando documentos a pagar'
                          '34'.
      select BUKRS LIFNR UMSKS UMSKZ AUGDT
           AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN
      from BSAK into table T_BSAK_A
      for all entries in T_BSAK_B
      where BUKRS eq T_BSAK_B-BUKRS and
            GJAHR eq T_BSAK_B-GJAHR and
            AUGBL eq T_BSAK_B-BELNR and
           ( BELNR ne T_BSAK_B-BELNR and BELNR ne T_BSAK_B-AUGBL ).

      if SY-SUBRC <> 0.
        exit.
      else.

*        LOOP AT t_bsak_a.
*          READ TABLE t_ekbec WITH KEY gjahr = t_bsak_a-gjahr
*                                      belnr = t_bsak_a-belnr.
*          IF sy-subrc = 0.
*            DELETE t_bsak_a.
*          ENDIF.
*        ENDLOOP.

        if T_BSAK_A[] is initial.
          exit.
        endif.
        append lines of T_BSAK_A to T_BSAK_AP.
        T_BSAK_B[] = T_BSAK_A[].
        refresh T_BSAK_A.
      endif.

    enddo.
  else.
    do.
      perform F_MSG using 'Selecionando documentos a pagar'
                          '34'.
      select BUKRS LIFNR UMSKS UMSKZ AUGDT
           AUGBL ZUONR GJAHR BELNR BUZEI BLART EBELN
      from BSAK into table T_BSAK_A
      for all entries in T_BSAK_B
      where BUKRS eq T_BSAK_B-BUKRS and
            GJAHR eq T_BSAK_B-GJAHR and
            BELNR eq T_BSAK_B-AUGBL and
           ( AUGBL ne T_BSAK_B-BELNR ). "AND belnr NE t_bsak_b-augbl ).

      if SY-SUBRC <> 0.
        exit.
      else.

*        LOOP AT t_bsak_a.
*          READ TABLE t_ekbec WITH KEY gjahr = t_bsak_a-gjahr
*                                      belnr = t_bsak_a-belnr.
*          IF sy-subrc = 0.
*            DELETE t_bsak_a.
*          ENDIF.
*        ENDLOOP.

        if T_BSAK_A[] is initial.
          exit.
        endif.
        append lines of T_BSAK_A to T_BSAK_AP.
        T_BSAK_B[] = T_BSAK_A[].
        refresh T_BSAK_A.
      endif.

    enddo.
  endif.
  perform F_MSG using 'Selecionando documentos a pagar'
                      '34'.
  select BUKRS LIFNR UMSKS UMSKZ AUGDT AUGBL ZUONR
         GJAHR BELNR BUZEI DMBTR DMBE2 ZFBDT ZBD1T BUDAT SHKZG
         BLART WRBTR
    from BSIK appending table T_BSIK
    for all entries in T_BSAK_AP
    where BUKRS eq T_BSAK_AP-BUKRS and
          GJAHR eq T_BSAK_AP-GJAHR and
          BELNR eq T_BSAK_AP-AUGBL.

  check SY-SUBRC is initial.

endform.                    " F_SELECIONA_CAIXA
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_CAIXA
*&---------------------------------------------------------------------*
form   F_MONTA_DADOS_CAIXA_PG .
  data: begin of TL_TOTAL occurs 0,
          EBELN type EKBE-EBELN,
          EBELP type EKBE-EBELP,
          TOTAL type EKBE-DMBTR,
        end of TL_TOTAL.

  data: WL_SAIDA          like line of TW_SAIDA,
        WL_EKBE           like line of T_EKBE,
        WL_BSAK           like line of T_BSAK,
        WL_TOTAL_ITEM     type EKBE-DMBTR,
        WL_TOTAL_ADTO     type EKBE-DMBTR,
        WL_PERCENTUAL(10) type P decimals 8,
        WL_TABIX          type SY-TABIX,
        TL_SAIDA          like table of TW_SAIDA with header line,
        WL_ADIANTAMENTO.

  if P_SINCAX is not initial
  or P_ANALIT is not initial
  or P_CONSOL is not initial.
    clear: TW_SAIDA-BELNR1, T_BSIS_DP, TW_SAIDA-TOTAPG, TW_SAIDA-BELNR3, WL_TOTAL_ITEM, WL_PERCENTUAL, WL_TOTAL_ADTO.

*    LOOP AT T_EKBEC WHERE EBELN = T_EKKO-EBELN AND
*                          EBELP = T_EKKO-EBELP. "AND
**                          VGABE = 2.
**      IF T_EKBEC-VGABE EQ 2.
*      ADD T_EKBEC-DMBTR TO WL_TOTAL_ITEM.
**      ELSEIF T_EKBEC-VGABE EQ 4.
**        ADD T_EKBEC-DMBTR TO WL_TOTAL_ADTO.
**    ENDIF,.
*    ENDLOOP.

  endif.
  loop at T_EKBEC where EBELN = T_EKKO-EBELN and
                        EBELP = T_EKKO-EBELP. "AND
*                        zekkn = t_ekko-zekkn.
*                        vgabe = 4.""  AND

    read table T_EKBE into WL_EKBE
      with key EBELN = T_EKBEC-EBELN
               EBELP = T_EKBEC-EBELP
*               zekkn  = t_ekbec-zekkn
               VGABE = T_EKBEC-VGABE.

    if SY-SUBRC is not initial.
      read table T_EKBE into WL_EKBE
      with key EBELN = T_EKBEC-EBELN
               EBELP = T_EKBEC-EBELP
*               zekkn  = t_ekbec-zekkn
               VGABE = 3.
    endif.
*** igor vilela - inicio
    TW_SAIDA-BELNR5 = WL_EKBE-BELNR.
*    tw_saida-belnr5 = T_ekbeC-belnr.
*** igor vilela - fim

    read table T_BSAK with key BUKRS = T_IMAK-ABUKRS
                               GJAHR = T_EKBEC-GJAHR
                               BELNR = T_EKBEC-BELNR.

    if SY-SUBRC <> 0.

      loop at T_BSAK where BUKRS = T_IMAK-ABUKRS and
                             GJAHR = T_EKBEC-GJAHR and
                             AUGBL = T_EKBEC-BELNR.
        T_BSAK-BELNR = T_BSAK-AUGBL.
        modify T_BSAK.
      endloop.
    endif.
    if T_EKBEC-VGABE ne 4.
*      CLEAR: WL_PERCENTUAL.
*      WL_PERCENTUAL = T_EKBEC-DMBTR  / WL_TOTAL_ITEM.
      loop at T_BSAK where BUKRS = T_IMAK-ABUKRS and
                           GJAHR = T_EKBEC-GJAHR and
                           BELNR = T_EKBEC-BELNR.

        read table T_BSIS_DP with key BUKRS = T_IMAK-ABUKRS
                                      GJAHR = T_BSAK-AUGDT(4) "t_bsak-gjahr
                                      BELNR = T_BSAK-BELNR.

        if SY-SUBRC = 0.
          loop at T_BSIS_DP where BUKRS = T_IMAK-ABUKRS  and
                               GJAHR = T_BSAK-AUGDT(4)   and
                               BELNR = T_BSAK-BELNR.

            read table T_LFA1 with key LIFNR = T_BSAK-LIFNR
                                       binary search.
            if SY-SUBRC = 0.
              concatenate T_LFA1-NAME1 T_BSAK-LIFNR into TW_SAIDA-FORNECEDOR
                separated by ' - '.
            endif.

*        t_bsis_dp-dmbtr = t_bsis_dp-dmbtr * t_ekko-porcento.

            if T_BSIS_DP-SHKZG = 'H'.
              T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * ( - 1 ) .
              T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * ( - 1 ) .
            endif.

*            IF t_ekbec-vgabe EQ 4.
*              IF NOT t_ekko-porcento IS INITIAL.
*                t_bsis_dp-dmbtr = t_ekbec-dmbtr * t_ekko-porcento.
*              ELSE.
*                t_bsis_dp-dmbtr = t_ekbec-dmbtr.
*              ENDIF.
*              IF t_ekbec-shkzg = 'S'.
*                MULTIPLY t_bsis_dp-dmbtr BY -1.
*                MOVE t_ekbec-shkzg TO t_bsis_dp-shkzg.
*              ELSE.
*                CONTINUE.
*              ENDIF.
*            ELSE.
*            clear: wl_bsak.
*            loop at t_bsak into wl_bsak where bukrs = t_imak-abukrs
*                                          and gjahr = t_ekbec-gjahr
*                                          and augbl = t_bsak-augbl.
*              if wl_bsak-belnr ne wl_bsak-augbl
*              and wl_bsak-belnr eq t_ekbec-belnr.
            TW_SAIDA-BELNR3 = T_EKBEC-BELNR.
*                exit.
*              endif.

*            endloop.
            clear: T_DOCS_AG.
            read table T_DOCS_AG
              with key BUKRS = T_BSAK-BUKRS
                       BELNR = T_BSAK-BELNR.
            if SY-SUBRC is initial.
              if T_DOCS_AG-AG eq 'X'.
                T_BSAK-BLART = 'AG'.
              else.
                T_BSAK-BLART = 'AB'.
              endif.
            endif.
            if T_BSAK-BLART eq 'AG'.
*            OR t_bsak-blart EQ 'AB'.
              read table T_BSAK_AG
                with key BUKRS = T_BSAK-BUKRS
                         GJAHR = T_BSAK-GJAHR
                         AUGBL = T_BSAK-BELNR
                         BELNR = T_EKBEC-BELNR.

              move : T_BSAK_AG-DMBTR to T_BSIS_DP-DMBTR.
*                     t_bsak_ag-dmbe2 to t_bsis_dp-dmbe2.
              read table T_BKPF_AG
                with key BUKRS = T_BSIS_DP-BUKRS
                         BELNR = T_BSIS_DP-BELNR
                         GJAHR = T_BSIS_DP-GJAHR.
              if SY-SUBRC is initial.
                if T_BKPF_AG-KURS2 is not initial.
                  move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                  divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                else.
                  T_BKPF_AG-KURS2 = T_BSIS_DP-DMBTR / T_BSIS_DP-DMBE2.
                  move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                  divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                endif.
              endif.

            endif.

            if P_SINCAX is not initial
            or P_ANALIT is not initial
            or P_CONSOL is not initial. "t_ekbec-vgabe EQ 3.
              clear T_EKKO3.
              if T_EKKO-AUFNR is not initial.
                read table T_EKKO3
                  with key EBELN = T_EKBEC-EBELN
                           EBELP = T_EKBEC-EBELP
                           AUFNR = T_EKKO-AUFNR
                           ZEKKN = T_EKKO-ZEKKN
                           BELNR = T_EKBEC-AWKEY(10).
              elseif T_EKKO-ANLN1 is not initial.
                read table T_EKKO3
                 with key EBELN = T_EKBEC-EBELN
                           EBELP = T_EKBEC-EBELP
                           ANLN1 = T_EKKO-ANLN1
                           ZEKKN = T_EKKO-ZEKKN
                           BELNR = T_EKBEC-AWKEY(10).
              endif.
              if not T_EKKO3-PORCENTO is initial.
                T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO3-PORCENTO.  "aaaa
                T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO3-PORCENTO.
              endif.
*              MULTIPLY T_BSIS_DP-DMBTR BY WL_PERCENTUAL.
*              MULTIPLY T_BSIS_DP-DMBE2 BY WL_PERCENTUAL.
            else.
              if not T_EKKO-PORCENTO is initial.
                T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO-PORCENTO.  "aaaa
                T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO-PORCENTO.
              endif.
            endif.
*            ENDIF.
            clear WE_BKPF.
            read table WE_BKPF
              with key BELNR = TW_SAIDA-BELNR3.

            TW_SAIDA-BELNR5 = WE_BKPF-AWKEY(10).

            if P_MOEDA eq 'USD'.
              move: T_BSIS_DP-DMBE2 to T_BSIS_DP-DMBTR.
            endif.

            move: "t_bsak-belnr    TO tw_saida-belnr3,
                  T_BSIS_DP-BELNR to TW_SAIDA-BELNR6.
*---> 13/06/2023 - Migração S4 - JS
*                  t_bsis_dp-dmbtr TO tw_saida-totpg.
            TW_SAIDA-TOTPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
            move:
            T_BSIS_DP-DMBTR to TW_SAIDA-NETPR,
            T_BSIS_DP-DMBE2 to TW_SAIDA-EMENGE,
            T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
            T_BSIS_DP-BUDAT to TW_SAIDA-BUDAT,
            'FIPA'          to TW_SAIDA-BELNR2.

            read table S_BUDAT index 1.
            if T_BSIS_DP-BUDAT > S_BUDAT-HIGH.
*---> 13/06/2023 - Migração S4 - JS
*            move T_BSIS_DP-DMBTR to TW_SAIDA-TOTAPG.
              TW_SAIDA-TOTAPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
              clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
            endif.

            TW_SAIDA-EBELN = T_EKKO-EBELN.
            TW_SAIDA-EBELP = T_EKKO-EBELP.
            TW_SAIDA-ZEKKN = T_EKKO-ZEKKN.
            read table TW_SAIDA into WL_SAIDA
                    with key EBELN  = TW_SAIDA-EBELN
                    EBELP  = TW_SAIDA-EBELP
*                  belnr1 = tw_saida-belnr1
                    AUFNR  = TW_SAIDA-AUFNR
                    ANLN1  = TW_SAIDA-ANLN1
                    BELNR2 = TW_SAIDA-BELNR2
                    BELNR3 = TW_SAIDA-BELNR3
                    BELNR4 = TW_SAIDA-BELNR4.

            if SY-SUBRC is not initial.
              if TW_SAIDA-AEDAT in S_BUDAT
              or TW_SAIDA-TOTAPG is not initial.
                append TW_SAIDA.
              endif.
            endif.
*        DELETE t_bsis_dp.
            clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE,
                   TW_SAIDA-TOTPG,  TW_SAIDA-BELNR2, TW_SAIDA-TOTAPG, TW_SAIDA-BELNR6.
          endloop.
        else.

***** igor vilela - correcao
          if P_SINCAX is not initial
          or P_ANALIT is not initial
          or P_CONSOL is not initial.
            loop at T_BSAK where BUKRS = T_IMAK-ABUKRS
                             and GJAHR = T_EKBEC-GJAHR
                             and BELNR = T_BSAK-AUGBL.


              loop at T_BSIS_DP where BUKRS = T_IMAK-ABUKRS  and
                                     GJAHR = T_BSAK-AUGDT(4) and
                                     BELNR = T_BSAK-AUGBL.

                read table T_LFA1 with key LIFNR = T_BSAK-LIFNR
                                           binary search.
                if SY-SUBRC = 0.
                  concatenate T_LFA1-NAME1 T_BSAK-LIFNR into TW_SAIDA-FORNECEDOR
                    separated by ' - '.
                endif.

*        t_bsis_dp-dmbtr = t_bsis_dp-dmbtr * t_ekko-porcento.
*                IF t_ekbec-vgabe EQ 4.
*                  IF NOT t_ekko-porcento IS INITIAL.
*                    t_bsis_dp-dmbtr = t_ekbec-dmbtr * t_ekko-porcento.
*                  ELSE.
*                    t_bsis_dp-dmbtr = t_ekbec-dmbtr.
*                  ENDIF.
*                  IF t_ekbec-shkzg = 'S'.
*                    MULTIPLY t_bsis_dp-dmbtr BY -1.
*                    MOVE t_ekbec-shkzg TO t_bsis_dp-shkzg.
*                  ELSE.
*                    CONTINUE.
*                  ENDIF.
*                ELSE.
*                clear: wl_bsak.
*                loop at t_bsak into wl_bsak where bukrs = t_imak-abukrs
*                                              and gjahr = t_ekbec-gjahr
*                                              and augbl = t_bsak-augbl.
*                  if wl_bsak-belnr ne wl_bsak-augbl
*                  and wl_bsak-belnr eq t_ekbec-belnr.
                TW_SAIDA-BELNR3 = T_EKBEC-BELNR.
*                    exit.
*                  endif.

*                endloop.
                clear: T_DOCS_AG.
                read table T_DOCS_AG
                  with key BUKRS = T_BSAK-BUKRS
                           BELNR = T_BSAK-BELNR.
                if SY-SUBRC is initial.
                  if T_DOCS_AG-AG eq 'X'.
                    T_BSAK-BLART = 'AG'.
                  else.
                    T_BSAK-BLART = 'AB'.
                  endif.
                endif.
                if T_BSAK-BLART eq 'AG'.
*                OR t_bsak-blart EQ 'AB'.
                  read table T_BSAK_AG
                    with key BUKRS = T_BSAK-BUKRS
                             GJAHR = T_BSAK-GJAHR
                             AUGBL = T_BSAK-BELNR
                             BELNR = T_EKBEC-BELNR.

                  move : T_BSAK_AG-DMBTR to T_BSIS_DP-DMBTR.
*                         t_bsak_ag-dmbe2 to t_bsis_dp-dmbe2.

                  read table T_BKPF_AG
                    with key BUKRS = T_BSIS_DP-BUKRS
                             BELNR = T_BSIS_DP-BELNR
                             GJAHR = T_BSIS_DP-GJAHR.
                  if SY-SUBRC is initial.
                    if T_BKPF_AG-KURS2 is not initial.
                      move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                      divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                    else.
                      T_BKPF_AG-KURS2 = T_BSIS_DP-DMBTR / T_BSIS_DP-DMBE2.
                      move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                      divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                    endif.
                  endif.
                endif.

                if P_SINCAX is not initial
                or P_ANALIT is not initial
                or P_CONSOL is not initial. "t_ekbec-vgabe EQ 3.
                  clear T_EKKO3.
                  if T_EKKO-AUFNR is not initial.
                    read table T_EKKO3
                  with key EBELN = T_EKBEC-EBELN
                           EBELP = T_EKBEC-EBELP
                           AUFNR = T_EKKO-AUFNR
                           ZEKKN = T_EKKO-ZEKKN
                           BELNR = T_EKBEC-AWKEY(10).
                  elseif T_EKKO-ANLN1 is not initial.
                    read table T_EKKO3
                with key EBELN = T_EKBEC-EBELN
                         EBELP = T_EKBEC-EBELP
                         ANLN1 = T_EKKO-ANLN1
                         ZEKKN = T_EKKO-ZEKKN
                         BELNR = T_EKBEC-AWKEY(10).
                  endif.
                  if not T_EKKO3-PORCENTO is initial.
                    T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO3-PORCENTO.  "aaaa
                    T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO3-PORCENTO.
                  endif.
*                  MULTIPLY T_BSIS_DP-DMBTR BY WL_PERCENTUAL.
*                  MULTIPLY T_BSIS_DP-DMBE2 BY WL_PERCENTUAL.
                else.
                  if not T_EKKO-PORCENTO is initial.
                    T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO-PORCENTO. "aaaaa
                    T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO-PORCENTO.
                  endif.
                endif.
*                ENDIF.

                if T_BSIS_DP-SHKZG = 'H'.
                  T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * ( - 1 ) .
                  T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * ( - 1 ) .
                endif.

                clear WE_BKPF.
                read table WE_BKPF
                  with key BELNR = TW_SAIDA-BELNR3.

                TW_SAIDA-BELNR5 = WE_BKPF-AWKEY(10).

                if P_MOEDA eq 'USD'.
                  move: T_BSIS_DP-DMBE2 to T_BSIS_DP-DMBTR.
                endif.

                move: "t_bsak-belnr    TO tw_saida-belnr3,
                      T_BSIS_DP-BELNR to TW_SAIDA-BELNR6.
*---> 13/06/2023 - Migração S4 - JS
*            T_BSIS_DP-DMBTR to TW_SAIDA-TOTPG,
                TW_SAIDA-TOTPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
                move: T_BSIS_DP-DMBTR to TW_SAIDA-NETPR,
                      T_BSIS_DP-DMBE2 to TW_SAIDA-EMENGE,
                      T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                      T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                      'FIPA'          to TW_SAIDA-BELNR2.

                read table S_BUDAT index 1.
                if T_BSIS_DP-BUDAT > S_BUDAT-HIGH.
*---> 13/06/2023 - Migração S4 - JS
*            move T_BSIS_DP-DMBTR to TW_SAIDA-TOTAPG.
                  TW_SAIDA-TOTAPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
                  clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
                endif.

                TW_SAIDA-EBELN = T_EKKO-EBELN.
                TW_SAIDA-EBELP = T_EKKO-EBELP.
                TW_SAIDA-ZEKKN = T_EKKO-ZEKKN.

                read table TW_SAIDA transporting no fields
                  with key BELNR3 = TW_SAIDA-BELNR3
                           TOTPG  = TW_SAIDA-TOTPG
                           NETPR  = TW_SAIDA-NETPR
                           EMENGE = TW_SAIDA-EMENGE
                           AEDAT  = TW_SAIDA-AEDAT
                           AUFNR  = TW_SAIDA-AUFNR  " aaaa
                           ANLN1  = TW_SAIDA-ANLN1
                           BELNR2 = TW_SAIDA-BELNR2
                           EBELN  = TW_SAIDA-EBELN
                           EBELP  = TW_SAIDA-EBELP
                           ZEKKN  = TW_SAIDA-ZEKKN.

                if SY-SUBRC is not initial.
                  read table TW_SAIDA into WL_SAIDA
                    with key EBELN  = TW_SAIDA-EBELN
                    EBELP  = TW_SAIDA-EBELP
                    BELNR1 = TW_SAIDA-BELNR1
                    BELNR2 = TW_SAIDA-BELNR2
                    BELNR3 = TW_SAIDA-BELNR3
                    BELNR4 = TW_SAIDA-BELNR4
                    BELNR6 = TW_SAIDA-BELNR6
                    AUFNR  = TW_SAIDA-AUFNR
                    ANLN1  = TW_SAIDA-ANLN1
                    ZEKKN  = TW_SAIDA-ZEKKN. " aaaa.

                  if SY-SUBRC is initial.
*                  ADD: wl_saida-totpg TO tw_saida-totpg,
*                       wl_saida-netpr  TO tw_saida-netpr,
*                       wl_saida-emenge TO tw_saida-emenge.
*
*                  modify tw_saida index sy-tabix.
                  else.
                    if TW_SAIDA-AEDAT in S_BUDAT
                    or TW_SAIDA-TOTAPG is not initial.
                      append TW_SAIDA.
                    endif.
                  endif.
                endif.
*        DELETE t_bsis_dp.
                clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE, TW_SAIDA-BELNR6,
                       TW_SAIDA-TOTPG,  TW_SAIDA-BELNR2, TW_SAIDA-TOTPG, TW_SAIDA-TOTAPG.
              endloop.
              loop at T_BSAK where   BUKRS = T_IMAK-ABUKRS  and
                                     GJAHR = T_BSAK-GJAHR   and
                                     BELNR = T_BSAK-AUGBL.
                read table TW_SAIDA transporting no fields
                with key EBELN = T_EKKO-EBELN
                         EBELP = T_EKKO-EBELP
                         BELNR2 = 'FIPA'
                         BELNR3 = T_BSAK-AUGBL.
                if SY-SUBRC is not initial.
                  loop at T_BSAK where BUKRS = T_IMAK-ABUKRS
                               and GJAHR = T_EKBEC-GJAHR
                               and BELNR = T_BSAK-AUGBL.

                    if T_BSAK-EBELN is not initial
                    and T_BSAK-EBELN ne T_EKBEC-EBELN.
                      continue.
                    endif.

                    loop at T_BSIS_DP where BUKRS = T_IMAK-ABUKRS  and
                                           GJAHR = T_BSAK-AUGDT(4) and
                                           BELNR = T_BSAK-AUGBL.


                      read table T_LFA1 with key LIFNR = T_BSAK-LIFNR
                                                 binary search.
                      if SY-SUBRC = 0.
                        concatenate T_LFA1-NAME1 T_BSAK-LIFNR into TW_SAIDA-FORNECEDOR
                          separated by ' - '.
                      endif.

*        t_bsis_dp-dmbtr = t_bsis_dp-dmbtr * t_ekko-porcento.

*                      IF t_ekbec-vgabe EQ 4.
*                        t_bsis_dp-dmbtr = t_ekbec-dmbtr.
*                        IF t_ekbec-shkzg = 'S'.
*                          MULTIPLY t_bsis_dp-dmbtr BY -1.
*                          MOVE t_ekbec-shkzg TO t_bsis_dp-shkzg.
*                        ELSE.
*                          CONTINUE.
*                        ENDIF.
*                      ELSE.
*                      clear: wl_bsak.
*                      loop at t_bsak into wl_bsak where bukrs = t_imak-abukrs
*                                                    and gjahr = t_ekbec-gjahr
*                                                    and augbl = t_bsak-augbl.
*                        if wl_bsak-belnr ne wl_bsak-augbl
*                        and wl_bsak-belnr eq t_ekbec-belnr.
                      TW_SAIDA-BELNR3 = T_EKBEC-BELNR.
*                          exit.
*                        endif.

*                      endloop.
                      clear: T_DOCS_AG.
                      read table T_DOCS_AG
                        with key BUKRS = T_BSAK-BUKRS
                                 BELNR = T_BSAK-BELNR.
                      if SY-SUBRC is initial.
                        if T_DOCS_AG-AG eq 'X'.
                          T_BSAK-BLART = 'AG'.
                        else.
                          T_BSAK-BLART = 'AB'.
                        endif.
                      endif.
                      if T_BSAK-BLART eq 'AG'.
*                      OR t_bsak-blart EQ 'AB'.
                        read table T_BSAK_AG
                          with key BUKRS = T_BSAK-BUKRS
                                   GJAHR = T_BSAK-GJAHR
                                   AUGBL = T_BSAK-BELNR
                                   BELNR = T_EKBEC-BELNR.

                        move : T_BSAK_AG-DMBTR to T_BSIS_DP-DMBTR.
*                               t_bsak_ag-dmbe2 to t_bsis_dp-dmbe2.

                        read table T_BKPF_AG
                          with key BUKRS = T_BSIS_DP-BUKRS
                                   BELNR = T_BSIS_DP-BELNR
                                   GJAHR = T_BSIS_DP-GJAHR.
                        if SY-SUBRC is initial.
                          if T_BKPF_AG-KURS2 is not initial.
                            move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                            divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                          else.
                            T_BKPF_AG-KURS2 = T_BSIS_DP-DMBTR / T_BSIS_DP-DMBE2.
                            move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                            divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                          endif.
                        endif.
                      endif.

                      if P_SINCAX is not initial
                      or P_CONSOL is not initial. "t_ekbec-vgabe EQ 3.
                        clear T_EKKO3.
                        if T_EKKO-AUFNR is not initial.
                          read table T_EKKO3
                           with key EBELN = T_EKBEC-EBELN
                                    EBELP = T_EKBEC-EBELP
                                    AUFNR = T_EKKO-AUFNR
                                    ZEKKN = T_EKKO-ZEKKN
                                    BELNR = T_EKBEC-AWKEY(10).
                        elseif T_EKKO-ANLN1 is not initial.
                          read table T_EKKO3
                         with key EBELN = T_EKBEC-EBELN
                                  EBELP = T_EKBEC-EBELP
                                  ANLN1 = T_EKKO-ANLN1
                                  ZEKKN = T_EKKO-ZEKKN
                                  BELNR = T_EKBEC-AWKEY(10).
                        endif.
                        if not T_EKKO3-PORCENTO is initial.
                          T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO3-PORCENTO.  "aaaa
                          T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO3-PORCENTO.  "aaaa

                        endif.
*                        MULTIPLY T_BSIS_DP-DMBTR BY WL_PERCENTUAL.
*                        MULTIPLY T_BSIS_DP-DMBE2 BY WL_PERCENTUAL.
                      else.
                        if not T_EKKO-PORCENTO is initial.
                          T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO-PORCENTO.  "aaaaa
                          T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO-PORCENTO.  "aaaaa

                        endif.
                      endif.
*                      ENDIF.

                      if T_BSIS_DP-SHKZG = 'H'.
                        T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * ( - 1 ) .
                        T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * ( - 1 ) .
                      endif.

*                      clear: wl_bsak.
*                      loop at t_bsak into wl_bsak where bukrs = t_imak-abukrs
*                                                    and gjahr = t_ekbec-gjahr
*                                                    and augbl = t_bsak-augbl.
*                        if wl_bsak-belnr ne wl_bsak-augbl
*                        and wl_bsak-belnr eq t_ekbec-belnr.
                      TW_SAIDA-BELNR3 = T_EKBEC-BELNR.
*                          exit.
*                        endif.
*
*                      endloop.

                      clear WE_BKPF.
                      read table WE_BKPF
                        with key BELNR = TW_SAIDA-BELNR3.

                      TW_SAIDA-BELNR5 = WE_BKPF-AWKEY(10).
                      if P_MOEDA eq 'USD'.
                        move: T_BSIS_DP-DMBE2 to T_BSIS_DP-DMBTR.
                        if T_BSIS_DP-DMBTR gt 0.
                          multiply T_BSIS_DP-DMBTR by -1.
                        endif.
                      endif.

                      move: "t_bsak-belnr    TO tw_saida-belnr3,
                            T_BSIS_DP-BELNR to TW_SAIDA-BELNR6.
*---> 13/06/2023 - Migração S4 - JS
*            T_BSIS_DP-DMBTR to TW_SAIDA-TOTPG,
                      TW_SAIDA-TOTPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
                      move: T_BSIS_DP-DMBTR to TW_SAIDA-NETPR,
                            T_BSIS_DP-DMBE2 to TW_SAIDA-EMENGE,
                            T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                            T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                            'FIPA'          to TW_SAIDA-BELNR2.

                      read table S_BUDAT index 1.
                      if T_BSIS_DP-BUDAT > S_BUDAT-HIGH.
*---> 13/06/2023 - Migração S4 - JS
*            move T_BSIS_DP-DMBTR to TW_SAIDA-TOTAPG.
                        TW_SAIDA-TOTAPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
                        clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
                      endif.

                      TW_SAIDA-EBELN = T_EKKO-EBELN.
                      TW_SAIDA-EBELP = T_EKKO-EBELP.
                      TW_SAIDA-ZEKKN = T_EKKO-ZEKKN.

                      read table TW_SAIDA transporting no fields
                        with key BELNR3 = TW_SAIDA-BELNR3
                                 TOTPG  = TW_SAIDA-TOTPG
                                 NETPR  = TW_SAIDA-NETPR
                                 EMENGE = TW_SAIDA-EMENGE
                                 AEDAT  = TW_SAIDA-AEDAT
*                         aufnr  = tw_saida-aufnr    "aaaa
                                 BELNR2 = TW_SAIDA-BELNR2.

                      if SY-SUBRC is not initial.
                        read table TW_SAIDA into WL_SAIDA
                          with key EBELN  = TW_SAIDA-EBELN
                          EBELP  = TW_SAIDA-EBELP
                          BELNR1 = TW_SAIDA-BELNR1
                          BELNR2 = TW_SAIDA-BELNR2
                          BELNR3 = TW_SAIDA-BELNR3
                          BELNR4 = TW_SAIDA-BELNR4
                          BELNR6 = TW_SAIDA-BELNR6
                          ZEKKN  = TW_SAIDA-ZEKKN.

                        if SY-SUBRC is initial.
**                  ADD: wl_saida-totpg TO tw_saida-totpg,
**                       wl_saida-netpr  TO tw_saida-netpr,
**                       wl_saida-emenge TO tw_saida-emenge.
**
**                  modify tw_saida index sy-tabix.
                        else.
                          if TW_SAIDA-AEDAT in S_BUDAT
                          or TW_SAIDA-TOTAPG is not initial.
                            append TW_SAIDA.
                          endif.
                        endif.
                      endif.
*        DELETE t_bsis_dp.
                      clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE, TW_SAIDA-BELNR6,
                             TW_SAIDA-TOTPG,  TW_SAIDA-BELNR2, TW_SAIDA-TOTPG, TW_SAIDA-TOTAPG.
                    endloop.
                  endloop.
                endif.
              endloop.
            endloop.
            if SY-SUBRC is not initial. "igor
              do.
                read table T_BSAK with key BUKRS = T_IMAK-ABUKRS
                                           GJAHR = T_EKBEC-GJAHR
                                           BELNR = T_BSAK-AUGBL.
                if SY-SUBRC <> 0 or
                   T_BSAK-BELNR = T_BSAK-AUGBL.
                  exit.
                endif.

              enddo.

              loop at T_BSIS_DP where BUKRS = T_IMAK-ABUKRS  and
                                   GJAHR = T_BSAK-AUGDT(4)   and
                                   BELNR = T_BSAK-AUGBL.

                read table T_LFA1 with key LIFNR = T_BSAK-LIFNR
                                           binary search.
                if SY-SUBRC = 0.
                  concatenate T_LFA1-NAME1 T_BSAK-LIFNR into TW_SAIDA-FORNECEDOR
                    separated by ' - '.
                endif.

*        t_bsis_dp-dmbtr = t_bsis_dp-dmbtr * t_ekko-porcento.

*                IF t_ekbec-vgabe EQ 4.
*                  IF NOT t_ekko-porcento IS INITIAL.
*                    t_bsis_dp-dmbtr = t_ekbec-dmbtr * t_ekko-porcento.
*                  ELSE.
*                    t_bsis_dp-dmbtr = t_ekbec-dmbtr.
*                  ENDIF.
*                  IF t_ekbec-shkzg = 'S'.
*                    MULTIPLY t_bsis_dp-dmbtr BY -1.
*                    MOVE t_ekbec-shkzg TO t_bsis_dp-shkzg.
*                  ELSE.
*                    CONTINUE.
*                  ENDIF.
*                ELSE.
                read table T_DOCS_AG
                 with key BUKRS = T_BSAK-BUKRS
                          BELNR = T_BSAK-BELNR.
                if SY-SUBRC is initial.
                  if T_DOCS_AG-AG eq 'X'.
                    T_BSAK-BLART = 'AG'.
                  else.
                    T_BSAK-BLART = 'AB'.
                  endif.
                endif.
                if T_BSAK-BLART eq 'AG'.
*                OR t_bsak-blart EQ 'AB'.
                  read table T_BSAK_AG
                    with key BUKRS = T_BSAK-BUKRS
                             GJAHR = T_BSAK-GJAHR
                             AUGBL = T_BSAK-BELNR
                             BELNR = T_EKBEC-BELNR.

                  move : T_BSAK_AG-DMBTR to T_BSIS_DP-DMBTR.
*                         t_bsak_ag-dmbe2 to t_bsis_dp-dmbe2.

                  read table T_BKPF_AG
                    with key BUKRS = T_BSIS_DP-BUKRS
                             BELNR = T_BSIS_DP-BELNR
                             GJAHR = T_BSIS_DP-GJAHR.
                  if SY-SUBRC is initial.
                    if T_BKPF_AG-KURS2 is not initial.
                      move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                      divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                    else.
                      T_BKPF_AG-KURS2 = T_BSIS_DP-DMBTR / T_BSIS_DP-DMBE2.
                      move : T_BSIS_DP-DMBTR to T_BSIS_DP-DMBE2.
                      divide T_BSIS_DP-DMBE2 by T_BKPF_AG-KURS2.
                    endif.
                  endif.
                endif.

                if P_SINCAX is not initial
                or P_ANALIT is not initial
                or P_CONSOL is not initial. "t_ekbec-vgabe EQ 3.
                  clear T_EKKO3.
                  if T_EKKO-AUFNR is not initial.
                    read table T_EKKO3
                  with key EBELN = T_EKBEC-EBELN
                           EBELP = T_EKBEC-EBELP
                           AUFNR = T_EKKO-AUFNR
                           ZEKKN = T_EKKO-ZEKKN
                           BELNR = T_EKBEC-AWKEY(10).
                  elseif T_EKKO-ANLN1 is not initial.
                    read table T_EKKO3
                 with key EBELN = T_EKBEC-EBELN
                          EBELP = T_EKBEC-EBELP
                          ANLN1 = T_EKKO-ANLN1
                          ZEKKN = T_EKKO-ZEKKN
                          BELNR = T_EKBEC-AWKEY(10).
                  endif.
                  if not T_EKKO3-PORCENTO is initial.
                    T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO3-PORCENTO.  "aaaa
                    T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO3-PORCENTO.  "aaaa

                  endif.
*                  MULTIPLY T_BSIS_DP-DMBTR BY WL_PERCENTUAL.
*                  MULTIPLY T_BSIS_DP-DMBE2 BY WL_PERCENTUAL.
                else.
                  if not T_EKKO-PORCENTO is initial.
                    T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO-PORCENTO.
                    T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO-PORCENTO.

                  endif.
                endif.
*                  ENDIF.

                if T_BSIS_DP-SHKZG = 'H'.
                  T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * ( - 1 ) .
                  T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * ( - 1 ) .
                endif.

                if P_MOEDA eq 'USD'.
                  move: T_BSIS_DP-DMBE2 to T_BSIS_DP-DMBTR.
                endif.

*                move: t_bsis_dp-belnr to tw_saida-belnr3,
*---> 13/06/2023 - Migração S4 - JS
*            T_BSIS_DP-DMBTR to TW_SAIDA-TOTPG,
                TW_SAIDA-TOTPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS

                move:  T_BSIS_DP-DMBTR to TW_SAIDA-NETPR,
                       T_BSIS_DP-DMBE2 to TW_SAIDA-EMENGE,
                       T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                       T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                       'FIPA'          to TW_SAIDA-BELNR2.

                read table S_BUDAT index 1.
                if T_BSIS_DP-BUDAT > S_BUDAT-HIGH.
*---> 13/06/2023 - Migração S4 - JS
*            move T_BSIS_DP-DMBTR to TAIW_SDA-TOTAPG.
                  TW_SAIDA-TOTPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS

                  clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
                endif.

                TW_SAIDA-EBELN = T_EKKO-EBELN.
                TW_SAIDA-EBELP = T_EKKO-EBELP.
                TW_SAIDA-ZEKKN = T_EKKO-ZEKKN.
                if TW_SAIDA-AEDAT in S_BUDAT
                or TW_SAIDA-TOTAPG is not initial.
                  append TW_SAIDA.
                endif.
*        DELETE t_bsis_dp.
                clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE,
                       TW_SAIDA-TOTPG,  TW_SAIDA-BELNR2, TW_SAIDA-TOTPG, TW_SAIDA-TOTAPG.
              endloop.
            endif. "igor
          else.
            do.
              read table T_BSAK with key BUKRS = T_IMAK-ABUKRS
                                         GJAHR = T_EKBEC-GJAHR
                                         BELNR = T_BSAK-AUGBL.
              if SY-SUBRC <> 0 or
                 T_BSAK-BELNR = T_BSAK-AUGBL.
                exit.
              endif.

            enddo.

            loop at T_BSIS_DP where BUKRS = T_IMAK-ABUKRS  and
                                 GJAHR = T_BSAK-AUGDT(4)   and
                                 BELNR = T_BSAK-AUGBL.

              read table T_LFA1 with key LIFNR = T_BSAK-LIFNR
                                         binary search.
              if SY-SUBRC = 0.
                concatenate T_LFA1-NAME1 T_BSAK-LIFNR into TW_SAIDA-FORNECEDOR
                  separated by ' - '.
              endif.

*        t_bsis_dp-dmbtr = t_bsis_dp-dmbtr * t_ekko-porcento.
              if P_SINCAX is not initial
              or P_ANALIT is not initial
              or P_CONSOL is not initial.. "t_ekbec-vgabe EQ 3.
                clear T_EKKO3.
                if T_EKKO-AUFNR is not initial.
                  read table T_EKKO3
                  with key EBELN = T_EKBEC-EBELN
                           EBELP = T_EKBEC-EBELP
                           AUFNR = T_EKKO-AUFNR
                           BELNR = T_EKBEC-AWKEY(10).
                elseif T_EKKO-ANLN1 is not initial.
                  read table T_EKKO3
                with key EBELN = T_EKBEC-EBELN
                         EBELP = T_EKBEC-EBELP
                         ANLN1 = T_EKKO-ANLN1
                         BELNR = T_EKBEC-AWKEY(10).
                endif.
                if not T_EKKO3-PORCENTO is initial.
                  T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO3-PORCENTO.  "aaaa
                  T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO3-PORCENTO.  "aaaa

                endif.
*                MULTIPLY T_BSIS_DP-DMBTR BY WL_PERCENTUAL.
*                MULTIPLY T_BSIS_DP-DMBE2 BY WL_PERCENTUAL.
              else.
                if not T_EKKO-PORCENTO is initial.
                  T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * T_EKKO-PORCENTO.
                  T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * T_EKKO-PORCENTO.
                endif.
              endif.

              if T_BSIS_DP-SHKZG = 'H'.
                T_BSIS_DP-DMBTR = T_BSIS_DP-DMBTR * ( - 1 ) .
                T_BSIS_DP-DMBE2 = T_BSIS_DP-DMBE2 * ( - 1 ) .
              endif.

              if P_MOEDA eq 'USD'.
                move: T_BSIS_DP-DMBE2 to T_BSIS_DP-DMBTR.
              endif.

*              move: t_bsis_dp-belnr to tw_saida-belnr3,
              move: T_EKBEC-BELNR   to TW_SAIDA-BELNR3.
*---> 13/06/2023 - Migração S4 - JS
*            T_BSIS_DP-DMBTR to TW_SAIDA-TOTPG,
              TW_SAIDA-TOTPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
              move:  T_BSIS_DP-DMBTR to TW_SAIDA-NETPR,
                     T_BSIS_DP-DMBE2 to TW_SAIDA-EMENGE,
                     T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                     T_BSIS_DP-BUDAT to TW_SAIDA-AEDAT,
                     'FIPA'          to TW_SAIDA-BELNR2.

              read table S_BUDAT index 1.
              if T_BSIS_DP-BUDAT > S_BUDAT-HIGH.
*---> 13/06/2023 - Migração S4 - JS
*            move T_BSIS_DP-DMBTR to TW_SAIDA-TOTAPG.
                TW_SAIDA-TOTAPG = conv #( T_BSIS_DP-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS

                clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
              endif.

              TW_SAIDA-EBELN = T_EKKO-EBELN.
              TW_SAIDA-EBELP = T_EKKO-EBELP.
              TW_SAIDA-ZEKKN = T_EKKO-ZEKKN.
              if TW_SAIDA-AEDAT in S_BUDAT
              or TW_SAIDA-TOTAPG is not initial.
                append TW_SAIDA.
              endif.
*        DELETE t_bsis_dp.
              clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE,
                     TW_SAIDA-TOTPG,  TW_SAIDA-BELNR2, TW_SAIDA-TOTPG, TW_SAIDA-TOTAPG.
            endloop.
          endif. " igor vielal - correção
        endif.
      endloop.
    else.  "adiantamento !
      read table T_BSIS_LB
        with key BUKRS = T_IMAK-ABUKRS
                 GJAHR = T_EKBEC-GJAHR
                 BELNR = T_EKBEC-BELNR.

      if SY-SUBRC is not initial.
        read table T_BSAS_LB into T_BSIS_LB
                with key BUKRS = T_IMAK-ABUKRS
                         GJAHR = T_EKBEC-GJAHR
                         BELNR = T_EKBEC-BELNR.

*        move: t_bsas_lb-bukrs  to t_bsis_lb-bukrs,
*              t_bsas_lb-hkont  to t_bsis_lb-hkont.

      endif.

      if SY-SUBRC is initial.
        read table T_SETLEAF
          with key VALFROM = T_BSIS_LB-HKONT.

        if SY-SUBRC is not initial.
          read table T_SKB1_LB
            with key BUKRS = T_BSIS_LB-BUKRS
                     SAKNR = T_BSIS_LB-HKONT.

        endif.
        if T_EKBEC-SHKZG ne 'S'
        or SY-SUBRC is not initial.
          continue.
        endif.
        read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                      binary search.
        if SY-SUBRC = 0.
          concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
            separated by ' - '.
        endif.

        if P_SINCAX is not initial
        or P_ANALIT is not initial
        or P_CONSOL is not initial. "t_ekbec-vgabe EQ 3.
          clear: T_BKPF_LB.
          read table T_BKPF_LB
            with key BUKRS = T_BSIS_LB-BUKRS
                     BELNR = T_BSIS_LB-BELNR
                     GJAHR = T_BSIS_LB-GJAHR.

          clear: T_EKKO3.
          read table T_EKKO3
          with key EBELN = T_EKBEC-EBELN
                   EBELP = T_EKBEC-EBELP
                   BELNR = T_EKBEC-BELNR.

          if not T_EKKO3-PORCENTO is initial.
*            t_ekbec-dmbtr = t_ekbec-dmbtr * t_ekko3-porcento.  "aaaa
*            t_bsis_dp-dmbe2 = t_bsis_dp-dmbe2 * t_ekko3-porcento.  "aaaa

            T_EKBEC-DMBTR = T_EKBEC-DMBTR * T_EKKO3-PORCENTO.  "aaaa
            T_EKBEC-WRBTR = T_EKBEC-WRBTR * T_EKKO3-PORCENTO.  "aaaa
*            TRY.
*                T_BSIS_LB-DMBE2 = T_EKBEC-DMBTR / T_BKPF_LB-KURS2.
*              CATCH CX_SY_ZERODIVIDE .
*            ENDTRY.
*            T_BSIS_LB-DMBE2 = T_BSIS_LB-DMBE2 * T_EKKO3-PORCENTO.  "aaaa
          endif.
        else.
          if not T_EKKO-PORCENTO is initial.
*            t_ekbec-dmbtr   = t_ekbec-dmbtr * t_ekko-porcento.
*            t_bsis_dp-dmbe2 = t_bsis_dp-dmbe2 * t_ekko-porcento.

            T_EKBEC-DMBTR   = T_EKBEC-DMBTR * T_EKKO-PORCENTO.
            T_BSIS_LB-DMBE2 = T_BSIS_LB-DMBE2 * T_EKKO-PORCENTO.
          endif.
        endif.
        multiply T_EKBEC-DMBTR by -1.

        if T_BSIS_LB-SHKZG = 'H'. "t_bsis_dp-shkzg = 'H'.
*          t_bsis_dp-dmbe2 = t_bsis_dp-dmbe2 * ( - 1 ).
          T_BSIS_LB-DMBE2 = T_BSIS_LB-DMBE2 * ( - 1 ).

        endif.

        if P_MOEDA eq  'USD'.
*          move: t_bsis_dp-dmbe2 to t_ekbec-dmbtr.
          if T_EKBEC-WAERS eq 'EUR'.
            T_BSIS_LB-DMBE2 = T_EKBEC-WRBTR * T_BKPF_LB-KURS2.
          else.
*            try.
*                t_bsis_lb-dmbe2 = t_ekbec-dmbtr / t_bkpf_lb-kurs2.
*              catch cx_sy_zerodivide .
*            endtry.
            if T_BKPF_LB-KURS2 is not initial.
              move : T_EKBEC-DMBTR to T_BSIS_LB-DMBE2.
              divide T_BSIS_LB-DMBE2 by T_BKPF_LB-KURS2.
            else.
              T_BKPF_LB-KURS2 = T_BSIS_LB-DMBTR / T_BSIS_LB-DMBE2.
              move : T_EKBEC-DMBTR to T_BSIS_LB-DMBE2.
              divide T_BSIS_LB-DMBE2 by T_BKPF_LB-KURS2.
            endif.
          endif.
          if T_BSIS_LB-DMBE2 gt 0.
            multiply T_BSIS_LB-DMBE2 by -1.
          endif.
          move: T_BSIS_LB-DMBE2 to T_EKBEC-DMBTR.
        endif.


        TW_SAIDA-BELNR3 = T_EKBEC-BELNR.
        move: "t_bsak-belnr    TO tw_saida-belnr3,
              T_EKBEC-BELNR to TW_SAIDA-BELNR6,
              T_EKBEC-DMBTR to TW_SAIDA-TOTPG,
              T_EKBEC-DMBTR to TW_SAIDA-NETPR,
*            t_bsis_dp-dmbe2 TO tw_saida-emenge,
              T_EKBEC-BUDAT to TW_SAIDA-AEDAT,
              T_EKBEC-BUDAT to TW_SAIDA-AEDAT,
              'FIPA'        to TW_SAIDA-BELNR2.

        TW_SAIDA-EBELN = T_EKKO-EBELN.
        TW_SAIDA-EBELP = T_EKKO-EBELP.

        read table TW_SAIDA into WL_SAIDA
          with key EBELN  = TW_SAIDA-EBELN
                   EBELP  = TW_SAIDA-EBELP
                   BELNR1 = TW_SAIDA-BELNR1
                   BELNR2 = TW_SAIDA-BELNR2
                   BELNR3 = TW_SAIDA-BELNR3
                   BELNR4 = TW_SAIDA-BELNR4
                   BELNR6 = TW_SAIDA-BELNR6.
        if SY-SUBRC is not initial.
          if TW_SAIDA-AEDAT in S_BUDAT.
            append TW_SAIDA.
          endif.
        endif.
      endif.
      clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE, TW_SAIDA-BELNR6,
             TW_SAIDA-TOTPG,  TW_SAIDA-BELNR2, TW_SAIDA-TOTPG, TW_SAIDA-TOTAPG,
             T_BSIS_LB.

    endif.
    clear TW_SAIDA-BELNR5.
  endloop.

  refresh: TL_SAIDA.
  clear: WL_ADIANTAMENTO, T_EKBEC, WL_TOTAL_ITEM.


  TL_SAIDA[] = TW_SAIDA[].
  delete TL_SAIDA where EBELN ne T_EKKO-EBELN
                     or EBELP ne T_EKKO-EBELP
                    or BELNR2 ne 'FIPA'.
*                    AND BELNR2 NE 'FIAP'.
  loop at TL_SAIDA.
    if TL_SAIDA-ANLN1 is not initial.
      read table T_EKBEC
      with key EBELN = TL_SAIDA-EBELN
               EBELP = TL_SAIDA-EBELP
               ANLN1 = TL_SAIDA-ANLN1
               BELNR = TL_SAIDA-BELNR3
               VGABE = 2.

    elseif TL_SAIDA-AUFNR is not initial.
      read table T_EKBEC
      with key EBELN = TL_SAIDA-EBELN
               EBELP = TL_SAIDA-EBELP
               AUFNR = TL_SAIDA-AUFNR
               BELNR = TL_SAIDA-BELNR3
               VGABE = 2.
    endif.
    if SY-SUBRC is initial.
*      ADD 1 TO WL_LINES.
      add T_EKBEC-DMBTR to WL_TOTAL_ITEM.
    endif.
  endloop.

  read table T_EKBEC transporting no fields
    with key EBELN = T_EKKO-EBELN
             EBELP = T_EKKO-EBELP
             VGABE = 4
             SHKZG = 'H'.

  if SY-SUBRC eq 0.
    loop at TW_SAIDA into WL_SAIDA  where EBELN eq T_EKKO-EBELN
                       and EBELP eq T_EKKO-EBELP
                       and BELNR2 eq 'FIPA'.
      WL_TABIX = SY-TABIX.
      if WL_SAIDA-ANLN1 is not initial.
        read table T_EKBEC
             with key EBELN = WL_SAIDA-EBELN
                      EBELP = WL_SAIDA-EBELP
                      ANLN1 = WL_SAIDA-ANLN1
                      BELNR = WL_SAIDA-BELNR3
                      VGABE = 2.

      elseif WL_SAIDA-AUFNR is not initial.
        read table T_EKBEC
           with key EBELN = WL_SAIDA-EBELN
                    EBELP = WL_SAIDA-EBELP
                    AUFNR = WL_SAIDA-AUFNR
                    BELNR = WL_SAIDA-BELNR3
                    VGABE = 2.
      endif.
      if SY-SUBRC is initial.
        try.
            WL_SAIDA-TOTPG = ( WL_SAIDA-TOTPG * ( T_EKBEC-DMBTR  / WL_TOTAL_ITEM ) ).
            WL_SAIDA-NETPR = WL_SAIDA-TOTPG.
            if WL_SAIDA-TOTAPG is not initial.
              WL_SAIDA-TOTAPG = ( WL_SAIDA-TOTAPG * ( T_EKBEC-DMBTR  / WL_TOTAL_ITEM ) ).
              WL_SAIDA-NETPR = WL_SAIDA-TOTAPG.
            endif.
          catch CX_SY_ZERODIVIDE.
        endtry.
        modify TW_SAIDA from WL_SAIDA index WL_TABIX.
      endif.
      clear: WL_SAIDA.
    endloop.
  endif.
endform.                    " F_MONTA_DADOS_CAIXA
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_CAIXA_APG
*&---------------------------------------------------------------------*
form F_MONTA_DADOS_CAIXA_APG .
  loop at T_EKBEA where EBELN = T_EKKO-EBELN and "AND
                        EBELP = T_EKKO-EBELP and
                        ( VGABE = 2 or VGABE = 3 ).

*** igor vilela - inicio
    if P_SINCAX is not initial
    or P_CONSOL is not initial.
      clear: TW_SAIDA-BELNR1.
    endif.
    TW_SAIDA-BELNR5 = T_EKBEA-BELNR.
*** igor vilela - fim

    if not T_EKBEA-ANLN1 is initial.
      if T_EKBEA-ANLN1 ne T_EKKO-ANLN1.
        continue.
      endif.
    endif.

*    IF P_SINCAX IS INITIAL.
    if not T_EKBEA-AUFNR is initial.
      if T_EKBEA-AUFNR ne T_EKKO-AUFNR.
        continue.
      endif.
    endif.
*    ENDIF.
    read table WE_BKPF with key AWKEY = T_EKBEA-AWKEY
                                binary search.
    check SY-SUBRC is initial.

    read table T_BSAK_AP with key BUKRS = WE_BKPF-BUKRS
                                  BELNR = WE_BKPF-BELNR
                                  GJAHR = WE_BKPF-GJAHR.

    if SY-SUBRC = 0.
      loop at T_BSAK_AP where BUKRS = WE_BKPF-BUKRS and
                              BELNR = WE_BKPF-BELNR and
                              GJAHR = WE_BKPF-GJAHR.


        perform F_MONTA_DADOS_CAIXA_APG_BSIK using T_BSAK_AP-BUKRS
                                                   T_BSAK_AP-AUGBL
                                                   T_BSAK_AP-GJAHR.

        if P_SINCAX is not initial
        or P_CONSOL is not initial.
          loop at T_BSAK_AP where BUKRS = T_BSAK_AP-BUKRS and
                                  BELNR = T_BSAK_AP-AUGBL and
                                  GJAHR = T_BSAK_AP-GJAHR.

            read table TW_SAIDA transporting no fields
              with key EBELN = T_EKKO-EBELN
                       EBELP = T_EKKO-EBELP
                       BELNR2 = 'FIAP'
                       BELNR3 = T_BSAK_AP-AUGBL.

            if SY-SUBRC is not initial.
              perform F_MONTA_DADOS_CAIXA_APG_BSIK using T_BSAK_AP-BUKRS
                                                         T_BSAK_AP-AUGBL
                                                         T_BSAK_AP-GJAHR.
            endif.
          endloop.
        endif.
      endloop.
    else.
      perform F_MONTA_DADOS_CAIXA_APG_BSIK using WE_BKPF-BUKRS
                                                 WE_BKPF-BELNR
                                                 WE_BKPF-GJAHR.
    endif.
    clear: TW_SAIDA-BELNR5.
  endloop.
endform.                    " F_MONTA_DADOS_CAIXA_APG
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_LAYOUT_CX1
*&---------------------------------------------------------------------*
form F_MODIFICA_LAYOUT_CX1 .

** Dados CAIXA
  data: LS_FIELDCATALOG type LVC_S_FCAT.
  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.
    if W_PGMT is initial.
      case LS_FIELDCATALOG-FIELDNAME.
        when 'NETWR' .
          LS_FIELDCATALOG-COL_POS = 1.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Pedido'.
        when 'DMBTR'.
          LS_FIELDCATALOG-COL_POS = 2.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
          LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
          LS_FIELDCATALOG-COLTEXT = 'Realizado'.
        when 'TOTPG'.
          LS_FIELDCATALOG-COL_POS = 3.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pago'.
        when 'TOTAPG'.
          LS_FIELDCATALOG-COL_POS = 4.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pagar'.
        when 'IMP_TOTAL'.
          LS_FIELDCATALOG-COL_POS = 11.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S = 'Tot. Impostos'.
          LS_FIELDCATALOG-SCRTEXT_M = 'Tot. Impostos'.
          LS_FIELDCATALOG-COLTEXT = 'Tot. Impostos'.
        when others.
          LS_FIELDCATALOG-NO_OUT = 'X'.
          LS_FIELDCATALOG-KEY    = ''.
      endcase.
    else.
      case LS_FIELDCATALOG-FIELDNAME.
        when 'NETWR' .
          LS_FIELDCATALOG-COL_POS = 1.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Pedido'.
        when 'DMBTR'.
          LS_FIELDCATALOG-COL_POS = 2.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
          LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
          LS_FIELDCATALOG-COLTEXT = 'Realizado'.
        when 'TOTPG'.
          LS_FIELDCATALOG-COL_POS = 3.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pago'.
        when 'TOTAPG'.
          LS_FIELDCATALOG-COL_POS = 4.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pagar'.
        when 'PAGO30'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '0-30 dias'.
        when 'PAGO60'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '31-60 dias'.
        when 'PAGO90'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '61-90 dias'.
        when 'PAGO120'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '91-120 dias'.
        when 'PAGO150'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '121-150 dias'.
        when 'PAGO180'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '151-180 dias'.
        when 'PAGO210'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '181-210 dias'.
        when 'PAGO999'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '211-999 dias'.
        when others.
          LS_FIELDCATALOG-NO_OUT = 'X'.
          LS_FIELDCATALOG-KEY    = ''.
      endcase.
    endif.


    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.

endform.                    " F_MODIFICA_LAYOUT_CX1
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_LAYOUT_CX2
*&---------------------------------------------------------------------*
form F_MODIFICA_LAYOUT_CX2 .

** change fieldcatalog
  data: LS_FIELDCATALOG type LVC_S_FCAT.
*        w_pos TYPE i.
*  CLEAR w_pos.
  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.
    if W_PGMT is initial.
      case LS_FIELDCATALOG-FIELDNAME.

        when 'NETWR' .
          LS_FIELDCATALOG-COL_POS = 1.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Pedido'.
        when 'DMBTR'.
          LS_FIELDCATALOG-COL_POS = 2.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
          LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
          LS_FIELDCATALOG-COLTEXT = 'Realizado'.
        when 'TOTPG'.
          LS_FIELDCATALOG-COL_POS = 3.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pago'.
        when 'TOTAPG'.
          LS_FIELDCATALOG-COL_POS = 4.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pagar'.
        when 'EBELN'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-OUTPUTLEN = 15.
          LS_FIELDCATALOG-KEY    = 'X'.
          LS_FIELDCATALOG-HOTSPOT = 'X'.
        when 'EBELP'.
          LS_FIELDCATALOG-COL_POS = 6.
          LS_FIELDCATALOG-OUTPUTLEN = 8.
          LS_FIELDCATALOG-KEY    = 'X'.
        when 'AEDAT'.
          LS_FIELDCATALOG-COL_POS = 7.
          LS_FIELDCATALOG-OUTPUTLEN = 15.
          LS_FIELDCATALOG-KEY    = 'X'.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Data'.
        when 'BELNR2'.
          LS_FIELDCATALOG-COL_POS = 8.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tipo Doc.'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
        when 'BELNR1' .
          LS_FIELDCATALOG-COL_POS = 9.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Docum. (MM)'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-HOTSPOT = 'X'.
        when 'BELNR3' .
          LS_FIELDCATALOG-COL_POS = 10.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Doc.Contábil'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-HOTSPOT = 'X'.

        when 'OBJETO' .
          LS_FIELDCATALOG-COL_POS = 12.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Objeto'.
          LS_FIELDCATALOG-OUTPUTLEN = 30.
*        ls_fieldcatalog-hotspot = 'X'.


        when 'NFENUM' .
          LS_FIELDCATALOG-COL_POS = 13.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Nº NF'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
        when 'NETPR' .
          LS_FIELDCATALOG-COL_POS = 14.
*        ls_fieldcatalog-do_sum = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Vl. Docum.'.
*** colocar campo de valor.
        when 'MATNR' . LS_FIELDCATALOG-COL_POS = 15.
        when 'TXZ01' . LS_FIELDCATALOG-COL_POS = 16.
        when 'SRVPOS' . LS_FIELDCATALOG-COL_POS = 17.
        when 'KTEXT1'.
          LS_FIELDCATALOG-COL_POS = 18.
          LS_FIELDCATALOG-SCRTEXT_S = 'Descrição'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Texto Breve (Desc.Serviço)'.
        when 'EXTROW'.
          LS_FIELDCATALOG-COL_POS = 19.
          LS_FIELDCATALOG-SCRTEXT_S = 'Linha Serviço'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Linha (Serviço)'.
        when 'MENTE' . LS_FIELDCATALOG-COL_POS = 20.
        when 'MEINS'.
          LS_FIELDCATALOG-OUTPUTLEN = 10.
          LS_FIELDCATALOG-COL_POS = 21.
        when 'FORNECEDOR'.
          LS_FIELDCATALOG-COL_POS = 22.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Fornecedor'.
        when 'EMISSOR'.
          LS_FIELDCATALOG-COL_POS = 23.
          LS_FIELDCATALOG-SCRTEXT_S = 'Emissor'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Emissor da fatura'.

        when others.
          LS_FIELDCATALOG-NO_OUT = 'X'.
          LS_FIELDCATALOG-KEY    = ''.
      endcase.

    else.
      case LS_FIELDCATALOG-FIELDNAME.

        when 'NETWR' .
          LS_FIELDCATALOG-COL_POS = 1.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Pedido'.
        when 'DMBTR'.
          LS_FIELDCATALOG-COL_POS = 2.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
          LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
          LS_FIELDCATALOG-COLTEXT = 'Realizado'.
        when 'TOTPG'.
          LS_FIELDCATALOG-COL_POS = 3.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pago'.
        when 'TOTAPG'.
          LS_FIELDCATALOG-COL_POS = 4.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tot. Pagar'.
        when 'EBELN'.
          LS_FIELDCATALOG-COL_POS = 5.
          LS_FIELDCATALOG-OUTPUTLEN = 15.
          LS_FIELDCATALOG-KEY    = 'X'.
          LS_FIELDCATALOG-HOTSPOT = 'X'.
        when 'EBELP'.
          LS_FIELDCATALOG-COL_POS = 6.
          LS_FIELDCATALOG-OUTPUTLEN = 8.
          LS_FIELDCATALOG-KEY    = 'X'.
        when 'AEDAT'.
          LS_FIELDCATALOG-COL_POS = 7.
          LS_FIELDCATALOG-OUTPUTLEN = 15.
          LS_FIELDCATALOG-KEY    = 'X'.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Data'.
        when 'BELNR2'.
          LS_FIELDCATALOG-COL_POS = 8.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Tipo Doc.'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
        when 'BELNR1' .
          LS_FIELDCATALOG-COL_POS = 9.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Docum. (MM)'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-HOTSPOT = 'X'.
        when 'BELNR3' .
          LS_FIELDCATALOG-COL_POS = 10.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Doc.Contábil'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-HOTSPOT = 'X'.

        when 'OBJETO' .
          LS_FIELDCATALOG-COL_POS = 11.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Objeto'.
          LS_FIELDCATALOG-OUTPUTLEN = 30.
*        ls_fieldcatalog-hotspot = 'X'.

        when 'NFENUM' .
          LS_FIELDCATALOG-COL_POS = 11.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Nº NF'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
        when 'NETPR' .
          LS_FIELDCATALOG-COL_POS = 12.
*        ls_fieldcatalog-do_sum = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 20.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Vl. Docum.'.
*** colocar campo de valor.
        when 'MATNR' . LS_FIELDCATALOG-COL_POS = 13.
        when 'TXZ01' . LS_FIELDCATALOG-COL_POS = 14.
        when 'SRVPOS' . LS_FIELDCATALOG-COL_POS = 15.
        when 'KTEXT1'.
          LS_FIELDCATALOG-COL_POS = 16.
          LS_FIELDCATALOG-SCRTEXT_S = 'Descrição'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Texto Breve (Desc.Serviço)'.
        when 'EXTROW'.
          LS_FIELDCATALOG-COL_POS = 17.
          LS_FIELDCATALOG-SCRTEXT_S = 'Linha Serviço'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Linha (Serviço)'.
        when 'MENTE' . LS_FIELDCATALOG-COL_POS = 18.
        when 'MEINS'.
          LS_FIELDCATALOG-OUTPUTLEN = 10.
          LS_FIELDCATALOG-COL_POS = 19.
        when 'FORNECEDOR'.
          LS_FIELDCATALOG-COL_POS = 20.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Fornecedor'.
        when 'EMISSOR'.
          LS_FIELDCATALOG-COL_POS = 21.
          LS_FIELDCATALOG-SCRTEXT_S = 'Emissor'.
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = 'Emissor da fatura'.


        when 'PAGO30'.
          LS_FIELDCATALOG-COL_POS = 22.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '0-30 dias'.
        when 'PAGO60'.
          LS_FIELDCATALOG-COL_POS = 23.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '31-60 dias'.
        when 'PAGO90'.
          LS_FIELDCATALOG-COL_POS = 24.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '61-90 dias'.
        when 'PAGO120'.
          LS_FIELDCATALOG-COL_POS = 25.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '91-120 dias'.
        when 'PAGO150'.
          LS_FIELDCATALOG-COL_POS = 26.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '121-150 dias'.
        when 'PAGO180'.
          LS_FIELDCATALOG-COL_POS = 27.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '151-180 dias'.
        when 'PAGO210'.
          LS_FIELDCATALOG-COL_POS = 28.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '181-210 dias'.
        when 'PAGO999'.
          LS_FIELDCATALOG-COL_POS = 29.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S =
          LS_FIELDCATALOG-SCRTEXT_M =
          LS_FIELDCATALOG-COLTEXT = '211-999 dias'.


        when others.
          LS_FIELDCATALOG-NO_OUT = 'X'.
          LS_FIELDCATALOG-KEY    = ''.
      endcase.
    endif.
    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.

endform.                    " F_MODIFICA_LAYOUT_CX2
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_CAIXA_APG_BSIK
*&---------------------------------------------------------------------*
form F_MONTA_DADOS_CAIXA_APG_BSIK using P_BUKRS
                                        P_AUGBL
                                        P_GJAHR.

  data: WL_FATOR type TCURR-UKURS,
        WL_SAIDA like line of TW_SAIDA.
*        wl_data  TYPE sy-datum.

  loop at T_BSIK where BUKRS = P_BUKRS and
                       BELNR = P_AUGBL and
                       GJAHR = P_GJAHR.
*    DELETE t_bsik.
    read table T_LFA1 with key LIFNR = T_BSIK-LIFNR
                               binary search.
    if SY-SUBRC = 0.
      concatenate T_LFA1-NAME1 T_BSIK-LIFNR into TW_SAIDA-FORNECEDOR
        separated by ' - '.
    endif.

    read table T_DOCS_AG
      with key BUKRS = T_BSIK-BUKRS
               BELNR = T_BSIK-BELNR.
    if SY-SUBRC is initial.
      if T_DOCS_AG-AG eq 'X'.
        T_BSIK-BLART = 'AG'.
      else.
        T_BSIK-BLART = 'AB'.
      endif.
    endif.
    if T_BSIK-BLART eq 'AG'.
      read table T_BSAK_AG
        with key BUKRS = T_BSIK-BUKRS
                 GJAHR = T_BSIK-GJAHR
                 AUGBL = T_BSIK-BELNR
                 BELNR = WE_BKPF-BELNR.

      move : T_BSAK_AG-DMBTR to T_BSIK-DMBTR,
             T_BSAK_AG-DMBE2 to T_BSIK-DMBE2.

    endif.
*    ENDIF.

    if ( P_MOEDA = 'BRL' )        and
       ( T_EKKO-WAERS = 'USD' or
         T_EKKO-WAERS = 'EUR'    ).

      if T_EKKO-WAERS = 'USD'.
        call function 'CONVERT_TO_LOCAL_CURRENCY'
          exporting
            DATE             = SY-DATUM
            FOREIGN_AMOUNT   = T_BSIK-DMBE2
            FOREIGN_CURRENCY = P_MOEDA
            LOCAL_CURRENCY   = T_EKKO-WAERS
            RATE             = 0
            TYPE_OF_RATE     = 'B'
            READ_TCURR       = 'X'
          importing
            EXCHANGE_RATE    = WL_FATOR
          exceptions
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            DERIVED_2_TIMES  = 5
            others           = 6.


        if WL_FATOR < 0.
          WL_FATOR = WL_FATOR * ( - 1 ).
        endif.
        if WL_FATOR > 0.
          T_BSIK-DMBTR = T_BSIK-DMBE2 * WL_FATOR.
        endif.
      endif.
    elseif ( P_MOEDA ne 'BRL' ).
      call function 'CONVERT_TO_LOCAL_CURRENCY'
        exporting
          DATE             = SY-DATUM
          FOREIGN_AMOUNT   = T_BSIK-DMBE2
          FOREIGN_CURRENCY = P_MOEDA
          LOCAL_CURRENCY   = T_EKKO-WAERS
          RATE             = 0
          TYPE_OF_RATE     = 'B'
          READ_TCURR       = 'X'
        importing
          EXCHANGE_RATE    = WL_FATOR
        exceptions
          NO_RATE_FOUND    = 1
          OVERFLOW         = 2
          NO_FACTORS_FOUND = 3
          NO_SPREAD_FOUND  = 4
          DERIVED_2_TIMES  = 5
          others           = 6.

      if WL_FATOR < 0.
        WL_FATOR = WL_FATOR * ( - 1 ).
      endif.

      if WL_FATOR ne 0.
        T_BSIK-DMBTR = T_BSIK-DMBE2 / WL_FATOR.
      endif.

    endif.

    if T_BSIK-SHKZG = 'H'.
      T_BSIK-DMBTR = T_BSIK-DMBTR * ( - 1 ) .
      T_BSIK-DMBE2 = T_BSIK-DMBE2 * ( - 1 ) .
    endif.

    if P_SINCAX is not initial
    or P_CONSOL is not initial. "t_ekbea-vgabe EQ 3.
      clear T_EKKO3.
      if T_EKBEA-AUFNR is not initial.
        read table T_EKKO3
          with key EBELN = T_EKBEA-EBELN
                   EBELP = T_EKBEA-EBELP
                   BELNR = T_EKBEA-BELNR
                   AUFNR = T_EKBEA-AUFNR.

      elseif T_EKBEA-ANLN1 is not initial.
        read table T_EKKO3
          with key EBELN = T_EKBEA-EBELN
                   EBELP = T_EKBEA-EBELP
                   BELNR = T_EKBEA-BELNR
                   ANLN1 = T_EKBEA-ANLN1.
      endif.

      if T_EKKO3-PORCENTO is not initial.
        T_BSIK-DMBTR = T_BSIK-DMBTR * T_EKKO3-PORCENTO.
        T_BSIK-DMBE2 = T_BSIK-DMBE2 * T_EKKO3-PORCENTO.
      elseif T_EKKO3-VPROZ is not initial.
        T_BSIK-DMBTR = T_BSIK-DMBTR * ( T_EKKO3-VPROZ / 100 ).
        T_BSIK-DMBE2 = T_BSIK-DMBE2 * ( T_EKKO3-VPROZ / 100 ).
      endif.
      if T_EKBEA-VGABE eq 3.
        read table T_EKBE
          with key EBELN = T_EKKO3-EBELN
                   EBELP = T_EKKO3-EBELP
                   VGABE = 3.
        move: T_EKBE-BELNR to T_BSIK-BELNR.
      endif.
    else.
      if T_EKKO-PORCENTO is not initial.
        T_BSIK-DMBTR = T_BSIK-DMBTR * T_EKKO-PORCENTO.
        T_BSIK-DMBE2 = T_BSIK-DMBE2 * T_EKKO-PORCENTO.
      elseif T_EKKO-VPROZ is not initial.
        T_BSIK-DMBTR = T_BSIK-DMBTR * ( T_EKKO-VPROZ / 100 ).
        T_BSIK-DMBE2 = T_BSIK-DMBE2 * ( T_EKKO-VPROZ / 100 ).
      endif.
    endif.
    if P_MOEDA eq 'USD'.
      move: T_BSIK-DMBE2 to T_BSIK-DMBTR.
    endif.
    move: T_BSIK-BELNR to TW_SAIDA-BELNR3.
*---> 13/06/2023 - Migração S4 - JS
*            T_BSIK-DMBTR to TW_SAIDA-TOTAPG,
    TW_SAIDA-TOTAPG = conv #( T_BSIK-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
    move: T_BSIK-DMBTR to TW_SAIDA-NETPR,
          T_BSIK-DMBE2 to TW_SAIDA-EMENGE,
*          t_bsik-budat TO tw_saida-aedat,
          'FIAP'       to TW_SAIDA-BELNR2,
***          igor Vilela adição de campo buzei - inicio
          T_BSIK-BUZEI to TW_SAIDA-BUZEI.
***          igor Vilela adição de campo buzei - Fim



    TW_SAIDA-EBELN = T_EKKO-EBELN.
    TW_SAIDA-EBELP = T_EKKO-EBELP.
    clear: W_DIAS, W_PAGAT.
*    w_pagat = t_bsik-zfbdt + t_bsik-zbd1t.
    W_PAGAT = T_BSIK-BUDAT.
    TW_SAIDA-AEDAT = W_PAGAT.
    W_DIAS = W_PAGAT - SY-DATUM.

    if W_DIAS le 30.
*---> 13/06/2023 - Migração S4 - JS
*      tw_saida-pago30 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 31 AND
*           w_dias LE 60.
*      tw_saida-pago60 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 61 AND
*           w_dias LE 90.
*      tw_saida-pago90 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 91 AND
*           w_dias LE 120.
*      tw_saida-pago120 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 121 AND
*           w_dias LE 150.
*      tw_saida-pago150 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 151 AND
*           w_dias LE 180.
*      tw_saida-pago180 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 181 AND
*           w_dias LE 210.
*      tw_saida-pago210 = t_bsik-dmbtr.
*    ELSEIF w_dias GE 211.
*      tw_saida-pago999 = t_bsik-dmbtr.

      TW_SAIDA-PAGO30 = conv   #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 31 and
           W_DIAS le 60.
      TW_SAIDA-PAGO60 = conv   #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 61 and
           W_DIAS le 90.
      TW_SAIDA-PAGO90 = conv   #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 91 and
           W_DIAS le 120.
      TW_SAIDA-PAGO120 = conv  #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 121 and
           W_DIAS le 150.
      TW_SAIDA-PAGO150 = conv  #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 151 and
           W_DIAS le 180.
      TW_SAIDA-PAGO180 = conv   #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 181 and
           W_DIAS le 210.
      TW_SAIDA-PAGO210 = conv  #( T_BSIK-DMBTR ).
    elseif W_DIAS ge 211.
      TW_SAIDA-PAGO999 = conv  #( T_BSIK-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
    endif.

    if P_SINCAX is initial
    and P_CONSOL is initial.
      if TW_SAIDA-AEDAT in S_BUDAT.
        append TW_SAIDA.
      endif.
    else.
      if T_BSIK-UMSKS is not initial.
        continue.
      endif.
      read table TW_SAIDA into WL_SAIDA
        with key EBELN  = TW_SAIDA-EBELN
                 EBELP  = TW_SAIDA-EBELP
                 BELNR1 = TW_SAIDA-BELNR1
                 BELNR2 = TW_SAIDA-BELNR2
                 BELNR3 = TW_SAIDA-BELNR3
                 BELNR4 = TW_SAIDA-BELNR4
                 AUFNR  = TW_SAIDA-AUFNR
                 BUZEI  = TW_SAIDA-BUZEI.
      if SY-SUBRC is initial.
*        ADD: wl_saida-totapg TO tw_saida-totapg,
*             wl_saida-netpr  TO tw_saida-netpr,
*             wl_saida-emenge TO tw_saida-emenge,
*             wl_saida-pago30 TO tw_saida-pago30,
*             wl_saida-pago60 TO tw_saida-pago60,
*             wl_saida-pago90 TO tw_saida-pago90,
*             wl_saida-pago120 TO tw_saida-pago120,
*             wl_saida-pago150 TO tw_saida-pago150,
*             wl_saida-pago180 TO tw_saida-pago180,
*             wl_saida-pago210 TO tw_saida-pago210,
*             wl_saida-pago999 TO tw_saida-pago999.
*
*        MODIFY tw_saida INDEX sy-tabix.
      else.
        read table TW_SAIDA into WL_SAIDA
         with key EBELN = TW_SAIDA-EBELN
*                  belnr1 = tw_saida-belnr1
                  BELNR2 = TW_SAIDA-BELNR2
                  BELNR3 = TW_SAIDA-BELNR3
                  BELNR4 = TW_SAIDA-BELNR4
                  BUZEI  = TW_SAIDA-BUZEI.

*        IF sy-subrc IS INITIAL.
*          ADD: wl_saida-totapg TO tw_saida-totapg,
*               wl_saida-netpr  TO tw_saida-netpr,
*               wl_saida-emenge TO tw_saida-emenge,
*               wl_saida-pago30 TO tw_saida-pago30,
*               wl_saida-pago60 TO tw_saida-pago60,
*               wl_saida-pago90 TO tw_saida-pago90,
*               wl_saida-pago120 TO tw_saida-pago120,
*               wl_saida-pago150 TO tw_saida-pago150,
*               wl_saida-pago180 TO tw_saida-pago180,
*               wl_saida-pago210 TO tw_saida-pago210,
*               wl_saida-pago999 TO tw_saida-pago999.
*
*          MODIFY tw_saida INDEX sy-tabix.
*        ELSE.
        if TW_SAIDA-AEDAT in S_BUDAT.
          append TW_SAIDA.
        endif.
*        ENDIF.
      endif.
    endif.
    clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-EMENGE, TW_SAIDA-BELNR2,
           TW_SAIDA-TOTAPG, TW_SAIDA-PAGO30, TW_SAIDA-PAGO90, TW_SAIDA-PAGO120,
           TW_SAIDA-PAGO150, TW_SAIDA-PAGO180, TW_SAIDA-PAGO210, TW_SAIDA-PAGO999,
           TW_SAIDA-PAGO60, TW_SAIDA-TOTAPG, WL_SAIDA, TW_SAIDA-BUZEI.
  endloop.

endform.                    " F_MONTA_DADOS_CAIXA_APG_BSIK
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_LAYOUT_CX3
*&---------------------------------------------------------------------*
form F_MODIFICA_LAYOUT_CX3 .


** change fieldcatalog
  data: LS_FIELDCATALOG type LVC_S_FCAT.
*        w_pos TYPE i.
*  CLEAR w_pos.
  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.

    case LS_FIELDCATALOG-FIELDNAME.

      when 'NETWR' .
        LS_FIELDCATALOG-COL_POS = 1.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido'.
      when 'DMBTR'.
        LS_FIELDCATALOG-COL_POS = 2.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
        LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
        LS_FIELDCATALOG-COLTEXT = 'Realizado'.
      when 'TOTPG'.
        LS_FIELDCATALOG-COL_POS = 3.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tot. Pago'.
      when 'TOTAPG'.
        LS_FIELDCATALOG-COL_POS = 4.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tot. Pagar'.
      when 'IMP_TOTAL' .
        LS_FIELDCATALOG-COL_POS = 4.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-SCRTEXT_S = 'Total de Impostos'.
        LS_FIELDCATALOG-SCRTEXT_M = 'Total de Impostos'.
        LS_FIELDCATALOG-COLTEXT = 'Total de Impostos'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-HOTSPOT = ' '.
      when 'EBELN'.
        LS_FIELDCATALOG-COL_POS = 5.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'EBELP'.
        LS_FIELDCATALOG-COL_POS = 6.
        LS_FIELDCATALOG-OUTPUTLEN = 8.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'AEDAT'.
        LS_FIELDCATALOG-COL_POS = 7.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Data'.
      when 'BELNR2'.
        LS_FIELDCATALOG-COL_POS = 8.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tipo Doc.'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
      when 'BELNR3' .
        LS_FIELDCATALOG-COL_POS = 10.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Doc.Contábil'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'BELNR6' .
        LS_FIELDCATALOG-COL_POS = 11.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Doc.Comp.'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'BELNR5' .
        LS_FIELDCATALOG-COL_POS = 11.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Doc.Miro'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'OBJETO' .
        LS_FIELDCATALOG-COL_POS = 12.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Objeto'.
        LS_FIELDCATALOG-OUTPUTLEN = 30.
*        ls_fieldcatalog-hotspot = 'X'.


      when 'NETPR' .
        LS_FIELDCATALOG-COL_POS = 13.
*        ls_fieldcatalog-do_sum = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Vl. Docum.'.
      when 'FORNECEDOR'.
        LS_FIELDCATALOG-COL_POS = 20.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Fornecedor'.
      when others.
        LS_FIELDCATALOG-NO_OUT = 'X'.
        LS_FIELDCATALOG-KEY    = ''.
    endcase.

    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.


endform.                    " F_MODIFICA_LAYOUT_CX3
*&---------------------------------------------------------------------*
*&      Form  F_AJUSTA_VALORES
*&---------------------------------------------------------------------*
form F_AJUSTA_VALORES .
  types: begin of TYL_RSEG,
           BELNR type RSEG-BELNR,
           GJAHR type RSEG-GJAHR,
           BUZEI type RSEG-BUZEI,
           EBELN type RSEG-EBELN,
           EBELP type RSEG-EBELP,
           WRBTR type RSEG-WRBTR,
           BNKAN type RSEG-BNKAN,
         end of TYL_RSEG,

         begin of TYL_EKBE_RATEIO,
           EBELN type EKBE-EBELN,
           EBELP type EKBE-EBELP,
           GJAHR type EKBE-GJAHR,
           BELNR type EKBE-BELNR,
           DMBTR type EKBE-DMBTR,
         end of TYL_EKBE_RATEIO.

  data: WL_TABIX       type SY-TABIX,
        WL_BELNR       type EKBE-BELNR,
        WL_VPROZ(5)    type P decimals 3,
        TL_EKBE_RATEIO type table of TYL_EKBE_RATEIO with header line,
        TL_RSEG        type table of TYL_RSEG with header line,
        TL_EKKN        type table of EKKN with header line.

  sort T_EKKO by EBELN EBELP ANLN1 ANLN2 AUFNR ZEKKN.
  delete adjacent duplicates from T_EKKO comparing EBELN EBELP ANLN1 ANLN2 AUFNR ZEKKN.

  data: begin of T_EKKO_TOTAL occurs 0,
          EBELN type EKKO-EBELN,
          BELNR type BKPF-BELNR,
          GJAHR type BKPF-GJAHR,
          VLTOT type EKPO-NETWR,
*          qtdtot type ekpo-menge,
        end of T_EKKO_TOTAL.

  data: begin of T_EKKO_TOTAL_QTD occurs 0,
          EBELN  type EKPO-EBELN,
          EBELP  type EKPO-EBELP,
          BELNR  type BKPF-BELNR,
          GJAHR  type BKPF-GJAHR,
          QTDTOT type EKPO-MENGE,
          DMBTR  type EKBE-DMBTR,
        end of T_EKKO_TOTAL_QTD.

  data: begin of T_EKKO_TOTAL3 occurs 0,
          EBELN type EKKO-EBELN,
          BELNR type BKPF-BELNR,
*          gjahr type bkpf-gjahr,
          VLTOT type EKPO-NETWR,
        end of T_EKKO_TOTAL3.


  data: begin of T_EKPO occurs 0,
          EBELN type EKPO-EBELN,
          EBELP type EKPO-EBELP,
          NETWR type EKPO-NETWR,
          KNTTP type EKPO-KNTTP,
          PSTYP type EKPO-PSTYP,
        end of T_EKPO.


*  data: t_ekko_total3 like table of t_ekko_total with header line.

  perform F_MSG using 'Selecionando EKPO'
                      '42'.
  select EBELN EBELP NETWR KNTTP PSTYP from EKPO into table T_EKPO
    for all entries in T_EKKO
    where EBELN = T_EKKO-EBELN.

  perform F_MSG using 'Selecionando EKBE'
                      '46'.

  if P_SINCAX is not initial
  or P_ANALIT is not initial
  or P_CONSOL is not initial.
    select EKBE~EBELN EKBE~EBELP EKBE~ZEKKN EKBE~VGABE EKBE~GJAHR EKBE~BELNR
         EKBE~BUZEI EKBE~BWART EKBE~BUDAT EKBE~MENGE EKBE~DMBTR EKBE~WRBTR EKBE~WAERS
         EKBE~LFBNR EKBE~CPUDT EKBE~SHKZG EKBE~XBLNR
         EKKN~ANLN1 EKKN~AUFNR
    from EKBE
    left outer join EKKN on
    ( EKBE~EBELN = EKKN~EBELN and
      EKBE~EBELP = EKKN~EBELP and
      EKBE~ZEKKN = EKKN~ZEKKN     )
    into table E_EKBE
    for all entries in T_EKPO
    where EKBE~EBELN eq T_EKPO-EBELN and
          EKBE~EBELP eq T_EKPO-EBELP and
          EKBE~VGABE in (2,3)        and
          EKBE~SHKZG eq 'S'.
  else.
    select EKBE~EBELN EKBE~EBELP EKBE~ZEKKN EKBE~VGABE EKBE~GJAHR EKBE~BELNR
          EKBE~BUZEI EKBE~BWART EKBE~BUDAT EKBE~MENGE EKBE~DMBTR EKBE~WRBTR EKBE~WAERS
          EKBE~LFBNR EKBE~CPUDT EKBE~SHKZG EKBE~XBLNR
          EKKN~ANLN1 EKKN~AUFNR
     from EKBE
     left outer join EKKN on
     ( EKBE~EBELN = EKKN~EBELN and
       EKBE~EBELP = EKKN~EBELP and
       EKBE~ZEKKN = EKKN~ZEKKN     )
     into table E_EKBE
     for all entries in T_EKPO
     where EKBE~EBELN eq T_EKPO-EBELN and
           EKBE~EBELP eq T_EKPO-EBELP and
           EKBE~VGABE eq 2.
  endif.


  if P_SINCAX is not initial
  or P_ANALIT is not initial
  or P_CONSOL is not initial.
    if T_EKBE[] is not initial.
      select *
        from EKKN
        into table TL_EKKN
         for all entries in T_EKBE
         where EBELN eq T_EKBE-EBELN
           and EBELP eq T_EKBE-EBELP.

    endif.

    if E_EKBE[] is not initial.
      select BELNR GJAHR STBLG
        from RBKP
        into table T_RBKP_EST
         for all entries in E_EKBE
         where BELNR eq E_EKBE-BELNR
           and GJAHR eq E_EKBE-GJAHR
           and STBLG ne SPACE.

      loop at E_EKBE.
        WL_TABIX = SY-TABIX.
        read table T_RBKP_EST
          with key BELNR = E_EKBE-BELNR
                   GJAHR = E_EKBE-GJAHR.

        if SY-SUBRC is initial.
          delete E_EKBE index WL_TABIX.
        endif.
      endloop.
    endif.
  endif.
*  IF p_sincax IS INITIAL
*  or p_consol is not initial.
*  IF P_CONSOL IS NOT INITIAL.
*    SORT E_EKBE BY EBELN EBELP VGABE.
*    LOOP AT T_EKPO.
*
*      READ TABLE E_EKBE WITH KEY EBELN = T_EKPO-EBELN
*                                 EBELP = T_EKPO-EBELP
*                                 VGABE = 2
*                                 BINARY SEARCH.
*      IF SY-SUBRC = 0.
*
*        MOVE: T_EKPO-EBELN TO T_EKKO_TOTAL-EBELN,
*              E_EKBE-BELNR TO T_EKKO_TOTAL-BELNR,
*              T_EKPO-NETWR TO T_EKKO_TOTAL-VLTOT.
*        COLLECT T_EKKO_TOTAL. CLEAR T_EKKO_TOTAL.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
  refresh: TL_RSEG.
  if E_EKBE[] is not initial.
    select BELNR GJAHR BUZEI EBELN EBELP WRBTR BNKAN
      from RSEG
      into table TL_RSEG
       for all entries in E_EKBE
       where BELNR eq E_EKBE-BELNR
         and GJAHR eq E_EKBE-GJAHR.

    sort: TL_RSEG by BELNR GJAHR.
    loop at TL_RSEG.
      move: TL_RSEG-EBELN to T_EKKO_TOTAL-EBELN,
            TL_RSEG-BELNR to T_EKKO_TOTAL-BELNR,
            TL_RSEG-GJAHR to T_EKKO_TOTAL-GJAHR.

      if TL_RSEG-WRBTR is not initial.
        move: TL_RSEG-WRBTR to T_EKKO_TOTAL-VLTOT.
      else.
        move: TL_RSEG-BNKAN to T_EKKO_TOTAL-VLTOT.
      endif.
      collect T_EKKO_TOTAL.
      clear: T_EKKO_TOTAL.
    endloop.
  endif.

  sort E_EKBE by EBELN EBELP VGABE.
  loop at T_EKPO.

*    READ TABLE e_ekbe WITH KEY ebeln = t_ekpo-ebeln
*                               ebelp = t_ekpo-ebelp
*                               vgabe = 2
*                               BINARY SEARCH.
    loop at E_EKBE where EBELN = T_EKPO-EBELN
                         and EBELP = T_EKPO-EBELP
                         and  VGABE = 2.

*        READ TABLE T_EKKO_TOTAL TRANSPORTING NO FIELDS
*          with key BELNR =

*      IF sy-subrc = 0.
*        READ TABLE TL_RSEG
*          WITH KEY BELNR = E_EKBE-BELNR
*                   GJAHR = E_EKBE-GJAHR
*                   BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          WL_TABIX = SY-TABIX.
*          LOOP AT TL_RSEG FROM WL_TABIX.
*            IF TL_RSEG-BELNR NE E_EKBE-BELNR
*            OR TL_RSEG-GJAHR NE E_EKBE-GJAHR.
*              EXIT.
*            ENDIF.
*
*            MOVE: T_EKPO-EBELN TO T_EKKO_TOTAL-EBELN,
*                  E_EKBE-BELNR TO T_EKKO_TOTAL-BELNR,
*                  E_EKBE-GJAHR TO T_EKKO_TOTAL-GJAHR.
**              E_EKBE-DMBTR TO T_EKKO_TOTAL-VLTOT,<<<<<<
*            IF TL_RSEG-WRBTR IS NOT INITIAL.
*              MOVE: TL_RSEG-WRBTR TO T_EKKO_TOTAL-VLTOT.
*            ELSE.
*              MOVE: TL_RSEG-BNKAN TO T_EKKO_TOTAL-VLTOT.
*            ENDIF.

      move: E_EKBE-MENGE to T_EKKO_TOTAL_QTD-QTDTOT.
*            t_ekpo-netwr to t_ekko_total-vltot.
*            COLLECT T_EKKO_TOTAL. CLEAR T_EKKO_TOTAL.

      move:T_EKPO-EBELN to T_EKKO_TOTAL_QTD-EBELN,
           T_EKPO-EBELP to T_EKKO_TOTAL_QTD-EBELP,
           E_EKBE-BELNR to T_EKKO_TOTAL_QTD-BELNR,
           E_EKBE-GJAHR to T_EKKO_TOTAL_QTD-GJAHR,
           E_EKBE-MENGE to T_EKKO_TOTAL_QTD-QTDTOT,
           E_EKBE-WRBTR to T_EKKO_TOTAL_QTD-DMBTR.
*                 E_EKBE-DMBTR TO T_EKKO_TOTAL_QTD-DMBTR.<<<<<<

      collect T_EKKO_TOTAL_QTD. clear T_EKKO_TOTAL_QTD.
*          ENDLOOP.
*        ENDIF.
    endloop.

  endloop.
*  ENDIF.
** Vgabe 3

  loop at T_EKPO.

*    READ TABLE e_ekbe WITH KEY ebeln = t_ekpo-ebeln
*                               ebelp = t_ekpo-ebelp
*                               vgabe = 3
*                               BINARY SEARCH.
    loop at E_EKBE where EBELN = T_EKPO-EBELN
                     and EBELP = T_EKPO-EBELP
                     and  VGABE = 3.
*                      OR  vgabe = 4.
*      IF sy-subrc = 0.
      read table TL_RSEG
        with key BELNR = E_EKBE-BELNR
                 GJAHR = E_EKBE-GJAHR
                 binary search.
      if SY-SUBRC is initial.
        WL_TABIX = SY-TABIX.
        loop at TL_RSEG from WL_TABIX.
          if TL_RSEG-BELNR ne E_EKBE-BELNR
          or TL_RSEG-GJAHR ne E_EKBE-GJAHR.
            exit.
          endif.

          move: T_EKPO-EBELN to T_EKKO_TOTAL3-EBELN,
                E_EKBE-BELNR to T_EKKO_TOTAL3-BELNR.
*                E_EKBE-DMBTR TO T_EKKO_TOTAL3-VLTOT.
*                TL_RSEG-BNKAN TO T_EKKO_TOTAL3-VLTOT.
          if TL_RSEG-WRBTR is not initial.
            move: TL_RSEG-WRBTR to T_EKKO_TOTAL3-VLTOT.
          else.
            move: TL_RSEG-BNKAN to T_EKKO_TOTAL3-VLTOT.
          endif.

          collect T_EKKO_TOTAL3. clear T_EKKO_TOTAL3.
*      ENDIF.
        endloop.
      endif.
    endloop.
  endloop.

  loop at E_EKBE.
    if E_EKBE-ANLN1 is not initial.
      read table T_EKKO with key EBELN = E_EKBE-EBELN
                                   EBELP = E_EKBE-EBELP
                                   ANLN1 = E_EKBE-ANLN1.
    elseif E_EKBE-AUFNR is not initial.
      read table T_EKKO with key EBELN = E_EKBE-EBELN
                                EBELP = E_EKBE-EBELP
                                AUFNR = E_EKBE-AUFNR.
    endif.
    if SY-SUBRC <> 0.
      read table T_EKKO with key EBELN = E_EKBE-EBELN
                               EBELP = E_EKBE-EBELP.
      if SY-SUBRC = 0.
        move-corresponding E_EKBE to T_EKKO.
        append T_EKKO. clear T_EKKO.
      endif.
    endif.
  endloop.


  data: W_VALOR     type EKPO-NETWR,
        W_VALOR_AUX type EKPO-NETWR,
        W_TAB       type SY-TABIX.

  if P_SINCAX is not initial
  or P_ANALIT is not initial
  or P_CONSOL is not initial
  and T_EKBE[] is not initial.
    select BELNR GJAHR STBLG
         from RBKP
         into table T_RBKP_EST
          for all entries in T_EKBE
          where BELNR eq T_EKBE-BELNR
            and GJAHR eq T_EKBE-GJAHR
            and STBLG ne SPACE.

    loop at T_EKBE.
      WL_TABIX = SY-TABIX.
      read table T_RBKP_EST
        with key BELNR = T_EKBE-BELNR
                 GJAHR = T_EKBE-GJAHR.

      if SY-SUBRC is initial.
        delete T_EKBE index WL_TABIX.
      endif.
    endloop.
  endif.

  sort T_EKPO by EBELN EBELP.
  if P_SINCAX is not initial
  or P_ANALIT is not initial
  or P_CONSOL is not initial.
*    SORT t_ekbe BY ebeln ebelp vgabe shkzg.
  else.
    sort T_EKBE by EBELN EBELP VGABE.
  endif.

  loop at T_EKKO.
    W_TAB = SY-TABIX.
    read table T_EKPO with key EBELN = T_EKKO-EBELN
                               EBELP = T_EKKO-EBELP
                               binary search.
    if SY-SUBRC <> 0.
      continue.
    endif.

    if P_SINCAX is not initial
    or P_ANALIT is not initial
    or P_CONSOL is not initial.
      read table T_EKBE with key EBELN = T_EKKO-EBELN
                                EBELP = T_EKKO-EBELP
                                VGABE = 2
                                SHKZG = 'S'
                                binary search.
*****      IF t_ekko-anln1 IS NOT INITIAL.
*****        READ TABLE t_ekbe WITH KEY ebeln = t_ekko-ebeln
*****                                       ebelp = t_ekko-ebelp
*****                                       anln1 = t_ekko-anln1
*****                                       vgabe = 2
*****                                       shkzg = 'S'.
******                                       BINARY SEARCH.
*****
*****        LOOP AT t_ekbe WHERE ebeln = t_ekko-ebeln
*****                         AND ebelp = t_ekko-ebelp
*****                         AND anln1 = t_ekko-anln1
*****                         AND vgabe = 2
*****                         AND shkzg = 'S'.
*****
*****          READ TABLE t_ekko_total WITH KEY "ebeln = t_ekko-ebeln
*****                                           belnr = t_ekbe-belnr.
*****
*****          IF sy-subrc = 0.
*****            IF t_ekko-vproz > 0.
******      and p_sincax is initial.
*****              w_valor = t_ekko-netwr * ( t_ekko-vproz / 100 ).
*****            ELSE.
*****              IF p_sincax IS NOT INITIAL.
*****                w_valor = t_ekbe-dmbtr.
*****              ELSE.
*****                w_valor = t_ekko-netwr.
*****              ENDIF.
*****            ENDIF.
*****            t_ekko-porcento = w_valor / t_ekko_total-vltot.
*****
*****            MODIFY t_ekko INDEX w_tab.
*****          ENDIF.
*****        ENDLOOP.
*****        CONTINUE.
*****      ELSEIF t_ekko-aufnr IS NOT INITIAL.
*****        LOOP AT t_ekbe WHERE ebeln = t_ekko-ebeln
*****                                 AND ebelp = t_ekko-ebelp
*****                                 AND aufnr = t_ekko-aufnr
*****                                 AND vgabe = 2
*****                                 AND shkzg = 'S'.
*****
*****          READ TABLE t_ekko_total WITH KEY "ebeln = t_ekko-ebeln
*****                                           belnr = t_ekbe-belnr.
*****
*****          IF sy-subrc = 0.
*****            IF t_ekko-vproz > 0.
******      and p_sincax is initial.
*****              w_valor = t_ekko-netwr * ( t_ekko-vproz / 100 ).
*****            ELSE.
*****              IF p_sincax IS NOT INITIAL.
*****                w_valor = t_ekbe-dmbtr.
*****              ELSE.
*****                w_valor = t_ekko-netwr.
*****              ENDIF.
*****            ENDIF.
*****            t_ekko-porcento = w_valor / t_ekko_total-vltot.
*****
*****            MODIFY t_ekko INDEX w_tab.
*****          ENDIF.
*****        ENDLOOP.
*****        CONTINUE.
*****
*****      ENDIF.
    else.
      read table T_EKBE with key EBELN = T_EKKO-EBELN
                                  EBELP = T_EKKO-EBELP
                                  VGABE = 2
                                  binary search.

    endif.
    if SY-SUBRC <> 0.
      continue.
    else.
      read table T_EKKO_TOTAL with key EBELN = T_EKKO-EBELN
                                       BELNR = T_EKBE-BELNR.
    endif.
*    ELSE.
*      READ TABLE t_ekko_total WITH KEY ebeln = t_ekko-ebeln.
**                                       belnr = t_ekbec-belnr.
*    ENDIF.


    if SY-SUBRC = 0.
      if T_EKKO-VPROZ > 0.
*      and p_sincax is initial.
        W_VALOR = T_EKKO-NETWR * ( T_EKKO-VPROZ / 100 ).
      else.
        if P_SINCAX is not initial.
          W_VALOR = T_EKBE-DMBTR.
        else.
          W_VALOR = T_EKKO-NETWR.
        endif.
      endif.
      try.

          T_EKKO-PORCENTO = W_VALOR / T_EKKO_TOTAL-VLTOT.
        catch CX_SY_ZERODIVIDE .
      endtry.
      modify T_EKKO index W_TAB.
    endif.
  endloop.

*  ELSE.
  if P_SINCAX is not initial
  or P_ANALIT is not initial
  or P_CONSOL is not initial.
    refresh: TL_RSEG.
    if T_EKKO_TOTAL[] is not initial.
      select BELNR GJAHR BUZEI EBELN EBELP WRBTR BNKAN
        from RSEG
        into table TL_RSEG
         for all entries in T_EKKO_TOTAL
         where BELNR eq T_EKKO_TOTAL-BELNR
           and GJAHR eq T_EKKO_TOTAL-GJAHR.
*           and ebeln ne t_ekko_total-ebeln.

      if SY-SUBRC is initial.
        loop at TL_RSEG.
          read table T_EKPO transporting no fields
            with key EBELN = TL_RSEG-EBELN
                     EBELP = TL_RSEG-EBELP.
          if SY-SUBRC is not initial.
            read table T_EKKO_TOTAL
              with key BELNR = TL_RSEG-BELNR
                       GJAHR = TL_RSEG-GJAHR.
*                       ebeln = tl_rseg-ebeln.
            if SY-SUBRC is initial.
              add TL_RSEG-WRBTR to T_EKKO_TOTAL-VLTOT.
***              MODIFY T_EKKO_TOTAL INDEX SY-TABIX. <<<<<
            endif.
          endif.
        endloop.
      endif.
    endif.
    loop at T_EKKO_TOTAL.
*      LOOP AT t_ekko.
*        MOVE-CORRESPONDING: t_ekko TO t_ekko3.
*        MOVE: t_ekko_total3-belnr TO t_ekko3-belnr.

*      READ TABLE t_ekpo WITH KEY ebeln = t_ekko3-ebeln
*                                 ebelp = t_ekko3-ebelp
*                                 BINARY SEARCH.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.


*        READ TABLE t_ekbe WITH KEY ebeln = t_ekko3-ebeln
*                                  ebelp = t_ekko3-ebelp
*                                  vgabe = 3
*                                  shkzg = 'S'
*                                  belnr = t_ekko_total3-belnr.
      loop at T_EKBE where VGABE = 2
                       and SHKZG = 'S'
                       and EBELN = T_EKKO_TOTAL-EBELN
                       and BELNR = T_EKKO_TOTAL-BELNR.

        if T_EKBE-AUFNR is not initial.
          read table TL_EKKN
           with key EBELN = T_EKBE-EBELN
                    EBELP = T_EKBE-EBELP
                    AUFNR = T_EKBE-AUFNR.
          if SY-SUBRC is initial.
            read table T_EKKO with key EBELN = T_EKBE-EBELN
                                       EBELP = T_EKBE-EBELP
                                       AUFNR = T_EKBE-AUFNR
                                       ZEKKN = TL_EKKN-ZEKKN.
          else.
            continue.
          endif.

        elseif T_EKBE-ANLN1 is not initial.
          read table TL_EKKN
           with key EBELN = T_EKBE-EBELN
                    EBELP = T_EKBE-EBELP
                    ANLN1 = T_EKBE-ANLN1.
          if SY-SUBRC is initial.
            read table T_EKKO with key EBELN = T_EKBE-EBELN
                                        EBELP = T_EKBE-EBELP
                                        ANLN1 = T_EKBE-ANLN1
                                        ZEKKN = TL_EKKN-ZEKKN.
          else.
            continue.
          endif.
        endif.
        move-corresponding: T_EKKO to T_EKKO3.
        move: T_EKKO_TOTAL-BELNR to T_EKKO3-BELNR,
              TL_EKKN-ZEKKN      to T_EKKO3-ZEKKN.

        if SY-SUBRC <> 0.
          continue.
        else.
          read table T_EKKO_TOTAL with key BELNR = T_EKBE-BELNR.
          read table T_EKKO_TOTAL_QTD with key BELNR = T_EKBE-BELNR
                                               GJAHR = T_EKBE-GJAHR
                                               EBELN = T_EKBE-EBELN
                                               EBELP = T_EKBE-EBELP.

*          read table t_ekko_total with key ebeln = t_ekko3-ebeln
*                                           belnr = t_ekbe-belnr.
        endif.
*    ELSE.
*      READ TABLE t_ekko_total WITH KEY ebeln = t_ekko-ebeln.
**                                       belnr = t_ekbec-belnr.
*    ENDIF.


*        IF SY-SUBRC = 0.
        if T_EKKO3-VPROZ > 0.
*      and p_sincax is initial.
          clear: WL_VPROZ, W_VALOR_AUX.
*            w_valor = t_ekbe-dmbtr * ( t_ekko3-vproz / 100 ).
          read table T_EKPO
                     with key EBELN = T_EKBE-EBELN
                              EBELP = T_EKBE-EBELP.
          if T_EKPO-PSTYP eq '9'.
*              WL_VPROZ = ( ( T_EKBE-DMBTR * 100 ) / T_EKKO_TOTAL_QTD-DMBTR ). " t_ekko3-menge ).<<<<<<<<
            WL_VPROZ = ( ( T_EKBE-WRBTR * 100 ) / T_EKKO_TOTAL_QTD-DMBTR ). " t_ekko3-menge ).
          else.
            WL_VPROZ = ( ( T_EKBE-MENGE * 100 ) / T_EKKO_TOTAL_QTD-QTDTOT ). " t_ekko3-menge ).
          endif.
*            WL_VPROZ = ( ( T_EKBE-MENGE * 100 ) / T_EKKO_TOTAL_QTD-QTDTOT ). " t_ekko3-menge ).
**            w_valor = t_ekko3-netwr * ( wl_vproz / 100 ).
*            LOOP AT E_EKBE
*                WHERE BELNR EQ T_EKBE-BELNR
*                  AND GJAHR EQ T_EKBE-GJAHR
*                  AND EBELN EQ T_EKBE-EBELN
*                  AND EBELP EQ T_EKBE-EBELP
*                  AND VGABE EQ T_EKBE-VGABE.
*
**              ADD E_EKBE-DMBTR TO W_VALOR_AUX.<<<<<<
*              ADD E_EKBE-WRBTR TO W_VALOR_AUX.
*            ENDLOOP.
          loop at TL_RSEG
              where BELNR eq T_EKBE-BELNR
                and GJAHR eq T_EKBE-GJAHR
                and EBELN eq T_EKBE-EBELN
                and EBELP eq T_EKBE-EBELP.

            if TL_RSEG-WRBTR is not initial.
              add TL_RSEG-WRBTR to W_VALOR_AUX.
            else.
              add TL_RSEG-BNKAN to W_VALOR_AUX.
            endif.
          endloop.
          W_VALOR = W_VALOR_AUX * ( WL_VPROZ / 100 ).
*            w_valor = t_ekko3-netwr * ( t_ekko3-vproz / 100 ).
        else.
*            W_VALOR = T_EKBE-DMBTR. <<<<<
          W_VALOR = T_EKBE-WRBTR.
*            w_valor = t_ekko3-netwr.
        endif.
        try.
            T_EKKO3-PORCENTO = W_VALOR / T_EKKO_TOTAL-VLTOT.
          catch CX_SY_ZERODIVIDE .
        endtry.
        if T_EKKO3-AUFNR is not initial.
          read table T_EKKO3 into WL_EKKO3
            with key EBELN = T_EKKO3-EBELN
                     EBELP = T_EKKO3-EBELP
                     BELNR = T_EKKO3-BELNR
                     AUFNR = T_EKKO3-AUFNR
                     ZEKKN = T_EKKO3-ZEKKN.

        elseif T_EKKO3-ANLN1 is not initial.
          read table T_EKKO3 into WL_EKKO3
          with key EBELN = T_EKKO3-EBELN
                   EBELP = T_EKKO3-EBELP
                   BELNR = T_EKKO3-BELNR
                   ANLN1 = T_EKKO3-ANLN1
                   ZEKKN = T_EKKO3-ZEKKN.
        endif.

        if SY-SUBRC is initial.
          WL_TABIX = SY-TABIX.
          add T_EKKO3-PORCENTO to WL_EKKO3-PORCENTO. " erro
          modify T_EKKO3 from WL_EKKO3 index WL_TABIX.
        else.
          append T_EKKO3.
        endif.
*        ENDIF.
      endloop.
    endloop.
*    t_ekko3[] = t_ekko[].

    loop at T_EKKO_TOTAL3.
*      LOOP AT t_ekko.
*        MOVE-CORRESPONDING: t_ekko TO t_ekko3.
*        MOVE: t_ekko_total3-belnr TO t_ekko3-belnr.

*      READ TABLE t_ekpo WITH KEY ebeln = t_ekko3-ebeln
*                                 ebelp = t_ekko3-ebelp
*                                 BINARY SEARCH.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.


*        READ TABLE t_ekbe WITH KEY ebeln = t_ekko3-ebeln
*                                  ebelp = t_ekko3-ebelp
*                                  vgabe = 3
*                                  shkzg = 'S'
*                                  belnr = t_ekko_total3-belnr.
      loop at T_EKBE where VGABE = 3
*                        OR vgabe = 4
                       and SHKZG = 'S'
                       and BELNR = T_EKKO_TOTAL3-BELNR
                       and EBELN = T_EKKO_TOTAL3-EBELN.

        if T_EKBE-AUFNR is not initial.
          read table T_EKKO with key EBELN = T_EKBE-EBELN
                                     EBELP = T_EKBE-EBELP
                                     AUFNR = T_EKBE-AUFNR.
        elseif T_EKBE-ANLN1 is not initial.
          read table T_EKKO with key EBELN = T_EKBE-EBELN
                                    EBELP = T_EKBE-EBELP
                                    ANLN1 = T_EKBE-ANLN1.
        endif.
        move-corresponding: T_EKKO to T_EKKO3.
        move: T_EKKO_TOTAL3-BELNR to T_EKKO3-BELNR.

        if SY-SUBRC <> 0.
          continue.
        else.
          read table T_EKKO_TOTAL3 with key EBELN = T_EKKO3-EBELN
                                           BELNR = T_EKBE-BELNR.
        endif.
*    ELSE.
*      READ TABLE t_ekko_total WITH KEY ebeln = t_ekko-ebeln.
**                                       belnr = t_ekbec-belnr.
*    ENDIF.


        if SY-SUBRC = 0.
          if T_EKKO3-VPROZ > 0.
*      and p_sincax is initial.
            W_VALOR = T_EKBE-DMBTR * ( T_EKKO3-VPROZ / 100 ).
          else.
            W_VALOR = T_EKBE-DMBTR.
          endif.
          try.
              T_EKKO3-PORCENTO = W_VALOR / T_EKKO_TOTAL3-VLTOT.
            catch CX_SY_ZERODIVIDE .
          endtry.
          if T_EKKO3-AUFNR is not initial.
            read table T_EKKO3 into WL_EKKO3
             with key EBELN = T_EKKO3-EBELN
                      EBELP = T_EKKO3-EBELP
                      BELNR = T_EKKO3-BELNR
                      AUFNR = T_EKKO3-AUFNR.
          elseif T_EKKO3-ANLN1 is not initial.
            read table T_EKKO3 into WL_EKKO3
           with key EBELN = T_EKKO3-EBELN
                    EBELP = T_EKKO3-EBELP
                    BELNR = T_EKKO3-BELNR
                    ANLN1 = T_EKKO3-ANLN1.
          endif.

          if SY-SUBRC is initial.
            WL_TABIX = SY-TABIX.
            add T_EKKO3-PORCENTO to WL_EKKO3-PORCENTO.
            modify T_EKKO3 from WL_EKKO3 index WL_TABIX.
          else.
            append T_EKKO3.
          endif.
        endif.
        clear: WL_TABIX, WL_EKKO3.
      endloop.
    endloop.
  endif.

  T_EKKO_POR[] = T_EKKO[].

endform.                    " F_AJUSTA_VALORES
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_COMPETENCIA
*&---------------------------------------------------------------------*
form F_SELECIONA_COMPETENCIA .

  select IMAKPA~POSNR IMAKPA~LFDNR IMAKPA~AKOSTL IMAKPA~AGSBER
         J_1BBRANCH~NAME CSKT~LTEXT
    from IMAKPA inner join J_1BBRANCH on
    ( IMAKPA~AGSBER eq J_1BBRANCH~BRANCH )
                inner join CSKT on
    ( IMAKPA~AKOSTL eq CSKT~KOSTL )
    into table T_IMAKPA
    for all entries in T_IMAK
    where IMAKPA~POSNR = T_IMAK-POSNR and
          J_1BBRANCH~BUKRS in S_BUKRS and
          CSKT~SPRAS       eq SY-LANGU and
          CSKT~DATBI       ge SY-DATUM.

  select * from ZIM01_SOL_AP_INV into table T_INV
    for all entries in T_IMAKPA
    where BUKRS in S_BUKRS and
          KOSTL eq T_IMAKPA-AKOSTL and
          POSNR eq T_IMAKPA-POSNR  and
          ANO   in S_BUDAT.

endform.                    " F_SELECIONA_COMPETENCIA
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_ANALITICO
*&---------------------------------------------------------------------*
form F_MONTA_DADOS_ANALITICO using P_TIPO.

  data: W_VALOR type ZIM01_SOL_AP_INV-VLR_TOTAL.

  loop at T_IMAKPA where POSNR = T_IMAK-POSNR.
    loop at T_INV where BUKRS = T_IMAK-ABUKRS    and
*                        ano   = t_imak-gjahr    AND
                        KOSTL = T_IMAKPA-AKOSTL and
                        POSNR = T_IMAK-POSNR    and
                        FASE = 1.

      case P_MOEDA.
        when 'BRL'.
          W_VALOR = T_INV-VLR_TOTAL.
        when 'USD'.
          W_VALOR = T_INV-VL_USD.
        when 'EUR'.
          W_VALOR = T_INV-VL_EUR.
        when others.
          W_VALOR = T_INV-VLR_TOTAL.
      endcase.
      if T_INV-SAKNR ne '0000132050' and
         T_INV-SAKNR ne '0000132006'.
        if P_TIPO = ' '.
          continue.
        endif.
        TW_SAIDA-TXTTIPO = 'IMOBILIZADOS (AA-Imobilizado)'.
        concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2 into TW_SAIDA-OBJETO separated by '-'.
        TW_SAIDA-TXT050 = T_IMAKA-TXT50.
      else.
        if P_TIPO = 'X'.
          continue.
        endif.
        TW_SAIDA-TXTTIPO = 'IMOBILIZADO EM ANDAMENTO (CO-Ordem Interna)'.
        TW_SAIDA-OBJETO = T_IMAKZ-ANLN1.
        TW_SAIDA-TXT050 = T_IMAK-TXT50.

      endif.
      if T_INV-FASE = 1.
        TW_SAIDA-ANLN1 = 'PLANEJADO'.
        TW_SAIDA-TXT50 = 'Orçamento'.

        clear TW_SAIDA-DMBTR.

*        tw_saida-txz01 = t_inv-objetivo.
        concatenate T_INV-BUZEI TW_SAIDA-ANLN1 T_INV-DESCR_ITEM
        into TW_SAIDA-TXZ01 separated by ' - '.
        TW_SAIDA-ORCADO = T_INV-VLR_TOTAL.
        TW_SAIDA-APROVADO = TW_SAIDA-PED_AP = TW_SAIDA-REAL_AP = W_VALOR."t_inv-vlr_total.
*        ADD tw_saida-ped_ap TO tw_saida-ped_ap.
        TW_SAIDA-EBELP = T_INV-BUZEI.
      else.
        TW_SAIDA-ANLN1 = 'EXTRA'.
        TW_SAIDA-TXT50 = 'Orçamento'.

        clear TW_SAIDA-DMBTR.

*        tw_saida-txz01 = t_inv-objetivo.
        concatenate T_INV-BUZEI TW_SAIDA-ANLN1 T_INV-DESCR_ITEM
        into TW_SAIDA-TXZ01 separated by ' - '.
        TW_SAIDA-EBELP = T_INV-BUZEI.
        TW_SAIDA-APROVADO = TW_SAIDA-PED_AP = TW_SAIDA-REAL_AP = W_VALOR."t_inv-vlr_total.
*        t_inv-vlr_total
      endif.

      perform F_DEFINE_COR using 'APROVADO' '6' '1'.
      TW_SAIDA-COLOR[] = T_INFOGRID-COLOR[].
      append TW_SAIDA.
      clear: TW_SAIDA-APROVADO, TW_SAIDA-ORCADO,
             TW_SAIDA-PED_AP,   TW_SAIDA-REAL_AP.

    endloop.


    loop at T_INV where BUKRS = T_IMAK-ABUKRS    and
*                        ano   = t_imak-gjahr    AND
                        KOSTL = T_IMAKPA-AKOSTL and
                        POSNR = T_IMAK-POSNR    and
                        FASE ne 1.

      case P_MOEDA.
        when 'BRL'.
          W_VALOR = T_INV-VLR_TOTAL.
        when 'USD'.
          W_VALOR = T_INV-VL_USD.
        when 'EUR'.
          W_VALOR = T_INV-VL_EUR.
        when others.
          W_VALOR = T_INV-VLR_TOTAL.
      endcase.

      if T_INV-SAKNR ne '0000132050' and
         T_INV-SAKNR ne '0000132006'.
        if P_TIPO = ' '.
          continue.
        endif.
        TW_SAIDA-TXTTIPO = 'IMOBILIZADOS (AA-Imobilizado)'.
        concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2 into TW_SAIDA-OBJETO separated by '-'.
        TW_SAIDA-TXT050 = T_IMAKA-TXT50.
      else.
        if P_TIPO = 'X'.
          continue.
        endif.
        TW_SAIDA-TXTTIPO = 'IMOBILIZADO EM ANDAMENTO (CO-Ordem Interna)'.
        TW_SAIDA-OBJETO = T_IMAKZ-ANLN1.
        TW_SAIDA-TXT050 = T_IMAK-TXT50.
      endif.
      if T_INV-FASE = 1.
        TW_SAIDA-ANLN1 = 'PLANEJADO'.
        TW_SAIDA-TXT50 = 'Orçamento'.
*        tw_saida-txz01 = t_inv-objetivo.
        concatenate T_INV-BUZEI TW_SAIDA-ANLN1 T_INV-DESCR_ITEM
        into TW_SAIDA-TXZ01 separated by ' - '.
        TW_SAIDA-ORCADO = T_INV-VLR_TOTAL.
        TW_SAIDA-APROVADO = TW_SAIDA-PED_AP = TW_SAIDA-REAL_AP = W_VALOR."t_inv-vlr_total.
        TW_SAIDA-EBELP = T_INV-BUZEI.
      else.
        case T_INV-FASE.
          when 1.
            TW_SAIDA-ANLN1 = 'PLANEJADO'.
          when 2.
            TW_SAIDA-ANLN1 = 'EXTRA'.
          when 3.
            TW_SAIDA-ANLN1 = 'SINISTRO'.
        endcase.
        concatenate T_INV-BUZEI TW_SAIDA-ANLN1 T_INV-DESCR_ITEM
        into TW_SAIDA-TXZ01 separated by ' - '.


        TW_SAIDA-ANLN1 = 'EXTRA'.
        TW_SAIDA-TXT50 = 'Orçamento'.
        clear TW_SAIDA-DMBTR.


*        tw_saida-txz01 = t_inv-objetivo.
        TW_SAIDA-EBELP = T_INV-BUZEI.
        TW_SAIDA-APROVADO = TW_SAIDA-PED_AP = TW_SAIDA-REAL_AP = W_VALOR."t_inv-vlr_total.
      endif.

*      PERFORM f_define_cor USING 'ORCADO' '6' '1'.
*      tw_saida-color[] = t_infogrid-color[].
      append TW_SAIDA.
      clear: TW_SAIDA-APROVADO, TW_SAIDA-ORCADO,
             TW_SAIDA-PED_AP,   TW_SAIDA-REAL_AP.

    endloop.


  endloop.

endform.                    " F_MONTA_DADOS_ANALITICO
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_LAYOUT_ANALIT
*&---------------------------------------------------------------------*
form MODIFICA_LAYOUT_ANALIT .

** change fieldcatalog
  data: LS_FIELDCATALOG type LVC_S_FCAT.
*        w_pos TYPE i.
*  CLEAR w_pos.
  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.

    case LS_FIELDCATALOG-FIELDNAME.

      when 'ORCADO' .
        LS_FIELDCATALOG-COL_POS = 1.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-COLTEXT = 'Planejado'.
      when 'APROVADO'.
        LS_FIELDCATALOG-COL_POS = 2.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-COLTEXT = 'Previsto'.
      when 'NETWR' .
        LS_FIELDCATALOG-COL_POS = 3.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido'.
        LS_FIELDCATALOG-COLTEXT = 'Pedido'.
      when 'DMBTR'.
        LS_FIELDCATALOG-COL_POS = 4.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-COLTEXT = 'Realizado'.
      when 'TOTPG'.
        LS_FIELDCATALOG-COL_POS = 5.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-COLTEXT = 'Total Pago'.
      when 'TOTAPG'.
        LS_FIELDCATALOG-COL_POS = 6.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-COLTEXT = 'Total a Pagar'.

      when 'PED_AP' .
        LS_FIELDCATALOG-COL_POS = 7.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-COLTEXT = 'Pedido (-) Previsto'.
      when 'REAL_AP'.
        LS_FIELDCATALOG-COL_POS = 8.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-COLTEXT = 'Realizado (-) Previsto'.

      when others.
        LS_FIELDCATALOG-NO_OUT = 'X'.
        LS_FIELDCATALOG-KEY    = ''.
    endcase.

    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.

endform.                    " MODIFICA_LAYOUT_ANALIT
*&---------------------------------------------------------------------*
*&      Form  monta_fieldcat
*&---------------------------------------------------------------------*
form MONTA_FIELDCAT using
               X_FIELD X_TAB X_REF X_TEXT X_SUM X_JUST X_KEY X_HOTSPOT X_OUTPUTLEN .

  I = I + 1.
  clear FC.
  FC-COL_POS       = I.
  FC-FIELDNAME     = X_FIELD.
  FC-TABNAME       = X_TAB.
  FC-REF_TABLE     = X_REF.
  FC-DO_SUM        = X_SUM.
  FC-JUST          = X_JUST.
  FC-KEY           = X_KEY.
  FC-HOTSPOT       = X_HOTSPOT.
  FC-SELTEXT       =
  FC-COLTEXT       = X_TEXT.
  FC-OUTPUTLEN     = X_OUTPUTLEN.
  append FC to IT_FIELDCATALOG.

endform.                               " MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  f_define_cor
*&---------------------------------------------------------------------*
form F_DEFINE_COR using FIELDNAME COLOR_COL COLOR_INT.

  clear E_COLOR.
  E_COLOR-FIELDNAME = FIELDNAME.
  E_COLOR-COLOR-COL = COLOR_COL.
  E_COLOR-COLOR-INV = COLOR_INT.
  append E_COLOR to T_INFOGRID-COLOR.

endform.                    " f_define_cor
*&---------------------------------------------------------------------*
*&      Form  f_monta_catalogo
*&---------------------------------------------------------------------*
form F_MONTA_CATALOGO.
* Monta a definição de cada coluna do grid
  clear I.
  refresh IT_FIELDCATALOG.

  if not P_SINCAX is initial.

    perform MONTA_FIELDCAT using:
         'POSNR'          'T_ALV1' 'ZES_IM01' 'Sol. Inv.         ' ' ' ' ' ' ' ' ' ' ',
         'DESCR'          'T_ALV1' 'IMAKT'    'Descrição         ' ' ' ' ' ' ' ' ' '30',

         'ABUKRS'         'T_ALV1' 'IMAK'     'Empresa           ' ' ' ' ' ' ' ' ' '08',
         'BUTXT'          'T_ALV1' 'T001'     'Nome da Empresa   ' ' ' ' ' ' ' ' ' '25',
         'AGSBER'         'T_ALV1' 'IMAKPA'   'Filial            ' ' ' ' ' ' ' ' ' '08',
         'NAME'           'T_ALV1' '1BBRANCH' 'Nome da Filial    ' ' ' ' ' ' ' ' ' '25',
         'AKOSTL'         'T_ALV1' 'IMAKPA'   'Centro de Custo   ' ' ' ' ' ' ' ' ' '10',
         'LTEXT'          'T_ALV1' 'CSKT'     'Descrição Centro de Custo' ' ' ' ' ' ' ' ' '25',


         'TXTTIPO'        'T_ALV1' 'ZES_IM01' 'Classificação     ' ' ' ' ' ' ' ' ' ' ',
         'ANLN1'          'T_ALV1' ' '        'Nro. Objeto Custo ' ' ' ' ' ' ' ' ' '65',
*         'ANLN2'          'T_ALV1' ' '        'Nro. Objeto Custo ' ' ' ' ' ' ' ' ' '65',
         'TXTIMOB'        'T_ALV1' ' '        'Denominação Imobilizado ' ' ' ' ' ' ' ' ' '',
         'NETWR'          'T_ALV1' 'ZES_IM01' 'Vlr. Pedido       ' ' ' ' ' ' ' ' ' ' ',
         'DMBTR'          'T_ALV1' 'ZES_IM01' 'Vlr. Realizado    ' ' ' ' ' ' ' ' ' ' ',
         'TOTPG'          'T_ALV1' 'ZES_IM01' 'Total Pago        ' ' ' ' ' ' ' ' ' ' ',
         'TOTAPG'         'T_ALV1' 'ZES_IM01' 'Total a Pagar     ' ' ' ' ' ' ' ' ' ' ',
         'MODULO'         'T_ALV1' 'ZES_IM01' 'Módulo            ' ' ' ' ' ' ' ' ' '20',
         'EBELN'          'T_ALV1' 'ZES_IM01' 'Pedido Compra     ' ' ' ' ' ' ' ' ' ' ',
         'EBELP'          'T_ALV1' 'ZES_IM01' 'Item              ' ' ' ' ' ' ' ' ' ' ',
         'AEDAT'          'T_ALV1' 'ZES_IM01' 'Data PG           ' ' ' ' ' ' ' ' ' ' ',
         'BELNR2'         'T_ALV1' 'ZES_IM01' 'Tipo Doc.         ' ' ' ' ' ' ' ' ' ' ',
         'BELNR3'         'T_ALV1' 'ZES_IM01' 'Doc. Contábil     ' ' ' ' ' ' ' ' ' ' ',
         'BELNR6'         'T_ALV1' 'ZES_IM01' 'Doc. Comp.        ' ' ' ' ' ' ' ' ' ' ',
         'BELNR5'         'T_ALV1' 'ZES_IM01' 'Doc. Miro         ' ' ' ' ' ' ' ' ' ' ',
         'NETPR'          'T_ALV1' 'ZES_IM01' 'Vlr. Doc.         ' ' ' ' ' ' ' ' ' ' ',
         'FORNECEDOR'     'T_ALV1' 'ZES_IM01' 'Fornecedor        ' ' ' ' ' ' ' ' ' ' ',
         'PAGO30'         'T_ALV1' 'ZES_IM01' '0-30 dias         ' ' ' ' ' ' ' ' ' ' ',
         'PAGO60'         'T_ALV1' 'ZES_IM01' '31-60 dias        ' ' ' ' ' ' ' ' ' ' ',
         'PAGO90'         'T_ALV1' 'ZES_IM01' '61-90 dias        ' ' ' ' ' ' ' ' ' ' ',
         'PAGO120'        'T_ALV1' 'ZES_IM01' '91-120 dias       ' ' ' ' ' ' ' ' ' ' ',
         'PAGO150'        'T_ALV1' 'ZES_IM01' '121-150 dias      ' ' ' ' ' ' ' ' ' ' ',
         'PAGO180'        'T_ALV1' 'ZES_IM01' '151-180 dias      ' ' ' ' ' ' ' ' ' ' ',
         'PAGO210'        'T_ALV1' 'ZES_IM01' '181-210 dias      ' ' ' ' ' ' ' ' ' ' ',
         'PAGO999'        'T_ALV1' 'ZES_IM01' '211-999 dias      ' ' ' ' ' ' ' ' ' ' ',
         'IMP_TOTAL'      'T_ALV1' 'ZES_IM01' 'Total de Impostos ' ' ' ' ' ' ' ' ' '20'.

    loop at TG_IMPOSTOS.
      read table IT_FIELDCATALOG transporting no fields
        with key FIELDNAME = TG_IMPOSTOS-FIELD_IMP.
      if SY-SUBRC is not initial.
        perform MONTA_FIELDCAT using:
      TG_IMPOSTOS-FIELD_IMP        'T_ALV1' 'ZES_IM01' TG_IMPOSTOS-BEZEI ' ' ' ' ' ' ' ' '25'.
      endif.
    endloop.

    loop at TW_SAIDA.
      if TW_SAIDA-AEDAT not in S_BUDAT
      and TW_SAIDA-BELNR2(2) ne 'FI'.
        delete TW_SAIDA.
      endif.
    endloop.

    loop at TW_SAIDA.

      if TW_SAIDA-BELNR2(4) = 'MIRO' and ( TW_SAIDA-SGTXT(3) eq 'ZPI' or TW_SAIDA-SGTXT(3) eq 'ZEF' ).
        clear: TW_SAIDA-DMBTR, TW_SAIDA-DIFERENCA.
      endif.
      perform AJUSTES_ALV.
      perform Z_AJUSTA_SAIDA.

      clear T_ALV1.
      move TW_SAIDA-POSNR      to T_ALV1-POSNR.

      move-corresponding TW_SAIDA to T_ALV1.

      sort T_IMAK by POSNR TXT50.
      read table T_IMAK with key POSNR = T_ALV1-POSNR
                             binary search.
      if SY-SUBRC eq 0.
        move: T_IMAK-TXT50        to T_ALV1-DESCR,
              T_IMAK-VGSBER       to T_ALV1-AGSBER,
              T_IMAK-NAME         to T_ALV1-NAME.
      endif.
      move TW_SAIDA-TXTTIPO    to T_ALV1-TXTTIPO.
      if TW_SAIDA-ANLN1 is initial.
        concatenate TW_SAIDA-AUFNR TW_SAIDA-TXTIMOB
               into T_ALV1-ANLN1 separated by '-'.
      else.
        concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2
               into T_ALV1-ANLN1 separated by '-'.
      endif.
      if TW_SAIDA-BELNR2 eq 'PEDIDO'.
        move TW_SAIDA-NETWR      to T_ALV1-NETWR.
      endif.
      move TW_SAIDA-DMBTR      to T_ALV1-DMBTR.
      move TW_SAIDA-TOTPG      to T_ALV1-TOTPG.
      move TW_SAIDA-TOTAPG     to T_ALV1-TOTAPG.
      if not TW_SAIDA-BELNR3 is initial.
        if TW_SAIDA-BELNR2(2) = 'FI'.
          T_ALV1-MODULO = 'FI (Financeiro)'.
          clear: T_ALV1-EBELN, T_ALV1-EBELP.
        else.
          T_ALV1-MODULO = 'FI (Contábil)'.
          clear: T_ALV1-EBELN, T_ALV1-EBELP.
        endif.
      else.
        T_ALV1-MODULO = 'MM-Compras'.
      endif.
      if TW_SAIDA-BELNR2(4) eq 'MIGO'
      or TW_SAIDA-BELNR2(4) eq 'MIRO'
      or TW_SAIDA-BELNR2(2) eq 'PE'.
        T_ALV1-MODULO = 'MM-Compras'.
      endif.
      move TW_SAIDA-EBELN      to T_ALV1-EBELN.
      move TW_SAIDA-EBELP      to T_ALV1-EBELP.
      move TW_SAIDA-AEDAT      to T_ALV1-AEDAT.
      move TW_SAIDA-BELNR2     to T_ALV1-BELNR2.
      move TW_SAIDA-BELNR3     to T_ALV1-BELNR3.
      move TW_SAIDA-NETPR      to T_ALV1-NETPR.
      move TW_SAIDA-FORNECEDOR to T_ALV1-FORNECEDOR.
      move TW_SAIDA-PAGO30     to T_ALV1-PAGO30.
      move TW_SAIDA-PAGO60     to T_ALV1-PAGO60.
      move TW_SAIDA-PAGO90     to T_ALV1-PAGO90.
      move TW_SAIDA-PAGO120    to T_ALV1-PAGO120.
      move TW_SAIDA-PAGO150    to T_ALV1-PAGO150.
      move TW_SAIDA-PAGO180    to T_ALV1-PAGO180.
      move TW_SAIDA-PAGO210    to T_ALV1-PAGO210.
      move TW_SAIDA-PAGO999    to T_ALV1-PAGO999.

      read table T_IMAKPA with key POSNR = TW_SAIDA-POSNR.
      if SY-SUBRC = 0.
        move-corresponding T_IMAKPA to T_ALV1.
        T_ALV1-BUTXT = W_NOME.
        T_ALV1-ABUKRS = TW_SAIDA-ABUKRS.
      endif.


      move TW_SAIDA-TXTIMOB    to T_ALV1-TXTIMOB.


      concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2 into T_ALV1-ANLN1 separated by '-'.

      append T_ALV1.
      append T_ALV1 to IT_OUTTAB1.
      clear: T_ALV1.

    endloop.
    delete IT_OUTTAB1 where MODULO eq 'FI (Contábil)'.
    delete IT_OUTTAB1 where AEDAT not in S_BUDAT
              and ( BELNR2(4) eq 'FIAP'
                or  BELNR2    eq 'FBA7' ).

    perform BUSCA_IMPOSTOS tables T_ALV1.
    perform BUSCA_IMPOSTOS tables IT_OUTTAB1.

  elseif not P_CONSOL is initial.

    perform MONTA_FIELDCAT using:
         'POSNR'          'T_ALV2' 'ZES_IM01' 'Sol. Inv.         ' ' ' ' ' ' ' ' ' ' ',
         'DESCR'          'T_ALV2' 'IMAKT'    'Descrição         ' ' ' ' ' ' ' ' ' '30',

         'ABUKRS'         'T_ALV2' 'IMAK'     'Empresa           ' ' ' ' ' ' ' ' ' '08',
         'BUTXT'          'T_ALV2' 'T001'     'Nome da Empresa   ' ' ' ' ' ' ' ' ' '25',
         'AGSBER'         'T_ALV2' 'IMAKPA'   'Filial            ' ' ' ' ' ' ' ' ' '08',
         'NAME'           'T_ALV2' '1BBRANCH' 'Nome da Filial    ' ' ' ' ' ' ' ' ' '25',
         'AKOSTL'         'T_ALV2' 'IMAKPA'   'Centro de Custo   ' ' ' ' ' ' ' ' ' '10',
         'LTEXT'          'T_ALV2' 'CSKT'     'Descrição Centro de Custo' ' ' ' ' ' ' ' ' '25',

         'TXTTIPO'        'T_ALV2' 'ZES_IM01' 'Classificação     ' ' ' ' ' ' ' ' ' ' ',
         'ANLN1'          'T_ALV2' ' '        'Nro. Objeto Custo ' ' ' ' ' ' ' ' ' '20',
         'TXTIMOB'        'T_ALV2' ' '        'Denominação Imobilizado ' ' ' ' ' ' ' ' ' '40',
         'NETWR'          'T_ALV2' 'ZES_IM01' 'Vlr. Pedido       ' ' ' ' ' ' ' ' ' ' ',
         'DMBTR'          'T_ALV2' 'ZES_IM01' 'Vlr. Realizado    ' ' ' ' ' ' ' ' ' ' ',
         'MODULO'         'T_ALV2' 'ZES_IM01' 'Módulo            ' ' ' ' ' ' ' ' ' '20',
         'DIFERENCA'      'T_ALV2' 'ZES_IM01' 'Pedido (-) Realizado         ' ' ' ' ' ' ' ' ' ' ',
         'EBELN'          'T_ALV2' 'ZES_IM01' 'Pedido Compra     ' ' ' ' ' ' ' ' ' ' ',
         'EBELP'          'T_ALV2' 'ZES_IM01' 'Item              ' ' ' ' ' ' ' ' ' ' ',
         'SGTXT'          'T_ALV2' 'ZES_IM01' 'Texto do item     ' ' ' ' ' ' ' ' ' ' ',
         'AEDAT'          'T_ALV2' 'ZES_IM01' 'Data PG           ' ' ' ' ' ' ' ' ' ' ',
         'BELNR2'         'T_ALV2' 'ZES_IM01' 'Tipo Doc.         ' ' ' ' ' ' ' ' ' ' ',
         'BELNR1'         'T_ALV2' 'ZES_IM01' 'Docum. (MM)       ' ' ' ' ' ' ' ' ' ' ',
         'BELNR3'         'T_ALV2' 'ZES_IM01' 'Doc. Contábil     ' ' ' ' ' ' ' ' ' ' ',
         'NFENUM'         'T_ALV2' 'ZES_IM01' 'Nº NF             ' ' ' ' ' ' ' ' ' ' ',
         'NETPR'          'T_ALV2' 'ZES_IM01' 'Vlr. Doc.         ' ' ' ' ' ' ' ' ' ' ',
         'MATNR'          'T_ALV2' 'ZES_IM01' 'Material          ' ' ' ' ' ' ' ' ' ' ',
         'TXZ01'          'T_ALV2' 'ZES_IM01' 'Denominação do Pedido' ' ' ' ' ' ' ' ' ' ',
         'SRVPOS'         'T_ALV2' 'ZES_IM01' 'Serviço           ' ' ' ' ' ' ' ' ' ' ',
         'KTEXT1'         'T_ALV2' 'ZES_IM01' 'Descriçao         ' ' ' ' ' ' ' ' ' ' ',
         'MENGE'          'T_ALV2' 'ZES_IM01' 'Qtde.             ' ' ' ' ' ' ' ' ' ' ',
         'MEINS'          'T_ALV2' 'ZES_IM01' 'Unid.             ' ' ' ' ' ' ' ' ' ' ',
         'FORNECEDOR'     'T_ALV2' 'ZES_IM01' 'Fornecedor        ' ' ' ' ' ' ' ' ' ' ',
         'EMISSOR'        'T_ALV2' 'ZES_IM01' 'Emissor           ' ' ' ' ' ' ' ' ' ' '.

    loop at TW_SAIDA.
      if  S_EBELN is not initial
    or S_LIFNR is not initial
    or S_KONNR is not initial
    or S_LIFRE is not initial.
        if ( TW_SAIDA-BELNR2 eq 'MBST'
            or TW_SAIDA-BELNR2 eq 'FB01'
            or TW_SAIDA-BELNR2 eq 'MB1A'
            or TW_SAIDA-BELNR2 eq 'FB08'
            or TW_SAIDA-BELNR2 eq 'FB05'
            or TW_SAIDA-BELNR2 eq 'FB50'
            or TW_SAIDA-BELNR2 eq 'Ciclo Rate'
            or TW_SAIDA-BELNR2 eq 'FBCJ'
            or TW_SAIDA-BELNR2 eq 'FBR2'
            or TW_SAIDA-BELNR2 eq 'FBA7'
            or TW_SAIDA-BELNR2 eq 'FBZ2'
            or TW_SAIDA-BELNR2 eq 'FBD5'
            or TW_SAIDA-BELNR2 eq 'FBVB'
            or TW_SAIDA-BELNR2 eq 'FB1K'
            or TW_SAIDA-BELNR2 eq 'PRRW' ).
          continue.
        endif.
      endif.
      if TW_SAIDA-TXTTIPO(5) eq 'OBRAS' or
       TW_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( TW_SAIDA-TXTTIPO(5) eq 'IMOBI' and not TW_SAIDA-AUFNR is initial ) ..
        TW_SAIDA-ANLN1 = TW_SAIDA-AUFNR.
        TW_SAIDA-AUFNR = TW_SAIDA-AUFNR.
      endif.

      if  TW_SAIDA-ANLN1 is initial.
        TW_SAIDA-ANLN1 = TW_SAIDA-AUFNR.
      endif.

      if TW_SAIDA-ANLN1 = 'PLANEJADO' or
            TW_SAIDA-ANLN1 = 'EXTRA'.
        continue.
      endif.
      clear T_ALV2.

      perform AJUSTES_ALV.

      move-corresponding TW_SAIDA to T_ALV2.

      move TW_SAIDA-EBELN      to T_ALV2-EBELN.
      move TW_SAIDA-EBELP      to T_ALV2-EBELP.

      move TW_SAIDA-POSNR      to T_ALV2-POSNR.
      sort T_IMAK by POSNR TXT50.
      read table T_IMAK with key POSNR = T_ALV2-POSNR
                             binary search.
      if SY-SUBRC eq 0.
        move: T_IMAK-TXT50        to T_ALV2-DESCR,
              T_IMAK-VGSBER       to T_ALV2-AGSBER,
              T_IMAK-NAME         to T_ALV2-NAME.
      endif.
      move TW_SAIDA-TXTTIPO    to T_ALV2-TXTTIPO.
      if TW_SAIDA-ANLN1 is initial.
*        CONCATENATE tw_saida-aufnr tw_saida-txt50
*               INTO t_alv2-anln1 SEPARATED BY '-'.
        T_ALV2-ANLN1 = TW_SAIDA-AUFNR.
      else.
        concatenate TW_SAIDA-ANLN1 TW_SAIDA-ANLN2
               into T_ALV2-ANLN1 separated by '-'.
      endif.
      if TW_SAIDA-BELNR2 eq 'PEDIDO'.
        move TW_SAIDA-NETWR      to T_ALV2-NETWR.
        clear TW_SAIDA-BELNR3.
        move TW_SAIDA-BELNR1      to T_ALV2-BELNR1.
      endif.
      move TW_SAIDA-DMBTR      to T_ALV2-DMBTR.
      if not TW_SAIDA-BELNR3 is initial.
        if TW_SAIDA-BELNR2(2) = 'FI'.
          T_ALV2-MODULO = 'FI (Financeiro)'.
          clear: T_ALV2-EBELN, T_ALV2-EBELP.
        else.

          if TW_SAIDA-BELNR2(4) eq 'MIGO'
or TW_SAIDA-BELNR2(4) eq 'MIRO'
or TW_SAIDA-BELNR2(2) eq 'PE'.
            T_ALV2-MODULO = 'MM-Compras'.
          else.

            T_ALV2-MODULO = 'FI (Contábil)'.
            clear: T_ALV2-EBELN, T_ALV2-EBELP, T_ALV2-TXZ01, T_ALV2-SGTXT,
                   T_ALV2-EMISSOR, T_ALV2-FORNECEDOR, T_ALV2-SRVPOS, "tw_saida-sgtxt,
                   T_ALV2-KTEXT1, T_ALV2-MATNR, T_ALV2-NFENUM, T_EKKO-MENGE, T_ALV2-MEINS, T_ALV2-MENGE.
            T_ALV2-SGTXT = TW_SAIDA-SGTXT.


          endif.
        endif.
      else.
        T_ALV2-MODULO = 'MM-Compras'.
      endif.
      if TW_SAIDA-BELNR2(4) eq 'MIGO'
      or TW_SAIDA-BELNR2(4) eq 'MIRO'
      or TW_SAIDA-BELNR2(2) eq 'PE'.
        T_ALV2-MODULO = 'MM-Compras'.
      endif.
      move TW_SAIDA-DIFERENCA  to T_ALV2-DIFERENCA.
*      MOVE tw_saida-ebeln      TO t_alv2-ebeln.
*      MOVE tw_saida-ebelp      TO t_alv2-ebelp.
      if TW_SAIDA-BELNR2(2) = 'FI'.
        clear: T_ALV2-EBELN, T_ALV2-EBELP.
      endif.
      move TW_SAIDA-AEDAT      to T_ALV2-AEDAT.
      move TW_SAIDA-BELNR2     to T_ALV2-BELNR2.
      move TW_SAIDA-BELNR1     to T_ALV2-BELNR1.
      move TW_SAIDA-BELNR3     to T_ALV2-BELNR3.
      move TW_SAIDA-NFENUM     to T_ALV2-NFENUM.
      move TW_SAIDA-NETPR      to T_ALV2-NETPR.
      move TW_SAIDA-MATNR      to T_ALV2-MATNR.
      move TW_SAIDA-TXZ01      to T_ALV2-TXZ01.
      move TW_SAIDA-SRVPOS     to T_ALV2-SRVPOS.
      move TW_SAIDA-KTEXT1     to T_ALV2-KTEXT1.
      move TW_SAIDA-MENGE      to T_ALV2-MENGE.
      move TW_SAIDA-MEINS      to T_ALV2-MEINS.
      move TW_SAIDA-FORNECEDOR to T_ALV2-FORNECEDOR.
      move TW_SAIDA-EMISSOR    to T_ALV2-EMISSOR.
      move TW_SAIDA-BELNR1     to T_ALV2-BELNR1.

      read table T_IMAKPA with key POSNR = TW_SAIDA-POSNR.
      if SY-SUBRC = 0.
        move-corresponding T_IMAKPA to T_ALV2.
        T_ALV2-BUTXT = W_NOME.
        T_ALV2-ABUKRS = TW_SAIDA-ABUKRS.
      endif.

      move TW_SAIDA-TXTIMOB    to T_ALV2-TXTIMOB.

*      CONCATENATE tw_saida-anln1 tw_saida-anln2 INTO t_alv2-anln1 SEPARATED BY '-'.

      case T_ALV2-BELNR2.
        when 'FB01'.
          clear: T_ALV2-EMISSOR, T_ALV2-FORNECEDOR, T_ALV2-SRVPOS, T_ALV2-KTEXT1, T_ALV2-MENGE,
             T_ALV2-MEINS, T_ALV2-EBELN, T_ALV2-EBELP, T_ALV2-NFENUM, T_ALV2-BELNR1, T_ALV2-TXZ01.
        when 'PEDIDO'.
          clear T_ALV2-NETPR.
        when 'FB50'.
          clear: T_ALV2-EBELN, T_ALV2-EBELP.
        when others.
      endcase.

      if T_ALV2-MODULO = 'FI (Contábil)'.
        clear: T_ALV2-EBELN, T_ALV2-EBELP, T_ALV2-TXZ01, T_ALV2-SGTXT,
               T_ALV2-EMISSOR, T_ALV2-FORNECEDOR, T_ALV2-SRVPOS, "tw_saida-sgtxt,
               T_ALV2-KTEXT1, T_ALV2-MATNR, T_ALV2-NFENUM, T_EKKO-MENGE, T_ALV2-MEINS, T_ALV2-BELNR1, T_ALV2-MENGE.
        T_ALV2-SGTXT = TW_SAIDA-SGTXT.
      endif.

      if T_ALV2-SGTXT = 'ZPI' or T_ALV2-SGTXT = 'ZIM'.
        clear T_ALV2-SGTXT.
      endif.

      append T_ALV2.
      append T_ALV2 to IT_OUTTAB2.
      clear T_ALV2.

    endloop.

  elseif not P_ANALIT is initial.

    perform MONTA_FIELDCAT using:
         'POSNR'          'T_ALV3' 'ZES_IM01' 'Sol. Inv.         ' ' ' ' ' ' ' ' ' ' ',
         'DESCR'          'T_ALV3' 'IMAKT'    'Descrição         ' ' ' ' ' ' ' ' ' '30',

         'ABUKRS'         'T_ALV3' 'IMAK'     'Empresa           ' ' ' ' ' ' ' ' ' '08',
         'BUTXT'          'T_ALV3' 'T001'     'Nome da Empresa   ' ' ' ' ' ' ' ' ' '25',
         'AGSBER'         'T_ALV3' 'IMAKPA'   'Filial            ' ' ' ' ' ' ' ' ' '08',
         'NAME'           'T_ALV3' '1BBRANCH' 'Nome da Filial    ' ' ' ' ' ' ' ' ' '25',
         'AKOSTL'         'T_ALV3' 'IMAKPA'   'Centro de Custo   ' ' ' ' ' ' ' ' ' '10',
         'LTEXT'          'T_ALV3' 'CSKT'     'Descrição Centro de Custo' ' ' ' ' ' ' ' ' '25',

         'TXTTIPO'        'T_ALV3' 'ZES_IM01' 'Classificação     ' ' ' ' ' ' ' ' ' ' ',
         'ANLN1'          'T_ALV3' ' '        'Nro. Objeto Custo ' ' ' ' ' ' ' ' ' '65',
         'TXTIMOB'        'T_ALV3' ' '        'Denominação Imobilizado ' ' ' ' ' ' ' ' ' '',
         'ORCADO'         'T_ALV3' 'ZES_IM01' 'Planejado         ' ' ' ' ' ' ' ' ' ' ',
         'MODULO'         'T_ALV3' 'ZES_IM01' 'Módulo            ' ' ' ' ' ' ' ' ' '20',
         'APROVADO'       'T_ALV3' 'ZES_IM01' 'Plan+Extra        ' ' ' ' ' ' ' ' ' ' ',
         'NETWR'          'T_ALV3' 'ZES_IM01' 'Vlr. Pedido       ' ' ' ' ' ' ' ' ' ' ',
         'DMBTR'          'T_ALV3' 'ZES_IM01' 'Vlr. Realizado    ' ' ' ' ' ' ' ' ' ' ',
         'TOTPG'          'T_ALV3' 'ZES_IM01' 'Total Pago        ' ' ' ' ' ' ' ' ' ' ',
         'TOTAPG'         'T_ALV3' 'ZES_IM01' 'Total a Pagar     ' ' ' ' ' ' ' ' ' ' ',
         'PED_AP'         'T_ALV3' 'ZES_IM01' 'Ped. Aprov.       ' ' ' ' ' ' ' ' ' ' ',
         'REAL_AP'        'T_ALV3' 'ZES_IM01' 'Real Aprov.       ' ' ' ' ' ' ' ' ' ' '.

    loop at TW_SAIDA.
      clear T_ALV3.

      perform AJUSTES_ALV.

      if TW_SAIDA-BELNR2(4) = 'MIRO' and ( TW_SAIDA-SGTXT(3) eq 'ZPI' or TW_SAIDA-SGTXT(3) eq 'ZEF' ).
        clear: TW_SAIDA-DMBTR, TW_SAIDA-DIFERENCA.
      endif.
      move-corresponding TW_SAIDA to T_ALV3.
      move TW_SAIDA-POSNR      to T_ALV3-POSNR.
      sort T_IMAK by POSNR TXT50.
      read table T_IMAK with key POSNR = T_ALV3-POSNR
                             binary search.
      if SY-SUBRC eq 0.
        move: T_IMAK-TXT50        to T_ALV3-DESCR,
              T_IMAK-VGSBER       to T_ALV3-AGSBER,
              T_IMAK-NAME         to T_ALV3-NAME.
      endif.
      move TW_SAIDA-TXTTIPO    to T_ALV3-TXTTIPO.
      if TW_SAIDA-ANLN1 is initial.
        concatenate TW_SAIDA-AUFNR TW_SAIDA-TXT50
               into T_ALV3-ANLN1 separated by '-'.
      else.
        concatenate TW_SAIDA-ANLN1 TW_SAIDA-TXT50
               into T_ALV3-ANLN1 separated by '-'.
      endif.
      move TW_SAIDA-ORCADO     to T_ALV3-ORCADO.
      move TW_SAIDA-APROVADO   to T_ALV3-APROVADO.
      if TW_SAIDA-BELNR2 eq 'PEDIDO'.
        move TW_SAIDA-NETWR      to T_ALV3-NETWR.
      endif.
      if not TW_SAIDA-BELNR3 is initial.
        if TW_SAIDA-BELNR2(2) = 'FI'.
          T_ALV3-MODULO = 'FI (Financeiro)'.
          clear: T_ALV2-EBELN, T_ALV2-EBELP.
        else.
          T_ALV3-MODULO = 'FI (Contábil)'.
          clear: T_ALV2-EBELN, T_ALV2-EBELP.
        endif.
      else.
        T_ALV3-MODULO = 'MM-Compras'.
      endif.
      if TW_SAIDA-BELNR2(4) eq 'MIGO'
      or TW_SAIDA-BELNR2(4) eq 'MIRO'
      or TW_SAIDA-BELNR2(2) eq 'PE'.
        T_ALV3-MODULO = 'MM-Compras'.
      endif.
      move TW_SAIDA-DMBTR      to T_ALV3-DMBTR.
      move TW_SAIDA-TOTPG      to T_ALV3-TOTPG.
      move TW_SAIDA-TOTAPG     to T_ALV3-TOTAPG.
      move TW_SAIDA-PED_AP     to T_ALV3-PED_AP.
      move TW_SAIDA-REAL_AP    to T_ALV3-REAL_AP.

      read table T_IMAKPA with key POSNR = TW_SAIDA-POSNR.
      if SY-SUBRC = 0.
        move-corresponding T_IMAKPA to T_ALV3.
        T_ALV3-BUTXT = W_NOME.
        T_ALV3-ABUKRS = TW_SAIDA-ABUKRS.
      endif.

      move TW_SAIDA-TXTIMOB    to T_ALV3-TXTIMOB.

      append T_ALV3.
      append T_ALV3 to IT_OUTTAB3.
      clear T_ALV3.

    endloop.

  endif.

endform.                    " f_monta_catalogo
*&---------------------------------------------------------------------*
*&      Form  f_monta_quebra
*&---------------------------------------------------------------------*
form F_MONTA_QUEBRA.

  clear I.
  refresh IT_SORT.

endform.                    " f_monta_quebra
*&---------------------------------------------------------------------*
*&      Module  m_container  OUTPUT
*&---------------------------------------------------------------------*
module M_CONTAINER output.
  if O_CUSTOM_CONTAINER is initial.

    create object O_CUSTOM_CONTAINER
      exporting
        REPID = V_REPID
        DYNNR = '0200'
        SIDE  = O_CUSTOM_CONTAINER->DOCK_AT_BOTTOM
        RATIO = 80.

    create object O_GRID
      exporting
        I_PARENT = O_CUSTOM_CONTAINER.

    IT_LAYOUT-ZEBRA = 'X'.
    VARIANTE-REPORT = SY-REPID.
    if not P_SINCAX is initial.
      call method O_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        exporting
          IS_LAYOUT       = IT_LAYOUT
          I_SAVE          = 'A'
          IS_VARIANT      = VARIANTE
        changing
          IT_OUTTAB       = IT_OUTTAB1
          IT_SORT         = IT_SORT
          IT_FIELDCATALOG = IT_FIELDCATALOG.
    elseif not P_CONSOL is initial.
      call method O_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        exporting
          IS_LAYOUT       = IT_LAYOUT
          I_SAVE          = 'A'
          IS_VARIANT      = VARIANTE
        changing
          IT_OUTTAB       = IT_OUTTAB2
          IT_SORT         = IT_SORT
          IT_FIELDCATALOG = IT_FIELDCATALOG.
    elseif not P_ANALIT is initial.
      call method O_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        exporting
          IS_LAYOUT       = IT_LAYOUT
          I_SAVE          = 'A'
          IS_VARIANT      = VARIANTE
        changing
          IT_OUTTAB       = IT_OUTTAB3
          IT_SORT         = IT_SORT
          IT_FIELDCATALOG = IT_FIELDCATALOG.
    endif.

  endif.

endmodule.                 " m_container  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
module USER_COMMAND_0200 input.

  call method CL_GUI_CFW=>DISPATCH.

  OK_CODE = SY-UCOMM.
  case OK_CODE.
    when 'BACK'.
      call method O_CUSTOM_CONTAINER->FREE.
      leave to screen 0.
  endcase.

endmodule.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_STATUS  OUTPUT
*&---------------------------------------------------------------------*
module M_STATUS output.

  set titlebar '200'.
  set pf-status '200'.

endmodule.                 " M_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_MSG
*&---------------------------------------------------------------------*
form F_MSG  using    P_MSG
                     P_POR.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      PERCENTAGE = P_POR
      TEXT       = P_MSG.


endform.                    " F_MSG
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_CILCO_RATEIO
*&---------------------------------------------------------------------*
form F_SELECIONA_CILCO_RATEIO .

*  DATA: t_coep LIKE t_ciclo OCCURS 0 WITH HEADER LINE.

  data: begin of T_COBK occurs 0,
          KOKRS like COBK-KOKRS,
          BELNR like COBK-BELNR,
          BUDAT like COBK-BUDAT,
          BLTXT like COBK-BLTXT,
        end of T_COBK.

  read table T_IMAK index 1.

*Inicio alteração IR080294
*  SELECT kokrs belnr budat bltxt
*    FROM cobk
*    INTO TABLE t_cobk
*    WHERE cobk~kokrs EQ 'MAGI'           AND
*          cobk~vrgng IN ('RKU1', 'RKIV') AND
*          cobk~budat IN s_budat.

  select KOKRS BELNR BUDAT BLTXT
  from COBK
  into table T_COBK
  where COBK~KOKRS eq V_KOKRS           and
        COBK~VRGNG in ('RKU1', 'RKIV') and
        COBK~BUDAT in S_BUDAT.
*Fim alteração IR080294


  check SY-SUBRC is initial.

  ranges: R_OBJNR for IMAKZ-OBJNR.
  loop at T_IMAKZ.
    R_OBJNR-SIGN = 'I'.
    R_OBJNR-OPTION = 'EQ'.
    R_OBJNR-LOW = T_IMAKZ-OBJNR.
    collect R_OBJNR. clear R_OBJNR.
  endloop.

*Inicio alteração IR080294

*SELECT kokrs belnr buzei wogbtr wtgbtr
*           beknz objnr
*        FROM coep
*        INTO TABLE t_ciclo
*        FOR ALL ENTRIES IN t_cobk
*        WHERE kokrs EQ 'MAGI'           AND  -----------
*              belnr EQ t_cobk-belnr     AND
*              bukrs EQ t_imak-abukrs    AND
*              gsber IN s_werks          AND
*              objnr IN r_objnr.

  select KOKRS BELNR BUZEI WOGBTR WTGBTR
         BEKNZ OBJNR
      from COEP
      into table T_CICLO
      for all entries in T_COBK
      where KOKRS eq V_KOKRS           and
            BELNR eq T_COBK-BELNR     and
            BUKRS eq T_IMAK-ABUKRS    and
            GSBER in S_WERKS          and
            OBJNR in R_OBJNR.
  check SY-SUBRC is initial.
  sort T_COBK by KOKRS BELNR.
*Fim alteração IR080294

  loop at T_CICLO.
    read table T_COBK with key KOKRS = T_CICLO-KOKRS
                               BELNR = T_CICLO-BELNR
                               binary search.
    if SY-SUBRC = 0.
      T_CICLO-BUDAT = T_COBK-BUDAT.
      T_CICLO-BLTXT = T_COBK-BLTXT.
      modify T_CICLO.
    endif.
  endloop.
endform.                    " F_SELECIONA_CILCO_RATEIO
*&---------------------------------------------------------------------*
*&      Form  COTACAO
*&---------------------------------------------------------------------*

form COTACAO .

  data: WL_FATOR type TCURR-UKURS,
        WL_GDATU type TCURR-GDATU,
        WL_DATA  type SY-DATUM.

  WL_DATA = SY-DATUM.

*  w_data_e = w_data_u = wl_data.

  select single UKURS GDATU from TCURR into (WL_FATOR, WL_GDATU)
    where KURST = 'B' and
          FCURR = 'BRL' and
          TCURR = 'USD'.


  if SY-SUBRC = 0.
    if WL_FATOR < 0.
      WL_FATOR = WL_FATOR * ( - 1 ).
    endif.
    write: WL_FATOR to W_CO_U.
    call function 'CONVERSION_EXIT_INVDT_OUTPUT'
      exporting
        INPUT  = WL_GDATU
      importing
        OUTPUT = W_DATA_U.
  endif.
  select single UKURS GDATU from TCURR into (WL_FATOR, WL_GDATU)
    where KURST = 'B' and
          FCURR = 'BRL' and
          TCURR = 'EUR'.

*  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
*    EXPORTING
**   CLIENT                  = SY-MANDT
*      date                    = wl_data
*      foreign_amount          = 1
*      foreign_currency        = 'EUR'
*      local_currency          = 'BRL'
*     rate                    = 0
*     type_of_rate            = 'B'
*     read_tcurr              = 'X'
*   IMPORTING
*     exchange_rate           = wl_fator
*   EXCEPTIONS
*     no_rate_found           = 1
*     overflow                = 2
*     no_factors_found        = 3
*     no_spread_found         = 4
*     derived_2_times         = 5
*     OTHERS                  = 6
*            .
  if SY-SUBRC = 0.
    if WL_FATOR < 0.
      WL_FATOR = WL_FATOR * ( - 1 ).
    endif.
    write: WL_FATOR to W_CO_E.
    call function 'CONVERSION_EXIT_INVDT_OUTPUT'
      exporting
        INPUT  = WL_GDATU
      importing
        OUTPUT = W_DATA_E.
  endif.
endform.                    " COTACAO
*&---------------------------------------------------------------------*
*&      Form  Z_EKBE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_BELNR  text
*----------------------------------------------------------------------*
form Z_EKBE  using    P_BELNR changing P_DATA.

  data: WL_HKONT type BSIS-HKONT.

  if P_BELNR = '2'.

    loop at T_EKKN where AUFNR = T_EKKO-AUFNR  and
                         EBELN = T_EKKO-EBELN  and
                         EBELP = T_EKKO-EBELP.

      read table T_EKBE_1 with key EBELN = T_EKKN-EBELN
                              EBELP = T_EKKN-EBELP
                              ZEKKN = T_EKKN-ZEKKN
                              VGABE = 2
                              AUFNR = T_IMAKZ-AUFNR.
      if SY-SUBRC <> 0.
        read table T_EKBE_1 with key EBELN = T_EKKN-EBELN
                                EBELP = T_EKKN-EBELP
                                ZEKKN = T_EKKN-ZEKKN
                                VGABE = 3
                                AUFNR = T_IMAKZ-AUFNR.
        if SY-SUBRC <> 0.
          perform Z_EKBE2 using P_BELNR changing P_DATA.
        endif.
      endif.
*---> 05/07/2023 - Migração S4 - DL
      SORT T_RBKP BY BELNR GJAHR.
*<--- 05/07/2023 - Migração S4 - DL
      loop at T_EKBE_1 where EBELN = T_EKKN-EBELN and
                              EBELP = T_EKKN-EBELP and
                              ZEKKN = T_EKKN-ZEKKN and
                              ( VGABE = 2 or VGABE = 3 ).
*        IF NOT t_ekbe_1-anln1 IS INITIAL.
*          IF t_ekbe_1-anln1 NE t_imaka-anln1.
*            CONTINUE.
*          ENDIF.
*        ENDIF.

        if not T_EKBE_1-AUFNR is initial.
          if T_EKBE_1-AUFNR ne  T_IMAKZ-AUFNR.
            continue.
          endif.
        endif.


*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
        if P_MOEDA      eq 'BRL' and
           T_EKBE_1-WAERS ne 'BRL'.
          if not P_HIST is initial.
            P_DATA = T_EKBE_1-BUDAT.

          else.
            P_DATA = P_DATAC.
          endif.

        elseif P_MOEDA ne 'BRL' and
*               t_ekbe-waers ne p_moeda.
               T_EKBE_1-WAERS ne P_MOEDA.

          if not P_HIST is initial.
            P_DATA = T_EKBE_1-BUDAT.
            clear: WE_BKPF.
            read table WE_BKPF with key AWKEY = T_EKBE_1-AWKEY
                                               binary search.
            if SY-SUBRC = 0.
              try.
                  if T_EKBE_1-DMBTR gt 0.
                    multiply T_EKBE_1-DMBTR by -1.
                  endif.
                  divide T_EKBE_1-DMBTR by WE_BKPF-KURS2.
*                  exit.
                catch CX_SY_ZERODIVIDE.
              endtry.
            endif.
          else.
            P_DATA = P_DATAC.
          endif.
          if WE_BKPF-KURS2 is initial.
            perform F_CONVERTE_COTACAO using T_EKBE_1-WAERS
                                             P_DATA
                                    changing T_EKBE_1-DMBTR.
          endif.
        elseif P_MOEDA ne 'BRL' and
             T_EKBE_1-WAERS eq P_MOEDA.
          T_EKBE_1-DMBTR = T_EKBE_1-WRBTR.
        endif.
*** Fim da rotina da TAXA


        move:
              T_EKBE_1-XBLNR to TW_SAIDA-NFENUM,
              T_EKBE_1-MENGE to TW_SAIDA-MENGE,
              T_EKBE_1-BUDAT to TW_SAIDA-AEDAT,
              T_EKBE_1-BUZEI to TW_SAIDA-BUZEI,
              T_EKBE_1-DMBTR to TW_SAIDA-NETPR.

        if T_EKBE_1-VGABE = 1.
          move: T_EKBE_1-BELNR to TW_SAIDA-BELNR1.
*                  t_ekbe-dmbtr TO tw_saida-dmbtr.
          clear TW_SAIDA-BELNR2.

          if T_EKBE_1-BWART = '102'.
            TW_SAIDA-BELNR2 = 'MIGO-EST'.
          elseif T_EKBE_1-BWART = '122'.
            TW_SAIDA-BELNR2 = 'MIGO-DEV'.
          else.
            TW_SAIDA-BELNR2 = 'MIGO'.
          endif.
        else.
          read table T_RBKP with key BELNR = T_EKBE_1-BELNR
                                     GJAHR = T_EKBE_1-GJAHR
                                     binary search.
          if SY-SUBRC = 0.
            TW_SAIDA-NFENUM = T_RBKP-XBLNR .
          endif.

          move: T_EKBE_1-BELNR to TW_SAIDA-BELNR1, "2
                T_EKBE_1-DMBTR to TW_SAIDA-DMBTR.
          clear: TW_SAIDA-BELNR2." tw_saida-dmbtr.
          if T_EKBE_1-VGABE = 2 and
             T_EKBE_1-SHKZG = 'H'.
            TW_SAIDA-BELNR2 = 'MIRO-NC'.
          else.
*            tw_saida-belnr2 = 'MIRO'.
            concatenate 'MIRO' T_EKKN-ZEKKN into TW_SAIDA-BELNR2 separated by '-'.
          endif.

        endif.

        read table WE_BKPF with key AWKEY = T_EKBE_1-AWKEY
                                    binary search.
        if SY-SUBRC = 0.
*            IF t_ekbe-vgabe = 1.
          if T_EKBE_1-VGABE eq 1
          or T_EKBE_1-VGABE eq 2
          or T_EKBE_1-VGABE eq 3.
            loop at WE_BKPF where AWKEY eq T_EKBE_1-AWKEY
                                  and BLART ne 'ML'.

              move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
              exit.
            endloop.
          else.
            move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
          endif.
        else.
        endif.

        if T_EKBE_1-SHKZG = 'H'.
          TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ) .
          TW_SAIDA-MENGE = TW_SAIDA-MENGE * ( - 1 ) .
        endif.

        if T_EKBE_1-VGABE eq 1.
        endif.

*Início Alteração Ricardo Furst.
        if P_ALV is initial.
          clear:   TW_SAIDA-EXTROW, "tw_saida-werks,
                 TW_SAIDA-TXT50, TW_SAIDA-TXTPED, "tw_saida-netpr,
                 TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD,
                 TW_SAIDA-ANLKL," tw_saida-matnr, tw_saida-txz01, tw_saida-meins,
                 TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR.
        endif.
*Fim Alteração Ricardo Furst.
* tw_saida-ktext1, , tw_saida-aufnr  tw_saida-txttipo, tw_saida-srvpos, , tw_saida-txtimob


        read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                   binary search.
        if SY-SUBRC = 0.
          concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
            separated by ' - '.
        endif.

        if T_EKBE_1-VGABE = 1.
          read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                                   binary search.
          if SY-SUBRC = 0.
            concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
              separated by ' - '.
          endif.
        else.
          read table T_RBKP with key BELNR = T_EKBE_1-BELNR
                                     GJAHR = T_EKBE_1-GJAHR
                                     binary search.
          if SY-SUBRC = 0.
            concatenate T_RBKP-NAME1 T_RBKP-LIFNR into TW_SAIDA-EMISSOR
            separated by ' - '.
          endif.
        endif.

        TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).

        TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]

        read table T_161 with key BSART = T_EKKO-BSART
                                  binary search.
        if SY-SUBRC = 0.
          concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
          separated by ' - '.
        else.
          clear TW_SAIDA-TIPOPED.
        endif.

        if T_EKBE_1-VGABE = 3.
          TW_SAIDA-BELNR2 = 'MIRO-DP'.
          perform BUSCA_NFENUM using T_EKBE_1-BELNR
                                     T_EKBE_1-GJAHR
                               changing TW_SAIDA-NFENUM.


          if not TW_SAIDA-KONNR is initial.
            TW_SAIDA-KTPNR = T_EKKO-EBELP.
          endif.
          read table T_RBKP with key BELNR = T_EKBE_1-BELNR.
          if SY-SUBRC = 0.
*            tw_saida-emissor = t_rbkp-lifnr.
            concatenate T_RBKP-NAME1 T_RBKP-LIFNR into TW_SAIDA-EMISSOR
                                                    separated by ' - '.

          else.
            read table T_RBKP1 with key BELNR = T_EKBE_1-BELNR
                                        GJAHR = T_EKBE_1-GJAHR
                                        binary search.

            if SY-SUBRC = 0.
              concatenate T_RBKP1-NAME1 T_RBKP1-LIFNR into TW_SAIDA-EMISSOR
                                                      separated by ' - '.
            endif.

          endif.
        endif.

        TW_SAIDA-WAERS = T_EKBE_1-WAERS.

*Se EKKO-BSART=ZIM e EKBE-VGABE=2 na coluna REALIZADO  deixar com valor igual a zero
        if T_EKKO-BSART(3) = 'ZIM' and T_EKBE_1-VGABE = 2.
          if P_SINCAX eq 'X'.
            select single VGABE from EKBE
              into T_EKBE_1-VGABE
              where EBELN = T_EKBE_1-EBELN and
                    EBELP = T_EKBE_1-EBELP       and
                    VGABE = 1.
            if SY-SUBRC = 0.
              clear TW_SAIDA-SGTXT.
            else.
              clear TW_SAIDA-DMBTR.
            endif.
          else.
            clear TW_SAIDA-DMBTR.
          endif.
        endif.


        TW_SAIDA-KONNR = T_EKKO-KONNR.
        TW_SAIDA-KTPNR = T_EKKO-KTPNR.

        TW_SAIDA-WERKS = T_IMAK-WERKS.

        if not P_SINCAX is initial.
          move T_EKBE_1-BUDAT to TW_SAIDA-BUDAT.
        endif.

        append TW_SAIDA.
      endloop.
    endloop.
  else.
*---> 05/07/2023 - Migração S4 - DL
    SORT T_161 BY BSART.
*<--- 05/07/2023 - Migração S4 - DL
    loop at T_EKKN where ANLN1 = T_IMAKA-ANLN1 and
                         ANLN2 = T_IMAKA-ANLN2 and
                         EBELN = T_EKKO-EBELN  and
                         ZEKKN = T_EKKO-ZEKKN  and
                         EBELP = T_EKKO-EBELP.
*        LOOP AT t_ekbe_1 WHERE ebeln = t_ekko-ebeln AND
*                             ebelp = t_ekko-ebelp AND
*                             zekkn ne t_ekko-zekkn and
*                             belnr =  p_belnr and
*                               vgabe = 2         .
      loop at T_EKBE_1 where EBELN = T_EKKN-EBELN and
                             EBELP = T_EKKN-EBELP and
                             ZEKKN = T_EKKN-ZEKKN and
                             ( VGABE = 2 or VGABE = 3 ).
        if not T_EKBE_1-ANLN1 is initial.
          if T_EKBE_1-ANLN1 ne T_IMAKA-ANLN1.
            continue.
          endif.
        endif.


        if T_EKBE_1-VGABE = 3.
          if T_EKKO-BSART(3) = 'ZIM' or
             T_EKKO-BSART(3) = 'ZPI'.
            read table T_EKBE with key EBELN = T_EKBE_1-EBELN
                                       EBELP = T_EKBE_1-EBELP
                                       VGABE = 1.
            if SY-SUBRC = 0.
*            IF t_ekbe_1-cputm > t_ekbe-cputm AND
*               t_ekbe_1-cpudt > t_ekbe-cpudt.
              if T_EKBE-DMBTR ne 0.
                if T_EKBE-CPUDT ge T_EKBE_1-CPUDT.
                  if T_EKBE-CPUTM > T_EKBE_1-CPUTM.
                    continue.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.
*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
        if P_MOEDA      eq 'BRL' and
           T_EKBE_1-WAERS ne 'BRL'.
          if not P_HIST is initial.
            P_DATA = T_EKBE_1-BUDAT.
          else.
            P_DATA = P_DATAC.
          endif.

        elseif P_MOEDA ne 'BRL' and
               T_EKBE_1-WAERS ne P_MOEDA.

          if not P_HIST is initial.
            P_DATA = T_EKBE_1-BUDAT.
          else.
            P_DATA = P_DATAC.
          endif.
          read table WE_BKPF with key AWKEY = T_EKBE_1-AWKEY
                                    binary search.
          if SY-SUBRC = 0.
*            IF t_ekbe-vgabe = 1.
            clear: T_TAXA.
            read table T_TAXA
              with key BELNR = WE_BKPF-BELNR
                       GJAHR = WE_BKPF-GJAHR
                       BUKRS = WE_BKPF-BUKRS.
            if T_TAXA-DMBE2 is not initial.
              T_EKBE_1-DMBTR = T_EKBE_1-DMBTR / (  T_TAXA-DMBTR / T_TAXA-DMBE2 ). "#EC CI_FLDEXT_OK[2610650]
            else.
              T_EKBE_1-DMBTR = T_EKBE_1-DMBTR / WE_BKPF-KURS2.
            endif.

          else.
            perform F_CONVERTE_COTACAO using T_EKBE_1-WAERS
                                             P_DATA
                                    changing T_EKBE_1-DMBTR.
          endif.
        elseif P_MOEDA ne 'BRL' and
             T_EKBE_1-WAERS eq P_MOEDA.
          T_EKBE_1-DMBTR = T_EKBE_1-WRBTR.
        endif.
*** Fim da rotina da TAXA


        move:
              T_EKBE_1-XBLNR to TW_SAIDA-NFENUM,
              T_EKBE_1-MENGE to TW_SAIDA-MENGE,
              T_EKBE_1-BUDAT to TW_SAIDA-AEDAT,
              T_EKBE_1-DMBTR to TW_SAIDA-NETPR.

        if T_EKBE_1-VGABE = 1.
          move: T_EKBE_1-BELNR to TW_SAIDA-BELNR1.
*                  t_ekbe-dmbtr TO tw_saida-dmbtr.
          clear TW_SAIDA-BELNR2.

          if T_EKBE_1-BWART = '102'.
            TW_SAIDA-BELNR2 = 'MIGO-EST'.
          elseif T_EKBE_1-BWART = '122'.
            TW_SAIDA-BELNR2 = 'MIGO-DEV'.
          else.
            TW_SAIDA-BELNR2 = 'MIGO' .
          endif.
        else.
          read table T_RBKP with key BELNR = T_EKBE_1-BELNR
                                     GJAHR = T_EKBE_1-GJAHR
                                     binary search.
          if SY-SUBRC = 0.
            TW_SAIDA-NFENUM = T_RBKP-XBLNR .
          endif.

          move: T_EKBE_1-BELNR to TW_SAIDA-BELNR1, "2
                T_EKBE_1-DMBTR to TW_SAIDA-DMBTR.
          clear: TW_SAIDA-BELNR2." tw_saida-dmbtr.
          if T_EKBE_1-VGABE = 2 and
             T_EKBE_1-SHKZG = 'H'.
            TW_SAIDA-BELNR2 = 'MIRO-NC'.
          else.
*            tw_saida-belnr2 = 'MIRO'.
            concatenate 'MIRO' T_EKKN-ZEKKN into TW_SAIDA-BELNR2 separated by '-'.
          endif.
        endif.

        read table WE_BKPF with key AWKEY = T_EKBE_1-AWKEY
                                    binary search.
        if SY-SUBRC = 0.
*            IF t_ekbe-vgabe = 1.

          if T_EKBE_1-VGABE eq 1
          or T_EKBE_1-VGABE eq 2
          or T_EKBE_1-VGABE eq 3.
            loop at WE_BKPF where AWKEY eq T_EKBE_1-AWKEY
                              and BLART ne 'ML'.

              move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
              exit.
            endloop.
          else.
            move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
          endif.
        else.
        endif.

*        if t_ekbe_1-shkzg = 'H'.
*          tw_saida-dmbtr = tw_saida-dmbtr * ( - 1 ) .
*          tw_saida-menge = tw_saida-menge * ( - 1 ) .
*        endif.

        if T_EKBE_1-SHKZG eq 'S'.
          if TW_SAIDA-DMBTR lt 0.
            multiply TW_SAIDA-DMBTR by -1.
          endif.
          if TW_SAIDA-NETPR lt 0.
            multiply TW_SAIDA-NETPR by -1.
          endif.
          if TW_SAIDA-MENGE lt 0.
            multiply TW_SAIDA-MENGE by -1.
          endif.
        elseif T_EKBE_1-SHKZG eq 'H'.
          if TW_SAIDA-DMBTR gt 0.
            multiply TW_SAIDA-DMBTR by -1.
          endif.
          if TW_SAIDA-NETPR gt 0.
            multiply TW_SAIDA-NETPR by -1.
          endif.
          if TW_SAIDA-MENGE gt 0.
            multiply TW_SAIDA-MENGE by -1.
          endif.
        endif.

        if T_EKBE_1-VGABE eq 1.
        endif.

*Início Alteração Ricardo Furst.
        if P_ALV is initial.
          clear:   TW_SAIDA-EXTROW, "tw_saida-werks,
                 TW_SAIDA-TXT50, TW_SAIDA-TXTPED, "tw_saida-netpr,
                 TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD,
                 TW_SAIDA-ANLKL," tw_saida-matnr, tw_saida-txz01, tw_saida-meins,
                 TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR.
        endif.
*Fim Alteração Ricardo Furst.
* tw_saida-ktext1, , tw_saida-aufnr  tw_saida-txttipo, tw_saida-srvpos, , tw_saida-txtimob


        read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                                   binary search.
        if SY-SUBRC = 0.
          concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
            separated by ' - '.
        endif.

        if T_EKBE_1-VGABE = 1.
          read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                                   binary search.
          if SY-SUBRC = 0.
            concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
              separated by ' - '.
          endif.
        else.
          read table T_RBKP with key BELNR = T_EKBE_1-BELNR
                                     GJAHR = T_EKBE_1-GJAHR
                                     binary search.
          if SY-SUBRC = 0.
            concatenate T_RBKP-NAME1 T_RBKP-LIFNR into TW_SAIDA-EMISSOR
            separated by ' - '.
          else.


            read table T_RBKP1 with key BELNR = T_EKBE_1-BELNR
                                        GJAHR = T_EKBE_1-GJAHR
                                        binary search.

            if SY-SUBRC = 0.
              concatenate T_RBKP1-NAME1 T_RBKP1-LIFNR into TW_SAIDA-EMISSOR
                                                      separated by ' - '.
            endif.


          endif.
        endif.

        TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).

        TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]

        read table T_161 with key BSART = T_EKKO-BSART
                                  binary search.
        if SY-SUBRC = 0.
          concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
          separated by ' - '.
        else.
          clear TW_SAIDA-TIPOPED.
        endif.

        if T_EKBE_1-VGABE = 3.
          TW_SAIDA-BELNR2 = 'MIRO-DP'.
          perform BUSCA_NFENUM using T_EKBE_1-BELNR
                                     T_EKBE_1-GJAHR
                               changing TW_SAIDA-NFENUM.

        endif.

        TW_SAIDA-WAERS = T_EKBE_1-WAERS.
        TW_SAIDA-WERKS = T_IMAK-WERKS.
        TW_SAIDA-KONNR = T_EKKO-KONNR.
        TW_SAIDA-KTPNR = T_EKKO-KTPNR.

        if T_EKBE_1-VGABE = 2 and
           T_EKKO-BSART = 'ZIM'.
*          CLEAR tw_saida-dmbtr.


          if P_SINCAX eq 'X'.
            select single VGABE from EKBE
              into T_EKBE_1-VGABE
              where EBELN = T_EKBE_1-EBELN and
                    EBELP = T_EKBE_1-EBELP       and
                    VGABE = 1.
            if SY-SUBRC = 0.
              clear TW_SAIDA-SGTXT.
            else.
              clear TW_SAIDA-DMBTR.
            endif.
          else.
            clear TW_SAIDA-DMBTR.
          endif.



        endif.

        if not P_SINCAX is initial.
          move T_EKBE_1-BUDAT to TW_SAIDA-BUDAT.
        endif.


*** 08.05.2011
        if T_EKKO-BSART = 'ZIM'.
          clear WL_HKONT.
          select single HKONT from BSIS
            into WL_HKONT
            where BUKRS = T_IMAKA-BUKRS   and
                  BELNR = TW_SAIDA-BELNR3 and
                  GJAHR = T_EKBE_1-GJAHR  and
                ( BSCHL ne '31' and
                  BSCHL ne '21'     ).

          if SY-SUBRC = 0.
            if WL_HKONT ne '0000132208'.
              TW_SAIDA-SGTXT = 'APAGAR'.
            endif.

          endif.
        endif.
*** 08.05.2011


        append TW_SAIDA.
*        COLLECT TW_SAIDA.
      endloop.
    endloop.
  endif.
endform.                    " Z_EKBE
*&---------------------------------------------------------------------*
*&      Form  VALIDA_RATEIO
*&---------------------------------------------------------------------*
form VALIDA_RATEIO .

  data: begin of T_EKKX occurs 0,
          EBELN    like EKKN-EBELN,
          EBELP    like EKKN-EBELP,
          AUFNR    like EKKN-AUFNR,
          ANLN1    like EKKN-ANLN1,
          ANLN2    like EKKN-ANLN2,
          VPROZ(6) type P decimals 2,
        end of T_EKKX.

  loop at T_EKKN.
    move-corresponding T_EKKN to T_EKKX.
    collect T_EKKX.
  endloop.

  loop at T_EKKO.
    read table T_EKKX with key EBELN = T_EKKO-EBELN
                               EBELP = T_EKKO-EBELP
                               AUFNR = T_EKKO-AUFNR
                               ANLN1 = T_EKKO-ANLN1
                               ANLN2 = T_EKKO-ANLN2.
    if SY-SUBRC = 0.
      if T_EKKX-VPROZ = 100.
        clear T_EKKO-VPROZ.
      else.
        T_EKKO-VPROZ = T_EKKX-VPROZ.
      endif.
      modify T_EKKO.
    endif.

  endloop.

endform.                    " VALIDA_RATEIO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_OFFLINE
*&---------------------------------------------------------------------*
form F_SELECIONA_DADOS_OFFLINE .

  data: WL_VISAO type ZIM08_REL_INV2-VISAO.

  ranges: RG_GJAHR for BSIS-GJAHR.

  case 'X'.
    when P_CONSOL.
      WL_VISAO = '01'.
    when P_ANALIT.
      WL_VISAO = '02'.
    when P_SINCAX .
      WL_VISAO = '03'.
  endcase.

  refresh: RG_GJAHR[].
  loop at S_BUDAT.
    move-corresponding: S_BUDAT to RG_GJAHR.

    append RG_GJAHR.
    clear: RG_GJAHR.
  endloop.

  if P_MOEDA = 'BRL'.
    select * from ZIM08_REL_INV2
      into table T_08
      where VISAO eq WL_VISAO and
            ABUKRS in S_BUKRS and
            WERKS  in S_WERKS and
            AKOSTL in S_KOSTL and
            POSNR  in S_POSNR and
            ANLN1  in S_ANLN1 and
            AUFNR  in S_AUFNR and
            GJAHR  in RG_GJAHR and
            EBELN  in S_EBELN." AND
*          aedat  IN s_budat." AND
*          usuario IN s_user AND
*          data_exec IN s_data AND
*          hora_exec IN s_hora.

  else.
    select * from ZIM08_REL_INV_US
     into table T_08
     where VISAO eq WL_VISAO and
           ABUKRS in S_BUKRS and
           WERKS  in S_WERKS and
           AKOSTL in S_KOSTL and
           POSNR  in S_POSNR and
           ANLN1  in S_ANLN1 and
           AUFNR  in S_AUFNR and
           GJAHR  in RG_GJAHR and
           EBELN  in S_EBELN..
  endif.

  data: begin of T_08_2 occurs 0.
          include structure ZIM08_REL_INV2.
  data: end of T_08_2.

  T_08_2[] = T_08[].
  delete T_08_2 where AEDAT not in S_BUDAT.

  sort T_08_2 by ABUKRS EBELN WERKS AUFNR AGSBER AKOSTL POSNR.

  loop at T_08.
    if T_08-AEDAT ne 00000000.
      if T_08-AEDAT not in S_BUDAT.

        if T_08-BELNR2 = 'PEDIDO'.
          read table T_08_2 with       key ABUKRS = T_08-ABUKRS
                                           EBELN  = T_08-EBELN
                                           WERKS  = T_08-WERKS
                                           AUFNR  = T_08-AUFNR
                                           AGSBER = T_08-AGSBER
                                           AKOSTL = T_08-AKOSTL
                                           POSNR  = T_08-POSNR
                                            binary search.
          if SY-SUBRC <> 0.
            delete T_08.
          endif.
        else.
          delete T_08.
        endif.
      endif.
    endif.
  endloop.

  sort T_08 by NUMERADOR .

  if not P_CONSOL is initial.
*    w_visao = 'Consolidado (Competência e Caixa)'.
    W_VISAO = 'Competência'.
  elseif not P_ANALIT is initial.
*    w_visao =   'Analitico/Sintético (Competência)'.
    W_VISAO = 'Consolidado (Competência e Caixa)'.
  elseif not P_SINCAX is initial.
    W_VISAO = 'Caixa'.
  endif.

  if not S_BUKRS[] is initial.
    select single BUTXT from T001 into W_NOME
      where BUKRS in S_BUKRS.
  else.
    read table T_08 index 1.
    select single BUTXT from T001 into W_NOME
      where BUKRS eq T_08-ABUKRS.
  endif.

  write: S_BUDAT-LOW  to W_BUDAT_LOW,
         S_BUDAT-HIGH to W_BUDAT_HIGH,
         S_CPUDT-LOW  to W_CPUDT_LOW,
         S_CPUDT-HIGH to W_CPUDT_HIGH.

*
*
*
*SELECT-OPTIONS: s_bukrs  FOR t001-bukrs      NO-EXTENSION NO INTERVALS,
*                s_werks  FOR imak-werks      ,"NO-EXTENSION NO INTERVALS,
*                s_kostl  FOR imakpa-akostl,
*                s_posnr  FOR imak-posnr,
*                s_anln1  FOR anla-anln1,
*                s_aufnr  FOR coas-aufnr     MATCHCODE OBJECT orde,
*                s_ebeln  FOR ekko-ebeln,
*                s_lifnr  FOR ekko-lifnr,
*                s_lifre  FOR ekko-lifre,
*                s_konnr  FOR ekab-konnr MATCHCODE OBJECT space ,
*                s_budat  FOR ekbe-budat NO-EXTENSION,
*                s_cpudt  FOR ekbe-cpudt                NO-EXTENSION,
**                s_gjahr  FOR imak-gjahr.
*                s_gjahr  FOR imak-gjahr NO-EXTENSION.
endform.                    " F_SELECIONA_DADOS_OFFLINE
*&---------------------------------------------------------------------*
*&      Form  Z_EKBE2
*&---------------------------------------------------------------------*

form Z_EKBE2 using    P_BELNR changing P_DATA..
*---> 05/07/2023 - Migração S4 - DL
SORT T_RBKP BY BELNR.
*<--- 05/07/2023 - Migração S4 - DL
  loop at T_EKBE_1 where EBELN = T_EKKN-EBELN and
                           EBELP = T_EKKN-EBELP and
                           BEKKN = T_EKKN-ZEKKN and
                           ( VGABE = 1 ).
*        IF NOT t_ekbe_1-anln1 IS INITIAL.
*          IF t_ekbe_1-anln1 NE t_imaka-anln1.
*            CONTINUE.
*          ENDIF.
*        ENDIF.

    if not T_EKBE_1-AUFNR is initial.
      if T_EKBE_1-AUFNR ne  T_IMAKZ-AUFNR.
        continue.
      endif.
    endif.

    if T_EKBE_1-DMBTR is initial.
      select single DMBTR from EKBE into T_EKBE_1-DMBTR
        where EBELN = T_EKBE_1-EBELN and
              EBELP = T_EKBE_1-EBELP and
              ZEKKN = T_EKBE_1-ZEKKN and
              VGABE = 2              and
              GJAHR = T_EKBE_1-GJAHR and
              LFBNR = T_EKBE_1-LFBNR.
    endif.

*** Se a moeda do doc for diferente de BRL e a selecao BRL, converter
    if P_MOEDA      eq 'BRL' and
       T_EKBE_1-WAERS ne 'BRL'.
      if not P_HIST is initial.
        P_DATA = T_EKBE_1-BUDAT.
      else.
        P_DATA = P_DATAC.
      endif.

    elseif P_MOEDA ne 'BRL' and
           T_EKBE-WAERS ne P_MOEDA.

      if not P_HIST is initial.
        P_DATA = T_EKBE_1-BUDAT.
      else.
        P_DATA = P_DATAC.
      endif.
      perform F_CONVERTE_COTACAO using T_EKBE_1-WAERS
                                       P_DATA
                              changing T_EKBE_1-DMBTR.
    elseif P_MOEDA ne 'BRL' and
         T_EKBE_1-WAERS eq P_MOEDA.
      T_EKBE_1-DMBTR = T_EKBE_1-WRBTR.
    endif.
*** Fim da rotina da TAXA


    move:
          T_EKBE_1-XBLNR to TW_SAIDA-NFENUM,
          T_EKBE_1-MENGE to TW_SAIDA-MENGE,
          T_EKBE_1-BUDAT to TW_SAIDA-AEDAT,
          T_EKBE_1-DMBTR to TW_SAIDA-NETPR.

    if T_EKBE_1-VGABE = 1.

      read table T_RBKP with key BELNR = T_EKBE_1-BELNR
                                 GJAHR = T_EKBE_1-GJAHR
                                 binary search.
      if SY-SUBRC = 0.
        TW_SAIDA-NFENUM = T_RBKP-XBLNR .
      endif.

      move: T_EKBE_1-BELNR to TW_SAIDA-BELNR1, "2
            T_EKBE_1-DMBTR to TW_SAIDA-DMBTR.
      clear: TW_SAIDA-BELNR2." tw_saida-dmbtr.
      if T_EKBE_1-VGABE = 2 and
         T_EKBE_1-SHKZG = 'H'.
        TW_SAIDA-BELNR2 = 'MIRO-NC'.
      else.
        TW_SAIDA-BELNR2 = 'MIRO'.
      endif.
    endif.

    read table WE_BKPF with key AWKEY = T_EKBE_1-AWKEY
                                binary search.
    if SY-SUBRC = 0.

      if T_EKBE_1-VGABE = 3.
        loop at WE_BKPF where AWKEY eq T_EKBE_1-AWKEY
                                     and BLART ne 'ML'.

          move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
          exit.
        endloop.
      else.
*            IF t_ekbe-vgabe = 1.
        move WE_BKPF-BELNR to TW_SAIDA-BELNR3.
      endif.
    else.
    endif.

    if T_EKBE_1-SHKZG = 'H'.
      TW_SAIDA-DMBTR = TW_SAIDA-DMBTR * ( - 1 ) .
      TW_SAIDA-MENGE = TW_SAIDA-MENGE * ( - 1 ) .
    endif.

    if T_EKBE_1-VGABE eq 1.
    endif.

*Início Alteração Ricardo Furst.
    if P_ALV is initial.
      clear:   TW_SAIDA-EXTROW, "tw_saida-werks,
             TW_SAIDA-TXT50, TW_SAIDA-TXTPED, "tw_saida-netpr,
             TW_SAIDA-NETWR, TW_SAIDA-WAERS, TW_SAIDA-TXTORD,
             TW_SAIDA-ANLKL," tw_saida-matnr, tw_saida-txz01, tw_saida-meins,
             TW_SAIDA-FORNECEDOR, TW_SAIDA-EMISSOR.
    endif.
*Fim Alteração Ricardo Furst.
* tw_saida-ktext1, , tw_saida-aufnr  tw_saida-txttipo, tw_saida-srvpos, , tw_saida-txtimob


    read table T_LFA1 with key LIFNR = T_EKKO-LIFNR
                               binary search.
    if SY-SUBRC = 0.
      concatenate T_LFA1-NAME1 T_EKKO-LIFNR into TW_SAIDA-FORNECEDOR
        separated by ' - '.
    endif.

    if T_EKBE_1-VGABE = 1.
      read table T_LFA1 with key LIFNR = T_EKKO-LIFRE
                               binary search.
      if SY-SUBRC = 0.
        concatenate T_LFA1-NAME1 T_EKKO-LIFRE into TW_SAIDA-EMISSOR
          separated by ' - '.
      endif.
    else.
      read table T_RBKP with key BELNR = T_EKBE_1-BELNR
                                 GJAHR = T_EKBE_1-GJAHR
                                 binary search.
      if SY-SUBRC = 0.
        concatenate T_RBKP-NAME1 T_RBKP-LIFNR into TW_SAIDA-EMISSOR
        separated by ' - '.
      endif.
    endif.

    TW_SAIDA-DIFERENCA = TW_SAIDA-DMBTR * ( - 1 ).

    TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR * ( - 1 ). "#EC CI_FLDEXT_OK[2610650]

    read table T_161 with key BSART = T_EKKO-BSART
                              binary search.
    if SY-SUBRC = 0.
      concatenate T_161-BSART T_161-BATXT into TW_SAIDA-TIPOPED
      separated by ' - '.
    else.
      clear TW_SAIDA-TIPOPED.
    endif.

    if T_EKBE_1-VGABE = 3.
      TW_SAIDA-BELNR2 = 'MIRO-DP'.
      perform BUSCA_NFENUM using T_EKBE_1-BELNR
                                 T_EKBE_1-GJAHR
                           changing TW_SAIDA-NFENUM.
    endif.

    TW_SAIDA-WAERS = T_EKBE_1-WAERS.
    TW_SAIDA-WERKS = T_IMAK-WERKS.
    append TW_SAIDA.
  endloop.

endform.                                                    " Z_EKBE2
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_TEXTO
*&---------------------------------------------------------------------*
form ORGANIZA_TEXTO .
*  tw_saida_aux[] = tw_saida[].
  types: begin of TYL_BKPF,
           BUKRS type BKPF-BUKRS,
           BELNR type BKPF-BELNR,
           GJAHR type BKPF-GJAHR,
           KURS2 type BKPF-KURS2,
         end of TYL_BKPF.

  data TL_BKPF type table of TYL_BKPF with header line.

  data: begin of TL_EKPO occurs 0,
          EBELN like EKPO-EBELN,
          EBELP like EKPO-EBELP,
          TXZ01 like EKPO-TXZ01,
        end of TL_EKPO.

  data: begin of TL_ANLA occurs 0,
          BUKRS like ANLA-BUKRS,
          ANLN1 like ANLA-ANLN1,
          ANLN2 like ANLA-ANLN2,
          TXT50 like ANLA-TXT50,
        end of TL_ANLA.

  data: begin of TL_COAS occurs 0,
          AUFNR like COAS-AUFNR,
          KTEXT like COAS-KTEXT,
        end of TL_COAS.


  refresh: TL_ANLA, TL_COAS, TL_EKPO.
  data: TL_SAIDA type E_SAIDA occurs 0 with header line.
  data: WL_TABIX       type C,
        WL_TABIX2      type SY-TABIX,
        WL_SAIDA       like line of TL_SAIDA,
        PS_SAIDA       like line of TL_SAIDA,
        TL_ZIMT001     type table of ZIMT001 with header line,
        TL_ZIMT001_AUX type table of ZIMT001 with header line,
        W_TABIX        type SY-TABIX,
        W_SAIDA        like line of TW_SAIDA.
  data: WL_EBELN type E_SAIDA-EBELN,
        WL_POSNR type E_SAIDA-POSNR.
*  tl_saida[] = tw_saida[].
*  DELETE tl_saida WHERE ebeln IS INITIAL.
*  IF NOT tl_saida IS INITIAL.
*    SELECT ebeln ebelp txz01
*      FROM ekpo INTO TABLE tl_ekpo
*      FOR ALL ENTRIES IN tl_saida
*      WHERE ebeln = tl_saida-ebeln AND
*            ebelp = tl_saida-ebelp.
*    SORT tl_ekpo BY ebeln ebelp.
*  ENDIF.

  TL_SAIDA[] = TW_SAIDA[].

  delete TL_SAIDA where ANLN1 is initial.
  if not TL_SAIDA[] is initial.
    select BUKRS ANLN1 ANLN2 TXT50
      from ANLA into table TL_ANLA
      for all entries in TL_SAIDA
      where BUKRS = TL_SAIDA-ABUKRS and
            ANLN1 = TL_SAIDA-ANLN1  and
            ANLN2 = TL_SAIDA-ANLN2.

    sort TL_ANLA by BUKRS ANLN1 ANLN2.
  endif.


  TL_SAIDA[] = TW_SAIDA[].
  delete TL_SAIDA where AUFNR is initial.
  if not TL_SAIDA[] is initial.
    select AUFNR KTEXT
      from COAS into table TL_COAS
      for all entries in TL_SAIDA
      where AUFNR = TL_SAIDA-AUFNR.

    sort TL_COAS by AUFNR.
  endif.

  loop at TW_SAIDA.

*    if tw_saida-belnr2(6) = 'PEDIDO'.
*      tw_saida-belnr1 = tw_saida-ebeln.
*    endif.

    if not TW_SAIDA-ANLN1 is initial.
      read table TL_ANLA with key BUKRS = TW_SAIDA-ABUKRS
                                  ANLN1 = TW_SAIDA-ANLN1
                                  ANLN2 = TW_SAIDA-ANLN2
                                  binary search.
      if SY-SUBRC = 0.
        TW_SAIDA-TXTIMOB = TL_ANLA-TXT50.
      endif.
    endif.

    if not TW_SAIDA-AUFNR is initial.
*      tw_saida-objeto =  tw_saida-aufnr.
      read table TL_COAS with key AUFNR = TW_SAIDA-AUFNR.
      if SY-SUBRC = 0.
        TW_SAIDA-TXTIMOB = TL_COAS-KTEXT.

      endif.
    endif.

*    IF NOT tw_saida-ebeln IS INITIAL.
*      READ TABLE tl_ekpo WITH KEY ebeln = tw_saida-ebeln
*                                  ebelp = tw_saida-ebelp
*                                  BINARY SEARCH.
*      IF sy-subrc = 0.
*        tw_saida-sgtxt = tl_ekpo-txz01.
*      ENDIF.
*    ENDIF.


    modify TW_SAIDA.
  endloop.
*  if p_sincax is initial.
*    delete tw_saida where aedat not in s_budat.
*  else.

*  endif.
  if P_OFF is initial.
*---> 04/07/2023 - Migração S4 - WS
  SORT TW_SAIDA.
*<--- 04/07/2023 - Migração S4 - WS
    delete adjacent duplicates from TW_SAIDA comparing all fields.
  endif.
  if P_SINCAX is initial.
    do.
      read table TW_SAIDA with key SGTXT = 'APAGAR'.
      if SY-SUBRC <> 0.
        exit.
      endif.
      clear TW_SAIDA-SGTXT.
      modify TW_SAIDA index SY-TABIX.
      WL_EBELN = TW_SAIDA-EBELN.
      WL_POSNR = TW_SAIDA-POSNR.
      loop at TW_SAIDA where EBELN = WL_EBELN and
                             POSNR = WL_POSNR and
                             ( BELNR2 <> 'PEDIDO' and
                               BELNR2 <> 'MIGO'       ).
        read table TW_SAIDA into W_SAIDA with key EBELN = WL_EBELN
                                                  POSNR = WL_POSNR
                                                  BELNR2 = 'PEDIDO'.
        if SY-SUBRC ne 0.
          clear TW_SAIDA-SGTXT.
        else.
          TW_SAIDA-SGTXT = W_SAIDA-SGTXT.
        endif.
        modify TW_SAIDA.
      endloop.
    enddo.

    loop at TW_SAIDA where TXT50 = 'Orçamento'.
      TW_SAIDA-AEDAT = S_BUDAT-LOW.
      modify TW_SAIDA.
    endloop.

    delete TW_SAIDA where not AEDAT in S_BUDAT and
                                ( BELNR2 ne 'PEDIDO' and
                                  BELNR2 ne SPACE        ).

    loop at TW_SAIDA where TXT50 = 'Orçamento'.
      clear: TW_SAIDA-AEDAT, TW_SAIDA-DMBTR.
      modify TW_SAIDA.
    endloop.

    select *
        from ZIMT001
        into table TL_ZIMT001
          where VISAO = '1'
            and ABUKRS in S_BUKRS
            and WERKS  in S_WERKS
            and BELNR4 in S_KOSTL
            and POSNR  in S_POSNR
            and ANLN1  in S_ANLN1
            and AUFNR  in S_AUFNR
            and EBELN  in S_EBELN
*        and aedat  in s_budat
            and ANULAR eq SPACE.

    delete TL_ZIMT001 where BELNR2 ne 'PEDIDO'
                        and STATUS ne 'E'
                        and AEDAT not in S_BUDAT.
*  endloop.

    TL_ZIMT001_AUX[] = TL_ZIMT001[].
    delete TL_ZIMT001_AUX where STATUS ne 'E'
                            and STATUS ne 'A'.

    loop at TL_ZIMT001_AUX.
      delete TW_SAIDA where POSNR   eq TL_ZIMT001_AUX-POSNR
                        and ABUKRS  eq TL_ZIMT001_AUX-ABUKRS
                        and EBELN   eq TL_ZIMT001_AUX-EBELN
                        and EBELP   eq TL_ZIMT001_AUX-EBELP
                        and AUFNR   eq TL_ZIMT001_AUX-AUFNR
                        and ANLN1   eq TL_ZIMT001_AUX-ANLN1
                        and ANLN2   eq TL_ZIMT001_AUX-ANLN2
                        and BELNR1  eq TL_ZIMT001_AUX-DOC_MM
                        and BELNR2(4) eq TL_ZIMT001_AUX-BELNR2(4).
    endloop.

    TL_ZIMT001_AUX[] = TL_ZIMT001[].
    delete TL_ZIMT001_AUX where STATUS ne 'I'
                            and STATUS ne 'A'.

    if TL_ZIMT001_AUX[] is not initial
    and P_MOEDA ne 'BRL'.
      select BUKRS BELNR GJAHR KURS2
        from BKPF
        into table TL_BKPF
         for all entries in TL_ZIMT001_AUX
         where BUKRS eq TL_ZIMT001_AUX-ABUKRS
           and BELNR eq TL_ZIMT001_AUX-BELNR6
           and GJAHR eq TL_ZIMT001_AUX-AEDAT(4).
    endif.

    data: WL_TABIX_AUX type SY-TABIX.
    clear: TW_SAIDA.
    sort: TL_BKPF by BUKRS BELNR GJAHR.
    loop at TL_ZIMT001_AUX.
      clear: W_TABIX.
      move-corresponding: TL_ZIMT001_AUX to TW_SAIDA.
      if P_MOEDA ne 'BRL'.
        if TL_ZIMT001_AUX-DMBE2 is not initial.
          if TW_SAIDA-DMBTR is not initial.
            move: TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-DMBTR.
          elseif TW_SAIDA-DIFERENCA is not initial.
            move: TL_ZIMT001_AUX-DIFERENCA to TW_SAIDA-DMBTR.
          endif.
        else.
          read table TL_BKPF
                with key BUKRS = TW_SAIDA-ABUKRS
                         BELNR = TW_SAIDA-BELNR6
                         GJAHR = TW_SAIDA-AEDAT(4)
                         binary search.

          if SY-SUBRC is initial.
            divide TW_SAIDA-DMBTR by TL_BKPF-KURS2.
            divide TW_SAIDA-DIFERENCA by TL_BKPF-KURS2.
            multiply TW_SAIDA-DMBTR by -1.
            multiply TW_SAIDA-DIFERENCA by -1.
          endif.
        endif.
      endif.
      move: TL_ZIMT001_AUX-DOC_MM to TW_SAIDA-BELNR1.
*    READ TABLE tw_saida TRANSPORTING NO FIELDS
*          WITH KEY abukrs  = tw_saida-abukrs
*                   posnr   = tw_saida-posnr
*                   txttipo = tw_saida-txttipo
*                   anln1   = tw_saida-anln1
*                   aufnr   = tw_saida-aufnr
*                   txt050  = tw_saida-txt050
*                   ebeln   = tw_saida-ebeln
*                   ebelp   = tw_saida-ebelp
*                   belnr2(4)  = tw_saida-belnr2(4).
*
*
*    IF sy-subrc IS NOT INITIAL.
      read table TW_SAIDA transporting no fields
            with key ABUKRS  = TW_SAIDA-ABUKRS
                     POSNR   = TW_SAIDA-POSNR
                     TXTTIPO = TW_SAIDA-TXTTIPO
                     ANLN1   = TW_SAIDA-ANLN1
                     AUFNR   = TW_SAIDA-AUFNR
                     TXT050  = TW_SAIDA-TXT050
                     EBELN   = TW_SAIDA-EBELN
                     EBELP   = TW_SAIDA-EBELP
                     BELNR2(4)  = TW_SAIDA-BELNR2(4).


      if SY-SUBRC is not initial.
        read table TW_SAIDA transporting no fields
          with key ABUKRS  = TW_SAIDA-ABUKRS
                   POSNR   = TW_SAIDA-POSNR
                   TXTTIPO = TW_SAIDA-TXTTIPO
                   ANLN1   = TW_SAIDA-ANLN1
                   AUFNR   = TW_SAIDA-AUFNR
                   TXT050  = TW_SAIDA-TXT050
                   EBELN   = TW_SAIDA-EBELN
                   EBELP   = TW_SAIDA-EBELP.
*                 belnr2(4)  = tw_saida-belnr2(4).


        if SY-SUBRC is not initial.
          read table TW_SAIDA transporting no fields
         with key ABUKRS  = TW_SAIDA-ABUKRS
                  POSNR   = TW_SAIDA-POSNR
                  TXTTIPO = TW_SAIDA-TXTTIPO
                  ANLN1   = TW_SAIDA-ANLN1
                  AUFNR   = TW_SAIDA-AUFNR
                  TXT050  = TW_SAIDA-TXT050
                  EBELN   = TW_SAIDA-EBELN.
*             ebelp   = tw_saida-ebelp.
          if SY-SUBRC is not initial.
            read table TW_SAIDA transporting no fields
              with key ABUKRS  = TW_SAIDA-ABUKRS
            POSNR   = TW_SAIDA-POSNR
            TXTTIPO = TW_SAIDA-TXTTIPO
            ANLN1   = TW_SAIDA-ANLN1
            AUFNR   = TW_SAIDA-AUFNR
            TXT050  = TW_SAIDA-TXT050.
*        ebeln   = tw_saida-ebeln.

            if SY-SUBRC is not initial.
              read table TW_SAIDA transporting no fields
               with key ABUKRS  = TW_SAIDA-ABUKRS
             POSNR   = TW_SAIDA-POSNR
             TXTTIPO = TW_SAIDA-TXTTIPO
             ANLN1   = TW_SAIDA-ANLN1
             AUFNR   = TW_SAIDA-AUFNR.
*         txt050  = tw_saida-txt050.
              if SY-SUBRC is not initial.
                read table TW_SAIDA transporting no fields
                 with key ABUKRS  = TW_SAIDA-ABUKRS
               POSNR   = TW_SAIDA-POSNR
               TXTTIPO = TW_SAIDA-TXTTIPO
               ANLN1   = TW_SAIDA-ANLN1.
*         aufnr   = tw_saida-aufnr.

                if SY-SUBRC is not initial.
                  read table TW_SAIDA transporting no fields
                 with key ABUKRS  = TW_SAIDA-ABUKRS
               POSNR   = TW_SAIDA-POSNR
               TXTTIPO = TW_SAIDA-TXTTIPO.
                  if SY-SUBRC is not initial.
                    read table TW_SAIDA transporting no fields
                   with key ABUKRS  = TW_SAIDA-ABUKRS
                            POSNR   = TW_SAIDA-POSNR.
                    if SY-SUBRC is not initial.
                      read table TW_SAIDA transporting no fields
                       with key ABUKRS  = TW_SAIDA-ABUKRS.
                      if SY-SUBRC is not initial.
                        append TW_SAIDA.
                        clear: TW_SAIDA.
                        continue.
                      endif.
                    else.
                      WL_TABIX_AUX = SY-TABIX + 1.
                    endif.
                  else.
                    WL_TABIX_AUX = SY-TABIX + 1.
                  endif.
                else.
                  WL_TABIX_AUX = SY-TABIX + 1.
                endif.
              else.
                WL_TABIX_AUX = SY-TABIX + 1.
              endif.
            else.
              WL_TABIX_AUX = SY-TABIX + 1.
            endif.
          else.
            WL_TABIX_AUX = SY-TABIX + 1.
          endif.
        else.
          WL_TABIX_AUX = SY-TABIX + 1.
        endif.
      else.
        WL_TABIX_AUX = SY-TABIX + 1.
      endif.

      if WL_TABIX_AUX is initial.
        add 1 to WL_TABIX_AUX.
      endif.

      insert TW_SAIDA index  WL_TABIX_AUX.
      clear: TW_SAIDA.
    endloop.

    loop at TW_SAIDA into W_SAIDA.
      WL_TABIX_AUX = SY-TABIX.

      read table TL_ZIMT001 transporting no fields
            with key ABUKRS  = W_SAIDA-ABUKRS
                     POSNR   = W_SAIDA-POSNR
                     TXTTIPO = W_SAIDA-TXTTIPO
                     AUFNR   = W_SAIDA-AUFNR
                     TXT050  = W_SAIDA-TXT050
                     EBELN   = W_SAIDA-EBELN
                     EBELP   = W_SAIDA-EBELP
                     BELNR2(4)  = W_SAIDA-BELNR2(4).


      if SY-SUBRC is not initial.
        if W_SAIDA-BELNR2 = 'MIGO'.
          clear W_SAIDA-DIFERENCA.
        endif.

        if W_SAIDA-BELNR2 = 'MIGO' and ( W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
          clear W_SAIDA-NETWR.
          W_SAIDA-DMBTR = W_SAIDA-NETPR.
          W_SAIDA-DIFERENCA = W_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
        elseif W_SAIDA-BELNR2 = 'MIGO'.
          clear: W_SAIDA-DMBTR, W_SAIDA-NETWR.
        endif.

      endif.

      read table TL_ZIMT001 transporting no fields
            with key ABUKRS  = W_SAIDA-ABUKRS
                     POSNR   = W_SAIDA-POSNR
                     TXTTIPO = W_SAIDA-TXTTIPO
                     AUFNR   = W_SAIDA-AUFNR
                     TXT050  = W_SAIDA-TXT050
                     EBELN   = W_SAIDA-EBELN
                     EBELP   = W_SAIDA-EBELP
                     BELNR2(4)  = W_SAIDA-BELNR2(4).

      if SY-SUBRC is not initial.
        if W_SAIDA-BELNR2 = 'MIGO-EST'.

          clear: W_SAIDA-DIFERENCA, W_SAIDA-NETWR, W_SAIDA-DMBTR.
        endif.
      endif.

      modify TW_SAIDA from W_SAIDA index WL_TABIX_AUX.
    endloop.
  else.
    if TW_SAIDA[] is not initial.
      select *
          from ZIMT001
          into table TL_ZIMT001_AUX
        for all entries in TW_SAIDA
            where VISAO = '3'
              and ABUKRS eq TW_SAIDA-ABUKRS
*              and werks  eq tw_saida-werks
*              and belnr4 eq tw_saida-akostl
              and POSNR  eq TW_SAIDA-POSNR
              and ANLN1  eq TW_SAIDA-ANLN1
              and AUFNR  eq TW_SAIDA-AUFNR
              and EBELN  eq TW_SAIDA-EBELN
              and EBELP  eq TW_SAIDA-EBELP
*              and aedat  eq tw_saida-budat
              and ANULAR eq SPACE
              and ( STATUS eq 'E'
                 or STATUS eq 'A' ).


      loop at TL_ZIMT001_AUX.
        delete TW_SAIDA where POSNR   eq TL_ZIMT001_AUX-POSNR
                          and ABUKRS  eq TL_ZIMT001_AUX-ABUKRS
                          and EBELN   eq TL_ZIMT001_AUX-EBELN
                          and EBELP   eq TL_ZIMT001_AUX-EBELP
                          and AUFNR   eq TL_ZIMT001_AUX-AUFNR
                          and ANLN1   eq TL_ZIMT001_AUX-ANLN1
                          and ANLN2   eq TL_ZIMT001_AUX-ANLN2
*                      and belnr1  eq tl_zimt001_aux-doc_mm
                          and BELNR2(4) eq TL_ZIMT001_AUX-BELNR2(4)
                          and BELNR3    eq TL_ZIMT001_AUX-BELNR3
                          and BELNR6    eq TL_ZIMT001_AUX-BELNR6.
      endloop.
    endif.

    delete TL_ZIMT001_AUX where STATUS ne 'A'.
    select *
        from ZIMT001
        appending table TL_ZIMT001_AUX
          where VISAO = '3'
            and ABUKRS in S_BUKRS
            and WERKS  in S_WERKS
            and BELNR4 in S_KOSTL
            and POSNR  in S_POSNR
            and ANLN1  in S_ANLN1
            and AUFNR  in S_AUFNR
            and EBELN  in S_EBELN
*            and aedat  in s_budat
            and ANULAR eq SPACE
            and STATUS eq 'I'.
*    tl_zimt001_aux[] = tl_zimt001[].
    delete TL_ZIMT001_AUX where AEDAT lt S_BUDAT-LOW.

    if TL_ZIMT001_AUX[] is not initial
      and P_MOEDA ne 'BRL'.
      select BUKRS BELNR GJAHR KURS2
        from BKPF
        into table TL_BKPF
         for all entries in TL_ZIMT001_AUX
         where BUKRS eq TL_ZIMT001_AUX-ABUKRS
           and BELNR eq TL_ZIMT001_AUX-BELNR6
           and GJAHR eq TL_ZIMT001_AUX-AEDAT(4).
    endif.

    sort: TL_BKPF by BUKRS BELNR GJAHR.
    loop at TL_ZIMT001_AUX.
      move-corresponding: TL_ZIMT001_AUX to TW_SAIDA.
      if TL_ZIMT001_AUX-AEDAT > S_BUDAT-HIGH.
        if TW_SAIDA-BELNR2 eq 'FIPA'.
          move TL_ZIMT001_AUX-TOTPG to TW_SAIDA-TOTAPG.
        endif.
        clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
      endif.

      if P_MOEDA ne 'BRL'.
        if TL_ZIMT001_AUX-DMBE2 is not initial.
          if TW_SAIDA-BELNR2 eq 'FIPA'.
            if TL_ZIMT001_AUX-AEDAT > S_BUDAT-HIGH.
              move TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-TOTAPG.
              clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
            else.

              move: TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-TOTPG.
            endif.
          elseif TW_SAIDA-BELNR2 eq 'FIAP'.
            move: TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-TOTAPG.
          endif.
        else.
          read table TL_BKPF
            with key BUKRS = TW_SAIDA-ABUKRS
                     BELNR = TW_SAIDA-BELNR6
                     GJAHR = TW_SAIDA-AEDAT(4)
                     binary search.

          if SY-SUBRC is initial.
            if TW_SAIDA-BELNR2 eq 'FIPA'.
              divide TW_SAIDA-TOTPG by TL_BKPF-KURS2.
              multiply TW_SAIDA-TOTPG by -1.
            elseif TW_SAIDA-BELNR2 eq 'FIAP'.
              divide TW_SAIDA-TOTAPG by TL_BKPF-KURS2.
              multiply TW_SAIDA-TOTAPG by -1.
            endif.
          endif.
        endif.
      endif.

      move: TL_ZIMT001_AUX-DOC_MM to TW_SAIDA-BELNR1,
            TL_ZIMT001_AUX-N0     to TW_SAIDA-N0,
            TL_ZIMT001_AUX-N1     to TW_SAIDA-N1,
            TL_ZIMT001_AUX-N2     to TW_SAIDA-N2,
            TL_ZIMT001_AUX-N3     to TW_SAIDA-N3,
            TL_ZIMT001_AUX-N4     to TW_SAIDA-N4.

      append TW_SAIDA.
      clear: TW_SAIDA.
    endloop.
  endif.
endform.                    " ORGANIZA_TEXTO
*&---------------------------------------------------------------------*
*&      Form  BUSCA_NFENUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TW_SAIDA_NFENUM  text
*---------------------- ------------------------------------------------*
form BUSCA_NFENUM using    P_BELNR
                           P_GJAHR
                  changing CH_NFENUM.

  read table T_RBKP with key BELNR = P_BELNR
                             GJAHR = P_GJAHR.
  if SY-SUBRC = 0.
    CH_NFENUM = T_RBKP-XBLNR.

  else.
    read table T_RBKP_1 with key BELNR = P_BELNR
                              GJAHR = P_GJAHR.
    if SY-SUBRC = 0.
      CH_NFENUM = T_RBKP_1-XBLNR.
    endif.

  endif.

endform.                    " BUSCA_NFENUM
*&---------------------------------------------------------------------*
*&      Form  Z_AJUSTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form Z_AJUSTA_SAIDA .

  data: W_SAIDA type E_SAIDA.

  W_SAIDA = TW_SAIDA.
  if W_SAIDA-BELNR2(6) = 'MIRO-0'.
    W_SAIDA-BELNR2 = 'MIRO'.
    if W_SAIDA-NETWR is initial and
       W_SAIDA-SGTXT ne 'ZIM' .
      W_SAIDA-DMBTR = W_SAIDA-NETPR .
    endif.
  endif.


  if W_SAIDA-BELNR2 = 'MIRO-NC'.
    if W_SAIDA-NETWR is initial and
       W_SAIDA-SGTXT ne 'ZIM' .
      W_SAIDA-DMBTR = W_SAIDA-NETPR * ( - 1 ).
    endif.
  endif.


  if W_SAIDA-BELNR2 is initial.
    clear: W_SAIDA-EBELN, W_SAIDA-EBELP.
  endif.

  if W_SAIDA-BELNR2 = 'PEDIDO'.
    clear: W_SAIDA-BELNR3, W_SAIDA-DMBTR, W_SAIDA-NFENUM.
  endif.

  if W_SAIDA-BELNR2 = 'MIRO'.
    clear W_SAIDA-DIFERENCA.
  endif.

  W_SAIDA-DIFERENCA = W_SAIDA-NETWR - W_SAIDA-DMBTR.

  if W_SAIDA-BELNR2 = 'MIGO-DEV'.
    clear: W_SAIDA-NETWR, W_SAIDA-DIFERENCA, W_SAIDA-DMBTR.
  endif.

  if W_SAIDA-BELNR2 = 'MIGO'.
    clear W_SAIDA-DIFERENCA.
  endif.

  if W_SAIDA-BELNR2 = 'MIGO' and ( W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF'  ).
    clear W_SAIDA-NETWR.
    W_SAIDA-DMBTR = W_SAIDA-NETPR.
    W_SAIDA-DIFERENCA = W_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
  elseif W_SAIDA-BELNR2 = 'MIGO'.
    clear: W_SAIDA-DMBTR, W_SAIDA-NETWR.
  endif.



  if W_SAIDA-BELNR2 = 'MIGO-EST'.
    clear: W_SAIDA-DIFERENCA, W_SAIDA-NETWR, W_SAIDA-DMBTR.
  endif.

  if W_SAIDA-BELNR2 = 'FIPA' or W_SAIDA-BELNR2 = 'FIAP'.
    clear: W_SAIDA-DMBTR.
  endif.

  W_SAIDA-ZWAERS = W_SAIDA-WAERS.

  if W_SAIDA-BELNR2 = 'MIRO' and ( W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
    clear: W_SAIDA-DMBTR, W_SAIDA-DIFERENCA.
  endif.
  TW_SAIDA = W_SAIDA.
endform.                    " Z_AJUSTA_SAIDA
*&---------------------------------------------------------------------*
*&      Form  AJUSTES_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form AJUSTES_ALV .


  if TW_SAIDA-BELNR2 ne 'PEDIDO'.
    clear TW_SAIDA-NETWR.
  endif.

** Eduardo - Calculo
  TW_SAIDA-REAL_AP = TW_SAIDA-DMBTR - TW_SAIDA-APROVADO. "#EC CI_FLDEXT_OK[2610650]
** Eduardo - Calculo

  if TW_SAIDA-BELNR2(6) = 'MIRO-0'.
    TW_SAIDA-BELNR2 = 'MIRO'.
    if TW_SAIDA-NETWR is initial and
       TW_SAIDA-SGTXT ne 'ZIM' .
      TW_SAIDA-DMBTR = TW_SAIDA-NETPR .
    endif.
  endif.


*  if tw_saida-belnr2 = 'MIRO-NC'.
*    if tw_saida-netwr is initial and
*       tw_saida-sgtxt ne 'ZIM' .
*      tw_saida-dmbtr = tw_saida-netpr * ( - 1 ).
*    endif.
*  endif.


  if TW_SAIDA-BELNR2 is initial.
    clear: TW_SAIDA-EBELN, TW_SAIDA-EBELP.
  endif.

  if TW_SAIDA-BELNR2 = 'PEDIDO'.
    clear: TW_SAIDA-BELNR3, TW_SAIDA-DMBTR, TW_SAIDA-NFENUM.
  endif.

  if TW_SAIDA-BELNR2 = 'MIRO'.
    clear TW_SAIDA-DIFERENCA.
  endif.

  TW_SAIDA-DIFERENCA = TW_SAIDA-NETWR - TW_SAIDA-DMBTR.

  if TW_SAIDA-BELNR2 = 'MIGO-DEV'.
    clear: TW_SAIDA-NETWR, TW_SAIDA-DIFERENCA, TW_SAIDA-DMBTR.
  endif.

  if TW_SAIDA-BELNR2 = 'MIGO'.
    clear TW_SAIDA-DIFERENCA.
  endif.

  if TW_SAIDA-BELNR2 = 'MIGO' and ( TW_SAIDA-SGTXT(3) eq 'ZIM' or TW_SAIDA-SGTXT(3) eq 'ZPI' or TW_SAIDA-SGTXT(3) eq 'ZEF' ).
    clear TW_SAIDA-NETWR.
    TW_SAIDA-DMBTR = TW_SAIDA-NETPR.
    TW_SAIDA-DIFERENCA = TW_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
  elseif TW_SAIDA-BELNR2 = 'MIGO'.
    clear: TW_SAIDA-DMBTR, TW_SAIDA-NETWR.
  endif.



  if TW_SAIDA-BELNR2 = 'MIGO-EST'.
    clear: TW_SAIDA-DIFERENCA, TW_SAIDA-NETWR, TW_SAIDA-DMBTR.
  endif.

  if TW_SAIDA-BELNR2 = 'FIPA' or TW_SAIDA-BELNR2 = 'FIAP'.
    clear: TW_SAIDA-DMBTR.
  endif.

  TW_SAIDA-ZWAERS = TW_SAIDA-WAERS.

  if TW_SAIDA-BELNR2 = 'MIRO' and ( TW_SAIDA-SGTXT(3) eq 'ZPI' or TW_SAIDA-SGTXT(3) eq 'ZEF').
    clear: TW_SAIDA-DMBTR, TW_SAIDA-DIFERENCA.
  endif.



  if TW_SAIDA-BELNR2(4) = 'MIRO' and ( TW_SAIDA-SGTXT(3) eq 'ZPI' or TW_SAIDA-SGTXT(3) eq 'ZEF' ).
    clear: TW_SAIDA-DMBTR, TW_SAIDA-DIFERENCA.
  endif.

endform.                    " AJUSTES_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CREATE_HIERARCHY_3 .

  clear T_08.
  refresh T_08.

  data: WL_CRIA_FI,
        WL_CH type LVC_NKEY.

  clear: WL_CRIA_FI, WL_CH.

  clear: TW_SAIDA. refresh TW_SAIDA.

  data: WL_EBELN       type E_SAIDA-EBELN,
        WL_POSNR       type E_SAIDA-POSNR,
        X_PASTA_CRIADA,
        TW_SAIDA_AUX   like table of TW_SAIDA.

  data: begin of WE_SAIDA occurs 0.
          include structure ZES_IM01.
  data:   COLOR type   KKBLO_SPECIALCOL occurs 0.
  data: end of WE_SAIDA.


  data: "w_saida TYPE zes_im01,
    W_SAIDA       type E_SAIDA,
    W_TABIX       type SY-TABIX,
    W_TIPO(1),
    W_MUDA(1),
    W_PST(1),
    W_FI(1),
    W_PED         type LVC_NKEY,
    W_EBELN       type EKKO-EBELN,
* add data to tree
    L_POSNR_KEY   type LVC_NKEY,
    L_BUKRS_KEY   type LVC_NKEY,
    L_TXTTIPO_KEY type LVC_NKEY,
    L_ANLN1_KEY   type LVC_NKEY,
    L_EBELN_KEY   type LVC_NKEY,
    L_PEDIDO_KEY  type LVC_NKEY,
    L_LAST_KEY    type LVC_NKEY.


  if not P_ON is initial.
    perform F_PREPARA_DADOS.
    delete TW_SAIDA where AEDAT not in S_BUDAT.
    do.
      read table TW_SAIDA with key SGTXT = 'APAGAR'.
      if SY-SUBRC <> 0.
        exit.
      endif.
      clear TW_SAIDA-SGTXT.
      modify TW_SAIDA index SY-TABIX.
      WL_EBELN = TW_SAIDA-EBELN.
      WL_POSNR = TW_SAIDA-POSNR.
      loop at TW_SAIDA where EBELN = WL_EBELN and
                             POSNR = WL_POSNR and
                             ( BELNR2 <> 'PEDIDO' and
                               BELNR2 <> 'MIGO'       ).
        read table TW_SAIDA into W_SAIDA with key EBELN = WL_EBELN
                                                  POSNR = WL_POSNR
                                                  BELNR2 = 'PEDIDO'.
        if SY-SUBRC ne 0.
          clear TW_SAIDA-SGTXT.
        else.
          TW_SAIDA-SGTXT = W_SAIDA-SGTXT.
        endif.
        modify TW_SAIDA.
      endloop.
    enddo.


  else.
    perform F_SELECIONA_DADOS_OFFLINE.
    loop at T_08.
      move-corresponding T_08 to TW_SAIDA.
      append TW_SAIDA.
    endloop.

    perform ORGANIZA_TEXTO.

  endif.
  if not P_SINCAX is initial.
    delete TW_SAIDA where BELNR2 = 'MIGO'.
  endif.



  data: W_POSNR     like TW_SAIDA-POSNR,
        W_TXTTIPO   like TW_SAIDA-TXTTIPO,
        W_ANLN1(20),"   LIKE tw_saida-anln1,
        W_ANLN0(20),
        W_EB(1).

  clear: W_REALIZADO, W_PEDIDO, W_POSNR, X_PASTA_CRIADA.

  loop at TW_SAIDA where TXT50 = 'Orçamento'.
    TW_SAIDA-AEDAT = S_BUDAT-LOW.
    modify TW_SAIDA.
  endloop.


  if P_SINCAX is initial.
    delete TW_SAIDA where not AEDAT in S_BUDAT and
                            ( BELNR2 ne 'PEDIDO' and
                              BELNR2 ne SPACE        ).
  else.
    delete TW_SAIDA where not BUDAT in S_BUDAT and
                            ( BELNR2 ne 'PEDIDO' and
                              BELNR2 ne SPACE    and
                              BELNR2 ne 'FIPA'   and
                              BELNR2 ne 'FIAP'   ) ."AND
*                              belnr2(4) NE 'MIRO'     ).
  endif.
  loop at TW_SAIDA where TXT50 = 'Orçamento'.
    clear: TW_SAIDA-AEDAT, TW_SAIDA-DMBTR.
    modify TW_SAIDA.
  endloop.

  if P_CONSOL eq 'X'.
*    DELETE ADJACENT DUPLICATES FROM tw_saida COMPARING ALL FIELDS.
  endif.


  if P_OFF is initial.

*---> 04/07/2023 - Migração S4 - WS
  SORT TW_SAIDA.
*<--- 04/07/2023 - Migração S4 - WS
    delete adjacent duplicates from TW_SAIDA comparing all fields.
  endif.


  data: WL_TABIX type C.
  TW_SAIDA_AUX[] = TW_SAIDA[].
  delete TW_SAIDA_AUX where BELNR2(2) eq 'FI'.
  loop at TW_SAIDA_AUX into W_SAIDA.

** Eduardo - Calculo
    W_SAIDA-REAL_AP = W_SAIDA-DMBTR - W_SAIDA-APROVADO. "#EC CI_FLDEXT_OK[2610650]
** Eduardo - Calculo

    if W_SAIDA-BELNR2(6) = 'MIRO-0'.
      W_SAIDA-BELNR2 = 'MIRO'.
      if W_SAIDA-NETWR is initial and
         W_SAIDA-SGTXT ne 'ZIM' .
        W_SAIDA-DMBTR = W_SAIDA-NETPR .
      endif.
    endif.


    if W_SAIDA-BELNR2 = 'MIRO-NC'.
      if W_SAIDA-NETWR is initial and
         W_SAIDA-SGTXT ne 'ZIM' .
        W_SAIDA-DMBTR = W_SAIDA-NETPR * ( - 1 ).
      endif.
    endif.


    if W_SAIDA-BELNR2 is initial.
      clear: W_SAIDA-EBELN, W_SAIDA-EBELP.
    endif.

    if W_SAIDA-BELNR2 = 'PEDIDO'.
      clear: W_SAIDA-BELNR3, W_SAIDA-DMBTR, W_SAIDA-NFENUM.
    endif.

    if W_SAIDA-BELNR2 = 'MIRO'.
      clear W_SAIDA-DIFERENCA.
    endif.

    W_SAIDA-DIFERENCA = W_SAIDA-NETWR - W_SAIDA-DMBTR.

    if W_SAIDA-BELNR2 = 'MIGO-DEV'.
      clear: W_SAIDA-NETWR, W_SAIDA-DIFERENCA, W_SAIDA-DMBTR.
    endif.

    if W_SAIDA-BELNR2 = 'MIGO'.
      clear W_SAIDA-DIFERENCA.
    endif.

    if W_SAIDA-BELNR2 = 'MIGO' and ( W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
      clear W_SAIDA-NETWR.
      W_SAIDA-DMBTR = W_SAIDA-NETPR.
      W_SAIDA-DIFERENCA = W_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
    elseif W_SAIDA-BELNR2 = 'MIGO'.
      clear: W_SAIDA-DMBTR, W_SAIDA-NETWR.
    endif.



    if W_SAIDA-BELNR2 = 'MIGO-EST'.
      clear: W_SAIDA-DIFERENCA, W_SAIDA-NETWR, W_SAIDA-DMBTR.
    endif.

    if W_SAIDA-BELNR2 = 'FIPA' or W_SAIDA-BELNR2 = 'FIAP'.
      clear: W_SAIDA-DMBTR.
    endif.

    W_SAIDA-ZWAERS = W_SAIDA-WAERS.

    if W_SAIDA-BELNR2 = 'MIRO' and ( W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ) .
      clear: W_SAIDA-DMBTR, W_SAIDA-DIFERENCA.
    endif.
    if SY-TCODE eq 'ZIM08' or SY-BATCH eq 'X'.
      T_08-NUMERADOR = SY-TABIX.
      move-corresponding W_SAIDA to T_08.
      if W_SAIDA-AEDAT is initial.
        T_08-GJAHR = W_SAIDA-BUDAT.
      else.
        T_08-GJAHR = W_SAIDA-AEDAT.
      endif.
      move: SY-UNAME to  T_08-USUARIO,
            SY-DATUM to T_08-DATA_EXEC,
            WL_HORA  to T_08-HORA_EXEC.
      case 'X'.

        when P_CONSOL.
          T_08-VISAO = '01'.
        when P_ANALIT.
          T_08-VISAO = '02'.
        when P_SINCAX .
          T_08-VISAO = '03'.
      endcase.

*      APPEND t_08.
      collect T_08.
      clear T_08.
      continue.
    endif.


    if W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI'.
      clear W_SAIDA-SGTXT.
    endif.
*    MOVE-CORRESPONDING we_saida to w_saida.
    clear W_MUDA.
    if SY-UCOMM = 'GROUP'.
      W_TABIX = SY-TABIX.
      W_PASTA = 1.
    else.
      W_TABIX = 0.
    endif.
    add W_SAIDA-NETWR to W_PEDIDO.
    add W_SAIDA-DMBTR to W_REALIZADO . "#EC CI_FLDEXT_OK[2610650]

    if not W_SAIDA-AUFNR is initial.
      W_PASTA = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
    else.
      concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_PASTA.
      concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN0.
*      w_pasta = w_saida-anln1.
    endif.

    if W_SAIDA-BELNR2 = 'MB1A' or
       W_SAIDA-BELNR2 = 'MBST'.
      W_PASTA = W_SAIDA-EBELN + 1.
    endif.
    if not W_SAIDA-BELNR3 is initial and
           W_SAIDA-BELNR1 is initial.
      W_PASTA = W_SAIDA-EBELN + 1.
    endif.


*    IF w_saida-belnr2 = 'MIGO' OR w_saida-belnr1 = 'MIRO'.
*      w_pasta = w_saida-ebeln + 1.
*    ENDIF.

*    IF NOT w_saida-belnr3 IS INITIAL AND
*           w_saida-belnr2 NE 'MIRO'  .
*      IF w_saida-belnr2(2) = 'FI'.
*      ELSEIF w_saida-belnr2(2) = 'CI'.
*      ELSE.
*        CLEAR w_saida-ebeln.
*      ENDIF.
*    ENDIF.

    clear WL_TABIX.
    if SY-TABIX = 1.
      WL_TABIX = 'X'.
      perform ADD_BUKRS_LINE using    W_SAIDA
                                       ''
                              changing L_BUKRS_KEY.
    endif.

    if W_POSNR is initial.
*      w_tipo = 'X'.
      W_POSNR = W_SAIDA-POSNR.
      clear W_TXTTIPO.
      perform ADD_POSNR_LINE using    W_SAIDA
                                       L_BUKRS_KEY
                              changing L_POSNR_KEY.

      if W_SAIDA-TXTTIPO(5) eq 'OBRAS' or
         W_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( W_SAIDA-TXTTIPO(5) eq 'IMOBI' and not W_SAIDA-AUFNR is initial ) .
        W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
        W_ANLN0 = W_SAIDA-AUFNR.
        W_SAIDA-AUFNR = W_SAIDA-AUFNR.
      endif.

    elseif W_POSNR ne W_SAIDA-POSNR.
*      w_tipo = 'X'.
      W_POSNR = W_SAIDA-POSNR.
      clear: W_TXTTIPO, W_ANLN1.
      perform ADD_POSNR_LINE using    W_SAIDA
                                     L_BUKRS_KEY
                            changing L_POSNR_KEY.
    endif.

    if W_SAIDA-TXTTIPO(5) eq 'OBRAS' or
       W_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( W_SAIDA-TXTTIPO(5) eq 'IMOBI' and not W_SAIDA-AUFNR is initial ) ..
      W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
      W_SAIDA-AUFNR = W_SAIDA-AUFNR.
    endif.

    if W_TXTTIPO is initial.
      W_TXTTIPO = W_SAIDA-TXTTIPO.
      clear: W_ANLN1.
*      PERFORM add_txttipo_line USING   w_saida
*                                       l_posnr_key
*                              CHANGING l_txttipo_key.
    elseif W_TXTTIPO ne W_SAIDA-TXTTIPO.
      W_TXTTIPO = W_SAIDA-TXTTIPO.
      clear W_TIPO.
*      PERFORM add_txttipo_line USING   w_saida
*                                       l_posnr_key
*                              CHANGING l_txttipo_key.
    endif.

    if not W_TIPO is initial.
      clear W_TIPO.
*      PERFORM add_txttipo_line USING   w_saida
*                                       l_posnr_key
*                              CHANGING l_txttipo_key.
    endif.

    if  W_SAIDA-ANLN1 is initial.
      W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
    endif.

    if W_SAIDA-ANLN1 = 'PLANEJADO' or
      W_SAIDA-ANLN1 = 'EXTRA'.

      on change of W_SAIDA-TXT50 or W_SAIDA-POSNR.
        perform ADD_ANLN1_LINE using     W_SAIDA
                                         L_POSNR_KEY
                                changing L_ANLN1_KEY.
      endon.

      perform ADD_COMPLETE_LINE using  W_SAIDA
                                       L_ANLN1_KEY
                              changing L_LAST_KEY.

      continue.
    endif.
    if W_ANLN1 is initial.
*      w_anln1 = w_saida-anln1.
      if W_SAIDA-AUFNR is initial.
        concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN1.
      else.
        W_ANLN1 = W_SAIDA-ANLN1.
      endif.
      W_FI = 'X'.
      W_PST = 'X'.
      WL_CRIA_FI = 'X'.
      if W_SAIDA-BELNR2 = 'PEDIDO'.
        WL_TABIX = 'X'.
      endif.
*      PERFORM add_anln1_line USING     w_saida
*                                       l_posnr_key
*                              CHANGING l_anln1_key.
    elseif W_ANLN1 ne W_ANLN0. "w_saida-anln1.
*      w_anln1 = w_saida-anln1.
      if W_SAIDA-AUFNR is initial.
        concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN1.
      else.
        W_ANLN1 = W_SAIDA-ANLN1.
      endif.
      W_PST = 'X'.
      WL_CRIA_FI = 'X'.
      if W_SAIDA-BELNR2 = 'PEDIDO'.
        WL_TABIX = 'X'.
      endif.
*      PERFORM add_anln1_line USING     w_saida
*                                       l_posnr_key
*                              CHANGING l_anln1_key.
      W_FI = 'X'.
    else.
      clear W_PST.
      if W_SAIDA-BELNR2 <> 'MB1A'.
        clear W_FI.
      endif.
    endif.

*    IF w_tabix = 1.
*      PERFORM add_ebeln_line USING     w_saida
*                                     l_anln1_key
*                            CHANGING l_ebeln_key.
*    ELSE.
    if W_SAIDA-BELNR2(2) = 'FI'.
*       w_saida-belnr2(2) = 'CI' or.
      W_PASTA = W_SAIDA-BELNR2(2).
    endif.
    on change of W_PASTA.
      if W_SAIDA-BELNR2(2) = 'FI' and
         P_SINCAX eq 'X'.
        clear W_EB.
      else.
        W_EB = 'X'.
      endif.
      clear W_PST.
*      if w_saida-belnr2 <> 'MB1A'.
      if W_SAIDA-BELNR2 eq 'MIGO' or
         W_SAIDA-BELNR2 eq 'PEDIDO' and
        X_PASTA_CRIADA is initial.
        perform ADD_EBELN_LINE using     W_SAIDA
                                         L_POSNR_KEY
                                changing L_EBELN_KEY.

        X_PASTA_CRIADA = 'X'.
      endif.
    endon.
    if W_TABIX = 1 and W_EB is initial.
      W_EB = 'X'.
      perform ADD_EBELN_LINE using     W_SAIDA
                                       L_POSNR_KEY
                              changing L_EBELN_KEY.
    elseif not W_PST is initial.
      clear W_PST.
      W_EB = 'X'.
      perform ADD_EBELN_LINE using     W_SAIDA
                                       L_POSNR_KEY
                              changing L_EBELN_KEY.

    endif.

    clear W_FI.
    on change of W_SAIDA-BELNR2.
      W_FI = 'X'.
    endon.


    if not W_SAIDA-BELNR3 is initial and
           W_SAIDA-BELNR1 is initial.

      W_EBELN = W_SAIDA-EBELN.
      clear W_SAIDA-EBELN.

*      IF NOT w_eb = 'X'.
      W_PED = L_PEDIDO_KEY.
*      ELSE.
*        w_ped = l_pedido_key.
*      ENDIF.

      L_PEDIDO_KEY = L_EBELN_KEY.
    else.
      on change of W_SAIDA-EBELN.
        clear WL_TABIX.
        if W_SAIDA-BELNR2 ne 'MB1A'" AND p_sincax NE 'X'.
        and W_SAIDA-BELNR2 ne 'FIAP'
        and W_SAIDA-BELNR2 ne 'FIPA'.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        else.
          L_PEDIDO_KEY = L_EBELN_KEY.
        endif.
        clear W_EB.
      endon.

** Eduado 10.05.2011
      if not WL_TABIX is initial ."AND p_sincax EQ 'X'.
        if W_SAIDA-BELNR2 ne 'MB1A' ."AND p_sincax NE 'X'.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        else.
          L_PEDIDO_KEY = L_EBELN_KEY.
        endif.
        clear W_EB.
      endif.
** Eduado 10.05.2011

      if P_SINCAX ne 'X'.
        if not W_EB is initial.
* eduardo 29.03.2011
*        and p_sincax ne 'X'        .
* eduardo 29.03.2011
          clear W_EB.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        endif.
      endif.
    endif.

*** Eduardo 05.04.2011 - CAIXA
*** Se for CAIXA, cria pasta para PAGOS e A PAGAR
    if not P_SINCAX is initial and
       W_SAIDA-BELNR2(2) = 'FI'.
      W_MUDA = 'X'.

      if not WL_CRIA_FI is initial.
*      ON CHANGE OF w_saida-belnr2 or w_anln1.

        clear WL_CRIA_FI.

*
*      IF w_fi = 'X'.
        clear W_MUDA.
*        PERFORM add_pedido_line USING     w_saida
*                                         l_ebeln_key
*                                CHANGING l_pedido_key.
*      ENDIF.
*      ENDON.
        WL_CH = L_PEDIDO_KEY.

      endif.

    endif.
*** Eduardo 05.04.2011 - CAIXA

    if W_MUDA eq 'X'.
      clear W_MUDA.
*      l_pedido_key = w_ped.
      L_PEDIDO_KEY =  WL_CH.
    endif.

    if not W_EBELN is initial.
      W_SAIDA-EBELN = W_EBELN.
    endif.

    if W_SAIDA-BELNR2 eq 'FIPA'
    or W_SAIDA-BELNR2 eq 'FIAP'.
      perform ADD_COMPLETE_LINE using  W_SAIDA
                                       L_EBELN_KEY
                              changing L_LAST_KEY.
    else.
      perform ADD_COMPLETE_LINE using  W_SAIDA
                                     L_PEDIDO_KEY
                            changing L_LAST_KEY.
    endif.
    clear W_EBELN.
  endloop.
  clear W_SAIDA. free W_SAIDA.

  clear: X_PASTA_CRIADA.
  TW_SAIDA_AUX[] = TW_SAIDA[].
  delete TW_SAIDA_AUX where BELNR2(2) ne 'FI'.
  sort: TW_SAIDA_AUX by EBELN.
  loop at TW_SAIDA_AUX into W_SAIDA.

** Eduardo - Calculo
    W_SAIDA-REAL_AP = W_SAIDA-DMBTR - W_SAIDA-APROVADO. "#EC CI_FLDEXT_OK[2610650]
** Eduardo - Calculo

    if W_SAIDA-BELNR2(6) = 'MIRO-0'.
      W_SAIDA-BELNR2 = 'MIRO'.
      if W_SAIDA-NETWR is initial and
         W_SAIDA-SGTXT ne 'ZIM' .
        W_SAIDA-DMBTR = W_SAIDA-NETPR .
      endif.
    endif.


    if W_SAIDA-BELNR2 = 'MIRO-NC'.
      if W_SAIDA-NETWR is initial and
         W_SAIDA-SGTXT ne 'ZIM' .
        W_SAIDA-DMBTR = W_SAIDA-NETPR * ( - 1 ).
      endif.
    endif.


    if W_SAIDA-BELNR2 is initial.
      clear: W_SAIDA-EBELN, W_SAIDA-EBELP.
    endif.

    if W_SAIDA-BELNR2 = 'PEDIDO'.
      clear: W_SAIDA-BELNR3, W_SAIDA-DMBTR, W_SAIDA-NFENUM.
    endif.

    if W_SAIDA-BELNR2 = 'MIRO'.
      clear W_SAIDA-DIFERENCA.
    endif.

    W_SAIDA-DIFERENCA = W_SAIDA-NETWR - W_SAIDA-DMBTR.

    if W_SAIDA-BELNR2 = 'MIGO-DEV'.
      clear: W_SAIDA-NETWR, W_SAIDA-DIFERENCA, W_SAIDA-DMBTR.
    endif.

    if W_SAIDA-BELNR2 = 'MIGO'.
      clear W_SAIDA-DIFERENCA.
    endif.

    if W_SAIDA-BELNR2 = 'MIGO' and ( W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
      clear W_SAIDA-NETWR.
      W_SAIDA-DMBTR = W_SAIDA-NETPR.
      W_SAIDA-DIFERENCA = W_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
    elseif W_SAIDA-BELNR2 = 'MIGO'.
      clear: W_SAIDA-DMBTR, W_SAIDA-NETWR.
    endif.



    if W_SAIDA-BELNR2 = 'MIGO-EST'.
      clear: W_SAIDA-DIFERENCA, W_SAIDA-NETWR, W_SAIDA-DMBTR.
    endif.

    if W_SAIDA-BELNR2 = 'FIPA' or W_SAIDA-BELNR2 = 'FIAP'.
      clear: W_SAIDA-DMBTR.
    endif.

    W_SAIDA-ZWAERS = W_SAIDA-WAERS.

    if W_SAIDA-BELNR2 = 'MIRO' and ( W_SAIDA-SGTXT(3) eq 'ZPI' or W_SAIDA-SGTXT(3) eq 'ZEF' ).
      clear: W_SAIDA-DMBTR, W_SAIDA-DIFERENCA.
    endif.
    if SY-TCODE eq 'ZIM08' or SY-BATCH eq 'X'.
      T_08-NUMERADOR = SY-TABIX.
      move-corresponding W_SAIDA to T_08.
      if W_SAIDA-AEDAT is initial.
        T_08-GJAHR = W_SAIDA-BUDAT.
      else.
        T_08-GJAHR = W_SAIDA-AEDAT.
      endif.
      move: SY-UNAME to  T_08-USUARIO,
            SY-DATUM to T_08-DATA_EXEC,
            WL_HORA  to T_08-HORA_EXEC.
      case 'X'.

        when P_CONSOL.
          T_08-VISAO = '01'.
        when P_ANALIT.
          T_08-VISAO = '02'.
        when P_SINCAX .
          T_08-VISAO = '03'.
      endcase.

*      APPEND t_08.
      collect T_08.
      clear T_08.
      continue.
    endif.


    if W_SAIDA-SGTXT(3) eq 'ZIM' or W_SAIDA-SGTXT(3) eq 'ZPI'.
      clear W_SAIDA-SGTXT.
    endif.
*    MOVE-CORRESPONDING we_saida to w_saida.
    clear W_MUDA.
    if SY-UCOMM = 'GROUP'.
      W_TABIX = SY-TABIX.
      W_PASTA = 1.
    else.
      W_TABIX = 0.
    endif.
    add W_SAIDA-NETWR to W_PEDIDO.
    add W_SAIDA-DMBTR to W_REALIZADO . "#EC CI_FLDEXT_OK[2610650]

    if not W_SAIDA-AUFNR is initial.
      W_PASTA = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
    else.
      concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_PASTA.
      concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN0.
*      w_pasta = w_saida-anln1.
    endif.

    if W_SAIDA-BELNR2 = 'MB1A' or
       W_SAIDA-BELNR2 = 'MBST'.
      W_PASTA = W_SAIDA-EBELN + 1.
    endif.
    if not W_SAIDA-BELNR3 is initial and
           W_SAIDA-BELNR1 is initial.
      W_PASTA = W_SAIDA-EBELN + 1.
    endif.


*    IF w_saida-belnr2 = 'MIGO' OR w_saida-belnr1 = 'MIRO'.
*      w_pasta = w_saida-ebeln + 1.
*    ENDIF.

*    IF NOT w_saida-belnr3 IS INITIAL AND
*           w_saida-belnr2 NE 'MIRO'  .
*      IF w_saida-belnr2(2) = 'FI'.
*      ELSEIF w_saida-belnr2(2) = 'CI'.
*      ELSE.
*        CLEAR w_saida-ebeln.
*      ENDIF.
*    ENDIF.

    clear WL_TABIX.
    if SY-TABIX = 1.
      WL_TABIX = 'X'.
*      PERFORM add_bukrs_line USING    w_saida
*                                       ''
*                              CHANGING l_bukrs_key.
    endif.

    if W_POSNR is initial.
*      w_tipo = 'X'.
      W_POSNR = W_SAIDA-POSNR.
      clear W_TXTTIPO.
      perform ADD_POSNR_LINE using    W_SAIDA
                                       L_BUKRS_KEY
                              changing L_POSNR_KEY.

      if W_SAIDA-TXTTIPO(5) eq 'OBRAS' or
         W_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( W_SAIDA-TXTTIPO(5) eq 'IMOBI' and not W_SAIDA-AUFNR is initial ) .
        W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
        W_ANLN0 = W_SAIDA-AUFNR.
        W_SAIDA-AUFNR = W_SAIDA-AUFNR.
      endif.

    elseif W_POSNR ne W_SAIDA-POSNR.
*      w_tipo = 'X'.
      W_POSNR = W_SAIDA-POSNR.
      clear: W_TXTTIPO, W_ANLN1.
      perform ADD_POSNR_LINE using    W_SAIDA
                                     L_BUKRS_KEY
                            changing L_POSNR_KEY.
    endif.

    if W_SAIDA-TXTTIPO(5) eq 'OBRAS' or
       W_SAIDA-TXTTIPO(5) eq 'CUSTO' or
        ( W_SAIDA-TXTTIPO(5) eq 'IMOBI' and not W_SAIDA-AUFNR is initial ) ..
      W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
      W_SAIDA-AUFNR = W_SAIDA-AUFNR.
    endif.

    if W_TXTTIPO is initial.
      W_TXTTIPO = W_SAIDA-TXTTIPO.
      clear: W_ANLN1.
*      PERFORM add_txttipo_line USING   w_saida
*                                       l_posnr_key
*                              CHANGING l_txttipo_key.
    elseif W_TXTTIPO ne W_SAIDA-TXTTIPO.
      W_TXTTIPO = W_SAIDA-TXTTIPO.
      clear W_TIPO.
*      PERFORM add_txttipo_line USING   w_saida
*                                       l_posnr_key
*                              CHANGING l_txttipo_key.
    endif.

    if not W_TIPO is initial.
      clear W_TIPO.
*      PERFORM add_txttipo_line USING   w_saida
*                                       l_posnr_key
*                              CHANGING l_txttipo_key.
    endif.

    if  W_SAIDA-ANLN1 is initial.
      W_SAIDA-ANLN1 = W_SAIDA-AUFNR.
      W_ANLN0 = W_SAIDA-AUFNR.
    endif.

    if W_SAIDA-ANLN1 = 'PLANEJADO' or
      W_SAIDA-ANLN1 = 'EXTRA'.

      on change of W_SAIDA-TXT50 or W_SAIDA-POSNR.
        perform ADD_ANLN1_LINE using     W_SAIDA
                                         L_POSNR_KEY
                                changing L_ANLN1_KEY.
      endon.

      perform ADD_COMPLETE_LINE using  W_SAIDA
                                       L_ANLN1_KEY
                              changing L_LAST_KEY.

      continue.
    endif.
    if W_ANLN1 is initial.
*      w_anln1 = w_saida-anln1.
      if W_SAIDA-AUFNR is initial.
        concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN1.
      else.
        W_ANLN1 = W_SAIDA-ANLN1.
      endif.
      W_FI = 'X'.
      W_PST = 'X'.
      WL_CRIA_FI = 'X'.
      if W_SAIDA-BELNR2 = 'PEDIDO'.
        WL_TABIX = 'X'.
      endif.
*      PERFORM add_anln1_line USING     w_saida
*                                       l_posnr_key
*                              CHANGING l_anln1_key.
    elseif W_ANLN1 ne W_ANLN0. "w_saida-anln1.
*      w_anln1 = w_saida-anln1.
      if W_SAIDA-AUFNR is initial.
        concatenate W_SAIDA-ANLN1 W_SAIDA-ANLN2 into W_ANLN1.
      else.
        W_ANLN1 = W_SAIDA-ANLN1.
      endif.
      W_PST = 'X'.
      WL_CRIA_FI = 'X'.
      if W_SAIDA-BELNR2 = 'PEDIDO'.
        WL_TABIX = 'X'.
      endif.
*      PERFORM add_anln1_line USING     w_saida
*                                       l_posnr_key
*                              CHANGING l_anln1_key.
      W_FI = 'X'.
    else.
      clear W_PST.
      if W_SAIDA-BELNR2 <> 'MB1A'.
        clear W_FI.
      endif.
    endif.

*    IF w_tabix = 1.
*      PERFORM add_ebeln_line USING     w_saida
*                                     l_anln1_key
*                            CHANGING l_ebeln_key.
*    ELSE.
******    IF w_saida-belnr2(2) = 'FI'.
*******       w_saida-belnr2(2) = 'CI' or.
*******      w_pasta = w_saida-belnr2(2).
******      write w_saida-belnr2(2) to w_pasta.
******    ENDIF.
******    ON CHANGE OF w_pasta.
******      IF w_saida-belnr2(2) = 'FI' AND
******         p_sincax EQ 'X'.
******        CLEAR w_eb.
******      ELSE.
******        w_eb = 'X'.
******      ENDIF.
******      CLEAR w_pst.
*******      if w_saida-belnr2 <> 'MB1A'.
    if W_SAIDA-BELNR2(2) eq 'FI' and
      X_PASTA_CRIADA is initial.
      perform ADD_EBELN_LINE using     W_SAIDA
                                       L_POSNR_KEY
                              changing L_EBELN_KEY.

      X_PASTA_CRIADA = 'X'.
    endif.
******    ENDON.
******    IF w_tabix = 1 AND w_eb IS INITIAL.
******      w_eb = 'X'.
******      PERFORM add_ebeln_line USING     w_saida
******                                       l_posnr_key
******                              CHANGING l_ebeln_key.
******    ELSEIF NOT w_pst IS INITIAL
******     AND ( x_pasta_criada IS INITIAL ).
******      CLEAR w_pst.
******      w_eb = 'X'.
******      x_pasta_criada = 'X'.
******      PERFORM add_ebeln_line USING     w_saida
******                                       l_posnr_key
******                              CHANGING l_ebeln_key.
******
******    ENDIF.
******
******    CLEAR w_fi.
******    ON CHANGE OF w_saida-belnr2.
******      w_fi = 'X'.
******    ENDON.


    if not W_SAIDA-BELNR3 is initial and
           W_SAIDA-BELNR1 is initial.

      W_EBELN = W_SAIDA-EBELN.
      clear W_SAIDA-EBELN.

*      IF NOT w_eb = 'X'.
      W_PED = L_PEDIDO_KEY.
*      ELSE.
*        w_ped = l_pedido_key.
*      ENDIF.

      L_PEDIDO_KEY = L_EBELN_KEY.
    else.
      on change of W_SAIDA-EBELN.
        clear WL_TABIX.
        if W_SAIDA-BELNR2 ne 'MB1A'" AND p_sincax NE 'X'.
        and W_SAIDA-BELNR2 ne 'FIAP'
        and W_SAIDA-BELNR2 ne 'FIPA'.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        else.
          L_PEDIDO_KEY = L_EBELN_KEY.
        endif.
        clear W_EB.
      endon.

** Eduado 10.05.2011
      if not WL_TABIX is initial ."AND p_sincax EQ 'X'.
        if W_SAIDA-BELNR2 ne 'MB1A' ."AND p_sincax NE 'X'.
*          PERFORM add_pedido_line USING     w_saida
*                                           l_posnr_key "l_ebeln_key
*                                  CHANGING l_pedido_key.
        else.
          L_PEDIDO_KEY = L_EBELN_KEY.
        endif.
        clear W_EB.
      endif.
** Eduado 10.05.2011

      if P_SINCAX ne 'X'.
        if not W_EB is initial.
* eduardo 29.03.2011
*        and p_sincax ne 'X'        .
* eduardo 29.03.2011
          clear W_EB.
          perform ADD_PEDIDO_LINE using     W_SAIDA
                                           L_EBELN_KEY
                                  changing L_PEDIDO_KEY.
        endif.
      endif.
    endif.

*** Eduardo 05.04.2011 - CAIXA
*** Se for CAIXA, cria pasta para PAGOS e A PAGAR
    if not P_SINCAX is initial and
       W_SAIDA-BELNR2(2) = 'FI'.
      W_MUDA = 'X'.

      if not WL_CRIA_FI is initial.
*      ON CHANGE OF w_saida-belnr2 or w_anln1.

        clear WL_CRIA_FI.

*
*      IF w_fi = 'X'.
        clear W_MUDA.
*        PERFORM add_pedido_line USING     w_saida
*                                         l_ebeln_key
*                                CHANGING l_pedido_key.
*      ENDIF.
*      ENDON.
        WL_CH = L_PEDIDO_KEY.

      endif.

    endif.
*** Eduardo 05.04.2011 - CAIXA

    if W_MUDA eq 'X'.
      clear W_MUDA.
*      l_pedido_key = w_ped.
      L_PEDIDO_KEY =  WL_CH.
    endif.

    if not W_EBELN is initial.
      W_SAIDA-EBELN = W_EBELN.
    endif.

    if W_SAIDA-BELNR2 eq 'FIPA'
    or W_SAIDA-BELNR2 eq 'FIAP'.
      perform ADD_COMPLETE_LINE using  W_SAIDA
                                       L_EBELN_KEY
                              changing L_LAST_KEY.
    else.
      perform ADD_COMPLETE_LINE using  W_SAIDA
                                     L_PEDIDO_KEY
                            changing L_LAST_KEY.
    endif.
    clear W_EBELN.
  endloop.
  clear W_SAIDA. free W_SAIDA.
  clear TW_SAIDA. refresh TW_SAIDA. free TW_SAIDA.
  W_DIF = W_PEDIDO - W_REALIZADO.

  if SY-TCODE ne 'ZIM08' and SY-BATCH ne 'X'.

* calculate totals
    call method TREE1->UPDATE_CALCULATIONS.

* this method must be called to send the data to the frontend
    call method TREE1->FRONTEND_UPDATE.
  else.

    if P_MOEDA = 'BRL'.

      modify ZIM08_REL_INV2 from table T_08.
    else.
      refresh: T_08_USD.
      loop at  T_08.
        move-corresponding: T_08 to T_08_USD.

        append  T_08_USD.
        clear: T_08_USD.
      endloop.

      modify ZIM08_REL_INV_US from table T_08_USD.
    endif.
    commit work and wait .
    refresh: T_08, T_08_USD.
  endif.

endform.                    " CREATE_HIERARCHY_2
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CREATE_HIERARCHY_2.
  types: begin of TYL_BKPF,
           BUKRS type BKPF-BUKRS,
           BELNR type BKPF-BELNR,
           GJAHR type BKPF-GJAHR,
           KURS2 type BKPF-KURS2,
         end of TYL_BKPF.

  data TL_BKPF type table of TYL_BKPF with header line.

  clear: T_08, TL_BKPF.
  refresh: T_08, TL_BKPF.

  data: WL_CRIA_FI,
        WL_CH type LVC_NKEY.

  clear: WL_CRIA_FI, WL_CH.

  clear: TW_SAIDA. refresh TW_SAIDA.

  data: WL_EBELN       type E_SAIDA-EBELN,
        WL_POSNR       type E_SAIDA-POSNR,
        X_PASTA_CRIADA,
        TW_SAIDA_AUX   like table of TW_SAIDA.

  data: begin of WE_SAIDA occurs 0.
          include structure ZES_IM01.
  data:   COLOR type   KKBLO_SPECIALCOL occurs 0.
  data: end of WE_SAIDA.


  data: "w_saida TYPE zes_im01,
    W_SAIDA   type E_SAIDA,
    W_TABIX   type SY-TABIX,
    W_TIPO(1),
    W_MUDA(1),
    W_PST(1),
    W_FI(1),
    W_PED     type LVC_NKEY,
    W_EBELN   type EKKO-EBELN,
    WL_TEXTO  type LVC_VALUE,
    WL_IMAGE  type LVC_S_LAYI-T_IMAGE,
* add data to tree
*       l_posnr_key   TYPE lvc_nkey,
*       l_bukrs_key   TYPE lvc_nkey,
*       l_txttipo_key TYPE lvc_nkey,
*       l_anln1_key   TYPE lvc_nkey,
*       l_ebeln_key   TYPE lvc_nkey,
*       l_pedido_key  TYPE lvc_nkey,
*       l_last_key    TYPE lvc_nkey.
    L_N1_KEY  type LVC_NKEY,
    L_N2_KEY  type LVC_NKEY,
    L_N3_KEY  type LVC_NKEY,
    L_N4_KEY  type LVC_NKEY,
    L_N5_KEY  type LVC_NKEY,
    WL_N0     type LVC_VALUE,
    WL_N1     type LVC_VALUE,
    WL_N2     type LVC_VALUE,
    WL_N3     type LVC_VALUE.



  if not P_ON is initial.
    perform F_PREPARA_DADOS.
    delete TW_SAIDA where AEDAT not in S_BUDAT
                      and ( BELNR2(4) eq 'FIAP'
                        or  BELNR2    eq 'FBA7' ).
    do.
      read table TW_SAIDA with key SGTXT = 'APAGAR'.
      if SY-SUBRC <> 0.
        exit.
      endif.
      clear TW_SAIDA-SGTXT.
      modify TW_SAIDA index SY-TABIX.
      WL_EBELN = TW_SAIDA-EBELN.
      WL_POSNR = TW_SAIDA-POSNR.
      loop at TW_SAIDA where EBELN = WL_EBELN and
                             POSNR = WL_POSNR and
                             ( BELNR2 <> 'PEDIDO' and
                               BELNR2 <> 'MIGO'       ).
        read table TW_SAIDA into W_SAIDA with key EBELN = WL_EBELN
                                                  POSNR = WL_POSNR
                                                  BELNR2 = 'PEDIDO'.
        if SY-SUBRC ne 0.
          clear TW_SAIDA-SGTXT.
        else.
          TW_SAIDA-SGTXT = W_SAIDA-SGTXT.
        endif.
        modify TW_SAIDA.
      endloop.
    enddo.


  else.
    perform F_SELECIONA_DADOS_OFFLINE.
    loop at T_08.
      move-corresponding T_08 to TW_SAIDA.
      append TW_SAIDA.
    endloop.

    perform ORGANIZA_TEXTO.

  endif.
  if not P_SINCAX is initial.
    delete TW_SAIDA where BELNR2 = 'MIGO'.
  endif.


  data: W_TXT type T001-BUTXT.
  data: W_POSNR     like TW_SAIDA-POSNR,
        W_TXTTIPO   like TW_SAIDA-TXTTIPO,
        W_ANLN1(20),"   LIKE tw_saida-anln1,
        W_ANLN0(20),
        W_EB(1).

  clear: W_REALIZADO, W_PEDIDO, W_POSNR, X_PASTA_CRIADA.

  loop at TW_SAIDA where TXT50 = 'Orçamento'.
    TW_SAIDA-AEDAT = S_BUDAT-LOW.
    modify TW_SAIDA.
  endloop.


  if P_SINCAX is initial.
    delete TW_SAIDA where not AEDAT in S_BUDAT and
                            ( BELNR2 ne 'PEDIDO' and
                              BELNR2 ne SPACE        ).
  else.
*    DELETE tw_saida WHERE NOT budat IN s_budat AND
*                            ( belnr2 NE 'PEDIDO' AND
*                              belnr2 NE space    AND
*                              belnr2 NE 'FIPA'   AND
*                              belnr2 NE 'FIAP'   ) ."AND
**                              belnr2(4) NE 'MIRO'     ).
  endif.
  loop at TW_SAIDA where TXT50 = 'Orçamento'.
    clear: TW_SAIDA-AEDAT, TW_SAIDA-DMBTR.
    modify TW_SAIDA.
  endloop.

  if P_CONSOL eq 'X'.
*    DELETE ADJACENT DUPLICATES FROM tw_saida COMPARING ALL FIELDS.
  endif.


  if P_OFF is initial.
*---> 04/07/2023 - Migração S4 - WS
  SORT TW_SAIDA.
*<--- 04/07/2023 - Migração S4 - WS
    delete adjacent duplicates from TW_SAIDA comparing all fields.
  endif.


  data: WL_TABIX       type C,
        WL_TABIX2      type SY-TABIX,
        WL_SAIDA       like line of TW_SAIDA,
        PS_SAIDA       like line of TW_SAIDA,
        TL_ZIMT001     type table of ZIMT001 with header line,
        TL_ZIMT001_AUX type table of ZIMT001 with header line.
  TW_SAIDA_AUX[] = TW_SAIDA[].
*  DELETE tw_saida_aux WHERE belnr2(2) EQ 'FI'.
  if P_OFF is initial.

    if TW_SAIDA[] is not initial.
      select *
          from ZIMT001
          into table TL_ZIMT001_AUX
        for all entries in TW_SAIDA
            where VISAO = '3'
              and ABUKRS eq TW_SAIDA-ABUKRS
*              and werks  eq tw_saida-werks
*              and belnr4 eq tw_saida-akostl
              and POSNR  eq TW_SAIDA-POSNR
              and ANLN1  eq TW_SAIDA-ANLN1
              and AUFNR  eq TW_SAIDA-AUFNR
              and EBELN  eq TW_SAIDA-EBELN
              and EBELP  eq TW_SAIDA-EBELP
*              and aedat  eq tw_saida-budat
              and ANULAR eq SPACE
              and ( STATUS eq 'E'
                 or STATUS eq 'A' ).

*      tl_zimt001_aux[] = tl_zimt001[].
      delete TL_ZIMT001_AUX where STATUS ne 'E'
                              and STATUS ne 'A'.

      loop at TL_ZIMT001_AUX.
        delete TW_SAIDA where POSNR   eq TL_ZIMT001_AUX-POSNR
                          and ABUKRS  eq TL_ZIMT001_AUX-ABUKRS
                          and EBELN   eq TL_ZIMT001_AUX-EBELN
                          and EBELP   eq TL_ZIMT001_AUX-EBELP
                          and AUFNR   eq TL_ZIMT001_AUX-AUFNR
                          and ANLN1   eq TL_ZIMT001_AUX-ANLN1
                          and ANLN2   eq TL_ZIMT001_AUX-ANLN2
*                      and belnr1  eq tl_zimt001_aux-doc_mm
                          and BELNR2(4) eq TL_ZIMT001_AUX-BELNR2(4)
                          and BELNR3    eq TL_ZIMT001_AUX-BELNR3
                          and BELNR6    eq TL_ZIMT001_AUX-BELNR6.
      endloop.
    endif.

    delete TL_ZIMT001_AUX where STATUS ne 'A'.
    select *
        from ZIMT001
        appending table TL_ZIMT001_AUX
          where VISAO = '3'
            and ABUKRS in S_BUKRS
            and WERKS  in S_WERKS
            and BELNR4 in S_KOSTL
            and POSNR  in S_POSNR
            and ANLN1  in S_ANLN1
            and AUFNR  in S_AUFNR
            and EBELN  in S_EBELN
*            and aedat  in s_budat
            and ANULAR eq SPACE
            and STATUS eq 'I'.
*    tl_zimt001_aux[] = tl_zimt001[].
    delete TL_ZIMT001_AUX where AEDAT lt S_BUDAT-LOW.


    if TL_ZIMT001_AUX[] is not initial
    and P_MOEDA ne 'BRL'.
      select BUKRS BELNR GJAHR KURS2
        from BKPF
        into table TL_BKPF
         for all entries in TL_ZIMT001_AUX
         where BUKRS eq TL_ZIMT001_AUX-ABUKRS
           and BELNR eq TL_ZIMT001_AUX-BELNR6
           and GJAHR eq TL_ZIMT001_AUX-AEDAT(4).
    endif.

    data: WL_TABIX_AUX type SY-TABIX.
    clear: TW_SAIDA.

    sort: TL_BKPF by BUKRS BELNR GJAHR.
    loop at TL_ZIMT001_AUX.
      move-corresponding: TL_ZIMT001_AUX to TW_SAIDA.
      if TL_ZIMT001_AUX-AEDAT > S_BUDAT-HIGH.
        if TW_SAIDA-BELNR2 eq 'FIPA'.
          move TL_ZIMT001_AUX-TOTPG to TW_SAIDA-TOTAPG.
        endif.
        clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
      endif.

      if P_MOEDA ne 'BRL'.
        if TL_ZIMT001_AUX-DMBE2 is not initial.
          if TW_SAIDA-BELNR2 eq 'FIPA'.
            if TL_ZIMT001_AUX-AEDAT > S_BUDAT-HIGH.
              move TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-TOTAPG.
              clear: TW_SAIDA-NETPR, TW_SAIDA-TOTPG.
            else.

              move: TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-TOTPG.
            endif.
          elseif TW_SAIDA-BELNR2 eq 'FIAP'.
            move: TL_ZIMT001_AUX-DMBE2 to TW_SAIDA-TOTAPG.
          endif.
        else.
          read table TL_BKPF
            with key BUKRS = TW_SAIDA-ABUKRS
                     BELNR = TW_SAIDA-BELNR6
                     GJAHR = TW_SAIDA-AEDAT(4)
                     binary search.

          if SY-SUBRC is initial.
            if TW_SAIDA-BELNR2 eq 'FIPA'.
              divide TW_SAIDA-TOTPG by TL_BKPF-KURS2.
              multiply TW_SAIDA-TOTPG by -1.
            elseif TW_SAIDA-BELNR2 eq 'FIAP'.
              divide TW_SAIDA-TOTAPG by TL_BKPF-KURS2.
              multiply TW_SAIDA-TOTAPG by -1.
            endif.
          endif.
        endif.
      endif.

      move: TL_ZIMT001_AUX-DOC_MM to TW_SAIDA-BELNR1,
            TL_ZIMT001_AUX-N0     to TW_SAIDA-N0,
            TL_ZIMT001_AUX-N1     to TW_SAIDA-N1,
            TL_ZIMT001_AUX-N2     to TW_SAIDA-N2,
            TL_ZIMT001_AUX-N3     to TW_SAIDA-N3,
            TL_ZIMT001_AUX-N4     to TW_SAIDA-N4.

      append TW_SAIDA.
      clear: TW_SAIDA.
    endloop.
  endif.
  perform BUSCA_IMPOSTOS tables TW_SAIDA.

  sort: TW_SAIDA by ABUKRS POSNR BELNR2 EBELN EBELP.
  loop at TW_SAIDA into WL_SAIDA.
    if WL_SAIDA-N0 is not initial.
      continue.
    endif.
    WL_TABIX2 = SY-TABIX.
**    Texto de empresa
    select single BUTXT from T001 into WL_SAIDA-N0
      where BUKRS = WL_SAIDA-ABUKRS.

**   Texto da SI
    perform BUSCA_TEXTO_SI using WL_SAIDA-POSNR
                           changing WL_SAIDA-N1.

**   Texto de tipo de documento
    if WL_SAIDA-BELNR2(4) eq 'MIRO'.
      WL_SAIDA-N2 =  'COMPETÊNCIA - MM-Compras'.
      concatenate 'Pedido de compra -' WL_SAIDA-EBELN
          into WL_SAIDA-N3 separated by SPACE.
      WL_SAIDA-N4 = WL_SAIDA-EBELN.
*        wl_image = '@K4@'.
      clear: WL_SAIDA-TOTPG, WL_SAIDA-TOTAPG.
    else.
      if WL_SAIDA-BELNR2(2) eq 'FI'
      or WL_SAIDA-BELNR2    eq 'FBA7'
      or WL_SAIDA-BELNR2    eq 'FBZ2'.
        WL_SAIDA-N2 = 'CAIXA - FI (Financeiro)'.
        concatenate 'Pedido de compra -' WL_SAIDA-EBELN
          into WL_SAIDA-N3 separated by SPACE.
        WL_SAIDA-N4 = WL_SAIDA-EBELN.
        if WL_SAIDA-EBELN is initial.
          delete TW_SAIDA index WL_TABIX2.
          clear: WL_SAIDA.
          continue.
        endif.
*        wl_saida-n3 = wl_saida-ebeln.
*          wl_image = '@FD@'.
      elseif WL_SAIDA-BELNR2 eq 'MBST'
          or WL_SAIDA-BELNR2 eq 'FB01'
          or WL_SAIDA-BELNR2 eq 'MB1A'
          or WL_SAIDA-BELNR2 eq 'FB08'
          or WL_SAIDA-BELNR2 eq 'FB05'
          or WL_SAIDA-BELNR2 eq 'FB50'
          or WL_SAIDA-BELNR2 eq 'FBD5'
          or WL_SAIDA-BELNR2 eq 'FBVB'
          or WL_SAIDA-BELNR2 eq 'Ciclo Rate'
          or WL_SAIDA-BELNR2 eq 'FBCJ'
          or WL_SAIDA-BELNR2 eq 'FBR2'
          or WL_SAIDA-BELNR2 eq 'FBB1'
          or WL_SAIDA-BELNR2 eq 'PRRW'.
        WL_SAIDA-N2 = 'COMPETÊNCIA - FI (Contábil)'.
        concatenate 'Doc. Contábil -' WL_SAIDA-BELNR3
         into WL_SAIDA-N3 separated by SPACE.
        WL_SAIDA-N4 = WL_SAIDA-EBELN.
*          wl_image = '@FD@'.
        delete TW_SAIDA index WL_TABIX2.
        clear: WL_SAIDA.
        continue.
      endif.
    endif.
    if WL_SAIDA-BELNR2(4) eq 'MIGO'
    or WL_SAIDA-BELNR2 eq 'PEDIDO'.
      WL_SAIDA-N2 =  'COMPETÊNCIA - MM-Compras'.
      concatenate 'Pedido de compra -' WL_SAIDA-EBELN
          into WL_SAIDA-N3 separated by SPACE.
      WL_SAIDA-N4 = WL_SAIDA-EBELN.
*        wl_image = '@K4@'.
      clear: WL_SAIDA-TOTPG, WL_SAIDA-TOTAPG.
    endif.
*    IF wl_saida-belnr2 NE 'FIPA'
*    AND wl_saida-belnr2 NE 'FIAP'.
*      CONCATENATE 'Pedido de Compras' wl_saida-ebeln
*          INTO wl_saida-n3 SEPARATED BY ' - '.
*    ENDIF.
************EBELN***************
* IF ( ps_saida-belnr3 IS INITIAL AND
*      ps_saida-belnr2 NE 'MIRO'       )." or
**     ps_saida-belnr2 ne 'MIRO'       .
**    ls_item_layout-t_image = '@FD@'.
*  ELSE.
**    ls_item_layout-t_image = '@K4@'.
*  ENDIF.
*
*
*  IF ps_saida-belnr2 = 'MB1A'.
**    ls_item_layout-t_image = '@FD@'.
*  ENDIF.
*
*    IF p_sincax IS INITIAL.
*    IF NOT ps_saida-belnr3 IS INITIAL AND
*           ps_saida-belnr2 NE 'MIRO'  .
*      IF ps_saida-belnr2(2) = 'FI'.
*        PS_saida-n2 = 'FI (Financeiro)'.
**        ls_item_layout-t_image = '@FD@'.
*      ELSEIF ps_saida-belnr2(2) = 'CI'.
*        PS_saida-n2 = 'FI (Contábil)'.
**        ls_item_layout-t_image = '@FD@'.
*      ELSE.
*        PS_saida-n2 = 'FI (Contábil)'.
**        ls_item_layout-t_image = '@FD@'.
*      ENDIF.
*    ELSE.
*      PS_saida-n2 = 'MM-Compras'.
**      ls_item_layout-t_image = '@K4@'.
**    READ TABLE t_total WITH KEY campo = ps_saida-ebeln.
**    IF sy-subrc = 0.
**      ls_saida-dmbtr = t_total-dmbtr.
**    ENDIF.
*    ENDIF.
*
*  ELSE.
*    IF ps_saida-belnr2 NE 'MIRO'  .
*      IF ps_saida-belnr2(2) = 'FI'.
*        PS_saida-n2 = 'FI (Financeiro)'.
**        ls_item_layout-t_image = '@FD@'.
*      ELSEIF ps_saida-belnr2(2) = 'CI'.
*        PS_saida-n2 = 'FI (Contábil)'.
**        ls_item_layout-t_image = '@FD@'.
*      ELSE.
*        PS_saida-n2 = 'FI (Contábil)'.
**        ls_item_layout-t_image = '@FD@'.
*      ENDIF.
*    ELSE.
*      PS_saida-n2 = 'MM-Compras'.
**      ls_item_layout-t_image = '@K4@'.
**    READ TABLE t_total WITH KEY campo = ps_saida-ebeln.
**    IF sy-subrc = 0.
**      ls_saida-dmbtr = t_total-dmbtr.
**    ENDIF.
*    ENDIF.
*  ENDIF.
*  IF ps_saida-belnr2 EQ 'MIGO' OR
*     ps_saida-belnr2 EQ 'PEDIDO' .
*    PS_saida-n2 = 'MM-Compras'.
**    ls_item_layout-t_image = '@K4@'.
*  ENDIF.
**  APPEND ls_item_layout TO lt_item_layout.
*
*  IF ps_saida-belnr2 = 'MB1A'.
*    PS_saida-n2 = 'FI (Financeiro)'.
*  ENDIF.
*************EBELN***************
*************PEDIDO**************
*IF (  ps_saida-belnr3 IS INITIAL AND
*        ps_saida-belnr2 NE 'MIRO'       )."
**    ls_item_layout-t_image = '@FD@'.
*  ELSE.
**    ls_item_layout-t_image = '@K4@'.
*
*  ENDIF.
*
*  IF ps_saida-belnr2 = 'MB1A'.
**    ls_item_layout-t_image = '@FD@'.
*  ENDIF.
*
** add node
*  IF (  ps_saida-belnr3 IS INITIAL AND
*        ps_saida-belnr2 NE 'MIRO'       )."
*
*    PS_saida-n3 = 'MM-Compras'.
**    ls_item_layout-t_image = '@K4@'.
*
*  ELSE.
*    PS_saida-n3 = 'FI (Financeiro)'.
**    ls_item_layout-t_image = '@FD@'.
*  ENDIF.
*IF ps_saida-belnr2 EQ 'PEDIDO' OR
*    ps_saida-belnr2  EQ 'MIGO'.
*    PS_saida-n3 = 'MM-Compras'.
**    ls_item_layout-t_image = '@K4@'.
*  ENDIF.
*
*  IF ps_saida-belnr2 = 'FIPA'.
*    PS_saida-n3 = 'Documentos Pagos'.
*  ELSEIF ps_saida-belnr2 = 'FIAP'.
*    PS_saida-n3 = 'Documentos a Pagar'.
*  ELSE.
*    CONCATENATE 'Pedido de Compras' ps_saida-ebeln
*          INTO PS_saida-n3 SEPARATED BY ' - '.
*  ENDIF.
*
*  IF ps_saida-belnr2 = 'MB1A'.
*    PS_saida-n3 = 'FI (Financeiro)'.
**    ls_item_layout-t_image = '@FD@'.
*  ENDIF.
*
*  IF PS_saida-n3(6) = 'Pedido'.
**    ls_item_layout-t_image = '@K4@'.
*  ENDIF.
*
**  APPEND ls_item_layout TO lt_item_layout.
*************PEDIDO**************
*********************************
*IF ( ( NOT ps_saida-belnr3 IS INITIAL AND
*             ps_saida-belnr1 IS INITIAL        ) OR
*     (       ps_saida-belnr2 = 'MB1A' OR
*             ps_saida-belnr2 = 'MBST'     ) ).
*
*    IF ps_saida-belnr2 = 'MBST'.
**      ls_item_layout-t_image = '@11@'.
*    ELSE.
**      ls_item_layout-t_image = '@FD@'.
*    ENDIF.
*  ELSE.
**    ls_item_layout-t_image = '@96@'.
*  ENDIF.
*
**  CASE ps_saida-belnr2.
**    WHEN 'MIGO'.     ls_item_layout-t_image = '@PG@'.
**    WHEN 'MIGO-EST'. ls_item_layout-t_image = '@11@'.
**    WHEN 'MIGO-DEV'. ls_item_layout-t_image = '@2W@'.
**    WHEN 'MIRO'.     ls_item_layout-t_image = '@BE@'.
**  ENDCASE.
*
*IF ps_saida-anln1 = 'PLANEJADO' OR
*    ps_saida-anln1 = 'EXTRA'.
**    CONCATENATE " ps_saida-anln1'Item' ps_saida-ebelp ps_saida-txz01 INTO l_node_text SEPARATED BY ' - '.
**    CONCATENATE  ps_saida-ebelp ps_saida-anln1 ps_saida-txz01 INTO l_node_text SEPARATED BY ' - '.
*    PS_saida-n3 = ps_saida-txz01 .
*  ELSE.
**    IF NOT ps_saida-belnr3 IS INITIAL AND
**           ps_saida-belnr1 IS INITIAL .
*    IF ( ( NOT ps_saida-belnr3 IS INITIAL AND
*               ps_saida-belnr1 IS INITIAL        ) OR
*       (       ps_saida-belnr2 = 'MB1A' OR
*               ps_saida-belnr2 = 'MBST'        ) ).
*      CONCATENATE 'Doc. Contábil -' ps_saida-belnr3
*      INTO PS_saida-n3 SEPARATED BY space.
*    ELSE.
*      IF p_sincax IS INITIAL.
*        CONCATENATE ps_saida-ebeln ps_saida-ebelp
*           INTO PS_saida-n3 SEPARATED BY ' - '.
*
*      ELSE.
*        MOVE: ps_saida-ebeln TO PS_saida-n3.
*
*      ENDIF.
*    ENDIF.
*
*    IF ps_saida-belnr2 = 'PEDIDO'.
*      CONCATENATE ps_saida-ebeln ps_saida-ebelp
*       INTO PS_saida-n3 SEPARATED BY ' - '.
*
*    ENDIF.
*
*  ENDIF.
*********************************
    modify TW_SAIDA from WL_SAIDA index WL_TABIX2.
    clear WL_SAIDA.
  endloop.
  if P_SINCAX is not initial.
    loop at TW_SAIDA.
      if TW_SAIDA-AEDAT not in S_BUDAT
      and TW_SAIDA-BELNR2(2) ne 'FI'.
        delete TW_SAIDA.
      endif.
    endloop.

  endif.
  clear: WL_N0, WL_N1, WL_N2, WL_N3.
  sort: TW_SAIDA by N0 N1 N2 N3 N4.
  loop at TW_SAIDA into WL_SAIDA.
**************
* Eduardo - Calculo
    WL_SAIDA-REAL_AP = WL_SAIDA-DMBTR - WL_SAIDA-APROVADO. "#EC CI_FLDEXT_OK[2610650]
** Eduardo - Calculo

    if WL_SAIDA-BELNR2(6) = 'MIRO-0'.
      WL_SAIDA-BELNR2 = 'MIRO'.
      if WL_SAIDA-NETWR is initial and
         WL_SAIDA-SGTXT ne 'ZIM' .
        WL_SAIDA-DMBTR = WL_SAIDA-NETPR .
      endif.
    endif.


    if WL_SAIDA-BELNR2 = 'MIRO-NC'.
      if WL_SAIDA-NETWR is initial and
         WL_SAIDA-SGTXT ne 'ZIM' .
        if WL_SAIDA-NETPR gt 0.
          WL_SAIDA-DMBTR = WL_SAIDA-NETPR * ( - 1 ).
        else.
          WL_SAIDA-DMBTR = WL_SAIDA-NETPR.
        endif.
      endif.
    endif.


    if WL_SAIDA-BELNR2 is initial.
      clear: WL_SAIDA-EBELN, WL_SAIDA-EBELP.
    endif.

    if WL_SAIDA-BELNR2 = 'PEDIDO'.
      clear: WL_SAIDA-BELNR3, WL_SAIDA-DMBTR, WL_SAIDA-NFENUM.
    endif.

    if WL_SAIDA-BELNR2 = 'MIRO'.
      clear WL_SAIDA-DIFERENCA.
    endif.

    WL_SAIDA-DIFERENCA = WL_SAIDA-NETWR - WL_SAIDA-DMBTR.

    if WL_SAIDA-BELNR2 = 'MIGO-DEV'.
      clear: WL_SAIDA-NETWR, WL_SAIDA-DIFERENCA, WL_SAIDA-DMBTR.
    endif.

    if WL_SAIDA-BELNR2 = 'MIGO'.
      clear WL_SAIDA-DIFERENCA.
    endif.

    if WL_SAIDA-BELNR2 = 'MIGO' and ( WL_SAIDA-SGTXT(3) eq 'ZIM' or WL_SAIDA-SGTXT(3) eq 'ZPI' or WL_SAIDA-SGTXT(3) eq 'ZEF').
      clear WL_SAIDA-NETWR.
      WL_SAIDA-DMBTR = WL_SAIDA-NETPR.
      WL_SAIDA-DIFERENCA = WL_SAIDA-NETPR * ( - 1 ).
*      CLEAR w_saida-sgtxt.
    elseif WL_SAIDA-BELNR2 = 'MIGO'.
      clear: WL_SAIDA-DMBTR, WL_SAIDA-NETWR.
    endif.



    if WL_SAIDA-BELNR2 = 'MIGO-EST'.
      clear: WL_SAIDA-DIFERENCA, WL_SAIDA-NETWR, WL_SAIDA-DMBTR.
    endif.

    if WL_SAIDA-BELNR2 = 'FIPA' or WL_SAIDA-BELNR2 = 'FIAP'.
      clear: WL_SAIDA-DMBTR.
    endif.

    WL_SAIDA-ZWAERS = WL_SAIDA-WAERS.

    if WL_SAIDA-BELNR2 = 'MIRO' and ( WL_SAIDA-SGTXT(3) eq 'ZPI' or WL_SAIDA-SGTXT(3) eq 'ZEF').
      clear: WL_SAIDA-DMBTR, WL_SAIDA-DIFERENCA.
    endif.
    if SY-TCODE eq 'ZIM08' or SY-BATCH eq 'X'.
      T_08-NUMERADOR = SY-TABIX.
      move-corresponding W_SAIDA to T_08.
      if W_SAIDA-AEDAT is initial.
        T_08-GJAHR = WL_SAIDA-BUDAT.
      else.
        T_08-GJAHR = WL_SAIDA-AEDAT.
      endif.
      move: SY-UNAME to  T_08-USUARIO,
            SY-DATUM to T_08-DATA_EXEC,
            WL_HORA  to T_08-HORA_EXEC.
      case 'X'.

        when P_CONSOL.
          T_08-VISAO = '01'.
        when P_ANALIT.
          T_08-VISAO = '02'.
        when P_SINCAX .
          T_08-VISAO = '03'.
      endcase.

*      APPEND t_08.
      collect T_08.
      clear T_08.
      continue.
    endif.


    if WL_SAIDA-SGTXT(3) eq 'ZIM' or WL_SAIDA-SGTXT(3) eq 'ZPI'.
      clear WL_SAIDA-SGTXT.
    endif.
*    MOVE-CORRESPONDING we_saida to w_saida.
    clear W_MUDA.
    if SY-UCOMM = 'GROUP'.
      W_TABIX = SY-TABIX.
      W_PASTA = 1.
    else.
      W_TABIX = 0.
    endif.
    add WL_SAIDA-NETWR to W_PEDIDO.
    add WL_SAIDA-DMBTR to W_REALIZADO . "#EC CI_FLDEXT_OK[2610650]

    if not WL_SAIDA-AUFNR is initial.
      W_PASTA = WL_SAIDA-AUFNR.
      W_ANLN0 = WL_SAIDA-AUFNR.
    else.
      concatenate WL_SAIDA-ANLN1 WL_SAIDA-ANLN2 into W_PASTA.
      concatenate WL_SAIDA-ANLN1 WL_SAIDA-ANLN2 into W_ANLN0.
*      w_pasta = w_saida-anln1.
    endif.

    if WL_SAIDA-BELNR2 = 'MB1A' or
       WL_SAIDA-BELNR2 = 'MBST'.
      W_PASTA = WL_SAIDA-EBELN + 1.
    endif.
    if not WL_SAIDA-BELNR3 is initial and
           WL_SAIDA-BELNR1 is initial.
      W_PASTA = WL_SAIDA-EBELN + 1.
    endif.

**************

    if WL_SAIDA-N0 is not initial
    and WL_SAIDA-N0 ne WL_N0.
      PS_SAIDA = WL_SAIDA.
      clear: PS_SAIDA-BELNR2, PS_SAIDA-EBELN, PS_SAIDA-EBELP, PS_SAIDA-AEDAT, PS_SAIDA-BELNR3, PS_SAIDA-BELNR4,
             PS_SAIDA-BELNR5, PS_SAIDA-BELNR6, PS_SAIDA-OBJETO, PS_SAIDA-NETPR, PS_SAIDA-FORNECEDOR.
*      ON CHANGE OF wl_saida-n0. " por probelmas q fogem da minha inteligencia o On Change não funciona
      " o tratamento esta sendo feito atravez de "if"
      perform CRIA_PASTA    using PS_SAIDA
                                  SPACE
                                  '@A8@'
                                  WL_SAIDA-N0
                                  CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                         changing L_N1_KEY.
*      ENDON.
      WL_N0 = WL_SAIDA-N0.
    endif.

    if WL_SAIDA-N1 is not initial
    and WL_SAIDA-N1 ne WL_N1.
      PS_SAIDA = WL_SAIDA.
      clear: PS_SAIDA-BELNR2, PS_SAIDA-EBELN, PS_SAIDA-EBELP, PS_SAIDA-AEDAT, PS_SAIDA-BELNR3, PS_SAIDA-BELNR4,
             PS_SAIDA-BELNR5, PS_SAIDA-BELNR6, PS_SAIDA-OBJETO, PS_SAIDA-NETPR, PS_SAIDA-FORNECEDOR.
*      ON CHANGE OF wl_saida-n1.

      perform CRIA_PASTA    using PS_SAIDA
                                  L_N1_KEY
                                  '@A8@'
                                  WL_SAIDA-N1
                                  CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                         changing L_N2_KEY.
*      ENDON.
      WL_N1 = WL_SAIDA-N1.
***   limpa o link da pasta do nivel abaixo, pois nos casos que tem o mesmo nome de pasta ele nao trata como novo nivel.
      clear: WL_N2.
    endif.

    if WL_SAIDA-N2 is not initial
    and WL_SAIDA-N2 ne WL_N2.
      PS_SAIDA = WL_SAIDA.
      clear: PS_SAIDA-BELNR2, PS_SAIDA-EBELN, PS_SAIDA-EBELP, PS_SAIDA-AEDAT, PS_SAIDA-BELNR3, PS_SAIDA-BELNR4,
             PS_SAIDA-BELNR5, PS_SAIDA-BELNR6, PS_SAIDA-OBJETO, PS_SAIDA-NETPR, PS_SAIDA-FORNECEDOR.
*      ON CHANGE OF wl_saida-n2.
      if WL_SAIDA-N2(2) eq 'FI'.
        WL_IMAGE = '@FD@'.
      else.
        WL_IMAGE = '@K4@'.
      endif.
      perform CRIA_PASTA    using PS_SAIDA
                                  L_N2_KEY
                                  WL_IMAGE
                                  WL_SAIDA-N2
                                  CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                         changing L_N3_KEY.
*      ENDON.
      WL_N2 = WL_SAIDA-N2.
***   limpa o link da pasta do nivel abaixo, pois nos casos que tem o mesmo nome de pasta ele nao trata como novo nivel.
      clear: WL_N3.
    endif.

    if WL_SAIDA-N3 is not initial
    and WL_SAIDA-N3 ne WL_N3.
      PS_SAIDA = WL_SAIDA.
      clear: PS_SAIDA-BELNR2, PS_SAIDA-EBELN, PS_SAIDA-EBELP, PS_SAIDA-AEDAT, PS_SAIDA-BELNR3, PS_SAIDA-BELNR4,
             PS_SAIDA-BELNR5, PS_SAIDA-BELNR6, PS_SAIDA-OBJETO, PS_SAIDA-NETPR, PS_SAIDA-FORNECEDOR.
*      ON CHANGE OF wl_saida-n3.
      if WL_SAIDA-N2(2) eq 'FI'.
        WL_IMAGE = '@FD@'.
      else.
        WL_IMAGE = '@K4@'.
      endif.
      perform CRIA_PASTA    using PS_SAIDA
                                  L_N3_KEY
                                  WL_IMAGE
                                  WL_SAIDA-N3
                                  CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                         changing L_N4_KEY.
*      ENDON.
      WL_N3 = WL_SAIDA-N3.
    endif.

    if WL_SAIDA-N4 is not initial.
*    and wl_saida-n3 ne wl_n3.
*      ps_saida = wl_saida.
*      CLEAR: ps_saida-belnr2, ps_saida-ebeln, ps_saida-ebelp, ps_saida-aedat, ps_saida-belnr3, ps_saida-belnr4,
*             ps_saida-belnr5, ps_saida-belnr6, ps_saida-objeto, ps_saida-netpr, ps_saida-fornecedor.
*      ON CHANGE OF wl_saida-n3.
      if WL_SAIDA-N2(2) eq 'FI'.
        WL_IMAGE = '@FD@'.
      else.
        WL_IMAGE = '@K4@'.
      endif.
      perform CRIA_PASTA    using WL_SAIDA
                                  L_N4_KEY
                                  WL_IMAGE
                                  WL_SAIDA-N4
                                  CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                         changing L_N5_KEY.
*      ENDON.
    endif.

  endloop.
  clear W_SAIDA. free W_SAIDA.
  clear TW_SAIDA. refresh TW_SAIDA. free TW_SAIDA.
  W_DIF = W_PEDIDO - W_REALIZADO.

  if SY-TCODE ne 'ZIM08' and SY-BATCH ne 'X'.

* calculate totals
    call method TREE1->UPDATE_CALCULATIONS.

* this method must be called to send the data to the frontend
    call method TREE1->FRONTEND_UPDATE.
  else.

    if P_MOEDA = 'BRL'.

      modify ZIM08_REL_INV2 from table T_08.
    else.
      refresh: T_08_USD.
      loop at  T_08.
        move-corresponding: T_08 to T_08_USD.

        append  T_08_USD.
        clear: T_08_USD.
      endloop.

      modify ZIM08_REL_INV_US from table T_08_USD.
    endif.
    refresh: T_ZIMT003, T_08_USD.

    move: S_BUKRS-LOW to T_ZIMT003-BUKRS,
          SY-DATUM    to T_ZIMT003-AEDAT,
          SY-UZEIT    to T_ZIMT003-AENTIME,
          SY-UNAME    to T_ZIMT003-AENUSER.

    append T_ZIMT003.
    clear T_ZIMT003.
    modify  ZIMT003 from table T_ZIMT003.
    commit work and wait .
    refresh T_08.
  endif.

endform.                    " CREATE_HIERARCHY_2
*&---------------------------------------------------------------------*
*&      Form  CRIA_PASTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_SAIDA  text
*      -->P_SPACE  text
*      -->P_WL_SAIDA_BUTXT  text
*      <--P_P_NAME1  text
*----------------------------------------------------------------------*
form CRIA_PASTA  using    WL_SAIDA
                          P_RELAT_KEY
                          P_IMAGE
                          P_NODE_TEXT
                    value(P_FONTE)
                 changing P_NEXT_KEY.

  data: L_NODE_TEXT type LVC_VALUE.
  data: W_TXT type T001-BUTXT.
  data: P_NODE_KEY type LVC_NKEY.

* set item-layout
  data: LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI.

  LS_ITEM_LAYOUT-T_IMAGE = P_IMAGE.                         "'@A8@'.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
  LS_ITEM_LAYOUT-STYLE   = P_FONTE.
*                        cl_gui_column_tree=>style_intensifd_critical.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

* add node
  L_NODE_TEXT =  P_NODE_TEXT.

  data: LS_NODE type LVC_S_LAYN.
  LS_NODE-N_IMAGE   = SPACE.
  LS_NODE-EXP_IMAGE = SPACE.
  call method TREE1->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = WL_SAIDA
*     is_node_layout   = ls_node
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NEXT_KEY.
endform.                    " CRIA_PASTA
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TEXTO_SI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_SAIDA_POSNR  text
*      <--P_WL_TEXTO  text
*----------------------------------------------------------------------*
form BUSCA_TEXTO_SI  using    WL_POSNR
                     changing P_TEXTO.

  P_TEXTO =  WL_POSNR.
  shift P_TEXTO left deleting leading '0'.

  read table T_IMAK with key POSNR = WL_POSNR.

  if SY-SUBRC = 0.
    concatenate P_TEXTO T_IMAK-TXT50
          into P_TEXTO separated by SPACE.
  else.
    select single TXT50 from IMAKT into T_IMAK-TXT50
      where SPRAS eq SY-LANGU and POSNR = WL_POSNR.
    if SY-SUBRC = 0.
      concatenate P_TEXTO T_IMAK-TXT50
           into P_TEXTO separated by SPACE.
    endif.
  endif.
endform.                    " BUSCA_TEXTO_SI
*&---------------------------------------------------------------------*
*&      Form  CRIA_TABELA_IMPOSTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CRIA_TABELA_IMPOSTOS .
  data: TG_COMP type ref to DATA.
  create data TG_COMP like line of TW_SAIDA.

  GO_SDESCR ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( TG_COMP ).
  GD_TABNAM     = GO_SDESCR->GET_RELATIVE_NAME( ).

  loop at GO_SDESCR->COMPONENTS into GS_COMPONENT.

    "   Cria os campos
    concatenate GD_TABNAM GS_COMPONENT-NAME into GD_TABFIELD
                                            separated by '-'.

    clear: GS_COMP.
    GS_COMP-TYPE ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_NAME( GD_TABFIELD ).
    GS_COMP-NAME  = GS_COMPONENT-NAME.
    append GS_COMP to GT_COMPONENTS.
  endloop.

  loop at TG_IMPOSTOS.
    GS_COMP-TYPE ?= CL_ABAP_DATADESCR=>DESCRIBE_BY_NAME( 'BSIS-DMBTR' ).
    GS_COMP-NAME  = TG_IMPOSTOS-FIELD_IMP.
    read table GT_COMPONENTS transporting no fields
      with key NAME = GS_COMP-NAME.
    if SY-SUBRC is not initial.
      append GS_COMP to GT_COMPONENTS.
    endif.
  endloop.

  "   Cria instacia para a estrutura dinamica
  GO_SDESCR_NEW  = CL_ABAP_STRUCTDESCR=>CREATE( GT_COMPONENTS ).
  GO_TDESCR      = CL_ABAP_TABLEDESCR=>CREATE( GO_SDESCR_NEW ).
  "   Cria os dados conforme referencia
  create data E_TABELA type handle GO_TDESCR.
  assign E_TABELA->* to <FS_IMPOSTOS>.
endform.                    " CRIA_TABELA_IMPOSTOS
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_LAYOUT_CX4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_MODIFICA_LAYOUT_CX4 .
  data: LS_FIELDCATALOG type LVC_S_FCAT.


  loop at GT_FIELDCATALOG into LS_FIELDCATALOG.

    case LS_FIELDCATALOG-FIELDNAME.

      when 'NETWR' .
        LS_FIELDCATALOG-COL_POS = 1.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-SCRTEXT_S = 'Pedido'.
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Pedido'.
      when 'DMBTR'.
        LS_FIELDCATALOG-COL_POS = 2.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S = 'Vl Realizado'.
        LS_FIELDCATALOG-SCRTEXT_M = 'Realizado'.
        LS_FIELDCATALOG-COLTEXT = 'Realizado'.
      when 'TOTPG'.
        LS_FIELDCATALOG-COL_POS = 3.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tot. Pago'.
      when 'TOTAPG'.
        LS_FIELDCATALOG-COL_POS = 4.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Tot. Pagar'.
      when 'IMP_TOTAL' .
        LS_FIELDCATALOG-COL_POS = 4.
        LS_FIELDCATALOG-DO_SUM = 'X'.
        LS_FIELDCATALOG-SCRTEXT_S = 'Total de Impostos'.
        LS_FIELDCATALOG-SCRTEXT_M = 'Total de Impostos'.
        LS_FIELDCATALOG-COLTEXT = 'Total de Impostos'.
        LS_FIELDCATALOG-OUTPUTLEN = 25.
        LS_FIELDCATALOG-HOTSPOT = ' '.
      when 'EBELN'.
        LS_FIELDCATALOG-COL_POS = 5.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'EBELP'.
        LS_FIELDCATALOG-COL_POS = 6.
        LS_FIELDCATALOG-OUTPUTLEN = 8.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'AEDAT'.
        LS_FIELDCATALOG-COL_POS = 7.
        LS_FIELDCATALOG-OUTPUTLEN = 15.
        LS_FIELDCATALOG-KEY    = 'X'.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Data'.
      when 'BELNR5' .
        LS_FIELDCATALOG-COL_POS = 7.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Doc.Miro'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when 'BELNR3' .
        LS_FIELDCATALOG-COL_POS = 8.
        LS_FIELDCATALOG-SCRTEXT_S =
        LS_FIELDCATALOG-SCRTEXT_M =
        LS_FIELDCATALOG-COLTEXT = 'Doc.Contábil'.
        LS_FIELDCATALOG-OUTPUTLEN = 20.
        LS_FIELDCATALOG-HOTSPOT = 'X'.
      when others.
        read table TG_IMPOSTOS
        with key FIELD_IMP = LS_FIELDCATALOG-FIELDNAME.
        if SY-SUBRC is initial.
          describe table GT_FIELDCATALOG lines LS_FIELDCATALOG-COL_POS.
          add 1 to LS_FIELDCATALOG-COL_POS.
          LS_FIELDCATALOG-DO_SUM = 'X'.
          LS_FIELDCATALOG-OUTPUTLEN = 25.
          LS_FIELDCATALOG-SCRTEXT_S = TG_IMPOSTOS-BEZEI.
          LS_FIELDCATALOG-SCRTEXT_M = TG_IMPOSTOS-BEZEI.
          LS_FIELDCATALOG-COLTEXT = TG_IMPOSTOS-BEZEI.
        else.
          LS_FIELDCATALOG-NO_OUT = 'X'.
          LS_FIELDCATALOG-KEY    = ''.
        endif.
    endcase.
    modify GT_FIELDCATALOG from LS_FIELDCATALOG.
  endloop.

endform.                    " F_MODIFICA_LAYOUT_CX4
*&---------------------------------------------------------------------*
*&      Form  BUSCA_IMPOSTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TW_SAIDA  text
*----------------------------------------------------------------------*
form BUSCA_IMPOSTOS  tables   P_TW_SAIDA.
  data: CONDI_S     type STRING,
        CONDI_L     type STRING,
        WL_RATE(10) type P decimals 8.

*        C_MIRO(4) TYPE C VALUE 'MIRO'.
  data: begin of TL_BSIS_IMP occurs 0,
          BUKRS type BSIS-BUKRS,
          BELNR type BSIS-BELNR,
          GJAHR type BSIS-GJAHR,
          HKONT type BSIS-HKONT,
          DMBTR type BSIS-DMBTR,
          DMBE2 type BSIS-DMBE2,
          SHKZG type BSIS-SHKZG,
          AUGDT type BSIS-AUGDT,
          AUGBL type BSIS-AUGBL,
          ZUONR type BSIS-ZUONR,
          BUZEI type BSIS-BUZEI,
        end of TL_BSIS_IMP,

        begin of TL_MIRO_TOT occurs 0,
          BELNR type EKBE-BELNR,
          DMBTR type EKBE-DMBTR,
        end of TL_MIRO_TOT,

        begin of TL_BKPF_IMP occurs 0,
          BUKRS    type BKPF-BUKRS,
          BELNR    type BKPF-BELNR,
          GJAHR    type BKPF-GJAHR,
          AWKEY    type BKPF-AWKEY,
          KURS2    type BKPF-KURS2,
          BELNR_EK type EKBE-BELNR,
          GJAHR_EK type EKBE-GJAHR,
        end of TL_BKPF_IMP,

        begin of TL_EKBE_IMP occurs 0,
          EBELN type EKBE-EBELN,
          EBELP type EKBE-EBELP,
          BELNR type EKBE-BELNR,
          GJAHR type EKBE-GJAHR,
          DMBTR type EKBE-DMBTR,
          ZEKKN type EKBE-ZEKKN,
          BUZEI type EKBE-BUZEI,
        end of TL_EKBE_IMP.

  if P_TW_SAIDA[] is not initial.

    refresh: TG_IMPOSTOS.

    select ZIMT002~FIELD_IMP ZIMT002~SAKNR ZIMT002T~BEZEI
      from ZIMT002
      inner join ZIMT002T on
      ( ZIMT002~FIELD_IMP eq ZIMT002T~FIELD_IMP )
      into table TG_IMPOSTOS.

    data: WL_FIELD_AUX(40).
    field-symbols: <FS_FIELD_AUX> type ANY.
    field-symbols: <FS_FIELD_AUX2> type ANY.
    field-symbols: <FS_FIELD_AUX3> type ANY.
    CONDI_S = 'BUKRS EQ P_TW_SAIDA-ABUKRS AND BELNR EQ P_TW_SAIDA-BELNR3 AND GJAHR EQ P_TW_SAIDA-AEDAT(4)'.

    select BUKRS BELNR GJAHR HKONT DMBTR DMBE2 SHKZG
           AUGDT AUGBL ZUONR BUZEI
      from BSIS
      into table TL_BSIS_IMP
       for all entries in P_TW_SAIDA
        where (CONDI_S).

    select BUKRS BELNR GJAHR AWKEY KURS2
      from BKPF
      into table TL_BKPF_IMP
       for all entries in P_TW_SAIDA
        where (CONDI_S)
          and STBLG eq SPACE.

    if SY-SUBRC is initial.
      loop at TL_BKPF_IMP.
        condense TL_BKPF_IMP-AWKEY no-gaps.

        move: TL_BKPF_IMP-AWKEY(10)   to TL_BKPF_IMP-BELNR_EK,
              TL_BKPF_IMP-AWKEY+10(4) to TL_BKPF_IMP-GJAHR_EK.

        modify TL_BKPF_IMP.
      endloop.

      select EBELN EBELP BELNR GJAHR DMBTR ZEKKN BUZEI
        from EKBE
        into table TL_EKBE_IMP
         for all entries in TL_BKPF_IMP
         where BELNR eq TL_BKPF_IMP-BELNR_EK
           and GJAHR eq TL_BKPF_IMP-GJAHR_EK
           and VGABE eq '2'.
    endif.

    loop at TL_BSIS_IMP.
      read table TG_IMPOSTOS transporting no fields
        with key SAKNR = TL_BSIS_IMP-HKONT.
      if SY-SUBRC is not initial.
        delete table TL_BSIS_IMP.
      endif.
    endloop.
    sort TL_BKPF_IMP by BELNR_EK GJAHR_EK.
    loop at TL_EKBE_IMP.
      read table TL_BKPF_IMP
        with key BELNR_EK = TL_EKBE_IMP-BELNR
                 GJAHR_EK = TL_EKBE_IMP-GJAHR
                    binary search.

      move: TL_BKPF_IMP-BELNR to TL_MIRO_TOT-BELNR.
      if P_MOEDA ne 'BRL'.
        if TL_BKPF_IMP-KURS2 is not initial.
          TL_MIRO_TOT-DMBTR = TL_EKBE_IMP-DMBTR / TL_BKPF_IMP-KURS2.
*            tl_ekbe_imp-dmbtr to tl_miro_tot-dmbtr.
        endif.
      else.
        TL_MIRO_TOT-DMBTR = TL_EKBE_IMP-DMBTR.
      endif.
      collect TL_MIRO_TOT.
    endloop.
*    loop at p_tw_saida.
*      clear: wl_field_aux.
*      unassign <fs_field_aux>.
*
*      wl_field_aux = 'P_TW_SAIDA-BELNR2(4)' .
*      assign (wl_field_aux) to <fs_field_aux>.
*      if <fs_field_aux> eq 'MIRO'.
*        unassign <fs_field_aux>.
*        wl_field_aux = 'P_TW_SAIDA-DMBTR' .
*        assign (wl_field_aux) to <fs_field_aux>.
*        move: <fs_field_aux> to  tl_miro_tot-dmbtr.
*        unassign <fs_field_aux>.
*        wl_field_aux = 'P_TW_SAIDA-BELNR3'.
*        assign (wl_field_aux) to <fs_field_aux>.
*        move: <fs_field_aux> to  tl_miro_tot-belnr.
*        collect tl_miro_tot.
*      endif.
*    endloop.


    loop at P_TW_SAIDA.
      clear: WL_FIELD_AUX.
      unassign <FS_FIELD_AUX>.

      WL_FIELD_AUX = 'P_TW_SAIDA-BELNR2(4)' .
      assign (WL_FIELD_AUX) to <FS_FIELD_AUX>.
      if <FS_FIELD_AUX> eq 'MIRO'.

        loop at TL_BSIS_IMP.
          clear: WL_FIELD_AUX.
          unassign <FS_FIELD_AUX>.

          WL_FIELD_AUX = 'P_TW_SAIDA-ABUKRS' .
          assign (WL_FIELD_AUX) to <FS_FIELD_AUX>.
          if TL_BSIS_IMP-BUKRS ne <FS_FIELD_AUX>.
            continue.
          endif.

          clear: WL_FIELD_AUX.
          unassign <FS_FIELD_AUX>.

          WL_FIELD_AUX = 'P_TW_SAIDA-BELNR3' .
          assign (WL_FIELD_AUX) to <FS_FIELD_AUX>.
          if TL_BSIS_IMP-BELNR ne <FS_FIELD_AUX>.
            continue.
          endif.

          clear: WL_FIELD_AUX.
          unassign <FS_FIELD_AUX>.

          WL_FIELD_AUX = 'P_TW_SAIDA-AEDAT(4)' .
          assign (WL_FIELD_AUX) to <FS_FIELD_AUX>.
          if TL_BSIS_IMP-GJAHR ne <FS_FIELD_AUX>.
            continue.
          endif.

          read table TG_IMPOSTOS
           with key SAKNR = TL_BSIS_IMP-HKONT.

          if SY-SUBRC is initial
          and TG_IMPOSTOS-FIELD_IMP is not initial.
            clear: WL_FIELD_AUX.
            unassign: <FS_FIELD_AUX>, <FS_FIELD_AUX2>, <FS_FIELD_AUX3>.

            concatenate 'P_TW_SAIDA-'TG_IMPOSTOS-FIELD_IMP into WL_FIELD_AUX.
            assign (WL_FIELD_AUX) to <FS_FIELD_AUX>.

            move: 'P_TW_SAIDA-IMP_TOTAL' to WL_FIELD_AUX.
            assign (WL_FIELD_AUX) to <FS_FIELD_AUX2>.

            read table TL_MIRO_TOT
              with key BELNR = TL_BSIS_IMP-BELNR.
            if SY-SUBRC is initial.
              clear: WL_RATE.
              move: 'P_TW_SAIDA-DMBTR' to WL_FIELD_AUX.
              assign (WL_FIELD_AUX) to <FS_FIELD_AUX3>.
              if <FS_FIELD_AUX3> is assigned.
                try.
                    WL_RATE =  <FS_FIELD_AUX3> / TL_MIRO_TOT-DMBTR.
                  catch CX_SY_ZERODIVIDE.
                endtry.
              endif.
            endif.

            if TL_BSIS_IMP-SHKZG eq 'H'.
              multiply TL_BSIS_IMP-DMBTR by -1.
              multiply TL_BSIS_IMP-DMBE2 by -1.
            endif.

            if <FS_FIELD_AUX> is assigned.
              if P_MOEDA eq 'USD'.
                multiply TL_BSIS_IMP-DMBE2 by WL_RATE.
                add  TL_BSIS_IMP-DMBE2 to <FS_FIELD_AUX>.
              else.
                multiply TL_BSIS_IMP-DMBTR by WL_RATE.
                add  TL_BSIS_IMP-DMBTR to <FS_FIELD_AUX>.
              endif.
            endif.

            if <FS_FIELD_AUX2> is assigned.
              if P_MOEDA eq 'USD'.
*                multiply tl_bsis_imp-dmbe2 by wl_rate.
                add TL_BSIS_IMP-DMBE2 to <FS_FIELD_AUX2>.
              else.
*                multiply tl_bsis_imp-dmbtr by wl_rate.
                add TL_BSIS_IMP-DMBTR to <FS_FIELD_AUX2>.
              endif.
            endif.
          endif.
*          delete tl_bsis_imp.
        endloop.
        modify P_TW_SAIDA.
      endif.
    endloop.

  endif.
endform.                    " BUSCA_IMPOSTOS
