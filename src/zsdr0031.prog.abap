*&---------------------------------------------------------------------*
*& Report  ZSDR0031
*&
*&---------------------------------------------------------------------*
*&TITULO  : REL. VENDA ALGODÃO
*&AUTOR   : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA.   : 03.09.2013
*TRANSACAO: ZSDT0071
*&---------------------------------------------------------------------*


REPORT  zsdr0031.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zsdt0051, zsdt0053, zsdt0066, zsdt0045, j_1bnfdoc, mchb, zppt0002.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-005.
  PARAMETER: r_rege RADIOBUTTON GROUP b2 DEFAULT 'X' USER-COMMAND a,
             r_volu RADIOBUTTON GROUP b2,
             r_peso RADIOBUTTON GROUP b2,
             r_fard RADIOBUTTON GROUP b2,
             r_acts RADIOBUTTON GROUP b2.  "CS2023000189-31.05.2023-#108695-JT
*          r_expo RADIOBUTTON GROUP b2.  "CS2022000332-#78919-16.06.2022-JT
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_vkorg     FOR zsdt0051-vkorg  NO INTERVALS MODIF ID c2, "## "Incluido opção seleção multiplas / USER STORY 101490 / AOENNING
                  p_spart     FOR zsdt0051-spart NO INTERVALS MODIF ID c2, "##
                  p_vkbur     FOR zsdt0051-vkbur MODIF ID c2,
                  p_werks     FOR zsdt0053-werks MODIF ID c2,
                  p_kunnr     FOR zsdt0051-kunnr MODIF ID c2,
                  p_bstkd     FOR zsdt0051-bstkd NO INTERVALS MODIF ID c2,
                  p_instr     FOR zsdt0066-instrucao NO INTERVALS MODIF ID c3,
                  p_data      FOR zsdt0053-data_atual NO-EXTENSION MODIF ID c2, "##
                  p_matnr     FOR zsdt0053-matnr MODIF ID c2,
                  p_dtins     FOR zsdt0045-data_instr MODIF ID c3,
                  p_safra     FOR zsdt0045-safra MODIF ID c3,
                  l_dtfor     FOR j_1bnfdoc-docdat MODIF ID c6,
                  p_dtfor     FOR j_1bnfdoc-docdat MODIF ID c2.

  SELECT-OPTIONS: p_vkorg4     FOR zsdt0051-vkorg NO INTERVALS MODIF ID c4, "## "Incluido opção seleção multiplas / USER STORY 101490 / AOENNING
                  p_safra4     FOR zsdt0045-safra MODIF ID c4,
                  p_kunnr4     FOR zsdt0051-kunnr MODIF ID c4,
                  p_instr4     FOR zsdt0066-instrucao NO INTERVALS MODIF ID c4,
                  p_dtins4     FOR zsdt0045-data_instr MODIF ID c4.

*-CS2023000189-31.05.2023-#108695-JT-inicio
  SELECT-OPTIONS: s_werks5 FOR mchb-werks      MODIF ID c10, "NO INTERVALS NO-EXTENSION      MODIF ID c10,
                  s_lgort5 FOR mchb-lgort      MODIF ID c10, "NO INTERVALS NO-EXTENSION      MODIF ID c10,
                  s_charg5 FOR mchb-charg      MODIF ID c10, "NO INTERVALS NO-EXTENSION      MODIF ID c10,
                  s_cdsai5 FOR zppt0002-cd_sai MODIF ID c10. "NO INTERVALS NO-EXTENSION      MODIF ID c10.
*-CS2023000189-31.05.2023-#108695-JT-fim

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_lote AS CHECKBOX USER-COMMAND lote MODIF ID c5.
    SELECTION-SCREEN COMMENT 2(50) TEXT-003 FOR FIELD r_lote MODIF ID c5.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_hist AS CHECKBOX USER-COMMAND hist  MODIF ID c5.
    SELECTION-SCREEN COMMENT 2(50) TEXT-004 FOR FIELD r_hist  MODIF ID c5.
  SELECTION-SCREEN END OF LINE.

  PARAMETER: r_saldo RADIOBUTTON GROUP g2             MODIF ID c4,
             r_todos RADIOBUTTON GROUP g2 DEFAULT 'X' MODIF ID c4.

SELECTION-SCREEN: END OF BLOCK b2.

*-CS2022000332-#78927-17.06.2022-JT-inicio
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-010.
  PARAMETER: r_aemba RADIOBUTTON GROUP g3 DEFAULT 'X' MODIF ID c7,
             r_embdo RADIOBUTTON GROUP g3             MODIF ID c7.
SELECTION-SCREEN: END OF BLOCK b4.
*-CS2022000332-#78927-17.06.2022-JT-fim

*-CS2022000332-#79606-17.06.2022-JT-inicio
SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-011.
  PARAMETER: r_flote RADIOBUTTON GROUP g4 DEFAULT 'X' MODIF ID c8,
             r_venex RADIOBUTTON GROUP g4             MODIF ID c8.
SELECTION-SCREEN: END OF BLOCK b5.
*-CS2022000332-#79606-17.06.2022-JT-fim

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_zsdt0051.
         INCLUDE STRUCTURE zsdt0051.
TYPES:   objek TYPE zsdt0045-objek.
TYPES: instrucao TYPE zsdt0045-instrucao,
       END OF ty_zsdt0051.

TYPES: BEGIN OF ty_zsdt0053.
         INCLUDE STRUCTURE zsdt0053.
TYPES:   lifnr TYPE lfa1-lifnr,
       END OF ty_zsdt0053.

TYPES: BEGIN OF ty_zsdt0045.
         INCLUDE STRUCTURE zsdt0045.
TYPES:   nro_sol_ov  TYPE zsdt0051-nro_sol_ov,
         desc_filial TYPE name1,
       END OF ty_zsdt0045.

TYPES: BEGIN OF ty_zsdt0066.
         INCLUDE STRUCTURE zsdt0066.
TYPES:   lifnr       TYPE lfa1-lifnr,
         objek       TYPE zsdt0045-objek,
         desc_filial TYPE name1,
       END OF ty_zsdt0066.

TYPES:BEGIN OF ty_vbfa,
        vbelv   TYPE vbfa-vbelv,
        vbeln   TYPE vbfa-vbeln,
        rfmng   TYPE vbfa-rfmng,
        refkey  TYPE j_1bnflin-refkey,
        vbtyp_n TYPE vbfa-vbtyp_n,
        vbtyp_v TYPE vbfa-vbtyp_v,
      END OF ty_vbfa,

      BEGIN OF ty_vtfa,
        vbelv TYPE vtfa-vbelv,
        vbeln TYPE vtfa-vbeln,
      END OF ty_vtfa,

      BEGIN OF ty_vfkp,
        fknum TYPE vfkp-fknum,
        kzwi1 TYPE vfkp-kzwi1,
        kzwi2 TYPE vfkp-kzwi2,
      END OF ty_vfkp,

      BEGIN OF ty_vttp,
        vbeln TYPE vttp-vbeln,
        tknum TYPE vttp-tknum,
      END OF ty_vttp,

      BEGIN OF ty_vttk,
        tknum TYPE vttk-tknum,
        tdlnr TYPE vttk-tdlnr,
      END OF ty_vttk,

      BEGIN OF ty_j_1bnflin,
        refkey TYPE j_1bnflin-refkey,
        docnum TYPE j_1bnflin-docnum,
        netwr  TYPE j_1bnflin-netwr,
        menge  TYPE j_1bnflin-menge,
      END OF ty_j_1bnflin,

      BEGIN OF ty_j_1bnfdoc,
        docnum TYPE j_1bnfdoc-docnum,
        nfenum TYPE j_1bnfdoc-nfenum,
        docdat TYPE j_1bnfdoc-docdat,
        brgew  TYPE j_1bnfdoc-brgew,
        ntgew  TYPE j_1bnfdoc-ntgew,
      END OF ty_j_1bnfdoc,

      BEGIN OF ty_doc_lin,
        branch     TYPE j_1bbranch-branch,
        refkey     TYPE j_1bnflin-refkey,
        docnum     TYPE j_1bnflin-docnum,
        netwr      TYPE j_1bnflin-netwr,
        menge      TYPE j_1bnflin-menge,
        nfenum     TYPE j_1bnfdoc-nfenum,
        docdat     TYPE j_1bnfdoc-docdat,
        brgew      TYPE j_1bnfdoc-brgew,
        ntgew      TYPE j_1bnfdoc-ntgew,
        cancel     TYPE j_1bnfdoc-cancel,
        docref     TYPE j_1bnfdoc-docref,
        vbeln      TYPE vbfa-vbeln,
        lifnr_emit TYPE lfa1-lifnr,
      END OF ty_doc_lin
      ,

      BEGIN OF ty_lfa1_emit,
        lifnr TYPE lfa1-lifnr,
        name4 TYPE lfa1-name4,
      END OF ty_lfa1_emit
      ,

      BEGIN OF ty_kna1,
        kunnr TYPE kna1-kunnr,
        mcod3 TYPE kna1-mcod3,
        name1 TYPE kna1-name1,
      END OF ty_kna1,

      BEGIN OF ty_lfa1,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
        stcd1 TYPE lfa1-stcd1,
      END OF ty_lfa1,

      BEGIN OF ty_bloco,
        lgort TYPE zmmt0008-lgort,
        charg TYPE zmmt0008-charg,
        tipo  TYPE zppt0004-tipo,
        conta TYPE i,
        menge TYPE zmmt0008-menge,
      END OF ty_bloco,

      BEGIN OF ty_makt,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
      END OF ty_makt,

      BEGIN OF ty_vbap,
        vbeln TYPE vbap-vbeln,
        netpr TYPE vbap-netpr,
      END OF ty_vbap,

      BEGIN OF ty_bl,
        werks    TYPE zmmt0008-werks,
        lgort    TYPE zmmt0008-lgort,
        vbeln_vf TYPE zmmt0008-vbeln_vf,
        charg    TYPE zmmt0008-charg,
        tipo     TYPE zppt0004-tipo,
      END OF ty_bl,

      BEGIN OF ty_saida,
        nro_sol_ov         TYPE zsdt0051-nro_sol_ov,
        werks              TYPE zsdt0053-werks     , "CENTRO FAZ
        name1              TYPE t001w-name1        , "FAZENDA
        stcd1(18)          TYPE c,  " TYPE LFA1-STCD1,
        data               TYPE zsdt0053-data_atual, "DATA EXP
        kunnr              TYPE kna1-kunnr         , "COD.CLIENTE
        name1_kun          TYPE kna1-name1         , "CLIENTE
        contrato           TYPE zsdt0045-contrato,
        instrucao          TYPE zsdt0045-instrucao,
        qtd_ctners         TYPE zsdt0045-qtd_ctners,
        ano(4)                             ,
        mes(15)                            , " por extenso
        matnr_e            TYPE zsdt0053-matnr     , "Para Exportação
        maktx              TYPE makt-maktx,
        volum_exp          TYPE zsdt0053-volum     , "FDS EXP
        zmeng_exp          TYPE zsdt0053-zmeng     , "PESO EXP
        vlr_usd            TYPE zsdt0053-vlrtot    , "VALOR EXP USD
        vlr_brl            TYPE zsdt0053-vlrtot    , "VALOR EXP BRL
        kursf              TYPE zsdt0053-kursf     , "TAXA CAMBIO EXP
        vbeln_exp          TYPE zsdt0053-vbeln     , "ORDEM VENDA EXP
        docdat             TYPE j_1bnfdoc-docdat   , "DATA EXP
        nfnum_exp          TYPE j_1bnfdoc-nfnum    , "NOTA FISCAL EXP

        lgort(60), "      TYPE ZMMT0008-LGORT     , "Nº BLOCO
        posnr66            TYPE zsdt0066-posnr,
        volum              TYPE zsdt0066-volum     , "FDS FRM LOTE
        zmeng_lote         TYPE zsdt0066-zmeng     , "PESO FRM LOTE
        nfnum_lote         TYPE j_1bnfdoc-nfnum    , "NOTA FISCAL FRM LOTE
        data_lote          TYPE zsdt0066-data_atual, "DATA FRM LOTE
        vbeln_lote         TYPE zsdt0066-vbeln     , "ORDEM VENDA FRM LOTE
        vlrtot_lote        TYPE zsdt0066-vlrtot    , "VALOR TOTAL FRM LOTE
        netwr              TYPE j_1bnflin-netwr,
        libra_to           TYPE zsdt0066-libra_to,
        usd_to             TYPE zsdt0066-usd_to,

        kzwi1              TYPE vfkp-kzwi1         , "VALOR FRETE LIQUIDO
        kzwi1_totl         TYPE vfkp-kzwi1         , "VALOR TOTAL FRETE LIQUIDO
        kzwi1_val          TYPE vfkp-kzwi1         , "VALOR FRETE
        kzwi1_tot          TYPE vfkp-kzwi1         , "TOTAL FRETE
        placa_cav          TYPE zsdt0001-placa_cav , "PLACA TRANSPORTE
        transp             TYPE lfa1-name1         , "TRANSPORTADORA
        lentrega           TYPE zsdt0066-lentrega  , "LOCAL ENTREGA
        name1_arm          TYPE kna1-name1         , "Descrição ARMAZEM
        mcod3              TYPE kna1-mcod3         , "CIDADE DESTINO

        vlr_rec_u          TYPE zsdt0053-vlrtot    , "VALOR REC USD ( 0 )
        vlr_rec_b          TYPE zsdt0053-vlrtot    , "VALOR REC BRL ( 0 )
        menge_rec          TYPE zmmt0008-menge     , "PESO RECEBIDO ( 0 )
*        VOLUM_DIF     TYPE ZSDT0066-VOLUM     , "DIFERENÇA FDS
*        VOLUM_REC     TYPE ZSDT0066-VOLUM     , "DIFERENÇA PESO A REC
*        VLR_DIF_U     TYPE ZSDT0053-VLRTOT    , "DIFERENÇA USD A REC
*        VLR_DIF_B     TYPE ZSDT0053-VLRTOT    , "DIFERENÇA BRL A REC
        vbeln              TYPE vbfa-vbeln,
        werks66            TYPE zsdt0066-werks,
        contador           TYPE c LENGTH 120,
        flg_agrp(1),

*** Marcos Faneli -> Ch.128554
*        FDS_INST      TYPE ZSDT0045-QUANTIDADE, "FDS Instrução
        fds_inst           TYPE i,
        saldo_fds          TYPE i,                   "Saldo FDS Instr
        peso_nf_exp        TYPE j_1bnfdoc-brgew    , "Peso NF Exp
        peso_nf_frm        TYPE j_1bnfdoc-ntgew    , "Peso NF FRM
        peso_nf_bruto      TYPE j_1bnfdoc-brgew    , "Peso NF bruto "138693 - Adicionar Peso nf bruto
        peso_origem        TYPE char30,
        t_peso_origem      TYPE zsdt0053-vlrtot,
        tipo               TYPE char10,
        dif_pnf_pto        TYPE j_1bnfdoc-brgew,

        hist               TYPE c,
        color(4)           TYPE c,
*** LG CS2017001463
        lgort2             TYPE zsdt0066-lgort,
        despachante        TYPE lfa1-name1,
        corretor           TYPE lfa1-name1,
        status             TYPE string,
        local_carregamento TYPE name1,
        safra              TYPE zsdt0045-safra,
      END OF ty_saida,

      BEGIN OF ty_rel_qtd_fardos,
        local_carregamento TYPE name1,
        prazo_final_porto  TYPE sy-datum,
        transportadora     TYPE name1,
        instrucao          TYPE zsded030,
        qtd_fardos         TYPE int4,
        qtd_carregados     TYPE int4,
        saldo              TYPE int4,
        peso_max           TYPE gsgew,
        rfmng              TYPE rfmng,
        saldo_peso_rfmng   TYPE rfmng,
      END OF ty_rel_qtd_fardos.

*-CS2023000189-31.05.2023-#108695-JT-inicio
TYPES: BEGIN OF type_mchb,
         matnr             TYPE mchb-matnr,
         werks             TYPE mchb-werks,
         lgort             TYPE mchb-lgort,
         charg             TYPE mchb-charg,
         clabs             TYPE mchb-clabs,
         cspem             TYPE mchb-cspem,
         maktx             TYPE makt-maktx,
         lgortr            TYPE mchb-lgort,
         chargr            TYPE mchb-charg,
         matnc             TYPE mchb-matnr,
         cd_sai            TYPE zppt0002-cd_sai,
         icon              TYPE char05,
         log               TYPE char40,
         acts              TYPE string,
         possuiacts        TYPE boole_d,
         status            TYPE string,
         takeupmarcadoacts TYPE boole_d,
         safra             TYPE mchb-charg,
       END   OF type_mchb.

TYPES: BEGIN OF ty_zsdt0166.
         INCLUDE TYPE zsdt0166.
TYPES:   lgort TYPE mchb-lgort.
TYPES: END   OF ty_zsdt0166.

TYPES: BEGIN OF ty_selec,
         werks  TYPE mchb-werks,
         lgort  TYPE mchb-lgort,
         safra  TYPE zppt0002-cd_safra,
         cd_sai TYPE zppt0002-cd_sai.
TYPES: END   OF ty_selec.
*-CS2023000189-31.05.2023-#108695-JT-fim

DATA: BEGIN OF ta_saida OCCURS 0,
        werks     TYPE zsdt0053-werks     , "CENTRO FAZ
        name1     TYPE t001w-name1        , "FAZENDA
        data      TYPE zsdt0053-data_atual, "DATA EXP
        kunnr     TYPE zsdt0051-kunnr     , "CLIENTE
        contrato  TYPE zsdt0045-contrato,
        instrucao TYPE zsdt0045-instrucao,
      END OF ta_saida.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: it_zsdt0051        TYPE TABLE OF ty_zsdt0051,
      it_zsdt0053        TYPE TABLE OF ty_zsdt0053,
      it_zsdt0045        TYPE TABLE OF ty_zsdt0045,
      it_0045            TYPE TABLE OF ty_zsdt0045,
      it_0045_aux        TYPE TABLE OF ty_zsdt0045,
      it_zsdt0066        TYPE TABLE OF ty_zsdt0066,
      it_zsdt0328        TYPE TABLE OF zsdt0328, " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
      it_0045_inst       TYPE TABLE OF ty_zsdt0045, " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
*
      it_zsdt0166        TYPE TABLE OF ty_zsdt0166,     "*-CS2023000189-31.05.2023-#108695-JT
      it_mchb_grp        TYPE TABLE OF type_mchb,       "*-CS2023000189-31.05.2023-#108695-JT
      it_mchb            TYPE TABLE OF type_mchb,       "*-CS2023000189-31.05.2023-#108695-JT
      sl_mchb            TYPE type_mchb,                "*-CS2023000189-31.05.2023-#108695-JT
      sl_mchb_grp        TYPE type_mchb,                "*-CS2023000189-31.05.2023-#108695-JT
      it_zppt0002_grp    TYPE TABLE OF zppt0002,        "*-CS2023000189-31.05.2023-#108695-JT
      it_zppt0002        TYPE TABLE OF zppt0002,        "*-CS2023000189-31.05.2023-#108695-JT
      sl_zppt0002        TYPE zppt0002,        "*-CS2023000189-31.05.2023-#108695-JT
      sl_zppt0002_grp    TYPE zppt0002,        "*-CS2023000189-31.05.2023-#108695-JT
      r_charg            TYPE RANGE OF zppt0002-charg,  "*-CS2023000189-31.05.2023-#108695-JT
      vg_werk            TYPE werks_d,                  "*-CS2023000189-31.05.2023-#108695-JT
      vg_cd_sai          TYPE char20,                   "*-CS2023000189-31.05.2023-#108695-JT
      vg_lote            TYPE char04,                   "*-CS2023000189-31.05.2023-#108695-JT
      vg_lgort           TYPE lgort_d,                  "*-CS2023000189-31.05.2023-#108695-JT
      vl_index           TYPE p,                        "*-CS2023000189-31.05.2023-#108695-JT
      v_trace            TYPE char1 VALUE 0,            "*-CS2023000189-31.05.2023-#108695-JT
      v_letrac           TYPE char1 VALUE 0,            "*-CS2023000189-31.05.2023-#108695-JT
      l_erro             TYPE char1,                    "*-CS2023000189-31.05.2023-#108695-JT
      r_data             TYPE zde_acts_tracecotton_t,   "*-CS2023000189-31.05.2023-#108695-JT
      r_data_ger         TYPE zde_acts_tracecotton_t,   "*-CS2023000189-31.05.2023-#108695-JT
      t_selec            TYPE TABLE OF ty_selec,        "*-CS2023000189-31.05.2023-#108695-JT
      w_selec            TYPE ty_selec,                 "*-CS2023000189-31.05.2023-#108695-JT
*
      it_t001w           TYPE TABLE OF t001w,
      it_zmmt0008        TYPE TABLE OF zmmt0008,
      it_zmmt0008_a      TYPE TABLE OF zmmt0008,
      it_zmmt0008_x      TYPE TABLE OF zmmt0008,
      it_zmmt0008_bl     TYPE TABLE OF zmmt0008,
      it_zmmt0008_bl_2   TYPE TABLE OF zmmt0008,
      it_zsdt_depara_cen TYPE TABLE OF zsdt_depara_cen,
      it_vbfa            TYPE TABLE OF ty_vbfa,
      it_vbfa_66         TYPE TABLE OF ty_vbfa,
      it_vbfa_8          TYPE TABLE OF ty_vbfa,
      it_vbfa_j          TYPE TABLE OF ty_vbfa,
      it_vtfa            TYPE TABLE OF ty_vtfa,
      it_vbap            TYPE TABLE OF ty_vbap,
      it_vfkp            TYPE TABLE OF ty_vfkp,
      it_vttp            TYPE TABLE OF ty_vttp,
      it_vttk            TYPE TABLE OF ty_vttk,
      it_j_1bnflin       TYPE TABLE OF ty_j_1bnflin,
      it_j_1bnfdoc       TYPE TABLE OF ty_j_1bnfdoc,
      it_doc_lin         TYPE TABLE OF ty_doc_lin WITH DEFAULT KEY,
      it_lfa1_emit       TYPE TABLE OF ty_lfa1_emit,
      it_zsdt0176        TYPE TABLE OF zsdt0176, "  De para Filial SAP com a Trace Cotton
      it_docnum          TYPE TABLE OF ty_doc_lin WITH DEFAULT KEY,
      it_zsdt0001        TYPE TABLE OF zsdt0001,
      it_kna1            TYPE TABLE OF ty_kna1,
      it_lfa1            TYPE TABLE OF ty_lfa1,
      tg_bloco           TYPE TABLE OF ty_bloco,
      it_makt            TYPE TABLE OF ty_makt,
      it_saida           TYPE TABLE OF ty_saida,
      it_0051_a          TYPE TABLE OF ty_zsdt0051,
      it_0051_x          TYPE TABLE OF ty_zsdt0051,
      saida              TYPE TABLE OF ty_saida,
      t_bl               TYPE TABLE OF ty_bl WITH DEFAULT KEY,
      it_bl              TYPE TABLE OF ty_bl WITH HEADER LINE,
      it_rel             TYPE TABLE OF ty_rel_qtd_fardos,
      wa_rel             TYPE ty_rel_qtd_fardos.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_zsdt0051        TYPE ty_zsdt0051,
      wa_zsdt0053        TYPE ty_zsdt0053,
      wa_zsdt0045        TYPE ty_zsdt0045,
      wa_zsdt0066        TYPE ty_zsdt0066,
      wa_zsdt0166        TYPE ty_zsdt0166,   "*-CS2023000189-31.05.2023-#108695-JT
      wa_zsdt0328        TYPE zsdt0328, " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
      wa_t001w           TYPE t001w,
      wa_zmmt0008        TYPE zmmt0008,
      wa_zmmt0008_2      TYPE zmmt0008,
      wa_zsdt_depara_cen TYPE zsdt_depara_cen,
      wa_vbfa            TYPE ty_vbfa,
      wa_vbfa_66         TYPE ty_vbfa,
      wa_vbfa_8          TYPE ty_vbfa,
      wa_vbfa_j          TYPE ty_vbfa,
      wa_vtfa            TYPE ty_vtfa,
      wa_vbap            TYPE ty_vbap,
      wa_vfkp            TYPE ty_vfkp,
      wa_vttp            TYPE ty_vttp,
      wa_vttk            TYPE ty_vttk,
      wa_j_1bnflin       TYPE ty_j_1bnflin,
      wa_j_1bnfdoc       TYPE ty_j_1bnfdoc,
      wa_zsdt0001        TYPE zsdt0001,
      wa_kna1            TYPE ty_kna1,
      wa_lfa1            TYPE ty_lfa1,
      wg_bloco           TYPE ty_bloco,
      wa_makt            TYPE ty_makt,
      wa_saida           TYPE ty_saida,
      wa_saida2          TYPE ty_saida.

DATA wvbeln TYPE RANGE OF vbfa-vbeln WITH HEADER LINE.
DATA instrucao TYPE RANGE OF zsdt0045-instrucao WITH HEADER LINE.

*-CS2022000332-#78919-16.06.2022-JT-inicio
DATA: r_expo        TYPE char1,
      l_param_espec TYPE zsdt0051-param_espec,
      t_0066        TYPE TABLE OF zsdt0066,
      t_0053        TYPE TABLE OF zsdt0053.
*-CS2022000332-#78919-16.06.2022-JT-fim

*----------------------------------------------------------------------*
* TABELAS FARDO A FARDO - CS2018001961 -Sara Oikawa - 05/2020
*----------------------------------------------------------------------*
* Tabelas para Relatório Fardo a Fardo

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_fardo,
         kunnr      TYPE  kunnr,
         name1      TYPE  name1,
         inst_fml   TYPE  zsded030,
         inst_exp	  TYPE  zsded030,
         lote       TYPE  charg_d,
         qt_fds_fml TYPE  zsded029,
         qt_fds_exp TYPE  zsded029,
         qt_fds_sld TYPE  zsded029,
         peso_fml   TYPE  brgew_ap,
         peso_exp	  TYPE  brgew_ap,
         peso_sld	  TYPE  brgew_ap,
         safra      TYPE lgort_d,
         quantidade TYPE zsdt0045-quantidade,  "*-CS2022000332-#79606-17.06.2022-JT-inicio
         volum      TYPE zsdt0213-volum,       "*-CS2022000332-#79606-17.06.2022-JT-inicio
         btgew      TYPE zsdt0045-btgew,      "*-CS2022000332-#79606-17.06.2022-JT-inicio
       END OF ty_fardo.

TYPES: BEGIN OF ty_instr,
         zseq_inst   TYPE zseq_inst,
         objek       TYPE objnum,
         objecttable TYPE tabelle,
         bukrs       TYPE	bukrs,
         instrucao   TYPE	zsded030,
         data_instr  TYPE dats,
         kunnr       TYPE kunag,
         nro_sol_ov  TYPE zsded013,
         safra       TYPE lgort_d,
         quantidade  TYPE zsdt0045-quantidade, "*-CS2022000332-#79606-17.06.2022-JT-inicio
         btgew       TYPE zsdt0045-btgew,      "*-CS2022000332-#79606-17.06.2022-JT-inicio
         charg       TYPE zsdt0045-charg,      "*-CS2022000332-#79606-17.06.2022-JT-inicio
       END OF ty_instr.

TYPES: BEGIN OF ty_fmlote,
         nro_sol_ov	TYPE zsded013,
         posnr      TYPE posnr_va,
         instrucao  TYPE zsded030,
         werks      TYPE werks_d,
         volum      TYPE zsded029,
         vbeln      TYPE vbeln,
         charg_ori  TYPE charg_d,
         zmeng      TYPE zsdt0066-zmeng,
         processado TYPE char1,
       END OF ty_fmlote.

TYPES: BEGIN OF ty_export,
         inst_fml   TYPE zsded030,
         inst_exp   TYPE zsded030,
         nro_sol_ov TYPE zsded013,
         posnr      TYPE posnr_va,
         vbeln      TYPE vbeln,
         lgort      TYPE lgort_d,
         volum      TYPE zsded029,
         status     TYPE zsdt0213-status,
         werks      TYPE werks_d,
         charg      TYPE zsdt0053-charg,
       END OF ty_export.

TYPES:BEGIN OF ty_vbfa_fard,
        vbelv TYPE vbfa-vbelv,
        vbeln TYPE j_1bnflin-refkey,
      END OF ty_vbfa_fard.

TYPES: BEGIN OF ty_nflote,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         refkey TYPE vbeln_vf,
         refitm TYPE j_1bnflin-refitm,
         menge  TYPE j_1bnflin-menge,
         nfenum TYPE j_1bnfdoc-nfenum,
         docdat TYPE j_1bnfdoc-docdat,
         cancel TYPE j_1bnfdoc-cancel,
         docsta TYPE j_1bnfe_active-docsta,
         scssta TYPE j_1bnfe_active-scssta,
         brgew  TYPE vbrp-brgew,
         aubel  TYPE vbeln_va,
         aupos  TYPE posnr_va,
       END OF ty_nflote.

TYPES: BEGIN OF ty_sum_0008,
         inst_fml   TYPE zsded030,
         lote       TYPE lgort_d,
         vbeln      TYPE vbeln,
         vbeln_vf  	TYPE vbeln_vf,
         menge      TYPE menge_d,
         qt_fds_fml TYPE zsded029,
         peso       TYPE brgew_ap,
         peso_7dec  TYPE zquant17_7,
       END OF ty_sum_0008.

TYPES: BEGIN OF ty_sum_final,
         inst_fml TYPE zsded030,
         inst_exp TYPE zsded030,
         lote     TYPE lgort_d,
         qt_fds   TYPE zsded029,
         peso     TYPE brgew_ap,
         peso_tot TYPE brgew_ap,
         btgew    TYPE zsdt0045-btgew,
       END OF ty_sum_final.

*----------------------------------------------------------------------*
* TABELAS / WORK AREAS
*----------------------------------------------------------------------*

DATA: i_fieldcat       TYPE lvc_t_fcat,
      i_fieldcat1      TYPE lvc_t_fcat,
      i_sort           TYPE lvc_t_sort,

      it_instr         TYPE TABLE OF ty_instr,
      wa_instr         TYPE ty_instr,

      it_fmlote        TYPE TABLE OF ty_fmlote,
      wa_fmlote        TYPE ty_fmlote,

      it_export        TYPE TABLE OF ty_export,
      wa_export        TYPE ty_export,

      it_vbfa_fard     TYPE TABLE OF ty_vbfa_fard,
      wa_vbfa_fard     TYPE  ty_vbfa_fard,

      it_nflote        TYPE TABLE OF ty_nflote,
      it_nflote_exp    TYPE TABLE OF ty_nflote,
      wa_nflote        TYPE  ty_nflote,

      it_sum_lote      TYPE TABLE OF  ty_sum_0008,
      it_sum_fat       TYPE TABLE OF  ty_sum_0008,

      it_sum_final     TYPE TABLE OF  ty_sum_final,
      wa_sum_final     TYPE ty_sum_final,

      it_sum_instr     TYPE TABLE OF  ty_sum_final,
      it_sum_instr_exp TYPE TABLE OF  ty_sum_final,
      wa_sum_instr     TYPE ty_sum_final,

      it_sum_lotexp    TYPE TABLE OF  ty_sum_final,
      wa_sum_lotexp    TYPE ty_sum_final,

      wa_sum_0008      TYPE ty_sum_0008,

      it_fardo1        TYPE TABLE OF ty_fardo,
      it_fardo2        TYPE TABLE OF ty_fardo,

      wa_fardo1        TYPE ty_fardo,
      wa_fardo2        TYPE ty_fardo.

*----------------------------------------------------------------------*
* FIELD SYMBOLS
*----------------------------------------------------------------------*

FIELD-SYMBOLS: <fs_sum_fat>   TYPE ty_sum_0008,
               <fs_sum_lote>  TYPE ty_sum_0008,
               <fs_sum_final> TYPE ty_sum_final,
               <fs_fardo1>    TYPE ty_fardo.


*----------------------------------------------------------------------*
* INCLUDE ALV FARDO A FARDO  - CS2018001961 Sara Oikawa 05/2020
*----------------------------------------------------------------------*
* The definition and implementation of the event reciever class
INCLUDE zsdr0031_fardo_alv_class_0300.


************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: editcontainer   TYPE REF TO cl_gui_custom_container,
      cl_container    TYPE REF TO cl_gui_custom_container,
      editor          TYPE REF TO cl_gui_textedit,
      cl_container_95 TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id   TYPE REF TO cl_dd_document,
      cl_grid         TYPE REF TO cl_gui_alv_grid,
      wa_stable       TYPE lvc_s_stbl,
      gt_sort         TYPE lvc_t_sort,         " Sorting Table
      bl_sort         TYPE lvc_t_sort,         " Sorting Table
      it_fieldcat     TYPE lvc_t_fcat,
      wa_afield       TYPE lvc_s_fcat,
      wa_layout       TYPE lvc_s_layo,

      grid1           TYPE REF TO cl_gui_alv_grid,
      obg_conteiner   TYPE REF TO cl_gui_custom_container,
      t_fieldcatalog  TYPE lvc_t_fcat,
      w_fieldcatalog  TYPE lvc_s_fcat.

DATA r_tree LIKE bsid-umskz VALUE ''.

DATA: cnpj_for(18)       TYPE c,
      cnpj_formatado(18) TYPE c.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x         TYPE c VALUE 'X',
           c_exit(4)   TYPE c VALUE 'EXIT',
           c_back(4)   TYPE c VALUE 'BACK',
           c_cancel(6) TYPE c VALUE 'CANCEL'.

CLASS zcl_monta_dados DEFINITION.
  PUBLIC SECTION.

    TYPES: tt_docnum              TYPE RANGE OF j_1bnfdoc-docnum.

    DATA: at_lgort(30),
          at_peso_origem(30),
          at_peso_t_origem_s(40),
          at_peso_t_origem       TYPE zsdt0053-dmbtr,
          at_tipo(10),
          at_instrucao           TYPE RANGE OF zsdt0045-instrucao,
          at_docnum              TYPE RANGE OF j_1bnfdoc-docnum,
          at_refkey              TYPE RANGE OF j_1bnflin-refkey,
          at_zmeng_lote          TYPE dzmeng,
          at_vlrtot_lote         TYPE dmbtr.

    METHODS:
      get_t001w
        IMPORTING input         TYPE werks_ext
        RETURNING VALUE(return) TYPE name1,
      get_lfa1
        IMPORTING input         TYPE lifnr
        RETURNING VALUE(return) TYPE name1_gp,
      get_kna1
        IMPORTING input         TYPE kunnr
        RETURNING VALUE(return) TYPE kna1,
      get_mes
        IMPORTING input         TYPE j_1bdocdat
        RETURNING VALUE(return) TYPE fcltx,
      get_makt
        IMPORTING input         TYPE matnr
        RETURNING VALUE(return) TYPE maktx,
      get_transp
        IMPORTING input         TYPE vbeln_von
        RETURNING VALUE(return) TYPE name1_gp,
      get_placa_cav
        IMPORTING input         TYPE vbeln_von
                  input1        TYPE vbeln_von
        RETURNING VALUE(return) TYPE zplaca,
      get_contador
        IMPORTING input         TYPE werks_ext
                  input1        TYPE vbeln_von
        RETURNING VALUE(return) TYPE char120,
      get_tipo
        IMPORTING input         TYPE charg_d
        RETURNING VALUE(return) TYPE char10,
*-CS2022000332-#83055-05.08.2022-JT-inicio
      get_tipo_alterna
        IMPORTING input         TYPE charg_d
        RETURNING VALUE(return) TYPE char10,
*-CS2022000332-#83055-05.08.2022-JT-fim
      set_saida_nf
        IMPORTING input TYPE char1 OPTIONAL,
      set_range_instrucao,
      get_dados_historico,
      get_seleciona_dados_nf,
      get_descricao_geral,
      dados_transporte,
      modify_vbfa,
      agrupa_0045,
      get_zsdt0008,
      del_duplicado,
      del_doc_lin,
      clear,
      get_seq
        RETURNING VALUE(return) TYPE i,
      get_quantidade
        IMPORTING input         TYPE ty_zsdt0045
        RETURNING VALUE(return) TYPE numc10,
      get_qtd_ctners
        IMPORTING input         TYPE ty_zsdt0045
        RETURNING VALUE(return) TYPE zsdt0045-qtd_ctners,
      set_primeiro
        IMPORTING input         TYPE ty_zsdt0066
        RETURNING VALUE(return) TYPE zsded029,
      select_qtd_fardos,
      processa_qtd_fardos,
      exibe_dados,
      get_name1_werks IMPORTING input         TYPE ty_zsdt0045
                      RETURNING VALUE(return) TYPE name1,
      get_transportadora IMPORTING input         TYPE lifnr
                         RETURNING VALUE(return) TYPE name1,
      get_peso_carregado IMPORTING input         TYPE vbeln
                         RETURNING VALUE(return) TYPE rfmng,
      get_qtd_fardos  IMPORTING input         TYPE numc10
                      RETURNING VALUE(return) TYPE numc10,
      get_qtd_fardos_carregados IMPORTING input         TYPE ty_zsdt0066
                                          input1        TYPE name1
                                RETURNING VALUE(return) TYPE int4.

  PRIVATE SECTION.

    DATA: at_qtd_fardos TYPE numc10.

ENDCLASS.

DATA(obj_dados) = NEW zcl_monta_dados( ).

CLASS zcl_monta_dados IMPLEMENTATION.

  METHOD get_t001w.
    CLEAR return.
    TRY .
        return = it_t001w[ werks = input ]-name1.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_lfa1.
    CLEAR return.
    TRY .
        return = it_lfa1[ lifnr = input ]-stcd1.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_mes.
    CLEAR return.
    SELECT SINGLE ltx FROM t247 INTO return WHERE spras EQ sy-langu AND mnr EQ input+4(2).
  ENDMETHOD.

  METHOD get_kna1.
    CLEAR return.
    TRY .
        return = CORRESPONDING #( it_kna1[ kunnr =  input ] ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_makt.
    CLEAR return.
    return = it_makt[ matnr = input ]-maktx.
  ENDMETHOD.

  METHOD get_transp.
    CLEAR return.
    TRY.
        return = it_lfa1[ lifnr = it_vttk[ tknum = it_vttp[ vbeln = input ]-tknum ]-tdlnr ]-name1.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_placa_cav.
    CLEAR return.
    TRY.
        return = it_zsdt0001[ vbeln   = input doc_rem = input1  ]-placa_cav.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_contador.

    CLEAR: return, at_peso_origem, at_lgort, at_tipo, at_peso_t_origem.
    FREE: it_bl, t_bl.

    DATA: vcont_lgort     TYPE i,
          vcont_qtd_lgort TYPE zsdt0053-dmbtr,
          vcont_lgort_s   TYPE c LENGTH 4,
          tabix           TYPE sy-tabix,
          tabix_vbfa      TYPE sy-tabix,
          lv_sum_fds      TYPE i.

    LOOP AT it_zmmt0008_bl INTO wa_zmmt0008 WHERE werks EQ input
                                           AND vbeln_vf EQ input1.

      ADD 1 TO tabix.
      CLEAR: vcont_qtd_lgort.
* Pega a Quantidade do Lote
      vcont_qtd_lgort = REDUCE #( INIT x = vcont_qtd_lgort FOR ls IN it_zmmt0008
                             WHERE ( werks    EQ input AND
                                     vbeln_vf EQ input1 AND
                                     lgort    EQ wa_zmmt0008-lgort )
                                     NEXT x = x + ls-menge ).

* Concatena a Quantidade do Lote
      at_peso_origem = |{ at_peso_origem }, { vcont_qtd_lgort CURRENCY = 'BRL' NUMBER = USER }|.
* Concatena a Lote
      at_lgort       = |{ at_lgort }, { wa_zmmt0008-lgort }|.

      CLEAR: vcont_lgort.
* Prga a Quantidade do Fardos por Lote
      vcont_lgort = REDUCE i( INIT a = 0 FOR ls IN it_zmmt0008_bl_2
                            WHERE ( werks    EQ input AND
                                    vbeln_vf EQ input1 AND
                                    lgort    EQ wa_zmmt0008-lgort )
                                    NEXT a = a + 1 ).

* Concatena a Quantidade do Fardos
      MOVE vcont_lgort TO vcont_lgort_s.
      return = |{ return }, { vcont_lgort_s }|.

* Cria a tabela para Verificar o Tipo
      t_bl = VALUE #( FOR ls IN  it_zmmt0008_bl_2
                                  WHERE ( werks    EQ input AND
                                          vbeln_vf EQ input1 AND
                                          lgort    EQ wa_zmmt0008-lgort )
                                   (
                                      werks    = ls-werks
                                      lgort    = ls-lgort
                                      vbeln_vf = ls-vbeln_vf
                                      charg    = ls-charg
                                      tipo     = ls-tipo_fardo "Projeto Reestruturação Algodao 2024
                                   )
                     ).
      APPEND LINES OF t_bl TO it_bl.
    ENDLOOP.

    SORT it_bl BY werks lgort vbeln_vf charg.

    LOOP AT it_bl ASSIGNING FIELD-SYMBOL(<bl>).
      IF <bl>-tipo IS INITIAL. "Projeto Reestruturação Algodao 2024
        <bl>-tipo = me->get_tipo( <bl>-charg ).
      ENDIF.
    ENDLOOP.

    SORT it_bl BY werks lgort vbeln_vf.
    DELETE it_bl WHERE tipo EQ abap_false.
    DELETE ADJACENT DUPLICATES FROM it_bl COMPARING werks lgort vbeln_vf.

    LOOP AT it_bl INTO DATA(bl).
      at_tipo = |{ at_tipo }, { bl-tipo }|.
    ENDLOOP.

    SHIFT at_tipo LEFT DELETING LEADING ','.
    SHIFT return  LEFT DELETING LEADING ','.
    SHIFT at_lgort LEFT DELETING LEADING ','.
    SHIFT at_peso_origem LEFT DELETING LEADING ','.

    at_peso_t_origem = REDUCE #( INIT b = at_peso_t_origem FOR ls IN it_zmmt0008 WHERE ( werks EQ input AND vbeln_vf EQ input1 ) NEXT b = b + ls-menge ).
    at_peso_t_origem_s = |{ at_peso_t_origem CURRENCY = 'BRL' NUMBER = USER }|.

  ENDMETHOD.

  METHOD get_tipo.

    DATA: wa_0002 TYPE zppt0002.

    CLEAR return.

    SELECT SINGLE verid werks acharg
      FROM zppt0002
      INTO CORRESPONDING FIELDS OF wa_0002
      WHERE acharg EQ input.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE tipo
      FROM zppt0004
      INTO return
     WHERE werks EQ wa_0002-werks
       AND verid EQ wa_0002-verid
       AND tipo NE abap_false.

*-CS2022000332-#83055-05.08.2022-JT-inicio
    IF sy-subrc <> 0.
      return = me->get_tipo_alterna( wa_0002-acharg ).
    ENDIF.
*-CS2022000332-#83055-05.08.2022-JT-fim

  ENDMETHOD.

*-CS2022000332-#83055-05.08.2022-JT-inicio
  METHOD get_tipo_alterna.

    SELECT lgort, vbeln
      INTO @DATA(_0008)
      FROM zmmt0008
        UP TO 1 ROWS
     WHERE charg = @input.
    ENDSELECT.

    CHECK sy-subrc = 0.

    SELECT instrucao
      INTO @DATA(_instrucao)
      FROM zsdt0066
        UP TO 1 ROWS
     WHERE vbeln = @_0008-vbeln.
    ENDSELECT.

    CHECK sy-subrc = 0.

    SELECT tamanho_fardo
      INTO return
      FROM zsdt0045
        UP TO 1 ROWS
     WHERE instrucao = _instrucao
       AND charg     = _0008-lgort.
    ENDSELECT.

  ENDMETHOD.
*-CS2022000332-#83055-05.08.2022-JT-fim

  METHOD set_range_instrucao.
    LOOP AT it_saida INTO wa_saida.
      APPEND VALUE #( sign = 'I'
                      option = 'BT'
                      low = wa_saida-instrucao
                      high = wa_saida-instrucao ) TO at_instrucao.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_saida_nf.

    IF it_doc_lin[] IS NOT INITIAL.

      LOOP AT it_doc_lin ASSIGNING FIELD-SYMBOL(<fs_lin>).
        <fs_lin>-lifnr_emit = <fs_lin>-branch.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_lin>-lifnr_emit
          IMPORTING
            output = <fs_lin>-lifnr_emit.
      ENDLOOP.

      SELECT lifnr name4
        FROM lfa1 INTO TABLE it_lfa1_emit
        FOR ALL ENTRIES IN it_doc_lin
       WHERE lifnr = it_doc_lin-lifnr_emit.

      SORT it_lfa1_emit BY lifnr.

    ENDIF.

    LOOP AT it_doc_lin INTO DATA(wa_doc_lin).

      TRY .
          wa_vbfa = it_vbfa[ vbeln = wa_doc_lin-vbeln ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_vbfa.
          CONTINUE.
      ENDTRY.

      TRY .
          wa_zsdt0066 = it_zsdt0066[ vbeln = wa_vbfa-vbelv ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_zsdt0066.
          CONTINUE.
      ENDTRY.

      TRY.
          wa_zsdt0045 = it_zsdt0045[
                                     objek     = wa_zsdt0066-objek
                                     werks     = wa_zsdt0066-werks
                                     instrucao = wa_zsdt0066-instrucao
                                     matnr     = wa_zsdt0066-matnr
                                   ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_zsdt0045.
          CONTINUE.
      ENDTRY.

      TRY.
          wa_zsdt0051 = it_zsdt0051[ nro_sol_ov = wa_zsdt0045-nro_sol_ov ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_zsdt0051.
          CONTINUE.
      ENDTRY.

      TRY .
          wa_vbfa_j = it_vbfa_j[ vbeln = wa_vbfa-vbeln ].
          wa_vbfa_66-vbeln = wa_vbfa_j-vbelv.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_vbfa_j.
      ENDTRY.

      TRY .
          wa_vfkp = it_vfkp[ fknum = it_vtfa[ vbelv = it_vbfa_8[ vbelv = wa_vbfa_j-vbelv ]-vbeln ]-vbeln ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_vfkp.
      ENDTRY.

      CLEAR: cnpj_for, cnpj_formatado.

      cnpj_for = obj_dados->get_lfa1( wa_zsdt0066-lifnr ).

      CONCATENATE cnpj_for(2) '.' cnpj_for+2(3) '.' cnpj_for+5(3) '/' cnpj_for+8(4) '-' cnpj_for+12(3) INTO cnpj_formatado.
      DATA: i_local_carregamento  TYPE name1,
            i_local_carregamento2 TYPE name1,
            i_saldo               TYPE i,
            i_qtd_carregados      TYPE i,
            i_status(20)          TYPE c,
            desc_despachante1     TYPE lfa1-name1,
            desc_corretor1        TYPE lfa1-name1,
            desc_despachante      TYPE lfa1-name1,
            desc_corretor         TYPE lfa1-name1.

      READ TABLE it_zsdt0176[] ASSIGNING FIELD-SYMBOL(<lfs_zsdt0176>)
        WITH KEY werks = wa_zsdt0066-werks BINARY SEARCH.
      IF ( sy-subrc = 0 ).
        i_local_carregamento2 = <lfs_zsdt0176>-empresa.
      ELSE.
        i_local_carregamento2 = |Configurar centro { wa_zsdt0066-werks } na ZSDT0140|.
      ENDIF.

***      i_local_carregamento2 = SWITCH #( wa_zsdt0066-werks WHEN '1507' THEN 'TUCUNARÉ'
***                                    WHEN '1521' THEN 'ITAMARATI'
***                                    WHEN '1519' THEN 'TANGURO'
***                                    WHEN '1520' THEN 'ÁGUA QUENTE'
***                                    WHEN '1801' THEN 'AGRO SAM'
***                                    ELSE '######'
***                      ).

      IF i_local_carregamento2 EQ 'TUCUNARÉ' AND wa_zsdt0066-lgort NE 'ALGD'.
        i_local_carregamento = 'CHEROKEE'.
      ELSEIF i_local_carregamento2 EQ 'CHEROKEE' AND wa_zsdt0066-lgort EQ 'ALGD'.
        i_local_carregamento = 'TUCUNARÉ'.
      ELSE.
        i_local_carregamento = i_local_carregamento2.
      ENDIF.

      IF wa_doc_lin-docdat >= '20240101'. "Projeto Reestruturação Algodao 2024
        READ TABLE it_lfa1_emit INTO DATA(lwa_lfa1_emit) WITH KEY lifnr = wa_doc_lin-lifnr_emit.
        IF sy-subrc EQ 0.
          i_local_carregamento = lwa_lfa1_emit-name4.
        ENDIF.
      ENDIF.

      DATA(_fardos) = obj_dados->get_qtd_fardos_carregados( input = wa_zsdt0066 input1 = i_local_carregamento ).

      "DATA(_FARDO) = GET_QTD_FARDOS_CARREGADOS( INPUT = WA_ZSDT0066 INPUT1 = WA_REL-LOCAL_CARREGAMENTO ).
      ADD _fardos TO i_qtd_carregados.

      i_saldo = wa_zsdt0045-quantidade - i_qtd_carregados.

      "BREAK-POINT.
      IF i_saldo GT 0.
        i_status = 'EM ANDAMENTO'.
      ELSE.
        i_status = 'FINALIZADO'.
      ENDIF.

      CLEAR: desc_corretor, desc_despachante, desc_corretor1, desc_despachante1.
      SELECT SINGLE name1 INTO desc_corretor1 FROM lfa1 WHERE lifnr = wa_zsdt0051-correto.
      SELECT SINGLE name1 INTO desc_despachante1 FROM lfa1 WHERE lifnr = wa_zsdt0045-cod_despach.

      "SELECT SINGLE NAME1 INTO CORRETOR FROM LFA1 WHERE LIFNR EQ WA_ZSDT0051-CORRETO.

      IF desc_corretor1 IS NOT INITIAL.
        CONCATENATE wa_zsdt0051-correto '-' desc_corretor1 INTO desc_corretor SEPARATED BY space.
      ENDIF.

      IF desc_despachante1 IS NOT INITIAL.
        CONCATENATE wa_zsdt0045-cod_despach '-' desc_despachante1 INTO desc_despachante SEPARATED BY space.
      ENDIF.



      APPEND VALUE #(
                        nro_sol_ov         = wa_zsdt0051-nro_sol_ov
                        werks              = wa_zsdt0066-werks
                        posnr66            = wa_zsdt0066-posnr
                        name1              = me->get_t001w( wa_zsdt0066-werks )
                        stcd1              = cnpj_formatado "ME->GET_LFA1( WA_ZSDT0066-LIFNR )
                        data               = wa_doc_lin-docdat
                        ano                = wa_doc_lin-docdat+0(4)
                        mes                = me->get_mes( wa_doc_lin-docdat )
                        kunnr              = wa_zsdt0051-kunnr
                        name1_kun          = me->get_kna1( wa_zsdt0051-kunnr )-name1
                        contrato           = wa_zsdt0045-contrato
                        instrucao          = wa_zsdt0045-instrucao
                        safra              = wa_zsdt0045-safra
                        qtd_ctners         = me->get_qtd_ctners( wa_zsdt0045 )
                        maktx              = me->get_makt( wa_zsdt0066-matnr )
                        matnr_e            = |{ wa_zsdt0066-matnr ALPHA = OUT }|
                        volum              = me->set_primeiro( wa_zsdt0066 )
                        zmeng_lote         = at_zmeng_lote
                        vlrtot_lote        = at_vlrtot_lote
                        nfnum_lote         = wa_doc_lin-nfenum
                        data_lote          = wa_doc_lin-docdat
                        vbeln_lote         = wa_zsdt0066-vbeln
                        netwr              = wa_doc_lin-netwr
                        libra_to           = wa_zsdt0066-libra_to
                        usd_to             = wa_zsdt0066-usd_to
                        kzwi1_totl         = wa_vfkp-kzwi1
                        kzwi1              = ( wa_vfkp-kzwi1 / wa_vbfa-rfmng ) * 1000
                        kzwi1_tot          = wa_vfkp-kzwi1 + wa_vfkp-kzwi2
                        kzwi1_val          = ( wa_vfkp-kzwi1 / wa_vbfa-rfmng ) * 1000
                        transp             = me->get_transp( wa_vbfa_j-vbelv )
                        placa_cav          = me->get_placa_cav( input = wa_zsdt0066-vbeln input1 = wa_vbfa_66-vbeln )
                        lentrega           = wa_zsdt0066-lentrega
                        mcod3              = me->get_kna1( wa_zsdt0066-lentrega )-mcod3
                        name1_arm          = me->get_kna1( wa_zsdt0066-lentrega )-name1
                        vbeln              = wa_vbfa-vbeln
                        werks66            = wa_zsdt0066-werks
                        fds_inst           = me->get_quantidade( wa_zsdt0045 )
                        peso_nf_bruto      = wa_doc_lin-brgew "138693 - Adicionar Peso nf bruto
                        peso_nf_frm        = wa_doc_lin-ntgew
                        lgort2             = wa_zsdt0066-lgort
                        contador           = me->get_contador( input = wa_zsdt0066-werks input1 = wa_vbfa-vbeln )
                        lgort              = me->at_lgort
                        saldo_fds          = REDUCE i( INIT x = 0 FOR ls IN it_zmmt0008_bl_2 WHERE ( werks EQ wa_zsdt0066-werks AND vbeln_vf EQ wa_vbfa-vbeln ) NEXT x = x + 1 )
                        peso_origem        = me->at_peso_origem
                        t_peso_origem      = me->at_peso_t_origem
                        despachante        = desc_despachante
                        corretor           = desc_corretor
                        local_carregamento = i_local_carregamento
                        status             = i_status
                        tipo               = me->at_tipo
                        hist               = COND #( WHEN input EQ abap_true THEN abap_true ELSE abap_false )
                        color              = COND #( WHEN input EQ abap_true THEN 'C500'    ELSE abap_false )
                    ) TO it_saida.

    ENDLOOP.


  ENDMETHOD.

  METHOD get_dados_historico.

    me->set_range_instrucao( ).

    IF at_instrucao[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0066
        INTO TABLE it_zsdt0066
        WHERE instrucao IN at_instrucao.
    ENDIF.

*-CS2022000332-#78919-16.06.2022-JT-inicio
    IF at_instrucao[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0053
        INTO TABLE it_zsdt0053
       WHERE instrucao IN at_instrucao.
    ENDIF.

    IF it_zsdt0053[] IS NOT INITIAL.
      PERFORM f_append_0066 TABLES t_0053
                                   t_0066.
    ENDIF.
*-CS2022000332-#78919-16.06.2022-JT-fim

    CHECK it_zsdt0066 IS NOT INITIAL.

    LOOP AT it_zsdt0066 ASSIGNING FIELD-SYMBOL(<f_zsdt0066>).
      <f_zsdt0066>-lifnr = |{ <f_zsdt0066>-werks ALPHA = IN }|.
      <f_zsdt0066>-objek = <f_zsdt0066>-nro_sol_ov.
    ENDLOOP.

    me->dados_transporte( ).

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
        FOR ALL ENTRIES IN it_zsdt0066
        WHERE objek EQ it_zsdt0066-objek
         AND  werks EQ it_zsdt0066-werks.

    IF it_zsdt0045 IS NOT INITIAL.

      me->agrupa_0045( ).

      SELECT *
        FROM zsdt0051
        INTO TABLE it_zsdt0051
         FOR ALL ENTRIES IN it_zsdt0045
            WHERE nro_sol_ov EQ it_zsdt0045-nro_sol_ov.

    ENDIF.

    SELECT vbelv vbeln rfmng vbtyp_n vbtyp_v
       FROM vbfa
       INTO CORRESPONDING FIELDS OF TABLE it_vbfa
       FOR ALL ENTRIES IN it_zsdt0066
          WHERE vbelv   = it_zsdt0066-vbeln
            AND   vbtyp_n = 'M'
            AND   vbtyp_v = 'C'.

    CHECK it_vbfa IS NOT INITIAL.

    me->modify_vbfa( ).

    SELECT  doc~branch
            lin~refkey
            lin~docnum
            lin~netwr
            lin~menge
            doc~nfenum
            doc~docdat
            doc~brgew
            doc~ntgew
        FROM j_1bnflin AS lin
        INNER JOIN j_1bnfdoc AS doc ON doc~docnum EQ lin~docnum
        INTO  TABLE it_doc_lin
      WHERE refkey IN at_refkey.

    LOOP AT it_doc_lin ASSIGNING FIELD-SYMBOL(<f_doc_lin>).
      <f_doc_lin>-vbeln = <f_doc_lin>-refkey.
    ENDLOOP.

    me->get_descricao_geral( ).
    me->get_zsdt0008( ).

    APPEND LINES OF it_saida[] TO saida[].
    FREE: it_saida[].

  ENDMETHOD.

  METHOD get_seleciona_dados_nf.

    me->clear( ).

    SELECT doc~branch
          lin~refkey
          lin~docnum
          lin~netwr
          lin~menge
          doc~nfenum
          doc~docdat
          doc~brgew
          doc~ntgew
          doc~cancel
          doc~docref
      FROM j_1bnflin AS lin
      INNER JOIN j_1bnfdoc AS doc ON doc~docnum EQ lin~docnum
      INTO  TABLE it_doc_lin
      WHERE doc~docdat IN p_dtfor
      AND   lin~matkl    EQ '700140'
      AND ( lin~cfop     EQ '6504AA'
       OR   lin~cfop     EQ '6505AA'    "*-CS2022000332-#78919-16.06.2022-JT-inicio
       OR   lin~cfop     EQ '5501AA'    "*-CS2022000332-#78919-16.06.2022-JT-inicio
       OR   lin~cfop     EQ '5501AB' )  "*-CS2022000332-#78919-16.06.2022-JT-inicio
      AND   doc~bukrs    IN p_vkorg
      AND   doc~branch   IN p_werks.

    CHECK it_doc_lin IS NOT INITIAL.

    me->del_doc_lin( ).

    SELECT vbelv vbeln rfmng vbtyp_n vbtyp_v
      FROM vbfa
      INTO CORRESPONDING FIELDS OF TABLE it_vbfa
      FOR ALL ENTRIES IN it_doc_lin
      WHERE vbeln   = it_doc_lin-vbeln
      AND   vbtyp_n = 'M'
      AND   vbtyp_v = 'C'.

    IF it_vbfa IS NOT INITIAL.

      me->modify_vbfa( ).

      SELECT *
        FROM zsdt0066
        INTO TABLE it_zsdt0066
        FOR ALL ENTRIES IN it_vbfa
        WHERE vbeln EQ it_vbfa-vbelv.

*-CS2022000332-#78919-16.06.2022-JT-inicio
      SELECT *
        FROM zsdt0053
        INTO TABLE it_zsdt0053
        FOR ALL ENTRIES IN it_vbfa
        WHERE vbeln EQ it_vbfa-vbelv.

      IF it_zsdt0053[] IS NOT INITIAL.
        PERFORM f_append_0066 TABLES t_0053
                                     t_0066.
      ENDIF.
*-CS2022000332-#78919-16.06.2022-JT-fim

      LOOP AT it_zsdt0066 ASSIGNING FIELD-SYMBOL(<f_zsdt0066>).
        <f_zsdt0066>-lifnr = |{ <f_zsdt0066>-werks ALPHA = IN }|.
        <f_zsdt0066>-objek = <f_zsdt0066>-nro_sol_ov.
      ENDLOOP.

    ENDIF.

    IF it_zsdt0066 IS NOT INITIAL.

      me->dados_transporte( ).

      SELECT *
        FROM zsdt0045
        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
          FOR ALL ENTRIES IN it_zsdt0066
          WHERE objek     EQ it_zsdt0066-objek
           AND  werks     EQ it_zsdt0066-werks
           AND safra      IN p_safra
           AND  instrucao IN p_instr.

      IF it_zsdt0045 IS NOT INITIAL.

        me->agrupa_0045( ).

        SELECT *
          FROM zsdt0051
          INTO TABLE it_zsdt0051
           FOR ALL ENTRIES IN it_zsdt0045
              WHERE nro_sol_ov EQ it_zsdt0045-nro_sol_ov
              AND vkorg IN p_vkorg
              AND spart IN p_spart
              AND vkbur IN p_vkbur
              AND kunnr IN p_kunnr
              AND bstkd IN p_bstkd
              AND data_venda IN p_data
              AND param_espec IN ( 'A', 'X', 'Z' ) " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
              AND status EQ 'L'.

      ENDIF.

      me->get_descricao_geral( ).
      me->get_zsdt0008( ).

    ENDIF.

    "De para Filial SAP com a Trace Cotton
    SELECT * FROM zsdt0176
      INTO TABLE it_zsdt0176
      ORDER BY werks ASCENDING.


  ENDMETHOD.

  METHOD get_descricao_geral.

    SELECT lifnr name1 stcd1
      FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE lifnr = it_zsdt0066-lifnr.

    SELECT matnr maktx
      FROM makt
      APPENDING TABLE it_makt
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE matnr = it_zsdt0066-matnr
      AND   spras = sy-langu.

    SELECT *
      FROM t001w
      APPENDING TABLE it_t001w
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE werks = it_zsdt0066-werks.

    SELECT  kunnr mcod3 name1
      FROM kna1
      APPENDING TABLE it_kna1
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE kunnr = it_zsdt0066-lentrega.

    IF it_vttk[] IS NOT INITIAL.
      SELECT lifnr name1 stcd1
        FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_vttk
        WHERE lifnr = it_vttk-tdlnr.
    ENDIF.

    CHECK it_zsdt0051[] IS NOT INITIAL.

    SELECT  kunnr mcod3 name1
       FROM kna1
       APPENDING TABLE it_kna1
         FOR ALL ENTRIES IN it_zsdt0051
         WHERE kunnr = it_zsdt0051-kunnr.

  ENDMETHOD.

  METHOD dados_transporte.

    SELECT vbelv vbeln
       FROM vbfa
       INTO CORRESPONDING FIELDS OF TABLE it_vbfa_66
       FOR ALL ENTRIES IN it_zsdt0066
       WHERE vbelv   = it_zsdt0066-vbeln
       AND   vbtyp_n = 'J'
       AND   vbtyp_v = 'C'.

    IF it_vbfa_66[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001
        INTO TABLE it_zsdt0001
        FOR ALL ENTRIES IN it_vbfa_66
        WHERE doc_rem = it_vbfa_66-vbeln
        AND   tp_movimento = 'S'.

      SELECT vbelv vbeln
        FROM vbfa
        INTO TABLE it_vbfa_8
        FOR ALL ENTRIES IN it_vbfa_66
        WHERE vbelv   = it_vbfa_66-vbeln
        AND   vbtyp_n = '8'
        AND   vbtyp_v = 'J'.

      IF it_vbfa_8[] IS NOT INITIAL.

        SELECT  vbelv vbeln
          FROM vtfa
          INTO TABLE it_vtfa
          FOR ALL ENTRIES IN it_vbfa_8
          WHERE vbelv = it_vbfa_8-vbeln.

        IF it_vtfa[] IS NOT INITIAL.

          SELECT  fknum kzwi1 kzwi2
            FROM vfkp
            INTO TABLE it_vfkp
            FOR ALL ENTRIES IN it_vtfa
            WHERE fknum = it_vtfa-vbeln.
        ENDIF.
      ENDIF.

      SELECT  vbeln tknum
        FROM vttp
        INTO TABLE it_vttp
        FOR ALL ENTRIES IN it_vbfa_66
        WHERE vbeln = it_vbfa_66-vbeln.

      IF it_vttp[] IS NOT INITIAL.

        SELECT  tknum  tdlnr
          FROM vttk
          INTO TABLE it_vttk
          FOR ALL ENTRIES IN it_vttp
          WHERE tknum = it_vttp-tknum.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD modify_vbfa.

    FREE: at_refkey, wvbeln.

    LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<vbfa>).
      <vbfa>-refkey = <vbfa>-vbeln.

      APPEND VALUE #( sign = 'I'
                      option = 'BT'
                      low = <vbfa>-refkey
                      high = <vbfa>-refkey ) TO at_refkey.

*      APPEND VALUE #( SIGN = 'I'
*                      OPTION = 'BT'
*                      LOW = <VBFA>-VBELN
*                      HIGH = <VBFA>-VBELN ) TO WVBELN.
    ENDLOOP.

    SELECT vbelv vbeln
      FROM vbfa
      INTO CORRESPONDING FIELDS OF TABLE it_vbfa_j
      FOR ALL ENTRIES IN it_vbfa
        WHERE vbeln   = it_vbfa-vbeln
        AND   vbtyp_n = 'M'
        AND   vbtyp_v = 'J'.

  ENDMETHOD.

  METHOD agrupa_0045.

    LOOP AT it_zsdt0045 ASSIGNING FIELD-SYMBOL(<zsdt0045>).
      <zsdt0045>-nro_sol_ov = <zsdt0045>-objek.
    ENDLOOP.

    it_0045 = it_zsdt0045.

    SORT it_0045 BY bukrs werks contrato instrucao matnr.
    DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING bukrs werks contrato instrucao matnr.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
      <f0045>-quantidade = 0.
      LOOP AT it_zsdt0045 INTO DATA(wa_0045) WHERE contrato EQ <f0045>-contrato AND
                                                   bukrs EQ <f0045>-bukrs AND
                                                   werks EQ <f0045>-werks AND
                                                   instrucao EQ <f0045>-instrucao AND
                                                   matnr EQ <f0045>-matnr.
        ADD wa_0045-quantidade TO <f0045>-quantidade.
      ENDLOOP.
    ENDLOOP.

    it_zsdt0045 = it_0045.

  ENDMETHOD.

  METHOD get_zsdt0008.

    DATA: tg_0066 TYPE TABLE OF zsdt0066 WITH DEFAULT KEY,
          tg_0053 TYPE TABLE OF zsdt0053 WITH DEFAULT KEY.

    SELECT *
      FROM zsdt0066
      INTO TABLE tg_0066
      FOR ALL ENTRIES IN it_zsdt0045
      WHERE nro_sol_ov EQ it_zsdt0045-nro_sol_ov
        AND werks EQ it_zsdt0045-werks
        AND instrucao EQ it_zsdt0045-instrucao.

*-CS2022000332-#78919-16.06.2022-JT-inicio
    SELECT *
      FROM zsdt0053
      INTO TABLE tg_0053
      FOR ALL ENTRIES IN it_zsdt0045
      WHERE nro_sol_ov EQ it_zsdt0045-nro_sol_ov
        AND werks      EQ it_zsdt0045-werks
        AND instrucao  EQ it_zsdt0045-instrucao.

    IF tg_0053[] IS NOT INITIAL.
      PERFORM f_append_0066 TABLES tg_0053
                                   tg_0066.
    ENDIF.
*-CS2022000332-#78919-16.06.2022-JT-fim

    CHECK tg_0066 IS NOT INITIAL.

    SELECT *
      FROM zmmt0008
      INTO TABLE it_zmmt0008
      FOR ALL ENTRIES IN tg_0066
      WHERE werks EQ tg_0066-werks
      AND   vbeln EQ tg_0066-vbeln.

    APPEND LINES OF it_zmmt0008[] TO it_zmmt0008_bl[].
    APPEND LINES OF it_zmmt0008_bl[] TO it_zmmt0008_bl_2[].

    SORT: it_zmmt0008_bl     BY werks vbeln_vf lgort,
          it_lfa1            BY lifnr.

    DELETE ADJACENT DUPLICATES FROM it_zmmt0008_bl COMPARING werks vbeln_vf lgort.

  ENDMETHOD.

  METHOD clear.

    FREE: it_doc_lin, it_vbfa, it_vbfa_j, it_zsdt0066,
     it_lfa1, it_makt, it_t001w, it_kna1, it_zsdt0045, it_zsdt0051,
     it_vbfa_66, it_zsdt0001, it_vbfa_8, it_vtfa, it_vfkp,
     it_vttp, it_vttk, it_lfa1, wvbeln, it_zmmt0008, saida.

  ENDMETHOD.

  METHOD del_duplicado.

    LOOP AT saida INTO DATA(wa1).
      DELETE it_saida WHERE nfnum_lote EQ wa1-nfnum_lote.
    ENDLOOP.

    APPEND LINES OF saida[] TO it_saida[].

  ENDMETHOD.

  METHOD del_doc_lin.

    FREE at_docnum.

    LOOP AT it_doc_lin ASSIGNING FIELD-SYMBOL(<f_doc_lin>).
      <f_doc_lin>-vbeln = <f_doc_lin>-refkey.
    ENDLOOP.

    DATA(doc1) = VALUE tt_docnum( FOR ls IN it_doc_lin WHERE ( cancel EQ abap_true )
                          LET s = 'I' o = 'BT'
                          IN  sign = s option = o
                          (
                            low  = ls-docnum
                            high = ls-docnum
                          )
                        ).

    APPEND LINES OF doc1 TO at_docnum.

    CHECK at_docnum IS NOT INITIAL.

    SELECT *
      FROM j_1bnfdoc
      INTO TABLE @DATA(it_docnum)
        WHERE docref IN @at_docnum.

    CHECK it_docnum IS NOT INITIAL.

    DATA(doc2) = VALUE tt_docnum( FOR ls1 IN it_docnum
                          LET s = 'I' o = 'BT'
                          IN  sign = s option = o
                          (
                            low  = ls1-docnum
                            high = ls1-docnum
                          )
                       ).

    APPEND LINES OF doc2 TO at_docnum.

    DELETE it_doc_lin WHERE docnum IN at_docnum.

  ENDMETHOD.

  METHOD get_seq.
    return = lines( it_fieldcat[] ) + 1.
  ENDMETHOD.

  METHOD get_quantidade.

    CLEAR return.

    CHECK NOT line_exists( it_saida[ contrato = input-contrato
                                      werks    = input-werks
                                     instrucao = input-instrucao ] ) .

    return = REDUCE #( INIT c = return FOR ls1 IN it_zsdt0045
                                       WHERE ( contrato   EQ input-contrato  AND
                                               werks      EQ input-werks     AND
                                               instrucao  EQ input-instrucao
                                              ) NEXT c = c + ls1-quantidade ).

  ENDMETHOD.

  METHOD get_qtd_ctners.

    CLEAR return.

    CHECK NOT line_exists( it_saida[ instrucao = input-instrucao ] ) .

    return = input-qtd_ctners.

  ENDMETHOD.

  METHOD set_primeiro.

    CLEAR: at_zmeng_lote, at_vlrtot_lote, return.

    CHECK NOT line_exists( it_saida[
                                     nro_sol_ov = input-nro_sol_ov
                                     posnr66    = input-posnr
                                   ]
                         ).

    return =
    REDUCE #( INIT a = return FOR ls IN it_zsdt0066
              WHERE ( nro_sol_ov EQ input-nro_sol_ov AND
                      posnr      EQ input-posnr
            ) NEXT a = a + ls-volum ).

    at_zmeng_lote =
    REDUCE #( INIT b = at_zmeng_lote FOR ls1 IN it_zsdt0066
              WHERE ( nro_sol_ov EQ input-nro_sol_ov AND
                      posnr      EQ input-posnr
            ) NEXT b = b + ls1-zmeng ).

    at_vlrtot_lote =
    REDUCE #( INIT c = at_vlrtot_lote FOR ls2 IN it_zsdt0066
               WHERE ( nro_sol_ov EQ input-nro_sol_ov AND
                       posnr      EQ input-posnr
            ) NEXT c = c + ls2-vlrtot ).

  ENDMETHOD.

  METHOD select_qtd_fardos.

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
      WHERE bukrs      IN p_vkorg  "*-CS2022000332-#78927-17.06.2022-JT
        AND instrucao  IN p_instr
        AND safra      IN p_safra
        AND data_instr IN p_dtins.

*-CS2022000332-#78927-17.06.2022-JT-inicio
    IF it_zsdt0045[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0051
        INTO TABLE it_zsdt0051
        FOR ALL ENTRIES IN it_zsdt0045
      WHERE nro_sol_ov  EQ it_zsdt0045-objek(10)
        AND param_espec IN ( 'A', 'X', 'Z' ) " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
        AND status      EQ 'L'.
    ENDIF.
*Inicio de alteeração - fmartins - CS104119 - 12/08/2022
*    DATA: it_zsdt0051_aux TYPE TABLE OF ty_zsdt0051,
*          wa_zsdt0051_aux TYPE  ty_zsdt0051.
*
*
**    LOOP AT it_zsdt0051 INTO wa_zsdt0051.
**      DATA(l_tabix) = sy-tabix.
**      READ TABLE it_zsdt0045  INTO wa_zsdt0045 WITH KEY objek(10) = wa_zsdt0051-nro_sol_ov.
**      IF sy-subrc = 0.
**        wa_zsdt0051-instrucao = wa_zsdt0045-instrucao.
**        MODIFY it_zsdt0051 FROM wa_zsdt0051 INDEX l_tabix.
**      ENDIF.
**    ENDLOOP.
*
*    LOOP AT it_zsdt0051 INTO wa_zsdt0051.
*
*      LOOP AT it_zsdt0045  INTO wa_zsdt0045 WHERE objek(10) = wa_zsdt0051-nro_sol_ov.
*        MOVE-CORRESPONDING wa_zsdt0051 TO wa_zsdt0051_aux.
*        wa_zsdt0051_aux-instrucao = wa_zsdt0045-instrucao.
*        APPEND wa_zsdt0051_aux TO it_zsdt0051_aux.
*      ENDLOOP.
*
*    ENDLOOP.
*
**    SORT it_zsdt0051_aux BY instrucao ASCENDING.                           ">>SKM-IR120912-16.12.22
**    DELETE ADJACENT DUPLICATES FROM it_zsdt0051_aux COMPARING instrucao.   ">>SKM-IR102912-16.12.22
*    SORT it_zsdt0051_aux BY instrucao id_contrato BSTKD ASCENDING.                "<<SKM-IR120912-16.12.22
*    DELETE ADJACENT DUPLICATES FROM it_zsdt0051_aux COMPARING instrucao id_contrato BSTKD.  "<<SKM-IR102912-16.12.22
*
*    it_zsdt0051[] = it_zsdt0051_aux[].
*
**Inicio de alteeração - fmartins - CS104119 - 12/08/2022

    LOOP AT it_zsdt0051 INTO wa_zsdt0051.
      DATA(l_tabix) = sy-tabix.
      READ TABLE it_zsdt0045  INTO wa_zsdt0045 WITH KEY objek(10) = wa_zsdt0051-nro_sol_ov.
      IF sy-subrc = 0.
        wa_zsdt0051-instrucao = wa_zsdt0045-instrucao.
        MODIFY it_zsdt0051 FROM wa_zsdt0051 INDEX l_tabix.
      ENDIF.
    ENDLOOP.

    LOOP AT it_zsdt0045 ASSIGNING FIELD-SYMBOL(<f_0045>).
      <f_0045>-desc_filial = get_name1_werks( <f_0045> ).
    ENDLOOP.

    it_0045 = it_zsdt0045.
    it_0045_aux = it_zsdt0045.

    SORT it_0045 BY bukrs werks desc_filial instrucao.
    DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING bukrs werks desc_filial instrucao.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
      <f0045>-quantidade = 0.
      LOOP AT it_zsdt0045 INTO DATA(wa_0045) WHERE "OBJEK EQ <F0045>-OBJEK AND
                                                   bukrs EQ <f0045>-bukrs AND
                                                   werks EQ <f0045>-werks AND
                                                   desc_filial EQ <f0045>-desc_filial AND
                                                   instrucao EQ <f0045>-instrucao.
        ADD wa_0045-quantidade TO <f0045>-quantidade.
*        <F0045>-NRO_SOL_OV = <F0045>-OBJEK.
      ENDLOOP.
    ENDLOOP.

    it_0045_inst = it_zsdt0045.
    it_zsdt0045 = it_0045.

*-CS2022000332-#78919-16.06.2022-JT-inicio
    it_0051_a[] = it_zsdt0051[].
    DELETE it_0051_a WHERE param_espec <> 'A'
                       AND param_espec <> 'Z'. " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ.

    it_0051_x[] = it_zsdt0051[].
    DELETE it_0051_x WHERE param_espec <> 'X'.
*-CS2022000332-#78919-16.06.2022-JT-inicio

    IF it_zsdt0045 IS NOT INITIAL.
*     SELECT *
*       FROM zsdt0066
*       INTO TABLE it_zsdt0066
*       FOR ALL ENTRIES IN it_zsdt0045
*       WHERE instrucao EQ it_zsdt0045-instrucao.
      IF it_0051_a[] IS NOT INITIAL.
     "inicio >>> IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
*        SELECT *
*          FROM zsdt0066
*          INTO TABLE it_zsdt0066
*          FOR ALL ENTRIES IN it_0051_a
*          WHERE nro_sol_ov EQ it_0051_a-nro_sol_ov
*          AND instrucao    EQ it_0051_a-instrucao.

        SELECT *
          FROM zsdt0066
          INTO TABLE it_zsdt0066
          FOR ALL ENTRIES IN it_0045_inst
          WHERE nro_sol_ov EQ it_0045_inst-objek(10)
          AND instrucao    EQ it_0045_inst-instrucao.

        if sy-SUBRC EQ 0.
          SELECT *
            FROM zsdt0328
            INTO TABLE it_zsdt0328
            FOR ALL ENTRIES IN it_zsdt0066
            WHERE nro_sol_ov EQ it_zsdt0066-nro_sol_ov
            AND instrucao    EQ it_zsdt0066-instrucao
            AND werks        EQ it_zsdt0066-werks
            AND cancelado    NE abap_true.
        ENDIF.
       "fim <<< IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
      ENDIF.
*-CS2022000332-#78927-17.06.2022-JT-inicio
      IF it_0051_x[] IS NOT INITIAL.
        SELECT *
          FROM zsdt0053
          INTO TABLE it_zsdt0053
          FOR ALL ENTRIES  IN it_0051_x
        WHERE nro_sol_ov   EQ it_0051_x-nro_sol_ov
          AND instrucao    EQ it_0051_x-instrucao.

        IF it_zsdt0053[] IS NOT INITIAL.
          PERFORM f_append_0066 TABLES t_0053
                                       t_0066.
        ENDIF.
      ENDIF.
*-CS2022000332-#78927-17.06.2022-JT-fim

      IF it_zsdt0066 IS NOT INITIAL.

        LOOP AT it_zsdt0066 ASSIGNING FIELD-SYMBOL(<f_0066>).
          IF <f_0066>-vbeln IS INITIAL.
            <f_0066>-vbeln = '##########'.
          ENDIF.
          "inicio >>> IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
           READ TABLE it_zsdt0328 INTO wa_zsdt0328 WITH KEY NRO_SOL_OV = <f_0066>-NRO_SOL_OV
                                                            POSNR      = <f_0066>-POSNR
                                                            INSTRUCAO  = <f_0066>-instrucao
                                                            WERKS      = <f_0066>-werks.
           if sy-SUBRC EQ 0.

             TRY.
                  <f_0066>-desc_filial = it_0045_aux[ werks = <f_0066>-werks
                                                      instrucao = <f_0066>-instrucao
                                                      charg  = wa_zsdt0328-CHARG_ORI
                                                      ]-desc_filial.

                CATCH cx_sy_itab_line_not_found.
             ENDTRY.
          "fim <<< IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
           else.
             TRY.
                  <f_0066>-desc_filial = it_0045_aux[ werks = <f_0066>-werks
                                                      instrucao = <f_0066>-instrucao
                                                      charg  = <f_0066>-charg_ori
                                                      ]-desc_filial.

                CATCH cx_sy_itab_line_not_found.
             ENDTRY.
           endif.

        ENDLOOP.

        SELECT *
          FROM zmmt0008
          INTO TABLE it_zmmt0008
          FOR ALL ENTRIES IN it_zsdt0066
          WHERE werks EQ it_zsdt0066-werks
          AND   vbeln EQ it_zsdt0066-vbeln.

      ENDIF.
    ENDIF.


    processa_qtd_fardos( ).

    exibe_dados( ).

  ENDMETHOD.

  METHOD exibe_dados.

    TYPES: BEGIN OF ty_estrutura.
             INCLUDE TYPE slis_fieldcat_main.
             INCLUDE TYPE slis_fieldcat_alv_spec.
    TYPES: END OF ty_estrutura.

    DATA: _lay_  TYPE slis_layout_alv,
          _fcat_ TYPE TABLE OF ty_estrutura,
          _sort  TYPE slis_t_sortinfo_alv.

    _lay_-colwidth_optimize = abap_true.
    _lay_-zebra             = abap_true.

    DATA(exibir) = COND #( WHEN r_peso IS NOT INITIAL THEN '' ELSE 'X' ).


    _fcat_ = COND #( WHEN r_peso IS NOT INITIAL
    THEN VALUE #(

                  ( col_pos = 1 tabname   = 'IT_REL'  fieldname = 'INSTRUCAO'           seltext_l = 'Instrução' )
                  ( col_pos = 2 tabname   = 'IT_REL'  fieldname = 'PRAZO_FINAL_PORTO'   seltext_l = 'Prazo Final Porto'  )
                  ( col_pos = 3 tabname   = 'IT_REL'  fieldname = 'QTD_FARDOS'          seltext_l = 'Qnte Fardos'       do_sum = abap_true )
                  ( col_pos = 4 tabname   = 'IT_REL'  fieldname = 'QTD_CARREGADOS'      seltext_l = 'Qnte carregado'    do_sum = abap_true )
                  ( col_pos = 5 tabname   = 'IT_REL'  fieldname = 'SALDO'               seltext_l = 'Saldo Fardos a Carregar' do_sum = abap_true )
                  ( col_pos = 6 tabname   = 'IT_REL'  fieldname = 'PESO_MAX'            seltext_l = 'Peso Limite'       do_sum = abap_true )
                  ( col_pos = 7 tabname   = 'IT_REL'  fieldname = 'RFMNG'               seltext_l = 'Peso Carregado'    do_sum = abap_true )
                  ( col_pos = 8 tabname   = 'IT_REL'  fieldname = 'SALDO_PESO_RFMNG'    seltext_l = 'Saldo Peso'        do_sum = abap_true )
                )

    ELSE  VALUE #(
                  ( col_pos = 1 tabname   = 'IT_REL'  fieldname = 'LOCAL_CARREGAMENTO'  seltext_l = 'Local de Carregamento' )
                  ( col_pos = 2 tabname   = 'IT_REL'  fieldname = 'PRAZO_FINAL_PORTO'   seltext_l = 'Prazo Final Porto'  )
                  ( col_pos = 3 tabname   = 'IT_REL'  fieldname = 'TRANSPORTADORA'      seltext_l = 'Transportadora'    )
                  ( col_pos = 4 tabname   = 'IT_REL'  fieldname = 'INSTRUCAO'           seltext_l = 'Instrução' )
                  ( col_pos = 5 tabname   = 'IT_REL'  fieldname = 'QTD_FARDOS'          seltext_l = 'Qnte Fardos'       do_sum = abap_true )
                  ( col_pos = 6 tabname   = 'IT_REL'  fieldname = 'QTD_CARREGADOS'      seltext_l = 'Qnte carregado'    do_sum = abap_true )
                  ( col_pos = 7 tabname   = 'IT_REL'  fieldname = 'SALDO'               seltext_l = 'Saldo à carregar' do_sum = abap_true )
                ) ).

    _sort = COND #( WHEN r_peso IS INITIAL
    THEN VALUE #( ( spos = 1 fieldname = 'LOCAL_CARREGAMENTO' up = abap_true subtot = abap_true ) )
    ).


*    _SORT = COND #( WHEN R_PESO IS NOT INITIAL
*    THEN VALUE #( ( SPOS = 1 FIELDNAME = 'INSTRUCAO' UP = ABAP_TRUE SUBTOT = ABAP_TRUE ) )
*    ELSE VALUE #( ( SPOS = 1 FIELDNAME = 'LOCAL_CARREGAMENTO' UP = ABAP_TRUE SUBTOT = ABAP_TRUE ) )
*    ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = _lay_
        it_fieldcat        = _fcat_
        it_sort            = _sort
      TABLES
        t_outtab           = it_rel.


  ENDMETHOD.

  METHOD processa_qtd_fardos.
    IF r_peso IS NOT INITIAL.

      DATA(t_0045) = it_0045_aux.
      DATA(t_0045_qtd_fardos) = it_0045_aux.


      SORT t_0045 BY instrucao.
      DELETE ADJACENT DUPLICATES FROM t_0045 COMPARING instrucao.

      SORT it_0045_aux BY instrucao werks.
      DELETE ADJACENT DUPLICATES FROM it_0045_aux COMPARING instrucao werks.

      LOOP AT t_0045 INTO DATA(w0045).
        LOOP AT it_0045_aux INTO DATA(w_0045) WHERE instrucao EQ w0045-instrucao.

          wa_rel-local_carregamento = get_name1_werks( w_0045 ).
          wa_rel-prazo_final_porto  = w_0045-data_porto.
          wa_rel-instrucao          = w_0045-instrucao.

          LOOP AT t_0045_qtd_fardos INTO DATA(w_qtd) WHERE instrucao EQ w0045-instrucao AND werks EQ  w_0045-werks.
            wa_rel-qtd_fardos         = wa_rel-qtd_fardos + get_qtd_fardos( w_qtd-quantidade ).
          ENDLOOP.

          wa_rel-peso_max           = w_0045-peso_max.

          LOOP AT it_zsdt0066 INTO DATA(w_0066) WHERE instrucao EQ w_0045-instrucao
                                                  AND werks EQ w_0045-werks.
*
*            IF GET_NAME1_WERKS( W_0045 ) EQ 'TUCUNARÉ' AND W_0066-LGORT NE 'ALGD'.
*              WA_REL-LOCAL_CARREGAMENTO = 'CHEROKEE'.
*            ELSE.
*              WA_REL-LOCAL_CARREGAMENTO = GET_NAME1_WERKS( W_0045 ).
*            ENDIF.

            DATA(_fardos) = get_qtd_fardos_carregados( input = w_0066 input1 = wa_rel-local_carregamento ).
            ADD _fardos TO wa_rel-qtd_carregados.
            DATA(_carregado) = get_peso_carregado( w_0066-vbeln ).
            ADD _carregado TO wa_rel-rfmng.
          ENDLOOP.


          wa_rel-saldo = wa_rel-qtd_fardos - wa_rel-qtd_carregados.

          wa_rel-saldo_peso_rfmng = wa_rel-peso_max - wa_rel-rfmng.

        ENDLOOP.
        APPEND wa_rel TO it_rel.
        CLEAR wa_rel.

      ENDLOOP.

    ELSE.

      LOOP AT it_zsdt0045 INTO w_0045.

        wa_rel-local_carregamento = get_name1_werks( w_0045 ).

        wa_rel-prazo_final_porto  = w_0045-data_porto.
        wa_rel-transportadora     = get_transportadora( w_0045-cod_transp ).
        wa_rel-instrucao          = w_0045-instrucao.
        wa_rel-qtd_fardos         = get_qtd_fardos( w_0045-quantidade ).

        LOOP AT it_zsdt0066 INTO w_0066 WHERE instrucao EQ w_0045-instrucao
                                          AND desc_filial EQ w_0045-desc_filial
                                          AND werks EQ w_0045-werks.

*          IF GET_NAME1_WERKS( W_0045 ) EQ 'TUCUNARÉ' AND W_0066-LGORT NE 'ALGD'.
*            WA_REL-LOCAL_CARREGAMENTO = 'CHEROKEE'.
*          ELSE.
*            WA_REL-LOCAL_CARREGAMENTO = GET_NAME1_WERKS( W_0045 ).
*          ENDIF.


          _fardos = get_qtd_fardos_carregados( input = w_0066 input1 = wa_rel-local_carregamento ).
          ADD _fardos TO wa_rel-qtd_carregados.
        ENDLOOP.

        wa_rel-saldo = wa_rel-qtd_fardos - wa_rel-qtd_carregados.

*-CS2022000332-#78927-17.06.2022-JT-inicio
        IF     r_aemba = abap_true AND wa_rel-saldo IS NOT INITIAL.
          APPEND wa_rel TO it_rel.
        ELSEIF r_embdo = abap_true AND wa_rel-saldo IS INITIAL.
          APPEND wa_rel TO it_rel.
        ENDIF.
*-CS2022000332-#78927-17.06.2022-JT-fim

        CLEAR wa_rel.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD get_name1_werks.
    SELECT SINGLE *
      FROM zsdt0166
      INTO @DATA(wa_0166)
      WHERE id EQ ( SELECT MAX( id )
                      FROM zsdt0166
                      WHERE lote EQ @input-charg
                       AND safra EQ @input-safra
                       AND werks EQ @input-werks
                  ).

    IF sy-subrc IS INITIAL.
      return = wa_0166-algodoeira.
    ELSE.
      return = SWITCH #( input-werks WHEN '1507' THEN 'TUCUNARÉ'
                                     WHEN '1521' THEN 'ITAMARATI'
                                     WHEN '1519' THEN 'TANGURO'
                                     WHEN '1520' THEN 'ÁGUA QUENTE'
                                     WHEN '1801' THEN 'AGRO SAM'
                                     ELSE '######'
                       ).
    ENDIF.
  ENDMETHOD.

  METHOD get_transportadora.
    SELECT SINGLE name1 FROM lfa1 INTO return WHERE lifnr = input.
  ENDMETHOD.

  METHOD get_peso_carregado.
    SELECT SUM( rfmng )
      FROM vbfa
      INTO return
      WHERE vbelv = input
    AND vbtyp_n = 'J'
    AND vbtyp_v = 'C'.
  ENDMETHOD.

  METHOD get_qtd_fardos.
    "//ADD INPUT TO AT_QTD_FARDOS.
    "// RETURN = AT_QTD_FARDOS.
    ADD input TO return.
  ENDMETHOD.

  METHOD get_qtd_fardos_carregados.

    IF input-werks EQ '1507'.

      DATA(it_008) = it_zmmt0008.
      SORT it_008 BY werks lgort vbeln.
      DELETE ADJACENT DUPLICATES FROM it_008 COMPARING werks lgort vbeln.

      LOOP AT it_008 INTO DATA(wa_008) WHERE werks EQ input-werks AND vbeln EQ input-vbeln.

        SELECT SINGLE *
          FROM zsdt0166
          INTO @DATA(wa_0166)
          WHERE id EQ ( SELECT MAX( id )
                          FROM zsdt0166
                          WHERE lote EQ @wa_008-lgort
                           AND werks EQ @wa_008-werks
                      ).

        IF sy-subrc IS INITIAL.
          DATA(qtd_ok) = REDUCE i( INIT x = 0
                              FOR ls IN it_zmmt0008
                                  WHERE ( werks EQ wa_0166-werks
                                      AND vbeln EQ input-vbeln
                                      AND lgort EQ wa_0166-lote )
                             NEXT x = x + 1
                           ).

          IF r_peso EQ space.                                    "<<RIM-SKM-IR127205-16.02.23
            IF input1 EQ wa_0166-algodoeira.
              ADD qtd_ok TO return.
            ENDIF.
          ELSE.                                                  "<<RIM-SKM-IR127205-16.02.23
            IF input-desc_filial EQ wa_0166-algodoeira.          "<<RIM-SKM-IR127205-16.02.23
              ADD qtd_ok TO return.                              "<<RIM-SKM-IR127205-16.02.23
            ENDIF.                                               "<<RIM-SKM-IR127205-16.02.23
          ENDIF.                                                 "<<RIM-SKM-IR127205-16.02.23

        ELSE.
          DATA(qtd_tuc) = REDUCE i( INIT x = 0
                    FOR ls IN it_zmmt0008
                        WHERE ( werks EQ wa_008-werks
                            AND vbeln EQ wa_008-vbeln
                            AND lgort EQ wa_008-lgort )
                   NEXT x = x + 1
                 ).

          IF input1 EQ 'TUCUNARÉ'.
            ADD qtd_tuc TO return.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ELSE.
      return = REDUCE i( INIT x = 0
                      FOR ls IN it_zmmt0008
                          WHERE ( werks EQ input-werks
                              AND vbeln EQ input-vbeln )
                     NEXT x = x + 1
                   ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD catch_hotspot.
    DATA: wl_0004 TYPE zppt0004,
          wl_0002 TYPE zppt0002.

*-CS2022000332-#83055-05.08.2022-JT-inicio
    DATA: x_ref TYPE REF TO zcl_monta_dados.
    CREATE OBJECT x_ref.
*-CS2022000332-#83055-05.08.2022-JT-fim

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      IF e_column_id = 'LGORT'
      AND wa_saida-vbeln IS NOT INITIAL
      OR 1 = 1.
        REFRESH tg_bloco.
        LOOP AT it_zmmt0008 INTO wa_zmmt0008 WHERE werks    = wa_saida-werks66
                                             AND   vbeln_vf = wa_saida-vbeln .
          wg_bloco-lgort = wa_zmmt0008-lgort.
          wg_bloco-charg = wa_zmmt0008-charg.

          IF wa_zmmt0008-tipo_fardo IS NOT INITIAL. ""Projeto Reestruturação Algodao 2024
            wg_bloco-tipo = wa_zmmt0008-tipo_fardo.
          ELSE.
            SELECT SINGLE verid werks acharg
              FROM zppt0002
              INTO CORRESPONDING FIELDS OF wl_0002
              WHERE acharg = wg_bloco-charg.

            IF sy-subrc IS INITIAL.
              SELECT SINGLE tipo
                FROM zppt0004
                INTO CORRESPONDING FIELDS OF wl_0004
               WHERE werks = wl_0002-werks
                 AND verid = wl_0002-verid.

              IF sy-subrc IS INITIAL.
                wg_bloco-tipo = wl_0004-tipo.
              ELSE.
*  -CS2022000332-#83055-05.08.2022-JT-inicio
                wg_bloco-tipo = x_ref->get_tipo_alterna( wl_0002-acharg ).
*  -CS2022000332-#83055-05.08.2022-JT-fim
              ENDIF.
            ENDIF.
          ENDIF.

          wg_bloco-conta = 1.
          wg_bloco-menge = wa_zmmt0008-menge.
          APPEND wg_bloco TO tg_bloco.
          CLEAR wg_bloco.
        ENDLOOP.
        CALL SCREEN 200
          STARTING AT 010 06
          ENDING   AT 110 32.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "catch_hotspot
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
* Objetos para TREE
*----------------------------------------------------------------------*

CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA tree1  TYPE REF TO cl_gui_alv_tree_simple.
"DATA TREE1  TYPE REF TO CL_GUI_ALV_TREE.

INCLUDE <icon>.
"INCLUDE BCALV_SIMPLE_EVENT_RECEIVER.

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING
          node_key.

    METHODS: on_add_hierarchy_node
      FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree_simple
      IMPORTING grouplevel
                index_outtab.

ENDCLASS.                    "LCL_TREE_EVENT_RECEIVER DEFINITION

DATA: ok_code         LIKE sy-ucomm.           " OK-Code
DATA: ok-code         LIKE sy-ucomm.           " OK-Code
DATA: _ok             TYPE c.
*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
DATA: tree_event_receiver   TYPE REF TO lcl_tree_event_receiver.
*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.

    IF node_key GT 1.
*      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
*      READ TABLE TG_PRECO TRANSPORTING NO FIELDS
*        WITH KEY ITEM_KEY = NODE_KEY.
*      IF SY-SUBRC IS NOT INITIAL.
*        PERFORM PRECO_FRAME_ALV USING NODE_KEY.
*      ENDIF.
    ENDIF.


  ENDMETHOD.                    "handle_double_click

  METHOD on_add_hierarchy_node.
    DATA ls_outtab_line TYPE ty_saida.

    CALL METHOD tree1->set_hierarchy_data
      EXPORTING
        is_outtab_line = ls_outtab_line.
  ENDMETHOD.                    "ON_ADD_HIERARCHY_NODE

ENDCLASS.                    "LCL_TREE_EVENT_RECEIVER IMPLEMENTATION

AT SELECTION-SCREEN OUTPUT.


  IF r_rege IS NOT INITIAL.

    LOOP AT SCREEN.

      IF screen-group1 = 'C4' OR screen-group1 = 'C6' OR screen-group1 = 'C7' OR screen-group1 = 'C8' OR
         screen-group1 = 'C10'. "*-CS2023000189-31.05.2023-#108695-JT
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDLOOP.
  ENDIF.

  IF r_volu IS NOT INITIAL OR r_peso IS NOT INITIAL.

    LOOP AT SCREEN.

      IF r_peso IS NOT INITIAL AND ( screen-group1 = 'C7' OR
                                     screen-group1 = 'C10' ). "*-CS2023000189-31.05.2023-#108695-JT
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF screen-group1 = 'C2' OR screen-group1 = 'C4' OR screen-group1 = 'C5' OR screen-group1 = 'C6' OR screen-group1 = 'C8'.
        IF ( r_volu IS NOT INITIAL OR r_peso IS NOT INITIAL ) AND
           ( screen-name CS 'P_VKORG_' OR  "*-CS2022000332-#78927-17.06.2022-JT
             screen-name CS 'P_VKORG-' ).
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF screen-group1 = 'C10'. "*-CS2023000189-31.05.2023-#108695-JT
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.


  IF r_fard IS NOT INITIAL.

    LOOP AT SCREEN.

      IF screen-group1 = 'C1' OR screen-group1 = 'C2' OR screen-group1 = 'C3'  OR screen-group1 = 'C5' OR screen-group1 = 'C6' OR
         screen-group1 = 'C7' OR
         screen-group1 = 'C10'. "*-CS2023000189-31.05.2023-#108695-JT
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDIF.

  IF r_expo IS NOT INITIAL.
    LOOP AT SCREEN.

      IF screen-group1 = 'C4'  OR screen-group1 = 'C5' OR screen-group1 = 'C7' OR
         screen-group1 = 'C10'. "*-CS2023000189-31.05.2023-#108695-JT
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF screen-name CS 'P_DTFOR'.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDLOOP.
  ENDIF.

*-CS2023000189-31.05.2023-#108695-JT-inicio
  IF r_acts IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'C1' OR screen-group1 = 'C2' OR screen-group1 = 'C3' OR screen-group1 = 'C4' OR
         screen-group1 = 'C5' OR screen-group1 = 'C6' OR screen-group1 = 'C7' OR screen-group1 = 'C8' OR
         screen-group1 = 'C9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
*-CS2023000189-31.05.2023-#108695-JT-fim

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF r_rege IS NOT INITIAL.

    IF NOT p_dtfor-low IS INITIAL AND r_lote IS INITIAL.
      MESSAGE 'É Obrigatorio o Preenchimento da Opção "Formação de Lote"!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF p_dtfor-low IS INITIAL AND NOT r_lote IS INITIAL.
      MESSAGE 'É Obrigatorio o Preenchimento da Data no campo "Data da formação de Lote"!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF NOT r_hist IS INITIAL AND r_lote IS INITIAL.
      MESSAGE 'É Obrigatorio o Preenchimento do Opção "Formação de Lote"!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF p_vkorg IS INITIAL.
      MESSAGE 'O campo "Organização de Vendas" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF p_spart IS INITIAL.
      MESSAGE 'O campo "Setor de Atividade" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF  p_data IS INITIAL.
      MESSAGE 'O campo "Data da Venda" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.

  IF r_volu IS NOT INITIAL.

*-CS2022000332-#78927-17.06.2022-JT-inicio
    IF p_vkorg IS INITIAL.
      MESSAGE 'O campo "Organização de Vendas" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.
*-CS2022000332-#78927-17.06.2022-JT-fim

    IF p_instr IS INITIAL AND p_dtins IS INITIAL.
      MESSAGE 'Preenche pelo menos um dos Campos para Busca!' TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.


  IF r_fard IS NOT INITIAL.

    IF p_vkorg4 IS INITIAL.
      MESSAGE 'O campo "Organização de Vendas" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF p_dtins4 IS INITIAL AND p_dtins IS INITIAL.
      MESSAGE 'O campo "Data da Instrução" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.

*-CS2023000189-31.05.2023-#108695-JT-inicio
  IF r_acts IS NOT INITIAL.
    IF s_werks5 IS INITIAL.
      MESSAGE 'O campo "Centro" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.
   IF s_lgort5 IS INITIAL. ""Projeto Reestruturação Algodao 2024
     MESSAGE 'O campo "Depósito" é Obrigatório!' TYPE 'I'.
     EXIT.
   ENDIF.
    IF s_charg5 IS INITIAL.
      MESSAGE 'O campo "Safra" é Obrigatório!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2023000189-31.05.2023-#108695-JT-fim

  IF r_fard IS NOT INITIAL.

    " Relatório Fardo a Fardo
    PERFORM f_trata_fardo_a_fardo.

  ELSE.


    CLEAR _ok.

    IF r_volu EQ abap_true OR r_peso EQ abap_true.
      obj_dados->select_qtd_fardos( ).
      EXIT.
    ENDIF.

    IF r_lote IS INITIAL AND r_hist IS INITIAL AND r_expo IS INITIAL AND r_acts IS INITIAL.

      PERFORM: f_seleciona_dados. " Form seleciona dados
      PERFORM  f_saida_. " Form de saida

    ELSEIF r_lote IS NOT INITIAL AND r_hist IS INITIAL.

      obj_dados->get_seleciona_dados_nf( ).
      obj_dados->set_saida_nf( ).

    ELSEIF r_lote IS NOT INITIAL AND r_hist IS NOT INITIAL.

      obj_dados->get_seleciona_dados_nf( ).
      obj_dados->set_saida_nf( ).
      obj_dados->get_dados_historico( ).
      obj_dados->set_saida_nf( abap_true ).
      obj_dados->del_duplicado( ).

    ELSEIF r_expo IS NOT INITIAL.

      PERFORM: f_seleciona_dados_expo. " Form seleciona dados
      PERFORM  f_saida_expo. " Form de saida

    ELSEIF r_acts IS NOT INITIAL.  "*-CS2023000189-31.05.2023-#108695-JT
      PERFORM f_seleciona_acts.
    ENDIF.

*  IF NOT R_HIST IS INITIAL.
*    _OK = ABAP_TRUE.
*    PERFORM: HISTORICO_INSTRUCAO.
*    PERFORM: F_SELECIONA_DADOS.
*    PERFORM  F_SAIDA_.
*
*
*  ENDIF.

    PERFORM: f_imprime_dados.

  ENDIF.


END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&  INCLUDE ZSDR0031_FARDO_ALV_SCREEN - CS2018001961 Sara Oikawa 05/2020
*&---------------------------------------------------------------------*
  INCLUDE zsdr0031_fardo_alv_screen_0300.

*&---------------------------------------------------------------------*
*& Append linhas na tabela IT_ZSDT0066
*&---------------------------------------------------------------------*
FORM f_append_0066 TABLES pt_0053 STRUCTURE zsdt0053
                          pt_0066 STRUCTURE zsdt0066.

  DATA: w_0053 TYPE zsdt0053,
        w_0066 TYPE zsdt0066.

  IF pt_0053[] IS INITIAL.
    IF it_zsdt0053[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0213
        INTO TABLE @DATA(t_0213)
         FOR ALL ENTRIES IN @it_zsdt0053
       WHERE nro_sol_ov = @it_zsdt0053-nro_sol_ov
         AND posnr      = @it_zsdt0053-posnr
         AND status    <> @abap_true.
    ENDIF.
  ELSE.
    SELECT *
      FROM zsdt0213
      INTO TABLE t_0213
       FOR ALL ENTRIES IN pt_0053
     WHERE nro_sol_ov = pt_0053-nro_sol_ov
       AND posnr      = pt_0053-posnr
       AND status    <> abap_true.
  ENDIF.

  IF pt_0053[] IS INITIAL.
    LOOP AT it_zsdt0053            INTO wa_zsdt0053.
      MOVE-CORRESPONDING wa_zsdt0053 TO wa_zsdt0066.
      MOVE wa_zsdt0053-terminal      TO wa_zsdt0066-lentrega.
      MOVE wa_zsdt0053-kvgr3         TO wa_zsdt0066-classificacao.

      READ TABLE t_0213 INTO DATA(w_0213) WITH KEY nro_sol_ov = wa_zsdt0053-nro_sol_ov
                                                   posnr      = wa_zsdt0053-posnr.
      IF sy-subrc = 0.
        MOVE w_0213-lgort            TO wa_zsdt0066-charg_ori.
      ENDIF.

      APPEND  wa_zsdt0066            TO it_zsdt0066.
    ENDLOOP.
    FREE: it_zsdt0053.
  ELSE.
    LOOP AT pt_0053                INTO w_0053.
      MOVE-CORRESPONDING w_0053      TO w_0066.
      MOVE wa_zsdt0053-terminal      TO w_0066-lentrega.
      MOVE wa_zsdt0053-kvgr3         TO w_0066-classificacao.

      READ TABLE t_0213 INTO w_0213 WITH KEY nro_sol_ov = w_0053-nro_sol_ov
                                             posnr      = w_0053-posnr.
      IF sy-subrc = 0.
        MOVE w_0213-lgort            TO w_0066-charg_ori.
      ENDIF.

      APPEND  w_0066                 TO pt_0066.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
  "Tabela de Solicitação Ordem de Venda - Cabeçalho
  FREE: it_kna1,it_zsdt0053,
*  IT_ZSDT_DEPARA_CEN,
  it_t001w, it_vbfa, it_vbap, it_j_1bnflin, it_j_1bnfdoc, it_zsdt0045, it_zsdt0066,
  it_lfa1, it_makt, it_t001w, it_kna1, it_vbfa, it_vbfa_j, it_j_1bnflin, it_j_1bnfdoc, it_vbfa_66, it_zsdt0001, it_vbfa_8,
  it_vtfa, it_vfkp, it_vttp, it_vttk, it_lfa1, it_zmmt0008.

  IF _ok IS INITIAL.

    SELECT *
      FROM zsdt0051
      INTO TABLE it_zsdt0051
      WHERE vkorg IN p_vkorg
      AND   spart IN p_spart
      AND   vkbur IN p_vkbur
      AND   kunnr IN p_kunnr
      AND   bstkd IN p_bstkd
      AND   data_venda IN p_data
      AND   param_espec IN ( 'A', 'X', 'Z' ) " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ " 'A'-> Algodão AMAGGI, 'X'-> Algodão OTELHAR
      AND   status EQ 'L'.

    PERFORM check_dados.

  ENDIF.

  CHECK it_zsdt0051[] IS NOT INITIAL.

  SELECT  kunnr mcod3 name1
     FROM kna1
     INTO TABLE it_kna1
     FOR ALL ENTRIES IN it_zsdt0051
     WHERE kunnr = it_zsdt0051-kunnr.

  LOOP AT it_zsdt0051 ASSIGNING FIELD-SYMBOL(<zsdt0051>).
    <zsdt0051>-objek = <zsdt0051>-nro_sol_ov.
  ENDLOOP.

  "Tabela de Instruções de Embarque de Algodão
  IF _ok IS INITIAL.

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE objek EQ it_zsdt0051-objek
       AND  instrucao IN p_instr
       AND safra      IN p_safra
       AND  data_instr IN p_dtins.

    it_0045 = it_zsdt0045.

    SORT it_0045 BY objek bukrs werks instrucao matnr.
    DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING  objek bukrs werks instrucao matnr.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
      <f0045>-quantidade = 0.
      LOOP AT it_zsdt0045 INTO DATA(wa_0045) WHERE objek EQ <f0045>-objek AND
                                                   bukrs EQ <f0045>-bukrs AND
                                                   werks EQ <f0045>-werks AND
                                                   instrucao EQ <f0045>-instrucao AND
                                                   matnr EQ <f0045>-matnr.
        ADD wa_0045-quantidade TO <f0045>-quantidade.
      ENDLOOP.
    ENDLOOP.

    it_zsdt0045 = it_0045.

*-CS2022000332-#78919-16.06.2022-JT-inicio
    it_0051_a[] = it_zsdt0051[].
    DELETE it_0051_a WHERE param_espec <> 'A'
                       AND param_espec <> 'Z'. " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ..

    it_0051_x[] = it_zsdt0051[].
    DELETE it_0051_x WHERE param_espec <> 'X'.
*-CS2022000332-#78919-16.06.2022-JT-inicio

*-CS2022000332-#78919-16.06.2022-JT-inicio
    "Tabela de Solicitação de Ordem de Venda – Formação de Lote
    IF it_0051_a[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0066
        INTO TABLE it_zsdt0066
        FOR ALL ENTRIES IN it_0051_a
      WHERE nro_sol_ov EQ it_0051_a-nro_sol_ov
        AND instrucao    IN p_instr
        AND werks        IN p_werks
        AND matnr        IN p_matnr.
    ENDIF.

    IF it_0051_x[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0053
        INTO TABLE it_zsdt0053
        FOR ALL ENTRIES IN it_0051_x
      WHERE nro_sol_ov EQ it_0051_x-nro_sol_ov
        AND instrucao    IN p_instr
        AND werks        IN p_werks
        AND matnr        IN p_matnr.

      PERFORM f_append_0066 TABLES t_0053
                                   t_0066.
    ENDIF.
*-CS2022000332-#78919-16.06.2022-JT-fim

  ELSE.

*  HISTORICO DA INSTRUÇÃO
    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE objek EQ it_zsdt0051-objek
        AND safra      IN p_safra
        AND instrucao IN instrucao.

    it_0045 = it_zsdt0045.

    SORT it_0045 BY objek bukrs werks instrucao matnr.
    DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING  objek bukrs werks instrucao matnr.

    LOOP AT it_0045 ASSIGNING <f0045>.
      <f0045>-quantidade = 0.
      LOOP AT it_zsdt0045 INTO wa_0045 WHERE objek EQ <f0045>-objek AND
                                                   bukrs EQ <f0045>-bukrs AND
                                                   werks EQ <f0045>-werks AND
                                                   instrucao EQ <f0045>-instrucao AND
                                                   matnr EQ <f0045>-matnr.
        ADD wa_0045-quantidade TO <f0045>-quantidade.
      ENDLOOP.
    ENDLOOP.

    it_zsdt0045 = it_0045.

    "Tabela de Solicitação de Ordem de Venda – Formação de Lote
    SELECT *
      FROM zsdt0066
      INTO TABLE it_zsdt0066
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov
        AND instrucao IN instrucao.

  ENDIF.


  LOOP AT it_zsdt0066 ASSIGNING FIELD-SYMBOL(<f_zsdt0066>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <f_zsdt0066>-werks
      IMPORTING
        output = <f_zsdt0066>-lifnr.
  ENDLOOP.

  IF it_zsdt0066[] IS NOT INITIAL.
    SELECT lifnr name1 stcd1
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE lifnr = it_zsdt0066-lifnr.

    SELECT matnr maktx
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE matnr = it_zsdt0066-matnr
      AND   spras = sy-langu.

    SELECT *
      FROM t001w
      APPENDING TABLE it_t001w
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE werks = it_zsdt0066-werks.

    SELECT  kunnr mcod3 name1
      FROM kna1
      APPENDING TABLE it_kna1
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE kunnr = it_zsdt0066-lentrega.

    SELECT vbelv vbeln rfmng vbtyp_n vbtyp_v
     FROM vbfa
     APPENDING CORRESPONDING FIELDS OF TABLE it_vbfa
     FOR ALL ENTRIES IN it_zsdt0066
     WHERE vbelv   = it_zsdt0066-vbeln
     AND   vbtyp_n = 'M'
     AND   vbtyp_v = 'C'.

    LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<vbfa>).
      <vbfa>-refkey = <vbfa>-vbeln .
    ENDLOOP.

    IF it_vbfa[] IS NOT INITIAL.

      SELECT vbelv vbeln
      FROM vbfa
      APPENDING CORRESPONDING FIELDS OF TABLE it_vbfa_j
      FOR ALL ENTRIES IN it_vbfa
      WHERE vbeln   = it_vbfa-vbeln
      AND   vbtyp_n = 'M'
      AND   vbtyp_v = 'J'.

      SELECT refkey docnum netwr menge
        FROM j_1bnflin
        APPENDING CORRESPONDING FIELDS OF TABLE it_j_1bnflin
        FOR ALL ENTRIES IN it_vbfa
        WHERE refkey = it_vbfa-refkey.

      IF it_j_1bnflin[] IS NOT INITIAL.
        IF _ok IS INITIAL.
          SELECT docnum nfenum docdat brgew ntgew
            FROM j_1bnfdoc
            INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfdoc
            FOR ALL ENTRIES IN it_j_1bnflin
            WHERE docnum = it_j_1bnflin-docnum
              AND docdat IN p_dtfor.
        ELSE.
          SELECT docnum nfenum docdat brgew ntgew
            FROM j_1bnfdoc
            INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfdoc
            FOR ALL ENTRIES IN it_j_1bnflin
            WHERE docnum = it_j_1bnflin-docnum.

        ENDIF.
      ENDIF.
    ENDIF.

    SELECT vbelv vbeln
     FROM vbfa
     INTO CORRESPONDING FIELDS OF TABLE it_vbfa_66
     FOR ALL ENTRIES IN it_zsdt0066
     WHERE vbelv   = it_zsdt0066-vbeln
     AND   vbtyp_n = 'J'
     AND   vbtyp_v = 'C'.

    IF it_vbfa_66[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001
        INTO TABLE it_zsdt0001
        FOR ALL ENTRIES IN it_vbfa_66
        WHERE doc_rem = it_vbfa_66-vbeln
        AND   tp_movimento = 'S'.

      SELECT vbelv vbeln
      FROM vbfa
      INTO TABLE it_vbfa_8
      FOR ALL ENTRIES IN it_vbfa_66
      WHERE vbelv   = it_vbfa_66-vbeln
      AND   vbtyp_n = '8'
      AND   vbtyp_v = 'J'.

      IF it_vbfa_8[] IS NOT INITIAL.
        SELECT  vbelv vbeln
          FROM vtfa
          INTO TABLE it_vtfa
          FOR ALL ENTRIES IN it_vbfa_8
          WHERE vbelv = it_vbfa_8-vbeln.

        IF it_vtfa[] IS NOT INITIAL.
          SELECT  fknum kzwi1 kzwi2
            FROM vfkp
            INTO TABLE it_vfkp
            FOR ALL ENTRIES IN it_vtfa
            WHERE fknum = it_vtfa-vbeln.
        ENDIF.
      ENDIF.

      SELECT  vbeln tknum
      FROM vttp
      INTO TABLE it_vttp
      FOR ALL ENTRIES IN it_vbfa_66
      WHERE vbeln = it_vbfa_66-vbeln.

      IF it_vttp[] IS NOT INITIAL.
        SELECT  tknum  tdlnr
          FROM vttk
          INTO TABLE it_vttk
          FOR ALL ENTRIES IN it_vttp
          WHERE tknum = it_vttp-tknum.
        IF it_vttk[] IS NOT INITIAL.
          SELECT lifnr name1 stcd1
            FROM lfa1
            APPENDING TABLE it_lfa1
            FOR ALL ENTRIES IN it_vttk
            WHERE lifnr = it_vttk-tdlnr.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT it_vbfa IS INITIAL.
      wvbeln-sign = 'I'.
      wvbeln-option = 'BT'.
      LOOP AT it_vbfa INTO DATA(wvbfa).
        wvbeln-low = wvbeln-high = wvbfa-vbeln.
        APPEND wvbeln.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM zmmt0008
      INTO TABLE it_zmmt0008
      FOR ALL ENTRIES IN it_zsdt0066
      WHERE werks = it_zsdt0066-werks
      AND   vbeln_vf IN wvbeln.

  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS

*********************************************************
* selecao ACTS
*********************************************************
FORM f_seleciona_acts.

  DATA: lwa_mchb TYPE type_mchb.

  FREE: it_zsdt0166, v_trace, v_letrac.

  SELECT *
    FROM zsdt0166
    INTO TABLE it_zsdt0166
   WHERE werks IN s_werks5
     AND lote  IN s_lgort5
     AND safra IN s_charg5
     AND acts   = abap_true.

  SORT it_zsdt0166 BY werks lote safra id DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0166 COMPARING werks lote safra.

  CHECK it_zsdt0166[] IS NOT INITIAL.

  LOOP AT it_zsdt0166  ASSIGNING FIELD-SYMBOL(<fs_zsdt0166>).
    <fs_zsdt0166>-lgort = <fs_zsdt0166>-lote.
    "MODIFY it_zsdt0166 FROM wa_zsdt0166 INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_zsdt0166 INTO DATA(lwa_zsdt0166).

    zcl_trace_cotton_utils=>get_fardos_bloco_trace_cotton(
      EXPORTING
        i_safra                     = CONV #( lwa_zsdt0166-safra )
        i_filial_algodoeira         = CONV #( lwa_zsdt0166-werks )
        i_bloco                     = CONV #( lwa_zsdt0166-lgort )
        i_matnr                     = CONV #( lwa_zsdt0166-matnr )
        i_check_embarque_sap        = abap_true
        i_return_dados_acts         = abap_true
      IMPORTING
        e_msg_error                 = DATA(_msg_error)
        e_fardos_bloco_trace_cotton = DATA(lit_fardos_trace)
    ).

    IF _msg_error IS NOT INITIAL.
      MESSAGE _msg_error TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT lit_fardos_trace INTO DATA(lwa_fardo_trace).
      CLEAR: lwa_mchb.


      lwa_mchb-icon                     = lwa_fardo_trace-validacao_acts-icon.
      lwa_mchb-werks                    = lwa_zsdt0166-werks.
      lwa_mchb-safra                    = lwa_fardo_trace-safra.
      lwa_mchb-lgort                    = lwa_zsdt0166-lgort.
      lwa_mchb-charg                    = lwa_fardo_trace-nr_fardo_completo.
      lwa_mchb-clabs                    = lwa_fardo_trace-peso_liquido.
      lwa_mchb-cd_sai                   = lwa_fardo_trace-cd_sai.
      lwa_mchb-acts                     = lwa_fardo_trace-cd_acts.
      lwa_mchb-possuiacts               = lwa_fardo_trace-possui_acts.
      lwa_mchb-status                   = lwa_fardo_trace-status_takeup_lote_recente.
      lwa_mchb-takeupmarcadoacts        = lwa_fardo_trace-takeup_marcado_acts.

      APPEND lwa_mchb TO it_mchb.
    ENDLOOP.
  ENDLOOP.

  DELETE it_mchb WHERE cd_sai NOT IN s_cdsai5.

*  IF s_cdsai5[] IS INITIAL.
*
*
*    SELECT matnr werks lgort
*           charg clabs cspem
*      FROM mchb
*      INTO TABLE it_mchb
*       FOR ALL ENTRIES IN it_zsdt0166
*     WHERE matnr = it_zsdt0166-matnr
*       AND werks = it_zsdt0166-werks
*       AND lgort = it_zsdt0166-lgort.
*
*    IF it_mchb[] IS NOT INITIAL.
*      SELECT *
*        FROM zppt0002
*        INTO TABLE it_zppt0002
*         FOR ALL ENTRIES IN it_mchb
*       WHERE acharg    = it_mchb-charg
*         AND cd_safra IN s_charg5. "i_data-valfrom.
*
*      IF sy-subrc EQ 0.
*        r_charg = VALUE #( FOR l IN it_zppt0002 ( sign = 'I' option = 'EQ' low = l-acharg  ) ).
*      ELSE.
*        v_trace = 1.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    SELECT matnr werks lgort
*           charg clabs cspem
*      FROM mchb AS a
*      INTO TABLE it_mchb
*       FOR ALL ENTRIES IN it_zsdt0166
*     WHERE matnr = it_zsdt0166-matnr
*       AND werks = it_zsdt0166-werks
*       AND lgort = it_zsdt0166-lgort
*       AND EXISTS ( SELECT * FROM zppt0002 AS b WHERE acharg EQ a~charg AND cd_sai IN s_cdsai5 ).
*
*    IF it_mchb[] IS NOT INITIAL.
*      SELECT *
*        FROM zppt0002
*        INTO TABLE it_zppt0002
*         FOR ALL ENTRIES IN it_mchb
*       WHERE acharg    = it_mchb-charg
*         AND cd_safra IN s_charg5.
*    ENDIF.
*  ENDIF.
*
*
**-----------------------------
** montar filtro para consulta API
**-----------------------------
*  FREE: t_selec, it_mchb_grp.
*
*  it_zppt0002_grp[] = it_zppt0002[].
*
*  SORT it_zppt0002_grp BY cd_safra cd_sai.
*  DELETE ADJACENT DUPLICATES FROM it_zppt0002_grp
*                        COMPARING cd_safra cd_sai.
*
*  LOOP AT it_mchb           INTO sl_mchb.
*    LOOP AT it_zppt0002_grp INTO sl_zppt0002_grp WHERE acharg = sl_mchb-charg.
*      sl_mchb-safra            = sl_zppt0002_grp-cd_safra.
*      sl_mchb-cd_sai           = COND #( WHEN s_cdsai5[] IS INITIAL THEN abap_off
*                                                                    ELSE sl_zppt0002_grp-cd_sai ).
*      APPEND sl_mchb          TO it_mchb_grp.
*    ENDLOOP.
*  ENDLOOP.
*
*  it_mchb[] = it_mchb_grp[].
*
*  CHECK it_mchb[] IS NOT INITIAL.
*
*  it_mchb_grp[] = it_mchb[].
*
*  SORT it_mchb_grp BY werks lgort safra cd_sai.
*  DELETE ADJACENT DUPLICATES FROM it_mchb_grp
*                        COMPARING werks lgort safra cd_sai.
*
**-----------------------------
** Consulta da API.
**-----------------------------
*  FREE: r_data_ger.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      percentage = 50
*      text       = 'Aguarde...Pesquisando Trace Cotton...'.
*
*  LOOP AT it_mchb_grp INTO sl_mchb_grp.
*
*    TRY .
*        CLEAR: vg_werk, vg_cd_sai, vg_lote, vg_lgort.
*
*        vg_werk   = sl_mchb_grp-werks.
*        vg_cd_sai = sl_mchb_grp-cd_sai.
*        vg_lote   = sl_mchb_grp-lgort.
*        vg_lgort  = sl_mchb_grp-safra.
*
*        zcl_int_acts_tracecotton=>zif_int_acts_tracecotton~get_instance( i_servico =  '23'
*           )->set_fazenda(    EXPORTING i_fazenda = vg_werk
*           )->set_lote(       EXPORTING i_lote    = vg_lgort
*           )->set_lgort(      EXPORTING i_lgort   = vg_lote
*           )->set_cd_sai(     EXPORTING i_cd_sai  = vg_cd_sai
*           )->set_dados_acts( IMPORTING e_return  = r_data ).
*
*        APPEND LINES OF r_data[] TO r_data_ger[].
*
*      CATCH zcx_integracao INTO DATA(ex_integra).
*        LOOP AT it_mchb ASSIGNING FIELD-SYMBOL(<sl_mchb2>) WHERE werks  = sl_mchb_grp-werks
*                                                             AND lgort  = sl_mchb_grp-lgort
*                                                             AND safra  = sl_mchb_grp-safra
*                                                             AND cd_sai = sl_mchb_grp-cd_sai.
*          <sl_mchb2>-icon        = icon_led_red.
*          <sl_mchb2>-status      = 'Fardo não encontrado Trace Cotton'. "abap_false.
*          <sl_mchb2>-possuiacts  = abap_false.
*          <sl_mchb2>-takeupmarcadoacts = abap_false.
*        ENDLOOP.
*        l_erro = abap_true.
*
*      CATCH zcx_error INTO DATA(ex_error).    "  "
*        LOOP AT it_mchb ASSIGNING FIELD-SYMBOL(<sl_mchb3>) WHERE werks  = sl_mchb_grp-werks
*                                                             AND lgort  = sl_mchb_grp-lgort
*                                                             AND safra  = sl_mchb_grp-safra
*                                                             AND cd_sai = sl_mchb_grp-cd_sai.
*          <sl_mchb3>-icon        = icon_led_red.
*          <sl_mchb3>-status      = 'Fardo não encontrado Trace Cotton'. "abap_false.
*          <sl_mchb3>-possuiacts  = abap_false.
*          <sl_mchb3>-takeupmarcadoacts = abap_false.
*        ENDLOOP.
*        l_erro = abap_true.
*    ENDTRY.
*  ENDLOOP.
*
*  r_data[] = r_data_ger[].
*
**-----------------------------
** Monta saida
**-----------------------------
*  LOOP AT it_mchb ASSIGNING FIELD-SYMBOL(<sl_mchb>).
*
*    vl_index = sy-tabix.
*
*    READ TABLE it_zppt0002 INTO DATA(ws_zppt0002) WITH KEY werks  = <sl_mchb>-werks
*                                                           lgort  = <sl_mchb>-lgort
*                                                           acharg = <sl_mchb>-charg.
*    IF sy-subrc EQ 0.
*      READ TABLE r_data INTO DATA(w_data) WITH KEY codigosai = ws_zppt0002-cd_sai.
*
*      IF sy-subrc EQ 0.
*        IF w_data-takeupmarcadoacts EQ abap_true.
*          IF w_data-possuiacts EQ abap_true AND w_data-statustakeuploterecente EQ 'Aprovado'.
*            <sl_mchb>-icon              = icon_led_green.
*            <sl_mchb>-status            = w_data-statustakeuploterecente.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*
*          ELSEIF w_data-possuiacts EQ abap_false AND w_data-statustakeuploterecente NE 'Aprovado'.
*            <sl_mchb>-icon              = icon_led_green. "icon_led_red.
*            <sl_mchb>-status            = w_data-statustakeuploterecente.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*
*          ELSEIF w_data-possuiacts EQ abap_true AND w_data-statustakeuploterecente NE 'Aprovado'.
*            <sl_mchb>-icon              = icon_led_green. "icon_led_red.
*            <sl_mchb>-status            = w_data-statustakeuploterecente.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*
*          ELSEIF w_data-possuiacts EQ abap_false AND w_data-statustakeuploterecente EQ 'Aprovado'.
*            <sl_mchb>-icon              = icon_led_green. "icon_led_red.
*            <sl_mchb>-status            = w_data-statustakeuploterecente.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*          ENDIF.
*        ELSE.
*          IF w_data-statustakeuploterecente EQ 'Aprovado'.
*            <sl_mchb>-icon              = icon_led_green.
*            <sl_mchb>-status            = w_data-statustakeuploterecente.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*
*          ELSEIF w_data-statustakeuploterecente NE 'Aprovado'.
*            <sl_mchb>-icon              = icon_led_green. "icon_led_red.
*            <sl_mchb>-status            = w_data-statustakeuploterecente.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*          ELSE.
*            <sl_mchb>-icon              = icon_led_green. "icon_led_red.
*            <sl_mchb>-status            = w_data-statustakeuploterecente. " abap_false.
*            <sl_mchb>-acts              = w_data-acts.
*            <sl_mchb>-possuiacts        = w_data-possuiacts.
*            <sl_mchb>-takeupmarcadoacts = w_data-takeupmarcadoacts.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        <sl_mchb>-icon                  = icon_led_red.
*        <sl_mchb>-status                = 'Fardo não encontrado Trace Cotton'.
*        <sl_mchb>-acts                  = abap_false.
*        <sl_mchb>-possuiacts            = abap_false.
*        <sl_mchb>-takeupmarcadoacts     = abap_false.
*      ENDIF.
*
*    ELSE.
*      IF v_trace EQ 1.
*        <sl_mchb>-icon                  = icon_led_red. "icon_led_green.
*        <sl_mchb>-status                = 'Fardo não encontrado na ZPPT0002'. "w_data-statustakeuploterecente.
*        <sl_mchb>-acts                  = abap_false.
*        <sl_mchb>-possuiacts            = abap_false.
*        <sl_mchb>-takeupmarcadoacts     = abap_false.
*
*        IF <sl_mchb>-clabs IS INITIAL AND <sl_mchb>-cspem IS NOT INITIAL.
*          <sl_mchb>-clabs = <sl_mchb>-cspem.
*          MODIFY it_mchb FROM <sl_mchb> INDEX vl_index.
*        ENDIF.
*      ELSE.
*        <sl_mchb>-icon                  = icon_led_red.
*        <sl_mchb>-status                = 'Fardo não encontrado na ZPPT0002'. "w_data-statustakeuploterecente.
*        <sl_mchb>-acts                  = abap_false.
*        <sl_mchb>-possuiacts            = abap_false.
*        <sl_mchb>-takeupmarcadoacts     = abap_false.
*      ENDIF.
*    ENDIF.
*
*    IF <sl_mchb>-clabs IS INITIAL AND v_trace EQ 0.
*      <sl_mchb>-clabs = sl_mchb-cspem.
*    ENDIF.
*
*    CLEAR: w_data, ws_zppt0002.
*  ENDLOOP.

ENDFORM.

FORM f_seleciona_dados_expo .
  FREE: it_kna1,it_zsdt0053,
  it_t001w, it_vbfa, it_vbap, it_j_1bnflin, it_j_1bnfdoc, it_zsdt0045, it_zsdt0066,
  it_lfa1, it_makt, it_t001w, it_kna1, it_vbfa, it_vbfa_j, it_j_1bnflin, it_j_1bnfdoc, it_vbfa_66, it_zsdt0001, it_vbfa_8,
  it_vtfa, it_vfkp, it_vttp, it_vttk, it_lfa1, it_zmmt0008.

  SELECT *
    FROM zsdt0051
    INTO TABLE it_zsdt0051
    WHERE vkorg IN p_vkorg
    AND   spart IN p_spart
    AND   vkbur IN p_vkbur
    AND   kunnr IN p_kunnr
    AND   bstkd IN p_bstkd
    AND   data_venda IN p_data
    AND   param_espec IN ( 'A', 'X', 'Z' ) " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ " 'A'-> Algodão AMAGGI, 'X'-> Algodão OTELHAR
    AND   status EQ 'L'.

  PERFORM check_dados.

  CHECK it_zsdt0051[] IS NOT INITIAL.

  SELECT  kunnr mcod3 name1
     FROM kna1
     INTO TABLE it_kna1
     FOR ALL ENTRIES IN it_zsdt0051
     WHERE kunnr = it_zsdt0051-kunnr.

  LOOP AT it_zsdt0051 ASSIGNING FIELD-SYMBOL(<zsdt0051>). <zsdt0051>-objek = <zsdt0051>-nro_sol_ov. ENDLOOP.

  SELECT *
    FROM zsdt0045
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
    FOR ALL ENTRIES IN it_zsdt0051
    WHERE objek EQ it_zsdt0051-objek
     AND  instrucao IN p_instr
     AND safra      IN p_safra
     AND  data_instr IN p_dtins.

  it_0045 = it_zsdt0045.

  SORT it_0045 BY objek bukrs werks instrucao matnr.
  DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING  objek bukrs werks instrucao matnr.

  LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
    <f0045>-quantidade = 0.
    LOOP AT it_zsdt0045 INTO DATA(wa_0045) WHERE objek EQ <f0045>-objek AND
                                                 bukrs EQ <f0045>-bukrs AND
                                                 werks EQ <f0045>-werks AND
                                                 instrucao EQ <f0045>-instrucao AND
                                                 matnr EQ <f0045>-matnr.
      ADD wa_0045-quantidade TO <f0045>-quantidade.
    ENDLOOP.
  ENDLOOP.

  it_zsdt0045 = it_0045.

  "Tabela de Solicitação de Ordem de Venda – Formação de Lote
  SELECT *
    FROM zsdt0053
    INTO TABLE it_zsdt0053
    FOR ALL ENTRIES IN it_zsdt0051
    WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

  LOOP AT it_zsdt0053 ASSIGNING FIELD-SYMBOL(<f_zsdt0053>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <f_zsdt0053>-werks
      IMPORTING
        output = <f_zsdt0053>-lifnr.
  ENDLOOP.

  IF it_zsdt0053[] IS NOT INITIAL.
    SELECT lifnr name1 stcd1
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0053
      WHERE lifnr = it_zsdt0053-lifnr.

    SELECT matnr maktx
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_zsdt0053
      WHERE matnr = it_zsdt0053-matnr
      AND   spras = sy-langu.

    SELECT *
      FROM t001w
      APPENDING TABLE it_t001w
      FOR ALL ENTRIES IN it_zsdt0053
      WHERE werks = it_zsdt0053-werks.

    SELECT vbelv vbeln rfmng vbtyp_n vbtyp_v
     FROM vbfa
     APPENDING CORRESPONDING FIELDS OF TABLE it_vbfa
     FOR ALL ENTRIES IN it_zsdt0053
     WHERE vbelv   = it_zsdt0053-vbeln
     AND   vbtyp_n = 'M'
     AND   vbtyp_v = 'C'.

    LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<vbfa>).  <vbfa>-refkey = <vbfa>-vbeln .   ENDLOOP.

    IF it_vbfa[] IS NOT INITIAL.

      SELECT vbelv vbeln
      FROM vbfa
      APPENDING CORRESPONDING FIELDS OF TABLE it_vbfa_j
      FOR ALL ENTRIES IN it_vbfa
      WHERE vbeln   = it_vbfa-vbeln
      AND   vbtyp_n = 'M'
      AND   vbtyp_v = 'J'.

      SELECT refkey docnum netwr menge
        FROM j_1bnflin
        APPENDING CORRESPONDING FIELDS OF TABLE it_j_1bnflin
        FOR ALL ENTRIES IN it_vbfa
        WHERE refkey = it_vbfa-refkey.

      IF it_j_1bnflin[] IS NOT INITIAL.
        SELECT docnum nfenum docdat brgew ntgew
         FROM j_1bnfdoc
         INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnflin
         WHERE docnum = it_j_1bnflin-docnum
           AND docdat IN l_dtfor.
      ENDIF.
    ENDIF.

    SELECT vbelv vbeln
     FROM vbfa
     INTO CORRESPONDING FIELDS OF TABLE it_vbfa_66
     FOR ALL ENTRIES IN it_zsdt0053
     WHERE vbelv   = it_zsdt0053-vbeln
     AND   vbtyp_n = 'J'
     AND   vbtyp_v = 'C'.

    IF it_vbfa_66[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001
        INTO TABLE it_zsdt0001
        FOR ALL ENTRIES IN it_vbfa_66
        WHERE doc_rem = it_vbfa_66-vbeln
        AND   tp_movimento = 'S'.

      SELECT vbelv vbeln
      FROM vbfa
      INTO TABLE it_vbfa_8
      FOR ALL ENTRIES IN it_vbfa_66
      WHERE vbelv   = it_vbfa_66-vbeln
      AND   vbtyp_n = '8'
      AND   vbtyp_v = 'J'.

      IF it_vbfa_8[] IS NOT INITIAL.
        SELECT  vbelv vbeln
          FROM vtfa
          INTO TABLE it_vtfa
          FOR ALL ENTRIES IN it_vbfa_8
          WHERE vbelv = it_vbfa_8-vbeln.

        IF it_vtfa[] IS NOT INITIAL.
          SELECT  fknum kzwi1 kzwi2
            FROM vfkp
            INTO TABLE it_vfkp
            FOR ALL ENTRIES IN it_vtfa
            WHERE fknum = it_vtfa-vbeln.
        ENDIF.
      ENDIF.

      SELECT  vbeln tknum
      FROM vttp
      INTO TABLE it_vttp
      FOR ALL ENTRIES IN it_vbfa_66
      WHERE vbeln = it_vbfa_66-vbeln.

      IF it_vttp[] IS NOT INITIAL.
        SELECT  tknum  tdlnr
          FROM vttk
          INTO TABLE it_vttk
          FOR ALL ENTRIES IN it_vttp
          WHERE tknum = it_vttp-tknum.
        IF it_vttk[] IS NOT INITIAL.
          SELECT lifnr name1 stcd1
            FROM lfa1
            APPENDING TABLE it_lfa1
            FOR ALL ENTRIES IN it_vttk
            WHERE lifnr = it_vttk-tdlnr.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT it_vbfa IS INITIAL.
      wvbeln-sign = 'I'.
      wvbeln-option = 'EQ'.
      LOOP AT it_vbfa INTO DATA(wvbfa).
        wvbeln-low = wvbfa-vbeln.
        APPEND wvbeln.
      ENDLOOP.
    ENDIF.

    IF wvbeln IS NOT INITIAL.
      SELECT *
        FROM zmmt0008
        INTO TABLE it_zmmt0008
        WHERE vbeln_vf IN wvbeln.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS

FORM f_saida_expo.

  DATA: vcont_lgort         TYPE i,
        vcont_lgort_s       TYPE c LENGTH 4,
        tabix               TYPE sy-tabix,
        tabix_vbfa          TYPE sy-tabix,
        lv_sum_fds          TYPE i,
        despachante         TYPE string,
        corretor            TYPE string,
        saldo               TYPE i,
        qtd_carregados      TYPE i,
        local_carregamento  TYPE name1,
        local_carregamento2 TYPE name1.

  DATA: cnpj_for(18)       TYPE c,
        cnpj_formatado(18) TYPE c.

*-CS2022000332-#83055-05.08.2022-JT-inicio
  DATA: x_ref TYPE REF TO zcl_monta_dados.
  CREATE OBJECT x_ref.
*-CS2022000332-#83055-05.08.2022-JT-fim

  PERFORM aj_zsdt0053.

  LOOP AT it_zsdt0051 INTO wa_zsdt0051.

    CLEAR: corretor, despachante, wa_saida-corretor, wa_saida-despachante, wa_saida-local_carregamento.

    wa_saida-kunnr      = wa_zsdt0051-kunnr.
    wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

    SELECT SINGLE name1 INTO corretor FROM lfa1 WHERE lifnr EQ wa_zsdt0051-correto.

    IF corretor IS NOT INITIAL.
      CONCATENATE wa_zsdt0051-correto '-' corretor INTO wa_saida-corretor SEPARATED BY space.
    ENDIF.
    "WA_SAIDA-CORRETOR = CORRETOR.

    LOOP AT it_zsdt0045 INTO wa_zsdt0045 WHERE objek = wa_zsdt0051-objek.

      wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.
      wa_saida-contrato   = wa_zsdt0045-contrato  .
      wa_saida-instrucao  = wa_zsdt0045-instrucao .
      wa_saida-qtd_ctners  = wa_zsdt0045-qtd_ctners .

      "LOCAL_CARREGAMENTO = OBJ_DADOS->GET_NAME1_WERKS( WA_ZSDT0045 ).

      "WA_SAIDA-LOCAL_CARREGAMENTO = LOCAL_CARREGAMENTO.

      SELECT SINGLE name1 INTO despachante FROM lfa1 WHERE lifnr EQ wa_zsdt0045-cod_despach.

      IF despachante IS NOT INITIAL.
        CONCATENATE wa_zsdt0045-cod_despach '-' despachante INTO wa_saida-despachante SEPARATED BY space.
      ENDIF.
      wa_saida-despachante = despachante.

      IF line_exists( it_saida[
                                nro_sol_ov = wa_zsdt0051-nro_sol_ov
                                instrucao = wa_zsdt0045-instrucao
                                matnr_e = wa_zsdt0045-matnr
                                werks = wa_zsdt0045-werks
                              ] ).
        ADD wa_zsdt0045-quantidade TO wa_saida-fds_inst.
        MODIFY it_saida FROM wa_saida INDEX sy-tabix TRANSPORTING fds_inst.
        CLEAR wa_saida-fds_inst.

      ELSE.
        wa_saida-fds_inst   = wa_zsdt0045-quantidade.
      ENDIF.

      IF line_exists( it_saida[ instrucao = wa_zsdt0045-instrucao ] ).
*        WA_SAIDA-QTD_CTNERS  = WA_ZSDT0045-QTD_CTNERS.
*        MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING QTD_CTNERS.
*        CLEAR WA_SAIDA-QTD_CTNERS.
*
*      ELSE.
        wa_saida-qtd_ctners  = wa_zsdt0045-qtd_ctners .
      ENDIF.

      LOOP AT it_zsdt0053 INTO wa_zsdt0053 WHERE nro_sol_ov EQ wa_zsdt0051-nro_sol_ov
                                              AND instrucao EQ wa_zsdt0045-instrucao
                                              AND matnr     EQ wa_zsdt0045-matnr
                                              AND werks     EQ wa_zsdt0045-werks.


        local_carregamento2 = obj_dados->get_name1_werks( wa_zsdt0045 ).

        "IF local_carregamento2 EQ 'TUCUNARÉ' AND wa_saida-lgort2 NE 'ALGD'.
        IF local_carregamento2 EQ 'TUCUNARÉ' AND wa_saida-lgort NE 'ALGD'.
          local_carregamento = 'CHEROKEE'..
        ELSE.
          local_carregamento = local_carregamento2.
        ENDIF.

        wa_saida-local_carregamento = local_carregamento.

        TRY .
            wa_saida-name1_kun = it_kna1[ kunnr =  wa_zsdt0051-kunnr ]-name1.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

*        DATA(_fardo) = obj_dados->get_qtd_fardos_carregados( input = wa_zsdt0053 input1 = local_carregamento ).

        "DATA(_FARDO) = GET_QTD_FARDOS_CARREGADOS( INPUT = WA_ZSDT0066 INPUT1 = WA_REL-LOCAL_CARREGAMENTO ).
*        ADD _fardo TO qtd_carregados.

        saldo = wa_zsdt0045-quantidade - qtd_carregados.

        "BREAK-POINT.
        IF saldo GT 0.
          wa_saida-status = 'EM ANDAMENTO'.
        ELSE.
          wa_saida-status = 'FINALIZADO'.
        ENDIF.


        wa_saida-werks      = wa_zsdt0053-werks     . "CENTRO FAZ
*        wa_saida-libra_to   = wa_zsdt0066-libra_to  . "
*        wa_saida-usd_to     = wa_zsdt0066-usd_to    . "

        CLEAR: cnpj_for, cnpj_formatado.
        TRY .

            cnpj_for = it_lfa1[ lifnr  = wa_zsdt0053-lifnr ]-stcd1.

            CONCATENATE cnpj_for(2) '.' cnpj_for+2(3) '.' cnpj_for+5(3) '/' cnpj_for+8(4) '-' cnpj_for+12(3) INTO cnpj_formatado.

            wa_saida-stcd1 = cnpj_formatado.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        TRY .
            wa_saida-maktx = it_makt[ matnr = wa_zsdt0053-matnr ]-maktx.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_zsdt0053-matnr
          IMPORTING
            output = wa_saida-matnr_e.

        TRY .
            wa_saida-name1      = it_t001w[ werks = wa_zsdt0053-werks ]-name1. "FAZENDA
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        wa_saida-kunnr     = wa_zsdt0051-kunnr.

        TRY .
            wa_saida-name1_kun = it_kna1[ kunnr =  wa_zsdt0051-kunnr ]-name1.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

        tabix_vbfa = 0.
        LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv = wa_zsdt0053-vbeln.
          ADD 1 TO tabix_vbfa.

          CLEAR: wa_saida-lgort, wa_saida-contador, lv_sum_fds.

          tabix = 0.

          FREE it_bl.
          CLEAR  it_bl.
          LOOP AT it_zmmt0008_bl INTO wa_zmmt0008 WHERE werks     = wa_zsdt0053-werks
                                                  AND   vbeln_vf  = wa_vbfa-vbeln.
            ADD 1 TO tabix.
            wa_saida-lgort = |{ wa_saida-lgort } { wa_zmmt0008-lgort }|.

            IF tabix > 0.
              wa_saida-lgort = |{ wa_saida-lgort },|.
            ENDIF.

            CLEAR: vcont_lgort.
            LOOP AT it_zmmt0008_bl_2 INTO wa_zmmt0008_2 WHERE werks     = wa_zsdt0053-werks
                                                         AND  vbeln_vf  = wa_vbfa-vbeln
                                                         AND  lgort     = wa_zmmt0008-lgort.
              it_bl-werks    = wa_zmmt0008_2-werks.
              it_bl-lgort    = wa_zmmt0008_2-lgort.
              it_bl-vbeln_vf = wa_zmmt0008_2-vbeln_vf.
              it_bl-charg    = wa_zmmt0008_2-charg.
              it_bl-tipo     = wa_zmmt0008_2-tipo_fardo. "Projeto Reestruturação Algodao 2024
              APPEND it_bl.
              CLEAR  it_bl.
              ADD 1 TO vcont_lgort.
            ENDLOOP.
***          Marcos Faneli --> Ch.128554
***          Soma do total de fardos
            ADD vcont_lgort TO lv_sum_fds.

            MOVE vcont_lgort TO vcont_lgort_s.
            wa_saida-contador = |{ wa_saida-contador } { vcont_lgort_s }|.

            IF wa_saida-contador IS NOT INITIAL.
              wa_saida-contador = |{ wa_saida-contador },|.
            ENDIF.

          ENDLOOP.

          SORT it_bl BY werks lgort vbeln_vf charg.
*          DELETE ADJACENT DUPLICATES FROM IT_BL COMPARING WERKS LGORT VBELN_VF.
          DATA: wa_0004 TYPE zppt0004,
                wa_0002 TYPE zppt0002.

          LOOP AT it_bl ASSIGNING FIELD-SYMBOL(<bl>).
            CLEAR wa_0002.

            IF <bl>-tipo IS NOT INITIAL. "Projeto Reestruturação Algodao 2024
              <bl>-tipo = <bl>-tipo.
            ELSE.
              SELECT SINGLE verid werks acharg
                FROM zppt0002
                INTO CORRESPONDING FIELDS OF wa_0002
                WHERE acharg EQ <bl>-charg.

              IF sy-subrc IS INITIAL.
                CLEAR wa_0004.
                SELECT SINGLE tipo
                  FROM zppt0004
                  INTO CORRESPONDING FIELDS OF wa_0004
                 WHERE werks EQ wa_0002-werks
                   AND verid EQ wa_0002-verid
                   AND tipo NE abap_false.

                IF sy-subrc IS INITIAL.
                  <bl>-tipo = wa_0004-tipo.
                ELSE.
*  -CS2022000332-#83055-05.08.2022-JT-inicio
                  <bl>-tipo = x_ref->get_tipo_alterna( wa_0002-acharg ).
*  -CS2022000332-#83055-05.08.2022-JT-fim
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          SORT it_bl BY werks lgort vbeln_vf.
          DELETE it_bl WHERE tipo EQ abap_false.
          DELETE ADJACENT DUPLICATES FROM it_bl COMPARING werks lgort vbeln_vf.

          LOOP AT it_bl.
            wa_saida-tipo = |{ wa_saida-tipo }, { it_bl-tipo }|.
          ENDLOOP.
          SHIFT wa_saida-tipo LEFT DELETING LEADING ','.

          wa_saida-vbeln       = wa_vbfa-vbeln.
          wa_saida-saldo_fds   = lv_sum_fds.
          wa_saida-werks66     = wa_zsdt0053-werks.
          wa_saida-volum       = wa_zsdt0053-volum. "FDS FRM LOTE
          wa_saida-zmeng_lote  = wa_zsdt0053-zmeng. "PESO FRM LOTE

          IF line_exists( it_j_1bnflin[ refkey = wa_vbfa-refkey ] ).
            wa_j_1bnflin = it_j_1bnflin[ refkey = wa_vbfa-refkey ].

            IF line_exists( it_j_1bnfdoc[ docnum = wa_j_1bnflin-docnum ] ).
              wa_j_1bnfdoc = it_j_1bnfdoc[ docnum = wa_j_1bnflin-docnum ].

              wa_saida-netwr         = wa_j_1bnflin-netwr.
              wa_saida-nfnum_lote    = wa_j_1bnfdoc-nfenum. "NOTA FISCAL FRM LOTE
              wa_saida-peso_nf_bruto = wa_j_1bnfdoc-brgew.  "Peso NF Bruto.
              wa_saida-peso_nf_frm   = wa_j_1bnfdoc-ntgew.  "Peso NF FRM.
              wa_saida-data_lote     = wa_j_1bnfdoc-docdat.

            ENDIF.
          ENDIF.

          PERFORM data USING wa_saida-data_lote.

          wa_saida-lgort2      = wa_zsdt0053-lgort.      " Depósito FRM - LG CS2017001463
          wa_saida-vbeln_lote  = wa_zsdt0053-vbeln     . "ORDEM VENDA FRM LOTE
          wa_saida-vlrtot_lote = wa_zsdt0053-vlrtot    . "VALOR TOTAL FRM LOTE
          wa_saida-transp      = ''                    . "TRANSPORTADORA

          IF tabix_vbfa GT 1.
            CLEAR : wa_saida-volum,wa_saida-zmeng_lote,wa_saida-vlrtot_lote.",WA_SAIDA-DATA_LOTE." WA_SAIDA-VBELN_LOTE
          ENDIF.

          CLEAR: wa_saida-kzwi1_tot,wa_saida-kzwi1_val,wa_saida-transp, wa_saida-kzwi1, wa_saida-kzwi1_totl,wa_vbfa_66-vbeln.

          TRY .
              wa_vbfa_j = it_vbfa_j[ vbeln = wa_vbfa-vbeln ].
              wa_vbfa_66-vbeln = wa_vbfa_j-vbelv.
            CATCH cx_sy_itab_line_not_found.
              CLEAR wa_vbfa_j.
          ENDTRY.

          TRY .
              wa_vfkp = it_vfkp[ fknum = it_vtfa[ vbelv = it_vbfa_8[ vbelv = wa_vbfa_j-vbelv ]-vbeln ]-vbeln ].
            CATCH cx_sy_itab_line_not_found.
              CLEAR wa_vfkp.
          ENDTRY.

          wa_saida-kzwi1_totl  = wa_vfkp-kzwi1                         . "TOTAL FRETE LIQUIDO
          wa_saida-kzwi1       = ( wa_saida-kzwi1_totl / wa_vbfa-rfmng ) * 1000. "VALOR FRETE
          wa_saida-kzwi1_tot   = wa_vfkp-kzwi1 + wa_vfkp-kzwi2         . "TOTAL FRETE
          wa_saida-kzwi1_val   = ( wa_saida-kzwi1_tot / wa_vbfa-rfmng ) * 1000. "VALOR FRETE

          TRY .
              wa_saida-transp = it_lfa1[ lifnr = it_vttk[ tknum = it_vttp[ vbeln = wa_vbfa_j-vbelv ]-tknum ]-tdlnr ]-name1.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          CLEAR: wa_saida-placa_cav, wa_saida-mcod3, wa_saida-name1_arm.

          TRY .
              wa_saida-placa_cav   = it_zsdt0001[ vbeln   = wa_zsdt0053-vbeln
                                                  doc_rem = wa_vbfa_66-vbeln  ]-placa_cav . "PLACA TRANSPORTE
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

*          wa_saida-lentrega   = wa_zsdt0066-lentrega   . "LOCAL ENTREGA

*          TRY .
*              wa_saida-mcod3      = it_kna1[ kunnr = wa_zsdt0066-lentrega ]-mcod3          . "CIDADE DESTINO
*              wa_saida-name1_arm  = it_kna1[ kunnr = wa_zsdt0066-lentrega ]-name1          . "
*            CATCH cx_sy_itab_line_not_found.
*          ENDTRY.
          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida-fds_inst. "Para vir FDS Instrução apenas na primeira linha de cada Loop da ZSDT0045
          CLEAR: wa_saida-status, wa_saida-local_carregamento.
          CLEAR wa_saida-tipo.
        ENDLOOP.

        wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

        IF tabix_vbfa = 0.

          CLEAR wa_saida-lgort.

          wa_saida-volum       = wa_zsdt0053-volum     . "FDS FRM LOTE
          wa_saida-lgort2      = wa_zsdt0053-lgort     . "Depósito FRM - LG CS2017001463
          wa_saida-zmeng_lote  = wa_zsdt0053-zmeng     . "PESO FRM LOTE
          wa_saida-vbeln_lote  = wa_zsdt0053-vbeln     . "ORDEM VENDA FRM LOTE
          wa_saida-vlrtot_lote = wa_zsdt0053-vlrtot    . "VALOR TOTAL FRM LOTE
          wa_saida-transp      = ''                    . "TRANSPORTADORA


          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida-fds_inst. "Para vir FDS Instrução apenas na primeira linha de cada Loop da ZSDT0045
          CLEAR: wa_saida-status, wa_saida-local_carregamento.
        ENDIF.

        CLEAR wa_saida.

        wa_saida-contrato   = wa_zsdt0045-contrato  .
        wa_saida-instrucao  = wa_zsdt0045-instrucao .
        wa_saida-qtd_ctners = wa_zsdt0045-qtd_ctners.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<saida>).
    PERFORM peso_origem USING <saida>-werks66     <saida>-vbeln
                     CHANGING <saida>-peso_origem <saida>-t_peso_origem.
  ENDLOOP.

  IF r_lote IS NOT INITIAL.
    DELETE it_saida WHERE nfnum_lote EQ ''.
  ENDIF.

  IF r_expo IS NOT INITIAL.
    DELETE it_saida WHERE data_lote NOT IN l_dtfor.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida_ .

  DATA: vcont_lgort         TYPE i,
        vcont_lgort_s       TYPE c LENGTH 4,
        tabix               TYPE sy-tabix,
        tabix_vbfa          TYPE sy-tabix,
        lv_sum_fds          TYPE i,
        despachante         TYPE string,
        corretor            TYPE string,
        saldo               TYPE i,
        qtd_carregados      TYPE i,
        local_carregamento  TYPE name1,
        local_carregamento2 TYPE name1.

  DATA: cnpj_for(18)       TYPE c,
        cnpj_formatado(18) TYPE c.

*-CS2022000332-#83055-05.08.2022-JT-inicio
  DATA: x_ref TYPE REF TO zcl_monta_dados.
  CREATE OBJECT x_ref.
*-CS2022000332-#83055-05.08.2022-JT-fim

  IF NOT _ok IS INITIAL.
    FREE: it_saida[].
    CLEAR wa_saida.
  ENDIF.

* Ajusta o centro Real da Tabela
  PERFORM aj_zsdt0053.

  LOOP AT it_zsdt0051 INTO wa_zsdt0051.
    CLEAR: corretor, despachante, wa_saida-corretor, wa_saida-despachante, wa_saida-local_carregamento.

    wa_saida-kunnr      = wa_zsdt0051-kunnr.
    wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

    SELECT SINGLE name1 INTO corretor FROM lfa1 WHERE lifnr EQ wa_zsdt0051-correto.

    IF corretor IS NOT INITIAL.
      CONCATENATE wa_zsdt0051-correto '-' corretor INTO wa_saida-corretor SEPARATED BY space.
    ENDIF.
    "WA_SAIDA-CORRETOR = CORRETOR.

    LOOP AT it_zsdt0045 INTO wa_zsdt0045 WHERE objek = wa_zsdt0051-objek.

      wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.
      wa_saida-contrato   = wa_zsdt0045-contrato  .
      wa_saida-instrucao  = wa_zsdt0045-instrucao .
      wa_saida-qtd_ctners  = wa_zsdt0045-qtd_ctners .

      "LOCAL_CARREGAMENTO = OBJ_DADOS->GET_NAME1_WERKS( WA_ZSDT0045 ).

      "WA_SAIDA-LOCAL_CARREGAMENTO = LOCAL_CARREGAMENTO.

      SELECT SINGLE name1 INTO despachante FROM lfa1 WHERE lifnr EQ wa_zsdt0045-cod_despach.

      IF despachante IS NOT INITIAL.
        CONCATENATE wa_zsdt0045-cod_despach '-' despachante INTO wa_saida-despachante SEPARATED BY space.
      ENDIF.
      wa_saida-despachante = despachante.

      IF line_exists( it_saida[
                                nro_sol_ov = wa_zsdt0051-nro_sol_ov
                                instrucao = wa_zsdt0045-instrucao
                                matnr_e = wa_zsdt0045-matnr
                                werks = wa_zsdt0045-werks
                              ] ).
        ADD wa_zsdt0045-quantidade TO wa_saida-fds_inst.
        MODIFY it_saida FROM wa_saida INDEX sy-tabix TRANSPORTING fds_inst.
        CLEAR wa_saida-fds_inst.

      ELSE.
        wa_saida-fds_inst   = wa_zsdt0045-quantidade.
      ENDIF.

      IF line_exists( it_saida[ instrucao = wa_zsdt0045-instrucao ] ).
*        WA_SAIDA-QTD_CTNERS  = WA_ZSDT0045-QTD_CTNERS.
*        MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING QTD_CTNERS.
*        CLEAR WA_SAIDA-QTD_CTNERS.
*
*      ELSE.
        wa_saida-qtd_ctners  = wa_zsdt0045-qtd_ctners .
      ENDIF.

      LOOP AT it_zsdt0066 INTO wa_zsdt0066 WHERE nro_sol_ov EQ wa_zsdt0051-nro_sol_ov
                                              AND instrucao EQ wa_zsdt0045-instrucao
                                              AND matnr     EQ wa_zsdt0045-matnr
                                              AND werks     EQ wa_zsdt0045-werks.

        local_carregamento2 = obj_dados->get_name1_werks( wa_zsdt0045 ).

        IF local_carregamento2 EQ 'TUCUNARÉ' AND wa_saida-lgort2 NE 'ALGD'.
          local_carregamento = 'CHEROKEE'..
        ELSE.
          local_carregamento = local_carregamento2.
        ENDIF.

        wa_saida-local_carregamento = local_carregamento.

        TRY .
            wa_saida-name1_kun = it_kna1[ kunnr =  wa_zsdt0051-kunnr ]-name1.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        DATA(_fardo) = obj_dados->get_qtd_fardos_carregados( input = wa_zsdt0066 input1 = local_carregamento ).

        "DATA(_FARDO) = GET_QTD_FARDOS_CARREGADOS( INPUT = WA_ZSDT0066 INPUT1 = WA_REL-LOCAL_CARREGAMENTO ).
        ADD _fardo TO qtd_carregados.

        saldo = wa_zsdt0045-quantidade - qtd_carregados.

        "BREAK-POINT.
        IF saldo GT 0.
          wa_saida-status = 'EM ANDAMENTO'.
        ELSE.
          wa_saida-status = 'FINALIZADO'.
        ENDIF.


        wa_saida-werks      = wa_zsdt0066-werks     . "CENTRO FAZ
        wa_saida-libra_to   = wa_zsdt0066-libra_to  . "
        wa_saida-usd_to     = wa_zsdt0066-usd_to    . "

        CLEAR: cnpj_for, cnpj_formatado.
        TRY .

            cnpj_for = it_lfa1[ lifnr  = wa_zsdt0066-lifnr ]-stcd1.

            CONCATENATE cnpj_for(2) '.' cnpj_for+2(3) '.' cnpj_for+5(3) '/' cnpj_for+8(4) '-' cnpj_for+12(3) INTO cnpj_formatado.

            wa_saida-stcd1 = cnpj_formatado.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        TRY .
            wa_saida-maktx = it_makt[ matnr = wa_zsdt0066-matnr ]-maktx.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_zsdt0066-matnr
          IMPORTING
            output = wa_saida-matnr_e.

        TRY .
            wa_saida-name1      = it_t001w[ werks = wa_zsdt0066-werks ]-name1. "FAZENDA
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        wa_saida-kunnr     = wa_zsdt0051-kunnr.

        TRY .
            wa_saida-name1_kun = it_kna1[ kunnr =  wa_zsdt0051-kunnr ]-name1.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

        tabix_vbfa = 0.
        LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv = wa_zsdt0066-vbeln.
          ADD 1 TO tabix_vbfa.

          CLEAR: wa_saida-lgort, wa_saida-contador, lv_sum_fds.

          tabix = 0.

          FREE it_bl.
          CLEAR  it_bl.
          LOOP AT it_zmmt0008_bl INTO wa_zmmt0008 WHERE werks     = wa_zsdt0066-werks
                                                  AND   vbeln_vf  = wa_vbfa-vbeln.
            ADD 1 TO tabix.
            wa_saida-lgort = |{ wa_saida-lgort } { wa_zmmt0008-lgort }|.

            IF tabix > 0.
              wa_saida-lgort = |{ wa_saida-lgort },|.
            ENDIF.

            CLEAR: vcont_lgort.
            LOOP AT it_zmmt0008_bl_2 INTO wa_zmmt0008_2 WHERE werks     = wa_zsdt0066-werks
                                                         AND  vbeln_vf  = wa_vbfa-vbeln
                                                         AND  lgort     = wa_zmmt0008-lgort.
              it_bl-werks    = wa_zmmt0008_2-werks.
              it_bl-lgort    = wa_zmmt0008_2-lgort.
              it_bl-vbeln_vf = wa_zmmt0008_2-vbeln_vf.
              it_bl-charg    = wa_zmmt0008_2-charg.
              it_bl-tipo     = wa_zmmt0008_2-tipo_fardo.
              APPEND it_bl.
              CLEAR  it_bl.
              ADD 1 TO vcont_lgort.
            ENDLOOP.
***          Marcos Faneli --> Ch.128554
***          Soma do total de fardos
            ADD vcont_lgort TO lv_sum_fds.

            MOVE vcont_lgort TO vcont_lgort_s.
            wa_saida-contador = |{ wa_saida-contador } { vcont_lgort_s }|.

            IF wa_saida-contador IS NOT INITIAL.
              wa_saida-contador = |{ wa_saida-contador },|.
            ENDIF.

          ENDLOOP.

          SORT it_bl BY werks lgort vbeln_vf charg.
*          DELETE ADJACENT DUPLICATES FROM IT_BL COMPARING WERKS LGORT VBELN_VF.
          DATA: wa_0004 TYPE zppt0004,
                wa_0002 TYPE zppt0002.

          LOOP AT it_bl ASSIGNING FIELD-SYMBOL(<bl>).

            IF <bl>-tipo IS NOT INITIAL.
              <bl>-tipo = <bl>-tipo.
            ELSE.
              CLEAR wa_0002.
              SELECT SINGLE verid werks acharg
                FROM zppt0002
                INTO CORRESPONDING FIELDS OF wa_0002
                WHERE acharg EQ <bl>-charg.

              IF sy-subrc IS INITIAL.
                CLEAR wa_0004.
                SELECT SINGLE tipo
                  FROM zppt0004
                  INTO CORRESPONDING FIELDS OF wa_0004
                 WHERE werks EQ wa_0002-werks
                   AND verid EQ wa_0002-verid
                   AND tipo NE abap_false.

                IF sy-subrc IS INITIAL.
                  <bl>-tipo = wa_0004-tipo.
                ELSE.
*-CS2022000332-#83055-05.08.2022-JT-inicio
                  <bl>-tipo = x_ref->get_tipo_alterna( wa_0002-acharg ).
*-CS2022000332-#83055-05.08.2022-JT-fim
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          SORT it_bl BY werks lgort vbeln_vf.
          DELETE it_bl WHERE tipo EQ abap_false.
          DELETE ADJACENT DUPLICATES FROM it_bl COMPARING werks lgort vbeln_vf.

          LOOP AT it_bl.
            wa_saida-tipo = |{ wa_saida-tipo }, { it_bl-tipo }|.
          ENDLOOP.
          SHIFT wa_saida-tipo LEFT DELETING LEADING ','.

          wa_saida-vbeln       = wa_vbfa-vbeln.
          wa_saida-saldo_fds   = lv_sum_fds.
          wa_saida-werks66     = wa_zsdt0066-werks.
          wa_saida-volum       = wa_zsdt0066-volum. "FDS FRM LOTE
          wa_saida-zmeng_lote  = wa_zsdt0066-zmeng. "PESO FRM LOTE

          IF line_exists( it_j_1bnflin[ refkey = wa_vbfa-refkey ] ).
            wa_j_1bnflin = it_j_1bnflin[ refkey = wa_vbfa-refkey ].

            IF line_exists( it_j_1bnfdoc[ docnum = wa_j_1bnflin-docnum ] ).
              wa_j_1bnfdoc = it_j_1bnfdoc[ docnum = wa_j_1bnflin-docnum ].

              wa_saida-netwr       = wa_j_1bnflin-netwr.
              wa_saida-nfnum_lote  = wa_j_1bnfdoc-nfenum. "NOTA FISCAL FRM LOTE
*              WA_SAIDA-PESO_NF_FRM = WA_J_1BNFDOC-BRGEW.  "Peso NF FRM.
              wa_saida-peso_nf_frm = wa_j_1bnfdoc-ntgew.  "Peso NF FRM.
              wa_saida-data_lote   = wa_j_1bnfdoc-docdat.

            ENDIF.
          ENDIF.

          PERFORM data USING wa_saida-data_lote.

          wa_saida-lgort2      = wa_zsdt0066-lgort.      " Depósito FRM - LG CS2017001463
          wa_saida-vbeln_lote  = wa_zsdt0066-vbeln     . "ORDEM VENDA FRM LOTE
          wa_saida-vlrtot_lote = wa_zsdt0066-vlrtot    . "VALOR TOTAL FRM LOTE
          wa_saida-transp      = ''                    . "TRANSPORTADORA

          IF tabix_vbfa GT 1.
            CLEAR : wa_saida-volum,wa_saida-zmeng_lote,wa_saida-vlrtot_lote.",WA_SAIDA-DATA_LOTE." WA_SAIDA-VBELN_LOTE
          ENDIF.

          CLEAR: wa_saida-kzwi1_tot,wa_saida-kzwi1_val,wa_saida-transp, wa_saida-kzwi1, wa_saida-kzwi1_totl,wa_vbfa_66-vbeln.

          TRY .
              wa_vbfa_j = it_vbfa_j[ vbeln = wa_vbfa-vbeln ].
              wa_vbfa_66-vbeln = wa_vbfa_j-vbelv.
            CATCH cx_sy_itab_line_not_found.
              CLEAR wa_vbfa_j.
          ENDTRY.

          TRY .
              wa_vfkp = it_vfkp[ fknum = it_vtfa[ vbelv = it_vbfa_8[ vbelv = wa_vbfa_j-vbelv ]-vbeln ]-vbeln ].
            CATCH cx_sy_itab_line_not_found.
              CLEAR wa_vfkp.
          ENDTRY.

          wa_saida-kzwi1_totl  = wa_vfkp-kzwi1                         . "TOTAL FRETE LIQUIDO
          wa_saida-kzwi1       = ( wa_saida-kzwi1_totl / wa_vbfa-rfmng ) * 1000. "VALOR FRETE
          wa_saida-kzwi1_tot   = wa_vfkp-kzwi1 + wa_vfkp-kzwi2         . "TOTAL FRETE
          wa_saida-kzwi1_val   = ( wa_saida-kzwi1_tot / wa_vbfa-rfmng ) * 1000. "VALOR FRETE

          TRY .
              wa_saida-transp = it_lfa1[ lifnr = it_vttk[ tknum = it_vttp[ vbeln = wa_vbfa_j-vbelv ]-tknum ]-tdlnr ]-name1.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          CLEAR: wa_saida-placa_cav, wa_saida-mcod3, wa_saida-name1_arm.

          TRY .
              wa_saida-placa_cav   = it_zsdt0001[ vbeln   = wa_zsdt0066-vbeln
                                                  doc_rem = wa_vbfa_66-vbeln  ]-placa_cav . "PLACA TRANSPORTE
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          wa_saida-lentrega   = wa_zsdt0066-lentrega   . "LOCAL ENTREGA

          TRY .
              wa_saida-mcod3      = it_kna1[ kunnr = wa_zsdt0066-lentrega ]-mcod3          . "CIDADE DESTINO
              wa_saida-name1_arm  = it_kna1[ kunnr = wa_zsdt0066-lentrega ]-name1          . "
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida-fds_inst. "Para vir FDS Instrução apenas na primeira linha de cada Loop da ZSDT0045
          CLEAR: wa_saida-status, wa_saida-local_carregamento.
          CLEAR wa_saida-tipo.
        ENDLOOP.

        wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

        IF tabix_vbfa = 0.

          CLEAR wa_saida-lgort.

          wa_saida-volum       = wa_zsdt0066-volum     . "FDS FRM LOTE
          wa_saida-lgort2      = wa_zsdt0066-lgort     . "Depósito FRM - LG CS2017001463
          wa_saida-zmeng_lote  = wa_zsdt0066-zmeng     . "PESO FRM LOTE
          wa_saida-vbeln_lote  = wa_zsdt0066-vbeln     . "ORDEM VENDA FRM LOTE
          wa_saida-vlrtot_lote = wa_zsdt0066-vlrtot    . "VALOR TOTAL FRM LOTE
          wa_saida-transp      = ''                    . "TRANSPORTADORA


          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida-fds_inst. "Para vir FDS Instrução apenas na primeira linha de cada Loop da ZSDT0045
          CLEAR: wa_saida-status, wa_saida-local_carregamento.
        ENDIF.

        CLEAR wa_saida.

        wa_saida-contrato   = wa_zsdt0045-contrato  .
        wa_saida-instrucao  = wa_zsdt0045-instrucao .
        wa_saida-qtd_ctners = wa_zsdt0045-qtd_ctners.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<saida>).
*
    PERFORM peso_origem USING <saida>-werks66     <saida>-vbeln
                     CHANGING <saida>-peso_origem <saida>-t_peso_origem.
    IF NOT _ok IS INITIAL.
      <saida>-hist = abap_true.
      <saida>-color = 'C500'.
    ENDIF.
*
  ENDLOOP.

  IF r_lote IS NOT INITIAL.
    DELETE it_saida WHERE nfnum_lote EQ ''.
  ENDIF.

  IF NOT _ok IS INITIAL.

    LOOP AT it_saida ASSIGNING <saida>.
      LOOP AT saida ASSIGNING FIELD-SYMBOL(<saida1>).

        <saida1>-hist  = abap_true.
        <saida1>-color = 'C500'.

        IF <saida> EQ <saida1>.
          <saida>-hist  = abap_true.
          <saida>-color = abap_false.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .

  CALL SCREEN 0100.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Module  M_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_pbo OUTPUT.
  SET TITLEBAR  'ZFTITLE'.
  IF r_tree = abap_true.
    IF tree1 IS INITIAL.
      PERFORM init_tree.
    ENDIF.
  ELSE.
    PERFORM init_alv.
  ENDIF.
  SET PF-STATUS 'Z001'.
ENDMODULE.                 " M_PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree .
  PERFORM build_fieldcatalog.

  PERFORM build_sort_table.

  CREATE OBJECT obj_dyndoc_id
    EXPORTING
      no_margins = abap_true.

  PERFORM zf_alv_header .

  CALL METHOD obj_dyndoc_id->merge_document.

  CALL METHOD obj_dyndoc_id->display_document
    EXPORTING
      reuse_control      = abap_true
      parent             = editcontainer
    EXCEPTIONS
      html_display_error = 1.

* create container for alv-tree
  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.
  l_tree_container_name = 'TREE1'.

  CREATE OBJECT l_custom_container
    EXPORTING
      container_name              = l_tree_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

* create tree control
  CREATE OBJECT tree1
    EXPORTING
      i_parent                    = l_custom_container
      i_node_selection_mode       = cl_gui_column_tree=>node_sel_mode_multiple
      i_item_selection            = abap_true
      i_no_html_header            = abap_true
      i_no_toolbar                = abap_false
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant = VALUE #( report = sy-repid ).

* register events
  PERFORM register_events.

* create hierarchy
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      i_background_id = 'ALV_BACKGROUND'
      i_save          = 'A'
      is_variant      = ls_variant
    CHANGING
      it_sort         = gt_sort
      it_outtab       = it_saida
      it_fieldcatalog = it_fieldcat.

* expand first level
  CALL METHOD tree1->expand_tree
    EXPORTING
      i_level = 1.

* optimize column-width
  CALL METHOD tree1->column_optimize
    EXPORTING
      i_start_column = tree1->c_hierarchy_column_name
      i_end_column   = tree1->c_hierarchy_column_name.

  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

  lt_events = VALUE #( (
                        eventid = cl_gui_column_tree=>eventid_node_double_click
                        appl_event = abap_true
                     ) ).

  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

ENDFORM.                    " INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  DATA direcao TYPE char1 VALUE abap_true.

  CASE abap_true.
*-CS2023000189-31.05.2023-#108695-JT-inicio
    WHEN r_acts.
      it_fieldcat =
      VALUE #(
      ( col_pos = obj_dados->get_seq( ) fieldname = 'ICON'                scrtext_l = 'Status'                      outputlen = 10 icon = abap_true       )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'WERKS'               scrtext_l = 'Cod.Faz'                     outputlen = 10                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'SAFRA'               scrtext_l = 'Safra'                       outputlen = 05                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'LGORT'               scrtext_l = 'Depósito'                    outputlen = 10                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'CHARG'               scrtext_l = 'Fardinho'                    outputlen = 12                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'CLABS'               scrtext_l = 'Estoque Util.Livre'          outputlen = 15                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'CD_SAI'              scrtext_l = 'Cd Sai'                      outputlen = 20                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'ACTS'                scrtext_l = 'Trace: ACTS'                 outputlen = 25                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'POSSUIACTS'          scrtext_l = 'Trace: Possui ACTS'          outputlen = 20 checkbox = abap_true   )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'STATUS'              scrtext_l = 'Trace: TakeUp Lote Recente'  outputlen = 35                        )
      ( col_pos = obj_dados->get_seq( ) fieldname = 'TAKEUPMARCADOACTS'   scrtext_l = 'Trace: Takeup Marcado ACTS'  outputlen = 26 checkbox = abap_true   )
      ).

      LOOP AT it_fieldcat ASSIGNING FIELD-SYMBOL(<fieldt>).
        <fieldt>-ref_table  = abap_false.
        <fieldt>-scrtext_s  =  <fieldt>-scrtext_m = <fieldt>-scrtext_l.
        <fieldt>-col_opt    = abap_false.
        <fieldt>-edit       = abap_false.
        <fieldt>-key        = abap_false.
      ENDLOOP.
*-CS2023000189-31.05.2023-#108695-JT-fim

    WHEN OTHERS.
      CASE direcao.
        WHEN abap_true.
          it_fieldcat =
          VALUE #(
          ( col_pos = obj_dados->get_seq( ) fieldname = 'WERKS'               scrtext_l = 'Cod.Faz'                                                                              )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'NAME1'               scrtext_l = 'Fazenda'                                                                              )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'STCD1'               scrtext_l = 'CNPJ'                                                                                 )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'KUNNR'               scrtext_l = 'Cod.Cliente'                                                                          )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'NAME1_KUN'           scrtext_l = 'Cliente'                                                                              )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'CONTRATO'            scrtext_l = 'Contrato'                                                                             )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'LOCAL_CARREGAMENTO'  scrtext_l = 'Local Carregamento'                                                                   )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'INSTRUCAO'           scrtext_l = 'Instrução'                                                                            )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'QTD_CTNERS'          scrtext_l = 'QTD Containers'                                                                            )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'STATUS'              scrtext_l = 'Status'                                                                               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'FDS_INST'            scrtext_l = 'FDS Instrução'       colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'MATNR_E'             scrtext_l = 'Material'            colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'MAKTX'               scrtext_l = 'Descr. Material'     colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'ANO'                 scrtext_l = 'Ano'                                                                                  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'MES'                 scrtext_l = 'Mês'                                                                                  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'LGORT'               scrtext_l = 'Nº BLOCO'            colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' hotspot = 'X' )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'VOLUM'               scrtext_l = 'FDS FRM LOTE'        colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'SAFRA'               scrtext_l = 'Safra'               colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'CONTADOR'            scrtext_l = 'FDS REMESSA'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'TIPO'                scrtext_l = 'TIPO'                colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'SALDO_FDS'           scrtext_l = 'Total FDS Remessa'   colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'PESO_ORIGEM'         scrtext_l = 'Peso Origem'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'T_PESO_ORIGEM'       scrtext_l = 'Total Peso Origem'   colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'ZMENG_LOTE'          scrtext_l = 'PESO FRM LOTE'       colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'DATA_LOTE'  	        scrtext_l = 'DATA NF FRM LOTE'    colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'VBELN_LOTE'          scrtext_l = 'ORDEM FRM L.'        colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'LGORT2'              scrtext_l = 'Depóstio FRM'        colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'VLRTOT_LOTE'         scrtext_l = 'VLR TOTAL FRM L.'    colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'NETWR'               scrtext_l = 'VLR. NF FRM'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'LIBRA_TO'            scrtext_l = 'C.USD LP'            colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'USD_TO'              scrtext_l = 'Vlr. USD TO'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'NFNUM_LOTE'          scrtext_l = 'NF FRM LOTE'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'PESO_NF_FRM'         scrtext_l = 'Peso NF FRM'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'PESO_NF_BRUTO'       scrtext_l = 'Peso NF Bruto'       colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'KZWI1_VAL'           scrtext_l = 'VALOR FRETE BRUTO'   colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'KZWI1'               scrtext_l = 'VALOR FRETE LIQUIDO' colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'KZWI1_TOT'  	        scrtext_l = 'TOTAL FRETE BRUTO'   colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'KZWI1_TOTL'          scrtext_l = 'TOTAL FRETE LIQUIDO' colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M' do_sum = 'X'  )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'PLACA_CAV'  	        scrtext_l = 'PLACA TRANSPORTE'    colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'TRANSP'              scrtext_l = 'TRANSPORTADORA'      colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'LENTREGA'            scrtext_l = 'ARMAZEM'             colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'NAME1_ARM'  	        scrtext_l = 'DESCRIÇÃO ARMAZEM'   colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'MCOD3'               scrtext_l = 'CIDADE DESTINO'      colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'DESPACHANTE'         scrtext_l = 'Despachante'         colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ( col_pos = obj_dados->get_seq( ) fieldname = 'CORRETOR'            scrtext_l = 'Corretor'            colddictxt = 'M' selddictxt = 'M' tipddictxt = 'M'               )
          ).

          LOOP AT it_fieldcat ASSIGNING FIELD-SYMBOL(<field>).

            <field>-ref_table  = abap_false.
            <field>-scrtext_s =  <field>-scrtext_m = <field>-scrtext_l.
            <field>-col_opt = abap_true.
            <field>-edit = abap_false.
            <field>-key = abap_false.

          ENDLOOP.

        WHEN OTHERS.
          PERFORM modelo1.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort_table .

  gt_sort =
         VALUE #(
                  ( spos = 1  fieldname = 'WERKS'              up = abap_true )
                  ( spos = 2  fieldname = 'NAME1'              up = abap_true )
                  ( spos = 3  fieldname = 'STCD1'              up = abap_true )
                  ( spos = 4  fieldname = 'KUNNR'              up = abap_true )
                  ( spos = 5  fieldname = 'NAME1_KUN'          up = abap_true )
                  ( spos = 6  fieldname = 'CONTRATO'           up = abap_true )
                  ( spos = 7  fieldname = 'STATUS'             up = abap_true )
                  ( spos = 8  fieldname = 'INSTRUCAO'          up = abap_true )
                  ( spos = 9  fieldname = 'QTD_CTNERS'         up = abap_true )
                  ( spos = 10 fieldname = 'FDS_INST'           up = abap_true )
                  ( spos = 11 fieldname = 'MATNR_E'            up = abap_true )
                  ( spos = 12 fieldname = 'MAKTX'              up = abap_true )
                  ( spos = 13 fieldname = 'LOCAL_CARREGAMENTO' up = abap_true )
                ).

  bl_sort =
         VALUE #(
                  ( spos = 1  fieldname = 'LGORT'     up = abap_true subtot = abap_true )
                ).

ENDFORM.                    " BUILD_SORT_TABLE


*&---------------------------------------------------------------------*
*&      Form  build_header
*&---------------------------------------------------------------------*
*       build table for header
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  APPEND VALUE #(
                  typ  = 'H'
                  info = 'Relatório Gerencial - Algodão'
                ) TO pt_list_commentary.

  p_logo = 'LOGOMAGGI_193_95'.

ENDFORM.                    "build_comment


*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*  Handling the events in the ALV Tree control in backend
*----------------------------------------------------------------------*
FORM register_events.
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

  lt_events = VALUE #(
                       ( eventid = cl_gui_column_tree=>eventid_node_context_menu_req )
                       ( eventid = cl_gui_column_tree=>eventid_item_context_menu_req )
                       ( eventid = cl_gui_column_tree=>eventid_header_context_men_req )
                       ( eventid = cl_gui_column_tree=>eventid_expand_no_children )
                       ( eventid = cl_gui_column_tree=>eventid_header_click )
                       ( eventid = cl_gui_column_tree=>eventid_item_keypress )
                     ).

  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

* set Handler
  DATA: l_event_receiver TYPE REF TO lcl_tree_event_receiver.
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->on_add_hierarchy_node
                                                        FOR tree1.
ENDFORM.                               " register_events
*&---------------------------------------------------------------------*
*&      Module  M_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_pai INPUT.

  CASE ok_code.
    WHEN 'EXIT' OR 'CANCEL'.
      IF r_tree = 'X'.
        PERFORM exit_program.
      ELSE.
        LEAVE TO SCREEN 0.
*       LEAVE PROGRAM.
      ENDIF.
    WHEN 'BACK' OR 'UP'.
      REFRESH it_saida.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.                 " M_PAI  INPUT

*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       free object and leave program
*----------------------------------------------------------------------*
FORM exit_program.

  CALL METHOD tree1->free.
  LEAVE PROGRAM.

ENDFORM.                               " exit_program
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv .

  IF cl_grid IS INITIAL.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = abap_true.

    PERFORM build_fieldcatalog.
    PERFORM build_sort_table.

    CREATE OBJECT cl_grid
      EXPORTING
        i_parent = cl_container_95.

    DATA: ls_variant TYPE disvariant.

    ls_variant = VALUE #( report = sy-repid ).

    CLEAR: wa_layout.
    wa_layout = VALUE #(
                        zebra      = abap_true
                        no_rowins  = abap_true
                        info_fname = 'COLOR'
                        sel_mode   = 'C'
                       ).

*-CS2023000189-31.05.2023-#108695-JT-inicio
    CASE abap_true.
      WHEN r_acts.
        CALL METHOD cl_grid->set_table_for_first_display
          EXPORTING
            is_variant      = ls_variant
            is_layout       = wa_layout
            i_save          = 'A'
            i_default       = 'X'
          CHANGING
            it_fieldcatalog = it_fieldcat[]
*           it_sort         = gt_sort[]
            it_outtab       = it_mchb[].
*-CS2023000189-31.05.2023-#108695-JT-fim

      WHEN OTHERS.
        CALL METHOD cl_grid->set_table_for_first_display
          EXPORTING
            is_variant      = ls_variant
            is_layout       = wa_layout
            i_save          = 'A'
            i_default       = 'X'
          CHANGING
            it_fieldcatalog = it_fieldcat[]
            it_sort         = gt_sort[]
            it_outtab       = it_saida[].
    ENDCASE.

    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>catch_hotspot FOR cl_grid.
  ENDIF.
ENDFORM.                    " INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv_header .
ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

  DATA: event       TYPE cntl_simple_event,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.

  wa_stable = VALUE #( row = abap_true ).
  wa_layout = VALUE #(
                       zebra      = abap_true
                       no_rowmark = abap_true
                       info_fname = 'COLOR'
                       no_toolbar = space
                       grid_title = abap_false
                     ).

  "GRID1
  IF obg_conteiner IS INITIAL.
    CREATE OBJECT obg_conteiner
      EXPORTING
        container_name = 'CC_BLOCO'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = obg_conteiner.

    PERFORM montar_layout.

    FREE: tl_function.
    tl_function =
            VALUE #(
                    ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                    ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                    ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                    ( cl_gui_alv_grid=>mc_fc_loc_paste )
                    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                    ( cl_gui_alv_grid=>mc_fc_loc_undo )
                    ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                    ( cl_gui_alv_grid=>mc_fc_loc_copy )
                    ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                    ( cl_gui_alv_grid=>mc_fc_loc_cut )
                   ).

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_sort              = bl_sort[]
        it_outtab            = tg_bloco[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    PERFORM montar_layout.
    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 ' '           ' '        'TG_BLOCO' 'LGORT'           'Bloco'          '12' ' ' ' ' ' ',
        2 ' '           ' '        'TG_BLOCO' 'CHARG'           'Fardo'          '12' ' ' ' ' ' ',
        3 ' '           ' '        'TG_BLOCO' 'TIPO'            'Tipo Fardo'     '12' ' ' 'X' ' ',
        4 ' '           ' '        'TG_BLOCO' 'CONTA'           'Qtd. FDS'       '12' ' ' 'X' ' ',
        5 ' '           ' '        'TG_BLOCO' 'MENGE'           'Peso'           '15' ' ' 'X' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  APPEND VALUE #(
                  fieldname     = p_field
                  tabname       = p_tabname
                  ref_table     = p_ref_tabname
                  ref_field     = p_ref_fieldname
                  key           = abap_false
                  edit          = p_edit
                  do_sum        = p_sum
                  col_pos       = p_col_pos
                  outputlen     = COND #( WHEN p_outputlen IS NOT INITIAL THEN p_outputlen ELSE abap_false )
                  no_out        = abap_false
                  reptext       = p_scrtext_l
                  scrtext_s     = p_scrtext_l
                  scrtext_m     = p_scrtext_l
                  scrtext_l     = p_scrtext_l
                  emphasize     = p_emphasize
                ) TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok-code.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      SET SCREEN 0.
    WHEN c_exit.
      LEAVE PROGRAM.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'Z002'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT

FORM historico_instrucao.

  DATA(hist) = it_saida[].

  LOOP AT it_saida INTO wa_saida.
    APPEND VALUE #( sign = 'I'
                    option = 'BT'
                    low = wa_saida-instrucao
                    high = wa_saida-instrucao ) TO instrucao.
  ENDLOOP.

  SORT instrucao BY low.

  DELETE instrucao WHERE low EQ ''.
  DELETE ADJACENT DUPLICATES FROM instrucao COMPARING low.

  SELECT *
    FROM zsdt0045
    INTO TABLE @DATA(it_0045)
    WHERE instrucao IN @instrucao
     AND safra      IN @p_safra.

  SELECT *
    FROM zsdt0051
    INTO TABLE it_zsdt0051
    FOR ALL ENTRIES IN it_0045
    WHERE nro_sol_ov EQ it_0045-objek(10).

  DELETE it_zsdt0051 WHERE nro_sol_ov EQ ''.
  CLEAR: saida[].
  APPEND LINES OF it_saida[] TO saida[].

ENDFORM.

FORM peso_origem USING p_werks p_vbeln CHANGING svalor ivalor.

  DATA: convert TYPE char128,
        p_2(13) TYPE p DECIMALS 2,
        f_g     TYPE zsded038.

  FREE tg_bloco.

  LOOP AT it_zmmt0008 INTO wa_zmmt0008 WHERE werks    = p_werks
                                       AND   vbeln_vf = p_vbeln.

    wg_bloco-lgort = wa_zmmt0008-lgort.
    wg_bloco-menge = wa_zmmt0008-menge.

    COLLECT wg_bloco INTO tg_bloco.
    CLEAR wg_bloco.

  ENDLOOP.

  SORT tg_bloco BY lgort.

  LOOP AT tg_bloco INTO wg_bloco.

    convert = wg_bloco-menge.
    CONDENSE convert NO-GAPS.

    p_2 = convert.
    WRITE p_2 TO f_g.
    CONDENSE f_g NO-GAPS.

    IF sy-tabix EQ 1.
      svalor = |{ f_g }|.
    ELSE.
      svalor = |{ svalor }, { f_g }|.
    ENDIF.

    ADD wg_bloco-menge TO ivalor.

  ENDLOOP.

ENDFORM.

FORM aj_zsdt0053.

  it_zmmt0008_bl[]   = it_zmmt0008[].
  it_zmmt0008_bl_2[] = it_zmmt0008_bl[].

  SORT: it_zmmt0008_bl     BY werks vbeln_vf lgort,
        it_lfa1            BY lifnr.

  DELETE ADJACENT DUPLICATES FROM it_zmmt0008_bl COMPARING werks vbeln_vf lgort.

ENDFORM.

FORM data USING p_da_atual.


  wa_saida-data = p_da_atual. "DATA EXP
  wa_saida-ano  = p_da_atual+0(4).

  CASE p_da_atual+4(2).
    WHEN '01'. wa_saida-mes   = 'Janeiro'.
    WHEN '02'. wa_saida-mes   = 'Fevereiro'.
    WHEN '03'. wa_saida-mes   = 'Março'.
    WHEN '04'. wa_saida-mes   = 'Abril'.
    WHEN '05'. wa_saida-mes   = 'Maio'.
    WHEN '06'. wa_saida-mes   = 'Junho'.
    WHEN '07'. wa_saida-mes   = 'Julho'.
    WHEN '08'. wa_saida-mes   = 'Agosto'.
    WHEN '09'. wa_saida-mes   = 'Setembro'.
    WHEN '10'. wa_saida-mes   = 'Outubro'.
    WHEN '11'. wa_saida-mes   = 'Novembro'.
    WHEN '12'. wa_saida-mes   = 'Dezembro'.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_dados .
  DATA: r_sol_ov TYPE RANGE OF zsdt0051-nro_sol_ov WITH HEADER LINE.

  IF NOT p_instr IS INITIAL OR NOT p_dtins IS INITIAL.

    LOOP AT it_zsdt0051 ASSIGNING FIELD-SYMBOL(<zsdt0051>).
      <zsdt0051>-objek = <zsdt0051>-nro_sol_ov.
    ENDLOOP.

    SELECT * FROM zsdt0045
        INTO TABLE @DATA(it_0045)
        FOR ALL ENTRIES IN @it_zsdt0051
        WHERE objek EQ @it_zsdt0051-objek
         AND  instrucao IN @p_instr
        AND safra      IN @p_safra
         AND  data_instr IN @p_dtins.

    LOOP AT it_zsdt0051 ASSIGNING FIELD-SYMBOL(<w0051>).
      IF line_exists( it_0045[ objek = <w0051>-objek ] ) .
      ELSE.
        <w0051>-nro_sol_ov = 'W'.
      ENDIF.
    ENDLOOP.
    DELETE it_zsdt0051 WHERE nro_sol_ov EQ 'W'.
  ENDIF.

  IF NOT p_dtfor IS INITIAL.

    FREE: it_vbfa, it_j_1bnflin, it_j_1bnfdoc.

    SELECT *
      FROM zsdt0066
      INTO TABLE @DATA(it_0066)
      FOR ALL ENTRIES IN @it_zsdt0051
      WHERE nro_sol_ov EQ @it_zsdt0051-nro_sol_ov
        AND instrucao IN @instrucao.

    SELECT vbelv vbeln rfmng vbtyp_n vbtyp_v
       FROM vbfa
       APPENDING CORRESPONDING FIELDS OF TABLE it_vbfa
       FOR ALL ENTRIES IN it_0066
       WHERE vbelv   = it_0066-vbeln
         AND   vbtyp_n = 'M'
         AND   vbtyp_v = 'C'.

    LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<wa_vbfa>).
      <wa_vbfa>-refkey = <wa_vbfa>-vbeln .
    ENDLOOP.

    SELECT refkey docnum netwr menge
      FROM j_1bnflin
      APPENDING CORRESPONDING FIELDS OF TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_vbfa
      WHERE refkey = it_vbfa-refkey.

    SELECT docnum nfenum docdat brgew ntgew
      FROM j_1bnfdoc
      INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfdoc
      FOR ALL ENTRIES IN it_j_1bnflin
      WHERE docnum = it_j_1bnflin-docnum
        AND docdat IN p_dtfor.

    LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<doc>).
      LOOP AT it_j_1bnflin ASSIGNING FIELD-SYMBOL(<lin>) WHERE docnum EQ <doc>-docnum.
        LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<fa>) WHERE refkey EQ <lin>-refkey.
          LOOP AT it_0066 ASSIGNING FIELD-SYMBOL(<sd66>) WHERE vbeln EQ <fa>-vbelv.
            LOOP AT it_zsdt0051 ASSIGNING FIELD-SYMBOL(<sd51>) WHERE nro_sol_ov EQ <sd66>-nro_sol_ov.
              r_sol_ov-sign    = 'I'.
              r_sol_ov-option  = 'EQ'.
              r_sol_ov-low     = <sd51>-nro_sol_ov.
              r_sol_ov-high    = <sd51>-nro_sol_ov.
              APPEND r_sol_ov.
              CLEAR r_sol_ov.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DELETE it_zsdt0051 WHERE NOT nro_sol_ov IN r_sol_ov.

  ENDIF.


  FREE: it_vbfa[], it_j_1bnflin[], it_j_1bnfdoc[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados_nf .

  obj_dados->clear( ).

  SELECT doc~branch
        lin~refkey
        lin~docnum
        lin~netwr
        lin~menge
        doc~nfenum
        doc~docdat
        doc~brgew
        doc~ntgew
    FROM j_1bnflin AS lin
    INNER JOIN j_1bnfdoc AS doc ON doc~docnum EQ lin~docnum
    INTO  TABLE it_doc_lin
    WHERE doc~docdat IN p_dtfor
    AND lin~matkl    EQ '700140'
    AND ( lin~cfop     EQ '6504AA'
     OR   lin~cfop     EQ '6505AA'    "*-CS2022000332-#78919-16.06.2022-JT-inicio
     OR   lin~cfop     EQ '5501AA'    "*-CS2022000332-#78919-16.06.2022-JT-inicio
     OR   lin~cfop     EQ '5501AB' )  "*-CS2022000332-#78919-16.06.2022-JT-inicio
    AND doc~bukrs    IN p_vkorg
    AND doc~branch   IN p_werks.

  CHECK it_doc_lin IS NOT INITIAL.

  LOOP AT it_doc_lin ASSIGNING FIELD-SYMBOL(<f_doc_lin>).
    <f_doc_lin>-vbeln = <f_doc_lin>-refkey.
  ENDLOOP.

  SELECT vbelv vbeln rfmng vbtyp_n vbtyp_v
    FROM vbfa
    INTO CORRESPONDING FIELDS OF TABLE it_vbfa
    FOR ALL ENTRIES IN it_doc_lin
    WHERE vbeln   = it_doc_lin-vbeln
    AND   vbtyp_n = 'M'
    AND   vbtyp_v = 'C'.

  IF it_vbfa IS NOT INITIAL.

    obj_dados->modify_vbfa( ).

    SELECT *
      FROM zsdt0066
      INTO TABLE it_zsdt0066
      FOR ALL ENTRIES IN it_vbfa
      WHERE vbeln EQ it_vbfa-vbelv.

    LOOP AT it_zsdt0066 ASSIGNING FIELD-SYMBOL(<f_zsdt0066>).
      <f_zsdt0066>-lifnr = |{ <f_zsdt0066>-werks ALPHA = IN }|.
      <f_zsdt0066>-objek = <f_zsdt0066>-nro_sol_ov.
    ENDLOOP.

  ENDIF.

  IF it_zsdt0066 IS NOT INITIAL.

    obj_dados->get_descricao_geral( ).
    obj_dados->dados_transporte( ).

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
        FOR ALL ENTRIES IN it_zsdt0066
        WHERE objek EQ it_zsdt0066-objek
         AND  werks EQ it_zsdt0066-werks
         AND safra      IN p_safra
         AND  instrucao IN p_instr.

    IF it_zsdt0045 IS NOT INITIAL.

      obj_dados->agrupa_0045( ).

      SELECT *
        FROM zsdt0051
        INTO TABLE it_zsdt0051
         FOR ALL ENTRIES IN it_zsdt0045
            WHERE nro_sol_ov EQ it_zsdt0045-nro_sol_ov
            AND vkorg IN p_vkorg
            AND spart IN p_spart
            AND vkbur IN p_vkbur
            AND kunnr IN p_kunnr
            AND bstkd IN p_bstkd
            AND data_venda IN p_data
            AND param_espec IN ( 'A', 'X', 'Z' ) " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
            AND status EQ 'L'.

    ENDIF.

    obj_dados->get_zsdt0008( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida_nf .

  APPEND LINES OF it_zmmt0008[] TO it_zmmt0008_bl[].
  APPEND LINES OF it_zmmt0008_bl[] TO it_zmmt0008_bl_2[].

  SORT: it_zmmt0008_bl     BY werks vbeln_vf lgort,
        it_lfa1            BY lifnr.

  DELETE ADJACENT DUPLICATES FROM it_zmmt0008_bl COMPARING werks vbeln_vf lgort.

  LOOP AT it_doc_lin INTO DATA(wa_doc_lin).

    TRY .
        wa_vbfa = it_vbfa[ vbeln = wa_doc_lin-vbeln ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_vbfa.
        CONTINUE.
    ENDTRY.

    TRY .
        wa_zsdt0066 = it_zsdt0066[ vbeln = wa_vbfa-vbelv ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_zsdt0066.
        CONTINUE.
    ENDTRY.

    TRY.
        wa_zsdt0045 = it_zsdt0045[
                                   objek = wa_zsdt0066-objek
                                   werks = wa_zsdt0066-werks
                                 ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_zsdt0045.
        CONTINUE.
    ENDTRY.

    TRY.
        wa_zsdt0051 = it_zsdt0051[ nro_sol_ov = wa_zsdt0045-nro_sol_ov ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_zsdt0051.
        CONTINUE.
    ENDTRY.

    TRY .
        wa_vbfa_j = it_vbfa_j[ vbeln = wa_vbfa-vbeln ].
        wa_vbfa_66-vbeln = wa_vbfa_j-vbelv.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_vbfa_j.
    ENDTRY.

    TRY .
        wa_vfkp = it_vfkp[ fknum = it_vtfa[ vbelv = it_vbfa_8[ vbelv = wa_vbfa_j-vbelv ]-vbeln ]-vbeln ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_vfkp.
    ENDTRY.



    cnpj_for = obj_dados->get_lfa1( wa_zsdt0066-lifnr ).

    CONCATENATE cnpj_for(2) '.' cnpj_for+2(3) '.' cnpj_for+5(3) '/' cnpj_for+8(4) '-' cnpj_for+12(3) INTO cnpj_formatado.

    APPEND VALUE #(
                      nro_sol_ov    = wa_zsdt0051-nro_sol_ov
                      werks         = wa_zsdt0066-werks
                      name1         = obj_dados->get_t001w( wa_zsdt0066-werks )
                      stcd1         = cnpj_formatado "OBJ_DADOS->GET_LFA1( WA_ZSDT0066-LIFNR )
                      data          = wa_doc_lin-docdat
                      ano           = wa_doc_lin-docdat+0(4)
                      mes           = obj_dados->get_mes( wa_doc_lin-docdat )
                      kunnr         = wa_zsdt0051-kunnr
                      name1_kun     = obj_dados->get_kna1( wa_zsdt0051-kunnr )-name1
                      contrato      = wa_zsdt0045-contrato
                      instrucao     = wa_zsdt0045-instrucao
                      qtd_ctners     = wa_zsdt0045-qtd_ctners
                      maktx         = obj_dados->get_makt( wa_zsdt0066-matnr )
                      matnr_e       = |{ wa_zsdt0066-matnr ALPHA = OUT }|
                      volum         = wa_zsdt0066-volum
                      zmeng_lote    = wa_zsdt0066-zmeng
                      nfnum_lote    = wa_doc_lin-nfenum
                      data_lote     = wa_doc_lin-docdat
                      vbeln_lote    = wa_zsdt0066-vbeln
                      vlrtot_lote   = wa_zsdt0066-vlrtot
                      netwr         = wa_doc_lin-netwr
                      libra_to      = wa_zsdt0066-libra_to
                      usd_to        = wa_zsdt0066-usd_to
                      kzwi1_totl    = wa_vfkp-kzwi1
                      kzwi1         = ( wa_vfkp-kzwi1 / wa_vbfa-rfmng ) * 1000
                      kzwi1_tot     = wa_vfkp-kzwi1 + wa_vfkp-kzwi2
                      kzwi1_val     = ( wa_vfkp-kzwi1 / wa_vbfa-rfmng ) * 1000
                      transp        = obj_dados->get_transp( wa_vbfa_j-vbelv )
                      placa_cav     = obj_dados->get_placa_cav( input = wa_zsdt0066-vbeln input1 = wa_vbfa_66-vbeln )
                      lentrega      = wa_zsdt0066-lentrega
                      mcod3         = obj_dados->get_kna1( wa_zsdt0066-lentrega )-mcod3
                      name1_arm     = obj_dados->get_kna1( wa_zsdt0066-lentrega )-name1
                      vbeln         = wa_vbfa-vbeln
                      werks66       = wa_zsdt0066-werks
                      fds_inst      = wa_zsdt0045-quantidade
                      peso_nf_bruto = wa_doc_lin-brgew "138693 - Adicionar Peso nf bruto
                      peso_nf_frm   = wa_doc_lin-ntgew
                      lgort2        = wa_zsdt0066-lgort
                      contador      = obj_dados->get_contador( input = wa_zsdt0066-werks input1 = wa_vbfa-vbeln )
                      lgort         = obj_dados->at_lgort
                      saldo_fds     = REDUCE i( INIT x = 0 FOR ls IN it_zmmt0008_bl_2 WHERE ( werks EQ wa_zsdt0066-werks AND vbeln_vf EQ wa_vbfa-vbeln ) NEXT x = x + 1 )
                      peso_origem   = obj_dados->at_peso_origem
                      t_peso_origem = obj_dados->at_peso_t_origem
                      tipo          = obj_dados->at_tipo
                  ) TO it_saida.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DADOS_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dados_transporte .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGRUPA_0045
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM agrupa_0045 .

  LOOP AT it_zsdt0045 ASSIGNING FIELD-SYMBOL(<zsdt0045>).
    <zsdt0045>-nro_sol_ov = <zsdt0045>-objek.
  ENDLOOP.

  it_0045 = it_zsdt0045.

  SORT it_0045 BY objek bukrs werks instrucao matnr.
  DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING  objek bukrs werks instrucao matnr.

  LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
    <f0045>-quantidade = 0.
    LOOP AT it_zsdt0045 INTO DATA(wa_0045) WHERE objek EQ <f0045>-objek AND
                                                 bukrs EQ <f0045>-bukrs AND
                                                 werks EQ <f0045>-werks AND
                                                 instrucao EQ <f0045>-instrucao AND
                                                 matnr EQ <f0045>-matnr.
      ADD wa_0045-quantidade TO <f0045>-quantidade.
    ENDLOOP.
  ENDLOOP.

  it_zsdt0045 = it_0045.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODELO1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modelo1 .

  DATA i TYPE i.
  IF r_tree = 'X'.
    wa_afield-tabname    = '1'.
    wa_afield-ref_table  = 'IT_SAIDA'.
    wa_afield-colddictxt = 'M'.
    wa_afield-selddictxt = 'M'.
    wa_afield-tipddictxt = 'M'.

  ELSE.
    wa_afield-tabname    = 'IT_SAIDA'.
    wa_afield-ref_table  = ''.
    wa_afield-colddictxt = 'M'.
    wa_afield-selddictxt = 'M'.
    wa_afield-tipddictxt = 'M'.
    wa_afield-col_opt = 'X'.
  ENDIF.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WERKS'.
  wa_afield-scrtext_s     = 'Cod.Faz'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  IF r_tree = 'X'.
    wa_afield-no_out        = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_s     = 'Fazenda'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  IF r_tree = 'X'.
    wa_afield-no_out        = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'STCD1'.
  wa_afield-scrtext_s     = 'CNPJ'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KUNNR'.
  wa_afield-scrtext_s     = 'Cod.Cliente'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  IF r_tree = 'X'.
    wa_afield-no_out        = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1_KUN'.
  wa_afield-scrtext_s     = 'Cliente'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  IF r_tree = 'X'.
    wa_afield-no_out        = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CONTRATO'.
  wa_afield-scrtext_s     = 'Contrato'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  IF r_tree = 'X'.
    wa_afield-no_out        = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'INSTRUCAO'.
  wa_afield-scrtext_s     = 'Instrução'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  IF r_tree = 'X'.
    wa_afield-no_out        = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

***  Marcos Faneli --> Ch.128554
  ADD 1 TO i.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'FDS_INST'.
  wa_afield-scrtext_m   = 'FDS Instrução'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-do_sum      = 'X'.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'MATNR_E'.
  wa_afield-scrtext_m   = 'Material'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'MAKTX'.
  wa_afield-scrtext_m   = 'Descr. Material'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'ANO'.
  wa_afield-scrtext_s   = 'Ano'.
  wa_afield-scrtext_l   = wa_afield-scrtext_s.
  wa_afield-scrtext_m   = wa_afield-scrtext_s.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  IF r_tree = 'X'.
    wa_afield-no_out    = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'MES'.
  wa_afield-scrtext_s   = 'Mês'.
  wa_afield-scrtext_l   = wa_afield-scrtext_s.
  wa_afield-scrtext_m   = wa_afield-scrtext_s.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  IF r_tree = 'X'.
    wa_afield-no_out    = 'X'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'LGORT'.
  wa_afield-scrtext_s   = 'Nº BLOCO'.
  wa_afield-scrtext_l   = wa_afield-scrtext_s.
  wa_afield-scrtext_m   = wa_afield-scrtext_s.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-hotspot     = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'VOLUM'.
  wa_afield-scrtext_m   = 'FDS FRM LOTE'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  IF r_tree = 'X'.
    wa_afield-datatype  = 'CURR'.
    wa_afield-inttype	  = 'P'.
    wa_afield-intlen    = 17.
    wa_afield-domname   = 'S_SUM'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CONTADOR'.
  wa_afield-scrtext_s = 'FDS REMESSA'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TIPO'.
  wa_afield-scrtext_s = 'TIPO'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = ''.
  APPEND wa_afield TO it_fieldcat.

***  Marcos Faneli --> Ch.128554
  ADD 1 TO i.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'SALDO_FDS'.
  wa_afield-scrtext_m   = 'Total FDS Remessa'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-do_sum      = 'X'.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  ADD 1 TO i.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'PESO_ORIGEM'.
  wa_afield-scrtext_m   = 'Peso Origem'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-do_sum      = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  ADD 1 TO i.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'T_PESO_ORIGEM'.
  wa_afield-scrtext_m   = 'Total Peso Origem'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-do_sum      = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ZMENG_LOTE'.
  wa_afield-scrtext_m   = 'PESO FRM LOTE'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-do_sum        = 'X'.
  IF r_tree = 'X'.
    wa_afield-datatype      = 'CURR'.
    wa_afield-inttype	      = 'P'.
    wa_afield-intlen        = 17.
    wa_afield-domname       = 'S_SUM'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DATA_LOTE'.
  wa_afield-scrtext_m   = 'DATA NF FRM LOTE'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'VBELN_LOTE'.
  wa_afield-scrtext_m   = 'ORDEM FRM L.'.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'LGORT2'.
  wa_afield-scrtext_m   = 'Depóstio FRM'.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'VLRTOT_LOTE'.
  wa_afield-scrtext_m   = 'VLR TOTAL FRM L.'.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'NETWR'.
  wa_afield-scrtext_m   = 'VLR. NF FRM'.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'LIBRA_TO'.
  wa_afield-scrtext_m   = 'C.USD LP'.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'USD_TO'.
  wa_afield-scrtext_m   = 'Vlr. USD TO'.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'NFNUM_LOTE'.
  wa_afield-scrtext_m   = 'NF FRM LOTE'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

***  Marcos Faneli --> Ch.128554
  ADD 1 TO i.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'PESO_NF_FRM'.
  wa_afield-scrtext_m   = 'Peso NF FRM'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-do_sum      = 'X'.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.

  ADD 1 TO i.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'PESO_NF_BRUTO'.
  wa_afield-scrtext_m   = 'Peso NF Bruto'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-do_sum      = 'X'.
  wa_afield-key         = ''.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'KZWI1_VAL'.
  wa_afield-scrtext_m   = 'VALOR FRETE BRUTO'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  IF r_tree = 'X'.
    wa_afield-datatype  = 'CURR'.
    wa_afield-inttype	  = 'P'.
    wa_afield-intlen    = 17.
    wa_afield-domname   = 'S_SUM'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'KZWI1'.
  wa_afield-scrtext_m   = 'VALOR FRETE LIQUIDO'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  IF r_tree = 'X'.
    wa_afield-datatype  = 'CURR'.
    wa_afield-inttype	  = 'P'.
    wa_afield-intlen    = 17.
    wa_afield-domname   = 'S_SUM'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'KZWI1_TOT'.
  wa_afield-scrtext_m   = 'TOTAL FRETE BRUTO'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  IF r_tree = 'X'.
    wa_afield-datatype  = 'CURR'.
    wa_afield-inttype	  = 'P'.
    wa_afield-intlen    = 17.
    wa_afield-domname   = 'S_SUM'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_pos     = i.
  wa_afield-fieldname   = 'KZWI1_TOTL'.
  wa_afield-scrtext_m   = 'TOTAL FRETE LIQUIDO'.
  wa_afield-scrtext_l   = wa_afield-scrtext_m.
  wa_afield-scrtext_s   = wa_afield-scrtext_m.
  wa_afield-edit        = ''.
  wa_afield-key         = ''.
  wa_afield-do_sum      = 'X'.
  IF r_tree = 'X'.
    wa_afield-datatype  = 'CURR'.
    wa_afield-inttype	  = 'P'.
    wa_afield-intlen    = 17.
    wa_afield-domname   = 'S_SUM'.
  ENDIF.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'PLACA_CAV'.
  wa_afield-scrtext_m = 'PLACA TRANSPORTE'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TRANSP'.
  wa_afield-scrtext_m = 'TRANSPORTADORA'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LENTREGA'.
  wa_afield-scrtext_s = 'ARMAZEM'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1_ARM'.
  wa_afield-scrtext_s = 'DESCRIÇÃO ARMAZEM'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MCOD3'.
  wa_afield-scrtext_m = 'CIDADE DESTINO'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

ENDFORM.


********************* FORMS TRATA_FARDO_A_FARDO ************************
* Sara Oikawa - 05/2020
*&---------------------------------------------------------------------*
*& Form f_fardo_objects_create
*&---------------------------------------------------------------------*
* For creating Custom Containers
*----------------------------------------------------------------------*
FORM f_fardo_objects_create.

  DATA: lv_repid TYPE sy-repid.

  lv_repid = sy-repid.

  CREATE OBJECT custom_container1
    EXPORTING
      container_name              = container1
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  CREATE OBJECT custom_container2
    EXPORTING
      container_name              = container2
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc NE 0.
* add your handling, for example
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = lv_repid
        txt2  = sy-subrc
        txt1  = 'The control could not be created'.   "(510).
  ENDIF.
  CREATE OBJECT o_alvgrid1
    EXPORTING
      i_parent = custom_container1.
  CREATE OBJECT o_alvgrid2
    EXPORTING
      i_parent = custom_container2.
ENDFORM. " f_fardo_objects_create

*&---------------------------------------------------------------------*
*& Form f_fardo_layout
*&---------------------------------------------------------------------*
* To define the layout
*----------------------------------------------------------------------*
FORM f_fardo_layout USING VALUE(ptitle)
                    VALUE(pzebra)
                    VALUE(pmode)
                    VALUE(pwidth).

  w_layout-grid_title = ptitle.
  w_layout-zebra      = pzebra.
  w_layout-sel_mode   = pmode.
  w_layout-cwidth_opt = pwidth.
  w_variant-report    = sy-repid.
ENDFORM. " f_fardo_layout

*&---------------------------------------------------------------------*
*& Form f9006_error_handle
*&---------------------------------------------------------------------*
* To handle event
*----------------------------------------------------------------------*
* -->P_PTEXT text
*----------------------------------------------------------------------*
FORM f9006_error_handle USING VALUE(ptext).

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = TEXT-e03 " Error Note
        txt2  = sy-subrc
        txt1  = ptext.
  ENDIF.
ENDFORM. " f9006_error_handle

*---------------------------------------------------------------------*
* FORM F_FARDO_EXIT_PROGRAM *
*---------------------------------------------------------------------*
FORM f_fardo_exit_program.

  CALL METHOD custom_container1->free.
  CALL METHOD custom_container2->free.
  CALL METHOD cl_gui_cfw=>flush.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = sy-repid
        txt2  = sy-subrc
        txt1  = 'Error in FLush'(500).
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_FARDO_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM f_fardo_fieldcatalog  TABLES p_i_fieldcat STRUCTURE lvc_s_fcat
                           USING  VALUE(p_tab).

  IF p_tab EQ '1'.

    CASE abap_true.

*---------------------------
*---- foemacao lote
*---------------------------
      WHEN r_flote.
        IF r_saldo IS INITIAL.
          PERFORM f_fill_fieldcatalog TABLES p_i_fieldcat
                                      USING:
          'KUNNR'                ''                      '' '' '' '' '' '' 'X' ''   ,
          'NAME1'                'Cliente'               '' '' '40' '' '' '' '' ''  ,
          'INST_FML'             'Instrução Form.Lote'   '' '' '' '' '' '' '' ''    ,
          'INST_EXP'             'Instrução Exportação'  '' '' '' '' '' '' '' ''    ,
          'LOTE'                 'Lote'                  '' '' '10' '' '' '' '' ''  ,
          'SAFRA'                'Safra'                 '' '' '08' '' '' '' '' ''  ,
          'QT_FDS_FML'           'Fardos Form. Lote'     '' '' '' '' '' 'X' '' ''   ,
          'QT_FDS_EXP'           'Fardos Exportação'     '' '' '' '' '' 'X' '' ''   ,
          'QT_FDS_SLD'           'Fardos a Exportar'     '' '' '' '' '' 'X' '' ''   .
        ELSE.
          PERFORM f_fill_fieldcatalog TABLES p_i_fieldcat
                                      USING:
          'KUNNR'                ''                      '' '' '' '' '' '' 'X' ''  ,
          'NAME1'                'Cliente'               '' '' '' '' '' '' ''  ''  ,
          'INST_FML'             'Instrução Form.Lote'   '' '' '' '' '' '' ''  ''  ,
          'INST_EXP'             'Instrução Exportação'  '' '' '' '' '' '' ''  ''  ,
          'LOTE'                 'Lote'                  '' '' '' '' '' '' ''  ''  ,
          'SAFRA'                'Safra'                 '' '' '' '' '' '' ''  ''  ,
          'QT_FDS_FML'           'Fardos Form. Lote'     '' '' '' '' '' 'X' '' ''  ,
          'QT_FDS_EXP'           'Fardos Exportação'     '' '' '' '' '' 'X' '' ''  ,
          'QT_FDS_SLD'           'Fardos a Exportar'     '' '' '' '' '' 'X' '' ''  ,
          'PESO_FML'             'Peso Form. Lote'       '' '' '' '' '' '' 'X' ''  ,
          'PESO_EXP'             'Peso Exportação'       '' '' '' '' '' '' 'X' ''  ,
          'PESO_SLD'             'Peso a Exportar'       '' '' '' '' '' '' 'X' ''  .
        ENDIF.

*---------------------------
*---- exportacao
*---------------------------
      WHEN r_venex.
        IF r_saldo IS INITIAL.
          PERFORM f_fill_fieldcatalog TABLES p_i_fieldcat
                                      USING:
          'KUNNR'                ''                      '' '' '' '' '' '' 'X' ''   ,
          'NAME1'                'Cliente'               '' '' '40' '' '' '' '' ''  ,
          'INST_FML'             'Instrução'             '' '' '' '' '' '' '' ''    ,
*         'QUANTIDADE'           'Fardos Instruídos'     '' '' '' '' '' '' '' ''    ,
*         'INST_EXP'             'Instrução Exportação'  '' '' '' '' '' '' '' ''    ,
          'LOTE'                 'Lote'                  '' '' '10' '' '' '' '' ''  ,
          'SAFRA'                'Safra'                 '' '' '08' '' '' '' '' ''  ,
          'QUANTIDADE'           'Fardos Instruídos'     '' '' '' '' '' '' '' ''    ,
          'VOLUM'                'Fardos Vendidos'       '' '' '' '' '' 'X' '' ''   ,
*         'QT_FDS_FML'           'Fardos Form. Lote'     '' '' '' '' '' 'X' '' ''   ,
*         'QT_FDS_EXP'           'Fardos Exportação'     '' '' '' '' '' 'X' '' ''   ,
          'QT_FDS_SLD'           'Saldo Fardos'          '' '' '' '' '' 'X' '' ''   .
*         'PESO_FML'             'Peso Form. Lote'       '' '' '' '' '' 'X' '' ''   ,
*         'PESO_EXP'             'Peso Exportação'       '' '' '' '' '' 'X' '' ''   ,
*         'PESO_SLD'             'Peso a Exportar'       '' '' '' '' '' 'X' '' ''   .
        ELSE.
          PERFORM f_fill_fieldcatalog TABLES p_i_fieldcat
                                      USING:
          'KUNNR'                ''                      '' '' '' '' '' '' 'X' ''   ,
          'NAME1'                'Cliente'               '' '' '40' '' '' '' '' ''  ,
          'INST_FML'             'Instrução'             '' '' '' '' '' '' '' ''    ,
*         'QUANTIDADE'           'Fardos Instruídos'     '' '' '' '' '' '' '' ''    ,
*         'INST_EXP'             'Instrução Exportação'  '' '' '' '' '' '' '' ''    ,
          'LOTE'                 'Lote'                  '' '' '10' '' '' '' '' ''  ,
          'SAFRA'                'Safra'                 '' '' '08' '' '' '' '' ''  ,
          'QUANTIDADE'           'Fardos Instruídos'     '' '' '' '' '' '' '' ''    ,
          'VOLUM'                'Fardos Vendidos'       '' '' '' '' '' 'X' '' ''   ,
*         'QT_FDS_FML'           'Fardos Form. Lote'     '' '' '' '' '' 'X' '' ''   ,
*         'QT_FDS_EXP'           'Fardos Exportação'     '' '' '' '' '' 'X' '' ''   ,
          'QT_FDS_SLD'           'Saldo Fardos'          '' '' '' '' '' 'X' '' ''   .
*         'PESO_FML'             'Peso Form. Lote'       '' '' '' '' '' 'X' '' ''   ,
*         'PESO_EXP'             'Peso Exportação'       '' '' '' '' '' 'X' '' ''   ,
*         'PESO_SLD'             'Peso a Exportar'       '' '' '' '' '' 'X' '' ''   .
        ENDIF.
    ENDCASE.

  ELSE.
    CASE abap_true.
*---------------------------
*---- foemacao lote
*---------------------------
      WHEN r_flote.
        PERFORM f_fill_fieldcatalog TABLES p_i_fieldcat
                                    USING:
        'KUNNR'                ''                      '' '' '' '' '' '' 'X' ''   ,
        'NAME1'                'Cliente'               '' '' '' '' '' '' ''  ''   ,
        'INST_FML'             'Instrução Form.Lote'   '' '' '' '' '' '' ''  ''   ,
        'INST_EXP'             'Instrução Exportação'  '' '' '' '' '' '' ''  ''   ,
        'LOTE'                 'Lote'                  '' '' '' '' '' '' 'X' ''   ,
        'SAFRA'                'Safra'                 '' '' '' '' '' '' ''  ''   ,
        'PESO_FML'             'Peso Form. Lote'       '' '' '' '' '' 'X' '' ''   ,
        'PESO_EXP'             'Peso Exportação'       '' '' '' '' '' 'X' '' ''   ,
        'PESO_SLD'             'Peso a Exportar'       '' '' '' '' '' 'X' '' ''   .

*---------------------------
*---- exportacao
*---------------------------
      WHEN r_venex.
        PERFORM f_fill_fieldcatalog TABLES p_i_fieldcat
                                    USING:
        'KUNNR'                ''                      '' '' '' '' '' '' 'X' ''   ,
        'NAME1'                'Cliente'               '' '' '' '' '' '' ''  ''   ,
        'INST_FML'             'Instrução'             '' '' '' '' '' '' ''  ''   ,
*       'INST_EXP'             'Instrução Exportação'  '' '' '' '' '' '' ''  ''   ,
        'LOTE'                 'Lote'                  '' '' '' '' '' '' 'X' ''   ,
        'SAFRA'                'Safra'                 '' '' '' '' '' '' ''  ''   ,
        'BTGEW'                'Peso Instruído'        '' '' '' '' '' '' ''  ''   ,
*       'PESO_FML'             'Peso Form. Lote'       '' '' '' '' '' 'X' '' ''   ,
        'PESO_EXP'             'Peso Venda'            '' '' '' '' '' 'X' '' ''   ,
*       'PESO_SLD'             'Peso a Exportar'       '' '' '' '' '' 'X' '' ''   ,
        'PESO_SLD'             'Saldo de Peso'         '' '' '' '' '' 'X' '' ''   .
    ENDCASE.

  ENDIF.

ENDFORM.                    " F_FARDO_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  F_FILL_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM f_fill_fieldcatalog    TABLES p_i_fieldcat STRUCTURE lvc_s_fcat
                            USING
                            VALUE(p_fieldname)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_no_zero)
                            VALUE(p_hotspot)
                            VALUE(p_cor)
                            VALUE(p_just)
                            VALUE(p_sum)
                            VALUE(p_no_out)
                            VALUE(p_col_opt).
*                           VALUE(P_CHECKBOX).

  DATA: w_fieldcat TYPE lvc_s_fcat.

  w_fieldcat-fieldname = p_fieldname.
  w_fieldcat-scrtext_l = p_desc.
  w_fieldcat-scrtext_m = p_desc.
  w_fieldcat-coltext   = p_desc.
  w_fieldcat-outputlen = p_tam.
  w_fieldcat-no_zero   = p_no_zero.
  w_fieldcat-hotspot   = p_hotspot.
  w_fieldcat-emphasize = p_cor.
  w_fieldcat-just      = p_just.
  w_fieldcat-do_sum    = p_sum.
  w_fieldcat-no_out    = p_no_out.
  w_fieldcat-col_opt   = p_col_opt.
*  W_FIELDCAT-CHECKBOX  = P_CHECKBOX.

*  W_FIELDCATALOG-EDIT = COND #( WHEN BLOCK IS INITIAL THEN P_CHECKBOX ELSE ABAP_FALSE ).

  APPEND w_fieldcat  TO p_i_fieldcat.

  CLEAR: w_fieldcat.

ENDFORM.                    " F_FILL_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FIELDCAT  text
*----------------------------------------------------------------------*
*FORM F_MODIFY_FIELD_CAT.   "  TABLES   P_I_FIELDCAT STRUCTURE LVC_T_FCAT.
*
*  FIELD-SYMBOLS <FS_FIELDCAT> TYPE LVC_S_FCAT.
*
*  LOOP AT I_FIELDCAT  ASSIGNING <FS_FIELDCAT>.

*-  Atualizar textos
*    CASE <FS_FIELDCAT>-FIELDNAME.
*
*      WHEN 'KUNNR'.
*        <FS_FIELDCAT>-NO_OUT = ABAP_TRUE.
*
*      WHEN 'NAME1'.
*        <FS_FIELDCAT>-SCRTEXT_L = <FS_FIELDCAT>-SCRTEXT_M =
*        <FS_FIELDCAT>-SCRTEXT_S =
*        <FS_FIELDCAT>-COLTEXT = 'Cliente'.
*
*      WHEN OTHERS.
*
*    ENDCASE.
*
*  ENDLOOP.


*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_SORT
*&---------------------------------------------------------------------*
FORM f_fardo_sort TABLES p_i_sort STRUCTURE lvc_s_sort.

  DATA: w_sort TYPE lvc_s_sort.

  w_sort-fieldname = 'NAME1'.
  w_sort-up        = abap_true.
  APPEND w_sort  TO p_i_sort.

  w_sort-fieldname = 'INST_FML'.
  w_sort-subtot    = abap_true.
  APPEND w_sort  TO p_i_sort.

*  W_SORT-FIELDNAME = 'INST_EXP'.
*  W_SORT-GROUP     = ABAP_TRUE.
*  CLEAR  W_SORT-SUBTOT.
*  APPEND W_SORT  TO P_I_SORT.

  w_sort-fieldname = 'LOTE'.
  w_sort-up        = abap_true.
  CLEAR: w_sort.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_TRATA_FARDO_A_FARDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_trata_fardo_a_fardo .

  PERFORM:  f_fardo_seleciona_dados,
            f_fardo_sumariza,
            f_fardo_atribui_peso_lote,
            f_fardo_exibe_saida.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_SELECIONA_DADOS.
*&---------------------------------------------------------------------*
FORM f_fardo_seleciona_dados.

  DATA: gr_werks TYPE RANGE OF zmmt0008-werks,
        ls_werks LIKE LINE  OF gr_werks.

*  Selecionar Dados do Relatório Fardo a Fardo.

  " Seleciona Tabela de Instruções + Solic. Ordem Venda
  SELECT  t45~zseq_inst
          t45~objek
          t45~objecttable
          t45~bukrs
          t45~instrucao
          t45~data_instr
          t51~kunnr
          t51~nro_sol_ov
          t45~safra
          t45~quantidade  "*-CS2022000332-#79606-17.06.2022-JT-inicio
          t45~btgew       "*-CS2022000332-#79606-17.06.2022-JT-inicio
          t45~charg       "*-CS2022000332-#79606-17.06.2022-JT-inicio
     INTO TABLE it_instr
     FROM zsdt0045 AS t45
     INNER JOIN zsdt0051  AS t51
       ON  t45~objek      EQ t51~nro_sol_ov
     WHERE t45~bukrs      IN p_vkorg4
       AND t45~instrucao  IN p_instr4
       AND t45~safra      IN p_safra4
       AND t45~data_instr IN p_dtins4
       AND t51~kunnr      IN p_kunnr4.

  SORT it_instr BY objek instrucao charg.
  DELETE ADJACENT DUPLICATES FROM it_instr COMPARING objek instrucao charg.

  DELETE it_instr WHERE ( kunnr IS INITIAL OR objek IS INITIAL ).

*-CS2022000332-#79606-17.06.2022-JT-inicio
  IF it_instr[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0051
      INTO TABLE it_zsdt0051
       FOR ALL ENTRIES IN it_instr
     WHERE nro_sol_ov  EQ it_instr-objek(10)
       AND param_espec IN ( 'A', 'X', 'Z' ) " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ
       AND status      EQ 'L'.

    LOOP AT it_zsdt0051 INTO wa_zsdt0051.
      DATA(l_tabix) = sy-tabix.
      READ TABLE it_instr  INTO wa_instr WITH KEY objek(10) = wa_zsdt0051-nro_sol_ov.
      IF sy-subrc = 0.
        wa_zsdt0051-instrucao = wa_instr-instrucao.
        MODIFY it_zsdt0051 FROM wa_zsdt0051 INDEX l_tabix.
      ENDIF.
    ENDLOOP.

    IF     r_flote = abap_true.
      DELETE it_zsdt0051 WHERE param_espec <> 'A'
                           AND param_espec <> 'Z'. " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ

    ELSEIF r_venex = abap_true.
      DELETE it_zsdt0051 WHERE param_espec <> 'X'.
    ENDIF.

    it_0051_a[] = it_zsdt0051[].
    DELETE it_0051_a WHERE param_espec <> 'A'
                       AND param_espec <> 'Z'. " IR195471 - inclusão do param Z - VENDA FRAME ALGODAO- PQ

    it_0051_x[] = it_zsdt0051[].
    DELETE it_0051_x WHERE param_espec <> 'X'.
  ENDIF.
*-CS2022000332-#79606-17.06.2022-JT-fim

  "Seleciona Clientes das Instruções
  IF  it_instr IS NOT INITIAL.
    SELECT  kunnr mcod3 name1
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_instr
      WHERE kunnr = it_instr-kunnr.

    SORT it_kna1 BY kunnr.
  ENDIF.

*-CS2022000332-#79606-17.06.2022-JT-inicio
  " Seleciona Tabela de Solicitação de Ordem de Venda – Formação de Lote
  IF it_0051_a[] IS NOT INITIAL.
    SELECT nro_sol_ov
           posnr
           instrucao
           werks
           volum
           vbeln
           charg_ori
           zmeng
      FROM zsdt0066
      INTO TABLE it_fmlote
       FOR ALL ENTRIES IN it_0051_a
     WHERE nro_sol_ov EQ it_0051_a-nro_sol_ov
       AND instrucao  EQ it_0051_a-instrucao.

*    SELECT nro_sol_ov
*           posnr
*           instrucao
*           werks
*           volum
*           vbeln
*           charg_ori
*      FROM zsdt0066
*      INTO TABLE it_fmlote
*      FOR ALL ENTRIES IN it_instr
*      WHERE nro_sol_ov EQ it_instr-nro_sol_ov
*        AND instrucao  EQ it_instr-instrucao.
*-CS2022000332-#79606-17.06.2022-JT-fim
  ENDIF.
  DELETE it_fmlote WHERE vbeln IS INITIAL.

*-CS2022000332-#79606-17.06.2022-JT-inicio
  IF it_0051_a[] IS NOT INITIAL.
    SELECT t053~instrucao      "Seleciona Lotes Exportação
           t053~instrucao_ant
           t053~nro_sol_ov
           t053~posnr
           t053~vbeln
           t213~lgort
           t213~volum
           t213~status
           t053~werks
           t053~charg
      FROM zsdt0053 AS t053
     INNER JOIN zsdt0213 AS t213
        ON t053~nro_sol_ov EQ t213~nro_sol_ov
       AND t053~posnr      EQ t213~posnr
      INTO TABLE it_export
       FOR ALL ENTRIES IN it_0051_a
     WHERE t053~nro_sol_ov EQ it_0051_a-nro_sol_ov
       AND t053~instrucao  EQ it_0051_a-instrucao.

    DELETE it_export WHERE inst_exp IS NOT INITIAL.

    SELECT t053~instrucao_ant
           t053~instrucao
           t053~nro_sol_ov
           t053~posnr
           t053~vbeln
           t213~lgort
           t213~volum
           t213~status
           t053~werks
           t053~charg
        FROM zsdt0053 AS t053
       INNER JOIN zsdt0213 AS t213
          ON t053~nro_sol_ov EQ t213~nro_sol_ov
         AND t053~posnr      EQ t213~posnr
       APPENDING TABLE it_export
         FOR ALL ENTRIES IN it_0051_a
       WHERE t053~nro_sol_ov     EQ it_0051_a-nro_sol_ov
         AND t053~instrucao_ant  EQ it_0051_a-instrucao.
  ENDIF.

  IF it_0051_x[] IS NOT INITIAL.
    SELECT t053~instrucao      "Seleciona Lotes Exportação
           t053~instrucao_ant
           t053~nro_sol_ov
           t053~posnr
           t053~vbeln
           t213~lgort
           t213~volum
           t213~status
           t053~werks
           t053~charg
      FROM zsdt0053 AS t053
     INNER JOIN zsdt0213 AS t213
        ON t053~nro_sol_ov EQ t213~nro_sol_ov
       AND t053~posnr      EQ t213~posnr
      INTO TABLE it_export
       FOR ALL ENTRIES IN it_0051_x
     WHERE t053~nro_sol_ov EQ it_0051_x-nro_sol_ov
       AND t053~instrucao  EQ it_0051_x-instrucao.

    DELETE it_export WHERE inst_exp IS NOT INITIAL.

    SELECT t053~instrucao_ant
           t053~instrucao
           t053~nro_sol_ov
           t053~posnr
           t053~vbeln
           t213~lgort
           t213~volum
           t213~status
           t053~werks
           t053~charg
        FROM zsdt0053 AS t053
       INNER JOIN zsdt0213 AS t213
          ON t053~nro_sol_ov EQ t213~nro_sol_ov
         AND t053~posnr      EQ t213~posnr
       APPENDING TABLE it_export
         FOR ALL ENTRIES IN it_0051_x
       WHERE t053~nro_sol_ov     EQ it_0051_x-nro_sol_ov
         AND t053~instrucao_ant  EQ it_0051_x-instrucao.
  ENDIF.
*-CS2022000332-#79606-17.06.2022-JT-fim

  DELETE it_export WHERE vbeln  IS INITIAL.
  DELETE it_export WHERE status IS NOT INITIAL.
  SORT it_export BY inst_fml inst_exp nro_sol_ov posnr.

*  IF  it_instr IS NOT INITIAL.
*    SELECT t053~instrucao      "Seleciona Lotes Exportação
*           t053~instrucao_ant
*           t053~nro_sol_ov
*           t053~posnr
*           t053~vbeln
*           t213~lgort
*           t213~volum
*           t213~status
*        FROM zsdt0053 AS t053
*        INNER JOIN zsdt0213 AS t213
*          ON  t053~nro_sol_ov EQ t213~nro_sol_ov
*          AND t053~posnr      EQ t213~posnr
*        INTO TABLE it_export
*        FOR ALL ENTRIES IN it_instr
*        WHERE instrucao  EQ it_instr-instrucao.
*
*    DELETE it_export WHERE inst_exp IS NOT INITIAL.
*
*    SELECT t053~instrucao_ant
*           t053~instrucao
*           t053~nro_sol_ov
*           t053~posnr
*           t053~vbeln
*           t213~lgort
*           t213~volum
*           t213~status
*        FROM zsdt0053 AS t053
*        INNER JOIN zsdt0213 AS t213
*          ON  t053~nro_sol_ov EQ t213~nro_sol_ov
*          AND t053~posnr      EQ t213~posnr
*        APPENDING TABLE it_export
*        FOR ALL ENTRIES IN it_instr
*        WHERE instrucao_ant  EQ it_instr-instrucao.
*
*    DELETE it_export WHERE vbeln  IS INITIAL.
*    DELETE it_export WHERE status IS NOT INITIAL.
*    SORT it_export BY inst_fml inst_exp nro_sol_ov posnr.
*  ENDIF.
*-CS2022000332-#79606-17.06.2022-JT-fim

*-CS2022000332-#79606-17.06.2022-JT-inicio
* IF p_vkorg4-low  EQ '0001'.
  IF it_0051_a[] IS NOT INITIAL.
    DELETE it_fmlote WHERE charg_ori IS INITIAL.
  ENDIF.
*-CS2022000332-#79606-17.06.2022-JT-fim

  "Seleciona NF´s de Formação de Lote
  IF it_fmlote IS NOT INITIAL.
    SELECT vbelv vbeln
     FROM vbfa
     INTO TABLE it_vbfa_fard
        FOR ALL ENTRIES IN it_fmlote
          WHERE vbelv   = it_fmlote-vbeln
          AND   vbtyp_n = 'M'
          AND   vbtyp_v = 'C'.

    IF it_vbfa_fard IS NOT INITIAL.

      SELECT
          doc~docnum
          lin~itmnum
          lin~refkey
          lin~refitm
          lin~menge
          doc~nfenum
          doc~docdat
          doc~cancel
          act~docsta
          act~scssta
*          VBR~BRGEW
          vbr~ntgew
          vbr~aubel
          vbr~aupos

      FROM j_1bnflin AS lin
      INNER JOIN j_1bnfdoc AS doc      ON doc~docnum EQ lin~docnum
      INNER JOIN j_1bnfe_active AS act ON doc~docnum EQ act~docnum
      INNER JOIN vbrp AS vbr ON vbr~vbeln EQ lin~refkey
                            AND vbr~posnr EQ lin~refitm

      INTO  TABLE it_nflote
       FOR ALL ENTRIES IN it_vbfa_fard
     WHERE reftyp EQ 'BI'
      AND  refkey EQ it_vbfa_fard-vbeln.

      "Elimina Notas Canceladas/Rejeitadas
      DELETE it_nflote WHERE ( cancel IS NOT INITIAL OR docsta NE '1' OR scssta EQ '2' ).

    ENDIF.

  ENDIF.

  "Seleciona NF´s - Exportação
  REFRESH it_vbfa_fard.

  IF it_export[] IS NOT INITIAL.
    SELECT vbelv vbeln
     FROM vbfa
     INTO TABLE it_vbfa_fard
        FOR ALL ENTRIES IN it_export
          WHERE vbelv   = it_export-vbeln
          AND   vbtyp_n = 'M'
          AND   vbtyp_v = 'C'.

    IF it_vbfa_fard IS NOT INITIAL.

      SELECT doc~docnum
             lin~itmnum
             lin~refkey
             lin~refitm
             lin~menge
             doc~nfenum
             doc~docdat
             doc~cancel
             act~docsta
             act~scssta
*             VBR~BRGEW
             vbr~ntgew
             vbr~aubel
             vbr~aupos
      FROM       j_1bnflin AS lin
      INNER JOIN j_1bnfdoc AS doc ON doc~docnum EQ lin~docnum
      INNER JOIN j_1bnfe_active AS act ON doc~docnum EQ act~docnum
      INNER JOIN vbrp AS vbr ON vbr~vbeln EQ lin~refkey
                            AND vbr~posnr EQ lin~refitm

      INTO  TABLE it_nflote_exp
       FOR  ALL ENTRIES IN it_vbfa_fard
      WHERE reftyp EQ 'BI'
       AND  refkey EQ it_vbfa_fard-vbeln.

      "Elimina Notas Canceladas/Rejeitadas
      DELETE it_nflote_exp WHERE ( cancel IS NOT INITIAL OR docsta NE '1' OR scssta EQ '2' ).

    ENDIF.

  ENDIF.

  " Seleciona ZMMT0008
* IF p_vkorg4-low  EQ '0015'.
* IF it_0051_a[] IS NOT INITIAL.  "*-CS2022000332-#79606-17.06.2022-JT-fim
  IF it_fmlote[] IS NOT INITIAL.  "*-CS2022000332-#79606-17.06.2022-JT-fim
    "Monta Range de Centros para acesso ZMMT0008
    LOOP AT it_fmlote INTO wa_fmlote.
      ls_werks-sign   = 'I'.
      ls_werks-option = 'EQ'.
      ls_werks-low    = wa_fmlote-werks.
      APPEND ls_werks TO gr_werks.
    ENDLOOP.
    SORT gr_werks BY low.
    DELETE ADJACENT DUPLICATES FROM gr_werks COMPARING low.

    IF it_nflote IS NOT INITIAL.
      SELECT *
        FROM zmmt0008
        INTO TABLE it_zmmt0008_a
         FOR ALL ENTRIES IN it_nflote
       WHERE werks       IN gr_werks
         AND vbeln_vf    EQ it_nflote-refkey.
    ENDIF.
  ENDIF.

*-CS2022000332-#79606-17.06.2022-JT-inicio
* IF it_0051_x[] IS NOT INITIAL.  "*-CS2022000332-#79606-17.06.2022-JT-fim
  IF it_export[] IS NOT INITIAL.  "*-CS2022000332-#79606-17.06.2022-JT-fim
    "Monta Range de Centros para acesso ZMMT0008
    FREE: gr_werks.
    LOOP AT it_export INTO wa_export.
      ls_werks-sign   = 'I'.
      ls_werks-option = 'EQ'.
      ls_werks-low    = wa_export-werks.
      APPEND ls_werks TO gr_werks.
    ENDLOOP.
    SORT gr_werks BY low.
    DELETE ADJACENT DUPLICATES FROM gr_werks COMPARING low.

    IF it_nflote_exp[] IS NOT INITIAL.
      SELECT *
        FROM zmmt0008
        INTO TABLE it_zmmt0008_x
         FOR ALL ENTRIES IN it_nflote_exp
       WHERE werks       IN gr_werks
         AND vbeln_vf    EQ it_nflote_exp-refkey.
    ENDIF.
  ENDIF.
*-CS2022000332-#79606-17.06.2022-JT-fim

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_SUMARIZA
*&---------------------------------------------------------------------*
FORM f_fardo_sumariza_param_a.

  DATA: vl_calc_peso     TYPE zquant17_7,
        vl_qtd_nf        TYPE sy-tfill,
        wa_export_aux    TYPE ty_export,

        vl_peso          TYPE p DECIMALS 3,
        vl_sum_lote_peso TYPE f.

  SORT it_instr  BY instrucao charg.
  SORT it_fmlote BY vbeln.

  "  OBS:  Quando ZSDT0051-VKORG = ‘0001’ não há necessidade de fazer a busca na Tabela
  "  ZMMT0008, pois o processo utilizado por essa empresa não alimenta essa tabela
  "  nao se aplica mais a regra acima

  " Totalizações
* REFRESH: it_sum_fat, it_sum_lote, it_sum_instr, it_sum_instr_exp, it_sum_final.
  SORT it_fmlote BY vbeln.

  LOOP AT it_zmmt0008_a INTO wa_zmmt0008.
    CLEAR wa_sum_0008.
    READ TABLE it_fmlote INTO wa_fmlote WITH KEY vbeln = wa_zmmt0008-vbeln
                                        BINARY SEARCH.
    IF sy-subrc = 0.
      wa_fmlote-processado = abap_true.
      MODIFY it_fmlote  FROM wa_fmlote INDEX sy-tabix TRANSPORTING processado.
    ENDIF.

    wa_sum_0008-inst_fml = wa_fmlote-instrucao.
    wa_sum_0008-lote       = wa_zmmt0008-lgort.
    wa_sum_0008-vbeln      = wa_zmmt0008-vbeln.
    wa_sum_0008-vbeln_vf   = wa_zmmt0008-vbeln_vf.
    wa_sum_0008-menge      = wa_zmmt0008-menge.
    wa_sum_0008-qt_fds_fml = 1.
    COLLECT wa_sum_0008 INTO it_sum_lote.

    " Totais por NF
    CLEAR wa_sum_0008-lote.
    COLLECT wa_sum_0008 INTO it_sum_fat.
  ENDLOOP.

  SORT it_sum_fat BY inst_fml lote vbeln vbeln_vf.

  " Atualizar Peso Lote por NF
  LOOP AT it_sum_fat ASSIGNING <fs_sum_fat>.
    READ TABLE it_nflote INTO wa_nflote WITH KEY refkey = <fs_sum_fat>-vbeln_vf.
    IF sy-subrc IS INITIAL.
      <fs_sum_fat>-menge   = wa_nflote-brgew.
      <fs_sum_fat>-peso_7dec  = <fs_sum_fat>-menge / <fs_sum_fat>-qt_fds_fml.
    ENDIF.
  ENDLOOP.

  " Atualizar Peso Lote
  LOOP AT it_sum_lote ASSIGNING <fs_sum_lote>.
    CLEAR wa_sum_0008.

    READ TABLE it_sum_fat ASSIGNING <fs_sum_fat> WITH KEY vbeln_vf = <fs_sum_lote>-vbeln_vf.
    IF sy-subrc IS INITIAL.
      vl_sum_lote_peso = <fs_sum_lote>-qt_fds_fml * <fs_sum_fat>-peso_7dec.
      <fs_sum_lote>-peso = vl_sum_lote_peso.
    ENDIF.

    wa_sum_final-inst_fml   = <fs_sum_lote>-inst_fml.
    wa_sum_final-inst_exp   = <fs_sum_lote>-inst_fml.
    wa_sum_final-lote       = <fs_sum_lote>-lote.
    wa_sum_final-peso       = <fs_sum_lote>-peso.
    wa_sum_final-qt_fds     = <fs_sum_lote>-qt_fds_fml.
    COLLECT wa_sum_final INTO it_sum_final.

    CLEAR wa_sum_instr.
    wa_sum_instr-inst_fml   = <fs_sum_lote>-inst_fml.
    wa_sum_final-inst_exp   = <fs_sum_lote>-inst_fml.
    wa_sum_instr-peso       = <fs_sum_lote>-peso.
    COLLECT wa_sum_instr INTO it_sum_instr.

    " Tabela FARDO1 com Formação Lotes
    READ TABLE it_instr INTO wa_instr WITH KEY instrucao = <fs_sum_lote>-inst_fml
                                               charg     = <fs_sum_lote>-lote
                                      BINARY SEARCH.
    wa_fardo1-kunnr    =  wa_instr-kunnr.
    wa_fardo1-inst_fml =  <fs_sum_lote>-inst_fml.
    wa_fardo1-inst_exp =  <fs_sum_lote>-inst_fml.
    wa_fardo1-lote     =  <fs_sum_lote>-lote.
*-CS2022000332-#79606-17.06.2022-JT-inicio
    wa_fardo1-safra      = wa_instr-safra.
    wa_fardo1-quantidade = wa_instr-quantidade.
    wa_fardo1-btgew      = wa_instr-btgew.
*-CS2022000332-#79606-17.06.2022-JT-fim

    APPEND wa_fardo1 TO it_fardo1.
    CLEAR wa_fardo1.
  ENDLOOP.

  SORT it_nflote_exp BY aubel.

  LOOP AT it_fmlote INTO wa_fmlote WHERE processado = abap_false.
    CLEAR vl_qtd_nf.
    LOOP AT it_nflote INTO wa_nflote WHERE aubel = wa_fmlote-vbeln.
      vl_qtd_nf =  vl_qtd_nf + 1.
      IF vl_qtd_nf = 1.
        CLEAR wa_sum_final.
        wa_sum_final-inst_fml   = wa_fmlote-instrucao.
        wa_sum_final-inst_exp   = wa_fmlote-instrucao.
        wa_sum_final-lote       = wa_fmlote-charg_ori.
        wa_sum_final-qt_fds     = wa_fmlote-volum.
        COLLECT wa_sum_final INTO it_sum_final.
      ENDIF.

      CLEAR wa_sum_instr.
      wa_sum_instr-inst_fml   = wa_fmlote-instrucao.
      wa_sum_final-inst_exp   = wa_fmlote-instrucao.
      wa_sum_instr-peso       = wa_nflote-brgew.
      COLLECT wa_sum_instr INTO it_sum_instr.
    ENDLOOP.
  ENDLOOP.

  LOOP AT it_sum_final ASSIGNING <fs_sum_final>.
    " Tabela FARDO1 com Formação Lotes
    READ TABLE it_instr INTO wa_instr WITH KEY instrucao = <fs_sum_final>-inst_fml
                                               charg     = <fs_sum_final>-lote
                                      BINARY SEARCH.
    wa_fardo1-kunnr    =   wa_instr-kunnr.
    wa_fardo1-inst_fml =  <fs_sum_final>-inst_fml.
    wa_fardo1-inst_exp =  <fs_sum_final>-inst_fml.
    wa_fardo1-lote     =  <fs_sum_final>-lote.
*-CS2022000332-#79606-17.06.2022-JT-inicio
    wa_fardo1-safra      = wa_instr-safra.
    wa_fardo1-quantidade = wa_instr-quantidade.
    wa_fardo1-btgew      = wa_instr-btgew.
*-CS2022000332-#79606-17.06.2022-JT-fim
    APPEND wa_fardo1 TO it_fardo1.
    CLEAR wa_fardo1.
  ENDLOOP.

  LOOP AT it_export INTO wa_export.

    " Tabela FARDO1 com Lotes de Exportação
    CLEAR wa_instr.
    READ TABLE it_instr INTO wa_instr WITH KEY instrucao = wa_export-inst_fml
                                               charg     = wa_export-lgort
                                      BINARY SEARCH.

    wa_export_aux = wa_export.
    CLEAR wa_sum_lotexp.
    wa_sum_lotexp-inst_fml   = wa_export-inst_fml.
    wa_sum_lotexp-inst_exp   = wa_export-inst_exp.
    IF wa_sum_lotexp-inst_exp IS INITIAL.
      wa_sum_lotexp-inst_exp = wa_export-inst_fml.
    ENDIF.
    wa_sum_lotexp-lote       = wa_export-lgort.
    wa_sum_lotexp-qt_fds     = wa_export-volum.
    wa_sum_lotexp-btgew      = wa_instr-btgew.  "*-CS2022000332-#79606-17.06.2022-JT-inicio

    wa_export                = wa_export_aux.

    READ TABLE it_nflote_exp  INTO wa_nflote WITH KEY aubel = wa_export-vbeln.
*                                                        aupos = wa_export-posnr.
*                                             BINARY SEARCH.
    CHECK sy-subrc = 0.

    wa_sum_lotexp-peso = wa_nflote-brgew.

    COLLECT wa_sum_lotexp INTO it_sum_lotexp.

    wa_sum_instr = wa_sum_lotexp.
    CLEAR wa_sum_instr-lote.
    COLLECT wa_sum_instr INTO it_sum_instr_exp.

    " Tabela FARDO1 com Lotes de Exportação
    READ TABLE it_instr INTO wa_instr WITH KEY instrucao = wa_export-inst_fml
                                               charg     = wa_export-lgort
                                      BINARY SEARCH.
    wa_fardo1-kunnr    =  wa_instr-kunnr.
    wa_fardo1-safra    =  wa_instr-safra.
    wa_fardo1-inst_fml =  wa_export-inst_fml.
    wa_fardo1-inst_exp =  wa_export-inst_exp.
    IF wa_fardo1-inst_exp IS INITIAL.
      wa_fardo1-inst_exp = wa_export-inst_fml.
    ENDIF.

    wa_fardo1-lote       =  wa_export-lgort.
*-CS2022000332-#79606-17.06.2022-JT-inicio
    wa_fardo1-quantidade =  wa_instr-quantidade.
    wa_fardo1-volum      =  wa_export-volum.
    wa_fardo1-btgew      =  wa_instr-btgew.
*-CS2022000332-#79606-17.06.2022-JT-fim
    APPEND wa_fardo1    TO it_fardo1.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_SUMARIZA
*&---------------------------------------------------------------------*
FORM f_fardo_sumariza_param_x.

  DATA: vl_calc_peso     TYPE zquant17_7,
        vl_qtd_nf        TYPE sy-tfill,
        wa_export_aux    TYPE ty_export,

        vl_peso          TYPE p DECIMALS 3,
        vl_sum_lote_peso TYPE f.

  SORT it_instr  BY instrucao charg.
  SORT it_fmlote BY vbeln.

  "  OBS:  Quando ZSDT0051-VKORG = ‘0001’ não há necessidade de fazer a busca na Tabela
  "  ZMMT0008, pois o processo utilizado por essa empresa não alimenta essa tabela
  "  nao se aplica mais a regra acima

  " Totalizações
* REFRESH: it_sum_fat, it_sum_lote, it_sum_instr, it_sum_instr_exp, it_sum_final.
  SORT it_fmlote BY vbeln.

  LOOP AT it_zmmt0008_x INTO wa_zmmt0008.
    CLEAR wa_sum_0008.
    READ TABLE it_export INTO wa_export WITH KEY vbeln = wa_zmmt0008-vbeln
                                        BINARY SEARCH.
    wa_sum_0008-inst_fml   = wa_export-inst_fml.
    wa_sum_0008-lote       = wa_zmmt0008-lgort.
    wa_sum_0008-vbeln      = wa_zmmt0008-vbeln.
    wa_sum_0008-vbeln_vf   = wa_zmmt0008-vbeln_vf.
    wa_sum_0008-menge      = wa_zmmt0008-menge.
    wa_sum_0008-qt_fds_fml = 1.
    COLLECT wa_sum_0008 INTO it_sum_lote.

    " Totais por NF
    CLEAR wa_sum_0008-lote.
    COLLECT wa_sum_0008 INTO it_sum_fat.
  ENDLOOP.

  SORT it_sum_fat BY inst_fml lote vbeln vbeln_vf.

  " Atualizar Peso Lote por NF
  LOOP AT it_sum_fat ASSIGNING <fs_sum_fat>.
    READ TABLE it_nflote_exp INTO wa_nflote WITH KEY refkey = <fs_sum_fat>-vbeln_vf.
    IF sy-subrc IS INITIAL.
      <fs_sum_fat>-menge   = wa_nflote-brgew.
      <fs_sum_fat>-peso_7dec  = <fs_sum_fat>-menge / <fs_sum_fat>-qt_fds_fml.
    ENDIF.
  ENDLOOP.

  " Atualizar Peso Lote
  LOOP AT it_sum_lote ASSIGNING <fs_sum_lote>.
    CLEAR wa_sum_0008.

    READ TABLE it_sum_fat ASSIGNING <fs_sum_fat> WITH KEY vbeln_vf = <fs_sum_lote>-vbeln_vf.
    IF sy-subrc IS INITIAL.
      vl_sum_lote_peso = <fs_sum_lote>-qt_fds_fml * <fs_sum_fat>-peso_7dec.
      <fs_sum_lote>-peso = vl_sum_lote_peso.
    ENDIF.

    wa_sum_final-inst_fml   = <fs_sum_lote>-inst_fml.
    wa_sum_final-inst_exp   = <fs_sum_lote>-inst_fml.
    wa_sum_final-lote       = <fs_sum_lote>-lote.
    wa_sum_final-peso       = <fs_sum_lote>-peso.
    wa_sum_final-qt_fds     = <fs_sum_lote>-qt_fds_fml.
    COLLECT wa_sum_final INTO it_sum_final.
    COLLECT wa_sum_final INTO it_sum_lotexp.

    CLEAR wa_sum_instr.
    wa_sum_instr-inst_fml   = <fs_sum_lote>-inst_fml.
    wa_sum_final-inst_exp   = <fs_sum_lote>-inst_fml.
    wa_sum_instr-peso       = <fs_sum_lote>-peso.
    COLLECT wa_sum_instr INTO it_sum_instr.

    " Tabela FARDO1 com Formação Lotes
    READ TABLE it_instr INTO wa_instr WITH KEY instrucao = <fs_sum_lote>-inst_fml
                                               charg     = <fs_sum_lote>-lote
                                      BINARY SEARCH.
    wa_fardo1-kunnr    =  wa_instr-kunnr.
    wa_fardo1-inst_fml =  <fs_sum_lote>-inst_fml.
    wa_fardo1-inst_exp =  <fs_sum_lote>-inst_fml.
    wa_fardo1-lote     =  <fs_sum_lote>-lote.
*-CS2022000332-#79606-17.06.2022-JT-inicio
    wa_fardo1-safra      = wa_instr-safra.
    wa_fardo1-quantidade = wa_instr-quantidade.
    wa_fardo1-btgew      = wa_instr-btgew.
*-CS2022000332-#79606-17.06.2022-JT-fim

    APPEND wa_fardo1 TO it_fardo1.
    CLEAR wa_fardo1.
  ENDLOOP.

  SORT it_nflote_exp BY aubel.

  LOOP AT it_export INTO wa_export.
    CLEAR vl_qtd_nf.
    LOOP AT it_nflote_exp INTO wa_nflote WHERE aubel = wa_export-vbeln.
      vl_qtd_nf =  vl_qtd_nf + 1.
      IF vl_qtd_nf = 1.
        CLEAR wa_sum_final.
        wa_sum_final-inst_fml   = wa_export-inst_fml.
        wa_sum_final-inst_exp   = wa_export-inst_exp.
        wa_sum_final-lote       = wa_export-lgort. "wa_export-charg.
        wa_sum_final-qt_fds     = wa_export-volum.
        COLLECT wa_sum_final INTO it_sum_lotexp. "it_sum_final.
      ENDIF.

      CLEAR wa_sum_instr.
      wa_sum_instr-inst_fml   = wa_export-inst_fml.
      wa_sum_final-inst_exp   = wa_export-inst_exp.
      wa_sum_instr-peso       = wa_nflote-brgew.
      COLLECT wa_sum_instr INTO it_sum_instr.
    ENDLOOP.
  ENDLOOP.

  DELETE it_sum_lotexp WHERE peso = 0.

* LOOP AT it_sum_final ASSIGNING <fs_sum_final>.
  LOOP AT it_sum_lotexp ASSIGNING <fs_sum_final>.
    " Tabela FARDO1 com Formação Lotes
    READ TABLE it_instr INTO wa_instr WITH KEY instrucao = <fs_sum_final>-inst_fml
                                               charg     = <fs_sum_final>-lote
                                      BINARY SEARCH.
    wa_fardo1-kunnr    =   wa_instr-kunnr.
    wa_fardo1-inst_fml =  <fs_sum_final>-inst_fml.
    wa_fardo1-inst_exp =  <fs_sum_final>-inst_fml.
    wa_fardo1-lote     =  <fs_sum_final>-lote.
*-CS2022000332-#79606-17.06.2022-JT-inicio
    wa_fardo1-safra      = wa_instr-safra.
    wa_fardo1-quantidade = wa_instr-quantidade.
    wa_fardo1-volum      = wa_export-volum.
    wa_fardo1-btgew      = wa_instr-btgew.
*-CS2022000332-#79606-17.06.2022-JT-fim
    APPEND wa_fardo1 TO it_fardo1.
    CLEAR wa_fardo1.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_SUMARIZA
*&---------------------------------------------------------------------*
FORM f_fardo_sumariza.

  REFRESH: it_sum_fat, it_sum_lote, it_sum_instr, it_sum_instr_exp, it_sum_final.

  IF it_0051_a[] IS NOT INITIAL.
    PERFORM f_fardo_sumariza_param_a.
  ENDIF.

  IF it_0051_x[] IS NOT INITIAL.
    PERFORM f_fardo_sumariza_param_x.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_ATRIBUI_PESO_LOTE
*&---------------------------------------------------------------------*
FORM  f_fardo_atribui_peso_lote.

  " Atribui Peso Lote ao início de cada registro de Instrução/Instr Exp

  DATA: wa_sum_final_aux TYPE ty_sum_final,
        lv_tabix         TYPE sy-tabix.

  IF it_0051_a[] IS NOT INITIAL.
    SORT it_sum_final BY inst_fml lote.
    LOOP AT it_sum_final INTO wa_sum_final.
      wa_sum_final_aux = wa_sum_final.
      lv_tabix = sy-tabix.
      AT NEW inst_fml.
        wa_sum_final = wa_sum_final_aux.
        READ TABLE it_sum_instr INTO wa_sum_instr WITH KEY inst_fml = wa_sum_final-inst_fml.
        wa_sum_final-peso_tot  = wa_sum_instr-peso.
        MODIFY it_sum_final FROM wa_sum_final INDEX lv_tabix.
      ENDAT.
    ENDLOOP.
  ENDIF.

  SORT it_sum_lotexp BY inst_fml inst_exp lote.

  IF it_0051_x[] IS NOT INITIAL.
    LOOP AT it_sum_lotexp INTO wa_sum_final.
      wa_sum_final_aux = wa_sum_final.
      lv_tabix = sy-tabix.
      AT NEW inst_exp.
        wa_sum_final = wa_sum_final_aux.
        READ TABLE it_sum_instr_exp INTO wa_sum_instr WITH KEY inst_fml = wa_sum_final-inst_fml
                                                               inst_exp = wa_sum_final-inst_exp.
        wa_sum_final-peso_tot  = wa_sum_instr-peso.
        MODIFY it_sum_lotexp FROM wa_sum_final INDEX lv_tabix.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FARDO_EXIBE_SAIDA
*&---------------------------------------------------------------------*
FORM f_fardo_exibe_saida.

  DATA: vl_dec       TYPE p DECIMALS 3,
        vl_dec_round TYPE p DECIMALS 3 VALUE '0.900'.

  SORT it_fardo1 BY  kunnr inst_fml inst_exp lote.
  DELETE ADJACENT DUPLICATES FROM it_fardo1 COMPARING kunnr
                                                      inst_fml
                                                      inst_exp
                                                      lote.

  SORT it_sum_final  BY inst_fml inst_exp lote.
  SORT it_sum_lotexp BY inst_fml inst_exp lote.

  LOOP AT it_fardo1 ASSIGNING <fs_fardo1>.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = <fs_fardo1>-kunnr
                                    BINARY SEARCH.
    <fs_fardo1>-name1    = wa_kna1-name1.

    READ TABLE it_sum_final INTO wa_sum_final WITH KEY inst_fml = <fs_fardo1>-inst_fml
                                                       inst_exp = <fs_fardo1>-inst_exp
                                                       lote     = <fs_fardo1>-lote
                                              BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_fardo1>-qt_fds_fml  = wa_sum_final-qt_fds.
      <fs_fardo1>-volum       = wa_sum_final-qt_fds.

*     <fs_fardo1>-btgew  = wa_sum_final-btgew.     "*-CS2022000332-#79606-17.06.2022-JT-inicio
*     <FS_FARDO1>-PESO_FML    = WA_SUM_FINAL-PESO_TOT.

      READ TABLE it_instr INTO wa_instr WITH KEY instrucao = <fs_fardo1>-inst_fml "*-CS2022000332-#79606-17.06.2022-JT-inicio
                                                 charg     = <fs_fardo1>-lote
                                        BINARY SEARCH.
*     IF sy-subrc = 0.
*       wa_sum_final-peso_tot = wa_instr-btgew.
*     ENDIF.

      vl_dec = frac( wa_sum_final-peso_tot ).

      IF vl_dec > vl_dec_round.
        <fs_fardo1>-peso_fml  = ceil( wa_sum_final-peso_tot ).
      ELSE.
        <fs_fardo1>-peso_fml  = wa_sum_final-peso_tot.
      ENDIF.
      "*-CS2022000332-#79606-17.06.2022-JT-inicio
*     IF  <fs_fardo1>-peso_fml IS NOT INITIAL.
*       IF  <fs_fardo1>-inst_fml EQ <fs_fardo1>-inst_exp.
*         READ TABLE it_sum_instr_exp INTO wa_sum_instr WITH KEY inst_fml = <fs_fardo1>-inst_fml
*                                                                inst_exp = <fs_fardo1>-inst_exp.
*         IF sy-subrc IS INITIAL.
*           <fs_fardo1>-peso_exp    = wa_sum_instr-peso.
*         ENDIF.
*       ENDIF.
*     ENDIF.
      "*-CS2022000332-#79606-17.06.2022-JT-fim

    ENDIF.

    READ TABLE it_sum_lotexp INTO wa_sum_final WITH KEY inst_fml  = <fs_fardo1>-inst_fml
                                                        inst_exp  = <fs_fardo1>-inst_exp
                                                        lote      = <fs_fardo1>-lote
                                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_fardo1>-qt_fds_exp  = wa_sum_final-qt_fds.
      <fs_fardo1>-peso_exp    = wa_sum_final-peso.

      IF <fs_fardo1>-inst_fml NE <fs_fardo1>-inst_exp.
        <fs_fardo1>-peso_exp = wa_sum_final-peso_tot.
      ENDIF.
*     <fs_fardo1>-btgew  = wa_sum_final-btgew. "*-CS2022000332-#79606-17.06.2022-JT-inicio
    ENDIF.

*-#79606-14.07.2022-JT-inicio
    IF r_venex = abap_true.
      <fs_fardo1>-qt_fds_sld  = <fs_fardo1>-quantidade - <fs_fardo1>-volum.
      <fs_fardo1>-peso_sld    = <fs_fardo1>-btgew      - <fs_fardo1>-peso_exp.
    ELSE.
      <fs_fardo1>-qt_fds_sld  = <fs_fardo1>-qt_fds_fml - <fs_fardo1>-qt_fds_exp.
      <fs_fardo1>-peso_sld    = <fs_fardo1>-peso_fml   - <fs_fardo1>-peso_exp.
    ENDIF.
*-#79606-14.07.2022-JT-fim

*  Monta tabela Visão por Peso

    wa_fardo1 =  <fs_fardo1>.
    CLEAR: wa_fardo1-lote,
           wa_fardo1-qt_fds_fml,
           wa_fardo1-qt_fds_exp,
           wa_fardo1-qt_fds_sld.
    COLLECT wa_fardo1 INTO it_fardo2.

  ENDLOOP.

  " Opção SALDO - Aba “Visão por Fardos”:  Deve exibir apenas os registros quando na coluna “Fds à Exportar”  <> ‘Zero’
  "               Aba “Visão por Peso”  :  Deve exibir apenas os registros quando na coluna “Peso à Exportar” <> ‘Zero’

  IF r_saldo IS NOT INITIAL.
    DELETE it_fardo1 WHERE qt_fds_sld IS INITIAL.
    DELETE it_fardo2 WHERE peso_sld   IS INITIAL.
  ENDIF.

  SORT it_fardo1 BY inst_fml lote   ASCENDING
                         qt_fds_fml DESCENDING.
  SORT it_fardo2 BY inst_fml inst_exp.

*  Exibir Relatórios
  CALL SCREEN 0300.


ENDFORM.
