*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: AMAGGI                                                  *
* Autor......: Vagner Rodrigues dos Santos                             *
* Data.......: 04.09.2010                                              *
* Descrição  : Programa de execução da Miro para Frete Próprio         *
* Transação..:                                                         *
* Projeto....: INOVAR                                                  *
* Cód Espec..:                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                                         *
* Observações:                                                         *
* Data.......:                                                         *
*----------------------------------------------------------------------*

REPORT  zlesr0010 NO STANDARD PAGE HEADING LINE-COUNT 60 LINE-SIZE 120.

* Tabela
TABLES  zlest0032. "Relac. frete proprio/terceiro com a revisão de fatura (Miro)

TYPES: BEGIN OF ty_zib_contabil.
         INCLUDE STRUCTURE zib_contabil.
TYPES:   rate TYPE j_1btxpis-rate,
       END OF ty_zib_contabil.
* Tipo
TYPES: BEGIN OF y_dados,
         tknum               TYPE zlest0032-tknum,
         fknum               TYPE zlest0032-fknum,
         ebeln               TYPE zlest0032-ebeln,
         ebelp               TYPE zlest0032-ebelp,
         lblni               TYPE zlest0032-lblni,
         budat               TYPE vfkk-budat,
         add03               TYPE zlest0032-add03,
         vsart               TYPE vttk-vsart,
         tplst               TYPE vttk-tplst,
         exti1               TYPE vttk-exti1,
         exti2               TYPE vttk-exti2,
         shtyp               TYPE vttk-shtyp,
         tdlnr               TYPE vttk-tdlnr,
         meins               TYPE ekpo-meins,
         tp_emissor          TYPE zlest0021-tp_emissor,
         werks               TYPE lips-werks,
         fornecedor          TYPE lfa1-lifnr,
         ck_gera_miro        TYPE char01,
         tp_veiculo          TYPE zde_tp_prop_veiculo_ctb,
         bukrs               TYPE bukrs,
         branch	             TYPE j_1bbranc_,
         ck_gera_seguro      TYPE char01,
         tipo_remetente      TYPE zde_tp_reme_mercadoria,
         emite_conhecimento  TYPE char01,
         ck_nao_gera_subcont TYPE char01,
       END OF y_dados,

       BEGIN OF y_dados_est,
         belnr  TYPE zlest0032-belnr,
         gjahr  TYPE zlest0032-gjahr,
         tknum  TYPE zlest0032-tknum,
         fknum  TYPE zlest0032-fknum,
         ebeln  TYPE zlest0032-ebeln,
         ebelp  TYPE zlest0032-ebelp,
         lblni  TYPE zlest0032-lblni,
         docnum TYPE zlest0032-docnum,
         bukrs  TYPE rbkp-bukrs,
         lifnr  TYPE rbkp-lifnr,
       END   OF y_dados_est,

       BEGIN OF y_dados_todos,
         tknum TYPE zlest0032-tknum,
         fknum TYPE zlest0032-fknum,
         ebeln TYPE zlest0032-ebeln,
         ebelp TYPE zlest0032-ebelp,
         lblni TYPE zlest0032-lblni,
         vsart TYPE vttk-vsart,
         tplst TYPE vttk-tplst,
         exti1 TYPE vttk-exti1,
         exti2 TYPE vttk-exti2,
         shtyp TYPE vttk-shtyp,
         tdlnr TYPE vttk-tdlnr,
         meins TYPE ekpo-meins,
         knumv TYPE vfkp-knumv,
         budat TYPE vfkp-budat,
         prsdt TYPE vfkp-prsdt,
         bukrs TYPE vfkp-bukrs,
         waers TYPE vfkp-waers,
         netwr TYPE vfkp-netwr,
       END OF y_dados_todos,

       BEGIN OF y_vfkp,
         fknum TYPE vfkp-fknum,
         fkpos TYPE vfkp-fkpos,
         knumv TYPE vfkp-knumv,
         budat TYPE vfkp-budat,
         prsdt TYPE vfkp-prsdt,
         bukrs TYPE vfkp-bukrs,
         waers TYPE vfkp-waers,
         netwr TYPE vfkp-netwr,
         werks TYPE vfkp-werks,
       END OF y_vfkp,

       BEGIN OF y_vfkp_acum,
         fknum TYPE vfkp-fknum,
         waers TYPE vfkp-waers,
         netwr TYPE vfkp-netwr,
       END OF y_vfkp_acum,

       BEGIN OF y_konv,
         knumv TYPE konv-knumv,
         kposn TYPE konv-kposn,
         kschl TYPE konv-kschl,
         kwert TYPE konv-kwert,
       END OF y_konv,

       BEGIN OF y_ekbe,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         lfbnr TYPE ekbe-lfbnr,
         belnr TYPE ekbe-belnr,
         lfgja TYPE ekbe-lfgja,
         menge TYPE ekbe-menge,
       END OF y_ekbe,

*       BEGIN OF Y_ZLEST0021,
**         SHTYP      TYPE ZLEST0021-SHTYP,
**         TCODE      TYPE ZLEST0021-TCODE,
**         FATURA     TYPE ZLEST0021-FATURA,
**         TP_EMISSOR TYPE ZLEST0021-TP_EMISSOR,
**         RAZAODEB   TYPE ZLEST0021-RAZAODEB,
**         RAZAOCRED  TYPE ZLEST0021-RAZAOCRED,
**         OPERFRETE  TYPE ZLEST0021-OPERFRETE,
**         TP_VEICULO TYPE ZLEST0021-TP_VEICULO,
*
*       END OF Y_ZLEST0021,

       BEGIN OF y_vttp,
         tknum TYPE vttp-tknum,
         vbeln TYPE vttp-vbeln,
       END OF y_vttp,

       BEGIN OF y_lips,
         vbeln TYPE lips-vbeln,
         werks TYPE lips-werks,
       END OF y_lips,

       BEGIN OF y_j_1bbranch,
         branch TYPE j_1bbranch-branch,
         bukrs  TYPE j_1bbranch-bukrs,
       END OF y_j_1bbranch,

       BEGIN OF y_vtpa,
         vbeln TYPE vtpa-vbeln,
         parvw TYPE vtpa-parvw,
         lifnr TYPE vtpa-lifnr,
       END OF y_vtpa,

       BEGIN OF y_lifnr_pv,
         lifnr TYPE lfa1-lifnr,
         stkzn TYPE lfa1-stkzn,
       END OF y_lifnr_pv,

       BEGIN OF y_notas,
         tknum   TYPE zlest0032-tknum,
         vbeln_k TYPE vbak-vbeln,
         vbeln_f TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         docnum  TYPE j_1bnflin-docnum,
         text1   TYPE vttk-text1,
         shtyp   TYPE vttk-shtyp,
         tplst   TYPE vttk-tplst,
       END OF y_notas,

       BEGIN OF y_vbak,
         tknum    TYPE zlest0032-tknum,
         vbeln    TYPE vbak-vbeln,
         kunnr    TYPE vbak-kunnr,
         fkara    TYPE vbak-fkara,
         bukrs_vf TYPE vbak-bukrs_vf,
         vkorg    TYPE vbak-vkorg,
         vtweg    TYPE vbak-vtweg,
         spart    TYPE vbak-spart,
       END OF y_vbak,

       BEGIN OF y_vbap,
         vbeln TYPE vbap-vbeln,
         matnr TYPE vbap-matnr,
         werks TYPE vbap-werks,
         gsber TYPE vbap-gsber,
       END OF y_vbap,

       BEGIN OF y_taxas,
         lifnr     TYPE lfbw-lifnr,
         bukrs     TYPE lfbw-bukrs,
         witht     TYPE lfbw-witht,
         wt_withcd TYPE lfbw-wt_withcd,
         qproz     TYPE v_t059z-qproz,
       END OF y_taxas.

* Campos auxiliares
DATA: vg_invoicedocnumber_miro TYPE bapi_incinv_fld-inv_doc_no,
      vg_ano_miro              TYPE bapi_incinv_fld-fisc_year,
      vg_docnum                TYPE bapi_j_1bnfdoc-docnum,
      vg_fisica                TYPE char1,
      vg_augdt                 TYPE augdt,
      vg_augbl                 TYPE augbl,
      vg_docnum_est            TYPE j_1bdocnum,
      t_return                 TYPE TABLE OF bapiret2,
      w_return                 TYPE bapiret2,
      obj_cte                  TYPE REF TO zcl_cte_dist_g,
      gva_msg_error            TYPE c LENGTH 250,
      w_bkpf                   TYPE bkpf,
      vg_kbert_pis_pf          TYPE zlest0215-kbert_pis,
      vg_kbert_cofins_pf       TYPE zlest0215-kbert_cofins,
      lc_vlr_pedagio           TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
      lc_vlr_pis_ped           TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
      lc_vlr_cofins_ped        TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
      lc_vlr_liq_ped           TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
      v_stblg                  TYPE bkpf-stblg,
      v_belnr                  TYPE bkpf-belnr.


DATA: vobj_key    TYPE zib_contabil-obj_key,
      vobj_keypd  TYPE zib_contabil-obj_key,
      vobj_keysg  TYPE zib_contabil-obj_key,
      vobj_keysu  TYPE zib_contabil-obj_key,
      vobj_keyre  TYPE zib_contabil-obj_key,
      v_awkey_sub TYPE awkey,
      v_awkey_rec TYPE awkey.

* Tabelas internas
DATA: ti_dados           TYPE TABLE OF y_dados,
      ti_vfkp            TYPE TABLE OF y_vfkp,
      ti_vfkp_acum       TYPE TABLE OF y_vfkp_acum,
      ti_konv            TYPE TABLE OF y_konv,
      ti_ekbe            TYPE TABLE OF y_ekbe,
      ti_zlest0021       TYPE TABLE OF zlest0021,
      ti_vtpa            TYPE TABLE OF y_vtpa,
      ti_lifnr_pv        TYPE TABLE OF y_lifnr_pv,
      ti_notas           TYPE TABLE OF y_notas,
      ti_itemdata        TYPE TABLE OF bapi_incinv_create_item,
      ti_glaccountdata   TYPE TABLE OF bapi_incinv_create_gl_account,
      ti_withtaxdata     TYPE TABLE OF bapi_incinv_create_withtax,
      ti_return          TYPE TABLE OF bapiret2,
      ti_header          TYPE TABLE OF j_1bnfdoc,
      ti_header_nfe      TYPE TABLE OF j_1bnfe_active,
      ti_partner         TYPE TABLE OF j_1bnfnad,
      ti_item            TYPE TABLE OF j_1bnflin,
      ti_item_tax        TYPE TABLE OF j_1bnfstx,
      ti_header_msg      TYPE TABLE OF j_1bnfftx,
      ti_refer_msg       TYPE TABLE OF j_1bnfref,
      ti_obj_partner     TYPE TABLE OF bapi_j_1bnfnad,
      ti_obj_item        TYPE TABLE OF bapi_j_1bnflin,
      ti_obj_item_add    TYPE TABLE OF bapi_j_1bnflin_add,
      ti_obj_item_tax    TYPE TABLE OF bapi_j_1bnfstx,
      ti_obj_ot_partner  TYPE TABLE OF bapi_j_1bnfcpd,
      ti_vbak            TYPE TABLE OF y_vbak,
      ti_vbap            TYPE TABLE OF y_vbap,
      ti_taxas           TYPE TABLE OF y_taxas,
      ti_vbfa            TYPE TABLE OF vbfa,
      ti_j_1bnflin       TYPE TABLE OF j_1bnflin,
      ti_j_1bnfe_active  TYPE TABLE OF j_1bnfe_active,
      ti_zsdt_depara_cen TYPE TABLE OF zsdt_depara_cen,
      ti_dados_est       TYPE TABLE OF y_dados_est.

DATA: r_refkey TYPE RANGE OF j_1brefkey,
      r_tknum  TYPE RANGE OF tknum,
      zvbelv   TYPE vbeln_von,
      l_rudat  TYPE tvpod-rudat, l_rutim TYPE tvpod-rutim.


* Estrutruas
DATA: st_dados            TYPE y_dados,
      st_dados_aux        TYPE y_dados,
      st_dados_est        TYPE y_dados_est,
      st_vfkp             TYPE y_vfkp,
      st_vfkp_acum        TYPE y_vfkp_acum,
      st_konv             TYPE y_konv,
      st_ekbe             TYPE y_ekbe,
      st_zlest0021        TYPE zlest0021,
      st_vtpa             TYPE y_vtpa,
      st_notas            TYPE y_notas,
      st_header           TYPE j_1bnfdoc,
      st_header_nfe       TYPE j_1bnfe_active,
      st_header_nfe_aux   TYPE j_1bnfe_active,
      st_item             TYPE j_1bnflin,
      st_item_tax         TYPE j_1bnfstx,
      st_headerdata       TYPE bapi_incinv_create_header,
      st_itemdata         TYPE bapi_incinv_create_item,
      st_glaccountdata    TYPE bapi_incinv_create_gl_account,
      st_withtaxdata      TYPE bapi_incinv_create_withtax,
      it_zib_contabil     TYPE TABLE OF ty_zib_contabil WITH HEADER LINE,
      it_zib_contabil_sub TYPE TABLE OF ty_zib_contabil WITH HEADER LINE,
      wa_zib_contabil_err TYPE  zib_contabil_err,
      st_zib_contabil     TYPE ty_zib_contabil,
      st_return           TYPE bapiret2,
      st_obj_header       TYPE bapi_j_1bnfdoc,
      st_obj_partner      TYPE bapi_j_1bnfnad,
      st_obj_item         TYPE bapi_j_1bnflin,
      st_obj_item_tax     TYPE bapi_j_1bnfstx,
      st_vbak             TYPE y_vbak,
      st_vbap             TYPE y_vbap,
      wa_lfa1             TYPE lfa1,
      st_taxas            TYPE y_taxas,
      st_zsdt_depara_cen  TYPE zsdt_depara_cen.

" Tabelas e Workarea BAPI_ACC
DATA: gd_documentheader LIKE bapiache09,
      wa_returnobj      LIKE zfie_returnobj,
      wa_accountgl      LIKE bapiacgl09,
      wa_receivable     LIKE bapiacar09,
      wa_payable        LIKE bapiacap09,
      wa_currencyamount LIKE bapiaccr09,
      wa_criteria       LIKE bapiacwt09,
      wa_extension1     LIKE bapiacextc,
      wa_bapiret        LIKE bapiret2,

      it_accountgl      LIKE STANDARD TABLE OF wa_accountgl,
      it_receivable     LIKE STANDARD TABLE OF wa_receivable,
      it_payable        LIKE STANDARD TABLE OF wa_payable,
      it_currencyamount LIKE STANDARD TABLE OF wa_currencyamount,
      it_bapiret        LIKE STANDARD TABLE OF wa_bapiret,
      it_accountwt      LIKE STANDARD TABLE OF wa_criteria,
      it_criteria       LIKE bapiackec9 OCCURS 0 WITH HEADER LINE,
      it_accounttax     LIKE bapiactx09 OCCURS 0 WITH HEADER LINE,
      it_extension1     LIKE STANDARD TABLE OF bapiacextc.

*-CS2020000576 - 01.09.2021 - JT - inicio
TYPES: BEGIN OF ty_vttk.
         INCLUDE STRUCTURE vttk.
TYPES:   branch TYPE j_1bbranch-branch,
       END OF ty_vttk.

DATA: tl_vttk TYPE TABLE OF ty_vttk,
      sl_vttk TYPE ty_vttk.
*-CS2020000576 - 01.09.2021 - JT - fim

* Constantes
CONSTANTS: c_0000000001(10)         TYPE c VALUE '0000000001',
           c_zfre(4)                TYPE c VALUE 'ZFRE',
           c_zped(4)                TYPE c VALUE 'ZPED',
           c_zseg(4)                TYPE c VALUE 'ZSEG',
           c_ziof(4)                TYPE c VALUE 'ZIOF',
           c_zicm(4)                TYPE c VALUE 'ZICM',
           c_zsen(4)                TYPE c VALUE 'ZSEN',
           c_zset(4)                TYPE c VALUE 'ZSET',
           c_zinn(4)                TYPE c VALUE 'ZINN',
           c_zins(4)                TYPE c VALUE 'ZINS',
           c_ziss(4)                TYPE c VALUE 'ZISS',
           c_zred(4)                TYPE c VALUE 'ZRED',
           c_zirf(4)                TYPE c VALUE 'ZIRF',
           c_zbir(4)                TYPE c VALUE 'ZBIR',

           c_zpis(4)                TYPE c VALUE 'ZPIS',
           c_zcof(4)                TYPE c VALUE 'ZCOF',

           c_bbra(4)                TYPE c VALUE 'BBRA',
           c_hsbc(4)                TYPE c VALUE 'HSBC',
           c_bbd(3)                 TYPE c VALUE 'BBD',
           c_001(3)                 TYPE c VALUE '001',
           c_237(3)                 TYPE c VALUE '237',
           c_399(3)                 TYPE c VALUE '399',
           c_01(2)                  TYPE c VALUE '01',
           c_0                      TYPE c VALUE '0',
           c_1                      TYPE c VALUE '1',
           c_2                      TYPE c VALUE '2',
           c_3                      TYPE c VALUE '3',
           c_4                      TYPE c VALUE '4',
           c_5                      TYPE c VALUE '5',
           c_6                      TYPE c VALUE '6',
           c_9                      TYPE c VALUE '9',
           c_10(2)                  TYPE c VALUE '10',
           c_mais(1)                TYPE c VALUE '+',
           c_12(2)                  TYPE c VALUE '12',
           c_14(2)                  TYPE c VALUE '14',
           c_21(2)                  TYPE c VALUE '21',
           c_23(2)                  TYPE c VALUE '23',
           c_26(2)                  TYPE c VALUE '26',
           c_27(2)                  TYPE c VALUE '27',
           c_28(2)                  TYPE c VALUE '28',
           c_29(2)                  TYPE c VALUE '29',
           c_30(2)                  TYPE c VALUE '30',
           c_31(2)                  TYPE c VALUE '31',
           c_40(2)                  TYPE c VALUE '40',
           c_50(2)                  TYPE c VALUE '50',
           c_miro(4)                TYPE c VALUE 'MIRO',
           c_f02(4)                 TYPE c VALUE 'F-02',
           c_p                      TYPE c VALUE 'P',
           c_li(2)                  TYPE c VALUE 'LI',
           c_c1(2)                  TYPE c VALUE 'C1',
           c_f2(2)                  TYPE c VALUE 'F2',
           c_pv(2)                  TYPE c VALUE 'PV',
           c_pd(2)                  TYPE c VALUE 'PD',
           c_sg(2)                  TYPE c VALUE 'SG',
           c_me(2)                  TYPE c VALUE 'ME',
           c_brl(3)                 TYPE c VALUE 'BRL',
           c_ss(2)                  TYPE c VALUE 'SS',
           c_ic(2)                  TYPE c VALUE 'IC',
           c_iw(2)                  TYPE c VALUE 'IW',
           c_is(2)                  TYPE c VALUE 'IS',
           c_in(2)                  TYPE c VALUE 'IN',
           c_c                      TYPE c VALUE 'C',
           c_e                      TYPE c VALUE 'E',
           c_h                      TYPE c VALUE 'H',
           c_j                      TYPE c VALUE 'J',
           c_lf(2)                  TYPE c VALUE 'LF',
           c_m                      TYPE c VALUE 'M',
           c_n                      TYPE c VALUE 'N',
           c_s                      TYPE c VALUE 'S',
           c_a                      TYPE c VALUE 'A',
           c_u                      TYPE c VALUE 'U',
           c_v                      TYPE c VALUE 'V',
           c_x                      TYPE c VALUE 'X',
           c_fr(2)                  TYPE c VALUE 'FR',
           c_z001(4)                TYPE c VALUE 'Z001',
           c_z005(4)                TYPE c VALUE 'Z005',
           "C_I7(2)              TYPE C VALUE 'I7',
           "C_I8(2)              TYPE C VALUE 'I8',
           c_c2(2)                  TYPE c VALUE 'C2',
           c_0001(4)                TYPE c VALUE '0001',
           c_000001(6)              TYPE c VALUE '000001',
           c_000002(6)              TYPE c VALUE '000002',
           c_000003(6)              TYPE c VALUE '000003',
           c_000004(6)              TYPE c VALUE '000004',
           c_000005(6)              TYPE c VALUE '000005',
           c_000006(6)              TYPE c VALUE '000006',
           c_000007(6)              TYPE c VALUE '000007',
           c_000008(6)              TYPE c VALUE '000008',
           c_000009(6)              TYPE c VALUE '000009',
           c_000010(6)              TYPE c VALUE '000010',
           c_000011(6)              TYPE c VALUE '000011',
           c_000012(6)              TYPE c VALUE '000012',
           c_les(3)                 TYPE c VALUE 'LES',
           c_zform01(10)            TYPE c VALUE 'ZFORM01',
           c_pedagio_frete(13)      TYPE c VALUE 'PEDAGIO-FRETE',
           c_seguro_frete(12)       TYPE c VALUE 'SEGURO-FRETE',
           c_seguro(6)              TYPE c VALUE 'SEGURO',
           c_pedagio(7)             TYPE c VALUE 'PEDAGIO',
           c_subcontra_total(15)    TYPE c VALUE 'SUBCONTRA_TOTAL',
           c_subcontra_liqui(15)    TYPE c VALUE 'SUBCONTRA_LIQUI',
           c_subcontra_cofin(15)    TYPE c VALUE 'SUBCONTRA_COFIN',
           c_subcontra_pis(13)      TYPE c VALUE 'SUBCONTRA_PIS',

           c_subcontra_patronal(13) TYPE c VALUE 'SUBCONTRA_PATRONAL',

           c_subcontra_frete(20)    TYPE c VALUE 'SUBCONTRATADO-FRETE',

           c_receita_liquida(15)    TYPE c VALUE 'RECEITA_LIQUIDA',
           c_receita_seguro(14)     TYPE c VALUE 'RECEITA_SEGURO',
           c_receita_bruto(13)      TYPE c VALUE 'RECEITA_BRUTO',

           c_txt_seguro(27)         TYPE c VALUE 'Valor Seguro - Conhecimento',
           c_txt_pedagio(22)        TYPE c VALUE 'Pedágio - Conhecimento',
           c_ponto                  TYPE c VALUE '.',
           c_miro_frete_proprio(18) TYPE c VALUE 'Miro Frete Próprio',
           c_pis_cof_pv_pf(20)      TYPE c VALUE 'PIS_COF_PV_PF',     "*-CS2022000741-04.10.1024-#83334-JT-inicio
           c_pis_cof_ped(20)        TYPE c VALUE 'PIS_COF_PED'.       "*-CS2022000741-04.10.1024-#83334-JT-inicio

*----------------------------------------------------------------------*
* Critérios de seleção
SELECTION-SCREEN: BEGIN OF BLOCK bl_001 WITH FRAME TITLE TEXT-010.
  SELECT-OPTIONS: s_tknum     FOR zlest0032-tknum,
                  s_fknum     FOR zlest0032-fknum,
                  s_ebeln     FOR zlest0032-ebeln,
                  s_ebelp     FOR zlest0032-ebelp,
                  s_lblni     FOR zlest0032-lblni.
SELECTION-SCREEN: END OF BLOCK bl_001.

SELECTION-SCREEN: BEGIN OF BLOCK bl_002 WITH FRAME TITLE TEXT-015.
  PARAMETERS:     c_ped TYPE c AS CHECKBOX USER-COMMAND uc1.
  PARAMETERS:     c_seg TYPE c AS CHECKBOX USER-COMMAND uc1.
  PARAMETERS:     c_sub TYPE c AS CHECKBOX USER-COMMAND uc1.
  PARAMETERS:     c_rci TYPE c AS CHECKBOX USER-COMMAND uc1.
*-CS2020000460 - 25.08.2021 - JT - inicio
  PARAMETERS:     c_est TYPE c AS CHECKBOX USER-COMMAND uc1.
*-CS2020000460 - 25.08.2021 - JT - fim
SELECTION-SCREEN: END OF BLOCK bl_002.

*-CS2020000460 - 25.08.2021 - JT - inicio
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF c_est = abap_true.
      IF screen-name = 'C_PED' OR
         screen-name = 'C_SEG' OR
         screen-name = 'C_SUB' OR
         screen-name = 'C_RCI'.
        CLEAR: c_ped, c_seg, c_sub, c_rci.
        screen-input = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*-CS2020000460 - 25.08.2021 - JT - fim

*----------------------------------------------------------------------*
START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF ( NOT c_seg IS INITIAL OR NOT c_ped IS INITIAL OR NOT c_sub IS INITIAL OR NOT c_rci IS INITIAL OR
       NOT c_est IS INITIAL ) AND
     ( s_tknum IS INITIAL ).
    MESSAGE 'Deve ser informado o Nr. transporte!' TYPE 'S'.
  ENDIF.

*-CS2020000460 - 25.08.2021 - JT - inicio
  IF c_est = abap_true.
*-- Estorno
    PERFORM z_ler_dados_estorno.

    IF ti_dados_est[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Não ha documentos a estornar!'.
      STOP.
    ENDIF.
*-CS2020000460 - 25.08.2021 - JT - fim
  ELSE.
*-- Obter dados da tabela zlest0032.
    PERFORM z_ler_dados_diversos.

*-- Obter dados da tabela vfkp se houver dados na tabela interna ti_zlest0032.
    CHECK ti_dados IS NOT INITIAL.
    PERFORM z_ler_vfkp.

*-- Obter dados da tabela konv se houver dados na tabela interna ti_vfkp.
    CHECK ti_vfkp IS NOT INITIAL.
    PERFORM z_ler_konv.

*-- Obter histórico de documento de compras
    PERFORM z_ler_ekbe.

*-- Selecionar contas contábeis para transações MIRO e F-02
    " PERFORM Z_LER_ZLEST0021.

*-- Selecionar os parceiros do transporte
    PERFORM z_ler_vtpa.

*-- Selecionar taxa
    PERFORM z_obter_taxas.

*-- Obter dados da nota fiscal de serviço de transporte
    PERFORM z_obter_dados_nf.
  ENDIF.

*----------------------------------------------------------------------*
END-OF-SELECTION.

* Direcionar para rotina de execução da bapi
*-CS2020000460 - 25.08.2021 - JT - inicio
  IF c_est = abap_true.
    PERFORM z_processar_estorno.
*-CS2020000460 - 25.08.2021 - JT - fim
  ELSE.
    IF NOT ( ( ( NOT c_seg IS INITIAL ) ) AND ( s_tknum IS INITIAL ) ).
      PERFORM z_processar_dados.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_ESTORNO
*&---------------------------------------------------------------------*
*  Obter dados de várias tabelas..
*----------------------------------------------------------------------*
FORM z_ler_dados_estorno.

  DATA: tl_zlest0032 TYPE TABLE OF zlest0032,
        sl_zlest0032 TYPE zlest0032,
        tl_rbkp      TYPE TABLE OF rbkp,
        sl_rbkp      TYPE rbkp.

  FREE: ti_dados_est.

  SELECT zlest0032~*
    FROM zlest0032
   INNER JOIN essr ON zlest0032~lblni = essr~lblni
    INTO TABLE @tl_zlest0032
   WHERE zlest0032~tknum IN @s_tknum
     AND zlest0032~fknum IN @s_fknum
     AND zlest0032~ebeln IN @s_ebeln
     AND zlest0032~ebelp IN @s_ebelp
     AND zlest0032~lblni IN @s_lblni
     AND zlest0032~add03 EQ @c_0000000001
     AND essr~loekz      NE 'X'.

  DELETE tl_zlest0032 WHERE belnr = abap_off.

  CHECK tl_zlest0032[] IS NOT INITIAL.

  SELECT *
    FROM rbkp
    INTO TABLE @tl_rbkp
     FOR ALL ENTRIES IN @tl_zlest0032
    WHERE belnr = @tl_zlest0032-belnr
      AND gjahr = @tl_zlest0032-gjahr.

  SORT tl_rbkp BY belnr gjahr.

  LOOP AT tl_zlest0032 INTO sl_zlest0032.

    CLEAR sl_rbkp.
    READ TABLE tl_rbkp INTO sl_rbkp WITH KEY belnr = sl_zlest0032-belnr
                                             gjahr = sl_zlest0032-gjahr
                                    BINARY SEARCH.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING sl_zlest0032 TO st_dados_est.
    MOVE-CORRESPONDING sl_rbkp      TO st_dados_est.
    APPEND st_dados_est             TO ti_dados_est.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_LER_DADOS_DIVERSOS
*&---------------------------------------------------------------------*
*  Obter dados de várias tabelas..
*----------------------------------------------------------------------*
FORM z_processar_estorno.

  LOOP AT ti_dados_est INTO st_dados_est.

    FREE: t_return,      vg_augdt,                 vg_augbl,
          vg_docnum_est, vg_invoicedocnumber_miro, vg_ano_miro,
          w_bkpf.

    MOVE-CORRESPONDING st_dados_est TO st_dados.

*----------------------------
*-- esta pendente de compensacao
*----------------------------
    CALL FUNCTION 'Z_VERIFICA_MIRO_PAGA'
      EXPORTING
        belnr = st_dados_est-belnr
        gjahr = st_dados_est-gjahr
      CHANGING
        augdt = vg_augdt
        augbl = vg_augbl.

    IF vg_augdt IS NOT INITIAL.
      CONCATENATE 'Miro ' st_dados_est-belnr  '/' st_dados_est-gjahr 'já paga/compensada.'
                  'Doc. Compensação:' vg_augbl '/' vg_augdt(4)
                  'Necessário estornar compensação.'
             INTO st_return-message SEPARATED BY space.
      PERFORM z_listar_inconsistencias.
      CONTINUE.
    ENDIF.

*----------------------------
*-- estorno da miro
*----------------------------
    IF st_dados_est-belnr IS NOT INITIAL AND
       st_dados_est-gjahr IS NOT INITIAL.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
        EXPORTING
          invoicedocnumber          = st_dados_est-belnr
          fiscalyear                = st_dados_est-gjahr
          reasonreversal            = 'Z1'
        IMPORTING
          invoicedocnumber_reversal = vg_invoicedocnumber_miro
          fiscalyear_reversal       = vg_ano_miro
        TABLES
          return                    = t_return.

      IF t_return[] IS INITIAL.
        CONCATENATE 'Estorno da MIRO realizada com sucesso.'
                    'Doc.gerado: ' vg_invoicedocnumber_miro '/' vg_ano_miro
               INTO st_return-message .
        PERFORM z_listar_inconsistencias.

        UPDATE zlest0032 SET belnr       = abap_off
                             gjahr       = abap_off
                             "obj_key_sub = abap_off
                             belnr_est   = vg_invoicedocnumber_miro
                             gjahr_est   = vg_ano_miro
                       WHERE tknum       = st_dados_est-tknum.

        PERFORM z_executar_comit.
      ELSE.
        LOOP AT t_return INTO w_return.
          CONCATENATE 'Erro Estorno MIRO: ' w_return-message
                 INTO st_return-message.
          PERFORM z_listar_inconsistencias.
        ENDLOOP.
        CONTINUE.
      ENDIF.
    ENDIF.

*----------------------------
*-- estornar registro fiscal
*----------------------------
    IF st_dados_est-docnum IS NOT INITIAL.
      CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
        EXPORTING
          doc_number               = st_dados_est-docnum
          ref_type                 = space
          ref_key                  = space
          can_dat                  = sy-datum
        IMPORTING
          doc_number               = vg_docnum_est
        EXCEPTIONS
          document_not_found       = 1
          cancel_not_possible      = 2
          nf_cancel_type_not_found = 3
          database_problem         = 4
          docum_lock               = 5
          nfe_cancel_simulation    = 6
          OTHERS                   = 7.

      IF vg_docnum_est IS NOT INITIAL.
        CONCATENATE 'Documento estorno Registro fiscal gerado: ' vg_docnum_est
               INTO st_return-message SEPARATED BY space.
        PERFORM z_listar_inconsistencias.

        UPDATE zlest0032 SET docnum      = abap_off
                             "obj_key_sub = abap_off
                             docnum_est  = vg_docnum_est
                       WHERE tknum       = st_dados_est-tknum.

        PERFORM z_executar_comit.
      ELSE.
        st_return-message = 'Não foi possível gerar estorno do Registro Fiscal'.
        PERFORM z_listar_inconsistencias.
      ENDIF.
    ENDIF.

*----------------------------
*-- gerar compensação das partidas estornadas
*----------------------------
    IF st_dados_est-belnr       IS NOT INITIAL AND
       st_dados_est-gjahr       IS NOT INITIAL AND
       vg_invoicedocnumber_miro IS NOT INITIAL AND
       vg_ano_miro              IS NOT INITIAL.

      CALL FUNCTION 'Z_FI_GL_COMPENSA_ESTORNO_MIRO'
        EXPORTING
          e_bukrs       = st_dados_est-bukrs
          e_lifnr       = st_dados_est-lifnr
          e_invoice_in  = st_dados_est-belnr
          e_year_in     = st_dados_est-gjahr
          e_invoice_out = vg_invoicedocnumber_miro
          e_year_out    = vg_ano_miro
        IMPORTING
          i_bkpf        = w_bkpf
        EXCEPTIONS
          nao_compensou = 1
          erro_bloqueio = 2
          sem_acesso    = 3
          OTHERS        = 4.

      IF sy-subrc <> 0.
        CONCATENATE 'Erro Compensação Partidas Estornadas: '
                    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               INTO st_return-message.
        PERFORM z_listar_inconsistencias.
      ELSE.
        CONCATENATE 'Compensação Partidas Estornadas: '
                    w_bkpf-belnr ' / ' w_bkpf-gjahr
               INTO st_return-message.
        PERFORM z_listar_inconsistencias.

        UPDATE zlest0032 SET belnr_comp  = w_bkpf-belnr
                             gjahr_comp  = w_bkpf-gjahr
                             "obj_key_sub = abap_off
                       WHERE tknum       = st_dados_est-tknum.

        PERFORM z_executar_comit.
      ENDIF.
    ENDIF.

    ULINE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_LER_DADOS_DIVERSOS
*&---------------------------------------------------------------------*
*  Obter dados de várias tabelas..
*----------------------------------------------------------------------*
FORM z_ler_dados_diversos.

* TYPES: BEGIN OF ty_vttk.
*          INCLUDE STRUCTURE vttk.
*          TYPES:  branch TYPE j_1bbranch-branch,
*        END OF ty_vttk.

  TYPES: BEGIN OF ty_rseg,
           ebeln TYPE rseg-ebeln,
           lfbnr TYPE rseg-lfbnr,
           xblnr TYPE rseg-xblnr,
         END OF ty_rseg.


  DATA: tl_zlest0032      TYPE TABLE OF zlest0032,
        tl_zlest0032_copy TYPE TABLE OF zlest0032,
        sl_zlest0032      TYPE zlest0032,
        tl_aux            TYPE TABLE OF zlest0032,
        tl_ekpo           TYPE TABLE OF ekpo,
        sl_ekpo           TYPE ekpo,
*       tl_vttk        TYPE TABLE OF ty_vttk,
*       sl_vttk        TYPE ty_vttk,
        sl_dados          TYPE y_dados,
        tl_vttp           TYPE TABLE OF y_vttp,
        tl_lips           TYPE TABLE OF y_lips,
        tl_j_1bbranch     TYPE TABLE OF y_j_1bbranch,
        tl_depara_cen     TYPE TABLE OF zsdt_depara_cen,
        sl_vttp           TYPE y_vttp,
        sl_lips           TYPE y_lips,
        sl_j_1bbranch     TYPE y_j_1bbranch,
        sl_j_1bbranch2    TYPE y_j_1bbranch,
        vbranch           TYPE j_1bbranch-branch,
        tl_rseg_busca     TYPE TABLE OF ty_rseg,
        lr_refkey         TYPE RANGE OF j_1bnflin-refkey.

  REFRESH ti_dados.

  IF c_seg IS INITIAL AND c_ped IS INITIAL AND c_sub IS INITIAL AND c_rci IS INITIAL.
    SELECT zlest0032~*
      FROM zlest0032
      INNER JOIN essr ON zlest0032~lblni = essr~lblni
      INTO TABLE @tl_zlest0032
    WHERE zlest0032~tknum IN @s_tknum
      AND zlest0032~fknum IN @s_fknum
      AND zlest0032~ebeln IN @s_ebeln
      AND zlest0032~ebelp IN @s_ebelp
      AND zlest0032~lblni IN @s_lblni
      AND zlest0032~add03 EQ @c_0000000001
      AND zlest0032~belnr EQ @space
      AND essr~loekz NE 'X'.

    " 126779 - ZLES0040  Recuperar Miro - PANF - Inicio

    LOOP AT tl_zlest0032 ASSIGNING FIELD-SYMBOL(<fs_zlest0032>).
      APPEND INITIAL LINE TO tl_rseg_busca ASSIGNING FIELD-SYMBOL(<fs_rseg_b>).
      <fs_rseg_b>-ebeln = <fs_zlest0032>-ebeln.
      <fs_rseg_b>-lfbnr = <fs_zlest0032>-lblni.
      <fs_rseg_b>-xblnr = <fs_zlest0032>-tknum.
    ENDLOOP.

    IF tl_rseg_busca IS NOT	INITIAL.

      SELECT ebeln, lfbnr, xblnr, belnr, gjahr
      FROM rseg
      INTO TABLE @DATA(lt_rseg)
      FOR ALL ENTRIES IN @tl_rseg_busca
      WHERE ebeln = @tl_rseg_busca-ebeln
        AND lfbnr = @tl_rseg_busca-lfbnr
        AND xblnr = @tl_rseg_busca-xblnr.

      IF sy-subrc = 0.

        SORT: lt_rseg BY ebeln lfbnr xblnr.

        SELECT belnr, gjahr
          FROM rbkp
          INTO TABLE @DATA(lt_rbkp)
          FOR ALL ENTRIES IN @lt_rseg
          WHERE belnr = @lt_rseg-belnr
           AND gjahr = @lt_rseg-gjahr
           AND stblg  = @space.

        IF sy-subrc = 0.

          SORT: lt_rbkp BY belnr gjahr.
          lr_refkey = VALUE #( FOR ls_rbklp IN lt_rbkp
                              ( sign = 'I'
                                option = 'EQ'
                                low = ls_rbklp-belnr ) ).

          SELECT a~docnum, a~refkey
            FROM j_1bnflin AS a
            INNER JOIN j_1bnfdoc AS b
              ON a~docnum = b~docnum
            INTO TABLE @DATA(lt_j_1bnflin)
            WHERE a~refkey IN @lr_refkey
              AND b~cancel  = @space.

          IF sy-subrc = 0.

            SORT: lt_j_1bnflin BY refkey.

            tl_zlest0032_copy[] = tl_zlest0032[].

            LOOP AT tl_zlest0032_copy ASSIGNING <fs_zlest0032>.

              LOOP AT lt_rseg ASSIGNING FIELD-SYMBOL(<fs_rseg>)
                                WHERE ebeln = <fs_zlest0032>-ebeln
                                AND   lfbnr = <fs_zlest0032>-lblni
                                AND   xblnr = CONV xblnr1( <fs_zlest0032>-tknum ).

                READ TABLE lt_rbkp ASSIGNING FIELD-SYMBOL(<fs_rbkp>)
                                   WITH KEY  belnr = <fs_rseg>-belnr
                                             gjahr = <fs_rseg>-gjahr
                                             BINARY SEARCH.
                IF sy-subrc = 0.

                  READ TABLE lt_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_1bnflin>)
                                   WITH KEY  refkey = <fs_rbkp>-belnr
                                             BINARY SEARCH.
                  IF sy-subrc = 0.

                    UPDATE zlest0032
                           SET belnr = <fs_rseg>-belnr
                               gjahr = <fs_rseg>-gjahr
                               docnum = <fs_1bnflin>-docnum
                            WHERE tknum = <fs_zlest0032>-tknum
                              AND fknum = <fs_zlest0032>-fknum
                              AND ebeln = <fs_zlest0032>-ebeln
                              AND ebelp = <fs_zlest0032>-ebelp
                              AND lblni = <fs_zlest0032>-lblni.

                    IF sy-subrc = 0.
                      COMMIT WORK AND WAIT.
                    ENDIF.

                    DELETE tl_zlest0032 WHERE tknum = <fs_zlest0032>-tknum
                              AND fknum = <fs_zlest0032>-fknum
                              AND ebeln = <fs_zlest0032>-ebeln
                              AND ebelp = <fs_zlest0032>-ebelp
                              AND lblni = <fs_zlest0032>-lblni.

                  ENDIF.

                ENDIF.

              ENDLOOP.

            ENDLOOP.


          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    " 126779 - ZLES0040  Recuperar Miro - PANF - Fim

    "===============================================================
    IF NOT tl_zlest0032[] IS INITIAL.
      FREE: ti_vbak.
      SELECT tknum vbeln  kunnr fkara bukrs_vf vkorg vtweg spart
      FROM vbak
      INTO TABLE ti_vbak
      FOR ALL ENTRIES IN tl_zlest0032
      WHERE tknum = tl_zlest0032-tknum.

      IF ti_vbak IS NOT INITIAL.
        FREE: ti_vbfa.
        SELECT * FROM vbfa
        INTO CORRESPONDING FIELDS OF TABLE ti_vbfa
        FOR ALL ENTRIES IN ti_vbak
          WHERE vbelv EQ ti_vbak-vbeln AND vbtyp_n EQ 'M'.

        IF sy-subrc EQ 0.
          FREE: r_refkey.
          r_refkey = VALUE #( FOR l IN ti_vbfa ( sign = 'I' option  = 'EQ' low   = l-vbeln ) ).
          FREE: ti_j_1bnflin.
          SELECT * FROM j_1bnflin
          INTO TABLE ti_j_1bnflin
            WHERE refkey  IN r_refkey.

          IF sy-subrc EQ 0.
            FREE: ti_j_1bnfe_active.
            SELECT * FROM j_1bnfe_active
              INTO TABLE ti_j_1bnfe_active
              FOR ALL ENTRIES IN ti_j_1bnflin
              WHERE docnum EQ ti_j_1bnflin-docnum
                AND cancel NE 'X'.
            IF sy-subrc EQ 0.
              SORT ti_j_1bnfe_active DESCENDING BY authdate authtime.

              "Pegar horario de duas anterior a hora atual do sistema.
              CALL FUNCTION 'TSTR_CALC_TIME'
                EXPORTING
*                 IV_CALENDARID            = '99'
                  iv_begin_datelocal_req   = sy-datum
                  iv_begin_timelocal_req   = sy-uzeit
                  iv_duration_integer      = 7200
                  iv_direction             = '-'
                IMPORTING
                  ev_end_datelocal         = l_rudat
                  ev_end_timelocal         = l_rutim
                EXCEPTIONS
                  fatal_error              = 1
                  time_invalid             = 2
                  time_missing             = 3
                  tstream_not_loadable     = 4
                  tstream_generation_error = 5
                  parameter_error          = 6
                  unspecified_error        = 7
                  OTHERS                   = 8.

              "Deletando informações anterior a 2 horas do horario atual do sistema.
              DELETE ti_j_1bnfe_active WHERE authdate EQ sy-datum AND authtime > l_rutim.
              IF ti_j_1bnfe_active IS NOT INITIAL.
                LOOP AT ti_j_1bnfe_active ASSIGNING FIELD-SYMBOL(<w_active>).
                  READ TABLE ti_j_1bnflin INTO DATA(w_j_1bnflin) WITH KEY docnum = <w_active>-docnum.
                  IF sy-subrc EQ 0.
                    CLEAR: zvbelv.
                    zvbelv = CONV #( w_j_1bnflin-refkey ).
                    READ TABLE ti_vbfa INTO DATA(w_vbfa) WITH KEY vbeln = zvbelv.
                    IF sy-subrc EQ 0.
                      READ TABLE ti_vbak INTO DATA(w_vbak) WITH KEY vbeln =  w_vbfa-vbelv.
                      IF sy-subrc EQ 0.
                        APPEND VALUE #( sign    = 'I' option  = 'EQ' low   = w_vbak-tknum ) TO r_tknum.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF r_tknum IS NOT INITIAL.
        SORT r_tknum BY low.
        DELETE tl_zlest0032 WHERE tknum NOT IN r_tknum.
        FREE: r_tknum.
      ENDIF.
    ENDIF.
    "===================================================================

  ELSE.
    SELECT zlest0032~*
      FROM zlest0032
      INNER JOIN essr ON zlest0032~lblni = essr~lblni
      INTO TABLE @tl_zlest0032
    WHERE zlest0032~tknum IN @s_tknum
      AND zlest0032~fknum IN @s_fknum
      AND zlest0032~ebeln IN @s_ebeln
      AND zlest0032~ebelp IN @s_ebelp
      AND zlest0032~lblni IN @s_lblni
      AND zlest0032~add03 EQ @c_0000000001
      AND zlest0032~belnr NE @space
      AND essr~loekz NE 'X'.
  ENDIF.

  IF NOT tl_zlest0032[] IS INITIAL.
    tl_aux[] = tl_zlest0032[].
    SORT tl_aux BY ebeln ASCENDING
                   ebelp ASCENDING.
    DELETE ADJACENT DUPLICATES FROM tl_aux COMPARING ebeln ebelp.
    SELECT *
      FROM ekpo
      INTO TABLE tl_ekpo
      FOR ALL ENTRIES IN tl_aux
    WHERE  ebeln EQ tl_aux-ebeln
     AND   ebelp EQ tl_aux-ebelp.
    SORT tl_ekpo BY ebeln ASCENDING
                    ebelp ASCENDING.
  ENDIF.

  IF NOT tl_zlest0032[] IS INITIAL.
    SELECT tknum vbeln  kunnr fkara bukrs_vf vkorg vtweg spart
      FROM vbak
      INTO TABLE ti_vbak
      FOR ALL ENTRIES IN tl_zlest0032
      WHERE tknum = tl_zlest0032-tknum.

    IF ti_vbak[] IS NOT INITIAL.
      SELECT vbeln matnr werks gsber
        FROM vbap
        INTO TABLE ti_vbap
        FOR ALL ENTRIES IN ti_vbak
        WHERE vbeln = ti_vbak-vbeln.
    ENDIF.

    SELECT *
      FROM vttk
      INTO TABLE tl_vttk
       FOR ALL ENTRIES IN tl_zlest0032
     WHERE tknum EQ tl_zlest0032-tknum.
    SORT tl_vttk BY tknum ASCENDING.

    LOOP AT tl_vttk INTO sl_vttk.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = sl_vttk-tdlnr
        IMPORTING
          output = sl_vttk-branch.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sl_vttk-branch
        IMPORTING
          output = sl_vttk-branch.

      MODIFY tl_vttk FROM sl_vttk INDEX sy-tabix TRANSPORTING branch.
    ENDLOOP.

    SELECT tknum vbeln
       FROM vttp
       INTO TABLE tl_vttp
       FOR ALL ENTRIES IN tl_zlest0032
     WHERE  tknum EQ tl_zlest0032-tknum.
    SORT tl_vttp BY tknum ASCENDING.

    SELECT vbeln werks
      FROM lips
      INTO TABLE tl_lips
      FOR ALL ENTRIES IN tl_vttp
      WHERE vbeln	=	tl_vttp-vbeln.
    SORT tl_lips BY vbeln.

    " Empresa do Centro
    SELECT  branch bukrs
      FROM j_1bbranch
      INTO TABLE tl_j_1bbranch
      FOR ALL ENTRIES IN tl_lips
      WHERE branch = tl_lips-werks.

    SELECT  *
      FROM zsdt_depara_cen
      INTO TABLE tl_depara_cen
      FOR ALL ENTRIES IN tl_lips
      WHERE centrov_1 = tl_lips-werks.

    IF tl_depara_cen[] IS NOT INITIAL.
      SELECT  branch bukrs
        FROM j_1bbranch  APPENDING TABLE tl_j_1bbranch
         FOR ALL ENTRIES IN tl_depara_cen
        WHERE branch = tl_depara_cen-centro_real.
    ENDIF.

    " Empresa do Agente Frete
    SELECT  branch bukrs
      FROM j_1bbranch
      APPENDING TABLE tl_j_1bbranch
      FOR ALL ENTRIES IN tl_vttk
      WHERE branch = tl_vttk-branch.
    SORT tl_j_1bbranch BY branch.


    SELECT * INTO TABLE @DATA(it_essr)
      FROM essr
       FOR ALL ENTRIES IN @tl_zlest0032
     WHERE lblni EQ @tl_zlest0032-lblni.

    SORT it_essr BY lblni.

  ENDIF.

  LOOP AT tl_zlest0032 INTO sl_zlest0032.

    CLEAR: sl_dados,
           sl_vttk,
           sl_ekpo.

    READ TABLE tl_vttk INTO sl_vttk
      WITH KEY tknum = sl_zlest0032-tknum
      BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    READ TABLE it_essr INTO DATA(wa_esst) WITH KEY lblni = sl_zlest0032-lblni.

    CHECK sy-subrc IS INITIAL.

    READ TABLE tl_ekpo INTO sl_ekpo
      WITH KEY ebeln = sl_zlest0032-ebeln
               ebelp = sl_zlest0032-ebelp
      BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    sl_dados-tknum = sl_zlest0032-tknum.
    sl_dados-fknum = sl_zlest0032-fknum.
    sl_dados-ebeln = sl_zlest0032-ebeln.
    sl_dados-ebelp = sl_zlest0032-ebelp.
    sl_dados-lblni = sl_zlest0032-lblni.
    sl_dados-add03 = sl_zlest0032-add03.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = sl_vttk-tplst
      IMPORTING
        centro_out           = sl_dados-tplst
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    sl_dados-vsart = sl_vttk-vsart.
    sl_dados-exti1 = sl_vttk-exti1.
    sl_dados-exti2 = sl_vttk-exti2.
    sl_dados-shtyp = sl_vttk-shtyp.
    sl_dados-tdlnr = sl_vttk-tdlnr.
    sl_dados-budat = wa_esst-budat.
    sl_dados-meins = sl_ekpo-meins.
    sl_dados-tp_emissor = 'T'.
    IF sl_vttk-add03 = '0000000001'. "Frete Próprio

      TRY .
          zcl_faturamento=>zif_faturamento~get_instance(
            )->get_tipo_veiculo(
                 EXPORTING
                   i_placa = sl_vttk-text1(7)
                 IMPORTING
                   e_tipo = DATA(e_tipo)
                   e_proprietario = DATA(e_proprietario)
            ).

          IF e_tipo EQ zif_faturamento=>st_tp_prop_veiculo_terceiro.
            sl_dados-tp_veiculo   = '0'.
            sl_dados-ck_gera_miro = abap_true.
            sl_dados-ck_gera_seguro = abap_true.
          ELSE.

            DATA: e_j_1bbranch  TYPE j_1bbranch.

            DATA(e_agente_frete) =
            CAST zcl_fornecedores(
               zcl_fornecedores=>zif_parceiros~get_instance(
                 )->set_parceiro( i_parceiro = sl_vttk-tdlnr
                 )->ck_parceiro_local_negocio(  IMPORTING e_j_1bbranch = e_j_1bbranch
                 ) )->at_lfa1.

            IF e_proprietario-stcd1 IS NOT INITIAL.

              IF e_agente_frete-stcd1(8) NE e_proprietario-stcd1(8).
                sl_dados-ck_gera_seguro = abap_true.
              ELSE.
                sl_dados-ck_nao_gera_subcont = abap_true.
              ENDIF.

            ENDIF.


            sl_dados-ck_gera_miro = abap_false.
            sl_dados-tp_veiculo   = '1'.

            zcl_fornecedores=>zif_parceiros~get_instance(
               )->set_parceiro( i_parceiro = e_proprietario-lifnr
               )->ck_parceiro_local_negocio(  IMPORTING e_j_1bbranch = DATA(e_j_1bbranch_prop)
               ).

            sl_dados-bukrs  = e_j_1bbranch_prop-bukrs.
            sl_dados-branch = e_j_1bbranch_prop-branch.

            TRY.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
                  EXPORTING
                    i_tknum          = CONV #( sl_vttk-tknum )
                  IMPORTING
                    e_tipo_remetente = sl_dados-tipo_remetente
                    e_conhecimento   = sl_dados-emite_conhecimento ).
              CATCH zcx_faturamento INTO DATA(_zcx_fat).
              CATCH zcx_error       INTO DATA(_zcx_error).
            ENDTRY.

            IF sl_dados-emite_conhecimento EQ abap_true.
              sl_dados-ck_gera_miro = abap_true.
            ENDIF.

            IF sl_dados-tipo_remetente EQ 'I'. "Intercompany
              sl_dados-tp_veiculo   = '2'.
            ENDIF.

          ENDIF.
        CATCH zcx_faturamento.    "
          sl_dados-tp_veiculo = '0'.
        CATCH zcx_error.    "
          sl_dados-tp_veiculo = '0'.
      ENDTRY.

      sl_dados-tp_emissor = 'P'.
      READ TABLE tl_vttp INTO sl_vttp WITH KEY tknum = sl_zlest0032-tknum BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE tl_lips INTO sl_lips WITH KEY vbeln = sl_vttp-vbeln      BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE tl_j_1bbranch INTO sl_j_1bbranch  WITH KEY branch = sl_lips-werks BINARY SEARCH.
          IF sy-subrc NE 0.
            READ TABLE tl_depara_cen INTO DATA(lwa_depara_cen) WITH KEY centrov_1 = sl_lips-werks.
            IF sy-subrc EQ 0.
              READ TABLE tl_j_1bbranch INTO sl_j_1bbranch  WITH KEY branch = lwa_depara_cen-centro_real.
            ENDIF.
          ENDIF.
          IF sy-subrc = 0.
            READ TABLE tl_j_1bbranch INTO sl_j_1bbranch2 WITH KEY branch = sl_vttk-branch BINARY SEARCH.
            IF sy-subrc = 0.
              IF sl_j_1bbranch-bukrs NE sl_j_1bbranch2-bukrs.
                sl_dados-tp_emissor = 'I'. " PROPRIO INTERCOMPANY
                sl_dados-fornecedor = sl_vttk-branch.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = sl_dados-fornecedor
                  IMPORTING
                    output = sl_dados-fornecedor.
              ELSEIF  sl_lips-werks  =   sl_vttk-branch.
                IF sl_vttk-branch NE '0119'.
                  sl_dados-tp_emissor = 'T'.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      sl_dados-fornecedor     = sl_vttk-tdlnr.
      sl_dados-tp_veiculo     = '0'.
      sl_dados-ck_gera_miro   = abap_true.
    ENDIF.

    "ALRS 23.07.15
    READ TABLE tl_vttp INTO sl_vttp WITH KEY tknum = sl_zlest0032-tknum BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE tl_lips INTO sl_lips WITH KEY vbeln = sl_vttp-vbeln      BINARY SEARCH.
      IF sy-subrc = 0.
        sl_dados-werks = sl_lips-werks.

        IF NOT ( '0123456789' CS sl_lips-werks+0(1) ) .
          SELECT SINGLE *
            FROM zsdt_depara_cen
          INTO st_zsdt_depara_cen
          WHERE centrov_1 = sl_lips-werks.

          IF sy-subrc = 0.
            sl_dados-werks = st_zsdt_depara_cen-centro_real.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT ( ( sl_dados-tp_veiculo = '1' ) AND ( sl_dados-emite_conhecimento EQ abap_false ) ).

      DATA: v_candat_null TYPE j_1bnfdoc-candat.

      SELECT SINGLE a~* INTO @DATA(wa_zcte_identifica)
      FROM zcte_identifica AS a
      JOIN j_1bnfdoc AS b
      ON a~docnum = b~docnum
      WHERE a~tknum EQ @sl_dados-tknum
            AND b~candat = @v_candat_null.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      DATA(cl_autorizado) = abap_true.
      TRY .
          zcl_cte=>zif_doc_eletronico~get_instance( i_docnum = wa_zcte_identifica-docnum
            )->set_registro( EXPORTING i_docnum = wa_zcte_identifica-docnum
            )->get_ck_autorizado_uso(
            ).
        CATCH zcx_doc_eletronico.    " .
          cl_autorizado = abap_false.
      ENDTRY.

      IF cl_autorizado EQ abap_false.
        CONTINUE.
      ENDIF.

    ENDIF.

    APPEND sl_dados TO ti_dados.
    CLEAR sl_zlest0032.

  ENDLOOP.

ENDFORM.                    " Z_LER_DADOS_DIVERSOS
*&---------------------------------------------------------------------*
*&      Form  Z_LER_VFKP
*&---------------------------------------------------------------------*
* Ler a tabela VFKP
*----------------------------------------------------------------------*
FORM z_ler_vfkp .

  REFRESH ti_vfkp.
  SELECT fknum fkpos knumv budat
         prsdt bukrs waers netwr werks
    INTO TABLE ti_vfkp
    FROM vfkp
     FOR ALL ENTRIES IN ti_dados
   WHERE fknum = ti_dados-fknum
     AND fkpty = c_z001.

  CHECK sy-subrc EQ 0.

* Acumular os valores obtidos por custo de frete
  REFRESH ti_vfkp_acum.
  LOOP AT ti_vfkp INTO st_vfkp.
    MOVE-CORRESPONDING st_vfkp TO st_vfkp_acum.
    COLLECT st_vfkp_acum INTO ti_vfkp_acum.
  ENDLOOP.


ENDFORM.                    " Z_LER_VFKP
*&---------------------------------------------------------------------*
*&      Form  Z_LER_KONV
*&---------------------------------------------------------------------*
* Ler a tabela de condições.
*----------------------------------------------------------------------*
FORM z_ler_konv .

  REFRESH ti_konv.
  SELECT FROM v_konv FIELDS knumv , kposn , kschl , kwert FOR ALL ENTRIES IN @ti_vfkp WHERE knumv = @ti_vfkp-knumv AND kschl IN ( @c_zfre , @c_zped , @c_zseg , @c_ziof , @c_zicm , @c_zsen , @c_zset , @c_zinn , @c_zins , @c_zred , @c_zirf , @c_zpis ,
@c_zcof
,
@c_ziss
,
@c_zbir
)
INTO
TABLE
@ti_konv .

ENDFORM.                    " Z_LER_KONV
*&---------------------------------------------------------------------*
*&      Form  Z_LER_EKBE
*&---------------------------------------------------------------------*
* Obter dados no histórido dos pedidos de compra.
*----------------------------------------------------------------------*
FORM z_ler_ekbe .

  REFRESH ti_ekbe.
*  SELECT ebeln ebelp lfbnr belnr gjahr menge INTO TABLE ti_ekbe
  SELECT ebeln ebelp lfbnr belnr lfgja menge INTO TABLE ti_ekbe
                           FROM ekbe
                        FOR ALL ENTRIES IN ti_dados
                          WHERE ebeln = ti_dados-ebeln
                            AND ebelp = ti_dados-ebelp
                            AND vgabe = c_9
                            AND lfbnr = ti_dados-lblni.

ENDFORM.                    " Z_LER_EKBE
*&---------------------------------------------------------------------*
*&      Form  Z_LER_ZLEST0021
*&---------------------------------------------------------------------*
* Obter as contas contábeis de débito e crédito do Les
*----------------------------------------------------------------------*
FORM z_ler_zlest0021
    USING
          p_shtyp         TYPE shtyp
          p_dt_referencia TYPE budat
          p_tp_emissor    TYPE ztp_emissor
          p_tp_veiculo    TYPE zde_tp_prop_veiculo.

  CLEAR: ti_zlest0021[], ti_zlest0021.

  TRY .

      zcl_controle_conta_razao=>get_instance(
        )->get_conta_razao(
        EXPORTING
          i_shtyp                  = p_shtyp " Tipo de transporte
          i_tcode                  = CONV #( c_miro )   " Código de transação
          i_fatura                 = c_p    " Emissor da fatura - Fretes
          i_tp_emissor             = p_tp_emissor    " Tipo de emissor
          i_operfrete_range        = VALUE #( sign = 'I' option = 'EQ'
                                               ( low = '1'  high = '1' )  ( low = '2'  high = '2' )
                                               ( low = '3'  high = '3' )  ( low = '4'  high = '4' )
                                               ( low = '5'  high = '5' )  ( low = '6'  high = '6' )
                                               ( low = '9'  high = '9' )  ( low = '10' high = '10' )
                                               ( low = '12' high = '12' ) ( low = '14' high = '14' )
                                               ( low = '23' high = '23' ) ( low = '26' high = '26' )
                                               ( low = '27' high = '27' ) ( low = '28' high = '28' )
                                               ( low = '29' high = '29' ) ( low = '30' high = '30' )
                                               )    " Ranges Operação de lançamento no razão - Frete
          i_tp_veiculo             = VALUE #( sign = 'I' option = 'EQ' ( low = p_tp_veiculo high = p_tp_veiculo ) )    " Tipo de Proprietário de Veículo para Contabilização
          i_dt_referencia          = p_dt_referencia    " Data de lançamento no documento
        IMPORTING
          e_it_zlest0021           = DATA(ti_zlest0021_miro)    " Controle de desterminação conta razão
        ).
    CATCH zcx_controle_conta_razao.    " .
  ENDTRY.

  TRY .

      zcl_controle_conta_razao=>get_instance(
        )->get_conta_razao(
        EXPORTING
          i_shtyp                  = p_shtyp " Tipo de transporte
          i_tcode                  = CONV #( c_f02 )   " Código de transação
          i_fatura                 = c_p    " Emissor da fatura - Fretes
          i_tp_emissor             = p_tp_emissor    " Tipo de emissor
          i_operfrete_range        = VALUE #( sign = 'I' option = 'EQ'
                                               ( low = '1'  high = '1' )  ( low = '2'  high = '2' )
                                               ( low = '3'  high = '3' )  ( low = '4'  high = '4' )
                                               ( low = '5'  high = '5' )  ( low = '6'  high = '6' )
                                               ( low = '9'  high = '9' )  ( low = '10' high = '10' )
                                               ( low = '12' high = '12' ) ( low = '14' high = '14' )
                                               ( low = '23' high = '23' ) ( low = '26' high = '26' )
                                               ( low = '27' high = '27' ) ( low = '28' high = '28' )
                                               ( low = '29' high = '29' ) ( low = '30' high = '30' )
                                               )    " Ranges Operação de lançamento no razão - Frete
          i_tp_veiculo             = VALUE #( sign = 'I' option = 'EQ' ( low = p_tp_veiculo high = p_tp_veiculo ) )    " Tipo de Proprietário de Veículo para Contabilização
          i_dt_referencia          = p_dt_referencia    " Data de lançamento no documento
        IMPORTING
          e_it_zlest0021           = DATA(ti_zlest0021_f02)    " Controle de desterminação conta razão
        ).
    CATCH zcx_controle_conta_razao.    " .
  ENDTRY.

  LOOP AT ti_zlest0021_miro INTO DATA(wa_zlest0021_miro).
    APPEND wa_zlest0021_miro TO ti_zlest0021.
  ENDLOOP.

  LOOP AT ti_zlest0021_f02 INTO wa_zlest0021_miro.
    APPEND wa_zlest0021_miro TO ti_zlest0021.
  ENDLOOP.

*  REFRESH TI_ZLEST0021.
*  SELECT SHTYP    TCODE     FATURA TP_EMISSOR
*         RAZAODEB RAZAOCRED OPERFRETE TP_VEICULO
*         INTO TABLE TI_ZLEST0021
*         FROM ZLEST0021
*      FOR ALL ENTRIES IN TI_DADOS
*        WHERE SHTYP      = TI_DADOS-SHTYP
*          AND TP_EMISSOR = TI_DADOS-TP_EMISSOR
*          AND TCODE     IN (C_MIRO,C_F02)
*          AND FATURA     = C_P
*          AND OPERFRETE IN (C_1,C_2,C_3,C_4,C_5,C_6,C_9,C_10,C_12,C_14,C_23).

  LOOP AT ti_zlest0021 ASSIGNING FIELD-SYMBOL(<fs_frete>).
    IF <fs_frete>-tp_veiculo IS INITIAL.
      <fs_frete>-tp_veiculo = '0'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " Z_LER_ZLEST0021
*&---------------------------------------------------------------------*
*&      Form  Z_LER_VTPA
*&---------------------------------------------------------------------*
*  Obter dados dos parceiros de transporte
*----------------------------------------------------------------------*
FORM z_ler_vtpa .

  REFRESH ti_vtpa.
  SELECT vbeln parvw lifnr INTO TABLE ti_vtpa
               FROM vtpa
             FOR ALL ENTRIES IN ti_dados
              WHERE vbeln = ti_dados-tknum
                AND parvw IN (c_pv,c_pd,c_sg).

  DATA(ti_vtpa_pv) = ti_vtpa[].
  DELETE ti_vtpa_pv WHERE parvw NE c_pv.
  IF ti_vtpa_pv[] IS NOT INITIAL.
    SELECT lifnr, stkzn
      INTO TABLE @ti_lifnr_pv
      FROM lfa1
       FOR ALL ENTRIES IN @ti_vtpa_pv
     WHERE lifnr EQ @ti_vtpa_pv-lifnr.
  ENDIF.

ENDFORM.                    " Z_LER_VTPA
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_TAXAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_obter_taxas.

  SELECT l~lifnr
         l~bukrs
         l~witht
         l~wt_withcd
         t~qproz INTO TABLE ti_taxas
                         FROM lfbw AS l
                    INNER JOIN t059z AS t ON t~witht     = l~witht
                                         AND t~wt_withcd = l~wt_withcd
               FOR ALL ENTRIES IN ti_vtpa
                         WHERE lifnr = ti_vtpa-lifnr.

ENDFORM.                    " Z_OBTER_TAXAS

*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_DADOS_NF
*&---------------------------------------------------------------------*
* Obter os dados da nota fiscal de serviço de transporte a partir do
* documento de transporte.
*----------------------------------------------------------------------*
FORM z_obter_dados_nf.

* Tabelas locais
  DATA: tl_header     TYPE TABLE OF j_1bnfdoc,
        tl_partner    TYPE TABLE OF j_1bnfnad,
        tl_item       TYPE TABLE OF j_1bnflin,
        tl_item_tax   TYPE TABLE OF j_1bnfstx,
        tl_header_msg TYPE TABLE OF j_1bnfftx,
        tl_refer_msg  TYPE TABLE OF j_1bnfref,
        p_autorizado  TYPE char01.


  DATA: wa_j_1bnfdoc     TYPE j_1bnfdoc,
        wa_zlest0038     TYPE zlest0038,
        sl_lifnr_c       TYPE c LENGTH 10,
        sl_series_c      TYPE c LENGTH 3,
        sl_lifnr         TYPE lfa1-lifnr,
        sl_series        TYPE j_1bseries,
        sl_werks         TYPE mseg-werks,
        sl_nftype        TYPE j_1bnftype,
        sl_xblnr1        TYPE xblnr1,
        sl_docdat        TYPE invdt,
        wa_zib_nfe_forn  TYPE zib_nfe_forn,
        wa_zib_nfe_forn2 TYPE zib_nfe_forn,
        "WA_VTTK          TYPE VTTK,
        wa_vtpa          TYPE vtpa,
        vbranch          TYPE zib_nfe_forn-branch.

  DATA: r_vbelv TYPE RANGE OF vbeln_von.


* Obter as notas fiscais a partir dos documentos de transporte
  REFRESH: ti_notas.
  SELECT v~tknum k~vbeln f~vbeln f~posnn j~docnum v~text1 v~shtyp v~tplst INTO TABLE ti_notas
    FROM vttk AS v
   INNER JOIN vbak AS k ON k~tknum = v~tknum
   INNER JOIN vbfa AS f ON f~vbelv = k~vbeln AND f~vbtyp_n = c_m   "Fatura
                                             AND f~vbtyp_v = c_c   "Ordem
   INNER JOIN j_1bnflin AS j ON j~refkey = f~vbeln AND j~refitm = f~posnn
     FOR ALL ENTRIES IN ti_dados
   WHERE v~tknum = ti_dados-tknum.

  IF sy-subrc = 0.
    "============================================================ AONNING.
    SELECT * FROM vbfa INTO TABLE @DATA(t_vbfa) FOR ALL ENTRIES IN @ti_notas WHERE vbelv EQ @ti_notas-vbeln_f.
    IF sy-subrc EQ 0.
      r_vbelv = VALUE #( FOR l IN t_vbfa ( sign = 'I' option  = 'EQ' low   = l-vbelv ) ).
      IF r_vbelv IS NOT INITIAL.
        DELETE ti_notas WHERE vbeln_f IN r_vbelv.
      ENDIF.
    ENDIF.
    FREE: t_vbfa, r_vbelv.

    "===========================================================
  ENDIF.

* Obter os dados das notas fiscais selecionadas
  LOOP AT ti_notas INTO st_notas.

* Inicializar estrutura e tabelas locais.
    CLEAR: st_header, p_autorizado.
    REFRESH: tl_partner,
             tl_item,
             tl_item_tax,
             tl_header_msg,
             tl_refer_msg.

    SELECT SINGLE * FROM j_1bnfdoc INTO wa_j_1bnfdoc WHERE docnum EQ st_notas-docnum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_j_1bnfdoc-branch
      IMPORTING
        output = sl_lifnr_c.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_j_1bnfdoc-series
      IMPORTING
        output = sl_series_c.

    sl_lifnr  = sl_lifnr_c.
    sl_series = sl_series_c.

    SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ sl_lifnr.
    "SELECT SINGLE * FROM VTTK INTO WA_VTTK WHERE TKNUM EQ ST_NOTAS-TKNUM.

    IF ( st_notas-shtyp NE 'Z026' ).

      IF ( st_notas-shtyp EQ 'Z020' ).

        SELECT SINGLE * FROM vtpa INTO wa_vtpa WHERE vbeln EQ st_notas-tknum
                                                 AND parvw EQ 'LR'.

        vbranch = wa_vtpa-kunnr+6(4).

        SELECT SINGLE * FROM vttp  INTO @DATA(wa_vttp) WHERE tknum EQ @st_notas-tknum.

        SELECT SINGLE * FROM vbak INTO @DATA(wl_vbak_serv) WHERE tknum EQ @st_notas-tknum.

        IF sy-subrc EQ 0.
          vbranch = wl_vbak_serv-kunnr+6(4).
        ENDIF.

*        SELECT SINGLE * FROM ZSDT0001 INTO @DATA(WA_ZSDT0001) WHERE VBELN        EQ @WA_VTTP-VBELN
*                                                               AND  TP_MOVIMENTO EQ 'S'.
*        IF SY-SUBRC = 0.
*          SELECT SINGLE * FROM EKPA INTO @DATA(WA_EKPA) WHERE EBELN EQ @WA_ZSDT0001-VBELN
*                                                         AND  PARVW EQ 'ZT'.
*          IF SY-SUBRC = 0.
*            VBRANCH =  ST_NOTAS-TPLST.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE KUNNR
*            FROM VBPA
*              INTO @DATA(VL_KUNNR)
*            WHERE VBELN = @WA_VTTP-VBELN
*              AND PARVW EQ 'WE'.
*          IF SY-SUBRC IS INITIAL .
*            VBRANCH = VL_KUNNR+6(4).
*          ENDIF.
*        ENDIF.

        SELECT SINGLE *  INTO wa_zib_nfe_forn
          FROM zib_nfe_forn
         WHERE nu_chave_cnpj    EQ wa_lfa1-stcd1
           AND nu_chave_numero  EQ wa_j_1bnfdoc-nfenum
           AND nu_chave_serie   EQ sl_series
           AND nu_chave_modelo  EQ wa_j_1bnfdoc-model
           AND dt_emissao       EQ wa_j_1bnfdoc-docdat
           AND branch           EQ vbranch.

        IF ( sy-subrc NE 0 ).

          SELECT SINGLE *  INTO wa_zib_nfe_forn
            FROM zib_nfe_forn
           WHERE nu_chave_cnpj    EQ wa_lfa1-stcd1
             AND nu_chave_numero  EQ wa_j_1bnfdoc-nfenum
             AND nu_chave_serie   EQ sl_series
             AND nu_chave_modelo  EQ wa_j_1bnfdoc-model
             AND dt_emissao       EQ wa_j_1bnfdoc-credat
             AND branch           EQ vbranch.

        ENDIF.


      ELSE.

        SELECT SINGLE *  INTO wa_zib_nfe_forn
          FROM zib_nfe_forn
         WHERE nu_chave_cnpj    EQ wa_lfa1-stcd1
           AND nu_chave_numero  EQ wa_j_1bnfdoc-nfenum
           AND nu_chave_serie   EQ sl_series
           AND nu_chave_modelo  EQ wa_j_1bnfdoc-model
           AND dt_emissao       EQ wa_j_1bnfdoc-docdat
           AND branch           EQ wa_j_1bnfdoc-parid+6(4).

        IF ( sy-subrc NE 0 ).

          SELECT SINGLE *  INTO wa_zib_nfe_forn
            FROM zib_nfe_forn
           WHERE nu_chave_cnpj    EQ wa_lfa1-stcd1
             AND nu_chave_numero  EQ wa_j_1bnfdoc-nfenum
             AND nu_chave_serie   EQ sl_series
             AND nu_chave_modelo  EQ wa_j_1bnfdoc-model
             AND dt_emissao       EQ wa_j_1bnfdoc-credat
             AND branch           EQ wa_j_1bnfdoc-parid+6(4).

        ENDIF.


      ENDIF.

      IF ( sy-subrc <> 0 ).
        MESSAGE e899(fi) WITH 'Arquivo XML não recebido para NF-e/CT-e,'
                              'solicitar para Parceiro enviar para o email'
                              'nfe.fiscal@grupomaggi.com.br (cte.fiscal)' INTO st_return-message.

        READ TABLE ti_dados INTO st_dados WITH KEY tknum = st_notas-tknum.
        PERFORM z_listar_inconsistencias.

        DELETE ti_dados WHERE tknum EQ st_notas-tknum.
        CONTINUE.
      ELSE.
        MOVE-CORRESPONDING wa_zib_nfe_forn  TO wa_zib_nfe_forn2.
        SELECT SINGLE *  INTO wa_zib_nfe_forn
            FROM zib_nfe_forn
           WHERE nu_chave_cnpj    EQ wa_zib_nfe_forn2-nu_chave_cnpj
             AND nu_chave_numero  EQ wa_zib_nfe_forn2-nu_chave_numero
             AND nu_chave_serie   EQ wa_zib_nfe_forn2-nu_chave_serie
             AND nu_chave_modelo  EQ wa_zib_nfe_forn2-nu_chave_modelo
             AND dt_emissao       EQ wa_zib_nfe_forn2-dt_emissao
             AND branch           EQ wa_zib_nfe_forn2-branch
             AND st_nota          IN ('2','3').
        IF ( sy-subrc = 0 ).
          MESSAGE e899(fi) WITH 'Arquivo XML recebido para NF-e/CT-e,' 'está cancelado.' INTO st_return-message.
          READ TABLE ti_dados INTO st_dados WITH KEY tknum = st_notas-tknum.
          PERFORM z_listar_inconsistencias.

          DELETE ti_dados WHERE tknum EQ st_notas-tknum.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.


* Obter os dados da nota fiscal de produto
    CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
      EXPORTING
        doc_number         = st_notas-docnum
      IMPORTING
        doc_header         = st_header
      TABLES
        doc_partner        = tl_partner
        doc_item           = tl_item
        doc_item_tax       = tl_item_tax
        doc_header_msg     = tl_header_msg
        doc_refer_msg      = tl_refer_msg
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CHECK sy-subrc EQ 0.

*-CS2021000827 - 31.08.2021 - JT - inicio
*--------------------------------------------
*-- de para imposto para frete proprio
*--------------------------------------------
    LOOP AT tl_item_tax INTO DATA(w_item_tax).
      IF     w_item_tax-taxtyp = 'ICON'.
        w_item_tax-taxtyp = 'ICOF'.
      ELSEIF w_item_tax-taxtyp = 'IPSN'.
        w_item_tax-taxtyp = 'IPIS'.
      ENDIF.
      MODIFY tl_item_tax FROM w_item_tax INDEX sy-tabix.
    ENDLOOP.
*-CS2021000827 - 31.08.2021 - JT - fim

    CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
      EXPORTING
        p_docnum      = st_notas-docnum
      IMPORTING
        p_autorizado  = p_autorizado
      EXCEPTIONS
        cancelado     = 1
        nao_cancelado = 2
        pendente      = 3
        nao_concluido = 4
        nao_existe    = 5
        OTHERS        = 6.

    IF sy-subrc <> 0.
      "Excluir vinculo
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO st_return-message.
      READ TABLE ti_dados INTO st_dados WITH KEY tknum = st_notas-tknum.
      PERFORM z_listar_inconsistencias.

      DELETE ti_dados WHERE tknum EQ st_notas-tknum.
    ELSE.
      "Salvar os dados das tabelas temporárias
      APPEND st_header              TO ti_header.
      APPEND LINES OF tl_partner    TO ti_partner.
      APPEND LINES OF tl_item       TO ti_item.
      APPEND LINES OF tl_item_tax   TO ti_item_tax.
      APPEND LINES OF tl_header_msg TO ti_header_msg.
      APPEND LINES OF tl_refer_msg  TO ti_refer_msg.
      IF NOT st_header-nfe IS INITIAL.
        SELECT * APPENDING TABLE ti_header_nfe
          FROM j_1bnfe_active
         WHERE docnum EQ st_header-docnum.
      ENDIF.
    ENDIF.

  ENDLOOP.

* Classificar as tabelas internas de notas fiscais pelo número do
* documento.
  SORT: ti_header     BY docnum,
        ti_header_nfe BY docnum,
        ti_item       BY docnum.

ENDFORM.                    " Z_OBTER_DADOS_NF
*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_DAOS
*&---------------------------------------------------------------------*
* Processar os dados encontrados, alimentando a estrutura e tabelas da
* bapi e a tabela local.
*----------------------------------------------------------------------*
FORM z_processar_dados.

* Campos auxiliares locais
  DATA: vl_fknum  TYPE vfkp-fknum,
        vl_number TYPE char10,
        vl_total  TYPE konv-kwert,
        vl_tabix  TYPE sy-tabix,
        vg_msg    TYPE string,
        vbranch   TYPE j_1bbranch-branch,
        vbukrs    TYPE vfkp-bukrs.

* Classificar as tabelas internas
  SORT ti_dados       BY fknum.       "Nr.custo frete
  SORT ti_notas       BY tknum.       "Docto de transporte e nota fiscal.
  SORT ti_vfkp        BY fknum fkpos. "Nr.custo frete e item
  SORT ti_vfkp_acum   BY fknum.       "Nr.custo frete
  SORT ti_vtpa        BY vbeln parvw. "Nr.docto. transporte
  SORT ti_lifnr_pv    BY lifnr.
  SORT ti_konv        BY knumv kschl. "Nr. condição e condição
  SORT ti_ekbe        BY ebeln ebelp lfbnr. "Ped.compra, item e docto. ref.
  SORT ti_taxas       BY lifnr bukrs. "Nr. do fornecedor e empresa
  SORT ti_header      BY docnum.      "Nr. do documento
  SORT ti_header_nfe  BY docnum.      "Active NFE
  SORT ti_vbak        BY tknum.
  SORT ti_vbap        BY vbeln.

  LOOP AT ti_dados INTO st_dados.

** Não processar documentos de transporte sem a nota fiscal.
*    READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados-tknum
*                                      BINARY SEARCH.
*    CHECK sy-subrc eq 0.

    MOVE-CORRESPONDING st_dados TO st_dados_aux.

    PERFORM z_ler_zlest0021 USING st_dados-shtyp st_dados-budat st_dados-tp_emissor st_dados-tp_veiculo.
    SORT ti_zlest0021   BY shtyp tcode tp_emissor operfrete. "Tp.transp, transação e oper.lancto.

    AT NEW fknum.
      PERFORM z_inicializar_areas.
      READ TABLE ti_vfkp INTO st_vfkp WITH KEY fknum = st_dados_aux-fknum BINARY SEARCH.
      CHECK sy-subrc EQ 0.
      PERFORM z_montar_cabecalho_bapi.
    ENDAT.

* Posicionar o ponteiro de leitura
    READ TABLE ti_vfkp INTO st_vfkp WITH KEY fknum = st_dados_aux-fknum
                                    BINARY SEARCH.
    vl_tabix = sy-tabix.
    vl_fknum = st_vfkp-fknum.
* Ler todos os item do custo de frete
    DO.
      CLEAR: lc_vlr_liq_ped, lc_vlr_pis_ped, lc_vlr_cofins_ped.   "*-CS2022000741-04.10.1024-#83334-JT-inicio

      READ TABLE ti_vfkp INTO st_vfkp INDEX vl_tabix.
      IF sy-subrc <> 0 OR
         st_vfkp-fknum <> vl_fknum.
        EXIT.
      ELSE.
        ADD 1 TO vl_tabix.
      ENDIF.
      PERFORM z_montar_item_bapi.
      PERFORM z_montar_conta_frete.
      PERFORM z_montar_conta_icms.

      PERFORM z_montar_conta_pis             CHANGING lc_vlr_pedagio lc_vlr_pis_ped lc_vlr_cofins_ped lc_vlr_liq_ped. "*-CS2022000741-04.10.1024-#83334-JT-inicio
      PERFORM z_montar_conta_cofins          CHANGING lc_vlr_pedagio lc_vlr_pis_ped lc_vlr_cofins_ped lc_vlr_liq_ped. "*-CS2022000741-04.10.1024-#83334-JT-inicio
      PERFORM z_montar_conta_pis_nao_acu     CHANGING lc_vlr_pedagio lc_vlr_pis_ped lc_vlr_cofins_ped lc_vlr_liq_ped. "*-CS2022000741-04.10.1024-#83334-JT-inicio
      PERFORM z_montar_conta_cofins_nao_acu  CHANGING lc_vlr_pedagio lc_vlr_pis_ped lc_vlr_cofins_ped lc_vlr_liq_ped. "*-CS2022000741-04.10.1024-#83334-JT-inicio

      PERFORM z_montar_conta_inss_patronal.
      PERFORM z_montar_taxas TABLES ti_withtaxdata
                              USING st_vfkp
                                    '1' "Checagem de Impostos da Empresa da VI
                           CHANGING gva_msg_error.

      IF gva_msg_error IS NOT INITIAL.
        st_return-message = gva_msg_error.
        PERFORM z_listar_inconsistencias.
        CONTINUE.
      ENDIF.

*-CS2020000576 - 01.09.2021 - JT - inicio
      PERFORM z_expande_fornecedor_pv.
*-CS2020000576 - 01.09.2021 - JT - fim

* Desvio para rotina de execução da bapi.
      IF NOT c_seg IS INITIAL
      OR NOT c_ped IS INITIAL
      OR NOT c_sub IS INITIAL
      OR NOT c_rci IS INITIAL.

        SELECT SINGLE belnr gjahr docnum obj_key_ped  obj_key_seg obj_key_sub obj_key_rec awkey_sub awkey_rec
          INTO (vg_invoicedocnumber_miro, vg_ano_miro, vg_docnum, vobj_keypd, vobj_keysg, vobj_keysu, vobj_keyre, v_awkey_sub, v_awkey_rec)
          FROM zlest0032
          INNER JOIN essr ON zlest0032~lblni EQ essr~lblni
         WHERE tknum = st_dados_aux-tknum
          AND essr~loekz NE 'X'.


        IF NOT sy-subrc IS INITIAL.
          CONCATENATE 'Não encontrado Miro e Doc. Fiscal Entrada do Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
          MESSAGE vg_msg TYPE 'W'.
          CONTINUE.
        ENDIF.

        " Checar e apagar SEGURO com erro na ZIB
        IF NOT c_seg IS INITIAL AND vobj_keysg IS NOT INITIAL.
          SELECT * INTO TABLE it_zib_contabil
            FROM zib_contabil
           WHERE obj_key = vobj_keysg.

          IF sy-subrc IS INITIAL.
            READ TABLE it_zib_contabil INDEX 1.
            SELECT SINGLE *
              FROM zib_contabil_err
              INTO wa_zib_contabil_err
              WHERE obj_key = it_zib_contabil-obj_key.
            IF sy-subrc EQ 0. " SE HOUVER ERRO APAGA
              DELETE FROM zib_contabil      WHERE obj_key = it_zib_contabil-obj_key.
              DELETE FROM zib_contabil_err  WHERE obj_key = it_zib_contabil-obj_key.
              UPDATE zlest0032 SET obj_key_seg = ''
              WHERE tknum = st_dados_aux-tknum.
            ELSE. " SENÃO JÁ PROCESSADO  COM SUCESSO
              CONCATENATE 'Já criado ZIB_CONTABIL SEGURO para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
              MESSAGE vg_msg TYPE 'I'.
            ENDIF.
          ENDIF.
        ENDIF.

        " Checar e apagar PEDAGIO com erro na ZIB
        IF NOT c_ped IS INITIAL AND vobj_keypd IS NOT INITIAL.
          SELECT * INTO TABLE it_zib_contabil
            FROM zib_contabil
           WHERE obj_key = vobj_keypd.

          IF sy-subrc IS INITIAL.
            READ TABLE it_zib_contabil INDEX 1.
            SELECT SINGLE *
              FROM zib_contabil_err
              INTO wa_zib_contabil_err
              WHERE obj_key = it_zib_contabil-obj_key.
            IF sy-subrc EQ 0. " SE HOUVER ERRO APAGA
              DELETE FROM zib_contabil      WHERE obj_key = it_zib_contabil-obj_key.
              DELETE FROM zib_contabil_err  WHERE obj_key = it_zib_contabil-obj_key.
              UPDATE zlest0032 SET obj_key_ped = ''
              WHERE tknum = st_dados_aux-tknum.
            ELSE. " SENÃO JÁ PROCESSADO  COM SUCESSO
              CONCATENATE 'Já criado ZIB_CONTABIL PEDÁGIO para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
              MESSAGE vg_msg TYPE 'I'.
            ENDIF.
          ENDIF.
        ENDIF.

        " Checar se SUBCONTRATADO
        "ZLES0040  - Subcontratado - BG #134203 iNICIO PT03

*        IF NOT c_sub IS INITIAL.
*          IF  NOT vobj_keysu IS INITIAL.  " SENÃO JÁ PROCESSADO  COM SUCESSO
*            CONCATENATE 'Já criado SUBCONTRATADO para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
*            MESSAGE vg_msg TYPE 'I'.
*          ENDIF.
*        ENDIF.

        IF vobj_keysu IS  INITIAL AND  v_awkey_sub IS NOT INITIAL.
          SELECT SINGLE stblg belnr FROM bkpf INTO (v_stblg, v_belnr)
            WHERE "GJAHR = VG_ANO_MIRO AND ALINHADO QUE NÃO PRECISA DO ANO
                  blart  = 'FR' AND
                  bktxt  = 'SUBCONTRATADO' AND
                  awkey = v_awkey_sub.

          IF v_stblg IS NOT INITIAL.
            UPDATE zlest0032 SET awkey_sub = ''
                      WHERE tknum = st_dados_aux-tknum.

            PERFORM z_montar_exec_bapi  CHANGING vobj_key.

            UPDATE zlest0032 SET: obj_key_sub = vobj_key
                            WHERE tknum = st_dados_aux-tknum.
          ELSE.
            UPDATE zlest0032 SET:     obj_key_sub = v_belnr
                              WHERE tknum = st_dados_aux-tknum.
            CONCATENATE 'Já criado SUBCONTRATADO para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
            MESSAGE vg_msg TYPE 'I'.
          ENDIF.

        ENDIF.

        IF vobj_keysu IS NOT INITIAL .
          CLEAR: v_stblg, v_belnr.
          SELECT SINGLE stblg  FROM bkpf INTO (v_stblg)
                WHERE belnr = vobj_keysu AND
                       gjahr = vg_ano_miro AND
                       blart  = 'FR' AND
                      bktxt  = 'SUBCONTRATADO'.

          IF v_stblg IS NOT INITIAL.
            UPDATE zlest0032 SET obj_key_sub = ''
                                 awkey_sub   = ''
                          WHERE tknum = st_dados_aux-tknum.

            PERFORM z_montar_exec_bapi  CHANGING vobj_key.
            UPDATE zlest0032 SET: obj_key_sub = vobj_key
                            WHERE tknum = st_dados_aux-tknum.
          ELSE.
            CONCATENATE 'Já criado SUBCONTRATADO para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
            MESSAGE vg_msg TYPE 'I'.
          ENDIF.
        ENDIF.
        "ZLES0040  - Subcontratado - BG #134203 FIM PT3

        IF NOT c_rci IS INITIAL.
          "ZLES0040  - Subcontratado - BG #134203 INICIO PT4
*          IF NOT vobj_keyre IS INITIAL.
*            CONCATENATE 'Já criado RECEITA para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
*            MESSAGE vg_msg TYPE 'I'.
*          ENDIF.

          IF vobj_keyre IS INITIAL AND v_awkey_rec IS NOT INITIAL.
            SELECT SINGLE  belnr  FROM bkpf INTO (v_belnr)
                 WHERE gjahr = vg_ano_miro AND
                        blart  = 'NQ' AND
                       bktxt  = 'RECEITA INTERCOMPANY' AND
                        awkey = v_awkey_rec AND
                        stblg EQ ' ' .

            IF sy-subrc IS NOT INITIAL ."v_STBLG IS NOT INITIAL.
              UPDATE zlest0032 SET awkey_rec = ''
                           WHERE tknum = st_dados_aux-tknum.

              PERFORM z_montar_exec_bapi_receita  CHANGING vobj_key.
              UPDATE zlest0032 SET: obj_key_rec = vobj_key
                              WHERE tknum = st_dados_aux-tknum.
            ELSE.
              UPDATE zlest0032 SET: obj_key_rec = v_belnr
                                WHERE tknum = st_dados_aux-tknum.
              CONCATENATE 'Já criado RECEITA para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
              MESSAGE vg_msg TYPE 'I'.
            ENDIF.
          ENDIF.
          IF  vobj_keyre IS NOT INITIAL.
            CLEAR: v_stblg, v_belnr.
            SELECT SINGLE stblg  FROM bkpf INTO (v_stblg)
                  WHERE belnr = vobj_keyre AND
                         gjahr = vg_ano_miro AND
                         blart  = 'NQ' AND
                        bktxt  = 'RECEITA INTERCOMPANY'.
            IF v_stblg IS NOT INITIAL.
              UPDATE zlest0032 SET obj_key_rec = ''
                                   awkey_rec   = ''
                            WHERE tknum = st_dados_aux-tknum.

              PERFORM z_montar_exec_bapi_receita  CHANGING vobj_key.
              UPDATE zlest0032 SET: obj_key_rec = vobj_key
                              WHERE tknum = st_dados_aux-tknum.
            ELSE.
              CONCATENATE 'Já criado RECEITA para este Nr. Transporte' st_dados_aux-tknum INTO vg_msg SEPARATED BY space.
              MESSAGE vg_msg TYPE 'I'.
            ENDIF.
          ENDIF.
          "ZLES0040  - Subcontratado - BG #134203 FIM PT4
        ENDIF.

      ELSE.

        IF ( st_dados_aux-shtyp EQ 'Z026' ).
          PERFORM: gerar_miro_z026.
        ELSE.

          CLEAR: vg_invoicedocnumber_miro, vg_ano_miro, ti_return, vg_docnum.

          CASE st_dados-ck_gera_miro.
            WHEN abap_true.
              PERFORM z_executar_bapi.
            WHEN abap_false.
              PERFORM z_montar_bapi_nf USING st_headerdata-del_costs_taxc.
              IF vg_docnum IS NOT INITIAL.
                PERFORM z_atualizar_zlest0032.
                PERFORM z_executar_comit.
              ENDIF.
          ENDCASE.
        ENDIF.

      ENDIF.

* Se a notafiscal não foi criada, significa que um erro ocorreu, que o
* rollback work foi executado, logo a tabela zib_contabil não deve ser
* atualizada.
      "CHECK VG_DOCNUM IS NOT INITIAL.
      IF ( NOT vg_docnum IS INITIAL ) OR ( NOT vg_invoicedocnumber_miro IS INITIAL OR st_dados-ck_gera_miro EQ abap_false ) .
* Geração Transação F-02 (Adiantamento dos Postos)
*------------------------------------------------------------------------
* SEGURO
*------------------------------------------------------------------------
        CLEAR: it_zib_contabil[].

        CLEAR: vobj_keypd, vobj_keysg, vobj_keysu.

        SELECT SINGLE  obj_key_ped  obj_key_seg obj_key_sub obj_key_rec awkey_sub awkey_rec
          INTO (vobj_keypd, vobj_keysg, vobj_keysu, vobj_keyre, v_awkey_sub, v_awkey_rec )
          FROM zlest0032
          INNER JOIN essr ON zlest0032~lblni EQ essr~lblni
         WHERE tknum = st_dados_aux-tknum
          AND essr~loekz NE 'X'.

        IF ( vobj_keysg IS INITIAL ) AND ( st_dados_aux-ck_gera_seguro EQ abap_true ).
          CLEAR: vl_number.
          PERFORM z_obter_proximo_numero CHANGING vl_number.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = st_dados_aux-tdlnr
            IMPORTING
              output = vbranch.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vbranch
            IMPORTING
              output = vbranch.

          SELECT SINGLE bukrs
            FROM j_1bbranch INTO vbukrs
            WHERE  branch = vbranch.

* 1a. partida - Débito  Fornecedor
          PERFORM z_montar_registro USING c_4
                                          vl_number
                                          c_000001
                                          c_21
                                          st_dados_aux-tplst
                                          st_dados_aux-tdlnr
                                          vbukrs "ST_VFKP-BUKRS
                                          st_vfkp-prsdt
                                          st_vfkp-budat
                                          st_dados_aux-shtyp
                                          st_vfkp-knumv
                                          vl_total
                                          st_dados_aux-exti1
                                          st_dados_aux-exti2
                                          st_dados_aux-tknum
                                          c_pv
                                          c_23
                                          c_seguro
                                          vg_invoicedocnumber_miro
                                          vg_ano_miro
                                          st_dados_aux-tp_emissor
                                          st_dados_aux-tp_veiculo.

* 2a. partida - Crédito de conta razão
          PERFORM z_montar_registro USING c_3
                                          vl_number
                                          c_000002
                                          c_50
                                          st_dados_aux-tplst
                                          st_dados_aux-tdlnr
                                          vbukrs "ST_VFKP-BUKRS
                                          st_vfkp-prsdt
                                          st_vfkp-budat
                                          st_dados_aux-shtyp
                                          st_vfkp-knumv
                                          vl_total
                                          st_dados_aux-exti1
                                          st_dados_aux-exti2
                                          st_dados_aux-tknum
                                          ' '
                                          c_23
                                          c_seguro
                                          vg_invoicedocnumber_miro
                                          vg_ano_miro
                                          st_dados_aux-tp_emissor
                                          st_dados_aux-tp_veiculo.

          READ TABLE it_zib_contabil  INTO st_zib_contabil WITH KEY bktxt         = c_seguro_frete.
          IF sy-subrc = 0.
            CONCATENATE c_les vl_number sy-datum(4) INTO vobj_key.
            vobj_keysg  = vobj_key.
          ENDIF.
        ENDIF.


*------------------------------------------------------------------------
* PEDÁGIO
*------------------------------------------------------------------------
        IF vobj_keypd IS INITIAL.

*-CS2022000741-04.10.1024-#83334-JT-inicio
          READ TABLE ti_vtpa INTO DATA(w_vtpa) WITH KEY vbeln = st_dados_aux-tknum  "BUG 157525-07.11.2024-JT
                                                        parvw = 'PV'.
          IF sy-subrc = 0.
            SELECT SINGLE ktokk
              INTO @DATA(_ktokk)
              FROM lfa1
             WHERE lifnr = @w_vtpa-lifnr
               AND ktokk = 'ZFIC'.
          ENDIF.

          IF sy-subrc = 0.
            DELETE it_zib_contabil WHERE bktxt = c_pedagio_frete.
          ELSE.
*-CS2022000741-04.10.1024-#83334-JT-fim

* 1a. partida - Débito
            CLEAR: vl_number.
            PERFORM z_obter_proximo_numero CHANGING vl_number.

            SELECT SINGLE bukrs
              FROM j_1bbranch INTO vbukrs
              WHERE  branch = st_dados_aux-werks.

*-CS2022000741-04.10.1024-#83334-JT-inicio
            IF lc_vlr_pis_ped    IS INITIAL  AND
               lc_vlr_cofins_ped IS INITIAL.

* 1a. partida - CRedito
              PERFORM z_montar_registro USING c_2
                                              vl_number
                                              c_000001
                                              c_31
                                              st_dados_aux-tplst
                                              st_dados_aux-werks
                                              vbukrs "ST_VFKP-BUKRS
                                              st_vfkp-prsdt
                                              st_vfkp-budat
                                              st_dados_aux-shtyp
                                              st_vfkp-knumv
                                              vl_total
                                              st_dados_aux-exti1
                                              st_dados_aux-exti2
                                              st_dados_aux-tknum
                                              c_pd
                                              ' '
                                              c_pedagio
                                              vg_invoicedocnumber_miro
                                              vg_ano_miro
                                              st_dados_aux-tp_emissor
                                              st_dados_aux-tp_veiculo.

* 2a. partida - Debito
              PERFORM z_montar_registro USING c_3
                                              vl_number
                                              c_000002
                                              c_40
                                              st_dados_aux-tplst
                                              st_dados_aux-werks
                                              vbukrs "ST_VFKP-BUKRS
                                              st_vfkp-prsdt
                                              st_vfkp-budat
                                              st_dados_aux-shtyp
                                              st_vfkp-knumv
                                              vl_total
                                              st_dados_aux-exti1
                                              st_dados_aux-exti2
                                              st_dados_aux-tknum
                                              ' '
                                              c_14
                                              c_pedagio
                                              vg_invoicedocnumber_miro
                                              vg_ano_miro
                                              st_dados_aux-tp_emissor
                                              st_dados_aux-tp_veiculo.
            ELSE.
              lc_vlr_liq_ped = lc_vlr_pedagio - lc_vlr_liq_ped.

* 1a. partida - CRedito
              PERFORM z_montar_registro USING c_2
                                              vl_number
                                              c_000001
                                              c_31
                                              st_dados_aux-tplst
                                              st_dados_aux-werks
                                              vbukrs "ST_VFKP-BUKRS
                                              st_vfkp-prsdt
                                              st_vfkp-budat
                                              st_dados_aux-shtyp
                                              st_vfkp-knumv
                                              vl_total
                                              st_dados_aux-exti1
                                              st_dados_aux-exti2
                                              st_dados_aux-tknum
                                              c_pd
                                              ' '
                                              c_pedagio
                                              vg_invoicedocnumber_miro
                                              vg_ano_miro
                                              st_dados_aux-tp_emissor
                                              st_dados_aux-tp_veiculo.

* 2a. partida - Débito
              PERFORM z_montar_registro USING c_3
                                              vl_number
                                              c_000002
                                              c_40
                                              st_dados_aux-tplst
                                              st_dados_aux-werks
                                              vbukrs "ST_VFKP-BUKRS
                                              st_vfkp-prsdt
                                              st_vfkp-budat
                                              st_dados_aux-shtyp
                                              st_vfkp-knumv
                                              lc_vlr_liq_ped
                                              st_dados_aux-exti1
                                              st_dados_aux-exti2
                                              st_dados_aux-tknum
                                              ' '
                                              c_14
                                              c_pedagio
                                              vg_invoicedocnumber_miro
                                              vg_ano_miro
                                              st_dados_aux-tp_emissor
                                              st_dados_aux-tp_veiculo.

* 3a. partida - Dédito
              PERFORM z_montar_registro USING c_3
                                              vl_number
                                              c_000003
                                              c_40
                                              st_dados_aux-tplst
                                              st_dados_aux-werks
                                              vbukrs "ST_VFKP-BUKRS
                                              st_vfkp-prsdt
                                              st_vfkp-budat
                                              st_dados_aux-shtyp
                                              st_vfkp-knumv
                                              lc_vlr_pis_ped
                                              st_dados_aux-exti1
                                              st_dados_aux-exti2
                                              st_dados_aux-tknum
                                              ' '
                                              c_9
                                              c_pedagio
                                              vg_invoicedocnumber_miro
                                              vg_ano_miro
                                              st_dados_aux-tp_emissor
                                              st_dados_aux-tp_veiculo.

* 4a. partida - Crébito
              PERFORM z_montar_registro USING c_3
                                              vl_number
                                              c_000004
                                              c_40
                                              st_dados_aux-tplst
                                              st_dados_aux-werks
                                              vbukrs "ST_VFKP-BUKRS
                                              st_vfkp-prsdt
                                              st_vfkp-budat
                                              st_dados_aux-shtyp
                                              st_vfkp-knumv
                                              lc_vlr_cofins_ped
                                              st_dados_aux-exti1
                                              st_dados_aux-exti2
                                              st_dados_aux-tknum
                                              ' '
                                              c_1
                                              c_pedagio
                                              vg_invoicedocnumber_miro
                                              vg_ano_miro
                                              st_dados_aux-tp_emissor
                                              st_dados_aux-tp_veiculo.

            ENDIF.
          ENDIF.
*-CS2022000741-04.10.1024-#83334-JT-fim

          READ TABLE it_zib_contabil  INTO st_zib_contabil WITH KEY bktxt         = c_pedagio_frete.
          IF sy-subrc = 0.
            CONCATENATE c_les
                        vl_number
                        sy-datum(4) INTO vobj_key.
            vobj_keypd  = vobj_key.
*            UPDATE ZLEST0032 SET: OBJ_KEY_PED = VOBJ_KEY
*                    WHERE TKNUM = ST_DADOS_AUX-TKNUM.
          ENDIF.
        ENDIF.

*------------------------------------------------------------------------
* SUBCONTRATADO
*------------------------------------------------------------------------

        "Quanto o Agente de Frete for intercompany, e o Proprietario do Veiculo não for da Empresa do Faturamento, gerar subcontratação...
        " e o veiculo não for de propriedade do Emissor do frete
        IF ( st_dados_aux-tp_emissor = 'I' AND st_dados_aux-tp_veiculo NE '1' ) AND ( st_dados_aux-ck_nao_gera_subcont EQ abap_false ).

          IF vobj_keysu IS INITIAL AND v_awkey_sub IS INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = st_dados_aux-tdlnr
              IMPORTING
                output = vbranch.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vbranch
              IMPORTING
                output = vbranch.

            SELECT SINGLE bukrs
              FROM j_1bbranch INTO vbukrs
              WHERE  branch = vbranch.

            CLEAR: vl_number.
            PERFORM z_obter_proximo_numero CHANGING vl_number.
            PERFORM z_montar_registro USING c_2
                                          vl_number
                                          c_000001
                                          c_31
                                          st_dados_aux-tplst
                                          st_dados_aux-tdlnr
                                          vbukrs "ST_VFKP-BUKRS
                                          st_vfkp-prsdt
                                          st_vfkp-budat
                                          st_dados_aux-shtyp
                                          st_vfkp-knumv
                                          vl_total
                                          st_dados_aux-exti1
                                          st_dados_aux-exti2
                                          st_dados_aux-tknum
                                          c_pv
                                          c_12
                                          c_subcontra_total
                                          vg_invoicedocnumber_miro
                                          vg_ano_miro
                                          st_dados_aux-tp_emissor
                                          st_dados_aux-tp_veiculo.

            "VLR_TOTAL - PIS - COFINS
            PERFORM z_montar_registro USING c_1
                                          vl_number
                                          c_000002
                                          c_40
                                          st_dados_aux-tplst
                                          st_dados_aux-tdlnr
                                          vbukrs "ST_VFKP-BUKRS
                                          st_vfkp-prsdt
                                          st_vfkp-budat
                                          st_dados_aux-shtyp
                                          st_vfkp-knumv
                                          vl_total
                                          st_dados_aux-exti1
                                          st_dados_aux-exti2
                                          st_dados_aux-tknum
                                          ' '
                                          c_12
                                          c_subcontra_liqui
                                          vg_invoicedocnumber_miro
                                          vg_ano_miro
                                          st_dados_aux-tp_emissor
                                          st_dados_aux-tp_veiculo.

            "VLR_TOTAL = PIS
            PERFORM z_montar_registro USING c_1
                                          vl_number
                                          c_000003
                                          c_40
                                          st_dados_aux-tplst
                                          st_dados_aux-tdlnr
                                          vbukrs "ST_VFKP-BUKRS
                                          st_vfkp-prsdt
                                          st_vfkp-budat
                                          st_dados_aux-shtyp
                                          st_vfkp-knumv
                                          vl_total
                                          st_dados_aux-exti1
                                          st_dados_aux-exti2
                                          st_dados_aux-tknum
                                          ' '
                                          c_29
                                          c_subcontra_pis
                                          vg_invoicedocnumber_miro
                                          vg_ano_miro
                                          st_dados_aux-tp_emissor
                                          st_dados_aux-tp_veiculo.

            "VLR_TOTAL = COFINS
            PERFORM z_montar_registro USING c_1
                                          vl_number
                                          c_000004
                                          c_40
                                          st_dados_aux-tplst
                                          st_dados_aux-tdlnr
                                          vbukrs "ST_VFKP-BUKRS
                                          st_vfkp-prsdt
                                          st_vfkp-budat
                                          st_dados_aux-shtyp
                                          st_vfkp-knumv
                                          vl_total
                                          st_dados_aux-exti1
                                          st_dados_aux-exti2
                                          st_dados_aux-tknum
                                          ' '
                                          c_30
                                          c_subcontra_cofin
                                          vg_invoicedocnumber_miro
                                          vg_ano_miro
                                          st_dados_aux-tp_emissor
                                          st_dados_aux-tp_veiculo.


            "INSS PATRONAL DEBITO

            PERFORM z_montar_registro USING c_1
                                            vl_number
                                            c_000005
                                            c_40
                                            st_dados_aux-tplst
                                            st_dados_aux-tdlnr
                                            vbukrs "ST_VFKP-BUKRS
                                            st_vfkp-prsdt
                                            st_vfkp-budat
                                            st_dados_aux-shtyp
                                            st_vfkp-knumv
                                            vl_total
                                            st_dados_aux-exti1
                                            st_dados_aux-exti2
                                            st_dados_aux-tknum
                                            ' '
                                            c_6
                                            c_subcontra_patronal
                                            vg_invoicedocnumber_miro
                                            vg_ano_miro
                                            st_dados_aux-tp_emissor
                                            st_dados_aux-tp_veiculo.


            "INSS PATRONAL CREDITO
            PERFORM z_montar_registro USING c_2
                                            vl_number
                                            c_000006
                                            c_50
                                            st_dados_aux-tplst
                                            st_dados_aux-tdlnr
                                            vbukrs "ST_VFKP-BUKRS
                                            st_vfkp-prsdt
                                            st_vfkp-budat
                                            st_dados_aux-shtyp
                                            st_vfkp-knumv
                                            vl_total
                                            st_dados_aux-exti1
                                            st_dados_aux-exti2
                                            st_dados_aux-tknum
                                            ' '
                                            c_6
                                            c_subcontra_patronal
                                            vg_invoicedocnumber_miro
                                            vg_ano_miro
                                            st_dados_aux-tp_emissor
                                            st_dados_aux-tp_veiculo.



            it_zib_contabil_sub[] = it_zib_contabil[].
            DELETE it_zib_contabil     WHERE bktxt EQ 'SUBCONTRATADO'.
            DELETE it_zib_contabil_sub WHERE bktxt NE 'SUBCONTRATADO'.

            PERFORM z_montar_exec_bapi CHANGING vobj_key.

            UPDATE zlest0032 SET: obj_key_sub = vobj_key
                    WHERE tknum = st_dados_aux-tknum.
          ENDIF.

        ENDIF.

        "Quando o Agente de Frete for Proprio ou Intercompany, e proprietario do veiculo for intercompany, deve gerar receita na empresa proprietaria do veiculo.
        IF ( st_dados_aux-tp_emissor = 'P' OR st_dados_aux-tp_emissor = 'I' ) AND st_dados_aux-tp_veiculo EQ '2'.

          IF vobj_keyre IS INITIAL AND v_awkey_rec IS INITIAL .

            PERFORM z_montar_exec_bapi_receita CHANGING vobj_key.

            IF vobj_key IS NOT INITIAL.
              UPDATE zlest0032 SET: obj_key_rec = vobj_key
                      WHERE tknum = st_dados_aux-tknum.
            ENDIF.
          ENDIF.

        ENDIF.

        IF NOT it_zib_contabil[] IS INITIAL.
          MODIFY zib_contabil FROM TABLE it_zib_contabil.
          "
          UPDATE zlest0032 SET: obj_key_seg = vobj_keysg
                                obj_key_ped = vobj_keypd
             WHERE tknum = st_dados_aux-tknum.
          "
          COMMIT WORK.
        ENDIF.


      ENDIF.
    ENDDO."    ALRS 14/07/2015

    CLEAR: st_dados,
      st_dados_aux,
      st_vfkp.

  ENDLOOP.

ENDFORM.                    " Z_PROCESSAR_DADOS.

*&---------------------------------------------------------------------*
*&      Form  Z_ALIMENTAR_GLACCOUNT
*&---------------------------------------------------------------------*
FORM z_expande_fornecedor_pv.

  DATA: w_err_messages  TYPE cvis_message,
        w_succ_messages TYPE cvis_message.

  FREE: w_err_messages,
        w_succ_messages.

  READ TABLE tl_vttk INTO DATA(w_vttk) WITH KEY tknum = st_dados_aux-tknum.
  CHECK sy-subrc = 0.

  READ TABLE ti_vtpa INTO DATA(w_vtpa) WITH KEY vbeln = st_dados_aux-tknum
                                                parvw = c_pv.
  CHECK sy-subrc = 0.

  READ TABLE ti_lifnr_pv INTO DATA(w_lifnr_pv) WITH KEY lifnr = w_vtpa-lifnr.
  CHECK sy-subrc = 0.

*-----------------------------------------
* Empresa do Agente Frete
*-----------------------------------------
  SELECT bukrs, branch
    INTO @DATA(w_branch)
      UP TO 1 ROWS
    FROM j_1bbranch
   WHERE branch = @w_vttk-branch.
  ENDSELECT.

*-----------------------------------------
* verifica se fornecedor ja existe para empresa
*-----------------------------------------
  SELECT *
    INTO @DATA(_lfb1)
    FROM lfb1
      UP TO 1 ROWS
   WHERE lifnr = @w_lifnr_pv-lifnr
     AND bukrs = @w_branch-bukrs.
  ENDSELECT.

  CHECK sy-subrc <> 0.

*-----------------------------------------
* dados fornecedor
*-----------------------------------------
  SELECT *
    INTO @DATA(w_lfa1)
    FROM lfa1
      UP TO 1 ROWS
   WHERE lifnr = @w_lifnr_pv-lifnr.
  ENDSELECT.

  SELECT *
    INTO @DATA(w_lfb1)
    FROM lfb1
      UP TO 1 ROWS
   WHERE lifnr = @w_lifnr_pv-lifnr.
  ENDSELECT.

*-----------------------------------------
* Expansao do fornecedor
*-----------------------------------------
  zcl_mestre_fornecedor=>get_instance(
         )->expandir_dados_fornecedor(
    EXPORTING
      i_lfb1           = w_lfb1            " Mestre de fornecedores (empresa)
      i_lfa1           = w_lfa1            " Mestre de fornecedores (parte geral)
      i_bukrs          = w_branch-bukrs    " Empresa
    IMPORTING
      gs_err_messages  = w_err_messages    " Indicador de erro e mensagens do sistema
      gs_succ_messages = w_succ_messages   " Indicador de sucesso e mensagens do sistema
  ).

  IF w_err_messages-is_error = abap_true.
    LOOP AT w_err_messages-messages INTO DATA(w_message).
      st_return-message = 'Erro expansão Fornecedor:' && w_message-message.
      PERFORM z_listar_inconsistencias.
    ENDLOOP.
  ELSE.
    PERFORM z_executar_comit.

*-----------------------------------------
* verifica se fornecedor foi criado na LFB1
*-----------------------------------------
    DATA(l_count) = 0.

    DO.
      l_count = l_count + 1.
      WAIT UP TO 1 SECONDS. "ALRS
      IF l_count > 3.
        EXIT.
      ENDIF.

      SELECT lifnr, bukrs
        INTO @DATA(_lfb1_bukrs)
        FROM lfb1
          UP TO 1 ROWS
       WHERE lifnr = @w_lifnr_pv-lifnr
         AND bukrs = @w_branch-bukrs.
      ENDSELECT.

      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_ALIMENTAR_GLACCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_000001  text
*      -->P_RAZAO   text
*      -->P_NETWR   text
*      -->P_H       text
*      -->P_BUKRS   text
*      -->P_TPLST   text
*----------------------------------------------------------------------*
FORM z_alimentar_glaccount  USING    p_000001
                                     p_razao
                                     p_netwr
                                     p_deb_cred
                                     p_bukrs
                                     p_bus_area
                                     p_operfrete
                                     p_texto
                                     p_texto2.

  DATA: vl_numero(5) TYPE p DECIMALS 2 VALUE '0.20',
        wl_tka02     TYPE tka02,
        wl_cskb      TYPE cskb,
        vbukrs       TYPE j_1bbranch-bukrs.

  CLEAR st_glaccountdata.

  IF ( st_dados_aux-shtyp EQ 'Z026' ).
    SELECT SINGLE bukrs
               FROM j_1bbranch INTO vbukrs
               WHERE  branch = st_dados_aux-werks.

    SELECT SINGLE *
              FROM tka02
              INTO wl_tka02
              WHERE bukrs  = vbukrs.

    SELECT SINGLE *                    "#EC CI_DB_OPERATION_OK[2389136]
        FROM cskb
        INTO wl_cskb
        WHERE  kokrs  = wl_tka02-kokrs
        AND    kstar  = p_razao
        AND    datab  LE sy-datum
        AND    datbi  GE sy-datum.


    IF sy-subrc = 0.
      DATA: ls_cobl_i TYPE cobl,
            ls_cobl_e TYPE cobl.
      DATA: lt_copadata TYPE TABLE OF copadata WITH HEADER LINE.

      CLEAR: ls_cobl_i, ls_cobl_e.
      REFRESH: lt_copadata.
      READ TABLE ti_vbak INTO st_vbak WITH KEY tknum = st_dados_aux-tknum BINARY SEARCH.
      READ TABLE ti_vbap INTO st_vbap WITH KEY vbeln = st_vbak-vbeln BINARY SEARCH.
      ls_cobl_e-bldat = st_headerdata-doc_date.
      ls_cobl_e-budat = st_headerdata-pstng_date.
      ls_cobl_e-bukrs = vbukrs.
      ls_cobl_e-kokrs = wl_tka02-kokrs.
      ls_cobl_e-matnr = st_vbap-matnr.
      ls_cobl_e-gjahr = ls_cobl_e-budat(4).
      ls_cobl_e-monat = ls_cobl_e-budat+4(2).

      lt_copadata-fnam = 'WWMAT'.
      lt_copadata-fval = st_vbap-matnr.
      APPEND lt_copadata.

      CALL FUNCTION 'COPA_PROFITABILITY_SEGMENT'
        EXPORTING
          dialog              = space
          i_cobl              = ls_cobl_e
        IMPORTING
          e_cobl              = ls_cobl_i
        TABLES
          t_copadata          = lt_copadata
        EXCEPTIONS
          abnormal_leave      = 1
          btrans_not_relevant = 2
          error_copa          = 3
          OTHERS              = 4.
      IF sy-subrc = 0.
        st_glaccountdata-profit_segm_no = ls_cobl_i-paobjnr.
      ENDIF.
    ENDIF.
  ENDIF.

  st_glaccountdata-invoice_doc_item = p_000001.
  st_glaccountdata-gl_account       = p_razao.
  IF p_operfrete = c_2  OR
     p_operfrete = c_5  OR
     p_operfrete = c_1  OR "COFINS
     p_operfrete = c_9  OR "PIS
     p_operfrete = c_29 OR "PIS NÃO ACUMULATIVO
     p_operfrete = c_30 .  "COFINS NÃO ACUMULATIVO
    st_glaccountdata-item_amount = p_netwr.
  ELSE.
    st_glaccountdata-item_amount = ( ( p_netwr * vl_numero ) * vl_numero ).
  ENDIF.
  st_glaccountdata-db_cr_ind        = p_deb_cred.
  st_glaccountdata-comp_code        = p_bukrs.
  st_glaccountdata-bus_area         = p_bus_area.
  st_glaccountdata-alloc_nmbr       = p_texto2.
  CONCATENATE p_texto
              st_dados_aux-exti1
              INTO st_glaccountdata-item_text SEPARATED BY space.

  APPEND st_glaccountdata TO ti_glaccountdata.

ENDFORM.                    " Z_ALIMENTAR_GLACCOUNT
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_PROXIMO_NUMERO
*&---------------------------------------------------------------------*
* Obter o próximo número.
*----------------------------------------------------------------------*
FORM z_obter_proximo_numero  CHANGING    p_number.

* Controle sequencial para retorno de numeração
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = c_01
      object      = c_zform01
    IMPORTING
      number      = p_number.

ENDFORM.                    " Z_OBTER_PROXIMO_NUMERO
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PARTIDA text
*      -->P_NUMBER  text
*      -->P_CHAVE  text
*      -->P_SEQUENCIA text
*      -->P_TPLST  text
*      -->P_TDLNR  text
*      -->P_BUKRS  text
*      -->P_PRSDT  text
*      -->P_BUDAT  text
*      -->P_SHTYP  text
*      -->P_RAZAODEB  text
*      -->P_KNUMV  text
*      -->P_TOTAL  text
*      -->P_EXTI1  text
*      -->P_EXTI2  text
*      -->P_TKNUM  text
*      -->P_PARVW  text
*      -->P_OPERFRETE  text
*      -->P_TIPO  text
*      -->P_NR_MIRO  text
*      -->P_ANO_MIRO  text
*----------------------------------------------------------------------*
FORM z_montar_registro  USING    p_partida
                                 p_number
                                 p_sequencia
                                 p_chave
                                 p_tplst
                                 p_tdlnr
                                 p_bukrs
                                 p_prsdt
                                 p_budat
                                 p_shtyp
                                 p_knumv
                                 p_total
                                 p_exti1
                                 p_exti2
                                 p_tknum
                                 p_parvw
                                 p_operfrete
                                 p_tipo
                                 p_nr_miro
                                 p_ano_miro
                                 p_tp_emissor
                                 p_tp_veiculo.

  DATA: lv_kwert_aux      TYPE konv-kwert,
        lv_vlr_pedagio    TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
        lv_vlr_pis_ped    TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
        lv_vlr_cofins_ped TYPE konv-kbetr,     "*-CS2022000741-04.10.1024-#83334-JT-inicio
        lv_vlr_liq_ped    TYPE konv-kbetr.     "*-CS2022000741-04.10.1024-#83334-JT-inicio

*  IF P_TIPO EQ C_SEGURO AND P_TP_VEICULO EQ '1'.
*    EXIT.
*  ENDIF.

  " ALRS
  p_budat = sy-datum.

  DATA: v_total     TYPE konv-kwert,
        vl_total    TYPE konv-kwert,
        vl_banco(3) TYPE c,
        p_data_val  TYPE datum,
        vl_validto  TYPE j_1btxpis-validto,
        vl_data     TYPE c LENGTH 10.


  DATA: wa_vbfa      TYPE vbfa,
        wa_lips      TYPE lips,
        wa_vbpa      TYPE vbpa,
        wa_zlest0033 TYPE zlest0033,
        v_ratepis    TYPE zlest0215-kbert_pis,
        v_ratecof    TYPE zlest0215-kbert_cofins.

  CASE p_tipo.
    WHEN c_subcontra_patronal.
      CHECK vg_fisica IS NOT INITIAL.
    WHEN OTHERS.
  ENDCASE.


* Inicialiar o campo de valor
  CLEAR vl_total.
  CLEAR st_zib_contabil.
* Somar o valor das condiçoes de seguro e IOF
  IF p_tipo = c_seguro.
    LOOP AT ti_konv INTO st_konv
                    WHERE knumv = p_knumv
                      AND ( kschl = c_zseg
                       OR   kschl = c_ziof ).
      ADD st_konv-kwert TO vl_total.
    ENDLOOP.
  ELSEIF p_tipo = c_pedagio.
*-CS2022000741-04.10.1024-#83334-JT-inicio
    IF p_total IS NOT INITIAL.
      vl_total = p_total.
    ELSE.
*-CS2022000741-04.10.1024-#83334-JT-fim
* Obter o valor do pedágio
      LOOP AT ti_konv INTO st_konv WHERE knumv = p_knumv
                                     AND kschl = c_zped.
        ADD st_konv-kwert TO vl_total.
      ENDLOOP.
    ENDIF.
  ELSE.
    CONCATENATE st_vfkp-budat+6(2) '.'  st_vfkp-budat+4(2) '.' st_vfkp-budat(4) INTO vl_data.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = vl_data
      IMPORTING
        output = vl_validto.

* Obter o valor do Subcontratado
    SELECT SINGLE rate
      FROM j_1btxpis
    INTO v_ratepis
    WHERE country	= 'BR'
    AND   gruop   = 72
    AND   value   = p_tdlnr+6(4)
    AND   validto	  LE vl_validto
    AND   validfrom GE vl_validto.

    SELECT SINGLE rate
      FROM j_1btxcof
    INTO v_ratecof
    WHERE country	= 'BR'
    AND   gruop   = 71
    AND   value   = p_tdlnr+6(4)
    AND   validto	  LE vl_validto
    AND   validfrom GE vl_validto.

    PERFORM f_check_set_pis_cof_pv_pf USING st_dados_aux  'P' CHANGING lv_kwert_aux
                                                                       vg_kbert_pis_pf
                                                                       vg_kbert_cofins_pf.

    IF vg_kbert_pis_pf IS NOT INITIAL.
      v_ratepis = vg_kbert_pis_pf.
    ENDIF.

    IF vg_kbert_cofins_pf IS NOT INITIAL.
      v_ratecof = vg_kbert_cofins_pf.
    ENDIF.

*    READ TABLE TI_KONV INTO ST_KONV WITH KEY KNUMV = P_KNUMV
*                                             KSCHL = C_ZFRE
*                                             BINARY SEARCH.

    CLEAR: v_total.
    LOOP AT ti_konv INTO st_konv WHERE knumv = p_knumv  "Correção: Atualemnte esta pegando apenas um valor
                                   AND kschl = c_zfre.  "da tabela e sera corrigido para pegar todos os valores.

      v_total = st_konv-kwert.

      IF p_tipo = c_subcontra_liqui.
        v_total = v_total - ( v_total * ( v_ratepis ) / 100 ) - ( v_total * ( v_ratecof ) / 100 ).
      ELSEIF p_tipo = c_subcontra_cofin.
        st_zib_contabil-rate = v_ratecof.
        v_total = ( v_total * ( v_ratecof ) / 100 ).
      ELSEIF p_tipo = c_subcontra_pis.
        st_zib_contabil-rate = v_ratepis.
        v_total = ( v_total * ( v_ratepis ) / 100 ).
      ELSEIF p_tipo = c_subcontra_patronal.
        v_total = ( ( v_total * '0.2' ) * '0.2').
      ENDIF.

      ADD v_total TO vl_total.
    ENDLOOP.
  ENDIF.


* Não gravar registros sem valor.
  CHECK vl_total IS NOT INITIAL.


  st_zib_contabil-mandt         = sy-mandt.
  CONCATENATE c_les
              p_number
              sy-datum(4) INTO st_zib_contabil-obj_key.
  st_zib_contabil-seqitem       = p_sequencia.
  st_zib_contabil-bschl         = p_chave.
  IF strlen( p_tdlnr ) = 10.
    st_zib_contabil-gsber         = p_tdlnr+6(4).
  ELSE.
    st_zib_contabil-gsber         = p_tdlnr.
  ENDIF.

  st_zib_contabil-bukrs         = p_bukrs.
  st_zib_contabil-interface     = c_3.
  IF p_tipo = c_seguro.
    st_zib_contabil-bktxt         = c_seguro_frete.
    CONCATENATE c_txt_seguro
                p_exti1 INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ELSEIF p_tipo = c_pedagio.
    st_zib_contabil-bktxt         = c_pedagio_frete.
    CONCATENATE c_txt_pedagio
                p_exti1 INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ELSEIF  p_tipo = c_subcontra_total.
    st_zib_contabil-bktxt         = 'SUBCONTRATADO'.
    CONCATENATE 'TOTAL Frete'
                p_exti1  INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ELSEIF  p_tipo = c_subcontra_liqui.
    st_zib_contabil-bktxt         = 'SUBCONTRATADO'.
    CONCATENATE 'LIQUIDO'
                p_exti1  INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ELSEIF  p_tipo = c_subcontra_pis.
    st_zib_contabil-bktxt         = 'SUBCONTRATADO'.
    CONCATENATE 'PIS'
                p_exti1  INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ELSEIF  p_tipo = c_subcontra_cofin.
    st_zib_contabil-bktxt         = 'SUBCONTRATADO'.
    CONCATENATE 'COFINS'
                p_exti1  INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ELSEIF p_tipo = c_subcontra_patronal.
    st_zib_contabil-bktxt         = 'SUBCONTRATADO'.
    CONCATENATE 'INSS PATRONAL'
                p_exti1  INTO st_zib_contabil-sgtxt
                SEPARATED BY space.
  ENDIF.

  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = p_budat
      p_bukrs        = p_bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    IMPORTING
      p_data_val     = p_data_val
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

  IF p_budat+4(2) NE   p_data_val+4(2).
    p_prsdt = p_data_val.
    p_budat = p_data_val.
  ENDIF.

  CONCATENATE p_prsdt+6(2)      "dia
              c_ponto
              p_prsdt+4(2)      "mes
              c_ponto
              p_prsdt+0(4)      "ano
              INTO st_zib_contabil-bldat.
  CONCATENATE p_budat+6(2)      "dia
              c_ponto
              p_budat+4(2)      "mes
              c_ponto
              p_budat+0(4)      "ano
              INTO st_zib_contabil-budat.
  st_zib_contabil-gjahr         = p_budat(4).
  st_zib_contabil-monat         = p_budat+4(2).
  st_zib_contabil-blart         = c_me.
  CONCATENATE p_nr_miro
          p_ano_miro INTO st_zib_contabil-xblnr.
  IF strlen( p_tdlnr ) = 10.
    st_zib_contabil-bupla         = p_tdlnr+6(4).
  ELSE.
    st_zib_contabil-bupla         = p_tdlnr.
  ENDIF.

  IF p_tipo = c_subcontra_total OR
     p_tipo = c_subcontra_liqui OR
     p_tipo = c_subcontra_cofin OR
     p_tipo = c_subcontra_patronal OR
     p_tipo = c_subcontra_pis.
    st_zib_contabil-blart = 'FR'.
    st_zib_contabil-xblnr = p_exti1.
    IF strlen( p_tdlnr ) = 10.
      st_zib_contabil-bupla         = p_tdlnr+6(4).
    ELSE.
      st_zib_contabil-bupla         = p_tdlnr.
    ENDIF.
  ENDIF.

  IF ( st_dados-shtyp EQ 'Z026' ).

    IF p_partida = c_1 OR
       p_partida = c_4 .

      IF  p_partida = c_4 AND p_tipo = c_seguro.
        CLEAR st_vtpa.
        READ TABLE ti_vtpa INTO st_vtpa WITH KEY vbeln = p_tknum
                                                 parvw = p_parvw
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          st_zib_contabil-hkont       = st_vtpa-lifnr.
        ENDIF.
      ELSE.
        CLEAR st_zlest0021.
        READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = p_shtyp
                                                           tcode      = c_f02
                                                           tp_emissor = p_tp_emissor
                                                           operfrete  = p_operfrete
                                                           tp_veiculo = p_tp_veiculo
                                                           BINARY SEARCH.

        IF sy-subrc EQ 0.
          IF ( st_zlest0021-operfrete = c_10   OR
               st_zlest0021-operfrete = c_1    OR
               st_zlest0021-operfrete = c_4    OR
               st_zlest0021-operfrete = c_9    OR
               st_zlest0021-operfrete = c_12  ) OR
             ( st_zlest0021-operfrete = c_14  AND
               p_partida = c_3 ).
            st_zib_contabil-hkont       = st_zlest0021-razaodeb.
          ELSEIF ( p_partida = c_4                AND
                   st_zlest0021-operfrete = c_14 ) OR
                 ( p_partida = c_3                AND
                 ( st_zlest0021-operfrete = c_3 OR st_zlest0021-operfrete = c_23 ) ).
            st_zib_contabil-hkont  = st_zlest0021-razaocred.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF ( p_partida = c_2 AND p_tipo = c_pedagio ).


      CLEAR st_vtpa.
      READ TABLE ti_vtpa INTO st_vtpa WITH KEY vbeln = p_tknum
                                               parvw = p_parvw
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        st_zib_contabil-hkont       = st_vtpa-lifnr.
      ENDIF.

      DATA: p_valor TYPE  netwr_fp.
      DATA: p_waers TYPE  waers.
      p_valor = vl_total.
      p_waers = c_brl.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs           = st_zib_contabil-bukrs
          p_lifnr           = st_zib_contabil-hkont
          p_zlsch           = zcl_miro=>st_forma_pagamento_tranf
          p_valor           = p_valor
          p_bvtyp           = '0001'
          p_waers           = p_waers
        IMPORTING
          p_forma_pagamento = st_zib_contabil-zlsch
          p_princ_bnc_emp   = st_zib_contabil-hbkid
        EXCEPTIONS
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          banco_empresa     = 5
          OTHERS            = 6.

      IF sy-subrc IS NOT INITIAL.
        CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
          EXPORTING
            p_bukrs           = st_zib_contabil-bukrs
            p_lifnr           = st_zib_contabil-hkont
            p_zlsch           = zcl_miro=>st_forma_pagamento_ted
            p_valor           = p_valor
            p_bvtyp           = '0001'
            p_waers           = p_waers
          IMPORTING
            p_forma_pagamento = st_zib_contabil-zlsch
            p_princ_bnc_emp   = st_zib_contabil-hbkid
          EXCEPTIONS
            nao_fornecedor    = 1
            fornecedor_conta  = 2
            fornecedor_banco  = 3
            faixa_valor       = 4
            banco_empresa     = 5
            OTHERS            = 6.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.

        "Obter o banco
        CLEAR vl_banco.
        SELECT bankl INTO vl_banco
                     FROM lfbk
                  UP TO 1 ROWS
                    WHERE lifnr = st_zib_contabil-hkont.
        ENDSELECT.

        IF vl_banco(3)     = c_237.
          st_zib_contabil-hbkid = c_bbd.
        ELSEIF vl_banco(3) = c_399.
          st_zib_contabil-hbkid = c_hsbc.
        ELSE.
          st_zib_contabil-hbkid = c_bbra.
        ENDIF.

        IF vl_banco(3) = c_001 OR
           vl_banco(3) = c_237 OR
           vl_banco(3) = c_399.
          st_zib_contabil-zlsch       = c_u.
        ELSEIF vl_total < 5000.
          st_zib_contabil-zlsch       = c_m.
        ELSE.
          st_zib_contabil-zlsch       = c_s.
        ENDIF.

      ENDIF.

    ELSEIF ( p_partida = c_3 AND p_tipo = c_seguro ).
      CLEAR st_zlest0021.
      READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = p_shtyp
                                                         tcode      = c_f02
                                                         tp_emissor = p_tp_emissor
                                                         operfrete  = p_operfrete
                                                         tp_veiculo = p_tp_veiculo
                                                         BINARY SEARCH.
      st_zib_contabil-hkont  = st_zlest0021-razaocred.

    ELSEIF ( p_partida = c_3 AND p_tipo = c_pedagio ).

*      CLEAR: WA_VBFA, WA_LIPS, WA_VBPA.
*
*      SELECT SINGLE * FROM VBFA INTO WA_VBFA WHERE VBELN   EQ P_TKNUM
*                                               AND VBTYP_N EQ '8'.
*
*      SELECT SINGLE * FROM LIPS INTO WA_LIPS WHERE VBELN EQ WA_VBFA-VBELV.
*
*
*      SELECT SINGLE * FROM VBPA INTO WA_VBPA WHERE VBELN EQ WA_LIPS-VGBEL
*                                               AND PARVW EQ 'AG'.
*
*      IF SY-SUBRC EQ 0.
*        ST_ZIB_CONTABIL-HKONT       = WA_VBPA-KUNNR.
*      ENDIF.
      CLEAR st_zlest0021.
      READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = p_shtyp
                                                         tcode      = c_f02
                                                         tp_emissor = p_tp_emissor
                                                         operfrete  = p_operfrete
                                                         tp_veiculo = p_tp_veiculo
                                                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF  st_zlest0021-operfrete = c_14.
          st_zib_contabil-hkont       = st_zlest0021-razaodeb.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.

    IF p_partida = c_1 OR
       p_partida = c_3 OR
     ( p_partida = c_2 AND p_tipo = c_subcontra_patronal ) OR
     ( p_partida = c_4 AND p_tipo = c_pedagio ).
      CLEAR st_zlest0021.
      READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = p_shtyp
                                                         tcode      = c_f02
                                                         tp_emissor = p_tp_emissor
                                                         operfrete  = p_operfrete
                                                         tp_veiculo = p_tp_veiculo
                                                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF ( st_zlest0021-operfrete = c_10   OR
             st_zlest0021-operfrete = c_1    OR
             st_zlest0021-operfrete = c_4    OR
             st_zlest0021-operfrete = c_9    OR
             st_zlest0021-operfrete = c_12 ) OR
           ( st_zlest0021-operfrete = c_14  AND p_partida = c_3 ) OR
           ( st_zlest0021-operfrete = c_6   AND p_partida = c_1 ).

          st_zib_contabil-hkont       = st_zlest0021-razaodeb.

        ELSEIF ( p_partida = c_4 AND st_zlest0021-operfrete = c_14 ) OR
               ( p_partida = c_2 AND st_zlest0021-operfrete = c_6  ) OR
               ( p_partida = c_3 AND ( st_zlest0021-operfrete = c_3 OR st_zlest0021-operfrete = c_23 ) ).

          st_zib_contabil-hkont  = st_zlest0021-razaocred.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR st_vtpa.
      READ TABLE ti_vtpa INTO st_vtpa WITH KEY vbeln = p_tknum
                                               parvw = p_parvw
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        st_zib_contabil-hkont       = st_vtpa-lifnr.
      ENDIF.

      p_valor = vl_total.
      p_waers = c_brl.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs           = st_zib_contabil-bukrs
          p_lifnr           = st_zib_contabil-hkont
          p_zlsch           = zcl_miro=>st_forma_pagamento_tranf
          p_valor           = p_valor
          p_bvtyp           = '0001'
          p_waers           = p_waers
        IMPORTING
          p_forma_pagamento = st_zib_contabil-zlsch
          p_princ_bnc_emp   = st_zib_contabil-hbkid
        EXCEPTIONS
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          banco_empresa     = 5
          OTHERS            = 6.

      IF sy-subrc IS NOT INITIAL.
        CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
          EXPORTING
            p_bukrs           = st_zib_contabil-bukrs
            p_lifnr           = st_zib_contabil-hkont
            p_zlsch           = zcl_miro=>st_forma_pagamento_ted
            p_valor           = p_valor
            p_bvtyp           = '0001'
            p_waers           = p_waers
          IMPORTING
            p_forma_pagamento = st_zib_contabil-zlsch
            p_princ_bnc_emp   = st_zib_contabil-hbkid
          EXCEPTIONS
            nao_fornecedor    = 1
            fornecedor_conta  = 2
            fornecedor_banco  = 3
            faixa_valor       = 4
            banco_empresa     = 5
            OTHERS            = 6.
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
* Obter o banco
        CLEAR vl_banco.
        SELECT bankl INTO vl_banco
                     FROM lfbk
                  UP TO 1 ROWS
                    WHERE lifnr = st_zib_contabil-hkont.
        ENDSELECT.

        IF vl_banco(3)     = c_237.
          st_zib_contabil-hbkid = c_bbd.
        ELSEIF vl_banco(3) = c_399.
          st_zib_contabil-hbkid = c_hsbc.
        ELSE.
          st_zib_contabil-hbkid = c_bbra.
        ENDIF.

        IF vl_banco(3) = c_001 OR
           vl_banco(3) = c_237 OR
           vl_banco(3) = c_399.
          st_zib_contabil-zlsch       = c_u.
        ELSEIF vl_total < 5000.
          st_zib_contabil-zlsch       = c_m.
        ELSE.
          st_zib_contabil-zlsch       = c_s.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  "========================================================== CS2019001590 - AOENNING.
  IF p_partida EQ c_1 AND p_operfrete EQ c_12 AND st_zlest0021-operfrete EQ c_12.
    SELECT SINGLE *
      FROM zlest0033
      INTO wa_zlest0033
      WHERE bukrs =  st_zib_contabil-bukrs
      AND   gsber =  st_zib_contabil-gsber
      AND   saknr =  st_zlest0021-razaodeb.
    IF wa_zlest0033-kostl IS NOT INITIAL.
      st_zib_contabil-kostl = wa_zlest0033-kostl.
    ENDIF.
  ENDIF.
  "========================================================== CS2019001590 - AOENNING.

  st_zib_contabil-wrbtr         = vl_total.
  st_zib_contabil-waers         = c_brl.
  st_zib_contabil-waers_i       = c_brl.

  SHIFT p_exti2 LEFT DELETING LEADING space.
  CONCATENATE 'FR-' p_exti2 INTO st_zib_contabil-zuonr.
  st_zib_contabil-dmbtr         = vl_total.
  IF p_chave  = '31' OR p_chave = '21'.
    SELECT bvtyp INTO st_zib_contabil-bvtyp
                 FROM lfbk
              UP TO 1 ROWS
                WHERE lifnr = st_zib_contabil-hkont.
    ENDSELECT.
  ENDIF.

  st_zib_contabil-rg_atualizado = c_n.

  IF p_tipo = c_seguro AND
     p_chave = c_50.
    SELECT SINGLE *
      FROM zlest0033
      INTO wa_zlest0033
      WHERE bukrs =  st_zib_contabil-bukrs
      AND   gsber =  st_zib_contabil-gsber
      AND   saknr =  st_zib_contabil-hkont.
    IF sy-subrc = 0.
      st_zib_contabil-kostl = wa_zlest0033-kostl.
    ENDIF.
  ENDIF.

  " Objeto de custo
  DATA:
    wl_tka02 TYPE tka02,
    wl_cskb  TYPE cskb.

  SELECT SINGLE *
           FROM tka02
           INTO wl_tka02
           WHERE bukrs  = st_zib_contabil-bukrs.

  SELECT SINGLE *                      "#EC CI_DB_OPERATION_OK[2389136]
      FROM cskb
      INTO wl_cskb
      WHERE  kokrs  = wl_tka02-kokrs
      AND    kstar  = st_zib_contabil-hkont
      AND    datab  LE sy-datum
      AND    datbi  GE sy-datum.

  IF sy-subrc = 0.
    READ TABLE ti_vbak INTO st_vbak WITH KEY tknum = p_tknum BINARY SEARCH.
    READ TABLE ti_vbap INTO st_vbap WITH KEY vbeln = st_vbak-vbeln BINARY SEARCH.
*    ST_ZIB_CONTABIL-MATNR = ST_VBAP-MATNR.
    st_zib_contabil-matnr_fi = st_vbap-matnr.
  ENDIF.

  APPEND st_zib_contabil TO it_zib_contabil.

ENDFORM.                    " Z_MONTAR_REGISTRO
*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTAR_COMIT
*&---------------------------------------------------------------------*
* Executar a função de comit para bapi
*----------------------------------------------------------------------*
FORM z_executar_comit .

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " Z_EXECUTAR_COMIT
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CABECALHO_BAPI
*&---------------------------------------------------------------------*
* Alimentar a estrutura do cabeçalho da bapi
*----------------------------------------------------------------------*
FORM z_montar_cabecalho_bapi.

  DATA: vl_stkzn TYPE lfa1-stkzn,
        vl_stcd2 TYPE lfa1-stcd2,
        it_t001b TYPE TABLE OF t001b,
        wa_t001b TYPE t001b,
        wl_data  TYPE sy-datum,
        wa_iva   TYPE zib_cte_dist_iva.

  CLEAR: st_vfkp_acum,
         st_vtpa,
         wl_data.

* Obter o valor acumulado para o custo de frete
  READ TABLE ti_vfkp_acum INTO st_vfkp_acum WITH KEY fknum = st_vfkp-fknum
                                            BINARY SEARCH.

* Obter o parceiro PV (Proprietário do veículo) para o cabeçalho da miro
  READ TABLE ti_vtpa INTO st_vtpa WITH KEY vbeln = st_dados_aux-tknum
                                           parvw = c_pv
                                  BINARY SEARCH.

  SELECT SINGLE stkzn stcd2   "*-CS2022000741-04.10.1024-#83334-JT-inicio
    FROM lfa1
    INTO (vl_stkzn, vl_stcd2) "*-CS2022000741-04.10.1024-#83334-JT-inicio
   WHERE lifnr EQ st_vtpa-lifnr.

  IF vl_stkzn IS NOT INITIAL AND vl_stcd2 IS NOT INITIAL.  "*-CS2022000741-04.10.1024-#83334-JT-inicio
    vg_fisica = 'X'.
  ELSE.
    vg_fisica = space.
  ENDIF.

  SELECT * FROM t001b INTO TABLE it_t001b
    WHERE bukrs EQ st_vfkp-bukrs.

  SORT it_t001b BY toye1.
  DELETE ADJACENT DUPLICATES FROM it_t001b COMPARING toye1.

*  if ( st_vfkp-budat(4) eq sy-datum(4) ) and ( st_vfkp-budat+4(2) eq sy-datum+4(2) ).
  "WL_DATA = ST_VFKP-BUDAT.
  wl_data = sy-datum. "ALRS
*  else.

*    read table it_t001b into wa_t001b with key toye1 = st_vfkp-budat(4) binary search.
*
*    if sy-subrc is initial.
*      if ( wa_t001b-frpe1 le st_vfkp-budat+4(2) and
*           wa_t001b-tope1 ge st_vfkp-budat+4(2) ) and
*           wa_t001b-toye1 eq st_vfkp-budat(4).
*        concatenate st_vfkp-budat(4) st_vfkp-budat+4(2) st_vfkp-budat+6(2) into wl_data.
*      else.
*        concatenate st_vfkp-budat(4) wa_t001b-tope1+1 '01' into wl_data.
*      endif.
*    else.
*      sort it_t001b by mkoar.
*      read table it_t001b into wa_t001b with key mkoar = c_mais binary search.
*      if sy-subrc is initial.
*        concatenate wa_t001b-frye1 wa_t001b-frpe1+1 '01' into wl_data.
*      else.
*        concatenate st_vfkp-budat(4) st_vfkp-budat+4(2) st_vfkp-budat+6(2) into wl_data.
*      endif.
*    endif.
*  endif.

  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = wl_data
      p_bukrs        = st_vfkp-bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    IMPORTING
      p_data_val     = wl_data
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Alimentar a tabela e estruturas de Cabeçalho
  st_headerdata-invoice_ind     = c_x.
  st_headerdata-doc_type        = c_fr.
  st_headerdata-doc_date        = st_vfkp-budat.
  st_headerdata-pstng_date      = wl_data.
  st_headerdata-bline_date      = wl_data.

  READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados_aux-tknum BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    READ TABLE ti_header INTO st_header WITH KEY docnum = st_notas-docnum BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CASE st_header-nfe.
        WHEN 'X'.
          CONCATENATE st_header-nfenum '-' st_header-series INTO st_headerdata-ref_doc_no.
        WHEN space.
          CONCATENATE st_header-nfnum '-' st_header-series INTO st_headerdata-ref_doc_no.
      ENDCASE.
    ELSE.
      st_headerdata-ref_doc_no = st_dados_aux-exti2.
    ENDIF.
  ELSE.
    st_headerdata-ref_doc_no = st_dados_aux-exti2.

  ENDIF.

  st_headerdata-comp_code       = st_vfkp-bukrs.

  CLEAR st_headerdata-diff_inv.
  IF st_dados_aux-tp_emissor = 'P' OR  st_dados_aux-shtyp EQ 'Z026'.
    st_headerdata-diff_inv        = st_vtpa-lifnr.
  ELSE.

  ENDIF.
  st_headerdata-currency        = st_vfkp-waers.
  st_headerdata-gross_amount    = st_vfkp_acum-netwr.

  IF st_dados_aux-tp_emissor = 'I'. "INTERCOMPANY
    st_headerdata-pmnttrms        = c_z005.
    st_headerdata-pmnt_block      = c_a.
    st_headerdata-pymt_meth       = c_e.
    st_headerdata-partner_bk      = c_0001.
  ELSE.
    st_headerdata-pmnttrms        = c_z001.
    st_headerdata-pmnt_block      = c_u.
    st_headerdata-pymt_meth       = c_e.
    CLEAR st_headerdata-partner_bk.
  ENDIF.

  CONCATENATE 'FR-' st_dados_aux-exti2 INTO st_headerdata-alloc_nmbr.
* Obter a condição de ICMS
  READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                           kschl = c_zicm
                                           BINARY SEARCH.

  IF ( sy-subrc IS INITIAL ) AND ( st_konv-kwert IS NOT INITIAL ).
    DATA(lv_diferimento) = space.
    PERFORM f_get_diferimento USING st_dados_aux CHANGING lv_diferimento.
    IF lv_diferimento EQ abap_true.
      wa_iva-icms = abap_false.
    ELSE.
      wa_iva-icms = abap_true.
    ENDIF.
  ELSE.
    wa_iva-icms = abap_false.
  ENDIF.

  wa_iva-bukrs    = st_vfkp-bukrs.
  wa_iva-shtyp    = st_dados_aux-shtyp.
  wa_iva-tdlnr    = st_vtpa-lifnr.

  "Busca Local de negócio: categoria CFOP
  SELECT SINGLE industry
    INTO wa_iva-industry
    FROM j_1bbranch
   WHERE bukrs  EQ st_vfkp-bukrs
     AND branch EQ st_vfkp-werks.

  CASE st_dados_aux-tp_emissor.
    WHEN 'I'.

      DATA: ag_intercompany TYPE tdlnr.

      "Bucar por Fornecedor (Excessão)
      SELECT SINGLE * INTO wa_iva
        FROM zib_cte_dist_iva
       WHERE bukrs    EQ wa_iva-bukrs
         AND shtyp    EQ wa_iva-shtyp
         AND tdlnr    EQ st_dados_aux-fornecedor
         AND industry EQ wa_iva-industry
         AND icms     EQ wa_iva-icms.

    WHEN OTHERS.

      "Bucar por Fornecedor (Excessão)
      SELECT SINGLE * INTO wa_iva
        FROM zib_cte_dist_iva
       WHERE bukrs    EQ wa_iva-bukrs
         AND shtyp    EQ wa_iva-shtyp
         AND tdlnr    EQ wa_iva-tdlnr
         AND industry EQ wa_iva-industry
         AND icms     EQ wa_iva-icms.

  ENDCASE.

  "Bucar sem Fornecedor
  IF sy-subrc IS NOT INITIAL.

    SELECT SINGLE * INTO wa_iva
      FROM zib_cte_dist_iva
     WHERE bukrs    EQ wa_iva-bukrs
       AND shtyp    EQ wa_iva-shtyp
       AND industry EQ wa_iva-industry
       AND icms     EQ wa_iva-icms
       AND tdlnr    EQ space.

    IF sy-subrc IS NOT INITIAL.
      st_headerdata-del_costs_taxc = 'I7'.
    ELSE.
      st_headerdata-del_costs_taxc = wa_iva-mwskz.
    ENDIF.
  ELSE.
    st_headerdata-del_costs_taxc = wa_iva-mwskz.
  ENDIF.

  TRY .
      st_headerdata-housebankid = zcl_miro=>get_banco_forma_pagamento( i_bukrs = st_headerdata-comp_code i_forma_pagamento  = st_headerdata-pymt_meth ).
    CATCH zcx_miro_exception.  "

  ENDTRY.

  IF st_dados_aux-tp_emissor = 'I'. "INTERCOMPANY
    st_headerdata-bus_area        = st_vfkp-werks.
    st_headerdata-business_place  = st_vfkp-werks.
  ELSE.
    st_headerdata-bus_area        = st_dados_aux-tdlnr+6(4).
  ENDIF.
*  st_headerdata-j_1bnftype      = c_c2.

ENDFORM.                    " Z_MONTAR_CABECALHO_BAPI
*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTAR_BAPI
*&---------------------------------------------------------------------*
* Executar a bapi da MIRO.
*----------------------------------------------------------------------*
FORM z_executar_bapi .

  CLEAR: vg_invoicedocnumber_miro,
         vg_ano_miro,
         ti_return,
         vg_docnum.

* Criar o documento de revisão de fatura através da bapi da MIRO
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata       = st_headerdata
    IMPORTING
      invoicedocnumber = vg_invoicedocnumber_miro
      fiscalyear       = vg_ano_miro
    TABLES
      itemdata         = ti_itemdata
      glaccountdata    = ti_glaccountdata
      withtaxdata      = ti_withtaxdata
      return           = ti_return.

  IF ti_return[] IS NOT INITIAL.
    LOOP AT ti_return INTO st_return.
      PERFORM z_listar_inconsistencias.
      ROLLBACK WORK.
    ENDLOOP.
  ELSE.

    IF ( st_dados_aux-shtyp NE 'Z026' ).
** Não processar documentos de transporte sem a nota fiscal.
      READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados-tknum
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        PERFORM z_montar_bapi_nf USING st_headerdata-del_costs_taxc.
      ENDIF.
* Se a nota fiscal foi criada, atualizar o banco de dados.
      IF vg_docnum IS NOT INITIAL.
* Desvio para rotina de atualizar a tabela zlest0032.
        PERFORM z_atualizar_zlest0032.
        PERFORM z_executar_comit.
      ENDIF.

    ENDIF.
  ENDIF.


  CALL FUNCTION 'DEQUEUE_ALL'
    EXPORTING
      _synchron = 'X'.
  WAIT UP TO 1 SECONDS.
ENDFORM.                    " Z_EXECUTAR_BAPI
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_ITEM_BAPI
*&---------------------------------------------------------------------*
* Alimentar a tabela intera de itens da bapi.
*----------------------------------------------------------------------*
FORM z_montar_item_bapi .

  CLEAR: st_itemdata,
         st_ekbe    .

* Alimentar a tabela de Itens
  st_itemdata-invoice_doc_item      = c_000001.
  st_itemdata-po_number             = st_dados_aux-ebeln.
  st_itemdata-po_item               = st_dados_aux-ebelp.

  READ TABLE ti_ekbe INTO st_ekbe
  WITH KEY ebeln = st_dados_aux-ebeln ebelp = st_dados_aux-ebelp lfbnr = st_dados_aux-lblni
  BINARY SEARCH.

  st_itemdata-ref_doc               = st_ekbe-belnr.
  st_itemdata-ref_doc_year          = st_ekbe-lfgja.
  st_itemdata-item_amount           = st_vfkp-netwr.
  st_itemdata-sheet_no              = st_dados_aux-lblni.
  st_itemdata-tax_code              = st_headerdata-del_costs_taxc.
  APPEND st_itemdata TO ti_itemdata.

ENDFORM.                    " Z_MONTAR_ITEM_BAPI
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_FRETE
*&---------------------------------------------------------------------*
* Alimentar a tabela com dados da conta de frete
*----------------------------------------------------------------------*
FORM z_montar_conta_frete .

  DATA: vl_razao    TYPE zlest0021-razaocred,
        vl_deb_cred TYPE c,
        vg_zuonr    TYPE dzuonr.

  CLEAR st_zlest0021.

* Alimentar a tabela com dados do conta razão - Transitória do Frete
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_2
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  CHECK sy-subrc EQ 0.
  DO 2 TIMES.
    IF sy-index = 1.
      vl_razao = st_zlest0021-razaocred.
      vl_deb_cred = c_h.
    ELSE.
      vl_razao = st_zlest0021-razaodeb.
      vl_deb_cred = c_s.
    ENDIF.

    CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.

    PERFORM z_alimentar_glaccount USING c_000001
                                        vl_razao
                                        st_vfkp-netwr
                                        vl_deb_cred
                                        st_vfkp-bukrs
                                        st_dados_aux-tplst
                                        st_zlest0021-operfrete
                                        TEXT-012
                                        vg_zuonr.
  ENDDO.

ENDFORM.                    " Z_MONTAR_CONTA_FRETE

*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_INSS
*&---------------------------------------------------------------------*
* Alimentar a tabela com dados da conta de ICMS
*----------------------------------------------------------------------*
FORM z_montar_conta_icms .

  DATA: vl_razao    TYPE zlest0021-razaocred,
        vl_bus_area TYPE bapi_incinv_create_gl_account-bus_area,
        vl_deb_cre  TYPE c,
        vl_total    TYPE konv-kwert,
        vg_zuonr    TYPE dzuonr,
        vl_lifnr    TYPE lfa1-lifnr.

  CLEAR: st_zlest0021,
         st_konv     .

* Alimentar a tabela com dados do conta razão - ICMS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_5
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  CHECK sy-subrc = 0.

* Obter o valor da condição referente ao icms
  CLEAR: vl_total.
  LOOP AT ti_konv INTO st_konv WHERE knumv = st_vfkp-knumv
                                 AND kschl = c_zicm.
    ADD st_konv-kwert TO vl_total.
  ENDLOOP.

  DO 2 TIMES.
    IF sy-index = 1.
      vl_razao    = st_zlest0021-razaocred.
      vl_bus_area = st_dados_aux-tplst.
      vl_deb_cre  = c_h.
    ELSE.
      vl_razao    = st_zlest0021-razaodeb.
      vl_bus_area = st_dados_aux-tplst.
      vl_deb_cre  = c_s.
    ENDIF.

*-CS2021000779 - 23.07.2021 - JT - inicio
    DATA(lv_diferimento) = space.
    PERFORM f_get_diferimento USING st_dados_aux CHANGING lv_diferimento.
    IF lv_diferimento EQ abap_true.
      EXIT.
    ENDIF.
*-CS2021000779 - 23.07.2021 - JT - fim

    CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.
    PERFORM z_alimentar_glaccount USING c_000001
                                        vl_razao
                                        vl_total
                                        vl_deb_cre
                                        st_vfkp-bukrs
                                        vl_bus_area
                                        st_zlest0021-operfrete
                                        TEXT-013
                                        vg_zuonr.
  ENDDO.

ENDFORM.                    " Z_MONTAR_CONTA_ICMS

FORM f_get_diferimento USING p_dados_aux  TYPE y_dados CHANGING r_diferimento TYPE c.

  CLEAR: r_diferimento.

  DATA(lv_crtrn) = space.
  PERFORM f_get_cod_regime_trib USING p_dados_aux CHANGING lv_crtrn.
  IF lv_crtrn EQ '4'. "Diferimento
    r_diferimento = abap_true.
  ENDIF.

ENDFORM.

FORM f_get_cod_regime_trib USING p_dados_aux  TYPE y_dados
                        CHANGING r_crtn TYPE lfa1-crtn.

  DATA: vl_bus_area TYPE bapi_incinv_create_gl_account-bus_area,
        vl_lifnr    TYPE lfa1-lifnr.

  CLEAR: vl_lifnr, r_crtn.

  vl_bus_area = p_dados_aux-tplst.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_bus_area
    IMPORTING
      output = vl_lifnr.

  CHECK vl_lifnr IS NOT INITIAL.

  SELECT SINGLE crtn
    FROM lfa1 INTO r_crtn
   WHERE lifnr EQ vl_lifnr.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_INSS_PATRONAL
*&---------------------------------------------------------------------*
* Alimentar a tabela com dados da conta de inss patronal
*----------------------------------------------------------------------*
FORM z_montar_conta_inss_patronal .

  DATA: vl_razao    TYPE zlest0021-razaocred,
        vl_bus_area TYPE bapi_incinv_create_gl_account-bus_area,
        vl_deb_cre  TYPE c,
        vg_zuonr    TYPE dzuonr.

  CLEAR st_zlest0021.

  CHECK vg_fisica IS NOT INITIAL.

* Alimentar a tabela com dados do conta razão - INSS Patronal
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_6
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  CHECK sy-subrc = 0.
  DO 2 TIMES.
    IF sy-index = 1.
      vl_razao    = st_zlest0021-razaocred.
      vl_bus_area = st_dados_aux-tdlnr+6(4).
      vl_deb_cre  = c_h.
    ELSE.
      vl_razao    = st_zlest0021-razaodeb.
      vl_bus_area = st_dados_aux-tplst.
      vl_deb_cre  = c_s.
    ENDIF.
    CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.
    PERFORM z_alimentar_glaccount USING c_000001
                                        vl_razao
                                        st_vfkp-netwr
                                        vl_deb_cre
                                        st_vfkp-bukrs
                                        vl_bus_area
                                        st_zlest0021-operfrete
                                        TEXT-014
                                        vg_zuonr.
  ENDDO.
ENDFORM.                    " Z_MONTAR_CONTA_INSS_PATRONAL
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_TAXAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_montar_taxas TABLES t_withtaxdata STRUCTURE bapi_incinv_create_withtax
                     USING p_vi TYPE y_vfkp
                           p_emp_ck_imp_retido TYPE c
                  CHANGING c_msg_error TYPE c .

  DATA: lva_imposto_ret TYPE string.

  DATA: vl_base      TYPE bapi_incinv_create_withtax-wi_tax_base,
        vl_imposto   TYPE bapi_incinv_create_withtax-wi_tax_amt,
        vl_tabix     TYPE sy-tabix,
        vl_kschl     TYPE konv-kschl,
        vl_kschl2    TYPE konv-kschl,
        wa_zlest0023 TYPE zlest0023.

  CLEAR: c_msg_error.

*------------------------------------------------------------------------------------------------------------------------------*
* Valida se possui condicoes referente a impostos retidos da VI, e se as mesmas estão parametrizadas no cadastro do fornecedor
*------------------------------------------------------------------------------------------------------------------------------*
  LOOP AT ti_vtpa INTO st_vtpa WHERE vbeln = st_dados_aux-tknum
                                 AND parvw = c_pv.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(lwa_lfa1_pv)
    WHERE  lifnr EQ @st_vtpa-lifnr.

    IF sy-subrc NE 0.
      CONCATENATE 'Cadastro do fornecedor: ' st_vtpa-lifnr ' não encontrado!' INTO c_msg_error SEPARATED BY space.
      RETURN.
    ENDIF.


* Obter a nota fiscal referente ao documento de transporte
    READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados-tknum
                                      BINARY SEARCH.

    CHECK sy-subrc EQ 0.

* Obter a empresa da nota fiscal
    READ TABLE ti_header INTO st_header WITH KEY docnum = st_notas-docnum
                                        BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    LOOP AT ti_konv INTO st_konv WHERE ( knumv EQ st_vfkp-knumv )
                                   AND ( kschl EQ c_zset OR
                                         kschl EQ c_zins OR
                                         kschl EQ c_zirf OR
                                         kschl EQ c_ziss ).

      CLEAR: lva_imposto_ret.

      CASE st_konv-kschl.
        WHEN c_zset. "Sest Senat

          READ TABLE ti_taxas INTO st_taxas WITH KEY lifnr = st_vtpa-lifnr
                                                     bukrs = st_header-bukrs
                                                     witht = c_ss.
          IF sy-subrc NE 0.
            lva_imposto_ret = c_ss.
          ENDIF.

        WHEN c_zins. "INSS

          READ TABLE ti_taxas INTO st_taxas WITH KEY lifnr = st_vtpa-lifnr
                                                     bukrs = st_header-bukrs
                                                     witht = c_in.
          IF sy-subrc NE 0.
            lva_imposto_ret = c_in.
          ENDIF.


        WHEN c_zirf. "IRF

          DATA(_count_witht_ic_iw) = 0.
          LOOP AT ti_taxas INTO st_taxas WHERE lifnr = st_vtpa-lifnr
                                           AND bukrs = st_header-bukrs
                                           AND ( witht = c_ic OR
                                                 witht = c_iw ).
            ADD 1 TO _count_witht_ic_iw.
          ENDLOOP.

          IF _count_witht_ic_iw > 1.
            CONCATENATE 'Cadastro do fornecedor: ' st_vtpa-lifnr 'existe mais de um imposto retido(IC e IW)!' INTO c_msg_error SEPARATED BY space.
            RETURN.
          ENDIF.

          IF lwa_lfa1_pv-stkzn EQ abap_true. "Pessoa Fisica

            READ TABLE ti_taxas INTO st_taxas WITH KEY lifnr = st_vtpa-lifnr
                                                       bukrs = st_header-bukrs
                                                       witht = c_ic.
            IF sy-subrc NE 0.
              lva_imposto_ret = c_ic.
            ENDIF.

          ELSE.

            READ TABLE ti_taxas INTO st_taxas WITH KEY lifnr = st_vtpa-lifnr
                                                       bukrs = st_header-bukrs
                                                       witht = c_iw.
            IF sy-subrc NE 0.
              lva_imposto_ret = c_iw.
            ENDIF.

          ENDIF.


        WHEN c_ziss. "ISS


          READ TABLE ti_taxas INTO st_taxas WITH KEY lifnr = st_vtpa-lifnr
                                                     bukrs = st_header-bukrs
                                                     witht = c_is.
          IF sy-subrc NE 0.
            lva_imposto_ret = c_is.
          ENDIF.

      ENDCASE.

      IF lva_imposto_ret IS NOT INITIAL.
        CONCATENATE 'Não parametrizado o imposto retido:'  lva_imposto_ret
                        'para o fornecedor: ' st_vtpa-lifnr
                        'na Empresa:' st_header-bukrs
                        'na transação XK03'
               INTO c_msg_error SEPARATED BY space.

        RETURN.
      ENDIF.

    ENDLOOP.

  ENDLOOP.


* Obter os parceiros
  LOOP AT ti_vtpa INTO st_vtpa
                  WHERE vbeln = st_dados_aux-tknum
                    AND parvw = c_pv.

    CLEAR: st_notas,
           st_header,
           st_taxas.

* Obter a nota fiscal referente ao documento de transporte
    READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados-tknum
                                      BINARY SEARCH.

* Obter a empresa da nota fiscal
    READ TABLE ti_header INTO st_header WITH KEY docnum = st_notas-docnum
                                        BINARY SEARCH.

* Obter o ponteiro do parceiro na tabela
    READ TABLE ti_taxas INTO st_taxas WITH KEY lifnr = st_vtpa-lifnr
                                               bukrs = st_header-bukrs
                                    BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    vl_tabix = sy-tabix.

* Obter todos os impostos do parceiro
    DO.
      CLEAR: st_taxas      ,
             st_withtaxdata.
      READ TABLE ti_taxas INTO st_taxas INDEX vl_tabix.

      IF ( sy-subrc NE 0 ) OR ( st_taxas-lifnr NE st_vtpa-lifnr ).
        EXIT.
      ENDIF.


      CASE p_emp_ck_imp_retido.
        WHEN '1'. "Checa com empresa da VI
          IF ( st_taxas-bukrs NE p_vi-bukrs ).
            EXIT.
          ENDIF.
        WHEN '2'. "Checa com empresa emissora do CT-e
          IF ( st_taxas-bukrs NE st_header-bukrs ).
            EXIT.
          ENDIF.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

* Obter o valor base do imposto
      IF st_taxas-witht = c_ss.
        vl_kschl  = c_zsen.
        vl_kschl2 = c_zset.
      ELSEIF st_taxas-witht = c_in.
        vl_kschl  = c_zinn.
        vl_kschl2 = c_zins.
      ELSEIF st_taxas-witht = c_ic.
        "vl_kschl  = c_zred.
        vl_kschl2 = c_zirf.
      ELSEIF st_taxas-witht = c_iw.
        vl_kschl  = c_zbir.
        vl_kschl2 = c_zirf.
      ELSEIF st_taxas-witht = c_is.
        vl_kschl  = c_zfre.
        vl_kschl2 = c_ziss.
      ELSE.
        ADD 1 TO vl_tabix.
        CONTINUE.
      ENDIF.
* Obter a base do imposto
      CLEAR: vl_base, vl_imposto.

      IF st_taxas-witht EQ c_ic.
        SELECT SINGLE * INTO wa_zlest0023
         FROM zlest0023
        WHERE bukrs EQ st_header-bukrs
          AND tknum EQ st_dados_aux-tknum.

        IF sy-subrc IS INITIAL.
          vl_base = wa_zlest0023-baseirrf.
        ENDIF.
      ELSE.
        READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                                 kschl = vl_kschl
                           BINARY SEARCH.
        IF sy-subrc EQ 0.
          vl_base = st_konv-kwert.
        ENDIF.
      ENDIF.
* Obter o valor do imposto
      READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                               kschl = vl_kschl2
                         BINARY SEARCH.
      IF sy-subrc EQ 0.

        vl_imposto = st_konv-kwert.
        IF vl_imposto < 0.
          MULTIPLY vl_imposto BY -1.
        ENDIF.

*      vl_base = st_vfkp-netwr * ( st_taxas-qproz / 100 ).

        st_withtaxdata-split_key             = c_000001.
        st_withtaxdata-wi_tax_type           = st_taxas-witht.
        st_withtaxdata-wi_tax_code           = st_taxas-wt_withcd.
        st_withtaxdata-wi_tax_base           = vl_base.
        st_withtaxdata-wi_tax_amt            = vl_imposto.
        st_withtaxdata-wi_tax_withheld_amt   = vl_imposto.
        APPEND st_withtaxdata TO t_withtaxdata.

      ENDIF.
* Incrementar 1 no ponteiro
      ADD 1 TO vl_tabix.

    ENDDO.

    CLEAR st_vtpa.

  ENDLOOP.

ENDFORM.                    " Z_MONTAR_TAXAS
*&---------------------------------------------------------------------*
*&      Form  Z_INICIALIZAR_AREAS
*&---------------------------------------------------------------------*
* Limpar as tabelas e estruturas utilizadas pela bapi.
*----------------------------------------------------------------------*
FORM z_inicializar_areas .

  CLEAR: st_headerdata,
         st_vfkp,
         st_vfkp_acum,
         st_vtpa,
         st_konv,
         st_withtaxdata.
  REFRESH: ti_itemdata,
           ti_glaccountdata,
           ti_return,
           ti_withtaxdata.

  CLEAR: vobj_keypd,
         vobj_keysg,
         vobj_keysu.
ENDFORM.                    " Z_INICIALIZAR_AREAS
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZAR_ZLEST0032
*&---------------------------------------------------------------------*
* Atualizar a tabela do banco de dados (zlest0032) com o número e ano da
* revisão de fatura criada.
*----------------------------------------------------------------------*
FORM z_atualizar_zlest0032 .

  UPDATE zlest0032 SET: belnr      = vg_invoicedocnumber_miro
                        gjahr      = vg_ano_miro
                        docnum     = vg_docnum
*-CS2020000460 - 25.08.2021 - JT - inicio
                        belnr_est  = abap_off
                        gjahr_est  = abap_off
                        belnr_comp = abap_off
                        gjahr_comp = abap_off
                        docnum_est = abap_off
*-CS2020000460 - 25.08.2021 - JT - fim
                 WHERE tknum = st_dados_aux-tknum.

ENDFORM.                    " Z_ATUALIZAR_ZLEST0032
*&---------------------------------------------------------------------*
*&      Form  Z_LISTAR_INCONSISTENCIAS
*&---------------------------------------------------------------------*
* Apresentar um relatório com as mensagens de erro gerado pela BAPI.
*----------------------------------------------------------------------*
FORM z_listar_inconsistencias .

  WRITE: /001 st_dados-tknum,
          013 st_dados-fknum,
          025 st_dados-ebeln,
          037 st_dados-ebelp,
          043 st_dados-lblni,
          055 st_return-message(55).
  IF st_return-message+55(55) IS NOT INITIAL.
    WRITE /055 st_return-message+55(55).
  ENDIF.
  IF st_return-message+110(55) IS NOT INITIAL.
    WRITE /055 st_return-message+110(55).
  ENDIF.
  IF st_return-message+165(55) IS NOT INITIAL.
    WRITE /055 st_return-message+165(55).
  ENDIF.

ENDFORM.                    " Z_LISTAR_INCONSISTENCIAS

*&---------------------------------------------------------------------*
* Impressão do cabeçalho do relatório.
TOP-OF-PAGE.

  WRITE: 001 sy-repid,
         093 TEXT-001, sy-datum,   "Hora
        /040 TEXT-002,             "Lista de erros
         093 TEXT-003, sy-uzeit,   "hora
        /093 TEXT-004, sy-pagno USING EDIT MASK 'RR___'.   "Página

  SKIP 2.

  WRITE: 001 TEXT-005,      "Doc.Transp.
         013 TEXT-006,      "Custo Frete
         025 TEXT-007,      "Doc.Compras
         037 TEXT-008,      "Item
         043 TEXT-011,      "Fl.Serviço
         055 TEXT-009,      "Erro
        /001 '----------- ----------- ----------- ----- -----------',
         055 '------------------------------------------------------'.
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_BAPI_NF
*&---------------------------------------------------------------------*
* Montar bapi de criação de nota fiscal
*----------------------------------------------------------------------*
FORM z_montar_bapi_nf USING p_iva TYPE mwskz.

  DATA: vl_dstcat    TYPE zlest0030-dstcat,
        vl_industry  TYPE zlest0030-industry,
        vl_cfop      TYPE zlest0030-cfop,
        vl_docnum    TYPE bapi_j_1bnfdoc-docnum,
        st_nfcheck   TYPE bapi_j_1bnfcheck,
        vl_parid     TYPE char10,
        wa_zlest0038 TYPE zlest0038,
        p_data_ent   TYPE datum,
        p_data_val   TYPE datum,
        wl_lfa1_nfe  TYPE lfa1,
        wl_zib_nfe   TYPE zib_nfe_forn.


  DATA: sl_zlest0040 TYPE zlest0040.

  DATA: wa_setleaf TYPE setleaf,
        it_setleaf LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.


* Limpar área de trabalho e tabelas internas.
  CLEAR: st_obj_header,
         st_obj_item  ,
         st_notas     ,
         st_header    ,
         vl_parid     .

  REFRESH: ti_obj_item,
           ti_obj_item_tax,
           ti_obj_partner,
           ti_obj_ot_partner,
           ti_return.

* Obter a nota fiscal que está associada ao documento de transporte.
  READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados_aux-tknum
                                    BINARY SEARCH.

* Obter os dados do cabeçalho da nota fiscal
  READ TABLE ti_header INTO st_header WITH KEY docnum = st_notas-docnum
                                      BINARY SEARCH.

* Importante ===> A nota de entrada que será criada dever ser idêntica
* a nota de saída apenas trocando-se alguns campos.

* Alimentar a estrutura do cabeçalho da nota
  MOVE-CORRESPONDING st_header TO st_obj_header.

  st_obj_header-pstdat = sy-datum. "ALRS

  "Opter Área de contabilidade de custos
  SELECT * INTO TABLE it_setleaf
    FROM setleaf
   WHERE setname EQ 'CTE_AVULSO'.

  SELECT SINGLE * INTO wa_zlest0038
    FROM zlest0038
   WHERE nftype_s EQ st_header-nftype.

  CHECK sy-subrc IS INITIAL.

  st_obj_header-nftype  = wa_zlest0038-nftype_e.
  st_obj_header-doctyp  = c_1.
  st_obj_header-direct  = c_1.
  st_obj_header-docstat = space.

*01 01  0001  Rodoviario
*02 02  0002  Ferroviário
*03 03  0003  Navegação fluvial
*04 04  0004  Marítimo
*05 05  0005  Aéreo
*06 06  0006  Correio, serv.postal
*07 07  0007  Multimodal
*
*1 Transporte rodoviário
*2 Transporte aéreo
*3 Via marítima
*4 Transporte ferroviário
*5 Pipeline


  "Não encontrado Modo de transporte para Tipo de expedição &1 (VT)!
  CASE st_dados_aux-vsart.
    WHEN '01'.
      st_obj_header-transp_mode = '1'.
    WHEN '02'.
      st_obj_header-transp_mode = '4'.
    WHEN '03' OR '04'.
      st_obj_header-transp_mode = '3'.
    WHEN OTHERS.
      MESSAGE e147(zles) WITH st_dados_aux-vsart.
  ENDCASE.

  p_data_ent  = st_obj_header-pstdat.

  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = p_data_ent
      p_bukrs        = st_obj_header-bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    IMPORTING
      p_data_val     = p_data_val
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  st_obj_header-pstdat  = p_data_val.

* Alimentar a estrutura do parceiros da nota (somente o parceiro LF é necessário)
  st_obj_partner-mandt  = sy-mandt.
  st_obj_partner-parvw  = c_lf.
  st_obj_partner-parid  = st_dados_aux-tdlnr.
  st_obj_partner-partyp = c_v.
  APPEND st_obj_partner TO ti_obj_partner.

  SELECT SINGLE * FROM lfa1 INTO wl_lfa1_nfe WHERE lifnr EQ st_dados_aux-tdlnr.

  IF ( sy-subrc EQ 0 ).

    SELECT SINGLE * FROM zib_nfe_forn INTO wl_zib_nfe WHERE nu_chave_numero EQ st_header-nfenum
                                                        AND nu_chave_cnpj   EQ wl_lfa1_nfe-stcd1
                                                        AND nu_chave_modelo EQ '57'.

    st_obj_header-access_key = wl_zib_nfe-nu_chave.
    st_obj_header-docstat    = wl_zib_nfe-st_nota.


  ENDIF.

* Alimentar a tabela com os itens da nota
  LOOP AT ti_item INTO st_item
                  WHERE docnum = st_notas-docnum.


    MOVE-CORRESPONDING st_item TO st_obj_item. "#EC CI_FLDEXT_OK[2215424]

    IF vl_parid IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = st_obj_item-werks
        IMPORTING
          output = vl_parid.
    ENDIF.

* Leis Fiscais para entrada de frete próprio
    CASE st_dados_aux-add03+9(1).
      WHEN: '1'.
        SELECT SINGLE * FROM zlest0040 INTO sl_zlest0040 WHERE iva EQ p_iva AND fatura EQ 'P'.
        st_obj_item-taxlw1 = sl_zlest0040-icms.
        st_obj_item-taxlw2 = sl_zlest0040-ipi.
        st_obj_item-taxlw4 = sl_zlest0040-cofins.
        st_obj_item-taxlw5 = sl_zlest0040-pis.
    ENDCASE.

    st_obj_item-bwkey  = st_obj_header-parid+06(04).
    st_obj_item-werks  = st_obj_item-bwkey.
    st_obj_item-refkey = vg_invoicedocnumber_miro.
    st_obj_item-reftyp = c_li.                      "Logística: Revisão de Faturas
    st_obj_item-refitm = c_000001.
    st_obj_item-maktx  = c_miro_frete_proprio.
* Obter o cfop de entrada a partir do cfop de saída
    CLEAR: vl_dstcat, vl_industry.
    CALL FUNCTION 'CONVERSION_EXIT_CFOBR_INPUT'
      EXPORTING
        input  = st_item-cfop
      IMPORTING
        output = vl_cfop.

    DATA(_cfop_uf_dif_prest) = abap_false.

    SELECT SINGLE *
      FROM zlest0030 INTO @DATA(lwa_zlest0030_aux)
     WHERE cfop_uf_emit_dif_prest EQ @vl_cfop.

    IF ( sy-subrc EQ 0 ) AND ( vl_cfop IS NOT INITIAL ).
      _cfop_uf_dif_prest = abap_true.
    ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 26.07.23 - 117757
    DATA: vl_vbeln    TYPE vbak-vbeln,
          vl_bukrs_vf TYPE vbak-bukrs_vf,
          vl_vkaus    TYPE vbap-vkaus,
          vl_werks    TYPE vbap-werks,
          vl_tdlnr    TYPE tdlnr.

    SELECT SINGLE vbeln bukrs_vf
           INTO (vl_vbeln , vl_bukrs_vf)
           FROM vbak
           WHERE tknum EQ st_dados_aux-tknum.

    IF sy-subrc EQ 0.
      "ALRS 21.12.2023
      vl_bukrs_vf = st_headerdata-comp_code.
      SELECT SINGLE vkaus werks
             INTO (vl_vkaus, vl_werks)
             FROM vbap
             WHERE vbeln EQ vl_vbeln
               AND posnr EQ '000010'.


      IF vl_werks IS NOT INITIAL.
        vl_tdlnr = vl_werks.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vl_tdlnr
          IMPORTING
            output = vl_tdlnr.
      ENDIF.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 26.07.23 - 117757

    IF _cfop_uf_dif_prest EQ abap_true.
*Inicio Alteração - Leandro Valentim Ferreira - 26.07.23 - 117757
***      SELECT dstcat industry INTO (vl_dstcat, vl_industry)
***                             FROM zlest0030
***                         UP TO 1 ROWS
***                            WHERE direct                 EQ c_2
***                              AND cfop_uf_emit_dif_prest EQ vl_cfop
***                              AND tpparceiro             EQ c_0.
***      ENDSELECT.


      SELECT SINGLE dstcat industry
               INTO (vl_dstcat, vl_industry)
               FROM zlest0030
              WHERE direct                 EQ c_2
                AND vkaus                  EQ vl_vkaus
                AND tdlnr                  EQ vl_tdlnr
                AND bukrs                  EQ vl_bukrs_vf
                AND cfop_uf_emit_dif_prest EQ vl_cfop
                AND tpparceiro             EQ c_0.

      IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
        SELECT SINGLE dstcat industry
                 INTO (vl_dstcat, vl_industry)
                 FROM zlest0030
                WHERE direct                 EQ c_2
                  AND vkaus                  EQ vl_vkaus
                  AND tdlnr                  EQ vl_tdlnr
                  AND bukrs                  EQ space
                  AND cfop_uf_emit_dif_prest EQ vl_cfop
                  AND tpparceiro             EQ c_0.


        IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
          SELECT SINGLE dstcat industry
                   INTO (vl_dstcat, vl_industry)
                   FROM zlest0030
                  WHERE direct                 EQ c_2
                    AND vkaus                  EQ space
                    AND tdlnr                  EQ vl_tdlnr
                    AND bukrs                  EQ vl_bukrs_vf
                    AND cfop_uf_emit_dif_prest EQ vl_cfop
                    AND tpparceiro             EQ c_0.

          IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
            SELECT SINGLE dstcat industry
                     INTO (vl_dstcat, vl_industry)
                     FROM zlest0030
                    WHERE direct                 EQ c_2
                      AND vkaus                  EQ space
                      AND tdlnr                  EQ vl_tdlnr
                      AND bukrs                  EQ space
                      AND cfop_uf_emit_dif_prest EQ vl_cfop
                      AND tpparceiro             EQ c_0.
**========================================================LES - ZLES0040 - docnum de entrada com CFOP #137539 AOENNING
            IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
              SELECT SINGLE dstcat industry
                       INTO (vl_dstcat, vl_industry)
                       FROM zlest0030
                      WHERE direct                 EQ c_2
                        AND vkaus                  EQ vl_vkaus
                        AND tdlnr                  EQ space
                        AND bukrs                  EQ space
                        AND cfop_uf_emit_dif_prest EQ vl_cfop
                        AND tpparceiro             EQ c_0.
**========================================================LES - ZLES0040 - docnum de entrada com CFOP #137539 AOENNING

* RJF - Ini - 2024.01.09 - ZLES0040  - Erro ao determinar CFOP - 130835
              IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
                SELECT SINGLE dstcat industry
                         INTO (vl_dstcat, vl_industry)
                         FROM zlest0030
                        WHERE direct                 EQ c_2
                          AND vkaus                  EQ space
                          AND tdlnr                  EQ space
                          AND bukrs                  EQ space
                          AND cfop_uf_emit_dif_prest EQ vl_cfop
                          AND tpparceiro             EQ c_0.

                IF sy-subrc IS NOT INITIAL.
                  MESSAGE s024(sd) WITH 'Não localizado CFOP do doc. de saída.'.
                  CONTINUE.
                ENDIF.

              ENDIF.
* RJF - Fim - 2024.01.09 - ZLES0040  - Erro ao determinar CFOP - 130835

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 26.07.23 - 117757
    ELSE.
*inicio alteração - leandro valentim ferreira - 26.07.23 - 117757
***      SELECT dstcat industry INTO (vl_dstcat, vl_industry)
***                             FROM zlest0030
***                         UP TO 1 ROWS
***                            WHERE direct     EQ c_2
***                              AND cfop       EQ vl_cfop
***                              AND tpparceiro EQ c_0.
***      ENDSELECT.

      SELECT SINGLE dstcat industry
               INTO (vl_dstcat, vl_industry)
               FROM zlest0030
              WHERE direct                 EQ c_2
                AND vkaus                  EQ vl_vkaus
                AND tdlnr                  EQ vl_tdlnr
                AND bukrs                  EQ vl_bukrs_vf
                AND cfop                   EQ vl_cfop
                AND tpparceiro             EQ c_0.

      IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
        SELECT SINGLE dstcat industry
                 INTO (vl_dstcat, vl_industry)
                 FROM zlest0030
                WHERE direct                 EQ c_2
                  AND vkaus                  EQ vl_vkaus
                  AND tdlnr                  EQ vl_tdlnr
                  AND bukrs                  EQ space
                  AND cfop                   EQ vl_cfop
                  AND tpparceiro             EQ c_0.


        IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
          SELECT SINGLE dstcat industry
                   INTO (vl_dstcat, vl_industry)
                   FROM zlest0030
                  WHERE direct                 EQ c_2
                    AND vkaus                  EQ space
                    AND tdlnr                  EQ vl_tdlnr
                    AND bukrs                  EQ vl_bukrs_vf
                    AND cfop                   EQ vl_cfop
                    AND tpparceiro             EQ c_0.

          IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
            SELECT SINGLE dstcat industry
                     INTO (vl_dstcat, vl_industry)
                     FROM zlest0030
                    WHERE direct                 EQ c_2
                      AND vkaus                  EQ space
                      AND tdlnr                  EQ vl_tdlnr
                      AND bukrs                  EQ space
                      AND cfop                   EQ vl_cfop
                      AND tpparceiro             EQ c_0.
**========================================================LES - ZLES0040 - docnum de entrada com CFOP #137539 AOENNING
            IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
              SELECT SINGLE dstcat industry
                       INTO (vl_dstcat, vl_industry)
                       FROM zlest0030
                      WHERE direct                 EQ c_2
                        AND vkaus                  EQ vl_vkaus
                        AND tdlnr                  EQ space
                        AND bukrs                  EQ space
                        AND cfop                   EQ vl_cfop
                        AND tpparceiro             EQ c_0.
**========================================================LES - ZLES0040 - docnum de entrada com CFOP #137539 AOENNING
* RJF - Ini - 2024.01.09 - ZLES0040  - Erro ao determinar CFOP - 130835
              IF vl_dstcat IS INITIAL AND vl_industry IS INITIAL.
                SELECT SINGLE dstcat industry
                         INTO (vl_dstcat, vl_industry)
                         FROM zlest0030
                        WHERE direct                 EQ c_2
                          AND vkaus                  EQ space
                          AND tdlnr                  EQ space
                          AND bukrs                  EQ space
                          AND cfop                   EQ vl_cfop
                          AND tpparceiro             EQ c_0.

                IF sy-subrc IS NOT INITIAL.
                  MESSAGE s024(sd) WITH 'Não localizado CFOP do doc. de saída.'.
                  CONTINUE.
                ENDIF.
              ENDIF.
* RJF - Fim - 2024.01.09 - ZLES0040  - Erro ao determinar CFOP - 130835

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 26.07.23 - 117757
    ENDIF.

    IF sy-subrc IS INITIAL.
      CLEAR vl_cfop.

      SELECT SINGLE * INTO lwa_zlest0030_aux
        FROM zlest0030
       WHERE direct     EQ c_1
         AND dstcat     EQ vl_dstcat
         AND industry   EQ vl_industry
         AND tpparceiro EQ c_0
         AND tdlnr      EQ st_obj_header-parid
         AND bukrs      EQ st_obj_header-bukrs.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE * INTO lwa_zlest0030_aux
          FROM zlest0030
         WHERE direct     EQ c_1
           AND dstcat     EQ vl_dstcat
           AND industry   EQ vl_industry
           AND tpparceiro EQ c_0
           AND tdlnr      EQ st_obj_header-parid
           AND bukrs      EQ space.

        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE * INTO lwa_zlest0030_aux
            FROM zlest0030
           WHERE direct     EQ c_1
             AND dstcat     EQ vl_dstcat
             AND industry   EQ vl_industry
             AND tpparceiro EQ c_0
             AND tdlnr      EQ space
             AND bukrs      EQ st_obj_header-bukrs.

          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE * INTO lwa_zlest0030_aux
              FROM zlest0030
             WHERE direct     EQ c_1
               AND dstcat     EQ vl_dstcat
               AND industry   EQ vl_industry
               AND tpparceiro EQ c_0
               AND tdlnr      EQ space
               AND bukrs      EQ space.
          ENDIF.
        ENDIF.
      ENDIF.

      IF sy-subrc EQ 0.
        IF _cfop_uf_dif_prest EQ abap_true.
          vl_cfop = lwa_zlest0030_aux-cfop_uf_emit_dif_prest.
        ELSE.
          vl_cfop = lwa_zlest0030_aux-cfop.
        ENDIF.
      ENDIF.

**========================================================LES - ZLES0040 - docnum de entrada com CFOP #137539 AOENNING
      "Adicionando validação.
      IF vl_cfop IS INITIAL.
        MESSAGE s024(sd) WITH 'Não localizado CFOP para doc. de entrada.'.
        CONTINUE.
      ENDIF.
**========================================================LES - ZLES0040 - docnum de entrada com CFOP #137539 AOENNING

    ENDIF.
* O campo CFOP correto é o CFOP_10, o campo CFOP deve ser inicializado pois seu
* tamanho é de 5 posições e foi preenchido no move-corresponding logo acima.
    CLEAR st_obj_item-cfop.
    st_obj_item-cfop_10 = vl_cfop.

    APPEND st_obj_item TO ti_obj_item.
    CLEAR st_item.
  ENDLOOP.

* Alimentar a tabela com os impostos
  LOOP AT ti_item_tax INTO st_item_tax
                  WHERE docnum = st_notas-docnum.
    MOVE-CORRESPONDING st_item_tax TO st_obj_item_tax.

    IF st_item_tax-taxgrp = 'ICMS'.
      DATA(lv_diferimento) = space.
      PERFORM f_get_diferimento USING st_dados_aux CHANGING lv_diferimento.
      IF lv_diferimento EQ abap_true.
        st_obj_item_tax-othbas = st_obj_item_tax-base.
        CLEAR: st_obj_item_tax-base, st_obj_item_tax-rate, st_obj_item_tax-taxval, st_obj_item_tax-excbas.
      ENDIF.
    ENDIF.

    APPEND st_obj_item_tax TO ti_obj_item_tax.
  ENDLOOP.

  st_nfcheck-chekcon  = c_x.
  st_obj_header-parvw = 'LF'.
  st_obj_header-partyp = 'V'.
  CLEAR st_obj_header-form.
  st_obj_header-parid = vl_parid.

  st_obj_header-branch = st_obj_item-werks.
  st_obj_header-manual = 'X'.

  SELECT SINGLE bukrs FROM j_1bbranch INTO st_obj_header-bukrs WHERE  branch = st_obj_header-branch.

  CLEAR vg_docnum.

* Executar a bapi de criação da nota fiscal
  CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      obj_header     = st_obj_header
      nfcheck        = st_nfcheck
    IMPORTING
      e_docnum       = vg_docnum
    TABLES
      obj_partner    = ti_obj_partner
      obj_item       = ti_obj_item
      obj_item_tax   = ti_obj_item_tax
      obj_ot_partner = ti_obj_ot_partner
      return         = ti_return.

  IF vg_docnum IS INITIAL.
    ROLLBACK WORK.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    READ TABLE ti_header_nfe INTO st_header_nfe WITH KEY docnum = st_notas-docnum
                                                BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      SELECT SINGLE * INTO st_header_nfe_aux
        FROM j_1bnfe_active
       WHERE docnum EQ vg_docnum.

      IF sy-subrc IS INITIAL.

        st_header_nfe_aux-authcod = st_header_nfe-authcod.
        st_header_nfe_aux-docnum9 = st_header_nfe-docnum9.
        st_header_nfe_aux-docsta  = st_header_nfe-docsta.
        st_header_nfe_aux-cdv     = st_header_nfe-cdv.

        CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
          EXPORTING
            i_acttab  = st_header_nfe_aux
            i_updmode = 'U'.

      ENDIF.

    ENDIF.

* Listar todas as mensagens de erro.
    LOOP AT ti_return INTO st_return
         WHERE type NE 'S'.
      PERFORM z_listar_inconsistencias.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " Z_MONTAR_BAPI_NF
*&---------------------------------------------------------------------*
*&      Form  GERAR_MIRO_Z026
*&---------------------------------------------------------------------*
FORM gerar_miro_z026 .

  CLEAR: vg_invoicedocnumber_miro,
         vg_ano_miro,
         ti_return,
         vg_docnum.

* Criar o documento de revisão de fatura através da bapi da MIRO - Z026
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata       = st_headerdata
    IMPORTING
      invoicedocnumber = vg_invoicedocnumber_miro
      fiscalyear       = vg_ano_miro
    TABLES
      itemdata         = ti_itemdata
      glaccountdata    = ti_glaccountdata
      withtaxdata      = ti_withtaxdata
      return           = ti_return.

  IF ti_return[] IS NOT INITIAL.
    LOOP AT ti_return INTO st_return.
      PERFORM z_listar_inconsistencias.
      ROLLBACK WORK.
    ENDLOOP.
  ELSE.
    "Executa caso SHTYP for igual Z026 e a miro criada.
    IF ( vg_ano_miro IS NOT INITIAL ).
      PERFORM z_atualizar_zlest0032.
      PERFORM z_executar_comit.
    ENDIF.
  ENDIF.


ENDFORM.                    " GERAR_MIRO_Z026
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_EXEC_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_montar_exec_bapi CHANGING vobj_key.

  DATA: lit_withtax TYPE TABLE OF bapi_incinv_create_withtax.


  DATA:
    vbudat TYPE sy-datum,
    vbldat TYPE sy-datum,
    vname1 TYPE lfa1-name1,
    vbase  TYPE zib_contabil-wrbtr.

  DATA: vg_awtyp LIKE bkpf-awtyp,
        vg_awkey LIKE bkpf-awkey,
        vg_awsys LIKE bkpf-awsys,
        vg_belnr LIKE bkpf-belnr,
        vg_bukrs LIKE bkpf-bukrs,
        vg_gjahr LIKE bkpf-gjahr,
        vg_total TYPE zib_contabil-wrbtr,
        vg_resto TYPE zib_contabil-wrbtr,
        vg_difer TYPE zib_contabil-wrbtr.

  REFRESH:  it_criteria,
            it_accountgl,
            it_receivable,
            it_payable,
            it_accounttax,
            it_currencyamount,
            it_extension1,
            it_bapiret,
            it_accountwt.

  CLEAR: vg_total,vg_resto,vname1.
  "Acerta arredondamento
  LOOP AT it_zib_contabil_sub INTO st_zib_contabil WHERE sgtxt(13) NE 'INSS PATRONAL'.
    "Conta1 Fornecedor
    IF st_zib_contabil-sgtxt+0(5) = 'TOTAL'.
      ADD st_zib_contabil-wrbtr TO vg_total.
      SELECT SINGLE name1 FROM lfa1
        INTO vname1
        WHERE lifnr = st_zib_contabil-hkont.
      CONCATENATE st_zib_contabil-sgtxt '-' vname1 INTO st_zib_contabil-sgtxt.
      MODIFY it_zib_contabil_sub FROM st_zib_contabil INDEX sy-tabix TRANSPORTING sgtxt.
    ELSEIF st_zib_contabil-sgtxt+0(7) = 'LIQUIDO'.
      ADD st_zib_contabil-wrbtr TO vg_resto.
      CONCATENATE st_zib_contabil-sgtxt '-' vname1 INTO st_zib_contabil-sgtxt.
      MODIFY it_zib_contabil_sub FROM st_zib_contabil INDEX sy-tabix TRANSPORTING sgtxt.
    ELSE.
      ADD st_zib_contabil-wrbtr TO vg_resto.
    ENDIF.
  ENDLOOP.

  vg_difer = vg_total - vg_resto.

  IF vg_difer NE 0.
    "Acerta arredondamento
    LOOP AT it_zib_contabil_sub INTO st_zib_contabil.
      IF  st_zib_contabil-sgtxt+0(6) = 'COFINS'.
        ADD vg_difer TO  st_zib_contabil-wrbtr.
        MODIFY it_zib_contabil_sub FROM st_zib_contabil INDEX sy-tabix TRANSPORTING wrbtr.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  READ TABLE ti_vbak INTO st_vbak WITH KEY tknum = st_dados_aux-tknum BINARY SEARCH.
  READ TABLE ti_vbap INTO st_vbap WITH KEY vbeln = st_vbak-vbeln BINARY SEARCH.

* Informações fixas - pré definadas para utilização da BAPI
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = gd_documentheader-obj_sys.

  READ TABLE it_zib_contabil_sub INTO st_zib_contabil WITH KEY bschl = '31'. "FORNECEDOR
  vbase = st_zib_contabil-wrbtr.
  SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ st_zib_contabil-hkont. " Dados do fornecedor


*  CONCATENATE ST_ZIB_CONTABIL-BUDAT+6(4) ST_ZIB_CONTABIL-BUDAT+3(2) ST_ZIB_CONTABIL-BUDAT+0(2) INTO VBUDAT.


  vbudat = sy-datum. "Alterando para data atual data de lançamento. (AOENNING).
  CONCATENATE st_zib_contabil-bldat+6(4) st_zib_contabil-bldat+3(2) st_zib_contabil-bldat+0(2) INTO vbldat.

*> OBJ_TYPE has to be replaced by customers object key (Y* or Z*)
  gd_documentheader-obj_type   = 'IDOC'.
  gd_documentheader-username   = sy-uname.

  IF c_sub EQ abap_true.
    gd_documentheader-pstng_date = vbudat.
  ENDIF.

  IF c_sub NE abap_true.
    IF ( vbldat < vbudat ) AND vbldat+4(2) =  vbudat+4(2). "mesmo mes
      gd_documentheader-pstng_date = vbldat.
    ELSE.
      gd_documentheader-pstng_date = vbudat.
    ENDIF.
  ENDIF.


*  "ALRS
*  GD_DOCUMENTHEADER-PSTNG_DATE = VBUDAT.

  gd_documentheader-bus_act    = 'RFBU'.

  gd_documentheader-obj_key    =  st_zib_contabil-obj_key.
  gd_documentheader-header_txt = 'SUBCONTRATADO'.
  gd_documentheader-comp_code  = st_zib_contabil-bukrs.

  gd_documentheader-doc_date   = vbldat.
  gd_documentheader-fisc_year  = st_zib_contabil-gjahr.

  IF c_sub NE abap_true.
    gd_documentheader-fis_period = vbudat.
  ENDIF.

  IF c_sub NE abap_true.
    IF ( vbldat < vbudat ) AND vbldat+4(2) =  vbudat+4(2). "mesmo mes.
      gd_documentheader-fis_period = vbldat+4(2).
    ELSE.
      gd_documentheader-fis_period = st_zib_contabil-monat.
    ENDIF.
  ENDIF.


*  "ALRS
*  GD_DOCUMENTHEADER-FIS_PERIOD = ST_ZIB_CONTABIL-MONAT.

  gd_documentheader-doc_type   = st_zib_contabil-blart.
  gd_documentheader-ref_doc_no = ''.

  LOOP AT it_zib_contabil_sub INTO st_zib_contabil.
    "Conta1 Fornecedor
    IF st_zib_contabil-sgtxt+0(5) = 'TOTAL'.
      CLEAR wa_payable.
      wa_payable-itemno_acc    = 1.
      wa_payable-vendor_no     = st_zib_contabil-hkont.

      wa_payable-ref_key_1     = ''.
      wa_payable-ref_key_2     = ''.
      wa_payable-ref_key_3     = ''.
      wa_payable-bline_date    = vbudat.
      wa_payable-pymt_meth     = 'U'.
      wa_payable-pmnt_block    = 'A'.
      wa_payable-alloc_nmbr    = st_zib_contabil-zuonr.
      wa_payable-item_text     = st_zib_contabil-sgtxt.
      wa_payable-sp_gl_ind     = ''.
      wa_payable-bus_area      = st_zib_contabil-gsber.
      wa_payable-partner_bk    = st_zib_contabil-bvtyp.
      wa_payable-bank_id       = st_zib_contabil-hbkid.
      APPEND wa_payable TO it_payable.

      " Moeda fornecedor
      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 1.
      wa_currencyamount-curr_type = '00'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr * -1.
      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 1.
      wa_currencyamount-curr_type = '10'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr * -1.
      APPEND wa_currencyamount TO it_currencyamount.

*---------------------------------------------------------------------------------------------------------*
*     Impostos Retidos
*---------------------------------------------------------------------------------------------------------*
      CLEAR: lit_withtax[].

      PERFORM z_montar_taxas TABLES lit_withtax
                              USING st_vfkp
                                    '2' "Checagem de Impostos da Empresa Emissora do CT-e
                           CHANGING gva_msg_error.

      IF gva_msg_error IS NOT INITIAL.
        WRITE: /001 st_dados-tknum,
           013 st_dados_aux-fknum,
           025 st_dados_aux-ebeln,
           037 st_dados_aux-ebelp,
           043 st_dados_aux-lblni,
           055 gva_msg_error(55).
        IF gva_msg_error+55(55) IS NOT INITIAL.
          WRITE /055 gva_msg_error+55(55).
        ENDIF.
        IF gva_msg_error+110(55) IS NOT INITIAL.
          WRITE /055 gva_msg_error+110(55).
        ENDIF.
        IF gva_msg_error+165(55) IS NOT INITIAL.
          WRITE /055 gva_msg_error+165(55).
        ENDIF.

        RETURN.
      ENDIF.

      LOOP AT lit_withtax INTO DATA(lwa_with_tax).
        CLEAR: wa_criteria.

        wa_criteria-itemno_acc    = wa_payable-itemno_acc.
        wa_criteria-wt_type       = lwa_with_tax-wi_tax_type.
        wa_criteria-wt_code       = lwa_with_tax-wi_tax_code.
        wa_criteria-bas_amt_lc    = lwa_with_tax-wi_tax_base.
        wa_criteria-bas_amt_tc    = lwa_with_tax-wi_tax_base.
        wa_criteria-man_amt_lc    = lwa_with_tax-wi_tax_amt.
        wa_criteria-man_amt_tc    = lwa_with_tax-wi_tax_amt.
        wa_criteria-bas_amt_ind   = abap_true.
        wa_criteria-man_amt_ind   = abap_true.

        APPEND wa_criteria TO it_accountwt.
      ENDLOOP.

*---------------------------------------------------------------------------------------------------------*
*     Impostos Retidos - Fim
*---------------------------------------------------------------------------------------------------------*

      CONTINUE.
    ENDIF.

    "CONTA 2 Resultado
    IF st_zib_contabil-sgtxt+0(7) = 'LIQUIDO'.
      CLEAR wa_accountgl.
      wa_accountgl-costcenter     = st_zib_contabil-kostl.
      wa_accountgl-itemno_acc     = 2.
      wa_accountgl-gl_account     = st_zib_contabil-hkont.
      wa_accountgl-ref_key_1      = ''.
      wa_accountgl-ref_key_2      = ''.
      wa_accountgl-ref_key_3      = ''.
      wa_accountgl-acct_type      = 'S'.
      wa_accountgl-item_text      = st_zib_contabil-sgtxt.
      wa_accountgl-bus_area       = st_zib_contabil-gsber.
      wa_accountgl-alloc_nmbr     = st_zib_contabil-zuonr.
      wa_accountgl-tax_code       = 'I0'.
      wa_accountgl-doc_type       = st_zib_contabil-blart.
      wa_accountgl-taxjurcode     = wa_lfa1-txjcd.
      wa_accountgl-trade_id       = wa_lfa1-vbund.
      wa_accountgl-cs_trans_t     = ''.
      wa_accountgl-orderid        = ''.
      wa_accountgl-itemno_tax     = 2.

      APPEND wa_accountgl TO it_accountgl.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'ARTNR'.
*---> 13/06/2023 - Migração S4 - JS
*            it_criteria-character     = st_vbap-matnr.
      it_criteria-character = CONV #( st_vbap-matnr ).
*<--- 13/06/2023 - Migração S4 - JS

      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'KNDNR'.
      it_criteria-character     = st_vbak-kunnr.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'FKART'.
      it_criteria-character     = st_vbak-fkara.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'KAUFN'.
      it_criteria-character     = st_vbak-vbeln.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'BUKRS'.
      it_criteria-character     = st_vbak-bukrs_vf.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'VKORG'.
      it_criteria-character     = st_vbak-vkorg.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'VTWEG'.
      it_criteria-character     = st_vbak-vtweg.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'SPART'.
      it_criteria-character     = st_vbak-spart.
      APPEND it_criteria.

      it_criteria-itemno_acc    = 2.
      it_criteria-fieldname     = 'WERKS'.
      it_criteria-character     = st_vbap-werks.
      APPEND it_criteria.

      " Moeda Resultado
      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 2.
      wa_currencyamount-curr_type = '00'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.
      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 2.
      wa_currencyamount-curr_type = '10'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.
      APPEND wa_currencyamount TO it_currencyamount.

      CONTINUE.
    ENDIF.

    IF st_zib_contabil-sgtxt+0(6) = 'COFINS'.
      "Cofins
      it_accounttax-itemno_acc  = 3.
      it_accounttax-gl_account  = st_zib_contabil-hkont. " Cofins
      it_accounttax-cond_key    = 'ICOV'.
      it_accounttax-acct_key    = 'CO3'.
      it_accounttax-tax_rate    = st_zib_contabil-rate.
      it_accounttax-tax_code    = 'I0'.
      it_accounttax-taxjurcode  = wa_lfa1-txjcd.
      it_accounttax-itemno_tax  = wa_accountgl-itemno_tax.
      APPEND it_accounttax.

      " Valor do Cofins
      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 3.
      wa_currencyamount-curr_type = '00'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_base  = vbase.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.
      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 3.
      wa_currencyamount-curr_type = '10'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_base  = vbase.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.
      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR  wa_extension1.
      wa_extension1-field1 = 'DIVISAO'.
      CONCATENATE  '003' st_zib_contabil-gsber INTO wa_extension1-field2.
      APPEND wa_extension1 TO it_extension1.

      CONTINUE.
    ENDIF.

    "PIS
    IF st_zib_contabil-sgtxt+0(3) = 'PIS'.
      it_accounttax-itemno_acc  = 4.
      it_accounttax-gl_account  = st_zib_contabil-hkont. " Pis
      it_accounttax-cond_key    = 'IPSV'.
      it_accounttax-acct_key    = 'PI3'.
      it_accounttax-tax_rate    = st_zib_contabil-rate.
      it_accounttax-tax_code    = 'I0'.
      it_accounttax-taxjurcode  = wa_lfa1-txjcd.
      it_accounttax-itemno_tax  = wa_accountgl-itemno_tax.
      APPEND it_accounttax.

      " Valor do Pis
      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 4.
      wa_currencyamount-curr_type = '00'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_base  = vbase.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.
      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc    = 4.
      wa_currencyamount-curr_type = '10'.
      wa_currencyamount-currency  = st_zib_contabil-waers.
      wa_currencyamount-amt_base  = vbase.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.
      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR  wa_extension1.
      wa_extension1-field1 = 'DIVISAO'.
      CONCATENATE  '004' st_zib_contabil-gsber INTO wa_extension1-field2.
      APPEND wa_extension1 TO it_extension1.

      CONTINUE.
    ENDIF.

    "INSS Patronal
    IF st_zib_contabil-sgtxt+0(13) = 'INSS PATRONAL'.

      CLEAR wa_accountgl.
      IF st_zib_contabil-bschl = '40'.
        wa_accountgl-itemno_acc = 5.
      ELSEIF st_zib_contabil-bschl = '50'.
        wa_accountgl-itemno_acc = 6.
      ENDIF.

      wa_accountgl-gl_account     = st_zib_contabil-hkont.
      wa_accountgl-ref_key_1      = ''.
      wa_accountgl-ref_key_2      = ''.
      wa_accountgl-ref_key_3      = ''.
      wa_accountgl-acct_type      = 'S'.
      wa_accountgl-item_text      = st_zib_contabil-sgtxt.
      wa_accountgl-bus_area       = st_zib_contabil-gsber.
      wa_accountgl-alloc_nmbr     = st_zib_contabil-zuonr.
      "wa_accountgl-tax_code       = 'I0'.
      wa_accountgl-doc_type       = st_zib_contabil-blart.
      wa_accountgl-cs_trans_t     = ''.
      wa_accountgl-orderid        = ''.
      "wa_accountgl-itemno_tax     = 2.

      APPEND wa_accountgl TO it_accountgl.

      " Moeda Resultado
      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc = wa_accountgl-itemno_acc.
      wa_currencyamount-curr_type  = '00'.
      wa_currencyamount-currency   = st_zib_contabil-waers.
      wa_currencyamount-amt_doccur = st_zib_contabil-wrbtr.

      IF st_zib_contabil-bschl = '50'.
        wa_currencyamount-amt_doccur = wa_currencyamount-amt_doccur * -1.
      ENDIF.

      APPEND wa_currencyamount TO it_currencyamount.

      CLEAR wa_currencyamount.
      wa_currencyamount-itemno_acc   = wa_accountgl-itemno_acc.
      wa_currencyamount-curr_type    = '10'.
      wa_currencyamount-currency     = st_zib_contabil-waers.
      wa_currencyamount-amt_doccur   = st_zib_contabil-wrbtr.

      IF st_zib_contabil-bschl = '50'.
        wa_currencyamount-amt_doccur = wa_currencyamount-amt_doccur * -1.
      ENDIF.

      APPEND wa_currencyamount TO it_currencyamount.

      CONTINUE.
    ENDIF.

  ENDLOOP.

  "ZLES0040  - Subcontratado - BG #134203

  sy-tcode              = 'FB05'.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "#EC CI_USAGE_OK[2438131]
    EXPORTING                            "#EC CI_USAGE_OK[2628704]
      documentheader    = gd_documentheader
    IMPORTING
      obj_type          = wa_returnobj-obj_type
      obj_key           = wa_returnobj-obj_key
      obj_sys           = wa_returnobj-obj_sys
    TABLES
      criteria          = it_criteria
      accountgl         = it_accountgl
      accountreceivable = it_receivable
      accountpayable    = it_payable
      accounttax        = it_accounttax
      currencyamount    = it_currencyamount
      extension1        = it_extension1
      return            = it_bapiret
      accountwt         = it_accountwt.

  CLEAR vobj_key.

  READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'E'.
  IF ( sy-subrc NE 0 ).
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    COMMIT WORK.

    UPDATE zlest0032 SET: awkey_sub = st_zib_contabil-obj_key WHERE tknum = st_dados_aux-tknum. "170427 Transação ZLES0040 Não gera OBJ_KEY_SUB PSA
    "ZLES0040  - Subcontratado - BG #134203 INICIO PT2
    DO 5 TIMES . "BG

      "intevalo de processamento
      SELECT awtyp awkey awsys belnr bukrs gjahr
        FROM bkpf UP TO 1 ROWS
        INTO (vg_awtyp, vg_awkey, vg_awsys,
              vg_belnr, vg_bukrs, vg_gjahr)
       WHERE ( awtyp EQ wa_returnobj-obj_type )
         AND ( awkey EQ wa_returnobj-obj_key  )
         AND ( stblg = space )
        ORDER BY awtyp awkey awsys.
      ENDSELECT.
      "processamento de 2 segundos
      IF vobj_key IS INITIAL.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    "ZLES0040  - Subcontratado - BG #134203 FIM PT2


    vobj_key = vg_belnr.
  ELSE.
* Listar todas as mensagens de erro.
    LOOP AT it_bapiret INTO wa_bapiret
         WHERE type NE 'S'.
      WRITE: /001 st_dados-tknum,
         013 st_dados_aux-fknum,
         025 st_dados_aux-ebeln,
         037 st_dados_aux-ebelp,
         043 st_dados_aux-lblni,
         055 wa_bapiret-message(55).
      IF wa_bapiret-message+55(55) IS NOT INITIAL.
        WRITE /055 wa_bapiret-message+55(55).
      ENDIF.
      IF wa_bapiret-message+110(55) IS NOT INITIAL.
        WRITE /055 wa_bapiret-message+110(55).
      ENDIF.
      IF wa_bapiret-message+165(55) IS NOT INITIAL.
        WRITE /055 wa_bapiret-message+165(55).
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " Z_MONTAR_EXEC_BAPI

*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_PIS
*&---------------------------------------------------------------------*
* Alimentar a tabela com dados da conta de pis
*----------------------------------------------------------------------*
FORM z_montar_conta_pis CHANGING  p_vlr_pedagio      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                  p_vlr_pis_ped      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                  p_vlr_cofins_ped   TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                  p_vlr_liq_ped      TYPE konv-kbetr.  "*-CS2022000741-04.10.1024-#83334-JT-inicio

  DATA: vl_razao    TYPE zlest0021-razaocred,
        vl_bus_area TYPE bapi_incinv_create_gl_account-bus_area,
        vl_deb_cre  TYPE c,
        vg_zuonr    TYPE dzuonr.

  CLEAR: st_zlest0021,
         st_konv     .

* Alimentar a tabela com dados do conta razão - PIS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_9
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
*-CS2022000741-#160701-11.12.2024-JT-inicio
  IF sy-subrc = 0.
* CHECK sy-subrc = 0.
*-CS2022000741-#160701-11.12.2024-JT-fim

* Obter o valor da condição referente ao PIS
    READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                             kschl = c_zpis
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR st_konv-kwert.
    ENDIF.

    IF st_konv-kwert IS INITIAL.
      PERFORM f_check_set_pis_cof_pv_pf USING st_dados_aux  'P' CHANGING st_konv-kwert "US 71631
                                                                         vg_kbert_pis_pf
                                                                         vg_kbert_cofins_pf.
    ENDIF.

    DO 2 TIMES.
      IF sy-index = 1.
        vl_razao    = st_zlest0021-razaocred.
        vl_bus_area = st_dados_aux-tplst.
        vl_deb_cre  = c_h.
      ELSE.
        vl_razao    = st_zlest0021-razaodeb.
        vl_bus_area = st_dados_aux-tplst.
        vl_deb_cre  = c_s.
      ENDIF.
      CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.
      PERFORM z_alimentar_glaccount USING c_000001
                                          vl_razao
                                          st_konv-kwert
                                          vl_deb_cre
                                          st_vfkp-bukrs
                                          vl_bus_area
                                          st_zlest0021-operfrete
                                          TEXT-016
                                          vg_zuonr.
    ENDDO.
  ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT-inicio

*-CS2022000741-#160701-11.12.2024-JT-inicio
* Alimentar a tabela com dados do conta razão - PIS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_f02
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_9
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  IF sy-subrc = 0.
* Obter o valor da condição referente ao PIS
    READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                             kschl = c_zpis
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR st_konv-kwert.
    ENDIF.
*-CS2022000741-#160701-11.12.2024-JT-fim

*-CS2022000741-04.10.1024-#83334-JT-inicio
*-CS2022000741-#160701-11.12.2024-JT-comentado
*   IF st_konv-kwert IS INITIAL.
*     PERFORM f_check_set_pis_cof_pedagio USING st_dados_aux  'P' CHANGING st_konv-kwert "US 71631
*                                                                          vg_kbert_pis_pf
*                                                                          vg_kbert_cofins_pf
*                                                                          p_vlr_pedagio      "*-CS2022000741-04.10.1024-#83334-JT-inicio
*                                                                          p_vlr_pis_ped      "*-CS2022000741-04.10.1024-#83334-JT-inicio
*                                                                          p_vlr_cofins_ped   "*-CS2022000741-04.10.1024-#83334-JT-inicio
*                                                                          p_vlr_liq_ped.     "*-CS2022000741-04.10.1024-#83334-JT-inicio
*   ENDIF.
*-CS2022000741-#160701-11.12.2024-JT-comentado
  ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT
*-CS2022000741-04.10.1024-#83334-JT-fim

ENDFORM.                    " Z_MONTAR_CONTA_PIS

*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_COFINS
*&---------------------------------------------------------------------*
* Alimentar a tabela com dados da conta de COFINS
*----------------------------------------------------------------------*
FORM z_montar_conta_cofins CHANGING  p_vlr_pedagio      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                     p_vlr_pis_ped      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                     p_vlr_cofins_ped   TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                     p_vlr_liq_ped      TYPE konv-kbetr.  "*-CS2022000741-04.10.1024-#83334-JT-inicio

  DATA: vl_razao    TYPE zlest0021-razaocred,
        vl_bus_area TYPE bapi_incinv_create_gl_account-bus_area,
        vl_deb_cre  TYPE c,
        vg_zuonr    TYPE dzuonr.

  CLEAR: st_zlest0021,
         st_konv     .

* Alimentar a tabela com dados do conta razão - COFINS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_1
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
*-CS2022000741-#160701-11.12.2024-JT-inicio
  IF sy-subrc = 0.
* CHECK sy-subrc = 0.
*-CS2022000741-#160701-11.12.2024-JT-fim

* Obter o valor da condição referente ao COFINS
    READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                             kschl = c_zcof
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR st_konv-kwert.
    ENDIF.

    IF st_konv-kwert IS INITIAL.
      PERFORM f_check_set_pis_cof_pv_pf USING st_dados_aux  'C' CHANGING st_konv-kwert "US 71631
                                                                         vg_kbert_pis_pf
                                                                         vg_kbert_cofins_pf.
    ENDIF.

    DO 2 TIMES.
      IF sy-index = 1.
        vl_razao    = st_zlest0021-razaocred.
        vl_bus_area = st_dados_aux-tplst.
        vl_deb_cre  = c_h.
      ELSE.
        vl_razao    = st_zlest0021-razaodeb.
        vl_bus_area = st_dados_aux-tplst.
        vl_deb_cre  = c_s.
      ENDIF.
      CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.
      PERFORM z_alimentar_glaccount USING c_000001
                                          vl_razao
                                          st_konv-kwert
                                          vl_deb_cre
                                          st_vfkp-bukrs
                                          vl_bus_area
                                          st_zlest0021-operfrete
                                          TEXT-017
                                          vg_zuonr.
    ENDDO.
  ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT

*-CS2022000741-#160701-11.12.2024-JT-inicio
* Alimentar a tabela com dados do conta razão - COFINS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_f02
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_1
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  IF sy-subrc = 0.
* Obter o valor da condição referente ao COFINS
    READ TABLE ti_konv INTO st_konv WITH KEY knumv = st_vfkp-knumv
                                             kschl = c_zcof
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR st_konv-kwert.
    ENDIF.
*-CS2022000741-#160701-11.12.2024-JT-fim

*-CS2022000741-04.10.1024-#83334-JT-inicio
*-CS2022000741-#160701-11.12.2024-JT-comentado
*   IF st_konv-kwert IS INITIAL.
*     PERFORM f_check_set_pis_cof_pedagio USING st_dados_aux  'C' CHANGING st_konv-kwert "US 71631
*                                                                          vg_kbert_pis_pf
*                                                                          vg_kbert_cofins_pf
*                                                                          p_vlr_pedagio      "*-CS2022000741-04.10.1024-#83334-JT-inicio
*                                                                          p_vlr_pis_ped      "*-CS2022000741-04.10.1024-#83334-JT-inicio
*                                                                          p_vlr_cofins_ped   "*-CS2022000741-04.10.1024-#83334-JT-inicio
*                                                                          p_vlr_liq_ped.     "*-CS2022000741-04.10.1024-#83334-JT-inicio
*   ENDIF.
*-CS2022000741-#160701-11.12.2024-JT-comentado
  ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT
*-CS2022000741-04.10.1024-#83334-JT-fim

ENDFORM.                    " Z_MONTAR_CONTA_COFINS

*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_EXEC_BAPI_RECEITA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VOBJ_KEY  text
*----------------------------------------------------------------------*
FORM z_montar_exec_bapi_receita  CHANGING vobj_key.

  DATA:
    vbudat TYPE sy-datum,
    vbldat TYPE sy-datum,
    vname1 TYPE lfa1-name1,
    vbase  TYPE zib_contabil-wrbtr.

  DATA: vg_awtyp LIKE bkpf-awtyp,
        vg_awkey LIKE bkpf-awkey,
        vg_awsys LIKE bkpf-awsys,
        vg_belnr LIKE bkpf-belnr,
        vg_bukrs LIKE bkpf-bukrs,
        vg_gjahr LIKE bkpf-gjahr,
        vg_total TYPE zib_contabil-wrbtr,
        vg_resto TYPE zib_contabil-wrbtr,
        vg_difer TYPE zib_contabil-wrbtr.

  REFRESH:  it_criteria,
            it_accountgl,
            it_receivable,
            it_payable,
            it_accounttax,
            it_currencyamount,
            it_extension1,
            it_bapiret,
            it_accountwt.

  CLEAR: vg_total,vg_resto,vname1.

  READ TABLE ti_vbak INTO st_vbak WITH KEY tknum = st_dados_aux-tknum BINARY SEARCH.
  READ TABLE ti_vbap INTO st_vbap WITH KEY vbeln = st_vbak-vbeln BINARY SEARCH.

* Informações fixas - pré definadas para utilização da BAPI
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = gd_documentheader-obj_sys.

  SELECT SINGLE * INTO @DATA(wa_zcte_identifica)
    FROM zcte_identifica
   WHERE tknum EQ @st_dados_aux-tknum.

  CHECK sy-subrc IS INITIAL.

  vbldat = wa_zcte_identifica-dhemi.
  vbudat = sy-datum.

  DATA: p_data_val TYPE datum.

  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = vbudat
      p_bukrs        = st_dados_aux-bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    IMPORTING
      p_data_val     = p_data_val
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

  DATA(lc_kuunr) = '000000' && st_vbap-gsber.
  SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1) WHERE kunnr EQ @lc_kuunr. "Dados do Cliente

  IF vbldat(6) NE p_data_val(6).
    vbldat = p_data_val.
    vbudat = p_data_val.
  ENDIF.

  gd_documentheader-obj_type    = 'IDOC'.
  gd_documentheader-username    = sy-uname.
  gd_documentheader-pstng_date  = vbudat.
  gd_documentheader-bus_act     = 'RFBU'.
  gd_documentheader-ref_doc_no  = st_dados_aux-exti1.
  gd_documentheader-obj_key     = st_dados_aux-tknum.
  gd_documentheader-header_txt  = 'RECEITA INTERCOMPANY'.
  gd_documentheader-comp_code   = st_dados_aux-bukrs.
  gd_documentheader-doc_date    = vbldat.
  gd_documentheader-fisc_year   = vbldat(4).
  gd_documentheader-fis_period  = vbldat+4(2).
  gd_documentheader-doc_type    = 'NQ'.

  READ TABLE ti_zlest0021 INTO st_zlest0021
  WITH KEY shtyp      = st_dados_aux-shtyp
           tcode      = c_f02
           tp_emissor = st_dados_aux-tp_emissor
           operfrete  = '28'
           tp_veiculo = st_dados_aux-tp_veiculo
           BINARY SEARCH.

  ""Cliente Valor Total
*  CLEAR WA_ACCOUNTGL.
*  WA_ACCOUNTGL-ITEMNO_ACC     = 1.
*  WA_ACCOUNTGL-GL_ACCOUNT     = ST_ZLEST0021-RAZAODEB.
*  WA_ACCOUNTGL-CUSTOMER       = WA_KNA1-KUNNR.
*  WA_ACCOUNTGL-REF_KEY_1      = ''.
*  WA_ACCOUNTGL-REF_KEY_2      = ''.
*  WA_ACCOUNTGL-REF_KEY_3      = ''.
*  WA_ACCOUNTGL-ACCT_TYPE      = 'S'.
*  WA_ACCOUNTGL-ITEM_TEXT      = 'Frete Rod nr. ' && ST_DADOS_AUX-EXTI1 && ' - ' && WA_KNA1-NAME1.
*  WA_ACCOUNTGL-BUS_AREA       = ST_DADOS_AUX-BRANCH.
*  WA_ACCOUNTGL-ALLOC_NMBR     = 'FR-' && ST_DADOS_AUX-EXTI2.
*  WA_ACCOUNTGL-TAX_CODE       = ''.
*  WA_ACCOUNTGL-DOC_TYPE       = 'FR'.
*  WA_ACCOUNTGL-TAXJURCODE     = ''.
*  WA_ACCOUNTGL-TRADE_ID       = WA_KNA1-VBUND.
*  WA_ACCOUNTGL-CS_TRANS_T     = ''.
*  WA_ACCOUNTGL-ORDERID        = ''.
*  APPEND WA_ACCOUNTGL TO IT_ACCOUNTGL.
*

  "Frete """"""""""""""""""" Ajuste 27.11.2020
  DATA: vl_frete TYPE konv-kwert.
  vl_frete = 0.
  LOOP AT ti_konv INTO st_konv
                  WHERE knumv = st_vfkp-knumv
                    AND ( kschl = c_zfre ).
    ADD st_konv-kwert TO vl_frete.
  ENDLOOP.

  " Moeda Resultado
  CLEAR wa_currencyamount.
  wa_currencyamount-itemno_acc    = 1.
  wa_currencyamount-curr_type    = '00'.
  wa_currencyamount-currency     = st_vfkp-waers.
  wa_currencyamount-amt_doccur   = vl_frete."ST_VFKP-NETWR. Ajuste 27.11.2020
  APPEND wa_currencyamount TO it_currencyamount.

  CLEAR wa_currencyamount.
  wa_currencyamount-itemno_acc   = 1.
  wa_currencyamount-curr_type    = '10'.
  wa_currencyamount-currency     = st_vfkp-waers.
  wa_currencyamount-amt_doccur   = vl_frete."ST_VFKP-NETWR. Ajuste 27.11.2020
  APPEND wa_currencyamount TO it_currencyamount.

  CLEAR: wa_receivable.
  wa_receivable-itemno_acc    = 1.
  wa_receivable-alloc_nmbr    = 'NQ-' && st_dados_aux-exti2.
  wa_receivable-item_text     = 'Frete Rod nr. ' && st_dados_aux-exti1 && ' - ' && wa_kna1-name1..
  wa_receivable-comp_code     = st_dados_aux-bukrs.
  wa_receivable-bus_area      = st_dados_aux-branch.
  wa_receivable-gl_account    = st_zlest0021-razaodeb.
  wa_receivable-customer      = wa_kna1-kunnr.
  APPEND wa_receivable TO it_receivable.

  IF wa_kna1-vbund IS NOT INITIAL.
    CLEAR  wa_extension1.
    wa_extension1-field1 = 'SOCPARC'.
    wa_extension1-field2 = 1.
    wa_extension1-field3 = wa_kna1-vbund.
    APPEND wa_extension1 TO it_extension1.
  ENDIF.

  "Seguro """""""""""""""""""
  DATA: vl_seguro TYPE konv-kwert.
  vl_seguro = 0.
  LOOP AT ti_konv INTO st_konv
                  WHERE knumv = st_vfkp-knumv
                    AND ( kschl = c_zseg
                     OR   kschl = c_ziof ).
    ADD st_konv-kwert TO vl_seguro.
  ENDLOOP.

  IF vl_seguro GT 0.

    READ TABLE ti_zlest0021 INTO st_zlest0021
    WITH KEY shtyp      = st_dados_aux-shtyp
             tcode      = c_f02
             tp_emissor = st_dados_aux-tp_emissor
             operfrete  = '27'
             tp_veiculo = st_dados_aux-tp_veiculo
             BINARY SEARCH.

    "Credito (Cliente)----------------------------------------------------------------------------------------------------------------*
    CLEAR: wa_receivable.
    wa_receivable-itemno_acc    = 2.
    wa_receivable-alloc_nmbr    = 'NQ-' && st_dados_aux-exti2.
    wa_receivable-item_text     = 'Seguro s/ frete Rod nr. ' && st_dados_aux-exti1 && ' - ' && wa_kna1-name1.
    wa_receivable-comp_code     = st_dados_aux-bukrs.
    wa_receivable-bus_area      = st_dados_aux-branch.
    wa_receivable-customer      = wa_kna1-kunnr.
    APPEND wa_receivable TO it_receivable.

    IF wa_kna1-vbund IS NOT INITIAL.
      CLEAR  wa_extension1.
      wa_extension1-field1 = 'SOCPARC'.
      wa_extension1-field2 = 1.
      wa_extension1-field3 = wa_kna1-vbund.
      APPEND wa_extension1 TO it_extension1.
    ENDIF.

    " Moeda Resultado
    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc    = 2.
    wa_currencyamount-curr_type    = '00'.
    wa_currencyamount-currency     = st_vfkp-waers.
    wa_currencyamount-amt_doccur   = vl_seguro * -1.
    APPEND wa_currencyamount TO it_currencyamount.

    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc   = 2.
    wa_currencyamount-curr_type    = '10'.
    wa_currencyamount-currency     = st_vfkp-waers.
    wa_currencyamount-amt_doccur   = vl_seguro * -1.
    APPEND wa_currencyamount TO it_currencyamount.

    "Debito Razao ----------------------------------------------------------------------------------------------------------------*
    CLEAR wa_accountgl.
    wa_accountgl-itemno_acc = 3.
    wa_accountgl-gl_account = st_zlest0021-razaodeb.
    wa_accountgl-ref_key_1  = ''.
    wa_accountgl-ref_key_2  = ''.
    wa_accountgl-ref_key_3  = ''.
    wa_accountgl-acct_type  = 'S'.
    wa_accountgl-item_text  = 'Seguro s/ frete Rod nr. ' && st_dados_aux-exti1 && ' - ' && wa_kna1-name1.
    wa_accountgl-comp_code  = st_dados_aux-bukrs.
    wa_accountgl-bus_area   = st_dados_aux-branch.
    wa_accountgl-doc_type   = 'NQ'.

    SELECT SINGLE *
      FROM zlest0033 INTO @DATA(lwa_zlest0033)
      WHERE bukrs = @st_dados_aux-bukrs
      AND   gsber = @st_dados_aux-branch
      AND   saknr = @st_zlest0021-razaodeb.

    IF sy-subrc = 0.
      wa_accountgl-costcenter = lwa_zlest0033-kostl.
    ENDIF.

    APPEND wa_accountgl TO it_accountgl.

    " Moeda Resultado
    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc    = 3.
    wa_currencyamount-curr_type    = '00'.
    wa_currencyamount-currency     = st_vfkp-waers.
    wa_currencyamount-amt_doccur   = vl_seguro.
    APPEND wa_currencyamount TO it_currencyamount.

    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc   = 3.
    wa_currencyamount-curr_type    = '10'.
    wa_currencyamount-currency     = st_vfkp-waers.
    wa_currencyamount-amt_doccur   = vl_seguro.
    APPEND wa_currencyamount TO it_currencyamount.

  ENDIF.

  READ TABLE ti_zlest0021 INTO st_zlest0021
  WITH KEY shtyp      = st_dados_aux-shtyp
           tcode      = c_f02
           tp_emissor = st_dados_aux-tp_emissor
           operfrete  = '26'
           tp_veiculo = st_dados_aux-tp_veiculo
           BINARY SEARCH.

  ""Receita Líquido
  CLEAR wa_accountgl.
  wa_accountgl-itemno_acc = 4.
  wa_accountgl-gl_account = st_zlest0021-razaocred.
  wa_accountgl-ref_key_1  = ''.
  wa_accountgl-ref_key_2  = ''.
  wa_accountgl-ref_key_3  = ''.
  wa_accountgl-acct_type  = 'S'.
  wa_accountgl-item_text  = 'Frete Rod nr. ' && st_dados_aux-exti1 && ' - ' && wa_kna1-name1.
  wa_accountgl-comp_code  = st_dados_aux-bukrs.
  wa_accountgl-bus_area   = st_dados_aux-branch.
  wa_accountgl-alloc_nmbr = 'NQ-' && st_dados_aux-exti2.
  wa_accountgl-tax_code   = ''.
  wa_accountgl-doc_type   = 'NQ'.
  wa_accountgl-taxjurcode = ''.
  wa_accountgl-trade_id   = wa_kna1-vbund.
  wa_accountgl-cs_trans_t = ''.
  wa_accountgl-orderid    = ''.
  APPEND wa_accountgl TO it_accountgl.

  " Moeda Resultado
  CLEAR wa_currencyamount.
  wa_currencyamount-itemno_acc    = 4.
  wa_currencyamount-curr_type    = '00'.
  wa_currencyamount-currency     = st_vfkp-waers.
  wa_currencyamount-amt_doccur   = vl_frete * -1. "( ST_VFKP-NETWR - VL_SEGURO ) * -1.  Ajuste 27.11.2020
  APPEND wa_currencyamount TO it_currencyamount.

  CLEAR wa_currencyamount.
  wa_currencyamount-itemno_acc   = 4.
  wa_currencyamount-curr_type    = '10'.
  wa_currencyamount-currency     = st_vfkp-waers.
  wa_currencyamount-amt_doccur   = vl_frete * -1. "( ST_VFKP-NETWR - VL_SEGURO ) * -1.  Ajuste 27.11.2020
  APPEND wa_currencyamount TO it_currencyamount.

  it_criteria-itemno_acc    = 4.
  it_criteria-fieldname     = 'ARTNR'.
*---> 13/06/2023 - Migração S4 - JS
*            it_criteria-character     = st_vbap-matnr.
  it_criteria-character = CONV #( st_vbap-matnr ).
*<--- 13/06/2023 - Migração S4 - JS
  APPEND it_criteria.

  "ZLES0040  - Subcontratado - BG #134203
  UPDATE zlest0032 SET: awkey_rec = st_dados_aux-tknum
                      WHERE tknum = st_dados_aux-tknum.
  COMMIT WORK.

  sy-tcode = 'FB05'.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "#EC CI_USAGE_OK[2438131]
    EXPORTING                            "#EC CI_USAGE_OK[2628704]
      documentheader    = gd_documentheader
    IMPORTING
      obj_type          = wa_returnobj-obj_type
      obj_key           = wa_returnobj-obj_key
      obj_sys           = wa_returnobj-obj_sys
    TABLES
      criteria          = it_criteria
      accountgl         = it_accountgl
      accountreceivable = it_receivable
      accountpayable    = it_payable
      accounttax        = it_accounttax
      currencyamount    = it_currencyamount
      extension1        = it_extension1
      return            = it_bapiret
      accountwt         = it_accountwt.

  CLEAR vobj_key.

  READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'E'.
  IF ( sy-subrc NE 0 ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    COMMIT WORK.

    "ZLES0040  - Subcontratado - BG #134203 INICIO PT1
    DO 5 TIMES.

      SELECT awtyp awkey awsys belnr bukrs gjahr
        FROM bkpf UP TO 1 ROWS
        INTO (vg_awtyp, vg_awkey, vg_awsys,
              vg_belnr, vg_bukrs, vg_gjahr)
       WHERE ( awtyp EQ wa_returnobj-obj_type )
         AND ( awkey EQ wa_returnobj-obj_key  )
         AND ( blart EQ 'NQ' )
         AND ( stblg = space )
        ORDER BY awtyp awkey awsys.
      ENDSELECT.
      "processamento de 2 segundos
      IF   vobj_key IS INITIAL.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    "ZLES0040  - Subcontratado - BG #134203 FIM PT1

    vobj_key = vg_belnr.
  ELSE.
* Listar todas as mensagens de erro.
    LOOP AT it_bapiret INTO wa_bapiret
         WHERE type NE 'S'.
      WRITE: /001 st_dados-tknum,
         013 st_dados_aux-fknum,
         025 st_dados_aux-ebeln,
         037 st_dados_aux-ebelp,
         043 st_dados_aux-lblni,
         055 wa_bapiret-message(55).
      IF wa_bapiret-message+55(55) IS NOT INITIAL.
        WRITE /055 wa_bapiret-message+55(55).
      ENDIF.
      IF wa_bapiret-message+110(55) IS NOT INITIAL.
        WRITE /055 wa_bapiret-message+110(55).
      ENDIF.
      IF wa_bapiret-message+165(55) IS NOT INITIAL.
        WRITE /055 wa_bapiret-message+165(55).
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_PIS_NAO_ACU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_montar_conta_pis_nao_acu CHANGING  p_vlr_pedagio      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                          p_vlr_pis_ped      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                          p_vlr_cofins_ped   TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                          p_vlr_liq_ped      TYPE konv-kbetr.  "*-CS2022000741-04.10.1024-#83334-JT-inicio

  DATA: vl_razao      TYPE zlest0021-razaocred,
        vl_bus_area   TYPE bapi_incinv_create_gl_account-bus_area,
        vl_deb_cre    TYPE c,
        vg_zuonr      TYPE dzuonr,
        vl_data       TYPE c LENGTH 10,
        vl_total_icms TYPE konv-kwert,
        vl_validto    TYPE j_1btxpis-validto,
        vl_valor      TYPE kwert. "LES-CS2022000615 -ZLES0040 -Criação Miro Frete Próprio -BG  #79990

  CLEAR: st_zlest0021.

* Alimentar a tabela com dados do conta razão - PIS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_29
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.

*-CS2022000741-#160701-11.12.2024-JT-inicio
  IF sy-subrc = 0.
* CHECK sy-subrc IS INITIAL.
*-CS2022000741-#160701-11.12.2024-JT-fim

    "Buscar Percentual do PIS Não Acumulativo
    READ TABLE ti_vtpa INTO DATA(wa_vtpa) WITH KEY vbeln = st_dados_aux-tknum
                                             parvw = c_pv
                                             BINARY SEARCH.

    IF sy-subrc = 0.            "*-CS2022000741-#160701-11.12.2024-JT
*   CHECK sy-subrc IS INITIAL.  "*-CS2022000741-#160701-11.12.2024-JT

      READ TABLE ti_lifnr_pv INTO DATA(wa_lifnr_pv) WITH KEY lifnr = wa_vtpa-lifnr BINARY SEARCH.

*LES-CS2022000615 -ZLES0040 -Criação Miro Frete Próprio -BG  #79990 - INICIO

      IF sy-subrc = 0.           "*-CS2022000741-#160701-11.12.2024-JT
*     CHECK sy-subrc IS INITIAL. "*-CS2022000741-#160701-11.12.2024-JT  "AND wa_lifnr_pv-stkzn EQ abap_false.

        IF wa_lifnr_pv-stkzn EQ 'X'.
          PERFORM f_check_set_pis_cof_pv_pf USING st_dados_aux  'P' CHANGING vl_valor
                                                                             vg_kbert_pis_pf
                                                                             vg_kbert_cofins_pf.
        ELSE.
          vl_validto = st_dados_aux-budat.

          CONCATENATE st_vfkp-budat+6(2) '.'  st_vfkp-budat+4(2) '.' st_vfkp-budat(4) INTO vl_data.

          CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
            EXPORTING
              input  = vl_data
            IMPORTING
              output = vl_validto.

          SELECT SINGLE rate
            FROM j_1btxpis
            INTO @DATA(v_ratepis)
           WHERE country   EQ 'BR'
             AND gruop     EQ 72
             AND value     EQ @st_dados_aux-tdlnr+6(4)
             AND validto   LE @vl_validto
             AND validfrom GE @vl_validto.


          LOOP AT ti_konv INTO st_konv WHERE knumv = st_vfkp-knumv
                                         AND kschl = c_zicm.
            ADD st_konv-kwert TO vl_total_icms.
          ENDLOOP.

          DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins( i_valor_frete = CONV #( st_vfkp-netwr )
                                                                                i_valor_icms  = CONV #( vl_total_icms ) ).


          vl_valor = ( lva_base_calc_pis_cofins * ( v_ratepis ) / 100 ).

        ENDIF.

*LES-CS2022000615 -ZLES0040 -Criação Miro Frete Próprio -BG  #79990 - FIM

        DO 2 TIMES.
          IF sy-index = 1.
            vl_razao    = st_zlest0021-razaocred.
            vl_bus_area = st_dados_aux-tplst.
            vl_deb_cre  = c_h.
          ELSE.
            vl_razao    = st_zlest0021-razaodeb.
            vl_bus_area = st_dados_aux-tplst.
            vl_deb_cre  = c_s.
          ENDIF.
          CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.

          PERFORM z_alimentar_glaccount USING c_000001
                                              vl_razao
                                              vl_valor
                                              vl_deb_cre
                                              st_vfkp-bukrs
                                              vl_bus_area
                                              st_zlest0021-operfrete
                                              TEXT-018
                                              vg_zuonr.
        ENDDO.
      ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT
    ENDIF.    "*-CS2022000741-#160701-11.12.2024-JT
  ENDIF.      "*-CS2022000741-#160701-11.12.2024-JT

*-CS2022000741-#160701-11.12.2024-JT-inicio
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_f02
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_29
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  IF sy-subrc = 0.
*-CS2022000741-#160701-11.12.2024-JT-fim

*-CS2022000741-04.10.1024-#83334-JT-inicio
    PERFORM f_check_set_pis_cof_pedagio USING st_dados_aux  'P' CHANGING vl_valor
                                                                         vg_kbert_pis_pf
                                                                         vg_kbert_cofins_pf
                                                                         p_vlr_pedagio      "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                                                         p_vlr_pis_ped      "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                                                         p_vlr_cofins_ped   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                                                         p_vlr_liq_ped.     "*-CS2022000741-04.10.1024-#83334-JT-inicio
*-CS2022000741-04.10.1024-#83334-JT-fim
  ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CONTA_COFINS_NAO_ACU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_montar_conta_cofins_nao_acu CHANGING  p_vlr_pedagio      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                             p_vlr_pis_ped      TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                             p_vlr_cofins_ped   TYPE konv-kbetr   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                             p_vlr_liq_ped      TYPE konv-kbetr.  "*-CS2022000741-04.10.1024-#83334-JT-inicio


  DATA: vl_razao      TYPE zlest0021-razaocred,
        vl_bus_area   TYPE bapi_incinv_create_gl_account-bus_area,
        vl_deb_cre    TYPE c,
        vg_zuonr      TYPE dzuonr,
        vl_total_icms TYPE konv-kwert,
        vl_data       TYPE c LENGTH 10,
        vl_validto    TYPE j_1btxcof-validto,
        vl_valor      TYPE kwert. "*LES-CS2022000615 -ZLES0040 -Criação Miro Frete Próprio -BG  #79990 - INICIO pt02

  CLEAR: st_zlest0021.

* Alimentar a tabela com dados do conta razão - PIS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_miro
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_30
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
*-CS2022000741-#160701-11.12.2024-JT-inicio
  IF sy-subrc = 0.
* CHECK sy-subrc IS INITIAL.
*-CS2022000741-#160701-11.12.2024-JT-fim

    "Buscar Percentual do PIS Não Acumulativo
    READ TABLE ti_vtpa INTO DATA(wa_vtpa) WITH KEY vbeln = st_dados_aux-tknum
                                             parvw = c_pv
                                             BINARY SEARCH.


    IF sy-subrc IS INITIAL.    "*-CS2022000741-#160701-11.12.2024-JT
*   CHECK sy-subrc IS INITIAL. "*-CS2022000741-#160701-11.12.2024-JT

      READ TABLE ti_lifnr_pv INTO DATA(wa_lifnr_pv) WITH KEY lifnr = wa_vtpa-lifnr BINARY SEARCH.

*LES-CS2022000615 -ZLES0040 -Criação Miro Frete Próprio -BG  #79990 - INICIO pt02

      IF sy-subrc IS INITIAL.      "*-CS2022000741-#160701-11.12.2024-JT
*     CHECK sy-subrc IS INITIAL.   "*-CS2022000741-#160701-11.12.2024-JT " AND wa_lifnr_pv-stkzn EQ abap_false.

        IF wa_lifnr_pv-stkzn EQ 'X'.
          PERFORM f_check_set_pis_cof_pv_pf USING st_dados_aux  'C' CHANGING vl_valor
                                                                             vg_kbert_pis_pf
                                                                             vg_kbert_cofins_pf.
        ELSE.
          CONCATENATE st_vfkp-budat+6(2) '.'  st_vfkp-budat+4(2) '.' st_vfkp-budat(4) INTO vl_data.

          CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
            EXPORTING
              input  = vl_data
            IMPORTING
              output = vl_validto.

          SELECT SINGLE rate
            FROM j_1btxcof
            INTO @DATA(v_ratecof)
           WHERE country   EQ 'BR'
             AND gruop     EQ 71
             AND value     EQ @st_dados_aux-tdlnr+6(4)
             AND validto   LE @vl_validto
             AND validfrom GE @vl_validto.

          LOOP AT ti_konv INTO st_konv WHERE knumv = st_vfkp-knumv
                                         AND kschl = c_zicm.
            ADD st_konv-kwert TO vl_total_icms.
          ENDLOOP.

          DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins( i_valor_frete = CONV #( st_vfkp-netwr )
                                                                                i_valor_icms  = CONV #( vl_total_icms ) ).

          vl_valor = ( lva_base_calc_pis_cofins * ( v_ratecof ) / 100 ).

        ENDIF.



        DO 2 TIMES.
          IF sy-index = 1.
            vl_razao    = st_zlest0021-razaocred.
            vl_bus_area = st_dados_aux-tplst.
            vl_deb_cre  = c_h.
          ELSE.
            vl_razao    = st_zlest0021-razaodeb.
            vl_bus_area = st_dados_aux-tplst.
            vl_deb_cre  = c_s.
          ENDIF.
          CONCATENATE 'FR-' st_dados_aux-exti2 INTO vg_zuonr.

          PERFORM z_alimentar_glaccount USING c_000001
                                              vl_razao
                                              vl_valor
                                              vl_deb_cre
                                              st_vfkp-bukrs
                                              vl_bus_area
                                              st_zlest0021-operfrete
                                              TEXT-019
                                              vg_zuonr.
        ENDDO.
      ENDIF.  "*-CS2022000741-#160701-11.12.2024-JT
    ENDIF.    "*-CS2022000741-#160701-11.12.2024-JT
  ENDIF.      "*-CS2022000741-#160701-11.12.2024-JT

*-CS2022000741-#160701-11.12.2024-JT-inicio
* Alimentar a tabela com dados do conta razão - PIS
  READ TABLE ti_zlest0021 INTO st_zlest0021 WITH KEY shtyp      = st_dados_aux-shtyp
                                                     tcode      = c_f02
                                                     tp_emissor = st_dados_aux-tp_emissor
                                                     operfrete  = c_30
                                                     tp_veiculo = st_dados_aux-tp_veiculo
                                                     BINARY SEARCH.
  IF sy-subrc = 0.
*-CS2022000741-#160701-11.12.2024-JT-fim

*-CS2022000741-04.10.1024-#83334-JT-inicio
    PERFORM f_check_set_pis_cof_pedagio USING st_dados_aux  'C' CHANGING vl_valor
                                                                         vg_kbert_pis_pf
                                                                         vg_kbert_cofins_pf
                                                                         p_vlr_pedagio      "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                                                         p_vlr_pis_ped      "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                                                         p_vlr_cofins_ped   "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                                                         p_vlr_liq_ped.     "*-CS2022000741-04.10.1024-#83334-JT-inicio
*-CS2022000741-04.10.1024-#83334-JT-fim
  ENDIF.   ""*-CS2022000741-#160701-11.12.2024-JT

ENDFORM.

FORM f_check_set_pis_cof_pv_pf USING p_dados_aux      TYPE y_dados
                                     p_tipo           TYPE c
                            CHANGING p_kwert          TYPE konv-kwert
                                     p_kbert_pis      TYPE zlest0215-kbert_pis
                                     p_kbert_confis   TYPE zlest0215-kbert_cofins.

  DATA: lva_zfre TYPE konv-kwert.

  CLEAR: p_kwert, p_kbert_pis, p_kbert_confis.

  READ TABLE ti_vtpa INTO DATA(wa_vtpa) WITH KEY vbeln = p_dados_aux-tknum
                                                 parvw = c_pv.

  CHECK sy-subrc EQ 0.

  READ TABLE ti_lifnr_pv INTO DATA(wa_lifnr_pv) WITH KEY lifnr = wa_vtpa-lifnr.
  CHECK sy-subrc EQ 0.

  CHECK sy-subrc EQ 0 AND wa_lifnr_pv-stkzn EQ abap_true.

*-CS2022000741-04.10.1024-#83334-JT-inicio
  SELECT SINGLE * FROM zlest0215 INTO @DATA(lwa_zlest0215) WHERE bukrs       EQ @st_vfkp-bukrs
                                                             AND lifnr       EQ @p_dados_aux-tdlnr
                                                             AND id_processo EQ @c_pis_cof_pv_pf.
*-CS2022000741-04.10.1024-#83334-JT-fim

  CHECK ( sy-subrc EQ 0 ) AND ( p_dados_aux-tdlnr IS NOT INITIAL ).

  p_kbert_pis    = lwa_zlest0215-kbert_pis.
  p_kbert_confis = lwa_zlest0215-kbert_cofins.

  CHECK ( p_dados_aux-tp_emissor EQ 'P' ) AND    "Emissor Proprio
        ( p_dados_aux-tp_veiculo EQ '0' ).     "Veiculo Terceiro.

  CLEAR: lva_zfre.
  LOOP AT ti_konv INTO st_konv WHERE knumv = st_vfkp-knumv
                                 AND kschl = c_zfre.
    ADD st_konv-kwert TO lva_zfre.
  ENDLOOP.

  CHECK lva_zfre > 0.

  CASE p_tipo.
    WHEN 'P'. "PIS
      CHECK lwa_zlest0215-kbert_pis IS NOT INITIAL.
      p_kwert          = ( lva_zfre       * ( lwa_zlest0215-kbert_pis ) / 100 ).

    WHEN 'C'. "COFINS
      CHECK lwa_zlest0215-kbert_cofins IS NOT INITIAL.
      p_kwert          = ( lva_zfre       * ( lwa_zlest0215-kbert_cofins ) / 100 ).
  ENDCASE.

ENDFORM.

*-CS2022000741-04.10.1024-#83334-JT-inicio
FORM f_check_set_pis_cof_pedagio USING p_dados_aux      TYPE y_dados
                                       p_tipo           TYPE c
                              CHANGING p_kwert          TYPE konv-kwert
                                       p_kbert_pis      TYPE zlest0215-kbert_pis
                                       p_kbert_confis   TYPE zlest0215-kbert_cofins
                                       p_vlr_pedagio    TYPE konv-kbetr       "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                       p_vlr_pis_ped    TYPE konv-kbetr       "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                       p_vlr_cofins_ped TYPE konv-kbetr       "*-CS2022000741-04.10.1024-#83334-JT-inicio
                                       p_vlr_liq_ped    TYPE konv-kbetr.      "*-CS2022000741-04.10.1024-#83334-JT-inicio

  DATA: lva_zped      TYPE konv-kwert,  "*-CS2022000741-04.10.1024-#83334-JT-inicio
        lwa_zlest0215 TYPE zlest0215.

  CLEAR: p_kwert, p_kbert_pis, p_kbert_confis.

  READ TABLE ti_vtpa INTO DATA(wa_vtpa) WITH KEY vbeln = p_dados_aux-tknum
                                                 parvw = c_pv.

  CHECK sy-subrc EQ 0.

  READ TABLE ti_lifnr_pv INTO DATA(wa_lifnr_pv) WITH KEY lifnr = wa_vtpa-lifnr.

  CHECK sy-subrc EQ 0. " AND wa_lifnr_pv-stkzn EQ abap_true. "*-CS2022000741-04.10.1024-#83334-JT-inicio

*-CS2022000741-04.10.1024-#83334-JT-inicio
  CLEAR lwa_zlest0215.
  SELECT SINGLE * FROM zlest0215 INTO lwa_zlest0215 WHERE bukrs       EQ st_vfkp-bukrs
                                                      AND lifnr       EQ p_dados_aux-tdlnr
                                                      AND id_processo EQ c_pis_cof_ped.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM zlest0215 INTO lwa_zlest0215 WHERE bukrs       EQ st_vfkp-bukrs
                                                        AND lifnr       EQ abap_off
                                                        AND id_processo EQ c_pis_cof_ped.
  ENDIF.
*-CS2022000741-04.10.1024-#83334-JT-fim

  CHECK ( sy-subrc EQ 0 ) AND ( p_dados_aux-tdlnr IS NOT INITIAL ).

*-CS2022000741-04.10.1024-#83334-JT-inicio
  p_kbert_pis    = lwa_zlest0215-kbert_pis.
  p_kbert_confis = lwa_zlest0215-kbert_cofins.
*-CS2022000741-04.10.1024-#83334-JT-fim

  CHECK ( p_dados_aux-tp_emissor EQ 'P'   OR    "Emissor Proprio
          p_dados_aux-tp_emissor EQ 'I' ) AND  "Emissor Inercompany  "*-CS2022000741-04.10.1024-#83334-JT-inicio
        ( p_dados_aux-tp_veiculo EQ '0' ).     "Veiculo Terceiro.

*-CS2022000741-04.10.1024-#83334-JT-inicio
  CLEAR: lva_zped.
  LOOP AT ti_konv INTO st_konv WHERE knumv = st_vfkp-knumv
                                 AND kschl = c_zped.
    ADD st_konv-kwert TO lva_zped.
  ENDLOOP.

  p_vlr_pedagio = lva_zped.
*-CS2022000741-04.10.1024-#83334-JT-fim

  CHECK lva_zped > 0. "*-CS2022000741-04.10.1024-#83334-JT-inicio

  CASE p_tipo.
    WHEN 'P'. "PIS
      CHECK lwa_zlest0215-kbert_pis IS NOT INITIAL.
      p_kwert          = ( lva_zped       * ( lwa_zlest0215-kbert_pis ) / 100 ).
      p_vlr_pis_ped    = p_vlr_pis_ped    + ( lva_zped * ( lwa_zlest0215-kbert_pis ) / 100 ).     "*-CS2022000741-04.10.1024-#83334-JT-inicio
      p_vlr_liq_ped    = p_vlr_liq_ped    + p_kwert.                                              "*-CS2022000741-#160701-11.12.2024-JT

    WHEN 'C'. "COFINS
      CHECK lwa_zlest0215-kbert_cofins IS NOT INITIAL.
      p_kwert          = ( lva_zped       * ( lwa_zlest0215-kbert_cofins ) / 100 ).
      p_vlr_cofins_ped = p_vlr_cofins_ped + ( lva_zped * ( lwa_zlest0215-kbert_cofins ) / 100 ).  "*-CS2022000741-04.10.1024-#83334-JT-inicio
      p_vlr_liq_ped    = p_vlr_liq_ped    + p_kwert.                                              "*-CS2022000741-#160701-11.12.2024-JT
  ENDCASE.

ENDFORM.
*-CS2022000741-04.10.1024-#83334-JT-fim
