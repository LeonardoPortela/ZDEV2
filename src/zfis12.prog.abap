*&------------P R O J E T O  E V O L U I R   -   M A G G I-------------*
* Programa   : ZFIS12                                                  *
* Descrição  : Conferência de Notas Fiscais                            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 06.10.2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Camila Brand                          Data: 07.07.2011  *
* Observações: Alteraçães conforme chamado 44124                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Camila Brand                          Data: 03.11.2011  *
* Observações: Alteraçães conforme chamado 54664                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Thomas Ferraz                         Data: 23.04.2014  *
* Empresa    : Solveplan                                               *
* Observações: Criação de flag para gravar dados da TY_SAIDA na tabela *
*              ZDADOS_SAIDA atraves da form F_GRAVAR_DADOS             *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Fabricio Fonseca                      Data: 28.11.2022  *
* Observações: Ajustes chamado #79638 - Adicionando novas colunas e    *
*              filtro por pedido. "CS2022000515 - FF  28.11.2022                                      *
*----------------------------------------------------------------------*
REPORT   ZFIS12.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.

TABLES : J_1BNFDOC ,
         VBFA,
         J_1BNFLIN,
         ZIB_NFE_DIST_ITM,
         J_1BBRANCH,
         VBAP,
         ZDADOS_SAIDA.

CONSTANTS C_ZCONF_DIAS_FILTRO TYPE TVARVC-NAME VALUE 'ZCONF_DIAS_FILTRO'.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TY_J_1BNFDOC,
    BELNR     TYPE J_1BNFDOC-BELNR,
    DOCNUM    TYPE J_1BNFDOC-DOCNUM,
    PSTDAT    TYPE J_1BNFDOC-PSTDAT,
    BUKRS     TYPE J_1BNFDOC-BUKRS,
    SERIES    TYPE J_1BNFDOC-SERIES,
    NFTYPE    TYPE J_1BNFDOC-NFTYPE,
    DOCDAT    TYPE J_1BNFDOC-DOCDAT,
    CRENAM    TYPE J_1BNFDOC-CRENAM,
    MODEL     TYPE J_1BNFDOC-MODEL,
    NFNUM     TYPE J_1BNFDOC-NFNUM,
    BRANCH    TYPE J_1BNFDOC-BRANCH,
    PARID     TYPE J_1BNFDOC-PARID,
    NFE       TYPE J_1BNFDOC-NFE,
    NFENUM    TYPE J_1BNFDOC-NFENUM,
    PARTYP    TYPE J_1BNFDOC-PARTYP,
    NFTOT     TYPE J_1BNFDOC-NFTOT,
    DIRECT    TYPE J_1BNFDOC-DIRECT,
    CANCEL    TYPE J_1BNFDOC-CANCEL,
    NTGEW     TYPE J_1BNFDOC-NTGEW,
    CRT_BUPLA TYPE J_1BNFDOC-CRT_BUPLA, " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC
  END   OF TY_J_1BNFDOC,
*
*      BEGIN OF ty_j_1bnfstx,
*        docnum    TYPE j_1bnfstx-docnum,
*        taxtyp    TYPE j_1bnfstx-taxtyp,
*        taxval    TYPE j_1bnfstx-taxval,
*        base      TYPE j_1bnfstx-base,
*        othbas    TYPE j_1bnfstx-othbas,
*        excbas    TYPE j_1bnfstx-excbas,
*      END   OF ty_j_1bnfstx,
*
*      BEGIN OF ty_j_1baj,
*        taxtyp    TYPE j_1baj-taxtyp,
*        taxgrp    TYPE j_1baj-taxgrp,
*      END   OF ty_j_1baj,
*
*      BEGIN OF ty_j_1bnflin,
*        docnum    TYPE j_1bnflin-docnum,
*        cfop      TYPE j_1bnflin-cfop,
*        menge     TYPE j_1bnflin-menge,
*        meins     TYPE j_1bnflin-meins,
*        netwrt    TYPE j_1bnflin-netwrt,
*        matnr     TYPE j_1bnflin-matnr,
*        refkey    TYPE j_1bnflin-refkey,
*      END   OF ty_j_1bnflin,
*
*
*     BEGIN OF ty_j_1batl1v,
*        taxlaw TYPE j_1batl1v-taxlaw,
*      END OF ty_j_1batl1v,
*
*     BEGIN OF ty_vbfa_doc,
*        docnum    TYPE j_1bnflin-docnum,
*      END   OF ty_vbfa_doc,
*
  BEGIN OF TY_REMESSA,
    REFKEY        LIKE VBFA-VBELN,
    CH_REFERENCIA LIKE ZSDT0001-CH_REFERENCIA,
    VBELN         LIKE LIPS-VBELN,
    NR_ROMANEIO   LIKE ZSDT0001-NR_ROMANEIO,
    VGBEL         LIKE LIPS-VGBEL,
  END OF TY_REMESSA,
*
*      BEGIN OF ty_remessa_3,
*        vbeln         LIKE vbfa-vbeln,
*        refkey        LIKE bkpf-awkey,
*      END   OF ty_remessa_3,
*
*      BEGIN OF ty_remessa2,
*        refkey        LIKE vbfa-vbeln,
*        vgbel         LIKE lips-vgbel,
*      END OF ty_remessa2,
*
*      BEGIN OF ty_t001,
*        bukrs     TYPE t001-bukrs,
*        butxt     TYPE t001-butxt,
*      END   OF ty_t001,
*
*      BEGIN OF ty_t001w,
*        werks     TYPE t001w-werks,
*        name1     TYPE t001w-name1,
*      END   OF ty_t001w,
*
*      BEGIN OF ty_lfa1,
*        lifnr     TYPE lfa1-lifnr,
*        name1     TYPE lfa1-name1,
*      END   OF ty_lfa1,
*
*      BEGIN OF ty_j_1bnfe_active,
*        docnum    TYPE j_1bnfe_active-docnum,
*        docsta    TYPE j_1bnfe_active-docsta,
*        cancel    TYPE j_1bnfe_active-cancel,
*        scssta    TYPE j_1bnfe_active-scssta,
*
*        regio   TYPE j_1bnfe_active-regio,
*        nfyear  TYPE j_1bnfe_active-nfyear,
*        nfmonth TYPE j_1bnfe_active-nfmonth,
*        stcd1   TYPE j_1bnfe_active-stcd1,
*        model   TYPE j_1bnfe_active-model,
*        serie   TYPE j_1bnfe_active-serie,
*        nfnum9  TYPE j_1bnfe_active-nfnum9,
*        docnum9 TYPE j_1bnfe_active-docnum9,
*        cdv     TYPE j_1bnfe_active-cdv,
*      END   OF ty_j_1bnfe_active,
*
*      BEGIN OF ty_vbrp,
*        vbeln   TYPE vbrp-vbeln,
*        kursk   TYPE vbrp-kursk,
*      END   OF ty_vbrp,
*
*      BEGIN OF ty_bkpf,
*        bukrs TYPE bkpf-bukrs,
*        gjahr TYPE bkpf-gjahr,
*        awkey TYPE bkpf-awkey,
*        belnr TYPE bkpf-belnr,
*      END   OF ty_bkpf,
*
*      " Adicionado BSID e BSAD conforme solicitação chamado 44124
*      BEGIN OF ty_bsid,
*        bukrs TYPE bsid-bukrs,
*        belnr TYPE bsid-belnr,
*        gjahr TYPE bsid-gjahr,
*        dmbtr TYPE bsid-dmbtr,
*        dmbe2 TYPE bsid-dmbe2,
*        shkzg TYPE bsid-shkzg,
*      END   OF ty_bsid,
*
*     BEGIN OF ty_bsad,
*       bukrs TYPE bsad-bukrs,
*       belnr TYPE bsad-belnr,
*       gjahr TYPE bsad-gjahr,
*       dmbtr TYPE bsad-dmbtr,
*       dmbe2 TYPE bsad-dmbe2,
*       shkzg TYPE bsad-shkzg,
*     END OF ty_bsad,
*
*      BEGIN OF ty_zdoc_exp,
*        vbeln            TYPE zdoc_exp-vbeln,
*        id_nomeacao_tran TYPE zdoc_exp-id_nomeacao_tran,
*        ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
*        ds_porto         TYPE znom_transporte-ds_porto ,
*        ds_terminal      TYPE znom_transporte-ds_terminal,
*      END   OF ty_zdoc_exp,

  BEGIN OF TY_SAIDA,
    BUKRS           TYPE J_1BNFDOC-BUKRS,
    BRANCH          TYPE J_1BNFDOC-BRANCH, "FILIAL
    CFOP            TYPE J_1BNFLIN-CFOP,   "cfop
    NFENUM          TYPE J_1BNFDOC-NFENUM, "Nr. Nota
    INCO1           TYPE J_1BNFDOC-INCO1,
    SERIES          TYPE J_1BNFDOC-SERIES, "sERIE
    MEINS           TYPE J_1BNFLIN-MEINS,  "UNIDADE
    PSTDAT          TYPE J_1BNFDOC-PSTDAT, "DATA LANÇAMENTO
    DOCDAT          TYPE J_1BNFDOC-DOCDAT, "DATA DOCUMENTO
    MENGE           TYPE J_1BNFLIN-MENGE,  "Quantidade
    NETWRT          TYPE J_1BNFLIN-NETWRT,  "VALOR TOTAL
    BASE_ICMS       TYPE J_1BNFSTX-BASE,   "BASE ICMS
    OUTROS          TYPE J_1BNFSTX-OTHBAS, "Outros
    ICMS            TYPE J_1BNFSTX-TAXVAL, "Vlr.ICMS
    PIS             TYPE J_1BNFSTX-TAXVAL, "Vlr.PIS
    COFINS          TYPE J_1BNFSTX-TAXVAL, "Vlr.COFINS
    ISS             TYPE J_1BNFSTX-TAXVAL, "Vlr.COFINS
    INSS            TYPE J_1BNFSTX-TAXVAL, "Vlr.INSS s/ Fat
    DOCNUM          TYPE J_1BNFDOC-DOCNUM, "Nº documento
    STCD3           TYPE LFA1-STCD3,       "Inscricao Estadual
    EXCBAS          TYPE J_1BNFSTX-EXCBAS, "ISENTAS
    PRODUTO         TYPE MAKT-MAKTX,       "Produto
    MATKL           TYPE T023T-MATKL,
    WGBEZ60         TYPE T023T-WGBEZ60,
    ORDEM           TYPE LIPS-VGBEL,
    REMESSA         TYPE LIPS-VBELN,
    INSTRUCAO       TYPE ZSDT0053-VBELN,
    ROMANEIO        TYPE ZSDT0001-NR_ROMANEIO,
    REF_RO          TYPE ZSDT0001-CH_REFERENCIA,
    UTILIZACAO      TYPE TVLVT-BEZEI,
    DOC_FATURA      TYPE VBFA-VBELN,
    KURSK           TYPE VBRP-KURSK,
    VLR_DOLAR       TYPE J_1BNFLIN-NETWRT,
    CPF_PROD        TYPE LFA1-STCD1,
    UNIT_DOLAR      TYPE P DECIMALS 4,
    STATUS          TYPE C LENGTH 20,      "Status
    NF_STATUS       TYPE C LENGTH 10,      "Estornada / Ativa
    VLR_UNIT        TYPE P DECIMALS 4,
    "nome_clifor      TYPE c LENGTH 35,
    COD_CLIFOR      TYPE LFA1-LIFNR,
    NOME_CLIFOR     TYPE LFA1-NAME1,
    UF_CLIFOR       TYPE LFA1-REGIO,
    ORT01           TYPE LFA1-ORT01,
    CRT_BUPLA       TYPE STRING           , " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC
    PARC_COLETA     TYPE C LENGTH 35,
    CID_COLETA      TYPE LFA1-ORT01,
    UF_COLETA       TYPE LFA1-REGIO,
    LOCAL_ENT       TYPE C LENGTH 35,
    UF_ENT          TYPE LFA1-REGIO,
    TERMINAL        TYPE C LENGTH 35,
    UF_TERMINAL     TYPE LFA1-REGIO,
    NFE             TYPE J_1BNFDOC-NFE,
    DATA(25)        TYPE C,
    USER            TYPE SY-UNAME,
    DOC_CONTABIL    TYPE ZDOC_EXP-VBELN,
    NFTYPE          TYPE J_1BNFDOC-NFTYPE,
    MODEL           TYPE J_1BNFDOC-MODEL,
    REFKEY          TYPE J_1BNFLIN-REFKEY,

    " add
    MAKTX           TYPE J_1BNFLIN-MAKTX,

    TAXLW1          TYPE J_1BNFLIN-TAXLW1,
    TAXLW2          TYPE J_1BATL2-TAXSIT,
    TAXLW4          TYPE J_1BNFLIN-TAXLW4,
    TAXLW5          TYPE J_1BNFLIN-TAXLW5,

    IVA             TYPE RSEG-MWSKZ,
    CHARG           TYPE J_1BNFLIN-CHARG,
    NCM             TYPE J_1BNFLIN-NBM,

    CHAVE_NFE       TYPE CHAR44,

    LEI_ICMS(150),
    LEI_COFINS(150),

    SHTYP           TYPE ZSDT0011-SHTYP,
    ANLN1           TYPE ANLN1,
    STKZN           TYPE LFA1-STKZN,
    NTGEW           TYPE J_1BNFDOC-NTGEW,
    RAZCLI          TYPE SKB1-SAKNR, "Informação da conta razão do cliente  115831 CS2023000374 - PSA
    RAZREC          TYPE SKB1-SAKNR, "Informação da conta razão receita 115831 CS2023000374 - PSA
    WAERS           TYPE BSIS_VIEW-WAERS, "Moeda do documento 115831 CS2023000374 - PSA
    TXT20           TYPE SKAT-TXT20, "Descrição conta razão 115831 CS2023000374 - PSA
  END   OF TY_SAIDA.
*
*      BEGIN OF ty_parceiro,
*        docnum  TYPE j_1bnfnad-docnum,
*        parid   TYPE j_1bnfnad-parid,
*        partyp  TYPE j_1bnfnad-partyp,
*        parvw   TYPE j_1bnfnad-parvw,
*      END OF ty_parceiro,
*
*      BEGIN OF ty_vbap,
*        vbeln TYPE vbap-vbeln ,
*        bezei TYPE tvlvt-bezei,
*      END OF ty_vbap.

TYPES: BEGIN OF TY_ESTRUTURA.
*         INCLUDE TYPE slis_fieldcat_main.
*         INCLUDE TYPE slis_fieldcat_alv_spec.
         INCLUDE TYPE SLIS_FIELDCAT_ALV.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF Y_ZDADOS_SAIDA.
         INCLUDE STRUCTURE ZDADOS_SAIDA.
TYPES:   EBELP           TYPE ZFIWRT0008-EBELP,
         CFOTXT          TYPE J_1BAGNT-CFOTXT,
         BASE            TYPE J_1BNFSTX-BASE,
         CHAVE_REFER     TYPE ZIB_CTE_DIST_N55-N55_CHAVE_ACESSO, " Rubenilson - 17.01.25
         MAT_CHAVE_REFER TYPE MAKT-MAKTX,                        " Rubenilson - 17.01.25
         MTART           TYPE MARA-MTART, "US #167066 - MMSILVA - 17.02.2025
         MTBEZ           TYPE T134T-MTBEZ, "US #167066 - MMSILVA - 17.02.2025
       END OF  Y_ZDADOS_SAIDA.
*&---------------------------------------------------------------------*
*& CONSTANTES
*&---------------------------------------------------------------------*
*CONSTANTS: c_pis(3)    TYPE c VALUE 'PIS',
*           c_ipi(3)    TYPE c VALUE 'IPI',
*           c_icms(4)   TYPE c VALUE 'ICMS',
*           c_cofins(6) TYPE c VALUE 'COFI',
*           c_iss(4)    TYPE c VALUE 'ISSS'.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: "t_j_1bnfdoc TYPE TABLE OF j_1bnfdoc,
*      t_vbfa_doc   TYPE TABLE OF ty_vbfa_doc,
*      t_vbfa_ord   TYPE TABLE OF vbfa,
*      t_j_1bnfstx  TYPE TABLE OF j_1bnfstx,
*      t_j_1baj     TYPE TABLE OF ty_j_1baj,
*      t_j_1bnflin  TYPE TABLE OF j_1bnflin,
*      t_j_1bnflin2 TYPE TABLE OF j_1bnflin WITH HEADER LINE,
*      t_vbpa       TYPE TABLE OF vbpa,
*      t_makt       TYPE TABLE OF makt,
*      t_t001       TYPE TABLE OF ty_t001,
*      t_t001w      TYPE TABLE OF ty_t001w,
*      t_lfa1       TYPE TABLE OF lfa1,
*      t_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active,
*      t_bkpf       TYPE TABLE OF ty_bkpf,
*      t_bsad       TYPE TABLE OF ty_bsad,
*      t_bsid       TYPE TABLE OF ty_bsid,
*      t_vbap       TYPE TABLE OF ty_vbap,
      "   t_saida      TYPE TABLE OF ty_saida.
      T_SAIDA     TYPE TABLE OF  Y_ZDADOS_SAIDA. "zdados_saida.
*      t_remessa    TYPE TABLE OF ty_remessa,
*      t_remessa2   TYPE TABLE OF ty_remessa2,
*      t_remessa_3  TYPE TABLE OF ty_remessa_3,
*      t_parceiro   TYPE TABLE OF ty_parceiro,
*      t_vbrp       TYPE TABLE OF ty_vbrp.


*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: WA_SAIDA    TYPE Y_ZDADOS_SAIDA, "zdados_saida,
      "wa_saida-parid TYPE j_1bnfnad-parid,
      WA_ZSDT0011 TYPE ZSDT0011.
*      wa_j_1bnfdoc      TYPE j_1bnfdoc,
*      wa_vbfa_ord       TYPE vbfa,
*      wa_vbfa_doc       TYPE ty_vbfa_doc,
*      wa_j_cabe         TYPE j_1bindoc,
*      wa_j_1bnfstx      TYPE j_1bnfstx,
*      wa_j_1baj         TYPE ty_j_1baj,
*      wa_j_1bnflin      TYPE j_1bnflin,
*      wa_j_1bnflin_aux  TYPE j_1bnflin,
*      wa_vbpa           TYPE vbpa,
*      wa_makt           TYPE makt,
*      wa_t001           TYPE ty_t001,
*      wa_j_1bnfe_active TYPE ty_j_1bnfe_active,
*      wa_bkpf           TYPE ty_bkpf,
*      wa_bsid           TYPE ty_bsid,
*      wa_bsad           TYPE ty_bsad,
*      wa_remessa        TYPE ty_remessa,
*      wa_remessa2       TYPE ty_remessa2,
*      wa_remessa_3      TYPE ty_remessa_3,

*      wa_parceiro       TYPE ty_parceiro,
*      wa_vbap           TYPE ty_vbap,
*      wa_vbrp           TYPE ty_vbrp.
*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*
*DATA: x_data TYPE d,
*      x_hora TYPE sy-uzeit.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      T_SORT       TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      VG_OBRIG,
      VG_DIAS      TYPE I.

*Inicio Alteração - Leandro Valentim Ferreira - 20.09.23 #107019
DATA: RL_MODEL   TYPE RANGE OF J_1BMODEL.
*Fim Alteração - Leandro Valentim Ferreira - 20.09.23 #107019

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS:      p_bukrs  TYPE ty_j_1bnfdoc-bukrs.  "OBLIGATORY. "CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF / Anderson Oenning

  SELECT-OPTIONS: P_BUKRS  FOR J_1BNFDOC-BUKRS,  "OBLIGATORY. "CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF / Anderson Oenning
                  P_BRANCH FOR VBAP-WERKS,
                  P_DIRECT FOR J_1BNFDOC-DIRECT NO INTERVALS.
*  PARAMETERS:     p_direct TYPE ty_j_1bnfdoc-direct. "OBLIGATORY.

  PARAMETERS: R_OV  LIKE T074U-UMSKZ AS CHECKBOX  DEFAULT ' ' MODIF ID A.

  SELECT-OPTIONS: P_PSTDAT FOR  J_1BNFDOC-PSTDAT NO-EXTENSION," OBLIGATORY
                  P_DOCDAT FOR  J_1BNFDOC-DOCDAT NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: P_TMISS TYPE J_1BNFLIN-TMISS RADIOBUTTON GROUP R1,
              P_TMPRO TYPE J_1BNFLIN-TMISS RADIOBUTTON GROUP R1,
              P_TMTOD TYPE C RADIOBUTTON GROUP R1 DEFAULT 'X'.
  "p_nordem  type vbfa-vbelv.
  "p_nordem  TYPE vbfa-vbelv.
  SELECT-OPTIONS:
                  P_EBELN   FOR  ZDADOS_SAIDA-EBELN, "CS2022000515 - FF  28.11.2022
                  P_NORDEM  FOR  VBFA-VBELV,
                  P_PARID   FOR  J_1BNFDOC-PARID,   "NO INTERVALS NO-EXTENSION,
                  P_DOCNUM  FOR  J_1BNFDOC-DOCNUM,  "NO-EXTENSION,
                  P_NFENUM  FOR  J_1BNFDOC-NFENUM,   "NO-EXTENSION,
                  P_CFOP    FOR  J_1BNFLIN-CFOP,
                  P_MODEL   FOR  J_1BNFDOC-MODEL,
                  P_CRENAM  FOR  J_1BNFDOC-CRENAM NO INTERVALS,
                  P_NFE     FOR  J_1BNFDOC-NFE    NO INTERVALS NO-EXTENSION,
                  P_NFNUM   FOR  J_1BNFDOC-NFNUM  NO INTERVALS NO-EXTENSION,
                  P_CHAVE   FOR  ZIB_NFE_DIST_ITM-CHAVE_NFE NO INTERVALS,
                  P_MAT     FOR  J_1BNFLIN-MATNR,
                  P_MATKL   FOR  J_1BNFLIN-MATKL.
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: P_ATIVAS TYPE TY_J_1BNFDOC-CANCEL DEFAULT 'X',
              P_NATIVA TYPE TY_J_1BNFDOC-CANCEL.
SELECTION-SCREEN: END OF BLOCK B3.

SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: P_AUTOR  TYPE TY_J_1BNFDOC-CANCEL DEFAULT 'X',
              P_REJEIT TYPE TY_J_1BNFDOC-CANCEL,
              P_RECUS  TYPE TY_J_1BNFDOC-CANCEL,
              P_CANCEL TYPE TY_J_1BNFDOC-CANCEL,
              P_AGRES  TYPE TY_J_1BNFDOC-CANCEL,
              P_NENV   TYPE TY_J_1BNFDOC-CANCEL.
SELECTION-SCREEN: END OF BLOCK B4.

SELECTION-SCREEN: BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-006.
  PARAMETERS: P_GRAVR AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK B5.

SELECTION-SCREEN: BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-007.
  PARAMETERS: P_FREPRO AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK B6.

* CS2023000865 Filtros Leiaute ZCONF - Ini - RJF - 2024.01.19
SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE TEXT-008.
  PARAMETERS: P_LAYOUT        TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B7.

*=============================================================================*
*AT SELECT SCREEN
*=============================================================================*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LAYOUT.
  PERFORM F_ALV_VARIANT_F4 CHANGING P_LAYOUT.

* CS2023000865 Filtros Leiaute ZCONF - Ini - RJF - 2024.01.19

INITIALIZATION.

  CLEAR: VG_DIAS.
  SELECT SINGLE LOW
    FROM TVARVC
    INTO @DATA(LV_DIAS)
    WHERE NAME = @C_ZCONF_DIAS_FILTRO.

  IF SY-SUBRC = 0.
    VG_DIAS = LV_DIAS.
  ENDIF.



AT SELECTION-SCREEN.

  VG_OBRIG = ABAP_FALSE.

  LOOP AT SCREEN.

    IF P_DOCNUM IS INITIAL.
      VG_OBRIG = ABAP_TRUE.
    ENDIF.

    CASE VG_OBRIG.
      WHEN ABAP_TRUE.
        IF P_BUKRS IS INITIAL AND P_DOCNUM IS INITIAL AND P_CHAVE IS INITIAL.
          MESSAGE 'Campo Empresa é obrigatório!' TYPE 'E'.
          EXIT.
        ENDIF.
        IF P_BRANCH IS INITIAL AND P_DOCNUM IS INITIAL AND P_CHAVE IS INITIAL.
          MESSAGE 'Campo Filial é obrigatório!' TYPE 'E'.
          EXIT.
        ENDIF.
        IF P_DIRECT IS INITIAL AND P_DOCNUM IS INITIAL AND P_CHAVE IS INITIAL.
          MESSAGE 'Campo Entrada/Saida é obrigatório!' TYPE 'E'.
          EXIT.
        ENDIF.

        IF P_PSTDAT IS INITIAL AND P_DOCNUM IS INITIAL AND P_CHAVE IS INITIAL.
          MESSAGE 'Campo Data de Lançamento é obrigatório!' TYPE 'E'.
          EXIT.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  IF P_PSTDAT IS NOT INITIAL.

    DATA: LV_TIME     TYPE SY-UZEIT,
          LV_QTD_DIAS TYPE P,
          LV_DIAS_STR TYPE STRING,
          LV_MSG_DIAS TYPE STRING.

    IF VG_DIAS IS NOT INITIAL.

      LV_TIME = 1.

      CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
        EXPORTING
          DATE1            = P_PSTDAT-LOW
          TIME1            = LV_TIME
          DATE2            = P_PSTDAT-HIGH
          TIME2            = LV_TIME
        IMPORTING
          DATEDIFF         = LV_QTD_DIAS
        EXCEPTIONS
          INVALID_DATETIME = 1
          OTHERS           = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      IF LV_QTD_DIAS > VG_DIAS.
        LV_DIAS_STR = VG_DIAS.
        CONCATENATE 'Período superior ao configurado:' LV_DIAS_STR 'dias' INTO LV_MSG_DIAS SEPARATED BY SPACE.
        MESSAGE LV_MSG_DIAS TYPE 'E'.
        EXIT.
      ENDIF.

    ENDIF.

  ENDIF.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF P_DIRECT = '1'.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'A'.
        CLEAR R_OV.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  IF P_MODEL[] IS INITIAL.
    P_MODEL[] = VALUE #( SIGN = 'I' OPTION = 'NE' ( LOW = '58' HIGH = '58' ) ).
  ENDIF.

  PERFORM F_INICIAR_VARIAVES.

* Field list for SELECT statement
  TYPES: SBIWA_S_FIELDS LIKE RSFIELDSEL,
         SBIWA_T_FIELDS TYPE SBIWA_S_FIELDS OCCURS 0.

* Selection criteria for SELECT statement
  TYPES: SBIWA_S_SELECT  LIKE RSSELECT,
         SBIWA_T_SELECT  TYPE SBIWA_S_SELECT OCCURS 0,
         SBIWA_TX_SELECT TYPE SBIWA_T_SELECT OCCURS 0.

  DATA: GT_DATA_DOC   TYPE TABLE OF J_1BNFDOC,
        GT_DATA_LIN   TYPE TABLE OF J_1BNFLIN,
        I_T_SELECT    TYPE SBIWA_T_SELECT,
        WA_I_T_SELECT TYPE SBIWA_S_SELECT.

  "CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF
*  IF p_bukrs IS NOT INITIAL .
*    CLEAR wa_i_t_select.
*    wa_i_t_select-fieldnm = 'bukrs'.
*    wa_i_t_select-sign    = 'I'.
*    wa_i_t_select-option  = 'EQ'.
*    wa_i_t_select-low     = p_bukrs.
**  wa_i_t_select-high    = p_bukrs-hight[].
*
*    APPEND wa_i_t_select TO i_t_select.
*  ENDIF.

  IF P_BUKRS IS NOT INITIAL .
    LOOP AT P_BUKRS .
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'bukrs'.
      WA_I_T_SELECT-SIGN    = P_BUKRS-SIGN.
      WA_I_T_SELECT-OPTION  = P_BUKRS-OPTION.
      WA_I_T_SELECT-LOW     = P_BUKRS-LOW.
      WA_I_T_SELECT-HIGH    = P_BUKRS-HIGH.
      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.
  "CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF

  DATA: WA_BRANCH TYPE SBIWA_S_SELECT.

  IF P_BRANCH IS NOT INITIAL .
    LOOP AT P_BRANCH .
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'branch'.
      WA_I_T_SELECT-SIGN    = P_BRANCH-SIGN.
      WA_I_T_SELECT-OPTION  = P_BRANCH-OPTION.
      WA_I_T_SELECT-LOW     = P_BRANCH-LOW .
      WA_I_T_SELECT-HIGH    = P_BRANCH-HIGH.
      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_DIRECT IS NOT INITIAL .
*CS2024000139 - Inclusão de campos | ITSOUZA
    LOOP AT P_DIRECT.
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'direct'.
      WA_I_T_SELECT-SIGN    = P_DIRECT-SIGN.   "'I'.
      WA_I_T_SELECT-OPTION  = P_DIRECT-OPTION. "'EQ'.
      WA_I_T_SELECT-LOW     = P_DIRECT-LOW .
      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_PSTDAT IS NOT INITIAL .
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM = 'pstdat'.
    WA_I_T_SELECT-SIGN    =  P_PSTDAT-SIGN.
    WA_I_T_SELECT-OPTION  =  P_PSTDAT-OPTION.
    WA_I_T_SELECT-LOW     =  P_PSTDAT-LOW .
    WA_I_T_SELECT-HIGH    =  P_PSTDAT-HIGH.

    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  IF P_DOCDAT IS NOT INITIAL .
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM = 'docdat'.
    WA_I_T_SELECT-SIGN    =  P_DOCDAT-SIGN.
    WA_I_T_SELECT-OPTION  =  P_DOCDAT-OPTION.
    WA_I_T_SELECT-LOW     =  P_DOCDAT-LOW .
    WA_I_T_SELECT-HIGH    =  P_DOCDAT-HIGH.

    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  IF P_TMISS IS NOT INITIAL .
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM = 'tmiss'.
    WA_I_T_SELECT-SIGN    = 'I'.
    WA_I_T_SELECT-OPTION  = 'EQ'. "P_TMISS-OPTION'.
    WA_I_T_SELECT-LOW     =  P_TMISS .
    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  IF P_TMPRO IS NOT INITIAL.
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM = 'tmpro'.
    WA_I_T_SELECT-SIGN    = 'I'.
    WA_I_T_SELECT-OPTION  = 'EQ'. "P_TMISS-OPTION'.
    WA_I_T_SELECT-LOW     =  P_TMPRO .
    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  IF P_TMTOD IS NOT INITIAL.
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM = 'tmptod'.
    WA_I_T_SELECT-SIGN    = 'I'.
    WA_I_T_SELECT-OPTION  = 'EQ'. "P_TMISS-OPTION'.
    WA_I_T_SELECT-LOW     =  P_TMTOD .
    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  IF P_NORDEM IS NOT INITIAL .
    LOOP AT P_NORDEM .
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'nordem'.
      WA_I_T_SELECT-SIGN    =  P_NORDEM-SIGN.
      WA_I_T_SELECT-OPTION  =  P_NORDEM-OPTION.
      WA_I_T_SELECT-LOW     =  P_NORDEM-LOW .
      WA_I_T_SELECT-HIGH    =  P_NORDEM-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_PARID IS NOT INITIAL .
    LOOP AT P_PARID.
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'parid'.
      WA_I_T_SELECT-SIGN    =  P_PARID-SIGN.
      WA_I_T_SELECT-OPTION  =  P_PARID-OPTION.
      WA_I_T_SELECT-LOW     =  P_PARID-LOW .
      WA_I_T_SELECT-HIGH    =  P_PARID-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_DOCNUM IS NOT INITIAL .
    LOOP AT P_DOCNUM.
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'docnum'.
      WA_I_T_SELECT-SIGN    =  P_DOCNUM-SIGN.
      WA_I_T_SELECT-OPTION  =  P_DOCNUM-OPTION.
      WA_I_T_SELECT-LOW     =  P_DOCNUM-LOW.
      WA_I_T_SELECT-HIGH    =  P_DOCNUM-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_NFENUM IS NOT INITIAL .
    LOOP AT P_NFENUM. "Incluindo opção de seleção multiplas.#84724 / Anderson Oenning
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'nfenum'.
      WA_I_T_SELECT-SIGN    = P_NFENUM-SIGN.
      WA_I_T_SELECT-OPTION  = P_NFENUM-OPTION.
      WA_I_T_SELECT-LOW     = P_NFENUM-LOW .
      WA_I_T_SELECT-HIGH    = P_NFENUM-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.



  IF P_CFOP IS NOT INITIAL .
    LOOP AT P_CFOP.
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'cfop'.
      WA_I_T_SELECT-SIGN    = P_CFOP-SIGN.
      WA_I_T_SELECT-OPTION  = P_CFOP-OPTION.
      WA_I_T_SELECT-LOW     = P_CFOP-LOW .
      WA_I_T_SELECT-HIGH    = P_CFOP-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_MODEL[] IS NOT INITIAL .
    LOOP AT P_MODEL INTO DATA(WA_MODEL).
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'model'.
      WA_I_T_SELECT-SIGN    = WA_MODEL-SIGN.
      WA_I_T_SELECT-OPTION  = WA_MODEL-OPTION.
      WA_I_T_SELECT-LOW     = WA_MODEL-LOW .
      WA_I_T_SELECT-HIGH    = WA_MODEL-HIGH.
      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_CRENAM IS NOT INITIAL .
    LOOP AT P_CRENAM. "US 159943 - MMSILVA - 18.12.2024
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'crenam'.
      WA_I_T_SELECT-SIGN    = P_CRENAM-SIGN.
      WA_I_T_SELECT-OPTION  = P_CRENAM-OPTION.
      WA_I_T_SELECT-LOW     = P_CRENAM-LOW .
      WA_I_T_SELECT-HIGH    = P_CRENAM-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_NFE IS NOT INITIAL .
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM = 'nfe'.
    WA_I_T_SELECT-SIGN    = P_NFE-SIGN.
    WA_I_T_SELECT-OPTION  = P_NFE-OPTION.
    WA_I_T_SELECT-LOW     = P_NFE-LOW.
    WA_I_T_SELECT-HIGH    = P_NFE-HIGH.

    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  "Chave NFe.
  IF P_CHAVE IS NOT INITIAL .
    LOOP AT P_CHAVE INTO DATA(WS_CHAVE).
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'chave'.
      WA_I_T_SELECT-SIGN    = WS_CHAVE-SIGN.
      WA_I_T_SELECT-OPTION  = WS_CHAVE-OPTION.
      WA_I_T_SELECT-LOW     = WS_CHAVE-LOW.
      WA_I_T_SELECT-HIGH    = WS_CHAVE-HIGH.
      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_NFNUM IS NOT INITIAL .
    CLEAR WA_I_T_SELECT.
    WA_I_T_SELECT-FIELDNM =  'nfnum'.
    WA_I_T_SELECT-SIGN    =  P_NFNUM-SIGN.
    WA_I_T_SELECT-OPTION  =  P_NFNUM-OPTION.
    WA_I_T_SELECT-LOW     =  P_NFNUM-LOW .
    WA_I_T_SELECT-HIGH    =  P_NFNUM-HIGH.

    APPEND WA_I_T_SELECT TO I_T_SELECT.
  ENDIF.

  IF P_MAT IS NOT INITIAL .
    LOOP AT P_MAT.
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'mat'.
      WA_I_T_SELECT-SIGN    = P_MAT-SIGN.
      WA_I_T_SELECT-OPTION  = P_MAT-OPTION.
      WA_I_T_SELECT-LOW     = P_MAT-LOW .
      WA_I_T_SELECT-HIGH    = P_MAT-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.

  IF P_MATKL IS NOT INITIAL .
    LOOP AT P_MATKL.
      CLEAR WA_I_T_SELECT.
      WA_I_T_SELECT-FIELDNM = 'matkl'.
      WA_I_T_SELECT-SIGN    = P_MATKL-SIGN.
      WA_I_T_SELECT-OPTION  = P_MATKL-OPTION.
      WA_I_T_SELECT-LOW     = P_MATKL-LOW .
      WA_I_T_SELECT-HIGH    = P_MATKL-HIGH.

      APPEND WA_I_T_SELECT TO I_T_SELECT.
    ENDLOOP.
  ENDIF.
*CALL FUNCTION 'ZF_BW_FI_05'
*  EXPORTING
*    p_ativas   = p_ativas
*    p_nativa   = p_nativa
*    p_autor    = p_autor
*    p_rejeit   = p_rejeit
*    p_recus    = p_recus
*    p_cancel   = p_cancel
*    p_agres    = p_agres
*    p_nenv     = p_nenv
*    p_gravr    = p_gravr
*  IMPORTING
*    e_t_data   = t_saida
*  TABLES
*    i_t_select = i_t_select.


  EXPORT P1 = R_OV   TO MEMORY ID 'MZCONFX'.

  CALL FUNCTION 'ZF_BW_FI_05'
    EXPORTING
      P_ATIVAS   = P_ATIVAS
      P_NATIVA   = P_NATIVA
      P_AUTOR    = P_AUTOR
      P_REJEIT   = P_REJEIT
      P_RECUS    = P_RECUS
      P_CANCEL   = P_CANCEL
      P_AGRES    = P_AGRES
      P_NENV     = P_NENV
      P_GRAVR    = P_GRAVR
      P_FREPRO   = P_FREPRO
    TABLES
      I_T_SELECT = I_T_SELECT
      E_T_DATA   = T_SAIDA.

*Inicio Alteração - Leandro Valentim Ferreira - 20.09.23 #107019
  IF P_TMPRO EQ ABAP_TRUE.
    APPEND INITIAL LINE TO RL_MODEL ASSIGNING FIELD-SYMBOL(<FS_MODEL>).
    <FS_MODEL>-SIGN   = 'I'.
    <FS_MODEL>-OPTION = 'EQ'.
    <FS_MODEL>-LOW    = '55'.

    APPEND INITIAL LINE TO RL_MODEL ASSIGNING <FS_MODEL>.
    <FS_MODEL>-SIGN   = 'I'.
    <FS_MODEL>-OPTION = 'EQ'.
    <FS_MODEL>-LOW    = '57'.

    APPEND INITIAL LINE TO RL_MODEL ASSIGNING <FS_MODEL>.
    <FS_MODEL>-SIGN   = 'I'.
    <FS_MODEL>-OPTION = 'EQ'.
    <FS_MODEL>-LOW    = '66'.

    APPEND INITIAL LINE TO RL_MODEL ASSIGNING <FS_MODEL>.
    <FS_MODEL>-SIGN   = 'I'.
    <FS_MODEL>-OPTION = 'EQ'.
    <FS_MODEL>-LOW    = '67'.

    DELETE T_SAIDA WHERE MODEL NOT IN RL_MODEL.
  ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 20.09.23 #107019

* CS2023000865 Filtros Leiaute ZCONF - Ini - RJF - 2024.01.19
*  IF p_chave[] IS NOT INITIAL.
*    DELETE t_saida WHERE chave_nfe NOT IN p_chave.
*  ENDIF.
* CS2023000865 Filtros Leiaute ZCONF - Fim - RJF - 2024.01.19

  " PERFORMANCE - PFERRAZ - 04/12/23 - Inicio

  IF T_SAIDA IS NOT INITIAL.
*** Inicio - Rubenilson  - 16.01.25 - US144011
    SELECT *
      FROM ZIB_CTE_DIST_N55
      INTO TABLE @DATA(LT_CTE_DIST)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE N55_CHAVE_ACESSO = @T_SAIDA-CHAVE_NFE.
    IF SY-SUBRC IS INITIAL.
      SORT LT_CTE_DIST BY CD_CHAVE_CTE.

      DATA(LT_CTE_DIST_AUX) = LT_CTE_DIST.
      SORT LT_CTE_DIST_AUX BY DOCNUM_NFE.
      DELETE ADJACENT DUPLICATES FROM LT_CTE_DIST_AUX COMPARING DOCNUM_NFE.

      SELECT *
        FROM J_1BNFLIN
        INTO TABLE @DATA(LT_LIN)
        FOR ALL ENTRIES IN @LT_CTE_DIST_AUX
        WHERE DOCNUM = @LT_CTE_DIST_AUX-DOCNUM_NFE.
      IF SY-SUBRC IS INITIAL.
        SORT LT_LIN BY DOCNUM.
      ENDIF.

      LT_CTE_DIST_AUX = LT_CTE_DIST.
      SORT LT_CTE_DIST_AUX BY TKNUM.
      DELETE ADJACENT DUPLICATES FROM LT_CTE_DIST_AUX COMPARING TKNUM.

      SELECT A~TKNUM,B~BEZEI
        FROM VTTK AS A
        INNER JOIN TVTKT AS B
        ON B~SHTYP = A~SHTYP
        AND B~SPRAS = @SY-LANGU
        INTO TABLE @DATA(LT_VTTK)
        FOR ALL ENTRIES IN @LT_CTE_DIST_AUX
        WHERE A~TKNUM = @LT_CTE_DIST_AUX-TKNUM.
      IF SY-SUBRC IS INITIAL.
        SORT LT_VTTK BY TKNUM.
      ENDIF.

    ENDIF.
*** Fim - Rubenilson  - 16.01.25 - US144011

    SELECT BELNR, BUKRS, DMBTR, HKONT, WAERS
      FROM BSIS_VIEW
      INTO TABLE @DATA(LT_CONTABIL)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE BELNR = @T_SAIDA-DOC_CONTABIL.

    IF SY-SUBRC = 0.

      SELECT BUKRS, KTOPL
        FROM T001
        INTO TABLE @DATA(LT_T001)
        FOR ALL ENTRIES IN @LT_CONTABIL
        WHERE BUKRS = @LT_CONTABIL-BUKRS.

      SELECT BUKRS, SAKNR, MITKZ
        FROM SKB1
        INTO TABLE @DATA(LT_SKB1)
        FOR ALL ENTRIES IN @LT_CONTABIL
         WHERE BUKRS = @LT_CONTABIL-BUKRS
           AND SAKNR = @LT_CONTABIL-HKONT.

      IF SY-SUBRC = 0.

        SELECT SAKNR, KTOPL, TXT20
          FROM SKAT
          INTO TABLE @DATA(LT_SKAT)
          FOR ALL ENTRIES IN @LT_SKB1
          WHERE SPRAS = @SY-LANGU
            AND SAKNR = @LT_SKB1-SAKNR.

      ENDIF.

    ENDIF.

*    SELECT docnum, direct FROM j_1bnfdoc
*      INTO TABLE @DATA(t_j_1bnfdoc)
*      FOR ALL ENTRIES IN @t_saida
*      WHERE docnum EQ @t_saida-docnum.

    SELECT CFOP, CFOTXT FROM J_1BAGNT
      INTO TABLE @DATA(T_J_1BAGNT)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE CFOP EQ @T_SAIDA-CFOP.

*** Stefanini - IR206379 - 18/11/2024 - LAZAROSR - Início de Alteração
*    SELECT docnum, base FROM j_1bnfstx
    SELECT DOCNUM, ITMNUM, BASE FROM J_1BNFSTX
*** Stefanini - IR206379 - 18/11/2024 - LAZAROSR - Fim de Alteração
      INTO TABLE @DATA(T_J_1BNFSTX)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE DOCNUM EQ @T_SAIDA-DOCNUM
        AND TAXGRP EQ 'PIS'.

    SORT: LT_CONTABIL BY BELNR DESCENDING DMBTR,
          LT_T001 BY BUKRS,
          LT_SKB1 BY BUKRS SAKNR,
          LT_SKAT BY SAKNR KTOPL,
*** Stefanini - IR206379 - 18/11/2024 - LAZAROSR - Início de Alteração
          T_J_1BNFSTX BY DOCNUM ITMNUM.
*** Stefanini - IR206379 - 18/11/2024 - LAZAROSR - Fim de Alteração

    "US #167066 - MMSILVA - 17.02.2025 - Inicio
    SELECT MATNR, MTART
      FROM MARA
      INTO TABLE @DATA(IT_MTART)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE MATNR = @T_SAIDA-PRODUTO.

    IF IT_MTART IS NOT INITIAL.
      SELECT MTART, MTBEZ
        FROM T134T
        INTO TABLE @DATA(IT_MTBEZ)
        FOR ALL ENTRIES IN @IT_MTART
        WHERE MTART = @IT_MTART-MTART
        AND   SPRAS = 'P'.
    ENDIF.
    "US #167066 - MMSILVA - 17.02.2025 - Fim

  ENDIF.

* Ini - RJF - CS2024000160 ZCONF - AJUSTE INFORMAÇÃO COLUNA
  SELECT SHTYP, BEZEI FROM TVTKT
    INTO TABLE @DATA(IT_TVTK)
    FOR ALL ENTRIES IN @T_SAIDA
    WHERE SHTYP EQ @T_SAIDA-SHTYP
       AND SPRAS EQ 'P'.
  IF SY-SUBRC IS INITIAL.
    SORT IT_TVTK BY SHTYP.
  ENDIF.
* Fim - RJF - CS2024000160 ZCONF - AJUSTE INFORMAÇÃO COLUNA

  LOOP AT T_SAIDA ASSIGNING FIELD-SYMBOL(<T_SAIDA>).


*    READ TABLE t_j_1bnfdoc INTO DATA(s_j_1bnfdoc) WITH KEY docnum = <t_saida>-docnum.
*    IF sy-subrc EQ 0.
*      <t_saida>-direct = s_j_1bnfdoc-direct.
*    ENDIF.

* Ini - RJF - CS2024000160 ZCONF - AJUSTE INFORMAÇÃO COLUNA
    READ TABLE IT_TVTK INTO DATA(WA_TVTK) WITH KEY SHTYP = <T_SAIDA>-SHTYP
                                          BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      <T_SAIDA>-BEZEI        = <T_SAIDA>-SHTYP && '-' && WA_TVTK-BEZEI.
    ELSE.
      <T_SAIDA>-BEZEI        = <T_SAIDA>-SHTYP.
    ENDIF.
* Fim - RJF - CS2024000160 ZCONF - AJUSTE INFORMAÇÃO COLUNA

*** Inicio - Rubenilson  - 16.01.25 - US144011
    READ TABLE LT_CTE_DIST ASSIGNING FIELD-SYMBOL(<FS_CTE_DIST>)
    WITH KEY N55_CHAVE_ACESSO = <T_SAIDA>-CHAVE_NFE
    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      <T_SAIDA>-CHAVE_REFER = <FS_CTE_DIST>-N55_CHAVE_ACESSO.

      "READ TABLE lt_lin ASSIGNING FIELD-SYMBOL(<fs_lin>)
      LOOP AT LT_LIN INTO DATA(WA_LIN) WHERE DOCNUM = <FS_CTE_DIST>-DOCNUM_NFE.
        "WITH KEY docnum = <fs_cte_dist>-docnum_nfe
        "BINARY SEARCH.
        "IF sy-subrc IS INITIAL.
        IF <T_SAIDA>-MAT_CHAVE_REFER IS INITIAL.
          <T_SAIDA>-MAT_CHAVE_REFER = WA_LIN-MAKTX.
        ELSE.
          IF <T_SAIDA>-MAT_CHAVE_REFER = WA_LIN-MAKTX.
            CONTINUE.
          ELSE.
            CONCATENATE <T_SAIDA>-MAT_CHAVE_REFER '/' WA_LIN-MAKTX INTO <T_SAIDA>-MAT_CHAVE_REFER  SEPARATED BY SPACE.
            "<t_saida>-mat_chave_refer = wa_lin-maktx.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF <T_SAIDA>-BEZEI IS INITIAL.
        READ TABLE LT_VTTK ASSIGNING FIELD-SYMBOL(<FS_VTTK>)
        WITH KEY TKNUM = <FS_CTE_DIST>-TKNUM
        BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          <T_SAIDA>-BEZEI = <FS_VTTK>-BEZEI.
        ENDIF.
      ENDIF.

    ENDIF.
*** Fim - Rubenilson  - 16.01.25 - US144011
    READ TABLE T_J_1BAGNT INTO DATA(S_J_1BAGNT) WITH KEY CFOP = <T_SAIDA>-CFOP.
    IF SY-SUBRC EQ 0.
      <T_SAIDA>-CFOTXT = S_J_1BAGNT-CFOTXT.
    ENDIF.

    READ TABLE T_J_1BNFSTX INTO DATA(S_J_1BNFSTX) WITH KEY DOCNUM = <T_SAIDA>-DOCNUM
*** Stefanini - IR206379 - 18/11/2024 - LAZAROSR - Início de Alteração
                                                           ITMNUM = <T_SAIDA>-ITMNUM
                                                                      BINARY SEARCH.
*** Stefanini - IR206379 - 18/11/2024 - LAZAROSR - Fim de Alteração
    IF SY-SUBRC EQ 0.
      <T_SAIDA>-BASE = S_J_1BNFSTX-BASE.
    ENDIF.

    IF <T_SAIDA>-DOC_CONTABIL IS NOT INITIAL. "PSA

      READ TABLE LT_CONTABIL ASSIGNING FIELD-SYMBOL(<FS_CONTABIL>)
              WITH KEY BELNR = <T_SAIDA>-DOC_CONTABIL BINARY SEARCH.

      IF SY-SUBRC = 0.

        READ TABLE LT_T001 ASSIGNING FIELD-SYMBOL(<FS_T001>)
              WITH KEY BUKRS = <T_SAIDA>-BUKRS
              BINARY SEARCH.

        IF SY-SUBRC = 0.

          READ TABLE LT_SKB1 ASSIGNING FIELD-SYMBOL(<FS_SKB1>)
            WITH KEY BUKRS = <FS_T001>-BUKRS
                     SAKNR = <FS_CONTABIL>-HKONT
                     BINARY SEARCH.

          IF SY-SUBRC = 0.

            READ TABLE LT_SKAT ASSIGNING FIELD-SYMBOL(<FS_SKAT>)
            WITH KEY SAKNR = <FS_SKB1>-SAKNR
                     KTOPL = <FS_T001>-KTOPL
                     BINARY SEARCH.

            IF SY-SUBRC = 0.

              CASE <FS_SKB1>-MITKZ.
                WHEN 'D'.
                  <T_SAIDA>-RAZCLI = <FS_CONTABIL>-HKONT.
                  <T_SAIDA>-WAERS = <FS_CONTABIL>-WAERS.
                  <T_SAIDA>-TXT20 = <FS_SKAT>-TXT20.
                WHEN OTHERS.
                  <T_SAIDA>-RAZREC = <FS_CONTABIL>-HKONT.
                  <T_SAIDA>-WAERS = <FS_CONTABIL>-WAERS.
                  <T_SAIDA>-TXT20 = <FS_SKAT>-TXT20.
              ENDCASE.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    "US #167066 - MMSILVA - 17.02.2025 - Inicio
    READ TABLE IT_MTART INTO DATA(LR_MTART) WITH KEY MATNR = <T_SAIDA>-PRODUTO.

    IF SY-SUBRC EQ 0.
      <T_SAIDA>-MTART = LR_MTART-MTART.

      READ TABLE IT_MTBEZ INTO DATA(LR_MTBEZ) WITH KEY MTART = LR_MTART-MTART.

      IF SY-SUBRC EQ 0.
        <T_SAIDA>-MTBEZ = LR_MTBEZ-MTBEZ.
      ENDIF.
    ENDIF.
    "US #167066 - MMSILVA - 17.02.2025 - Fim

  ENDLOOP.

  " PERFORMANCE - PFERRAZ - 04/12/23 - Inicio



**  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<t_saida>).
**
**    IF <t_saida>-doc_contabil IS NOT INITIAL. "PSA
**
**      DATA aux_dmbrt TYPE bsis_view-dmbtr.
**
**      SELECT DISTINCT a~dmbtr, c~saknr, a~waers,d~txt20
**      FROM bsis_view AS a
**      INNER JOIN t001 AS b ON b~bukrs = a~bukrs
**      INNER JOIN skb1 AS c ON c~bukrs = a~bukrs AND c~saknr = a~hkont
**      INNER JOIN skat AS d ON d~saknr = c~saknr AND d~spras = b~spras AND d~ktopl = b~ktopl
**      WHERE a~belnr = @<t_saida>-doc_contabil
**      AND c~mitkz = 'D'
**      ORDER BY a~dmbtr DESCENDING
**      INTO (@aux_dmbrt,@<t_saida>-razcli,@<t_saida>-waers,@<t_saida>-txt20)
**      UP TO 1 ROWS.
**      ENDSELECT.
**
**      SELECT DISTINCT a~dmbtr,c~saknr,a~waers,d~txt20
**      FROM bsis_view AS a
**      INNER JOIN t001 AS b ON b~bukrs = a~bukrs
**      INNER JOIN skb1 AS c ON c~bukrs = a~bukrs AND c~saknr = a~hkont
**      INNER JOIN skat AS d ON d~saknr = c~saknr AND d~spras = b~spras AND d~ktopl = b~ktopl
**      WHERE a~belnr = @<t_saida>-doc_contabil
**      AND c~mitkz <> 'D'
**      ORDER BY a~dmbtr DESCENDING
**      INTO (@aux_dmbrt,@<t_saida>-razrec,@<t_saida>-waers,@<t_saida>-txt20)
**      UP TO 1 ROWS.
**      ENDSELECT.
**
**    ENDIF.
**
**  ENDLOOP.




  PERFORM F_BUSCAR_NUMERO_PEDIDO.

*PERFORM f_seleciona_dados.
*PERFORM f_organiza_dados.
*PERFORM f_gravar_dados.


  PERFORM F_IMPRIME_DADOS.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_seleciona_dados .
*
*  DATA : t_j_1bnfstx_aux TYPE TABLE OF j_1bnfstx ,
*         t_j_1bnflin_aux TYPE TABLE OF j_1bnflin ,
*         t_vbpa_aux      TYPE TABLE OF vbpa      ,
*         t_remessa_aux   TYPE TABLE OF ty_remessa,
*         vg_tabix        TYPE          sy-tabix  .
*
*  IF NOT p_nordem IS INITIAL AND p_direct EQ 1.
*    MESSAGE i000(z01) WITH 'O n° da Ordem de venda pode se informado '
*                           'somente para saída' .
*    STOP.
*  ENDIF.
*
*  IF p_ativas NE 'X' AND  p_nativa NE 'X' AND p_autor  NE 'X' AND p_rejeit NE 'X' AND p_recus  NE 'X' AND
*    p_cancel NE 'X' AND  p_agres  NE 'X' AND p_nenv   NE 'X' .
*
*    MESSAGE i000(z01) WITH 'Selecione pelo menos uma opção de Status da NF !' .
*    STOP.
*  ENDIF.
*
*  SELECT *
*    FROM j_1bnfdoc
*    INTO TABLE t_j_1bnfdoc
*    WHERE bukrs  EQ p_bukrs
*      AND branch IN p_branch
*      AND direct EQ p_direct
*      AND model  IN p_model
*      AND docnum IN p_docnum
*      AND crenam IN p_crenam
*      AND nfenum IN p_nfenum
*      AND parid  IN p_parid
*      AND nfe    IN p_nfe
*      AND nfnum  IN p_nfnum
*      AND pstdat IN p_pstdat
*      AND docdat IN p_docdat
*      AND doctyp NE '5'. "Estorno
*
*
*  IF t_j_1bnfdoc IS NOT INITIAL.
*
*
*
*    "------Parceiros da Nota--------
*    SELECT docnum parid partyp parvw
*      FROM j_1bnfnad
*      INTO TABLE t_parceiro
*      FOR ALL ENTRIES IN t_j_1bnfdoc
*        WHERE docnum EQ t_j_1bnfdoc-docnum.
*
*    "------Status nfe ---------
*    SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
*      FROM j_1bnfe_active
*      INTO TABLE t_j_1bnfe_active
*      FOR ALL ENTRIES IN t_j_1bnfdoc
*      WHERE docnum EQ t_j_1bnfdoc-docnum.
*
*    "--------Itens -------------
*    SELECT *
*      FROM j_1bnflin
*      INTO TABLE t_j_1bnflin
*      FOR ALL ENTRIES IN t_j_1bnfdoc
*      WHERE docnum EQ t_j_1bnfdoc-docnum
*        AND cfop   IN p_cfop
*        AND matnr  IN p_mat
*        AND ( tmiss  EQ p_tmiss OR tmiss  EQ ' ' ).
*
*    IF sy-subrc IS INITIAL.
*
*      IF p_nordem[] IS NOT INITIAL.
*
*        SELECT *
*          FROM vbfa
*          INTO TABLE t_vbfa_ord
*          FOR ALL ENTRIES IN t_j_1bnflin
*          WHERE vbeln EQ t_j_1bnflin-refkey(10)
*          AND vbtyp_n NE 'N'
*          AND vbtyp_v NE 'M'.
*
*        DELETE t_vbfa_ord WHERE vbelv NOT IN p_nordem.
*
*        IF  t_vbfa_ord IS NOT INITIAL.
*
*          LOOP AT t_vbfa_ord INTO wa_vbfa_ord.
*
*            READ TABLE t_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey  = wa_vbfa_ord-vbeln.
*            IF sy-subrc IS  INITIAL.
*              wa_vbfa_doc =  wa_j_1bnflin-docnum.
*              APPEND wa_vbfa_doc TO t_vbfa_doc.
*            ENDIF.
*
*          ENDLOOP.
*
*          IF t_vbfa_doc IS NOT INITIAL.
*            LOOP AT t_j_1bnfdoc INTO wa_j_1bnfdoc.
*              vg_tabix = sy-tabix.
*              READ TABLE t_vbfa_doc INTO wa_vbfa_doc WITH KEY docnum = wa_j_1bnfdoc-docnum.
*              IF sy-subrc IS NOT INITIAL.
*                CLEAR: wa_j_1bnfdoc-docnum.
*                MODIFY t_j_1bnfdoc INDEX vg_tabix FROM wa_j_1bnfdoc TRANSPORTING docnum.
*              ENDIF.
*            ENDLOOP.
*
*            DELETE t_j_1bnfdoc WHERE docnum EQ space.
*
*          ENDIF.
*          "ELSE.
*          "MESSAGE i000(z01) WITH 'O n° da Ordem de venda não encontrado '.
*          "EXIT.
*        ENDIF.
*
*        CLEAR: wa_j_1bnfdoc,
*               wa_vbfa_ord,
*               wa_vbfa_doc,
*               wa_j_1bnflin.
*
*      ENDIF.
*
*
*      "------Imposto-------------
*      SELECT *
*        FROM j_1bnfstx
*        INTO TABLE t_j_1bnfstx
*        FOR ALL ENTRIES IN t_j_1bnflin
*        WHERE docnum EQ t_j_1bnflin-docnum
*          AND itmnum EQ t_j_1bnflin-itmnum.
*
*      t_j_1bnfstx_aux[] = t_j_1bnfstx[].
*
*      SORT t_j_1bnfstx_aux BY taxtyp.
*
*      DELETE ADJACENT DUPLICATES FROM t_j_1bnfstx_aux COMPARING taxtyp.
*
*      "-----Tipo de Imposto-------
*      IF t_j_1bnfstx_aux IS NOT INITIAL.
*
*        SELECT taxtyp taxgrp
*          FROM j_1baj
*          INTO TABLE t_j_1baj
*          FOR ALL ENTRIES IN t_j_1bnfstx_aux
*           WHERE taxtyp EQ t_j_1bnfstx_aux-taxtyp.
*
*      ENDIF.
*
*      t_j_1bnflin_aux[] = t_j_1bnflin[].
*
*      SORT t_j_1bnflin_aux BY matnr.
*
*      DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING matnr.
*
*      "-------Produto------------
*      SELECT *
*        FROM makt
*        INTO TABLE t_makt
*        FOR ALL ENTRIES IN t_j_1bnflin_aux
*        WHERE matnr EQ t_j_1bnflin_aux-matnr.
*
*      t_j_1bnflin_aux[] = t_j_1bnflin[].
*
*      SORT t_j_1bnflin_aux BY refkey.
*
*      DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING refkey.
*
*      "-----Documento de faturamento-----
*      SELECT vbeln kursk
*        FROM vbrp
*        INTO TABLE t_vbrp
*        FOR ALL ENTRIES IN t_j_1bnflin_aux
*        WHERE vbeln EQ  t_j_1bnflin_aux-refkey(10).
*
*      "------Parceiro da Fatura
*      SELECT *
*        FROM vbpa
*        INTO TABLE t_vbpa
*        FOR ALL ENTRIES IN t_j_1bnflin_aux
*        WHERE vbeln EQ t_j_1bnflin_aux-refkey(10)
*          AND parvw EQ 'Z1'.
*
*      t_vbpa_aux[] = t_vbpa[].
*
*      SORT t_vbpa_aux BY lifnr.
*
*      DELETE ADJACENT DUPLICATES FROM t_vbpa_aux COMPARING lifnr.
*
*      IF t_vbpa_aux IS NOT INITIAL.
*        SELECT *
*          FROM lfa1
*          INTO TABLE t_lfa1
*          FOR ALL ENTRIES IN t_vbpa_aux
*          WHERE lifnr EQ t_vbpa_aux-lifnr.
*      ENDIF.
*
*      "-------Remessa-----------
*
*      SELECT v~vbeln re~ch_referencia l~vbeln re~nr_romaneio l~vgbel
*        FROM vbfa AS v
*        INNER JOIN lips AS l ON l~vbeln = v~vbelv
*        LEFT JOIN zsdt0001 AS re ON re~doc_rem = l~vbeln AND tp_movimento EQ 'S'
*        INTO TABLE t_remessa
*        FOR ALL ENTRIES IN t_j_1bnflin_aux
*        WHERE v~vbeln   EQ t_j_1bnflin_aux-refkey(10)
*          AND v~vbtyp_v EQ 'J'.
*
*
*
**      SELECT V~VBELN V~VBELV
**         FROM VBFA AS V
**         INTO TABLE T_REMESSA2
**         FOR ALL ENTRIES IN T_J_1BNFLIN_AUX
**         WHERE V~VBELN   EQ T_J_1BNFLIN_AUX-REFKEY(10)
**           AND V~VBTYP_V EQ 'C'
**           AND V~VBTYP_N IN ('M','O').
**
*      IF sy-subrc IS INITIAL.
**        LOOP AT T_REMESSA2 INTO WA_REMESSA2.
**          CLEAR WA_REMESSA.
**
**          WA_REMESSA-REFKEY = WA_REMESSA2-REFKEY.
**          WA_REMESSA-VGBEL  = WA_REMESSA2-VGBEL.
**
**          APPEND WA_REMESSA TO T_REMESSA.
**        ENDLOOP.
*
*        "-------Remessa-----------
*        t_remessa_aux[] = t_remessa[].
*
*        SORT t_remessa_aux BY vgbel.
*
*        DELETE ADJACENT DUPLICATES FROM t_remessa_aux COMPARING vgbel.
*
*        "--------Utilização-----
*        SELECT vbeln bezei
*          FROM vbap AS v
*          INNER JOIN tvlvt AS t ON t~abrvw  EQ v~vkaus
*          INTO TABLE t_vbap
*          FOR ALL ENTRIES IN t_remessa_aux
*          WHERE vbeln EQ t_remessa_aux-vgbel.
*
**        delete t_vbak where: vkorg      ne p_vkorg,
**                       j_1bbranch not in p_werks,
**                       auart      not in p_auart,
**                       vbeln      not in p_vbeln,
**                       kunnr      not in p_parid.
*
*        t_remessa_aux[] = t_remessa[].
*
*        DELETE t_remessa_aux WHERE: refkey EQ ''.
*
*        SORT t_remessa_aux BY refkey.
*
*
*        LOOP AT t_remessa_aux INTO wa_remessa.
*
*          CONCATENATE wa_remessa-refkey '          ' INTO wa_remessa_3-refkey .
**          call function 'CONVERSION_EXIT_INVDT_INPUT'
**          exporting
**            input  = WA_REMESSA-refkey
**          importing
**            output = WA_REMESSA_3-refkey .
*
*          wa_remessa_3-vbeln  = wa_remessa-refkey.
*
*          APPEND wa_remessa_3 TO t_remessa_3.
*
*        ENDLOOP.
*
*        DELETE ADJACENT DUPLICATES FROM t_remessa_aux COMPARING refkey.
*
*        SELECT bukrs gjahr awkey belnr
*          FROM bkpf
*          INTO TABLE t_bkpf
*          FOR ALL ENTRIES IN t_remessa_3
*          WHERE awkey = t_remessa_3-refkey .
*
*      ENDIF.
*
*    ENDIF.
*
*
*    IF t_bkpf IS NOT INITIAL.
*      SELECT bukrs belnr gjahr dmbtr dmbe2 shkzg
*        FROM bsid
*        INTO TABLE t_bsid
*        FOR ALL ENTRIES IN t_bkpf
*        WHERE bukrs EQ t_bkpf-bukrs
*        AND   belnr EQ t_bkpf-belnr
*        AND   gjahr EQ t_bkpf-gjahr.
*
*      SELECT bukrs belnr gjahr dmbtr dmbe2 shkzg
*        FROM bsad
*        INTO TABLE t_bsad
*        FOR ALL ENTRIES IN t_bkpf
*        WHERE bukrs EQ t_bkpf-bukrs
*        AND   belnr EQ t_bkpf-belnr
*        AND   gjahr EQ t_bkpf-gjahr.
*    ENDIF.
*
*  ELSE.
*
*    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
*                       'informados' .
*    STOP.
*
*  ENDIF.
*
*ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_organiza_dados .
*
*  DATA: i                   TYPE c LENGTH 1,
*        wa_kna1             TYPE kna1,
*        wa_lfa1             TYPE lfa1,
*        wa_t001w            TYPE t001w,
*        wa_vbpavb           TYPE vbpavb,
*        wa_bkpf_aux         TYPE bkpf,
*        wa_zib_contabil_chv TYPE zib_contabil_chv,
*        wa_zfiwrt0008       TYPE zfiwrt0008,
*        wa_j_1batl1         TYPE j_1batl1,
*        wa_j_1batl1t        TYPE j_1batl1t,
*        wa_j_1batl2         TYPE j_1batl2,
*        wa_j_1batl4a        TYPE j_1batl4a,
*        wa_j_1batl4t        TYPE j_1batl4t,
*        wa_j_1batl5         TYPE j_1batl5,
*
*        quantidade          TYPE j_1bnflin-menge,
*        utilizacao          TYPE tvlvt-bezei,
*        cont                TYPE n LENGTH 4,
*        v_bukrs             TYPE j_1bnfdoc-bukrs,
*        v_len               TYPE i,
*        v_len2              TYPE i,
*        v_belnr             TYPE rseg-belnr,
*        v_gjahr             TYPE rseg-gjahr,
*        v_buzei             TYPE rseg-buzei,
*        p_nordem_c          TYPE c LENGTH 10,
*        v_netwrt            TYPE j_1bnflin-netwrt.
*
*  SORT: t_j_1bnfdoc      BY belnr pstdat bukrs,
*        t_j_1bnflin      BY docnum,
*        t_lfa1           BY lifnr,
*        t_t001           BY bukrs,
*        t_remessa        BY refkey,
*        t_remessa2       BY refkey,
*        t_j_1baj         BY taxtyp,
*        t_parceiro       BY docnum,
*        t_j_1bnfe_active BY docnum,
*        t_makt           BY matnr,
*        t_vbpa           BY vbeln,
*        t_j_1bnfstx      BY docnum itmnum,
*        t_bkpf           BY awkey ,
*        t_remessa_3      BY vbeln .
*
*  DATA: wa_vbrp_aux TYPE vbrp,
*        wa_rseg_aux TYPE rseg.
*
*  DATA: cl_util TYPE REF TO zcl_util.
*
*  LOOP AT t_j_1bnfdoc INTO wa_j_1bnfdoc.
*
*    CLEAR: wa_j_1bnflin, wa_bsad, wa_bsid, t_j_1bnflin2[], wa_j_1batl1,v_netwrt.
*
*    LOOP AT t_j_1bnflin INTO wa_j_1bnflin WHERE docnum = wa_j_1bnfdoc-docnum.
*
*      cont = 0.
*      LOOP AT t_j_1bnflin INTO wa_j_1bnflin_aux WHERE docnum = wa_j_1bnfdoc-docnum.
*        cont = cont + 1.
*      ENDLOOP.
*
*      CLEAR: wa_saida, wa_lfa1, wa_j_1bnfstx, wa_j_1baj, wa_j_1bnfe_active,wa_bkpf, wa_makt, wa_vbap, wa_remessa, wa_remessa2, wa_remessa_3, wa_vbpa, x_data, x_hora.
*
*      CLEAR :  wa_j_1bnfstx, v_netwrt.
*
*      LOOP AT t_j_1bnfstx INTO wa_j_1bnfstx WHERE docnum EQ wa_j_1bnflin-docnum AND itmnum EQ wa_j_1bnflin-itmnum.
*        IF wa_j_1bnfstx-taxtyp EQ 'ICM1' OR
*           wa_j_1bnfstx-taxtyp EQ 'ICM2' OR
*           wa_j_1bnfstx-taxtyp EQ 'ICM3' OR
*           wa_j_1bnfstx-taxtyp EQ 'ICOF' OR
*           wa_j_1bnfstx-taxtyp EQ 'IPIS'.
*          ADD wa_j_1bnfstx-taxval TO v_netwrt.
*        ENDIF.
*
*        READ TABLE t_j_1baj INTO wa_j_1baj WITH KEY taxtyp = wa_j_1bnfstx-taxtyp.
*
*        IF sy-subrc IS INITIAL.
*          IF wa_j_1baj-taxgrp EQ c_pis.
**             PIS
*            wa_saida-pis = wa_j_1bnfstx-taxval.
*          ELSEIF wa_j_1baj-taxgrp EQ c_icms.
**             ICMS
*
*            IF wa_j_1baj-taxtyp EQ 'ICM2' .
*              wa_saida-outros    = wa_j_1bnfstx-base.
*
*            ELSE.
*
*              wa_saida-icms      = wa_j_1bnfstx-taxval.
*              wa_saida-base_icms = wa_j_1bnfstx-base.
*              wa_saida-outros    = wa_j_1bnfstx-othbas.
*              wa_saida-excbas    = wa_j_1bnfstx-excbas.
*            ENDIF.
*
*          ELSEIF wa_j_1baj-taxgrp EQ c_cofins.
**             COFINS
*            wa_saida-cofins = wa_j_1bnfstx-taxval.
*          ELSEIF wa_j_1baj-taxgrp EQ c_iss .
**             ISS
*            wa_saida-iss    = wa_j_1bnfstx-taxval.
*          ELSEIF wa_j_1baj-taxgrp EQ 'INSS' .
**             ISS
*            wa_saida-inss    = wa_j_1bnfstx-taxval.
*          ENDIF.
*
*        ENDIF.
*
*      ENDLOOP.
*
*      IF wa_j_1bnfdoc-nfe EQ 'X'.
*
*        READ TABLE t_j_1bnfe_active INTO wa_j_1bnfe_active WITH KEY docnum = wa_j_1bnfdoc-docnum.
*
*        IF wa_j_1bnfe_active-docsta EQ '  ' AND wa_j_1bnfe_active-scssta EQ ' ' .
*          wa_saida-status = 'Não Enviada'.
*        ELSEIF wa_j_1bnfe_active-docsta EQ '  '.
*          wa_saida-status = 'Aguardando resposta'.
*        ELSEIF wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel EQ 'X'.
*          wa_saida-status = 'Cancelada'.
*        ELSEIF wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel NE 'X'.
*          wa_saida-status = 'autorizada'.
*        ELSEIF wa_j_1bnfe_active-docsta EQ 2.
*          wa_saida-status = 'rejeitada'.
*        ELSEIF wa_j_1bnfe_active-docsta EQ 3.
*          wa_saida-status = 'recusada'.
*        ELSE.
*          wa_saida-status =  wa_j_1bnfe_active-docsta .
*        ENDIF.
*      ENDIF.
*
*      IF wa_j_1bnfdoc-cancel EQ 'X'.
*        wa_saida-nf_status = 'Estornada'.
*      ELSE.
*        wa_saida-nf_status = 'Ativa'.
*      ENDIF.
*
*      "Retira empresa do codigo do parceiro
*      IF wa_j_1bnfdoc-parvw = 'BR'.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = wa_j_1bnfdoc-bukrs
*          IMPORTING
*            output = v_bukrs.
*
*        CONDENSE v_bukrs NO-GAPS.
*        v_len  = STRLEN( v_bukrs ).
*        v_len2 = 10 - v_len.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = wa_j_1bnfdoc-parid
*          IMPORTING
*            output = wa_j_1bnfdoc-parid.
*
*        CONDENSE wa_j_1bnfdoc-parid NO-GAPS.
*        wa_j_1bnfdoc-parid = wa_j_1bnfdoc-parid+v_len(v_len2).
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = wa_j_1bnfdoc-parid
*          IMPORTING
*            output = wa_j_1bnfdoc-parid.
*      ENDIF.
*
*      "Cliente / Fornecedor / Local
*      CALL FUNCTION 'Z_PARCEIRO_INFO'
*        EXPORTING
*          p_parceiro   = wa_j_1bnfdoc-parid
*          p_partype    = wa_j_1bnfdoc-partyp
*        CHANGING
*          wa_info_part = wa_lfa1.
*
*      wa_saida-cod_clifor    = wa_lfa1-lifnr.
*      wa_saida-nome_clifor   = wa_lfa1-name1.
*      wa_saida-stcd3         = wa_lfa1-stcd3.
*      wa_saida-uf_clifor     = wa_lfa1-regio.
*      wa_saida-ort01         = wa_lfa1-ort01.
*
*      IF NOT wa_lfa1-stcd1 IS INITIAL.
*        wa_saida-cpf_prod     = wa_lfa1-stcd1.
*      ELSE.
*        wa_saida-cpf_prod     = wa_lfa1-stcd2.
*      ENDIF.
*
*      "Chave de Acesso
*      CONCATENATE
*        wa_j_1bnfe_active-regio
*        wa_j_1bnfe_active-nfyear
*        wa_j_1bnfe_active-nfmonth
*        wa_j_1bnfe_active-stcd1
*        wa_j_1bnfe_active-model
*        wa_j_1bnfe_active-serie
*        wa_j_1bnfe_active-nfnum9
*        wa_j_1bnfe_active-docnum9
*        wa_j_1bnfe_active-cdv    INTO chave_nfe.
*
*      LOOP AT t_parceiro INTO wa_parceiro WHERE docnum EQ wa_j_1bnfdoc-docnum.
*
*        CLEAR wa_lfa1.
*
*        CASE wa_parceiro-parvw.
*          WHEN 'PC'.
*            CALL FUNCTION 'Z_PARCEIRO_INFO'
*              EXPORTING
*                p_parceiro   = wa_parceiro-parid
*                p_partype    = wa_parceiro-partyp
*              CHANGING
*                wa_info_part = wa_lfa1.
*
*            wa_saida-parc_coleta = wa_lfa1-name1.
*            wa_saida-uf_coleta   = wa_lfa1-regio.
*            wa_saida-cid_coleta  = wa_lfa1-ort01.
*            wa_saida-ort01         = wa_lfa1-ort01.
*          WHEN 'LR'.
*
*            CALL FUNCTION 'Z_PARCEIRO_INFO'
*              EXPORTING
*                p_parceiro   = wa_parceiro-parid
*                p_partype    = wa_parceiro-partyp
*              CHANGING
*                wa_info_part = wa_lfa1.
*
*            wa_saida-local_ent   = wa_lfa1-name1.
*            wa_saida-uf_ent      = wa_lfa1-regio.
*            wa_saida-ort01         = wa_lfa1-ort01.
*        ENDCASE.
*
*      ENDLOOP.
*
*      CLEAR wa_lfa1.
*
*      READ TABLE t_vbpa INTO wa_vbpa WITH KEY vbeln = wa_j_1bnflin-refkey(10).
*
*      IF sy-subrc IS INITIAL.
*        READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa-lifnr.
*        wa_saida-terminal    = wa_lfa1-name1.
*        wa_saida-uf_terminal = wa_lfa1-regio.
*      ENDIF.
*
*      READ TABLE t_remessa INTO wa_remessa WITH KEY refkey = wa_j_1bnflin-refkey(10).
*
*      READ TABLE t_remessa_3 INTO wa_remessa_3 WITH KEY vbeln = wa_remessa-refkey BINARY SEARCH .
*
*
*      CASE  wa_j_1bnflin-reftyp.
*
*        WHEN 'LI'.
*
*          SELECT SINGLE * FROM bkpf INTO wa_bkpf_aux WHERE awkey EQ wa_j_1bnflin-refkey
*                                                       AND blart EQ 'RE'.
*          wa_saida-doc_contabil = wa_bkpf_aux-belnr.
*
*        WHEN 'ZW'.
*
*          SELECT SINGLE * FROM zfiwrt0008 INTO wa_zfiwrt0008 WHERE seq_lcto EQ wa_j_1bnflin-refkey.
*
*          IF ( sy-subrc EQ 0 ).
*            SELECT SINGLE * FROM zib_contabil_chv INTO wa_zib_contabil_chv WHERE obj_key EQ wa_zfiwrt0008-obj_key.
*            wa_saida-doc_contabil = wa_zib_contabil_chv-belnr.
*          ENDIF.
*
*        WHEN OTHERS.
*
*          READ TABLE t_bkpf INTO wa_bkpf WITH KEY awkey = wa_remessa_3-refkey.
*          wa_saida-doc_contabil = wa_bkpf-belnr.
*
*      ENDCASE.
*
**      "Novo Valor
**      READ TABLE t_bsid INTO wa_bsid WITH KEY belnr = wa_bkpf-belnr.
**      IF sy-subrc IS INITIAL.
**        "IF wa_bsid-shkzg = 'H'.
**
**        "wa_saida-netwrt = ( wa_bsid-dmbtr * ( -1 ) ).
**        "wa_saida-vlr_dolar  = ( wa_bsid-dmbe2 * ( -1 ) ).
**
**        "ELSE.
**        wa_saida-netwrt    = wa_bsid-dmbtr .
**        wa_saida-vlr_dolar = wa_bsid-dmbe2 .
**
**        "ENDIF.
**      ELSE.
**        "IF  ( ( wa_saida-netwrt EQ 0 ) OR ( wa_saida-vlr_dolar EQ 0 ) ).
**        READ TABLE t_bsad INTO wa_bsad WITH KEY belnr = wa_bkpf-belnr.
**        IF sy-subrc IS INITIAL.
**          "IF wa_bsad-shkzg = 'H'.
**          "wa_saida-netwrt = ( wa_bsad-dmbtr * ( -1 ) ).
**          "wa_saida-vlr_dolar  = ( wa_bsad-dmbe2 * ( -1 ) ).
**          "ELSE.
**          wa_saida-netwrt    = wa_bsad-dmbtr.
**          wa_saida-vlr_dolar = wa_bsad-dmbe2.
**        ENDIF.
**        "ENDIF.
**      ENDIF.
**      " Fim Novo Valor
*
*                                                            "CH.129254
*      wa_saida-netwrt   = wa_j_1bnflin-netwr .
*
*      READ TABLE t_j_1bnflin2 WITH KEY docnum = wa_j_1bnflin-docnum.
*      IF sy-subrc IS INITIAL.
*        CLEAR: wa_saida-vlr_dolar.
*      ENDIF.
*
*
*      READ TABLE t_vbap INTO wa_vbap WITH KEY vbeln = wa_remessa-vgbel.
*      wa_saida-utilizacao  = wa_vbap-bezei.
*
*      utilizacao = ''.
*
*      READ TABLE t_vbrp INTO wa_vbrp WITH KEY vbeln = wa_j_1bnflin-refkey(10).
*
*      IF sy-subrc IS INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = wa_remessa-vgbel
*          IMPORTING
*            output = wa_saida-ordem.
*
*        "WA_SAIDA-ORDEM       = WA_REMESSA-VGBEL.
*        wa_saida-remessa     = wa_remessa-vbeln.
*        wa_saida-romaneio    = wa_remessa-nr_romaneio.
*        wa_saida-ref_ro      = wa_remessa-ch_referencia.
*        wa_saida-doc_fatura  = wa_remessa-refkey.
*
*        CLEAR wa_lfa1.
*
*      ENDIF.
*
*      READ TABLE t_makt INTO wa_makt WITH KEY  matnr  = wa_j_1bnflin-matnr.
*      "WA_SAIDA-PRODUTO             = WA_MAKT-MAKTX.
*      wa_saida-produto             = wa_j_1bnflin-matnr.
*
*      IF wa_j_1bnfdoc-nfe EQ 'X' .
*        wa_saida-nfenum   = wa_j_1bnfdoc-nfenum.
*      ELSE .
*        wa_saida-nfenum   = wa_j_1bnfdoc-nfnum.
*      ENDIF.
*
*      quantidade = wa_j_1bnflin-menge.
*
*      IF quantidade EQ 0.
*        quantidade = 1.
*      ENDIF.
*
*      wa_saida-nfe      = wa_j_1bnfdoc-nfe   .
*      wa_saida-bukrs    = wa_j_1bnfdoc-bukrs .
*      wa_saida-branch   = wa_j_1bnfdoc-branch.
*      wa_saida-docnum   = wa_j_1bnfdoc-docnum.
*      wa_saida-pstdat   = wa_j_1bnfdoc-pstdat.
*      wa_saida-docdat   = wa_j_1bnfdoc-docdat.
*      wa_saida-series   = wa_j_1bnfdoc-series.
*      wa_saida-cfop     = wa_j_1bnflin-cfop  .
*      wa_saida-menge    = wa_j_1bnflin-menge .
*      wa_saida-meins    = wa_j_1bnflin-meins .
*      wa_saida-charg    = wa_j_1bnflin-charg.
*      wa_saida-ncm      = wa_j_1bnflin-nbm.
*
*
*      wa_saida-nftype  = wa_j_1bnfdoc-nftype.
*      wa_saida-model   = wa_j_1bnfdoc-model.
*      wa_saida-refkey  = wa_j_1bnflin-refkey.
*
*      wa_saida-maktx  = wa_j_1bnflin-maktx.
*
*      SELECT SINGLE * FROM j_1batl1 INTO wa_j_1batl1 WHERE taxlaw EQ wa_j_1bnflin-taxlw1.
*
*      IF ( sy-subrc EQ 0 ).
*        SELECT SINGLE *
*          FROM j_1batl1t
*          INTO wa_j_1batl1t
*         WHERE taxlaw = wa_j_1batl1-taxlaw
*           AND langu = sy-langu.
*
*        IF sy-subrc = 0.
*          CONCATENATE wa_j_1batl1t-line1
*                      wa_j_1batl1t-line2
*                      wa_j_1batl1t-line3
*                      wa_j_1batl1t-line4
*          INTO wa_saida-lei_icms
*          SEPARATED BY space.
*        ENDIF.
*
*        CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
*          EXPORTING
*            input  = wa_j_1batl1-taxsit
*          IMPORTING
*            output = wa_saida-taxlw1.
*        CLEAR: wa_j_1batl1, wa_j_1batl1t.
*      ENDIF.
*
*      SELECT SINGLE * FROM j_1batl2 INTO wa_j_1batl2 WHERE taxlaw EQ wa_j_1bnflin-taxlw2.
*
*      IF ( sy-subrc EQ 0 ).
*        wa_saida-taxlw2 = wa_j_1batl2-taxsit.
*        CLEAR: wa_j_1batl1.
*      ENDIF.
*
*      SELECT SINGLE * FROM j_1batl4a INTO wa_j_1batl4a WHERE taxlaw EQ wa_j_1bnflin-taxlw4.
*
*      IF ( sy-subrc EQ 0 ).
*        wa_saida-taxlw4 = wa_j_1batl4a-taxsit.
*        SELECT SINGLE *
*          FROM j_1batl4t
*          INTO wa_j_1batl4t
*         WHERE taxlaw = wa_j_1batl4a-taxlaw
*           AND langu = sy-langu.
*
*        IF sy-subrc = 0.
*          CONCATENATE wa_j_1batl4t-line1
*                      wa_j_1batl4t-line2
*                      wa_j_1batl4t-line3
*                      wa_j_1batl4t-line4
*          INTO wa_saida-lei_cofins
*          SEPARATED BY space.
*        ENDIF.
*
*        CLEAR: wa_j_1batl1, wa_j_1batl4t.
*      ENDIF.
*
*      SELECT SINGLE * FROM j_1batl5 INTO wa_j_1batl5 WHERE taxlaw EQ wa_j_1bnflin-taxlw5.
*
*      IF ( sy-subrc EQ 0 ).
*        wa_saida-taxlw5 = wa_j_1batl5-taxsit.
*      ENDIF.
*
*
*
*      IF ( wa_j_1bnflin-reftyp EQ 'BI' ).
*
*        CLEAR: wa_vbrp_aux.
*        SELECT SINGLE * FROM vbrp INTO wa_vbrp_aux WHERE vbeln EQ wa_j_1bnflin-refkey.
*
*        wa_saida-iva = wa_vbrp_aux-j_1btxsdc.
*
*      ELSEIF ( wa_j_1bnflin-reftyp EQ 'LI').
*
*        CLEAR: wa_rseg_aux.
*        v_belnr = wa_j_1bnflin-refkey+0(10).
*        v_gjahr = wa_j_1bnflin-refkey+10(4).
*        SELECT SINGLE * FROM rseg INTO wa_rseg_aux WHERE belnr EQ v_belnr
*                                                   AND   gjahr EQ v_gjahr
*                                                   AND   buzei EQ  wa_j_1bnflin-refitm.
*
*        wa_saida-iva = wa_rseg_aux-mwskz.
*
*      ENDIF.
*
*
*      "Valor Total da NF
*
**      call function 'J_1B_NF_VALUE_DETERMINATION'
**        exporting
**          nf_header   = wa_j_1bnfdoc
**        importing
**          ext_header  = wa_j_cabe
**        tables
**          nf_item     = t_j_1bnflin
**          nf_item_tax = t_j_1bnfstx.
*
*      " Se não achar BSID E BSAD continuar com valores
*
*      " Se for nota de serviço (exceção a regra)
*      IF wa_j_1bnfdoc-nfesrv EQ 'X' .
*        wa_saida-netwrt   = wa_j_1bnflin-nfnet.
*      ENDIF.
*
*      IF  wa_saida-netwrt EQ 0 .
**        IF WA_J_1BNFLIN-NFNET > 0 .
**          WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NFNET.
**        ELSE.
**         comentado em 04.12.12 solicitação Marcos Santos
**          IF P_DIRECT = 1 AND WA_J_1BNFLIN-REFTYP NE 'ZW'.
**            WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NETWR  + WA_SAIDA-ICMS + WA_SAIDA-PIS + WA_SAIDA-COFINS + WA_SAIDA-ISS.
**          ELSE.
**            WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NETWR .
**          ENDIF.
*        wa_saida-netwrt   = wa_j_1bnflin-netwr .
**        ENDIF.
*      ENDIF.
*      " Valor Item
*      IF cont > 1.
*        wa_saida-netwrt = wa_j_1bnflin-netwr.
*      ENDIF.
*
*
*      "ALRS
*      IF v_netwrt GT 0 AND p_direct = 1.
*        ADD v_netwrt TO wa_saida-netwrt.
*      ENDIF.
*
*      wa_saida-vlr_unit = wa_saida-netwrt / quantidade.
*      wa_saida-user     = sy-uname.
*
*      IF  wa_saida-vlr_dolar EQ 0 . " Se não achar BSID E BSAD continuar com valores
*        IF wa_vbrp-kursk > 1 .
*          wa_saida-kursk      = wa_vbrp-kursk."TAXA
*          wa_saida-vlr_dolar  = wa_saida-netwrt / wa_vbrp-kursk."VLR DOLAR
*          wa_saida-unit_dolar = ( wa_saida-netwrt / quantidade ) /  wa_vbrp-kursk.
*        ELSE.
*          wa_saida-kursk      = 0.
*          wa_saida-vlr_dolar  = 0.
*          wa_saida-unit_dolar = 0.
*        ENDIF.
*      ENDIF.
*
*      IF wa_saida-nf_status EQ 'Cancelada'.
*        wa_saida-menge      = 0.
*        wa_saida-netwrt     = 0.
*        wa_saida-vlr_unit   = 0.
*        wa_saida-pis        = 0.
*        wa_saida-icms       = 0.
*        wa_saida-base_icms  = 0.
*        wa_saida-outros     = 0.
*        wa_saida-excbas     = 0.
*        wa_saida-cofins     = 0.
*        wa_saida-kursk      = 0.
*        wa_saida-vlr_dolar  = 0.
*        wa_saida-unit_dolar = 0.
*      ENDIF.
*
*      x_data = sy-datum.
*      x_hora = sy-uzeit.
*
*      CONCATENATE x_data+6(2) '/'
*                  x_data+4(2) '/'
*                  x_data(4)   ' -  '
*                  x_hora(2)   ':'
*                  x_hora+2(2) ':'
*                  x_hora+4(2) INTO wa_saida-data.
*      i = '1'.
*
*      IF ( p_ativas EQ 'X' AND wa_j_1bnfdoc-cancel NE 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
*         ( p_nativa EQ 'X' AND wa_j_1bnfdoc-cancel EQ 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
*         ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfdoc-nfe EQ 'X' AND wa_j_1bnfe_active-cancel NE 'X'  ) OR
*         ( p_rejeit EQ 'X' AND wa_j_1bnfe_active-docsta EQ 2 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
*         ( p_recus  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 3 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
*         ( p_cancel EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel EQ 'X' ) OR
*     "    ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel EQ 'X' AND  wa_j_1bnfdoc-nfe EQ 'X'  ) OR
*         ( p_agres  EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta NE ' ' AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
*         ( p_nenv   EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta EQ   ' '  AND wa_j_1bnfdoc-nfe EQ 'X'  ).
*        i = '0'.
*      ENDIF.
*
*      IF ( p_ativas EQ 'X' AND p_nativa NE 'X' AND  wa_j_1bnfdoc-cancel EQ 'X' ) OR
*         ( p_nativa EQ 'X' AND p_ativas NE 'X' AND  wa_j_1bnfdoc-cancel NE 'X' ) .
*        i = '1'.
*      ENDIF.
*
*
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = wa_saida-ordem
*        IMPORTING
*          output = p_nordem_c.
*
*
*      IF  ( p_nordem  IS INITIAL AND i = '0' ).
*        i = '0'.
*      ELSEIF ( (  p_nordem IS NOT INITIAL AND p_nordem_c IN p_nordem ) AND i = '0' ).
*        i = '0'.
*      ELSEIF ( ( p_nordem IS NOT INITIAL  AND p_nordem_c NOT IN p_nordem ) AND i = '0') .
*        i = '1'.
*      ENDIF.
*
*      IF i EQ '1' .
*        CONTINUE.
*      ELSE.
*        APPEND wa_saida TO t_saida.
*      ENDIF.
*
*      APPEND wa_j_1bnflin TO t_j_1bnflin2.
*
*    ENDLOOP.
*
*    CLEAR: wa_j_1bnfdoc, wa_saida.
*
*  ENDLOOP.
*
*ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVAR_DADOS.

  DATA: V_ANTERIOR LIKE ZDADOS_SAIDA-DOCNUM.

  IF P_GRAVR EQ 'X'.

    LOOP AT T_SAIDA INTO WA_SAIDA.

      IF WA_SAIDA-DOCNUM NE V_ANTERIOR.
        ZDADOS_SAIDA-SEQUENCIA = 1.
        V_ANTERIOR = WA_SAIDA-DOCNUM.
      ELSE.
        ZDADOS_SAIDA-SEQUENCIA = ZDADOS_SAIDA-SEQUENCIA + 1.
      ENDIF.


      ZDADOS_SAIDA-RAZREC      = WA_SAIDA-RAZREC.
      ZDADOS_SAIDA-RAZCLI      = WA_SAIDA-RAZCLI.
      ZDADOS_SAIDA-WAERS       = WA_SAIDA-WAERS.
      ZDADOS_SAIDA-TXT20       = WA_SAIDA-TXT20.



      ZDADOS_SAIDA-BUKRS        = WA_SAIDA-BUKRS.
      ZDADOS_SAIDA-BRANCH       = WA_SAIDA-BRANCH.
      ZDADOS_SAIDA-CFOP         = WA_SAIDA-CFOP.
      ZDADOS_SAIDA-NFENUM       = WA_SAIDA-NFENUM.
      ZDADOS_SAIDA-SERIES       = WA_SAIDA-SERIES.
      ZDADOS_SAIDA-MEINS        = WA_SAIDA-MEINS.
      ZDADOS_SAIDA-PSTDAT       = WA_SAIDA-PSTDAT.
      ZDADOS_SAIDA-DOCDAT       = WA_SAIDA-DOCDAT.
      ZDADOS_SAIDA-MENGE        = WA_SAIDA-MENGE.
      ZDADOS_SAIDA-NETWRT       = WA_SAIDA-NETWRT.
      ZDADOS_SAIDA-BASE_ICMS    = WA_SAIDA-BASE_ICMS.
      ZDADOS_SAIDA-OUTROS       = WA_SAIDA-OUTROS.
      ZDADOS_SAIDA-ICMS         = WA_SAIDA-ICMS.
      ZDADOS_SAIDA-PIS          = WA_SAIDA-PIS.
      ZDADOS_SAIDA-COFINS       = WA_SAIDA-COFINS.
      ZDADOS_SAIDA-ISS          = WA_SAIDA-ISS.
      ZDADOS_SAIDA-INSS         = WA_SAIDA-INSS.
      ZDADOS_SAIDA-DOCNUM       = WA_SAIDA-DOCNUM.
      ZDADOS_SAIDA-STCD3        = WA_SAIDA-STCD3.
      ZDADOS_SAIDA-EXCBAS       = WA_SAIDA-EXCBAS.
      ZDADOS_SAIDA-PRODUTO      = WA_SAIDA-PRODUTO.
      ZDADOS_SAIDA-ORDEM        = WA_SAIDA-ORDEM.
      ZDADOS_SAIDA-REMESSA      = WA_SAIDA-REMESSA.
      ZDADOS_SAIDA-ROMANEIO     = WA_SAIDA-ROMANEIO.
      ZDADOS_SAIDA-REF_RO       = WA_SAIDA-REF_RO.
      ZDADOS_SAIDA-UTILIZACAO   = WA_SAIDA-UTILIZACAO.
      ZDADOS_SAIDA-DOC_FATURA   = WA_SAIDA-DOC_FATURA.
      ZDADOS_SAIDA-KURSK        = WA_SAIDA-KURSK.
      ZDADOS_SAIDA-VLR_DOLAR    = WA_SAIDA-VLR_DOLAR.
      ZDADOS_SAIDA-CPF_PROD     = WA_SAIDA-CPF_PROD.
      ZDADOS_SAIDA-UNIT_DOLAR   = WA_SAIDA-UNIT_DOLAR.
      ZDADOS_SAIDA-STATUS       = WA_SAIDA-STATUS.
      ZDADOS_SAIDA-NF_STATUS    = WA_SAIDA-NF_STATUS.
      ZDADOS_SAIDA-VLR_UNIT     = WA_SAIDA-VLR_UNIT.
      ZDADOS_SAIDA-COD_CLIFOR   = WA_SAIDA-COD_CLIFOR.
      ZDADOS_SAIDA-NOME_CLIFOR  = WA_SAIDA-NOME_CLIFOR.
      ZDADOS_SAIDA-UF_CLIFOR    = WA_SAIDA-UF_CLIFOR.
      ZDADOS_SAIDA-ORT01        = WA_SAIDA-ORT01.
      ZDADOS_SAIDA-CRT_BUPLA    = WA_SAIDA-CRT_BUPLA. "wa_saida-crtn. " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC
      ZDADOS_SAIDA-STKZN        = WA_SAIDA-STKZN.
      ZDADOS_SAIDA-PARC_COLETA  = WA_SAIDA-PARC_COLETA.
      ZDADOS_SAIDA-CID_COLETA   = WA_SAIDA-CID_COLETA.
      ZDADOS_SAIDA-UF_COLETA    = WA_SAIDA-UF_COLETA.
      ZDADOS_SAIDA-LOCAL_ENT    = WA_SAIDA-LOCAL_ENT.
      ZDADOS_SAIDA-UF_ENT       = WA_SAIDA-UF_ENT.
      ZDADOS_SAIDA-TERMINAL     = WA_SAIDA-TERMINAL.  "Terminal
      ZDADOS_SAIDA-UF_TERMINAL  = WA_SAIDA-UF_TERMINAL.
      ZDADOS_SAIDA-NFE          = WA_SAIDA-NFE.
      ZDADOS_SAIDA-DATA         = WA_SAIDA-DATA.
      ZDADOS_SAIDA-USER1        = WA_SAIDA-USER1.
      ZDADOS_SAIDA-DOC_CONTABIL = WA_SAIDA-DOC_CONTABIL.
      ZDADOS_SAIDA-NFTYPE       = WA_SAIDA-NFTYPE.
      ZDADOS_SAIDA-MODEL        = WA_SAIDA-MODEL.
      ZDADOS_SAIDA-REFKEY       = WA_SAIDA-REFKEY.
      ZDADOS_SAIDA-MAKTX        = WA_SAIDA-MAKTX.
      ZDADOS_SAIDA-TAXLW1       = WA_SAIDA-TAXLW1.
      ZDADOS_SAIDA-TAXLW2       = WA_SAIDA-TAXLW2.
      ZDADOS_SAIDA-TAXLW4       = WA_SAIDA-TAXLW4.
      ZDADOS_SAIDA-TAXLW5       = WA_SAIDA-TAXLW5.
      ZDADOS_SAIDA-IVA          = WA_SAIDA-IVA.
      ZDADOS_SAIDA-CHARG        = WA_SAIDA-CHARG.
      ZDADOS_SAIDA-NCM          = WA_SAIDA-NCM.
      ZDADOS_SAIDA-CHAVE_NFE    = WA_SAIDA-CHAVE_NFE.
      ZDADOS_SAIDA-LEI_ICMS     = WA_SAIDA-LEI_ICMS.
      ZDADOS_SAIDA-LEI_COFINS   = WA_SAIDA-LEI_COFINS.
      ZDADOS_SAIDA-SHTYP        = WA_SAIDA-SHTYP.
      ZDADOS_SAIDA-ANLN1        = WA_SAIDA-ANLN1.
      ZDADOS_SAIDA-INSTRUCAO    = WA_SAIDA-INSTRUCAO.
      ZDADOS_SAIDA-DIRECT       = WA_SAIDA-DIRECT.
      INSERT ZDADOS_SAIDA.
    ENDLOOP.

  ENDIF.
ENDFORM.                    "F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  DATA: WL_LAYOUT     TYPE SLIS_LAYOUT_ALV.
  DATA: WL_DISVARIANT TYPE DISVARIANT.

* Fabio Purcino (SolvePlan) em 05.06.2014
* não exibir nada na tela caso esteja sendo executado em batch input
  IF SY-BINPT NE 'X'.

    IF T_SAIDA[] IS INITIAL.
      MESSAGE I000(Z01) WITH 'Não foram encontrados dados para os parametros'
                             'informados' .
      STOP.
    ENDIF.
    PERFORM F_DEFINIR_EVENTOS.
    PERFORM F_ALV_SORT.
    PERFORM F_MONTAR_LAYOUT.

    WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*    wl_layout-no_totalline = 'X'.
    WL_DISVARIANT-VARIANT = P_LAYOUT.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM       = V_REPORT
        I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
        I_CALLBACK_USER_COMMAND  = 'F_USER_COMMAND'
        IT_FIELDCAT              = ESTRUTURA[]
        IT_SORT                  = T_SORT[]
        IS_VARIANT               = WL_DISVARIANT
        IS_LAYOUT                = WL_LAYOUT  "  p_layout "
        I_SAVE                   = 'A'
        IT_EVENTS                = EVENTS
        IS_PRINT                 = T_PRINT
      TABLES
        T_OUTTAB                 = T_SAIDA.

  ENDIF.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT.

* CS2023000865 Filtros Leiaute ZCONF - Ini - RJF - 2024.01.19
  IF P_LAYOUT IS INITIAL.

    PERFORM F_MONTAR_ESTRUTURA USING:
    1  ''   ''      'T_SAIDA' 'BRANCH'              'Filial'              04,
    2  ''   ''      'T_SAIDA' 'CFOP'                'CFOP'                10,
    2  ''   ''      'T_SAIDA' 'CFOTXT'              'Desc.CFOP'           50,
    2  ''   ''      'T_SAIDA' 'DIRECT'              'E/S'                 3,
    2  ''   ''      'T_SAIDA' 'BASE'                'Base PIS/COFINS'     17,
    3  ''   ''      'T_SAIDA' 'NFENUM'              'Nr. Nota'            09,
    4  ''   ''      'T_SAIDA' 'DOCNUM'              'Nº documento'        10,
    4  ''   ''      'T_SAIDA' 'DOCREF'              'Nº Doc. Original'    10,
    5  ''   ''      'T_SAIDA' 'SERIES'              'Série'               03,
    6  ''   ''      'T_SAIDA' 'STCD3'               'Insc. Estadual'      18,
    7  ''   ''      'T_SAIDA' 'COD_CLIFOR'       'Cod. Fornecedor'          35,
    7  ''   ''      'T_SAIDA' 'NOME_CLIFOR'      'Fornecedor'               35,
    7  ''   ''      'T_SAIDA' 'ORT01'            'Municipio do Fornecedor'  35,
    7  ''   ''      'T_SAIDA' 'CRT_BUPLA'        'Cod. Reg. Tribut. '       35," (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC
    10  ''   ''      'T_SAIDA' 'STKZN'              'P.F.'              04,
    8  ''   ''      'T_SAIDA' 'CPF_PROD'           'CPF/CNPJ'           16,
    9  ''   ''      'T_SAIDA' 'UF_CLIFOR'          'UF Cli/For'         03,
    10 ''   ''      'T_SAIDA' 'NF_STATUS'          'Status SAP'         10,
    11 ''   ''      'T_SAIDA' 'STATUS'             'Status NFE'         20,
    12 ''   ''      'T_SAIDA' 'MEINS'              'Un'                 03,
    13 ''   ''      'T_SAIDA' 'PRODUTO'            'Produto'            40,
    13 ''   ''      'T_SAIDA' 'MTART'             'Tipo Material'       08, "US #167066 - MMSILVA - 17.02.2025
    13 ''   ''      'T_SAIDA' 'MTBEZ'             'Descrição Tipo Mat.' 20, "US #167066 - MMSILVA - 17.02.2025
    14 ''   ''      'T_SAIDA' 'MATKL'              'Grupo Mercadorias'  17,
    15 ''   ''      'T_SAIDA' 'WGBEZ60'            'Desc. Grp. Merc.'   40,
    16 ''   ''      'T_SAIDA' 'PSTDAT'             'Dt. Lançamento'     08,
    17 ''   ''      'T_SAIDA' 'DOCDAT'             'Dt. Documento'      08,
    18 ''   ''      'T_SAIDA' 'MENGE'              'Quantidade'         17,
    18 ''   ''      'T_SAIDA' 'NTGEW'              'Peso.Liq.Tot.NF'    17,
    19 ''   ''      'T_SAIDA' 'VLR_UNIT'           'Vlr. Unit'          16,
    20 ''   ''      'T_SAIDA' 'UNIT_DOLAR'         'Val. Unit Dolar'    16,
    21 ''   ''      'T_SAIDA' 'NETWRT'             'Valor NF'           21,
    22 ''   ''      'T_SAIDA' 'VLR_DOLAR'         'Valor Dolar'         21,
    23 ''   ''      'T_SAIDA' 'KURSK'              'Taxa Dolar'         12,
    24 ''   ''      'T_SAIDA' 'BASE_ICMS'          'Base Icms'          21,
    25 ''   ''      'T_SAIDA' 'ICMS'               'Vlr. ICMS'          21,
    26 ''   ''      'T_SAIDA' 'ISS'                'Vlr. ISS'           21,
    27 ''   ''      'T_SAIDA' 'INSS'               'INSS s/Fat.'        21,
    28 ''   ''      'T_SAIDA' 'INCO1'              'Incoterms parte 1.' 10,
    29 ''   ''      'T_SAIDA' 'INCO2'              'Incoterms parte 2.' 28,
    30 ''   ''      'T_SAIDA' 'OUTROS'             'Outros'             21,
    31 ''   ''      'T_SAIDA' 'EXCBAS'             'Isentos'            21,
    32 ''   ''      'T_SAIDA' 'PIS'                'Vlr. PIS'           21,
    33 ''   ''      'T_SAIDA' 'COFINS'             'Vlr. COFINS'        21,
    34 ''   ''      'T_SAIDA' 'ORDEM'              'Ordem de Venda'     10,
    35 ''   ''      'T_SAIDA' 'INSTRUCAO'          'Instrução'          10,
    36 ''   ''      'T_SAIDA' 'UTILIZACAO'         'Utilização'         20,
    37 ''   ''      'T_SAIDA' 'REMESSA'            'Doc. Rem.'          10,
    38 ''   ''      'T_SAIDA' 'ROMANEIO'           'Romaneio'           09,
    39 ''   ''      'T_SAIDA' 'REF_RO'             'Ref. Romaneio'      20,
    40 ''   ''      'T_SAIDA' 'DOC_FATURA'         'Doc. Fatura'        10,
    41 ''   ''      'T_SAIDA' 'DOC_CONTABIL'       'Doc. Contabil'      10,
    42 ''   ''      'T_SAIDA' 'ORDEM_DOC'          'Ordem Interna'      12,
    43 ''   ''      'T_SAIDA' 'DATA_REGISTRO'      'Data Registro'      10,
    44 ''   ''      'T_SAIDA' 'HORA_REGISTRO'      'Hora Registro'      10,
    45 ''   ''      'T_SAIDA' 'PARC_COLETA'        'Ponto de coleta'    35,
    46 ''   ''      'T_SAIDA' 'CID_COLETA'         'Cidade PC'          35,
    47 ''   ''      'T_SAIDA' 'UF_COLETA'          'UF Ponto Coleta'    03,
    48 ''   ''      'T_SAIDA' 'LOCAL_ENT'          'Local de entrega'   35,
    49 ''   ''      'T_SAIDA' 'UF_ENT'             'UF Local entrada'   03,
    40 ''   ''      'T_SAIDA' 'TERMINAL'           'Terminal'           35, " TERMINAL
    51 ''   ''      'T_SAIDA' 'UF_TERMINAL'        'UF Terminal'        03,
    52 ''   ''      'T_SAIDA' 'NFTYPE'             'Cat. Nota'          06,
    53 ''   ''      'T_SAIDA' 'MODEL'              'Modelo NF'          07,
    54 ''   ''      'T_SAIDA' 'REFKEY'             'Doc.Ref'            10,
    55 ''   ''      'T_SAIDA' 'MAKTX'              'Descr. Material'    35,
    56 ''   ''      'T_SAIDA' 'TAXLW1'             'CST ICMS'           3,
    57 ''   ''      'T_SAIDA' 'COD_ICMS'           'Cód. ICMS'          3, "CS2022000515 - FF  28.11.2022
    57 ''   ''      'T_SAIDA' 'LEI_ICMS'           'Lei ICMS'           20,"(Faneli)
    57 ''   ''      'T_SAIDA' 'TAXLW2'             'CST IPI'            3,
    58 ''   ''      'T_SAIDA' 'TAXLW4'             'CST COFINS'         3,
    59 ''   ''      'T_SAIDA' 'TAXLW4_XML'         'CST COFINS XML'     3,
    59 ''   ''      'T_SAIDA' 'LEI_COFINS'         'Lei COFINS'         20,"(Faneli)
    50 ''   ''      'T_SAIDA' 'TAXLW5'             'CST PIS'            3,
    61 ''   ''      'T_SAIDA' 'TAXLW5_XML'         'CST PIS XML'        3,
    62 ''   ''      'T_SAIDA' 'IVA'                'IVA'                3,
    63 ''   ''      'T_SAIDA' 'CHARG'              'LOTE'               10,
    64 ''   ''      'T_SAIDA' 'NCM'                'NCM'                10,
    65 ''   ''      'T_SAIDA' 'CHAVE_NFE'          'Chave de acesso doc fiscal'         44, " Rubenilson - 16.01.25 - US144011
*    66 ''   ''      'T_SAIDA' 'SHTYP'              'Tipo Transporte'    44, RJF
    66 ''   ''      'T_SAIDA' 'BEZEI'              'Tipo Transporte'    44,
    67 ''   ''      'T_SAIDA' 'ANLN1'              'Imobilizado'        13,
    68 ''   ''      'T_SAIDA' 'ANLN2'              'Subnº imobilizado'  04, "CS2022000515 - FF  28.11.2022
    69 ''   ''      'T_SAIDA' 'EBELN'              'Pedido'             10,
    70 ''   ''      'T_SAIDA' 'EBELP2'             'Item do Pedido de compras' 05, "CS2022000515 - FF  28.11.2022
    71 ''   ''      'T_SAIDA' 'KNTTP'              'CtgClsCnt'          09,
    72 ''   ''      'T_SAIDA' 'AUART'              'Tp. Ordem'          09,
    73 ''   ''      'T_SAIDA' 'AUTYP'              'Ctg. Ordem'         10,
    74 ''   ''      'T_SAIDA' 'DDTEXT'             'Desc. Ctg.'         60,
    75 ''   ''      'T_SAIDA' 'KOSTL'              'Centro Custo'       10,
    76 ''   ''      'T_SAIDA' 'DESC_CENTRO_CUST'   'Desc. Centro Custo' 50,
    77 ''   ''      'T_SAIDA' 'SAKTO'              'Conta ctb.'         10,
    78 ''   ''      'T_SAIDA' 'DESC_CONTA_CTB'     'Desc. Conta ctb.'   40,
    79 ''   ''      'T_SAIDA' 'AUFNR'              'Ordem'              12, "CS2022000515 - FF  28.11.2022
    80 ''   ''      'T_SAIDA' 'VORNR'              'Nº operação'        04,
    81 ''   ''      'T_SAIDA' 'LTXA1'              'Descrição Operação' 40,
    82 ''   ''      'T_SAIDA' 'REGIO'              'UF do ct.ped.'      02,
    83 ''   ''      'T_SAIDA' 'RECAP'              'RECAP'              03,
    85 ''   ''      'T_SAIDA' 'RAZCLI'             'Ct.Rz. Cliente'     10, "Informação da conta razão do cliente
    86 ''   ''      'T_SAIDA' 'RAZREC'             'Ct.Rz. Receita'     10, "Informação da conta razão receita
    87 ''   ''      'T_SAIDA' 'WAERS'              'Moeda Doc'          10, "Moeda do documento
    88 ''   ''      'T_SAIDA' 'TXT20'              'Ct.Rz. Descr.'      10, "Descrição conta razão
    89 ''   ''      'T_SAIDA' 'PLACA_CAV'          'Placa do Veículo'   30, "Placa do Veículo
    90 ''   ''      'T_SAIDA' 'ITMNUM'             'Sequência item doc. fiscal'   30, "Sequência item doc. fiscal
    91 ''   ''      'T_SAIDA' 'TTYPETXT'           'Tipo de imposto'   30, "Tipo de Imposto
    92 ''   ''      'T_SAIDA' 'CHAVE_REFER'        'Chave Referenciada'   44,    " Rubenilson - 16.01.25 - US144011
    93 ''   ''      'T_SAIDA' 'MAT_CHAVE_REFER'    'Material Chave refer.'   40. " Rubenilson - 16.01.25 - US144011

  ELSE.
* CS2023000865 Filtros Leiaute ZCONF - Ini - RJF - 2024.01.19

    PERFORM F_MONTAR_ESTRUTURA USING:

    "TABELA "CAMPO    "TAB INTERNA  "VARIAVEL DA WA "CAPTION

    1  ''   ''      'T_SAIDA' 'BRANCH'              'Filial'              04,
    2  ''   ''      'T_SAIDA' 'CFOP'                'CFOP'                10,
    3  ''   ''      'T_SAIDA' 'NFENUM'              'Nr. Nota'            09,
    4  ''   ''      'T_SAIDA' 'DOCNUM'              'Nº documento'        10,
    5  ''   ''      'T_SAIDA' 'SERIES'              'Série'               03,
    6  ''   ''      'T_SAIDA' 'STCD3'               'Insc. Estadual'      18.

    IF LINES( P_DIRECT ) > 1.
      PERFORM F_MONTAR_ESTRUTURA USING:
         7  ''   ''      'T_SAIDA' 'COD_CLIFOR'       'Cod. Fornecedor/Cliente'          35,
         7  ''   ''      'T_SAIDA' 'NOME_CLIFOR'      'Fornecedor/Cliente'               35,
         7  ''   ''      'T_SAIDA' 'ORT01'            'Municipio do Fornecedor/Cliente'  35,
         7  ''   ''      'T_SAIDA' 'CRT_BUPLA'        'Cod. Reg. Tribut. '               35.

    ELSEIF P_DIRECT-LOW EQ 1.
      PERFORM F_MONTAR_ESTRUTURA USING:
      7  ''   ''      'T_SAIDA' 'COD_CLIFOR'       'Cod. Fornecedor'          35,
      7  ''   ''      'T_SAIDA' 'NOME_CLIFOR'      'Fornecedor'               35,
      7  ''   ''      'T_SAIDA' 'ORT01'            'Municipio do Fornecedor'  35,
      7  ''   ''      'T_SAIDA' 'CRT_BUPLA'        'Cod. Reg. Tribut. '       35." (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC

    ELSEIF P_DIRECT-LOW EQ 2.
      PERFORM F_MONTAR_ESTRUTURA USING:
      7  ''   ''      'T_SAIDA' 'COD_CLIFOR'       'Cod. Cliente'             35,
      7  ''   ''      'T_SAIDA' 'NOME_CLIFOR'      'Cliente'                  35,
      7  ''   ''      'T_SAIDA' 'ORT01'            'Municipio do Cliente'     35.

    ENDIF.

*    IF p_direct EQ 1.
*      PERFORM f_montar_estrutura USING:
*      7  ''   ''      'T_SAIDA' 'COD_CLIFOR'       'Cod. Fornecedor'          35,
*      7  ''   ''      'T_SAIDA' 'NOME_CLIFOR'      'Fornecedor'               35,
*      7  ''   ''      'T_SAIDA' 'ORT01'            'Municipio do Fornecedor'  35,
*      7  ''   ''      'T_SAIDA' 'CRT_BUPLA'        'Cod. Reg. Tribut. '       35." (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC
*    ENDIF.
*
*    IF p_direct EQ 2.
*      PERFORM f_montar_estrutura USING:
*      7  ''   ''      'T_SAIDA' 'COD_CLIFOR'       'Cod. Cliente'             35,
*      7  ''   ''      'T_SAIDA' 'NOME_CLIFOR'      'Cliente'                  35,
*      7  ''   ''      'T_SAIDA' 'ORT01'            'Municipio do Cliente'     35.
*    ENDIF.

    PERFORM F_MONTAR_ESTRUTURA USING:

    10  ''   ''      'T_SAIDA' 'STKZN'              'P.F.'              04,
    8  ''   ''      'T_SAIDA' 'CPF_PROD'           'CPF/CNPJ'           16,
    9  ''   ''      'T_SAIDA' 'UF_CLIFOR'          'UF Cli/For'         03,
    10 ''   ''      'T_SAIDA' 'NF_STATUS'          'Status SAP'         10,
    11 ''   ''      'T_SAIDA' 'STATUS'             'Status NFE'         20,
    12 ''   ''      'T_SAIDA' 'MEINS'              'Un'                 03,
    13 ''   ''      'T_SAIDA' 'PRODUTO'            'Produto'            40,
    14 ''   ''      'T_SAIDA' 'MATKL'              'Grupo Mercadorias'  17,
    15 ''   ''      'T_SAIDA' 'WGBEZ60'            'Desc. Grp. Merc.'   40,
    16 ''   ''      'T_SAIDA' 'PSTDAT'             'Dt. Lançamento'     08,
    17 ''   ''      'T_SAIDA' 'DOCDAT'             'Dt. Documento'      08,
    18 ''   ''      'T_SAIDA' 'MENGE'              'Quantidade'         17,
    18 ''   ''      'T_SAIDA' 'NTGEW'              'Peso.Liq.Tot.NF'    17,
    19 ''   ''      'T_SAIDA' 'VLR_UNIT'           'Vlr. Unit'          16,
    20 ''   ''      'T_SAIDA' 'UNIT_DOLAR'         'Val. Unit Dolar'    16,
    21 ''   ''      'T_SAIDA' 'NETWRT'             'Valor NF'           21,
    22 ''   ''      'T_SAIDA' 'VLR_DOLAR'         'Valor Dolar'         21,
    23 ''   ''      'T_SAIDA' 'KURSK'              'Taxa Dolar'         12,
    24 ''   ''      'T_SAIDA' 'BASE_ICMS'          'Base Icms'          21,
    25 ''   ''      'T_SAIDA' 'ICMS'               'Vlr. ICMS'          21,
    26 ''   ''      'T_SAIDA' 'ISS'                'Vlr. ISS'           21,
    27 ''   ''      'T_SAIDA' 'INSS'               'INSS s/Fat.'        21,
    28 ''   ''      'T_SAIDA' 'INCO1'              'Incoterms parte 1.' 10,
    29 ''   ''      'T_SAIDA' 'INCO2'              'Incoterms parte 2.' 28,
    30 ''   ''      'T_SAIDA' 'OUTROS'             'Outros'             21,
    31 ''   ''      'T_SAIDA' 'EXCBAS'             'Isentos'            21,
    32 ''   ''      'T_SAIDA' 'PIS'                'Vlr. PIS'           21,
    33 ''   ''      'T_SAIDA' 'COFINS'             'Vlr. COFINS'        21,
    34 ''   ''      'T_SAIDA' 'ORDEM'              'Ordem de Venda'     10,
    35 ''   ''      'T_SAIDA' 'INSTRUCAO'          'Instrução'          10,
    36 ''   ''      'T_SAIDA' 'UTILIZACAO'         'Utilização'         20,
    37 ''   ''      'T_SAIDA' 'REMESSA'            'Doc. Rem.'          10,
    38 ''   ''      'T_SAIDA' 'ROMANEIO'           'Romaneio'           09,
    39 ''   ''      'T_SAIDA' 'REF_RO'             'Ref. Romaneio'      20,
    40 ''   ''      'T_SAIDA' 'DOC_FATURA'         'Doc. Fatura'        10,
    41 ''   ''      'T_SAIDA' 'DOC_CONTABIL'       'Doc. Contabil'      10,
    42 ''   ''      'T_SAIDA' 'ORDEM_DOC'          'Ordem Interna'      12,
    43 ''   ''      'T_SAIDA' 'DATA_REGISTRO'      'Data Registro'      10,
    44 ''   ''      'T_SAIDA' 'HORA_REGISTRO'      'Hora Registro'      10,
    45 ''   ''      'T_SAIDA' 'PARC_COLETA'        'Ponto de coleta'    35,
    46 ''   ''      'T_SAIDA' 'CID_COLETA'         'Cidade PC'          35,
    47 ''   ''      'T_SAIDA' 'UF_COLETA'          'UF Ponto Coleta'    03,
    48 ''   ''      'T_SAIDA' 'LOCAL_ENT'          'Local de entrega'   35,
    49 ''   ''      'T_SAIDA' 'UF_ENT'             'UF Local entrada'   03,
    40 ''   ''      'T_SAIDA' 'TERMINAL'           'Terminal'           35, " TERMINAL
    51 ''   ''      'T_SAIDA' 'UF_TERMINAL'        'UF Terminal'        03,
    52 ''   ''      'T_SAIDA' 'NFTYPE'             'Cat. Nota'          06,
    53 ''   ''      'T_SAIDA' 'MODEL'              'Modelo NF'          07,
    54 ''   ''      'T_SAIDA' 'REFKEY'             'Doc.Ref'            10,
    55 ''   ''      'T_SAIDA' 'MAKTX'              'Descr. Material'    35,
    56 ''   ''      'T_SAIDA' 'TAXLW1'             'CST ICMS'           3,
    57 ''   ''      'T_SAIDA' 'COD_ICMS'           'Cód. ICMS'          3, "CS2022000515 - FF  28.11.2022
    57 ''   ''      'T_SAIDA' 'LEI_ICMS'           'Lei ICMS'           20,"(Faneli)
    57 ''   ''      'T_SAIDA' 'TAXLW2'             'CST IPI'            3,
    58 ''   ''      'T_SAIDA' 'TAXLW4'             'CST COFINS'         3,
    59 ''   ''      'T_SAIDA' 'TAXLW4_XML'         'CST COFINS XML'     3,
    59 ''   ''      'T_SAIDA' 'LEI_COFINS'         'Lei COFINS'         20,"(Faneli)
    50 ''   ''      'T_SAIDA' 'TAXLW5'             'CST PIS'            3,
    61 ''   ''      'T_SAIDA' 'TAXLW5_XML'         'CST PIS XML'        3,
    62 ''   ''      'T_SAIDA' 'IVA'                'IVA'                3,
    63 ''   ''      'T_SAIDA' 'CHARG'              'LOTE'               10,
    64 ''   ''      'T_SAIDA' 'NCM'                'NCM'                10,
    65 ''   ''      'T_SAIDA' 'CHAVE_NFE'          'CHAVE de acesso doc fiscal'         44, " Rubenilson - 16.01.25 - US144011
*    66 ''   ''      'T_SAIDA' 'SHTYP'              'Tipo Transporte'    44, RJF
    66 ''   ''      'T_SAIDA' 'BEZEI'              'Tipo Transporte'    44,
    67 ''   ''      'T_SAIDA' 'ANLN1'              'Imobilizado'        13,
    68 ''   ''      'T_SAIDA' 'ANLN2'              'Subnº imobilizado'  04, "CS2022000515 - FF  28.11.2022
    69 ''   ''      'T_SAIDA' 'EBELN'              'Pedido'             10,
    70 ''   ''      'T_SAIDA' 'EBELP2'             'Item do Pedido de compras'   05, "CS2022000515 - FF  28.11.2022
    71 ''   ''      'T_SAIDA' 'KNTTP'              'CtgClsCnt'          09,
    72 ''   ''      'T_SAIDA' 'AUART'              'Tp. Ordem'          09,
    73 ''   ''      'T_SAIDA' 'AUTYP'              'Ctg. Ordem'         10,
    74 ''   ''      'T_SAIDA' 'DDTEXT'             'Desc. Ctg.'         60,
    75 ''   ''      'T_SAIDA' 'KOSTL'              'Centro Custo'       10,
    76 ''   ''      'T_SAIDA' 'DESC_CENTRO_CUST'   'Desc. Centro Custo' 50,
    77 ''   ''      'T_SAIDA' 'SAKTO'              'Conta ctb.'         10,
    78 ''   ''      'T_SAIDA' 'DESC_CONTA_CTB'     'Desc. Conta ctb.'   40,
    79 ''   ''      'T_SAIDA' 'AUFNR'              'Ordem'              12, "CS2022000515 - FF  28.11.2022

**  Begin of "CS2022000515 - FF  16.01.2023
    80 ''   ''      'T_SAIDA' 'VORNR'              'Nº operação'        04,
    81 ''   ''      'T_SAIDA' 'LTXA1'              'Descrição Operação' 40,
    82 ''   ''      'T_SAIDA' 'REGIO'              'UF do ct.ped.'      02,
    83 ''   ''      'T_SAIDA' 'RECAP'              'RECAP'              03,
    85 ''   ''      'T_SAIDA' 'RAZCLI'             'Ct.Rz. Cliente'     10, "Informação da conta razão do cliente
    86 ''   ''      'T_SAIDA' 'RAZREC'             'Ct.Rz. Receita'     10, "Informação da conta razão receita
    87 ''   ''      'T_SAIDA' 'WAERS'              'Moeda Doc'          10, "Moeda do documento
    88 ''   ''      'T_SAIDA' 'TXT20'              'Ct.Rz. Descr.'      10, "Descrição conta razão
    89 ''   ''      'T_SAIDA' 'PLACA_CAV'          'Placa do Veículo'   30, "Placa do Veículo
    90 ''   ''      'T_SAIDA' 'ITMNUM'             'Sequência item doc. fiscal'   30, "Sequência item doc. fiscal
    91 ''   ''      'T_SAIDA' 'TTYPETXT'           'Tipo de imposto'   30, "Tipo de Imposto
    92 ''   ''      'T_SAIDA' 'CHAVE_REFER'        'Chave Referenciada'   44, " Rubenilson - 16.01.25 - US144011
    93 ''   ''      'T_SAIDA' 'MAT_CHAVE_REFER'    'Material Chave refer.'   40." Rubenilson - 16.01.25 - US144011

** End of "CS2022000515 - FF  16.01.2023


    "38 ''   ''      'T_SAIDA' 'DS_TERMINAL '       'Terminal'           ' '.

  ENDIF.

ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                              VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                              VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                              VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                              VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                              VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                              VALUE(P_OUTPUTLEN)     LIKE DD03P-OUTPUTLEN.

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.
  " Comentado para usar defaul. Filtro de cliente estava com tamanho menor.
  "x_contador = STRLEN( p_scrtext_l ).

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-OUTPUTLEN     = P_OUTPUTLEN.


  IF ( ( P_FIELD EQ 'DOCNUM' ) OR  ( P_FIELD EQ 'DOC_FATURA' )  OR  ( P_FIELD EQ 'DOC_CONTABIL' ) OR  ( P_FIELD EQ 'EBELN' ) ) .
    WA_ESTRUTURA-HOTSPOT = 'X'.
  ELSE.
    CLEAR WA_ESTRUTURA-HOTSPOT.
  ENDIF.

  IF ( P_FIELD EQ 'ORDEM_DOC' ).
    WA_ESTRUTURA-LZERO = 'X'.
  ENDIF.

  IF ( P_FIELD EQ 'EBELP' ).
    WA_ESTRUTURA-NO_OUT = 'X'.
    WA_ESTRUTURA-TECH = 'X'.
  ENDIF.
*
  CASE P_FIELD.
    WHEN 'MENGE'.
      WA_ESTRUTURA-DO_SUM = 'X'.
    WHEN 'NETWRT'. "Valor NF
      WA_ESTRUTURA-DO_SUM = 'X'.
    WHEN OTHERS.
  ENDCASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES.

  DATA: W_TEXTO1(40).
  DATA: W_TEXTO2(20).
  DATA: W_TEXTO3(20).
  DATA: VG_LINES TYPE CHAR02.

  V_REPORT = SY-REPID.


*** Nome do Report
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-005.

  "CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF / Anderson Oenning
  "Empresa
  DESCRIBE TABLE P_BUKRS LINES VG_LINES.

  IF VG_LINES > 1.

    SORT P_BUKRS BY LOW.
    READ TABLE P_BUKRS INTO DATA(W_BUKRS) INDEX 1.
    DATA(EMP_INI) = W_BUKRS-LOW.

    SELECT SINGLE BUTXT FROM T001 INTO W_TEXTO2
    WHERE BUKRS EQ EMP_INI.

    "
    SORT P_BUKRS DESCENDING BY LOW.
    READ TABLE P_BUKRS INTO W_BUKRS INDEX 1.
    DATA(EMP_FIM) = W_BUKRS-LOW.

    SELECT SINGLE BUTXT FROM T001 INTO W_TEXTO3
    WHERE BUKRS EQ EMP_FIM.

    CONCATENATE 'Empresa:' EMP_INI '-' W_TEXTO2 'Até' EMP_FIM '-' W_TEXTO3 INTO W_TEXTO1 SEPARATED BY SPACE.
*** Nome da empresa
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO1.

  ELSE.
    CONCATENATE 'Empresa:' P_BUKRS '-' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
*** Nome da empresa
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO1.
  ENDIF.

  ""CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF

  SELECT SINGLE NAME1 FROM T001W INTO W_TEXTO2
    WHERE WERKS = P_BRANCH.

  CONCATENATE 'Filial:' P_BRANCH  '-' W_TEXTO2 INTO  W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.

  WRITE: SY-DATUM TO W_TEXTO2.
  CONCATENATE 'Data:' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
  WRITE: SY-UZEIT TO W_TEXTO2.
  CONCATENATE 'Hora:' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
  CONCATENATE 'Usuário:' SY-UNAME INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT.
*  REFRESH t_sort[].
*  CLEAR: t_sort.
*  t_sort-fieldname = 'BRANCH'.
*  t_sort-up = 'X'.
*  t_sort-subtot = 'X'.
*  t_sort-tabname = 'T_SAIDA'.
*  APPEND t_sort.
ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM F_USER_COMMAND USING L_UCOMM
                          L_SELFIELD TYPE SLIS_SELFIELD.

  DATA: VL_NFOBJN TYPE J_1BINTERF-NFOBJN,
        VL_DOCNUM TYPE J_1BNFDOC-DOCNUM.

  CLEAR: WA_SAIDA.

  IF L_SELFIELD-FIELDNAME = 'DOCNUM'.

    READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.

    VL_DOCNUM = L_SELFIELD-VALUE.

    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        DOC_NUMBER         = VL_DOCNUM
      IMPORTING
        OBJ_NUMBER         = VL_NFOBJN
      EXCEPTIONS
        DOCUMENT_NOT_FOUND = 1
        DOCUM_LOCK         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
      EXPORTING
        OBJ_NUMBER         = VL_NFOBJN
      EXCEPTIONS
        OBJECT_NOT_FOUND   = 1
        SCR_CTRL_NOT_FOUND = 2
        OTHERS             = 3.
  ELSE.
    CASE L_SELFIELD-FIELDNAME.
      WHEN 'DOC_FATURA'.
        READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.
        SET PARAMETER ID  'VF' FIELD WA_SAIDA-DOC_FATURA.
        CALL TRANSACTION  'VF03' AND SKIP FIRST SCREEN.

      WHEN 'DOC_CONTABIL'.
        READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.
        SET PARAMETER ID  'BLN' FIELD WA_SAIDA-DOC_CONTABIL.
        SET PARAMETER ID  'BUK' FIELD WA_SAIDA-BUKRS.
        CALL TRANSACTION  'FB03' AND SKIP FIRST SCREEN.

      WHEN 'EBELN'.
*---> CS1078990 / IR132814 --->
        READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.
*<--- CS1078990 / IR132814 <---
        SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
        SET PARAMETER ID 'BSP' FIELD WA_SAIDA-EBELP.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    ENDCASE.
  ENDIF.
*** PBI - 65246 - CSB - INICIO
  CASE  L_UCOMM.
    WHEN 'BACK'.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE SCREEN.
    WHEN 'VISAO_NFE'.
      READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.
      PERFORM F_DETALHE_NFE USING WA_SAIDA .
  ENDCASE.
*** PBI - 65246 - CSB - FIM
ENDFORM.                    "f_user_command
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_NUMERO_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCAR_NUMERO_PEDIDO .

  IF T_SAIDA[] IS NOT INITIAL.

    "Itens da nota fiscal
    SELECT DOCNUM, ITMNUM, REFTYP, REFKEY, REFITM, DOCREF FROM J_1BNFLIN
      INTO TABLE @DATA(IT_LIN)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE DOCNUM = @T_SAIDA-DOCNUM
        AND REFTYP = 'ZW'.

    IF IT_LIN[] IS NOT INITIAL.

      "Tabela de Lançamento  Fiscais NF.WRITER
      SELECT  SEQ_LCTO, EBELN, EBELP FROM ZFIWRT0008
        INTO TABLE @DATA(IT_008)
        FOR ALL ENTRIES IN @IT_LIN
        WHERE BUKRS     IN @P_BUKRS     "CS2022001012 SELEÇÃO MULTIPLA EM FILTRO TRANSAÇÃO ZCONF / Anderson Oenning
          AND SEQ_LCTO = @IT_LIN-REFKEY(10)
          AND EBELN IN @P_EBELN. ""CS2022000515 - FF  28.11.2022

    ENDIF.

*  >>>  "Buscar Pedido entrega futura -LP #102004

    "Itens da nota fiscal
    SELECT DOCNUM, ITMNUM, REFTYP, REFKEY, REFITM, DOCREF FROM J_1BNFLIN
      INTO TABLE @DATA(IT_LIN_FUT)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE DOCNUM = @T_SAIDA-DOCNUM
        AND REFTYP = 'MD'.
    IF IT_LIN_FUT[] IS NOT INITIAL.
      "Buscar pedido
      SELECT MBLNR, EBELN, EBELP FROM MSEG
        INTO TABLE @DATA(IT_MSEG)
        FOR ALL ENTRIES IN @IT_LIN_FUT
        WHERE BUKRS     IN @P_BUKRS
          AND MBLNR = @IT_LIN_FUT-REFKEY(10)
          AND BWART    EQ'801'
          AND EBELN IN @P_EBELN.
    ENDIF.

*<<<<<end

    SORT: IT_LIN BY DOCNUM,
          IT_008 BY SEQ_LCTO,
          IT_LIN_FUT BY DOCNUM,
          IT_MSEG BY MBLNR.

* Ini - RJF - CS2023000908 INCLUR COLUNA  Nº DOC ORIGINAL.

    "Itens da nota fiscal
    SELECT DOCNUM, ITMNUM, REFTYP, REFKEY, REFITM, DOCREF FROM J_1BNFLIN
      INTO TABLE @DATA(IT_LINX)
      FOR ALL ENTRIES IN @T_SAIDA
      WHERE DOCNUM = @T_SAIDA-DOCNUM.
    IF SY-SUBRC IS INITIAL.
      LOOP AT T_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>).
        " Buscar número do pedido
        READ TABLE IT_LINX INTO DATA(WA_LINX)
                  WITH KEY DOCNUM =  <FS_SAIDA>-DOCNUM BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          <FS_SAIDA>-DOCREF = WA_LINX-DOCREF. "RJF
        ENDIF.
      ENDLOOP.
    ENDIF.
* Fim - RJF - CS2023000908 INCLUR COLUNA  Nº DOC ORIGINAL.

    IF IT_008 IS NOT INITIAL OR IT_MSEG IS NOT INITIAL.
      LOOP AT T_SAIDA ASSIGNING <FS_SAIDA>.

        " Buscar número do pedido
        READ TABLE IT_LIN INTO DATA(WA_LIN)
                  WITH KEY DOCNUM =  <FS_SAIDA>-DOCNUM BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.

          READ TABLE IT_008 INTO DATA(WA_008)
                WITH KEY SEQ_LCTO = WA_LIN-REFKEY BINARY SEARCH.
          IF SY-SUBRC IS INITIAL AND <FS_SAIDA>-DOCNUM IS NOT INITIAL.

            <FS_SAIDA>-EBELN = WA_008-EBELN.
            <FS_SAIDA>-EBELP = WA_008-EBELP.

            CLEAR: WA_LIN, WA_008.

          ENDIF.

        ENDIF.

        "Entrega futura:

        READ TABLE IT_LIN_FUT INTO DATA(WA_LINFUT)
            WITH KEY DOCNUM =  <FS_SAIDA>-DOCNUM BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.

          IF <FS_SAIDA>-DOCREF IS INITIAL AND WA_LINFUT-DOCREF IS NOT INITIAL.
            <FS_SAIDA>-DOCREF = WA_LINFUT-DOCREF. "RJF
          ENDIF.

          READ TABLE IT_MSEG INTO DATA(WA_MSEG)
                WITH KEY MBLNR = WA_LINFUT-REFKEY(10) BINARY SEARCH.
          IF SY-SUBRC IS INITIAL AND <FS_SAIDA>-DOCNUM IS NOT INITIAL.

            <FS_SAIDA>-EBELN = WA_MSEG-EBELN.
            <FS_SAIDA>-EBELP = WA_MSEG-EBELP.

            CLEAR: WA_LIN, WA_008.

          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  DELETE T_SAIDA WHERE EBELN NOT IN P_EBELN. ""CS2022000515 - FF  28.11.2022

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DETALHE_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM F_DETALHE_NFE  USING P_WA_SAIDA TYPE Y_ZDADOS_SAIDA.
  IF P_WA_SAIDA-MODEL NE '55'.
    MESSAGE 'Documento não é um DANFE/DACTE' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'Z_SHOW_DETALHAMENTO_NFE'
    EXPORTING
      I_CHAVE_NFE = P_WA_SAIDA-CHAVE_NFE
    EXCEPTIONS
      NO_FOUND    = 1
      OTHERS      = 2.

  IF SY-SUBRC <> 0.
    MESSAGE 'XML não foi localizado.' TYPE 'S'.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.

  DATA: IEXTAB TYPE SLIS_EXTAB.

  MOVE '&ETA'     TO IEXTAB-FCODE .
  APPEND IEXTAB TO RT_EXTAB.
  MOVE '&REFRESH' TO IEXTAB-FCODE .
  APPEND IEXTAB TO RT_EXTAB.
  MOVE '&SAL'     TO IEXTAB-FCODE .
  APPEND IEXTAB TO RT_EXTAB.
  MOVE '&ALL'     TO IEXTAB-FCODE .
  APPEND IEXTAB TO RT_EXTAB.
  MOVE '&ILD'     TO IEXTAB-FCODE .
  APPEND IEXTAB TO RT_EXTAB.
*  MOVE '&SUM'     TO iextab-fcode .
*  APPEND iextab TO rt_extab.

  SET PF-STATUS 'Z_PF0100'  EXCLUDING RT_EXTAB.

ENDFORM. "Set_pf_status
*&---------------------------------------------------------------------*
*& Form f_alv_variant_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_LAYOUT
*&---------------------------------------------------------------------*
FORM F_ALV_VARIANT_F4  CHANGING PA_VARI.

  DATA: RS_VARIANT LIKE DISVARIANT.

  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = RS_VARIANT
      I_SAVE     = 'A'
    IMPORTING
      ES_VARIANT = RS_VARIANT
    EXCEPTIONS
      OTHERS     = 1.

  IF SY-SUBRC = 0.
    PA_VARI = RS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.
