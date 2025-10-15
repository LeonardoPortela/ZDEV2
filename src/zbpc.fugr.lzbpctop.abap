FUNCTION-POOL zbpc.                         "MESSAGE-ID .

*&---------------------------------------------------------------------*
*& Ranges
*&---------------------------------------------------------------------*
RANGES: p_bukrs      FOR zgl029_dre_dados-bukrs,
        p_gjahr      FOR zgl029_dre_dados-gjahr,
        p_gjahr_ant  FOR zgl029_dre_dados-gjahr,
        p_monat      FOR zgl029_dre_dados-monat,
        p_saknr      FOR zgl029_dre_dados-saknr,
        p_afabe      FOR anlc-afabe,
        p_dt_exec    FOR anlb-adatu,
        p_hkont      FOR bsis-hkont,
        p_set        FOR setleaf-valfrom,
        p_branch     FOR j_1bnfdoc-branch,
        p_direct     FOR j_1bnfdoc-direct,
        p_tmiss      FOR j_1bnflin-tmiss,
        p_tmpro      FOR j_1bnflin-tmiss,
        p_tmptod      FOR j_1bnflin-tmiss,
        p_nordem     FOR vbfa-vbelv,
        p_parid      FOR j_1bnfdoc-parid,
        p_docnum     FOR j_1bnfdoc-docnum,
        p_nfenum     FOR j_1bnfdoc-nfenum,
        p_cfop       FOR j_1bnflin-cfop,
        p_model      FOR j_1bnfdoc-model,
        p_crenam     FOR j_1bnfdoc-crenam ,
        p_nfe        FOR j_1bnfdoc-nfe ,
        p_nfnum      FOR j_1bnfdoc-nfnum ,
        p_mat        FOR j_1bnflin-matnr ,
        p_matkl      FOR j_1bnflin-matkl ,
        p_pstdat     FOR j_1bnfdoc-pstdat,
        p_docdat     FOR j_1bnfdoc-docdat.
*&---------------------------------------------------------------------*
*& Tipos
*&---------------------------------------------------------------------*
TYPES : BEGIN OF ty_csks,
          kostl TYPE csks-kostl,
          kokrs TYPE csks-kokrs,
        END   OF  ty_csks,

        BEGIN OF ty_setnode,
          subsetname TYPE setnode-subsetname,
        END   OF ty_setnode ,

        BEGIN OF ty_coas_aux,
          objnr(22) TYPE c,
        END   OF ty_coas_aux,

        BEGIN OF ty_nfe,
          regio   TYPE  j_1bnfe_active-regio,
          nfyear  TYPE  j_1bnfe_active-nfyear,
          nfmonth TYPE  j_1bnfe_active-nfmonth,
          stcd1   TYPE  j_1bnfe_active-stcd1,
          model   TYPE  j_1bnfe_active-model,
          serie   TYPE  j_1bnfe_active-serie,
          nfnum9  TYPE  j_1bnfe_active-nfnum9,
          docnum9 TYPE  j_1bnfe_active-docnum9,
          cdv     TYPE  j_1bnfe_active-cdv,
        END OF ty_nfe,

        BEGIN OF ty_saida_vu,
          bukrs         TYPE coep-bukrs,
          gjahr         TYPE coep-gjahr,
          perio         TYPE coep-perio,
          kstar         TYPE coep-kstar,
          matkl         TYPE mara-matkl,
          matnr         TYPE afpo-matnr,
          moeda(05)     TYPE c,
          wogbtr        TYPE coep-wogbtr,
          wkgbtr        TYPE coep-wkgbtr,
          acumulado_usd TYPE coep-wogbtr,
          acumulado_brl TYPE coep-wogbtr,

        END   OF ty_saida_vu.

*
*      BEGIN OF TY_SAIDA,
*        BUKRS            TYPE J_1BNFDOC-BUKRS,
*        BRANCH           TYPE J_1BNFDOC-BRANCH, "FILIAL
*        CFOP             TYPE J_1BNFLIN-CFOP,   "cfop
*        NFENUM           TYPE J_1BNFDOC-NFENUM, "Nr. Nota
*        SERIES           TYPE J_1BNFDOC-SERIES, "sERIE
*        MEINS            TYPE J_1BNFLIN-MEINS,  "UNIDADE
*        PSTDAT           TYPE J_1BNFDOC-PSTDAT, "DATA LANÇAMENTO
*        DOCDAT           TYPE J_1BNFDOC-DOCDAT, "DATA DOCUMENTO
*        MENGE            TYPE J_1BNFLIN-MENGE,  "Quantidade
*        NETWRT           TYPE J_1BNFLIN-NETWRT,  "VALOR TOTAL
*        BASE_ICMS        TYPE J_1BNFSTX-BASE,   "BASE ICMS
*        OUTROS           TYPE J_1BNFSTX-OTHBAS, "Outros
*        ICMS             TYPE J_1BNFSTX-TAXVAL, "Vlr.ICMS
*        PIS              TYPE J_1BNFSTX-TAXVAL, "Vlr.PIS
*        COFINS           TYPE J_1BNFSTX-TAXVAL, "Vlr.COFINS
*        ISS              TYPE J_1BNFSTX-TAXVAL, "Vlr.COFINS
*        INSS             TYPE J_1BNFSTX-TAXVAL, "Vlr.INSS s/ Fat
*        DOCNUM           TYPE J_1BNFDOC-DOCNUM, "Nº documento
*        STCD3            TYPE LFA1-STCD3,       "Inscricao Estadual
*        EXCBAS           TYPE J_1BNFSTX-EXCBAS, "ISENTAS
*        PRODUTO          TYPE MAKT-MAKTX,       "Produto
*        ORDEM            TYPE LIPS-VGBEL,
*        REMESSA          TYPE LIPS-VBELN,
*        ROMANEIO         TYPE ZSDT0001-NR_ROMANEIO,
*        REF_RO           TYPE ZSDT0001-CH_REFERENCIA,
*        UTILIZACAO       TYPE TVLVT-BEZEI,
*        DOC_FATURA       TYPE VBFA-VBELN,
*        KURSK            TYPE VBRP-KURSK,
*        VLR_DOLAR        TYPE J_1BNFLIN-NETWRT,
*        CPF_PROD         TYPE LFA1-STCD1,
*        UNIT_DOLAR       TYPE P DECIMALS 4,
*        STATUS           TYPE C LENGTH 20,      "Status
*        NF_STATUS        TYPE C LENGTH 10,      "Estornada / Ativa
*        VLR_UNIT         TYPE P DECIMALS 4,
*        "nome_clifor      TYPE c LENGTH 35,
*        COD_CLIFOR       TYPE LFA1-LIFNR,
*        NOME_CLIFOR      TYPE LFA1-NAME1,
*        UF_CLIFOR        TYPE LFA1-REGIO,
*        ORT01            TYPE LFA1-ORT01,
*        PARC_COLETA      TYPE C LENGTH 35,
*        CID_COLETA       TYPE LFA1-ORT01,
*        UF_COLETA        TYPE LFA1-REGIO,
*        LOCAL_ENT        TYPE C LENGTH 35,
*        UF_ENT           TYPE LFA1-REGIO,
*        TERMINAL         TYPE C LENGTH 35,
*        UF_TERMINAL      TYPE LFA1-REGIO,
*        NFE              TYPE J_1BNFDOC-NFE,
*        DATA(25)         TYPE C,
*        USER             TYPE SY-UNAME,
*        DOC_CONTABIL     TYPE ZDOC_EXP-VBELN,
*        NFTYPE           TYPE J_1BNFDOC-NFTYPE,
*        MODEL            TYPE J_1BNFDOC-MODEL,
*        REFKEY           TYPE J_1BNFLIN-REFKEY,
*
*        " add
*       MAKTX             TYPE J_1BNFLIN-MAKTX,
*       TAXLW1            TYPE J_1BNFLIN-TAXLW1,
*       TAXLW2            TYPE J_1BATL2-TAXSIT,
*       TAXLW4            TYPE J_1BNFLIN-TAXLW4,
*       TAXLW5            TYPE J_1BNFLIN-TAXLW5,
*       IVA               TYPE RSEG-MWSKZ,
*       CHARG             TYPE J_1BNFLIN-CHARG,
*       NCM               TYPE J_1BNFLIN-NBM,
*       CHAVE_NFE         TYPE CHAR44,
*       LEI_ICMS(150),
*       LEI_COFINS(150),
*
*      END   OF TY_SAIDA.

*&---------------------------------------------------------------------*
*& Tabelas Internas
*&---------------------------------------------------------------------*

DATA : it_setleaf                     TYPE TABLE OF setleaf,
       it_coas                        TYPE TABLE OF coas,
       it_coas_aux                    TYPE TABLE OF ty_coas_aux,
       it_afpo                        TYPE TABLE OF afpo,
       it_mara                        TYPE TABLE OF mara,
       it_bsis                        TYPE TABLE OF bsis,
       it_bsis_aux                    TYPE TABLE OF bsis,
       it_bsis_final                  TYPE TABLE OF bsis,
       it_coep                        TYPE TABLE OF coep,
       it_coep_aux                    TYPE TABLE OF coep,
       it_coep_result                 TYPE TABLE OF coep,
       it_t001                        TYPE TABLE OF t001,
       it_tka02                       TYPE TABLE OF tka02,
       it_t005                        TYPE TABLE OF t005,
       it_anla                        TYPE TABLE OF anla,
       it_anla_aux                    TYPE TABLE OF anla,
       it_anlb                        TYPE TABLE OF anlb,
       it_anlc                        TYPE TABLE OF anlc,
       it_anlp                        TYPE TABLE OF anlp,
       it_t095                        TYPE TABLE OF t095,
       it_csks                        TYPE TABLE OF ty_csks,
       it_zgl014_dre_depa             TYPE TABLE OF zgl014_dre_depa,
       it_setnode                     TYPE TABLE OF ty_setnode,
       it_sub_setnode                 TYPE TABLE OF ty_setnode,
       it_zfit0039                    TYPE TABLE OF zfit0039,
       "Movimentação Mensal
       it_movimento_mensal            TYPE TABLE OF zmovimento_mensal_bpc,
       it_movimento_mensal_zconf      TYPE TABLE OF zmovimento_mensal_bpc,
       it_ret_movimento_mensal        TYPE TABLE OF zret_movimento_mensal_bpc,
       it_ret_movimento_mensal_zconf  TYPE TABLE OF zdados_saida,
       it_zgl029_dre_dados            TYPE TABLE OF zgl029_dre_dados,
       it_zgl029_dre_dados_aux        TYPE TABLE OF zgl029_dre_dados,
       "Saldo Acumulado Mes
       it_saldo_acumulado_mes         TYPE TABLE OF zsaldo_acumulado_bpc,
       it_ret_saldo_acumulado_mes     TYPE TABLE OF zret_saldo_acumulado_bpc,
       it_zgl030_dre_acm              TYPE TABLE OF zgl030_dre_acm,
       it_zgl030_dre_acm_aux          TYPE TABLE OF zgl030_dre_acm,
       "Movimentação Mensal e Saldo Acumulado no Mês
       it_mov_mensal_sld_acml         TYPE TABLE OF zmov_mensal_sld_acml_bpc,
       it_ret_mov_mensal_sld_acml     TYPE TABLE OF zret_mov_mensal_sld_acml,
       it_zcot0008                    TYPE TABLE OF zcot0008,
       it_zcot0008_aux                TYPE TABLE OF zcot0008,
       it_zcot0008_acu                TYPE TABLE OF zcot0008,
       " Vida útil imobiliado
       it_vida_util_imobilizado       TYPE TABLE OF zvida_util_imobilizado_bpc,
       it_retvida_util_imobilizado    TYPE TABLE OF zretvida_util_imobilizado_bpc,
       it_retvida_util_imobilizado_ax TYPE TABLE OF zretvida_util_imobilizado_bpc,
       it_zfit0044                    TYPE TABLE OF zfit0044,
       it_saida_vu                    TYPE TABLE OF ty_saida_vu,
       it_nfe                         TYPE TABLE OF ty_nfe.



DATA : wa_setleaf                     TYPE setleaf,
       wa_coas                        TYPE coas,
       wa_coep                        TYPE coep,
       wa_coep_aux                    TYPE coep,
       wa_coep_result                 TYPE coep,
       wa_coas_aux                    TYPE ty_coas_aux,
       wa_afpo                        TYPE afpo,
       wa_mara                        TYPE mara,
       wa_bsis                        TYPE bsis,
       wa_bsis_aux                    TYPE bsis,
       wa_bsis_final                  TYPE bsis,
       wa_t001                        TYPE t001,
       wa_t005                        TYPE t005,
       wa_tka02                       TYPE tka02,
       wa_anla                        TYPE anla,
       wa_anla_aux                    TYPE anla,
       wa_anlb                        TYPE anlb,
       wa_anlc                        TYPE anlc,
       wa_anlp                        TYPE anlp,
       wa_t095                        TYPE t095,
       wa_csks                        TYPE ty_csks,
       wa_zgl014_dre_depa             TYPE zgl014_dre_depa,
       wa_zfit0039                    TYPE zfit0039,
       "Movimentação Mensal
       wa_movimento_mensal            TYPE zmovimento_mensal_bpc,
       wa_ret_movimento_mensal        TYPE zret_movimento_mensal_bpc,
       wa_zgl029_dre_dados            TYPE zgl029_dre_dados,
       wa_zgl029_dre_dados_aux        TYPE zgl029_dre_dados,
       "Saldo Acumulado Mes
       wa_saldo_acumulado_mes         TYPE zsaldo_acumulado_bpc,
       wa_ret_saldo_acumulado_mes     TYPE zret_saldo_acumulado_bpc,
       wa_zgl030_dre_acm              TYPE zgl030_dre_acm,
       wa_zgl030_dre_acm_aux          TYPE zgl030_dre_acm,
       "Movimentação Mensal e Saldo Acumulado no Mês
       wa_mov_mensal_sld_acml         TYPE zmov_mensal_sld_acml_bpc,
       wa_ret_mov_mensal_sld_acml     TYPE zret_mov_mensal_sld_acml,
       wa_ret_mov_mensal_sld_acml_aux TYPE zret_mov_mensal_sld_acml,
       wa_zcot0008                    TYPE zcot0008,
       wa_zcot0008_aux                TYPE zcot0008,
       wa_zcot0008_acu                TYPE zcot0008,
       " Vida útil imobiliado
       wa_vida_util_imobilizado       TYPE zvida_util_imobilizado_bpc,
       wa_retvida_util_imobilizado    TYPE zretvida_util_imobilizado_bpc,
       wa_retvida_util_imobilizado_ax TYPE zretvida_util_imobilizado_bpc,
       wa_zfit0044                    TYPE zfit0044,
       wa_saida_vu                    TYPE ty_saida_vu,
       " zconf
       wa_movimento_mensal_zconf      TYPE zmovimento_mensal_bpc,
       wa_ret_movimento_mensal_zconf  TYPE zdados_saida.
"WA_SAIDA                           TYPE TY_SAIDA.


*&---------------------------------------------------------------------*
*& Variaveis
*&---------------------------------------------------------------------*
DATA : v_acum_mes_ant1 TYPE zret_mov_mensal_sld_acml-sld_acl_mes,
       v_acum_mes_ant2 TYPE zret_mov_mensal_sld_acml-sld_acl_mes,
       v_acum_mes_ant3 TYPE zret_mov_mensal_sld_acml-sld_acl_mes.
