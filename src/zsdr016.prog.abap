*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 01/01/2012                                              &*
*& Descrição: Simulador de Vendas                                     &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK920021   10.01.2012                            &*
*& Marcos Faneli   DEVK937401   08.05.2012                            &*
*& Sara Oikawa     DEVK9A0MDO   10.08.2020   38859 - Melhorias        &*
*& Sara Oikawa     DEVK9A0NM6   07.10.2020   Melhorias Pacote 4       &*
*& Sara Oikawa     DEVK9A0OEU   19.10.2020   Melhorias Pacote 5/6     &*
*& Sara Oikawa     DEVK9A0PEK   19.11.2020   44164 Melhorias Pacote 7 &*
*& Sara Oikawa     DEVK9A0PEK   19.11.2020   39798 Melhorias Pacote 8 &*
*&--------------------------------------------------------------------&*

REPORT  ZSDR016.
INCLUDE <ICON>.
INCLUDE <CL_ALV_CONTROL>.
TYPE-POOLS: VRM, USTYP, SLIS, F4TYP.

TABLES: SPELL, ZSDT0159.

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1).

TYPES: BEGIN OF TY_ITENS,
*         DOC_SIMULACAO TYPE ZSDT0041-DOC_SIMULACAO,
         POSNR         TYPE ZSDT0041-POSNR,
         AUART         TYPE ZSDT0041-AUART,
         SPART         TYPE ZSDT0041-SPART,
         INCO1         TYPE ZSDT0041-INCO1,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
         CULTURA_APL   TYPE ZSDT0041-CULTURA_APL,
         SAFRA_APL     TYPE ZSDT0041-SAFRA_APL,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
         MATNR         TYPE ZSDT0041-MATNR,
         MAKTX         TYPE MAKT-MAKTX,
         WERKS_FORNEC  TYPE ZSDT0041-WERKS_FORNEC,
         WERKS         TYPE ZSDT0041-WERKS,
         CHARG         TYPE ZSDT0041-CHARG,
         ZMENG         TYPE ZSDT0041-ZMENG,
         ZIEME         TYPE ZSDT0041-ZIEME,
         DTVENC        TYPE ZSDT0041-DTVENC,
         DESCONTO      TYPE ZSDT0041-DESCONTO,
*         DESC_ABSOLUTO TYPE ZSDT0041-DESC_ABSOLUTO,
         DESC_ABSOLUTO TYPE  ZSDT0094-TOTAL_PROPORC,
         VLR_AJUSTE    TYPE  ZSDT0094-TOTAL_PROPORC,
         VL_UNIT       TYPE P DECIMALS 2,
*         ZWERT         TYPE ZSDT0041-ZWERT,
         ZWERT         TYPE P DECIMALS 2,
         CALCU         TYPE ZSDT0041-CALCU,
         TRUNIT        TYPE ZSDT0041-TRUNIT,
         SIUMB         TYPE ZSDT0041-SIUMB,
         TRTOT         TYPE ZSDT0041-TRTOT,
         COMPR         TYPE ZSDT0041-COMPR,
         MGCAD         TYPE ZSDT0041-MGCAD,
         MGEFE         TYPE ZSDT0041-MGEFE,
         VLR_FRETE     TYPE ZSDT0041-VLR_FRETE,
*         VLRTOT        TYPE ZSDT0041-VLRTOT,
         VLRTOT        TYPE P DECIMALS 2,
         STYLE         TYPE LVC_T_STYL,
         VBELN         TYPE VBAK-VBELN,
         VLR_ICMS      TYPE ZSDT0041-VLR_ICMS,
         VLR_COFINS    TYPE ZSDT0041-VLR_COFINS,
         VLR_PIS       TYPE ZSDT0041-VLR_PIS,
         "CS2025000025-#164218-27.01.2025-JT-Inicio
         J_1BTXSDC     TYPE ZSDT0008-J_1BTXSDC,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
         J_1BTAXLW1    TYPE ZSDT0008-J_1BTAXLW1, "*-CS2025000025-#164218-27.01.2025-JT-inicio
         J_1BTAXLW5    TYPE ZSDT0008-J_1BTAXLW5, "*-CS2025000025-#164218-27.01.2025-JT-inicio
         J_1BTAXLW4    TYPE ZSDT0008-J_1BTAXLW4, "*-CS2025000025-#164218-27.01.2025-JT-inicio

         "j_1btxsdc     TYPE zsdt0370-j_1btxsdc,
         "j_1btaxlw1    TYPE zsdt0370-j_1btaxlw1,
         "j_1btaxlw5    TYPE zsdt0370-j_1btaxlw5,
         "j_1btaxlw4    TYPE zsdt0370-j_1btaxlw4,
         "CS2025000025-#164218-27.01.2025-JT-Fim

         ZWERT_LIQDO   TYPE ZSDT0041-ZWERT_LIQDO,
       END OF TY_ITENS,

       BEGIN OF TY_ANTEC,
         MARK(1),
         VBELN                   TYPE ZSDT0159-VBELN,
         ZID_LANC                TYPE ZSDT0159-ZID_LANC,
         ADIANT                  TYPE ZSDT0159-ADIANT,
         GJAHR                   TYPE ZSDT0159-GJAHR,
         MONT_MOEDA              TYPE ZSDT0159-MONT_MOEDA,
         AUGBL                   TYPE BSAD_VIEW-AUGBL,
         AUGDT                   TYPE BSAD_VIEW-AUGDT,
         USNAM                   TYPE ZSDT0159-USNAM,
         DATA_ATUAL              TYPE ZSDT0159-DATA_ATUAL,
         USNAM_E                 TYPE ZSDT0159-USNAM_E,
         DATA_ATUAL_E            TYPE ZSDT0159-DATA_ATUAL_E,
         " 21.02.2023 - RAMON - 102323 -->
         ID_TRANSACAO_FINANCEIRA TYPE ZSDT0159-ID_TRANSACAO_FINANCEIRA,
         " 21.02.2023 - RAMON - 102323 --<
         OBJ_KEY                 TYPE ZSDT0159-OBJ_KEY,
         ESTORNO                 TYPE ZSDT0159-ESTORNO,
         BUKRS                   TYPE ZIB_CONTABIL-BUKRS,
         SEQ                     TYPE ZSDT0159-SEQ,
         LINE_COLOR(4)           TYPE C,
       END OF TY_ANTEC,

       BEGIN OF TY_INFOR,
         SPART  TYPE TSPAT-SPART,
         VTEXT  TYPE TSPAT-VTEXT,
         VLRTOT TYPE ZSDT0041-VLRTOT,
       END OF TY_INFOR,

*       BEGIN OF TY_TRANS.
*         SEQUENCIA TYPE ZSDT0090-SEQUENCIA,
*         AUART     TYPE ZSDT0090-AUART,
*         VBELN     TYPE ZSDT0090-VBELN,
*         VBELV     TYPE ZSDT0090-VBELV,
*         ESTORNO   TYPE ZSDT0090-ESTORNO,
*       END OF TY_TRANS.

       BEGIN OF TY_FORMULA,
         VLR_PERC TYPE ZSDT0042-VLR_PERC,
         VLR_ALIQ TYPE ZSDT0042-VLR_ALIQ,
       END OF TY_FORMULA,

       BEGIN OF TY_MEMO,
         TANTE          TYPE ZSDT0039-TX_JUROS,
         INSS           TYPE ZSDT0042-VLR_PERC,
         FACS(4)        TYPE P DECIMALS 2,
         PBRTO          TYPE ZSDT0040-PREC_ANT_CULT,
         VLRANT         TYPE ZSDT0040-PREC_ANT_CULT,
         VLRANT_AUX     TYPE ZSDED005,
         VLRINSS        TYPE ZSDT0040-PREC_ANT_CULT,
         VLRINSS_AUX    TYPE ZSDED005,
         VLRFACS(4)     TYPE P DECIMALS 2,
         PLIQDO         TYPE ZSDT0040-PREC_ANT_CULT,
         PLIQDO_AUX     TYPE ZSDED005,
         QUANT          TYPE ZSDT0040-TROTOTSC,
         VTA_BRUTO      TYPE P DECIMALS 2,
*#138092 - ITSOUZA - 21.05.2024 - Inicio
         FUNDEINFRA     TYPE ZSDT0042-VLR_PERC,
         VLRFUNDEIN     TYPE ZSDT0040-PREC_ANT_CULT,
         VLRFUNDEIN_AUX TYPE ZSDED005,
*#138092 - ITSOUZA - 21.05.2024 - Fim

       END OF TY_MEMO,

       BEGIN OF TY_MGLOBAL,
         MATKL TYPE T023T-WGBEZ,
         MGCAD TYPE ZSDT0041-MGCAD,
         MGEFE TYPE ZSDT0041-MGEFE,
       END OF TY_MGLOBAL,

       BEGIN OF TY_MATNR,
         MARK,
         MATNR        TYPE MARA-MATNR,
         MAKTX        TYPE MAKT-MAKTX,
         MEINS        TYPE ZSDT0036-MEINS,
         WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
         INCO1        TYPE ZSDT0036-INCO1,
         SAFRA        TYPE ZSDT0036-SAFRA,
         AUART        TYPE ZSDT0041-AUART,
         SPART        TYPE ZSDT0041-SPART,
         CHARG        TYPE ZSDT0041-CHARG,
         DTVENC       TYPE ZSDT0041-DTVENC,
       END OF TY_MATNR .

* Saida para o contrato
TYPES: BEGIN OF TY_SAIDA,
         V_VBELN_E      TYPE VBELN,
         V_VBELN_I      TYPE C LENGTH 200,

         "DESCRIÇÃO DO USUARIOS COMPANHEIRA DOCUMENTOS E ENDEREÇO.
         ID_NOME        TYPE KUNNR, " Id do Cliente
         NOME           TYPE NAME1_GP, " nome do Cliente
         RG             TYPE STCD3, " RG do Cliente
         CPF            TYPE STCD2, " CPF do Cliente
         ID_NOME_C      TYPE KONZS, " Id do Companheiro do Cliente
         NOME_C         TYPE NAME1_GP, " Nome do companheiro do cliente
         RG_C           TYPE STCD3, " RG do companheiro do Cliente
         CPF_C          TYPE STCD2, " CPF do companheiro do Cliente
         RUA            TYPE STRAS_GP, " Endereço do Cliente
         CIDADE         TYPE MCDD3, " Cidade do Cliente
         UF             TYPE REGIO, " UF do Cliente
         ESTADO         TYPE BEZEI20, " Estado do Cliente
*        ZSDT0041
         AUART          TYPE AUART, " Tipo de Mercadoria
*        ZSDT0040
         TPSIM          TYPE ZSDT0040-TPSIM, " Tipo de transação
         WAERK          TYPE WAERK, " tipo moeda
*      REAIS       TYPE DMBTR, " valor moeda Reais
         REAIS          TYPE ZSDED005, " valor moeda Reais
         REAIS_DESC     TYPE CHAR200, " desc moeda Reais

         DOLAR          TYPE ZSDED005, " valor moeda Dolar
         DOLAR_DESC     TYPE CHAR200, " Desc moeda Dolar

         VENC           TYPE BAPI_JBD_DTE_DZTERM, " vencimento moeda
         VENC_DIA       TYPE C LENGTH 2, " vencimento moeda Dia
         VENC_MES       TYPE C LENGTH 2, " vencimento moeda Mes
         VENC_ANO       TYPE C LENGTH 4, " vencimento moeda Ano


         JUROS          TYPE ZSDED002,
         JUROS_DESC     TYPE CHAR200,
         PAG_PRORROGADO TYPE CHAR1, " Pagamento prorrogado
         JUROS_DIA      TYPE N LENGTH 2,
         JUROS_MES      TYPE N LENGTH 2,
         JUROS_ANO      TYPE N LENGTH 4,

         ICON           TYPE INCO1,
         LOCAL          TYPE ZSDT0040-FAZENDA,
         LOCAL1         TYPE ORT02,
         WERKS          TYPE WERKS,
*        Dados Fiadores
         F1             TYPE ZSDT0040-FIADOR_01,
         F1_NAME        TYPE NAME1_GP,
         PROF_F1        TYPE ZSDT0040-FIADOR_01,
         RG_F1          TYPE STCD3,
         ORGAO_RG_F1    TYPE ZSDT0040-FIADOR_01,
         CPF_F1         TYPE STCD2,
         ID_F1_C        TYPE KUNNR,
         F1_C           TYPE NAME1_GP,
         PROF_F1_C      TYPE ZSDT0040-FIADOR_01,
         RG_F1_C        TYPE STCD3,
         ORGAO_RG_F1_C  TYPE ZSDT0040-FIADOR_01,
         CPF_F1_C       TYPE STCD2,

         END_F1         TYPE STRAS_GP,
         CIDADE_F1      TYPE MCDD3,
         UF_F1          TYPE REGIO,
         ESTADO_F1      TYPE BEZEI20, " Estado do F1

         F2             TYPE ZSDT0040-FIADOR_02,
         F2_NAME        TYPE NAME1_GP,
         PROF_F2        TYPE ZSDT0040-FIADOR_02,
         RG_F2          TYPE STCD3,
         ORGAO_RG_F2    TYPE ZSDT0040-FIADOR_02,
         CPF_F2         TYPE STCD2,
         ID_F2_C        TYPE KUNNR,
         F2_C           TYPE NAME1_GP,
         PROF_F2_C      TYPE ZSDT0040-FIADOR_02,
         RG_F2_C        TYPE STCD3,
         ORGAO_RG_F2_C  TYPE ZSDT0040-FIADOR_02,
         CPF_F2_C       TYPE STCD2,

         END_F2         TYPE STRAS_GP,
         CIDADE_F2      TYPE MCDD3,
         UF_F2          TYPE REGIO,
         ESTADO_F2      TYPE BEZEI20, " Estado do F2

         DIA            TYPE C LENGTH 2,
         MES            TYPE C LENGTH 2,
         ANO            TYPE C LENGTH 4,
         S_C            TYPE C LENGTH 1,
         S_F1           TYPE C LENGTH 1,
         S_F2           TYPE C LENGTH 1,
         DT_ENTREGA_SEM TYPE EPSDELDAT,
         DT_ENTREGA_DEF TYPE EPSDELDAT,
         DT_ENTREGA_FET TYPE EPSDELDAT,

       END OF TY_SAIDA.

TYPES: TG_ANTEC_F         TYPE STANDARD TABLE OF TY_ANTEC.

* Declarações do Contrato
DATA: IT_CONTRATO  TYPE TABLE OF ZSDT0040,
      WA_CONTRATO  TYPE ZSDT0040,
      IT_ITENS     TYPE TABLE OF ZSDT0041,
      IT_ITENS_AUX TYPE TABLE OF ZSDT0041,
      WA_ITENS     TYPE ZSDT0041,
      IT_KNA1      TYPE TABLE OF KNA1,
      WA_KNA1      TYPE KNA1,
      IT_KNA1_C    TYPE TABLE OF KNA1,
      WA_KNA1_C    TYPE KNA1,
      IT_KNA1_F    TYPE TABLE OF KNA1,
      WA_KNA1_F    TYPE KNA1,
      IT_MAKT      TYPE TABLE OF MAKT,
      WA_MAKT      TYPE MAKT,
      IT_SAIDA     TYPE TABLE OF TY_SAIDA,
      WA_SAIDA     TYPE TY_SAIDA,
      CONVERT      TYPE N LENGTH 10,
      WA_IMPORT    TYPE ZCONTRATO_INSUMOS,
*      WA_IMPORT   TYPE TY_SAIDA,
      C_VBELN      TYPE C LENGTH 255,
      TAXA         TYPE UKURS_CURR.

DATA: WA_T001K     TYPE T001K.

DATA: VALOR(8) TYPE P DECIMALS 2.

* Estrutura para Palavras por extenso
DATA: BEGIN OF PALAVRA,
        INTEIRO     LIKE SPELL-WORD,
        REAL(6),
        DOLAR(7),
        FILLER(3),
        DECIMAL     LIKE SPELL-DECWORD,
        CENTAVOS(8),
      END OF PALAVRA.

DATA: DECIMALS TYPE P.
DATA WORD LIKE SPELL.
DATA GV_INFO.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_ZSDT0090.
         INCLUDE TYPE ZSDT0090.
TYPES COLOR(4).
TYPES: END OF TY_ZSDT0090.

TYPES: BEGIN OF T_CURSOR,              "Typ für Cursor-Position
         FNAME LIKE D021S-FNAM,        "Feldname
         POS   LIKE SY-STEPL,            "Loop-Zeile auf akt. Seite
         VALUE LIKE D021S-FNAM,        "Inhalt des Dynprofeldes
         TC    LIKE DD04L-ROLLNAME,       "Table-Control-Name (tc+)
         TCSEC LIKE DD04L-ROLLNAME,    "TC-Zusatzattr.name (tc+_sec)
         LINE  LIKE SY-STEPL,           "Zeile in ITAB
       END OF T_CURSOR.

TYPES: BEGIN OF TY_ITENS_ID,
         INDEX   TYPE I,
         DOC     TYPE ZSDT0041-DOC_SIMULACAO,
         VBELN   TYPE ZSDT0041-VBELN,
         POSNR   TYPE ZSDT0041-POSNR,
         VLR_INI TYPE ZSDED005,
         VLR_ATU TYPE ZSDED005,
         VLR_DIF TYPE ZSDED005,
       END OF TY_ITENS_ID.


TYPES: BEGIN OF TY_PRINT,
         DT_ENTREGA_SEM TYPE ZSDT0040-DT_ENTREGA_SEM,
         DT_ENTREGA_DEF TYPE ZSDT0040-DT_ENTREGA_DEF,
         DT_ENTREGA_FET TYPE ZSDT0040-DT_ENTREGA_FET,
         JUROS_MORA     TYPE ZSDT0040-JUROS_MORA,
         FIADOR_01      TYPE ZSDT0040-FIADOR_01,
         FIADOR_02      TYPE ZSDT0040-FIADOR_02,
         CHECKYES       TYPE C,
         CHECKNO        TYPE C,
         CHECKYES1      TYPE C,
         CHECKNO1       TYPE C,
       END OF TY_PRINT.

TYPES: BEGIN OF TY_LOG_ACAO,
         ID_HISTORICO  TYPE ZSDT0186-ID_HISTORICO,
         DOC_SIMULACAO TYPE ZSDT0186-DOC_SIMULACAO,
         STATUS        TYPE ZSDT0186-STATUS,
         MOTIVO        TYPE ZSDT0186-MOTIVO,
         USNAM         TYPE ZSDT0186-USNAM,
         DATA_ATUAL    TYPE ZSDT0186-DATA_ATUAL,
         HORA_ATUAL    TYPE ZSDT0186-HORA_ATUAL,
       END OF TY_LOG_ACAO.


DATA: WA_PRINT    TYPE TY_PRINT,
      IT_0041     TYPE TABLE OF ZSDT0040,
      WA_0041     TYPE ZSDT0040,
      IT_DESABS   TYPE TABLE OF TY_ITENS WITH HEADER LINE,
      VTA_SISTPRO TYPE P DECIMALS 2,
      VLR_TOTAL   TYPE P DECIMALS 2,
*      MEIO_PAGO TYPE ZSDT0040-MEIO_PAGO.
      MEIO_PAGO   TYPE ZSDED049.

* Início - Sara Oikawa - 38859 - Agosto/2020
DATA: WG_MEIO_PAGO   TYPE CHAR1,
      WG_FLG_RECEB   TYPE FLAG,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
      WG_CULTURA_APL TYPE ZSDT0038-CULTURA.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6
TYPES: BEGIN OF TY_SIGAM,
         MARK,
         DOC_SIMULACAO     TYPE ZSDT0260-DOC_SIMULACAO,
         ID_COMPRA         TYPE ZSDT0260-ID_COMPRA,
         SAFRA             TYPE ZSDT0260-SAFRA,
         STATUS            TYPE ZSDT0260-STATUS,
         ID_FILIAL_SAP     TYPE ZSDT0260-ID_FILIAL_SAP,
         ID_MATERIAL_SAP   TYPE ZSDT0260-ID_MATERIAL_SAP,
         ID_CLIENTE_SAP    TYPE ZSDT0260-ID_CLIENTE_SAP,
         ID_FORNEC_SAP     TYPE ZSDT0260-ID_FORNEC_SAP,
         COMPRA_FIM_EXPORT TYPE ZSDT0260-COMPRA_FIM_EXPORT,
         DATA_COMPRA       TYPE ZSDT0260-DATA_COMPRA,
       END OF TY_SIGAM,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         SPERR TYPE LFA1-SPERR,
         SPERM TYPE LFA1-SPERM,
         SPERQ TYPE LFA1-SPERQ,
       END   OF TY_LFA1.

DATA: TG_SIGAM           TYPE TABLE OF TY_SIGAM,
      WG_SIGAM           TYPE TY_SIGAM,

      TG_0260            TYPE ZSDT0260_T,
      WG_0260            TYPE ZSDT0260,

      TG_LFA1            TYPE TABLE OF TY_LFA1,
      WG_LFA1            TYPE TY_LFA1,

      TG_FUNEX           TYPE TABLE OF ZSDT0001FUNEX,
      WG_FUNEX           TYPE ZSDT0001FUNEX,

      WG_CNPJ            TYPE STCD1,
      WG_CPF             TYPE STCD2,

      WG_SAFRA_0110_SG   TYPE AJAHR,
      WG_IDCOMPR_0110_SG TYPE ZDE_ID_COMPRA_SG.

* Fim - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6


DATA: WG_HEADER     TYPE ZSDT0040,
      WG_HEADER_OLD TYPE ZSDT0040.

DATA: BTN_FI TYPE C.

DATA: W_ANSWER(1),
      WL_LINES      TYPE SY-TABIX,
      TL_INDEX_ROWS TYPE LVC_T_ROW,
      WL_INDEX_ROWS TYPE LVC_S_ROW.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_X              TYPE C VALUE 'X',
           C_A              TYPE C VALUE 'A',
           C_R              TYPE C VALUE 'R',
           C_B              TYPE C VALUE 'B',
           C_T              TYPE C VALUE 'T',
           C_V              TYPE C VALUE 'V',
           C_P              TYPE C VALUE 'P',
           C_TRO(3)         TYPE C VALUE 'TRO',
           C_VIS(3)         TYPE C VALUE 'VIS',
           C_ADT(3)         TYPE C VALUE 'ADT',
           C_ADD(3)         TYPE C VALUE 'ADD',
           C_DEL(3)         TYPE C VALUE 'DEL',
           C_EXIT(4)        TYPE C VALUE 'EXIT',
           C_BACK(4)        TYPE C VALUE 'BACK',
           C_COPY(4)        TYPE C VALUE 'COPY',
           C_SAVE(4)        TYPE C VALUE 'SAVE',
           C_BLOQ(4)        TYPE C VALUE 'BLOQ',
           C_MEMO(4)        TYPE C VALUE 'MEMO',
           C_CLOS_MSG(8)    TYPE C VALUE 'CLOS_MSG',
           C_MGLOB(5)       TYPE C VALUE 'MGLOB',
           C_GERA(5)        TYPE C VALUE 'GERA',
           C_ATUAL(5)       TYPE C VALUE 'ATUAL',
           C_PRINT(5)       TYPE C VALUE 'PRINT',
           C_PRINTC(6)      TYPE C VALUE 'PRINTC',
           C_MODIF(5)       TYPE C VALUE 'MODIF',
           C_SACAS(8)       TYPE C VALUE 'QTD_SACA',
           C_DSC_ABS(7)     TYPE C VALUE 'DSC_ABS',
           C_COL_EXP(7)     TYPE C VALUE 'COL_EXP',
           C_VINC_DESC(9)   TYPE C VALUE 'VINC_DESC',
           C_APROV(5)       TYPE C VALUE 'APROV',
*Início - Sara Oikawa - 38859 - Agosto/2020
           C_ALTJUR(6)      TYPE C VALUE 'ALTJUR',
           C_ALTADT(6)      TYPE C VALUE 'ALTADT',
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
           C_ALTCSA(6)      TYPE C VALUE 'ALTCSA',
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
           C_BTN_SG(6)      TYPE C VALUE 'BTN_SG',
           C_S              TYPE C VALUE 'S',
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
           C_ALTCPG(6)      TYPE C VALUE 'ALTCPG',
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

*           c_red(4)         type c value '@0A@',
*           c_yellow(4)      type c value '@09@',
*           c_green(4)       type c value '@08@',
*           c_aguard(4)      type c value '@9R@',

           " 05.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
           C_BOLETA_VIN(10) TYPE C VALUE 'BOLETA_VIN',
           " 05.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <

           C_TPORD(5)       TYPE C VALUE 'TPORD',
           C_DESCP(5)       TYPE C VALUE 'DESCP',
           C_PROCES(6)      TYPE C VALUE 'PROCES',
           C_CANCEL(6)      TYPE C VALUE 'CANCEL',
           C_ATUALI(6)      TYPE C VALUE 'ATUALI',
           C_SEARCH(6)      TYPE C VALUE 'SEARCH',
           C_REPROV(6)      TYPE C VALUE 'REPROV',
           C_DESCONT(7)     TYPE C VALUE 'DESCONT',
           C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE',
           C_DP_CLICK(8)    TYPE C VALUE 'DP_CLICK',
           C_MAT(3)         TYPE C VALUE 'MAT',
           C_SHOW_LOG(9)    TYPE C VALUE 'SHOW_LOG',
           C_RECALC(6)      TYPE C VALUE 'RECALC'.

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA:
*       WG_HEADER          TYPE ZSDT0040,
  TG_ITENS           TYPE TABLE OF TY_ITENS WITH HEADER LINE,
  TG_ANTEC           TYPE TABLE OF TY_ANTEC WITH HEADER LINE,

  IT_ITENS_ID        TYPE TABLE OF TY_ITENS_ID,
  WA_ITENS_ID        TYPE TY_ITENS_ID,
  TG_INFOR           TYPE TABLE OF TY_INFOR WITH HEADER LINE,
*  TG_TRANS           TYPE TABLE OF TY_TRANS WITH HEADER LINE,
*  TG_TRANS           TYPE TABLE OF ZSDT0090 WITH HEADER LINE,
  TG_TRANS           TYPE TABLE OF TY_ZSDT0090 WITH HEADER LINE,

  TG_SETLEAF         TYPE TABLE OF SETLEAF WITH HEADER LINE,
  TG_SETLINET        TYPE TABLE OF SETLINET WITH HEADER LINE,
  TG_SETLEAF_CULT    TYPE TABLE OF SETLEAF WITH HEADER LINE,
  TG_SETLINET_CULT   TYPE TABLE OF SETLINET WITH HEADER LINE,
  IT_ZSDT0041_AJUSTE TYPE STANDARD TABLE OF ZSDT0041,
  WG_DESC_KUNNR      TYPE KNA1-NAME1,
*Início - Sara Oikawa - 38859 - Agosto/2020
  WG_DESC_EMISSOR    TYPE CHAR80,
  WG_DESC_ORT01      TYPE KNA1-ORT01,
  WG_DESC_REGIO      TYPE KNA1-REGIO,
*Fim - Sara Oikawa - 38859 - Agosto/2020
  WG_DESC_VKBUR      TYPE T001W-NAME1,
  WG_DESC_WAERK      TYPE TCURT-KTEXT,
  WG_DESC_CULTURA    TYPE ZSDT0038-DESCRICAO,
  WG_MEMO            TYPE TY_MEMO,
  WG_PROP            TYPE TY_MEMO,
  TG_MGLOBAL         TYPE TABLE OF TY_MGLOBAL WITH HEADER LINE,
  TG_MATNR           TYPE TABLE OF TY_MATNR WITH HEADER LINE,
  TG_LOG_ACAO        TYPE ZSDT0186,
  TG_LOG             TYPE TABLE OF ZSDT0186 WITH HEADER LINE,

*Início - Sara Oikawa - 38859 - Agosto/2020
  TG_LOG_EDICAO      TYPE TABLE OF ZSDT0091,
  WG_LOG_EDICAO      TYPE ZSDT0091,

  WL_DTLIMITE        TYPE DATUM,
  WL_QTD_DIASU       TYPE I,
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  WG_MATNR           TYPE MARA-MATNR,
  WG_GROES           TYPE MARA-GROES,
  WG_MEINS           TYPE MARA-MEINS,
  WG_GROES_STRING    TYPE CHAR30,
  WG_QTD_PROP        TYPE ZSDT0041-ZMENG,
  WG_GROES_DEC       TYPE ESECOMPAVG,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  WG_TPSIM_ANT       TYPE ZSDT0040-TPSIM,
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  WG_DESC_HBKID(50), "US - 81799 - CSB
  WG_DESC_TPCULT(30),
  WG_DESC_VENDEDOR   TYPE TVGRT-BEZEI,
  WA_STYLE           TYPE LVC_S_STYL,
  STYLE              TYPE LVC_T_STYL WITH HEADER LINE,
  WG_DESC_DAT(30)    VALUE 'Dt.Pagamento',
  WG_ACAO(10),
  S_ACAO(10),
  WG_SAVE(10),
  WG_COPY(01),
  WG_DISPLAY,
  X_FIELD(30),
  WG_MENSAGEM(30),
  WG_OBJ(40),
  WG_FLAG,
  WG_AREA_HA         TYPE I,
  WG_STATUS(4),
  WG_COLAPS(4)       VALUE '@K2@',
  INIT,
  WL_DIAS_VCT        TYPE I,
  WL_JURO_DIAS(7)    TYPE P DECIMALS 5,
  OK_CODE            TYPE SY-UCOMM,
  FIADOR_01          TYPE NAME1,
  FIADOR_02          TYPE NAME1,
  TPSIM              TYPE C LENGTH 2,
  C_ERRO             TYPE SY-SUBRC,
  CONVERT_TRATOTSC   TYPE I,
  TOTAL_SAC          TYPE I,
  _HOJE_INVDT        TYPE SY-DATUM,
  SUM_VLRTOT         TYPE ZSDT0040-VLRTOT,
  CONVERT_TEXTO      TYPE SYTITLE.

*US 143453 - INICIO - PQ
DATA: LT_VKORG TYPE TABLE OF RGSB4,
      W_VKORG  TYPE RGSB4.

RANGES: R_VKORG    FOR ZSDT0040-VKORG.
*US 143453 - FIM - PQ

* Variaveis para o PopUp do Desconto
DATA: DESCONTO TYPE P DECIMALS 2.
DATA: TOTAL TYPE P DECIMALS 2.
DATA: DESC_ACRES TYPE C LENGTH 15.

" 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
DATA GS_0308 TYPE ZSDT0308.
DATA GV_TAXA_NEG TYPE UKURSP.
" 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

DATA: TAXA_FRE TYPE ZSDT0117-KURSK.
*RSI - Case #CS0971604 Ajuste divisão frete
DATA: ERRO_TAXA_FRETE TYPE BUKRS.
*RSI - Case #CS0971604 Ajuste divisão frete
DATA: BEGIN OF GT_VALUES OCCURS 0,
        DOMVALUE_L TYPE DOMVALUE_L,
        DDTEXT     TYPE VAL_TEXT,
      END OF GT_VALUES.

DATA: TG_MSG_RET TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,
      T_USERMD   TYPE STANDARD TABLE OF RGSB4 WITH HEADER LINE.

DATA: TY_TOOLBAR TYPE STB_BUTTON.

DATA: WG_TVKOT TYPE TVKOT,
      WG_TVTWT TYPE TVTWT,
      WG_TVTW  TYPE TVTW,
      WG_TSPAT TYPE TSPAT,
      WG_TVKBT TYPE TVKBT,
      WG_TVGRT TYPE TVGRT.

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis para SHDB                                  &*
*&--------------------------------------------------------------------&*
DATA: TI_BDCDATA       TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA       LIKE LINE OF TI_BDCDATA,
      T_MESSTAB        TYPE TABLE OF BDCMSGCOLL,
      WG_DOCUMENTO(10).

DATA: WG_BDC TYPE BDCDATA,
      TG_BDC TYPE TABLE OF BDCDATA,
      TG_MSG TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      WG_MSG TYPE BDCMSGCOLL.

*&--------------------------------------------------------------------&*
*& Paineis
*&--------------------------------------------------------------------&*
DATA: DYNNR_TOP(4) TYPE N VALUE '0105'.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS: LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER1           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,

      GRID2                TYPE REF TO CL_GUI_ALV_GRID,
      G_CONTAINER          TYPE SCRFNAME VALUE 'CC_TRANS',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1

      GRID3                TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER_3          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,


      OBJ_GRID_INFOR       TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONT_INFOR       TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS TYPE SLIS_ALV_EVENT,
      EVENTS    TYPE SLIS_T_EVENT,
      T_PRINT   TYPE SLIS_PRINT_ALV,
      T_TOP     TYPE SLIS_T_LISTHEADER.

DATA: T_FIELDCATALOG   TYPE LVC_T_FCAT,
      T_FIELDCATALOG2  TYPE LVC_T_FCAT,
      T_FIELDCAT_INFOR TYPE LVC_T_FCAT,
      W_FIELDCATALOG   TYPE LVC_S_FCAT,
      TG_SELECTEDCELL  TYPE LVC_T_CELL,
      WG_SELECTEDCELL  TYPE LVC_S_CELL,
      WA_LAYOUT        TYPE LVC_S_LAYO,
      WA_STABLE        TYPE LVC_S_STBL VALUE 'XX',
      WG_CELL          TYPE LVC_S_CELL,
      TG_CELL          TYPE LVC_T_CELL.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID VALUE SY-REPID.


SELECTION-SCREEN: BEGIN OF SCREEN 999.
  PARAMETERS P_VLR AS CHECKBOX.
SELECTION-SCREEN: END OF SCREEN 999.

SELECTION-SCREEN:  BEGIN OF SCREEN 0211 AS SUBSCREEN.
  PARAMETERS: SIM RADIOBUTTON GROUP RD1 USER-COMMAND UC_S,
              NAO RADIOBUTTON GROUP RD1.
  SELECTION-SCREEN SKIP.
  PARAMETERS: VLR AS CHECKBOX USER-COMMAND CB_V.
SELECTION-SCREEN:  END OF SCREEN 0211.

CALL SCREEN 100.

AT SELECTION-SCREEN.
  PERFORM CHECK_CAMPOS.

AT SELECTION-SCREEN OUTPUT.
  PERFORM CHECK_CAMPOS.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

    CLASS-METHODS:
      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

    CLASS-METHODS:
      ON_HOTSPOT_CLICK_TRA FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA
                  ET_BAD_CELLS E_DISPLAY.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

    WL_DESACTIVE = SPACE.
    IF SY-DYNNR = '0108'.

**   variable for Toolbar Button
*      TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
*      TY_TOOLBAR-FUNCTION  = C_CLOS_MSG.
*      TY_TOOLBAR-DISABLED  = SPACE.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.

    ELSE.
      TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
      TY_TOOLBAR-FUNCTION  =  C_ADD.
      IF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF
      OR WG_ACAO EQ C_DESCONT.
        LOOP AT TG_ITENS TRANSPORTING NO FIELDS
            WHERE VBELN IS NOT INITIAL.

        ENDLOOP.
        IF SY-SUBRC IS INITIAL.
          TY_TOOLBAR-DISABLED  = 1.
        ELSE.
          TY_TOOLBAR-DISABLED  = SPACE.
        ENDIF.
      ELSE.
        TY_TOOLBAR-DISABLED  = 1.
      ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF WG_SAVE EQ C_ALTCPG.
        TY_TOOLBAR-DISABLED  = 1.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      TY_TOOLBAR-BUTN_TYPE = 0.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.


*    IF WG_DOCS-DOCNUM IS INITIAL.
*      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
*        WITH KEY GROUP1 = 'GR1'.
*      IF SY-SUBRC IS INITIAL.
*        WL_DESACTIVE = SPACE.
*      ELSE.
*        WL_DESACTIVE = 1.
*      ENDIF.
*    ELSE.
*      WL_DESACTIVE = 1.
*    ENDIF.
      TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
      TY_TOOLBAR-FUNCTION  =  C_DEL.
      IF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF
      OR WG_ACAO EQ C_DESCONT.
        LOOP AT TG_ITENS TRANSPORTING NO FIELDS
            WHERE VBELN IS NOT INITIAL.

        ENDLOOP.
        IF SY-SUBRC IS INITIAL.
          TY_TOOLBAR-DISABLED  = 1.
        ELSE.
          TY_TOOLBAR-DISABLED  = SPACE.
        ENDIF.

      ELSE.
        TY_TOOLBAR-DISABLED  = 1.
      ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF WG_SAVE EQ C_ALTCPG.
        TY_TOOLBAR-DISABLED  = 1.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      TY_TOOLBAR-BUTN_TYPE = 0.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.

*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> INI     &*
*& CH 114788 - Simulador de Vendas                                   &*
*&-------------------------------------------------------------------&*
      TY_TOOLBAR-BUTN_TYPE = 3.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.

      TY_TOOLBAR-ICON      =  ICON_SELECT_BLOCK.
      TY_TOOLBAR-FUNCTION  =  C_MAT.
      IF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF
      OR WG_ACAO EQ C_DESCONT.
        LOOP AT TG_ITENS TRANSPORTING NO FIELDS
            WHERE VBELN IS NOT INITIAL.

        ENDLOOP.
        IF SY-SUBRC IS INITIAL.
          TY_TOOLBAR-DISABLED  = 1.
        ELSE.
          TY_TOOLBAR-DISABLED  = SPACE.
        ENDIF.
      ELSE.
        TY_TOOLBAR-DISABLED  = 1.
      ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF WG_SAVE EQ C_ALTCPG.
        TY_TOOLBAR-DISABLED  = 1.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      TY_TOOLBAR-BUTN_TYPE = 0.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.
    ENDIF.

*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> END     &*
*&-------------------------------------------------------------------&*
*
*    TY_TOOLBAR-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
**   variable for Toolbar Button
*    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
*    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
*    TY_TOOLBAR-DISABLED  = SPACE.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar

    APPEND VALUE #( BUTN_TYPE = CNTB_BTYPE_BUTTON
                       FUNCTION  = 'ZCINT'
                       ICON      = '@17@'
                       TEXT      = 'Criar itinerário'
                     ) TO E_OBJECT->MT_TOOLBAR.

    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
    DATA: TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
          WL_ITENS     LIKE LINE OF TG_ITENS,
          WL_LINES     TYPE SY-TABIX.
    REFRESH: TL_ITENS_AUX.

**   User Command Botões Incluidos
*    IF P_OPERACAO IS  NOT INITIAL
*AND P_BUKRS IS    NOT INITIAL
* AND P_BRANCH IS  NOT INITIAL
*  AND P_PARVW IS  NOT INITIAL
*   AND P_PARID IS NOT INITIAL.
    CASE E_UCOMM.
*        WHEN C_CLOS_MSG.
*          IF GRID2 IS NOT INITIAL.
*            CALL METHOD GRID2->FREE.
*            FREE: CONTAINER_2, GRID2.
*          ENDIF.
**    posiciona spliter na altura x
*          IF SPLITTER IS NOT INITIAL.
*            CALL METHOD SPLITTER->SET_ROW_HEIGHT
*              EXPORTING
*                ID     = 1
*                HEIGHT = 100.
*          ENDIF.
*          LEAVE TO SCREEN 100.
      WHEN C_ADD.
*        APPEND INITIAL LINE TO TG_SAIDA.

        IF WG_HEADER-TPSIM IS NOT INITIAL.
          TL_ITENS_AUX[] = TG_ITENS[].
          REFRESH: TG_ITENS.
          LOOP AT TL_ITENS_AUX INTO WL_ITENS.
            WL_ITENS-POSNR = SY-TABIX * 10.
            APPEND WL_ITENS TO TG_ITENS.
          ENDLOOP.
          DESCRIBE TABLE TG_ITENS LINES WL_LINES.
          CLEAR: WL_ITENS.
          WL_ITENS-POSNR = ( WL_LINES + 1 ) * 10 .

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          IF WG_CULTURA_APL IS NOT INITIAL.
            WL_ITENS-CULTURA_APL = WG_CULTURA_APL.
          ENDIF.

          WL_ITENS-SAFRA_APL = WG_HEADER-SAFRA.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          APPEND WL_ITENS TO TG_ITENS.

        ENDIF.
        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.

      WHEN C_DEL.

        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          DELETE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

        ENDLOOP.

        PERFORM VLRTOT.

*        CALL SCREEN 0100.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.

*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> INI     &*
*& CH 114788 - Simulador de Vendas                                   &*
*&-------------------------------------------------------------------&*
      WHEN C_MAT.
        PERFORM VLRTOT.
        PERFORM DISPLAY_MATNR.
*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> END     &*
*&-------------------------------------------------------------------&*

      WHEN 'ZCINT'.
        PERFORM F_CRIA_INTINERARIO.


    ENDCASE.
*    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN   = '100'
        I_SHOW     = SPACE
        I_REPID    = SY-REPID
        I_POPUP    = 0
*       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*       I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM = WG_MENSAGEM
      TABLES
        IT_MSGS    = TG_MSG_RET.

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

*    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.
*    data: wl_itens like line of tg_itens,
*          tl_impo_aux like table of tg_impo.
*
*
*    if e_row gt 0.
*      read table tg_itens into wl_itens index e_row.
*      if wl_itens-matnr is not initial
*      and wl_itens-werks is not initial
*      and wl_itens-menge is not initial
*      and wl_itens-netpr is not initial.
**    posiciona spliter na altura x
*        call method splitter->set_row_height
*          exporting
*            id     = 1
*            height = 50.
*
*        vg_subscreen1 = c_dummy_header.
*
*        if grid2 is not initial.
*          call method grid2->free.
*
*        endif.
*
*        free: container_2, grid2, tl_impo_aux.
*
*        call method splitter->get_container
*          exporting
*            row       = 2
*            column    = 1
*          receiving
*            container = container_2.
*        if grid2 is initial.
*          wa_layout-no_toolbar = c_x.
*          create object grid2
*            exporting
*              i_parent = container_2.
*
*          wa_layout-cwidth_opt = c_x.
**          wa_layout-grid_title = 'Impostos'.
*          condense e_row no-gaps.
*          concatenate 'Impostos do Item' '-' wl_itens-itmnum into wa_layout-grid_title separated by space.
*          perform montar_layout_impostos.
*          perform monta_impostos tables tl_impo_aux
*                                 using e_row.
*          call method grid2->set_table_for_first_display
*            exporting
*              is_layout       = wa_layout
*            changing
*              it_fieldcatalog = t_fieldcatalog[]
*              it_outtab       = tl_impo_aux[].
*
**      *** Método de atualização de dados na Tela
*          call method grid2->refresh_table_display
*            exporting
*              is_stable = wa_stable.
*        else.
**      *** Método de atualização de dados na Tela
*          call method grid2->refresh_table_display
*            exporting
*              is_stable = wa_stable.
*
*        endif.
*        wg_dg1 = c_maximizar.
*        leave to screen 100.
*      else.
**    posiciona spliter na altura x
*        call method splitter->set_row_height
*          exporting
*            id     = 1
*            height = 100.
*      endif.
*    endif.
*
*** Método de atualização de dados na Tela
**    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD         TYPE LVC_S_MODI,
*            LV_VALUE        TYPE,
          VL_TABIX        TYPE SY-TABIX,
          VL_VALUE        TYPE LVC_VALUE,
          WL_MARA         TYPE MARA,
          WL_MAKT         TYPE MAKT,
          WL_0036         TYPE ZSDT0036,
          WL_0039         TYPE ZSDT0039,
          WL_CALCULO      TYPE ZSDT0041-CALCU,
          WL_TUNIT        TYPE ZSDED005,  "   type zsdt0041-trunit,
          WL_PRUNIT       TYPE ZSDED005,  "type zsdt0041-zwert,
          WL_DTVENC       TYPE ZSDT0041-DTVENC,
          WL_TRTOT        TYPE ZSDED005,  "type zsdt0041-trtot,
          WL_ZMENG        TYPE ZSDT0041-ZMENG,
          WL_DESC_ABS     TYPE ZSDT0041-ZMENG,
          WL_VLRTOT       TYPE ZSDED005,  "type zsdt0041-vlrtot,
          WL_COMPR        TYPE ZSDT0041-COMPR,
          WL_MATNR        TYPE ZSDT0041-MATNR,
          WL_AUART        TYPE VBAK-AUART,
          WL_0042         TYPE ZSDT0042,
          TL_0042         TYPE TABLE OF ZSDT0042,
          WL_VLR_PERC     TYPE ZSDT0042-VLR_PERC,
          WL_VLR_ALIQ     TYPE ZSDT0042-VLR_ALIQ,
          WL_TOT_VLR      TYPE ZSDT0041-ZWERT,
          WL_TOT_VLR_UNIT TYPE ZSDT0041-ZWERT,
          WL_XA           TYPE ZSDT0041-ZWERT,
          WL_XB           TYPE ZSDT0041-ZWERT,
          WL_XC           TYPE ZSDT0041-ZWERT,
          WL_XD           TYPE ZSDT0041-ZWERT,
          WL_INCO1        TYPE ZSDT0041-INCO1,
          WL_WERKS        TYPE ZSDT0041-WERKS,
          WL_SETLEAF      TYPE SETLEAF,
          WL_SETLINET     TYPE SETLINET,
          WL_DESCONTO     TYPE ZSDT0041-DESCONTO,
*          WL_NEGOCIADO    TYPE ZSDED005,
          WL_NEGOCIADO    TYPE P DECIMALS 2,
          WL_MGEFE_AUX    TYPE ZSDED005,
          WL_MGEFE        TYPE ZSDT0041-MGEFE,
          WL_CALC_AUX     TYPE ZSDT0041-CALCU,
          WL_KNA1         TYPE KNA1,
          WL_ZIEME        TYPE ZSDT0041-ZIEME,
          WL_ZSDT0087     TYPE ZSDT0087,
          WL_0037         TYPE ZSDT0037,
          C_DESC          TYPE C LENGTH 1,
          WL_ITENS        LIKE LINE OF TG_ITENS.
**** CS2022000324
    DATA: R_WERKS   TYPE RANGE OF WERKS_D,
          IT_VALUES TYPE TABLE OF RGSB4,
          WA_VALUES TYPE RGSB4,
          WA_WERKS  LIKE LINE OF R_WERKS.
**** CS2022000324
    REFRESH: TL_0042.
    CLEAR:   WL_MAKT, WL_MARA, WL_0036, WL_CALCULO,
             WL_TUNIT, WL_PRUNIT, WL_0037, WL_0039, WL_TRTOT,
             WL_ZMENG, WL_VLRTOT, WL_MATNR, WL_0042,
             WL_VLR_PERC, WL_VLR_ALIQ, WL_TOT_VLR, WL_TOT_VLR_UNIT,
             WL_AUART, TG_SETLINET, TG_SETLEAF, WL_INCO1, WL_WERKS,
             WL_SETLEAF,  WL_SETLINET, WL_DESCONTO, WL_MGEFE_AUX, WL_MGEFE, WL_KNA1, WL_ZIEME.
*
**** CS2022000324
    REFRESH IT_VALUES.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        SETNR         = 'MV45AFZZ_WERKS'
        TABLE         = 'VBAP'
        CLASS         = '0000'
        FIELDNAME     = 'WERKS'
      TABLES
        SET_VALUES    = IT_VALUES
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF SY-SUBRC IS INITIAL.

      LOOP AT IT_VALUES INTO WA_VALUES.
        WA_WERKS = 'IEQ'.
        WA_WERKS-LOW    = WA_VALUES-FROM.
        IF WA_VALUES-TO IS NOT INITIAL.
          WA_WERKS = 'IBT'.
          WA_WERKS-HIGH = WA_VALUES-TO.
        ENDIF.

        APPEND WA_WERKS TO R_WERKS.
      ENDLOOP.
    ENDIF.
**** CS2022000324


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD WHERE FIELDNAME = 'DESC_ABSOLUTO'.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.

      MOVE: WG_HEADER-DOC_SIMULACAO  TO WA_ITENS_ID-DOC,
            WL_ITENS-VBELN           TO WA_ITENS_ID-VBELN,
            WL_ITENS-POSNR           TO WA_ITENS_ID-POSNR,
            LS_GOOD-ROW_ID           TO WA_ITENS_ID-INDEX,
            WL_ITENS-VLRTOT          TO WA_ITENS_ID-VLR_INI.

      IF LINE_EXISTS( IT_ITENS_ID[ INDEX = LS_GOOD-ROW_ID ] ).
        MODIFY IT_ITENS_ID FROM WA_ITENS_ID INDEX LS_GOOD-ROW_ID TRANSPORTING DOC
                                                                              VBELN
                                                                              POSNR
                                                                              INDEX.
*                                                                              VLR_INI.
      ELSE.
        APPEND WA_ITENS_ID TO IT_ITENS_ID.
      ENDIF.
      CLEAR: WL_ITENS, WA_ITENS_ID .
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD WHERE FIELDNAME = 'MATNR'
                                                           OR FIELDNAME = 'ZMENG'
                                                           OR FIELDNAME = 'VENCI'
                                                           OR FIELDNAME = 'ZWERT'
                                                           OR FIELDNAME = 'DTVENC'
                                                           OR FIELDNAME = 'DESCONTO'
                                                           OR FIELDNAME = 'DESC_ABSOLUTO'
                                                           OR FIELDNAME = 'INCO1'.

      IF LS_GOOD-FIELDNAME =  'ZMENG'.
*        PERFORM CALCULA_ITENS.
      ENDIF.

      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.

      IF ( LS_GOOD-FIELDNAME NE 'MATNR' ).
        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_TABIX     = LS_GOOD-TABIX
            I_FIELDNAME = 'MATNR'
          IMPORTING
            E_VALUE     = WL_MATNR.
      ELSE.
        WL_MATNR = LS_GOOD-VALUE.
      ENDIF.

      SELECT SINGLE *
        FROM MARA
        INTO WL_MARA
       WHERE MATNR EQ WL_MATNR.

      CLEAR WL_ZSDT0087.

      IF ( SY-SUBRC IS INITIAL ).
        SELECT SINGLE *
          FROM MAKT
          INTO WL_MAKT
         WHERE MATNR EQ WL_MARA-MATNR
           AND SPRAS EQ SY-LANGU.

        SELECT SINGLE *
          FROM KNA1
          INTO WL_KNA1
         WHERE KUNNR EQ WG_HEADER-KUNNR.

        SELECT *
          FROM ZSDT0042
          INTO TABLE TL_0042
         WHERE CULTURA EQ WG_HEADER-CULTURA
           AND WAERK   EQ WG_HEADER-WAERK
           AND ESTADO  EQ WL_KNA1-REGIO
           AND SAFRA   EQ WG_HEADER-SAFRA  " RJF - 61516 - 2023.08.31
           AND VAL_DE  LE WG_HEADER-DTENT  " RJF - 61516 - 2023.08.31
           AND VAL_ATE GE WG_HEADER-DTENT. " RJF - 61516 - 2023.08.31

        LOOP AT TL_0042 INTO WL_0042.
          IF ( WL_0042-WITHT EQ 'FR' ).
            IF WL_KNA1-STKZN IS NOT INITIAL.
              IF WG_HEADER-FUNRURAL IS INITIAL.
                ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
              ELSE.
                ADD WL_0042-VLR_PERC1 TO WL_VLR_PERC.
              ENDIF.
              ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
            ENDIF.
*#138092 - ITSOUZA - Inicio
          ELSEIF ( WL_0042-WITHT EQ 'FI' ).
            CHECK WL_0042-ESTADO EQ 'GO'.
            IF WG_HEADER-FUNDEINFRA_EXCE IS INITIAL.
              ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
            ELSE.
              ADD WL_0042-VLR_PERC1 TO WL_VLR_PERC.
            ENDIF.
            ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
*#138092 - ITSOUZA - Fim
          ELSE.
            ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
            ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
          ENDIF.
        ENDLOOP.

        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_TABIX     = LS_GOOD-TABIX
            I_FIELDNAME = 'ZIEME'
          IMPORTING
            E_VALUE     = WL_ZIEME.

        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_TABIX     = LS_GOOD-TABIX
            I_FIELDNAME = 'INCO1'
          IMPORTING
            E_VALUE     = WL_INCO1.

        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_TABIX     = LS_GOOD-TABIX
            I_FIELDNAME = 'WERKS'
          IMPORTING
            E_VALUE     = WL_WERKS.

        SELECT SINGLE *
          FROM ZSDT0036
          INTO WL_0036
         WHERE VAL_DE       LE SY-DATUM
           AND VAL_ATE      GE SY-DATUM
           AND MATNR        EQ WL_MARA-MATNR
           AND WAERK        EQ WG_HEADER-WAERK
           AND MEINS        EQ WL_ZIEME
           AND INCO1        EQ WL_INCO1
           AND SAFRA        EQ WG_HEADER-SAFRA
           AND CULTURA      EQ WG_HEADER-CULTURA
           AND LOEKZ        EQ SPACE
           AND WERKS_FORNEC EQ WL_ITENS-WERKS
           AND ELIMINADO    EQ SPACE
           AND BUKRS        EQ WA_T001K-BUKRS.

        IF ( LS_GOOD-FIELDNAME EQ 'MATNR' ).
          MOVE WL_0036-DTVENC TO WL_ITENS-DTVENC.
        ENDIF.

        IF ( LS_GOOD-FIELDNAME NE 'WERKS' ).
          CLEAR WL_INCO1.
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_TABIX     = LS_GOOD-TABIX
              I_FIELDNAME = 'INCO1'
            IMPORTING
              E_VALUE     = WL_INCO1.
        ELSE.
          WL_INCO1 = LS_GOOD-VALUE.
        ENDIF.

        CLEAR WL_WERKS.
        IF ( WL_INCO1 EQ 'CIF' ).
          IF LS_GOOD-FIELDNAME NE 'WERKS'.
            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
              EXPORTING
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_TABIX     = LS_GOOD-TABIX
                I_FIELDNAME = 'WERKS'
              IMPORTING
                E_VALUE     = WL_WERKS.
          ELSE.
            WL_WERKS = LS_GOOD-VALUE.
          ENDIF.
        ENDIF.

        IF ( SY-SUBRC IS INITIAL ).
          SELECT SINGLE *
            FROM ZSDT0037
            INTO WL_0037
           WHERE VAL_DE          LE SY-DATUM
             AND VAL_ATE         GE SY-DATUM
             AND MEINS           EQ WL_0036-MEINS
             AND FILIAL_ORIGEM   EQ WL_WERKS
             AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
             AND MATKL           EQ WL_MARA-MATKL
             AND WAERS           EQ 'BRL'
             AND BUKRS           EQ WA_T001K-BUKRS
*             AND VLR_FRETE       EQ (
*
*           SELECT MAX( VLR_FRETE ) FROM  ZSDT0037
*             WHERE VAL_DE          LE SY-DATUM
*               AND VAL_ATE         GE SY-DATUM
*               AND MEINS           EQ WL_0036-MEINS
*               AND FILIAL_ORIGEM   EQ WL_WERKS
*               AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
*               AND MATKL           EQ WL_MARA-MATKL
*               AND WAERS           EQ WG_HEADER-WAERK
*               AND BUKRS           EQ WA_T001K-BUKRS
*              )
             .

          CASE WL_INCO1.
            WHEN 'CIF' OR 'CPT' OR 'CFR'.
              PERFORM ATUALIZA_FRETE CHANGING WL_0037-VLR_FRETE.
            WHEN OTHERS.
              WL_0037-VLR_FRETE = 0.
          ENDCASE.

          IF ( LS_GOOD-FIELDNAME NE 'DESCONTO' ).
            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
              EXPORTING
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_TABIX     = LS_GOOD-TABIX
                I_FIELDNAME = 'DESCONTO'
              IMPORTING
                E_VALUE     = WL_DESCONTO.
          ELSE.
            WL_DESCONTO = LS_GOOD-VALUE.
          ENDIF.

          IF ( LS_GOOD-FIELDNAME NE 'ZWERT' ).
            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
              EXPORTING
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_TABIX     = LS_GOOD-TABIX
                I_FIELDNAME = 'ZWERT'
              IMPORTING
                E_VALUE     = WL_NEGOCIADO.
          ELSE.
            WL_NEGOCIADO = LS_GOOD-VALUE.
          ENDIF.

          DATA: LV_VALUE_AUX TYPE F.

          IF NOT WL_0036-VLR_VENDA IS INITIAL AND
            NOT WL_NEGOCIADO IS INITIAL.
*            NOT WL_0037-VLR_FRETE IS INITIAL.
            TRY.                                                                                           "BUG 46674 - 28.10.2020
                LV_VALUE_AUX  =  ( ( WL_0036-VLR_VENDA / ( WL_NEGOCIADO - WL_0037-VLR_FRETE ) ) - 1 ) * 100.
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.

            CLEAR WL_NEGOCIADO.

          ENDIF.

* Calcula a porcentagem do valor negociado.
*         _Inicio_

*          CLEAR: WL_NEGOCIADO.

          IF ( LS_GOOD-FIELDNAME NE 'ZWERT' ).

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_NEGOCIADO.

*            CLEAR: WL_CALCULO.

            IF ( WL_DESCONTO IS INITIAL ).
              WL_NEGOCIADO = WL_ITENS-VL_UNIT.

            ELSEIF ( WL_DESCONTO LE 100 ).
*              WL_CALC_AUX =  WL_DESCONTO / 100.
              WL_CALC_AUX =  LV_VALUE_AUX / 100.
              ADD 1 TO WL_CALC_AUX.
              WL_NEGOCIADO = ( WL_0036-VLR_VENDA / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
              ADD WL_0037-VLR_FRETE TO WL_NEGOCIADO.

            ELSEIF ( WL_DESCONTO GE 0 ).
              MESSAGE 'Porcentagem de desconto ultrapassa a margem permitida.' TYPE 'S' DISPLAY LIKE 'E'.
              WL_NEGOCIADO = WL_ITENS-VL_UNIT.
              WL_DESCONTO  = SPACE.
            ENDIF.

*            MOVE: WL_NEGOCIADO TO WL_ITENS-ZWERT,
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING ZWERT.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

*            C_DESC = 'X'.
          ELSE.
*            IF ( C_DESC IS INITIAL ).
*              C_DESC = 'X'.

*            WL_NEGOCIADO = LS_GOOD-VALUE / 10000.

            WL_NEGOCIADO = LS_GOOD-VALUE.

            IF ( WL_NEGOCIADO EQ WL_ITENS-VL_UNIT ).
              WL_DESCONTO = SPACE.
            ELSE.

              DATA: "LV_VALUE_AUX TYPE F,
                    LV_VALUE     TYPE ZSDT0041-DESCONTO.

              IF ( WL_NEGOCIADO IS NOT INITIAL ).
                "LV_VALUE_AUX  = ( ( WL_0036-VLR_VENDA - ( WL_NEGOCIADO - WL_0037-VLR_FRETE ) ) / ( WL_NEGOCIADO - WL_0037-VLR_FRETE ) ) * 100.
                TRY.                                                                                           "BUG 46674 - 28.10.2020
                    LV_VALUE_AUX  =  ( ( WL_0036-VLR_VENDA / ( WL_NEGOCIADO - WL_0037-VLR_FRETE ) ) - 1 ) * 100.
                  CATCH CX_SY_ZERODIVIDE.
                ENDTRY.
              ELSE.
                WL_NEGOCIADO = WL_ITENS-VL_UNIT.
                CLEAR LV_VALUE_AUX.
              ENDIF.

              IF ( LV_VALUE_AUX > 100 ).
                MESSAGE 'Valor negociado ultrapassa a margem permitida p/ desconto.' TYPE 'S' DISPLAY LIKE 'E'.
                WL_NEGOCIADO = WL_ITENS-VL_UNIT.
                WL_DESCONTO  = SPACE.
              ELSE.
                CLEAR : LV_VALUE.
                CALL FUNCTION 'ROUND'
                  EXPORTING
                    DECIMALS = 1
                    INPUT    = LV_VALUE_AUX
                  IMPORTING
                    OUTPUT   = LV_VALUE.

                WL_DESCONTO  = LV_VALUE.
              ENDIF.
            ENDIF.

*               WL_NEGOCIADO = WL_NEGOCIADO - WL_0037-VLR_FRETE.
*               WL_DESCONTO = WL_0036-VLR_VENDA - WL_NEGOCIADO .
*               WL_DESCONTO = WL_DESCONTO /  WL_NEGOCIADO.
*               WL_DESCONTO = WL_DESCONTO * 100.

*            MOVE: WL_DESCONTO  TO WL_ITENS-DESCONTO,
*                  WL_NEGOCIADO TO WL_ITENS-ZWERT.
*
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING DESCONTO
*                                                                            ZWERT.

*              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*                EXPORTING
*                  I_ROW_ID    = LS_GOOD-ROW_ID
*                  I_TABIX     = LS_GOOD-TABIX
*                  I_FIELDNAME = 'DESCONTO'
*                  I_VALUE     = LV_VALUE.
*            ENDIF.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LS_GOOD-VALUE.
          ENDIF.

          MOVE: WL_DESCONTO  TO WL_ITENS-DESCONTO,
                WL_NEGOCIADO TO WL_ITENS-ZWERT.

          MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING DESCONTO
                                                                          ZWERT.

*         _fim _


*** Realiza o calculo do campo "Margem Efetiva"

*          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZWERT'
*            IMPORTING
*              E_VALUE     = WL_PRUNIT.

          TRY.
              WL_MGEFE = ( ( ( WL_NEGOCIADO - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_NEGOCIADO ) * 100 ).
*                move: wl_mgefe_aux to wl_mgefe.
            CATCH:
              CX_SY_ZERODIVIDE, CX_SY_ARITHMETIC_OVERFLOW.
          ENDTRY.

          MOVE WL_MGEFE TO WL_ITENS-MGEFE.
          MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING MGEFE.
*           ____


*** Realiza o calculo do campo "Valor Total item"

*          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZWERT'
*            IMPORTING
*              E_VALUE     = WL_PRUNIT.

          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_TABIX     = LS_GOOD-TABIX
              I_FIELDNAME = 'DESC_ABSOLUTO'
            IMPORTING
              E_VALUE     = WL_DESC_ABS.

          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_TABIX     = LS_GOOD-TABIX
              I_FIELDNAME = 'ZMENG'
            IMPORTING
              E_VALUE     = WL_ZMENG.

          WL_VLRTOT = WL_NEGOCIADO * WL_ZMENG - WL_DESC_ABS.

          MOVE WL_VLRTOT TO WL_ITENS-VLRTOT.

          MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING VLRTOT.
*           ____

          IF ( LS_GOOD-FIELDNAME EQ 'DESCONTO' ).
            WL_DESCONTO = LS_GOOD-VALUE.
          ENDIF.

          IF WG_HEADER-TPSIM(1) EQ C_TRO(1).

*            CLEAR: WL_CALCULO.
*
*            WL_CALC_AUX =  WL_DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_CALCULO = ( WL_0036-VLR_VENDA  / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
*            ADD WL_0037-VLR_FRETE TO  WL_CALCULO.
*
*            LV_VALUE =  WL_CALCULO.

            MOVE WL_NEGOCIADO TO WL_ITENS-CALCU.
            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING CALCU.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'CALCU'
*                I_VALUE     = LV_VALUE.

*            CLEAR: WL_CALCULO, WL_TUNIT.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'CALCU'
*              IMPORTING
*                E_VALUE     = WL_CALCULO.

** Realiza o calculo do campo "Troca Unitaria"
*            CLEAR: WL_TOT_VLR, WL_PRUNIT.
            TRY.
*                CLEAR: WL_CALCULO.
*                CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*                  EXPORTING
*                    I_ROW_ID    = LS_GOOD-ROW_ID
*                    I_TABIX     = LS_GOOD-TABIX
*                    I_FIELDNAME = 'CALCU'
*                  IMPORTING
*                    E_VALUE     = WL_CALCULO.

                WL_PRUNIT = WG_HEADER-PREC_ANT_CULT. "/ ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
                WL_TUNIT  = ( WL_NEGOCIADO / ( ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ) ) ).
*                wl_tunit = ( wl_calculo / wg_header-prec_cult ) *
*                             ( wl_0039-tx_juros / 100 + 1 ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.

            MOVE WL_TUNIT TO WL_ITENS-TRUNIT.
            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING TRUNIT.

*            MOVE: WL_TUNIT TO LV_VALUE.

            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL(
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_TABIX     = LS_GOOD-TABIX
                I_FIELDNAME = 'TRUNIT'
                I_VALUE     = WL_ITENS-TRUNIT ).

**  Realiza o calculo do campo "Valor Unitario"
*            if wg_header-tpsim(1) eq c_tro(1).
*            wl_prunit = wl_tunit  *  wg_header-prec_cult / 100 + 1.

*            CLEAR: WL_PRUNIT.
            TRY.
*                CLEAR: WL_TUNIT.
*                CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*                  EXPORTING
*                    I_ROW_ID    = LS_GOOD-ROW_ID
*                    I_TABIX     = LS_GOOD-TABIX
*                    I_FIELDNAME = 'TRUNIT'
*                  IMPORTING
*                    E_VALUE     = WL_TUNIT.

                WL_PRUNIT       = WG_HEADER-PREC_ANT_CULT. "/ ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
                WL_TOT_VLR      = ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ).
                WL_TOT_VLR_UNIT = WL_TUNIT * WL_TOT_VLR.
*                WL_XB = ( wl_prunit * ( wl_vlr_perc / 100 ) ).
*                WL_XC = ( wl_vlr_aliq ).
*                WL_XD = ( wl_prunit - ( WL_XB + WL_XC ) ).
*                wl_tot_vlr_unit = wl_tunit * WL_XD.

              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
*            CLEAR: WL_PRUNIT.
*            MOVE: WL_TOT_VLR_UNIT TO WL_PRUNIT.

*            endif.
*            MOVE: WL_PRUNIT TO LV_VALUE.

            MOVE WL_TOT_VLR_UNIT TO WL_ITENS-ZWERT.
            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING ZWERT.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Troca Total Item"
*            CLEAR: WL_TUNIT, WL_PRUNIT.

            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
              EXPORTING
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_TABIX     = LS_GOOD-TABIX
                I_FIELDNAME = 'TRUNIT'
              IMPORTING
                E_VALUE     = WL_TUNIT.

            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
              EXPORTING
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_TABIX     = LS_GOOD-TABIX
                I_FIELDNAME = 'ZMENG'
              IMPORTING
                E_VALUE     = WL_ZMENG.

            WL_TRTOT = WL_TUNIT * WL_ZMENG.

            MOVE WL_TRTOT TO WL_ITENS-TRTOT.
            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING TRTOT.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'TRTOT'
*                I_VALUE     = LV_VALUE.

*** Realiza o calculo do campo "Valor Total item"
**            CLEAR: WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZMENG'
*              IMPORTING
*                E_VALUE     = WL_ZMENG.
*
*            WL_VLRTOT = WL_PRUNIT * WL_ZMENG.
*
*            MOVE WL_VLRTOT TO WL_ITENS-VLRTOT.
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING VLRTOT.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*                I_VALUE     = LV_VALUE.

**          _____ Realiza o calculo do campo "Margem Efetiva" ____
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            TRY.
*                WL_MGEFE = ( ( ( WL_PRUNIT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_PRUNIT ) * 100 ).
**                move: wl_mgefe_aux to wl_mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*
*            MOVE WL_MGEFE TO WL_ITENS-MGEFE.
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING MGEFE.
*
**           ____

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'MGEFE'
*                I_VALUE     = LV_VALUE.

          ELSEIF WG_HEADER-TPSIM(1) EQ C_ADT(1).
**  Realiza o calculo do campo "Valor Unitario"
*            IF WL_DESCONTO GT 0.
*            WL_CALC_AUX =  WL_DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_CALCULO = ( WL_0036-VLR_VENDA  / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
*            ADD WL_0037-VLR_FRETE TO WL_CALCULO.
*            MOVE: WL_CALCULO  TO WL_PRUNIT.
**            ELSE.
**              WL_PRUNIT =  ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
**            ENDIF.
*            MOVE: WL_PRUNIT TO LV_VALUE.
*
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Valor Total item"
*            CLEAR: WL_PRUNIT, WL_ZMENG.

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZMENG'
*              IMPORTING
*                E_VALUE     = WL_ZMENG.
*
*            WL_VLRTOT = WL_PRUNIT * WL_ZMENG.
*            MOVE: WL_VLRTOT TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Compromisso"
*            CLEAR: WL_VLRTOT, WL_COMPR.

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*              IMPORTING
*                E_VALUE     = WL_VLRTOT.

** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
            TRY.
                WL_COMPR = WL_ITENS-VLRTOT / WG_HEADER-VLR_ADTO.
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
** "// Retornar AD WBARBOSA para Time Credito 09/07/2025

            MOVE WL_COMPR TO WL_ITENS-COMPR.
            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING COMPR.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'COMPR'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Margem Efetiva"

*            CLEAR: WL_PRUNIT, WL_MGEFE_AUX, WL_MGEFE.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*            TRY.
*                WL_MGEFE = ( ( ( WL_PRUNIT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_PRUNIT ) * 100 ).
**                move: wl_mgefe_aux to wl_mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*
*            MOVE: WL_MGEFE TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'MGEFE'
*                I_VALUE     = LV_VALUE.

          ELSEIF WG_HEADER-TPSIM(1) EQ C_VIS(1) OR LS_GOOD-FIELDNAME EQ 'DTVENC'.
**          Realiza o calculo do campo "Valor Unitario"
*            BREAK ABAP.
*            IF WL_DESCONTO GT 0.
*            WL_CALC_AUX =  WL_DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_CALCULO = ( WL_0036-VLR_VENDA  / WL_CALC_AUX ). "( 1 +  wl_desconto / 100 ) ) ).
*            ADD WL_0037-VLR_FRETE TO WL_NEGOCIADO.
*            MOVE: WL_CALCULO  TO WL_PRUNIT.
*            ELSE.
*              WL_PRUNIT = ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
*            ENDIF.


*            _____________________________JUROS____________________________________

*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'DTVENC'
*              IMPORTING
*                E_VALUE     = WL_DTVENC.
*
*            WL_DIAS_VCT = 0.
*
*            IF ( WG_HEADER-DTINIJUROS IS NOT INITIAL ).
*              WL_DIAS_VCT = WG_HEADER-DTPGTCULT - WG_HEADER-DTINIJUROS.
*            ENDIF.
*
*            IF ( WL_DIAS_VCT GT 0 ).
*              WL_JURO_DIAS = ( ( WG_HEADER-JUROS_ANO / 365 ) * WL_DIAS_VCT ) / 100.
*              WL_NEGOCIADO = WL_NEGOCIADO + ( WL_NEGOCIADO * WL_JURO_DIAS ).
*            ENDIF.
*
*            MOVE WL_NEGOCIADO TO WL_ITENS-ZWERT.
*            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING ZWERT.

*           ______________________________________________________________________

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Valor Total item"

*            CLEAR: WL_PRUNIT, WL_ZMENG.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZMENG'
*              IMPORTING
*                E_VALUE     = WL_ZMENG.
*
*            WL_VLRTOT = WL_PRUNIT * WL_ZMENG.
*
*            MOVE: WL_VLRTOT TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'VLRTOT'
*                I_VALUE     = LV_VALUE.

** Realiza o calculo do campo "Margem Efetiva"
*            CLEAR: WL_PRUNIT, WL_MGEFE_AUX, WL_MGEFE.
*            CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'ZWERT'
*              IMPORTING
*                E_VALUE     = WL_PRUNIT.
*
*            TRY.
*                WL_MGEFE = ( ( ( WL_PRUNIT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_PRUNIT ) * 100 ).
**                move: wl_mgefe_aux to wl_mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*            MOVE: WL_MGEFE TO LV_VALUE.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'MGEFE'
*                I_VALUE     = LV_VALUE.
          ENDIF.

          MOVE: WL_MAKT-MAKTX         TO WL_ITENS-MAKTX,
                WL_0036-WERKS_FORNEC  TO WL_ITENS-WERKS,
                WL_0036-MEINS         TO WL_ITENS-ZIEME,
                WL_0036-INCO1         TO WL_ITENS-INCO1,
                WL_0036-PERC_MARGEM   TO WL_ITENS-MGCAD.

          MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING MAKTX
                                                                          WERKS
                                                                          ZIEME
                                                                          INCO1
                                                                          MGCAD
                                                                          DTVENC.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MAKTX'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'WERKS'
*              I_VALUE     = LV_VALUE.

*          MOVE: WL_0036-DTVENC  TO LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'DTVENC'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZIEME'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'INCO1'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MGCAD'
*              I_VALUE     = LV_VALUE.

*          CLEAR WL_AUART.

          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_TABIX     = LS_GOOD-TABIX
              I_FIELDNAME = 'INCO1'
            IMPORTING
              E_VALUE     = WL_INCO1.

          CONDENSE WL_INCO1 NO-GAPS.

          "ALRS
          CLEAR WL_ZSDT0087.
          SELECT SINGLE *
            FROM ZSDT0087
            INTO WL_ZSDT0087
            WHERE MATKL = WL_MARA-MATKL
            AND   TPSIM = WG_HEADER-TPSIM
            AND   INCO1 = WL_INCO1.

          IF ( SY-SUBRC = 0 ).

            MOVE: WL_ZSDT0087-AUART TO WL_ITENS-AUART,
                  WL_ZSDT0087-SPART TO WL_ITENS-SPART,
                  WL_ZSDT0087-AUART TO WL_AUART.

            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING AUART
                                                                            SPART.


*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'AUART'
*                I_VALUE     = VL_VALUE.

* ZDEF e ZODF Removido conforme Chamado CS2016000364
            IF ( "WL_AUART EQ 'ZDEF' OR
                 WL_AUART EQ 'ZFTE'
            OR   WL_AUART EQ 'ZOFE'
            OR   WL_AUART EQ 'YFTE' "165455 - PQ 24.02.25
*            OR   WL_AUART EQ 'ZODF'
            OR   WL_AUART EQ 'ZFUT' ).

              MOVE WG_HEADER-SAFRA TO WL_ITENS-CHARG.


**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>
              CASE WL_ITENS-AUART.
                WHEN 'ZFTE' OR 'ZOFE' OR 'ZFUT'.
                  IF WL_ITENS-SPART EQ '03'.
                    CLEAR: WL_ITENS-CHARG.
                  ENDIF.
              ENDCASE.
**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>


**** Clear no campo Charg CS2016001142>>>>>
**** CS2022000324
              CASE WL_ITENS-AUART.
                WHEN 'ZFTE' OR 'ZOFE' OR 'YFTE'. "165455 - PQ 24.02.25
*                  IF wl_itens-werks EQ '0175' AND wl_mara-mtart EQ 'ZFER'.
                  IF WL_ITENS-WERKS IN R_WERKS AND WL_MARA-MTART EQ 'ZFER'.
                    CLEAR WL_ITENS-CHARG.
                  ENDIF.
              ENDCASE.
**** CS2022000324
**** Clear no campo Charg CS2016001142<<<<<

              MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING CHARG.

*              CONDENSE VL_VALUE NO-GAPS.
*              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*                EXPORTING
*                  I_ROW_ID    = LS_GOOD-ROW_ID
*                  I_TABIX     = LS_GOOD-TABIX
*                  I_FIELDNAME = 'CHARG'
*                  I_VALUE     = VL_VALUE.

            ENDIF.

            MOVE WL_ZSDT0087-SPART TO WL_ITENS-SPART.
            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING SPART.

*            CONDENSE VL_VALUE NO-GAPS.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'SPART'
*                I_VALUE     = VL_VALUE.

          ELSE.

            MOVE: SPACE TO WL_ITENS-AUART,
                  SPACE TO WL_ITENS-SPART.

            MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING AUART
                                                                            SPART.

*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'AUART'
*                I_VALUE     = VL_VALUE.
*            "
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_TABIX     = LS_GOOD-TABIX
*                I_FIELDNAME = 'SPART'
*                I_VALUE     = VL_VALUE.
          ENDIF.

        ELSE.
          MOVE: SPACE TO WL_ITENS-CALCU,
                SPACE TO WL_ITENS-TRUNIT,
                SPACE TO WL_ITENS-TRTOT,
                SPACE TO WL_ITENS-COMPR,
                SPACE TO WL_ITENS-MGCAD,
                SPACE TO WL_ITENS-ZWERT,
                SPACE TO WL_ITENS-VLRTOT,
                SPACE TO WL_ITENS-AUART,
                SPACE TO WL_ITENS-SPART,
                SPACE TO WL_ITENS-MAKTX,
                SPACE TO WL_ITENS-WERKS,
                SPACE TO WL_ITENS-DTVENC,
                SPACE TO WL_ITENS-INCO1,
                SPACE TO WL_ITENS-ZIEME.

          MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING CALCU
                                                                          TRUNIT
                                                                          TRTOT
                                                                          COMPR
                                                                          MGCAD
                                                                          ZWERT
                                                                          VLRTOT
                                                                          AUART
                                                                          SPART
                                                                          MAKTX
                                                                          WERKS
                                                                          DTVENC
                                                                          INCO1
                                                                          ZIEME.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'CALCU'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'TRUNIT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'TRTOT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'COMPR'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MGCAD'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZWERT'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'VLRTOT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'AUART'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'SPART'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'MAKTX'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'WERKS'
*              I_VALUE     = LV_VALUE.

*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'DTVENC'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'INCO1'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_TABIX     = LS_GOOD-TABIX
*              I_FIELDNAME = 'ZIEME'
*              I_VALUE     = LV_VALUE.
        ENDIF.
      ELSE.
        MOVE: SPACE TO WL_ITENS-CALCU,
              SPACE TO WL_ITENS-TRUNIT,
              SPACE TO WL_ITENS-TRTOT,
              SPACE TO WL_ITENS-COMPR,
              SPACE TO WL_ITENS-MGCAD,
              SPACE TO WL_ITENS-ZWERT,
              SPACE TO WL_ITENS-VLRTOT,
              SPACE TO WL_ITENS-AUART,
              SPACE TO WL_ITENS-SPART,
              SPACE TO WL_ITENS-MAKTX,
              SPACE TO WL_ITENS-WERKS,
              SPACE TO WL_ITENS-DTVENC,
              SPACE TO WL_ITENS-INCO1,
              SPACE TO WL_ITENS-ZIEME.

        MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING CALCU
                                                                        TRUNIT
                                                                        TRTOT
                                                                        COMPR
                                                                        MGCAD
                                                                        ZWERT
                                                                        VLRTOT
                                                                        AUART
                                                                        SPART
                                                                        MAKTX
                                                                        WERKS
                                                                        DTVENC
                                                                        INCO1
                                                                        ZIEME.

*        MOVE: SPACE TO LV_VALUE.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'CALCU'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'TRUNIT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'TRTOT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'COMPR'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'ZWERT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'VLRTOT'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'AUART'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'SPART'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'MAKTX'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'WERKS'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'DTVENC'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'INCO1'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'MGCAD'
*            I_VALUE     = LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'ZIEME'
*            I_VALUE     = LV_VALUE.
      ENDIF.
      CLEAR: WL_MAKT, WL_MARA, LV_VALUE, WL_0036,
             WL_PRUNIT, WL_CALCULO, WL_TUNIT, TG_SETLEAF, TG_SETLINET,
             WL_VLR_PERC, WL_VLR_ALIQ, WL_ZIEME, WL_INCO1, WL_KNA1, WL_MATNR,
             WL_DESCONTO, WL_ITENS, WL_CALC_AUX, WL_NEGOCIADO, WL_0037, WL_MGEFE,
             WL_DESC_ABS, WL_ZMENG, WL_VLRTOT, WL_PRUNIT, WL_TUNIT, WL_TOT_VLR,
             WL_TOT_VLR_UNIT, WL_TRTOT, WL_ZSDT0087.

      FREE: TL_0042.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD WHERE FIELDNAME = 'DESC_ABSOLUTO'.

      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.
      WA_ITENS_ID = IT_ITENS_ID[ INDEX = LS_GOOD-ROW_ID ].

      WA_ITENS_ID-VLR_ATU =  WL_ITENS-VLRTOT.
      WA_ITENS_ID-VLR_DIF = WA_ITENS_ID-VLR_ATU - WA_ITENS_ID-VLR_INI.

      MODIFY IT_ITENS_ID
        FROM WA_ITENS_ID
          TRANSPORTING VLR_ATU
                       VLR_DIF
                       WHERE INDEX EQ LS_GOOD-ROW_ID .

    ENDLOOP.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD WHERE FIELDNAME = 'ZWERT' OR FIELDNAME = 'MATNR'.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.
      PERFORM BUSCA_IMPOSTO CHANGING WL_ITENS.
      MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING VLR_ICMS ZWERT_LIQDO.
    ENDLOOP.

    PERFORM VERIFICA_ERROS.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN    = '100'
        I_SHOW      = SPACE
        I_REPID     = SY-REPID
        I_POPUP     = 0
*       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        I_SET_FIELD = 'X_FIELD'
      IMPORTING
        E_MESSAGEM  = WG_MENSAGEM
      TABLES
        IT_MSGS     = TG_MSG_RET.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

    TYPES: BEGIN OF TYL_MARA,
             MATNR TYPE MARA-MATNR,
             MATKL TYPE SETLEAF-VALFROM,
           END OF TYL_MARA.

    DATA: WL_ITENS        LIKE LINE OF TG_ITENS,
          TL_MARA         TYPE TABLE OF TYL_MARA,
          WL_MARA         TYPE TYL_MARA,
          WL_MARA_AUX     TYPE MARA,
          TL_MAKT         TYPE TABLE OF MAKT,
          WL_MAKT         TYPE MAKT,
          TL_0036         TYPE TABLE OF ZSDT0036,
          WL_0036         TYPE ZSDT0036,
          TL_0042         TYPE TABLE OF ZSDT0042,
          WL_0042         TYPE ZSDT0042,
          TL_0037         TYPE TABLE OF ZSDT0037,
          WL_0037         TYPE ZSDT0037,
          WL_0039         TYPE ZSDT0039,
          WL_VLR_PERC     TYPE ZSDT0042-VLR_PERC,
          WL_VLR_ALIQ     TYPE ZSDT0042-VLR_ALIQ,
          WL_PRUNIT       TYPE ZSDT0041-ZWERT,
          WL_MGEFE_AUX    TYPE ZSDT0041-ZWERT,
          WL_TOT_VLR      TYPE ZSDT0041-ZWERT,
          WL_TOT_VLR_UNIT TYPE ZSDT0041-ZWERT,
          LS_GOOD         TYPE LVC_S_MODI,
          TL_SETLEAF      TYPE TABLE OF SETLEAF,
          WL_SETLEAF      TYPE SETLEAF,
          TL_SETLINET     TYPE TABLE OF SETLINET,
          WL_SETLINET     TYPE SETLINET,
          WL_DESCONTO     TYPE ZSDT0041-DESCONTO,
          WL_CALC_AUX     TYPE ZSDT0041-CALCU,
          WL_KNA1         TYPE KNA1,
          WL_TROTOTSC(30),
          WL_TABIX        TYPE SY-TABIX,
          WL_VALOR_AUX    LIKE WG_HEADER-TROTOTSC,
          WL_ZSDT0087     TYPE ZSDT0087,
          TL_ITENS        LIKE TABLE OF TG_ITENS.




    CLEAR: WL_ITENS, WL_MARA, WL_MAKT, WL_0036, WL_0042, WL_0037, WL_0039, WL_VLR_PERC,
           WL_VLR_ALIQ, WL_PRUNIT, WL_TOT_VLR, WL_TOT_VLR_UNIT, LS_GOOD, TG_SETLEAF, TG_SETLINET,
           TL_0037, WL_DESCONTO, WL_MGEFE_AUX, WL_CALC_AUX, WL_KNA1, WL_VALOR_AUX.

*    REFRESH: TL_MARA, TL_MAKT, TL_0036, TL_0042, TL_ITENS.

    CHECK ET_GOOD_CELLS IS NOT INITIAL.

*    SELECT MATNR MATKL
*      FROM MARA
*      INTO TABLE TL_MARA
*   FOR ALL ENTRIES IN TG_ITENS
*     WHERE MATNR EQ TG_ITENS-MATNR.
*
*      IF SY-SUBRC IS INITIAL.
*        SELECT *
*         FROM MAKT
*         INTO TABLE TL_MAKT
*          FOR ALL ENTRIES IN TL_MARA
*          WHERE MATNR EQ TL_MARA-MATNR.
*
*        SELECT *
*           FROM ZSDT0036
*           INTO TABLE TL_0036
*           FOR ALL ENTRIES IN TL_MARA
*            WHERE VAL_DE    LE SY-DATUM
*              AND VAL_ATE   GE SY-DATUM
*              AND MATNR     EQ TL_MARA-MATNR
*              AND WAERK     EQ WG_HEADER-WAERK
*              AND SAFRA     EQ WG_HEADER-SAFRA
*              AND LOEKZ     EQ SPACE
*              AND CULTURA   EQ WG_HEADER-CULTURA
*              AND ELIMINADO EQ SPACE.
**              and werks   eq wg_header-vkbur.
*      ENDIF.
*
*
*      SELECT SINGLE *
*        FROM KNA1
*        INTO WL_KNA1
*         WHERE KUNNR EQ WG_HEADER-KUNNR.
*
*      SELECT *
*          FROM ZSDT0042
*          INTO TABLE TL_0042
*           WHERE CULTURA EQ WG_HEADER-CULTURA
*             AND WAERK   EQ WG_HEADER-WAERK
*             AND ESTADO  EQ WL_KNA1-REGIO.
*
*
*
*      CLEAR: WL_VLR_PERC, WL_VLR_ALIQ.
*      LOOP AT TL_0042 INTO WL_0042.
*
*        IF WL_0042-WITHT EQ 'FR'.
*          IF WL_KNA1-STKZN IS NOT INITIAL.
*            ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
*            ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
*          ENDIF.
*        ELSE.
*          ADD WL_0042-VLR_PERC TO WL_VLR_PERC.
*          ADD WL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
*        ENDIF.
*
*      ENDLOOP.
*
*      SELECT *
*        FROM ZSDT0037
*        INTO TABLE TL_0037
*         FOR ALL ENTRIES IN TG_ITENS
*         WHERE VAL_DE          LE SY-DATUM
*           AND VAL_ATE         GE SY-DATUM
*           AND MEINS           EQ TG_ITENS-ZIEME
*           AND FILIAL_ORIGEM   EQ TG_ITENS-WERKS
*           AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
*           AND WAERS           EQ WG_HEADER-WAERK.
*
*    ENDIF.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.
*
**      READ TABLE TL_MARA INTO WL_MARA
**        WITH KEY MATNR = WL_ITENS-MATNR.
*
      SELECT SINGLE *
        FROM MARA
        INTO WL_MARA_AUX
        WHERE MATNR = WL_ITENS-MATNR.

      IF ( SY-SUBRC IS INITIAL ).

        SELECT SINGLE *
          FROM ZSDT0087
          INTO WL_ZSDT0087
         WHERE MATKL = WL_MARA_AUX-MATKL
           AND TPSIM = WG_HEADER-TPSIM
           AND INCO1 = WL_ITENS-INCO1.

        IF ( SY-SUBRC IS INITIAL ).
          MOVE: WL_ZSDT0087-AUART TO WL_ITENS-AUART,
                WL_ZSDT0087-SPART TO WL_ITENS-SPART.
        ENDIF.
      ENDIF.
*

**     ___________________________________Commented by Enio Jesus___________________________________
*

*        READ TABLE TL_MAKT INTO WL_MAKT
*          WITH KEY MATNR = WL_MARA-MATNR.
*
*        MOVE WL_MAKT-MAKTX TO WL_ITENS-MAKTX.
*
*        READ TABLE TL_0036 INTO WL_0036
*                  WITH KEY MATNR = WL_MARA-MATNR
*                           INCO1 = WL_ITENS-INCO1
*                           MEINS = WL_ITENS-ZIEME
*                           SAFRA = WG_HEADER-SAFRA
*                           WERKS_FORNEC = WL_ITENS-WERKS
*                           DTVENC = WL_ITENS-DTVENC.

*        IF ( SY-SUBRC IS INITIAL ).
*          IF ( WL_ITENS-INCO1 EQ 'CIF' ).
*            CLEAR: WL_0037.
*            READ TABLE TL_0037 INTO WL_0037
*              WITH KEY FILIAL_ORIGEM  = WL_ITENS-WERKS
*                       MEINS          = WL_ITENS-ZIEME
*                       FILIAL_DESTINO = WG_HEADER-VKBUR
*                       WAERS          = WG_HEADER-WAERK
*                       MATKL          = WL_MARA-MATKL.
*          ELSE.
*            CLEAR: WL_0037.
*          ENDIF.
*
*          MOVE: WL_0036-MEINS        TO WL_ITENS-ZIEME,
*                WL_0036-PERC_MARGEM  TO WL_ITENS-MGCAD,
*                WL_0036-WERKS_FORNEC TO WL_ITENS-WERKS.
*
*          IF ( LS_GOOD-FIELDNAME EQ 'MATNR' ).
*            MOVE WL_0036-DTVENC TO WL_ITENS-DTVENC.
*          ENDIF.
*
*          IF WG_HEADER-TPSIM(1) EQ C_TRO(1).
*            CLEAR: WL_ITENS-CALCU, WL_PRUNIT, WL_ITENS-TRUNIT, WL_ITENS-ZWERT, WL_ITENS-TRTOT,
*                   WL_ITENS-MGEFE, WL_MGEFE_AUX, WL_CALC_AUX.
***            Realiza o calculo do campo "Calculo"
**            IF WL_ITENS-DESCONTO GT 0.
*
**            WL_CALC_AUX =  WL_ITENS-DESCONTO / 100.
**            ADD 1 TO WL_CALC_AUX.
**            WL_ITENS-CALCU = ( WL_0036-VLR_VENDA / WL_CALC_AUX ).
**            ADD WL_0037-VLR_FRETE  TO WL_ITENS-CALCU.
*
*
**              MOVE: WL_ITENS-CALCU TO WL_ITENS-ZWERT.
**            ELSE.
**              WL_ITENS-CALCU = ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
**              WL_ITENS-ZWERT = WL_ITENS-CALCU.
**            ENDIF.
*
***            Realiza o calculo do campo "Troca Unitaria"
**            TRY.
**                WL_PRUNIT = WG_HEADER-PREC_CULT / ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
**                WL_ITENS-TRUNIT = ( WL_ITENS-CALCU / ( ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ) ) ).
**              CATCH CX_SY_ZERODIVIDE.
**            ENDTRY.
*
***            Realiza o calculo do campo "Valor Unitario"
*            CLEAR: WL_PRUNIT.
*            TRY.
*                WL_PRUNIT = WG_HEADER-PREC_CULT / ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
*                WL_TOT_VLR = ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ).
*                WL_ITENS-ZWERT = WL_ITENS-TRUNIT * WL_TOT_VLR.
**                MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
***            Realiza o calculo do campo "Troca Total Item"
*            WL_ITENS-TRTOT = WL_ITENS-TRUNIT * WL_ITENS-ZMENG.
***            Realiza o calculo do campo "valor Total Item"
*            WL_ITENS-VLRTOT = ( WL_ITENS-ZWERT * WL_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
***           Realiza o calculo do campo "Margem Efetiva"
*            TRY.
*                WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_ITENS-ZWERT ) * 100 ).
**                move: wl_mgefe_aux to wl_itens-mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*                WL_ITENS-MGEFE = 0.
*            ENDTRY.
*
*          ELSEIF WG_HEADER-TPSIM(1) EQ C_ADT(1).
*            CLEAR: WL_ITENS-ZWERT, WL_ITENS-VLRTOT, WL_ITENS-COMPR, WL_ITENS-MGEFE, WL_MGEFE_AUX,
*                   WL_CALC_AUX.
***          Realiza o calculo do campo "Valor Unitario"
**            IF WL_ITENS-DESCONTO GT 0.
*
*
**            WL_CALC_AUX =  WL_ITENS-DESCONTO / 100.
**            ADD 1 TO WL_CALC_AUX.
**            WL_ITENS-ZWERT = ( WL_0036-VLR_VENDA / WL_CALC_AUX ).
***              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
**            ADD WL_0037-VLR_FRETE  TO WL_ITENS-ZWERT.
**
***            ELSE.
***              WL_ITENS-VL_UNIT = WL_ITENS-ZWERT =  ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
***              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
***            ENDIF.
****          Realiza o calculo do campo "Valor Total item"
**            WL_ITENS-VLRTOT = ( WL_ITENS-ZWERT * WL_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
****          Realiza o calculo do campo "Compromisso"
**            TRY.
**                WL_ITENS-COMPR = WL_ITENS-VLRTOT / WG_HEADER-VLR_ADTO.
**              CATCH CX_SY_ZERODIVIDE.
**            ENDTRY.
****           Realiza o calculo do campo "Margem Efetiva"
**            TRY.
**                WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_ITENS-ZWERT ) * 100 ).
***                move: wl_mgefe_aux to wl_itens-mgefe.
**              CATCH CX_SY_ZERODIVIDE.
**                WL_ITENS-MGEFE = 0.
**            ENDTRY.
*
*          ELSEIF WG_HEADER-TPSIM(1) EQ C_VIS(1) OR LS_GOOD-FIELDNAME EQ  'DTVENC'.
*            CLEAR: WL_ITENS-ZWERT, WL_ITENS-VLRTOT, WL_ITENS-MGEFE, WL_MGEFE_AUX, WL_CALC_AUX.
***          Realiza o calculo do campo "Valor Unitario"
**            IF WL_ITENS-DESCONTO GT 0.
*            WL_CALC_AUX =  WL_ITENS-DESCONTO / 100.
*            ADD 1 TO WL_CALC_AUX.
*            WL_ITENS-ZWERT = ( WL_0036-VLR_VENDA / WL_CALC_AUX ).
**              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
*            ADD WL_0037-VLR_FRETE  TO WL_ITENS-ZWERT.
**              WL_ITENS-ZWERT = WL_ITENS-CALCU.
**            ELSE.
**              WL_ITENS-ZWERT = ( WL_0036-VLR_VENDA + WL_0037-VLR_FRETE ).
**              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
**               WL_ITENS-calcu =  WL_ITENS-ZWERT.
**            ENDIF.
*
*            "JUROS
*            WL_DIAS_VCT = 0.
*            IF WG_HEADER-DTINIJUROS IS NOT INITIAL.
*              WL_DIAS_VCT = WG_HEADER-DTPGTCULT - WG_HEADER-DTINIJUROS.
*            ENDIF.
*            IF WL_DIAS_VCT GT 0.
*              WL_JURO_DIAS = ( ( WG_HEADER-JUROS_ANO / 365 ) * WL_DIAS_VCT ) / 100.
*              WL_ITENS-ZWERT = WL_ITENS-ZWERT + ( WL_ITENS-ZWERT * WL_JURO_DIAS ).
**              MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.
*            ENDIF.
*
***          Realiza o calculo do campo "Valor Total item"
*            WL_ITENS-VLRTOT = ( WL_ITENS-ZWERT * WL_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
***           Realiza o calculo do campo "Margem Efetiva"
*            TRY.
*                WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / WL_ITENS-ZWERT ) * 100 ).
**                move: wl_mgefe_aux to wl_itens-mgefe.
*              CATCH CX_SY_ZERODIVIDE.
*                WL_ITENS-MGEFE = 0.
*            ENDTRY.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        CLEAR: WL_ITENS-AUART, WL_ITENS-SPART, WL_ITENS-MAKTX.
*      ENDIF.

**       ___________________________________________________________________________________________

      CASE LS_GOOD-FIELDNAME.
        WHEN 'ZWERT'.
          IF ( WL_ITENS-DESCONTO IS INITIAL ).
            WL_ITENS-ZWERT    = WL_ITENS-VL_UNIT.
          ELSE.
            "WL_ITENS-ZWERT = ( WL_ITENS-ZWERT / 10000 ).
          ENDIF.

        WHEN 'DESCONTO'.
          IF ( LS_GOOD-VALUE > 100 ).
            WL_ITENS-DESCONTO = SPACE.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING ZWERT
                                                                      DESCONTO
                                                                      AUART
                                                                      SPART.
      CLEAR: WL_MAKT,
             WL_MARA,
             WL_0036,
             WL_ITENS,
             WL_0037.
    ENDLOOP.

***** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*   PERFORM calcula_header USING wl_tot_vlr.
    IF NOT WG_SAVE EQ C_ALTCSA.
      PERFORM CALCULA_HEADER USING WL_TOT_VLR.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

*    IF ( WG_HEADER-TPSIM(1) EQ C_ADT(1)
*    OR   WG_HEADER-TPSIM(1) EQ C_B     ).
*
*      CLEAR: WL_TOT_VLR, WG_HEADER-VLRTOT, WG_HEADER-AREA_PENHOR, WG_HEADER-COMPRSC.
*
*      LOOP AT TG_ITENS INTO WL_ITENS.
*        ADD WL_ITENS-VLRTOT TO WL_TOT_VLR.
***    Realiza o calculo do campo "Valor Total"
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
***    Realiza o calculo do campo "Compromisso/sac"
*        ADD WL_ITENS-COMPR  TO WG_HEADER-COMPRSC.
*      ENDLOOP.
***    Realiza o calculo do campo "Area de Penhor"
*      TRY.
*          WG_HEADER-AREA_PENHOR = WL_TOT_VLR / WG_HEADER-ADTO_HA.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*
*
*    ELSEIF ( WG_HEADER-TPSIM(1) EQ C_TRO(1) ).
**        BREAK ABAP.
*
***__________________________________________________________________________________
*      CLEAR: WG_HEADER-TROTOTSC, WG_HEADER-SCHA, WG_HEADER-VLRTOT, CONVERT_TRATOTSC.
*      LOOP AT TG_ITENS INTO WL_ITENS.
*
**    Realiza o calculo do campo "Troca Total Sc"
*        ADD: WL_ITENS-TRTOT TO WG_HEADER-TROTOTSC,
*             WL_ITENS-TRTOT TO CONVERT_TRATOTSC.
*
***    Realiza o calculo do campo "Valor Total"
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
*
*      ENDLOOP.
*
**      MODIFY TG_ITENS FROM WL_ITENS TRANSPORTING TRTOT VLRTOT.
***__________________________________________________________________________________
*
**        WRITE WG_HEADER-TROTOTSC TO WL_TROTOTSC.
**        CONDENSE WL_TROTOTSC NO-GAPS.
**        FIND ','  IN WL_TROTOTSC MATCH OFFSET WL_TABIX.
***      WL_TABIX = SY-TABIX.
**        ADD 1 TO WL_TABIX.
**        IF WL_TROTOTSC+WL_TABIX(1) GT 5.
**          WL_TROTOTSC+WL_TABIX(3) = '000'.
**          TRANSLATE WL_TROTOTSC USING: '. '.
**          TRANSLATE WL_TROTOTSC USING: ', '.
**          TRANSLATE WL_TROTOTSC+WL_TABIX USING: '0 '.
**          CONDENSE WL_TROTOTSC NO-GAPS.
**
**          MOVE: WL_TROTOTSC TO WG_HEADER-TROTOTSC.
**          ADD 1 TO WG_HEADER-TROTOTSC.
**        ELSEIF WL_TROTOTSC+WL_TABIX(1) LT 5.
**          WL_TROTOTSC+WL_TABIX(3) = '000'.
**          TRANSLATE WL_TROTOTSC USING: '. '.
**          TRANSLATE WL_TROTOTSC USING: ', '.
**          TRANSLATE WL_TROTOTSC+WL_TABIX USING: '0 '.
**          CONDENSE WL_TROTOTSC NO-GAPS.
**
**          MOVE: WL_TROTOTSC TO WG_HEADER-TROTOTSC.
**
**        ENDIF.
***__________________________________________________________________________________
*
*      PERFORM MONTA_MEMO.
***__________________________________________________________________________________
**      WL_VALOR_AUX = WG_HEADER-TROTOTSC * WG_MEMO-PLIQDO_AUX.
**      SUBTRACT WG_HEADER-VLRTOT FROM WL_VALOR_AUX.
**      TL_ITENS[] = TG_ITENS[].
**      SORT: TL_ITENS BY ZMENG DESCENDING.
**      READ TABLE TL_ITENS INTO TG_ITENS INDEX 1.
**      TG_ITENS-ZWERT = ( ( WL_VALOR_AUX / TG_ITENS-ZMENG ) + TG_ITENS-ZWERT ).
**      TG_ITENS-VLRTOT = ( TG_ITENS-ZWERT * TG_ITENS-ZMENG ) - WL_ITENS-DESC_ABSOLUTO.
**      CLEAR: WL_0036.
**      READ TABLE TL_0036 INTO WL_0036
**             WITH KEY MATNR = TG_ITENS-MATNR.
***                   werks = wg_header-vkbur.
**
**      IF TG_ITENS-INCO1 EQ 'CIF'.
**        CLEAR: WL_0037.
**        READ TABLE TL_MARA INTO WL_MARA
**                  WITH KEY MATNR = TG_ITENS-MATNR.
**        READ TABLE TL_0037 INTO WL_0037
**          WITH KEY FILIAL_ORIGEM = TG_ITENS-WERKS
**                   MEINS         = TG_ITENS-ZIEME
**                   MATKL         = WL_MARA-MATKL.\
**      ELSE.
**        CLEAR: WL_0037.
**      ENDIF.
***           Realiza o calculo do campo "Margem Efetiva"
**      TRY.
**          TG_ITENS-MGEFE = ( ( ( TG_ITENS-ZWERT - WL_0036-VLR_CUSTO - WL_0037-VLR_FRETE ) / TG_ITENS-ZWERT ) * 100 ).
***                move: wl_mgefe_aux to wl_itens-mgefe.
**        CATCH CX_SY_ZERODIVIDE.
**          TG_ITENS-MGEFE = 0.
**      ENDTRY.
*
*      CLEAR: WG_HEADER-VLRTOT.
*      LOOP AT TG_ITENS INTO WL_ITENS.
***    Realiza o calculo do campo "Valor Total"
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
*      ENDLOOP.
*
****   Calculo Sc/Há
*      TRY.
*          WG_HEADER-SCHA = WG_HEADER-TROTOTSC / WG_AREA_HA. "WG_HEADER-AREA_HA.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*    ELSE.
**    Realiza o calculo do campo "Valor Total"
*      CLEAR: WG_HEADER-VLRTOT.
*      LOOP AT TG_ITENS INTO WL_ITENS.
*        ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
*      ENDLOOP.
*
*    ENDIF.

    IF ( E_MODIFIED IS NOT INITIAL ).
      LEAVE TO SCREEN 100.
    ENDIF.

*    REFRESH: TL_MARA, TL_MAKT, TL_T001W, TL_SAIDA.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
*       WHERE TABIX GT 0.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*      APPEND WL_SAIDA TO TL_SAIDA.
*    ENDLOOP.
*
*    IF TL_SAIDA[] IS NOT INITIAL.
*      SELECT WERKS NAME1
*        FROM T001W
*        INTO TABLE TL_T001W
*         FOR ALL ENTRIES IN TL_SAIDA
*          WHERE WERKS EQ TL_SAIDA-WERKS.
*
*      SELECT MATNR MAKTX
*        FROM MAKT
*        INTO TABLE TL_MAKT
*         FOR ALL ENTRIES IN TL_SAIDA
*         WHERE MATNR EQ TL_SAIDA-MATNR.
*
*      SELECT MATNR MATKL
*        FROM MARA
*        INTO TABLE TL_MARA
*         FOR ALL ENTRIES IN TL_SAIDA
*         WHERE MATNR EQ TL_SAIDA-MATNR.
*
*    ENDIF.
*    SORT: TL_MAKT BY MATNR,
*          TL_MARA BY MATNR,
*          TL_T001W BY WERKS.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
*      WHERE TABIX GT 0.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*      READ TABLE TL_MAKT INTO WL_MAKT
*        WITH KEY MATNR = WL_SAIDA-MATNR
*                 BINARY SEARCH.
*
*      READ TABLE TL_MARA INTO WL_MARA
*        WITH KEY MATNR = WL_SAIDA-MATNR
*                 BINARY SEARCH.
*
*      READ TABLE TL_T001W INTO WL_T001W
*        WITH KEY WERKS = WL_SAIDA-WERKS
*                 BINARY SEARCH.
*
*      MOVE: WL_MAKT-MAKTX TO WL_SAIDA-MAKTX,
*            WL_MARA-MATKL TO WL_SAIDA-MATKL,
*            WL_T001W-NAME1 TO WL_SAIDA-NAME1.
*
*      MODIFY TG_SAIDA FROM WL_SAIDA INDEX LS_GOOD-ROW_ID.
*      CLEAR: WL_SAIDA, WL_T001W, WL_MARA, WL_MAKT.
*    ENDLOOP.
*
*    PERFORM VERIFICA_ERROS.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        I_SCREEN      = '100'
*        I_SHOW        = SPACE
*        I_REPID       = SY-REPID
*        I_POPUP       = 1
**            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**            I_SET_FIELD   = 'X_FIELD'
*      IMPORTING
*        E_MESSAGEM    = WG_MENSAGEM
*      TABLES
*        IT_MSGS       = TG_MSG_RET.
*
**      read table tg_itens into wl_itens index ls_good-row_id.
**      if sy-subrc is initial.
**
***    LOOP AT tg_itens INTO wl_itens.
*****> Determina o CFOP
**        if wg_direitos-cfop is initial.
**          select single *
**            from marc
**            into wl_marc
**             where matnr eq wl_itens-matnr
**               and werks eq p_branch.
**
**          if sy-subrc is initial.
**            wl_itens-steuc = wl_marc-steuc.
**            select single *
**              from mbew
**              into wl_mbew
**               where matnr eq wl_itens-matnr
**                 and bwkey eq wl_itens-werks.
**
**            if sy-subrc is initial.
**              select single *
**                from j_1bbranch
**                 into wl_1bbranch
**                 where bukrs  eq p_bukrs
**                   and branch eq p_branch.
**
**              if sy-subrc is initial.
**                select single *
**                  from j_1baa
**                  into wl_1baa
**                   where nftype eq wg_fiscal-nftype.
**
**                if wl_1baa-entrad eq c_x.
**                  wl_direct = c_1.
**                else.
**                  wl_direct = c_2.
**                endif.
**
**                if wg_direitos-indcoper eq c_d.
**                  wl_dstcat = c_0.
**
**                else.
**                  wl_dstcat = c_1.
**
**                endif.
**
**                select single *
**                  from j_1bapn
**                  into wl_1bapn
**                   where direct eq wl_direct
**                     and dstcat eq wl_dstcat
**                     and indus3 eq wl_marc-indus
**                     and itmtyp eq wg_fiscal-itmtyp
**                     and ownpro eq wl_mbew-ownpr
**                     and matuse eq wl_mbew-mtuse
**                     and indus1 eq wl_1bbranch-industry.
**
**                if sy-subrc is initial.
**                  wl_itens-cfop = wl_1bapn-cfop.
**                else.
**                  clear: wl_itens-cfop.
**                endif.
**              else.
**                clear: wl_itens-cfop.
**              endif.
**            else.
**              clear: wl_itens-cfop.
**            endif.
**          else.
**            clear: wl_itens-cfop, wl_itens-steuc.
**          endif.
**        else.
**          select single *
**            from marc
**            into wl_marc
**             where matnr eq wl_itens-matnr
**               and werks eq p_branch.
**
**          wl_itens-steuc = wl_marc-steuc.
**          wl_itens-cfop = wg_direitos-cfop.
**        endif.
**
**        wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
**        modify tg_itens from wl_itens index ls_good-row_id.
**
**      endif.
**    endloop.
**
***** Método de atualização de dados na Tela
**    call method grid1->refresh_table_display
**      exporting
**        is_stable = wa_stable.
**
**    perform verifica_erros.
  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD ON_ONF4.


    TYPES: BEGIN OF TYL_MARA,
             MATNR TYPE MARA-MATNR,
             MATKL TYPE SETLEAF-VALFROM,
             MTART TYPE MTART,
           END OF TYL_MARA,

           BEGIN OF TYL_FIELD,
             TABNAME   TYPE DD03L-TABNAME,    "Nome da tabela
             FIELDNAME TYPE DD03L-FIELDNAME,    "Nome de campo
             s(1)      TYPE C,
           END OF TYL_FIELD,

           BEGIN OF TYL_VALUE,
             TABNAME    TYPE DD03L-TABNAME,    "Nome da tabela
             FIELDNAME  TYPE DD03L-FIELDNAME,    "Nome de campo
             CHAR79(79) TYPE C,
           END OF TYL_VALUE.

    DATA: BEGIN OF WL_MATNR,
            MATNR        TYPE MARA-MATNR,
            MAKTX        TYPE MAKT-MAKTX,
            MEINS        TYPE ZSDT0036-MEINS,
            WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
            INCO1        TYPE ZSDT0036-INCO1,
            SAFRA        TYPE ZSDT0036-SAFRA,
          END OF WL_MATNR,

          BEGIN OF WL_MATNR_AUX,
            FIELD(50),
          END OF WL_MATNR_AUX,

          BEGIN OF WL_0036,
            VAL_DE       TYPE ZSDT0036-VAL_DE,
            VAL_ATE      TYPE ZSDT0036-VAL_ATE,
            DTVENC       TYPE ZSDT0036-DTVENC,
            MATNR        TYPE ZSDT0036-MATNR,
            WAERK        TYPE ZSDT0036-WAERK,
            INCO1        TYPE ZSDT0036-INCO1,
            SAFRA        TYPE ZSDT0036-SAFRA,
            CULTURA      TYPE ZSDT0036-CULTURA,
            MEINS        TYPE ZSDT0036-MEINS,
            WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
            VLR_CUSTO    TYPE ZSDT0036-VLR_CUSTO,
            PERC_MARGEM  TYPE ZSDT0036-PERC_MARGEM,
            VLR_MARGEM   TYPE ZSDT0036-VLR_MARGEM,
            VLR_VENDA    TYPE ZSDT0036-VLR_VENDA,
            LOEKZ        TYPE ZSDT0036-LOEKZ,
            BUKRS        TYPE ZSDT0036-BUKRS,
          END OF WL_0036.

* RANGES: rg_safra for zsdt0036-safra.

    DATA: TL_MATNR      LIKE TABLE OF WL_MATNR,
          TL_MATNR_AUX  LIKE TABLE OF WL_MATNR_AUX,
          TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL,
          WL_RETURN_TAB TYPE DDSHRETVAL,
          TL_DSELC      TYPE TABLE OF DSELC,
          TL_0036       LIKE TABLE OF WL_0036,              "zsdt0036,
*          wl_0036        TYPE zsdt0036,
          TL_MAKT       TYPE TABLE OF MAKT,
          WL_MAKT       TYPE MAKT,
          WL_ITENS      LIKE LINE OF TG_ITENS,
          TL_SETLEAF    TYPE TABLE OF SETLEAF,
          WL_SETLEAF    TYPE SETLEAF,
          TL_SETLINET   TYPE TABLE OF SETLINET,
          WL_SETLINET   TYPE SETLINET,
          TL_MARA       TYPE TABLE OF TYL_MARA,
          WL_MARA       TYPE TYL_MARA,
          TL_FIELD      TYPE TABLE OF TYL_FIELD,
          WL_FIELD      TYPE TYL_FIELD,
          TL_VALUE      TYPE TABLE OF TYL_VALUE,
          WL_VALUE      TYPE TYL_VALUE,
          WL_CHAR(20),
          WL_INDEX      TYPE SY-TABIX,
          TL_DYNPFIELDS TYPE TABLE OF DYNPREAD,
          WL_DYNPFIELDS TYPE DYNPREAD.

    DATA: RGL_MATNR   TYPE RANGE OF MARA-MATNR,
          WR_MATNR    LIKE LINE OF RGL_MATNR,
          RG_CULTURA  TYPE RANGE OF ZSDT0036-CULTURA,
          WR_CULTURA  LIKE LINE OF RG_CULTURA,
          WL_ZSDT0087 TYPE ZSDT0087,
**** CS2022000324
          R_WERKS     TYPE RANGE OF WERKS_D,
          R_USER_LIB  TYPE RANGE OF USR01-BNAME,
          IT_VALUES   TYPE TABLE OF RGSB4,
          WA_VALUES   TYPE RGSB4,
          WA_WERKS    LIKE LINE OF R_WERKS.
**** CS2022000324

*    RANGES: rg_safra for zsdt0036-safra.

    REFRESH: TL_MATNR, RGL_MATNR, TL_MAKT, TL_0036, TL_MARA, TL_SETLINET, TL_SETLEAF, TL_VALUE, TL_FIELD, TL_MATNR_AUX, TL_DYNPFIELDS,
             RG_CULTURA, IT_VALUES.
    CLEAR:   WL_MATNR, RGL_MATNR, WR_MATNR, WL_MAKT, WL_ITENS, WL_0036, WL_RETURN_TAB,
             WL_SETLEAF, WL_SETLINET, WL_MARA, WL_FIELD, WL_VALUE, WL_MATNR_AUX, WL_DYNPFIELDS, WG_HEADER-VLRTOT.

**** CS2022000324
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        SETNR         = 'MV45AFZZ_WERKS'
        TABLE         = 'VBAP'
        CLASS         = '0000'
        FIELDNAME     = 'WERKS'
      TABLES
        SET_VALUES    = IT_VALUES
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF SY-SUBRC IS INITIAL.

      LOOP AT IT_VALUES INTO WA_VALUES.
        WA_WERKS = 'IEQ'.
        WA_WERKS-LOW    = WA_VALUES-FROM.
        IF WA_VALUES-TO IS NOT INITIAL.
          WA_WERKS = 'IBT'.
          WA_WERKS-HIGH = WA_VALUES-TO.
        ENDIF.

        APPEND WA_WERKS TO R_WERKS.
      ENDLOOP.
    ENDIF.
**** CS2022000324


    IF WG_HEADER-VKBUR IS INITIAL.
      MESSAGE 'Escritório de Venda não Informado!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF WG_HEADER-CULTURA IS NOT INITIAL.
      WR_CULTURA-SIGN    = 'I'.
      WR_CULTURA-OPTION  = 'EQ'.
      WR_CULTURA-LOW     = WG_HEADER-CULTURA.

      APPEND WR_CULTURA TO RG_CULTURA.
      CLEAR: WR_CULTURA.
    ENDIF.
    WR_CULTURA-SIGN    = 'I'.
    WR_CULTURA-OPTION  = 'EQ'.
    WR_CULTURA-LOW     =  SPACE.

    APPEND WR_CULTURA TO RG_CULTURA.
    CLEAR: WR_CULTURA.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
    SELECT *
      FROM ZSDT0384
      WHERE VKBUR = @WG_HEADER-VKBUR
      INTO TABLE @DATA(LT_ZSDT0384).

    IF SY-SUBRC = 0.
    ENDIF.

    "FF #180476 - inicio
*    call function 'G_SET_GET_ALL_VALUES'
*      exporting
*        setnr         = 'Z_MAT_SIMULADOR'
*        class         = '0000'
*      tables
*        set_values    = it_values
*      exceptions
*        set_not_found = 1
*        others        = 2.
*    if sy-subrc is initial.
*      r_user_lib = value #( for ls_value in it_values
*                            ( sign = 'I'
*                              option = 'EQ'
*                              low = ls_value-to ) ).
*    endif.

    SELECT FROM ZSDT0394 FIELDS USUARIO
      INTO TABLE @DATA(LT_394).

    IF SY-SUBRC = 0.
      R_USER_LIB = VALUE #( FOR ROW IN LT_394 ( SIGN  = 'I'
                                                OPTION = 'EQ'
                                                LOW = ROW-USUARIO ) ).
    ELSE.
      CLEAR R_USER_LIB.
    ENDIF.
    "FF #180476 - fim

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

    "FF #174195 - inicio
    IF SY-UNAME IN R_USER_LIB.
      "FF #174195 - fim
      SELECT VAL_DE VAL_ATE DTVENC MATNR WAERK INCO1 SAFRA CULTURA MEINS
             WERKS_FORNEC VLR_CUSTO PERC_MARGEM VLR_MARGEM VLR_VENDA LOEKZ BUKRS
        FROM ZSDT0036
        INTO TABLE TL_0036
          WHERE "val_de  LE sy-datum
            "AND val_ate GE sy-datum
                LOEKZ     EQ SPACE
            AND WAERK     EQ WG_HEADER-WAERK
*          AND cultura EQ wg_header-cultura
            AND CULTURA   IN RG_CULTURA
            AND SAFRA     EQ WG_HEADER-SAFRA
            AND ELIMINADO EQ SPACE
            AND BUKRS     EQ WA_T001K-BUKRS.
*          AND safra   in rg_safra.
*          and werks   in rgl_werks.

      "FF #174195 - inicio
    ELSE.

      IF LT_ZSDT0384[] IS NOT INITIAL.
        SELECT A~VAL_DE,
               A~VAL_ATE,
               A~DTVENC,
               A~MATNR,
               A~WAERK,
               A~INCO1,
               A~SAFRA,
               A~CULTURA,
               A~MEINS,
               A~WERKS_FORNEC,
               A~VLR_CUSTO,
               A~PERC_MARGEM,
               A~VLR_MARGEM,
               A~VLR_VENDA,
               A~LOEKZ,
               A~BUKRS
          FROM ZSDT0036 AS A
          INNER JOIN MARA AS B ON A~MATNR = B~MATNR
          WHERE A~LOEKZ     = @SPACE
            AND A~WAERK     = @WG_HEADER-WAERK
            AND A~CULTURA   IN @RG_CULTURA
            AND A~SAFRA     = @WG_HEADER-SAFRA
            AND A~ELIMINADO = @SPACE
            AND A~BUKRS     = @WA_T001K-BUKRS
            AND EXISTS (
            SELECT 1
              FROM @LT_ZSDT0384 AS LT_384
             WHERE LT_384~MATKL = B~MATKL
               AND LT_384~WERKS = A~WERKS_FORNEC )
         INTO TABLE @TL_0036.
      ENDIF.
      "FF #174195 - fim

    ENDIF.

    LOOP AT TL_0036 INTO WL_0036.
      IF WL_0036-CULTURA IS NOT INITIAL
      AND WL_0036-CULTURA NE WG_HEADER-CULTURA.
        DELETE TABLE TL_0036 FROM WL_0036.
      ENDIF.

      IF  WL_0036-VAL_DE  LE SY-DATUM
     AND  WL_0036-VAL_ATE GE SY-DATUM.
      ELSE.
        DELETE TABLE TL_0036 FROM WL_0036.
      ENDIF.
    ENDLOOP.

    SORT: TL_0036 BY  MATNR WERKS_FORNEC INCO1 WAERK.
    DELETE ADJACENT DUPLICATES FROM TL_0036 COMPARING MATNR WERKS_FORNEC INCO1 WAERK.

    IF TL_0036[] IS NOT INITIAL.
      SELECT *
        FROM MAKT
        INTO TABLE TL_MAKT
         FOR ALL ENTRIES IN TL_0036
          WHERE MATNR EQ TL_0036-MATNR
            AND SPRAS EQ SY-LANGU.

      SELECT MATNR MATKL MTART
        FROM MARA
        INTO TABLE TL_MARA
         FOR ALL ENTRIES IN TL_0036
         WHERE MATNR EQ TL_0036-MATNR.

    ENDIF.

    SORT: TL_MAKT BY MATNR.

    LOOP AT TL_0036 INTO WL_0036.


* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
      READ TABLE TL_MARA INTO WL_MARA
          WITH KEY MATNR = WL_0036-MATNR.

      READ TABLE TL_MAKT INTO WL_MAKT
        WITH KEY MATNR = WL_MARA-MATNR
                 BINARY SEARCH.
      "FF #174195 - inicio
*
*      IF sy-uname NOT IN r_user_lib.
*
*        READ TABLE lt_zsdt0384 TRANSPORTING NO FIELDS
*             WITH KEY matkl = wl_mara-matkl.
*        IF sy-subrc <> 0.
*          EXIT.
*        ENDIF.
*
*      ENDIF.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
      "FF #174195 - fim

      MOVE-CORRESPONDING: WL_MARA TO WL_MATNR.
      MOVE:  WL_MAKT-MAKTX        TO WL_MATNR-MAKTX,
             WL_0036-MEINS        TO WL_MATNR-MEINS,
             WL_0036-SAFRA        TO WL_MATNR-SAFRA,
             WL_0036-WERKS_FORNEC TO WL_MATNR-WERKS_FORNEC,
             WL_0036-INCO1        TO WL_MATNR-INCO1.

      APPEND WL_MATNR TO TL_MATNR.

      MOVE: WL_MATNR-MATNR TO WL_MATNR_AUX-FIELD.
      APPEND WL_MATNR_AUX  TO TL_MATNR_AUX.

      MOVE: WL_MAKT-MAKTX TO WL_MATNR_AUX-FIELD.
      APPEND WL_MATNR_AUX TO TL_MATNR_AUX.

      MOVE: WL_MATNR-MEINS TO WL_MATNR_AUX-FIELD.
      APPEND WL_MATNR_AUX TO TL_MATNR_AUX.

      MOVE: WL_0036-WERKS_FORNEC TO WL_MATNR_AUX-FIELD.
      APPEND WL_MATNR_AUX TO TL_MATNR_AUX.

      MOVE: WL_0036-INCO1 TO WL_MATNR_AUX-FIELD.
      APPEND WL_MATNR_AUX TO TL_MATNR_AUX.

      CLEAR: WL_MATNR, WL_MAKT, WL_MATNR_AUX.
    ENDLOOP.

    WL_FIELD-TABNAME = 'MARA'.
    WL_FIELD-FIELDNAME = 'MATNR'.
    WL_FIELD-S = 'X'.
    APPEND WL_FIELD TO TL_FIELD.


    WL_FIELD-TABNAME = 'MAKT'.
    WL_FIELD-FIELDNAME = 'MAKTX'.
    WL_FIELD-S = ' '.
    APPEND WL_FIELD TO TL_FIELD.

    WL_FIELD-TABNAME = 'ZSDT0036'.
    WL_FIELD-FIELDNAME = 'MEINS'.
    WL_FIELD-S = ' '.
    APPEND WL_FIELD TO TL_FIELD.

    WL_FIELD-TABNAME = 'ZSDT0036'.
    WL_FIELD-FIELDNAME = 'WERKS_FORNEC'.
    WL_FIELD-S = ' '.
    APPEND WL_FIELD TO TL_FIELD.

    WL_FIELD-TABNAME = 'ZSDT0036'.
    WL_FIELD-FIELDNAME = 'INCO1'.
    WL_FIELD-S = ' '.
    APPEND WL_FIELD TO TL_FIELD.

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
        CUCOL                     = 3
        CUROW                     = 15
        FIELDNAME                 = 'MATNR'
        TABNAME                   = 'ZSDT0041'
      IMPORTING
        INDEX                     = WL_INDEX
        SELECT_VALUE              = WL_CHAR
      TABLES
        FIELDS                    = TL_FIELD
        SELECT_VALUES             = TL_VALUE
        VALUETAB                  = TL_MATNR_AUX
      EXCEPTIONS
        FIELD_NOT_IN_DDIC         = 001
        MORE_THEN_ONE_SELECTFIELD = 002
        NO_SELECTFIELD            = 003.



    IF SY-SUBRC IS INITIAL.
      READ TABLE TL_MATNR INTO WL_MATNR INDEX WL_INDEX.
      IF ES_ROW_NO-ROW_ID GT 0.
        READ TABLE TG_ITENS INTO WL_ITENS INDEX ES_ROW_NO-ROW_ID.

        READ TABLE TL_MAKT INTO WL_MAKT
          WITH KEY MATNR = WL_MATNR-MATNR.

        READ TABLE TL_MARA INTO WL_MARA
          WITH KEY MATNR = WL_MATNR-MATNR.

*        MOVE WL_ITENS-ZWERT TO WL_ITENS-VL_UNIT.

        CLEAR WL_ZSDT0087.
        CLEAR: WL_ITENS-SPART, WL_ITENS-AUART.
        SELECT SINGLE *
          FROM ZSDT0087
          INTO WL_ZSDT0087
          WHERE MATKL = WL_MARA-MATKL
          AND   TPSIM = WG_HEADER-TPSIM
          AND   INCO1 = WL_MATNR-INCO1.
        IF SY-SUBRC = 0.
          MOVE: WL_ZSDT0087-SPART TO WL_ITENS-SPART,
                WL_ZSDT0087-AUART TO WL_ITENS-AUART.

          CASE WL_ITENS-AUART.
            WHEN 'ZFTE' OR 'ZFUT' OR 'YFTE'. "165455 - PQ 24.02.25
              MOVE: WG_HEADER-SAFRA TO WL_ITENS-CHARG.
            WHEN OTHERS.
              MOVE: '' TO WL_ITENS-CHARG.
          ENDCASE.

*          IF WL_ITENS-AUART EQ 'ZFTE'
*          OR WL_ITENS-AUART EQ 'ZFUT'.
*            MOVE: WG_HEADER-SAFRA TO WL_ITENS-CHARG.
*          ENDIF.

        ENDIF.

**** Clear no campo Charg CS2016001142>>>>>
        CASE WL_ITENS-AUART.
          WHEN 'ZFTE' OR 'ZOFE' OR 'YFTE'. "165455 - PQ 24.02.25
*            IF wl_matnr-werks_fornec EQ '0175' AND wl_mara-mtart EQ 'ZFER'.
            IF WL_MATNR-WERKS_FORNEC IN R_WERKS AND WL_MARA-MTART EQ 'ZFER'.
              CLEAR: WL_ITENS-CHARG.
            ENDIF.
        ENDCASE.
**** Clear no campo Charg CS2016001142<<<<<

**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>
        CASE WL_ITENS-AUART.
          WHEN 'ZFTE' OR 'ZOFE' OR 'ZFUT' OR 'YFTE'. "165455 - PQ 24.02.25
            IF WL_ITENS-SPART EQ '03'.
              CLEAR: WL_ITENS-CHARG.
            ENDIF.
        ENDCASE.
**** Clear no campo Charg US109832-CS2023000286 - SC>>>>>


        READ TABLE TL_0036 INTO WL_0036
          WITH KEY MATNR        = WL_MATNR-MATNR
                   WERKS_FORNEC = WL_MATNR-WERKS_FORNEC
                   INCO1        = WL_MATNR-INCO1
                   MEINS        = WL_MATNR-MEINS
                   SAFRA        = WL_MATNR-SAFRA.

        MOVE: WL_MATNR-MATNR         TO WL_ITENS-MATNR,
              WL_MAKT-MAKTX          TO WL_ITENS-MAKTX,
              WL_MATNR-WERKS_FORNEC  TO WL_ITENS-WERKS,
              WL_MATNR-INCO1         TO WL_ITENS-INCO1,
              WL_0036-DTVENC         TO WL_ITENS-DTVENC,
              WL_0036-MEINS          TO WL_ITENS-ZIEME.

        SELECT SINGLE VLR_FRETE
            FROM ZSDT0037
            INTO (WL_ITENS-VLR_FRETE)
           WHERE VAL_DE          LE SY-DATUM
             AND VAL_ATE         GE SY-DATUM
             AND MEINS           EQ WL_ITENS-ZIEME
             AND FILIAL_ORIGEM   EQ WL_ITENS-WERKS
             AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
             AND MATKL           EQ WL_MARA-MATKL
             AND WAERS           EQ 'BRL'
             AND BUKRS           EQ WA_T001K-BUKRS.

        CASE WL_MATNR-INCO1.
          WHEN 'CIF' OR 'CPT' OR 'CFR'.
*            PERFORM ATUALIZA_FRETE CHANGING WL_ITENS-VLR_FRETE.
          WHEN OTHERS.
            WL_ITENS-VLR_FRETE = 0.
        ENDCASE.

        MODIFY TG_ITENS FROM WL_ITENS INDEX ES_ROW_NO-ROW_ID.
      ENDIF.
    ENDIF.

    PERFORM CALCULA_ITENS.

*    BREAK ABAP.
    IF WG_HEADER-TPSIM(1) EQ C_TRO(1).
      MOVE: 'WG_HEADER-TROTOTSC'        TO WL_DYNPFIELDS-FIELDNAME,
            WG_HEADER-TROTOTSC          TO WL_DYNPFIELDS-FIELDVALUE.
      APPEND WL_DYNPFIELDS TO TL_DYNPFIELDS.

      MOVE: 'WG_HEADER-SCHA'        TO  WL_DYNPFIELDS-FIELDNAME,
            WG_HEADER-SCHA          TO  WL_DYNPFIELDS-FIELDVALUE.
      APPEND WL_DYNPFIELDS TO TL_DYNPFIELDS.

    ELSEIF WG_HEADER-TPSIM(1) EQ C_ADT(1).
      MOVE: 'WG_HEADER-VLRTOT'        TO WL_DYNPFIELDS-FIELDNAME,
            WG_HEADER-VLRTOT          TO WL_DYNPFIELDS-FIELDVALUE.
      APPEND WL_DYNPFIELDS TO TL_DYNPFIELDS.

      MOVE: 'WG_HEADER-COMPRSC'        TO  WL_DYNPFIELDS-FIELDNAME,
            WG_HEADER-COMPRSC          TO  WL_DYNPFIELDS-FIELDVALUE.
      APPEND WL_DYNPFIELDS TO TL_DYNPFIELDS.
    ELSE.
      MOVE: 'WG_HEADER-VLRTOT'        TO  WL_DYNPFIELDS-FIELDNAME,
            WG_HEADER-VLRTOT          TO  WL_DYNPFIELDS-FIELDVALUE.
      APPEND WL_DYNPFIELDS TO TL_DYNPFIELDS.
    ENDIF.

    PERFORM VLRTOT.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME     = SY-REPID
        DYNUMB     = SY-DYNNR
      TABLES
        DYNPFIELDS = TL_DYNPFIELDS.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        FUNCTIONCODE           = '/00'
      EXCEPTIONS
        FUNCTION_NOT_SUPPORTED = 1.

  ENDMETHOD.                                                "on_ONF4
  METHOD ON_HOTSPOT_CLICK.

    DATA: WL_ITENS LIKE LINE OF TG_ITENS.
    DATA: WL_ANTEC LIKE LINE OF TG_ANTEC.
    DATA: TL_TEXTO   TYPE CATSXT_LONGTEXT_ITAB,
          WL_TEXTO   TYPE LINE OF CATSXT_LONGTEXT_ITAB,
          IT_ZIB_ERR TYPE TABLE OF ZIB_CONTABIL_ERR.

    IF E_ROW_ID GT 0.
      IF SY-DYNNR = '0108'.
        READ TABLE TG_ANTEC INTO WL_ANTEC INDEX E_ROW_ID.
        IF E_COLUMN_ID = 'ADIANT'.
          IF WL_ANTEC-ADIANT = ICON_OPERATION.

          ELSEIF WL_ANTEC-ADIANT = ICON_MESSAGE_ERROR_SMALL.
            SELECT *
               FROM ZIB_CONTABIL_ERR
               INTO TABLE IT_ZIB_ERR
               WHERE OBJ_KEY EQ WL_ANTEC-OBJ_KEY.

            LOOP AT IT_ZIB_ERR INTO DATA(WG_ZIB_ERR)  WHERE OBJ_KEY EQ WL_ANTEC-OBJ_KEY.

              WL_TEXTO = WG_ZIB_ERR-MESSAGE.

              APPEND WL_TEXTO TO TL_TEXTO.
              CLEAR: WL_TEXTO.
            ENDLOOP.
            IF TL_TEXTO[] IS NOT INITIAL.
              CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
                EXPORTING
                  IM_TITLE        = 'Erros'
                  IM_DISPLAY_MODE = C_X
                CHANGING
                  CH_TEXT         = TL_TEXTO.
            ENDIF.
            "
          ELSEIF WL_ANTEC-ADIANT IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WL_ANTEC-ADIANT.
            SET PARAMETER ID 'GJR' FIELD WL_ANTEC-GJAHR.
            SET PARAMETER ID 'BUK' FIELD WL_ANTEC-BUKRS.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ELSEIF E_COLUMN_ID = 'AUGBL' AND WL_ANTEC-ADIANT IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WL_ANTEC-AUGBL.
            SET PARAMETER ID 'GJR' FIELD WL_ANTEC-AUGDT(4).
            SET PARAMETER ID 'BUK' FIELD WL_ANTEC-BUKRS.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE TG_ITENS INTO WL_ITENS INDEX E_ROW_ID.

        IF WL_ITENS-VBELN IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD WL_ITENS-VBELN.
          CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                                                "on_hotsot

  METHOD ON_HOTSPOT_CLICK_TRA.
    DATA: WL_TRANS LIKE LINE OF TG_TRANS.
    IF E_ROW_ID GT 0.
      READ TABLE TG_TRANS INTO WL_TRANS INDEX E_ROW_ID.

      IF E_COLUMN_ID =  'VBELN' AND WL_TRANS-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD WL_TRANS-VBELN.
        CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.
      ELSEIF E_COLUMN_ID =  'VBELV' AND WL_TRANS-VBELV IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD WL_TRANS-VBELV.
        CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                                                "on_hotsot
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: BEGIN OF TL_UCOMM OCCURS 0,
          UCOMM TYPE  SY-UCOMM,
        END OF TL_UCOMM,

        WL_0040   TYPE ZSDT0040,
        WL_0041   TYPE ZSDT0041,
        VAR_VBELN TYPE C,
        USUARIO   TYPE SY-MSGV1.

  DATA: WL_0043  TYPE ZSDT0043,
        WL_ITENS LIKE LINE OF TG_ITENS.

  REFRESH:TL_UCOMM.
  CLEAR: TL_UCOMM, WL_0040, WL_0043, WL_0041, VAR_VBELN.

  LOOP AT TG_ITENS WHERE NOT VBELN IS INITIAL.
    MOVE ABAP_TRUE TO VAR_VBELN.
  ENDLOOP.

  PERFORM CARREGA_ANTEC USING TG_ANTEC[] WG_HEADER-DOC_SIMULACAO.
  DELETE TG_ANTEC WHERE ESTORNO = 'X'.
  DELETE TG_ANTEC WHERE AUGBL IS NOT INITIAL.
  IF TG_ANTEC[] IS INITIAL.
    APPEND VALUE #( UCOMM = 'BOLETO' ) TO TL_UCOMM[].
  ENDIF.

  APPEND VALUE #( UCOMM = C_PRINT ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_APROV ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_REPROV ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_BLOQ ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_GERA ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_PRINTC ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_SACAS ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_DSC_ABS ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_VINC_DESC ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_RECALC ) TO TL_UCOMM[].

  APPEND VALUE #( UCOMM = C_ALTCSA ) TO TL_UCOMM[].


*Início - Sara Oikawa - 38859 - Agosto/2020
  APPEND VALUE #( UCOMM = C_ALTJUR ) TO TL_UCOMM[].
  APPEND VALUE #( UCOMM = C_ALTADT ) TO TL_UCOMM[].
*Fim - Sara Oikawa - 38859 - Agosto/2020

  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
  APPEND VALUE #( UCOMM = C_BOLETA_VIN ) TO TL_UCOMM[].
  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*  APPEND VALUE #( ucomm = c_altcsa ) TO tl_ucomm[]. "//CS2021000715 #62000 Exibir para todos os Usuarios.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  APPEND VALUE #( UCOMM = C_ALTCPG ) TO TL_UCOMM[].
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  IF LINE_EXISTS( TG_TRANS[
                            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO
                            CATEGORIA = 'O'
                            FLAG = ABAP_FALSE
                            ESTORNO = ABAP_FALSE
                          ] ).
    DELETE TL_UCOMM WHERE UCOMM EQ C_VINC_DESC.
  ENDIF.

  IF WG_STATUS EQ ICON_RELEASE OR
     WG_STATUS EQ ICON_GIS_PAN.
    APPEND VALUE #( UCOMM = C_MODIF ) TO TL_UCOMM[].
  ENDIF.

  CLEAR C_ERRO.
  PERFORM F_VAL_SACAS_USER CHANGING C_ERRO.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'SAVE' OR 'SEARCH'.
    WHEN OTHERS.
*    WHEN 'ADD' OR 'MODIF' OR 'COPY' OR 'ATUAL' OR 'PRINT' OR 'SHOW_MSGRE' OR
*         'APROV' OR 'REPROV' OR 'BLOQ' OR 'GERA' OR 'PRINTC' OR 'BOLETO' OR
*         'QTD_SACA' OR 'VINC_DESC' OR 'SHOW_LOG' OR 'RECALC'.

      IF NOT WG_HEADER-DOC_SIMULACAO IS INITIAL AND S_ACAO IS INITIAL.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

        CALL FUNCTION 'ENQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO  = WG_HEADER-DOC_SIMULACAO
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.

        IF SY-SUBRC <> 0.

          USUARIO = SY-MSGV1.

          MOVE: C_MODIF TO TL_UCOMM.
          APPEND TL_UCOMM.
          CLEAR: TL_UCOMM.

          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
            W_FIELDCATALOG-EDIT = ABAP_FALSE.
            MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
          ENDLOOP.

          CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
            EXPORTING
              IT_FIELDCATALOG = T_FIELDCATALOG.

          CALL METHOD CL_GUI_CFW=>DISPATCH.

          "FF #174195 - inicio
          IF WG_HEADER-TPSIM = 'AD'. "if "Cond. pgto" = Adiantamento (AD).
            DELETE TL_UCOMM WHERE UCOMM = 'BOLETO'. "Mostrar a opção "Gerar Boleto"
          ENDIF.
          "FF #174195 - fim

          SET PF-STATUS 'Z001' EXCLUDING TL_UCOMM.
          SET TITLEBAR 'Z001'.

          CLEAR: TL_UCOMM.
          MESSAGE S836(SD) WITH |Doc. Simulação { WG_HEADER-DOC_SIMULACAO } Bloqueado por { USUARIO }!| .
          EXIT.
        ENDIF.
      ENDIF.

  ENDCASE.


  DATA(COUNT) = REDUCE INT4( INIT Y = 0 FOR LS2 IN TG_ITENS WHERE ( VBELN IS NOT INITIAL ) NEXT Y = Y + 1 ).

  IF COUNT IS INITIAL.
    IF VTA_SISTPRO NE VLR_TOTAL AND WG_STATUS NE ICON_GIS_PAN. " // ICON_GIS_PAN equivalente a Bloqueado
      IF WG_ACAO NE 'MODIF' AND WG_ACAO NE 'ADD'.
        CASE SY-UCOMM.
          WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'SAVE' OR 'SEARCH'.
          WHEN OTHERS.
* Início - Sara Oikawa - 38859 - Agosto/2020
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
            IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV' "FF #174195
            OR WG_HEADER-TPSIM = 'TT'.                      "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
* Fim - Sara Oikawa - 38859 - Agosto/2020
              DELETE TL_UCOMM WHERE UCOMM EQ C_RECALC.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.

  IF WG_STATUS NE ICON_GIS_PAN.

    IF WG_ACAO NE C_ADD
    AND WG_ACAO NE C_MODIF.

      MOVE: C_SAVE TO TL_UCOMM.
      APPEND TL_UCOMM.
      CLEAR: TL_UCOMM.

      IF C_ERRO IS INITIAL.
        CASE WG_SAVE.
          WHEN C_SACAS OR C_DSC_ABS.
            DELETE TL_UCOMM WHERE UCOMM EQ C_SAVE.
*Início - Sara Oikawa - 38859 - Agosto/2020
          WHEN C_ALTJUR.
            DELETE TL_UCOMM WHERE UCOMM EQ C_SAVE.

          WHEN C_ALTADT.
            DELETE TL_UCOMM WHERE UCOMM EQ C_SAVE.
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
* Inicio - wsb CS2021000715
*          WHEN c_altcsa.
*            DELETE tl_ucomm WHERE ucomm EQ c_save.
*            IF grid1 IS NOT INITIAL.
*
*              LOOP AT t_fieldcatalog INTO w_fieldcatalog
*                 WHERE fieldname EQ 'CULTURA_APL'
*                    OR fieldname EQ 'SAFRA_APL'.
*                w_fieldcatalog-edit = c_x.
*                MODIFY t_fieldcatalog FROM w_fieldcatalog.
*              ENDLOOP.
*
*              CALL METHOD grid1->set_frontend_fieldcatalog
*                EXPORTING
*                  it_fieldcatalog = t_fieldcatalog.
*            ENDIF.
* Fim - wsb CS2021000715
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
          WHEN C_ALTCPG.
            DELETE TL_UCOMM WHERE UCOMM EQ C_SAVE.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

        ENDCASE.
      ENDIF.

      CASE WG_SAVE.
        WHEN C_ALTCSA.
          DELETE TL_UCOMM WHERE UCOMM EQ C_SAVE.
          IF GRID1 IS NOT INITIAL.

            LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
               WHERE FIELDNAME EQ 'CULTURA_APL'
                  OR FIELDNAME EQ 'SAFRA_APL'.
              W_FIELDCATALOG-EDIT = C_X.
              MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
            ENDLOOP.

            CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
              EXPORTING
                IT_FIELDCATALOG = T_FIELDCATALOG.
          ENDIF.
      ENDCASE.

      IF WG_ACAO EQ C_ATUAL.

        IF C_ERRO IS INITIAL.
          CASE WG_HEADER-TPSIM.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
            WHEN 'TS' OR 'TV' "FF #174195"
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
            OR 'TT'. "FF #174195"

              DELETE TL_UCOMM WHERE UCOMM EQ C_SACAS.
*Início - Sara Oikawa - 38859 - Agosto/2020
            WHEN 'AD'.
              DELETE TL_UCOMM WHERE UCOMM EQ C_ALTJUR.
              DELETE TL_UCOMM WHERE UCOMM EQ C_ALTADT.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*            WHEN 'PM' OR 'VF' OR 'VP' OR 'VV' .
            WHEN 'PM' OR 'VF'.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
              DELETE TL_UCOMM WHERE UCOMM EQ C_ALTJUR.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
            WHEN 'VP' OR 'VV' .
              DELETE TL_UCOMM WHERE UCOMM EQ C_ALTJUR.
              DELETE TL_UCOMM WHERE UCOMM EQ C_ALTCPG.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
          ENDCASE.

        ENDIF.

        SELECT SINGLE *
          FROM ZSDT0040
          INTO WL_0040
           WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

        IF SY-SUBRC IS INITIAL.

          DELETE TL_UCOMM WHERE UCOMM EQ C_PRINT.

          CASE WL_0040-STATUS.
            WHEN C_A.

              SELECT SINGLE *
               FROM ZSDT0041
                INTO WL_0041
                WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
                AND VBELN         EQ SPACE.

              IF SY-SUBRC IS INITIAL.
                DELETE TL_UCOMM WHERE UCOMM EQ C_GERA.
              ELSE.

                IF C_ERRO IS INITIAL.
                  DELETE TL_UCOMM WHERE UCOMM EQ C_DSC_ABS.
                ENDIF.

                DELETE TL_UCOMM WHERE UCOMM EQ C_PRINTC.
              ENDIF.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
* Inicio - wsb CS2021000715
              DELETE TL_UCOMM WHERE UCOMM EQ C_ALTCSA.
* Fim - wsb CS2021000715
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

*Início - Sara Oikawa - 38859 - Agosto/2020
            WHEN OTHERS.
              APPEND VALUE #( UCOMM = C_ALTJUR ) TO TL_UCOMM[].

              APPEND VALUE #( UCOMM = C_ALTADT ) TO TL_UCOMM[].
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
              APPEND VALUE #( UCOMM = C_ALTCPG ) TO TL_UCOMM[].
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

          ENDCASE.

          IF SY-UCOMM NE 'SAVE'.

            CLEAR: WL_0043.
            SELECT SINGLE *
              FROM ZSDT0043
              INTO WL_0043
               WHERE WERKS EQ WL_0040-VKBUR
                 AND UNAME EQ SY-UNAME.

            IF SY-SUBRC IS INITIAL.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
              IF WG_HEADER-ERNAM IS NOT INITIAL.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077

                IF WL_0040-STATUS NE C_A AND WG_HEADER-ERNAM NE SY-UNAME.

                  DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
                ENDIF.

*          Se não tem nenhum VBELN na Solicitação o Botão C_BLOQ e o C_REPROV é ATIVADO.
                IF VAR_VBELN IS INITIAL.
                  DELETE TL_UCOMM WHERE UCOMM EQ C_BLOQ.
                  DELETE TL_UCOMM WHERE UCOMM EQ C_REPROV.

*            IF WL_0040-STATUS EQ C_A.
*
*            ENDIF.

                ENDIF.

                " 05.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
                DELETE TL_UCOMM WHERE UCOMM EQ C_BOLETA_VIN.
                " 05.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
              ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077



            ENDIF.
          ENDIF.

          IF GRID1 IS NOT INITIAL.

            LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
              IF W_FIELDCATALOG-FIELDNAME EQ 'CULTURA_APL' OR
                 W_FIELDCATALOG-FIELDNAME EQ 'SAFRA_APL'.
                W_FIELDCATALOG-EDIT = C_X.
              ELSE.
                W_FIELDCATALOG-EDIT = SPACE.
                MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
              ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
            ENDLOOP.

            CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
              EXPORTING
                IT_FIELDCATALOG = T_FIELDCATALOG.
          ENDIF.

        ENDIF.

*        BREAK WBARBOSA.

        SELECT SINGLE A~*
            FROM ZSDT0040 AS A
          INTO @DATA(W_0040)
          WHERE A~DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.

        IF SY-SUBRC IS INITIAL.

          SELECT SINGLE *
            FROM ZSDT0043
            INTO @DATA(W_0043)
             WHERE WERKS EQ @WG_HEADER-VKBUR
               AND UNAME EQ @SY-UNAME.

*          IF WG_HEADER-ERNAM IS INITIAL AND WG_HEADER-ERDAT IS NOT INITIAL .
*            DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
*          ELSE.

          IF SY-SUBRC = 0.
            IF W_0040-ERNAM EQ SY-UNAME.

              " 21.02.2023 - RAMON - 102323 -->
              IF W_0040-ECOMMERCE = 'X'.
                DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
              ENDIF.
              " 21.02.2023 - RAMON - 102323 --<

            ELSE.
              DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
            ENDIF.
          ENDIF.
*          ENDIF.

          IF W_0040-STATUS EQ C_A.
            APPEND VALUE #( UCOMM = C_APROV ) TO TL_UCOMM[].
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF WG_ACAO EQ C_ADD
        OR WG_ACAO EQ C_MODIF.

      MOVE: C_ATUAL TO TL_UCOMM.

      APPEND TL_UCOMM.
      CLEAR: TL_UCOMM.

      IF GRID1 IS NOT INITIAL.

        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
           WHERE FIELDNAME EQ 'MATNR'
              OR FIELDNAME EQ 'ZMENG'
              OR FIELDNAME EQ 'DTVENC'
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
              OR FIELDNAME EQ 'CULTURA_APL'
              OR FIELDNAME EQ 'SAFRA_APL'
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
               OR FIELDNAME  = 'ZWERT'.                     "FF #174195
          W_FIELDCATALOG-EDIT = C_X.
          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
        ENDLOOP.


        CLEAR: WL_0043.
        SELECT SINGLE *
          FROM ZSDT0043
          INTO WL_0043
           WHERE WERKS EQ WG_HEADER-VKBUR
             AND UNAME EQ SY-UNAME.

        IF SY-SUBRC IS INITIAL.

          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
           WHERE FIELDNAME EQ 'DESCONTO'
              OR FIELDNAME EQ 'ZWERT'.

            W_FIELDCATALOG-EDIT = C_X.
            MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.

          ENDLOOP.

          LOOP AT SCREEN.
            IF SCREEN-GROUP2 EQ 'A3'.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

        ELSEIF SY-SUBRC IS NOT INITIAL
           AND WG_ACAO EQ C_MODIF.

          REFRESH: STYLE, TG_ITENS-STYLE.

        ENDIF.

        CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = T_FIELDCATALOG.
      ENDIF.
    ENDIF.
  ELSE.
    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      W_FIELDCATALOG-EDIT = ABAP_FALSE.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.

  ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  IF WG_ACAO EQ C_MODIF AND
     WG_SAVE EQ C_ALTCPG.
    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      W_FIELDCATALOG-EDIT = ABAP_FALSE.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.
  ENDIF.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


  IF WG_ACAO EQ C_MODIF.

    MOVE: C_ADD TO TL_UCOMM.
    APPEND TL_UCOMM.
    CLEAR: TL_UCOMM.

    MOVE: C_COPY TO TL_UCOMM.
    APPEND TL_UCOMM.
    CLEAR: TL_UCOMM.

  ENDIF.

*  BREAK WBARBOSA.
*  IF WG_ACAO IS NOT INITIAL.
*    IF WG_HEADER-ERNAM IS INITIAL.
*      DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
*    ELSE.
*      SELECT SINGLE A~*
*        FROM ZSDT0040 AS A
*        INNER JOIN ZSDT0043 AS B ON B~WERKS EQ A~VKBUR
*                                AND B~UNAME EQ A~ERNAM
*        INTO @DATA(W_0040)
*         WHERE A~DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.
*
*      IF W_0040-ERNAM EQ SY-UNAME.
*        DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

  " 21.02.2023 - RAMON - 102323 -->
  IF WG_HEADER-ECOMMERCE = 'X'.
    APPEND VALUE #( UCOMM = 'BOLETO' ) TO TL_UCOMM[].
  ENDIF.
  " 21.02.2023 - RAMON - 102323 --<

  "FF #174195 - inicio
  IF WG_HEADER-TPSIM = 'AD' AND WL_0040-STATUS = C_A. "if "Cond. pgto" = Adiantamento (AD).
    DELETE TL_UCOMM WHERE UCOMM = 'BOLETO'. "Mostrar a opção "Gerar Boleto"
  ENDIF.
  "FF #174195 - fim

  IF SY-UNAME EQ 'WBARBOSA'.
    DELETE TL_UCOMM WHERE UCOMM EQ C_APROV.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET PF-STATUS 'Z001' EXCLUDING TL_UCOMM.
  SET TITLEBAR 'Z001'.

  CLEAR: TL_UCOMM.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA MTART TYPE MTART.

  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
  DATA LV_ERRO.
  " 03.01.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL <--

*Início - Sara Oikawa - 38859 - Agosto/2020
  DATA LV_RESP.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  DATA: LV_MSG TYPE CHAR255.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

**** CS2022000324
  DATA: R_WERKS   TYPE RANGE OF WERKS_D,
        IT_VALUES TYPE TABLE OF RGSB4,
        WA_VALUES TYPE RGSB4,
        WA_WERKS  LIKE LINE OF R_WERKS.

  REFRESH IT_VALUES.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      SETNR         = 'MV45AFZZ_WERKS'
      TABLE         = 'VBAP'
      CLASS         = '0000'
      FIELDNAME     = 'WERKS'
    TABLES
      SET_VALUES    = IT_VALUES
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  IF SY-SUBRC IS INITIAL.

    LOOP AT IT_VALUES INTO WA_VALUES.
      WA_WERKS = 'IEQ'.
      WA_WERKS-LOW    = WA_VALUES-FROM.
      IF WA_VALUES-TO IS NOT INITIAL.
        WA_WERKS = 'IBT'.
        WA_WERKS-HIGH = WA_VALUES-TO.
      ENDIF.

      APPEND WA_WERKS TO R_WERKS.
    ENDLOOP.
  ENDIF.
**** CS2022000324

  CASE SY-UCOMM.
    WHEN C_ADD.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_ADD.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Criar Simulação'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E87.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      IF WG_FLAG IS INITIAL.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

        PERFORM LIMPA_VARIAVEL USING SY-UCOMM.

        " 21.02.2023 - RAMON - 102323 -->
        PERFORM F_INIT_ADD.
        " 21.02.2023 - RAMON - 102323 --<

*        PERFORM GET_NEXT_NUMBER  USING  'ZSIMULACAO'
*                                        '1'
*                               CHANGING WG_HEADER-DOC_SIMULACAO.

        PERFORM GET_NEXT_NUMBER  USING 'ZNR_SOL_OV' "--'ZSIMULACAO'
                               '01' CHANGING WG_HEADER-DOC_SIMULACAO.


        MOVE: SY-UNAME TO WG_HEADER-ERNAM,
              SY-DATUM TO WG_HEADER-ERDAT,
              SY-UZEIT TO WG_HEADER-ERZET.
      ENDIF.
      MOVE C_ADD TO WG_ACAO.

    WHEN C_ATUAL.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_ATUAL.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Buscar Simulação'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E91.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      MOVE: C_ATUAL TO WG_ACAO.

    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.

* Início - Sara Oikawa - 38859 - Agosto/2020
      IF WG_SAVE EQ C_ALTJUR.
        SY-UCOMM = C_ALTJUR.
      ENDIF.

      IF WG_SAVE EQ C_ALTADT.
        SY-UCOMM = C_ALTADT.
      ENDIF.
* Fim - Sara Oikawa - 38859 - Agosto/2020

    WHEN C_COPY.
      WG_COPY = ABAP_TRUE.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_COPY.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Copiar Simulação'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E89.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7


**    Valida se existe documento para ser modificado.
      SELECT SINGLE DOC_SIMULACAO ECOMMERCE
        FROM ZSDT0040
        INTO (WG_HEADER-DOC_SIMULACAO,WG_HEADER-ECOMMERCE)
         WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

      IF SY-SUBRC IS INITIAL.

        " 21.02.2023 - RAMON - 102323 -->
        IF WG_HEADER-ECOMMERCE = 'X'.

          PERFORM F_CHECK_USER_ECOMM CHANGING GV_INFO.

          IF GV_INFO IS INITIAL.
            MESSAGE S836(SD) DISPLAY LIKE 'E'
              WITH 'Usuário sem permissão para'
                   'Copiar Venda com origem E-commerce'.
            EXIT.
          ENDIF.

        ENDIF.
        " 21.02.2023 - RAMON - 102323 --<

        PERFORM LIMPA_VARIAVEL USING C_ATUAL.
        PERFORM BUSCA_DADOS_DOC.
        PERFORM BUSCA_DADOS.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

        PERFORM GET_NEXT_NUMBER  USING 'ZNR_SOL_OV'
                               '01' CHANGING WG_HEADER-DOC_SIMULACAO.

*        PERFORM GET_NEXT_NUMBER  USING 'ZSIMULACAO'
*                                       '1' CHANGING WG_HEADER-DOC_SIMULACAO.

        MOVE: SY-UNAME TO WG_HEADER-ERNAM,
              SY-DATUM TO WG_HEADER-ERDAT,
              SY-UZEIT TO WG_HEADER-ERZET.

        PERFORM ZF_DETERMINE_FUNRURAL USING ABAP_TRUE. "Forçar determinar Funrural

        LOOP AT TG_ITENS.
          CLEAR: TG_ITENS-VBELN, TG_ITENS-STYLE, TG_ITENS-DESCONTO, TG_ITENS-VLR_AJUSTE,
                 TG_ITENS-VL_UNIT, TG_ITENS-ZWERT, TG_ITENS-DESC_ABSOLUTO, TG_ITENS-VLRTOT, TG_ITENS-VLR_FRETE,
                 WG_HEADER-DT_ENTREGA_SEM, WG_HEADER-DT_ENTREGA_DEF, WG_HEADER-DT_ENTREGA_FET.

          SELECT SINGLE MTART
            FROM MARA
            INTO MTART
            WHERE MATNR EQ TG_ITENS-MATNR.

**** Clear no campo Charg CS2016001142>>>>>
          CASE TG_ITENS-AUART.
            WHEN 'ZFTE' OR 'ZOFE' OR 'YFTE'. "165455 - PQ 24.02.25
*              IF tg_itens-werks EQ '0175' AND mtart EQ 'ZFER'.
              IF TG_ITENS-WERKS IN R_WERKS AND MTART EQ 'ZFER'.
                CLEAR TG_ITENS-CHARG.
              ENDIF.
          ENDCASE.
**** Clear no campo Charg CS2016001142<<<<<

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          IF TG_ITENS-CULTURA_APL IS INITIAL.
            IF WG_CULTURA_APL IS NOT INITIAL.
              TG_ITENS-CULTURA_APL = WG_CULTURA_APL.
            ENDIF.
          ENDIF.

          IF TG_ITENS-SAFRA_APL IS INITIAL.
            TG_ITENS-SAFRA_APL = WG_HEADER-SAFRA.
          ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

          MODIFY TG_ITENS.
        ENDLOOP.

        MOVE: ICON_INITIAL TO WG_STATUS.
        MOVE C_MODIF TO WG_ACAO.

      ENDIF.

      " 06.02.2023 - TENTATIVA NAO DAR ERRO AO COPIAR -->
***      IF grid1 IS BOUND.
***
***        PERFORM calcula_itens.
***        PERFORM vlrtot.
***
***        CALL METHOD grid1->check_changed_data.
***        CALL METHOD grid1->refresh_table_display.
***
***      ENDIF.
      " 06.02.2023 - TENTATIVA NAO DAR ERRO AO COPIAR -->

    WHEN C_DESCP.
      PERFORM INPUTA_DESC.

    WHEN C_MODIF.

**    Valida se existe documento para ser modificado.
      SELECT SINGLE DOC_SIMULACAO ECOMMERCE
        FROM ZSDT0040
        INTO (WG_HEADER-DOC_SIMULACAO,WG_HEADER-ECOMMERCE)
         WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
           AND ( STATUS NE C_A AND STATUS NE C_B ).

      IF SY-SUBRC IS INITIAL.

        " 21.02.2023 - RAMON - 102323 -->
        IF WG_HEADER-ECOMMERCE = 'X'.

          PERFORM F_CHECK_USER_ECOMM CHANGING GV_INFO.

          IF GV_INFO IS INITIAL.
            MESSAGE S836(SD) DISPLAY LIKE 'E'
              WITH 'Usuário sem permissão para'
                   'Editar Venda com origem E-commerce'.
            EXIT.
          ENDIF.

        ENDIF.
        " 21.02.2023 - RAMON - 102323 --<

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

        CALL FUNCTION 'ENQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO  = WG_HEADER-DOC_SIMULACAO
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        PERFORM LIMPA_VARIAVEL USING C_ATUAL.
        PERFORM BUSCA_DADOS_DOC.
        PERFORM BUSCA_DADOS.

        MOVE: ICON_INITIAL TO WG_STATUS.
        MOVE C_MODIF TO WG_ACAO.
      ELSE.
        MESSAGE S836(SD)
        DISPLAY LIKE 'E'
        WITH |Simulador esta com Status de { COND #( WHEN C_B EQ 'B' THEN 'Bloqueado' ELSE 'Aprovado' ) }|.
      ENDIF.

    WHEN C_TPORD.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      IF WG_SAVE EQ C_ALTCPG.
*        IF WG_HEADER-TPSIM NE 'TS' AND WG_HEADER-TPSIM  NE 'TV' AND WG_HEADER-TPSIM  NE 'AD'.
*          MESSAGE S836(SD)
*          DISPLAY LIKE 'E'
*          WITH 'Alteração de Condição de Pagamento não Permitida.'.
*          WG_HEADER-TPSIM = WG_TPSIM_ANT.
**          MOVE C_ATUAL TO WG_ACAO.
**          clear sy-ucomm.
*          EXIT.
*        ENDIF.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
        IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM  EQ 'TV' OR WG_HEADER-TPSIM  EQ 'AD' "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
          OR WG_HEADER-TPSIM = 'TT' OR WG_HEADER-TPSIM  = 'AD'. "FF #174195
          MOVE C_MODIF TO WG_ACAO.
        ENDIF.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.

    WHEN C_CANCEL.
      CALL FUNCTION 'DEQUEUE_EZSDT0040'
        EXPORTING
          DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.

    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN    = '100'
          I_SHOW      = 'X'
          I_REPID     = SY-REPID
          I_POPUP     = 0
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          I_SET_FIELD = 'X_FIELD'
          I_SET_CELL  = 'WG_CELL'
          I_SET_OBJ   = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM  = WG_MENSAGEM
        TABLES
          IT_MSGS     = TG_MSG_RET.

    WHEN C_SAVE.
      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM VERIFICA_ERROS.


* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
      " Quando se clicar no Botão Alterar Cultura/Safra de aplicação,
      " O sistema deve deixar Gravar, mesmo se houver outras mensagens de erro
      " (Obs.: exceto onde a mensagem de erro é referente a Cultura/Safra)
      IF WG_SAVE EQ C_ALTCSA.
        DELETE TG_MSG_RET WHERE ( MSG NS TEXT-E67 AND  MSG NS TEXT-E68 AND  MSG NS TEXT-E69 ).
      ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

      IF TG_MSG_RET[] IS INITIAL.

*Início - Sara Oikawa - 38859 - Agosto/2020
        IF WG_SAVE EQ 'ALTADT'.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TEXT_QUESTION         = TEXT-I03  "Confirma Alterações de Adiantamento?'
              TEXT_BUTTON_1         = 'Sim'
              TEXT_BUTTON_2         = 'Não'
              DISPLAY_CANCEL_BUTTON = ' '
            IMPORTING
              ANSWER                = LV_RESP.

          IF LV_RESP EQ 1.
            PERFORM GRAVA_DADOS.
          ENDIF.

        ELSE.
*Fim - Sara Oikawa - 38859 - Agosto/2020

          PERFORM GRAVA_DADOS.
        ENDIF.

      ELSE.

        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.

      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN    = '100'
          I_SHOW      = 'X'
          I_REPID     = SY-REPID
          I_POPUP     = 0
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          I_SET_FIELD = 'X_FIELD'
          I_SET_CELL  = 'WG_CELL'
          I_SET_OBJ   = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM  = WG_MENSAGEM
        TABLES
          IT_MSGS     = TG_MSG_RET.

    WHEN 'INFOR'.
      PERFORM MONTA_PROPOSTA.
      CALL SCREEN 102
*        ENDING AT 51 9
        STARTING AT 3 3.

    WHEN 'PRINT'.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE 'PRINT'.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Imprimir Simulação'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E92.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM LIMPA_VARIAVEL USING C_ATUAL.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      PERFORM IMPRIME_SIMULACAO USING WG_HEADER-DOC_SIMULACAO.
      WG_ACAO = C_ATUAL.

    WHEN 'PRINTC'.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE 'PRINTC'.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Gerar Contrato'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E96.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM IMPRIMI_CONTRATO.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          FUNCTIONCODE           = '/00'
        EXCEPTIONS
          FUNCTION_NOT_SUPPORTED = 1.

    WHEN 'BACK'
      OR 'EXIT'.
      CALL FUNCTION 'DEQUEUE_EZSDT0040'
        EXPORTING
          DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.
      LEAVE TO SCREEN 0.

    WHEN C_APROV.

      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.

      PERFORM VERIFICA_ERROS.

      IF TG_MSG_RET[] IS INITIAL.

        "PERFORM salva_imposto.
        PERFORM MODIFICA_STATUS USING 'A'.

        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
        IF LV_ERRO IS INITIAL.
          PERFORM SALVA_IMPOSTO.
        ENDIF.
        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

      ELSE.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN    = '100'
            I_SHOW      = 'X'
            I_REPID     = SY-REPID
            I_POPUP     = 0
            I_SET_FIELD = 'X_FIELD'
          IMPORTING
            E_MESSAGEM  = WG_MENSAGEM
          TABLES
            IT_MSGS     = TG_MSG_RET.
      ENDIF.

    WHEN C_REPROV.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_REPROV.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Reprovar'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E93.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM MODIFICA_STATUS USING 'R'.

    WHEN C_MGLOB.
      PERFORM MONTA_MGLOBAL.
      PERFORM MOTA_LAYOUT_MGLOBAL.
      PERFORM EXIBE_MGBLOBAL.

    WHEN C_MEMO.
      PERFORM MONTA_MEMO.
      CALL SCREEN 101 ENDING AT 51 13 STARTING AT 3 3.

    WHEN 'PG_ANTE'.
      PERFORM CARREGA_ANTEC USING TG_ANTEC[] WG_HEADER-DOC_SIMULACAO.
      CALL SCREEN 108. " ENDING AT 165 10 STARTING AT 3 3.

    WHEN 'BOLETO'.
      DATA: WL_BOLETO TYPE J_1BDOCNUM.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE 'BOLETO'.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Gerar Boleto'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E97.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7


*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      DATA: WL_INSTRUCOES TYPE ZFI_BOLETO,
            VL_JUROS_ANO  TYPE ZSDT0040-JUROS_ANO,
            VL_VALOR      TYPE C LENGTH 7.

      SELECT SINGLE JUROS_ANO
        FROM ZSDT0040
        INTO VL_JUROS_ANO
         WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

      IF SY-SUBRC EQ 0.
        WL_INSTRUCOES-TXT_INSTRUCAO1 = 'Sujeito a protesto.'.

        VL_VALOR = VL_JUROS_ANO.
        CONCATENATE 'Após a data de vencimento será cobrado ' VL_VALOR ' % de Juros a.a'
                INTO WL_INSTRUCOES-TXT_INSTRUCAO2 SEPARATED BY SPACE.
      ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      MOVE WG_HEADER-DOC_SIMULACAO TO WL_BOLETO.
      CALL FUNCTION 'Z_SD_PRINT_BOLETO'
        EXPORTING
          DOC_NUMERO     = WL_BOLETO
          TIPO           = 'A'  "Antecipação Pagamento
          HBKID          = WG_HEADER-HBKID "US 81799 - CBRAND  Devemos descomentar antes de publicar a US 81799 em PRD
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          INSTRUCOES     = WL_INSTRUCOES
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        EXCEPTIONS
          NAO_LOCALIZADO = 1
          OTHERS         = 2.

    WHEN C_BLOQ.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_BLOQ.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Bloquear'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E94.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM MODIFICA_STATUS USING 'B'.

    WHEN C_GERA.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_BLOQ.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Gerar OV'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E95.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7

      PERFORM LOG_ACAO USING 'G' '' CHANGING LV_ERRO.

      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
      CHECK LV_ERRO IS INITIAL.
      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

      PERFORM GERA_ORDEM_VENDA.

    WHEN C_SACAS.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 7
      IF WG_SAVE IS NOT INITIAL AND
          WG_SAVE NE C_SACAS.
        " Por favor, concluir a ação anterior antes de prosseguir com 'Ajustar Sacas'.
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E98.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 7


* Início - Sara Oikawa - 38859 - Agosto/2020
*      MOVE: c_sacas TO wg_save.
      " Verifica se existe OV gerada
      DATA(COUNT_OV) = REDUCE INT4( INIT Y = 0 FOR LS2 IN TG_ITENS WHERE ( VBELN IS NOT INITIAL ) NEXT Y = Y + 1 ).

      IF COUNT_OV IS NOT INITIAL.
        " Erro : Ação não é permitida. Simulador já possui OV !
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E71.
        CLEAR SY-UCOMM.
      ELSE.
        MOVE: C_SACAS TO WG_SAVE.
      ENDIF.
* Fim - Sara Oikawa - 38859 - Agosto/2020

    WHEN C_DSC_ABS.
      MOVE: C_DSC_ABS TO WG_SAVE.
      IT_DESABS = TG_ITENS.

*      WSB
    WHEN C_COL_EXP.
      IF WG_COLAPS EQ '@K1@'.
        WG_COLAPS = '@K2@'.
        DYNNR_TOP = '0105'.
      ELSE.
        WG_COLAPS = '@K1@'.
        DYNNR_TOP = '0106'.
      ENDIF.
    WHEN C_VINC_DESC.
      PERFORM VINC_DESC.
      PERFORM LOG_ACAO USING 'V' '' CHANGING LV_ERRO.

      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
      CHECK LV_ERRO IS INITIAL.
      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<


      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      MOVE: C_ATUAL TO WG_ACAO.

    WHEN C_SHOW_LOG.
      PERFORM SHOW_LOG_ACAO.
    WHEN C_RECALC.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
*      CLEAR WG_SAVE.
      CLEAR WG_SAVE.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
      BTN_FI = ABAP_FALSE.
      PERFORM VERIFICA_ERROS.
      PERFORM RECALCULO.

*Início - Sara Oikawa - 38859 - Agosto/2020
    WHEN C_ALTJUR.

      PERFORM F_VERIFICA_RECEBIMENTOS.

      IF WG_FLG_RECEB IS INITIAL.

        IF WG_SAVE IS NOT INITIAL AND
           WG_SAVE NE C_ALTJUR.
          " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Juros"
          MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E55.
          SY-UCOMM = WG_SAVE.
          EXIT.
        ENDIF.

        MOVE C_ALTJUR TO WG_SAVE.

      ENDIF.

    WHEN C_ALTADT.

      PERFORM F_VERIFICA_RECEBIMENTOS.

      IF WG_FLG_RECEB IS INITIAL.

        IF WG_SAVE IS NOT INITIAL AND
           WG_SAVE NE C_ALTADT.
          " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Adiantamento"
          MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E56.
          SY-UCOMM = WG_SAVE.
          EXIT.
        ENDIF.

        MOVE C_ALTADT TO WG_SAVE.

      ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
    WHEN C_ALTCSA.

      IF WG_SAVE IS NOT INITIAL AND
         WG_SAVE NE C_ALTCSA.
        " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Cultura/Safra"
        MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E70.
        SY-UCOMM = WG_SAVE.
        EXIT.
      ENDIF.

      MOVE C_ALTCSA TO WG_SAVE.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    WHEN C_ALTCPG.

      PERFORM F_VERIFICA_RECEBIMENTOS.

      IF WG_FLG_RECEB IS INITIAL.

        IF WG_SAVE IS NOT INITIAL AND
           WG_SAVE NE C_ALTCPG.
          " Por favor, concluir a ação anterior antes de prosseguir com "Alterar Cond.Pgto"
          MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E53 TEXT-E54 TEXT-E90.
          SY-UCOMM = WG_SAVE.
          EXIT.
        ENDIF.

        CONCATENATE TEXT-I06 TEXT-I07 INTO LV_MSG.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TEXT_QUESTION         = LV_MSG  "
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DISPLAY_CANCEL_BUTTON = ' '
          IMPORTING
            ANSWER                = LV_RESP.

        IF LV_RESP EQ 1.
          SY-UCOMM = C_ALTCPG.
          WG_SAVE  = C_ALTCPG.
          WG_TPSIM_ANT = WG_HEADER-TPSIM.
        ELSE.
          CLEAR SY-UCOMM.
        ENDIF.


      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
      " Dados Compra SIGAM
    WHEN 'BTN_SG'.
      CALL SCREEN 0110 STARTING AT 4 3.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    WHEN C_BOLETA_VIN.
      PERFORM F_CALL_BOLETA_REPORT.
      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: WL_REPID    TYPE SY-REPID,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE,
        LT_F4       TYPE LVC_T_F4 WITH HEADER LINE.


  WL_REPID = SY-REPID.
  PERFORM VERIFICA_ERROS.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN    = '100'
      I_SHOW      = SPACE
      I_REPID     = SY-REPID
      I_POPUP     = 0
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      I_SET_FIELD = 'X_FIELD'
    IMPORTING
      E_MESSAGEM  = WG_MENSAGEM
    TABLES
      IT_MSGS     = TG_MSG_RET.

  IF CONTAINER1 IS INITIAL.
*    INICIO WSB
    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*    FIM WSB

    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
*    WA_STABLE-ROW        = C_X.

    CREATE OBJECT CONTAINER1
      EXPORTING
        CONTAINER_NAME = 'CC_01'.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER1.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

    REFRESH TL_FUNCTION.

    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR FOR GRID1,
                 OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

    LT_F4-FIELDNAME = 'MATNR'.
    LT_F4-REGISTER = 'X' .
    LT_F4-GETBEFORE = 'X' .
    APPEND LT_F4 .

    PERFORM MONTAR_LAYOUT.

    PERFORM BUILD_DROPDOWN.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
*       i_save               = 'X'
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_ITENS[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

*    SET HANDLER: LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID1.
    SET HANDLER: LCL_EVENT_HANDLER=>ON_HOTSPOT_CLICK         FOR GRID1,
                 LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
                 LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR GRID1,
                 LCL_EVENT_HANDLER=>ON_ONF4                  FOR GRID1.

  ELSE.
    PERFORM MONTAR_LAYOUT.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.

** Valida se usuário tem permissão de visuzlizar simulação
  IF WG_HEADER-VKBUR IS NOT INITIAL.
    PERFORM F_VALIDAR_ESC_VENDA USING WG_HEADER-VKBUR
                                CHANGING SY-SUBRC.
    IF SY-SUBRC IS INITIAL.
      SELECT *
         FROM ZSDT0090
         INTO TABLE TG_TRANS
         WHERE DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO
         ORDER BY SEQUENCIA.

      LOOP AT TG_TRANS ASSIGNING FIELD-SYMBOL(<W0090>)
        WHERE ESTORNO EQ ABAP_TRUE.
        <W0090>-COLOR = 'C600'.
      ENDLOOP.

    ENDIF.
  ENDIF.

  "Transferencias
  IF G_CUSTOM_CONTAINER IS INITIAL.
    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
    WA_LAYOUT-NO_TOOLBAR = C_X.
    WA_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.
    WA_LAYOUT-INFO_FNAME = 'COLOR'.

*    WA_STABLE-ROW        = C_X.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_1.

    CREATE OBJECT GRID2
      EXPORTING
        I_PARENT = CONTAINER_1.


    PERFORM MONTAR_LAYOUT_TRA.

*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = GRID1.

**      * Register event handler
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       I_STRUCTURE_NAME     = 'ZSDT0090'\
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG2[]
        IT_OUTTAB            = TG_TRANS[].

    SET HANDLER:
            LCL_EVENT_HANDLER=>ON_HOTSPOT_CLICK_TRA FOR GRID2.

*    posiciona spliter na altura x
    IF TG_TRANS[] IS NOT INITIAL.
      CALL METHOD SPLITTER->SET_ROW_HEIGHT
        EXPORTING
          ID     = 1
          HEIGHT = 100.
    ELSE.
      CALL METHOD SPLITTER->SET_ROW_HEIGHT
        EXPORTING
          ID     = 1
          HEIGHT = 0.
    ENDIF.
  ELSE.
*    posiciona spliter na altura x
    IF TG_TRANS[] IS NOT INITIAL.
      CALL METHOD SPLITTER->SET_ROW_HEIGHT
        EXPORTING
          ID     = 1
          HEIGHT = 100.
    ELSE.
      CALL METHOD SPLITTER->SET_ROW_HEIGHT
        EXPORTING
          ID     = 1
          HEIGHT = 0.
    ENDIF.
    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_INFOR .
  REFRESH T_FIELDCAT_INFOR.
  PERFORM MONTAR_ESTRUTURA USING:
*       1   'ZSDT0041'  'SPART'       'TG_INFOR'  'SPART'       ' '                   '06'   ' ' ' '   ' '   ' '  ' '  ' ',
        2   'TSPAT'     'VTEXT'       'TG_INFOR'  'VTEXT'       'Descrição'           '25'   ' ' ' '   ' '   ' '  ' '  ' ',
        3   ' '         ' '           'TG_INFOR'  'VLRTOT'      'Vlr.Total'           '16'   ' ' ' '   ' '   ' '  ' '  ' '.
ENDFORM.                    "MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*        1   'ZSDT0041'  'POSNR'         'TG_ITENS'  'POSNR'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
*        2   'ZSDT0041'  'AUART'         'TG_ITENS'  'AUART'         'TP O.V.'                 '05' ' '  ' '   ' '   ' '  ' '  ' ',
*        3   'VBAK'      'VBELN'         'TG_ITENS'  'VBELN'         'Nº O.V'                  '10' ' '  ' '   ' '   ' '  ' '  'X',
*        4   'ZSDT0041'  'SPART'         'TG_ITENS'  'SPART'         ' '                       '02' ' '  ' '   ' '   ' '  ' '  ' ',
*        5   ' '         ' '             'TG_ITENS'  'MATNR'         'Material'                '07' ' '  'X'   ' '   ' '  'X'  ' ',
*        6   'MAKT'      'MAKTX'         'TG_ITENS'  'MAKTX'         ' '                       ' '  ' '  ' '   ' '   ' '  ' '  ' ',
*        7   'ZSDT0041'  'WERKS'         'TG_ITENS'  'WERKS'         'Centro Fornec.'          '04' ' '  ' '   ' '   ' '  ' '  ' ',
**        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  'X'   ' '   ' '  ' '  ' ', *        **** Block campo CHARG CS2016001142>>>>>
*        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  ' '   ' '   ' '  ' '  ' ',
*        9   'ZSDT0041'  'INCO1'         'TG_ITENS'  'INCO1'         ' '                       '06' ' '  ' '   ' '   ' '  ' '  ' ',
*        10  'ZSDT0041'  'ZMENG'         'TG_ITENS'  'ZMENG'         ' '                       ' '  'X'  'X'   ' '   ' '  ' '  ' ',
*        11  'ZSDT0041'  'ZIEME'         'TG_ITENS'  'ZIEME'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
*        12  'ZSDT0041'  'DTVENC'        'TG_ITENS'  'DTVENC'        ' Data Vencimento'        '10' ' '  'X'   ' '   ' '  ' '  ' ',
*        13  'ZSDT0041'  'DESCONTO'      'TG_ITENS'  'DESCONTO'      '% Desconto'              '06' ' '  ' '   ' '   ' '  ' '  ' ',
*        14  ' '         ' '             'TG_ITENS'  'VL_UNIT'       'Vlr.Unit'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        15  'ZSDT0041'  'ZWERT'         'TG_ITENS'  'ZWERT'         'Vlr. Negociado'          ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        16  ' '         ' '             'TG_ITENS'  'CALCU'         'Calculo'                 ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        17  ' '         ' '             'TG_ITENS'  'TRUNIT'        'Troc.Unit.'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        18  ' '         ' '             'TG_ITENS'  'COMPR'         'Compromisso'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        19  'ZSDT0041'  'TRTOT'         'TG_ITENS'  'TRTOT'         'Troc.Total'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        20  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'VLR_AJUSTE'    'Vlr Ajuste'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        21  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'DESC_ABSOLUTO' 'Desc.Abs'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        22  'ZSDT0041'  'VLRTOT'        'TG_ITENS'  'VLRTOT'        'Vlr.Total'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        23  ' '         ' '             'TG_ITENS'  'MGCAD'         '% Marg. Cadastrada'      ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        24  ' '         ' '             'TG_ITENS'  'MGEFE'         '% Marg. Efetiva'         ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        25  'ZSDT0041'  'VLR_ICMS'      'TG_ITENS'  'VLR_ICMS'      'Vlr. ICMS'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        26  'ZSDT0041'  'VLR_COFINS'    'TG_ITENS'  'VLR_COFINS'    'Vlr. COFINS'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        27  'ZSDT0041'  'VLR_PIS'       'TG_ITENS'  'VLR_PIS'       'Vlr. PIS'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
*        28  'ZSDT0041'  'ZWERT_LIQDO'   'TG_ITENS'  'ZWERT_LIQDO'   'Vlr. Neg. Liqdo'         ' '  'X'  ' '   ' '   ' '  ' '  ' '.


        1   'ZSDT0041'  'POSNR'         'TG_ITENS'  'POSNR'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
        2   'ZSDT0041'  'AUART'         'TG_ITENS'  'AUART'         'TP O.V.'                 '05' ' '  ' '   ' '   ' '  ' '  ' ',
        3   'VBAK'      'VBELN'         'TG_ITENS'  'VBELN'         'Nº O.V'                  '10' ' '  ' '   ' '   ' '  ' '  'X',
        4   'ZSDT0041'  'SPART'         'TG_ITENS'  'SPART'         ' '                       '02' ' '  ' '   ' '   ' '  ' '  ' ',
        5   ' '         ' '             'TG_ITENS'  'MATNR'         'Material'                '07' ' '  'X'   ' '   ' '  'X'  ' ',
        6   'MAKT'      'MAKTX'         'TG_ITENS'  'MAKTX'         ' '                       ' '  ' '  ' '   ' '   ' '  ' '  ' ',
        7   'ZSDT0041'  'WERKS'         'TG_ITENS'  'WERKS'         'Centro Fornec.'          '04' ' '  ' '   ' '   ' '  ' '  ' ',
*        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  'X'   ' '   ' '  ' '  ' ', *        **** Block campo CHARG CS2016001142>>>>>
        8   'ZSDT0041'  'CHARG'         'TG_ITENS'  'CHARG'         ' '                       '05' ' '  ' '   ' '   ' '  ' '  ' ',
        9   'ZSDT0041'  'INCO1'         'TG_ITENS'  'INCO1'         ' '                       '06' ' '  ' '   ' '   ' '  ' '  ' ',

        10  'ZSDT0041'  'CULTURA_APL'   'TG_ITENS'  'CULTURA_APL'   ' '                       '02' ' '  ' '   ' '   ' '  ' '  ' ',
        11  'ZSDT0041'  'SAFRA_APL'     'TG_ITENS'  'SAFRA_APL'     ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',

        12  'ZSDT0041'  'ZMENG'         'TG_ITENS'  'ZMENG'         ' '                       ' '  'X'  'X'   ' '   ' '  ' '  ' ',
        13  'ZSDT0041'  'ZIEME'         'TG_ITENS'  'ZIEME'         ' '                       '04' ' '  ' '   ' '   ' '  ' '  ' ',
        14  'ZSDT0041'  'DTVENC'        'TG_ITENS'  'DTVENC'        ' Data Vencimento'        '10' ' '  'X'   ' '   ' '  ' '  ' ',
        15  'ZSDT0041'  'DESCONTO'      'TG_ITENS'  'DESCONTO'      '% Desconto'              '06' ' '  ' '   ' '   ' '  ' '  ' ',
        16  ' '         ' '             'TG_ITENS'  'VL_UNIT'       'Vlr.Unit'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        17  'ZSDT0041'  'ZWERT'         'TG_ITENS'  'ZWERT'         'Vlr. Negociado'          ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        18  ' '         ' '             'TG_ITENS'  'CALCU'         'Calculo'                 ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        19  ' '         ' '             'TG_ITENS'  'TRUNIT'        'Troc.Unit.'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        20  ' '         ' '             'TG_ITENS'  'COMPR'         'Compromisso'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        21  'ZSDT0041'  'TRTOT'         'TG_ITENS'  'TRTOT'         'Troc.Total'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        22  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'VLR_AJUSTE'    'Vlr Ajuste'              ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        23  'ZSDT0041'  'DESC_ABSOLUTO' 'TG_ITENS'  'DESC_ABSOLUTO' 'Desc.Abs'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        24  'ZSDT0041'  'VLRTOT'        'TG_ITENS'  'VLRTOT'        'Vlr.Total'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        25  ' '         ' '             'TG_ITENS'  'MGCAD'         '% Marg. Cadastrada'      ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        26  ' '         ' '             'TG_ITENS'  'MGEFE'         '% Marg. Efetiva'         ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        27  'ZSDT0041'  'VLR_ICMS'      'TG_ITENS'  'VLR_ICMS'      'Vlr. ICMS'               ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        28  'ZSDT0041'  'VLR_COFINS'    'TG_ITENS'  'VLR_COFINS'    'Vlr. COFINS'             ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        29  'ZSDT0041'  'VLR_PIS'       'TG_ITENS'  'VLR_PIS'       'Vlr. PIS'                ' '  'X'  ' '   ' '   ' '  ' '  ' ',
        30  'ZSDT0041'  'ZWERT_LIQDO'   'TG_ITENS'  'ZWERT_LIQDO'   'Vlr. Neg. Liqdo'         ' '  'X'  ' '   ' '   ' '  ' '  ' '.

* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
                        USING SY-UNAME.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_TRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_TRA.

  REFRESH T_FIELDCATALOG2.
  PERFORM MONTAR_ESTRUTURA USING:
        01 'ZSDT0090'  'SEQUENCIA'        'TG_TRANS'   'SEQUENCIA'        '' '' '' '' '' '' '' '',
        02 'ZSDT0090'  'CATEGORIA'        'TG_TRANS'   'CATEGORIA'        'Ação' '' '' '' '' '' '' '',
        03 'ZSDT0090'  'AUART'            'TG_TRANS'   'AUART'            '' '' '' '' '' '' '' '',
        04 'ZSDT0090'  'VBELN'            'TG_TRANS'   'VBELN'            '' '' '' '' '' '' '' 'X',
        05 'ZSDT0090'  'POSNN'            'TG_TRANS'   'POSNN'            '' '' '' '' '' '' '' '',
        06 'ZSDT0090'  'SPART'            'TG_TRANS'   'SPART'            '' '' '' '' '' '' '' '',
        07 'ZSDT0090'  'ZMENG'            'TG_TRANS'   'ZMENG'            '' '' '' '' '' '' '' '',
        08 'ZSDT0090'  'ZIEME'            'TG_TRANS'   'ZIEME'            '' '' '' '' '' '' '' '',
        09 'ZSDT0090'  'NETPR'            'TG_TRANS'   'NETPR'            '' '' '' '' '' '' '' '',
        10 'ZSDT0090'  'KMEIN'            'TG_TRANS'   'KMEIN'            '' '' '' '' '' '' '' '',
        11 'ZSDT0090'  'CHARG'            'TG_TRANS'   'CHARG'            '' '' '' '' '' '' '' '',
        12 'ZSDT0090'  'MATNR'            'TG_TRANS'   'MATNR'            '' '' '' '' '' '' '' '',
        13 'ZSDT0090'  'MATKL'            'TG_TRANS'   'MATKL'            '' '' '' '' '' '' '' '',
        14 'ZSDT0090'  'INCO1'            'TG_TRANS'   'INCO1'            '' '' '' '' '' '' '' '',
        15 'ZSDT0090'  'WERKS'            'TG_TRANS'   'WERKS'            '' '' '' '' '' '' '' '',
        16 'ZSDT0090'  'KUNNR'            'TG_TRANS'   'KUNNR'            '' '' '' '' '' '' '' '',
        17 'ZSDT0090'  'AUARTV'           'TG_TRANS'   'AUARTV'           '' '' '' '' '' '' '' '',
        18 'ZSDT0090'  'VBELV'            'TG_TRANS'   'VBELV'            '' '' '' '' '' '' '' 'X',
        19 'ZSDT0090'  'POSNV'            'TG_TRANS'   'POSNV'            '' '' '' '' '' '' '' '',
        20 'ZSDT0090'  'SPARTV'           'TG_TRANS'   'SPARTV'           '' '' '' '' '' '' '' '',
        21 'ZSDT0090'  'ZMENGV'           'TG_TRANS'   'ZMENGV'           '' '' '' '' '' '' '' '',
        22 'ZSDT0090'  'ZIEMEV'           'TG_TRANS'   'ZIEMEV'           '' '' '' '' '' '' '' '',
        23 'ZSDT0090'  'NETPRV'           'TG_TRANS'   'NETPRV'           '' '' '' '' '' '' '' '',
        24 'ZSDT0090'  'KMEINV'           'TG_TRANS'   'KMEINV'           '' '' '' '' '' '' '' '',
        25 'ZSDT0090'  'CHARGV'           'TG_TRANS'   'CHARGV'           '' '' '' '' '' '' '' '',
        26 'ZSDT0090'  'MATNRV'           'TG_TRANS'   'MATNRV'           '' '' '' '' '' '' '' '',
        27 'ZSDT0090'  'MATKLV'           'TG_TRANS'   'MATKLV'           '' '' '' '' '' '' '' '',
        28 'ZSDT0090'  'INCO1V'           'TG_TRANS'   'INCO1V'           '' '' '' '' '' '' '' '',
        29 'ZSDT0090'  'WERKSV'           'TG_TRANS'   'WERKSV'           '' '' '' '' '' '' '' '',
        30 'ZSDT0090'  'KUNNRV'           'TG_TRANS'   'KUNNRV'           '' '' '' '' '' '' '' '',
        31 'ZSDT0090'  'KURRF'            'TG_TRANS'   'KURRF'            '' '' '' '' '' '' '' '',
        32 'ZSDT0090'  'VALDT'            'TG_TRANS'   'VALDT'            '' '' '' '' '' '' '' '',
        33 'ZSDT0090'  'DESC_ABSOLUTO'    'TG_TRANS'   'DESC_ABSOLUTO'    'Vlr. Desc. Absoluto' '' '' '' '' '' '' '',
*        34 'ZSDT0090'  'DESC_ABSOLUTO_LQ' 'TG_TRANS'   'DESC_ABSOLUTO_LQ' '' '' '' '' '' '' '' '',
        34 'ZSDT0090'  'USNAM'            'TG_TRANS'   'USNAM'            '' '' '' '' '' '' '' '',
        35 'ZSDT0090'  'DATA_ATUAL'       'TG_TRANS'   'DATA_ATUAL'       'Data' '' '' '' '' '' '' '',
        36 'ZSDT0090'  'HORA_ATUAL'       'TG_TRANS'   'HORA_ATUAL'       'Hora' '' '' '' '' '' '' ''.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
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
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_OPT)           TYPE C
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE)
                            VALUE(P_F4)
                            VALUE(P_HOTSPOT).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME        = P_FIELD.
  W_FIELDCATALOG-TABNAME          = P_TABNAME.

  CASE P_FIELD.
    WHEN 'VL_UNIT' OR 'ZWERT' OR 'CALCU'
      OR 'TRUNIT'  OR 'COMPR' OR 'TRTOT'
      OR 'VLRTOT' OR 'MGCAD'  OR 'MGEFE'.
    WHEN OTHERS.
      W_FIELDCATALOG-REF_TABLE        = P_REF_TABNAME.
  ENDCASE.

  W_FIELDCATALOG-REF_FIELD        = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY              = ' '.

  IF WG_DISPLAY IS INITIAL.
    W_FIELDCATALOG-EDIT           = P_EDIT.
  ENDIF.

  W_FIELDCATALOG-DO_SUM           = P_SUM.
  W_FIELDCATALOG-COL_POS          = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL
    AND P_OUTPUTLEN NE 'X'.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.

  IF P_OUTPUTLEN EQ 'X'.
    W_FIELDCATALOG-COL_OPT        = P_OPT.
  ENDIF.

  W_FIELDCATALOG-NO_OUT = COND #( WHEN P_REF_FIELDNAME EQ 'DESC_ABSOLUTO' THEN ABAP_TRUE ELSE ABAP_FALSE ).

  W_FIELDCATALOG-REPTEXT          = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S        = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M        = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L        = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE        = P_EMPHASIZE.
  W_FIELDCATALOG-F4AVAILABL       = P_F4.
  W_FIELDCATALOG-HOTSPOT          = P_HOTSPOT.

  IF P_FIELD EQ 'VLRTOT'
  OR P_FIELD EQ 'ZWERT'
  OR P_FIELD EQ 'CALCU'
  OR P_FIELD EQ 'TRUNIT'
  OR P_FIELD EQ 'COMPR'.
    W_FIELDCATALOG-DATATYPE         = 'CURR'.
  ENDIF.

*  IF P_FIELD EQ 'VLRTOT'
*  OR P_FIELD EQ 'ZWERT'
*  OR P_FIELD EQ 'CALCU'
*  OR P_FIELD EQ 'TRUNIT'
*  OR P_FIELD EQ 'COMPR'.
*    W_FIELDCATALOG-DECIMALS_O = '000002'.
*  ENDIF.

  IF P_FIELD EQ 'AUART'.
    "W_FIELDCATALOG-DRDN_HNDL  = 1.
  ELSEIF P_FIELD EQ 'INCO1'.
    W_FIELDCATALOG-DRDN_HNDL  = 2.
  ENDIF.

  IF P_FIELD EQ 'MATNR'.
    W_FIELDCATALOG-EDIT_MASK  = '==MATN1'.
  ENDIF.

  IF P_REF_TABNAME =  'ZSDT0090'.
    APPEND W_FIELDCATALOG TO T_FIELDCATALOG2.
  ELSEIF P_TABNAME EQ 'TG_INFOR'.
    APPEND W_FIELDCATALOG TO T_FIELDCAT_INFOR.
  ELSE.
    APPEND W_FIELDCATALOG TO T_FIELDCATALOG.
  ENDIF.
ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_VARIAVEL USING P_ACAO.
  DATA: WL_DOC_SIMULACAO TYPE ZSDT0040-DOC_SIMULACAO.

  CLEAR: WL_DOC_SIMULACAO.

  IF P_ACAO EQ C_CANCEL
   OR P_ACAO EQ C_ATUAL.
    WL_DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

    CLEAR:
           WG_HEADER,
           WG_HEADER_OLD,
           WG_0260,
           WG_AREA_HA,
           TG_ITENS,
           WG_ACAO,
           WG_SAVE,
           WG_FLAG,
           X_FIELD,
           TG_MSG_RET,
           WG_CELL,
           WG_OBJ,
           WG_DESC_KUNNR,
*Início - Sara Oikawa - 38859 - Agosto/2020
           WG_DESC_EMISSOR,
           WG_DESC_ORT01,
           WG_DESC_REGIO,
           VTA_SISTPRO,
           VLR_TOTAL,
*Fim - Sara Oikawa - 38859 - Agosto/2020
           WG_DESC_VKBUR,
           WG_DESC_WAERK,
           WG_DESC_CULTURA,
           WG_DESC_TPCULT,
           WG_DESC_VENDEDOR,
           WG_DESC_HBKID,
           WG_TVKOT,
           WG_TVTWT,
           WG_TSPAT,
           STYLE,
           WG_STATUS,
           WG_MEMO,
           TG_MGLOBAL,
           WG_DESC_DAT,
           S_ACAO,
           TG_LOG.

    FREE: TG_ITENS,
          TG_MSG_RET,
          STYLE,
          TG_ITENS-STYLE,
          TG_MGLOBAL,
          TG_TRANS,
          TG_LOG.

    WG_DESC_DAT = 'Dt.Pagamento'.
    WG_HEADER-DOC_SIMULACAO = WL_DOC_SIMULACAO.
  ELSEIF P_ACAO EQ C_ADD.
    CLEAR: WG_HEADER,
           WG_HEADER_OLD,
           WG_0260,
           WG_AREA_HA,
           TG_ITENS,
           WG_ACAO,
           WG_SAVE,
           X_FIELD,
           TG_MSG_RET,
           WG_DESC_KUNNR,
*Início - Sara Oikawa - 38859 - Agosto/2020
           WG_DESC_EMISSOR,
           WG_DESC_ORT01,
           WG_DESC_REGIO,
           VTA_SISTPRO,
           VLR_TOTAL,
*Fim - Sara Oikawa - 38859 - Agosto/2020
           WG_DESC_VKBUR,
           WG_DESC_WAERK,
           WG_DESC_CULTURA,
           WG_DESC_TPCULT,
           WG_DESC_VENDEDOR,
           WG_DESC_HBKID,
           WG_STATUS,
           WG_MEMO,
           TG_MGLOBAL,
           WG_DESC_DAT,
           S_ACAO,
           TG_LOG.

    REFRESH: TG_ITENS,
             TG_MSG_RET,
             STYLE,
             TG_ITENS-STYLE,
             TG_MGLOBAL,
             TG_LOG.

    WG_DESC_DAT = 'Dt.Pagamento'.
    WG_HEADER-ANTEC = 12.
*    IF     WG_HEADER-ANTEC = 0.
*      WG_HEADER-ANTEC = 12.
*    ENDIF.

  ELSEIF P_ACAO EQ C_TPORD.
    CLEAR: TG_ITENS.
*           WG_HEADER-PREC_CULT,
*           WG_HEADER-JUROS_ANO,
*           WG_HEADER-VLR_ADTO,
*           WG_HEADER-ADTO_HA,
*           WG_HEADER-AREA_PENHOR,
*           WG_HEADER-TROTOTSC,
*           WG_HEADER-SCHA,
*           WG_HEADER-VLRTOT,
*           WG_HEADER-COMPRSC.
*           WG_HEADER-DTPGTCULT.

*    REFRESH: TG_ITENS, STYLE, TG_ITENS-STYLE.
    LOOP AT TG_ITENS.
      CLEAR: "tg_itens-zwert, "BUG 53832
*             TG_ITENS-DTVENC,
             "tg_itens-desconto, "BUG 53832
             TG_ITENS-CALCU,
             TG_ITENS-TRUNIT,
             "tg_itens-siumb,    "BUG 53832
             TG_ITENS-TRTOT,
             TG_ITENS-COMPR,
             TG_ITENS-MGCAD,
             TG_ITENS-MGEFE.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
** Na alteração de CondPagto, não VBELN se estiver preenchido
*             TG_ITENS-VLRTOT,
*             TG_ITENS-VBELN.
      "tg_itens-vlrtot.  "BUG 53832
*      IF tg_itens-vbeln IS NOT INITIAL AND wg_save NE c_altcpg.
*        CLEAR tg_itens-vbeln.
*      ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

      MODIFY TG_ITENS.
    ENDLOOP.
    PERFORM CALCULA_ITENS.
  ELSE.
    CLEAR: TG_ITENS, S_ACAO.
    REFRESH: TG_ITENS, TG_ITENS-STYLE.
  ENDIF.


ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  " Bloco de Código abaixo movido para SubScreen 0103 - PBO MODIFY_SCREEN_0103
*  DATA: VALUES       TYPE VRM_VALUES WITH HEADER LINE.
*  FIELD-SYMBOLS: <FS_CAMPO> TYPE ANY.
*  DATA: IT_VBFA TYPE TABLE OF VBFA,
*        WA_VBFA TYPE VBFA,
*        IT_VBAP TYPE TABLE OF VBAP,
*        WA_VBAP TYPE VBAP,
*        IT_VBAK TYPE TABLE OF VBAK,
*        WA_VBAK TYPE VBAK,
*        IT_41 TYPE TABLE OF ZSDT0041,
*        WA_41 TYPE ZSDT0041.
*
*  IF INIT IS INITIAL.
*    SELECT *
*      FROM SETLEAF
*      INTO TABLE TG_SETLEAF_CULT
*       WHERE SETNAME EQ 'MAGGI_ZSDT0044_03'.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT *
*        FROM SETLINET
*        INTO TABLE TG_SETLINET_CULT
*        FOR ALL ENTRIES IN TG_SETLEAF_CULT
*         WHERE SETNAME EQ TG_SETLEAF_CULT-SETNAME
*           AND LANGU   EQ SY-LANGU
*           AND LINEID  EQ TG_SETLEAF_CULT-LINEID.
*    ENDIF.
*    LOOP AT TG_SETLEAF_CULT.
*      READ TABLE TG_SETLINET_CULT
*        WITH KEY SETNAME = TG_SETLEAF_CULT-SETNAME
*                 LINEID  = TG_SETLEAF_CULT-LINEID.
*      IF SY-SUBRC IS INITIAL.
*
*        VALUES-TEXT = TG_SETLINET_CULT-DESCRIPT.
*        VALUES-KEY  = TG_SETLEAF_CULT-VALFROM.
*        APPEND VALUES.
*      ENDIF.
*    ENDLOOP.
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-TPCULT'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    REFRESH: VALUES.
*    CLEAR: VALUES.
*
*    VALUES-TEXT = 'CIF'.
*    VALUES-KEY  = 'CIF'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'FOB'.
*    VALUES-KEY  = 'FOB'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'CPT'.
*    VALUES-KEY  = 'CPT'.
*    APPEND VALUES.
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-FCULT'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*
*    ENDIF.
*
*
*    REFRESH VALUES.
*    VALUES-TEXT = 'Adiantamento'.
*    VALUES-KEY  = 'AD'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Venda A Vista'.
*    VALUES-KEY  = 'VV'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Venda A Prazo'.
*    VALUES-KEY  = 'VP'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Troca Safra'.
*    VALUES-KEY  = 'TS'.
*    APPEND VALUES.
*
*    VALUES-TEXT = 'Troca a Vista'.
*    VALUES-KEY  = 'TV'.
*    APPEND VALUES.
*
*    " Usuários que podem ter acesso as condições de pagamento adicionais
*    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*      EXPORTING
*        CLASS         = '0000'
*        SETNR         = 'MAGGI_ZSDT0044_04'
*      TABLES
*        SET_VALUES    = T_USERMD
*      EXCEPTIONS
*        SET_NOT_FOUND = 1
*        OTHERS        = 2.
*    IF SY-SUBRC <> 0.
**     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    SORT T_USERMD BY FROM.
*    READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.
*    IF SY-SUBRC = 0.
*      VALUES-TEXT = 'Venda Futura'.
*      VALUES-KEY  = 'VF'.
*      APPEND VALUES.
*
*      VALUES-TEXT = 'Bonificação'.
*      VALUES-KEY  = 'BN'.
*      APPEND VALUES.
*    ENDIF.
*
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-TPSIM'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*
*    ENDIF.
*
*    INIT = C_X.
*  ENDIF.
*
*** Adiantamento
*  IF WG_HEADER-TPSIM(1) EQ C_A.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_TRO
*      OR SCREEN-GROUP1 EQ C_VIS.
*        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'
*        OR SCREEN-NAME NE 'WG_HEADER-TPCULT'
*        OR SCREEN-NAME NE 'WG_HEADER-FCULT'
*        OR SCREEN-NAME NE 'WG_HEADER-DTENT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-ANTEC'
*      OR SCREEN-NAME = 'WG_HEADER-DTINIJUROS'
*      OR SCREEN-NAME = 'WG_HEADER-KURSF'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG.
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*      IF W_FIELDCATALOG-FIELDNAME EQ 'CALCU'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRUNIT'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRTOT'.
*
*        W_FIELDCATALOG-NO_OUT = C_X.
*      ELSE.
*        W_FIELDCATALOG-NO_OUT = SPACE.
*
*      ENDIF.
*
*      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*      CLEAR: W_FIELDCATALOG.
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG.
*** Troca
*  ELSEIF WG_HEADER-TPSIM(1) EQ C_T.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_ADT
*      OR SCREEN-GROUP1 EQ C_VIS.
*        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'.
*          IF SCREEN-NAME NE 'WG_DESC_DAT'.
*            ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*            IF <FS_CAMPO> IS ASSIGNED.
*              CLEAR: <FS_CAMPO>.
*            ENDIF.
*            UNASSIGN <FS_CAMPO>.
*          ENDIF.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-KURSF'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-ANTEC'.
*        IF WG_HEADER-TPSIM+1(1) EQ 'V'.
*          IF SCREEN-NAME NE 'WG_DESC_DAT'.
*            ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*            IF <FS_CAMPO> IS ASSIGNED.
*              CLEAR: <FS_CAMPO>.
*            ENDIF.
*            UNASSIGN <FS_CAMPO>.
*          ENDIF.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG.
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*      IF W_FIELDCATALOG-FIELDNAME EQ 'COMPR'.
*
*        W_FIELDCATALOG-NO_OUT = C_X.
*      ELSE.
*        W_FIELDCATALOG-NO_OUT = SPACE.
*      ENDIF.
*      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*      CLEAR: W_FIELDCATALOG.
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG.
*** A Vista
*  ELSEIF WG_HEADER-TPSIM(1) EQ C_V.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_ADT
*      OR SCREEN-GROUP1 EQ C_TRO.
*        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'.
*          IF SCREEN-NAME NE 'WG_DESC_DAT'.
*            IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO'  AND WG_HEADER-TPSIM EQ 'VP') OR
*               ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'VP').
*            ELSE.
*              ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*              IF <FS_CAMPO> IS ASSIGNED.
*                CLEAR: <FS_CAMPO>.
*              ENDIF.
*              UNASSIGN <FS_CAMPO>.
*            ENDIF.
*          ENDIF.
*          SCREEN-ACTIVE = 0.
*          IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO' AND WG_HEADER-TPSIM EQ 'VP').
*            WG_DESC_DAT = 'Data Vencimento'.
*            SCREEN-ACTIVE = 1.
*          ENDIF.
*          IF ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'VP').
*            SCREEN-ACTIVE = 1.
*          ENDIF.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*
*      IF ( SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR SCREEN-NAME = 'WG_DESC_DAT') AND ( WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP' ).
*        IF SCREEN-NAME NE 'WG_DESC_DAT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*        ENDIF.
*        SCREEN-ACTIVE   = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-KURSF' AND WG_HEADER-TPSIM NE 'VF'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*      IF SCREEN-NAME = 'WG_HEADER-ANTEC'.
*        IF SCREEN-NAME NE 'WG_DESC_DAT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*        ENDIF.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG.
*
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*      IF W_FIELDCATALOG-FIELDNAME EQ 'COMPR'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRUNIT'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'SIUMB'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'TRTOT'
*      OR W_FIELDCATALOG-FIELDNAME EQ 'CALCU'.
*
*        W_FIELDCATALOG-NO_OUT = C_X.
*      ELSE.
*        W_FIELDCATALOG-NO_OUT = SPACE.
*      ENDIF.
*      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*      CLEAR: W_FIELDCATALOG.
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG.
*  ELSE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ C_ADT
*      OR SCREEN-GROUP1 EQ C_TRO
*      OR SCREEN-GROUP1 EQ C_VIS.
*        IF  SCREEN-NAME EQ 'WG_HEADER-VLRTOT' AND WG_HEADER-TPSIM EQ 'BN'.
*          SCREEN-ACTIVE = 1.
*        ELSE.
*          SCREEN-ACTIVE = 0.
*        ENDIF.
*        MODIFY SCREEN.
*      ENDIF.
*      IF ( SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR SCREEN-NAME = 'WG_DESC_DAT' OR SCREEN-NAME = 'WG_HEADER-ANTEC' ) AND WG_HEADER-TPSIM EQ 'BN'.
*        IF SCREEN-NAME NE 'WG_DESC_DAT'.
*          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*          IF <FS_CAMPO> IS ASSIGNED.
*            CLEAR: <FS_CAMPO>.
*          ENDIF.
*          UNASSIGN <FS_CAMPO>.
*        ENDIF.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF SCREEN-NAME = 'WG_HEADER-KURSF' AND WG_HEADER-TPSIM EQ 'BN'.
*        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
*        IF <FS_CAMPO> IS ASSIGNED.
*          CLEAR: <FS_CAMPO>.
*        ENDIF.
*        UNASSIGN <FS_CAMPO>.
*        SCREEN-ACTIVE = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
*
*  IF WG_ACAO EQ C_ADD
*  OR WG_ACAO EQ C_MODIF.
*    LOOP AT TG_ITENS WHERE VBELN IS NOT INITIAL.
*
*    ENDLOOP.
*    LOOP AT SCREEN.
*      IF SY-SUBRC IS NOT INITIAL.
*        IF SCREEN-GROUP2 EQ 'A1'.
*          IF SCREEN-NAME EQ 'PB_DESCP'.
*            SCREEN-INVISIBLE = 0.
*          ENDIF.
*          SCREEN-INPUT = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*      IF SCREEN-GROUP2 EQ 'A2'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
**** modificar aqui
*    LOOP AT TG_ITENS WHERE VBELN IS NOT INITIAL.
*
*    ENDLOOP.
*    IF SY-SUBRC IS INITIAL.
*      IF GRID1 IS NOT INITIAL.
*
*        CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*          IMPORTING
*            ET_FIELDCATALOG = T_FIELDCATALOG.
*
**      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
****         FIM DE BLOQUEI DE CAMPOS
*
*        LOOP AT TG_ITENS.
*          REFRESH: TG_ITENS-STYLE.
*          REFRESH: STYLE.
*          CLEAR: WA_STYLE.
*          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
*           WHERE EDIT EQ C_X.
*            WA_STYLE-FIELDNAME = W_FIELDCATALOG-FIELDNAME.
*            IF TG_ITENS-VBELN IS NOT INITIAL.
*              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*            ELSE.
*              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*            ENDIF.
*            INSERT  WA_STYLE INTO TABLE STYLE .
*          ENDLOOP.
*          INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
*          MODIFY TG_ITENS.
*        ENDLOOP.
*
*      ENDIF.
*    ENDIF.
*
*  ELSEIF WG_ACAO EQ C_DESCONT.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP2 EQ 'A2'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  CASE SY-UCOMM.
*      "WSB SCREEN SACA
*    WHEN C_SACAS.
*      LOOP AT SCREEN.
*        IF  SCREEN-NAME EQ 'WG_HEADER-TROTOTSC'
*         OR SCREEN-NAME EQ 'CONVERT_TRATOTSC'.
*
*          SCREEN-INPUT = 1.
*          MODIFY SCREEN.
*
*        ENDIF.
*      ENDLOOP.
*
*    WHEN C_DSC_ABS.
*
*      LOOP AT TG_ITENS.
*        REFRESH: IT_VBFA, IT_VBAP, IT_41, IT_VBAK.
*
*        SELECT *
*          FROM VBFA
*          INTO TABLE IT_VBFA
*          WHERE VBELV EQ TG_ITENS-VBELN.
*
*        IF IT_VBFA IS NOT INITIAL.
*          REFRESH IT_VBFA.
*
*          SELECT *
*            FROM VBAK
*            INTO TABLE IT_VBAK
*            WHERE VBELN EQ TG_ITENS-VBELN.
*
*          IF IT_VBAK IS NOT INITIAL.
*            SELECT *
*              FROM VBAP
*              INTO TABLE IT_VBAP
*              FOR ALL ENTRIES IN IT_VBAK
*              WHERE VBELN EQ IT_VBAK-VBELN.
*
*            IF IT_VBAP IS NOT INITIAL.
*              SELECT *
*                FROM VBFA
*                INTO TABLE IT_VBFA
*                FOR ALL ENTRIES IN IT_VBAP
*                WHERE VBELV EQ IT_VBAP-VBELN.
*
*              LOOP AT IT_VBAP INTO WA_VBAP.
*                READ TABLE IT_VBFA TRANSPORTING NO FIELDS WITH KEY VBELV = WA_VBAP-VBELN
*                                                                   POSNV = WA_VBAP-POSNR.
*                IF SY-SUBRC IS INITIAL.
*                  SELECT *
*                  FROM ZSDT0041
*                  APPENDING TABLE IT_41
*                    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
*                      AND POSNR EQ TG_ITENS-POSNR
*                      AND MATNR EQ WA_VBAP-MATNR.
*
*                ENDIF.
*              ENDLOOP.
*            ENDIF.
*          ENDIF.
*        ENDIF.
**        IF IT_41 IS NOT INITIAL.
*
*        CLEAR: WA_STYLE.
*        REFRESH: TG_ITENS-STYLE.
*        WA_STYLE-FIELDNAME = 'DESC_ABSOLUTO'.
*
**          IF TG_ITENS-VBELN IS NOT INITIAL.
*        IF IT_41 IS NOT INITIAL.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**          ENDIF.
*        ELSE.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*        ENDIF.
*
*
*        READ TABLE STYLE TRANSPORTING NO FIELDS WITH KEY FIELDNAME = 'DESC_ABSOLUTO'.
*        IF SY-SUBRC IS INITIAL.
*          DELETE STYLE WHERE FIELDNAME = 'DESC_ABSOLUTO'.
*          INSERT  WA_STYLE INTO TABLE STYLE.
*        ELSE.
*          INSERT  WA_STYLE INTO TABLE STYLE.
*        ENDIF.
*
**
**        WA_STYLE-FIELDNAME = 'VBELN'.
**        WA_STYLE-STYLE = '00000006'.
**
**        INSERT  WA_STYLE INTO TABLE STYLE .
**        ELSE.
*
**          CLEAR: WA_STYLE.
**          REFRESH: TG_ITENS-STYLE.
*
**          WA_STYLE-FIELDNAME = 'DESC_ABSOLUTO'.
*
**          IF TG_ITENS-VBELN IS NOT INITIAL.
**            WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**          ENDIF.
*
**          INSERT  WA_STYLE INTO TABLE STYLE .
*
**        ENDIF.
*        INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
*        MODIFY TG_ITENS.
*
*      ENDLOOP.
*
*  ENDCASE.
  " Bloco de Código acima movido para SubScreen 0103 - PBO MODIFY_SCREEN_0103

  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
                        USING SY-UNAME.

  CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  PERFORM VERIFICA_ERROS.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN    = '100'
      I_SHOW      = SPACE
      I_REPID     = SY-REPID
      I_POPUP     = 0
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      I_SET_FIELD = 'X_FIELD'
    IMPORTING
      E_MESSAGEM  = WG_MENSAGEM
    TABLES
      IT_MSGS     = TG_MSG_RET.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."'WG_DESC_OPERACAO'.
  ENDIF.

  IF WG_CELL IS NOT INITIAL .
    REFRESH: TG_CELL.
    APPEND WG_CELL TO TG_CELL.
    CALL METHOD GRID1->SET_SELECTED_CELLS
      EXPORTING
        IT_CELLS = TG_CELL[].
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DOC INPUT.
  DATA: BEGIN OF TL_DOC OCCURS 0,
          DOC_SIMULACAO TYPE ZSDT0040-DOC_SIMULACAO,
          NAME1         TYPE KNA1-NAME1,
          SAFRA         TYPE ZSDT0040-SAFRA,
          VKBUR         TYPE ZSDT0040-VKBUR,
          TPSIM         TYPE ZSDED007,
          STATUS        TYPE ZSDT0040-STATUS,
          ERDAT         TYPE ZSDT0040-ERDAT,
        END OF TL_DOC.
  DATA: TL_0040       TYPE TABLE OF ZSDT0040 WITH HEADER LINE,
        TL_KNA1       TYPE TABLE OF KNA1 WITH HEADER LINE,

*        TL_VALUES     TYPE VRM_VALUES,
*        WL_VALUES     TYPE LINE OF VRM_VALUES,

        TL_SET        TYPE TABLE OF RGSB4      WITH HEADER LINE,

        TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  REFRESH: TL_DOC, TL_0040, TL_KNA1.
  CLEAR: TL_DOC, TL_0040, TL_KNA1.

  SELECT *
    FROM ZSDT0040
    INTO TABLE TL_0040.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM KNA1
      INTO TABLE TL_KNA1
       FOR ALL ENTRIES IN TL_0040
     WHERE KUNNR EQ TL_0040-KUNNR.
  ENDIF.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS           = '0000'
      SETNR           = 'MAGGI_ZSDT0044_05'
      NO_DESCRIPTIONS = SPACE
      NO_RW_INFO      = SPACE
    TABLES
      SET_VALUES      = TL_SET
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

***  Busca texto de tipo de operação
*  CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
*    EXPORTING
*      I_TABLE_NAME = 'ZSDT0040'
*      I_FIELD_NAME = 'TPSIM'
*    IMPORTING
*      E_T_LIST     = TL_VALUES.

  LOOP AT TL_0040.
    READ TABLE TL_KNA1
      WITH KEY KUNNR = TL_0040-KUNNR.

*    READ TABLE TL_VALUES INTO WL_VALUES WITH KEY KEY = TL_0040-TPSIM.
    READ TABLE TL_SET WITH KEY FROM = TL_0040-TPSIM.
    MOVE: TL_0040-DOC_SIMULACAO TO TL_DOC-DOC_SIMULACAO,
          TL_0040-SAFRA         TO TL_DOC-SAFRA,
          TL_0040-VKBUR         TO TL_DOC-VKBUR,
          TL_KNA1-NAME1         TO TL_DOC-NAME1,
*          WL_VALUES-TEXT        TO TL_DOC-TPSIM,
          TL_SET-TITLE          TO TL_DOC-TPSIM,
          TL_0040-STATUS        TO TL_DOC-STATUS,
          TL_0040-ERDAT         TO TL_DOC-ERDAT.

    APPEND TL_DOC.
    CLEAR: TL_DOC, TL_KNA1.

  ENDLOOP.

  SORT: TL_DOC BY ERDAT DESCENDING.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DOC_SIMULACAO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_HEADER-DOC_SIMULACAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_DOC
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.                 " SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM GET_NEXT_NUMBER  USING    P_OBJECT   "TYPE nrobj
                               P_NR_RANGE "TYPE nrnr
                      CHANGING P_NUMBER.

  CLEAR P_NUMBER.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NR_RANGE
      OBJECT                  = P_OBJECT
    IMPORTING
      NUMBER                  = P_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CLEAR: P_NUMBER.
    MESSAGE E836(SD) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    WG_FLAG = C_X.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS_DOC.

  DATA: WL_0040 TYPE ZSDT0040,
        TL_0041 TYPE TABLE OF ZSDT0041,
        TL_0186 TYPE TABLE OF ZSDT0186 WITH HEADER LINE,
        TL_MAKT TYPE TABLE OF MAKT.

  FREE WA_T001K.

  SELECT SINGLE *
    FROM ZSDT0040
    INTO WL_0040
     WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  TPSIM = WL_0040-TPSIM.

  IF SY-SUBRC IS INITIAL.

    PERFORM F_VALIDAR_USUARIO USING TPSIM CHANGING SY-SUBRC.
    IF SY-SUBRC IS INITIAL.
** Valida se usuário tem permissão de visuzlizar simulação
      PERFORM F_VALIDAR_ESC_VENDA USING WL_0040-VKBUR
                                  CHANGING SY-SUBRC.
      IF SY-SUBRC IS INITIAL.

        SELECT *
          FROM ZSDT0041
          INTO TABLE TL_0041
           WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

        IF SY-SUBRC IS INITIAL.
          SELECT *
            FROM MAKT
            INTO TABLE TL_MAKT
            FOR ALL ENTRIES IN TL_0041
             WHERE MATNR EQ TL_0041-MATNR
               AND SPRAS EQ SY-LANGU.

        ENDIF.

        SELECT SINGLE *
          FROM T001K
            INTO WA_T001K
                WHERE BWKEY EQ WG_HEADER-VKBUR.

        SELECT *
          FROM ZSDT0186
          INTO TABLE TL_0186
            WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

*Início - Sara Oikawa - 38859 - Agosto/2020
        IF SY-UCOMM EQ C_COPY.
          REFRESH TL_0186.
        ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
        " Busca Vínculo Dados de Compra SIGAM
        IF SY-UCOMM EQ C_ATUAL.
          SELECT SINGLE *
             FROM ZSDT0260
             INTO WG_0260
            WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
        ENDIF.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

        PERFORM MONTA_DADOS_DOC TABLES: TL_0041
                                        TL_MAKT
                                        TL_0186
                                 USING  WL_0040.
      ENDIF.

    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Usuário não possui acesso as '
                                             'Condições de Pag. Bonificação/Venda Futura/Permuta!'.
    ENDIF.

  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'O documento de simulação de venda'
                                           'não foi encontrado'.
  ENDIF.



ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM F_VALIDAR_USUARIO USING  P_TPSIM TYPE CHAR2 CHANGING C_ERRO  TYPE SY-SUBRC .


  CASE P_TPSIM.
    WHEN 'BN' OR 'CF' OR 'PM'.

      CLEAR C_ERRO.

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          CLASS         = '0000'
          SETNR         = 'MAGGI_ZSDT0044_04'
        TABLES
          SET_VALUES    = T_USERMD
        EXCEPTIONS
          SET_NOT_FOUND = 1
          OTHERS        = 2.

      IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      SORT T_USERMD BY FROM.
      READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.

      C_ERRO = SY-SUBRC.

    WHEN OTHERS.
      C_ERRO = 0.
  ENDCASE.


ENDFORM.                    " F_VALIDAR_USUARIO

*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0041  text
*      -->P_WL_0040  text
*----------------------------------------------------------------------*
FORM MONTA_DADOS_DOC  TABLES   TL_0041 STRUCTURE ZSDT0041
                               TL_MAKT STRUCTURE MAKT
                               TL_0186 STRUCTURE ZSDT0186
                      USING    WL_0040 TYPE ZSDT0040.


  DATA: WL_OV_GERADA,
        WL_0037      TYPE ZSDT0037,
        WL_MARA      TYPE MARA.

  CLEAR: WG_HEADER, WG_HEADER_OLD,TG_ITENS, WL_OV_GERADA, WL_0037.
  REFRESH: TG_ITENS.

  MOVE: WL_0040-DOC_SIMULACAO  TO WG_HEADER-DOC_SIMULACAO,
        WL_0040-KUNNR          TO WG_HEADER-KUNNR,
        WL_0040-VKBUR          TO WG_HEADER-VKBUR,
        WL_0040-FAZENDA        TO WG_HEADER-FAZENDA,
        WL_0040-TPSIM          TO WG_HEADER-TPSIM,
        WL_0040-VENDEDOR       TO WG_HEADER-VENDEDOR,
        WL_0040-AREA_HA        TO WG_AREA_HA,
        WL_0040-WAERK          TO WG_HEADER-WAERK,
        WL_0040-CULTURA        TO WG_HEADER-CULTURA,
        WL_0040-SAFRA          TO WG_HEADER-SAFRA,
        WL_0040-ERNAM          TO WG_HEADER-ERNAM,
        WL_0040-ERDAT          TO WG_HEADER-ERDAT,
        WL_0040-ERZET          TO WG_HEADER-ERZET,
        WL_0040-PREC_ANT_CULT  TO WG_HEADER-PREC_ANT_CULT,
        WL_0040-JUROS_ANO      TO WG_HEADER-JUROS_ANO,
        WL_0040-VLR_ADTO       TO WG_HEADER-VLR_ADTO,
        WL_0040-ADTO_HA        TO WG_HEADER-ADTO_HA,
        WL_0040-AREA_PENHOR    TO WG_HEADER-AREA_PENHOR,
        WL_0040-VKORG          TO WG_HEADER-VKORG,
        WL_0040-MEIO_PAGO      TO WG_HEADER-MEIO_PAGO,
*Início - Sara Oikawa - 38859 - Agosto/2020
        WL_0040-MEIO_PAGO      TO WG_MEIO_PAGO,
*Fim - Sara Oikawa - 38859 - Agosto/2020
        WL_0040-VTWEG          TO WG_HEADER-VTWEG,

* Criado o CONVERT_TRATOTSC para remover as virgulas na impressão na Tela.
*        WL_0040-TROTOTSC      TO WG_HEADER-TROTOTSC,
        WL_0040-TROTOTSC       TO CONVERT_TRATOTSC,
        CONVERT_TRATOTSC       TO WG_HEADER-TROTOTSC,
        WL_0040-SCHA           TO WG_HEADER-SCHA,
        WL_0040-VLRTOT         TO WG_HEADER-VLRTOT,
        WL_0040-COMPRSC        TO WG_HEADER-COMPRSC,
        WL_0040-TPCULT         TO WG_HEADER-TPCULT,
        WL_0040-FCULT          TO WG_HEADER-FCULT,
        WL_0040-DTENT          TO WG_HEADER-DTENT,
        WL_0040-ANTEC          TO WG_HEADER-ANTEC,
        WL_0040-DTPGTCULT      TO WG_HEADER-DTPGTCULT,

        " 09.01.2023 - RAMON - 61181 -->
        WL_0040-DTPGTCULT      TO WG_HEADER-DTPREVPGTO,
        " 09.01.2023 - RAMON - 61181 --<

        WL_0040-DTINIJUROS     TO WG_HEADER-DTINIJUROS,
        WL_0040-KURSF          TO WG_HEADER-KURSF,

        WL_0040-TAXA_FRETE     TO WG_HEADER-TAXA_FRETE,

        WL_0040-DT_ENTREGA_SEM TO WG_HEADER-DT_ENTREGA_SEM,
        WL_0040-DT_ENTREGA_DEF TO WG_HEADER-DT_ENTREGA_DEF,
        WL_0040-DT_ENTREGA_FET TO WG_HEADER-DT_ENTREGA_FET,
        WL_0040-MEIO_PAGO      TO WG_HEADER-MEIO_PAGO,
*Início - Sara Oikawa - 38859 - Agosto/2020
        WL_0040-MEIO_PAGO      TO WG_MEIO_PAGO,
*Fim - Sara Oikawa - 38859 - Agosto/2020
        WL_0040-DTVENCOV       TO WG_HEADER-DTVENCOV,
        WL_0040-REPIQUE        TO WG_HEADER-REPIQUE,
        WL_0040-FUNRURAL       TO WG_HEADER-FUNRURAL,

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
        WL_0040-INSS               TO WG_HEADER-INSS,
        WL_0040-FACS               TO WG_HEADER-FACS,
        WL_0040-FUNRURAL           TO WG_HEADER-FUNRURAL,
        WL_0040-FUNUSER            TO WG_HEADER-FUNUSER,
        WL_0040-FUNDATA            TO WG_HEADER-FUNDATA,
        WL_0040-FUNHORA            TO WG_HEADER-FUNHORA,
        WL_0040-HBKID              TO WG_HEADER-HBKID, " US 81799 - CBRAND

  " 21.02.2023 - RAMON - 102323 -->
  WL_0040-ECOMMERCE  TO WG_HEADER-ECOMMERCE,

*#141572 -  ITSOUZA - Inicio
        WL_0040-FUNDEINFRA         TO WG_HEADER-FUNDEINFRA      ,
        WL_0040-FUNDEINFRA_EXCE    TO WG_HEADER-FUNDEINFRA_EXCE .
*#141572 -  ITSOUZA - Fim

  IF SY-UCOMM NE 'COPY'.
    MOVE WL_0040-ID_ORDER_ECOMMERCE TO WG_HEADER-ID_ORDER_ECOMMERCE.
  ENDIF.

  " 21.02.2023 - RAMON - 102323 --<


  CASE WL_0040-STATUS.
    WHEN ''.  WG_STATUS = ICON_INITIAL.
    WHEN C_A. WG_STATUS = ICON_RELEASE.
    WHEN C_R. WG_STATUS = ICON_DEFECT.
    WHEN C_B. WG_STATUS = ICON_GIS_PAN.
  ENDCASE.

*-CS2019001753-04.01.2023-#65741-JT-inicio
  SELECT SINGLE *
    FROM T001K
    INTO WA_T001K
   WHERE BWKEY EQ WG_HEADER-VKBUR.
*-CS2019001753-04.01.2023-#65741-JT-fim

  LOOP AT TL_0041 WHERE VBELN NE SPACE.
    WL_OV_GERADA = C_X.
    EXIT.
  ENDLOOP.

  IF SY-UCOMM EQ 'MODIF' OR
    SY-UCOMM EQ 'BTN_FUN'.
    CLEAR WG_HEADER-VLRTOT.
  ENDIF.

  SORT: TL_MAKT BY MATNR.
  LOOP AT TL_0041.
    READ TABLE TL_MAKT
      WITH KEY MATNR = TL_0041-MATNR
               BINARY SEARCH.

    MOVE: TL_0041-POSNR       TO TG_ITENS-POSNR,
          TL_0041-MATNR       TO TG_ITENS-MATNR,
          TL_0041-ZMENG       TO TG_ITENS-ZMENG,
          TL_0041-ZIEME       TO TG_ITENS-ZIEME,
          TL_0041-DTVENC      TO TG_ITENS-DTVENC,
          TL_0041-ZWERT       TO TG_ITENS-ZWERT,
          TL_0041-CALCU       TO TG_ITENS-CALCU,
          TL_0041-TRUNIT      TO TG_ITENS-TRUNIT,
          TL_0041-SIUMB       TO TG_ITENS-SIUMB,
          TL_0041-TRTOT       TO TG_ITENS-TRTOT,
          TL_0041-COMPR       TO TG_ITENS-COMPR,
          TL_0041-VLRTOT      TO TG_ITENS-VLRTOT,
          TL_MAKT-MAKTX       TO TG_ITENS-MAKTX,
          TL_0041-AUART       TO TG_ITENS-AUART,
          TL_0041-SPART       TO TG_ITENS-SPART,
          TL_0041-WERKS       TO TG_ITENS-WERKS,
          TL_0041-CHARG       TO TG_ITENS-CHARG,
          TL_0041-INCO1       TO TG_ITENS-INCO1,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          TL_0041-CULTURA_APL TO TG_ITENS-CULTURA_APL,
          TL_0041-SAFRA_APL   TO TG_ITENS-SAFRA_APL,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          TL_0041-DESCONTO    TO TG_ITENS-DESCONTO,
          TL_0041-VLR_FRETE   TO TG_ITENS-VLR_FRETE,
          TL_0041-VL_UNIT     TO TG_ITENS-VL_UNIT,
          TL_0041-VLR_ICMS    TO TG_ITENS-VLR_ICMS,
          TL_0041-VLR_COFINS  TO TG_ITENS-VLR_COFINS,
          TL_0041-VLR_PIS     TO TG_ITENS-VLR_PIS,
          TL_0041-VLR_AJUSTE  TO TG_ITENS-VLR_AJUSTE,
          TL_0041-ZWERT_LIQDO TO TG_ITENS-ZWERT_LIQDO.

    IF SY-UCOMM EQ 'MODIF' OR
      SY-UCOMM EQ 'BTN_FUN'.
      SUBTRACT TG_ITENS-VLR_AJUSTE FROM TG_ITENS-VLRTOT.
      CLEAR TG_ITENS-VLR_AJUSTE.
      ADD TG_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
    ENDIF.
*BREAK ABAP.


*    IF ( TL_0041-VL_UNIT IS INITIAL ).
*      MOVE TL_0041-ZWERT   TO TG_ITENS-VL_UNIT.
*    ELSE.
*      MOVE TL_0041-VL_UNIT TO TG_ITENS-VL_UNIT.
*    ENDIF.

    IF ( SY-UCOMM EQ 'COPY' ).
**   Selecionar novamente os registros em caso de cópia para verificar se houve **
**   alteração nos valores                                                      **
      SELECT SINGLE VLR_VENDA
        FROM ZSDT0036
        INTO (TG_ITENS-VL_UNIT)
       WHERE VAL_DE       LE SY-DATUM
         AND VAL_ATE      GE SY-DATUM
         AND MATNR        EQ TG_ITENS-MATNR
         AND WAERK        EQ WG_HEADER-WAERK
         AND MEINS        EQ TG_ITENS-ZIEME
         AND INCO1        EQ TG_ITENS-INCO1
         AND SAFRA        EQ WG_HEADER-SAFRA
         AND CULTURA      EQ WG_HEADER-CULTURA
         AND LOEKZ        EQ SPACE
         AND WERKS_FORNEC EQ TG_ITENS-WERKS
         AND ELIMINADO    EQ SPACE
         AND BUKRS        EQ WA_T001K-BUKRS.

      IF ( TG_ITENS-INCO1 = 'CIF').

**    Verifica se possuí frete **
        SELECT SINGLE MATKL
          FROM MARA
          INTO (WL_MARA-MATKL)
         WHERE MATNR EQ TG_ITENS-MATNR.

        SELECT SINGLE VLR_FRETE
          FROM ZSDT0037
          INTO (WL_0037-VLR_FRETE)
         WHERE VAL_DE          LE SY-DATUM
           AND VAL_ATE         GE SY-DATUM
           AND MEINS           EQ TG_ITENS-ZIEME
           AND FILIAL_ORIGEM   EQ TG_ITENS-WERKS
           AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
           AND MATKL           EQ WL_MARA-MATKL
           AND WAERS           EQ 'BRL'
           AND BUKRS           EQ WA_T001K-BUKRS.

** COPIA
        PERFORM ATUALIZA_FRETE CHANGING WL_0037-VLR_FRETE.

        ADD WL_0037-VLR_FRETE TO TG_ITENS-VL_UNIT.
      ENDIF.

      TG_ITENS-ZWERT         = TG_ITENS-VL_UNIT.
      TG_ITENS-VLRTOT        = TG_ITENS-ZMENG * TG_ITENS-ZWERT.
    ELSE.
      TG_ITENS-DESC_ABSOLUTO = TL_0041-DESC_ABSOLUTO.
    ENDIF.

    MOVE: TL_0041-MGCAD    TO TG_ITENS-MGCAD,
          TL_0041-MGEFE    TO TG_ITENS-MGEFE,
          TL_0041-VBELN    TO TG_ITENS-VBELN.
*          tl_0041-antec  to tg_itens-antec.



    IF WL_OV_GERADA IS NOT INITIAL.
      REFRESH: STYLE, TG_ITENS-STYLE.

      WA_STYLE-FIELDNAME = 'VBELN'.
      IF TG_ITENS-VBELN IS NOT INITIAL.
        WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE..
      ELSE.
        WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE..
      ENDIF.
      INSERT  WA_STYLE INTO TABLE STYLE.
      CLEAR: WA_STYLE.
      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
    ENDIF.
    APPEND TG_ITENS.
    CLEAR: TG_ITENS.
  ENDLOOP.

  LOOP AT TL_0186.

    MOVE-CORRESPONDING: TL_0186 TO TG_LOG.

    APPEND TG_LOG.
    CLEAR: TG_LOG, TL_0186.
  ENDLOOP.


  WG_HEADER_OLD = WG_HEADER.

ENDFORM.                    " MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: WL_INPUT_0040 TYPE ZSDT0040,
        TL_INPUT_0041 TYPE TABLE OF ZSDT0041 WITH HEADER LINE.

*Início - Sara Oikawa - 38859 - Agosto/2020
  DATA: WL_40_OLD TYPE ZSDT0040,
        TL_41_OLD TYPE TABLE OF ZSDT0041 WITH HEADER LINE.
*Fim - Sara Oikawa - 38859 - Agosto/2020

  DATA: VL_INDEX TYPE SY-TABIX.

  REFRESH: TL_INPUT_0041.
  CLEAR: WL_INPUT_0040, TL_INPUT_0041.


* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Inicio
*      PERFORM f_valida_sisdev.
  DATA: ZCL_OBJ_MANUTENCAO_INSUMOS TYPE REF TO ZCL_MANUTENCAO_INSUMOS.
  CREATE OBJECT ZCL_OBJ_MANUTENCAO_INSUMOS.
  DATA: VG_CHECK_ERRO TYPE CHAR01.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

  MOVE: WG_MEIO_PAGO      TO WG_HEADER-MEIO_PAGO,
        CONVERT_TRATOTSC  TO WG_HEADER-TROTOTSC.

  MOVE-CORRESPONDING WG_HEADER TO WL_INPUT_0040.

  MOVE: WG_AREA_HA               TO WL_INPUT_0040-AREA_HA,
        SPACE                    TO WL_INPUT_0040-PREC_CULT,
        ABAP_FALSE               TO WL_INPUT_0040-JOB,
        SY-UNAME                 TO WL_INPUT_0040-USNAM,
        SY-DATUM                 TO WL_INPUT_0040-DATA_ATUAL,
        SY-UZEIT                 TO WL_INPUT_0040-HORA_ATUAL.

  CASE WG_SAVE.
    WHEN C_DSC_ABS.
      MOVE 'A' TO WL_INPUT_0040-STATUS.
    WHEN C_SACAS.
      SELECT SINGLE STATUS
        FROM ZSDT0040
          INTO WL_INPUT_0040-STATUS
        WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
  ENDCASE.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  SELECT SINGLE *
    FROM ZSDT0040
      INTO WL_40_OLD
    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
  MOVE WL_40_OLD-STATUS TO WL_INPUT_0040-STATUS.
  SELECT *
    FROM ZSDT0041
    INTO TABLE TL_41_OLD
    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  IF SY-UCOMM EQ C_RECALC.
    SELECT SINGLE STATUS
      FROM ZSDT0040
        INTO WL_INPUT_0040-STATUS
      WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
  ENDIF.

  LOOP AT TG_ITENS.
    CLEAR: VG_CHECK_ERRO.
    VL_INDEX = SY-TABIX.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Inicio
    IF TG_ITENS-SPART EQ '03' OR TG_ITENS-SPART EQ '04'.

      "FF #174195 - inicio
      DATA LV_WERKS TYPE ZSDT0041-WERKS.

      LV_WERKS = TG_ITENS-WERKS_FORNEC.

      IF LV_WERKS IS INITIAL.
        LV_WERKS = TG_ITENS-WERKS.
      ENDIF.
      "FF #174195 - fim

      ZCL_OBJ_MANUTENCAO_INSUMOS->CHECK_CADASTRO_FORN_SISDEV(
        EXPORTING
          I_WERKS = LV_WERKS " Centro
          I_KUNNR = WG_HEADER-KUNNR " Nº cliente
        IMPORTING
          E_ERRO  = VG_CHECK_ERRO                 " Campo de texto do comprimento 1
      ).

      IF  VG_CHECK_ERRO IS NOT INITIAL.
        MESSAGE 'Cliente não possui cadastro no SISDEV' TYPE 'I'.
      ENDIF.
    ENDIF.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Inicio

    PERFORM BUSCA_IMPOSTO CHANGING TG_ITENS.
    MODIFY TG_ITENS[] FROM TG_ITENS INDEX VL_INDEX TRANSPORTING VLR_ICMS ZWERT_LIQDO.

    MOVE: WG_HEADER-DOC_SIMULACAO  TO TL_INPUT_0041-DOC_SIMULACAO,
          TG_ITENS-POSNR           TO TL_INPUT_0041-POSNR,
          TG_ITENS-MATNR           TO TL_INPUT_0041-MATNR,
          TG_ITENS-ZMENG           TO TL_INPUT_0041-ZMENG,
          TG_ITENS-ZIEME           TO TL_INPUT_0041-ZIEME,
          TG_ITENS-ZWERT           TO TL_INPUT_0041-ZWERT,
          TG_ITENS-CALCU           TO TL_INPUT_0041-CALCU,
          TG_ITENS-TRUNIT          TO TL_INPUT_0041-TRUNIT,
          TG_ITENS-SIUMB           TO TL_INPUT_0041-SIUMB,
          TG_ITENS-TRTOT           TO TL_INPUT_0041-TRTOT,
          TG_ITENS-COMPR           TO TL_INPUT_0041-COMPR,
          TG_ITENS-VLRTOT          TO TL_INPUT_0041-VLRTOT,
          TG_ITENS-AUART           TO TL_INPUT_0041-AUART,
          TG_ITENS-SPART           TO TL_INPUT_0041-SPART,
          TG_ITENS-WERKS           TO TL_INPUT_0041-WERKS,
          TG_ITENS-CHARG           TO TL_INPUT_0041-CHARG,
          TG_ITENS-INCO1           TO TL_INPUT_0041-INCO1,
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          TG_ITENS-CULTURA_APL     TO TL_INPUT_0041-CULTURA_APL,
          TG_ITENS-SAFRA_APL       TO TL_INPUT_0041-SAFRA_APL,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          TG_ITENS-DESCONTO        TO TL_INPUT_0041-DESCONTO,
          TG_ITENS-VLR_AJUSTE      TO TL_INPUT_0041-VLR_AJUSTE,
          TG_ITENS-DESC_ABSOLUTO   TO TL_INPUT_0041-DESC_ABSOLUTO,
          TG_ITENS-VL_UNIT         TO TL_INPUT_0041-VL_UNIT,
          TG_ITENS-MGCAD           TO TL_INPUT_0041-MGCAD,
          TG_ITENS-MGEFE           TO TL_INPUT_0041-MGEFE,
          TG_ITENS-DTVENC          TO TL_INPUT_0041-DTVENC,
          TG_ITENS-VBELN           TO TL_INPUT_0041-VBELN,
          TG_ITENS-VLR_FRETE       TO TL_INPUT_0041-VLR_FRETE,
          TG_ITENS-VLR_ICMS        TO TL_INPUT_0041-VLR_ICMS,
          TG_ITENS-ZWERT_LIQDO     TO TL_INPUT_0041-ZWERT_LIQDO,
          TG_ITENS-J_1BTXSDC       TO TL_INPUT_0041-J_1BTXSDC,
          TG_ITENS-J_1BTAXLW1      TO TL_INPUT_0041-J_1BTAXLW1,
          TG_ITENS-J_1BTAXLW5      TO TL_INPUT_0041-J_1BTAXLW5,
          TG_ITENS-J_1BTAXLW4      TO TL_INPUT_0041-J_1BTAXLW4.
*          tg_itens-antec          to tl_input_0041-antec.

    APPEND TL_INPUT_0041.
    CLEAR: TG_ITENS, TL_INPUT_0041.
  ENDLOOP.

  DELETE FROM ZSDT0040 WHERE DOC_SIMULACAO EQ WL_INPUT_0040-DOC_SIMULACAO.
  DELETE FROM ZSDT0041 WHERE DOC_SIMULACAO EQ WL_INPUT_0040-DOC_SIMULACAO.

  " 06.02.2023 - Correção inserção data pagamento prev ->
  IF WL_INPUT_0040-TPSIM = 'VV'.
    WL_INPUT_0040-DTPREVPGTO = WL_INPUT_0040-DTVENCOV.
  ELSE.
    WL_INPUT_0040-DTPREVPGTO = WL_INPUT_0040-DTPGTCULT.
  ENDIF.
  " 06.02.2023 - Correção inserção data pagamento prev -<

  MODIFY ZSDT0040 FROM WL_INPUT_0040.
  MODIFY ZSDT0041 FROM TABLE TL_INPUT_0041.

  COMMIT WORK.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  " Grava Vínculo Dados Compra Sigam
  IF WL_INPUT_0040-TPSIM(1) EQ C_TRO(1).
    IF WG_0260-ID_COMPRA IS NOT INITIAL.
      MODIFY ZSDT0260 FROM WG_0260.
    ELSE.
      DELETE FROM ZSDT0260
       WHERE DOC_SIMULACAO EQ WG_0260-DOC_SIMULACAO.
    ENDIF.
  ENDIF.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

* Início - CS2019000925 - Sara Oikawa - Julho/2020
*> Ao realizar a alteração da quantidade de Sacas
*> Só fazer o disparo caso o simulador esteja aprovado.
*> O sistema deverá identificar se houve uma alteração no valor do campo "Valor Total".
*> Disparar para a tabela ZSDT0094, o valor de diferença.  Tanto para venda (VDI) quanto para Frete (FRI)
*> O valor da diferença deve levar em consideração a proporção entre os "Setor de Atividade" existentes no simulador.

  IF WG_SAVE EQ C_SACAS OR ( SY-UCOMM EQ C_RECALC AND WL_INPUT_0040-TPSIM EQ 'TT' )
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    OR ( SY-UCOMM EQ C_RECALC AND WL_INPUT_0040-TPSIM EQ 'TS' ) ""FF #174195
    OR ( SY-UCOMM EQ C_RECALC AND WL_INPUT_0040-TPSIM EQ 'TV' ). "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    IF WL_INPUT_0040-STATUS EQ 'A'.

*     DISPARO DO HEDGE PARA VENDA
      IF WL_INPUT_0040-WAERK EQ 'BRL'.

        PERFORM DISPARA_HEDGE_AJUSTE_SACAS TABLES TL_INPUT_0041
                                    USING  WL_INPUT_0040
                                           'VDI'.
      ENDIF.

*     DISPARO DO HEDGE PARA FRETE
      PERFORM DISPARA_HEDGE_AJUSTE_SACAS TABLES TL_INPUT_0041
                                  USING  WL_INPUT_0040
                                         'FRI'.

    ENDIF.
  ENDIF.

* Fim  - CS2019000925 - Sara Oikawa - Julho/2020


  IF WG_SAVE EQ C_SACAS.
    PERFORM LOG_ACAO USING 'S' '' CHANGING LV_ERRO.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK LV_ERRO IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  ENDIF.

  IF WG_SAVE EQ C_DSC_ABS.
    PERFORM ATUALIZA_ORDEM_VENDA.
  ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF WG_SAVE EQ C_ALTJUR.
    PERFORM LOG_ACAO USING 'J' '' CHANGING LV_ERRO.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK LV_ERRO IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
*                          USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.

  IF WG_SAVE EQ C_ALTADT.
    PERFORM LOG_ACAO USING 'D' '' CHANGING LV_ERRO.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK LV_ERRO IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
*                          USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  IF WG_SAVE EQ C_ALTCSA.
    PERFORM LOG_ACAO USING 'C' '' CHANGING LV_ERRO.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK LV_ERRO IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    PERFORM f_log_edicao  TABLES tl_input_0041 tl_41_old
*                          USING  wl_input_0040 wl_40_old.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF WG_SAVE EQ C_ALTCPG.
    PERFORM ZF_ALTERA_PAGTO_OV.
    DATA: LV_COMPL TYPE C LENGTH 20.
    CONCATENATE WL_40_OLD-TPSIM 'para' WL_INPUT_0040-TPSIM INTO LV_COMPL SEPARATED BY SPACE.
    PERFORM LOG_ACAO USING 'T' LV_COMPL CHANGING LV_ERRO.

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK LV_ERRO IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  PERFORM F_LOG_EDICAO  TABLES TL_INPUT_0041 TL_41_OLD
                        USING  WL_INPUT_0040 WL_40_OLD.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  MESSAGE S836(SD) WITH 'Doc. Simulação'
                         WL_INPUT_0040-DOC_SIMULACAO
                         ', criado/modificado com sucesso!'.

*  if sy-subrc is initial.
*    call function 'POPUP_TO_CONFIRM'
*      exporting
*        text_question  = text-i00
*      importing
*        answer         = wl_answer
*      exceptions
*        text_not_found = 1
*        others         = 2.
*
*    if not sy-subrc is initial.
*      message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    endif.
*
*    case wl_answer.
*      when '1'.
*        perform imprime_simulacao using wg_header-doc_simulacao.
*      when '2' or
*           'A'.
*
*    endcase.
*  endif.
  CALL FUNCTION 'DEQUEUE_EZSDT0040'
    EXPORTING
      DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

  WG_ACAO = C_ATUAL.
  CHECK SY-UCOMM NE C_RECALC.
  CHECK SY-UCOMM NE 'BTN_FUN'.
  CHECK SY-UCOMM NE 'BTN_FI'.


  PERFORM LIMPA_VARIAVEL USING C_ATUAL.
  WG_ACAO = C_ATUAL.
  S_ACAO = 'S_ATU'.
  LEAVE TO SCREEN 100.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .

  TYPES: BEGIN OF TY_LIMIT,
           SETNAME  TYPE SETNAMENEW,
           LINEID   TYPE SETLINE,
           DESCRIPT TYPE SETLINTEXT,
           DE	      TYPE SETVALMIN,
           ATE      TYPE SETVALMAX,
         END OF TY_LIMIT.

  DATA: WL_KNA1     TYPE KNA1,
        WL_MATKL    TYPE MARA-MATKL,
        WL_T001W    TYPE T001W,
        WL_0038     TYPE ZSDT0038,
        WL_LINHA(6),
        WL_DATA     TYPE ZSDT0041-DTVENC,
        TL_0036     TYPE TABLE OF ZSDT0036 WITH HEADER LINE,
        TL_0037     TYPE TABLE OF ZSDT0037 WITH HEADER LINE,
        TL_TSPA     TYPE TABLE OF TSPA WITH HEADER LINE,
        IT_LIMIT    TYPE TABLE OF TY_LIMIT,
        WL_TVKO     TYPE TVKO,
        WL_TVTW     TYPE TVTW,
        WL_0044     TYPE ZSDT0044,
        TL_TVAK     TYPE TABLE OF TVAK WITH HEADER LINE,
        WL_TCURC    TYPE TCURC,
        TL_T001W    TYPE TABLE OF T001W WITH HEADER LINE,
        TL_MARC     TYPE TABLE OF MARC WITH HEADER LINE,
        B_KNA1      TYPE KNA1,
        B_KNB1      TYPE KNB1,
        ERRO        TYPE C.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  DATA: TL_0038     TYPE TABLE OF ZSDT0038,
        WL_0038_APL TYPE ZSDT0038,
        TL_0044     TYPE TABLE OF ZSDT0044,
        WL_0044_APL TYPE ZSDT0044.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  REFRESH: TG_MSG_RET, TL_0036, TL_0037, TL_TSPA, TL_TVAK, TL_T001W,
           TL_MARC.
  CLEAR: TG_MSG_RET, WL_KNA1, WL_T001W, TL_0036, TL_0037, WL_LINHA,
         TL_TSPA, WL_TVKO, WL_TVTW, TL_TVAK, WL_TCURC, WL_DATA.

*US 143453 - INICIO - PQ
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_ZSDT0044_09'
    TABLES
      SET_VALUES    = LT_VKORG
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  LOOP AT LT_VKORG INTO W_VKORG.
    R_VKORG-SIGN   = 'I'.
    R_VKORG-OPTION = 'EQ'.
    R_VKORG-LOW    = W_VKORG-FROM.
    APPEND R_VKORG.
  ENDLOOP.

*US 143453 - INICIO - PQ


  PERFORM F_CHECK_DATE_RETROATIVA.

  SELECT SINGLE *
    FROM KNA1
    INTO WL_KNA1
     WHERE KUNNR EQ WG_HEADER-KUNNR.

  SELECT SINGLE *
    FROM T001W
    INTO WL_T001W
     WHERE WERKS EQ WG_HEADER-VKBUR.

  SELECT SINGLE *
    FROM ZSDT0038
    INTO WL_0038
     WHERE CULTURA EQ WG_HEADER-CULTURA.

  SELECT SINGLE *
    FROM ZSDT0044
    INTO WL_0044
     WHERE SAFRA EQ WG_HEADER-SAFRA.

  SELECT SINGLE *
    FROM TVKO
    INTO WL_TVKO
     WHERE VKORG = WG_HEADER-VKORG.

  SELECT SINGLE *
    FROM TVTW
    INTO WL_TVTW
     WHERE VTWEG = WG_HEADER-VTWEG.

  SELECT SINGLE *
    FROM TCURC
    INTO WL_TCURC
     WHERE WAERS EQ WG_HEADER-WAERK.


  IF TG_ITENS[] IS NOT INITIAL.
    SELECT *
      FROM ZSDT0036
      INTO TABLE TL_0036
       FOR ALL ENTRIES IN TG_ITENS
       WHERE MATNR        EQ TG_ITENS-MATNR
*         and werks eq wg_header-vkbur
         AND VAL_DE       LE SY-DATUM
         AND VAL_ATE      GE SY-DATUM
         AND CULTURA      EQ WG_HEADER-CULTURA
         AND WAERK        EQ WG_HEADER-WAERK
         AND MEINS        EQ TG_ITENS-ZIEME
         AND SAFRA        EQ WG_HEADER-SAFRA
         AND INCO1        EQ TG_ITENS-INCO1
         AND WERKS_FORNEC EQ TG_ITENS-WERKS
         AND LOEKZ        EQ SPACE
         AND ELIMINADO    EQ SPACE
         AND BUKRS        EQ WA_T001K-BUKRS.

    SELECT *
      FROM ZSDT0036
      APPENDING TABLE TL_0036
       FOR ALL ENTRIES IN TG_ITENS
       WHERE MATNR        EQ TG_ITENS-MATNR
*         and werks eq wg_header-vkbur
         AND VAL_DE       LE SY-DATUM
         AND VAL_ATE      GE SY-DATUM
         AND CULTURA      EQ SPACE
         AND WAERK        EQ WG_HEADER-WAERK
         AND MEINS        EQ TG_ITENS-ZIEME
         AND SAFRA        EQ WG_HEADER-SAFRA
         AND INCO1        EQ TG_ITENS-INCO1
         AND WERKS_FORNEC EQ TG_ITENS-WERKS
         AND LOEKZ        EQ SPACE
         AND ELIMINADO    EQ SPACE
         AND BUKRS        EQ WA_T001K-BUKRS.

    SELECT *
      FROM ZSDT0037
      INTO TABLE TL_0037
       FOR ALL ENTRIES IN TG_ITENS
       WHERE VAL_DE          LE SY-DATUM
         AND VAL_ATE         GE SY-DATUM
         AND MEINS           EQ TG_ITENS-ZIEME
         AND FILIAL_ORIGEM   EQ TG_ITENS-WERKS
         AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
         AND WAERS           EQ 'BRL'
         AND BUKRS           EQ WA_T001K-BUKRS.

    SELECT *
      FROM TVAK
      INTO TABLE TL_TVAK
      FOR ALL ENTRIES IN TG_ITENS
       WHERE AUART = TG_ITENS-AUART.

    SELECT *
      FROM TSPA
      INTO TABLE TL_TSPA
       FOR ALL ENTRIES IN TG_ITENS
       WHERE SPART = TG_ITENS-SPART.

    SELECT *
      FROM T001W
      INTO TABLE TL_T001W
       FOR ALL ENTRIES IN TG_ITENS
       WHERE WERKS EQ TG_ITENS-WERKS.

    SELECT *
    FROM MARC
    INTO TABLE TL_MARC
     FOR ALL ENTRIES IN TG_ITENS
     WHERE WERKS EQ TG_ITENS-WERKS
       AND MATNR EQ TG_ITENS-MATNR.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
    SELECT *
      FROM ZSDT0038
      INTO TABLE TL_0038
      FOR ALL ENTRIES IN TG_ITENS
      WHERE CULTURA EQ TG_ITENS-CULTURA_APL.

    SELECT *
      FROM ZSDT0044
      INTO TABLE TL_0044
      FOR ALL ENTRIES IN TG_ITENS
      WHERE SAFRA EQ TG_ITENS-SAFRA_APL.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  ENDIF.
*RSI - Case #CS0971604 Ajuste divisão frete
  IF ERRO_TAXA_FRETE IS NOT INITIAL.
    DATA: LV_MSG_ERRO_FRETE TYPE STRING.
    CONCATENATE 'Não Foi localizada a taxa de câmbio para conversão do frete para a empresa'
                ERRO_TAXA_FRETE
                '. Procure a equipe do Insumos Corporativo para atualizar o cadastro.' INTO LV_MSG_ERRO_FRETE SEPARATED BY SPACE.
    MOVE: LV_MSG_ERRO_FRETE       TO TG_MSG_RET-MSG,
          'WG_HEADER-TAXA_FRETE'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
    CLEAR: ERRO_TAXA_FRETE.
  ENDIF.
*RSI - Case #CS0971604 Ajuste divisão frete
*Início - Sara Oikawa - 38859 - Agosto/2020
*  IF WG_HEADER-WAERK NE 'BRL' AND WG_HEADER-MEIO_PAGO = 'B'.
  IF WG_HEADER-WAERK NE 'BRL' AND WG_MEIO_PAGO = 'R'.
*Fim - Sara Oikawa - 38859 - Agosto/2020
    MOVE: TEXT-E49                TO TG_MSG_RET-MSG,
         'WG_HEADER-KUNNR'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.
** Emissor da fatura (KUNNR)
  IF WG_HEADER-KUNNR IS INITIAL.
    MOVE: TEXT-E01                TO TG_MSG_RET-MSG,
          'WG_HEADER-KUNNR'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_KNA1-KUNNR IS INITIAL.
    MOVE: TEXT-E02                TO TG_MSG_RET-MSG,
          'WG_HEADER-KUNNR'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSEIF NOT WL_KNA1-KUNNR IS INITIAL.

    FREE ERRO.

*  Bloqueio Geral p/ todas empresas
    IF     NOT WL_KNA1-SPERR IS INITIAL.
      ERRO = ABAP_TRUE.
*  Bloqueio de ordem centralizado para cliente
    ELSEIF NOT WL_KNA1-AUFSD IS INITIAL.
      ERRO = ABAP_TRUE.
*  Bloqueio de remessa centralizado para cliente
    ELSEIF NOT WL_KNA1-LIFSD IS INITIAL.
      ERRO = ABAP_TRUE.
*  Bloqueio centralizado de faturamento para cliente
    ELSEIF NOT WL_KNA1-FAKSD IS INITIAL.
      ERRO = ABAP_TRUE.
*  Bloqueio de contatos central para cliente
    ELSEIF NOT WL_KNA1-CASSD IS INITIAL.
      ERRO = ABAP_TRUE.
*  Bloqueio de Pagamento
    ELSEIF NOT WL_KNA1-SPERZ IS INITIAL.
      ERRO = ABAP_TRUE.
*  Bloqueio central de eliminação para registro mestre
    ELSEIF NOT WL_KNA1-NODEL IS INITIAL.
      ERRO = ABAP_TRUE.
    ENDIF.

    IF ERRO EQ ABAP_TRUE.
      MOVE: TEXT-I02                TO TG_MSG_RET-MSG,
            'WG_HEADER-KUNNR'       TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDIF.

** Escritorio de vendas (VKBUR)
  IF WG_HEADER-VKBUR IS INITIAL.
    MOVE: TEXT-E03                TO TG_MSG_RET-MSG,
           'WG_HEADER-VKBUR'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_T001W-WERKS IS INITIAL.
    MOVE: TEXT-E04                TO TG_MSG_RET-MSG,
          'WG_HEADER-VKBUR'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

** Cultura (CULTURA)
  IF WG_HEADER-CULTURA IS INITIAL.
    MOVE: TEXT-E05                TO TG_MSG_RET-MSG,
           'WG_HEADER-CULTURA'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_0038-CULTURA IS INITIAL.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*                                  MOVE: TEXT-E06                TO TG_MSG_RET-MSG,
    MOVE: TEXT-E59               TO TG_MSG_RET-MSG,
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
        'WG_HEADER-CULTURA'      TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
  ELSE.
    IF WL_0038-PAGAMENTO IS INITIAL.
      MOVE: TEXT-E60                TO TG_MSG_RET-MSG,
        'WG_HEADER-CULTURA'      TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    " Se 'Cultura Pagamento' do cabeçalho também está habilitada para ser uma 'Cultura Aplicação'
    CLEAR WG_CULTURA_APL.
    IF WL_0038-APLICACAO IS NOT INITIAL.
      WG_CULTURA_APL = WG_HEADER-CULTURA.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

  ENDIF.

** Fazenda (FAZENDA)
  IF WG_HEADER-FAZENDA IS INITIAL.
    MOVE: TEXT-E07                TO TG_MSG_RET-MSG,
          'WG_HEADER-FAZENDA'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ENDIF.

** Vendedor (VENDEDOR)
  IF WG_HEADER-VENDEDOR IS INITIAL.
    MOVE: TEXT-E08                TO TG_MSG_RET-MSG,
          'WG_HEADER-VENDEDOR'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
  ELSE.
    IF WG_DESC_VENDEDOR IS INITIAL.
      MOVE: TEXT-E72               TO TG_MSG_RET-MSG,
            'WG_HEADER-VENDEDOR'    TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077

  ENDIF.

** Area/há (AREA_HA)
  IF WG_AREA_HA IS INITIAL.
    MOVE: TEXT-E09                TO TG_MSG_RET-MSG,
          'WG_AREA_HA'            TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ENDIF.

** Moeda (WAERK)
  IF WG_HEADER-WAERK IS INITIAL.
    MOVE: TEXT-E10                TO TG_MSG_RET-MSG,
          'WG_HEADER-WAERK'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSEIF WL_TCURC-WAERS IS INITIAL.
    MOVE: TEXT-E25                TO TG_MSG_RET-MSG,
          'WG_HEADER-WAERK'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ENDIF.

** Safra (SAFRA)
  IF WG_HEADER-SAFRA IS INITIAL.
    MOVE: TEXT-E11                TO TG_MSG_RET-MSG,
          'WG_HEADER-SAFRA'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_0044-SAFRA IS INITIAL.
    MOVE: TEXT-E34                TO TG_MSG_RET-MSG,
          'WG_HEADER-SAFRA'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

** Tipo de Ordem (TPSIM)
  IF WG_HEADER-TPSIM IS INITIAL.
    MOVE: TEXT-E12                TO TG_MSG_RET-MSG,
          'WG_HEADER-TPSIM'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.

  ELSE.

    IF WG_HEADER-TPSIM(1) EQ C_TRO(1).
*    or wg_header-tpsim(1) eq c_adt(1).
***** Data de Entrega (DTENT)
***      if wg_header-dtent is initial.
***        move: text-e37               to tg_msg_ret-msg,
***              'WG_HEADER-DTENT'      to tg_msg_ret-field.
***
***        append tg_msg_ret.
***        clear: tg_msg_ret.
***
***      endif.
***
***** Frete da Cultura (FCULT)
***      if wg_header-fcult is initial.
***        move: text-e38               to tg_msg_ret-msg,
***              'WG_HEADER-FCULT'      to tg_msg_ret-field.
***
***        append tg_msg_ret.
***        clear: tg_msg_ret.
***
***      endif.
***
***** Tipo de Cultura (TPCULT)
***      if wg_header-tpcult is initial.
***        move: text-e39                to tg_msg_ret-msg,
***              'WG_HEADER-TPCULT'      to tg_msg_ret-field.
***
***        append tg_msg_ret.
***        clear: tg_msg_ret.
***
***      endif.

    ENDIF.
  ENDIF.

  CASE WG_HEADER-TPSIM(2).
    WHEN 'VV'. "WSBB
*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF WG_HEADER-MEIO_PAGO IS INITIAL.
      IF WG_MEIO_PAGO IS INITIAL.
*Fim - Sara Oikawa - 38859 - Agosto/2020
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Meio de Pagamento'| .
        MOVE 'WG_HEADER-MEIO_PAGO'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF WG_HEADER-DTVENCOV IS INITIAL.
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Data de Vencimento OV'| .
        MOVE 'WG_HEADER-DTVENCOV'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
****Início - Sara Oikawa - 38859 - Agosto/2020
**      CLEAR WL_QTD_DIASU.
**      WL_DTLIMITE = SY-DATUM.
**
**      WHILE WL_QTD_DIASU < 3.   "(Até 3 dias uteis sem considerar a data da venda)
**
**        WL_DTLIMITE = WL_DTLIMITE + 1.
**
**        ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WL_DTLIMITE
**                                      IMPORTING E_SUBRC      = DATA(LV_SUBRC)
**                                               ).
**        IF LV_SUBRC IS NOT INITIAL.
**          WL_QTD_DIASU = WL_QTD_DIASU + 1.
**        ENDIF.
**
**      ENDWHILE.
**
**      IF WG_HEADER-DTVENCOV > WL_DTLIMITE.
**        MOVE: 'WG_HEADER-DTVENCOV'   TO TG_MSG_RET-FIELD.
**        TG_MSG_RET-MSG = TEXT-E52.
**        APPEND TG_MSG_RET.
**        CLEAR: TG_MSG_RET.
**      ENDIF.
****Fim - Sara Oikawa - 38859 - Agosto/2020
** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

*** US - 81799 - Inicio - CBRAND
      IF WG_HEADER-HBKID IS INITIAL AND WG_MEIO_PAGO = 'D'.
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Banco Empresa'| .
        MOVE 'WG_HEADER-HBKID'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*** US - 81799 - Fim - CBRAND

    WHEN 'VP'. "WSBB
*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF WG_HEADER-MEIO_PAGO IS INITIAL.
      IF WG_MEIO_PAGO IS INITIAL.
*Fim - Sara Oikawa - 38859 - Agosto/2020
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Meio de Pagamento'| .
        MOVE 'WG_HEADER-MEIO_PAGO'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF WG_HEADER-DTPGTCULT IS INITIAL.
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Data de Vencimento'| .
        MOVE 'WG_HEADER-DTPGTCULT'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*** US - 81799 - Inicio - CBRAND
      IF WG_HEADER-HBKID IS INITIAL  AND WG_MEIO_PAGO = 'D'.
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Banco Empresa'| .
        MOVE 'WG_HEADER-HBKID'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*** US - 81799 - Fim - CBRAND

    WHEN 'AD'.
      IF WG_HEADER-DTVENCOV IS INITIAL.
        TG_MSG_RET-MSG = |{ TEXT-E43 } 'Data de Vencimento OV'| .
        MOVE 'WG_HEADER-DTVENCOV'      TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*US 143453 - INICIO - PQ
      IF WG_HEADER-VKORG NOT IN R_VKORG.
        IF  WG_HEADER-ERDAT >= '20240626'.  "US 144227 - PQ
          IF WG_HEADER-DTVENCOV IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL.
            IF WG_HEADER-DTVENCOV >= WG_HEADER-DT_ENTREGA_FET.
              TG_MSG_RET-MSG = 'Data de Vencimento OV não pode ser igual ou superior a data de entrega do Fertilizante'.
              MOVE 'WG_HEADER-DTVENCOV'      TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
          IF WG_HEADER-DTVENCOV IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL.
            IF WG_HEADER-DTVENCOV >= WG_HEADER-DT_ENTREGA_SEM.
              TG_MSG_RET-MSG = 'Data de Vencimento OV não pode ser igual ou superior a data de entrega da Semente'.
              MOVE 'WG_HEADER-DTVENCOV'      TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
          IF WG_HEADER-DTVENCOV IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL.
            IF WG_HEADER-DTVENCOV >= WG_HEADER-DT_ENTREGA_DEF.
              TG_MSG_RET-MSG = 'Data de Vencimento OV não pode ser igual ou superior a data de entrega do Defensivo'.
              MOVE 'WG_HEADER-DTVENCOV'      TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


    WHEN 'TS'.
      IF WG_HEADER-ANTEC LE 0.
        CONCATENATE TEXT-E43 'Tx. De Antecipação'         INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        MOVE 'WG_HEADER-ANTEC'      TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.

      IF NOT WG_HEADER-DTPGTCULT+6(2) BETWEEN 01 AND 25.
        APPEND VALUE #(
                        MSG = 'Data Cultura fora do Limite estipulado "01/25"!'
                        FIELD = 'WG_HEADER-DTPGTCULT'
                      ) TO TG_MSG_RET.
      ENDIF.

      IF WG_HEADER-VKORG NOT IN R_VKORG.
        IF  WG_HEADER-ERDAT >= '20240626'. "US 144227 - PQ
          IF WG_HEADER-DTPGTCULT IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL.
            IF WG_HEADER-DTPGTCULT >= WG_HEADER-DT_ENTREGA_FET.
              TG_MSG_RET-MSG = 'A Dt Pagamento Milho/Soja não pode ser igual ou superior a data de entrega do Fertilizante'.
              MOVE 'WG_HEADER-DTPGTCULT'      TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
          IF WG_HEADER-DTPGTCULT IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL.
            IF WG_HEADER-DTPGTCULT >= WG_HEADER-DT_ENTREGA_SEM.
              TG_MSG_RET-MSG = 'A Dt Pagamento Milho/Soja não pode ser igual ou superior a data de entrega da Semente'.
              MOVE 'WG_HEADER-DTPGTCULT'      TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
          IF WG_HEADER-DTPGTCULT IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL.
            IF WG_HEADER-DTPGTCULT >= WG_HEADER-DT_ENTREGA_DEF.
              TG_MSG_RET-MSG = 'A Dt Pagamento Milho/Soja não pode ser igual ou superior a data de entrega do Defensivo'.
              MOVE 'WG_HEADER-DTPGTCULT'      TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    WHEN 'TV'                                               "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
      OR 'TT'.                                              "FF #174195
      IF WG_HEADER-VKORG IS NOT INITIAL.
        IF WG_HEADER-VKORG NOT IN R_VKORG.
*          tg_msg_ret-msg = | A Condição de Pagamento 'Troca a Vista' não pode ser utilizada em Vendas da Organização { wg_header-vkorg } |. "FF #174195
          TG_MSG_RET-MSG = | A Condição de Pagamento 'Troca' não pode ser utilizada em Vendas da Organização { WG_HEADER-VKORG } |. "FF #174195
          MOVE 'WG_HEADER-TPSIM'      TO TG_MSG_RET-FIELD.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.
*US 143453 - FIM - PQ
  ENDCASE.

* COMENTADO AQUI PQ A COLOQUEI VALIDADAÇÃO PARA DENTRO DO CASE ACIMA - US 143453 - INICIO - PQ
*  IF wg_header-tpsim(2) EQ 'TS'.
** Tx. De Antecipação
*    IF wg_header-antec LE 0.
*      CONCATENATE TEXT-e43 'Tx. De Antecipação'         INTO tg_msg_ret-msg SEPARATED BY space.
*      MOVE 'WG_HEADER-ANTEC'      TO tg_msg_ret-field.
*
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*
*    ENDIF.
*
*    IF NOT wg_header-dtpgtcult+6(2) BETWEEN 01 AND 25.
*      APPEND VALUE #(
*                      msg = 'Data Cultura fora do Limite estipulado "01/25"!'
*                      field = 'WG_HEADER-DTPGTCULT'
*                    ) TO tg_msg_ret.
*    ENDIF.
*  ENDIF.
* COMENTADO AQUI PQ A COLOQUEI VALIDADAÇÃO PARA DENTRO DO CASE ACIMA - US 143453 - FIM - PQ

  IF NOT WG_HEADER-DTENT IS INITIAL.
*    ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DTENT
*                                    IMPORTING E_SUBRC    = DATA(S_SUBRC)
*                                ).
*    IF S_SUBRC IS INITIAL.
*      MOVE: 'WG_HEADER-DTENT'   TO TG_MSG_RET-FIELD.
*      TG_MSG_RET-MSG = | Data de Entrega não é dia Útil!|.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.

*    IF wg_header-dtent < sy-datum.
*      MOVE: 'WG_HEADER-DTENT'   TO tg_msg_ret-field.
*      tg_msg_ret-msg = | Data de Entrega não pode ser retroativa!|.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

  ENDIF.

  DATA(DATA_LIMITE) = SY-DATUM + 730.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF WG_SAVE NE C_ALTCPG.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

    IF NOT WG_HEADER-DT_ENTREGA_FET IS INITIAL.
      ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DT_ENTREGA_FET
                                    IMPORTING E_SUBRC      = DATA(S_SUBRC)
                                             ).
      IF S_SUBRC IS INITIAL.
        MOVE: 'WG_HEADER-DT_ENTREGA_FET'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data de Entrega de Fertilizante não é dia Útil!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*      IF wg_header-dt_entrega_fet < sy-datum.
*        MOVE: 'WG_HEADER-DT_ENTREGA_FET'   TO tg_msg_ret-field.
*        tg_msg_ret-msg = | Data de Entrega de Fertilizante não pode ser retroativa!|.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.

      IF WG_HEADER-DT_ENTREGA_FET > DATA_LIMITE.
        MOVE: 'WG_HEADER-DT_ENTREGA_FET'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data de Entrega de Fertilizante não pode ser superior a DOIS anos!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.

    IF NOT WG_HEADER-DT_ENTREGA_SEM IS INITIAL.
      ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DT_ENTREGA_SEM
                                    IMPORTING E_SUBRC      = S_SUBRC
                                             ).
      IF S_SUBRC IS INITIAL.
        MOVE: 'WG_HEADER-DT_ENTREGA_SEM'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data de Entrega da Semente não é dia Útil!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*      IF wg_header-dt_entrega_sem < sy-datum.
*        MOVE: 'WG_HEADER-DT_ENTREGA_SEM'   TO tg_msg_ret-field.
*        tg_msg_ret-msg = | Data de Entrega de Semente não pode ser retroativa!|.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.

      IF WG_HEADER-DT_ENTREGA_SEM > DATA_LIMITE.
        MOVE: 'WG_HEADER-DT_ENTREGA_SEM'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data de Entrega de Semente não pode ser superior a DOIS anos!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.


    IF NOT WG_HEADER-DT_ENTREGA_DEF IS INITIAL.
      ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DT_ENTREGA_DEF
                                    IMPORTING E_SUBRC      = S_SUBRC
                                             ).
      IF S_SUBRC IS INITIAL.
        MOVE: 'WG_HEADER-DT_ENTREGA_DEF'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data de Entrega do Defensivos não é dia Útil!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*      IF wg_header-dt_entrega_def < sy-datum.
*        MOVE: 'WG_HEADER-DT_ENTREGA_DEF'   TO tg_msg_ret-field.
*        tg_msg_ret-msg = | Data de Entrega de Defensivos não pode ser retroativa!|.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.

      IF WG_HEADER-DT_ENTREGA_DEF > DATA_LIMITE.
        MOVE: 'WG_HEADER-DT_ENTREGA_DEF'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data de Entrega de Defensivos não pode ser superior a DOIS anos!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  ENDIF.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  IF NOT WG_HEADER-DTPGTCULT IS INITIAL.
    ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DTPGTCULT
                                  IMPORTING E_SUBRC      = S_SUBRC
                                           ).
    IF S_SUBRC IS INITIAL.
      MOVE: 'WG_HEADER-DTPGTCULT'   TO TG_MSG_RET-FIELD.
      TG_MSG_RET-MSG = | Data Informada Vencimento SJ/ML não é Dia Útil!|.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
*
*    IF wg_header-dtpgtcult < sy-datum.
*      MOVE: 'WG_HEADER-DTPGTCULT'   TO tg_msg_ret-field.
*      tg_msg_ret-msg = | Data Informada Vencimento SJ/ML não pode ser retroativa!|.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

  ENDIF.

  IF NOT WG_HEADER-DTVENCOV IS INITIAL.
    ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DTVENCOV
                                  IMPORTING E_SUBRC      = S_SUBRC
                                           ).
    IF S_SUBRC IS INITIAL.
      MOVE: 'WG_HEADER-DTVENCOV'   TO TG_MSG_RET-FIELD.
      TG_MSG_RET-MSG = | Data Informada no Venc OV não é Dia Útil!|.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

*    IF wg_header-dtvencov < sy-datum.
*      MOVE: 'WG_HEADER-DTVENCOV'   TO tg_msg_ret-field.
*      tg_msg_ret-msg = | Data Informada no Venc OV não pode ser retroativa!|.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

    IF WG_HEADER-DTVENCOV+6(2) > 25  .
      IF ( WG_HEADER-TPSIM(2) NE 'VV' ) AND ( WG_HEADER-TPSIM(2) NE 'VP' ).
        MOVE: 'WG_HEADER-DTVENCOV'   TO TG_MSG_RET-FIELD.
        TG_MSG_RET-MSG = | Data do Venc OV não pode ser superior ao dia 25 de qualquer mês!|.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

  ENDIF.

  IF NOT WG_HEADER-DTINIJUROS IS INITIAL.
    ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = WG_HEADER-DTINIJUROS
                                  IMPORTING E_SUBRC      = S_SUBRC
                                           ).
    IF S_SUBRC IS INITIAL.
      MOVE: 'WG_HEADER-DTINIJUROS'   TO TG_MSG_RET-FIELD.
      TG_MSG_RET-MSG = | Data Inicial de Juros Informada não é Dia Útil!|.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

** Dt.Pagamento da Cultura (DTPGTCULT)
***  IF wg_header-dtpgtcult IS INITIAL AND wg_header-tpsim(2) NE 'VF'
***                                    AND wg_header-tpsim(2) NE 'BN'
***                                    AND wg_header-tpsim(2) NE 'VV'.
***    MOVE: "TEXT-E39                TO TG_MSG_RET-MSG,
***          'WG_HEADER-DTPGTCULT'   TO tg_msg_ret-field.
***    CONCATENATE TEXT-e43 ' " ' wg_desc_dat' "' INTO tg_msg_ret-msg.
***    APPEND tg_msg_ret.
***    CLEAR: tg_msg_ret.
***
***  ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF WG_HEADER-DTINIJUROS IS NOT INITIAL AND WG_HEADER-DTPGTCULT IS NOT INITIAL
     AND  WG_HEADER-TPSIM(2) EQ 'VP'.
    IF WG_HEADER-DTINIJUROS < WG_HEADER-DTPGTCULT.
      MOVE: 'WG_HEADER-DTINIJUROS'   TO TG_MSG_RET-FIELD.
      TG_MSG_RET-MSG = TEXT-E51.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

  IF WG_HEADER-KURSF IS INITIAL AND WG_HEADER-TPSIM(2) EQ 'VF'.
    MOVE: "TEXT-E43                TO TG_MSG_RET-MSG,
          'WG_HEADER-KURSF'   TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E43 ' Taxa de Câmbio' INTO TG_MSG_RET-MSG.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_HEADER-TPSIM EQ 'VP'.
    IF  WG_HEADER-DTINIJUROS IS INITIAL.
      MOVE: "TEXT-E39                TO TG_MSG_RET-MSG,
         'WG_HEADER-DTINIJUROS'   TO TG_MSG_RET-FIELD.
      CONCATENATE TEXT-E43 ' "Data Inicio Juros"' INTO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

** Organização vendas (VKORG)
  IF WG_HEADER-VKORG IS INITIAL.
    MOVE: TEXT-E18                TO TG_MSG_RET-MSG,
          'WG_HEADER-VKORG'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_TVKO-VKORG IS INITIAL.
    MOVE: TEXT-E19                TO TG_MSG_RET-MSG,
          'WG_HEADER-VKORG'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

** Canal de distribuição (VTWEG)
  IF WG_HEADER-VTWEG IS INITIAL.
    MOVE: TEXT-E20                TO TG_MSG_RET-MSG,
          'WG_HEADER-VTWEG'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_TVTW-VTWEG IS INITIAL.
    MOVE: TEXT-E21                TO TG_MSG_RET-MSG,
          'WG_HEADER-VTWEG'       TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.



  IF WG_HEADER-TPSIM(1) EQ C_TRO(1).
** Preço Sc. (Bruto) (PREC_CULT)
    IF WG_HEADER-PREC_ANT_CULT IS INITIAL.
      MOVE: TEXT-E26                  TO TG_MSG_RET-MSG,
            'WG_HEADER-PREC_ANT_CULT' TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    IF WG_0260-ID_COMPRA IS NOT INITIAL.
      PERFORM ZF_VALIDA_COMPRA_SIGAM USING 'X'
                                     CHANGING TG_MSG_RET-MSG.
      IF TG_MSG_RET-MSG IS NOT INITIAL.
        MOVE  'WG_0260-ID_COMPRA'   TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6


  ELSEIF WG_HEADER-TPSIM(1) EQ C_ADT(1).
** Juros ao Ano(%) (JUROS_ANO)
    IF WG_HEADER-JUROS_ANO IS INITIAL.
      MOVE: TEXT-E27                TO TG_MSG_RET-MSG,
            'WG_HEADER-JUROS_ANO'   TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio

** Vlr.Adiantado por Sc. (VLR_ADTO)
    IF WG_HEADER-VLR_ADTO IS INITIAL.
      MOVE: TEXT-E28                TO TG_MSG_RET-MSG,
            'WG_HEADER-VLR_ADTO'   TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
** Adiantamento P/Hectare (ADTO_HA)
    IF WG_HEADER-ADTO_HA IS INITIAL.
      MOVE: TEXT-E29                TO TG_MSG_RET-MSG,
            'WG_HEADER-ADTO_HA'   TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
  ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF WG_HEADER-TPSIM(1) EQ  'V' OR
     WG_HEADER-TPSIM    EQ  'PM'.
** Juros ao Ano(%) (JUROS_ANO)
    IF WG_HEADER-JUROS_ANO IS INITIAL.
      MOVE: TEXT-E27                TO TG_MSG_RET-MSG,
            'WG_HEADER-JUROS_ANO'   TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020


  READ TABLE TG_ITENS WITH KEY SPART = '02'.
  IF SY-SUBRC IS INITIAL.
    IF WG_HEADER-DT_ENTREGA_FET IS INITIAL.
      TG_MSG_RET-MSG = |{ TEXT-I01 } da Data de Fertilizante |.
      TG_MSG_RET-FIELD = 'WG_HEADER-DT_ENTREGA_FET'.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  READ TABLE TG_ITENS WITH KEY SPART = '03'.
  IF SY-SUBRC IS INITIAL.
    IF WG_HEADER-DT_ENTREGA_DEF IS INITIAL.
      TG_MSG_RET-MSG = |{ TEXT-I01 } da Data de Defensivos |.
      TG_MSG_RET-FIELD = 'WG_HEADER-DT_ENTREGA_DEF'.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  READ TABLE TG_ITENS WITH KEY SPART = '04'.
  IF SY-SUBRC IS INITIAL.
    IF WG_HEADER-DT_ENTREGA_SEM IS INITIAL.
      TG_MSG_RET-MSG = |{ TEXT-I01 } da Data de Sementes |.
      TG_MSG_RET-FIELD = 'WG_HEADER-DT_ENTREGA_SEM'.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.
  "165455 - PQ 24.02.25
  READ TABLE TG_ITENS WITH KEY SPART = '12'.
  IF SY-SUBRC IS INITIAL.
    IF WG_HEADER-DT_ENTREGA_FET IS INITIAL.
      TG_MSG_RET-MSG = |{ TEXT-I01 } da Data de Fertilizante |.
      TG_MSG_RET-FIELD = 'WG_HEADER-DT_ENTREGA_FET'.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  READ TABLE TG_ITENS WITH KEY SPART = '13'.
  IF SY-SUBRC IS INITIAL.
    IF WG_HEADER-DT_ENTREGA_DEF IS INITIAL.
      TG_MSG_RET-MSG = |{ TEXT-I01 } da Data de Defensivos |.
      TG_MSG_RET-FIELD = 'WG_HEADER-DT_ENTREGA_DEF'.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.
  "165455 - PQ 24.02.25

*  if wg_header-venci is initial.
*    move: 'VENCI'  to tg_msg_ret-field,
*          'GRID1'  to tg_msg_ret-obj,
*          wl_linha to tg_msg_ret-tabix.
*
*    concatenate text-e16 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*    append tg_msg_ret.
*    clear: tg_msg_ret.
*  endif.

  IF TG_ITENS[] IS INITIAL.
    MOVE: TEXT-E13                TO TG_MSG_RET-MSG.
*      'WG_HEADER-TPSIM'       to tg_msg_ret-field.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  SORT: TL_0036  BY MATNR WAERK MEINS SAFRA INCO1 WERKS_FORNEC,
        TL_TVAK  BY AUART,
        TL_TSPA  BY SPART,
        TL_T001W BY WERKS,
        TL_MARC  BY WERKS MATNR.

  TOTAL_SAC = 0.
  SUM_VLRTOT = 0.

  LOOP AT TG_ITENS.
    WL_LINHA = SY-TABIX.

    READ TABLE TL_0036
      WITH KEY MATNR = TG_ITENS-MATNR
               WAERK = WG_HEADER-WAERK
               MEINS = TG_ITENS-ZIEME
               SAFRA = WG_HEADER-SAFRA
               INCO1 = TG_ITENS-INCO1
               WERKS_FORNEC = TG_ITENS-WERKS
               BINARY SEARCH.

    IF SY-SUBRC IS NOT INITIAL.
      MOVE: 'MATNR'  TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E14 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSE.
      IF TG_ITENS-ZMENG IS INITIAL.
        MOVE: 'ZMENG'  TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E15 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
    "FF #174195 - inicio // preenchido automaticamente, validação não é necessária.
** Tipo de documento (AUART)
*    CLEAR: tl_tvak.
*    READ TABLE tl_tvak
*      WITH KEY auart = tg_itens-auart
*               BINARY SEARCH.

*    IF tg_itens-auart IS INITIAL.

*      MOVE: 'AUART'  TO tg_msg_ret-field,
*            'GRID1'  TO tg_msg_ret-obj,
*            wl_linha TO tg_msg_ret-tabix.
*
*      CONCATENATE TEXT-e17 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.

*    ELSEIF tl_tvak-auart IS INITIAL.
*      MOVE: 'AUART'  TO tg_msg_ret-field,
*            'GRID1'  TO tg_msg_ret-obj,
*            wl_linha TO tg_msg_ret-tabix.
*
*      CONCATENATE TEXT-e24 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*
*    ENDIF.

*** Setor de atividade (SPART)
*    CLEAR: tl_tspa.
*    READ TABLE tl_tspa
*     WITH KEY spart = tg_itens-spart
*              BINARY SEARCH.
*
*    IF tg_itens-spart IS INITIAL.
*      MOVE: 'SPART'  TO tg_msg_ret-field,
*            'GRID1'  TO tg_msg_ret-obj,
*            wl_linha TO tg_msg_ret-tabix.
*
*      CONCATENATE TEXT-e22 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.

*    ELSEIF tl_tspa-spart IS INITIAL.
*      MOVE: 'SPART'  TO tg_msg_ret-field,
*            'GRID1'  TO tg_msg_ret-obj,
*            wl_linha TO tg_msg_ret-tabix.
*
*      CONCATENATE TEXT-e23 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
    "FF #174195 - fim

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
* Quantidade Embalagem (para Defensivos)

    IF TG_ITENS-SPART EQ '03'.
      IF NOT TG_ITENS-MATNR IS INITIAL.

        PERFORM ZF_BUSCA_QTD_EMBALAGEM_ZDEF.

        IF WG_GROES_STRING EQ C_X.
          MOVE: 'ZMENG'      TO TG_MSG_RET-FIELD,
                'GRID1'      TO TG_MSG_RET-OBJ,
                 WL_LINHA    TO TG_MSG_RET-TABIX.
          " 'A quantidade da embalagem do material XXXXX informada no cadastro mestre, deve ser numérica.'
          CONCATENATE TEXT-E61 WG_MATNR TEXT-E62 ' LINHA: ' WL_LINHA
                 INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.

        ELSE.
          IF  WG_GROES_DEC IS INITIAL.
            MOVE: 'ZMENG'      TO TG_MSG_RET-FIELD,
                  'GRID1'      TO TG_MSG_RET-OBJ,
                   WL_LINHA    TO TG_MSG_RET-TABIX.
            " 'O Cadastro do Material  XXXXXX  está sem a quantidade da embalagem no cadastro mestre.'
            CONCATENATE TEXT-E63 WG_MATNR TEXT-E64 ' LINHA: ' WL_LINHA
                   INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.

          ELSE.
            WG_QTD_PROP = FRAC( TG_ITENS-ZMENG / WG_GROES_DEC ).
            IF WG_QTD_PROP IS NOT INITIAL.
              MOVE: 'ZMENG'   TO TG_MSG_RET-FIELD,
                    'GRID1'   TO TG_MSG_RET-OBJ,
                    WL_LINHA  TO TG_MSG_RET-TABIX.
              " 'A quantidade informada para o item, não é proporcional à quantidade de XXXXX XX da embalagem do material.'
              CONCATENATE TEXT-E65 WG_GROES WG_MEINS TEXT-E66 ' LINHA: ' WL_LINHA
                     INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

* Validar Cultura e Safra Aplicação

    IF TG_ITENS-CULTURA_APL IS NOT INITIAL.

      CLEAR WL_0038_APL.
      READ TABLE TL_0038 INTO WL_0038_APL WITH KEY CULTURA = TG_ITENS-CULTURA_APL.

      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'CULTURA_APL'  TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
               WL_LINHA TO TG_MSG_RET-TABIX.
        "Cultura de Aplicação informada não existe.
        CONCATENATE TEXT-E67 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ELSEIF WL_0038_APL-APLICACAO IS INITIAL.
        MOVE: 'CULTURA_APL'  TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
               WL_LINHA TO TG_MSG_RET-TABIX.
        "Cultura de Aplicação informada não habilitada.
        CONCATENATE TEXT-E68 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF TG_ITENS-SAFRA_APL IS NOT INITIAL.

      CLEAR WL_0044_APL.
      READ TABLE TL_0044 INTO WL_0044_APL WITH KEY SAFRA = TG_ITENS-SAFRA_APL.

      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'SAFRA_APL'  TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
               WL_LINHA TO TG_MSG_RET-TABIX.
        "Safra de Aplicação não é válida.
        CONCATENATE TEXT-E69 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4


** Data Vencimento (DTVENC)
    WL_DATA = TL_0036-DTVENC.
    ADD 92 TO WL_DATA.
    IF TG_ITENS-DTVENC IS INITIAL.
      MOVE: 'DTVENC'  TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E40 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.

*      ZCL_SOLICITACAO_OV=>DIA_UTIL( EXPORTING P_VENCIMENTO = TG_ITENS-DTVENC
*                                            IMPORTING E_SUBRC  = S_SUBRC
*                                      ).
*
*      IF S_SUBRC IS INITIAL.
*        MOVE: 'DTVENC'  TO TG_MSG_RET-FIELD,
*              'GRID1'  TO TG_MSG_RET-OBJ,
*              WL_LINHA TO TG_MSG_RET-TABIX.
*        TG_MSG_RET-MSG = |Data de Vencimento do Iten não é dia Útil! LINHA { WL_LINHA }.|.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.

      IF TG_ITENS-DTVENC GT WL_DATA.
        MOVE: 'DTVENC'  TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E41 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.

******* Centro fornecedor (WERKS)
*****    clear: tl_t001w.
*****    read table tl_t001w
*****     with key werks = tg_itens-werks
*****              binary search.
*****
*****    if tg_itens-werks is initial.
*****      move: 'WERKS'  to tg_msg_ret-field,
*****            'GRID1'  to tg_msg_ret-obj,
*****            wl_linha to tg_msg_ret-tabix.
*****
*****      concatenate text-e30 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*****      append tg_msg_ret.
*****      clear: tg_msg_ret.
*****    elseif tl_t001w-werks is initial.
*****      move: 'WERKS'  to tg_msg_ret-field,
*****            'GRID1'  to tg_msg_ret-obj,
*****            wl_linha to tg_msg_ret-tabix.
*****
*****      concatenate text-e31 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*****      append tg_msg_ret.
*****      clear: tg_msg_ret.
*****    endif.

*** Verifica se o material esta expandido para o centro
*    clear: tl_marc.
*    read table tl_marc
*     with key werks = tg_itens-werks
*              matnr = tg_itens-matnr
*              binary search.
*
*    if sy-subrc is not initial.
*      move: 'MATNR'  to tg_msg_ret-field,
*            'GRID1'  to tg_msg_ret-obj,
*            wl_linha to tg_msg_ret-tabix.
*
*      concatenate text-e32 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*      append tg_msg_ret.
*      clear: tg_msg_ret.
*    endif.

    SELECT SINGLE MATKL
    FROM MARA
    INTO WL_MATKL
    WHERE MATNR EQ TG_ITENS-MATNR.

    IF TG_ITENS-INCO1 IS INITIAL.
      MOVE: 'INCO1' TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E33 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSEIF TG_ITENS-INCO1 EQ 'CIF'.

      READ TABLE TL_0037
            WITH KEY FILIAL_ORIGEM  = TG_ITENS-WERKS
                     MEINS          = TG_ITENS-ZIEME
                     FILIAL_DESTINO = WG_HEADER-VKBUR
                     WAERS          = 'BRL'
                     MATKL          = WL_MATKL.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'INCO1' TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E35 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.

*    ELSEIF TG_ITENS-INCO1 EQ 'CPT'.
*
*      IF WL_MATKL EQ '658445'.
*        READ TABLE TL_0037
*         WITH KEY FILIAL_ORIGEM  = TG_ITENS-WERKS
*           MEINS          = TG_ITENS-ZIEME
*           FILIAL_DESTINO = WG_HEADER-VKBUR
*           WAERS          = 'BRL'
*           MATKL          = WL_MATKL.
*
*        IF SY-SUBRC IS NOT INITIAL.
*          MOVE: 'INCO1' TO TG_MSG_RET-FIELD,
*                'GRID1'  TO TG_MSG_RET-OBJ,
*                WL_LINHA TO TG_MSG_RET-TABIX.
*
*          CONCATENATE TEXT-E35 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.
*          CLEAR: TG_MSG_RET.
*
*        ENDIF.
*      ENDIF.
    ENDIF.

    IF TG_ITENS-ZWERT IS INITIAL.
      MOVE: 'ZWERT'  TO TG_MSG_RET-FIELD,
      'GRID1'  TO TG_MSG_RET-OBJ,
      WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE 'É obrigatório o preenchimento do campo "Vlr negociado".' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

* Valida os campos AUART da grid1 verificando se é igual ao TPSIM ZFUT e ZBON
    CASE WG_HEADER-TPSIM(2).
      WHEN 'VF'.
        IF TG_ITENS-AUART NE 'ZFUT'.
          MOVE: 'AUART'  TO TG_MSG_RET-FIELD,
                'GRID1'  TO TG_MSG_RET-OBJ,
                WL_LINHA TO TG_MSG_RET-TABIX.

          CONCATENATE 'Tipo da Ordem de Venda diferente de "ZFUT".' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

      WHEN 'BN'.
        IF TG_ITENS-AUART NE 'ZBON'.
          MOVE: 'AUART'  TO TG_MSG_RET-FIELD,
                'GRID1'  TO TG_MSG_RET-OBJ,
                WL_LINHA TO TG_MSG_RET-TABIX.

          CONCATENATE 'Tipo da Ordem de Venda diferente de "ZBON".' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
*Início - Sara Oikawa - 38859 - Agosto/2020
      WHEN OTHERS.
        IF TG_ITENS-AUART EQ 'ZFUT'.
          MOVE: 'AUART'  TO TG_MSG_RET-FIELD,
                'GRID1'  TO TG_MSG_RET-OBJ,
                WL_LINHA TO TG_MSG_RET-TABIX.

          CONCATENATE 'Tipo da Ordem de Venda deve ser diferente de "ZFUT".' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020
    ENDCASE.


    IF TG_ITENS-VLR_FRETE IS INITIAL.
      IF 'CIF' EQ TG_ITENS-INCO1.


        MOVE: 'VLR_FRETE'  TO TG_MSG_RET-FIELD,
                  'GRID1'  TO TG_MSG_RET-OBJ,
                  WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE 'Frete não agregado ao Valor do produto.  LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

*      IF 'CPT' EQ TG_ITENS-INCO1 AND TG_ITENS-SPART EQ '03'.
*
*        MOVE: 'VLR_FRETE'  TO TG_MSG_RET-FIELD,
*              'GRID1'  TO TG_MSG_RET-OBJ,
*              WL_LINHA TO TG_MSG_RET-TABIX.
*
*        TG_MSG_RET-MSG = | 'Não Localizado o Valor do Frete. Necessário para o lançamento do HEDGE.  LINHA: { WL_LINHA } |.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
    ENDIF.

*    Estabelece um Teto Maximo para variação de correção do Troca Total/sac
    ADD TG_ITENS-TRTOT TO TOTAL_SAC.
    ADD TG_ITENS-VLRTOT TO SUM_VLRTOT.

  ENDLOOP.

  LOOP AT TG_TRANS ASSIGNING FIELD-SYMBOL(<W90>)
  WHERE CATEGORIA EQ 'O'
  AND ESTORNO EQ ABAP_FALSE
  AND FLAG EQ ABAP_TRUE.
    ADD <W90>-DESC_ABSOLUTO TO SUM_VLRTOT.
  ENDLOOP.

  IF NOT LINE_EXISTS( TG_ITENS[ SPART = '04' ] ).
    FREE WG_HEADER-DT_ENTREGA_SEM.
  ENDIF.

  IF NOT LINE_EXISTS( TG_ITENS[ SPART = '03' ] ) AND
     NOT LINE_EXISTS( TG_ITENS[ SPART = '13' ] ). "165455 - PQ 24.02.25
    FREE WG_HEADER-DT_ENTREGA_DEF.
  ENDIF.

  IF NOT LINE_EXISTS( TG_ITENS[ SPART = '02' ] ) AND
     NOT LINE_EXISTS( TG_ITENS[ SPART = '12' ] ). "165455 - PQ 24.02.25
    FREE WG_HEADER-DT_ENTREGA_FET.
  ENDIF.

  "Verifica se o Valor total dos Itens + o desconto absoluto que foram agregado é DIFERENTE do valor total do Simulador.
  IF SUM_VLRTOT NE WG_HEADER-VLRTOT.
    MOVE: TEXT-E45            TO TG_MSG_RET-MSG,
          'WG_HEADER-VLRTOT'  TO TG_MSG_RET-FIELD.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*WG_HEADER-CULTURA
* Set para limitar a quantidade do Total Saca ML SJ AL
  IF NOT CONVERT_TRATOTSC IS INITIAL AND WG_HEADER-CULTURA IS NOT INITIAL.
    TOTAL_SAC = TOTAL_SAC - CONVERT_TRATOTSC.

    SELECT A~SETNAME A~LINEID A~DESCRIPT B~VALTO B~VALFROM
      FROM SETLINET AS A
      INNER JOIN SETLEAF AS B ON B~SETNAME EQ A~SETNAME AND
                                 B~LINEID  EQ A~LINEID
      INTO TABLE IT_LIMIT
       WHERE A~SETNAME EQ 'MAGGI_ZSDT0044_07'
         AND A~LANGU   EQ SY-LANGU
         AND A~DESCRIPT EQ WG_HEADER-CULTURA.

    SORT IT_LIMIT BY DESCRIPT.
    DELETE ADJACENT DUPLICATES FROM IT_LIMIT COMPARING DESCRIPT.

    IF IT_LIMIT IS NOT INITIAL.
      IF TOTAL_SAC NOT BETWEEN IT_LIMIT[ 1 ]-DE AND IT_LIMIT[ 1 ]-ATE.

        MOVE: TEXT-E44            TO TG_MSG_RET-MSG,
              'CONVERT_TRATOTSC'  TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ELSE.
      MESSAGE 'Limite não Cadastrado para a Cultura! Set "MAGGI_ZSDT0044_07"' TYPE 'S'.
    ENDIF.
  ENDIF.

*  Inicio Bloqueio cliente com restrições CS2016000357 WB
  IF NOT WG_HEADER-KUNNR IS INITIAL.

    SELECT SINGLE *
      FROM KNA1
        INTO B_KNA1
          WHERE KUNNR EQ WG_HEADER-KUNNR.

*  Bloqueio Geral p/ todas empresas
    IF     NOT B_KNA1-SPERR IS INITIAL.
      MOVE: TEXT-001            TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

*  Bloqueio de ordem centralizado para cliente
    ELSEIF NOT B_KNA1-AUFSD IS INITIAL.
      MOVE: TEXT-001 TO TG_MSG_RET-MSG. APPEND TG_MSG_RET. CLEAR: TG_MSG_RET.
*  Bloqueio de remessa centralizado para cliente
    ELSEIF NOT B_KNA1-LIFSD IS INITIAL.
      MOVE: TEXT-001 TO TG_MSG_RET-MSG. APPEND TG_MSG_RET. CLEAR: TG_MSG_RET.
*  Bloqueio centralizado de faturamento para cliente
    ELSEIF NOT B_KNA1-FAKSD IS INITIAL.
      MOVE: TEXT-001 TO TG_MSG_RET-MSG. APPEND TG_MSG_RET. CLEAR: TG_MSG_RET.
*  Bloqueio de contatos central para cliente
    ELSEIF NOT B_KNA1-CASSD IS INITIAL.
      MOVE: TEXT-001 TO TG_MSG_RET-MSG. APPEND TG_MSG_RET. CLEAR: TG_MSG_RET.
    ENDIF.

    SELECT SINGLE *
      FROM KNB1
        INTO B_KNB1
          WHERE KUNNR EQ WG_HEADER-KUNNR
            AND BUKRS EQ WG_HEADER-VKORG.

*  Bloqueio Especifico parea uma empresas
    IF     NOT B_KNB1-SPERR IS INITIAL.
      MOVE: TEXT-001 TO TG_MSG_RET-MSG. APPEND TG_MSG_RET. CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.
*    Fim Bloqueio cliente com restrições CS2016000357 WB

  DATA(CONT) = REDUCE INT4( INIT X = 0 FOR LS IN TG_ITENS WHERE ( VBELN IS NOT INITIAL ) NEXT X = X + 1 ).

  IF CONT IS INITIAL.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV'   "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    OR WG_HEADER-TPSIM EQ 'TT'.                             "FF #174195
      IF WG_ACAO NE 'MODIF' AND WG_ACAO NE 'ADD'.
        PERFORM VLR_AJUSTE.
        IF VLR_TOTAL NE VTA_SISTPRO.
          APPEND VALUE #( MSG = TEXT-E50 ) TO TG_MSG_RET.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*    Busca erro de imposto
  IF SY-UCOMM EQ C_APROV.
    PERFORM VERIFICA_ERROS_IMPOSTOS.
    IF CONT IS INITIAL.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
      IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV' "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
      OR WG_HEADER-TPSIM EQ 'TT'.                           "FF #174195
        PERFORM VLR_AJUSTE.
        IF VLR_TOTAL NE VTA_SISTPRO.
          APPEND VALUE #( MSG = TEXT-E50 ) TO TG_MSG_RET.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
  " 21.02.2023 - RAMON - 102323 -->
***  IF wg_header-ecommerce = 'X'.
***
***    DATA lv_msgn TYPE string.
***
***    IF wg_header-tpsim NE 'VV' AND wg_header-tpsim NE 'BN'.
***
***      CLEAR: tg_msg_ret.
***
***      IF wg_header-tpsim IS INITIAL.
***        lv_msgn = `Para Vendas E-commerce a ''Condição de Pagamento'' informada não é permitida`.
***      ELSE.
***        lv_msgn = `Para Vendas E-commerce a (` && wg_header-tpsim && `) informada não é permitida!`.
***      ENDIF.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***    IF wg_header-id_order_ecommerce IS INITIAL.
***      lv_msgn = `Para Vendas E-commerce o ''Cód. Pedido E-commerce'' deve ser informado`.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***    IF wg_header-id_order_ecommerce CO '0'.
***      lv_msgn = `''Cód. Pedido E-commerce'' deve ser diferente de ZERO`.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***    IF wg_header-vendedor <> 'E00'.
***
***      CLEAR: tg_msg_ret.
***
***      lv_msgn = `Para Vendas E-commerce código do Vendedor deve ser 'E00 - E-COMMERCE'`.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***    IF wg_header-waerk <> 'BRL'.
***
***      CLEAR: tg_msg_ret.
***
***      lv_msgn = `Para Vendas E-commerce a Moeda deve ser BRL`.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***    IF wg_header-hbkid IS NOT INITIAL AND ( wg_header-hbkid NE 'AL5' AND wg_header-hbkid NE 'ITAU3' ). "// US-163166 WBARBOSA 13/01/2025 incluido o ITAU3
***
***      CLEAR: tg_msg_ret.
***
***      lv_msgn = `Para Vendas E-commerce o banco empresa deve ser AL5`.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***    IF wg_header-hbkid IS INITIAL .
***
***      CLEAR: tg_msg_ret.
***
***      lv_msgn = 'Preencher Banco Empresa'.
***
***      tg_msg_ret-field = 'WG_HEADER-HBKID'.
***
***      MOVE lv_msgn TO tg_msg_ret-msg. APPEND tg_msg_ret.
***
***    ENDIF.
***
***  ENDIF.
  " 21.02.2023 - RAMON - 102323 --<

  DATA LV_MSGN TYPE STRING.
  LOOP AT TG_ITENS WHERE CULTURA_APL IS INITIAL .
    CLEAR: TG_MSG_RET.
    LV_MSGN = 'Preencher Cultura Aplicação'.
    TG_MSG_RET-FIELD = 'WG_HEADER-CULTURA'.
    MOVE LV_MSGN TO TG_MSG_RET-MSG. APPEND TG_MSG_RET.
    EXIT.
  ENDLOOP.

  LOOP AT TG_ITENS WHERE SAFRA_APL IS INITIAL .
    CLEAR: TG_MSG_RET.
    LV_MSGN = 'Preencher Safra Pagto Aplicação'.
    TG_MSG_RET-FIELD = 'WG_HEADER-SAFRA'.
    MOVE LV_MSGN TO TG_MSG_RET-MSG. APPEND TG_MSG_RET.
    EXIT.
  ENDLOOP.


  CASE WG_HEADER-TPSIM.
    WHEN 'TS' OR 'TV'.

      IF SY-UCOMM EQ C_ADD.
        APPEND
        VALUE #(
                  MSG   = 'Condição de Pagamento Obsoleto!'
                  FIELD = 'WG_HEADER-TPSIM'
                ) TO TG_MSG_RET.
      ENDIF.

      SELECT SINGLE COUNT(*)
        FROM ZSDT0040
        INTO @DATA(LV_COUNT)
        WHERE DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO
          AND TPSIM IN ('TS', 'TV')
          AND ERDAT LE '20250701'.

      IF SY-SUBRC IS NOT INITIAL.
        APPEND
        VALUE #(
                  MSG   = 'Condição de Pagamento Obsoleto!'
                  FIELD = 'WG_HEADER-TPSIM'
                ) TO TG_MSG_RET.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

  PERFORM F_VALIDA_INTINERARIO.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim


  SORT TG_MSG_RET.
  DELETE ADJACENT DUPLICATES FROM TG_MSG_RET COMPARING ALL FIELDS.

ENDFORM.                    " VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  DATA: WL_TVBVK TYPE TVBVK,
        WL_TVKGR TYPE TVKGR.
*        WL_TVKBZ TYPE TVKBZ.

  CLEAR: WL_TVBVK, WL_TVKGR, WG_DESC_KUNNR, WG_HEADER-FAZENDA,
*Início - Sara Oikawa - 38859 - Agosto/2020
         WG_DESC_EMISSOR, WG_DESC_ORT01, WG_DESC_REGIO,
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
         WG_CNPJ, WG_CPF,
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
         WG_DESC_VKBUR, WG_DESC_WAERK, WG_DESC_CULTURA, WG_DESC_TPCULT.

  FREE: TG_SETLEAF_CULT, TG_SETLINET_CULT.

  PERFORM F_CHECK_DATE_RETROATIVA.

  SELECT SINGLE NAME1 ORT02
*Início - Sara Oikawa - 38859 - Agosto/2020
                ORT01 REGIO
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
                STCD1 STCD2
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    FROM KNA1
    INTO (WG_DESC_KUNNR, WG_HEADER-FAZENDA, WG_DESC_ORT01, WG_DESC_REGIO,
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
          WG_CNPJ, WG_CPF)
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
     WHERE KUNNR EQ WG_HEADER-KUNNR.

*Início - Sara Oikawa - 38859 - Agosto/2020
  IF NOT WG_DESC_KUNNR IS INITIAL.
    CONCATENATE WG_DESC_KUNNR '-' WG_DESC_ORT01 '-' WG_DESC_REGIO INTO WG_DESC_EMISSOR SEPARATED BY SPACE.
  ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

  PERFORM ZF_DETERMINE_FUNRURAL USING ABAP_FALSE. " Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  SELECT SINGLE NAME1
    FROM T001W
    INTO WG_DESC_VKBUR
     WHERE WERKS EQ WG_HEADER-VKBUR.

  SELECT SINGLE *
    FROM T001K
      INTO WA_T001K
        WHERE BWKEY EQ WG_HEADER-VKBUR.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
  CLEAR WG_DESC_VENDEDOR.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
  SELECT SINGLE *
    FROM TVBVK
    INTO WL_TVBVK
     WHERE VKBUR EQ WG_HEADER-VKBUR
       AND VKGRP EQ WG_HEADER-VENDEDOR.

  IF SY-SUBRC IS INITIAL.

    SELECT SINGLE BEZEI
      FROM TVGRT
      INTO WG_DESC_VENDEDOR
       WHERE SPRAS EQ SY-LANGU
         AND VKGRP EQ WG_HEADER-VENDEDOR.

  ENDIF.

  SELECT SINGLE VKORG VTWEG
    FROM TVKBZ
    INTO (WG_HEADER-VKORG, WG_HEADER-VTWEG)
     WHERE VKBUR EQ WG_HEADER-VKBUR.

*** US - 81799 - CBRAND
  CLEAR: WG_DESC_HBKID.
  SELECT SINGLE TEXT1
    FROM T012T
    INTO WG_DESC_HBKID
    WHERE SPRAS EQ SY-LANGU
    AND BUKRS EQ WG_HEADER-VKORG
    AND HBKID EQ WG_HEADER-HBKID.
*** US - 81799 - CBRAND

  SELECT SINGLE KTEXT
    FROM TCURT
    INTO WG_DESC_WAERK
     WHERE SPRAS EQ SY-LANGU
       AND WAERS EQ WG_HEADER-WAERK.

  SELECT SINGLE DESCRICAO
    FROM ZSDT0038
    INTO WG_DESC_CULTURA
     WHERE CULTURA EQ WG_HEADER-CULTURA.

  SELECT SINGLE *
    FROM SETLEAF
     INTO TG_SETLEAF_CULT
     WHERE SETNAME EQ 'MAGGI_ZSDT0044_03'
       AND VALFROM EQ WG_HEADER-TPCULT.

  SELECT SINGLE DESCRIPT
    FROM SETLINET
    INTO WG_DESC_TPCULT
     WHERE SETNAME EQ TG_SETLEAF_CULT-SETNAME
       AND LANGU   EQ SY-LANGU
       AND LINEID  EQ TG_SETLEAF_CULT-LINEID.

  IF WG_DESC_CULTURA IS NOT INITIAL.
    CONCATENATE 'Dt.Pagamento' WG_DESC_CULTURA INTO WG_DESC_DAT SEPARATED BY SPACE.
  ELSE.
    WG_DESC_DAT = 'Dt.Pagamento'.
  ENDIF.

  IF WG_HEADER-TPSIM EQ 'VP'.
    WG_DESC_DAT = 'Data Vencimento'.
  ENDIF.

** Verkaufsorganisationstext ermitteln
  PERFORM TVKOT_SELECT   USING WG_HEADER-VKORG
                             SPACE
                             SY-SUBRC.
** Vertriebsweg-Text ermitteln
  PERFORM TVTWT_SELECT   USING WG_HEADER-VTWEG
                             SPACE
                             SY-SUBRC.
*** Spartentext ermitteln
  PERFORM TSPAT_SELECT   USING WG_HEADER-SPART
                             SPACE
                             SY-SUBRC.



ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_SIMULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_HEADER_SIMULACAO  text
*----------------------------------------------------------------------*
FORM IMPRIME_SIMULACAO  USING    P_SIMULACAO.
  DATA: VL_FORM   TYPE TDSFNAME,
        VL_NAME   TYPE RS38L_FNAM,
        WL_HEADER TYPE ZSDS003,
        WL_KNA1   TYPE KNA1,
        WL_0040   TYPE ZSDT0040,
        TL_ITENS  TYPE TABLE OF ZSDT0041 WITH HEADER LINE.

  CLEAR: WL_KNA1, WL_HEADER.
  REFRESH: TL_ITENS.

  SELECT SINGLE *
    FROM ZSDT0040
    INTO WL_0040
    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  IF SY-SUBRC IS INITIAL.
    SELECT SINGLE VTEXT
      FROM TVKOT
      INTO WL_HEADER-ORG_VENDAS
       WHERE VKORG EQ WG_HEADER-VKORG
         AND SPRAS EQ SY-LANGU.

    SELECT SINGLE VTEXT
      FROM TVTWT
      INTO WL_HEADER-CANAL_DISTR
       WHERE VTWEG EQ WG_HEADER-VTWEG
         AND SPRAS EQ SY-LANGU.

    SELECT SINGLE BEZEI
    FROM TVKBT
    INTO WL_HEADER-ESCRIT_VENDAS
     WHERE VKBUR EQ WG_HEADER-VKBUR
       AND SPRAS EQ SY-LANGU.

    SELECT SINGLE DESCRICAO
      FROM ZSDT0038
      INTO WL_HEADER-CULTURA
       WHERE CULTURA EQ WG_HEADER-CULTURA.

    SELECT SINGLE *
      FROM KNA1
      INTO WL_KNA1
       WHERE KUNNR EQ WG_HEADER-KUNNR.

    MOVE: WL_KNA1-NAME1           TO WL_HEADER-EMISSOR,
          WL_KNA1-STRAS           TO WL_HEADER-ENDERECO,
          WL_KNA1-ORT01           TO WL_HEADER-MUNICIPIO,
          WL_KNA1-STCD1           TO WL_HEADER-CNPJ,
          WL_KNA1-STCD2           TO WL_HEADER-CPF,
          WL_KNA1-STCD3           TO WL_HEADER-IE,
          WL_KNA1-REGIO           TO WL_HEADER-UF,
          WL_KNA1-PSTLZ           TO WL_HEADER-CEP,
          WG_AREA_HA              TO WL_HEADER-AREA,
          WG_HEADER-DOC_SIMULACAO TO WL_HEADER-SIMULACAO,
          WG_HEADER-VKBUR         TO WL_HEADER-VKBUR,
          WG_HEADER-SAFRA         TO WL_HEADER-SAFRA,
          WG_HEADER-PREC_ANT_CULT TO WL_HEADER-PRECO,
          WG_HEADER-DOC_SIMULACAO TO WL_HEADER-COTACAO,
          WG_HEADER-FAZENDA       TO WL_HEADER-FAZENDA,
          WG_HEADER-JUROS_ANO     TO WL_HEADER-JUROS_ANO,
          WG_HEADER-VLR_ADTO      TO WL_HEADER-VLR_ADTO,
          WG_HEADER-ADTO_HA       TO WL_HEADER-ADTO_HA,
          WG_HEADER-AREA_PENHOR   TO WL_HEADER-AREA_PENHOR,
          WG_TVKOT-VTEXT          TO WL_HEADER-ORG_VENDAS,
          WG_TVTWT-VTEXT          TO WL_HEADER-CANAL_DISTR,
          WG_DESC_VKBUR           TO WL_HEADER-ESCRIT_VENDAS,
*          WG_HEADER-VENCI         TO WL_HEADER-VENCIMENTO,
          WG_HEADER-TROTOTSC      TO WL_HEADER-TOTALSC.

    IF WG_HEADER-TPSIM EQ 'TS'.
      WL_HEADER-MODALIDADE = 'Troca Safra'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
      WL_HEADER-VENCIMENTO = WG_HEADER-DTPGTCULT.
    ELSEIF WG_HEADER-TPSIM EQ 'TV'.
      WL_HEADER-MODALIDADE = 'Troca a Vista'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
      WL_HEADER-VENCIMENTO = WG_HEADER-DTPGTCULT.
    ELSEIF WG_HEADER-TPSIM EQ 'TT'.
      WL_HEADER-MODALIDADE = 'Troca'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
      WL_HEADER-VENCIMENTO = WG_HEADER-DTPGTCULT.
    ELSEIF WG_HEADER-TPSIM(1) EQ C_ADT(1).
      WL_HEADER-MODALIDADE = 'Adiantamento'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
    ELSEIF WG_HEADER-TPSIM EQ 'VV'.
      WL_HEADER-MODALIDADE = 'Venda a Vista'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
    ELSEIF WG_HEADER-TPSIM EQ 'PM'.
      WL_HEADER-MODALIDADE = 'Permuta'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
    ELSEIF WG_HEADER-TPSIM EQ 'VP'.
      WL_HEADER-MODALIDADE = 'Venda a Prazo'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
    ELSEIF WG_HEADER-TPSIM EQ 'VF'.
      WL_HEADER-MODALIDADE = 'Venda Futura'.
      WL_HEADER-TPSIM   = WG_HEADER-TPSIM.
    ENDIF.

    LOOP AT TG_ITENS.
      MOVE: WG_HEADER-DOC_SIMULACAO  TO TL_ITENS-DOC_SIMULACAO,
            TG_ITENS-POSNR           TO TL_ITENS-POSNR,
            TG_ITENS-MATNR           TO TL_ITENS-MATNR,
            TG_ITENS-ZMENG           TO TL_ITENS-ZMENG,
            TG_ITENS-ZIEME           TO TL_ITENS-ZIEME,
            TG_ITENS-ZWERT           TO TL_ITENS-ZWERT,
            TG_ITENS-CALCU           TO TL_ITENS-CALCU,
            TG_ITENS-TRUNIT          TO TL_ITENS-TRUNIT,
            TG_ITENS-SIUMB           TO TL_ITENS-SIUMB,
            TG_ITENS-TRTOT           TO TL_ITENS-TRTOT,
            TG_ITENS-COMPR           TO TL_ITENS-COMPR,
            TG_ITENS-VLRTOT          TO TL_ITENS-VLRTOT,
            TG_ITENS-AUART           TO TL_ITENS-AUART.

      APPEND TL_ITENS.
      CLEAR: TG_ITENS, TL_ITENS.
    ENDLOOP.

    SORT: TL_ITENS BY AUART MATNR.

    CALL  SELECTION-SCREEN 999 ENDING AT 51 8 STARTING AT 3 3.
    IF SY-SUBRC IS INITIAL.
      VL_FORM = 'ZSDS0002'.
*
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          FORMNAME           = VL_FORM
        IMPORTING
          FM_NAME            = VL_NAME
        EXCEPTIONS
          NO_FORM            = 1
          NO_FUNCTION_MODULE = 2
          OTHERS             = 3.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION VL_NAME
        EXPORTING
          WG_HEADER        = WL_HEADER
          WG_VALOR_UNIT    = P_VLR
        TABLES
          TG_ITENS         = TL_ITENS
        EXCEPTIONS
          FORMATTING_ERROR = 1
          INTERNAL_ERROR   = 2
          SEND_ERROR       = 3
          USER_CANCELED    = 4
          OTHERS           = 5.

      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'O Doc. simulação não'
                                           'foi encontrado.'.
  ENDIF.
ENDFORM.                    " IMPRIME_SIMULACAO
*&---------------------------------------------------------------------*
*&      Form  TVKOT_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBAK_VKORG  text
*      -->P_SPACE  text
*      -->P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM TVKOT_SELECT  USING US_VKORG
                         US_ERROR
                         CH_SUBRC.

  CH_SUBRC = 0.
  CHECK: US_VKORG NE WG_TVKOT-VKORG.

  IF US_VKORG = SPACE.
    CLEAR: WG_TVKOT.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM TVKOT INTO WG_TVKOT WHERE SPRAS = SY-LANGU
                                             AND VKORG = US_VKORG.
  IF SY-SUBRC NE 0.
    CLEAR: WG_TVKOT.
    IF US_ERROR NE SPACE.
      MESSAGE E313(V1) WITH US_VKORG.
    ELSE.
      CH_SUBRC = 4.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " TVKOT_SELECT
*---------------------------------------------------------------------*
*       FORM TSPAT_SELECT                                             *
*---------------------------------------------------------------------*
*       Sparte lesen Text                                             *
*---------------------------------------------------------------------*
FORM TSPAT_SELECT USING US_SPART
                        US_ERROR
                        CH_SUBRC.

  CH_SUBRC = 0.
  CHECK: US_SPART NE WG_TSPAT-SPART.

  IF US_SPART = SPACE.
    CLEAR: WG_TSPAT.
    EXIT.
  ENDIF.


  SELECT SINGLE * FROM TSPAT INTO WG_TSPAT WHERE SPRAS = SY-LANGU
                                             AND SPART = US_SPART.
  IF SY-SUBRC NE 0.
    CLEAR: WG_TSPAT.
    IF US_ERROR NE SPACE.
      MESSAGE E157(V2) WITH US_SPART SY-LANGU.
    ELSE.
      CH_SUBRC = 4.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "TSPAT_SELECT
*---------------------------------------------------------------------*
*       FORM TVTWt_SELECT                                             *
*---------------------------------------------------------------------*
*       Vertriebsweg und zugehörigen Text lesen                       *
*---------------------------------------------------------------------*
FORM TVTWT_SELECT USING US_VTWEG
                        US_ERROR
                        CH_SUBRC.

  CH_SUBRC = 0.
  CHECK: US_VTWEG NE WG_TVTWT-VTWEG.

  IF US_VTWEG = SPACE.
    CLEAR: WG_TVTW, WG_TVTWT.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM TVTWT INTO WG_TVTWT WHERE SPRAS = SY-LANGU
                                             AND VTWEG = US_VTWEG.
  IF SY-SUBRC NE 0.
    CLEAR: WG_TVTWT.
    IF US_ERROR NE SPACE.
      MESSAGE E314(V1) WITH US_VTWEG.
    ELSE.
      CH_SUBRC = 4.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "TVTWT_SELECT
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATUALIZA_DADOS INPUT.
  CLEAR WG_HEADER-MEIO_PAGO.
*Início - Sara Oikawa - 38859 - Agosto/2020
  CLEAR WG_MEIO_PAGO.
*Fim - Sara Oikawa - 38859 - Agosto/2020
  PERFORM BUSCA_DADOS.

ENDMODULE.                 " ATUALIZA_DADOS  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE REFRESH_VKBUR INPUT.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
*  CLEAR: wg_header-vendedor, wg_desc_vendedor, wg_header-vtweg,
*         wg_header-vkorg.
  CLEAR: WG_HEADER-VTWEG, WG_HEADER-VKORG.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4 - BUG 44077
ENDMODULE.                 " REFRESH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_VENDOR INPUT.
  DATA: TL_TVBVK TYPE TABLE OF TVBVK WITH HEADER LINE.
*        tl_tvgrt type table of tvgrt with header line.

  DATA: BEGIN OF TL_TVGRT OCCURS 0,
          VKGRP TYPE TVGRT-VKGRP,
          BEZEI TYPE TVGRT-BEZEI,
        END OF TL_TVGRT.

  CLEAR: TL_TVBVK, TL_TVGRT, TL_RETURN_TAB, TL_DSELC.
  REFRESH: TL_TVBVK, TL_TVGRT, TL_RETURN_TAB, TL_DSELC.

  SELECT *
    FROM TVBVK
    INTO TABLE TL_TVBVK
     WHERE VKBUR EQ WG_HEADER-VKBUR.

  IF SY-SUBRC IS INITIAL.
    SELECT VKGRP BEZEI
      FROM TVGRT
      INTO TABLE TL_TVGRT
      FOR ALL ENTRIES IN TL_TVBVK
       WHERE SPRAS EQ SY-LANGU
         AND VKGRP EQ TL_TVBVK-VKGRP.

  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'VKGRP'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_HEADER-VENDEDOR'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TVGRT
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.                 " SEARCH_VENDOR  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN .
  DATA: LS_DROPDOWN TYPE LVC_S_DROP,
        LT_DROPDOWN TYPE LVC_T_DROP.


  LS_DROPDOWN-HANDLE = '1'.
  REFRESH: TG_SETLEAF , TG_SETLINET, GT_VALUES.

  LS_DROPDOWN-HANDLE = '2'.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'FOB'.
  GT_VALUES-DOMVALUE_L = 'FOB'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CPT'.
  GT_VALUES-DOMVALUE_L = 'CPT'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CIF'.
  GT_VALUES-DOMVALUE_L = 'CIF'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CFR'.
  GT_VALUES-DOMVALUE_L = 'CFR'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
  CALL METHOD GRID1->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN = LT_DROPDOWN.
ENDFORM.                    " BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*&      Module  REFRESH_CALCU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE REFRESH_CALCU INPUT.
  PERFORM CALCULA_ITENS.
ENDMODULE.                 " REFRESH_CALCU  INPUT
*&---------------------------------------------------------------------*
*&      Form  CALCULA_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULA_ITENS.

  DATA: TL_0042         TYPE TABLE OF ZSDT0042 WITH HEADER LINE,
        TL_0037         TYPE TABLE OF ZSDT0037 WITH HEADER LINE,
        TL_0036         TYPE TABLE OF ZSDT0036 WITH HEADER LINE,
        TL_ITENS        LIKE TABLE OF TG_ITENS,
        WL_ITENS        LIKE LINE OF TG_ITENS,
        WL_0039         TYPE ZSDT0039,
        WL_VLR_PERC     TYPE ZSDT0041-ZWERT,
        WL_VLR_ALIQ     TYPE ZSDT0042-VLR_ALIQ,
        WL_PRUNIT       TYPE ZSDT0041-ZWERT,
        WL_TOT_VLR      TYPE ZSDT0041-ZWERT,
        WL_TOT_VLR_UNIT TYPE ZSDT0041-ZWERT,
        WL_TABIX        TYPE SY-TABIX,
        WL_CALC_AUX     TYPE ZSDT0041-CALCU,
        WL_KNA1         TYPE KNA1,
        WL_VALOR_AUX    LIKE WG_HEADER-TROTOTSC,
        WL_MATKL        TYPE MARA-MATKL,
        WL_TROTOTSC(30).

  CLEAR: WL_ITENS, WL_0039, WL_VLR_PERC,
         WL_VLR_ALIQ, WL_TOT_VLR_UNIT,
         WL_TABIX, WL_KNA1, WL_TROTOTSC,
         WL_VALOR_AUX, TL_0036, TL_0042,
         TL_0037, TL_ITENS, WL_PRUNIT,
         WL_TOT_VLR.

  WG_HEADER-DTINIJUROS = WG_HEADER-DTPGTCULT.

  SELECT SINGLE *
    FROM KNA1
    INTO WL_KNA1
   WHERE KUNNR EQ WG_HEADER-KUNNR.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
  IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV'     "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
  OR WG_HEADER-TPSIM EQ 'TT'.                               "FF #174195

    SELECT *
      FROM ZSDT0042 INTO TABLE TL_0042
     WHERE CULTURA EQ WG_HEADER-CULTURA
       AND WAERK   EQ WG_HEADER-WAERK
       AND ESTADO  EQ WL_KNA1-REGIO
       AND SAFRA   EQ WG_HEADER-SAFRA  " RJF - 61516 - 2023.08.31
       AND VAL_DE  LE WG_HEADER-DTENT  " RJF - 61516 - 2023.08.31
       AND VAL_ATE GE WG_HEADER-DTENT. " RJF - 61516 - 2023.08.31

    LOOP AT TL_0042.
      CHECK ( TL_0042-WITHT NE 'FI' ).
      IF ( TL_0042-WITHT EQ 'FR' ).
        IF ( WL_KNA1-STKZN IS NOT INITIAL ).
          IF WG_HEADER-FUNRURAL IS INITIAL.
            ADD TL_0042-VLR_PERC TO WL_VLR_PERC.
          ELSE.
            ADD TL_0042-VLR_PERC1 TO WL_VLR_PERC.
          ENDIF.
          ADD TL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
        ENDIF.
      ELSE.
        ADD TL_0042-VLR_PERC TO WL_VLR_PERC.
        ADD TL_0042-VLR_ALIQ TO WL_VLR_ALIQ.
      ENDIF.
    ENDLOOP.

  ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF WG_ACAO EQ C_ATUAL AND SY-UCOMM NE 'BTN_FUN'.
    WL_VLR_PERC = WG_HEADER-INSS.
    WL_VLR_ALIQ = WG_HEADER-FACS.
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  WG_HEADER-INSS = WL_VLR_PERC.
  WG_HEADER-FACS = WL_VLR_ALIQ.

  IF ( TG_ITENS[] IS NOT INITIAL ).

    SELECT *
      FROM ZSDT0037
INTO TABLE TL_0037
   FOR ALL ENTRIES IN TG_ITENS
     WHERE VAL_DE          LE SY-DATUM
       AND VAL_ATE         GE SY-DATUM
       AND MEINS           EQ TG_ITENS-ZIEME
       AND FILIAL_ORIGEM   EQ TG_ITENS-WERKS
       AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
       AND WAERS           EQ 'BRL'
       AND BUKRS           EQ WA_T001K-BUKRS.

    SELECT *
      FROM ZSDT0036
INTO TABLE TL_0036
   FOR ALL ENTRIES IN TG_ITENS
     WHERE VAL_DE       LE SY-DATUM
       AND VAL_ATE      GE SY-DATUM
       AND MATNR        EQ TG_ITENS-MATNR
       AND BUKRS        EQ WA_T001K-BUKRS
       AND WAERK        EQ WG_HEADER-WAERK
       AND MEINS        EQ TG_ITENS-ZIEME
       AND SAFRA        EQ WG_HEADER-SAFRA
       AND INCO1        EQ TG_ITENS-INCO1
       AND WERKS_FORNEC EQ TG_ITENS-WERKS
       AND LOEKZ        EQ SPACE
       AND CULTURA      IN (WG_HEADER-CULTURA , SPACE )
       AND ELIMINADO    EQ SPACE.

*    IF WG_HEADER-WAERK EQ 'USD'.
*      CLEAR TAXA.
*
*      SELECT SINGLE KURSK
*        FROM ZSDT0117
*        INTO TAXA_FRE
*        WHERE BUKRS EQ WA_T001K-BUKRS
*        AND DESATIVADO NE ABAP_TRUE.
*
*      WL_0037-VLR_FRETE = WL_0037-VLR_FRETE / TAXA.
*    ENDIF.

  ENDIF.

  LOOP AT TG_ITENS INTO WL_ITENS.
    WL_TABIX = SY-TABIX.

    CLEAR: TL_0036.
    READ TABLE TL_0036 WITH KEY MATNR = WL_ITENS-MATNR
                                INCO1 = WL_ITENS-INCO1
                              CULTURA = WG_HEADER-CULTURA
                         WERKS_FORNEC = WL_ITENS-WERKS.

    IF ( SY-SUBRC IS NOT INITIAL ).
      READ TABLE TL_0036 WITH KEY MATNR = WL_ITENS-MATNR
                                  INCO1 = WL_ITENS-INCO1
                                CULTURA = SPACE
                           WERKS_FORNEC = WL_ITENS-WERKS.

    ENDIF.
*** CALCULO CIF
    IF ( WL_ITENS-INCO1 EQ 'CIF' ).
      CLEAR  TL_0037.
      SELECT SINGLE MATKL
        FROM MARA
        INTO WL_MATKL
       WHERE MATNR EQ WL_ITENS-MATNR.

      READ TABLE TL_0037 WITH KEY FILIAL_ORIGEM  = WL_ITENS-WERKS
                                          MEINS  = WL_ITENS-ZIEME
                                  FILIAL_DESTINO = WG_HEADER-VKBUR
                                          WAERS  = 'BRL'
                                          MATKL  = WL_MATKL.

      PERFORM ATUALIZA_FRETE CHANGING TL_0037-VLR_FRETE.
    ELSE.
      CLEAR: TL_0037.
    ENDIF.

    MOVE: TL_0036-PERC_MARGEM  TO WL_ITENS-MGCAD.
    DATA: LV_VALUE_AUX TYPE F.

    IF NOT TL_0036-VLR_VENDA IS INITIAL AND
       NOT WL_ITENS-ZWERT IS INITIAL.
*       NOT TL_0037-VLR_FRETE IS INITIAL.
      TRY.                                                                                           "BUG 46674 - 28.10.2020
          LV_VALUE_AUX  =  ( ( TL_0036-VLR_VENDA / ( WL_ITENS-ZWERT - TL_0037-VLR_FRETE ) ) - 1 ) * 100.
        CATCH  CX_SY_ZERODIVIDE.
      ENDTRY.
    ENDIF.

**  Realiza o calculo do campo  "Vlr Negociado"  **
    IF ( WL_ITENS-DESCONTO NE 0 ).

*     WL_CALC_AUX    = WL_ITENS-DESCONTO / 100.
      WL_CALC_AUX    = LV_VALUE_AUX / 100.
      ADD 1 TO WL_CALC_AUX.

      WL_ITENS-ZWERT = ( TL_0036-VLR_VENDA / WL_CALC_AUX ).
      ADD TL_0037-VLR_FRETE TO WL_ITENS-ZWERT.

    ELSE.
      WL_ITENS-ZWERT = ( TL_0036-VLR_VENDA + TL_0037-VLR_FRETE ).
    ENDIF.

**  Realiza o calculo do campo "Margem Efetiva" **
    TRY.
        WL_ITENS-MGEFE = ( ( ( WL_ITENS-ZWERT - TL_0036-VLR_CUSTO ) / WL_ITENS-ZWERT ) * 100 ).
      CATCH: CX_SY_ZERODIVIDE, CX_SY_ARITHMETIC_OVERFLOW.
        WL_ITENS-MGEFE = 0.
    ENDTRY.

**  Realiza calculo do "Vlr unitário"
    WL_ITENS-VL_UNIT = ( TL_0036-VLR_VENDA + TL_0037-VLR_FRETE ).

    IF ( WG_HEADER-TPSIM(1) EQ C_TRO(1) ).
      WL_ITENS-CALCU = WL_ITENS-ZWERT.
**    Realiza o calculo do campo "Troca Unitaria"

      TRY.
          WL_PRUNIT       = WG_HEADER-PREC_ANT_CULT. " / ( ( WG_HEADER-ANTEC / 100 ) + 1 ).
          IF NOT WL_PRUNIT IS INITIAL.
            WL_ITENS-TRUNIT = ( WL_ITENS-CALCU / ( ( WL_PRUNIT - ( ( WL_PRUNIT * ( WL_VLR_PERC / 100 ) ) + ( WL_VLR_ALIQ ) ) ) ) ).
          ENDIF.
        CATCH CX_SY_ZERODIVIDE.
      ENDTRY.

**    Realiza o calculo do campo "Troca Total Item" **
      WL_ITENS-TRTOT = WL_ITENS-TRUNIT * WL_ITENS-ZMENG.

    ELSEIF ( WG_HEADER-TPSIM(1) EQ C_ADT(1) ).

** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
*    Realiza o calculo do campo "Compromisso"
      TRY.
          WL_ITENS-COMPR = WL_ITENS-VLRTOT / WG_HEADER-VLR_ADTO.
        CATCH CX_SY_ZERODIVIDE.
      ENDTRY.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
** "// Retornar AD WBARBOSA para Time Credito 09/07/2025

    ELSEIF ( WG_HEADER-TPSIM(2) EQ 'BN' ).

      CLEAR: WL_ITENS-ZWERT, WL_ITENS-VLRTOT, WL_CALC_AUX.
      WL_ITENS-ZWERT = ( TL_0036-VLR_CUSTO ).

    ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
    IF WG_HEADER-TPSIM(2) NE 'VP'.
*Fim - Sara Oikawa - 38859 - Agosto/2020
      "JUROS
      WL_DIAS_VCT = 0.
      IF ( NOT WG_HEADER-DTINIJUROS IS INITIAL ).
        WL_DIAS_VCT = WG_HEADER-DTPGTCULT - WG_HEADER-DTINIJUROS.
      ENDIF.

      IF ( WL_DIAS_VCT GT 0 ).
        WL_JURO_DIAS     = ( ( WG_HEADER-JUROS_ANO / 365 ) * WL_DIAS_VCT ) / 100.
        WL_ITENS-VL_UNIT = WL_ITENS-ZWERT = WL_ITENS-ZWERT + ( WL_ITENS-ZWERT * WL_JURO_DIAS ).
      ENDIF.
*Início - Sara Oikawa - 38859 - Agosto/2020
    ENDIF.
*Fim - Sara Oikawa - 38859 - Agosto/2020

*   Realiza o calculo do campo "Valor Total item"
    WL_ITENS-VLRTOT = WL_ITENS-ZWERT * WL_ITENS-ZMENG.

*   Preenche o imposto
    PERFORM BUSCA_IMPOSTO CHANGING WL_ITENS.

    MODIFY TG_ITENS FROM WL_ITENS INDEX WL_TABIX.

  ENDLOOP.

***** Método de atualização de dados na Tela
  CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  CLEAR: WL_ITENS.
  IF ( WG_HEADER-TPSIM(1) EQ C_ADT(1) ).
    CLEAR: WL_TOT_VLR,
           WG_HEADER-VLRTOT,
           WG_HEADER-COMPRSC.

    PERFORM VLRTOT.

    LOOP AT TG_ITENS INTO WL_ITENS.
      ADD WL_ITENS-VLRTOT TO WL_TOT_VLR.
**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
**    Realiza o calculo do campo "Compromisso/sac"
      ADD WL_ITENS-COMPR  TO WG_HEADER-COMPRSC.
    ENDLOOP.

** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
**    Realiza o calculo do campo "Area de Penhor"
    TRY.
        WG_HEADER-AREA_PENHOR = WL_TOT_VLR / WG_HEADER-ADTO_HA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
** "// Retornar AD WBARBOSA para Time Credito 09/07/2025

  ELSEIF WG_HEADER-TPSIM(1) EQ C_TRO(1).
    CLEAR: WG_HEADER-TROTOTSC, WG_HEADER-SCHA, WG_HEADER-VLRTOT, CONVERT_TRATOTSC.

    PERFORM VLRTOT.

    LOOP AT TG_ITENS INTO WL_ITENS.
*    Realiza o calculo do campo "Troca Total Sc"
      ADD: WL_ITENS-TRTOT TO WG_HEADER-TROTOTSC,
           WL_ITENS-TRTOT TO CONVERT_TRATOTSC.
**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
    ENDLOOP.

**    Realiza o calculo do campo "Sc/há"
    TRY.
        WG_HEADER-SCHA = WG_HEADER-TROTOTSC / WG_AREA_HA. "WG_HEADER-AREA_HA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

  ENDIF.
ENDFORM.                    " CALCULA_ITENS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_SAFRA INPUT.
  DATA: BEGIN OF TL_SAFRA OCCURS 0,
          SAFRA     TYPE ZSDT0044-SAFRA,
          DESCRICAO TYPE ZSDT0044-DESCRICAO,
        END OF TL_SAFRA.

  REFRESH: TL_SAFRA, TL_RETURN_TAB, TL_DSELC.
  CLEAR: TL_SAFRA, TL_RETURN_TAB, TL_DSELC.

  SELECT SAFRA DESCRICAO
    FROM ZSDT0044
    INTO TABLE TL_SAFRA.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'SAFRA'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_HEADER-SAFRA'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_SAFRA
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_SAFRA  INPUT

*&      Form  MODIFICA_STATUS
FORM MODIFICA_STATUS  USING  P_STATUS.

  DATA: WL_0040 TYPE ZSDT0040.

  CLEAR: WL_0040.

  IF P_STATUS EQ C_B.

    DATA: P_RESP.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION         = 'Deseja Bloqueiar este Simulador?               Após Bloqueado o mesmo ficará disponível apenas para Visualização!'
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = P_RESP.

    CHECK P_RESP EQ 1.

  ENDIF.

  DATA(DOC) = WG_HEADER-DOC_SIMULACAO.
  DOC = |{ DOC ALPHA = OUT }|.

*// Bloqueia o Documento de Simulação para que o JOB não execute
*  CALL FUNCTION 'ENQUEUE_EZSDT0040_JOB'
*    EXPORTING
*      DOC_SIMULACAO  = DOC
*    EXCEPTIONS
*      FOREIGN_LOCK   = 1
*      SYSTEM_FAILURE = 2
*      OTHERS         = 3.


  SELECT SINGLE *
    FROM ZSDT0040
    INTO WL_0040
     WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  IF SY-SUBRC IS INITIAL.

*    Realiza o desbloqueio das ação enteriores e bloqueia novamente para não acumular Bloqueio.
    CALL FUNCTION 'DEQUEUE_EZSDT0040'
      EXPORTING
        DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

    CALL FUNCTION 'ENQUEUE_EZSDT0040'
      EXPORTING
        DOC_SIMULACAO  = WG_HEADER-DOC_SIMULACAO
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    " INICIO LOG AÇÕES
    PERFORM LOG_ACAO USING P_STATUS '' CHANGING LV_ERRO.
    " FIM LOG AÇÕES

    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
    CHECK LV_ERRO IS INITIAL.
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<


    MOVE: P_STATUS TO WL_0040-STATUS.

    "CS2021000667 - #111424 - SC -->
*    CASE wl_0040-status.
*      WHEN c_a. wg_status = icon_release.
*      WHEN c_r. wg_status = icon_defect.
*      WHEN c_b. wg_status = icon_gis_pan.
*      WHEN OTHERS.
*        IF wl_0040-status IS INITIAL.
*          wg_status = icon_initial.
*        ENDIF.
*    ENDCASE.
*
*    wl_0040-job = abap_false.



    "MODIFY zsdt0040 FROM wl_0040.
    "CS2021000667 - #111424 - SC --<

    " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
    TRY .

*    IF SY-UNAME NE 'WBARBOSA'.
        CASE P_STATUS.
          WHEN C_A.

*    DISPARO DO HEDGE PARA VENDA
            IF WL_0040-WAERK EQ 'BRL'.

              CASE WL_0040-TPSIM.
                WHEN 'BN' OR 'PM'.
                WHEN OTHERS.
                  ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO      = WG_HEADER-DOC_SIMULACAO
                                                          I_ACAO        = SY-UCOMM
                                                          I_TIPO        = 'VDI'
                                                          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
                                                          I_TAXA_BOLETA = GV_TAXA_NEG
                                                          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
                                                          ).
              ENDCASE.
            ENDIF.

*    DISPARO DO HEDGE PARA FRETE
            IF WL_0040-TPSIM NE 'PM'.
              ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WG_HEADER-DOC_SIMULACAO
                                                      I_ACAO   = SY-UCOMM
                                                      I_TIPO   = 'FRI'
                                                      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
                                                      "i_taxa_boleta = gv_taxa_neg
                                                      " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
                                                      ).
            ENDIF.

          WHEN C_R OR C_B.
*    REVERTE O DISPARO DO HEDGE PARA FRETE E VENDA
            ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WG_HEADER-DOC_SIMULACAO
                                                    I_TIPO   = 'EST'
                                                    ).
        ENDCASE.
*    ENDIF.

        CALL FUNCTION 'DEQUEUE_EZSDT0040'
          EXPORTING
            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.


*    CALL FUNCTION 'DEQUEUE_EZSDT0040_JOB'
*      EXPORTING
*        DOC_SIMULACAO = DOC.

        CLEAR SY-UCOMM.


      CATCH ZCX_WEBSERVICE.
        LV_ERRO = ABAP_TRUE. "CS2021000667 - #111424 - SC"
        MESSAGE W104(ZSD).
    ENDTRY.
    " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- <

    "CS2021000667 - #111424 - SC -->
    IF LV_ERRO IS INITIAL.

      CASE WL_0040-STATUS.
        WHEN C_A. WG_STATUS = ICON_RELEASE.
        WHEN C_R. WG_STATUS = ICON_DEFECT.
        WHEN C_B. WG_STATUS = ICON_GIS_PAN.
        WHEN OTHERS.
          IF WL_0040-STATUS IS INITIAL.
            WG_STATUS = ICON_INITIAL.
          ENDIF.
      ENDCASE.

      WL_0040-JOB = ABAP_FALSE.

      MODIFY ZSDT0040 FROM WL_0040.

    ENDIF.
    "CS2021000667 - #111424 - SC --<

  ENDIF.

ENDFORM.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM VALIDA_LAYOUT  TABLES   TL_FIELDCATALOG STRUCTURE LVC_S_FCAT
                     USING   UNAME.

*BREAK-POINT.

  DATA: TL_PARAMETROS        TYPE USTYP_T_PARAMETERS,
        WL_PARAMETROS        TYPE USTYP_PARAMETERS,
        WL_FIELDCATALOG      TYPE LVC_S_FCAT,
        WL_VARIANTE01        TYPE ZVARIANTE01,
        TL_VARIANTE02_ALV    TYPE TABLE OF ZVARIANTE02 WITH HEADER LINE,
        TL_VARIANTE02_SCREEN TYPE TABLE OF ZVARIANTE02 WITH HEADER LINE,
        WL_TABIX             TYPE SY-TABIX,
        WL_ATRIBUTO(30).

  REFRESH: TL_PARAMETROS, TL_VARIANTE02_ALV, TL_VARIANTE02_SCREEN.
  FIELD-SYMBOLS: <FS_ATRIBUTOS> TYPE ANY.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      USER_NAME           = UNAME
*     WITH_TEXT           =
    TABLES
      USER_PARAMETERS     = TL_PARAMETROS
    EXCEPTIONS
      USER_NAME_NOT_EXIST = 1
      OTHERS              = 2.
  IF SY-SUBRC <> 0.
  ENDIF.

  READ TABLE TL_PARAMETROS INTO WL_PARAMETROS
    WITH KEY PARID = 'ZVARIANTE'.
  IF SY-SUBRC IS INITIAL.
    SELECT SINGLE *
      FROM ZVARIANTE01
      INTO WL_VARIANTE01
       WHERE GRPVA EQ WL_PARAMETROS-PARVA
         AND TCODE EQ SY-TCODE.

    IF SY-SUBRC IS INITIAL.
      CONDENSE WL_VARIANTE01-GRPVA NO-GAPS.
      SELECT *
        FROM ZVARIANTE02
        INTO TABLE TL_VARIANTE02_ALV
         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
           AND TCODE   EQ SY-TCODE
           AND ATR_TIP EQ 'ALV'
           AND DYNNR   EQ SY-DYNNR.

      SELECT *
        FROM ZVARIANTE02
        INTO TABLE TL_VARIANTE02_SCREEN
         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
           AND TCODE   EQ SY-TCODE
           AND ATR_TIP NE 'ALV'
           AND DYNNR   EQ SY-DYNNR.

    ENDIF.
    IF TL_VARIANTE02_SCREEN[] IS NOT INITIAL
    AND ( SY-TCODE NE 'SE38'
       AND SY-TCODE NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE TL_VARIANTE02_SCREEN
          WITH KEY FIELD = SCREEN-NAME.

        IF SY-SUBRC IS INITIAL.

*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 >>> INI
*CH 103640  - Simulador de Vendas - Ajuste campo Taxa Antecipação
          IF TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL  AND
             WG_ACAO                   IS NOT INITIAL.
*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 <<< FIM
            IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 >>> INI
*CH 103640  - Simulador de Vendas - Ajuste campo Taxa Antecipação
*           AND TL_VARIANTE02_SCREEN-ACAO eq WG_ACAO )
            AND TL_VARIANTE02_SCREEN-ACAO CS WG_ACAO )
*** Modificação - Eduardo Ruttkowski Tavares - 31.07.2013 <<< FIM
              OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
              UNASSIGN <FS_ATRIBUTOS>.
              CONCATENATE 'SCREEN' TL_VARIANTE02_SCREEN-ATR_TIP INTO WL_ATRIBUTO SEPARATED BY '-'.
              ASSIGN (WL_ATRIBUTO) TO <FS_ATRIBUTOS>.
              IF <FS_ATRIBUTOS> IS ASSIGNED.
                <FS_ATRIBUTOS> = TL_VARIANTE02_SCREEN-FATR_VALUE.
                MODIFY SCREEN.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF TL_VARIANTE02_ALV[] IS INITIAL
    AND ( SY-TCODE EQ 'SE38'
       OR SY-TCODE EQ 'SE80' ).
      EXIT.
    ENDIF.

    READ TABLE TL_VARIANTE02_ALV WITH KEY DYNNR = SY-DYNNR.
    IF SY-SUBRC IS INITIAL.
      LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
        WL_TABIX = SY-TABIX.
        READ TABLE TL_VARIANTE02_ALV WITH KEY FIELD = WL_FIELDCATALOG-FIELDNAME.
        IF SY-SUBRC IS NOT INITIAL.
          IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
           AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
            OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
            DELETE TL_FIELDCATALOG INDEX WL_TABIX.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF SY-TABIX EQ 1. ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM ZVARIANTE01
      INTO WL_VARIANTE01
       WHERE DEFAULT_VAR EQ C_X
         AND TCODE EQ SY-TCODE.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM ZVARIANTE02
        INTO TABLE TL_VARIANTE02_ALV
         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
           AND TCODE   EQ SY-TCODE
           AND ATR_TIP EQ 'ALV'
           AND DYNNR   EQ SY-DYNNR.

      SELECT *
         FROM ZVARIANTE02
         INTO TABLE TL_VARIANTE02_SCREEN
          WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
            AND TCODE   EQ SY-TCODE
            AND ATR_TIP NE 'ALV'
            AND DYNNR   EQ SY-DYNNR.
    ENDIF.
    IF TL_VARIANTE02_SCREEN[] IS NOT INITIAL
        AND ( SY-TCODE NE 'SE38'
           AND SY-TCODE NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE TL_VARIANTE02_SCREEN
          WITH KEY FIELD = SCREEN-NAME.

        IF SY-SUBRC IS INITIAL.
          IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
            AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
              OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
            UNASSIGN <FS_ATRIBUTOS>.
            CONCATENATE 'SCREEN' TL_VARIANTE02_SCREEN-ATR_TIP INTO WL_ATRIBUTO SEPARATED BY '-'.
            ASSIGN (WL_ATRIBUTO) TO <FS_ATRIBUTOS>.
            IF <FS_ATRIBUTOS> IS ASSIGNED.
              <FS_ATRIBUTOS> = TL_VARIANTE02_SCREEN-FATR_VALUE.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF TL_VARIANTE02_ALV[] IS INITIAL
    AND ( SY-TCODE EQ 'SE38'
       OR SY-TCODE EQ 'SE80' ).
      EXIT.
    ENDIF.
    READ TABLE TL_VARIANTE02_ALV WITH KEY DYNNR = SY-DYNNR.
    IF SY-SUBRC IS INITIAL.
      LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
        WL_TABIX = SY-TABIX.
        READ TABLE TL_VARIANTE02_ALV WITH KEY FIELD = WL_FIELDCATALOG-FIELDNAME.
        IF SY-SUBRC IS NOT  INITIAL.
          IF ( TL_VARIANTE02_ALV-ACAO IS NOT INITIAL
           AND TL_VARIANTE02_ALV-ACAO EQ WG_ACAO )
            OR TL_VARIANTE02_ALV-ACAO IS INITIAL.
            DELETE TL_FIELDCATALOG INDEX WL_TABIX.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTA_proposta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTA_PROPOSTA.
  DATA: TL_0042 TYPE TABLE OF ZSDT0042 WITH HEADER LINE,
        WL_KNA1 TYPE KNA1.

  CLEAR: TL_0042, WL_KNA1, WG_PROP.
  REFRESH: TL_0042.

  SELECT SINGLE *
    FROM KNA1
    INTO WL_KNA1
     WHERE KUNNR EQ WG_HEADER-KUNNR.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF WG_ACAO EQ C_ATUAL.
    WG_PROP-INSS  = WG_HEADER-INSS.
    WG_PROP-FACS  = WG_HEADER-FACS.
    WG_PROP-TANTE = WG_HEADER-ANTEC.        "Taxa de Antecipado
    WG_PROP-FUNDEINFRA = WG_HEADER-FUNDEINFRA.
  ELSE.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    WG_PROP-TANTE = WG_HEADER-ANTEC.        "Taxa de Antecipado
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV'   "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    OR WG_HEADER-TPSIM EQ 'TT' .                            "FF #174195

      SELECT *
        FROM ZSDT0042
        INTO TABLE TL_0042
         WHERE CULTURA EQ WG_HEADER-CULTURA
           AND WAERK   EQ WG_HEADER-WAERK
           AND ESTADO  EQ WL_KNA1-REGIO   " INCLUIDO REGIO DO CLIENTE
           AND SAFRA   EQ WG_HEADER-SAFRA  " RJF - 61516 - 2023.08.31
           AND VAL_DE  LE WG_HEADER-DTENT  " RJF - 61516 - 2023.08.31
           AND VAL_ATE GE WG_HEADER-DTENT. " RJF - 61516 - 2023.08.31

      IF WL_KNA1-STKZN IS NOT INITIAL.
*  * Taxa do INSS
        READ TABLE TL_0042 WITH KEY WITHT = 'FR'
                                    WAERK = WG_HEADER-WAERK.
        IF SY-SUBRC IS INITIAL.
          IF WG_HEADER-FUNRURAL IS INITIAL.
            WG_PROP-INSS = TL_0042-VLR_PERC.
          ELSE.
            WG_PROP-INSS = TL_0042-VLR_PERC1.
          ENDIF.
        ENDIF.

      ENDIF.
*#138092 -  ITSOUZA - Inicio
*  * Taxa do FUNDEINFRA
      READ TABLE TL_0042 WITH KEY WITHT = 'FI'
                                  WAERK = WG_HEADER-WAERK.
      IF SY-SUBRC IS INITIAL.
        IF WG_HEADER-FUNDEINFRA_EXCE IS INITIAL.
          WG_PROP-FUNDEINFRA = TL_0042-VLR_PERC.
        ELSE.
          WG_PROP-FUNDEINFRA = TL_0042-VLR_PERC1.
        ENDIF.
      ENDIF.
*#138092 -  ITSOUZA - Fim

*  * Taxa do FACS/FETHAB
      READ TABLE TL_0042 WITH KEY WITHT = 'FT'
                                  WAERK = WG_HEADER-WAERK.
      IF SY-SUBRC IS INITIAL.
        IF TL_0042-WAERK EQ WG_HEADER-WAERK.
          MOVE  TL_0042-VLR_ALIQ TO WG_PROP-FACS.
        ENDIF.
      ENDIF.

    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  CLEAR WL_0040.

  SELECT SINGLE *
    FROM ZSDT0040
    INTO WL_0040
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  IF ( WL_0040-PREC_CULT IS NOT INITIAL ).

    TRY.
        WG_PROP-VLRANT_AUX = ( WL_0040-PREC_CULT / ( 1 + ( WG_PROP-TANTE / 100 ) ) ) .
        WG_PROP-VLRANT = WG_PROP-VLRANT_AUX.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

  ELSE.
    WG_PROP-VLRANT = WG_HEADER-PREC_ANT_CULT.
  ENDIF.

** Valor total Antecipado Bruto
  WG_PROP-VTA_BRUTO = CONVERT_TRATOTSC * WG_PROP-VLRANT.


ENDFORM.                    "MONTA_proposta

*&---------------------------------------------------------------------*
*&      Form  MONTA_MEMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_MEMO.
  DATA: TL_0042 TYPE TABLE OF ZSDT0042 WITH HEADER LINE,
        WL_KNA1 TYPE KNA1.

  CLEAR: TL_0042, WL_KNA1, WG_MEMO, WL_0040.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF WG_ACAO EQ C_ATUAL AND SY-UCOMM NE 'BTN_FUN'.
    WG_MEMO-INSS  = WG_HEADER-INSS.
    WG_MEMO-FACS  = WG_HEADER-FACS.
    WG_MEMO-TANTE = WG_HEADER-ANTEC. "Taxa de Antecipado
  ELSE.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    WG_MEMO-TANTE = WG_HEADER-ANTEC. "Taxa de Antecipado
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV'   "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    OR WG_HEADER-TPSIM EQ 'TT'.                             "FF #174195

      SELECT SINGLE *
        FROM KNA1
        INTO WL_KNA1
       WHERE KUNNR EQ WG_HEADER-KUNNR.

      SELECT *
        FROM ZSDT0042
        INTO TABLE TL_0042
       WHERE CULTURA EQ WG_HEADER-CULTURA
         AND WAERK   EQ WG_HEADER-WAERK
           AND ESTADO  EQ WL_KNA1-REGIO
           AND SAFRA   EQ WG_HEADER-SAFRA  " RJF - 61516 - 2023.08.31
           AND VAL_DE  LE WG_HEADER-DTENT  " RJF - 61516 - 2023.08.31
           AND VAL_ATE GE WG_HEADER-DTENT. " RJF - 61516 - 2023.08.31



*   __________________Taxa do INSS__________________________
      IF ( WL_KNA1-STKZN IS NOT INITIAL ).
        READ TABLE TL_0042 WITH KEY WITHT = 'FR'
                                    WAERK = WG_HEADER-WAERK.
        IF ( SY-SUBRC IS INITIAL ).
          WG_MEMO-INSS = COND #( WHEN WG_HEADER-FUNRURAL IS INITIAL THEN TL_0042-VLR_PERC ELSE TL_0042-VLR_PERC1 ).
        ENDIF.
      ENDIF.

*   _________________Taxa do FACS/FETHAB____________________
      READ TABLE TL_0042 WITH KEY WITHT = 'FT'
                                  WAERK = WG_HEADER-WAERK.
      IF ( SY-SUBRC IS INITIAL ).
        IF TL_0042-WAERK EQ WG_HEADER-WAERK.
          WG_MEMO-FACS = TL_0042-VLR_ALIQ.
        ENDIF.
      ENDIF.
    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

    WG_HEADER-INSS        = WG_MEMO-INSS.
    WG_HEADER-FACS        = WG_MEMO-FACS.

  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6


*#138092 - ITSOUZA - Inicio
*  IF ( wg_acao EQ c_atual OR wg_acao EQ c_modif ) AND sy-ucomm NE 'BTN_FI' AND wg_copy NE abap_true.
  IF ( WG_ACAO EQ C_ATUAL ) AND SY-UCOMM NE 'BTN_FI' AND WG_COPY NE ABAP_TRUE."SMC
    WG_MEMO-FUNDEINFRA = WG_HEADER-FUNDEINFRA.
  ELSE.
    IF WL_KNA1 IS INITIAL AND
       TL_0042 IS INITIAL.

      SELECT SINGLE *
        FROM KNA1
        INTO WL_KNA1
       WHERE KUNNR EQ WG_HEADER-KUNNR.

      SELECT *
        FROM ZSDT0042
        INTO TABLE TL_0042
       WHERE CULTURA EQ WG_HEADER-CULTURA
         AND WAERK   EQ WG_HEADER-WAERK
           AND ESTADO  EQ WL_KNA1-REGIO
           AND SAFRA   EQ WG_HEADER-SAFRA  " RJF - 61516 - 2023.08.31
           AND VAL_DE  LE WG_HEADER-DTENT  " RJF - 61516 - 2023.08.31
           AND VAL_ATE GE WG_HEADER-DTENT. " RJF - 61516 - 2023.08.31
    ENDIF.

*   __________________Taxa do fundeinfra__________________________
*    IF ( wl_kna1-stkzn IS NOT INITIAL ). Nesse caso tanto faz para pessoa fisica quanto para juridica - SMC
    READ TABLE TL_0042 WITH KEY WITHT = 'FI'
                                WAERK = WG_HEADER-WAERK.

    IF ( SY-SUBRC IS INITIAL ).
*        IF sy-ucomm EQ 'BTN_FI'.
*          wg_header-fundeinfra_exce  = abap_true.
      WG_MEMO-FUNDEINFRA = COND #( WHEN WG_HEADER-FUNDEINFRA_EXCE IS INITIAL THEN TL_0042-VLR_PERC
                                   ELSE TL_0042-VLR_PERC1 ).
*        ELSE.
*          wg_memo-fundeinfra = tl_0042-vlr_perc.
*        ENDIF.
    ENDIF.
*    ENDIF.

    WG_HEADER-FUNDEINFRA  = WG_MEMO-FUNDEINFRA.
  ENDIF.
  CLEAR WG_COPY.
*#138092 - ITSOUZA - Fim

* _________________Valor Antecipado Bruto________________

  SELECT SINGLE *
    FROM ZSDT0040
    INTO WL_0040
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  IF ( WL_0040-PREC_CULT IS NOT INITIAL ).

    TRY.
        WG_MEMO-VLRANT_AUX = ( WL_0040-PREC_CULT / ( 1 + ( WG_MEMO-TANTE / 100 ) ) ) .
        WG_MEMO-VLRANT = WG_MEMO-VLRANT_AUX.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

  ELSE.
    WG_MEMO-VLRANT = WG_HEADER-PREC_ANT_CULT.
  ENDIF.

* ____________________Valor INSS________________________________________
  TRY.
      WG_MEMO-VLRINSS_AUX = ( WG_MEMO-VLRANT * ( WG_MEMO-INSS / 100 ) ).
      WG_MEMO-VLRINSS     =   WG_MEMO-VLRINSS_AUX.
    CATCH CX_SY_ZERODIVIDE.
  ENDTRY.


*#138092 - ITSOUZA - Inicio
* ____________________Valor Fundeinfra________________________________________
  TRY.
      WG_MEMO-VLRFUNDEIN_AUX = ( WG_MEMO-VLRANT * ( WG_MEMO-FUNDEINFRA / 100 ) ).
      WG_MEMO-VLRFUNDEIN     =   WG_MEMO-VLRFUNDEIN_AUX.
    CATCH CX_SY_ZERODIVIDE.
  ENDTRY.
*#138092 - ITSOUZA - Fim

* __________________Valor FACS/FETHAB___________________________________

  WG_MEMO-VLRFACS    = WG_MEMO-FACS.

* _________________Preço Sc. Liquido____________________________________

  WG_MEMO-PLIQDO_AUX = ( WG_MEMO-VLRANT - ( WG_MEMO-VLRINSS_AUX + WG_MEMO-VLRFACS ) - WG_MEMO-VLRFUNDEIN ).
  WG_MEMO-PLIQDO     = WG_MEMO-PLIQDO_AUX.

* _____________________________________________________________________


ENDFORM.                    " MONTA_MEMO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.

  DATA: STATUS_BTN TYPE CHAR1.

  SET PF-STATUS 'Z01'.
  SET TITLEBAR 'Z002'.

  CLEAR STATUS_BTN.

*#138092 -  ITSOUZA - 28.05.2024 15:59:54 - Inicio
  SELECT SINGLE *
    FROM ZSDT0043
    INTO WL_0043
     WHERE WERKS EQ WL_0040-VKBUR
       AND UNAME EQ SY-UNAME.

  SELECT SINGLE *
FROM KNA1
INTO @DATA(WL_KNA1)
WHERE KUNNR EQ @WG_HEADER-KUNNR.

  SELECT *
    FROM ZSDT0042
    INTO TABLE @DATA(LT_ZSDT0042)
   WHERE CULTURA EQ @WG_HEADER-CULTURA
     AND WAERK   EQ @WG_HEADER-WAERK
     AND ESTADO  EQ @WL_KNA1-REGIO
     AND SAFRA   EQ @WG_HEADER-SAFRA
     AND VAL_DE  LE @WG_HEADER-DTENT
     AND VAL_ATE GE @WG_HEADER-DTENT.
* #138092 -  ITSOUZA - 28.05.2024 15:59:54 - Fim

  IF WL_0040-STATUS NE '' AND WL_0040-STATUS NE 'R'.
    STATUS_BTN = ABAP_TRUE.
  ELSE.

*#138092 -  ITSOUZA - 28.05.2024 15:59:54 - Inicio

    READ TABLE LT_ZSDT0042 TRANSPORTING NO FIELDS WITH KEY WITHT = 'FR'.
    IF SY-SUBRC NE 0.
      STATUS_BTN = ABAP_TRUE.
    ENDIF.
*    SELECT SINGLE *
*      FROM zsdt0043
*      INTO wl_0043
*       WHERE werks EQ wl_0040-vkbur
*         AND uname EQ sy-uname.
* #138092 -  ITSOUZA - 28.05.2024 15:59:54 - Fim

    IF WL_0043 IS INITIAL.
      STATUS_BTN = ABAP_TRUE.
    ENDIF.

*    COUNT = REDUCE INT4( INIT Y = 0 FOR LS2 IN TG_ITENS WHERE ( VBELN IS NOT INITIAL ) NEXT Y = Y + 1 ).
*    IF COUNT IS NOT INITIAL.
*      STATUS_BTN = ABAP_TRUE.
*    ENDIF.
  ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  IF TG_MSG_RET[] IS NOT INITIAL.
    STATUS_BTN = ABAP_TRUE.
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  IF STATUS_BTN IS NOT INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'BTN_FUN'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*#138092 - ITSOUZA - Inicio


  READ TABLE LT_ZSDT0042 TRANSPORTING NO FIELDS WITH KEY WITHT = 'FI'.
  IF SY-SUBRC EQ 0.
    IF WL_0043 IS NOT INITIAL.
      LOOP AT SCREEN.
        IF SCREEN-NAME EQ 'BTN_FI'.
          SCREEN-INVISIBLE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  CLEAR LT_ZSDT0042.
*#138092 - ITSOUZA - Fim
ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  DATA: WL_TOT_VLR TYPE ZSDT0041-ZWERT.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  DATA: WL_TEXTO_MSG   TYPE CHAR255.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  CASE SY-UCOMM.
    WHEN 'CANCEL'
      OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_FUN'.
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
      IF WG_0260-COMPRA_FIM_EXPORT = 'S'.
        "Simulador está vinculado a uma compra com Fins Exportação.
        MESSAGE TEXT-E83 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
      ELSE.
        IF ( WG_HEADER-FUNRURAL IS NOT INITIAL ) AND ( WG_HEADER-FUNUSER IS INITIAL ).
          " O Cliente está cadastrado na Exceção para a Não retenção do FUNRURAL.
          " Deseja prosseguir com a Porcentagem de INSS para valor cheio?
          CONCATENATE TEXT-I04 TEXT-I05 INTO WL_TEXTO_MSG.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TEXT_QUESTION         = WL_TEXTO_MSG
              TEXT_BUTTON_1         = 'Sim'
              TEXT_BUTTON_2         = 'Não'
              START_COLUMN          = 30
              START_ROW             = 8
              DISPLAY_CANCEL_BUTTON = ' '
            IMPORTING
              ANSWER                = LV_RESP.

          IF LV_RESP NE 1.
            LEAVE SCREEN.
          ELSE.
            SY-UCOMM = 'BTN_FUN'.
          ENDIF.
        ENDIF.
      ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
      PERFORM ATUALIZA_FUNRURAL.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      PERFORM MONTA_MEMO.
      PERFORM CALCULA_HEADER USING WL_TOT_VLR.
      PERFORM CALCULA_ITENS.
      PERFORM GRAVA_DADOS.

*  #138092 - ITSOUZA - Inicio
    WHEN 'BTN_FI'.

      WL_TEXTO_MSG = COND #( WHEN WG_HEADER-FUNDEINFRA_EXCE IS INITIAL THEN |{ TEXT-I08 } { TEXT-I09 }|
                             ELSE TEXT-I10 ).
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = WL_TEXTO_MSG
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          START_COLUMN          = 30
          START_ROW             = 8
          DISPLAY_CANCEL_BUTTON = ' '
        IMPORTING
          ANSWER                = LV_RESP.

      IF LV_RESP NE 1.
        LEAVE SCREEN.
      ELSE.
        SY-UCOMM = 'BTN_FI'.
      ENDIF.
      BTN_FI = ABAP_TRUE.
      PERFORM ATUALIZA_FUNDEINFRA.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      PERFORM MONTA_MEMO.
      PERFORM CALCULA_HEADER USING WL_TOT_VLR.
      PERFORM CALCULA_ITENS.
      PERFORM GRAVA_DADOS.
      PERFORM VERIFICA_ERROS.
*  #138092 - ITSOUZA - Fim
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_ANTECIPACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_ANTECIPACAO INPUT.
  CLEAR: WG_HEADER-ANTEC.
  IF WG_HEADER-TPSIM+1(1) EQ 'P'
    OR WG_HEADER-TPSIM+1(1) EQ 'S'.
**        Taxa de juros
    SELECT SINGLE TX_JUROS
      FROM ZSDT0039
      INTO WG_HEADER-ANTEC
       WHERE VAL_DE         LE SY-DATUM
         AND VAL_ATE        GE SY-DATUM.
*             AND FILIAL_ORIGEM  EQ WG_HEADER-VKBUR.

  ENDIF.
  IF WG_HEADER-ANTEC IS INITIAL.
    WG_HEADER-ANTEC = 12.
  ENDIF.
ENDMODULE.                 " BUSCA_ANTECIPACAO  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTA_MGLOBAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_MGLOBAL .
  DATA: BEGIN OF TL_ITENS_AUX OCCURS 0.
          INCLUDE TYPE TY_ITENS.
  DATA:   MATKL TYPE V023-WGBEZ,
        END OF TL_ITENS_AUX,

        BEGIN OF TL_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
          MATKL TYPE MARA-MATKL,
        END OF TL_MARA,

        BEGIN OF TL_V023 OCCURS 0,
          MATKL TYPE T023T-MATKL,
          WGBEZ TYPE T023T-WGBEZ,
        END OF TL_V023.

  DATA: WL_SOMA_MGCAD TYPE ZSDT0041-VLRTOT,
        WL_SOMA_MGEFE TYPE ZSDT0041-VLRTOT,
        WL_VLRTOT     TYPE ZSDT0041-VLRTOT.

  REFRESH: TL_ITENS_AUX, TL_MARA, TL_V023, TG_MGLOBAL.

  IF TG_ITENS[] IS NOT INITIAL.
    SELECT MATNR MATKL
      FROM MARA
      INTO TABLE TL_MARA
       FOR ALL ENTRIES IN TG_ITENS
        WHERE MATNR EQ TG_ITENS-MATNR.

    IF SY-SUBRC IS INITIAL.
      SELECT MATKL WGBEZ
        FROM T023T
        INTO TABLE TL_V023
         FOR ALL ENTRIES IN TL_MARA
         WHERE SPRAS EQ SY-LANGU
           AND MATKL EQ TL_MARA-MATKL.

    ENDIF.

  ENDIF.
  LOOP AT TG_ITENS.
    READ TABLE TL_MARA
      WITH KEY MATNR = TG_ITENS-MATNR.
    IF SY-SUBRC IS INITIAL.
      READ TABLE TL_V023
        WITH KEY MATKL = TL_MARA-MATKL.
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING: TG_ITENS TO TL_ITENS_AUX.
        MOVE: TL_V023-WGBEZ TO TL_ITENS_AUX-MATKL.

        APPEND TL_ITENS_AUX.
      ENDIF.
    ENDIF.
    CLEAR: TL_ITENS_AUX.
  ENDLOOP.

  SORT TL_V023 BY WGBEZ.
  DELETE ADJACENT DUPLICATES FROM TL_V023 COMPARING WGBEZ.

  SORT: TL_ITENS_AUX BY MATKL.
  CLEAR: WL_SOMA_MGCAD, WL_SOMA_MGEFE, WL_VLRTOT.
  LOOP AT TL_V023.
    LOOP AT TL_ITENS_AUX
      WHERE MATKL EQ TL_V023-WGBEZ.

      WL_SOMA_MGCAD = ( WL_SOMA_MGCAD + ( TL_ITENS_AUX-VLRTOT * ( TL_ITENS_AUX-MGCAD / 100 ) ) ).
      WL_SOMA_MGEFE = ( WL_SOMA_MGEFE + ( TL_ITENS_AUX-VLRTOT * ( TL_ITENS_AUX-MGEFE / 100 ) ) ).
      ADD TL_ITENS_AUX-VLRTOT TO WL_VLRTOT.

    ENDLOOP.
    TG_MGLOBAL-MGCAD = ( ( WL_SOMA_MGCAD / WL_VLRTOT ) * 100 ) .
    TG_MGLOBAL-MGEFE = ( ( WL_SOMA_MGEFE / WL_VLRTOT ) * 100 ) .
    TG_MGLOBAL-MATKL = TL_ITENS_AUX-MATKL.

    COLLECT TG_MGLOBAL.
    CLEAR: TG_MGLOBAL, WL_SOMA_MGCAD, WL_SOMA_MGEFE, WL_VLRTOT.
  ENDLOOP.
ENDFORM.                    " MONTA_MGLOBAL
*&---------------------------------------------------------------------*
*&      Form  MOTA_LAYOUT_MGLOBAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOTA_LAYOUT_MGLOBAL .
  REFRESH: ESTRUTURA.
  PERFORM MONTAR_ESTRUTURA_MGLOBAL USING:
  1  'T023T'      'WGBEZ'    'TG_MGLOBAL' 'MATKL'    'Grp.Mercadoria'  ' ',
  2  'ZSDT0041'   'MGCAD'    'TG_MGLOBAL' 'MGCAD'    'Mrg.Cadastrada' ' ',
  3  'ZSDT0041'   'MGEFE'    'TG_MGLOBAL' 'MGEFE'    'Mrg.Efetiva'  ' '.
ENDFORM.                    " MOTA_LAYOUT_MGLOBAL
*&---------------------------------------------------------------------*
*&      Form  EXIBE_MGBLOBAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXIBE_MGBLOBAL .

  DATA: WL_LAYOUT TYPE  SLIS_LAYOUT_ALV.
  WL_LAYOUT-ZEBRA = C_X.
  WL_LAYOUT-WINDOW_TITLEBAR = 'Simulador de Vendas - Insumos'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM    = V_REPORT
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      IS_LAYOUT             = WL_LAYOUT
      IT_FIELDCAT           = ESTRUTURA[]
*     IT_SORT               = T_SORT[]
      I_SAVE                = 'A'
      I_SCREEN_START_COLUMN = 3
      I_SCREEN_START_LINE   = 3
      I_SCREEN_END_COLUMN   = 51
      I_SCREEN_END_LINE     = 13
    TABLES
      T_OUTTAB              = TG_MGLOBAL.
ENDFORM.                    " EXIBE_MGBLOBAL
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
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
FORM MONTAR_ESTRUTURA_MGLOBAL USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

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
  WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  INPUTA_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INPUTA_DESC .
  DATA: BEGIN OF TL_DESCP OCCURS 0,
          FIELD(50),
        END OF TL_DESCP,

        BEGIN OF TL_FIELD OCCURS 0,
          TABNAME   TYPE DD03L-TABNAME,    "Nome da tabela
          FIELDNAME TYPE DD03L-FIELDNAME,    "Nome de campo
          s(1)      TYPE C,
        END OF TL_FIELD,

        BEGIN OF TL_VALUE OCCURS 0,
          TABNAME    TYPE DD03L-TABNAME,    "Nome da tabela
          FIELDNAME  TYPE DD03L-FIELDNAME,    "Nome de campo
          CHAR79(79) TYPE C,
        END OF TL_VALUE.

  DATA: TL_0046        TYPE TABLE OF ZSDT0046 WITH HEADER LINE,
        TL_0047        TYPE TABLE OF ZSDT0047 WITH HEADER LINE,
        TL_HEADING_TAB TYPE  F4TYP_HEADING_TAB,
        WL_HEADING_TAB LIKE LINE OF TL_HEADING_TAB,
        WL_INDEX       TYPE SY-TABIX,
        WL_CHAR(20).

  REFRESH: TL_0046, TL_0047, TL_VALUE, TL_FIELD, TL_DESCP, TL_HEADING_TAB.

  IF TG_ITENS[] IS NOT INITIAL.
    READ TABLE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
    SELECT *
      FROM ZSDT0046
      INTO TABLE TL_0046
       FOR ALL ENTRIES IN TG_ITENS
       WHERE VAL_DE  LE SY-DATUM
         AND VAL_ATE GE SY-DATUM
         AND AUART   EQ TG_ITENS-AUART
         AND BUKRS   EQ WA_T001K-BUKRS.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM ZSDT0047
        INTO TABLE TL_0047
         FOR ALL ENTRIES IN TL_0046
          WHERE SEQ_DESC EQ TL_0046-SEQ_DESC
            AND USNAM    EQ SY-UNAME.

      LOOP AT TL_0047.
        DELETE TL_0046 WHERE SEQ_DESC EQ TL_0047-SEQ_DESC.

      ENDLOOP.

      LOOP AT TL_0046.
        READ TABLE TL_DESCP TRANSPORTING NO FIELDS
          WITH KEY FIELD = TL_0046-SEQ_DESC.
        IF SY-SUBRC IS NOT INITIAL.
          MOVE: TL_0046-SEQ_DESC TO TL_DESCP-FIELD.
          APPEND TL_DESCP.
        ENDIF.

      ENDLOOP.

      TL_FIELD-TABNAME = 'ZSDT0046'.
      TL_FIELD-FIELDNAME = 'SEQ_DESC'.
      TL_FIELD-S = 'X'.
      APPEND  TL_FIELD.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          FIELDNAME                 = 'DESCONTO'
          TABNAME                   = 'ZSDT0041'
        IMPORTING
          INDEX                     = WL_INDEX
          SELECT_VALUE              = WL_CHAR
        TABLES
          FIELDS                    = TL_FIELD
          SELECT_VALUES             = TL_VALUE
          VALUETAB                  = TL_DESCP
          HEADING_TABLE             = TL_HEADING_TAB
        EXCEPTIONS
          FIELD_NOT_IN_DDIC         = 001
          MORE_THEN_ONE_SELECTFIELD = 002
          NO_SELECTFIELD            = 003.

      IF SY-SUBRC IS INITIAL.
        READ TABLE TL_DESCP INDEX WL_INDEX.
        LOOP AT TL_0046
           WHERE SEQ_DESC EQ TL_DESCP-FIELD.

          LOOP AT TG_ITENS
            WHERE AUART EQ TL_0046-AUART.

            MOVE TL_0046-VLR_DESC TO TG_ITENS-DESCONTO.

            MODIFY TABLE TG_ITENS.

          ENDLOOP.
          CLEAR: TG_ITENS, TL_0046.

        ENDLOOP.

        PERFORM CALCULA_ITENS.

        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
          EXPORTING
            FUNCTIONCODE           = '/00'
          EXCEPTIONS
            FUNCTION_NOT_SUPPORTED = 1.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " INPUTA_DESC
*&---------------------------------------------------------------------*
*&      Form  GERA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERA_ORDEM_VENDA.
  DATA: WL_VBELN    TYPE VBAK-VBELN,
        WL_ERRO(1),
        AT_CONTADOR TYPE INT4.

  CALL FUNCTION 'ZSDMF001_GERA_OV_SIMULADOR'
    EXPORTING
      I_DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO
    IMPORTING
      ERRO            = WL_ERRO.

  CHECK WL_ERRO IS INITIAL.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      FUNCTIONCODE           = C_ATUAL
    EXCEPTIONS
      FUNCTION_NOT_SUPPORTED = 1.

  AT_CONTADOR = 0.

  DO.
    IF AT_CONTADOR EQ 10.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = |Processando 40 { AT_CONTADOR }|.

    PERFORM REAJUSTE_41.
    ADD 1 TO AT_CONTADOR.

  ENDDO.

*-CS2019001753-05.01.2023-#65741-JT-inicio
  PERFORM F_CRIA_TABELAS_OV(SAPLZGFSD001) USING WG_HEADER-DOC_SIMULACAO.
*-CS2019001753-05.01.2023-#65741-JT-fim

ENDFORM.                    " GERA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MATNR .

  IF WG_HEADER-TPSIM IS NOT INITIAL.
    IF NOT WG_HEADER-VKBUR IS INITIAL.
      PERFORM ORGANIZA_MATNR.
      PERFORM EXIBE_MATNR.
    ELSE.
      MESSAGE 'Escritório de Venda não Informado!' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.                    " DISPLAY_MATNR
*&---------------------------------------------------------------------*
*&      Form  MOTA_LAYOUT_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_LAYOUT_MATNR .
  REFRESH: ESTRUTURA.
  PERFORM MONTAR_ESTRUTURA_MATNR USING:
     1  'MARA'       'MATNR'           'TG_MATNR'   'MATNR'           ' '  ' ',
     2  'MAKT'       'MAKTX'           'TG_MATNR'   'MAKTX'           ' '  ' ',
     3  'ZSDT0036'   'MEINS'           'TG_MATNR'   'MEINS'           ' '  ' ',
     4  'ZSDT0036'   'WERKS_FORNEC'    'TG_MATNR'   'WERKS_FORNEC'    ' '  ' ',
     5  'ZSDT0036'   'INCO1'           'TG_MATNR'   'INCO1'           ' '  ' '.
ENDFORM.                    " MONTA_LAYOUT_MATNR
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
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
FORM MONTAR_ESTRUTURA_MATNR USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

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
  WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  EXIBE_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXIBE_MATNR.
  DATA: WL_LAYOUT TYPE  SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTA_LAYOUT_MATNR.

  V_REPORT = SY-REPID.

  WL_LAYOUT-ZEBRA = C_X.
  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
  WL_LAYOUT-BOX_TABNAME  = 'TG_MATNR'.
  WL_LAYOUT-WINDOW_TITLEBAR = 'Simulador de Vendas - Material'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      IS_LAYOUT               = WL_LAYOUT
      IT_FIELDCAT             = ESTRUTURA[]
*     IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      I_SCREEN_START_COLUMN   = 3
      I_SCREEN_START_LINE     = 3
      I_SCREEN_END_COLUMN     = 95
      I_SCREEN_END_LINE       = 13
    TABLES
      T_OUTTAB                = TG_MATNR.

ENDFORM.                    " EXIBE_MATNR
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_MATNR .
  TYPES: BEGIN OF TYL_MARA,
           MATNR TYPE MARA-MATNR,
           MATKL TYPE SETLEAF-VALFROM,
           MTART TYPE MTART,
         END OF TYL_MARA.

  DATA: BEGIN OF WL_0036,
          VAL_DE       TYPE ZSDT0036-VAL_DE,
          VAL_ATE      TYPE ZSDT0036-VAL_ATE,
          DTVENC       TYPE ZSDT0036-DTVENC,
          MATNR        TYPE ZSDT0036-MATNR,
          WAERK        TYPE ZSDT0036-WAERK,
          INCO1        TYPE ZSDT0036-INCO1,
          SAFRA        TYPE ZSDT0036-SAFRA,
          CULTURA      TYPE ZSDT0036-CULTURA,
          MEINS        TYPE ZSDT0036-MEINS,
          WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
          VLR_CUSTO    TYPE ZSDT0036-VLR_CUSTO,
          PERC_MARGEM  TYPE ZSDT0036-PERC_MARGEM,
          VLR_MARGEM   TYPE ZSDT0036-VLR_MARGEM,
          VLR_VENDA    TYPE ZSDT0036-VLR_VENDA,
          LOEKZ        TYPE ZSDT0036-LOEKZ,
          BUKRS        TYPE ZSDT0036-BUKRS,
        END OF WL_0036.

  DATA: TL_0036     LIKE TABLE OF WL_0036,
        TL_MAKT     TYPE TABLE OF MAKT,
        WL_MAKT     TYPE MAKT,
        TL_SETLEAF  TYPE TABLE OF SETLEAF,
        WL_SETLEAF  TYPE SETLEAF,
        TL_SETLINET TYPE TABLE OF SETLINET,
        WL_SETLINET TYPE SETLINET,
        TL_MARA     TYPE TABLE OF TYL_MARA,
        WL_MARA     TYPE TYL_MARA.

  DATA: RGL_MATNR  TYPE RANGE OF MARA-MATNR,
        WR_MATNR   LIKE LINE OF RGL_MATNR,
        RG_CULTURA TYPE RANGE OF ZSDT0036-CULTURA,
        WR_CULTURA LIKE LINE OF RG_CULTURA.

  REFRESH: RGL_MATNR, TL_MAKT, TL_0036, TL_MARA, TL_SETLINET, TL_SETLEAF, RG_CULTURA.
  CLEAR: RGL_MATNR, WR_MATNR, WL_MAKT, WL_0036,
         WL_SETLEAF, WL_SETLINET, WL_MARA.

  IF WG_HEADER-CULTURA IS NOT INITIAL.
    WR_CULTURA-SIGN    = 'I'.
    WR_CULTURA-OPTION  = 'EQ'.
    WR_CULTURA-LOW     = WG_HEADER-CULTURA.

    APPEND WR_CULTURA TO RG_CULTURA.
    CLEAR: WR_CULTURA.
  ENDIF.
  WR_CULTURA-SIGN    = 'I'.
  WR_CULTURA-OPTION  = 'EQ'.
  WR_CULTURA-LOW     =  SPACE.

  APPEND WR_CULTURA TO RG_CULTURA.
  CLEAR: WR_CULTURA.


  SELECT VAL_DE VAL_ATE DTVENC MATNR WAERK INCO1 SAFRA CULTURA MEINS
         WERKS_FORNEC VLR_CUSTO PERC_MARGEM VLR_MARGEM VLR_VENDA LOEKZ BUKRS
    FROM ZSDT0036
    INTO TABLE TL_0036
      WHERE LOEKZ     EQ SPACE
        AND WAERK     EQ WG_HEADER-WAERK
        AND CULTURA   IN RG_CULTURA
        AND SAFRA     EQ WG_HEADER-SAFRA
        AND ELIMINADO EQ SPACE
        AND BUKRS     EQ WA_T001K-BUKRS.


  LOOP AT TL_0036 INTO WL_0036.
    IF WL_0036-CULTURA IS NOT INITIAL
    AND WL_0036-CULTURA NE WG_HEADER-CULTURA.
      DELETE TABLE TL_0036 FROM WL_0036.
    ENDIF.

    IF  WL_0036-VAL_DE  LE SY-DATUM
   AND  WL_0036-VAL_ATE GE SY-DATUM.
    ELSE.
      DELETE TABLE TL_0036 FROM WL_0036.
    ENDIF.
  ENDLOOP.

  IF TL_0036[] IS NOT INITIAL.
    SELECT *
      FROM MAKT
      INTO TABLE TL_MAKT
       FOR ALL ENTRIES IN TL_0036
        WHERE MATNR EQ TL_0036-MATNR
          AND SPRAS EQ SY-LANGU.

    SELECT MATNR MATKL MTART
      FROM MARA
      INTO TABLE TL_MARA
       FOR ALL ENTRIES IN TL_0036
       WHERE MATNR EQ TL_0036-MATNR.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM SETLEAF
        INTO TABLE TL_SETLEAF
         FOR ALL ENTRIES IN TL_MARA
         WHERE SETNAME EQ 'MAGGI_ZSDT0044_02'
          AND VALFROM  EQ TL_MARA-MATKL.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM SETLINET
          INTO TABLE TL_SETLINET
          FOR ALL ENTRIES IN TL_SETLEAF
           WHERE SETNAME EQ 'MAGGI_ZSDT0044_02'
             AND LANGU   EQ SY-LANGU
             AND LINEID  EQ TL_SETLEAF-LINEID.
      ENDIF.

    ENDIF.

  ENDIF.

  REFRESH TG_MATNR.
  CLEAR TG_MATNR.
  SORT: TL_MAKT     BY MATNR,
        TL_MARA     BY MATNR,
        TL_SETLEAF  BY VALFROM,
        TL_SETLINET BY LINEID.

  LOOP AT TL_0036 INTO WL_0036.
    READ TABLE TL_MAKT INTO WL_MAKT
      WITH KEY MATNR = WL_0036-MATNR
               BINARY SEARCH.

    READ TABLE TL_MARA INTO WL_MARA
      WITH KEY MATNR = WL_0036-MATNR
               BINARY SEARCH.

    READ TABLE TL_SETLEAF INTO WL_SETLEAF
          WITH KEY VALFROM = WL_MARA-MATKL.
    IF SY-SUBRC IS INITIAL.
      READ TABLE TL_SETLINET INTO WL_SETLINET
        WITH KEY LINEID = WL_SETLEAF-LINEID.

      MOVE: WL_SETLINET-DESCRIPT TO TG_MATNR-AUART.
    ELSE.
      CLEAR: TG_MATNR-AUART.
    ENDIF.

    IF TG_MATNR-AUART IS NOT INITIAL.
      READ TABLE TG_SETLEAF INTO TG_SETLEAF
        WITH KEY VALFROM = TG_MATNR-AUART.

      IF SY-SUBRC IS INITIAL.
        READ TABLE TG_SETLINET INTO TG_SETLINET
          WITH KEY LINEID = TG_SETLEAF-LINEID.

        MOVE: TG_SETLINET-DESCRIPT TO TG_MATNR-SPART.
      ENDIF.
*      comentado o ZDEF conforme chamado CS2016000364
      IF  TG_MATNR-AUART EQ 'ZFTE' OR
          TG_MATNR-AUART EQ 'YFTE'. "165455 - PQ 24.02.25
*        TG_MATNR-AUART EQ 'ZDEF' OR

        MOVE: WG_HEADER-SAFRA TO TG_MATNR-CHARG.
      ENDIF.
    ELSE.
      CLEAR: TG_MATNR-SPART.
    ENDIF.

**** Clear no campo Charg CS2016001142>>>>>
    CASE TG_MATNR-AUART.
      WHEN 'ZFTE' OR 'ZOFE' OR 'YFTE'. "165455 - PQ 24.02.25
*        IF wl_0036-werks_fornec EQ '0175' AND wl_mara-mtart EQ 'ZFER'.
        IF WL_0036-WERKS_FORNEC IN R_WERKS AND WL_MARA-MTART EQ 'ZFER'.
          CLEAR: TG_MATNR-CHARG.
        ENDIF.
    ENDCASE.
**** Clear no campo Charg CS2016001142<<<<<


    MOVE-CORRESPONDING: WL_0036 TO TG_MATNR.
    MOVE:  WL_MAKT-MAKTX        TO TG_MATNR-MAKTX,
           WL_0036-WERKS_FORNEC TO TG_MATNR-WERKS_FORNEC,
           WL_0036-INCO1        TO TG_MATNR-INCO1,
           WL_0036-DTVENC       TO TG_MATNR-DTVENC.

    APPEND TG_MATNR.
    CLEAR: TG_MATNR.
  ENDLOOP.

  SORT TG_MATNR BY AUART.

ENDFORM.                    " ORGANIZA_MATNR
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS .

  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_4780   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD..     "#EC CALLED

  DATA: TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
        WL_ITENS     LIKE LINE OF TG_ITENS,
        WL_LINES     TYPE SY-TABIX.
  REFRESH: TL_ITENS_AUX.

  CASE SY-UCOMM.

    WHEN '&ONT'.
      IF WG_HEADER-TPSIM IS NOT INITIAL.
        TL_ITENS_AUX[] = TG_ITENS[].
        REFRESH: TG_ITENS.
        LOOP AT TL_ITENS_AUX INTO WL_ITENS.
          WL_ITENS-POSNR = SY-TABIX * 10.
          APPEND WL_ITENS TO TG_ITENS.
        ENDLOOP.
        CLEAR: WL_ITENS, WL_LINES.
        DESCRIBE TABLE TG_ITENS LINES WL_LINES.

        LOOP AT TG_MATNR WHERE MARK IS NOT INITIAL.

          SELECT SINGLE VLR_FRETE
              FROM ZSDT0037
              INTO (WL_ITENS-VLR_FRETE)
                 WHERE VAL_DE          LE SY-DATUM
                   AND VAL_ATE         GE SY-DATUM
                   AND MEINS           EQ TG_MATNR-MEINS
                   AND FILIAL_ORIGEM   EQ TG_MATNR-WERKS_FORNEC
                   AND FILIAL_DESTINO  EQ WG_HEADER-VKBUR
                   AND MATKL           EQ ( SELECT MATKL FROM MARA WHERE MATNR EQ TG_MATNR-MATNR )
                   AND WAERS           EQ 'BRL'
                   AND BUKRS           EQ WA_T001K-BUKRS.

          CASE TG_MATNR-INCO1.
            WHEN 'CIF' OR 'CPT' OR 'CFR'.
*              PERFORM ATUALIZA_FRETE CHANGING WL_ITENS-VLR_FRETE.
            WHEN OTHERS.
              WL_ITENS-VLR_FRETE = 0.
          ENDCASE.

          ADD 1 TO WL_LINES.
          WL_ITENS-POSNR = WL_LINES * 10 .
          WL_ITENS-MATNR  = TG_MATNR-MATNR.
          WL_ITENS-MAKTX  = TG_MATNR-MAKTX.
          WL_ITENS-ZIEME  = TG_MATNR-MEINS.
          WL_ITENS-WERKS  = TG_MATNR-WERKS_FORNEC.
          WL_ITENS-INCO1  = TG_MATNR-INCO1.
          WL_ITENS-AUART  = TG_MATNR-AUART.
          WL_ITENS-CHARG  = TG_MATNR-CHARG.
          WL_ITENS-SPART  = TG_MATNR-SPART.
          WL_ITENS-DTVENC = TG_MATNR-DTVENC.

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
          IF WL_ITENS-CULTURA_APL IS INITIAL.
            IF WG_CULTURA_APL IS NOT INITIAL.
              WL_ITENS-CULTURA_APL = WG_CULTURA_APL.
            ENDIF.
          ENDIF.

          IF WL_ITENS-SAFRA_APL IS INITIAL.
            WL_ITENS-SAFRA_APL = WG_HEADER-SAFRA.
          ENDIF.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

          APPEND WL_ITENS TO TG_ITENS.
        ENDLOOP.

        PERFORM CALCULA_ITENS.

      ENDIF.
      CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
  ENDCASE.
ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR INPUT.
  DATA: L_CURSOR TYPE T_CURSOR.

  GET CURSOR FIELD L_CURSOR-FNAME LINE L_CURSOR-POS AREA L_CURSOR-TC
             VALUE L_CURSOR-VALUE.

  IF L_CURSOR-FNAME EQ  'WG_HEADER-WAERK'
  OR L_CURSOR-FNAME EQ  'WG_HEADER-SAFRA'
  OR L_CURSOR-FNAME EQ  'WG_HEADER-VKBUR'
  OR L_CURSOR-FNAME EQ  'WG_HEADER-CULTURA'.
    PERFORM CALCULA_ITENS.
  ENDIF.

ENDMODULE.                 " GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WL_VBELN  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM F_SHDB_FAT  TABLES IT_MSG
                 CHANGING P_VBELN
                          P_ERRO
                          P_DOC.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
     'SAPMV60A'  '0102'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '/00',
     ''          ''      ''   'KOMFK-VBELN(01)'    P_VBELN,

     'SAPMV60A'  '0104'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '=SICH'.


  CLEAR P_ERRO.
  PERFORM ZF_CALL_TRANSACTION TABLES IT_MSG USING 'VF01' CHANGING P_ERRO P_DOC.
ENDFORM.                    " F_SHDB_FAT

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.

  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT
*&---------------------------------------------------------------------*
FORM BATCH_INPUT  USING     VALUE(P_FLAG)
                            VALUE(P_FNAM)
                            VALUE(P_FVAL).

  CLEAR WA_BDCDATA.

  IF NOT P_FLAG IS INITIAL.
    WA_BDCDATA-PROGRAM  = P_FNAM.
    WA_BDCDATA-DYNPRO   = P_FVAL.
    WA_BDCDATA-DYNBEGIN = 'X'.
  ELSE.
    WA_BDCDATA-FNAM = P_FNAM.
    WA_BDCDATA-FVAL = P_FVAL.
  ENDIF.

  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    "batch_input

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION TABLES IT_MSG_RET USING P_TRANS CHANGING P_ERRO P_DOC.

  DATA: C_MSGID LIKE IT_MSG-MSGID VALUE 'VF',
        C_MSGNR LIKE IT_MSG-MSGNR VALUE '311'.

  REFRESH IT_MSG.

  IF P_TRANS EQ 'FB08'.
    C_MSGID = 'F5'.
    C_MSGNR = '312'.
  ENDIF.

  WL_MODE = 'E'.
  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        MESSAGES INTO IT_MSG.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
  IF SY-SUBRC = 0.
    P_ERRO = 'X'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      P_ERRO = 'X'.
    ENDIF.
  ENDIF.

  CLEAR WG_DOCUMENTO.

  READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                             MSGNR = C_MSGNR
                             MSGTYP = 'S'.

  IF SY-SUBRC = 0.
    MOVE IT_MSG-MSGV1 TO WG_DOCUMENTO.
  ENDIF.

  IF  WG_DOCUMENTO IS INITIAL.
    P_ERRO = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_DOCUMENTO
      IMPORTING
        OUTPUT = WG_DOCUMENTO.
  ENDIF.
  P_DOC = WG_DOCUMENTO.
  IT_MSG_RET[] = IT_MSG[].

ENDFORM.                    "ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_INFOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS_INFOR OUTPUT.
  DATA: TL_TSPAT TYPE TABLE OF TSPAT WITH HEADER LINE.

**  Seleciona dados a serem exibidosno ALV com base no ALV principal
  SELECT *
    FROM TSPAT
    INTO TABLE TL_TSPAT
    FOR ALL ENTRIES IN TG_ITENS
    WHERE SPART EQ TG_ITENS-SPART
     AND  SPRAS EQ SY-LANGU.

  FREE TG_INFOR.
  CLEAR TG_INFOR.

  LOOP AT TG_ITENS.
    READ TABLE TG_INFOR WITH KEY SPART = TG_ITENS-SPART.
    IF SY-SUBRC IS INITIAL.
      ADD TG_ITENS-VLRTOT TO TG_INFOR-VLRTOT.
      MODIFY TG_INFOR TRANSPORTING VLRTOT WHERE SPART = TG_ITENS-SPART.
    ELSE.
      MOVE-CORRESPONDING TG_ITENS TO TG_INFOR.
      READ TABLE TL_TSPAT WITH KEY SPART = TG_ITENS-SPART.
      TG_INFOR-VTEXT = TL_TSPAT-VTEXT.
      APPEND TG_INFOR.
    ENDIF.
  ENDLOOP.

  IF OBJ_CONT_INFOR IS INITIAL.

    WA_LAYOUT-ZEBRA      = ABAP_TRUE.
    WA_LAYOUT-NO_ROWMARK = ABAP_TRUE.
*    WA_STABLE-ROW        = ABAP_TRUE.

    CREATE OBJECT OBJ_CONT_INFOR
      EXPORTING
        CONTAINER_NAME = 'OBJ_INFOR'.

    CREATE OBJECT OBJ_GRID_INFOR
      EXPORTING
        I_PARENT = OBJ_CONT_INFOR.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM MONTAR_LAYOUT_INFOR.

    CALL METHOD OBJ_GRID_INFOR->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
*       i_save               = 'X'
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCAT_INFOR[]
        IT_OUTTAB            = TG_INFOR[].

  ELSE.
    CALL METHOD OBJ_GRID_INFOR->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS_INFOR  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ESC_VEDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->C_ERRO     text
*----------------------------------------------------------------------*
FORM F_VALIDAR_ESC_VENDA USING    P_VKBUR TYPE ZSDT0040-VKBUR
                        CHANGING C_ERRO  TYPE SY-SUBRC.
  DATA: TL_0060 TYPE TABLE OF ZSDT0060 WITH HEADER LINE,
        LV_ERRO TYPE I,
        LV_MSG  TYPE C LENGTH 255.

  CLEAR C_ERRO.

  SELECT *
    INTO TABLE TL_0060
    FROM ZSDT0060
    WHERE USNAM EQ SY-UNAME
     AND  PROGRAMA EQ SY-CPROG.

  READ TABLE TL_0060 WITH KEY VKBUR = P_VKBUR.
  IF SY-SUBRC IS NOT INITIAL.
    CONCATENATE 'Sem permissão para visualizar vendas do escritório ' P_VKBUR '.' INTO LV_MSG SEPARATED BY SPACE.
    MESSAGE LV_MSG TYPE 'E' DISPLAY LIKE 'I'.
    SY-SUBRC = LV_ERRO.

  ENDIF.

ENDFORM.                    "F_VALIDA_ESC_VEDA
*&---------------------------------------------------------------------*
*&      Form  IMPRIMI_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_HEADER_DOC_SIMULACAO  text
*----------------------------------------------------------------------*
FORM IMPRIMI_CONTRATO.

*  SELECT SINGLE * FROM ZSDT0040 INTO WA_0041
*    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
*
*  IF WA_0041 IS NOT INITIAL.
*
*    MOVE WA_0041-DT_ENTREGA_SEM TO WA_PRINT-DT_ENTREGA_SEM.
*    MOVE WA_0041-DT_ENTREGA_DEF TO WA_PRINT-DT_ENTREGA_DEF.
*    MOVE WA_0041-DT_ENTREGA_FET TO WA_PRINT-DT_ENTREGA_FET.
*    MOVE WA_0041-JUROS_MORA     TO WA_PRINT-JUROS_MORA.
*    MOVE WA_0041-FIADOR_01      TO WA_PRINT-FIADOR_01.
*    MOVE WA_0041-FIADOR_02      TO WA_PRINT-FIADOR_02.
*
*    IF WA_0041-PAG_PRORROGADO IS NOT INITIAL.
*      MOVE WA_0041-PAG_PRORROGADO TO WA_PRINT-CHECKYES.
*    ELSE.
*      MOVE 'X' TO WA_PRINT-CHECKYES.
*    ENDIF.
*
*  ENDIF.

*  CALL FUNCTION 'K_KKB_POPUP_RADIO2'
*    EXPORTING
*      I_TITLE   = |Imprimir Ordens da S.V?|
*      I_TEXT1   = |SIM!|
*      I_TEXT2   = |NÃO!|
*      I_DEFAULT = '1'
*    IMPORTING
*      I_RESULT  = W_ANSWER
*    EXCEPTIONS
*      CANCEL    = 1
*      OTHERS    = 2.



  SELECT COUNT(*)
    FROM ZSDT0090
    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
    AND FLAG NE ABAP_TRUE
    AND CATEGORIA EQ 'O'
    AND ESTORNO EQ ABAP_FALSE.

  IF SY-SUBRC IS INITIAL.
    MESSAGE |Agregar Desconto para Emitir o Contrato! | TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*--------------------------------------------
* este bloco esta sendo implementado pelo JAime - Projeto Insumos
* Gerador de Documentos
*--------------------------------------------
*-CS2019001753-04.01.2023-#65741-JT-inicio
  SELECT SINGLE NR_VENDA
    FROM ZSDT0310
   WHERE NR_VENDA      = @WG_HEADER-DOC_SIMULACAO
     AND TIPO_DOC      = 'CTR'
     AND ID_DOCUMENTO IS NOT INITIAL
     AND STATUS       <> '10'
    INTO @DATA(L_NR_VENDA). "Cancelado

  IF SY-SUBRC = 0.
    CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
      EXPORTING
        I_POPUP         = ABAP_TRUE
        I_SINTET        = ABAP_FALSE
        I_ANALIT        = ABAP_TRUE
        I_DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO
        I_DOCTOD        = ABAP_TRUE
        I_TODOS         = ABAP_TRUE.
  ELSE.
*-CS2019001753-04.01.2023-#65741-JT-fim
    CALL SCREEN 300 STARTING AT 35 05.
  ENDIF.
******  CALL SCREEN 210 ENDING AT 68 15 STARTING AT 3 3.

ENDFORM.                    " IMPRIMI_CONTRATO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0210 OUTPUT.
  SET PF-STATUS 'S0210'.
  SET TITLEBAR 'T0210'.
  DATA: CONT TYPE SY-SUBRC.

  CLEAR: CONT.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'ZSEM'.  CONT = CONT + SY-SUBRC.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'ZOSM'.  CONT = CONT + SY-SUBRC.
  IF CONT > 4.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'ZSDT0040-DT_ENTREGA_SEM'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        WHEN 'WA_PRINT-DT_ENTREGA_SEM'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  CLEAR: CONT.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'ZFTE'.  CONT = CONT + SY-SUBRC.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'ZOFE'.  CONT = CONT + SY-SUBRC.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'YFTE'.  CONT = CONT + SY-SUBRC. "165455 - PQ 24.02.25
  IF CONT > 8. "165455 - PQ 24.02.25
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'ZSDT0040-DT_ENTREGA_FET'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        WHEN 'WA_PRINT-DT_ENTREGA_FET'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  CLEAR: CONT.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'ZDEF'.  CONT = CONT + SY-SUBRC.
  READ TABLE TG_ITENS TRANSPORTING NO FIELDS WITH KEY AUART = 'ZODF'.  CONT = CONT + SY-SUBRC.
  IF CONT > 4.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'ZSDT0040-DT_ENTREGA_DEF'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        WHEN 'WA_PRINT-DT_ENTREGA_DEF'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  IF WA_PRINT-FIADOR_01 IS NOT INITIAL.
    SELECT SINGLE NAME1
      FROM KNA1
      INTO (FIADOR_01)
      WHERE KUNNR EQ WA_PRINT-FIADOR_01.
  ENDIF.

  IF WA_PRINT-FIADOR_02 IS NOT INITIAL.
    SELECT SINGLE NAME1
    FROM KNA1
    INTO (FIADOR_02)
    WHERE KUNNR EQ WA_PRINT-FIADOR_02.
  ENDIF.

ENDMODULE.                 " STATUS_0210  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0210 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAVEC'.
      PERFORM ADD_CAMPOS USING WG_HEADER-DOC_SIMULACAO.
    WHEN 'CANCELC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0210  INPUT

*&---------------------------------------------------------------------*
*&      Form  ADD_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_CAMPOS USING DOC_SIMULACAO.

  DATA: VL_FORMNAME TYPE TDSFNAME,
        VL_NAME     TYPE RS38L_FNAM,
        I_VBELN     TYPE VBELN.

  WA_0041-DT_ENTREGA_SEM = WA_PRINT-DT_ENTREGA_SEM.
  WA_0041-DT_ENTREGA_DEF = WA_PRINT-DT_ENTREGA_DEF.
  WA_0041-DT_ENTREGA_FET = WA_PRINT-DT_ENTREGA_FET.
  WA_0041-JUROS_MORA     = WA_PRINT-JUROS_MORA.
  WA_0041-FIADOR_01      = WA_PRINT-FIADOR_01.
  WA_0041-FIADOR_02      = WA_PRINT-FIADOR_02.


  IF SIM EQ 'X'.
    WA_0041-PAG_PRORROGADO =  SIM.
  ELSEIF NAO EQ 'X'.
    WA_0041-PAG_PRORROGADO =  SIM.
  ENDIF.

  IF DOC_SIMULACAO IS NOT INITIAL.
*    UPDATE ZSDT0040 SET DT_ENTREGA_SEM = WA_0041-DT_ENTREGA_SEM
*                        DT_ENTREGA_DEF = WA_0041-DT_ENTREGA_DEF
*                        DT_ENTREGA_FET = WA_0041-DT_ENTREGA_FET
*                        JUROS_MORA     = WA_0041-JUROS_MORA
*                        FIADOR_01      = WA_0041-FIADOR_01
*                        FIADOR_02      = WA_0041-FIADOR_02
*                        PAG_PRORROGADO = WA_0041-PAG_PRORROGADO
*                        WHERE DOC_SIMULACAO EQ DOC_SIMULACAO.
*
*    COMMIT WORK.
*    WAIT UP TO 2 SECONDS.

    PERFORM IMPRIME_CONTRATO USING DOC_SIMULACAO.

  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.                    " ADD_CAMPOS
*&-----------------------------------------------------.----------------*
*&      Form  IMPRIME_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIME_CONTRATO USING I_DOC.

  CALL FUNCTION 'Z_GERA_CONTRATO'
    EXPORTING
      I_DOC = I_DOC
      I_DIR = W_ANSWER.

ENDFORM.                    " UPPER_LOWER
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_ORDEM_VENDA .
  DATA TG_ITENS_41 TYPE TABLE OF ZSDT0041.

  FIELD-SYMBOLS <W0041> TYPE ZSDT0041.

  CLEAR TG_ITENS_41.

  SORT IT_ITENS_ID BY INDEX.
  DELETE ADJACENT DUPLICATES FROM IT_ITENS_ID COMPARING INDEX.

  CHECK NOT IT_ITENS_ID IS INITIAL.

  SELECT *
    FROM ZSDT0041
    INTO TABLE TG_ITENS_41
 FOR ALL ENTRIES IN IT_ITENS_ID
   WHERE DOC_SIMULACAO EQ IT_ITENS_ID-DOC
     AND POSNR         EQ IT_ITENS_ID-POSNR
     AND VBELN         EQ IT_ITENS_ID-VBELN.

  IF ( TG_ITENS_41 IS NOT INITIAL ).
    CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR'
      TABLES
        TI_ITENS_OV       = TG_ITENS_41
      EXCEPTIONS
        OV_NAO_ENCONTRADA = 1
        OTHERS            = 2.

    LOOP AT TG_ITENS_41 ASSIGNING <W0041>.
      <W0041>-DESC_ABSOLUTO = IT_ITENS_ID[ DOC = <W0041>-DOC_SIMULACAO
                                         POSNR = <W0041>-POSNR
                                         VBELN = <W0041>-VBELN ]-VLR_DIF.
      <W0041>-DESC_ABSOLUTO = <W0041>-DESC_ABSOLUTO * -1.
    ENDLOOP.

*    DISPARO DO HEDGE PARA DESCONTO ABSOLUTO "VENDA"
    IF WG_HEADER-WAERK EQ 'BRL'.
      CASE WG_HEADER-TPSIM.
        WHEN 'BN' OR 'PM'.
        WHEN OTHERS.
          ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WG_HEADER-DOC_SIMULACAO
                                                  I_ACAO   = 'DSC_ABS'
                                                  I_TIPO   = 'VDI'
                                                  I_ITENS  = TG_ITENS_41[]
                                                  ).
      ENDCASE.
    ENDIF.

*    DISPARO DO HEDGE PARA DESCONTO ABSOLUTO "FRETE"
    IF WG_HEADER-TPSIM NE 'PM'.
      ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_NUMERO = WG_HEADER-DOC_SIMULACAO
                                              I_ACAO   = 'DSC_ABS'
                                              I_TIPO   = 'FRI'
                                              I_ITENS  = TG_ITENS_41[]
                                              ).
    ENDIF.
  ENDIF.

  FREE IT_ITENS_ID.
ENDFORM.                    " ATUALIZA_ORDEM_VENDA
*&---------------------------------------------------------------------*
*&      Form  F_VAL_SACAS_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM F_VAL_SACAS_USER  CHANGING C_ERRO.

  CLEAR C_ERRO.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_ZSDT0044_06'
    TABLES
      SET_VALUES    = T_USERMD
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  SORT T_USERMD BY FROM.
  READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.

  C_ERRO = SY-SUBRC.


ENDFORM.                    " F_VAL_SACAS_USER

*&---------------------------------------------------------------------*
*&      Form  f_format-value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALOR    STRING
*----------------------------------------------------------------------*
FORM F_FORMAT_VALUE CHANGING P_VALOR.

  DATA:
    OBJ_JS    TYPE REF TO CL_JAVA_SCRIPT,
    MY_SCRIPT TYPE STRING.

  CONCATENATE 'function numeroParaMoeda(n, c, d, t)'
              '{'
              'c = isNaN(c = Math.abs(c)) ? 2 : c,'
              'd = d == undefined ? "," : d,'
              't = t == undefined ? "." : t,'
              's = n < 0 ? "-" : "",'
              'i = parseInt(n = Math.abs(+n || 0).toFixed(c)) + "",'
              'j = (j = i.length) > 3 ? j % 3 : 0;'
              'return s + (j ? i.substr(0, j) + t : "") +'
              'i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) +'
              '(c ? d + Math.abs(n - i).toFixed(c).slice(2) : "");'
              '}'
              'var vlr = numeroParaMoeda (' P_VALOR ')'
              INTO MY_SCRIPT SEPARATED BY CL_ABAP_CHAR_UTILITIES=>CR_LF.

  OBJ_JS = CL_JAVA_SCRIPT=>CREATE( ).

  OBJ_JS->COMPILE( EXPORTING SCRIPT_NAME = 'script.js'
                             SCRIPT      = MY_SCRIPT ).

  OBJ_JS->EXECUTE( EXPORTING SCRIPT_NAME = 'script.js' ).
  P_VALOR = OBJ_JS->GET( NAME = 'vlr' ).

ENDFORM.                    "f_format-value
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_0103 OUTPUT.

  DATA: VALUES       TYPE VRM_VALUES WITH HEADER LINE.
  FIELD-SYMBOLS: <FS_CAMPO> TYPE ANY.
  DATA: IT_VBFA TYPE TABLE OF VBFA,
        WA_VBFA TYPE VBFA,
        IT_VBAP TYPE TABLE OF VBAP,
        WA_VBAP TYPE VBAP,
        IT_VBAK TYPE TABLE OF VBAK,
        WA_VBAK TYPE VBAK,
        IT_41   TYPE TABLE OF ZSDT0041,
        IT_90   TYPE TABLE OF ZSDT0090 WITH HEADER LINE,
        WA_41   TYPE ZSDT0041.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  DATA: VALUES_CPG_ANT TYPE VRM_VALUES WITH HEADER LINE,
        VALUES_CPG     TYPE VRM_VALUES WITH HEADER LINE.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
                        USING SY-UNAME.


  IF INIT IS INITIAL.
    SELECT *
      FROM SETLEAF
      INTO TABLE TG_SETLEAF_CULT
       WHERE SETNAME EQ 'MAGGI_ZSDT0044_03'.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM SETLINET
        INTO TABLE TG_SETLINET_CULT
        FOR ALL ENTRIES IN TG_SETLEAF_CULT
         WHERE SETNAME EQ TG_SETLEAF_CULT-SETNAME
           AND LANGU   EQ SY-LANGU
           AND LINEID  EQ TG_SETLEAF_CULT-LINEID.
    ENDIF.
    LOOP AT TG_SETLEAF_CULT.
      READ TABLE TG_SETLINET_CULT
        WITH KEY SETNAME = TG_SETLEAF_CULT-SETNAME
                 LINEID  = TG_SETLEAF_CULT-LINEID.
      IF SY-SUBRC IS INITIAL.

        VALUES-TEXT = TG_SETLINET_CULT-DESCRIPT.
        VALUES-KEY  = TG_SETLEAF_CULT-VALFROM.
        APPEND VALUES.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID              = 'WG_HEADER-TPCULT'
        VALUES          = VALUES[]
      EXCEPTIONS
        ID_ILLEGAL_NAME = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    REFRESH: VALUES.
    CLEAR: VALUES.

    VALUES-TEXT = 'CIF'.
    VALUES-KEY  = 'CIF'.
    APPEND VALUES.

    VALUES-TEXT = 'FOB'.
    VALUES-KEY  = 'FOB'.
    APPEND VALUES.

    VALUES-TEXT = 'CPT'.
    VALUES-KEY  = 'CPT'.
    APPEND VALUES.

    VALUES-TEXT = 'CFR'.
    VALUES-KEY  = 'CFR'.
    APPEND VALUES.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID              = 'WG_HEADER-FCULT'
        VALUES          = VALUES[]
      EXCEPTIONS
        ID_ILLEGAL_NAME = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.


    ENDIF.

    REFRESH VALUES.
    VALUES-TEXT = 'Adiantamento'.
    VALUES-KEY  = 'AD'.
    APPEND VALUES.

    VALUES-TEXT = 'Venda A Vista'.
    VALUES-KEY  = 'VV'.
    APPEND VALUES.

    VALUES-TEXT = 'Venda A Prazo'.
    VALUES-KEY  = 'VP'.
    APPEND VALUES.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    VALUES-TEXT = 'Troca Safra'.
    VALUES-KEY  = 'TS'.
    APPEND VALUES.

    VALUES-TEXT = 'Troca a Vista'.
    VALUES-KEY  = 'TV'.
    APPEND VALUES.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025

    VALUES-TEXT = 'Troca'.
    VALUES-KEY  = 'TT'.
    APPEND VALUES.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

    " Usuários que podem ter acesso as condições de pagamento adicionais
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS         = '0000'
        SETNR         = 'MAGGI_ZSDT0044_04'
      TABLES
        SET_VALUES    = T_USERMD
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT T_USERMD BY FROM.
    READ TABLE T_USERMD WITH KEY FROM = SY-UNAME.
    IF SY-SUBRC = 0.
      VALUES-TEXT = 'Venda Futura'.
      VALUES-KEY  = 'VF'.
      APPEND VALUES.

      VALUES-TEXT = 'Bonificação'.
      VALUES-KEY  = 'BN'.
      APPEND VALUES.

      VALUES-TEXT = 'Permuta'.
      VALUES-KEY  = 'PM'.
      APPEND VALUES.
    ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    VALUES_CPG_ANT[] = VALUES[].
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID              = 'WG_HEADER-TPSIM'
        VALUES          = VALUES[]
      EXCEPTIONS
        ID_ILLEGAL_NAME = 1
        OTHERS          = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    INIT = C_X.
  ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF WG_SAVE EQ C_ALTCPG.
    REFRESH VALUES_CPG.

    IF WG_HEADER-TPSIM EQ 'VV'.
      VALUES_CPG-TEXT = 'Venda A Vista'.
      VALUES_CPG-KEY  = 'VV'.
      APPEND VALUES_CPG.
    ENDIF.

    IF WG_HEADER-TPSIM EQ 'VP'.
      VALUES_CPG-TEXT = 'Venda A Prazo'.
      VALUES_CPG-KEY  = 'VP'.
      APPEND VALUES_CPG.
    ENDIF.

    VALUES_CPG-TEXT = 'Adiantamento'.
    VALUES_CPG-KEY  = 'AD'.
    APPEND VALUES_CPG.

    "FF #174195 - inicio
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    VALUES_CPG-TEXT = 'Troca Safra'.
    VALUES_CPG-KEY  = 'TS'.
    APPEND VALUES_CPG.

    VALUES_CPG-TEXT = 'Troca a Vista'.
    VALUES_CPG-KEY  = 'TV'.
    APPEND VALUES_CPG.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025

    VALUES_CPG-TEXT = 'Troca'.
    VALUES_CPG-KEY  = 'TT'.
    APPEND VALUES_CPG.
    "FF #174195 - fim

  ELSE.
    VALUES_CPG[] = VALUES_CPG_ANT[].
  ENDIF.

* "// Limitar a inclusão do TS e TV para novos Simuladores
* "// TS TV WBARBOSA 08/07/2025
  IF SY-UCOMM EQ C_ADD.
    DELETE VALUES_CPG WHERE KEY EQ 'TS'.
    DELETE VALUES_CPG WHERE KEY EQ 'TV'.
  ENDIF.
* "// TS TV WBARBOSA 08/07/2025

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'WG_HEADER-TPSIM'
      VALUES          = VALUES_CPG[]
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


*Início - Sara Oikawa - 38859 - Agosto/2020
  " Valores Drop-Down para Meio Pagamento
  REFRESH VALUES.
  IF WG_HEADER-TPSIM NE 'VV'.
    VALUES-TEXT = 'Acerto'.
    VALUES-KEY  = 'A'.
    APPEND VALUES.
  ENDIF.

*  values-text = 'Boleto Bancário'.
*  values-key  = 'B'.
*  APPEND values.
*
*  values-text = 'Depósito em Conta'.
*  values-key  = 'D'.
*  APPEND values.

  VALUES-TEXT = 'Boleto Bancário'.
  VALUES-KEY  = 'D'.
  APPEND VALUES.

  VALUES-TEXT = 'Depósito em Conta'.
  VALUES-KEY  = 'U'.
  APPEND VALUES.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
  IF WG_HEADER-TPSIM = 'VV' .

    "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25 - inicio
*    values-text = 'PIX/Bolepix'.
*    values-key  = 'X'.
*    APPEND values.
    "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25 - fim

    VALUES-TEXT = 'Acerto'.
    VALUES-KEY  = 'Y'.
    APPEND VALUES.

  ENDIF.

  "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25 - inicio
*  IF wg_header-tpsim = 'VP' .
*
*    values-text = 'PIX/Bolepix'.
*    values-key  = 'X'.
*    APPEND values.
*
*  ENDIF.
  "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25 - fim

  "Troca
  IF WG_HEADER-TPSIM = 'TT' .

    VALUES-TEXT = 'Troca'.
    VALUES-KEY  = 'T'.
    APPEND VALUES.

  ENDIF.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim


  " 21.02.2023 - RAMON - 102323 -->
  IF WG_HEADER-TPSIM = 'VV' AND WG_HEADER-ECOMMERCE = 'X'.

    "DELETE values WHERE key = 'U'. "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25
    "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25 - inicio
*    values-text = 'PIX'.
*    values-key  = 'X'.
*    APPEND values.
    "FF #174195 - definido via conversa no Teams com o Gabriel Avila/Anderson em 05/06/25 - fim

    "23-02-2024 - SMC - 134596
    VALUES-TEXT = 'Financiamento'.
    VALUES-KEY  = 'F'.
    APPEND VALUES.
    "23-02-2024 - SMC - 134596

  ENDIF.
  " 21.02.2023 - RAMON - 102323 --<

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'WG_MEIO_PAGO'
      VALUES          = VALUES[]
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

*Fim - Sara Oikawa - 38859 - Agosto/2020


  SELECT SINGLE *
  FROM ZSDT0040
  INTO WL_0040
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.
  LOOP AT SCREEN.
    IF ( SCREEN-NAME EQ 'PB_PAGTO' ).
      "ALRS
*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF WG_HEADER-MEIO_PAGO EQ 'B'
      IF ( WG_MEIO_PAGO EQ 'D'

        " 07.03.2023 - RAMON - E-Commerce -->
        OR WG_MEIO_PAGO EQ 'X'
        " 07.03.2023 - RAMON - E-Commerce --<

        OR WG_MEIO_PAGO EQ 'F' )"23-02-2024 - SMC - 134596

*Fim - Sara Oikawa - 38859 - Agosto/2020
        AND ( WG_HEADER-TPSIM = 'VV' OR WG_HEADER-TPSIM = 'VP' )  "WSBB
        AND WG_HEADER-WAERK = 'BRL'
        AND WL_0040-STATUS = 'A'.

        SCREEN-INVISIBLE = 0.
        MODIFY SCREEN.
      ELSE.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*** Adiantamento
  IF WG_HEADER-TPSIM(1) EQ C_A.
    LOOP AT SCREEN.

      IF SCREEN-GROUP1 EQ C_TRO
      OR SCREEN-GROUP1 EQ C_VIS.
        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'
        OR SCREEN-NAME NE 'WG_HEADER-TPCULT'
        OR SCREEN-NAME NE 'WG_HEADER-FCULT'
        OR SCREEN-NAME NE 'WG_HEADER-DTENT'.
          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
          IF <FS_CAMPO> IS ASSIGNED.
            CLEAR: <FS_CAMPO>.
          ENDIF.
          UNASSIGN <FS_CAMPO>.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF SCREEN-NAME = 'WG_HEADER-ANTEC'
      OR SCREEN-NAME = 'WG_HEADER-DTINIJUROS'
      OR SCREEN-NAME = 'WG_HEADER-KURSF'.
        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
        IF <FS_CAMPO> IS ASSIGNED.
          CLEAR: <FS_CAMPO>.
        ENDIF.
        UNASSIGN <FS_CAMPO>.

        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.

      IF ( SCREEN-NAME EQ 'WG_HEADER-DTVENCOV' ).
        CASE WG_HEADER-TPSIM.
          WHEN 'AD' OR 'VV'.
          WHEN OTHERS.
            SCREEN-ACTIVE = 0.
            MODIFY SCREEN.
        ENDCASE.

*        IF WG_HEADER-TPSIM NE 'AD' OR WG_HEADER-TPSIM NE 'VV'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( SCREEN-NAME EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'. "WSBB
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.



*** US - 81799 - Inicio - Cbrand
      IF ( SCREEN-NAME EQ 'WG_HEADER-HBKID' ) .
        IF ( WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP' ) AND WG_MEIO_PAGO <> 'D'.
          SCREEN-ACTIVE = 0.
          SCREEN-INPUT  = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) OR ( SCREEN-NAME EQ 'WG_HEADER-HBKID').
        IF ( WG_HEADER-TPSIM EQ 'VV' OR WG_HEADER-TPSIM EQ 'VP' ) AND WG_MEIO_PAGO = 'D'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand

*** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
**      IF SCREEN-NAME = 'WG_HEADER-VLR_ADTO' OR
**         SCREEN-NAME = 'WG_HEADER-ADTO_HA' OR
**         SCREEN-NAME = 'WG_HEADER-AREA_PENHOR' OR
**         SCREEN-NAME = 'WG_HEADER-COMPRSC' OR
**         SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR
**        SCREEN-NAME = 'WG_DESC_DAT'.
**        SCREEN-ACTIVE = 0.
**        MODIFY SCREEN.
**      ENDIF.
** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
*** "// Retornar AD WBARBOSA para Time Credito 09/07/2025

    ENDLOOP.

    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG.

    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      IF W_FIELDCATALOG-FIELDNAME EQ 'CALCU'
      OR W_FIELDCATALOG-FIELDNAME EQ 'TRUNIT'
      OR W_FIELDCATALOG-FIELDNAME EQ 'TRTOT'.

        W_FIELDCATALOG-NO_OUT = C_X.
      ELSE.
        W_FIELDCATALOG-NO_OUT = SPACE.

      ENDIF.

      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      CLEAR: W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.

** Troca
  ELSEIF WG_HEADER-TPSIM(1) EQ C_T.

    LOOP AT SCREEN.

      IF SCREEN-GROUP1 EQ C_ADT
      OR SCREEN-GROUP1 EQ C_VIS.
        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'.
          IF SCREEN-NAME NE 'WG_DESC_DAT'.
            ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
            IF <FS_CAMPO> IS ASSIGNED.
              CLEAR: <FS_CAMPO>.
            ENDIF.
            UNASSIGN <FS_CAMPO>.
          ENDIF.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

** "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
      IF WG_HEADER-TPSIM EQ 'TT'.

** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
        IF SCREEN-NAME EQ 'WG_HEADER-TPCULT'
        OR SCREEN-NAME EQ 'WG_HEADER-FCULT'
*      OR screen-name EQ 'WG_HEADER-DTENT' "FF #169501
        OR SCREEN-NAME EQ 'WG_DESC_TPCULT'
        OR SCREEN-NAME EQ 'WG_HEADER-SCHA'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDIF.
** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
** "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025

      IF SCREEN-NAME = 'WG_HEADER-KURSF'.
        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
        IF <FS_CAMPO> IS ASSIGNED.
          CLEAR: <FS_CAMPO>.
        ENDIF.
        UNASSIGN <FS_CAMPO>.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME = 'WG_HEADER-ANTEC'.
        IF WG_HEADER-TPSIM+1(1) EQ 'V'.
          IF SCREEN-NAME NE 'WG_DESC_DAT'.
            ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
            IF <FS_CAMPO> IS ASSIGNED.
              CLEAR: <FS_CAMPO>.
            ENDIF.
            UNASSIGN <FS_CAMPO>.
          ENDIF.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( SCREEN-NAME EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'. "WSBB
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( SCREEN-NAME EQ 'WG_HEADER-HBKID' ) OR ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM EQ 'VV' OR WG_HEADER-TPSIM EQ 'VP'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Fim - Cbrand

      IF ( SCREEN-NAME EQ 'WG_HEADER-DTVENCOV' ).
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.



    ENDLOOP.

    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG.

    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      IF W_FIELDCATALOG-FIELDNAME EQ 'COMPR'.

        W_FIELDCATALOG-NO_OUT = C_X.
      ELSE.
        W_FIELDCATALOG-NO_OUT = SPACE.
      ENDIF.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      CLEAR: W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.
** A Vista
  ELSEIF WG_HEADER-TPSIM(1) EQ C_V OR WG_HEADER-TPSIM(1) EQ C_P .

    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ C_ADT
      OR SCREEN-GROUP1 EQ C_TRO.
        IF SCREEN-NAME NE 'WG_HEADER-VLRTOT'.
          IF SCREEN-NAME NE 'WG_DESC_DAT'.
            IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO'  AND WG_HEADER-TPSIM EQ 'VP') OR
               ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO'  AND ( WG_HEADER-TPSIM EQ 'PM'  OR          "Inclusão - Sara - 38859 - Agosto/2020
                                                             WG_HEADER-TPSIM EQ 'VF'  OR
                                                             WG_HEADER-TPSIM EQ 'VV' ) ) OR
               ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'VP').

            ELSE.
              ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
              IF <FS_CAMPO> IS ASSIGNED.
                CLEAR: <FS_CAMPO>.
              ENDIF.
              UNASSIGN <FS_CAMPO>.
            ENDIF.
          ENDIF.
          SCREEN-ACTIVE = 0.
          IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO' AND WG_HEADER-TPSIM EQ 'VP').
*            WG_DESC_DAT = 'Data Vencimento'.
            SCREEN-ACTIVE = 1.
          ENDIF.
          IF ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'VP').
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
            SCREEN-ACTIVE = 1.
            SCREEN-INPUT = 0.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
          ENDIF.
*Início - Sara Oikawa - 38859 - Agosto/2020
          IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO' AND WG_HEADER-TPSIM EQ 'PM').
            SCREEN-ACTIVE = 1.
          ENDIF.

          IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO' AND WG_HEADER-TPSIM EQ 'VF').
            SCREEN-ACTIVE = 1.
          ENDIF.

          IF ( SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO' AND WG_HEADER-TPSIM EQ 'VV').
            SCREEN-ACTIVE = 1.
          ENDIF.

*          IF ( SCREEN-NAME EQ 'WG_HEADER-DTINIJUROS' AND WG_HEADER-TPSIM EQ 'PM').
*            SCREEN-ACTIVE = 1.
*          ENDIF.

*Fim - Sara Oikawa - 38859 - Agosto/2020

          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF ( SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR
           SCREEN-NAME = 'WG_DESC_DAT'       ) AND
*         ( WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP' AND WG_HEADER-TPSIM NE 'PM' ).
          ( WG_HEADER-TPSIM NE 'VP' AND WG_HEADER-TPSIM NE 'PM' ).
        IF SCREEN-NAME NE 'WG_DESC_DAT'.
          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
          IF <FS_CAMPO> IS ASSIGNED.
            CLEAR: <FS_CAMPO>.
          ENDIF.
          UNASSIGN <FS_CAMPO>.
        ENDIF.
        SCREEN-ACTIVE   = 0.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME = 'WG_HEADER-KURSF' AND WG_HEADER-TPSIM NE 'VF'.
        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
        IF <FS_CAMPO> IS ASSIGNED.
          CLEAR: <FS_CAMPO>.
        ENDIF.
        UNASSIGN <FS_CAMPO>.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME = 'WG_HEADER-ANTEC'.
        IF SCREEN-NAME NE 'WG_DESC_DAT'.
          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
          IF <FS_CAMPO> IS ASSIGNED.
            CLEAR: <FS_CAMPO>.
          ENDIF.
          UNASSIGN <FS_CAMPO>.
        ENDIF.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.

      IF ( SCREEN-NAME EQ 'WG_HEADER-DTVENCOV' ).
        CASE WG_HEADER-TPSIM.
          WHEN 'AD' OR 'VV'.
          WHEN OTHERS.
            SCREEN-ACTIVE = 0.
            MODIFY SCREEN.
        ENDCASE.
*        IF WG_HEADER-TPSIM NE 'AD' OR WG_HEADER-TPSIM NE 'VV'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( SCREEN-NAME EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'. "WSBB
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM EQ 'VV' OR WG_HEADER-TPSIM EQ 'VP'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand
** US - 81799 - Inicio - Cbrand
*      IF ( screen-name EQ 'WG_HEADER-HBKID' ).
*        IF wg_header-tpsim EQ 'VV' OR wg_header-tpsim EQ 'VP'.
*
*          ASSIGN (screen-name) TO <fs_campo>.
*          IF <fs_campo> IS ASSIGNED.
*            CLEAR: <fs_campo>.
*          ENDIF.
*          UNASSIGN <fs_campo>.
*
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
    ENDLOOP.
** US - 81799 - Fim - Cbrand
    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG.


    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      IF W_FIELDCATALOG-FIELDNAME EQ 'COMPR'
      OR W_FIELDCATALOG-FIELDNAME EQ 'TRUNIT'
      OR W_FIELDCATALOG-FIELDNAME EQ 'SIUMB'
      OR W_FIELDCATALOG-FIELDNAME EQ 'TRTOT'
      OR W_FIELDCATALOG-FIELDNAME EQ 'CALCU'.

        W_FIELDCATALOG-NO_OUT = C_X.
      ELSE.
        W_FIELDCATALOG-NO_OUT = SPACE.
      ENDIF.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      CLEAR: W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.
  ELSE.
    LOOP AT SCREEN.

      IF SCREEN-GROUP1 EQ C_ADT
      OR SCREEN-GROUP1 EQ C_TRO
      OR SCREEN-GROUP1 EQ C_VIS.
        IF  SCREEN-NAME EQ 'WG_HEADER-VLRTOT' AND WG_HEADER-TPSIM EQ 'BN'.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF ( SCREEN-NAME = 'WG_HEADER-DTPGTCULT' OR SCREEN-NAME = 'WG_DESC_DAT' OR SCREEN-NAME = 'WG_HEADER-ANTEC' ) AND WG_HEADER-TPSIM EQ 'BN'.
        IF SCREEN-NAME NE 'WG_DESC_DAT'.
          ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
          IF <FS_CAMPO> IS ASSIGNED.
            CLEAR: <FS_CAMPO>.
          ENDIF.
          UNASSIGN <FS_CAMPO>.
        ENDIF.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME = 'WG_HEADER-KURSF' AND WG_HEADER-TPSIM EQ 'BN'.
        ASSIGN (SCREEN-NAME) TO <FS_CAMPO>.
        IF <FS_CAMPO> IS ASSIGNED.
          CLEAR: <FS_CAMPO>.
        ENDIF.
        UNASSIGN <FS_CAMPO>.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( SCREEN-NAME EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'. "WSBB
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( SCREEN-NAME EQ 'WG_HEADER-HBKID' ) OR ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM EQ 'VV' OR WG_HEADER-TPSIM EQ 'VP'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand

    ENDLOOP.

  ENDIF.

  IF WG_ACAO EQ C_ADD
  OR WG_ACAO EQ C_MODIF.
    LOOP AT TG_ITENS WHERE VBELN IS NOT INITIAL.

    ENDLOOP.
    LOOP AT SCREEN.
* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*      IF SY-SUBRC IS NOT INITIAL.
      IF SY-SUBRC IS NOT INITIAL OR WG_SAVE EQ C_ALTCPG.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
        IF SCREEN-GROUP2 EQ 'A1'.
          IF SCREEN-NAME EQ 'PB_DESCP'.
            SCREEN-INVISIBLE = 0.
          ENDIF.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF SCREEN-GROUP2 EQ 'A2'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.

      IF ( SCREEN-NAME EQ 'WG_HEADER-DTVENCOV' ).
        CASE WG_HEADER-TPSIM.
          WHEN 'AD' OR 'VV'.
          WHEN OTHERS.
            SCREEN-ACTIVE = 0.
            MODIFY SCREEN.
        ENDCASE.
*        IF WG_HEADER-TPSIM NE 'AD' OR WG_HEADER-TPSIM NE 'VV'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.

*Início - Sara Oikawa - 38859 - Agosto/2020
*      IF ( SCREEN-NAME EQ 'WG_HEADER-MEIO_PAGO' ).
      IF ( SCREEN-NAME EQ 'WG_MEIO_PAGO' ).
*Fim - Sara Oikawa - 38859 - Agosto/2020
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'. "WSBB
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*** US - 81799 - Inicio - Cbrand
      IF ( SCREEN-NAME EQ 'WG_HEADER-HBKID' ) OR ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM NE 'VV' AND WG_HEADER-TPSIM NE 'VP'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF ( SCREEN-NAME EQ 'WG_DESC_HBKID' ) .
        IF WG_HEADER-TPSIM EQ 'VV' OR WG_HEADER-TPSIM EQ 'VP'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*** US - 81799 - Fim - Cbrand

    ENDLOOP.

*** modificar aqui
    LOOP AT TG_ITENS WHERE VBELN IS NOT INITIAL.

    ENDLOOP.
    IF SY-SUBRC IS INITIAL.
      IF GRID1 IS NOT INITIAL.

        CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
          IMPORTING
            ET_FIELDCATALOG = T_FIELDCATALOG.

*      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
***         FIM DE BLOQUEI DE CAMPOS

        LOOP AT TG_ITENS.
          REFRESH: TG_ITENS-STYLE.
          REFRESH: STYLE.
          CLEAR: WA_STYLE.
          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
           WHERE EDIT EQ C_X.
            WA_STYLE-FIELDNAME = W_FIELDCATALOG-FIELDNAME.
            IF TG_ITENS-VBELN IS NOT INITIAL.
              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            ELSE.
              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
            ENDIF.
            INSERT  WA_STYLE INTO TABLE STYLE .
          ENDLOOP.
          INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
          MODIFY TG_ITENS.
        ENDLOOP.

      ENDIF.
    ENDIF.

    LOOP AT SCREEN.
      LOOP AT TG_ITENS.
        CASE TG_ITENS-SPART.
          WHEN '02' OR '12'. "165455 - PQ 24.02.25
            IF SCREEN-NAME EQ 'WG_HEADER-DT_ENTREGA_FET'.
              SCREEN-INPUT = 1.
            ENDIF.
          WHEN '03' OR '13'. "165455 - PQ 24.02.25
            IF SCREEN-NAME EQ 'WG_HEADER-DT_ENTREGA_DEF'.
              SCREEN-INPUT = 1.
            ENDIF.
          WHEN '04'.
            IF SCREEN-NAME EQ 'WG_HEADER-DT_ENTREGA_SEM'.
              SCREEN-INPUT = 1.
            ENDIF.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    ENDLOOP.


  ELSEIF WG_ACAO EQ C_DESCONT.
    LOOP AT SCREEN.
      IF SCREEN-GROUP2 EQ 'A2'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CASE SY-UCOMM.
      "WSB SCREEN SACA
    WHEN C_SACAS.
      LOOP AT SCREEN.
        IF  SCREEN-NAME EQ 'WG_HEADER-TROTOTSC'
         OR SCREEN-NAME EQ 'CONVERT_TRATOTSC'.

          SCREEN-INPUT = 1.
          MODIFY SCREEN.

        ENDIF.
      ENDLOOP.

    WHEN C_DSC_ABS.

      LOOP AT TG_ITENS.
        FREE: IT_VBFA, IT_VBAP, IT_41, IT_VBAK, IT_90.

        SELECT *
          FROM VBFA
          INTO TABLE IT_VBFA
          WHERE VBELV EQ TG_ITENS-VBELN.

        IF IT_VBFA IS NOT INITIAL.
          FREE IT_VBFA.

          SELECT *
            FROM VBAK
            INTO TABLE IT_VBAK
            WHERE VBELN EQ TG_ITENS-VBELN.

          IF IT_VBAK IS NOT INITIAL.
            SELECT *
              FROM VBAP
              INTO TABLE IT_VBAP
              FOR ALL ENTRIES IN IT_VBAK
              WHERE VBELN EQ IT_VBAK-VBELN.

            IF IT_VBAP IS NOT INITIAL.
              SELECT *
                FROM VBFA
                INTO TABLE IT_VBFA
                FOR ALL ENTRIES IN IT_VBAP
                WHERE VBELV EQ IT_VBAP-VBELN.

              LOOP AT IT_VBAP INTO WA_VBAP.
                READ TABLE IT_VBFA TRANSPORTING NO FIELDS WITH KEY VBELV = WA_VBAP-VBELN
                                                                   POSNV = WA_VBAP-POSNR.
                IF SY-SUBRC IS INITIAL.
                  SELECT *
                  FROM ZSDT0041
                  APPENDING TABLE IT_41
                    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
                      AND POSNR EQ TG_ITENS-POSNR
                      AND MATNR EQ WA_VBAP-MATNR.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        SELECT *
          FROM ZSDT0090
            APPENDING TABLE IT_90
              WHERE VBELV EQ TG_ITENS-VBELN.

        CLEAR: WA_STYLE.
        REFRESH: TG_ITENS-STYLE.
        WA_STYLE-FIELDNAME = 'DESC_ABSOLUTO'.


        IF IT_41 IS NOT INITIAL OR IT_90[] IS NOT INITIAL.
          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ELSE.
          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ENDIF.


        READ TABLE STYLE TRANSPORTING NO FIELDS WITH KEY FIELDNAME = 'DESC_ABSOLUTO'.
        IF SY-SUBRC IS INITIAL.
          DELETE STYLE WHERE FIELDNAME = 'DESC_ABSOLUTO'.
          INSERT  WA_STYLE INTO TABLE STYLE.
        ELSE.
          INSERT  WA_STYLE INTO TABLE STYLE.
        ENDIF.

        INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
        MODIFY TG_ITENS.

      ENDLOOP.

*Início - Sara Oikawa - 38859 - Agosto/2020
    WHEN C_ALTJUR.
      LOOP AT SCREEN.
        IF  SCREEN-NAME EQ 'WG_HEADER-JUROS_ANO'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN C_ALTADT.
      LOOP AT SCREEN.
        IF  SCREEN-NAME EQ 'WG_HEADER-VLR_ADTO' OR
            SCREEN-NAME EQ 'WG_HEADER-ADTO_HA'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    WHEN C_ALTCPG.
      LOOP AT SCREEN.
        IF  SCREEN-NAME EQ 'WG_HEADER-TPSIM'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

  ENDCASE.

  IF WG_ACAO IS INITIAL OR S_ACAO EQ 'S_ATU'.
    LOOP AT SCREEN.
      IF  SCREEN-NAME EQ 'WG_HEADER-DOC_SIMULACAO'.
        SCREEN-INPUT = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
  IF WG_ACAO EQ C_MODIF AND WG_SAVE EQ C_ALTCPG.

    IF WG_HEADER-TPSIM(1) EQ C_TRO(1).
      LOOP AT SCREEN.

        IF SCREEN-GROUP1 NE C_TRO.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.

        IF SCREEN-NAME = 'WG_HEADER-TPSIM' OR
           SCREEN-NAME = 'WG_HEADER-DTPGTCULT'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF WG_HEADER-TPSIM(1) EQ C_ADT(1).
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 NE C_ADT.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.

        IF SCREEN-NAME = 'WG_HEADER-TPSIM'    OR
           SCREEN-NAME = 'WG_HEADER-DTVENCOV' OR
           SCREEN-NAME = 'WG_HEADER-DTPGTCULT'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8


* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  " A escolha das informações SIGAM se dará apenas pelas ações de NOVO / COPIAR / EDITAR
  " Ou Seja, o Simulador deverá estar com um Status que permita a edição das informações como um todo
  IF ( WL_0040-STATUS = C_A AND WG_SAVE NE C_ALTCPG ) OR     "Pacote8
    ( WG_ACAO NE C_ADD AND WG_ACAO NE C_COPY AND WG_ACAO NE C_MODIF ).
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ C_BTN_SG.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

**Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*  IF wl_0040-status = c_a and ( wg_acao EQ c_modif and wg_save = c_altcpg ).
*    LOOP AT SCREEN.
*      IF screen-name EQ c_btn_sg.
*        screen-active = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
**Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

*  " 21.02.2023 - RAMON - 102323 -->
  LOOP AT SCREEN.

    IF SCREEN-NAME = 'WG_HEADER-ID_ORDER_ECOMMERCE'..

      IF WG_HEADER-ECOMMERCE = 'X'.
        "screen-input = 1.
      ELSE.

        IF WG_ACAO = C_ADD OR WG_ACAO = C_MODIF.
          SCREEN-INPUT = 0.
        ENDIF.

      ENDIF.

      MODIFY SCREEN.

    ENDIF.


*    IF WG_HEADER-TPSIM(1) EQ C_ADT(1).
*      IF WG_ACAO = C_ADD OR WG_ACAO = C_MODIF.
*        IF SCREEN-NAME = 'WG_HEADER-DTVENCOV'.
*          SCREEN-INPUT = 0.
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*    ENDIF.


  ENDLOOP.
*
*  " 21.02.2023 - RAMON - 102323 --<

ENDMODULE.                 " MODIFY_SCREEN_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_FRETE CHANGING P_VLR_FRETE.

  CLEAR TAXA_FRE.

  SELECT SINGLE KURSK
    FROM ZSDT0117
    INTO TAXA_FRE
    WHERE BUKRS EQ WA_T001K-BUKRS
    AND DESATIVADO NE ABAP_TRUE.

  WG_HEADER-TAXA_FRETE = TAXA_FRE.

  CHECK WG_HEADER-WAERK EQ 'USD'.
*RSI - Case #CS0971604 Ajuste divisão frete
  IF TAXA_FRE NE 0.
    P_VLR_FRETE = P_VLR_FRETE / TAXA_FRE.
  ELSE.
    CLEAR P_VLR_FRETE.
    ERRO_TAXA_FRETE = WA_T001K-BUKRS.
  ENDIF.
*RSI - Case #CS0971604 Ajuste divisão frete
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALCULA_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULA_HEADER USING WL_TOT_VLR.

  IF ( WG_HEADER-TPSIM(1) EQ C_ADT(1)
  OR   WG_HEADER-TPSIM(1) EQ C_B     ).

    CLEAR: WL_TOT_VLR, WG_HEADER-VLRTOT, WG_HEADER-AREA_PENHOR, WG_HEADER-COMPRSC.

    PERFORM VLRTOT.

    LOOP AT TG_ITENS INTO WL_ITENS.
      ADD WL_ITENS-VLRTOT TO WL_TOT_VLR.
**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
**    Realiza o calculo do campo "Compromisso/sac"
      ADD WL_ITENS-COMPR  TO WG_HEADER-COMPRSC.
    ENDLOOP.

** "// Retornar AD WBARBOSA para Time Credito 09/07/2025
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
**    Realiza o calculo do campo "Area de Penhor"
    TRY.
        WG_HEADER-AREA_PENHOR = WL_TOT_VLR / WG_HEADER-ADTO_HA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim
** "// Retornar AD WBARBOSA para Time Credito 09/07/2025


  ELSEIF ( WG_HEADER-TPSIM(1) EQ C_TRO(1) ).

    CLEAR: WG_HEADER-TROTOTSC, WG_HEADER-SCHA, WG_HEADER-VLRTOT, CONVERT_TRATOTSC.

    PERFORM VLRTOT.

    LOOP AT TG_ITENS INTO WL_ITENS.

*    Realiza o calculo do campo "Troca Total Sc"
      ADD: WL_ITENS-TRTOT TO WG_HEADER-TROTOTSC,
           WL_ITENS-TRTOT TO CONVERT_TRATOTSC.

**    Realiza o calculo do campo "Valor Total"
*      ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.

    ENDLOOP.
    PERFORM MONTA_MEMO.

    PERFORM VLRTOT.

***   Calculo Sc/Há
    TRY.
        WG_HEADER-SCHA = WG_HEADER-TROTOTSC / WG_AREA_HA. "WG_HEADER-AREA_HA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
  ELSE.
*    Realiza o calculo do campo "Valor Total"
    PERFORM VLRTOT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VLRTOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VLRTOT .
**    Realiza o calculo do campo "Valor Total"

  CLEAR: WG_HEADER-VLRTOT.

  LOOP AT TG_ITENS INTO WL_ITENS.
    ADD WL_ITENS-VLRTOT TO WG_HEADER-VLRTOT.
  ENDLOOP.

*  PERFORM VLR_AJUSTE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  DESBLOQUEIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DESBLOQUEIO INPUT.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  VINC_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VINC_DESC.

  CLEAR: DESCONTO, TOTAL, DESC_ACRES.

  LOOP AT TG_TRANS ASSIGNING FIELD-SYMBOL(<TRANS>)
    WHERE CATEGORIA EQ 'O'
    AND FLAG EQ ABAP_FALSE
    AND ESTORNO NE ABAP_TRUE.

    <TRANS>-FLAG = ABAP_TRUE.
    ADD <TRANS>-DESC_ABSOLUTO TO DESCONTO.

  ENDLOOP.

  ADD WG_HEADER-VLRTOT TO TOTAL.
  ADD DESCONTO TO TOTAL.

  IF  DESCONTO > 0.
    DESC_ACRES = 'Acrescimo:'.
  ELSE.
    DESC_ACRES = 'Desconto:'.
  ENDIF.

  CALL SCREEN 0107 ENDING AT 51 7 STARTING AT 3 3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO0107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO0107 OUTPUT.
  SET PF-STATUS 'PF0107'.
  SET TITLEBAR  'TI0107'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI0107 INPUT.

  DATA: T_0090 TYPE TABLE OF ZSDT0090 WITH HEADER LINE.

  CASE SY-UCOMM.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'APLICAR'.

      LOOP AT TG_TRANS INTO DATA(W0090).
        MOVE-CORRESPONDING W0090 TO T_0090.
        APPEND T_0090.
        CLEAR T_0090.
      ENDLOOP.

      CHECK NOT T_0090[] IS INITIAL.

      MODIFY ZSDT0090 FROM TABLE T_0090.

      CHECK NOT WG_HEADER-DOC_SIMULACAO IS INITIAL.

      UPDATE ZSDT0040 SET VLRTOT = TOTAL
      WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_IMPOSTO
*&---------------------------------------------------------------------*
FORM BUSCA_IMPOSTO CHANGING P_WL_ITENS LIKE LINE OF TG_ITENS.

  DATA: WA_REGIO_K   TYPE KNA1-REGIO,
        WA_REGIO_L   TYPE LFA1-REGIO,
        VL_LIFNR     TYPE LFA1-LIFNR,

        "*-CS2025000025-#164218-27.01.2025-JT-inicio
        WA_ZSDT0008  TYPE ZSDT0008,
        "lc_dados     TYPE zsde0183,
        "lc_retorno   TYPE zsdt0370_t,
        "wc_retorno   TYPE zsdt0370,
        "*-CS2025000025-#164218-27.01.2025-JT-Fim

        WA_J_1BTXIC3 TYPE J_1BTXIC3,
        WA_OWNPR     TYPE MBEW-OWNPR.
  "wa_mtorg     TYPE mbew-mtorg.  "*-CS2025000025-#164218-27.01.2025-JT-inicio

  CLEAR: P_WL_ITENS-VLR_ICMS,
         P_WL_ITENS-VLR_COFINS ,
         P_WL_ITENS-VLR_PIS,
         P_WL_ITENS-J_1BTXSDC,
         P_WL_ITENS-J_1BTAXLW1,
         P_WL_ITENS-J_1BTAXLW5,
         P_WL_ITENS-J_1BTAXLW4,
         P_WL_ITENS-ZWERT_LIQDO.

  VL_LIFNR = |{ P_WL_ITENS-WERKS ALPHA = IN }|.

  "165455 - PQ - Ini
  SELECT SINGLE LAND1
    FROM T001 INTO @DATA(LVA_LAND1)
    WHERE BUKRS EQ @WG_HEADER-VKORG.

  CHECK SY-SUBRC EQ 0.

  IF LVA_LAND1 NE 'BR'.
    P_WL_ITENS-ZWERT_LIQDO = P_WL_ITENS-ZWERT.
    EXIT.
  ENDIF.
  "165455 - PQ - Fim

  SELECT SINGLE REGIO
    FROM KNA1
    INTO WA_REGIO_K
    WHERE KUNNR EQ WG_HEADER-KUNNR.

  SELECT SINGLE REGIO
    FROM LFA1
    INTO WA_REGIO_L
    WHERE LIFNR EQ VL_LIFNR.

  "CS2025000025-#164218-27.01.2025-JT-Inicio
  SELECT SINGLE OWNPR
    FROM MBEW
    INTO WA_OWNPR
    WHERE MATNR EQ P_WL_ITENS-MATNR
      AND BWKEY EQ P_WL_ITENS-WERKS.

*  SELECT SINGLE ownpr, mtorg
*    FROM mbew
*    INTO ( @wa_ownpr, @wa_mtorg )
*    WHERE matnr EQ @p_wl_itens-matnr
*      AND bwkey EQ @p_wl_itens-werks.
  "CS2025000025-#164218-27.01.2025-JT-Fim


*-CS2025000025-#164218-27.01.2025-JT-inicio
  SELECT SINGLE *
    FROM ZSDT0008
    INTO WA_ZSDT0008
   WHERE AUART      EQ P_WL_ITENS-AUART
     AND VKAUS      EQ 'I'
     AND MWSK1      EQ 'SD'
     AND UF_CENTRO  EQ WA_REGIO_L
     AND UF_CLIENTE EQ WA_REGIO_K
     AND OWNPR      EQ WA_OWNPR.

*  lc_dados-auart-valor      = p_wl_itens-auart.
*  lc_dados-vkaus-valor      = 'I'.
*  lc_dados-mwsk1-valor      = 'SD'.
*  lc_dados-uf_centro-valor  = wa_regio_l.
*  lc_dados-uf_cliente-valor = wa_regio_k.
*  lc_dados-ownpr-valor      = wa_ownpr.
*  lc_dados-bukrs_emit-valor = wa_t001k-bukrs.
*  lc_dados-kunnr-valor      = abap_on.
*  lc_dados-werks-valor      = p_wl_itens-werks.
*  lc_dados-matnr-valor      = p_wl_itens-matnr.
*
*  lc_retorno = zcl_impostos=>get_tax_imposto( i_dados = lc_dados ).
*
*  READ TABLE lc_retorno INTO wc_retorno INDEX 1.
*-CS2025000025-#164218-27.01.2025-JT-fim

  ZCL_UTIL_SD=>CONV_DATA_US_BR( EXPORTING I_DATA = SY-DATUM
                                RECEIVING E_DATA = _HOJE_INVDT ).

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = _HOJE_INVDT
    IMPORTING
      OUTPUT = _HOJE_INVDT.

  IF WA_ZSDT0008 IS NOT INITIAL.  "*-CS2025000025-#164218-27.01.2025-JT-
    "  IF wc_retorno IS NOT INITIAL.   "*-CS2025000025-#164218-27.01.2025-JT-

    "*-CS2025000025-#164218-27.01.2025-JT-Inicio
    P_WL_ITENS-J_1BTXSDC  = WA_ZSDT0008-J_1BTXSDC.   "*-CS2025000025-#164218-27.01.2025-JT-
    P_WL_ITENS-J_1BTAXLW5 = WA_ZSDT0008-J_1BTAXLW5.  "*-CS2025000025-#164218-27.01.2025-JT-
    P_WL_ITENS-J_1BTAXLW4 = WA_ZSDT0008-J_1BTAXLW4.  "*-CS2025000025-#164218-27.01.2025-JT-

    "p_wl_itens-j_1btxsdc  = wc_retorno-j_1btxsdc.
    "p_wl_itens-j_1btaxlw5 = wc_retorno-j_1btaxlw5.
    "p_wl_itens-j_1btaxlw4 = wc_retorno-j_1btaxlw4.
    "*-CS2025000025-#164218-27.01.2025-JT-Fim

    SELECT COUNT(*)
      FROM KNVI
      UP TO 1 ROWS
      WHERE KUNNR EQ WG_HEADER-KUNNR
      AND TATYP EQ 'IBRX'
      AND TAXKD EQ '2'.

    IF SY-SUBRC IS INITIAL.

      P_WL_ITENS-ZWERT_LIQDO = P_WL_ITENS-ZWERT.

    ELSE.


      "-CS2025000025-#164218-27.01.2025-JT-Inicio
      SELECT COUNT(*)
        FROM J_1BTXSDC
      WHERE TAXCODE EQ WA_ZSDT0008-J_1BTXSDC  "*-CS2025000025-#164218-27.01.2025-JT-
        AND CUSTUSAGE EQ '1'
        AND ICMS EQ ABAP_TRUE.

*      SELECT COUNT(*)
*        FROM j_1btxsdc
*      WHERE taxcode EQ wc_retorno-j_1btxsdc
*        AND custusage EQ '1'
*        AND icms EQ abap_true.
      "-CS2025000025-#164218-27.01.2025-JT-Fim

      IF SY-SUBRC IS INITIAL.

        SELECT SINGLE *
           FROM J_1BTXIC3
           INTO WA_J_1BTXIC3
          WHERE LAND1     EQ 'BR'
            AND SHIPFROM  EQ WA_REGIO_L
            AND SHIPTO    EQ WA_REGIO_K
            AND GRUOP     EQ '76'
            AND VALUE     EQ WG_HEADER-KUNNR
            AND VALUE2    EQ P_WL_ITENS-MATNR
            AND VALIDFROM GE _HOJE_INVDT
            AND VALIDTO   LE _HOJE_INVDT.


        IF SY-SUBRC IS NOT INITIAL.

          SELECT SINGLE EXTWG
            FROM MARA
            INTO @DATA(GRUPO_MERC_EXT)
            WHERE MATNR EQ @P_WL_ITENS-MATNR.

          IF GRUPO_MERC_EXT IS NOT INITIAL.
* RIM - SKM - IR121669 - Início
            SELECT SINGLE *
                  FROM J_1BTXIC3
                  INTO WA_J_1BTXIC3
                 WHERE LAND1    EQ 'BR'
                   AND SHIPFROM EQ WA_REGIO_L
                   AND SHIPTO   EQ WA_REGIO_K
                   AND GRUOP    EQ '78'
                   AND VALUE    EQ GRUPO_MERC_EXT
                   AND VALUE2   EQ P_WL_ITENS-WERKS
                   AND VALIDFROM GE _HOJE_INVDT  ">=
                   AND VALIDTO   LE _HOJE_INVDT. "<=
            IF SY-SUBRC IS NOT INITIAL.
* RIM - SKM - IR121669 - Fim
              SELECT SINGLE *
                    FROM J_1BTXIC3
                    INTO WA_J_1BTXIC3
                   WHERE LAND1    EQ 'BR'
                     AND SHIPFROM EQ WA_REGIO_L
                     AND SHIPTO   EQ WA_REGIO_K
                     AND GRUOP    EQ '79'
                     AND VALUE    EQ GRUPO_MERC_EXT
                     AND VALIDFROM GE _HOJE_INVDT  ">=
                     AND VALIDTO   LE _HOJE_INVDT. "<=
            ENDIF.  "<<RIM - SKM - IR120631
            IF SY-SUBRC IS NOT INITIAL.

              SELECT SINGLE *
                          FROM J_1BTXIC3
                          INTO WA_J_1BTXIC3
                         WHERE LAND1    EQ 'BR'
                           AND SHIPFROM EQ WA_REGIO_L
                           AND SHIPTO   EQ WA_REGIO_K
                           AND GRUOP    EQ '77'
                           AND VALUE    EQ P_WL_ITENS-MATNR
                           AND VALIDFROM GE _HOJE_INVDT  ">=
                           AND VALIDTO   LE _HOJE_INVDT. "<=
            ENDIF.

          ELSE.

            SELECT SINGLE *
                  FROM J_1BTXIC3
                  INTO WA_J_1BTXIC3
                 WHERE LAND1    EQ 'BR'
                   AND SHIPFROM EQ WA_REGIO_L
                   AND SHIPTO   EQ WA_REGIO_K
                   AND GRUOP    EQ '77'
                   AND VALUE    EQ P_WL_ITENS-MATNR
                   AND VALIDFROM GE _HOJE_INVDT  ">=
                   AND VALIDTO   LE _HOJE_INVDT. "<=
*             AND TAXLAW   EQ WA_ZSDT0008-J_1BTAXLW1.

          ENDIF.
        ENDIF.

        P_WL_ITENS-J_1BTAXLW1 = WA_J_1BTXIC3-TAXLAW.

        P_WL_ITENS-VLR_ICMS = ( P_WL_ITENS-ZWERT * WA_J_1BTXIC3-BASE / 100 ) * ( WA_J_1BTXIC3-RATE / 100 ).
        P_WL_ITENS-ZWERT_LIQDO = P_WL_ITENS-ZWERT - P_WL_ITENS-VLR_ICMS - P_WL_ITENS-VLR_COFINS - P_WL_ITENS-VLR_PIS.

        IF P_WL_ITENS-ZWERT_LIQDO IS INITIAL.
          P_WL_ITENS-ZWERT_LIQDO = P_WL_ITENS-ZWERT.
        ENDIF.

      ELSE.
        P_WL_ITENS-ZWERT_LIQDO = P_WL_ITENS-ZWERT.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVA_IMPOSTO
*&---------------------------------------------------------------------*
FORM SALVA_IMPOSTO.

  DATA: VL_INDEX TYPE I.

  LOOP AT TG_ITENS.

    VL_INDEX = SY-TABIX.
    PERFORM BUSCA_IMPOSTO CHANGING TG_ITENS.
    MODIFY TG_ITENS[] FROM TG_ITENS INDEX VL_INDEX TRANSPORTING VLR_ICMS ZWERT_LIQDO.
    CLEAR: TG_ITENS.

  ENDLOOP.

  LOOP AT TG_ITENS.

    UPDATE ZSDT0041 SET VLR_ICMS = TG_ITENS-VLR_ICMS
                     ZWERT_LIQDO = TG_ITENS-ZWERT_LIQDO
                  WHERE DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO
                    AND POSNR = TG_ITENS-POSNR.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIME_OV.

  SELECT * FROM ZSDT0041
    INTO TABLE @DATA(IT_0041_)
    WHERE DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.

  CALL FUNCTION 'Z_GERA_OV_CONTRATO'
    EXPORTING
      I_VLR = VLR
    TABLES
      VBELN = IT_0041_.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS_IMPOSTOS
*&---------------------------------------------------------------------*
FORM VERIFICA_ERROS_IMPOSTOS.

  DATA: WA_REGIO_K   TYPE KNA1-REGIO,
        IT_LFA1      TYPE STANDARD TABLE OF LFA1,
        IT_MBEW      TYPE STANDARD TABLE OF MBEW,
        WA_MBEW      TYPE MBEW,
        WA_LFA1      TYPE LFA1,
        "CS2025000025-#164218-27.01.2025-JT-Inicio
        WA_ZSDT0008  TYPE ZSDT0008,
*        wa_marc      TYPE marc,
*        wa_mara      TYPE mara,
*        lc_dados     TYPE zsde0183,    "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        lc_retorno   TYPE zsdt0370_t,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        wc_retorno   TYPE zsdt0370,    "*-CS2025000025-#164218-27.01.2025-JT-inicio
        "CS2025000025-#164218-27.01.2025-JT-Fim

        WA_J_1BTXIC3 TYPE J_1BTXIC3,
        VL_LIFNR     TYPE LFA1-LIFNR,
        WL_LINHA(6).

*  REFRESH: TG_MSG_RET.
*  CLEAR: TG_MSG_RET.

*165455 - PQ - Desconsiderar calculo de imposto quando Empresa Argentina
  CHECK WG_HEADER-VKORG NE '0100'.
*165455 - PQ

  SELECT SINGLE REGIO
    FROM KNA1
    INTO WA_REGIO_K
    WHERE KUNNR EQ WG_HEADER-KUNNR.

  SELECT *
    FROM MBEW
    INTO TABLE IT_MBEW
    FOR ALL ENTRIES IN TG_ITENS
    WHERE MATNR EQ TG_ITENS-MATNR
      AND BWKEY EQ TG_ITENS-WERKS.

  ZCL_UTIL_SD=>CONV_DATA_US_BR( EXPORTING I_DATA = SY-DATUM
                                RECEIVING E_DATA = _HOJE_INVDT ).

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = _HOJE_INVDT
    IMPORTING
      OUTPUT = _HOJE_INVDT.

  LOOP AT TG_ITENS.

    WL_LINHA = SY-TABIX.
    CLEAR: WA_MBEW,
           "CS2025000025-#164218-27.01.2025-JT-Inicio
           "wc_retorno,
           WA_ZSDT0008,
           "CS2025000025-#164218-27.01.2025-JT-Fim
           WA_J_1BTXIC3, WA_LFA1, VL_LIFNR.

    VL_LIFNR = |{ TG_ITENS-WERKS ALPHA = IN }|.

    SELECT SINGLE *
      FROM LFA1
      INTO WA_LFA1
      WHERE LIFNR EQ VL_LIFNR.

    IF WA_LFA1 IS NOT INITIAL.

      READ TABLE IT_MBEW INTO WA_MBEW WITH KEY MATNR = TG_ITENS-MATNR
                                               BWKEY = TG_ITENS-WERKS.

*-CS2025000025-#164218-27.01.2025-JT-inicio
      SELECT SINGLE *
         FROM ZSDT0008
         INTO WA_ZSDT0008
        WHERE AUART      EQ TG_ITENS-AUART
          AND VKAUS      EQ 'I'
          AND MWSK1      EQ 'SD'
          AND UF_CENTRO  EQ WA_LFA1-REGIO
          AND UF_CLIENTE EQ WA_REGIO_K
          AND OWNPR      EQ WA_MBEW-OWNPR.

*      lc_dados-auart-valor      = tg_itens-auart.
*      lc_dados-vkaus-valor      = 'I'.
*      lc_dados-mwsk1-valor      = 'SD'.
*      lc_dados-uf_centro-valor  = wa_lfa1-regio.
*      lc_dados-uf_cliente-valor = wa_regio_k.
*      lc_dados-ownpr-valor      = wa_mbew-ownpr.
*      lc_dados-bukrs_emit-valor = wa_t001k-bukrs.
*      lc_dados-kunnr-valor      = abap_on.
*      lc_dados-werks-valor      = tg_itens-werks.
*      lc_dados-matnr-valor      = tg_itens-matnr.
*
*      lc_retorno = zcl_impostos=>get_tax_imposto( i_dados = lc_dados ).
*
*      READ TABLE lc_retorno INTO wc_retorno INDEX 1.
*-CS2025000025-#164218-27.01.2025-JT-fim

      IF WA_ZSDT0008 IS INITIAL.  "*-CS2025000025-#164218-27.01.2025-JT-
        "      IF wc_retorno  IS INITIAL.  "*-CS2025000025-#164218-27.01.2025-JT-
        "Falta parâmetros de cadastro na trasação ZSDT0011
        MOVE: 'VLR_ICMR' TO TG_MSG_RET-FIELD,
              'GRID1'    TO TG_MSG_RET-OBJ,
              WL_LINHA  TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E48 ' LINHA: ' WL_LINHA INTO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ELSE.

        SELECT SINGLE *
                       FROM J_1BTXIC3
                       INTO WA_J_1BTXIC3
                      WHERE LAND1     EQ 'BR'
                        AND SHIPFROM  EQ WA_LFA1-REGIO
                        AND SHIPTO    EQ WA_REGIO_K
                        AND GRUOP     EQ '76'
                        AND VALUE     EQ WG_HEADER-KUNNR
                        AND VALUE2    EQ TG_ITENS-MATNR
                        AND VALIDFROM GE _HOJE_INVDT
                        AND VALIDTO   LE _HOJE_INVDT.

        IF SY-SUBRC IS NOT INITIAL.

          SELECT COUNT(*)
            FROM KNVI
            UP TO 1 ROWS
            WHERE KUNNR EQ WG_HEADER-KUNNR
            AND TATYP EQ 'IBRX'
            AND TAXKD EQ '2'.

          IF SY-SUBRC IS INITIAL.
*                   "//Suframa
*                    SELECT SINGLE *
*                          FROM J_1BTXIC3
*                          INTO WA_J_1BTXIC3
*                         WHERE LAND1     EQ 'BR'
*                           AND SHIPFROM  EQ WA_LFA1-REGIO
*                           AND SHIPTO    EQ WA_REGIO_K
*                           AND GRUOP     EQ '76'
*                           AND VALUE     EQ WG_HEADER-KUNNR
*                           AND VALUE2    EQ TG_ITENS-MATNR
*                           AND VALIDFROM GE _HOJE_INVDT
*                           AND VALIDTO   LE _HOJE_INVDT.

*                    IF SY-SUBRC IS NOT INITIAL.
            "Não foi localizado cadastro da Suframa
            APPEND VALUE #(
                            FIELD = 'VLR_ICMS'
                            OBJ = 'GRID1'
                            TABIX = WL_LINHA
                            MSG = |Cliente Suframa! { TEXT-E46 }  LINHA: { WL_LINHA } |
                          ) TO TG_MSG_RET.
*                    ENDIF.

          ELSE.

            IF WA_LFA1-REGIO NE WA_REGIO_K.

              SELECT SINGLE EXTWG
                  FROM MARA
                 INTO @DATA(GRUPO_MERC_EXT)
                  WHERE MATNR EQ @TG_ITENS-MATNR.

              IF GRUPO_MERC_EXT IS NOT INITIAL.
* RIM - SKM - IR120631 - Início
                SELECT SINGLE *
                      FROM J_1BTXIC3
                      INTO WA_J_1BTXIC3
                     WHERE LAND1    EQ 'BR'
                       AND SHIPFROM EQ WA_LFA1-REGIO
                       AND SHIPTO   EQ WA_REGIO_K
                       AND GRUOP    EQ '78'
                       AND VALUE    EQ GRUPO_MERC_EXT
                       AND VALUE2   EQ TG_ITENS-WERKS
                       AND VALIDFROM GE _HOJE_INVDT  ">=
                       AND VALIDTO   LE _HOJE_INVDT. "<=
                IF SY-SUBRC IS NOT INITIAL.
* RIM - SKM - IR120631 - Fim
                  SELECT SINGLE *
                        FROM J_1BTXIC3
                        INTO WA_J_1BTXIC3
                       WHERE LAND1    EQ 'BR'
                         AND SHIPFROM EQ WA_LFA1-REGIO
                         AND SHIPTO   EQ WA_REGIO_K
                         AND GRUOP    EQ '79'
                         AND VALUE    EQ GRUPO_MERC_EXT
                         AND VALIDFROM GE _HOJE_INVDT  ">=
                         AND VALIDTO   LE _HOJE_INVDT. "<=
                ENDIF.  "<<RIM - SKM - IR120631

                IF SY-SUBRC IS NOT INITIAL.

                  SELECT SINGLE *
                              FROM J_1BTXIC3
                              INTO WA_J_1BTXIC3
                             WHERE LAND1    EQ 'BR'
                               AND SHIPFROM EQ WA_LFA1-REGIO
                               AND SHIPTO   EQ WA_REGIO_K
                               AND GRUOP    EQ '77'
                               AND VALUE    EQ TG_ITENS-MATNR
                               AND VALIDFROM GE _HOJE_INVDT  ">=
                               AND VALIDTO   LE _HOJE_INVDT. "<=
                ENDIF.

              ELSE.

                SELECT SINGLE *
                           FROM J_1BTXIC3
                           INTO WA_J_1BTXIC3
                          WHERE LAND1    EQ 'BR'
                            AND SHIPFROM EQ WA_LFA1-REGIO
                            AND SHIPTO   EQ WA_REGIO_K
                            AND GRUOP    EQ '77'
                            AND VALUE    EQ TG_ITENS-MATNR
                            AND VALIDFROM GE _HOJE_INVDT  ">=
                            AND VALIDTO   LE _HOJE_INVDT. "<=
              ENDIF.

              IF WA_J_1BTXIC3 IS INITIAL.
                "Não foi localizado cadastro de Excessão Dinâmica de ICMS
                MOVE: 'VLR_ICMS' TO TG_MSG_RET-FIELD,
                      'GRID1'    TO TG_MSG_RET-OBJ,
                       WL_LINHA  TO TG_MSG_RET-TABIX.

                CONCATENATE TEXT-E46 ' LINHA: ' WL_LINHA INTO TG_MSG_RET-MSG.
                APPEND TG_MSG_RET.
                CLEAR: TG_MSG_RET.
              ELSE.
                "Não faz nada, tem cadastro.
              ENDIF.

            ELSE.
              "Não faz, domicílios no mesmo estado.
            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.

    ELSE.
      "Não localizado UF para o Centro Fornecedor
      MOVE: 'WAERS' TO TG_MSG_RET-FIELD,
            'GRID1' TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E47 ' LINHA: ' WL_LINHA INTO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALCULA_AJUSTES_ORDEM
*&---------------------------------------------------------------------*
FORM CALCULA_AJUSTES_ORDEM.

  DATA: WA_ZSDT0041_AUX    TYPE ZSDT0041,
        WA_ZSDT0041_AJUSTE TYPE ZSDT0041,
        IT_ZSDT0041_AUX    TYPE STANDARD TABLE OF ZSDT0041,
        VL_ARRED_LIQ       TYPE ZSDT0041-ZWERT_LIQDO,
        VL_ARRED_TX        TYPE ZSDT0041-ZWERT_LIQDO.

  CLEAR: IT_ZSDT0041_AJUSTE.

  SELECT *
    FROM ZSDT0041
    INTO TABLE IT_ZSDT0041_AUX
    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  LOOP AT IT_ZSDT0041_AUX INTO WA_ZSDT0041_AUX.

    WA_ZSDT0041_AJUSTE-DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.
    WA_ZSDT0041_AJUSTE-VBELN         = WA_ZSDT0041_AUX-VBELN.
    WA_ZSDT0041_AJUSTE-POSNR         = WA_ZSDT0041_AUX-POSNR.
    WA_ZSDT0041_AJUSTE-MATNR         = WA_ZSDT0041_AUX-MATNR.

    SELECT SINGLE KNUMV
      FROM VBAK
      INTO @DATA(VL_KNUMV)
      WHERE VBELN EQ @WA_ZSDT0041_AUX-VBELN.

    SELECT SINGLE POSNR
      FROM VBAP
      INTO @DATA(VL_POSNR)
      WHERE VBELN EQ @WA_ZSDT0041_AUX-VBELN
        AND MATNR EQ @WA_ZSDT0041_AUX-MATNR.


*---> 19/07/2023 - Migração S4 - DG
*    SELECT SINGLE kbetr
*      FROM konv
*      INTO @DATA(vl_kbetr)
*      WHERE knumv EQ @vl_knumv
*        AND kposn EQ @vl_posnr
*        AND kschl EQ 'RB00'.

    SELECT SINGLE KBETR
      FROM V_KONV
      INTO @DATA(VL_KBETR)
      WHERE KNUMV EQ @VL_KNUMV
        AND KPOSN EQ @VL_POSNR
        AND KSCHL EQ 'RB00'.
*<--- 19/07/2023 - Migração S4 - DG

* Arredondando parte do valor líquido do ajuste

    VL_ARRED_LIQ = WA_ZSDT0041_AUX-ZWERT_LIQDO.

    CALL FUNCTION 'ROUND'
      EXPORTING
        DECIMALS = 2
        INPUT    = VL_ARRED_LIQ
        SIGN     = 'X'
      IMPORTING
        OUTPUT   = VL_ARRED_LIQ.

    VL_ARRED_LIQ = VL_ARRED_LIQ * WA_ZSDT0041_AUX-ZMENG.

    CALL FUNCTION 'ROUND'
      EXPORTING
        DECIMALS = 2
        INPUT    = VL_ARRED_LIQ
        SIGN     = 'X'
      IMPORTING
        OUTPUT   = VL_ARRED_LIQ.

* Arredondando parte da taxa do imposto do ajuste

    VL_ARRED_TX = WA_ZSDT0041_AUX-VLR_ICMS.

*    CALL FUNCTION 'ROUND'
*      EXPORTING
*        DECIMALS = 4
*        INPUT    = VL_ARRED_TX
*        SIGN     = 'X'
*      IMPORTING
*        OUTPUT   = VL_ARRED_TX.

    VL_ARRED_TX = VL_ARRED_TX * WA_ZSDT0041_AUX-ZMENG.

    CALL FUNCTION 'ROUND'
      EXPORTING
        DECIMALS = 2
        INPUT    = VL_ARRED_TX
        SIGN     = 'X'
      IMPORTING
        OUTPUT   = VL_ARRED_TX.

* Cálculo do ajuste

    WA_ZSDT0041_AJUSTE-DESC_ABSOLUTO = WA_ZSDT0041_AUX-ZWERT * WA_ZSDT0041_AUX-ZMENG - ( VL_ARRED_TX + VL_ARRED_LIQ ).
    ADD VL_KBETR TO WA_ZSDT0041_AJUSTE-DESC_ABSOLUTO.

    APPEND WA_ZSDT0041_AJUSTE TO IT_ZSDT0041_AJUSTE.

    "Chamar a função e passar a 0041 global após gerar ordem

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'S0300'.
  SET TITLEBAR 'T0300'.

  IF NAO EQ ABAP_TRUE.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'VLR'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  CASE SY-UCOMM.
    WHEN 'OK'.

      IF SIM EQ ABAP_TRUE.
        PERFORM IMPRIME_CONTRATO USING WG_HEADER-DOC_SIMULACAO.
        PERFORM IMPRIME_OV.
      ELSE.
        PERFORM IMPRIME_CONTRATO USING WG_HEADER-DOC_SIMULACAO.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_CAMPOS .
  IF NAO EQ ABAP_TRUE.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'VLR'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO0108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO0108 OUTPUT.
  SET PF-STATUS 'PF0108'.
  SET TITLEBAR  'TI0108'.
  "
  "ALV
  IF CONTAINER_3 IS INITIAL.
*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT-ZEBRA      = C_X.
*    WA_LAYOUT-NO_ROWMARK = C_X.
*    WA_STABLE-ROW        = C_X.
*    WA_LAYOUT-NO_TOOLBAR = ''.
*    WA_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*    WA_LAYOUT-SEL_MODE   = 'A'.
*    WA_LAYOUT-BOX_FNAME  = 'MARK'.

*    WA_STABLE-ROW = 'X'.
**    WA_STABLE-COL = 'X'.

    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA       = C_X.
    WA_LAYOUT-NO_ROWMARK  = SPACE.
    WA_LAYOUT-COL_OPT     = C_X.
*    WA_STABLE-ROW         = C_X.
    WA_LAYOUT-SEL_MODE    = 'A'.
    WA_LAYOUT-INFO_FNAME  = 'LINE_COLOR'.
    WA_LAYOUT-STYLEFNAME = 'STYLE'.

    CREATE OBJECT CONTAINER_3
      EXPORTING
        CONTAINER_NAME = 'CC_ANTEC'.

    CREATE OBJECT GRID3
      EXPORTING
        I_PARENT = CONTAINER_3.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID3.

    REFRESH TL_FUNCTION.

    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR FOR GRID3,
                 OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID3.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM MONTAR_LAYOUT_ANTEC.


    CALL METHOD GRID3->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
*       i_save               = 'X'
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_ANTEC[].

    CALL METHOD GRID3->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID3->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: LCL_EVENT_HANDLER=>ON_HOTSPOT_CLICK         FOR GRID3.
  ELSE.
    CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0108  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI0108 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'CTB'.
      PERFORM GERAR_CONTABIL.
    WHEN 'ELIM'.
      PERFORM ELIMINAR.
    WHEN 'EST'.
      PERFORM ESTORNAR_CONTABIL.
    WHEN 'REFRESH'.
      PERFORM CARREGA_ANTEC USING TG_ANTEC[] WG_HEADER-DOC_SIMULACAO.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ANTEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_ANTEC.

  REFRESH T_FIELDCATALOG.

  PERFORM MONTAR_ESTRUTURA USING:
        1   ' '        ' '             'TG_ANTEC'  'VBELN'        'Nº OV'               '10'   ' ' ' '   ' '   ' '  ' '  ' ',
        2   ' '        ' '             'TG_ANTEC'  'ZID_LANC'     'ID Lanç'             '10'   ' ' ' '   ' '   ' '  ' '  ' ',
        3   'ZSDT0159' 'MONT_MOEDA'    'TG_ANTEC'  'MONT_MOEDA'   'Valor Total'         '15'   ' ' ' '   ' '   ' '  ' '  ' ',
        4   ' '        ' '             'TG_ANTEC'  'ADIANT'       'Doc. Contábil'       '10'   ' ' ' '   ' '   ' '  ' '  'X',
        5   ' '        ' '             'TG_ANTEC'  'AUGBL'        'Doc. Compensação'    '10'   ' ' ' '   ' '   ' '  ' '  'X',
        6   ' '        ' '             'TG_ANTEC'  'USNAM'        'Usuário'             '12'   ' ' ' '   ' '   ' '  ' '  ' ',
        7   'ZSDT0159' 'DATA_ATUAL'    'TG_ANTEC'  'DATA_ATUAL'   'Data'                '12'   ' ' ' '   ' '   ' '  ' '  ' ',
        8   ' '        ' '             'TG_ANTEC'  'USNAM_E'      'Usuário Est.'        '12'   ' ' ' '   ' '   ' '  ' '  ' ',
        9   'ZSDT0159' 'DATA_ATUAL_E'  'TG_ANTEC'  'DATA_ATUAL_E' 'Data Est.'           '12'   ' ' ' '   ' '   ' '  ' '  ' ',

        " 21.02.2023 - RAMON - 102323 -->
        10 'ZSDT0159' 'ID_TRANSACAO_FINANCEIRA' 'TG_ANTEC' 'ID_TRANSACAO_FINANCEIRA' 'ID Transação Financeira ' '12'   ' ' ' '   ' '   ' '  ' '  ' '.
  " 21.02.2023 - RAMON - 102323 --<

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_MEMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CARREGA_ANTEC USING TG_ANTEC TYPE TG_ANTEC_F
                         DOC_SIMULACAO TYPE ZSDT0040-DOC_SIMULACAO.
  DATA: IT_ZIB_CONTABIL_CHV TYPE TABLE OF ZIB_CONTABIL_CHV,
        IT_BSAD             TYPE TABLE OF BSAD_VIEW,
        TABIX               TYPE SY-TABIX.

  REFRESH TG_ANTEC.
  SELECT *
    FROM ZSDT0159
    INTO CORRESPONDING FIELDS OF TABLE TG_ANTEC
  WHERE DOC_SIMULACAO = DOC_SIMULACAO.

  CHECK TG_ANTEC[] IS NOT INITIAL.

*  SELECT *
*    FROM ZIB_CONTABIL_CHV
*    INTO TABLE IT_ZIB_CONTABIL_CHV
*   FOR ALL ENTRIES IN TG_ANTEC
*    WHERE OBJ_KEY EQ TG_ANTEC-OBJ_KEY.
*
*  IF IT_ZIB_CONTABIL_CHV[] IS NOT INITIAL.
*    SELECT *
*      FROM BSAD
*      INTO TABLE IT_BSAD
*      FOR ALL ENTRIES IN IT_ZIB_CONTABIL_CHV
*      WHERE BUKRS EQ IT_ZIB_CONTABIL_CHV-BUKRS
*      AND   BELNR EQ IT_ZIB_CONTABIL_CHV-BELNR
*      AND   GJAHR EQ IT_ZIB_CONTABIL_CHV-GJAHR
*      AND   BSAD~BELNR NE BSAD~AUGBL.
*
*  ENDIF.

  SELECT *
    FROM BSAD_VIEW
    INTO TABLE @IT_BSAD
    FOR ALL ENTRIES IN @TG_ANTEC
    WHERE BUKRS EQ @TG_ANTEC-BUKRS
    AND   BELNR EQ @TG_ANTEC-ADIANT
    AND   GJAHR EQ @TG_ANTEC-GJAHR
    AND   BSAD_VIEW~BELNR NE BSAD_VIEW~AUGBL.

  SORT: IT_ZIB_CONTABIL_CHV BY OBJ_KEY,
        IT_BSAD BY BUKRS BELNR GJAHR.

  LOOP AT TG_ANTEC INTO DATA(WG_ANTEC).
    TABIX = SY-TABIX.
    IF WG_ANTEC-ADIANT IS NOT INITIAL.
      SELECT SINGLE   *
         FROM BKPF
         INTO @DATA(WG_BKPF_FB082)
         WHERE BUKRS EQ @WG_ANTEC-BUKRS
         AND   BELNR EQ @WG_ANTEC-ADIANT
         AND   GJAHR EQ @WG_ANTEC-GJAHR
         AND   STBLG NE ''.
      IF SY-SUBRC = 0.
        WG_ANTEC-ESTORNO = 'X'.
      ENDIF.
      READ TABLE IT_BSAD INTO DATA(WL_BSAD2) WITH KEY BUKRS = WG_ANTEC-BUKRS
                                                     BELNR = WG_ANTEC-ADIANT
                                                     GJAHR = WG_ANTEC-GJAHR BINARY SEARCH.
      IF SY-SUBRC = 0.
        SELECT SINGLE   *
           FROM BKPF
           INTO @DATA(WG_BKPF082)
           WHERE BUKRS EQ @WG_ANTEC-BUKRS
           AND   BELNR EQ @WL_BSAD2-AUGBL
           AND   GJAHR EQ @WL_BSAD2-AUGDT(4)
           AND   STBLG NE ''.
        IF SY-SUBRC NE 0.
          WG_ANTEC-AUGBL = WL_BSAD2-AUGBL.
          WG_ANTEC-AUGDT = WL_BSAD2-AUGDT.
        ENDIF.
      ENDIF.

    ELSE.

      READ TABLE IT_ZIB_CONTABIL_CHV INTO DATA(WL_ZIB) WITH KEY OBJ_KEY = WG_ANTEC-OBJ_KEY BINARY SEARCH.
      IF SY-SUBRC = 0.
        WG_ANTEC-BUKRS  = WL_ZIB-BUKRS.
        WG_ANTEC-ADIANT = WL_ZIB-BELNR.
        WG_ANTEC-GJAHR  = WL_ZIB-GJAHR.
        SELECT SINGLE   *
             FROM BKPF
             INTO @DATA(WG_BKPF_FB08)
             WHERE BUKRS EQ @WL_ZIB-BUKRS
             AND   BELNR EQ @WL_ZIB-BELNR
             AND   GJAHR EQ @WL_ZIB-GJAHR
             AND   STBLG NE ''.
        IF SY-SUBRC = 0.
          WG_ANTEC-ESTORNO = 'X'.
        ENDIF.
        READ TABLE IT_BSAD INTO DATA(WL_BSAD) WITH KEY BUKRS = WL_ZIB-BUKRS
                                                       BELNR = WL_ZIB-BELNR
                                                       GJAHR = WL_ZIB-GJAHR BINARY SEARCH.
        IF SY-SUBRC = 0.
          SELECT SINGLE   *
             FROM BKPF
             INTO @DATA(WG_BKPF08)
             WHERE BUKRS EQ @WL_ZIB-BUKRS
             AND   BELNR EQ @WL_BSAD-AUGBL
             AND   GJAHR EQ @WL_BSAD-AUGDT(4)
             AND   STBLG NE ''.
          IF SY-SUBRC NE 0.
            WG_ANTEC-AUGBL = WL_BSAD-AUGBL.
            WG_ANTEC-AUGDT = WL_BSAD-AUGDT.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE *
          FROM ZIB_CONTABIL
          INTO @DATA(WL_ZIBC)
         WHERE OBJ_KEY = @WG_ANTEC-OBJ_KEY.
        IF SY-SUBRC = 0.
          WG_ANTEC-ADIANT = ICON_OPERATION.
        ENDIF.
        SELECT SINGLE *
          FROM ZIB_CONTABIL_ERR
          INTO @DATA(WL_ZIBE)
         WHERE OBJ_KEY = @WG_ANTEC-OBJ_KEY.
        IF SY-SUBRC = 0.
          WG_ANTEC-ADIANT = ICON_MESSAGE_ERROR_SMALL.
        ENDIF.
      ENDIF.
    ENDIF.
    IF WG_ANTEC-ESTORNO = 'X'.
      WG_ANTEC-LINE_COLOR = 'C601'.
    ENDIF.
    MODIFY TG_ANTEC FROM WG_ANTEC INDEX TABIX TRANSPORTING BUKRS ADIANT GJAHR AUGBL AUGDT LINE_COLOR ESTORNO.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERAR_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERAR_CONTABIL .

*  "// INICIO WBARBOSA US-163166 17-01-2025
  DATA: LT_FIELDS TYPE TABLE OF SVAL.

  DATA: RETURNCODE(1)   TYPE C,
        POPUP_TITLE(30) TYPE C.
*  "// FIM WBARBOSA US-163166 17-01-2025


  DATA: IT_ZSDT0041     TYPE TABLE OF ZSDT0041,
        IT_ZSDT0041R    TYPE TABLE OF ZSDT0041,
        IT_ZSDT0159     TYPE TABLE OF ZSDT0159,
        IT_ZFIT0026     TYPE TABLE OF ZFIT0026,
        IT_ZIB_CONTABIL TYPE TABLE OF ZIB_CONTABIL.

  DATA: WA_ZSDT0159     TYPE ZSDT0159,
        WA_ZIB          TYPE ZIB_CONTABIL,
        WA_ZFIT0026     TYPE ZFIT0026,
        WA_ZIB_CONTABIL TYPE ZIB_CONTABIL,
        W_ZSDT0090      TYPE ZSDT0090.

  DATA: VSEQ                TYPE ZSDT0159-SEQ,
        VSEQ2               TYPE ZFIT0026-SEQ,
        VSEQC(6),
        VGRAVA(1)           VALUE ' ',
        VCHECK(1),
        WL_DATA(10),
        WL_DATA_PASSADO(10), "// WBARBOSA US-163166 17-01-2025
        WL_VENC(10),
        WL_BKTXT(25),
        WL_VALOR(16),
        VL_MSG_AUX          TYPE STRING,
        OPT                 TYPE CTU_PARAMS,
        P_ZID               TYPE NUMC10,
        VL_NETWR            TYPE NETWR,
        VL_MWSBP            TYPE MWSBP.

  SORT TG_ANTEC BY VBELN ESTORNO.
  "
  SELECT *
   FROM ZSDT0041
   INTO TABLE IT_ZSDT0041
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO
   AND   VBELN         NE ''.

  IF IT_ZSDT0041[] IS INITIAL.
    MESSAGE 'Não há ordens de venda criadas!' TYPE 'I'.
    EXIT.
  ENDIF.

*** Projeto E-commerce #102323
  IF WG_HEADER-ECOMMERCE EQ 'X'.
    IF ZSDT0159-ID_TRANSACAO_FINANCEIRA IS INITIAL.
      MESSAGE 'Obrigatório informar o ID Transação Financeira!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
*** Projeto E-commerce #102323

*  INICIO US-163166 WBARBOSA 17/01/2025
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = 'O pagamento dessa OV situa-se no passado?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      START_COLUMN          = 30
      START_ROW             = 8
      DISPLAY_CANCEL_BUTTON = ' '
    IMPORTING
      ANSWER                = LV_RESP.

  IF LV_RESP EQ 1.

    APPEND
    VALUE #(
              TABNAME    = 'BKPF'
              FIELDNAME  = 'BUDAT'
              FIELD_OBL  = 'X'
            ) TO LT_FIELDS.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_CHECKED'
      EXPORTING
        POPUP_TITLE = 'Informe a Data de Lançamento'
        PROGRAMNAME = 'ZSDR016'
        FORMNAME    = 'F_CHECK_DATA_LANCAMENTO_DOC'
      IMPORTING
        RETURNCODE  = RETURNCODE
      TABLES
        FIELDS      = LT_FIELDS.

    READ TABLE LT_FIELDS INTO DATA(LS_FIELDS) INDEX 1.
    IF SY-SUBRC IS INITIAL AND LS_FIELDS-VALUE IS NOT INITIAL.
      WL_DATA_PASSADO = LS_FIELDS-VALUE.

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          INPUT  = LS_FIELDS-VALUE
        IMPORTING
          OUTPUT = WL_DATA_PASSADO.
    ELSE.
      EXIT.
    ENDIF.

  ENDIF.
*  FIM US-163166 WBARBOSA 17/01/2025


*** BUG - 83481 - Inicio - CBRAND
  SELECT SINGLE *
   FROM ZSDT0040
  INTO  @DATA(W_ZSDT0040)
   WHERE DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.

*** BUG - 83481 - Inicio - CBRAND

  IT_ZSDT0041R[] = IT_ZSDT0041[].
  SORT IT_ZSDT0041R BY VBELN.
  DELETE ADJACENT DUPLICATES FROM IT_ZSDT0041R COMPARING VBELN.

  LOOP AT IT_ZSDT0041R INTO DATA(WL_41R).
    CLEAR:  WA_ZFIT0026, TG_ANTEC, WA_ZSDT0159.
    READ TABLE TG_ANTEC WITH KEY VBELN    = WL_41R-VBELN
                                 ESTORNO  = ' ' BINARY SEARCH.
    IF SY-SUBRC NE 0 OR TG_ANTEC-ADIANT EQ ICON_MESSAGE_ERROR_SMALL.
      VGRAVA = 'X'.
      CLEAR: VSEQ, VSEQ2.
      "
      CLEAR WL_41R-VLRTOT.
      LOOP AT IT_ZSDT0041 INTO DATA(WL_41) WHERE VBELN = WL_41R-VBELN.
        ADD WL_41-VLRTOT TO WL_41R-VLRTOT.
      ENDLOOP.
      IF TG_ANTEC-ADIANT EQ ICON_MESSAGE_ERROR_SMALL.
        WA_ZFIT0026-OBJ_KEY = TG_ANTEC-OBJ_KEY.
        DELETE FROM ZIB_CONTABIL WHERE OBJ_KEY = TG_ANTEC-OBJ_KEY.
        DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = TG_ANTEC-OBJ_KEY.
        COMMIT WORK.
      ELSE.
        "sequencia 1
        SELECT MAX( SEQ )
          INTO VSEQ
          FROM ZSDT0159
        WHERE DOC_SIMULACAO = WL_41R-DOC_SIMULACAO
        AND   VBELN         = WL_41R-VBELN.
        ADD 1 TO VSEQ.

        "Sequencia 2
        SELECT COUNT(*)
            INTO VSEQ2
            FROM ZFIT0026
        WHERE VBELN EQ WL_41R-VBELN.
        ADD 1 TO VSEQ2.

        VSEQC = VSEQ2.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = VSEQC
          IMPORTING
            OUTPUT = VSEQC.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR = '01'
            OBJECT      = 'ZID_LANC'
          IMPORTING
            NUMBER      = P_ZID.
        CONCATENATE WL_41R-VBELN VSEQC SY-DATUM(4) INTO WA_ZFIT0026-OBJ_KEY.

        VCHECK = 'X'.
        WHILE VCHECK EQ 'X'.

          SELECT SINGLE * FROM ZIB_CONTABIL
            INTO WA_ZIB
            WHERE OBJ_KEY EQ WA_ZFIT0026-OBJ_KEY.

          IF SY-SUBRC IS INITIAL.

            ADD 1 TO VSEQC.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = VSEQC
              IMPORTING
                OUTPUT = VSEQC.

            CONCATENATE WL_41R-VBELN VSEQC SY-DATUM(4) INTO WA_ZSDT0159-OBJ_KEY.

          ELSE.
            VCHECK = ' '.
          ENDIF.
        ENDWHILE.

      ENDIF.
******************************************************************      "Comentado Aoenning "21/02/2020
      "WG_HEADER-TPSIM
*      WA_ZFIT0026-ZID_LANC       = P_ZID.
*      WA_ZFIT0026-DOCNUM         = ''.
*      WA_ZFIT0026-VBELN          = WL_41R-VBELN.
*      WA_ZFIT0026-SEQ            = VSEQ2.
*      WA_ZFIT0026-DATA_VENC      = SWITCH #( WG_HEADER-TPSIM WHEN 'VV' THEN WG_HEADER-DTVENCOV
*                                                             WHEN 'VP' THEN WG_HEADER-DTPGTCULT
*                                                             ELSE '' ).
*      WA_ZFIT0026-MOEDA          = WG_HEADER-WAERK.
*      WA_ZFIT0026-MONT_MOEDA     = WL_41R-VLRTOT.
*      WA_ZFIT0026-TAXA           = 1.
*      WA_ZFIT0026-MONT_MI        = WL_41R-VLRTOT.
*      WA_ZFIT0026-FORMA_PAG      = 'D'.
*      WA_ZFIT0026-STATUS         = 'G'.
*      WA_ZFIT0026-UNAME          = SY-UNAME.
*      WA_ZFIT0026-DATA_REGISTRO  = SY-DATUM.
*      WA_ZFIT0026-BUKRS          = WG_HEADER-VKORG.
*      WA_ZFIT0026-RAZAO_ESPECIAL = ''.
*      WA_ZFIT0026-ELIMINADO      = ''.
*      WA_ZFIT0026-OBSERVACAO     = 'GERACAO DE BOLETO BANCARIO AUTOMÁTICA'.
*      WA_ZFIT0026-ZTERM          = 'I003'.
************************************************************************************************************
      SELECT *
        FROM ZSDT0090
        INTO TABLE @DATA(T_ZSDT0090)
        WHERE DOC_SIMULACAO  EQ @WL_41R-DOC_SIMULACAO
        AND   VBELV          EQ @WL_41R-VBELN
        AND   CATEGORIA      EQ 'V'
        AND   ESTORNO        NE 'X'
        ORDER BY SEQUENCIA DESCENDING.

      IF T_ZSDT0090[] IS NOT INITIAL.
        READ TABLE T_ZSDT0090 INTO W_ZSDT0090 INDEX 1.
        WA_ZSDT0159-DATA_VENC      = W_ZSDT0090-VALDT.
      ELSE.
        WA_ZSDT0159-DATA_VENC = SWITCH #( WG_HEADER-TPSIM WHEN 'VV' THEN WG_HEADER-DTVENCOV
                                                          WHEN 'VP' THEN WG_HEADER-DTPGTCULT
                                                          ELSE '' ).

      ENDIF.

      IF TG_ANTEC-ADIANT NE ICON_MESSAGE_ERROR_SMALL.
*        APPEND WA_ZFIT0026 TO IT_ZFIT0026.   "Comentado Aoenning "21/02/2020
      ENDIF.

      " Grava na ZIB
      SELECT SINGLE *
        FROM VBAK
        INTO @DATA(WL_VBAK)
        WHERE VBELN = @WL_41R-VBELN.

      "E-commerce - ZSDT0044 - Ajustar Gerar Pag. Antencip #112462 BG
      CLEAR: VL_NETWR, VL_MWSBP.
      SELECT SUM( NETWR ) SUM( MWSBP )
      FROM VBAP
      INTO (VL_NETWR, VL_MWSBP)
      WHERE VBELN = WL_41R-VBELN.




      WA_ZSDT0159-DOC_SIMULACAO   = WL_41R-DOC_SIMULACAO.
      WA_ZSDT0159-VBELN           = WL_41R-VBELN.
      WA_ZSDT0159-SEQ             = VSEQ.
      WA_ZSDT0159-ZID_LANC        = P_ZID. "WA_ZFIT0026-ZID_LANC. "Comentado Aoenning "21/02/2020
*      WA_ZSDT0159-OBJ_KEY         = WA_ZFIT0026-OBJ_KEY.  "Comentado Aoenning "21/02/2020
      WA_ZSDT0159-BUKRS           = WL_VBAK-VKORG.
      WA_ZSDT0159-GJAHR           = SY-DATUM+0(4).
      WA_ZSDT0159-MONT_MOEDA      = VL_NETWR + VL_MWSBP. "wl_41r-vlrtot. "E-commerce - ZSDT0044 - Ajustar Gerar Pag. Antencip #112462 BG
*      WA_ZSDT0159-DATA_VENC       = WA_ZFIT0026-DATA_VENC. "Comentado Aoenning "21/02/2020
      WA_ZSDT0159-USNAM           = SY-UNAME.
      WA_ZSDT0159-DATA_ATUAL      = SY-DATUM.
      WA_ZSDT0159-HORA_ATUAL      = SY-UZEIT.

      " 21.02.2023 - RAMON - 102323 -->
      WA_ZSDT0159-ID_TRANSACAO_FINANCEIRA = ZSDT0159-ID_TRANSACAO_FINANCEIRA.

      " 21.02.2023 - RAMON - 102323 --<


      IF TG_ANTEC-ADIANT NE ICON_MESSAGE_ERROR_SMALL.
        APPEND WA_ZSDT0159 TO IT_ZSDT0159.
      ENDIF.
      "
*      "// WBARBOSA US-163166 17-01-2025
      IF WL_DATA_PASSADO IS NOT INITIAL.
        WRITE WL_DATA_PASSADO TO WL_DATA.
      ELSE.
        WRITE SY-DATUM TO WL_DATA.
      ENDIF.
*      "// WBARBOSA US-163166 17-01-2025

      WRITE WA_ZSDT0159-DATA_VENC TO WL_VENC.
*      WRITE WA_ZFIT0026-MONT_MOEDA TO WL_VALOR."WL_41R-VLRTOT.
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*                 WRITE wl_41r-vlrtot TO wl_valor."WL_41R-VLRTOT.
      WRITE WA_ZSDT0159-MONT_MOEDA TO WL_VALOR."WL_41R-VLRTOT.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

      CONCATENATE 'VENDA' WL_41R-DOC_SIMULACAO INTO WL_BKTXT SEPARATED BY SPACE.

      " 21.02.2023 - RAMON - 102323 -->

      DATA LV_PE_ECOMM TYPE DZUONR.

      IF WG_HEADER-ECOMMERCE = 'X'.

        LV_PE_ECOMM = 'PE' && WG_HEADER-ID_ORDER_ECOMMERCE.

      ENDIF.
      " 21.02.2023 - RAMON - 102323 --<

      REFRESH: TG_MSG, TG_BDC.
      PERFORM F_PREENCHER_DYNPRO USING:
                'X' 'SAPMF05A'                      '0113',
                ' ' 'BKPF-BLDAT'                    WL_DATA,
                ' ' 'BKPF-BLART'                    'DZ',
                ' ' 'BKPF-BUKRS'                    WL_VBAK-VKORG,
                ' ' 'BKPF-BUDAT'                    WL_DATA,
                ' ' 'BKPF-MONAT'                    WL_DATA+3(2),
                ' ' 'BKPF-WAERS'                    WL_VBAK-WAERK,
                ' ' 'BKPF-BKTXT'                    WL_BKTXT,
                ' ' 'BKPF-XBLNR'                    WL_BKTXT,
                ' ' 'RF05A-NEWKO'                   WL_VBAK-KUNNR,
*                ' ' 'RF05A-ZUMSK'                   'A',
                ' ' 'RF05A-ZUMSK'                   'L',
                ' ' 'BDC_OKCODE'                    '/00'.


      PERFORM F_PREENCHER_DYNPRO USING:
             'X' 'SAPMF05A'                      '0304',
             ' ' 'BSEG-WRBTR'                    WL_VALOR,
             ' ' 'BSEG-GSBER'                    WL_41R-WERKS,
             ' ' 'BSEG-ZFBDT'                    WL_VENC,

             " 21.02.2023 - RAMON - 106134 -->
             ' ' 'BSEG-ZLSCH'                    W_ZSDT0040-MEIO_PAGO,
             "' ' 'BSEG-ZLSCH'                    'D',
             " 21.02.2023 - RAMON - 106134 --<

             " 21.02.2023 - RAMON - 102323 -->
             ' ' 'BSEG-ZUONR'                    LV_PE_ECOMM,
             " 21.02.2023 - RAMON - 102323 --<
             ' ' 'BDC_OKCODE'                    '=ZK'.

      PERFORM F_PREENCHER_DYNPRO USING:
             'X' 'SAPMF05A'                      '0331',
             ' ' 'BSEG-HBKID'                    W_ZSDT0040-HBKID, "'BBRA', BUG - 83481 - CBRAND
             ' ' 'BSEG-HZUON'                    WL_41R-VBELN,
             ' ' 'BDC_OKCODE'                    '=BU'.

      OPT-DISMODE = 'N'.
      OPT-DEFSIZE = 'X'.
      CALL TRANSACTION 'F-37' USING TG_BDC
        OPTIONS FROM OPT
        MESSAGES INTO TG_MSG.


      READ TABLE TG_MSG
        WITH KEY MSGTYP = 'S'
                 MSGNR  = '312'.
      IF  SY-SUBRC IS INITIAL.
        CONDENSE TG_MSG-MSGV1 NO-GAPS.
        READ TABLE IT_ZSDT0159 INTO WA_ZSDT0159 WITH KEY DOC_SIMULACAO   = WL_41R-DOC_SIMULACAO
                                                         VBELN           = WL_41R-VBELN
                                                         SEQ             = VSEQ.
        WA_ZSDT0159-ADIANT = TG_MSG-MSGV1.
        WA_ZSDT0159-GJAHR  = SY-DATUM+0(4).
        MODIFY IT_ZSDT0159 FROM WA_ZSDT0159 INDEX SY-TABIX TRANSPORTING ADIANT GJAHR.

*       "// WBARBOSA BUG-163166 14/01/2025

        DO.

          SELECT COUNT(*)
            FROM BSEG
            WHERE BUKRS EQ WA_ZSDT0159-BUKRS
            AND BELNR EQ WA_ZSDT0159-ADIANT
            AND GJAHR EQ WA_ZSDT0159-GJAHR.

          IF SY-SUBRC IS INITIAL.
            EXIT.
          ENDIF.

        ENDDO.

        SUBMIT Z_SHDB_FB02
                   WITH P_BUKRS = WA_ZSDT0159-BUKRS
                   WITH P_BELNR = WA_ZSDT0159-ADIANT
                   WITH P_GJAHR = WA_ZSDT0159-GJAHR
                   WITH P_XREF2 = WA_ZSDT0159-VBELN
                   WITH P_XREF3 = WA_ZSDT0159-ID_TRANSACAO_FINANCEIRA
                   WITH P_TIPO  = 'O'
                   WITH P_HBKID = W_ZSDT0040-HBKID AND RETURN.
*       "// WBARBOSA BUG-163166 14/01/2025

      ELSE.
        LOOP AT TG_MSG WHERE MSGTYP = 'E'.
          MESSAGE ID TG_MSG-MSGID TYPE 'S' NUMBER TG_MSG-MSGNR WITH TG_MSG-MSGV1 TG_MSG-MSGV2 TG_MSG-MSGV3 TG_MSG-MSGV4 INTO VL_MSG_AUX.
          MESSAGE VL_MSG_AUX TYPE 'I'.
        ENDLOOP.
      ENDIF.
*                              USING TL_PGT_ANT.

*      " Partida 1 e 2
*      DO 2 TIMES.
*        WA_ZIB_CONTABIL-OBJ_KEY     = WA_ZFIT0026-OBJ_KEY.
*        WA_ZIB_CONTABIL-SEQITEM     = SY-INDEX.
*        WA_ZIB_CONTABIL-GSBER       = WL_41R-WERKS.
*        WA_ZIB_CONTABIL-BUKRS       = WL_VBAK-VKORG.
*        WA_ZIB_CONTABIL-INTERFACE   = '96'.
*        WA_ZIB_CONTABIL-BKTXT       = 'Recbto Venda Insumos'.
*        CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO WA_ZIB_CONTABIL-BLDAT.
*        WA_ZIB_CONTABIL-BUDAT       = WA_ZIB_CONTABIL-BLDAT.
*        WA_ZIB_CONTABIL-GJAHR       = SY-DATUM+0(4).
*        WA_ZIB_CONTABIL-MONAT       = SY-DATUM+4(2).
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WL_41R-VBELN
*          IMPORTING
*            OUTPUT = WA_ZIB_CONTABIL-XBLNR.
*
*        WA_ZIB_CONTABIL-BLART       = 'NC'.
*
*        " Lançamento - Conta de Débito
*        CONCATENATE 'Ordem de Venda Insumos' WL_41R-VBELN INTO WA_ZIB_CONTABIL-SGTXT SEPARATED BY SPACE.
*
*        IF SY-INDEX = 1.
*          WA_ZIB_CONTABIL-BSCHL          = '09'.
*        ELSE.
*          WA_ZIB_CONTABIL-BSCHL          = '19'.
*        ENDIF.
*        WA_ZIB_CONTABIL-HKONT          = WL_VBAK-KUNNR.
*        WA_ZIB_CONTABIL-WAERS          = WL_VBAK-WAERK.
*        CONCATENATE WA_ZFIT0026-DATA_VENC+6(2) '.' WA_ZFIT0026-DATA_VENC+4(2) '.' WA_ZFIT0026-DATA_VENC+0(4) INTO  WA_ZIB_CONTABIL-ZFBDT.
*        WA_ZIB_CONTABIL-ZLSPR          = 'A'.
*        WA_ZIB_CONTABIL-ZLSCH          = 'U'.
*        WA_ZIB_CONTABIL-KIDNO          = SPACE.
*        WA_ZIB_CONTABIL-XREF1          = SPACE.
*        WA_ZIB_CONTABIL-XREF2          = SPACE.
*        WA_ZIB_CONTABIL-XREF3          = SPACE.
*        WA_ZIB_CONTABIL-BUPLA          = WL_VBAK-VKBUR.
*        WA_ZIB_CONTABIL-ZTERM          = WA_ZFIT0026-ZTERM.
*
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WL_41R-VBELN
*          IMPORTING
*            OUTPUT = WA_ZIB_CONTABIL-ZUONR.
*
*
*        WA_ZIB_CONTABIL-UMSKZ  = 'L'.
*        WA_ZIB_CONTABIL-KOSTL          = SPACE.
*        WA_ZIB_CONTABIL-AUFNR          = SPACE.
*        WA_ZIB_CONTABIL-PRCTR          = SPACE.
*        WA_ZIB_CONTABIL-WAERS_I        = 'BRL'.
*        WA_ZIB_CONTABIL-WAERS_F        = WL_VBAK-WAERK.
*
*        IF ( WL_VBAK-WAERK EQ 'USD' ).
*          WA_ZIB_CONTABIL-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBE2          = 0.
*        ELSE.
*          WA_ZIB_CONTABIL-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBTR          = WA_ZFIT0026-MONT_MOEDA.
*          WA_ZIB_CONTABIL-DMBE2          = 0.
*        ENDIF.
*
*        WA_ZIB_CONTABIL-BVTYP          = SPACE.
*        WA_ZIB_CONTABIL-HBKID          = 'BBRA'.
*        WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
*        WA_ZIB_CONTABIL-BANKL          = SPACE.
*        WA_ZIB_CONTABIL-BANKN          = SPACE.
*        WA_ZIB_CONTABIL-NEWBW          = SPACE.
*        WA_ZIB_CONTABIL-ANLN1          = SPACE.
*        WA_ZIB_CONTABIL-ANLN2          = SPACE.
*        APPEND WA_ZIB_CONTABIL TO IT_ZIB_CONTABIL.
*      ENDDO.
*      "
*      "fim ZIB
*      "
    ENDIF.
  ENDLOOP.
  CLEAR ZSDT0159-ID_TRANSACAO_FINANCEIRA.
  "
*  CHECK VGRAVA = 'X'.

*  MODIFY ZFIT0026     FROM TABLE IT_ZFIT0026.
*  MODIFY ZIB_CONTABIL FROM TABLE IT_ZIB_CONTABIL.

  DELETE IT_ZSDT0159 WHERE ADIANT IS INITIAL.
  IF IT_ZSDT0159[] IS NOT INITIAL.
*    MODIFY ZFIT0026     FROM TABLE IT_ZFIT0026.
    MODIFY ZSDT0159     FROM TABLE IT_ZSDT0159.
    COMMIT WORK.
  ENDIF.

  PERFORM CARREGA_ANTEC USING TG_ANTEC[] WG_HEADER-DOC_SIMULACAO.

ENDFORM.

* INICIO US-163166 WBARBOSA 17/01/2025
* Processo de Verificação da Data informada pelo usuario na Função POPUP_GET_VALUES_USER_CHECKED
* no perform GERAR_CONTABIL
FORM F_CHECK_DATA_LANCAMENTO_DOC TABLES   T_FIELDS STRUCTURE SVAL
                                 CHANGING ERROR  STRUCTURE SVALE.

  READ TABLE T_FIELDS INTO DATA(W_FIELDS) INDEX 1.
  IF SY-SUBRC IS INITIAL AND W_FIELDS-VALUE NE '00000000'.
    IF W_FIELDS-VALUE GE SY-DATUM.
      CLEAR ERROR.
      ERROR =
      VALUE #(
                ERRORTAB   = 'BKPF'
                ERRORFIELD = 'BUDAT'
                MSGTY      = 'I'
                MSGID      = 'ZSD'
                MSGNO      = '020'
             ).
      EXIT.
    ENDIF.
  ELSE.
    CLEAR ERROR.
    ERROR =
    VALUE #(
              ERRORTAB   = 'BKPF'
              ERRORFIELD = 'BUDAT'
              MSGTY      = 'I'
              MSGID      = 'ZSD'
              MSGNO      = '021'
           ).
    EXIT.
  ENDIF.

ENDFORM.
*  FIM US-163166 WBARBOSA 17/01/2025

FORM ELIMINAR.
  DATA W_ERRO(1) VALUE ''.

  CALL METHOD GRID3->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = TL_INDEX_ROWS.

  LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
    READ TABLE TG_ANTEC INDEX  WL_INDEX_ROWS-INDEX.
    IF SY-SUBRC = 0.
      IF TG_ANTEC-ADIANT EQ ICON_MESSAGE_ERROR_SMALL
         OR TG_ANTEC-ESTORNO IS NOT INITIAL.
        DELETE FROM ZSDT0159 WHERE ZID_LANC = TG_ANTEC-ZID_LANC.
        DELETE FROM ZFIT0026 WHERE ZID_LANC = TG_ANTEC-ZID_LANC.
        IF TG_ANTEC-ADIANT EQ ICON_MESSAGE_ERROR_SMALL .
          DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY = TG_ANTEC-OBJ_KEY.
          DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = TG_ANTEC-OBJ_KEY.
        ENDIF.
        COMMIT WORK.
      ELSE.
        W_ERRO = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "
  IF W_ERRO = 'X'.
    MESSAGE 'Itens selecionados não podem ser eliminados!' TYPE 'I'.
  ENDIF.
  PERFORM CARREGA_ANTEC USING TG_ANTEC[] WG_HEADER-DOC_SIMULACAO.
  "
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM ESTORNAR_CONTABIL .
  DATA: VEST(1)      VALUE '',
        VDATA(10),
        P_ERRO(1),
        ZIB_DATA_DOC TYPE BKPF-BLDAT,
        WA_ZIB       TYPE ZIB_CONTABIL.

  LOOP AT TG_ANTEC.
    IF TG_ANTEC-ADIANT IS NOT INITIAL AND
      TG_ANTEC-ADIANT NE ICON_MESSAGE_ERROR_SMALL AND
      TG_ANTEC-ADIANT NE ICON_OPERATION AND
      TG_ANTEC-AUGBL IS INITIAL AND
      TG_ANTEC-ESTORNO IS INITIAL.
      VEST = 'X'.
    ENDIF.
  ENDLOOP.
  "
  IF VEST IS INITIAL.
    MESSAGE 'Não há documentos para estornar!' TYPE 'I'.
    EXIT.
  ENDIF.
  "
  CALL METHOD GRID3->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = TL_INDEX_ROWS.

  LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
    READ TABLE TG_ANTEC INDEX  WL_INDEX_ROWS-INDEX.
    CHECK SY-SUBRC = 0.
    IF TG_ANTEC-ADIANT IS NOT INITIAL
       AND TG_ANTEC-ADIANT NE ICON_MESSAGE_ERROR_SMALL
       AND TG_ANTEC-ADIANT NE ICON_OPERATION
       AND TG_ANTEC-AUGBL IS INITIAL
       AND TG_ANTEC-ESTORNO IS INITIAL.

*      SELECT SINGLE * FROM ZIB_CONTABIL INTO WA_ZIB WHERE OBJ_KEY EQ TG_ANTEC-OBJ_KEY.
*
*      IF SY-SUBRC IS INITIAL.
      SELECT SINGLE BLDAT
        FROM BKPF
        INTO ZIB_DATA_DOC
        WHERE BUKRS = TG_ANTEC-BUKRS
        AND   BELNR = TG_ANTEC-ADIANT
        AND   GJAHR = TG_ANTEC-GJAHR.

      FREE: TI_BDCDATA.
      CLEAR: ZIB_DATA_DOC, VDATA.

*      CONCATENATE WA_ZIB-BLDAT+6(4) WA_ZIB-BLDAT+3(2) INTO ZIB_DATA_DOC.

      IF ZIB_DATA_DOC(6) EQ SY-DATUM(6).
        "mesmo mes
        PERFORM BATCH_INPUT USING:  'X' 'SAPMF05A'     '0105',
                                   ''  'BDC_CURSOR'   'UF05A-STGRD',
                                   ''  'BDC_OKCODE'   '=BU',
                                   ''  'RF05A-BELNS'  TG_ANTEC-ADIANT,
                                   ''  'BKPF-BUKRS'   TG_ANTEC-BUKRS,
                                   ''  'RF05A-GJAHS'  TG_ANTEC-GJAHR,
                                   ''  'UF05A-STGRD'  '01'.
      ELSE.
        "mes retroativo
        CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM(4) INTO VDATA.

        PERFORM BATCH_INPUT USING:  'X' 'SAPMF05A'     '0105',
                                   ''  'BDC_CURSOR'   'UF05A-STGRD',
                                   ''  'BDC_OKCODE'   '=BU',
                                   ''  'RF05A-BELNS'  TG_ANTEC-ADIANT,
                                   ''  'BKPF-BUKRS'   TG_ANTEC-BUKRS,
                                   ''  'RF05A-GJAHS'  TG_ANTEC-GJAHR,
                                   ''  'UF05A-STGRD'  '02',
                                   ''  'BSIS-BUDAT'   VDATA,
                                   ''  'BSIS-MONAT'   SY-DATUM+4(2).
      ENDIF.
*      ENDIF.

      CLEAR P_ERRO.
      PERFORM ZF_CALL_TRANSACTION TABLES IT_MSG USING 'FB08' CHANGING P_ERRO WG_DOCUMENTO.
      IF P_ERRO IS INITIAL.
        UPDATE ZSDT0159 SET ESTORNO       = 'X'
                            USNAM_E       = SY-UNAME
                            DATA_ATUAL_E  = SY-DATUM
                            HORA_ATUAL_E  = SY-UZEIT
*       WHERE OBJ_KEY =  TG_ANTEC-OBJ_KEY. "// WBARBOSA BUG-163166 14/01/2025
        WHERE ADIANT EQ TG_ANTEC-ADIANT.   "// WBARBOSA BUG-163166 14/01/2025

        DELETE FROM ZFIT0026 WHERE ZID_LANC = TG_ANTEC-ZID_LANC.

        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
      ELSE.
        READ TABLE IT_MSG INTO DATA(WA_MESS) WITH KEY MSGTYP = 'E'.
        IF SY-SUBRC IS INITIAL.
          MESSAGE ID WA_MESS-MSGID TYPE 'S' NUMBER WA_MESS-MSGNR WITH WA_MESS-MSGV1 WA_MESS-MSGV2 WA_MESS-MSGV3 WA_MESS-MSGV4
          DISPLAY LIKE WA_MESS-MSGTYP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM CARREGA_ANTEC USING TG_ANTEC[] WG_HEADER-DOC_SIMULACAO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG_ACAO
*&---------------------------------------------------------------------*
*       Gravar as LOG de Ações Aprovar / Reprovar / Bloquear
*----------------------------------------------------------------------*
*      -->P_5342   text
*----------------------------------------------------------------------*
FORM LOG_ACAO USING P_ACAO
                    P_COMPLEMENTO
    " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
           CHANGING P_CANCELADO.
  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL <--

  DATA: TL_TEXTO TYPE CATSXT_LONGTEXT_ITAB,
        WL_TEXTO TYPE LINE OF CATSXT_LONGTEXT_ITAB.
  DATA MOTIVO TYPE ZSDED033.
  DATA: ID TYPE ZSDED032.

  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
  DATA LR_TPSIM TYPE RANGE OF CHAR2.

  APPEND 'IEQAD' TO LR_TPSIM.
  APPEND 'IEQVV' TO LR_TPSIM.
  APPEND 'IEQVP' TO LR_TPSIM.
  APPEND 'IEQTS' TO LR_TPSIM.
  APPEND 'IEQTV' TO LR_TPSIM.
  APPEND 'IEQVF' TO LR_TPSIM.

  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  CLEAR: TG_LOG_ACAO.

  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
  CLEAR LV_ERRO.
  " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

  PERFORM GET_NEXT_NUMBER
              USING
                 'ZID_LOG'
                 '01'
              CHANGING
                 ID.

  REFRESH: TL_TEXTO.
  CLEAR: CONVERT_TEXTO.

  CASE P_ACAO.
    WHEN 'A' OR 'R' OR 'B'.

      " 04.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->

      IF P_ACAO = 'A'.

        CLEAR GS_0308.

        SELECT SINGLE * FROM ZSDT0308
          INTO GS_0308
            WHERE DOC_SIMU = WG_HEADER-DOC_SIMULACAO.

      ENDIF.

      IF P_ACAO = 'A' AND WG_HEADER-WAERK = 'BRL'
        AND TPSIM IN LR_TPSIM
        AND GS_0308 IS INITIAL.

        PERFORM F_POPUP_BOLETA CHANGING MOTIVO P_CANCELADO.

      ELSE.

        IF GS_0308 IS NOT INITIAL.
          GV_TAXA_NEG = GS_0308-TAXA_NEG.
        ENDIF.

        " 04.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            IM_TITLE = 'Descreva o Motivo'
          CHANGING
            CH_TEXT  = TL_TEXTO.

        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
        IF SY-UCOMM = 'CX_CANC'.
          P_CANCELADO = 'X'.
          EXIT.
        ENDIF.
        " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

        LOOP AT TL_TEXTO INTO WL_TEXTO.
          IF SY-TABIX EQ 1.
            MOTIVO = WL_TEXTO.
          ELSE.
            MOTIVO = |{ MOTIVO } { WL_TEXTO }|.
          ENDIF.
        ENDLOOP.

      ENDIF. " 04.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL

    WHEN 'G'.
      MOTIVO  = 'Gerando Ordens de Venda'.
    WHEN 'S'.
      MOTIVO  = 'Ajustando Quantidade de Sacas'.
    WHEN 'V'.
      MOTIVO  = 'Agregando Desct. Absoluto lançado pelo Cockpit'.
    WHEN 'W'.
      MOTIVO  = 'Recalculando os Valores dos Itens com o Vlr Total.'.

*Início - Sara Oikawa - 38859 - Agosto/2020
    WHEN 'J'.
      MOTIVO  = 'Alterando Juros ao Ano'.
    WHEN 'D'.
      MOTIVO  = 'Alterando Valores de Adiantamento'.
*Fim - Sara Oikawa - 38859 - Agosto/2020
* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
    WHEN 'C'.
      MOTIVO  = 'Alterando Cultura/Safra Aplicação'.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
    WHEN 'T'.
      CONCATENATE 'Alterando Condição de Pagamento' P_COMPLEMENTO INTO MOTIVO SEPARATED BY SPACE.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8

    WHEN 'F'.

      SELECT SINGLE FUNRURAL
        INTO @DATA(_FUN)
        FROM ZSDT0040
        WHERE DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.

      IF _FUN IS INITIAL.
        MOTIVO = 'Estorno da Exceção Funrural'.
      ELSE.
        MOTIVO = 'Exceção Funrural'.
      ENDIF.


  ENDCASE.

  TG_LOG_ACAO =
  VALUE #(
            ID_HISTORICO  = ID
            MOTIVO        = MOTIVO
            DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO
            STATUS        = P_ACAO
            USNAM         = SY-UNAME
            DATA_ATUAL    = SY-DATUM
            HORA_ATUAL    = SY-UZEIT
         ).

  MODIFY ZSDT0186 FROM TG_LOG_ACAO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG_ACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_LOG_ACAO .

  DATA: WL_LAYOUT_LOG TYPE  SLIS_LAYOUT_ALV.
  DATA: BEGIN OF TL_LOG OCCURS 0,
          ID_HISTORICO  TYPE ZSDT0186-ID_HISTORICO,
          DOC_SIMULACAO TYPE ZSDT0186-DOC_SIMULACAO,
          STATUS(25),
          MOTIVO        TYPE ZSDT0186-MOTIVO,
          USNAM         TYPE ZSDT0186-USNAM,
          DATA_ATUAL    TYPE ZSDT0186-DATA_ATUAL,
          HORA_ATUAL    TYPE ZSDT0186-HORA_ATUAL,
        END OF TL_LOG.

  REFRESH: TL_LOG.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  SORT TG_LOG BY ID_HISTORICO.
  DELETE ADJACENT DUPLICATES FROM TG_LOG COMPARING ID_HISTORICO.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  LOOP AT TG_LOG.
    MOVE-CORRESPONDING: TG_LOG TO TL_LOG.

    IF TG_LOG-STATUS EQ 'A'.
      TL_LOG-STATUS = 'Aprovar'.
    ELSEIF TG_LOG-STATUS EQ 'R'.
      TL_LOG-STATUS = 'Reprovar'.
    ELSEIF TG_LOG-STATUS EQ 'B'.
      TL_LOG-STATUS = 'Bloquear'.
    ELSEIF TG_LOG-STATUS EQ 'S'.
      TL_LOG-STATUS = 'Ajustar Sacas'.
    ELSEIF TG_LOG-STATUS EQ 'G'.
      TL_LOG-STATUS = 'Gerar O.V.'.
    ELSEIF TG_LOG-STATUS EQ 'V'.
      TL_LOG-STATUS = 'Agregar Desc. Absoluto'.
    ELSEIF TG_LOG-STATUS EQ 'W'.
      TL_LOG-STATUS = 'Recalculo'.
* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    ENDIF.

    ELSEIF TG_LOG-STATUS EQ 'J'.
      TL_LOG-STATUS = 'Alterar Juros Ao Ano'.
    ELSEIF TG_LOG-STATUS EQ 'D'.
      TL_LOG-STATUS = 'Alterar Adiantamento'.
    ELSEIF TG_LOG-STATUS EQ 'C'.
      TL_LOG-STATUS = 'Alterar Cultura/Safra'.
    ELSEIF TG_LOG-STATUS EQ 'F'.
      TL_LOG-STATUS = 'Alterar Funrural'.
    ELSEIF TG_LOG-STATUS EQ 'T'.
      TL_LOG-STATUS = 'Alterar Cond.Pgto'.
    ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    APPEND TL_LOG.
  ENDLOOP.

  SORT TL_LOG BY ID_HISTORICO.

  PERFORM MONTAR_LAYOUT_LOG.

  WL_LAYOUT_LOG-ZEBRA = C_X.
  WL_LAYOUT_LOG-COLWIDTH_OPTIMIZE = C_X.
  WL_LAYOUT_LOG-WINDOW_TITLEBAR = 'Simulador de Vendas - Log de Ações'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM    = V_REPORT
      IS_LAYOUT             = WL_LAYOUT_LOG
      IT_FIELDCAT           = ESTRUTURA[]
      I_DEFAULT             = ' '
      I_SAVE                = ' '
      I_SCREEN_START_COLUMN = 3
      I_SCREEN_START_LINE   = 3
      I_SCREEN_END_COLUMN   = 100
      I_SCREEN_END_LINE     = 13
    TABLES
      T_OUTTAB              = TL_LOG.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_LOG .

  REFRESH: ESTRUTURA.
  PERFORM MONTAR_ESTRUTURA_ALV USING:
     1  'ZSDT0186'    'ID_HISTORICO'     'TL_LOG_ACAO' 'ID_HISTORICO'  ' '            ' ' ' '  ' ' ' ',
     2  ' '           ' '                'TL_LOG_ACAO' 'STATUS'        'Ação'         ' ' ' '  ' ' ' ',
     3  'ZSDT0186'    'MOTIVO'           'TL_LOG_ACAO' 'MOTIVO'        ' '            ' ' ' '  ' ' ' ',
     4  'ZSDT0186'    'USNAM'            'TL_LOG_ACAO' 'USNAM'         'Usuário'      ' ' ' '  ' ' ' ',
     5  'ZSDT0186'    'DATA_ATUAL'       'TL_LOG_ACAO' 'DATA_ATUAL'    'Data Atual.'  ' ' ' '  ' ' ' ',
     6  'ZSDT0186'    'HORA_ATUAL'       'TL_LOG_ACAO' 'HORA_ATUAL'    'Hora Atual.'  ' ' ' '  ' ' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_4437   text
*      -->P_4438   text
*      -->P_4439   text
*      -->P_4440   text
*      -->P_4441   text
*      -->P_4442   text
*      -->P_4443   text
*      -->P_4444   text
*      -->P_4445   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-EDIT          = P_EDIT.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.

*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM F_PREENCHER_DYNPRO USING L_START TYPE C L_NAME TYPE C L_VALUE.

  MOVE L_START TO WG_BDC-DYNBEGIN.
  IF L_START = 'X'.
    MOVE:
  L_NAME  TO WG_BDC-PROGRAM,
  L_VALUE TO WG_BDC-DYNPRO.
  ELSE.
    MOVE:
      L_NAME  TO WG_BDC-FNAM,
      L_VALUE TO WG_BDC-FVAL.
  ENDIF.
  APPEND WG_BDC TO TG_BDC.
  CLEAR: WG_BDC.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Module  CLEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CLEAR INPUT.
  CLEAR: WG_HEADER-DTPGTCULT, WG_HEADER-MEIO_PAGO, WG_HEADER-HBKID, WG_HEADER-DTINIJUROS, WG_HEADER-KURSF, WG_HEADER-DTVENCOV.
*Início - Sara Oikawa - 38859 - Agosto/2020
  CLEAR  WG_MEIO_PAGO.
*Fim - Sara Oikawa - 38859 - Agosto/2020
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  VLR_AJUSTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VLR_AJUSTE.

  DATA: VLR_ANTECIPADO_AUX TYPE ZSDED005,
        VTA_BRUTO          TYPE P DECIMALS 2,
        VLR_TROTOT         TYPE P DECIMALS 2,
        VLR_ANTECIPADO     TYPE  NETWR_AK.

  CLEAR: VTA_SISTPRO, VLR_TROTOT.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  DATA: INSS       TYPE ZSDT0042-VLR_PERC.
  DATA: FUNDEINFRA TYPE ZSDT0042-VLR_PERC.
  DATA: FETHAB     TYPE ZSDT0042-VLR_ALIQ.

  IF WG_ACAO EQ C_ATUAL AND SY-UCOMM NE 'BTN_FUN'.
    INSS   = WG_HEADER-INSS.
    FETHAB = WG_HEADER-FACS.

  ELSE.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV'   "FF #174195
      OR  WG_HEADER-TPSIM EQ 'TT' .                         "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025

* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

      SELECT SINGLE *
      FROM KNA1
      INTO @DATA(REGIAO)
     WHERE KUNNR EQ @WG_HEADER-KUNNR.

      SELECT *
        FROM ZSDT0042
        INTO TABLE @DATA(IT_0042)
         WHERE CULTURA EQ @WG_HEADER-CULTURA
           AND WAERK   EQ @WG_HEADER-WAERK
           AND ESTADO  EQ @REGIAO-REGIO
           AND SAFRA   EQ @WG_HEADER-SAFRA  " RJF - 61516 - 2023.08.31
           AND VAL_DE  LE @WG_HEADER-DTENT  " RJF - 61516 - 2023.08.31
           AND VAL_ATE GE @WG_HEADER-DTENT. " RJF - 61516 - 2023.08.31

*   Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*    DATA: inss TYPE zsdt0042-vlr_perc.
*    DATA: fethab TYPE zsdt0042-vlr_aliq.
*   Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

      LOOP AT IT_0042 ASSIGNING FIELD-SYMBOL(<L_0042>).
        IF ( <L_0042>-WITHT EQ 'FR' ).
          IF REGIAO-STKZN IS NOT INITIAL.
            IF WG_HEADER-FUNRURAL IS INITIAL.
              ADD <L_0042>-VLR_PERC TO INSS.
            ELSE.
              ADD <L_0042>-VLR_PERC1 TO INSS.
            ENDIF.
            ADD <L_0042>-VLR_ALIQ TO FETHAB.
          ENDIF.
        ELSEIF ( <L_0042>-WITHT EQ 'FI' ).
          CHECK <L_0042>-ESTADO EQ 'GO'.
          IF WG_HEADER-FUNDEINFRA_EXCE IS INITIAL.
            ADD <L_0042>-VLR_PERC TO FUNDEINFRA.
          ELSE.
            ADD <L_0042>-VLR_PERC1 TO FUNDEINFRA.
          ENDIF.
        ELSE.
          ADD <L_0042>-VLR_PERC TO INSS.
          ADD <L_0042>-VLR_ALIQ TO FETHAB.
        ENDIF.
      ENDLOOP.

    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    WG_HEADER-INSS = INSS.
    WG_HEADER-FACS = FETHAB.

  ENDIF.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6


  IF WG_ACAO EQ C_ATUAL AND SY-UCOMM NE 'BTN_FI'.
    FUNDEINFRA   = WG_HEADER-FUNDEINFRA.
  ELSE.
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
    IF WG_HEADER-TPSIM EQ 'TS' OR WG_HEADER-TPSIM EQ 'TV'   "FF #174195
    OR WG_HEADER-TPSIM EQ 'TT' .                            "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025

      SELECT SINGLE *
      FROM KNA1
      INTO REGIAO
     WHERE KUNNR EQ WG_HEADER-KUNNR.

      SELECT *
        FROM ZSDT0042
        INTO TABLE IT_0042
         WHERE CULTURA EQ WG_HEADER-CULTURA
           AND WAERK   EQ WG_HEADER-WAERK
           AND ESTADO  EQ REGIAO-REGIO
           AND SAFRA   EQ WG_HEADER-SAFRA
           AND VAL_DE  LE WG_HEADER-DTENT
           AND VAL_ATE GE WG_HEADER-DTENT
           AND WITHT   EQ 'FI'.

      LOOP AT IT_0042 ASSIGNING <L_0042>.
        CHECK <L_0042>-ESTADO EQ 'GO'.
        IF WG_HEADER-FUNDEINFRA_EXCE IS INITIAL.
          ADD <L_0042>-VLR_PERC TO FUNDEINFRA.
        ELSE.
          ADD <L_0042>-VLR_PERC1 TO FUNDEINFRA.
        ENDIF.
      ENDLOOP.
    ENDIF.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
    WG_HEADER-FUNDEINFRA = FUNDEINFRA.

  ENDIF.

*    TRY .
*        DATA(INSS)   = COND #( WHEN WG_HEADER-FUNRURAL IS INITIAL
*                                      THEN IT_0042[ WITHT = 'FR' ]-VLR_PERC
*                                      ELSE IT_0042[ WITHT = 'FR' ]-VLR_PERC1 ).

*        DATA(FETHAB) = IT_0042[ WITHT = 'FT' ]-VLR_ALIQ.
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*    ENDTRY.

  SELECT SINGLE PREC_CULT
     FROM ZSDT0040
     INTO @DATA(VLR_PREC_CULT)
    WHERE DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.

  IF ( VLR_PREC_CULT IS NOT INITIAL ).
    TRY.
        VLR_ANTECIPADO_AUX = ( VLR_PREC_CULT / ( 1 + ( WG_HEADER-ANTEC / 100 ) ) ) .
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
    VLR_ANTECIPADO = VLR_ANTECIPADO_AUX.
  ELSE.
    VLR_ANTECIPADO = WG_HEADER-PREC_ANT_CULT.
  ENDIF.

  VTA_BRUTO = CONVERT_TRATOTSC * VLR_ANTECIPADO.

  SELECT SINGLE TROTOTSC
     FROM ZSDT0040
     INTO VLR_TROTOT
    WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

*  VLR_TROTOT = REDUCE #( INIT X TYPE MENGE_D FOR LS IN TG_ITENS NEXT X = X + LS-TRTOT ).
  VLR_TOTAL = WG_HEADER-VLRTOT.

  TRY .
      VTA_SISTPRO = ( VTA_BRUTO - ( VTA_BRUTO * ( INSS / 100 ) ) - ( VTA_BRUTO * ( FUNDEINFRA / 100 ) ) ) - ( FETHAB * VLR_TROTOT ).
    CATCH CX_SY_ZERODIVIDE.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RECALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RECALCULO.

  DATA: WL_TOT_VLR   TYPE ZSDT0041-ZWERT,
        TOTAL_AJUSTE TYPE ZDMBTR,
        DIFERENCA    TYPE P DECIMALS 2,
        RESTO        TYPE P DECIMALS 2.

  DIFERENCA = VTA_SISTPRO - VLR_TOTAL.

  SORT TG_ITENS BY ZMENG.

  LOOP AT TG_ITENS ASSIGNING FIELD-SYMBOL(<ITENS>).

    TRY .
        <ITENS>-VLR_AJUSTE = ( <ITENS>-VLRTOT / VLR_TOTAL ) * DIFERENCA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    ADD <ITENS>-VLR_AJUSTE TO TOTAL_AJUSTE.

    IF LINES( TG_ITENS[] ) EQ SY-TABIX.
      IF TOTAL_AJUSTE NE DIFERENCA.
        RESTO = DIFERENCA - TOTAL_AJUSTE.
        ADD RESTO TO <ITENS>-VLR_AJUSTE.
      ENDIF.
    ENDIF.

    ADD <ITENS>-VLR_AJUSTE TO <ITENS>-VLRTOT.

  ENDLOOP.

*  BREAK-POINT.
*    Realiza o calculo do campo "Valor Total"
  PERFORM VLRTOT.
  PERFORM GRAVA_DADOS.
*  PERFORM CALCULA_ITENS.
*  PERFORM CALCULA_HEADER USING WL_TOT_VLR.

  SORT TG_ITENS BY POSNR.

  PERFORM LOG_ACAO USING 'W' '' CHANGING LV_ERRO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REAJUSTE_41
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REAJUSTE_41 .

  DATA VL_DIF TYPE NETWR_AP.
  DATA VL_TOT TYPE NETWR_AP.


  SELECT *
    FROM ZSDT0041
    INTO TABLE @DATA(IT_0041_AUX)
    WHERE DOC_SIMULACAO EQ @WG_HEADER-DOC_SIMULACAO.

  CHECK IT_0041_AUX IS NOT INITIAL.

  SELECT *
    FROM VBAP
     INTO TABLE @DATA(IT_VBAP)
     FOR ALL ENTRIES IN @IT_0041_AUX
    WHERE VBELN EQ @IT_0041_AUX-VBELN
      AND MATNR EQ @IT_0041_AUX-MATNR.

  LOOP AT IT_0041_AUX INTO DATA(L_0041).
    LOOP AT IT_VBAP INTO DATA(L_VBAP) WHERE VBELN EQ L_0041-VBELN AND MATNR EQ L_0041-MATNR.

      SELECT SINGLE KNUMV
        FROM VBAK
        INTO @DATA(VL_KNUMV)
        WHERE VBELN EQ @L_VBAP-VBELN.


*---> 19/07/2023 - Migração S4 - DG
*      SELECT SINGLE kbetr
*        FROM konv
*        INTO @DATA(vl_kbetr)
*        WHERE knumv EQ @vl_knumv
*          AND kposn EQ @l_vbap-posnr
*          AND kschl EQ 'RB00'.

      SELECT SINGLE KBETR
        FROM V_KONV
        INTO @DATA(VL_KBETR)
        WHERE KNUMV EQ @VL_KNUMV
          AND KPOSN EQ @L_VBAP-POSNR
          AND KSCHL EQ 'RB00'.
*<--- 19/07/2023 - Migração S4 - DG


      VL_TOT = 0.
      VL_DIF = 0.
      VL_TOT = L_VBAP-NETWR + L_VBAP-MWSBP.

      IF VL_TOT NE L_0041-VLRTOT.

        VL_DIF = L_0041-VLRTOT - VL_TOT.

        ADD VL_KBETR TO VL_DIF.

        APPEND VALUE #(
                        VBELN         = L_VBAP-VBELN
                        MATNR         = L_VBAP-MATNR
                        DESC_ABSOLUTO = VL_DIF
                      ) TO IT_ZSDT0041_AJUSTE.

        CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR_2'
          TABLES
            TI_ITENS_OV       = IT_ZSDT0041_AJUSTE
          EXCEPTIONS
            OV_NAO_ENCONTRADA = 1
            OTHERS            = 2.

        FREE IT_ZSDT0041_AJUSTE.

      ENDIF.
    ENDLOOP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_FUNRURAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_FUNRURAL.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
*  SELECT SINGLE funrural
*    INTO wg_header-funrural
*    FROM zsdt0040
*    WHERE doc_simulacao EQ wg_header-doc_simulacao.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  IF WG_HEADER-FUNRURAL IS INITIAL.
    WG_HEADER-FUNRURAL = ABAP_TRUE.
  ELSE.
    WG_HEADER-FUNRURAL = ABAP_FALSE.
  ENDIF.

  WG_HEADER-FUNUSER  = SY-UNAME.
  WG_HEADER-FUNDATA  = SY-DATUM.
  WG_HEADER-FUNHORA  = SY-UZEIT.

  UPDATE ZSDT0040 SET FUNRURAL = WG_HEADER-FUNRURAL
                      FUNUSER  = WG_HEADER-FUNUSER
                      FUNDATA  = WG_HEADER-FUNDATA
                      FUNHORA  = WG_HEADER-FUNHORA
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  PERFORM LOG_ACAO  USING 'F' '' CHANGING LV_ERRO.
ENDFORM.

* Início - CS2019000925 - Sara Oikawa - Julho/2020
*&---------------------------------------------------------------------*
*&      Form  DISPARA_HEDGE_AJUSTE_SACAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPARA_HEDGE_AJUSTE_SACAS TABLES P_TL_INPUT_0041
                         USING  P_WL_INPUT_0040 P_WL_TIPO.

******************************************************************************
**** TABELA ZSDT0041 COM MATKL PARA AGRUPAR OS DADOS PELO GRUPO DE MERCADORIA
******************************************************************************
  TYPES BEGIN OF TY_0041.
  INCLUDE TYPE ZSDT0041.
  TYPES MATKL TYPE MATKL.
  TYPES BRGEW TYPE BRGEW.
  TYPES DTPGTCULT TYPE BAPI_JBD_DTE_DZTERM.
  TYPES KURSK TYPE KURSK.
  TYPES END OF TY_0041.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
  DATA: OBJ_TX_CURVA TYPE REF TO ZCL_WEBSERVICE_TX_CURVA,
        OBJ_INSERE   TYPE REF TO ZCL_TAXA_CURVA_DB,
        OBJ_0094     TYPE REF TO ZCL_TAXA_CURVA.

******************************************
********  TABELAS INTERNAS E WORK AREAS
******************************************
  DATA: IT_0041        TYPE TABLE OF TY_0041,
        WA_0040        TYPE ZSDT0040,
        TIPO           TYPE CHAR3,
        VAR_TOTAL      TYPE DMBTR,
        SEQUENCIA      TYPE POSNR,
        TAXA           TYPE KURSF,
        TAXA_0090      TYPE KURSF,
        VL_QTDE_EQUAL  TYPE C,
        BRGEW          TYPE BRGEW,
        V_LIQ          TYPE NETWR,
        V_IMP          TYPE MWSBP,

        SET_PORC_FRETE TYPE P DECIMALS 5,
        VLR_FRETE      TYPE ZSDT0037-VLR_FRETE,
        UM_FRETE       TYPE ZSDT0037-MEINS,
        VLR_FRETE_V    TYPE ZSDT0037-VLR_FRETE,
        UM_FRETE_V     TYPE ZSDT0037-MEINS.

  DATA: IT_SET TYPE TABLE OF RGSB4,
        WA_SET TYPE RGSB4.


  FIELD-SYMBOLS <FINAL> TYPE TY_0041.

**********************
****** LIBERA OS OBJ
**********************
  FREE: OBJ_TX_CURVA, OBJ_0094, OBJ_INSERE.

**********************
****** CRIA OS OBJ
**********************
  CREATE OBJECT: OBJ_TX_CURVA, OBJ_0094, OBJ_INSERE.

  CLEAR TAXA.

  WA_0040 = P_WL_INPUT_0040.

  TIPO = P_WL_TIPO.

  CASE TIPO.

    WHEN 'VDI'. "Venda mercado Interno

**********************************************************************************
* MONTA A ESTRUTURA COM OS DADOS AGRUPADOS SOMADOS E DIVIDIDOS PELO INCO1 E MATKL
**********************************************************************************
      OBJ_0094->AGRUPA_DADOS( EXPORTING I_NUMERO = WA_0040-DOC_SIMULACAO
                                        I_TIPO   = TIPO
*                            T_ITENS  = T_ITENS
                              IMPORTING I_0041   = IT_0041
                                       ).

*        SELECT SINGLE * FROM ZSDT0040
*          INTO @DATA(WA_0040)
*          WHERE DOC_SIMULACAO EQ @I_NUMERO.

*    MONTA A SAIDA PARA INCLUIR NO DISPARO
      TIPO = P_WL_TIPO.
      LOOP AT IT_0041 ASSIGNING <FINAL>.

        IF <FINAL>-VLR_AJUSTE IS INITIAL.
          CONTINUE.
        ENDIF.

        OBJ_0094->SET_NUMERO( <FINAL>-DOC_SIMULACAO ).

        OBJ_0094->SET_DATA_VENC( <FINAL>-DTPGTCULT ).

        OBJ_0094->SET_DATA_LIB( SY-DATUM ).

        VAR_TOTAL = <FINAL>-VLR_AJUSTE.

        OBJ_0094->SET_TOTAL_PROPORCIONAL( VAR_TOTAL ).
*        OBJ_0094->TIPO_TAXA_IN( TIPO ).
        OBJ_0094->TIPO_TAXA_IN( '' ).


        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
        TRY .

            OBJ_0094->SET_TAXA_CURVA(
            OBJ_TX_CURVA->BUSCAR_TAXA( I_DATA     = OBJ_0094->GET_DATA_VENC( )
                                       I_DATA_LIB = OBJ_0094->GET_DATA_LIB(  )
                                       I_TIPO     = OBJ_0094->GET_TIPO_TAXA( )
                                     ) ).

          CATCH ZCX_WEBSERVICE .

            MESSAGE W104(ZSD).

        ENDTRY.
        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >

        OBJ_0094->SET_TIPO( TIPO ).
        OBJ_0094->SET_INCOTERMS( <FINAL>-INCO1 ).
        OBJ_0094->SET_MATKL( I_MATKL = <FINAL>-MATKL
                             I_BRGEW = <FINAL>-BRGEW
                             ).
        OBJ_0094->SET_BEZEI( '' ).
        OBJ_0094->SET_CADENCIA_IN( I_CADENCIA = <FINAL>-ZMENG
                                   I_NEGATIVA = 'N'
                                   ).

        OBJ_0094->SET_TAXA_IN( ).
        IF WA_0040-TPSIM EQ 'VF'.
          OBJ_0094->SET_TAXA_CAMBIO( WA_0040-KURSF ).
        ELSE.
          IF OBJ_0094->GET_TAXA_IN( ) IS INITIAL.
            OBJ_0094->SET_TAXA_CAMBIO( OBJ_0094->GET_TAXA_CURVA( ) ).
          ELSE.
            OBJ_0094->SET_TAXA_CAMBIO( OBJ_0094->GET_TAXA_IN( ) ).
          ENDIF.
        ENDIF.

        OBJ_INSERE->ZIF_TAXA_CURVA_DB~INSERIR( OBJ_TAXA = OBJ_0094 ).

      ENDLOOP.

    WHEN 'FRI'. "Frete Mercado interno

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          CLASS           = '0000'
          SETNR           = 'MAGGI_FRI_HEDGE'
          NO_DESCRIPTIONS = SPACE
          NO_RW_INFO      = SPACE
        TABLES
          SET_VALUES      = IT_SET
        EXCEPTIONS
          SET_NOT_FOUND   = 1
          OTHERS          = 2.

      IF SY-SUBRC IS INITIAL.

        WA_SET = IT_SET[ 1 ].

        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            CHR             = WA_SET-FROM
          IMPORTING
            NUM             = SET_PORC_FRETE
          EXCEPTIONS
            CONVT_NO_NUMBER = 1
            CONVT_OVERFLOW  = 2
            OTHERS          = 3.

        SET_PORC_FRETE = SET_PORC_FRETE / 100.

      ENDIF.

*****************************
***** CABEÇALHO DA SIMULAÇÃO
*****************************
*        SELECT SINGLE * FROM ZSDT0040
*          INTO WA_0040
*          WHERE DOC_SIMULACAO EQ I_NUMERO.

      SELECT SINGLE * FROM ZSDT0117
        INTO @DATA(WL_0117)
        WHERE BUKRS      EQ @WA_0040-VKORG
          AND DESATIVADO EQ @ABAP_FALSE.

      WA_0040-DT_ENTREGA_SEM = WA_0040-DT_ENTREGA_SEM + 30.
      WA_0040-DT_ENTREGA_DEF = WA_0040-DT_ENTREGA_DEF + 30.
      WA_0040-DT_ENTREGA_FET = WA_0040-DT_ENTREGA_FET + 30.

**********************************************************************************
* MONTA A ESTRUTURA COM OS DADOS AGRUPADOS SOMADOS E DIVIDIDOS PELO INCO1 E MATKL
**********************************************************************************
      OBJ_0094->AGRUPA_DADOS( EXPORTING I_NUMERO = WA_0040-DOC_SIMULACAO
                                        I_TIPO   = TIPO
*                             T_ITENS  = T_ITENS
                              IMPORTING I_0041   = IT_0041
                                       ).

      LOOP AT IT_0041 ASSIGNING <FINAL>.

        IF <FINAL>-VLR_AJUSTE IS INITIAL.
          CONTINUE.
        ENDIF.

        OBJ_0094->SET_NUMERO( <FINAL>-DOC_SIMULACAO ).

        OBJ_0094->SET_MATKL( I_MATKL = <FINAL>-MATKL
                             I_BRGEW = <FINAL>-BRGEW
                             ).

        IF <FINAL>-SPART EQ '03'.

          IF ( WA_0040-WAERK = 'USD' ) AND ( WL_0117-KURSK IS NOT INITIAL ).
            VAR_TOTAL = ( <FINAL>-VLR_AJUSTE * SET_PORC_FRETE ) * WL_0117-KURSK.
          ELSE.
            VAR_TOTAL = <FINAL>-VLR_AJUSTE * SET_PORC_FRETE.
          ENDIF.

        ELSE.
          CONTINUE.
        ENDIF.


        OBJ_0094->SET_BEZEI( '' ).
        OBJ_0094->SET_CADENCIA_IN( I_CADENCIA = <FINAL>-ZMENG
                                   I_NEGATIVA = 'S'
                                   I_0040     = WA_0040
                                   ).
        OBJ_0094->SET_DATA_LIB( SY-DATUM ).
        OBJ_0094->SET_TOTAL_PROPORCIONAL( I_TOTAL    = VAR_TOTAL
                                          I_NEGATIVA = ABAP_TRUE
                                          ).
*        OBJ_0094->TIPO_TAXA_IN( TIPO ).
        OBJ_0094->TIPO_TAXA_IN( '' ).



        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >
        TRY .

            OBJ_0094->SET_TAXA_CURVA(
            OBJ_TX_CURVA->BUSCAR_TAXA( I_DATA     = OBJ_0094->GET_DATA_VENC( )
                                       I_DATA_LIB = OBJ_0094->GET_DATA_LIB(  )
                                       I_TIPO     = OBJ_0094->GET_TIPO_TAXA( )
                                     ) ).
          CATCH ZCX_WEBSERVICE .

            MESSAGE W104(ZSD).

        ENDTRY.
        " 19.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -- >

        OBJ_0094->SET_TAXA_CAMBIO( <FINAL>-KURSK ).
        OBJ_0094->SET_TIPO( TIPO ).
        OBJ_0094->SET_INCOTERMS( <FINAL>-INCO1 ).
        OBJ_INSERE->ZIF_TAXA_CURVA_DB~INSERIR( OBJ_TAXA = OBJ_0094 ).

      ENDLOOP.


  ENDCASE.

ENDFORM.
* Fim - CS2019000925 - Sara Oikawa - Julho/2020

*Início - Sara Oikawa - 38859 - Agosto/2020
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_RECEBIMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_RECEBIMENTOS .

  TYPES: BEGIN OF TY_ZFIT0026,
           VBELN TYPE VBELN,
         END OF TY_ZFIT0026.

  DATA: LT_ZFIT0026 TYPE TABLE OF TY_ZFIT0026,
        LS_ZFIT0026 TYPE TY_ZFIT0026,

        LV_MSG      TYPE TEXT255,
        LT_VALUETAB TYPE TABLE OF STRING,
        LS_VALUETAB TYPE STRING.

  CLEAR WG_FLG_RECEB.

  IF TG_ITENS[] IS NOT INITIAL.
    SELECT VBELN
      FROM ZFIT0026
      INTO TABLE LT_ZFIT0026
      FOR ALL ENTRIES IN TG_ITENS
      WHERE VBELN EQ TG_ITENS-VBELN.
  ENDIF.

  IF TG_TRANS[] IS NOT INITIAL.
    SELECT VBELN
      FROM ZFIT0026
      APPENDING TABLE LT_ZFIT0026
      FOR ALL ENTRIES IN TG_TRANS
      WHERE VBELN EQ TG_TRANS-VBELN.
  ENDIF.

  DELETE LT_ZFIT0026 WHERE VBELN IS INITIAL.

  IF LT_ZFIT0026 IS NOT INITIAL.

    CLEAR: WG_SAVE, SY-UCOMM.
    WG_FLG_RECEB = C_X.

    READ TABLE LT_ZFIT0026 INTO LS_ZFIT0026 INDEX 1.
    MESSAGE I001(ZSD) DISPLAY LIKE 'E' WITH TEXT-E57 TEXT-E58 LS_ZFIT0026-VBELN.

*    LOOP AT LT_ZFIT0026 INTO LS_ZFIT0026.
*      LS_VALUETAB = LS_ZFIT0026-VBELN.
*      APPEND LS_VALUETAB TO LT_VALUETAB.
*    ENDLOOP.

  ENDIF.

*  IF LT_VALUETAB IS NOT INITIAL.
*
*    CLEAR WG_SAVE.
*    WG_FLG_RECEB = C_X.
*
*    CONCATENATE 'Erro. Alteração não possível!' 'Já existem recebimentos lançados.'
*                INTO LV_MSG SEPARATED BY SPACE.      "Title
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_TABLE'
*      EXPORTING
*        TITLEBAR        = LV_MSG
*        START_COLUMN    = 10
*        START_ROW       = 5
*        END_COLUMN      = 70
*        END_ROW         = 10
*        COLUMNNAME      = 'Ov´s em ZFIS26:'
**      IMPORTING
**       ANSWER          =
*      CHANGING
*        CT_DISPLAYTABLE = LT_VALUETAB.
*
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_LOG_EDICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LOG_EDICAO  TABLES P_TL_41 P_TL_41_OLD
                   USING  P_WL_40 P_WL_40_OLD.

  DATA: WL_40         TYPE          ZSDT0040,
        WL_40_OLD     TYPE          ZSDT0040,

        TL_41         TYPE TABLE OF ZSDT0041,
        WL_41         TYPE          ZSDT0041,

        TL_41_OLD     TYPE TABLE OF ZSDT0041,
        WL_41_OLD     TYPE          ZSDT0041,

        FIELDNAME(30) TYPE C.

  WL_40       = P_WL_40.
  WL_40_OLD   = P_WL_40_OLD.

* Início - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6
  " Caso Docto em inclusão, não gravar LOG
  CHECK  WL_40_OLD-DOC_SIMULACAO IS NOT INITIAL.
* Fim - Sara Oikawa - 19.10.2020  Melhorias Pacote 5/6

  TL_41[]     = P_TL_41[].
  TL_41_OLD[] = P_TL_41_OLD[].

  REFRESH TG_LOG_EDICAO.

* Grava Log Edição - Header
  DATA(T_FIELD) = ZCL_SOLICITACAO_OV=>GET_FIELDNAME_STRUCTURE( WL_40 ).
  LOOP AT T_FIELD INTO DATA(_FIELD).
    FIELDNAME = |WL_40-{ _FIELD-NAME }|.
    PERFORM INPUT_LOG_ED USING WL_40     WL_40_OLD
                               WL_41     WL_41_OLD
                               FIELDNAME 'HEADER'  '0'.
  ENDLOOP.


* Grava Log Edição - Itens
  T_FIELD = ZCL_SOLICITACAO_OV=>GET_FIELDNAME_STRUCTURE( WL_41 ).
  LOOP AT TL_41 INTO WL_41.
*    WL_TABIX_AUX = SY-TABIX.

    CLEAR: WL_41_OLD.
    READ TABLE TL_41_OLD INTO WL_41_OLD WITH KEY POSNR = WL_41-POSNR.

    LOOP AT T_FIELD INTO _FIELD.
      FIELDNAME = |WL_41-{ _FIELD-NAME }|.
      PERFORM INPUT_LOG_ED USING WL_40     WL_40_OLD
                                 WL_41     WL_41_OLD
                                 FIELDNAME 'ITENS'  WL_41-POSNR.
    ENDLOOP.

  ENDLOOP.

  IF TG_LOG_EDICAO IS NOT INITIAL.
    MODIFY ZSDT0091 FROM TABLE TG_LOG_EDICAO.
    COMMIT WORK.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INPUT_LOG_ED
*&---------------------------------------------------------------------*
FORM INPUT_LOG_ED  USING    WL_40
                            WL_40_OLD
                            WL_41
                            WL_41_OLD
                            P_CAMPO
                            P_AREA
                            P_ITEM .

  DATA: WL_40_I           TYPE ZSDT0040,
        WL_FIELD(30),
        WL_FIELD_OLD(40),
        WL_FIELD_AUX(40),
        WL_FIELD_AUX2(40).
  FIELD-SYMBOLS: <FS_FIELD>     TYPE ANY,
                 <FS_FIELD_OLD> TYPE ANY.

  WL_40_I = WL_40.

  UNASSIGN <FS_FIELD>.
  UNASSIGN <FS_FIELD_OLD>.

  WL_FIELD = P_CAMPO.
  SPLIT WL_FIELD AT '-' INTO WL_FIELD_AUX
                             WL_FIELD_AUX2.

  IF WL_FIELD_AUX2 NE 'MANDT'      AND WL_FIELD_AUX2 NE 'USNAM'  AND
     WL_FIELD_AUX2 NE 'DATA_ATUAL' AND WL_FIELD_AUX2 NE 'HORA_ATUAL' AND
     WL_FIELD_AUX2 NE 'JOB'        AND WL_FIELD_AUX2 NE 'FUNDATA'    AND
     WL_FIELD_AUX2 NE 'FUNHORA'    AND WL_FIELD_AUX2 NE 'FUNUSER'.

    CONCATENATE WL_FIELD_AUX '_OLD-' WL_FIELD_AUX2 INTO WL_FIELD_OLD.
    ASSIGN (WL_FIELD) TO <FS_FIELD>.
    ASSIGN (WL_FIELD_OLD) TO <FS_FIELD_OLD>.
    IF <FS_FIELD> IS ASSIGNED.
      IF <FS_FIELD> NE <FS_FIELD_OLD>.

        SPLIT P_CAMPO AT '-' INTO WL_FIELD
                                  WL_FIELD_AUX.

        MOVE: WL_40_I-DOC_SIMULACAO    TO WG_LOG_EDICAO-DOC_SIMULACAO,
              P_ITEM                   TO WG_LOG_EDICAO-ITEM,
              TG_LOG_ACAO-ID_HISTORICO TO WG_LOG_EDICAO-ID_HISTORICO,
              P_AREA                   TO WG_LOG_EDICAO-AREA,
              WL_FIELD_AUX             TO WG_LOG_EDICAO-CAMPO,
              <FS_FIELD>               TO WG_LOG_EDICAO-NEW_VALUE,
              <FS_FIELD_OLD>           TO WG_LOG_EDICAO-OLD_VALUE,
              SY-UNAME                 TO WG_LOG_EDICAO-USNAM,
              SY-DATUM                 TO WG_LOG_EDICAO-DATA_ATUAL,
              SY-UZEIT                 TO WG_LOG_EDICAO-HORA_ATUAL.
        APPEND WG_LOG_EDICAO TO TG_LOG_EDICAO.
        CLEAR: WG_LOG_EDICAO.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " INPUT_LOG
*Fim - Sara Oikawa - 38859 - Agosto/2020

* Início - Sara Oikawa - 07.10.2020   Melhorias Pacote 4
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_QTD_EMBALAGEM
*&---------------------------------------------------------------------*
*   Verificação da Quantidade da Embalagem (Somente para Defensivos)
*----------------------------------------------------------------------*
FORM ZF_BUSCA_QTD_EMBALAGEM_ZDEF .

  CLEAR:  WG_MATNR, WG_GROES, WG_MEINS , WG_GROES_STRING, WG_GROES_DEC.

  SELECT SINGLE MATNR GROES MEINS
    FROM MARA
    INTO ( WG_MATNR, WG_GROES, WG_MEINS )
   WHERE MATNR EQ TG_ITENS-MATNR.

  IF SY-SUBRC IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        INPUT  = TG_ITENS-MATNR
      IMPORTING
        OUTPUT = WG_MATNR.

    IF WG_GROES IS NOT INITIAL.

      WG_GROES_STRING = WG_GROES.

      IF WG_GROES CA '0123456789,'.
        CALL FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
          EXPORTING
            I_STRING                   = WG_GROES_STRING
          IMPORTING
*           E_FLOAT                    =
            E_DEC                      = WG_GROES_DEC
*           E_DECIMALS                 =
          EXCEPTIONS
            WRONG_CHARACTERS           = 1
            FIRST_CHARACTER_WRONG      = 2
            ARITHMETIC_SIGN            = 3
            MULTIPLE_DECIMAL_SEPARATOR = 4
            THOUSANDSEP_IN_DECIMAL     = 5
            THOUSAND_SEPARATOR         = 6
            NUMBER_TOO_BIG             = 7
            OTHERS                     = 8.
        IF SY-SUBRC IS INITIAL.
          CLEAR WG_GROES_STRING.
        ENDIF.
      ELSE.
        WG_GROES_STRING = C_X.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
* Fim - Sara Oikawa - 07.10.2020   Melhorias Pacote 4

* Início - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6
*&---------------------------------------------------------------------*
*&      Module  PBO0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO0110 OUTPUT.

  SET PF-STATUS 'PF0110'.
  SET TITLEBAR  'TI0110'.

  WG_SAFRA_0110_SG = WG_HEADER-SAFRA.
  CLEAR WG_IDCOMPR_0110_SG.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI0110 INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI0110 INPUT.
  CASE SY-UCOMM.
    WHEN 'SEARCH_SG'.
      IF WG_SAFRA_0110_SG IS INITIAL AND WG_IDCOMPR_0110_SG IS INITIAL.
        MESSAGE 'É obrigatório o preenchimento do campo "SAFRA" ou "ID Compra'  TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        IF WG_SAFRA_0110_SG IS NOT INITIAL AND WG_IDCOMPR_0110_SG IS NOT INITIAL.
          MESSAGE 'Informar apenas campo "SAFRA" ou campo "ID Compra'  TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          PERFORM ZF_BUSCA_DADOS_SIGAM.
        ENDIF.
      ENDIF.

    WHEN 'DELETE_SG'.
      IF  WG_0260-ID_COMPRA IS NOT INITIAL.
        CLEAR: WG_0260-ID_COMPRA,
               WG_0260-COMPRA_FIM_EXPORT.
      ENDIF.

      PERFORM ZF_DETERMINE_FUNRURAL USING ABAP_TRUE. "Forçar determinar Funrural

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_DADOS_SIGAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_BUSCA_DADOS_SIGAM .

  DATA: LV_ERRO     TYPE CHAR1,
        LV_CPF      TYPE STCD2,
        LV_CNPJ     TYPE STCD1,
        WS_ZSDT0260 TYPE ZSDT0260.

  REFRESH: TG_SIGAM, TG_0260.
  CLEAR:   WG_SIGAM, WS_ZSDT0260, LV_ERRO, LV_CPF, LV_CNPJ.

  LV_CPF =  WG_CPF.
  LV_CNPJ = WG_CNPJ.

  IF WG_IDCOMPR_0110_SG IS NOT INITIAL.
    CLEAR: WG_SAFRA_0110_SG.
  ENDIF.

  IF WG_SAFRA_0110_SG IS NOT INITIAL.
    CLEAR: WG_IDCOMPR_0110_SG.
  ENDIF.

  CALL FUNCTION 'ZSDMF011_BUSCA_COMPRA_SIGAM'
    EXPORTING
      I_SAFRA     = WG_SAFRA_0110_SG
      I_ID_COMPRA = WG_IDCOMPR_0110_SG
      I_CPF       = LV_CPF
      I_CNPJ      = LV_CNPJ
    IMPORTING
      E_ZSDT0260  = TG_0260
      E_ERRO      = LV_ERRO.


*  IF lv_erro IS INITIAL.

**  SELECT * FROM zsdt0260
**    INTO CORRESPONDING FIELDS OF TABLE tg_sigam.

  LOOP AT TG_0260 INTO WS_ZSDT0260.
    MOVE-CORRESPONDING WS_ZSDT0260 TO WG_SIGAM.
    APPEND WG_SIGAM TO TG_SIGAM.
  ENDLOOP.

  IF TG_SIGAM IS NOT INITIAL.
    PERFORM ZF_EXIBE_DADOS_SIGAM.
    LEAVE TO SCREEN 0.

  ELSE.

    " Nenhum ID Compra foi localizado para os dados informados.
    MESSAGE TEXT-E73 TYPE 'I' DISPLAY LIKE 'E'.

  ENDIF.

*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_DADOS_SIGAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_EXIBE_DADOS_SIGAM .

  DATA: WL_LAYOUT TYPE  SLIS_LAYOUT_ALV.

  PERFORM ZF_DEFINIR_EVENTOS_0110.
  PERFORM ZF_MONTA_LAYOUT_COMPRA_SIGAM.

  V_REPORT = SY-REPID.

  WL_LAYOUT-ZEBRA = C_X.
  WL_LAYOUT-BOX_FIELDNAME   = 'MARK'.
  WL_LAYOUT-BOX_TABNAME     = 'TG_SIGAM'.
  WL_LAYOUT-WINDOW_TITLEBAR = 'Compra SIGAM'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      I_CALLBACK_USER_COMMAND = 'ZF_XUSER_COMMAND_0110'
      IS_LAYOUT               = WL_LAYOUT
      IT_FIELDCAT             = ESTRUTURA[]
*     IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      I_SCREEN_START_COLUMN   = 4
      I_SCREEN_START_LINE     = 9
      I_SCREEN_END_COLUMN     = 100
      I_SCREEN_END_LINE       = 15
    TABLES
      T_OUTTAB                = TG_SIGAM.

ENDFORM.                    " ZF_EXIBE_DADOS_SIGAM


*&---------------------------------------------------------------------*
*&      Form  ZF_DEFINIR_EVENTOS_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_DEFINIR_EVENTOS_0110.

  PERFORM F_CARREGAR_EVENTOS USING:
                             SLIS_EV_USER_COMMAND 'ZF_XUSER_COMMAND_0110'.

ENDFORM.                    " ZF_DEFINIR_EVENTOS_0110
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_LAYOUT_COMPRA_SIGAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_MONTA_LAYOUT_COMPRA_SIGAM.
  REFRESH: ESTRUTURA.
  PERFORM MONTAR_ESTRUTURA_MATNR USING:
     1  'ZSDT0260'  'ID_COMPRA'          'TG_SIGAM'  'ID_COMPRA'          'ID Compra'        ' ',
     2  'ZSDT0260'  'COMPRA_FIM_EXPORT'  'TG_SIGAM'  'COMPRA_FIM_EXPORT'  'Fins Exportação'  ' ',
     3  'ZSDT0260'  'SAFRA'              'TG_SIGAM'  'SAFRA'              'Safra'            ' ',
     4  'ZSDT0260'  'STATUS'             'TG_SIGAM'  'STATUS'             'Status'           ' ',
     5  'ZSDT0260'  'ID_MATERIAL_SAP'    'TG_SIGAM'  'ID_MATERIAL_SAP'    'Produto'          ' ',
     6  'ZSDT0260'  'ID_FILIAL_SAP'      'TG_SIGAM'  'ID_FILIAL_SAP'      'Filial'           ' ',
     7  'ZSDT0260'  'DATA_COMPRA'        'TG_SIGAM'  'DATA_COMPRA'        'Data Compra'      ' ',
     8  'ZSDT0260'  'ID_FORNEC_SAP'      'TG_SIGAM'  'ID_FORNEC_SAP'      'Cod. Fornecedor'  ' ',
     9  'ZSDT0260'  'ID_CLIENTE_SAP'     'TG_SIGAM'  'ID_CLIENTE_SAP'     'Cod. Cliente'     ' '.

ENDFORM.                    " ZF_MONTA_LAYOUT_COMPRA_SIGAM

*---------------------------------------------------------------------*
*       FORM ZF_XUSER_COMMAND_0110                                    *
*---------------------------------------------------------------------*
* Trata Tela 0110 - Dados Compra SIGAM                                *
*---------------------------------------------------------------------*
FORM ZF_XUSER_COMMAND_0110 USING UCOMM    LIKE SY-UCOMM
                                 SELFIELD TYPE KKBLO_SELFIELD.. "#EC CALLED

  DATA: TL_SIGAM_AUX TYPE TABLE OF TY_SIGAM,
        WL_SIGAM     TYPE TY_SIGAM,
        WL_LINES     TYPE SY-TABIX.

  REFRESH: TL_SIGAM_AUX.

  CASE SY-UCOMM.

    WHEN '&ONT'.
      TL_SIGAM_AUX[] = TG_SIGAM[].
      DELETE TL_SIGAM_AUX WHERE MARK IS INITIAL.
      DESCRIBE TABLE TL_SIGAM_AUX LINES WL_LINES.
      IF WL_LINES EQ 0.
        " Selecionar uma linha.
        MESSAGE TEXT-E85  TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
      IF WL_LINES GT 1.
        " Selecionar apenas uma linha.
        MESSAGE TEXT-E86 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ELSE.
        READ TABLE TL_SIGAM_AUX INTO  WL_SIGAM INDEX 1.
        MOVE-CORRESPONDING WL_SIGAM TO WG_0260.    "Tabela Dados Compra SIGAM
        PERFORM ZF_VALIDA_COMPRA_SIGAM USING ' '
                                       CHANGING  TG_MSG_RET-MSG.

        PERFORM ZF_DETERMINE_FUNRURAL USING ABAP_TRUE. "Forçar determinar Funrural

      ENDIF.

      CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE SCREEN.

  ENDCASE.

ENDFORM. "ZF_XUSER_COMMAND_0110

*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_COMPRA_SIGAM
*&---------------------------------------------------------------------*
*  Se p_log = ' ', mensagem na própria tela (Compra Sigam)
*  Se p_log = 'X', mensagem retorno no LOG de ERROS (FORM Verifica_erros)
*                  Colocar validações no LOG de ERROS, para garantir que
*                  após o usuário escolher o ID_COMPRA_SIGAM,
*                  caso ele altere as informações da
*                  "Cultura Pagamento" / "Safra Pagamento" "Escritório de Vendas",
*                  o sistema valide e não deixe Salvar, se houver inconsistência.
*                  Observado que o problema ocorrerá quando a venda
*                  for TS (Troca SAFRA)
*----------------------------------------------------------------------*
FORM ZF_VALIDA_COMPRA_SIGAM USING    P_LOG
                            CHANGING P_MSG TYPE CHAR255.

  DATA:  LV_MATKL  TYPE MARA-MATKL.

  WG_0260-DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT  = WG_0260-ID_MATERIAL_SAP
    IMPORTING
      OUTPUT = WG_0260-ID_MATERIAL_SAP.

  SELECT SINGLE MATKL
    INTO LV_MATKL
     FROM MARA
    WHERE MATNR = WG_0260-ID_MATERIAL_SAP.

  CLEAR P_MSG.

  IF WG_0260-STATUS NE 'EF'.
    " Compra deve estar com Status "Efetivado".
    P_MSG = TEXT-E74.
    IF P_LOG IS INITIAL.
      CLEAR WG_0260.
      MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
  IF WG_HEADER-TPSIM EQ 'TS'                                "FF #174195
* "// Retornar TS TV WBARBOSA para Time Credito 07/07/2025
  OR WG_HEADER-TPSIM EQ 'TT'.                               "FF #174195
    IF WG_HEADER-CULTURA = 'SJ' AND LV_MATKL NE '700110'.
      " Material da Compra deve ser compatível com a Cultura do Simulador "Soja"
      CONCATENATE TEXT-E75 TEXT-E76 INTO P_MSG.
      IF P_LOG IS INITIAL.
        CLEAR WG_0260.
        MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF WG_HEADER-CULTURA = 'ML' AND LV_MATKL NE '700170'.
      " Material da Compra deve ser compatível com a Cultura do Simulador "Milho"
      CONCATENATE TEXT-E75 TEXT-E77 INTO P_MSG.
      IF P_LOG IS INITIAL.
        CLEAR WG_0260.
        MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF WG_HEADER-CULTURA = 'AL' AND LV_MATKL NE '700140'.
      " Material da Compra deve ser compatível com a Cultura do Simulador "Algodão"
      CONCATENATE TEXT-E75 TEXT-E78 INTO P_MSG.
      IF P_LOG IS INITIAL.
        CLEAR WG_0260.
        MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF WG_0260-SAFRA NE WG_HEADER-SAFRA.
      " Safra da Compra deve ser igual a Safra do Simulador.
      P_MSG = TEXT-E79.
      IF P_LOG IS INITIAL.
        CLEAR WG_0260.
        MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

    IF WG_0260-ID_FILIAL_SAP NE WG_HEADER-VKBUR.
      " Filial da Compra deve ser igual ao Escritório de Venda do Simulador.
      P_MSG = TEXT-E80.
      IF P_LOG IS INITIAL.
        CLEAR WG_0260.
        MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ENDIF.
    ENDIF.

  ENDIF.

  IF WG_0260-ID_CLIENTE_SAP IS INITIAL.
    CONCATENATE 'Compra Sigam Id:' WG_0260-ID_COMPRA 'sem Cliente SAP!' INTO P_MSG SEPARATED BY SPACE.
    IF P_LOG IS INITIAL.
      CLEAR WG_0260.
      MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  IF WG_HEADER-KUNNR IS INITIAL.
    P_MSG = TEXT-E01.

    IF P_LOG IS INITIAL.
      CLEAR WG_0260.
      MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM KNA1 INTO @DATA(LWA_KNA1_COMPRA)
   WHERE KUNNR EQ @WG_0260-ID_CLIENTE_SAP.

  IF SY-SUBRC NE 0.
    CONCATENATE 'Cliente com Id:' WG_0260-ID_CLIENTE_SAP 'não encontrado!' INTO P_MSG SEPARATED BY SPACE.
    IF P_LOG IS INITIAL.
      CLEAR WG_0260.
      MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM KNA1 INTO @DATA(LWA_KNA1_SIM)
   WHERE KUNNR EQ @WG_HEADER-KUNNR.

  IF SY-SUBRC NE 0.
    CONCATENATE 'Cliente com Id:' WG_HEADER-KUNNR 'não encontrado!' INTO P_MSG SEPARATED BY SPACE.
    IF P_LOG IS INITIAL.
      CLEAR WG_0260.
      MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

  IF NOT ( ( LWA_KNA1_COMPRA-STCD1 EQ LWA_KNA1_SIM-STCD1 ) AND
           ( LWA_KNA1_COMPRA-STCD2 EQ LWA_KNA1_SIM-STCD2 ) ).

    CONCATENATE 'Cliente Compra Id:' LWA_KNA1_COMPRA-KUNNR 'não corresponde ao Emissor Ordem: Id:' LWA_KNA1_SIM-KUNNR
           INTO P_MSG SEPARATED BY SPACE.

    IF P_LOG IS INITIAL.
      CLEAR WG_0260.
      MESSAGE P_MSG TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form ZF_DETERMINE_FUNRURAL
*&---------------------------------------------------------------------*
*   Determinar Funrural
*----------------------------------------------------------------------*
FORM ZF_DETERMINE_FUNRURAL USING P_FORCE TYPE C.

  REFRESH:  TG_LFA1,
            TG_FUNEX.

  DATA(LVA_FUNRURAL) = ABAP_FALSE.

  IF ( P_FORCE EQ ABAP_FALSE ).

    IF WG_SAVE NE C_ALTCPG.
      CHECK ( WG_ACAO  EQ C_ADD   OR SY-UCOMM EQ C_ADD ) OR
            ( WG_ACAO  EQ C_MODIF OR SY-UCOMM EQ C_ADD ).

      CHECK ( WG_HEADER-KUNNR <> WG_HEADER_OLD-KUNNR ).
    ENDIF.
  ENDIF.

  IF ( WG_0260-ID_COMPRA IS NOT INITIAL ) AND ( WG_0260-COMPRA_FIM_EXPORT = 'S' ).
    LVA_FUNRURAL = ABAP_TRUE.
  ELSE.

    IF WG_CNPJ IS NOT INITIAL.
      SELECT LIFNR SPERR SPERM SPERQ
        INTO TABLE TG_LFA1
        FROM LFA1
        WHERE STCD1 EQ WG_CNPJ.
    ENDIF.

    IF WG_CPF IS NOT INITIAL.
      SELECT LIFNR SPERR SPERM SPERQ
        INTO TABLE TG_LFA1
        FROM LFA1
        WHERE STCD2 EQ WG_CPF.
    ENDIF.

    "Elimina bloqueados
    DELETE TG_LFA1 WHERE SPERR IS NOT INITIAL.
    DELETE TG_LFA1 WHERE SPERM IS NOT INITIAL.
    DELETE TG_LFA1 WHERE SPERQ IS NOT INITIAL.

    IF TG_LFA1[] IS NOT INITIAL.
      SELECT * FROM ZSDT0001FUNEX
        INTO TABLE TG_FUNEX
        FOR ALL ENTRIES IN TG_LFA1
      WHERE LIFNR     EQ TG_LFA1-LIFNR
        AND DT_INICIO LE WG_HEADER-ERDAT
        AND DT_FINAL  GE WG_HEADER-ERDAT.

      IF SY-SUBRC IS INITIAL.
        LVA_FUNRURAL = ABAP_TRUE.
      ENDIF.

    ENDIF.

  ENDIF.

  WG_HEADER-FUNRURAL = LVA_FUNRURAL.

  CLEAR: WG_HEADER-FUNUSER,
         WG_HEADER-FUNDATA,
         WG_HEADER-FUNHORA.

  WG_HEADER_OLD-KUNNR = WG_HEADER-KUNNR.

ENDFORM.

* Fim - Sara Oikawa - 19.10.2020   Melhorias Pacote 5/6

* Início - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*&---------------------------------------------------------------------*
*&      Form  ZF_ALTERA_PAGTO_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALTERA_PAGTO_OV .

  DATA:
    TL_ITENS_AUX      TYPE TABLE OF TY_ITENS,
    WL_ITENS_AUX      TYPE TY_ITENS,
    TL_0090           TYPE TABLE OF ZSDT0090,
    WL_0090           TYPE ZSDT0090,
    WL_ORDERHEADERIN  TYPE BAPISDH1,
    WL_ORDERHEADERINX TYPE BAPISDH1X,
    TL_RETURN         TYPE TABLE OF BAPIRET2 WITH HEADER LINE,

    LV_SEQ            TYPE ZSDT0090-SEQUENCIA,
    LV_ZTERM_ANT      TYPE VBKD-ZTERM,
    LV_ZTERM          TYPE VBKD-ZTERM.

  DATA: WL_0040 TYPE ZSDT0040.

*  CLEAR _OK.

  TL_ITENS_AUX[] = TG_ITENS[].
  DELETE TL_ITENS_AUX WHERE VBELN IS INITIAL.
  SORT TL_ITENS_AUX BY VBELN.
  DELETE ADJACENT DUPLICATES FROM TL_ITENS_AUX COMPARING VBELN.

  CLEAR LV_SEQ.
  SELECT COUNT(*)
    FROM ZSDT0090
    INTO LV_SEQ
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  CLEAR LV_ZTERM.
  IF WG_HEADER-TPSIM EQ 'TS'.
    LV_ZTERM = 'I001'.
  ELSEIF WG_HEADER-TPSIM EQ 'AD'.
    LV_ZTERM = 'I002'.
  ELSEIF WG_HEADER-TPSIM EQ 'TV'.
    LV_ZTERM = 'I004'.
  ELSEIF WG_HEADER-TPSIM EQ 'TT'. "Troca "FF #174195
    LV_ZTERM = 'I008'.
  ENDIF.

  LOOP AT TL_ITENS_AUX INTO WL_ITENS_AUX.

    SELECT SINGLE ZTERM INTO LV_ZTERM_ANT
      FROM VBKD
     WHERE VBELN EQ WL_ITENS_AUX-VBELN.

    WL_ORDERHEADERINX-UPDATEFLAG = 'U'.

    WL_ORDERHEADERIN-PMNTTRMS    = LV_ZTERM.
    WL_ORDERHEADERINX-PMNTTRMS   = ABAP_TRUE.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        SALESDOCUMENT    = WL_ITENS_AUX-VBELN
        ORDER_HEADER_IN  = WL_ORDERHEADERIN
        ORDER_HEADER_INX = WL_ORDERHEADERINX
      TABLES
        RETURN           = TL_RETURN.

    READ TABLE TL_RETURN WITH KEY TYPE = 'E'.
    IF NOT SY-SUBRC IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.
    ENDIF.

    " Inserir um único Registro (não será por item de OV) na tabela ZSDT0090,
    " com ZSDT0090-CATEGORIA = (T - Alter. Cond. Pagamento), para identificar
    " todas as ordens do simulador que sofreram alteração da condição de pagamento.

    CLEAR WL_0090.

    ADD 1 TO LV_SEQ.

    WL_0090-DOC_SIMULACAO  = WG_HEADER-DOC_SIMULACAO.
    WL_0090-SEQUENCIA      = LV_SEQ.
    WL_0090-VBELV          = WL_ITENS_AUX-VBELN.
*   WL_0090-WERKSV
*   WL_0090-KUNNRV
    WL_0090-ZTERM          = LV_ZTERM.
    WL_0090-ZTERMV         = LV_ZTERM_ANT.
    WL_0090-CATEGORIA      = 'T'.
    WL_0090-USNAM          = SY-UNAME.
    WL_0090-DATA_ATUAL     = SY-DATUM.
    WL_0090-HORA_ATUAL     = SY-UZEIT.

    INSERT INTO ZSDT0090  VALUES WL_0090.
    COMMIT WORK.

  ENDLOOP.

ENDFORM.
* Fim - Sara Oikawa - 19.11.2020  Melhorias Pacote 8
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATE_RETROATIVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CHECK_DATE_RETROATIVA .

  IF WG_STATUS NE ICON_RELEASE.
    CASE WG_HEADER-TPSIM.
      WHEN 'AD'.
        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF <
SY-DATUM.
          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
**        IF wg_header-dtpgtcult < sy-datum.
**          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
**        ENDIF.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

        IF WG_HEADER-DTVENCOV < SY-DATUM.
          APPEND VALUE #( MSG = 'Data de vencimento não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

      WHEN 'BN'.
        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF <
SY-DATUM.
          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.


      WHEN 'PM'.
        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF <
SY-DATUM.
          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        IF WG_HEADER-DTPGTCULT < SY-DATUM.
          APPEND VALUE #( MSG = 'Data de pagamento não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        "FF #169501 - inicio
      WHEN 'TT'.


        IF ( WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM )
        OR ( WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM )
        OR ( WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF < SY-DATUM ).

          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.

        ENDIF.




        IF WG_HEADER-DTPGTCULT < SY-DATUM.
          APPEND VALUE #( MSG = 'Data de pagamento não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        IF WG_HEADER-DTENT IS INITIAL.
          APPEND VALUE #( MSG = 'Data de entrega não pode ficar em branco' ) TO TG_MSG_RET.
        ENDIF.


*      WHEN 'TS'.
*        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
*sy-datum.
*          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
*        ENDIF.
*
*        IF wg_header-dtpgtcult < sy-datum.
*          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
*        ENDIF.
*
*        IF wg_header-dtent < sy-datum.
*          APPEND VALUE #( msg = 'Data de vencimento não pode ser menor que a data atual' ) TO tg_msg_ret.
*        ENDIF.
*
*      WHEN 'TV'.
*        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < sy-datum OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < sy-datum OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def <
*sy-datum.
*          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
*        ENDIF.
*
*        IF wg_header-dtpgtcult < sy-datum.
*          APPEND VALUE #( msg = 'Data de pagamento não pode ser menor que a data atual' ) TO tg_msg_ret.
*        ENDIF.
*
*        IF wg_header-dtent < sy-datum.
*          APPEND VALUE #( msg = 'Data de Data de entrega não pode ser menor que a data atual' ) TO tg_msg_ret.
*        ENDIF.
        "FF #169501 - fim


      WHEN 'VF'.
        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF <
SY-DATUM.
          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

      WHEN 'VP'.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
        WG_HEADER-DTINIJUROS = WG_HEADER-DTPGTCULT.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF <
SY-DATUM.
          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        IF WG_HEADER-DTPGTCULT < SY-DATUM.
          APPEND VALUE #( MSG = 'Data de pagamento não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        IF WG_HEADER-DTINIJUROS < SY-DATUM.
          APPEND VALUE #( MSG = 'Data de pagamento não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
*        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET > WG_HEADER-DTPGTCULT
*          OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM > WG_HEADER-DTPGTCULT
*          OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF > WG_HEADER-DTPGTCULT.
*          APPEND VALUE #( MSG = 'Data de entrega não pode ser maior que a data de vencimento' ) TO TG_MSG_RET.
*        ENDIF.
* CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

      WHEN 'VV'.
        IF WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < SY-DATUM OR WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < SY-DATUM OR WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF <
SY-DATUM.
          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        IF WG_HEADER-DTVENCOV < SY-DATUM.
          APPEND VALUE #( MSG = 'Data de vencimento não pode ser menor que a data atual' ) TO TG_MSG_RET.
        ENDIF.

        "FF #169501 - inicio
** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - inicio
*        IF wg_header-dt_entrega_fet IS NOT INITIAL AND wg_header-dt_entrega_fet < wg_header-dtpgtcult
*          OR wg_header-dt_entrega_sem IS NOT INITIAL AND wg_header-dt_entrega_sem < wg_header-dtpgtcult
*          OR wg_header-dt_entrega_def IS NOT INITIAL AND wg_header-dt_entrega_def < wg_header-dtpgtcult.
*          APPEND VALUE #( msg = 'Data de entrega não pode ser menor que a data de vencimento' ) TO tg_msg_ret.
*        ENDIF.
** CS2025000249 - Projetos Insumos 25 - PANF  28-04-25 - Fim

        " Validar data de entrega (sementes, fertilizantes, defensivos) contra a data de vencimento da OV
        IF WG_HEADER-DTVENCOV IS NOT INITIAL AND
          (
            ( WG_HEADER-DT_ENTREGA_FET IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_FET < WG_HEADER-DTVENCOV ) OR
            ( WG_HEADER-DT_ENTREGA_SEM IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_SEM < WG_HEADER-DTVENCOV ) OR
            ( WG_HEADER-DT_ENTREGA_DEF IS NOT INITIAL AND WG_HEADER-DT_ENTREGA_DEF < WG_HEADER-DTVENCOV )
          ).

          APPEND VALUE #( MSG = 'Data de entrega não pode ser menor que a data de vencimento da OV' ) TO TG_MSG_RET.
        ENDIF.
        "FF #169501 - fim

      WHEN OTHERS.
    ENDCASE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_BOLETA
*&---------------------------------------------------------------------*
FORM F_POPUP_BOLETA CHANGING P_MOTIVO TYPE ZSDED033
                             P_CANC TYPE C.

  DATA LS_REQUEST TYPE ZSDE0026.
  DATA LT_BOLETAS TYPE ZSDC013.

  DATA LS_BOLETA TYPE ZSDE013.
  DATA LS_ZSDT0308 TYPE ZSDT0308.

  "DATA lv_erro TYPE c.
  DATA LV_MOTIVO TYPE STRING.

  IF WG_HEADER-TPSIM = 'VV'.
    LS_REQUEST-BLDAT = WG_HEADER-DTVENCOV.
  ELSE.
    LS_REQUEST-BLDAT = WG_HEADER-DTPGTCULT.
  ENDIF.

  DATA(LV_DAT_ANT) = LS_REQUEST-BLDAT.
  DATA(LV_DAT_POS) = LS_REQUEST-BLDAT.

  SUBTRACT 5 FROM LV_DAT_ANT.
  ADD 5 TO LV_DAT_POS.

  LS_REQUEST-BUKRS = WG_HEADER-VKORG.

  CLEAR GV_TAXA_NEG.

  CALL FUNCTION 'ZSDMF001_CONSULTA_BOLETA_API'
    EXPORTING
      IV_DOC_SIMU = WG_HEADER-DOC_SIMULACAO
      IS_REQUEST  = LS_REQUEST
    IMPORTING
      ET_BOLETAS  = LT_BOLETAS.

  " Mostrar somente boletas que possuem saldo disponível maior ou igual ao 'valor total-VLRTOT' do simulador.
  DELETE LT_BOLETAS WHERE SALDO < WG_HEADER-VLRTOT. "#debug ramon

*  " Mostrar somente boletas com um range de datas 5 dias anterior e 5 dia posterior a 'data de vencimento-DTPGTCULT' do simulador.
  "DELETE lt_boletas WHERE data_venc < lv_dat_ant.
  "DELETE lt_boletas WHERE data_venc > lv_dat_pos.

  CALL FUNCTION 'ZSDMF001_POPUP_BOLETA'
    EXPORTING
      IV_EDIT   = 'X'
      IV_VENC   = WG_HEADER-DTPGTCULT
    IMPORTING
      ES_LINE   = LS_BOLETA
      EV_MOTIVO = LV_MOTIVO
      EV_CANCEL = P_CANC
    TABLES
      CT_BOLETA = LT_BOLETAS.

  "break rblima.

  CHECK P_CANC IS INITIAL AND LS_BOLETA IS NOT INITIAL.

  P_MOTIVO = LV_MOTIVO.

  MOVE-CORRESPONDING LS_BOLETA TO LS_ZSDT0308.

  LS_ZSDT0308-VLR_BRL_ENVIADO = WG_HEADER-VLRTOT.


  CALL FUNCTION 'ZSDMF001_REVERTE_BOLETA_API'
    IMPORTING
      EV_ERRO           = P_CANC
    CHANGING
      CS_REQUEST        = LS_ZSDT0308
    EXCEPTIONS
      FALHA_COMUNICACAO = 1
      OTHERS            = 2.

  IF SY-SUBRC <> 0.
    PERFORM F_MENSAGEM_SISTEMA.
  ENDIF.

  CHECK P_CANC IS INITIAL.

  MODIFY ZSDT0308 FROM LS_ZSDT0308.

  COMMIT WORK AND WAIT.

  GV_TAXA_NEG = LS_ZSDT0308-TAXA_NEG.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_BOLETA_REPORT
*&---------------------------------------------------------------------*
FORM F_CALL_BOLETA_REPORT.

  DATA LR_SIMU TYPE RANGE OF ZSDED003.

  CHECK WG_HEADER-DOC_SIMULACAO IS NOT INITIAL.

  APPEND 'IEQ' && WG_HEADER-DOC_SIMULACAO TO LR_SIMU.

  SUBMIT ZSDR0146 WITH SO_SIMU IN LR_SIMU AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM F_MENSAGEM_SISTEMA .

  MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_ADD
*&---------------------------------------------------------------------*
FORM F_INIT_ADD .

  DATA LV_ANSWER.
  DATA LV_ECOMM.

  PERFORM F_CHECK_USER_ECOMM CHANGING LV_ECOMM.

  CHECK LV_ECOMM = 'X'.

  CALL FUNCTION 'ZSDMF_POPUP_RADIO_OPTIONS'
    EXPORTING
      IV_QUESTION = 'Qual a Origem da Venda?'
      IV_TEXT01   = 'Convencional'
      IV_TEXT02   = 'E-commerce'
    IMPORTING
      EV_ANSWER   = LV_ANSWER.

  IF LV_ANSWER = '0' OR LV_ANSWER = '1'.
    EXIT.
  ENDIF.

  WG_HEADER-ECOMMERCE = 'X'.

  WG_HEADER-VENDEDOR = 'E00'. " (E-commerce)
  "wg_header-tpsim = 'VV'. "(Venda a Vista) "FF #174195
  "wg_header-waerk = 'BRL'. "FF #174195
  WG_HEADER-HBKID = 'AL5'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_USER_ECOMM
*&---------------------------------------------------------------------*
FORM F_CHECK_USER_ECOMM CHANGING P_ECOMM TYPE C.

  DATA LT_USERS_ECOMM TYPE TABLE OF RGSB4.

  CLEAR P_ECOMM.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_ZSDT0044_08'
    TABLES
      SET_VALUES    = LT_USERS_ECOMM
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  READ TABLE LT_USERS_ECOMM ASSIGNING FIELD-SYMBOL(<FS_LINE>)
    WITH KEY FROM = SY-UNAME.

  CHECK SY-SUBRC EQ 0.

  P_ECOMM = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_108 OUTPUT.

  LOOP AT SCREEN.

    IF WG_HEADER-ECOMMERCE = 'X'.

      SCREEN-INPUT = 1.
    ELSE.
      SCREEN-INPUT = 0.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form atualiza_fundeinfra
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ATUALIZA_FUNDEINFRA .
  IF WG_HEADER-FUNDEINFRA_EXCE IS INITIAL.
    WG_HEADER-FUNDEINFRA_EXCE = ABAP_TRUE.
  ELSE.
    WG_HEADER-FUNDEINFRA_EXCE = ABAP_FALSE.
  ENDIF.

  UPDATE ZSDT0040 SET FUNDEINFRA_EXCE = WG_HEADER-FUNDEINFRA_EXCE
   WHERE DOC_SIMULACAO EQ WG_HEADER-DOC_SIMULACAO.

  PERFORM LOG_ACAO  USING 'F' '' CHANGING LV_ERRO.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_valida_intinerario
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_VALIDA_INTINERARIO .

  DATA: LV_KUNNR TYPE KNA1-KUNNR.
  DATA: WL_LINHA(6).              "25/07/25 - PQ - Projeto Renova Insumos - sem US

  IF WG_HEADER-KUNNR IS NOT INITIAL.

    SELECT SINGLE LZONE
      FROM LFA1
      WHERE LIFNR = @WG_HEADER-KUNNR
      INTO @DATA(LV_AZONE).

    LOOP AT TG_ITENS ASSIGNING FIELD-SYMBOL(<FS_ITENS>).
      WL_LINHA = SY-TABIX. "25/07/25 - PQ - Projeto Renova Insumos - sem US

      CHECK <FS_ITENS>-WERKS IS NOT INITIAL.

      LV_KUNNR = |{ <FS_ITENS>-WERKS ALPHA = IN }|.
*"// WBARBOSA 27/05/25
      ZCL_MANUTENCAO_INSUMOS=>VERIFICAR_ITINERARIO(
        EXPORTING
          I_PC  = LV_KUNNR
          I_LR  = WG_HEADER-KUNNR
        IMPORTING
          IS_OK = DATA(IS_OK)
      ).

*      SELECT SINGLE LZONE
*      FROM KNA1
*      WHERE KUNNR = @LV_KUNNR
*      INTO @DATA(LV_LZONE).
*
*      IF SY-SUBRC = 0.
*
*        SELECT COUNT(*)
*          FROM TROLZ
*          WHERE AZONE = @LV_AZONE
*          AND LZONE = @LV_LZONE.
*        IF SY-SUBRC <> 0.
      IF IS_OK IS INITIAL.
** 25/07/25 - Projeto Renova Insumos... sem US apenas melhoria na mensagem exibida ao usuário.
        MOVE: 'VLR_FRETE'  TO TG_MSG_RET-FIELD,
              'GRID1'      TO TG_MSG_RET-OBJ,
              WL_LINHA     TO TG_MSG_RET-TABIX.

        CONCATENATE 'Itinerário não localizado, saindo do centro fornecedor da LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
*        APPEND VALUE #( MSG = 'Itinerário não localizado para Emissor/centro' ) TO TG_MSG_RET.
** 25/07/25 - Projeto Renova Insumos...
      ENDIF.
*        ENDIF.
*    ENDIF.
      "// WBARBOSA 27/05/25
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_valida_sisdev
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_VALIDA_SISDEV .

  SELECT COUNT(*)
      FROM ZSDT0205
      WHERE CPFCNPJ = WG_CNPJ.

  IF SY-SUBRC = 0.

    APPEND VALUE #( MSG = 'Produtor não tem o cadastro no SISDEV' ) TO TG_MSG_RET.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  SEARCH_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_BANCO INPUT.

  DATA: BEGIN OF TL_BANCO OCCURS 0,
          HBKID     TYPE T012T-HBKID,
          DESCRICAO TYPE T012T-TEXT1,
        END OF TL_BANCO.

  REFRESH: TL_BANCO, TL_RETURN_TAB, TL_DSELC.
  CLEAR: TL_BANCO, TL_RETURN_TAB, TL_DSELC.

  SELECT B~HBKID, B~TEXT1
    FROM ZSDT0383 AS A
    INNER JOIN T012T AS B ON
    A~BUKRS = B~BUKRS
    AND A~HBKID = B~HBKID
    AND B~SPRAS = @SY-LANGU
    INTO TABLE @TL_BANCO
    WHERE A~BUKRS = @WA_T001K-BUKRS
       AND A~MEIO_PGTO = @WG_HEADER-MEIO_PAGO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'HBKID'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_HEADER-HBKID'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_BANCO
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_cria_intinerario
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CRIA_INTINERARIO .

  DATA: LV_KUNNR TYPE KNA1-KUNNR.

  IF TG_ITENS IS INITIAL.
    APPEND VALUE #( MSG = 'Itinerário não localizado para Emissor/centro' ) TO TG_MSG_RET.
  ENDIF.

  IF WG_HEADER-KUNNR IS NOT INITIAL.

    SELECT SINGLE LZONE
      FROM LFA1
      WHERE LIFNR = @WG_HEADER-KUNNR
      INTO @DATA(LV_AZONE).

    LOOP AT TG_ITENS ASSIGNING FIELD-SYMBOL(<FS_ITENS>).

      CHECK <FS_ITENS>-WERKS IS NOT INITIAL.

      LV_KUNNR = |{ <FS_ITENS>-WERKS ALPHA = IN }|.

* "// WBARBOSA 27/05/25
      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>CALL_CREATE_ITINERARIO
        EXPORTING
          I_PC = LV_KUNNR
          I_LR = WG_HEADER-KUNNR.
* "// WBARBOSA 27/05/25

      "FF #174195 - inicio // Itinerário foi criado, não exibir a msg.
      DELETE TG_MSG_RET WHERE MSG = 'Itinerário não localizado para Emissor/centro'.
      "FF #174195 - fim

*      SELECT SINGLE LZONE
*      FROM KNA1
*      WHERE KUNNR = @LV_KUNNR
*      INTO @DATA(LV_LZONE).
*
*      IF SY-SUBRC = 0.
*
*        SELECT COUNT(*)
*          FROM TROLZ
*          WHERE AZONE = @LV_AZONE
*          AND LZONE = @LV_LZONE.
*        IF SY-SUBRC <> 0.
*
*          SET PARAMETER ID 'ZONA_ORI' FIELD LV_AZONE.
*          SET PARAMETER ID 'ZONA_DES' FIELD LV_LZONE.
*          SET PARAMETER ID 'COD_CLI'  FIELD WG_HEADER-KUNNR.
*          SET PARAMETER ID 'COD_PC'   FIELD <FS_ITENS>-WERKS.
*
*          SUBMIT ZLESR0162 AND RETURN WITH P_KUNNR = <FS_ITENS>-WERKS
*                                 WITH P_LIFNR = WG_HEADER-KUNNR
*                                 WITH P_LZONE = LV_LZONE.
*        ELSE.
*          MESSAGE 'Intinerário já criado' TYPE 'S' DISPLAY LIKE 'I'.
*        ENDIF.

*    ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
