*&---------------------------------------------------------------------*
*&  Include           ZGL033_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_SAIDA,
         SEQ_LCTO        TYPE ZGLT050-SEQ_LCTO,
         BUKRS           TYPE ZGLT050-BUKRS,
         TP_OPR          TYPE ZGLT050-TP_OPR,
         NRO_APOLICE     TYPE ZGLT050-NRO_APOLICE,
         REF_SEQ_LCTO    TYPE ZGLT050-REF_SEQ_LCTO,
         VIG_DE          TYPE ZGLT050-VIG_DE,
         VIG_ATE         TYPE ZGLT050-VIG_ATE,
         SEQ_TIPO        TYPE ZGLT050-SEQ_TIPO,
         DESCR_TIPO      TYPE ZGLT064-DESCR,
         COD_SEGURADORA  TYPE ZGLT050-COD_SEGURADORA,
         NAME1           TYPE LFA1-NAME1,
         WAERS           TYPE ZGLT050-WAERS,
         SEQ_PARC        TYPE ZGLT050-SEQ_PARC,
         NRO_PARC        TYPE ZGLT067-NRO_PARC,
         WERKS           TYPE ZGLT067-WERKS,
         WKURS           TYPE ZGLT067-WKURS,
         VLR_PREMIO_USD  TYPE ZGLT067-VLR_PREMIO_USD,
         VLR_PREMIO_BRL  TYPE ZGLT067-VLR_PREMIO_BRL,
         DT_VENC         TYPE ZGLT067-DT_VENC,
         HBKID           TYPE ZGLT067-HBKID,
         BVTYP           TYPE ZGLT067-BVTYP,
         ZLSPR           TYPE ZGLT067-ZLSPR,
         ZLSCH           TYPE ZGLT067-ZLSCH,
         BANKS           TYPE ZGLT067-BANKS,
         LOTE            TYPE ZGLT067-LOTE,
         DOC_LCTO        TYPE ZGLT067-DOC_LCTO,
         DT_LCTO_CTB     TYPE ZGLT067-DT_LCTO_CTB,
         BELNR           TYPE BSAK-BELNR,
         AUGBL           TYPE BSAK-AUGBL,
         AUGDT           TYPE BSAK-AUGDT,
         ERNAM           TYPE ZGLT067-ERNAM,
         ERDAT           TYPE ZGLT067-ERDAT,
         COD_BARRAS      TYPE ZGLT067-COD_BARRAS,
         SALDO_USD       TYPE ZGLT068-VLR_PREMIO_USD,
         SALDO_BRL       TYPE ZGLT068-VLR_PREMIO_BRL,
         COLOR           TYPE KKBLO_SPECIALCOL OCCURS 0,
       END OF TY_SAIDA.

 TYPES: BEGIN OF TY_067,
          BUKRS   TYPE ZGLT050-BUKRS,
          BELNR   TYPE BKPF-BELNR,
          AUGBL   TYPE BSAK-AUGBL,
          AUGDT   TYPE BSAK-AUGDT,
          OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY.
          INCLUDE STRUCTURE ZGLT067.
 TYPES  END OF TY_067.

*---------------------------------------------------------------------*
*  Internal Tables e Work Areas
*---------------------------------------------------------------------*

DATA: TG_034    TYPE TABLE OF ZGLT034 WITH HEADER LINE,
      TG_050    TYPE TABLE OF ZGLT050 WITH HEADER LINE,
      TG_LFA1   TYPE TABLE OF LFA1    WITH HEADER LINE,
      TG_064    TYPE TABLE OF ZGLT064 WITH HEADER LINE,
      TG_067    TYPE TABLE OF TY_067  WITH HEADER LINE,
      TG_BSAK   TYPE TABLE OF BSAK    WITH HEADER LINE,
      TG_BSAD   TYPE TABLE OF BSAD    WITH HEADER LINE,
      TG_050_BX TYPE TABLE OF ZGLT050 WITH HEADER LINE,
      TG_067_BX TYPE TABLE OF ZGLT067 WITH HEADER LINE,
      IT_SAIDA  TYPE TABLE OF TY_SAIDA,
      WA_SAIDA  TYPE TY_SAIDA.

*---------------------------------------------------------------------*
*  Variáveis
*---------------------------------------------------------------------*

 DATA: VG_NOT_FOUND TYPE C,
       VG_DT_PGTO_INI  TYPE SY-DATUM,
       VG_DT_PGTO_FIM  TYPE SY-DATUM.

*----------------------------------------------------------------------*
* Estruturas ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      GS_VARIANT   TYPE DISVARIANT,
      VARIANTE     LIKE DISVARIANT,
      GS_VARIANT_C TYPE DISVARIANT.
