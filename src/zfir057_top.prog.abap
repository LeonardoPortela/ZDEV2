*&---------------------------------------------------------------------*
*&  Include           ZFIR057_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPE-POOLS: SLIS, ICON.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.


TYPES: BEGIN OF TY_ZFIT0104,
          EMPRESA          TYPE ZFIT0104-EMPRESA,
          LOTEAMENTO       TYPE ZFIT0104-LOTEAMENTO,
          NRO_TERRENO      TYPE ZFIT0104-NRO_TERRENO,
          NRO_QUADRA       TYPE ZFIT0104-NRO_QUADRA,
          COD_EV           TYPE ZFIT0104-COD_EV,
          VALOR            TYPE ZFIT0104-VALOR,
          COND_PGTO        TYPE ZFIT0104-COND_PGTO,
          FORMA_PGTO       TYPE ZFIT0104-FORMA_PGTO,
          QTD_PARCELAS     TYPE ZFIT0104-QTD_PARCELAS,
          DATA_VENC        TYPE ZFIT0104-DATA_VENC,
       END OF TY_ZFIT0104.


TYPES: BEGIN OF TY_SAIDA,
         STATUS               TYPE ZFIT0101-STATUS,
         EMPRESA              TYPE ZFIT0099-EMPRESA,
         LOTEAMENTO           TYPE ZFIT0099-LOTEAMENTO,
         NOME_LOTEAMENTO      TYPE ZFIT0098-NOME_LOTEAMENTO,
         NRO_QUADRA           TYPE ZFIT0099-NRO_QUADRA,
         NRO_TERRENO          TYPE ZFIT0099-NRO_TERRENO,
         CLIENTE              TYPE ZFIT0100-CLIENTE,
         DESC_CLIENTE         TYPE KNA1-NAME1,
         NOME_EVENTO          TYPE ZFIT0102-NOME_EVENTO,
         COND_PGTO            TYPE ZFIT0104-COND_PGTO,
         DESC_COND_PGTO(30)   TYPE C,
         FORMA_PGTO           TYPE ZFIT0104-FORMA_PGTO,
         DESC_FORMA_PGTO(30)  TYPE C,
         DATA_VENC            TYPE ZFIT0104-DATA_VENC,
         STV                  TYPE ZFIT0101-STATUS,
         VALOR                TYPE ZFIT0099-VALOR,
         BUDAT                TYPE BSID-BUDAT,
         AUGDT                TYPE BSAD-AUGDT,
         BELNR                TYPE BSID-BELNR,
         AUGBL                TYPE BSAD-AUGBL,
       END OF TY_SAIDA.


*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------

DATA: IT_SAIDA      TYPE TABLE OF TY_SAIDA,
      WA_SAIDA      TYPE TY_SAIDA,
      IT_ZFIT0098   TYPE TABLE OF ZFIT0098,
      WA_ZFIT0098   TYPE ZFIT0098,
      IT_ZFIT0099   TYPE TABLE OF ZFIT0099,
      WA_ZFIT0099   TYPE ZFIT0099,
      IT_ZFIT0100   TYPE TABLE OF ZFIT0100,
      WA_ZFIT0100   TYPE ZFIT0100,
      IT_ZFIT0101   TYPE TABLE OF ZFIT0101,
      WA_ZFIT0101   TYPE ZFIT0101,
      IT_ZFIT0102   TYPE TABLE OF ZFIT0102,
      WA_ZFIT0102   TYPE ZFIT0102,
      IT_ZFIT0104   TYPE TABLE OF ZFIT0104,
      WA_ZFIT0104   TYPE ZFIT0104,
      IT_KNA1       TYPE TABLE OF KNA1,
      WA_KNA1       TYPE KNA1,
      IT_BSID       TYPE TABLE OF BSID,
      WA_BSID       TYPE BSID,
      IT_BSAD       TYPE TABLE OF BSAD,
      WA_BSAD       TYPE BSAD,
      IT_BKPF       TYPE TABLE OF BKPF,
      WA_BKPF       TYPE BKPF.

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
      GS_VARIANT   TYPE DISVARIANT.
