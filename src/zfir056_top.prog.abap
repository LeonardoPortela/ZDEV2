*&---------------------------------------------------------------------*
*&  Include           ZFIR056_TOPO
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPE-POOLS: SLIS.

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
         "Dados Terreno
         EMPRESA          TYPE ZFIT0099-EMPRESA,
         LOTEAMENTO       TYPE ZFIT0099-LOTEAMENTO,
         NOME_LOTEAMENTO  TYPE ZFIT0098-NOME_LOTEAMENTO,
         NRO_QUADRA       TYPE ZFIT0099-NRO_QUADRA,
         NRO_TERRENO      TYPE ZFIT0099-NRO_TERRENO,
         MATRICULA        TYPE ZFIT0099-MATRICULA,
         ENDERECO         TYPE ZFIT0099-ENDERECO,
         NUMERO           TYPE ZFIT0099-NUMERO,
         BAIRRO_LOCALIZ   TYPE ZFIT0099-BAIRRO_LOCALIZ,
         CEP              TYPE ZFIT0099-CEP,
         CIDADE           TYPE ZFIT0099-CIDADE,
         UF               TYPE ZFIT0099-UF,
         VALOR            TYPE ZFIT0099-VALOR,
         VALOR_AGREGADO   TYPE ZFIT0099-VALOR_AGREGADO,
         VALOR_TOTAL      TYPE ZFIT0099-VALOR_TOTAL,
         MET_LADO_DIR     TYPE ZFIT0099-MET_LADO_DIR,
         MET_LADO_ESQ     TYPE ZFIT0099-MET_LADO_ESQ,
         MET_FUNDOS       TYPE ZFIT0099-MET_FUNDOS,
         MET_FRENTE       TYPE ZFIT0099-MET_FRENTE,
         MET_TOT_M2       TYPE ZFIT0099-MET_TOT_M2,
         "Dados Venda
         CLIENTE          TYPE ZFIT0100-CLIENTE,
         DESC_CLIENTE     TYPE KNA1-NAME1,
         COND_PGTO        TYPE ZFIT0104-COND_PGTO,
         FORMA_PGTO       TYPE ZFIT0104-FORMA_PGTO,
         QTD_PARCELAS     TYPE ZFIT0104-QTD_PARCELAS,
         DATA_VENC        TYPE ZFIT0104-DATA_VENC,
         "Dados Inclusão
         DATA_REGISTRO    TYPE ZFIT0099-DATA_REGISTRO,
         HORA_REGISTRO    TYPE ZFIT0099-HORA_REGISTRO,
         USUARIO_REGISTRO TYPE ZFIT0099-USUARIO_REGISTRO,
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
      IT_ZFIT0104   TYPE TABLE OF TY_ZFIT0104,
      WA_ZFIT0104   TYPE TY_ZFIT0104,
      IT_KNA1       TYPE TABLE OF KNA1,
      WA_KNA1       TYPE KNA1.

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
