*&---------------------------------------------------------------------*
*&  Include           ZFIR0062_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPE-POOLS: SLIS.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_SAIDA,
         BUKRS        TYPE ZFIT0080-BUKRS,
         CD_GRUPO     TYPE ZFIT0105-CD_GRUPO,
         DS_GRUPO     TYPE ZFIT0105-DESCRICAO,
         COD_FLX      TYPE ZFIT0077-COD_FLX,
         DESC_FLX     TYPE ZFIT0077-DESC_FLX,
         ENT_SAI      TYPE C,
         RMVCT        TYPE ZFIT0080-RMVCT,
         SLD_INI_FIM  TYPE C,
         DAY_01       TYPE ZFIT0080-DMBTR,
         DAY_02       TYPE ZFIT0080-DMBTR,
         DAY_03       TYPE ZFIT0080-DMBTR,
         DAY_04       TYPE ZFIT0080-DMBTR,
         DAY_05       TYPE ZFIT0080-DMBTR,
         DAY_06       TYPE ZFIT0080-DMBTR,
         DAY_07       TYPE ZFIT0080-DMBTR,
         DAY_08       TYPE ZFIT0080-DMBTR,
         DAY_09       TYPE ZFIT0080-DMBTR,
         DAY_10       TYPE ZFIT0080-DMBTR,
         DAY_11       TYPE ZFIT0080-DMBTR,
         DAY_12       TYPE ZFIT0080-DMBTR,
         DAY_13       TYPE ZFIT0080-DMBTR,
         DAY_14       TYPE ZFIT0080-DMBTR,
         DAY_15       TYPE ZFIT0080-DMBTR,
         DAY_16       TYPE ZFIT0080-DMBTR,
         DAY_17       TYPE ZFIT0080-DMBTR,
         DAY_18       TYPE ZFIT0080-DMBTR,
         DAY_19       TYPE ZFIT0080-DMBTR,
         DAY_20       TYPE ZFIT0080-DMBTR,
         DAY_21       TYPE ZFIT0080-DMBTR,
         DAY_22       TYPE ZFIT0080-DMBTR,
         DAY_23       TYPE ZFIT0080-DMBTR,
         DAY_24       TYPE ZFIT0080-DMBTR,
         DAY_25       TYPE ZFIT0080-DMBTR,
         DAY_26       TYPE ZFIT0080-DMBTR,
         DAY_27       TYPE ZFIT0080-DMBTR,
         DAY_28       TYPE ZFIT0080-DMBTR,
         DAY_29       TYPE ZFIT0080-DMBTR,
         DAY_30       TYPE ZFIT0080-DMBTR,
         DAY_31       TYPE ZFIT0080-DMBTR,
         TOT_MONTH    TYPE ZFIT0080-DMBTR,
         ROWCOLOR(4)  TYPE C,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ZFIT0080,
         BUKRS    TYPE ZFIT0080-BUKRS,
         AUGDT    TYPE ZFIT0080-BUDAT,
         COD_FLX  TYPE ZFIT0080-COD_FLX,
         RMVCT    TYPE ZFIT0080-RMVCT,
         DMBTR    TYPE ZFIT0080-DMBTR,
         DMBE2    TYPE ZFIT0080-DMBE2,
       END OF TY_ZFIT0080.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------

DATA: IT_SAIDA        TYPE TABLE OF TY_SAIDA,
      IT_SAIDA_AUX    TYPE TABLE OF TY_SAIDA,
      WA_SAIDA        TYPE TY_SAIDA,
      IT_ZFIT0105     TYPE TABLE OF ZFIT0105,
      WA_ZFIT0105     TYPE ZFIT0105,
      IT_ZFIT0080     TYPE TABLE OF TY_ZFIT0080,
      WA_ZFIT0080     TYPE TY_ZFIT0080,
      IT_0080_SAI     TYPE TABLE OF TY_ZFIT0080,
      WA_0080_SAI     TYPE TY_ZFIT0080,
      IT_0080_AUX     TYPE TABLE OF TY_ZFIT0080,
      WA_0080_AUX     TYPE TY_ZFIT0080,
      IT_ZFIT0077     TYPE TABLE OF ZFIT0077,
      WA_ZFIT0077     TYPE ZFIT0077,
      IT_0077_AUX     TYPE TABLE OF ZFIT0077,
      WA_0077_AUX     TYPE ZFIT0077,
      WA_T247         TYPE T247.


*-------------------------------------------------------------------
* Variaveis
*-------------------------------------------------------------------
DATA: VG_VALIDA_CAMPOS TYPE C.

*-------------------------------------------------------------------
* Variaveis Colunns
*-------------------------------------------------------------------
DATA: DAY_01_MOV TYPE C,
      DAY_02_MOV TYPE C,
      DAY_03_MOV TYPE C,
      DAY_04_MOV TYPE C,
      DAY_05_MOV TYPE C,
      DAY_06_MOV TYPE C,
      DAY_07_MOV TYPE C,
      DAY_08_MOV TYPE C,
      DAY_09_MOV TYPE C,
      DAY_10_MOV TYPE C,
      DAY_11_MOV TYPE C,
      DAY_12_MOV TYPE C,
      DAY_13_MOV TYPE C,
      DAY_14_MOV TYPE C,
      DAY_15_MOV TYPE C,
      DAY_16_MOV TYPE C,
      DAY_17_MOV TYPE C,
      DAY_18_MOV TYPE C,
      DAY_19_MOV TYPE C,
      DAY_20_MOV TYPE C,
      DAY_21_MOV TYPE C,
      DAY_22_MOV TYPE C,
      DAY_23_MOV TYPE C,
      DAY_24_MOV TYPE C,
      DAY_25_MOV TYPE C,
      DAY_26_MOV TYPE C,
      DAY_27_MOV TYPE C,
      DAY_28_MOV TYPE C,
      DAY_29_MOV TYPE C,
      DAY_30_MOV TYPE C,
      DAY_31_MOV TYPE C.

*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------

RANGES: R_MES_ANO FOR ZFIT0080-BUDAT.

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

DATA: VARIANTE     LIKE DISVARIANT,
      GS_VARIANT_C TYPE DISVARIANT.
