*&---------------------------------------------------------------------*
*&  Include           ZFIY0001_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF TY_OUT_AUX,
        LINE(255) TYPE C,
END OF TY_OUT_AUX .

TYPES: BEGIN OF TY_DATA ,
        "ARQ(1)         TYPE C    ,
        PUBLI(8)       TYPE C    ,
        VIGEN(8)       TYPE C    ,
        FINAL(8)       TYPE C    ,
        STCD1          TYPE STCD1,
        TIPO(1)        TYPE C    ,
        ALTA_SUJETO(1) TYPE C    ,
        CAMBIO_ALIC(1) TYPE C    ,
        ALICU(4)       TYPE C    ,
        ALICU_PER(4)   TYPE C    ,
        GRUPO(2)       TYPE C    ,
*        ALICU_RET(4)   TYPE C    ,
*        GR_PER(2)      TYPE C    ,
*        GR_RET(2)      TYPE C    ,
        LIFNR          TYPE LIFNR,
        BUKRS          TYPE BUKRS,
END OF TY_DATA .


TYPES: BEGIN OF TY_DATA_ARBA ,
         ARQ(1)         TYPE C    ,
         PUBLI(8)       TYPE C    ,
         VIGEN(8)       TYPE C    ,
         FINAL(8)       TYPE C    ,
         STCD1          TYPE STCD1,
         TIPO(1)        TYPE C    ,
         ALTA_SUJETO(1) TYPE C    ,
         CAMBIO_ALIC(1) TYPE C    ,
         ALICU(4)       TYPE C    ,
         GRUPO(2)       TYPE C    ,
*        ALICU_PER(4)   TYPE C    ,
*        ALICU_RET(4)   TYPE C    ,
*        GR_PER(2)      TYPE C    ,
*        GR_RET(2)      TYPE C    ,
         LIFNR          TYPE LIFNR,
         BUKRS          TYPE BUKRS,
 END OF TY_DATA_ARBA.

TYPES: BEGIN OF TY_LFB1 ,
       LIFNR TYPE LIFNR ,
       BUKRS TYPE BUKRS ,
 END OF  TY_LFB1.

TYPES: BEGIN OF TY_LFA1 ,
       STCD1 TYPE STCD1 ,
       LIFNR TYPE LIFNR ,
       NAME1 TYPE CHAR40,
       BUKRS TYPE BUKRS ,
END OF TY_LFA1 .

TYPES: BEGIN OF TY_BAJADA,
         LINEA(255),
  END OF TY_BAJADA.

CONSTANTS: C_PATH TYPE PATHEXTERN VALUE '/DGI'.

DATA:
*       Tablas
        T_BAJ_COR  TYPE STANDARD TABLE OF  TY_BAJADA ,
        T_BAJ_INC  TYPE STANDARD TABLE OF  TY_BAJADA ,
        T_PADRONES TYPE STANDARD TABLE OF  TY_BAJADA ,
        T_LFA1     TYPE STANDARD TABLE OF  TY_LFA1   ,
        T_LFB1     TYPE STANDARD TABLE OF  TY_LFB1   ,
        T_NO_MOD   TYPE STANDARD TABLE OF  TY_LFA1   ,
        T_MODIF    TYPE STANDARD TABLE OF  TY_DATA   ,
        T_MODIF_ARBA TYPE STANDARD TABLE OF  TY_DATA_ARBA,
        T_OUT_AUX  TYPE STANDARD TABLE OF  TY_OUT_AUX,
        T_OUT_AUX2 TYPE STANDARD TABLE OF  TY_OUT_AUX,
        T_DATA     TYPE STANDARD TABLE OF  TY_DATA   ,
        T_DATA2    TYPE STANDARD TABLE OF  TY_DATA   ,
        T_DATA_ARBA   TYPE STANDARD TABLE OF  TY_DATA_ARBA,
        T_DATA2_ARBA  TYPE STANDARD TABLE OF  TY_DATA_ARBA,
*       Estructuras
        ST_DATA       TYPE  TY_DATA   ,
        ST_DATA_ARBA  TYPE  TY_DATA_ARBA,
        ST_SETLEAF    TYPE  SETLEAF    ,
        ST_BAJADA     TYPE  TY_BAJADA ,
        ST_PADRONES   TYPE  TY_BAJADA ,
        ST_LFA1       TYPE  TY_LFA1   ,
        ST_LFB1       TYPE  TY_LFB1   ,
        ST_OUT_AUX    TYPE  TY_OUT_AUX,
        ST_OUT_AUX2   TYPE  TY_OUT_AUX,
*       Variables
        V_F_PUBLI  TYPE CHAR10,
        V_ANS      TYPE CHAR1,
        V_VALFROM  TYPE DATUM,
        V_FECHA    TYPE DATUM.

DATA: V_PATH LIKE RLGRAP-FILENAME.
DATA  WL_ERRO(1).

DATA O_PROGRESS_IND TYPE REF TO CL_AKB_PROGRESS_INDICATOR.
