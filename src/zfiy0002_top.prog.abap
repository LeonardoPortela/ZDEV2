*&---------------------------------------------------------------------*
*&  Include           ZFIY0002_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_OUT_AUX,
        LINE(255) TYPE C  ,
END OF TY_OUT_AUX .
TYPES: BEGIN OF TY_LFBW ,
       MANDT     TYPE MANDT     ,
       LIFNR     TYPE LIFNR     ,
       BUKRS     TYPE BUKRS     ,
       WITHT     TYPE WITHT     ,
       WT_SUBJCT TYPE WT_SUBJCT ,
       QSREC     TYPE WT_QSREC  ,
       WT_WTSTCD TYPE WT_WTSTCD ,
       WT_WITHCD TYPE WT_WITHCD ,
       WT_EXNR   TYPE WT_EXNR   ,
       WT_EXRT   TYPE WT_EXRT   ,
       WT_EXDF   TYPE WT_EXDF   ,
       WT_EXDT   TYPE WT_EXDT   ,
       WT_WTEXRS TYPE WT_WTEXRS ,
END OF TY_LFBW .

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
        GRUPO(2)       TYPE C    ,
*        ALICU_PER(4)   TYPE C    ,
*        ALICU_RET(4)   TYPE C    ,
*        GR_PER(2)      TYPE C    ,
*        GR_RET(2)      TYPE C    ,
        LIFNR          TYPE LIFNR,
        BUKRS          TYPE BUKRS,
        RET            TYPE CHAR2,
        RET_A          TYPE CHAR2,
END OF TY_DATA .

TYPES: BEGIN OF TY_DATA_ARBA,
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
        RET            TYPE CHAR2,
        RET_A          TYPE CHAR2,
END OF TY_DATA_ARBA .

TYPES: BEGIN OF TY_DATA_PAR,
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
        RET            TYPE CHAR2,
        RET_A          TYPE CHAR2,
END OF TY_DATA_PAR.


TYPES: BEGIN OF TY_ALV ,
       SEMAFORO TYPE CHAR4 ,
       STCD1    TYPE STCD1 ,
       LIFNR    TYPE LIFNR ,
       BUKRS    TYPE BUKRS ,
       MODO     TYPE CHAR40,
       TEXT     TYPE CHAR40,
       RETA     TYPE WITHT,
       RETD     TYPE WITHT,
END OF TY_ALV .

TYPES: BEGIN OF TY_LFA1 ,
       STCD1 TYPE STCD1 ,
       LIFNR TYPE LIFNR ,
       NAME1 TYPE CHAR40,
END OF TY_LFA1 .

TYPES: BEGIN OF TY_BAJADA,
         LINEA(255),
  END OF TY_BAJADA.

CONSTANTS: C_PATH TYPE PATHEXTERN VALUE '/DGI'.

DATA:
*       Tablas
        T_MES_SAL TYPE STANDARD TABLE OF TY_ALV     ,
        T_LFBW    TYPE STANDARD TABLE OF  TY_LFBW   ,
        T_LFA1    TYPE STANDARD TABLE OF  TY_LFA1   ,
        T_NO_MOD  TYPE STANDARD TABLE OF  TY_LFA1   ,
        T_MODIF   TYPE STANDARD TABLE OF  TY_DATA   ,
        T_MODIF_ARBA TYPE STANDARD TABLE OF  TY_DATA_ARBA,
        T_OUT_AUX TYPE STANDARD TABLE OF  TY_OUT_AUX,
        T_DATA    TYPE STANDARD TABLE OF  TY_DATA   ,
        T_DATA_ARBA TYPE STANDARD TABLE OF  TY_DATA_ARBA,

*       Estructuras
        ST_MES     TYPE  TY_ALV    ,
        ST_DATA    TYPE  TY_DATA   ,
        ST_DATA_PAR TYPE  TY_DATA_PAR,
        ST_DATA_ARBA TYPE  TY_DATA_ARBA   ,
        ST_LFBW    TYPE  TY_LFBW   ,
        ST_LFA1    TYPE  TY_LFA1   ,
        ST_OUT_AUX TYPE  TY_OUT_AUX,
        ST_SETLEAF TYPE SETLEAF,

*      Variables
        V_VALFROM  TYPE DATUM,
        V_FECHA    TYPE DATUM,
        V_SUBRC    TYPE SY-SUBRC,
        V_ANS      TYPE CHAR1,
        VL_MODO    TYPE C.

DATA: V_PATH LIKE RLGRAP-FILENAME.
*******************ALV**********************************
TYPE-POOLS: SLIS.
TYPES: W_SLIS_T_FIELDCAT_ALV TYPE SLIS_FIELDCAT_ALV OCCURS 1.

DATA:
        LAYOUT              TYPE SLIS_LAYOUT_ALV,
        T_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
        T_EVENTS            TYPE SLIS_T_EVENT,
        GT_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
        V_REPID             TYPE SY-REPID.
