*----------------------------------------------------------------------*
* Include ZFIY0020_TOP
*----------------------------------------------------------------------*

* Tsblas transparentes ------------------------------------------------*
TABLES: BSIK, WITH_ITEM.

* Tipos de datos ------------------------------------------------------*
TYPES: BEGIN OF TY_SAL,
        BUKRS LIKE BSIK-BUKRS,
        GJAHR LIKE BSIK-GJAHR,
        LIFNR LIKE BSIK-LIFNR,
        NAME1 LIKE LFA1-NAME1,
        BELNR LIKE BSIK-BELNR,
        WITHT LIKE WITH_ITEM-WITHT,
        WIOLD LIKE WITH_ITEM-WT_WITHCD,
        WIACT LIKE WITH_ITEM-WT_WITHCD,
       END OF TY_SAL.

* Tablas internas -----------------------------------------------------*
DATA: T_SAL TYPE STANDARD TABLE OF TY_SAL,
      E_SAL LIKE LINE OF T_SAL.

* Definiciones ALV ----------------------------------------------------*
TYPE-POOLS: KKBLO.
* Variables
DATA: V_POS      TYPE I,
      V_STATUS   TYPE SLIS_FORMNAME,
      V_USER     TYPE SLIS_FORMNAME,
      V_TOP      TYPE SLIS_FORMNAME,
      T_VARIANT  TYPE DISVARIANT.
* FIELD CATALOG
DATA: T_AFIELD   TYPE KKBLO_FIELDCAT,
      T_FIELDCAT TYPE KKBLO_T_FIELDCAT,
      T_FCAT     TYPE SLIS_T_FIELDCAT_ALV.
* LAYOUT
DATA: T_LAYOUT   TYPE KKBLO_LAYOUT,
      T_LAY      TYPE SLIS_LAYOUT_ALV,
      T_HEAD     TYPE KKBLO_LISTHEADER,
      T_HEADER   TYPE KKBLO_T_LISTHEADER.
* EVENTS
DATA: T_EVENT    TYPE SLIS_T_EVENT,
      T_EVENTS   TYPE SLIS_ALV_EVENT.
* SORT
DATA: T_SORT    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.
* OUT
DATA: BEGIN OF T_OUT OCCURS 0.
DATA:   BOX     TYPE C.
        INCLUDE STRUCTURE E_SAL.
DATA:   COLINFO TYPE KKBLO_T_SPECIALCOL,
      END OF T_OUT.
* Constantes
CONSTANTS : C_TOP    TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
            C_USER   TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
