*---------------------------------------------------------------------*
*    generated viewmaintenance function pool top
*---------------------------------------------------------------------*
FUNCTION-POOL ZPARAMETROS                MESSAGE-ID SV.
TYPE-POOLs: SLIS, KKBLO.
TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      LAYOUT       TYPE SLIS_LAYOUT_ALV,
*        t_print      TYPE slis_print_alv,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID VALUE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.

DATA: TG_FILENAME  TYPE TABLE OF SDOKPATH WITH HEADER LINE.
  DATA: BEGIN OF Tg_SAIDA OCCURS 0,
        MARK,
        FILENAME TYPE SDOK_FILNM,
        END OF Tg_SAIDA.

INCLUDE LSVIMDAT                                . "general data decl.
INCLUDE LZPARAMETROST00                         . "view rel. data dcl.
