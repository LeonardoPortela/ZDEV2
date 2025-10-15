*----------------------------------------------------------------------*
***INCLUDE ZFIR0072_SEARCH_CONTAI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SEARCH_CONTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE SEARCH_CONTA INPUT.
*
*  TYPES: BEGIN OF F4_CON,
*           SAKNR TYPE SKAT-SAKNR,
*           TXT50 TYPE   SKAT-TXT50,
*         END OF F4_CON.
**  --------------------------------------------------------------*
**  Data Declaration
**  --------------------------------------------------------------*
*  DATA: WA_F4 TYPE F4_CON,
*        IT_F4 TYPE TABLE OF F4_CON.
*  DATA: IT_RETURN TYPE TABLE OF DDSHRETVAL,
*        WA_RETURN TYPE DDSHRETVAL.
*  DATA: WA_DYNPFIELDS TYPE DYNPREAD,
*        IT_DYNPFIELDS TYPE TABLE OF DYNPREAD.
**  DATA: GV_WERKS       TYPE MARC-WERKS.
**  --------------------------------------------------------------*
**  Selection-Screen
**  --------------------------------------------------------------*
**  PARAMETERS: P_WERKS TYPE MARC-WERKS OBLIGATORY.
**  PARAMETERS: P_MATNR TYPE MARA-MATNR.
*
**  --------------------------------------------------------------*
**  Selection-Screen on Value-Request
**  --------------------------------------------------------------*
**AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CON.
*
*  REFRESH IT_DYNPFIELDS.
*
*  WA_DYNPFIELDS-FIELDNAME = 'P_CON'.
*  APPEND WA_DYNPFIELDS TO IT_DYNPFIELDS.
*
***  Get plant value on the selection screen
**  CALL FUNCTION 'DYNP_VALUES_READ'
**    EXPORTING
**      DYNAME               = SY-REPID
**      DYNUMB               = SY-DYNNR
**    TABLES
**      DYNPFIELDS           = GT_DYNPFIELDS
**    EXCEPTIONS
**      INVALID_ABAPWORKAREA = 1
**      INVALID_DYNPROFIELD  = 2
**      INVALID_DYNPRONAME   = 3
**      INVALID_DYNPRONUMMER = 4
**      INVALID_REQUEST      = 5
**      NO_FIELDDESCRIPTION  = 6
**      INVALID_PARAMETER    = 7
**      UNDEFIND_ERROR       = 8
**      DOUBLE_CONVERSION    = 9
**      STEPL_NOT_FOUND      = 10
**      OTHERS               = 11.
*
**  Get values from the database based on plant
**  SELECT A~WERKS
**         A~MATNR
**         B~MAKTX
**         UP TO 10 ROWS
**         INTO TABLE GT_MATNR
**         FROM MARC AS A
**         INNER JOIN MAKT AS B
**         ON A~MATNR = B~MATNR
**         WHERE A~WERKS = GV_WERKS
**           AND B~SPRAS = 'EN'.
*
*  SELECT SAKNR TXT50 FROM SKAT INTO TABLE IT_F4.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      RETFIELD        = 'SAKNR'
*      VALUE_ORG       = 'S'
*    TABLES
*      VALUE_TAB       = IT_F4
*      RETURN_TAB      = IT_RETURN
*    EXCEPTIONS
*      PARAMETER_ERROR = 1
*      NO_VALUES_FOUND = 2
*      OTHERS          = 3.
*
*  READ TABLE IT_RETURN INTO WA_RETURN INDEX 1.
*
*  IF SY-SUBRC = 0.
*    P_CON = WA_RETURN-FIELDVAL.
*  ENDIF.
*
*ENDMODULE.
