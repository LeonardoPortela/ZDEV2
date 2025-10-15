*----------------------------------------------------------------------*
***INCLUDE ZFIR0072_SEARCH_MODALI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SEARCH_MODAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_MODAL INPUT.

  TYPES: BEGIN OF F4_TCO,
           COD  TYPE TBSL-KOART,
           DESC TYPE BSIK-SGTXT,
         END OF F4_TCO.
*  --------------------------------------------------------------*
*  Data Declaration
*  --------------------------------------------------------------*
  DATA: WA_F4_2 TYPE F4_TCO,
        IT_F4_2 TYPE TABLE OF F4_TCO.
  DATA: IT_RETURN_2 TYPE TABLE OF DDSHRETVAL,
        WA_RETURN_2 TYPE DDSHRETVAL.
  DATA: WA_DYNPFIELDS_2 TYPE DYNPREAD,
        IT_DYNPFIELDS_2 TYPE TABLE OF DYNPREAD.
*  --------------------------------------------------------------*
*  Selection
*  --------------------------------------------------------------*
  REFRESH IT_DYNPFIELDS_2.

  WA_DYNPFIELDS_2-FIELDNAME = 'P_TCO'.
  APPEND WA_DYNPFIELDS_2 TO IT_DYNPFIELDS_2.

  WA_F4_2-COD = 'K'.
  WA_F4_2-DESC = 'Fornecedor'.
  APPEND WA_F4_2 TO IT_F4_2.

  WA_F4_2-COD = 'D'.
  WA_F4_2-DESC = 'Cliente'.
  APPEND WA_F4_2 TO IT_F4_2.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'COD'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_F4_2
      RETURN_TAB      = IT_RETURN_2
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  READ TABLE IT_RETURN_2 INTO WA_RETURN_2 INDEX 1.

  IF SY-SUBRC = 0.
    P_TCO = WA_RETURN_2-FIELDVAL.
  ENDIF.

  REFRESH: IT_F4_2.

ENDMODULE.
