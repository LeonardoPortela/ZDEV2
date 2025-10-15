*&---------------------------------------------------------------------*
*& Report  ZMMR019_03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR019_03.

FORM EXPORT_MZMMR019  TABLES  P_IT_ITEMDATA STRUCTURE BAPI_INCINV_CREATE_ITEM.

  READ TABLE P_IT_ITEMDATA INTO  DATA(WA_ITEMDATA) INDEX 1.

  EXPORT P1 = WA_ITEMDATA-ITEM_TEXT  TO MEMORY ID 'MZMMR019'.

ENDFORM.
