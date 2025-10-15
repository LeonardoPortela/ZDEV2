*&---------------------------------------------------------------------*
*& Report  ZMMR127
*&
*&---------------------------------------------------------------------*
*& Programa para gerar pedido em BackGround para utilizar um usuário de
*& Serviço
*&---------------------------------------------------------------------*
REPORT ZMMR127.

TABLES: ZMMT_PO_ZGR.

PARAMETERS POBJKEY TYPE ZMMT_PO_ZGR-OBJ_KEY NO-DISPLAY.

START-OF-SELECTION.

  CHECK POBJKEY IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(IT_ZMM_PO_ZGR)
    FROM ZMMT_PO_ZGR
   WHERE OBJ_KEY EQ @POBJKEY.

*  SELECT * INTO TABLE @DATA(IT_ZMMT_PO_ITEM_ZGR)
*    FROM ZMMT_PO_ITEM_ZGR
*   WHERE OBJ_KEY EQ @POBJKEY.

  DATA: I_SAP_UNITARIO TYPE CHAR01.

  I_SAP_UNITARIO = ABAP_TRUE.

  CALL FUNCTION 'Z_MM_INBOUND_POZGR'
    TABLES
      T_ZMM_PO_ZGR   = IT_ZMM_PO_ZGR
    CHANGING
      I_SAP_UNITARIO = I_SAP_UNITARIO.

  COMMIT WORK.
