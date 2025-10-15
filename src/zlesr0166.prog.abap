*&---------------------------------------------------------------------*
*& Report ZFIR0116
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlesr0166.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  "Contas Desconto Antecipação
  SELECTION-SCREEN BEGIN OF LINE.
    "PARAMETERS: p_opc_01 RADIOBUTTON GROUP rb1.
    PARAMETERS: p_opc_01 AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(40) TEXT-002 FOR FIELD p_opc_01.
  SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.

  CASE abap_true.
    WHEN p_opc_01. "Parâmetros Empresas
      SUBMIT zregister_data WITH p_db_tab = 'ZLEST0252'
                            WITH p_stcnam = 'ZLEST0252'
                            WITH p_scmant = '0278'
                            WITH p_title  = 'Parâmetros Empresas - ZLES0079' AND RETURN.

  ENDCASE.
