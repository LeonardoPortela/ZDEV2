*&---------------------------------------------------------------------*
*& Report ZFIR0116
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0116.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  "Contas Desconto Antecipação
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_opc_01 RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(40) TEXT-002 FOR FIELD p_opc_01.
  SELECTION-SCREEN END OF LINE.

  "Tipo Ordem Venda
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_opc_02 RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(20) TEXT-003 FOR FIELD p_opc_02.
  SELECTION-SCREEN END OF LINE.

  "Grupo Mercadoria
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_opc_03 RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(20) TEXT-004 FOR FIELD p_opc_03.
  SELECTION-SCREEN END OF LINE.

  "Empresas
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_opc_04 RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(20) TEXT-005 FOR FIELD p_opc_03.
  SELECTION-SCREEN END OF LINE.


    "Empresas
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_opc_05 RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(50) TEXT-006 FOR FIELD p_opc_03.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.

  CASE abap_true.
    WHEN p_opc_01. "Contas Desconto Antecipação
      SUBMIT zregister_data WITH p_db_tab = 'ZFIT0211'
                            WITH p_stcnam = 'ZFIT0211'
                            WITH p_scmant = '0252'
                            WITH p_title  = 'Parâmetros Contas Descontos Antecipação - ZFI0064' AND RETURN.

    WHEN p_opc_02. "Tipos Ordem Venda
      SUBMIT zregister_data WITH p_db_tab = 'ZFIT0221'
                            WITH p_stcnam = 'ZFIT0221'
                            WITH p_scmant = '0275'
                            WITH p_title  = 'Parâmetros Tipos Ordem Venda - ZFI0064' AND RETURN.

    WHEN p_opc_03. "Grupo Mercadoria
      SUBMIT zregister_data WITH p_db_tab = 'ZFIT0222'
                            WITH p_stcnam = 'ZFIT0222'
                            WITH p_scmant = '0276'
                            WITH p_title  = 'Parâmetros Grupo Mercadoria - ZFI0064' AND RETURN.
    WHEN p_opc_04. "Empresas
      SUBMIT zregister_data WITH p_db_tab = 'ZFIT0223'
                            WITH p_stcnam = 'ZFIT0223'
                            WITH p_scmant = '0277'
                            WITH p_title  = 'Parâmetros Empresas - ZFI0064' AND RETURN.


        WHEN p_opc_05. "Contas Abatimento/Desconto/Juros  "RG / #175263 / 12.05.2025
      SUBMIT zregister_data WITH p_db_tab = 'ZFIT0233'
                            WITH p_stcnam = 'ZFIT0233'
                            WITH p_scmant = '0308'
                            WITH p_title  = 'Parâmetros Empresas - ZFI0064' AND RETURN.
  ENDCASE.
