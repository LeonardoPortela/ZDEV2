*----------------------------------------------------------------------*
***INCLUDE MZREPOM_0002.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.

  DATA: CK_CONSULTA TYPE CHAR01.

  CASE OK_CODE.
    WHEN OK_PESQUISAR.
      CLEAR: OK_CODE.
      PERFORM PESQUISAR_VIAGENS USING CK_CONSULTA.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0002 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2001'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_VIAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CK_CONSULTA  text
*----------------------------------------------------------------------*
FORM PESQUISAR_VIAGENS  USING  P_CONSULTA TYPE CHAR01.

  DATA: OBJ_VIAGEM TYPE REF TO ZCL_REPOM_VIAGEM_VPR.

  DATA: I_FILTROS   TYPE ZDE_ZLEST0123_FILTRO,
        E_REGISTROS TYPE ZDE_ZLEST0123_ALV_T.

  CLEAR: I_FILTROS.

  MOVE: P2012[] TO I_FILTROS-ID_PROC_CLIENTE,
        P2001[] TO I_FILTROS-BUKRS,
        P2002[] TO I_FILTROS-BRANCH,
        P2003[] TO I_FILTROS-CD_CID_ORIGEM,
        P2004[] TO I_FILTROS-CD_CID_DESTINO,
        P2005[] TO I_FILTROS-MOTORISTA_CPF,
        P2006[] TO I_FILTROS-VEICULO_PLACA,
        P2007[] TO I_FILTROS-TP_STATUS_AUT,
        P2008[] TO I_FILTROS-TP_STATUS_CAN,
        P2009[] TO I_FILTROS-DT_EMISSAO_PEDAGIO,
        P2010[] TO I_FILTROS-TKNUM,
        P2011[] TO I_FILTROS-US_ULTIMO_AJUSTE,
        P2013[] TO I_FILTROS-DT_ULTIMO_AJUSTE.

  CALL FUNCTION 'Z_REPOM_CHAMA_MONITOR'
    EXPORTING
      I_FILTROS = I_FILTROS.

ENDFORM.
