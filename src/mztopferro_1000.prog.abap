*----------------------------------------------------------------------*
***INCLUDE MZTOPFERRO_1000.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.

  DATA: IT_CODE TYPE TABLE OF SY-UCOMM WITH HEADER LINE.

  CLEAR: GB_TP_FRECO.

  IF SY-TCODE EQ 'ZLES0081'.
    GB_TP_FRECO = SPACE.
    GB_TP_MODAL = '4'.
  ELSE.
    GB_TP_FRECO = 'G'.
    GB_TP_MODAL = SPACE.
  ENDIF.

  IF VG_TELA_1000 IS INITIAL.
    DT_INICIAL_VALIDADE = SY-DATUM.
    PERFORM PESQUISAR_FILTRAR.
    VG_TELA_1000 = TL_1100.
  ENDIF.

  CASE VG_TELA_1000.
    WHEN TL_1100.
      CLEAR: IT_CODE[].
      APPEND OK_CARGA_DD TO IT_CODE.
      SET PF-STATUS 'PF1100' EXCLUDING IT_CODE.

      IF SY-TCODE EQ 'ZLES0081'.
        SET TITLEBAR  'TL1100'.
      ELSE.
        SET TITLEBAR  'TL1100G'.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.

  "CK_OPERACAO  TYPE CHAR01.  "I - Inclusão / A - Modificação / C - Consulta

  CASE OK_CODE.
    WHEN OK_PESQUISAR.
      PERFORM PESQUISAR.
      CLEAR: OK_CODE.
    WHEN OK_INCLUIR.
      PERFORM CADASTRAR.
      CLEAR: OK_CODE.
    WHEN OK_MODIFICAR.
      PERFORM EDITAR.
      CLEAR: OK_CODE.
    WHEN OK_CONSULTAR.
      PERFORM ABRIR.
    WHEN OK_EXCLUIR.
      PERFORM EXCLUIR.
      CLEAR: OK_CODE.
    WHEN OK_ITINERARIO.
      PERFORM ITINERARIO.
      CLEAR: OK_CODE.
    WHEN OK_REATIVAR.
      PERFORM REATIVAR.
      CLEAR: OK_CODE.
    WHEN OK_CARGA_DD.
      PERFORM CARGA_DE_DADOS.
      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000_EXIT INPUT.
  LEAVE PROGRAM.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CARGA_DE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CARGA_DE_DADOS .

  DATA: IT_ZLEST0069 TYPE TABLE OF ZLEST0069 WITH HEADER LINE,
        IT_ZLEST0070 TYPE TABLE OF ZLEST0070 WITH HEADER LINE.

  CLEAR: IT_ZLEST0118[], IT_ZLEST0119[].

  SELECT * INTO TABLE IT_ZLEST0069 FROM ZLEST0069.

  LOOP AT IT_ZLEST0069.
    CLEAR: IT_ZLEST0118.
    IT_ZLEST0118-LIFNR            = IT_ZLEST0069-LIFNR.
    IT_ZLEST0118-PAIS             = 'BR'.
    IT_ZLEST0118-DOMICILIO_ORIGEM = IT_ZLEST0069-DOMICILIO_ORIGEM.
    IT_ZLEST0118-DOMICILIO_DESTIN = IT_ZLEST0069-DOMICILIO_DESTIN.
    APPEND IT_ZLEST0118.
  ENDLOOP.

  IF IT_ZLEST0118[] IS NOT INITIAL.
    MODIFY ZLEST0118 FROM TABLE IT_ZLEST0118.
    COMMIT WORK.
  ENDIF.

  SELECT * INTO TABLE IT_ZLEST0070 FROM ZLEST0070.
  LOOP AT IT_ZLEST0070.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR = '01'
        OBJECT      = 'ZLES000001'
      IMPORTING
        NUMBER      = IT_ZLEST0119-CD_SEQ_LANC.

    IT_ZLEST0119-DT_INICIO        = IT_ZLEST0070-DT_INICIO.
    IT_ZLEST0119-DT_FIM           = IT_ZLEST0070-DT_FIM.
    IT_ZLEST0119-LIFNR            = IT_ZLEST0070-LIFNR.
    IT_ZLEST0119-PAIS             = 'BR'.
    IT_ZLEST0119-DOMICILIO_ORIGEM = IT_ZLEST0070-DOMICILIO_ORIGEM.
    IT_ZLEST0119-DOMICILIO_DESTIN = IT_ZLEST0070-DOMICILIO_DESTIN.
    IT_ZLEST0119-TIPO             = IT_ZLEST0070-TIPO.
    IT_ZLEST0119-QTD_NEGOCIADO    = IT_ZLEST0070-PESO.
    IT_ZLEST0119-UND_NEGOCIADO    = IT_ZLEST0070-UNID_MEDIDA.
    IT_ZLEST0119-PERC_TOLERANCIA  = IT_ZLEST0070-PERC_TOLER.
    IT_ZLEST0119-QTD_TOLERANCIA   = IT_ZLEST0070-PESO_TOLER.
    IT_ZLEST0119-WAERK            = IT_ZLEST0070-WAERK.
    IT_ZLEST0119-PRECO            = IT_ZLEST0070-NETPR.
    IT_ZLEST0119-UND_PRECO        = IT_ZLEST0070-UNID_MED_NETPR.
    IT_ZLEST0119-CK_EXCLUIDO      = ABAP_FALSE.
    IT_ZLEST0119-TP_PRECO         = GB_TP_FRECO.
    APPEND IT_ZLEST0119.
  ENDLOOP.

  IF IT_ZLEST0119[] IS NOT INITIAL.
    MODIFY ZLEST0119 FROM TABLE IT_ZLEST0119.
    COMMIT WORK.
  ENDIF.

  CLEAR: IT_ZLEST0118[], IT_ZLEST0119[].

ENDFORM.
