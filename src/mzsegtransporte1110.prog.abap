*----------------------------------------------------------------------*
***INCLUDE MZSEGTRANSPORTE1110.
*----------------------------------------------------------------------*

DATA: CK_FILTRAR TYPE CHAR01.

SELECTION-SCREEN BEGIN OF SCREEN 1111 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK APODATA WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: PFORNE FOR ZLEST0115-CD_FORNECEDOR,
                NAPOLI FOR ZLEST0115-NR_APOLICE,
                NPROPO FOR ZLEST0115-NR_PROPOSTA.
PARAMETERS: DTVALI TYPE SY-DATUM DEFAULT SY-DATUM,
            PEXCLU AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK APODATA.

SELECTION-SCREEN BEGIN OF BLOCK APGRUPO WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: PEMPRE FOR ZLEST0116-CD_EMPRESA,
                PGRUPO FOR ZLEST0116-CD_GRUPO.
SELECTION-SCREEN END OF BLOCK APGRUPO.
SELECTION-SCREEN END OF SCREEN 1111.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1110 OUTPUT.
  SET PF-STATUS 'PF1110'.
  SET TITLEBAR 'TL1110'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1110 INPUT.
  CASE OK_CODE.
    WHEN OK_FILTRAR.
      CK_FILTRAR = ABAP_TRUE.
      CLEAR: OK_CODE.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1110_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1110_EXIT INPUT.
  CLEAR: OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_APOLICES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_APOLICES .

  CK_FILTRAR = ABAP_FALSE.

  CALL SCREEN 1110 STARTING AT 5 5.

  IF CK_FILTRAR EQ ABAP_TRUE.
    PERFORM PESQUISAR_FILTRAR_APOLICES.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_FILTRAR_APOLICES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_FILTRAR_APOLICES .

  DATA: IT_LFA1 TYPE TABLE OF LFA1 WITH HEADER LINE.

  RANGES: RINICIO  FOR ZLEST0115-DT_INICIO,
          RFINAL   FOR ZLEST0115-DT_FINAL,
          RAPOLICE FOR ZLEST0115-CD_APOLICE,
          RELIMINA FOR ZLEST0115-CK_EXCLUIDO.

  CLEAR: IT_ZLEST0115,
         IT_ZLEST0115_ALV,
         IT_ZLEST0115[],
         IT_ZLEST0115_ALV[],
         IT_ZLEST0116,
         IT_ZLEST0116[],
         IT_ZLEST0116_ALV,
         IT_ZLEST0116_ALV[],
         RAPOLICE,
         RELIMINA,
         RINICIO,
         RFINAL.

  IF DTVALI IS NOT INITIAL OR DT_INICIAL_VALIDADE IS NOT INITIAL.

    IF DT_INICIAL_VALIDADE IS INITIAL.
      DT_INICIAL_VALIDADE = DTVALI.
    ENDIF.

    RINICIO-SIGN   = 'I'.
    RINICIO-OPTION = 'LE'.
    RINICIO-LOW    = DT_INICIAL_VALIDADE.
    RINICIO-HIGH   = DT_INICIAL_VALIDADE.
    APPEND RINICIO.

    RFINAL-SIGN   = 'I'.
    RFINAL-OPTION = 'GE'.
    RFINAL-LOW    = DT_INICIAL_VALIDADE.
    RFINAL-HIGH   = DT_INICIAL_VALIDADE.
    APPEND RFINAL.

    CLEAR: DT_INICIAL_VALIDADE.
  ENDIF.

  IF PEMPRE IS NOT INITIAL OR PGRUPO IS NOT INITIAL.
    CLEAR: IT_ZLEST0116[].
    SELECT * INTO TABLE IT_ZLEST0116
      FROM ZLEST0116
     WHERE CD_EMPRESA IN PEMPRE
       AND CD_GRUPO   IN PGRUPO.
    LOOP AT IT_ZLEST0116.
      RAPOLICE-SIGN   = 'I'.
      RAPOLICE-OPTION = 'EQ'.
      RAPOLICE-LOW    = IT_ZLEST0116-CD_APOLICE.
      RAPOLICE-HIGH   = IT_ZLEST0116-CD_APOLICE.
      APPEND RAPOLICE.
    ENDLOOP.
  ENDIF.

  CLEAR: RELIMINA.
  RELIMINA-SIGN   = 'I'.
  RELIMINA-OPTION = 'EQ'.
  IF PEXCLU EQ ABAP_TRUE.
    RELIMINA-LOW    = ABAP_TRUE.
    RELIMINA-HIGH   = ABAP_TRUE.
    APPEND RELIMINA.
  ELSE.
    RELIMINA-LOW    = ABAP_FALSE.
    RELIMINA-HIGH   = ABAP_FALSE.
  ENDIF.
  APPEND RELIMINA.

  SELECT * INTO TABLE IT_ZLEST0115
    FROM ZLEST0115
   WHERE CD_APOLICE    IN RAPOLICE
     AND CD_FORNECEDOR IN PFORNE
     AND NR_APOLICE    IN NAPOLI
     AND NR_PROPOSTA   IN NPROPO
     AND DT_INICIO     IN RINICIO
     AND DT_FINAL      IN RFINAL
     AND CK_EXCLUIDO   IN RELIMINA.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * INTO TABLE IT_LFA1
    FROM LFA1
     FOR ALL ENTRIES IN IT_ZLEST0115
   WHERE LIFNR EQ IT_ZLEST0115-CD_FORNECEDOR.

  SORT IT_LFA1 BY LIFNR.

  LOOP AT IT_ZLEST0115.
    CLEAR: IT_ZLEST0115_ALV.
    MOVE-CORRESPONDING IT_ZLEST0115 TO IT_ZLEST0115_ALV.

    READ TABLE IT_LFA1 WITH KEY LIFNR = IT_ZLEST0115-CD_FORNECEDOR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0115_ALV-NM_FORNECEDOR = IT_LFA1-NAME1.
    ENDIF.

    APPEND IT_ZLEST0115_ALV.
  ENDLOOP.

ENDFORM.
