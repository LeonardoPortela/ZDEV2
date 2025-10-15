*&---------------------------------------------------------------------*
*& Report  ZGL037
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZGL037.

INCLUDE ZGL037_TOP.
*INCLUDE ZGL033_TOP.

TABLES: ZGLT064,ZGLT050,ZGLT067.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZGLT050-BUKRS OBLIGATORY,
                S_VGDE    FOR ZGLT050-VIG_DE,
                S_VGATE   FOR ZGLT050-VIG_ATE,
                S_LIFNR   FOR ZGLT050-COD_SEGURADORA,
                S_STIPO   FOR ZGLT064-SEQ_TIPO NO INTERVALS NO-EXTENSION,
                S_TPOPR   FOR ZGLT050-TP_OPR NO INTERVALS NO-EXTENSION,
                S_SLCTO   FOR ZGLT050-SEQ_LCTO,
                S_NR_APL  FOR ZGLT050-NRO_APOLICE,
                S_WERKS   FOR ZGLT067-WERKS,
                S_ZFBDT   FOR ZGLT067-DT_VENC.

PARAMETER: P_COMP TYPE C LENGTH 6,
           P_CPPG TYPE C LENGTH 6.

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETER: P_VARIA TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK B2.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*

DATA: VG_VARIANT        TYPE DISVARIANT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.

  VARIANTE-REPORT = SY-REPID.

  IF ( P_VARIA IS NOT INITIAL ).
    VG_VARIANT-VARIANT = P_VARIA.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = VARIANTE
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = VARIANTE
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE VARIANTE-VARIANT TO P_VARIA.
    MOVE VARIANTE-VARIANT TO GS_VARIANT_C-VARIANT.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_STIPO-LOW.

   PERFORM HELP_SEARCH USING 'S_STIPO-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SLCTO-LOW.

   PERFORM HELP_SEARCH USING 'S_SLCTO-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SLCTO-HIGH.

   PERFORM HELP_SEARCH USING 'S_SLCTO-HIGH'.

*----------------------------------------------------------------------*
* INCLUDES
**----------------------------------------------------------------------*

INCLUDE ZGL037_FORM.
*INCLUDE ZGL033_FORM.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM INICIAR_VARIAVEIS.
  PERFORM SELECIONA_DADOS.

  IF VG_NOT_FOUND IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM PROCESSA_DADOS.
  PERFORM IMPRIME_DADOS.
