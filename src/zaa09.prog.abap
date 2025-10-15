*&---------------------------------------------------------------------*
*& Report  ZAA09
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZAA09.

TABLES: T001, RA02S, ANLA.


DATA: TG_ANLA TYPE TABLE OF ANLA WITH HEADER LINE.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS     FOR T001-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,
                S_RANL1     FOR RA02S-RANL1,
                S_RANL2     FOR RA02S-RANL2,
                S_DEAKT     FOR ANLA-DEAKT NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT *
    INTO TABLE TG_ANLA
    FROM ANLA
   WHERE BUKRS IN S_BUKRS
     AND ANLN1 IN S_RANL1
     AND ANLN2 IN S_RANL2.

  IF TG_ANLA[] IS NOT INITIAL.
    MESSAGE 'Desativando os Imobilizados informados!' TYPE 'S'.
  ELSE.
    MESSAGE 'Não foram encontrados registros para os parâmetros informados!' TYPE 'S'.
    RETURN.
  ENDIF.

  LOOP AT TG_ANLA.

    IF TG_ANLA-DEAKT IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    TG_ANLA-DEAKT = S_DEAKT-LOW.

    MODIFY ANLA FROM TG_ANLA.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro na desativação dos imobilizados!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.
  MESSAGE 'Imobilizados Desativados!' TYPE 'S'.

  "PERFORM CHAMA_BABI.

INCLUDE ZAA09_FORM.
