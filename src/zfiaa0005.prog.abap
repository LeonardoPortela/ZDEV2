*&---------------------------------------------------------------------*
*& Report  ZFIAA0005
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfiaa0005.

TABLES: anla.


*---------------------------------------------------------------------*
* TABLES                                                              *
*---------------------------------------------------------------------*
DATA: git_anla TYPE TABLE OF anla,
      gwa_anla TYPE anla.

*---------------------------------------------------------------------*
* SELECTION SCREEN                                                    *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs  FOR anla-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY ,
                s_anln1  FOR anla-anln1 NO INTERVALS OBLIGATORY,
                s_anln2  FOR anla-anln2 NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_aktiv  FOR anla-aktiv NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_zugdt  FOR anla-zugdt NO INTERVALS NO-EXTENSION, "Data da aquisição
                s_zuper  FOR anla-zuper NO INTERVALS NO-EXTENSION, "Mês aquisição
                s_zujhr  FOR anla-zujhr NO INTERVALS NO-EXTENSION. "Ano aquisição
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  DATA: v_date     TYPE sy-datum,
        v_year(4)  TYPE c,
        v_month(2) TYPE c,
        v_day(2)   TYPE c.


* Monta mês/ano
  v_year  = s_aktiv-low+0(4).
  v_month = s_aktiv-low+4(2).

  s_zuper-low =  v_month.
  s_zujhr-low =  v_year.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen.

*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .
  LOOP AT SCREEN.
    IF screen-name = 'S_ZUJHR-LOW'. "Ano Aquisição.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_ZUPER-LOW'. "Mês Aquisição.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_ZUGDT-LOW'. "Data de Aquisição.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF s_aktiv-low IS NOT INITIAL.
    DATA: v_date     TYPE sy-datum,
          v_year(4)  TYPE c,
          v_month(2) TYPE c,
          v_day(2)   TYPE c.


* Monta mês/ano
    v_year  = s_aktiv-low+0(4).
    v_month = s_aktiv-low+4(2).

    s_zuper-low =  v_month.
    APPEND s_zuper.
    s_zujhr-low =  v_year.
    APPEND s_zujhr.
    s_zugdt-low = s_aktiv-low.
    APPEND s_zugdt.

    MODIFY SCREEN.
  ENDIF.


ENDFORM.

*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM seleciona_dados.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados .

  SELECT * FROM anla INTO TABLE git_anla
    WHERE bukrs  IN s_bukrs
      AND anln1	 IN s_anln1
      AND anln2  IN s_anln2.

  IF git_anla IS INITIAL.
    MESSAGE 'Imobilizado não encontrado' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.

    LOOP AT git_anla INTO gwa_anla.

      UPDATE anla SET
          zujhr  = s_aktiv-low+0(4) "Ano do Exercício
          zuper  = s_aktiv-low+4(2) "Mês Aquisição
          aktiv  = s_aktiv-low     "Data da Incorporação
          zugdt  = s_aktiv-low     "Data da Aquisição
          WHERE  bukrs  EQ gwa_anla-bukrs
             AND anln1 EQ gwa_anla-anln1
             AND anln2 EQ gwa_anla-anln2.
      CLEAR:  gwa_anla.

    ENDLOOP.
    COMMIT WORK.
    MESSAGE 'Alteração realizada com sucesso' TYPE 'I'.
  ENDIF.
ENDFORM.
