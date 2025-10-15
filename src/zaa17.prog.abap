*&---------------------------------------------------------------------*
*& Report  ZAA17
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zaa17.




TABLES: anla,anlz, zaa005.

TYPES: BEGIN OF ty_saida,
         bukrs TYPE anla-bukrs,
         anln1 TYPE anla-anln1,
         anln2 TYPE anla-anln2,
         aktiv TYPE anla-aktiv,
         txt50 TYPE anla-txt50,
         txa50 TYPE anla-txa50,
         invnr TYPE anla-invnr,
         sernr TYPE anla-sernr,
         stort TYPE anlz-stort,
         raumn TYPE anlz-raumn,
         werks TYPE anlz-werks,
         kostl TYPE anlz-kostl,
         kfzkz TYPE anlz-kfzkz,
       END OF ty_saida.


DATA: it_saida  TYPE TABLE OF ty_saida,
      wa_saida  TYPE ty_saida,
      wa_zaa005 TYPE zaa005.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_bukrs FOR anla-bukrs  OBLIGATORY, "Empresa
                p_werks FOR anlz-werks, "Filial
                p_kostl FOR anlz-kostl, "Centro de custo
                p_anln1 FOR anla-anln1, "Imobilizado
                p_anln2 FOR anla-anln2, "SUB-Numero
                p_deakt FOR zaa005-gjahr NO INTERVALS NO-EXTENSION DEFAULT sy-datum(4) MODIF ID so2. "Exercício
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'SO2'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.


  PERFORM z_busca_dados.

  "GET peras.

END-OF-SELECTION.
  PERFORM z_processa_dados.

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_busca_dados .

  SELECT
      a~bukrs
      a~anln1
      a~anln2
      a~aktiv
      a~txt50
      a~txa50
      a~invnr
      a~sernr
      z~stort
      z~raumn
      z~werks
      z~kostl
      z~kfzkz
    INTO TABLE it_saida
    FROM
      anla AS a INNER JOIN anlz AS z
      ON a~bukrs = z~bukrs AND a~anln1 = z~anln1 AND a~anln2 = z~anln2
     WHERE
      a~bukrs IN p_bukrs AND
      z~werks IN p_werks  AND
      z~kostl IN p_kostl  AND
      a~anln1 IN p_anln1 AND
      a~anln2 IN p_anln2 AND
      a~deakt = '00000000'.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Para os parâmetros informados não foi encontrado registros' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_processa_dados .

  IF it_saida[] IS NOT INITIAL.

    LOOP AT it_saida INTO wa_saida.

      CLEAR wa_zaa005.

      wa_zaa005-mandt       =        sy-mandt.
      wa_zaa005-anln1       =        wa_saida-anln1.
      wa_zaa005-anln2       =        wa_saida-anln2.
      wa_zaa005-bukrs       =        wa_saida-bukrs.
      wa_zaa005-gsber       =        wa_saida-werks.
      wa_zaa005-kostl       =        wa_saida-kostl.
      wa_zaa005-gjahr       =        p_deakt-low.
      wa_zaa005-aktiv       =        wa_saida-aktiv.
      wa_zaa005-txt50       =        wa_saida-txt50.
      wa_zaa005-txa50       =        wa_saida-txa50.
      wa_zaa005-invnr       =        wa_saida-invnr.
      wa_zaa005-sernr       =        wa_saida-sernr.
      wa_zaa005-stort       =        wa_saida-stort.
      wa_zaa005-kfzkz       =        wa_saida-kfzkz.
      wa_zaa005-raumn       =        wa_saida-raumn.
      wa_zaa005-dt_proc     =        sy-datum.
      wa_zaa005-hr_proc     =        sy-uzeit.
      wa_zaa005-us_proc     =        sy-uname.

      SELECT *
        FROM zaa005
        INTO @DATA(w_0005)
          UP TO 1 ROWS
       WHERE anln1 = @wa_zaa005-anln1
         AND anln2 = @wa_zaa005-anln2
         AND bukrs = @wa_zaa005-bukrs
         AND gsber = @wa_zaa005-gsber
         AND kostl = @wa_zaa005-kostl
         AND gjahr = @wa_zaa005-gjahr.
      ENDSELECT.

      IF sy-subrc = 0.
        wa_zaa005-zimob_v = w_0005-zimob_v.
        wa_zaa005-zimob_a = w_0005-zimob_a.
        wa_zaa005-zimob_p = w_0005-zimob_p.
        wa_zaa005-zimob_r = w_0005-zimob_r.
        wa_zaa005-zusap_4 = w_0005-zusap_4.
        wa_zaa005-zdtap_4 = w_0005-zdtap_4.
        wa_zaa005-zhrap_4 = w_0005-zhrap_4.
      ENDIF.

      MODIFY  zaa005  FROM wa_zaa005.

    ENDLOOP.

    MESSAGE 'Tabela ZAA005 foi atualizada!' TYPE 'S'.
  ENDIF.

ENDFORM.
