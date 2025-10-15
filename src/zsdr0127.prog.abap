*&---------------------------------------------------------------------*
*& Report  ZSDR0127
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0127.
TABLES zsdt0090.

SELECT-OPTIONS s_vbeln FOR zsdt0090-vbeln NO INTERVALS .

DATA: c_trava_cambio TYPE  c,
      c_taxa         TYPE  kursf,
      c_valdt        TYPE  zsdt0090-valdt,
      vl_proc_ov     TYPE c,
      vl_vbeln_aux   TYPE vbeln,
      it_90          TYPE TABLE OF zsdt0090.


IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    WRITE 'JA EXISTE UM JOB EM EXECUÇÃO'.
    EXIT.
  ENDIF.
ENDIF.

SELECT *
  FROM zsdt0090
  INTO TABLE @DATA(it_0090)
  WHERE categoria EQ 'C'
  AND estorno EQ @abap_false
  AND vbelv IN @s_vbeln.

CHECK sy-subrc IS INITIAL.

LOOP AT it_0090 INTO DATA(wa_0090).

  c_taxa         = wa_0090-kurrf.
  c_valdt        = wa_0090-valdt.

  c_trava_cambio = abap_true.

  vl_vbeln_aux    = wa_0090-vbelv.
  vl_proc_ov      = abap_true.

* verifico se o vbelv tem filho
  SELECT  *
    FROM zsdt0090
    INTO TABLE @DATA(it_0090_1)
    WHERE vbelv EQ @vl_vbeln_aux
      AND vbeln NE ' '
      AND categoria NE 'C'
    AND estorno EQ @abap_false.

  CHECK sy-subrc IS INITIAL.

  FREE: it_90.

  APPEND LINES OF it_0090_1 TO it_90.

  WHILE vl_proc_ov EQ abap_true.

    SELECT  *
      FROM zsdt0090 AS a
      INTO TABLE @DATA(it_0090_2)
      FOR ALL ENTRIES IN @it_0090_1
      WHERE a~vbelv EQ @it_0090_1-vbeln
        AND a~vbeln NE ' '
        AND a~categoria NE 'C'
        AND a~vbeln     NE a~vbelv
        AND a~estorno EQ @abap_false.

    IF sy-subrc IS INITIAL.
      APPEND LINES OF it_0090_2 TO it_90.
      it_0090_1 = it_0090_2.
    ELSE.
      vl_proc_ov = abap_false.
    ENDIF.

  ENDWHILE.

  SORT it_90 BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_90 COMPARING vbeln.
  DELETE it_90 WHERE vbeln EQ ' '.

  SELECT  *
    FROM zsdt0090 AS a
    INTO TABLE @DATA(it_0090_3)
    FOR ALL ENTRIES IN @it_90
    WHERE a~vbelv EQ @it_90-vbeln
      AND a~categoria EQ 'C'
      AND a~vbeln     NE a~vbelv
      AND a~estorno EQ @abap_false.

  LOOP AT it_90 INTO DATA(wa_90).
    IF line_exists( it_0090_3[ vbelv = wa_90-vbeln ] ).
      CONTINUE.
    ELSE.
      PERFORM insert_90.
    ENDIF.
  ENDLOOP.

ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  INSERT_90
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_90_VBELN  text
*----------------------------------------------------------------------*
FORM insert_90.

  DATA: w_ins_90 TYPE zsdt0090,
        seq      TYPE zsdt0090-sequencia.

  CLEAR: seq.

  SELECT COUNT(*)
      FROM zsdt0090
        INTO seq
          WHERE doc_simulacao EQ wa_90-doc_simulacao.

  ADD 1 TO seq.

  w_ins_90-sequencia     = seq.
  w_ins_90-doc_simulacao = wa_90-doc_simulacao.
  w_ins_90-vbelv         = wa_90-vbeln.
  w_ins_90-usnam         = sy-uname.
  w_ins_90-data_atual    = sy-datum.
  w_ins_90-hora_atual    = sy-uzeit.
  w_ins_90-auartv        = wa_90-auart.
  w_ins_90-spartv        = wa_90-spart.
  w_ins_90-chargv        = wa_90-charg.
  w_ins_90-matklv        = wa_90-matkl.
  w_ins_90-inco1v        = wa_90-inco1.
  w_ins_90-werksv        = wa_90-werks.
  w_ins_90-kunnrv        = wa_90-kunnr.
  w_ins_90-categoria     = 'C'.
  w_ins_90-kurrf         = c_taxa.
  w_ins_90-valdt         = c_valdt.

  MODIFY zsdt0090 FROM w_ins_90.
  COMMIT WORK.

ENDFORM.
