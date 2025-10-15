FUNCTION zsd_exibe_ordem_carregamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ID_ORDEM) TYPE  ZDE_ID_ORDEM
*"----------------------------------------------------------------------

  FREE: zsdt0001od,
        zlest0185,
        w_texto.

*--------------------------------------
* seleao zsdt0001od
*--------------------------------------
  SELECT *
    FROM zsdt0001od
    INTO zsdt0001od
      UP TO 1 ROWS
   WHERE id_ordem = i_id_ordem.
  ENDSELECT.

*--------------------------------------
* seleao zlest0185
*--------------------------------------
  SELECT *
    FROM zlest0185
    INTO zlest0185
      UP TO 1 ROWS
   WHERE id_ordem = i_id_ordem.
  ENDSELECT.

*--------------------------------------
* dados adicionais
*--------------------------------------
  SELECT butxt
    FROM t001
    INTO w_texto-t001_butxt_rec
      UP TO 1 ROWS
   WHERE bukrs = zsdt0001od-id_bukrs.
  ENDSELECT.

  SELECT name
    FROM j_1bbranch
    INTO w_texto-j_1bbranch_name_rec
      UP TO 1 ROWS
   WHERE bukrs  = zsdt0001od-id_bukrs
     AND branch = zsdt0001od-id_branch.
  ENDSELECT.

  SELECT butxt
    FROM t001
    INTO w_texto-t001_butxt_age
      UP TO 1 ROWS
   WHERE bukrs = zsdt0001od-id_bukrs_ag.
  ENDSELECT.

  SELECT name
    FROM j_1bbranch
    INTO w_texto-j_1bbranch_name_age
      UP TO 1 ROWS
   WHERE bukrs  = zsdt0001od-id_bukrs_ag
     AND branch = zsdt0001od-id_branch_ag.
  ENDSELECT.

  SELECT name1
    FROM lfa1
    INTO w_texto-lfa1_name1_coleta
      UP TO 1 ROWS
   WHERE lifnr = zsdt0001od-id_local_coleta.
  ENDSELECT.

  SELECT name1
    FROM lfa1
    INTO w_texto-lfa1_name1_destino
      UP TO 1 ROWS
   WHERE lifnr = zsdt0001od-id_local_destino.
  ENDSELECT.

  SELECT name1
    FROM kna1
    INTO w_texto-kna1_name1_descarga
      UP TO 1 ROWS
   WHERE kunnr = zsdt0001od-id_local_descarga.
  ENDSELECT.

  SELECT maktx
    FROM makt
    INTO w_texto-mara_maktx
      UP TO 1 ROWS
   WHERE matnr = zsdt0001od-id_produto
     AND spras = sy-langu.
  ENDSELECT.

  SELECT name1
    FROM lfa1
    INTO w_texto-lfa1_name1_motorista
      UP TO 1 ROWS
   WHERE lifnr = zsdt0001od-id_motorista.
  ENDSELECT.

*--------------------------------------
* exibe informacoes
*--------------------------------------
  CALL SCREEN 100 STARTING AT 08  01
                    ENDING AT 182 23.

ENDFUNCTION.
