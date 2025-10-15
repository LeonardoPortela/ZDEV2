*&---------------------------------------------------------------------*
*& Report  ZMMR131
*&
*&---------------------------------------------------------------------*
*&
*& Eliminar reservas com mais de 10 dias sem atendimento (JOB).
*&---------------------------------------------------------------------*
REPORT zmmr131.

DATA: var_data   TYPE sy-datum,
      var_data_i TYPE sy-datum,
      w_elimresb TYPE zelimresb,
      w_elim     TYPE char01.

"RESERVAS
DATA: res_items LIKE bapiresb OCCURS 0.
DATA: wa_res_items LIKE LINE OF res_items.
DATA: it_change LIKE bapi2093_res_item_change OCCURS 0 WITH HEADER LINE.
DATA: it_changex LIKE bapi2093_res_item_changex OCCURS 0 WITH HEADER LINE.
DATA: it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
DATA: t_elimresb  TYPE TABLE OF zelimresb WITH HEADER LINE.


"REQUISIÇÃO
DATA:
  ec_bapieband  LIKE bapieband OCCURS 0 WITH HEADER LINE,
  ec_bapireturn LIKE bapireturn OCCURS 0 WITH HEADER LINE.

DATA :
  lt_return  TYPE TABLE OF bapiret2         WITH HEADER LINE,
  lt_pr_itm  TYPE TABLE OF bapimereqitemimp WITH HEADER LINE,
  lt_pr_itmx TYPE TABLE OF bapimereqitemx   WITH HEADER LINE.


TABLES: zelimresb.

START-OF-SELECTION.
  SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
    FROM tbtco
   WHERE jobname EQ 'ELIMINA_RESERVA_MM'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    PERFORM seleciona_dados.
    PERFORM seleciona_dados_rc.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .

  var_data_i = sy-datum - 20000.
  var_data   = sy-datum - 10.
* ---> CS1130518 / IR147882 ---->
  SELECT *
   FROM zelimresb
   INTO TABLE t_elimresb
   WHERE atualiza NE 'X'.
* <--- CS1130518 / IR147882 <----
  IF t_elimresb[] IS INITIAL.








    SELECT *
      FROM resb
      INTO TABLE @DATA(it_resb)
      WHERE kzear NE 'X'
      AND   xloek NE 'X'
      AND   matnr NE ' '
      AND   ( bdter >= @var_data_i AND bdter <= @var_data  ).

    LOOP AT it_resb INTO DATA(wa_resb).
      REFRESH: it_change, it_changex, it_return.
      it_change-res_item = wa_resb-rspos.
      it_change-delete_ind = 'X'.
      APPEND it_change.

      it_changex-res_item = wa_resb-rspos.
      it_changex-delete_ind = 'X'.














      APPEND it_changex.


      CALL FUNCTION 'BAPI_RESERVATION_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          reservation               = wa_resb-rsnum
        TABLES
          reservationitems_changed  = it_change
          reservationitems_changedx = it_changex
          return                    = it_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      WRITE:/ 'Reserva->',wa_resb-rsnum, wa_resb-rspos.
      LOOP AT it_return.
        IF it_return-type = 'E'.
          WRITE:/ it_return-message.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ELSE.
* ---> CS1130518 / IR147882 ---->
    w_elim = 'X'.
    LOOP AT t_elimresb INTO w_elimresb.
      UPDATE resb SET
          xloek = w_elimresb-xloek
          kzear = w_elimresb-kzear
       WHERE rsnum EQ w_elimresb-rsnum
         AND rspos EQ w_elimresb-rspos.

      IF sy-subrc EQ 0.
        UPDATE zelimresb SET
           atualiza = 'X'
        WHERE rsnum EQ w_elimresb-rsnum
          AND rspos EQ w_elimresb-rspos.
      ENDIF.
    ENDLOOP.
    COMMIT WORK.
* <--- CS1130518 / IR147882 <----
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_RC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_rc .


  CHECK w_elim EQ ''.

  var_data_i = sy-datum - 20000.
  var_data   = sy-datum - 120.

  SELECT *
      FROM eban
    INTO TABLE @DATA(it_eban)
    WHERE  loekz NE 'X'
    AND    ebakz NE 'X'
    AND    knttp NE 'F'
    AND    bsart IN ( 'RCS', 'NB', 'ZDBP', 'REG' )
    AND    eban~menge GT eban~bsmng
    AND   ( badat GE @var_data_i AND badat LE @var_data  ).


  LOOP AT it_eban INTO DATA(wa_eban).
    IF    ( wa_eban-menge - wa_eban-bsmng ) > 0 AND wa_eban-bsmng = 0. "pendente total elimina
      REFRESH: ec_bapieband,ec_bapireturn.
      ec_bapieband-preq_item  = wa_eban-bnfpo.
      ec_bapieband-delete_ind = 'X'.
      ec_bapieband-closed     = 'X'.
      APPEND ec_bapieband.
      CALL FUNCTION 'BAPI_REQUISITION_DELETE'
        EXPORTING
          number                      = wa_eban-banfn
        TABLES
          requisition_items_to_delete = ec_bapieband
          return                      = ec_bapireturn
        EXCEPTIONS
          OTHERS                      = 1.

      CLEAR : ec_bapireturn.
      READ TABLE ec_bapireturn WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      WRITE:/ 'Requisição->',wa_eban-banfn, wa_eban-bnfpo.
      LOOP AT ec_bapireturn.
        IF ec_bapireturn-type = 'E'.
          WRITE:/ ec_bapireturn-message.
        ENDIF.
      ENDLOOP.
    ELSEIF ( wa_eban-menge - wa_eban-bsmng ) > 0 AND wa_eban-bsmng > 0. "parcial remessa final
      REFRESH: lt_return,lt_pr_itm,lt_pr_itmx.
      lt_pr_itm-preq_item  = wa_eban-bnfpo.
      lt_pr_itm-closed     = 'X'.
      APPEND lt_pr_itm.
      "
      lt_pr_itmx-preq_item = wa_eban-bnfpo.
      lt_pr_itmx-closed    = 'X'.
      APPEND lt_pr_itmx.
      CALL FUNCTION 'BAPI_PR_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number  = wa_eban-banfn
        TABLES
          return  = lt_return
          pritem  = lt_pr_itm
          pritemx = lt_pr_itmx.

      CLEAR : lt_return.
      READ TABLE lt_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      WRITE:/ 'Requisição->',wa_eban-banfn, wa_eban-bnfpo.
      LOOP AT lt_return.
        IF lt_return-type = 'E'.
          WRITE:/ lt_return-message.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
