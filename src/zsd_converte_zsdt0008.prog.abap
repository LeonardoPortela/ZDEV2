*&---------------------------------------------------------------------*
*& Report ZSD_CONVERTE_ZSDT0008
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_converte_zsdt0008.

DATA: l_chmod TYPE zch_modelo,
      w_0370  TYPE zsdt0370.

DELETE FROM zsdt0370.

SELECT *
  FROM zsdt0008
  INTO TABLE @DATA(t_0008).

LOOP AT t_0008 INTO DATA(w_0008).
  TRY.
      w_0370-ch_modelo = zcl_impostos=>get_ch_modelo( ).
    CATCH zcx_error INTO DATA(ex_error).
      MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO DATA(l_mesg).
  ENDTRY.

  MOVE-CORRESPONDING w_0008  TO w_0370.
  MOVE w_0008-bukrs          TO w_0370-bukrs_toma.
  MODIFY zsdt0370          FROM w_0370.

  COMMIT WORK.
ENDLOOP.
