FUNCTION zdue_consulta_nomeacao_sol_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NRO_SOL_OV) TYPE  ZSDED013
*"     REFERENCE(I_NUMERO_RUC) TYPE  ZDE_NUMERO_RUC OPTIONAL
*"  EXPORTING
*"     VALUE(E_DATA_BL) TYPE  SY-DATUM
*"----------------------------------------------------------------------

  RANGES: lra_numero_ruc FOR zsdt0053-numero_ruc.

  DATA: lwa_zsdt0051 TYPE zsdt0051.

  DATA: lit_zsdt0053_aux TYPE TABLE OF zsdt0053.

  CLEAR: lra_numero_ruc[], lwa_zsdt0051, git_zsdt0053[].

  IF i_nro_sol_ov IS INITIAL.
    EXIT.
  ENDIF.

  IF i_numero_ruc IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = i_numero_ruc ) TO lra_numero_ruc.
  ENDIF.

  SELECT *
    FROM zsdt0053 INTO TABLE git_zsdt0053
   WHERE nro_sol_ov EQ i_nro_sol_ov
     AND numero_ruc IN lra_numero_ruc.

  IF git_zsdt0053[] IS INITIAL.

    IF i_numero_ruc IS NOT INITIAL.
      DATA(_msg_ruc) = 'RUC' && i_numero_ruc.
    ENDIF.

    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0051 INTO lwa_zsdt0051
   WHERE nro_sol_ov EQ i_nro_sol_ov.

  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CLEAR: gwa_nomeacao_sol_ov.

  gwa_nomeacao_sol_ov-numero_ruc = i_numero_ruc.

  READ TABLE git_zsdt0053 INTO DATA(lwa_zsdt0053) INDEX 1.

  gwa_nomeacao_sol_ov-id_nomeacao_tran = lwa_zsdt0053-id_nomeacao_tran.

  IF lwa_zsdt0053-id_nomeacao_tran IS NOT INITIAL.

    SELECT SINGLE *
      FROM znom_transporte INTO @DATA(lwa_znom_transpor)
     WHERE id_nomeacao_tran EQ @lwa_zsdt0053-id_nomeacao_tran.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    gwa_nomeacao_sol_ov-ds_nome_transpor = lwa_znom_transpor-ds_nome_transpor.
    gwa_nomeacao_sol_ov-booking          = lwa_znom_transpor-booking.

    SELECT SINGLE *
      FROM znom_conhec INTO @DATA(lwa_nom_conhec)
     WHERE id_nomeacao_tran EQ @lwa_znom_transpor-id_nomeacao_tran.

    IF sy-subrc EQ 0.
      gwa_nomeacao_sol_ov-id_conhec = lwa_nom_conhec-id_conhec.
      gwa_nomeacao_sol_ov-dt_data   = lwa_nom_conhec-dt_data.
      gwa_nomeacao_sol_ov-nr_conhec = lwa_nom_conhec-nr_conhec.
    ENDIF.

  ENDIF.

  E_DATA_BL = gwa_nomeacao_sol_ov-dt_data.

ENDFUNCTION.
