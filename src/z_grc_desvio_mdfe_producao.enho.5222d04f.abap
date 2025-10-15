"Name: \FU:J_1B_NFE_CHECK_ACTIVE_SERVER\SE:BEGIN\EI
ENHANCEMENT 0 Z_GRC_DESVIO_MDFE_PRODUCAO.

  CHECK is_branch_info-model <> '58'.

  SELECT SINGLE *
    FROM zdrct0005
    INTO @DATA(_zdrct0005)
   WHERE regio  = @is_branch_info-regio
     AND model  = @is_branch_info-model.

  IF sy-subrc = 0.
    RETURN.
  ENDIF.
*
ENDENHANCEMENT.
