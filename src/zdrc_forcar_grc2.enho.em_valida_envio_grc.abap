METHOD valida_envio_grc .

  DATA: lva_no_check_use_drc TYPE c.

  IMPORT lva_no_check_use_drc TO lva_no_check_use_drc FROM MEMORY ID 'Z_NO_CHECK_USE_BRANCH_DRC'.
  DELETE FROM MEMORY ID 'Z_NO_CHECK_USE_BRANCH_DRC'.

  CHECK lva_no_check_use_drc EQ abap_false.

  FREE: es_envia_grc.

*---------------------------
*- verifica se filial esta cadastrana
*---------------------------
  SELECT SINGLE *
    FROM tvarvc
    INTO @DATA(w_tvarv2)
   WHERE name = 'BRANCH_DRC'
     AND low  = @is_nf-branch.

  IF sy-subrc <> 0.
    es_envia_grc = abap_true.
  ENDIF.

ENDMETHOD.
