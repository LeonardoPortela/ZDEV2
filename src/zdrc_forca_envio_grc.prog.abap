*&---------------------------------------------------------------------*
*& Include ZDRC_FORCA_ENVIO_GRC
*&---------------------------------------------------------------------*

FORM f_enviar_grc CHANGING p_cust3 TYPE j_1bnfe_cust3.

  SELECT SINGLE *
    FROM tvarvc
    INTO @DATA(w_tvarv1)
   WHERE name = 'DESTINATION_GRC'.

  CHECK sy-subrc = 0.

  CHECK p_cust3-rfcdest <> w_tvarv1-low.

*---------------------------
*- verifica se filial esta cadastrana
*---------------------------
  SELECT SINGLE *
    FROM tvarvc
    INTO @DATA(w_tvarv2)
   WHERE name = 'BRANCH_DRC'
     AND low  = @p_cust3-branch.

  CHECK sy-subrc <> 0.

  SELECT SINGLE *
    FROM j_1bnfe_cust3
    INTO @DATA(w_cust3)
   WHERE bukrs   = @p_cust3-bukrs
     AND branch  = @p_cust3-branch
     AND model   = @p_cust3-model
     AND rfcdest = @w_tvarv1-low.

  IF sy-subrc = 0.
    p_cust3         = w_cust3.
  ELSE.
    p_cust3-rfcdest = w_tvarv1-low.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
