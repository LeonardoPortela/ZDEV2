FUNCTION zsd_busca_itinerario.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ROTEIRO) TYPE  ZSDT0132-NR_ROT
*"     REFERENCE(I_VBELN) TYPE  VBAK-VBELN
*"  EXPORTING
*"     REFERENCE(E_ROUTE) TYPE  TROLZ-ROUTE
*"     REFERENCE(E_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------
  DATA(lv_erro) = abap_false.

  SELECT SINGLE lzone FROM zsdt0132 WHERE nr_rot = @i_roteiro INTO @DATA(lv_lzone).
  IF sy-subrc = 0 and lv_lzone is NOT INITIAL.

    SELECT SINGLE lzone FROM vbpa
    WHERE vbeln = @i_vbeln
      and parvw = 'PC'
    INTO @DATA(lv_vbpa_lzone).
    IF sy-subrc = 0.

      SELECT route
      FROM trolz
      WHERE azone = @lv_vbpa_lzone
        AND lzone = @lv_lzone
        INTO @DATA(lv_route) UP TO 1 ROWS .
      ENDSELECT.

      IF sy-subrc  = 0.
        e_route = lv_route.

      ELSE.
        lv_erro = abap_true.
      ENDIF.

    ELSE.
      lv_erro = abap_true.
    ENDIF.

  ELSE.
    lv_erro = abap_true.
  ENDIF.

  IF lv_erro = abap_true.
    e_return-type = 'E'.
    e_return-message = 'Itinerário não encontrado.'.

  ELSE.

    CLEAR e_return.

  ENDIF.

ENDFUNCTION.
