FUNCTION z_sd_impostos_cte.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_NFLIN) TYPE  J_1BNFLIN_TAB
*"  EXPORTING
*"     REFERENCE(E_OBSERVACAO) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: w_jnflin TYPE logbr_s_nf_item,
        l_obs    TYPE string.

  FREE: e_observacao, l_obs.

  LOOP AT it_nflin INTO w_jnflin.

    IF w_jnflin-taxlw1 IS NOT INITIAL.
      SELECT SINGLE *
        INTO @DATA(_1batl1t)
        FROM j_1batl1t
       WHERE langu  = @sy-langu
         AND taxlaw = @w_jnflin-taxlw1.

      IF sy-subrc = 0.
        IF _1batl1t-line1 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl1t-line1.
        ENDIF.
        IF _1batl1t-line2 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl1t-line2.
        ENDIF.
        IF _1batl1t-line3 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl1t-line3.
        ENDIF.
        IF _1batl1t-line4 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl1t-line4.
        ENDIF.
      ENDIF.
    ENDIF.

    IF w_jnflin-taxlw4 IS NOT INITIAL.
      SELECT SINGLE *
        INTO @DATA(_1batl4t)
        FROM j_1batl4t
       WHERE langu  = @sy-langu
         AND taxlaw = @w_jnflin-taxlw4.

      IF sy-subrc = 0.
        IF _1batl4t-line1 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl4t-line1.
        ENDIF.
        IF _1batl4t-line2 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl4t-line2.
        ENDIF.
        IF _1batl4t-line3 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl4t-line3.
        ENDIF.
        IF _1batl4t-line4 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl4t-line4.
        ENDIF.
      ENDIF.
    ENDIF.

    IF w_jnflin-taxlw5 IS NOT INITIAL.
      SELECT SINGLE *
        INTO @DATA(_1batl5t)
        FROM j_1batl5t
       WHERE langu  = @sy-langu
         AND taxlaw = @w_jnflin-taxlw5.

      IF sy-subrc = 0.
        IF _1batl5t-line1 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl5t-line1.
        ENDIF.
        IF _1batl5t-line2 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl5t-line2.
        ENDIF.
        IF _1batl5t-line3 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl5t-line3.
        ENDIF.
        IF _1batl5t-line4 IS NOT INITIAL.
          l_obs = l_obs && '-' && _1batl5t-line4.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  e_observacao = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = l_obs ) ).

ENDFUNCTION.
