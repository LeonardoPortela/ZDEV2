*----------------------------------------------------------------------*
***INCLUDE LZGFS_INT_OB_CRTF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_integrar_parceiros
*&---------------------------------------------------------------------*
FORM f_integrar_parceiros USING p_zsdt0132 TYPE zsdt0132
                                p_somente_cadastrar.

  ls_parceiro-parceiro      = COND #( WHEN p_zsdt0132-kunnr IS NOT INITIAL THEN p_zsdt0132-kunnr ELSE p_zsdt0132-lifnr ).
  ls_parceiro-metodo        = 'GET'.
  ls_parceiro-tipo_parceiro = COND #( WHEN p_zsdt0132-kunnr IS NOT INITIAL THEN 'C' ELSE 'F' ).
  ls_parceiro-nr_rot        = p_zsdt0132-nr_rot.

  TRY.
      zcl_int_ob_safra_crt_contact=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_parceiro ).
      ls_parceiro-metodo = 'PUT'.
    CATCH zcx_integracao INTO DATA(ex_integra).
      ls_parceiro-metodo = 'POST'.
    CATCH zcx_error      INTO DATA(ex_error).
      ls_parceiro-metodo = 'POST'.
  ENDTRY.

  IF p_somente_cadastrar = abap_false OR ls_parceiro-metodo = 'POST'.
    DO 3 TIMES.
      TRY.
          zcl_int_ob_safra_crt_contact=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_parceiro ).
          lv_erro = abap_false.
          RETURN.
        CATCH zcx_integracao INTO ex_integra.
          lv_erro = abap_true.
          WAIT UP TO 5 SECONDS.
        CATCH zcx_error      INTO ex_error.
          lv_erro = abap_true.
          WAIT UP TO 5 SECONDS.
      ENDTRY.
    ENDDO.

    IF lv_erro = abap_true.
      RAISE erro_integracao.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_integrar_endercos
*&---------------------------------------------------------------------*
FORM f_integrar_enderecos USING p_zsdt0132 TYPE zsdt0132
                                p_somente_cadastrar.

  ls_parceiro-parceiro      = COND #( WHEN p_zsdt0132-kunnr IS NOT INITIAL THEN p_zsdt0132-kunnr ELSE p_zsdt0132-lifnr ).
  ls_parceiro-metodo        = 'GET'.
  ls_parceiro-tipo_parceiro = COND #( WHEN p_zsdt0132-kunnr IS NOT INITIAL THEN 'C' ELSE 'F' ).
  ls_parceiro-nr_rot        = p_zsdt0132-nr_rot.

  TRY.
      zcl_int_ob_safra_crt_conta_adr=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_parceiro ).
      ls_parceiro-metodo = 'PUT'.
    CATCH zcx_integracao INTO DATA(ex_integra).
      ls_parceiro-metodo = 'POST'.
    CATCH zcx_error      INTO DATA(ex_error).
      ls_parceiro-metodo = 'POST'.
  ENDTRY.

* IF P_somente_cadastrar = abap_false OR ls_parceiro-metodo = 'POST'.
  IF ls_parceiro-metodo = 'POST'.
    DO 3 TIMES.
      TRY.
          zcl_int_ob_safra_crt_conta_adr=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_parceiro ).
          lv_erro = abap_false.
          RETURN.
        CATCH zcx_integracao INTO ex_integra.
          lv_erro = abap_true.
          WAIT UP TO 5 SECONDS.
        CATCH zcx_error      INTO ex_error.
          lv_erro = abap_true.
          WAIT UP TO 5 SECONDS.
      ENDTRY.
    ENDDO.

    IF lv_erro = abap_true.
      RAISE erro_integracao.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
