*----------------------------------------------------------------------*
***INCLUDE LZPMF0003F02.
*----------------------------------------------------------------------*

FORM f_preenche_numero_equipe.

  IF zpmt0021-centro IS INITIAL OR zpmt0021-dsequipe IS INITIAL.
    MESSAGE 'Favor preencher todos campos obrigatórios' TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZZEQUIPE'
    IMPORTING
      number                  = zpmt0021-idequipe
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.

  ENDIF.


ENDFORM.

FORM f_valida_delete.

  MESSAGE 'Não é permitido deletar registros' TYPE 'E'.

ENDFORM.

FORM f_envia_dados_mobman.
  DATA: ls_equipe TYPE zpme_equipe,
        lv_action TYPE c,
        lv_qtd    TYPE sy-tabix.

  LOOP AT extract.

    MOVE-CORRESPONDING <vim_extract_struc> TO ls_equipe.

    IF ls_equipe-centro IS INITIAL.
      CONTINUE.
    ENDIF.

    lv_qtd = strlen( extract ).
    lv_qtd = lv_qtd - 1.
    lv_action = extract+lv_qtd(1).

    IF lv_action NS 'U' AND lv_action NS 'N' AND lv_action NS 'M' .
      CONTINUE.
    ENDIF.

    TRY .
        zcl_int_ob_envia_equipe_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_equipe ).
      CATCH zcx_integracao INTO DATA(zcx_integracao).
*      _error = abap_true.
        MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
         NUMBER zcx_integracao->zif_error~msgno
           WITH zcx_integracao->zif_error~msgv1
                zcx_integracao->zif_error~msgv2
                zcx_integracao->zif_error~msgv3
                zcx_integracao->zif_error~msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
*      _error = abap_true.
        MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
         NUMBER zcx_error->zif_error~msgno
           WITH zcx_error->zif_error~msgv1
                zcx_error->zif_error~msgv2
                zcx_error->zif_error~msgv3
                zcx_error->zif_error~msgv4.

    ENDTRY.

  ENDLOOP.

ENDFORM.
