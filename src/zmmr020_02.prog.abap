*&---------------------------------------------------------------------*
*& Report  ZMMR020_02
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr020_02.

FORM estorno_destinacao
  USING p_wa_mat_doc TYPE mblnr
        p_doc_year TYPE mjahr
  CHANGING sy_subrc TYPE sy-subrc
           p_wa_mat_doc_out TYPE mblnr
           p_doc_year_out TYPE mjahr
           vg_erro TYPE char01
           it_return TYPE bapiret2_t.

  SELECT SINGLE * INTO @DATA(wa_zmmt0114)
    FROM zmmt0114
   WHERE mblnr EQ @p_wa_mat_doc
     AND mjahr EQ @p_doc_year.

  sy_subrc = sy-subrc.

  IF sy-subrc IS INITIAL.

    DATA: r_estornou TYPE char01,
          r_retorno	 TYPE bapiret2_t.

    TRY .
        DATA(lc_retorno) =
          zcl_factory_mat_destinacao=>zif_factory_mat_destinacao~get_instance(
            )->set_factory_objeto( EXPORTING i_id_destinacao = wa_zmmt0114-id_destinacao
            )->get_factory_objeto(
            )->set_registro( i_id_destinacao = wa_zmmt0114-id_destinacao
            )->set_estornar_movimento( IMPORTING e_estornou = r_estornou e_retorno = r_retorno
            ).

        IF r_estornou EQ abap_true.

          SELECT SINGLE * INTO @wa_zmmt0114
            FROM zmmt0114
           WHERE mblnr EQ @p_wa_mat_doc
             AND mjahr EQ @p_doc_year.

          p_wa_mat_doc_out = wa_zmmt0114-mblnr_estorno.
          p_doc_year_out = wa_zmmt0114-mjahr_estorno.
          CLEAR: vg_erro.

        ELSE.
          vg_erro = abap_true.
          it_return[] = r_retorno.
        ENDIF.

      CATCH zcx_material_destinacao.    "

        it_return[] = r_retorno.
        vg_erro = abap_true.

    ENDTRY.

  ENDIF.

ENDFORM.
