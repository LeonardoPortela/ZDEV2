*&---------------------------------------------------------------------*
*& Include          ZMMR209_CLS
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      handle_on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no,

      handle_on_button_click2 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      on_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,


      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells,

      on_f4_2 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells.

ENDCLASS.                    "lcl_event_receiver DEFINITION

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_on_button_click.

    DATA: tl_texto       TYPE catsxt_longtext_itab,
          wl_texto       TYPE LINE OF catsxt_longtext_itab,
          wl_field       TYPE lvc_s_col,
          v_cont         TYPE i,
          c_x            TYPE c,
          wa_observacoes TYPE ty_observacoes,
          wl_name        TYPE thead-tdname,
          lt_lines       TYPE TABLE OF tline.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    REFRESH tl_texto.
    CLEAR:wl_texto.

    IF es_col_id EQ 'OBSERVACAO'.

      PERFORM f_monta_observacao USING es_row_no
                              CHANGING t_saida.

    ENDIF.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = is_table.

    CALL METHOD cl_gui_cfw=>dispatch.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.                    "HANDLE_ON_BUTTON_CLICK.

  METHOD handle_on_button_click2.

    DATA: tl_texto       TYPE catsxt_longtext_itab,
          wl_texto       TYPE LINE OF catsxt_longtext_itab,
          wl_field       TYPE lvc_s_col,
          v_cont         TYPE i,
          c_x            TYPE c,
          wa_observacoes TYPE ty_observacoes,
          wl_name        TYPE thead-tdname,
          lt_lines       TYPE TABLE OF tline.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    REFRESH tl_texto.
    CLEAR:wl_texto.

    IF es_col_id EQ 'OBSERVACAO'.

      PERFORM f_monta_observacao USING es_row_no
                              CHANGING t_saida_sol_item.

    ENDIF.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = is_table.

    CALL METHOD cl_gui_cfw=>dispatch.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.                    "HANDLE_ON_BUTTON_CLICK.

  METHOD on_data_changed.

    DATA: ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value,
          lv_sol        TYPE vbap-kwmeng,
          lv_roteiro    TYPE zsdt0132-nr_rot,
          lv_entrega_dt TYPE sydatum.

    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    DATA: lva_qtde_aux TYPE zmmt0196-solicitacao_qte.

    FIELD-SYMBOLS: <fs_field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_good>).

      READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <ls_good>-row_id.
      ASSIGN COMPONENT <ls_good>-fieldname OF STRUCTURE <fs_saida> TO <fs_field>.

      CASE <ls_good>-fieldname.


        WHEN 'ENTREGA_DT'.

          lv_entrega_dt = <ls_good>-value.

          IF lv_entrega_dt < sy-datum.

            CALL METHOD er_data_changed->modify_cell(
                i_row_id    = <ls_good>-row_id
                i_tabix     = <ls_good>-tabix
                i_fieldname = <ls_good>-fieldname
                i_value     = '' ).

            MESSAGE 'A Data de Entrega não pode ser no Passado!' TYPE 'S' DISPLAY LIKE 'E'.

          ENDIF.

        WHEN 'ROTA_PC'.

          IF <fs_saida>-parceiro_pc IS INITIAL.
            MESSAGE 'Favor preencher o parceiro de ponto de coleta' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_pc.
            RETURN.
          ENDIF.

          SELECT SINGLE nr_rot, rot_desc
            FROM zsdt0132
            INTO @DATA(ls_rt_pc)
            WHERE lifnr = @<fs_saida>-parceiro_pc
              AND nr_rot = @<ls_good>-value.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Rota não pertence ao parceiro ou é inválida!' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_pc.
          ELSE.
            <fs_saida>-rota_pc = ls_rt_pc-nr_rot.
            <fs_saida>-rota_pc_desc = ls_rt_pc-rot_desc.
          ENDIF.

        WHEN 'ROTA_LE'.

          IF <fs_saida>-parceiro_le IS INITIAL.
            MESSAGE 'Favor preencher o parceiro de local de entrega' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_le.
            RETURN.
          ENDIF.

          SELECT SINGLE nr_rot, rot_desc
            FROM zsdt0132
            INTO @DATA(ls_rt_le)
            WHERE lifnr = @<fs_saida>-parceiro_le
              AND nr_rot = @<ls_good>-value.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Rota não pertence ao parceiro ou é inválida!' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_le.
          ELSE.
            <fs_saida>-rota_le = ls_rt_le-nr_rot.
            <fs_saida>-rota_le_desc = ls_rt_le-rot_desc.
          ENDIF.

        WHEN 'SOLICITACAO_QTE'.
          DATA(lv_qtd_tot_sol) = <ls_good>-value.

          IF lv_qtd_tot_sol > <fs_saida>-saldo_a_solicitar.

            CALL METHOD er_data_changed->modify_cell(
                i_row_id    = <ls_good>-row_id
                i_tabix     = <ls_good>-tabix
                i_fieldname = 'SOLICITACAO_QTE'
                i_value     = 0 ).

            MESSAGE 'Quantidade solicitada é maior que a quantidade do Saldo à Solicitar!' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.

        WHEN 'SOLICITACAO_QTE_MANUAL'.

          DATA(lv_qtd_manual) = <ls_good>-value.

          IF lv_qtd_manual > <fs_saida>-solicitacao_qte.

            CALL METHOD er_data_changed->modify_cell(
                i_row_id    = <ls_good>-row_id
                i_tabix     = <ls_good>-tabix
                i_fieldname = 'SOLICITACAO_QTE_MANUAL'
                i_value     = 0 ).

            MESSAGE 'Quantidade manual é maior que a quantidade da solicitação!' TYPE 'S' DISPLAY LIKE 'E'.

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed2.

    DATA: ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value,
          lv_sol        TYPE vbap-kwmeng,
          lv_roteiro    TYPE zsdt0132-nr_rot,
          lv_entrega_dt TYPE sy-datum.

    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    FIELD-SYMBOLS: <fs_field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_good>).

      READ TABLE t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <ls_good>-row_id.
      ASSIGN COMPONENT <ls_good>-fieldname OF STRUCTURE <fs_saida> TO <fs_field>.

      CASE <ls_good>-fieldname.

        WHEN 'ENTREGA_DT'.

          lv_entrega_dt = <ls_good>-value.

          IF lv_entrega_dt < sy-datum.

            CALL METHOD er_data_changed->modify_cell(
                i_row_id    = <ls_good>-row_id
                i_tabix     = <ls_good>-tabix
                i_fieldname = <ls_good>-fieldname
                i_value     = '' ).

            MESSAGE 'A Data de Entrega não pode ser no Passado!' TYPE 'S' DISPLAY LIKE 'E'.

            RETURN.

          ENDIF.

          LOOP AT t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida_sol>) WHERE nro_sol = <fs_saida>-nro_sol.
            <fs_saida_sol>-entrega_dt = <ls_good>-value.
          ENDLOOP.

        WHEN 'ROTA_PC'.

          IF <fs_saida>-parceiro_pc IS INITIAL.
            MESSAGE 'Favor preencher o parceiro de ponto de coleta' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_pc.
            RETURN.
          ENDIF.

          SELECT nr_rot
            FROM zsdt0132
            INTO <fs_saida>-rota_pc
            UP TO 1 ROWS
            WHERE lifnr = <fs_saida>-parceiro_pc
              AND nr_rot = <ls_good>-value.
          ENDSELECT.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Rota não pertence ao parceiro ou é inválida!' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_pc.
          ENDIF.

        WHEN 'ROTA_LE'.

          IF <fs_saida>-parceiro_le IS INITIAL.
            MESSAGE 'Favor preencher o parceiro de local de entrega' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_le.
            RETURN.
          ENDIF.

          SELECT nr_rot
            FROM zsdt0132
            INTO <fs_saida>-rota_le
            UP TO 1 ROWS
            WHERE lifnr = <fs_saida>-parceiro_le
              AND nr_rot = <ls_good>-value.
          ENDSELECT.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Rota não pertence ao parceiro ou é inválida!' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: <ls_good>-value,
                   <fs_saida>-rota_le.
          ENDIF.

        WHEN 'SOLICITACAO_QTE'.

          CASE lv_direcao.
            WHEN '1'.
              IF <ls_good>-value > <fs_saida>-menge.
                MESSAGE 'Quantidade solicitada é maior que a quantidade do pedido!' TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

              IF <ls_good>-value < <fs_saida>-saldo_entregue_solic.

                CALL METHOD er_data_changed->modify_cell(
                    i_row_id    = <ls_good>-row_id
                    i_tabix     = <ls_good>-tabix
                    i_fieldname = 'SOLICITACAO_QTE'
                    i_value     = <fs_saida>-qtd_solicitada ).

                MESSAGE 'A quantidade informada não pode ser inferior a Quantidade Entregue!' TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

            WHEN '2'.
              IF <ls_good>-value > <fs_saida>-saldo_a_solicitar.

                CALL METHOD er_data_changed->modify_cell(
                    i_row_id    = <ls_good>-row_id
                    i_tabix     = <ls_good>-tabix
                    i_fieldname = 'SOLICITACAO_QTE'
                    i_value     = <fs_saida>-solicitacao_qte ).

                MESSAGE 'Quantidade solicitada é maior que a quantidade do Saldo do pedido!' TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

              IF <ls_good>-value < <fs_saida>-saldo_entregue_solic.

                CALL METHOD er_data_changed->modify_cell(
                    i_row_id    = <ls_good>-row_id
                    i_tabix     = <ls_good>-tabix
                    i_fieldname = 'SOLICITACAO_QTE'
                    i_value     = <fs_saida>-solicitacao_qte ).

                MESSAGE 'A quantidade informada não pode ser inferior a Quantidade Entregue!' TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

            WHEN OTHERS.
          ENDCASE.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_f4.

    SET PARAMETER ID 'LIF' FIELD space.

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.

      CASE e_fieldname.
        WHEN 'ROTA_PC'.

          IF <fs_saida>-parceiro_pc IS INITIAL.
            MESSAGE 'Necessário preencher o campo Parceiro Ponto de Coleta' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          SET PARAMETER ID 'LIF' FIELD <fs_saida>-parceiro_pc.

        WHEN 'ROTA_LE'.

          IF <fs_saida>-parceiro_le IS INITIAL.
            MESSAGE 'Necessário preencher o campo Parceiro Local de Entrega' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          SET PARAMETER ID 'LIF' FIELD <fs_saida>-parceiro_le.

        WHEN OTHERS.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD on_f4_2.

    SET PARAMETER ID 'LIF' FIELD space.

    READ TABLE t_saida_sol_item ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.

      CASE e_fieldname.
        WHEN 'ROTA_PC'.

          IF <fs_saida>-parceiro_pc IS INITIAL.
            MESSAGE 'Necessário preencher o campo Parceiro Ponto de Coleta' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          SET PARAMETER ID 'LIF' FIELD <fs_saida>-parceiro_pc.

        WHEN 'ROTA_LE'.

          IF <fs_saida>-parceiro_le IS INITIAL.
            MESSAGE 'Necessário preencher o campo Parceiro Local de Entrega' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          SET PARAMETER ID 'LIF' FIELD <fs_saida>-parceiro_le.

        WHEN OTHERS.

      ENDCASE.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
