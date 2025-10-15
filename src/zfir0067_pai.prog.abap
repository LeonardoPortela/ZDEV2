*----------------------------------------------------------------------*
***INCLUDE ZFIR0067_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF obj_alv_mov_flx IS NOT INITIAL.
        CALL METHOD obj_alv_mov_flx->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: obj_alv_mov_flx.
      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF g_tree IS NOT INITIAL.
        CALL METHOD g_tree->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: g_tree.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN: 'PSQ_V1'.
      PERFORM pesq_versao USING '1'.
    WHEN: 'PSQ_V2'.
      PERFORM pesq_versao USING '2'.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF obj_alv_var_v1 IS NOT INITIAL.
        CALL METHOD obj_alv_var_v1->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: obj_alv_var_v1.
      ENDIF.

      IF obj_alv_var_v2 IS NOT INITIAL.
        CALL METHOD obj_alv_var_v2->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: obj_alv_var_v2.
      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0104 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0105 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0106  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0106 INPUT.

  CASE sy-ucomm.
    WHEN 'REP_FLX'.
      PERFORM processar_fluxo.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0107 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      PERFORM gravar_ajustes.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.


MODULE user_command_0108 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRMAR'.

      IF wg_coment_ajuste IS NOT INITIAL.
        MODIFY zfit0149 FROM wg_coment_ajuste.
        MESSAGE 'Comentário incluído/alterado com sucesso!' TYPE 'S'.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.

  CASE vg_ok_0110.

    WHEN 'CONS'.

      "Tabela Detalhe Resumo Fluxo Caixa Prev. - Fluxo Financeiro
      IF s_bukvrs IS NOT INITIAL OR s_dtvrs  IS NOT INITIAL.

        SELECT dt_base_versao bukrs versao hora_versao FROM zfit0111
          APPENDING CORRESPONDING FIELDS OF TABLE t_list_vrs
          WHERE bukrs         IN s_bukvrs
          AND dt_base_versao  IN s_dtvrs.

        SORT t_list_vrs BY bukrs dt_base_versao versao.
        DELETE ADJACENT DUPLICATES FROM t_list_vrs COMPARING bukrs dt_base_versao versao.

        "Descrição da empresa
        SELECT bukrs, butxt FROM t001
         INTO TABLE  @DATA(t_t001)
          FOR ALL ENTRIES IN @t_list_vrs
          WHERE bukrs = @t_list_vrs-bukrs.

        LOOP AT t_list_vrs ASSIGNING FIELD-SYMBOL(<fs_list_vrs>).

          READ TABLE t_t001 INTO DATA(w_t001) WITH KEY bukrs = <fs_list_vrs>-bukrs.
          IF sy-subrc IS INITIAL.
            <fs_list_vrs>-butxt = w_t001-butxt.
          ENDIF.

        ENDLOOP.

      ENDIF.

      CALL METHOD v_grid_110->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN 'CANC'.

      SET SCREEN 0.

  ENDCASE.
ENDMODULE.
