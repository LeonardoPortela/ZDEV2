*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato &*
*&                                    |Compra.                        &*
*&                                    |Chamado: 168919 2ª Parte.      &*
*&--------------------------------------------------------------------&*
**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  CLEAR ok_code.

  SET PF-STATUS 'ZLESR0152'.
  SET TITLEBAR 'ZLESR0152'.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

* CASE ok_code.
*    WHEN 'VARIANTE'.
*      PERFORM f_get_variant.
*
*    WHEN 'SAVE'.
*      PERFORM f_save_variant.
*
*    WHEN 'SELECAO'.
*      PERFORM f_selecao_dados CHANGING l_erro.
*
*      IF l_erro = abap_false.
*        CASE abap_true.
*          WHEN p_sintet.
*            PERFORM f_processa_dados_sintetico.
*          WHEN p_analit.
*            PERFORM f_processa_dados_analitico.
*        ENDCASE.
*
*        CALL SCREEN 0300.
*      ENDIF.
*
*    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
*      LEAVE TO SCREEN 0.
*
* ENDCASE.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  CLEAR ok_code2.
  SET PF-STATUS 'ZSDR0145_SCREEN'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  l_editar = abap_false.

  CASE ok_code2.
    WHEN 'SAVE'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CLEAR: rsvar-variant, rsvar-vtext.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0300 OUTPUT.

  CLEAR ok_code.

  IF g_popup = abap_false.
    SET PF-STATUS 'ZSDR0150'.
    SET TITLEBAR 'ZLESR0152'.
  ELSE.
    SET PF-STATUS 'INSUMOSF01'.
    SET TITLEBAR 'INSUMOSF01'.
  ENDIF.

  IF g_popup = abap_false.
    PERFORM f_init_alv.
  ELSE.
    PERFORM f_init_alv_pop.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  STATUS_0400  OUTPUT
**********************************************************************
MODULE status_0400 OUTPUT.

  FREE: t_ucomm.

  CLEAR ok_code.

  IF t_0310_ass_ovd[] IS NOT INITIAL.
    MOVE 'CANCEL'     TO w_ucomm.
    APPEND w_ucomm    TO t_ucomm.
  ENDIF.

  SET PF-STATUS 'ZSDR0150B'  EXCLUDING t_ucomm.
  SET TITLEBAR 'ZLESR0152C'.

ENDMODULE.

**********************************************************************
*      Module  STATUS_0500  OUTPUT
**********************************************************************
MODULE status_0500 OUTPUT.

  CLEAR ok_code.

  SET PF-STATUS 'ZSDR0150B'.
  SET TITLEBAR 'ZLESR0152D'.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0300 INPUT.

  l_editar = abap_false.

  CASE ok_code.

      " 29.11.2024 - RAMON - US #158242 -->
    WHEN 'UPDT_DOCUM'.

      PERFORM f_refresh_doc CHANGING gv_erro.

      IF gv_erro IS INITIAL.

        CASE abap_true.
          WHEN p_insumo.
            PERFORM f_selecao_dados USING 0.
            CASE abap_true.
              WHEN p_sintet.
                PERFORM f_processa_dados_sintetico.
              WHEN p_analit.
                PERFORM f_processa_dados_analitico.
            ENDCASE.
        ENDCASE.

      ENDIF.

      " 29.11.2024 - RAMON - US #158242 <--

    WHEN 'REFRESH'.
      FREE: t_rows.

      CASE abap_true.
        WHEN p_insumo.
          PERFORM f_selecao_dados USING 0.
          CASE abap_true.
            WHEN p_sintet.
              PERFORM f_processa_dados_sintetico.
            WHEN p_analit.
              PERFORM f_processa_dados_analitico.
          ENDCASE.
      ENDCASE.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.

      IF g_popup = abap_false.
        CALL METHOD g_custom_container->free.
        CALL METHOD cl_container_95->free.
        FREE: g_grid, g_custom_container, obj_dyndoc_id, cl_container_95, g_popup.
      ELSE.
        CALL METHOD g_custom_container_pop->free.
        FREE: g_grid_pop, g_custom_container_pop, g_popup.
      ENDIF.

      p_insumo  = p_insumo_sel.
      p_tpinsm  = p_tpinsm_sel.   "<<<------"168919 - NMS ------>>>
      p_merint  = p_merint_sel.
      p_sintet  = p_sintet_sel.
      p_analit  = p_analit_sel.
      p_contra  = p_contra_sel.
      p_venda   = p_venda_sel.
      p_distra  = p_distra_sel.
      p_aditiv  = p_aditiv_sel.
      p_decrec  = p_decrec_sel.
      p_doctod  = p_doctod_sel.
      p_pend    = p_pend_sel.
      p_conclu  = p_conclu_sel.
      p_todos   = p_todos_sel.
*
      s_vkorg[] = s_vkorg_sel[].
      s_vkbur[] = s_vkbur_sel[].
      s_docsi[] = s_docsi_sel[].
      s_kunnr[] = s_kunnr_sel[].
      s_erdat[] = s_erdat_sel[].
      s_cultu[] = s_cultu_sel[].
      s_safra[] = s_safra_sel[].
      s_spart[] = s_spart_sel[].
      s_moeda[] = s_moeda_sel[].
**<<<------"168919 - NMS - INI------>>>
      s_bukrs[] = s_bukrs_sel[].
      s_werks[] = s_werks_sel[].
      s_nosol[] = s_nosol_sel[].
      s_ebeln[] = s_ebeln_sel[].
      s_lifnr[] = s_lifnr_sel[].
      s_safr2[] = s_safr2_sel[].
      s_dtatl[] = s_dtatl_sel[].
**<<<------"168919 - NMS - FIM------>>>
*     CALL METHOD obj_dyndoc_id->custom_container->free( ).
      LEAVE TO SCREEN 0.

  ENDCASE.

  IF g_popup = abap_false.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ELSE.
    CALL METHOD g_grid_pop->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  FREE ok_code.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0400  INPUT
**********************************************************************
MODULE user_command_0400 INPUT.

  CASE ok_code.
    WHEN 'OK'.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = 'Aguarde...Processando...'.

      LEAVE TO SCREEN 0.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  FREE ok_code.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0500  INPUT
**********************************************************************
MODULE user_command_0500 INPUT.

  CASE ok_code.
    WHEN 'OK'.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = 'Aguarde...Processando...'.

      LEAVE TO SCREEN 0.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  FREE ok_code.

ENDMODULE.
