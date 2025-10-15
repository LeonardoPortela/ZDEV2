*----------------------------------------------------------------------*
***INCLUDE LZGFS_REM_CONTA_ORDEMI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&DESMARCA'.
      PERFORM f_elimina_link.

      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN '&CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& eliminar link
*&---------------------------------------------------------------------*
FORM f_elimina_link.

  DELETE FROM zlest0210 WHERE chave_nf_venda = g_chave_nf_venda.
  COMMIT WORK AND WAIT.

  FREE g_nfnum9.

ENDFORM.

*&---------------------------------------------------------------------*
*& validar dados
*&---------------------------------------------------------------------*
FORM f_validar CHANGING p_erro.

  FREE: p_erro.

  IF w_j_1bnfdoc-brgew <> w_saida-prod_qtd_comerci.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Qtdade NF de Remessa:' w_j_1bnfdoc-brgew 'diferente de Qtdade de NF de venda:' w_saida-prod_qtd_comerci DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& salvar
*&---------------------------------------------------------------------*
FORM f_salva.

  DATA: l_vbeln         TYPE vbeln_va,
        l_remessa_dummy TYPE vbeln_vl.

  FREE: w_zlest0210,
        l_vbeln,
        l_remessa_dummy.

*--------------------------
* procura remessa dummy
*--------------------------
*  SELECT vbeln
*    INTO l_vbeln
*    FROM vbfa
*      UP TO 1 ROWS
*   WHERE vbelv   = g_vbeln
*     AND vbtyp_n = 'C'.
*  ENDSELECT.
*
*  IF sy-subrc = 0.
*    SELECT vbeln
*      INTO l_remessa_dummy
*      FROM vbfa
*        UP TO 1 ROWS
*     WHERE vbelv   = l_vbeln
*       AND vbtyp_n = 'J'.
*    ENDSELECT.
*  ENDIF.
*
*  w_zlest0210-mandt               = sy-mandt.
*  w_zlest0210-chave_nf_venda      = g_chave_nf_venda.
*  w_zlest0210-chave_nf_cta_ordem  = w_saida-chave_nfe.
*  w_zlest0210-vbeln_venda         = g_vbeln.
*  w_zlest0210-remessa_dummy       = l_remessa_dummy.
*
*  MODIFY zlest0210             FROM w_zlest0210.
*
*  COMMIT WORK AND WAIT.

*--------------------------------
* grava vinculacao
*--------------------------------
  zcl_remessa_terceiro=>zif_remessa_terceiro~set_salva_vinculacao(
     EXPORTING i_chave_nf_venda     = g_chave_nf_venda
               i_chave_nf_cta_ordem = w_saida-chave_nfe
               i_vbeln_venda        = g_vbeln ).

  w_campos_nfe = zcl_util->get_atributos_nfe( w_saida-chave_nfe ).
  g_nfnum9     = w_campos_nfe-nfnum9.

ENDFORM.
