function-pool zsapmzplancomp message-id zplan_nomeacao.

type-pools: zplac.

data: ok_code_1001 type sy-ucomm.

data: wa_zdoc_exp_recusa_tela type zdoc_exp_recusa_tela,
      it_zdoc_exp_recusa_tela type table of zdoc_exp_recusa_tela with header line.

constants: ok_sel type sy-ucomm value 'SELECIONAR',
           ok_can type sy-ucomm value 'CANCELAR'.


*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
form z_estrutura_fieldcat tables it_catalogo type lvc_t_fcat
                           using p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit.

  data: plan_catalog type lvc_s_fcat.
  plan_catalog-tabname     = p_tab_name.
  plan_catalog-fieldname   = p_fieldname.
  plan_catalog-scrtext_l   = p_texto_grande.
  plan_catalog-scrtext_m   = p_texto_grande.
  plan_catalog-scrtext_s   = p_texto_grande.
  plan_catalog-hotspot     = p_hot.
  plan_catalog-col_pos     = p_posicao.
  plan_catalog-outputlen   = p_outputlen.
  plan_catalog-fix_column  = p_fix_column.
  plan_catalog-convexit    = p_convexit.
  plan_catalog-do_sum      = p_do_sum.
  plan_catalog-icon        = p_icon.
  plan_catalog-just        = p_just.
  plan_catalog-emphasize   = p_emphasize.
  plan_catalog-edit        = p_edit.

  CASE p_tab_name.
    WHEN 'IT_ZNOM_REMETENTE_ALV'.
      IF p_fieldname eq 'NR_QTD_VINCULAR'.
        plan_catalog-ref_table = 'ZNOM_REMETENTE'.
        plan_catalog-ref_field = 'NR_PROGRAMADA'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.


  append plan_catalog to it_catalogo.
endform.                    " Z_ESTRUTURA_FIELDCAT
