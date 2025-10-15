
process before output.
*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TAB_MEMO_NF_EXP'
  module tab_memo_nf_exp_change_tc_attr.
*&SPWIZARD: MODULE TAB_MEMO_NF_EXP_CHANGE_COL_ATTR.
  loop at   it_notas_exp
       with control tab_memo_nf_exp
       cursor tab_memo_nf_exp-current_line.
*&SPWIZARD:   MODULE TAB_MEMO_NF_EXP_CHANGE_FIELD_ATTR
  endloop.

* MODULE STATUS_1004.
*
process after input.
*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TAB_MEMO_NF_EXP'
  loop at it_notas_exp.
    chain.
      field it_notas_exp-nr_nota_exp.
      field it_notas_exp-proprio.
      field it_notas_exp-serie.
      field it_notas_exp-numero_nota.
      field it_notas_exp-nfe.
      field it_notas_exp-emissor.
      field it_notas_exp-docnum.
      field it_notas_exp-dt_emissao_nota.
      field it_notas_exp-material.
      field it_notas_exp-quantidade.
      field it_notas_exp-unidade.
      field it_notas_exp-moeda.
      field it_notas_exp-valor_total.
      field it_notas_exp-valor_unitario.
      field it_notas_exp-emissorn.
      field it_notas_exp-emissorcnpj.
      field it_notas_exp-materialn.
      field it_notas_exp-quant_vinc.
      field it_notas_exp-saldo_disp.
      field it_notas_exp-quant_vinc_2.
      field it_notas_exp-saldo_disp_2.
      field it_notas_exp-danfe.
    endchain.
    field it_notas_exp-docnum module chama_doc_sap at cursor-selection.
    field it_notas_exp-danfe  module chama_danfe at cursor-selection.
  endloop.
*&SPWIZARD: MODULE TAB_MEMO_NF_EXP_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TAB_MEMO_NF_EXP_CHANGE_COL_ATTR.

* MODULE USER_COMMAND_1004.
