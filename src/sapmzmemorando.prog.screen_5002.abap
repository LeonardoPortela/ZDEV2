
process before output.

  module cria_alv_acomp.

**&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TAB_EXPORT_ACP'
*  module tab_export_acp_change_tc_attr.
**&SPWIZARD: MODULE TAB_EXPORT_ACP_CHANGE_COL_ATTR.
*  loop at   it_export_acomp
*       with control tab_export_acp
*       cursor tab_export_acp-current_line.
**&SPWIZARD:   MODULE TAB_EXPORT_ACP_CHANGE_FIELD_ATTR
*  endloop.

* MODULE STATUS_5002.

process after input.

**&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TAB_EXPORT_ACP'
*  loop at it_export_acomp.
*    chain.
*      field it_export_acomp-werks.
*      field it_export_acomp-exportador.
*      field it_export_acomp-exportadorn.
*      field it_export_acomp-exportcnpj.
*      field it_export_acomp-regiao.
*      field it_export_acomp-produto.
*      field it_export_acomp-produton.
*      field it_export_acomp-unidade.
*      field it_export_acomp-quantidade.
*      field it_export_acomp-quantcompe.
*      field it_export_acomp-quantacomp.
*      field it_export_acomp-status.
*    endchain.
*    field it_export_acomp-mark
*      module tab_export_acp_mark on request.
*  endloop.
**&SPWIZARD: MODULE TAB_EXPORT_ACP_CHANGE_TC_ATTR.
**&SPWIZARD: MODULE TAB_EXPORT_ACP_CHANGE_COL_ATTR.
*
** MODULE USER_COMMAND_5002.
