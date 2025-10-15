class ZCL_IM_MD_PIR_FLEX_CONS definition
  public
  final
  create public .

public section.

  interfaces IF_EX_MD_PIR_FLEX_CONS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MD_PIR_FLEX_CONS IMPLEMENTATION.


  method if_ex_md_pir_flex_cons~check_pir_cons_reduc.
    break-point id zpppi002.
    check im_werks eq '0175'.   "Comodoro

    check im_cons_characteristic is not initial.

    ch_cons_reduc_active = 'X'.
  endmethod.


  method if_ex_md_pir_flex_cons~get_req_cons_characteristic.
    field-symbols: <fs_plaab> type plaab,
                   <fs_planr> type planr.

    break-point id zpppi002.

    check im_werks eq '0175'.

    assign ('(SAPLM61X)MDZB-PLAAB') to <fs_plaab>.
    assign ('(SAPLM61X)MDZB-PLANR') to <fs_planr>.

    check <fs_planr> is assigned.
    check <fs_planr> is not initial.

    <fs_plaab> = '02'.
    clear <fs_planr>.
    ch_cons_characteristic = 'OK'.
  endmethod.
ENDCLASS.
