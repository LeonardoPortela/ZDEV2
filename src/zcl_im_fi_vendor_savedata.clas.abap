class ZCL_IM_FI_VENDOR_SAVEDATA definition
  public
  final
  create public .

*"* public components of class ZCL_IM_FI_VENDOR_SAVEDATA
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA .
protected section.
*"* protected components of class ZCL_IM_FI_VENDOR_SAVEDATA
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_FI_VENDOR_SAVEDATA
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_FI_VENDOR_SAVEDATA IMPLEMENTATION.


method IF_EX_VENDOR_ADD_DATA~BUILD_TEXT_FOR_CHANGE_DETAIL.
endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ACCOUNT_NUMBER.
endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ADD_ON_ACTIVE.
endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ALL_DATA.
endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_DATA_CHANGED.
endmethod.


method IF_EX_VENDOR_ADD_DATA~GET_CHANGEDOCS_FOR_OWN_TABLES.
endmethod.


method IF_EX_VENDOR_ADD_DATA~INITIALIZE_ADD_ON_DATA.
endmethod.


method if_ex_vendor_add_data~modify_account_number.
endmethod.


method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_CCODE.
endmethod.


method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_PORG.
endmethod.


method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_PORG_ALTERNATIVE.
endmethod.


method IF_EX_VENDOR_ADD_DATA~READ_ADD_ON_DATA.
endmethod.


method if_ex_vendor_add_data~save_data.


data: wa_tmp   type zbccarga_tmp,
      wa_indx  type indx,
      vl_lifnr type lfa1-lifnr.


clear wa_tmp.

wa_tmp-carga    = 'FOR'.
wa_tmp-dt_atu   = sy-datum.
wa_tmp-hr_atu   = sy-uzeit.
wa_tmp-registro = i_lifnr.

modify zbccarga_tmp from wa_tmp.
vl_lifnr        = i_lifnr.

export tmp = vl_lifnr to shared buffer indx(hk) from wa_indx id 'TES'.
endmethod.


method IF_EX_VENDOR_ADD_DATA~SET_USER_INPUTS.
break brxs_braxis.
endmethod.
ENDCLASS.
