class ZCL_IM_FI_VENDOR_SAVE_BI definition
  public
  final
  create public .

*"* public components of class ZCL_IM_FI_VENDOR_SAVE_BI
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_BI .
protected section.
*"* protected components of class ZCL_IM_FI_VENDOR_SAVE_BI
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_FI_VENDOR_SAVE_BI
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_FI_VENDOR_SAVE_BI IMPLEMENTATION.


method IF_EX_VENDOR_ADD_DATA_BI~CHECK_DATA_ROW.
break brxs_braxis.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~FILL_ALE_SEGMENTS_OWN_DATA.
break brxs_braxis.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~FILL_BI_TABLE_WITH_OWN_SEGMENT.
break brxs_braxis.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~FILL_FT_TABLE_USING_DATA_ROWS.
break brxs_braxis.

endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~MODIFY_BI_STRUCT_FROM_STD_SEG.
break brxs_braxis.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~PASS_NON_STANDARD_SEGMENT.
break brxs_braxis.
endmethod.


method IF_EX_VENDOR_ADD_DATA_BI~PROCESS_ALE_OWN_CHANGE_POINTER.
break brxs_braxis.
endmethod.
ENDCLASS.
