class ZCL_IM_PP_WORKORDER_GOODS definition
  public
  final
  create public .

*"* public components of class ZCL_IM_PP_WORKORDER_GOODS
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_GOODSMVT .
protected section.
*"* protected components of class ZCL_IM_PP_WORKORDER_GOODS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PP_WORKORDER_GOODS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PP_WORKORDER_GOODS IMPLEMENTATION.


method IF_EX_WORKORDER_GOODSMVT~BACKFLUSH.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~COGI_AUTHORITY_CHECK.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~COGI_POST.
endmethod.


  method IF_EX_WORKORDER_GOODSMVT~COMPLETE_GOODSMOVEMENT.
  endmethod.


METHOD if_ex_workorder_goodsmvt~gm_screen_line_check.

  CONSTANTS: c_gt_comp(19) TYPE c        VALUE '(SAPLCOWB)gt_comp[]',
             c_mf42n       TYPE sy-tcode VALUE 'MF42N',
             c_mf42        TYPE sy-tcode VALUE 'MF42',
             c_mfbf        TYPE sy-tcode VALUE 'MFBF',
             c_261(3)      TYPE c        VALUE '261',
             c_531(3)      TYPE c        VALUE '531'.

  FIELD-SYMBOLS: <fs_gt_comp> TYPE STANDARD TABLE.

  DATA: ti_comp TYPE SORTED TABLE OF cowb_comp
          WITH NON-UNIQUE KEY matnr.

  DATA: wa_comp TYPE cowb_comp.

  IF sy-tcode EQ c_mf42n OR
     sy-tcode EQ c_mf42  OR
     sy-tcode EQ c_mfbf.

    ASSIGN (c_gt_comp) TO <fs_gt_comp>.

    IF sy-subrc EQ 0.
      ti_comp = <fs_gt_comp>.

      READ TABLE ti_comp
        INTO wa_comp
        WITH KEY matnr = i_cowb_comp-matnr
        BINARY SEARCH.

      IF sy-subrc EQ 0.
        IF i_cowb_comp-bwart NE wa_comp-bwart.
          IF wa_comp-bwart EQ c_531.
            MESSAGE e014(z01) WITH i_cowb_comp-matnr.
          ELSEIF wa_comp-bwart EQ c_261.
            MESSAGE e015(z01) WITH i_cowb_comp-matnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


method IF_EX_WORKORDER_GOODSMVT~GM_SCREEN_OKCODE_CHECK.
endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_CHECK.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_PROPOSE.
  endmethod.


method IF_EX_WORKORDER_GOODSMVT~GOODS_RECEIPT.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~IM_CALLED.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~MANUAL_GOODS_RECEIPT.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~PICKLIST.
endmethod.
ENDCLASS.
