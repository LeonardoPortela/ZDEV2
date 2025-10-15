class ZCL_RSI_NFE_ADDITIONAL_FIELDS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZRSI_IF_NFE_ADDITIONAL_FIELDS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_RSI_NFE_ADDITIONAL_FIELDS IMPLEMENTATION.


  METHOD zrsi_if_nfe_additional_fields~change_nf_header.

    ch_doc-ind_intermed = '0'.

    IF ch_doc-ind_pres NE '2' and
       ch_doc-ind_pres NE '3' and
       ch_doc-ind_pres NE '4' and
       ch_doc-ind_pres NE '9'.
      clear ch_doc-ind_intermed .
    ENDIF.

  ENDMETHOD.


  method ZRSI_IF_NFE_ADDITIONAL_FIELDS~CHANGE_NF_ITEM.

  endmethod.


  method ZRSI_IF_NFE_ADDITIONAL_FIELDS~CHANGE_NF_PAYMENT.

  endmethod.


  method ZRSI_IF_NFE_ADDITIONAL_FIELDS~FILL_ICMSSTDESON.
  endmethod.
ENDCLASS.
