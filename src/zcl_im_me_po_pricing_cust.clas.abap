class ZCL_IM_ME_PO_PRICING_CUST definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PO_PRICING_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_PO_PRICING_CUST IMPLEMENTATION.


  METHOD if_ex_me_po_pricing_cust~process_komk.
    IF ( ch_komk-kalsm NE 'ZRM000' ).
      SELECT COUNT( * )
        FROM tvarvc
        WHERE name = 'ZMM_PRICE_PO_BSART'
          AND low  = im_ekko-bsart.

      IF ( sy-subrc IS INITIAL ).

        SELECT COUNT( * )
          FROM tvarvc
          WHERE name = 'ZMM_PRICE_PO_WERKS'
            AND low  = im_ekpo-werks.

        IF ( sy-subrc IS INITIAL ).
          ch_komk-kalsm = 'ZRM000'.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  method IF_EX_ME_PO_PRICING_CUST~PROCESS_KOMP.

  endmethod.
ENDCLASS.
