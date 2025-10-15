"Name: \FU:J_1B_NF_DOCUMENT_UPDATE\SE:BEGIN\EI
ENHANCEMENT 0 Z_1B_SET_NF_DATA.

   IF 1 = 2.
*    DATA: lit_item_change TYPE ty_j_1bnflin,
*          lwa_j_1bnfdoc   TYPE j_1bnfdoc.

    data(lwa_j_1bnfdoc)     = DOC_HEADER.
    data(lit_item_change) = DOC_ITEM[].

    zcl_j_1bset_nf_data=>change_nf( CHANGING c_docheader = lwa_j_1bnfdoc
                                             c_it_item   = lit_item_change ).

    DOC_HEADER   =  lwa_j_1bnfdoc.
    DOC_ITEM[]   =  lit_item_change[].
   ENDIF.


ENDENHANCEMENT.
