"Name: \FU:J_1B_NF_DOCUMENT_INSERT\SE:BEGIN\EI
ENHANCEMENT 0 Z_1B_SET_NF_DATA.



      "IF 1 = 2.
*      DATA:
*            lit_item_change LIKE j_1bnflin,
*            lwa_j_1bnfdoc   like j_1bnfdoc.

*      lwa_j_1bnfdoc     = DOC_HEADER.
*      lit_item_change[] = DOC_ITEM[].
      DATA(lwa_j_1bnfdoc)     = doc_header.
      DATA(lit_item_change) = doc_item[].
      lit_item_change[] =   doc_item[] .
      zcl_j_1bset_nf_data=>change_nf( CHANGING c_docheader = lwa_j_1bnfdoc
                                               c_it_item   = lit_item_change ).


      doc_header   =  lwa_j_1bnfdoc.
      doc_item[]   =  lit_item_change[].
      " ENDIF.
*** Stefanini - IR238712 - 24/06/2025 - LAZAROSR - Início de Alteração
      INCLUDE zsdi_nf_removedor_tmiss.
*** Stefanini - IR238712 - 24/06/2025 - LAZAROSR - Fim de Alteração


*** Stefanini - IR254459 - 01/09/2025 - ISOUZA - Início de Alteração
      SORT doc_item_tax BY docnum itmnum taxtyp.
      DELETE ADJACENT DUPLICATES FROM doc_item_tax[] COMPARING docnum itmnum taxtyp.
*** Stefanini - IR254459 - 01/09/2025 - ISOUZA - Fim de Alteração
ENDENHANCEMENT.
