*&---------------------------------------------------------------------*
*& Include ZSDI_NF_REMOVEDOR_TMISS
*&---------------------------------------------------------------------*

TRY.

    DATA:
          lo_nf_removedor_tmiss TYPE REF TO zcl_nf_removedor_tmiss.

    CREATE OBJECT lo_nf_removedor_tmiss
      EXPORTING
        i_s_doc_header = doc_header.

    lo_nf_removedor_tmiss->iniciar(
      CHANGING
        c_t_doc_item =  doc_item[] ).

  CATCH cx_root.
    " Erro ao executar a classe...
ENDTRY.
