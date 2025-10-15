*&---------------------------------------------------------------------*
*& Include ZFII_CONTAB_ARGENTINA
*&---------------------------------------------------------------------*

DATA:
      ol_fi_contab_argentina TYPE REF TO zcl_fi_contab_argentina.

TRY.

    CREATE OBJECT ol_fi_contab_argentina
      EXPORTING
        i_w_febko   = febko
        i_v_arquivo = umsatz-zeile.

    ol_fi_contab_argentina->iniciar(
      CHANGING
        c_w_febep = febep
    ).

  CATCH cx_root.
    " Ocorreu algum erro...
ENDTRY.
