FUNCTION Z_FI_GRAVA_TABELAZ_F110.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LAUFD) TYPE  ZFIT0006-LAUFD
*"     REFERENCE(LAUFI) TYPE  ZFIT0006-LAUFI
*"----------------------------------------------------------------------

  data: wa_doc              like zfit0006.

    wa_doc-laufd = laufd.
    wa_doc-laufi = laufi.
    wa_doc-data  = sy-datum.
    wa_doc-hora  = sy-uzeit.

    insert into  zfit0006 values wa_doc.

ENDFUNCTION.
