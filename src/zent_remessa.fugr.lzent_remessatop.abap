FUNCTION-POOL zent_remessa MESSAGE-ID zre.

TYPES: BEGIN OF ty_remessas,
         numero   TYPE vbeln_vl, "Remessa
         ref_doc  TYPE vbeln_va, "Documento de vendas
         ref_item TYPE posnr_va, "Item do documento de vendas
         l_subrc  TYPE sy-subrc,
       END OF ty_remessas.

DATA: wa_entrada_rem TYPE zsdt_entrada_rem,
      wa_romaneio    TYPE zsdt0001,
      vg_status      LIKE zsdt_entrada_rem-tp_status,
      w_messtab      TYPE bdcmsgcoll,
      wa_log         TYPE zsdt_entrada_log,
      it_log         TYPE TABLE OF zsdt_entrada_log.

DATA: it_zsdt0001 TYPE TABLE OF zsdt0001 WITH HEADER LINE INITIAL SIZE 0,
      t_bdcdata   LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      t_messtab   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA  remessas TYPE ty_remessas.
