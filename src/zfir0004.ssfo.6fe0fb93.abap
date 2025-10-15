CLEAR: wg_ir.
READ TABLE TG_WITH_ITEM INTO WG_WITH_ITEM
  WITH KEY WITHT = 'IR'
           BELNR = WG_BKPF-BELNR
           GJAHR = WG_BKPF-GJAHR.

 wg_ir = wg_with_item-wt_qbshh.


















