"Name: \PR:RPRPOSTD\FO:PROCESS_LOAD_PAYABLE\SE:BEGIN\EI
ENHANCEMENT 0 ZRPRPOSTD.
* Enhancement para modificar o campo BSEG-SGTXT

if sy-tcode = 'PRRW'.
data: w_doc type tab_ptrv_doc_it WITH HEADER LINE,
      w_name TYPE lfa1-name1,
      w_sgtxt TYPE bseg-sgtxt.
loop at lt_ptrv_doc_it into w_doc.
if sy-tabix = 1.

select single name1 from lfa1 INTO w_name
    where lifnr = w_doc-lifnr.

  CONCATENATE w_name 'viagem' w_doc-EXBEL into w_sgtxt SEPARATED BY space.

  endif.
  w_doc-SGTXT = w_sgtxt.
  MODIFY lt_ptrv_doc_it from w_doc index sy-tabix.
  endloop.
  endif.
ENDENHANCEMENT.
