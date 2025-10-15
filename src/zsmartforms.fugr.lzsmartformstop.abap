FUNCTION-POOL zsmartforms.                  "MESSAGE-ID ..

TYPES: lt_pdf_table(1000) TYPE x.


DATA: pdf_size   TYPE i,
      l_url(80)  TYPE c,
      l_pdf_line TYPE lt_pdf_table,
      l_pdf_data TYPE STANDARD TABLE OF lt_pdf_table,
      pdf_data   TYPE xstring.            "#149060-19.08.2024-JT

DATA: ok_code         TYPE sy-ucomm,
      g_doc_simulacao TYPE zid_documento,
      g_salvar        TYPE char01.        "#149060-19.08.2024-JT

* INCLUDE LZSMARTFORMSD...                   " Local class definition
