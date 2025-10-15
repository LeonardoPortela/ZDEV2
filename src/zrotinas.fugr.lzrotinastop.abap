FUNCTION-POOL zrotinas.                     "MESSAGE-ID ..

TYPE-POOLS: ole2.


DATA: wv_excel      TYPE ole2_object,
      wv_workbook   TYPE ole2_object,
      wv_book       TYPE ole2_object,
      wv_awork      TYPE ole2_object,
      gs_cell1      TYPE ole2_object,
      gs_cell2      TYPE ole2_object,
      gs_cells      TYPE ole2_object,
      gs_font       TYPE ole2_object,
      gs_interior   TYPE ole2_object,
      wv_celula     TYPE string,
      wv_range      TYPE ole2_object,
      p_arq         TYPE string,
      p_cor_linha   TYPE i.
