DATA: it_screen_status TYPE TABLE OF sy-ucomm.

TYPES: BEGIN OF ty_saida,
         anln1             TYPE anln1, "Imobilizado  -> A
         anln2             TYPE anln2, "Imobilizado 2 -> B
         zimob_v           TYPE zimob_v, "Status -> C
         zimob_p           TYPE zimob_p, "Placa de Ativo -> D
         observ            TYPE badi_char255, "Observação -> E
         iventariado       TYPE i,
         img               TYPE zaa005-img, "Imagem -> G
         inventariante(50) TYPE c,
       END OF ty_saida.

DATA: o_alv     TYPE REF TO cl_salv_table,
      it_ZAA005 TYPE STANDARD TABLE OF zaa005 INITIAL SIZE 0,
      wa_ZAA005 TYPE zaa005,
      it_saida  TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0,
      wa_saida  TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0.

DATA: lv_bukrs TYPE anlz-bukrs,
      iv_file  TYPE string,
      iv_pfile TYPE string.

TABLES: anlz.
