*&---------------------------------------------------------------------*
*& Include          ZPMR0090_TOP
*&---------------------------------------------------------------------*

TABLES: equi.

TYPES:
  BEGIN OF ty_saida.
    INCLUDE STRUCTURE zpme_alv_calendario.
TYPES: drop_down_handle TYPE int4.
*TYPES: celltab TYPE lvc_t_styl.
TYPES: END OF ty_saida,

BEGIN OF ty_equi,
  equnr TYPE equi-equnr,
  eqart TYPE equi-eqart,
  eqktx TYPE eqkt-eqktx,
END OF ty_equi,

BEGIN OF ty_afih,
  equnr TYPE afih-equnr,
  aufnr TYPE afih-aufnr,
END OF ty_afih,

BEGIN OF ty_afko,
  aufnr TYPE afko-aufnr,
  gstrp TYPE afko-gstrp,
  gltrp TYPE afko-gltrp,
  aufpl TYPE afko-aufpl,
END OF ty_afko,

BEGIN OF ty_equz,
  equnr TYPE equz-equnr,
  iwerk TYPE equz-iwerk,
  iloan TYPE equz-iloan,
END OF ty_equz,

BEGIN OF ty_iloa,
  iloan TYPE iloa-iloan,
  adrnr TYPE iloa-adrnr,
END OF ty_iloa,

BEGIN OF ty_qpct,
  kurztext TYPE qpct-kurztext,
END OF ty_qpct,

BEGIN OF ty_adrc,
  addrnumber TYPE adrc-addrnumber,
  name1      TYPE adrc-name1,
END OF ty_adrc,

BEGIN OF ty_afvc,
  aufpl TYPE afvc-aufpl,
  objnr TYPE afvc-objnr,
END OF ty_afvc,

BEGIN OF ty_coep,
  kokrs  TYPE coep-kokrs,
  belnr  TYPE coep-belnr,
  buzei  TYPE coep-buzei,
  objnr  TYPE coep-objnr,
  wtgbtr TYPE coep-wtgbtr,
END OF ty_coep,

BEGIN OF ty_class,
  class TYPE m_clasa-class,
  kschg TYPE m_clasa-kschg,
END OF ty_class.

DATA: gt_saida    TYPE TABLE OF ty_saida,
      gt_equi     TYPE TABLE OF ty_equi,
      gt_afih     TYPE TABLE OF ty_afih,
      gt_afko     TYPE TABLE OF ty_afko,
      gt_aufk     TYPE TABLE OF aufk,
      gt_equz     TYPE TABLE OF ty_equz,
      gt_qpct     TYPE TABLE OF ty_qpct,
      gt_zpmt0080 TYPE TABLE OF zpmt0080,
      gt_iloa     TYPE TABLE OF ty_iloa,
      gt_adrc     TYPE TABLE OF ty_adrc,
      gt_afvc     TYPE TABLE OF ty_afvc,
      gt_coep     TYPE TABLE OF ty_coep,
      gt_class    TYPE TABLE OF ty_class.

DATA: gr_alv              TYPE REF TO cl_gui_alv_grid,
      go_custom_container TYPE REF TO cl_gui_docking_container.
