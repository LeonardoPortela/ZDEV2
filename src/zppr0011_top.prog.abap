*&---------------------------------------------------------------------*
*&  Include           ZPPR0011_TOP
*&---------------------------------------------------------------------*

REPORT zppr007
MESSAGE-ID zppr.


TABLES: klah, zppt0014.

*SELECT-OPTIONS: P_CLASS FOR KLAH-CLASS MATCHCODE OBJECT CRAM.

TYPES:
  BEGIN OF ty_saida,
    clint     TYPE zppt0014-clint,
    class     TYPE zppt0014-class,
    atinn     TYPE zppt0014-atinn,
    atnam     TYPE zppt0014-atnam,
    klart     TYPE zppt0014-klart,
    valor_de  TYPE zppt0014-valor_de,
    valor_ate TYPE zppt0014-valor_ate,
  END OF ty_saida,


  BEGIN OF ty_s_report,
    clint      TYPE zppt0014-clint,
    class      TYPE zppt0014-class,
    atinn      TYPE zppt0014-atinn,
    atnam      TYPE zppt0014-atnam,
    klart      TYPE zppt0014-klart,
    valor_de   TYPE zppt0014-valor_de,
    valor_ate  TYPE zppt0014-valor_ate,
    referencia TYPE zppt0014-valor_ate,  "*-CS2022000332-#83225-26.07.2022-JT-inicio
    marc       TYPE c,
  END OF ty_s_report.

DATA:
  it_s_report         TYPE TABLE OF ty_s_report,
  gt_usuario          TYPE TABLE OF ztpm_d_usuario,
  wa_s_report         TYPE ty_s_report,
  it_ausp             TYPE TABLE OF ausp WITH HEADER LINE,
  it_klah             TYPE TABLE OF klah WITH HEADER LINE,
  it_zmmt0025         TYPE TABLE OF zmmt0025 WITH HEADER LINE,
  it_zppt0014         TYPE TABLE OF zppt0014,
  wa_zppt0014         TYPE zppt0014,
  it_saida            TYPE TABLE OF ty_saida,
  wa_saida            TYPE ty_saida,
  it_v_mkm_zu_kls_bez TYPE TABLE OF v_mkm_zu_kls_bez,
  wa_v_mkm_zu_kls_bez TYPE v_mkm_zu_kls_bez.

DATA:
  wa_cont        TYPE REF TO cl_gui_custom_container,
  wa_alv         TYPE REF TO cl_gui_alv_grid,
  wa_layout      TYPE lvc_s_layo,
  wa_fcat        TYPE lvc_s_fcat,
  it_select_rows TYPE lvc_t_row,
  it_fcat        TYPE TABLE OF lvc_s_fcat,
  it_p_rows      TYPE lvc_t_row,
  wa_select_rows TYPE lvc_s_row,
  wa_p_rows      TYPE lvc_s_row,
  clicks         TYPE sy-tabix.

DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
      tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.


CLASS event DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column sender.
ENDCLASS.

DATA(obj_even) = NEW event( ).

CLASS event IMPLEMENTATION.
  METHOD handle_double_click.
    CHECK e_row-rowtype IS INITIAL.
    PERFORM seleciona_dados_mod USING e_row e_column-fieldname.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM call_scrren.

END-OF-SELECTION.
