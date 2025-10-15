class ZCL_MATERIAL definition
  public
  final
  create public .

public section.

  methods GET_MATERIAL_BY_MATNR
    importing
      !IV_MATNR type MATNR
    returning
      value(ES_MARA) type MARA .
  methods GET_MATERIAL_BY_LIST
    importing
      !IR_MATNR_RANGE type CFB_T_MATNR_RANGE
    returning
      value(R_MARA_TAB) type MARA_TT .
  methods GET_MATERIAL_TEXT_BY_TABLE
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(R_MAKT_TAB) type MAKT_TAB .
  methods GET_MATERIAL_BY_TABLE
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(R_MARA_TAB) type MARA_TT .
  methods GET_MATERIAL_TEXT_BY_MATNR
    importing
      !IV_MATNR type MATNR
    returning
      value(ES_MAKT) type MAKT .
  methods GET_MATERIAL_TEXT_BY_LIST
    importing
      !IR_MATNR_RANGE type CFB_T_MATNR_RANGE
    returning
      value(R_MAKT_TAB) type MAKT_TAB .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_MATERIAL .
  methods GET_ZSDT0371_BY_MATNR
    importing
      !IV_MATNR type MATNR
    returning
      value(ES_ZSDT0371) type ZSDT0371 .
  methods GET_ZSDT0371_TABLE_BY_MATNR
    importing
      !IV_MATNR type MATNR
    returning
      value(ET_ZSDT0371) type ZSDC0371 .
protected section.
private section.

  data GT_MARA type MARA_TT .
  data GT_MAKT type MAKT_TAB .
  data GT_ZSDT0371 type ZSDC0371 .
  class-data GO_INSTANCE type ref to ZCL_MATERIAL .
ENDCLASS.



CLASS ZCL_MATERIAL IMPLEMENTATION.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      go_instance = NEW zcl_material( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD GET_MATERIAL_BY_LIST.

    DATA lr_matnr TYPE cfb_t_matnr_range.

    lr_matnr = ir_matnr_range.

    SORT lr_matnr ASCENDING.

    DELETE ADJACENT DUPLICATES FROM lr_matnr.

    LOOP AT gt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>) WHERE matnr IN ir_matnr_range.

      " APAGA DA BUSCA OS QUE JÁ EXISTEM
      DELETE lr_matnr WHERE low = <fs_mara>-matnr.

    ENDLOOP.

    IF lr_matnr IS NOT INITIAL.

      SELECT * FROM mara
        APPENDING TABLE gt_mara
          WHERE matnr IN lr_matnr.

      SORT gt_mara ASCENDING.

    ENDIF.

    CLEAR r_mara_tab[].

    LOOP AT gt_mara ASSIGNING <fs_mara> WHERE matnr IN ir_matnr_range.

      APPEND INITIAL LINE TO r_mara_tab ASSIGNING FIELD-SYMBOL(<fs_append>).

      MOVE-CORRESPONDING <fs_mara> TO <fs_append>.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_material_by_matnr.

    IF NOT line_exists( gt_mara[ matnr = iv_matnr ] ).

      SELECT * FROM mara
        APPENDING TABLE gt_mara
          WHERE matnr = iv_matnr.

      SORT gt_mara ASCENDING.

    ENDIF.

    es_mara = VALUE #( gt_mara[ matnr = iv_matnr ] DEFAULT '' ).

  ENDMETHOD.


  METHOD get_material_by_table.

    FIELD-SYMBOLS <fs_matnr> TYPE matnr.

    DATA lr_matnr TYPE cfb_t_matnr_range.

    CHECK it_table[] IS NOT INITIAL.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<fs_dyn>).

      ASSIGN ('<FS_DYN>-MATNR') TO <fs_matnr>.

      CHECK sy-subrc EQ 0.

      APPEND 'IEQ' && <fs_matnr> TO lr_matnr.

    ENDLOOP.

    r_mara_tab = me->get_material_by_list( lr_matnr ).

  ENDMETHOD.


  METHOD get_material_text_by_list.

    DATA lr_matnr TYPE cfb_t_matnr_range.

    lr_matnr = ir_matnr_range.

    LOOP AT gt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>) WHERE matnr IN ir_matnr_range.

      " APAGA DA BUSCA OS QUE JÁ EXISTEM
      DELETE lr_matnr WHERE low = <fs_makt>-matnr.

    ENDLOOP.

    SORT lr_matnr.

    DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING ALL FIELDS.

    IF lr_matnr IS NOT INITIAL.

      SELECT * FROM makt
        APPENDING TABLE gt_makt
          WHERE matnr IN lr_matnr
            AND spras = sy-langu.

      SORT gt_makt ASCENDING.

    ENDIF.

    CLEAR r_makt_tab[].

    LOOP AT gt_makt ASSIGNING <fs_makt> WHERE matnr IN ir_matnr_range.

      APPEND INITIAL LINE TO r_makt_tab ASSIGNING FIELD-SYMBOL(<fs_append>).

      MOVE-CORRESPONDING <fs_makt> TO <fs_append>.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_material_text_by_matnr.

    IF NOT line_exists( gt_makt[ matnr = iv_matnr ] ).

      SELECT * FROM makt
        APPENDING TABLE gt_makt
          WHERE matnr = iv_matnr
            AND spras = sy-langu.

      SORT gt_makt ASCENDING.

    ENDIF.

    es_makt = VALUE #( gt_makt[ matnr = iv_matnr ] DEFAULT '' ).

  ENDMETHOD.


  METHOD get_material_text_by_table.

    FIELD-SYMBOLS <fs_matnr> TYPE matnr.

    DATA lr_matnr TYPE cfb_t_matnr_range.

    CHECK it_table[] IS NOT INITIAL.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<fs_dyn>).

      ASSIGN ('<FS_DYN>-MATNR') TO <fs_matnr>.

      CHECK sy-subrc EQ 0.

      APPEND 'IEQ' && <fs_matnr> TO lr_matnr.

    ENDLOOP.

    r_makt_tab = me->get_material_text_by_list( lr_matnr ).

  ENDMETHOD.


  METHOD get_zsdt0371_by_matnr.


    DATA(ls_mara) = zcl_material=>get_instance( )->get_material_by_matnr( iv_matnr ).

    CHECK ls_mara IS NOT INITIAL.

    IF NOT line_exists( gt_zsdt0371[ matkl = ls_mara-matkl ] ).

      SELECT * FROM zsdt0371
        APPENDING TABLE gt_zsdt0371
          WHERE matkl = ls_mara-matkl.

      SORT gt_zsdt0371 ASCENDING.

    ENDIF.

    es_zsdt0371 = VALUE #( gt_zsdt0371[ matkl = ls_mara-matkl ] DEFAULT '' ).

  ENDMETHOD.


  METHOD get_zsdt0371_table_by_matnr.

    DATA lt_0371 TYPE TABLE OF zsdt0371.

    DATA(ls_0371) = me->get_zsdt0371_by_matnr( iv_matnr ).

    CLEAR et_zsdt0371.

    LOOP AT gt_zsdt0371 ASSIGNING FIELD-SYMBOL(<fs_0371>) WHERE matkl = ls_0371-matkl.

      APPEND <fs_0371> TO et_zsdt0371.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
