"Name: \PR:RIEQUI20\IC:EAMCC3_MIOLXF14_01\SE:END\EI
ENHANCEMENT 0 ZPM_ADD_FIELD_IE05.

IF NOT object_tab[] IS INITIAL.

  TYPES: BEGIN OF ty_imobilizado,
           objnr         TYPE fleet-objnr,
           zzimobilizado TYPE fleet-zzimobilizado.
  TYPES: END OF ty_imobilizado.

  DATA it_imob TYPE TABLE OF ty_imobilizado.
  DATA l TYPE sy-index.

  SELECT objnr zzimobilizado FROM fleet INTO TABLE it_imob
    FOR ALL ENTRIES IN object_tab
    WHERE objnr = object_tab-objnr.

  IF sy-subrc IS INITIAL AND sy-tcode = 'IE05' OR sy-tcode = 'IH08'.

    LOOP AT object_tab.

      READ TABLE it_imob ASSIGNING FIELD-SYMBOL(<fs1>) WITH KEY objnr = object_tab-objnr.

      IF sy-subrc IS INITIAL AND <fs1> IS ASSIGNED.

        object_tab-zzimob =  <fs1>-zzimobilizado.
        MODIFY object_tab.

      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE g_fieldcat_tab LINES l.
    READ TABLE g_fieldcat_tab ASSIGNING FIELD-SYMBOL(<fs2>) WITH KEY fieldname = 'ZZIMOB'.
    IF sy-subrc IS INITIAL AND <fs2> IS ASSIGNED.
      <fs2>-no_out = space.
      <fs2>-col_pos = l + 1.
    ENDIF.
  ENDIF.
  "Inicio Marcio Miguel 24.01.2023
  IF sy-tcode = 'IE05' OR sy-tcode = 'IH08'.
    DATA: lv_object TYPE ausp-objek.
    DATA: lt_class      TYPE TABLE OF sclass,
          lt_objectdata	TYPE TABLE OF	clobjdat.

    LOOP AT object_tab ASSIGNING FIELD-SYMBOL(<fs_object_tab>).

      lv_object = <fs_object_tab>-equnr.

      CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
        EXPORTING
          classtype          = '002'
          object             = lv_object
        TABLES
          t_class            = lt_class
          t_objectdata       = lt_objectdata
        EXCEPTIONS
          no_classification  = 1
          no_classtypes      = 2
          invalid_class_type = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        CONTINUE.
      ELSE.
        READ TABLE lt_class INTO DATA(wl_class) INDEX 1.
        CHECK sy-subrc = 0.
        <fs_object_tab>-zzclass = wl_class-class.
        <fs_object_tab>-zzklbez = wl_class-klbez.
      ENDIF.

    ENDLOOP.
    DESCRIBE TABLE g_fieldcat_tab LINES l.
    READ TABLE g_fieldcat_tab ASSIGNING <fs2> WITH KEY fieldname = 'ZZCLASS'.
    IF sy-subrc IS INITIAL AND <fs2> IS ASSIGNED.
      <fs2>-no_out = space.
      <fs2>-col_pos = l + 1.
    ENDIF.

    READ TABLE g_fieldcat_tab ASSIGNING <fs2> WITH KEY fieldname = 'ZZKLBEZ'.
    IF sy-subrc IS INITIAL AND <fs2> IS ASSIGNED.
      <fs2>-no_out = space.
      <fs2>-col_pos = <fs2>-col_pos + 1.
    ENDIF.
    SORT g_fieldcat_tab BY col_pos.
  ENDIF.
  "Final Marcio Miguel 24.01.2023
ENDIF.

ENDENHANCEMENT.
