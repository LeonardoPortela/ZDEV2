*&---------------------------------------------------------------------*
*& Report  ZSDR0134
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0134.

TABLES cdhdr.

TYPES BEGIN OF ty_saida.
        INCLUDE TYPE cdhdr.
        INCLUDE TYPE cdshw.
TYPES END OF ty_saida.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: class FOR cdhdr-objectclas NO-EXTENSION NO INTERVALS DEFAULT 'VERKBELEG',
                id    FOR cdhdr-objectid NO-EXTENSION NO INTERVALS DEFAULT '0010000001'.
SELECTION-SCREEN: END OF BLOCK b1.


DATA:
  t_cdhdr     TYPE TABLE OF cdhdr,
  t_editpos   TYPE TABLE OF cdshw,
  it_fcat     TYPE TABLE OF slis_fieldcat_alv,
  it_fieldcat TYPE TABLE OF slis_fieldcat_alv,
  wa_layout   TYPE  slis_layout_alv,
  it_saida    TYPE TABLE OF ty_saida,
  wa_saida    TYPE  ty_saida.

CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
  EXPORTING
    objectclass                = class-low
    objectid                   = id-low
    username                   = '*'
  TABLES
    i_cdhdr                    = t_cdhdr
  EXCEPTIONS
    no_position_found          = 1
    wrong_access_to_archive    = 2
    time_zone_conversion_error = 3
    OTHERS                     = 4.

LOOP AT t_cdhdr INTO DATA(w_cdhdr).

  REFRESH: t_editpos.

  CALL FUNCTION 'CHANGEDOCUMENT_READ_POSITIONS'
    EXPORTING
      changenumber            = w_cdhdr-changenr
    TABLES
      editpos                 = t_editpos
    EXCEPTIONS
      no_position_found       = 1
      wrong_access_to_archive = 2
      OTHERS                  = 3.

  MOVE-CORRESPONDING w_cdhdr TO wa_saida.

  LOOP AT t_editpos INTO DATA(w_editpos).
    MOVE-CORRESPONDING w_editpos TO wa_saida.
    APPEND wa_saida TO it_saida.
  ENDLOOP.

ENDLOOP.

CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    i_structure_name       = 'cdhdr'
  CHANGING
    ct_fieldcat            = it_fieldcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.

APPEND LINES OF it_fieldcat TO it_fcat.

FREE it_fieldcat.

CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    i_structure_name       = 'cdshw'
  CHANGING
    ct_fieldcat            = it_fieldcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.

APPEND LINES OF it_fieldcat TO it_fcat.

SORT it_fcat.
DELETE ADJACENT DUPLICATES FROM it_fcat COMPARING ALL FIELDS.

wa_layout-colwidth_optimize = abap_true.
wa_layout-zebra = abap_true.

SORT it_saida BY udate utime.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program = sy-repid
    i_save             = 'A'
    it_fieldcat        = it_fcat
    is_layout          = wa_layout
  TABLES
    t_outtab           = it_saida
  EXCEPTIONS
    program_error      = 1
    OTHERS             = 2.
