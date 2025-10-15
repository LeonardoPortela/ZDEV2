class ZCL_UTILS_ABAP definition
  public
  final
  create public .

public section.

  class-methods SHOW_DATA_ALV
    importing
      !I_DATA_TABLE type STANDARD TABLE
      !I_STRUCTURE_NAME type DD02L-TABNAME
      !I_GRID_TITLE type LVC_TITLE optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UTILS_ABAP IMPLEMENTATION.


  METHOD show_data_alv.

    DATA: w_layout   TYPE slis_layout_alv,
          w_fieldcat TYPE slis_fieldcat_alv,
          t_fieldcat TYPE slis_t_fieldcat_alv.

    DATA: d_it_saida               TYPE REF TO data.
    FIELD-SYMBOLS: <fs_it_saida>   TYPE table.

    FREE: t_fieldcat.

    CREATE DATA d_it_saida TYPE TABLE OF (i_structure_name).
    ASSIGN d_it_saida->* TO <fs_it_saida>.

    MOVE-CORRESPONDING i_data_table[] TO <fs_it_saida>.

    w_layout-colwidth_optimize = abap_true.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = i_structure_name
      CHANGING
        ct_fieldcat            = t_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    LOOP AT t_fieldcat     INTO w_fieldcat.
      w_fieldcat-seltext_l    = w_fieldcat-fieldname.
      w_fieldcat-seltext_m    = w_fieldcat-fieldname.
      w_fieldcat-seltext_s    = w_fieldcat-fieldname.
      w_fieldcat-reptext_ddic = w_fieldcat-fieldname.
      MODIFY t_fieldcat    FROM w_fieldcat INDEX sy-tabix.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_background_id    = 'ALV_BACKGROUND'
        i_buffer_active    = 'X'
        i_callback_program = sy-repid
        it_fieldcat        = t_fieldcat
        i_grid_title       = i_grid_title
*       i_structure_name   = 'ZHCMT_PA_0025'
        is_layout          = w_layout
        i_save             = 'A'
*       IS_VARIANT         = GS_VARIANT
*       IT_EVENTS          = GT_EVENTS[]
*       I_SCREEN_START_COLUMN    = 0     "Use coordinates for
*       I_SCREEN_START_LINE      = 0     "display as dialog box
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE  = 0
      TABLES
        t_outtab           = <fs_it_saida>
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.




  ENDMETHOD.
ENDCLASS.
