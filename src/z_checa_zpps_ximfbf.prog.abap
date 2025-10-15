*&---------------------------------------------------------------------*
*& Report Z_CHECA_ZPPS_XIMFBF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_checa_zpps_ximfbf.

TABLES: zpps_ximfbf_log.

DATA: t_log1       TYPE TABLE OF zpps_ximfbf_log,
      t_log2       TYPE TABLE OF zpps_ximfbf_log,
      w_log1       TYPE zpps_ximfbf_log,
      w_log2       TYPE zpps_ximfbf_log,
      t_det        TYPE TABLE OF zpps_ximfbf_log,
      t_det2       TYPE TABLE OF zpps_ximfbf_log,
      l_x          TYPE i,
      l_grid_title TYPE lvc_title,
      l_program    TYPE sy-repid,
      w_layout     TYPE slis_layout_alv,
      t_fieldcat   TYPE slis_t_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_data FOR zpps_ximfbf_log-data.
SELECTION-SCREEN END OF BLOCK b1.

SELECT *
  FROM zpps_ximfbf_log
  INTO TABLE t_log1
 WHERE data      IN s_data
   AND id_cotton <> abap_off.

LOOP AT t_log1 INTO w_log1.
  IF strlen( w_log1-charg ) = 4.
    DELETE t_log1 INDEX sy-tabix.
  ENDIF.
ENDLOOP.

t_log2[] = t_log1[].

FREE: t_det.

SORT t_log1 BY charg.
SORT t_log2 BY charg.

LOOP AT t_log1 INTO w_log1.
  FREE: l_x, t_det2.

  LOOP AT t_log2 INTO w_log2 WHERE charg = w_log1-charg.
    l_x = l_x + 1.
    APPEND w_log2  TO t_det2.
  ENDLOOP.
  IF l_x > 1.
    APPEND LINES OF t_det2  TO t_det.
  ENDIF.
ENDLOOP.

l_program                  = sy-repid.
l_grid_title               = 'CHARG repetidos'.
w_layout-expand_all        = abap_true.
w_layout-colwidth_optimize = abap_true.

CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    i_program_name         = l_program
    i_structure_name       = 'ZPPS_XIMFBF_LOG'
  CHANGING
    ct_fieldcat            = t_fieldcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program = l_program
    is_layout          = w_layout
    it_fieldcat        = t_fieldcat
    i_grid_title       = l_grid_title
*   i_screen_start_column = 10
*   i_screen_start_line   = 02
*   i_screen_end_column   = 182
*   i_screen_end_line  = 20
  TABLES
    t_outtab           = t_det
  EXCEPTIONS
    program_error      = 1
    OTHERS             = 2.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
