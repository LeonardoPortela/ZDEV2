function Z_TEXTO_PROTEGIDO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IM_TITLE) TYPE  SYTITLE
*"     REFERENCE(IM_DISPLAY_MODE) TYPE  XFELD DEFAULT SPACE
*"     REFERENCE(IM_START_COLUMN) TYPE  I DEFAULT 10
*"     REFERENCE(IM_START_ROW) TYPE  I DEFAULT 10
*"     REFERENCE(IM_PROTEGER) TYPE  I DEFAULT '0'
*"  CHANGING
*"     REFERENCE(CH_TEXT) TYPE  CATSXT_LONGTEXT_ITAB
*"----------------------------------------------------------------------
  create object ste
    exporting
      im_title         = im_title
      im_display_mode  = im_display_mode
      im_longtext_tab  = ch_text
      im_proteger      = im_proteger.

  call screen 400 starting at im_start_column
                              im_start_row.

  ch_text = ste->get_text( ).

  call method ste->free.

  clear ste.

endfunction.
