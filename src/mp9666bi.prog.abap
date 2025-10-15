*&---------------------------------------------------------------------*
*& Report  MP9666BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9666bi.

TABLES: p9666.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9666.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  DATA: dec_help_char(50) TYPE c.
  p9666 = <bi_wplog>.
  WRITE p9666-zperc
   TO dec_help_char LEFT-JUSTIFIED.
  PERFORM fill_field(rhaltd00) USING
  'P9666-ZPERC'
  dec_help_char.
  WRITE p9666-zvalor
   TO dec_help_char LEFT-JUSTIFIED.
  PERFORM fill_field(rhaltd00) USING
  'P9666-ZVALOR'
  dec_help_char.
  PERFORM fill_field(rhaltd00) USING
  'P9666-ZTPBENEF'
  p9666-ztpbenef.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9666-DUMMY' P9666-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
