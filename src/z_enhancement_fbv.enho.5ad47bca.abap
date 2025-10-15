"Name: \PR:SAPLF040\FO:FCODE_BEARBEITUNG\SE:BEGIN\EI
ENHANCEMENT 0 Z_ENHANCEMENT_FBV.
*
  DATA: T_HKONT TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.

  if 'FBV1' cs sy-tcode   and
    ( sy-ucomm = 'BU' or sy-ucomm = 'BP' ).

    call function 'G_SET_GET_ALL_VALUES'
      exporting
        class         = '0000'
        setnr         = 'CONTAS_EC-CS'
      tables
        set_values    = t_hkont
      exceptions
        set_not_found = 1
        others        = 2.
      if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

    loop at xbseg.

      read table t_hkont with key from = xbseg-hkont.

      if sy-subrc eq 0.

        if xbseg-vbund is initial.

          message e000(z01) with 'Sociedade Parceira Obrigatória para esta conta.'.

          endif.

      endif.

    endloop.
  endif.
ENDENHANCEMENT.
