*&---------------------------------------------------------------------*
*&  Include           ZXM02U03
*&---------------------------------------------------------------------*

DATA: l_mereq_item TYPE mereq_item,
      eban         TYPE eban,
      l_aktvt      TYPE aktvt.

IF NOT im_req_item IS INITIAL.
  " read item data from system
  l_mereq_item = im_req_item->get_data( ).

  l_aktvt = im_req_item->get_activity( ).

  IF l_aktvt EQ 'A'.

  ELSEIF l_aktvt EQ 'V' OR l_aktvt EQ 'H'.
    IF l_mereq_item-lifnr_coupa NE eban-lifnr_coupa.
      l_mereq_item-lifnr_coupa = eban-lifnr_coupa.
      ex_changed = 'X'.
    ENDIF.

     IF l_mereq_item-konnr_coupa NE eban-konnr_coupa.
      l_mereq_item-konnr_coupa = eban-konnr_coupa.
      ex_changed = 'X'.
    ENDIF.

    IF ex_changed = 'X'.
      CALL METHOD im_req_item->set_data( l_mereq_item ).
    ENDIF.

  ENDIF.

ENDIF.
