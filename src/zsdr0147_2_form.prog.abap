
FORM get_row.
  CLEAR: lt_rows1,qtd_rows1.
  lt_rows1 = gr_table1->get_selections( )->get_selected_rows( ).

  IF lt_rows1 IS NOT INITIAL.

    LOOP AT lt_rows1 ASSIGNING FIELD-SYMBOL(<row>).
      IF <row>-index IS NOT INITIAL.
        CLEAR:qtd_rows1.
        qtd_rows1 = sy-tabix.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM container.

*** Cria Splitter Container
painel_control = NEW cl_gui_splitter_container( parent = cl_gui_container=>screen0
                                             no_autodef_progid_dynnr = abap_true
                                             rows = 2
                                             columns = 1 ).

* marking container
painel1 = painel_control->get_container( row = 1 column = 1 ).
painel2 = painel_control->get_container( row = 2 column = 1 ).

    PERFORM ALV1.
    PERFORM ALV2.


ENDFORM.
