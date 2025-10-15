
FORM get_row.
  CLEAR: lt_rows,qtd_rows.
  lt_rows = gr_table->get_selections( )->get_selected_rows( ).

  IF lt_rows IS NOT INITIAL.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>).
      IF <row>-index IS NOT INITIAL.
        CLEAR:qtd_rows.
        qtd_rows = sy-tabix.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM container.

*** Cria Splitter Container
painel_control = NEW cl_gui_splitter_container( parent = cl_gui_container=>screen0
                                             no_autodef_progid_dynnr = abap_true
                                             rows = 1
                                             columns = 2 ).

* marking container
painel1 = painel_control->get_container( row = 1 column = 1 ).
painel2 = painel_control->get_container( row = 1 column = 2 ).

    PERFORM ALV1.
    PERFORM ALV2.


ENDFORM.

FORM grava_check_contas .

  TYPES: BEGIN OF contas,
           mandt TYPE skb1-mandt,
           racct TYPE skb1-saknr,
         END OF contas.

  DATA: it_contas TYPE STANDARD TABLE OF contas INITIAL SIZE 0.

  SELECT DISTINCT a~mandt, a~saknr
  INTO TABLE @it_contas
  FROM skb1 as a
    left join tvarvc as b on a~bukrs = b~low and b~name = 'ZFIR0100_LEDGER50'
  WHERE a~mitkz = 'A'
  AND a~saknr NOT IN ( SELECT DISTINCT racct FROM zfit0100_cont ).

  IF it_contas IS NOT INITIAL.
    LOOP AT it_contas ASSIGNING FIELD-SYMBOL(<conta_save>).
      MODIFY zfit0100_cont FROM TABLE it_contas.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.

  CLEAR: it_contas.
ENDFORM.
