"Name: \PR:SAPLCSDI\FO:BOM_BEFORE_SAVE_PREPARE\SE:END\EI
ENHANCEMENT 0 ZENH_CALC_LIST_TOTAL_KG.

IF sy-ucomm = 'FCBU' OR sy-ucomm = 'YES'.
  IF tsd-stlan = '1'. "stkob-stlal = '01'.

    TYPES:
      BEGIN OF ty_STPO.
        INCLUDE STRUCTURE stpo.
    TYPES:
        stlal TYPE stasb-stlal,
      END OF ty_STPO.


    DATA: qtd_total_agrupada TYPE stkob-bmeng.
    DATA: qtd_total TYPE stkob-bmeng.
    DATA: it_STPO_get TYPE STANDARD TABLE OF ty_STPO WITH HEADER LINE.
    DATA: it_STPO_new TYPE STANDARD TABLE OF ty_STPO WITH HEADER LINE.
    DATA: it_STPO_old TYPE STANDARD TABLE OF ty_STPO WITH HEADER LINE.
    DATA: it_STPO_calc TYPE STANDARD TABLE OF ty_STPO WITH HEADER LINE.

    CLEAR: qtd_total_agrupada,qtd_total,it_STPO_get,it_STPO_new,it_STPO_old,it_STPO_calc.
    FREE: it_STPO_get[],it_STPO_new[],it_STPO_old[],it_STPO_calc[].

    MOVE-CORRESPONDING lt_stpob[] TO it_stpo_get[].
    qtd_total = stkob-bmeng.

    LOOP AT it_stpo_get[] ASSIGNING FIELD-SYMBOL(<get_stlal>).
      READ TABLE lt_stasb[] INTO DATA(wa_stasb) WITH KEY stlkn = <get_stlal>-stvkn.
      IF wa_stasb-stvkn IS NOT INITIAL.
        <get_stlal>-stlal = wa_stasb-stlal.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_stkob[] ASSIGNING FIELD-SYMBOL(<list>) WHERE stlal = stkob-stlal GROUP BY <list>-stlal.
      "qtd_total = <list>-bmeng.


      LOOP AT it_stpo_get[] ASSIGNING FIELD-SYMBOL(<stpob_old>) WHERE stlkn < 90000000 AND meins = 'KG' AND stlal = <list>-stlal.
        MOVE-CORRESPONDING <stpob_old> TO it_STPO_old.
        APPEND it_STPO_old.
      ENDLOOP.
      LOOP AT it_stpo_get[] ASSIGNING FIELD-SYMBOL(<stpob_new>) WHERE stlkn >= 90000000 AND meins = 'KG' AND stlal = <list>-stlal.
        MOVE-CORRESPONDING <stpob_new> TO it_STPO_new.
        APPEND it_STPO_new.
      ENDLOOP.

      IF it_STPO_new[] IS NOT INITIAL.

        LOOP AT it_STPO_new[] ASSIGNING FIELD-SYMBOL(<_del_old>).
          DELETE it_STPO_old[] WHERE posnr = <_del_old>-posnr.
        ENDLOOP.

        APPEND LINES OF it_STPO_new[] TO it_stpo_calc[].

      ENDIF.

      IF it_STPO_old[] IS NOT INITIAL.
        APPEND LINES OF it_STPO_old[] TO it_stpo_calc[].
      ENDIF.


      LOOP AT it_stpo_calc[] ASSIGNING FIELD-SYMBOL(<somakg>).
        qtd_total_agrupada = qtd_total_agrupada + <somakg>-menge.
      ENDLOOP.

      IF  qtd_total IS NOT INITIAL AND qtd_total_agrupada IS NOT INITIAL.

        IF qtd_total <> qtd_total_agrupada.
          DATA(msg_dif_kg) = |Quantidade dos componentes diferente da unidade basica da lista técnica!|.
          MESSAGE msg_dif_kg TYPE 'S' DISPLAY LIKE 'E'.
          p_l_fl_error = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.
ENDIF.
ENDENHANCEMENT.
