FUNCTION zles_get_data_spl_0001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAIDA_001 TYPE  ZLEST0231
*"      T_SAIDA_002 TYPE  ZLEST0231_1
*"      T_SAIDA_003 TYPE  BBY_TT_T001W
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_matkl,
           matkl TYPE t023t-matkl,
         END OF ty_matkl.

  TYPES: lr_range_te TYPE RANGE OF erdat.
  DATA: it_range_erdat TYPE RANGE OF zlest0096-erdat.
  DATA: it_saida    TYPE STANDARD TABLE OF zlede0231,
        it_matkl    TYPE STANDARD TABLE OF ty_matkl,
        lit_t001w   TYPE TABLE OF t001w,
        ls_matkl    TYPE ty_matkl,
        ls_out_001  TYPE zlede0231,
        lv_matkl(6) TYPE c,
        lv_qtd      TYPE sy-tabix,
        lv_qtd2     TYPE sy-tabix,
        lv_linhas   TYPE sy-tabix,
        lra_stcd1   TYPE RANGE OF lfa1-stcd1,
        lv_achou    TYPE c.

*        it_kna1  TYPE STANDARD TABLE OF kna1.

  IF i_erdat_ini IS NOT INITIAL.
    it_range_erdat = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                              option = 'BT' ( low = i_erdat_ini high = i_erdat_fim )
                                                                          ).
  ENDIF.



  SELECT o2~*,
*         tr~name1 AS name6,
         fl~werks AS fl_werks,
         fl~name1 AS fl_name1
*         gb~wgbez
    INTO CORRESPONDING FIELDS OF TABLE @it_saida
    FROM zlest0112 AS o2
*    LEFT OUTER JOIN kna1 AS tr
*     ON o2~cod_transbordo    = tr~kunnr
    LEFT OUTER JOIN t001w AS fl
    ON o2~werks = fl~werks
    WHERE o2~erdat IN @it_range_erdat.
  IF sy-subrc IS INITIAL.

    LOOP AT it_saida INTO DATA(ls_saida).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_saida-matkl
        IMPORTING
          output = lv_matkl.

      ls_matkl-matkl = lv_matkl.
      APPEND ls_matkl TO it_matkl.
      CLEAR: ls_matkl , lv_matkl.

    ENDLOOP.

    SORT it_matkl BY matkl.
    DELETE ADJACENT DUPLICATES FROM it_matkl COMPARING matkl.

    IF it_matkl[] IS NOT INITIAL.
      SELECT *
        FROM t023t
        INTO TABLE @DATA(it_t023t)
        FOR ALL ENTRIES IN @it_matkl
        WHERE spras = 'P'
          AND matkl EQ @it_matkl-matkl.
    ENDIF.

    SELECT *
      FROM kna1
*      INTO TABLE it_kna1
      INTO TABLE @DATA(it_kna1)
      FOR ALL ENTRIES IN @it_saida
      WHERE kunnr EQ @it_saida-cod_transbordo.
    IF sy-subrc IS INITIAL.

      DESCRIBE TABLE it_kna1 LINES lv_linhas.

      LOOP AT it_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).

        ADD 1 TO lv_qtd2.

        APPEND VALUE #( sign = 'I' option = 'CP' low = <fs_kna1>-stcd1(8) && '*' ) TO lra_stcd1.

        IF lv_qtd2 = 1000 OR lv_qtd2 = lv_linhas.

          lv_linhas = lv_linhas - lv_qtd2.

          SELECT *
             FROM lfa1
             APPENDING TABLE @DATA(it_lfa1)
             WHERE stcd1 IN @lra_stcd1.

          CLEAR lv_qtd2.
          REFRESH: lra_stcd1.

        ENDIF.

      ENDLOOP.
    ENDIF.

    IF it_lfa1 IS NOT INITIAL.
      SORT it_lfa1 BY stcd1.

      SELECT *
        FROM zlest0070
        INTO TABLE @DATA(it_zlest0070)
        FOR ALL ENTRIES IN @it_lfa1
        WHERE lifnr EQ @it_lfa1-lifnr.

    ENDIF.

    SELECT lifnr name1
       FROM lfa1
       APPENDING CORRESPONDING FIELDS OF TABLE it_lfa1
      FOR ALL ENTRIES IN it_saida
       WHERE lifnr EQ it_saida-cod_porto.

    SELECT kunnr name1
       FROM kna1
       APPENDING CORRESPONDING FIELDS OF TABLE it_kna1
      FOR ALL ENTRIES IN it_saida
       WHERE kunnr EQ it_saida-cod_cliente.

    SELECT werks name1
       FROM t001w
       INTO CORRESPONDING FIELDS OF TABLE lit_t001w
      FOR ALL ENTRIES IN it_saida
       WHERE werks EQ it_saida-cod_cliente+6(4).

    SORT: it_t023t BY matkl,
          it_kna1  BY kunnr,
          it_lfa1  BY stcd1,
          it_zlest0070 BY lifnr.


    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
      MOVE-CORRESPONDING: <fs_saida> TO ls_out_001.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_saida>-matkl
        IMPORTING
          output = <fs_saida>-matkl.

      READ TABLE it_t023t INTO DATA(ls_t023t) WITH KEY matkl = <fs_saida>-matkl BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_out_001-gb_wgbez = ls_t023t-wgbez.
      ENDIF.

      READ TABLE it_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = <fs_saida>-cod_transbordo BINARY SEARCH .
      IF sy-subrc IS INITIAL.

        ls_out_001-tr_name1 = ls_kna1-name1.

        READ TABLE it_lfa1 INTO DATA(ls_lfa1) WITH KEY stcd1(8) = ls_kna1-stcd1(8) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT it_lfa1 INTO ls_lfa1 FROM sy-tabix.
            IF ls_lfa1-stcd1(8) <> ls_kna1-stcd1(8).
              EXIT.
            ENDIF.

            READ TABLE it_zlest0070 INTO DATA(ls_0070) WITH KEY lifnr = ls_lfa1-lifnr BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              lv_achou = abap_true.
              EXIT.
            ENDIF.

          ENDLOOP.

          IF lv_achou IS  NOT INITIAL.
            ls_out_001-ds_tp_transbordo = 'Ferroviario'.
          ELSE.
            ls_out_001-ds_tp_transbordo = ' '.
          ENDIF.

          CLEAR lv_achou.
        ENDIF.

      ENDIF.

      IF <fs_saida>-cod_porto IS NOT INITIAL.
        READ TABLE it_lfa1 INTO ls_lfa1 WITH KEY lifnr = <fs_saida>-cod_porto.
        IF sy-subrc EQ 0.
          ls_out_001-nm_destino = ls_lfa1-name1.
        ENDIF.
      ELSE.
        IF <fs_saida>-bsart IS INITIAL.
          READ TABLE it_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_saida>-cod_cliente.
          IF sy-subrc EQ 0.
            ls_out_001-nm_destino = ls_kna1-name1.
          ENDIF.
        ELSE.
          READ TABLE it_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_saida>-cod_cliente+06(04).
          IF sy-subrc EQ 0.
            ls_out_001-nm_destino = ls_kna1-name1.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND  ls_out_001  TO t_saida_001.
      CLEAR ls_out_001.

    ENDLOOP.

  ENDIF.



  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_saida_002
    FROM zlest00100.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_saida_003
      FROM t001w.




ENDFUNCTION.
