FUNCTION z_lanc_mass_zmmr115.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CNPJ) TYPE  LFA1-STCD1
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"  TABLES
*"      I_CHAVE TYPE  ZLIST_CHAVENFE
*"----------------------------------------------------------------------

  SELECT SINGLE * FROM lfa1 WHERE stcd1 = @i_cnpj INTO @DATA(ls_lfa1).

  SELECT * FROM ekko
  WHERE 1 = 1
  AND lifnr = @ls_lfa1-lifnr
  AND substring( aedat,1,4 ) >= @i_ano
  INTO TABLE @DATA(it_ekko_lifnr).

  DATA: it_ekko_ped TYPE STANDARD TABLE OF ekko INITIAL SIZE 0.

  APPEND LINES OF it_ekko_lifnr TO it_ekko_ped.
  FREE: it_ekko_lifnr.

  SELECT * FROM ekko
  WHERE 1 = 1
  AND lifre = @ls_lfa1-lifnr
  AND substring( aedat,1,4 ) >= @i_ano
  INTO TABLE @DATA(it_ekko_lifre).

  APPEND LINES OF it_ekko_lifre TO it_ekko_ped.
  FREE: it_ekko_lifre.

  SORT it_ekko_ped BY ebeln ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_ekko_ped.

  DATA: it_list_ekpo TYPE STANDARD TABLE OF ekpo INITIAL SIZE 0.
  FREE:it_list_ekpo.

  SELECT DISTINCT * FROM ekpo
    FOR ALL ENTRIES IN @it_ekko_ped
    WHERE ebeln = @it_ekko_ped-ebeln
    "AND loekz = ''
    INTO TABLE @it_list_ekpo.

  IF it_list_ekpo IS NOT INITIAL.

    DATA: it_lista TYPE STANDARD TABLE OF zelistekpo INITIAL SIZE 0.

    MOVE-CORRESPONDING it_list_ekpo TO it_lista.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name      = 'ZELISTEKPO'
        i_screen_start_column = 5
        i_screen_end_column   = 40
        i_screen_start_line   = 1
        i_screen_end_line     = 10
      TABLES
        t_outtab              = it_lista
      EXCEPTIONS
        OTHERS                = 2.

    IF sy-subrc = 0 AND sy-ucomm = '&ONT'.
      READ TABLE it_lista INTO DATA(_lista) INDEX 1.
      READ TABLE it_list_ekpo INTO DATA(_ekpo) WITH KEY ebeln = _lista-ebeln ebelp = _lista-ebelp.
      SELECT SINGLE * FROM zib_nfe_dist_ter
        INTO @DATA(_nfe_top).

      IF _nfe_top-st_fiscal <> ''.
        MESSAGE 'Nota XXXX ja iniciado o processo de entrada!' TYPE 'I'.
        EXIT.
      ELSE.

        LOOP AT i_chave[] ASSIGNING FIELD-SYMBOL(<_chave>).
          "ZIB_NFE_DIST_TER
          SELECT * FROM zib_nfe_dist_itm
            WHERE chave_nfe = @<_chave>
            INTO TABLE @DATA(it_nfe_item).

          DATA: _qtd_itens   TYPE i.
          CLEAR: _qtd_itens.
          DESCRIBE TABLE it_nfe_item LINES _qtd_itens.

          IF _qtd_itens > 1.
            MESSAGE 'PEDIDO NÃO PODE SER ATRIBUIDO PARA MAIS DE UMNA LINHA DA NF!' TYPE 'I'.
            EXIT.
          ELSE.

            READ TABLE it_nfe_item ASSIGNING FIELD-SYMBOL(<_item_nfe>) WITH KEY chave_nfe = <_chave>.
            IF sy-subrc = 0.

              SELECT SINGLE * FROM mara WHERE matnr = @_ekpo-matnr INTO @DATA(ls_mara).

              IF sy-subrc = 0.
                UPDATE zib_nfe_dist_itm
                SET
                ebeln = @_ekpo-ebeln,
                ebelp = @_ekpo-ebelp,
                matnr = @_ekpo-matnr,
                menge = @<_item_nfe>-prod_qtd_comerci,
                meins = @ls_mara-meins
                WHERE chave_nfe = @<_chave>.
                COMMIT WORK.

                UPDATE zib_nfe_dist_ter
                SET
                ebeln           = @_ekpo-ebeln,
                st_fiscal       = 99,
                st_fisico       = 00,
                st_armazem      = 98,
                st_documento    = 01,
                cd_departamento = 9999,
                ck_fiscal       = @abap_true,
                ck_fisico       = @abap_false,
                p_emissor       = @ls_lfa1-lifnr
                WHERE chave_nfe = @<_chave>.
                COMMIT WORK.

              ENDIF.
            ENDIF.

          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ELSE.
    MESSAGE 'Pedido/item não existe!' TYPE 'I'.
  ENDIF.

ENDFUNCTION.
