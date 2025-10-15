FUNCTION zfi_get_dados_pgto_ferrovia.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_BUKRS STRUCTURE  BUKRS_FIS OPTIONAL
*"      T_LIFNR STRUCTURE  EINA_LIFNR OPTIONAL
*"      T_SAIDA STRUCTURE  ZFIE_DADOS_FERROVIA OPTIONAL
*"----------------------------------------------------------------------

  DATA: lra_bukrs    TYPE RANGE OF bukrs,
        lra_lifnr    TYPE RANGE OF lifnr,
        lra_data     TYPE RANGE OF zsdt0143-data_atual.

  APPEND VALUE #( sign = 'I' option = 'BT' low = i_data_ini high = i_data_fim ) TO lra_data.

  LOOP AT t_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_bukrs> ) TO lra_bukrs.
  ENDLOOP.

  LOOP AT t_lifnr ASSIGNING FIELD-SYMBOL(<fs_lifnr>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_lifnr> ) TO lra_lifnr.
  ENDLOOP.

  SELECT bsak~bukrs
         t001~butxt
         bsak~xblnr
         fili~stcd1 AS cnpj_filial
         lfa1~stcd1 AS cnpj_lfa1
         bsak~lifnr
         lfa1~dlgrp
         bsak~augdt
         bsak~dmbtr
         lfa1~adrnr
    FROM bsak             AS bsak
    INNER JOIN lfa1       AS lfa1
            ON bsak~mandt = lfa1~mandt  AND
               bsak~lifnr = lfa1~lifnr
    INNER JOIN j_1bbranch AS fili
            ON bsak~mandt = fili~mandt  AND
               bsak~bukrs = fili~bukrs  AND
               bsak~gsber = fili~branch
    INNER JOIN t001       AS t001
            ON bsak~mandt = t001~mandt  AND
               bsak~bukrs = t001~bukrs
    INTO TABLE t_saida
    WHERE lfa1~dlgrp = '0002'
      AND bsak~xblnr <> ' '
      AND bsak~bukrs IN lra_bukrs
      AND bsak~augdt IN lra_data
      AND bsak~lifnr IN lra_lifnr
    ORDER BY fili~stcd1 lfa1~stcd1 bsak~xblnr.
  IF sy-subrc IS INITIAL.
    DATA(lt_saida) = t_saida[].
    SORT lt_saida BY adrnr.
    DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING adrnr.

    SELECT *
      FROM adr6
      INTO TABLE @DATA(lt_adr6)
      FOR ALL ENTRIES IN @lt_saida
      WHERE addrnumber = @lt_saida-adrnr.
    IF sy-subrc IS INITIAL.
      SORT lt_adr6 BY addrnumber.

      LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

        READ TABLE lt_adr6 TRANSPORTING NO FIELDS
        WITH KEY addrnumber = <fs_saida>-adrnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_adr6 ASSIGNING FIELD-SYMBOL(<fs_adr6>) FROM sy-tabix.
            IF <fs_adr6>-addrnumber <> <fs_saida>-adrnr.
              EXIT.
            ENDIF.

            CONCATENATE <fs_saida>-email <fs_adr6>-smtp_addr ';' INTO <fs_saida>-email.

          ENDLOOP.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.


ENDFUNCTION.
