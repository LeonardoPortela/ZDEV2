FUNCTION zsd_get_logs_remessa .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZSDE_LOGS_REMESSA
*"----------------------------------------------------------------------


  SELECT objectid, udate, utime
    FROM cdhdr
    INTO TABLE @DATA(lt_cdhdr)
    WHERE udate BETWEEN @i_data_ini AND @i_data_fim
      AND objectclas = 'LIEFERUNG'.
  IF sy-subrc IS INITIAL.

    DATA(lt_cdhdr_aux) = lt_cdhdr.
    SORT lt_cdhdr_aux BY objectid.
    DELETE ADJACENT DUPLICATES FROM lt_cdhdr_aux COMPARING objectid.

    SELECT vbeln
      FROM likp
      INTO TABLE @DATA(lt_likp)
      FOR ALL ENTRIES IN @lt_cdhdr_aux
     WHERE vbeln = @lt_cdhdr_aux-objectid(10).
    IF sy-subrc IS INITIAL.
      SORT lt_likp BY vbeln.
    ENDIF.

    LOOP AT lt_cdhdr ASSIGNING FIELD-SYMBOL(<fs_cdhdr>).
      APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

      <fs_saida>-vbeln     = <fs_cdhdr>-objectid(10).
      <fs_saida>-dt_change = <fs_cdhdr>-udate.
      <fs_saida>-hr_change = <fs_cdhdr>-utime.

      READ TABLE lt_likp TRANSPORTING NO FIELDS
      WITH KEY vbeln = <fs_cdhdr>-objectid(10)
      BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        <fs_saida>-excluida = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE t_saida WHERE vbeln EQ '0000000000'.

  ENDIF.


ENDFUNCTION.
